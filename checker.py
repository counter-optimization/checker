import sys
import logging
from abc import ABC, abstractmethod
import typing
from typing import List, Optional, Union
from pathlib import Path
import csv
import argparse

import angr
import pyvex
import claripy

# GLOBALS
__version__ = '0.0.0'

logger = logging.getLogger(__name__)
logger.setLevel(logging.WARNING)

default_solver = claripy.Solver()
comp_simp_solver = None

argparser = argparse.ArgumentParser()

argparser.add_argument('--version', 
                        action='version',
                        version=__version__)

checker_arg_group = argparser.add_argument_group('Available checkers')
checker_arg_group.add_argument('--silent-stores', 
                        action='store_true',
                        help='check the function in the binary for instructions '
                             'vulnerable to silent stores')

checker_arg_group.add_argument('--comp-simp',
                        action='store_true',
                        help='check the function in the binary for instructions '
                             'vulnerable to computation simplification')

comp_simp_arg_group = argparser.add_argument_group('Computation simplification')
comp_simp_arg_group.add_argument('--use-small-bitwidth-solver', 
                        action='store_true')

comp_simp_arg_group.add_argument('--bitwidth-for-small-bitwidth-solver',
                        choices=[1, 2, 4, 8, 16, 32, 64, 128],
                        help='must be a power of two >= 1, <= 128',
                        type=int)

required_arg_group = argparser.add_argument_group('Required target file info')
required_arg_group.add_argument('path_to_binary',
                        help='path to binary containing the function to check. '
                             'for the current version of the checker, it *must be'
                             '* an ELF file (or at least a format cle understands '
                             'which is at least not a macho file)')

required_arg_group.add_argument('function_name_symbol',
                        help='name of the function that is also its '
                             'symbol in the binary')

parsed_args = None
# END GLOBALS

class Checker(ABC):
    vulnerable_states: Union[List[angr.sim_state.SimState], 'NotImplemented']  = NotImplemented
    effects: Union[List[str], 'NotImplemented'] = NotImplemented

    @staticmethod
    @abstractmethod
    def check(state: angr.sim_state.SimState) -> bool:
        """
        Takes a state, adds state to CheckerSubClass.vulnerable_states if
        vulnerable, and returns True if the Checker subclass flags this
        state as vulnerable/important
        """
        raise NotImplemented

class SilentStoreChecker(Checker):
    vulnerable_states = []

     # how does this checker affect symbolic state
    effects = ['mem_read']

    @staticmethod
    def check(state: angr.sim_state.SimState) -> bool:
        logger.debug(f"Entering before mem write at state: {state}") 
        expr = state.inspect.mem_write_expr
        addr = state.inspect.mem_write_address

        if expr.size() == 64:
            prev_val = state.mem[addr].uint64_t.resolved
        elif expr.size() == 32:
            prev_val = state.mem[addr].uint32_t.resolved
        elif expr.size() == 16:
            prev_val = state.mem[addr].uint16_t.resolved
        elif expr.size() == 8:
            prev_val = state.mem[addr].uint8_t.resolved
        else:
            raise RuntimeError(f"Unhandled bitwidth in store at state: {state}")

        logger.debug(f"expr is {expr}")
        logger.debug(f"prev_val is {prev_val}")
        logger.debug(f"addr is {addr}")

        default_solver.add(expr == prev_val)
        is_sat = default_solver.satisfiable() # True if SAT, False if UNSAT/UNKNOWN
        
        if is_sat:
            SilentStoreChecker.vulnerable_states.append(state)
            logger.warning(f"Found state {state} with a silent store")
        else:
            logger.debug(f"No silent store possible in state {state}")
        
        logger.debug(f"Values could be the same: {is_sat}")
        logger.debug(f"Leaving before mem write at state: {state}")

        return is_sat

class CompSimpDataRecord():
    func_identifier = "" 

    @staticmethod
    def set_func_identifier(name: str):
        CompSimpDataRecord.func_identifier = name
    
    def __init__(self, expr: pyvex.expr.IRExpr, state: angr.sim_state.SimState):
        self.state = state
        self.expr = expr

        self.operation = ""
        self.bitwidth = pyvex.get_type_size(expr.result_type(state.scratch.tyenv))
        
        self.numConstantOperands = 0
        self.firstOperandConst = None
        self.secondOperandConst = None

        self.powerOfTwoSignificant = False
        self.numPowerOfTwoOperands = 0
        self.firstOperandPowerOfTwo = False
        self.secondOperandPowerOfTwo = False

        self.hasLeftIdentity = False
        self.hasRightIdentity = False
        self.numIdentityOperands = 0
        self.firstOperandIdentity = False
        self.secondOperandIdentity = False
        self.rightIdentity = NotImplemented
        self.leftIdentity = NotImplemented

        self.hasLeftZero = False
        self.hasRightZero = False
        self.numZeroElementOperands = 0
        self.firstOperandZeroElem = False
        self.secondOperandZeroElem = False

        self.numPossibleFirstOperand = None
        self.numPossibleSecondOperand = None

        # base64 encoded for shoving into csv
        self.serializedFirstOperandFormula = ""
        self.serializedSecondOperandFormula = ""

    def operandIsConst(self, expr):
        return type(expr) == pyvex.expr.Const

    def isTmpVar(self, expr) -> bool:
        return type(expr) == pyvex.expr.RdTmp

    def isSupportedOperandType(self, expr) -> bool:
        return self.isTmpVar(expr) or self.operandIsConst(expr)

    def getTmpSymbolicValue(self, expr):
        assert(self.isTmpVar(expr))
        logger.debug(f"in getTmpSymbolicValue, expr.tmp is {expr.tmp}")
        tmp_expr = self.state.scratch.tmp_expr(expr.tmp)
        logger.debug(f"tmp_expr returns: {tmp_expr}")
        # return self.state.scratch.temps[expr.tmp]
        return tmp_expr

    def couldBePowerOfTwo(self, symval) -> bool:
        # symval != 0 and ((symval & (symval - 1)) == 0)
        # https://stackoverflow.com/questions/600293/how-to-check-if-a-number-is-a-power-of-2
        # originally, this generated one or zero using self.bitwidth
        # this now just matches symval.length (symval's bitwidth) for in the
        # case of division (vexir does div of 64 bit value by 32 bit value,
        # so the operation's bitwidth is not reliable
        # one = claripy.BVV(1, self.bitwidth)
        # zero = claripy.BVV(0, self.bitwidth)
        one = claripy.BVV(1, symval.length)
        zero = claripy.BVV(0, symval.length)
        comp_simp_solver.add(symval != zero)
        comp_simp_solver.add((symval & (symval - one)) == zero)
        is_sat = comp_simp_solver.satisfiable()
        return is_sat

    def couldBePowerOfTwoConcrete(self, concval: pyvex.expr.Const) -> bool:
        value = concval.con.value
        cond = value != 0
        cond = cond and ((value & (value - 1)) == 0)
        return cond

    def couldBeRightIdentity(self, expr) -> bool:
        comp_simp_solver.add(expr == self.rightIdentity(expr.length))
        is_sat = comp_simp_solver.satisfiable()
        return is_sat

    def couldBeRightIdentityConcrete(self, expr: pyvex.expr.Const) -> bool:
        # just default to largest bitwidth
        return (expr.con.value == self.rightIdentity(64)).is_true()

    def couldBeLeftIdentity(self, expr) -> bool:
        comp_simp_solver.add(expr == self.leftIdentity(expr.length))
        is_sat = comp_simp_solver.satisfiable()
        return is_sat

    def couldBeLeftIdentityConcrete(self, expr: pyvex.expr.Const) -> bool:
        # just default to largest bitwidth
        return (expr.con.value == self.leftIdentity(64)).is_true()

    def couldBeLeftZero(self, expr) -> bool:
        comp_simp_solver.add(expr == self.leftZero(expr.length))
        is_sat = comp_simp_solver.satisfiable()
        return is_sat

    def couldBeRightZero(self, expr) -> bool:
        comp_simp_solver.add(expr == self.rightZero(expr.length))
        is_sat = comp_simp_solver.satisfiable()
        return is_sat

    def couldBeRightZeroConcrete(self, expr: pyvex.expr.Const) -> bool:
        # just default to largest bitwidth
        return (expr.con.value == self.rightZero(64)).is_true()

    def couldBeLeftZeroConcrete(self, expr: pyvex.expr.Const) -> bool:
        # just default to largest bitwidth
        return (expr.con.value == self.leftZero(64)).is_true()

    def checkForSpecialValues(self, expr, isLeft: bool):
        logger.debug(f"Checking {expr} for special values")
        if self.powerOfTwoSignificant:
            if self.couldBePowerOfTwo(expr):
                logger.debug(f"{expr} could be power of two")
                self.numPowerOfTwoOperands += 1
                if isLeft:
                    self.firstOperandPowerOfTwo = True
                else:
                    self.secondOperandPowerOfTwo = True

        if isLeft:
            if self.hasLeftIdentity:
                if self.couldBeLeftIdentity(expr):
                    logger.debug(f"{expr} could be left ident")
                    self.numIdentityOperands += 1
                    self.firstOperandIdentity = True
            if self.hasLeftZero:
                if self.couldBeLeftZero(expr):
                    logger.debug(f"{expr} could be left zero")
                    self.numZeroElementOperands += 1
                    self.firstOperandZeroElem = True
        else:
            if self.hasRightIdentity:
                if self.couldBeRightIdentity(expr):
                    logger.debug(f"{expr} could be right ident")
                    self.numIdentityOperands += 1
                    self.secondOperandIdentity = True
            if self.hasRightZero:
                if self.couldBeRightZero(expr):
                    logger.debug(f"{expr} could be right zero")
                    self.numZeroElementOperands += 1
                    self.secondOperandZeroElem = True

    def checkForSpecialValuesConcrete(self, expr, isLeft: bool):
        logger.debug(f"Checking {expr} for special values concrete")
        if self.powerOfTwoSignificant:
            if self.couldBePowerOfTwoConcrete(expr):
                logger.debug(f"{expr} could be power of two concrete")
                self.numPowerOfTwoOperands += 1
                if isLeft:
                    self.firstOperandPowerOfTwo = True
                else:
                    self.secondOperandPowerOfTwo = True

        if isLeft:
            if self.hasLeftIdentity:
                if self.couldBeLeftIdentityConcrete(expr):
                    logger.debug(f"{expr} could be left ident concrete")
                    self.numIdentityOperands += 1
                    self.firstOperandIdentity = True

            if self.hasLeftZero:
                if self.couldBeLeftZeroConcrete(expr):
                    logger.debug(f"{expr} could be left zero concrete")
                    self.numZeroElementOperands += 1
                    self.firstOperandZeroElem = True
        else:
            if self.hasRightIdentity:
                if self.couldBeRightIdentityConcrete(expr):
                    logger.debug(f"{expr} could be right ident concrete")
                    self.numIdentityOperands += 1
                    self.secondOperandIdentity = True
            if self.hasRightZero:
                if self.couldBeRightZeroConcrete(expr):
                    logger.debug(f"{expr} could be right zero concrete")
                    self.numZeroElementOperands += 1
                    self.secondOperandZeroElem = True

    def handleBinOp(self):
        """
        in two-address, firstOperand is dst
        e.g., in angr, addq rax, rbx is:
        $tmpXXX = Add64(RdTmp($tmpXXX), RdTmp($tmpYYY))
        """
        # Getting
        assert(type(self.expr) == pyvex.expr.Binop)
        firstOperand = self.expr.args[0]
        secondOperand = self.expr.args[1]
        operation = self.expr.op
        self.operation = operation
        logger.debug(f"In handleBinOp: firstOperand is: {firstOperand}")
        logger.debug(f"In handleBinOp: secondOperand is: {secondOperand}")

        # Decoding
        if not self.isSupportedOperandType(firstOperand):
            warn_str = "In handlBinOp: unsupported operand type for operand: "
            warn_str += (str(firstOperand))
            warn_str += (" with type: ")
            warn_str += (type(firstOperand))
            logger.critical(warn_str)
            raise RuntimeError(warn_str)

        if not self.isSupportedOperandType(secondOperand):
            warn_str = "In handlBinOp: unsupported operand type for operand: "
            warn_str += (str(secondOperand))
            warn_str += (" with type: ")
            warn_str += (type(secondOperand))
            logger.critical(warn_str)
            raise RuntimeError(warn_str)

        # TODO: so this symval should be handled at least for expr: RdTmp,
        # expr: Const, expr: Reg(?) in addition to just tmp vars
        firstOperandSymVal = None
        secondOperandSymVal = None

        if self.isTmpVar(firstOperand):
            logger.debug("first operand is tmp var")
            firstOperandSymVal = self.getTmpSymbolicValue(firstOperand)

        if self.isTmpVar(secondOperand):
            logger.debug("second operand is tmp var")
            secondOperandSymVal = self.getTmpSymbolicValue(secondOperand)

        logger.debug(f"firstOperandSymVal: {firstOperandSymVal}")
        logger.debug(f"secondOperandSymVal: {secondOperandSymVal}")

        if self.operandIsConst(firstOperand):
            self.firstOperandConst = firstOperand.con.value
            self.numConstantOperands += 1
            self.checkForSpecialValuesConcrete(firstOperand, isLeft=True)
        elif self.isTmpVar(firstOperand):
            if firstOperandSymVal.concrete:
                self.firstOperandConst = str(firstOperandSymVal)
                self.numConstantOperands += 1
            self.checkForSpecialValues(firstOperandSymVal, isLeft=True)
        else:
            raise RuntimeError(f"Unsupported operand type, firstOperand: {firstOperand}")

        if self.operandIsConst(secondOperand):
            self.secondOperandConst = secondOperand.con.value
            self.numConstantOperands += 1
            self.checkForSpecialValuesConcrete(secondOperand, isLeft=False)
        elif self.isTmpVar(secondOperand):
            if secondOperandSymVal.concrete:
                self.secondOperandConst = str(secondOperandSymVal)
                self.numConstantOperands += 1
            self.checkForSpecialValues(secondOperandSymVal, isLeft=False)
        else:
            raise RuntimeError(f"Unsupported operand type, secondOperand: {secondOperand}")

    def writeInsns(self):
        logger.debug(f"Writing insns to {CompSimpDataRecord.func_identifier}")
        insns_file = Path.cwd() / Path(f"{CompSimpDataRecord.func_identifier}.insns")
        if not insns_file.exists():
            with insns_file.open(mode='w') as f:
                block = self.state.project.factory.block(self.state.addr)
                block.pp()
                f.write(str(block))
                f.write(str(block.vex))

    def address(self) -> str:
        return hex(self.state.addr)

    def expression(self) -> str:
        return str(self.expr)

    @staticmethod
    def getCSVHeaderColNames() -> List[str]:
        cols = ['address',
                'expression',
                'operation',
                'bitwidth',
                'numConstantOperands',
                'firstOperandConst',
                'secondOperandConst',
                'numPowerOfTwoOperands',
                'firstOperandPowerOfTwo',
                'secondOperandPowerOfTwo',
                'numIdentityOperands',
                'firstOperandIdentity',
                'secondOperandIdentity',
                'numZeroElementOperands',
                'firstOperandZeroElem',
                'secondOperandZeroElem',
                'numPossibleFirstOperand',
                'numPossibleSecondOperand',
                'serializedFirstOperandFormula',
                'serializedSecondOperandFormula']
        return cols

    def getAttributeResult(self, attrname: str) -> str:
        intermediate_result = getattr(self, attrname)
        if callable(intermediate_result):
            return str(intermediate_result())
        else:
            return str(intermediate_result)

    def getCSVRow(self) -> List[str] :
        values = map(lambda x: self.getAttributeResult(x),
                     CompSimpDataRecord.getCSVHeaderColNames())
        return list(values)

class AddDataRecord(CompSimpDataRecord):
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = False
        self.hasLeftZero = False
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.leftIdentity = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = False
        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()

class ShiftDataRecord(CompSimpDataRecord):
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = False
        self.hasRightZero = False
        self.hasLeftZero = True
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.leftZero = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = False
        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()

class SubDataRecord(CompSimpDataRecord):
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = False
        self.hasRightZero = False
        self.hasLeftZero = False
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = False
        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()

class AndDataRecord(CompSimpDataRecord):
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = True
        self.hasLeftZero = True
        self.rightIdentity = lambda bw: ~claripy.BVV(0, bw)
        self.leftIdentity = self.rightIdentity
        self.rightZero = lambda bw: claripy.BVV(0, bw)
        self.leftZero = self.rightZero
        self.powerOfTwoSignificant = False
        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()

class OrDataRecord(CompSimpDataRecord):
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = False
        self.hasLeftZero = False
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.leftIdentity = self.rightIdentity
        self.powerOfTwoSignificant = False
        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()

class XorDataRecord(CompSimpDataRecord):
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = False
        self.hasLeftZero = False
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.leftIdentity = self.rightIdentity
        self.powerOfTwoSignificant = False
        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()

class MulDataRecord(CompSimpDataRecord):
   def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = True
        self.hasLeftZero = True
        self.rightIdentity = lambda bw: claripy.BVV(1, bw)
        self.leftIdentity = lambda bw: claripy.BVV(1, bw)
        self.rightZero = lambda bw: claripy.BVV(0, bw)
        self.leftZero = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = True
        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()

class DivDataRecord(CompSimpDataRecord):
   def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = False
        self.hasRightZero = False
        self.hasLeftZero = True
        self.rightIdentity = lambda bw: claripy.BVV(1, bw)
        self.leftZero = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = True
        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()
                
class CompSimpDataCollectionChecker(Checker):
    """
    Driver class for finding insn expressions of interest
    In charge of filtering out insns, passing the state and expr
    to some class that processes these, and then outputting a CSV
    of all the processes states and exprs.

    Examples of some states and exprs
    expr Shl64(t349,0x01) (<class 'pyvex.expr.Binop'>) is binop
    expr Shl64(t349,0x01) op: Iop_Shl64. tag: Iex_Binop
    expr And64(t440,t157) (<class 'pyvex.expr.Binop'>) is binop
    expr And64(t440,t157) op: Iop_And64. tag: Iex_Binop
    """
    vulnerable_states = []
    effects = []
    finders = []
    csv_records = []

    # maps substring of pyvex binop expr names to their checking class
    checkers = {'add': AddDataRecord,
                'mul': MulDataRecord,
                'div': DivDataRecord,
                'sh': ShiftDataRecord,
                'sa': ShiftDataRecord,
                'sub': SubDataRecord,
                'and': AndDataRecord,
                'or': OrDataRecord,
                'xor': XorDataRecord}

    @staticmethod
    def check(state: angr.sim_state.SimState) -> bool:
        expr = state.inspect.expr
        checkers = CompSimpDataCollectionChecker.checkers
        
        # only pyvex.expr.Binop has `.op` attribute
        if not isinstance(expr, pyvex.expr.Binop):
            return False

        logger.debug(f"Checking expr: {expr}")

        try:
            for op_kw in checkers.keys():
                if op_kw in expr.op.lower():
                    logger.debug(f"{expr} is going to be recorded")
                    checker = checkers[op_kw]
                    logger.debug(f"Using {checker} to check {expr}")
                    filled_out_record = checker(expr, state)
                    csv_record = filled_out_record.getCSVRow()
                    CompSimpDataCollectionChecker.csv_records.append(csv_record)
        except Exception as err:
            # unfortunately, exceptions are not being propagated when running this
            # from the command line... putting this here to catch anything since
            # any exceptions in the checking here are very important.
            exception_type, exception_object, exception_traceback = sys.exc_info()
            import traceback
            filename = exception_traceback.tb_frame.f_code.co_filename
            line_number = exception_traceback.tb_lineno
            logger.critical(f"ERROR in {filename} at L{line_number}! ({err})")
            logger.critical(f"exception occured when processing state {state} expr {expr} op {expr.op}")
            traceback.print_tb(exception_traceback)
            sys.exit(-1)

        return False

    @staticmethod
    def get_unique_csv_records():
        unique_rows = []
        [unique_rows.append(rec) for rec in \
         CompSimpDataCollectionChecker.csv_records if rec not in unique_rows]
        return unique_rows

    @staticmethod
    def write_records_to_csv(csv_file_name: str):
        csv_file_path = Path(csv_file_name)
        if csv_file_path.exists():
            logger.debug(f"csv file {csv_file_path} exists, unlinking...")
            csv_file_path.unlink()

        with csv_file_path.open(mode="w") as f:
            csv_file = csv.writer(f)

            header_cols = CompSimpDataRecord.getCSVHeaderColNames()
            csv_file.writerow(header_cols)
            csv_rows = CompSimpDataCollectionChecker.get_unique_csv_records()
            logger.debug(f"Writing {len(csv_rows)} csv records")
            csv_file.writerows(csv_rows)

class ComputationSimplificationChecker(Checker):
    """
    TODO
    Wrapper class around the csv recording CompSimpDataCollectionChecker.
    The CompSimpDataCollectionChecker writes a csv, this one gives alerts
    if an insn is actually simplifiable/bypassable.
    """
    vulnerable_states = []
    effects = []

    @staticmethod
    def check(state: angr.sim_state.SimState) -> bool:
        return False

class DMPChecker(Checker):
    vulnerable_states = []
    effects = ['mem_read']

    @staticmethod
    def check(state: angr.sim_state.SimState) -> bool:
        return False

class VoidSimProcedure(angr.SimProcedure):
    def run(self):
        return 0

# here, p is a pythonic definition of the point type from HACL*:
# p is a list of 20 uint64_t types. this list is broke down into field
# elements of 5 uint64_t types or 5 51-bit limbs.
# 
# the precondition in HACL* looks like:
# 
# let mul_inv_t (f:felem5) =
#   let (o0, o1, o2, o3, o4) = f in
#   if v o1 >= pow2 51 then
#     felem_fits5 f (1, 2, 1, 1, 1) /\ v o1 % pow2 51 < 8192
#   else felem_fits5 f (1, 1, 1, 1, 1)
# 
# with
# 
# let felem_fits1 (x:uint64) (m:scale64) =
#   uint_v x <= m * max51

# so for (p0, p1, p2, p3, p4):

# let felem_fits5 (f:felem5) (m:scale64_5) =
#   let (x1,x2,x3,x4,x5) = f in
#   let (m1,m2,m3,m4,m5) = m in
#   felem_fits1 x1 m1 /\
#   felem_fits1 x2 m2 /\
#   felem_fits1 x3 m3 /\
#   felem_fits1 x4 m4 /\
#   felem_fits1 x5 m5
def ed25519_point_addition_predicate(point, state):
    pow2_51 = claripy.BVV(1 << 51, 64)
    # (2 ** 51) - 1 and 64 bits wide
    max51 = claripy.BVV((1 << 51) - 1, 64)
    
    
    one = claripy.BVV(1, 64)
    two = claripy.BVV(2, 64)

    all_ones = [one, one, one, one, one]
    with_a_two = [one, two, one, one, one]
    
    def felem_fits1(x, m):
        return claripy.ULE(x, m * max51)

    def felem_fits5(x, scale):
        x1, x2, x3, x4, x5 = x
        m1, m2, m3, m4, m5 = scale
        return claripy.And(
            felem_fits1(x1, m1),
            felem_fits1(x2, m2),
            felem_fits1(x3, m3),
            felem_fits1(x4, m4),
            felem_fits1(x5, m5)
        )

    def mul_inv_t(felem):
        f1, f2, f3, f4, f5 = felem
        state.solver.add(
            claripy.If(
                claripy.UGE(f2, pow2_51),
                claripy.And(
                    felem_fits5(felem,
                                with_a_two),
                    claripy.ULT(claripy.SMod(f2, pow2_51),
                                claripy.BVV(8192, 64))
                    
                ),
                felem_fits5(felem, all_ones)
            )
        )
        
    x = point[0:5]
    y = point[5:10]
    z = point[10:15]
    t = point[15:20]

    mul_inv_t(x)
    mul_inv_t(y)
    mul_inv_t(z)
    mul_inv_t(t)

def ed25519_priv_key_precondition(priv_key_arr, state):
    first_byte = priv_key_arr[0]
    # mysecret[0] &= 248;
    state.solver.add(
        first_byte[2:0] == claripy.BVV(0, 3)
    )
    
    last_byte = priv_key_arr[31]
    
    # mysecret[31] &= 127;
    state.solver.add(
        last_byte[7] == claripy.BVV(0, 1)
    )

    # mysecret[31] |= 64;
    state.solver.add(
        last_byte[6] == claripy.BVV(1, 1)
    )

def setup_symbolic_state_for_ed25519_pub_key_gen(proj, init_state, fn_name):
    logger.warning("Setting up symbolic state for ed25519 comp simp checking")

    # 1. generate priv key
    priv_key = [claripy.BVS(f"priv{n}", 8) for n in range (1, 33)]
    pub_key_storage = [claripy.BVS(f"pub{n}", 8) for n in range (1, 33)]

    # 2. add preconditions to priv key
    # logger.debug("Adding preconditions...")
    # ed25519_priv_key_precondition(priv_key, init_state)
    # logger.debug("Done adding preconditions.")

    # 3. use some current stack memory and ensure it is aligned
    # i think on X86_64, rsp + 8 has to be 16 byte aligned.
    # priv key is 32 bytes
    size_of_priv_key = claripy.BVV(32 * 8, 64)
    
    init_state.regs.rsp = init_state.regs.rsp - size_of_priv_key
    priv_key_addr = init_state.regs.rsp
    
    init_state.regs.rsp = init_state.regs.rsp - size_of_priv_key
    pub_key_addr = init_state.regs.rsp

    init_state.regs.rsp = init_state.regs.rsp - claripy.BVV(8, 64)
    logger.debug(f"priv_key_addr: {priv_key_addr}")
    logger.debug(f"pub_key_addr: {pub_key_addr}")
    logger.debug(f"rsp is: {init_state.regs.rsp}")

    # 4. put the keys in their addresses
    def store_point_at_addr(key, addr):
        sizeof_uint8_t = claripy.BVV(1, 64)
        cur_addr = addr
        for byte in key:
            logger.debug(f"Storing limb {byte} to addr {cur_addr}")
            init_state.mem[cur_addr].uint8_t = byte
            cur_addr += sizeof_uint8_t

    store_point_at_addr(priv_key, priv_key_addr)
    store_point_at_addr(pub_key_storage, pub_key_addr)

    # 5. set rdi, rsi, rdx for first three arguments
    # rdi is u8* pub
    # rsi is u8* priv
    init_state.regs.rdi = pub_key_addr
    init_state.regs.rsi = priv_key_addr
    logger.debug(f"for init_state ({init_state}), rdi holds {init_state.regs.rdi}")
    logger.debug(f"for init_state ({init_state}), rsi holds {init_state.regs.rsi}")

    stack_chk_fail_sym_str = "__stack_chk_fail"
    if proj.loader.find_symbol(stack_chk_fail_sym_str):
        logger.warning(f"Hooking {stack_chk_fail_sym_str} with VoidSimProcedure")
        proj.hook_symbol(stack_chk_fail_sym_str, VoidSimProcedure())

    memcpy_chk_fail_sym_str = "__memcpy_chk"
    if proj.loader.find_symbol(memcpy_chk_fail_sym_str):
        logger.warning(f"Hooking {memcpy_chk_fail_sym_str} with VoidSimProcedure")
        proj.hook_symbol(memcpy_chk_fail_sym_str, VoidSimProcedure())

    memcpy_sym_str = "memcpy"
    if proj.loader.find_symbol(memcpy_sym_str):
        logger.warning(f"Hooking {memcpy_sym_str} with angr memcpy SimProcedure")
        proj.hook_symbol(memcpy_sym_str, angr.SIM_PROCEDURES['libc']['memcpy']())

    memset_sym_str = "memset"
    if proj.loader.find_symbol(memset_sym_str):
        logger.warning(f"Hooking {memset_sym_str} with angr memset SimProcedure")
        proj.hook_symbol(memset_sym_str, angr.SIM_PROCEDURES['libc']['memset']())
    
def setup_symbolic_state_for_ed25519_point_addition(proj, init_state, fn_name):
    """
    1. generate three points using claripy                                  │
    2. add the preconditions to the two input points                                │
    3. using cle, find some memory that is not being used                 │
    4. put the three points (p, q, out) there, and for now,               │
       keep them disjoint                                                 │
    5. at the start state of running symbolic execution on the point      │
       addition, set the argument registers to the memory addresses of the│
       three points                                                       │
    """

    # in case i forget to comment this out for later
    if "hacl" not in fn_name.lower():
        return
    else:
        logger.warn("Setting up symbolic state for ed25519 comp simp checking")

    # 1. generate three points
    point1 = [claripy.BVS(f"x{n}", 64) for n in range(1, 21)]
    point2 = [claripy.BVS(f"y{n}", 64) for n in range(1, 21)]
    out = [claripy.BVS(f"out{n}", 64) for n in range(1, 21)]

    # 2. add preconditions to input points
    logger.warning("Adding point addition preconditions...")
    ed25519_point_addition_predicate(point1, init_state)
    ed25519_point_addition_predicate(point2, init_state)
    logger.warning("Done adding point addition preconditions.")

    # 3. use some current stack memory and ensure it is aligned
    # i think on X86_64, rsp + 8 has to be 16 byte aligned.
    # we need sizeof(uint64_t) * 20 = 160 bytes for each limb, so
    # allocate 160 bytes per point on the stack,
    size_of_point = claripy.BVV(160, 64)
    
    init_state.regs.rsp = init_state.regs.rsp - size_of_point
    point1_addr = init_state.regs.rsp
    
    init_state.regs.rsp = init_state.regs.rsp - size_of_point
    point2_addr = init_state.regs.rsp

    init_state.regs.rsp = init_state.regs.rsp - size_of_point
    out_addr = init_state.regs.rsp

    init_state.regs.rsp = init_state.regs.rsp - claripy.BVV(8, 64)
    logger.debug(f"point1_addr: {point1_addr}")
    logger.debug(f"point2_addr: {point2_addr}")
    logger.debug(f"out_addr: {out_addr}")

    # 4. put the points in their addresses!
    def store_point_at_addr(point, addr):
        sizeof_uint64_t = claripy.BVV(8, 64)
        cur_addr = addr
        for limb in point:
            logger.debug(f"Storing limb {limb} to addr {cur_addr}")
            init_state.mem[cur_addr].uint64_t = limb
            cur_addr += sizeof_uint64_t

    store_point_at_addr(point1, point1_addr)
    store_point_at_addr(point2, point2_addr)

    # 5. set rdi, rsi, rdx for first three arguments
    # rdi is u64* out
    # rsi is u64* p
    # rdx is u64* q
    init_state.regs.rdi = out_addr
    init_state.regs.rsi = point1_addr
    init_state.regs.rdx = point2_addr
    logger.debug(f"for init_state ({init_state}), rdi holds {init_state.regs.rdi}")
    logger.debug(f"for init_state ({init_state}), rsi holds {init_state.regs.rsi}")
    logger.debug(f"for init_state ({init_state}), rdx holds {init_state.regs.rdx}")

def setup_state_for_curve25519_point_add_and_double(proj, init_state, fn_name):
    # 1. generate arguments
    p01_tmp1_swap = [claripy.BVS(f"p01_tmp1_swap{n}", 8) for n in range (1, 8 * 42)]
    init = [claripy.BVS(f"init{n}", 8) for n in range (1, 8*11)]
    tmp2 = [claripy.BVS(f"init{n}", 8) for n in range (1, 16*11)]

    # 3. use some current stack memory and ensure it is aligned
    # i think on X86_64, rsp + 8 has to be 16 byte aligned.
    # priv key is 32 bytes
    size_of_p01_tmp1_swap = claripy.BVV(64 * 41, 64)
    size_of_init = claripy.BVV(64 * 10, 64)
    size_of_tmp2 = claripy.BVV(128 * 10, 64)
    
    init_state.regs.rsp = init_state.regs.rsp - size_of_p01_tmp1_swap
    p01_tmp1_swap_addr = init_state.regs.rsp
    
    
    init_state.regs.rsp = init_state.regs.rsp - size_of_init
    init_addr = init_state.regs.rsp

    init_state.regs.rsp = init_state.regs.rsp - size_of_tmp2
    tmp2_addr = init_state.regs.rsp

    init_state.regs.rsp = init_state.regs.rsp - claripy.BVV(8, 64)
    logger.warning(f"p01_tmp1_swap_addr: {p01_tmp1_swap_addr}")
    logger.warning(f"init_addr: {init_addr}")
    logger.warning(f"tmp2_addr is: {tmp2_addr}")
    logger.warning(f"rsp is: {init_state.regs.rsp}")

    # 4. put the values in their addresses
    def store_point_at_addr(key, addr):
        sizeof_uint8_t = claripy.BVV(1, 64)
        cur_addr = addr
        for byte in key:
            logger.debug(f"Storing limb {byte} to addr {cur_addr}")
            init_state.mem[cur_addr].uint8_t = byte
            cur_addr += sizeof_uint8_t

    store_point_at_addr(p01_tmp1_swap, p01_tmp1_swap_addr)
    store_point_at_addr(init, init_addr)
    store_point_at_addr(tmp2, tmp2_addr)

    # 5. set rdi, rsi, rdx for first three arguments
    init_state.regs.rdi = init_addr
    init_state.regs.rsi = p01_tmp1_swap_addr
    init_state.regs.rdx = tmp2_addr
    logger.warning(f"for init_state ({init_state}), rdi holds {init_state.regs.rdi}")
    logger.warning(f"for init_state ({init_state}), rsi holds {init_state.regs.rsi}")
    logger.warning(f"for init_state ({init_state}), rdx holds {init_state.regs.rdx}")

    stack_chk_fail_sym_str = "__stack_chk_fail"
    if proj.loader.find_symbol(stack_chk_fail_sym_str):
        logger.warning(f"Hooking {stack_chk_fail_sym_str} with VoidSimProcedure")
        proj.hook_symbol(stack_chk_fail_sym_str, VoidSimProcedure())

    memcpy_chk_fail_sym_str = "__memcpy_chk"
    if proj.loader.find_symbol(memcpy_chk_fail_sym_str):
        logger.warning(f"Hooking {memcpy_chk_fail_sym_str} with VoidSimProcedure")
        proj.hook_symbol(memcpy_chk_fail_sym_str, VoidSimProcedure())

    memcpy_sym_str = "memcpy"
    if proj.loader.find_symbol(memcpy_sym_str):
        logger.warning(f"Hooking {memcpy_sym_str} with angr memcpy SimProcedure")
        proj.hook_symbol(memcpy_sym_str, angr.SIM_PROCEDURES['libc']['memcpy']())

    memset_sym_str = "memset"
    if proj.loader.find_symbol(memset_sym_str):
        logger.warning(f"Hooking {memset_sym_str} with angr memset SimProcedure")
        proj.hook_symbol(memset_sym_str, angr.SIM_PROCEDURES['libc']['memset']())

def output_filename_stem(target_filename: str, target_funcname: str) -> str:
    """
    pulled this out into a function since testing relies on this to check
    compsimpDatacollectionChecker's csv file output
    """
    return f"{Path(target_filename).name}-{target_funcname}"

def setup_solver_globally(args):
    """
    :param args: the namespace (or whichever object) returned from argparse
    """
    global comp_simp_solver

    if args.use_small_bitwidth_solver:
        use_bitwidth = args.bitwidth_for_small_bitwidth_solver

        # TODO: tell argparse that if use_small_bitwidth_solver option
        # enabled, then bitwidth_for_small_bitwidth_solver is also
        # required
        if not use_bitwidth:
            raise("option --bitwidth-for-small-bitwidth-solver "
                  "required if using small bitwidth solver")

        log_msg = f"Running comp simp checker with claripy small ({use_bitwidth}) bitwidth solver"
        logger.warning(log_msg)

        comp_simp_solver = claripy.SmallBitwidthSolver(bitwidth=use_bitwidth)
    else:
        log_msg = f"Running comp simp checker with default claripy solver"
        logger.warning(log_msg)
        comp_simp_solver = default_solver

def run(args):
    """
    1) Load the binary 
    2) Try to find the function in the binary *using on symbols*
    3) Set up angr options for symbolic execution useful for verification and
       helpful for running on cryptographic code
    4) Set up the solver to use based on the command line args
    4) Register the checkers based on the command line args (@args)
    5) Run the symbolic execution
    6) If any results need to be logged, log them

    :param args: the namespace (or whichever object) returned from argparse
    """
    filename = args.path_to_binary
    funcname = args.function_name_symbol

    proj = angr.Project(filename)
    
    func_symbol = proj.loader.find_symbol(funcname)
    if func_symbol:
        print(f"Found symbol: {func_symbol}")
    else:
        print(f"Couldn't find symbol for funcname: {funcname}")
        sys.exit(1)

    state = proj.factory.blank_state(addr=func_symbol.rebased_addr)
    # state.options.add(angr.options.SYMBOL_FILL_UNCONSTRAINED_MEMORY)
    # state.options.add(angr.options.SYMBOL_FILL_UNCONSTRAINED_REGISTERS)
    state.options.add(angr.options.SYMBOLIC_INITIAL_VALUES)
    state.options.add(angr.options.UNDER_CONSTRAINED_SYMEXEC)
    state.options.add(angr.options.LAZY_SOLVES)
    # state.options.add(angr.options.EFFICIENT_STATE_MERGING) maybe?
    state.options.remove('SIMPLIFY_MERGED_CONSTRAINTS')
    state.options.remove('SIMPLIFY_MEMORY_WRITES')
    state.options.remove('SIMPLIFY_REGISTER_WRITES')

    # set up special, global solvers
    setup_solver_globally(args)

    # setup_symbolic_state_for_ed25519_point_addition(proj, state, funcname)
    # setup_state_for_curve25519_point_add_and_double(proj, state, funcname)
    # setup_symbolic_state_for_ed25519_point_addition(proj, state, funcname)

    setup_symbolic_state_for_ed25519_pub_key_gen(proj, state, funcname)
    state.regs.rbp = state.regs.rsp

    if args.comp_simp:
        logger.warning("Checking for potential computation simplification spots")
        # TODO: this should be handled in the comp-simp specific checker
        compsimp_file_name = output_filename_stem(filename, funcname)
        CompSimpDataRecord.set_func_identifier(compsimp_file_name)

        state.inspect.b('expr',
                        when=angr.BP_BEFORE,
                        action=CompSimpDataCollectionChecker.check)
    
    if args.silent_stores:
        logger.warning("Checking for potential silent store spots")
        state.inspect.b('mem_write',
                        when=angr.BP_BEFORE,
                        action=SilentStoreChecker.check)

    simgr = proj.factory.simgr(state)
    simgr.run(opt_level=-1)
    logger.warning("Done running checkers")

    # TODO: this should be handled in the comp-simp specific checker
    if args.comp_simp:
        comp_simp_record_csv_file_name = f"{compsimp_file_name}.csv"
        logger.warning(f"Writing results to {comp_simp_record_csv_file_name}...")
        CompSimpDataCollectionChecker.write_records_to_csv(comp_simp_record_csv_file_name)
        logger.warning("Done writing comp simp results.")

if '__main__' == __name__:
    # argparser.parse_args expects to receive only the CL options
    args = sys.argv[1:]
    parsed_args = argparser.parse_args(args)
    run(parsed_args)
 
