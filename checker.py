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
logger.setLevel(logging.INFO)

get_default_solver = lambda: claripy.Solver()
get_comp_simp_solver = None

argparser = argparse.ArgumentParser()

argparser.add_argument('--version', 
                        action='version',
                        version=__version__)

argparser.add_argument('--verbose',
                       action='store_true',
                       help='print debug log statements. if not provided, then '
                            'the logging level is logging.INFO')

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

comp_simp_arg_group.add_argument('--use-interval-analysis', 
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

utility_group = argparser.add_argument_group('Helpers and utilities')
utility_group.add_argument('--just-print-cfg-vex', 
                            action="store_true",
                            help='print the vex ir of `function_name_symbol`'
                                 ' and exit')

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

        logger.debug("in ss check: expr (%s) size is %s, addr to load is %s" %
                     (expr, expr.size(), addr))

        size_of_byte = 8
        bytes_to_load = expr.size() // size_of_byte
        logger.debug("bytes_to_load are: %s" % bytes_to_load)
        prev_val = state.memory.load(addr,
                                     size=bytes_to_load,
                                     disable_actions=True,
                                     inspect=False)

        logger.debug("prev_val is %s" % prev_val)

        solver = get_default_solver()
        solver.add(expr == prev_val)
        is_sat = solver.satisfiable() # True if SAT, False if UNSAT/UNKNOWN
        
        if is_sat:
            SilentStoreChecker.vulnerable_states.append(state)
            logger.warning(f"Found state {state} with a silent store")
        
        logger.debug(f"Values could be the same: {is_sat}")
        logger.debug(f"Leaving before mem write at state: {state}")

        return is_sat

class CompSimpCheckerError(Exception):
    pass

class CompSimpDataRecord():
    checks_ops: List[str] = []
    func_identifier: str = "" 

    @staticmethod
    def set_func_identifier(name: str):
        CompSimpDataRecord.func_identifier = name
    
    def __init__(self, expr: pyvex.expr.IRExpr, state: angr.sim_state.SimState):
        self.state = state
        self.expr = expr

        self.operation = expr.op
        self.bitwidth = pyvex.get_type_size(expr.result_type(state.scratch.tyenv))
        
        # Sane defaults for csv values
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
        self.leftZero = NotImplemented
        self.rightZero = NotImplemented

    def check(self):
        self.__check_expr(self.expr)

    def __check_expr(self, e: pyvex.expr.IRExpr):
        # only supporting binops right now because of 
        # how left/right ident/zero/etc are defined
        if not isinstance(e, pyvex.expr.Binop):
            error_msg = f"Operand ({e}) arity not supported (not a binop)."
            error_msg += f" State: {self.state}, expr: {self.expr}"
            raise CompSimpCheckerError(error_msg)

        operands = []
        for ir_expr in e.child_expressions:
            operands.append(self.__decode_operand(ir_expr))
        assert(len(operands) == 2) # Should be a binop (for now)

        self.__check_operand(operands[0], is_left=True)
        self.__check_operand(operands[1], is_left=False)

    def __decode_operand(self, o: pyvex.expr.IRExpr) -> claripy.ast.Base:
        """
        Takes an operand to a pyvex expr (vex ir expr) and returns its backing
        claripy expr AST.
        """
        if isinstance(o, pyvex.const.IRConst):
            return self.__wrap_in_claripy_ast(o)
        elif isinstance(o, pyvex.expr.Const):
            value = o.con.value
            return self.__wrap_in_claripy_ast(value)
        elif isinstance(o, pyvex.expr.RdTmp):
            stored_tmp_expr = self.state.scratch.tmp_expr(o.tmp)
            return self.__decode_operand(stored_tmp_expr)
        elif isinstance(o, claripy.ast.Base):
            return o
        else:
            error_msg = f"Operand ({o}) type not supported."
            error_msg += f" State: {self.state}, expr: {self.expr}"
            raise CompSimpCheckerError(error_msg)
            
    def __wrap_in_claripy_ast(self, o) -> claripy.ast.Base:
        if isinstance(o, claripy.ast.Base):
            return o
        elif isinstance(o, int):
            return claripy.BVV(o, self.bitwidth)
        else:
            error_msg = f"Operand ({o}) type not supported."
            error_msg += f" State: {self.state}, expr: {self.expr}"
            raise CompSimpCheckerError(error_msg)

    def __check_operand(self, o: claripy.ast.Base, is_left=True):
        bw = o.length # the operand's bitwidth

        # check if it is a constant
        constants = {'BVV', 'BoolV', 'FPV', 'Int'}
        if o.op in constants:
            logger.debug(f"expr ({self.expr}) has a constant operand")
            self.numConstantOperands += 1
            if is_left:
                self.firstOperandConst = o
            else:
                self.secondOperandConst = o

        # check if it is ident
        if (self.hasLeftIdentity and is_left) or \
            (self.hasRightIdentity and not is_left):
            ident = self.leftIdentity(bw) if is_left else self.rightIdentity(bw)
            is_sat = self.__could_be_true(ident == o)
            if is_sat:
                logger.debug(f"expr ({self.expr}) can have ident element operand")
                self.numIdentityOperands += 1
                if is_left:
                    self.firstOperandIdentity = True
                else:
                    self.secondOperandIdentity = True

        # check if it is zero
        if (self.hasLeftZero and is_left) or (self.hasRightZero and not is_left):
            zero = self.leftZero(bw) if is_left else self.rightZero(bw)
            is_sat = self.__could_be_true(zero == o)
            if is_sat:
                logger.debug(f"expr ({self.expr}) can have zero element operand")
                self.numZeroElementOperands += 1
                if is_left:
                    self.firstOperandZeroElem = True
                else:
                    self.secondOperandZeroElem = True

        # check if it is a power of two
        if self.powerOfTwoSignificant:
            if self.__could_be_power_of_two(o):
                logger.debug(f"expr ({self.expr}) can have power of two operand")
                self.numPowerOfTwoOperands += 1
                if is_left:
                    self.firstOperandPowerOfTwo = True
                else:
                    self.secondOperandPowerOfTwo = True

    def __could_be_power_of_two(self, symval) -> bool:
        # symval != 0 and ((symval & (symval - 1)) == 0)
        # https://stackoverflow.com/questions/600293/how-to-check-if-a-number-is-a-power-of-2
        one = claripy.BVV(1, symval.length)
        zero = claripy.BVV(0, symval.length)
        is_sat = self.__all_could_be_true([symval != zero, 
            (symval & (symval - one)) == zero])
        return is_sat

    def __write_insns(self):
        logger.debug(f"Writing insns to {CompSimpDataRecord.func_identifier}")
        insns_file = Path.cwd() / Path(f"{CompSimpDataRecord.func_identifier}.insns")
        if not insns_file.exists():
            with insns_file.open(mode='w') as f:
                block = self.state.project.factory.block(self.state.addr)
                block.pp()
                f.write(str(block))
                f.write(str(block.vex))

    def __all_could_be_true(self, constraints: List[claripy.ast.Base]) -> bool:
        solver = get_comp_simp_solver()
        for c in constraints:
            solver.add(c)
        is_sat = solver.satisfiable()
        return is_sat

    def __could_be_true(self, constraint: claripy.ast.Base) -> bool:
        solver = get_comp_simp_solver()
        solver.add(constraint)
        is_sat = solver.satisfiable()
        return is_sat

    def address(self) -> str:
        return hex(self.state.addr)

    def expression(self) -> str:
        return str(self.expr)

    @staticmethod
    def get_csv_header_col_names() -> List[str]:
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
                'secondOperandZeroElem']
        return cols

    def getAttributeResult(self, attrname: str) -> str:
        intermediate_result = getattr(self, attrname)
        if callable(intermediate_result):
            return str(intermediate_result())
        else:
            return str(intermediate_result)

    def get_csv_row(self) -> List[str] :
        values = map(lambda x: self.getAttributeResult(x),
                     CompSimpDataRecord.get_csv_header_col_names())
        return list(values)

class AddDataRecord(CompSimpDataRecord):
    checks_ops: List[str] = ["Iop_Add8",  "Iop_Add16",  "Iop_Add32",  "Iop_Add64"]

    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = False
        self.hasLeftZero = False
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.leftIdentity = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = False

class ShiftDataRecord(CompSimpDataRecord):
    checks_ops: List[str] = ["Iop_Shl8",  "Iop_Shl16",  "Iop_Shl32",  "Iop_Shl64",
      "Iop_Shr8",  "Iop_Shr16",  "Iop_Shr32",  "Iop_Shr64",
      "Iop_Sar8",  "Iop_Sar16",  "Iop_Sar32",  "Iop_Sar64"
      ]

    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = False
        self.hasRightZero = False
        self.hasLeftZero = True
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.leftZero = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = False

class SubDataRecord(CompSimpDataRecord):
    checks_ops: List[str] = ["Iop_Sub8",  "Iop_Sub16",  "Iop_Sub32",  "Iop_Sub64"]
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = False
        self.hasRightZero = False
        self.hasLeftZero = False
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = False

class AndDataRecord(CompSimpDataRecord):
    checks_ops: List[str] = ["Iop_And8",  "Iop_And16",  "Iop_And32",  "Iop_And64"]
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

class OrDataRecord(CompSimpDataRecord):
    checks_ops: List[str] = ["Iop_Or8",   "Iop_Or16",   "Iop_Or32",   "Iop_Or64"]
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = False
        self.hasLeftZero = False
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.leftIdentity = self.rightIdentity
        self.powerOfTwoSignificant = False

class XorDataRecord(CompSimpDataRecord):
    checks_ops: List[str] = ["Iop_Xor8",  "Iop_Xor16",  "Iop_Xor32",  "Iop_Xor64"]
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = False
        self.hasLeftZero = False
        self.rightIdentity = lambda bw: claripy.BVV(0, bw)
        self.leftIdentity = self.rightIdentity

class MulDataRecord(CompSimpDataRecord):
    checks_ops: List[str] = ["Iop_Mul8",  "Iop_Mul16",  "Iop_Mul32",  "Iop_Mul64"]
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

class DivDataRecord(CompSimpDataRecord):
    checks_ops: List[str] = [
      "Iop_DivU32",
      "Iop_DivS32",
      "Iop_DivU64",
      "Iop_DivS64",
      "Iop_DivU64E",           
      "Iop_DivS64E",
      "Iop_DivU32E",        
      "Iop_DivS32E",
      "Iop_DivModU64to32",            
      "Iop_DivModS64to32",
      "Iop_DivModU128to64",       
      "Iop_DivModS128to64",
      "Iop_DivModS64to64"
    ]
                       
    def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = False
        self.hasRightZero = False
        self.hasLeftZero = True
        self.rightIdentity = lambda bw: claripy.BVV(1, bw)
        self.leftZero = lambda bw: claripy.BVV(0, bw)
        self.powerOfTwoSignificant = True
                
class CompSimpDataCollectionChecker(Checker):
    """
    Driver class for finding insn expressions of interest
    In charge of filtering out insns, passing the state and expr
    to some class that processes these, and then outputting a CSV
    of all the processes states and exprs.
    """
    vulnerable_states = []
    effects = []
    finders = []
    csv_records = []

    checkers = [AddDataRecord,
                MulDataRecord,
                DivDataRecord,
                ShiftDataRecord,
                SubDataRecord,
                AndDataRecord,
                OrDataRecord,
                XorDataRecord]

    @staticmethod
    def check(state: angr.sim_state.SimState) -> bool:
        expr = state.inspect.expr

        logger.debug(f"Checking expr: {expr}")

        # only check Add, Mul, Sub, etc, not subexprs of those
        # and not get/puts of registers
        if not isinstance(expr, pyvex.expr.Binop):
            return False

        # figure out which comp simp checker(s) to run on the expr
        checkers_to_use = []
        all_checkers = CompSimpDataCollectionChecker.checkers
        operator_name = expr.op

        for checker_clz in all_checkers:
            if operator_name in checker_clz.checks_ops:
                logger.debug(f"Using checker class {checker_clz} to check expr {expr}")
                checkers_to_use.append(checker_clz)

        # run the checkers
        try:
            for checker_clz in checkers_to_use:
                data_record = checker_clz(expr, state)
                data_record.check()
                csv_record = data_record.get_csv_row()
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

            header_cols = CompSimpDataRecord.get_csv_header_col_names()
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
        logger.info(f"Hooking {stack_chk_fail_sym_str} with VoidSimProcedure")
        proj.hook_symbol(stack_chk_fail_sym_str, VoidSimProcedure())

    memcpy_chk_fail_sym_str = "__memcpy_chk"
    if proj.loader.find_symbol(memcpy_chk_fail_sym_str):
        logger.info(f"Hooking {memcpy_chk_fail_sym_str} with VoidSimProcedure")
        proj.hook_symbol(memcpy_chk_fail_sym_str, VoidSimProcedure())

    memcpy_sym_str = "memcpy"
    if proj.loader.find_symbol(memcpy_sym_str):
        logger.info(f"Hooking {memcpy_sym_str} with angr memcpy SimProcedure")
        proj.hook_symbol(memcpy_sym_str, angr.SIM_PROCEDURES['libc']['memcpy']())

    memset_sym_str = "memset"
    if proj.loader.find_symbol(memset_sym_str):
        logger.info(f"Hooking {memset_sym_str} with angr memset SimProcedure")
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
    logger.warning("Setting up symbolic state for ed25519 comp simp checking")

    # 1. generate three points
    point1 = [claripy.BVS(f"x{n}", 64) for n in range(1, 21)]
    point2 = [claripy.BVS(f"y{n}", 64) for n in range(1, 21)]
    out = [claripy.BVS(f"out{n}", 64) for n in range(1, 21)]

    # 2. add preconditions to input points
    logger.debug("Adding point addition preconditions...")
    ed25519_point_addition_predicate(point1, init_state)
    ed25519_point_addition_predicate(point2, init_state)
    logger.debug("Done adding point addition preconditions.")

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
    logger.debug(f"p01_tmp1_swap_addr: {p01_tmp1_swap_addr}")
    logger.debug(f"init_addr: {init_addr}")
    logger.debug(f"tmp2_addr is: {tmp2_addr}")
    logger.debug(f"rsp is: {init_state.regs.rsp}")

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
    logger.debug(f"for init_state ({init_state}), rdi holds {init_state.regs.rdi}")
    logger.debug(f"for init_state ({init_state}), rsi holds {init_state.regs.rsi}")
    logger.debug(f"for init_state ({init_state}), rdx holds {init_state.regs.rdx}")

    stack_chk_fail_sym_str = "__stack_chk_fail"
    if proj.loader.find_symbol(stack_chk_fail_sym_str):
        logger.debug(f"Hooking {stack_chk_fail_sym_str} with VoidSimProcedure")
        proj.hook_symbol(stack_chk_fail_sym_str, VoidSimProcedure())

    memcpy_chk_fail_sym_str = "__memcpy_chk"
    if proj.loader.find_symbol(memcpy_chk_fail_sym_str):
        logger.debug(f"Hooking {memcpy_chk_fail_sym_str} with VoidSimProcedure")
        proj.hook_symbol(memcpy_chk_fail_sym_str, VoidSimProcedure())

    memcpy_sym_str = "memcpy"
    if proj.loader.find_symbol(memcpy_sym_str):
        logger.debug(f"Hooking {memcpy_sym_str} with angr memcpy SimProcedure")
        proj.hook_symbol(memcpy_sym_str, angr.SIM_PROCEDURES['libc']['memcpy']())

    memset_sym_str = "memset"
    if proj.loader.find_symbol(memset_sym_str):
        logger.debug(f"Hooking {memset_sym_str} with angr memset SimProcedure")
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
    global get_comp_simp_solver

    if args.use_small_bitwidth_solver:
        use_bitwidth = args.bitwidth_for_small_bitwidth_solver

        # TODO: tell argparse that if use_small_bitwidth_solver option
        # enabled, then bitwidth_for_small_bitwidth_solver is also
        # required
        if not use_bitwidth:
            raise("option --bitwidth-for-small-bitwidth-solver "
                  "required if using small bitwidth solver")

        log_msg = f"Running comp simp checker with claripy small ({use_bitwidth}) bitwidth solver"
        logger.info(log_msg)

        get_comp_simp_solver = lambda: claripy.SmallBitwidthSolver(bitwidth=use_bitwidth)
    elif args.use_interval_analysis:
        log_msg = "Running comp simp checker with claripy VSA (strided intervals) abstract interpretation solver"
        logger.info(log_msg)
        
        get_comp_simp_solver = lambda: claripy.SolverVSA()
    else:
        log_msg = f"Running comp simp checker with default claripy solver"
        logger.info(log_msg)
        get_comp_simp_solver = get_default_solver

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

    if args.verbose:
        logger.setLevel(logging.DEBUG)
    else:
        logger.setLevel(logging.INFO)

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

    # setup_symbolic_state_for_ed25519_point_addition(proj, state, funcname)
    setup_symbolic_state_for_ed25519_pub_key_gen(proj, state, funcname)
    state.regs.rbp = state.regs.rsp

    if args.comp_simp:
        logger.info("Checking for potential computation simplification spots")
        # TODO: this should be handled in the comp-simp specific checker
        compsimp_file_name = output_filename_stem(filename, funcname)
        CompSimpDataRecord.set_func_identifier(compsimp_file_name)

        state.inspect.b('expr',
                        when=angr.BP_BEFORE,
                        action=CompSimpDataCollectionChecker.check)
    
    if args.silent_stores:
        logger.info("Checking for potential silent store spots")
        state.inspect.b('mem_write',
                        when=angr.BP_BEFORE,
                        action=SilentStoreChecker.check)

    simgr = proj.factory.simgr(state)
    simgr.run(opt_level=-1)
    logger.info("Done running checkers")

    # TODO: this should be handled in the comp-simp specific checker
    if args.comp_simp:
        comp_simp_record_csv_file_name = f"{compsimp_file_name}.csv"
        logger.info(f"Writing results to {comp_simp_record_csv_file_name}...")
        CompSimpDataCollectionChecker.write_records_to_csv(comp_simp_record_csv_file_name)
        logger.info("Done writing comp simp results.")

def print_cfg_vex(args):
    filename = args.path_to_binary
    funcname = args.function_name_symbol
    logger.info("only printing the cfg for func %s" % funcname)

    proj = angr.Project(filename)
    
    func_symbol = proj.loader.find_symbol(funcname)
    if func_symbol:
        logger.info(f"Found symbol: {func_symbol}")
    else:
        logger.critical(f"Couldn't find symbol for funcname: {funcname}")
        sys.exit(1)

    logger.info("Building the CFG for the project...")
    cfg = proj.analyses.CFGFast()
    logger.info("Project CFG built")
    func_cfg = cfg.kb.functions[func_symbol.rebased_addr]

    for bb_addr in func_cfg.block_addrs:
        proj.factory.block(addr=bb_addr, opt_level=-1).vex.pp()

    logger.info("done")

if '__main__' == __name__:
    # argparser.parse_args expects to receive only the CL options
    args = sys.argv[1:]
    parsed_args = argparser.parse_args(args)

    if parsed_args.just_print_cfg_vex:
        print_cfg_vex(parsed_args)
    else:
        run(parsed_args)
 
