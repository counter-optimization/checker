import sys
import logging
from abc import ABC, abstractmethod
import typing
from typing import List, Optional, Union
import re
import base64
from pathlib import Path
import csv

import angr
import pyvex
import claripy

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

class Checker(ABC):
    vulnerable_states: List[angr.sim_state.SimState] = NotImplemented
    effects: List[str] = NotImplemented

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

        solver = claripy.Solver()
        solver.add(expr == prev_val)
        is_sat = solver.satisfiable() # True if SAT, False if UNSAT/UNKNOWN
        
        if is_sat:
            SilentStoreChecker.vulnerable_states.append(state)
            logger.warning(f"Found state {state} with a silent store")
        else:
            logger.debug(f"No silent store possible in state {state}")
        
        logger.debug(f"Values could be the same: {is_sat}")
        logger.debug(f"Leaving before mem write at state: {state}")

        return is_sat

class CompSimpDataRecord():
    func_identifier = f"{Path(sys.argv[1]).name}-{sys.argv[2]}"
    
    def __init__(self, expr: pyvex.expr.IRExpr, state: angr.sim_state.SimState):
        self.state = state
        self.expr = expr

        self.operation = ""
        self.bitwidth = pyvex.get_type_size(expr.result_type(state.scratch.tyenv))
        
        self.numConstantOperands = 0
        self.firstOperandConst = None
        self.secOperandConst = None

        self.powerOfTwoSignificant = False
        self.numPowerOfTwoOperands = 0
        self.firstOperandPowerOfTwo = False
        self.secondOperandPowerOfTwo = False

        self.hasLeftIdentity = False
        self.hasRightIdentity = False
        self.numIdentityOperands = 0
        self.firstOperandIdentity = False
        self.secOperandIdentity = False

        self.hasLeftZero = False
        self.hasRightZero = False
        self.zeroElementOperands = 0
        self.firstOperandZeroElem = False
        self.secOperandZeroElem = False

        self.numPossibleFirstOperand = None
        self.numPossibleSecondOperand = None

        # base64 encoded for shoving into csv
        self.serializedFirstOperandFormula = ""
        self.serializedSecondOperandFormula = ""

        self.writeInsns()

        if type(self.expr) == pyvex.expr.Binop:
            self.handleBinOp()

    def operandIsConst(self, expr):
        return type(expr) == pyvex.expr.Const

    def isTmpVar(self, expr) -> bool:
        return type(expr) == pyvex.expr.RdTmp

    def isSupportedOperandType(self, expr) -> bool:
        return self.isTmpVar(expr) or self.operandIsConst(expr)

    def getTmpSymbolicValue(self, expr):
        assert(self.isTmpVar(expr))
        return self.state.scratch.temps[expr.tmp]

    def couldBePowerOfTwo(self, symval) -> bool:
        # symval != 0 and ((symval & (symval - 1)) == 0)
        # https://stackoverflow.com/questions/600293/how-to-check-if-a-number-is-a-power-of-2
        if not symval:
            return False
        
        one = claripy.BVV(1, self.bitwidth)
        zero = claripy.BVV(0, self.bitwidth)
        solver = claripy.Solver()
        solver.add(symval != zero)
        solver.add((symval & (symval - one)) == zero)
        is_sat = solver.satisfiable()
        return is_sat

    def couldBePowerOfTwoConcrete(self, concval) -> bool:
        assert(concval)
        cond = concval != 0
        cond = cond and ((concval & (concval - 1)) == 0)
        return cond

    def couldBeRightIdentity(self, expr) -> bool:
        if not expr:
            return False
        solver = claripy.Solver()
        solver.add(expr == self.rightIdentity)
        is_sat = solver.satisfiable()
        return is_sat

    def couldBeRightIdentityConcrete(self, expr) -> bool:
        assert(expr)
        return expr == self.rightIdentity

    def couldBeLeftIdentity(self, expr) -> bool:
        if not expr:
            return False
        solver = claripy.Solver()
        solver.add(expr == self.leftIdentity)
        is_sat = solver.satisfiable()
        return is_sat

    def couldBeLeftIdentityConcrete(self, expr) -> bool:
        assert(expr)
        return expr == self.leftIdentity

    def checkForSpecialValues(self, expr, isLeft: bool):
        if self.powerOfTwoSignificant:
            if self.couldBePowerOfTwo(expr):
                self.numPowerOfTwoOperands += 1
                self.firstOperandPowerOfTwo = True

        if isLeft:
            if self.hasLeftIdentity:
                if self.couldBeLeftIdentity(expr):
                    self.numIdentityOperands += 1
                    self.firstOperandIdentity = True

            if self.hasLeftZero:
                pass
        else:
            if self.hasRightIdentity:
                if self.couldBeRightIdentity(expr):
                    self.numIdentityOperands += 1
                    self.secOperandIdentity = True
            if self.hasRightZero:
                pass

    def checkForSpecialValuesConcrete(self, expr, isLeft: bool):
        if self.powerOfTwoSignificant:
            if self.couldBePowerOfTwoConcrete(expr):
                self.numPowerOfTwoOperands += 1
                self.firstOperandPowerOfTwo = True

        if isLeft:
            if self.hasLeftIdentity:
                if self.couldBeLeftIdentityConcrete(expr):
                    self.numIdentityOperands += 1
                    self.firstOperandIdentity = True

            if self.hasLeftZero:
                pass
        else:
            if self.hasRightIdentity:
                if self.couldBeRightIdentityConcrete(expr):
                    self.numIdentityOperands += 1
                    self.secOperandIdentity = True
            if self.hasRightZero:
                pass

    def handleBinOp(self):
        assert(type(self.expr) == pyvex.expr.Binop)
        # in two-address, firstOperand is dst
        # e.g., in angr, addq rax, rbx is:
        # $tmpXXX = Add64(RdTmp($tmpXXX), RdTmp($tmpYYY))
        firstOperand = self.expr.args[0]
        secondOperand = self.expr.args[1]
        operation = self.expr.op
        self.operation = operation
        logger.debug(f"In handleBinOp: firstOperand is: {firstOperand}")
        logger.debug(f"In handleBinOp: secondOperand is: {secondOperand}")

        if not self.isSupportedOperandType(firstOperand):
            warn_str = "In handlBinOp: unsupported operand type for operand: "
            warn_str.append(str(firstOperand))
            warn_str.append(" with type: ")
            warn_str.append(type(firstOperand))
            logger.critical(warn_str)
            raise RuntimeError(warn_str)

        if not self.isSupportedOperandType(secondOperand):
            warn_str = "In handlBinOp: unsupported operand type for operand: "
            warn_str.append(str(secondOperand))
            warn_str.append(" with type: ")
            warn_str.append(type(secondOperand))
            logger.critical(warn_str)
            raise RuntimeError(warn_str)

        # TODO: so this symval should be handled at least for expr: RdTmp,
        # expr: Const, expr: Reg(?) in addition to just tmp vars
        firstOperandSymVal = None
        secondOperandSymVal = None

        if self.isTmpVar(firstOperand):
            firstOperandSymVal = self.getTmpSymbolicValue(firstOperand)

        if self.isTmpVar(secondOperand):
            secondOperandSymVal = self.getTmpSymbolicValue(secondOperand)

        if self.operandIsConst(firstOperand):
            self.firstOperandConst = firstOperand.con.value
            self.numConstantOperands += 1
            self.checkForSpecialValuesConcrete(firstOperand, isLeft=True)
        elif self.isTmpVar(firstOperand):
            self.checkForSpecialValues(firstOperandSymVal, isLeft=True)
        else:
            raise RuntimeError(f"Unsupported operand type, firstOperand: {firstOperand}")

        if self.operandIsConst(secondOperand):
            self.secOperandConst = secondOperand.con.value
            self.numConstantOperands += 1
            self.checkForSpecialValuesConcrete(secondOperand, isLeft=False)
        elif self.isTmpVar(secondOperand):
            self.checkForSpecialValues(secondOperandSymVal, isLeft=False)
        else:
            raise RuntimeError(f"Unsupported operand type, secondOperand: {secondOperand}")

    def writeInsns(self):
        logger.debug(f"Writing insns to {CompSimpDataRecord.func_identifier}")
        insns_file = Path.cwd() / Path(f"{CompSimpDataRecord.func_identifier}.insns")
        if not insns_file.exists():
            with insns_file.open(mode='w') as f:
                block = state.project.factory.block(self.state.addr)
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
                'secOperandConst',
                'numPowerOfTwoOperands',
                'firstOperandPowerOfTwo',
                'secondOperandPowerOfTwo',
                'numIdentityOperands',
                'firstOperandIdentity',
                'secOperandIdentity',
                'zeroElementOperands',
                'firstOperandZeroElem',
                'secOperandZeroElem',
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

    def getCSVRow(self) -> str:
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
        self.rightIdentity = claripy.BVV(0, self.bitwidth)
        self.leftIdentity = claripy.BVV(0, self.bitwidth)
        self.powerOfTwoSignificant = False

class MulDataRecord(CompSimpDataRecord):
   def __init__(self, expr, state):
        super().__init__(expr, state)
        self.hasRightIdentity = True
        self.hasLeftIdentity = True
        self.hasRightZero = True
        self.hasLeftZero = True
        self.rightIdentity = claripy.BVV(1, self.bitwidth)
        self.leftIdentity = claripy.BVV(1, self.bitwidth)
        self.rightZero = claripy.BVV(0, self.bitwidth)
        self.leftZero = claripy.BVV(0, self.bitwidth)
        self.powerOfTwoSignificant = True
                
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
    filtered_ops = ['Add', 'Or', 'Mul', 'Sub', 'Xor', 'And', 'Mod',
                    'DivS', 'DivU', 'Shl', 'Shr', 'Sar']
    finders = []
    csv_records = []

    @staticmethod
    def binOpNameFinderForMnemonicPrefix(pre: str):
        def finder(e: pyvex.expr.IRExpr) -> Optional[CompSimpDataRecord]:
            regex_str = r"Iop_{}\d+".format(pre)
            if re.match(regex_str, e.op, re.IGNORECASE):
                logger.debug(f"Found expr with op: {regex_str}")
                if pre == 'Add':
                    return AddDataRecord
                elif pre == 'Mul':
                    return MulDataRecord
                else:
                    return None
            return None
        return finder

    @staticmethod
    def check(state: angr.sim_state.SimState) -> bool:
        expr = state.inspect.expr

        if not CompSimpDataCollectionChecker.finders:
            CompSimpDataCollectionChecker.finders = list(map(CompSimpDataCollectionChecker.binOpNameFinderForMnemonicPrefix,
                                                             CompSimpDataCollectionChecker.filtered_ops))
        
        if isinstance(expr, pyvex.expr.Binop):
            for finder in CompSimpDataCollectionChecker.finders:
                if not finder:
                    pass
                dataRecordingClass = finder(expr)
                if dataRecordingClass:
                    record = dataRecordingClass(expr, state)
                    csv_record = record.getCSVRow()
                    CompSimpDataCollectionChecker.csv_records.append(csv_record)

        return False

    @staticmethod
    def write_records_to_csv(csv_file_name: str):
        csv_file_path = Path(csv_file_name)
        if csv_file_path.exists():
            logger.debug(f"csv file {csv_file_path} exists, unlinking...")
            csv_file_path.unlink()

        csv_file = csv.writer(csv_file_path.open(mode="a"))

        header_cols = CompSimpDataRecord.getCSVHeaderColNames()
        csv_file.writerow(header_cols)

        csv_file.writerows(CompSimpDataCollectionChecker.csv_records)

class ComputationSimplificationChecker(Checker):
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
    
def setup_symbolic_state_for_ed25519_point_addition(proj, init_state):
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
    if "hacl" not in sys.argv[2].lower():
        return
    else:
        logger.warn("Setting up symbolic state for ed25519 comp simp checking")

    # 1. generate three points
    point1 = [claripy.BVS(f"x{n}", 64) for n in range(1, 21)]
    point2 = [claripy.BVS(f"y{n}", 64) for n in range(1, 21)]
    out = [claripy.BVS(f"out{n}", 64) for n in range(1, 21)]

    # 2. add preconditions to input points
    logger.debug("Adding preconditions...")
    ed25519_point_addition_predicate(point1, init_state)
    ed25519_point_addition_predicate(point2, init_state)
    logger.debug("Done adding preconditions.")

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
            logger.debug(f"Storing limb {limb} to addr {cur_addr")
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
    
if '__main__' == __name__:
    expected_num_args = 2
    assert(len(sys.argv) - 1 == expected_num_args)
    
    filename = sys.argv[1]
    funcname = sys.argv[2]

    proj = angr.Project(filename)
    func_symbol = proj.loader.find_symbol(funcname)
    if func_symbol:
        print(f"Found symbol: {func_symbol}")
    else:
        print(f"Couldn't find symbol for funcname: {funcname}")

    state = proj.factory.entry_state(addr=func_symbol.rebased_addr)
    state.options.add(angr.options.SYMBOL_FILL_UNCONSTRAINED_MEMORY)
    state.options.add(angr.options.SYMBOL_FILL_UNCONSTRAINED_REGISTERS)

    # For now, just print the code before running checkers. TODO: need
    # to later figure out how to output problematic states for a checker
    # in a way that is debuggable
    proj.factory.block(state.addr).pp()
    
    state.inspect.b('mem_write',
                    when=angr.BP_BEFORE,
                    action=SilentStoreChecker.check)

    state.inspect.b('expr',
                    when=angr.BP_BEFORE,
                    action=CompSimpDataCollectionChecker.check)

    simgr = proj.factory.simgr(state)
    simgr.run()

    comp_simp_record_csv_file_name = f"{Path(sys.argv[1]).name}-{sys.argv[2]}.csv"
    CompSimpDataCollectionChecker.write_records_to_csv(comp_simp_record_csv_file_name)
