import sys
import logging
from abc import ABC, abstractmethod
import typing
from typing import List, Optional, Union
import re
import base64
from pathlib import Path

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
        result = solver.satisfiable() # True if SAT, False if UNSAT/UNKNOWN
        
        if result:
            SilentStoreChecker.vulnerable_states.append(state)
            logger.warning(f"Found state {state} with a silent store")
        else:
            logger.debug(f"No silent store possible in state {state}")
        
        logger.debug(f"Values could be the same: {result}")
        logger.debug(f"Leaving before mem write at state: {state}")

        return result

class CompSimpDataRecord():
    func_identifier = f"{Path(sys.argv[1]).name}-{sys.argv[2]}"
    
    def __init__(self, expr: pyvex.expr.IRExpr, state: angr.sim_state.SimState):
        self.state = state
        self.expr = expr

        self.operation = ""
        self.bitwidth = 0
        
        self.numConstantOperands = 0
        self.firstOperandConst = None
        self.secOperandConst = None

        self.powerOfTwoSignificant = False
        self.numPowerOfTwoOperands = 0
        self.firstOperandPowerOfTwo = None
        self.secondOperandPowerOfTwo = None

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

    def getTmpSymbolicValue(self, expr):
        assert(self.isTmpVar(expr))
        return self.state.scratch.temps[expr.tmp]

    def isPowerOfTwo(self, symval) -> bool:
        # symval != 0 and ((symval & (symval - 1)) == 0)
        # https://stackoverflow.com/questions/600293/how-to-check-if-a-number-is-a-power-of-2
        return False

    def handleBinOp(self):
        assert(type(self.expr) == pyvex.expr.Binop)
        # in two-address, firstOperand is dst
        # e.g., in angr, addq rax, rbx is:
        # $tmpXXX = Add64(RdTmp($tmpXXX), RdTmp($tmpYYY))
        firstOperand = self.expr.args[0]
        secondOperand = self.expr.args[1]
        operation = self.expr.op
        logger.debug(f"In handleBinOp: firstOperand is: {firstOperand}")
        logger.debug(f"In handleBinOp: secondOperand is: {secondOperand}")

        firstOperandSymVal = None
        secondOperandSymVal = None

        if self.isTmpVar(firstOperand):
            firstOperandSymVal = self.getTmpSymbolicValue(firstOperand)

        if self.isTmpVar(secondOperand):
            secondOperandSymVal = self.getTmpSymbolicValue(secondOperand)

        self.operation = operation

        if self.operandIsConst(firstOperand):
            self.firstOperandConst = firstOperand.con.value
            self.numConstantOperands += 1

        if self.operandIsConst(secondOperand):
            self.secOperandConst = secondOperand.con.value
            self.numConstantOperands += 1

        if self.powerOfTwoSignificant:
            pass

        if self.hasLeftIdentity:
            pass

        if self.hasRightIdentity:
            pass

        if self.hasLeftZero:
            pass

        if self.hasRightZero:
            pass

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

    def getCSVHeader(self) -> List[str]:
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
                     self.getCSVHeader())
        return ','.join(values)

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
    INFO    | 2022-05-31 23:01:36,166 | __main__ | expr Shl64(t349,0x01) (<class 'pyvex.expr.Binop'>) is binop
    INFO    | 2022-05-31 23:01:36,166 | __main__ | expr Shl64(t349,0x01) op: Iop_Shl64. tag: Iex_Binop
    INFO    | 2022-05-31 23:01:36,184 | __main__ | expr And64(t440,t157) (<class 'pyvex.expr.Binop'>) is binop
    INFO    | 2022-05-31 23:01:36,184 | __main__ | expr And64(t440,t157) op: Iop_And64. tag: Iex_Binop
    """
    vulnerable_states = []
    effects = []

    filtered_ops = ['Add', 'Or', 'Mul', 'Sub', 'Xor', 'And', 'Mod',
                    'DivS', 'DivU', 'Shl', 'Shr', 'Sar']
    finders = []

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
            CompSimpDataCollectionChecker.finders = list(map(CompSimpDataCollectionChecker.binOpNameFinderForMnemonicPrefix, CompSimpDataCollectionChecker.filtered_ops))
        
        if isinstance(expr, pyvex.expr.Binop):
            logger.debug(f"expr {expr} ({type(expr)}) is binop")
            logger.debug(f"expr {expr} op: {expr.op}. tag: {expr.tag}")

            for finder in CompSimpDataCollectionChecker.finders:
                dataRecordingClass = finder(expr)
                if dataRecordingClass:
                    record = dataRecordingClass(expr, state)
                    logger.debug(record.getCSVRow())
            
        return False

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

    
if '__main__' == __name__:
    expected_num_args = 2
    assert(len(sys.argv) - 1 == expected_num_args)
    
    filename = sys.argv[1]
    funcname = sys.argv[2]

    proj = angr.Project(filename)
    func_symbol = proj.loader.find_symbol(funcname)
    print(f"Found symbol: {func_symbol}")

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
