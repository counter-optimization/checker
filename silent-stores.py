import sys
import logging
from abc import ABC, abstractmethod
import typing
from typing import List
import re

import angr
import pyvex
import claripy

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

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
    def __init__(self, expr: pyvex.expr.IRExpr, state: angr.sim_state.SimState):
        self.state = state
        self.expr = expr
        
        self.numConstantOperands = 0
        self.firstOperandConst = False
        self.secOperandConst = False
        
        self.numPowerOfTwoOperands = 0
        self.firstOperandPowerOfTwo = False
        self.secondOperandPowerOfTwo = False

        self.numIdentityOperands = 0
        self.firstOperandIdentity = False
        self.secOperandIdentity = False

        self.zeroElementOperands = 0
        self.firstOperandZeroElem = False
        self.secOperandZeroElem = False

        self.numPossibleFirstOperand = 0
        self.numPossibleSecondOperand = 0

        self.serializedLeftOperandFormula = False
        self.serializedRightOperandFormula = False
        
        if type(self.expr) == pyvex.expr.Binop):
            self.handleBinOp()

    def handleBinOp(self):
        assert(type(expr) == pyvex.expr.Binop)
        
        

class CompSimpDataCollectionChecker(Checker):
    """
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
        def finder(e: pyvex.expr.IRExpr) -> bool:
            regex_str = r"Iop_{}\d+".format(pre)
            if re.match(regex_str, e.op, re.IGNORECASE):
                logger.debug(f"Found expr with op: {regex_str}")
                return True
            return False
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
                finder(expr)
            
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
