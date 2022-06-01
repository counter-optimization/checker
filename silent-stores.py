import sys
import logging
from abc import ABC, abstractmethod
import typing
from typing import List

import angr
import claripy

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

class Checker(ABC):
    vulnerable_states: List[angr.sim_state.SimState] = NotImplemented
    effects: List[str] = NotImplemented

    @staticmethod
    @abstractmethod
    def check(state: angr.sim_state.SimState) -> bool:
        raise NotImplemented

class SilentStoreChecker(Checker):
    vulnerable_states = []

     # how does this checker affect symbolic state
    effects = ['mem_read', 'mem_write']

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
        
        logger.debug(f"Values could be the same: {result}")
        logger.debug(f"Leaving before mem write at state: {state}")

        return result

class ComputationSimplificationChecker(Checker):
    vulnerable_states = []
    effects = ['mem_read']

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
    assert(len(sys.argv) == expected_num_args + 1)
    
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

    simgr = proj.factory.simgr(state)
    simgr.run()
