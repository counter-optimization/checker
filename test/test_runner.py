import sys
from pathlib import Path
import typing
from typing import List
import logging
import re

# Make sure ENV var $PYTHONPATH is set or python knows how to find the checker module
# https://docs.python.org/3/tutorial/modules.html#the-module-search-path
import checker

logger = logging.getLogger("Checker Test Harness")
logger.setLevel(logging.INFO)

COMP_SIMP_TEST_DIR = Path("comp-simp")
SILENT_STORES_TEST_DIR = Path("silent-stores")

def get_objs_in_test_dir(test_dir: Path) -> List[Path]:
    return list(test_dir.glob("*.o"))

def get_test_func_name_for_file(test_obj_file_name: Path) -> str:
    return test_obj_file_name.stem

def is_venv():
    return (hasattr(sys, 'real_prefix') or
            (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix))

def test_runner(filename: Path, funcname: str):
    file_abs_path = filename.resolve()
    checker.run(file_abs_path, funcname)

if '__main__' == __name__:
    obj_files = get_objs_in_test_dir(COMP_SIMP_TEST_DIR)
    test_functions = [get_test_func_name_for_file(f) for f in obj_files]
    test_cases = list(zip(obj_files, test_functions))
    
    logger.info(f"Test cases (file, test_fn_name) are: {test_cases}")

    first_f, first_fn = test_cases[0]
    test_runner(first_f, first_fn)
