import sys
from pathlib import Path
import typing
from typing import List, Optional
import re
import subprocess

# Make sure ENV var $PYTHONPATH is set or python knows how to find the checker module
# https://docs.python.org/3/tutorial/modules.html#the-module-search-path
import checker

# this is meant to be run from the root of the directory
COMP_SIMP_TEST_DIR = Path("./test/comp-simp")
SILENT_STORES_TEST_DIR = Path("./test/silent-stores")

assert(COMP_SIMP_TEST_DIR.exists())
assert(SILENT_STORES_TEST_DIR.exists())

def get_objs_in_test_dir(test_dir: Path) -> List[Path]:
    return list(test_dir.glob("*.o"))

def get_test_func_name_for_file(test_obj_file_name: Path) -> str:
    return test_obj_file_name.stem

def is_venv():
    return (hasattr(sys, 'real_prefix') or
            (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix))

def run_checker(filename: Path, funcname: str):
    file_abs_path = filename.resolve()
    checker.run(file_abs_path, funcname)

def remove_csv_files_from_dir(d: Path):
    csv_files = list(d.glob("*.csv"))
    # logger.info(f"Removing files: {csv_files}")
    if csv_files:
        for f in csv_files:
            f.unlink()

def remove_insn_files_from_dir(d: Path):
    insn_files = list(d.glob("*.insns"))
    # logger.info(f"Removing files: {insn_files}")
    if insn_files:
        for f in insn_files:
            f.unlink()

def build_test_obj_files() -> bool:
    return run_cmd(['make'])

def remove_test_obj_files() -> bool:
    return run_cmd(['make', 'clean'])

# returns true or false depending on whether `cmd` succeeded
def run_cmd(cmd: List[str], timeout: int = 30) -> bool:
    try:
        subprocess.run(cmd,
                       timeout=timeout, #seconds
                       check=True)
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        # logger.critical("Couldn't run make!")
        return False
    
    return True

def get_csv_value_from_col_name(col_name: str, csv_row: List[str]) -> Optional[str]:
    all_col_names = checker.CompSimpDataRecord.getCSVHeaderColNames()
    if not col_name in all_col_names:
        return None
    else:
        csv_row_idx = all_col_names.index(col_name)
        return csv_row[csv_row_idx]

if '__main__' == __name__:
    obj_files = get_objs_in_test_dir(COMP_SIMP_TEST_DIR)
    test_functions = [get_test_func_name_for_file(f) for f in obj_files]
    test_cases = list(zip(obj_files, test_functions))
    
    # logger.info(f"Test cases (file, test_fn_name) are: {test_cases}")

    first_f, first_fn = test_cases[0]
    run_checker(filename=first_f, funcname=first_fn)