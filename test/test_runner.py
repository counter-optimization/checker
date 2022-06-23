import sys
from pathlib import Path
import typing
import logging

# Make sure ENV var $PYTHONPATH is set or python knows how to find the checker module
# https://docs.python.org/3/tutorial/modules.html#the-module-search-path
import checker

logger = logging.getLogger("Checker Test Harness")
logger.setLevel(logging.INFO)

COMP_SIMP_TEST_DIR = Path("comp-simp")
SILENT_STORES_TEST_DIR = Path("silent-stores")

def get_objs_in_test_dir(test_dir: Path):
    return list(test_dir.glob("*.o"))

def is_venv():
    return (hasattr(sys, 'real_prefix') or
            (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix))

def test_runner(filename: str, funcname: str):
    pass

if '__main__' == __name__:
    print(get_objs_in_test_dir(COMP_SIMP_TEST_DIR))
