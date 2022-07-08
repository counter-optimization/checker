import csv
from pathlib import Path
import sys
import logging

import test_common

logger = logging.getLogger("csv_filter")
logger.setLevel(logging.DEBUG)

filepath = Path(sys.argv[1])
assert(filepath.exists())

filtered_filepath = Path(sys.argv[2])

if filtered_filepath.exists():
    filtered_filepath.unlink()

fh = filtered_filepath.open(mode="w")
writer = csv.writer(fh)

def get_value(col_name):
    return test_common.get_csv_value_from_col_name(col_name, row)

with filepath.open() as f:
    reader = csv.reader(f)
    is_header = True
    row_num = 1
    for row in reader:
        if is_header:
            writer.writerow(row)
            continue
        if get_value('firstOperandConst') is None:
            if get_value('firstOperandPowerOfTwo') or \
               get_value('firstOperandIdentity') or \
               get_value('firstOperandZeroElem'):
                logger.debug(f"writing row num {row_num}")
                writer.writerow(row)
        if get_value('secondOperandConst') is None:
            if get_value('secondOperandPowerOfTwo') or \
               get_value('secondOperandIdentity') or \
               get_value('secondOperandZeroElem'):
                logger.debug(f"writing row num {row_num}")
                writer.writerow(row)
        row_num += 1
