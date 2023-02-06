import csv
import sys
import typing
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

"""
let csv_header : string = "subroutine_name,mir_opcode,addr,tid,problematic_operands,left_operand,right_operand,live_flags,is_live,alert_reason,description"
"""

def is_comp_simp_warn(csv_row_dict) -> bool:
    return csv_row_dict['alert_reason'] == 'comp-simp'

def is_silent_store_warn(csv_row_dict) -> bool:
    return csv_row_dict['alert_reason'] == 'silent-stores'

def has_live_flags(csv_row_dict) -> bool:
    return len(csv_row_dict['live_flags']) != 0

def get_live_flags(csv_row_dict):
    live_flags_csv_str = csv_row_dict['live_flags']
    live_flags = str.split(live_flags_csv_str, sep=',')
    return set(live_flags)

if __name__ == '__main__':
    cs_opcodes = set()
    ss_opcodes = set()
    
    for csv_file_name in sys.argv[1:]:
        # calculate mir_opcodes -> live flags for all alerts
        with open(csv_file_name, mode="r") as csv_file:
            reader = csv.DictReader(csv_file)

            opcodes_to_live_flags = dict()
            opcodes_to_addrs = dict()
            subs = set()

            logger.info(f"Processing csv file: {csv_file_name}")
            for row in reader:
                logger.debug(f"Opcode ({row['mir_opcode']}) has live flags csv row: {row['live_flags']}")
                opcode = row['mir_opcode']

                subs.add(row['subroutine_name'])

                if is_comp_simp_warn(row):
                    cs_opcodes.add(opcode)

                if is_silent_store_warn(row):
                    ss_opcodes.add(opcode)

                if is_comp_simp_warn(row) and has_live_flags(row):
                    live_flags = get_live_flags(row)
                    cur_flags = opcodes_to_live_flags.get(opcode, set())
                    cur_flags |= live_flags
                    opcodes_to_live_flags[opcode] = cur_flags

                    addrs = row['addr']
                    cur_addrs = opcodes_to_addrs.get(opcode, set())
                    cur_addrs.add(addrs)
                    opcodes_to_addrs[opcode] = cur_addrs

            logger.info("Printing live flag info")
            for opcode, live_flags in opcodes_to_live_flags.items():
                addrs = opcodes_to_addrs[opcode]
                print(f"{opcode} ({len(addrs)}) ({addrs}): {live_flags}")

    logger.info("Printing all CS opcodes")
    for opcode in sorted(cs_opcodes):
        print(opcode)

    logger.info("Printing all SS opcodes")
    for opcode in sorted(ss_opcodes):
        print(opcode)
