import csv
import sys
import typing
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

"""
let csv_header : string = "subroutine_name,mir_opcode,addr,tid,problematic_operands,left_operand,right_operand,live_flags,is_live,alert_reason,description,flags_live_in"
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

def has_flags_live_in(csv_row_dict) -> bool:
    return len(csv_row_dict['flags_live_in']) != 0

def get_flags_live_in(csv_row_dict):
    flags_live_in_csv_str = csv_row_dict['flags_live_in']
    flags_live_in = str.split(flags_live_in_csv_str, sep=',')
    return set(flags_live_in)
    
if __name__ == '__main__':
    cs_opcodes = set()
    ss_opcodes = set()

    opcodes_to_live_flags = dict()
    opcodes_to_flags_live_in = dict()
    opcodes_to_addrs_live_out = dict()
    opcodes_to_addrs_live_in = dict()
    subs = set()
    
    for csv_file_name in sys.argv[1:]:
        # calculate mir_opcodes -> live flags for all alerts
        with open(csv_file_name, mode="r") as csv_file:
            reader = csv.DictReader(csv_file)

            logger.info(f"Processing csv file: {csv_file_name}")
            for row in reader:
                logger.debug(f"Opcode ({row['mir_opcode']}) has live flags csv row: {row['live_flags']}")
                logger.debug(f"Opcode ({row['mir_opcode']}) has flags live *in* csv row: {row['flags_live_in']}")
                opcode = row['mir_opcode']

                subs.add(row['subroutine_name'])


                if row['rpo_idx'] != "":
                    logger.critical(f"Row ({row}) has rpo_idx")
                
                if has_flags_live_in(row) or has_live_flags(row):
                    logger.debug(f"Row ({row}) has live flags out: {row['live_flags']}, live flags in: {row['flags_live_in']}")

                if is_comp_simp_warn(row):
                    cs_opcodes.add(opcode)

                if is_silent_store_warn(row):
                    ss_opcodes.add(opcode)

                if has_flags_live_in(row):
                    flags_live_in = get_flags_live_in(row)
                    if opcode == "SHR64ri" and "ZF" in flags_live_in:
                        logger.critical(f"shift with flags live in: {row['addr']} {opcode}")
                    cur_flags = opcodes_to_flags_live_in.get(opcode, set())
                    cur_flags |= flags_live_in
                    opcodes_to_flags_live_in[opcode] = cur_flags

                    addr = row['addr']
                    cur_addrs = opcodes_to_addrs_live_in.get(opcode, set())
                    cur_addrs.add(addr)
                    opcodes_to_addrs_live_in[opcode] = cur_addrs

                if has_live_flags(row):
                    live_flags = get_live_flags(row)
                    cur_flags = opcodes_to_live_flags.get(opcode, set())
                    cur_flags |= live_flags
                    opcodes_to_live_flags[opcode] = cur_flags

                    addrs = row['addr']
                    cur_addrs = opcodes_to_addrs_live_out.get(opcode, set())
                    cur_addrs.add(addrs)
                    opcodes_to_addrs_live_out[opcode] = cur_addrs

    print("Flag live out info:")
    for opcode, live_flags in opcodes_to_live_flags.items():
        addrs = opcodes_to_addrs_live_out[opcode]
        print(f"{opcode} ({len(addrs)}) ({addrs}): {live_flags}")

    print("Flag live in info:")
    for opcode, flags_live_in in opcodes_to_flags_live_in.items():
        addrs = opcodes_to_addrs_live_in[opcode]
        print(f"{opcode} ({len(addrs)}) ({addrs}): {flags_live_in}")

    print("Opcodes of leaky instructions whose flags are used:")
    for opcode, live_flags in opcodes_to_live_flags.items():
        addrs = opcodes_to_addrs_live_out[opcode]
        # print(f"{opcode} ({len(addrs)}) ({addrs}): {live_flags}")
        print(f"{opcode}: {live_flags}")

    print("Opcodes of leaky instructions and the flags they must preserve:")
    for opcode, flags_live_in in opcodes_to_flags_live_in.items():
        addrs = opcodes_to_addrs_live_in[opcode]
        # print(f"{opcode} ({len(addrs)}) ({addrs}): {flags_live_in}")
        print(f"{opcode}: {flags_live_in}")

    print("Printing all CS opcodes")
    for opcode in sorted(cs_opcodes):
        print(opcode)

    print("Printing all SS opcodes")
    for opcode in sorted(ss_opcodes):
        print(opcode)

    print(f"total transforms needed: {len(cs_opcodes) + len(ss_opcodes)}")
