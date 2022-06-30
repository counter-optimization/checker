import subprocess
import logging

logging.basicConfig()

insn_lengths = list(range(1, 11))

command = ["racket", "synth-comp-simp-defenses.rkt"]

for length in insn_lengths:
    cur_cmd = [command[0], command[1], str(length)]
    logging.warning(f"Running command: {' '.join(cur_cmd)}")

    try:
        subprocess.run(cur_cmd, check=True)
    except subprocess.CalledProcessError:
        logging.critical(f"Couldn't find insn sequence for length {length}")

