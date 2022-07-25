if [[ ! -d log ]]; then
    mkdir log
fi

starts=`seq 0 6 18`

for start in ${starts}; do
    echo Batch start is $start
    end=$((start+6))

    for insn_seq_len in `seq ${start} ${end}`; do
        echo Running insn_seq_len: $insn_seq_len

        # generate the hardcoded insn sequence file
        subbed_file_name=synth-comp-simp-defenses-macrod-${insn_seq_len}.rkt
        cat synth-comp-simp-defenses-macrod.rkt \
        | sed 's/REPLACE_ME/${insn_seq_len}/' > ${subbed_file_name}

        racket ${subbed_file_name} ${insn_seq_len} &>./log/hard-coded-synthesis-seq-len-$\
{insn_seq_len}-`date | tr ' ' '-'`.log &
        pids[$i]=$!
        echo Done running insn_seq_len: $insn_seq_len
    done

    for pid_no in ${pids[*]}; do
        echo Waiting on pid $pid_no
        wait $pid_no
    done
done

# start_insn_seq_len=1
# end_insn_seq_len=20
# for len in `seq ${start_insn_seq_len} ${end_insn_seq_len}`; do
#     log_file=`date -u | tr ' ' '-'`-len-${len}.log
#     echo Running synth for insn seq len: $len
#     racket synth-comp-simp-defenses.rkt $len >> ${log_file} 2>&1
#     exit_code=$?
#     echo Synth for insn seq len had exit code: $exit_code
#     echo Synth for insn seq len had exit code: $exit_code >> ${log_file}
#     echo Done running synth for insn seq len: $len
# done