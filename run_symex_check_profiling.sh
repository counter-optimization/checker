#!/bin/bash

function run_bap {
    bap --plugin-path=. --pass=uarch-checker \
	--uarch-checker-output-csv-file=./libna.checker.output.csv \
	--uarch-checker-config-file=libsodium.uarch_checker.config \
	--uarch-checker-ss \
	--uarch-checker-cs \
	--uarch-checker-symex-profiling-output-file=./symex-profiling-data-$1.csv \
	--no-optimization --bil-optimization=0 \
	$2 &> $3
}
