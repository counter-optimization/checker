#!/bin/bash

function run_bap {
    bap --plugin-path=. --pass=uarch-checker \
	--uarch-checker-output-csv-file=./libna.checker.output.csv \
	--uarch-checker-config-file=libsodium.uarch_checker.config \
	--uarch-checker-ss \
	--uarch-checker-cs \
	--no-optimization --bil-optimization=0 \
	$1 >> $2 2>&1
}

while true; do
    run_bap $1 $2 $3
done
