#!/usr/bin/env bash

# Script for running rule analysis with various settings.

(
    # Run the jobs generator.
    R < ./02-rule-analysis-jobs.R --no-save --no-restore \
        > ../logs/02-rule-analysis-jobs.Rout 2>&1;

    jobs=./02-rule-analysis-jobs.csv
    echo "Jobs file MD5 checksum:"
    md5sum ${jobs}
    cat ${jobs}

    max=$(awk 'END {print NR - 1}' ${jobs})
    echo -e "\nMax jobs: ${max}"

    script=./03-rule-analysis.R
    echo -e "\nScript file MD5 checksum:"
    md5sum ${script}

    echo ""
    for i in $(seq 1 "${max}"); do
        echo -e "\n### Processing Job ${i} ...\n"
        suffix=$(awk -v id="${i}" 'NR == (id + 1) {gsub(",", "-", $0); print}' ${jobs})
        echo "Suffix: ${suffix}"
        log="../logs/03-rulue-analysis/job-${suffix}.Rout"
        if ! R < ${script} --no-save --no-restore --args "${i}" > "${log}" 2>&1; then
            echo "Error in the R script:"
            grep -C 2 -i "error" "${log}";
        fi
        grep -i "warning" "${log}";
    done
    echo "Done."
) 2>&1 | tee ../logs/03-rule-analysis-bash.log # self log
