import csv
import sys
import re
from random import choices

import matplotlib.pyplot as plt
import numpy as np
import statistics

import argparse

parser = argparse.ArgumentParser(prog='Symex profiling data parser')
parser.add_argument('-t', '--is-twenty-five', action='store_true')

# silentstores uses leftconstraints
symex_profiling_csv_fieldnames = ['is_cs', 'is_ss','sstime','lefttime','righttime','leftconstraints','rightconstraints','dependentdefs']

binop_syms = [' + ',
              ' - ',
              ' * ',
              ' / ',
              ' % ',
              ' < ',
              ' > ',
              ' & ',
              ' | ',
              ' ^ ',
              ' << ',
              ' >> ']

def contains_128_bit_mul(depdefs):
     """
     not a fullproof check since not handling XMM/YMM regs,
     just this format:
     extend:128[RCX] * extend:128[RSI]
     """
     target_rex = re.compile(r"(extend|pad):128\[.*?\] \* (extend|pad):128\[.*?\]")
     is_found = re.search(target_rex, depdefs) is not None
     return is_found

def pull_symbol_distributions(dependentdefs):
    counts = [dependentdefs.count(sym) for sym in binop_syms]
    count_alist = list(zip(binop_syms, counts))
    return count_alist

if __name__ == '__main__':
    filenames = sys.argv[1:]

    args, filenames = parser.parse_known_args()

    is_twenty_five_bound = False
    if args.is_twenty_five:
        is_twenty_five_bound = True

    print(f"is_twenty_five_bound: {is_twenty_five_bound}")

    ss = []
    cs = []
    ss_times = []
    cs_times = []

    for filename in filenames:
        with open(filename, mode='r') as csvfile:
            reader = csv.DictReader(csvfile, fieldnames=symex_profiling_csv_fieldnames)
        
            for row in reader:
                if row['is_ss']:
                    sstime = int(row['sstime'])
                    constr = row['leftconstraints']
                    depdefs = row['dependentdefs']
                    ss_times.append(sstime)
                    ss.append(row)

                if row['is_cs']:
                    lefttime = int(row['lefttime'])
                    righttime = int(row['righttime'])
                    leftconstr = row['leftconstraints']
                    rightconstr = row['rightconstraints']
                    depdefs = row['dependentdefs']
                    cs_times.append(lefttime) 
                    cs_times.append(righttime)
                    row['binop_dist'] = pull_symbol_distributions(depdefs)
                    cs.append(row)

    # print(f"ss times are: {ss_times}")
    # print(f"cs times are: {sorted(cs_times)}")

    print(f"there are {len(ss_times)} ss times")
    print(f"there are {len(cs_times)} cs times")
    print(f"min ss time is {min(ss_times)}, max: {max(ss_times)}")
    print(f"min cs time is {min(cs_times)}, max: {max(cs_times)}")

    # silent store optimization statistics and plotting
    num_samples = 1000
    sample_sz = 3000
    ss_times = np.array(ss_times)
    total_ss_times = []
    for i in range(num_samples):
        sample = choices(ss_times,
                         k=sample_sz)
        total_ss_times = np.array(sample).sum()
    print(f"bootstrapped avg total ss time: {np.mean(total_ss_times)}, stdev: {np.std(total_ss_times)}")

    fig, ax = plt.subplots(figsize=(11,8))
    n, bins, _ = ax.hist(ss_times, density=True)
    print(f"in plotting ss times, n: {n}, bins: {bins}")
    ax.set_title("[SilentStores] Distribution of check times")
    ax.set_xlabel("Time (s)")
    # ax.set_xticks(bins,
    ax.set_ylabel("Percent of all checks")
    fig.savefig('all-ss-times.png', bbox_inches='tight')
    
    # computation simplification statistics and plotting

    # what do the dependent defs look like for the cs check
    # that takes the most amount of time?
    print_longest_N = 3000
    num_containing_128bit_mul = 0
    sort_key = lambda row: int(row['lefttime']) + int(row['righttime'])
    sorted_cs = sorted(cs, key=sort_key, reverse=True)
    with open("longest_checks_no_128bit_mul.txt", "w") as of, \
         open("longest_checks_info.txt", "w") as f:
        for ii in range(print_longest_N):
            longest = sorted_cs[ii]
            depdefs = str(longest['dependentdefs'])
            if contains_128_bit_mul(depdefs):
                num_containing_128bit_mul += 1
            else:
                of.write(f"{ii}th longest cs check (check time: {int(longest['lefttime']) * 10**-9}, {int(longest['righttime']) * 10**-9} seconds) has dependent defs:\n")
                of.write(depdefs + '\n')
                of.write(str(longest['binop_dist']) + '\n')
            f.write(f"{ii}th longest cs check (check time: {int(longest['lefttime']) * 10**-9}, {int(longest['righttime']) * 10**-9} seconds) has dependent defs:\n")
            f.write(depdefs + '\n')
            f.write(str(longest['binop_dist']) + '\n')
    print(f"{num_containing_128bit_mul} of the slowest {print_longest_N} comp simp symex checks contain a 128 bit multiplication")

    # plot distribution of cs checking times
    fig, ax = plt.subplots(figsize=(11,8))
    cs_times = np.array(cs_times)
    if not is_twenty_five_bound:
        cs_bins = [400_000,
                   500_000,
                   1_000_000,
                   100_000_000,
                   500_000_000,
                   1_000_000_000,
                   5_000_000_000,
                   10_000_000_000,
                   20_000_000_000,
                   50_000_000_000,
                   100_000_000_000,
                   200_000_000_000]
        counts, edges = np.histogram(cs_times, bins=cs_bins)
    else:
        counts, edges = np.histogram(cs_times)
        cs_bins = edges
    print(f"sum of counts are: {counts.sum()}")
    print(f"histogram counts are {counts}")
    print(f"histogram edges are {edges}")
    print(f"zipped together: {list(zip(counts, edges))}")

    fake_ticks = [10 * i for i in range(len(cs_bins))]
    ticks_as_secs = [str(i * (10**(-9))) for i in cs_bins]
    ax.bar(fake_ticks[:-1],
           counts / counts.sum(),
           # edgecolor="white",
           tick_label=ticks_as_secs[:-1])
    ax.set_title("[CompSimp] Distribution of all symex checking times")
    ax.set_xlabel("Time (seconds)")
    ax.set_ylabel("Percent of all check times")
    ax.set_xticks(fake_ticks, ticks_as_secs)
    ax.set_yscale("log")
    ax.set_ylim(top=1)
    fig.savefig('all-cs-times.png', bbox_inches='tight')

    # do bootstrap sampling
    num_samples = 1000
    sample_sz = 52000
    cumsum_counts = [[] for i in range(sample_sz)]
    total_check_times = []
    for n in range(num_samples):
        sample = np.random.choice(cs_times,
                                  size=sample_sz,
                                  replace=True)
        sample = np.sort(sample)
        cumsum = np.cumsum(sample)
        total_check_times.append(sample.sum())
        for csidx in range(sample_sz):
            cumsum_counts[csidx].append(cumsum[csidx])

    counts = [np.array(cs) * 10**(-9) for cs in cumsum_counts]
    avg_cumsum_counts = [np.mean(cs) for cs in counts]
    stdev_cumsum_counts = [2 * np.std(cs) for cs in counts]
    avg_total_check_time = np.mean(np.array(total_check_times))
    stdev_total_check_time = np.std(np.array(total_check_times))
    print(f"bootstrapped avg total check time: {avg_total_check_time}, stdev: {stdev_total_check_time}")

    fig, ax = plt.subplots(figsize=(11,8))
    avgs = np.array(avg_cumsum_counts)
    stdevs = np.array(stdev_cumsum_counts)
    xs = np.arange(avgs.size)
    ax.plot(xs, avgs)
    ax.fill_between(xs, avgs - stdevs, avgs + stdevs, alpha=0.2)
    ax.set_title("[CompSimp] Cumulative time to solve from sampled times")
    ax.set_xlabel("Idx in sorted list of solve times")
    ax.set_ylabel("Cumulative time (seconds)")
    ticks = [5000 * i for i in range(55000 // 5000)]
    ax.set_xticks(ticks, ticks)
    fig.savefig('cs-times-cumsum.png', bbox_inches='tight')
    
