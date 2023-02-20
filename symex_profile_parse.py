import csv
import sys

# silentstores uses leftconstraints
symex_profiling_csv_fieldnames = ['is_cs', 'is_ss','sstime','lefttime','righttime','leftconstraints','rightconstraints','dependentdefs'] 

if __name__ == '__main__':
    filename = sys.argv[1]

    with open(filename, mode='r') as csvfile:
        reader = csv.DictReader(csvfile, fieldnames=symex_profiling_csv_fieldnames)

        ss_times = []
        cs_times = []

        for row in reader:
            if row['is_ss']:
                sstime = row['sstime']
                ss_times.append(sstime)

        print(f"ss times are: {ss_times}")
