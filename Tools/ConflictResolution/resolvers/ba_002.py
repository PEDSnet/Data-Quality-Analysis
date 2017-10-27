# function to resolve conflicts from log file
# Inputs: (i) log_issue - an object read the log file (ii) secondary_issue - a similar issue read from the secondary
# report,
#  (iii) threshold_l, and threshold_u are the thresholds corresponding to the check type CA-006
# returns a mutation of secondary issue with updated findings. 

import re


perct_re = re.compile(r'(\d+(?:\.\d+)?)')


def extract_miss(s):
    m = perct_re.match(s)
    if not m:
        return

    p = m.group()

    try:
        return int(p)
    except:
        pass

    return float(p)


def resolve(log_issue, secondary_issue, threshold_l, threshold_u):
    result_issue_list = []  # list of objects to be returned. Issue class
    # mutate the old issue in the secondary report with updated findings
    mutated_issue = secondary_issue.copy()
    mutated_issue.finding = log_issue.finding
    


    #extract % of no match data from previous cycle
    nm_prev = extract_miss(secondary_issue.finding)

    #extract % of no match data in current cycle
    nm_curr = extract_miss(log_issue.finding)

    if nm_curr - nm_prev > 10:
        mutated_issue.status='under review'
	
    result_issue_list.append(mutated_issue)
  
    return result_issue_list
