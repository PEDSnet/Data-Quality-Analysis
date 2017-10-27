# function to resolve conflicts from log file
# Inputs: (i) log_issue - an object read the log file (ii) secondary_issue - a similar issue read from the secondary
# report,
#  (iii) threshold_l, and threshold_u are the thresholds corresponding to the check type CA-006
# returns a mutation of secondary issue with updated findings. 


def resolve(log_issue, secondary_issue, threshold_l, threshold_u):
    result_issue_list = []  # list of objects to be returned. Issue class
    # mutate the old issue in the secondary report with updated findings
    mutated_issue = secondary_issue.copy()
    mutated_issue.finding = log_issue.finding
    result_issue_list.append(mutated_issue)
  
    return result_issue_list
