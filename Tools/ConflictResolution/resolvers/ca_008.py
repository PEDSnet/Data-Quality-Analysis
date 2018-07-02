# function to resolve conflicts from log file for the check type CA008
# Inputs: (i) log_issue - an object read the log file (ii) secondary_issue - a similar issue read from the secondary
# report,
#  (iii) threshold_l, and threshold_u - not applicable 
# returns a set of objects that would replace the secondary_issue in the secondary report


def resolve(log_issue, secondary_issue, threshold_l, threshold_u):
    result_issue_list = []  # list of objects to be returned. Issue class
  
   
    # if the finding is the same, no further action needed 
    if log_issue.finding == secondary_issue.finding:
        return
                

    # mutate the old issue in the secondary report with latest findings
    mutated_issue = secondary_issue.copy()
    mutated_issue.finding = log_issue.finding
    mutated_issue.status = 'under review'

 
    result_issue_list.append(mutated_issue)

    return result_issue_list
