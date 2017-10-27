# function to resolve conflicts from log file
# Inputs: (i) log_issue - an object read the log file (ii) secondary_issue - a similar issue read from the secondary
# report,
#  (iii) threshold_l, and threshold_u are the thresholds corresponding to the check type CA-006
# returns a set of objects that would replace the secondary_issue in the secondary report

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
    diff_check_code = 'CA-006'
    diff_check_type = 'unexpected change in missingness of a field between data cycles'

    # extract % of missing data from previous cycle
    m_prev = extract_miss(secondary_issue.finding)

    # extract % of missing data in current cycle
    m_curr = extract_miss(log_issue.finding)

    if m_prev is None:
        raise ValueError('no missing percentage in secondary issue')

    if m_curr is None:
        raise ValueError('no missing percentage in log issue')

    m_diff = m_curr - m_prev

    # if the finding is the same
    if m_diff == 0:
        return

    # prepare a new issue showing difference between missingness
    new_issue = log_issue.copy()
    new_issue.check_code = diff_check_code
    new_issue.check_type = diff_check_type
    new_issue.finding = str(m_diff) + '%'
    new_issue.status = 'new'

    # mutate the old issue in the secondary report with latest findings
    mutated_issue = secondary_issue.copy()
    mutated_issue.finding = str(m_curr) + '%'

    if secondary_issue.status == 'under review':
        if m_prev == 100 or m_curr == 100 or m_diff > threshold_u or m_diff < threshold_l:  # outside acceptable range
            result_issue_list.append(new_issue)
            result_issue_list.append(mutated_issue)
        if threshold_u > m_diff > threshold_l:  # if difference within acceptable limits
            result_issue_list.append(mutated_issue)
    elif secondary_issue.status == 'persistent':
        if m_prev == 100 or m_curr == 100 or m_diff > threshold_u or m_diff < threshold_l:  # outside acceptable range
            result_issue_list.append(new_issue)
            result_issue_list.append(log_issue)
        if threshold_u > m_diff > threshold_l:  # if different within acceptable limits
            result_issue_list.append(mutated_issue)

    return result_issue_list
