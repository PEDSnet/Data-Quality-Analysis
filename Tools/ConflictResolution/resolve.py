#!/usr/bin/env python

"""
Usage:
    python resolve.py [--resolvers=<path>] [--tests=<path>] [file]

Options:
    --resolvers=<path>      Directory of resolver modules.
    --tests=<path>          Directory of test cases.

Examples:
    Specify a directory of resolver scripts and read a set of
    issues from a file.

        python resolve.py --resolvers=./resolvers issues.json

    Specify a directory of test cases.

        python resolve.py --resolvers=./resolvers --tests=./tests

Documentation:
    This program takes a set of conflicting issues and passes them into their
    corresponding resolver function for the issue code. The output of the
    resolution is zero or more issues that will replace the existing secondary
    report issue.

    A "resolver" is a Python module named after a valid check code such as
    `ba_001.py` which corresponds to "BA-001". Module names must be lowercase
    and an underscore must be used instead of a hyphen. A resolver module must
    define a function called `resolve` with the following signature:

        def resolve(log_issue, secondary_issue, lower_threshold, upper_threshold):
            pass

    The return value of the function must either be:
        - `None` to remove the issue altogether
        - An instance of `Issue`
        - A list of `Issue` instances

    When this program runs, it will assume the resolver modules are in the
    current working directory. The `--resolvers` option can be used to
    explicitly set the directory (recommended).
    The input is a JSON-encoded array of issues that need to be resolved. The
    reason for this encoding is because it has been designed to be used by the
    pedsnet-dqa tool directly and thus a simple structured format must be used.
    That being said, it is very easy to make test cases for testing the
    resolver modules. The format is shown below:

        [
          {
            "log": {
              "check_code": "BA-001",
              "finding": "10%",
              ...
            },
            "secondary": {
              "check_code": "BA-001",
              "finding": "20%",
              ...
            },
            "threshold_low": 0,
            "threshold_high": 0
          },
          ...
        ]

    The "log" entry is the issue from the log file, the "secondary" entry is
    the issue from the secondary report, and the two thresholds come from the
    DQA Catalog Check Master List.
    The output of the program is a list with an element corresponding to the
    input list. Each element is `null` or a list of issues that will replace
    the resolved issue in the secondary report.

        [
          [
            {
              "status": null,
              "field": null,
              "check_type": null,
              "reviewer": null,
              "goal": null,
              "github_id": null,
              "prevalence": null,
              "cause": null,
              "rank": null,
              "data_version": null,
              "model_version": null,
              "site_response": null,
              "check_code": "BA-001",
              "dqa_version": null,
              "table": null,
              "model": null,
              "finding": "20%",
              "method": null
            }
          ],
          ...
        ]

"""  # noqa

import os
import sys
import json
import traceback
from copy import copy
from importlib import import_module


issue_attrs = {
    'model',
    'model_version',
    'data_version',
    'dqa_version',
    'table',
    'field',
    'goal',
    'check_code',
    'check_type',
    'check_alias',
    'finding',
    'prevalence',
    'rank',
    'site_response',
    'cause',
    'status',
    'reviewer',
    'github_id',
    'method',
}


def json_defaults(o):
    if isinstance(o, Issue):
        return o.dict()
    raise ValueError


class Issue(object):
    __slots__ = issue_attrs

    def __init__(self, **kwargs):
        for k in issue_attrs:
            v = kwargs.pop(k, None)
            setattr(self, k, v)

        # Remaining keyword indicates unknown attributes.
        if kwargs:
            raise AttributeError('unknown attributes: {}', ', '.join(kwargs))

    def copy(self):
        return copy(self)

    def dict(self):
        d = {}

        for k in self.__slots__:
            v = getattr(self, k)
            if v is not None:
                d[k] = v

        return d


def import_resolver(check_code):
    mod_name = check_code.replace('-', '_').lower()
    return import_module(mod_name)


def resolve_issue(issue):
    log_issue = Issue(**issue['log'])
    secondary_issue = Issue(**issue['secondary'])
    threshold_low = issue['threshold_low']
    threshold_high = issue['threshold_high']

    if not log_issue.check_code:
        return

    module = import_resolver(log_issue.check_code)

    # Inject names into the module.
    setattr(module, 'Issue', Issue)

    res = module.resolve(log_issue,
                         secondary_issue,
                         threshold_low,
                         threshold_high)

    if res is None:
        return ()

    if isinstance(res, (list, tuple)):
        return res
    elif isinstance(res, Issue):
        return (res,)

    raise TypeError('resolver must return an Issue or list of '
                    'Issues, but returned {} instead'
                    .format(type(res)))


def main_test(test, name):
    issue = test['issue']
    expected = [Issue(**o).dict() for o in test['expected']]

    actual = resolve_issue(issue)

    sys.stderr.write('Test: {}\n'.format(name))
    if actual == expected:
        sys.stderr.write('* Result: PASS\n')
    else:
        sys.stderr.write('* Result: FAIL\n')
        sys.stderr.write('* Expected:\n')
        sys.stderr.write(json.dumps(expected, sort_keys=True, indent=2))
        sys.stderr.write('\n')
        sys.stderr.write('* Actual:\n')
        sys.stderr.write(json.dumps(actual, sort_keys=True, indent=2))
    sys.stderr.write('\n')


def main(issues):
    out = []

    for issue in issues:
        try:
            res = resolve_issue(issue)
        except Exception:
            out.append({
                'error': traceback.format_exc(),
            })
            continue

        out.append({
            'issues': res,
        })

    return out


if __name__ == '__main__':
    args = list(sys.argv[1:])

    resolvers = '.'
    for arg in args:
        if arg.startswith('--resolvers='):
            args.remove(arg)
            resolvers = arg.split('=')[1]
            break

    resolvers = os.path.join(os.getcwd(), resolvers)
    sys.path.insert(0, resolvers)

    tests = None
    for arg in args:
        if arg.startswith('--tests='):
            args.remove(arg)
            tests = arg.split('=')[1]
            break

    if tests:
        for fn in os.listdir(tests):
            if os.path.isdir(fn):
                continue

            if not fn.endswith('.json'):
                continue

            with open(os.path.join(tests, fn)) as f:
                test = json.load(f)
                main_test(test, fn)

    else:
        if len(args):
            with open(args[0]) as f:
                issues = json.load(f)
        else:
            issues = json.load(sys.stdin)

        out = main(issues)
        json.dump(out, sys.stdout, indent=2, default=json_defaults)
