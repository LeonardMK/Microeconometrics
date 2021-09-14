#!/usr/bin/env python
"""This script manages all tasks for the TRAVIS build server."""
import subprocess as sp

if __name__ == '__main__':
    
    for name in ['Replication_Program.ipynb', 'Robustness_checks.ipynb']:
        
        cmd = ' jupyter nbconvert --execute {}  --ExecutePreprocessor.timeout=-1'.format(name)
        sp.check_call(cmd, shell=True)

