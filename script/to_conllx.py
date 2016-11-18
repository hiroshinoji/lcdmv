#!/usr/bin/env python

import sys

for line in sys.stdin:
    if line.startswith('#'):
        continue
    else:
        line = line.strip()
        if not line:
            print
        else:
            h = line.find('-')
            if h >= 0 and h < line.find('\t'): # 1-2\t...
                continue
            else:
                print line
