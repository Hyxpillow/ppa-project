#!/usr/bin/env python3

import sys

size = int(sys.argv[1])

for i in range(0,size-1):
  print(f"{i} {size-1}")
  print(f"{i} {(i+1) % (size-1)}")