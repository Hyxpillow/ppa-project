#!/usr/bin/env python3

import sys

size = int(sys.argv[1])

nv = 2*size

edges = \
  [(2*i, 2*i+2) for i in range(0, size-1)] + \
  [(2*i+1, 2*i+3) for i in range(0, size-1)] + \
  [(2*i+2, 2*i+1) for i in range(0, size-1)] + \
  [(2*i, 2*i+1) for i in range(0, size-1, 2)] + \
  [(2*i+3, 2*i+2) for i in range(0, size-1, 2)]

for (u, v) in edges:
  print(f"{u} {v}")