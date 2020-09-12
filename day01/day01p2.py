#! /usr/bin/env python3
import sys


lines = [l for l in sys.stdin]

freq = 0
seen = set()
while True:
    for l in lines:
        delta = int(l)
        freq += delta
        if freq in seen:
            print(freq)
            sys.exit(0)
        seen.add(freq)

