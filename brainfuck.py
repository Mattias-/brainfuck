#/usr/bin/env python

import sys
from collections import defaultdict

def interpret(program):
    instrPtr = 0
    mem = [0]
    ptr = 0
    d = defaultdict(int, {'[':1, ']':-1})
    while(instrPtr < len(program)):
        instr = program[instrPtr]
        if instr == '>':
            ptr += 1
            if ptr >= len(mem):
                mem.append(0)
        elif instr == '<':
            ptr -= 1
        elif instr == '+':
            mem[ptr] += 1
        elif instr == '-':
            mem[ptr] -= 1
        elif instr == '.':
            sys.stdout.write(chr(mem[ptr]))
        elif instr == ',':
            mem[ptr] = ord(sys.stdin.read(1))
        elif instr == '[' and mem[ptr] == 0 or instr == ']' and mem[ptr] != 0:
            nest = 1
            while(nest != 0):
                instrPtr += d[instr]
                nest += d[instr] * d[program[instrPtr]]
        instrPtr += 1

interpret(open(sys.argv[1]).read())
