#!/usr/bin/env python3

import sys
import re

with open("instr_table.txt", "r") as f:
    data = map(str.strip, f.readlines())

t = [('None', 'IMPL') for _ in range(256)]
for l in data:
    oc, name = l.split(' ')
    oc = int(oc, 16)
    name = name.split('_')
    t[oc] = (name[0], name[1] if len(name) > 1 else "IMPL")

print('#[derive(Debug, Copy, Clone, PartialEq, Eq)]')
print('pub enum Instr {')
instrs = list(set(x[0] for x in t if x[0] != 'None'))
instrs.sort()
for i in instrs:
    print('    {},'.format(i))
print('\n    None,')
print('}\n')

print('impl Instr {')
print('    pub fn name(self) -> &\'static str {')
print('        match self {')
for i in instrs:
    print('            Instr::{} => "{}",'.format(i, i))
print('            Instr::None => unreachable!(),')
print('        }\n    }\n}\n')

print('pub fn m6502_instr_table<M: crate::Memory>() -> &\'static [(Instr, crate::addressing_mode::AM, fn(&mut crate::R6502<M>, crate::addressing_mode::AM))] {')
print('    &[')
for name, am in t:
    print('        (Instr::{:5} crate::addressing_mode::AM::{:5} crate::instr::{}),'.format(name + ',', am + ',', name.lower() if name != 'None' else 'nop'))
print('    ]\n}\n')
