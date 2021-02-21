#!/bin/sh
set -e
printf "POKE 1111,2\nCALL 1111\n" | cargo r -- -m | grep -e '^R' -e '^W' | cmp - exit-basic.out
