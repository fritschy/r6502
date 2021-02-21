i#!/bin/sh
cargo r -- -M < a1basic/collatz2.bas && ffmpeg -y -r 60 -i mem-%05d.pgm -s 768x768 -crf 3 mem.mp4
