#!/bin/sh
# Filters a raw irssi log and outputs the name\tline for each utterance.
grep -v 'zhenya_bot' | \
sed -rn 's/^[^<]+< ?@?([a-zA-Z]+?)(_[a-zA-Z]*)?>\s+/\1\t/gp'
