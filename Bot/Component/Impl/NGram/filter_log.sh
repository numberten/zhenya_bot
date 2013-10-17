#!/bin/sh
# Filters a raw irssi log and outputs the name\tline for each utterance.
sed -rn 's/^[^<]+< ?@?([a-zA-Z_]+?)>\s+/\1\t/gp'
