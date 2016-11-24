#! /bin/sh
# Clean up residue from runs

demo=8

rm -f demo${demo}plot demo${demo}par demo${demo}[1-2].eps cshot1plot \
	demo${demo}shot demo${demo}traces

rm -f data.su
