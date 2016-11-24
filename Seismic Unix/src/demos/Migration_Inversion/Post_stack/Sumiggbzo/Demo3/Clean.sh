#! /bin/sh
# Clean up residue from runs

demo=3

rm -f record.cwp
(cd model.dir; Clean.sh)
(cd miggbzo.dir; Clean.sh)
