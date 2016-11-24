#! /bin/sh
# Clean up residue from runs

demo=5

rm -f record.cwp
(cd model.dir; Clean)
(cd miggbzo.dir; Clean)
