#! /bin/sh
# clean the directory

rm -f *.b
rm -f demo.su*
rm -f model.ps
rm -f demo*

cd MakeData ; Clean.sh

cd ..

cd VelocityFiles ; Clean.sh


exit 0
