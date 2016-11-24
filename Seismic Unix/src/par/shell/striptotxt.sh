#! /bin/sh
# /*********************** self documentation **********************/
# STRIPTOTXT -  put files from $CWPROOT/src/doc/Stripped into a new
#               directory in the form $CWPROOT/src/TXT/NAME.txt
#
# Usage: striptotxt
#
# Author: John Stockwell,  Sept 2001
# /**************** end self doc ********************************/
 
# directory definitions
DOC=$CWPROOT/src/doc
STRIPPED=$DOC/Stripped
TXT=$DOC/TXT
 
if [ ! -d $TXT ]
then
        mkdir $TXT
fi
 
# clean out old .txt files
rm -f  $TXT/*.txt
 
# convert Stripped files to .txt files
cd $STRIPPED
 
for i in *
do
        NAME=` echo $i | sed 's/\./ /g' | awk '{ print $1 }'`
        cp $i $TXT/$NAME.txt
done

exit 0
