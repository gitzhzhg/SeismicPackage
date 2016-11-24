#! /bin/sh
#/*********************** self documentation **********************/
# TIME_NOW - prints time in ZULU format with no spaces 
#
# Usage: time_now
#
# Note: Useful for building dated filenames
#
# /**************** end self doc ********************************/

# Author: John Stockwell, 28 May 1993

PATH=/bin:/usr/bin:/usr/ucb/bin

NOW=`date | awk '{print $4}'`
echo $NOW

exit 0



