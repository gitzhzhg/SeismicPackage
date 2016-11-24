#! /bin/sh
#/*********************** self documentation **********************/
# WEEKDAY - prints today's WEEKDAY designation
#
# Usage: weekday
#
# Note: Useful for building dated filenames
#
# /**************** end self doc ********************************/

# Author: John Stockwell, 28 May 1993

PATH=/bin:/usr/bin:/usr/ucb/bin

WEEKDAY=`date | awk '{print $1}'`
echo $WEEKDAY

exit 0



