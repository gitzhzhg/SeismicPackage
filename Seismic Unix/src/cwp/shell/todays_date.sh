#! /bin/sh
#/*********************** self documentation **********************/
# TODAYS_DATE - prints today's date in ZULU format with no spaces 
#
# Usage: todays_date
#
# Note: Useful for building dated filenames
#
# /**************** end self doc ********************************/

# Author: John Stockwell, 28 May 1993

PATH=/bin:/usr/bin:/usr/ucb/bin

TODAY=`date | awk '{print $3$2$6}'`
echo $TODAY

exit 0



