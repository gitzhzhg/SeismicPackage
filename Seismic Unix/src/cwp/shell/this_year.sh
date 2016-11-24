#! /bin/sh
# /*********************** self documentation **********************/
# THIS_YEAR - print the current year
#
# Usage: this_year
#
# NOTES - useful for building dated filenames, etc.
#
# /**************** end self doc ********************************/

# Author: John Stockwell, Jun 1993

date | awk '{ print $6}'

exit 0
