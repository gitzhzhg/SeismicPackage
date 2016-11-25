#! /bin/bash -f
##--------------------------- grow_path.sh --------------------------------##
##--------------------------- grow_path.sh --------------------------------##
##--------------------------- grow_path.sh --------------------------------##

# This script provides a convenient way to conditionally add a path to any
# path-type environment variable.  
#
# To call this script, simply run the following:
#
#                   source  grow_path.sh  NAME  ADDITION
#                   source  grow_path.sh  NAME  ADDITION  atend
#
# where   NAME   is the name of the path-type environment variable to update.
# where ADDITION is the path to add to the path-type environment variable.
#
# The path is added to the beginning of the path-type environment variable,
# unless the argument "atend" is added to the command, in which case the path
# is added to the end of the path-type environment variable.
#
# Nothing is done if the path is already present in the environment variable.
# If the path-type environment variable is missing, it will be created.
#
# Examples of path-type environment variables are these:
#
#      PATH   MANPATH   LD_LIBRARY_PATH   XFILESEARCHPATH   CLASSPATH

##------------------------------ script ------------------------------------##
##------------------------------ script ------------------------------------##
##------------------------------ script ------------------------------------##

if [ "$1" == "" ]; then return; fi
if [ "$2" == "" ]; then return; fi
 
name=$1
addition=$2
atend=$3
value=`printenv $name`

if [ "$value" == "" ]; then
  export $name=$addition
  return
fi
 
if [ "$atend" == atend ]; then
  echo :${value}: | grep :${addition}: > /dev/null || export $name=${value}:$addition
else
  echo :${value}: | grep :${addition}: > /dev/null || export $name=${addition}:$value
fi

# NOTE: The colons are included above so that a small path being added will
# not match part of an already-existing longer path, and also so a match
# will be detected even if the already-existing path is the first or last
# path in the environment variable.

##-------------------------------- end -------------------------------------##
##-------------------------------- end -------------------------------------##
##-------------------------------- end -------------------------------------##

