#! /bin/csh -f
##--------------------------- grow_path.csh --------------------------------##
##--------------------------- grow_path.csh --------------------------------##
##--------------------------- grow_path.csh --------------------------------##

# This script provides a convenient way to conditionally add a path to any
# path-type environment variable.  
#
# To call this script, simply run the following:
#
#                   source  grow_path.csh  NAME  ADDITION
#                   source  grow_path.csh  NAME  ADDITION  atend
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

if ($1 == "") exit
if ($2 == "") exit
 
set name     = $1
set addition = $2
set atend    = $3
set value    = `printenv $name`

if ($value == "") then
  setenv $name $addition
  exit
endif
 
if ($atend == atend) then
  echo :${value}: | grep :${addition}: > /dev/null || setenv $name ${value}:$addition
else
  echo :${value}: | grep :${addition}: > /dev/null || setenv $name ${addition}:$value
endif

# NOTE: The colons are included above so that a small path being added will
# not match part of an already-existing longer path, and also so a match
# will be detected even if the already-existing path is the first or last
# path in the environment variable.

##-------------------------------- end -------------------------------------##
##-------------------------------- end -------------------------------------##
##-------------------------------- end -------------------------------------##

