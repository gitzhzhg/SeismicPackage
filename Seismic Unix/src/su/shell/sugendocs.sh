#! /bin/sh
# /*********************** self documentation **********************/
# SUGENDOCS - generate complete list of selfdocs in latex form
#
# Usage: sugendocs -o  output filename is: selfdocs.tex
# Note: this shell simply calls    gendocs
#
#/**************** end self doc ********************************/

# Author: John Stockwell -- 5 Jan 1992

# test for CWPROOT
if test "${CWPROOT}" = ""
then
	echo "The environment variable \"CWPROOT\" "
	echo "is not set in the user's working shell environment."
	echo "To set this variable in C-shell, use the command: "
	echo "  setenv  CWPROOT  /your/cwp/root/path"
	echo "To set this variable in Bourne or Korn-shell, use the command:"
	echo "  export  CWPROOT=/your/cwp/root/path" ; exit 1

fi

ROOT=${CWPROOT}
BIN=$ROOT/bin
PATH=${PATH}:${BIN}

gendocs

exit 0

