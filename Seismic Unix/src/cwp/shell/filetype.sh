#! /bin/sh
# /*********************** self documentation **********************/
# FILETYPE - list all files of given type
#
# Usage: filetype string_from_file_output
#
# Examples:
#	filetype text      - list printable files
#       filetype stripped  - list unstripped files
# /**************** end self doc ********************************/

# Credit: Fiedler and Hunter

PATH=/bin:/usr/bin:/usr/ucb/bin

case $# in
	1)  # OK
		;; 
	*) 
	echo "Usage: filetype string_from_file_output" 1>&2; exit 1
		 ;;
esac

file * | grep $1 | sed 's/:.*$//'
