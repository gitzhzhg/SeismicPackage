#! /bin/sh
#/*********************** self documentation **********************/
# ARGV - give examples of dereferencing char **argv
#
# Usage: argv
#
#/**************** end self doc ********************************/

# Jack K. Cohen, 1985

PATH=/bin:/usr/bin:/usr/ucb

exec cat << 'END'
char *argv[] = char **argv
argv		pointer to pointer to argument strings
argv[0]		pointer to first argument string {initially, the command name}
argv[1]		pointer to second string {initially, the first "real" argument}
argv[0] [0]	first character in first string
argv[1] [0]	first character in second string
argv[0] [1]	second character in first string

*argv		= argv[0]
*argv[0] 	= *(argv[0]) = argv[0] [0]
(*argv)[0] 	= (argv[0])[0] = argv[0] [0] = *argv[0]
**argv 		= *(*argv) = *(argv[0]) = argv[0] [0]

++argv		advance pointer {which now points at the pointer 
                to the "current" argv[0]}

*++argv		= (++argv)[0] {pointer to the "current" first string}
**++argv	= (++argv)[0] [0] {first character in "current" first string}
++*argv		= ++argv[0] = argv[0] + 1 {pointer to argv[0] [1]}
*++*argv	= *(++*argv) = argv[0] [1]

*++argv[0]	= *++(argv[0]) = *++*argv
(*++argv)[0]	= **++argv
END
