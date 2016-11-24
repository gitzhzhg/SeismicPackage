#! /bin/sh
# /*********************** self documentation **********************/
# VARLIST - list variables used in a Fortran program
#
# Usage: varlist file.f ... 
# 
# Output is in the file: vars.file
#
# /**************** end self doc ********************************/

# Jack K. Cohen, 1985

BIN=${CWPROOT}/bin
PATH=/bin:/usr/bin:/usr/ucb:$BIN

cmd=`basename $0`

case $# in
0)
	echo Usage: $cmd file.f 1>&2; exit 1
;;
esac

for i
do
	name=`basename $i .f`

	case $i in
	*.f)	# correct usage
	;;
	*)
		echo "$cmd: No .f extension on $i" 1>&2; exit 1
	;;
	esac

	cat $i |
	upfort |
	sed "
		/^[C\*]/d
		s/'.*'//
		s/DOUBLE PRECISION//
		s/DOUBLE COMPLEX//
		s/BLOCK DATA//
		s/GO TO//
		s/\. *TRUE *\.//
		s/\. *FALSE *\.//
		s/\. *GT *\.//
		s/\. *GE *\.//
		s/\. *LT *\.//
		s/\. *LE *\.//
		s/\. *EQ *\.//
		s/\. *NE *\.//
		s/\. *NOT *\.//
		s/\. *AND *\.//
		s/\. *OR *\.//
		s/\. *EQV *\.//
		s/\. *NEQV *\.//
	" |
	/usr/bin/tr -sc '[A-Z][0-9]' '\012' |
	tr -sc "[A-Z][0-9]" "[\012*]" |
	sort |
	sed '
		/^[0-9]/d
		/^$/d
	' |
	uniq -c|
	sed '
		/ PROGRAM$/d
		/ END$/d
		/ INTEGER$/d
		/ REAL$/d
		/ CHARACTER$/d
		/ DOUBLEPRECISION$/d
		/ LOGICAL$/d
		/ COMPLEX$/d
		/ IMPLICIT$/d
		/ PARAMETER$/d
		/ DATA$/d
		/ PRINT$/d
		/ READ$/d
		/ IF$/d
		/ THEN$/d
		/ ELSE$/d
		/ ELSEIF$/d
		/ ENDIF$/d
		/ DO$/d
		/ CONTINUE$/d
		/ CALL$/d
		/ GOTO$/d
		/ SUBROUTINE$/d
		/ FUNCTION$/d
		/ ENTRY$/d
		/ SAVE$/d
		/ WRITE$/d
		/ FORMAT$/d
		/ OPEN$/d
		/ CLOSE$/d
		/ INQUIRE$/d
		/ BACKSPACE$/d
		/ REWIND$/d
		/ ENDFILE$/d
		/ COMMON$/d
		/ BLOCKDATA$/d
		/ ASSIGN$/d
		/ TO$/d
		/ DIMENSION$/d
		/ EQUIVALENCE$/d
		/ EXTERNAL$/d
		/ INTRINSIC$/d
		/ PAUSE$/d
		/ RETURN$/d
		/ STOP$/d
		/ UNIT$/d
		/ FMT$/d
		/ REC$/d
		/ ERR$/d
		/ IOSTAT$/d
		/ FILE$/d
		/ ACCESS$/d
		/ STATUS$/d
		/ FORM$/d
		/ RECL$/d
		/ BLANK$/d
		/ EXIST$/d
		/ OPENED$/d
		/ NUMBER$/d
		/ NAMED$/d
		/ NAME$/d
		/ SEQUENTIAL$/d
		/ DIRECT$/d
		/ NAME$/d
		/ FORMATTED$/d
		/ UNFORMATTED$/d
		/ NEXTREC$/d
		/ NAMELIST$/d
		/ INT$/d
		/ NINT$/d
		/ FLOAT$/d
		/ DBLE$/d
		/ ABS$/d
		/ MOD$/d
		/ DIM$/d
		/ MAX$/d
		/ MIN$/d
		/ ICHAR$/d
		/ LEN$/d
		/ INDEX$/d
		/ REAL$/d
		/ SQRT$/d
		/ EXP$/d
		/ LOG$/d
		/ LOG10$/d
		/ SIN$/d
		/ COS$/d
		/ TAN$/d
		/ ASIN$/d
		/ ACOS$/d
		/ ATAN$/d
		/ SINH$/d
		/ COSH$/d
		/ TANH$/d
		/ CHAR$/d
	' >vars.$name
done

exit 0
