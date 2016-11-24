#! /bin/sh
# mailhome.sh - send a message back to CWP 
# John Stockwell, Center for Wave Phenomena, 1 August 1997
#set -x

# these items identify the date/release of the codes
DATE="12 Aug 2015"
RELEASE="44"

echo
echo
echo "################################################################"
echo "####### Legal Statement for ${DATE} Release ${RELEASE} of CWP/SU #######"
echo "################################################################"
echo
echo "hit return key to continue"  | tr -d "\012"
read RESP 
echo
	more ./LEGAL_STATEMENT
echo
echo "By answering you agree to abide by the terms and conditions of"
echo "the above LEGAL STATEMENT ?[y/n]"  | tr -d "\012"
read RESP

case $RESP in
	y*|Y*) # continue
	echo "CWP/SU Release $RELEASE $DATE" > cwp_su_version
	;;
	*) # Stop installation
		exit 1
	;;
		esac

exit 0
