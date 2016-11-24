#! /bin/sh
# mailhome.sh - send a message back to CWP 
# John Stockwell, Center for Wave Phenomena, 1 August 1997
#set -x

# these items identify the date/release of the codes
DATE="14 Aug 2015"
RELEASE="44"

# home address
ADDRESS="john@dix.mines.edu"

# message
MESSAGE="Installing the $DATE version (Release ${RELEASE}) of the CWP codes."
YOURADDRESS=


echo
echo
echo "######################"
echo "######################"
echo
echo
echo "To give us at CWP an idea of who uses our codes"
echo "the master Makefile will email the following message:"
echo
echo " \"${MESSAGE}\" "
echo
echo "to: \"${ADDRESS}\" "
echo
echo
echo
echo "You will then be put on our CWP/SU mailing list"
echo "and will be informed of future updates of the CWP codes."
echo
echo
echo "However, if you would rather not have this message sent"
echo "you may specify this as your response to the next query."
echo
echo
echo "Send automatic mail message back to CWP?[y/n]"  | tr -d "\012"
read RESP 

case $RESP in
	y*|Y*) # continue

		ok=false
		while [ $ok = false ]
		do
			echo "please type your e-mail address: "  | tr -d "\012"
			read YOURADDRESS
			#check to see if address is correct
			echo "is $YOURADDRESS correct?[y/n]"  | tr -d "\012"
			read ISADDRESSOK
			case $ISADDRESSOK in
			n*) ok=false
			;;
			*)  ok=true
				echo "$MESSAGE $YOURADDRESS" | mail $ADDRESS

				echo 
				echo
				echo "Beginning installation process"
				echo
				echo
				sleep 5
			;;
			esac
		done
	;;
	*) # don't send the message
		echo "Continuing without sending the mailing."
	;;
		esac

exit 0
