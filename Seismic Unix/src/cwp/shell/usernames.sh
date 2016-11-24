#! /bin/sh
# /*********************** self documentation **********************/
# USERNAMES - get list of all login names
#
# Usage: usernames
#
# /**************** end self doc ********************************/

PATH=/bin:/usr/bin:/usr/ucb

awk -F: '{print $1 ":	" $5}' /etc/passwd |
sed '
	/The Super-user/d
	/switch user/d
	/The adduser authority/d
	/Owner of background daemon processes/d
	/Owner of all binaries/d
	/System Administrator/d
	/ADM/d
	/UUCP/d
	/Nobody at all/d
	/For INmail LPP/d
	/sync/d
	s/\/.*//
	s/;.*//
	s/,.*//
' |
sort
