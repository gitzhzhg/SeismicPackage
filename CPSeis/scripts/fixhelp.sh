#!/bin/bash
# Replace help file information with current CPSEIS_INSTALL_DIR
[ -z $CPSEIS_INSTALL_DIR ] && echo "CPSEIS_INSTALL_DIR not defined. " && exit 1
cd $CPSEIS_INSTALL_DIR/spws_home/app-defaults
PROGS="Cbyt Va Cfg Csv Msep"
for prog in $PROGS ; do
	echo "Fixing $prog to point to installed help file"
	sleep 1
	cat $prog.base | sed "s:CPSEIS_INSTALL_DIR:${CPSEIS_INSTALL_DIR}:g" >$prog
done
