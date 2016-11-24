/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUGET: $Revision: 1.7 $ ; $Date: 2011/11/16 23:23:25 $                */

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[]= {
"									",
" SUGET  - Connect SU program to file descriptor for input stream.	",
"									",
"    suget fd=$1 | next_su_module					",
"									",
" This program is for interfacing \" outside processing systems \"	",
" with SU. Typically, an outside system would execute the su command file",
" and a file descriptor would be passed by an outside system to		",
" the su command file so that output data from the outside system	",
" could be piped into the su programs executing inside the command file.",
"									",
" Example:    suget fd=$1 | next_su_module				",
"									",
"      fd=-1        file_descriptor_for_input_stream			",
"      verbose=0    minimal listing					",
"                   =1  asks for message with each trace processed.	",
NULL};

/*
 * Author: John Anderson (visiting scholar from Mobil) July 1994
 */
/**************** end self doc ********************************/

int fgettrn(FILE *fp, segy *tp);

segy tr;
int
main(int argc, char **argv)
{
	FILE *fp;
	int fd,j=0,verbose;

	initargs(argc,argv);
	requestdoc(1);

	if(!getparint("fd",&fd)) fd=-1;
	if(!getparint("verbose",&verbose)) verbose=0; 
        checkpars();
	warn("File descriptor passed to suget = %d",fd);
	if( (fp = (FILE *) fdopen(fd,"r"))==NULL) err("Bad file descriptor");
	warn("About to read first trace");
	if(!fgettr(fp,&tr)) err("Can't get first trace");

	do{
		if(verbose>0){
			warn("read trace %d",j);
			j++;
		}

		puttr(&tr);

	} while(fgettr(fp,&tr));

	return(CWP_Exit());
}
