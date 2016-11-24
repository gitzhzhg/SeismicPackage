/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPUT: $Revision: 1.7 $ ; $Date: 2011/11/16 23:23:25 $                */

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[]={
"									",
" SUPUT - Connect SU program to file descriptor for output stream.	",
"									",
"       su_module | suput fp=$1						",
"									",
" This program is for interfacing \" outside processing systems \"	",
" with SU. Typically, the outside system would execute the SU command file.",
" The outside system provides the file descriptor it would like to read	",
" from to the command file to be an argument for suput.			",
"									",
" Example: su_module | suput fp=$1					",
"									",
"       fd=-1       file_descriptor_for_output_stream_from_su		",
"       verbose=0   minimal listing					",
"                   =1  asks for message with each trace processed.	",
NULL};

/*
 * Author: John Anderson (visiting scholar from Mobil) July 1994
 */

/**************** end self doc ********************************/

void fputtrn(FILE *fp, segy *tp);

segy tr;
int main(int argc, char **argv)
{
	FILE *fp;
	int fd,nread=0,verbose;

	initargs(argc,argv);
	requestdoc(1);

	if(!getparint("fd",&fd)) fd=-1;
	fprintf(stderr,"File descriptor passed to suput = %d\n",fd);

	if(!getparint("verbose",&verbose)) verbose=0; 
        checkpars();

	if( (fp = (FILE *) fdopen(fd,"w"))==NULL) err("Bad file descriptor \n");

	if(!gettr(&tr)) err("Can't get first trace \n");
	do{
		if(verbose>0){
			warn("suput: read input traces %d", nread);
			nread++;
		}

		fputtr(fp,&tr);
	} while(gettr(&tr));

	(void) fclose(fp);
	return(CWP_Exit());
}
