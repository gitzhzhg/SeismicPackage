/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUGETGTHR: $Revision: 1.5 $ ; $Date: 2011/11/17 00:03:38 $		*/

#include <sys/stat.h> 
#include <sys/types.h>
#include <fcntl.h>
#include <dirent.h>

#include "su.h"
#include "segy.h"
#include "header.h"


#define CWP_O_LARGEFILE	0100000

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUGETGTHR - Gets su files from a directory and put them               ",
"             throught the unix pipe. This creates continous data flow.	",
" 									",
"  sugetgthr  <stdin >sdout   						",
" 									",
" Required parameters:							",
" 									",
" dir=            Name of directory to fetch data from 			",
" 	          Every file in the directory is treated as an su file	",
" Optional parameters:							",
" verbose=0		=1 more chatty					",
" vt=0			=1 allows gathers with variable length traces	",
" 			no header checking is done!			",
" ns=			must be specified if vt=1; number of samples to read",
" 									",
NULL};



segy tr;

int
main(int argc, char **argv)
{
	
	cwp_String dir="";	/* input directory containng the gathers */
	char *fname=NULL;	
	char *ffname=NULL;
	
	DIR *dp=NULL;
	struct dirent *d=NULL;
	struct stat __st;
	FILE *fp=NULL;
	int fd=0;
	
	int verbose;
	int vt=0;
	ssize_t nread;
	int ns=0;

	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

        /* Get parameters */
        MUSTGETPARSTRING("dir", &dir);
       	if (!getparint   ("verbose", &verbose)) verbose = 0;
       	if (!getparint   ("vt", &vt)) vt = 0;
	if(vt)MUSTGETPARINT("ns",&ns); 
        checkpars();
	
	/* Open the directory */
	if ((dp = opendir(dir)) == NULL)
		err(" %s directory not found\n",dir);
	
	/* For each file in directory */
	while (( d = readdir(dp)) !=NULL) {
		
		fname = ealloc1(strlen(d->d_name)+1,sizeof(char));
		strcpy(fname,d->d_name);
		
		/* Skip . and .. directory entries */
		if(strcmp(fname,".") && strcmp(fname,"..")) {		
			ffname = ealloc1(strlen(d->d_name)+strlen(dir)+2,sizeof(char));
			
			/* Create full filename */
			sprintf(ffname, "%s/%s",dir,fname);
			if(verbose==1) warn("%s",ffname);
			
			/* get some info from the file */
			stat(ffname,&__st);
			if(__st.st_size > 0) {
			
				/* Open the file and read traces into stdout*/
 				if(vt) {
					fd = open(ffname,O_RDONLY|CWP_O_LARGEFILE);
				/*	nread=fread(&tr,(size_t) HDRBYTES,1,fp);  */
					nread=read(fd,&tr,(size_t) HDRBYTES); 
					memset((void *) &tr.data[tr.ns], (int) '\0' ,MAX(ns-tr.ns,0)*FSIZE);
				/*	nread+=fread(&tr.data[0],(size_t) tr.ns*FSIZE,1,fp); */
					nread+=read(fd,&tr.data[0],(size_t) tr.ns*FSIZE);
				} else {
					fp = efopen(ffname, "r");
					nread=fgettr(fp, &tr);
				}
				do {
					if(vt) { 
						tr.ns=ns;
						fwrite(&tr,ns*FSIZE+HDRBYTES,1,stdout);
					} else {
						puttr(&tr);
					}
					if(vt) {
						/* nread=fread(&tr,(size_t) HDRBYTES,1,fp); */
						nread=read(fd,&tr,(size_t) HDRBYTES);
						memset((void *) &tr.data[tr.ns], (int) '\0' ,MAX(ns-tr.ns,0)*FSIZE);
						/* nread+=fread(&tr.data[0],(size_t) tr.ns*FSIZE,1,fp); */
						nread+=read(fd,&tr.data[0],(size_t) tr.ns*FSIZE);
					} else {
						nread=fgettr(fp, &tr);
					}
				} while(nread);
				if(vt) close(fd);
				else efclose(fp);
			} else {
				warn(" File %s has zero size, skipped.\n",ffname);
			}
			free1(ffname);
		}
		free1(fname);
		
	}
	closedir(dp);
        return(CWP_Exit());
}
