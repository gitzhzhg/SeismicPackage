/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPUTGTHR: $Revision: 1.5 $ ; $Date: 2011/11/17 00:03:38 $	*/

#include "su.h"
#include "segy.h"
#include <unistd.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUPUTGTHR - split the stdout flow to gathers on the bases of given	",
" 		key parameter. 						",
" 									",
"	suputgthr <stdin   dir= [Optional parameters]			",
" 									",
" Required parameters:							",
" dir=		Name of directory where to put the gathers		",
" Optional parameters: 							",
" key=ep		header key word to watch   			",
" suffix=\".hsu\"	extension of the output files			",
" verbose=0		verbose = 1 echos information			",
" numlength=7		Length of numeric part of filename		",
" 									",
" Notes: 			    					",
" The name of the file is constructed from the key parameter. Traces	",
" are put into a temporary disk file, and renamed when key parameter	",
" changes in the input flow to \"key.suffix\". The result is that the	",
" directory \"dir\" contains separate files by \"key\" ensemble. 	",	
" 									",
" Header field modified:  ntr  to be the number of traces in a given 	",
" ensemble.								",
" Related programs: sugetgthr, susplit 					",
NULL};

/* 
 * Credits: Balazs Nemeth, Potash Corporation, Saskatoon Saskatchewan
 * given to CWP in 2008
 * Note:
 *	The "valxxx" subroutines are in su/lib/valpkge.c.  In particular,
 *	"valcmp" shares the annoying attribute of "strcmp" that
 *		if (valcmp(type, val, valnew) {
 *			...
 *		}
 *	will be performed when val and valnew are different.
 *
 */

/**************** end self doc ***********************************/


segy intrace;
segy tmptr;

int
main(int argc, char **argv)
{
	cwp_String key;	/* header key word from segy.h		*/
	cwp_String type;/* ... its type				*/
	int index;	/* ... its index			*/
	int nsegy;	/* number of bytes in the segy		*/
	Value val;	/* value of key in current gather	*/
	Value valnew;	/* value of key in trace being treated	*/
	int verbose;	/* verbose flag				*/
	int val_i;	/* key value as an integer		*/

	int ntr=0;	/* count of number of traces in an ensemble */
	int numlength;	/* length of numerical part of filenames */
	
	FILE *tmpfp=NULL;		/* file pointer			*/
	FILE *outfp=NULL;	/* file pointer			*/
	cwp_String dir;		/* directory name		*/
	cwp_String suffix;	/* suffix of output files	*/

	char directory[BUFSIZ];		/* storage for directory name	*/
 	char tempfilename[BUFSIZ];	/* 	...temporary filename	*/
	char middle[BUFSIZ];		/*      ...middle of final filename */
	char stem[BUFSIZ];		/*      ...stem of final filename */
 	char format[BUFSIZ];		/* 	...format of numeric part */
	char outfile[BUFSIZ];		/*      ...final filename	*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters */
	MUSTGETPARSTRING("dir", &dir);
	if (!getparint   ("verbose", &verbose))	 	verbose = 0;
	if (!getparstring("key", &key))		 	key = "ep";
	if (!getparstring("suffix", &suffix))		suffix = ".hsu";
	if (!getparint("numlength", &numlength)) 	numlength=7;
        checkpars();

	/* Initialize */
	/* tempfilename = ealloc1(strlen(tmplt)+3,sizeof(char)); */
	type = hdtype(key);
	index = getindex(key);

	/* Set up for first trace (must compare new key field each time) */
	nsegy = gettr(&intrace);
	
	/* Create temporary filename for the first gather */
	strcpy(directory, dir);
	strcpy(tempfilename, temporary_filename(directory));
	if((tmpfp = fopen(tempfilename,"w+")) == NULL)
		err("Cannot open file %s\n",tempfilename);

	if (verbose) warn(" temporary filename = %s", tempfilename);

	/* get key header value */
	gethval(&intrace, index, &val);
	
	ntr=0;
	do {

		++ntr;
		/* Get header value */				 
		gethval(&intrace, index, &valnew);

		/* compare past header value to current value */
		if (valcmp(type, val, valnew) || !nsegy) {
		 /* Either val and valnew differ, indicating a  */
		 /* new gather or nsegy is zero, indicating the */
		 /* end of the traces.			  */
			
			/* capture key field value */
			/* and build output filename */
			val_i=vtoi(type,val);

			/* zero out name parts */
			strcpy(outfile,"");
			strcpy(middle,"");
			strcpy(stem,"");

			/* build output name parts */
			strcat(middle,directory);
			strcat(middle,"/");
			strcat(stem,key);
			strcat(stem,"=");

			/* format for numeric part */
			(void)sprintf(format, "%%s%%s%%0%dd%%s",numlength);
		
			/* build name of output file */
			(void)sprintf(outfile, format,middle,stem,
					val_i, suffix);

			if (verbose) warn(" outfile = %s", outfile);

			/* rewind file */
			rewind(tmpfp);

			/* open the new file */
			if((outfp = fopen(outfile,"w+")) == NULL)
				err("Cannot open file %s\n",outfile);
			/* loop over traces setting ntr field */
			/*  and write to finalfile */	
			while(fgettr(tmpfp,&tmptr))  {
				tmptr.ntr = ntr;
				fputtr(outfp,&tmptr);
			}
			/* Close files */
			fclose(tmpfp);
			fclose(outfp);
			remove(tempfilename);

			if (verbose) warn("val= %i", val_i);

			/* Set up for next gather */
			/* create new tempfname first */
			strcpy(tempfilename, temporary_filename(directory));
			
			/* open filename */
			if((tmpfp = fopen(tempfilename,"w+")) == NULL)
				err("Cannot open file %s\n",tempfilename);
			val = valnew;
			ntr=0;
		}
		fputtr(tmpfp,&intrace);
	} while(gettr(&intrace));
	
	/* Close file */
	rewind(tmpfp);
	val_i=vtoi(type,val);
	
	/* Move last gather into new location */
	/* capture key field value */
	/* and build output filename */

	/* null out name parts */
	strcpy(outfile,"");
	strcpy(middle,"");
	strcpy(stem,"");

	/* build name parts */
	strcat(middle,directory);
	strcat(middle,"/");
	strcat(stem,key);
	strcat(stem,"=");
	
	/* write format of numeric part of output filename */
	(void)sprintf(format, "%%s%%s%%0%dd%%s",numlength);

	/* write output filename */
	(void)sprintf(outfile, format,middle,stem,
				val_i, suffix);

	/* open the output file */
	if((outfp = fopen(outfile,"w+")) == NULL)
			err("Cannot open file %s\n",outfile);
	/* loop over traces setting ntr field */
	while(fgettr(tmpfp,&tmptr))  {
		tmptr.ntr = ntr;
		fputtr(outfp,&tmptr);
	}
	/* Close file */
	fclose(tmpfp);
	fclose(outfp);
	remove(tempfilename);

	if (verbose) warn(" outfile = %s", outfile);
	if (verbose) warn("val= %i",val_i);

	return(CWP_Exit());

}
