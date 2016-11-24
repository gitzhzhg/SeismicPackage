/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUFNZERO: $Revision: 1.5 $ ; $Date: 2011/12/23 19:36:06 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*************************** self documentation **************************/
char *sdoc[] = 	{
"									",
" SUFNZERO - get Time of First Non-ZERO sample by trace              ",
"									",
"  sufnzero <stdin >stdout [optional parameters] 			",
"									",
" Required parameters:							",
"	none								",
"									",
" Optional parameters: 							",
"	mode=first   	Output time of first non-zero sample		",
"	             	=last for last non-zero sample			",
"	             	=both for both first & last non-zero samples    ",
"									",
"	min=0   	Threshold value for considering as zero         ",
"			Any abs(sample)<min is considered to be zero	",
"									",
"	key=key1,...	Keyword(s) to print				",
"									",
NULL};

/* Credits:
 *      Geocon : Garry Perratt
 *	based on surms by the same, itself based on sugain & sumax by:
 *	CWP : John Stockwell
 */
/**************** end self doc ***********************************/

segy tr;

int
main(int argc, char **argv)
{
	cwp_String key[SU_NKEYS];	/* array of keywords		*/
	int ikey;			/* input key counter		*/
	int nkeys;			/* number of keywords to be gotten*/
	int index;			/* index of header keys		*/
	Value keyval;			/* header key value		*/
	cwp_String keytype;		/* header key type		*/
	float min=0.0;			/* Threshold value considered zero*/
	char *mode;			/* desired output		*/
	int nt;				/* number of time points on trace*/
	int itr=0;			/* trace number	-- bumped at loop top*/
	int i=0;			/* time point number		*/
	float ftime=0.0;		/* time of first non-zero sample*/
	float ltime=0.0;		/* time of last non-zero sample	*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get optional parameters */
	if (!getparstring("mode", &mode)) mode="first";
	if (!(	STREQ(mode, "first") || 
		STREQ(mode, "last") ||
		STREQ(mode, "both")
	)) err("%s unknown mode", mode); 

	if (!getparfloat("min", &min)) min=0.0;

	if ((nkeys=countparval("key"))!=0) getparstringarray("key",key);

        checkpars();
	/* Get info from first trace */
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;

	/* Loop through data */
	do {
		itr++ ;

		ftime=-1.0;
		ltime=-1.0;
		for (i = 0; i < nt; ++i) {
			/* fabs for absolute value of float */
			if (fabs(tr.data[i])>min && ftime==-1) 
					ftime=i*tr.dt/1000;

			if (fabs(tr.data[i])>min)
					ltime=i*tr.dt/1000;
		}

		/* Output ASCII */
		for (ikey = 0; ikey < nkeys; ++ikey) {
			index=getindex(key[ikey]);
			gethval(&tr,index,&keyval);
			keytype=hdtype(ikey[key]);
			printfval(keytype, keyval); 
			printf(" ");
		}
		if (STREQ(mode, "first")) printf("%g\n", ftime); 
		else if (STREQ(mode, "last")) printf("%g\n", ltime);
		else if (STREQ(mode, "both")) printf("%g %g\n", ftime, ltime); 

	} while (gettr(&tr)); 

	return(CWP_Exit());
}
