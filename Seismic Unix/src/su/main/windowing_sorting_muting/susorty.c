/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSORTY: $Revision: 1.5 $ ; $Date: 2011/11/17 00:03:38 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SUSORTY - make a small 2-D common shot off-end  		",
"	    data set in which the data show geometry 		",
"	    values to help visualize data sorting.		",
"								",
"  susorty [optional parameters] > out_data_file  		",
"								",
" Optional parameters:						",
"	nt=100 		number of time samples			",
"	nshot=10 	number of shots				",
"	dshot=10 	shot interval (m)			",
"	noff=20 	number of offsets			",
"	doff=20 	offset increment (m)			",
"								",
" Notes:							",
" Creates a common shot su data file for sort visualization	",
"	       time samples           quantity			",
"	       ----------------      ----------			",
"	       first   25%           shot coord			",
"	       second  25%           rec coord			",
"	       third   25%           offset			",
"	       fourth  25%           cmp coord			",
"								",
"								",
" 1. default is shot ordered (hsv2 cmap looks best to me)	",
" susorty | suximage legend=1 units=meters cmap=hsv2		",
"								",
" 2. sort on cmp (note random order within a cmp)		",
" susorty | susort cdp > junk.su 				",
" suximage < junk.su legend=1 units=meters cmap=hsv2		",
"								",
" 3. sort to cmp and subsort on offset 	 			",
" susorty | susort cdp offset > junk.su 			",
" suximage < junk.su legend=1 units=meters cmap=hsv2		",
"								",
NULL};

/* Credits:
 *	CWP: Chris Liner  10.09.01
 *
 * Trace header fields set: ns, dt, sx, gx, offset, cdp, tracl 
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt,nshot,noff;
        int ishot,ioff,it;
        float dt,dshot,doff,sx,gx,offset,cmp;

	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */

	nt = 100;	getparint("nt", &nt);
	CHECK_NT("nt",nt);				tr.ns = nt;
	nshot = 10;	getparint("nshot", &nshot);
	noff  = 24;	getparint("noff", &noff);
	dt = 0.004;	getparfloat("dt", &dt);		tr.dt = dt*1000000;
	dshot = 10;	getparfloat("dshot", &dshot);
	doff = 20;	getparfloat("doff", &doff);
        checkpars();
	for (ishot = 0; ishot < nshot; ishot++) {
          sx = ishot*dshot;
	  for (ioff = 0; ioff < noff; ioff++) {
                offset = (ioff+1)*doff;
                gx = sx + offset; 
                cmp = (sx + gx)/2.;
		memset( (void *) tr.data, 0, nt * FSIZE);
                for (it = 0; it < nt/4; it++) {
		  tr.data[it] = sx;  
                }
                for (it = nt/4; it < nt/2; it++) {
		  tr.data[it] = gx;  
                }
                for (it = nt/2; it < 3*nt/4; it++) {
		  tr.data[it] = offset;  
                }
                for (it = 3*nt/4; it < nt; it++) {
		  tr.data[it] = cmp;  
                }
		tr.sx = sx;
		tr.gx = gx;
		tr.offset = offset;
		tr.cdp = cmp;
		tr.tracl = ishot*nshot + ioff + 1;
		puttr(&tr);
          }
        }


	return(CWP_Exit());
}
