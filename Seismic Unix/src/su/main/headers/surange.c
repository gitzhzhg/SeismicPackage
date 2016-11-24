/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURANGE: $Revision: 1.19 $ ; $Date: 2013/06/24 16:56:54 $  */

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SURANGE - get max and min values for non-zero header entries	",
" 								",
" surange <stdin	 					",
"								",
" Optional parameters:						",
"	key=		Header key(s) to range (default=all)	",
" 								",
" Note: Gives partial results if interrupted			",
" 								",
" Output is: 							",
" number of traces 						",
" keyword min max (first - last) 				",
" north-south-east-west limits of shot, receiver and midpoint   ",
" 								",
NULL};

/* Credits:
 *      Stanford: Stewart A. Levin
 *              Added print of eastmost, northmost, westmost,
 *              southmost coordinates of shots, receivers, and 
 *              midpoints.  These coordinates have had any
 *              nonzero coscal header value applied.
 *	Geocon: Garry Perratt (output one header per line;
 *		option to specify headers to range;
 *		added first & last values where min<max)
 *	Based upon original by:
 *		SEP: Stew Levin
 *		CWP: Jack K. Cohen
 *
 * Note: the use of "signal" is inherited from BSD days and may
 *       break on some UNIXs.  It is dicey in that the responsibility
 *	 for program termination is lateraled back to the main.
 *
 */
/**************** end self doc ***********************************/


/* Prototypes */
void printrange(segy *tpmin, segy *tpmax, segy *tpfirst, segy *tplast);
static void closeinput(void);

static segy tr, trmin, trmax, trfirst, trlast;

int
main(int argc, char **argv)
{
	int ntr;			/* number of traces		*/
	int nkeys=0;			/* number of keywords to range	*/
	Value val;			/* value of current keyword	*/
	Value valmin;			/* smallest seen so far		*/
	Value valmax;			/* largest seen so far		*/
	cwp_String type;		/* data type of keyword		*/
	cwp_String key[SU_NKEYS];	/* array of keywords		*/

        double eastShot[2], westShot[2], northShot[2], southShot[2];
        double eastRec[2], westRec[2], northRec[2], southRec[2];
        double eastCmp[2], westCmp[2], northCmp[2], southCmp[2];
        double dcoscal = 1.0;
        double sx, sy, gx, gy, mx, my;
        int coscal = 1;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get "key" value */
	if ((nkeys=countparval("key"))!=0) {
		getparstringarray("key",key);
	}

        checkpars();

	/* Zero out values of trmin and trmax */
	memset((void *) &trmin, 0, sizeof(segy));
	memset( (void *) &trmax, 0, sizeof(segy));
        northShot[0] = southShot[0] = eastShot[0] = westShot[0] = 0.0;
        northShot[1] = southShot[1] = eastShot[1] = westShot[1] = 0.0;
        northRec[0] = southRec[0] = eastRec[0] = westRec[0] = 0.0;
        northRec[1] = southRec[1] = eastRec[1] = westRec[1] = 0.0;
        northCmp[0] = southCmp[0] = eastCmp[0] = westCmp[0] = 0.0;
        northCmp[1] = southCmp[1] = eastCmp[1] = westCmp[1] = 0.0;
        sx = sy = gx = gy = mx = my = 0.0;

	/* Set up closing commands */
	signal(SIGINT, (void (*) (int)) closeinput);
	signal(SIGTERM, (void (*) (int)) closeinput);

	/* Do first trace outside loop to initialize mins and maxs */
	if (!gettr(&tr)) err("can't get first trace");
	{	register int i;
		if (nkeys==0) {
   			for (i = 0; i < SU_NKEYS; ++i) {
 				gethval(&tr, i, &val);
 				puthval(&trmin, i, &val);
 				puthval(&trmax, i, &val);
 				puthval(&trfirst, i, &val);
                                if(i == 20) { coscal = val.h; if(coscal == 0) coscal = 1; dcoscal = (coscal > 0) ? 1.0*coscal : 1.0/coscal; }
                                if(i == 21) sx = eastShot[0] = westShot[0] = northShot[0] = southShot[0] = val.i*dcoscal;
                                if(i == 22) sy = eastShot[1] = westShot[1] = northShot[1] = southShot[1] = val.i*dcoscal;
                                if(i == 23) gx = eastRec[0] = westRec[0] = northRec[0] = southRec[0] = val.i*dcoscal;
                                if(i == 24) gy = eastRec[1] = westRec[1] = northRec[1] = southRec[1] = val.i*dcoscal;
			}
		} else	{
			register int j;
			for (i=0;i<nkeys;i++) {
				j = getindex(key[i]);
 				gethval(&tr, j, &val);
 				puthval(&trmin, j, &val);
 				puthval(&trmax, j, &val);
 				puthval(&trfirst, j, &val);
			}
		}
	}
        if(nkeys == 0) {
            mx = eastCmp[0] = westCmp[0] = northCmp[0] = southCmp[0] = 0.5*(eastShot[0]+eastRec[0]);
            my = eastCmp[1] = westCmp[1] = northCmp[1] = southCmp[1] = 0.5*(eastShot[1]+eastRec[1]);
        }

	ntr = 1;
	while (gettr(&tr)) {
		register int i;
                sx = sy = gx = gy = mx = my = 0.0;
		if (nkeys==0) {
	       		for (i = 0; i < SU_NKEYS; ++i) {
				type = hdtype(getkey(i));
				gethval(&tr, i, &val);
				gethval(&trmin, i, &valmin);
				gethval(&trmax, i, &valmax);
				if (valcmp(type, val, valmin) < 0)
					puthval(&trmin, i, &val);
				if (valcmp(type, val, valmax) > 0)
					puthval(&trmax, i, &val);
 				puthval(&trlast, i, &val);
                                if(i == 20) { coscal = val.h; if(coscal == 0) coscal = 1; dcoscal = (coscal > 0) ? 1.0*coscal : 1.0/coscal; }
                                if(i == 21)  sx = val.i*dcoscal;
                                if(i == 22)  sy = val.i*dcoscal;
                                if(i == 23)  gx = val.i*dcoscal;
                                if(i == 24)  gy = val.i*dcoscal;
			}
		} else	{
			register int j;
			for (i=0;i<nkeys;i++) {
				type = hdtype(key[i]);
				j = getindex(key[i]);
				gethval(&tr, j, &val);
				gethval(&trmin, j, &valmin);
				gethval(&trmax, j, &valmax);
				if (valcmp(type, val, valmin) < 0)
					puthval(&trmin, j, &val);
				if (valcmp(type, val, valmax) > 0)
					puthval(&trmax, j, &val);
 				puthval(&trlast, j, &val);

			}
		}
                if(nkeys == 0) {
                    mx = 0.5*(sx+gx); my = 0.5*(sy+gy);
                    if(eastShot[0] < sx) {eastShot[0] = sx; eastShot[1] = sy;}
                    if(westShot[0] > sx) {westShot[0] = sx; westShot[1] = sy;}
                    if(northShot[1] < sy){northShot[0] = sx; northShot[1] = sy;}
                    if(southShot[1] > sy){southShot[0] = sx; southShot[1] = sy;}
                    if(eastRec[0] < gx) {eastRec[0] = gx; eastRec[1] = gy;}
                    if(westRec[0] > gx) {westRec[0] = gx; westRec[1] = gy;}
                    if(northRec[1] < gy){northRec[0] = gx; northRec[1] = gy;}
                    if(southRec[1] > gy){southRec[0] = gx; southRec[1] = gy;}
                    if(eastCmp[0] < mx) {eastCmp[0] = mx; eastCmp[1] = my;}
                    if(westCmp[0] > mx) {westCmp[0] = mx; westCmp[1] = my;}
                    if(northCmp[1] < my){northCmp[0] = mx; northCmp[1] = my;}
                    if(southCmp[1] > my){southCmp[0] = mx; southCmp[1] = my;}
                }
		++ntr;
	}

	printf("%d traces:\n",ntr);
	printrange(&trmin, &trmax, &trfirst, &trlast);
        if(nkeys == 0) {
            if(northShot[1] != 0.0 || southShot[1] != 0.0 ||
               eastShot[0] != 0.0 || westShot[0] != 0.0) printf(
                   "\nShot coordinate limits:\n"
                   "\tNorth(%g,%g) South(%g,%g) East(%g,%g) West(%g,%g)\n",
                   northShot[0],northShot[1],southShot[0],southShot[1],
                   eastShot[0],eastShot[1],westShot[0],westShot[1]);
            if(northRec[1] != 0.0 || southRec[1] != 0.0 ||
               eastRec[0] != 0.0 || westRec[0] != 0.0) printf(
                   "\nReceiver coordinate limits:\n"
                   "\tNorth(%g,%g) South(%g,%g) East(%g,%g) West(%g,%g)\n",
                   northRec[0],northRec[1],southRec[0],southRec[1],
                   eastRec[0],eastRec[1],westRec[0],westRec[1]);
            if(northCmp[1] != 0.0 || southCmp[1] != 0.0 ||
               eastCmp[0] != 0.0 || westCmp[0] != 0.0) printf(
                   "\nMidpoint coordinate limits:\n"
                   "\tNorth(%g,%g) South(%g,%g) East(%g,%g) West(%g,%g)\n",
                   northCmp[0],northCmp[1],southCmp[0],southCmp[1],
                   eastCmp[0],eastCmp[1],westCmp[0],westCmp[1]);
        }

	return(CWP_Exit());
}



/* printrange - print non-zero header values ranges	*/
void printrange(segy *tpmin, segy *tpmax, segy *tpfirst, segy *tplast)
{
	register int i = 0;
	Value valmin, valmax, valfirst, vallast;
	double dvalmin, dvalmax, dvalfirst, dvallast;
	cwp_String key;
	cwp_String type;
	int kmin = 0, kmax=SU_NKEYS;

	for (i = kmin; i < kmax; ++i) {
		key = getkey(i);
		type = hdtype(key);
		gethval(tpmin, i, &valmin);
		gethval(tpmax, i, &valmax);
		gethval(tpfirst, i, &valfirst);
		gethval(tplast, i, &vallast);
		dvalmin = vtod(type, valmin);
		dvalmax = vtod(type, valmax);
		dvalfirst = vtod(type, valfirst);
		dvallast = vtod(type, vallast);
		if (dvalmin || dvalmax) {
			if (dvalmin < dvalmax) {
				printf("%-8s ", key);
				printfval(type, valmin);
				printf(" ");
				printfval(type, valmax);
				printf(" (");
				printfval(type, valfirst);
				printf(" - ");
				printfval(type, vallast);
				printf(")");
			} else {
				printf("%-8s ", key);
				printfval(type, valmin);
			}
			putchar('\n');
		}
	}
	return;
}


static void closeinput(void) /* for graceful interrupt termination */
{
	/* Close stdin and open /dev/null in its place.  Now we are reading */
	/* from an empty file and the loops terminate in a normal fashion.  */

	efreopen("/dev/null", "r", stdin);
}
