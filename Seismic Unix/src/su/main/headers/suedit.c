/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUEDIT: $Revision: 1.38 $; $Date: 2011/11/12 00:13:49 $    */

#include "su.h"
#include "segy.h"
#include "header.h"

#include <errno.h>


/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUEDIT - examine segy diskfiles and edit headers			",
" 									",
" suedit diskfile  (open for possible header modification if writable)	",
" suedit <diskfile  (open read only)					",
" 							        	",
" The following commands are recognized:				",
" number	read in that trace and print nonzero header words	",
" <CR>		go to trace one step away (step is initially -1)	",
" +		read in next trace (step is set to +1)			",
" -		read in previous trace (step is set to -1)		",
" dN		advance N traces (step is set to N)			",
" %		print some percentiles of the trace data		",
" r		print some ranks (rank[j] = jth smallest datum) 	",
" p [n1 [n2]]  	tab plot sample n1 to n2 on current trace		",
" g [tr1 tr2]  	ximage plot the trace [traces tr1 to tr2]	",
" w [tr1 tr2]  	xwigb plot the trace [traces tr1 to tr2]	",
" f [tr1 tr2]   ximage plot the amplitude spectra of the trace		",
" u [tr1 tr2]   apply user pipeline to specified traces ",
" ! key=val  	change a value in a field (e.g. ! tracr=101)		",
" ?		print help file						",
" q		quit							",
" 									",
" NOTE: sample numbers are 1-based (first sample is 1).			",
"                                                                       ",
" 'u 1000000  1000100 suwind >subset.su' will quickly extract a few     ",
" traces from the middle of a large dataset                             ",
" 									",
NULL};

/* Credits:
 * SEP: Einar Kjartansson, Shuki Ronen, Stew Levin
 * CWP: Jack K. Cohen
 * Unocal: Reg Beardsley
 * Trace header fields accessed: ns
 * Trace header fields modified: ntr (only for internal plotting)
 */
/**************** end self doc ***********************************/


segy tr;		/* a segy trace structure		*/
FILE *tty;		/* /dev/tty is used to read user input	*/
char userin[BUFSIZ];	/* buffer user requests			*/
int nt;			/* number of sample points on traces	*/
FILE *infp;		/* file pointer for trace file		*/
char tmpwig[L_tmpnam];	/* file for trace plots			*/

char *help[] = {
"					",
" n		read in trace #n	",
" <CR>		step			",
" +		next trace;   step -> +1",
" -		prev trace;   step -> -1",
" dN		adv N traces; step -> N	",
" %		percentiles		",
" r		ranks			",
" p [n1 [n2]]  	tabplot			",
" w [tr1 tr2]  	wiggle plot		",
" g [tr1 tr2]  	greyscale plot		",
" f [tr1 tr2]   amplitude spectra plot  ",
" u [tr1 tr2]   user specified pipeline ",
" ! key=val  	modify field		",
" ?		print this file		",
" q		quit			",
"					",
NULL};

char **helpptr = help ;         /* help pointer */

#define SCREENFUL	19	/* default number of points in tab plot */

/* subroutine prototypes */
void editkey(void);
void wigplot(void);
void ftwigplot(void);
int cmp_indirect();
void userwait(void);
void edxplot(int mode);
char* uptr;

int
main(int argc, char **argv)
{
	int step = -1;		/* step +1/-1 for traversing data	*/
	int itr;		/* trace number (zero based)		*/
	int ntr;		/* number of traces in data set		*/
	int *rank;		/* permuted indices for indirect sort	*/
	int i;			/* counter				*/
	int iq;			/* index of qth quantile (100qth %tile)	*/
        cwp_Bool write_ok=cwp_false;    /* is file writable?            */


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	if (argc > 2)  err("only one filename argument is allowed");

	tty = efopen("/dev/tty", "r");

	/* Open file and print editing warnings if appropriate */
	if (!isatty(STDIN)) {	/* stdin was redirected to file */
		infp = stdin;
		write_ok = cwp_false;
		warn("! examine only (no header editing from STDIN)\n");

	} else {  	/* file is given by argument */

		/* First try for read and write */
		if (0 == (access(argv[1], READ_OK | WRITE_OK))) {
			infp = fopen(argv[1], "r+");
			write_ok = cwp_true;

		/* Then for just read */
		} else if (0 == (access(argv[1], READ_OK))) {
			infp = fopen(argv[1], "r");
			write_ok = cwp_false;
			warn("! %s is readonly (no header editing)\n",
								argv[1]);
		/* Admit defeat */
		} else {
			err("can't open %s for reading", argv[1]);
		}
	}

	/* allow large file access */
	fseeko(infp,0,1);

	/* Get information from first trace */
	ntr = fgettra(infp, &tr, 0);
	nt = tr.ns;

	/* Set up array for indirect sort requested by 'r' and '%' keys */
	rank = ealloc1int(nt);
	for (i = 0; i < nt; ++i)  rank[i] = i;

	printf("%d traces in input file\n", ntr);

	/* Start from last trace */
	itr = ntr - 1;

	fgettra(infp, &tr, itr);
	printheader(&tr);
	printf("> ");
	efflush(stdout);

	/* Get user directives and do requested tasks */
	while (NULL != fgets(userin, BUFSIZ, tty)) {

                /* strip leading whitespace */
                uptr = &(userin[strspn( userin ," \t" )]);

		switch(*uptr) {
		case '0': /* check if digit */
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8':
		case '9': /* end check if digit */
			itr = eatoi(uptr) - 1;
			fgettra(infp, &tr, itr);
			printheader(&tr);
		break;
		case 'q':
        		return(CWP_Exit());
		case 'p':
			{ static int p1, p2;
			  /* Get user inputs (avoid running off end of data) */
			  switch(sscanf(uptr + 1, "%d %d", &p1, &p2)) {
			  case 2:	/* user specified final position */
				if (p2 < p1) {
					warn("need p1=%d < p2=%d", p1, p2);
        				return(CWP_Exit());
				}
				p2 = MIN(p2, nt);
			  break;
			  default:
				p2 = MIN(p1 + SCREENFUL, nt);
			  break;
			  }
			  if (p1 >= nt || p1 <= 0) p1 = 1;
			  tabplot(&tr, p1-1, p2-1); /* 1-base -> 0-base */
			  p1 = p2;
			}
		break;
		case 'g':
			edxplot(0);
		break;
		case 'w':
			edxplot(1);
		break;
		case 'f':
			edxplot(2);
		break;
		case 'u':
			edxplot(3);
		break;
		case '+':
			++itr;
			if (itr > ntr - 1) {
				step = -1;
				itr = ntr - 2;
				printf("\nBounced off end of data:\n\n");
			}
			fgettra(infp, &tr, itr);
			printheader(&tr);
			step = 1;
		break;
		case '-':
			itr--;
			if (itr < 0) {
				step = 1;
				itr = 2;
				printf("\nBounced off end of data:\n\n");
			}
			fgettra(infp, &tr, itr);
			printheader(&tr);
			step = -1;
		break;
		case '\n':
			itr += step;
			if (itr < 0 || itr > ntr - 1) {
				step *= -1;
				itr += 2*step;
				printf("\nBounced off end of data:\n\n");
			}
			fgettra(infp, &tr, itr);
			printheader(&tr);
		break;
		case 'r':
			/* sort: rank[] holds rank of datum in tr */
			qsort(rank, nt, FSIZE, cmp_indirect);

			/* Could make table of desired i's and loop */
			i = 0;
			printf(" rank[%d] = %8.2e", i+1, tr.data[rank[i]]);
			i = nt / 20;
			printf(" rank[%d] = %8.2e", i+1, tr.data[rank[i]]);
			i = nt/2 - i;
			printf(" rank[%d] = %8.2e", i+1, tr.data[rank[i]]);
			printf("\n");
			i = nt - 1 - i;
			printf(" rank[%d] = %8.2e", i+1, tr.data[rank[i]]);
			i = nt - 1 - nt/20;
			printf(" rank[%d] = %8.2e", i+1, tr.data[rank[i]]);
			i = nt - 1;
			printf(" rank[%d] = %8.2e", i+1, tr.data[rank[i]]);
			printf("\nmin is at sample %d,  max at %d\n",
					rank[0] + 1, rank[nt-1] + 1);
		break;
		case '%':
			/* sort: rank[] holds rank of datum in tr */
			qsort(rank, nt, FSIZE, cmp_indirect);

			/* round to qth quantile (100 qth percentile) */
			/* thus (q*nt - 1) + .5 (-1 for zero basing) */
			i = 1; iq = (int) (0.01*nt - 0.5);
			printf(" %dst percentile is %8.2e\n",
					i+1, tr.data[rank[iq]]);
			i = 5; iq = (int) (0.05*nt - 0.5);
			printf(" %dth percentile is %8.2e\n",
					i+1, tr.data[rank[iq]]);
			i = 25; iq = (int) (0.25*nt - 0.5);
			printf("%dth percentile is %8.2e\n",
					i+1, tr.data[rank[iq]]);
			i = 50; iq = (int) (0.50*nt - 0.5);
			printf("%dth percentile is %8.2e\n",
					i+1, tr.data[rank[iq]]);
			i = 75; iq = (int) (0.75*nt - 0.5);
			printf("%dth percentile is %8.2e\n",
					i+1, tr.data[rank[iq]]);
			i = 95; iq = (int) (0.95*nt - 0.5);
			printf("%dth percentile is %8.2e\n",
					i+1, tr.data[rank[iq]]);
			i = 99; iq = (int) (0.99*nt - 0.5);
			printf("%dth percentile is %8.2e\n",
					i+1, tr.data[rank[iq]]);
			printf("min at sample %d equals %8.2e\n",
					rank[0] + 1, tr.data[rank[0]]);
			printf("max at sample %d equals %8.2e\n",
					rank[nt-1] + 1, tr.data[rank[nt-1]]);
		break;
		case 'd':
			step = eatoi(uptr + 1);
			itr += step;
			if (itr < 0 || itr > ntr - 1) {
				step *= -1;
				itr += 2*step;
				printf("\nBounced off end of data:\n\n");
			}
			fgettra(infp, &tr, itr);
			printheader(&tr);
		break;
		case '!':
			if (write_ok) {
				editkey();
			} else {
				warn("file not writable");
			}
		break;
		case '?':
                        while(*helpptr) fprintf(stderr,"%s\n", *helpptr++);
                        helpptr = help;

		break;
		default:
			warn("bad key %s\n%s", uptr, help);
                        while(*helpptr) fprintf(stderr,"%s\n", *helpptr++);
                        helpptr = help;

		break;
		}
		printf("> ");
		efflush(stdout);
	}
        return(CWP_Exit());

}



/* Modify a header field value */
void editkey(void)
{
	cwp_String keyword;	/* buffer and then header key word	*/
	cwp_String keyval;	/* header key value in ascii		*/
	cwp_String ptr;	/* pointer to isolate key word		*/
	cwp_String type;	/* type of key word			*/
	int nsegy;	/* length of trace in bytes		*/
	Value val;	/* numerical value of key word		*/

	/* char userin[] is "!    keyword  = keyval" */

	/* Strip the '!' and any leading spaces from buffer */
	for (keyword = userin + 1; isspace((int)(*keyword)); keyword++);

	/* Set keyval to start of val */
 	if (NULL == (keyval = strchr(keyword, '=') + 1)) {
		printf("command error: format is \"! key=val\"\n");
		return;
	}

	/* Null terminate keyword (back up over = and any spaces) */
	for (ptr = keyval - 2; isspace((int)(*ptr)); ptr--);
	(*(ptr + 1)) = '\0';

	/* Convert ascii keyval string to numeric val value */
	type = hdtype(keyword);
	errno = 0;
	atoval(type, keyval, &val);
	if (errno) {
	    fprintf( stderr ,"failed to convert %s to numeric, field not changed",
								keyval);
	}

	/* Insert value into header */
	puthdval(&tr, keyword, &val);

	/* Write back the trace with the new value */
	nsegy = nt * FSIZE + HDRBYTES;
	fseeko(infp, -nsegy, SEEK_CUR);
	fwrite(&tr, 1, HDRBYTES, infp);

	/* Restore file pointer */
	fseeko(infp, nsegy, SEEK_CUR);

	/* Show the user the new header value */
	printheader(&tr);

	return;
}


/* Wiggle plot of selected adjacent traces */
void edxplot(int mode)
{
	int n1;			/* first trace to be plotted		*/
	int n2;			/* last trace to be plotted		*/
	int i;			/* counter				*/
	char cmd[8192];	        /* build command for system call	*/
	char user[8192];        /* user input buffer                    */
	FILE *tmpfp;		/* fp for suwig input file		*/


	/* Prepare temporary file to hold traces to be plotted */
	tmpfp = efopen(tmpnam(tmpwig), "w+");


	/* Parse request and subtract 1 for internal trace numbers */
	switch(sscanf(uptr + 1, "%d %d %[^\n]", &n1, &n2 ,user)) {
	case 1: /* user specified remote trace to plot */
		--n1;
		fgettra(infp, &tr, n1);
		fputtr(tmpfp, &tr);
	break;
	case 2: /* user specified block of traces to plot */
        case 3:
		if (n2 < n1) {
			warn("must specify n1=%d < n2=%d", n1, n2);
			return;
		}
		for (i = n1 - 1; i <= n2 - 1; ++i) {
			fgettra(infp, &tr, i);
			fputtr(tmpfp, &tr);
		}
	break;
	default: /* no numbers given by user: plot current trace */
		fputtr(tmpfp, &tr);
	break;
	}

	/* Set up system call */
	efclose(tmpfp);

        switch( mode ){

           case 0:
	      sprintf(cmd, "(suximage perc=99 <%s ;sleep 10;rm %s)&", tmpwig ,tmpwig);
              break;
           case 1:
	      sprintf(cmd, "(suxwigb <%s perc=99;sleep 10;rm %s)  &", tmpwig ,tmpwig);
              break;
           case 2:
	      sprintf(cmd, "(suspecfx <%s | suximage legend=1 cmap=hue perc=99;sleep 10;rm %s )&", tmpwig ,tmpwig);
              break;
           case 3:
              sprintf(cmd ,"(<%s %s ;sleep 10;rm %s)&" ,tmpwig ,user,tmpwig );
              break;
        }


	system(cmd);

	printheader(&tr);

	return;
}



/* Comparison function for qsort */
int cmp_indirect(int *r, int *s)
{
	float diff = tr.data[*r] - tr.data[*s];

	if      (diff > 0)	return(1);
	else if (diff < 0)	return(-1);
	else  /* diff == 0 */	return(0);
}


/* userwait - prompt and wait for user signal to continue */
void userwait(void)
{
	/* Note: leading newline helps some devices switch to ascii */
	fprintf(stderr, "\npress return key to continue\n");
	getc(tty);

	return;
}
