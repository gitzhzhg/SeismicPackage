/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUXEDIT: $Revision: 1.29 $; $Date: 2011/11/12 00:20:05 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <errno.h>
extern int errno;

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUXEDIT - examine segy diskfiles and edit headers			",
" 									",
" suxedit diskfile  (open for possible header modification if writable)	",
" suxedit <diskfile  (open read only)					",
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
" g [tr1 tr2] [\"opts\"] 	wiggle plot (graph) the trace		",
"				[traces tr1 to tr2]			",
" f		wiggle plot the Fourier transform of the trace		",
" ! key=val  	change a value in a field (e.g. ! tracr=101)		",
" ?		print help file						",
" q		quit							",
" 									",
" NOTE: sample numbers are 1-based (first sample is 1).			",
" 									",
NULL};

/* Credits:
 *	SEP: Einar Kjartansson, Shuki Ronen, Stew Levin
 *	CWP: Jack K. Cohen
 *
 * Trace header fields accessed: ns
 * Trace header fields modified: ntr (only for internal plotting)
 */
/**************** end self doc ***********************************/


segy tr;		/* a segy trace structure		*/
FILE *tty;		/* /dev/tty is used to read user input	*/
char userin[BUFSIZ];	/* buffer user requests			*/
int nt;			/* number of sample points on traces	*/
FILE *infp;		/* file descriptor of trace file	*/
char tmpwig[L_tmpnam];	/* file for trace plots			*/

/* tabulate help message as an array of strings */
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
" g [tr1 tr2] [\"opts\"]	wiggle plot",
" f		wig plot Fourier Transf ",
" ! key=val  	modify field		",
" ?		print this file		",
" q		quit			",
"					",
NULL};

char **helpptr = help ;		/* help pointer */

#define SCREENFUL	19	/* default number of points in tab plot */

/* subroutine prototypes */
void editkey(void);
void wigplot(void);
void ftwigplot(void);
int cmp_indirect(const void *pr, const void *ps);
void userwait(void);

int
main(int argc, char **argv)
{
	int step = -1;		/* step +1/-1 for traversing data	*/
	int itr;		/* trace number (zero based)		*/
	int ntr;		/* number of traces in data set		*/
	int *rank;		/* permuted indices for indirect sort	*/
	int i;			/* counter				*/
	int iq;			/* index of qth quantile (100qth %tile)	*/
	cwp_Bool write_ok=cwp_false;	/* is file writable?		*/


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
			infp = efopen(argv[1], "r+");
			write_ok = cwp_true;

		/* Then for just read */
		} else if (0 == (access(argv[1], READ_OK))) {
			infp = efopen(argv[1], "r");
			write_ok = cwp_false;
			warn("! %s is readonly (no header editing)\n",
								argv[1]);
		/* Admit defeat */
		} else {
			err("can't open %s for reading", argv[1]);
		}
	}

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
		switch(*userin) {
		case '0': /* check if digit */
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8':
		case '9': /* end check if digit */
			itr = eatoi(userin) - 1;
			if (itr < 0 || itr > ntr - 1) {
			 	warn("no such trace");
				itr = ntr - 1;
			}
			fgettra(infp, &tr, itr);
			printheader(&tr);
		break;
		case 'q':
			return(CWP_Exit());
		case 'p':
			{ static int p1, p2;
			  /* Get user inputs (avoid running off end of data) */
			  switch(sscanf(userin + 1, "%d %d", &p1, &p2)) {
			  case 2:	/* user specified final position */
				if (p2 < p1) {
					warn("need p1=%d < p2=%d", p1, p2);
					return 0;
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
			wigplot();
		break;
		case 'f':
			ftwigplot();
		break;
		case '+':
			++itr;
			if (itr > ntr - 1) {
				step = -1;
			    	itr = ntr - 1;
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
				itr = 0;
				printf("\nBounced off end of data:\n\n");
			}
			fgettra(infp, &tr, itr);
			printheader(&tr);
			step = -1;
		break;
		case '\n':
			itr += step;
			if (itr > ntr - 1) {
				step *= -1;
			    	itr = ntr - 1;
				printf("\nBounced off end of data:\n\n");
			}
			if (itr < 0) {
				step *= -1;
				itr = 0;
				printf("\nBounced off end of data:\n\n");
			}
			fgettra(infp, &tr, itr);
			printheader(&tr);
		break;
		case 'd':
			step = eatoi(userin + 1);
			itr += step;
			if (itr > ntr - 1) {
				step *= -1;
			    	itr = ntr - 1;
				printf("\nBounced off end of data:\n\n");
			}
			if (itr < 0) {
				step *= -1;
				itr = 0;
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
			warn("bad key %s\n", userin);
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
	cwp_String ptr;		/* pointer to isolate key word		*/
	cwp_String type;	/* type of key word			*/
	ssize_t nsegy;		/* length of trace in bytes		*/
	int databytes;		/* length of data in bytes		*/
	Value val;		/* numerical value of key word		*/

	/* char userin[] is "!    keyword  = keyval" */

	/* Strip the '!' and any leading spaces from buffer */
/* suxedit.c:339: warning: subscript has type `char' */
	for (keyword = userin + 1; isspace((int)(*keyword)); keyword++);

	/* Set keyval to start of val */
 	if (NULL == (keyval = strchr(keyword, '=') + 1)) {
		printf("command error: format is \"! key=val\"\n");
		return;
	}

	/* Null terminate keyword (back up over = and any spaces) */
/* suxedit.c:348: warning: subscript has type `char' */
	for (ptr = keyval - 2; isspace((int)(*ptr)); ptr--);
	(*(ptr + 1)) = '\0';

	/* Convert ascii keyval string to numeric val value */
	type = hdtype(keyword);
	errno = 0;
	atoval(type, keyval, &val);
	if (errno) {
	    warn("failed to convert %s to numeric, field not changed", keyval);
	}

	/* Insert value into header */
	puthdval(&tr, keyword, &val);

	/* Write back the trace with the new value */
	databytes = nt * FSIZE;
	nsegy = (ssize_t) (databytes + HDRBYTES);
	efseeko(infp, (off_t) (-nsegy), SEEK_CUR);
	efwrite(&tr, 1, HDRBYTES, infp);

	/* Restore file pointer */
	efseeko(infp,(off_t) databytes, SEEK_CUR);

	/* Show the user the new header value */
	printheader(&tr);

	return;
}


/* Wiggle plot of selected adjacent traces */
void wigplot(void)
{
	int n1;			/* first trace to be plotted		*/
	int n2;			/* last trace to be plotted		*/
	int i;			/* counter				*/
	char cmd[BUFSIZ];	/* build command for system call	*/
	char disp_opts[256];	/* display options for suxwigb		*/
	FILE *wigfp;		/* fp for suxwigb input file		*/


	/* Prepare temporary file to hold traces to be plotted */
	wigfp = efopen(tmpnam(tmpwig), "w+");



	/* Parse request and subtract 1 for internal trace numbers */
	strcpy (disp_opts, "");
	switch(sscanf(userin + 1,
		      "%d %d %[-a-zA-Z0-9.= ]", &n1, &n2, disp_opts)) {
	case 1: /* user specified remote trace to plot */
		--n1;
		fgettra(infp, &tr, n1);
		tr.ntr = 0;
		fputtr(wigfp, &tr);
	break;
	case 2: /* user specified block of traces to plot */
	case 3: /* user optionally specifies display options */
		if (n2 < n1) {
			warn("must specify n1=%d < n2=%d", n1, n2);
			return;
		}
		for (i = n1 - 1; i <= n2 - 1; ++i) {
			fgettra(infp, &tr, i);
			tr.ntr = 0;
			fputtr(wigfp, &tr);
		}
	break;
	default: /* no numbers given by user: plot current trace */
		fputtr(wigfp, &tr);
	break;
	}

	/* Set up system call to suxwigb */
	rewind(wigfp);
	sprintf(cmd, "suxwigb <%s %s", tmpwig, disp_opts);
	system(cmd);

	/* Clean up temp file */
	efclose(wigfp);
	eremove(tmpwig);

	/* Prepare for next user request */
	userwait();	/* prompt and pause till user presses return  */
	printheader(&tr);

	return;
}


/* Wiggle plot Fourier transform of current trace */
void ftwigplot(void)
{
	char cmd[BUFSIZ];	/* build command for system call	*/
	FILE *fftfp;		/* fp for suspecfx input file		*/


	/* Prepare temporary file to hold fft trace to be plotted */
	fftfp = efopen(tmpnam(tmpwig), "w+");
	tr.ntr = 0;

	fputtr(fftfp, &tr);

	/* Set up system call for suspecfx */
	rewind(fftfp);
	sprintf(cmd, "suspecfx <%s | suxwigb", tmpwig);
	system(cmd);

	/* Clean up temp file */
	efclose(fftfp);
	eremove(tmpwig);

	/* Prepare for next user request */
	userwait();	/* prompt and pause till user presses return  */
	printheader(&tr);

	return;
}


/* Comparison function for qsort */
int cmp_indirect(const void *pr, const void *ps)
{
	const int *r = pr;
	const int *s = ps;
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
