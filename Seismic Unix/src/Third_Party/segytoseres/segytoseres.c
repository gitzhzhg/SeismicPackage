/* SEGYTOSERES */

#include "su.h"
#include "segy.h"
#include <sys/types.h>
#include <netinet/in.h>

/*********************** self documentation **********************/
String sdoc =
" 								\n"
" SEGYTOSERES - convert segy input to seres ascii output        \n"
" 								\n"
" segytoseres > stdout [tape=RMTDEVICE]				\n"
" 								\n"
" Required parameters:						\n"
" 	none							\n"
" 								\n"
" Optional parameters:						\n"
" 	tape=RMTDEVICE	tape device to use, see suport.h	\n"
" 	verbose=0	silent operation			\n"
" 			= 1 ; echo every 20 traces		\n"
" 	over=0		quit if bhed format not equal 1		\n"
" 			= 1 ; override and attempt conversion  	\n"
" 			= 2 ; override and assume already converted \n"
" 	ns=bh.hns	number of samples (use if bhed ns wrong)\n"
" 	trmin=1		first trace to read			\n"
" 	trmax=LONG_MAX	last trace to read			\n"
"	ignembedded=0	= 1 ; ignore embedded single tape marks \n"
" 								\n"
" Note: If you have a tape with multiple sequences of binary	\n"
"	header, ebcdic header, traces, use the RMTDEVICE that	\n"
"	invokes the no-rewind option and issue multiple segytoseres\n"
"	commands (making an appropriate shell script if you	\n"
"	want to save all the headers).                          \n"
" 								\n"
"       You should fill in the fldr, tracf, dt, lcf, hcf, lcs, hcs\n"
"	trace header fields for SERES use.			\n"
" 								\n"
" 								\n"
;
/**************** end self doc ***********************************/

/* Credits:
 *	SEP: Einar
 *	CWP: Jack, Brian, Chris
 *      MOBIL: Stew Levin
 *             Added over=2 option to support Promax IEEE output files.
 *
 * Notes:
 *	The library subroutine, ibm_to_float, that converts IBM floating
 *	point to IEEE floating point is NOT portable and must be
 *	altered for non-IEEE machines.  See the notes in that code.
 *
 *	A direct read by dd would suck up the entire tape; hence the
 *	dancing around with buffers and files.
 *
 */

static void
prushort( unsigned short *p, int nshort )
{
int i;
for (i=0; i < nshort; i += 10)
	fprintf(stdout,"%8d%8d%8d%8d%8d%8d%8d%8d%8d%8d\n",
	ntohs(p[i]), ntohs(p[i+1]), ntohs(p[i+2]), ntohs(p[i+3]),
	ntohs(p[i+4]), ntohs(p[i+5]), ntohs(p[i+6]), ntohs(p[i+7]),
	ntohs(p[i+8]), ntohs(p[i+9]));
}

static void
prfloat(float * p, int ns )
{
int i;
int j;
j = (ns - 1)/5;
j = 5 * j;

for (i=0; i< j; i += 5)
    fprintf(stdout,"%16.6E%16.6E%16.6E%16.6E%16.6E\n",
    p[i], p[i+1], p[i+2], p[i+3], p[i+4]);

for (i=j; i<ns; i++) fprintf(stdout,"%16.6E",p[i]);
(void) putc('\n',stdout);
}

segy tr;
bhed bh;

main(int argc, char **argv)
{
	String tape;	/* name of raw tape device	*/
	int tfd;	/* file descriptor for tape	*/
	FILE *fp;	/* file pointer for popen write	*/
	unsigned
	 int nsegy;	/* size of whole trace in bytes */
	int itr;	/* current trace number		*/
	int trmin;	/* first trace to read		*/
	int trmax;	/* last trace to read		*/
        int i, j;
        unsigned short *pshort;
	int ns;		/* number of data samples	*/
	int over;	/* flag for bhed.float override	*/
	int verbose;	/* flag for echoing traces read */
	int ignembedded; /* flag to ignore embedded single tape marks */
	int neof;	/* number of consecutive eof's encountered */
	Bool nsflag;	/* flag for error in tr.ns	*/
	char cmdbuf[BUFSIZ];	/* dd command buffer	*/
	char ebcbuf[EBCBYTES];	/* ebcdic data buffer	*/


	/* Initialize */
	initargs(argc, argv);
	askdoc(0); /* stdin not used */


	/* Make sure stdout is a file or pipe */
	switch(filestat(STDOUT)) {
	case TTY:
		err("stdout can't be tty");
	break;
	case DIRECTORY:
		err("stdout must be a file, not a directory");
	break;
	case BADFILETYPE:
		err("stdout is illegal filetype");
	break;
	}


	/* Set filenames */
	if (!getparstring("tape",  &tape))	tape = RMTDEVICE;

	
	/* Set parameters */
	if (!getparint("trmin", &trmin))	trmin = 1;
	if (!getparint("trmax", &trmax))	trmax = LONG_MAX;
	if (!getparint("verbose", &verbose)) 	verbose = 0;
	if (!getparint("ignembedded", &ignembedded)) ignembedded = 0;


	/* Check if user wants to override binary format value */
	if (!getparint("over", &over))		over = 0;


	/* Open files - first the tape */
	if(strcmp(tape,"stdin"))
	tfd = eopen(tape, O_RDONLY, 0444);
	else tfd = fileno(stdin);


	/* Read the ebcdic raw bytes from the tape into the buffer */
	eread(tfd, ebcbuf, EBCBYTES);


	/* Open pipe to use dd to convert ascii to ebcdic */
	strcpy(cmdbuf, "dd ibs=3200 conv=ascii cbs=80 count=1");
	fp = epopen(cmdbuf, "w");


	/* Write ebcdic stream from buffer into pipe */
	efwrite(ebcbuf, EBCBYTES, 1, fp);

	epclose(fp);

	/* Read binary header from tape to bhed structure */
	eread(tfd, (char *) &bh, BNYBYTES);
	if (bh.format != 1) {
		(over) ? warn("ignore bh.format ... continue") :
			 err("format not IBM floating point");
		}
	if(bh.format != 5)
	    bh.format = -1;   /* indicate that file is no longer SEG-Y */


	/* Compute length of trace (can't use sizeof here!) */
	if (!getparint("ns", &ns))  ns = bh.hns; /* let user override bhed */
	if (!ns) err("samples/trace not set in binary header");
	bh.hns = ns; /* update for later output */
	nsegy = ns*4 + 240;

        pshort = (unsigned short *) (&bh);
        prushort ( pshort, sizeof(bh)/sizeof(unsigned short) );

	/* Read the traces */
	nsflag = false;
	itr = 0;
	neof = 0;
	while (itr < trmax) {
		/* read trace and check for tape mark */
		if(eread(tfd, (char *) &tr, nsegy) == 0) {
			neof++;
			if(ignembedded && neof < 2) continue;
			else break;
		} else {
			neof = 0;
		}

		/* Check tr.ns field */
		if (!nsflag && ns != tr.ns) {
			warn("discrepant tr.ns = %d with tape/user ns = %d\n"
				"\t... first noted on trace %d",
				tr.ns, ns, itr + 1);
			nsflag = true;
		}
		tr.ns = ns; /* update output trace header */

		/* Convert and write desired traces */
		if (++itr >= trmin) {
			/* Convert IBM floats to native floats */
			if(over != 2)
			    ibm_to_float((int *) tr.data, (int *) tr.data, ns);

			/* Write the trace to disk */
                        pshort = (unsigned short *) (&tr);
			prushort(pshort, (sizeof(tr)-sizeof(tr.data))/sizeof(unsigned short));

                        prfloat((&(tr.data)), ns);

			/* Echo under verbose option */
			if (verbose && itr % 20 == 0)
				warn(" %d traces from tape", itr);
		}
	}


	/* Re-iterate error in case not seen during run */
	if (nsflag) warn("discrepancy found in header and trace ns values\n"
		"the value (%d) was used to extract traces", ns);


	/* Clean up */
	eclose(tfd);

	exit(EXIT_SUCCESS);
}
