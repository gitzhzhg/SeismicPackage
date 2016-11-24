/* SEGYREAD: $Revision: 1.75 $ ; $Date: 2015/08/07 21:59:07 $     */

#define _XOPEN_SOURCE
#include <unistd.h>
#include "su.h"
#include "segy.h"
#include "tapesegy.h"
#include "tapebhdr.h"
#include "bheader.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SEGYREAD - read an SEG-Y tape						",
"									",
"   segyread > stdout tape=						",
"									",
"   or									",
"									",
"   SEG-Y data stream ... | segyread tape=-  > stdout			",
"									",
" Required parameter:							",
" tape=		input tape device or seg-y filename (see notes)		",
"									",
" Optional parameters:							",
" buff=1	for buffered device (9-track reel tape drive)		",
"		=0 possibly useful for 8mm EXABYTE drives		",
" verbose=0	silent operation					",
"		=1 ; echo every 'vblock' traces				",
" vblock=50	echo every 'vblock' traces under verbose option		",
" hfile=header	file to store ebcdic block (as ascii)			",
" bfile=binary	file to store binary block				",
" xfile=xhdrs	file to store extended text block			",
" over=0	quit if bhed format not equal 1, 2, 3, 5, or 8		",
"		= 1 ; override and attempt conversion			",
" format=bh.format	if over=1 try to convert assuming format value  ",
" conv=1	convert data to native format				",
"			= 0 ; assume data is in native format		",
" ebcdic=1	perform ebcdic to ascii conversion on 3200 byte textural",
"               header. =0 do not perform conversion			",
" ns=bh.hns	number of samples (use if bhed ns wrong)		",
" trcwt=1	apply trace weighting factor (bytes 169-170)		",
"		=0, do not apply.  (Default is 0 for formats 1 and 5)	",
" trmin=1		first trace to read				",
" trmax=INT_MAX	last trace to read					",
" endian=(autodetected) =1 for big-endian,  =0 for little-endian byte order",
" swapbhed=endian	swap binary reel header?			",
" swaphdrs=endian	swap trace headers?				",
" swapdata=endian	swap data?					",
" errmax=0	allowable number of consecutive tape IO errors		",
" remap=...,...	remap key(s) 						",
" byte=...,...	formats to use for header remapping 			",
" 									",
" Notes:								",
" Traditionally tape=/dev/rmt0.	 However, in the modern world tape device",
" names are much less uniform.  The magic name can often be deduced by	",
" \"ls /dev\".  Likely man pages with the names of the tape devices are:",
" \"mt\", \"sd\" \"st\".  Also try \"man -k scsi\", \" man mt\", etc.	",
" Sometimes \"mt status\" will tell the device name.			",
" 									",
" For a SEG-Y diskfile use tape=filename.				",
" 									",
" The xfile argument will only be used if the file contains extended	",
" text headers.								",
" 									",
" Remark: a SEG-Y file is not the same as an su file. A SEG-Y file	",
" consists of three parts: an ebcdic header, a binary reel header, and	",
" the traces.  The traces are (usually) in 32 bit IBM floating point	",
" format.  An SU file consists only of the trace portion written in the ",
" native binary floats.							",
"									",
" Formats supported:							",
" 1: IBM floating point, 4 byte (32 bits)				",
" 2: two's complement integer, 4 byte (32 bits)				",
" 3: two's complement integer, 2 byte (16 bits)				",
" 5: IEEE floating point, 4 byte (32 bits)				",
" 8: two's complement integer, 1 byte (8 bits)				",
"									",
" tape=-   read from standard input. Caveat, under Solaris, you will	",
" need to use the buff=1 option, as well.				",
"									",
" Header remap:								",
" The value of header word remap is mapped from the values of byte	",
"									",
" Map a float at location 221 to sample spacing d1:			",
"	segyread <data >outdata remap=d1 byte=221f			",
"									",
" Map a long at location 225 to source location sx:			",
"	segyread <data >outdata remap=sx byte=225l			",
"									",
" Map a short at location 229 to gain constant igc:			",
"	segyread <data >outdata remap=igc byte=229s			",
"									",
" Or all combined: 							",
"	segyread <data >outdata remap=d1,sx,igc byte=221f,225l,229s	",
"									",
" Segy header words are accessed as Xt where X denotes the byte number	",
" starting at 1 in correspondance with the SEGY standard (1975)		",
" Known types include:	f	float (4 bytes)				",
" 			l	long int (4 bytes)			",
" 			s	short int (2 bytes)			",
" 			b	byte (1 bytes)				",
"									",
"	  type:	  sudoc segyread   for further information		",
"									",
NULL};

/*
 * Note:
 *      If you have a tape with multiple sequences of ebcdic header,
 *	binary header,traces, use the device that
 *	invokes the no-rewind option and issue multiple segyread
 *	commands (making an appropriate shell script if you
 *	want to save all the headers).	Consider using >> if
 *	you want a single trace file in the end.  Similar
 *	considerations apply for multiple reels of tapes,
 *	but use the standard rewind on end of file.
 *
 * Note: For buff=1 (default) tape is accessed with 'read', for buff=0
 *	tape is accessed with fread. We suggest that you try buff=1
 *	even with EXABYTE tapes.
 * Caveat: may be slow on an 8mm streaming (EXABYTE) tapedrive
 * Warning: segyread or segywrite to 8mm tape is fragile. Allow sufficient
 *	time between successive reads and writes.
 * Warning: may return the error message "efclose: fclose failed"
 *	intermittently when segyreading/segywriting to 8mm (EXABYTE) tape
 *	even if actual segyread/segywrite is successful. However, this
 *	error message may be returned if your tape drive has a fixed
 *	block size set.
 * Caution: When reading or writing SEG-Y tapes, the tape
 *	drive should be set to be able to read variable block length
 *	tape files.
 */

/* Credits:
 *	SEP: Einar Kjartansson
 *	CWP: Jack K. Cohen, Brian Sumner, Chris Liner
 *	   : John Stockwell (added 8mm tape stuff)
 * conv parameter added by:
 *	Tony Kocurko
 *	Department of Earth Sciences
 *	Memorial University of Newfoundland
 *	St. John's, Newfoundland
 * read from stdin via tape=-  added by	Tony Kocurko
 * bhed format = 2,3 conversion by:
 *	Remco Romijn (Applied Geophysics, TU Delft)
 *	J.W. de Bruijn (Applied Geophysics, TU Delft)
 * bhed format = 8 conversion by: John Stockwell
 * header remap feature added by:
 * 	Matthias Imhof, Virginia Tech
 *--------------------------
 * Additional Notes:
 *	Brian's subroutine, ibm_to_float, which converts IBM floating
 *	point to IEEE floating point is NOT portable and must be
 *	altered for non-IEEE machines.	See the subroutine notes below.
 *
 *	A direct read by dd would suck up the entire tape; hence the
 *	dancing around with buffers and files.
 *
 */
/**************** end self doc ***********************************/

#define SEGYREAD_TAPE_ERROR(which) \
	if (verbose) \
		warn("tape read error on %s", which); \
	if (++errcount > errmax) \
		err("exceeded maximum io errors"); \

#define SEGYREAD_TAPE_ERROR_TRACE(which) \
	if (verbose) \
		warn("tape read error on trace %d", which); \
	if (++errcount > errmax) \
		err("exceeded maximum io errors"); \

/* Subroutine prototypes */
static void ibm_to_float(int from[], int to[], int n, int endian, int verbose);
static void int_to_float(int from[], float to[], int n, int endian);
static void short_to_float(short from[], float to[], int n, int endian);
static void integer1_to_float(signed char from[], float to[], int n);
static void tapebhed_to_bhed(const tapebhed *tapebhptr, bhed *bhptr);
static void tapesegy_to_segy(const tapesegy *tapetrptr, segy *trptr);

static void ugethval(cwp_String type1, Value *valp1,
		     char type2, int ubyte,
		      char *tr, int endian, int conv, int verbose);

/* Globals */
tapesegy tapetr;
tapebhed tapebh;
segy tr;
bhed bh;

int
main(int argc, char **argv)
{
	char *tape;		/* name of raw tape device	*/
	char *bfile;		/* name of binary header file	*/
	char *hfile;		/* name of ascii header file	*/
	char *xfile;		/* name of extended header file	*/

	int tapefd = 0;		/* file descriptor for tape	*/

	FILE *tapefp = NULL;	/* file pointer for tape	*/
	FILE *binaryfp;		/* file pointer for bfile	*/
	FILE *headerfp;		/* file pointer for hfile	*/
	FILE *xheaderfp;	/* file pointer for xfile	*/
	FILE *pipefp;		/* file pointer for popen write */

	size_t nsegy;		/* size of whole trace in bytes		*/
	int i;			/* counter				*/
	int itr;		/* current trace number			*/
	int trmin;		/* first trace to read			*/
	int trmax;		/* last trace to read			*/
	int ns;			/* number of data samples		*/

	int over;		/* flag for bhed.float override		*/
	int format;		/* flag for to specify override format	*/
	cwp_Bool format_set = cwp_false;
				/* flag to see if new format is set	*/
	int conv;		/* flag for data conversion		*/
	int trcwt;		/* flag for trace weighting		*/
	int verbose;		/* echo every ...			*/
	int vblock;		/* ... vblock traces with verbose=1	*/
	int buff;		/* flag for buffered/unbuffered device	*/
	int endian;		/* flag for big=1 or little=0 endian	*/
	int swapdata;		/* flag for big=1 or little=0 endian	*/
	int swapbhed;		/* flag for big=1 or little=0 endian	*/
	int swaphdrs;		/* flag for big=1 or little=0 endian	*/
	int errmax;		/* max consecutive tape io errors	*/
	int errcount = 0;	/* counter for tape io errors		*/
	cwp_Bool nsflag;	/* flag for error in tr.ns		*/

	char cmdbuf[BUFSIZ];	/* dd command buffer			*/
	char ebcbuf[EBCBYTES];	/* ebcdic data buffer			*/

	int ebcdic=1;		/* ebcdic to ascii conversion flag	*/

	cwp_String key1[SU_NKEYS];	/* output key(s)		*/
	cwp_String key2[SU_NKEYS];	/* first input key(s)		*/
	cwp_String type1[SU_NKEYS];	/* array of types for key1	*/
	char type2[SU_NKEYS];		/* array of types for key2	*/
	int ubyte[SU_NKEYS];
	int nkeys;			/* number of keys to be computed*/
	int n;				/* counter of keys getparred	*/
	int ikey;			/* loop counter of keys 	*/
	int index1[SU_NKEYS];		/* array of indexes for key1 	*/
	Value val1;			/* value of key1		*/

	/* deal with number of extended text headers */
	short nextended;

	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */


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
	default: /* Others OK */
		break;
	}

	/* Set filenames */
	MUSTGETPARSTRING("tape",  &tape);
	if (!getparstring("hfile", &hfile))	hfile = "header";
	if (!getparstring("bfile", &bfile))	bfile = "binary";
	if (!getparstring("xfile", &xfile))	xfile = "xhdrs";


	/* Set parameters */
	if (!getparint("trmin", &trmin))	trmin = 1;
	if (!getparint("trmax", &trmax))	trmax = INT_MAX;
	if (!getparint("verbose", &verbose))	verbose = 0;
	if (!getparint("vblock", &vblock))	vblock = 50;
	if (!getparint("endian", &endian))      {
              union { short s; char c[2]; } testend;
                testend.s = 1;
 		endian = (testend.c[0] == '\0') ? 1 : 0;
	}
	if (!getparint("swapdata", &swapdata)) 	swapdata=endian;
	if (!getparint("swapbhed", &swapbhed)) 	swapbhed=endian;
	if (!getparint("swaphdrs", &swaphdrs)) 	swaphdrs=endian;
	
	if (!getparint("ebcdic", &ebcdic)) 	ebcdic=1;

	if (!getparint("errmax", &errmax))	errmax = 0;
	if (!getparint("buff", &buff))		buff = 1;


	/* Override binary format value */
	if (!getparint("over", &over))		over = 0;
	if (getparint("format", &format))	format_set = cwp_true;
	if (((over!=0) && (format_set))) {
		bh.format = format;
		if ( !(
			   (format == 1)
			|| (format == 2)
			|| (format == 3)
			|| (format == 5)
			|| (format == 8)
			) ) {
 	          warn("Specified format=%d not supported", format);
		  warn("Assuming IBM floating point format, instead");
		}
	}

	/* Override conversion of IBM floating point data? */
	if (!getparint("conv", &conv))		conv = 1;

	/* Get parameters */
	/* get key1's */
	if ((n = countparval("remap")) != 0){
		nkeys = n;
		getparstringarray("remap", key1);
	} else { /* set default */
		nkeys = 0;
	}

	/* get key2's */
	if ((n = countparval("byte")) != 0){
		if (n != nkeys)
			err("number of byte's and remap's must be equal!");

		getparstringarray("byte", key2);
	}

	for (ikey = 0; ikey < nkeys; ++ikey) {

		/* get types and index values */
		type1[ikey]  = hdtype(key1[ikey]);
		index1[ikey] = getindex(key1[ikey]);
		}

	for (ikey = 0; ikey < nkeys; ++ikey) {
		#if 0
		fprintf(stderr, "%s\t", key2[ikey]);
		#endif
		if (sscanf(key2[ikey],"%d%c", &ubyte[ikey], &type2[ikey]) != 2)
			err("user format XXXt");
		#if 0
		sscanf(key2[ikey],"%d%c", &ubyte[ikey], &type2[ikey]);
		fprintf(stderr, "%d\t%c\n", ubyte[ikey], type2[ikey]);
		#endif
		}

	/* Open files - first the tape */
	if (buff) {
		if ( tape[0] == '-' && tape[1] == '\0' ) tapefd = 0;
		else tapefd = eopen(tape, O_RDONLY, 0444);
	} else {
		if ( tape[0] == '-' && tape[1] == '\0' ) tapefp = stdin;
		else tapefp = efopen(tape, "r");
	}

	/* - the ebcdic header file in ascii */
	headerfp = efopen(hfile, "w");
	if (verbose) warn("header file opened successfully");

	/* - the binary data file */
	binaryfp = efopen(bfile, "w");
	if (verbose) warn("binary file opened successfully");

	/* Read the ebcdic raw bytes from the tape into the buffer */
	if (buff) {
		if (-1 == (int) read(tapefd, ebcbuf, EBCBYTES)) {
			SEGYREAD_TAPE_ERROR("ebcdic header");
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	} else {
/* segyread.c:350: warning: value computed is not used */
/* 		 (int) fread(ebcbuf, 1, EBCBYTES, tapefp); */
		 fread(ebcbuf, 1, EBCBYTES, tapefp);
		 if (ferror(tapefp)) {
			SEGYREAD_TAPE_ERROR("ebcdic header");
			clearerr(tapefp);
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	}

	/* Open pipe to use dd to convert  ebcdic to ascii */
#ifdef SUN
	if (ebcdic==1)  {
		sprintf(cmdbuf, "dd ibs=1 of=%s conv=ascii count=3200", hfile);
	} else {
		sprintf(cmdbuf, "dd ibs=1 of=%s count=3200", hfile);
	}
#else
	/* this command gives a file containing 3240 bytes on sun */
	/* see top of Makefile.config for versions */
	/* not sure why this breaks now; works in version 37 */
	if (ebcdic==1){
		sprintf(cmdbuf, "dd ibs=1 of=%s conv=ascii cbs=80 count=3200", hfile);
	} else {
		
		sprintf(cmdbuf, "dd ibs=1 of=%s cbs=80 count=3200", hfile);
	}
#endif
	pipefp = epopen(cmdbuf, "w");

	/* Write ebcdic stream from buffer into pipe */
	efwrite(ebcbuf, EBCBYTES, 1, pipefp);

	/* Read binary header from tape to bh structure */
	if (buff) {
		if (-1 == read(tapefd, (char *) &tapebh, BNYBYTES)) {
			SEGYREAD_TAPE_ERROR("binary header");
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	} else {
		 fread((char *) &tapebh, 1, BNYBYTES, tapefp);
		 if (ferror(tapefp)) {
			 SEGYREAD_TAPE_ERROR("binary header");
			 clearerr(tapefp);
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	}

	/* Convert from bytes to ints/shorts */
	tapebhed_to_bhed(&tapebh, &bh);

	/* if little endian machine, swap bytes in binary header */
	if (swapbhed == 0) for (i = 0; i < BHED_NKEYS; ++i) swapbhval(&bh, i);

	/* Override binary format value */
	if (!getparint("over", &over))		over = 0;
	if (getparint("format", &format))	format_set = cwp_true;
	if (((over!=0) && (format_set)))	bh.format = format;

	/* Override application of trace weighting factor? */
	/* Default no for floating point formats, yes for integer formats. */
	if (!getparint("trcwt",&trcwt))
		trcwt = (bh.format == 1 || bh.format == 5) ? 0 : 1;

	switch (bh.format) {
	case 1:
		if (verbose) warn("assuming IBM floating point input");
		break;
	case 2:
		if (verbose) warn("assuming 4 byte integer input");
		break;
	case 3:
		if (verbose) warn("assuming 2 byte integer input");
		break;
	case 5:
		if (verbose) warn("assuming IEEE floating point input");
		break;
	case 8:
		if (verbose) warn("assuming 1 byte integer input");
		break;
	default:
		(over) ? warn("ignoring bh.format ... continue") :
			err("format not SEGY standard (1, 2, 3, 5, or 8)");
	}

	/* Compute length of trace (can't use sizeof here!) */
	if (!getparint("ns", &ns))  ns = bh.hns; /* let user override */
	if (!ns) err("samples/trace not set in binary header");
	bh.hns = ns;
	switch (bh.format) {
	case 8:
		nsegy = ns + SEGY_HDRBYTES;
		break;
	case 3:
		nsegy = ns * 2 + SEGY_HDRBYTES;
		break;
	case 1:
	case 2:
	case 5:
	default:
		nsegy = ns * 4 + SEGY_HDRBYTES;
	}

	/* Write binary header from bhed structure to binary file */
	efwrite( (char *) &bh, 1, BNYBYTES, binaryfp);

	/* Close binary and header files now to allow pipe into segywrite */
	efclose(binaryfp);
	if (verbose) warn("binary file closed successfully");
	efclose(headerfp);
	epclose(pipefp);
	if (verbose) warn("header file closed successfully");

        getparshort( "nextended" ,&nextended );
        checkpars();

	nextended = *((short *) (((unsigned char *)&tapebh) + 304));
	if (endian == 0) swap_short_2((short *) &nextended);
	if (verbose) warn("Number of extended text headers: %d", nextended);
	if (nextended > 0) /* number of extended text headers > 0 */
	{
		/* need to deal with -1 nextended headers */
		/* so test should actually be !=0, but ... */

		/* open the extended text header file in whatever it's in */
		xheaderfp = efopen(xfile, "w");
		if (verbose)
			warn("extended text header file opened successfully");

		for (i = 0; i < nextended; i++) {
			/* cheat -- an extended text header is same size as
			 * EBCDIC header */
			/* Read the bytes from the tape for one xhdr into the
			 * buffer */
			if (buff) {
				if (-1 == (int) read(tapefd, ebcbuf, EBCBYTES)) {
					SEGYREAD_TAPE_ERROR("extended text header");
				} else { /* Reset counter on successful tape IO */
					errcount = 0;
				}
			} else {
				fread(ebcbuf, 1, EBCBYTES, tapefp);
				if (ferror(tapefp)) {
					SEGYREAD_TAPE_ERROR("extended text header");
					clearerr(tapefp);
				} else { /* Reset counter on successful tape IO */
					errcount = 0;
				}
			}
			/* Write extended header to file */
			efwrite( (char *) ebcbuf, 1, EBCBYTES, xheaderfp);
		}
		/* Close extended text header file */
		efclose(xheaderfp);
		if (verbose) warn("extended text header file closed successfully");
	}

	/* Read the traces */
	nsflag = cwp_false;
	itr = 0;
	while (itr < trmax) {
		int nread;

		if (buff) {
			if (-1 ==
			   (nread = (int) read(tapefd, (char *) &tapetr, nsegy))){

				SEGYREAD_TAPE_ERROR_TRACE(itr);
			} else { /* Reset counter on successful tape IO */
				errcount = 0;
			}
		} else {
			nread = (int) fread((char *) &tapetr, 1, nsegy, tapefp);
			if (ferror(tapefp)) {
				SEGYREAD_TAPE_ERROR_TRACE(itr);
				clearerr(tapefp);
			} else { /* Reset counter on successful tape IO */
				errcount = 0;
			}
		}

		if (!nread)   /* middle exit loop instead of mile-long while */
			break;

		/* Convert from bytes to ints/shorts */
		tapesegy_to_segy(&tapetr, &tr);

		/* If little endian machine, then swap bytes in trace header */
		if (swaphdrs == 0)
			for (i = 0; i < SEGY_NKEYS; ++i) swaphval(&tr, i);

		/* Check tr.ns field */
		if (!nsflag && ns != tr.ns) {
			warn("discrepant tr.ns = %d with tape/user ns = %d\n"
			     "\t... first noted on trace %d",
			     tr.ns, ns, itr + 1);
			nsflag = cwp_true;
		}

		/* loop over key fields and remap */
		for (ikey = 0; ikey < nkeys; ++ikey) {

			/* get header values */

			ugethval(type1[ikey], &val1,
				 type2[ikey], ubyte[ikey] - 1,
				 (char*) &tapetr, endian, conv, verbose);
			puthval(&tr, index1[ikey], &val1);
		}
		/* Are there different swapping instructions for the data */
		/* Convert and write desired traces */
		if (++itr >= trmin) {

			/* Convert IBM floats to native floats */
			if (conv) {
				switch (bh.format) {
				case 1:
			/* Convert IBM floats to native floats */
					ibm_to_float((int *) tr.data,
						     (int *) tr.data, ns,
						     swapdata,verbose);
					break;
				case 2:
			/* Convert 4 byte integers to native floats */
					int_to_float((int *) tr.data,
						      (float *) tr.data, ns,
						      swapdata);
					break;
				case 3:
			/* Convert 2 byte integers to native floats */
					short_to_float((short *) tr.data,
						       (float *) tr.data, ns,
						       swapdata);
					break;
				case 5:
			/* IEEE floats.  Byte swap if necessary. */
                                        if (swapdata == 0)
						for (i = 0; i < ns ; ++i)
							swap_float_4(&tr.data[i]);
					break;
				case 8:
			/* Convert 1 byte integers to native floats */
					integer1_to_float((signed char *)tr.data,
							  (float *) tr.data, ns);
					break;
				}

				/* Apply trace weighting. */
				if (trcwt && tr.trwf != 0) {
			  		float scale = pow(2.0, -tr.trwf);
				  	int i;
				  	for (i = 0; i < ns; ++i) {
				    		tr.data[i] *= scale;
				  	}
				}
			} else if (conv == 0) {
				/* don't convert, if not appropriate */

                                switch (bh.format) {
                                case 1: /* swapdata=0 byte swapping */
				case 5:
                                        if (swapdata == 0)
						for (i = 0; i < ns ; ++i)
							swap_float_4(&tr.data[i]);
					break;
                                case 2: /* convert longs to floats */
                                        /* SU has no provision for reading */
                                        /* data as longs */
                                        int_to_float((int *) tr.data,
                                                (float *) tr.data, ns, endian);
                                        break;
                                case 3: /* shorts are the SHORTPAC format */
					/* used by supack2 and suunpack2 */
                                        if (swapdata == 0)/* swapdata=0 byte swap */
                                           for (i = 0; i < ns ; ++i)
                                                swap_short_2((short *) &tr.data[i]);
                                        /* Set trace ID to SHORTPACK format */
                                        tr.trid = SHORTPACK;
                                        break;
                                case 8: /* convert bytes to floats */
                                        /* SU has no provision for reading */
                                        /* data as bytes */
					integer1_to_float((signed char *)tr.data,
					(float *) tr.data, ns);
                                        break;
                                }
                        }

			/* Write the trace to disk */
			tr.ns = ns;
			puttr(&tr);

			/* Echo under verbose option */
			if (verbose && itr % vblock == 0)
				warn(" %d traces from tape", itr);
		}
	}

	/* Re-iterate error in case not seen during run */
	if (nsflag) warn("discrepancy found in header and trace ns values\n"
		"the value (%d) was used to extract traces", ns);


	/* Clean up (binary & header files already closed above) */
	(buff) ? eclose(tapefd):
		 efclose(tapefp);
	if (verbose) warn("tape closed successfully");


	return(CWP_Exit());
}

static void ibm_to_float(int from[], int to[], int n, int endian, int verbose)
/***********************************************************************
ibm_to_float - convert between 32 bit IBM and IEEE floating numbers
************************************************************************
Input::
from		input vector
to		output vector, can be same as input vector
endian		byte order =0 little endian (DEC, PC's)
			    =1 other systems
*************************************************************************
Notes:
Up to 3 bits lost on IEEE -> IBM

Assumes sizeof(int) == sizeof(float) == 4

IBM -> IEEE may overflow or underflow, taken care of by
substituting large number or zero

*************************************************************************
Credits: SEP: Stewart A. Levin,  c.1995
*************************************************************************/

/* See if this fits the bill for your needs - Stew */
/* ibmflt.f -- translated by f2c (version 1995/10/25).
*/
/* Subroutine */
{
    /* Initialized data */

    static int first = 1;

    /* System generated locals */
    int i__1;
    int j,k;

    /* Local variables */
    int   *in;
    float *out;
    int eibm, i__, mhibm;
    static int m1[512];
    static float r1[512];

    unsigned int jj;
    union {
      float rrf;
      int iif;
      unsigned int uuf;
    } cvtmp;
	
    float r_infinity__;
    int et3e;

    if(endian == 0) {
      swab(from,to,n*sizeof(int));
      for(i__ = 0; i__<n; ++i__) {
        j = to[i__];
        k = j<<16;
        to[i__] = k+((j>>16)&65535);
      }
      in = to;
    } else {
      in = from;
    }
    /* Parameter adjustments */
    out = (float *) to;
    --out;
    --in;
    /* Function Body */

    if (first) {
	first = ! first;
	cvtmp.iif = 2139095039;
	r_infinity__ = cvtmp.rrf;
	for (i__ = 0; i__ <= 511; ++i__) {
	    i__1 = i__ & 255;
	    eibm = i__1 >> 1;
	    mhibm = i__ & 1;
	    et3e = (eibm << 2) - 130;
	    if (et3e > 0 && et3e <= 255) {
		i__1 = et3e ^ (i__ & 255);
		m1[i__] = i__1 << 23;
		if (mhibm == 1) {
		    r1[i__] = 0.f;
		} else {
		    i__1 = et3e | (i__ & 256);
		    cvtmp.iif = i__1 << 23;
		    r1[i__] = -(cvtmp.rrf);
		}
	    } else if (et3e <= 0) {
		m1[i__] = i__ << 23;
		r1[i__] = 0.f;
	    } else {
		m1[i__] = i__ << 23;
		if (i__ < 256) {
		    r1[i__] = r_infinity__;
		} else {
		    r1[i__] = -r_infinity__;
		}
	    }
/* L10: */
	}
    }

    for (i__ = 1; i__ <= n; ++i__) {
	cvtmp.iif = in[i__];
/* use 9 high bits for table lookup */
	jj = cvtmp.uuf>>23;
/* fix up exponent */
	cvtmp.iif = m1[jj] ^ cvtmp.iif;
/* fix up mantissa */
	out[i__] = cvtmp.rrf + r1[jj];
/* L20: */
    }
}

static void tapebhed_to_bhed(const tapebhed *tapebhptr, bhed *bhptr)
/****************************************************************************
tapebhed_to_bhed -- converts the seg-y standard 2 byte and 4 byte
	integer header fields to, respectively, the
	machine's short and int types.
*****************************************************************************
Input:
tapbhed		pointer to array of
*****************************************************************************
Notes:
The present implementation assumes that these types are actually the "right"
size (respectively 2 and 4 bytes), so this routine is only a placeholder for
the conversions that would be needed on a machine not using this convention.
*****************************************************************************
Author: CWP: Jack  K. Cohen, August 1994
****************************************************************************/

{
	register int i;
	Value val;

	/* convert binary header, field by field */
	for (i = 0; i < BHED_NKEYS; ++i) {
		gettapebhval(tapebhptr, i, &val);
		putbhval(bhptr, i, &val);
	}
}

static void tapesegy_to_segy(const tapesegy *tapetrptr, segy *trptr)
/****************************************************************************
tapesegy_to_segy -- converts the seg-y standard 2 byte and 4 byte
		    integer header fields to, respectively, the machine's
		    short and int types.
*****************************************************************************
Input:
tapetrptr	pointer to trace in "tapesegy" (SEG-Y on tape) format

Output:
trptr		pointer to trace in "segy" (SEG-Y as in	 SU) format
*****************************************************************************
Notes:
Also copies float data byte by byte.  The present implementation assumes that
the integer types are actually the "right" size (respectively 2 and 4 bytes),
so this routine is only a placeholder for the conversions that would be needed
on a machine not using this convention.	 The float data is preserved as
four byte fields and is later converted to internal floats by ibm_to_float
(which, in turn, makes additonal assumptions).
*****************************************************************************
Author: CWP:Jack K. Cohen,  August 1994
****************************************************************************/
{
	register int i;
	Value val;

	/* convert header trace header fields */
	for (i = 0; i < SEGY_NKEYS; ++i) {
		gettapehval(tapetrptr, i, &val);
		puthval(trptr, i, &val);
	}

	/* copy the optional portion */
	memcpy((char *)&(trptr->otrav) + 2, tapetrptr->unass, 60);

	/* copy data portion */
	memcpy(trptr->data, tapetrptr->data, 4 * SU_NFLTS);
}

static void int_to_float(int from[], float to[], int n, int endian)
/****************************************************************************
Author:	J.W. de Bruijn, May 1995
****************************************************************************/
{
	register int i;

	if (endian == 0) {
		for (i = 0; i < n; ++i) {
			swap_int_4(&from[i]);
			to[i] = (float) from[i];
		}
	} else {
		for (i = 0; i < n; ++i) {
			to[i] = (float) from[i];
		}
	}
}

static void short_to_float(short from[], float to[], int n, int endian)
/****************************************************************************
short_to_float - type conversion for additional SEG-Y formats
*****************************************************************************
Author: Delft: J.W. de Bruijn, May 1995
Modified by: Baltic Sea Reasearch Institute: Toralf Foerster, March 1997
****************************************************************************/
{
	register int i;

	if (endian == 0) {
		for (i = n - 1; i >= 0 ; --i) {
			swap_short_2(&from[i]);
			to[i] = (float) from[i];
		}
	} else {
		for (i = n - 1; i >= 0 ; --i)
			to[i] = (float) from[i];
	}
}

static void integer1_to_float(signed char from[], float to[], int n)
/****************************************************************************
integer1_to_float - type conversion for additional SEG-Y formats
*****************************************************************************
Author: John Stockwell,  2005
****************************************************************************/
{
  	while (n--) {
		to[n] = from[n];
	}
}

void ugethval(cwp_String type1, Value *valp1,
	      char type2, int ubyte,
	      char *ptr2, int endian, int conv, int verbose)
{	double dval1 = 0;
	char   c = 0;
	short  s = 0;
	int    l = 0;
	float  f = 0.0;
	char	*ptr1;

	#if 0
	fprintf(stderr, "start ugethval %d %c\n", ubyte, type2);
	#endif

	switch (type2) {
	case 'b':
		ptr1 = (char*) &c;
		ptr1[0] = ptr2[ubyte];
		dval1 = (double) c;
		break;
	case 's':
		ptr1 = (char*) &s;
		ptr1[0] = ptr2[ubyte];
		ptr1[1] = ptr2[ubyte+1];
		if (endian == 0)
		   swap_short_2(&s);
		dval1 = (double) s;
		break;
	case 'l':
		ptr1 = (char*) &l;
		ptr1[0] = ptr2[ubyte];
		ptr1[1] = ptr2[ubyte+1];
		ptr1[2] = ptr2[ubyte+2];
		ptr1[3] = ptr2[ubyte+3];
		if (endian == 0)
/* segyread.c:903: warning: dereferencing type-punned pointer will break strict-aliasing rules */
/* 		   swap_long_4((long *)&l); */
/* note: long is 64-bits on 64-bit machine! */
			swap_int_4((int *)&l);
		dval1 = (double) l;
		break;
	case 'f':
		ptr1 = (char*) &f;
		ptr1[0] = ptr2[ubyte];
		ptr1[1] = ptr2[ubyte+1];
		ptr1[2] = ptr2[ubyte+2];
		ptr1[3] = ptr2[ubyte+3];
		if (conv)
/* get this message twice */
/* segyread.c:913: warning: dereferencing type-punned pointer will break strict-aliasing rules */
/* 		   ibm_to_float((int*) &f, (int*) &f, 1, endian, verbose); */
		  {
			  memcpy (&l, &f, 4);
			  ibm_to_float(&l, &l, 1, endian, verbose);
		  }
		else if (conv == 0 && endian == 0)
		   swap_float_4(&f);
		dval1 = (double) f;
		break;
	default:
		err("unknown type %s", type2);
		break;
	}

	#if 0
	fprintf(stderr, "value %lf\n", dval1);
	#endif

	switch (*type1) {
	case 's':
		err("can't change char header word");
		break;
	case 'h':
		valp1->h = (short) dval1;
		break;
	case 'u':
		valp1->u = (unsigned short) dval1;
		break;
	case 'l':
		valp1->l = (long) dval1;
		break;
	case 'v':
		valp1->v = (unsigned long) dval1;
		break;
	case 'i':
		valp1->i = (int) dval1;
		break;
	case 'p':
		valp1->p = (unsigned int) dval1;
		break;
	case 'f':
		valp1->f = (float) dval1;
		break;
	case 'd':
		valp1->d = (double) dval1;
		break;
	default:
		err("unknown type %s", type1);
		break;
	}
}

#undef SEGYREAD_TAPE_ERROR
#undef SEGYREAD_TAPE_ERROR_TRACE
