/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SEGYWRITE: $Revision: 1.55 $ ; $Date: 2015/08/07 22:00:09 $    */

#ifdef SUXDR    /* begin if SUXDR */
#include "su_xdr.h"

#else		/* else if not SUXDR */


#include "su.h"
#include "segy.h"
#include "tapebhdr.h"

#endif		/* end if  SUXDR */

#include "tapesegy.h"
#include "bheader.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SEGYWRITE - write an SEG-Y tape					",
"									",
" segywrite <stdin tape=						",
"									",
" Required parameters:							",
"	tape=		tape device to use (see sudoc segyread)		",
"									",
" Optional parameter:							",
" verbose=0	silent operation				",
"		=1 ; echo every 'vblock' traces			",
" vblock=50	echo every 'vblock' traces under verbose option ",
" buff=1		for buffered device (9-track reel tape drive)	",
"		=0 possibly useful for 8mm EXABYTE drive	",
" conv=1		=0 don't convert to IBM format			",
" ebcdic=1	convert text header to ebcdic, =0 leave as ascii	",
" hfile=header	ebcdic card image header file			",
" bfile=binary	binary header file				",
" trmin=1 first trace to write					",
" trmax=INT_MAX  last trace to write			       ",
" endian=(autodetected)	=1 for big-endian and =0 for little-endian byte order",
" errmax=0	allowable number of consecutive tape IO errors	",
" format=		override value of format in binary header file	",
"									",
" Note: The header files may be created with  'segyhdrs'.		",
"									",
"									",
" Note: For buff=1 (default) tape is accessed with 'write', for buff=0	",
"	tape is accessed with fwrite. Try the default setting of buff=1 ",
"	for all tape types.						",
" Caveat: may be slow on an 8mm streaming (EXABYTE) tapedrive		",
" Warning: segyread or segywrite to 8mm tape is fragile. Allow time	",
"	   between successive reads and writes.				",
" Precaution: make sure tapedrive is set to read/write variable blocksize",
"	   tapefiles.							",
"									",
" For more information, type:	sudoc <segywrite>			",
"									",
NULL};

/*
 * Warning: may return the error message "efclose: fclose failed"
 *	 intermittently when segyreading/segywriting to 8mm EXABYTE tape,
 *	 even if actual segyread/segywrite is successful. However, this
 *	 may indicate that your tape drive has been set to a fixed block
 *	 size. Tape drives should be set to variable block size before reading
 *	 or writing tapes in the SEG-Y format.
 *
 * Credits:
 *	SEP: Einar Kjartansson
 *	CWP: Jack, Brian, Chris
 *	   : John Stockwell (added EXABYTE functionality)
 * Notes:
 *	Brian's subroutine, float_to_ibm, for converting IEEE floating
 *	point to IBM floating point is NOT portable and must be
 *	altered for non-IEEE machines.	See the subroutine notes below.
 *
 *	On machines where shorts are not 2 bytes and/or ints are not 
 *	4 bytes, routines to convert SEGY 16 bit and 32 bit integers 
 *	will be required.
 *
 *	The program, segyhdrs, can be used to make the ascii and binary
 *	files required by this code.
 */

/**************** end self doc ***********************************/

/* typedefs */
#ifdef SUXDR		/* begin if SUXDR */
#if defined(_CRAYMPP)	  /* begin if _CRAYMPP */
typedef short fourbyte;
#else			  /* else if SUXDR but not _CRAYMPP */
typedef int fourbyte;
#endif			  /* end if _CRAYMPP */
#endif			/* end if SUXDR */

/* subroutine prototypes */
#ifdef SUXDR		/* begin if  SUXDR */
static void float_to_ibm(fourbyte *from, fourbyte *to, int n, int endian);

#else			/* if not SUXDR */
static void float_to_ibm(int from[], int to[], int n, int endian);
static void bhed_to_tapebhed(const bhed *bhptr, tapebhed *tapebhptr); 
static void
	segy_to_tapesegy(const segy *trptr, tapesegy *tapetrptr, size_t nsegy); 

/*  globals */
tapesegy tapetr;
tapebhed tapebh;
#endif			/* end if SUXDR */

/* globals */
segy tr;
bhed bh;

int
main(int argc, char **argv)
{
	cwp_String tape;	/* name of raw tape device		*/
	cwp_String hfile;	/* name of ebcdic header file		*/
	cwp_String bfile;	/* name of binary header file		*/

#ifdef SUXDR	/* begin SUXDR */
	int j;			/* counter				*/
	FILE *headerfp;		/* file pointer to header file		*/
#else		/* else if not SUXDR */
	FILE *pipefp;		/* file pointer for popen read		*/
#endif		/* end if SUXDR */
	FILE *tapefp=NULL;	/* file pointer for tape		*/
	FILE *binaryfp;		/* file pointer for bfile		*/

	int tapefd=0;		/* file descriptor for tape buff=0	*/

	int i;			/* counter				*/
	int ns;			/* number of data samples		*/
	size_t nsegy;		/* size of whole trace in bytes		*/
	int itr;		/* current trace number			*/
	int trmax;		/* last trace to write			*/
	int trmin;		/* first trace to write			*/
	int verbose;		/* echo every ...			*/
	int vblock;		/* ... vblock traces with verbose=1	*/
	int buff;		/* buffered or unbuffered device	*/
	int endian;		/* =0 little endian; =1 big endian	*/
	int conv;		/* =1 IBM format =0 don't convert	*/
	int ebcdic=1;		/* =1 ebcdic =0 don't convert	*/
	int errmax;		/* max consecutive tape io errors	*/
	int errcount = 0;	/* counter for tape io errors		*/
	int format = 0;		/* tape format				*/
	cwp_Bool format_set = cwp_false; /* tape format			*/

#ifdef SUXDR	/* begin if SUXDR */
#if defined(CRAY) /* begin if defined CRAY */
#if defined(_CRAYMPP)  /* begin if defined _CRAYMPP */
	fourbyte imone = -1;	/* constant for Fortran linkage		*/
	fourbyte fns;		/* for Fortran CRAYMPP linkage		*/
#else		/* CRAY but not _CRAYMPP */	
	int ier;		/* CRAY ibmfloat error flag		*/
	fourbyte ione = -1;	/* constant for Fortran linkage		*/
#endif		/* end if _CRAYMPP */
#endif /* end if SUXDR and CRAY but not _CRAYMPP  */

	char ebcbuf[EBCBYTES+1];/* ebcdic data buffer			*/
	char bhbuf[BNYBYTES];	/* binary reel header buffer		*/
	char *trbuf;		/* output trace buffer			*/
	XDR bhed_xdr, bhbuf_xdr;/* for handling binary reel header	*/
	XDR trhd_xdr;
	unsigned int trstart;	/* "offset" of trhd stream buffer	*/

#else /* not Cray and not SUXDR */
	char cmdbuf[BUFSIZ];	/* dd command buffer			*/
	char ebcbuf[EBCBYTES];	/* ebcdic data buffer			*/
#endif		/* end if  SUXDR */
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters */
	MUSTGETPARSTRING("tape", &tape);
	if (!getparstring("hfile", &hfile))	hfile = "header";
	if (!getparstring("bfile", &bfile))	bfile = "binary";
	if (!getparint	 ("trmin", &trmin))	trmin = 1;
	if (!getparint	 ("trmax", &trmax))	trmax = INT_MAX;
	if (!getparint	 ("verbose", &verbose)) verbose = 0;
	if (!getparint	 ("vblock", &vblock))	vblock = 50;
	if (!getparint	 ("buff", &buff))	buff = 1;
	if (!getparint	 ("conv", &conv))	conv = 1;
	if (!getparint	 ("ebcdic", &ebcdic))	ebcdic = 1;
	if (!getparint("endian", &endian))      {
		union { short s; char c[2]; } testend;
			testend.s = 1;
			endian = (testend.c[0] == '\0') ? 1 : 0;
	}
	
	if (!getparint	 ("errmax", &errmax))	errmax = 0;
	if (getparint("format", &format))	format_set = cwp_true;
        checkpars();
	
	/* Check parameters */
	if (trmin < 1 || trmax < 1 || trmax < trmin)
		err("bad trmin/trmax values, trmin = %d, trmax = %d",
			trmin, trmax);

	/* Get first trace early to be sure that binary file is ready */
	gettr(&tr);

	/* Open files - first the tape */
	if (buff) tapefd = eopen(tape, O_WRONLY | O_CREAT | O_TRUNC, 0666);
	else tapefp = efopen(tape, "w");
	if (verbose) warn("tape opened successfully ");

#ifdef SUXDR	/* begin SUXDR */

	/* Open ascii header file */
	headerfp = efopen(hfile, "r");

	if (verbose) warn("header file opened successfully");

	/* - binary header file */
	binaryfp = efopen(bfile, "r");
	xdrstdio_create(&bhed_xdr,binaryfp,XDR_DECODE);
	xdrmem_create(&bhbuf_xdr,bhbuf,BNYBYTES,XDR_ENCODE);

	if (verbose) warn("binary file opened successfully");

	/* Read ascii header into buffer and blank newlines & nulls */
	memset(&(ebcbuf[0]),0,EBCBYTES);
	if (ebcdic==1) {
		for(i = 0; i<EBCBYTES; i += 80) {
			fgets(&(ebcbuf[i]),81, headerfp);
			j = (int) strlen(&(ebcbuf[i]));
			ebcbuf[i+j] = ' ';
			j--;
	
		}

		/* Convert to EBCDIC */
		if (ebcdic==1) zebc(&(ebcbuf[0]),&(ebcbuf[0]),EBCBYTES);
	} else {
		fread(ebcbuf,1,EBCBYTES,headerfp);
	}
		

	efclose(headerfp);
	if (verbose) warn("header file closed successfully");

	/* Write ebcdic stream to tape */
	if (buff) {
		if (EBCBYTES != write(tapefd, ebcbuf, EBCBYTES)) {
			if (verbose)
				warn("tape write error on ebcdic header");
			if (++errcount > errmax)
				err("exceeded maximum io errors");
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	} else {
		 fwrite(ebcbuf, 1, EBCBYTES, tapefp);
		 if (ferror(tapefp)) {
			if (verbose)
				warn("tape write error on ebcdic header");
			if (++errcount > errmax)
				err("exceeded maximum io errors");
			clearerr(tapefp);
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	}

	/* Read binary file into bh structure */
	xdrbhdrsub(&bhed_xdr, &bh);

	/* update requisite field */
	if (format_set) bh.format = format;  /* manually set format */
	bh.ntrpr  = 1;  /* one trace per record */

#else  /* not SUXDR */

	/* - binary header file */
	binaryfp = efopen(bfile, "r");
	if (verbose) warn("binary file opened successfully");

	/* Open pipe to use dd to convert ascii to ebcdic */
	if (ebcdic==1) {
		sprintf(cmdbuf, "dd if=%s conv=ebcdic cbs=80 obs=3200", hfile);
	} else {
		sprintf(cmdbuf, "dd if=%s cbs=80 obs=3200", hfile);
	}
	pipefp = epopen(cmdbuf, "r");

	/* Read ebcdic stream from pipe into buffer */
	efread(ebcbuf, 1, EBCBYTES, pipefp);

	/* Write ebcdic stream to tape */
	if (buff) {
		if (EBCBYTES != write(tapefd, ebcbuf, EBCBYTES)) {
			if (verbose)
				warn("tape write error on ebcdic header");
			if (++errcount > errmax)
				err("exceeded maximum io errors");
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	} else {
		 fwrite(ebcbuf, 1, EBCBYTES, tapefp);
		 if (ferror(tapefp)) {
			if (verbose)
				warn("tape write error on ebcdic header");
			if (++errcount > errmax)
				err("exceeded maximum io errors");
			clearerr(tapefp);
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	}

	/* Read binary file into bh structure */
	efread((char *) &bh, 1, BNYBYTES, binaryfp);
	if (format_set) bh.format = format;  /* manually set format  */

	if (bh.ntrpr == 0) bh.ntrpr  = 1;	/* one trace per record */

#endif		/* end if SUXDR */

	/* Compute trace size (can't use HDRBYTES here!) */
	ns = bh.hns;
	if (!ns) err("bh.hns not set in binary header");
	nsegy = ns*4 + SEGY_HDRBYTES;

#ifdef SUXDR 	/* begin SUXDR */
	/* Convert from ints to shorts */
	xdrbhdrsub(&bhbuf_xdr, &bh);

	/* Write binary structure to tape */
	if (buff) {
		if (BNYBYTES != write(tapefd, bhbuf, BNYBYTES)) {
			if (verbose)
				warn("tape write error on binary header");
			if (++errcount > errmax)
				err("exceeded maximum io errors");
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	} else {
		 fwrite(bhbuf, 1, BNYBYTES, tapefp);
		 if (ferror(tapefp)) {
			if (verbose)
				warn("tape write error on binary header");
			if (++errcount > errmax)
				err("exceeded maximum io errors");
			clearerr(tapefp);
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	}

#else /* not SUXDR */

	/* if little endian (endian=0) swap bytes of binary header */
	if (endian==0) for (i = 0; i < BHED_NKEYS; ++i) swapbhval(&bh,i);


	/* Convert from ints/shorts to bytes */
	bhed_to_tapebhed(&bh, &tapebh);


	/* Write binary structure to tape */
	if (buff) {
		if (BNYBYTES != write(tapefd, (char *) &tapebh, BNYBYTES)) {
			if (verbose)
				warn("tape write error on binary header");
			if (++errcount > errmax)
				err("exceeded maximum io errors");
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	} else {
		 fwrite((char *) &tapebh, 1, BNYBYTES, tapefp);
		 if (ferror(tapefp)) {
			if (verbose)
				warn("tape write error on binary header");
			if (++errcount > errmax)
				err("exceeded maximum io errors");
			clearerr(tapefp);
		} else { /* Reset counter on successful tape IO */
			errcount = 0;
		}
	}
#endif		/* end if SUXDR */


	/* Copy traces from stdin to tape */

#ifdef SUXDR	/* begin SUXDR */

	trbuf = (char *) alloc1(nsegy, sizeof(char));
	xdrmem_create(&trhd_xdr,trbuf,(unsigned int) nsegy,XDR_ENCODE);
	trstart = xdr_getpos(&trhd_xdr);

#endif 		/* end if SUXDR */

	itr = 0;
	do {

		/* Set/check trace header words */
		tr.tracr = ++itr;
		if (tr.ns != ns)
			warn("conflict: tr.ns = %d, bh.ns = %d: trace %d",
					tr.ns, ns, itr);

		/* Convert and write desired traces */
		if (itr >= trmin) {

#ifdef SUXDR 	/* begin SUXDR */
	 		/* convert trace header to SEGY standard */       
			if(FALSE == xdr_setpos(&trhd_xdr,trstart)) 
			    err("%s: trouble \"seeking\" start of trace",
				__FILE__);
			xdrhdrsub(&trhd_xdr,&tr);

			/* Convert internal floats to IBM floats */
			if (conv) {
#if defined(CRAY)
#if defined(_CRAYMPP)
			    float_to_ibm((fourbyte *) (&(tr.data[0])),
					 (fourbyte *) (&(tr.data[0])),
					 ns, endian);
/* Stew's Fortran routine...
			    fns = ns;
			    IBMFLT(tr.data,tr.data,&fns,&imone);
*/
#else /* !_CRAYMPP */
			    USSCTI(tr.data,tr.data,&ione,&ns,&ier);
#endif /* _CRAYMPP */
#else /* !CRAY */
			    float_to_ibm((fourbyte *) (&(tr.data[0])),
					 (fourbyte *) (&(tr.data[0])),
					 ns, endian);
#endif /* !CRAY */
			    memcpy(trbuf+SEGY_HDRBYTES,(char *) tr.data,
				ns*4*sizeof(char));
			} else {
			    xdr_vector(&trhd_xdr,(char *) tr.data,
				ns, sizeof(float), (xdrproc_t) xdr_float);
			}

			/* Write the trace to tape */
			if (buff) {
			    if (nsegy !=
			       write(tapefd, trbuf, nsegy)){
				if (verbose)
				    warn("tape write error on trace %d", itr);
				if (++errcount > errmax)
				    err("exceeded maximum io errors");
			    } else { /* Reset counter on successful tape IO */
				errcount = 0;
			    }
			} else {
			    fwrite(trbuf,sizeof(char),nsegy,tapefp);
			    if (ferror(tapefp)) {
				if (verbose)
				    warn("tape write error on trace %d", itr);
				if (++errcount > errmax)
				    err("exceeded maximum io errors");
				    clearerr(tapefp);
			    } else { /* Reset counter on successful tape IO */
				errcount = 0;
			    }
			}

#else /* not SUXDR */
		
			/* Convert internal floats to IBM floats */
			if (conv)
				float_to_ibm((int *) tr.data, (int *) tr.data,
								ns, endian);

		       /* handle no ibm conversion for little endian case */
		       if (conv==0 && endian==0)
				for (i = 0; i < ns ; ++i)
					swap_float_4(&tr.data[i]);
			
			/* if little endian, swap bytes in header */
			if (endian==0)
			    for (i = 0; i < SEGY_NKEYS; ++i) swaphval(&tr,i);

			/* Convert from ints/shorts to bytes */
			segy_to_tapesegy(&tr, &tapetr, nsegy);

			/* Write the trace to tape */
			if (buff) {
			    if (nsegy !=
			       write(tapefd, (char *) &tapetr, nsegy)){
				if (verbose)
				    warn("tape write error on trace %d", itr);
				if (++errcount > errmax)
				    err("exceeded maximum io errors");
			    } else { /* Reset counter on successful tape IO */
				errcount = 0;
			    }
			} else {
			    fwrite((char *)&tapetr,1,nsegy,tapefp);
			    if (ferror(tapefp)) {
				if (verbose)
				    warn("tape write error on trace %d", itr);
				if (++errcount > errmax)
				    err("exceeded maximum io errors");
				    clearerr(tapefp);
			    } else { /* Reset counter on successful tape IO */
				errcount = 0;
			    }
			}

#endif		/* end if SUXDR */

			/* Echo under verbose option */
			if (verbose && itr % vblock == 0)
				warn(" %d traces written to tape", itr);
		}
	} while (gettr(&tr) && itr < trmax);


	/* Clean up */
	(buff) ?  eclose(tapefd) :
		  efclose(tapefp);
	if (verbose) warn("tape closed successfully");

#ifdef SUXDR	/* begin SUXDR */
	xdr_destroy(&trhd_xdr);
	xdr_destroy(&bhed_xdr);
	xdr_destroy(&bhbuf_xdr);
#endif 		/* end if SUXDR */

	efclose(binaryfp);
	if (verbose) warn("binary file closed successfully");

#ifndef SUXDR	/* begin not SUXDR */
	epclose(pipefp);
#endif		/* end if not SUXDR */

	return(CWP_Exit());
}

#ifdef SUXDR	/* begin SUXDR */

/* Assumes fourbyte == 4 byte integer */
static void float_to_ibm(fourbyte *from, fourbyte *to, int n, int endian)
/**********************************************************************
 float_to_ibm - convert between 32 bit IBM and IEEE floating numbers
*********************************************************************** 
Input:
from       input vector
n	  number of floats in vectors
endian     =0 for little endian machine, =1 for big endian machines

Output:
to	 output vector, can be same as input vector

*********************************************************************** 
Notes:
Up to 3 bits lost on IEEE -> IBM

IBM -> IEEE may overflow or underflow, taken care of by 
substituting large number or zero

Only integer shifting and masking are used.
*********************************************************************** 
Credits:     CWP: Brian Sumner
***********************************************************************/
{
    register fourbyte fconv, fmant, t;
    register int i;

    for (i=0;i<n;++i) {
	fconv = from[i];
	if (fconv) {
	    fmant = (0x007fffff & fconv) | 0x00800000;
	    t = (fourbyte) ((0x7f800000 & fconv) >> 23) - 126;
	    while (t & 0x3) { ++t; fmant >>= 1; }
	    fconv = (0x80000000 & fconv) | (((t>>2) + 64) << 24) | fmant;
	}
	if(endian==0)
		fconv = (fconv<<24) | ((fconv>>24)&0xff) |
			((fconv&0xff00)<<8) | ((fconv&0xff0000)>>8);

	to[i] = fconv;
    }
    return;
}

#else	/* not SUXDR */

#ifdef _HPUX_SOURCE
void float_to_ibm(int from[], int to[], int n, int endian)
{
    register int fconv, fmant, i, t, dummy;

	dummy = endian;

    for (i=0;i<n;++i) {
	fconv = from[i];
	if (fconv) {
	    fmant = (0x007fffff & fconv) | 0x00800000;
	    t = (int) ((0x7f800000 & fconv) >> 23) - 126;
	    while (t & 0x3) { ++t; fmant >>= 1; }
	    fconv = (0x80000000 & fconv) | (((t>>2) + 64) << 24) | fmant;
	}
	to[i] = fconv;
    }
    return;
}

#else

/* Assumes sizeof(int) == 4 */
static void float_to_ibm(int from[], int to[], int n, int endian)
/**********************************************************************
 float_to_ibm - convert between 32 bit IBM and IEEE floating numbers
*********************************************************************** 
Input:
from	   input vector
n	   number of floats in vectors
endian	   =0 for little endian machine, =1 for big endian machines

Output:
to	   output vector, can be same as input vector

*********************************************************************** 
Notes:
Up to 3 bits lost on IEEE -> IBM

IBM -> IEEE may overflow or underflow, taken care of by 
substituting large number or zero

Only integer shifting and masking are used.
*********************************************************************** 
Credits:     CWP: Brian Sumner
***********************************************************************/
{
    register int fconv, fmant, i, t;

    for (i=0;i<n;++i) {
	fconv = from[i];
	if (fconv) {
	    fmant = (0x007fffff & fconv) | 0x00800000;
	    t = (int) ((0x7f800000 & fconv) >> 23) - 126;
	    while (t & 0x3) { ++t; fmant >>= 1; }
	    fconv = (0x80000000 & fconv) | (((t>>2) + 64) << 24) | fmant;
	}
	if(endian==0)
		fconv = (fconv<<24) | ((fconv>>24)&0xff) |
			((fconv&0xff00)<<8) | ((fconv&0xff0000)>>8);

	to[i] = fconv;
    }
    return;
}

#endif 

static void bhed_to_tapebhed(const bhed *bhptr, tapebhed *tapebhptr)
/***************************************************************************
bhed_tape_bhed -- converts the binary tape header in the machine's short
and int types to, respectively, the seg-y standard 2 byte and 4 byte integer
types.
****************************************************************************
Input:
bhptr		pointer to binary header vector

Output:
tapebhptr	pointer to tape binary header vector
****************************************************************************
Notes:
The present implementation assumes that these types are actually the "right"
size (respectively 2 and 4 bytes), so this routine is only a placeholder for
the conversions that would be needed on a machine not using this convention.
****************************************************************************
Author: CWP: Jack K. Cohen  August 1994
***************************************************************************/
{
	register int i;
	Value val;
	
	/* convert the binary header field by field */
	for (i = 0; i < BHED_NKEYS; ++i) {
		getbhval(bhptr, i, &val);
		puttapebhval(tapebhptr, i, &val);
	}
}

static
void segy_to_tapesegy(const segy *trptr, tapesegy *tapetrptr, size_t nsegy)
/***************************************************************************
tapesegy_to_segy -- converts the integer header fields from, respectively,
		    the machine's short and int types to the  seg-y standard
		    2 byte and 4 byte types.
****************************************************************************
Input:
trptr		pointer to SU SEG-Y data vector		
nsegy		whole size of a SEG-Y trace in bytes

Output:
tapetrptr	pointer to tape SEG-Y data vector
****************************************************************************
Notes:
Also copies the float data byte by byte.  The present implementation assumes
that the integer types are actually the "right" size (respectively 2 and
4 bytes), so this routine is only a placeholder for the conversions that
would be needed on a machine not using this convention.	 The float data
is preserved as four byte fields and is later converted to internal floats
by float_to_ibm (which, in turn, makes additonal assumptions)
****************************************************************************
Author: CWP: Jack K. Cohen  August 1994
***************************************************************************/
{
	register int i;
	Value val;
	
	/* convert trace header, field by field */
	for (i = 0; i < SEGY_NKEYS; ++i) {
		gethval(trptr, i, &val);
		puttapehval(tapetrptr, i, &val);
	}
	
	/* copy optional portion */
	memcpy(tapetrptr->unass, (char *)&(trptr->otrav)+2, 60);

	/* copy data portion */
	memcpy(tapetrptr->data, trptr->data, 4*SU_NFLTS);

} 
#endif		/* end if SUXDR */
