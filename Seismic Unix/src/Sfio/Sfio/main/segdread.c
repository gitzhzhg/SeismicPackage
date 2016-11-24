/* Copyright (c) Colorado School of Mines, 2007.*/
/* All rights reserved.                       */

/* SEGDREAD: $Revision: 1.9 $ ; $Date: 2004/02/02 18:34:30 $	*/

#include "su.h"
#include "segy.h"
#include "segd.h"

#include "sfio.h"
#include "header.h"	/* added for HDRBYTES in puttr workaround */

/* Portable decoding utilities to retrieve char/u_char/short/u_short from SEG-D byte stream */
static char GET_C(Sfio_t *f)
{
 int n;
 char c;

 n = sfgetc(f);
 c = (char) ((n > 127) ? (n-256) : n);

 return (c);
}

static unsigned int GET_UC(Sfio_t *f)
{
 int n;
 unsigned char uc;

 n = sfgetc(f);
 uc = (unsigned char) n;

 return (uc);
}

static short GET_S(Sfio_t *f)
{
 int n1, n2;
 int n;
 short s;

 n1 = sfgetc(f);
 n2 = sfgetc(f);
 n = (n1<<8) | n2;
 s = (short) ((n > 32767)? n - 65536 : n);

 return (s);
}

static unsigned short GET_US(Sfio_t *f)
{
 int n1, n2;
 int n;
 unsigned short us;

 n1 = sfgetc(f);
 n2 = sfgetc(f);
 n = (n1<<8) | n2;
 us = (unsigned short) n;

 return (us);
}

#define REC_L (1*20/*demux header*/ + 255*32 /*up to 255 header extensions*/ + 4*SU_NFLTS/*4 bytes per sample max*/) /* expected max record size: 20 + 10*(SU_NFLTS)/4 bytes */

#define BCD_FF (15+10*(15))                   /* bcd interpretation of FF */
#define BCD_FFF (15+10*(15+10*(15)))          /* bcd interpretation of FFF */
#define BCD_FFFF (15+10*(15+10*(15+10*(15)))) /* bcd interpretation of FFFF */

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                      ",
" SEGDREAD - read an SEG-D tape                                        ",
"                                                                      ",
" segdread > stdout tape=                                              ",
"                                                                      ",
"                                                                      ",
" Required parameters:                                                 ",
"       tape=           input tape device                              ",
" 			tape=- to read from stdin                      ",
"                                                                      ",
" Optional parameters:                                                 ",
"       use_stdio=0     for record devices (9-track reel tape drive)   ",
"                       =1 for pipe, disk and fixed-block 8mm drives   ",
"       verbose=0       silent operation                               ",
"                       = 1 ; echo every 'vblock' traces               ",
"                       = 2 ; echo information about blocks            ",
"       vblock=50       echo every 'vblock' traces under verbose option",
"       ptmin=1         first shot to read                             ",
"       ptmax=INT_MAX   last shot to read                              ",
"       gain=0          no application of gain                         ",
"       aux=0           no recovery of auxiliary traces                ",
"       errmax=0        allowable number of consecutive tape IO errors ",
"	ns=0		override of computed ns to work around SEG-D   ",
"			flaws.  Ignored when use_stdio=0.              ",
"	pivot_year=30   Use current century for 2 digit yrs less than  ",
"	                pivot_year, previous century otherwise.        ",
"                                                                      ",
"         type:   sudoc segdread   for further information             ",
NULL};

/* Credits:
 *  for version 1:
 *    IPRA, Pau, France: Dominique Rousset, rousset@iprrs1.univ-pau.fr
 *  for version 2.2:
 *    EOST, Strasbourg, France: Celine Girard
 *  for versions 2.3:
 *    EOST, Strasbourg, France: Marc Schaming, mschaming@eost.u-strasbg.fr
 *  for version 2.4:
 *    SEP, Stanford University: Stew Levin, stew@sep.stanford.edu
 *    a) Changed definitions of BCD_FF/BCD_FFFF
 *    b) Corrected decoding of general_header_1 in info_gh1.
 *    c) Changed buff=0 to use_stdio=1 to avoid confusion (stdio
 *       IS buffered I/O). Kept old buff= internally for backwards
 *       compatibility.
 *    d) Changed F8015 decoding of negative mantissas to avoid
 *       1 part in 2^14th decoding error on 2's complement platforms.
 *    e) Adapted F8015 to F0015 decoding routine. Unused, but now available.
 *    f) Use AT&T sfio package for tape read.
 *    g) Handle endian and wordsize dependencies portably (I think).
 *    g) Allow tape=- command line argument to accept input from stdin.
 *    h) Compute trace length explicitly from headers so that disk data
 *       input can work.
 *    i) Correct tape trace length calculation to account for demux
 *       trace header extensions.
 *    j) Fix a couple of typos in comments and selfdoc
 *    k) Added F8022 for 1 byte quaternary exponent demux.
 *    l) Added F8024 for 2 byte quaternary exponent demux.
 *    m) Added F8042 for 1 byte hexadecimal exponent demux.
 *    n) Added F8044 for 2 byte hexadecimal exponent demux.
 *    o) Added F8048 for 4 byte hexadecimal exponent demux.
 *    p) Added F8036 for 2 byte 2's complement integer demux.
 *    q) Added F8038 for 4 byte 2's complement integer demux.
 *    r) Added F8058 for 4 byte IEEE float demux.
 *    s) Added ns= parameter to work around bad SEG-D trace
 *       length specifications in headers.
 *  for version 2.5:
 *    SEP, Stanford University: Stew Levin, stew@sep.stanford.edu
 *    a) Added pivot_year to disambiguate decoding of 2-digit yrs
 *    b) Modified decode of 2-byte BCD to avoid endian problems
 *    c) Modified debug printout to fix endian BCD display problems
 *    d) Don't let dem_trace_header override ns specified on command line
 *    e) Removed extra factor of two in decoding of general_header_1.r
 *    f) Removed conditional disabling of sfio
 *
 *--------------------------------------------------------------------
 * SEGDREAD: Version 2.1, 10/10/94
 *           Version 2.2, 17/08/95
 *           Version 2.3, 04/1997 Thu Apr 10 11:55:45 DFT 1997
 *           Version 2.4, 10/03/98 Tue Mar 10 1998
 *           Version 2.5, Feb 4, 2001
 *--------------------------------------------------------------------
 */
/**************** end self doc ***********************************/

/* subroutine prototypes */
static int bcd (unsigned char *ptr, int begin, int n) ;
#if 0
/* not used */
static void F0015_to_float (Sfio_t *from, float to[], int len);
#endif
static void F8015_to_float (Sfio_t *from, float to[], int len);
static void F8022_to_float (Sfio_t *from, float to[], int len);
static void F8024_to_float (Sfio_t *from, float to[], int len);
static void F8036_to_float (Sfio_t *from, float to[], int len);
static void F8038_to_float (Sfio_t *from, float to[], int len);
static void F8042_to_float (Sfio_t *from, float to[], int len);
static void F8044_to_float (Sfio_t *from, float to[], int len);
static void F8048_to_float (Sfio_t *from, float to[], int len);
static void F8058_to_float (Sfio_t *from, float to[], int len);

int get_gh1(general_header_1 * gh1, Sfio_t *tapeun) ;
int get_gh2(general_header_2 * gh2, Sfio_t *tapeun) ;
int get_ghn(general_header_n * ghn, Sfio_t *tapeun) ;
int get_gn_sn358(gen_head_sn358 * gh358, Sfio_t *tapeun) ;
int get_csh(channel_set_header * csh, Sfio_t *tapeun) ;
int get_ssh(sample_skew * ssh, Sfio_t *tapeun) ;
int get_ech(extended_header * ech, Sfio_t *tapeun) ;
int get_exh(external_header * exh, Sfio_t *tapeun) ;
int get_gt(general_trailer * gt, Sfio_t *tapeun) ;
int get_dth(dem_trace_header * dth, Sfio_t *tapeun) ;
int get_the(trace_header_ext * the, Sfio_t *tapeun) ;

void info_gh1(general_header_1 * gh1) ;
void info_gh2(general_header_2 * gh2) ;
void info_ghn(general_header_n * ghn) ;
void info_gn_sn358(gen_head_sn358 * gh358);
void info_csh(channel_set_header * csh) ;
void info_ssh(sample_skew * ssh) ;
void info_ech(extended_header * ech);
void info_exh(external_header * exh);
void info_gt(general_trailer * gt);
void info_dth(dem_trace_header * dth);

segy tr;

int
main(int argc, char **argv)
{
 general_header_1   segd_general_header_1;
 general_header_2   segd_general_header_2;
 general_header_n   segd_general_header_n;
 gen_head_sn358     segd_gen_head_sn358;
 channel_set_header *segd_channel_set_header;
 sample_skew        segd_sample_skew;
 extended_header    segd_extended_header;
 external_header    segd_external_header;
 general_trailer    segd_general_trailer;
 dem_trace_header   segd_dem_trace_header;
 trace_header_ext   segd_trace_header_ext;

 channel_set_header **csd = NULL;
			/* array[n_str][n_cs] of channel_set_header */

 char *tape=NULL;        /* name of raw tape device */
 int tapefd=0;           /* file descriptor for tape */
/* segdread.c:213: warning: `tapeun' might be used uninitialized in this function */
 Sfio_t  *tapeun = NULL;		/* input for Sfio_t reads. May be memory or stdio */
 Sfoff_t startpos;		/* for rewind on memory Sfio_t */

 short scan_type;        /* scan type number */
 short chan_set;         /* channel set number */

 register int i, j;
 register int i_scan, i_cs, i_tr;
 int i_ss;
/* segdread.c:222: warning: `nread' might be used uninitialized in this function */
 int nread = 0;               /* bytes read */
 int ns;                                  /* number of data samples */
 int n_gh;         /* number of additional blocks in general header */
 int n_str;                      /* number of scan types per record */
 int n_cs;                  /* number of channel sets per scan type */
 int n_sk;               /* number of 32 byte field for sample skew */
 int n_ec;                                /* extended header length */
 int n_ex;                                /* external header length */
 int n_gt=0;                 /* number of blocks of general trailer */
 int n_chan;                            /* total number of channels */
 int itr;                                   /* current trace number */
 int ipt;                                    /* current shot number */
 int ptmin;                                  /* first trace to read */
 int ptmax;                                   /* last trace to read */
 int verbose;                   /* echo every ...                   */
 int vblock;                    /* ... vblock traces with verbose=1 */
 int buff;                   /* flag for buffered/unbuffered device */
 int gain;                                /* flag for applying gain */
 int aux;                      /* flag for keeping auxiliary traces */
 int errmax;                      /* max consecutive tape io errors */
 int pivot_year;		  /* for choosing correct century   */
 int errcount = 0;                    /* counter for tape io errors */
 int hdr1_i, hdr1_r;                   /* i and r decoded from hdr1 */
 int ns_override;				/* for trace length fudging */
 unsigned int nsamp_hdr1;                   /* number of samples/trace using */
                            /* only general header 1 i and r fields */
 unsigned int nsamp_hdr2;            /* number of samples/trace from general */
                      /* header 2 erl and general header 1 i fields */
 unsigned int nsamp_hdr358;   /* Sercel header rec_length plus hdr1_i fields */
 unsigned int nsamp_cs;      /* nsamp from channel set named in demux header */
 unsigned int nsamp_the;         /* nsamp from demux trace header extensions */

 float  mp;                                   /* descaling exponent */
 float **mmp = NULL;    /* array[n_cs][n_str] for escaling exponent */

 char *bloc1;                              /* pointer on data block */

 unsigned databytes = 0;	/* added for puttr workaround */


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

 /* Set parameters */
 if (!getparint("ptmin", &ptmin))        ptmin = 1;
 if (!getparint("ptmax", &ptmax))        ptmax = INT_MAX;
 if (!getparint("verbose", &verbose))    verbose = 0;
 if (verbose==2) {ptmax=ptmin; warn("ptmax set to ptmin for verbose=2");}
 if (!getparint("vblock", &vblock))      vblock = 50;
 if (verbose==2) vblock = 1;
 if (!getparint("use_stdio",&buff)) {
	if (!getparint("buff", &buff))          buff = 1;
 } else {
	buff = !buff;
 }
 if (!getparint("gain", &gain))          gain = 0;
 if (!getparint("aux", &aux))            aux = 0;
 if (!getparint("errmax", &errmax))      errmax = 0;
 if (!getparint("pivot_year", &pivot_year))      pivot_year = 30;
 if (!getparint("ns", &ns_override))      	 ns_override = 0;

 /* Allocate space for the record block */
 if ((bloc1 = alloc1(REC_L, sizeof(char))) == NULL) err("error at bloc1 allocation");
 if ((unsigned long) bloc1 % 2)
	warn("there may be a problem since bloc1 is not on a short boundary (%ul)",(unsigned long) bloc1);

 /* Open the tape */
 if ( STREQ(tape,"-") ) {
    if (buff) tapefd = fileno(stdin);
    else tapeun = sfstdin;
 } else {
	 if (buff) tapefd = eopen(tape, O_RDONLY, 0444);
	 else      {
			tapeun = sfopen((Sfio_t *) 0, tape, "rb");
			if (tapeun == ((Sfio_t *) 0)) err("Unable to open tape %s\n");
		     }
 }
 if (verbose) warn("tape opened successfully");

 /* Create Sfio_t input stream */
 if(buff)
	tapeun = sfnew((Sfio_t *) 0, (Void_t *) bloc1, (size_t) REC_L, 0, SF_STRING|SF_READ);
 startpos = sftell(tapeun);

 /* Read the traces */
 ipt = 0;    /*current shot number */
 itr = 0;    /*current trace number */
 while (ipt < ptmax) {

 	/************************
 	* Read the Header Block *
 	************************/

 	if (buff) {
 		if (-1 == (nread = (int) read(tapefd, (void *) bloc1, (size_t) REC_L))){
 			if (verbose)
 				warn("tape read error on header block from shot %d", (ipt+1));
 			if (++errcount > errmax)
 				err("exceeded maximum io errors");
 		} else { /* Reset counter on successful tape IO */
 			errcount = 0;
 		}
	(void) sfseek(tapeun,startpos,SEEK_SET);  /* reset Sfio_t pointer to start of block */
 	if (!nread) break; /* middle exit loop instead of mile-long while */
	}

 	/* General Header #1 */

	if ( EXIT_FAILURE == get_gh1(&segd_general_header_1, tapeun) ) break;

      ns = 0;
      nsamp_hdr1 = 0; nsamp_hdr2 = 0; nsamp_hdr358 = 0;
	hdr1_r = bcd(&segd_general_header_1.z_r1,1,3);
        hdr1_i = segd_general_header_1.i;
        if(hdr1_r != BCD_FFF && hdr1_i != 0) {
              hdr1_r *= 2;  /* range is 10 to 1990 */
              nsamp_hdr1 = (hdr1_r*512*16)/(10*hdr1_i)  + 1;
                         /* 20*r*512 msec /10*(i/16) msec */
	      if(ns == 0) ns = nsamp_hdr1;
          if(verbose) warn("nsamp_hdr1=%d\n",nsamp_hdr1);
	}


 	tr.fldr =   bcd ((unsigned char *) &(segd_general_header_1.f[0]), 0, 4);
 	tr.year =   bcd ((unsigned char *) &segd_general_header_1.yr, 0, 2);
        if(tr.year < pivot_year) tr.year += 2000;
        else tr.year += 1900;
 	n_gh =      bcd ((unsigned char *) &segd_general_header_1.gh_dy1, 0, 1);
 	tr.day =    bcd ((unsigned char *) &segd_general_header_1.gh_dy1, 1, 3);
 	tr.hour =   bcd ((unsigned char *) &segd_general_header_1.h, 0, 2);
 	tr.minute = bcd ((unsigned char *) &segd_general_header_1.mi, 0, 2);
 	tr.sec =    bcd ((unsigned char *) &segd_general_header_1.se, 0, 2);
 	tr.dt =     (segd_general_header_1.i*1000) >> 4;
 	n_str =     bcd ((unsigned char *) &segd_general_header_1.str, 0, 2);
 	n_cs =      bcd ((unsigned char *) &segd_general_header_1.cs, 0, 2);
 	n_sk =      bcd ((unsigned char *) &segd_general_header_1.sk, 0, 2);
 	n_ec =      bcd ((unsigned char *) &segd_general_header_1.ec, 0, 2);
 	n_ex =      bcd ((unsigned char *) &segd_general_header_1.ex, 0, 2);

 	if (verbose==2) info_gh1(&segd_general_header_1);

 	/* Additional general headers */

 	if((n_gh == 2) && (segd_general_header_1.m[0] == 0x13)) { /* Special case for Sercel SN358 */
        if( EXIT_FAILURE == get_gn_sn358(&segd_gen_head_sn358, tapeun) ) break;

 		if (verbose==2) info_gn_sn358(&segd_gen_head_sn358);
	        if(hdr1_i != 0) {
			nsamp_hdr358 = bcd(&(segd_gen_head_sn358.rec_length[0]),1,3)*100 /* 10ths sec to msec */ * 16 /* base scan per msec */ /hdr1_i + 1;
          if(verbose) warn("nsamp_hdr358=%d\n",nsamp_hdr358);
			if (ns != 0 && nsamp_hdr358 != 0 && ns != nsamp_hdr358)
				warn("General Header 1 nsamp %u not equal to Sercel Header nsamp %u\n",nsamp_hdr1, nsamp_hdr358);
			if (nsamp_hdr358 != 0) ns = nsamp_hdr358;
		}
 	}
 	else {
 		for (i = 0; i < n_gh; i++) {
 			if (i == 0) {

 				/* General header #2 */

				if ( EXIT_FAILURE == get_gh2(&segd_general_header_2, tapeun) ) break;
				if ((segd_general_header_2.rev[0] <= 1) && (segd_general_header_2.rev[0] != 0)) { /* looks like SEGD rev 1 */
 				if (tr.fldr == BCD_FFFF) tr.fldr = 65536 * segd_general_header_2.ef[0] +
					256 * segd_general_header_2.ef[1] + segd_general_header_2.ef[2];
 				if (n_cs == BCD_FF) n_cs = 256 * segd_general_header_2.en[0] + segd_general_header_2.en[1];
 				if (n_ec == BCD_FF) n_ec = 256 * segd_general_header_2.ecx[0] + segd_general_header_2.ecx[1];
 				if (n_ex == BCD_FF) n_ex = 256 * segd_general_header_2.eh[0] + segd_general_header_2.eh[1];
 				n_gt = segd_general_header_2.gt;
 				if (verbose==2) info_gh2(&segd_general_header_2);
			 if (hdr1_r == BCD_FFF && hdr1_i != 0)
				nsamp_hdr2 = segd_general_header_2.erl[2] +
			               256 * (segd_general_header_2.erl[1] +
				       256 * (segd_general_header_2.erl[0])) *
				       16 / hdr1_i + 1;
			 else nsamp_hdr2 = 0;
          if(verbose) warn("nsamp_hdr2=%d\n",nsamp_hdr2);
			 if(nsamp_hdr2 != 0 && ns != 0 && ns != nsamp_hdr2)
				warn("General Header 2 nsamp %u differs from previous General Header(s) nsamp %u\n", nsamp_hdr2, ns);
			 if(nsamp_hdr2 != 0) ns = nsamp_hdr2;
			} else {
 				if (verbose==2) info_gh2(&segd_general_header_2);
			}
 			}
 			else {

 				/* General header #n */

				if ( EXIT_FAILURE == get_ghn(&segd_general_header_n, tapeun) ) break;
 				if (verbose==2) info_ghn(&segd_general_header_n);
 			}
 	 	}
 	}

 	/* Verify the length of the first record */
        if(!buff) nread = sftell(tapeun) - startpos;
 	if (buff && nread != ((1 + n_gh + n_str * (n_cs + n_sk) + n_ec + n_ex) * 32))
 		err("Error with length of first record\n"
 			"\t... first record = %d bytes differs from ((1 + n_gh + n_str * (n_cs + n_sk) + n_ec + n_ex) * 32)\n"
 			"\t    with n_gh=%d, n_str=%d, n_cs=%d, n_sk=%d, n_ec=%d, n_ex=%d\n",
 			nread, n_gh, n_str, n_cs, n_sk, n_ec, n_ex);

 	/* Allocate space for array csd */
	if (csd == NULL)
 		if ((csd = (channel_set_header **) alloc2 ((size_t) n_cs, (size_t) n_str, (size_t) sizeof(channel_set_header))) == NULL)
 			err("error at csd allocation");

 	/* if gain allocate space for mmp array */
 	if (gain && (mmp == NULL))
 		if ((mmp = (float **) alloc2float ((size_t) n_cs, (size_t) n_str)) == NULL)
 			err("error at mmp allocation");

 	/* For each scan type */

 	n_chan = 0;
 	for (i_scan = 0; i_scan < n_str; i_scan ++) {

 		/* For each channel set */

 		for (i_cs = 0; i_cs < n_cs; i_cs ++) {
			segd_channel_set_header = &csd[i_scan][i_cs];
			if ( EXIT_FAILURE == get_csh(segd_channel_set_header, tapeun) ) break;
 			n_chan += bcd((unsigned char*) &(segd_channel_set_header->cs), 0, 4);
 			if (gain) {
 				mp = ((float) ((segd_channel_set_header->mp[1] & 0x7f) << 8 | segd_channel_set_header->mp[0])) / 1024.;
 				if (segd_channel_set_header->mp[1] >> 7) mp *= -1.;
 				mmp[i_scan][i_cs] = pow ((double) 2., (double) mp);

 				/* For the seismic traces */
 				if (verbose && segd_channel_set_header->c == 0x10)
 					warn("Multiplier value for channel set %d of scan type %d is : %7.3e", i_cs, i_scan, mmp[i_scan][i_cs]);
 			}
 			if (verbose==2) info_csh(segd_channel_set_header);
 		}

 		/* Sample skew header */
 		for (i_ss = 0; i_ss < n_sk; i_ss ++) {
			if ( EXIT_FAILURE ==  get_ssh(&segd_sample_skew, tapeun) ) break;
 			if (verbose==2) info_ssh(&segd_sample_skew);
 		}
 	}

 	/* Extended Header */
 	for (j = 0; j < n_ec; j ++) {
		if( EXIT_FAILURE == get_ech(&segd_extended_header, tapeun) ) break;
		/* Local decoding */
 		/* (void) sscanf(&segd_extended_header[11], "%2hd:%2hd:%2hd", &tr.hour, &tr.minute, &tr.sec); */
 		if (verbose==2) info_ech(&segd_extended_header);
 	}

 	/* External Header */
 	for (j = 0; j < n_ex; j ++) {
		if( EXIT_FAILURE == get_exh(&segd_external_header, tapeun) ) break;
 		if (verbose==2) info_exh(&segd_external_header);
 	}
 	if (verbose==2) warn ("there are %d channels", n_chan);

 	/*************************
 	* Read the n_chan traces *
 	*************************/

 	for (i_tr=0; i_tr<n_chan; i_tr++) {

 		if (buff) {
 			if (-1 == (nread = (int) read(tapefd, (void *) bloc1, (size_t) REC_L))){
 				if (verbose)
 					warn("tape read error on trace %d", itr);
 				if (++errcount > errmax)
 					err("exceeded maximum io errors");
 			} else { /* Reset counter on successful tape IO */
 				errcount = 0;
 			}
		(void) sfseek(tapeun,startpos,SEEK_SET);
		if (!nread) break;  /* middle exit loop instead of mile-long while */
		}

 			if (!(segd_general_header_1.y & 0x8000)) { /* multiplexed data */
				/* decode data regarding the format */
 				switch (segd_general_header_1.y) {
 					case 0x0015:                           /* 20 bit binary multiplexed */
 						err("Format 0015 (20 bit binary multiplexed) not yet implemented"); break;
 					case 0x0022:                        /* 8 bit quaternary multiplexed */
 						err("Format 0022 (8 bit quaternary multiplexed) not yet implemented"); break;
 					case 0x0024:                       /* 16 bit quaternary multiplexed */
 						err("Format 0024 (16 bit quaternary multiplexed) not yet implemented"); break;
 					case 0x0036:           /* 24 bit 2's compliment integer multiplexed */
 						err("Format 0036 (24 bit 2's compliment integer multiplexed) not yet implemented"); break;
 					case 0x0038:           /* 32 bit 2's compliment integer multiplexed */
 						err("Format 0038 (32 bit 2's compliment integer multiplexed) not yet implemented"); break;
 					case 0x0042:                       /* 8 bit hexadecimal multiplexed */
 						err("Format 0042 (8 bit hexadecimal multiplexed) not yet implemented"); break;
 					case 0x0044:                      /* 16 bit hexadecimal multiplexed */
 						err("Format 0044 (16 bit hexadecimal multiplexed) not yet implemented"); break;
 					case 0x0048:                      /* 32 bit hexadecimal multiplexed */
 						err("Format 0048 (32 bit hexadecimal multiplexed) not yet implemented"); break;
 					case 0x0058:                             /* 32 bit IEEE multiplexed */
 						err("Format 0058 (32 bit IEEE multiplexed) not yet implemented"); break;
 					case 0x0200:                                             /* illegal */
 						err("Format 0200 illegal, do not use"); break;
 					case 0x0000:                                             /* illegal */
 						err("Format 0000 illegal, do not use"); break;
 					default:
 						err("Data format code: %04x not recognized",segd_general_header_1.y);
 				}
 			}
 			else { /* demultiplexed data */
				nsamp_cs = 0; nsamp_the = 0;
				if( EXIT_FAILURE == get_dth(&segd_dem_trace_header, tapeun) ) break;
 				if (verbose==2) info_dth(&segd_dem_trace_header);
 				scan_type = bcd ((unsigned char *) &segd_dem_trace_header.st, 0, 2) -1;
 				chan_set = bcd ((unsigned char *) &segd_dem_trace_header.cn, 0, 2) -1;
				nsamp_the = 0;
				nsamp_cs = 0;
				if (csd[scan_type][chan_set].te != 0 && hdr1_i != 0)
					nsamp_cs = 2*(csd[scan_type][chan_set].te - csd[scan_type][chan_set].tf)*(16<<bcd(&csd[scan_type][chan_set].sc_j,0,1))/hdr1_i + 1 ;
				else nsamp_cs = ((ns-1)<<bcd(&csd[scan_type][chan_set].sc_j,0,1)) + 1 ;
				if (nsamp_cs != 0 && ns_override == 0) ns = nsamp_cs;
          if(verbose) warn("nsamp_cs=%d\n",nsamp_cs);


				for (i=0; i < segd_dem_trace_header.the; i++) { /* read the trace header extension blocks */
					if ( EXIT_FAILURE == get_the(&segd_trace_header_ext, tapeun) ) break;
				warn ("segd_dem_trace_header.the = %d", segd_dem_trace_header.the);
				if (i == 0) { nsamp_the = segd_trace_header_ext.nbs[2] + 256*(segd_trace_header_ext.nbs[1]+256*(segd_trace_header_ext.nbs[0]));
          if(verbose) warn("nsamp_the=%d\n",nsamp_the); }
				if (nsamp_the != 0 && nsamp_cs != 0 && nsamp_cs != nsamp_the)
				warn("Demux trace header nsamp %u != Demux trace header nsamp %u\n", nsamp_cs, nsamp_the);
				if (nsamp_the != 0) ns = nsamp_the;
				}


  				/* set trace identification code from channel type */
 				switch (csd[scan_type][chan_set].c) {
 					case 0xc0: tr.trid = TDUMMY; break; /* Auxiliary data trailer */
 					case 0x90: tr.trid = TDUMMY; break; /* Signature, filtered */
 					case 0x80: tr.trid = TDUMMY; break; /* Signature, unfiltered */
 					case 0x70: tr.trid = TDUMMY; break; /* Other */
 					case 0x60: tr.trid = TDUMMY; break; /* External data */
 					case 0x50: tr.trid = TIMING; break; /* timing traces */
 					case 0x40: tr.trid = WBREAK; break; /* Water break traces */
 					case 0x30: tr.trid = UPHOLE; break; /* Uphole traces */
 					case 0x20: tr.trid = TBREAK; break; /* Time break traces*/
 					case 0x10: tr.trid = TREAL;  break; /* Real time traces*/
 					case 0x00: tr.trid = TDUMMY; break; /* Unused */
 					default: tr.trid = TDUMMY; warn("channel type %02x unknown", csd[scan_type][chan_set].c); break;
 				}

 				tr.tracf = bcd ((unsigned  char *) &(segd_dem_trace_header.tn[0]), 0, 4);
 				tr.delrt = csd[scan_type][chan_set].tf * 2;

				if(ns_override != 0) ns = ns_override;

				/* decode data regarding the format */
 				switch (segd_general_header_1.y) {
 					case 0x8015:                        /* 20 bits binary demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the)*4)/10; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*5/2);
						ns = tr.ns;
 						F8015_to_float (tapeun, (float *) tr.data, ns); break;
 					case 0x8022:                      /* 8 bit quaternary demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the))/1; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*1);
						ns = tr.ns;
 						F8022_to_float (tapeun, (float *) tr.data, ns); break;
 					case 0x8024:                      /* 16 bit quaternary demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the))/2; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*2);
						ns = tr.ns;
 						F8024_to_float (tapeun, (float *) tr.data, ns); break;
 					case 0x8036:          /* 24 bit 2's compliment integer demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the))/3; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*3);
						ns = tr.ns;
 						F8036_to_float (tapeun, (float *) tr.data, ns); break;
 					case 0x8038:          /* 32 bit 2's compliment integer demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the))/4; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*4);
						ns = tr.ns;
 						F8038_to_float (tapeun, (float *) tr.data, ns); break;
 					case 0x8042:                      /* 8 bit hexadecimal demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the))/1; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*1);
						ns = tr.ns;
 						F8042_to_float (tapeun, (float *) tr.data, ns); break;
 					case 0x8044:                     /* 16 bit hexadecimal demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the))/2; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*2);
						ns = tr.ns;
 						F8044_to_float (tapeun, (float *) tr.data, ns); break;
 					case 0x8048:                     /* 32 bit hexadecimal demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the))/4; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*4);
						ns = tr.ns;
 						F8048_to_float (tapeun, (float *) tr.data, ns); break;
 					case 0x8058:                            /* 32 bit IEEE demultiplexed */
						if(buff) tr.ns = ((nread-20-32*segd_dem_trace_header.the))/4; /* number of samples from block length */
						else tr.ns = ns;
 						if (tr.ns != ns)
						   warn("Tape read length for trace %d inconsistent with trace length computed from header(s)\n",nread,ns*4);
						ns = tr.ns;
 						F8058_to_float (tapeun, (float *) tr.data, ns); break;
 					default:
 						err("Data format code: %04x not recognized",segd_general_header_1.y);
 				}

  				/* Apply gain if requested */
 				if (gain) {
 					for (i = 0; i<tr.ns; i++)
 						tr.data[i] *= mmp[scan_type][chan_set];
 				}
				if (ipt >= ptmin-1) {
 				/* Write the trace */
 				/* if aux = 0, skip auxiliary channels */
 				if (aux!=0 || csd[scan_type][chan_set].c == 0x10)
				{
/*  				puttr(&tr); */
				/* this essentially does the important part of
				 * what puttr does without seg faulting with
				 * gcc 3.2.1, 3.4.5, 4.1.{1,2} */
					fwrite(&tr, 1, HDRBYTES, stdout);
					databytes = ns * sizeof(float);
					fwrite(tr.data, 1, databytes, stdout);
				}

 				/* Echo under verbose option */
 				if (verbose && ++itr % vblock == 0)
 					warn(" %d traces from tape", itr);

				}
 			}
 	}

 	/***************************
 	* Read the General Trailer *
 	***************************/

 	for (j = 0; j < n_gt; j ++) {
		if( EXIT_FAILURE == get_gt(&segd_general_trailer, tapeun) ) break;
 		if (verbose==2) info_gt(&segd_general_trailer);
 	}

 	/* EOF = end of shot */
 	if (buff) {
 		if (-1 == (nread = (int) read(tapefd, (void *) bloc1, (size_t) REC_L))){
 			if (verbose)
 				warn("tape read error on header block from shot %d", (ipt+1));
 			if (++errcount > errmax)
 				err("exceeded maximum io errors");
 		} else { /* Reset counter on successful tape IO */
 			errcount = 0;
 		}
	(void) sfseek(tapeun, startpos, SEEK_SET);
 	if (nread) warn("not at EOF as should be!");
	}
 	ipt++ ;
 }

 /* Clean up */
 ipt = ipt - ptmin + 1;
 if (verbose) warn ("%d shots (%d traces) from tape", ipt, itr);
 (void) sfclose(tapeun);
 if(buff) eclose(tapefd);

 if (verbose) warn("tape closed successfully");
 if(mmp != NULL) free2float (mmp);
 if(csd != NULL) free2 ((void **) csd);
 if(bloc1 != NULL) free1 (bloc1);

 return EXIT_SUCCESS;
}

/* bcd - convert bcd to int
 *
 * Credits:
 *      EOPG: Marc, Jdt
 *
 * Parameters:
 *    ptr    - address of first byte of the number
 *    begin  - 0 or 1, position of the first digit of the number
 *    n      - number of digits
 *
 */

static int bcd (unsigned char * ptr , int begin , int n)
{
 register int i;
 unsigned int val;

 val = 0;
 if (n == 0) return (val);

 for (i = 0; i<n; i++) {
 	val *= 10;
 	if (begin++ & 1) val += (*ptr++ & 15);
 	else val += (*ptr >> 4) & 15;
 }
 return (val);
}

#if 0
/* segdread.c:809: warning: `F0015_to_float' defined but not used */
/* F0015_to_float - convert 20 bit binary multiplexed data into floating numbers
 *
 * Credits:
 *      EOPG: Marc Schaming, Jean-Daniel Tissot
 *      SEP:  Stew Levin - fixed low-order bit error in conversion
 *            of negative values on 2's complement machines.
 *            Use ldexp() function instead of much slower value*pow(2,expo)
 *      SEP:  Adapted F8015 to F0015 conversion
 *
 *
 * Parameters:
 *    from   - input vector
 *    to     - output vector
 *    len    - number of packets of 4 floats in vectors
 *
 */
/*
 *
 * Format 0015 is a 10 byte per 4 words (2 1/2 bytes per word)
 * representation.  According to the SEG specifications, the
 * bit layout of the 10 bytes is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1    C3    C2    C1    C0    C3    C2    C1    C0    Exponents for
 * Byte 2    C3    C2    C1    C0    C3    C2    C1    C0    channels 1 thru 4
 *
 * Byte 3     S    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 1
 * Byte 4    Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14   0
 * Byte 5     S    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 2
 * Byte 6    Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14   0
 * Byte 7     S    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 3
 * Byte 8    Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14   0
 * Byte 9     S    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 4
 * Byte 10   Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14   0
 *
 * S=sign bit. - (One = negative number)
 * C=binary exponents. - This is a 4 bit positive binary exponent of 2
 *               CCCC
 *   written as 2     where CCCC can assume values of 0-15.  The four
 *   exponents are in channel number order for the four channels starting
 *   with channel one in bits 0-3 of Byte 1.
 * Q1-14-fraction. - This is a 14 bit one's complement binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *                                  -1
 *   with the MSB being defined as 2 .  The sign and fraction can assume
 *                  -14       -14
 *   values from 1-2   to -1+2  .  Note that bit 7 of the second byte
 *   of each sample must be zero in order to guarantee the uniqueness of
 *   the start of scan.  Negative zero is invalid and must be converted
 *   to positive zero.
 *                                       CCCC    MP                   MP
 * Input signal = S.QQQQ,QQQQ,QQQQ,QQ x 2    x 2    millivolts where 2
 *   is the value required to descale the data word to the recording
 *   system input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 * Note that in utilizing this data recording method, the number of data
 *   channels per channel set must be exactly divisible by 4 in order to
 *   preserve the data grouping of this method.
 */

static void F0015_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register short ex1_4;
 int expo;
 short fraction;

 for (i = 0; i < len; i += 4) {
	ex1_4 = GET_S(from);
 	expo = ((ex1_4 >> 12) & 0x0F) - 15;
	fraction = GET_S(from);
	if (fraction < 0) fraction = -((~fraction)&(~1));
 	*(to++) = ldexp((double) fraction, expo);

 	expo = ((ex1_4 >> 8) & 0x0F) - 15;
	fraction = GET_S(from);
	if (fraction < 0) fraction = -((~fraction)&(~1));
 	*(to++) = ldexp((double) fraction, expo);

 	expo = ((ex1_4 >> 4) & 0x0F) - 15;
	fraction = GET_S(from);
	if (fraction < 0) fraction = -((~fraction)&(~1));
 	*(to++) = ldexp((double) fraction, expo);

 	expo = (ex1_4 & 0x0F) - 15;
	fraction = GET_S(from);
	if (fraction < 0) fraction = -((~fraction)&(~1));
 	*(to++) = ldexp((double) fraction, expo);
 }
}
#endif

/* F8015_to_float - convert 20 bit binary demultiplexed data into floating numbers
 *
 * Credits:
 *      EOPG: Marc Schaming, Jean-Daniel Tissot
 *      SEP:  Stew Levin - fixed low-order bit error in conversion
 *            of negative values on 2's complement machines.
 *            Use ldexp() function instead of much slower value*pow(2,expo)
 *
 * Parameters:
 *    from   - input vector
 *    to     - output vector
 *    len    - number of packets of 4 floats in vectors
 *
 */
/*
 *
 * Format 8015 is a 10 byte per 4 words (2 1/2 bytes per word)
 * representation.  According to the SEG specifications, the
 * bit layout of the 10 bytes is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1    C3    C2    C1    C0    C3    C2    C1    C0    Exponents for
 * Byte 2    C3    C2    C1    C0    C3    C2    C1    C0    channels 1 thru 4
 *
 * Byte 3     S    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 1
 * Byte 4    Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14  Q-15
 * Byte 5     S    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 2
 * Byte 6    Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14  Q-15
 * Byte 7     S    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 3
 * Byte 8    Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14  Q-15
 * Byte 9     S    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 4
 * Byte 10   Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14  Q-15
 *
 * S=sign bit. - (One = negative number)
 * C=binary exponents. - This is a 4 bit positive binary exponent of 2
 *               CCCC
 *   written as 2     where CCCC can assume values of 0-15.  The four
 *   exponents are in channel number order for the four channels starting
 *   with channel one in bits 0-3 of Byte 1.
 * Q1-15-fraction. - This is a 15 bit one's complement binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *                                  -1
 *   with the MSB being defined as 2 .  The sign and fraction can assume
 *                  -15       -15
 *   values from 1-2   to -1+2  .  Negative zero is invalid and must be
 *   converted to positive zero.
 *                                        CCCC    MP                   MP
 * Input signal = S.QQQQ,QQQQ,QQQQ,QQQ x 2    x 2    millivolts where 2
 *   is the value required to descale the data word to the recording
 *   system input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 * Note that in utilizing this data recording method, the number of data
 *   channels per channel set must be exactly divisible by 4 in order to
 *   preserve the data grouping of this method.
 */

static void F8015_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register short ex1_4;
 int expo;
 short fraction;

 for (i = 0; i < len; i += 4) {
      ex1_4 = GET_S(from);
 	expo = ((ex1_4 >> 12) & 15) - 15;
      fraction = GET_S(from);
	if (fraction < 0) fraction = -(~fraction);
 	*(to++) = ldexp((double) fraction, expo);

 	expo = ((ex1_4 >> 8) & 15) - 15;
      fraction = GET_S(from);
	if (fraction < 0) fraction = -(~fraction);
 	*(to++) = ldexp((double) fraction, expo);

 	expo = ((ex1_4 >> 4) & 15) - 15;
      fraction = GET_S(from);
	if (fraction < 0) fraction = -(~fraction);
 	*(to++) = ldexp((double) fraction, expo);

 	expo = (ex1_4 & 15) - 15;
      fraction = GET_S(from);
	if (fraction < 0) fraction = -(~fraction);
 	*(to++) = ldexp((double) fraction, expo);
 }
}

/* F8022_to_float - convert 8 bit quaternary demultiplexed data into floating numbers
 *
 * Credits:
 *      SEP:  Stew Levin
 *
 * Parameters:
 *    from   - input sfio unit
 *    to     - output vector
 *    len    - number of packets of 4 floats in vectors
 *
 */
/*
 *
 * Format 8022 is a 1 byte per word representation.
 * According to the SEG specifications, the bit
 * layout of the byte is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1     S    C2    C1    C0    Q-1   Q-2   Q-3   Q-4
 *
 * S=sign bit. - (One = negative number)
 * C=quaternary exponent. - This is a 3 bit positive binary exponent of 4
 *               CCC
 *   written as 4    where CCC can assume values of 0-7.
 * Q1-4-fraction. - This is a 4 bit one's complement binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *                                  -1
 *   with the MSB being defined as 2 .  The fraction can have values
 *           -4        -4
 *   from 1-2   to -1+2  .  Negative zero is invalid and must be
 *   converted to positive zero.
 *                          CCC   MP                   MP
 * Input signal = S.QQQQ x 4   x 2   millivolts where 2    is the
 *   value required to descale the data word to the recording system
 *   input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 */

static void F8022_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register int ex1_4;
 int expo;
 short fraction;

 for (i = 0; i < len; i ++) {
      ex1_4 = GET_C(from);
      expo = ((ex1_4 >> 3) & 14) - 4;
      fraction = ex1_4 & 15;
	if (ex1_4 & 128) fraction = -(15^fraction);
 	*(to++) = ldexp((double) fraction, expo);
 }
}

/* F8024_to_float - convert 16 bit quaternary demultiplexed data into floating numbers
 *
 * Credits:
 *      SEP:  Stew Levin
 *
 * Parameters:
 *    from   - input sfio unit
 *    to     - output vector
 *    len    - number of packets of 4 floats in vectors
 *
 */
/*
 *
 * Format 8024 is a 2 byte per word representation.
 * According to the SEG specifications, the bit
 * layout of the bytes is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1     S    C2    C1    C0    Q-1   Q-2   Q-3   Q-4
 * Byte 2    Q-5   Q-6   Q-7   Q-8   Q-9   Q-10  Q-11  Q-12
 *
 * S=sign bit. - (One = negative number)
 * C=quaternary exponent. - This is a 3 bit positive binary exponent of 4
 *               CCC
 *   written as 4    where CCC can assume values of 0-7.
 * Q1-12-fraction. - This is a 12 bit one's complement binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *                                  -1
 *   with the MSB being defined as 2 .  The fraction can have values
 *           -12        -12
 *   from 1-2    to -1+2   .  Negative zero is invalid and must be
 *   converted to positive zero.
 *                                    CCC   MP                   MP
 * Input signal = S.QQQQ,QQQQ,QQQQ x 4   x 2   millivolts where 2
 *   is the value required to descale the data word to the recording
 *   system input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 */

static void F8024_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register int ex1_4;
 int expo;
 short fraction;

 for (i = 0; i < len; i ++) {
      ex1_4 = GET_S(from);
      expo = ((ex1_4 >> 11) & 14) - 12;
      fraction = ex1_4 & 4095;
      if (ex1_4 & 32768) fraction = -(4095^fraction);
      *(to++) = ldexp((double) fraction, expo);
 }
}

/* F8036_to_float - convert 24 bit quaternary demultiplexed data into floating numbers
 *
 * Credits:
 *      SEP:  Stew Levin
 *
 * Parameters:
 *    from   - input sfio unit
 *    to     - output vector
 *    len    - number of packets of 4 floats in vectors
 *
 */
/*
 *
 * Format 8036 is a 3 byte per word representation.
 * According to the SEG specifications, the bit
 * layout of the bytes is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Q-8
 * Byte 2    Q-9   Q-10  Q-11  Q-12  Q-13  Q-14  Q-15  Q-16
 * Byte 3    Q-17  Q-18  Q-19  Q-20  Q-21  Q-22  Q-23  Q-24
 *
 * Q1-24-integer. - This is a 24 bit two's complement binary integer.
 *                         MP                   MP
 * Input signal = Q...Q x 2   millivolts where 2
 *   is the value required to descale the data word to the recording
 *   system input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 */

static void F8036_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register long int ival;

 for (i = 0; i < len; i ++) {
      ival = GET_UC(from);
      ival <<= 8; ival |= GET_UC(from);
      ival <<= 8; ival |= GET_UC(from);
      if(ival > 8388607) ival -= 16777216;
      *(to++) = (float) ival;
 }
}

/* F8038_to_float - convert 32 bit quaternary demultiplexed data into floating numbers
 *
 * Credits:
 *      SEP:  Stew Levin
 *
 * Parameters:
 *    from   - input sfio unit
 *    to     - output vector
 *    len    - number of packets of 4 floats in vectors
 *
 */
/*
 *
 * Format 8038 is a 4 byte per word representation.
 * According to the SEG specifications, the bit
 * layout of the bytes is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Q-8
 * Byte 2    Q-9   Q-10  Q-11  Q-12  Q-13  Q-14  Q-15  Q-16
 * Byte 3    Q-17  Q-18  Q-19  Q-20  Q-21  Q-22  Q-23  Q-24
 * Byte 4    Q-25  Q-26  Q-27  Q-28  Q-29  Q-30  Q-31  Q-32
 *
 * Q1-32-fraction. - This is a 32 bit two's complement binary integer.
 *                         MP                   MP
 * Input signal = Q...Q x 2   millivolts where 2
 *   is the value required to descale the data word to the recording
 *   system input level.  MP is defined in Byte 8 of each of the corre-
 */
 /* Note this conversion routine assumes the target architecture is
  * already 2's complement.
  */

static void F8038_to_float (Sfio_t *from, float to[], int len)
{
 int i;
 long int ex1_4;
 long int ex2_4;
 long int value;

 for (i = 0; i < len; i ++) {
      ex1_4 = GET_S(from);
      ex2_4 = GET_S(from);
      value = (ex1_4<<16) | (ex2_4&65535);
      *(to++) = (float) value;
 }
}

/* F8042_to_float - convert 8 bit hexadecimal demultiplexed data into floating numbers
 *
 * Credits:
 *      SEP:  Stew Levin
 *
 * Parameters:
 *    from   - input sfio unit
 *    to     - output vector
 *    len    - number of floats in vector
 *
 */
/*
 *
 * Format 8042 is a 1 byte per word representation.
 * According to the SEG specifications, the bit
 * layout of the byte is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1     S    C1    C0    Q-1   Q-2   Q-3   Q-4   Q-5
 *
 * S=sign bit. - (One = negative number)
 * C=hexadecimal exponent. - This is a 2 bit positive binary exponent of 16
 *                CC
 *   written as 16    where CC can assume values of 0-3.
 * Q1-5-fraction. - This is a 5 bit positive binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *                                  -1
 *   with the MSB being defined as 2 .  The fraction can have values
 *           -5        -5
 *   from 1-2   to -1+2  .
 *                             CC    MP                   MP
 * Input signal = S.QQQQ,Q x 16   x 2   millivolts where 2    is the
 *   value required to descale the data word to the recording system
 *   input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 */

static void F8042_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register int ex1_4;
 int expo;
 short fraction;

 for (i = 0; i < len; i ++) {
      ex1_4 = GET_C(from);
      expo = ((ex1_4 >> 3) & 12) - 5;
      fraction = ex1_4 & 31;
      if (ex1_4 & 128) fraction = -fraction;
      *(to++) = ldexp((double) fraction, expo);
 }
}

/* F8044_to_float - convert 16 bit hexadecimal demultiplexed data into floating numbers
 *
 * Credits:
 *      SEP:  Stew Levin
 *
 * Parameters:
 *    from   - input sfio unit
 *    to     - output vector
 *    len    - number of floats in vector
 *
 */
/*
 *
 * Format 8044 is a 2 byte per word representation.
 * According to the SEG specifications, the bit
 * layout of the bytes is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1     S    C1    C0    Q-1   Q-2   Q-3   Q-4   Q-5
 * Byte 2    Q-6   Q-7   Q-8   Q-9   Q-10  Q-11  Q-12  Q-13
 *
 * S=sign bit. - (One = negative number)
 * C=hexadecimal exponent. - This is a 2 bit positive binary exponent of 16
 *                CC
 *   written as 16    where CC can assume values of 0-3.
 * Q1-13-fraction. - This is a 13 bit positive binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *                                  -1
 *   with the MSB being defined as 2 .  The fraction can have values
 *           -13        -13
 *   from 1-2    to -1+2   .
 *                                       CC    MP                   MP
 * Input signal = S.QQQQ,QQQQ,QQQQ,Q x 16   x 2   millivolts where 2
 *   is the value required to descale the data word to the recording system
 *   input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 */

static void F8044_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register int ex1_4;
 int expo;
 short fraction;

 for (i = 0; i < len; i ++) {
      ex1_4 = GET_S(from);
      expo = ((ex1_4 >> 11) & 12) - 13;
      fraction = ex1_4 & 8191;
      if (ex1_4 & 32768) fraction = -fraction;
      *(to++) = ldexp((double) fraction, expo);
 }
}

/* F8048_to_float - convert 32 bit hexadecimal demultiplexed data into floating numbers
 *
 * Credits:
 *      SEP:  Stew Levin
 *
 * Parameters:
 *    from   - input sfio unit
 *    to     - output vector
 *    len    - number of floats in vector
 *
 */
/*
 *
 * Format 8048 is a 4 byte per word representation.
 * According to the SEG specifications, the bit
 * layout of the bytes is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1     S    C6    C5    C4    C3    C2    C1    C0
 * Byte 2    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Q-8
 * Byte 3    Q-9   Q-10  Q-11  Q-12  Q-13  Q-14  Q-15  Q-16
 * Byte 4    Q-17  Q-18  Q-19  Q-20  Q-21  Q-22  Q-23  0
 *
 * S=sign bit. - (One = negative number)
 * C=hexadecimal exponent. - This is a binary exponent of 16
 *                (CCCCCCC-64)
 *   written as 16             where CC can assume values of 0-127.
 * Q1-23-fraction. - This is a 23 bit positive binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *                                  -1
 *   with the MSB being defined as 2 .  The sign and fraction can have
 *                 -23        -23
 *   values from 1-2    to -1+2   .
 *                                   C-64    MP                   MP
 * Input signal = S.QQQQ,...,QQQ x 16     x 2   millivolts where 2
 *   is the value required to descale the data word to the recording system
 *   input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 *   The data recording method has more than sufficient range to
 *   handle the dynamic range of a typical seismic system.  Thus, MP
 *   may not be needed to account for any scaling and may be recorded
 *   as zero.
 */

static void F8048_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register int ex1_4;
 int expo;
 long int fraction;

 for (i = 0; i < len; i ++) {
      ex1_4 = GET_S(from);
      expo = ((ex1_4 >> 6) & 508) - (24+256);
      fraction = ex1_4 & 255;
      fraction <<= 16; fraction |= (GET_S(from)&65535);
      if (ex1_4 & 32768) fraction = -fraction;
      *(to++) = ldexp((double) fraction, expo);
 }
}

/* F8058_to_float - convert 32 bit IEEE float demultiplexed data into floating numbers
 *
 * Credits:
 *      SEP:  Stew Levin
 *
 * Parameters:
 *    from   - input sfio unit
 *    to     - output vector
 *    len    - number of floats in vector
 *
 */
/*
 *
 * Format 8058 is a 4 byte per word representation.
 * According to the SEG specifications, the bit
 * layout of the bytes is:
 *
 *
 *  Bit       0     1     2     3     4     5     6     7
 *-----------------------------------------------------------
 * Byte 1     S    C7    C6    C5    C4    C3    C2    C1
 * Byte 2    C0    Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7
 * Byte 3    Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14  Q-15
 * Byte 4    Q-16  Q-17  Q-18  Q-19  Q-20  Q-21  Q-22  Q-23
 *
 * S=sign bit. - (One = negative number)
 * C=exponent. - This is a excess-127 binary exponent of 2
 *               (CCCCCCCC-127)
 *   written as 2               where CC can assume values of 0-255.
 * Q1-23-fraction. - This is a 23 bit positive binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *                                  -1
 *   with the MSB being defined as 2 .  With the exceptions noted below:
 *
 *                    S                    C-127    MP                   MP
 * Input signal = (-1) x 1.QQQQ,...,QQQ x 2     x 2   millivolts where 2
 *   is the value required to descale the data word to the recording system
 *   input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 *   The data recording method has more than sufficient range to
 *   handle the dynamic range of a typical seismic system.  Thus, MP
 *   may not be needed to account for any scaling and may be recorded
 *   as zero.
 *
 * Exceptions:
 *
 * If C=0 then
 *                    S                    -126     MP
 * Input signal = (-1) x 0.QQQQ,...,QQQ x 2     x 2   millivolts
 *
 * If C=255 and Q=0, then
 *                    S
 * Input signal = (-1) x infinity  (overflow)
 *
 * If C=255 and Q!=0, then
 *
 * Input signal = NaN  (Not-a-Number)
 */

static void F8058_to_float (Sfio_t *from, float to[], int len)
{
 register int i;
 register int ex1_4, ex2_4;
 int expo;
 long int fraction;

 for (i = 0; i < len; i ++) {
      ex1_4 = GET_S(from);
      ex2_4 = GET_S(from);
      expo = ((ex1_4 >> 7) & 255);
      fraction = ex1_4 & 127;
      fraction <<= 16; fraction |= (ex2_4&65535);
      if(expo) fraction |= 8388608;
      else fraction <<= 1;
      if (ex1_4 & 32768) fraction = -fraction;
      *(to++) = ldexp((double) fraction, expo-(23+127));
 }
}

int
get_gh1(general_header_1 * gh1, Sfio_t *tapeun)
{
  int status;

  gh1->f[0]=GET_UC(tapeun);
  gh1->f[1]=GET_UC(tapeun);
  gh1->y=GET_US(tapeun);
  gh1->k1_k2=GET_C(tapeun);
  gh1->k3_k4=GET_C(tapeun);
  gh1->k5_k6=GET_C(tapeun);
  gh1->k7_k8=GET_C(tapeun);
  gh1->k9_k10=GET_C(tapeun);
  gh1->k11_k12=GET_C(tapeun);
  gh1->yr=GET_UC(tapeun);
  gh1->gh_dy1=GET_UC(tapeun);

  gh1->dy=GET_UC(tapeun);
  gh1->h=GET_UC(tapeun);
  gh1->mi=GET_UC(tapeun);
  gh1->se=GET_UC(tapeun);

  gh1->m[0]=GET_UC(tapeun);
  gh1->m[1]=GET_UC(tapeun);
  gh1->m[2]=GET_UC(tapeun);
  gh1->b[0]=GET_UC(tapeun);

  gh1->b[1]=GET_UC(tapeun);
  gh1->b[2]=GET_UC(tapeun);
  gh1->i=GET_UC(tapeun);
  gh1->p_sbx=GET_UC(tapeun);

  gh1->sb=GET_UC(tapeun);
  gh1->z_r1=GET_UC(tapeun);
  gh1->r=GET_UC(tapeun);
  gh1->str=GET_UC(tapeun);

  gh1->cs=GET_UC(tapeun);
  gh1->sk=GET_UC(tapeun);
  gh1->ec=GET_UC(tapeun);
  gh1->ex=GET_UC(tapeun);

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int
get_gh2(general_header_2 * gh2, Sfio_t *tapeun)
{
  int status;

  gh2->ef[0]=GET_UC(tapeun);
  gh2->ef[1]=GET_UC(tapeun);
  gh2->ef[2]=GET_UC(tapeun);
  gh2->en[0]=GET_UC(tapeun);

  gh2->en[1]=GET_UC(tapeun);
  gh2->ecx[0]=GET_UC(tapeun);
  gh2->ecx[1]=GET_UC(tapeun);
  gh2->eh[0]=GET_UC(tapeun);

  gh2->eh[1]=GET_UC(tapeun);
  gh2->x1=GET_C(tapeun);
  gh2->rev[0]=GET_UC(tapeun);
  gh2->rev[1]=GET_UC(tapeun);

  gh2->gt=GET_US(tapeun);
  gh2->erl[0]=GET_UC(tapeun);
  gh2->erl[1]=GET_UC(tapeun);

  gh2->erl[2]=GET_UC(tapeun);
  gh2->x2=GET_C(tapeun);
  gh2->bn=GET_UC(tapeun);
  gh2->x3[0]=GET_C(tapeun);

  gh2->x3[1]=GET_C(tapeun);
  gh2->x3[2]=GET_C(tapeun);
  gh2->x3[3]=GET_C(tapeun);
  gh2->x3[4]=GET_C(tapeun);

  gh2->x3[5]=GET_C(tapeun);
  gh2->x3[6]=GET_C(tapeun);
  gh2->x3[7]=GET_C(tapeun);
  gh2->x3[8]=GET_C(tapeun);

  gh2->x3[9]=GET_C(tapeun);
  gh2->x3[10]=GET_C(tapeun);
  gh2->x3[11]=GET_C(tapeun);
  gh2->x3[12]=GET_C(tapeun);

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int
get_ghn(general_header_n * ghn, Sfio_t *tapeun)
{
  int status;

  ghn->x1[0]=GET_C(tapeun);
  ghn->x1[1]=GET_C(tapeun);
  ghn->x1[2]=GET_C(tapeun);
  ghn->sln[0]=GET_UC(tapeun);

  ghn->sln[1]=GET_UC(tapeun);
  ghn->sln[2]=GET_UC(tapeun);
  ghn->sln[3]=GET_UC(tapeun);
  ghn->sln[4]=GET_UC(tapeun);

  ghn->spn[0]=GET_UC(tapeun);
  ghn->spn[1]=GET_UC(tapeun);
  ghn->spn[2]=GET_UC(tapeun);
  ghn->spn[3]=GET_UC(tapeun);

  ghn->spn[4]=GET_UC(tapeun);
  ghn->spi=GET_UC(tapeun);
  ghn->pc=GET_UC(tapeun);
  ghn->v=GET_UC(tapeun);

  ghn->pa=GET_S(tapeun);
  ghn->bn=GET_UC(tapeun);
  ghn->ss=GET_UC(tapeun);

  ghn->x2[0]=GET_C(tapeun);
  ghn->x2[1]=GET_C(tapeun);
  ghn->x2[2]=GET_C(tapeun);
  ghn->x2[3]=GET_C(tapeun);

  ghn->x2[4]=GET_C(tapeun);
  ghn->x2[5]=GET_C(tapeun);
  ghn->x2[6]=GET_C(tapeun);
  ghn->x2[7]=GET_C(tapeun);

  ghn->x2[8]=GET_C(tapeun);
  ghn->x2[9]=GET_C(tapeun);
  ghn->x2[10]=GET_C(tapeun);
  ghn->x2[11]=GET_C(tapeun);

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int
get_gn_sn358(gen_head_sn358 * gh358, Sfio_t *tapeun)
{
  int status;

  gh358->fc1=GET_UC(tapeun);
  gh358->lc1=GET_UC(tapeun);
  gh358->fc2=GET_UC(tapeun);
  gh358->f_lc2=GET_UC(tapeun);

  gh358->lc2=GET_UC(tapeun);
  gh358->fc3=GET_UC(tapeun);
  gh358->f_lc3=GET_UC(tapeun);
  gh358->lc3=GET_UC(tapeun);

  gh358->fc4=GET_UC(tapeun);
  gh358->f_lc4=GET_UC(tapeun);
  gh358->lc4=GET_UC(tapeun);
  gh358->f_lac1=GET_UC(tapeun);

  gh358->fsc1=GET_UC(tapeun);
  gh358->f_lsc1=GET_UC(tapeun);
  gh358->lsc1=GET_UC(tapeun);
  gh358->sam_int1=GET_UC(tapeun);

  gh358->fac2=GET_UC(tapeun);
  gh358->fsc2=GET_UC(tapeun);
  gh358->f_lsc2=GET_UC(tapeun);
  gh358->lsc2=GET_UC(tapeun);

  gh358->sam_int2=GET_UC(tapeun);
  gh358->bl_sig_le=GET_UC(tapeun);
  gh358->rec_length[0]=GET_UC(tapeun);
  gh358->rec_length[1]=GET_UC(tapeun);

  gh358->dyn_swit_del[0]=GET_UC(tapeun);
  gh358->dyn_swit_del[1]=GET_UC(tapeun);
  gh358->rec_del[0]=GET_UC(tapeun);
  gh358->rec_del[1]=GET_UC(tapeun);

  gh358->ty_a_cha12=GET_UC(tapeun);
  gh358->ty_a_cha34=GET_UC(tapeun);
  gh358->ty_a_cha56=GET_UC(tapeun);
  gh358->ty_a_cha78=GET_UC(tapeun);

  gh358->mode_num=GET_UC(tapeun);
  gh358->an_sys_co=GET_UC(tapeun);
  gh358->reel_num[0]=GET_UC(tapeun);
  gh358->reel_num[1]=GET_UC(tapeun);

  gh358->file_num[0]=GET_UC(tapeun);
  gh358->file_num[1]=GET_UC(tapeun);
  gh358->sp_num[0]=GET_UC(tapeun);
  gh358->sp_num[1]=GET_UC(tapeun);

  gh358->lc_nf=GET_UC(tapeun);
  gh358->fg_ic=GET_UC(tapeun);
  gh358->of1=GET_UC(tapeun);
  gh358->of2=GET_UC(tapeun);

  gh358->of3=GET_UC(tapeun);
  gh358->osc_att=GET_UC(tapeun);
  gh358->t_sig_ph=GET_UC(tapeun);
  gh358->m_gain=GET_UC(tapeun);

  gh358->dum[0]=GET_UC(tapeun);
  gh358->dum[1]=GET_UC(tapeun);
  gh358->dum[2]=GET_UC(tapeun);
  gh358->dum[3]=GET_UC(tapeun);

  gh358->dum[4]=GET_UC(tapeun);
  gh358->dum[5]=GET_UC(tapeun);
  gh358->dum[6]=GET_UC(tapeun);
  gh358->dum[7]=GET_UC(tapeun);

  gh358->dum[8]=GET_UC(tapeun);
  gh358->dum[9]=GET_UC(tapeun);
  gh358->dum[10]=GET_UC(tapeun);
  gh358->dum[11]=GET_UC(tapeun);

  gh358->dum[12]=GET_UC(tapeun);
  gh358->dum[13]=GET_UC(tapeun);
  gh358->dum[14]=GET_UC(tapeun);
  gh358->dum[15]=GET_UC(tapeun);

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int
get_csh(channel_set_header * csh, Sfio_t *tapeun)
{
  int status;

  csh->st=GET_UC(tapeun);
  csh->cn=GET_UC(tapeun);
  csh->tf=GET_US(tapeun);

  csh->te=GET_US(tapeun);
  csh->mp[0]=GET_UC(tapeun);
  csh->mp[1]=GET_UC(tapeun);

  csh->cs[0]=GET_UC(tapeun);
  csh->cs[1]=GET_UC(tapeun);
  csh->c=GET_UC(tapeun);
  csh->sc_j=GET_UC(tapeun);

  csh->af=GET_US(tapeun);
  csh->as=GET_US(tapeun);

  csh->lc=GET_US(tapeun);
  csh->ls=GET_US(tapeun);

  csh->nt[0]=GET_US(tapeun);
  csh->nt[1]=GET_US(tapeun);

  csh->nt[2]=GET_US(tapeun);
  csh->ecs=GET_US(tapeun);

  csh->efh=GET_UC(tapeun);
  csh->vs=GET_UC(tapeun);
  csh->cab=GET_UC(tapeun);
  csh->ary=GET_UC(tapeun);

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}


int
get_the(trace_header_ext * the, Sfio_t *tapeun)
{
  int status;

  the->rln[0]=GET_UC(tapeun);
  the->rln[1]=GET_UC(tapeun);
  the->rln[2]=GET_UC(tapeun);
  the->rpn[0]=GET_UC(tapeun);

  the->rpn[1]=GET_UC(tapeun);
  the->rpn[2]=GET_UC(tapeun);
  the->rpi=GET_UC(tapeun);
  the->nbs[0]=GET_UC(tapeun);

  the->nbs[1]=GET_UC(tapeun);
  the->nbs[2]=GET_UC(tapeun);
  the->x[0]=GET_C(tapeun);
  the->x[1]=GET_C(tapeun);

  the->x[2]=GET_C(tapeun);
  the->x[3]=GET_C(tapeun);
  the->x[4]=GET_C(tapeun);
  the->x[5]=GET_C(tapeun);

  the->x[6]=GET_C(tapeun);
  the->x[7]=GET_C(tapeun);
  the->x[8]=GET_C(tapeun);
  the->x[9]=GET_C(tapeun);

  the->x[10]=GET_C(tapeun);
  the->x[11]=GET_C(tapeun);
  the->x[12]=GET_C(tapeun);
  the->x[13]=GET_C(tapeun);

  the->x[14]=GET_C(tapeun);
  the->x[15]=GET_C(tapeun);
  the->x[16]=GET_C(tapeun);
  the->x[17]=GET_C(tapeun);

  the->x[18]=GET_C(tapeun);
  the->x[19]=GET_C(tapeun);
  the->x[20]=GET_C(tapeun);
  the->x[21]=GET_C(tapeun);

 status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int get_ssh(sample_skew * ssh, Sfio_t *tapeun)
{
  int status;
  int i;

  for(i=0; i<32; i++ ) {
	ssh->skew[i] = GET_UC(tapeun);
	}

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int get_ech(extended_header * ech, Sfio_t *tapeun)
{
  int status;
  int i;

  for(i=0; i<32; i++ ) {
	ech->dummy[i] = GET_UC(tapeun);
	}

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int get_exh(external_header * exh, Sfio_t *tapeun)
{
  int status;
  int i;

  for(i=0; i<32; i++ ) {
	exh->dummy[i] = GET_UC(tapeun);
	}

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int get_gt(general_trailer * gt, Sfio_t *tapeun)
{
  int status;
  int i;

  gt->gt = GET_US(tapeun);
  for(i=0; i<8; i++ ) {
	gt->x1[i] = GET_C(tapeun);
  }

  gt->c = GET_UC(tapeun);

  for(i=0; i<21; i++ ) {
	gt->x2[i] = GET_C(tapeun);
  }

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}

int get_dth(dem_trace_header * dth, Sfio_t *tapeun)
{
  int status;

  dth->f = GET_S(tapeun);
  dth->st = GET_UC(tapeun);
  dth->cn = GET_UC(tapeun);
  dth->tn[0] = GET_UC(tapeun);
  dth->tn[1] = GET_UC(tapeun);
  dth->t[0] = GET_UC(tapeun);
  dth->t[1] = GET_UC(tapeun);
  dth->t[2] = GET_UC(tapeun);
  dth->the = GET_UC(tapeun);
  dth->ss = GET_UC(tapeun);
  dth->tr = GET_UC(tapeun);
  dth->tw[0] = GET_UC(tapeun);
  dth->tw[1] = GET_UC(tapeun);
  dth->tw[2] = GET_UC(tapeun);
  dth->en[0] = GET_UC(tapeun);
  dth->en[1] = GET_UC(tapeun);
  dth->efn[0] = GET_UC(tapeun);
  dth->efn[1] = GET_UC(tapeun);
  dth->efn[2] = GET_UC(tapeun);

  status = (sferror(tapeun)||sfeof(tapeun));

  return (status?EXIT_FAILURE:EXIT_SUCCESS);
}


void info_gh1(general_header_1 * gh1)
{
int n_gh, n_str, n_cs;
 n_gh = (*gh1).gh_dy1 >> 4;
 n_str = bcd ((unsigned char *)  (&(gh1->str)), 0, 2);
 n_cs = bcd ((unsigned char *) (&(gh1->cs)), 0, 2);

 warn("\n********************************\n"
 	"segd_general_header_1 (%2d bytes)\n"
 	"********************************", sizeof(*gh1));
 warn("file number: %d", tr.fldr);
 warn("\t\tformat code: %0.4hx\n", gh1->y);
 warn("general constants K1-K12 not decoded");
 warn("\t\tyear: %d\n", tr.year % 100);
 warn("#blks in general header extension: %d\n", n_gh);
 warn("day: %02d", tr.day);
 warn("\t\thour: %02d", tr.hour);
 warn("\t\tminute: %02d", tr.minute);
 warn("\t\tsecond: %02d\n", tr.sec);
 warn("manufacturer s code: %2x", (*gh1).m[0]);
 warn("\t\tmanufacturer's serial number: %02x%02x\n", (*gh1).m[1], (*gh1).m[2]);
 warn("bytes per scan (multiplexed formats) : not decoded");
 warn("\t\tbase scan interval: %d microseconds\n", tr.dt);
 warn("polarity: %x", (*gh1).p_sbx >> 4);
 warn("\t\tscan/block: %d x 2 **%d\n", (*gh1).sb, ((*gh1).p_sbx & 0xff));
 warn("record type (8=normal record) : %x", (*gh1).z_r1 >> 4);
 warn("\t\trecord length: %d units\n", bcd ((unsigned char *) &(*gh1).z_r1, 1, 3));
 warn("scan types / records: %d", n_str);
 warn("\t\tchannel sets per scan type: %d\n", n_cs);
 warn("number of 32-bytes fields for sample skew: %x\n", (*gh1).sk);
 warn("extended header length: %x", (*gh1).ec);
 warn("\t\texternal header length: %x\n", (*gh1).ex);
}

void info_gh2(general_header_2 *gh2)
{
 warn("\n********************************\n"
 	"segd_general_header_2 (%2d bytes)\n"
 	"********************************\n", sizeof(*gh2));
 warn("expanded file number: %2x%2x%2x\n", (*gh2).ef[0], (*gh2).ef[1], (*gh2).ef[1] );
 warn("extended channel sets/scantype: %02x%02x\n", (*gh2).en[0], (*gh2).en[1]);
 warn("extended header blocks: %02x%02x\n", (*gh2).ecx[0], (*gh2).ecx[1] );
 warn("external header blocks: %02x%02x\n", (*gh2).eh[0], (*gh2).eh[1]);
 warn("seg-d revision number: %02x%02x\n", (*gh2).rev[0], (*gh2).rev[1]);
 warn("general trailer number of blocks:  %d\n", (*gh2).gt);
 warn("extended record length: %02x%02x%02x\n", (*gh2).erl[0], (*gh2).erl[1], (*gh2).erl[2]);
}

void info_ghn(general_header_n *ghn)
{
 warn("\n********************************\n"
 	"segd_general_header_n (%2d bytes)\n"
 	"********************************\n", sizeof(*ghn));
 warn("source line number: %02x%02x%02x%02x%02x\n", (*ghn).sln[0], (*ghn).sln[1], (*ghn).sln[2], (*ghn).sln[3], (*ghn).sln[4]);
 warn("source point number: %02x%02x%02x%02x%02x\n", (*ghn).spn[0], (*ghn).spn[1], (*ghn).spn[2], (*ghn).spn[3], (*ghn).spn[4]);
 warn("source point index: %2x\n", (*ghn).spi);
 warn("phase control: %2x\n", (*ghn).pc);
 warn("vibrator type: %2x\n", (*ghn).v);
 warn("phase angle:  %0.4hx\n", (*ghn).pa);
 warn("general header block number: %2x\n", (*ghn).bn);
 warn("source set number: %2x\n", (*ghn).ss);
}

void info_gn_sn358(gen_head_sn358 * gh358)
{
 warn("\n************************************\n"
 	"segd_general_header_sn358 (%2d bytes)\n"
 	"************************************\n", sizeof(*gh358));
 warn("first channel of seismic parameter set 1: 1");
 warn("\tlast1: %d\n", bcd ((unsigned char*) &(*gh358).fc1, 1, 3));
 warn("first channel of seismic parameter set 2: %d", bcd ((unsigned char*) &(*gh358).fc2, 0, 3));
 warn("\tlast: %d\n", bcd ((unsigned char*) &(*gh358).f_lc2, 1, 3));
 warn("first channel of seismic parameter set 3: %d", bcd ((unsigned char*) &(*gh358).fc3, 0, 3));
 warn("\tlast: %d\n", bcd ((unsigned char*) &(*gh358).f_lc3, 1, 3));
 warn("first channel of seismic parameter set 4: %d", bcd ((unsigned char*) &(*gh358).fc4, 0, 3));
 warn("\tlast: %d\n", bcd ((unsigned char*) &(*gh358).f_lc4, 1, 3));
 warn("first auxiliary channel of scan type 1: %d", bcd ((unsigned char*) &(*gh358).f_lac1, 0, 1));
 warn("\tlast: %d\n", bcd ((unsigned char*) &(*gh358).f_lac1, 1, 1));
 warn("first seismic channel of scan type 1: %d", bcd ((unsigned char*) &(*gh358).fsc1, 0, 3));
 warn("\tlast: %d\n", bcd ((unsigned char*) &(*gh358).f_lsc1, 1, 3));
 warn("sample interval of scan type 1: %d\n", ((*gh358).sam_int1*1000) >> 4);
 warn("first auxiliary channel of scan type 2: %d", bcd ((unsigned char*) &(*gh358).fac2, 0, 1));
 warn("\tlast: %d\n", bcd ((unsigned char*) &(*gh358).fac2, 1, 1));
 warn("first seismic channel of scan type 2: %d", bcd ((unsigned char*) &(*gh358).fsc2, 0, 3));
 warn("\tlast: %d\n", bcd ((unsigned char*) &(*gh358).f_lsc2, 1, 3));
 warn("sample interval of scan type 2: %d\n", ((*gh358).sam_int2*1000) >> 4);
 warn("signature length: %3.1f", ((*gh358).bl_sig_le*.1));
 warn("\trecord length: %4.1f\n", bcd((unsigned char*) &((*gh358).rec_length[0]), 1, 3)*.1);
 warn("dynamically switching delay: %4.1f", bcd((unsigned char*) &((*gh358).dyn_swit_del[0]), 1, 3)*.1);
 warn("\trecording delay: %4.1f\n", bcd((unsigned char*) &((*gh358).rec_del[0]), 1, 3)*.1);
 warn("type of auxiliary channels 1 to 8: %1d %1d %1d %1d %1d %1d %1d %1d\n",
 	bcd ((unsigned char*) &(*gh358).ty_a_cha12, 0, 1), bcd ((unsigned char*) &(*gh358).ty_a_cha12, 1, 1),
 	bcd ((unsigned char*) &(*gh358).ty_a_cha34, 0, 1), bcd ((unsigned char*) &(*gh358).ty_a_cha34, 1, 1),
 	bcd ((unsigned char*) &(*gh358).ty_a_cha56, 0, 1), bcd ((unsigned char*) &(*gh358).ty_a_cha56, 1, 1),
 	bcd ((unsigned char*) &(*gh358).ty_a_cha78, 0, 1), bcd ((unsigned char*) &(*gh358).ty_a_cha78, 1, 1));
 warn("mode number: %d", bcd ((unsigned char *) &(*gh358).mode_num, 0, 2));
 warn("\tanalog system count: %d", bcd ((unsigned char *) &(*gh358).an_sys_co, 0, 1));
 warn("\ttape transport number: %d\n", bcd ((unsigned char *) &(*gh358).an_sys_co, 1, 1));
 warn("reel number: %d", bcd ((unsigned char*) &((*gh358).reel_num[0]), 0, 4));
 warn("\tfile logicial number: %d", bcd ((unsigned char*) &((*gh358).file_num[0]), 1, 3));
 warn("\tshot point number: %d\n",  bcd ((unsigned char*) &((*gh358).sp_num[0]), 0, 4));
 warn("LC=%d, NF=%d, FG=%d, IC=%d\n", bcd ((unsigned char *)&(*gh358).lc_nf, 0, 1), bcd (&(*gh358).lc_nf, 1, 1),
 	bcd (&(*gh358).fg_ic, 0, 1), bcd ((unsigned char *)&(*gh358).fg_ic, 1, 1));
 warn("oscillator frequency: %d", bcd ((unsigned char *)&(*gh358).of1, 1, 5));
 warn("\toscillator amplitude: %d\n", bcd ((unsigned char *)&(*gh358).osc_att, 0, 2));
 warn("test signal: %d", bcd ((unsigned char *)&(*gh358).t_sig_ph, 1, 1));
 warn("\tmain gain amplifier: %d\n", bcd ((unsigned char *)&(*gh358).m_gain, 0, 2));
}

void info_csh(channel_set_header *csh)
{
 warn("\n**********************************\n"
 	"segd_channel_set_header (%2d bytes)\n"
 	"**********************************\n", sizeof(*csh));
 warn("scan type number: %2x", (*csh).st);
 warn("\tchannel set number: %2x\n", (*csh).cn);
 warn("channel set start time: %d", (*csh).tf*2 );
 warn("\tchannel set end time: %d\n", (*csh).te*2 );
 warn("descale multiplier: %02x%02x\n", (unsigned int) ((*csh).mp[0]),
					(unsigned int) ((*csh).mp[1]) );
 warn("number of channels: %02x%02x", (unsigned int) ((*csh).cs[0]),
				      (unsigned int) ((*csh).cs[1]) );
 warn("\tchannel type (1=Seis, 8=Sig/unfiltered): %x\n", (*csh).c >> 4);
 warn("sample/channel: %x", (*csh).sc_j >> 4);
 warn("\tchannel gain: %x\n", (*csh).sc_j & 0xf);
 warn("alias filter frequency: %0.4hx", (*csh).af);
 warn("\talias filter slope: %0.4hx\n" ,(*csh).as);
 warn("low cut filter frequency: %0.4hx", (*csh).lc);
 warn("\tlow cut filter slope: %0.4hx\n", (*csh).ls);
 warn("first notch filter: %0.4hx", (*csh).nt[0]);
 warn("\tsecond: %0.4hx", (*csh).nt[1]);
 warn("\tthird: %0.4hx\n", (*csh).nt[2]);
 warn("extended channel set number: %0.4hx", (*csh).ecs);
 warn("\textended header flag: %x\n" , (*csh).efh >> 4);
 warn("vertical stack: %2x\n", (*csh).vs);
 warn("streamer cable number: %2x", (*csh).cab);
 warn("\tarray forming: %2x\n", (*csh).ary);
}

void info_ssh(sample_skew *ssh)
{
 warn("\n**********************************\n"
 	"segd_sample_skew_header (%2d bytes)\n"
 	"**********************************\n", sizeof(*ssh));
 warn("not decoded\n");
}

void info_ech(extended_header * ech)
{
 register int i;
 warn("\n**********************************\n"
 	"segd_extended_header (%2d bytes)\n"
 	"**********************************\n", sizeof(*ech));
 for (i=0; i<sizeof(*ech); i+=4) {
 	warn("%02x %02x %02x %02x\n", ech->dummy[i], ech->dummy[i+1], ech->dummy[i+2], ech->dummy[i+3]);
 }
}

void info_exh(external_header * exh)
{
	register int i;
	/*  unsigned char *ptr; */
	/*  ptr= (unsigned char *) &(exh->dummy[0]); */
	unsigned char *p;
	p = (unsigned char *) &(exh->dummy[0]);
	warn("\n*******************************\n"
	     "segd_external_header (%2d bytes)\n"
	     "*******************************\n", sizeof(*exh));
	for (i=0; i<sizeof(*exh); ) {
/* segdread.c:1978: warning: operation on `i' may be undefined */
/* 		warn("%02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x\n", */
/* 		ptr[i++], ptr[i++], ptr[i++], ptr[i++], ptr[i++], ptr[i++], ptr[i++], ptr[i++], */
/* 		ptr[i++], ptr[i++], ptr[i++], ptr[i++], ptr[i++], ptr[i++], ptr[i++], ptr[i++]); */
		warn("%02x%02x %02x%02x %02x%02x %02x%02x "
		     "%02x%02x %02x%02x %02x%02x %02x%02x\n",
		     p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7],
		     p[8], p[9], p[10], p[11], p[12], p[13], p[14], p[15]);
 }
}

void info_dth(dem_trace_header * dth)
{
 warn("\n********************************\n"
 	"segd_dem_trace_header (%2d bytes)\n"
 	"********************************\n", sizeof(*dth));
 warn("file number: %hx", (*dth).f);
 warn("\tscan type: %x, channel set: %x\n", (*dth).st, (*dth).cn);
 warn("trace_number: %02x%02x", (*dth).tn[0],(*dth).tn[1]);
 warn("\tfirst timing word: %x%x%X\n", (*dth).t[0], (*dth).t[1], (*dth).t[2]);
 warn("trace header extension: %x", (*dth).the);
 warn("\tsample skew: %x", (*dth).ss);
 warn("\ttrace edit: %x\n", (*dth).tr);
 warn("time break window: %02x%02x%02x\n", (*dth).tw[0], (*dth).tw[1], (*dth).tw[2]);
 warn("extended channel set number: %x%x", (*dth).en[0], (*dth).en[1]);
 warn("\textended file number: %x%x%x\n", (*dth).efn[0], (*dth).efn[1], (*dth).efn[2]);
}

void info_gt(general_trailer * gt)
{
 warn("\n********************************\n"
 	"segd_general_traler (%2d bytes)\n"
 	"********************************\n", sizeof(*gt));
 warn("General trailer number: %d", (*gt).gt);
 warn("\tchannel type identification: %01x\n", (*gt).c >> 4);
}
