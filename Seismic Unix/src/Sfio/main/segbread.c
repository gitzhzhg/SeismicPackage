/* Copyright (c) Colorado School of Mines, 2013.*/
/* All rights reserved.                       */

/* SUPLANE: $Revision: 1.3 $ ; $Date: 2010/01/26 21:20:56 $	*/


#include"su.h"
#include"segy.h"
#include"sfio.h"

#define HDR_L 16
#define NAUX 5

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SEGBREAD - read an SEG-B tape						",
"									",
" segbread > stdout tape=						",
"									",
"									",
" Required parameters:							",
"	tape=	   input tape device					",
" Optional parameters:							",
"									",
"	ns=		number of samples.This overrides the number	",
"			that is obtained from the file header		 ",
"			Usefull for variable trace length		",
"									",
"	auxf=0		1 output auxiliary channels			",
"	ntro=0		Number of traces per record.This overrides the	",
"			computed value (usefull for some DFS-V		",
"			instruments) if specified.			",
"									",
" ONLY READS DISK SEGB FILES! I tested it on files created by		",
" TransMedia Technologies Calgary Alberta, Canada			",
" In their format each data block is preceded by an eight byte header	",
" 2  unsigned 32 bit IBM format integer.				",
" First number is the block number, second is the length of block given	",
" in bytes.								",
"  (This program is largely untested. Testing reports on SEG B data	",
" and improvements to the code 									",
NULL};

/* 
 * Credits: Balasz Nemeth, Potash Corporation Saskatechwan
 * given to CWP in 2008
 * Based on SEGDREAD by Stew Levin of Landmark Graphics and others.
 */
/**************** end self doc ********************************/

static short GET_S(Sfio_t *f)
{
 int n1, n2;
 int n=0;
 short s;

 n1 = sfgetc(f);
 n2 = sfgetc(f);
 n = (n1<<8) | n2;
 s = (short) ((n > 32767)? n - 65536 : n);
 return (s);
}

struct segb_hdr
{
	/* information from the tape */
	int file_number;
	int data_id_const;
	int format_code;
	int N_bytes_scan;
	int s_dt;
	int manu_code;
	int eqp_ser;
	int record_length;
	int gain_c_mode;
	int record_type;
	int low_cut;
	int low_cut_slope;
	int zeros1;
	int high_cut;
	int high_cut_slope;
	int filter_spec;
	int filter_alias;
};
segy tr;

/* subroutine prototypes */
int bcd (unsigned char *ptr, int begin, int n) ;
float bit5_bin_to_float(unsigned char *s);
void get_hdr(unsigned char *data,struct segb_hdr *H);
void get_opt_hdr(int ntr,unsigned char *data, struct segb_hdr *H,float *pre_amp_gain,int *ch_id);
 void F0015_to_float (Sfio_t *from, float to[], int len);

int
main(int argc, char **argv) {

	char *tape=NULL;	/* name of raw tape device */
	int ntr;		/* number of traces */
	unsigned char *data;	/* data block */	
 	int tapefd=0;	   /* file descriptor for tape */
  	Sfio_t  *tapeun;	/* input for Sfio_t reads. May be memory or stdio */
 	Sfoff_t startpos=0;	/* for rewind on memory Sfio_t */
	
	
	/* flags */
	int verbose=1;
	int nread;
	int ioft=8;		/* initial offset at start of tape */
	int irec;
	int auxf;
	
	/* header */
	struct segb_hdr H;
	int opt_HDR_L;
	int nsc;		/* number of scans, number of samples */
	int ns;
	unsigned long disk_block_size[2];
	
	float *pre_amp_gain;
	int *ch_id;
	float *aux_amp_gain;
	int *aux_ch_id;
	float **rec;
	float **aux;
	
	int ntro;		/* override computed numeber of traces */
	
	
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
	
	MUSTGETPARSTRING("tape",  &tape);
	if (!getparint("ns", &ns))	ns = 0;
	if (!getparint("ntro", &ntro))	ntro = 0;
	if (!getparint("auxf", &auxf))	auxf = 0;
	if (!getparint("verbose", &verbose))	verbose = 0;

	
	/* Open the tape */
	if ( STREQ(tape,"-") ) {
	   	tapefd = fileno(stdin);
	} else {
		tapefd = eopen(tape, O_RDONLY, 0444);
	}
	
	if (verbose) fprintf(stderr,"Tape opened successfully %d\n",tapefd);
	
	
	/* Repeat till the end of file */
	for(irec=0;;irec++) {
	
	
		/* Allocate memory for header */
		data = ealloc1(SU_NFLTS, sizeof(unsigned char));
	
		if (-1 == (nread = (int) read(tapefd, (void *) disk_block_size, (size_t) ioft))) {
			err(" Tape/stream error 1; or end of file");
		}
		
		/* First block size */
		swap_u_long_4(&disk_block_size[0]);
		swap_u_long_4(&disk_block_size[1]);
		fprintf(stderr," Block Number  %lu",disk_block_size[0]);
		fprintf(stderr," Size of block %lu\n",disk_block_size[1]);
		
		/* End of file if block size is zero */
		if(!disk_block_size[1])
			break;
	
		/* Read first block */
		if (-1 == (nread = (int) read(tapefd, (void *) data, (size_t) disk_block_size[1]))) {
			err(" Tape/stream error 2");
		}
	
		/* process header */
		get_hdr(&data[0],&H);
		if(verbose) {
			fprintf(stderr," File number %d\n",H.file_number);
			fprintf(stderr," Number of bytes per scan from header %d\n",H.N_bytes_scan);
			
		}	
		
		/* Compute the number of traces */
		/* ((Number_of_bytes_p_scan - 4 )/2 - 5)/5 * 4) */
		/* 4 bytes for sync
		   5 auxiliary channels per scan 1 word (2 bytes) each 
		   5 word per scan for seismic ( gain +  4 traces) 1 word (2 bytes) each */  
		ntr = ((H.N_bytes_scan-4)/2 - NAUX)/NAUX*4;
		fprintf(stderr," Number of traces = %d \n", ntr);
		if(ntro) {
			fprintf(stderr," Number of traces override to = %d \n", ntro);
			H.N_bytes_scan=4+5*2+ntro/4*10;
			fprintf(stderr," New number of bytes per scan = %d \n", H.N_bytes_scan);
			ntr=ntro;
		}
		
		
		/* allocate gain array */
		pre_amp_gain = ealloc1float(ntr);
		ch_id = ealloc1int(ntr);
		aux_amp_gain = ealloc1float(5);
		aux_ch_id = ealloc1int(5);
		
		/* Compute optional header length */
		/* 8 bytes + ( number of traces + 5 aux) *2 bytes */ 
		opt_HDR_L= 8 + ntr*2 + NAUX*2;
		get_opt_hdr(ntr,&data[HDR_L],&H,pre_amp_gain,ch_id);
		free1(data);
	
		/* Read scans */
		/* number of scans = record length/ sample intervall */
		
		/* Read the size of second block */
		if (-1 == (nread = (int) read(tapefd, (void *) disk_block_size, (size_t) ioft))) {
			err(" Tape/stream error 4");
		}
		swap_u_long_4(&disk_block_size[0]);
		swap_u_long_4(&disk_block_size[1]);
		fprintf(stderr," Block Number  %lu",disk_block_size[0]);
		fprintf(stderr," Size of block %lu\n",disk_block_size[1]);
		
		
		/* Computed number of samples */
		nsc=(int)(disk_block_size[1]/H.N_bytes_scan);
		fprintf(stderr," Record length from header %d\n Sample interval (ms) %d\n Computed trace length (samples) %d\n",
			 H.record_length,H.s_dt,nsc);
		
		data = ealloc1(disk_block_size[1],sizeof(char));	/* the rest of the record= number of scans*byte per scan */
		rec = ealloc2float(ntr,nsc); 				/* transposed matrix because of multiplexed data */
		aux = ealloc2float(NAUX,nsc);
	
		/* Read the rest of the record */
		if (-1 == (nread = (int) read(tapefd, (void *) &data[0], (size_t) disk_block_size[1] ))) {
			err(" Tape/stream error");
		}
				
		/* Process the data */
		/* for every scan */
		{ int isc;
		  int seis_size=H.N_bytes_scan-4,scan_start,iaux;
			
			for(isc=0,scan_start=0;isc<nsc;isc++,scan_start+=H.N_bytes_scan) {
				
				/* read Aux channels */
				for(iaux=0;iaux<NAUX;iaux++) {
					aux[iaux][isc]=bcd(&data[scan_start+4+iaux*2],0,4);
				}
				
				/* Create Sfio_t input stream for seis data read */
				/* skip sync header */
				tapeun = sfnew((Sfio_t *) 0, (Void_t *) &data[scan_start+4+NAUX*2], (size_t)seis_size, 0, SF_STRING|SF_READ);
				startpos = sftell(tapeun);
				/* reset Sfio_t pointer to start of block */
				sfseek(tapeun,startpos,SEEK_SET);
				
				
				/* do the conversion */
				F0015_to_float(tapeun,rec[isc],ntr);
				
				sfclose(tapeun);
			}
		}
		free1(data);
		
		/* output one record */
		{int it,itr;
			
			tr.dt=H.s_dt*1000;
			tr.lcf=H.low_cut;
			tr.lcs=H.low_cut_slope*6;
			tr.hcf=H.high_cut;
			tr.hcs=H.high_cut_slope*6;
			if(!ns) {
				tr.ns=nsc;
			} else {
				tr.ns=ns;
			}
			tr.fldr=H.file_number;
			tr.ep=irec;
			
			if(auxf) {
				tr.trid=4;
				for(itr=0;itr<NAUX;itr++) {
					for(it=0;it<nsc;it++)
						tr.data[it] = aux[it][itr];
					tr.tracf=itr+1;
					puttr(&tr);
				}
			}
			
			for(itr=0;itr<ntr;itr++) {
				tr.trid=1;
				tr.igc=pre_amp_gain[itr]*4;
				for(it=0;it<nsc;it++)
					tr.data[it] = rec[it][itr]/(pre_amp_gain[itr]*4);
				tr.tracf=itr+1;
				puttr(&tr);
			}
		}
		
		free2float(rec);
		free2float(aux);
		free1float(pre_amp_gain);
		free1int(ch_id);
		free1float(aux_amp_gain);
		free1int(aux_ch_id);
		
		/* Read record trailer EOF */
		if (-1 == (nread = (int) read(tapefd, (void *) disk_block_size, (size_t) ioft))) {
			err(" Tape/stream error EOF; or end of file");
		}
		swap_u_long_4(&disk_block_size[0]);
		swap_u_long_4(&disk_block_size[1]);
		fprintf(stderr," EOF  %lu",disk_block_size[0]);
		fprintf(stderr," EOF  %lu\n",disk_block_size[1]);

	}	
	
	return EXIT_SUCCESS;

}

/* bcd - convert bcd to int
 *
 * Credits:
 *	EOPG: Marc, Jdt
 *
 * Parameters:
 *	ptr	- address of first byte of the number
 *	begin  - 0 or 1, position of the first digit of the number
 *	n	- number of digits
 *
 */
int bcd (unsigned char * ptr , int begin , int n)
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

void get_hdr(unsigned char *data,struct segb_hdr *H)
{
	H->file_number	 = bcd(&data[0],0,4);
	H->format_code	 = bcd(&data[2],0,4);
	H->data_id_const	= bcd(&data[4],0,12);
	H->N_bytes_scan	= bcd(&data[10],0,3);
	H->s_dt		= bcd(&data[11],1,1);
	H->manu_code	   = bcd(&data[12],0,2);
	H->eqp_ser		= bcd(&data[13],0,6);
	
}
void get_opt_hdr(int ntr,unsigned char *data, struct segb_hdr *H,float *pre_amp_gain,int *ch_id)
{
	H->record_length	= bcd(&data[0],0,2);
	H->gain_c_mode	 = bcd(&data[1],0,1);
	H->record_type	 = bcd(&data[1],1,1);
	H->low_cut		= bcd(&data[2],0,2);
	H->low_cut_slope	= bcd(&data[3],0,1);
	H->high_cut		= bcd(&data[4],0,3);
	H->high_cut_slope	= bcd(&data[5],1,1);
	H->filter_spec	 = bcd(&data[6],0,2);
	H->filter_alias	= bcd(&data[7],0,1);
	
	{ int i,ic;
	  float g1;
	  float g2;
	  unsigned char s;
		for( i=0,ic=0; i<ntr; i++,ic+=2) {
			s = data[8+ic];
			s = (s<<5)>>5;
			ch_id[i] = bcd(&s,0,1);
			g1 = bit5_bin_to_float(&data[8+ic]);
			g2 = bit5_bin_to_float(&data[9+ic]);
			pre_amp_gain[i]=g1;
		}
		for( i=0,ic=ntr*2; i<NAUX; i++,ic+=2) {
			s = data[8+ic];
			s = (s<<5)>>5;
			ch_id[i] = bcd(&s,0,1);
			g1 = bit5_bin_to_float(&data[8+ic]);
			g2 = bit5_bin_to_float(&data[9+ic]);
		}
	}
}

float bit5_bin_to_float(unsigned char *s)
/* convert a 5 bit binary to float 
Bit  0   1   2   3   4   5   6   7
				 
exp  x   x   x   4   3   2   1   0
	0   0   0  2   2   2   2   2

This is according to SEG specification
*/

{
	float f;
	
	f =(float)((*s&128)/128);
	f+=(float)((*s& 64)/32);
	f+=(float)((*s& 32)/8);
	f+=(float)((*s& 16)/2);
	f+=(float)((*s&  8));

	return(f);

}


 void F0015_to_float (Sfio_t *from, float to[], int len)
/* F0015_to_float - convert 20 bit binary multiplexed data into floating numbers
 *
 * Credits:
 *	EOPG: Marc Schaming, Jean-Daniel Tissot
 *	SEP:  Stew Levin - fixed low-order bit error in conversion
 *		of negative values on 2's complement machines.
 *		Use ldexp() function instead of much slower value*pow(2,expo)
 *	SEP:  Adapted F8015 to F0015 conversion
 *
 *
 * Parameters:
 *	from   - input vector
 *	to	- output vector
 *	len	- number of packets of 4 floats in vectors
 *
 */
/*
 *
 * Format 0015 is a 10 byte per 4 words (2 1/2 bytes per word)
 * representation.  According to the SEG specifications, the
 * bit layout of the 10 bytes is:
 *
 *
 *  Bit	0	1	2	3	4	5	6	7
 *-----------------------------------------------------------
 * Byte 1	C3	C2	C1	C0	C3	C2	C1	C0	Exponents for
 * Byte 2	C3	C2	C1	C0	C3	C2	C1	C0	channels 1 thru 4
 *
 * Byte 3	S	Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 1
 * Byte 4	Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14   0
 * Byte 5	S	Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 2
 * Byte 6	Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14   0
 * Byte 7	S	Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 3
 * Byte 8	Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14   0
 * Byte 9	S	Q-1   Q-2   Q-3   Q-4   Q-5   Q-6   Q-7   Channel 4
 * Byte 10   Q-8   Q-9   Q-10  Q-11  Q-12  Q-13  Q-14   0
 *
 * S=sign bit. - (One = negative number)
 * C=binary exponents. - This is a 4 bit positive binary exponent of 2
 *		CCCC
 *   written as 2	where CCCC can assume values of 0-15.  The four
 *   exponents are in channel number order for the four channels starting
 *   with channel one in bits 0-3 of Byte 1.
 * Q1-14-fraction. - This is a 14 bit one's complement binary fraction.
 *   The radix point is to the left of the most significant bit (Q-1)
 *				  -1
 *   with the MSB being defined as 2 .  The sign and fraction can assume
 *		  -14	-14
 *   values from 1-2   to -1+2  .  Note that bit 7 of the second byte
 *   of each sample must be zero in order to guarantee the uniqueness of
 *   the start of scan.  Negative zero is invalid and must be converted
 *   to positive zero.
 *					CCCC	MP		   MP
 * Input signal = S.QQQQ,QQQQ,QQQQ,QQ x 2	x 2	millivolts where 2
 *   is the value required to descale the data word to the recording
 *   system input level.  MP is defined in Byte 8 of each of the corre-
 *   sponding channel set descriptors in the scan type header.
 * Note that in utilizing this data recording method, the number of data
 *   channels per channel set must be exactly divisible by 4 in order to
 *   preserve the data grouping of this method.
 */
 
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
 	*(to++) = ldexp((double) fraction, -expo);

 	expo = ((ex1_4 >> 8) & 0x0F) - 15;
	fraction = GET_S(from);
	if (fraction < 0) fraction = -((~fraction)&(~1));
 	*(to++) = ldexp((double) fraction, -expo);

 	expo = ((ex1_4 >> 4) & 0x0F) - 15;
	fraction = GET_S(from);
	if (fraction < 0) fraction = -((~fraction)&(~1));
 	*(to++) = ldexp((double) fraction, -expo);

 	expo = (ex1_4 & 0x0F) - 15;
	fraction = GET_S(from);
	if (fraction < 0) fraction = -((~fraction)&(~1));
 	*(to++) = ldexp((double) fraction, -expo);
 }
} 
