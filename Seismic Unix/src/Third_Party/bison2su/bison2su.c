/************************************************************************/
/*
 * Modified:
 *	-1998/05/24 Anonymous
 *		-add many more error detection messages
 *		-use dynamic memory allocation for trace data to accomodate
 *			larger than previous 8192 size
 *		-retain all Bison channel headers in dynamically allocated array
 *		-convert input data to output data in place, no need for
 *			separate buffers
 *		-remove pow() call, use fast lookup map instead
 *		-change swap routines to work with arrays instead of single
 *			values
 *		-autodetect big/little endian, no need to give a swap flag
 *		-fix segy trace header fill for several variables going to the
 *			wrong location
 *		-fix the date in the segy output header and use the expected
 *			Day-Of-Year day in the segy trace header instead
 *			of the day of month
 */

/* program bison2su.c */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

/*  Jens Hartmann 21.6.93                              */
/*  Institut fuer Geophysik Hamburg                    */

typedef short		int16;
typedef unsigned short	uint16;
typedef int			int32;
typedef unsigned int	uint32;
typedef float		s_Data;



struct s_Trace_head {
    union {
      int32 d4[60];
      int16 s2[120];
    } H;
};
typedef struct s_Trace_head s_Trace_head;

/* Bison Header Struct */
struct s_Bison_Header { /* each header constant is followed by CR and LF */
    char f_num[8];      /* file number 6 digits */
    char j_num[14];     /* job number 6 digits */
    char date_time[12]; /* date and time mmddyyhhmm */
    char op_note[82];   /* operator note 80 alpha */
    char man_code[10];  /* manufacturer code BISON-2 */
    char rec_name[14];  /* record name 12 alpha */
    char head_type[4];  /* header type S/E (standard/extended) */
    char data_for[4];   /* data format W/L/F (word/long/float) */
    char ins_mod[8];    /* instrument model 6 alpha */
    char ins_soft[6];   /* instrument software 4 digits */
                        /* instrument digital processing history 12 alpha */
    char op1[14];       /* operation #1 12 alpha */
    char op2[14];       /* operation #2 12 alpha */
    char op3[14];       /* operation #3 12 alpha */
    char op4[14];       /* operation #4 12 alpha */
    char num_chan[6];   /* number of channels 4 digits always 1 for extended */
    char n_r[14];       /* 1-12 normal or revers 12 alpha */
    char num_sam[8];    /* number of samples 6 digits 000500-999999 */
    char sam_rate[6];   /* sample rate 4 alpha .001 to 99.9 msec */
    char delay_time[6]; /* delay time 4 digits 0000 to 9999 msec */
    char hi_pass[12];   /* hi-pass analog filter 4 digits and 6 alpha
			   examp 0150 BE 03 (150 Hz Bessel 3 pole) */
    char low_pass[12];  /* low-pass analog filter 4 digits and 6 alpha */
    char notch_fil[10]; /* analog notch filter 2 digits and 6 alpha */
    char shot_loc[82];  /* shot location 80 alpha */
    char shot_off[82];  /* shot offset 80 alpha */
    char geo_note[82];  /* geophone array note 80 alpha */
};
typedef struct s_Bison_Header s_Bison_Header;

struct s_Bison_Header_i { /* internal converted version */
	float		sample_rate;
	int		swap;
	int		debug;
	int		num_chan;
	int		num_sam;
	int		delay_time;
	int		rec_name;
	int		shot_off;
	int		year;
	int		month;
	int		day;
	int		hour;
	int		minute;
	int		doy;
	int		head_type;
	int		data_for;
};
typedef struct s_Bison_Header_i s_Bison_Header_i;

/* Bison Channel Header Struct */
struct s_Bison_Channel {
    char num_stack[6];  /* number of stacks chan l 4 digits */
    char fixed_gain[6]; /* fixed gain chan l 4 digits (db) */
    char pol_stat[6];   /* pol_stat_ ch l 4 alpha */
    char dc_low[6];     /* dc low */
    char dc_med[6];     /* dc med */
    char dc_high[6];    /* dc high */
    char ac_lm[6];      /* ac l-m */
    char ac_mh[6];      /* ac m-h */
};      /* channel specific section repeated for each channel */
typedef struct s_Bison_Channel s_Bison_Channel;



int	bigendian(void);
int	grgdoy(int,int,int);
void	swap_short_2(uint16*, int);
void	swap_long_4 (uint32*, int);
void	Bhead_to_head(s_Bison_Header*,s_Bison_Header_i*);
void	Bchan_to_thd(s_Bison_Channel*, s_Bison_Header_i*, s_Trace_head*);



int
main(int argc,char *argv[])
{
	s_Bison_Header	Bison_Header;
	s_Bison_Channel	Bison_Channel;

	s_Trace_head		*THDs;
	s_Trace_head		*thd;
	s_Bison_Header_i	BHi;
	s_Data		*Data;

	char		etx_test[2];     /* two test characters */

	int		swap	= 0;
	int		debug	= 0;
	int		bytes_trace_head;
	int		bytes_trace_data;
	int		i0, i1, i2;
	size_t	nio;
	size_t	nio_r;

	/* check native sizes and swap */
		if( sizeof(int16) != 2 )
		{
			fprintf(stderr,"ERROR sizeof(int16)=%d != 2\n",sizeof(int16));
			exit(1);
		}
		if( sizeof(int32) != 4 )
		{
			fprintf(stderr,"ERROR sizeof(int32)=%d != 4\n",sizeof(int32));
		}
		if( sizeof(float) != 4 )
		{
			fprintf(stderr,"ERROR sizeof(float)=%d != 4\n",sizeof(float));
		}

		swap = bigendian();

	for(i0=1 ; i0 < argc; i0++)
	{
		if (strcmp(argv[i0],"swap") == 0 || strcmp(argv[i0],"-swap") == 0)
			swap |= 2;
		else if (strcmp(argv[i0],"-d") == 0)
			debug=1;
	}
	BHi.swap	= swap;
	BHi.debug	= debug;

	/* report setup */
		if(swap != 0)
			fprintf(stderr,"bytes are swapped !\n");
		if((swap & 2) && !(swap & 1))
			fprintf(stderr,"no need to swap, swapping anyway!\n");

	/* read a header */
		nio = fread(&Bison_Header,sizeof(Bison_Header),1,stdin);
		if(nio != 1)
		{
			fprintf(stderr,"ERROR:reading Bison_Header\n");
			perror("fread");
			exit(1);
		}
		Bhead_to_head(&Bison_Header,&BHi);


	/* allocate space */
		THDs = (s_Trace_head*) calloc(sizeof(*THDs),BHi.num_chan);
		if(THDs == NULL)
		{
			fprintf(stderr,"ERROR:allocating input trace headers\n");
			exit(1);
		}

		nio = sizeof(s_Data) * BHi.num_sam;
		Data = (s_Data*) malloc(nio);
		if(Data == NULL)
		{
			fprintf(stderr,"ERROR:allocating input trace data\n");
			exit(1);
		}

	/* length of trace */
		bytes_trace_head = sizeof(s_Trace_head);
		bytes_trace_data = sizeof(s_Data) * BHi.num_sam;

	/* loop over the channel headers */
	for (i0=0;i0 < BHi.num_chan;i0++)
	{
		nio = fread(&Bison_Channel,sizeof(Bison_Channel),1,stdin);
		if(nio != 1)
		{
			fprintf(stderr,"ERROR: reading Bison_Channel\n");
			perror("fread");
			exit(1);
		}

		Bchan_to_thd(&Bison_Channel,&BHi, &THDs[i0]);

		fprintf(stderr,"Number of stacked traces :%s\n",
					&Bison_Channel.num_stack);
	}


	/* read terminating characters */
		nio = fread(&etx_test,1,2,stdin);
		if(nio != 2)
		{
			fprintf(stderr,"ERROR: reading etx_test"
					", expected 2, read %d\n", nio);
			perror("fread");
			exit(1);
		}
		if(etx_test[0] != 3 || etx_test[0] != 3)
		{
			fprintf(stderr,"ERROR: reading etx_test"
					", expected <3><3>, read <%x><%x>\n",
					etx_test[0],etx_test[1]);
		}

	/* read/write trace data */
		switch(BHi.data_for)
		{
			case 'W':
					nio_r = 2 * BHi.num_sam;
					break;
			case 'L':
					nio_r = 4 * BHi.num_sam;
					break;
			case 'F':
					nio_r = 2 * 5*((BHi.num_sam+3)/4);
					break;
			default:
					fprintf(stderr,"Error:invalid data type %s\n",
							BHi.data_for);
					exit(1);
		}

	for (i0=0;i0 < BHi.num_chan;i0++)
	{
		thd = &THDs[i0];

		thd->H.d4[1]	= i0+1;			/* tracr */
		/*thd->H.d4[3]	= i0+1;			/* cdpt */
		thd->H.d4[5]	= i0+1;			/* cdpt */

		nio = fread(Data,nio_r,1,stdin);
		if(nio != 1)
		{
			fprintf(stderr,"ERROR: reading data, trace %d\n",i0);
			perror("fread");
			exit(1);
		}

	/*
	 * Word 16 bit 2's complement fixed gain normalized channel by channel.
	 * Least significant byte is first inrecord.
	 */
		if (BHi.data_for == 'W')
		{
			int16	*v	= (int16*)Data;

			if (swap) swap_short_2((uint16*)Data,BHi.num_sam);

			/* convert to output data type */
			for (i1=BHi.num_sam;i1-- > 0;)
				Data[i1] = v[i1];
		}

	/*
	 * Long 32 bit 2's complement fixed gain instrumental format,
	 * not normalized, least significant byte first.
	 */
		else if (BHi.data_for == 'L')
		{
			int32	*v	= (int32*)Data;

			if (swap) swap_long_4((uint32*)Data,BHi.num_sam);

			/* convert to output data type */
			for (i1=0;i1 < BHi.num_sam;i1++)
				Data[i1] = v[i1];
		}

	/*
	 * Float 16 bit 2's complement mantissa, 4 bit exponent normalized float
	 * generated from 32 bit internal fixed format standard instrument storage
	 * format. Transmitted as follows: in groups of 5 words
	 * 
	 *	LSbyte of first sample mantissa
	 *	MSbyte of first sample mantissa
	 *	LSbyte of second sample mantissa
	 *	MSbyte of second sample mantissa
	 *	LSbyte of third sample mantissa
	 *	MSbyte of third sample mantissa
	 *	LSbyte of fourth sample mantissa
	 *	MSbyte of fourth sample mantissa
	 *
	 * Exponent word:
	 *	Least significant nibble of first byte for fourth word.
	 *	Most significant nibble of first byte for third word.
	 *	Least significant nibble of second byte for second word.
	 *	Most significant nibble of first byte for first word.
	 */
		else if (BHi.data_for == 'F')
		{
			struct di {
				int16		f0;
				int16		f1;
				int16		f2;
				int16		f3;
				uint16	gain;
			};
			struct di	*d		= (struct di*)Data;
			int		nblocks	= (BHi.num_sam + 3)/4;

			if (swap)
				swap_short_2((uint16*)Data,nio_r/2);

			d = &d[nblocks - 1];
			for (i1=BHi.num_sam - 1; i1 > 0; i1 -= 4, d--)
			{
				float emap[] = {
					1.,		2.,		4.,		8.,
					16.,		32.,		64.,		128.,
					256.,		512.,		1024.,	2048.,
					4096.,	8192.,	16384.,	32768.
						};
				unsigned int	gain = (unsigned)d->gain;

				
				Data[i1  ]	= d->f3 * emap[ (gain & 0x000f)    ];
				Data[i1-1]	= d->f2 * emap[((gain & 0x00f0)>>4)];
				Data[i1-2]	= d->f1 * emap[((gain & 0x0f00)>>8)];
				Data[i1-3]	= d->f0 * emap[((gain & 0xf000)>>12)];
			}
		}

		nio = fwrite(thd,bytes_trace_head,1,stdout);
		if(nio != 1)
		{
			fprintf(stderr,"ERROR:writing trace header %d\n",i0);
			perror("fwrite");
			exit(1);
		}
		nio = fwrite(Data,bytes_trace_data,1,stdout);
		if(nio != 1)
		{
			fprintf(stderr,"ERROR:writing trace data %d\n",i0);
			perror("fwrite");
			exit(1);
		}

    }

	free(THDs);
	free(Data);
	return 0;
}

/*****************************************************************************
 * convert date to gregorian day to day of year 1 <-> 365 or 366
 * 4 year, 100 year, 400 year rules observed
 */
int
grgdoy( int iyr, int imo, int ida)
{
#	define BIG 9999
	int mo[]={ 0, 31, 59, 90,120,151,181,212,243,273,304,334, BIG,
			0, 31, 60, 91,121,152,182,213,244,274,305,335, BIG };

	int leap;
	leap = ( !(iyr%4) && ( iyr%100 || !(iyr%400) ) )?13:0;
	return((int)(mo[imo+leap-1] + ida));
#	undef BIG
}
/*****************************************************************************
 * determine endianness of this machine
 */
int
bigendian(void)
{
		int32 a[2];
		char	*c;
		c = (char*)a;
		a[0] = 0x11223344;
		a[1] = 0x55667788;
		if( *c == 0x11 ) return 1;
		if( *c == 0x44 ) return 0;
		fprintf(stderr,"ERROR:cannot determine big/little endianness\n");
		exit(1);
}

/**************************************************************************
swap_short_2		swap short integers in place
***************************************************************************/
void swap_short_2(uint16 *tni2, int cnt)
{
	int		i	= cnt;
	uint16	*v	= (uint16*)tni2;
	for(i = cnt; i-- > 0 ; v++)
	{
		 /* *v=(((*v>>8) & 0xff) | ((*v & 0xff)<<8)); */
		 *v=( ((*v>>8) & 0xff) | ((*v << 8) & 0xff00) );
	}
}

/**************************************************************************
swap_long_4		swap a long integers in place
***************************************************************************/
void swap_long_4(uint32 *tni4, int cnt)
{
	int		i	= cnt;
	uint32	*v	= (uint32*)tni4;
	for(i = cnt; i-- > 0 ; v++)
	{
		 /* *v=(((*v>>24)&0xff) | ((*v&0xff)<<24) |
			    ((*v>>8)&0xff00) | ((*v&0xff00)<<8)); */
		 *v=(((*v>>24) & 0xff) | ((*v<<24) & 0xff000000) |
			    ((*v>>8) & 0xff00) | ((*v<<8) & 0xff0000));
	}
}


/*****************************************************************************
 * convert Bison header to internal representation
 */
void
Bhead_to_head(s_Bison_Header *bh,s_Bison_Header_i *ih)
{
	int	i;

	/* set EOL */
#	define BH_END(name)	bh->name[sizeof(bh->name)-2]=\
					bh->name[sizeof(bh->name)-1]='\0'
		BH_END(f_num);	BH_END(j_num);	BH_END(date_time);
		BH_END(op_note);	BH_END(man_code);	BH_END(rec_name);
		BH_END(head_type);BH_END(data_for);	BH_END(ins_mod);
		BH_END(ins_soft);
		BH_END(op1);	BH_END(op2);	BH_END(op3);	BH_END(op4);
		BH_END(num_chan);	BH_END(n_r);	BH_END(num_sam);
		BH_END(sam_rate);	BH_END(delay_time);
		BH_END(hi_pass);	BH_END(low_pass);	BH_END(notch_fil);
		BH_END(shot_loc);	BH_END(shot_off);	BH_END(geo_note);
#	undef BH_END

	ih->num_chan	= atoi(bh->num_chan);
	ih->num_sam		= atoi(bh->num_sam);
	ih->sample_rate	= atof(bh->sam_rate);
	ih->delay_time	= atoi(bh->delay_time);
	ih->rec_name	= atoi(&bh->rec_name[3]);
	ih->shot_off	= atol(bh->shot_off);

	ih->head_type	= (bh->head_type[0] == ' ') ?
					bh->head_type[1] : bh->head_type[0];
	ih->data_for	= (bh->data_for[0]  == ' ') ?
					bh->data_for[1]  : bh->data_for[0];

#	define EXTRACT_DT(val,pos) \
			i = bh->date_time[pos+2]; bh->date_time[pos+2] = '\0'; \
			ih->val = atoi(&bh->date_time[pos]); \
			bh->date_time[pos+2] = i;

		EXTRACT_DT(month,0);	EXTRACT_DT(day,2);	EXTRACT_DT(year,4);
		EXTRACT_DT(hour,6);	EXTRACT_DT(minute,8);
#	undef EXTRACT_DT

	ih->doy	= grgdoy(ih->year,ih->month,ih->day);

	fprintf(stderr,
		"Job number         :%s\n",		bh->j_num);
	fprintf(stderr,
		"Y/M/D H:M DOY      :%02d/%02d/%02d %02d:%02d %03d\n",
				ih->year,ih->month,ih->day,ih->hour,ih->minute,
				ih->doy);
	fprintf(stderr,
		"Manufacturer code  :%s\n",		bh->man_code);
	fprintf(stderr,
		"Record name        :%s\n",		bh->rec_name);
	fprintf(stderr,
		"Header type        :%c\n",		ih->head_type);
	fprintf(stderr,
		"Data format        :%c\n",		ih->data_for);
	fprintf(stderr,
		"Number of channels :%d\n",		ih->num_chan);
	fprintf(stderr,
		"Samples per channel:%d\n",		ih->num_sam);
	fprintf(stderr,
		"Sample rate (msec) :%f msec\n",
					ih->sample_rate);
	fprintf(stderr,
		"Delay              :%d\n",
					ih->delay_time);

	/* value check */
 	/* number of channels 4 digits always 1 for extended */
		if(ih->num_chan < 1 || ih->num_chan > 48)
		{
			fprintf(stderr,"ERROR:invalid num_chan:%d\n",ih->num_chan);
			exit(1);
		}
	/* number of samples 6 digits 000500-999999 */
		if(ih->num_sam < 500 || ih->num_sam > 16384)
		{
			fprintf(stderr,"ERROR:invalid sample count:%d\n",ih->num_sam);
			exit(1);
		}
	/* sample rate 4 alpha .001 to 99.9 msec */
		if(ih->sample_rate < 0.001 || ih->sample_rate > 99.9)
		{
			fprintf(stderr,"ERROR:invalid sample rate:%f\n",
						ih->sample_rate);
			exit(1);
		}
	/* delay time 4 digits 0000 to 9999 msec */
		if(ih->delay_time < 0 || ih->delay_time > 9999)
		{
			fprintf(stderr,"ERROR:invalid delay_time:%d\n",
						ih->delay_time);
			exit(1);
		}
	/* header type S/E (standard/extended) */
		if(strchr("SE",ih->head_type) == NULL)
		{
			fprintf(stderr,"ERROR:invalid header type:%c\n"
					" read from <%s>\n",
						ih->head_type,bh->head_type);
			exit(1);
		}
	/* data format W/L/F (word/long/float) */
		if(strchr("WLF",ih->data_for) == NULL)
		{
			fprintf(stderr,"ERROR:invalid data type:%c\n"
					" read from <%s>\n",
						ih->data_for,bh->data_for);
			exit(1);
		}
	/* date time, 2 digits each */
		if(
			ih->year   < 1 || ih->year   > 99
		||	ih->month  < 1 || ih->month  > 12
		||	ih->day    < 1 || ih->day    > 31
		||	ih->hour   < 0 || ih->hour   > 23
		||	ih->minute < 0 || ih->minute > 59
		) {
			fprintf(stderr,"ERROR:invalid date_time:\n"
					" y m d h m = %2d %2d %2d %2d %2d\n"
					" read from string <%s>\n",
				ih->month,ih->day, ih->year,
				ih->hour, ih->minute,
				bh->date_time);
			exit(1);
		}

}

/*****************************************************************************
 * convert Bison trace header to output header
 */
void
Bchan_to_thd(s_Bison_Channel *bc, s_Bison_Header_i *ih, s_Trace_head *th)
{
	/* set EOL */
#	define BC_END(name)	bc->name[sizeof(bc->name)-2]=\
					bc->name[sizeof(bc->name)-1]='\0'
		BC_END(num_stack);
		BC_END(fixed_gain);
		BC_END(pol_stat);
		BC_END(dc_low);	BC_END(dc_med);	BC_END(dc_high);
		BC_END(ac_lm);	BC_END(ac_mh);
#	undef BC_END

	th->H.d4[0]		= 1;						/* tracl */
	th->H.d4[2]		= ih->rec_name;				/* fldr */
	th->H.d4[4]		= 0;						/* ep */
	/*th->H.s2[16]	= 1;						/* trid */
	th->H.s2[14]	= 1;						/* trid */
	/*th->H.s2[17]	= atoi(bc->num_stack);			/* nvs */
	th->H.s2[15]	= atoi(bc->num_stack);			/* nvs */
	/*th->H.s2[19]	= 1;						/* duse */
	th->H.s2[17]	= 1;						/* duse */
	th->H.d4[9]		= ih->shot_off;				/* offset */
	th->H.s2[44]	= 1;						/* counit */
	th->H.s2[52]	= ih->delay_time;				/* delrt */
	th->H.s2[57]	= ih->num_sam;				/* ns */
	th->H.s2[58]	= floor(ih->sample_rate*1000);	/* dt */
	th->H.s2[78]	= ih->year;					/* year */
	th->H.s2[79]	= ih->doy;					/* day of year */
	th->H.s2[80]	= ih->hour;					/* hour */
	th->H.s2[81]	= ih->minute;				/* minute */
	th->H.s2[82]	= 0;						/* sec */
	th->H.s2[83]	= 1;						/* timbas */
}
/************************************************************************/
