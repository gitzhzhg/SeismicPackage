/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* cwpsegy.h - include file for SEGY traces
 *
 * declarations for:
 * typedef struct {} cwpsegy - the trace identification header
 * typedef struct {} cwpbhed - binary header
*             typedef struct {} sumap - map between SU and ProMAX headers
 *
 * Note:
 * If header words are added, run the makefile in this directory
 * to recreate hdr.h.
 *
 * Reference:
 * K. M. Barry, D. A. Cavers and C. W. Kneale, "Special Report:
 *    Recommended Standards for Digital Tape Formats",
 *    Geophysics, vol. 40, no. 2 (April 1975), P. 344-352.
 * 
 */ 

#ifndef HEADER_H
#define HEADER_H

#define SU_NKEYS        78      /* Number of key header words */
#define HDRBYTES        240     /* Bytes in the trace header */
#define MAXSEGY         131312

#endif

#ifndef SEGY_H
#define SEGY_H

#define SU_NFLTS  32768 /* Arbitrary limit on data array size  */

/* TYPEDEFS */
typedef struct {	/* segy - trace identification header */

	int tracl;	/* trace sequence number within line */

	int tracr;	/* trace sequence number within reel */

	int fldr;	/* field record number */

	int tracf;	/* trace number within field record */

	int ep;	/* energy source point number */

	int cdp;	/* CDP ensemble number */

	int cdpt;	/* trace number within CDP ensemble */

	short trid;	/* trace identification code:
			1 = seismic data
			2 = dead
			3 = dummy
			4 = time break
			5 = uphole
			6 = sweep
			7 = timing
			8 = water break
			9---, N = optional use (N = 32,767)

			Following are CWP id flags:

			 9 = autocorrelation

			10 = Fourier transformed - no packing
			     xr[0],xi[0], ..., xr[N-1],xi[N-1]

			11 = Fourier transformed - unpacked Nyquist
			     xr[0],xi[0],...,xr[N/2],xi[N/2]

			12 = Fourier transformed - packed Nyquist
	 		     even N:
			     xr[0],xr[N/2],xr[1],xi[1], ...,
				xr[N/2 -1],xi[N/2 -1]
				(note the exceptional second entry)
			     odd N:
			     xr[0],xr[(N-1)/2],xr[1],xi[1], ...,
				xr[(N-1)/2 -1],xi[(N-1)/2 -1],xi[(N-1)/2]
				(note the exceptional second & last entries)

			13 = Complex signal in the time domain
			     xr[0],xi[0], ..., xr[N-1],xi[N-1]

			14 = Fourier transformed - amplitude/phase
			     a[0],p[0], ..., a[N-1],p[N-1]

			15 = Complex time signal - amplitude/phase
			     a[0],p[0], ..., a[N-1],p[N-1]

			16 = Real part of complex trace from 0 to Nyquist

			17 = Imag part of complex trace from 0 to Nyquist

			18 = Amplitude of complex trace from 0 to Nyquist

			19 = Phase of complex trace from 0 to Nyquist

			21 = Wavenumber time domain (k-t)

			22 = Wavenumber frequency (k-omega)

			23 = Envelope of the complex time trace

			24 = Phase of the complex time trace

			25 = Frequency of the complex time trace

			30 = Depth-Range (z-x) traces

			43 = Seismic Data, Vertical Component 

			44 = Seismic Data, Horizontal Component 1 

			45 = Seismic Data, Horizontal Component 2 

			46 = Seismic Data, Radial Component

			47 = Seismic Data, Transverse Component  

			101 = Seismic data packed to bytes (by supack1)
			
			102 = Seismic data packed to 2 bytes (by supack2)
			*/

	short nvs;	/* number of vertically summed traces (see vscode
			   in bhed structure) */

	short nhs;	/* number of horizontally summed traces (see vscode
			   in bhed structure) */

	short duse;	/* data use:
				1 = production
				2 = test */

	int offset;	/* distance from source point to receiver
			   group (negative if opposite to direction
			   in which the line was shot) */

	int gelev;	/* receiver group elevation from sea level
			   (above sea level is positive) */

	int selev;	/* source elevation from sea level
			   (above sea level is positive) */

	int sdepth;	/* source depth (positive) */

	int gdel;	/* datum elevation at receiver group */

	int sdel;	/* datum elevation at source */

	int swdep;	/* water depth at source */

	int gwdep;	/* water depth at receiver group */

	short scalel;	/* scale factor for previous 7 entries
			   with value plus or minus 10 to the
			   power 0, 1, 2, 3, or 4 (if positive,
			   multiply, if negative divide) */

	short scalco;	/* scale factor for next 4 entries
			   with value plus or minus 10 to the
			   power 0, 1, 2, 3, or 4 (if positive,
			   multiply, if negative divide) */

	int  sx;	/* X source coordinate */

	int  sy;	/* Y source coordinate */

	int  gx;	/* X group coordinate */

	int  gy;	/* Y group coordinate */

	short counit;	/* coordinate units code:
				for previous four entries
				1 = length (meters or feet)
				2 = seconds of arc (in this case, the
				X values are longitude and the Y values
				are latitude, a positive value designates
				the number of seconds east of Greenwich
				or north of the equator */

	short wevel;	/* weathering velocity */

	short swevel;	/* subweathering velocity */

	short sut;	/* uphole time at source */

	short gut;	/* uphole time at receiver group */

	short sstat;	/* source static correction */

	short gstat;	/* group static correction */

	short tstat;	/* total static applied */

	short laga;	/* lag time A, time in ms between end of 240-
			   byte trace identification header and time
			   break, positive if time break occurs after
			   end of header, time break is defined as
			   the initiation pulse which maybe recorded
			   on an auxiliary trace or as otherwise
			   specified by the recording system */

	short lagb;	/* lag time B, time in ms between the time break
			   and the initiation time of the energy source,
			   may be positive or negative */

	short delrt;	/* delay recording time, time in ms between
			   initiation time of energy source and time
			   when recording of data samples begins
			   (for deep water work if recording does not
			   start at zero time) */

	short muts;	/* mute time--start */

	short mute;	/* mute time--end */

	unsigned short ns;	/* number of samples in this trace */

	unsigned short dt;	/* sample interval; in micro-seconds */

	short gain;	/* gain type of field instruments code:
				1 = fixed
				2 = binary
				3 = floating point
				4 ---- N = optional use */

	short igc;	/* instrument gain constant */

	short igi;	/* instrument early or initial gain */

	short corr;	/* correlated:
				1 = no
				2 = yes */

	short sfs;	/* sweep frequency at start */

	short sfe;	/* sweep frequency at end */

	short slen;	/* sweep length in ms */

	short styp;	/* sweep type code:
				1 = linear
				2 = cos-squared
				3 = other */

	short stas;	/* sweep trace length at start in ms */

	short stae;	/* sweep trace length at end in ms */

	short tatyp;	/* taper type: 1=linear, 2=cos^2, 3=other */

	short afilf;	/* alias filter frequency if used */

	short afils;	/* alias filter slope */

	short nofilf;	/* notch filter frequency if used */

	short nofils;	/* notch filter slope */

	short lcf;	/* low cut frequency if used */

	short hcf;	/* high cut frequncy if used */

	short lcs;	/* low cut slope */

	short hcs;	/* high cut slope */

	short year;	/* year data recorded */

	short day;	/* day of year */

	short hour;	/* hour of day (24 hour clock) */

	short minute;	/* minute of hour */

	short sec;	/* second of minute */

	short timbas;	/* time basis code:
				1 = local
				2 = GMT
				3 = other */

	short trwf;	/* trace weighting factor, defined as 1/2^N
			   volts for the least sigificant bit */

	short grnors;	/* geophone group number of roll switch
			   position one */

	short grnofr;	/* geophone group number of trace one within
			   original field record */

	short grnlof;	/* geophone group number of last trace within
			   original field record */

	short gaps;	/* gap size (total number of groups dropped) */

	short otrav;	/* overtravel taper code:
				1 = down (or behind)
				2 = up (or ahead) */

	/* local assignments */
	float d1;	/* sample spacing for non-seismic data */

	float f1;	/* first sample location for non-seismic data */

	float d2;	/* sample spacing between traces */

	float f2;	/* first trace location */

	float ungpow;	/* negative of power used for dynamic
			   range compression */

	float unscale;	/* reciprocal of scaling factor to normalize
			   range */

	short mark;	/* mark selected traces */

        short mutb;     /* start of end mute */

        float dz;       /* depth sampling interval (0.0 for time data */

        float fz;       /* depth of first sample */

        short n2;       /* number of traces per cdp or shot */

        short ens_end;  /* last trace in ensemble flag */

	int ntr; 	/* number of traces */

        int cdp_x;      /* CDP X coordinate */

        int cdp_y;      /* CDP Y coordinate */

        int aoffset;    /* absolute value of offset */

        float amp_norm; /* average sample amplitude normalization */

        float data[SU_NFLTS];

} cwpsegy;


typedef struct {	/* bhed - binary header */

	int jobid;	/* job identification number */

	int lino;	/* line number (only one line per reel) */

	int reno;	/* reel number */

	short ntrpr;	/* number of data traces per record */

        short nart;	/* number of auxiliary traces per record */

	unsigned short hdt; /* sample interval in micro secs for this reel */

	unsigned short dto; /* same for original field recording */

	unsigned short hns; /* number of samples per trace for this reel */

	unsigned short nso; /* same for original field recording */

	short format;	/* data sample format code:
				1 = floating point (4 bytes)
				2 = fixed point (4 bytes)
				3 = fixed point (2 bytes)
				4 = fixed point w/gain code (4 bytes) */

	short fold;	/* CDP fold expected per CDP ensemble */

	short tsort;	/* trace sorting code: 
				1 = as recorded (no sorting)
				2 = CDP ensemble
				3 = single fold continuous profile
				4 = horizontally stacked */

	short vscode;	/* vertical sum code:
				1 = no sum
				2 = two sum ...
				N = N sum (N = 32,767) */

	short hsfs;	/* sweep frequency at start */

	short hsfe;	/* sweep frequency at end */

	short hslen;	/* sweep length (ms) */

	short hstyp;	/* sweep type code:
				1 = linear
				2 = parabolic
				3 = exponential
				4 = other */

	short schn;	/* trace number of sweep channel */

	short hstas;	/* sweep trace taper length at start if
			   tapered (the taper starts at zero time
			   and is effective for this length) */

	short hstae;	/* sweep trace taper length at end (the ending
			   taper starts at sweep length minus the taper
			   length at end) */

	short htatyp;	/* sweep trace taper type code:
				1 = linear
				2 = cos-squared
				3 = other */

	short hcorr;	/* correlated data traces code:
				1 = no
				2 = yes */

	short bgrcv;	/* binary gain recovered code:
				1 = yes
				2 = no */

	short rcvm;	/* amplitude recovery method code:
				1 = none
				2 = spherical divergence
				3 = AGC
				4 = other */

	short mfeet;	/* measurement system code:
				1 = meters
				2 = feet */

	short polyt;	/* impulse signal polarity code:
				1 = increase in pressure or upward
				    geophone case movement gives
				    negative number on tape
				2 = increase in pressure or upward
				    geophone case movement gives
				    positive number on tape */

	short vpol;	/* vibratory polarity code:
				code	seismic signal lags pilot by
				1	337.5 to  22.5 degrees
				2	 22.5 to  67.5 degrees
				3	 67.5 to 112.5 degrees
				4	112.5 to 157.5 degrees
				5	157.5 to 202.5 degrees
				6	202.5 to 247.5 degrees
				7	247.5 to 292.5 degrees
				8	293.5 to 337.5 degrees */

	short hunass[170];	/* unassigned */

} cwpbhed;

/* DEFINES */
#define gettr(x)	fgettr(stdin, (x))
#define vgettr(x)	fvgettr(stdin, (x))
#define puttr(x)	fputtr(stdout, (x))
#define gettra(x, y)    fgettra(stdin, (x), (y))

/* The following refer to the trid field in segy.h		*/
/* CHARPACK represents byte packed seismic data from supack1	*/
#define		CHARPACK	101
/* SHORTPACK represents 2 byte packed seismic data from supack2	*/
#define		SHORTPACK	102

/* TREAL represents real time traces 				*/
#define		TREAL		1
/* TDEAD represents dead time traces 				*/
#define		TDEAD		2
/* TDUMMY represents dummy time traces 				*/
#define		TDUMMY		3
/* TBREAK represents time break traces 				*/
#define		TBREAK		4
/* UPHOLE represents uphole traces 				*/
#define		UPHOLE		5
/* SWEEP represents sweep traces 				*/
#define		SWEEP		6
/* TIMING represents timing traces 				*/
#define		TIMING		7
/* WBREAK represents timing traces 				*/
#define		WBREAK		8

/* TCMPLX represents complex time traces 			*/
#define		TCMPLX		13
/* TAMPH represents time domain data in amplitude/phase form	*/
#define		TAMPH		15
/* FPACK represents packed frequency domain data 		*/
#define		FPACK		12
/* FUNPACKNYQ represents complex frequency domain data 		*/
#define		FUNPACKNYQ	11
/* FCMPLX represents complex frequency domain data 		*/
#define		FCMPLX		10
/* FAMPH represents freq domain data in amplitude/phase form	*/
#define		FAMPH		14
/* REALPART represents the real part of a trace to Nyquist	*/
#define		REALPART	16
/* IMAGPART represents the real part of a trace to Nyquist	*/
#define		IMAGPART	17
/* AMPLITUDE represents the amplitude of a trace to Nyquist	*/
#define		AMPLITUDE	18
/* PHASE represents the phase of a trace to Nyquist		*/
#define		PHASE		19
/* KT represents wavenumber-time domain data 			*/
#define		KT		21
/* KOMEGA represents wavenumber-frequency domain data		*/
#define		KOMEGA		22
/* ENVELOPE represents the envelope of the complex time trace	*/
#define		ENVELOPE	23
/* INSTPHASE represents the phase of the complex time trace	*/
#define		INSTPHASE	24
/* INSTFREQ represents the frequency of the complex time trace	*/
#define		INSTFREQ	25
/* DEPTH represents traces in depth-range (z-x)			*/
#define		TRID_DEPTH	30
/* 3C data...  v,h1,h2=(11,12,13)+32 so a bitmask will convert  */
/* between conventions */
/* TVERT represents the vertical component */
#define     TVERT          43
/* TVHOZ1 represents the horizontal-1 component */
#define     THORZ1         44
/* TVHOZ2 represents the horizontal-2 component */
#define     THORZ2         45
/* TRADIAL represents the radial component */
#define     TRADIAL        46
/* TTRANS represents the transverse component */
#define     TTRANS         47

/* #define ISSEISMIC(id) (( (id)==0 || (id)==TREAL || (id)==TDEAD || (id)==TDUMMY ) ? cwp_true : cwp_false ) */
#define ISSEISMIC(id) (( (id)==0 || (id)==TREAL || (id)==TDEAD || (id)==TDUMMY || \
						 (id)==TVERT || (id)==THORZ1 || (id)==THORZ2 || \
						 (id)==TRADIAL || (id)==TTRANS  ) ? cwp_true : cwp_false ) 

#if 0
/* FUNCTION PROTOTYPES */
#ifdef __cplusplus /* if C++, specify external linkage to C functions */
extern "C" {
#endif

int fgettr(FILE *fp, cwpsegy *tp);
int fvgettr(FILE *fp, cwpsegy *tp);
void fputtr(FILE *fp, cwpsegy *tp);
int fgettra(FILE *fp, cwpsegy *tp, int itr);

/* hdrpkge */
void gethval(const cwpsegy *tp, int index, Value *valp);
void puthval(cwpsegy *tp, int index, Value *valp);
void getbhval(const bhed *bhp, int index, Value *valp);
void putbhval(bhed *bhp, int index, Value *valp);
void gethdval(const cwpsegy *tp, char *key, Value *valp);
void puthdval(cwpsegy *tp, char *key, Value *valp);
char *hdtype(const char *key);
char *getkey(const int index);
int getindex(const char *key);
void swaphval(cwpsegy *tp, int index);
void swapbhval(bhed *bhp, int index);
void printheader(const cwpsegy *tp);

void tabplot(cwpsegy *tp, int itmin, int itmax);

#ifdef __cplusplus /* if C++, end external linkage specification */
}
#endif

#endif
#endif


/*====================================================================*\

   sumap.h defines the mapping of SU header items to and from ProMax  

   Original version by John E. Anderson CWP/Mobil 1993

   Modified to fit on the page better and to allow dynamically
   calculating the number of entries in the table in suread and
   suwrite to ensure that adding a new header entry doesn't lead to
   an obscure bug in a future release.

   Reginald H. Beardsley                            rhb@acm.org

\*====================================================================*/


#define I2 0   /* 2 byte signed integer value   */
#define I4 1   /* 4 byte signed integer value   */
#define R4 2   /* 4 byte floating point value   */
#define U2 3   /* 2 byte unsigned integer value */

typedef struct {
   char* sukey;     /* CWP/SU header definition */
   char  sutype;   
   int   suoffs; 
   char* pkey;      /* ProMAX header definition */
   char  ptype; 
   char* desc;
}SU_HDR_MAP;

static SU_HDR_MAP sumap[] = {

    { "tracl"    ,I4   ,0 
     ,"TRACENO " ,I4 ,"Trace number in seismic line    " }

   ,{ "tracr"    ,I4   ,4       
     ,"SIN     " ,I4 ,"Source index number (internal)  " }

   ,{ "fldr"     ,I4   ,8 
     ,"FFID    " ,I4 ,"Field file ID number            " }

   ,{ "tracf"    ,I4  ,12 
     ,"CHAN    " ,I4 ,"Recording channel number        " }

   ,{ "ep"       ,I4  ,16 
     ,"SOURCE  " ,I4 ,"Live source number(usr-defined) " }

   ,{ "cdp"      ,I4  ,20 
     ,"CDP     " ,I4 ,"CDP bin number                  " }

   ,{ "cdpt"     ,I4  ,24 
     ,"SEQNO   " ,I4 ,"Sequence number in ensemble     " }

   ,{ "trid"     ,I2  ,28 
     ,"TRC_TYPE" ,I4 ,"Trace type (data ,aux ,etc.)    " }

   ,{ "nvs"      ,I2  ,30 
     ,"SU_NVS  " ,I4 ,"SU header: number vert stack    " }

   ,{ "nhs"      ,I2  ,32 
     ,"TR_FOLD " ,R4 ,"Actual trace fold               " }

   ,{ "duse"     ,I2  ,34 
     ,"SU_DUSE " ,I4 ,"SU header: data use: 1=prod     " }

   ,{ "offset"   ,I4  ,36 
     ,"OFFSET  " ,R4 ,"Signed source-receiver offset   " }

   ,{ "gelev"    ,I4  ,40 
     ,"REC_ELEV" ,R4 ,"Receiver elevation              " }

   ,{ "selev"    ,I4  ,44 
     ,"SOU_ELEV" ,R4 ,"Source elevation                " }

   ,{ "sdepth"   ,I4  ,48 
     ,"DEPTH   " ,R4 ,"Source depth                    " }

   ,{ "gdel"     ,I4  ,52 
     ,"SU_GDEL " ,R4 ,"SU header: rec datum elev       " }

   ,{ "sdel"     ,I4  ,56 
     ,"SU_SDEL " ,R4 ,"SU header: source datum elev    " }

   ,{ "swdep"    ,I4  ,60 
     ,"SOU_H2OD" ,R4 ,"Water depth at source           " }

   ,{ "gwdep"    ,I4  ,64 
     ,"REC_H2OD" ,R4 ,"Water depth at receiver         " }

   ,{ "scalel"   ,I2  ,68 
     ,"SUSCALE1" ,R4 ,"SU header: elevation scaler     " }

   ,{ "scalco"   ,I2  ,70 
     ,"SUSCALCO" ,R4 ,"SU header: coordinate scaler    " }

   ,{ "sx"       ,I4  ,72 
     ,"SOU_X   " ,R4 ,"Source X coordinate             " }

   ,{ "sy"       ,I4  ,76 
     ,"SOU_Y   " ,R4 ,"Source Y coordinate             " }

   ,{ "gx"       ,I4  ,80 
     ,"REC_X   " ,R4 ,"Receiver X coordinate           " }

   ,{ "gy"       ,I4  ,84 
     ,"REC_Y   " ,R4 ,"Receiver Y coordinate           " }

   ,{ "counit"   ,I2  ,88 
     ,"SUCOUNIT" ,I4 ,"SU header: coordinate units     " }

   ,{ "wevel"    ,I2  ,90 
     ,"SU_WEVEL" ,R4 ,"Source depth                    " }

   ,{ "swevel"   ,I2  ,92 
     ,"REC_DEP " ,R4 ,"Receiver depth                  " }

   ,{ "sut"      ,I2  ,94 
     ,"UPHOLE  " ,R4 ,"Source uphole time              " }

   ,{ "gut"      ,I2  ,96 
     ,"SU_GUT  " ,R4 ,"Receiver uphole time            " }

   ,{ "sstat"    ,I2  ,98 
     ,"SOU_STAT" ,R4 ,"Total static for source         " }

   ,{ "gstat"    ,I2 ,100 
     ,"REC_STAT" ,R4 ,"Total static for receiver       " }

   ,{ "tstat"    ,I2 ,102 
     ,"TOT_STAT" ,R4 ,"Total static for this trace     " }

   ,{ "laga"     ,I2 ,104 
     ,"NMO_STAT" ,R4 ,"NMO datum static (don't apply)  " }

   ,{ "lagb"     ,I2 ,106 
     ,"FNL_STAT" ,R4 ,"Static to move to final datum   " }

   ,{ "delrt"    ,I2 ,108 
     ,"SU_DELRT" ,I4 ,"Recording time delay            " }

   ,{ "muts"     ,I2 ,110 
     ,"SU_MUTS " ,R4 ,"Start time of mute              " }

   ,{ "mute"     ,I2 ,112 
     ,"TLIVE_S"  ,R4 ,"End time of mute                " }

   ,{ "ns"       ,U2 ,114 
     ,"SU_NS   " ,I4 ,"SU header: number of samples    " }

   ,{ "dt"       ,U2 ,116 
     ,"SU_DT   " ,I4 ,"SU header: sample rate usec     " }

   ,{ "gain"     ,I2 ,118 
     ,"NA_STAT " ,R4 ,"Portion of static not applied   " }

   ,{ "igc"      ,I2 ,120 
     ,"TLIVE_E " ,R4 ,"End of tail mute                " }

   ,{ "igi"      ,I2 ,122 
     ,"TFULL_S " ,I4 ,"Full amplitude time of mute     " }

   ,{ "corr"     ,I2 ,124 
     ,"EOJ     " ,I4 ,"End of job flag                 " }

   ,{ "sfs"      ,I2 ,126 
     ,"LEN_SURG" ,R4 ,"Length of surgical mute taper   " }

   ,{ "sfe"      ,I2 ,128 
     ,"FB_PICK " ,R4 ,"First break pick time           " }

   ,{ "slen"     ,I2 ,130 
     ,"SR_AZIM " ,R4 ,"Source to receiver azimuth      " }

   ,{ "styp"     ,I2 ,132 
     ,"CDP_NFLD" ,I4 ,"Fold within CDP bin             " }

   ,{ "stas"     ,I2 ,134 
     ,"CDP_ELEV" ,R4 ,"Elevation at CDP locattion      " }

   ,{ "stae"     ,I2 ,136 
     ,"NCHANS  " ,I4 ,"Number of channels of source    " }

   ,{ "tatyp"    ,I2 ,138 
     ,"SEQ_DISK" ,I4 ,"Trace sequence number from disk " }

   ,{ "afilf"    ,I2 ,140 
     ,"REPEAT  " ,I4 ,"REPEATED data copy number       " }

   ,{ "afils"    ,I2 ,142 
     ,"SU_AFILS" ,R4 ,"Autostatics source static       " }

   ,{ "nofilf"   ,I2 ,144 
     ,"SUNOFILF" ,R4 ,"Autostatics recvr static        " }

   ,{ "nofils"   ,I2 ,146 
     ,"TRIMSTAT" ,R4 ,"Trim static                     " }

   ,{ "lcf"      ,I2 ,148 
     ,"SOU_COMP" ,I4 ,"Source component (x,y,z)        " }

   ,{ "hcf"      ,I2 ,150 
     ,"GEO_COMP" ,I4 ,"Geophone component (x,y,z)      " }

   ,{ "lcs"      ,I2 ,152 
     ,"LSEG_END" ,I4 ,"Line segment end                " }

   ,{ "hcs"      ,I2 ,154 
     ,"LSEG_SEQ" ,I4 ,"Line segment sequence number    " }

   ,{ "year"     ,I2 ,156 
     ,"DMOOFF  " ,R4 ,"Offset bin for DMO              " }

   ,{ "day"      ,I2 ,158 
     ,"TFULL_S " ,R4 ,"Time of first full amplitude    " }

   ,{ "hour"     ,I2 ,160 
     ,"SU_HOUR " ,I4 ,"Hour of recording               " }

   ,{ "minute"   ,I2 ,162 
     ,"SU_MIN  " ,I4 ,"Minute of recording             " }

   ,{ "sec"      ,I2 ,164 
     ,"SU_SEC  " ,I4 ,"Second of recording             " }

   ,{ "timbas"   ,I2 ,166 
     ,"OFB_CNTR" ,R4 ,"Offset bin center               " }

   ,{ "trwf"     ,I2 ,168 
     ,"OFB_NO  " ,I4 ,"Offset bin number               " }

   ,{ "grnors"   ,I2 ,170 
     ,"LINE_NO " ,I4 ,"Line number (hashed line name)  " }

   ,{ "grnofr"   ,I2 ,172 
     ,"DS_SEQNO" ,I4 ,"Input dataset sequence number   " }

   ,{ "grnlof"   ,I2 ,174 
     ,"ILINE_NO" ,I4 ,"3D inline number                " }

   ,{ "gaps"     ,I2 ,176 
     ,"XLINE_NO" ,I4 ,"3D crossline number             " }

   ,{ "otrav"    ,I2 ,178 
     ,"DISKITER" ,I4 ,"Disk Data Input iteration       " }

   ,{ "d1"       ,R4 ,180 
     ,"SU_D1   " ,R4 ,"SU header: increment dim 1      " }

   ,{ "f1"       ,R4 ,184 
     ,"SU_F1   " ,R4 ,"SU header: first value dim 1    " }

   ,{ "d2"       ,R4 ,188 
     ,"SU_D2   " ,R4 ,"SU header: increment dim 2      " }

   ,{ "f2"       ,R4 ,192 
     ,"SU_F2   " ,R4 ,"SU header: first value dim 2    " } 

   ,{ "ungpow"   ,R4 ,196 
     ,"SU_UNGPO" ,R4 ,"SU header: gpower               " }

   ,{ "unscale"  ,R4 ,200 
     ,"SU_UNSCA" ,R4 ,"SU header: unscale              " }

   ,{ "mark"     ,I2 ,204 
     ,"SU_MARK " ,R4 ,"SU header: mark                 " }

   ,{ "mutb"     ,I2 ,206
     ,"TFULL_E " ,I4 ,"Start of tail mute              " }

   ,{ "dz"       ,R4 ,208 
     ,"SU_DZ   " ,R4 ,"Depth increment (0.0 for time   " }

   ,{ "fz"       ,R4 ,212 
     ,"SU_FZ   " ,R4 ,"Depth of first sample           " }

   ,{ "n2"       ,I2 ,216 
     ,"SU_N2   " ,I4 ,"Number of traces in ensemble    " }

   ,{ "end_ens"  ,I2 ,218 
     ,"END_ENS " ,I4 ,"End of ensemble flag            " }

   ,{ "ntr"      ,I4 ,220 
     ,"SU_NTR  " ,R4 ,"SU number of traces in file     " }

   ,{ "cdp_x"    ,I4 ,224 
     ,"CDP_X   " ,R4 ,"CDP X coordinate                " }

   ,{ "cdp_y"    ,I4 ,228 
     ,"CDP_Y   " ,R4 ,"CDP Y coordinate                " }

   ,{ "aoffset " ,I4 ,232 
     ,"AOFFSET " ,R4 ,"Absolute value of offset        " }

   ,{ "amp_norm" ,R4 ,236 
     ,"AMP_NORM" ,R4 ,"Average sample gain factor      " }

   ,{ "    "     ,-1  ,-1 
     ,"        " ,-1 ,"End of array marker record      " }

};

