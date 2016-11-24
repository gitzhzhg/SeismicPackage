/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUOP: $Revision: 1.32 $ ; $Date: 2013/06/03 18:23:54 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUOP - do unary arithmetic operation on segys 		",
" 								",
" suop <stdin >stdout op=abs					",
" 								",
" Required parameters:						",
"	none							",
"								",
" Optional parameter:						",
"	op=abs		operation flag				",
"			abs   : absolute value			",
"			avg   : remove average value		",
"			ssqrt : signed square root		",
"			sqr   : square				",
"			ssqr  : signed square			",
"			sgn   : signum function			",
"			exp   : exponentiate			",
"			sexp  : signed exponentiate		",
"			slog  : signed natural log		",
"			slog2 : signed log base 2		",
"			slog10: signed common log		",
"			cos   : cosine				",
"			sin   : sine				",
"			tan   : tangent				",
"			cosh  : hyperbolic cosine		",
"			sinh  : hyperbolic sine			",
"			tanh  : hyperbolic tangent		",
"			cnorm : norm complex samples by modulus ", 
"			norm  : divide trace by Max. Value	",
"			db    : 20 * slog10 (data)		",
"			neg   : negate value			",
"			posonly : pass only positive values	",
"			negonly : pass only negative values	",
"                       sum   : running sum trace integration   ",
"                       diff  : running diff trace differentiation",
"                       refl  : (v[i+1] - v[i])/(v[i+1] + v[i]) ",
"			mod2pi : modulo 2 pi			",
"			inv   : inverse				",
"			rmsamp : rms amplitude			",
"                       s2v   : sonic to velocity (ft/s) conversion     ",
"                       s2vm  : sonic to velocity (m/s) conversion     ",
"                       d2m   : density (g/cc) to metric (kg/m^3) conversion ",
"                       drv2  : 2nd order vertical derivative ",
"                       drv4  : 4th order vertical derivative ",
"                       integ : top-down integration            ",
"                       spike : local extrema to spikes         ",
"                       saf   : spike and fill to next spike    ",
"                       freq  : local dominant freqeuncy        ",
"                       lnza  : preserve least non-zero amps    ",
"                       --------- window operations ----------- ",
"                       mean  : arithmetic mean                 ",
"                       despike  : despiking based on median filter",
"                       std   : standard deviation              ",
"                       var   : variance                        ",
"       nw=21           number of time samples in window        ",
"                       --------------------------------------- ",
"			nop   : no operation			",
"								",
" Note:	Binary ops are provided by suop2.			",
" Operations inv, slog, slog2, and slog10 are \"punctuated\",	", 
" meaning that if, the input contains 0 values,			",
" 0 values are returned.					",	
"								",
" For file operations on non-SU format binary files use:  farith",
NULL};

/* Credits:
 *
 * CWP: Shuki Ronen, Jack K Cohen (c. 1987)
 *  Toralf Foerster: norm and db operations, 10/95.
 *  Additions by Reg Beardsley, Chris Liner, and others.
 *
 * Notes:
 *	If efficiency becomes important consider inverting main loop
 *      and repeating operation code within the branches of the switch.
 *
 *	Note on db option.  The following are equivalent:
 *	... | sufft | suamp | suop op=norm | suop op=slog10 |\
 *		sugain scale=20| suxgraph style=normal
 *
 *	... | sufft | suamp | suop op=db | suxgraph style=normal
 */
/**************** end self doc ***********************************/

#ifndef	TWOPI
#define	TWOPI (2.0 * PI)
#endif


#define	FABS	1
#define	SSQRT	2
#define	SQROP	3
#define	SSQR	4
#define EXP	5
#define SLOG	6
#define SLOG10	7
#define COS	8
#define SIN	9
#define TAN	10
#define COSH	11
#define SINH	12
#define TANH	13
#define NORM    14	/* normalize at maximal value   */
#define DB      15	/* for frequency plot in db     */
#define SIGN	16	/* signum function		*/

#define NEG	17
#define NOP	18
#define ONLY_POS 19
#define ONLY_NEG 20
#define SUM    21
#define REFL   22
#define DIFF   23
#define MOD_2PI 24
#define INV	25
#define AVG	26
#define RMS_AMP	27

#define S2V    28 
#define S2VM   29 
#define D2M    30
#define DRV2   31
#define DRV4   32
#define SPIKE  33
#define SLOG2  34
#define STD    35
#define MEAN   36
#define VAR    37
#define INTEG  38 
#define LNZA   39
#define FREQ   40
#define SAF    41	/* spike and fill 8_22_08 cll */ 
#define CNORM  42       /* normalize complex value by modulus */
#define DESPIKE  43       /* Despike with median filter */

#define SEXP  44

float mymedian(float *a, int nw);
int floatcomp(const void* elem1, const void* elem2);

segy tr;

int
main(int argc, char **argv)
{
	cwp_String op="abs";	/* operation: abs, exp, ..., 		*/
	int iop=FABS;		/* integer abbrev. for op in switch	*/
	int nt;			/* number of samples on input trace	*/
	float dt;		/* time sampling interval */
	float twobydt;		/* 2*dt */

        float *tmp=NULL;        /* temp trace for some calcs */
        float *tmp1=NULL;       /* temp trace for some calcs */
        int nw;                 /* number of time samples in window */
        int mt;                 /* half-width of window */
        int j;		/* counter */
        float val;	/* value */
        float mean;	/* mean */



	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get information from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = tr.dt/1000000.0;
	twobydt=2*dt;


	/* Get operation, recall iop initialized to the default FABS */
	getparstring("op", &op);
	if      (STREQ(op, "ssqrt"))	iop = SSQRT;
	else if (STREQ(op, "sqr"))	iop = SQROP;
	else if (STREQ(op, "ssqr"))	iop = SSQR;
	else if (STREQ(op, "sgn"))	iop = SIGN;
	else if (STREQ(op, "exp"))	iop = EXP;
	else if (STREQ(op, "sexp"))	iop = SEXP;
	else if (STREQ(op, "slog"))	iop = SLOG;
	else if (STREQ(op, "slog2"))	iop = SLOG2;
	else if (STREQ(op, "slog10"))	iop = SLOG10;
	else if (STREQ(op, "cos"))	iop = COS;
	else if (STREQ(op, "sin"))	iop = SIN;
 	else if (STREQ(op, "tan"))	iop = TAN;
	else if (STREQ(op, "cosh"))	iop = COSH;
	else if (STREQ(op, "sinh"))	iop = SINH;
	else if (STREQ(op, "tanh"))	iop = TANH;
	else if (STREQ(op, "norm"))     iop = NORM;
	else if (STREQ(op, "cnorm"))    iop = CNORM;
	else if (STREQ(op, "db"))       iop = DB;
	else if (STREQ(op, "neg"))      iop = NEG;
	else if (STREQ(op, "nop"))      iop = NOP;
	else if (STREQ(op, "posonly"))  iop = ONLY_POS;
	else if (STREQ(op, "negonly"))  iop = ONLY_NEG;
	else if (STREQ(op, "sum"))      iop = SUM;
	else if (STREQ(op, "refl"))     iop = REFL;
	else if (STREQ(op, "diff"))     iop = DIFF;
	else if (STREQ(op, "mod2pi"))  	iop = MOD_2PI;
	else if (STREQ(op, "inv"))  	iop = INV;
 	else if (STREQ(op, "avg"))  	iop = AVG;
 	else if (STREQ(op, "rmsamp"))  	iop = RMS_AMP;
	else if (STREQ(op, "std"))      iop = STD;
	else if (STREQ(op, "var"))      iop = VAR;
	else if (STREQ(op, "mean"))     iop = MEAN;
	else if (STREQ(op, "despike"))     iop = DESPIKE;
	else if (STREQ(op, "s2v"))      iop = S2V;
	else if (STREQ(op, "s2vm"))     iop = S2VM;
	else if (STREQ(op, "d2m"))      iop = D2M;
	else if (STREQ(op, "drv2"))     iop = DRV2;
	else if (STREQ(op, "drv4"))     iop = DRV4;
	else if (STREQ(op, "integ"))    iop = INTEG;
	else if (STREQ(op, "spike"))    iop = SPIKE;
	else if (STREQ(op, "freq"))     iop = FREQ;
	else if (STREQ(op, "lnza"))     iop = LNZA;
	else if (STREQ(op, "saf"))      iop = SAF;
	else if (!STREQ(op, "abs"))
		err("unknown operation=\"%s\", see self-doc", op);

        if (!getparint("nw", &nw)) nw = 21;
        checkpars();

        /* make sure nw is odd */
        if (nw%2 == 0) nw += 1;


	/* allocate memory for median. It might be that we
	do not need it but is small anyway */

	float *tomed=NULL; /* temporary data storage */
	tomed = ealloc1float(nw);
	tmp = ealloc1float(nt);

	/* Main loop over traces */
	do {

		switch(iop) { register int i;
		case FABS:
			for (i = 0; i < nt; ++i)
				tr.data[i] = ABS(tr.data[i]);
		break;
		case SSQRT:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];
				tr.data[i] = SGN(x) * sqrt(ABS(x));
			}
		break;
		case SQROP:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];
				tr.data[i] = SQR(x);
			}
		break;
		case SSQR:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];
				tr.data[i] = SGN(x) * x * x;
			}
		break;
		case SIGN:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];
				tr.data[i] = SGN(x);
			}
		break;
		case EXP:
			for (i = 0; i < nt; ++i){
				tr.data[i] = exp(tr.data[i]);
			}
		break;
		case SEXP:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];

				tr.data[i] = SGN(x) * exp(ABS(tr.data[i]));
			}

		break;
		case SLOG:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];

				if (ABS(x) > 0) {
					tr.data[i] = SGN(x) * log(ABS(x));
				} else {
					tr.data[i] = 0;
				}
			}
		break;
               case SLOG2:
                        for (i = 0; i < nt; ++i) {
                                float x = tr.data[i];

                                if (ABS(x) > 0) {
                                        tr.data[i] = SGN(x) * log(ABS(x))/log(2.0);
                                } else {
                                        tr.data[i] = 0;
                                }
                        }

		case SLOG10:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];
				if (ABS(x) > 0) {
					tr.data[i] = SGN(x) * log10(ABS(x));
				} else {
					tr.data[i] = 0;
				}
					
			}
		break;
		case COS:
			for (i = 0; i < nt; ++i)
				tr.data[i] = cos(tr.data[i]);
		break;
		case SIN:
			for (i = 0; i < nt; ++i)
				tr.data[i] = sin(tr.data[i]);
		break;
		case TAN:
			for (i = 0; i < nt; ++i)
				tr.data[i] = tan(tr.data[i]);
		break;
		case COSH:
			for (i = 0; i < nt; ++i)
				tr.data[i] = cosh(tr.data[i]);
		break;
		case SINH:
			for (i = 0; i < nt; ++i)
				tr.data[i] = sinh(tr.data[i]);
		break;
		case TANH:
			for (i = 0; i < nt; ++i)
				tr.data[i] = tanh(tr.data[i]);
		break;
		case NORM:
		      { float x, max;
			max = 0.0;
			for (i = 0; i < nt; ++i) {
				x = ABS (tr.data [i]);
				if (max < x) max = x;
			}
			if (max != 0.0)
				for (i = 0; i < nt; ++i) tr.data [i] /= max;
		      }	/* end scope of x, max */
		break;
 
		case CNORM:
		      { float x;
                        if( tr.trid != 111 ){
                           perror( "cnorm option only works for trid=111" );
                        }
			for (i = 0; i < nt; i+=2) {
                           x = sqrt( tr.data[i]*tr.data[i] + tr.data[i+1]*tr.data[i+1] );
                           tr.data[i]   /= x;
                           tr.data[i+1] /= x;
                        }
		      }
		break;
 
		case DB:
			if (iop == DB)  {
				float x;
				for (i = 0; i < nt; ++i) {
					x = tr.data[i];
					tr.data[i] = (ABS(x) > 0) ?
					   20.0*SGN(x)*log10(ABS(x)) : 0;
				}
			}
		break;
		
		case NEG:
			for (i = 0; i < nt; ++i)
				tr.data[i] = -tr.data[i];
		break;
		case NOP:
		break;
		case ONLY_POS:
		{
			float x;		     
	       		for (i = 0; i < nt; ++i) {   
				x = tr.data[i];      
				tr.data[i] = (x > 0) ? x : 0;  
			}
		}			      
		break; 
		case ONLY_NEG:
		{
			float x;		     
	       		for (i = 0; i < nt; ++i) {   
				x = tr.data[i];      
				tr.data[i] = (x < 0) ? x : 0;  
			}
		}			      
		break;
		case SUM:
		{
	       		for (i = 1; i < nt; ++i) {   
				tr.data[i] += tr.data[i-1];
			}
		}			      
		break; 
		case REFL:
		{
	       		for (i = nt-1; i > 0; --i) {   
				float numer=(tr.data[i] - tr.data[i-1]);
				float denom=(tr.data[i] + tr.data[i-1]);
				if (denom == 0.0) denom = 1.0;
				tr.data[i] = ( numer/denom ) ;
			}
			tr.data[0] = 0.0;
		}			      
		break;
		case DIFF:
		{
			float *datatmp=NULL; /* temporary data storage */

			/* allocate space for temporary data */
			datatmp = ealloc1float(nt);

			/* copy data from tr.data */
			memcpy((void *)datatmp, (const void *) tr.data, nt*FSIZE);
			/* do centered differences for the rest */
	       		for (i = 2; i < nt-2; ++i) {   
				float numer;
				numer=(datatmp[i+1] - datatmp[i-1]);
				tr.data[i] = ( numer/twobydt ) ;
			}
	
			/* simple diffrence for tr.data[0] */
			tr.data[0] = (datatmp[1] - datatmp[0])/dt;
			tr.data[nt-1] = (datatmp[nt-1] - datatmp[nt-2])/dt;

			/* centered difference for tr.data[1] */
			tr.data[1] = (datatmp[2] - datatmp[0])/twobydt;
			tr.data[nt-2] = (datatmp[nt-1] - datatmp[nt-3])/twobydt;
		}			      
		break;

 		case AVG:
 	        {
 			float sum = 0;float avg = 0 ;
	        for (i = 0; i < nt; ++i)
		        sum = sum + tr.data[i];
 			avg = sum / nt ;
 			for (i = 0; i < nt; ++i)
 				tr.data[i] = tr.data[i] - avg;
 			}
 		break;

 		case RMS_AMP:
	        {
 			float sum = 0;float rmsamp = 0 ;
	        for (i = 0; i < nt; ++i)
		        sum = sum + tr.data[i]*tr.data[i];
 			rmsamp = sqrt(sum / nt) ;
			
			memset((void *) &tr.data, 0, nt*FSIZE);
			tr.data[0] = rmsamp;
			}
 		break;

		case MOD_2PI:
			for (i = 0; i < nt; ++i)
			{	while(tr.data[i]<0) tr.data[i] += TWOPI;
				while(tr.data[i]>=TWOPI) tr.data[i] -= TWOPI;
	        }
		break;
		case INV:
			for (i = 0; i < nt; ++i)
				if (tr.data[i]) tr.data[i] = 1.0 / tr.data[i];
		break;
		case LNZA:
		{
			float x, y, z;
			
			/* copy a trace to tmp */
			for (i = 0; i < nt; ++i) {
				tmp[i] = tr.data[i];
			}
			
			/*
			here are the three cases
			
			x y z    x y       x           +
			--------------------------> t  0
			x y z        z       y z       -
			
			*/
			for (i = 1; i < nt-1; ++i) {
				x = tr.data[i-1];
				y = tr.data[i];
				z = tr.data[i+1];
				if ( SGN(x) == SGN(y) && SGN(y) == SGN(z)) {
					tmp[i] = 0.0;
				} 
				if ( SGN(x) == SGN(y) && SGN(y) != SGN(z)) {
					if ( abs(y) < abs(z) ) {
						tmp[i] = y;
					} else {
						tmp[i] = z;
					}
				} 
				if ( SGN(x) != SGN(y) && SGN(y) == SGN(z)) {
					if ( abs(x) < abs(y) ) {
						tmp[i] = x;
					} else {
						tmp[i] = y;
					}
				} 
			}
			
			/* fix first and last sample */
			tmp[0]  = 0.0;
			tmp[nt] = 0.0;
			
			/* copy values back to trace */
			for (i = 0; i < nt; ++i) {
				tr.data[i] = tmp[i];
			}
		}			      
		break; 
		case SPIKE:
		{
			float x1, x2, x3;
			for (i = 1; i < nt-1; ++i) {
				x1 = tr.data[i-1];
				x2 = tr.data[i];
				x3 = tr.data[i+1];
				/* local min or max */
				if ( ( (x1 < x2) && (x3 < x2) ) || 
				     ( (x1 > x2) && (x3 > x2) ) ){
					tmp[i] = x2;
				} else {
					/* neither */
					tmp[i] = 0.0;
				}
			}
			/* edge effects */
			tmp[0] = 0.0;
			tmp[nt] = 0.0;
			/* reload trace for output */
			for (i = 0; i < nt; ++i) {
				tr.data[i] = tmp[i];
			}
		}			      
		break;
		case SAF:
		{
			float x1, x2, x3;
			int iold;    /* index of last spike */
			int j;	     /* fill counter	    */
			float vold;  /* value of last spike */
			iold = 0;
			vold = 0.0;
			for (i = 1; i < nt-1; ++i) {
				x1 = tr.data[i-1];
				x2 = tr.data[i];
				x3 = tr.data[i+1];
				/* local min or max */
				if ( ( (x1 < x2) && (x3 < x2) ) || 
				     ( (x1 > x2) && (x3 > x2) ) ){
					tmp[i] = x2;
					/* fill from last spike */
					for (j = iold; j < i; ++j ) {
						tmp[j] = vold;
					}
					/* reset old values */
					iold = i;
					vold = x2;
				} else {
					/* neither */
					tmp[i] = 0.0;
				}
			}
			/* edge effects */
			tmp[0] = 0.0;
			tmp[nt] = 0.0;
			/* reload trace for output */
			for (i = 0; i < nt; ++i) {
				tr.data[i] = tmp[i];
			}
		}			      
		break;
		case FREQ:
		{
			float x1, x2, x3, delay, freq;
			register int iold, j;
			/*
			  scan for extrema
			  local max:  (x2 > x1) and (x2 > x3)... assign +1
			  local min:  (x2 < x1) and (x2 < x3)... assign -1
			*/
			for (i = 1; i < nt-1; ++i) {
				x1 = tr.data[i-1];
				x2 = tr.data[i];
				x3 = tr.data[i+1];
				/* local min or max */
				if ( (x2 > x1) && (x2 > x3) ) {
					tmp[i] =  1.0; 
				} else if ( (x2 < x1) && (x2 < x3) ) {
					tmp[i] = -1.0; 
				} else {
					/* neither */
					tmp[i] =  0.0;
				}
			}
			/* edge effects */
			tmp[0] = 0.0;
			tmp[nt] = 0.0;
			/*
			  delay between consecutive local minima 
			  is local dominant period, which is inverse 
			  of local dom frequency
			*/
			/* assume first sample is a minima... kludge */
			iold = 0;
			/* skip a few samples to avoid false high freq at start */
			for (i = 5; i < nt; ++i) {
				if ( tmp[i] == -1.0 ) {
						/* calc delay from last min */
						delay = (i - iold) * dt;
						/* calc freq Hz 
						   This delay is the period */
						freq  = 1 / ( delay);
						/* fill with this freq from last min to here */
						for (j = iold; j < i; ++j) {
							tmp1[j] = freq;
						}
					/* hold loc of this minima */
					iold = i;
				}
			}
			/* reload trace for output */
			for (i = 0; i < nt; ++i) {
				tr.data[i] = tmp1[i];
			}
		}			      
		break;
		case DRV4:
			for (i = 2; i < nt-2; ++i) {
				float x =         tr.data[i-2] 
					  - 8.0 * tr.data[i-1]
					  + 8.0 * tr.data[i+1] 
					        - tr.data[i+2];
				tmp[i] = - x / (12 * dt);
			}
			tmp[0]    = tmp[2];
			tmp[1]    = tmp[2];
			tmp[nt-1] = tmp[nt-2];
			tmp[nt]   = tmp[nt-2];
			for (i = 2; i < nt-2; ++i) {
				tr.data[i] = tmp[i];
			}
		break;
		case DRV2:
			for (i = 1; i < nt-1; ++i) {
				float x = tr.data[i-1] - tr.data[i];
				tmp[i] = x / (2.0 * dt);
			}
			tmp[0] = tmp[1];
			tmp[nt] = tmp[nt-1];
			for (i = 1; i < nt-1; ++i) {
				tr.data[i] = tmp[i];
			}
		break;
		case INTEG:
			/* copy trace into temp array */
			for (i = 0; i < nt; ++i) {
				tmp[i] = tr.data[i];
			}
			/* loop over output times */
			val = 0.0;
			for (i = 0; i < nt; ++i) {
				val = val + tmp[i];
				tr.data[i] = val;
			}
		break;
		case S2V:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];
				tr.data[i] = 1000000./x;
			}
		break;
		case S2VM:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];
				tr.data[i] = 304800./x;
			}
		break;
		case D2M:
			for (i = 0; i < nt; ++i) {
				float x = tr.data[i];
				tr.data[i] = 1000.0*x;
			}
		break;
		case STD:
			/* copy trace into temp array */
			for (i = 0; i < nt; ++i) {
				tmp[i] = tr.data[i];
			}
			/* half width of window */
			mt = (nw-1)/2;
			/* loop over output times */
			for (i = 0; i < nt; ++i) {
				val = 0.0;
				/* check we are not off the data ends */
				if (i-mt > 0 && i+mt < nt) {
					/* calc mean */
					for (j = i-mt; j <= i+mt; ++j) {
						val += tmp[j];
					}
					mean = val/nw;
					/* calc std deviation */
					for (j = i-mt; j <= i+mt; ++j) {
						val += (tmp[j] - mean)*(tmp[j] - mean);
					}
					tr.data[i] = sqrt(val/ (float) nw);
				} else {
					tr.data[i] = 0.0;
				}
			}
		break;
		case VAR:
			/* copy trace into temp array */
			for (i = 0; i < nt; ++i) {
				tmp[i] = tr.data[i];
			}
			/* half width of window */
			mt = (nw-1)/2;
			/* loop over output times */
			for (i = 0; i < nt; ++i) {
				val = 0.0;
				/* check we are not off the data ends */
				if (i-mt > 0 && i+mt < nt) {
					/* calc mean */
					for (j = i-mt; j <= i+mt; ++j) {
						val += tmp[j];
					}
					mean = val/nw;
					/* calc variance */
					for (j = i-mt; j <= i+mt; ++j) {
						val += (tmp[j] - mean)*(tmp[j] - mean);
					}
					tr.data[i] = val/ (float) nw;
				} else {
					tr.data[i] = 0.0;
				}
			}
		break;
		case MEAN:
			/* copy trace into temp array */
			for (i = 0; i < nt; ++i) {
				tmp[i] = tr.data[i];
			}
			/* half width of window */
			mt = (nw-1)/2;
			/* loop over output times */
			for (i = 0; i < nt; ++i) {
				val = 0.0;
				/* check we are not off the data ends */
				if (i-mt > 0 && i+mt < nt) {
					/* calc mean */
					for (j = i-mt; j <= i+mt; ++j) {
						val += tmp[j];
					}
					tr.data[i] = val/nw;
				} else {
					tr.data[i] = 0.0;
				}
			}
		break;
		case DESPIKE: 
			/* copy trace into temp array */
			for (i = 0; i < nt; ++i) {
				tmp[i] = tr.data[i];
			}
			/* half width of window */
			mt = (nw-1)/2;

			/* allocate data for median */
			

			/* loop over output times */
			for (i = 0; i < nt; ++i) {
				/* check we are not off the data ends */
				if (i-mt > 0 && i+mt < nt) 
				{
					for(j=0; j<nw; j++)
						tomed[j] = tmp[i-mt+j];
				} 
				else 
				{
					for(j=0; j<nw; j++)
						tomed[j] = (i-mt < 0) ? tmp[j] : tmp[nt-nw+j];
				}
                val = mymedian(tomed,  nw);
				tr.data[i] = val;
			}
		break;

		default:  /* defensive programming */
			err("mysterious operation=\"%s\"", op);
		} /* end scope of i */
		
		
		
		puttr(&tr);

	} while (gettr(&tr));

	/* free memory from median */
	free1float(tomed);
	free1float(tmp);

	return(CWP_Exit());
}

int floatcomp(const void* elem1, const void* elem2)
{
    if(*(const float*)elem1 < *(const float*)elem2)
        return -1;
    return *(const float*)elem1 > *(const float*)elem2;
}


float mymedian(float *buff, int nw)
{
	qsort(buff, nw, FSIZE, floatcomp);
	if(nw %2)
		return  buff[nw/2];
 	else			
		return  0.5*(buff[nw/2]+ buff[nw/2 + 1]);
}
