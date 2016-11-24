/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPICKAMP: $Revision: 1.12 $ ; $Date: 2011/11/16 23:13:27 $	*/

#include "su.h"
#include "segy.h"


/*********************** self documentation **********************/
char *sdoc[] = {
" 	   								",
" SUPICKAMP - pick amplitudes within user defined and resampled window	",
" 	   								",
"   supickamp <stdin >stdout d2=  [optional parameters]			",
" 									",
" Required parameters:							",
" d2=		   sampling interval for slow dimension			",
"			(required if key-parameter not specified)	",
" Optional parameters:							",
" key= 			Key header word specifying trace offset 	",
" 			(alternatively, specify d2,x2beg)		",
" 									",
" x_above=		array of lateral position values   		",
" 			(upper window corner)				",
" t_above=		array of time values   				",
" 			(upper window corner)				",
"									",
"  ... or input via files:						",
" t_xabove=		file containing time and lateral position values",
" 			(upper window corner)				",
" t_xbelow=		file containing time and lateral position values",
" 			(lower window corner)				",
" wl=		   	window width if t_xbelow is not specified	",
"			(No windowing if not specified)		 	",
" 									",
" dt_resamp=dt	  resampling interval within pick window	  	",
"			(dt has to come from trace headers)		",
" tmin=0		minimum time in input trace			",
" x2beg=0		first lateral position				",
" format=ascii 		write ascii data to stdout			",
"			 =binary for binary floats to stdout		",
" verbose=1 		writes complete  pick information into outpar   ",
"			=2 writes complete pick information into outpar	",
"			   in tab-delimited column format		",
" outpar=/dev/tty	output parameter file; contains output		",
"					from verbose			",
" arg1=max		output (first dimension) to stdout		",
" arg2=i2		output (second dimension) to stdout		",
"			(see notes for other options)			",
" Notes: 								",
"									",
" Window can be defined using						",
" (1)   vectors x_above, t_above, [wl]					",
" (2)   file  t_xabove, [wl]	or					",
" (3)   files t_xabove, t_xbelow					",
"									",
" files t_xabove, t_xbelow can be generated using xwigb's picking	",
" algorithm. The lateral positions have to be monotonically increasing  ",
" or decreasing for both vector and file input.				",
" verbose=1 or 2 writes min, max, abs[max], energy and associated times	",
" tmin,tmax,tabs to outpar, together with global values. verbose=0	",
" only outputs global values.						",
" Acceptable arg-parameters for lateral positions are			",
" (1) x2   (2) i2 = trace number					",
"									",
" If key=keyword is set, then the values of x2 are taken from the header",
" field represented by the keyword (for example key=offset)		",
" Type	sukeyword -o   to see the complete list of SU keywords.		",
"									",
NULL};

/*
 * Credits:
 *
 *	CWP: Andreas Rueger July 06, 1996
 *	MTU: David Forel,   Jan. 26, 2005  Add verbose=2 option
 */
/**************** end self doc ***********************************/

/* Locally defined macros */
#define diprint(expr) printf(#expr " = %i\n",expr)
#define dfprint(expr) printf(#expr " = %f\n",expr)
#define ddprint(expr) printf(#expr " = %g\n",expr)

segy tr;

/* Pick parameterss */
typedef struct PickStruct {
	float max,min,abs;
	float tmax,tmin,tabs;
	float energy;
	int imax,imin,iabs,ienergy;
} Pick;

/* Prototype of functions used internally */
int tracepick(Pick *pick1, float *t, float *val, int nt);
int initpick(Pick *pick1);
int globalpick(Pick *pick1, Pick *gpick);
int get_mode(char *arg, int *mode);
int getformat(char *format, int *modef);
int getarg(float *arg, int mode, Pick *pick, float x2,int trnum);


int
main(int argc, char **argv)
{
	char *arg1,*arg2;	/* control parameters		*/
	char *key;		/* pointer SEGY keyword		*/
	char *type=NULL;	/* ... its type			*/
	int index=0;		/* ... its index		*/
	Value val;		/* ... its value		*/
	float fval;		/* ... its value cast to float	*/
	
   	float *x_above=NULL;	/* pointer to x values above	*/
	float *x_below=NULL;	/* pointer to x values below	*/
	float *t_above=NULL;	/* pointer to t values above	*/
	float *t_below=NULL;	/* pointer to t values above	*/

	float wl;		/* window width			*/
	float x2beg;		/* first  x2 value		*/		
	float d2;		/* x2 sampling interval		*/
	float t1;		/* time values			*/
	float x1;		/* x values			*/
	float dt_resamp,tmin,dt,tmin_in;
	
	float *t_resamp=NULL;	/* resampled time		*/
	float *val_resamp=NULL;	/* resampled amplitude		*/
	float tmax;		/* maximum time			*/
	
	int nx,i,trnum;	
	int nt,nts,modef = 0;	
	int verbose,mode1,mode2 ;

	char *outpar=NULL;	/* name of file holding output parfile  */
	FILE *outparfp=NULL;	/* ... its file pointer			*/

	char *format=NULL;	/* format (ascii/binary) of output to stdout */

	/* Names of input files */
	cwp_String afile="";	
	FILE *afilep=NULL;		
	cwp_String bfile="";	
	FILE *bfilep=NULL;		

	/* Booleans to keep track of open files */
	cwp_Bool afile_set=cwp_false;
	cwp_Bool bfile_set=cwp_false;

	Pick *pick1,*gpick;
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Allocate space for picks */
	pick1=(Pick*)emalloc(1*sizeof(Pick));
	gpick=(Pick*)emalloc(1*sizeof(Pick));


	/* Get parameters */
	if (!getparfloat("d2", &d2)) d2=0; 
	if (getparstring("key", &key))	{
	   d2=0;
	} else if(d2==0) {
	   err(" Specify d2");
	}

	if (!getparint("verbose", &verbose))	verbose = 1;
	if (!getparstring("arg1", &arg1))	arg1 = "max";
	if (!getparstring("arg2", &arg2))	arg2 = "x2";

	if (!getparfloat("wl", &wl))	wl = 0.;
	if (!getparfloat("dt_resamp", &dt_resamp))   dt_resamp = 0.;
	if (!getparfloat("x2beg", &x2beg))	x2beg = 0.;
	if (!getparfloat("tmin", &tmin_in))	tmin_in = 0.;

	if (!getparstring("format", &format)) format="ascii";

	if (!getparstring("outpar", &outpar))  outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");

	/* column output */
	if(verbose == 2)
	fprintf(outparfp,
		"trace\tx\tt_above\tt_below\t"
		"trace #\tx2\t"
		"max\t tmax\t"
		"min\t tmin\t"
		"abs\t tabs\t"
		"energy in pick window\n") ;

	/* get key type and index */
	if(d2==0.) {
	   type = hdtype(key);
	   index = getindex(key);
	}


	if (getparstring("t_xabove",&afile)) {

		/* check if file exists */
		if ((afilep=fopen(afile,"r")) == NULL)
		fprintf(stderr,"can't open t_xabove file \n");

		/* Quick runthrough */
		nx=0;
   
		while (fscanf(afilep,"%f %f \n",&t1,&x1 ) != EOF) ++nx;

		rewind(afilep);

		x_above=ealloc1float(nx);
		t_above=ealloc1float(nx);  

		/* Read above_file */
		nx=0;
			while (fscanf(afilep,"%f %f \n",&t1,&x1 ) != EOF) {
				x_above[nx]=x1;
				t_above[nx]=t1;

				/* Test output 
				fprintf(outparfp,"%f %f \n",t1,x1); */
		
				++nx;
			}
		

		} else {

			nx=nt=0;
		
			nx = countparval("x_above");
			nt = countparval("t_above");

		if (nx != nt)
			err("lengths of x_above, t_above must be the same");

		x_above = ealloc1float(nx);  getparfloat("x_above", x_above);
		t_above = ealloc1float(nx);  getparfloat("t_above", t_above);

		/* Test output 

		for(i=0;i<nx;i++)
			fprintf(outparfp,"%f %f \n",t_above[i],x_above[i]); */

	}

	/* Lower limit picks of time-window */
	if (getparstring("t_xbelow",&bfile) ) {

		if (afilep == NULL)
		err(" Specify t_xabove -file");
		
		/* check if file exists */
		if ((bfilep=fopen(bfile,"r")) == NULL)
		fprintf(stderr,"can't open t_xbelow-file \n");


		/* Quick runthrough */
		nx=0;
   
		while (fscanf(bfilep,"%f %f \n",&t1,&x1 ) != EOF)
		{
		   nx++;
		}

		rewind(bfilep);

		x_below=ealloc1float(nx);
		t_below=ealloc1float(nx);  

		/* Read below_file */
		nx=0;
			while (fscanf(bfilep,"%f %f \n",&t1,&x1 ) != EOF)
		{
		x_below[nx]=x1;
		t_below[nx]=t1;

		/* Test output 
		fprintf(outparfp,"%f %f \n",t1,x1); */
		
		nx++;
		}

	 }
	
        checkpars();
	/* Specify window width */
	if(bfilep!= NULL && afilep != NULL) {
		afile_set = cwp_true;
		bfile_set = cwp_true;
	   warn("window determined by above_ and below_ files ");
	} else if(afilep != NULL && wl != 0) {
		afile_set = cwp_true;
	   warn("window determined by above_file and wl");
	} else if(afilep != NULL && wl == 0 ){
		afile_set = cwp_true;
	   warn("window determined by above_file and trace end");
	} else if(afilep == NULL && wl != 0 && nx!=0) { 
		afile_set = cwp_false;
	   warn("window determined by above_vector and wl");
	} else if(afilep == NULL && wl == 0 && nx!=0) {
		afile_set = cwp_false;
	   warn("window determined by above_vector and trace length");
	} else if(afilep == NULL && wl == 0) {
		afile_set = cwp_false;
	   warn("window determined by full trace length");
	} else {
	   err("window not properly specified");
	}


	/* get mode for stdout information */
	if(! get_mode(arg1,&mode1) )
	   err(" Unknown arg1 ");

	if(! get_mode(arg2,&mode2) )
	   err(" Unknown arg2");

	/* get format for stdout information */
	if(! getformat(format, &modef) )
	   err(" Unknown format"); 

	/* Get info from first trace */
	if (!gettr(&tr)) err("can't read first trace");
	if (!tr.dt) err("dt header field must be set");
	if (tr.delrt)
	   tmin_in = tr.delrt/1000.0;

	nt	= (int) tr.ns;
	dt   = ((double) tr.dt)/1000000.0;

	if(dt_resamp == 0)
	   dt_resamp=dt;
	
	/* allocate space for max search window */
	nts = (int) (nt-1)*dt/dt_resamp +1.;

	/* aux output 
	diprint(nts);
	diprint(nt);
	dfprint(dt_resamp);
	dfprint(dt);
	dfprint(tmin_in); */


	val_resamp=ealloc1float(nts);
	t_resamp=ealloc1float(nts);
   
	/* initialize val_resamp */
	for(i=0;i<nts;i++)
	   val_resamp[i]=0.0;
	
	/* initialize trace picks */
	if(! initpick(pick1) )
	   err("ERROR initializing local Pick structure");

	/* initialize global picks */
	if(! initpick(gpick) )
	   err("ERROR initializing global Pick structure");
	
	trnum=0;
	
	/* Loop over traces */
	do {
	
		register int itime;

		pick1->iabs=trnum;
		
		trnum++;
		
		/* Get value for second dimension */
		if(d2 == 0)
		{
		   /* get value of key and convert to float */
		   gethval(&tr, index, &val);
		   fval = vtof(type,val);
		} else 
		   fval=x2beg + (trnum-1)*d2;
 
		/* Get t_min in window */
		if(nx==0)
		   tmin=tmin_in;
		else

		  /* linearly interpolate between input values 
			can be taken out of loop for efficiency */
		  intlin(nx,x_above,t_above,t_above[0],t_above[nx-1],
			1,&fval,&tmin);
 
		t1=tmin_in + (nt-1)*dt;

		/* Get t_max in window */	
		if(bfilep!= NULL)
		   intlin(nx,x_below,t_below,t_below[0],t_below[nx-1],
			1,&fval,&tmax);
		else if(wl != 0)
		   tmax=tmin+wl;
		else 
		   tmax=t1;

		/* check for bounds */
		if(tmin <tmin_in)
		   tmin=tmin_in;
		if(tmax >t1)
		   tmax=t1;

		/* debug output */
		if(verbose == 1)
		fprintf(outparfp,"trace %i x=%f t_above=%f t_below=%f \n",
			trnum-1,fval,tmin,tmax);

		/* Compute output times */
		{ 
		   register float tvalue;
		   for (itime=0,tvalue=tmin; tvalue<=tmax; itime++,
			tvalue+=dt_resamp)
			t_resamp[itime] = tvalue;
		}

		/* sinc interpolate new data */
		ints8r(nt, dt, tmin_in, tr.data, 
			0.0, 0.0, itime+1,t_resamp, val_resamp); 
		
	  
		/* search for max,min,abs,energy,tmax,tmin,tabs */
		if(! tracepick(pick1,t_resamp,val_resamp,itime) )
		   err(" ERROR in tracepick ");

		/* normalize energy */
		pick1->energy *=dt_resamp;
		
		if(! globalpick(pick1,gpick) )
		   err(" ERROR in subroutine globalpick");
		
		/* debug output */
		if(verbose == 1)
		{
		fprintf(outparfp,"trace #=%i \t   x2=%e \n",
			pick1->iabs,fval);
		fprintf(outparfp,"max=%e \t tmax=%e \n",
			pick1->max,pick1->tmax);
		fprintf(outparfp,"min=%e \t tmin=%e \n",
			pick1->min,pick1->tmin);		   
		fprintf(outparfp,"abs=%e \t tabs=%e \n",
			pick1->abs,pick1->tabs);		   
		fprintf(outparfp,"energy in pick window =%e \n \n",
			pick1->energy);
		}

		/* column output */
		if(verbose == 2)
		fprintf(outparfp,
			"%i\t %f\t %f\t %f\t"
			"%i\t %e\t "
			"%e\t %e\t "
			"%e\t %e\t "
			"%e\t %e\t "
			"%e\n",
			trnum-1,fval,tmin,tmax,
			pick1->iabs,fval,
			pick1->max,pick1->tmax,
			pick1->min,pick1->tmin,
			pick1->abs,pick1->tabs,
			pick1->energy) ;

		if(! getarg(&t1,mode1,pick1,fval,trnum-1) )
		   err(" ERROR in subroutine <getarg>, argument 1");

		if(! getarg(&x1,mode2,pick1,fval,trnum-1) )
		   err(" ERROR in subroutine <getarg>, argument 2");

		if(modef==1)
		{
		   fprintf(stdout,"%e \t %e\n",t1,x1);
		} else {
		  /* Output raw BINARY pairs to stdout */  
			efwrite(&x1, FSIZE, 1, stdout);
			efwrite(&t1, FSIZE, 1, stdout);
		}
	} while (gettr(&tr));

	/* Output global information */
	fprintf(outparfp,"\nglobal max=%e \t #=%i \t tmax=%e \n",
	   gpick->max,gpick->imax,gpick->tmax);
	fprintf(outparfp,"global min=%e \t #=%i \t tmin=%e \n",
	   gpick->min,gpick->imin,gpick->tmin);
	fprintf(outparfp,"global abs=%e \t #=%i \t tabs=%e \n",
	   gpick->abs,gpick->iabs,gpick->tabs);
	fprintf(outparfp,"max energy in pick window =%e \t #=%i \n \n",
	   gpick->energy,gpick->ienergy);

	if (bfile_set) fclose(bfilep);
	if (afile_set) fclose(afilep);

	/* If output=binary, print number of pairs to outpar */
	if (modef==2)
		fprintf(outparfp, "\n npairs = %d\n", trnum);

	return(CWP_Exit());
}


int tracepick(Pick *pick1, float *t, float *val, int nt)
{
   register  int it;
   float min,max,abs,tmax,tmin,tabs,energy;

   min = FLT_MAX;
   max = -FLT_MAX;
  
   
   tmin = tmax = tabs = t[0];
   abs = 0.0;
   energy=0.0;
   
   for (it=0; it <nt;it++)
   {
	

		if (val[it] > max) { /* Find max */
				max = val[it];
				tmax = t[it];
		}

		if (val[it] < min) { /* Find min */
				min = val[it];
				tmin = t[it];
		}

		if (ABS(val[it]) > abs) {/* Find absmax */
				abs = ABS(val[it]);
				tabs = t[it];
		}

		energy += val[it]*val[it];
   
			
   
		val[it] = 0.0; /* zero data values */

   }
   
   pick1->min=min;
   pick1->max=max;
   pick1->abs=abs;
   pick1->tmin=tmin;
   pick1->tmax=tmax;
   pick1->tabs=tabs;
   pick1->energy=energy;
   
   return 1;
}


int initpick(Pick *pick1)
{
   pick1->min=FLT_MAX;
   pick1->max=FLT_MIN;
   pick1->abs=-.1;
   pick1->tmin=0.;
   pick1->tmax=0.;
   pick1->tabs=0.;
   pick1->energy=-.1;
   pick1->imax=0;
   pick1->imin=0;
   pick1->iabs=0;
   pick1->ienergy=0;  
 
   return 1;
}


int globalpick(Pick *pick1, Pick *gpick)
{
   		if (pick1->max > gpick->max) { /* Find global max */
			gpick->max = pick1->max;
			gpick->tmax = pick1->tmax;
			gpick->imax  = pick1->iabs;
		}

		if (pick1->min < gpick->min) { /* Find global min */
			gpick->min = pick1->min;
			gpick->tmin = pick1->tmin;
			gpick->imin  = pick1->iabs;
		}
		if (pick1->abs > gpick->abs) { /* Find global abs */
			gpick->abs = pick1->abs;
			gpick->tabs = pick1->tabs;
			gpick->iabs  = pick1->iabs;
		}
		if (pick1->energy > gpick->energy) { /* Find global energy */
			gpick->energy = pick1->energy;
			gpick->ienergy  = pick1->iabs;
		}

		return 1;
		
}
int get_mode(char *arg1, int *mode)
{
   if( STREQ(arg1, "max") )
   {
	*mode=1; return 1;
   }
   else if( STREQ(arg1, "tmax") )
   {
	*mode=2; return 1;
   }
   else if( STREQ(arg1, "min") )
   {
	*mode=3; return 1;
   }
   else if( STREQ(arg1, "tmin") )
   {
	*mode=4; return 1;
   }
   else if( STREQ(arg1, "abs") )
   {
	*mode=5; return 1;
   }
   else if( STREQ(arg1, "tabs") )
   {
	*mode=6; return 1;
   }
   else if( STREQ(arg1, "energy") )
   {
	*mode=7; return 1;
   }
   else if( STREQ(arg1, "x2") )
   {
	*mode=8; return 1;
   }	
   else if( STREQ(arg1, "i2") )
   {
	*mode=9; return 1;
   }
   else
	return 0 ;
}

int getarg(float *arg, int mode, Pick *pick, float x2, int trnum)
{
   switch(mode)
   {
	case 1:
		*arg = pick->max; break;
	case 2:
		*arg = pick->tmax; break;
	case 3:
		*arg = pick->min; break;
	case 4:
		*arg = pick->tmin; break;
	case 5:
		*arg = pick->abs; break;
	case 6:
		*arg = pick->tabs; break;
	case 7:
		*arg = pick->energy; break;
	case 8:
		*arg = x2; break;
	case 9:
		*arg = (float) trnum; break;
   }
   return 1;
}
int getformat(char *format, int *modef)	
{
   if (STREQ(format, "ascii") )
   {
	
	*modef=1; return 1;
   }
   else if(STREQ(format, "binary") )
   {
	
	*modef=2; return 1;
   }
   else return 0;
}  
