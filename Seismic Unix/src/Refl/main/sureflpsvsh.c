/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "su.h"
#include "segy.h"
#include "header.h"
#include "Reflect/reflpsvsh.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUREFLPSVSH - REFlectivity modeling of either PSV or SH waves for layered",
"		earth model						",
"									",
" surelfpsvsh  required parameters [optional parameters]	 		",
"									",
" Required Parameters:							",
" m0=			Seismic moment					",
" p2w=			maximum ray parameter value to which the	",
"			synthetics are computed				",
"									",
" Optional Parameters:							",
" Main Flags:								",
" wtype=1		=1 for PSV, =2 for SH				",
" stype=1		=1 if the moemnt tensor components are given	",
"			=2 if they are to be computed from fault	",
"			 plane mechanism parameters			",
" wfield=2		=1 for displacement, =2 for particle velocity,	",
"			=3 for particle acceleration			",
" flt=0			=1 to apply earth flattening correction		",
"			(important for earthquake data)			",
" vsp=0			=0 for surface data, =1 for VSP data		",
" int_type=1		=1 to compute the slowness integration using	",
"			the trapezoidal rule. =2 to use a first order	",
"			Filon scheme (faster but maybe noisier)		",
" verbose=0		=0 no processing information is output (makes	",
"			the program run a litle faster), =1 to output	",
"			processing information to the screen, =2 to	",
"			output processing information to a user		",
"			supplied file, =3 output processing		",
"			information to both, the screen and a file	",
"									",
" Flags for special non-standard options (used only in rare ocasions)	",
" rand=0		=1 to include random velocity and q layers	",
" qopt=0		=1 if a q-correction is desired			",
"									",
" Flags for output data							",
" win=0			=1 to apply a frequency domain Hanning		", 
"			window prior to inverse FFT to time domain	",
" wavelet_type		=1 for a spike, =2 for a Ricker wavelet		",
"			=3 for an akb wavelet				",
"									",
" Main input parameters							",
" tsec=2.048		length of computed traces (in seconds)		",
" dt=0.004		time sampling interval (in seconds)		",
" nt=tsec/dt		number of samples per trace			",
" nx=60			number of traces (ranges) per shot		",
" nw=100		number of frequencies to process		",
" nor=1			number of depths at which receivers are		",
"			located (different from one for VSP's)		",
" nlayers=10		number of horizontal layers in the model	",
" fref=1.0		reference frequency (Hz)			",
" bx=0.0 		first range (first trace offset) (km)		",
" dx=0.05		range increment (trace to trace distance) km	",
" pw1=0.0 pw2=0.1	apply Hanning window tapering to lower end	",
"			of ray parameter computations (if set to zero	",
"			good default values are used)			",
" pw3=6.7 pw4=7.0	apply Hanning window tapering to higher end	",
"			of ray parameter computations (if set to zero	",
"			good default values are used)			",
" fs=0.07		Filon sampling parameter, usually between	",
"			0.07 and 0.12. Sampling is finer as fs		",
"			increases. For short range synthetics		",
"			(<100km) use 0.07, for medium range (<1000km)	",
"			use 0.09 and for large	ranges (>1000km) use	",
"			0.12 (this parameter is ignored if int_type=1)	",
" np=1300		number of ray parameters used to compute the	",
"			seismograms. Automatically set if int_type=2	",
" bp=0.0		slowest ray parameter used to compute the	",
"			seismograms. Set to zero if int_type=2		",
" decay=50.0		decay factor use to avoid time series		",
"			wraparound. A value 'n' for decay means that	",
"			the wrapped around signals are diminished by	",
"			a factor of 'n'. The default of 50 is the	",
"			recomended value				",
" lobs=			array[nor] of layers on top of which the	",
"			receivers are located. If any receiver is	",
"			within a layer, introduce a ficticious layer	",
" cl=			array[nlayers] of compressional velocities	",
"			for each layer in km/s				",
" ct=			array[nlayers] of shear velocities for each	",
"			layer in km/s. A value of -1 assumes		",
"			ct=cl/sqrt(3.					", 
" ql=			array[nlayers] of compressional q values	",
"			for each layer					",
" qt=			array[nlayers] of shear q values for each	",
"			layer. A value of -1 assumes qt=ql/2.		",
" rho=			array[nlayers] of densities for each layer	",
"			in gr/cc					",
" t=			absolute thickness of the layer in kms		",
"									",
" Note: any of these arrays can also be input in a file as lobsfile=,clfile=,",
" ctfile=, etc.								",
"									",
" Source parameters:							", 
" lsource=1		layer on top of which the source is located	",
" h1=1.0 h2=0.0		vertical and horizontal linear part of the	",
"			source. For unit point source set h1=0, h2=0	",
"  			and all other source parameters to zero. For	",
"			earthquake source both h1 and h2 are usually	",
"			zero					 	",
" m1,m2,m3		[1][1],[1][2] and [2][2] components of the	",
"			moment tensor ([1][2]=[2][1] component)		",	
" delta=0.0		dip in degrees (necessary only if stype=1)	",
" lambda=0.0		rake in degrees (necessary only if stype=1)	",
" phis=0.0		fault plane azimuth in degrees (necessary	",
"			only if stype=1)				",
" phi=0.0		azimuth of the receiver location in degrees	",
"			(necessary only if stype=1)		  	",
" Parameters for output data:						",
" tlag=0.0		time lag to appy to the  seismograms (sec)	",
" nf=nw			number of frequencies in output data		",
" fpeak=25.0		peak frequency for Ricker or akb wavelet (hz)	",
" red_vel=0.0		reducing velocity (km/s). If set to zero, the	",
"			maximum compressional velocity is used		",
"			(this parameter is garanteed not to be <1.5)	",
" w1=0.0		A Hanning window will be applied to freqs	",
"			less than w1.If set to zero, the default of	",
"			0.15 of maximum frequency is used (provided	",
"			that win=1)					",
" w2=0.0		A Hanning window will be applied to freqs	",
"			greater than w2. If set to zero, the default	",
" 			0.85 of maximum frequency is used (provided	",
"			that win=1)					",
" nfilters=0		number of filters to apply to the synthetics	",
" filters_phase=	array[nfilters] of: 0 for zero phase filters	", 
" 			or 1 for minimum phase filters(can also be	",
"			input in a file via fphfile=			",
" filters_type=		array[nfilters] of: 1 for high cut filters,	",
"			input in a file via filphasefile=		",
" 			2 for low cut filters, 3 for notch filters	",
"			(can also be input in a file via filtypefile=)	",
" dbpo=			array[nfilters] of: filter slopes in db/oct	",
"			(can also be input in a file via dbpofile=)	",
" f1=			array[nfilters] of: frequency to start filter	",
"			action (Hz)(can also be input in a file via	",
"			f1file=)					",
" f2=			array[nfilters] of: frequency to end filter	",
"			action (Hz). Only for notch filters		",
"			(can also be input in a file via f2file=)	",
" wfp=			name of output pressure seismogram file		",
"			(only if wtype=1)				",
" wfr=			name of output radial comp seismogram file	",
"			(only if wtype=1)				",
" wfz=			name of output vertical comp seismogram file	",
"			(only if wtype=1)				",
" wft=			name of output tangential comp seismogram file	",
"			(only if wtype=1)			  	",
" outf=info		name of output processing information file	",
"			(only if verbos=2 or 3)				",
"									",
" Interpolation parameters (required only if layers with gradients desired)",
" nlint=0		number of times layer interp is required	",
" nintlayers=		array[nlint] of number of layers to interpol	",
"			each time (can be input as file nintlayfile=)	", 
" intlayers=		array[nlint] of layers on top of which to	",
"			start each interpolation			",
"			(can be input as file intlayfile=)		",
" intlayth=		array[nlint] of layer thicknesses to interp	",
"			(can be input as file intlaythfile=)		",
"									",
" Other parameters (required only under very special circumstances)	",
" nrand_layers=0	maximum number of random layers allowed		",
"			(only if rand=1)				",
" layer=0		layer on top of which the random velocity	",
"			layers are inserted (only if rand=1)		",
" zlayer=0.0		thickness of random layers (only if rand=1)	",
"   			(if zlayer<t[il], then zlayer=t[il])		",
" sdcl=0.0		standard deviation for compressional vels	",
"			(only if rand=1)				",
" sdct=0.0		standard deviation for shear velocities		",
"			(only if rand=1)				",
" layern=0		layer on top of which the q-option is invoked	",
"			(only if qopt=1)				",
" wrefp=1.0		reference frequency for compressional vels	",
"			(only if qopt=1)				",
" wrefs=1.0		reference frequency for shear velocities	",
"			(only if qopt=1)				",
" epsp=0.001		reference amplitude for comporessional vels	",
"			(only if qopt=1)				",
" epss=0.001		reference amplitude for shear velocities	",
"			(only if qopt=1)				",
" sigp=0.1		xxxxxx for comporessional vels			",
"			(only if qopt=1)				",
" sigs=0.1		xxxxxx for shear velocities		  	",
"			(only if qopt=1)				",
" Notes:								",
" Gradient zones between two layers can be handled with the use of the	",
" layer interpolation option. The program will automatically compute and",
" insert the required number of layers with the appropriate thicknesses	",
" between the layers above and below. There is no restriction as to how	",
" many layers the program can handle, provided enough computer power is	",
" available.								",
"									",
" The number of frequencies to be processed is the most critical parameter",
" to determine how long the program will take to run. The maximum frequency",
" that can be present in the seismogram is nw/tsec Hz, however, a lower	",
" frequency can be selected for the output via the nf param.		",
"									",
" The number of computed time samples is tsec/dt, however, the user may	",
" choose a smaller number of samples for the output traces by setting	",
" nt this can be useful when a short seismogram is required with a	",
" broadband frequency range (directly computing a small seismogram can	",
" be hazardous)								",
"									",
" The decay parameter should be chosen with care. A value of 50 seems	",
" to give good results, but depending on the data this parameter can	",
" boost up late wrapped around noncausal energy.			", 
"									",
" The integration flag is important for short range data, in particular ",
" for oil exploration, for which the first order Filon scheme, though	",
" faster, can produce noisier seismograms, specialy in the late parts of",
" the record. A standard trapezoidal rule seems to work better but is	",
" slower by about 25% in a normal situation.				",
"									",
" When arrays are required as input, they can also be input as files,	",
" however, the program will only check that the number of parameters is ",
" the same if the arrays are used or if the number of elements in the	",
" files are set. A combination of arrays and files is also permited.	",
"									",
" Examples of Source Parameters:					",
" For a vertical point force, set stype=1, h1=1, h2=0 and all moment	",
" tensor components to zero. Ignore all other source parameters.	",
" For an explosion, set stype=1, h1=h2=0, m1=m3=A, m2=0 where A is some	",
" constant (normally one). If A is negative, an implosion is generated	",
" instead.  Ignore all other source parameters.				",
" For a fault slip, set stype=2, set phis,phi,lambda,alpha and m0 to their",
" corresponding values and ignore the moment tensor components		",
"									",
NULL};

/*
 * Credits:
 *
 * Original Fortran 77 PSV version written by Subshashis Mallick (1988)
 * Original Fortran 77 SH version written by Mrinal Sen (1988) based on the 
 * PSV version by Subshashis Mallick 
 * Translated to C, expanded and reformatted for SU by Gabriel Alvarez (1995)
 *
 * References:
 * The reflectivity method: a tutorial. G Muller. J. Geophysics(1985)
 *	v. 58.  153-174
 * Practical aspects of reflectivity modeling. Mallick and Frazer. Geophysics
 *	v. 52 No. 10. October 1987. 1355-1364
 *
 */
/************************** end self doc *************************************/

#define RSO 6371.0

segy tr1,tr2,tr3;


int main(int argc, char **argv)
{
	int i,ix,it;		/* loop counters */
	int wtype;		/* =1 psv. =2 sh wavefields */
	int wfield;		/* =1 displcement =2 velocity =3 acceleration */
	int stype;		/* source type */
	int int_type;		/* =1 for trapezoidal rule. =2 for Filon */
	int flt;		/* =1 apply earth flattening correction */
	int rand;		/* =1 for random velocity layers */
	int qopt;		/* some flag ???? */
	int vsp;		/* =1 for vsp, =0 otherwise */
	int win;		/* =1 if frequency windowing required */
	int verbose;		/* flag to output processing information */
	int nt;			/* samples per trace in output traces */
	int ntc;		/* samples per trace in computed traces */
	int nx;			/* number of output traces */
	int np;			/* number of ray parameters */
	int nlint=0;		/* number of times layer interp is required */
	int lsource;		/* layer on top of which the source is located*/
	int nw;			/* number of frequencies */
	int nor;		/* number of receivers */
	int nlayers;		/* number of reflecting layers */
	int layern;
	int nrand_layers;	/* maximum number of random layers permitted */
	int nf;			/* number of frequencies in output traces */
	int *filters_phase=NULL;	/* =0 for zero phase, =1 for minimum phase fil*/
	int nfilters;		/* number of required filters */
	int wavelet_type;	/* =1 spike =2 ricker1 =3 ricker2 =4 akb */

	float dt;		/* time sampling interval */
	float tsec;		/* trace length in seconds */
	float fpeak;		/* peak frequency for output wavelet */
	float fref;		/* first frequency */
	float p2w;		/* maximum ray parameter value */
	float bp;		/* smallest ray parameter (s/km) */
	float bx;		/* beginning of range in Kms. */
	float fx;		/* final range in Kms. */
	float dx;		/* range increment in Kms. */
	float pw1,pw2,pw3,pw4;	/* window ray parameters (to apply taper) */
	float h1;		/* horizontal linear part of the source */ 
	float h2;		/* vertical linear part of the source */ 
	float m0;		/* seismic moment */
	float m1,m2,m3;		/* components of the moment tensor */

	float delta;		/* dip */
	float lambda;		/* rake */
	float phis;		/* azimuth of the fault plane */
	float phi;		/* azimuth of the receiver location */

	float sdcl,sdct;	/* standar deviation for p and s-wave vels */
	float z0=0.0;		/* reference depth */
	float zlayer;		/* thickness of random layers */
	int layer;		/* layer over on top of which to compute rand*/
	float tlag;		/* time lag in output traces */
	float red_vel;		/* erducing velocity */

	float w1=0.0;		/* low end frequency cutoff for taper */
	float w2=0.0;		/* high end frequency cutoff for taper */
	float wrefp;		/* reference frequency for p-wave velocities */
	float wrefs;		/* reference frequency for s-wave velocities */

	float epsp;		/* .... for p-wave velocities */
	float epss;		/* .... for p-wave velocities */
	float sigp;		/* .... for p-wave velocities */
	float sigs;		/* .... for s-wave velocities */
	float fs;		/* sampling parameter, usually 0.07<fs<0.12 */
	float decay;		/* decay factor to avoid wraparound */

	int *lobs;		/* layers on top of which lay the receivers */
	int *nintlayers=NULL;	/* array of number of layers to interpolate */
	int *filters_type;	/* array of 1 lo cut, 2 hi cut, 3 notch */

	float *dbpo=NULL;	/* array of filter slopes in db/octave */
	float *f1=NULL;		/* array of lo frequencies for filters */
	float *f2=NULL;		/* array of high frequencies for filters */
	float *cl;		/* array of compressional wave velocities */
	float *ql;		/* array of compressional Q values */
	float *ct;		/* array of shear wave velocities */
	float *qt;		/* array of shear Q values */
	float *rho;		/* array of densities */
	float *t;		/* array of absolute layer thickness */

	int *intlayers=NULL;	/* array of layers to interpolate */

	float *intlayth=NULL;	/* array of thicknesses over which to interp */
	float **wavefield1;	/* array for pressure wavefield component */
	float **wavefield2=NULL;/* array for radial wavefield component */
	float **wavefield3=NULL;/* array for vertical wavefield component */

	char *lobsfile="";	/* input file receiver layers */
	char *clfile="";	/* input file of p-wave velocities */
	char *qlfile="";	/* input file of compressional Q-values */
	char *ctfile="";	/* input file of s-wave velocities */
	char *qtfile="";	/* input file of shear Q-values */
	char *rhofile="";	/* input file of density values */
	char *tfile="";		/* input file of absolute layer thicknesses */
	char *intlayfile="";	/* input file of layers to interpolate */
	char *nintlayfile="";	/* input file of number of layers to interp */
	char *intlaythfile="";	/*input file of layer thickness where to inter*/
	char *filtypefile="";	/* input file of filter types to apply */
	char *fphfile="";	/* input file of filters phases */
	char *dbpofile="";	/* input file of filter slopes in db/octave */
	char *f1file="";	/* input file of lo-end frequency */
	char *f2file="";	/* input file of hi-end frequency */

	char *wfp="";		/* output file of pressure */
	char *wfr="";		/* output file of radial wavefield */
	char *wfz="";		/* output file of vertical wavefield */
	char *wft="";		/* output file of tangential wavefield */
	char *outf="";		/* output file for processing information */

	FILE *wfp_file;		/* file pointer to output pressure */
	FILE *wfr_file;		/* file pointer to output radial wavefield */
	FILE *wfz_file;		/* file pointer to output vertical wavefield */
	FILE *wft_file;		/* file pointer to output tangential wavefield*/
	FILE *outfp=NULL;	/* file pointer to processing information */
	FILE *infp;		/* file pointer to input information */

	
	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);			/* no input data */

	/* get required parameter, seismic moment */
	if (!getparfloat("m0",&m0))	
		err("error: the seismic moment, m0, is a required parameter\n");

	/*********************************************************************/
	/* get general flags and set their defaults */
	if (!getparint("rand",&rand))			rand	= 0;
	if (!getparint("qopt",&qopt))			qopt	= 0;
	if (!getparint("stype",&stype))			stype	= 1;
	if (!getparint("wtype",&wtype))			wtype	= 1;
	if (!getparint("wfield",&wfield))		wfield	= 1;
	if (!getparint("int_type",&int_type))		int_type= 1;
	if (!getparint("flt",&flt))			flt	= 0;
	if (!getparint("vsp",&vsp))			vsp	= 0;
	if (!getparint("win",&win))			win	= 0;
	if (!getparint("wavelet_type",&wavelet_type))	wavelet_type = 1;
	if (!getparint("verbose",&verbose))		verbose	= 0;

	/* get model parameters and set their defaults */
	if (!getparint("lsource",&lsource))		lsource = 0;
	if (!getparfloat("fs",&fs)) 			fs	= 0.07;
	if (!getparfloat("decay",&decay))		decay	= 50.0;
	if (!getparfloat("tsec",&tsec))			tsec	= 2.048;

	/* get response parameters and set their defaults */
	if (!getparfloat("fref",&fref))			fref	= 1.0;
	if (!getparint("nw",&nw))			nw	= 100;
	if (!getparint("nor",&nor))			nor	= 100;
	if (!getparint("np",&np))			np	= 1300;
	if (!getparfloat("p2w",&p2w))			p2w	= 5.0;
	if (!getparfloat("bx",&bx))			bx	= 0.005;
	if (!getparfloat("bp",&bp))			bp	= 0.0;
	if (!getparfloat("fx",&fx))			fx	= 0.1;
	if (!getparfloat("dx",&dx))			dx	= 0.001;
	if (!getparfloat("pw1",&pw1))			pw1	= 0.0;
	if (!getparfloat("pw2",&pw2))			pw2	= 0.1;
	if (!getparfloat("pw3",&pw3))			pw3	= 6.7;
	if (!getparfloat("pw4",&pw4))			pw4	= 7.0;
	if (!getparfloat("h1",&h1))			h1	= 1.0;
	if (!getparfloat("h2",&h2))			h2	= 0.0;

	/* get output parameters and set their defaults */
	if (!getparint("nx",&nx))			nx	= 100;
	if (!getparfloat("dt",&dt))			dt	= 0.004;
	if (!getparint("nt",&nt))			nt	= tsec/dt;
	if (!getparint("nf",&nf))			nf	= 50;
	if (!getparfloat("red_vel",&red_vel))		red_vel	= 5;
	if (!getparfloat("fpeak",&fpeak))		fpeak	= 25.;
	if (!getparfloat("tlag",&tlag))			tlag	= 0.;

     if (!getparstring("clfile",&clfile))    clfile="";
     if (!getparstring("ctfile",&ctfile))    ctfile="";
     if (!getparstring("qtfile",&qtfile))    qtfile="";
     if (!getparstring("qlfile",&qlfile))    qlfile="";
     if (!getparstring("rhofile",&rhofile))  rhofile="";
     if (!getparstring("tfile",&tfile))      tfile="";

	/* get names of output files */
	if (wtype==1) {
		getparstring("wfp",&wfp);
		getparstring("wfr",&wfr);
		getparstring("wfz",&wfz);
	} else if (wtype==2) {
		getparstring("wft",&wft);
	} else err ("wtype has to be zero or one");

	/*********************************************************************/
	/* get or compute moment tensor components */
	if (stype==1) {

		/* get source parameters */
		if (!getparfloat("delta",&delta))	
			err("if stype==1, delta is a required parameter\n");
		if (!getparfloat("lambda",&lambda))	
			err("if stype==1, lambda is a required parameter\n");
		if (!getparfloat("phis",&phis))	
			err("if stype==1, phis is a required parameter\n");
		if (!getparfloat("phi",&phi))	
			err("if stype==1, phi is a required parameter\n");

		/* compute moment tensor components */
		compute_moment_tensor (wtype, phi, lambda, delta, phis, m0, 
			&m1, &m2, &m3);

	} else if (stype==2) {

		/* get moment tensor components from input */	
		if (!getparfloat("m1",&m1))	
			err("if stype==2, m1 is a required parameter\n");
		if (!getparfloat("m2",&m2))	
			err("if stype==2, m2 is a required parameter\n");
		if (!getparfloat("m3",&m3))	
			err("if stype==2, m3 is a required parameter\n");

	} else err("error, stype flag has to be one or two\n");

	/*********************************************************************/
	/* if q-option is not requesed, set corresponding parameters to zero */
	if (!getparint("layern",&layern))		layern	=0;	
	if (!getparfloat("wrefp",&wrefp))		wrefp	=0.0;
	if (!getparfloat("wrefs",&wrefs))		wrefs	=0.0;
	if (!getparfloat("epsp",&epsp))			epsp	=0.0;
	if (!getparfloat("epss",&epss))			epss	=0.0;
	if (!getparfloat("sigp",&sigp))			sigp	=0.0;
	if (!getparfloat("sigs",&sigs))			sigs	=0.0;

	/*********************************************************************/
	/* get number of layers and check input parameters */
	if (*clfile=='\0') {	/* p-wave vels input from the comand line */
		nlayers=countparval("cl");
	} else  {		/* p-wave vels input from a file */
		getparint("nlayers",&nlayers);
	}
	if (*ctfile=='\0') {	/* s-wave vels input from the comand line */
		if (nlayers !=countparval("cl")) 
			err("number of p-wave and s-wave velocities"
				"has to be the same");
	}
	if (*qlfile=='\0') { 	/* compressional q-values from comand line */
		if (nlayers !=countparval("ql")) 
			err("number of p-wave velocities and q-values"
				"has to be the same");
	}
	if (*qtfile=='\0') { 	/* shear q-values input from comand line */
		if (nlayers !=countparval("qt")) 
			err("number of p-wave velocities and shear q-values"
				"has to be the same");
	}
	if (*rhofile=='\0') { 	/* densities input from comand line */
		if (nlayers !=countparval("rho")) 
			err("number of p-wave velocities and densities"
				"has to be the same");
	}
	if (*tfile=='\0') { 	/* layer thicknesses input from comand line */
		if (nlayers !=countparval("t")) 
			err("number of p-wave velocities and thicknesses"
				"has to be the same");
	}
	if (int_type!=1 && int_type!=2) err("int_type flag has to be one or two");

	/*********************************************************************/
	/* if layer interpolation is requested, get parameters */
	if (*intlayfile !='\0') {
		getparint("nlint",&nlint);
		if ((infp=efopen(intlayfile,"r"))==NULL)
			err("cannot open file of layer interp=%s\n",intlayfile);
		intlayers=alloc1int(nlint);
		fread (intlayers,sizeof(int),nlint,infp);
		efclose(infp);
	} else if (countparval("intlayers") !=0) {
		nlint=countparval("intlayers");
		intlayers=alloc1int(nlint);
		getparint("intlayers",intlayers);
	}
	if (*nintlayfile !='\0') {
		if ((infp=efopen(nintlayfile,"r"))==NULL)
			err("cannot open file of layer inter=%s\n",nintlayfile);
		nintlayers=alloc1int(nlint);
		fread (nintlayers,sizeof(int),nlint,infp);
		efclose(infp);
	} else if (countparval("nintlayers") !=0) {
		if (nlint !=countparval("nintlayers")) 
			err("number of values in intlay and nintlay not equal");
		nintlayers=alloc1int(nlint);
		getparint("nintlayers",nintlayers);
	}
	if (*intlaythfile !='\0') {
		if ((infp=efopen(intlaythfile,"r"))==NULL)
			err("cannot open file=%s\n",intlaythfile);
		intlayth=alloc1float(nlint);
		fread (intlayth,sizeof(int),nlint,infp);
		efclose(infp);
	} else if (countparval("intlayth") !=0) {
		if (nlint !=countparval("intlayth")) 
			err("# of values in intlay and intlayth not equal");
		intlayth=alloc1float(nlint);
		getparfloat("intlayth",intlayth);
	}
	/* update total number of layers */
	if (nlint!=0) {
		for (i=0; i<nlint; i++) nlayers +=intlayers[i]-1;
	}
		
	/*********************************************************************/
	/* if random velocity layers requested, get parameters */
	if (rand==1) {
		getparint("layer",&layer);
		getparint("nrand_layers",&nrand_layers);
		getparfloat("zlayer",&zlayer);
		getparfloat("sdcl",&sdcl);
		getparfloat("sdct",&sdct);
	} else nrand_layers=0;	

	/*********************************************************************/
	/* allocate space */
	getparint("nlayers",&nlayers);
     fprintf(stderr," Number of Layers %i \n",nlayers);

	cl = alloc1float(nlayers+nrand_layers);
	ct = alloc1float(nlayers+nrand_layers);
	ql = alloc1float(nlayers+nrand_layers);
	qt = alloc1float(nlayers+nrand_layers);
	rho = alloc1float(nlayers+nrand_layers);
	t = alloc1float(nlayers+nrand_layers);
	lobs = alloc1int(nor+1);
	lobs[nor]=0;

	/* get name of output file for processing information */
	if (verbose==2||verbose==3) {
		if (!getparstring("outf",&outf))	outf="info";
		if ((outfp=efopen(outf,"w"))==NULL) {
			warn("cannot open processing file =%s, no processing\n"
			"information file will be generated\n",outf);
			verbose=1;
		}
	}

	/*********************************************************************/
	/* read  input parameters from files or command line */
	fprintf(stderr,"clfile= %s\n",clfile);
	fprintf(stderr,"ctfile= %s\n",ctfile);
	fprintf(stderr,"rhofile= %s\n",rhofile);
	fprintf(stderr,"tfile= %s\n",tfile);
	fprintf(stderr,"qlfile= %s\n",qlfile);
	fprintf(stderr,"qtfile= %s\n\n",qtfile);
	fprintf(outfp,"clfile= %s\n",clfile);
	fprintf(outfp,"ctfile= %s\n",ctfile);
	fprintf(outfp,"rhofile= %s\n",rhofile);
	fprintf(outfp,"tfile= %s\n",tfile);
	fprintf(outfp,"qlfile= %s\n",qlfile);
	fprintf(outfp,"qtfile= %s\n\n",qtfile);
	if (*clfile !='\0') {			/* read from a file */	
	     fprintf(stderr,"In loop clfile= %s\n",clfile);
		if ((infp=efopen(clfile,"r"))==NULL)
			err("cannot open file of pwave velocities=%s\n",clfile);
		fread(cl,sizeof(float),nlayers,infp);
		efclose(infp);
	} else getparfloat("cl",cl);		/* get from command line */
	if (*qlfile !='\0') {
		if ((infp=efopen(qlfile,"r"))==NULL)
			err("cannot open file of compressional Q=%s\n",qlfile);
		fread(ql,sizeof(float),nlayers,infp);
		efclose(infp);
	} else getparfloat("ql",ql);
	if (*ctfile !='\0') {
		if ((infp=efopen(ctfile,"r"))==NULL)
			err("cannot open file of swave velocities=%s\n",ctfile);
		fread(ct,sizeof(float),nlayers,infp);
		efclose(infp);
	} else getparfloat("ct",ct);
	if (*qtfile !='\0') {
		if ((infp=efopen(qtfile,"r"))==NULL)
			err("cannot open file of shear Q=%s\n",qtfile);
		fread(qt,sizeof(float),nlayers,infp);
		efclose(infp);
	} else getparfloat("qt",qt);
	if (*rhofile !='\0') {
		if ((infp=efopen(rhofile,"r"))==NULL)
			err("cannot open file of densities=%s\n",rhofile);
		fread(rho,sizeof(float),nlayers,infp);
		efclose(infp);
	} else getparfloat("rho",rho);
	if (*tfile !='\0') {
		if ((infp=efopen(tfile,"r"))==NULL)
			err("cannot open file of thicknesses=%s\n",tfile);
		fread(t,sizeof(float),nlayers,infp);
		efclose(infp);
	} else getparfloat("t",t);
	if (*lobsfile !='\0') {
		if ((infp=efopen(lobsfile,"r"))==NULL)
			err("can't open file of receiver layers=%s\n",lobsfile);
		fread(lobs,sizeof(int),nor,infp);
		efclose(infp);
	} else getparint("lobs",lobs);


	/*********************************************************************/
	/* if requested, do interpolation and/or parameter adjustment */
	if (nlint!=0)
		parameter_interpolation (nlayers, intlayers, nintlayers, 
				intlayth, cl, ql, ct, qt, rho, t);	

	/* if requested, compute random velocity layers */
	if (rand==1) {
		random_velocity_layers (&nlayers, &lsource, nrand_layers, sdcl,
			sdct, layer, zlayer, cl, ql, ct, qt, rho, t);
	}

	/* if requested, apply earth flattening approximation */
	if (flt==1) {
		apply_earth_flattening (nlayers, z0, cl, ct, rho, t);
	}


	/*********************************************************************/
	/* get filter parameters */
	if (*filtypefile !='\0') {
		if ((infp=efopen(filtypefile,"r"))==NULL)
			err("cannot open file=%s\n",filtypefile);
		getparint("nfilters",&nfilters);
		filters_type=alloc1int(nfilters);
		fread (filters_type,sizeof(int),nfilters,infp);
		efclose(infp);
	} else {
		nfilters=countparval("filters_type");
		filters_type=alloc1int(nfilters);
		getparint("filters_type",filters_type);
	}
	if (*fphfile !='\0') {
		if ((infp=efopen(fphfile,"r"))==NULL)
			err("cannot open file=%s\n",fphfile);
		filters_phase=alloc1int(nfilters);
		fread (filters_phase,sizeof(float),nfilters,infp);
		efclose(infp);
	} else if (nfilters == countparval("filters_phase")) {
		filters_phase=alloc1int(nfilters);
		getparint("filters_phase",filters_phase);
	} else err("number of elements infilterstype and phase must be equal");
	if (*dbpofile !='\0') {
		if ((infp=efopen(dbpofile,"r"))==NULL)
			err("cannot open file=%s\n",dbpofile);
		dbpo=alloc1float(nfilters);
		fread (dbpo,sizeof(float),nfilters,infp);
		efclose(infp);
	} else if (nfilters == countparval("dbpo")) {
		dbpo=alloc1float(nfilters);
		getparfloat("dbpo",dbpo);
	} else err("number of elements in filters_type and dbpo must be equal");
	if (*f1file !='\0') {
		if ((infp=efopen(f1file,"r"))==NULL)
			err("cannot open file=%s\n",f1file);
		f1=alloc1float(nfilters);
		fread (f1,sizeof(float),nfilters,infp);
		efclose(infp);
	} else if (nfilters == countparval("f1")) {
		f1=alloc1float(nfilters);
		getparfloat("f1",f1);
	} else err("number of elements in filters_type and f1 must be equal");
	if (*f2file !='\0') {
		if ((infp=efopen(f2file,"r"))==NULL)
			err("cannot open file=%s\n",f2file);
		f2=alloc1float(nfilters);
		fread (f2,sizeof(float),nfilters,infp);
		efclose(infp);
	} else if (nfilters == countparval("f2")) {
		f2=alloc1float(nfilters);
		getparfloat("f2",f2);
	} else err("number of elements in filters_type and f2 must be equal");
		

	/*********************************************************************/
	/* allocate space for wavefield computations */
	wavefield1=alloc2float(nt,nx);
	if (wtype==1) {
		wavefield2=alloc2float(nt,nx);
		wavefield3=alloc2float(nt,nx);
	}

	/* get name of output file for processing information 
	if (verbose==2||verbose==3) {
		if (!getparstring("outf",&outf))	outf="info";
		if ((outfp=efopen(outf,"w"))==NULL) {
			warn("cannot open processing file =%s, no processing\n"
			"information file will be generated\n",outf);
			verbose=1;
		}
	}
	*/

	/* initialize wavefields */
	if (wtype==1) {
		for (ix=0;ix<nx;ix++) {
			for (it=0;it<nt;it++) {
				wavefield1[ix][it]=0.0;
				wavefield2[ix][it]=0.0;
				wavefield3[ix][it]=0.0;
			}
		}
	} else if (wtype==2) {
		for (ix=0;ix<nx;ix++) {
			for (it=0;it<nt;it++) {
				wavefield1[ix][it]=0.0;
			}
		}
	}

	/* number of time samples in computed traces */
	ntc=tsec/dt;
	if (int_type==2) bp=0.0;
	if (verbose==2||verbose==3) {
	    fprintf(outfp,"lay # Vp  Vs rho thickness Qp Qs \n"); /* Wraning check */
	}
	fprintf(stderr,"lay # Vp  Vs rho thickness Qp Qs \n"); /* Wraning check */
	for (i=0;i< nlayers;i++) {
	    fprintf(stderr,"%i    %5.2f %5.2f %5.2f %5.2f   %5.2f  %5.2f \n",i,cl[i],ct[i],rho[i],t[i],ql[i],qt[i]); /* Wraning check */
	    if (verbose==2||verbose==3) {
	         fprintf(outfp,"%i    %5.2f %5.2f %5.2f %5.2f   %5.2f  %5.2f \n",i,cl[i],ct[i],rho[i],t[i],ql[i],qt[i]); /* Wraning check */
	    }
	}
	/*********************************************************************/
	/* Now, compute the actual reflectivities */
	compute_reflectivities (int_type, verbose, wtype, wfield, vsp, flt,
		win, nx, nt, ntc, nor, nf, nlayers, lsource, layern, nfilters,
		filters_phase, nw, np, bp, tlag, red_vel, w1, w2, fx, dx, bx,
		fs, decay, p2w, tsec, fref, wrefp, wrefs, epsp, epss, sigp,
		sigs, pw1, pw2, pw3, pw4, h1, h2, m1, m2, m3, fref, lobs,
		filters_type, dbpo, f1, f2, cl, ct, ql, qt, rho, t, wavefield1,
		wavefield2, wavefield3, outfp);
	/*********************************************************************/

	/* if open, close processing information file */
	if (verbose==2||verbose==3) efclose(outfp);

	/* convolve with a wavelet and write the results out */
	if (wtype==1) {			/* PSV */
		
		/* convolve with a wavelet to produce the seismograms */
		convolve_wavelet (wavelet_type, nx, nt, dt, fpeak, wavefield1); 
		convolve_wavelet (wavelet_type, nx, nt, dt, fpeak, wavefield2); 
		convolve_wavelet (wavelet_type, nx, nt, dt, fpeak, wavefield3); 

		/* output results in SU format */
		if(*wfp!='\0'){
			if ((wfp_file=efopen(wfp,"w"))==NULL)
				err("cannot open pressure file=%s\n",wfp);
			{	register int ix;
				for (ix=0; ix<nx; ix++) {
					for (it=0; it<nt; it++)
						tr1.data[it]=wavefield1[ix][it];

					/* headers*/
					tr1.ns=nt;
					tr1.dt=1000*(int)(1000*dt);
					tr1.offset=(bx+ix*dx)*1000;
	
					/* output trace */
					fputtr(wfp_file, &tr1);
				}
				efclose (wfp_file);
			}
		}
		if (*wfr !='\0') {
			if ((wfr_file=efopen(wfr,"w"))==NULL)
					err("cannot open radial wfield file=%s\n",wfr);
			{	register int ix;
				for (ix=0; ix<nx; ix++) {
					for (it=0; it<nt; it++)
						tr2.data[it]=wavefield2[ix][it];
					tr2.ns=nt;
					tr2.dt=1000*(int)(1000*dt);
					tr2.offset=(bx+ix*dx)*1000;
					fputtr(wfr_file, &tr2);
				}
				efclose (wfr_file);
			}
		}
		if (*wfz !='\0') {
			if ((wfz_file=efopen(wfz,"w"))==NULL)
				err("canno open vertical field file=%s\n",wfz);
			{	register int ix;
				for (ix=0; ix<nx; ix++) {
					for (it=0; it<nt; it++)
							tr3.data[it]=wavefield3[ix][it];
					tr3.ns=nt;
					tr3.dt=1000*(int)(1000*dt);
					tr3.offset=(bx+ix*dx)*1000;
					fputtr(wfz_file, &tr3);
				}
				efclose (wfz_file);
			}
		}
		
		/* free allocated space */
		free2float(wavefield1);
		free2float(wavefield2);
		free2float(wavefield3);

	} else if (wtype==2) {			/* SH */

		/* convolve with a wavelet to produce the seismogram */
		convolve_wavelet (wavelet_type, nx, nt, dt, fpeak, wavefield1); 

		/* output the result in SU format */
		if (*wft !='\0') {
			if ((wft_file=efopen(wft,"w"))==NULL)
				err("cannot open tangential file=%s\n",wft);
			{	register int ix;
				for (ix=0; ix<nx; ix++) {
					for (it=0; it<nt; it++)
							tr1.data[it]=wavefield1[ix][it];
					tr1.ns=nt;
					tr1.dt=1000*(int)(1000*dt);
					tr1.offset=(bx+ix*dx)*1000;
					fputtr(wft_file, &tr1);
				}
				efclose (wft_file);
			}
		}

		/* free allocated space */
		free2float(wavefield1);
	}

	/* free workspace */
	free1float(cl);
	free1float(ct);
	free1float(ql);
	free1float(qt);
	free1float(rho);
	free1float(t);
	free1int(lobs);
	free1int(filters_type);
	free1int(filters_phase);
	free1float(dbpo);
	free1float(f1);
	free1float(f2);
	return EXIT_SUCCESS;
}
