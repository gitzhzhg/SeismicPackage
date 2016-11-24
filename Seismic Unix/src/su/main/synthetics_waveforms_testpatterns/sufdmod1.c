/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUFDMOD1: $Revision: 1.4 $ ; $Date: 2011/11/12 00:40:42 $	*/

#include "par.h"
#include "su.h"
#include "segy.h"

/*********************** self documentation ********************************/
char *sdoc[] = {
"									",
" SUFDMOD1 - Finite difference modelling (1-D 1rst order) for the	",
" acoustic wave equation						"
"									",
" sufdmod1 <vfile >sfile nz= tmax= sz= [optional parameters]		",
"									",
" Required parameters :							",
" <vfile or vfile=	binary file containing velocities[nz]		",
" >sfile or sfile=	SU file containing seimogram[nt]		",
" nz=		 number of z samples				   	",
" tmax=		maximum propagation time				",
" sz=		 z coordinate of source					",
"									",
" Optional parameters :							",
" dz=1	   z sampling interval						",
" fz=0.0	 first depth sample					",
" rz=1	   coordinate of receiver					",
" sz=1	   coordinate of source						",
" dfile=	 binary input file containing density[nz]		",
" wfile=	 output file for wave field (snapshots in a SU trace panel)",
" abs=0,1	absorbing conditions on top and bottom			",
" styp=0	 source type (0: gauss, 1: ricker 1, 2: ricker 2)	",
" freq=15.0	approximate source center frequency (Hz)		",
" nt=1+tmax/dt   number od time samples (dt determined for numerical	",
" stability)								",
" zt=1	   trace undersampling factor for trace and snapshots	 	",
" zd=1	   depth undersampling factor for snapshots		   	",
" press=1	to record the pressure field; 0 records the particle	",
"		velocity						",
" verbose=0	=1 for diagnostic messages				",
"									",
" Notes :								",
"  This program uses a first order explicit velocity/pressure  finite	",
"  difference equation.							",
"  The source function is applied on the pressure component.		",
"  If no density file is given, constant density is assumed	 	",
"  Wavefield  can be easily viewed with suximage, user must provide f2=0",
"  to the ximage program in order to  get correct time labelling	",
"  Seismic trace is shifted in order to get a zero phase source		",
"  Source begins and stop when it's amplitude is 10^-4 its maximum	",
"  Time and depth undersampling only modify the output trace and snapshots.",
"  These parameters are useful for keeping snapshot file small and	",
"  the number of samples under SU_NFLTS.				",
"									",
NULL  };

float source (float t, int styp, float dt, float dz, float t0, float alpha);

int main (int argc, char **argv)
{
	float *rv;	/* array of rock velocity from cfile */
	float *rd;	/* array of rock density from dfile on p knots */
	float *rd1_5;	/* array of rock density from dfile on v knots */
	float *p;	/* pressure */
	float *v;	/* particle velocity */
	float tmax, dt, t0;	/* maximum time , time step,  		*/
				/* time delay for near causal source	*/
	float vmax;		/* maximum rock velocity		*/
	int verbose;		/* is verbose?				*/
	int nz, nt;		/* number of z samples, time samples	*/
	float fz, dz;		/* first sample depth spatial depth	*/
	float sz;		/* source coordinate			*/
	int abs[2];		/* array of absorbing conditions	*/
	int isz;		/* source location index		*/
	float rz;		/* receiver depth */
	int irz;		/* zcoordinate (in samples) of the source */
	int iz, it, itsis;	/* counter */
	int ies;		/* end of source index */
	int press;		/* to choose  between pressure or particle */
				/* velocity */
	float t;		/* time */
	int td=1, zd=1;		/* time and depth decimation */
	segy snapsh, sismo;	/* recording of the seismic field, */
				/*  seismogram */
	char *dfile="";		/* density file name */
	char *wfile="";		/* seismogram file name */
	char *velfile="";	  /* velocity file name */
	char *sfile="";		/* velocity file name */
	float freq=0.0;		/* source center freq */
	float alpha=0.0;	/* source exp		*/
	float epst0=0.0;	/* source first amp ratio */
	int styp;		/* source type */

	FILE *seisfp=stdout;	/* pointer to seismic trace output file */
	FILE *wavefp=NULL;	/* pointer to wave field output file */
	FILE *velocityfp=stdin;	/* pointer to input velocity file */
	FILE *densityfp=NULL;	/* pointer to input density file */


	/* hook up getpar to handle the parameters */
	initargs (argc, argv);
	requestdoc(0);

	/* verbose */
	if (!getparint ("verbose",&verbose)) verbose=0;

	/* get required parameters */
	if (!getparint ("nz",&nz)) err("must specify nz ! ");
	if (verbose) warn("nz= %d", nz);

	if (!getparfloat ("tmax",&tmax)) err("must specify tmax ! ");
	if (verbose) warn("tmax= %f", tmax);

	if (!getparfloat("sz", &sz)) err ("must specify sz ! ");
	if (verbose) warn("sz= %f", sz);
	
	

	/* get optional parameters */
	if (!getparint ("nt", &nt)) nt=0; 
	if (verbose) warn("nt= %d", nt);
	if (!getparint ("styp", &styp)) styp=0;
	if (verbose) warn("styp= %d ", styp);
	if (!getparfloat ("dz", &dz)) dz=1;
	if (!getparfloat ("fz", &fz)) fz=0.0;

	/* source coordinates to samples */
	isz=NINT((sz-fz)/dz);
	if (verbose) warn( "source on knot number %d ", isz);


	if (!getparfloat("rz", &rz)) rz=0.0;
	irz = NINT ((rz-fz)/dz);
	if (verbose) warn("receiver depth : %f on knot # %d ", rz, irz);

	if (!getparfloat("freq", &freq)) freq=15.0;
	if (verbose) warn("frequency : %f  Hz",freq);

	getparstring ("velfile", &velfile);
	if (verbose) {
		if (*velfile != '\0' ) warn("Velocity file : %s ",velfile);
		else warn("Velocity file supplied via stdin");
	}

	getparstring ("sfile", &sfile);
	if (verbose) {
		if (*sfile != '\0' ) warn("Output trace file : %s ",sfile);
		else warn("Output trace via stdout");
	}

	getparstring ("dfile", &dfile);
	if (verbose) {
		if (*dfile != '\0' ) warn("Density file : %s ",dfile);
		else warn("No density file supplied ");
	}

	getparstring ("wfile", &wfile);
	if (verbose) {
		if (*wfile != '\0' ) warn("Wave file : %s ",wfile);
		else warn("No wave file requested ");
	}

	if ( NINT((float) nz/((float) zd)) + 1  >SU_NFLTS) {
		warn ("Too many depth points : impossible to output wave field. Increase zd ?");
		*wfile='\0';
	}
	

	/* get absorbing conditions */
	if (!getparint("abs",abs)) {  abs[0]=0; abs[1]=1;  }
	if (verbose) {
		if (abs[0]==1) warn("absorbing condition on top ");
		if (abs[1]==1) warn("absorbing condition on bottom ");
	}
	/* get decimation coefficients */
	if (!getparint("td",&td)) td=1 ;
	if (verbose) warn("time decimation ccoefficent: %d ",td);
	if (!getparint("zd",&zd)) zd=1 ;
	if (verbose) warn("depth decimation ccoefficent: %d ",zd);

	/* choose pressure or particle velocity */
	if (!getparint("press", &press)) press=1 ;
	if ((press != 0) && (press != 1)) err ("press must equal 0 or 1");
	if (verbose) {
		if (press==1) warn( "program will output pressure values");
		else if (press==0) warn( "program will output particle velocity values");
	}
		

	/* allocate space */
	p=alloc1float(nz);
	v=alloc1float(nz);
	rv=alloc1float(nz);
	rd=alloc1float(nz);
	rd1_5=alloc1float(nz);

	/* read velocity file */
	if (*velfile != '\0' ) {
		if ((velocityfp=fopen(velfile,"r"))=='\0') err("cannot open velfile=%s ",velfile);
	}
	if (efread (rv, sizeof(float), nz, velocityfp)!=nz) 
	   err("cannot read %d velocity values ", nz);

	/* read density file  and linearly inderpolate on corrrect location */
	if (*dfile != '\0') {
		if ((densityfp=fopen(dfile,"r"))=='\0') err("cannot open dfile=%s ",dfile);
		if (fread(rd,sizeof(float), nz, densityfp)!=nz) err("error reading dfile %s",dfile);
		fclose(densityfp);
	}
	else for (iz=0; iz<nz; iz++) rd[iz]=2500;
	for (iz=0; iz<nz-1; iz++) rd1_5[iz]=(rd[iz]+rd[iz+1])/2;
	rd1_5[nz-1]=rd[nz-1];

	/* time step computation */
	vmax=0;
	for (iz=0; iz<nz; iz++) if (rv[iz]>vmax) vmax=rv[iz];if (verbose) warn( "vmax= %f ", vmax);
	dt=dz/1.414/vmax/2; if (verbose) warn( "time step dt= %f ", dt);

	/* maximum number of iterations */
	if (nt==0) nt=1+tmax/dt;
	if (verbose) warn( "number of time steps nt= %d ", nt);
	if (NINT( (float) nt/((float)td))+1>SU_NFLTS) err("too many time steps. Increase td ?");

	/* source parameter computation */
	   alpha=2*9.8696*freq*freq;

	/* time shift to get a t0 centered source */

	if ((styp==0) || (styp == 2)) epst0=fabs(source (0, styp, dt, dz, 0, alpha) / 1e4);
	else if (styp==1) epst0=fabs(source (1/sqrt(2*alpha), styp, dt, dz, 0, alpha)) / 1e4;
	if (verbose) warn( "epst0 = %f ", epst0);

	t=tmax+dt;
	do t=t-dt; while (fabs(source(t, styp, dt, dz, 0, alpha))<epst0);
	t0=t;
	ies=2*t/dt;

	if (verbose) warn("time shift t0 = %f s", t0);

/* array initialization */
	for (iz=0; iz<nz; iz++) {  v[iz]=0; p[iz]=0;  }

	if (*wfile != '\0') {
		wavefp=fopen (wfile,"w");
		snapsh.d1=dz*zd; snapsh.f1=fz ; snapsh.ns=nz/zd+1; snapsh.d2=dt*td; snapsh.f2=0; 
		/* snapsh.f2=0 is useless since 0 is the "no value" code for SU headers */
	}
	/* propagation computation */
	itsis=0;
	for (it=0; it<=nt; it++) {
		t=it*dt;
		if (abs[0]==1) p[0]=(p[0]*(1-rv[0]*dt/dz)+2*rd[0]*rv[0]*rv[0]*dt/dz*v[0])/(1+rv[0]*dt/dz);
		else p[0]=0;
		for (iz=1; iz<nz; iz++) p[iz]=p[iz]+rd[iz]*rv[iz]*rv[iz]*dt/dz*(v[iz]-v[iz-1]);
		if (abs[1]!=1) p[nz-1]=0;
		if (it<ies) {
		p[isz]=p[isz]+source(t, styp, dt, dz, t0, alpha);
		}

		for (iz=0; iz<nz-1; iz++) v[iz]=v[iz]+dt/rd1_5[iz]/dz*(p[iz+1]-p[iz]);
		
		if (abs[1] != 1) v[nz-1]=0;
		else
		v[nz-1]=((rd1_5[nz-1]*dz-dt*rd[nz-1]*rv[nz-1])*v[nz-1]-2*dt*p[nz-1])/(rd1_5[nz-1]*dz+dt*rd[nz-1]*rv[nz-1]);

	  if (it % td == 0) {
		   if (press==1) 
			sismo.data[itsis]=p[irz];
		   else
			sismo.data[itsis]=v[irz];
		   itsis++;
		}

		if ((*wfile!='\0') && (it % td == 0)) {
		if (press==1) 
			for (iz=0; iz<nz/zd; ++iz) snapsh.data[iz]=p[iz*zd];
		else
			for (iz=0; iz<nz/zd; ++iz) snapsh.data[iz]=v[iz*zd];

	   	fputtr(wavefp, &snapsh);
		}

	}

	if (*wfile!='\0') fclose (wavefp);

	sismo.dt=td*dt*1e6;
	sismo.ns=itsis;
	sismo.delrt=-t0*1000;
	sismo.trid=TREAL;
	sismo.tracl=1;

	if (*sfile != '\0') seisfp=efopen(sfile,"w");
	fputtr (seisfp, &sismo);
	

return(CWP_Exit());
}

float source (float t, int styp, float dt, float dz, float t0, float alpha)
{
	float x=0.0, sou=0.0;
	x=-alpha*(t-t0)*(t-t0);
	if (x>-40) {
	 if (styp==0) sou=exp(x);
	 	else if (styp==1) sou=-2*alpha*(t-t0)*exp(x);
	 	else if (styp==2) sou=2*alpha*(1+2*x)*exp(x);
		}
	else sou=0;
	return sou/dz*dt*1e8;
}
