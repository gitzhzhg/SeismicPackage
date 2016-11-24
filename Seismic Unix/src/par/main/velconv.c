/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* VELCONV: $Revision: 1.14 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" VELCONV - VELocity CONVersion					",
" 								",
" velconv <infile >outfile intype= outtype= [optional parameters]",
" 								",
" Required Parameters:						",
" intype=                input data type (see valid types below)",
" outtype=               output data type (see valid types below)",
" 								",
" Valid types for input and output data are:			",
" vintt          interval velocity as a function of time	",
" vrmst          RMS velocity as a function of time		",
" vintz          velocity as a function of depth		",
" zt             depth as a function of time			",
" tz             time as a function of depth			",
" 								",
" Optional Parameters:						",
" nt=all                 number of time samples			",
" dt=1.0                 time sampling interval			",
" ft=0.0                 first time				",
" nz=all                 number of depth samples		",
" dz=1.0                 depth sampling interval		",
" fz=0.0                 first depth				",
" nx=all                 number of traces			",
" 								",
" Example:  \"intype=vintz outtype=vrmst\" converts an interval velocity",
"           function of depth to an RMS velocity function of time.",
" 								",
" Notes:  nt, dt, and ft are used only for input and output functions",
"         of time; you need specify these only for vintt, vrmst, orzt.",
"         Likewise, nz, dz, and fz are used only for input and output",
"         functions of depth.					",
" 								",
" The input and output data formats are C-style binary floats.	",
NULL};

/*
 *  AUTHOR:  Dave Hale, Colorado School of Mines, 07/07/89
 */
/**************** end self doc ********************************/

void in_vintt(int nt, float dt, float ft, int nz, float dz, float fz,
	float vintt[], float zt[], float tz[]);
void in_vrmst(int nt, float dt, float ft, int nz, float dz, float fz,
	float vrmst[], float zt[], float tz[]);
void in_zt(int nt, float dt, float ft, int nz, float dz, float fz,
	float ztin[], float zt[], float tz[]);
void in_vintz(int nt, float dt, float ft, int nz, float dz, float fz,
	float vintz[], float zt[], float tz[]);
void in_tz(int nt, float dt, float ft, int nz, float dz, float fz,
	float tzin[], float zt[], float tz[]);
void out_vintt(int nt, float dt, float zt[], float vintt[]);
void out_vrmst(int nt, float dt, float ft, float zt[], float vrmst[]);
void out_zt(int nt, float zt[], float ztout[]);
void out_vintz(int nz, float dz, float tz[], float vintz[]);
void out_tz(int nz, float tz[], float tzout[]);
void zttz(int nt, float dt, float ft, float zt[], float vft, float vlt, 
	int nz, float dz, float fz, float tz[]);
void tzzt(int nz, float dz, float fz, float tz[], float vfz, float vlz, 
	int nt, float dt, float ft, float zt[]);

int
main (int argc, char **argv)
{
	int nt,nz,nin=0,nout=0,nx,ix;
	float dt,ft,dz,fz,*din,*dout,*zt,*tz;
	char *intype="",*outtype="";
	FILE *infp=stdin,*outfp=stdout;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(2);

	/* get required parameters */
	if (!getparstring("intype",&intype)) err("Must specify intype!\n");
	if (!getparstring("outtype",&outtype)) err("Must specify outtype!\n");

	/* get optional parameters */
	if (!getparint("nt",&nt)) {
		if (getparint("nz",&nz)) {
			nt = nz;
		} else {
			if (efseek(infp,(off_t)0L,2)==-1)
				err("input file size unknown; specify nt or nz\n");
			nt = (int) (eftell(infp)/sizeof(float));
		}
	}
	if (!getparfloat("dt",&dt)) dt = 1.0;
	if (!getparfloat("ft",&ft)) ft = 0.0;
	if (!getparint("nz",&nz)) {
		if (getparint("nt",&nt)) {
			nz = nt;
		} else {
			if (efseek(infp,(off_t)0L,2)==-1)
				err("input file size unknown; specify nt or nz\n");
			nz = (int) (eftell(infp)/sizeof(float));
		}
	}
	if (!getparfloat("dz",&dz)) dz = 1.0;
	if (!getparfloat("fz",&fz)) fz = 0.0;

	/* determine number of samples per input and output trace */
	if (
		STREQ(intype,"vintt") || 
		STREQ(intype,"vrmst") || 
		STREQ(intype,"zt") ) {
		nin = nt;
	} else if (
		STREQ(intype,"vintz") || 
		STREQ(intype,"tz") ) {
		nin = nz;
	} else {
		err("invalid intype=%s!\n",intype);
	}
	if (
		STREQ(outtype,"vintt") || 
		STREQ(outtype,"vrmst") || 
		STREQ(outtype,"zt") ) {
		nout = nt;
	} else if (
		STREQ(outtype,"vintz") || 
		STREQ(outtype,"tz") ) {
		nout = nz;
	} else {
		err("invalid outtype=%s!\n",outtype);
	}

	/* determine number of traces to process */
	if (!getparint("nx",&nx)) nx = -1;

        checkpars();

	/* allocate space */
	tz = ealloc1float(nz);
	zt = ealloc1float(nt);
	din = ealloc1float(nin);
	dout = ealloc1float(nout);

	/* set input file pointer to beginning of file */
	efseek(infp,(off_t)0L,0);

	/* loop over traces */
	for (ix=0; ix<nx || nx<0; ix++) {

		/* read input data */
		if (efread(din,sizeof(float),nin,infp)!=nin) break;

		/* convert input data to zt and tz */
		if (STREQ(intype,"vintt"))
			in_vintt(nt,dt,ft,nz,dz,fz,din,zt,tz);
		else if (STREQ(intype,"vrmst"))
			in_vrmst(nt,dt,ft,nz,dz,fz,din,zt,tz);
		else if (STREQ(intype,"zt"))
			in_zt(nt,dt,ft,nz,dz,fz,din,zt,tz);
		else if (STREQ(intype,"vintz"))
			in_vintz(nt,dt,ft,nz,dz,fz,din,zt,tz);
		else if (STREQ(intype,"tz"))
			in_tz(nt,dt,ft,nz,dz,fz,din,zt,tz);
		else
			err("invalid intype=%s!\n",intype);

		/* convert zt and tz to output data */
		if (STREQ(outtype,"vintt"))
			out_vintt(nt,dt,zt,dout);
		else if (STREQ(outtype,"vrmst"))
			out_vrmst(nt,dt,ft,zt,dout);
		else if (STREQ(outtype,"zt"))
			out_zt(nt,zt,dout);
		else if (STREQ(outtype,"vintz"))
			out_vintz(nz,dz,tz,dout);
		else if (STREQ(outtype,"tz"))
			out_tz(nz,tz,dout);
		else
			err("invalid outtype=%s!\n",outtype);

		/* write output data */
		efwrite(dout,sizeof(float),nout,outfp);
	}
	return EXIT_SUCCESS;
}

/* compute z(t) and t(z) from input vint(t) */
void in_vintt(int nt, float dt, float ft, int nz, float dz, float fz,
	float vintt[], float zt[], float tz[])
{
	int it;
	float vft,vlt;

	if ( zt[0] < 0.0 ) 
		err("Negative value detected! Only positive velocities allowed");
	zt[0] = 0.5*ft*vintt[0];
	for (it=1; it<nt; it++){
		if ( zt[it] < 0.0 ) 
		err("Negative value detected! Only positive velocities allowed");
		zt[it] = zt[it-1]+0.5*dt*vintt[it-1];
	}
	vft = vintt[0];
	vlt = vintt[nt-1];
	zttz(nt,dt,ft,zt,vft,vlt,nz,dz,fz,tz);
}

/* compute z(t) and t(z) from input vrms(t) */
void in_vrmst(int nt, float dt, float ft, int nz, float dz, float fz,
	float vrmst[], float zt[], float tz[])
{
	int it;
	float t,vft,vlt,vtinys,vintts,vintt;

	if ( vrmst[0] < 0.0 ) 
		err("Negative value detected! Only positive velocities allowed");
	vintt = vrmst[0];
	vtinys = 0.00001*vintt*vintt;
	zt[0] = 0.5*ft*vintt;
	for (it=1,t=ft+dt; it<nt; it++,t+=dt) {
		if ( vrmst[it] < 0.0 ) 
		err("Negative value detected! Only positive velocities allowed");
		vintts = (t*vrmst[it]*vrmst[it] -
			(t-dt)*vrmst[it-1]*vrmst[it-1])/dt;
		vintt = sqrt(MAX(vintts,vtinys));
		zt[it] = zt[it-1]+0.5*dt*vintt;
	}
	vft = vrmst[0];
	vlt = vintt;
	zttz(nt,dt,ft,zt,vft,vlt,nz,dz,fz,tz);
}

/* compute z(t) and t(z) from input z(t) */
void in_zt(int nt, float dt, float ft, int nz, float dz, float fz,
	float ztin[], float zt[], float tz[])
{
	int it;
	float vft,vlt;

	for (it=0; it<nt; it++)
		zt[it] = ztin[it];
	vft = 2.0*(zt[1]-zt[0])/dt;
	vlt = 2.0*(zt[nt-1]-zt[nt-2])/dt;
	zttz(nt,dt,ft,zt,vft,vlt,nz,dz,fz,tz);
}

/* compute z(t) and t(z) from input vint(z) */
void in_vintz(int nt, float dt, float ft, int nz, float dz, float fz,
	float vintz[], float zt[], float tz[])
{
	int iz;
	float vfz,vlz;

	if ( vintz[0] < 0.0 ) 
		err("Negative value detected! Only positive velocities allowed");
	tz[0] = 2.0*fz/vintz[0];
	for (iz=1; iz<nz; iz++) {
		if ( vintz[iz] < 0.0 ) 
		err("Negative value detected! Only positive velocities allowed");
		tz[iz] = tz[iz-1]+2.0*dz/vintz[iz-1];
	}
	vfz = vintz[0];
	vlz = vintz[nz-1];
	tzzt(nz,dz,fz,tz,vfz,vlz,nt,dt,ft,zt);
}

/* compute z(t) and t(z) from input t(z) */
void in_tz(int nt, float dt, float ft, int nz, float dz, float fz,
	float tzin[], float zt[], float tz[])
{
	int iz;
	float vfz,vlz;

	for (iz=0; iz<nz; iz++)
		tz[iz] = tzin[iz];
	vfz = 2.0*dz/(tz[1]-tz[0]);
	vlz = 2.0*dz/(tz[nz-1]-tz[nz-2]);
	tzzt(nz,dz,fz,tz,vfz,vlz,nt,dt,ft,zt);
}

/* compute output vint(t) from z(t) and t(z) */
void out_vintt(int nt, float dt, float zt[], float vintt[])
{
	int it;

	for (it=0; it<nt-1; it++)
		vintt[it] = 2.0*(zt[it+1]-zt[it])/dt;
	vintt[nt-1] = vintt[nt-2];
}

/* compute output vrms(t) from z(t) and t(z) */
void out_vrmst(int nt, float dt, float ft, float zt[], float vrmst[])
{
	int it;
	float vintt,sum,t;

	vintt = 2.0*(zt[1]-zt[0])/dt;
	sum = ft*vintt*vintt;
	vrmst[0] = vintt;
	for (it=1,t=ft+dt; it<nt; it++,t+=dt) {
		vintt = 2.0*(zt[it]-zt[it-1])/dt;
		sum += dt*vintt*vintt;
		vrmst[it] = sqrt(sum/t);
	}
}

/* compute output z(t) from z(t) and t(z) */
void out_zt(int nt, float zt[], float ztout[])
{
	int it;

	for (it=0; it<nt; it++)
		ztout[it] = zt[it];
}

/* compute output vint(z) from z(t) and t(z) */
void out_vintz(int nz, float dz, float tz[], float vintz[])
{
	int iz;

	for (iz=0; iz<nz-1; iz++)
		vintz[iz] = 2.0*dz/(tz[iz+1]-tz[iz]);
	vintz[nz-1] = vintz[nz-2];
}

/* compute output t(z) from z(t) and t(z) */
void out_tz(int nz, float tz[], float tzout[])
{
	int iz;

	for (iz=0; iz<nz; iz++)
		tzout[iz] = tz[iz];
}

/* compute t(z) from z(t) */
void zttz(int nt, float dt, float ft, float zt[], float vft, float vlt, 
	int nz, float dz, float fz, float tz[])
{
	int iz;
	float z,lt=ft+(nt-1)*dt,lz=fz+(nz-1)*dz;

	yxtoxy(nt,dt,ft,zt,nz,dz,fz,0.0,0.0,tz);
	for (iz=0,z=fz; z<=zt[0]; iz++,z+=dz)
		tz[iz] = 2.0*z/vft;
	for (iz=nz-1,z=lz; z>=zt[nt-1]; iz--,z-=dz)
		tz[iz] = lt+2.0*(z-zt[nt-1])/vlt;
}

/* compute z(t) from t(z) */
void tzzt(int nz, float dz, float fz, float tz[], float vfz, float vlz, 
	int nt, float dt, float ft, float zt[])
{
	int it;
	float t,lt=ft+(nt-1)*dt,lz=fz+(nz-1)*dz;

	yxtoxy(nz,dz,fz,tz,nt,dt,ft,0.0,0.0,zt);
	for (it=0,t=ft; t<=tz[0]; it++,t+=dt)
		zt[it] = 0.5*t*vfz;
	for (it=nt-1,t=lt; t>=tz[nz-1]; it--,t-=dt)
		zt[it] = lz+0.5*(t-tz[nz-1])*vlz;
}
