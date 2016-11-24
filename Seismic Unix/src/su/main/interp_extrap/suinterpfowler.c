/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPLANE: $Revision: 1.6 $ ; $Date: 2011/11/16 22:12:22 $	*/

#include "su.h"
#include "segy.h" 

/*********************** self documentation **********************/
char *sdoc[] = { 
" SUINTERPFOWLER - interpolate output image from constant velocity panels",
"	   built by SUTIFOWLER or CVS					",
"									",
" These parameters should be specified the same as in SUTIFOWLER:	",
" vmin=1500.		minimum velocity				",
" vmax=2500.		maximum velocity				",
" nv=21			number of velocity panels			",
" etamin=0.10		minimum eta value				",
" etamax=0.25		maximum eta value				",
" neta=11		number of eta values				",
" ncdps=1130		number of cdp points				",
"									",
" If these parameters are specified so that nvstack>5, then the input 	",
" data are assumed to come from CVS and the SUTIFOWLER parameters are ignored.",
" nvstack=0		number of constant velocity stack panels output by CVS",
" vminstack=1450	minimum velocity specified for CVS		",
" vscale=1.0		scale factor for velocity functions		",
"									",
" These parameters specify the desired output (time,velocity,eta) model	",
" at each cdp location. The sequential cdp numbers should be specified in",
" increasing order from 0 to 'ncdps-1' at from 1 to 'ncdps' control point",
" locations. (Time values are in seconds.)				",
" cdp=0			cdp number for (t,v,eta) triplets (specify more than",
" 				once if needed)				",
" t=0.			array of times for (t,v,eta) triplets (specify more",
"				than once if needed)			",
" v=1500.		array of velocities for (t,v,eta) triplets (specify",
"				more than once if needed)		",
" eta=0.		array of etas for (t,v,eta) triplets (specify more",
"				than once if needed)			",
"									",
" Note: This is a simple research code based on linear interpolation.	",
" There are no protections against aliasing built into the code beyond	",
" suggesting that this program have a knowledgable user. A final version",
" should do a better job taking care of endpoint conditions.",
"									",
NULL};

/*
 * Author: (Visitor from Mobil) John E. Anderson, Spring 1994 
 */
/**************** end self doc ********************************/

segy tr;	/* Input and output trace data of length nt */
int main (int argc, char **argv)
{
	int nt;
	int icmp;
	int j;
	int neta;
	int nv;
	int ncdps;
	int nc;
	int ic;
	int ic1=0;
	int ic2=0;
	int it;
	int iv;
	int ie;
	int nvel;
	int nvstack;


	float *c;
	float *v;
	float *t;
	float *e;

	float *time;
	float *eta;
	float *vel;
	float *p;

	float vminstack;
	float dt;
	float vmin;
	float vmax;
	float etamin;
	float etamax;
	float dv;
	float deta;
	float wc=0.0;
	float wv;
	float we;
	float ee;
	float vv;
	float vscale;

	initargs(argc,argv);
	requestdoc(1);

	if (!getparint("nv",&nv)) nv = 21;
	if (!getparint("neta",&neta)) neta=11;
	if (!getparint("ncdps",&ncdps)) ncdps = 1130;
	if (!getparfloat("vmin",&vmin)) vmin = 1500.;
	if (!getparfloat("vmax",&vmax)) vmax = 2500.;
	if (!getparfloat("etamin",&etamin)) etamin = 0.10;
	if (!getparfloat("etamax",&etamax)) etamax = 0.25;
	if (!getparint("nvstack",&nvstack)) nvstack = 0;
	if (!getparfloat("vminstack",&vminstack)) vminstack = 0.25;
	if (!getparfloat("vscale",&vscale)) vscale = 1.0;

        checkpars();
	dv=(vmax-vmin)/MAX((nv-1),1);
	deta = (etamax-etamin)/MAX((neta-1),1);
	if(deta==0) {
		deta=1;
		neta=1;
	}
	if(nvstack>5) {
		etamin=0;
		etamax=0.;
		deta=1.;
		vmin=0.;
		nv=nvstack;
		neta=1;
		dv=1./(vminstack*vminstack*(nvstack-5));
	}

	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;
	dt = 0.000001*tr.dt;
	fprintf(stderr,"supaint: sample rate = %f s, number of samples = %d\n",
		dt,nt);
	p = ealloc1float(nv*neta*nt);

	nc = countparname("cdp");
	if(countparname("v")!=nc) err("v array must be specified for each cdp");
	if(countparname("t")!=nc) err("t array must be specified for each cdp");
	if(nvstack<6){
		if(countparname("eta")!=nc) err("eta array must be specified for each cdp");
	}
	vel=ealloc1float(nc*nt);
	eta=ealloc1float(nc*nt);
	time=ealloc1float(nt);
	c=ealloc1float(nc);
	for(it=0;it<nt;it++) time[it]=it*dt;
	if(nvstack>5) {
	    for(ic=0;ic<nc;ic++) {
		getnparfloat(ic+1,"cdp",&c[ic]);
		nvel=countnparval(ic+1,"v");
		fprintf(stderr,"cdp=%f, number of (time,velocity) values = %d\n",c[ic],nvel);
		if(countnparval(ic+1,"t")!=nvel)
			err("For each cdp control point, t and v must have the same number of values");
		v=ealloc1float(nvel);
		t=ealloc1float(nvel);
		getnparfloat(ic+1,"t",t);
		getnparfloat(ic+1,"v",v);
		for(j=0;j<nvel;j++) v[j]*=vscale;
		for(j=0;j<nvel;j++) fprintf(stderr,"	time=%f velocity=%f\n",t[j],v[j]);
		intlin(nvel,t,v,v[0],v[nvel-1],nt,time,&vel[ic*nt]);
		free(v);
		free(t);	
	    }
	    for(j=0;j<nt*nc;j++) {
		vel[j]=1./(vel[j]*vel[j]);
		eta[j]=0.;
	    }
	}else{
	    for(ic=0;ic<nc;ic++) {
		getnparfloat(ic+1,"cdp",&c[ic]);
		nvel=countnparval(ic+1,"v");
		fprintf(stderr,"cdp=%f, number of triplet values = %d\n",c[ic],nvel);
		if(countnparval(ic+1,"t")!=nvel || countnparval(ic+1,"eta")!=nvel ) 
			err("For each cdp control point, t,v,and eta must have the same number of values");
		v=ealloc1float(nvel);
		t=ealloc1float(nvel);
		e=ealloc1float(nvel);
		getnparfloat(ic+1,"t",t);
		getnparfloat(ic+1,"v",v);
		for(j=0;j<nvel;j++) v[j]*=vscale;
		getnparfloat(ic+1,"eta",e);
		for(j=0;j<nvel;j++) {
			fprintf(stderr,"	time=%f velocity=%f eta=%f\n",
				t[j],v[j],e[j]);
		}
		intlin(nvel,t,v,v[0],v[nvel-1],nt,time,&vel[ic*nt]);
		intlin(nvel,t,e,e[0],e[nvel-1],nt,time,&eta[ic*nt]);
		free(v);
		free(t);
		free(e);
	    }
	}
	free(time);
	for(icmp=0;icmp<ncdps;icmp++) {
		for(j=0;j<(nv*neta);j++) {
			if(icmp==0 && j==0 ) {
				/* data already in memory */
			}else{
				if (!gettr(&tr))  
					err("can't get input trace");
			}
			for(it=0;it<nt;it++)
				p[it+j*nt]=tr.data[it];
		}
		if(icmp<=c[0]) {
			ic1=0;
			ic2=0;
			wc=0.;
		}else if(icmp>=c[nc-1]) {
			ic1=nc-1;
			ic2=nc-1;
			wc=0.;
		}else{
			for(ic=1;ic<nc;ic++) {
				ic1=ic-1;
				ic2=ic;
				wc=(c[ic2]-icmp)/(c[ic2]-c[ic1]);
				if(icmp>c[ic1] && icmp<=c[ic2]) break;
			}
		}

		for(it=0;it<nt;it++) {
			ee=(wc*eta[it+ic1*nt]+(1.-wc)*eta[it+ic2*nt]-etamin)/deta;
			vv=(wc*vel[it+ic1*nt]+(1.-wc)*vel[it+ic2*nt]-vmin)/dv;
			ie=ee;
			we=ee-ie;
			iv=vv;
			wv=vv-iv;
			tr.data[it]=0.;
			if(neta==1) {
				if(iv>=0 && iv<nv-1) {
					tr.data[it]=(1-wv)*p[it+iv*nt] + 
							wv*p[it+(iv+1)*nt];	
				}
			}else if(iv>=0 && iv<nv-1 && ie>=0 && ie<neta-1) {
				tr.data[it]=(1-we)*(1-wv)*p[it+(ie*nv+iv)*nt]+
					(1-we)*wv*p[it+(ie*nv+iv+1)*nt]+
					we*(1-wv)*p[it+((ie+1)*nv+iv)*nt]+
					we*wv*p[it+((ie+1)*nv+iv+1)*nt];
				}
		}
		tr.f2=0.;
		tr.d2=1.;
		tr.f1=0.;
		tr.d1=dt;
		tr.offset=0;
		tr.cdp=icmp;
		tr.igc=0;
		tr.igi=0;
		tr.sx=0.5*(tr.sx+tr.gx);
		tr.gx=tr.sx;
		tr.cdpt=0;
		puttr(&tr);
		if(icmp==20*(icmp/20)) fprintf(stderr,"supaint: have output cmp %d\n",icmp);
	}
	free(p);
	free(c);
	free(eta);
	free(vel);
	return EXIT_SUCCESS;
}

