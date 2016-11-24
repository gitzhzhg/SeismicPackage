/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* su2voxet */



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation *****************************/
char *sdoc[] = {"                                                       ",
"                                                                       ",
"    SU2VOXET  - convert 3D data volume to Gocad VOXET object           ",
"                                                                       ",
"    su2voxet < infile fname [optional parameters]                      ",
"                                                                       ",
"    Optional parameters                                                ",
"                                                                       ",
"    u=cdp             key corresponding to x coordinate                ",
"    v=ep              key corresponding to y coordinate                ",
"    time=0             1                                               ",
"    bendian=0	       1 if the software is running on big endian machine",
"    datum=0		if not specified trace header word delrt is used",
"                                                                       ",
"    dz=-tr.dt/1000.0	or specified                                    ",
"    maxu=1000                                                          ",
"    maxv=1000                                                          ",
"                                                                       ",
"   The data has to be sorted according to header u and within u v !    ",
"   In seismic unix :  susort < data u v > data_sorted                  ",
NULL};

/**************************************************************************
Author:  B.Nemeth, Potash Corporation of Saskatoon, Saskatchewan CA  
	From the Potash_SU collection
**************************************************************************/

/**************** end self doc ********************************/
   
/* Segy data constans */
segy tr;				/* SEGY trace */

int main( int argc, char *argv[] )
{
	unsigned int maxu;	/* Maximum number of u axis values */
	unsigned int maxv;	/* Maximum number of v axis values */
	unsigned int nu;	/* number of u values */
	unsigned int nv;	/* number of v values */
	int *ua=NULL;		/* array of u values */
	int *va=NULL;		/* array of v values */
	float **xa=NULL;	/* array of x values */
	float **ya=NULL;	/* array of y values */
	int utmp;		/* array of u values */
	int vtmp;		/* array of v values */
	cwp_String u;           /* header key word from segy.h          */
	cwp_String v;           /* header key word from segy.h          */
	cwp_String utype;       /* .its type          */
	cwp_String vtype;       /* ..its type         */
	Value uval;      	 /* .its value        */
	Value vval;       	/* ..its value        */
	int uindx;
	int vindx;
	int time;		/* time data flag */
	cwp_String fname;       /* name of output files                 */
	char fdn[BUFSIZ];       /* name of output files                 */
	char fhn[BUFSIZ];       /* name of output files                 */
	
	int firstu=0;
	int firstv=0;
	
	FILE *hfp=NULL;
	FILE *dfp=NULL;
	
	double dudx;
	double dudy;
	double dvdx;
	double dvdy;
	double dz;
	float dzf;
	double x0;
	double y0;
	double z0;
	float cscale;
	float datum;
	
	unsigned int ntr=0;		/* trace counter */
	int flag;
	int bendian;
	int verbose;
	
	initargs(argc, argv);
   	requestdoc(1);
	
	if (!getparint("maxu", &maxu)) maxu = 1000;
	if (!getparint("maxv", &maxv)) maxv = 1000;
	if (!getparint("time", &time)) time = 0;
	if (!getparint("verbose", &verbose)) verbose = 0;
	if (!getparint("bendian", &bendian)) bendian = 0;
        if (!getparstring("u", &u)) u = "cdp";
        if (!getparstring("v", &v)) v = "ep";
	MUSTGETPARSTRING("fname",&fname);
	
	
	firstu=maxu;
	firstv=maxv;
	
	utype = hdtype(u);
	uindx = getindex(u);
	vtype = hdtype(v);
	vindx = getindex(v);
	
	/* allocate storage */
	ua = ealloc1int(maxu);
	va = ealloc1int(maxv);
	xa = ealloc2float(maxv,maxu);
	ya = ealloc2float(maxv,maxu);
	
       	nu=0;
	nv=0;
	
	/* open files */
	strncpy(fdn,fname,MIN(strlen(fname),BUFSIZ));
	strncpy(fhn,fname,MIN(strlen(fname),BUFSIZ));
	strcat(fdn,"_amplitude@@");
	strcat(fhn,".vo");
	dfp = efopen(fdn,"w");
	hfp = efopen(fhn,"w");
	
        /* get information from the first header */
        if (!gettr(&tr)) err("can't get first trace");
	
	if (!getparfloat("datum", &datum)) datum = -tr.delrt;

	do {
		gethval(&tr, uindx, &uval);
		gethval(&tr, vindx, &vval);
		utmp=vtoi(utype,uval);
		vtmp=vtoi(vtype,vval);
		cscale=(float)pow((double)10.0,(double)tr.scalco);
		
		
		/* Select the smallest index possible */
		if(firstu>utmp) firstu=utmp;
		if(firstv>vtmp) firstv=vtmp;
		

		flag=1;
		{ register int i;
			for(i=0;i<nu;i++)
				if(ua[i]==utmp) {
					flag=0;
				}
		}
		if(flag) {
			ua[nu]=utmp;
			nu++;
			if(verbose)
				fprintf(stderr," %d\n",nu);
		}
		
		flag=1;
		{ register int i;
			for(i=0;i<nv;i++)
				if(va[i]==vtmp) {
					flag=0;
				}
		}
		if(flag) {
			va[nv]=vtmp;
			nv++;
			if(verbose)
				fprintf(stderr," %d\n",nv);
		}
		
		xa[utmp-1][vtmp-1]=(float)tr.gx*cscale;
		ya[utmp-1][vtmp-1]=(float)tr.gy*cscale;
/*		fprintf(stderr," %d %d\n",utmp-1,vtmp-1); */
		
		if(!bendian) {
			{ register int i;
				for(i=0;i<tr.ns;i++) swap_float_4(&tr.data[i]);
			}
		}
			
		fwrite(&tr.data,sizeof(float),tr.ns,dfp);


		ntr++;
	} while(gettr(&tr));
	
	if(nu!=1) {
		/* Dimension of the cell in u direction in the XYZ */
		dudx=(double)(-xa[firstu-1][firstv-1]+xa[firstu+nu-2][firstv-1])/(nu-1);
		dudy=(double)(-ya[firstu-1][firstv-1]+ya[firstu+nu-2][firstv-1])/(nu-1);
	}
	
	
	if(nv!=1) {
		/* Dimension of the cell in v direction in the XYZ */
		dvdx=(double)(-xa[firstu-1][firstv-1]+xa[firstu-1][firstv+nv-2])/(nv-1);
		dvdy=(double)(-ya[firstu-1][firstv-1]+ya[firstu-1][firstv+nv-2])/(nv-1);
	}
	
	
	if(nu==1 && nv!=1) {
		/* du is normal to dv and the same size */
		dudy=-dvdx;
		dudx=dvdy;
	}
	
	if(nv==1 && nu!=1) {
		/* dv is normal to dv and the same size */
		dvdy=dudx;
		dvdx=-dudy;
	}
	
	
	
	x0=xa[firstu-1][firstv-1];
	y0=ya[firstu-1][firstv-1];
	z0=datum;
	if (!getparfloat("dz", &dzf)) dzf=-((double)tr.dt/1000.0);
        checkpars();
	dz=(double)dzf;
	
	
	/* Gocad Header */
	fprintf(hfp,"GOCAD Voxet 1\nHEADER {\n*name:%s\n}\n",fname);
	
	/*AXIS BLOCK */
	/* Voxet coordinate system in xyz space */
	fprintf(hfp,"AXIS_O %15f %15f %15f\n",x0,y0,z0);
	fprintf(hfp,"AXIS_U %15f %15f %15f\n",0.0,0.0,(float)dz);
	fprintf(hfp,"AXIS_V %15f %15f %15f\n",dvdx,dvdy,0.0);
	fprintf(hfp,"AXIS_W %15f %15f %15f\n",dudx,dudy,0.0);
	/* Voxet geometry in UVW space */
	fprintf(hfp,"AXIS_MIN %13f %15f %15f\n",0.0,0.0,0.0);
	fprintf(hfp,"AXIS_MAX %13f %15f %15f\n",(float)tr.ns-1,(float)nv-1,(float)nu-1);
	fprintf(hfp,"AXIS_N %15d %15d %15d\n",tr.ns,nv,nu);
	fprintf(hfp,"AXIS_NAME  \" depth\" \"inline\" \"crossline\"\n");
	fprintf(hfp,"AXIS_UNIT       \"m\"      \"m\"         \"m\"\n");
	fprintf(hfp,"AXIS_TYPE        even       even          even\n\n\n");
	
	/* PROPERTY BLOCK */
	fprintf(hfp,"PROPERTY 1 \"amplitude\"\n");
	fprintf(hfp,"PROPERTY_CLASS 1 \"amplitude\"\n");
	fprintf(hfp,"PROPERTY_CLASS_HEADER 1 \"amplitude\" {\n");
 	fprintf(hfp,"*colormap:gray\n");
 	fprintf(hfp,"*colormap*reverse:true\n");
 	fprintf(hfp,"}\n");
	fprintf(hfp,"PROP_UNIT 1 none\n");
	fprintf(hfp,"PROP_ESIZE 1 4 \n");
	fprintf(hfp,"PROP_ETYPE 1 IEEE \n");
	fprintf(hfp,"PROP_FILE 1 %s \n",fdn);
	fprintf(hfp,"END\n");
	

	
	/* Close the files */
	efclose(dfp);
	efclose(hfp);	


	/* Free arrays */
	free1int(ua);
	free1int(va);  
	free2float(xa);
	free2float(ya);  
	
   	return EXIT_SUCCESS;
}
