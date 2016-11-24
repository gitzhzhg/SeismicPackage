/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSYNCZ: $Revision: 1.16 $ ; $Date: 2011/11/12 00:40:42 $	*/

#include "su.h"
#include "segy.h" 

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUSYNCZ - SYNthetic seismograms for piecewise constant V(Z) function	",
"	   True amplitude (primaries only) modeling for 2.5D		",
" 									",
"  susyncz > outfile [parameters]					",
" 									",
" Required parameters:							",
" none									",
" 									",
" Optional Parameters:							",
" ninf=4        number of interfaces (not including upper surface)	",
" dip=5*i       dips of interfaces in degrees (i=1,2,3,4)		",
" zint=100*i    z-intercepts of interfaces at x=0 (i=1,2,3,4)		",
" v=1500+ 500*i velocities below surface & interfaces (i=0,1,2,3,4)	",
" rho=1,1,1,1,1 densities below surface & interfaces (i=0,1,2,3,4)	",
" nline=1	number of (identical) lines				",
" ntr=32        number of traces					",
" dx=10         trace interval						",
" tdelay=0      delay in recording time after source initiation		",
" dt=0.004      time interval						",
" nt=128        number of time samples					",
" 			 						",
" Notes:								",
" The original purpose of this code was to create some nontrivial	",
" data for Brian Sumner's CZ suite.					",
" 			 						",
" The program produces zero-offset data over dipping reflectors.	",
" 			 						",
" In the original fortran code, some arrays had the index		",
" interval 1:ninf, as a natural way to index over the subsurface	",
" reflectors.  This indexing was preserved in this C translation.	",
" Consequently, some arrays in the code do not use the 0 \"slot\".	",
" 			 						",
" Example:								",
"	susyncz | sufilter | sugain tpow=1 | display_program		",
" 			 						",
" Trace header fields set: tracl, ns, dt, delrt, ntr, sx, gx		",
NULL};

/*
 * Credits:
 * 	CWP: Brian Sumner, 1983, 1985, Fortran design and code 
 *      CWP: Stockwell & Cohen, 1995, translation to C 
 *
 */

/**************** end self doc *******************************************/

#define TORADS PI/180.0
#define FOURPI 4.0*PI

/* prototype */
void tabtrcoefs(int ninf, float *rho, float *v, float **theta,
					float *dip, float *trcoefs);
segy tr;

int
main(int argc, char **argv)
{
	int nline;		/* number of (identical) lines		*/
	int ntr;		/* number of traces			*/
	int nt;			/* number of time samples		*/
	int ninf;		/* number of subsurface reflectors	*/
	int itr;		/* trace number index			*/
	int iline;		/* line number index			*/
	int i, j;		/* counters				*/

	float dx;		/* trace interval			*/
	float dt;		/* time interval			*/
	float tdelay;		/* recording time delay			*/
	float x;		/* zero offset position			*/
	float xmin, xmax;	/* min and max horizontal coordinates 	*/

	float *zint;		/* reflector intercepts at x = 0 	*/
	float *dip;		/* reflector dips 			*/
	float *v;		/* speeds in layers 			*/
	float *rho;		/* densities in layers 			*/
	float *xl, *xr;		/* min and max horiz coords in a layer 	*/
	float *meet;		/* horizontal intercept for two adjacent
				 * layers, if layers not parallel */
	float *trcoefs;		/* transmission coefficients		*/
	float *data;		/* temp for holding synthetic trace	*/
	float *tout;		/* times[nt] for interpolation		*/
	float **theta;		/* angle at which the specular ray to
				 * interface j hits interface i */
	float **m;		/* slope for horiz travel calculation	*/
	float **b;		/* intercept for horiz travel calc	*/
	float **d;		/* distance traveled in a layer, if layers
				 * are parallel */
	float **k;		/* factor for finding horizontal distance
				 * in a layer */
	float **w;		/* for spreading factor calculation	*/

	cwp_Bool *dorow;	/* are adjacent reflectors parallel?	*/


	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(0);
	
	
	/* get scalar parameters */
	if (!getparint("nline",&nline))	     nline=1;
	if (!getparint("ntr",&ntr))	     ntr=32;	 tr.ntr = ntr*nline;
	if (!getparfloat("dx",&dx))	     dx=10.0;
	if (!getparint("nt",&nt))	     nt=128;
	CHECK_NT("nt",nt);					 tr.ns = nt;
	if (!getparfloat("dt",&dt))	     dt=0.004;	 tr.dt = dt*1000000;
	if (!getparfloat("tdelay",&tdelay))  tdelay=0.0;
						tr.delrt = tdelay*1000;
	if (!getparint("ninf",&ninf))	ninf=4;
	
	/* allocate space */
	data = ealloc1float(nt);
	v = ealloc1float(ninf+1);
	rho = ealloc1float(ninf+1);
	zint = ealloc1float(ninf+1);
	dip = ealloc1float(ninf+1);
	meet = ealloc1float(ninf+1);
	xl = ealloc1float(ninf+1);
	xr = ealloc1float(ninf+1);
	trcoefs = ealloc1float(ninf+1);
	theta = ealloc2float(ninf+1,ninf+1);
	m = ealloc2float(ninf+1,ninf+1);
	b = ealloc2float(ninf+1,ninf+1);
	d = ealloc2float(ninf+1,ninf+1);
	w = ealloc2float(ninf+1,ninf+1);
	k = ealloc2float(ninf+1,ninf+1);
	dorow = ealloc1(ninf+1,sizeof(cwp_Bool));
	
	/* compute output times for interpolation */
	tout = ealloc1float(nt);
	for (i=0; i<nt; i++) tout[i]=i*dt;

	 	
	/* getpar the reflector intercepts, dips, v's, and rho's */
	zint[0] = 0.0; /* surface hard wired */
	if (!getparfloat("zint", zint+1))
 		for (i = 1; i <= ninf; ++i) zint[i] = 100.0 * i;
	dip[0] = 0.0; /* surface hard wired */
	if (!getparfloat("dip", dip+1))
 		for (i = 1; i <= ninf; ++i) dip[i] = 5.0 * i;
	if (!getparfloat("v", v))
 		for (i = 0; i <= ninf; ++i) v[i] = 1500.+ 500*i;
	if (!getparfloat("rho", rho))
 		for (i = 0; i <= ninf; ++i) rho[i] = 1.0;

	/* check dips and intercepts */
	for (i = 1; i <= ninf; ++i) {
		if (dip[i] < -90.0 || dip[i] > 90.0) err("dips1");
		if (ABS(dip[i] - dip[i-1]) > 90.0) err("dips2");
		dip[i] *= TORADS;
		
		if (zint[i] <= zint[i-1]) err("intercept");
	}
	for (i = 0; i < ninf; ++i) {
		if (v[i] <= 0.0) err("velocities");
		if (rho[i] <= 0.0) err("density");
	}


	/* Table theta[j][i], the angle at which the specular to layer j
	 * leaves interface i. 						*/
	for (j = 1; j < ninf; ++j) {
		theta[j][j-1] = -dip[j];
		for (i = j-1; i > 0; --i) {
		    float temp;
		    
		    temp = v[i-1]*sin(theta[j][i]+dip[i])/v[i];
		    if (ABS(temp) > 1.0) err("specular %d", j);
			
		    theta[j][i-1] = asin(temp) - dip[i];
		}
	}


	/* Table m and b, which are used to find the x coordinate of
	 * the specular at the next interface from its x coordinate at
	 * the previous interface. 					*/
	for (j = 1; j <= ninf; ++j) {
		for (i = 0; i < j; ++i) {
			float s, temp;
			
			s = sin(theta[j][i]);
			temp = 1.0 - tan(dip[i+1])*s;
			m[j][i] = (1.0 - tan(dip[i])*s)/temp;
			b[j][i] = (zint[i+1]-zint[i])*s/temp;
		}
	}

	/* Make the final checks on the substructure specification.
	 * The strategy is to check the x coordinates of the
	 * interface intercepts against x coordinates of
	 * specular-interface intercepts. Only the rays from the
	 * left and right endpoints need to be checked, since they define
	 * the area being "observed". 					*/
	xmin = 0.0;
	xmax = dx * (ntr - 1);
	for (j = 1; j <= ninf; ++j) {
		xl[j] = xmin;  /*** changed Brian's code here! */
		xr[j] = xmax;
	}

	for (j=0; j<ninf; ++j) {
		int j2;
		for (j2=j+1; j2<=ninf; ++j2) {
		    if (dip[j2] != dip[j]) {
			float intercept;
			
			intercept = (zint[j2]-zint[j])/
						(tan(dip[j])-tan(dip[j2]));
			if (intercept > xmin && intercept < xmax) {
				err("intercept");
			}
		    }
		}

		for (j2=j+1; j<=ninf; ++j) {
			xl[j2] = m[j2][j]*xl[j2] + b[j2][j];
			xr[j2] = m[j2][j]*xr[j2] + b[j2][j];

			if (xl[j2] < xmin) xmin = xl[j2];
			if (xr[j2] > xmax) xmax = xr[j2];
		}
	}

	/* Table the arrays meet and k if two adjacent interfaces
	 * intersect, otherwise d if they are parallel.
	 * Meet is the x coordinate of intersection if there is
	 * an intersection, k is a value which will be used to
	 * find the distance travelled in that layer. If there is no
	 * intersection d will be a constant for all j and can be
	 * stored now. */
	for (i = 0; i < ninf; ++i) {
		if (dip[i+1] == dip[i]) {
			dorow[i] = cwp_false;
			for (j = i + 1; j <= ninf; ++j) {
				d[j][i] = (zint[i+1]-zint[i])*cos(dip[i]) /
						cos(theta[j][i] + dip[i]);
			}
		} else {
			dorow[i] = cwp_true;
			meet[i] = (zint[i+1]-zint[i])/
					(tan(dip[i])-tan(dip[i+1]));
			for (j = i + 1; j <= ninf; ++j) {
				k[j][i] = sin(dip[i]-dip[i+1]) /
				      (cos(theta[j][i]+dip[i+1])*cos(dip[i]));
			}
		}
	}

	/* Table t, the transmission coefficients. */
	tabtrcoefs(ninf, rho, v, theta, dip, trcoefs);

	/* Table w, the coefficients for the spreading factor calculation. */
	for (j = 1; j <= ninf; ++j) {
		w[j][0] = 1.0;
		for (i = 1; i < j; ++i) {
			float c1, c2;
			
			c1 = cos(theta[j][i-1] + dip[i]);
			c2 = cos(theta[j][i] + dip[i]);
			w[j][i] = w[j][i-1]*c1*c1/(c2*c2);
		}
	}

	/* Now ready to make synthetic traces. */
	for (iline = 0; iline < nline; ++iline) {
		x = 0.0;
		for (itr = 0; itr < ntr; itr++) {
			float ex, t0, time, tmp, p1, p2, dist, amp[1];
			int itime, it;
			
			memset( (void *) tr.data, 0, nt * FSIZE);
			memset( (void *) data, 0, nt * FSIZE);
	
			for (j = 1; j <= ninf; ++j) {
				ex = x;
				if (dorow[0]) {
					dist = (meet[0]-ex)*k[j][0];
				} else {
					dist = d[j][0];
				}
				t0 = dist/v[0];
				tmp = dist*v[0];
				p1 = tmp;
				p2 = tmp;
				for (i = 1; i < j; ++i) {
					ex = m[j][i-1]*ex + b[j][i-1];
					if (dorow[i]) {
						dist = (meet[i] - ex)*k[j][i];
					} else {
						dist = d[j][i];
					}
	
					t0 += dist/v[i];
					tmp = dist*v[i];
					p1 += tmp;
					p2 += tmp*w[j][i];
				}
	
				/* set strength and time of spike response */
				amp[0] = v[0]*trcoefs[j]/
						(dt*FOURPI*2.0*sqrt(p1*p2));
				time = 2.0*t0 - tdelay;
				itime = NINT(time/dt);
				if (itime >= 0 && itime < nt)
						data[itime] = amp[0];
				
				/* distribute spike response over full trace */
				ints8r(1,dt,time,amp,0,0,nt,tout,data);
				
				/* add distributed spike to segy trace */
				for (it = 0; it < nt; ++it)
					tr.data[it] += data[it];
			}
			
			/* fill tracl header and put trace out */
			tr.tracl = itr + 1;
			tr.gx = x;
			tr.sx = x;
			puttr(&tr);

			/* set horizontal location of next trace */
			x += dx;
	
		}
	}
	
	/**
	* Thats all folks.
	**/

	return(CWP_Exit());	
}

void tabtrcoefs(int ninf, float *rho, float *v, float **theta,
					float *dip, float *trcoefs)
/*****************************************************************************
table transmission coefficients
******************************************************************************
Input:
ninf		x coordinate of source (must be within x samples)
xxxx

Output:
trcoefs		array[1..ninf] containing transmission coefficients
******************************************************************************
Notes:
The parameters are exactly the same as in the main program.
This is a subroutine so that the temporary arrays may be disposed
of after exit. This routine is only called once.
******************************************************************************
Author:  Brian Sumner, Colorado School of Mines, 1985
******************************************************************************/
{
/**
* Local variables:
*    TEMP, TMP - temporary arrays
*    I, J - loop variables
*    T1, T2 - temporaries
*    R, P - more temporaries
**/

	int i,j;
	float t1, t2, r, p, *temp, **tmp;

	temp = ealloc1float(ninf+1);
	tmp = ealloc2float(ninf+1, ninf+1);
	
	for (i = 0; i <= ninf; ++i)  temp[i] = rho[i]*v[i];

	for (j = 1; j <= ninf; ++j) {
		for (i = 1; i < j; ++i) {
			t1 = temp[i]*cos(theta[j][i-1]+dip[i]);
			t2 = temp[i-1]*cos(theta[j][i]+dip[i]);
			r = (t1 - t2)/(t1 + t2);
			tmp[j][i] = 1.0 - r*r;
		}
	}

	for (j = 1; j <= ninf; ++j) {
		t1 = temp[j];
		t2 = temp[j-1];
		p = (t1 - t2)/(t1 + t2);
		for (i = 1; i < j; ++i)  p *= tmp[j][i];
		trcoefs[j] = p;
	}
	
	/* free space */
	free1float(temp);
	free2float(tmp);
}
