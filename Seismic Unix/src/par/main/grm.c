/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* GRM $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $  */ 

#include "par.h"

/************************** self documentation ************************/
char *sdoc[] = {
" 									",
" GRM - Generalized Reciprocal refraction analysis for a single layer	",
" 									",
"     grm <stdin >stdout  [parameters]    		 		",
" 									",
" Required parameters:							",
" nt=		Number of arrival time pairs				",
" dx=		Geophone spacing (m)					",
" v0=		Velocity in weathering layer (m/s)			",
" abtime=	If set to 0, use last time as a-b, else give time (ms)  ",
"									",
" Optional parameters:							",
" XY=      Value of XY if you want to override the optimum XY		",
"	  algorithm in the program. If it is not an integer multiple of ",
"	 dx, then it will be converted to the closest			",
"		 one.							",
"	XYmax   Maximum offset distance allowed when searching for      ",
"		optimum XY (m)  (Default is 2*dx*10)			",
"	depthres  Size of increment in x during verical depth search(m) ",
"		  (Default is 0.5m)					",
" Input file:								",
"	4 column ASCII - x,y, forward time, reverse time 		",
" Output file:								",
"	1) XYoptimum  							",
"	2) apparent refractor velcocity					",
"	3) x, y, z(x,y), y-z(x,y)					",
"		z(x,y) = calculated (GRM) depth below (x y) 		",
"		y-z(x,y) = GRM depth subtracted from y - absolute depth ",
"      .............							",
"      4) x, y, d(x,y), y-d(x,y), (error)  				",
"		d(x,y) = dip corrected depth estimate below (x,y)       ",
"		y-d(x,y) = dip corrected absolute depth 		",
"		error = estimated error in depth due only to the inexact",
"		      matching of tangents to arcs in dip estimate.	",
"									",
"      If the XY calculation is bypassed and XY specified, the values	",
"      used will precede 1) above.  XYoptimum will still be calculated	",
"      and displayed for reference.					",
"									",
" Notes:							       	",
"      Uses average refactor velocity along interface.			",
NULL}; 

/*  Credits:							       
 *
 *     CWP: Steven D. Sheaffer						 
 *								       
 *     D. Palmer, "The Generalized Reciprocal Method of Seismic	  
 *     Refraction Interpretation", SEG, 1982.				  
 *
 */
/************************* end self doc *******************************/

#define EPS .00001

int
main(int argc, char **argv)
{
	int i,j,nt,offset=0,jmax,jdx;
	float dx,v0,abtime,xymax,v1;
	float *x,*y,*tf,*tr,*lap,*zz,*d;
	float r1,r2,r3,r4,dlap,hold,slope,dslope;
	float *tv,tay,tbx,tab,tg,xy,depthres;
	float x1,x2,y1,y2,mc,mt,resid,newresid;
	float errd=0.0;

/* Initialize  */
	initargs(argc, argv);
	requestdoc(1);

/* Get parameters */
	if(!getparint("nt", &nt)) err("must specify nt");
	if(!getparfloat("dx", &dx)) err("must specify dx");
	if(!getparfloat("v0", &v0)) err("must specify v0");
	if(!getparfloat("abtime", &abtime)) err("must specify abtime");
	if(!getparfloat("xy", &xy))    xy=99999.9   ;
	if(!getparfloat("xymax", &xymax))     xymax = 2*dx*10   ;
	if(!getparfloat("depthres", &depthres))  depthres = 0.5   ;

        checkpars();

	/*  Turn XYmax into an integer # of samples on each side  */
	jmax = (xymax/2.0)/dx;

/* Allocate memory to arrays  */
	x=alloc1float(nt+1);
	y=alloc1float(nt+1);
	tf=alloc1float(nt+1);
	tr=alloc1float(nt+1);
	lap=alloc1float(jmax+1);
	tv=alloc1float(nt+1);
	zz=alloc1float(nt+1);
	d=alloc1float(nt+1); 

/* Read arrival time data */
	for(i=1;i<=nt;i++){
	scanf("%f %f %f %f",&r1,&r2,&r3,&r4);
	x[i]=r1; y[i]=r2; tf[i]=r3*0.001; tr[i]=r4*0.001;
	};

/* Set A-B time. If array type 2, use given abtime - if type 1, use */
/* last times.  						    */ 

	/* line here to check if tf[nt]=tr[1] for type 1 */

		if(abtime != 0.0)
			tab = abtime*0.001;
		else {
			tab = tf[nt];
		};

/* If XY is specified (flag1=1) then find offset; If not (flag1=0),   */
/* calculate the VELOCITY ANALYSIS FUNCTION for various values of XY. */
/* The data are at integer multiples of dx, so let XY be integer      */
/* multiples of dx, and generate a tv function at each. Determine a   */ 
/* measure of smoothness for each by finding the Laplacian along tv,  */
/* and use to determine the OPTIMUM XY.  Let j be the half-offset in  */ 
/* # of samples.						      */


	for(j=0; j<=jmax; j++){

		/* Because tay and tbx reach ahead and behind each mid*/
		/* point, tv can only be determined for midpoints from*/
		/* i=j+1 to nt-j, so the number of midpoints that can */
		/* be evaluated depends on the offset (j). Here, limit*/
		/* the range with jmax, so that all tv[i] are taken   */ 
		/* over the same points and are of equal length.      */ 

		for(i=jmax+1; i<=nt-jmax; i++){
		    tay = tf[i+j];
		    tbx = tr[i-j];
		    tv[i] = (tay - tbx + tab)/2; 
		};

	/* Find a measure of smoothness for tv[i] at this offset (j)  */
	/* using the FD approx. for the Laplacian and summing.	*/

		lap[j] = 0.0; dlap = 0.0;
		for(i=jmax+2; i<=nt-jmax-1; i++){
		   dlap = 1000.*(tv[i-1]-2*tv[i]+tv[i+1])/(dx*dx);
		   lap[j]=lap[j] + fabs(dlap);
		};

	};
	
	/* OPTIMUM XY is given by the smoothest tv function, which is */
	/* assumed to correspond to the smallest value of lap.	*/

	hold = 99999.0;

	for(j=jmax; j>=0; j--){
		if(lap[j] < hold) {
		   offset = j;
		   hold = lap[j];
		};
	}; 
	/* "offset" contains the half-optimum XY in samples.	 */
	printf("%s %5.1f \n","Optimum XY(m): ",offset*dx*2);

    	if(xy < 99999.7) {
	   offset=xy/(2*dx);
	   printf("%s %5.1f \n","XY Specified(m): ",xy);
	   printf("%s %5.1f \n","XY Used(m): ",offset*2*dx);
    	} 


/* Find the slope of tv optimum to get APPARENT REFRACTOR VELOCITY   */ 
/* (v1) via 1/average of slope. Generate tv[i] for the optimum or    */
/* specified offset.						     */ 

	for(i=offset+1; i<=nt-offset; i++){
		tay = tf[i+offset];
		tbx = tr[i-offset];
		tv[i] = (tay - tbx + tab)/2;
	};

	slope=0.0; dslope=0.0;

	/* To reduce effects of 1/small slopes, keep a total slope  */
	/* and take 1/avg slope, instead of averaging 1/slope.      */

	for(i=offset+1; i<=nt-offset-1; i++){
  		dslope = (tv[i+1]-tv[i])/dx;     
  		slope = slope + dslope;	  
	};

  	v1=(nt-(2*offset)-1)/slope;			  
  	printf("%s %5.1f \n","Avg. Refractor Vapp: ",v1);    
		

/* Calculate TIME DEPTH and DEPTH at each geophone location.	*/

	xy = offset*2*dx;

	/* Again, only the midpoints from i=offset+1 to nt-offset   */ 
	/* can be evaluated.					    */

	printf("%s \n","GRM RESULTS - X,Y,Z,(Y-Z):");

	for(i=offset+1; i<=nt-offset; i++){
		tay = tf[i+offset];
		tbx = tr[i-offset];
		tg = (tay + tbx - tab - (xy/v1))/2;
		zz[i] = tg*(v0*v1/sqrt((v1*v1)-(v0*v0)));
		printf("%f %f %f %f \n", x[i],y[i],zz[i],y[i]-zz[i]);	
	};

/* Convert depths to estimated VERTICAL DEPTH under each geophone by */
/* assuming the refractor is a line always tangent to the set of arcs*/
/* with radii zz(i) and center x(i), and assuming the refractor is   */
/* linear between geophone locations x(i).			   */

	printf("%s \n","DIP CORRECTED - X,Y,D,Y-D,error:");

  	for(i=offset+1;i<=nt-1-offset; i++){     

	   d[i]=0.0; 
	   newresid=0.0; resid=99999.0;
	   jdx = (2*zz[i]/depthres);

	   /* Move x1 along semicircle around xi w/ radius zi, find */
	   /* point on semicircle around xi+1, radius zi+1, with    */
	   /* same slope (same polar angle) x2-y2.  Here we assume  */
	   /* that for the pair, yi=0 since only the relative eleva-*/
	   /* -tions of the pair is needed to determine the tangent.*/

	   for(j=0; j<=jdx; j++){

	     x1 = x[i]-zz[i]+(j*depthres);
	     y1 = sqrt(fabs((zz[i]*zz[i])-((x1-x[i])*(x1-x[i]))));
	     x2 = ((x1-x[i])*zz[i+1]/zz[i]) + x[i+1];
	     y2 = sqrt(fabs((zz[i+1]*zz[i+1])-((x2-x[i+1])*(x2-x[i+1]))));

	     y2 = y2 + (y[i]-y[i+1]);

	   /* Compare slope of tangent line at x1-y1 with slope of line */
	   /* between x1-y1 and x2-y2.  When they are the same, mc is   */
	   /* slope of the line that is tangent to both semicircles.    */
	   /* (Assume this is the local dip at x[i])			*/
		
	     mc = (y1) ? (-(x1-x[i])*1/y1) : EPS;      
	     mt = (y2-y1)/(x2-x1);
	     newresid = fabs(mc-mt);

/*  	printf("%f %f %f %f %f %f %f \n",x1,y1,x2,y2,mc,mt,newresid);  */

	   /* Since the slopes will not match exactly, use a residual.  */
	   /* Want to keep results for the lowest residual, so check if */
	   /* this residual is lower than the lowest so far - if so calc*/
	   /* and reset lowest field (resid).     			*/

	     if(newresid < resid){
		 resid = newresid;
		 hold=atan(mc);
		 d[i]=zz[i]/(cos(hold));
		 /* estimate error using slope resid  */
		 hold=atan(mc+resid);
		 errd = fabs(d[i]-(zz[i]/cos(hold)));
	     };
	   };

	   printf("%f %f %f %f (%f) \n",x[i],y[i],d[i],(y[i]-d[i]),errd); 
	};	     


	return(CWP_Exit());
}
