/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPOLAR: $Revision: 1.9 $ ; $Date: 2015/08/07 22:21:04 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"                                                                       ",
" SUPOLAR - POLarization analysis of three-component data               ",
"                                                                       ",
" supolar <stdin [optional parameters]                                  ",
"                                                                       ",
" Required parameters:                                                  ",
"    none                                                               ",
"                                                                       ",
" Optional parameters:                                                  ",
"    dt=(from header)  time sampling intervall in seconds               ",
"    wl=0.1            correlation window length in seconds             ",
"    win=boxcar        correlation window shape, choose \"boxcar\",     ",
"                      \"hanning\", \"bartlett\", or \"welsh\"          ",
"    file=polar        base of output file name(s)                      ",
"    rl=1              1 = rectilinearity evaluating 2 eigenvalues,     ",
"                      2, 3 = rectilinearity evaluating 3 eigenvalues   ",
"    rlq=1.0           contrast parameter for rectilinearity            ",
"    dir=1             1 = 3 components of direction of polarization    ",
"                          (the only three-component output file)       ",
"    tau=0             1 = global polarization parameter                ",
"    ellip=0           1 = principal, subprincipal, and transverse      ",
"                          ellipticities e21, e31, and e32              ",
"    pln=0             1 = planarity measure                            ",
"    f1=0              1 = flatness or oblateness coefficient           ",
"    l1=0              1 = linearity coefficient                        ",
"    amp=0             1 = amplitude parameters: instantaneous,         ",
"                          quadratic, and eigenresultant ir, qr, and er ",
"    theta=0           1, 2, 3 = incidence angle of principal axis      ",
"    phi=0             1, 2, 3 = horizontal azimuth of principal axis   ",
"    angle=rad         unit of angles theta and phi, choose \"rad\",    ",
"                      \"deg\", or \"gon\"                              ",
"    all=0             1, 2, 3 = set all output flags to that value     ",
"    verbose=0         1 = echo additional information                  ",
"                                                                       ",
"                                                                       ",
" Notes:                                                                ",
"    Three adjacent traces are considered as one three-component        ",
"    dataset.                                                           ",
"    Correct calculation of angles theta and phi requires the first of  ",
"    these traces to be the vertical component, followed by the two     ",
"    horizontal components (e.g. Z, N, E, or Z, inline, crossline).     ",
"    Significant signal energy on Z is necessary to resolve the 180 deg ",
"    ambiguity of phi (options phi=2,3 only).                           ",
"                                                                       ",
"    Each calculated polarization attribute is written into its own     ",
"    SU file. These files get the same base name (set with \"file=\")   ",
"    and the parameter flag as an extension (e.g. polar.rl).            ",
"                                                                       ",
"    In case of a tapered correlation window, the window length wl may  ",
"    have to be increased compared to the boxcar case, because of their ",
"    smaller effective widths (Bartlett, Hanning: 1/2, Welsh: 1/3).     ",
"                                                                       ",
" Range of values:                                                      ",
"    parameter     option  interval                                     ",
"    rl            1, 2    0.0 ... 1.0   (1.0: linear polarization)     ",
"    rl            3      -1.0 ... 1.0                                  ",
"    tau, l1       1       0.0 ... 1.0   (1.0: linear polarization)     ",
"    pln, f1       1       0.0 ... 1.0   (1.0: planar polarization)     ",
"    e21, e31, e32 1       0.0 ... 1.0   (0.0: linear polarization)     ",
"    theta         1      -pi/2... pi/2  rad                            ",
"    theta         2, 3    0.0 ... pi/2  rad                            ",
"    phi           1      -pi/2... pi/2  rad                            ",
"    phi           2      -pi  ... pi    rad   (see notes above)        ",
"    phi           3       0.0 ... 2 pi  rad   (see notes above)        ",
"                                                                       ",
"                                                                       ",
NULL};

/* 
 * Author: Nils Maercklin, 
 *         GeoForschungsZentrum (GFZ) Potsdam, Germany, 1998-2001.
 *         E-mail: nils@gfz-potsdam.de
 * 
 *
 * References:
 *    Jurkevics, A., 1988: Polarization analysis of three-component
 *       array data. Bulletin of the Seismological Society of America, 
 *       vol. 78, no. 5.
 *    Kanasewich, E. R., 1981: Time Sequence Analysis in Geophysics.
 *       The University of Alberta Press.
 *    Kanasewich, E. R., 1990: Seismic Noise Attenuation.
 *       Handbook of Geophysical Exploration, Pergamon Press, Oxford.
 *    Meyer, J. H. 1988: First Comparative Results of Integral and
 *       Instantaneous Polarization Attributes for Multicomponent Seismic
 *       Data. Institut Francais du Petrole.
 *    Press, W. H., Teukolsky, S. A., Vetterling, W. T., and Flannery, B. P.
 *       1996: Numerical Recipes in C - The Art of Scientific Computing.
 *       Cambridge University Press, Cambridge.
 *    Samson, J. C., 1973: Description of the Polarisation States of Vector
 *       Processes: Application to ULF Electromagnetic Fields.
 *       Geophysical Journal vol. 34, p. 403-419.
 *    Sheriff, R. E., 1991: Encyclopedic Dictionary of Exploration
 *       Geophysics. 3rd ed., Society of Exploration Geophysicists, Tulsa.
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: none
 */
/**************** end self doc *******************************************/

/* prototypes of functions used internally */
float calc_ellip(float *d, int d1, int d2);
float calc_er(float *d);
float calc_f1(float *d);
float calc_l1(float *d);
float calc_phi(float **v, int opt);
float calc_plan(float *d);
float calc_rl(float d[], float q, int opt);
float calc_tau(float *d);
float calc_theta(float **v, int opt);
float covar(float *data1, float *data2, int istart, int iwl, float *w);
void ampparams(float **indata, float *data_ir, float *data_qr, int nt, int iwl);
void calc_dir(float **data3c_dir, float **v, int it);
void calc_window(float *w, int iwl, int iwin);
void fputdata(FILE *fileptr, FILE *headerptr, float outdata[], int nt);
void fputdata3c(FILE *fileptr, FILE *headerptr, float **outdata3c, int nt);


/* window shape identifiers */
#define WBOXCAR 0
#define WBARTLETT 1
#define WHANNING 2
#define WWELSH 3


segy tr;                /* SEG-Y record (trace and header) */

int
main(int argc, char **argv)
{
    /* output file pointers */    
    FILE *rlfp=NULL, *taufp=NULL, *e21fp=NULL;
    FILE *e31fp=NULL, *e32fp=NULL, *plnfp=NULL;
    FILE *f1fp=NULL, *l1fp=NULL;
    FILE *thetafp=NULL, *phifp=NULL, *dirfp=NULL;
    FILE *erfp=NULL, *irfp=NULL, *qrfp=NULL;
    FILE *headerfp=NULL; /* temporary file for trace headers */
			 /* (one 3C station only) */
        
    char *file=NULL;         /* base of output file name(s) */
    char *fname=NULL;        /* complete output file name */
    char *angle=NULL;        /* unit used for angles theta and phi */
    char *win=NULL;          /* shape of used time window */
    float fangle=0.0;   /* unit conversion factor applied to angles theta and phi */
    int iwin=0;           /* time window shape identifier */
        
    /* flags (see selfdoc) */
    int rl,theta,phi,tau,ellip,pln,f1,l1,dir,amp,verbose,all;
    
    int i,j,icomp;      /* indices for components (in loops) */
    int it;             /* index for time sample in main loop */
    int iwl;            /* correlation window length in samples */
    int nstat;          /* number of 3-component datasets */
    int nt;             /* number of time samples in one trace */

    float **data3c;     /* three-component data ([1..3][0..nt-1]) */
    float **a;          /* covariance matrix (a[1..3][1..3]) */
    float **v;          /* eigenvectors of covariance matrix (v[1..3][1..3]) */
    float *d;           /* the corresponding eigenvalues (d[1..3]) */
    float *w;           /* time window weights for correlation window */
    float dt;           /* sampling intervall in seconds */
    float rlq;          /* contrast factor of rectilinearity */
    float wl;           /* correlation window length in seconds */
    
    float *data_e21=NULL;    /* main ellipticity */
    float *data_e31=NULL;    /* second ellipticity */
    float *data_e32=NULL;    /* transverse ellipticity */
    float *data_er=NULL;     /* eigenresultant */
    float *data_f1=NULL;     /* flatness coefficient */
    float *data_ir=NULL;     /* instantaneous resultant */
    float *data_l1=NULL;     /* linearity coefficient */
    float *data_phi=NULL;    /* horizontal azimuth phi */
    float *data_pln=NULL;    /* planarity */    
    float *data_qr=NULL;     /* quadratic resultant */
    float *data_rl=NULL;     /* rectilinearity factor */
    float *data_tau=NULL;    /* polarization parameter tau */
    float *data_theta=NULL;  /* inclination angle theta */
    float **data3c_dir=NULL; /* 3 components of direction of polarization ([1..3][0..nt-1]) */
    
    
    /* initialize */
    initargs(argc, argv);
    requestdoc(1);
    
    /* get info from first trace */
    if(!gettr(&tr)) err("can't get first trace");
    nt = tr.ns;
       
    /* get parameters ... */
    if (!getparstring("file", &file)) file="polar";
    if (!getparstring("angle", &angle)) angle="rad";
    if (!getparstring("win", &win)) win="boxcar";
    if (!getparfloat("wl", &wl)) wl = 0.1;
    if (!getparfloat("dt", &dt)) dt = ((double) tr.dt)/1000000.0;
    if (!getparfloat("rlq", &rlq)) rlq = 1.0;
    if (!getparint("verbose", &verbose)) verbose = 0;
    
    /* ... and output flags */
    if (!getparint("all", &all))       all = 0;
    if (!getparint("rl", &rl))          rl = (all) ? all : 1;
    if (!getparint("dir", &dir))       dir = (all) ? 1 : 1;
    if (!getparint("theta", &theta)) theta = (all) ? all : 0;
    if (!getparint("phi", &phi))       phi = (all) ? all : 0;
    if (!getparint("tau", &tau))       tau = (all) ? 1 : 0;
    if (!getparint("ellip", &ellip)) ellip = (all) ? 1 : 0;
    if (!getparint("pln", &pln))       pln = (all) ? 1 : 0;
    if (!getparint("f1", &f1))          f1 = (all) ? 1 : 0;
    if (!getparint("l1", &l1))          l1 = (all) ? 1 : 0;
    if (!getparint("amp", &amp))       amp = (all) ? 1 : 0;

    checkpars();


    /* get time window shape */
    if      (STREQ(win, "boxcar"))   iwin=WBOXCAR;
    else if (STREQ(win, "bartlett")) iwin=WBARTLETT;
    else if (STREQ(win, "hanning"))  iwin=WHANNING;
    else if (STREQ(win, "welsh"))    iwin=WWELSH;
    else err("unknown win=%s", win);
    
    
    /* get unit conversion factor for angles */
    if      (STREQ(angle, "rad")) fangle=1.0;
    else if (STREQ(angle, "deg")) fangle=180.0/PI;
    else if (STREQ(angle, "gon")) fangle=200.0/PI;
    else err("unknown angle=%s", angle);

    /* convert seconds to samples */
    if (!dt) {
        dt = 0.004;
        warn("dt not set, assuming dt=0.004");
    }
    iwl = NINT(wl/dt);
        
    /* data validation */
    if (iwl<1) err("wl=%g must be positive", wl);
    if (iwl>nt) err("wl=%g too long for trace", wl);
    if (!strlen(file)) err("file= not set and default overridden");

    /* echo some information */
    if (verbose && (theta || phi)) warn("computing angles in %s", angle);
    if (verbose) warn("%s window length = %d samples\n", win, iwl);
    

    /* open temporary file for trace headers */
    headerfp = etmpfile();
    
    /* set filenames and open files */
    fname = malloc( strlen(file)+7 );
    sprintf(fname, "%s.rl", file);    if (rl) rlfp = efopen(fname, "w");
    sprintf(fname, "%s.theta", file); if (theta) thetafp = efopen(fname, "w");
    sprintf(fname, "%s.phi", file);   if (phi) phifp = efopen(fname, "w");
    sprintf(fname, "%s.tau", file);   if (tau) taufp = efopen(fname, "w");
    sprintf(fname, "%s.e21", file);   if (ellip) e21fp = efopen(fname, "w");
    sprintf(fname, "%s.e31", file);   if (ellip) e31fp = efopen(fname, "w");
    sprintf(fname, "%s.e32", file);   if (ellip) e32fp = efopen(fname, "w");
    sprintf(fname, "%s.pln", file);   if (pln) plnfp = efopen(fname, "w");
    sprintf(fname, "%s.f1", file);    if (f1) f1fp = efopen(fname, "w");
    sprintf(fname, "%s.l1", file);    if (l1) l1fp = efopen(fname, "w");
    sprintf(fname, "%s.dir", file);   if (dir) dirfp = efopen(fname, "w");
    sprintf(fname, "%s.er", file);    if (amp) erfp = efopen(fname, "w");
    sprintf(fname, "%s.ir", file);    if (amp) irfp = efopen(fname, "w");
    sprintf(fname, "%s.qr", file);    if (amp) qrfp = efopen(fname, "w");
    free(fname);
    
    /* allocate space for input data and analysis matrices */
    /* index ranges used here: data3c[1..3][0..nt-1],      */
    /* a[1..3][1..3], v[1..3][1..3], d[1..3]               */
    data3c = ealloc2float(nt,3); data3c-=1;
    a = ealloc2float(3,3); a[0]-=1; a-=1;
    v = ealloc2float(3,3); v[0]-=1; v-=1;    
    d = ealloc1float(3); d-=1;

    /* calculate time window weights */
    w = ealloc1float(iwl);
    memset((void *) w, 0, iwl*FSIZE);
    calc_window(w, iwl, iwin);

    /* allocate and zero out space for output data */
    if (rl) { 
        data_rl = ealloc1float(nt); 
        memset((void *) data_rl, 0, nt*FSIZE);
    }
    if (theta) {
        data_theta = ealloc1float(nt);
        memset((void *) data_theta, 0, nt*FSIZE);
    }
    if (phi) {
        data_phi = ealloc1float(nt);
        memset((void *) data_phi, 0, nt*FSIZE);
    }    
    if (tau) {
        data_tau = ealloc1float(nt);
        memset((void *) data_tau, 0, nt*FSIZE);
    }
    if (ellip) {
        data_e21 = ealloc1float(nt);
        data_e31 = ealloc1float(nt);
        data_e32 = ealloc1float(nt);
        memset((void *) data_e21, 0, nt*FSIZE);
        memset((void *) data_e31, 0, nt*FSIZE);
        memset((void *) data_e32, 0, nt*FSIZE);
    }
    if (pln) {
        data_pln = ealloc1float(nt);
        memset((void *) data_pln, 0, nt*FSIZE);
    }
    if (f1) {
        data_f1 = ealloc1float(nt);
        memset((void *) data_f1, 0, nt*FSIZE);
    }
    if (l1) {
        data_l1 = ealloc1float(nt);
        memset((void *) data_l1, 0, nt*FSIZE);
    }
    if (amp) {
        data_er = ealloc1float(nt);
        data_ir = ealloc1float(nt);
        data_qr = ealloc1float(nt);
        memset((void *) data_er, 0, nt*FSIZE);
        memset((void *) data_ir, 0, nt*FSIZE);
        memset((void *) data_qr, 0, nt*FSIZE);
    }
    if (dir) {
        data3c_dir = ealloc2float(nt,3); data3c_dir-=1;
        for (i=1;i<=3;i++) memset((void *) data3c_dir[i], 0, nt*FSIZE);
    }

    
    /* loop over traces */
    icomp=0;
    nstat=0;
    do {
        /* store trace header in temporary file and read data */
        efwrite(&tr, HDRBYTES, 1, headerfp);
        icomp++;
        memcpy((void *)data3c[icomp], (const void *) tr.data, nt*FSIZE);
                
        /* process 3-component dataset */
        if (icomp==3) {
            erewind(headerfp);
            icomp = 0;
            nstat++;
            if (verbose) 
                fprintf(stderr,"%s: analyzing station %d \r",argv[0], nstat);
            
            /* start loop over samples */
            for (it=iwl/2;it<nt-iwl/2;it++) {
                
                /* covariance matrix */
                for (i=1;i<=3;i++) {
                    for (j=i;j<=3;j++) {
                        a[i][j]=a[j][i]=covar(data3c[i], data3c[j], it-iwl/2, iwl, w);
                    }
                }
                    
                /* compute eigenvalues and -vectors */
                eig_jacobi(a,d,v,3);
                sort_eigenvalues(d,v,3);
                
                /* polarization parameters */
                if (rl) data_rl[it]=calc_rl(d,rlq,rl);
                if (theta) data_theta[it]=calc_theta(v, theta) * fangle;
                if (phi) data_phi[it]=calc_phi(v, phi) * fangle;
                if (tau) data_tau[it]=calc_tau(d);
                if (ellip) {
                    data_e21[it]=calc_ellip(d,2,1);
                    data_e31[it]=calc_ellip(d,3,1);
                    data_e32[it]=calc_ellip(d,3,2);
                }
                if (pln) data_pln[it]=calc_plan(d);
                if (f1) data_f1[it]=calc_f1(d);
                if (l1) data_l1[it]=calc_l1(d);
                if (amp) data_er[it]=calc_er(d);
                if (dir) calc_dir(data3c_dir,v,it);

            } /* end loop over samples */
             
            /* compute amplitude parameters */
            if (amp) ampparams(data3c, data_ir, data_qr, nt, iwl);

            /* write polarization attributes to files */
            if (rl) fputdata(rlfp, headerfp, data_rl, nt);
            if (theta) fputdata(thetafp, headerfp, data_theta, nt);
            if (phi) fputdata(phifp, headerfp, data_phi, nt);
            if (tau) fputdata(taufp, headerfp, data_tau, nt);
            if (ellip) {
                fputdata(e21fp, headerfp, data_e21, nt);
                fputdata(e31fp, headerfp, data_e31, nt);
                fputdata(e32fp, headerfp, data_e32, nt);
            }
            if (pln) fputdata(plnfp, headerfp, data_pln, nt);
            if (f1) fputdata(f1fp, headerfp, data_f1, nt);
            if (l1) fputdata(l1fp, headerfp, data_l1, nt);
            if (amp) {
                fputdata(erfp, headerfp, data_er, nt);
                fputdata(irfp, headerfp, data_ir, nt);
                fputdata(qrfp, headerfp, data_qr, nt);
            }
            if (dir) fputdata3c(dirfp, headerfp, data3c_dir, nt);
            
        } /* end of processing three-component dataset */
        
    } while (gettr(&tr)); /* end loop over traces */
    
    if (verbose) {
        fprintf(stderr,"\n");
        if (icomp) warn("last %d trace(s) skipped", icomp);
    }
    
    /* close files */
    efclose(headerfp);
    if (rl) efclose(rlfp);
    if (theta) efclose(thetafp);
    if (phi) efclose(phifp);
    if (tau) efclose(taufp);
    if (ellip) {
        efclose(e21fp);
        efclose(e31fp);
        efclose(e32fp);
    }
    if (pln) efclose(plnfp);
    if (f1) efclose(f1fp);
    if (l1) efclose(l1fp);
    if (amp) {
        efclose(erfp);
        efclose(irfp);
        efclose(qrfp);
    }
    if (dir) efclose(dirfp);
      
    return(CWP_Exit());
}



/**********************************************************************/
/* Functions used internally                                          */
/**********************************************************************/

/* calculate time window weights for a smooth time window,        */
/* after Sheriff, 1991, p. 335; tapered windows are suggested by  */
/* Jurkevics, 1988                                                */

void calc_window(float *w, int iwl, int iwin)
{
    int i,j;      /* loop indices within window */
    float m;      /* half of time window (iwl/2) */
    
    m = (float) iwl/2;
    
    switch (iwin) {
        case WBOXCAR :
            for (i=-iwl/2,j=0;i<iwl/2;i++,j++) {
                w[j] = 1.0;
            }
            if (j<iwl) w[iwl-1] = w[0];
            break;
        case WBARTLETT :
            for (i=-iwl/2,j=0;i<iwl/2;i++,j++) {
                w[j] = 1.0 - (fabs((float) i) / m);
                
            }
            if (j<iwl) w[iwl-1] = w[0];
            break;
        case WHANNING :
            for (i=-iwl/2,j=0;i<iwl/2;i++,j++) {
                w[j] = 0.5 + 0.5*cos(PI*fabs((float) i) / m);
            }
            if (j<iwl) w[iwl-1] = w[0];
            break;            
        case WWELSH :
            for (i=-iwl/2,j=0;i<iwl/2;i++,j++) {
                w[j] = fabs( fabs((float) i) / m - 1.0);
                w[j] *= w[j];            
            }
            if (j<iwl) w[iwl-1] = w[0];
            break;            
    }
}


/* covariance of two time sequences *data1 and *data2,   */
/* calculated in a window of length iwl,                 */
/* weighted with factors *w and starting at index istart */

float covar(float *data1, float *data2, int istart, int iwl, float *w)
{
    register int i, j;
    float cov=0.0;
    float mean1=0.0;
    float mean2=0.0;
    
    for (i=istart,j=0;i<(istart+iwl);i++,j++) {
        mean1 += data1[i] * w[j];
        mean2 += data2[i] * w[j];
    }
    mean1 = mean1 / (float) iwl;
    mean2 = mean2 / (float) iwl;

    for (i=istart,j=0;i<(istart+iwl);i++,j++) {
	/* modified by Mega G. Baitoff 6 May 2015 */
        /* cov += (data1[i]-mean1) * (data2[i]-mean2) * w[j]; */
        cov += (data1[i] * w[j] - mean1) * (data2[i] * w[j] - mean2);              
    }
    cov = cov / (float) iwl;
    return cov;
}   


/**********************************************************************/
/* Functions defining polarization attributes and angles              */
/**********************************************************************/

/* rectilinearity factor rl (3 different definitions) */

float calc_rl(float *d, float rlq, int opt)
{   
    float rl;
    
    if (!d[1]==0.0) {
        switch (opt) {
            default: 
                /* case 1: */
                /* rl definition after Kanasewich, 1981 */
                rl = 1 - pow(fabs(d[2]/d[1]), rlq);
                break;
            case 2:
                /* rl definition after Jurkevics, 1988 */
                rl = 1 - pow( 0.5*(fabs(d[2]/d[1]) + fabs(d[3]/d[1]) ), rlq);
                break;
            case 3:
                /* rl definition after Meyer, 1988 */
                rl = 1 - pow(fabs(d[2]/d[1]) + fabs(d[3]/d[1]), rlq);
                break;
        }
        return rl;
    }
    else {
        return 0.0;
    }
}

/* vertical (incidence) angle theta (different definitions) */
/* assumes a right handed coordinate system starting        */
/* with the vertical component Z (Z=1, N,R=2, E,T=3)        */

float calc_theta(float **v, int opt)
{
    float theta, horiz;
    
    switch (opt) {
        default:
            /* case 1: */
            /* definition after Kanasewich, 1981 */
            /* interval -pi/2 <= theta <= pi/2 */ 
            if (v[1][1]) {
                horiz = sqrt( v[2][1]*v[2][1] + v[3][1]*v[3][1] );
                theta = atan( horiz / v[1][1] );
            }
            else {
                theta = 0.0;
            }
            break;
        case 2:
        case 3:
            /* definition after Jurkevics, 1988 */
            /* interval 0.0 <= theta <= pi/2 */
            theta = acos( fabs(v[1][1]) );
            break;
    }
    return theta;            
}

/* horizontal azimuth phi (different definitions)    */
/* assumes a right handed coordinate system starting */
/* with the vertical component Z (Z=1, N,R=2, E,T=3) */

/* Note: The SIGN function is introduced to resolve  */
/* the 180 deg ambiguity by taking the positive      */
/* vertical component of v[1][1] (Jurkevics, 1988).  */
#define VSIGN ( (v[1][1]<0.0) ? -1.0 : 1.0 )

float calc_phi(float **v, int opt)
{
    float phi;
    
    switch (opt) {
        default:
            /* case 1: */
            /* definition after Kanasewich, 1981 */
            /* interval -pi/2 <= phi <= pi/2 */ 
            if (v[1][2]) {
                phi = atan( v[3][1] / v[2][1] );
            }
            else {
                phi = (v[3][1]>0.0) ? 0.5*PI : -0.5*PI;
            }
            break;
        case 2:
        case 3:
            /* definitions after Jurkevics, 1988 */
            /* interval -pi <= phi <= pi */
            if (v[2][1]) {
                phi = atan2( v[3][1]*VSIGN, v[2][1]*VSIGN);
            }
            else {
                phi = (v[3][1]>0.0) ? 0.5*PI*VSIGN : -0.5*PI*VSIGN;
            }
            
            /* interval 0.0 <= phi <= 2*pi */
            if (phi<0.0 && opt==3) phi += 2.0*PI;
            break;
    }   
    return phi;
}
#undef VSIGN

/* global polarization parameter tau (Samson, 1973) */

float calc_tau(float *d)
{
    float x1, x2, x3, x4, tau;
    
    if (d[1]) {   
        x1 = pow(( 1 - fabs(d[2]/d[1])), 2.);
        x2 = pow(( 1 - fabs(d[3]/d[1])), 2.);
        x3 = pow(( fabs(d[2]/d[1]) - fabs(d[3]/d[1]) ), 2.);
        x4 = pow(( 1 + fabs(d[2]/d[1]) + fabs(d[3]/d[1])), 2.);
        tau = sqrt( (x1+x2+x3) / (2*x4) );
        return tau;
    }
    else
        return 0.0;
}

/* ellipticities e_ik */

float calc_ellip(float *d, int d1, int d2)
{
    float ellip;
    
    if (d[d2]) {
        ellip = sqrt( fabs(d[d1]/d[d2]) );
        return ellip;
    }
    else
        return 0.0;
}

/* planarity after Jurkevics, 1988 */

float calc_plan(float *d)
{
    float pln;
    
    if (d[1]+d[2]) {
        pln = 1.0 - 2.0*d[3] / (d[1] + d[2]);
        return pln;
    }
    else
        return 0.0;
}

/* flatness coefficient f1 after Benhama et. al, 1988 */

float calc_f1(float *d)
{
    float f1,x1,x2;
    
    x1 = 3.0 * calc_ellip(d,3,1);
    x2 = 1.0 + calc_ellip(d,2,1) + calc_ellip(d,3,1);
    f1 = 1.0 - x1 / x2; 
    return f1;
}

/* linearity coefficient l1 */

float calc_l1(float *d)
{
    float l1,x1,x2;
    
    x1 = 3. * ( calc_ellip(d,2,1) + calc_ellip(d,3,1) );
    x2 = 2. * ( 1 + calc_ellip(d,2,1) + calc_ellip(d,3,1));
    l1 = 1. - x1 / x2;
    return l1;
}

/* direction cosines or directivity functions (3 components) */

void calc_dir(float **data3c_dir, float **v, int it)
{
    int i;
    for (i=1;i<=3;i++) {
        data3c_dir[i][it]=v[i][1];
    }
}

/* amplitude parameters */

/* eigenresultant */

float calc_er(float *d)
{
    float er;
    er = sqrt(fabs(d[1]));
    return er;
}

/* instantaneous and quadratic resultant (Meyer, 1988) */

void ampparams(float **indata, float *data_ir, float *data_qr, int nt, int iwl)
{
    int i,it;
    float sqrsum;
    
    for (it=0;it<nt;it++) {
        /* instantaneous resultant */
        data_ir[it] = sqrt(indata[1][it]*indata[1][it]+ \
            indata[2][it]*indata[2][it]+indata[3][it]*indata[3][it]);
        
        /* quadratic resultant */
        if ( (it >= iwl/2) && (it < nt-iwl/2) ) {
            sqrsum=0.0;
            for (i=it-iwl/2;i<(it+iwl);i++) {
                sqrsum += indata[1][i]*indata[1][i]+ \
                    indata[2][i]*indata[2][i]+indata[3][i]*indata[3][i];
            }
            data_qr[it] = sqrsum / (float) iwl;
        }
    }
}


/**********************************************************************/
/* Functions for data output                                          */
/**********************************************************************/


/* write one-component data into file */

void fputdata(FILE *fileptr, FILE *headerptr, float *outdata, int nt)
{    
    efread(&tr, 1, HDRBYTES, headerptr);
    erewind(headerptr);
    
    memcpy((void *)tr.data, (const void *) outdata, nt*FSIZE);

    fputtr(fileptr, &tr);
}

/* write three-component data into file */

void fputdata3c(FILE *fileptr, FILE *headerptr, float **outdata3c, int nt)
{
    int i;
   
    for(i=1;i<=3;i++) {
        efread(&tr, 1, HDRBYTES, headerptr);
        
        memcpy((void *)tr.data, (const void *) outdata3c[i], nt*FSIZE);

        fputtr(fileptr, &tr);
    }
    erewind(headerptr);
}

/* END OF FILE */
