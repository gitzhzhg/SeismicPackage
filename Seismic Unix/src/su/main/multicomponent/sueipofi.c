/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUEIPOFI: $Revision: 1.6 $ ; $Date: 2011/11/16 22:58:31 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"                                                                       ",
" SUEIPOFI - EIgenimage (SVD) based POlarization FIlter for             ",
"            three-component data                                       ",
"                                                                       ",
" sueipofi <stdin >stdout [optional parameters]                         ",
"                                                                       ",
" Required parameters:                                                  ",
"    none                                                               ",
"                                                                       ",
" Optional parameters:                                                  ",
"    dt=(from header)  time sampling intervall in seconds               ",
"    wl=0.1            SVD time window length in seconds                ",
"    pwr=1.0           exponent of filter weights                       ",
"    interp=cubic      interpolation between initially calculated       ",
"                      weights, choose \"cubic\" or \"linear\"          ",
"    verbose=0         1 = echo additional information                  ",
"                                                                       ",
"    file=polar        base name for additional output file(s) of       ",
"                      filter weights (see flags below)                 ",
"    rl1=0             1 = rectilinearity along first principal axis    ",
"    rl2=0             1 = rectilinearity along second principal axis   ",
"    pln=0             1 = planarity                                    ",
"                                                                       ",
"                                                                       ",
" Notes:                                                                ",
"    Three adjacent traces are considered as one three-component        ",
"    dataset.                                                           ",
"                                                                       ",
"    The filter is the sum of the first two eigenimages of the singular ",
"    value decomposition (SVD) of the signal matrix (time window).      ",
"    Weighting functions depending on linearity and planarity of the    ",
"    signal are applied, additionally. To avoid edge effects, these are ",
"    interpolated linearily or via cubic splines between initially      ",
"    calculated values of non-overlapping time windows.                 ",
"    The algorithm is based on the assumption that the particle motion  ",
"    trajectory is essentially 2D (elliptical polarization).            ",
"                                                                       ",
" Caveat:                                                               ",
"    Cubic spline interpolation may result in filter weights exceeding  ",
"    the set of values of initial weights. Weights outside the valid    ",
"    interval [0.0, 1.0] are clipped.                                   ",
"                                                                       ",
NULL};

/* 
 * Author: Nils Maercklin, 
 *         GeoForschungsZentrum (GFZ) Potsdam, Germany, 2001.
 *         E-mail: nils@gfz-potsdam.de
 *
 *
 * References:
 *    Franco, R. de, and Musacchio, G., 2000: Polarization Filter with
 *       Singular Value Decomposition, submitted to Geophysics and
 *       published electronically in Geophysics online (www.geo-online.org).
 *    Jurkevics, A., 1988: Polarization analysis of three-comomponent
 *       array data, Bulletin of the Seismological Society of America, 
 *       vol. 78, no. 5.
 *    Press, W. H., Teukolsky, S. A., Vetterling, W. T., and Flannery, B. P.
 *       1996: Numerical Recipes in C - The Art of Scientific Computing,
 *       Cambridge University Press, Cambridge.
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: none
 */
/**************** end self doc *******************************************/


/* function prototypes */
void do_intcub(float *iny, float *outy, int nin, int din, int inx0, int nout);
void do_intlin(float *iny, float *outy, int nin, int din, int inx0, int nout);
float calc_svdrl(float *w, int axis, float pwr);
float calc_planarity(float *w, float pwr);

void fputdata(FILE *fileptr, FILE *headerptr, float outdata[], int nt);
void fputdata3c(FILE *fileptr, FILE *headerptr, float **outdata3c, int nt);


segy tr;

int
main(int argc, char **argv)
{
    /* output file pointers */    
    FILE *headerfp;     /* temporary file for trace headers (one 3C station) */
    FILE *rl1fp=NULL, *rl2fp=NULL, *plnfp=NULL;
		/* files for polarization attributes */
        
    char *file;           /* base of output file name(s) */
    char *fname;          /* complete output file name */
        
    /* flags (see selfdoc) */
    int verbose, rl1, rl2, pln;
    
    register int i,icomp; /* indices for components (in loops) */
    register int j;       /* loop index */
    register int it, jt;  /* indices for time sample in main loop */
    register int iw;      /* time window counter */
    int nw;               /* number of windows per trace */
    int nstat;            /* number of 3-component datasets */
    int nt;               /* number of time samples in one trace */
    int iwl;              /* correlation window length in samples */
    float wl;             /* correlation window length in seconds */
    float pwr;            /* exponent of polarization attributes */
    float dt;             /* sampling intervall in seconds */
    float **data;         /* the three-component data */
    float **udata;        /* windowed data and matrix U after SVD */
    float **vdata;        /* matrix V */
    float *sval;          /* array of singular values */
    float **initpa;       /* initially calculated polarization attributes */
    float **polar;        /* cubic spline interpolated polarization attributes */
    float ***eigen;       /* data cube containing eigenimages */
    char *interp;         /* interpolation method */
    cwp_Bool iscubic=cwp_false;     /* true for cubic spline interpolation */
    
    /* initialize */
    initargs(argc, argv);
    requestdoc(1);
    
    /* get info from first trace */
    if(!gettr(&tr)) err("can't get first trace");
    nt = tr.ns;
    
    
    /* get parameters */
    if (!getparfloat("wl", &wl)) wl = 0.1;
    if (!getparfloat("dt", &dt)) dt = ((double) tr.dt)/1000000.0;
    if (!getparfloat("pwr", &pwr)) pwr = 1.0;
    if (!getparstring("interp", &interp)) interp = "cubic";
    if (!getparstring("file", &file)) file = "polar";
    if (!getparint("rl1", &rl1)) rl1 = 0;
    if (!getparint("rl2", &rl2)) rl2 = 0;
    if (!getparint("pln", &pln)) pln = 0;    
    if (!getparint("verbose", &verbose)) verbose = 0;
    
    checkpars();

    /* get interpolation method */
    if (STREQ(interp, "cubic")) iscubic=cwp_true;
    else if (STREQ(interp, "linear")) iscubic=cwp_false;
    else err("unknown interp=%s", interp);
    
    /* convert seconds to samples */
    if (!dt) {
        dt = 0.004;
        warn("dt not set, assuming dt=0.004");
    }
    iwl = NINT(wl/dt);
        
    /* data validation */
    if (iwl<3) err("wl=%g must be >= %g sec. (3 samples)", wl, 3.0*dt);
    if (iwl>nt) err("wl=%g too long for trace", wl);
    if (pwr<0.0) err("pwr=%g must be positive", pwr);
    
    /* number of time windows per trace */
    nw = nt / iwl;

    /* echo window information */
    if (verbose) warn("%d windows of %d samples per trace\n", nw, iwl);
    

    /* open temporary file for trace headers */
    headerfp = etmpfile();

    /* set filenames and open files for polarization attributes */
    fname = malloc( strlen(file)+5 ); 
    sprintf(fname, "%s.rl1", file); if (rl1) rl1fp = efopen(fname, "w");
    sprintf(fname, "%s.rl2", file); if (rl2) rl2fp = efopen(fname, "w");
    sprintf(fname, "%s.pln", file); if (pln) plnfp = efopen(fname, "w");
    free(fname);
    
    /* allocate space */
    data = ealloc2float(nt,3);    /* input data */
    udata = ealloc2float(3,iwl);  /* SVD matrix U */
    vdata = ealloc2float(3,3);    /* SVD matrix V */
    sval = ealloc1float(3);       /* singular values */    
    eigen = ealloc3float(nt,3,3); /* cube of eigenimages */
    polar = ealloc2float(nt,3);   /* polarization attributes rl1, rl2, p */
    initpa = ealloc2float(nw,3);  /* initially calc. polarization attributes */
    
    /* loop over traces */
    icomp=0;
    nstat=0;
    do {
        /* store trace header in temporary file and read data */
        efwrite(&tr, HDRBYTES, 1, headerfp);
        memcpy((void *) data[icomp], (const void *) tr.data, nt*FSIZE);
        icomp++;
        
        /* process 3-component dataset */
        if (icomp==3) {
            erewind(headerfp);
            icomp = 0;
            nstat++;
            if (verbose) 
                fprintf(stderr,"%s: processing station %d \r",argv[0], nstat);
            
            /* start loop over time windows */
            for (iw=0,jt=0; iw<nw; iw++,jt+=iwl) {
                
                /* copy data into matrix for SVD */
                for (i=0; i<3; i++) {
                    for (it=0; it<iwl; it++)
                        udata[it][i] = data[i][it+jt];
                }

                /* perform singular value decomposition (SVD) */
                compute_svd(udata, iwl, 3, sval, vdata);
                
                /* sort singular values in descending order */
                svd_sort(udata, sval, vdata, 3, iwl);

                
                /* construct and save eigenimages */
                for (j=0; j<3; j++) {
                    for (i=0; i<3; i++) {
                        for (it=jt; it<jt+iwl; it++) {
                            eigen[j][i][it] = \
                                udata[it-jt][j] * vdata[i][j] * sval[j];
                        }
                    }
                }

                /* calculate polarization attributes */
                for (j=0; j<2; j++) initpa[j][iw] = calc_svdrl(sval,j,pwr);
                initpa[2][iw] = calc_planarity(sval,pwr);

            } /* end loop time windows */

            /* interpolation of all polarization attributes */
            /* initially calculated values are asigned to window centers */
            for (i=0; i<3; i++) {
                if (iscubic) {
                    /* interpolation via cubic splines */
                    do_intcub( initpa[i], polar[i], nw, iwl, iwl/2, nt);
                    
                    /*... and clip values outside valid range */
                    for (it=0;it<nt; it++) {
                        if (polar[i][it]>1.0) polar[i][it] = 1.0;
                        if (polar[i][it]<0.0) polar[i][it] = 0.0;
                    }
                        
                }
                else {
                    /* linear interpolation */
                    do_intlin( initpa[i], polar[i], nw, iwl, iwl/2, nt);
                }
            }
            
            /* filter: polarization attribute weighted eigenimage stack */
            /* loop over components */

            for (i=0; i<3; i++) {
                /*loop over samples */
                for (it=0; it<(nw*iwl); it++) {
                    data[i][it] = eigen[0][i][it]*polar[0][it] + \
                                  eigen[1][i][it]*polar[1][it];
                    data[i][it] *= polar[2][it];
                }
            }

            /* write three-component data to stdout */
            fputdata3c(stdout, headerfp, data, nt);
            
            /* write polarization attributes to output files */
            if (rl1) fputdata(rl1fp, headerfp, polar[0], nt);
            if (rl2) fputdata(rl2fp, headerfp, polar[1], nt);
            if (pln) fputdata(plnfp, headerfp, polar[2], nt);            
            
        } /* end of processing three-component dataset */
        
    } while (gettr(&tr)); /* end loop over traces */
    
    if (verbose) {
        fprintf(stderr,"\n");
        if (icomp) warn("last %d trace(s) skipped", icomp);
    }
    
    /* close files for polarization attributes */
    if (rl1) efclose(rl1fp);
    if (rl2) efclose(rl2fp);
    if (pln) efclose(plnfp);
      
    return(CWP_Exit());
}

/**********************************************************************/
/* Functions used for polarization analysis                           */
/**********************************************************************/

/* rectilinearity, calculated from singular values                    */
/* (after Jurkevics, 1988, Franco and Musacchio, 2000)                */
/* w[3] = array containing singular values in descending order        */

float calc_svdrl(float *w, int axis, float pwr)
{
    float rl;   /* rectilinearity */

    if (w[axis]*w[axis]) {
        rl = 1.0 - (w[2]*w[2]) / (w[axis]*w[axis]);
    }
    else {
        rl = 0.0;
    }
        
    return pow(rl,pwr);
}

/* planarity, calculated from singular values (Jurkevics, 1988)       */
/* w[3] = array containing singular values in descending order        */
float calc_planarity(float *w, float pwr)
{
    float p;    /* planarity */
    
    if (w[0] + w[1]) {
        p = 1.0 - (2.0 * w[2]*w[2]) / (w[0]*w[0] + w[1]*w[1]);
    }
    else {
        p = 0.0;
    }
    
    return pow(p,pwr);
}

/**********************************************************************/
/* interpolation                                                      */
/**********************************************************************/

/* For more information on cubic spline interpolation methods used    */
/* here, see comments in "$CWPROOT/src/cwp/lib/cubicspline.c" and     */
/* "$CWPROOT/src/cwp/lib/intcub.c".                                   */

/* perform cubic spline interpolation of polarization attributes */
/* here: nin=nw, din=iwl, nout=nt, inx0=iwl/2                    */

void do_intcub(float *iny, float *outy, int nin, int din, int inx0, int nout)
{
    register int ix;
    float *inx, *outx;
    float (*yd)[4];
    
    /* allocate space */
    yd = (float(*)[4])ealloc1float(4*nin);
    inx = ealloc1float(nin);
    outx = ealloc1float(nout);

    /* initialize abscissa */   
    for (ix=0; ix<nin; ix++) {
        inx[ix] = (float) (inx0 + ix * din);
    }
    for (ix=0; ix<nout; ix++) {
        outx[ix] = (float) ix;
    }

    /* compute cubic spline coefficients */
    csplin(nin,inx,iny,yd);

    /* cubic spline interpolation */
    intcub(0,nin,inx,yd,nout,outx,outy);
    
    /* free memory */
    free(yd);
    free1float(inx);
    free1float(outx);

}

/* perform linear interpolation of polarization attributes       */
/* here: nin=nw, din=iwl, nout=nt, inx0=iwl/2                    */

void do_intlin(float *iny, float *outy, int nin, int din, int inx0, int nout)
{
    register int ix;
    float *inx, *outx;
    
    /* allocate space */
    inx = ealloc1float(nin);
    outx = ealloc1float(nout);

    /* initialize abscissa */   
    for (ix=0; ix<nin; ix++) {
        inx[ix] = (float) (inx0 + ix * din);
    }
    for (ix=0; ix<nout; ix++) {
        outx[ix] = (float) ix;
    }

    /* linear interpolation */
    intlin(nin,inx,iny,iny[0],iny[nin],nout,outx,outy);
    
    /* free memory */
    free1float(inx);
    free1float(outx);

}

/**********************************************************************/
/* utility functions                                                  */
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
   
    for(i=0;i<3;i++) {
        efread(&tr, 1, HDRBYTES, headerptr);
        
        memcpy((void *)tr.data, (const void *) outdata3c[i], nt*FSIZE);

        fputtr(fileptr, &tr);
    }
    erewind(headerptr);
}
/* END OF FILE */
