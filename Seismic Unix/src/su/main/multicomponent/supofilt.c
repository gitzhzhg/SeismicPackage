/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPOFILT: $Revision: 1.5 $ ; $Date: 2011/11/16 22:58:31 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"                                                                       ",
" SUPOFILT - POlarization FILTer for three-component data               ",
"                                                                       ",
" supofilt <stdin >stdout [optional parameters]                         ",
"                                                                       ",
" Required parameters:                                                  ",
"    dfile=polar.dir   file containing the 3 components of the          ",
"                      direction of polarization                        ",
"    wfile=polar.rl    file name of weighting polarization parameter    ",
"                                                                       ",
" Optional parameters:                                                  ",
"    dt=(from header)  time sampling intervall in seconds               ",
"    smooth=1          1 = smooth filter operators, 0 do not            ",
"    sl=0.05           smoothing window length in seconds               ",
"    wpow=1.0          raise weighting function to power wpow           ",
"    dpow=1.0          raise directivity functions to power dpow        ",
"    verbose=0         1 = echo additional information                  ",
"                                                                       ",
"                                                                       ",
" Notes:                                                                ",
"    Three adjacent traces are considered as one three-component        ",
"    dataset.                                                           ",
"                                                                       ",
"    This program SUPOFILT is an extension to the polarization analysis ",
"    program supolar. The files wfile and dfile are SU files as written ",
"    by SUPOLAR.                                                        ",
"                                                                       ",
NULL};

/* 
 * Author: Nils Maercklin, 
 *         GeoForschungsZentrum (GFZ) Potsdam, Germany, 1999-2000.
 *         E-mail: nils@gfz-potsdam.de
 * 
 *
 * References:
 *    Benhama, A., Cliet, C. and Dubesset, M., 1986: Study and
 *       Application of spatial directional filtering in three 
 *       component recordings.
 *       Geophysical Prospecting, vol. 36.
 *    Kanasewich, E. R., 1981: Time Sequence Analysis in Geophysics, 
 *       The University of Alberta Press.
 *    Kanasewich, E. R., 1990: Seismic Noise Attenuation, 
 *       Handbook of Geophysical Exploration, Pergamon Press, Oxford.
 * 
 *
 * Trace header fields accessed: ns, dt
 */
/**************** end self doc *******************************************/

/* prototypes of functions used internally */
void do_smooth(float *data, int nt, int isl);

segy tr,dtr,wtr;

int
main(int argc, char **argv)
{       
    FILE *dfp, *wfp;
    int smooth, verbose; /* flags */
    int i,icomp;    /* indices for components (in loops) */
    int nt;           /* number of time samples in one trace */
    int isl;          /* smoothing window length in samples */
    int itr;          /* trace number */
    char *wfile;      /* file containing weighting function */
    char *dfile;      /* file containing direction of polarization */ 
    float sl;         /* smooth window length in seconds */
    float dt;         /* sampling intervall in seconds */
    float wpow;       /* exponent of weighting function of filter */
    float dpow;       /* exponent of directivity functions */

    /* initialize */
    initargs(argc, argv);
    requestdoc(1);

    /* get info from first trace */
    if(!gettr(&tr)) err("can't get first trace");
    nt = tr.ns;
           
    /*get parameters */
    if (!getparstring("wfile", &wfile)) wfile="polar.rl";
    if (!getparstring("dfile", &dfile)) dfile="polar.dir";
    if (!getparfloat("sl", &sl)) sl = 0.05;
    if (!getparfloat("dt", &dt)) dt = ((double) tr.dt)/1000000.0;
    if (!getparfloat("wpow", &wpow)) wpow = 1.0;
    if (!getparfloat("dpow", &dpow)) dpow = 1.0;
    if (!getparint("smooth", &smooth)) smooth = 1;
    if (!getparint("verbose", &verbose)) verbose = 0;

    checkpars();

    /* convert seconds to samples */
    if (!dt) {
        dt = 0.004;
        warn("dt not set, assuming dt=0.004");
    }
    isl = NINT(sl/dt);
        
    if (verbose && smooth) warn("smoothing window = %d samples", isl);

    /* Open the other two files and read first trace(s) */
    if (!(dfp=fopen(dfile,"r"))) err("file %s does not exist", dfile);
    if (!(wfp=fopen(wfile,"r"))) err("file %s does not exist", wfile);
    if(!fgettr(dfp,&dtr)) err("can't get first trace of %s",dfile);
    if(!fgettr(wfp,&wtr)) err("can't get first trace of %s",wfile);
    

    /* data validation */
    if (smooth && isl<1) err("sl=%g must be positive", sl);
    if (smooth && isl>nt) err("sl=%g too long for trace", sl);
    if (nt!=dtr.ns) 
        err("stdin and %s have different ns (%d vs %d)", dfile, nt, dtr.ns);
    if (nt!=wtr.ns) 
        err("stdin and %s have different ns (%d vs %d)", wfile, nt, wtr.ns);
    
    /* loop over traces */
    icomp=0;
    itr=1;
    do {
       for (i=0;i<nt;i++) {
            dtr.data[i]=pow(fabs(dtr.data[i]),dpow);
        }
        if (smooth) do_smooth(dtr.data,nt,isl);
        if (!icomp) {
            for (i=0;i<nt;i++) {
                wtr.data[i]=pow(wtr.data[i],wpow);
            }
            if (smooth) do_smooth(wtr.data,nt,isl);
        }
        
        for (i=0;i<nt;i++) {
            tr.data[i]*=dtr.data[i]*wtr.data[i];
        }

        puttr(&tr);
      
        icomp++;
        if (icomp==3) {
            icomp=0;
            if (!fgettr(wfp,&wtr)) break;
        }
        itr++;
    } while(gettr(&tr) && fgettr(dfp,&dtr));
    
    if (verbose) warn("processed %d traces",itr);
    return(CWP_Exit());           
}


/* smooth data or filter operator */

void do_smooth(float *data, int nt, int isl)
{
    int it,jt;
    float *tmpdata, sval;
    
    tmpdata=ealloc1float(nt);

    for (it=0;it<nt;it++) {
          sval=0.0;
          if ( (it >= isl/2) && (it < nt-isl/2) ) {
            for (jt=it-isl/2;jt<it+isl/2;jt++) {
                  sval += data[jt];
            }
            tmpdata[it] = sval / (float) isl;
      }
      else {
            tmpdata[it] = 0.0;
      }
    }
    for (it=0;it<nt;it++) {
          data[it] = tmpdata[it];
    }
    free1float(tmpdata);
}

/* END OF FILE */
