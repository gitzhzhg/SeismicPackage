/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUASCII: $Revision: 1.19 $ ; $Date: 2012/01/03 20:24:53 $        */

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                       ",
" SUASCII - print non zero header values and data in various formats    ",
"                                                                       ",
" suascii <stdin >ascii_file                                            ",
"                                                                       ",
" Optional parameter:                                                   ",
"    bare=0     print headers and data                                  ",
"        =1     print only data                                         ",
"        =2     print headers only                                      ",
"        =3     print data in print data in .csv format, e.g. for Excel ",
"        =4     print data as tab delimited .txt file, e.g. for GnuPlot ",
"        =5     print data as .xyz file, e.g. for plotting with GMT     ",
"                                                                       ",
"    ntr=50     maximum number of output traces (bare=3 or bare=4 only) ",
"    index=0    don't include time/depth index in ascii file (bare=4)   ",
"         =1    include time/depth index in ascii file                  ",
"                                                                       ",
"    key=       if set, name of keyword containing x-value              ",
"               in .xyz output (bare=5 only)                            ",
"    sep=       if set, string separating traces in .xyz output         ",
"               (bare=5; default is no separation)                      ",
"                                                                       ",
"    verbose=0  =1 for detailed information                             ",
"                                                                       ",
" Notes:                                                                ",
"    The programs suwind and suresamp provide trace selection and       ",
"    subsampling, respectively.                                         ",
"    With bare=0 and bare=1 traces are separated by a blank line.       ",
"                                                                       ",
"    With bare=3 a maximum of ntr traces are output in .csv format      ",
"    (\"comma-separated value\"), e.g. for import into spreadsheet      ",
"    applications like Excel.                                           ",
"                                                                       ",
"    With bare=4 a maximum of ntr traces are output in as tab delimited ",
"    columns. Use bare=4 for plotting in GnuPlot.                       ",
"                                                                       ",
"    With bare=5 traces are written as \"x y z\" triples as required    ",
"    by certain plotting programs such as the Generic Mapping Tools     ",
"    (GMT). If sep= is set, traces are separated by a line containing   ",
"    the string provided, e.g. sep=\">\" for GMT multisegment files.    ",
"                                                                       ",
"    \"option=\" is an acceptable alias for \"bare=\".                  ",
"                                                                       ",
" Related programs: sugethw, sudumptrace                                ",
"                                                                       ",
NULL};

/* Credits:
 *    CWP: Jack K. Cohen  c. 1989
 *    CENPET: Werner M. Heigl 2006 - bug fixes & extensions
 *    RISSC:  Nils Maercklin 2006
 *
 * Trace header field accessed: ns, dt, delrt, d1, f1, trid
 */
/**************** end self doc ***********************************/

/* function prototype (for bare=5) */
void printXYZ(segy tr, char *key);

segy tr;

int
main(int argc, char **argv)
{
    int i,k=0;         /* loop variables */
    int j;             /* trace counter */
    int bare;          /* conversion mode */
    int index;         /* flag for time/depth index */
    int verbose;       /* 0: display basic information only */
                       /* 1: display detailed information   */
    float dt;          /* sampling interval in seconds */
    float tmin;        /* time/depth of first sample in seconds */
    int nt;            /* number of samples per trace    */
    int ntr;           /* maximum number of traces for bare=3 or bare=4 */
    float **data=NULL; /* data buffer for bare=3 or bare=4 */
    char *key=NULL;    /* key from segy.h used for bare=5 */
    char *sep=NULL;    /* trace separator string for bare=5 */
    cwp_Bool seismic;  /* flag: is this seismic data? */


    /* Initialize */
    initargs(argc, argv);
    requestdoc(1);

    /* Get parameters */
    if (!getparint("bare", &bare) && \
        !getparint("option", &bare)) bare=0;
    if (!getparint("index",&index))  index=0;  /* for bare=4 */
    if (!getparint("ntr",&ntr))      ntr=50;   /* for bare=3,4 */
    if (!getparstring("key", &key))  key=NULL; /* for bare=5 */
    if (!getparstring("sep", &sep))  sep=NULL; /* for bare=5 */
    if (!getparint("verbose",&verbose)) verbose=0;

    /* Validate user input */
    if (bare<0 || bare>5) err("bare=%d must be in range 0...5", bare);
    if (ntr<1) err("ntr=%d must be positive", ntr);

    /* Get info from first trace */
    if (!gettr(&tr))  err("can't get first trace");
        seismic = ISSEISMIC(tr.trid);        
    if (!seismic){
        if (verbose) warn("input is not seismic data, trid=%d",tr.trid);
        dt = tr.d1;
        if (!dt) getparfloat("dt", &dt);
        if (!dt) err("d1 field is zero and not getparred");
        tmin = tr.f1;
        if (!tmin && verbose) warn("f1 field is zero or not set");
    }
    else {
        if (verbose) warn("input is seismic data, trid=%d",tr.trid);
        dt = ((double) tr.dt)/1000000.0;
        if (!dt) getparfloat("dt", &dt);
        if (!dt)    err("dt field is zero and not getparred");
        tmin = ((double) tr.delrt)/1000.0;  
        if (!tmin && verbose) warn("delrt field is zero or not set");
    }
    checkpars();
    nt = (int) tr.ns;    
    
    /* user info */
    if (verbose) warn("dt=%g  tmin=%g  nt=%d", dt, tmin, nt);

    /* Allocate space for data buffer (csv output, bare=3,4) */
    if (bare==3 || bare==4) data=ealloc2float(nt,ntr);

    /* Loop over traces converting to ascii */
    j=0;
        
    do {

        switch (bare) {
            
            case 0:
                /* print non-zero header values and data */
                printheader(&tr);
                for (i=0;i<nt;i++) {
                    printf("%5d ", i+1);
                    printf("%11.4e\n", tr.data[i]);
                }
                putchar('\n');
                break;
            
            case 1:
                /* print data only */
                for (i=0;i<nt;i++) {
                    printf("%15.8f\n", tr.data[i]);
                }
                putchar('\n');
                break;

            case 2:
                /* Print non-zero header values */
                printheader(&tr);
                break;
                
            case 3:
                /* Store data in memory for csv output */
                memcpy((void *)data[j],(const void *)tr.data,nt*FSIZE);
                break;
                
            case 4:
                /* Store data in memory for tab delimited output */
                memcpy((void *)data[j],(const void *)tr.data,nt*FSIZE);
                break;
                
            case 5:
                /* Print trace and eventually trace separator string */    
                printXYZ(tr, key);
                if (sep) printf("%s\n", sep);
                break;
            default:
                break;
                
        }
        /* increment trace counter and check number of traces */
        j++;
        if ((bare==3 || bare==4) && j==ntr) break;
        
    } while (gettr(&tr));
    
    /* user info; particularily useful, if bare=3 or bare=4 */
    if (verbose) {
        if (bare==3 || bare==4) {
            warn("bare=%d, j=%d traces read, maximum number is ntr=%d", \
                bare, j, ntr);
        }
        else {
            warn("bare=%d, j=%d traces read",bare,j);
        }
    }


    /* Print data in csv format (e.g. for Excel) */
    if (bare==3) {
        for ( i=0; i<tr.ns; i++ ) {
            for ( k=0; k<j-1; k++ ) {
                printf( "%g," ,data[k][i]);
            }
            printf( "%g\n" ,data[j-1][i]);
        }

        /* Free data buffer */
        free2float(data);
    }

    /* Print data in tab delimited columns */
    if (bare==4) {
        for (k=0;k<nt;k++) {
            if (index) printf("%15.8f\t",tmin+k*dt);
            if (j==1) {            
                for (i=0;i<j;i++) {
                    printf("%15.8f\n",data[i][k]);
                }
            } 
            else {
                for (i=0;i<j-1;i++) {
                    printf("%15.8f\t",data[i][k]);
                }
                printf("%15.8f\n",data[j-1][k]);
            }
        }
        /* Free data buffer */
        free2float(data);
    }

           
    return(CWP_Exit());
}

/* Function used internally */


void printXYZ(segy tr, char *key) 
/************************************************************************
printXYZ - print ASCII triples to stdout, e.g for plotting with GMT 

*************************************************************************
Input:
tr      SEG-Y trace
key     key header word from segy.h defining x

Output:
        writes space-separated ASCII triples to stdout
        (value of key defining x, time of sample, trace sample value)

Trace header fields accessed: ns, dt, delrt, d1, f1, d2, f2, key=keyword

*************************************************************************
Notes:
Output (stdout) is a space-separated ASCII table for all trace samples.
First column is the value of the specified key, second column is the time 
of the sample, and third column is the sample value. If tr.dt is set, 
tr.dt and tr.delrt are used to determine the time of the sample (seismic
data), else tr.d1 and tr.f1 are used instead. If also tr.d1=0, tr.d1=1
is assumed.
If key=NULL, tr.d2 and tr.f2 from the first trace are used to determine
the value for the first column (if tr.d2=0, tr.d2=1 is assumed).
The output is useful for certain plotting programs such as the Generic 
Mapping Tools (GMT; http://gmt.soest.hawaii.edu).
*************************************************************************
Author: Nils Maercklin, March 2006
*************************************************************************/
{
    /* Variables */
    char *type;          /* type of header key from segy.h */
    int index;           /* ... its index */
    Value val;           /* ... its value */
    float x;             /* trace coordinate (slow dimension) */
    float d1,f1;         /* time sampling and time of first sample */
    static float d2,f2;  /* trace sampling and coordinate of first trace */
    static int itr=0;    /* internal trace counter */
    register int i;      /* loop index */


    /* Use value of key */
    if (key) {
        type = hdtype(key);
        index = getindex(key);
        gethval(&tr, index, &val);
        x = vtof(type,val);
    }
    /* ...or get trace sampling (f2, d2) from first trace, if key==NULL */
    else {
        if (!itr) {
            d2 = (tr.d2) ? tr.d2 : 1.0;
            f2 = tr.f2;
        }
        /* Compute trace coordinate */
        x = ((float) (itr++)) * d2 + f2;
    }

    /* Time sampling from tr.dt and tr.delrt */
    if (ISSEISMIC(tr.trid) && tr.dt) {
        d1 = ((float)tr.dt)/1000000.0;
        f1 = ((float)tr.delrt)/1000.0;
    }
    /* ...or from tr.d1 and tr.f1 */
    else {
        d1 = (tr.d1) ? tr.d1 : 1.0;
        f1 = tr.f1;
    }

    /* Print x, time, sample value */
    for (i=0; i < (int) tr.ns; ++i) {
        printf("%g %g %g\n", x, ((float)i)*d1 + f1, tr.data[i]);
    }
}

/* end of file */
