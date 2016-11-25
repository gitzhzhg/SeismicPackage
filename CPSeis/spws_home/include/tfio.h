/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#ifndef tfiodef
#define tfiodef

#include "tf3d.h"

/* This header file is used by the IO primitives that read
 * and write CPS trace files. It defines the global record
 * structure for the trace file and the control structure
 * that controls patterned IO transfers from disk */

#define MAXENT    10   /*Maximum number of GLBL structures
                      and limit on number of open files      */
#define MAXNUM   1000  /*Max. number of traces per r/w req      */
#define BUFFSIZ  132000/*IO buffer size for trace files         */
#define BYPERHC   80   /*Number of bytes per history card image */
#define GRECSIZ  1024  /*Number of bytes to reserve for global record
                      GRECSIZ can be overridden  at run time by the
                      same parameter in the global structure  */
#define GRECPOS   0    /*Byte position of start of global record .
                      Must start on a RECSIZE byte boundary   */
#define GMARKSIZ  8    /* Number of bytes to reserve for marker */
#define RECSIZE   512  /* Record size used by disk IO primitives*/
#define GNAMES    48   /* Limit on number of global names */
#define MAXSCR   5000  /* maximum size of a single io request */





struct CPSglbl {
  float dt; float t0;
  float x0; float y0;
  float dx[4]; float dn[4]; };

typedef struct CPSG_  /* c-structure that mimics the global common */
{ int   nwih;
  int   nt;
  float dt;
  float t0;
  float xorg;
  float yorg;
  float dx0[4];
  float dn0[4];
} CPSG;

struct TROThdr {
  int  ntrfil;
  char pack[8];
  char hist[8];
  int  nhd;
  int  ntd;
  struct CPSglbl CG;
  char type[8];
  int  lbuf;
  int  buf20; };

typedef struct _GlobalFmt {
  char names[GNAMES][32];
  char fmt[GNAMES][8];
  int  goff[GNAMES];
  int  nkey; } GlobalFmt;

/*
 * Cntrl structure controls which traces and samples are read
 * and whether the data is transposed and/or converted */
typedef struct Cntrl {
    int  iskp;  /*Initial No. of traces to skip.          */
    int  ndo;   /*No. of traces to read per group.        */
    int  nskp;  /*No. of traces to skip between groups.*/
    int  ntot;  /*Total No. of traces to read.            */
    int  nsamp; /*No. of samples to return                */
    int  samp1; /*1st sample to return                    */
    int  sdec;  /*Sample decimation factor                */
    int  trnsps;/*Transpose flag. 0=no transpose          */
    int  cnvrt; /*Conversion flag. 0-no conversion, 1-ch to real */
    int  axis;  /*If !=0 Indicates a cube axis. 1,2,3    */
    int  index; /* slice index(from 0) when axis is set.  */
    } IO_request;

/* function prototype (see tfdefs..h ) */
char *tf_glbl_to_asc(TF_Global *global);

#endif

