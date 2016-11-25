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
#ifndef _ErsSeis
#define _ErsSeis


/* Next lines used to undefine VMS PI.
 * Source files use PlotImage *(PI) in many places and this
 * causes a conflict with the vms compiler since
 * it defines PI as 3.1415
 */

#ifdef VMS
#ifdef PI
#undef PI
#endif
#endif

#include <image.h>

/* define symbols for CPS header words */
#define SEQHDR    1
#define XGRIDHDR  7
#define YGRIDHDR  8
#define XBASEHDR 17
#define YBASEHDR 18
#define XANNOHDR 37
#define YANNOHDR 38
#define TIMEHDR  -2
#define DPTHHDR  -3

/*************************************************
 * Function prototypes defined in ErsSeis.c    ***
 ************************************************/
#ifdef __cplusplus
extern "C" {                 // for C++
#endif
Bool   Erswcdc(Widget , float *, float *, float *, float *);
Bool   Ersdcwc(Widget , float *, float *, float *, float *);
Bool   ErsPointToKeyTime(Widget, int, int, float *, float *, float *,
         long *, float *, float *);
Bool   ErsKeyTimeToPoint(Widget , float , float , float , float, int *,
         int *);
Bool   ErsPKeyTimeToPoint(Widget , float , float , int *, int *);
Bool   ErsTraceTimeToPoint(Widget , long , float , int *,int *);
Bool   ErsTraceToKey(Widget ,long,float *,float *,float *);
Bool   ErsPKeyToTrace(Widget ,float ,long ,long *);
struct PlotImage *ErsGetPlotInf(Widget );
Bool   ErsSetPlotInf(Widget W, caddr_t);
Bool   ErsSeismicSetCoordInf(Widget );
void   ErsSeismicLandRTrace(Widget , long *, long *,
         float *, float *, Bool *);
void   ErsSeismicLandRPKey(Widget , float *, float *,
         float *, float *, long, Bool *);
void   ErsSeismicGetLimits(Widget, int *, float *, float *,
         float *, float *, float *, float *,float *, float *,
         float *, float *);
void   ErsSeismicGetSampleRate(Widget, float *);
void   ErsSeismicGethdDat(Widget , float **, int *, int *);
void   ErsSeismicRtoL(Widget, Bool *);
int    ErsSeismicGetDataStat(Widget);
void   ErsSeismicTimeToSample(Widget , float *, int *);
Bool   ErsSeismicTellSelection(Widget , long , long , long *,long *,long *);
Bool   ErsSeismicTellSelectionP(Widget , float , float ,int,  long *,long *);
float *ErsSeismicGetSamples(Widget , void *,
          float **, int *, int *, float *);
float *ErsSeismicGetTrTiWin(Widget ,long *,long *,float *,float *,
          float **, int *, int *, float *);
float *ErsSeismicGetData(Widget, long, long, float *, float *,int *,int *);
void   ErsSeismicDraw(Widget ,int ,int ,int ,int , Bool );
#ifdef __cplusplus
}                   // for C++
#endif

#endif
