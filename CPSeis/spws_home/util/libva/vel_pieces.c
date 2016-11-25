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


/*------------------------ vel_pieces.c --------------------------------*/

/*----------------------- header files ---------------------------------*/

#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stddef.h>
#include <Xm/MainW.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include "wproc.h"
#include "cprim.h"
#include "named_constants.h"
#include "trslib.h"
#include "vel_data.h"
#include "vel_bridge.h"

/*
#define NMAX 200  
*/


/*--------- routine to draw range of hyperbola -------------------------*/

static void draw_hyperbola_range(Widget w, GC gc, float h[], long nwh,
           long trace1, long number, float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float tp, float vp)
{
  long *x, *y;
  int i, i2, j;
  float tp2, vp2;

/*
    printf("tp=%f  vp=%f  trace1=%d number=%d\n",tp,vp,trace1,number);
*/
  x = (long *)malloc(number * sizeof(long));
  y = (long *)malloc(number * sizeof(long));
  tp2 = tp * tp;
  vp2 = vp * vp;  if(vp2 < 100.) vp2 = 100.;

  for(i = 0; i < number; i++)
       {
       i2 = i + trace1;
       j = nwh * i2 + 5;
       y[i] = yzero + 0.5 + (sqrt(tp2 + h[j] * h[j] / vp2) - tmin) / yperpix;
       x[i] = xzero + 0.5 + (i2+1) * trace_width;
       }
  number = reduce_segments(x, y, number);
  draw_segments(w, gc, x, y, number, x1, y1, x2, y2);
  free(x);
  free(y);
}





/*------------------- get trace range ----------------------------------*/
 /* returns trace1 and trace2 calculated from x1 and x2 */
 /* trace1 and trace2 are not necessarily adjusted to be within range */
 /* trace1 and trace2 are not necessarily in the correct order */

static void get_trace_range(int x1, int x2,
           long ntot, long trace_width, long xzero,
           long *trace1, long *trace2)
{
  if(x1 || x2)
       {
       *trace1 = (float)(x1 - xzero) / (float)trace_width - 1.5;
       *trace2 = (float)(x2 - xzero) / (float)trace_width + 0.5;
       }
  else
       {
       *trace1 = 0;
       *trace2 = ntot-1;
       }
} 



/*------------------- adjust index range -------------------------------*/
        /* returns adjusted values for first and last index */
        /* also returns number of indices */
        /* first and last will be between 0 and ntot-1 */
        /* first will be <= last */
        /* returns number = 0 if there are no indices within range */

static long adjust_index_range(long ntot, long *first, long *last )
{
  if(*first <  0    && *last  <  0   ) return 0;
  if(*first >= ntot && *last  >= ntot) return 0;
  if(*first < 0) *first = 0;  if(*first > ntot-1) *first = ntot-1;
  if(*last  < 0) *last  = 0;  if(*last  > ntot-1) *last  = ntot-1;
  if(*last  < *first)
  {
    *first = *first ^ *last ;
    *last  = *first ^ *last ;
    *first = *first ^ *last ;
  }
  return (*last  - *first + 1);
}




/*--------- routine to draw entire hyperbola for a given pick ----------*/
     /* the points are the corners of the exposed area */
     /* zero-offset time and stacking velocity are given  */
  /*
    Widget w         = widget to draw on.
    GC gc            = graphics context.
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float tmin, tmax = minimum and maximum times to plot.
    long trace_width = width of trace in pixels.
    double yperpix   = time increment per pixel.
    long xzero       = x pixel location of zero-th trace in the image.
    long yzero       = y pixel location of time tmin.
    int x1, y1       = upper left corner of exposed area.
    int x2, y2       = lower right corner of exposed area.
    float tp         = zero-offset time.
    float vp         = stacking velocity.
        (if x1 = y1 = x2 = y2 =    0, the entire area is re-drawn)
        (if x1 = y1 = x2 = y2 = -999, nothing is done)
  */

void draw_hyperbola_full(Widget w, GC gc, float h[], long nwh, long ntot,
           float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float tp, float vp)
{
  long trace1, trace2, number;

/*
  printf("am in draw_hyperbola_full\n");
*/
  if(x1 == -999 && y1 == -999 && x2 == -999 && y2 == -999) return;
  get_trace_range(x1, x2, ntot, trace_width, xzero, &trace1, &trace2);
  number = adjust_index_range(ntot, &trace1, &trace2);
  if(number < 1) return;
  draw_hyperbola_range(w, gc, h, nwh, trace1, number, tmin, tmax,
           trace_width, yperpix, xzero, yzero, x1, y1, x2, y2, tp, vp);
}





/*------------- get one point on hyperbola -----------------------------*/
     /* returns exact offset and time, and nearest trace */

static void get_point(float h[], long nwh, long ntot, float tmin,
           long trace_width, double yperpix, long xzero, long yzero,
           int x, int y, long *trace, float *off, float *time)
{
  float exact, offa, offb;
  long trunka, trunkb;

  *time  = tmin + (float)(y - yzero) * yperpix;
  exact = (float)(x - xzero) / (float)trace_width - 1.0;
  *trace = exact + 0.5;
  trunka = exact;
  trunkb = trunka + 1;
  if(trunka < 0) trunka = 0;  if(trunka > ntot-1) trunka = ntot-1;
  if(trunkb < 0) trunkb = 0;  if(trunkb > ntot-1) trunkb = ntot-1;
  if(*trace < 0) *trace = 0;  if(*trace > ntot-1) *trace = ntot-1;
  offa = h[nwh * trunka + 5];
  offb = h[nwh * trunkb + 5];
  offa = AbsoluteValue(offa);
  offb = AbsoluteValue(offb);
  if(trunka == trunkb) *off = offa;
  else  *off = offa + (exact - trunka) * (offb - offa) / (trunkb - trunka);
/*
  printf("\ny= %d   *time= %f\n", y, *time);
  printf("x= %d   exact= %f   *trace= %d   *off= %f\n", x, exact, *trace, *off);
  printf("trunka= %d   offa= %f\n", trunka, offa);
  printf("trunkb= %d   offb= %f\n", trunkb, offb);
*/
}





/*--------- routine to draw portion of hyperbola between points --------*/
     /* the points are the two buttonpress/motion/release locations */
     /* zero-offset time and stacking velocity are returned  */
     /* hyperbola and tp and vp are constrained to vmin,vmax,0,tmax */
  /*
    Widget w         = widget to draw on.
    GC gc            = graphics context.
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float vmin, vmax = minimum and maximum allowed velocity.
    float tmin, tmax = minimum and maximum times to plot.
    long trace_width = width of trace in pixels.
    double yperpix   = time increment per pixel.
    long xzero       = x pixel location of zero-th trace in the image.
    long yzero       = y pixel location of time tmin.
    int x1, y1       = pixel location of button press.
    int x2, y2       = pixel location of button motion or release.
    float *tp        = zero-offset time (returned).
    float *vp        = stacking velocity (returned).
  */

void draw_hyperbola_part(Widget w, GC gc, float h[], long nwh, long ntot,
           float vmin, float vmax, float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float *tp, float *vp)
{
  float off1, off2, time1, time2, tp2, vp2;
  long  trace1, trace2, number;

/*
  printf("am in draw_hyperbola_part\n");
  printf("\nvmin vmax = %f %f   tmin tmax = %f %f\n", vmin,vmax,tmin,tmax);
*/
  get_point(h, nwh, ntot, tmin, trace_width, yperpix, xzero, yzero,
                                         x1, y1, &trace1, &off1, &time1);
  get_point(h, nwh, ntot, tmin, trace_width, yperpix, xzero, yzero,
                                         x2, y2, &trace2, &off2, &time2);
  if(time1 == time2)
       {
       *vp = vmax;
       }
  else
       {
       vp2 = (off1 * off1 - off2 * off2) / (time1 * time1 - time2 * time2);
       if(vp2 > 0.)
            {
            *vp = sqrt(vp2);
            if(*vp < vmin) *vp = vmin;
            if(*vp > vmax) *vp = vmax;
            }
       else
            {
            *vp = vmin;
            }
       }
  tp2 = time1 * time1 - (off1 * off1) / (*vp * *vp);
  if(tp2 > 0.)
       {
       *tp = sqrt(tp2);
       if(*tp < 0   ) *tp = 0   ;
       if(*tp > tmax) *tp = tmax;
       }
  else
       {
       *tp = 0   ;
       }
  number = adjust_index_range(ntot, &trace1, &trace2);
  if(number < 1) return;
  draw_hyperbola_range(w, gc, h, nwh, trace1, number, tmin, tmax,
           trace_width, yperpix, xzero, yzero, 0, 0, 0, 0, *tp, *vp);
}

  



/*--------- get bounding rectangle for a hyperbola ---------------------*/
     /* the returned points are the corners of the rectangle */
     /* zero-offset time and stacking velocity are given  */
     /* returns all -999 if time and/or velocity is invalid */
  /*
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float tmin       = minimum time to plot.
    long trace_width = width of trace in pixels.
    double yperpix   = time increment per pixel.
    long xzero       = x pixel location of zero-th trace in the image.
    long yzero       = y pixel location of time tmin.
    int *x1, *y1     = upper left corner of rectangle (returned).
    int *x2, *y2     = lower right corner of rectangle (returned).
    float tp         = zero-offset time.
    float vp         = stacking velocity.
    float tq         = zero-offset time for a second pick.
    float vq         = stacking velocity for a second pick.
  */

void get_bounding_rect(float h[], long nwh, long ntot, float tmin,
           long trace_width, double yperpix, long xzero, long yzero,
           int *x1, int *y1, int *x2, int *y2,
           float tp, float vp, float tq, float vq)
{
  int i, j;
/*
  float fnil, tp2, vp2, x, y;
*/
  float tp2, vp2, x, y;

  *x1 = *y1 = *x2 = *y2 = -999;
/*
  get_fnil_(&fnil);
*/

  if(tp >= 0.0 && tp != FNIL && vp > 0.0 && vp != FNIL)
       {
       tp2 = tp * tp;
       vp2 = vp * vp;  if(vp2 < 100.) vp2 = 100.;
       for(i = 0; i < ntot; i++)
            {
            j = nwh * i + 5;
            y = yzero + 0.5 + (sqrt(tp2 + h[j] * h[j] / vp2) - tmin) / yperpix;
            x = xzero + 0.5 + (i+1) * trace_width;
            if(*x1 == -999 || x < *x1) *x1 = x;
            if(*x2 == -999 || x > *x2) *x2 = x;
            if(*y1 == -999 || y < *y1) *y1 = y;
            if(*y2 == -999 || y > *y2) *y2 = y;
            }
       }
  if(tq == tp && vq == vp) return;
  if(tq >= 0.0 && tq != FNIL && vq > 0.0 && vq != FNIL)
       {
       tp2 = tq * tq;
       vp2 = vq * vq;  if(vp2 < 100.) vp2 = 100.;
       for(i = 0; i < ntot; i++)
            {
            j = nwh * i + 5;
            y = yzero + 0.5 + (sqrt(tp2 + h[j] * h[j] / vp2) - tmin) / yperpix;
            x = xzero + 0.5 + (i+1) * trace_width;
            if(*x1 == -999 || x < *x1) *x1 = x;
            if(*x2 == -999 || x > *x2) *x2 = x;
            if(*y1 == -999 || y < *y1) *y1 = y;
            if(*y2 == -999 || y > *y2) *y2 = y;
            }
       }
}

  
  
/*---------- routine to convert velocity picks into a byte array -------*/
    /* not being used - the following float routine is used instead */
      /* vmin is mapped to value 1         */
      /* vmax is mapped to value 255       */
      /* tmin is mapped to index [0]       */
      /* tmax is mapped to index [nbins-1] */
 /* fills byte array with ones if there are no picks, or some picks are nil */

void vel_to_byte(float *vpoint, float *tpoint, long n, 
               float vmin, float vmax, float tmin, float tmax,
               long nbins, unsigned char *byte_array)
{
  int i, n2 = (int)n;
  float factor=0., dt=0., v=0., xterp;
/*
  float fnil, factor=0., dt=0., v=0., xterp;
*/

/*
  get_fnil_(&fnil);
*/
  if (nbins > 1) dt = (tmax - tmin)/(nbins - 1);
  if (vmax > vmin) factor = 254./(vmax - vmin);

  for(i=0; i<n; i++)
       {
       if(tpoint[i] == FNIL || vpoint[i] == FNIL) n = 0;
       }
  for(i=0; i<nbins; i++)
       {
       xterp = tmin + i * dt;
       if (n > 0) v = terp1_(&xterp, tpoint, &n2, vpoint);
       if(v < vmin) v = vmin;
       if(v > vmax) v = vmax;
       byte_array[i] = (unsigned char)(1. + factor * (v - vmin));
       }
}


/*---------- convert velocities into an xbin timeslice array -----------*/
      /* ymin is mapped to index [0]       */
      /* ymax is mapped to index [nbins-1] */
/* sets velocity to zero if velfun has no picks or some nil picks */

void xbin_to_float(VdStruct *vd, long veltype, float xbin, float time,
               float ymin, float ymax,
               long nbins, float *float_array, int vint_grade)
{
  float y[NFUNMAX], v[NFUNMAX], dy = 0.0, vel = 0.0;
  long list[NFUNMAX], n, i;
/*
  float xbinmin, xbinmax, ybinmin, ybinmax;
*/

  get_velfun_list(vd, False, vd->fnil, vd->fnil, xbin,   list, &n);
/*
  get_bin_limits(vd, &xbinmin, &xbinmax, &ybinmin, &ybinmax);
  get_velfun_list(vd, False, ybinmin, ybinmax, xbin,   list, &n);
  get_velfun_list(vd, False, -1.e30, 1.e30, xbin,   list, &n);
  get_velfun_list(vd, True, -1.e30, 1.e30, xbin,   list, &n);
*/
  for(i = 0; i < n; i++)
       {
       y[i] = vd->ybin[ list[i] - 1 ];
       v[i] = get_velocity(vd, veltype, time, list[i], vint_grade);
       }
  vel_to_float(v, y, n, ymin, ymax, nbins, float_array, 0, 0);
}




/*---------- routine to convert velocity picks into a float array ------*/
      /* tmin is mapped to index [0]       */
      /* tmax is mapped to index [nbins-1] */
 /* fills float array with zeros if there are no picks, or some picks are nil */
 /* if veltype is 7 or 8 or 9, and vint_grade is zero,
                         does special interval velocity interpolation. */

void vel_to_float(float *vpoint, float *tpoint, long n, 
               float tmin, float tmax,
               long nbins, float *float_array, long veltype, int vint_grade)
{
  int i, n2 = (int)n;
/*
  float fnil, dt=0., v=0., xterp;
*/
  float dt=0., v=0., xterp;

/*
  get_fnil_(&fnil);
*/
  if (nbins > 1) dt = (tmax - tmin)/(nbins - 1);

  for(i=0; i<n; i++)
       {
       if(tpoint[i] == FNIL || vpoint[i] == FNIL) n = 0;
       }
  for(i=0; i<nbins; i++)
       {
       xterp = tmin + i * dt;
       if(n > 0 && vint_grade == 0 && veltype >= 7 && veltype <= 9)
           {
           int j;
           v = vpoint[0];
           for(j = 0; j < n; j++)
               {
               v = vpoint[j];
               if(tpoint[j] >= xterp) break;
               }
           }
       else if (n > 0)
           {
           v = terp1_(&xterp, tpoint, &n2, vpoint);
           }
       float_array[i] = v;
       }
}




/*--------- routine to draw doppler mute line --------------------------*/
  /*
    Widget w         = widget to draw on.
    GC gc            = graphics context.
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float tmin, tmax = minimum and maximum times to plot.
    long trace_width = width of trace in pixels.
    double yperpix   = time increment per pixel.
    long xzero       = x pixel location of zero-th trace in the image.
    long yzero       = y pixel location of time tmin.
    int x1, y1       = upper left corner of exposed area.
    int x2, y2       = lower right corner of exposed area.
    float tp[]       = array of zero-offset times.
    float vp[]       = array of stacking velocities.
    long  np         = number of picks in velocity function.
    float dopler     = doppler mute parameter.
        (if x1 = y1 = x2 = y2 =    0, the entire area is re-drawn)
        (if x1 = y1 = x2 = y2 = -999, nothing is done)
  */

#define RESOLUTION 0.016
#define MAXNUMBER 20

void draw_doppler_mute(Widget w, GC gc, float h[], long nwh, long ntot,
           float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2,
           float tp[], float vp[], long np, float dopler)
{
  long step = 1 + (ntot - 1) / (MAXNUMBER - 1);
  int i, j, redo;
  float dt = RESOLUTION, tmute;
  static float tpkeep[NMAX], vpkeep[NMAX], offkeep[200];
  static float doplerkeep = 0.0, tminkeep = 0.0, tmaxkeep = 0.0;
  static long npkeep = 0, nwhkeep = 0, ntotkeep = 0;
  static long number = 0, x[MAXNUMBER], y[MAXNUMBER];

/*
  printf("am in draw_doppler_mute\n");
*/
  if(x1 == -999 && y1 == -999 && x2 == -999 && y2 == -999) return;
  if(ntot <= 0 || nwh < 6) return;

/*--------find out whether need to redo calculations----------*/
  redo = dopler != doplerkeep || np != npkeep || nwh != nwhkeep ||
          ntot != ntotkeep || tmin != tminkeep || tmax != tmaxkeep ||
          number == 0;
  if (!redo)
       {
       for(i = 0; i < npkeep && !redo; i++)
            {
            if(tp[i] != tpkeep[i] || vp[i] != vpkeep[i]) redo = 1;
            }
       }
  if (!redo)
       {
       for(i = 0; i < ntotkeep && !redo; i++)
            {
            j = nwh * i + 5;
            if(h[j] != offkeep[i]) redo = 1;
            }
       }

/*--------redisplay old line segments-------------------------*/
  if (!redo)
       {
/*
       printf("redisplaying %d old line segments\n", number);
*/
       draw_segments(w, gc, x, y, number, x1, y1, x2, y2);
       return;
       }

/*--------save information for testing later------------------*/
  doplerkeep = dopler;
  npkeep = np;
  nwhkeep = nwh;
  ntotkeep = ntot; if(ntotkeep > 200) ntotkeep = 200;
  tminkeep = tmin;
  tmaxkeep = tmax;
  for(i = 0; i < npkeep; i++)
       {
       tpkeep[i] = tp[i];
       vpkeep[i] = vp[i];
       }
  for(i = 0; i < ntotkeep; i++)
       {
       j = nwh * i + 5;
       offkeep[i] = h[j];
       }

/*----------do the work-------------*/
  number = 0;
  for(i = 0; i < ntot; i++)
       {
       if(i == step * (i / step) || i == ntot - 1)
          {
          j = nwh * i + 5;
          get_doppler_mute(&tmax, &dt, &dopler, &h[j], tp, vp, &np, &tmute);
          if(number < MAXNUMBER) number++;
          x[number - 1] = xzero + 0.5 + (i+1) * trace_width;
          y[number - 1] = yzero + 0.5 + (tmute - tmin) / yperpix;
          }
       }
  number = reduce_segments(x, y, number);
/*
  printf("displaying %d new line segments\n", number);
*/
  draw_segments(w, gc, x, y, number, x1, y1, x2, y2);
}



            /* new stuff follows for non-hyperbolic nmo */
            /* new stuff follows for non-hyperbolic nmo */
            /* new stuff follows for non-hyperbolic nmo */
            /* new stuff follows for non-hyperbolic nmo */
            /* new stuff follows for non-hyperbolic nmo */
            /* new stuff follows for non-hyperbolic nmo */


/*--------- routine to draw range of non-hyperbola -------------------------*/

static void draw_nonhyperbola_range(float nhosign, float nhoexp,
           Widget w, GC gc, float h[], long nwh,
           long trace1, long number, float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float tp, float vp)
{
  long *x, *y;
  int i, i2, j;
  float tp2, vp2, arg, offset, nhoexp2;

  nhoexp2 = ConstrainValue(nhoexp, 0.1, 8.0);
  x = (long *)malloc(number * sizeof(long));
  y = (long *)malloc(number * sizeof(long));
  tp2 = tp * tp;
/*
  vp2 = vp * vp;
*/
  vp2 = pow(vp, nhoexp2);
  if(vp2 < 100.) vp2 = 100.;

  for(i = 0; i < number; i++)
       {
       i2 = i + trace1;
       j = nwh * i2 + 5;
       offset = AbsoluteValue(h[j]);
       if(offset < 1.0) offset = 1.0;
       arg = tp2 + nhosign * pow(offset, nhoexp2) / vp2; 
       if(arg < 0.0) arg = 0.0;
       y[i] = yzero + 0.5 + (sqrt(arg) - tmin) / yperpix;
/*
       y[i] = yzero + 0.5 + (sqrt(tp2 + h[j] * h[j] / vp2) - tmin) / yperpix;
*/
       x[i] = xzero + 0.5 + (i2+1) * trace_width;
       }
  number = reduce_segments(x, y, number);
  draw_segments(w, gc, x, y, number, x1, y1, x2, y2);
  free(x);
  free(y);
}



/*--------- routine to draw entire non-hyperbola for a given pick ----------*/
     /* the points are the corners of the exposed area */
     /* zero-offset time and stacking velocity are given  */
  /*
    float nhosign    = 1.0 for normal NMO; typically -1.0 for non-hyp NMO.
    float nhoexp     = 2.0 for normal NMO; typically  4.0 for non-hyp NMO.
    Widget w         = widget to draw on.
    GC gc            = graphics context.
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float tmin, tmax = minimum and maximum times to plot.
    long trace_width = width of trace in pixels.
    double yperpix   = time increment per pixel.
    long xzero       = x pixel location of zero-th trace in the image.
    long yzero       = y pixel location of time tmin.
    int x1, y1       = upper left corner of exposed area.
    int x2, y2       = lower right corner of exposed area.
    float tp         = zero-offset time.
    float vp         = stacking velocity.
        (if x1 = y1 = x2 = y2 =    0, the entire area is re-drawn)
        (if x1 = y1 = x2 = y2 = -999, nothing is done)
  */

void draw_nonhyperbola_full(float nhosign, float nhoexp,
           Widget w, GC gc, float h[], long nwh, long ntot,
           float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float tp, float vp)
{
  long trace1, trace2, number;

  get_trace_range(x1, x2, ntot, trace_width, xzero, &trace1, &trace2);
  number = adjust_index_range(ntot, &trace1, &trace2);
  if(number < 1) return;
  draw_nonhyperbola_range(nhosign, nhoexp,
           w, gc, h, nwh, trace1, number, tmin, tmax,
           trace_width, yperpix, xzero, yzero, x1, y1, x2, y2, tp, vp);
}




/*--------- routine to draw portion of non-hyperbola between points --------*/
     /* the points are the two buttonpress/motion/release locations */
     /* zero-offset time and stacking velocity are returned  */
     /* hyperbola and tp and vp are constrained to vmin,vmax,0,tmax */
  /*
    float nhosign    = 1.0 for normal NMO; typically -1.0 for non-hyp NMO.
    float nhoexp     = 2.0 for normal NMO; typically  4.0 for non-hyp NMO.
    Widget w         = widget to draw on.
    GC gc            = graphics context.
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float vmin, vmax = minimum and maximum allowed velocity.
    float tmin, tmax = minimum and maximum times to plot.
    long trace_width = width of trace in pixels.
    double yperpix   = time increment per pixel.
    long xzero       = x pixel location of zero-th trace in the image.
    long yzero       = y pixel location of time tmin.
    int x1, y1       = pixel location of button press.
    int x2, y2       = pixel location of button motion or release.
    float *tp        = zero-offset time (returned).
    float *vp        = stacking velocity (returned).
  */

void draw_nonhyperbola_part(float nhosign, float nhoexp,
           Widget w, GC gc, float h[], long nwh, long ntot,
           float vmin, float vmax, float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float *tp, float *vp)
{
  float off1, off2, time1, time2, tp2, vp2, nhoexp2, off1e, off2e;
  long  trace1, trace2, number;

  nhoexp2 = ConstrainValue(nhoexp, 0.1, 8.0);
  get_point(h, nwh, ntot, tmin, trace_width, yperpix, xzero, yzero,
                                         x1, y1, &trace1, &off1, &time1);
  get_point(h, nwh, ntot, tmin, trace_width, yperpix, xzero, yzero,
                                         x2, y2, &trace2, &off2, &time2);
  off1e = 0.0;
  off2e = 0.0;
  if(off1 > 0.0) off1e = pow(off1, nhoexp2);
  if(off2 > 0.0) off2e = pow(off2, nhoexp2);
  if(time1 == time2)
       {
       *vp = vmax;
       }
  else
       {
/*
       vp2 = (off1 * off1 - off2 * off2) / (time1 * time1 - time2 * time2);
*/
       vp2 = nhosign * (off1e - off2e) / (time1 * time1 - time2 * time2);
       if(vp2 > 0.)
            {
            *vp = pow(vp2, 1.0/nhoexp2);
/*
            *vp = sqrt(vp2);
*/
            if(*vp < vmin) *vp = vmin;
            if(*vp > vmax) *vp = vmax;
            }
       else
            {
            *vp = vmax;
            }
       }
/*
  tp2 = time1 * time1 - (off1 * off1) / (*vp * *vp);
*/
  vp2 = pow(*vp, nhoexp2);
  tp2 = time1 * time1 - nhosign * off1e / vp2;
  if(tp2 > 0.)
       {
       *tp = sqrt(tp2);
       if(*tp < 0   ) *tp = 0   ;
       if(*tp > tmax) *tp = tmax;
       }
  else
       {
       *tp = 0   ;
       }
  number = adjust_index_range(ntot, &trace1, &trace2);
  if(number < 1) return;
  draw_nonhyperbola_range(nhosign, nhoexp,
           w, gc, h, nwh, trace1, number, tmin, tmax,
           trace_width, yperpix, xzero, yzero, 0, 0, 0, 0, *tp, *vp);

}

  



/*--------- get bounding rectangle for a non-hyperbola ---------------------*/
     /* the returned points are the corners of the rectangle */
     /* zero-offset time and stacking velocity are given  */
     /* returns all -999 if time and/or velocity is invalid */
  /*
    float nhosign    = 1.0 for normal NMO; typically -1.0 for non-hyp NMO.
    float nhoexp     = 2.0 for normal NMO; typically  4.0 for non-hyp NMO.
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float tmin       = minimum time to plot.
    long trace_width = width of trace in pixels.
    double yperpix   = time increment per pixel.
    long xzero       = x pixel location of zero-th trace in the image.
    long yzero       = y pixel location of time tmin.
    int *x1, *y1     = upper left corner of rectangle (returned).
    int *x2, *y2     = lower right corner of rectangle (returned).
    float tp         = zero-offset time.
    float vp         = stacking velocity.
    float tq         = zero-offset time for a second pick.
    float vq         = stacking velocity for a second pick.
  */

void get_nonhyp_bounding_rect(float nhosign, float nhoexp,
                              float h[], long nwh, long ntot, float tmin,
           long trace_width, double yperpix, long xzero, long yzero,
           int *x1, int *y1, int *x2, int *y2,
           float tp, float vp, float tq, float vq)
{
  int i, j;
/*
  float fnil, tp2, vp2, x, y, offset, arg, nhoexp2;
*/
  float tp2, vp2, x, y, offset, arg, nhoexp2;

  nhoexp2 = ConstrainValue(nhoexp, 0.1, 8.0);
  *x1 = *y1 = *x2 = *y2 = -999;
/*
  get_fnil_(&fnil);
*/

  if(tp >= 0.0 && tp != FNIL && vp > 0.0 && vp != FNIL)
       {
       tp2 = tp * tp;
/*
       vp2 = vp * vp;
*/
       vp2 = pow(vp, nhoexp2);
       if(vp2 < 100.) vp2 = 100.;
       for(i = 0; i < ntot; i++)
            {
            j = nwh * i + 5;
            offset = AbsoluteValue(h[j]);
            if(offset < 1.0) offset = 1.0;
            arg = tp2 + nhosign * pow(offset, nhoexp2) / vp2;
            if(arg < 0.0) arg = 0.0;
            y = yzero + 0.5 + (sqrt(arg) - tmin) / yperpix;
/*
            y = yzero + 0.5 + (sqrt(tp2 + h[j] * h[j] / vp2) - tmin) / yperpix;
*/
            x = xzero + 0.5 + (i+1) * trace_width;
            if(*x1 == -999 || x < *x1) *x1 = x;
            if(*x2 == -999 || x > *x2) *x2 = x;
            if(*y1 == -999 || y < *y1) *y1 = y;
            if(*y2 == -999 || y > *y2) *y2 = y;
            }
       }
  if(tq == tp && vq == vp) return;
  if(tq >= 0.0 && tq != FNIL && vq > 0.0 && vq != FNIL)
       {
       tp2 = tq * tq;
/*
       vp2 = vq * vq;
*/
       vp2 = pow(vq, nhoexp2);
       if(vp2 < 100.) vp2 = 100.;
       for(i = 0; i < ntot; i++)
            {
            j = nwh * i + 5;
            offset = AbsoluteValue(h[j]);
            if(offset < 1.0) offset = 1.0;
            arg = tp2 + nhosign * pow(offset, nhoexp2) / vp2;
            if(arg < 0.0) arg = 0.0;
            y = yzero + 0.5 + (sqrt(arg) - tmin) / yperpix;
/*
            y = yzero + 0.5 + (sqrt(tp2 + h[j] * h[j] / vp2) - tmin) / yperpix;
*/
            x = xzero + 0.5 + (i+1) * trace_width;
            if(*x1 == -999 || x < *x1) *x1 = x;
            if(*x2 == -999 || x > *x2) *x2 = x;
            if(*y1 == -999 || y < *y1) *y1 = y;
            if(*y2 == -999 || y > *y2) *y2 = y;
            }
       }
}

  
  
/*--------------------------- end --------------------------------------*/
/*--------------------------- end --------------------------------------*/

