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


/*------------------------- vel_cdata.c --------------------------------*/

/*----------------------- header files ---------------------------------*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stddef.h>
#include <Xm/MainW.h>
#include <X11/StringDefs.h>
#include "vel_data.h"
#include "trslib.h"
#include "cprim.h"

  
/*
#define TINY     0.01
*/
  
  
/* finished: */

/*----------------------------------------------------------------*/

/*
    The following is in vel_boxes.h, but is included here since I do
    not want to include vel_boxes.h in this file:
*/

#ifdef VMS
#define register_active_velfun_    register_active_velfun
#endif

#ifdef _AIX
#define register_active_velfun_    register_active_velfun
#endif

#ifdef __hpux
#define register_active_velfun_    register_active_velfun
#endif

#ifdef CRAY
#define register_active_velfun_    REGISTER_ACTIVE_VELFUN
#endif

  
/*------------------- get sort direction flags -------------------------*/
/*   returns xdir =1 (increasing X), =-1 (decreasing X), =0 Unknown).  */
/*   returns ydir =1 (increasing Y), =-1 (decreasing Y), =0 Unknown).  */
/*   returns sortflag=0 (sorted) or =1 (not sorted).                   */

#define GET_SORT(ONE,TWO,DIR,SORTFLAG)                            \
      { if(DIR != 0) { if(DIR * TWO < DIR * ONE) SORTFLAG = 1; }  \
        else if(TWO > ONE) DIR =  1;                              \
        else if(TWO < ONE) DIR = -1; }

static void get_sort_flags(VdStruct *vd, int *xdir, int *ydir, int *sortflag)
{
  int i;

  *xdir = *ydir = *sortflag = 0;
  for(i=1; i<vd->nfun; i++)
     {
     if(BinNumber(vd->ybin[i-1], vd->ycenter, vd->ywidth) ==
        BinNumber(vd->ybin[i  ], vd->ycenter, vd->ywidth))
          GET_SORT(vd->xbin[i-1], vd->xbin[i], *xdir, *sortflag)
     else
          GET_SORT(vd->ybin[i-1], vd->ybin[i], *ydir, *sortflag)
     }
}



/* finished: */

/*------------------- sort velocity functions --------------------------*/

void sort_velfuns(VdStruct *vd, char *msg, long *error)
{
  FunStruct fun;
  long nfun = vd->nfun;
  long ifun = vd->ifun;
  long activepoint = 0;
  float xbin_center[NFUNMAX], ybin_center[NFUNMAX];
  long  rec[NFUNMAX], i, offset;
  size_t nread, nwrite;
  int nseek, xdir, ydir, sortflag;
  long xdirwant = vd->xdirwant, ydirwant = vd->ydirwant;
  FILE *stream;

  if(nfun == 0L)
     {
     strcpy(msg, "no functions to sort");
     *error = 0L; return;
     }
  else if(nfun == 1L)
     {
     strcpy(msg, "only one function to sort");
     *error = 0L; return;
     }
  get_sort_flags(vd, &xdir, &ydir, &sortflag);
  if(sortflag == 0)
     {
     strcpy(msg, "functions already sorted");
     if     (xdir > 0) strcat(msg, " - X increasing");
     else if(xdir < 0) strcat(msg, " - X decreasing");
     if     (ydir > 0) strcat(msg, " - Y increasing");
     else if(ydir < 0) strcat(msg, " - Y decreasing");
     if(xdir == 0) xdirwant = 0;
     if(ydir == 0) ydirwant = 0;
     *error = 0L;
     if(xdir >= 0 && xdirwant >= 0 && ydir >= 0 && ydirwant >= 0) return;
     if(xdir >= 0 && xdirwant >= 0 && ydir <= 0 && ydirwant <= 0) return;
     if(xdir <= 0 && xdirwant <= 0 && ydir >= 0 && ydirwant >= 0) return;
     if(xdir <= 0 && xdirwant <= 0 && ydir <= 0 && ydirwant <= 0) return;
     }
  stream = tmpfile();
  if(stream == NULL)
     {
     strcpy(msg, "no scratch file available for sorting");
     *error = 1L; return;
     }

  if(ifun <= nfun) activepoint = vd->point[ifun - 1];
  for(i=0; i<nfun; i++)
     {
     fun.xbin  = vd->xbin [i];
     fun.ybin  = vd->ybin [i];
     fun.n     = vd->n    [i];
     fun.point = vd->point[i];
     memcpy(fun.vfid   , vd->vfid   [i], sizeof fun.vfid   );
     memcpy(fun.select , vd->select [i], sizeof fun.select );
     memcpy(fun.type   , vd->type   [i], sizeof fun.type   );
     memcpy(fun.errmsg , vd->errmsg [i], sizeof fun.errmsg );
     memcpy(fun.raymsg , vd->raymsg [i], sizeof fun.raymsg );
     memcpy(fun.project, vd->project[i], sizeof fun.project);
     memcpy(fun.line   , vd->line   [i], sizeof fun.line   );
     memcpy(fun.rdate  , vd->rdate  [i], sizeof fun.rdate  );
     memcpy(fun.pdate  , vd->pdate  [i], sizeof fun.pdate  );
     memcpy(fun.userid , vd->userid [i], sizeof fun.userid );
     memcpy(fun.comment, vd->comment[i], sizeof fun.comment);
     xbin_center[i] =           fun.xbin                          ;
     ybin_center[i] = BinCenter(fun.ybin, vd->ycenter, vd->ywidth);
     if(xdirwant < 0) xbin_center[i] = -xbin_center[i];
     if(ydirwant < 0) ybin_center[i] = -ybin_center[i];
     if(xdirwant == 0 && xdir < 0) xbin_center[i] = -xbin_center[i];
     if(ydirwant == 0 && ydir < 0) ybin_center[i] = -ybin_center[i];
     rec [i] = i;
     nwrite = fwrite(&fun, sizeof(FunStruct), 1u, stream);
     if(nwrite != 1u)
        {
        strcpy(msg, "sort file write error");
        *error = 2L; return;
        }
     }

  triplesort_(ybin_center, xbin_center, rec, &nfun);

  for(i=0; i<nfun; i++)
     {
     offset = rec[i] * sizeof(FunStruct);
     nseek = fseek(stream, offset, SEEK_SET);
     if(nseek)
        {
        strcpy(msg, "sort file seek error");
        *error = 3L; return;
        }
     nread = fread(&fun, sizeof(FunStruct), 1u, stream);
     if(nread != 1u || feof(stream) || ferror(stream)) 
        {
        strcpy(msg, "sort file read error");
        *error = 4L; return;
        }
     vd->xbin [i] = fun.xbin ;
     vd->ybin [i] = fun.ybin ;
     vd->n    [i] = fun.n    ;
     vd->point[i] = fun.point;
     memcpy(vd->vfid   [i], fun.vfid   , sizeof fun.vfid   );
     memcpy(vd->select [i], fun.select , sizeof fun.select );
     memcpy(vd->type   [i], fun.type   , sizeof fun.type   );
     memcpy(vd->errmsg [i], fun.errmsg , sizeof fun.errmsg );
     memcpy(vd->raymsg [i], fun.raymsg , sizeof fun.raymsg );
     memcpy(vd->project[i], fun.project, sizeof fun.project);
     memcpy(vd->line   [i], fun.line   , sizeof fun.line   );
     memcpy(vd->rdate  [i], fun.rdate  , sizeof fun.rdate  );
     memcpy(vd->pdate  [i], fun.pdate  , sizeof fun.pdate  );
     memcpy(vd->userid [i], fun.userid , sizeof fun.userid );
     memcpy(vd->comment[i], fun.comment, sizeof fun.comment);
     if(activepoint == vd->point[i]) vd->ifun = i + 1;
     }

  fclose(stream);
  register_active_velfun_();
  fix_select_codes_();
  strcpy(msg, "finished sorting");
  *error = 0L;
}


/* finished: */

/*------- get velocity at given time (for a given velfun number) ------*/
                    /* which = 1 thru nfun */
 /* if veltype is 7 or 8 or 9, and vint_grade is zero,
                         does special interval velocity interpolation. */

float get_velocity(VdStruct *vd, long veltype, float time, long which,
                                                       int vint_grade)
{
  int i = which - 1, ni;
  float *vpoint, *tpoint;

  if (vd->n[i] == 0 || vd->errmsg[i][0] != ' ') return 0.0;
  PickPoints(vpoint, tpoint, vd->point[i], veltype);
  if(veltype >= 7 && veltype <= 9 && vint_grade == 0)
      {
      int j;
      int n = vd->n[i];
      float v = vpoint[0];
      for(j = 0; j < n; j++)
          {
          v = vpoint[j];
          if(tpoint[j] >= time) break;
          }
      return v;
      }
  ni = (int)vd->n[i];
  return terp1_(&time, tpoint, &ni, vpoint);
}


/* removed: */

/*---------- get xbin line of velocities at given time -----------------*/
           /* arrays y[] and v[] are already allocated */

/*
void get_xbin_vels(VdStruct *vd, long veltype,
            double xbin, double time, float *y, float *v, long *n)
{
  long xbin_number, ybin_number;
  float *vpoint, *tpoint;
  int i, ni;

  *n = 0;
  for(i = 0; i < vd->nfun; i++)
       {
       if(fabs(vd->xbin[i] - xbin) < TINY)
            {
            y[*n] = vd->ybin[i];
            if (vd->n[i] > 0 && vd->errmsg[i][0] == ' ')
                 {
                 PickPoints(vpoint, tpoint, vd->point[i], veltype);
                 ni = (int)vd->n[i];
                 v[*n] = terp1_(&time, tpoint, &ni, vpoint);
                 }
            else
                 {
                 v[*n] = 0.0;
                 }
            (*n)++;
            }
       }
}
*/



/* finished: */

/*---------- routine to get velocity limits in velocity functions ------*/

void get_vel_limits(VdStruct *vd,
            float *vmin, float *vmax, float *tmin, float *tmax, long veltype)
{
  float *vpoint, *tpoint;
  int i, j, vstart = True, tstart = True;

  *vmin = *vmax = *tmin = *tmax = 0.0;
  for(i=0; i < vd->nfun; i++)
       {
       PickPoints(vpoint, tpoint, vd->point[i], veltype);
       for(j=0; j < vd->n[i]; j++)
            {
            if(vpoint[j] != vd->fnil)
                 {
                 if(vstart) { *vmin = *vmax = vpoint[j]; vstart = False; }
                 *vmin = MinimumValue(vpoint[j], *vmin);
                 *vmax = MaximumValue(vpoint[j], *vmax);
                 }
            if(tpoint[j] != vd->fnil)
                 {
                 if(tstart) { *tmin = *tmax = tpoint[j]; tstart = False; }
                 *tmin = MinimumValue(tpoint[j], *tmin);
                 *tmax = MaximumValue(tpoint[j], *tmax);
                 }
            }
       }
}



/* finished: */

/*------------ routine to get bin limits in velocity functions ---------*/

void get_bin_limits(VdStruct *vd,
            float *xbinmin, float *xbinmax, float *ybinmin, float *ybinmax)
{
  int i;
  float x, y;

  *xbinmin = *xbinmax = *ybinmin = *ybinmax = 0.0;
  for(i=0; i < vd->nfun; i++)
       {
       x = BinCenter(vd->xbin[i], vd->xcenter, vd->xwidth);
       y = BinCenter(vd->ybin[i], vd->ycenter, vd->ywidth);
       if(i == 0) { *xbinmin = *xbinmax = x;
                   *ybinmin = *ybinmax = y; }
       *xbinmin = MinimumValue(*xbinmin, x);
       *xbinmax = MaximumValue(*xbinmax, x);
       *ybinmin = MinimumValue(*ybinmin, y);
       *ybinmax = MaximumValue(*ybinmax, y);
       }
}


/* finished: */

/*------------ routine to get list of velocity functions ---------------*/
/*   list contains functions from xbinmin thru xbinmax at given ybin.   */
/*   list is returned in ascending or descending order.                 */
/*   duplicate locations are eliminated.                                */
/*   if ybin_choice = fnil, all ybins are allowed. */

static void get_velfun_list2(float *xbin, float *ybin, long nfun,
     double xcenter, double xwidth, double ycenter, double ywidth,
     double xbinmin, double xbinmax, double ybin_choice,
     double fnil,     long *list, long *nlist)
{
  long i, j, direction, /*forward,*/ min_x, max_x, sel_y;
  long xbin_number, ybin_number /*, prev_xbin_number*/;
  float bins[NFUNMAX], *dummy;
  long ibins[NFUNMAX];

  min_x = BinNumber( MinimumValue(xbinmin,xbinmax), xcenter, xwidth );
  max_x = BinNumber( MaximumValue(xbinmin,xbinmax), xcenter, xwidth );
  sel_y = BinNumber( ybin_choice,                   ycenter, ywidth );
  *nlist = 0;
  for(i = 0; i < nfun; i++)
       {
       xbin_number = BinNumber(xbin[i], xcenter, xwidth);
       ybin_number = BinNumber(ybin[i], ycenter, ywidth);
       if(ybin_choice == fnil) ybin_number = sel_y;
       if(xbinmin == fnil) min_x = xbin_number;
       if(xbinmax == fnil) max_x = xbin_number;
       if( ybin_number == sel_y  &&
           xbin_number >= min_x  && 
           xbin_number <= max_x )
            {
            list [*nlist] = i + 1;
            bins [*nlist] = xbin_number;
            ibins[*nlist] = xbin_number;
            (*nlist)++;
            }
       }
  if(*nlist <= 1) return;
  direction = find_iarray_direction(ibins, *nlist);
  if(direction == 0)
       {
       if(bins[*nlist - 1] >= bins[0]) direction =  1;
       else                            direction = -1;
       if(direction < 0) { for(i=0; i < *nlist; i++) { bins[i] = -bins[i]; } }
       dummy = (void*)ibins;
       triplesort_(bins, dummy, list, nlist);
       if(direction < 0) { for(i=0; i < *nlist; i++) { bins[i] = -bins[i]; } }
       }
  if(direction == 1 || direction == -1)
       {
       j = 1;
       for(i = 1; i < *nlist; i++)
            {
            if(bins[i] != bins[i-1]) { list[j] = list[i]; j++; }
            }
       *nlist = j;
       }
  if( (direction > 0 && xbinmin >  xbinmax) ||
      (direction < 0 && xbinmin <= xbinmax) )
       {
       switch_iarray_direction(list, *nlist);
       }
/*
  if(direction == 2 || direction == -2) return;
  if(direction == 0)
       {
       forward = (bins[*nlist - 1] >= bins[0]);
       dummy = (void*)ibins;
       triplesort_(bins, dummy, list, nlist);
       if(!forward) switch_iarray_direction(list, *nlist);
       }
  j = 1;
  for(i = 1; i < *nlist; i++)
       {
       if(bins[i] != bins[i-1]) { list[j] = list[i]; j++; }
       }
  *nlist = j;
*/
/*
  remove_duplicate_iarray_values(list, nlist);
*/
}
            
            
void get_velfun_list(VdStruct *vd, long xlist_flag,
    double binmin, double binmax, double bin_choice,  long *list, long *nlist)
{
  if(xlist_flag) get_velfun_list2(vd->xbin, vd->ybin, vd->nfun,
                         vd->xcenter, vd->xwidth, vd->ycenter, vd->ywidth,
                         binmin, binmax, bin_choice, vd->fnil,   list, nlist);
  else           get_velfun_list2(vd->ybin, vd->xbin, vd->nfun,
                         vd->ycenter, vd->ywidth, vd->xcenter, vd->xwidth,
                         binmin, binmax, bin_choice, vd->fnil,   list, nlist);
}



/*---old version follows---*/

/*
void get_velfun_list(VdStruct *vd,
     double xbinmin, double xbinmax, double ybin,    long *list, long *nlist)
{
  int i;
  float tolerance = 0.001;
  double xbinmin2, xbinmax2;

  if(xbinmin <= xbinmax) { xbinmin2 = xbinmin; xbinmax2 = xbinmax; }
  else                   { xbinmin2 = xbinmax; xbinmax2 = xbinmin; }
  *nlist = 0;
  for(i=0; i < vd->nfun; i++)
       {
       if((fabs(vd->ybin[i] - ybin)    < tolerance) &&
               (vd->xbin[i] > xbinmin2 - tolerance  && 
                vd->xbin[i] < xbinmax2 + tolerance))
            {
            if(i == 0 || 
                  fabs(vd->xbin[i] - vd->xbin[i-1]) > tolerance)
                 {
                 if(list) list[*nlist] = i + 1;
                 (*nlist)++;
                 }
            }
       }
}
*/
            

/* finished: */

/*------------ find the previous or next x location --------------------*/
/*   given arrays x[n] and y[n], and current location i (1 thru n),
         returns next x location at same y (1 thru n),
         or zero if no such function is found. */
/*  if x is NULL, that array is not used (x and xtol are irrelevant).  */
/*  if y is NULL, that array is not used (y and ytol are irrelevant).  */
/* arrays x and y can be in any order. */
/* direction > 0 returns next x location. */
/* direction < 0 returns previous x location. */
/* direction == 0 returns current location i. */
/* the roles of x and y can be reversed by reversing the arguments. */

int find_nearby_match(long i, long direction, double ycenter, double ywidth,
           float *x, float *y, long n)
{
  long j, which = -1, ybin_number = 0, y_number = 0;
  float xbin, distance = 0.0, xdist;   /*distance not used before set below*/

  if(i < 1 || i > n) return 0;
  if(direction == 0) return i;
  if(!x) return 0;
  xbin = x[i - 1];
  if(y) ybin_number = BinNumber(y[i - 1], ycenter, ywidth);
  for(j=0; j<n; j++)
       {
       if(y) y_number = BinNumber(y[j], ycenter, ywidth);
       if(y_number == ybin_number)
            {
            if(direction > 0) xdist = x[j] - xbin;
            else              xdist = xbin - x[j];
            if(xdist > 0.0)
                 {
                 if(which < 0 || xdist < distance) 
                      {
                      distance = xdist;  which = j;
                      }
                 }
            }
       }
  return which+1;
}


/* finished: */

/*------------ routine to find the nearest matching location -----------*/
/*   given arrays x[n] and y[n], and desired values xbin and ybin,
         returns nearest matching location (1 thru n) if one or more
         matches are found, or zero if no match is found.
     arrays x[n] and y[n] can be any order or disorder. */
/*  if y is NULL, that array is not used (ybin,ycenter,ywidth irrelevant). */

int find_match(double xbin, double ybin,
     double xcenter, double xwidth, double ycenter, double ywidth,
           float *x, float *y, long n)
{
  double distance = 0.0, xdist;   /* distance not used before set below*/
  long  xbin_number, ybin_number = 0;
  long  x_number, y_number = 0;
  int   i, which = -1;

  if(y) ybin_number = BinNumber(ybin, ycenter, ywidth);
        xbin_number = BinNumber(xbin, xcenter, xwidth);
  for(i=0; i<n; i++)
       {
       if(y) y_number = BinNumber(y[i], ycenter, ywidth);
             x_number = BinNumber(x[i], xcenter, xwidth);
       if(x_number == xbin_number && y_number == ybin_number)
            {
            xdist = fabs(x[i] - xbin);
            if(which < 0 || xdist < distance) 
                 {
                 distance = xdist;  which = i;
                 }
            }
       }
  return which+1;
}


/* finished: */

/*------------ routine to find where to insert a matching value --------*/
/*   given arrays x[n] and y[n], and desired values xbin and ybin,
         returns location where to insert these values (1 thru n+1).
     array x[n] must be either increasing or decreasing.
     array y[n] must be either increasing or decreasing.  */
/*  xdir =1 (increasing X), =-1 (decreasing X), =0 Unknown).  */
/*  ydir =1 (increasing Y), =-1 (decreasing Y), =0 Unknown).  */
/*  sortflag=0 (sorted) or =1 (not sorted).                   */
/*  if y is NULL, that array is not used (ybin,ycenter,ywidth irrelevant).*/
/*  xflag >= 0 means to create ascending order if either is possible. */
/*  xflag <  0 means to create descending order if either is possible. */

int find_where(double xbin, double ybin, double xflag, double ycenter,
           double ywidth, float *x, float *y, long n)
{
  int i, /*which,*/ xdir = 0, ydir = 0, sortflag = 0, same_ybin = TRUE;

  if(n == 0) return 1;
/*--------get sort flags. */
  for(i=1; i<n; i++)
     {
     if(y != NULL) same_ybin = (BinNumber(y[i-1], ycenter, ywidth) ==
                                BinNumber(y[i  ], ycenter, ywidth));
     if(same_ybin)
          GET_SORT(x[i-1], x[i], xdir, sortflag)
     else
          GET_SORT(y[i-1], y[i], ydir, sortflag)
     }
  if(sortflag) return n+1;
  if(xdir == 0) { if(xflag >= 0.0) xdir = 1; else xdir = -1; }
  if(ydir == 0) ydir = 1;
/*-------find proper place to put new function. */
  for(i=0; i<n; i++)
       {
       if(y != NULL) same_ybin = (BinNumber(y[i-1], ycenter, ywidth) ==
                                  BinNumber(y[i  ], ycenter, ywidth));
       if(same_ybin)
            {
            if(xdir * x[i] > xdir * xbin) return i+1;
            }
       else if(y != NULL && ydir * y[i] > ydir * ybin) return i+1;
       }
  return n+1;
}


/*------------ routines to set and get hyperbolic parameters -----------*/

int set_order (VdStruct *vd, long order)
{
  if(order == 2)
      {
      vd->order   =  2;
      vd->nhosign =  1.0;
      vd->nhoexp  =  2.0;
      return 0;
      }
  else if(order == 4)
      {
      vd->order   =  4;
      vd->nhosign = -1.0;
      vd->nhoexp  =  4.0;
      return 0;
      }
  vd->order   =  2;
  vd->nhosign =  1.0;
  vd->nhoexp  =  2.0;
  return 1;
}

int set_nonhyp(VdStruct *vd, float nhosign, float nhoexp)
{
  if(nhosign == 1.0 && nhoexp == 2.0)
      {
      vd->order   =  2;
      vd->nhosign =  1.0;
      vd->nhoexp  =  2.0;
      return 0;
      }
  else if(nhosign == -1.0 && nhoexp == 4.0)
      {
      vd->order   =  4;
      vd->nhosign = -1.0;
      vd->nhoexp  =  4.0;
      return 0;
      }
  vd->order   =  2;
  vd->nhosign =  1.0;
  vd->nhoexp  =  2.0;
  return 1;
}

long  get_order (VdStruct *vd)
{
  return vd->order;
}

float get_nhosign   (VdStruct *vd)
{
  return vd->nhosign;
}

float get_nhoexp    (VdStruct *vd)
{
  return vd->nhoexp;
}


/*--------------------------- end --------------------------------------*/

