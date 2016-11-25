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


/*----------------------- vel_cbridge.c --------------------------------*/

/*----------------------- header files ---------------------------------*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stddef.h>
#include <Xm/MainW.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include "cprim.h"
#include "inquire.h"

#include "va.h"
#include "vel_boxes.h"
#include "vel_bridge.h"
#include "wproc.h"
#include "trslib.h"

  

/*------------ give a pick to mike -------------------------------------*/
          /* called from vel_boxes.f */

/* Put these into trslib.h   MLS 10/99
#if (ultrix || sun || __sgi)
#define give_to_mike   give_to_mike_
#elif CRAY
#define give_to_mike   GIVE_TO_MIKE
#endif
*/

long give_to_mike(long *index, float *time, float *velocity)
{
  VelStruct *vel;
  get_velstruct_pointer_(&vel);
  if(*time     == vel->vd->fnil) return 0L;
  if(*velocity == vel->vd->fnil) return 0L;
  if(*velocity <= 0.0          ) return 0L;
  vel->p_rec.pick_index    = *index - 1;
  vel->p_rec.pick_time     = *time;
  vel->p_rec.pick_velocity = *velocity;
/*
      printf("index/time/velocity = %d %f %f\n",*index,
                       (double)(*time), (double)(*velocity));
*/
  return 1L;
}

  
  

#define SMALLEST 0.01
#define LARGEST  99999
#define FRACTION 0.25

/*--------------------- reduce delta -----------------------------------*/
 /* returns the minimum of delta and the minimum spacing in the array */
               /* delta must be preset upon entry */
       /* the returned delta will be <= the preset value */
         /* the array must be ascending or descending */
    /* the second array, if not NULL, forces comparisons in the
       first array only when the values in the second array are equal */
   /* two values within range SMALLEST from each other are considered equal
            and will not reduce delta */
   /* delta is unchanged unless the array has at least two values */

/*
static void reduce_delta(float *array, float *govern, long n, float *delta)
{
  int i;
  float test = 0.0, temp;

  if(array == NULL) return;
  for(i=1; i<n; i++)
       {
       if(govern != NULL) test = fabs(govern[i] - govern[i-1]);
       if(test < SMALLEST)
            {
            temp = fabs(array[i] - array[i-1]);
            if(temp >= SMALLEST && temp < *delta) *delta = temp;
            }
       }
}
*/


/*-------- find minimum x and y tolerances from three sources ----------*/
       /* the sources are semblance, cmp, and velfun arrays */
      /* each array must be in ascending or descending order */
/*
If the user-requested tolerance is outside of the range 0.01 to 99999,
  it is reset to be within this range.

If the automatic flag is false:
  - the user-requested tolerance is used.

If the automatic flag is true:
  - an intelligent tolerance is calculated.
  - an intelligent value is 0.25 times the minimum non-matching spacing
        in all the arrays (up to 3) which have at least two elements.
  - If none of the arrays have at least 2 elements, or all elements
        within each single array match, the intelligent value is 0.01. 
  - Elements are considered to match if they differ by less than 0.01.
  - The intelligent tolerance will not exceed 99999.

Here is the old method (when tolerances were in argument list):
  - a preset tolerance > 0 will not be changed.  
  - a preset tolerance < 0 will be reset to 1.e30 .  
  - a preset tolerance = 0 will be reset to an intelligent value.  
*/

/*
void find_min_tols(void)
{
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
  if(vel->vd->xtol_request < SMALLEST) vel->vd->xtol_request = SMALLEST;
  if(vel->vd->xtol_request > LARGEST ) vel->vd->xtol_request = LARGEST ;
  if(vel->vd->ytol_request < SMALLEST) vel->vd->ytol_request = SMALLEST;
  if(vel->vd->ytol_request > LARGEST ) vel->vd->ytol_request = LARGEST ;
  if(vel->vd->auto_xtol)
       {
       vel->vd->xtol = LARGEST;
       reduce_delta(vel->vd->xbin, vel->vd->ybin, vel->vd->nfun,&vel->vd->xtol);
       reduce_delta(vel->semxloc , NULL         , vel->snfun   ,&vel->vd->xtol);
       reduce_delta(vel->cmpxloc , NULL         , vel->cnfun   ,&vel->vd->xtol);
       if(vel->vd->xtol == LARGEST) vel->vd->xtol = SMALLEST;
       else                         vel->vd->xtol *= FRACTION;
       }
  else
       {
       vel->vd->xtol = vel->vd->xtol_request;
       }
  if(vel->vd->auto_ytol)
       {
       vel->vd->ytol = LARGEST;
       reduce_delta(vel->vd->ybin, NULL         , vel->vd->nfun,&vel->vd->ytol);
       if(vel->vd->ytol == LARGEST) vel->vd->ytol = SMALLEST;
       else                         vel->vd->ytol *= FRACTION;
       }
  else
       {
       vel->vd->ytol = vel->vd->ytol_request;
       }
}
*/

  
  
/*------------ find matching velfun, using find_min_tols ---------------*/

int find_xymatch(float xbin, float ybin, float *x, float *y, long n)
{
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
/*
  find_min_tols();
  return find_match(xbin, ybin, vel->vd->xtol, vel->vd->ytol, x, y, n);
*/
  return find_match(xbin, ybin, vel->vd->xcenter, vel->vd->xwidth,
                 vel->vd->ycenter, vel->vd->ywidth, x, y, n);
}

  
  
/*------------ find nearest velfun ------------------------------------*/
      /* returns function number (1 thru n) */
      /* returns zero if n == 0   */

int find_xynearest(float xbin, float ybin, float *x, float *y, long n,
                          float xperpix, float yperpix)
{
  VelStruct *vel;
  int i, imin = -1;
  float xdist, ydist, dist, distmin = 0.0;

  get_velstruct_pointer_(&vel);
  for(i = 0; i < n; i++)
       {
       xdist = fabs(x[i] - xbin) / (vel->vd->xwidth * xperpix);
       ydist = fabs(y[i] - ybin) / (vel->vd->ywidth * yperpix);
       dist = xdist * xdist + ydist * ydist;
       if(i == 0 || dist < distmin)
            { distmin = dist; imin = i; }
       }
  return (imin + 1);
}

  
/*------------ get direction of increasing x values --------------*/
      /* returns  1 if x is increasing. */
      /* returns -1 if x is decreasing. */
      /* returns  1 if direction of x cannot be determined. */
      /* the first example of x increasing or decreasing is returned. */
      /* if the data is not sorted, either 1 or -1 might be returned. */
      /* it is assumed that x changes faster than y. */

static int get_xdirection(float *x, float *y, long n)
{
  VelStruct *vel;
  int i;

  get_velstruct_pointer_(&vel);
  for(i = 1; i < n; i++)
      {
      if(BinNumber(y[i-1], vel->vd->ycenter, vel->vd->ywidth) ==
         BinNumber(y[i  ], vel->vd->ycenter, vel->vd->ywidth))
           {
           if     (x[i] > x[i-1]) return  1;
           else if(x[i] < x[i-1]) return -1;
           }
      }
  return 1;
}


  
/*------------ get direction of increasing y values --------------*/
      /* returns  1 if y is increasing. */
      /* returns -1 if y is decreasing. */
      /* returns  1 if direction of y cannot be determined. */
      /* the first example of y increasing or decreasing is returned. */
      /* if the data is not sorted, either 1 or -1 might be returned. */
      /* it is assumed that x changes faster than y. */

static int get_ydirection(float *x, float *y, long n)
{
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
  if(n <= 1) return 1;
  if(BinNumber(y[n-1], vel->vd->ycenter, vel->vd->ywidth) <
     BinNumber(y[0  ], vel->vd->ycenter, vel->vd->ywidth)) return -1;
  return 1;
}


  
/*--------- find next or previous x or y location ----------------------*/
      /* if i >= 1 and i <= n, searches from location i. */
      /* otherwise uses xbin and ybin to find i first. */


int find_xnext(long i, float xbin, float ybin, float *x, float *y, long n)
{
  VelStruct *vel;
  int direction = get_xdirection(x, y, n);

  if(i < 1 || i > n) i = find_xymatch(xbin, ybin, x, y, n);
  get_velstruct_pointer_(&vel);
  return find_nearby_match
             (i, direction, vel->vd->ycenter, vel->vd->ywidth, x, y, n);
}


int find_xprev(long i, float xbin, float ybin, float *x, float *y, long n)
{
  VelStruct *vel;
  int direction = - get_xdirection(x, y, n);

  if(i < 1 || i > n) i = find_xymatch(xbin, ybin, x, y, n);
  get_velstruct_pointer_(&vel);
  return find_nearby_match
             (i, direction, vel->vd->ycenter, vel->vd->ywidth, x, y, n);
}


int find_ynext(long i, float xbin, float ybin, float *x, float *y, long n)
{
  VelStruct *vel;
  int direction = get_ydirection(x, y, n);

  if(i < 1 || i > n) i = find_xymatch(xbin, ybin, x, y, n);
  get_velstruct_pointer_(&vel);
  return find_nearby_match
             (i, direction, vel->vd->xcenter, vel->vd->xwidth, y, x, n);
}


int find_yprev(long i, float xbin, float ybin, float *x, float *y, long n)
{
  VelStruct *vel;
  int direction = - get_ydirection(x, y, n);

  if(i < 1 || i > n) i = find_xymatch(xbin, ybin, x, y, n);
  get_velstruct_pointer_(&vel);
  return find_nearby_match
             (i, direction, vel->vd->xcenter, vel->vd->xwidth, y, x, n);
}


/*------------ find existing velfun, or create new one -----------------*/
   /*  also insert new velfun if none exists at the given coordinates */
   /*  then set this velfun to be the active one */
   /*  if velfun already exists, broadcast MESSAGE_ACTIVE */
   /*  if new velfun has been inserted, broadcast MESSAGE_NUMBER */
   /*  sender of message is SENDER_SEMB */
   /*  returns (1 thru vel->vd->nfun if matching velfun is found or
           successfully inserted */
   /*  returns (MINUS 1 thru vel->vd->nfun + 1) if an attempt to insert
           a velfun fails */

int find_velfun(float xbin, float ybin)
{
  long i, ierr;
  float xflag;
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
/*
  find_min_tols();
  i = find_match(xbin, ybin, vel->vd->xtol, vel->vd->ytol, 
                             vel->vd->xbin, vel->vd->ybin, vel->vd->nfun);
*/
  i = find_xymatch(xbin, ybin, vel->vd->xbin, vel->vd->ybin, vel->vd->nfun);
  if(i > 0)
       {
       f_activate_velfun_(&i);
       broadcast(vel, SENDER_SEMB, MESSAGE_ACTIVE, 0,0,0,0,0);
       return i;
       }
  xflag = 1.0;
  if     (vel->snfun >= 2)
                  { if(vel->semxloc[1] < vel->semxloc[0]) xflag = -1.0; }
  else if(vel->cnfun >= 2)
                  { if(vel->cmpxloc[1] < vel->cmpxloc[0]) xflag = -1.0; }
  i = find_where(xbin, ybin, xflag, vel->vd->ycenter, vel->vd->ywidth,
                           vel->vd->xbin, vel->vd->ybin, vel->vd->nfun);
/*
  i = find_where(xbin, ybin, xflag, vel->vd->ytol,
                           vel->vd->xbin, vel->vd->ybin, vel->vd->nfun);
*/
       f_insert_blank_velfun_(&i, &ierr);
       if(ierr) return -i;
       vel->vd->xbin[i-1] = xbin;
       vel->vd->ybin[i-1] = ybin;
       f_activate_velfun_(&i);
       broadcast(vel, SENDER_SEMB, MESSAGE_NUMBER, 0,0,0,0,0);
       return i;
}
  





/*------------ create, start, and stop wait cursor ---------------------*/

void create_wait_cursor(VelStruct *vel)
{
  vel->watch_cur = XCreateFontCursor(XtDisplay(vel->shell), XC_watch);
}


void start_wait_cursor_(VelStruct **vel)
{ set_cursor_on_shells((*vel)->shell, (*vel)->watch_cur); }

  
void stop_wait_cursor_(VelStruct **vel)
{ unset_cursor_on_shells((*vel)->shell); }


/*
void define_wait_cursor(VelStruct *vel, Cursor cursor)
{
  set_cursor_on_shells(vel->shell, cursor);
}


void define_wait_cursor(VelStruct *vel, Cursor cursor)
{
  define_cursor(vel->shell            , cursor);
  define_cursor(vel->vw.read_widget   , cursor);
  define_cursor(vel->vw.save_widget   , cursor);
  define_cursor(vel->vw.pick_widget   , cursor);
  define_cursor(vel->vw.fun_widget    , cursor);
  define_cursor(vel->vw.set_widget    , cursor);
  define_cursor(vel->vw.vfid_widget   , cursor);
  define_cursor(vel->vw.head_widget   , cursor);
  define_cursor(vel->vw.res_widget    , cursor);
  define_cursor(vel->vw.lat_widget    , cursor);
  define_cursor(vel->vw.lat2_widget   , cursor);
  define_cursor(vel->vw.ray_widget    , cursor);
  define_cursor(vel->vw.del_widget    , cursor);
  define_cursor(vel->vw.misc_widget   , cursor);
  define_cursor(vel->vw.offmute_widget, cursor);
  wbox_flush_buffer();
}

  
void start_wait_cursor_(VelStruct **vel)
{ define_wait_cursor(*vel, (*vel)->watch_cur); }

  
void stop_wait_cursor_(VelStruct **vel)
{ define_wait_cursor(*vel, None); }
*/

  


  
/*------------------- read velocity functions --------------------------*/
       /* called by Trey Roby from the input/display popup */
       /* we should not broadcast any message here */

int read_vels(Widget w, String filename, String msg)
{
  long ierr;
  VelStruct *vel;
  static char old_filename[200] = "xq rk bj &*%$";

  if(filename != NULL && !strcmp(filename, old_filename))
       {
       ierr = 0;
       }
  else if(filename == NULL || filename[0] == '\0' || filename[0] == ' ' ||
     strcmp(filename, "NONE") == 0 || strcmp(filename, "none") == 0)
       {
       remove_velocities_();
       ierr = 0;
       }
  else
       {
       get_velstruct_pointer_(&vel);
       start_wait_cursor_(&vel);
       define_cursor(w, vel->watch_cur);
       ierr = read_velocities_(filename, msg);
       define_cursor(w, None);
       stop_wait_cursor_(&vel);
       }
  return (int)ierr;
}




/*------------------- save velocity functions --------------------------*/
       /* called by Mike Sherrill to save auxiliary velocity file */

int save_vels(Widget w, String filename, String type2, String msg)
{
  long ierr;
  VelStruct *vel;

  if(filename == NULL || filename[0] == '\0' || filename[0] == ' ' ||
     strcmp(filename, "NONE") == 0 || strcmp(filename, "none") == 0)
       {
       ierr = 0;
       }
  else
       {
       get_velstruct_pointer_(&vel);
       start_wait_cursor_(&vel);
       define_cursor(w, vel->watch_cur);
       ierr = save_velocities_(filename, type2, msg);
       define_cursor(w, None);
       stop_wait_cursor_(&vel);
       }
  return (int)ierr;
}




/*------------------- validate velocity functions -----------------------*/
       /* called by Mike Sherrill to validate velocity file */

long validate_vels(String filename,
                          long *nhx, long *nhy, long *nfun, String info)
{
  long valid, ierr;
  VelStruct *vel;

  if(filename == NULL || filename[0] == '\0' || filename[0] == ' ' ||
     strcmp(filename, "NONE") == 0 || strcmp(filename, "none") == 0)
       {
       *nhx = 0;
       *nhy = 0;
       *nfun = 0;
       valid = INQUIRE_VALID_NO;
       }
  else
       {
       get_velstruct_pointer_(&vel);
       ierr = validate_velocities_(filename, nhx, nhy, nfun, info);
       if(ierr) valid = INQUIRE_VALID_NO;
       else     valid = INQUIRE_VALID_YES;
       }
  return valid;
}





/*------------------- sort velocity functions --------------------------*/

void fsort_functions_(VelStruct **vel2, long *error)
{
  sort_functions(*vel2, error);
}

void sort_functions(VelStruct *vel, long *error)
{
  char msg[100];

  wbox_messageline(vel->vw.fun_box, "functions being sorted...");
  sort_velfuns(vel->vd, msg, error);
  wbox_set_focus(vel->vw.fun_box, 14L, vel->vd->ifun);
  wbox_event(vel->vw.fun_box, "hello");
  wbox_messageline(vel->vw.fun_box, msg);
  adjust_arrow_sensitivity_(&vel);
  broadcast(vel, SENDER_WBOX, MESSAGE_SORT, 0,0,0,0,0);
}




/*---------- do nmo correction on byte arrays --------------------------*/
        /* uses the active velocity function for forward nmo */
        /* uses the saved velocity function for reverse nmo */
        /* returns 0 if all ok and 1 if fails */
        /* byte arrays are unchanged if fails */
  /*
    int  mode        =  >= 1 for forward nmo, <= -1 for reverse nmo.
                        == 0 for reverse nmo followed by forward nmo.
    float dop        = doppler mute (= 2 to allow stretch up to factor 2).
unsigned char bbbb[] = trace byte values for traces in the image.
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float tmin, tmax = minimum and maximum times to plot.
    float dt         = sample rate (seconds).
  */

/*
#define STEP 0.1
*/
#define STEP 0.05    /* changed 6/11/96 */

long do_nmo_correction(int mode, float dop,
              unsigned char bbbb[], float h[], long nwh,
              long ntot, float tmin, float tmax, float dt)
{
  static float times[NMAX], vnmo[NMAX], hkeep[40];
  static long nkeep, nwhkeep, ntotkeep, nmoflag = 0;
  long n, nwh2, ifun1, ierr, i, nsamp, iprint = 0;
  float dx = STEP;
  VelStruct *vel;
  VdStruct *vd;
  VpStruct *vp;

/*
        printf("am in nmo_correction in piece2\n");
*/
  if(dt <= 0.0 || ntot < 1 || nwh < 6) return 1;
  nsamp = 1.5 + (tmax - tmin)/dt;

  if(nwh <= 40) nwh2 = nwh; else nwh2 = 40;
  if(mode <= 0)
       {
       if(nmoflag == 0 || nwh != nwhkeep || ntot != ntotkeep) return 1;
       for(i = 0; i < nwh2; i++)
            {
            if(hkeep[i] != h[i]) return 1;
            }
/*
        printf("am calling do_byte_nmo from piece2\n");
*/
       ierr = do_byte_nmo(-1, dop, iprint, times, vnmo, nkeep, dx,
                              h, nwh, bbbb, nsamp, ntot, tmin, dt);
/*
       ierr = do_nmo_correction2(-1, dop, times, vnmo, nkeep,
                              bbbb, h, nwh, ntot, tmin, tmax, dt);
*/
       if(ierr == 0) nmoflag = 0;
       if(mode < 0 || ierr != 0) return ierr;
       }

  get_velstruct_pointer_(&vel);
  vd = vel->vd;
  ifun1 = vd->ifun - 1;
  vp = (VpStruct*)vd->point[ifun1];
  n  = vd->n[ifun1];
  if(n < 1) return 1;

/*
        printf("am calling do_byte_nmo from piece2\n");
*/
  ierr = do_byte_nmo(1, dop, iprint, vp->times, vp->vnmo, n, dx,
                         h, nwh, bbbb, nsamp, ntot, tmin, dt);
/*
  ierr = do_nmo_correction2(1, dop, vp->times, vp->vnmo, n,
                         bbbb, h, nwh, ntot, tmin, tmax, dt);
*/
  if(ierr == 0)
       {
       ntotkeep = ntot;
       nwhkeep = nwh;
       for(i = 0; i < nwh2; i++)
            {
            hkeep[i] = h[i];
            }
       nkeep = n;
       for(i = 0; i < n; i++)
            {
            times[i] = vp->times[i];
            vnmo [i] = vp->vnmo [i];
            }
       nmoflag = 1;
       }
  return ierr;
}




/*---------- do non-hyperbolic nmo correction on byte arrays ------------*/
        /* uses the active velocity function for forward nmo */
        /* uses the saved velocity function for reverse nmo */
        /* returns 0 if all ok and 1 if fails */
        /* byte arrays are unchanged if fails */
  /*
    float nhosign    = 1.0 for normal NMO; typically -1.0 for non-hyp NMO.
    float nhoexp     = 2.0 for normal NMO; typically  4.0 for non-hyp NMO.
    int  mode        =  >= 1 for forward nmo, <= -1 for reverse nmo.
                        == 0 for reverse nmo followed by forward nmo.
    float dop        = doppler mute (= 2 to allow stretch up to factor 2).
unsigned char bbbb[] = trace byte values for traces in the image.
    float h[]        = trace headers for traces in the image.
    long nwh         = number of words in each trace header (offset = 6th word).
    long ntot        = number of traces in the image.
    float tmin, tmax = minimum and maximum times to plot.
    float dt         = sample rate (seconds).
  */


long do_nonhyp_nmo_correction(float nhosign, float nhoexp,
              int mode, float dop,
              unsigned char bbbb[], float h[], long nwh,
              long ntot, float tmin, float tmax, float dt)
{
  static float times[NMAX], vnmo[NMAX], hkeep[40];
  static long nkeep, nwhkeep, ntotkeep, nmoflag = 0;
  long n, nwh2, ifun1, ierr, i, nsamp, iprint = 0;
  float dx = STEP;
  VelStruct *vel;
  VdStruct *vd;
  VpStruct *vp;

  if(dt <= 0.0 || ntot < 1 || nwh < 6) return 1;
  nsamp = 1.5 + (tmax - tmin)/dt;

  if(nwh <= 40) nwh2 = nwh; else nwh2 = 40;
  if(mode <= 0)
       {
       if(nmoflag == 0 || nwh != nwhkeep || ntot != ntotkeep) return 1;
       for(i = 0; i < nwh2; i++)
            {
            if(hkeep[i] != h[i]) return 1;
            }
       ierr = do_byte_nonhyp_nmo(nhosign, nhoexp,
                              -1, dop, iprint, times, vnmo, nkeep, dx,
                              h, nwh, bbbb, nsamp, ntot, tmin, dt);
       if(ierr == 0) nmoflag = 0;
       if(mode < 0 || ierr != 0) return ierr;
       }

  get_velstruct_pointer_(&vel);
  vd = vel->vd;
  ifun1 = vd->ifun - 1;
  vp = (VpStruct*)vd->point[ifun1];
  n  = vd->n[ifun1];
  if(n < 1) return 1;

  ierr = do_byte_nonhyp_nmo(nhosign, nhoexp,
                         1, dop, iprint, vp->times, vp->vnmo, n, dx,
                         h, nwh, bbbb, nsamp, ntot, tmin, dt);
  if(ierr == 0)
       {
       ntotkeep = ntot;
       nwhkeep = nwh;
       for(i = 0; i < nwh2; i++)
            {
            hkeep[i] = h[i];
            }
       nkeep = n;
       for(i = 0; i < n; i++)
            {
            times[i] = vp->times[i];
            vnmo [i] = vp->vnmo [i];
            }
       nmoflag = 1;
       }
  return ierr;
}




/*--------------------------- end --------------------------------------*/

