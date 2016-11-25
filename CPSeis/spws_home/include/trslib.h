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


/*------------------------- trslib.h -----------------------------------*/


#ifndef _TRSLIB_H_
#define _TRSLIB_H_

#include "c2f_interface.h"

#ifdef __cplusplus
extern "C" {
#endif




/*---------- beginning of sections for each source file ----------------*/
/*---------- beginning of sections for each source file ----------------*/
/*---------- beginning of sections for each source file ----------------*/
/*---------- beginning of sections for each source file ----------------*/
/*---------- beginning of sections for each source file ----------------*/



/*------------------------ math_futil.f --------------------------------*/
    /* only some of the functions in math_futil.f are included here */

#ifdef NEED_UNDERSCORE
#define triplesort           triplesort_
#define terp1                terp1_
#define terpfill             terpfill_
#define densify3             densify3_
#define densify_c            densify_c_
#define densify_nonlin_c     densify_nonlin_c_
#define give_to_mike         give_to_mike_
#endif

#ifdef NEED_CAPITALS
#define triplesort           TRIPLESORT
#define terp1                TERP1
#define terpfill             TERPFILL
#define densify3             DENSIFY3
#define densify_c            DENSIFY_C
#define densify_nonlin_c     DENSIFY_NONLIN_C
#define give_to_mike         GIVE_TO_MIKE
#endif

void   triplesort (float *y, float *x, long *r, long *n);
float terp1 (const float *xwant, const float *x, const int *n, const float *v);
void  terpfill (const float *x, const float *y, const int *n,
                const float *xmin, const float *xmax,
                const long *nsamp, float *array);

void   densify_c (long *terpflag, long *stepflag, long *endflag,
          float *dx, long *nwant, float *dz, long *ncoef, long *nrun,
          long *n , float *x , float *z ,
          long *nn, float *xx, float *zz,
          long *nmax, float *q, char *msg, long *ierr);

void densify_nonlin_c
         (const int *terpflag, const int *stepflag, const int *endflag,
          const float *delta1, const float *delta2, const int *nwant,
          const float *deltav,
          const int *ncoef, const int *nrun,
          const int *n, const float *x, const float *v,
          int *nn, float *xx, float *vv,
          const int *maxpicks, float *q,
          char *msg, int *ierr);

void   densify3 (float *dx, long *nmax, long *n, float x[], float z[],
                        long *nn, float xx[], float zz[], long *error);
 
 
/*----------------------- do_byte_nmo.c --------------------------------*/
 
long do_byte_nmo(long mode, float dop, long iprint,
              float times[], float vnmo[], long n, float dx,
              float head[], long nwords, unsigned char bbbb[],
              long nsamp, long ntraces, float tstrt, float dt);

long do_byte_nonhyp_nmo(float nhosign, float nhoexp,
              long mode, float dop, long iprint,
              float times[], float vnmo[], long n, float dx,
              float head[], long nwords, unsigned char bbbb[],
              long nsamp, long ntraces, float tstrt, float dt);

long do_float_nonhyp_nmo(float nhosign, float nhoexp,
              long mode, float dop, long iprint,
              float times[], float vnmo[], long n, float dx,
              float head[], long nwords, float bbbb[],
              long nsamp, long ntraces, float tstrt, float dt);

long do_byte_eta_nmo(long mode, float dop, long iprint,
              float times[], float vnmo[], float eta[], long n, float dx,
              float head[], long nwords, unsigned char bbbb[],
              long nsamp, long ntraces, float tstrt, float dt);

long do_float_eta_nmo(long mode, float dop, long iprint,
              float times[], float vnmo[], float eta[], long n, float dx,
              float head[], long nwords, float bbbb[],
              long nsamp, long ntraces, float tstrt, float dt);



/*---------------------- get_doppler_mute.f -----------------------------*/

#ifdef NEED_UNDERSCORE
#define get_doppler_mute     get_doppler_mute_
#endif

#ifdef NEED_CAPITALS
#define get_doppler_mute     GET_DOPPLER_MUTE
#endif

void get_doppler_mute(float *tmax, float *dt, float *dopler, float *offset,
                      float t[], float v[], long *n, float *tmute);


/*------------------------ combo_util.c ---------------------------------*/

#ifdef NEED_UNDERSCORE
#define force_updates        force_updates_
#endif

#ifdef NEED_CAPITALS
#define force_updates        FORCE_UPDATES
#endif

void register_update_function(void (*fun)(void *data), void *data);
void force_updates           (void);

 

/*---------------------- velfun_futil.f ----------------------------------*/

#ifdef NEED_UNDERSCORE
#define velfun_put_picks           velfun_put_picks_
#define velfun_get_picks           velfun_get_picks_
#define velfun_fixup_first_pick    velfun_fixup_first_pick_
#define velfun_fixup_first_pick_c  velfun_fixup_first_pick_c_
#define velfun_insert_pick         velfun_insert_pick_
#define velfun_remove_pick         velfun_remove_pick_
#define velfun_put_pick            velfun_put_pick_
#define velfun_get_pick            velfun_get_pick_
#define velfun_update              velfun_update_
#define velfun_update_c            velfun_update_c_
#define velfun_inv                 velfun_inv_
#define velfun_nmo                 velfun_nmo_
#define velfun_curve               velfun_curve_
#define velfun_nmo_prep            velfun_nmo_prep_
#define velfun_do_nmo              velfun_do_nmo_
#define nonhyp_nmo_prep            nonhyp_nmo_prep_
#define nonhyp_do_nmo              nonhyp_do_nmo_
#define eta_nmo_prep               eta_nmo_prep_
#define eta_do_nmo                 eta_do_nmo_
#endif

#ifdef NEED_CAPITALS
#define velfun_put_picks           VELFUN_PUT_PICKS
#define velfun_get_picks           VELFUN_GET_PICKS
#define velfun_fixup_first_pick    VELFUN_FIXUP_FIRST_PICK
#define velfun_fixup_first_pick_c  VELFUN_FIXUP_FIRST_PICK_C
#define velfun_insert_pick         VELFUN_INSERT_PICK
#define velfun_remove_pick         VELFUN_REMOVE_PICK
#define velfun_put_pick            VELFUN_PUT_PICK
#define velfun_get_pick            VELFUN_GET_PICK
#define velfun_update              VELFUN_UPDATE
#define velfun_update_c            VELFUN_UPDATE_C
#define velfun_inv                 VELFUN_INV
#define velfun_nmo                 VELFUN_NMO
#define velfun_curve               VELFUN_CURVE
#define velfun_nmo_prep            VELFUN_NMO_PREP
#define velfun_do_nmo              VELFUN_DO_NMO
#define nonhyp_nmo_prep            NONHYP_NMO_PREP
#define nonhyp_do_nmo              NONHYP_DO_NMO
#define eta_nmo_prep               ETA_NMO_PREP
#define eta_do_nmo                 ETA_DO_NMO
#endif

    /* prototypes without arg lists contain fortran character args */

void velfun_fixup_first_pick_c
  (const char *type_symbol, int *npicks, const int *maxpicks,
   const float *startvel, const float *timetol, const float *depthtol,
   float *depth, float *time, float *vnmo, float *vrms,
   float *vav, float *vint, float *thick);

void velfun_update_c
  (const int *invoke, const char *type_symbol, const int *npicks, /* input */
   const int *muteflag, const float *offmin, const float *offmax, /* input */
   const int *nmute, const float *omute, const float *tmute,      /* input */
   float *depth, float *time,                                 /* input/output */
   float *vnmo, float *vrms, float *vav, float *vint,         /* input/output */
   float *thick,                                              /* input/output */
   float *offset, int *nray, float *oray, float *tray,            /* output */
   char *msg, int *ierr);                                         /* output */

void velfun_put_picks_       ();
void velfun_get_picks_       ();
void velfun_fixup_first_pick_();
void velfun_insert_pick_     (long *j, long *n, long *nmax,
             float depths[], float times[], float vnmo[],
             float vrms[], float vav[], float vint[], float thick[]);
void velfun_remove_pick_ (long *j, long *n,
             float depths[], float times[], float vnmo[],
             float vrms[], float vav[], float vint[], float thick[]);
void velfun_put_pick_        ();
void velfun_get_pick_        ();
void velfun_update_          ();
void velfun_inv_             ();
void velfun_nmo_             ();
void velfun_curve_           (long *n, float depths[], float vint[],
             float *offmin, float *offmax, float *vnmo, float *offpick,
             long *nray, float oray[200], float tray[200]);

void velfun_nmo_prep_ (float times[], float vnmo[], long *n,
             float *tmin, float *tmax, float *dt,
             long *nnn, float ttt[], float vvv[], float tb[],
             float *cnst, long *ierr);
void velfun_do_nmo_ (float *dop, long *nnn, float ttt[], float vvv[],
             float tb[], float *cnst,
             long *idir, float *offset, float ta[],
             float a[], float b[], long *nval, float *bmute);

void nonhyp_nmo_prep_ (float *nhosign, float *nhoexp,
             float times[], float vnmo[], long *n,
             float *tmin, float *tmax, float *dt,
             long *nnn, float ttt[], float vvv[], float tb[],
             float *cnst, long *ierr);
void nonhyp_do_nmo_ (float *nhosign, float *nhoexp,
             float *dop, long *nnn, float ttt[], float vvv[],
             float tb[], float *cnst,
             long *idir, float *offset, float ta[],
             float a[], float b[], long *nval, float *bmute);

void eta_nmo_prep_ (float times[], float vnmo[], float eta[], long *n,
                    float *tmin, float *tmax, float *dt,
                    long *nnn, float ttt[], float vvv[],
                    float eee[], float fff[], float ggg[],
                    float tb[], float *cnst, long *ierr);
void eta_do_nmo_   (float *dop, long *nnn, float ttt[], float vvv[],
                    float eee[], float fff[], float ggg[],
                    float tb[], float *cnst,
                    long *idir, float *offset, float ta[],
                    float a[], float b[], long *nval, float *bmute);

 
 
/*---------------- end of sections for each source file ----------------*/
/*---------------- end of sections for each source file ----------------*/
/*---------------- end of sections for each source file ----------------*/
/*---------------- end of sections for each source file ----------------*/
/*---------------- end of sections for each source file ----------------*/


#ifdef __cplusplus
}
#endif


#endif

/*--------------------------- end --------------------------------------*/



