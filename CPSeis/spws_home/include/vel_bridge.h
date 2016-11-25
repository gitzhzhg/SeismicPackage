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

/*------------------------- vel_bridge.h --------------------------------*/

     /* this header file goes with vel_fbridge.f and vel_cbridge.c */

#ifndef _VEL_BRIDGE_H
#define _VEL_BRIDGE_H

#include "c2f_interface.h"

/*-------- fortran subroutine spelling adjustments for VMS --------------*/

#if (VMS || _AIX || __hpux)
#define start_wait_cursor_          start_wait_cursor
#define stop_wait_cursor_           stop_wait_cursor
#define f_close_tempfile_           f_close_tempfile
#define f_read_tempfile_            f_read_tempfile
#define f_activate_velfun_          f_activate_velfun
#define f_insert_blank_velfun_      f_insert_blank_velfun
#define f_insert_velfun_            f_insert_velfun
#define f_remove_velfun_            f_remove_velfun
#define f_modify_velfun_            f_modify_velfun
#define f_delete_velfuns_           f_delete_velfuns
#define f_multiply_velfuns_         f_multiply_velfuns
#define fsort_functions_            fsort_functions    /* C function */
#define get_velstruct_pointer_      get_velstruct_pointer
#define read_velocities_            read_velocities
#define save_velocities_            save_velocities
#define validate_velocities_        validate_velocities
#define read_ref_velocities_        read_ref_velocities
#define remove_velocities_          remove_velocities
#define f_autosave_workfile_        f_autosave_workfile
#endif

#ifdef ultrix
#endif



/*-------- fortran subroutine spelling adjustments for CRAY -------------*/

#ifdef NEED_CAPITALS
#define start_wait_cursor_          START_WAIT_CURSOR
#define stop_wait_cursor_           STOP_WAIT_CURSOR
#define f_close_tempfile_           F_CLOSE_TEMPFILE
#define f_read_tempfile_            F_READ_TEMPFILE
#define f_activate_velfun_          F_ACTIVATE_VELFUN
#define f_insert_blank_velfun_      F_INSERT_BLANK_VELFUN
#define f_insert_velfun_            F_INSERT_VELFUN
#define f_remove_velfun_            F_REMOVE_VELFUN
#define f_modify_velfun_            F_MODIFY_VELFUN
#define f_delete_velfuns_           F_DELETE_VELFUNS
#define f_multiply_velfuns_         F_MULTIPLY_VELFUNS
#define fsort_functions_            FSORT_FUNCTIONS    /* C function */
#define get_velstruct_pointer_      GET_VELSTRUCT_POINTER
#define read_velocities_            READ_VELOCITIES
#define save_velocities_            SAVE_VELOCITIES
#define validate_velocities_        VALIDATE_VELOCITIES
#define read_ref_velocities_        READ_REF_VELOCITIES
#define remove_velocities_          REMOVE_VELOCITIES
#define f_autosave_workfile_        F_AUTOSAVE_WORKFILE
#endif



/*------------------ c function prototypes -----------------------------*/
    /* prototypes without arg lists contain arg type VelStruct */

#ifdef __cplusplus
extern "C" {
#endif

void create_wait_cursor ();
void start_wait_cursor_ ();
void stop_wait_cursor_  ();
void fsort_functions_   ();
void sort_functions     ();
/*
void create_wait_cursor (VelStruct  *vel);
void start_wait_cursor_ (VelStruct **vel);
void stop_wait_cursor_  (VelStruct **vel);
void fsort_functions_   (VelStruct **vel, long *error);
void sort_functions     (VelStruct  *vel, long *error);
*/

void find_min_tols(void);

int  find_velfun   (float xbin, float ybin);
int  find_xymatch  (float xbin, float ybin, float *x, float *y, long n);
int  find_xynearest(float xbin, float ybin, float *x, float *y, long n,
                                      float xperpix, float yperpix);
int  find_xnext(long i, float xbin, float ybin, float *x, float *y, long n);
int  find_xprev(long i, float xbin, float ybin, float *x, float *y, long n);
int  find_ynext(long i, float xbin, float ybin, float *x, float *y, long n);
int  find_yprev(long i, float xbin, float ybin, float *x, float *y, long n);

int  read_vels    (Widget w, String filename,               String msg);
int  save_vels    (Widget w, String filename, String type2, String msg);
long validate_vels          (String filename,
                          long *nhx, long *nhy, long *nfun, String info);

void draw_doppler_mute(Widget w, GC gc, float h[], long nwh, long ntot,
           float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2,
           float tp[], float vp[], long np, float dopler);

void draw_nonhyperbola_full(float nhosign, float nhoexp,
           Widget w, GC gc, float h[], long nwh, long ntot,
           float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float tp, float vp);

void draw_nonhyperbola_part(float nhosign, float nhoexp,
           Widget w, GC gc, float h[], long nwh, long ntot,
           float vmin, float vmax, float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float *tp, float *vp);

void draw_hyperbola_full(Widget w, GC gc, float h[], long nwh, long ntot,
           float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float tp, float vp);

void draw_hyperbola_part(Widget w, GC gc, float h[], long nwh, long ntot,
           float vmin, float vmax, float tmin, float tmax,
           long trace_width, double yperpix, long xzero, long yzero,
           int x1, int y1, int x2, int y2, float *tp, float *vp);

void get_bounding_rect(float h[], long nwh, long ntot, float tmin,
           long trace_width, double yperpix, long xzero, long yzero,
           int *x1, int *y1, int *x2, int *y2,
           float tp, float vp, float tq, float vq);

void get_nonhyp_bounding_rect(float nhosign, float nhoexp,
                       float h[], long nwh, long ntot, float tmin,
           long trace_width, double yperpix, long xzero, long yzero,
           int *x1, int *y1, int *x2, int *y2,
           float tp, float vp, float tq, float vq);

void vel_to_byte(float *vpoint, float *tpoint, long n,
               float vmin, float vmax, float tmin, float tmax,
               long nbins, unsigned char *byte_array);

void xbin_to_float(VdStruct *vd, long veltype, float xbin, float time,
               float ymin, float ymax,
               long nbins, float *float_array, int vint_grade);

void vel_to_float(float *vpoint, float *tpoint, long n,
               float tmin, float tmax,
               long nbins, float *float_array, long veltype, int vint_grade);

long do_nmo_correction(int mode, float dop,
              unsigned char bbbb[], float h[], long nwh,
              long ntot, float tmin, float tmax, float srval);

long do_nonhyp_nmo_correction(float nhosign, float nhoexp,
              int mode, float dop,
              unsigned char bbbb[], float h[], long nwh,
              long ntot, float tmin, float tmax, float srval);


/*---------------- fortran function prototypes -------------------------*/
    /* prototypes without arg lists contain fortran character args */

void f_save_tempfile_  (long *ibox,  long * ierr);
void f_read_tempfile_  (long *dummy, long * ierr);   /* entry */
void f_close_tempfile_ (void);                       /* entry */

void f_read_inquire_();
void f_save_inquire_();
void f_read_velfile_();
void f_save_velfile_();

void f_activate_velfun_    (long *i                         );
void f_insert_blank_velfun_(long *i,              long *ierr);
void f_insert_velfun_      (long *i,              long *ierr);
void f_remove_velfun_      (long *i,              long *ierr);
void f_modify_velfun_      (long *i, long *itype, long *ierr);
void f_raytrace_velfuns_   ();
void f_resample_velfuns_   ();
void f_latsample_velfuns_  ();
void f_lat2sample_velfuns_ ();
void f_velfun_types_       ();
void f_velfun_vfids_       ();
void f_velfun_headers_(long *ichoice, long * which                           );
void f_misc_actions_  (long *ichoice, long *iwhich, float *vwater, long *ierr);
void f_delete_velfuns_(long *ichoice,                              long *ierr);
void f_multiply_velfuns_(long *ichoice,                            long *ierr);

void get_velstruct_pointer_();   /* arg is VelStruct * */
long read_velocities_(char *buffer,             char *muffer);
long save_velocities_(char *buffer, char *tbuf, char *muffer);
long validate_velocities_(char *buffer,
                           long *nhx, long *nhy, long *nfun, char *muffer);
long read_ref_velocities_(char *buffer, char *muffer);
void remove_velocities_(void);
void f_autosave_workfile_(long *ibox);
void f_autosave_workfile_(long *ibox);

#ifdef __cplusplus
}
#endif


#endif

/*--------------------------- end --------------------------------------*/

