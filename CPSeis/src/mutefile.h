/****
!<CPS_v1 type="HEADER_FILE"/>
****/
/*---------------------------- mutefile.h -----------------------------------*/
/*---------------------------- mutefile.h -----------------------------------*/
/*---------------------------- mutefile.h -----------------------------------*/

                     /* other files are:  mutefile.c */

/****
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2002-01-02  Stoeckley  Changes for compatibility with ~spws (to
!                             eliminate duplicate code).
!  2. 2001-05-10  Stoeckley  Move some header files to implementation file;
!                             bring up to specs.
!  1. 1999-09-10  O'Brien    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _MUTEFILE_H_
#define _MUTEFILE_H_

#include "xyzdim.h"

#ifdef __cplusplus
extern "C" {
#endif


/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/


typedef struct _MuteStruct MuteStruct;

struct _MuteStruct
  {
  XYZdimStruct *xyzdim;      /* XYZ 3D data object */
  long   nhx, nhy, nhz;      /* header word numbers  */
  float  ylatest, zlatest;   /* latest bins updated  */
  float  ysel   , zsel   ;   /* selected bins for comparison */

  int    yz_switch;  /* FALSE for comparison with prev or next Y bin */
                     /* TRUE  for comparison with prev or next Z bin */
  int    interp;
       /* FALSE to call mutefile_get_value      from mutefile_move_picks */
       /* TRUE  to call mutefile_get_terp_value from mutefile_move_picks */
  } ;

#ifndef YES
#define YES    1
#endif

#ifndef NO
#define NO    -1
#endif

#ifndef MAYBE
#define MAYBE  0
#endif

    /* the following definitions must match those in inquire.h */

#define MUTEFILE_BLANK          1 /* filename is "NONE" or ""  */
#define MUTEFILE_NOT_FOUND      2 /* file does not exist but is writeable */
#define MUTEFILE_NOT_CREATEABLE 3 /* file does not exist and is not writeable */
#define MUTEFILE_FOUND          4 /* file exists and is readable/writeable */
#define MUTEFILE_NOT_READABLE   5 /* file exists but is not readable */
#define MUTEFILE_NOT_WRITEABLE  6 /* file exists but is not writeable */
#define MUTEFILE_NOT_READ_WRITE 7 /* file exists but is not read/writeable */
#define MUTEFILE_VALID         21 /* file exists, is readable/writeable, and
                                     is a valid file of appropriate type */
#define MUTEFILE_ERROR         41 /* a file error occurred */
#define MUTEFILE_CREATE        42 /* output file will be created from scratch */
#define MUTEFILE_READ_ONLY     43 /* input file will be read-only */
#define MUTEFILE_COPY          44 /* input file will be copied to output file */
#define MUTEFILE_UPDATE        45 /* input file will be updated */
#define MUTEFILE_CHANGES       61 /* changes occurred in files or file status */


/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/


MuteStruct *mutefile_create       (void);
void        mutefile_clear        (MuteStruct *ss);
MuteStruct *mutefile_destroy      (MuteStruct *ss);

void  mutefile_make_grid (MuteStruct *ss,
                          float xmin, float ymin, float zmin,
                          float xmax, float ymax, float zmax,
                          float xinc, float yinc, float zinc,
                          MuteStruct *source);

void  mutefile_replace_with_grid (MuteStruct *ss,
                                  float xmin, float ymin, float zmin,
                                  float xmax, float ymax, float zmax,
                                  float xinc, float yinc, float zinc);

float mutefile_get_nil      (void);
long  mutefile_get_nz       (MuteStruct *ss);
long  mutefile_get_ny       (MuteStruct *ss, long iz);
long  mutefile_get_nx       (MuteStruct *ss, long iy, long iz);

float mutefile_get_z        (MuteStruct *ss, long iz);
float mutefile_get_y        (MuteStruct *ss, long iy, long iz);
float mutefile_get_x        (MuteStruct *ss, long ix, long iy, long iz);
float mutefile_get_v        (MuteStruct *ss, long ix, long iy, long iz);

long  mutefile_get_nhx      (MuteStruct *ss);
long  mutefile_get_nhy      (MuteStruct *ss);
long  mutefile_get_nhz      (MuteStruct *ss);

float mutefile_get_ylatest  (MuteStruct *ss);
float mutefile_get_zlatest  (MuteStruct *ss);
float mutefile_get_ysel     (MuteStruct *ss);
float mutefile_get_zsel     (MuteStruct *ss);
int   mutefile_get_yz_switch(MuteStruct *ss);
int   mutefile_get_interp   (MuteStruct *ss);

float mutefile_get_xmin     (MuteStruct *ss);
float mutefile_get_ymin     (MuteStruct *ss);
float mutefile_get_zmin     (MuteStruct *ss);
float mutefile_get_xmax     (MuteStruct *ss);
float mutefile_get_ymax     (MuteStruct *ss);
float mutefile_get_zmax     (MuteStruct *ss);

int   mutefile_set_nhx      (MuteStruct *ss, long nhx);
int   mutefile_set_nhy      (MuteStruct *ss, long nhy);
int   mutefile_set_nhz      (MuteStruct *ss, long nhz);
int   mutefile_set_ysel     (MuteStruct *ss, float ysel, int directional);
int   mutefile_set_zsel     (MuteStruct *ss, float zsel, int directional);
int   mutefile_set_yz_switch(MuteStruct *ss, int yz_switch);
int   mutefile_set_interp   (MuteStruct *ss, int interp);

float mutefile_get_terp_value (MuteStruct *ss, float x, float y, float z);
float mutefile_get_value      (MuteStruct *ss, float x, float y, float z);
int   mutefile_insert_point   (MuteStruct *ss, float x, float y, float z,
                                               float v);
int   mutefile_insert_range   (MuteStruct *ss, float xa, float va,
                                               float xb, float vb,
                                               float y, float z);

float mutefile_get_x_from_header (MuteStruct *ss, const float *head);
float mutefile_get_y_from_header (MuteStruct *ss, const float *head);
float mutefile_get_z_from_header (MuteStruct *ss, const float *head);

void mutefile_get_picks      (MuteStruct *ss, const float head[], long nwords,
                              long ntraces, float picks[]);
void mutefile_put_picks      (MuteStruct *ss, const float head[], long nwords,
                              long ntraces, float picks[]);
void mutefile_get_prev_picks (MuteStruct *ss, const float head[], long nwords,
                              long ntraces, float picks[]);
void mutefile_get_next_picks (MuteStruct *ss, const float head[], long nwords,
                              long ntraces, float picks[]);
void mutefile_get_sel_picks  (MuteStruct *ss, const float head[], long nwords,
                              long ntraces, float picks[]);

int  mutefile_prepare (MuteStruct *ss, char *filename1, char *filename2,
                       void (*msgfun)(void *data, char *msg),
                       void *msgdata, long status, char *msg);

int  mutefile_save_file   (MuteStruct *ss, char *filename, char *msg);
int  mutefile_read_file   (MuteStruct *ss, char *filename, char *msg); 

long mutefile_check_validity   (MuteStruct *ss, char *filename, char *info);

void mutefile_check_validities (MuteStruct *ss,
                                char *filename1, char *filename2,
                                long    *valid1, long    *valid2,
                                char     *info1, char     *info2,
                                long *same_datasets);

long mutefile_inquire (MuteStruct *ss,
                       char *filename1, char *filename2,
                       long  required1, long  required2,
                       char      *msg1, char      *msg2, char *msg3,
                       long   *status1, long   *status2);


/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/


#ifdef __cplusplus
}     
#endif

#endif 


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

