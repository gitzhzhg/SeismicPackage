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
/*
!<CPS_v1 type="HEADER_FILE"/>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2007-02-19  Corn         Handle some 64 bit machine issues.
!  1. 2000-04-25  Stoeckley    Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _swapio_H_
#define _swapio_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

#include "upgrade264.h"


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

int swapio_do_fread         (void   *ptr, int size, int nobj, FILE *stream);
int swapio_do_fread_char    (char   *ptr,           int nobj, FILE *stream);
int swapio_do_fread_short   (short  *ptr,           int nobj, FILE *stream);
int swapio_do_fread_int     (int    *ptr,           int nobj, FILE *stream);
int swapio_do_fread_long    (long   *ptr,           int nobj, FILE *stream);
int swapio_do_fread_float   (float  *ptr,           int nobj, FILE *stream);
int swapio_do_fread_double  (double *ptr,           int nobj, FILE *stream);

int swapio_do_fwrite  (const void   *ptr, int size, int nobj, FILE *stream);
int swapio_do_fwrite_char   (const char   *ptr,     int nobj, FILE *stream);
int swapio_do_fwrite_short  (const short  *ptr,     int nobj, FILE *stream);
int swapio_do_fwrite_int    (const int    *ptr,     int nobj, FILE *stream);
int swapio_do_fwrite_long   (const long   *ptr,     int nobj, FILE *stream);
int swapio_do_fwrite_float  (const float  *ptr,     int nobj, FILE *stream);
int swapio_do_fwrite_double (const double *ptr,     int nobj, FILE *stream);

void swapio_do_swap          (void    *ptr, int size, int nobj);
void swapio_do_swap_char     (char    *ptr,           int nobj);
void swapio_do_swap_short    (short   *ptr,           int nobj);
void swapio_do_swap_int      (int     *ptr,           int nobj);
void swapio_do_swap_int64_t  (int64_t *ptr,           int nobj);
void swapio_do_swap_long     (void    *ptr,           int nobj);
void swapio_do_swap_float    (float   *ptr,           int nobj);
void swapio_do_swap_double   (double  *ptr,           int nobj);

void swapio_read_swap_always            ();
void swapio_read_swap_never             ();
void swapio_read_swap_on_big_endian     ();
void swapio_read_swap_on_little_endian  ();

void swapio_write_swap_always           ();
void swapio_write_swap_never            ();
void swapio_write_swap_on_big_endian    ();
void swapio_write_swap_on_little_endian ();

void swapio_toggle_read_swapping_action ();

int swapio_whether_swap_after_read   ();
int swapio_whether_swap_before_write ();

void swapio_swap_after_read          (void    *ptr, int size, int nobj);
void swapio_swap_after_read_char     (char    *ptr,           int nobj);
void swapio_swap_after_read_short    (short   *ptr,           int nobj);
void swapio_swap_after_read_int      (int     *ptr,           int nobj);
void swapio_swap_after_read_int64    (int64_t *ptr,           int nobj);
void swapio_swap_after_read_long     (long    *ptr,           int nobj);
void swapio_swap_after_read_float    (float   *ptr,           int nobj);
void swapio_swap_after_read_double   (double  *ptr,           int nobj);

void swapio_swap_before_write        (void   *ptr, int size, int nobj);
void swapio_swap_before_write_char   (char   *ptr,           int nobj);
void swapio_swap_before_write_short  (short  *ptr,           int nobj);
void swapio_swap_before_write_int    (int    *ptr,           int nobj);
void swapio_swap_before_write_long   (long   *ptr,           int nobj);
void swapio_swap_before_write_float  (float  *ptr,           int nobj);
void swapio_swap_before_write_double (double *ptr,           int nobj);

int swapio_fread               (void   *ptr, int size, int nobj, FILE *stream);
int swapio_fread_char          (char   *ptr,           int nobj, FILE *stream);
int swapio_fread_short         (short  *ptr,           int nobj, FILE *stream);
int swapio_fread_int           (int    *ptr,           int nobj, FILE *stream);
int swapio_fread_long          (long   *ptr,           int nobj, FILE *stream);
int swapio_fread_float         (float  *ptr,           int nobj, FILE *stream);
int swapio_fread_double        (double *ptr,           int nobj, FILE *stream);

int swapio_fwrite        (const void   *ptr, int size, int nobj, FILE *stream);
int swapio_fwrite_char   (const char   *ptr,           int nobj, FILE *stream);
int swapio_fwrite_short  (const short  *ptr,           int nobj, FILE *stream);
int swapio_fwrite_int    (const int    *ptr,           int nobj, FILE *stream);
int swapio_fwrite_long   (const long   *ptr,           int nobj, FILE *stream);
int swapio_fwrite_float  (const float  *ptr,           int nobj, FILE *stream);
int swapio_fwrite_double (const double *ptr,           int nobj, FILE *stream);

void swapio_use_longlength (int *length);
int  swapio_get_longlength ();

/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/


#ifdef __cplusplus
}
#endif


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

