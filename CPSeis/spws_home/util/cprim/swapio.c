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
!<CPS_v1 type=PRIMITIVE"/>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E 
!
! Name       : swapio
! Category   : io
! Written    : 2000-04-25   by: Tom Stoeckley
! Revised    : 2007-03-20   by: Kruger Corn
! Maturity   : beta
! Purpose    : Swap bytes while reading or writing binary data.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
! These functions call the swap primitive followed by fwrite, or fread
! followed by the swap primitive.  By default, swapping is done on little
! endian machines before writing and after reading.  By default, no swapping
! is done on big endian machines.
!
! Examples of  big   endian machines are:  SUN
! Examples of little endian machines are:  INTEL  VAX
!
! These functions return the number of objects read or written.  This
! is the same value returned by fread and fwrite.
!
! Nothing is done if nobj is zero.
!
! Eight functions are available for overriding the default action.  These
! functions can be called at any time to change the action which occurs.
! The changed action is in effect until one of these functions is called
! again.  These functions can be useful when converting an existing file
! from byte-swapped to not byte-swapped or vice versa.
!
! Swapping is never done unless the following variables have the
! specified size:    short 2    int 4    long 4    float 4    double 8
!
! Swapping is never done by the ..._char functions, which exist for
! completeness only.
!
! Functions with the size argument will never swap unless the size argument
! is 2 or 4 or 8, in which cases the swap is done for short, float, and
! double variables.  Since byte swapping is the same for int, long, and
! float variables (all 4 bytes), this will also work for int and long
! variables.  If these functions are called for strings, it is important
! to set size to 1 and nobj to the number of characters, and not to set
! size to the number of characters because inadvertant byte swapping would
! occur if the number of characters happened to be 2 or 4 or 8.
!
! See also:  swap_frou.f90  swap.c  swap.h  bswap.c  bswap.h  swapio.h
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!  
! Do read or write:
!     (these read or write but do not swap bytes)
!     (these are wrappers around fread and fwrite)
!     (nothing is done if nobj is zero)
!
!                                        o        i         i            i
!  int swapio_do_fread         (void   *ptr, int size, int nobj, FILE *stream);
!  int swapio_do_fread_char    (char   *ptr,           int nobj, FILE *stream);
!  int swapio_do_fread_short   (short  *ptr,           int nobj, FILE *stream);
!  int swapio_do_fread_int     (int    *ptr,           int nobj, FILE *stream);
!  int swapio_do_fread_long    (long   *ptr,           int nobj, FILE *stream);
!  int swapio_do_fread_float   (float  *ptr,           int nobj, FILE *stream);
!  int swapio_do_fread_double  (double *ptr,           int nobj, FILE *stream);
!
!                                        i        i         i            i
!  int swapio_do_fwrite  (const void   *ptr, int size, int nobj, FILE *stream);
!  int swapio_do_fwrite_char   (const char   *ptr,     int nobj, FILE *stream);
!  int swapio_do_fwrite_short  (const short  *ptr,     int nobj, FILE *stream);
!  int swapio_do_fwrite_int    (const int    *ptr,     int nobj, FILE *stream);
!  int swapio_do_fwrite_long   (const long   *ptr,     int nobj, FILE *stream);
!  int swapio_do_fwrite_float  (const float  *ptr,     int nobj, FILE *stream);
!  int swapio_do_fwrite_double (const double *ptr,     int nobj, FILE *stream);
!
!
! Do swap bytes:
!     (these always swap bytes but do not read or write)
!     (nothing is done if nobj is zero)
!
!                                          b        i         i
!   void swapio_do_swap          (void   *ptr, int size, int nobj);
!   void swapio_do_swap_char     (char   *ptr,           int nobj);
!   void swapio_do_swap_short    (short  *ptr,           int nobj);
!   void swapio_do_swap_int      (int    *ptr,           int nobj);
!   void swapio_do_swap_long     (long   *ptr,           int nobj);
!   void swapio_do_swap_float    (float  *ptr,           int nobj);
!   void swapio_do_swap_double   (double *ptr,           int nobj);
!
!
! Set the current byte swapping action:
!
!   void swapio_read_swap_always            ();
!   void swapio_read_swap_never             ();
!   void swapio_read_swap_on_big_endian     ();
!   void swapio_read_swap_on_little_endian  ();   (default)
!
!   void swapio_write_swap_always           ();
!   void swapio_write_swap_never            ();
!   void swapio_write_swap_on_big_endian    ();
!   void swapio_write_swap_on_little_endian ();   (default)
!
!   void swapio_toggle_read_swapping_action ();

!
! Learn whether to swap bytes (if doing your own I/O):
!
!    o
!   int swapio_whether_swap_after_read   ();   (returns TRUE or FALSE)
!   int swapio_whether_swap_before_write ();   (returns TRUE or FALSE)
!
!
! Maybe swap bytes (if doing your own I/O):
!     (these swap bytes only if the current swapping action says so)
!     (nothing is done if nobj is zero)
!
!                                                   b        i         i
!   void swapio_swap_after_read          (void    *ptr, int size, int nobj);
!   void swapio_swap_after_read_char     (char    *ptr,           int nobj);
!   void swapio_swap_after_read_short    (short   *ptr,           int nobj);
!   void swapio_swap_after_read_int      (int     *ptr,           int nobj);
!   void swapio_swap_after_read_int64    (int64_t *ptr,           int nobj);
!   void swapio_swap_after_read_long     (long    *ptr,           int nobj);
!   void swapio_swap_after_read_float    (float   *ptr,           int nobj);
!   void swapio_swap_after_read_double   (double  *ptr,           int nobj);
!
!   void swapio_swap_before_write        (void    *ptr, int size, int nobj);
!   void swapio_swap_before_write_char   (char    *ptr,           int nobj);
!   void swapio_swap_before_write_short  (short   *ptr,           int nobj);
!   void swapio_swap_before_write_int    (int     *ptr,           int nobj);
!   void swapio_swap_before_write_long   (long    *ptr,           int nobj);
!   void swapio_swap_before_write_float  (float   *ptr,           int nobj);
!   void swapio_swap_before_write_double (double  *ptr,           int nobj);
!                                                   b        i         i
!
! Read or write and maybe swap bytes:
!     (these swap bytes only if the current swapping action says so)
!     (nothing is done if nobj is zero)
!
!                                        o        i         i            i
!  int swapio_fread            (void   *ptr, int size, int nobj, FILE *stream);
!  int swapio_fread_char       (char   *ptr,           int nobj, FILE *stream);
!  int swapio_fread_short      (short  *ptr,           int nobj, FILE *stream);
!  int swapio_fread_int        (int    *ptr,           int nobj, FILE *stream);
!  int swapio_fread_long       (long   *ptr,           int nobj, FILE *stream);
!  int swapio_fread_float      (float  *ptr,           int nobj, FILE *stream);
!  int swapio_fread_double     (double *ptr,           int nobj, FILE *stream);
!
!                                        i        i         i            i
!  int swapio_fwrite     (const void   *ptr, int size, int nobj, FILE *stream);
!  int swapio_fwrite_char   (const char   *ptr,        int nobj, FILE *stream);
!  int swapio_fwrite_short  (const short  *ptr,        int nobj, FILE *stream);
!  int swapio_fwrite_int    (const int    *ptr,        int nobj, FILE *stream);
!  int swapio_fwrite_long   (const long   *ptr,        int nobj, FILE *stream);
!  int swapio_fwrite_float  (const float  *ptr,        int nobj, FILE *stream);
!  int swapio_fwrite_double (const double *ptr,        int nobj, FILE *stream);
!
! Handle some 32 to 64 bit conversion issues. Attempt to support reading files
!   created on a machine having a different word length. Make no attempt to
!   write to a machine having a different word length. Caller may set the
!   long length for reading.
!
!                                     i
!  void swapio_use_longlength (int *length);
!
!-------------------------------------------------------------------------------
!              TO TEST A FILE FOR BYTE SWAPPING WHILE READING
!
! To read a file regardless of whether it is byte swapped:
!
!  (1) Read the first record (or the first few records) on the file.
!  (2) Test an integer just read to determine if it is reasonable.
!  (3) If the integer is reasonable, you are already reading the file correctly.
!  (4) If the integer is not reasonable, take the following steps:
!   (5) Call a do_swap routine to swap the bytes of the variables just read.
!   (6) Test the integer again and give up if it is still not reasonable.
!   (7) If the integer is now reasonable, continue with the following steps:
!    (8) Call swapio_toggle_read_swapping_action.
!    (9) Read the rest of the file.
!    (10) Call swapio_toggle_read_swapping_action when finished reading the file
!          to reset the swapping action so that subsequent reads of other
!          files will not be affected.
!
! If you are not reading the file all at once (i.e. if other files might
! be open being read with these routines at the same time), you must call
! swapio_toggle_read_swapping_action before and after each set of reads
! to keep from affecting other files being read.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2007-03-20  Corn           Upgraded to 64 bit architecture.
!  1. 2000-04-25  Stoeckley      Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
!
! This process normally expects 4-byte (32-bit) word sizes.
! No byte swapping is done for other word sizes.
!
!-------------------------------------------------------------------------------
!</portability_doc>
*/


#include "swapio.h"
#include "swap.h"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>
#include <string.h>

/*---------------------- static functions -------------------------------*/
/*---------------------- static functions -------------------------------*/
/*---------------------- static functions -------------------------------*/


static int allowswap = FALSE;
static int swapread  = FALSE;
static int swapwrite = FALSE;
static int mlonglength = 0;
static int alonglength = 0;
static int32_t *i32 = NULL;
static int64_t *i64 = NULL;
static int i32_size = 0;
static int i64_size = 0;

#define I32TOI64 8
#define I64TOI32 4

static void swapio_startup()
{
  static int startup = TRUE;
  if(startup)
      {
      mlonglength = sizeof(long);
      if (sizeof(short) != 2 || sizeof(int) != 4 ||
          (mlonglength != 4 && mlonglength != 8) || sizeof(float) != 4 ||
           sizeof(double) != 8)
           {
           allowswap = FALSE;
           swapread  = FALSE;
           swapwrite = FALSE;
           }
      else
           {
           allowswap = TRUE;
           swapread  = !swap_endian();
           swapwrite = !swap_endian();
           }
      alonglength = mlonglength;
      startup = FALSE;
      }
}


static void swapio_print (const char *name)
{
/*********
  printf ("%s  allowswap=%d  swapread=%d  swapwrite=%d\n",
                      name, allowswap, swapread, swapwrite);
*********/
}


static void swapio_check_long_size (int nobj)
{
  assert (alonglength != mlonglength);
  switch (alonglength) {
  case 4 :
    if (i32 == NULL) {
      i32 = (int32_t *)malloc (sizeof(int32_t)*nobj);
    }
    else {
      i32 = (int32_t *)realloc (i32, sizeof(int32_t)*nobj);
    }
    i32_size = nobj > i32_size ? nobj : i32_size;
    break;
  case 8 :
    if (i32 == NULL) {
      i32 = (int32_t *)malloc (sizeof(int32_t)*nobj);
    }
    else {
      i32 = (int32_t *)realloc (i32, sizeof(int32_t)*nobj);
    }
    i32_size = nobj > i32_size ? nobj : i32_size;
    break;
  default:
    assert (0);
  }
}

static void swapio_long_arrays (void *from, void *to, int nobj,
  int direction)
{
  int32_t *i32;
  int64_t *i64;
  int k2;

  assert (from != NULL && to != NULL);

  if (from == to || nobj == 0) return;

  switch (direction) {
  case I32TOI64 :
    i32 = (int32_t *)from;
    i64 = (int64_t *)to;
    for (k2 = 0; k2 < nobj; k2++) {
      i64[k2] = (int64_t)i32[k2];
    }
    break;
  case I64TOI32 :
    i64 = (int64_t *)from;
    i32 = (int32_t *)to;
    for (k2 = 0; k2 < nobj; k2++) {
      if (i64[k2] > INT_MAX) {
        assert (0); // force a failure if data cannot be read correctly.
      }
      else {
        i32[k2] = (int32_t)i64[k2];
      }
    }
    break;
  default:
    assert (0);
  }
}

void swapio_use_longlength (int *length)
{
  if (length != NULL) {
    alonglength = *length;
    if (*length == mlonglength) return;
  }
  else {
    alonglength = mlonglength;
    return;
  }
  switch (alonglength) {
  case 4 :
    if (i64 != NULL) {
      free (i64);
      i64 = NULL;
      i64_size = 0;
    }
    swapio_check_long_size (100);
    break;
  case 8 :
    if (i32 != NULL) {
      free (i32);
      i32 = NULL;
      i32_size = 0;
    }
    swapio_check_long_size (100);
    break;
  default:
    assert (0);
  }
}

int swapio_get_longlength ()
{
  return alonglength;
}

/*------------------------ learn whether to swap --------------------------*/
/*------------------------ learn whether to swap --------------------------*/
/*------------------------ learn whether to swap --------------------------*/


int swapio_whether_swap_after_read()
{
  swapio_startup();
  return swapread;
}


int swapio_whether_swap_before_write()
{
  swapio_startup();
  return swapwrite;
}



/*----------------- set the current byte swapping action -----------------*/
/*----------------- set the current byte swapping action -----------------*/
/*----------------- set the current byte swapping action -----------------*/


void swapio_read_swap_always            ()
{
  swapio_startup();
  if(allowswap) swapread = TRUE;
  swapio_print("swapio_read_swap_always");
}


void swapio_read_swap_never             ()
{
  swapio_startup();
  swapread = FALSE;
  swapio_print("swapio_read_swap_never");
}


void swapio_read_swap_on_big_endian     ()
{
  swapio_startup();
  if(allowswap) swapread = swap_endian();
  swapio_print("swapio_read_swap_on_big_endian");
}


void swapio_read_swap_on_little_endian  ()
{
  swapio_startup();
  if(allowswap) swapread = !swap_endian();
  swapio_print("swapio_read_swap_on_little_endian");
}


                    /*-----------------------*/


void swapio_write_swap_always           ()
{
  swapio_startup();
  if(allowswap) swapwrite = TRUE;
  swapio_print("swapio_write_swap_always");
}


void swapio_write_swap_never            ()
{
  swapio_startup();
  swapwrite = FALSE;
  swapio_print("swapio_write_swap_never");
}


void swapio_write_swap_on_big_endian    ()
{
  swapio_startup();
  if(allowswap) swapwrite = swap_endian();
  swapio_print("swapio_write_swap_on_big_endian");
}


void swapio_write_swap_on_little_endian ()
{
  swapio_startup();
  if(allowswap) swapwrite = !swap_endian();
  swapio_print("swapio_write_swap_on_little_endian");
}


                    /*-----------------------*/


void swapio_toggle_read_swapping_action  ()
{
  swapio_startup();
  if (allowswap) swapread = !swapread;
  swapio_print("swapio_toggle_read_swapping_action");
}



/*---------------------------- do swap ----------------------------------*/
/*---------------------------- do swap ----------------------------------*/
/*---------------------------- do swap ----------------------------------*/


void swapio_do_swap   (void *ptr, int size, int nobj)
{
  switch (size)
      {
      case 2 : swapio_do_swap_short  ((short *)ptr, nobj); break;
      case 4 : swapio_do_swap_float  ((float *)ptr, nobj); break;
      case 8 : swapio_do_swap_double ((double*)ptr, nobj); break;
      }
}


void swapio_do_swap_char     (char* ptr, int nobj)
{
   /* do nothing */
}


void swapio_do_swap_short    (short  *ptr, int nobj)
{
  swapio_startup();
  if(nobj > 0 && allowswap) swap_short_2_cvec(ptr, &nobj);
}


void swapio_do_swap_int      (int    *ptr, int nobj)
{
  swapio_startup();
  if(nobj > 0 && allowswap) swap_int_4_cvec(ptr, &nobj);
}


void swapio_do_swap_long     (void   *ptr, int nobj)
{
  if (sizeof(long) == 4) {
    swapio_startup();
    if (nobj > 0 && allowswap) {
      swap_long_4_cvec ((int32_t *)ptr, &nobj);
    }
  }
  else if (sizeof(long) == 8) {
    swapio_do_swap_int64_t ((int64_t *)ptr, nobj);
  }
  else {
    assert (0); /* invalid long length */
  }
}


void swapio_do_swap_int64_t  (int64_t *ptr, int nobj)
{
  swapio_startup();
  if(nobj > 0 && allowswap) swap_long_8_cvec(ptr, &nobj);
}


void swapio_do_swap_float    (float  *ptr, int nobj)
{
  swapio_startup();
  if(nobj > 0 && allowswap) swap_float_4_cvec(ptr, &nobj);
}


void swapio_do_swap_double   (double *ptr, int nobj)
{
  swapio_startup();
  if(nobj > 0 && allowswap) swap_double_8_cvec(ptr, &nobj);
}



/*--------------------------- do read or write ---------------------------*/
/*--------------------------- do read or write ---------------------------*/
/*--------------------------- do read or write ---------------------------*/


int swapio_do_fread (void *ptr, int size, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fread(ptr, size, nobj, stream);
}


int swapio_do_fread_char (char *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fread(ptr, sizeof(char), nobj, stream);
}


int swapio_do_fread_short (short *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fread(ptr, sizeof(short), nobj, stream);
}


int swapio_do_fread_int (int *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fread(ptr, sizeof(int), nobj, stream);
}


int swapio_do_fread_long (long *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fread(ptr, sizeof(long), nobj, stream);
}


int swapio_do_fread_int64_t (int64_t *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fread(ptr, sizeof(int64_t), nobj, stream);
}


int swapio_do_fread_float (float *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fread(ptr, sizeof(float), nobj, stream);
}


int swapio_do_fread_double (double *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fread(ptr, sizeof(double), nobj, stream);
}


                    /*-------------------*/


int swapio_do_fwrite (const void *ptr, int size, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fwrite(ptr, size, nobj, stream);
}


int swapio_do_fwrite_char     (const char *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fwrite(ptr, sizeof(char), nobj, stream);
}


int swapio_do_fwrite_short     (const short *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fwrite(ptr, sizeof(short), nobj, stream);
}


int swapio_do_fwrite_int       (const int *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fwrite(ptr, sizeof(int), nobj, stream);
}


int swapio_do_fwrite_long      (const long *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fwrite(ptr, sizeof(long), nobj, stream);
}


int swapio_do_fwrite_float     (const float *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fwrite(ptr, sizeof(float), nobj, stream);
}


int swapio_do_fwrite_double    (const double *ptr, int nobj, FILE *stream)
{
  if(nobj == 0) return 0;
  return fwrite(ptr, sizeof(double), nobj, stream);
}



/*-------------------------- maybe swap ---------------------------------*/
/*-------------------------- maybe swap ---------------------------------*/
/*-------------------------- maybe swap ---------------------------------*/


void swapio_swap_after_read               (void   *ptr, int size, int nobj)
{
  switch (size)
      {
      case 2 : swapio_swap_after_read_short  ((short *)ptr, nobj); break;
      case 4 : swapio_swap_after_read_float  ((float *)ptr, nobj); break;
      case 8 : swapio_swap_after_read_double ((double*)ptr, nobj); break;
      }
}



void swapio_swap_after_read_char     (char   *ptr, int nobj)
{
   /* do nothing */
}


void swapio_swap_after_read_short    (short  *ptr, int nobj)
{
  if(swapio_whether_swap_after_read()) swapio_do_swap_short(ptr, nobj);
}


void swapio_swap_after_read_int      (int    *ptr, int nobj)
{
  if(swapio_whether_swap_after_read()) swapio_do_swap_int(ptr, nobj);
}


void swapio_swap_after_read_long     (long   *ptr, int nobj)
{
  if(swapio_whether_swap_after_read()) swapio_do_swap_long(ptr, nobj);
}


void swapio_swap_after_read_int64    (int64_t *ptr, int nobj)
{
  if(swapio_whether_swap_after_read()) swapio_do_swap_int64_t(ptr, nobj);
}


void swapio_swap_after_read_float    (float  *ptr, int nobj)
{
  if(swapio_whether_swap_after_read()) swapio_do_swap_float(ptr, nobj);
}


void swapio_swap_after_read_double   (double *ptr, int nobj)
{
  if(swapio_whether_swap_after_read()) swapio_do_swap_double(ptr, nobj);
}


                    /*-------------------*/


void swapio_swap_before_write             (void   *ptr, int size, int nobj)
{
  switch (size)
      {
      case 2 : swapio_swap_before_write_short  ((short *)ptr, nobj); break;
      case 4 : swapio_swap_before_write_float  ((float *)ptr, nobj); break;
      case 8 : swapio_swap_before_write_double ((double*)ptr, nobj); break;
      }
}



void swapio_swap_before_write_char     (char* ptr, int  nobj)
{
   /* do nothing */
}


void swapio_swap_before_write_short    (short  *ptr, int nobj)
{
  if(swapio_whether_swap_before_write()) swapio_do_swap_short(ptr, nobj);
}


void swapio_swap_before_write_int      (int    *ptr, int nobj)
{
  if(swapio_whether_swap_before_write()) swapio_do_swap_int(ptr, nobj);
}


void swapio_swap_before_write_long     (long   *ptr, int nobj)
{
  if(swapio_whether_swap_before_write()) swapio_do_swap_long(ptr, nobj);
}


void swapio_swap_before_write_float    (float  *ptr, int nobj)
{
  if(swapio_whether_swap_before_write()) swapio_do_swap_float(ptr, nobj);
}


void swapio_swap_before_write_double   (double *ptr, int nobj)
{
  if(swapio_whether_swap_before_write()) swapio_do_swap_double(ptr, nobj);
}



/*------------------- read and write and maybe swap -----------------------*/
/*------------------- read and write and maybe swap -----------------------*/
/*------------------- read and write and maybe swap -----------------------*/


int swapio_fread (void *ptr, int size, int nobj, FILE *stream)
{
  int num;
  switch (size)
      {
      case 2 : num = swapio_fread_short  ((short *)ptr, nobj, stream); break;
      case 4 : num = swapio_fread_float  ((float *)ptr, nobj, stream); break;
      case 8 : num = swapio_fread_double ((double*)ptr, nobj, stream); break;
      default: num = swapio_fread_char ((char*)ptr, size*nobj, stream); break;
      }
  return num;
}



int swapio_fread_char (char *ptr, int nobj, FILE *stream)
{
  return swapio_do_fread_char (ptr, nobj, stream);
}


int swapio_fread_short (short *ptr, int nobj, FILE *stream)
{
  int num = swapio_do_fread_short (ptr, nobj, stream);
  if (num == nobj) swapio_swap_after_read_short(ptr,nobj);
  return num;
}


int swapio_fread_int (int *ptr, int nobj, FILE *stream)
{
  int num = swapio_do_fread_int (ptr, nobj, stream);
  if (num == nobj) swapio_swap_after_read_int(ptr,nobj);
  return num;
}


int swapio_fread_long (long *ptr, int nobj, FILE *stream)
{
  int num;
  if (alonglength == mlonglength) {
    num = swapio_do_fread_long (ptr, nobj, stream);
    if (num == nobj) swapio_swap_after_read_long(ptr,nobj);
  }
  else if (alonglength == 4 && mlonglength == 8) {
    swapio_check_long_size (nobj);
    num = swapio_do_fread_int (i32, nobj, stream);
    if (num == nobj) swapio_swap_after_read_int (i32, nobj);
    swapio_long_arrays (i32, ptr, nobj, I32TOI64);
  }
  else if (alonglength == 8 && mlonglength == 4) {
    swapio_check_long_size (nobj);
    num = swapio_do_fread_int64_t (i64, nobj, stream);
    if (num == nobj) swapio_swap_after_read_int64 (i64, nobj);
    swapio_long_arrays (i64, ptr, nobj, I64TOI32);
  }
  else {
    assert (0);
  }
  return num;
}


int swapio_fread_float (float *ptr, int nobj, FILE *stream)
{
  int num = swapio_do_fread_float (ptr, nobj, stream);
  if (num == nobj) swapio_swap_after_read_float(ptr,nobj);
  return num;
}


int swapio_fread_double (double *ptr, int nobj, FILE *stream)
{
  int num = swapio_do_fread_double (ptr, nobj, stream);
  if (num == nobj) swapio_swap_after_read_double(ptr,nobj);
  return num;
}


                    /*-------------------*/


int swapio_fwrite (const void *ptr, int size, int nobj, FILE *stream)
{
  int num;
  switch (size)
      {
      case 2 : num = swapio_fwrite_short  ((const short *)ptr, nobj, stream);
               break;
      case 4 : num = swapio_fwrite_float  ((const float *)ptr, nobj, stream);
               break;
      case 8 : num = swapio_fwrite_double ((const double*)ptr, nobj, stream);
               break;
      default: num = swapio_fwrite_char ((const char*)ptr, size*nobj, stream);
               break;
      }
  return num;
}



int swapio_fwrite_char     (const char *ptr, int nobj, FILE *stream)
{
  return swapio_do_fwrite_char (ptr, nobj, stream);
}


int swapio_fwrite_short     (const short *ptr, int nobj, FILE *stream)
{
  int num;
  if(nobj > 0 && swapio_whether_swap_before_write())
      {
      short *ptr2 = malloc(nobj * sizeof(short));
      memcpy(ptr2, ptr, nobj * sizeof(short));
      swapio_do_swap_short(ptr2,nobj);
      num = swapio_do_fwrite_short(ptr2, nobj, stream);
      free(ptr2);
      }
  else
      {
      num = swapio_do_fwrite_short(ptr, nobj, stream);
      }
  return num;
}


int swapio_fwrite_int       (const int *ptr, int nobj, FILE *stream)
{
  int num;
  if(nobj > 0 && swapio_whether_swap_before_write())
      {
      int *ptr2 = malloc(nobj * sizeof(int));
      memcpy(ptr2, ptr, nobj * sizeof(int));
      swapio_do_swap_int(ptr2,nobj);
      num = swapio_do_fwrite_int(ptr2, nobj, stream);
      free(ptr2);
      }
  else
      {
      num = fwrite(ptr, sizeof(int), nobj, stream);
      }
  return num;
}


int swapio_fwrite_long      (const long *ptr, int nobj, FILE *stream)
{
  int num;
  if(nobj > 0 && swapio_whether_swap_before_write())
      {
      long *ptr2 = malloc(nobj * sizeof(long));
      memcpy(ptr2, ptr, nobj * sizeof(long));
      swapio_do_swap_long(ptr2,nobj);
      num = swapio_do_fwrite_long(ptr2, nobj, stream);
      free(ptr2);
      }
  else
      {
      num = fwrite(ptr, sizeof(long), nobj, stream);
      }
  return num;
}


int swapio_fwrite_float     (const float *ptr, int nobj, FILE *stream)
{
  int num;
  if(nobj > 0 && swapio_whether_swap_before_write())
      {
      float *ptr2 = malloc(nobj * sizeof(float));
      memcpy(ptr2, ptr, nobj * sizeof(float));
      swapio_do_swap_float(ptr2,nobj);
      num = swapio_do_fwrite_float(ptr2, nobj, stream);
      free(ptr2);
      }
  else
      {
      num = fwrite(ptr, sizeof(float), nobj, stream);
      }
  return num;
}


int swapio_fwrite_double    (const double *ptr, int nobj, FILE *stream)
{
  int num;
  if(nobj > 0 && swapio_whether_swap_before_write())
      {
      double *ptr2 = malloc(nobj * sizeof(double));
      memcpy(ptr2, ptr, nobj * sizeof(double));
      swapio_do_swap_double(ptr2,nobj);
      num = swapio_do_fwrite_double(ptr2, nobj, stream);
      free(ptr2);
      }
  else
      {
      num = fwrite(ptr, sizeof(double), nobj, stream);
      }
  return num;
}



/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

