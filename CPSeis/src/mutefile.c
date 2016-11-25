/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*------------------------------- mutefile.c --------------------------------*/
/*------------------------------- mutefile.c --------------------------------*/
/*------------------------------- mutefile.c --------------------------------*/

                     /* other files are:  mutefile.h */

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


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E                        
!
! Name       : mutefile
! Category   : io
! Written    : 1994-11-30   by: Stoeckley
! Revised    : 2007-11-27   by: Stoeckley
! Maturity   : beta
! Purpose    : Maintain mute file information.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                            
!
!    This is a C-style object which maintains mute file information in
!    a hidden structure accessable through public functions.
!
!    This object also reads and writes CPS mute files to and from the
!    hidden structure.
!
!    This object is used by CBYT and by the CPS process MUTE.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS                        
!
! For each routine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!  If all of the arguments are INPUT, the flags may be omitted.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE                               
!-------------------------------------------------------------------------------
!  To create, clear, or destroy the data structure:
!
!                  ss = mutefile_create ()
!                 void  mutefile_clear  (ss)
!                  ss = mutefile_destroy(ss)
!
!  The entire structure is set to zero or blank by mutefile_create and
!    by mutefile_clear.
!  A NULL is returned from mutefile_create if unsuccessful.
!  A NULL is always returned from mutefile_destroy.
!  Memory pointed to by pointers in the structure is freed by
!    mutefile_clear and mutefile_destroy.
!
!-------------------------------------------------------------------------------
!  To get various items from the data structure:
!
!               nhx     = mutefile_get_nhx     (ss)
!               nhy     = mutefile_get_nhy     (ss)
!               nhz     = mutefile_get_nhz     (ss)
!
!   value = mutefile_get_terp_value     (ss, xbin, ybin, zbin)
!   value = mutefile_get_matching_value (ss, xbin, ybin, zbin)
!
!-------------------------------------------------------------------------------
!  To put various items into the data structure:
!
!    error = mutefile_set_nhx        (ss, nhx)
!    error = mutefile_set_nhy        (ss, nhy)
!    error = mutefile_set_nhz        (ss, nhz)
!
!-------------------------------------------------------------------------------
!  To check the validity of a CPS mute file:
!
!                                       i      i       o
!      valid = mutefile_check_validity (ss, filename, info)
!
!  char *filename = name of CPS mute file.
!  char     *info = information describing the file (blank unless valid = YES).
!  long     valid = whether the file is a valid file (YES or NO or MAYBE).
!
!  Returns valid = YES   if the file is a valid CPS mute file.
!  Returns valid = NO    if the file is NOT a valid CPS mute file.
!  Returns valid = MAYBE if the file cannot be opened for read.
!  Sets the header variables if YES is returned; otherwise sets them to zero.
!  The info argument can be NULL to prohibit return of the information.
!  See the documentation in inquire.c for more details.
!
!-------------------------------------------------------------------------------
!  To check the validity of a pair of CPS mute files:
!
!                                         i     i           i
!      void    mutefile_check_validities (ss, filename1, filename2,
!
!         &valid1, &valid2, info1, info2, &same_datasets)
!            o        o       o      o          o
!
!  char *filename1 = name of input  CPS mute file.
!  char *filename2 = name of output CPS mute file.
!  long     valid1 = whether the input  file is valid (YES or NO or MAYBE).
!  long     valid2 = whether the output file is valid (YES or NO or MAYBE).
!  char     *info1 = information describing the input  file.
!  char     *info2 = information describing the output file.
!  long same_datasets = TRUE if the two files appear to belong to the
!                         same datasets (i.e. ngrp and nch match).
!
!  Returns valid = YES   if the file is a valid CPS mute file.
!  Returns valid = NO    if the file is NOT a valid CPS mute file.
!  Returns valid = MAYBE if the file cannot be opened for read.
!  Sets the header variables if YES is returned; otherwise sets them to zero.
!  The info arguments can be NULL to prohibit return of the information.
!  See the documentation in inquire.c for more details.
!
!-------------------------------------------------------------------------------
!  To inquire about a pair of CPS mute files:
!
!                               i      i          i
!    status = mutefile_inquire (ss, filename1, filename2,
!
!      required1, required2, msg1, msg2, msg3, &status1, &status2)
!         i          i        o     o     o       o         o
!
!  char *filename1 = name of input CPS mute file.
!  char *filename2 = name of output CPS mute file.
!  long  required1 = TRUE if the input file is required.
!  long  required2 = TRUE if the output file is required.
!  char *msg1 = message referring to the status of the input file.
!  char *msg2 = message referring to the status of the output file.
!  char *msg3 = message referring to the two files together.
!  long status1 = the status of the input file (see below).
!  long status2 = the status of the output file (see below).
!  long  status = the status of the two files together (see below).
!
!  status1 = MUTEFILE_BLANK, MUTEFILE_VALID, MUTEFILE_ERROR.
!  status2 = MUTEFILE_BLANK, MUTEFILE_VALID, MUTEFILE_ERROR,
!            MUTEFILE_NOT_FOUND.
!  status  = MUTEFILE_CREATE, MUTEFILE_READ_ONLY, MUTEFILE_COPY,
!            MUTEFILE_UPDATE, MUTEFILE_ERROR.
!
!  See the documentation in inquire.c for more details.
!
!-------------------------------------------------------------------------------
!  To read or write picks associated with arbitrary traces:
!                                   b    i      i        i       o
!        void    mutefile_get_picks(ss, head, nwords, ntraces, picks)
!        void    mutefile_put_picks(ss, head, nwords, ntraces, picks)
!                                   b    i      i        i       i
!
!  MuteStruct *ss = pointer to opaque mute file data structure.
!  float   head[] = CPS trace headers (dimensioned nwords * ntraces).
!  long    nwords = number of words in each trace header.
!  long   ntraces = number of trace headers.
!  float  picks[] = pick times in seconds (dimensioned ntraces).
!
!  The headers can be any random mix of traces.
!
!-------------------------------------------------------------------------------
!  To read previous picks associated with arbitrary traces:
!  To read   next   picks associated with arbitrary traces:
!  To read selected picks associated with arbitrary traces:
!
!    void  mutefile_get_prev_picks(ss, head, nwords, ntraces, picks)
!    void  mutefile_get_next_picks(ss, head, nwords, ntraces, picks)
!    void  mutefile_get_sel_picks (ss, head, nwords, ntraces, picks)
!                                  b    i      i        i       i
!
!  MuteStruct *ss = pointer to opaque mute file data structure.
!  float   head[] = CPS trace headers (dimensioned nwords * ntraces).
!  long    nwords = number of words in each trace header.
!  long   ntraces = number of trace headers.
!  float  picks[] = pick times in seconds (dimensioned ntraces).
!
!  These functions work like mutefile_get_picks, except for the following
!    differences:
!  The returned picks correspond to the gather:
!     prior to that in the trace headers for mutefile_get_prev_picks.
!     after    that in the trace headers for mutefile_get_next_picks.
!     given by ysel and zsel             for mutefile_get_sel_picks.
!  The returned picks are matched up by offset (X-coordinate).
!
!-------------------------------------------------------------------------------
!                   TO CALL THIS PRIMITIVE FROM FORTRAN
!              (primarily to be called from the MUTE process)
!
!              o                                  i
!         mute_pointer = mutefile_wrapper_open (buffer)
!
!              o                                     i         i
!         mutevalue    = mutefile_wrapper_get  (mute_pointer,hd(:,k))
!
!                                                    b
!         call           mutefile_wrapper_kill (mute_pointer)
!
! integer,dimension(11)     :: buffer ... a hollerith character to assist
!                                         with communications between
!                                         fortran and c.
! C_POINTER                 :: mute_pointer
! real                      :: mutevalue
! double precision          :: hd(:,:)
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                             
!
!  MuteStruct  *ss = pointer to opaque mute file structure.
!  char *filename or *filename1 = name of input mute file.
!  char *filename or *filename2 = name of output mute file.
!  long  required1 = TRUE if input file is required, FALSE otherwise.
!  long  required2 = TRUE if output file is required, FALSE otherwise.
!
!  long   nhx = header word for the X-coord (usually offset).
!  long   nhy = header word for the Y-coord (usually inline bin #).
!  long   nhz = header word for the Z-coord (usually crossline bin #).
!
!  float    value = a single mute value (in seconds).
!
!  int   error = returned error (zero if no error, non-zero if error).
!  long status = returned status (see specific documentation below).
!  char   *msg = returned message corresponding with returned status.
!
!  All char* types must be null-terminated.
!  The filename must include the extension (if it has one), and may
!    optionally include a path.
!  The X-coordinate changes fastest in the array of mute values.
!  The Z-coordinate changes slowest in the array of mute values.
!
!  A special mute value is a nil value (-1.0E-30), defined as FNIL in
!  named_constants.h and returned by the function mutefile_get_nil().
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                              
!
!     Date        Author       Description
!     ----        ------       -----------
! 10. 2007-11-27  Stoeckley    Change mutefile_wrapper_open to return void and
!                               to return pointer in the argument list.
!  9. 2005-05-31  Stoeckley    fix to compile with C++.
!  8. 2002-01-02  Stoeckley    Changes for compatibility with ~spws (to
!                               eliminate duplicate code).
!  7. 2001-10-18  Stoeckley    Allow header word numbers > 64.
!  6. 2001-02-01  Stoeckley    Change called function read_card to readcard;
!                               add include files inquire.h and readcard.h;
!                               modify code to remove compiler warning messages.
!  5. 1999-12-21  Stoeckley    Remove static variable nil which was set to
!                               FNIL to comply with change in named_constants.h
!                               to make this more robust.
!  4. 1999-11-17  Stoeckley    Incorporate the MUTEFILE_WRAPPER into this
!                               primitive.  Also add ident string for RCS.
!  3. 1999-09-10  O'Brien      Added documentation during CPS conversion.
!  2. 1996-12-13  Goodger      Inserted PROG DOC cards for use in creating
!                               a CPS manual.  Did not recompile.
!  1. 1994-11-30  Stoeckley    Initial version.
!
!
! Revision history of MUTEFILE_WRAPPER:
!
!  2. 1999-09-07  O'Brien      Added documentation during conversion
!                               from old system.
!  1. 19??-??-??  Stoeckley    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>
****/


/*---------------------------- start of module -----------------------------*/
/*---------------------------- start of module -----------------------------*/
/*---------------------------- start of module -----------------------------*/


char MUTEFILE_IDENT[100] =
"$Id: mutefile.c,v 1.10 2007/11/28 14:56:19 Stoeckley beta sps $";


#include "mutefile.h"
#include "named_constants.h"
#include "readcard.h"
#include "inquire.h"
#include "c2f_interface.h"
#include "str.h"
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>


/*
#define DEBUG TRUE
*/
#define DEBUG FALSE


static float ZZERO = 0.0;
static float XTOL  = 0.5;
static float YTOL  = 0.5;
static float ZTOL  = 0.5;

#ifdef __cplusplus
extern "C" {
#endif

/*----------- create or clear or destroy data structure ---------------*/
/*----------- create or clear or destroy data structure ---------------*/
/*----------- create or clear or destroy data structure ---------------*/


MuteStruct *mutefile_create(void)
{
  MuteStruct *ss = (MuteStruct*)malloc(sizeof(MuteStruct));
  if(!ss) return NULL;
  ss->xyzdim    = xyzdim_create(XTOL, YTOL, ZTOL);
  ss->nhx       = 6;
  ss->nhy       = 46;
  ss->nhz       = 0;
  ss->ylatest   = ZZERO;
  ss->zlatest   = ZZERO;
  ss->ysel      = ZZERO;
  ss->zsel      = ZZERO;
  ss->yz_switch = FALSE;
  ss->interp    = FALSE;
  return ss;
}


void mutefile_clear(MuteStruct *ss)
{
  xyzdim_clear(ss->xyzdim);
  ss->ylatest   = ZZERO;
  ss->zlatest   = ZZERO;
  ss->ysel      = ZZERO;
  ss->zsel      = ZZERO;
  ss->yz_switch = FALSE;
  ss->interp    = FALSE;
}



MuteStruct *mutefile_destroy(MuteStruct *ss)
{
  if(!ss) return NULL;
  xyzdim_destroy(ss->xyzdim);
  free(ss);
  return NULL;
}



/*------------------ get various quantities --------------------*/
/*------------------ get various quantities --------------------*/
/*------------------ get various quantities --------------------*/

float mutefile_get_nil      (void)           { return FNIL; }
long  mutefile_get_nz       (MuteStruct *ss);
long  mutefile_get_ny       (MuteStruct *ss, long iz);
long  mutefile_get_nx       (MuteStruct *ss, long iy, long iz);

long  mutefile_get_nhx      (MuteStruct *ss) { return ss->nhx; }
long  mutefile_get_nhy      (MuteStruct *ss) { return ss->nhy; }
long  mutefile_get_nhz      (MuteStruct *ss) { return ss->nhz; }

float mutefile_get_ylatest  (MuteStruct *ss) { return ss->ylatest; }
float mutefile_get_zlatest  (MuteStruct *ss) { return ss->zlatest; }
float mutefile_get_ysel     (MuteStruct *ss) { return ss->ysel; }
float mutefile_get_zsel     (MuteStruct *ss) { return ss->zsel; }
int   mutefile_get_yz_switch(MuteStruct *ss) { return ss->yz_switch; }
int   mutefile_get_interp   (MuteStruct *ss) { return ss->interp; }

float mutefile_get_xmin (MuteStruct *ss) { return xyzdim_get_xmin(ss->xyzdim); }
float mutefile_get_ymin (MuteStruct *ss) { return xyzdim_get_ymin(ss->xyzdim); }
float mutefile_get_zmin (MuteStruct *ss) { return xyzdim_get_zmin(ss->xyzdim); }
float mutefile_get_xmax (MuteStruct *ss) { return xyzdim_get_xmax(ss->xyzdim); }
float mutefile_get_ymax (MuteStruct *ss) { return xyzdim_get_ymax(ss->xyzdim); }
float mutefile_get_zmax (MuteStruct *ss) { return xyzdim_get_zmax(ss->xyzdim); }


  
/*---------------- set nhx or nhy or nhz --------------------------*/
/*---------------- set nhx or nhy or nhz --------------------------*/
/*---------------- set nhx or nhy or nhz --------------------------*/


int mutefile_set_nhx(MuteStruct *ss, long nhx)
{
  if(nhx < 1) return 1;
  if(nhx == ss->nhy) return 1;
  if(nhx == ss->nhz) return 1;
  ss->nhx = nhx;
  return 0;
}


int mutefile_set_nhy(MuteStruct *ss, long nhy)
{
  if(nhy < 0) return 1;
  if(nhy == ss->nhx) return 1;
  if(nhy == ss->nhz &&     nhy != 0) return 1;
  if(nhy == 0       && ss->nhz != 0) return 1;
  ss->nhy = nhy;
  return 0;
}


int mutefile_set_nhz(MuteStruct *ss, long nhz)
{
  if(nhz < 0) return 1;
  if(nhz == ss->nhx) return 1;
  if(nhz == ss->nhy &&     nhz != 0) return 1;
  if(nhz != 0       && ss->nhy == 0) return 1;
  ss->nhz = nhz;
  return 0;
}



/*---------------- set ysel or zsel -------------------------------*/
/*---------------- set ysel or zsel -------------------------------*/
/*---------------- set ysel or zsel -------------------------------*/

/*
   If directional is TRUE, adjustment is made to the nearest bin
          at a location in the direction of the specified bin.
   If directional is FALSE, adjustment is made to the nearest bin,
          regardless of the direction from the specified bin.
   Returns TRUE if a change was made, and FALSE otherwise.
*/

int mutefile_set_ysel(MuteStruct *ss, float ysel, int directional)
{
  float keep = ss->ysel;
  long adjustment = 0;
  ysel = (float)NearestInteger(ysel);
  if(directional && ysel > ss->ysel) adjustment =  1;
  if(directional && ysel < ss->ysel) adjustment = -1;
  ss->ysel = xyzdim_get_nearest_y(ss->xyzdim, ysel, ss->zsel, adjustment);
  if(ss->ysel == FNIL) ss->ysel = ZZERO;
  return (ss->ysel != keep);
}


int mutefile_set_zsel(MuteStruct *ss, float zsel, int directional)
{
  float keep = ss->zsel;
  long adjustment = 0;
  zsel = (float)NearestInteger(zsel);
  if(directional && zsel > ss->zsel) adjustment =  1;
  if(directional && zsel < ss->zsel) adjustment = -1;
  ss->zsel = xyzdim_get_nearest_z(ss->xyzdim, ss->ysel, zsel, adjustment);
  if(ss->zsel == FNIL) ss->zsel = ZZERO;
  return (ss->zsel != keep);
}


 
/*---------------- set yz switch ----------------------------------*/
/*---------------- set yz switch ----------------------------------*/
/*---------------- set yz switch ----------------------------------*/

    /* returns TRUE if a change is made, and FALSE otherwise */

int mutefile_set_yz_switch(MuteStruct *ss, int yz_switch)
{
  if(yz_switch == ss->yz_switch) return FALSE;
  ss->yz_switch = yz_switch;
  return TRUE;
}



/*---------------- set interp -------------------------------------*/
/*---------------- set interp -------------------------------------*/
/*---------------- set interp -------------------------------------*/

    /* returns TRUE if a change is made, and FALSE otherwise */

int mutefile_set_interp(MuteStruct *ss, int interp)
{
  if(interp == ss->interp) return FALSE;
  ss->interp = interp;
  return TRUE;
}




/*---------------------- make grid ---------------------------*/
/*---------------------- make grid ---------------------------*/
/*---------------------- make grid ---------------------------*/

/*
      Replaces the current contents of the object with a regular
      grid of interpolated values.  Primarily for testing, or
      for display in a generic plotting program.  The source
      information is in MuteStruct *source.

      The arguments xinc, yinc, and zinc must be > 0.0.
      If any of the arguments xmin, ymin, zmin, xmax, ymax, zmax
      are nil, they are replaced by the minimum or maximum in the
      source object.  The source object is unchanged.
*/

void mutefile_make_grid(MuteStruct *ss,
                        float xmin, float ymin, float zmin,
                        float xmax, float ymax, float zmax,
                        float xinc, float yinc, float zinc,
                        MuteStruct *source)
{
  long ix, iy, iz;
  long nx, ny, nz;
  mutefile_clear(ss);
  if(xmin == FNIL) xmin = mutefile_get_xmin(source);
  if(ymin == FNIL) ymin = mutefile_get_ymin(source);
  if(zmin == FNIL) zmin = mutefile_get_zmin(source);
  if(xmax == FNIL) xmax = mutefile_get_xmax(source);
  if(ymax == FNIL) ymax = mutefile_get_ymax(source);
  if(zmax == FNIL) zmax = mutefile_get_zmax(source);
  assert(xinc > 0.0);
  assert(yinc > 0.0);
  assert(zinc > 0.0);
  nx = NearestInteger( (xmax - xmin) / xinc ) + 1;
  ny = NearestInteger( (ymax - ymin) / yinc ) + 1;
  nz = NearestInteger( (zmax - zmin) / zinc ) + 1;
  if(xmin + (nx - 1) * xinc < xmax) nx++;
  if(ymin + (ny - 1) * yinc < ymax) ny++;
  if(zmin + (nz - 1) * zinc < zmax) nz++;
  if(xmin + (nx - 1) * xinc < xmax) nx++;
  if(ymin + (ny - 1) * yinc < ymax) ny++;
  if(zmin + (nz - 1) * zinc < zmax) nz++;
  for(iz = 0; iz < nz; iz++) { float z = zmin + iz * zinc;
  for(iy = 0; iy < ny; iy++) { float y = ymin + iy * yinc;
  for(ix = 0; ix < nx; ix++) { float x = xmin + ix * xinc;
          float v = mutefile_get_terp_value(source, x, y, z);
          mutefile_insert_point(ss, x, y, z, v);
  } } }
}



/*------------------ get terp value --------------------------------*/
/*------------------ get terp value --------------------------------*/
/*------------------ get terp value --------------------------------*/

                /* needed on the cray */

float mutefile_get_terp_value(MuteStruct *ss, float x, float y, float z)
{
  if(x != FNIL) x = (float)NearestInteger(x);
  if(y != FNIL) y = (float)NearestInteger(y);
  if(z != FNIL) z = (float)NearestInteger(z);
  return xyzdim_get_terp_value(ss->xyzdim, x, y, z);
}



/*------------------------- get value ------------------------*/
/*------------------------- get value ------------------------*/
/*------------------------- get value ------------------------*/


float mutefile_get_value(MuteStruct *ss, float x, float y, float z)
{
  if(x != FNIL) x = (float)NearestInteger(x);
  if(y != FNIL) y = (float)NearestInteger(y);
  if(z != FNIL) z = (float)NearestInteger(z);
  return xyzdim_get_value(ss->xyzdim, x, y, z);
}



/*-------------------- insert range or point --------------------*/
/*-------------------- insert range or point --------------------*/
/*-------------------- insert range or point --------------------*/

int mutefile_insert_range(MuteStruct *ss, float x1, float v1, 
                                          float x2, float v2,
                                          float y, float z)
{
  if(x1 != FNIL) x1 = (float)NearestInteger(x1);
  if(x2 != FNIL) x2 = (float)NearestInteger(x2);
  if(y  != FNIL) y  = (float)NearestInteger(y);
  if(z  != FNIL) z  = (float)NearestInteger(z);
  return xyzdim_insert_range(ss->xyzdim, x1, v1, x2, v2, y, z);
}



int mutefile_insert_point(MuteStruct *ss, float x, float y, float z, float v)
{
  x = (float)NearestInteger(x);
  y = (float)NearestInteger(y);
  z = (float)NearestInteger(z);
  return xyzdim_insert_point(ss->xyzdim, x, y, z, v);
}



/*-------------- get x or y or z from header ----------------*/
/*-------------- get x or y or z from header ----------------*/
/*-------------- get x or y or z from header ----------------*/

/*
   nhy = header 46 = index 45 = source   sequential ground position
         header 47 = index 46 = receiver sequential ground position

   nhy = header 33 = index 32 = source   X grid ground position
         header 35 = index 34 = receiver X grid ground position

   nhz = header 34 = index 33 = source   Y grid ground position
         header 36 = index 35 = receiver Y grid ground position

          If nhx = 6 and nhy = 46 and nhz = 0:
             - forces x negative if header 47 <  header 46.
             - forces x positive if header 47 >= header 46.
          If nhx = 6 and nhy = 33 and nhz = 34:
             - forces x negative if header 35 <  header 33.
             - forces x positive if header 35 >= header 33.
*/

float mutefile_get_x_from_header(MuteStruct *ss, const float *head)
{
  float x = head[ss->nhx - 1];
  if(ss->nhx == 6 && ss->nhy == 46 && ss->nhz == 0)
       {
       if (head[46] < head[45]) x = -AbsoluteValue(x);
       else                     x =  AbsoluteValue(x);
       }
  else if(ss->nhx == 6 && ss->nhy == 33 && ss->nhz == 34)
       {
       if (head[34] < head[32]) x = -AbsoluteValue(x);
       else                     x =  AbsoluteValue(x);
       }
  return (float)NearestInteger(x);
}


float mutefile_get_y_from_header(MuteStruct *ss, const float *head)
{
  float y = ZZERO;
  if(ss->nhy != 0) y = head[ss->nhy - 1];
  return (float)NearestInteger(y);
}


float mutefile_get_z_from_header(MuteStruct *ss, const float *head)
{
  float z = ZZERO;
  if(ss->nhz != 0) z = head[ss->nhz - 1];
  return (float)NearestInteger(z);
}


              /******************************************/


static float mutefile_static_get_x(MuteStruct *ss, const DOUBLE *head)
{
  float x = (float)head[ss->nhx - 1];
  if(ss->nhx == 6 && ss->nhy == 46 && ss->nhz == 0)
       {
       if (head[46] < head[45]) x = -AbsoluteValue(x);
       else                     x =  AbsoluteValue(x);
       }
  else if(ss->nhx == 6 && ss->nhy == 33 && ss->nhz == 34)
       {
       if (head[34] < head[32]) x = -AbsoluteValue(x);
       else                     x =  AbsoluteValue(x);
       }
  return (float)NearestInteger(x);
}


static float mutefile_static_get_y(MuteStruct *ss, const DOUBLE *head)
{
  float y = ZZERO;
  if(ss->nhy != 0) y = (float)head[ss->nhy - 1];
  return (float)NearestInteger(y);
}


static float mutefile_static_get_z(MuteStruct *ss, const DOUBLE *head)
{
  float z = ZZERO;
  if(ss->nhz != 0) z = (float)head[ss->nhz - 1];
  return (float)NearestInteger(z);
}



/*------------------- restrict range (static) ------------------*/
/*------------------- restrict range (static) ------------------*/
/*------------------- restrict range (static) ------------------*/

       /* restrict trace range to be the largest consecutive
          set of traces which have the same Y and Z coords */

       /* uses header 9 if header Y is 46 or 33 */

static void mutefile_restrict_range(MuteStruct *ss,
                     const float *head, long nwords,
                     long n, long *index1, long *index2)
{
  long first_index = -999, last_index = -999;
  float active_y = -999.9, active_z = -999.9;
  long i, largest_number = 0;
  for(i = 0; i < n; i++)
      {
      float y = mutefile_get_y_from_header (ss, &head[nwords * i]);
      float z = mutefile_get_z_from_header (ss, &head[nwords * i]);
      if(ss->nhy == 46 || ss->nhy == 33)
          {
          y = head[nwords * i + 8];
          y = (float)NearestInteger(y);
          }
      if(i == 0)
          {
          active_y = y;
          active_z = z;
          first_index = i;
          last_index = i;
          }
      else if(y == active_y && z == active_z)
          {
          last_index = i;
          }
      else if(last_index - first_index + 1 > largest_number)
          {
          *index1 = first_index;
          *index2 = last_index;
          largest_number = last_index - first_index + 1;
          active_y = y;
          active_z = z;
          first_index = i;
          last_index = i;
          }
      else
          {
          active_y = y;
          active_z = z;
          first_index = i;
          last_index = i;
          }
      }
  if(last_index - first_index + 1 > largest_number)
      {
      *index1 = first_index;
      *index2 = last_index;
      }
}



/*--------------------- move picks (static) --------------------*/
/*--------------------- move picks (static) --------------------*/
/*--------------------- move picks (static) --------------------*/

#define CURR  1  /* get current  picks */
#define PREV  2  /* get previous picks */
#define NEXT  3  /* get next     picks */
#define SEL   4  /* get selected picks */

        /* yz_switch == FALSE if want prev or next Y bin */
        /* yz_switch == TRUE  if want prev or next Z bin */

static void mutefile_move_picks(MuteStruct *ss,
                     const float head[], long nwords,
                     long ntraces, float picks[], int option)
{
  int i;
  float x, y, z;

  for(i = 0; i < ntraces; i++)
       {
       x = mutefile_get_x_from_header(ss, &head[nwords * i]);
       y = mutefile_get_y_from_header(ss, &head[nwords * i]);
       z = mutefile_get_z_from_header(ss, &head[nwords * i]);
       if(option == PREV)
           {
           if(ss->yz_switch) z = xyzdim_get_prev_z(ss->xyzdim, y, z);
           else              y = xyzdim_get_prev_y(ss->xyzdim, y, z);
           }
       else if(option == NEXT)
           {
           if(ss->yz_switch) z = xyzdim_get_next_z(ss->xyzdim, y, z);
           else              y = xyzdim_get_next_y(ss->xyzdim, y, z);
           }
       else if(option == SEL)
           {
           z = ss->zsel;
           y = ss->ysel;
           }
       if(ss->interp) picks[i] = mutefile_get_terp_value(ss, x, y, z);
       else           picks[i] = mutefile_get_value     (ss, x, y, z);
       }
}



/*----------------- get or put picks for cbyt -------------------------*/
/*----------------- get or put picks for cbyt -------------------------*/
/*----------------- get or put picks for cbyt -------------------------*/

void mutefile_get_picks(MuteStruct *ss, const float head[], long nwords,
        long ntraces, float picks[])
{
  mutefile_move_picks(ss, head, nwords, ntraces, picks, CURR);
}



void mutefile_put_picks(MuteStruct *ss, const float head[], long nwords,
        long ntraces, float picks[])
{
  float x1, x2, y, z, time1, time2;
  long index1, index2;
  mutefile_restrict_range(ss, head, nwords, ntraces, &index1, &index2);
  x1 = mutefile_get_x_from_header (ss, &head[nwords * index1]);
  x2 = mutefile_get_x_from_header (ss, &head[nwords * index2]);
  y  = mutefile_get_y_from_header (ss, &head[nwords * index1]);
  z  = mutefile_get_z_from_header (ss, &head[nwords * index1]);
  time1 = picks[index1];
  time2 = picks[index2];
  mutefile_insert_range(ss, x1, time1, x2, time2, y, z);
  ss->ylatest = y;
  ss->zlatest = z;
}



void mutefile_get_prev_picks(MuteStruct *ss, const float head[], long nwords,
        long ntraces, float picks[])
{
  mutefile_move_picks(ss, head, nwords, ntraces, picks, PREV);
}



void mutefile_get_next_picks(MuteStruct *ss, const float head[], long nwords,
        long ntraces, float picks[])
{
  mutefile_move_picks(ss, head, nwords, ntraces, picks, NEXT);
}



void mutefile_get_sel_picks (MuteStruct *ss, const float head[], long nwords,
        long ntraces, float picks[])
{
  mutefile_move_picks(ss, head, nwords, ntraces, picks, SEL);
}




/*-------------- prepare data for cbyt --------------------------------*/
/*-------------- prepare data for cbyt --------------------------------*/
/*-------------- prepare data for cbyt --------------------------------*/

     /* there is no direct access to ss variables in this section */

int mutefile_prepare (MuteStruct *ss, char *filename1, char *filename2,
                      void (*msgfun)(void *data, char *msg),
                      void *msgdata, long status, char *msg)
{
  long e;

  if(status == MUTEFILE_CREATE)
       {
       if(msgfun) msgfun(msgdata, "creating mute file...");
       mutefile_clear(ss);
       e = mutefile_save_file(ss, filename2, msg);     if(e) goto error;
       }
  else if(status == MUTEFILE_COPY)
       {
       if(msgfun) msgfun(msgdata, "copying mute file...");
       e = mutefile_read_file(ss, filename1, msg);     if(e) goto error;
       e = mutefile_save_file(ss, filename2, msg);     if(e) goto error;
       }
  else if(status == MUTEFILE_READ_ONLY)
       {
       if(msgfun) msgfun(msgdata, "reading mute file...");
       e = mutefile_read_file(ss, filename1, msg);     if(e) goto error;
       }
  else if(status == MUTEFILE_UPDATE)
       {
       if(msgfun) msgfun(msgdata, "reading mute file...");
       e = mutefile_read_file(ss, filename1, msg);     if(e) goto error;
       }
  else
       {
       e = 777;  if(msg) strcpy(msg, "illegal status");  goto error;
       }
  if(msgfun) msgfun(msgdata, " ");
  return 0;

  error:
     if(msgfun) msgfun(msgdata, " ");
     mutefile_clear(ss);
     return e;         
}



/*----------- read or save start of file (static) -------------------*/
/*----------- read or save start of file (static) -------------------*/
/*----------- read or save start of file (static) -------------------*/

#define TITLE1      "### CPS 3D MUTE FILE\n"
#define TITLE2      "### CPS 3D MUTE FILE\n"
#define SAVED2      "### file saved: %s\n"
#define HEADERS1    "### offset, inline, crossline headers:  %ld  %ld  %ld\n"
#define HEADERS2    "### offset, inline, crossline headers:  %d  %d  %d\n"
#define LATEST1     "### latest inline and crossline updated:  %f  %f\n"
#define LATEST2     "### latest inline and crossline updated:  %f  %f\n"
#define COLUMNS1    "###   OFFSET        INLINE        CROSSLINE        TIME\n"
#define COLUMNS2    "###   OFFSET        INLINE        CROSSLINE        TIME\n"


static int mutefile_read_start_of_file(MuteStruct *ss, FILE *stream)
{
  long  nhx, nhy, nhz;
  float ylatest, zlatest;
  int e;
  char *card;

  e = fscanf(stream, TITLE1);
                                                    if(e != 0) return 1;
  card = readcard(stream);
                                                     if(!card) return 2;
  e = fscanf(stream, HEADERS1, &nhx, &nhy, &nhz); 
                                                    if(e != 3) return 3;
  e = fscanf(stream, LATEST1, &ylatest, &zlatest);
                                                    if(e != 2) return 4;
  e = fscanf(stream, COLUMNS1);
                                                    if(e != 0) return 5;
  e = mutefile_set_nhx(ss, nhx);
                                                         if(e) return 6;
  e = mutefile_set_nhy(ss, nhy);
                                                         if(e) return 7;
  e = mutefile_set_nhz(ss, nhz);
                                                         if(e) return 8;
  ss->ylatest = ylatest;
  ss->zlatest = zlatest;
  return 0;
}



static int mutefile_save_start_of_file(MuteStruct *ss, FILE *stream)
{
  const char *buffer  = str_time_date();
  int   nhx     = (int)mutefile_get_nhx(ss);
  int   nhy     = (int)mutefile_get_nhy(ss);
  int   nhz     = (int)mutefile_get_nhz(ss);
  float ylatest = mutefile_get_ylatest(ss);
  float zlatest = mutefile_get_zlatest(ss);
  int e;

  e = fprintf(stream, TITLE2);                    if(e < 0) return 1;
  e = fprintf(stream, SAVED2, buffer);            if(e < 0) return 2;
  e = fprintf(stream, HEADERS2, nhx, nhy, nhz);   if(e < 0) return 3;
  e = fprintf(stream, LATEST2, ylatest, zlatest); if(e < 0) return 4;
  e = fprintf(stream, COLUMNS2);                  if(e < 0) return 5;
  return 0;
}



/*---------------------- save file ----------------------------*/
/*---------------------- save file ----------------------------*/
/*---------------------- save file ----------------------------*/


#define SERR(EEE,SSS)                                              \
    {                                                              \
    if(stream) fclose(stream);                                     \
    if(msg) sprintf(msg,                                           \
             "error %d\ntrying to save\nmute file %s", EEE, SSS);  \
    return 1;                                                      \
    }
                  

int mutefile_save_file(MuteStruct *ss, char *filename, char *msg)
{
  FILE *stream = NULL;
  int e;

  stream = fopen(filename, "w");       if(stream == NULL) SERR(1,"(bad open)")
  e = mutefile_save_start_of_file(ss, stream);      if(e) SERR(e,"headers");
  e = xyzdim_write_to_file(ss->xyzdim, stream);     if(e) SERR(e,"values");
  fclose(stream);
  if(msg) strcpy(msg, " ");
  return 0;
}



/*-------------------- read file ------------------------------*/
/*-------------------- read file ------------------------------*/
/*-------------------- read file ------------------------------*/

#define RERR(EEE,SSS)                                            \
    {                                                            \
    if(stream) fclose(stream);                                   \
    mutefile_clear(ss);                                          \
    if(msg) sprintf(msg,                                         \
           "error %d\ntrying to read\nmute file %s", EEE, SSS);  \
    return 1;                                                    \
    }
                  

int mutefile_read_file(MuteStruct *ss, char *filename, char *msg)
{
  FILE *stream = NULL;
  int e;

  mutefile_clear(ss);
  stream = fopen(filename, "r");        if(stream == NULL) RERR(1,"(bad open)")
  e = mutefile_read_start_of_file(ss, stream);       if(e) RERR(e,"headers")
  e = xyzdim_read_from_file(ss->xyzdim, stream);     if(e) RERR(e,"values")
  fclose(stream);
  if(msg) strcpy(msg, " ");
  mutefile_set_ysel(ss, ss->ysel, FALSE);
  mutefile_set_zsel(ss, ss->zsel, FALSE);
  return 0;
}




/*----------------- check validity of one file ------------------------*/
/*----------------- check validity of one file ------------------------*/
/*----------------- check validity of one file ------------------------*/


#define VERR                 \
    {                        \
    mutefile_clear(ss);      \
    fclose(stream);          \
    if(DEBUG) printf("error %d validating file\n", e);  \
    return NO;               \
    }
                  
                  
long mutefile_check_validity (MuteStruct *ss, char *filename, char *info)
{
  FILE *stream;
  int e;

  mutefile_clear(ss);
  stream = fopen(filename, "r");       if(stream == NULL) return NO;
  e = mutefile_read_start_of_file(ss, stream);           if(e) VERR;
  if(info)
       {
       sprintf(info, "headers %d %d %d", (int)mutefile_get_nhx(ss),
                                         (int)mutefile_get_nhy(ss),
                                         (int)mutefile_get_nhz(ss));
       }
  fclose(stream);
  return YES;
}




/*----------------- check validity of both files ----------------------*/
/*----------------- check validity of both files ----------------------*/
/*----------------- check validity of both files ----------------------*/


void mutefile_check_validities (MuteStruct *ss,
                                char *filename1, char *filename2,
                                long    *valid1, long    *valid2,
                                char     *info1,     char *info2,
                                long *same_datasets)
{
  long nhx, nhy, nhz;

  *valid2 = mutefile_check_validity(ss, filename2, info2);
  nhx     = mutefile_get_nhx(ss);
  nhy     = mutefile_get_nhy(ss);
  nhz     = mutefile_get_nhz(ss);
  *valid1 = mutefile_check_validity(ss, filename1, info1);
  *same_datasets = (  nhx == mutefile_get_nhx(ss) &&
                      nhy == mutefile_get_nhy(ss) &&
                      nhz == mutefile_get_nhz(ss) ); 
}



/*----------------- inquire -------------------------------------------*/
/*----------------- inquire -------------------------------------------*/
/*----------------- inquire -------------------------------------------*/


long mutefile_inquire (MuteStruct *ss,
                       char *filename1, char *filename2,
                       long  required1, long  required2,
                       char      *msg1, char      *msg2, char *msg3,
                       long   *status1, long   *status2)
{
  long same_datasets, status, valid1, valid2;
  char info1[100], info2[100];
  static char filetype[] = "CPS 3D mute file";

  mutefile_check_validities(ss, filename1, filename2,
              &valid1, &valid2, info1, info2, &same_datasets);
  status = inquire_files_combo(filename1, filename2,
              filetype, required1, required2, valid1, valid2,
              info1, info2, same_datasets, status1, status2,
              msg1, msg2, msg3);
  return status;
}


/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/
/*------------------ fortran-callable wrapper --------------------------*/

            /* called from fortran CPS process MUTE */


/*---------------- spelling adjustments for fortran -----------------*/
/*---------------- spelling adjustments for fortran -----------------*/
/*---------------- spelling adjustments for fortran -----------------*/


#ifdef NEED_UNDERSCORE
#define mutefile_wrapper_open     mutefile_wrapper_open_
#define mutefile_wrapper_get      mutefile_wrapper_get_
#define mutefile_wrapper_kill     mutefile_wrapper_kill_
#elif defined NEED_CAPITALS
#define mutefile_wrapper_open     MUTEFILE_WRAPPER_OPEN 
#define mutefile_wrapper_get      MUTEFILE_WRAPPER_GET    
#define mutefile_wrapper_kill     MUTEFILE_WRAPPER_KILL     
#endif


/*---------------------- mutefile_wrapper_open --------------------------*/
/*---------------------- mutefile_wrapper_open --------------------------*/
/*---------------------- mutefile_wrapper_open --------------------------*/


void mutefile_wrapper_open(char *filename, void* *mute_pointer)
{
  MuteStruct *ss = mutefile_create();
  if(ss)
      {
      int e;
      e = mutefile_read_file(ss, filename, NULL);
      if(e) ss = mutefile_destroy(ss);
      }
  *mute_pointer = (void*)ss;
}

/*---------------------- mutefile_wrapper_kill --------------------------*/
/*---------------------- mutefile_wrapper_kill --------------------------*/
/*---------------------- mutefile_wrapper_kill --------------------------*/


void mutefile_wrapper_kill(void* *mute_pointer)
{
  MuteStruct *ss = (MuteStruct*)(*mute_pointer);
  ss = mutefile_destroy(ss);
}

/*------------------ mutefile_wrapper_get -------------------------*/
/*------------------ mutefile_wrapper_get -------------------------*/
/*------------------ mutefile_wrapper_get -------------------------*/


REAL mutefile_wrapper_get(void* *mute_pointer, const DOUBLE *hd)
{
  MuteStruct *ss = (MuteStruct*)(*mute_pointer);
/*
  float x = mutefile_get_x_from_header(ss, hd);
  float y = mutefile_get_y_from_header(ss, hd);
  float z = mutefile_get_z_from_header(ss, hd);
*/
  float x = mutefile_static_get_x(ss, hd);
  float y = mutefile_static_get_y(ss, hd);
  float z = mutefile_static_get_z(ss, hd);
  return mutefile_get_terp_value(ss, x, y, z);
}

#ifdef __cplusplus
}
#endif

/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/

