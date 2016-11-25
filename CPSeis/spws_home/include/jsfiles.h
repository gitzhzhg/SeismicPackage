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
/****
!<CPS_v1 type="HEADER_FILE"/>
****/
/*------------------------------ jsfiles.h ---------------------------------*/
/*------------------------------ jsfiles.h ---------------------------------*/
/*------------------------------ jsfiles.h ---------------------------------*/

    /* other files are:  jsfiles.c */
 
/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                      C P S   H E A D E R   F I L E
!
! Name       : jsfiles
! Category   : io
! Written    : 2006-08-22   by: Corn
! Revised    : 2008-11-06   by: Corn
! Maturity   : beta
! Purpose    : C class used to open multiple JavaSeisWrapper classes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2008-11-06  CORN         Added settracenumber & gettracenumber
!  4. 2008-10-14  CORN         Explicitly made argment in *delete void.
!  3. 2008-09-02  CORN         Added settracenumber
!  2. 2008-08-21  CORN         Added output capability.
!  1. 2006-08-22  Corn         Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _JSFILES_H_
#define _JSFILES_H_

#include <jni.h>

#define JSEIS_TYPE 19 /* see trciof77.h */

#ifdef __cplusplus
extern "C" {
#endif


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/



int jsfiles_create                     /* create JSFILES instance         */
  (int *alloc);                        /*   given number of JavaSeisFiles */

int jsfiles_getlun                     /* create a JavaSeisFile - rtn which */
  (const char *filename,               /*   given JavaSeisFile name       */
   const char *rw);                    /*   given rw flag: "r" or "rw"    */

int jsfiles_open                       /* open a JavaSeisFile - rtn ok    */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_close                      /* Close a JavaSeisFile given      */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_isa                        /* return 0 if not a JavaSeisFile  */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_status                     /* return 0 if NORMAL status       */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_message                    /* return JavaSeisWrapper msg size */
  (int which,                          /*   which JavaseisFile            */
   char *mess,                         /*   preallocated array to be popul*/
   int max_size);                      /*   maximum size of given array   */

int jsfiles_wtype                      /* return wtype size               */
  (int which,                          /*   which JavaseisFile            */
   char *wtype,                        /*   preallocated array to be popul*/
   int max_size);                      /*   maximum size of given array   */

long jsfiles_gettracecount             /* return trace count              */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_getnumdimensions           /* return number of dimensions     */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_getsamplecount             /* return sample count             */
  (int which);                         /*   which JavaseisFile            */

float jsfiles_getsamplerate            /* return sample rate in seconds   */
  (int which);                         /*   which JavaseisFile            */

double jsfiles_getlav                  /* return largest abs vlu estimate */
  (int which);                         /*   which JavaseisFile            */

double jsfiles_getstarttimeinsecs      /* return trace start time (sec)   */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_getheadercount             /* return number of header words   */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_gettrace                   /* return size of trace populated  */
  (int which,                          /*   which JavaseisFile            */
   float *trace,                       /*   preallocated array to be popul*/
   int max_size);                      /*   maximum size of given array   */

int jsfiles_getheaders                 /* rtn number of headers populated */
  (int which,                          /*   which JavaseisFile            */
   double *headers,                    /*   preallocated array to be popul*/
   int max_size);                      /*   maximum size of given array   */

int jsfiles_puttrace                   /* return no of trac smpls written */
  (int which,                          /*   which JavaseisFile            */
   float *trace,                       /*   trace array to write          */
   int size);                          /*   size of given array           */

int jsfiles_putheaders                 /* return no of hdr values written */
  (int which,                          /*   which JavaseisFile            */
   double *headers,                    /*   header array to write         */
   int size);                          /*   size of given array           */

int jsfiles_settracenumber             /* return 1 if trace number set    */
  (int which,                          /*   which JavaseisFile            */
   long trace_num);                    /*   trace number (1-rel)          */

long jsfiles_gettracenumber            /* return trace number (1-rel)     */
  (int which);                         /*   which JavaseisFile            */

int jsfiles_setaxis                    /* return 1 if file open           */
  (int which,                          /*   which JavaseisFile            */
   int idim,                           /*   which axis (1-rel) to set     */
   int length,                         /*   length of axis                */
   const char *domain,                 /*   axis data domain              */
   const char *units,                  /*   axis data units               */
   long logical_org,                   /*   axis logical origin           */
   long logical_del,                   /*   axis logical delta            */
   double physical_org,                /*   axis physical origin          */
   double physical_del);               /*   axis physical delta           */

int jsfiles_setdatatype                /* return 1 if file open           */
  (int which,                          /*   which JavaseisFile            */
   const char *type);                  /*   data type (e.g."STACK","CMP") */

int jsfiles_settraceformat             /* return 1 if file open           */
  (int which,                          /*   which JavaseisFile            */
   const char *format);                /*   format (e.g."FLOAT","INT16")  */

int jsfiles_remove                     /* return 1 if successful          */
  (int which);                         /*   which JavaseisFile            */

void jsfiles_delete (void);            /* delete JSFILES instance         */



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
