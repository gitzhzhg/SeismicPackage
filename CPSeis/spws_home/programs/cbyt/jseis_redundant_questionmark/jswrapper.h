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
/*----------------------------- jswrapper.h ---------------------------------*/
/*----------------------------- jswrapper.h ---------------------------------*/
/*----------------------------- jswrapper.h ---------------------------------*/

    /* other files are:  jswrapper.c */
 
/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                      C P S   H E A D E R   F I L E
!
! Name       : jswrapper
! Category   : io
! Written    : 2006-08-22   by: Corn
! Revised    : 2008-11-06   by: Corn
! Maturity   : beta
! Purpose    : C class used to call JavaSeisWrapper.java.
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
!  7. 2008-11-06  CORN         Added *gettracenumber.
!  6. 2008-10-14  CORN         Fixed an unterminated comment.
!  5. 2008-09-25  CORN         Added methods: *isreadwrite, *getfilename
!                                *setactive, & *getactive. Also, added
!                                the _active flag to JSWrapper struct.
!  4. 2008-09-02  CORN         Added settracenumber
!  3. 2008-08-21  CORN         Added output capability.
!  2. 2007-01-23  Corn         Make compatible with JavaSeisWrapper's
!                              getTraceCount method which now returns a long.
!  1. 2006-08-22  Corn         Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _JSWRAPPER_H_
#define _JSWRAPPER_H_

#include "jmwrapper.h"

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif


/*-------------------- abstract data definitions -------------------------*/
/*-------------------- abstract data definitions -------------------------*/
/*-------------------- abstract data definitions -------------------------*/


struct _JSWrapper
{
  JMWrapper *_jmw;
  jmethodID _isa;
  jmethodID _isrw;
  jmethodID _stat;
  jmethodID _mess;
  jmethodID _wtype;
  jmethodID _name;
  jmethodID _tcnt;
  jmethodID _ndim;
  jmethodID _scnt;
  jmethodID _srate;
  jmethodID _lav;
  jmethodID _strt;
  jmethodID _hcnt;
  jmethodID _trc;
  jmethodID _hdr;
  jmethodID _wtrc;
  jmethodID _whdr;
  jmethodID _set;
  jmethodID _get;
  jmethodID _open;
  jmethodID _axis;
  jmethodID _dtyp;
  jmethodID _tfmt;
  jmethodID _close;
  jmethodID _del;
  int       _active;
};

typedef struct _JSWrapper JSWrapper;


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/



JSWrapper *jswrapper_create            /* create a JSWrapper instance     */
  (const char *filename);              /*   given JavaSeisFile name       */

JSWrapper *jswrapper_createrw          /* create a JSWrapper instance     */
  (const char *filename,               /*   given JavaSeisFile name       */
   const char *rw);                    /*   given I/O flag ("r" or "rw")  */

int jswrapper_isajavaseisfile          /* return 0 if not a JavaSeisFile  */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_isreadwrite              /* return 0 if file is writable    */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_status                   /* return 0 if NORMAL status       */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

char *jswrapper_message                /* return JavaSeisWrapper message  */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   char *mess,                         /*   preallocated array to be popul*/
   size_t max_size);                   /*   maximum size of given array   */

char *jswrapper_wtype                  /* return pointer to wtype         */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   char *wtype,                        /*   preallocated array to be popul*/
   size_t max_size);                   /*   maximum size of given array   */

char *jswrapper_getfilename            /* return pointer to fname         */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   char *fname,                        /*   preallocated array to be popul*/
   size_t max_size);                   /*   maximum size of given array   */

long jswrapper_gettracecount           /* return trace count              */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_getnumdimensions         /* return number of dimensions     */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_getsamplecount           /* return sample count             */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

float jswrapper_getsamplerate          /* return sample rate in seconds   */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

double jswrapper_getlav                /* return largest abs vlu estimate */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

double jswrapper_getstarttimeinseconds /* return trace start time (sec)   */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_getheadercount           /* return number of header words   */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_gettrace                 /* return no of trace values read  */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   float *trace,                       /*   preallocated array to be popul*/
   size_t max_size);                   /*   maximum size of given array   */

int jswrapper_getheaders               /* return no of header values re   */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   double *headers,                    /*   preallocated array to be popul*/
   size_t max_size);                   /*   maximum size of given array   */

int jswrapper_puttrace                 /* return no of trc values written */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   float *trace,                       /*   array to contain trace values */
   size_t size);                       /*   size of given array           */

int jswrapper_putheaders               /* return no of hdr values written */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   double *headers,                    /*   array to contain headr values */
   size_t size);                       /*   size of given array           */

int jswrapper_settracenumber           /* return 1 if trace number was set*/
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   long trace_num);                    /*   1-relative trace number       */

long jswrapper_gettracenumber          /* return 1-relative trace number  */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_open                     /* return 1 if open successful     */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_setaxis                  /* return 1 if axis set            */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   int idim,                           /*   which axis dimension to set   */
   int length,                         /*   length of axis                */
   const char *domain,                 /*   axis data domain              */
   const char *units,                  /*   axis data units               */
   long logical_org,                   /*   axis logical origin           */
   long logical_del,                   /*   axis logical delta            */
   double physical_org,                /*   axis physical origin          */
   double physical_del);               /*   axis physical delta           */

int jswrapper_setdatatype              /* return 1 if data type set       */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   const char *type);                  /*   data type (e.g."STACK","CMP") */

int jswrapper_settraceformat          /* return 1 if trace format set    */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   const char *format);                /*   format (e.g."FLOAT","INT16")  */

void jswrapper_close                   /* close file                      */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

int jswrapper_remove                   /* return 1 if delete successful   */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

void jswrapper_delete                  /* destructor for                  */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */

void jswrapper_setactive               /* set active flag for             */
  (JSWrapper *jsw,                     /*   given JSWrapper object        */
   int active);                        /*   0=> unused, 1=> new,          */
                                       /*   2=> older,...                 */

int jswrapper_getactive                /* return active flag              */
  (JSWrapper *jsw);                    /*   given JSWrapper object        */



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

