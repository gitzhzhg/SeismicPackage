/****
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
!<CPS_v1 type="AUXILIARY_FILE"/>
****/
/*---------------- --------- jsf_crou.c ----------------------------*/
/*---------------- --------- jsf_crou.c ----------------------------*/
/*---------------- --------- jsf_crou.c ----------------------------*/

    /* other files are:  jsf_wrapper.f90  jsfiles.c  jsfiles.h */


/****


!<brief_doc>
!-------------------------------------------------------------------------------
!
!                    C P S   P R I M I T I V E
!
! Name       : JSF_CROU
! Category   : io
! Written    : 2006-08-22   by: Corn
! Revised    : 2008-11-06   by: Corn
! Maturity   : beta
! Purpose    : Interface between jsf_wrapper.f90 and jsfiles.c
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  4. 2008-11-06  Corn       Added gettracenumber
!  3. 2008-09-02  Corn       Added settracenumber
!  2. 2008-08-21  Corn       Added output and long trace numbers
!  1. 2006-08-22  Corn       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char JSF_CROU_IDENT[100] ="$Id: jsf_crou.c,v 1.2 2009/01/30 18:47:54 mengewm Exp $";


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#include "c2f_interface.h"
#include "jsfiles.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#ifdef NEED_UNDERSCORE
#define jsf_crou_create              jsf_crou_create_
#define jsf_crou_getlun              jsf_crou_getlun_
#define jsf_crou_open                jsf_crou_open_
#define jsf_crou_close               jsf_crou_close_
#define jsf_crou_isa                 jsf_crou_isa_
#define jsf_crou_status              jsf_crou_status_
#define jsf_crou_message             jsf_crou_message_
#define jsf_crou_wtype               jsf_crou_wtype_
#define jsf_crou_gettracecount       jsf_crou_gettracecount_
#define jsf_crou_getnumdimensions    jsf_crou_getnumdimensions_
#define jsf_crou_getsamplecount      jsf_crou_getsamplecount_
#define jsf_crou_getsamplerate       jsf_crou_getsamplerate_
#define jsf_crou_getlav              jsf_crou_getlav_
#define jsf_crou_getstarttimeinsecs  jsf_crou_getstarttimeinsecs_
#define jsf_crou_getheadercount      jsf_crou_getheadercount_
#define jsf_crou_gettrace            jsf_crou_gettrace_
#define jsf_crou_getheaders          jsf_crou_getheaders_
#define jsf_crou_puttrace            jsf_crou_puttrace_
#define jsf_crou_putheaders          jsf_crou_putheaders_
#define jsf_crou_settracenumber      jsf_crou_settracenumber_
#define jsf_crou_gettracenumber      jsf_crou_gettracenumber_
#define jsf_crou_setaxis             jsf_crou_setaxis_
#define jsf_crou_setdatatype         jsf_crou_setdatatype_
#define jsf_crou_settraceformat      jsf_crou_settraceformat_
#define jsf_crou_remove              jsf_crou_remove_
#define jsf_crou_delete              jsf_crou_delete_
#elif defined NEED_CAPITALS
#define jsf_crou_create              JSF_CROU_CREATE
#define jsf_crou_getlun              JSF_CROU_GETLUN
#define jsf_crou_open                JSF_CROU_OPEN
#define jsf_crou_close               JSF_CROU_CLOSE
#define jsf_crou_isa                 JSF_CROU_ISA
#define jsf_crou_status              JSF_CROU_STATUS
#define jsf_crou_message             JSF_CROU_MESSAGE
#define jsf_crou_wtype               JSF_CROU_WTYPE
#define jsf_crou_gettracecount       JSF_CROU_GETTRACECOUNT
#define jsf_crou_getnumdimensions    JSF_CROU_GETNUMDIMENSIONS
#define jsf_crou_getsamplecount      JSF_CROU_GETSAMPLECOUNT
#define jsf_crou_getsamplerate       JSF_CROU_GETSAMPLERATE
#define jsf_crou_getlav              JSF_CROU_GETLAV
#define jsf_crou_getstarttimeinsecs  JSF_CROU_GETSTARTTIMEINSECS
#define jsf_crou_getheadercount      JSF_CROU_GETHEADERCOUNT
#define jsf_crou_gettrace            JSF_CROU_GETTRACE
#define jsf_crou_getheaders          JSF_CROU_GETHEADERS
#define jsf_crou_puttrace            JSF_CROU_PUTTRACE
#define jsf_crou_putheaders          JSF_CROU_PUTHEADERS
#define jsf_crou_settracenumber      JSF_CROU_SETTRACENUMBER
#define jsf_crou_gettracenumber      JSF_CROU_GETTRACENUMBER
#define jsf_crou_setaxis             JSF_CROU_SETAXIS
#define jsf_crou_setdatatype         JSF_CROU_SETDATATYPE
#define jsf_crou_settraceformat      JSF_CROU_SETTRACEFORMAT
#define jsf_crou_remove              JSF_CROU_REMOVE
#define jsf_crou_delete              JSF_CROU_DELETE
#endif

#ifdef __cplusplus
extern "C" {
#endif


/*--------------------------- prototypes ---------------------------------*/
/*--------------------------- prototypes ---------------------------------*/
/*--------------------------- prototypes ---------------------------------*/

INTEGER jsf_crou_create            /* Constructor                           */
  (INTEGER *alloc);                /*   Number of JavaSeisFiles possible    */

INTEGER jsf_crou_getlun            /* Return a JavaSeisFile lun             */
  (char *filename,                 /*   Name of JavaSeisFile                */
   char *rw);                      /*   read: "r", write: "rw"              */

INTEGER jsf_crou_open              /* Open a JavaSeisFile                  */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_close             /* Close a JavaSeisFile                  */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_isa               /* Returns 0 if not a JavaSeisFile       */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_status            /* Returns 0 if JavaSeisFile OK          */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_message           /* Gets an error message                 */
  (INTEGER *which,                 /*   which JavaSeisFile to access        */
   char *mess,                     /*   preallocated error message          */
   INTEGER *max_size);             /*   maximum size of array               */

INTEGER jsf_crou_wtype             /* Gets a data type description          */
  (INTEGER *which,                 /*   which JavaSeisFile to access        */
   char *wtype,                    /*   preallocated data type (e.g. "JSEIS"*/
   INTEGER *max_size);             /*   maximum size of array               */

LONG jsf_crou_gettracecount        /* Returns trace count                   */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_getnumdimensions  /* Return number of dimesnsions          */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_getsamplecount    /* Returns sample count                  */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

REAL jsf_crou_getsamplerate        /* Returns sample rate                   */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

DOUBLE jsf_crou_getlav             /* Returns the largest ampl value (est)  */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

DOUBLE jsf_crou_getstarttimeinsecs /* Returns trace start time in secs      */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_getheadercount    /* Returns header count                  */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_gettrace          /* Gets a trace                          */
  (INTEGER *which,                 /*   which JavaSeisFile to access        */
   REAL *trace,                    /*   preallocated trace array            */
   INTEGER *max_size);             /*   maximum size of array               */

INTEGER jsf_crou_getheaders        /* Gets a set of trace headers           */
  (INTEGER *which,                 /*   which JavaSeisFile to access        */
   DOUBLE *headers,                /*   preallocated headers array          */
   INTEGER *max_size);             /*   maximum size of array               */

INTEGER jsf_crou_puttrace          /* Puts a trace                          */
  (INTEGER *which,                 /*   which JavaSeisFile to access        */
   REAL *trace,                    /*   trace array to write                */
   INTEGER *size);                 /*   size of array                       */

INTEGER jsf_crou_putheaders        /* Puts a header array                   */
  (INTEGER *which,                 /*   which JavaSeisFile to access        */
   DOUBLE *headers,                /*   header array to write               */
   INTEGER *size);                 /*   size of array                       */

INTEGER jsf_crou_settracenumber    /* Sets a trace number                   */
  (INTEGER *which,                 /*   which JavaSeisFile to access        */
   LONG *trace_num);               /*   which trace number (1-rel)          */

LONG jsf_crou_gettracenumber       /* Returns trace number (1-rel)          */
  (INTEGER *which);                /*   which JavaSeisFile to access        */

INTEGER jsf_crou_setaxis           /* return 1 if file open                 */
  (INTEGER *which,                 /*   which JavaseisFile                  */
   INTEGER *idim,                  /*   which axis dimension (1-rel) to set */
   INTEGER *length,                /*   length of axis                      */
   char *domain,                   /*   axis data domain                    */
   char *units,                    /*   axis data units                     */
   LONG *logical_org,              /*   axis logical origin                 */
   LONG *logical_del,              /*   axis logical delta                  */
   DOUBLE *physical_org,           /*   axis physical origin                */
   DOUBLE *physical_del);          /*   axis physical delta                 */

INTEGER jsf_crou_setdatatype       /* return 1 if file open                 */
  (INTEGER *which,                 /*   which JavaseisFile                  */
   char *type);                    /*   data type (e.g."STACK","CMP")       */

INTEGER jsf_crou_settraceformat    /* return 1 if file open                 */
  (INTEGER *which,                 /*   which JavaseisFile                  */
   char *format);                  /*   format (e.g."FLOAT","INT16")        */

INTEGER jsf_crou_remove            /* return 1 if successful                */
  (INTEGER *which);                /*   which JavaseisFile                  */

void jsf_crou_delete ();           /* Destructor                            */


/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/


INTEGER jsf_crou_create (INTEGER *alloc)
{
  return (INTEGER)jsfiles_create ((int *)alloc);
}

INTEGER jsf_crou_getlun (char *filename, char *rw)
{
  return (INTEGER)jsfiles_getlun ((const char *)filename, (const char *)rw);
}

INTEGER jsf_crou_open (INTEGER *which)
{
  return (INTEGER)jsfiles_open ((int)(*which));
}

INTEGER jsf_crou_close (INTEGER *which)
{
  return (INTEGER)jsfiles_close ((int)(*which));
}

INTEGER jsf_crou_isa (INTEGER *which)
{
  return (INTEGER)jsfiles_isa ((int)(*which));
}

INTEGER jsf_crou_status (INTEGER *which)
{
  return (INTEGER)jsfiles_status ((int)(*which));
}

INTEGER jsf_crou_message (INTEGER *which, char *mess, INTEGER *max_size)
{
  return (INTEGER)jsfiles_message ((int)(*which), mess, (size_t)(*max_size));
}

INTEGER jsf_crou_wtype (INTEGER *which, char *wtype, INTEGER *max_size)
{
  return (INTEGER)jsfiles_wtype ((int)(*which), wtype, (size_t)(*max_size));
}

LONG jsf_crou_gettracecount (INTEGER *which)
{
  return (LONG)jsfiles_gettracecount ((int)(*which));
}

INTEGER jsf_crou_getnumdimensions (INTEGER *which)
{
  return (INTEGER)jsfiles_getnumdimensions ((int)(*which));
}

INTEGER jsf_crou_getsamplecount (INTEGER *which)
{
  return (INTEGER)jsfiles_getsamplecount ((int)(*which));
}

REAL jsf_crou_getsamplerate (INTEGER *which)
{
  return (REAL)jsfiles_getsamplerate ((int)(*which));
}

DOUBLE jsf_crou_getlav (INTEGER *which)
{
  return (DOUBLE)jsfiles_getlav ((int)(*which));
}

DOUBLE jsf_crou_getstarttimeinsecs (INTEGER *which)
{
  return (DOUBLE)jsfiles_getstarttimeinsecs ((int)(*which));
}

INTEGER jsf_crou_getheadercount (INTEGER *which)
{
  return (INTEGER)jsfiles_getheadercount ((int)(*which));
}

INTEGER jsf_crou_gettrace (INTEGER *which, REAL *trace,
  INTEGER *max_size)
{
  return (INTEGER)jsfiles_gettrace ((int)(*which), (float *)trace,
    (size_t)(*max_size));
}

INTEGER jsf_crou_getheaders (INTEGER *which, DOUBLE *headers,
  INTEGER *max_size)
{
  return (INTEGER)jsfiles_getheaders ((int)(*which), (double *)headers,
    (size_t)(*max_size));
}

INTEGER jsf_crou_puttrace (INTEGER *which, REAL *trace,
  INTEGER *size)
{
  return (INTEGER)jsfiles_puttrace ((int)(*which), (float *)trace,
    (size_t)(*size));
}

INTEGER jsf_crou_putheaders (INTEGER *which, DOUBLE *headers,
  INTEGER *size)
{
  return (INTEGER)jsfiles_putheaders ((int)(*which),
    (double *)headers, (size_t)(*size));
}

INTEGER jsf_crou_settracenumber (INTEGER *which, LONG *trace_num)
{
  return (INTEGER)jsfiles_settracenumber ((int)(*which),
    (long)(*trace_num));
}

LONG jsf_crou_gettracenumber (INTEGER *which)
{
  return (LONG)jsfiles_gettracenumber ((int)(*which));
}

INTEGER jsf_crou_setaxis (INTEGER *which, INTEGER *idim, INTEGER *length,
  char *domain, char *units, LONG *logical_org, LONG *logical_del,
  DOUBLE *physical_org, DOUBLE *physical_del)
{
  return (INTEGER)jsfiles_setaxis ((int)(*which), (int)(*idim),
    (int)(*length), (const char *)domain, (const char *)units,
    (long)(*logical_org), (long)(*logical_del), (double)(*physical_org),
    (double)(*physical_del));
}

INTEGER jsf_crou_setdatatype (INTEGER *which, char *type)
{
  return (INTEGER)jsfiles_setdatatype ((int)(*which), (const char *)type);
}

INTEGER jsf_crou_settraceformat (INTEGER *which, char *format)
{
  return (INTEGER)jsfiles_settraceformat ((int)(*which),
    (const char *)format);
}

INTEGER jsf_crou_remove (INTEGER *which)
{
  return (INTEGER)jsfiles_remove ((int)(*which));
}

void jsf_crou_delete ()
{
  jsfiles_delete ();
}


/*-------------------------- end of functions -------------------------*/
/*-------------------------- end of functions -------------------------*/
/*-------------------------- end of functions -------------------------*/

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
