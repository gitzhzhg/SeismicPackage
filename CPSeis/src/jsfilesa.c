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
!<CPS_v1 type="PRIMITIVE"/>
****/
/*------------------------------ jsfilesa.c ---------------------------------*/
/*------------------------------ jsfilesa.c ---------------------------------*/
/*------------------------------ jsfilesa.c ---------------------------------*/
 
        /* other files are:  jsfiles.h */
 

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : JSFILESA 
! Category   : io
! Written    : 2008-11-06   by: Corn
! Revised    : 2008-11-06   by: Corn 
! Maturity   : beta
! Purpose    : C class used to open multiple JavaSeisWrapper.java classes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!    The purpose of this "C-style class" is to call methods on the Java class
!    JavaSeisWrapper using logical units. Only primitive data types are
!    given in arguments or returned from function calls. Multiple files can
!    be opened at once.
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
! For pointers, the flag (i,o,b) refers to the contents pointed to
! by the pointer, not to the value of the pointer itself.  The pointer
! value (the address) is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                   o                          i
!                  cnt   = jsfilesa_create ([count])
!
!                   o                          i      i
!                 which  = jsfilesa_getlun (filename, rw)
!
!                   o                      i
!                   ok  = jsfilesa_open (which)
!
!                   o                        i
!                   ok   = jsfilesa_close (which)
!
!                   o                      i
!                  isa   = jsfilesa_isa (which)
!
!                   o                         i
!                  stat  = jsfilesa_status (which)
!
!                   o                          i     b       i
!                  cnt   = jsfilesa_message (which, mess, max_size)
!
!                   o                        i      b       i
!                  cnt   = jsfilesa_wtype (which, wtype, max_size)
!
!                   o                                i
!                  tcnt  = jsfilesa_gettracecount (which)
!
!                   o                                   i
!                  ndim  = jsfilesa_getnumdimensions (which)
!
!                   o                                 i
!                  scnt  = jsfilesa_getsamplecount (which)
!
!                    o                               i
!                  srate = jsfilesa_getsamplerate (which)
!
!                   o                         i
!                  lav   = jsfilesa_getlav (which)
!
!                   o                                     i
!                  tstrt = jsfilesa_getstarttimeinsecs (which)
!
!                   o                                 i
!                  hcnt  = jsfilesa_getheadercount (which)
!
!                   o                           i      b       i
!                  cnt   = jsfilesa_gettrace (which, trace, max_size)
!
!                   o                             i       b        i
!                  cnt   = jsfilesa_getheaders (which, headers, max_size)
!
!                   o                           i      i     i
!                  cnt   = jsfilesa_puttrace (which, trace, size)
!
!                   o                             i       b      i
!                  cnt   = jsfilesa_putheaders (which, headers, size)
!
!                  o                                  i        i
!                  ok    = jsfilesa_settracenumber (which, trace_num)
!
!                   o                                 i
!                  tnum  = jsfilesa_gettracenumber (which)
!
!                  o                           i      i     i       i
!                  ok    = jsfilesa_setaxis (which, idim, length, domain,
!                                              i         i            i
!                                            units, logical_org, logical_del,
!                                                 i             i
!                                            physical_org, physical_del)
!
!                  o                               i     i
!                  ok    = jsfilesa_setdatatype (which, type)
!
!                  o                                  i      i
!                  ok    = jsfilesa_settraceformat (which, format)
!
!                  o                          i
!                  ok    = jsfilesa_remove (which);
!
!                  void    jsfilesa_delete ()
!
! int          cnt       = Returned size of the array populated/written
! int          *count    = Optional number of JavaSeisFiles possible (def=20)
! int          which     = Which JavaSeisFile (a logical unit: 1-relative)
! const char   *filename = Name of JavaSeisFile to open
! const char   *rw       = Mode in which to open JavaSeisFil
! int          ok        = Flag (0 means not OK)
! int          isa       = Returned as 0 if object is not for a JavaSeisFile
! int          stat      = Returned sd 0 if JavaSeisWrapper is NORMAL
! char         *mess     = A preallocated char array populated by the object 
! size_t       max_size  = preallocated size of the given array
! char         *wtype    = A preallocated char array given to the Java object
! long         tcnt      = Returned trace count
! int          ndim      = Returned number of dimensions in JavaSeisFile
! int          scnt      = Returned sample count
! float        srate     = Returned sample rate in seconds
! double       lav       = Returned largest amplitude value (est) JavaSeisFile
! double       tstrt     = Returned trace starting time in seconds
! int          hcnt      = Returned count of the headers for each trace
! int          nhw       = Returned number of header words for each trace
! long         trace_num = A given 1-relative trace number
! long         tnum      = Returned 1-relative trace number
! float        *trace    = A preallocated float array populated by the object
! double       *headers  = A preallocated double array populated by the object
! size_t       size      = size of given array
! int          idim      = Which axis of JavaSeisFile
! int          length    = Length of JavaSeisFile axis
! const char   *domain   = Axis domain
! cosnt char   *units    = Axis units
! const char   *type     = JavaSeisFile data type
! const char   *format   = JavaSeisFile trace format
!
! long      logical_org  = Logical origin of axis
! long      logical_del  = Logical delta for axis
! double    physical_org = Physical origin of axis
! double    physical_org = Physical delta for axis
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2008-11-06  CORN       Initial version. See history in jsfiles.c
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! Use with Java 5.0 or later.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
!
! comiple on sparc using:
! > cc -c -g -I$JAVA_HOME/include -I$JAVA_HOME/include/solaris \
! > -o jsfilesa.o jsfilesa.c
!
! link on sparc using:
! > cc -g -L$JAVA_HOME/jre/lib/sparc -L$JAVA_HOME/jre/lib/sparc/client \
! > -ljava -ljvm -lverify -o app_name app_name.o jsfilesa.o any_other.o
!
! comile on linux using:
! > gcc -c -g -I$JAVA_HOME/include -I$JAVA_HOME/include/linux \
! > -o jsfilesa.o jsfilesa.c
!
! link on linux using:
! > gcc -g -L$JAVA_HOME/jre/lib/i386 -L$JAVA_HOME/jre/lib/i386/client -ljava \
! > -ljvm -lverify -o app_name app_name.o jsfilesa.o any_other.o
!
! run with a CLASSPATH environment variable defined which will allow
! all relevant java packages to be satisfied
!
!-------------------------------------------------------------------------------
!</compile_doc>
****/

char JSFILESA_IDENT[100] ="$Id: jsfilesa.c,v 1.1 2009/01/30 21:33:01 mengewm Exp $";

/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


#include "jsfilesa.h"
#include "jswrapper.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>

#ifdef __cplusplus
extern "C" {
#endif


/*-------------------- abstract data definitions -------------------------*/
/*-------------------- abstract data definitions -------------------------*/
/*-------------------- abstract data definitions -------------------------*/


struct _JSFilesA
{
  JSWrapper **_jsw;
  int _alloc;
};
typedef struct _JSFilesA JSFilesA;

struct _JSNotFiles
{
  char **_fname;
  time_t *_time_stamp;
  int *_order;
  int _alloc;
};
typedef struct _JSNotFiles JSNotFiles;

/*--------------------------- static data --------------------------------*/
/*--------------------------- static data --------------------------------*/
/*--------------------------- static data --------------------------------*/

static JSFilesA *JSFILESA = NULL;
static JSNotFiles *JSNOTFILES = NULL;

#define ALLOC_DEF_A 20
#define NOT_ALLOC 20

/*--------------------- private function prototype ------------------------*/
/*--------------------- private function prototype ------------------------*/
/*--------------------- private function prototype ------------------------*/
int jsfilesa_fclose (int which);
int jsfilesa_getnext (void);
int jsfilesa_exists (const char *filename, const char *rw);
void jsfilesa_setorder (int which);
int jsfilesa_updateit (int active, int prev_active);
int jsfilesa_isnot (const char *filename);
void jsfilesa_setnot (const char *filename);
time_t jsfilesa_lastmodified (const char *filename);


/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/

/*
 * allow the static structure to expand by not shrink
 * allow the constructor to be reentrant
 * protect previously opened files
 * getlun will cause a new jswrapper class to be created if it doesn't
 *   already exist
 * a public close will set a jswrapper inactive
 * a private getnext uses a LULO (Last Used Last Out) method of getting
 *   the next lun
 * a private fclose will delete a jswrapper class
 * app determines number of files opened simultaneously
 *
 */
int jsfilesa_create (int *alloc)
{
  int retval;

  JSFilesA *jsfilesa = NULL;
  int doit, expand, k2;

  /* make sure the number to allocate is valid */
  if (alloc == NULL || *alloc < 1) {
    if (JSFILESA != NULL) {
      assert (JSFILESA->_alloc > 0); /* programming sanity check */
      retval = JSFILESA->_alloc;
    }
    else {
      retval = ALLOC_DEF_A;
    }
  }
  else {
    retval = *alloc;
  }

  /* see if requested more */
  if (JSFILESA != NULL) {
    if (retval > JSFILESA->_alloc) {
      expand = 1;
      doit = 1;
    }
    else {
      expand = 0;
      doit = 0;
    }
  }
  else {
    expand = 0;
    doit = 1;
  }

  if (doit != 0) {
    /* begin the initialization of the new structure */
    jsfilesa = (JSFilesA *)malloc (sizeof(JSFilesA));
    if (jsfilesa == NULL) return 0;
    jsfilesa->_jsw = (JSWrapper **)malloc ((retval)*sizeof(JSWrapper*));
    for (k2 = 0; k2 < retval; k2++) {
      jsfilesa->_jsw[k2] = NULL;
    }
  }

  /* complete initialization */
  if (doit != 0 && expand != 0) {
    jsfilesa->_alloc = retval;
    for (k2 = 0; k2 < JSFILESA->_alloc; k2++) {
      jsfilesa->_jsw[k2] = JSFILESA->_jsw[k2];
    }
    /* do not close the opened files, but free old structure */
    free (JSFILESA->_jsw);
    free (JSFILESA);
    JSFILESA = jsfilesa;
  }
  else if (doit != 0 && JSFILESA == NULL) {
    jsfilesa->_alloc = retval;
    JSFILESA = jsfilesa;
  }

  /* do a programming sanity check */
  assert (JSFILESA != NULL);
  assert (JSFILESA->_alloc == retval && retval > 0);

  return retval;
}

/* create the static structure but do not open yet */
int jsfilesa_getlun (const char *filename, const char *rw)
{
  int retval, current, not_a_jsfile;
  JSWrapper *obj;

  if (JSFILESA == NULL) assert (jsfilesa_create(NULL) > 0);

  not_a_jsfile = jsfilesa_isnot (filename);
  if (not_a_jsfile) return 0;

  retval = jsfilesa_exists (filename, rw);
  /*printf ("jsfilesa_getlun: exists = %d\n", retval);*/
  if (retval < 1) {
    current = jsfilesa_getnext ();
    if (current < JSFILESA->_alloc) {
      /*printf ("jsfilesa_getlun: filename = %s\n", filename);*/
      obj = jswrapper_createrw (filename, rw);
      /*printf ("jsfilesa_getlun: obj = %d\n", obj);*/
      if (obj != NULL) {
        if (jswrapper_isreadwrite(obj) == 0     &&
            jswrapper_isajavaseisfile(obj) == 0   ) {
          /*printf ("jsfilesa_getlun: %s\n is not a JavaSeisFile\n", filename);*/
          jsfilesa_setnot (filename);
          jswrapper_close (obj);
          jswrapper_delete (obj);
          retval = 0;
        }
        else {
          JSFILESA->_jsw[current] = obj;
          /*printf ("jsfilesa_getlun: JSFILES = %d\n", JSFILES);*/
          /*printf ("jsfilesa_getlun: JSFILES->_jsw = %d\n", JSFILES->_jsw);*/
          /*printf ("jsfilesa_getlun: JSFILES->_jsw[%d] = %d\n", current, obj);*/
          retval = current + 1; /* return a 1-relative number */
        }
      }
      else {
        retval = 0;
      }
    }
    else {
      /* should never get here */
      assert (!(current < JSFILESA->_alloc));
    }
  }
  jsfilesa_setorder (retval);
  return retval;
}

/* open the given lun */
int jsfilesa_open (int which)
{
  int retval;
  JSWrapper *obj;
  int current = which - 1;

  assert (JSFILESA != NULL);

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_open (obj);
      if (retval != 1) {
      }
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

/* dummy */
int jsfilesa_close (int which)
{
  int retval;
  if (jsfilesa_isa(which) == 1) {
    retval = 1; /* dummy: make the caller think that closure happened */
  }
  else {
    retval = 0; /* not a JavaSeis file tell caller closure didn't happen */
  }
  return retval;
}

int jsfilesa_fclose (int which)
{
  int retval;

  int current = which - 1;
  JSWrapper *obj;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      jswrapper_close (obj);
      jswrapper_delete (obj);
      /*printf ("jsfilesa_fclose: lun = %d\n", current);*/
      JSFILESA->_jsw[current] = NULL;
      retval = 1;
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_isa (int which)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_isajavaseisfile (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_status (int which)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_status (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_message (int which, char *mess, int max_size)
{
  int retval;
  JSWrapper *obj;
  char *mes;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      mes = jswrapper_message (obj, mess, (size_t)max_size);
      if (mes != NULL) {
        retval = strlen (mess);
      }
      else {
        retval = 0;
      }
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_wtype (int which, char *wtype, int max_size)
{
  int retval;
  JSWrapper *obj;
  char *wt;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      wt = jswrapper_wtype (obj, wtype, (size_t)max_size);
      if (wt != NULL) {
        retval = strlen (wt);
      }
      else {
        retval = 0;
      }
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

long jsfilesa_gettracecount (int which)
{
  long retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_gettracecount (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_getnumdimensions (int which)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_getnumdimensions (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_getsamplecount (int which)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_getsamplecount (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

float jsfilesa_getsamplerate (int which)
{
  float retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_getsamplerate (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

double jsfilesa_getlav (int which)
{
  double retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_getlav (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

double jsfilesa_getstarttimeinsecs (int which)
{
  double retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_getstarttimeinseconds (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_getheadercount (int which)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_getheadercount (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_gettrace (int which, float *trace, int max_size)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;

  assert (JSFILESA != NULL);

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_gettrace (obj, trace, (size_t)max_size);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_getheaders (int which, double *headers, int max_size)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_getheaders (obj, headers, (size_t)max_size);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_puttrace (int which, float *trace, int size)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;

  assert (JSFILESA != NULL);

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_puttrace (obj, trace, (size_t)size);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_putheaders (int which, double *headers, int size)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;

  assert (JSFILESA != NULL);

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_putheaders (obj, headers, (size_t)size);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_settracenumber (int which, long trace_num)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;

  assert (JSFILESA != NULL);

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_settracenumber (obj, trace_num);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

/* since trace number is 1-relative, valid trace numbers are > 0 */
long jsfilesa_gettracenumber (int which)
{
  long retval;

  JSWrapper *obj;
  int current = which - 1;

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_gettracenumber (obj);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_setaxis (int which, int idim, int length, const char *domain,
  const char *units, long logical_org, long logical_del, double physical_org,
  double physical_del)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;

  assert (JSFILESA != NULL);

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_setaxis (obj, idim-1, length, domain, units, logical_org,
        logical_del, physical_org, physical_del);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_setdatatype (int which, const char *type)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;

  assert (JSFILESA != NULL);

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_setdatatype (obj, type);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_settraceformat (int which, const char *format)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;

  assert (JSFILESA != NULL);

  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_settraceformat (obj, format);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfilesa_remove (int which)
{
  int retval;

  JSWrapper *obj;
  int current = which - 1;
  assert (JSFILESA != NULL);
  if (current > -1 && current < JSFILESA->_alloc) {
    obj = JSFILESA->_jsw[current];
    if (obj != NULL) {
      retval = jswrapper_remove (obj);
      if (retval == 1) {
        jswrapper_delete (obj);
      }
      if (retval == 1) {
        JSFILESA->_jsw[current] = NULL;
      }
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

void jsfilesa_delete (void)
{
  int k2;

  if (JSFILESA != NULL) {
    for (k2 = 0; k2 < JSFILESA->_alloc; k2++) {
      /*printf ("jsfilesa_delete: calling jsfilesa_fclose\n");*/
      jsfilesa_fclose (k2+1);
    }
    free (JSFILESA->_jsw);
    free (JSFILESA);
    JSFILESA = NULL;
  }
}

int jsfilesa_getnext (void)
{
  int retval, k2, active, oldest;

  JSWrapper *obj;

  for (k2 = 0, retval = JSFILESA->_alloc; k2 < JSFILESA->_alloc; k2++) {
    if (JSFILESA->_jsw[k2] == NULL) {
      retval = k2;
      k2 = JSFILESA->_alloc;
    }
  }

  if (retval == JSFILESA->_alloc) {
    /* reuse the oldest element in JSFILESA->_jsw[k2] */
    for (k2 = 0, oldest = 1; k2 < JSFILESA->_alloc; k2++) {
      obj = JSFILESA->_jsw[k2];
      active = jswrapper_getactive (obj);
      if (active > oldest) {
        oldest = active;
        retval = k2;
      }
    }
    jsfilesa_fclose (retval+1);
  }
  return retval;
}

int jsfilesa_exists (const char *filename, const char *rw)
{
  int retval, k2, irw;
  char loc_fname[300];
  char *loc_fnp;
  size_t max_size = 300; /* limited to filenames < 301 characters! */
  JSWrapper *obj;

  /*printf ("jsfilesa_exists: filename, rw = %s %s\n", filename, rw);*/
  /*printf ("jsfilesa_exists: count = %d\n", JSFILESA->_alloc);*/

  if (tolower((int)rw[0]) == 'r' && tolower((int)rw[1]) == 'w') {
    irw = 1;
  }
  else {
    irw = 0;
  }

  for (k2 = 0, retval = 0; k2 < JSFILESA->_alloc; k2++) {
    obj = JSFILESA->_jsw[k2];
    /*if (k2 == 0 || k2 == 1) {
        printf ("jsfilesa_exists: obj[%d] = %ld\n", k2, obj);
      }*/
    if (obj != NULL) {
      loc_fnp = jswrapper_getfilename (obj, loc_fname, max_size);
      /*if (k2 == 0 || k2 == 1) {
          printf ("jsfilesa_exists: loc_fname = %s\n", loc_fnp);
        }*/
      if (loc_fnp != NULL) {
        if (strcmp(loc_fnp,filename) == 0) {
          if (irw == 1) {
            if (jswrapper_isreadwrite(obj) == 0) {
              /*
               * if opened file is readonly then close it and act like it
               *   didn't exist!
               */
              /*printf ("jsfilesa_exists: closing %d\n", k2);*/
              jsfilesa_fclose (k2+1);
            }
          }
          else {
            retval = k2 + 1; /* return the lun */
          }
        }
      }
    }
  }
  return retval;
}

/*
 * if necessary, set the order so that the given lun is one and all
 *   others which are active are > 1 and unique
 */
void jsfilesa_setorder (int which)
{
  int current = which - 1;
  JSWrapper *obj;
  int k2, prev_active, active;

  if (!(current > -1 && current < JSFILESA->_alloc)) return;
  obj = JSFILESA->_jsw[current];

  if (obj == NULL) return;
  prev_active = jswrapper_getactive (obj);
  if (prev_active == 1) return;

  /* advance previously used obj's only as necessary */
  for (k2 = 0; k2 < JSFILESA->_alloc; k2++) {
    obj = JSFILESA->_jsw[k2];
    if (obj != NULL) {
      active = jswrapper_getactive (obj);
      if (k2 == current) {
        jswrapper_setactive (obj, 1);
      }
      else if (jsfilesa_updateit(active,prev_active) == 1) {
        active++;
        jswrapper_setactive (obj, active);
      } /*
      else {
        nothing to do
      }  */
    }
  }
}

/* should the active flag be updated? */
int jsfilesa_updateit (int active, int prev_active)
{
  int retval;

  if (prev_active == 0) {
    /* never used before so update all the other active obj's */
    retval = active > prev_active ? 1 : 0;
  }
  else {
    /* it was used before so update all the younger active obj's */
    retval = active < prev_active ? 1 : 0;
  }
  return retval;
}

int jsfilesa_isnot (const char *filename)
{
  int retval, k2, order, prev_order, k3;
  time_t time_stamp;
  char *fname;

  if (JSNOTFILES == NULL) {
    retval = 0;
  }
  else if (filename == NULL) {
    retval = 0;
  }
  else {
    for (k2 = 0, retval = 0; retval == 0 && k2 < JSNOTFILES->_alloc; k2++) {
      fname = JSNOTFILES->_fname[k2];
      if (fname != NULL) {
        if (strcmp(fname,filename) == 0) {
          time_stamp = jsfilesa_lastmodified (filename);
          if (time_stamp == JSNOTFILES->_time_stamp[k2]) {
            prev_order = JSNOTFILES->_order[k2];
            JSNOTFILES->_order[k2] = 1;
            /* update the order of the other non-NULL entries */
            for (k3 = 0; k3 < JSNOTFILES->_alloc; k3++) {
              fname = JSNOTFILES->_fname[k3];
              if (k3 != k2 && fname != NULL) {
                order = JSNOTFILES->_order[k3];
                if (jsfilesa_updateit(order,prev_order) == 1) {
                  order++;
                  JSNOTFILES->_order[k3] = order;
                }
              }
            }
          }
          retval = 1;
        }
      }
    }
  }
  return retval;
}

void jsfilesa_setnot (const char *filename)
{
  int k2, index, len, order;

  if (JSNOTFILES == NULL) {
    /* begin the initialization of the new structure */
    JSNOTFILES = (JSNotFiles *)malloc (sizeof(JSNotFiles));
    if (JSNOTFILES == NULL) return;
    JSNOTFILES->_fname = (char **)malloc ((NOT_ALLOC)*sizeof(char*));
    JSNOTFILES->_time_stamp = (time_t *)malloc ((NOT_ALLOC)*sizeof(time_t));
    JSNOTFILES->_order = (int *)malloc ((NOT_ALLOC)*sizeof(int));
    JSNOTFILES->_alloc = NOT_ALLOC;
    for (k2 = 0; k2 < JSNOTFILES->_alloc; k2++) {
      JSNOTFILES->_fname[k2] = NULL;
      JSNOTFILES->_time_stamp[k2] = (time_t)0;
      JSNOTFILES->_order[k2] = 0;
    }
  }

  /* look for index to put NOT filename */
  for (k2 = 0, index = -1; index == -1 && k2 < JSNOTFILES->_alloc; k2++) {
    if (JSNOTFILES->_fname[k2] == NULL) {
      assert (JSNOTFILES->_time_stamp[k2] == (time_t)0);
      assert (JSNOTFILES->_order[k2] == 0);
      index = k2;
    }
  }

  if (index == -1) {
    /* all indexes are used and should be from 1 to JSNOTFILES->_alloc
     * remove the k2 whose JSNOTFILES->_order[k2] == JSNOTFILES->_alloc */
    for (k2 = 0; index == -1 && k2 < JSNOTFILES->_alloc; k2++) {
      if (JSNOTFILES->_order[k2] == JSNOTFILES->_alloc) {
        free (JSNOTFILES->_fname[k2]);
        JSNOTFILES->_fname[k2] = NULL;
        JSNOTFILES->_time_stamp[k2] = (time_t)0;
        JSNOTFILES->_order[k2] = 0;
        index = k2;
      }
    }
    assert (index > -1);
  }
 
  /* store the fname */
  len = strlen (filename) + 1;
  JSNOTFILES->_fname[index] = (char *)malloc (len*sizeof(char));
  strcpy (JSNOTFILES->_fname[index], filename);
  JSNOTFILES->_time_stamp[index] = jsfilesa_lastmodified (filename);
  JSNOTFILES->_order[index] = 1;
  /* update the other previously used index orders */
  for (k2 = 0; k2 < JSNOTFILES->_alloc; k2++) {
    if (k2 != index && JSNOTFILES->_order[k2] != 0) {
      order = JSNOTFILES->_order[k2];
      order++;
      JSNOTFILES->_order[k2] = order;
    }
  }
}

time_t jsfilesa_lastmodified (const char *filename)
{
  time_t retval;
  char buf[BUFSIZ];
  int error;

  /* create the stat data */
  struct stat *status;
  status = (struct stat *)malloc (sizeof(struct stat));

  error = stat (filename, status);
  if (error != 0) {
    sprintf (buf, "On %s: \n", filename);
    strcat (buf, strerror(errno));
    printf ("%s\n", buf);
    retval = (time_t)0;
  }
  else {
    retval = status->st_mtime;
  }
  
  /* free the stat data */
  free (status);

  return retval;
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
