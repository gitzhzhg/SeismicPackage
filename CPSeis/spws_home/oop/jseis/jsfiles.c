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
/*------------------------------ jsfiles.c ----------------------------------*/
/*------------------------------ jsfiles.c ----------------------------------*/
/*------------------------------ jsfiles.c ----------------------------------*/
 
        /* other files are:  jsfiles.h */
 

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : JSFILES 
! Category   : io
! Written    : 2006-08-22   by: Corn
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
!                   o                         i
!                  cnt   = jsfiles_create ([count])
!
!                   o                         i      i
!                 which  = jsfiles_getlun (filename, rw)
!
!                   o                     i
!                   ok  = jsfiles_open (which)
!
!                   o                       i
!                   ok   = jsfiles_close (which)
!
!                   o                        i
!                   ok   = jsfiles_fclose (which)
!
!                   o                     i
!                  isa   = jsfiles_isa (which)
!
!                   o                        i
!                  stat  = jsfiles_status (which)
!
!                   o                         i     b       i
!                  cnt   = jsfiles_message (which, mess, max_size)
!
!                   o                       i      b       i
!                  cnt   = jsfiles_wtype (which, wtype, max_size)
!
!                   o                               i
!                  tcnt  = jsfiles_gettracecount (which)
!
!                   o                                  i
!                  ndim  = jsfiles_getnumdimensions (which)
!
!                   o                                i
!                  scnt  = jsfiles_getsamplecount (which)
!
!                    o                              i
!                  srate = jsfiles_getsamplerate (which)
!
!                   o                        i
!                  lav   = jsfiles_getlav (which)
!
!                   o                                    i
!                  tstrt = jsfiles_getstarttimeinsecs (which)
!
!                   o                                i
!                  hcnt  = jsfiles_getheadercount (which)
!
!                   o                          i       i        b
!                  cnt   = jsfiles_gettrace (which, trace_num, trace,
!                                                 i
!                                              max_size)
!
!                   o                            i       i         b
!                  cnt   = jsfiles_getheaders (which, trace_num, headers,
!                                                 i
!                                              max_size)
!
!                   o                          i       i         i     i
!                  cnt   = jsfiles_puttrace (which, trace_num, trace, size)
!
!                   o                            i       i         i
!                  cnt   = jsfiles_putheaders (which, trace_num, headers,
!                                               i
!                                              size)
!
!                  o                          i      i     i       i
!                  ok    = jsfiles_setaxis (which, idim, length, domain,
!                                             i         i            i
!                                           units, logical_org, logical_del,
!                                                i             i
!                                           physical_org, physical_del)
!
!                  o                              i     i
!                  ok    = jsfiles_setdatatype (which, type)
!
!                  o                                 i      i
!                  ok    = jsfiles_settraceformat (which, format)
!
!                  o                         i
!                  ok    = jsfiles_remove (which);
!
!                  void    jsfiles_delete ()
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
!  8. 2008-11-06  CORN       Added *gettracenumber and updated documentation.
!                              moved all of the methods in prior jsfiles.c
!                              to the new jsfilesa.c. made jsfiles.c methods
!                              pass-through methods for jsfilesa.c. this
!                              provides virtual JavaSeis files.
!  7. 2008-10-23  CORN       Modified *close which is a dummy to return
!                              0 when the given logical unit is known not
!                              to be a JavaSeis file and 1 otherwise.
!  6. 2008-10-14  CORN       Added private modes: *isnot, *setnot, &
!                              *lastmodified. These methods and other
!                              modifications are used to
!                              1) remember if a group of data sets are not
!                                 JavaSeis types
!                              2) keep track when specific data sets were last
!                                 accessed.
!                              3) when the num of data sets exceed the num
!                                 of allowed concurrent data sets the data
!                                 set which was accessed the furthest time
!                                 from now is released.
!  5. 2008-09-30  CORN       Fixed bug in *setorder to make sure 'int which'
!                              is valid.
!  4. 2008-09-25  CORN       Added private methods: *fclose, *exists,
!                              *setorder, & *updateit.
!                              Made *close a dummy call relying rather on
!                              the LULO (Last Used Last Out) mechanism
!                              within *getlun to fclose as needed.
!  3. 2008-09-02  CORN       Added settracenumber
!  2. 2008-08-21  CORN       Added output capability.
!  1. 2006-08-22  CORN       Initial version.
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
! > -o jsfiles.o jsfiles.c
!
! link on sparc using:
! > cc -g -L$JAVA_HOME/jre/lib/sparc -L$JAVA_HOME/jre/lib/sparc/client \
! > -ljava -ljvm -lverify -o app_name app_name.o jsfiles.o any_other.o
!
! comile on linux using:
! > gcc -c -g -I$JAVA_HOME/include -I$JAVA_HOME/include/linux \
! > -o jsfiles.o jsfiles.c
!
! link on linux using:
! > gcc -g -L$JAVA_HOME/jre/lib/i386 -L$JAVA_HOME/jre/lib/i386/client -ljava \
! > -ljvm -lverify -o app_name app_name.o jsfiles.o any_other.o
!
! run with a CLASSPATH environment variable defined which will allow
! all relevant java packages to be satisfied
!
!-------------------------------------------------------------------------------
!</compile_doc>
****/

char JSFILES_IDENT[100] ="$Id: jsfiles.c,v 1.7 2008/10/27 14:02:53 CORN beta sps $";

/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


#include "jsfiles.h"
#include "jsfilesa.h"

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


struct _JSFiles
{
  char **_fnames; /* initialized to NULL */
  int *_irws;     /* initialized to -1; otherwise 1 or 0 */
  int *_tnums;    /* initialized to 0; 1-relative trace number */
  int *_luns;     /* initialized to 0; 1-relative logical unit */
  int _alloc;
};
typedef struct _JSFiles JSFiles;


/*--------------------------- static data --------------------------------*/
/*--------------------------- static data --------------------------------*/
/*--------------------------- static data --------------------------------*/

static JSFiles *JSFILES = NULL;

#define ALLOC_DEF 200

/*--------------------- private function prototype ------------------------*/
/*--------------------- private function prototype ------------------------*/
/*--------------------- private function prototype ------------------------*/

int jsfiles_exists (const char *filename, const char *rw);
int jsfiles_getnext (void);
int jsfiles_isrw (const char *rw);
int jsfiles_which (int which);


/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/

int jsfiles_create (int *alloc)
{
  int retval;

  JSFiles *jsfiles = NULL;
  int doit, expand, k2;

  /* make sure the number to allocate is valid */
  if (alloc == NULL || *alloc < 1) {
    if (JSFILES != NULL) {
      assert (JSFILES->_alloc > 0); /* programming sanity check */
      retval = JSFILES->_alloc;
    }
    else {
      retval = ALLOC_DEF;
    }
  }
  else {
    retval = *alloc;
  }

  /* see if requested more */
  if (JSFILES != NULL) {
    if (retval > JSFILES->_alloc) {
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
    jsfiles = (JSFiles *)malloc (sizeof(JSFiles));
    if (jsfiles == NULL) return 0;
    jsfiles->_fnames = (char **)malloc ((retval)*sizeof(char *));
    jsfiles->_irws = (int *)malloc ((retval)*sizeof(int));
    jsfiles->_tnums = (int *)malloc ((retval)*sizeof(int));
    jsfiles->_luns = (int *)malloc ((retval)*sizeof(int));
    for (k2 = 0; k2 < retval; k2++) {
      jsfiles->_fnames[k2] = NULL;
      jsfiles->_irws[k2] = -1;
      jsfiles->_tnums[k2] = 0;
      jsfiles->_luns[k2] = 0;
    }
  }

  /* complete initialization */
  if (doit != 0 && expand != 0) {
    jsfiles->_alloc = retval;
    for (k2 = 0; k2 < JSFILES->_alloc; k2++) {
      jsfiles->_fnames[k2] = JSFILES->_fnames[k2];
      jsfiles->_irws[k2] = JSFILES->_irws[k2];
      jsfiles->_tnums[k2] = JSFILES->_tnums[k2];
      jsfiles->_luns[k2] = JSFILES->_luns[k2];
    }
    /* do not free the existing char arrays, but free pointers to them */
    free (JSFILES->_fnames);
    free (JSFILES->_irws);
    free (JSFILES->_tnums);
    free (JSFILES->_luns);
    free (JSFILES);
    JSFILES = jsfiles;
  }
  else if (doit != 0 && JSFILES == NULL) {
    jsfiles->_alloc = retval;
    JSFILES = jsfiles;
  }

  /* do a programming sanity check */
  assert (JSFILES != NULL);
  assert (JSFILES->_alloc == retval && retval > 0);

  return retval;
}

/* create a virtual logical unit */
int jsfiles_getlun (const char *filename, const char *rw)
{
  
  int retval, current;
  char *fname;
  int lun, len;

  if (JSFILES == NULL) assert (jsfiles_create(NULL) > 0);

  retval = jsfiles_exists (filename, rw);
  lun = jsfilesa_getlun (filename, rw);
  if (retval < 1) {
    if (lun > 0) {
      current = jsfiles_getnext ();
      if (current < JSFILES->_alloc) {
        len = strlen (filename) + 1;
        fname = (char *)malloc (len*sizeof(char));
        strcpy (fname, filename);
        JSFILES->_fnames[current] = fname;
        JSFILES->_irws[current] = jsfiles_isrw (rw);
        JSFILES->_tnums[current] = jsfilesa_gettracenumber (lun);
	JSFILES->_luns[current] = lun;
        retval = current + 1; /* return a 1-relative number */
      }
      else {
        /* no more luns available */
        retval = 0;
      }
    }
  }
  else { /* the virtual lun previously existed */
    /* assert that the produced actual lun is valid */
    assert (lun > 0);
    if (JSFILES->_tnums[current] > 0) {
      /* the trace number stored in JSFILES is likely valid: use it */
      jsfilesa_settracenumber (lun, JSFILES->_tnums[current]);
    }
    if (lun != JSFILES->_luns[current]) {
      /* the lun stored in JSFILES is out of date: update it */
      JSFILES->_luns[current] = lun;
    }
  }
  return retval;
}

/* open the given lun */
int jsfiles_open (int which)
{
  /* wmm added next line */
  return 0;
  return jsfilesa_open (jsfiles_which(which));
}

int jsfiles_close (int which)
{
  int retval;
  char *fname;
  /* wmm added next line */
  return 0;

  int current = which - 1;

  assert (JSFILES != NULL);

  retval = jsfilesa_close (jsfiles_which(which));

  if (current > -1 && current < JSFILES->_alloc) {
    fname = JSFILES->_fnames[current];
    if (fname != NULL) {
      free (fname);
      JSFILES->_fnames[current] = NULL;
      JSFILES->_irws[current] = -1;
      JSFILES->_tnums[current] = 0;
      JSFILES->_luns[current] = 0;
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

int jsfiles_isa (int which)
{
  return jsfilesa_isa (jsfiles_which(which));
}

int jsfiles_status (int which)
{
  return jsfilesa_status (jsfiles_which(which));
}

int jsfiles_message (int which, char *mess, int max_size)
{
  return jsfilesa_message (jsfiles_which(which), mess, max_size);
}

int jsfiles_wtype (int which, char *wtype, int max_size)
{
  return jsfilesa_wtype (jsfiles_which(which), wtype, max_size);
}

long jsfiles_gettracecount (int which)
{
  return jsfilesa_gettracecount (jsfiles_which(which));
}

int jsfiles_getnumdimensions (int which)
{
  return jsfilesa_getnumdimensions (jsfiles_which(which));
}

int jsfiles_getsamplecount (int which)
{
  return jsfilesa_getsamplecount (jsfiles_which(which));
}

float jsfiles_getsamplerate (int which)
{
  return jsfilesa_getsamplerate (jsfiles_which(which));
}

double jsfiles_getlav (int which)
{
  return jsfilesa_getlav (jsfiles_which(which));
}

double jsfiles_getstarttimeinsecs (int which)
{
  return jsfilesa_getstarttimeinsecs (jsfiles_which(which));
}

int jsfiles_getheadercount (int which)
{
  return jsfilesa_getheadercount (jsfiles_which(which));
}

int jsfiles_gettrace (int which, float *trace, int max_size)
{
  return jsfilesa_gettrace (jsfiles_which(which), trace, max_size);
}

int jsfiles_getheaders (int which, double *headers, int max_size)
{
  return jsfilesa_getheaders (jsfiles_which(which), headers, max_size);
}

int jsfiles_puttrace (int which, float *trace, int size)
{
  return jsfilesa_puttrace (jsfiles_which(which), trace, size);
}

int jsfiles_putheaders (int which, double *headers, int size)
{
  return jsfilesa_putheaders (jsfiles_which(which), headers, size);
}

int jsfiles_settracenumber (int which, long trace_num)
{
  return jsfilesa_settracenumber (jsfiles_which(which), trace_num);
}

long jsfiles_gettracenumber (int which)
{
  return jsfilesa_gettracenumber (jsfiles_which(which));
}

int jsfiles_setaxis (int which, int idim, int length, const char *domain,
  const char *units, long logical_org, long logical_del, double physical_org,
  double physical_del)
{
  return jsfilesa_setaxis (jsfiles_which(which), idim, length, domain, units,
    logical_org, logical_del, physical_org, physical_del);
}

int jsfiles_setdatatype (int which, const char *type)
{
  return jsfilesa_setdatatype (jsfiles_which(which), type);
}

int jsfiles_settraceformat (int which, const char *format)
{
  return jsfilesa_settraceformat (jsfiles_which(which), format);
}

int jsfiles_remove (int which)
{
  return jsfilesa_remove (jsfiles_which(which));
}

void jsfiles_delete (void)
{
  int k2;

  if (JSFILES != NULL) {
    for (k2 = 0; k2 < JSFILES->_alloc; k2++) {
      /*printf ("jsfilesa_delete: calling jsfilesa_fclose\n");*/
      jsfiles_close (k2+1);
    }
    free (JSFILES->_fnames);
    free (JSFILES->_irws);
    free (JSFILES->_tnums);
    free (JSFILES->_luns);
    free (JSFILES);
    JSFILES = NULL;
  }
}

int jsfiles_which (int which)
{
  int retval, current;

  assert (JSFILES != NULL);
  current = which - 1;

  if (current > -1 && current < JSFILES->_alloc) {
    retval = JSFILES->_luns[current];
  }
  else {
    retval = 0;
  }
  return retval;
}

int jsfiles_getnext (void)
{
  int retval, k2;

  for (k2 = 0, retval = JSFILES->_alloc; k2 < JSFILES->_alloc; k2++) {
    if (JSFILES->_fnames[k2] == NULL) {
      retval = k2;
      k2 = JSFILES->_alloc;
    }
  }
  /* 
   * notice if too many files then retval = JSFILES->_alloc which
   *   is invalid
   */
  return retval;
}

int jsfiles_exists (const char *filename, const char *rw)
{
  int retval, k2, irw;
  char *fname;

  assert (filename != NULL && rw != NULL);

  irw = jsfiles_isrw (rw);

  for (k2 = 0, retval = -1; k2 < JSFILES->_alloc; k2++) {
    fname = JSFILES->_fnames[k2];
    if (fname != NULL) {
      if (strcmp(fname,filename) == 0) {
        if (irw == 1) {
          if (JSFILES->_irws[k2] != 1) {
            /*
             * if opened file is readonly then close it and act like it
             *   didn't exist!
             */
            /*printf ("jsfilesa_exists: closing %d\n", k2);*/
            jsfiles_close (k2+1);
          }
        }
        else {
          retval = k2; /* return the lun */
        }
      }
    }
  }
  retval++; /* make 1-relative */
  return retval;
}

int jsfiles_isrw (const char *rw)
{
  int retval;

  if (tolower((int)rw[0]) == 'r' && tolower((int)rw[1]) == 'w') {
    retval = 1;
  }
  else {
    retval = 0;
  }
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
