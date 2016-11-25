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
/*----------------------------- jmwrapper.c ---------------------------------*/
/*----------------------------- jmwrapper.c ---------------------------------*/
/*----------------------------- jmwrapper.c ---------------------------------*/
 
        /* other files are:  jmwrapper.h, javavm.h */


/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : jmwrapper 
! Category   : io
! Written    : 2006-08-22   by: Corn
! Revised    : 2008-09-25   by: Corn
! Maturity   : beta
! Purpose    : Generic interface for C classes to call Java classes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!    The purpose of this "C-style class" is to be a generic interface
!    for specific C classes to call on specific Java classes. Although this
!    class provides only a small set of method call formats, it is easily
!    extensible. For example, the only Java Class that can be instatiated
!    using jmwrapper is one having a constructor which has a single string
!    argument. This is intended to be an aid to the transition from C/C++/f90
!    to Java applications.
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
!                 o                              i         i
!                jmw     = jmwrapper_create (class_name, carg1)
!
!                 o                               i         i      i
!                jmw     = jmwrapper_create2 (class_name, argc1, argc2)
!
!                                           i     i      b
!                          jmwrapper_call (jmw, method, mid)
!
!                   o                         i     i      b
!                iretval = jmwrapper_getint (jmw, method, mid)
!
!                   o                          i     i      b
!                lretval = jmwrapper_getlong (jmw, method, mid)
!
!                   o                           i     i      b
!                fretval = jmwrapper_getfloat (jmw, method, mid)
!
!                   o                            i     i      b
!                dretval = jmwrapper_getdouble (jmw, method, mid)
!
!                o                          i     i      b    i
!                ok      = jmwrapper_inti (jmw, method, mid, argi)
!
!                o                          i     i      b    i
!                ok      = jmwrapper_intf (jmw, method, mid, argf)
!
!                o                          i     i      b    i
!                ok      = jmwrapper_intd (jmw, method, mid, argd)
!
!                o                          i     i      b    i
!                ok      = jmwrapper_intc (jmw, method, mid, argc)
!
!                o                          i     i      b    i
!                ok      = jmwrapper_intl (jmw, method, mid, argl)
!
!                 o                             i     i      b    b
!                carr    = jmwrapper_getchars (jmw, method, mid, argc,
!                                                 i
!                                              max_size)
!
!                o                               i     i      b    b
!                ok    = jmwrapper_rtnintgetfa (jmw, method, mid, flts,
!                                                  i
!                                               max_size)
!
!                o                               i     i      b    b
!                ok    = jmwrapper_rtnintputfa (jmw, method, mid, flts,
!                                                i
!                                               size)
!
!                o                               i     i      b    b
!                ok    = jmwrapper_rtnintgetda (jmw, method, mid, dbls,
!                                                  i
!                                               max_size)
!
!                o                               i     i      b    b
!                ok    = jmwrapper_rtnintputda (jmw, method, mid, dbls,
!                                                i
!                                               size)
!
!                o                                i     i      b     i
!                ok    = jmwrapper_getintiiccdd (jmw, method, mid, argi1,
!                                               i      i      i      i
!                                             argi2, argc1, argc2, argd1,
!                                               i
!                                             argd2)
!
!                o                                  i     i      b     i
!                ok    = jmwrapper_getintiicclldd (jmw, method, mid, argi1,
!                                               i      i      i      i
!                                             argi2, argc1, argc2, argl1,
!                                               i      i      i
!                                             argl2, argd1, argd2)
!
!                                             i
!                void      jmwrapper_delete (jmw)
!
! JMWrapper  *jmw        = A pointer used to access Java object
! const char *class_name = Name of the Java Class of interest
! long       lretval     = Value returned by the Java method
! int        iretval     = Value returned by the Java method
! const char *method     = Name of the Java Class method to call
! jmethodID  *mid        = If input as zero, it is returned as a method ID 
! float      fretval     = Value returned by the Java method
! double     dretval     = Value returned by the Java method
! char       *carr       = pointer to chrs populated by the Java method
! char       *chrs       = preallocated char array given to the Java method
! size_t     max_size    = preallocated size of the given array
! size_t     size        = size of the given array
! double     *dbls       = preallocated double array given to the Java
!                          method
! int        ok          = return status (normal = 1)
! float      *flts       = preallocated float array given to the Java method
! int        argi[n]     = nth int argument
! long       argl[n]     = nth long argument
! float      argf[n]     = nth float argument
! double     argd[n]     = nth double argument
! const char *argc[n]    = nth char array argument
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  5. 2008-09-25  CORN       Made use of Global Refs to defeat an
!                              over zealous Java garbage collector.
!                              Also replaced some 0's with NULL's.
!  4. 2008-09-02  CORN       Updated various jmwrapper methods.
!  3. 2008-08-21  CORN       Added various new jmwrapper methods.
!  2. 2007-01-23  CORN       Added new jmwrapper_getlong method.
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
! > -o jmwrapper.o jmwrapper.c
!
! link on sparc using:
! > cc -g -L$JAVA_HOME/jre/lib/sparc -L$JAVA_HOME/jre/lib/sparc/client \
! > -ljava -ljvm -lverify -o app_name app_name.o jmwrapper.o any_other.o
!
! comile on linux using:
! > gcc -c -g -I$JAVA_HOME/include -I$JAVA_HOME/include/linux \
! > -o jmwrapper.o jmwrapper.c
!
! link on linux using:
! > gcc -g -L$JAVA_HOME/jre/lib/i386 -L$JAVA_HOME/jre/lib/i386/client -ljava \
! > -ljvm -lverify -o app_name app_name.o jmwrapper.o any_other.o
!
! run with a CLASSPATH environment variable defined which will allow
! all relevant java packages to be satisfied
!
!-------------------------------------------------------------------------------
!</compile_doc>
****/

char JMWRAPPER_IDENT[100] ="$Id: jmwrapper.c,v 1.1 2009/01/30 21:36:12 mengewm Exp $";


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


#include "jmwrapper.h"
#include "javavm.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif


/* prototypes for some private convenience functions */
int jmwrapper_alloc_byte_array (JMWrapper *jmw,
  size_t size); /* size of byte array to allocate */ 

int jmwrapper_alloc_double_array (JMWrapper *jmw,
  size_t size); /* size of double array to allocate */ 

int jmwrapper_alloc_float_array (JMWrapper *jmw,
  size_t size); /* size of float array to allocate */ 

void jmwrapper_copy_float_to_jfloat (
  float* floats,     /* from array */
  jfloat* jfloats,   /* to array */
  size_t count);     /* size of arrays */

void jmwrapper_copy_jfloat_to_float (
  jfloat* jfloats,   /* from array */
  float* floats,     /* to array */
  size_t count);     /* size of arrays */

void jmwrapper_copy_double_to_jdouble (
  double* doubles,   /* from array */
  jdouble* jdoubles, /* to array */
  size_t count);     /* size of arrays */

void jmwrapper_copy_jdouble_to_double (
  jdouble* jdoubles, /* from array */
  double* doubles,   /* to array */
  size_t count);     /* size of arrays */

void jmwrapper_copy_jbyte_to_char (
  jbyte* jbytes,     /* from array */
  char* chars,       /* to array */
  size_t count);     /* size of arrays */


/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/


/* limited to a java constructor using a java String arg */
JMWrapper *jmwrapper_create (const char *class_name, const char *argc)
{
  int len;
  JMWrapper *jmw = NULL;

  JNIEnv *env;

  jclass loc_cls;
  jobject loc_obj;
  jstring jargc;
  jmethodID mid;

  if (argc != NULL) {
    len = sizeof (argc); /* length at compile time! */
  }
  else {
    len = 0;
  }
  if (len < 1) return NULL;

  /* allocate the memory for the JMWrapper */
  jmw = (JMWrapper *)malloc (sizeof(JMWrapper));
  if (jmw == NULL) return jmw;

  env = (javavm_fetch())->_env;

  /* find the class */
  loc_cls = (*env)->FindClass (env, class_name);
  if (loc_cls == NULL) {
    if ((*env)->ExceptionOccurred(env)) {
      (*env)->ExceptionDescribe (env);
    }
    fprintf (stderr, "Failed to find %s class\n", class_name);
    return NULL;
  }

  /* prevent access to class from being garbage collected */
  jmw->_cls = (*env)->NewWeakGlobalRef (env, loc_cls);
  (*env)->DeleteLocalRef (env, loc_cls);
  if (jmw->_cls == NULL) {
    fprintf (stderr, "Failed to allocate memory for access to %s\n",
      class_name);
    return NULL;
  }

  /* find the constructor */
  mid = (*env)->GetMethodID (env, jmw->_cls, "<init>",
    "(Ljava/lang/String;)V");
  if (mid == NULL) {
    if ((*env)->ExceptionOccurred(env)) {
      (*env)->ExceptionDescribe (env);
    }
    fprintf (stderr, "Failed to find %s constructor\n", class_name);
    return NULL;
  }

  /* create a local Java string to contain the given argc */
  jargc = (*env)->NewStringUTF (env, argc);
  if (jargc == NULL) {
    fprintf (stderr, "Out of memory\n");
    return NULL;
  }

  /* instantiate an object */
  loc_obj = (*env)->NewObject (env, jmw->_cls, mid, jargc);
  if (loc_obj == NULL) {
    fprintf (stderr, "Failed to instantiate a %s object\n", class_name);
    return NULL;
  }

  /* prevent object instance from being garbage collected */
  jmw->_obj = (*env)->NewGlobalRef (env, loc_obj);
  (*env)->DeleteLocalRef (env, loc_obj);
  if (jmw->_obj == NULL) {
    fprintf (stderr, "Failed to allocate memory for instance of %s\n",
      class_name);
    return NULL;
  }

  /* initialize array sizes */
  jmw->_byts_size  = 0;
  jmw->_flts_size  = 0;
  jmw->_dbls_size  = 0;

  return jmw;
}

/* limited to a java constructor using a java String arg */
JMWrapper *jmwrapper_create2 (const char *class_name, const char *argc1,
  const char *argc2)
{
  int len1, len2;
  JMWrapper *jmw = NULL;

  JNIEnv *env;

  jclass loc_cls;
  jobject loc_obj;
  jstring jargc1, jargc2;
  jmethodID mid;

  if (argc1 != NULL) {
    len1 = sizeof (argc1); /* length at compile time! */
  }
  else {
    len1 = 0;
  }
  if (len1 < 1) return NULL;

  if (argc2 != NULL) {
    len2 = sizeof (argc2); /* length at compile time! */
  }
  else {
    len2 = 0;
  }
  if (len2 < 1) return NULL;

  /* allocate the memory for the JMWrapper */
  jmw = (JMWrapper *)malloc (sizeof(JMWrapper));
  if (jmw == NULL) return jmw;

  env = (javavm_fetch())->_env;

  /* find the class */
  loc_cls = (*env)->FindClass (env, class_name);
  if (loc_cls == NULL) {
    if ((*env)->ExceptionOccurred(env)) {
      (*env)->ExceptionDescribe (env);
    }
    fprintf (stderr, "Failed to find %s class\n", class_name);
    return NULL;
  }

  /* prevent access to class from being garbage collected */
  jmw->_cls = (*env)->NewWeakGlobalRef (env, loc_cls);
  (*env)->DeleteLocalRef (env, loc_cls);
  if (jmw->_cls == NULL) {
    fprintf (stderr, "Failed to allocate memory for access to %s\n",
      class_name);
    return NULL;
  }

  /* find the constructor */
  mid = (*env)->GetMethodID (env, jmw->_cls, "<init>",
    "(Ljava/lang/String;Ljava/lang/String;)V");
  if (mid == NULL) {
    if ((*env)->ExceptionOccurred(env)) {
      (*env)->ExceptionDescribe (env);
    }
    fprintf (stderr, "Failed to find %s constructor\n", class_name);
    return NULL;
  }

  /* create a local Java string to contain the given argc1 */
  jargc1 = (*env)->NewStringUTF (env, argc1);
  if (jargc1 == NULL) {
    fprintf (stderr, "Out of memory\n");
    return NULL;
  }

  /* create a local Java string to contain the given argc2 */
  jargc2 = (*env)->NewStringUTF (env, argc2);
  if (jargc2 == NULL) {
    fprintf (stderr, "Out of memory\n");
    return NULL;
  }

  /* instantiate an object */
  loc_obj = (*env)->NewObject (env, jmw->_cls, mid, jargc1, jargc2);
  if (loc_obj == NULL) {
    fprintf (stderr, "Failed to instantiate a %s object\n", class_name);
    return NULL;
  }

  /* prevent object instance from being garbage collected */
  jmw->_obj = (*env)->NewGlobalRef (env, loc_obj);
  (*env)->DeleteLocalRef (env, loc_obj);
  if (jmw->_obj == NULL) {
    fprintf (stderr, "Failed to allocate memory for instance of %s\n",
      class_name);
    return NULL;
  }

  /* initialize array sizes */
  jmw->_byts_size  = 0;
  jmw->_flts_size  = 0;
  jmw->_dbls_size  = 0;

  return jmw;
}

/* limited to a java method using no args with no return */
void jmwrapper_call (JMWrapper *jmw, const char *method, jmethodID *mid)
{
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "()V");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return;
    }
  }
  /* call the method */
  (*env)->CallVoidMethod (env, jmw->_obj, *mid);
}

/* limited to a java method using no args with an int returned */
int jmwrapper_getint (JMWrapper *jmw, const char *method, jmethodID *mid)
{
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "()I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (int)((*env)->CallIntMethod (env, jmw->_obj, *mid));
}

/* limited to a java method using no args with an long returned */
long jmwrapper_getlong (JMWrapper *jmw, const char *method, jmethodID *mid)
{
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "()J");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (long)((*env)->CallLongMethod (env, jmw->_obj, *mid));
}

/* limited to a Java method using no args with a float returned */
float jmwrapper_getfloat (JMWrapper *jmw, const char *method,
  jmethodID *mid)
{
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "()F");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (float)((*env)->CallFloatMethod (env, jmw->_obj, *mid));
}

/* limited to a Java method using no args with a double returned */
double jmwrapper_getdouble (JMWrapper *jmw, const char *method,
  jmethodID *mid)
{
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "()D");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (double)((*env)->CallDoubleMethod (env, jmw->_obj, *mid));
}

/* limited to a Java method using one int arg with an int returned */
int jmwrapper_getinti (JMWrapper *jmw, const char *method, jmethodID *mid,
  int argi)
{
  jint jargi = (jint)argi;
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "(I)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (int)((*env)->CallIntMethod (env, jmw->_obj, *mid, jargi));
}

/* limited to a Java method using one float arg with an int returned */
int jmwrapper_getintf (JMWrapper *jmw, const char *method, jmethodID *mid,
  float argf)
{
  jfloat jargf = (jfloat)argf;
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "(F)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (int)((*env)->CallIntMethod (env, jmw->_obj, *mid, jargf));
}

/* limited to a Java method using one double arg with an int returned */
int jmwrapper_getintd (JMWrapper *jmw, const char *method, jmethodID *mid,
  double argd)
{
  jdouble jargd = (jdouble)argd;
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "(D)V");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (int)((*env)->CallIntMethod (env, jmw->_obj, *mid, jargd));
}

/* limited to a Java method using one double arg with an int returned */
int jmwrapper_getintc (JMWrapper *jmw, const char *method, jmethodID *mid,
  const char *argc)
{
  int len;
  jstring jargc;
  JNIEnv *env = (javavm_fetch())->_env;

  if (argc != NULL) {
    len = sizeof (argc);  /* length at compile time! */
  }
  else {
    len = 0;
  }
  if (len < 1) return 0;

  /* create a local Java string to contain the given chars */
  jargc = (*env)->NewStringUTF (env, argc);
  if (jargc == NULL) {
    fprintf (stderr, "Out of memory\n");
    return 0;
  }

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method,
      "(Ljava/lang/String;)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (int)((*env)->CallIntMethod (env, jmw->_obj, *mid, jargc));
}

/* limited to a Java method using one long arg with an int returned */
int jmwrapper_getintl (JMWrapper *jmw, const char *method, jmethodID *mid,
  long argl)
{
  jlong jargl = (jlong)argl;
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "(J)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return 0;
    }
  }
  /* call the method */
  return (int)((*env)->CallIntMethod (env, jmw->_obj, *mid, jargl));
}

/* limited to a java method using a byte array arg with an int returned */
char *jmwrapper_getchars (JMWrapper *jmw, const char *method, jmethodID *mid,
  char *chrs, size_t max_size)
{
  char *retval = NULL;

  jint jcount;
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "([B)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return retval;
    }
  }

  /* create a local Java byte array to contain the C jbyte array */
  if (jmwrapper_alloc_byte_array(jmw,max_size) == 0) {
    fprintf (stderr, "jmwrapper_getchars: Failed to allocate byte array\n");
    return retval;
  }

  /* call the method to populate the Java byte array */
  jcount = (*env)->CallIntMethod (env, jmw->_obj, *mid, jmw->_jarray_byts);
  assert (jcount <= max_size); /* fatal error */

  if (jcount > 0) {
    /* convert the Java byte array to a C jbyte array */
    (*env)->GetByteArrayRegion (env, jmw->_jarray_byts, (jsize)0,
      (jsize)jcount, jmw->_byts);
    /*fprintf (stderr, "jmwrapper_getchars: chrs input is %s\n", chrs);*/
    retval = chrs;
    jmwrapper_copy_jbyte_to_char (jmw->_byts, retval, (size_t)jcount);
    retval[jcount] = '\0';
  }
  /*fprintf (stderr, "jmwrapper_getchars: retval is %s\n", chrs);*/
  return retval;
}

/* limited to a Java method using two args: incoming float array & size with an
   int returned */
int jmwrapper_rtnintgetfa (JMWrapper *jmw, const char *method,
  jmethodID *mid, float *flts, size_t max_size)
{
  int retval = 0;

  jint jcount;
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "([F)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return retval;
    }
  }

  /* create a local Java float array to contain the float array */
  if (jmwrapper_alloc_float_array(jmw,max_size) == 0) {
    return retval;
  }

  /* call the method to populate the local java float array */
  jcount = (*env)->CallIntMethod (env, jmw->_obj, *mid,
    jmw->_jarray_flts);
  assert (jcount <= max_size); /* fatal error */

  if (jcount > 0) {
    /* convert the Java float array to a C jfloat array */
    (*env)->GetFloatArrayRegion (env, jmw->_jarray_flts, (jsize)0,
      (jsize)jcount, jmw->_flts);
    /* move the C jfloat array */
    jmwrapper_copy_jfloat_to_float (jmw->_flts, flts,
      (size_t)jcount);
    retval = (int)jcount;
  }
  return retval;
}

/* limited to a Java method using two args: outgoing float array & size with an
   int returned */
int jmwrapper_rtnintputfa (JMWrapper *jmw, const char *method,
  jmethodID *mid, float *flts, size_t size)
{
  int retval = 0;

  jint jcount;
  JNIEnv *env = (javavm_fetch())->_env;

  if (size > 0) {
    if (*mid == NULL) {
      /* find the method */ 
      *mid = (*env)->GetMethodID (env, jmw->_cls, method, "([F)I");
      if (*mid == NULL) {
        fprintf (stderr, "Failed to find %s method\n", method);
        return retval;
      }
    }

    /* create a local Java float array to contain the C jfloat array */
    if (jmwrapper_alloc_float_array(jmw,size) == 0) {
      return retval;
    }

    /* move the float array */
    jmwrapper_copy_float_to_jfloat (flts, jmw->_flts, size);
    /* convert the C jfloat array to a Java float array */
    (*env)->SetFloatArrayRegion (env, jmw->_jarray_flts, (jsize)0,
      (jsize)size, jmw->_flts);

    /* call the method to put the Java float array */
    jcount = (*env)->CallIntMethod (env, jmw->_obj, *mid,
      jmw->_jarray_flts);
    retval = (int)jcount;
  }
  return retval;
}

/* limited to a Java method using two args: incoming double array & size with an
   int returned */
int jmwrapper_rtnintgetda (JMWrapper *jmw, const char *method,
  jmethodID *mid, double *dbls, size_t max_size)
{
  int retval = 0;

  jint jcount;
  JNIEnv *env = (javavm_fetch())->_env;

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method, "([D)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return retval;
    }
  }

  /* create a local Java double array to contain the double array */
  if (jmwrapper_alloc_double_array(jmw,max_size) == 0) {
    return retval;
  }

  /* call the method to populate the local java double array */
  jcount = (*env)->CallIntMethod (env, jmw->_obj, *mid,
    jmw->_jarray_dbls);
  assert (jcount <= max_size); /* fatal error */

  if (jcount > 0) {
    /* convert the Java double array to a C jdouble array */
    (*env)->GetDoubleArrayRegion (env, jmw->_jarray_dbls, (jsize)0,
      (jsize)jcount, jmw->_dbls);
    /* move the C jdouble array */
    jmwrapper_copy_jdouble_to_double (jmw->_dbls, dbls,
      (size_t)jcount);
    retval = (int)jcount;
  }
  return retval;
}

/* limited to a Java method using two args: outgoing double array & size with an
   int returned */
int jmwrapper_rtnintputda (JMWrapper *jmw, const char *method,
  jmethodID *mid, double *dbls, size_t size)
{
  int retval = 0;

  jint jcount;
  JNIEnv *env = (javavm_fetch())->_env;

  if (size > 0) {
    if (*mid == NULL) {
      /* find the method */ 
      *mid = (*env)->GetMethodID (env, jmw->_cls, method, "([D)I");
      if (*mid == NULL) {
        fprintf (stderr, "Failed to find %s method\n", method);
        return retval;
      }
    }

    /* create a local Java double array to contain the C jdouble array */
    if (jmwrapper_alloc_double_array(jmw,size) == 0) {
      return retval;
    }

    /* move the double array */
    jmwrapper_copy_double_to_jdouble (dbls, jmw->_dbls, size);
    /* convert the C jdouble array to a Java double array */
    (*env)->SetDoubleArrayRegion (env, jmw->_jarray_dbls, (jsize)0,
      (jsize)size, jmw->_dbls);

    /* call the method to put the Java double array */
    jcount = (*env)->CallIntMethod (env, jmw->_obj, *mid,
      jmw->_jarray_dbls);
    retval = (int)jcount;
  }
  return retval;
}

/* limited to a java method using args: (2 int, 2 Java String, 2 double) with an int returned*/
int jmwrapper_getintiiccdd (JMWrapper *jmw, const char *method, jmethodID *mid,
  int argi1, int argi2, const char *argc1, const char *argc2, double argd1,
  double argd2)
{
  int retval = 0;
  int len1, len2;
  jstring jargc1, jargc2;
  jint jargi1 = (jint)argi1;
  jint jargi2 = (jint)argi2;
  jdouble jargd1 = (jdouble)argd1;
  jdouble jargd2 = (jdouble)argd2;
  JNIEnv *env = (javavm_fetch())->_env;

  if (argc1 != NULL) {
    len1 = sizeof (argc1);  /* length at compile time! */
  }
  else {
    len1 = 0;
  }
  if (len1 < 1) return retval;

  if (argc2 != NULL) {
    len2 = sizeof (argc2);  /* length at compile time! */
  }
  else {
    len2 = 0;
  }
  if (len2 < 1) return retval;

  /* create a Java string to contain the given argc1 */
  jargc1 = (*env)->NewStringUTF (env, argc1);
  if (jargc1 == NULL) {
    fprintf (stderr, "Out of memory\n");
    return retval;
  }

  /* create a Java string to contain the given argc2 */
  jargc2 = (*env)->NewStringUTF (env, argc2);
  if (jargc2 == NULL) {
    fprintf (stderr, "Out of memory\n");
    return retval;
  }

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method,
      "(IILjava/lang/String;Ljava/lang/String;DD)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return retval;
    }
  }
  /* call the int method */
  return (int)((*env)->CallIntMethod (env, jmw->_obj, *mid, jargi1, jargi2, jargc1,
    jargc2, jargd1, jargd2));
}

/* limited to a java method using args: (2 int, 2 Java String, 2 long, 2 double) with ant int returned*/
int jmwrapper_getintiicclldd (JMWrapper *jmw, const char *method, jmethodID *mid,
  int argi1, int argi2, const char *argc1, const char *argc2, long argl1,
  long argl2, double argd1, double argd2)
{
  int retval = 0;

  int len1, len2;
  jstring jargc1, jargc2;
  jint jargi1 = (jint)argi1;
  jint jargi2 = (jint)argi2;
  jlong jargl1 = (jlong)argl1;
  jlong jargl2 = (jlong)argl2;
  jdouble jargd1 = (jdouble)argd1;
  jdouble jargd2 = (jdouble)argd2;
  JNIEnv *env = (javavm_fetch())->_env;

  if (argc1 != NULL) {
    len1 = sizeof (argc1);  /* length at compile time! */
  }
  else {
    len1 = 0;
  }
  if (len1 < 1) return retval;

  if (argc2 != NULL) {
    len2 = sizeof (argc2);  /* length at compile time! */
  }
  else {
    len2 = 0;
  }
  if (len2 < 1) return retval;

  /* create a Java string to contain the given argc1 */
  jargc1 = (*env)->NewStringUTF (env, argc1);
  if (jargc1 == NULL) {
    fprintf (stderr, "Out of memory\n");
    return retval;
  }

  /* create a Java string to contain the given argc2 */
  jargc2 = (*env)->NewStringUTF (env, argc2);
  if (jargc2 == NULL) {
    fprintf (stderr, "Out of memory\n");
    return retval;
  }

  if (*mid == NULL) {
    /* find the method */ 
    *mid = (*env)->GetMethodID (env, jmw->_cls, method,
      "(IILjava/lang/String;Ljava/lang/String;JJDD)I");
    if (*mid == NULL) {
      fprintf (stderr, "Failed to find %s method\n", method);
      return retval;
    }
  }
  /* call the method */
  return (int)((*env)->CallIntMethod (env, jmw->_obj, *mid, jargi1, jargi2, jargc1,
    jargc2, jargl1, jargl2, jargd1, jargd2));
}

/* delete the instance of JMWrapper */
void jmwrapper_delete (JMWrapper *jmw)
{
  JNIEnv *env = (javavm_fetch())->_env;

  if (jmw != NULL) {
    (*env)->DeleteWeakGlobalRef (env, jmw->_cls);
    (*env)->DeleteGlobalRef (env, jmw->_obj);
    if (jmw->_byts_size > 0) free (jmw->_byts);
    if (jmw->_dbls_size > 0) free (jmw->_dbls);
    if (jmw->_flts_size > 0) free (jmw->_flts);
    free (jmw);
  }
}

int jmwrapper_alloc_byte_array (JMWrapper *jmw, size_t size)
{
  int retval;

  jbyte *tmp;
  JNIEnv *env = (javavm_fetch())->_env;

  /* 
   * in the past an attemp was made to use a high-water mark concept
   *   this did not play nice with Java thus jmwrapper insists that the
   *   array sizes are exact matches
   */
  if (size != jmw->_byts_size) {

    /* create the Java byte array */
    jmw->_jarray_byts = (*env)->NewByteArray (env, (jsize)size);
    if (jmw->_jarray_byts == NULL) {
      fprintf (stderr, "Failed to allocate the Java byte array\n");
      retval = 0;
    }
    else {
      if (jmw->_byts_size != 0) {
        free (jmw->_byts);
        jmw->_byts = NULL;
        jmw->_byts_size = 0;
      }
      /* create the C jbyte array */
      tmp = (jbyte *)malloc (size*sizeof(jbyte));
      if (tmp != NULL) {
        /* successfully allocated the array */
        jmw->_byts = tmp;
        jmw->_byts_size  = (jsize)size;
        retval = 1;
      }
      else {
        fprintf (stderr, "Failed to allocate the jbyte array\n");
        retval = 0;
      }
    }
  }
  else {
    retval = 1;
  }
  return retval;
}

int jmwrapper_alloc_double_array (JMWrapper *jmw, size_t size)
{
  int retval;

  jdouble *tmp;
  JNIEnv *env = (javavm_fetch())->_env;

  /* 
   * in the past an attemp was made to use a high-water mark concept
   *   this did not play nice with Java thus jmwrapper insists that the
   *   array sizes are exact matches
   */
  if (size != jmw->_dbls_size) {

    /* create the Java double array */
    jmw->_jarray_dbls = (*env)->NewDoubleArray (env, (jsize)size);
    if (jmw->_jarray_dbls == NULL) {
      fprintf (stderr, "Failed to allocate the Java double array\n");
      retval = 0;
    }
    else {
      if (jmw->_dbls_size != 0) {
        free (jmw->_dbls);
        jmw->_dbls = NULL;
        jmw->_dbls_size = 0;
      }
      /* create the C jdouble array */
      tmp = (jdouble *)malloc (size*sizeof(jdouble));
      if (tmp != NULL) {
        /* successfully allocated the array */
        jmw->_dbls = tmp;
        jmw->_dbls_size  = (jsize)size;
        retval = 1;
      }
      else {
        fprintf (stderr, "Failed to allocate the jdouble array\n");
        retval = 0;
      }
    }
  }
  else {
    retval = 1;
  }
  return retval;
}

int jmwrapper_alloc_float_array (JMWrapper *jmw, size_t size)
{
  int retval;

  jfloat *tmp;
  JNIEnv *env = (javavm_fetch())->_env;

  /* 
   * in the past an attemp was made to use a high-water mark concept
   *   this did not play nice with Java thus jmwrapper insists that the
   *   array sizes are exact matches
   */
  if (size != jmw->_flts_size) {

    /* create the Java float array */
    jmw->_jarray_flts = (*env)->NewFloatArray (env, (jsize)size);
    if (jmw->_jarray_flts == NULL) {
      fprintf (stderr, "Failed to allocate the Java float array\n");
      retval = 0;
    }
    else {
      if (jmw->_flts_size != 0) {
        free (jmw->_flts);
        jmw->_flts = NULL;
        jmw->_flts_size = 0;
      }
      /* create the C jfloat array */
      tmp = (jfloat *)malloc (size*sizeof(jfloat));
      if (tmp != NULL) {
        /* successfully allocated the array */
        jmw->_flts = tmp;
        jmw->_flts_size  = (jsize)size;
        retval = 1;
      }
      else {
        fprintf (stderr, "Failed to allocate the jfloat array\n");
        retval = 0;
      }
    }
  }
  else {
    retval = 1;
  }
  return retval;
}

void jmwrapper_copy_float_to_jfloat (float* floats, jfloat* jfloats,
  size_t count)
{
  size_t k2;
  for (k2 = 0; k2 < count; k2++) {
    jfloats[k2] = (jfloat)floats[k2];
  }
}

void jmwrapper_copy_jfloat_to_float (jfloat* jfloats, float* floats,
  size_t count)
{
  size_t k2;
  for (k2 = 0; k2 < count; k2++) {
    floats[k2] = (float)jfloats[k2];
  }
}

void jmwrapper_copy_double_to_jdouble (double* doubles, jdouble* jdoubles,
  size_t count)
{
  size_t k2;
  for (k2 = 0; k2 < count; k2++) {
    jdoubles[k2] = (jdouble)doubles[k2];
  }
}

void jmwrapper_copy_jdouble_to_double (jdouble* jdoubles, double* doubles,
  size_t count)
{
  size_t k2;
  for (k2 = 0; k2 < count; k2++) {
    doubles[k2] = (double)jdoubles[k2];
  }
}

void jmwrapper_copy_jbyte_to_char (jbyte *jbytes, char *chars, size_t count)
{
  size_t k2;
  for (k2 = 0; k2 < count; k2++) {
    chars[k2] = (char)jbytes[k2];
  }
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
