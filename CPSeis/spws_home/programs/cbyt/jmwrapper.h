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
/*----------------------------- jmwrapper.h ---------------------------------*/
/*----------------------------- jmwrapper.h ---------------------------------*/
/*----------------------------- jmwrapper.h ---------------------------------*/

    /* other files are:  jmwrapper.c */
 
/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                      C P S   H E A D E R   F I L E
!
! Name       : jmwrapper
! Category   : io
! Written    : 2006-08-22   by: Corn
! Revised    : 2008-09-02   by: Corn
! Maturity   : beta
! Purpose    : Generic interface for C classes to call java classes.
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
!  4. 2008-09-02  CORN         Updated various jmwrapper methods.
!  3. 2008-08-21  CORN         Added various new jmwrapper methods.
!  2. 2007-01-23  CORN         Added new jmwrapper_getlong method.
!  1. 2006-08-22  Corn         Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _JMWRAPPER_H_
#define _JMWRAPPER_H_

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif


/*-------------------- abstract data definitions -------------------------*/
/*-------------------- abstract data definitions -------------------------*/
/*-------------------- abstract data definitions -------------------------*/


struct _JMWrapper
{
  jclass _cls;
  jobject _obj;

  jarray _jarray_byts;
  jbyte *_byts;
  jsize _byts_size;

  jarray _jarray_flts;
  jfloat *_flts;
  jsize _flts_size;

  jarray _jarray_dbls;
  jdouble *_dbls;
  jsize _dbls_size;
};

typedef struct _JMWrapper JMWrapper;


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/



/* limited to a constructor with one Java String arg                   */
JMWrapper *jmwrapper_create  /* constructor                            */
  (const char *class_name,   /*   name of Java class                   */
   const char *argc);        /*   argument char string                 */

/* limited to a constructor with two Java String args                  */
JMWrapper *jmwrapper_create2 /* constructor                            */
  (const char *class_name,   /*   name of Java class                   */
   const char *argc1,        /*   first char string                    */
   const char *argc2);       /*   second char string                   */

/* limited to a Java method using no args with no return               */
void jmwrapper_call          /* No return                              */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid);          /*   jmethodID associated with method     */ 

/* limited to a Java method using no args with an int returned         */
int jmwrapper_getint         /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call */
   jmethodID *mid);          /*   jmethodID associated with method */ 

/* limited to a Java method using no args with an long returned        */
long jmwrapper_getlong       /* Return a long                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call */
   jmethodID *mid);          /*   jmethodID associated with method */ 

/* limited to a Java method using no args with a float returned */
float jmwrapper_getfloat     /* Return a float                         */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /* name of method to call                 */
   jmethodID *mid);          /* jmethodID associated with method       */ 

/* limited to a Java method using no args with a double returned       */
double jmwrapper_getdouble   /* Return a double                        */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid);          /*   jmethodID associated with method     */ 

/* limited to a Java method using one int arg with an int returned     */
int jmwrapper_getinti       /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */ 
   int argi);                /*   one arg                              */

/* limited to a Java method using one float arg with an int returned   */
int jmwrapper_getintf       /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */ 
   float argf);              /*   one arg                              */

/* limited to a Java method using one double arg with an int returned  */
int jmwrapper_getintd       /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */ 
   double argd);             /*   one arg                              */

/* limited to a Java method using one Java String arg with an int returned*/
int jmwrapper_getintc       /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */ 
   const char *argc);        /*   one arg to put                       */

/* limited to a Java method using one long arg with an int returned    */
int jmwrapper_getintl       /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */ 
   long argl);               /*   one arg                              */

/* limited to a Java method using one byte array arg with an int returned */
char *jmwrapper_getchars     /* Return the char array                  */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */
   char *chars,              /*   array of chars preallocated          */
   size_t max_size);         /*   chars is at least max_size size      */

/* limited to a Java method using two args: incoming float array & size with an int returned */
int jmwrapper_rtnintgetfa    /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */
   float *flts,              /*   preallocated array                   */
   size_t max_size);         /*   array is at least max_size size      */

/* limited to a Java method using two args: outgoing float array & size with an int returned */
int jmwrapper_rtnintputfa    /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */
   float *flts,              /*   preallocated array                   */
   size_t size);             /*   size of array                        */

/* limited to a Java method using two args: incoming double array & size with an int returned */
int jmwrapper_rtnintgetda    /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */
   double *dbls,             /*   preallocated array                   */
   size_t max_size);         /*   array is at least max_size size      */

/* limited to a Java method using two args: outgoing double array & size with an int returned */
int jmwrapper_rtnintputda    /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */
   double *dbls,          /*   preallocated array                   */
   size_t size);             /*   size of array                        */

/* limited to a Java method using args: (2 int, 2 Java String, 2 double) with an int returned*/
int jmwrapper_getintiiccdd   /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */ 
   int argi1,                /*   the first int arg                    */
   int argi2,                /*   the second int arg                   */
   const char *argc1,        /*   1st char string                      */
   const char *argc2,        /*   2nd char string                      */
   double argd1,             /*   the first double arg                 */
   double argd2);            /*   the second double arg                */

/* limited to a Java method using args: (2 int, 2 Java String, 2 long, 2double) with an int return*/
int jmwrapper_getintiicclldd /* Return an int                          */
  (JMWrapper *jmw,           /*   given JMWrapper object               */
   const char *method,       /*   name of method to call               */
   jmethodID *mid,           /*   jmethodID associated with method     */ 
   int argi1,                /*   the first int arg                    */
   int argi2,                /*   the second int arg                   */
   const char *argc1,        /*   1st char string                      */
   const char *argc2,        /*   2nd char string                      */
   long argl1,               /*   the first long arg                   */
   long argl2,               /*   the second long   arg                */
   double argd1,             /*   the third double arg                 */
   double argd2);            /*   the fourth double arg                */
 
void jmwrapper_delete        /* destructor                             */
  (JMWrapper *jmw);          /*   given JMWrapper object               */



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
