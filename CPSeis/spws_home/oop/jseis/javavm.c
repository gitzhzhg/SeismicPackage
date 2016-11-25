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
/*------------------------------- javavm.c ----------------------------------*/
/*------------------------------- javavm.c ----------------------------------*/
/*------------------------------- javavm.c ----------------------------------*/
 
        /* other files are:  javavm.h */
 

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : javavm 
! Category   : io
! Written    : 2006-08-22   by: Corn
! Revised    : 2008-09-25   by: Corn
! Maturity   : beta
! Purpose    : Invokes the Java Virtual Machine.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!    The purpose of this "C-style class" is to invoke the Java Virtual
!    Machine intended to assist in making calls on Java classes from
!    legacy C/C++ codes. This will be an aid to the transition from
!    C/C++/f90 to Java applications.
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
!
!           void    javavm_create ()
!
!             o 
!          javima = javavm_fetch ()
!
! JaViMa  *javima = An instance of a Java Virtual Machine
!
!           void    javavm_delete ()
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2008-09-25  CORN       Increased JVM memory to 2G.
!  2. 2008-08-21  CORN       Updated to Java 1.6.
!  1. 2006-08-22  CORN       Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! Use with Java 5.0 or later. To successfully run an application using
! javavm, a CLASSPATH environment variable must be defined which permits
! the sourcing of all relevant Java classes and packages. The CLASSPATH
! is assumed to be less than 2048 characters. At this time, only one
! Java Virtual Machine is permitted by way of a single static pointer.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
!
! comiple on sparc using:
! > cc -c -g -I$JAVA_HOME/include -I$JAVA_HOME/include/solaris -o javavm.o \
! > javavm.c
!
! link on sparc using:
! > cc -g -L$JAVA_HOME/jre/lib/sparc -L$JAVA_HOME/jre/lib/sparc/client \
! > -ljava -ljvm -lverify -o app_name app_name.o javavm.o any_other.o
!
! comile on linux using:
! > gcc -c -g -I$JAVA_HOME/include -I$JAVA_HOME/include/linux -o javavm.o \
! > javavm.c
!
! link on linux using:
! > gcc -g -L$JAVA_HOME/jre/lib/i386 -L$JAVA_HOME/jre/lib/i386/client -ljava \
! > -ljvm -lverify -o app_name app_name.o javavm.o any_other.o
!
! run with a CLASSPATH environment variable defined which will allow
! all relevant java packages to be satisfied
!
!-------------------------------------------------------------------------------
!</compile_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/

char JAVAVM_IDENT[100] ="$Id: javavm.c,v 1.3 2008/09/26 13:33:42 CORN beta sps $";


#include "javavm.h"
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif


static char option_string[4096];
static char memory[12];

static JaViMa *JVM = NULL;


/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/


void javavm_create ()
{
  char *classpath;
  JavaVMInitArgs vm_args;
  JavaVMOption options[2];
  jint result;

  if (JVM == NULL) {
    JVM = (JaViMa *)malloc (sizeof(JaViMa));
    strcpy (option_string, "-Djava.class.path=");
    classpath = getenv ("CLASSPATH");
    strcat (option_string, classpath);
    strcpy (memory, " -Xms256m");
    strcat (option_string, memory);
    strcpy (memory, " -Xmx2G");
    strcat (option_string, memory);
    /*printf ("JVM option string is %s\n", option_string);*/

    options[0].optionString = option_string;

    memset (&vm_args, 0, sizeof(vm_args));
    vm_args.version = JNI_VERSION_1_6;
    /*vm_args.version = JNI_VERSION_1_4;*/
    /*vm_args.version = JNI_VERSION_1_2;*/
    vm_args.options = options;
    vm_args.nOptions = 1;
    vm_args.ignoreUnrecognized = 1;

    result = JNI_CreateJavaVM (&(JVM->_jvm), (void **)&(JVM->_env), &vm_args);
    if (result < 0) {
      fprintf (stderr, "Failed to create the Java VM\n");
      return;
    }
  }
}

JaViMa *javavm_fetch ()
{
  javavm_create ();
  return JVM;
}

void javavm_delete ()
{
  if (JVM != NULL) {
    (*(JVM->_jvm))->DestroyJavaVM (JVM->_jvm); /* has been buggy! */
    free (JVM);
    JVM = NULL;
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
