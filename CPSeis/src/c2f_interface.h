/*<CPS_v1 type="HEADER_FILE",pretag="!"/>*/
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

/*----------------------------- c2f_interface.h ---------------------------*/
/*----------------------------- c2f_interface.h ---------------------------*/
/*----------------------------- c2f_interface.h ---------------------------*/


/****
!<CPS_v1 type="HEADER_FILE"/>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : c2f_interface
! Category   : miscellaneous
! Written    : 1999-09-02   by: Tom Stoeckley
! Revised    : 2008-08-19   by: Kruger Corn
! Maturity   : beta
! Purpose    : Functions to read and write tetrahedral model files
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY 
!
!     Date        Author       Description
!     ----        ------       -----------
! 11. 2008-08-19  Corn         add LONG variables
! 10. 2007-12-04  Robert Kern  fixed int INTEGER and related issues.
!  9. 2004-12-15  Stoeckley    Change -DLINUX to -DLINUXA.
!  8. 2002-07-29  Vunderink    Add NEED_UNDERSCORE designation for LINUXI.
!  7. 2001-10-30  Stoeckley    Remove NEED_CAPITALS designation for LINUXI.
!  6. 2001-10-16  Stoeckley    Simplify and improve the documentation about
!                               calling Fortran classes from C and vice versa;
!                               add NEED_CAPITALS designation for LINUXI.
!  5. 2000-08-04  Stoeckley    Add documentation about calling Fortran classes
!                               from C or vice versa.
!  4. 1999-11-17  Stoeckley    Simplify based on agreed standards by removing
!                               symbols LINUX and REMOVE_UNDERSCORE and
!                               REMOVE_UNDERSCORE_AND_NEED_CAPITALS, and
!                               changing PORTLAND to LINUXP, and ABSOFT to
!                               LINUX.  Also simplify sample code accordingly.
!  3. 1999-10-06  Stoeckley    Add symbols LINUX and REMOVE_UNDERSCORE and
!                               REMOVE_UNDERSCORE_AND_NEED_CAPITALS.
!                               Also add WORDSIZE definition which can be used
!                               if needed at precompile time.
!  2. 1999-09-10  Stoeckley    Minor documentation changes.
!  1. 1999-09-02  Stoeckley    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/****
!-------------------------------------------------------------------------------
                       HOW TO USE THIS HEADER FILE

  (1) WHERE TO USE THIS HEADER FILE:
      Include this header file in any C or C++ code which calls Fortran code
      or contains C or C++ functions called from Fortran.
 
  (2) FLOATING POINT VARIABLES:
      For all floating point variables passed to or from Fortran, use the
      type REAL instead of float or double.  Currently, float seems to work
      everywhere except on some 64-bit machines, where double may be required,
      depending on compiler options.
 
  (3) DOUBLE PRECISION VARIABLES:
      For all double precision variables passed to or from Fortran, use the
      type DOUBLE instead of float or double.  Currently, double seems to
      everywhere except on some 64-bit machines, where real may sometimes
      be required, depending on compiler options.
 
  (4) INTEGER VARIABLES:
      For all integer-type variables passed to or from Fortran, use the type
      INTEGER instead of char or short or int for future portability
      assurance. Except for long, int seems to work everywhere we have tried.
 
  (5) LOGICAL VARIABLES:
      Do not try to pass logical variables to or from Fortran.  Use integer
      variables instead, setting them to zero for FALSE and non-zero for
      TRUE.  In Fortran, you can use the CONVERT primitive convenience
      routines CONVERT_II2LL and CONVERT_LL2II (and additional similar
      routines) to copy between integer and logical variables if you wish.
 
  (6) CHARACTER VARIABLES:
      Do not try to pass character variables to or from Fortran.  Use integer
      arrays in Fortran, and treat these as character arrays in C or C++.
      The integer arrays must contain character (hollerith) data and be
      null-terminated.  In Fortran, use the STRING primitive subroutines
      STRING_CC2HH and STRING_HH2CC (and additional similar routines) to
      copy between character variables and null-terminated integer arrays.

  (7) SPELLING ADJUSTMENTS AND PROTOTYPES:
      Write code similar to the SAMPLE CODE shown below for each Fortran
      routine called from C or C++, or each C or C++ function called from
      Fortran.  Put this code into the C or C++ implementation file which
      contains the C or C++ functions called from Fortran and/or contains
      calls from C or C++ functions to Fortran.  Put this information
      instead into a header file only if it might be needed in more than
      one C or C++ implementation file.

  (8) CALLING RESTRICTIONS:
      C and C++ code cannot call Fortran routines in a Fortran-90 module.
      Fortran and C code cannot call C++ functions which are members of a
      class, or C++ functions which do not have the extern "C" attribute.
 
  (9) ARGUMENT PASSING:
      In C or C++ code, receive or pass all arguments by address (or by
      reference) rather than by value, and use only integer and real
      arguments as much as possible.
 
 (10) FORTRAN POINTERS IN C CODE:
      If storing a Fortran-90 pointer in C or C++ code, store it in a
      F90Pointer structure (defined in this header file), and do not try
      to use or modify it.

 (11) C POINTERS IN FORTRAN CODE:
      If storing a C or C++ pointer in Fortran code, store it in a Fortran-90
      variable of derived type CPOINTER (defined in the NAMED_CONSTANTS
      module), and do not try to use or modify it.

 (12) LONG VARIABLES:
      For integer(kind=8) variables passed to or from Fortran, use the type
      LONG instead of long for future portability assurance.

!-------------------------------------------------------------------------------
                              SAMPLE CODE
 
    #include "c2f_interface.h"

    #if NEED_UNDERSCORE
    #define sub1    sub1_
    #define sub2    sub2_
    #define sub3    sub3_
    #define SUB1    sub1_    (can usually be omitted - see note below)
    #define SUB2    sub2_    (can usually be omitted - see note below)
    #define SUB3    sub3_    (can usually be omitted - see note below)
    #elif NEED_CAPITALS
    #define sub1    SUB1
    #define sub2    SUB2
    #define sub3    SUB3
    #define sub1_   SUB1     (can usually be omitted - see note below)
    #define sub2_   SUB2     (can usually be omitted - see note below)
    #define sub3_   SUB3     (can usually be omitted - see note below)
    #else                    (can usually be omitted - see note below)
    #define SUB1    sub1     (can usually be omitted - see note below)
    #define SUB2    sub2     (can usually be omitted - see note below)
    #define SUB3    sub3     (can usually be omitted - see note below)
    #define sub1_   sub1     (can usually be omitted - see note below)
    #define sub2_   sub2     (can usually be omitted - see note below)
    #define sub3_   sub3     (can usually be omitted - see note below)
    #endif
 
    REAL     sub1 ();
    void     sub2 ();
    INTEGER  sub3 (F90Pointer *ppp, INTEGER *aaa, REAL *bbb);
 
 NOTE: The three functions listed above can be either C or C++ functions
       called from Fortran, or Fortran routines called from C or C++.

 NOTE: The lines so indicated can be OMITTED if you do not spell Fortran-
       callable C and C++ function names, or Fortran routines called from
       C or C++, with a trailing underscore or with capital letters.
       Therefore it is recommended that you do NOT spell with a trailing
       underscore or with capital letters.

!-------------------------------------------------------------------------------
              EXAMPLES OF CALLING A FORTRAN OBJECT FROM C OR C++
                  OR CALLING A C OR C++ OBJECT FROM FORTRAN

 The following working templates (and actual examples) show how to call a
 Fortran-90 module from C or C++ or vice versa, including a portable way
 to store a Fortran-90 pointer in C or C++ code, or a C or C++ pointer
 in Fortran code.  The working templates (in ~sps/templates) can be compiled
 and run as they are.

 See the following working templates (calling Fortran from C):

       heidi_wrapper.h    (the C wrapper) 
       heidi_wrapper.c    (the C wrapper) 
       heidi_frou.f90     (private auxiliary file)
       heidi.f90          (the original Fortran-90 class)
 
 See the following working templates (calling Fortran from C++):

       andrew_wrapper.hh  (the C++ wrapper) 
       andrew_wrapper.cc  (the C++ wrapper) 
       andrew_frou.f90    (private auxiliary file)    (same as heidi_frou.f90)
       andrew.f90         (the original Fortran-90 class)  (same as heidi.f90)
 
 See the following working templates (calling C from Fortran):

       kathy_wrapper.f90  (the Fortran-90 wrapper) 
       kathy_crou.c       (private auxiliary file)
       kathy.h            (the original C class) 
       kathy.c            (the original C class)

 See the following working templates (calling C++ from Fortran):

       suki_wrapper.f90   (the Fortran-90 wrapper) (same as kathy_wrapper.f90) 
       suki_crou.cc       (private auxiliary file)
       suki.hh            (the original C++ class) 
       suki.cc            (the original C++ class)
 
 See the following actual example (calling Fortran from C++):

       grid_transform.hh  (the C++ GridTransform wrapper in ~spws)
       grid_transform.cc  (the C++ GridTransform wrapper in ~spws)
       grid_frou.f90      (private auxiliary file in ~spws)
       grid.f90           (the original Fortran-90 GRID primitive in ~sps)

 See the following actual example (calling C from Fortran):

       mute.f90    (the Fortran-90 MUTE process in ~sps)
       mutefile.h  (the original C class including a Fortran-callable wrapper)
       mutefile.c  (the original C class including a Fortran-callable wrapper)

 The "_wrapper" suffix is a reasonable addition to the standards if we find
 the need to add wrappers to ~sps.  (Currently these wrappers are needed
 primarily in ~spws.)  These wrappers differ from auxiliary files in that
 they are intended to be publicly callable and contain their own complete
 documentation.

 The wrappers illustrated here (except for MUTE) are considered to be part
 of the same primitive as the original primitive which they call.  There is
 of course nothing wrong with writing instead an entirely new primitive (with
 its own unique name) as a wrapper around another primitive.  If this is done,
 the files with the _WRAPPER suffix above should instead be the main files in
 the new primitive, and the files with the _FROU and _CROU suffix should
 normally be private auxiliary files in the new primitive.

!-------------------------------------------------------------------------------
****/


/*--------------------------- start of module -----------------------------*/
/*--------------------------- start of module -----------------------------*/
/*--------------------------- start of module -----------------------------*/


#ifndef _C2F_INTERFACE_H_
#define _C2F_INTERFACE_H_

/* RTK */
#include <sys/types.h>

/*-------------------------- general definitions ----------------------------*/
/*-------------------------- general definitions ----------------------------*/
/*-------------------------- general definitions ----------------------------*/


#if (ultrix || sun || __sgi || LINUXP || LINUXI)
#define NEED_UNDERSCORE  1
#elif (CRAY || LINUXA)
#define NEED_CAPITALS    1
#endif


#ifdef _CRAYMPP
#define REAL    double
#define DOUBLE  double
#define INTEGER int
#define LONG    long
#define REAL_is_double     1
#define DOUBLE_is_double   1
#define INTEGER_is_int     1
#define LONG_is_long       1
#else
#define REAL    float
#define DOUBLE  double
/* RTK */
#define INTEGER int
#define LONG    long
#define REAL_is_float      1
#define DOUBLE_is_double   1
#define INTEGER_is_int     1
#define LONG_is_long       1
#endif

typedef struct _F90Pointer
  {
  int a[50];
  } F90Pointer;


/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

