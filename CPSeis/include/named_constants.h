/****
!<CPS_v1 type="HEADER_FILE"/>
****/

/*-------------------------- named_constants.h -----------------------------*/
/*-------------------------- named_constants.h -----------------------------*/
/*-------------------------- named_constants.h -----------------------------*/

                  /* other files are:  named_constants.f90 */

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
! Name       : NAMED_CONSTANTS
! Category   : miscellaneous
! Written    : 1999-06-07   by: Tom Stoeckley
! Revised    : 2010-06-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Defines various named constants used in seismic processing.
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
! 10. 2010-06-29  Stoeckley    Added FEET_PER_METER constant.
!  9. 2010-06-11  Stoeckley    Added SNIL short variable type nil.
!  8. 2009-12-31  Stoeckley    Added MIN and MAX macros, and added C-style
!                               named header word indices.
!  7. 2005-04-21  Stoeckley    Added trace flow constants.
!  6. 2001-02-13  Stoeckley    Change syntax of CNIL definition to eliminate
!                               warning message; add definition for NULL.
!  5. 2000-01-28  Stoeckley    Add FILENAME_LENGTH constant.
!  4. 1999-12-29  Stoeckley    Change constants involving PI to exactly
!                               match the values in named_constants.f90.
!  3. 1999-12-21  Stoeckley    Change constants involving PI to consistent
!                               accuracies, and change FNIL and DNIL from
!                               defined constants to static const variables.
!  2. 1999-09-10  Stoeckley    Add reference to other files.
!  1. 1999-09-02  Stoeckley    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

              /* This header file is the C-language equivalent */
              /* to the Fortran-90 module with the same name,  */
              /* and should be considered part of that module. */

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _NAMED_CONSTANTS_H_
#define _NAMED_CONSTANTS_H_

/*---------------------------- true and false ----------------------------*/
/*---------------------------- true and false ----------------------------*/
/*---------------------------- true and false ----------------------------*/

#ifndef True
#define True 1
#endif
 
#ifndef TRUE
#define TRUE 1
#endif
 
#ifndef False
#define False 0
#endif
 
#ifndef FALSE
#define FALSE 0
#endif
 
#ifndef NULL
#define NULL 0
#endif

/*------------- miscellaneous macros used like functions ------------------*/
/*------------- miscellaneous macros used like functions ------------------*/
/*------------- miscellaneous macros used like functions ------------------*/
 
#ifndef MIN
           #define MIN(a,b) (  (a) < (b) ? (a) :  (b)  )
#endif
#ifndef MAX
           #define MAX(a,b) (  (a) > (b) ? (a) :  (b)  )
#endif
#ifndef ABS
           #define ABS(a)   (  (a) >  0  ? (a) : -(a)  )
#endif

#define MinimumValue(a,b)   (  (a) < (b) ? (a) :  (b)  )
#define MaximumValue(a,b)   (  (a) > (b) ? (a) :  (b)  )
#define AbsoluteValue(a)    (  (a) >  0  ? (a) : -(a)  )
#define AbsValue(a)         (  (a) >  0  ? (a) : -(a)  )
#define NearestInteger(a)   (  (a) >= 0  ? (long)((a) + 0.5)   \
                                         : (long)((a) - 0.5)  )
 
#define ConstrainValue(a,minimum,maximum)                                \
   (  (minimum) < (maximum)                                              \
     ? (  MinimumValue( (maximum), MaximumValue( (minimum), (a) ) )  )   \
     : (  MinimumValue( (minimum), MaximumValue( (maximum), (a) ) )  )  )
 
#define BinNumber(xbin,xcenter,xwidth)   \
   ( NearestInteger( ( (xbin) - (xcenter) ) / (xwidth) ) )
 
#define BinCenter(xbin,xcenter,xwidth)   \
   ( (xcenter) + (float)BinNumber(xbin,xcenter,xwidth) * (xwidth) )
 
#define BinCenter2(xbin_number,xcenter,xwidth)   \
   ( (xcenter) + (float)(xbin_number) * (xwidth) )

/*-------------------------- trace flow constants ------------------------*/
/*-------------------------- trace flow constants ------------------------*/
/*-------------------------- trace flow constants ------------------------*/

#define NO_MORE_TRACES  0
#define FATAL_ERROR    -1
#define NEED_TRACES    -2

/*------------------------------ nil values -----------------------------*/
/*------------------------------ nil values -----------------------------*/
/*------------------------------ nil values -----------------------------*/

/****
#define INIL  -888728
#define FNIL  -1.0e-30
#define DNIL  -1.0e-30
#define CNIL  ""
#define LNIL  0
static const char *CNIL = "";  generated warning message (defined but not used)
****/

static const int     INIL   = -888728;
static const short   SNIL   = -32373;
static const float   FNIL   = -1.0e-30;
static const double  DNIL   = -1.0e-30;
static const char    CNIL[] = "";
static const int     LNIL   = 0;     /* logical FALSE */

     /* NOTICE: The above #define statements for FNIL and DNIL were   */
     /* replaced by the static variables so that these two constants  */
     /* could be used more reliably.  If the #define statements were  */
     /* used instead, you would have to write the following code so   */
     /* that tests for equality would work:                           */
     /*                                                               */
     /*    instead of this:                                           */
     /*                       float  fvar;                            */
     /*                       double dvar;                            */
     /*                          ...                                  */
     /*                       if(fvar == FNIL) { ... }                */
     /*                       if(dvar == DNIL) { ... }                */
     /*                                                               */
     /*    you would have to do this:                                 */
     /*                       float  fvar, fnil = FNIL;               */
     /*                       double dvar, dnil = DNIL;               */
     /*                          ...                                  */
     /*                       if(fvar == fnil) { ... }                */
     /*                       if(dvar == dnil) { ... }                */
     /*                                                               */
     /* The only penalty for using the above static variables is      */
     /* that two words of memory are used in each file which includes */
     /* this header file, whether or not these two variables are      */
     /* actually referenced in that file.                             */

/*------------------------- file name length ----------------------------*/
/*------------------------- file name length ----------------------------*/
/*------------------------- file name length ----------------------------*/

#define FILENAME_LENGTH 140

/* The above constant should be used to specify the length of character     */
/* variables which contain names of disk files.  The intention is that this */
/* length should be large enough for any likely path (including remote user */
/* ID, remote node, and absolute directory path if necessary).              */

/*------------------- CpOffset and CpOffsetOf macros --------------------*/
/*------------------- CpOffset and CpOffsetOf macros --------------------*/
/*------------------- CpOffset and CpOffsetOf macros --------------------*/

          /* this code is taken from Intrinsic.h */
          /* same as XtOffset and XtOffsetOf */
          /* placed here so the user does not need Intrinsic.h */
 
#if defined(CRAY) || defined(__arm)
#ifdef CRAY2
 
#define CpOffset(p_type,field) \
        (sizeof(int)*((unsigned int)&(((p_type)NULL)->field)))
 
#else   /* !CRAY2 */
 
#define CpOffset(p_type,field) ((unsigned int)&(((p_type)NULL)->field))
 
#endif  /* !CRAY2 */
#else   /* ! (CRAY || __arm) */
 
#define CpOffset(p_type,field) \
     ((unsigned int) (((char *) (&(((p_type)NULL)->field))) - ((char *) NULL)))
 
#endif /* !CRAY */
 
#define CpOffsetOf(s_type,field) CpOffset(s_type*,field)
 
#ifdef notdef
/* this doesn't work on picky compilers */
#define CpOffset(p_type,field)  ((unsigned int)&(((p_type)NULL)->field))
#endif
 
/*------------------- trace header word numbers -------------------------*/
/*------------------- trace header word numbers -------------------------*/
/*------------------- trace header word numbers -------------------------*/

#define HDR_NOMINAL_SIZE         64
 
                /* these are also fortran-style indices */

#define HDR_SEQUENCE              1
#define HDR_TOP_MUTE              2
#define HDR_CURRENT_GROUP         3
#define HDR_CURRENT_CHANNEL       4
#define HDR_FOLD                  5
#define HDR_OFFSET                6
#define HDR_MIDPOINT_XGRID        7
#define HDR_MIDPOINT_YGRID        8
#define HDR_ORIGINAL_GROUP        9
#define HDR_ORIGINAL_CHANNEL     10
#define HDR_SOURCE_XLOC          11
#define HDR_SOURCE_YLOC          12
#define HDR_SOURCE_ELEV          13
#define HDR_RECEIVER_XLOC        14
#define HDR_RECEIVER_YLOC        15
#define HDR_RECEIVER_ELEV        16
#define HDR_MIDPOINT_XLOC        17
#define HDR_MIDPOINT_YLOC        18
#define HDR_MIDPOINT_ELEV        19
#define HDR_SOURCE_DEPTH         20
#define HDR_RECEIVER_DEPTH       21
#define HDR_SOURCE_COMPONENT     22
#define HDR_RECEIVER_COMPONENT   23
#define HDR_PANEL                24
#define HDR_LAV                  25
#define HDR_SOURCE_LINE          26
#define HDR_RECEIVER_LINE        27
#define HDR_RECEIVER_SHOTPOINT   28
#define HDR_SOURCE_SHOTPOINT     29
#define HDR_SCRATCH_30           30
#define HDR_SCRATCH_31           31
#define HDR_SCRATCH_32           32
#define HDR_SOURCE_XGRID         33
#define HDR_SOURCE_YGRID         34
#define HDR_RECEIVER_XGRID       35
#define HDR_RECEIVER_YGRID       36
#define HDR_MIDPOINT_SHOTPOINT   37
#define HDR_MIDPOINT_LINE        38
#define HDR_PRE                  39
#define HDR_POST                 40
#define HDR_CUM_DATUM_STATIC     41
#define HDR_CUM_REFR_STATIC      42
#define HDR_CUM_RESID_STATIC     43
#define HDR_SOURCE_UPTIME        44
#define HDR_RECEIVER_UPTIME      45
#define HDR_SOURCE_GP            46
#define HDR_RECEIVER_GP          47
#define HDR_USER_48              48
#define HDR_USER_49              49
#define HDR_USER_50              50
#define HDR_USER_51              51
#define HDR_USER_52              52
#define HDR_USER_53              53
#define HDR_USER_54              54
#define HDR_USER_55              55
#define HDR_RPRE                 56
#define HDR_RPOST                57
#define HDR_SCRATCH_58           58
#define HDR_SCRATCH_59           59
#define HDR_SCRATCH_60           60
#define HDR_SCRATCH_61           61
#define HDR_SCRATCH_62           62
#define HDR_GVS_MODIFIER         63
#define HDR_BOTTOM_MUTE          64

/*------------------- trace header word indices -------------------------*/
/*------------------- trace header word indices -------------------------*/
/*------------------- trace header word indices -------------------------*/

                    /* these are C-style indices */

#define CPS_SEQUENCE              0
#define CPS_TOP_MUTE              1
#define CPS_CURRENT_GROUP         2
#define CPS_CURRENT_CHANNEL       3
#define CPS_FOLD                  4
#define CPS_OFFSET                5
#define CPS_MIDPOINT_XGRID        6
#define CPS_MIDPOINT_YGRID        7
#define CPS_ORIGINAL_GROUP        8
#define CPS_ORIGINAL_CHANNEL      9
#define CPS_SOURCE_XLOC          10
#define CPS_SOURCE_YLOC          11
#define CPS_SOURCE_ELEV          12
#define CPS_RECEIVER_XLOC        13
#define CPS_RECEIVER_YLOC        14
#define CPS_RECEIVER_ELEV        15
#define CPS_MIDPOINT_XLOC        16
#define CPS_MIDPOINT_YLOC        17
#define CPS_MIDPOINT_ELEV        18
#define CPS_SOURCE_DEPTH         19
#define CPS_RECEIVER_DEPTH       20
#define CPS_SOURCE_COMPONENT     21
#define CPS_RECEIVER_COMPONENT   22
#define CPS_PANEL                23
#define CPS_LAV                  24
#define CPS_SOURCE_LINE          25
#define CPS_RECEIVER_LINE        26
#define CPS_RECEIVER_SHOTPOINT   27
#define CPS_SOURCE_SHOTPOINT     28
#define CPS_SCRATCH_30           29
#define CPS_SCRATCH_31           30
#define CPS_SCRATCH_32           31
#define CPS_SOURCE_XGRID         32
#define CPS_SOURCE_YGRID         33
#define CPS_RECEIVER_XGRID       34
#define CPS_RECEIVER_YGRID       35
#define CPS_MIDPOINT_SHOTPOINT   36
#define CPS_MIDPOINT_LINE        37
#define CPS_PRE                  38
#define CPS_POST                 39
#define CPS_CUM_DATUM_STATIC     40
#define CPS_CUM_REFR_STATIC      41
#define CPS_CUM_RESID_STATIC     42
#define CPS_SOURCE_UPTIME        43
#define CPS_RECEIVER_UPTIME      44
#define CPS_SOURCE_GP            45
#define CPS_RECEIVER_GP          46
#define CPS_USER_48              47
#define CPS_USER_49              48
#define CPS_USER_50              49
#define CPS_USER_51              50
#define CPS_USER_52              51
#define CPS_USER_53              52
#define CPS_USER_54              53
#define CPS_USER_55              54
#define CPS_RPRE                 55
#define CPS_RPOST                56
#define CPS_SCRATCH_58           57
#define CPS_SCRATCH_59           58
#define CPS_SCRATCH_60           59
#define CPS_SCRATCH_61           60
#define CPS_SCRATCH_62           61
#define CPS_GVS_MODIFIER         62
#define CPS_BOTTOM_MUTE          63

/*----------------------- mathematical constants -------------------------*/
/*----------------------- mathematical constants -------------------------*/
/*----------------------- mathematical constants -------------------------*/

#ifndef PI
#define PI                    3.1415926535898
#endif

#define RADIANS_PER_DEGREE    0.01745329251994333300
#define DEGREES_PER_RADIAN   57.29577951308219500000

/*
C compilers do not allow arithmetic expressions in static const definitions:
static const double PI                 = 3.1415926535898;
static const double RADIANS_PER_DEGREE = PI / 180.0;
static const double DEGREES_PER_RADIAN = 180.0 / PI;
C++ compilers do allow the above constructs.
*/

/*------------------------- physical constants ---------------------------*/
/*------------------------- physical constants ---------------------------*/
/*------------------------- physical constants ---------------------------*/

#define FEET_PER_METER  3.28084

/*----------------------- operating system flags -------------------------*/
/*----------------------- operating system flags -------------------------*/
/*----------------------- operating system flags -------------------------*/


/*---------------------------- debug flags -------------------------------*/
/*---------------------------- debug flags -------------------------------*/
/*---------------------------- debug flags -------------------------------*/


/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/

#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
