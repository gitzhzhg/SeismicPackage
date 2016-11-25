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


/*------------------------- convert_ii2cc.h -----------------------------*/

/*  This file contains function prototypes and spelling adjustments
    for subroutines in the source code file convert_ii2cc.f.  Only
    those routines which might be called from C are included here.  */

/*  This header file is now obsolete, since C routines should call
    routines in convert_ii2ss.c (using header file cprim.h) instead
    of these routines.  */


#ifndef _CONVERT_II2CC_H
#define _CONVERT_II2CC_H


/*-------- fortran subroutine spelling adjustments for VMS or IBM -------*/

#if (VMS || _AIX || __hpux)
#define convert_ii2hh_             convert_ii2hh
#define convert_ff2hh_             convert_ff2hh
#define convert_dd2hh_             convert_dd2hh
#define convert_hh2ii_             convert_hh2ii
#define convert_hh2ff_             convert_hh2ff
#define convert_hh2dd_             convert_hh2dd
#endif


/*-------- fortran subroutine spelling adjustments for CRAY -------------*/

#ifdef CRAY
#define convert_ii2hh_             CONVERT_II2HH
#define convert_ff2hh_             CONVERT_FF2HH
#define convert_dd2hh_             CONVERT_DD2HH
#define convert_hh2ii_             CONVERT_HH2II
#define convert_hh2ff_             CONVERT_HH2FF
#define convert_hh2dd_             CONVERT_HH2DD
#endif


/*-------------------- function prototypes -----------------------------*/

void   convert_ii2hh_ (long  *, char*, long*       );
void   convert_ff2hh_ (float *, char*, long*, long*);
void   convert_dd2hh_ (double*, char*, long*, long*);
void   convert_hh2ii_ (char*, long  *, long*);
void   convert_hh2ff_ (char*, float *, long*);
void   convert_hh2dd_ (char*, double*, long*);
 

#endif

/*--------------------------- end --------------------------------------*/

