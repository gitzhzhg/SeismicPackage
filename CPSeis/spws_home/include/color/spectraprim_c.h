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
/*!<CPS_v1 type="HEADER_FILE"/>*/
/*------------------------ spectraprim_c.h ----------------------------------*/
/*------------------------ spectraprim_c.h ----------------------------------*/
/*------------------------ sprctraprim_c.h ----------------------------------*/

                    /* other files are:  spectraprim_c.c */

/****

!<brief_doc>
!-------------------------------------------------------------------------------
!<center>                C P S   P R I M I T I V E               </center>
!
! Name       : spectraprim_c.h
! Category   : miscellaneous
! Written    : 2000-10-02   by: Michael L. Sherrill
! Revised    : 2004-03-17   by: R Selzler
! Maturity   : production
! Purpose    : Provide a C wrapper to the spectra primitives
! Portability: None known
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author                  Description
!     ----        ------                  -----------
!  2. 2004-03-17  R Selzler               Resolve SGI compiler warning.
!  1. 2001-02-01  Michael L. Sherrill     Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef SPECTRAPRIM_C
#define SPECTRAPRIM_C

#include "c2f_interface.h"


#ifdef NEED_UNDERSCORE
#define spectraprimf77 spectraprimf77_
#endif

#ifdef NEED_CAPITALS
#define spectraprimf77 SPECTRAPRIMF77
#endif


#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* see spectraprim_c.c in the CPS primitives transforms directory */
/* Prototypes of functions */

int      spectraprim_c(char   *opt_phase, 
                       int    hdr_tim_shft,      int    hdr_ave_phase,
                       int    hdr_tim_first,     float  freq_fit_beg,
                       float  freq_fit_end,      float  freq_max,
                       float  freq_scale,        float  freq_unwrap_beg, 
                       float  freq_unwrap_end,   float  len_taper,   
                       float  tim_beg,           float  tim_end,  
                       int    ndpt,              int    nwih,
                       float  dt,                float  tstrt,
                       float  *new_sample_rate,  float *tr,
                       double *hd,               float *amps_out,
                       float  *phase_out                              );

int      spectraprimf77(char *opt_phase, 
                       int   *hdr_tim_shft,     int   *hdr_ave_phase,
                       int   *hdr_tim_first,    float *freq_fit_beg,
                       float *freq_fit_end,     float *freq_max,
                       float *freq_scale,       float *freq_unwrap_beg, 
                       float *freq_unwrap_end,  float *len_taper,   
                       float *tim_beg,          float *tim_end,  
                       int   *ndpt,             int   *nwih,
                       float *dt,               float *tstrt,
                       float  *new_sample_rate, float *tr, 
                       double *hd,              float *amps_out, 
                       float  *phase_out                             );

#ifdef __cplusplus
}                   // for C++
#endif

#endif
