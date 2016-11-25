/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*---------------------------- apin_crou.c -------------------------------*/
/*---------------------------- apin_crou.c -------------------------------*/
/*---------------------------- apin_crou.c -------------------------------*/
 
        /* other files are:  */
 

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
! Name       : apin_crou
! Category   : amplitude_mod
! Written    : 2004-02-18   by: Michael Ried
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : To identify AVO anomalies and their AVO class type
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
!!!  AVO Product indicator routines written in C
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
!!!  --> This is a REQUIRED section which CANNOT be omitted.
!!!
!!!  --> This section should list all functions in this primitive, and
!!!  --> information about their arguments, in a format similar to that
!!!  --> illustrated here, customized as appropriate.
!!!
!!!  --> If this primitive contains several groups of related functions which
!!!  --> would be more clearly documented separately, or if this primitive
!!!  --> contains functions which require sufficient documentation to warrant
!!!  --> a separate section, there can be several sections such as this one,
!!!  --> appropriately individually titled.
!
!                        i              i            i
!           void apin_ms(nProd_part_in, nOverlay_in, nProd_type_in,
!
!             i              i             i           i          i
!             nNorm_type_in, nNorm_exp_in, nSmooth_in, nSamps_in, fSrate_in,
!
!             o    o    o     o      i         i         b     b     o 
!             hci, rvi, norm, norm1, nHdrs_in, nTrcs_in, Hdrs, trcs, nJerror,
!
!             o      o
!             szRst, nRstln) 
!
! int       *nProd_part_in = Part of the product (Real,Imaginary,Overlay)
! int       *nOverlay_in   = Overlay product with zero offset traces (Yes,No)
! int       *nProd_type_in = Type of product indicator
! int       *nNorm_type_in = Type of normalization factor
! int       *nNorm_exp_in  = Normalization exponent
! int       *nSmooth_in    = Length of smoothing filter
! int       *nSamps_in     = Number of samples
! float     *fSrate_in     = Sample rate
! float     *hci           = Imaginary part of AB*
! float     *rvi           = Real part of AB*
! float     *norm          = Normalization values
! float     *norm1         = Normatization values (#2)
! int       *nHdrs_in      = Number of headers
! int       *nTrcs_in      = Number of traces
! float     *Hdrs          = Header values
! float     *trcs          = Traces values
! int       *nJerror       = Error value
! char       szRst[]       = Return String
! int       *nRstln        = Length of return string
!
!                                         b     i      i       i
!                int apin_avoCheckSuite(Hdrs, nHdrs, stdHdr, nTrcs)
!
! float     *Hdrs     = Header to check
! int       nHdrs     = # of Headers values
! int       *StdHdr   = A standard Header structure
! int       nTrcs     = # of Traces
!
!                                    o    i   i   i   i   i   i
!                void apin_avoDelta(prod, r, s1, s2, tr, ti, ns)
!
! float     *prod     = Current product
! int       *r        = A correlation trace
! int       *s1       = A standard deviation trace
! int       *s2       = A standard deviation trace
! int       *tr       = A real trace
! int       *ti       = An imaginary trace
! int       ns        = # of Samples
!
!                                    o    o    i     i   i
!                void apin_avoPnorm(hci, rvi, norm, ns, norm_exp)
!
! float     *hci      = Imaginary part of AB*
! float     *rvi      = Real part of AB*
! float     *norm     = Normalization values
! int       ns        = Number of samples 
! Norm_exp  norm_exp  = Normalization type
!
!                                   i   o    i       i
!                void apin_avovsmul(in, out, scalar, ns)
!
! float     *in       =  Input vector array
! float     *out      =  Output vectory array
! float     scalar    =  Scalar value
! int       ns        =  # of samples
!
!                                         i   i   i   i   o     o     i
!                void apin_complexProduct(ar, ai, br, bi, outr, outi, ns)
!
! float     *ar       =  The real "A" trace
! float     *ai       =  The imaginary "A" trace
! float     *br       =  The real "B" trace
! float     *bi       =  The imaginary "B" trace
! float     *outr     =  The output real product
! float     *outi     =  The output imaginary product
! int       ns        =  # of samples
!
!                              o
!                void apin_end(nJerror)
!
! int      *nJerror   =  Error number
!
!                                     o    i  i  i
!                void apin_Magnitude(norm, a, b, ns)
!
! float   *norm       = Normalization value
! float   *a          = A values
! float   *b          = B values
! int     ns          = Number of samples
!
!                                     i    o     i   i
!                void apin_MovingAvg(ain, aout, ns, ntwin)
!
!
!!!  --> The type and description of each argument should be shown in a table
!!!  --> such as that shown below.  
!
! float   *ain        = The array to average
! float   *aout       = The averaged array
! int     ns          = The length of the arrays
! int     ntwin       = # samples in moving average
!
!                                      b     i
!                void apin_SquareRoot(trace, ns)
!
! float   *trace      = The current trace
! int     ns          = The length of the trace
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  2. 2005-05-31  Stoeckley     Fix to compile with C++.
!  1. 2005-01-03  Michael Ried  Put into CPS from Omega
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char APIN_CROU[100] =
"$Id: apin_crou.c,v 1.2 2005/05/31 13:04:07 Stoeckley prod sps $";

/****
!!!
!!! Put your header files here.
!!!
*/

#include "named_constants.h"
#include "c2f_interface.h"
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>

#ifdef NEED_UNDERSCORE
#define apin_avoCheckSuite  apin_avoCheckSuite_
#define apin_avoDelta  apin_avoDelta_
#define apin_avoPnorm  apin_avoPnorm_
#define apin_avovsmul  apin_avovsmul_
#define apin_complexProduct  apin_complexProduct_
#define apin_end  apin_end_
#define apin_Magnitude  apin_Magnitude_
#define apin_MovingAvg  apin_MovingAvg_
#define apin_ms  apin_ms_
#define apin_SquareRoot  apin_SquareRoot_
#elif defined NEED_CAPITALS
#define apin_avoCheckSuite  APIN_AVOCHECKSUITE
#define apin_avoDelta  APIN_AVODELTA
#define apin_avoPnorm  APIN_AVOPNORM
#define apin_avovsmul  APIN_AVOVSMUL
#define apin_complexProduct  APIN_COMPLEXPRODUCT
#define apin_end  APIN_END
#define apin_Magnitude  APIN_MAGNITUDE
#define apin_MovingAvg  APIN_MOVINGAVG
#define apin_ms  APIN_MS
#define apin_SquareRoot  APIN_SQUAREROOT
#endif

/****
!!!
!!! Put program header files here.
!!!
*/

#include "apin.h"

/*
 *  This ProMAX socket tool computes some standard AVO product
 *  indicators from a group of AVO standard suites.
 *
 *  Author:  Herb Swan     7-5-95
 */

/* Include ProMAX prototypes and globals */
/* #include "cpromax.h" */
/* #include "cglobal.h" */

static int nens_input=0, nens_valid=0, nEndfg=0, nEcalls=0;

#ifdef __cplusplus
extern "C" {
#endif

void apin_ms(int *nProd_part_in, int *nOverlay_in, int *nProd_type_in,
             int *nNorm_type_in, int *nNorm_exp_in, int *nSmooth_in,
             int *nSamps_in, float *fSrate_in, float *hci, float *rvi,
             float *norm, float *norm1, int *nHdrs_in, int *nTrcs_in,
             float *Hdrs, float *trcs, int *nJerror, char szRst[],
             int *nRstln) 
{

  /* int kbytes; */
  int nSamps, invalid;
  int nTrcs, overlay, i;
  float fSrate;
  int Ar, Ai, Br, Bi;
  int nArp, nAip, nBrp, nBip;
  int nRrp, nRip, nSbp, nSap;
  int nHdrs, nInitfg, nMsg_no, nHptr, nTptr;
  static Prod_part prod_part;
  static Prod_type prod_type;
  static Norm_type norm_type;
  static Norm_exp norm_exp;
  static StdHdr stdHdr;
  static int  outloc, outcode, smooth;
  static float *outptr;

/*
 *  Initialize
 */
  nInitfg = 1;
  if (nens_input > 0 || nEndfg == 1)
    nInitfg = 0;
  nJerror = 0;

  nHdrs = *nHdrs_in;
  nSamps = *nSamps_in;
  fSrate = *fSrate_in;
  nTrcs = *nTrcs_in;
/*
 *  The "init" phase:
 */
  if (nInitfg == 1) {
/*
 *  Check the ensemble size.
 */
    if (nTrcs < AVO_NStd) {
      /* stErrFatal ("Not a full standard suite is input."); */
      nMsg_no = 260;
      *nJerror = nMsg_no;
      strcpy(szRst,"apin_ms: The input is not a full standard suite!");
      *nRstln = strlen(szRst);
      return;
    }
/*
 *  Initialize to zero
 */
    memset(hci, '\0', nSamps*sizeof(float));
    memset(rvi, '\0', nSamps*sizeof(float));
/* 
 *  Get the part of the product to output:
 */
    prod_part = REAL1;
    if (*nProd_part_in < 0 || *nProd_part_in >= 3) {
      /* if (temp < 0 || temp >= 3) stErrFatal ("Unknown product part!"); */
      nMsg_no = 210;
      *nJerror = nMsg_no;
      strcpy(szRst,"apin_ms: Unknow product part!");
      *nRstln = strlen(szRst);
      return;
    }

    switch (*nProd_part_in) {
      case 0:
        prod_part = REAL1;
        break;
      case 1:
        prod_part = IMAG;
        break;
      case 2:
        prod_part = OVERLAY;
        break;
    }
/*
 *  Set the output trace location and trace type:
 */
    switch (prod_part) {
    case REAL1:
        outloc  = 0;
        outptr  = hci;
        outcode = HVO_Hc;
        break;
    case IMAG:
        outloc  = 0;
        outptr  = rvi;
        outcode = HVO_Rv;
        break;
    case OVERLAY:
        outloc  = 1;
        outptr  = hci;
        outcode = HVO_Hc;
        break;
    }
/*
 *  Determine whether to overlay the zero-offset trace.
 */
    overlay = *nOverlay_in;
    if (overlay) outloc = 1;
/*
 *  Get the type of product to generate:
 */
    if (*nProd_type_in < 0 || *nProd_type_in > 7) {
      /* if (temp < 0 || temp > 7) stErrFatal ("Unknown product type!"); */
      nMsg_no = 220;
      *nJerror = nMsg_no;
      strcpy(szRst,"apin_ms: Unknown product part!");
      *nRstln = strlen(szRst);
      return;
    }

    prod_type = AB;
    switch (*nProd_type_in) {
      case 0:
        prod_type = AB;
        break;
      case 1:
        prod_type = A_DB;
        break;
      case 2:
        prod_type = DA_B;
        break;
      case 3:
        prod_type = D_AB;
        break;
      case 4:
        prod_type = MAG_A;
        break;
      case 5:
        prod_type = MAG_B;
        break;
      case 6:
        prod_type = MAG1_A;
        break;
      case 7:
        prod_type = MAG1_B;
        break;
    }
/*
 *  Set the sources for the complex product.
 */
    Ar = AVO_Ar;                 /* Defaults for all but magnitudes */
    Ai = AVO_Ai;
    Br = AVO_Br;
    Bi = AVO_Bi;
    if (prod_type == MAG_A || prod_type == MAG1_A) {Br = Ar; Bi = Ai;}
    if (prod_type == MAG_B || prod_type == MAG1_B) {Ar = Br; Ai = Bi;}
/*
 *  Get the type of normalization.
 */
    if (*nNorm_type_in < 0 || *nNorm_type_in > 4) {
      /* stErrFatal ("Unknown normalization type!"); */
      nMsg_no = 240;
      *nJerror = nMsg_no;
      strcpy(szRst,"apin_ms: Unknown normalization type!");
      *nRstln = strlen(szRst);
      return;
    }

    switch (*nNorm_type_in) {
      case 0:
        norm_type = NONE;
        break;
      case 1:
        norm_type = NormA;
        break;
      case 2:
        norm_type = NormB;
        break;
      case 3:
        norm_type = NormAB;
        break;
      case 4:
        norm_type = NormRadius;
        break;
    }

    norm_exp = ZERO;
    smooth   = fSrate; 

    if (norm_type != NONE) {
      if (*nNorm_exp_in < 0 || *nNorm_exp_in > 3) {
        /* stErrFatal ("Unknown normalization exponent!"); */
        nMsg_no = 240;
        *nJerror = nMsg_no;
        strcpy(szRst,"apin_ms: Unknown normalization type!");
        *nRstln = strlen(szRst);
        return;
      }

      switch (*nNorm_exp_in) {
        case 0:
          norm_exp = ZERO;
          break;
        case 1:
          norm_exp = SQRT;
          break;
        case 2:
          norm_exp = UNITY;
          break;
        case 3:
          norm_exp = SQUARE;
          break;
      }
/*
 *  Get the length of the smoothing filter.
 */
      if (*nSmooth_in <= 0 ) {
        /* stErrFatal("Illegal smoothing length!"); */
        nMsg_no = 250;
        *nJerror = nMsg_no;
        strcpy(szRst,"apin_ms: Illegal smoothing length!");
        *nRstln = strlen(szRst);
        return;
      }
      smooth = *nSmooth_in; 
    }
    smooth = smooth / fSrate; 
/*
 *  Load the standard header structure.
 */
    stdHdr.icdp = 1;
    stdHdr.itrc_type = 2;
    stdHdr.iseqno = 3;

    nens_input = 0;  /* # ensembles input */
    nens_valid = 0;  /* # valid ensembles */
  }
/*
 *  --------------------------------------------------------------
 *
 *  Loop while ensembles still exist:
 */
  if (nEndfg != 1) {
/*
 *  Set the sources for the complex product.
 */
    Ar = AVO_Ar;                 /* Defaults for all but magnitudes */
    Ai = AVO_Ai;
    Br = AVO_Br;
    Bi = AVO_Bi;
    if (prod_type == MAG_A || prod_type == MAG1_A) {Br = Ar; Bi = Ai;}
    if (prod_type == MAG_B || prod_type == MAG1_B) {Ar = Br; Ai = Bi;}

    nens_input++;
/*
 *  If we have an insufficient number of traces in the ensemble,
 *  zero all the traces up to the maximum number we can have.
 *  In this case, apin_avoCheckSuite will already have killed the
 *  trace headers, and resequenced the traces.  Also throw away
 *  ensemble with a nonpositive CDP number.
 */
    invalid = apin_avoCheckSuite(Hdrs, nHdrs, &stdHdr, nTrcs);
    nHptr = stdHdr.icdp - 1;
    if (Hdrs[nHptr] <= 0) {
      printf("cdp header value is equal to %f\n",Hdrs[nHptr]);
    }
    else if (invalid < 0) {
      memset (trcs, (int) 0, nTrcs*nSamps*sizeof(int));
    }
/*
 *  If we have a sufficient number of traces, but some of them have
 *  nonconforming trace types, zero the nonconforming ones.  Also
 *  zero the output trace
 */
    else if (invalid > 0) {
      for (i=0; i<nTrcs; i++) {
        nHptr = i*nHdrs+stdHdr.itrc_type - 1;
        if (Hdrs[nHptr] ==HVO_K) {
          nTptr=nSamps*i;
          memset (&trcs[nTptr], (int) 0, nSamps*sizeof(int));
        }
      }
      memset (outptr, (int) 0, nSamps*sizeof(int));
    }
/*
 *  Otherwise, set Ai <-- Re{AB*};  Bi <-- Im{AB*}.
 */
    else {
      nArp=nSamps*Ar;
      nAip=nSamps*Ai;
      nBrp=nSamps*Br;
      nBip=nSamps*Bi;
      apin_complexProduct(&trcs[nArp], &trcs[nAip], &trcs[nBrp], &trcs[nBip],
                          hci, rvi, nSamps);
/*
 *  Set the normalization factors.
 */
      switch (norm_type) {
      case NormA:
        nArp=nSamps*AVO_Ar;
        nAip=nSamps*AVO_Ai;
        apin_Magnitude(norm, &trcs[nArp], &trcs[nAip], nSamps);
        break;
      case NormB:
        nBrp=nSamps*AVO_Br;
        nBip=nSamps*AVO_Bi;
        apin_Magnitude(norm, &trcs[nBrp], &trcs[nBip], nSamps);
        break;
      case NormAB:
        apin_complexProduct(hci, rvi, hci, rvi, norm, norm1, nSamps);
        break;
      case NormRadius:
        nArp=nSamps*AVO_Ar;
        nAip=nSamps*AVO_Ai;
        nBrp=nSamps*AVO_Br;
        nBip=nSamps*AVO_Bi;
        apin_Magnitude(norm, &trcs[nArp], &trcs[nAip], nSamps);
        apin_Magnitude(norm1, &trcs[nBrp], &trcs[nBip], nSamps);
        apin_Magnitude(norm, norm, norm1, nSamps);
        break;
      case NONE:
        break;
      }
/*
 *  Multiply by two if D(AB*).
 */
      if (prod_type == D_AB) {
        apin_avovsmul(hci, hci, 2.0, nSamps);
        apin_avovsmul(rvi, rvi, 2.0, nSamps);
      }
/*
 *  Adjust for Vertical deviations if A-DB* or D(AB*).
 */
      if (prod_type == D_AB || prod_type == A_DB) {
        nRrp=nSamps*AVO_Rr;
        nRip=nSamps*AVO_Ri;
        nSbp=nSamps*AVO_Sb;
        nSap=nSamps*AVO_Sa;
        nArp=nSamps*AVO_Ar;
        nAip=nSamps*AVO_Ai;
        apin_avoDelta(hci, &trcs[nRrp], &trcs[nSbp],
          &trcs[nSap], &trcs[nArp], &trcs[nAip], nSamps);
        apin_avoDelta(rvi, &trcs[nRip], &trcs[nSbp],
          &trcs[nSap], &trcs[nArp], &trcs[nAip], nSamps);
      }
/*
 *  Adjust for Horizontal deviations, if DA-B* or D(AB*).
 */
      if (prod_type == D_AB || prod_type == DA_B) {
        nRrp=nSamps*AVO_Rr;
        nRip=nSamps*AVO_Ri;
        nSbp=nSamps*AVO_Sb;
        nSap=nSamps*AVO_Sa;
        nBrp=nSamps*AVO_Br;
        nBip=nSamps*AVO_Bi;
        apin_avoDelta(hci, &trcs[nRrp], &trcs[nSap],
        &trcs[nSbp], &trcs[nBrp], &trcs[nBip], nSamps);
        apin_avoDelta(rvi, &trcs[nRip], &trcs[nSap],
        &trcs[nSbp], &trcs[nBrp], &trcs[nBip], nSamps);
      }
/*
 *  Perform the smoothing and normalization, if so requested.
 */
      if (norm_type != NONE) {
        apin_MovingAvg(norm, norm1, smooth, nSamps);
        apin_avoPnorm(hci, rvi, norm1, nSamps, norm_exp);
      }
/*
 *  Perform the square root, if so requested.
 */
      if (prod_type == MAG1_A || prod_type == MAG1_B)
        apin_SquareRoot(hci, nSamps);

      nens_valid++;
    }
/*
 *  Copy the specified trace.
 */
    nTptr=nSamps*outloc;
    memcpy(&trcs[nTptr], outptr, nSamps * sizeof(float));
/*
 *  Set the trace headers.
 */
    nHptr = (outloc*nHdrs)+(stdHdr.itrc_type - 1);
    Hdrs[nHptr] = invalid ? HVO_K : outcode;
  }
}

/* ******************************************************************* */

/*
 *  This routine:
 *
 *      (1)     Checks the first eight trace headers to ensure
 *              that they constitute a valid standard suite;
 *      (2)     Handles various exceptional conditions.
 *
 *  Return code:  -1:  Not enough traces in ensemble.
 *                 0:  Normal return.
 *                >0:  Index of first offending trace.
 *
 *  Author:  Herb Swan
 */


static short header_type[AVO_NStd] =
    {HVO_Ar, HVO_Br, HVO_Ai, HVO_Bi, HVO_Sa, HVO_Sb, HVO_Rr, HVO_Ri};

int apin_avoCheckSuite(float *Hdrs, 
                       int nHdrs,
                       StdHdr *stdHdr, 
                       int nTrcs) {
    int         i, code, nHptr;
    short       type;
/*
 *  Make sure it's large enough.
 */
  if (nTrcs < AVO_NStd) {
    for (i=0; i<nTrcs; i++) {
      nHptr = i*nHdrs+stdHdr->itrc_type - 1;
      Hdrs[nHptr] = HVO_K;
    }
    return(-1);
  }
/*
 *  Loop through each trace header in the suite.
 *  Look for a trace which doesn't conform.
 */
  code = 0;
  for (i=0; i<AVO_NStd; i++) {
    nHptr = i*nHdrs+stdHdr->itrc_type - 1;
    type = Hdrs[nHptr];
    if (type != header_type[i]) {
      if (!code) code = i + 1;
      Hdrs[nHptr] = HVO_K;
    }
  }
/*
 *  If invalid, make sure at least the CDP numbers match!
 */
  if (code) {
    for (i=1; i<nTrcs; i++) {
      nHptr = i*nHdrs+stdHdr->icdp - 1;
      Hdrs[nHptr] = Hdrs[stdHdr->icdp - 1];
    }
  }
  return(code);
}

/* ******************************************************************* */
/*
 *  This routine computes either the vertical or horizontal
 *  deviations from the trend.
 */

void apin_avoDelta(float *prod, float *r, float *s1, float *s2,
  float *tr, float *ti, int ns) {

    double magsq, ratio;
    int i;

    for (i=0; i<ns; i++) {
        magsq = tr[i]*tr[i] + ti[i]*ti[i];
        ratio = 0;
        if (s2[i] > 0) ratio = s1[i] / s2[i];
        prod[i] -= r[i] * ratio * magsq;
    }
  }

/* ******************************************************************* */

/*
 *  This routine performs instantaneous normalization for
 *  the AVO Seismic Workbench tool, 'a_avoProducts".
 */

void apin_avoPnorm(float *hci, float *rvi, float *norm, int ns,
                   Norm_exp norm_exp) {

    double value;
    int i;

    if (norm_exp == ZERO) return;

    for (i=0; i<ns; i++) {
      value = norm[i];
      if (value <= 0) continue;

        switch (norm_exp) {
        case SQRT:
            value = sqrt(value);
        case UNITY:
            value = sqrt(value);
            break;
        case ZERO:
            break;
        case SQUARE:
            break;
        }

        hci[i] /= value;
        rvi[i] /= value;
      }
    }

/* ******************************************************************* */
/*
 *  This routine simply multiplies a vector by a scalar.
 *  (It was easier to write it myself than figure out how
 *  to use a canned one.)
 */

void apin_avovsmul(float *in, float *out, float scalar, int ns) {

    int i;

    for (i=0; i<ns; i++) out[i] = in[i] * scalar;
    }
/* ******************************************************************* */
/*
 *  This routine computes the real & imag parts of the
 *  complex product of two vectors.
 *
 */

void apin_complexProduct(float *ar, float *ai, float *br, float *bi,
                         float *outr, float *outi, int ns) {

  double pr, pi;
  int i;

  for (i=0; i<ns; i++) {
    pr =  ar[i]*br[i] + ai[i]*bi[i];
    pi = -ar[i]*bi[i] + ai[i]*br[i];
    outr[i] = pr;
    outi[i] = pi;
  }

}

void apin_end(int *nJerror
             ) {
  int nMsg_no;
 
  /* The first call to this routine is before the last cmp */
  /* So wait for the 2nd call before executing routine */
  nEcalls=nEcalls+1;
  if (nEcalls == 2) {
    if (nens_valid < nens_input) {
      /*"WARNING (avoClass):  Only %d out of %d ensembles had valid suites",
        nens_valid, nens_input); */
      nMsg_no = 270;
      *nJerror = nMsg_no;
      printf("Warning: Only %d out of %d gathers had valid suites!",
             nens_valid,nens_input);
    }
    nEndfg=1;
  }
  *nJerror=0;
}

/* ******************************************************************* */
/*
 *  Routine to calculate the magnitude of an array of
 *  complex vectors for the AVO Seismic Workbench tool
 *  "a_avoProducts".
 *
 */

void apin_Magnitude(float *norm, float *a, float *b, int ns) {

    double va, vb;
    int i;

    for (i=0; i<ns; i++) {
      va = a[i];
      vb = b[i];
      norm[i] = va*va + vb*vb;
    }
  }

/* ******************************************************************* */
/*
 *  Compute the rms amplitude of a trace segment.
 *
 *    Author                 H. W. Swan
 *    Designer               H. W. Swan
 *    Language               Ansii C
 *    Written                Nov 18, 1994
 *
 *    Argument list:
 *
 *   IN/OUT   ARGUMENT   TYPE   DESCRIPTION
 *   ------   --------   ----   -----------
 *
 *     IN       ain   float *   The array to average
 *    OUT      aout   float *   The averaged array
 *     IN        ns      int    The length of the arrays
 *     IN     ntwin      int    # samples in moving average
 *
 *   Purpose:
 *
 *     This subroutine computes the summed values of the
 *      input data which lie within a sliding moving average
 *      window.  Hard input zeros are ignored.
 *
 */

void apin_MovingAvg(float *ain, float *aout, int ntwin, int ns) {

/*
 *  Declaration of local variables:
 */
    int         limit, l, l2, nterms;
    int         diff, i;
    double      sum;
/*
 *  Round "ntwin" up to next odd number.
 */
    l   = ntwin;
    l2  = l/2;
    sum = 0.0;
/*
 *  Phase 1:  No smoothing if l=1:
 */
    if (l == 1) {
      for (i=0; i<ns; i++) aout[i] = ain[i];
      return;
    }
/*
 *  Phase 2:  Set output to zero if l <= 0:
 */
    if (l <= 0) {
      for (i=0; i<ns; i++) aout[i] = sum;
      return;
    }
/*
 *  Phase 3:  Don't churn a lot of time if l is huge.
 */
    if (l2 > ns) {
      nterms = 0;
      for (i=0; i<ns; i++) {
        sum += ain[i];
        if (ain[i]) nterms++;
      }
      sum /= nterms;
      for (i=0; i<ns; i++) aout[i] = sum;
        return;
    }
    
/*
 *  Phase 4:  Perform the running sum.
 */
    nterms = 0;
    limit  = ns + l2;
    for  (i=0; i<limit; i++) {
      if (i < ns) {
        sum += ain[i];
        if (ain[i]) nterms++;
      }
      diff = i - l;
      if (diff >= 0) {
        sum -= ain[diff];
        if (ain[diff]) nterms--;
      }
      diff = i - l2;
      if (diff >= 0 && nterms)
        aout[diff] = sum / nterms;
    }
  }

/* ******************************************************************* */
void apin_SquareRoot(float *trace, int ns) {
    double data;
    int i;

    for (i=0; i<ns; i++) {
      data = trace[i];
      if (data <= 0) {
        trace[i] = 0;
        continue;
      }
      data = sqrt(data);
      trace[i] = data;
    }
  }
/* ******************************************************************* */

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
