/****
!<CPS_v1 type="HEADER_FILE"/>
****/

/****
!!!
!!!           Header File
!!!
****/

/*---------------------------- apin.h ---------------------------------*/
/*---------------------------- apin.h ---------------------------------*/
/*---------------------------- apin.h ---------------------------------*/

       /* other files are:  apin_crou.c  apin.f90  */

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
!                      C P S   H E A D E R   F I L E
!
! Name       : avocorr
! Category   : amplitude_mod
! Written    : 2004-02-17   by: Michael Ried
! Revised    : 2005-07-21   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Header file for APIN Product Indicator
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  2. 2005-07-21  Stoeckley      Fix to compile with C++.
!  1. 2005-01-03  Michael Ried  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

/*
 *  Define the trace positions of a AVO standard suite:
 */

#define AVO_Ar  0       /* The real "A" trace   */
#define AVO_Br  1       /* The real "B" trace   */
#define AVO_Ai  2       /* The imag "A" trace   */
#define AVO_Bi  3       /* The imag "B" trace   */
#define AVO_Sa  4       /* The std dev of "A"   */
#define AVO_Sb  5       /* The std dev of "B"   */
#define AVO_Rr  6       /* The real correlation */
#define AVO_Ri  7       /* The imag correlation */

#define AVO_NStd  8     /* Standard suite size  */

/*
 *  Define AVO Header values:
 */

#define HVO_Live 1      /* Live seismic data    */
#define HVO_K    2      /* A killed trace       */
#define HVO_Ar  43      /* The real "A" trace   */
#define HVO_Ai  17      /* The imag "A" trace   */
#define HVO_Br  44      /* The real "B" trace   */
#define HVO_Bi  18      /* The imag "B" trace   */
#define HVO_Sa  45      /* The std dev of "A"   */
#define HVO_Sb  47      /* The std dev of "B"   */
#define HVO_Rr  48      /* The real correlation */
#define HVO_Ri  41      /* The imag correlation */
#define HVO_Hc  42      /* The real product     */
#define HVO_Rv  46      /* The imag product     */
#define HVO_Pc  20      /* The joint percentile class  */
                        /* Also the marginal percentile class for A */
#define HVO_PcB 21      /* The marginal percentile class for B */
#define HVO_Vs  51      /* The stacking velocity trace         */
#define HVO_Th  52      /* The real principal component rotation angle */
#define HVO_Ph  53      /* The imag principal component rotation angle */

typedef enum {OFFSET, ANGLE} Offset_type;

struct AuxTraces {
        float   *Sq;            /* Hilbert trans of prestack trace */
        float   *Bstr;          /* Stretched B trace               */
        float   *Bqstr;         /* Stretched quadrature trace      */
        float   *Vrms;          /* Velocity trace                  */
        float   *T0;            /* T0(t) for OMN prestack data     */
        Offset_type  type;      /* OFFSET or ANGLE                 */
        int     nmo;            /* =1 if NMO was applied           */
        int     ns;             /* Trace length                    */
        float   si;             /* Sample length (ms)              */
        };

typedef struct AuxTraces Aux_traces;



/*
 *  Data structures:
 */

typedef struct stdHdrStruct {
        int icdp, itrc_type, iseqno;
} StdHdr;

typedef enum ProdPart {REAL1, IMAG, OVERLAY} Prod_part;
typedef enum ProdType {AB, A_DB, DA_B, D_AB,
               MAG_A, MAG_B, MAG1_A, MAG1_B} Prod_type;
typedef enum NormType {NONE, NormA, NormB, NormAB, NormRadius} Norm_type;
typedef enum NormExp {ZERO, SQRT, UNITY, SQUARE} Norm_exp;

#define KILLED 2

/*
 *  Function protocols:
 */

#ifdef __cplusplus
extern "C" {
#endif

int apin_avoCheckSuite(float *Hdrs, int nHdrs, StdHdr *stdHdr, int nTrcs);

void apin_complexProduct(float *ar, float *ai, float *br, float *bi,
        float *outr, float *outi, int ns);

void apin_avoDelta(float *prod, float *r, float *s1, float *s2,
        float *tr, float *ti, int ns);

void apin_avovsmul(float *in, float *out, float scalar, int ns);

void apin_avoPnorm(float *hci, float *rvi, float *norm, int ns,
        Norm_exp norm_exp);

void apin_Magnitude(float *norm, float *a, float *b, int ns);

void apin_MovingAvg(float *in, float *out, int ntwin, int ns);

void apin_SquareRoot(float *trace, int ns);

#ifdef __cplusplus
}
#endif

