/****
!<CPS_v1 type="HEADER_FILE"/>
****/

/****
!!!
!!!           Header File 
!!!
****/

/*---------------------------- avocorr.h ---------------------------------*/
/*---------------------------- avocorr.h ---------------------------------*/
/*---------------------------- avocorr.h ---------------------------------*/

       /* other files are:  avocorr_crou.c  avocorr.f90  */

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
! Category   : velocity
! Written    : 2004-01-29   by: Michael Ried
! Revised    : 2005-01-03   by: Michael Ried
! Maturity   : production
! Purpose    : Header file for AVO correlation matching
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
!  1. 2005-01-03  Michael Ried  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _avocorr_H_
#define _avocorr_H_


#ifdef __cplusplus
extern "C" {
#endif


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

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

#define AVO_NStd 8      /* Standard suite size  */

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
#define HVO_Pc  20      /* The joint percentile class            */
                    /* Also the marginal percentile class for A    */
#define HVO_PcB 21  /* The marginal percentile class for B         */
#define HVO_Vs  51  /* The stacking velocity trace                 */
#define HVO_Th  52  /* The real principal component rotation angle */
#define HVO_Ph  53  /* The imag principal component rotation angle */

typedef enum {OFFSET, ANGLE} Offset_type;

struct AuxTraces {
        float   *Sq;            /* Hilbert trans of prestack trace */
        float   *Bstr;          /* Stretched B trace               */
        float   *Bqstr;         /* Stretched quadrature trace      */
        float   *Vrms;          /* Velocity trace                  */
        float   *T0;            /* T0(t) for OMN prestack data     */
        Offset_type type;       /* OFFSET or ANGLE                 */
        int     nmo;            /* =1 if NMO was applied           */
        int     ns;             /* Trace length                    */
        float   si;             /* Sample length (ms)              */
       };

typedef struct AuxTraces Aux_traces;

typedef enum MatchType {DeltaA, DeltaB, CorrA, CorrB} Match_type;

struct MatchInfo {
        float           real_coef;      /* Real part of desired Corr Coef */
        float           imag_coef;      /* Imag part of desired Corr Coef */
        Match_type      match_type;     /* Type of matching to perform    */
        int             preserve;       /* True if Preserve Original Vars */
       };

typedef struct MatchInfo Match_info;

/*
 *  Function protocols:
 */

    void avocorr_reapplymute(float *trace, float tlive_s, float tlive_e, 
       float si,  int   ns);
  
    void avocorr_corrmatch(float **ens, Match_info *info, int ns);
  
    void avocorr_compt0(Aux_traces *auxTraces, float offset);
  
    void avocorr_auxtraces(float **ens, Aux_traces *aux, float *h,
       int nh, int is1, int is2, int noh2, int ftrlen);
  
    void avocorr_matchprestack(float **ens, Match_info *info, 
       Aux_traces *aux, float offset, int trace);
  
    void avocorr_hilba(float *a, float *h, int *nt, int *is1, int *is2,
       int *ftrlen);
  
    void avocorr_scantrace(float *a, int ns, int *is1, int *is2);

    int avocorr_prestackscan(double **headers, int itrc_type, int nens);

    void avocorr_rikflt(float *h, float *corr, float *work, double *aux,
       int *nh, int *mmxh, float *w0t, float *pctwns,
       float *pctcns, double *pole, int *ier);


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
