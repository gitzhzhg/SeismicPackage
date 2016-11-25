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
#ifndef _MODELIO_
#define _MODELIO_

#include "c2f_interface.h"

#include "pick.h"
#include "model.h"

/* File format flags */
#define INTTYP 3
#define ICPTYP 4
#define CVMTYP 5
#define GWSTYP 6
#define SRATYP 7
#define GO_TYP 8

/* Alias names for fortran compiler differences */
/* Wrapper routines for D. Hansons RMOD calls   */
#ifdef NEED_CAPITALS
#define rmodopen_w_  RMODOPEN_W
#define rmodclos_w_  RMODCLOS_W
#define rmodcloa_    RMODCLOA
#define rmodclos_    RMODCLOS
#define rmodapen_    RMODAPEN
#define rmodwrhd_w_  RMODWRHD_W
#define rmodgwtyp_   RMODGWTYP
#define rmodswtyp_   RMODSWTYP
#define rmodrdhd_w_  RMODRDHD_W
#define rmodhdpr_w_  RMODHDPR_W
#define rmodwrcard_  RMODWRCARD
#define rmodrdcard_  RMODRDCARD
#define rmodfstr_w_  RMODFSTR_W
#define rmodwchd_w_  RMODWCHD_W
#define rmodwval_    RMODWVAL
#define rmodrval_w_  RMODRVAL_W
#define rmodfcrd_w_  RMODFCRD_W
#define gridwrec_    GRIDWREC
#define gridrrec_    GRIDRREC
#define griddaop_    GRIDDAOP
#endif

#if (VMS || _AIX || __hpux)
#define rmodopen_w_  rmodopen_w
#define rmodclos_w_  rmodclos_w
#define rmodcloa_    rmodcloa
#define rmodclos_    rmodclos
#define rmodapen_    rmodapen
#define rmodwrhd_w_  rmodwrhd_w
#define rmodgwtyp_   rmodgwtyp
#define rmodswtyp_   rmodswtyp
#define rmodrdhd_w_  rmodrdhd_w
#define rmodhdpr_w_  rmodhdpr_w
#define rmodwrcard_  rmodwrcard
#define rmodrdcard_  rmodrdcard
#define rmodfstr_w_  rmodfstr_w
#define rmodwchd_w_  rmodwchd_w
#define rmodwval_    rmodwval
#define rmodrval_w_  rmodrval_w
#define rmodfcrd_w_  rmodfcrd_w
#define gridwrec_    gridwrec
#define gridrrec_    gridrrec
#define griddaop_    griddaop
#endif

/* Function prototypes defined in model_io.c*/
#ifdef __cplusplus
extern "C" {                 // for C++
#endif
extern Bool WriteModPik(ErsModel *, char *filename);
extern Bool ReadModPik (ErsModel *, char *filename);
extern Bool GetPCardField(char *buf,char *out,int first,int last);
extern Bool PutPCardField(char *buf,char *in,int first,int last);

extern Bool pcardSetDefaults(Widget, ErsModel *);
extern Bool pcardwr   (ErsModel *, char *, long);
extern Bool pcardrd   (ErsModel *, char *,int *);
extern Bool pcardwrhd (ErsModel *, int *, int *);
extern Bool pcardrdhd (ErsModel *, char *,char *,char *, int *);
extern Bool pcardwrhz (ErsModel *, int *, char *);
extern Bool pcardrdhz (int *, char *, int *, int *, int *,
                       int *, int *,char *a[], char *b[]);
extern Bool pcardwrpik(ErsModel *, int *);
extern Bool pcardrdpik(ErsModel *, int *);
extern Bool pcardwrml(ErsModel *, int *);
extern Bool pcardrdml(ErsModel *, int *);
extern void pcardsettran(ErsTransforms *, int ,float *, int , int, int, int);

extern Bool gwswrpik(ErsModel *, char *);
extern Bool gwsrdpik(ErsModel *, char *);
#ifdef __cplusplus
}                   // for C++
#endif

#endif
