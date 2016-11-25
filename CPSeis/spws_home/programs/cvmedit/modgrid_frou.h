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

/*------------------------------ modgrid_crou.h ---------------------------*/
/*------------------------------ modgrid_crou.h ---------------------------*/
/*------------------------------ modgrid_crou.h ---------------------------*/

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                      C P S   H E A D E R   F I L E
!
! Name       : MODGRID_FROU
! Category   : velocity
! Written    : 2004-03-16   by: R.S. Day
! Revised    : 2005-11-22   by: R.S. Day
! Maturity   : beta
! Purpose    : Manipulate velocity models. Provide C access to modgrid.
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
!  4. 2005-11-22  R.S.Day      Changed prototype of modgrid_regrid_c
!  3. 2005-07-28  R.S.Day      Changed prototype of modgrid_file_info
!  2. 2005-05-31  R.S.Day      Changed prototype of modgrid_data_stats_c
!  1. 2005-01-31  R.S.Day      Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


#ifndef MODGRID_H_
#define MODGRID_H_

#include "c2f_interface.h"


#if NEED_UNDERSCORE
#define modgrid_ftype_c  modgrid_ftype_c_
#define modgrid_dfile_c  modgrid_dfile_c_
#define modgrid_file_info  modgrid_file_info_
#define modgrid_rddesc_c  modgrid_rddesc_c_
#define modgrid_paint_by_file_c  modgrid_paint_by_file_c_
#define modgrid_paint_by_obj_c  modgrid_paint_by_obj_c_
#define modgrid_print_c  modgrid_print_c_
#define modgrid_delete_c  modgrid_delete_c_
#define modgrid_data_smooth_c  modgrid_data_smooth_c_
#define modgrid_modspec_to_gocad_vs modgrid_modspec_to_gocad_vs_
#define modgrid_data_stats_c modgrid_data_stats_c_
#define modgrid_data_zbnd_c modgrid_data_zbnd_c_
#define modgrid_regrid_c modgrid_regrid_c_
#elif NEED_CAPITALS
#define modgrid_ftype_c  MODGRID_FTYPE_C
#define modgrid_dfile_c  MODGRID_DFILE_C
#define modgrid_file_info  MODGRID_FILE_INFO
#define modgrid_rddesc_c  MODGRID_RDDESC_C
#define modgrid_paint_by_file_c  MODGRID_PAINT_BY_FILE_C
#define modgrid_paint_by_obj_c  MODGRID_PAINT_BY_OBJ_C
#define modgrid_print_c  MODGRID_PRINT_C
#define modgrid_delete_c  MODGRID_DELETE_C
#define modgrid_data_smooth_c  MODGRID_DATA_SMOOTH_C
#define modgrid_modspec_to_gocad_vs MODGRID_MODSPEC_TO_GOCAD_VS
#define modgrid_data_stats_c MODGRID_DATA_STATS_C
#define modgrid_data_zbnd_c MODGRID_DATA_ZBND_C
#define modgrid_regrid_c MODGRID_REGRID_C
#endif

#ifdef __cplusplus
extern "C" {
#endif


void  modgrid_ftype_c(char *fname,INTEGER *stdo, char *ftype,double *dfsize);
void  modgrid_dfile_c(char *ifile,INTEGER *stdo,char *iftype, char *idfile);

void modgrid_file_info(char *fname,char *ftype, char *iascii,
 INTEGER *rank,INTEGER *ng,REAL *og,REAL *dg,INTEGER *hd,
 char *label1,char *label2,char *label3,
 char *unit1,char *unit2,char *unit3,
 INTEGER *xhdr, INTEGER *yhdr, char *xyz);

INTEGER modgrid_paint_by_file_c(char *fname,INTEGER *maxmem, INTEGER *stdo,
 char *ovors, char *ivors,
 INTEGER *hx_out, INTEGER *hy_out,
 INTEGER *n1, REAL *o1, REAL *d1,
 INTEGER *n2, REAL *o2, REAL *d2,
 INTEGER *n3, REAL *o3, REAL *d3,
 REAL *ocube,  char *out_xyz, char *vtyp_out );

INTEGER modgrid_paint_by_obj_c(F90Pointer *obj, INTEGER *maxmem, INTEGER *stdo,
 char *ovors, char *ivors,
 INTEGER *hx_out, INTEGER *hy_out,
 INTEGER *n1, REAL *o1, REAL *d1,
 INTEGER *n2, REAL *o2, REAL *d2,
 INTEGER *n3, REAL *o3, REAL *d3,
 REAL *ocube,  char *out_xyz, char *vtyp_out );

INTEGER modgrid_data_stats_c(char *fname,REAL *dmin,REAL *dmax,REAL *gmax,
INTEGER *xhdr, INTEGER *yhdr);

INTEGER modgrid_data_zbnd_c(char *fname,REAL *dbnd,INTEGER *zbnd,REAL *zval);

INTEGER modgrid_regrid_c(char *fname, char *ofile, char *otype,
       char *oorder, INTEGER *ngo, REAL *ogo, REAL *dgo,
       INTEGER *xhdr,INTEGER *yhdr,INTEGER *o_endian,
       REAL *scale,REAL *clip_min,REAL *clip_max,INTEGER *reqmem);

INTEGER modgrid_rddesc_c(F90Pointer *obj,char *i_fname,INTEGER *stdo,
       char *i_dfile,char *i_wtype, char *i_ftype,
       INTEGER *xhdr,INTEGER *yhdr);

void modgrid_print_c(F90Pointer *obj, INTEGER *stdo);
void modgrid_delete_c(F90Pointer *obj);

INTEGER modgrid_modspec_to_gocad_vs(char *ifile, char *ofile);

void  modgrid_data_smooth_c(char *ifname,REAL *lambda, REAL *vbar,
      INTEGER *i_err);
#ifdef __cplusplus
}
#endif


#endif
