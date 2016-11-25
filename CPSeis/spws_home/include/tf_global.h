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
#ifndef TF_Glob_
#define TF_Glob_

#include "c2f_interface.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "tfio.h"

#ifdef NEED_CAPITALS
#define tf_global_from_grid_ TF_GLOBAL_FROM_GRID
#define tf_global_prhd_      TF_GLOBAL_PRHD
#define tf_global_fd_        TF_GLOBAL_FD
#define tf_global_put_block_ TF_GLOBAL_PUT_BLOCK
#define tf_global_put_brick_ TF_GLOBAL_PUT_BRICK
#define tf_global_put_hdwrds_ TF_GLOBAL_PUT_HDWRDS
#define tf_global_grecsizf_   TF_GLOBAL_GRECSIZF
#define tf_global_put_grecsizf_   TF_GLOBAL_PUT_GRECSIZF
#endif

#if(VMS || _AIX || __hpux)
#define tf_global_from_grid_ tf_global_from_grid
#define tf_global_prhd_      tf_global_prhd
#define tf_global_fd_        tf_global_fd
#define tf_global_put_block_ tf_global_put_block
#define tf_global_put_brick_ tf_global_put_brick
#define tf_global_put_hdwrds_ tf_global_put_hdwrds
#define tf_global_grecsizf_   tf_global_grecsizf
#define tf_global_put_grecsizf_   tf_global_put_grecsizf
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

int  tf_global_get_iotype (TF_Global *g);
void tf_global_put_ntrfil ( int ntrfil, TF_Global *g );
int  tf_global_get_ntrfil ( TF_Global *g );
void tf_global_put_nbycll ( int nbycll, TF_Global *g );
int  tf_global_get_nbycll ( TF_Global *g );
void tf_global_put_ntrcll ( int ntrcll, TF_Global *g );
void tf_global_put_grecsiz ( int grecsiz, TF_Global *g );
int  tf_global_get_grecsiz ( TF_Global *g );
int  tf_global_grecsizf_( TF_Global *g );
void tf_global_put_grecsizf_( int *grecsiz, TF_Global *g );
void tf_global_put_ntb ( int ntb, TF_Global *g );
int  tf_global_get_ntb ( TF_Global *g );
void tf_global_put_numhc ( int numhc, TF_Global *g );
int  tf_global_get_numhc ( TF_Global *g );
void tf_global_put_nbydp ( int nbydp, TF_Global *g );
void tf_global_put_nbyhd ( int nbyhd, TF_Global *g );
int  tf_global_get_nbyhd ( TF_Global *g );
void tf_global_put_hdtyp ( int hdtyp, TF_Global *g );
int  tf_global_get_hdtyp ( TF_Global *g );
void tf_global_put_nhdwd ( int nhdwd, TF_Global *g );
void tf_global_put_ndptr ( int ndptr, TF_Global *g );
int  tf_global_get_ndptr ( TF_Global *g );
void tf_global_put_srval ( float srval, TF_Global *g );
void  tf_global_put_tstrt ( float tstrt, TF_Global *g );
int   tf_global_get_dunits(TF_Global *g);
void  tf_global_put_dunits(int dunits, TF_Global *g);

void  tf_global_prhd_(TF_Global *g);
void  tf_global_tf_prhd(Grid3DDesc *h);
void  tf_global_rmod_prhd(Grid3DDesc *h);
void  tf_global_govo_prhd(Grid3DDesc *h);
int   tf_global_transform(char *ax, char *ay, char *az,
      void *T, TF_Global *g);



int   tf_global_from_grid_(TF_Global *g, char *hfile, char *dfile,
      char *pname, char *obj_name,
      float *o1, float *o2, float *o3,
      int   *n1, int   *n2, int   *n3,
      float *d1, float *d2, float *d3, 
      char *lab1,char *lab2,char *lab3,
      int *X, int *Y, int *Z,
      int *ftype, int  *wdtype, int *dunits);
void  tf_global_put_brick_(TF_Global *g, int *brick);
int   tf_global_brick(TF_Global *g, int *brick);
void  tf_global_put_block_(TF_Global *g, int *block);
int   tf_global_block(TF_Global *g, int *block);
void  tf_global_cps_rotparm(TF_Global *g, int dir);
char *tf_global_to_char(TF_Global *g,int *nby, char *temp);
char *tf_global_bld_segy3600(TF_Global *g,int *nby, char *temp);
char *tf_global_bld_segy(TF_Global *g,int *nby);
char *tf_global_bld_tf(TF_Global *g,int *nby);
int   tf_global_is3d(TF_Global *g);
int   tf_global_byinht(TF_Global *g);
int   tf_global_byintr(TF_Global *g);
int   tf_global_byinhd(TF_Global *g);
int   tf_global_first_hcbyt(TF_Global *g);
int   tf_global_fd_(TF_Global *g);
char *tf_global_header_file(TF_Global *g);
int   tf_global_hdwrd(TF_Global *g, int axis);
void  tf_global_hdwrds(TF_Global *g, int *h1, int *h2, int *h3);
void  tf_global_put_hdwrds_(TF_Global *g, int *hd);
int   tf_global_grid_size(TF_Global *g, int axis);
int   tf_global_grid_delta(TF_Global *g, float *d1, float *d2, float *d3);
int   tf_global_grid_orgs(TF_Global *g, float *o1, float *o2, float *o3);
char *tf_global_axis(TF_Global *g, int axis);
char *tf_global_cword_type(int wdtype);
int   tf_global_iword_type(char *word_string);

char *tf_global_ftype(TF_Global *g);
int  tf_global_get_nhdwd ( TF_Global *g );
char *tf_global_data_file(TF_Global *g);
int  tf_global_get_wdtyp ( TF_Global *g );
int   tf_global_grid_sizes(TF_Global *g, int *n1, int *n2, int *n3);
float tf_global_get_srval ( TF_Global *g );
float tf_global_get_tstrt ( TF_Global *g );
int  tf_global_get_nbydp ( TF_Global *g );
#ifdef __cplusplus
}                   // for C++
#endif

#endif

