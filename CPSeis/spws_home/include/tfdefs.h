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
#ifndef _TFIO_DEFS
#define _TFIO_DEFS

#include "c2f_interface.h"
#include "wrdcnvrt.h"
#include "dskio.h"
#include "tfio.h"
#include "bswap.h"



#ifdef NEED_CAPITALS
#define tf_glbl_add_     TF_GLBL_ADD
#define tf_glbl_add1_    TF_GLBL_ADD1
#define tf_glbl_get_     TF_GLBL_GET
#define tf_glbl_getn_    TF_GLBL_GETN
#define tf_glbl_add1_    TF_GLBL_ADD1
#define tf_glbl_get1_    TF_GLBL_GET1
#define tf_glbl_del_     TF_GLBL_DEL
#define tf_glbl_putg_    TF_GLBL_PUTG
#define tf_glbl_getg_    TF_GLBL_GETG
#define tf_glbl_to_asc_  TF_GLBL_TO_ASC
#define tf_get_glblpntr_ TF_GET_GLBLPNTR

#define tf_trcio_create_file_        TF_TRCIO_CREATE_FILE
#define tf_trcio_write_trace_        TF_TRCIO_WRITE_TRACE
#define tf_trcio_close_file_         TF_TRCIO_CLOSE_FILE


#define get_global_data_    GET_GLOBAL_DATA
#define get_global_keyword_ GET_GLOBAL_KEYWORD
#define get_global_old_byt_ GET_GLOBAL_OLD_BYT
#define cps_global_addr_    CPS_GLOBAL_ADDR
#define cps_global_grotm_   CPS_GLOBAL_GROTM
#define cps_global_srotm_   CPS_GLOBAL_SROTM

#define tf_open_         TF_OPEN
#define tf_xopenf_       TF_XOPENF
#define tf_close_        TF_CLOSE
#define tf_glbl_rd_      TF_GLBL_RD
#define tf_glbls_rd_     TF_GLBLS_RD
#define tf_glbl_wr_      TF_GLBL_WR
#define tf_glbls_wr_     TF_GLBLS_WR
#define tf_set_trmaxg_   TF_SET_TRMAXG
#define tf_tr_rd_        TF_TR_RD
#define tf_hc_rd_        TF_HC_RD
#define tf_tr_wr_        TF_TR_WR
#define tf_hc_wr_        TF_HC_WR
#define tf_arr_rd_       TF_ARR_RD
#define tf_arr_wr_       TF_ARR_WR
#define byte_to_float_   BYTE_TO_FLOAT
#define bit16_to_float_  BIT16_TO_FLOAT
#define short_to_float_  SHORT_TO_FLOAT
#define float_to_byt_    FLOAT_TO_BYT
#define float_to_bit16_  FLOAT_TO_BIT16
#define read_data_       READ_DATA
#define tf_cpsg_         TF_CPSG
#define tf_check_cntrl_  TF_CHECK_CNTRL
#define tf_print_glbls_  TF_PRINT_GLBLS
#define tf_bmov_         TF_BMOV
#define tf_set_gdefs_    TF_SET_GDEFS
#define tf_set_bsiz_     TF_SET_BSIZ
#define addext_rep_      ADDEXT_REP
#endif


#if(VMS || _AIX || __hpux)
#define tf_glbl_add_     tf_glbl_add
#define tf_glbl_get_     tf_glbl_get
#define tf_glbl_getn_    tf_glbl_getn
#define tf_glbl_add1_    tf_glbl_add1
#define tf_glbl_get1_    tf_glbl_get1
#define tf_glbl_del_     tf_glbl_del
#define tf_glbl_putg_    tf_glbl_putg
#define tf_glbl_getg_    tf_glbl_getg
#define tf_glbl_to_asc_  tf_glbl_to_asc
#define tf_get_glblpntr_ tf_get_glblpntr

#define get_global_data_    get_global_data
#define get_global_keyword_ get_global_keyword
#define get_global_old_byt_ get_global_old_byt
#define cps_global_addr_    cps_global_addr
#define cps_global_grotm_   cps_global_grotm
#define cps_global_srotm_   cps_global_srotm

#define tf_open_         tf_open
#define tf_xopenf_       tf_xopenf
#define tf_close_        tf_close
#define tf_glbl_rd_      tf_glbl_rd
#define tf_glbls_rd_     tf_glbls_rd
#define tf_glbl_wr_      tf_glbl_wr
#define tf_glbls_wr_     tf_glbls_wr
#define tf_set_trmaxg_   tf_set_trmaxg
#define tf_tr_rd_        tf_tr_rd
#define tf_tr_wr_        tf_tr_wr
#define tf_hc_wr_        tf_hc_wr
#define tf_hc_rd_        tf_hc_rd
#define tf_arr_rd_       tf_arr_rd
#define tf_arr_wr_       tf_arr_wr
#define byte_to_float_   byte_to_float
#define bit16_to_float_  bit16_to_float
#define short_to_float_  short_to_float
#define float_to_byt_    float_to_byt
#define float_to_bit16_  float_to_bit16
#define read_data_       read_data
#define tf_cpsg_         tf_cpsg
#define tf_check_cntrl_  tf_check_cntrl
#define tf_print_glbls_  tf_print_glbls
#define tf_bmov_         tf_bmov
#define tf_set_gdefs_    tf_set_gdefs
#define tf_set_bsiz_     tf_set_bsiz
#define addext_rep_      addext_rep
#endif

#ifdef __cplusplus  
extern "C" {                 // for C++ 
#endif

/*          FUNCTION PROTOTYPES          */
/* Function definitions for memory resident 'data base'   */
int  tf_glbl_add_( int  *istat, int  *lun, TF_Global *gtmp );
int  tf_glbl_add1_(char name[],int  *num,char val[],int  *,int  *);
int  tf_glbl_get_( int  *istat, int  *lun, TF_Global *gtmp );
int  tf_glbl_get1_(char name[],int  *num,char val[],int  *,int  *);
int  tf_glbl_getn_( int  *istat, char namen[], TF_Global *gtmp );
int  tf_glbl_del_( int  *istat, int  *lun );
int  tf_glbl_putg_(char name[],int  *num,char val[],int  *j,char mem[]);
int  tf_glbl_getg_(char name[],int  *num,char val[],int  *j,char mem[]);
int  tf_glbl_name_to_fd(char *name);
int  tf_set_trmaxg_(int  *lun, float *trmaxg);
TF_Global *tf_get_glblpntr_(int  *lun );
int  tf_Check_Glbl(TF_Global *G, char *msg);

/* Function definitions for trace file IO */
int  tf_open_(int  *lun, char *namen,TF_Global *glbu,
       int  *op_stat, char *msg);
int  tf_xopen(int * iotype, int  *lun, char *namen,TF_Global *glbu,
       int  *op_stat, char *msg);
int  tf_xopenf_(int *ufi, int *iotype, char *namen, int * op_stat,
     char *ftyp, int *grecsiz, char *wdtyp, int *nhdwd,
     int *n1, WREAL *o1, WREAL *d1, char *units1);
int  tf_close_(int  *lun, int  *cl_stat, char *msg );
int  tf_close_by_name( char *name,  int  *cl_stat, char *msg );
int  tf_glbls_rd_( int  *lun, char str[] , char *msg );
int  tf_glbls_wr_( int  *lun, int  *len , char str[] , char *msg );
int  tf_glbl_rd_( int  *lun, TF_Global *gtmp, char *msg );
int  tf_glbl_wr_( int  *lun, TF_Global *gtmp, char *msg );
int  tf_tr_rd_(int  *lun,int  *ns,int  *num, int  *nsamp, int  *samp1,
       int  *sdec, int  *trnsps, char hd[],char tr[],char *msg);
int  tf_tr_wr_(int  *lun,int  *ns,int  *num,char hd[],
       char tr[],char *msg);
int  tf_hc_wr_(int  *lun,int  *ns,int  *num,char hc[], char *msg);
int  tf_hc_rd_(int  *lun,int  *ns,int  *num,char hc[], char *msg);
int  tf_arr_rd_(int  *lun,IO_request *Cl, char hd[],
       char arr[],char *msg);
int  tf_arr_wr_(int  *lun,IO_request *Cl, char hd[],
       char arr[],char *msg);



/* Function definitions for some utility functions */
int  tf_print_glbls_(char name[], TF_Global *);
int  tf_check_cntrl_(TF_Global *,IO_request *);
GlobalFmt *tf_set_gdefs_();
void tf_bmov_(char *, int  *, char *, int  *, int  *);
int  tf_cpsg_( void *cpsin, void *gout);

/* see addext_rep.c */
int  addext_rep_( char name[], char ext[], int  *istat );

/* see get_global.c */
TF_Global *get_global_info(char *name);
int      get_global_keyword_(char *filename, char *search_for_keyword, 
                      char *returned_keyword_value);
int      get_global_data_(char *, TF_Global *, int  *);
int      get_global_old_byt_(char *filnam, char *msg, int  *, int  *,
          float *, float *, int  *, int  *, float *);
char    *get_global_buff(int newsiz, int *retsiz);
Grid3DDesc *get_global_rmod_parsehd(char *hfile,int ftype, char *in);
Grid3DDesc *get_global_govo_parsehd(char *hfile,int ftype, char *in);
void    *get_global_tfile_parsehd(TF_Global *g, char *tfile,
          int ftype, char *in);
void    *get_global_tf_parsehd(TF_Global *g, char *tfile,
          char *in);
int      get_global_grid_to_tfio(TF_Global *g, int ftype, void *h);

/* see file cps_global.f */
int   cps_global_addr_(void);
void  cps_global_grotm_(float *x, float *yo, float *dx, float *dn);
void  cps_global_srotm_(float *x, float *yo, float *dx, float *dn);

/* see file getgl_ftype.c */
int   getgl_ftype(char *file);
char *getgl_ftype_to_str(int type);
int   getgl_str_to_ftype(char *str);
int   getgl_ftype_hdata(char *file,int nbuf,
      char *buff, long *nbytes,int *type);


 
/* see readbyt.c    */
int  read_byt_(char name[], int  *istat, IO_request *Cl, int  *norm,
         int  *nhdrs, char hd[], unsigned char tr[],float *TASF );

/* see read_data.c  */
int  read_data_(char *, int  *, IO_request *, char hd[],
         char tr[],float *TASF, int *lun, int open_file);

/* see float_to_byt.c  */
void float_to_byt_(float ftr[], int  *nin,  unsigned char btr[],
     int  *nout, float *flav);
void float_to_bytsgn(float *ftr, int  *nin, unsigned char *btr,
     float *flav);
void float_to_bit16_(float *ftr, int  *nin,  char *str, float *flav);

int  byte_to_float_(unsigned char  ByteIn[],
                   int            *SamplesIn, int            *SamplesOut,
                   float          *MaxLav, float          FloatOut[]);
int  bit16_to_float_(short *shortin,
                   int      *SamplesIn, int         *SamplesOut,
                   float    *MaxLav, float          *FloatOut);


/* see dcodehdr.c */
int  dcodehdr(int  *istat, TF_Global *Glbls, IO_request *Cl,
     int  *nhdrs, char *hd, float *TASF);
int  dcodehd1(int  *istat, TF_Global *Glbls,
     int  *nhdrs, char *hd, float *cpshd, float *TASF);

/* see ccode.c */
int  ccode_( char fmt[][8], char names[][32], int off[], int  num,
     char *mem, char *cards, char *msg );


int tf_trcio_create_file_(char *filename, char *mode, int scratch, int nwih,
                          int nsamp, int nbits_trace, int nbits_header, 
                          float dt, float tmin, float tmax, float trmaxg,
                          int *lun);

int tf_trcio_write_trace_(int lun, float *hd, float *tr, int hd_size,  
                          int tr_size  );

void tf_trcio_close_file_(int lun, int *istat);


#ifdef __cplusplus  
}                   // for C++
#endif


#endif
