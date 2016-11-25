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
#ifndef _DSKIO_
#define _DSKIO_

#include "c2f_interface.h"

/* Flags for supported IO types */
#define DSKIO_CSYS 0
#define DSKIO_FFIO 1
#define DSKIO_SIO  2
#define DSKIO_AQIO 3
#define DSKIO_CSTR 4
#define DSKIO_FDA  5
#define DSKIO_PAIO 6

typedef struct DskioHandle_ {
  int   index;  /* a unique integer assigned to each open file */
  int   iotype;
  char  fname[256];
  long  offset;
  int   recl;
  int   oflag;
  void *iohandle;
  void *file_data;
  struct DskioHandle_ *next;
  struct DskioHandle_ *prev;
} DskioHandle;

typedef struct DskioChain_ {
   DskioHandle *first;
   DskioHandle *last;
   int          nlink;     /* count of open files */
} DskioChain;


#ifdef NEED_CAPITALS
#define dskiof_fdaop_    DSKIOF_FDAOP
#define dskiof_fdacl_    DSKIOF_FDACL
#define dskiof_fdard_    DSKIOF_FDARD
#define dskiof_fdawr_    DSKIOF_FDAWR
#define dskio_chain_iotype_ DSKIO_CHAIN_IOTYPE
#define dskio_chain_handle_ DSKIO_CHAIN_HANDLE
#define dskio_chain_fname_  DSKIO_CHAIN_FNAME
#define dskio_chain_recl_   DSKIO_CHAIN_RECL
#define dskio_chain_srecl_  DSKIO_CHAIN_SRECL
#define dskio_chain_oflag_  DSKIO_CHAIN_OFLAG
#define dskio_chain_file_data_  DSKIO_CHAIN_FILE_DATA
#define dskio_chain_sfile_data_  DSKIO_CHAIN_SFILE_DATA
#define dskio_siz_real_  DSKIO_SIZ_REAL
#define dskio_siz_float_ DSKIO_SIZ_FLOAT
#define dskio_xopc_      DSKIO_XOPC
#define dskio_xopb_      DSKIO_XOPB
#define dskio_xop_       DSKIO_XOP
#define dskio_xxcl_      DSKIO_XXCL
#define dskio_xsk_       DSKIO_XSK
#define dskio_xfsiz_     DSKIO_XFSIZ
#define dskio_xflush_    DSKIO_XFLUSH
#define dskio_xxskwr_    DSKIO_XXSKWR
#define dskio_xxskrd_    DSKIO_XXSKRD
#define dskio_xcl_       DSKIO_XCL
#define dskio_xskwr_     DSKIO_XSKWR
#define dskio_xskrd_     DSKIO_XSKRD
#define dskiocop_        DSKIOCOP
#define dskioccl_        DSKIOCCL
#define dskiocrd_        DSKIOCRD
#define dskiocwr_        DSKIOCWR
#define dskiowr_         DSKIOWR
#define dskiord_         DSKIORD
#define dskiorm_         DSKIORM
#define dskio_exist_     DSKIO_EXIST
#define dskio_aqop_      DSKIO_AQOP
#define dskio_aqcl_      DSKIO_AQCL
#define dskio_aqskrd_    DSKIO_AQSKRD
#define dskio_aqskwr_    DSKIO_AQSKWR
#define dskio_aqwt_      DSKIO_AQWT
#define dskio_a_aqskrd_  DSKIO_A_AQSKRD
#define dskio_a_aqskwr_  DSKIO_A_AQSKWR
#define dskio_ffop_      DSKIO_FFOP
#define dskio_ffcl_      DSKIO_FFCL
#define dskio_ffsk_      DSKIO_FFSK
#define dskio_ffskwr_    DSKIO_FFSKWR
#define dskio_ffskrd_    DSKIO_FFSKRD
#define dskio_sioop_     DSKIO_SIOOP
#define dskio_siocl_     DSKIO_SIOCL
#define dskio_siosk_     DSKIO_SIOSK
#define dskio_sioskwr_   DSKIO_SIOSKWR
#define dskio_sioskrd_   DSKIO_SIOSKRD
#define dskio_enrw_      DSKIO_ENRW
#define dskio_nrwt_      DSKIO_NRWT
#define dskio_nrw_       DSKIO_NRW
#define dskio_nwro_      DSKIO_NWRO
#define dskio_orw_       DSKIO_ORW
#define dskio_ordo_      DSKIO_ORDO
#define dskio_owro_      DSKIO_OWRO
#define dskio_is_local_  DSKIO_IS_LOCAL
#define dskio_host_      DSKIO_HOST
#define dskio_cwd_       DSKIO_CWD
#define dskio_envuser_   DSKIO_ENVUSER
#define dskio_netinfo_   DSKIO_NETINFO
#define dskio_host_to_ DSKIO_HOST_TO
#define dskio_to_host_ DSKIO_TO_HOST
#define dskio_host_to_optpath_ DSKIO_HOST_TO_OPTPATH
#define dskio_node_      DSKIO_NODE
#define dskio_userid_    DSKIO_USERID
#define dskio_file_      DSKIO_FILE
#define dskio_bare_file_ DSKIO_BARE_FILE
#define dskio_path_      DSKIO_PATH
#define dskio_ptmp_      DSKIO_PTMP
#define dskio_ext_       DSKIO_EXT
#define dskio_nonvol_    DSKIO_NONVOL
#define dskio_bld_full_  DSKIO_BLD_FULL
#define dskio_parse_full_ DSKIO_PARSE_FULL
#define dskio_parse_fullx1_ DSKIO_PARSE_FULLX1
#define dskio_netname_   DSKIO_NETNAME
#endif

#if(VMS || _AIX || __hpux)
#define dskiof_fdaop_    dskiof_fdaop
#define dskiof_fdacl_    dskiof_fdacl
#define dskiof_fdard_    dskiof_fdard
#define dskiof_fdawr_    dskiof_fdawr
#define dskio_chain_iotype_ dskio_chain_iotype
#define dskio_chain_handle_ dskio_chain_handle
#define dskio_chain_fname_  dskio_chain_fname
#define dskio_chain_recl_   dskio_chain_recl
#define dskio_chain_srecl_  dskio_chain_srecl
#define dskio_chain_oflag_  dskio_chain_oflag
#define dskio_chain_file_data_  dskio_chain_file_data
#define dskio_chain_sfile_data_  dskio_chain_sfile_data
#define dskio_siz_real_  dskio_siz_real
#define dskio_siz_float_ dskio_siz_float
#define dskio_xopc_      dskio_xopc
#define dskio_xopb_      dskio_xopb
#define dskio_xop_       dskio_xop
#define dskio_xxcl_      dskio_xxcl
#define dskio_xsk_       dskio_xsk
#define dskio_xfsiz_     dskio_xfsiz
#define dskio_xflush_    dskio_xflush
#define dskio_xxskrd_    dskio_xxskrd
#define dskio_xxskwr_    dskio_xxskwr
#define dskio_xcl_       dskio_xcl
#define dskio_xskwr_     dskio_xskwr
#define dskio_xskrd_     dskio_xskrd
#define dskiocop_        dskiocop
#define dskioccl_        dskioccl
#define dskiocrd_        dskiocrd
#define dskiocwr_        dskiocwr
#define dskiowr_         dskiowr
#define dskiord_         dskiord
#define dskiorm_         dskiorm
#define dskio_exist_     dskio_exist
#define dskio_aqop_      dskio_aqop
#define dskio_aqcl_      dskio_aqcl
#define dskio_aqskrd_    dskio_aqskrd
#define dskio_aqskwr_    dskio_aqskwr
#define dskio_aqwt_      dskio_aqwt
#define dskio_a_aqskrd_  dskio_a_aqskrd
#define dskio_a_aqskwr_  dskio_a_aqskwr
#define dskio_ffop_      dskio_ffop
#define dskio_ffcl_      dskio_ffcl
#define dskio_ffsk_      dskio_ffsk
#define dskio_ffskrd_    dskio_ffskrd
#define dskio_ffskwr_    dskio_ffskwr
#define dskio_sioop_     dskio_sioop
#define dskio_siocl_     dskio_siocl
#define dskio_siosk_     dskio_siosk
#define dskio_sioskrd_   dskio_sioskrd
#define dskio_sioskwr_   dskio_sioskwr
#define dskio_enrw_      dskio_enrw
#define dskio_nrwt_      dskio_nrwt
#define dskio_nrw_       dskio_nrw
#define dskio_nwro_      dskio_nwro
#define dskio_orw_       dskio_orw
#define dskio_ordo_      dskio_ordo
#define dskio_owro_      dskio_owro
#define dskio_is_local_  dskio_is_local
#define dskio_netinfo_   dskio_netinfo
#define dskio_host_to_ dskio_host_to
#define dskio_to_host_ dskio_to_host
#define dskio_host_to_optpath_ dskio_host_to_optpath
#define dskio_host_      dskio_host
#define dskio_cwd_       dskio_cwd
#define dskio_envuser_   dskio_envuser
#define dskio_node_      dskio_node
#define dskio_userid_    dskio_userid
#define dskio_file_      dskio_file
#define dskio_bare_file_ dskio_bare_file
#define dskio_path_      dskio_path
#define dskio_ptmp_      dskio_ptmp
#define dskio_ext_       dskio_ext
#define dskio_nonvol_    dskio_nonvol
#define dskio_bld_full_  dskio_bld_full
#define dskio_parse_full_ dskio_parse_full
#define dskio_parse_fullx1_ dskio_parse_fullx1
#define dskio_netname_   dskio_netname
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* see dskio.c     */
char *dskio_parse_file( char *file, char *name, char *path );
void dskio_cppath(char *hfile, char *file);
int  dskio_is_local_(char *fname, int *must_exist);
int  dskio_netinfo_(char * ninfo, char *node,char *user,char *path);
int  dskio_host_to_(char *host, char *disk);
int  dskio_to_host_(char *host, char *disk);
int  dskio_host_to_optpath_(char *host, char *disk, char *torp,
      int *nblk, int *byperblk);
int  dskio_host_(char * host);
int  dskio_cwd_(char *cwd);
int  dskio_envuser_(char *envuser);
int  dskio_node_(char *fname,char *node);
int  dskio_userid_(char *fname,char *userid);
int  dskio_file_(char *fname, char *file);
int  dskio_bare_file_(char *fname, char *bfile);
int  dskio_path_(char *fname, char *path);
int  dskio_ext_(char *name, char *ext);
int  dskio_nonvol_(char *fname, char *nvname,int *honor);
int  dskio_ptmp_(char *ptmp);
void dskio_bld_full_(char *full, char *u, char *n,char *p, char *bf);
void dskio_parse_full_(char *full, char *u, char *n,char *p, char *bf);
int  dskio_parse_fullx1_(char *fname, char *ninfo,
        char *rnode, char *ruser, char *rfile, char *bare,
        char *modified);
int  dskio_netname_(char *fname,char *fullname);

int  dskio_siz_real_(char *a, char *b);
int  dskio_siz_float_();

void dskio_set_buffers(int *size, int *npages);
void dskio_buffers(int *size, int *npages);
int dskio_convert_flag(int op_stat);

DskioHandle *DskioChainFromIndex(DskioChain *hc,int index);
DskioHandle *DskioChainFromName(DskioChain *hc,char *name);
DskioHandle *DskioChainFromHandle(DskioChain *hc,void *handle);
int  DskioChainIOtype(int ufi);
void DskioChainSIOtype(int iotpye, int ufi);
int  DskioChainCheck(DskioChain *hc, DskioHandle *dh);
void DskioChain_add(DskioChain *hc, DskioHandle *h);
void DskioChain_rm(DskioChain *hc, DskioHandle *h);
DskioHandle *DskioChainFirst(DskioChain *hc);
void DskioChainSFirst(DskioChain *hc, DskioHandle *first);
DskioHandle *DskioChainLast(DskioChain *hc);
void DskioChainSLast(DskioChain *hc, DskioHandle *h);
void DskioChainSLast(DskioChain *hc, DskioHandle *h);
void DskioChainSNlink(DskioChain *hc, int nlink);
int  dskio_chain_handle_(int *ufi);
int  dskio_chain_iotype_(int *ufi);
int  dskio_chain_recl_(int *ufi);
void dskio_chain_srecl_(int *recl,int *ufi);
int  dskio_chain_oflag_(int *ufi);
void dskio_chain_fname_(int *ufi, char *fname);
void *dskio_chain_file_data_(int *ufi);
void dskio_chain_sfile_data_(void *file_data,int *ufi);


DskioHandle *DskioHandleNew(int iotype, int oflag, char *fname);
void DskioHandleDel(DskioHandle *dh);
int DskioHandleNewIndex(DskioChain *hc);
DskioHandle *DskioHandleNext(DskioHandle *dh);
DskioHandle *DskioHandlePrev(DskioHandle *dh);
int  DskioHandleIndex(DskioHandle *dh);
int  DskioHandleIOtype(DskioHandle *dh);
int  DskioHandleOflag(DskioHandle *dh);
int  DskioHandleRecl(DskioHandle *dh);
void *DskioHandleIOhandle(DskioHandle *dh);
char *DskioHandleFname(DskioHandle *dh);
void DskioHandleSIndex(int index, DskioHandle *dh);
void DskioHandleSRecl(int recl,DskioHandle *dh);
void DskioHandleSIOtype(int iotype, DskioHandle *dh);
void DskioHandleSIOhandle(void *iohandle, DskioHandle *dh);
void DskioHandleSFname(char *fname, DskioHandle *dh);
void DskioHandleSNext(DskioHandle *O, DskioHandle *n);
void DskioHandleSPrev(DskioHandle *O, DskioHandle *p);
void *DskioHandleFileData(DskioHandle *h);
void DskioHandleSFileData(DskioHandle *h, void *file_data);


void dskio_xopc_(int *iotype,int *ufi, char *fname, int *oflag,
     int *i_err, int *bsize, int *npages,int *recl, int *wrd);
void dskio_xopb_(int *iotype,int *ufi, char *fname, int *oflag,
     int *i_err, int *bsize, int *npages);
void dskio_xop_(int *iotype, int *ufi, char *fname,int *oflag, int *i_err);
long dskio_xfsize(int *ufi);
long dskio_xfsiz_(int *ufi);
void dskio_xsk_(int *ufi, long *off, int *i_err);
void dskio_xflush_(int *ufi, int *i_err);
void dskio_xxcl_(int *ufi, int *i_err);
void dskio_xxrd_(int *ufi, char *b, int *nby, int *i_err);
void dskio_xxwr_(int *ufi, char *b, int *nby, int        *i_err);
void dskio_xxskrd_(int *ufi, char *b, long *off, int *nby, int *i_err);
void dskio_xxskwr_(int *ufi, char *b, long *off, int *nby, int   *i_err);
void dskio_xcl_(int *iotype,int *ufi, int *i_err);
void dskio_xskrd_(int *iotype,int *ufi, char *b,
     long *off, int *nby, int *i_err);
void dskio_xskwr_(int *iotype,int *ufi, char *b,
     long *off, int *nby, int   *i_err);


void dskio_aqop_(int *iaq, char *fname,int *oflag, int *ierr);
void dskio_aqcl_(int *iaq, int *ierr);
void dskio_aqsk_(int *iaq, long *off, int *i_err);
void dskio_aqskrd_(int *iaq,char *b, long *off, int *nb, int *ierr);
void dskio_aqskwr_(int *iaq,char *b, long *off, int *nb, int *ierr);

void dskio_sysop_(int *fd, char *fname,int *oflag, int *i_err);
void dskio_syscl_(int *fd, int *i_err);
void dskio_syswr_(int *fd, char *b, int *nby, int *i_err);
void dskio_sysrd_(int *fd, char *b, int *nby, int *i_err);
void dskio_syssk_(int *fd, long *off, int *i_err);
void dskio_sysskrd_(int *fd, char *b,long *off, int *nby, int *i_err);
void dskio_sysskwr_(int *fd, char *b,long *off, int *nby, int *i_err);

FILE *dskio_strop_(char *fname,int *oflag,int *i_err,int *bsize);
void dskio_strsk_(FILE *fp, long *off, int *i_err);
void dskio_strwr_(FILE *fp, char *b, int *nby, int *i_err);
void dskio_strrd_(FILE *fp, char *b, int *nby, int *i_err);
void dskio_strskrd_(FILE *fp, char *b,long *off, int *nby, int *i_err);
void dskio_strskwr_(FILE *fp, char *b,long *off, int *nby, int *i_err);

void dskio_ffop_(int *fd, char *fname,int *oflag, int *i_err,
     int *psize,int*npages);
void dskio_ffcl_(int *fd, int *i_err);
void dskio_ffwr_(int *fd, char *b, int *nby, int *i_err);
void dskio_ffrd_(int *fd, char *b, int *nby, int *i_err);
void dskio_ffsk_(int *fd, long *off, int *i_err);
void dskio_ffskrd_(int *fd, char *b,long *off, int *nby, int *i_err);
void dskio_ffskwr_(int *fd, char *b,long *off, int *nby, int *i_err);

void dskio_sioop_(int *fd, char *fname,int *oflag,  int *i_err);
void dskio_siocl_(int *fd, int *i_err);
void dskio_siowr_(int *fd, char *b, int *nby, int *i_err);
void dskio_siord_(int *fd, char *b, int *nby, int *i_err);
void dskio_siosk_(int *fd, long *off, int *i_err);
void dskio_sioskrd_(int *fd, char *b,long *off, int *nby, int *i_err);
void dskio_sioskwr_(int *fd, char *b,long *off, int *nby, int *i_err);

/* prototype the methods in dskiof.f for fortran DA IO */
void dskiof_fdaop_(int *fd, char *fname,char *status,
     int *byte_rec_siz, int *i_err);
void dskiof_fdacl_(int *ufi);
void dskiof_fdard_(int *ufi, long *off, int *nby ,char *b, int *i_err);
void dskiof_fdawr_(int *ufi, long *off, int *nby ,char *b, int *i_err);

void dskiocop_(int  *ifile, char *file, int  *imode);
void dskioccl_(int  *ifile);
void dskiocrd_(int  *ifile,char *buff,int  *np,int  *irec,int  *ierr);
void dskiocwr_(int  *ifile,char *buff,int  *np,int  *irec,int  *ierr);
int  dskiowr_(int  *fd, long *off, int  *nby, char *buf);
int  dskiord_(int  *fd, long *off, int  *nby, char *buf);
int  dskiorm_(char *name);
int  dskio_exist_(char *fname);

int  dskio_enrw_();
int  dskio_nrwt_();
int  dskio_nrw_();
int  dskio_nwro_();
int  dskio_orw_();
int  dskio_ordo_();
int  dskio_owro_();

#ifdef __cplusplus
}                   // for C++
#endif

#endif

