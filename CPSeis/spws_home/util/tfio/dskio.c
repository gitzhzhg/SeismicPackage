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
#if VMS
#include <stdlib.h>
#include <unixio.h>
#include <unixlib.h>
#include <file.h>
#include <types.h>
#define F_OK 0
#elif CRAY
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <ffio.h>
#else
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#endif
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "dskio.h"

int dskio_rmquote(char *name);

void dskio_paop_(int *fd, char *fname,int *oflag,int *i_err);
void dskio_pawr_(int *fd, char *b, int *nby, int *i_err);
void dskio_paskwr_(int *fd, char *b,long *off, int *nby, int *i_err);
void dskio_pard_(int *fd, char *b, int *nby, int *i_err);
void dskio_paskrd_(int *fd, char *b,long *off, int *nby, int *i_err);
int fseek_pa(int fd,long off,int mode) ;
int fopen_pa(char *name, char *mode) ;
int fclose_pa(int fd) ;
int fread_pa(char *b, size_t siz, size_t n, int fd) ;
int fwrite_pa(char *b, size_t siz, size_t n, int fd) ;


static DskioChain hchain;

#ifdef CRAY
static int dskio_bsize=1048576;
static int dskio_num_pages=12;
#else
static int dskio_bsize=16384;
static int dskio_num_pages=1;
#endif

typedef  struct HostPar_ {
   char *host;
   char *disk;
} HostPar;
static  HostPar dtable[] ={
  "poeplx01", "/ptmp/poeplx01/scratch",
  "poeplx02", "/ptmp/poeplx02/scratch",
  "poeplx03", "/ptmp/poeplx03/scratch",
  "poeplx04", "/ptmp/poeplx04/scratch",
  "poeplx05", "/ptmp/poeplx08/scratch",
  "poeplx06", "/ptmp/poeplx08/scratch",
  "poeplx07", "/ptmp/poeplx08/scratch",
  "poeplx08", "/ptmp/poeplx08/scratch",
  "poeplx09", "/ptmp/poeplx12/scratch",
  "poeplx10", "/ptmp/poeplx12/scratch",
  "poeplx11", "/ptmp/poeplx12/scratch",
  "poeplx12", "/ptmp/poeplx12/scratch",
  "poeplx13", "/ptmp/poeplx16/scratch",
  "poeplx14", "/ptmp/poeplx16/scratch",
  "poeplx15", "/ptmp/poeplx16/scratch",
  "poeplx16", "/ptmp/poeplx16/scratch",
  "poeplx17", "/ptmp/poeplx20/scratch",
  "poeplx18", "/ptmp/poeplx20/scratch",
  "poeplx19", "/ptmp/poeplx20/scratch",
  "poeplx20", "/ptmp/poeplx20/scratch",
  "poeplx21", "/ptmp/poeplx24/scratch",
  "poeplx22", "/ptmp/poeplx24/scratch",
  "poeplx23", "/ptmp/poeplx24/scratch",
  "poeplx24", "/ptmp/poeplx24/scratch",
  "poeplx25", "/ptmp/poeplx28/scratch",
  "poeplx26", "/ptmp/poeplx28/scratch",
  "poeplx27", "/ptmp/poeplx28/scratch",
  "poeplx28", "/ptmp/poeplx28/scratch",
  "poeplx29", "/ptmp/poeplx32/scratch",
  "poeplx30", "/ptmp/poeplx32/scratch",
  "poeplx31", "/ptmp/poeplx32/scratch",
  "poeplx32", "/ptmp/poeplx32/scratch",
  "poeplx33", "/ptmp/poeplx36/scratch",
  "poeplx34", "/ptmp/poeplx36/scratch",
  "poeplx35", "/ptmp/poeplx36/scratch",
  "poeplx36", "/ptmp/poeplx36/scratch",
  "poeplx37", "/ptmp/poeplx40/scratch",
  "poeplx38", "/ptmp/poeplx40/scratch",
  "poeplx39", "/ptmp/poeplx40/scratch",
  "poeplx40", "/ptmp/poeplx40/scratch",
  "poeplx41", "/ptmp/poeplx44/scratch",
  "poeplx42", "/ptmp/poeplx44/scratch",
  "poeplx43", "/ptmp/poeplx44/scratch",
  "poeplx44", "/ptmp/poeplx44/scratch",
  "poeplx45", "/ptmp/poeplx48/scratch",
  "poeplx46", "/ptmp/poeplx48/scratch",
  "poeplx47", "/ptmp/poeplx48/scratch",
  "poeplx48", "/ptmp/poeplx48/scratch",
  "poeplx49", "/ptmp/poeplx52/scratch",
  "poeplx50", "/ptmp/poeplx52/scratch",
  "poeplx51", "/ptmp/poeplx52/scratch",
  "poeplx52", "/ptmp/poeplx52/scratch",
  "poepsn03","./"
 };

#define PMODE 0666
int dskio_size();



/*-------------------------------------------------------------
C\USER DOC
C Name   : dskio_xop_    dskio_xcl_    dskio_xsk_,
C        : dskio_xrd_    dskio_xwr_    dskio_xskrd_  dskio_xskwr_
C        : dskio_xflush_
C        : dskio_ffop_   dskio_ffcl_   dskio_ffsk
C        : dskio_ffrd_   dskio_ffwr_   dskio_ffskrd_ dskio_ffskwr_
C        : dskio_sioop_  dskio_siocl_  dskio_siosk_
C        : dskio_siord_  dskio_siowr_
C        : dskio_sysop_  dskio_syscl_  dskio_syssk_
C        : dskio_sysrd_  dskio_syswr_  dskio_sysskrd_  dskio_sysskwr_
C        : dskio_aqop_   dskio_aqcl_   dskio_aqwt_
C        : dskio_aqskrd_ dskio_a_aqskrd_
C        : dskio_aqskwr_ dskio_a_aqskwr_
C        : dskio_strop_  dskio_strsk_  dskio_strrd_  dskio_strwr_
C        : dskio_strskrd_dskio_strskwr_
C        : dskiocop_     dskioccl_     dskiocrd_     dskiocwr_
C        : dskiowr_      dskiord_      dskiorm_
C        : dskio_chain_iotype_         dskio_chain_fname_
C        : dskio_chain_handle_         dskio_chain_recl_
C        : dskio_enrw_   dskio_nrwt_   dskio_nrw_
C        : dskio_nwro_   dskio_orw_    dskio_ordo_ dskio_owro_
C Purpose: open, close, seek, read and write disk files using
C          a variety of IO mathods.
C Written: R.S. Day, C.C. Burch, Scott Morton
C Author : R. Day
C Last revised: 99/10/18
C
C Function Prototypes:        ( Language = C )
C ufi        I&O        Unique File Index that is assigned at
C                       open.
C iotype     I          Specifies IO type.
C                       0 - C read & write calls(unbuffered)
C                       1 - Cray ffio calls
C                       2 - Cray sio calls(under construction)
C                       3 - Cray aqio calls
C                       4 - C fread & fwrite calls(buffered)
C                       5 - Fortran Direct Access
C dh         I&O        DskioHandle - a file handle.
C fd         I&O        A file descriptor
C fp         I&O        A "C-stream FILE" pointer.
C fname      I          The file name.
C i_err      O          0 if no errors occur.
C off        I          byte offset into a file
C nby        I          Number of bytes to read or write.
C oflag      I          flag for open mode. Call one of the
C                       utility routines to set this
C                       (e.g. dskio_nrw_(), dskio_nrwt_(),...)
C off        I          offset in bytes from file start
C bsize      I          A buffer size when iotype=1 or 4
C npages     I          Number of buffer pages when iotype=1
C recl       I          record length in ? for iotype=5
C wrd        I          if 1 recl is in units of ints
C                       otherwise it is bytes.
C imode      I          Open status.
C irec       I          Points to the irec'th block of 512 bytes
C
C        --- gateway io routines ---
C void dskio_xopc_(int *iotype,int *ufi,char *fname,int *oflag,
C                 int *i_err,int *bsize,int *npages,int *recl,
C                 int *wrd);
C void dskio_xopb_(int *iotype,int *ufi,char *fname,int *oflag,
C                 int *i_err,int *bsize, int *npages);
C void dskio_xop_(int *iotype,int *ufi,char *fname,int *oflag,
C                 int *i_err);
C long dskio_xfsize(int *ufi);
C void dskio_xxcl_(int *ufi,int *i_err);
C void dskio_xxwr_(int *ufi,char *b,
C                 int *nby,int *i_err);
C void dskio_xxrd_(int *ufi,char *b,
C                 int *nby,int *i_err);
C void dskio_xsk_(int *ufi,long *off,int *i_err);
C void dskio_xxskwr_(int *ufi,char *b,long *off,
C                   int *nby,int *i_err);
C void dskio_xxskrd_(int *ufi,char *b,long *off,
C                   int *nby,int *i_err);
C        --- opt = 0 --- low level system c calls
C  void dskio_sysop_(int *fd, char *fname,int *oflag,
C                 int *i_err);
C  void dskio_syscl_(int *fd, int *i_err);
C  void dskio_syssk_(int *fd, long *off,int *i_err);
C  void dskio_sysrd_(int *fd, char *b,int *nby,int *i_err);
C  void dskio_syswr_(int *fd, char *b,int *nby,int *i_err);
C  void dskio_sysskwr_(int *fd, char *b,long *off, int *nby,
C                 int *i_err)
C  void dskio_sysskrd_(int *fd, char *b,long *off, int *nby,
C                 int *i_err)
C        --- opt = 1 ---
C  void dskio_ffop_(int *fd, char *fname,int *oflag,
C                 int *i_err);
C  void dskio_ffcl_(int *fd, int *i_err);
C  void dskio_ffsk_(int *fd, long *off,int *i_err);
C  void dskio_ffrd_(int *fd, char *b,int *nby,int *i_err);
C  void dskio_ffwr_(int *fd, char *b,int *nby,int *i_err);
C  void dskio_ffskrd_(int *fd, char *b,long *off, int *nby,
C                 int *i_err)
C  void dskio_ffskwr_(int *fd, char *b,long *off, int *nby,
C                 int *i_err)
C        --- opt = 2 --- cray sio
C  void dskio_sioop_(int *fd, char *fname,int *oflag,
C                 int *i_err);
C  void dskio_siocl_(int *fd, int *i_err);
C  void dskio_siosk_(int *fd, long *off,int *i_err);
C  void dskio_siord_(int *fd, char *b,int *nby,int *i_err);
C  void dskio_siowr_(int *fd, char *b,int *nby,int *i_err);
C        --- opt = 3 --- cray aqio
C  void dskio_aqop_(int *iaq, char *fname,int *oflag,
C                   int *ierr);
C  void dskio_aqcl_(int *iaq, int *ierr)
C  void dskio_aqsk_(int *fd, long *off, int *i_err)
C  void  dskio_aqwt_(int *fd, int *i_err)
C  void dskio_aqskrd_(int *iaq,int *b,long *off, int *nb,
C                   int *ierr)
C  void dskio_a_aqskrd_(int *iaq,int *b,long *off, int *nb,
C                   int *ierr)
C  void dskio_aqskwr_(int *iaq,int *b,long *off, int *nb,
C                   int *ierr)
C  void dskio_a_aqskwr_(int *iaq, char *b,long *off, int *nb,
C                   int *i_err)
C        --- opt = 4 --- stream file IO
C  FILE *dskio_strop_(char *fname,int *oflag,int *i_err,
C                     int *bsize)
C  void dskio_strsk_(FILE *fp, long *off, int *i_err)
C  void dskio_strwr_(FILE *fp, char *b, int *nby, int *i_err)
C  void dskio_strrd_(FILE *fp, char *b, int *nby, int *i_err)
C  void dskio_strskrd_(FILE *fp, char *b,long *off, int *nby,
C                    int *i_err)
C  void dskio_strskwr_(FILE *fp, char *b,long *off, int *nby,
C                    int *i_err)
C        --- opt = 5 --- Fortran Direct Access IO
C  The methods called are in dskiof.f
C  void dskiof_fdaop_(int *fd, char *fname,char *status,
C                 int *byte_rec_siz, int *i_err);
C  void dskiof_fdacl_(int *fd);
C  void dskiof_fdard_(int *fd, int *recl,long *off, int *nby,
C                    char *b, int *i_err)
C  void dskiof_fdawr_(int *fd, int *recl,long *off, int *nby,
C                    char *b, int *i_err)
C        --- miscellaneous routines ---
C  void dskiocop_(int  *fd,char *fname, int  *imode)
C  void dskioccl_(int  *fd)
C  void dskiocrd_(int  *fd,char *buff,int  *nby,int  *irec,int *i_err)
C  void dskiocwr_(int  *fd,char *buff,int  *nby,int  *irec,int *i_err)
C  int  dskiord_ (int  *fd,long *off, int  *nby, char *buf)
C  int  dskiowr_ (int  *fd,long *off, int  *nby, char *buf)
C  void dskiorm_ (char *name)
C  int  dskio_exist_(chard *fname)
C  int dskio_chain_iotype_(int *ufi);
C  void dskio_chain_fname_(int *ufi, char *fname);
C
C         --- Utilities for name parsing ---
C  char *dskio_parse_file( char *file, char *name, char *path )
C  void dskio_cppath(char *hfile, char *file)
C  int  dskio_node_(char *fname,char *node)
C  int  dskio_userid_(char *fname,char *userid)
C  int  dskio_file_(char *fname, char *file)
C  int  dskio_bare_file_(char *fname, char *bfile)
C  int  dskio_path_(char *fname, char *path)
C  void dskio_nonvol_(char *fname, char *nvname)
C  int  dskio_ext_(char *name, char *ext)
C  int  dskio_ptmp_(char *ptmp)
C  void dskio_bld_full_(char *full,
C            char *u, char *n,char *p, char *bf)
C  void dskio_parse_full_(char *full,
C            char *u, char *n,char *p, char *bf)
C  int  dskio_parse_fullx1_(char *fname, char *ninfo,
C        char *rnode, char *ruser, char *rfile, char *bare,
C        char *modified)
C  int dskio_location_(char *node,
C            char *symb_path, char *true_path)
C
C         --- Utilities for non resident files ---
C  int  dskio_host_(char *host)
C  int  dskio_cwd_(char *cwd)
C  int  dskio_envuser_(char *envuser)
C  int  dskio_is_local_(char *fname, int *must_exist)
C  int  dskio_netinfo_(char *netinfo, char *node,char *user,char *path)
C
C  int dskio_host_to_(char *host, char *disk);
C  int dskio_to_host_(char *host, char *disk);
C
C  The following return some common open flag masks
C  which can be used in the open calls
C  int dskio_enrw_() : exclusive-new-read-write
C  int dskio_nrwt_() : new-read-write-truncate
C  int dskio_nrw_()  : new-read-write
C  int dskio_nwro_() : new-readonly
C  int dskio_orw_()  : old-read-write
C  int dskio_ordo_() : old-readonly
C  int dskio_owro_() : old-writeonly
C--------------------------------------------------------------
C
C NOTES:
C  1. The dskio_x* routines are a gateway to a variety of IO
C     methods. The dskio_xop routine sets the value a unique
C     file index, ufi. Pass this index to the read, write and
C     close calls. The value of iotype determines what is called.
C     (refer to documentation above)
C  2. imode = 0 read_only, old file.   (for dskiocop only)
C     imode = 1 write_only, old file.
C     imode = 2 read_write, old file.
C     imode = 3 write_only, new file.
C     imode = 4 read_write, new file.
C     imode =-4 read_write, old or new file.
C  3) dskio_sys??_ are an interface to the unbuffered c
C     calls(read,write), whereas dskio_str?? are an interface
C     to the buffered C calls(fread,fwrite).
C
C                         REVISION HISTORY
C
C   DATE     WHO         DESCRIPTION
C   -------- --------    --------------------------------------
C17.99/10/18 R.S.Day     Added dskio_xfsiz_ for fortran
C16.99/10/07 R.S.Day     Added function dskio_host_to_optpath
C15.99/09/20 R.S.Day     Added functions dskio_host_to_ and
C                        dskio_to_host_. static  HostPar dtable[]
C14.99/08/12 R.S.Day     dskio_netinfo fixed
C13.99/05/24 R.S.Day     Updating Conlib
C12.99/05/18 R.S.Day     Added dskio_rmquote to deal with quoted
C                        file names. Revised dskio_netname to
C                        deal with pogun addresses.
C                        Eliminated a printf in dskiorm
C11.99/04/29 R.S.Day     corrected small bug in dskio_bare_file.
C                        added dskio_is_local, dskio_netinfo
C                        dskio_parse_fullx1, dskio_host_
C                        dskio_cwd_ dskio_siz_float_,
C                        DskioHandleFileData, DskioHandleSFileData
C                        dskio_chain_file_data_,
C                        dskio_chain_sfile_data_ functions.
C                        Made slight mod to dskio_ptmp for VMS.
C                        Made dskio_file smarter about VMS names.
C                        dskio_xopc sets record length for all
C                        IO types and not just Fortran DA. 
C                        Added a new internal variable to the
C                        DskioHandle object.
C10.99/04/01 R.S.Day     Added a check on the file name in
C                        dskio_xop, in addition to the ufi.
C                        Failed to open file in some situations.
C                        Removed duplicate code for dskio_xcl,
C                        dskio_xrd,dskio_xwr,dskio_xskrd,
C                        dskio_xskwr and made them a wrapper
C                        around the dskio_xx* routinines.Added
C                        a method dskio_xopc for better control
C                        of fortran IO.
C 9.99/03/23 R.S.Day     dskio_xsk was not updating offset.
C 8.99/03/08 R.S.Day     Corrected a logic error in dskio_strop
C                        when opening new output STREAM files.
C 7.99/03/02 R.S.Day     A list of managed files is maintained.
C                        Unecessary seeks are avoided by 
C                        retaining the current file position.
C 6.99/02/23 R.S.Day     Changed file offsets from int to long.
C                        Added dskio_str* routines for buffered
C                        C-STREAM IO. Added control over ffio
C                        buffering via dskio_xopb call and the
C                        dskio_set_buffers,dskio_buffers functions.
C 5.98/12/09 R.S.Day     Updateing in conlib
C 4.98/11/30 R.S.Day     Added dskio_ext function.
C 3.98/10/29 R.S.Day     Added dskio_nonvol function. File name
C                        parsing recognizes PC paths as well as
C                        UNIX and VMS paths now. dskio_cppath
C                        added.
C 2.98/09/09 R.S.Day     Added functions for parsing file names
C 1.97/12/01 R.S.Day     Added routines for access to IO calls
C                        that are specific to Cray. FFIO,SIO,
C                        and AQIO. revert to low level C of
C                        not on a Cray.
C\END DOC
 *------------------------------------------------------------*/

long dskio_xfsiz_(int *ufi)
{ return dskio_xfsize(ufi); }

long dskio_xfsize(int *ufi)
{DskioHandle *dh= DskioChainFromIndex(&hchain, *ufi);
 int   p,i_err;
 off_t ot= 0, pos;
 long  o = 0,old_off;
 if(!dh) return 0;

  old_off = dh->offset;
  if(old_off< 0) old_off=0;
  switch (dh->iotype)   {
  case DSKIO_CSYS:
     if(*(int *) dh->iohandle<1) return 0;
     pos = lseek(*(int *)dh->iohandle,ot,SEEK_END);
     break;
  case DSKIO_FFIO:
     if(*(int *) dh->iohandle<0) return 0;
#ifdef CRAY
     p =   ffseek(*(int *)dh->iohandle,ot,SEEK_END);
     pos = p;
#else
     pos = lseek(*(int *)dh->iohandle,ot,SEEK_END);
#endif
     break;
  case DSKIO_SIO:
     if(*(int *) dh->iohandle<0) return 0;
#ifdef _CRAYMPP
     /*pos = sio_lseek(*fd,ot,SEEK_SET); */
     pos= -1;
#else
     pos = lseek(*(int *)dh->iohandle,ot,SEEK_END);
#endif
     break;
  case DSKIO_CSTR:
     if(!dh->iohandle) return 0;
     i_err = fseek((FILE *)dh->iohandle,0,SEEK_END);
     p = ftell((FILE *) dh->iohandle);
     pos=p;
     break;
  }
 if(old_off > pos) old_off=pos;
 dskio_xsk_(ufi, &old_off,&i_err);
 return (long) pos;
}

/* To be called from fortran to determine REAL size */
int dskio_siz_real_(char *a, char *b) {
 int n;
 return n = (b>a) ? b-a : a-b;
}
int dskio_siz_float_()
{ return sizeof(float);}

void dskio_set_buffers(int *size, int *npages)
{dskio_num_pages= *npages;
 dskio_bsize= *size;
}

void dskio_buffers(int *size, int *npages)
{*npages=dskio_num_pages;
 *size=dskio_bsize;
}

int dskio_size()
{return dskio_bsize;}

/* return some common open mode settings */
void dskio_xopb_(int *opt,int *ufi, char *fname, int *oflag,
     int *i_err, int *size, int *npages)
{int rec_size=8192, wrd=0;
 dskio_xopc_(opt,ufi,fname,oflag,i_err,size,npages,
       &rec_size,&wrd);
}

void dskio_xop_(int *opt, int *ufi, char *fname,int *oflag,
     int *i_err)
{ int def_size,def_pages,recl=8192,wrd=0;
  dskio_buffers(&def_size,&def_pages);
  dskio_xopc_(opt, ufi, fname,oflag,
     i_err, &def_size,&def_pages,&recl,&wrd);
}

void dskio_xopc_(int *opt, int *ufi, char *fname,int *oflag,
     int *i_err, int *bsize, int *npages,int *recl, int *wrd)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 int def_size,recl_max,recl_bytes;
 char status[16];
 if(dh) {/* already open ? */
   if(strcmp(fname,DskioHandleFname(dh))==0) {
     *i_err= 0;
      return;
   }
 }
 *i_err= 1;
 *ufi= -1;
 /* We assume memory already allocated for the pointer */
 if(strcmp(fname,"NONE")==0) return;
 if(strcmp(fname,"none")==0) return;
 dh = DskioHandleNew(*opt,*oflag,fname);
 if(!dh) return;
 recl_bytes = *recl;
 if(*wrd==1) recl_bytes= *recl *sizeof(int);
 DskioHandleSRecl(recl_bytes,dh); /* set record length */
  switch (*opt) {
  case DSKIO_CSYS:
   dskio_sysop_((int *) dh->iohandle, fname,oflag,i_err);
   break;
  case DSKIO_PAIO:
   dskio_paop_((int *) dh->iohandle, fname,oflag,i_err);
   break;
  case DSKIO_FFIO:
   dskio_ffop_((int *)  dh->iohandle, fname,oflag,i_err,bsize,npages);
   break;
  case DSKIO_SIO:
   dskio_sioop_((int *) dh->iohandle, fname,oflag,i_err);
   break;
  case DSKIO_AQIO:
   dskio_aqop_((int *)  dh->iohandle, fname,oflag,i_err);
   break;
  case DSKIO_CSTR:
   dh->iohandle =(void *) dskio_strop_(fname,oflag,i_err,bsize);
   break;
  case DSKIO_FDA:
   recl_bytes = *recl;
   if(*wrd==1) recl_bytes= *recl *sizeof(int);
   def_size = recl_bytes;
   recl_max = recl_bytes;
#ifdef VMS
   recl_max=32768;
#endif
   def_size=(def_size>recl_max) ? recl_max : def_size;
   strcpy(status,"OLD");
   if(*oflag & O_CREAT) strcpy(status,"NEW");
   dskiof_fdaop_( (int *) dh->iohandle,fname,status, &def_size,i_err);
   DskioHandleSRecl(def_size,dh);
   break;
  default:
   dh->iohandle =(void *) dskio_strop_(fname,oflag,i_err,bsize);
   *opt=DSKIO_CSTR;
  }

 if(*i_err != 0) {
  DskioHandleDel(dh);
  dh = 0;
  fprintf(stderr,"dskio_xop: error - file=%s\n",fname);
 } else {
  dh->offset=0;
  DskioChain_add(&hchain,dh);
  *ufi = dh->index;
/*
  printf("dskio_xop: UFI=%d FileCount=%d\n",*ufi,DskioChainNlink(&hchain));
*/
 }
 return;
}

void dskio_xcl_(int *iotype,int *ufi, int *i_err)
{
 dskio_xxcl_(ufi, i_err);
}

void dskio_xxcl_(int *ufi, int *i_err)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 *i_err= -1;
 if(!dh) return;

  switch (dh->iotype)   {
  case DSKIO_CSYS:
    if(*(int *)dh->iohandle <=0) return;
    *i_err = close(*(int *)dh->iohandle);
    break;
  case DSKIO_PAIO:
    if(*(int *)dh->iohandle <=0) return;
    if(fclose_pa(*(int *)dh->iohandle)==0) *i_err=0;
    break;
  case DSKIO_FFIO:
    if(*(int *)dh->iohandle < 0) return;
#ifdef CRAY
    *i_err = ffclose(*(int *)dh->iohandle       );
#else
    *i_err = close(*(int *)dh->iohandle);
#endif
    break;
  case DSKIO_SIO:
    if(*(int *)dh->iohandle <=0) return;
    dskio_siocl_(dh->iohandle,i_err);
    break;
  case DSKIO_AQIO:
#ifdef CRAY
    AQCLOSE((int *)dh->iohandle,i_err);
    *i_err=0;
#else      /*c stream io */
    if(*(int *) dh->iohandle<1) return;
    *i_err = close(*(int *)dh->iohandle);/*1st word of iaq=fd */
#endif
    break;
  case DSKIO_CSTR:  /*c stream file io */
    if(dh->iohandle) {
     if(fclose((FILE *) dh->iohandle)==0) *i_err=0;
    }
    break;
  case DSKIO_FDA:  /*fortran direct access io */
    if(*(int *)dh->iohandle <=0) return;
    dskiof_fdacl_((int *) ufi);
    break;
  }

 DskioChain_rm(&hchain,dh);
 DskioHandleDel(dh);
/*
 printf("dskio_xxcl:links=%d\n",DskioChainNlink(&hchain));
*/
}

void dskio_xflush_(int *ufi, int *i_err)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
  *i_err= -1;
  if(!dh) return;
  switch (dh->iotype)   {
  case DSKIO_CSTR: /* buffered */
     if(!dh->iohandle) return;
     *i_err = fflush((FILE *) dh->iohandle);
     (*i_err) = (*i_err == 0) ? 0 : -1;
     break;
  default:
   *i_err=0; /* no flush for AQIO,FDA,CSYS,SIO */
  }
}

void dskio_xsk_(int *ufi, long *off,int *i_err)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 int   p;
 off_t ot= *off, pos;
 long  o = *off;
  *i_err= -1;
  if(!dh) return;
  dh->offset= -1;
  switch (dh->iotype)   {
  case DSKIO_CSYS:
     if(*(int *) dh->iohandle<1) return;
     pos = lseek(*(int *)dh->iohandle,ot,SEEK_SET);
     (*i_err) = (pos == -1) ? -1 : 0;
     break;
  case DSKIO_PAIO:
     if(*(int *) dh->iohandle<1) return;
     *i_err = fseek_pa(*(int *)dh->iohandle,*off,SEEK_SET);
     (*i_err) = (*i_err == 0) ? 0 : -1;
     break;
  case DSKIO_FFIO:
     if(*(int *) dh->iohandle<0) return;
#ifdef CRAY
     p =   ffseek(*(int *)dh->iohandle,ot,SEEK_SET);
     (*i_err) = (p == -1) ? -1 : 0;
#else
     pos = lseek(*(int *)dh->iohandle,ot,SEEK_SET);
     (*i_err) = (pos == -1) ? -1 : 0;
#endif
     break;
  case DSKIO_SIO:
     if(*(int *) dh->iohandle<0) return;
#ifdef _CRAYMPP
     /*pos = sio_lseek(*fd,ot,SEEK_SET); */
     pos= -1;
#else
     pos = lseek(*(int *)dh->iohandle,ot,SEEK_SET);
#endif
     (*i_err) = (pos == -1) ? -1 : 0;
     break;
  case DSKIO_CSTR:
     if(!dh->iohandle) return;
     *i_err = fseek((FILE *)dh->iohandle,*off,SEEK_SET);
     (*i_err) = (*i_err == 0) ? 0 : -1;
     break;
  default:
   *i_err=0; /* no seek for AQIO and FDA */
  }
 if(*i_err == 0) dh->offset = *off;
}

void dskio_xskwr_(int *iotype, int *ufi, char *b,long *off,int *nby,int *i_err)
{
 dskio_xxskwr_(ufi, b,off,nby,i_err);
}

void dskio_xxskwr_(int *ufi, char *b,long *off,int *nby,int *i_err)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 long new_off= *off;

  *i_err= -1;
  if(*nby <=0) { /* do nothing when no data */
   *i_err=0;
   return;
  }
  if(!dh) return;
  switch (dh->iotype)   {
  case DSKIO_CSYS:
   if(dh->offset==new_off)
     dskio_syswr_((int *)dh->iohandle,b,nby,i_err);
   else
     dskio_sysskwr_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_PAIO:
   if(dh->offset==new_off)
     dskio_pawr_((int *)dh->iohandle,b,nby,i_err);
   else
     dskio_paskwr_((int *)dh->iohandle,b,off,nby,i_err);
   break;
   break;
  case DSKIO_FFIO:
   if(dh->offset==new_off)
     dskio_ffwr_((int *)dh->iohandle,b,nby,i_err);
   else
     dskio_ffskwr_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_SIO:
   if(dh->offset==new_off)
     dskio_siowr_((int *)dh->iohandle,b,nby,i_err);
   else
     dskio_sioskwr_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_AQIO:
   dskio_aqskwr_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_CSTR:
   if(dh->offset==new_off)
     dskio_strwr_((FILE *)dh->iohandle,b,nby,i_err);
   else
     dskio_strskwr_((FILE *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_FDA:
   dskiof_fdawr_((int *) ufi,off,nby,b,i_err);
   break;
  }
 if(*i_err == 0) new_off = *off + *nby;
 else new_off= -1;
 dh->offset=new_off;
}

void dskio_xxwr_(int *ufi, char *b,int *nby,int *i_err)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);

  *i_err= -1;
  if(*nby <=0) { /* do nothing when no data */
   *i_err=0;
   return;
  }
  if(!dh) return;
  switch (dh->iotype)   {
  case DSKIO_CSYS:
   dskio_syswr_((int *)dh->iohandle,b,nby,i_err);
   break;
  case DSKIO_FFIO:
   dskio_ffwr_((int *)dh->iohandle,b,nby,i_err);
   break;
  case DSKIO_SIO:
   dskio_siowr_((int *)dh->iohandle,b,nby,i_err);
   break;
  case DSKIO_CSTR:
   dskio_strwr_((FILE *)dh->iohandle,b,nby,i_err);
   break;
  }
}

void dskio_xxrd_(int *ufi, char *b,int *nby,int *i_err)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);

  *i_err= -1;
  if(*nby <=0) { /* do nothing when no data */
    *i_err= 0;
    return;
  }
  if(!dh) return;
  switch (dh->iotype)   {
  case DSKIO_CSYS:
   dskio_sysrd_((int *)dh->iohandle,b,nby,i_err);
   break;
  case DSKIO_FFIO:
   dskio_ffrd_((int *)dh->iohandle,b,nby,i_err);
   break;
  case DSKIO_SIO:
   dskio_siord_((int *)dh->iohandle,b,nby,i_err);
   break;
  case DSKIO_CSTR:
   dskio_strrd_((FILE   *)dh->iohandle,b,nby,i_err);
   break;
  }
}

void dskio_xskrd_(int *iotype, int *ufi, char *b,long *off,int *nby,int *i_err)
{
 dskio_xxskrd_(ufi, b,off,nby,i_err);
}

void dskio_xxskrd_(int *ufi, char *b,long *off,int *nby,int *i_err)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 long new_off= *off;
 int  nwds;

  *i_err= -1;
  if(*nby <=0) { /* do nothing when no data */
    *i_err= 0;
    return;
  }
  if(!dh) return;
  switch (dh->iotype)   {
  case DSKIO_CSYS:
   if(dh->offset==new_off)
      dskio_sysrd_((int *)dh->iohandle,b,nby,i_err);
   else
      dskio_sysskrd_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_PAIO:
   if(dh->offset==new_off)
     dskio_pard_((int *)dh->iohandle,b,nby,i_err);
   else
     dskio_paskrd_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_FFIO:
   if(dh->offset==new_off)
     dskio_ffrd_((int *)dh->iohandle,b,nby,i_err);
   else
     dskio_ffskrd_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_SIO:
   if(dh->offset==new_off)
     dskio_siord_((int *)dh->iohandle,b,nby,i_err);
   else
     dskio_sioskrd_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_AQIO:
   dskio_aqskrd_((int *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_CSTR:
   if(dh->offset==new_off)
     dskio_strrd_((FILE *)dh->iohandle,b,nby,i_err);
   else
     dskio_strskrd_((FILE *)dh->iohandle,b,off,nby,i_err);
   break;
  case DSKIO_FDA:
     dskiof_fdard_((int *)ufi,off,nby,b,i_err);
   break;
  }
 
 if(*i_err == 0) new_off = *off + *nby;
 else new_off= -1;
 dh->offset=new_off;
}

FILE *dskio_strop_(char *fname,int *oflag,int *i_err,int *bsize)
{int opf= *oflag,j;
 FILE *fp=0;
 int bsiz = *bsize;
 *i_err= -1;
 if(!fname) return fp;
 if(strcmp(fname,"none")==0) return fp;
 if(strcmp(fname,"NONE")==0) return fp;

 if(opf & O_CREAT) {
   if(opf & O_EXCL) {
    j = access(fname,F_OK);
    if(j != -1) return fp;
   }
   fp = fopen(fname, "w+");
 } else {
  if(opf == O_RDONLY) {
    fp = fopen(fname, "r");
  } else {
    fp = fopen(fname, "r+");
  }
 }
 if(fp && bsiz>8192)
  setvbuf( fp, 0, _IOFBF, (size_t) bsiz);
 (*i_err) = (fp == 0) ? -1 : 0;
 return fp;
}

void dskio_strsk_(FILE *fp, long *off, int *i_err)
{*i_err= -1;
 if(!fp) { return; }
 *i_err = fseek((FILE *)fp,*off,SEEK_SET);
 *i_err = (*i_err == 0) ? 0 : -1;
 return;
}

void dskio_strwr_(FILE *fp, char *b, int *nby, int *i_err)
{ int nw= *nby;
 *i_err= -1;
 if(!fp || nw<=0) return;
 nw = (int) fwrite(b, (size_t) (*nby), 1, (FILE *) fp);
 (*i_err) = (nw != 1) ? -1 : 0;
 return;
}

void dskio_strrd_(FILE *fp, char *b, int *nby, int *i_err)
{int nr= *nby;
 *i_err= -1;
 if(!fp || nr<=0) return;
 nr = (int) fread(b, (size_t) (*nby), 1, (FILE *) fp);
 (*i_err) = (nr != 1) ? -1 : 0;
 return;
}

void dskio_strskrd_(FILE *fp, char *b,long *off, int *nby, int *i_err)
{*i_err= -1;
 if(!fp) { return; }
 *i_err = fseek((FILE *)fp,*off,SEEK_SET);
 (*i_err) = (*i_err == 0) ? 0 : -1;
 if(*i_err < 0) return;
 dskio_strrd_(fp, b, nby, i_err);
}

void dskio_strskwr_(FILE *fp, char *b,long *off, int *nby, int *i_err)
{*i_err= -1;
 if(!fp) { return; }
 *i_err = fseek((FILE *)fp,*off,SEEK_SET);
 (*i_err) = (*i_err == 0) ? 0 : -1;
 if(*i_err < 0) return;
 dskio_strwr_(fp, b, nby, i_err);
}

void dskio_sysop_(int *fd, char *fname,int *oflag,int *i_err)
{int opf= *oflag;
 *fd = -1;
 *i_err= -1;
 if(!fname) return;
 if(strcmp(fname,"none")==0) return;
 if(strcmp(fname,"NONE")==0) return;
 if(opf & O_CREAT)
  *fd = open(fname, opf ,PMODE);
 else
  *fd = open(fname, opf,PMODE);
(*i_err) = (*fd < 0) ? -1 : 0;
 return;
}

void dskio_syscl_(int *fd, int *i_err)
{ *i_err= -1;
 if(*fd <=0) return;
 *i_err = close(*fd);
 return;
}

void dskio_syswr_(int *fd, char *b, int *nby, int *i_err)
{int nw= *nby;
 *i_err= -1;
 if(*fd < 0 || nw<=0) return;
 nw = (int) write(*fd, b, (size_t) (*nby));
 (*i_err) = (nw == -1) ? -1 : 0;
 return;
}

void dskio_sysrd_(int *fd, char *b, int *nby, int *i_err)
{int nr= *nby;
 *i_err= -1;
 if(*fd < 0 || nr<=0) return;
 nr = (int) read(*fd, b, (size_t) (*nby));
 (*i_err) = (nr == -1) ? -1 : 0;
 return;
}

void dskio_syssk_(int *fd, long *off, int *i_err)
{off_t o = *off, pos;
 *i_err= -1;
 if(*fd < 0) { return; }
 pos = lseek(*fd,o,SEEK_SET);
 (*i_err) = (pos == -1) ? -1 : 0;
}

void dskio_sysskrd_(int *fd, char *b,long *off, int *nby, int *i_err)
{off_t o = *off, pos;
 *i_err= -1;
 if(*fd < 0) { return; }
 pos = lseek(*fd,o,SEEK_SET);
 (*i_err) = (pos == -1) ? -1 : 0;
 if(*i_err < 0) return;
 dskio_sysrd_(fd, b, nby, i_err);
}

void dskio_sysskwr_(int *fd, char *b,long *off, int *nby, int *i_err)
{off_t o = *off, pos;
 *i_err= -1;
 if(*fd < 0) { return; }
 pos = lseek(*fd,o,SEEK_SET);
 (*i_err) = (pos == -1) ? -1 : 0;
 if(*i_err < 0) return;
 dskio_syswr_(fd, b, nby, i_err);
}

void dskio_ffop_(int *fd, char *fname,int *oflag, int *i_err,
     int *psize, int *npages)
{ int opf = *oflag;
 /* char *str = "global.privpos.page_size=256.num_pages=12"; */
#ifdef CRAY
  struct ffsw stat;
  int page_size,num_pages;
#endif
  char str[256];
  if(strcmp(fname,"none")==0) return;
  if(strcmp(fname,"NONE")==0) return;

#ifdef CRAY
  page_size = *psize/4096;
  num_pages = *npages;
  sprintf(str,"global.privpos.page_size=%d.num_pages=%d",
  page_size,num_pages);
 /* printf("string to ffopens:\n    %s\n",str); */
  *fd = ffopens(fname, opf, PMODE, 0L, 0, &stat, str);
#else
  dskio_sysop_(fd,fname,oflag,i_err);
#endif
  *i_err = (*fd == -1) ? -1 : 0;
  return;
}

void dskio_ffskrd_(int *fd, char *b,long *off, int *nby, int *i_err)
{dskio_ffsk_(fd, off, i_err);
 if(*i_err < 0) return;
 dskio_ffrd_(fd, b, nby, i_err);
}

void dskio_ffskwr_(int *fd, char *b,long *off, int *nby, int *i_err)
{dskio_ffsk_(fd, off, i_err);
 if(*i_err < 0) return;
 dskio_ffwr_(fd, b, nby, i_err);
}

void dskio_ffrd_(int *fd, char *buff, int *nbuff, int *i_err)
{int nr;
 if(*fd < 0) { *i_err= -1; return; }
#ifdef CRAY
  nr =  (int) ffread(*fd, buff, (size_t) (*nbuff));
#else
  nr =  (int) read(*fd, buff, (size_t) (*nbuff));
#endif
  *i_err = (nr == -1) ? -1 : 0;
  return;
}

void dskio_ffwr_(int *fd, char *buff, int *nbuff, int *i_err)
{int nw;
 if(*fd < 0) { *i_err= -1; return; }
#ifdef CRAY
  nw = (int) ffwrite(*fd, buff, (size_t) (*nbuff) );
#else
  nw = (int) write(*fd, buff, (size_t) (*nbuff) );
#endif
  (*i_err) = (nw == -1) ? -1 : 0;
  return;
}

void  dskio_ffsk_(int *fd, long *off, int *i_err)
{int pos;
 off_t o = *off;
 if(*fd < 0) { *i_err= -1; return; }
#ifdef CRAY
  pos = (int) ffseek(*fd,o,SEEK_SET);
#else
  pos = (int) lseek(*fd,o,SEEK_SET);
#endif
  (*i_err) = (pos == -1) ? -1 : 0;
  return;
}

void  dskio_ffcl_(int *fd, int *i_err)
{*i_err= -1;
 if(*fd < 0) return;
#ifdef CRAY
  *i_err = ffclose( (*fd) );
#else
  *i_err = close(*fd);
#endif
  return;
}

void dskio_sioop_(int *fd, char *fname,int *oflag, int *i_err)
{int opf = *oflag;
 *fd = -1;
 *i_err= -1;
 if(!fname) return;
 if(strcmp(fname,"none")==0) return;
 if(strcmp(fname,"NONE")==0) return;

#ifdef _CRAYMPP
/*
 if(*oflag & O_CREAT)
  *fd = sio_open(fname,opf,PMODE);
 else
  *fd = sio_open(fname,opf);
  */
#else
  dskio_sysop_(fd,fname,oflag,i_err);
#endif
 (*i_err) = (*fd == -1) ? -1 : 0;
 return;
}

void dskio_siocl_(int *fd, int *i_err)
{*i_err= -1;
 if(*fd <=0) return;
#ifdef _CRAYMPP
 /*
 *i_err = sio_close(*fd);
 */
#else
 *i_err = close(*fd);
#endif
}

void dskio_sioskrd_(int *fd, char *b,long *off, int *nby, int *i_err)
{dskio_siosk_(fd, off, i_err);
 if(*i_err < 0) return;
 dskio_siord_(fd, b, nby, i_err);
}

void dskio_sioskwr_(int *fd, char *b,long *off, int *nby, int *i_err)
{dskio_siosk_(fd, off, i_err);
 if(*i_err < 0) return;
 dskio_siowr_(fd, b, nby, i_err);
}

void dskio_siowr_(int *fd, char *b, int *nby, int *i_err)
{int nw= *nby;
 *i_err= -1;
 if(*fd < 0 || nw<=0) return;
#ifdef _CRAYMPP
 /* nw = (int) sio_write(*fd, b,(size_t) (*nby)); */
#else
 nw = (int) write(*fd, b, (size_t) (*nby));
#endif
 (*i_err) = (nw == -1) ? -1 : 0;
 return;
}

void dskio_siord_(int *fd, char *b, int *nby, int *i_err)
{int nr= *nby;
 *i_err= -1;
 if(*fd < 0 || nr<=0) return;
#ifdef _CRAYMPP
 /* nr = (int) sio_read(*fd, b, (size_t) (*nby)); */
#else
 nr = (int) read(*fd, b, (size_t) (*nby));
#endif
 (*i_err) = (nr == -1) ? -1 : 0;
 return;
}

void dskio_siosk_(int *fd, long *off, int *i_err)
{off_t o = *off, pos;
 *i_err= -1;
 if(*fd < 0) { return; }
#ifdef _CRAYMPP
  /*pos = sio_lseek(*fd,o,SEEK_SET); */
  pos= -1;
#else
  pos = lseek(*fd,o,SEEK_SET);
#endif
  (*i_err) = (pos == -1) ? -1 : 0;
  return;
}

void dskio_aqop_(int *iaq, char *fname,int *oflag, int *i_err)
{
 char name[32],path[120],cmd[80];
 int i,n=56;
 if(!fname) *i_err=1;
 if(strcmp(fname,"none")==0) return;
 if(strcmp(fname,"NONE")==0) return;

#ifdef CRAY
  if(strlen(fname)> 8) {/* must call from fortran */
    DSKIOF_AQOP(iaq,fname,i_err);
  } else {
    AQOPEN(iaq,&n,fname,i_err);
  }
#else
  dskio_sysop_(iaq,fname,oflag,i_err);
#endif
 /*
  (*i_err) = (*i_err < 0) ? -1 : 0;
 */
}

void dskio_aqcl_(int *iaq, int *i_err)
{*i_err= -1;
#ifdef CRAY
   AQCLOSE(iaq,i_err);
   *i_err=0;
#else
   if(iaq[0]<1) return;
   *i_err = close(iaq[0]);/*1st word of iaq=fd */
#endif
}

void  dskio_aqwt_(int *fd, int *i_err)
{
 *i_err=0;
#ifdef CRAY
  AQWAIT(fd,i_err);
#endif
}


void  dskio_aqsk_(int *fd, long *off, int *i_err)
{int pos;
 off_t o = *off;
 if(*fd < 0) { *i_err= -1; return; }
#ifdef CRAY
  pos = *off;  /* irrelevant for aqio */
#else
  pos = (int) lseek(*fd,o,SEEK_SET);
#endif
  (*i_err) = (pos == -1) ? -1 : 0;
  return;
}

void  dskio_a_aqskwr_(int *iaq, char *b, long *off, int *nb, int *i_err)
{int nby;
#ifdef CRAY
       int id, req=0; /*off is sector position from 0 */
       int p;
       p = *off/(4096);
       nby = *nb/(4096);
       AQWRITE(iaq,b,&p,&nby,&id,&req,i_err);
#else
       int  nw;
       *i_err=0;
       nby = *nb;
       nw = dskiowr_(iaq, off, &nby, (char *)b);
       (*i_err) = (nw<=0) ? -1 : 0;
#endif
}

void dskio_aqskwr_(int *iaq,char *b,long *off, int *nb, int *ierr)
{int nby;
#ifdef CRAY
       int id, req=0; /*off is sector position from 0 */
       int p;
       p = *off/(4096);
       nby = *nb/(4096);
       AQWRITE(iaq,b,&p,&nby,&id,&req,ierr);
       AQWAIT(iaq,ierr);
#else
       int nw;
       *ierr=0;
       nby = *nb;
       nw = dskiowr_(iaq, off, &nby, (char *)b);
       (*ierr) = (nw<=0) ? -1 : 0;
#endif
}

void dskio_a_aqskrd_(int *iaq,char *b,long *off, int *nb, int *ierr)
{
#ifdef CRAY
       int id, req=0; /*off is sector position from 0 */
       int p,nby;
       p = *off/(4096);
       nby = *nb/(4096);
       AQREAD(iaq,b,&p,&nby,&id,&req,ierr);
#else
       int nby,nr;
       *ierr=0;
       nby= *nb;
       nr = dskiord_(iaq, off, &nby, (char *)b);
       (*ierr) = (nr<=0) ? -1 : 0;
#endif
}

void dskio_aqskrd_(int *iaq,char *b,long *off, int *nb, int *ierr)
{
#ifdef CRAY
       int id, req=0; /*off is sector position from 0 */
       int p,nby;
       p = *off/(4096);
       nby = *nb/(4096);
       AQREAD(iaq,b,&p,&nby,&id,&req,ierr);
       AQWAIT(iaq,ierr);
#else
       int nby,nr;
       *ierr=0;
       nby = *nb;
       nr = dskiord_(iaq, off, &nby, (char *)b);
       (*ierr) = (nr<=0) ? -1 : 0;
#endif
}

void dskiocop_(int   *fd, char *file, int  *imode)
{
  int  imd;
  imd=(*imode);  /* imode=0 read, 2 update, 3 new */
  if (imd==3)
     (*fd)=open(file, O_CREAT|O_WRONLY|O_EXCL ,PMODE);
  else if (imd==4)
     (*fd)=open(file, O_CREAT|O_RDWR|O_EXCL ,PMODE);
  else if(imd== -4)
     (*fd)=open(file, O_CREAT|O_RDWR ,PMODE);
  else
     (*fd)=open(file,imd,PMODE);
}

void dskioccl_(int  *fd)
{ int ierr;
  if(*fd<0) return;
  ierr = close(*fd);
}

void dskiocrd_(int  *fd,char *buff,int  *np,int  *irec,int  *ierr)
{int  nr,n=(*np);
  if(lseek(*fd,512*((*irec)-1),0)<0) {*ierr=0; return; };
  nr=read(*fd,buff,n);
  if(nr==n) (*ierr)=1; else (*ierr)=0;
}

void dskiocwr_(int  *fd,char *buff,int  *np,int  *irec,int  *ierr)
{int  nw,n=(*np);
  if(lseek(*fd,512*((*irec)-1),0)<0) {*ierr=0; return; };
  nw=write(*fd,buff,n);
  if(nw==n) (*ierr)=1; else (*ierr)=0;
}

int dskiowr_(int  *fd, long *off, int  *nby, char *buf)
{int nw,nb= *nby;
 off_t o = *off;
 if(!fd || !buf) return 0;
 if(*fd<0) return 0;
 lseek(*fd, o,0);
 nw = write(*fd,buf,nb);
 return nw;
}

int dskiord_(int  *fd, long *off, int  *nby, char *buf)
{int nr, nb= *nby;
 off_t o = *off;
 if(!fd || !buf) return 0;
 if(*fd<0) return 0;
 lseek(*fd, o,0);
 nr = read(*fd,buf,nb);
 return nr;
}

int dskio_exist_(char *fname)
{
  return access(fname,F_OK);
}

int dskiorm_(char *name)
{/* a wrapper around remove for fortran, 0=OK */
 int i=2;
 if(!name) return i;
 if(strcmp(name,"NONE")==0) return i;
 if(strcmp(name,"none")==0) return i;
 i=remove(name);
 return i;
}

int dskio_enrw_()
{ return O_CREAT|O_RDWR|O_EXCL; }
int dskio_nrwt_()
{ return O_CREAT|O_RDWR|O_TRUNC; }
int dskio_nrw_()
{ return O_CREAT|O_RDWR; }
int dskio_nwro_()
{ return O_CREAT|O_WRONLY; }
int dskio_orw_()
{ return O_RDWR; }
int dskio_ordo_()
{ return O_RDONLY; }
int dskio_owro_()
{ return O_WRONLY; }

/*
 * converts old style open flag to our more current
 * flag used by xop calls.
 */
int dskio_convert_flag(int op_stat)
{int opf;
 switch (op_stat) {
   case -4:
     opf=dskio_nrwt_();
   break;
   case 0:
     opf=dskio_ordo_();
   break;
   case 1:
     opf=dskio_orw_();
   break;
   case 2:
     opf=dskio_orw_();
   break;
   case 3:
   case 4:
     opf=dskio_enrw_();
   break;
  default:
     opf=dskio_ordo_();
 }
return opf;
}

char *dskio_parse_file( char *file, char *name, char *path )
{/* path may contain node and userid info */
 char *loc1=NULL;
 char *ret=NULL;

 path[0]='\0';
 name[0]='\0';
 if(file == NULL || strlen(file)==0) { return ret; }

 loc1 = strrchr( file, ']' );           /* vms  */
 if(!loc1) loc1 = strrchr( file, '>' ); /* vms  */
 if(!loc1) loc1 = strrchr( file, '/' ); /* unix */
 if(!loc1) loc1 = strrchr( file, '\\' );/* pcs  */
 if(!loc1) { /* node::userid;;file */
  loc1 = strstr( file, ";;" );
  if(loc1) loc1++;
 }
 if(!loc1) { /* node::userid;;file */
  loc1 = strstr( file, "::" );
  if(loc1) loc1++;
 }
 if(!loc1) { /* userid@node:file */
  loc1 = strstr( file, ":" );
 }

 strcpy(name,file);
 if (loc1)
  { strcpy( name,++loc1);
    strncpy(path,file,loc1-file);
    path[loc1-file]='\0';
    ret= name;
  }
 return (ret);
}

/*
 * look for embedded userid or node name in file name
 * 2 styles are supported
 * fname = node::userid;;file  (user1d;; is optional)
 * fname = userid@node:file    (userid@ is optional )
 */
int dskio_userid_(char *fname,char *userid)
{char *loc1=0,*loc2=0;
 userid[0]='\0';
 if(!fname) return 0;
 loc1 = strstr(fname,"::");
 if(loc1) loc1 += 2;
 else loc1=fname;
 loc2=strstr(fname,";;");
 if(loc2) {
   strncpy(userid,loc1,loc2-loc1);
   userid[loc2-loc1]='\0';
   return loc2-loc1;
 }

 loc2=strstr(fname,"@");
 loc1=fname;
 if(loc2) {
  if(loc2-fname+1 < strlen(fname)) {
    strncpy(userid,fname,loc2-loc1);
    userid[loc2-loc1]='\0';
    return loc2-loc1;
  }
 }
 return 0;
}

int dskio_node_(char *fname,char *node)
{char *loc1=0,*loc2=0,*loc3=0;
 node[0]='\0';
 if(!fname) return 0;
 loc1 = strstr(fname,"::");
 if(loc1) {
   strncpy(node,fname,loc1-fname);
   node[loc1-fname]='\0';
   return loc1-fname;
 }

 loc2=strstr(fname,":");
 loc1=strstr(fname,"@");
 if(loc2) {
   if(loc1) {
    strncpy(node,loc1+1,loc2-loc1-1);
    node[loc2-loc1-1]='\0';
    return loc2-loc1-1;
   } else {
    loc3=strstr(fname,"[");
    if(loc3) return 0; /* a vms syntax? */
    strncpy(node,fname,loc2-fname);
    node[loc2-fname]='\0';
    return loc2-fname;
   }
 }
 return 0;
}

int dskio_rmquote(char *name)
{
 int i,j,n;
 n = strlen(name);
 j=0;
 for (i=0;i<n;i++) {
   if(name[i] != 34 && name[i] != 39) {
     name[j]=name[i]; j++;
   }
 }
 name[j]=0;
 return strlen(name);
}

int dskio_file_(char *fname, char *file)
{char *loc1=0,*loc2=0,*loc3=0;
 int  i;
 file[0]='\0';
 if(!fname) return 0;
 if(strlen(fname)==0) return 0;
 loc1 = strstr(fname,"::");
 if(loc1)  loc3=loc1+2;
 loc1 = strstr(fname,";;");
 if(loc1)  loc3=loc1+2;
 if(loc3) {
   strcpy(file,loc3);
   return dskio_rmquote(file); /*strlen(file); */
 }

 loc1=strstr(fname,"@");
 loc2=strstr(fname,":");
 if(loc2) {
   if(loc1) {
    strcpy(file,loc2+1);
    return dskio_rmquote(file); /*strlen(file); */
   }
   if(loc2[1]=='[') loc3=fname;
   else loc3=loc2+1;
   strcpy(file,loc3);
   return dskio_rmquote(file); /*strlen(file); */
 }

 if(fname != file) strcpy(file,fname);
 return dskio_rmquote(file); /*strlen(file); */
}


int dskio_bare_file_(char *fname, char *bfile)
{char *loc1=0;
 char tmp[192];
 int i;
 bfile[0]='\0';
 if(!fname) return 0;
 if(strlen(fname)==0) return 0;
 i=dskio_file_(fname,tmp); /* tmp has node,user info removed */

 if(i>0) {
   loc1 = strrchr( tmp, ']' );
   if(!loc1) loc1 = strrchr( tmp, '>' );
   if(!loc1) loc1 = strrchr( tmp, '/' );
   if(!loc1) loc1 = strrchr( tmp, '\\' );
   if (loc1) {
      loc1++;
      strcpy(bfile,loc1);
   } else {
    strcpy(bfile,tmp);
   }
 }
 return strlen(bfile);
}

int dskio_path_(char *fname, char *path)
{char *loc1=0;
 char p[192];
 path[0]='\0';
 if(!fname) return 0;
 /* remove node and userid info*/
 if(dskio_file_(fname,p)==0) return 0;
 loc1 = strrchr( p, ']' );
 if(!loc1) loc1 = strrchr( p, '>' );
 if(!loc1) loc1 = strrchr( p, '/' );
 if(!loc1) loc1 = strrchr( p, '\\' );
 if (loc1) {
    loc1++;
    strncpy(path,p,loc1-p);
    path[loc1-p]='\0';
  }
 return strlen(path);
}

int dskio_ptmp_(char *ptmp)
{char *loc1=0,*home=0, tmp[120];
 int i;
 ptmp[0]='\0';
 loc1 = getenv("PTMPDIR");
 if(loc1) { strcpy(ptmp,loc1); return strlen(ptmp); }

 home = getenv("HOME");
 if(home) {
#if VMS
  strcpy(tmp,home);
#else
  strcpy(tmp,home);
  strcat(tmp,"/ptmp");
#endif
  i = access(tmp,F_OK);
  if(i==0) strcpy(ptmp,tmp);
 }
 return strlen(ptmp);
}

/*
 * Find the extension of a file name
 * name... Input name.
 * ext ... Extension of name not including the dot.
 */
int dskio_ext_(char *name, char *ext)
{char *dot, *semic, bfile[88];
 int  i;
 ext[0]='\0';
 if(!name) return 0;
 if(dskio_bare_file_(name,bfile) <=0 ) return 0;
 dot = strrchr(bfile,'.');
 if(dot) {
   ++dot;
    strcpy( ext,dot);
    semic = strrchr(ext,';');
    if(semic) { semic[0]='\0'; }
 }

return strlen(ext);
}

/* prepend a non volatile path onto fname
   if there is no existing path information
 */
int dskio_nonvol_(char *fname, char *nvname, int *honor)
{char tmp[120],nvpath[120];
 char token=057; /* slash */
 int  i,j, hon= *honor;
#ifdef VMS
 token=0135; /* square close bracket */
#endif
 nvpath[0]='\0';
 nvname[0]='\0';
 i = dskio_path_(fname, nvpath);
 if(i!=0 && hon==1) { /* honor explicit path names */
  return dskio_file_(fname, nvname);
 }
 i = dskio_ptmp_(nvpath);
 strcpy(nvname,nvpath);
 j = dskio_bare_file_(fname, tmp);
 if(j==0) return j;
 strcpy(nvname,nvpath);
 if(nvname[i-1] != token) {
  nvname[i]=token;
  nvname[i+1]='\0';
 }
 strcat(nvname,tmp);
 return strlen(nvname);
}

int  dskio_netname_(char *fname,char *fullname)
{char rnode[64],ruser[64],rfile[160],bare[160],rpath[160];
 char ninfo[16];
 int i;
 fullname[0]='\0';
 strcpy(ninfo,"_netinfo");
 i = dskio_parse_fullx1_(fname, ninfo,
      rnode, ruser, rfile, bare, fullname);
 i = dskio_path_(rfile, rpath);
 
 if(rnode[0]==0) i=dskio_host_(rnode);
 if(ruser[0]==0) i=dskio_envuser_(ruser);
 if(rpath[0]==0) i=dskio_cwd_(rpath);
 if(rpath[0]!=0) {
   i = strlen(rpath);
   if(rpath[i-1]=='/' || rpath[i-1]==']')
    sprintf(rfile,"%s%s",rpath,bare);
   else
#ifdef VMS
    sprintf(rfile,"%s]%s",rpath,bare);
#else
    sprintf(rfile,"%s/%s",rpath,bare);
#endif
 }
 if(strlen(rnode)==0)
   sprintf(fullname,"%s",rfile);
 else {
   if(strcmp(rnode,"pogun")==0) strcpy(rnode,"POGUN");
   if(strlen(ruser)==0)
     sprintf(fullname,"%s::%s",rnode,rfile);
   else
     sprintf(fullname,"%s@%s:\"%s\"",ruser,rnode,rfile);
 }
 return strlen(fullname);
}

/* given the components, build a full name */
void dskio_bld_full_(char *full, char *u, char *n,char *p, char *bf)
{int i;
 full[0]='\0';
 if(strlen(u) > 0 && strcmp(u,"NONE")!=0) {
   strcpy(full,u);
   strcat(full,"@");
   if(strlen(n)==0) strcat(full,"NONE:");
 }
 if(strlen(n) > 0 && strcmp(n,"NONE")!=0) {
   strcat(full,n);
   strcat(full,":");
 }
 if(strlen(p)> 0) {
   strcat(full,p);
   i = strlen(full);
   if(full[i-1] == ']') { strcat(full,bf); return; }
   if(full[i-1] != '/') strcat(full,"/");
 } else {
   strcat(full,bf);
   return;
 }
 strcat(full,bf);
 return;
}

void dskio_cppath(char *hfile, char *file)
{ char hpath[96],hname[80],path[96],name[80];
/********************************************
 *--hfile.... a template file name.       ***
 *--file..... file name to expand.        ***
 *--Set the path for file= path for hfile,***
 *--unless it has an explicit path already***
 *******************************************/
 if(hfile==NULL || file==NULL) return;
 if((strlen(file)==0) || file[0]==' ') {
   strcpy(file,hfile);
   return;
 }
 if(strlen(hfile)==0 ) return;
 if(strcmp(file,"SAME")==0) {
   strcpy(file,hfile);
   return;
 }

 dskio_parse_file(hfile,hname,hpath);
 dskio_parse_file( file, name, path);
 if(strlen(path) > 0) return; /* keep old path */
 else strcpy(path,hpath);    /* adopt headers path */

 if(strcmp(name,"NONE") == 0) {
   strcpy(file,name);
   return;
 } else {
   sprintf(file,"%s%s",path,name);
   return;
 }

}


/* given the full name, determine the components */
void dskio_parse_full_(char *full, char *u, char *n,char *p, char *bf)
{int i;
 u[0]='\0';
 u[0]='\0';
 p[0]='\0';
 bf[0]='\0';
 if(strlen(full) == 0) return;
 i =dskio_node_(full,n);
 i =dskio_userid_(full,u);
 i =dskio_path_(full,p);
 i =dskio_bare_file_(full,bf);
 return;
}

/* Determine whether a file exists locally or not.
 * File is local if no explicit node name is found or if
 * the node name is equal to the current host name
 * return value = 1/0 for local/non-local
 * Set must_exist = 1 to verify the existence of
 * a local file
 */
int  dskio_cwd_(char *cwd)
{char *val,buf[196];
 cwd[0]='\0';
 buf[0]='\0';
 val = getcwd(buf,196);
 if(val) strcpy(cwd,buf);
 return strlen(cwd);
}

int  dskio_envuser_(char *envuser)
{char *val;
 envuser[0]='\0';
 val = getenv("USER");
 if(!val) val = getenv("UNAME"); /* for VMS fortran main? */
 if(val) strcpy(envuser,val);
 if( (val=strstr(envuser," "))!=0) val[0]='\0';
 return strlen(envuser);
}

int  dskio_host_(char * host)
{char *val,h[80];
 int  i,namelen=80;
 host[0]='\0';
 h[0]='\0';
#ifdef VMS
     val = getenv("HOST");
     if(val) { strcpy(h,val);}
#else
     i=gethostname(h,namelen);
#endif
 strcpy(host,h);
 return strlen(host);
}

int dskio_is_local_(char *fname, int *must_exist)
{int  i,ln,lf,local_node=0,namelen=80;
 char node[80],host[80],file[120],*val;
 if(strlen(fname) == 0) return local_node;
 ln =dskio_node_(fname,node);
 lf =dskio_file_(fname,file);
 if(ln>0) { /* possible remote file */
   if(strcmp(node,"NONE")==0) {
     local_node=1;
   } else {
     i = dskio_host_(host);
     if(i>=0)  {
      if(strcmp(node,"pogun")==0) strcpy(node,"POGUN");
      if(strcmp(node,host)==0) {
        /*printf("host is the same: %s\n",node); */
        local_node=1;
      }
     }
   }
 } else local_node=1;

 if(local_node==1 && *must_exist==1) { /* file resides locally? */
   if(access(file,F_OK) <0) {
     /* printf("did not find it: %s\n",file); */
    local_node=0;
   }
 }

 return local_node;
}

int dskio_netinfo_(char *netinfo, char *node,char *user,char *path)
{
/********************************************
 * Reads the file netinfo that is prepared **
 * by the job script for jobs submitted via**
 * CPS. Returns OK=1  or NOTOK=0           **
 *     user node path                      **
 ********************************************/
   FILE  *filePtr = NULL;
   char  _node[24],_user[24],_path[96];
   char  file[96];
   int   i,OK=1,NOTOK=0;

   _node[0]='\0';
    node[0]='\0';
   _user[0]='\0';
    user[0]='\0';
   _path[0]='\0';
    path[0]='\0';

   if(netinfo[0]=='\0') return NOTOK;
   strcpy(file,netinfo);
   if( (filePtr = fopen (file, "r")) == 0) {
      return NOTOK;
   }

   i = fscanf(filePtr,"%s%s%s",_user,_node,_path);
   if(i== EOF) { if(filePtr) fclose(filePtr); return NOTOK; }

   if(strcmp(_node,"NONE")==0)  _node[0]='\0';
   if(strcmp(_node,"LOCAL")==0) _node[0]='\0';
   strcpy(node,_node);
   if( i>1) strcpy(user,_user);
   if( i>2) strcpy(path,_path);

   if(filePtr) fclose(filePtr);
   return OK;
}

/* Associate hostnames with preferred path prefixes */
/* <VERIFICATION_CODE>
int main(int argc, char **argv) {
 int i,j;
 char host[256],disk[64],*s,*tok;
 i = dskio_host_(host);
 if(i==0) exit(1);
 i = dskio_host_to_(host,disk);
 if(i> 0 ) printf("host=%s disk=%s\n",host,disk);
 strcpy(disk,"/ptmp/poeplx05/scratch");
 i = dskio_to_host_(host,disk);
 s=host;
 if(i>0) {
  for(j=0;j<i;j++) {
   tok = strtok(s," ");
   s += strlen(tok)+1;
   if(tok){
   printf("disk=%s host=%s\n",disk,tok);
   }
   else
   printf("null token\n");
  }
 }
 exit(0);
}
</VERIFICATION_CODE> */

int dskio_host_to_optpath_(char *host, char *disk, char *torp,
    int *nblk, int *byperblk)
{int i, tlim=2000000000, maxblks,bypblk= *byperblk;
 char ext[4];
 char sw='P';
 if(torp[0]=='T') sw='T';
 if(torp[0]=='B') sw='B';
 if(bypblk<=0) bypblk=1;
 maxblks = tlim/(bypblk);
 if(*nblk > maxblks) sw='P';
 if(strcmp(host,"poeplx01")==0) sw='P';
 if(strcmp(host,"poeplx02")==0) sw='P';
 if(strcmp(host,"poeplx03")==0) sw='P';
 if(strcmp(host,"poeplx04")==0) sw='P';
 if(sw=='B') sw='T';
  ext[0]='\0';
#ifndef VMS
 strcpy(ext,"/");
#endif
 disk[0]='\0';
 if(sw=='P') {
   i = dskio_host_to_(host, disk);
   if(i> 0) strcat(disk,ext);
 }
 if(sw=='T') {
   if(strstr(host,"poeplx") !=0) {
     sprintf(disk,"/tmp/%s/",host);
   } 
 }
 return strlen(disk);
}

int dskio_host_to_(char *host, char *disk)
{int npe,i;
 char *s;
 
 disk[0]='\0';
 npe = sizeof(dtable)/sizeof(dtable[0]);
 
 for(i=0;i<npe;i++) {
   if(strcmp(dtable[i].host,host)==0) {
    strcpy(disk,dtable[i].disk);
    return strlen(dtable[i].disk);
   }
 }
 
 return 0;
}
 
int dskio_to_host_(char *host, char *disk)
{int npe,i,cnt;
 char *s;
 cnt=0;
 host[0]='\0';
 npe = sizeof(dtable)/sizeof(dtable[0]);
 s = host;
 for(i=0;i<npe;i++) {
   if(strstr(dtable[i].disk,disk)!=0) {
     cnt++;
     strcat(host,dtable[i].host);
     strcat(host," ");
   }
 }
 return cnt;
}


/* Purpose: An extension to dskio_parse_full which determines
 *          node, user, path information from a file name,fname.
 *          Honors explicit information but also check a file.
 * Precedence of node, user, path sources:
 * 1 -   Explicit node, user, path information
 * 2 -   Data from file name passed in ninfo
 * set ninfo=\0 to bypass information in ninfo
 *              (the name ninfo='_netinfo' is a CPS standard )
 */
int dskio_parse_fullx1_(char *fname, char *ninfo,
      char *rnode, char *ruser, char *rfile, char *bare,
      char *modified)
{int  i;
 char rpath[120];
 char nnode[64],nuser[64],npath[120]; /* network info */
 
 rnode[0]='\0';
 ruser[0]='\0';
 rfile[0]='\0';
 bare[0] ='\0';
 modified[0] ='\0';
 strcpy(modified,fname);
 dskio_parse_full_(fname, ruser, rnode, rpath, bare);
 dskio_file_(fname, rfile);
 i = dskio_netinfo_( ninfo,nnode,nuser,npath);
 if(rpath[0]==0) strcpy(rpath,npath);
 if(i == 1)  { /* found netinfo data */
   if(rnode[0] == 0) strcpy(rnode,nnode);
   if(ruser[0] == 0) strcpy(ruser,nuser);
   if(rpath[0] == 0) strcpy(rpath,npath);
   if(rpath[0] !=0) {
     i = strlen(rpath);
     if(rpath[i-1]=='/' || rpath[i-1]==']')
      sprintf(rfile,"%s%s",rpath,bare);
     else
#ifdef VMS
      sprintf(rfile,"%s]%s",rpath,bare);
#else
      sprintf(rfile,"%s/%s",rpath,bare);
#endif
   }
 }
 if(strlen(rnode)==0)
   sprintf(modified,"%s",rfile);
 else {
   if(strlen(ruser)==0)
     sprintf(modified,"%s::%s",rnode,rfile);
   else
     sprintf(modified,"%s@%s:\"%s\"",ruser,rnode,rfile);
 }
 return strlen(modified);
}

int dskio_location_(char *node, char *symb_path, char *true_path)
{char spath[64], *val;
 int i;
 true_path[0]='\0';
 if(strlen(symb_path) ==0) return 0;
 if(strlen(symb_path) >63) return 0;
 strcpy(spath,symb_path);
 for(i=0;i<strlen(spath);i++) toupper(spath[i]);

 if(strlen(node)==0 || strcmp(node,"NONE")==0) {
   if(strcmp(spath,"HOME")==0) {
     val = getenv("HOME");
     if(val) strcpy(true_path,val);
     return strlen(true_path);
   }
   if(strcmp(spath,"PTMP")==0) {
     i = dskio_ptmp_(true_path);
     return strlen(true_path);
   }
   if(strcmp(spath,"TMP")==0) {
     val = getenv("HOME");
     if(val) strcpy(true_path,val);
     strcat(true_path,"/tmp/");
     return strlen(true_path);
   }
   if(strcmp(spath,"LOCAL")==0) {
     val = getenv("PWD");
     if(val) strcpy(true_path,val);
     i = dskio_ptmp_(true_path);
     return strlen(true_path);
   }
 } else {
   if(strcmp(spath,"HOME")==0) {
     strcpy(true_path,"./");
     return strlen(true_path);
   }
   if(strcmp(spath,"PTMP")==0) {
     strcpy(true_path,"ptmp/");
     return strlen(true_path);
   }
   if(strcmp(spath,"TMP")==0) {
     strcpy(true_path,"./tmp/");
     return strlen(true_path);
   }
   if(strcmp(spath,"LOCAL")==0) {/*bad choice */
     return strlen(true_path);
   }
 }

 return strlen(true_path);
}


/**********************************************
 * Methods for DskioChain                   ***
 *********************************************/
int DskioChainCheck(DskioChain *hc,DskioHandle *dh)
{DskioHandle *h=DskioChainFirst(hc);
 if(!dh) return 0;

 while(h) {
  if(h==dh) return 1;
  h = DskioHandleNext(h);
 }
 return 0;
}

DskioHandle *DskioChainFromIndex(DskioChain *hc,int index)
{ DskioHandle *h;
  int index_h;
  if(!hc) return 0;
  h = DskioChainFirst(hc);
  while(h) {
    index_h = DskioHandleIndex(h);
    if(index == index_h) return h;
    h = DskioHandleNext(h);
  }
 return 0;
}

DskioHandle *DskioChainFromHandle(DskioChain *hc,void *handle)
{ DskioHandle *h;
  void *handle_h;
  h = DskioChainFirst(hc);
  while(h) {
    handle_h = DskioHandleIOhandle(h);
    if(handle == handle_h) return h;
    h = DskioHandleNext(h);
  }
 return 0;
}

DskioHandle *DskioChainFromName(DskioChain *hc,char *name)
{ DskioHandle *h;
  char *fname=0;
  if(!name) return 0;
  h = DskioChainFirst(hc);
  while(h) {
    fname =DskioHandleFname(h);
    if(fname) {
     if(strcmp(fname,name)==0) return h;
    }
    h = DskioHandleNext(h);
  }
 return 0;
}

/* add to list and assign a UFI */
void DskioChain_add(DskioChain *hc, DskioHandle *h)
{/* Add process on to end of list */
 if(hc == 0) return;
 if(h  == 0) return;
 DskioHandleSIndex(DskioHandleNewIndex(hc),h);
 if(hc->nlink == 0) {
    hc->first = h;
    hc->last  = h;
    DskioHandleSPrev(h,0);
    DskioHandleSNext(h,0);
    hc->nlink = 1;
    return;
 }
 if(hc->last){
    hc->last->next = h;
    if(h) {
       DskioHandleSPrev(h,hc->last);
       DskioHandleSNext(h,0);
       hc->last  = h;
    }
    hc->nlink += 1;
 }
 return;
}

void DskioChain_rm(DskioChain *hc, DskioHandle *h)
{DskioHandle *hlnk,*prev,*next;
 if( hc == NULL | h == NULL) return;
 hlnk = DskioChainFirst(hc);
 if(!hlnk) return;
 while(h != hlnk) hlnk = DskioHandleNext(hlnk);
 if(hlnk == h) {
    prev = DskioHandlePrev(hlnk);
    next = DskioHandleNext(hlnk);
    if(prev)
     DskioHandleSNext(prev,next);
    else
     hc->first = next;
    if(next)
     DskioHandleSPrev(next,prev);
    else
     hc->last=prev;
    hc->nlink--;
    if(hc->nlink<0) hc->nlink = 0;
 }
}

int dskio_chain_iotype_(int *ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 return DskioHandleIOtype(dh);
}
int dskio_chain_handle_(int *ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 int io= -1;
 if(DskioHandleIOtype(dh) != DSKIO_CSTR)
   io = *(int *) DskioHandleIOhandle(dh);
 return io;
}
int dskio_chain_recl_(int *ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 return DskioHandleRecl(dh);
}
void dskio_chain_srecl_(int *recl, int *ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 DskioHandleSRecl(*recl,dh);
}
int dskio_chain_oflag_(int *ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 return DskioHandleOflag(dh);
}
void dskio_chain_fname_(int *ufi, char *fname)
{char *fn;
 DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 fn = DskioHandleFname(dh);
 fname[0]='\0';
 if(fn) strcpy(fname,fn);
 return;
}
void *dskio_chain_file_data_(int *ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 return DskioHandleFileData(dh);
}
void dskio_chain_sfile_data_(void *file_data, int *ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, *ufi);
 DskioHandleSFileData(dh,file_data);
}

int DskioChainIOtype(int ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, ufi);
 return DskioHandleIOtype(dh);
}

void DskioChainSIOtype(int iotype, int ufi)
{DskioHandle *dh=DskioChainFromIndex(&hchain, ufi);
 DskioHandleSIOtype(iotype, dh);
}

DskioHandle *DskioChainFirst(DskioChain *hc)
{if(!hc) return 0;
 return hc->first;
}

void DskioChainSFirst(DskioChain *hc, DskioHandle *first)
{if(!hc) return;
 hc->first=first;
}

DskioHandle *DskioChainLast(DskioChain *hc)
{if(!hc) return 0;
 return hc->last;
}

void DskioChainSLast(DskioChain *hc, DskioHandle *h)
{if(!hc) return;
 hc->last=h;
}

int DskioChainNlink(DskioChain *hc)
{if(!hc) return 0;
 return hc->nlink;
}

void DskioChainSNlink(DskioChain *hc, int nlink)
{if(!hc) return;
 hc->nlink= nlink;
}

/**********************************************
 * The following methods are for DskioHandle***
 *********************************************/
DskioHandle *DskioHandleNew(int opt,int oflag, char *fname)
{DskioHandle *dh=0;

 if(opt < DSKIO_CSYS) return 0;
 if(opt > DSKIO_FDA) return 0;
 dh = malloc(sizeof(DskioHandle));
 if(!dh) return 0;
 if(!fname) {
   dh->fname[0]='\0';
 } else {
   if(strlen(fname) > 255) {
    strncpy(dh->fname,fname,255);
    dh->fname[255]='\0';
   } else {
    strcpy(dh->fname,fname);
   }
 }
 dh->iotype= opt;
  switch (opt) {
  case DSKIO_CSYS:
   dh->iohandle = (void *) malloc(sizeof(int));
   break;
  case DSKIO_FFIO:
   dh->iohandle = (void *) malloc(sizeof(int));
   break;
  case DSKIO_SIO:
   dh->iohandle = (void *) malloc(sizeof(int));
   break;
  case DSKIO_AQIO:
   dh->iohandle = (void *) malloc(56*sizeof(int));
   break;
  case DSKIO_CSTR:
   dh->iohandle =0;
   break;
  case DSKIO_FDA:
   dh->iohandle = (void *) malloc(sizeof(int));
   break;
  default:
   opt=DSKIO_CSTR;
   dh->iohandle =0;
  }
 dh->index= -1;
 dh->next=0;
 dh->prev=0;
 dh->oflag=oflag;
 dh->recl=0;
 dh->offset=0;
 dh->file_data=0;
 return dh;
}

void DskioHandleDel(DskioHandle *dh)
{if(!dh) return;

 dh->fname[0]='\0';
 if(dh->iotype!=DSKIO_CSTR) {
  if(dh->iohandle) free(dh->iohandle);
 }
 dh->index= -1;
 dh->file_data=0;
 free(dh);

}

DskioHandle *DskioHandleNext(DskioHandle *h)
{/* return the next link in the linked list */
  if(h == NULL) return (DskioHandle *) NULL;
  return (DskioHandle *) h->next;
}

void DskioHandleSNext(DskioHandle *O, DskioHandle *n)
{/* return the previous link in the linked list */
  if(!O) return;
  O->next = n;
}

DskioHandle *DskioHandlePrev(DskioHandle *h)
{/* return the previous link in the linked list */
  if(h == NULL) return (DskioHandle *) NULL;
  return (DskioHandle *) h->prev;
}

void DskioHandleSPrev(DskioHandle *O, DskioHandle *p)
{/* return the previous link in the linked list */
  if(!O) return;
  O->prev = p;
}

void *DskioHandleFileData(DskioHandle *h)
{/* return the previous link in the linked list */
  if(h == 0) return (void *) 0;
  return (void *) h->file_data;
}

void DskioHandleSFileData(DskioHandle *h, void *file_data)
{/* return the previous link in the linked list */
  if(h == 0) return;
  h->file_data = file_data;
}

int DskioHandleNewIndex(DskioChain *hc)
{DskioHandle *h;
 int i,old,beg,new_index=1,match=0;
 h = DskioChainFirst(hc);
 if(!h) return 3;
 beg = DskioHandleIndex(h) + 1;
 for(i=beg;i<256;i++) {
   h = DskioChainFirst(hc);
   match = 0;
   while(h) {
     old = DskioHandleIndex(h);
     if(i==old) match=1;
     h = DskioHandleNext(h);
   }
   if(match==0) return i;
 }
 return -1;
}

int DskioHandleIndex(DskioHandle *dh)
{if(!dh) return -1;
 return dh->index;
}

void DskioHandleSIndex(int index, DskioHandle *dh)
{if(!dh) return;
 dh->index=index;
}

int DskioHandleRecl(DskioHandle *dh)
{if(!dh) return 0;
 return dh->recl;
}

void DskioHandleSRecl(int recl, DskioHandle *dh)
{if(!dh) return;
 dh->recl=recl;
}

int DskioHandleOflag(DskioHandle *dh)
{if(!dh) return 0;
 return dh->oflag;
}

int DskioHandleIOtype(DskioHandle *dh)
{if(!dh) return -1;
 return dh->iotype;
}

void DskioHandleSIOtype(int iotype, DskioHandle *dh)
{if(!dh) return;
 dh->iotype=iotype;
}

void *DskioHandleIOhandle(DskioHandle *dh)
{if(!dh) return 0;
 return dh->iohandle;
}

void DskioHandleSIOhandle(void *iohandle, DskioHandle *dh)
{if(!dh) return;
 dh->iohandle = iohandle;
}

char *DskioHandleFname(DskioHandle *dh)
{if(!dh) return 0;
 return dh->fname;
}

void DskioHandleSFname(char *fname, DskioHandle *dh)
{if(!dh) return;
if(!fname) {dh->fname[0]='\0'; return; }
 if(strlen(fname) > 255) {
    strncpy(dh->fname,fname,255);
    dh->fname[255]='\0';
 } else {
    strcpy(dh->fname,fname);
 }
}

void dskio_paop_(int *fd, char *fname,int *oflag,int *i_err)
{int opf= *oflag,j;
 *i_err= -1;
 *fd= -1;
 if(!fname) return;
 if(strcmp(fname,"none")==0) return;
 if(strcmp(fname,"NONE")==0) return;

 if(opf & O_CREAT) {
   if(opf & O_EXCL) {
    j = access(fname,F_OK);
    if(j != -1) return;
   }
   *fd = fopen_pa(fname, "w+");
 } else {
  if(opf == O_RDONLY) {
    *fd = fopen_pa(fname, "r");
  } else {
    *fd = fopen_pa(fname, "r+");
  }
 }
/*
 if(fp && bsiz>8192)
  setvbuf( fp, 0, _IOFBF, (size_t) bsiz);
*/
 (*i_err) = (*fd < 0) ? -1 : 0;
 return;
}

void dskio_pawr_(int *fd, char *b, int *nby, int *i_err)
{ int nw= *nby;
 *i_err= -1;
 if(*fd<0 || nw<=0) return;
 nw = (int) fwrite_pa(b, (size_t) 1, (size_t) (*nby), *fd);
 (*i_err) = (nw != *nby) ? -1 : 0;
 return;
}

void dskio_paskwr_(int *fd, char *b,long *off, int *nby, int *i_err)
{*i_err= -1;
 if(*fd<0) { return; }
 *i_err = fseek_pa(*fd,*off,SEEK_SET);
 (*i_err) = (*i_err == 0) ? 0 : -1;
 if(*i_err < 0) return;
 dskio_pawr_(fd, b, nby, i_err);
}

void dskio_pard_(int *fd, char *b, int *nby, int *i_err)
{int nr= *nby;
 *i_err= -1;
 if(*fd<0 || nr<=0) return;
 nr = (int) fread_pa(b, 1, (size_t) (*nby), *fd);
 (*i_err) = (nr != *nby) ? -1 : 0;
 return;
}

void dskio_paskrd_(int *fd, char *b,long *off, int *nby, int *i_err)
{*i_err= -1;
 if(*fd<0) { return; }
 *i_err = fseek_pa(*(int *)fd,*off,SEEK_SET);
 (*i_err) = (*i_err == 0) ? 0 : -1;
 if(*i_err < 0) return;
 dskio_pard_(fd, b, nby, i_err);
}

int fseek_pa(int fd,long off,int mode) {
 return -1;
}

int fopen_pa(char *name, char *mode) {
 return -1;
}

int fclose_pa(int fd) {
 return -1;
}

int fread_pa(char *b, size_t siz, size_t n, int d) {
 return -1;
}

int fwrite_pa(char *b, size_t siz, size_t n, int d) {
 return -1;
}

