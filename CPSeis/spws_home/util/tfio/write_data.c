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
#include <string.h>
#include <stdio.h>
#include "tf_global.h"
#include "tfdefs.h"
#include "dio.h"
#include "wrdcnvrt.h"
#include "write_data.h"

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
float *write_data_iobuf(int nbytes);
#ifdef __cplusplus
}                   // for C++
#endif

#define LAVHDR 25
/*
 * An internal scratch buffer for write_data.
 * Only allocates more space if needed
 */

float *write_data_iobuf(int nbytes)
{static int  iobufsiz;
 static float *fbuf;
 int  bsiz,bfsz;

 if(fbuf == NULL || (nbytes > iobufsiz) ) {
   bsiz = nbytes;
   if(bsiz < 32768) bsiz = 32768;
   if(fbuf) { free(fbuf); fbuf = NULL; }
   bfsz = 1+bsiz/sizeof(float);
   fbuf = (float *) calloc(1,bsiz);
   iobufsiz = bsiz;
 }
 return fbuf;
}

/*------------------------------------------------------------
C\USER DOC
 *Name   : write_data_
 *Purpose: Writes out trace and header data to disk.
 *Author : R. Day
 *Date   : 96/02/08
 *Last revised: 2000/01/27  Day
 *
 *Function Definition:        ( Language = C )
 * int  write_dataf_(int *ufi, int * cnt, int  *tpos,
 *      WREAL *hd,int *nwih,
 *      WREAL *tr,int *ndpt,
 *      WREAL *Clip, char *wrkbuff, int *wrksize)
 *     ---> writes cnt hd&tr to file tied to descriptor ufi
 *     ---> Fortran callable interface
 * int  write_datac_(TF_Global *g,int *ufi,int *cnt,int  *tpos,
 *      float *hd, int *nwih,
 *      float *tr, int  *ndpt,
 *      float *Clip, char *wrkbuff, int *wrksize)
 *     ---> writes cnt hd&tr to file described by g.
 *     ---> C callable interface
 * int  write_data_(TF_Global *g, int *ufi, int  *tpos,
 *      float *hd, float *tr, int  *ndpt)
 *     ---> writes 1 hd&tr to file described by g.
 * g         in       Complete file description(see tfio.h)
 * ufi       in       A file descriptor returned by tf_open
 * hd        in       Header data - CPS format.
 * tr        in       Trace data.
 * tpos      in       Sequential trace number in the file
 * ndpt      in       1st stroage dimension of tr.
 * nwih      in       1st storage dimension of hd.
 * cnt       in       number of hd&tr vectors to output
 * Clip      in       clip data to this value when compresing
 *                    from float to 2byte or 1 byte types.
 *                    No clipping if Clip < 0.
 * wrkbuff   in       user provided buffer when
 *                    data is to be converted from float
 * wrksize   in       size of buffer in bytes.
 *                    (safe size is 4*cnt*(nwih+ndpt) )
 * 
 *
 *NOTES:
 * 1. The file is assumed to have already been created and
 *    opened with a call to tf_open. Most of the file
 *    characteristics are set at open time, which is why the
 *    argument list for the write call is so simple.
 * 2. write_data  does not close the file. Use tf_close_
 * 3. returns 0 if there is a failure
 * 4. TFILE,TFILE3,TFILE3F,VOXET,BRICK,HGRID are OK
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    -------------------------------------
 *2000/01/27 Day        Fixed buffer bug in write_data_
 *99/06/28  Day         bswap calls absorbed into wrdc_fcnvrt
 *99/04/29  Day         Commented out a write call that should
 *                      have been removed by the last revision.
 *                      Added write_dataf_ for fortran access.
 *                      Corrected write_datac_ bug when the
 *                      file header count!=memory header count.
 *                      Replaced netw_lnode with wrdc_localwd.
 *99/03/23  Day         Converted a few lseek and write calls
 *                      that were missed in the 3/08 changes.
 *                      Added a dskio_xsk call to time
 *                      slice transpose to force a flush.
 *99/03/08  Day         Converted to dskio_x... calls
 *99/02/08  Day         Problem fixed with inplace word
 *                      conversion. Increased scratch buffer.
 *                      Needed for stupid Cray conversion
 *                      routines.
 *                      Added bswap call to correct output of
 *                      IEEE words on byte swapped machines.
 *                      Removed signed chars.
 *99/02/02  Day         Updating Conlib with 01/29 and 01/27 changes
 *99/01/29  Day         Updated on t3e
 *99/01/27  Day         Added capability to produce either
 *                      signed or unsigned byte trace values
 *98/11/11  Day         Added write_data_iobuf to cure a
 *                      word alignment problem. Replaces call
 *                      to dio_iobuf
 *97/12/02  Day         T3F files now store hd 25 slice. Some
 *                      changes for better history support.
 *97/07/17  Day         action taken for malloc faiures in 
 *                      write_data_trace_to_slice.
 *96/11/20  Day         Corrected problem with TFILE headers
 *                      when outputting byte data
 *96/10/22  Vunderink   Inserted into CONLIB.
C\END DOC
 *-----------------------------------------------------------*/
int  write_dataf_(int *ufi, int * cnt, int  *tpos,
     WREAL *hd,int *nwih,
     WREAL *tr,int *ndpt,
     WREAL *Clip, char *wrkbuff, int *wrksize) {
 TF_Global glbl;
 int   i_err,num;
 float *hdo=0,*tro=0, clipf;

 if(*ufi < 0 || *cnt < 0) return 0;

/* get file global data */
 tf_glbl_get_( &i_err, ufi, &glbl);
 if(i_err!=0) {
   return 0;
 }

 /* Take care of REAL-float differences, convert in place */
 hdo = (float *) hd;
 tro = (float *) tr;
 if(sizeof(WREAL) != sizeof(float) ) {
   num = *cnt * *nwih;
   wrdc_real_to_float_(&num,hd,hdo);
   num = *cnt * *ndpt;
   wrdc_real_to_float_(&num,tr,tro);
 }

 clipf = *Clip;
 i_err = write_datac_(&glbl, ufi, cnt, tpos,
     hdo, nwih, tro, ndpt,
     &clipf, wrkbuff, wrksize);

 /* undo the in place conversion */
 if(sizeof(WREAL) != sizeof(float) ) {
   num = *cnt * *nwih;
   wrdc_float_to_real_(&num,hdo,hd);
   num = *cnt * *ndpt;
   wrdc_float_to_real_(&num,tro,tr);
 }
 return  i_err;
}

int  write_data_(TF_Global *g, int *ufi, int  *tpos,
     float *hd, float *tr, int  *ndpt)
{ int n= *ndpt, numtrace=1, nwih=64, wrksize=0;
  int   itr,ierr=0,ipos;
  float *hdi,*tri;
  float clip, *fbuf=0;
  float *wrkbuff=0;

  wrksize= 3*(nwih + n + 2) * sizeof(float);
  fbuf = (float *) write_data_iobuf(wrksize);
  wrkbuff = fbuf + 4*((nwih+ n + 1)/2); /* full word boundary */
  hdi=hd;
  tri=tr;
  for(itr=0;itr<numtrace;itr++) {
    clip = -1.0; /* no clipping, compute and use lav */
    ipos = *tpos + itr;
    wrksize= 2*((nwih + n + 1)/2) * sizeof(float);
    ierr = write_datac_(g, ufi, &numtrace,&ipos, hdi, &nwih, tri, ndpt,
        &clip,(char *) wrkbuff, &wrksize);
    if(ierr==0) return ierr;
    hdi += nwih;
    tri += n;
  }
 return ierr;
}

/* hd[cnt][*nwih] contains float header values
 * tr[cnt][*ndpt] contains float trace values
 * wrkbuff... holds converted traces and headers
 *           caller must provide sufficient buffer space
 */
int  write_datac_(TF_Global *g, int *ufi, int *cnt, int  *tpos,
     float *hd, int *nwih,
     float *tr, int  *ndpt,
     float *Clip, char *wrkbuff, int *wrksize)
{char  msg[80];
 float lav,clip= *Clip;
 int   ntraces= *cnt, nhdwd= *nwih;
 float *hdi,*tri;
 char  *hdo,*tro;
 int  i,l,n;
 int  itr=0,tracei;
 int  num, ierr;
 int  byt_per_tr;
 int  ftype,wdtypi,wdtypo;
 char *fbuf=0,*ff=0, *ffo=0;

 if(!g  || !ufi) return 0;
 if(!hd || !tr)  return 0;
 if(*ufi <=0)    return 0;
 if(!wrkbuff)     return 0;
 if(g->lun != *ufi) {/* because of data base */
   printf("write_data_: file descriptor mismatch?\n");
   printf("write_data_:  fd1=%d  fd2=%d\n",*ufi,g->lun);
   return 0;
 }
 if(ntraces<1) return 1;
 wdtypi = wrdc_localwd_();
 wdtypo = g->wdtyp;
 byt_per_tr = g->nbyhd*g->nhdwd + g->ndptr*g->nbydp;
 fbuf = wrkbuff;
 if(*wrksize>0) { /* use provided buffer */
   if(ntraces*byt_per_tr > *wrksize) {
    sprintf(msg,"write_datac: buffer too small for data conversion\n");
    return 0;
   }
 }
 /* allocate a static buffer to hold one trace */
 n = 2*(*ndpt + *nwih) * sizeof(float);
 ff = (char *) write_data_iobuf(n);
 if(!ff) return 0;
/*
 * initialize the output buffer
 */
 for(i=0;i<ntraces*byt_per_tr;i++) {
   fbuf[i] = '\0';
 }



/*************************************************************
 * Convert trace & header values to output types.          ***
 ************************************************************/
 num = (*ndpt > g->ndptr) ? g->ndptr : *ndpt;
 hdi = hd;
 tri = tr;
 hdo = fbuf;
 tro = fbuf + ntraces*g->nhdwd*g->nbyhd;
 for(itr=0;itr<ntraces;itr++) {

   lav = fabs(tri[0]);
   for(i=0;i<num;i++)
     lav = (fabs(tri[i]) > lav) ? fabs(tri[i]) : lav;
   hdi[LAVHDR-1] = lav;
   if(*Clip < 0) {
     clip = lav;
   }

   if(g->nbydp==1) {
     if(clip>g->trmaxg) g->trmaxg = clip;
     if(g->wdtyp==WBYTE)
      float_to_byt_(tri,&num,(unsigned char *) tro, &num, &clip);
     else
      float_to_bytsgn(tri,&num,(unsigned char *) tro, &clip);
   } else if(wdtypo==WIBM2) {
     if(clip>g->trmaxg) g->trmaxg = clip;
     float_to_bit16_(tri,&num,(char *) tro, &clip);
   } else {
     if(lav>g->trmaxg) g->trmaxg = lav;
     memcpy(ff,tri,num*sizeof(float));
     ffo = ff + 2*(num/2 + 1)*sizeof(float);
     l = wrdc_fcnvrt_(&wdtypi,(char *) ff,&wdtypo, (char *)ffo, &num, msg);
     if(l != 0) {goto error; }
/* RSD
     if(wdtypo==WIEEE) bswap_(&num,(unsigned char *)ffo);
*/
     memcpy(tro,ffo,num*g->nbydp);
   }
   tri += *ndpt;
   tro += g->ndptr*g->nbydp;

   ftype = getgl_str_to_ftype(g->ftyp);
   if(ftype==SEGY_TYPE) {/* build segy header */
     tracei = *tpos + itr;
     dio_segy_hbld(hdi, &tracei, &g->srval, &g->ndptr,(char *) hdo);
   } else { /* Tfile and all other types */
     if(g->nhdwd>0) {
       if(wdtypo == WBYTE) wdtypo = WIEEE;
       if(wdtypo == WSBYT) wdtypo = WIEEE;
       n = (g->nhdwd < nhdwd) ? g->nhdwd : nhdwd;
       l = wrdc_fcnvrt_(&wdtypi,(char *) hdi,&wdtypo, hdo, &n, msg);
       if(l != 0) goto error;
/* RSD
       if(wdtypo==WIEEE) bswap_(&nhdwd,(unsigned char *)hdo);
*/
     }
   }
   hdi += nhdwd;
   hdo += g->nbyhd*g->nhdwd;
 }

/*************************************************************
 * Write data to output file                               ***
 ************************************************************/
 hdo = fbuf;
 tro = fbuf + ntraces*g->nbyhd*g->nhdwd;
 n = tf_tr_wr_( ufi, tpos, &ntraces,hdo,tro,msg);
 if(n != 0) {
   goto error;
 }

/*************************************************************
 * Update the data base entry which was created by tf_open ***
 ************************************************************/
 if(*tpos + ntraces -1 >g->ntrfil) g->ntrfil= *tpos + ntraces -1;
 tf_glbl_add_( &ierr, ufi, g);

 return 1;

error:
 printf("write_data_: ERROR\nwrite_data_: %s\n",msg);
 return 0;
}

 /*------------------------------------------------------------
C\USER DOC
 *Name   : write_data_cubeslice
 *Purpose: Write a cube slice to output
 *Author : R. Day
 *Date   : 96/12/04
 *
 *Function Definition:        ( Language = C )
 * int write_data_cubeslice(int fd, IO_request *r, TF_Global *g
 *     char *hd, char *tr);
 *
 * fd        in       File Descriptor of open data file.
 * r         in       IO_request (see tfio.h)
 * g         in       Complete file description(see tfio.h)
 *
 *NOTES:
 * 1. tr is assumed to have a cube slice ready to write.
 *    opened with a call to tf_open. Make this call only
 *    after all normal traces have been output to the file.
 * 2. r->axis and r->index specify which axis and slice.
 * 3. returns 0/1 if there is a failure/no problem
 * 4. Supports VOXET,HGRID,TFILE3, and TFILE3F file types
 *    for axis 3, and 2. Only TFILE3F supported for axis=1.
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    -------------------------------------
C\END DOC
 *-----------------------------------------------------------*/
int write_data_cubeslice(int fd, IO_request *ioreq,TF_Global *g,
    char *hd, char *tr) {
 /* tr contains the cube slice */
 int fd_ = fd;
 int bypht, byphd, byptr, nbydp, ntowr, n1,n2,n3;
 int i_err,ftype,otr,itr,tinc,ohd,i;
 long offset;
 char *buff;
 int ISOK=1, NOTOK=0;

  if(!ioreq || !g) return NOTOK;
  if(tf_global_is3d(g) ==0) return NOTOK;
  tf_global_grid_sizes(g,&n1,&n2,&n3);
  bypht = tf_global_byinht(g);
  byptr = tf_global_byintr(g);
  byphd = tf_global_byinhd(g);
  nbydp = tf_global_get_nbydp(g);
  ftype = getgl_str_to_ftype(tf_global_ftype(g));



 switch (ioreq->axis) {
 case 3:
    if(ftype==VOXET_TYPE || ftype==HGRID_TYPE ||
       ftype==TF3D_TYPE  || ftype==TF3DF_TYPE) {
     ioreq->nsamp= n1;
     ioreq->samp1= 1;
     ioreq->sdec= 1;
     buff = (char *) malloc(n2*bypht);
     if(!buff) return NOTOK;
     offset= tf_global_get_grecsiz(g) + ioreq->index *  n2 * bypht;

     otr = 0;
     tinc = ioreq->nsamp*nbydp;
     ohd = 0; 
     for(i=0;i<n2;i++) { /* fill the out buffer */
       ioreq->ntot++;
       if(byphd>0 && hd) memcpy(buff + i*bypht, hd+ohd, byphd); 
       ohd += byphd;
       memcpy(buff+byphd+i*bypht,tr+otr, ioreq->nsamp*nbydp); 
       otr += tinc;
     }
     ntowr= n2*bypht;
     dskio_xxskwr_(&fd_,tr,&offset,&ntowr,&i_err);
     free(buff);
     if(i_err ==0) return ISOK;
    }

    break;
 case 2:
    if(ftype==VOXET_TYPE || ftype==HGRID_TYPE ||
       ftype==TF3D_TYPE  || ftype==TF3DF_TYPE) {
     ioreq->nsamp= n1;
     ioreq->samp1= 1;
     ioreq->sdec= 1;
     buff = (char *) malloc(2*bypht);
     if(!buff) return NOTOK;
     offset= tf_global_get_grecsiz(g) + ioreq->index * bypht;
     ntowr= bypht;
     otr = 0;
     tinc = ioreq->nsamp*nbydp;
     ohd = 0; 
     itr = byphd + (ioreq->samp1-1)*nbydp;
     for(i=0;i<n3;i++) {
       ioreq->ntot++;
       if(byphd>0 &&hd) memcpy(buff, hd+ohd, byphd); 
       ohd += byphd;
       memcpy(buff + itr, tr+otr, ioreq->nsamp*nbydp); 
       otr += tinc;
       dskio_xxskwr_(&fd_,buff,&offset,&ntowr,&i_err);
       if(i_err != 0) { free(buff); return NOTOK; }
       offset = (n2-1)*bypht;
     }
     free(buff);
     return ISOK;
    }

    break;
 case 1:
    if(ftype==VOXET_TYPE || ftype==HGRID_TYPE ||
       ftype==TF3D_TYPE ) {
     return NOTOK;
    }
    if(ftype==TF3DF_TYPE) {
     offset= tf_global_get_grecsiz(g) + n2*n3*bypht + n2*n3*ioreq->index*nbydp;
     ntowr= n2*n3*nbydp;
     dskio_xxskwr_(&fd_,tr,&offset,&ntowr,&i_err);
     if(i_err !=0) return NOTOK;
     return ISOK;
    }

    break;
 default:
    return NOTOK;
 }
 return NOTOK;
}

 /*------------------------------------------------------------
C\USER DOC
 *Name   : write_data_trace_to_slice
 *Purpose: Transpose traces in file to form time slices.
 *Author : R. Day
 *Date   : 96/12/04
 *
 *Function Definition:        ( Language = C )
 * int write_data_trace_to_slice(int fd, TF_Global *g)
 *
 * fd        in       File Descriptor of open data file.
 * g         in       Complete file description(see tfio.h)
 *
 *NOTES:
 * 1. The file is assumed to have already been created and
 *    opened with a call to tf_open. Make this call only
 *    after all normal traces have been output to the file.
 * 2. write_data_trace_to_slice does not close the file.
 * 3. returns 0/1 if there is a failure/no problem
 * 4. TFILE3F file type is the only valid type.
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    -------------------------------------
C\END DOC
 *-----------------------------------------------------------*/
int write_data_trace_to_slice_(int *fd, TF_Global *g)
{ int fd_= *fd, ftype;
  int NOTOK=0, ISOK=1, D3=8, last;
  int bypht, byphd, byptr, nbydp, nbyhd, ntord, nrd, n1,n2,n3;
  int i,j,k,scnt,itr,otr,tinc,iby, ntowr, i_err;
  char *rbuf=0, *xbuf=0, *hd, *trout,*hdout;
  long offset;

  if(!fd_ || !g) return NOTOK;
  if(g->ntrfil<1) return NOTOK;
  if(tf_global_is3d(g) ==0) return NOTOK;
  ftype = getgl_str_to_ftype(tf_global_ftype(g));
  if(ftype != TF3DF_TYPE) return NOTOK;
  if(tf_global_get_ntrcll(g) > 1) return NOTOK;
  bypht = tf_global_byinht(g);
  byptr = tf_global_byintr(g);
  byphd = tf_global_byinhd(g);
  nbydp = tf_global_get_nbydp(g);
  nbyhd = tf_global_get_nbyhd(g);
  tf_global_grid_sizes(g,&n1,&n2,&n3);

  if(D3>n3) D3=n3;
  i = (nbydp>nbyhd) ? nbydp : nbyhd;
  xbuf = (char *) malloc(D3*n2*i);
  if(!xbuf) { free(xbuf);
   printf("write_data_trace_to_slice: failure(fatal) to allocate xbuf\n");
   return NOTOK;
  }
  while( (rbuf = (char *) malloc(D3*n2*bypht))==NULL) {
   printf("write_data_trace_to_slice: mem failure for %d slices\n",D3);
   D3--;
   if(D3<4) {
    printf("write_data_trace_to_slice: failure(fatal) to allocate mem\n");
    free(xbuf); return NOTOK;
   }
  }


  for(i=0;i<n3;i+=D3) {

  /* read in a header-trace slab */
   last = i+D3-1;
   if(last>n3-1) last=n3-1;
   ntord=(last-i+1)*n2*bypht;
   offset= tf_global_get_grecsiz(g) + i*n2*bypht;
   dskio_xsk_(&fd_,&offset,&i_err); /* force a seek */
   dskio_xxskrd_(&fd_,rbuf,&offset,&ntord,&i_err);
   if(i_err !=0) { free(rbuf); free(xbuf); return NOTOK; }
   
 
  /* compress the header-trace slab to a trace slab */
   scnt = last-i+1;
   otr=0;
   tinc = byptr+nbyhd;
   hdout  = rbuf;
   trout  = rbuf+nbyhd;
   for(j=0;j<n2*scnt;j++) {
       hd = rbuf+j*bypht;
       memcpy(hdout, hd+(LAVHDR-1)*nbyhd,nbyhd);
       memcpy(trout, hd+byphd, byptr); 
       hdout += tinc;
       trout += tinc;
   }

  /* Transpose and output the slab
   * header 25 goes to the tail of the slab
  */
   for(j=0;j<n1;j++) {
    for(k=0;k<scnt;k++) {
      itr = nbyhd + j*nbydp +  k*n2*tinc;
      for(iby=0;iby<nbydp;iby++)
        tf_bmov_(rbuf+itr+iby,&tinc,xbuf+k*n2*nbydp+iby, &nbydp,&n2);
    }
    offset= tf_global_get_grecsiz(g) + n2*n3*bypht + j*n2*n3*nbydp
            + i*n2*nbydp;
    ntowr = scnt*n2*nbydp;
    dskio_xxskwr_(&fd_,xbuf,&offset,&ntowr,&i_err);
    if(i_err !=0) { free(rbuf); free(xbuf); return NOTOK; }
   }
   for(k=0;k<scnt;k++) {/* output the header 25 slice */
    itr = k*n2*tinc;
    for(iby=0;iby<nbyhd;iby++)
      tf_bmov_(rbuf+itr+iby,&tinc,xbuf+k*n2*nbyhd+iby, &nbyhd,&n2);
   }
   offset= tf_global_get_grecsiz(g) + n2*n3*bypht + 
      n1*n2*n3*nbydp + i*n2*nbyhd;
   ntowr = scnt*n2*nbyhd;
   dskio_xxskwr_(&fd_,xbuf,&offset,&ntowr,&i_err);

  }
 free(rbuf);
 free(xbuf);
 return ISOK;
}

/*------------------------------------------------------------
C\USER DOC
 *Name   : write_data_tslice
 *Purpose: Write a seismic time slice to a TF3DF type file.
 *Author : R. Day
 *Date   : 96/12/04
 *
 *Function Definition:        ( Language = C )
 * int write_data_tslice(int fd, TF_Global *g, int index,
 *     int n, char *slice)
 *
 * fd        in       File Descriptor of open data file.
 * g         in       Complete file description(see tfio.h)
 * index     in       index of the 1st slice, starting from 0.
 * n         in       number of slices to output.
 * slice     in       slices ready to output.
 *
 *NOTES:
 * 1. The file is assumed to have already been created and
 *    opened with a call to tf_open. Most of the file
 *    characteristics are set at open time, which is why the
 *    argument list for the write call is so simple.
 *    It is assumed the data slice is ready to write, and
 *    that it has dimensions consistent with TF_Global.
 * 2. write_data_tslice  does not close the file.
 * 3. returns 0/1 if there is a failure/no problem
 * 4. TFILE3F file type is the only valid type.
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    -------------------------------------
C\END DOC
 *-----------------------------------------------------------*/
int write_data_tslice(int fd, TF_Global *g, int index, int n, char *slice)
{ int fd_=fd, ftype;
  int NOTOK=0, ISOK=1;
  int bypht, nbydp, n1,n2,n3;
  int i_err,ntowr, pos_in_cell,cell_no;
  long offset;

  if(!fd_ || !g) return NOTOK;
  if(tf_global_is3d(g) ==0) return NOTOK;
  ftype = getgl_str_to_ftype(tf_global_ftype(g));
  if(ftype != TF3DF_TYPE) return NOTOK;
  if(tf_global_get_ntrcll(g) > 1) return NOTOK;
  bypht = tf_global_byinht(g);
  nbydp = tf_global_get_nbydp(g);
  tf_global_grid_sizes(g,&n1,&n2,&n3);


  cell_no    = (n2*n3-1)/g->ntrcll;
  pos_in_cell= n2*n3  - cell_no*g->ntrcll -1;
  offset     = g->grecsiz + cell_no*g->nbycll +
               (pos_in_cell+1)*bypht + index*n2*n3*nbydp;

  ntowr = n*n2*n3*nbydp;
  dskio_xxskwr_(&fd_,slice,&offset,&ntowr,&i_err);
 return ISOK;
}

/*------------------------------------------------------------
C\USER DOC
 *Name   : write_data_hist_
 *Purpose: Transfers history information from a CPS history
 *         file to a seismic or grid file.
 *         (To the header file if a dual header/data combo)
 *Author : R. Day
 *Date   : 96/02/08
 *
 *Function Definition:        ( Language = C )
 * int  write_data_hist_(TF_Global *g, char *hist_file )
 * g         in       Complete file description(see tfio.h)
 * hist_file in       Name of existing history file.
 *
 *NOTES:
 * 1. The file is assumed to have already been created and
 *    opened with a call to tf_open. Most of the file
 *    characteristics are set at open time, which is why the
 *    argument list for the write call is so simple.
 * 2. write_data_hist  does not close the file. Use tf_close_
 * 3. returns 0 if there is a failure
 * 4. TFILE,TFILE3,TFILE3F,VOXET,BRICK,HGRID are OK
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    -------------------------------------
C\END DOC
 *-----------------------------------------------------------*/
int write_data_hist_(TF_Global *g, char *hist_file )
{FILE *fp;
 char *hfile=0,*dfile=0;
 int   one=1,hcsiz=80,hcpos=0,bpos,start=0,i_err;
 int   fd,nby,opmode=2,doclose=0, opt=0;
 long  off=0;
 int   i,j,nbuf=0,bsiz=1024;
 int   nc=0,nc0=0,ntowr;
 char  card[160],dbuf[1024],*cpntr;
 
 char *str=0;
 if(!g) return 0;
 hfile=tf_global_header_file(g);
 dfile=tf_global_data_file(g);
 if(!hfile || !hist_file) return 0;
 if(strcmp(g->ftyp,"DSEGY")==0) return 0;

 fp = fopen(hist_file,"r");
 if(!fp)
  {printf("write_data_hist: open failure, file=%s\n",hist_file);
   return 0;
  }

 if(strcmp(dfile,hfile)==0 || strcmp(dfile,"SAME")==0)
  {/* header and data are in same file */
   doclose=0;
   fd = tf_global_fd_(g);
   hcpos = tf_global_first_hcbyt(g);
  }
 else
  {/* header file is seperate from data file*/
   doclose=0;
   opmode=dskio_orw_();
   dskio_xop_(&opt,&fd,hfile,&opmode,&i_err);
   hcpos = (int) dskio_xfsize(&fd);
  }

 dbuf[0]='\0';
 bpos=0;
 while((cpntr= fgets(card+1,hcsiz+5,fp)) != NULL)
 {
  card[81]='\0';
  if(ferror(fp)) break;
  if(start==0)
   {if(strstr(card+1,"Current history record")!= NULL) start=1;}
  if( start)
   {card[0] = '#'; /* makes history card a comment for dcd */
    card[hcsiz+1]='\n';
    for(i=0;i<hcsiz;i++)
      if(card[i]=='\n') break;
    for(j=i;j<hcsiz;j++) card[j]=' ';
    if(i> 0)
     {
       card[hcsiz-1]='\n';
       memcpy(dbuf+bpos,card,hcsiz);
       nc++;
       /* printf("%s",card);  */
       bpos += hcsiz;
     }
    if(bpos >= bsiz - hcsiz)
     {ntowr = bpos/hcsiz;
      nby = ntowr*hcsiz;
      off = hcpos + nc0*hcsiz;
      dskio_xxskwr_(&fd,dbuf,&off,&nby,&i_err);
      bpos=0;
      nc0=nc;
     }
   }
  if(feof(fp)) break;
 }

 ntowr = bpos/hcsiz;
 if(bpos>0 && start)
  {nby = ntowr*hcsiz;
   off = hcpos + nc0*hcsiz;
   dskio_xxskwr_(&fd,dbuf,&off,&nby,&i_err);
  }
 fclose(fp); /* close history file */
 if(doclose==1) dskio_xxcl_(&fd,&i_err);
 g->numhc += nc;
 if(doclose==0) tf_glbl_add_( &j, &fd, g);
 return nc;
}

