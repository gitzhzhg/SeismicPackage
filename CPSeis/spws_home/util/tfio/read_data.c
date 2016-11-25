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
#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#include <sys/types.h>
#include <sys/unistd.h>
#else
#if (!VMS)
#include <sys/types.h>
#include <unistd.h>
#endif
#endif
#include "tf_global.h"
#include "tfdefs.h"
#include "cprim.h"
#include "read_data.h"
#include "jsfiles.h"

#define HDROUT  64
#define LAVHDR  25
#define MUTHDR   2
#define TAILHDR 64 

static CubeTrcio *cube = 0;
static int  rdbufsiz = 0;
static char *fbuf    = NULL;
static int  muteadj=1;


#ifdef NEED_CAPITALS
#define sizeof_real_ SIZEOF_REAL
#endif
#if (VMS || _AIX || __hpux)
#define sizeof_real_ sizeof_real
#endif

int sizeof_real_();

/*------------------------------------------------------------------
C\USER DOC
 *Name   : read_data_
 *Purpose: Reads in trace and header data from supported files.
 *Author : R. Day
 *Date   : 93/07/30
 *Last revised: 99/12/08
 *
 *Function Definition:        ( Language = C )
 * int  read_data_(char name[], int  *istat, IO_request *Cl,
 *      char *hd, unsigned char *tr, float *TASF)
 * void read_data_madj(int toggle)
 * istat     output    0 if no errors occur.
 * name      in        Name of the trace file
 *  Cl       in&out    Controls which traces and samples will be read.
 *                     See tfio.h for structure definition.
 * hd[]      out       Will contain floating point headers from the
 *                     traces packed head to tail. 64 headers/trace
 * tr[]      out       Will contain the requested traces packed head
 *                     to tail.
 * TASF      out       Array to hold scale factors which can be used
 *                     to convert trace values to true amplitude.
 * toggle    in        1/0 --> do/dont adjust mutes for time windows.
 * If the file contains byte data :True value = (byte-128)*TASF 
 * If the file contains float data:True value = float *TASF
 *
 *NOTES:
 * 1. Supported file types = TFILE,STROT,DTROT,DSEGY.
 * 2. hd and tr must be dimensioned large enough to contain all the
 *    data requested.
 *    STROT and DTROT files contain 64 8byte headers per trace.
 * 3. read_data computes the lav(for the returned trace) and places
 *    it in header word 25.
 * 4. Cl-trnsps is set =0 so that traces are returned in trace order.
 * 5. read_data does not close the file. Follow it with a call to
 *    tf_close_() when you are done with the file!
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 * 00/07/18 M. Sherrill Added read_trcio_data to support the new Cps trot file
 * 99/12/08 Day         segy tail mute no longer ignored. CPS header
 *                      will be set for a segy input file.
 * 99/06/28 Day         bswap calls absorbed into wrdc_fcnvrt
 * 99/04/29 Day         Added fortran access call read_dataf_ .
 *                      Replaced netw_lnode by wrdc_localwd_.
 * 99/03/08 Day         replaced calls to read & lseek by calls to
 *                      the dskio_x... layer.
 * 99/02/01 Day         removed signed chars
 * 99/02/01 Day         Updating Conlib
 * 99/01/29 Day         (float *) tr+ot changed to (float *) (tr+ot)
 *                      in read_data_segy. Now supports signed and
 *                      unsigned byte data. read_data_wfix fixed
 *                      = to == in some ifs.
 * 98/10/30 Day         read_data_hdslice fix for cray
 * 98/10/26 Day         Fixed bug on workstation introduced by
 *                      previous bug fix on the cray.
 * 98/10/21 Day         corrected transpose of TFILE3F time slices
 *                      on systems where IEEE float != sizeof(float)
 * 98/10/19 Day         read_data_segy altered to also handle
 *                      Landmark 1 byte segy data.
 * 98/10/14 Day         read_data_segy altered to fix word alignment
 *                      problems on the j90.
 * 98/10/06 Day         Changed QTROT read logic so word alignment
 *                      is forced before wrdc_fcnvrt is called. The
 *                      IEG2CRAY function demands word alignment.
 * 98/09/18 Day         Changed prototype of wrdc_exp21 due
 *                      to word alignment problem for EXPAND21.
 * 98/09/14 Day         Added read_data_wfix, read_data_types,
 *                      read_data_groups, and updated read_data_trot
 *                      for QTROT & STROT recognition
 * 97/12/23 Day         Added read_data_hdslice and 
 *                      read_data_hdslice_byname. Using read_data.h
 * 97/04/04 Day         Corrected amplitude bug for byte tfiles
 * 96/11/20 Day         Corrected header problem with TILE3 files
 * 96/11/04 Day         Corrected mute logic. MUTHDR from 1 to 2.
 * 94/06/27 Day         Set bottom mute for segy files.
 * 94/02/14 Day         Now makes use of wrdc_fcnvrt routine
 * 93/07/30 Day         Dynamically allocating fbuf buffer.
 * 93/05/03 Day         Subtracted 1 from the TASF array subscript
 *                      Added capability to read some SEGY files.
 * 92/10/15 Day         Added true amplitude factor array to argument list.
 * 92/10/15 Day         Change head and tail mute when tmin > 1
 * 92/06/23 Day         Enabled Native floating Point TFILE files.
C\END DOC
 *------------------------------------------------------------------*/
/* to read a file that is already open */
int  read_dataf_(int *ufi, int *cnt, int *tpos,
        int *nwih, WREAL *hd,
        int *ndpt, WREAL *tr,
        char *wrkbuff, int *wrksize)
{ char name[160];
  TF_Global g;
  IO_request req;
  float *TASF, *ftmp;
  float *hdi=0,*tri=0;
  WREAL *hdo=0,*tro=0;
  int i,j,l,n,one=1,i_err,nby,nconh,ncont, lun;

 if(*ufi<0 || *cnt < 1) return 0;

/* Retrieve the globals of the open file */
 l = tf_glbl_get_(&i_err,ufi,&g);
 if(i_err!=0) return -1; /* file is not open */

 req.iskp= *tpos-1;
 req.ntot= *cnt;
 req.ndo = *cnt;
 req.nskp=0;
 req.nsamp=(g.ndptr>*ndpt) ? *ndpt : g.ndptr;
 req.samp1=1;
 req.sdec=1;
 req.trnsps=0;
 req.cnvrt=0;
 req.axis=0;
 req.index=0;
 dskio_chain_fname_(ufi, name);

 hdi = (float *) wrkbuff;
 tri = (float *) (wrkbuff+HDROUT*sizeof(float)**cnt);
 TASF= (float *) malloc((*cnt+1)*sizeof(float));
 if(!TASF) return -1;
 l = read_data_(name, &i_err, &req,
     (char *)hdi, (char *)tri, TASF, &lun, 0);
 if(i_err!= 0) { free(TASF); return -1;}
 
 for(i=0;i<*nwih**cnt;i++) hd[i]=0.;
 for(i=0;i<*ndpt**cnt;i++) tr[i]=0.;

 /* place headers in the output buffer */
   if(*nwih==HDROUT && sizeof(float)==sizeof(WREAL)) {
     nby = req.ntot * sizeof(float) * HDROUT;
     memcpy(hd,hdi,nby);
   } else {
     nconh = (HDROUT < *nwih) ? HDROUT : *nwih;
     hdo = (WREAL *) hd;
     for(j=0;j<req.ntot;j++) {
       for(i=0;i<nconh;i++) {
         hdo[i] =  hdi[i];
       }
       hdo += *nwih;
       hdi += HDROUT;
     }
   }

 /* place traces in the output buffer */
   tro = (WREAL *) tr;
   if(g.nbydp==1) {
     n = read_data_buffer(*ndpt*sizeof(WREAL));
     for(j=0;j<req.ntot;j++) {
       ftmp = (float *) fbuf;
       byte_to_float_((unsigned char *) tri,ndpt,ndpt,TASF+j, ftmp);
       for(i=0;i<*ndpt;i++) tro[i] = ftmp[i];
       tro += *ndpt;
       tri += req.nsamp;
     }
   } else {
     if(*ndpt==g.ndptr) {
       nby = req.ntot * sizeof(float) * g.ndptr;
       memcpy(tr,tri,nby);
     } else {
      ncont = (g.ndptr > *ndpt) ? *ndpt : g.ndptr;
      for(j=0;j<req.ntot;j++) {
        for(i=0;i<ncont;i++) {
          tro[i] =  tri[i];
        }
        tro += *ndpt;
        tri += req.nsamp;
      }
     }
   }

 free(TASF);
 return req.ntot;
}

int  read_data_(char *name, int  *istat, IO_request *Cl,
     char *hd, char *tr, float *TASF, int *lun, int open_file)
{char  msg[80];
 char *ftype=0;
 int  ierr;
 int  cl_stat;
 static TF_Global Glbls;
 long hdsize, trsize;


 
 *istat=4;

/* open file if unopen, retrieve globals */
 if(open_file)
   {
   *istat = read_data_open(lun, name, &Glbls, msg);
   if(*istat !=0) goto error;
   }
 

 /*Next are for new CPS I/O added MLS 07/14/2000*/
  if(!strcmp(Glbls.ftyp,"TROT") || !strcmp(Glbls.ftyp,"DSEGY") ||
     !strcmp(Glbls.ftyp,"JSEIS"))
    {
    read_trcio_data (name, istat, Cl, &Glbls, hd, tr, TASF, lun, open_file);
    return *istat;
    }



 ierr  = read_data_fromdisk(*lun, Cl, &Glbls, hd, tr);
 if(ierr != 1) {
  strcpy(msg,"read_data: error returned from read_data_fromdisk");
  *istat=3; goto error;
 }
/****************************************************************
 * Convert file in foreign word format to native float words  ***
 ***************************************************************/
 ierr = read_data_translate(Cl, &Glbls, hd, tr, TASF);
 if(ierr != 0) {
   strcpy(msg,"read_data: problem in read_data_translate");
   *istat=5;
   goto error;
 }
 *istat=0;
 return 0;
error:
 printf("%s\n",msg);
 if(*lun != 0 )
  {cl_stat=0; /* keep the file */
   tf_close_(lun,&cl_stat,msg);
   *lun=0;
   if(Glbls.h) free(Glbls.h);
   Glbls.lun =0;
  }
 return *istat;

}



 /* New CPS TROT type I/O added by M.L. Sherrill 07/14/2000
  * Note we are playing some games with double to float on
  * the header array. At the current time we do not want
  * to change all of the SPWS code to use a double precision header
  * array instead of the current float header array so we
  * will handle the casting here.
  */
void read_trcio_data(char *name, int  *istat, IO_request *Cl, 
                     TF_Global *Glbls, char *hd, char *tr, float *TASF,
                     int *lun, int open_file)
{
int    dir;
double *dhd = (double *)hd;
float  *ftr;
float  *float_trace_out = (float *)tr;
float  *fhd = (float *)hd;
long   hdr_offset, tr_offset, trace_out_offset;
int    num, i,j,k,m,n,hit_end;
int    ngrps, trace_num;
int    trace_count, n1, n2;
long   ierr;
int    isa_jseis_file;

/* In the case of a cube Z-slice (Cl->axis = 1) do not do this check on Cl
 *   because under some conditions tf_check_cntrl breaks Cl->nsamp which
 *   in turn breaks csv (kcc 03-05-01)
 */
  if (Cl->axis != 1) {
    *istat = (int)tf_check_cntrl(Glbls,Cl);
    if(*istat != 0)
      {
      return;
      }
  }

  if(Cl->axis > 0) {
    read_data_check_3d (Glbls);
    ierr = (long)read_trcio_cubeslice (name, Cl, Glbls, hd, tr, TASF, lun);
    if (ierr != 1) {
      *istat = 6;
    }
    else {
      *istat = 0;
    }
    return;
  }

  if(Glbls->ntrfil<=0) 
    {
    *istat = 2;
    Cl->ntot = 0;
    return;
    }

  if(Cl->iskp < 0 || Cl->iskp >Glbls->ntrfil-1) 
    {
    *istat = 3;
    Cl->ntot = 0;
    return;
    }

  if(Cl->ntot < 1) 
    {
    *istat = 4;
    return;
    }

  dir = (Cl->ndo>=0) ? 1 : -1;
  if(Cl->nskp == 0 && Cl->ndo < 10 )  
     Cl->ndo = dir*MAXNUM;
  if(Cl->ndo == 0) 
    {
    *istat = 5;
    Cl->ntot = 0;
    return;
    }

  ftr = (float *) malloc(Glbls->ndptr  * sizeof(float));
  if(!ftr)
    {
    *istat = 1;
    return;    
    }

  if (!strcmp(Glbls->ftyp,"JSEIS")) {
    isa_jseis_file = 1;
    if (open_file != 0) {
      fprintf (stderr, "read_trcio_data - no javaseis support\n");
      *istat=7;
      return; /* added this dec 2009 wmm */
      *lun = jsfiles_getlun (name, "r");
      if (*lun <= 0) {
	*istat = 7;
	return;
      }
      jsfiles_open (*lun);
      *istat = jsfiles_status (*lun);
      if (*istat != 0) return;
    }
  }
  else {
    isa_jseis_file = 0;
  }

  if(abs(Cl->ndo)> Cl->ntot ) Cl->ndo = dir * Cl->ntot;
  trace_count= 0;
  n1         = Cl->iskp + 1;   /* n1 is starting trace of a group */
  n2         = n1 + dir*(abs(Cl->ndo)-1);
  hit_end    = 0;
  ngrps      = (Cl->ntot - 1)/abs(Cl->ndo) + 1;
  for(i = 1; i <= ngrps; i++) 
    {
    if(n2 < 1 || n2 > Glbls->ntrfil) hit_end = 1;
    num = abs(Cl->ndo);
    if(!num)
      {
      *istat = 2;
      return;
      }
    num = (Cl->ntot - (i-1) * num < num) ? Cl->ntot - (i - 1) * num : num;

    for(j = 0; j < num; j++)
      {
      trace_num = n1 + j;
      hdr_offset = trace_count * Glbls->nhdwd;
      tr_offset  = trace_count * Glbls->ndptr;
      trace_out_offset = trace_count * Cl->nsamp;

      if (!isa_jseis_file) {
        trciof77wrapper_get_trace_(name, &open_file, &dhd[hdr_offset],
          &ftr[0], &trace_num, lun, istat, &Glbls->nhdwd, &Glbls->ndptr);
      }
      else {
        jsfiles_settracenumber (*lun, trace_num);
	jsfiles_gettrace (*lun, ftr, Glbls->ndptr);
	*istat = jsfiles_status (*lun);
	if (*istat == 0) {
          jsfiles_getheaders (*lun, &dhd[hdr_offset], Glbls->nhdwd);
	  *istat = jsfiles_status (*lun);
	}
      }

      open_file = 0;
      if(*istat != 0) 
        {
        free(ftr);
        return;
        }
      TASF[trace_count] = 1.0;/*Use 1.0 for trace scalers*/

      /*Convert double precision headers into the float header array*/
      for(k = 0; k < Glbls->nhdwd; k++)
        fhd[k + hdr_offset] = (float)dhd[k + hdr_offset];

      /*Data requested may be a window or decimated samples 
       *so get only the requested data
       */
      for(m = Cl->samp1 - 1, n = 0; m < Cl->nsamp * Cl->sdec + Cl->samp1 - 1; 
          m += Cl->sdec, n++) 
        {
        float_trace_out[trace_out_offset + n] = ftr[m];
        }
      trace_count++;
      }      

    n1 +=  dir*(abs(Cl->ndo) - 1) + Cl->nskp + 1;
    if(n1 < 1 || n1 > Glbls->ntrfil || hit_end > 0) 
      i = ngrps + 1;/*done*/
    n2 = n1 + dir * (abs(Cl->ndo) - 1);
    }



  free(ftr);

  return;   
}





int read_trcio_cubeslice (char *name, IO_request *ioreq, TF_Global *g,
    char *hd, char *tr, float *TASF, int *lun)
{ 
  int hdr_offset, trace_out_offset;
  int istat;
  float *ftr, ftmp;
  double *dhd;
  int axis, index, ftype, jump, count;
  int n1, n2, n3, k2, k3, k4;
  int trace, nhw, ns, stride;
  int ISOK=1, NOTOK=0;
  float *fhd             = (float *)hd;
  float *float_trace_out = (float *)tr;
  int open_file = 1;
  int isa_jseis_file;


  axis  = ioreq->axis;
  if (!name) return NOTOK;
  if (!g )   return NOTOK;
  if (tf_global_is3d(g) == 0) return NOTOK;
  if (axis < 1 || axis > 3) return NOTOK;

  if ((int)tf_global_get_ntrcll(g) > 1) return NOTOK;
  index = ioreq->index;
  ftype = getgl_str_to_ftype (tf_global_ftype(g));
  if (ftype != TROT_TYPE && ftype != QTROT_TYPE &&
      ftype != SEGY_TYPE && ftype != JSEIS_TYPE   ) return NOTOK;
  tf_global_grid_sizes (g, &n1, &n2, &n3);
  nhw = g->nhdwd;


  if (ftype == SEGY_TYPE) {
    g->trmaxg = (float)cube_trcio_getTrmaxg (cube);
  }

  if (!strcmp(g->ftyp,"JSEIS")) {
    isa_jseis_file = 1;
      fprintf (stderr,"read_trcio_cubeslice. javaseis not supported\n");
      return NOTOK;
    if (open_file != 0) {
      fprintf (stderr,"read_trcio_cubeslice.\n");
      *lun = jsfiles_getlun (name, "r");
      if (*lun <= 0) {
	return NOTOK;
      }
      jsfiles_open (*lun);
      istat = jsfiles_status (*lun);
      if (istat != 0) return NOTOK;
    }
  }
  else {
    isa_jseis_file = 0;
  }
  
  switch (axis) {

   case 3:
   case 2:

     if (axis == 3) { /* inline */
       jump  = 1;
       count = n2;
       trace = index * n2 + 1; /* offset to starting trace */
     }
     else if (axis == 2) { /* crossline */
       jump  = n2;
       count = n3;
       trace = index + 1; /* offset to starting trace */
     }
     else {
       return NOTOK;
     }

     ftr = (float *)malloc (g->ndptr*sizeof(float));
     if (!ftr) return NOTOK;

     dhd = (double *)malloc (nhw*sizeof(double));
     if (!dhd) {
       free (ftr);
       return NOTOK;
     }

     hdr_offset       = 0;
     trace_out_offset = 0;
     for (k2 = 0; k2 < count; k2++) {

       if (!isa_jseis_file) {
         trciof77wrapper_get_trace_ (name, &open_file, &dhd[0], &ftr[0],
           &trace, lun, &istat, &nhw, &g->ndptr);
       }
       else {
         jsfiles_settracenumber (*lun, trace);
	 jsfiles_gettrace (*lun, ftr, g->ndptr);
	 istat = jsfiles_status (*lun);
	 if (istat == 0) {
           jsfiles_getheaders (*lun, dhd, nhw);
	   istat = jsfiles_status (*lun);
	 }
       }

       if (istat) {
	 if (!isa_jseis_file) {
           trciof77wrapper_close_file_ (lun, &istat);
	 }
	 else {
           /*printf ("read_trcio_cubeslice: calling jsfiles_close\n");*/
	   jsfiles_close (*lun);
	 }
         free (ftr);
         free (dhd);
         return NOTOK;
       }
       open_file = 0;

       if (ftype == SEGY_TYPE) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the 8th HWD (Y Grid mid-point) for SEGY as temp fix
 */
         if (dhd[7] == 0) dhd[7] = dhd[2];
       }

       TASF[k2] = 1.0; /* use 1.0 for trace scalers */

/* Convert double precision headers into the float header arrays */
       for (k3 = 0; k3 < nhw; k3++) {
         fhd[k3+hdr_offset] = (float)dhd[k3];
       }

/*Data requested may be a window or decimated samples 
 *so get only the requested data
*/
       stride = (ioreq->samp1 - 1) * ioreq->sdec;
       for (k3 = ioreq->samp1-1, k4 = 0; k3 < ioreq->nsamp+ioreq->samp1-1;
         k3++, k4++) {
         float_trace_out[trace_out_offset+k4] = ftr[stride];
         stride += ioreq->sdec;
       }
       trace            += jump        ;
       hdr_offset       += nhw         ;
       trace_out_offset += ioreq->nsamp;
     }

     /*trciof77wrapper_close_file_ (&lun, &istat);*/
     free (ftr);
     free (dhd);
     return ISOK;

   case 1:

/* only allow complete Z-slices are read */
     if (cube_trcio_csvFileExists(cube)) {
/* trnsps temporarily opposite for TROT files!!! */
       if (!cube_trcio_readCsvSection(cube,ioreq->index,!(ioreq->trnsps),
         (float*)tr)) {

/* allocate to read headers */
         dhd = (double *)malloc (nhw*sizeof(double));
         if (!dhd) return NOTOK;

         if (!(ioreq->trnsps == 1)) {
/* trnsps temporarily opposite for TROT files!!! */
/* this IS the transpose case */
           ioreq->ntot  = n3;
           jump         = n2;
         }
         else {
           ioreq->ntot  = n2;
           jump         = 1 ;
         }

         ns  = 0       ;

         trace = 1;
         for (k2 = 0; k2 < ioreq->ntot; k2++) {
	   if (!isa_jseis_file) {
             trciof77wrapper_get_trace_ (name, &open_file, &dhd[0], &ftr[0],
               &trace, lun, &istat, &nhw, &ns);
	   }
	   else {
             jsfiles_settracenumber (*lun, trace);
	     jsfiles_getheaders (*lun, dhd, nhw);
	     istat = jsfiles_status (*lun);
	   }

	   if (istat) {
             free (dhd);
	     if (!isa_jseis_file) {
	       trciof77wrapper_close_file_ (lun, &istat);
	     }
	     else {
               /*printf ("read_trcio_cubeslice: calling jsfiles_close\n");*/
	       jsfiles_close (*lun);
	     }
             return NOTOK;
           }
           open_file = 0;

           if (ftype == SEGY_TYPE) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the 8th HWD (Y Grid mid-point) for SEGY as temp fix
 */
             if (dhd[7] == 0) dhd[7] = dhd[2];
           }

/* store the headers for the first row in order to define the X-coordinate
 *   of the Z-slice
 */
           hdr_offset = k2 * nhw;
           for (k3 = 0; k3 < nhw; k3++) {
             fhd[k3+hdr_offset] = (float)dhd[k3];
           }
           if (!(ioreq->trnsps == 1)) {
/* trnsps temporarily opposite for TROT files
 * this IS the transpose case
 * flip header words 7 & 8
 */
             ftmp              = fhd[6+hdr_offset];
             fhd[6+hdr_offset] = fhd[7+hdr_offset];
             fhd[7+hdr_offset] = ftmp;
/* flip header words 17 & 18 */
             ftmp               = fhd[16+hdr_offset];
             fhd[16+hdr_offset] = fhd[17+hdr_offset];
             fhd[17+hdr_offset] = ftmp;
/* flip header words 37 & 38 */
             ftmp               = fhd[36+hdr_offset];
             fhd[36+hdr_offset] = fhd[37+hdr_offset];
             fhd[37+hdr_offset] = ftmp;
           }
           trace += jump;

/* return 1.0 for trace scalers for each column of slice */
           TASF[k2] = 1.0;
         }

         /*trciof77wrapper_close_file_ (lun, &istat);*/
         free (dhd);
         return ISOK;
       }
     }
     return NOTOK;
/* DO NOT ALLOW BRUTE FORCE READ
 * auxiliary file does not exist so retrieve Z-slice by brute force 
     ioreq->samp1 = 1 ;
     if (!(ioreq->trnsps == 1)) {
 * trnsps temporarily opposite for TROT files!!! 
 * this IS the transpose case 
       ioreq->nsamp = n2;
       ioreq->ntot  = n3;
       next         = 1 ;
       jump         = n3;
     }
     else {
       ioreq->nsamp = n3;
       ioreq->ntot  = n2;
       next         = n2;
       jump         = 1 ;
     }

 * allocate to make reads 
     ftr = (float *)malloc (g->ndptr*sizeof(float));
     if (!ftr) return NOTOK;

     dhd = (double *)malloc (nhw*sizeof(double));
     if (!dhd) {
       free (ftr);
       return NOTOK;
     }

     k4 = 0;
     ns = index + 1;
     for (k2 = 0; k2 < ioreq->nsamp; k2++) {
       trace = k4 + 1;
       out   = k2;
       for (k3 = 0; k3 < ioreq->ntot; k3++) {
         trciof77wrapper_get_trace_ (name, &open_file, &dhd[0], &ftr[0],
           &trace, &lun, &istat, &nhw, &ns);

         open_file = 0;
	 if (istat) {
           free (ftr);
           free (dhd);
           trciof77wrapper_close_file_ (&lun, &istat);
           return NOTOK;
         }

         if (k2 == 0) {
 *
 * store the headers for the first row in order to define the X-coordinate
 *   of the Z-slice
 
           if (ftype == SEGY_TYPE) {
 * this is a cluge but force the 3rd HWD (current group number) into
 * the 8th HWD (Y Grid mid-point) for SEGY as temp fix
 
             if (dhd[7] == 0) dhd[7] = dhd[2];
           }
           hdr_offset = k3 * nhw;            *use g->nhdwd later 
           for (k5 = 0; k5 < nhw; k5++) {    *use g->nhdwd 
             fhd[k5+hdr_offset] = (float)dhd[k5];
           }
           if (!(ioreq->trnsps == 1)) {
 * trnsps temporarily opposite for TROT files 
 * this IS the transpose case 
 * flip header words 7 & 8 
             ftmp              = fhd[6+hdr_offset];
             fhd[6+hdr_offset] = fhd[7+hdr_offset];
             fhd[7+hdr_offset] = ftmp;
 * flip header words 17 & 18 
             ftmp               = fhd[16+hdr_offset];
             fhd[16+hdr_offset] = fhd[17+hdr_offset];
             fhd[17+hdr_offset] = ftmp;
 * flip header words 37 & 38 
             ftmp               = fhd[36+hdr_offset];
             fhd[36+hdr_offset] = fhd[37+hdr_offset];
             fhd[37+hdr_offset] = ftmp;
           }
         }

         float_trace_out[out] = ftr[index];
         out   += ioreq->nsamp;
         trace += jump;
       }
       k4 += next;
    }

    for (k2 = 0; k2 < ioreq->ntot; k2++) {
 * return 1.0 for trace scalers for each column of slice 
      TASF[k2] = 1.0;
    }

    trciof77wrapper_close_file_ (&lun, &istat);
    free (ftr);
    free (dhd);
    return ISOK;
 * END OF BRUTE FORCE READ
 */ 
  }
  return NOTOK; /* failure if we get here */
}




int read_data_open(int *lun, char *name, TF_Global *g, char *msg) {
 int  ierr, l;
 int  op_stat;
 char fname[120];
 char *ftype=0;
 int  trot_lun = -1;
 *lun = -1;
 

 ftype = tf_global_ftype(g);

 g->lun = -1;

 strcpy(fname,name);
 strcpy(g->ftyp,"UNKNOWN"); /* Dont know file type */


/********************************************************
 *** This note only for old byte types, trcio types   ***
 *** are not kept in a database.                      ***
 *** Check to see if fname is open already.           *** 
 *** If it doesnt exist or is not active:             ***
 *** 1. open the file and get the glbls information.  ***
 *** 2. close the file( reopen with a call to tf_open)***
 *** If it is already open get_global_data_ will leave***
 ***    it open, and return the negative of the unit  ***
 ***    number for the error status. Glbls will be set***
 *******************************************************/
 l = get_global_data_(fname,g,&ierr);

 /*The following will allow trot file types to be read 
   before they are complete*/
 if(!strcmp(g->ftyp,"TROT") || !strcmp(g->ftyp,"DSEGY") ||
    !strcmp(g->ftyp,"JSEIS"))
   {
   if(ierr == MAYBE_INCOMPLETE_FILE)
     {
     l = ierr = 0;
     }
   }

 if(ierr > 0){
   sprintf(msg,"read_data_open: ierr=%d from get_global_data",ierr);
   tf_global_prhd_(g);
   return 1;
 }
 
 /*If no error and we hava a trcio type we are done*/
 if(ierr==0) {
   if(!strcmp(g->ftyp,"TROT") || !strcmp(g->ftyp,"DSEGY") ||
      !strcmp(g->ftyp,"JSEIS"))
      {
      *lun = g->lun;
      strcpy(msg,"OK");
      return 0;
      }
 }

 strcpy(fname,tf_global_data_file(g));/*Save data file name*/

 if(read_data_types(g) !=0) {
   strcpy(msg,"read_data_open: invalid file for read_data");
   return 2;
 }
/******************** OPEN FILE *************************
 * 1. tf_open is aware of files that are already open *** 
 * 2. bypass tf_open if we know the file is open.     ***
 *******************************************************/
 if(ierr==0) {
   op_stat=0;
   tf_open_(lun, fname,g,&op_stat,msg );
   if(strncmp(msg,"OK",2)!=0 && strncmp(msg,"03",2)!=0) {
    sprintf(msg,"read_data_open: tf_open problem\nread_data: msg=%s",msg);
    return 6;
   }
  if(trot_lun != -1)
     *lun = trot_lun; 
 }
 else *lun = g->lun; /* file already open */
 return 0;
}

int read_data_types(TF_Global *g) {
 int  ites=1;
 char *ftype=0;
 ftype = tf_global_ftype(g);
 if(strcmp(ftype,getgl_ftype_to_str(TFILE_TYPE))==0)  ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(TF3D_TYPE))==0)   ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(TF3DF_TYPE))==0)  ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(STROT_TYPE))==0 ) ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(QTROT_TYPE))==0 ) ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(DTROT_TYPE))==0 ) ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(TROT_TYPE))==0 ) ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(SEGY_TYPE))==0  ) ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(VOXET_TYPE))==0 ) ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(BRICK_TYPE))==0 ) ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(HGRID_TYPE))==0 ) ites=0;
 if(strcmp(ftype,getgl_ftype_to_str(HG3DL_TYPE))==0 ) ites=0;
 return ites;
}

int read_data_translate(IO_request *ioreq, TF_Global *g,
          char *hd, char *tr, float *TASF) {
 int  nconh, ncont;
 int  n;
 extern int wrdc_get_mach();
 int   lw;
 lw = wrdc_localwd_();
/****************************************************************
 * Convert file in foreign word format to native float words  ***
 ***************************************************************/
 TASF[ioreq->ntot] = 0.0;
 nconh = g->nhdwd;
 ncont = ioreq->nsamp;
 n = read_data_buffer(ncont);
 if(ncont > rdbufsiz/sizeof(float) ) return 2;

 if(strcmp(g->ftyp,getgl_ftype_to_str(TFILE_TYPE)) == 0 ||
    strcmp(g->ftyp,getgl_ftype_to_str(TF3D_TYPE))==0 ||
    strcmp(g->ftyp,getgl_ftype_to_str(TF3DF_TYPE))==0 ) {
    if(read_data_tfile(ioreq,  g, hd, tr,TASF)) return 0;
    return 4;
 }

 /*This should no longer be called. read_data_ should call the new
   cps trcio functions instead
 if(strcmp(g->ftyp,"QTROT") == 0 ) {
    read_data_trot(ioreq, g, hd, tr,TASF);
 }
 */

 if(strcmp(g->ftyp,"STROT") == 0 ||
    strcmp(g->ftyp,"DTROT") == 0) {
    if(wrdc_get_mach() != 1 && wrdc_get_mach()!=8) return 2;
    read_data_trot(ioreq, g, hd, tr,TASF);
 }

 if(strcmp(g->ftyp,getgl_ftype_to_str(SEGY_TYPE)) == 0 ) {
   read_data_segy(ioreq, g, hd, tr,TASF);
 }

 if(strcmp(g->ftyp,getgl_ftype_to_str(BRICK_TYPE)) == 0 ) {
    read_data_voxet(ioreq, g,  hd, tr,TASF);
 }

 if(strcmp(g->ftyp,getgl_ftype_to_str(VOXET_TYPE)) == 0 ) {
    read_data_voxet(ioreq, g,  hd, tr,TASF);
 }

 if(strcmp(g->ftyp,getgl_ftype_to_str(HGRID_TYPE)) == 0 ) {
    read_data_hgrid(ioreq, g,  hd, tr,TASF);
 }

 if(strcmp(g->ftyp,getgl_ftype_to_str(HG3DL_TYPE)) == 0 ) {
    read_data_hgrid(ioreq, g,  hd, tr,TASF);
 }

 return 0;
}

/********************************************************
 * STROT or DTROT file on the Cray?                   ***
 * Expand the packed headers to native floating point ***
 * ?TROT files have headers and data packed 2:1 .     ***
 * Header 25 is lav for DTROT & STROT files.          ***
 *******************************************************/
void read_data_trot(IO_request *ioreq, TF_Global *g,
     char *hd, char *tr, float *TASF) {
 long one=1,zero=0,ncont;
 int mstride=1, mstrt=0, i,m,n;
 int ih,oh,it,ot,lw,wrdsiz,hdtyp;
 int istat,nconh,sf;
 float *hdr, hd25=0;
 char  msg[96];
 lw = wrdc_localwd_();

 TASF[ioreq->ntot] = 1.0;
 i=8;
 sf=sizeof(float);
 wrdsiz= g->nbydp;
 nconh = g->nhdwd;
 ncont = ioreq->nsamp;
 for(n=ioreq->ntot;n>0;n--) {
   ih =  wrdsiz*(n-1)*g->nhdwd;
   oh =  sizeof(float)*(n-1)*HDROUT;
   it = wrdsiz*(n-1)*ioreq->nsamp;
   ot = sizeof(float)*(n-1)*ioreq->nsamp;
   TASF[n-1] = 1.0;
   if(strcmp(g->ftyp,"QTROT") ==0) {
       hdtyp=WIEEE;
       istat = wrdc_fcnvrt_(&hdtyp,hd+ih,&lw,fbuf,
              &nconh,msg);
       memcpy(hd+oh,fbuf,nconh*sf);
       memcpy(fbuf,tr+it,ncont*wrdsiz); /*IEG2CRAY needs word bndry */
       istat = wrdc_fcnvrt_(&hdtyp,fbuf,&lw,tr+ot,
              &ncont,msg);
   }
   if(strcmp(g->ftyp,"STROT") ==0) {
       wrdc_exp21_(&g->nhdwd, (hd+ih), (hd+oh));
       memcpy(fbuf,tr+it,ncont*wrdsiz); /* EXPAND1 needs word bndry */
       wrdc_exp21_(&ioreq->nsamp, (char *) fbuf, (tr+ot));
   }
   float_data_lav_(tr+ot,&zero, &one, &ncont,&hd25);
   memcpy(hd+oh+(LAVHDR-1)*sizeof(float),&hd25,sf);
   if(ioreq->samp1>1 && muteadj==1) {/* fix mutes for time windows */
     m = sf*( (n-1)*HDROUT + MUTHDR-1); hdr = (float *) (hd+m);
     *hdr = (*hdr - ioreq->samp1 + 1)/ioreq->sdec;
     if(*hdr < .999) *hdr = 1.0;
     m = sf*( (n-1)*HDROUT + TAILHDR-1); hdr = (float *) (hd+m);
     *hdr = (*hdr - ioreq->samp1 + 1)/ioreq->sdec;
     if(*hdr < .999) *hdr = 1.0;
   }
 }
 return;
}

int  read_data_tfile(IO_request *Cl,  TF_Global *g,
                     char *hd, char *tr,float *tasf) {
  long   one=1,zero=0,ncont;
  int    i,m,n,sizf,wrdsiz;
  int    istat, ih,oh,it,ot, nconh, lw, wdtyp, hdtyp;
  float  *hdr,hd25,*hi=0,*ti=0,bigh25=0.,lav;
  char   msg[80],*to=0;
  lw = wrdc_localwd_();
 /* convert words to native f.p. format */
  if(!g) return 0;
  wdtyp = (int) g->wdtyp;
  if(Cl->index < 0) wdtyp = WIEEE; /* headers stored as traces */
  hdtyp = wdtyp;
  if(wdtyp==WBYTE) hdtyp=WIEEE;
  if(wdtyp==WSBYT) hdtyp=WIEEE;
  wrdsiz= (int) g->nbydp;
  if(Cl->index<0) wrdsiz = 4;
  tasf[Cl->ntot] = 1.0;
  nconh = g->nhdwd;
  ncont = Cl->nsamp;
  sizf  = sizeof(float);
  if(sizeof_real_() != sizeof(float)) {
  }

  for(n=Cl->ntot;n>0;n--)
   {ih = g->nbyhd*(n-1)*g->nhdwd;
    it = wrdsiz*(n-1)*Cl->nsamp;
    oh = sizf*(n-1)*HDROUT;
    ot = sizf*(n-1)*Cl->nsamp;
/* RSD
    if(wdtyp==WIEEE && lw==WIEEE) { 
     if(wrdsiz > 1 ) { i = bswap_(&ncont,(unsigned char *)  tr+it); }
    }
    if(lw==WIEEE) {
      if(g->nbyhd > 1 ) { i = bswap_(&nconh,(unsigned char *)  hd+ih); }
    }
*/
    istat = wrdc_fcnvrt_(&hdtyp,hd+ih,&lw,fbuf,
            &nconh,msg);
    memcpy(hd+oh,fbuf,nconh*sizf);
    if(wrdsiz>1) {/* only convert non-bytes */
    istat = wrdc_fcnvrt_(&wdtyp,tr+it,&lw,fbuf,
            &ncont,msg);
    memcpy(tr+ot,fbuf,ncont*sizf);
    }

    if(istat !=0) 
     { printf("read_data_tfile: %s\n",msg);
       return 0;
     }
   }

  if(g->nhdwd>25 ) /* Compute and save the trace lav */
   {
        for(n=Cl->ntot;n>0;n--)
         {oh = sizf*(n-1)*g->nhdwd;
          lav = *(float *) (hd + oh + (LAVHDR-1)*sizf);
          tasf[n-1] = lav/127.0;
          if(n==Cl->ntot) tasf[n] = tasf[n-1]; 
            if(g->nbydp > 1)
              {ot = sizf*(n-1)*Cl->nsamp;
               float_data_lav_(tr+ot,&zero, &one, &ncont,&hd25);
               tasf[n-1]=1.0;
              }
            else
              {ot = (n-1)*Cl->nsamp;
               byte_data_lav_(tr+ot,&zero,&one,&ncont,&hd25);
/*
               if(Cl->axis==1) {
                 tasf[n-1]=g->trmaxg/127.0;
               }
*/
               hd25 = lav *(hd25/127.0);
              }
            if(tasf[n-1] > tasf[Cl->ntot]) tasf[Cl->ntot] = tasf[n-1]; 
            memcpy(hd+oh+(LAVHDR-1)*sizf,&hd25,sizf);
            if(Cl->samp1>1 && muteadj==1) { /* fix mutes for time windows */
              m = sizf*( (n-1)*HDROUT + MUTHDR-1);
              hdr = (float *) (hd+m);
              *hdr = (*hdr - Cl->samp1 + 1)/Cl->sdec;
              if(*hdr < .999) *hdr = 1.0;
              m = sizf*( (n-1)*HDROUT + TAILHDR-1);
              hdr = (float *) (hd+m);
              *hdr = (*hdr - Cl->samp1 + 1)/Cl->sdec;
              if(*hdr < .999) *hdr = 1.0;
            }
         }
   }

 return 1;
}

/* return 0 for failure & 1 for success */
int read_data_wfix(int ntot, int nsamp, float *hd, 
    int wdi, char *bi, int wdo, char *bo) {
 char  *to=0;
 float *ti=0,*hi=0,lav,*wrk;
 int   i,n,lw,sizi=4,sizo=4;
 char  msg[80];
 lw = wrdc_localwd_();
 /* Input type has to be byte or native float */
 if(wdi!=lw && wdi!=WBYTE && wdi!=WSBYT) return 0;
 if(wdi==WBYTE || wdi==WSBYT) sizi = 1;
 if(wdi==WIBM2) sizi = 2;
 if(wdi==lw)    sizi = sizeof(float);
 if(wdo==WBYTE || wdo==WSBYT) sizo = 1;
 if(wdo==WIBM2) sizo = 2;
 if(wdo==lw)    sizo = sizeof(float);
 if(wdo==WBYTE || wdo==WSBYT) {
   hi = hd;
   if(wdi==wdo) {
     if(bo!=bi) memcpy(bo,bi,ntot*nsamp);
     return 1;
   } 
   if(wdi==WSBYT) {
    wrdc_sbtousb(ntot*nsamp,(char *) bi, (unsigned char *) bo);
   }
   if(wdi==WBYTE) {
    wrdc_usbtosb(ntot*nsamp,(unsigned char *) bi, (char *) bo);
   }
   ti = (float *) bi;
   to = bo;
   for(n=0;n<ntot;n++) {
      lav = fabs(ti[0]);
      for(i=0;i<nsamp;i++)
       lav = (fabs(ti[i]) > lav) ? fabs(ti[i]) : lav;
      hi[LAVHDR-1] = lav;
      if(wdo==WBYTE)
        float_to_byt_(ti,&nsamp,(unsigned char *) to, &nsamp, &lav);
      else
        float_to_bytsgn(ti,&nsamp, (unsigned char *) to, &lav);
      to += nsamp;
      ti += nsamp;
      hi += HDROUT;
   }
   return 1;
 } else {
   if(wdi==wdo) {
     if(bo!=bi) memcpy(bo,bi,ntot*nsamp*sizi);
     return 1;
   }
   if(wdi==WBYTE || wdi==WSBYT) {
     hi = hd + (ntot-1)*HDROUT;
     to = bo + (ntot-1)*nsamp*sizo;
     wrk = (float *) malloc(nsamp*sizeof(float));
     if(!wrk) return 0;
     for (n=ntot;n>0;n--) {
        lav = hi[LAVHDR-1];
        byte_to_float_((unsigned char *) bi+(n-1)*nsamp,&nsamp,
        &nsamp, &lav, wrk);
        i= wrdc_fcnvrt_(&lw,(char *)wrk,&wdo,to, 
            &nsamp,msg);
        to -= nsamp*sizo;
        hi -= HDROUT;
        if(i!=0) { free(wrk); return 0;}
     }
     if(wrk) free(wrk);
     return 1;
   }
   ti = (float *) (bi+sizi*nsamp*(ntot-1));
   to = (bo+sizo*nsamp*(ntot-1));
   for(n=ntot;n>0;n--) {
    i= wrdc_fcnvrt_(&wdi,(char *)ti,&wdo,to, 
            &nsamp,msg);
    if(i!=0) return 0;
    ti -= nsamp*sizi;
    to -= nsamp*sizo;
    if(i==1) return 0;
   }
   return 1;
 }
}

void read_data_segy(IO_request *ioreq, TF_Global *g,
     char *hd, char *tr,float *tasf)
{
  long   one=1,zero=0,ncont;
  int    i,n,sizf,wrdsiz;
  int    istat, ih,oh,it,ot, lw, wdtyp;
  float  cpshd[64],hd25,bigh25=0.,*fpntr;
  char   msg[80];
 /* convert from ibm words to native f.p. format */
  if(!g) return;
  wdtyp = (int) g->wdtyp;
  wrdsiz= (int) g->nbydp;
  lw = wrdc_localwd_();
  tasf[ioreq->ntot] = 1.0;
  
  ncont = ioreq->nsamp;
  sizf = sizeof(float);
  for(i=0;i<HDROUT;i++) { cpshd[i] = 0.0; }
  for(n=ioreq->ntot;n>0;n--)
   { ih = (n-1)*240;
     oh = (n-1)*sizf*HDROUT;
     it = (n-1)*wrdsiz*ioreq->nsamp;
     if(wrdsiz==1) /* don't convert bytes to float */
       ot = (n-1)*wrdsiz*ioreq->nsamp;
     else
       ot = (n-1)*sizf*ioreq->nsamp;
     wrdc_segy_tr_header((unsigned char *) hd+ih, cpshd);
     if(wdtyp != WBYTE && wdtyp != WSBYT) {
       memcpy(fbuf,tr+it,ncont*wrdsiz);
       istat = wrdc_fcnvrt_(&wdtyp,fbuf,&lw,tr+ot,
               &ncont,msg);
       if(cpshd[50] < 1.0) { /* apply scaling */
        fpntr = (float *) (tr+ot);
        for(i=0;i<ncont;i++) fpntr[i]= cpshd[50]*fpntr[i];
       }
       float_data_lav_(tr+ot,&zero, &one, &ncont,&hd25);
       tasf[n-1]=1.0;
     } else {/* find lav */
       if(wdtyp==WSBYT)
         wrdc_sbtousb(ncont, (char *) tr+it, (unsigned char *) tr+ot);
       byte_data_lav_( (unsigned char *) (tr+ot), &zero, &one,
        &ncont, &hd25);
       tasf[n-1]=hd25/127.0;
     }
   /*    memcpy(tr+ot,fbuf,ncont*sizf); */

     cpshd[0] = n;
     cpshd[1] = cpshd[1]/(1000.*g->srval*ioreq->sdec);
     cpshd[63]= cpshd[63]/(1000.*g->srval*ioreq->sdec);
     if(cpshd[63]<1 || cpshd[63]> ioreq->nsamp) cpshd[63]=ioreq->nsamp;
   /*  cpshd[63]= ioreq->nsamp; */
     cpshd[LAVHDR-1] = hd25;
     memcpy(hd+oh,cpshd,HDROUT*sizf);
     if(bigh25<hd25) bigh25 = hd25;
     if(tasf[n-1] > tasf[ioreq->ntot]) tasf[ioreq->ntot] = tasf[n-1]; 
   }
  g->trmaxg = bigh25; /* kluge around lack of true trmaxg */
}

void read_data_voxet(IO_request *ioreq, TF_Global *g,
     char *hd, char *tr,float *tasf)
{ Grid3DDesc *h = (Grid3DDesc *) g->h;
  long   one=1,zero=0,ncont;
  int    i,n,sizf;
  int    istat, oh,it,ot, lw;
  float  cpshd[64],hd25,bigh25=0.;
  char msg[80];
  /* convert from foreign words to native f.p. format */
   if(!g || !h) return;
   ncont = ioreq->nsamp;
   sizf = sizeof(float);
   lw = wrdc_localwd_();
   for(i=0;i<HDROUT;i++) { cpshd[i] = 0.0; }

   tasf[ioreq->ntot]=0.0;
   for(n=ioreq->ntot-1;n>=0;n--)
      {
       oh = n*sizf*HDROUT;
       it = n*g->nbydp*ncont;
       ot = n*sizf*ncont;
       if(g->wdtyp!= WBYTE && g->wdtyp != WSBYT)
        {/* convert word format and find lav */
         istat = wrdc_fcnvrt_(&g->wdtyp,tr+it,&lw,fbuf,
                &ncont,msg);
/* RSD
         if(lw==WIEEE) bswap_(&ncont, (unsigned char *) fbuf );
*/
         memcpy(tr+ot,fbuf,ncont*sizf);
         float_data_lav_(tr+ot,&zero, &one, &ncont,&hd25);
         tasf[n]=1.0;
        }
       else
        {/* find lav */
         byte_data_lav_( (unsigned char *) (tr+it), &zero, &one,
         &ncont, &hd25);
         tasf[n]=hd25/127.0;
        }

       cpshd[0] = n+1;
       cpshd[16]= h->O.v[1] + n*h->D.v[1]; 
       cpshd[17]= h->O.v[2] + n*h->D.v[2]; 
       /*   cpshd[1] = h->O.v[0] + (nsamp-1)*h->D.v[0];
       cpshd[16]= h->O.v[1] + n*h->D.v[1]; 
       cpshd[17]= h->O.v[2] + n*h->D.v[2]; 
       cpshd[10]= cpshd[16];
       cpshd[11]= cpshd[17];
       cpshd[13]= cpshd[16];
       cpshd[14]= cpshd[17];*/
       cpshd[63]= ioreq->nsamp;
       cpshd[LAVHDR-1] = hd25;
       if(bigh25<hd25) bigh25 = hd25;
       memcpy(hd+oh,cpshd,HDROUT*sizf);
       if(tasf[n] > tasf[ioreq->ntot] ) tasf[ioreq->ntot] = tasf[n]; 
      }
   
  g->trmaxg = bigh25; /* kluge around lack of true trmaxg */
}

void read_data_hgrid(IO_request *ioreq, TF_Global *g,
     char *hd, char *tr,float *tasf)
{ Grid3DDesc *h = (Grid3DDesc *) g->h;
  long   one=1,zero=0,ncont;
  int    i,n,sizf;
  int    istat, oh,it,ot, lw;
  float  cpshd[64],hd25,bigh25=0.;
  char   msg[80];
  /* convert from foreign words to native f.p. format */
   if(!g || !h) return;
   ncont = ioreq->nsamp;
   sizf = sizeof(float);
   lw = wrdc_localwd_();
   for(i=0;i<HDROUT;i++) { cpshd[i] = 0.0; }

   tasf[ioreq->ntot]=0.0;
   for(n=ioreq->ntot-1;n>=0;n--)
      {
       oh = n*sizf*HDROUT;
       it = n*g->nbydp*ncont;
       ot = n*sizf*ncont;
       if(g->wdtyp!= WBYTE && g->wdtyp != WSBYT)
        {/* convert word format and find lav */
         istat = wrdc_fcnvrt_(&g->wdtyp,tr+it,&lw,fbuf,
                &ncont,msg);
/* RSD
         if(lw==WIEEE) bswap_(&ncont, (unsigned char *) fbuf );
*/
         memcpy(tr+ot,fbuf,ncont*sizf);
         float_data_lav_(tr+ot,&zero, &one, &ncont,&hd25);
        }
       else
        {/* find lav */
         byte_data_lav_( (unsigned char *) (tr+it), &zero, &one,
         &ncont, &hd25);
        }

       cpshd[0] = n+1;
       cpshd[16]= h->O.v[0] + n*h->D.v[0]; 
       cpshd[17]= h->O.v[1] + n*h->D.v[1]; 
       /*   cpshd[1] = h->O.v[0] + (ncont-1)*h->D.v[0];
       cpshd[16]= h->O.v[0] + n*h->D.v[0]; 
       cpshd[17]= h->O.v[1] + n*h->D.v[1]; 
       cpshd[10]= cpshd[16];
       cpshd[11]= cpshd[17];
       cpshd[13]= cpshd[16];
       cpshd[14]= cpshd[17];*/
       cpshd[63]= ncont;
       cpshd[LAVHDR-1] = hd25;
       if(bigh25<hd25) bigh25 = hd25;
       memcpy(hd+oh,cpshd,HDROUT*sizf);
       tasf[n]=1.0;
       if(tasf[n] > tasf[ioreq->ntot] ) tasf[ioreq->ntot] = tasf[n]; 
      }
  g->trmaxg = bigh25; /* kluge around lack of true trmaxg */
   
}

/* return value is 1 if succes, 0 if failure */
int  read_data_fromdisk(int fd, IO_request *ioreq, TF_Global *g,
     char *hd, char *tr)
{int  fd_ = fd, n, stat=0, ftype;
 int  ISOK=1, NOTOK=0;
 char msg[160];

 if(fd_<0 || !g) return NOTOK;

 /* make sure request is sensible */
 if(tf_check_cntrl(g,ioreq) != 0) return NOTOK;

 if(ioreq->axis >0) {
  if(tf_global_is3d(g) ==0) return NOTOK;
  ftype = getgl_str_to_ftype(tf_global_ftype(g));
  if(ftype != BRICK_TYPE) {
    stat = read_data_cubeslice(fd_, ioreq, g , hd, tr);
  } else {
    stat = NOTOK;
#ifndef CRAY
/*
    stat = cube_util_get_slice(tf_global_header_file(g),ioreq,g,hd,tr);
*/
#endif
  }
  if(stat==NOTOK) return NOTOK;
 }
 else {
  if(ioreq->nskp != 0)
   { stat = tf_arr_rd_(&fd_, ioreq, hd, tr, msg); }
  else
   { n = ioreq->iskp + 1;
     stat = tf_tr_rd_(&fd_, &n, &ioreq->ntot,
               &ioreq->nsamp,&ioreq->samp1,&ioreq->sdec,
               &ioreq->trnsps, hd, tr, msg);
   }
  if(stat!= 0) return NOTOK;
 }

 return ISOK;
}

int read_data_cubeslice(int fd, IO_request *ioreq, TF_Global *g,
    char *hd, char *tr)
{ Grid3DDesc *h;
  Pt3Di *C,*c;
  int fd_=fd, ftype;
  int ISOK=1, NOTOK=0;
  int bypht, byphd, byptr, nbydp, ntord, nrd=0, n1,n2,n3,nhd;
  int nxr;
  int i=0,j,ni,no,itr,otr,ohd,jmp,iby;
  char *rbuf=0,*rbufi=0, v;
  int axis, index, i_err;
  long offset, offh;

  if(fd_<0) return NOTOK;
  if(!g )   return NOTOK;
  if(tf_global_is3d(g) ==0) return NOTOK;
  index = ioreq->index;
  axis  = ioreq->axis;
  if(axis<1 || axis>3) return NOTOK;
  ftype = getgl_str_to_ftype(tf_global_ftype(g));
  bypht = tf_global_byinht(g);
  byptr = tf_global_byintr(g);
  byphd = tf_global_byinhd(g);
  nbydp = tf_global_get_nbydp(g);
  if(tf_global_get_ntrcll(g) > 1) return NOTOK;
  tf_global_grid_sizes(g,&n1,&n2,&n3);
  
  ioreq->ntot = 0;
  switch (axis) {

   case 3:
    if(ftype==VOXET_TYPE || ftype==HGRID_TYPE ||
       ftype==TF3D_TYPE  || ftype==TF3DF_TYPE) {
     rbuf = (char *) malloc(n2*bypht);
     rbufi = rbuf;
     if(!rbuf) return NOTOK;
     offset= tf_global_get_grecsiz(g) + index *  n2 * bypht;
     ntord= n2*bypht;
     dskio_xxskrd_(&fd_, rbuf, &offset, &ntord, &i_err);
     if(i_err != 0) {
      printf("read_data_cubeslice: inline\n");
      printf("read only %d out of %d bytes\n",nrd,ntord);
      free(rbuf); return NOTOK;
     }
     jmp = ioreq->sdec * nbydp;
     otr = 0;
     ohd = 0; 
     no  = ioreq->nsamp*nbydp;
     for(i=0;i<n2;i++) {
       ioreq->ntot++;
       if(byphd>0 && hd) memcpy(hd+ohd, rbuf+i*bypht, byphd); 
       ohd += byphd;
       itr = byphd + (ioreq->samp1-1)*nbydp + i*bypht;
       /*    memcpy(tr+otr, rbuf + itr, no);  */
       for(j=0;j<nbydp;j++) {
        tf_bmov_(rbuf+itr+j, &jmp,tr+otr+j,&nbydp,&ioreq->nsamp);
       }
       otr += no;
     }
     free(rbuf);
     return ISOK;
    }

    if(ftype==BRICK_TYPE) {
     h = (Grid3DDesc *) g->h;
     C = tf3d_getN(h);
     c = tf3d_getBrick(h);
     return NOTOK;
    }

    return NOTOK;

   case 2:
    if(ftype==VOXET_TYPE || ftype==HGRID_TYPE ||
       ftype==TF3D_TYPE  || ftype==TF3DF_TYPE) {
     rbuf = (char *) malloc(n2*bypht);
     rbufi = rbuf;
     if(!rbuf) return NOTOK;
     offset= tf_global_get_grecsiz(g) + index * bypht;
     ntord= bypht;
     jmp = ioreq->sdec * nbydp;
     itr = byphd + (ioreq->samp1-1)*nbydp;
     otr = 0;
     ohd = 0; 
     no  = ioreq->nsamp*nbydp;
     for(i=0;i<n3;i++) {
       dskio_xxskrd_(&fd_, rbuf,&offset, &ntord, &i_err);
       if(i_err != 0) {
        printf("xline trace number=%d\n",i);
        printf("read only %d out of %d bytes\n",nrd,ntord);
        free(rbuf); return NOTOK;
       }
       ioreq->ntot++;
       offset += n2*bypht;
       if(byphd>0 &&hd) memcpy(hd+ohd, rbuf, byphd); 
       ohd += byphd;
       for(j=0;j<nbydp;j++) {
        tf_bmov_(rbuf+itr+j, &jmp,tr+otr+j,&nbydp,&ioreq->nsamp);
       }
       otr += no;
       if(rbuf != rbufi) {
        printf("io buffer was hit\n");
        printf("loop index=%d\n",i);
        printf("byphd=%d byptr=%d\n",byphd,byptr);
        printf("ohd=%d otr=%d\n",ohd,otr);
        free(rbuf); return NOTOK;
       }
     }
     free(rbuf);
     return ISOK;
    }

    if(ftype==BRICK_TYPE) {
     h = (Grid3DDesc *) g->h;
     C = tf3d_getN(h);
     c = tf3d_getBrick(h);
     return NOTOK;
    }

    return NOTOK;

   case 1:
    
    ioreq->samp1= 1;
    ioreq->nsamp= n2;
    if(ioreq->trnsps == 1) ioreq->nsamp= n3;
    if(ftype==VOXET_TYPE || ftype==HGRID_TYPE ||
       ftype==TF3D_TYPE || (ftype==TF3DF_TYPE && index<0) ) {
     offset= tf_global_get_grecsiz(g);
     dskio_xsk_(&fd_,&offset, &i_err);
     ntord= n2*bypht;
     rbuf = (char *) malloc(ntord);
     if(!rbuf) return NOTOK;

     for(j=0;j<n3;j++) { /* loop over inlines */
      dskio_xxrd_(&fd_, rbuf, &ntord, &i_err);
      if(i_err != 0) { free(rbuf); return NOTOK; }
     
      if(byphd>0 && hd) { /* get appropriate header data */
        if(ioreq->trnsps!=1) {
          memcpy(hd+ j*byphd, rbuf, byphd); 
        } else {
          if(j==0) {
           for(i=0;i<n2;i++) {
             memcpy(hd+ i*byphd,rbuf+i*bypht, byphd); 
           }
          }
        }
      }
      
      if(index >=0) {
       nxr = nbydp;
       itr = byphd + index * nbydp;
      } else {
       nxr = tf_global_get_nbyhd(g);
       itr = -(index+1) * nxr;
      }
      if(ioreq->trnsps!=1) { /* arrange the slice data */
       no = nxr;
       for(iby=0;iby<nxr;iby++)
        tf_bmov_(rbuf+itr+iby,&bypht,tr+j*n2*nxr+iby,&no,&n2);
       ioreq->ntot = j+1;
       ioreq->nsamp = n2;
      } else {
       no = n3*nxr;
       for(iby=0;iby<nxr;iby++)
        tf_bmov_(rbuf+itr+iby,&bypht,tr+j*nxr+iby,&no,&n2);
       ioreq->ntot  = n2;
       ioreq->nsamp = j+1;
      }
     }
     free(rbuf);
     return ISOK;
    }

    if(ftype==TF3DF_TYPE) {
     offset= tf_global_get_grecsiz(g);
     dskio_xsk_(&fd_,&offset, &i_err);
     nhd = n3;
     iby = n2*bypht;
     if(ioreq->trnsps==1) {
      nhd = n2;
      iby = bypht;
     }
     ohd = 0;
     for(j=0;j<nhd;j++) {     /* read in headers */
      offh = offset + j*iby;
      dskio_xxskrd_(&fd_, hd+ohd, &offh, &byphd, &i_err);
      if(i_err != 0) { return NOTOK; }
      ohd += byphd;
      ioreq->ntot++;
     }
     offset= tf_global_get_grecsiz(g) + n2*n3*bypht + n2*n3*index*nbydp;
     ntord= n2*n3*nbydp;
     if(ioreq->trnsps!=1) {  /* read in data slice */
       dskio_xxskrd_(&fd_, tr, &offset, &ntord, &i_err);
       if(i_err != 0) {
        printf("slice\n");
        printf("read only %d out of %d bytes\n",nrd,ntord);
        return NOTOK;
       }
     } else {
       rbuf = (char *) malloc(ntord);
       rbufi=rbuf;
       if(!rbuf) return NOTOK;
       dskio_xxskrd_(&fd_, rbuf, &offset, &ntord, &i_err);
       if(i_err != 0) {
        printf("t-slice\n");
        printf("read only %d out of %d bytes\n",nrd,ntord);
        free(rbuf); return NOTOK;
       }
       no = nbydp*n3;
       ni = nbydp;
       for(iby=0;iby<nbydp;iby++) {
        itr = iby;
        otr = iby;
        for(j=0;j<n3;j++) {
         tf_bmov_(rbuf+otr,&nbydp,tr+itr,&no,&n2);
         itr+=nbydp;
         otr+= n2*nbydp;
        }
       }
      ioreq->ntot = n2;
      ioreq->nsamp= n3;
      if(rbuf) free(rbuf);
     }
     for(i=0;i<n2*n3*nbydp;i++) v = tr[i];
     return ISOK;
    }

    if(ftype==BRICK_TYPE) {
#ifdef NEED_CAPITALS
     return NOTOK;
#else
     h = (Grid3DDesc *) g->h;
     C = tf3d_getN(h);
     c = tf3d_getBrick(h);
     return NOTOK;
#endif
    }

   default:
    i++;
  }
 return NOTOK; /* failure if we get here */
}

int read_data_hdslice_byname(char *fname, float *hd, int trnsps) {
 int i, ierr,iotype=0,opf,ufi= -1;
 TF_Global g;
 i = get_global_data_(fname,&g,&ierr);
 if(ierr > 0) return 0;
 opf = dskio_ordo_();
 dskio_xop_(&iotype,&ufi,fname,&opf,&ierr);
 g.lun = ufi;
 ierr= read_data_hdslice(ufi, &g, hd, trnsps);
 dskio_xxcl_(&ufi,&ierr);
 return ierr;
}

int read_data_hdslice(int fd, TF_Global *g, float *hd, int trnsps) {
 long offset;
  int ftype,x,y, i_err;
  int ISOK=1, NOTOK=0,i,n,lw,wdtyp,istat;
  int bypht, byphd, byptr, nbydp, nbyhd, ntord, nrd=0, n1,n2,n3;
  float *fpntr;
  char  msg[120];
  if(!g || !hd || fd < 0) return NOTOK;
  ftype = getgl_str_to_ftype(tf_global_ftype(g));
  if(ftype != TF3DF_TYPE) return NOTOK;
  bypht = tf_global_byinht(g);
  byptr = tf_global_byintr(g);
  byphd = tf_global_byinhd(g);
  nbydp = tf_global_get_nbydp(g);
  nbyhd = tf_global_get_nbyhd(g);
  if(tf_global_get_ntrcll(g) > 1) return NOTOK;
  tf_global_grid_sizes(g,&n1,&n2,&n3);

  offset= tf_global_get_grecsiz(g) + n2*n3*bypht + n1*n2*n3*nbydp;
 /* ntord= n2*n3*nbyhd; need space for transpose*/
  n = n2*n3;
  i = read_data_buffer(n);
  ntord= n2*n3*nbyhd;
  dskio_xxskrd_(&fd,(char *) fbuf,&offset, &ntord, &i_err);
  if(i_err != 0) { return NOTOK; }
  lw = wrdc_localwd_();
/* RSD
  if(lw==WIEEE) {
    if(nbyhd > 1 ) { i = bswap_(&n,(unsigned char *)  fbuf); }
  }
*/
  wdtyp = (int) g->wdtyp;
  if(wdtyp==WBYTE || wdtyp==WSBYT) wdtyp=WIEEE;
  istat = wrdc_fcnvrt_(&wdtyp,fbuf,&lw,(char *) hd,
          &n,msg);
  if(trnsps) {
   fpntr = (float *) fbuf;
   for(x=0;x<n2;x++) {
    for(y=0;y<n3;y++) {
     fpntr[y+x*n3] = hd[x+y*n2];
    }
   }
   for(i=0;i<n;i++) hd[i] = fpntr[i] / 127.0;
  } else {
   for(i=0;i<n;i++) hd[i] = hd[i] / 127.0;
  }
  return ISOK;
}

int read_data_buffer(int nwd) {
 int  bsiz;
 if(nwd<=0) {
   if(fbuf) free(fbuf);
   fbuf=0;
   rdbufsiz=0;
   return rdbufsiz;
 }
 if(fbuf == NULL || (nwd > rdbufsiz/sizeof(float) ) ) {
   bsiz = nwd*sizeof(float);
   if(bsiz < 20000) bsiz = 20000;
   bsiz = 8*((bsiz+7)/8);
   if(fbuf != NULL) { free(fbuf); fbuf = NULL; }
   fbuf = (char *) calloc(1,bsiz);
   rdbufsiz = bsiz;
 }
 return rdbufsiz;
}

void read_data_madj(int toggle)
{
 muteadj=toggle;
}
int  read_dataMadj()
{ return muteadj; }

/*  
C\USER DOC
C  
C int  read_data_group(int *lun, char *name,
C      IO_request *Cl, char *hd, char *tr, float *tasf,
C      int mgrp, int hkey, float *gval, float eps);
C
C    lun   O   returned file descriptor
C    name  I   File name
C    Cl    I-O specifies initial trace skip, window
C    hd    O   receives headers
C    tr    O   receives traces
C    tasf  O   trace amplitude scale factors
C    mgrp  I   maximum number of traces to include
C    hkey  I   header word to use for group key
C    gval  O   value of hd[hkey] for the group
C    eps   I   same group if abs(hd[hkey]-old)<eps
C
C NOTES:
C 1. read_data_group returns the group trace count
C    A return value of 0 indicates a problem.
C 2. Some file types do not have trace headers.
C 3. On entry
C    Cl->iskp+1 is the initial trace to read.
C    Cl->nsamp  is the samples/trace to return
C    Cl->sdec   is the sample decimation factor
C    On return
C    Cl->ntot   contains the group size
C 4. lun is returned as the open file descriptor. If
C    the file, name, is already open lun will be set
C    to the active f.d.
C
C\END DOC
*/
int  read_data_group(int *lun, char *name, IO_request *Cl,
     char *hd, char *tr, float *tasf,int mgrp,
     int hkey, float *gval, float eps)
{char  msg[80];
 char *hdpntr, *trpntr;
 int  n1, ngrp = 0, endgroup,hdsize,trsize; 
 int  i, istat, ierr;
 float hval= -999.0,hold;
 int  cl_stat;
 TF_Global Glbls;

 Cl->axis=0;
 Cl->ntot=0;
 Cl->ndo=1;
 Cl->nskp=0;
 n1 = Cl->iskp;
 if(hkey<1 || hkey > HDROUT) return ngrp;
/* open file if unopen, retrieve globals */
 istat = read_data_open(lun, name, &Glbls, msg);
 if(istat !=0) goto error;

 endgroup=0;
 hdpntr= hd;
 trpntr= tr;
 hdsize=256;
 trsize=Cl->nsamp*sizeof(float);
 while(endgroup==0 && ngrp < mgrp) {
  Cl->iskp = n1 + ngrp;
  Cl->ntot = 1;
  ierr  = read_data_fromdisk(*lun, Cl, &Glbls, hdpntr, trpntr);
  if(ierr != 1) {
   strcpy(msg,"read_data_group: error returned from read_data_fromdisk");
   istat=3; goto error;
  }
  ierr = read_data_translate(Cl, &Glbls, hdpntr, trpntr, &tasf[ngrp]);
  if(ierr!=0) {
    strcpy(msg,"read_data_group: problem in read_data_translate");
    istat=5; goto error;
  }
  memcpy(&hval,hdpntr+(hkey-1)*sizeof(float), sizeof(float));
  if(ngrp==0) {
   hold = hval;
   *gval= hval;
  }
  if(abs(hold-hval) < eps) {
    ngrp += 1;
    hdpntr += hdsize;
    trpntr += trsize;
  } else {
    endgroup=1;
  }
  Cl->ntot=1;
  Cl->iskp = n1 + ngrp;
 }
 tasf[ngrp]=tasf[0];
 for(i=0;i<ngrp;i++) {
   tasf[ngrp]= (tasf[0]> tasf[ngrp]) ? tasf[0] : tasf[ngrp];
 }
 Cl->ntot= ngrp;
 Cl->iskp= n1;
 return ngrp;

error:
 printf("%s\n",msg);
 Cl->ntot= ngrp;
 if(*lun != 0 )
  {cl_stat=0; /* keep the file */
   tf_close_(lun,&cl_stat,msg);
   *lun=0;
   if(Glbls.h) free(Glbls.h);
   Glbls.lun =0;
  }
 return ngrp;

}

CubeTrcio *read_data_check_3d (TF_Global *Glbls)
{
  CubeTrcio *retval = 0;
  int X, Y;
  int create = 0, describe = 0;

  if (Glbls->ftyp == '\0' || (strcmp(Glbls->ftyp,"TROT" ) &&
                              strcmp(Glbls->ftyp,"DSEGY") &&
                              strcmp(Glbls->ftyp,"JSEIS")   )) return retval;


  X = Glbls->crossline_header;
  Y = Glbls->inline_header;

  if (cube) {
    if ((int)cube_trcio_matches(cube,Glbls->path)) {
      describe = !((int)tf_global_is3d (Glbls));
    }
    else {
      cube_trcio_delete(&cube);
      create = 1;
    }
    retval = cube;
  }
  else {
    create = 1;
  }
  if (create) {
    cube = cube_trcio_create(Glbls->path, &X, &Y, Glbls);
    if (cube) {
      describe = 1;
      retval = cube;
    }
  }
  if (describe) {
    Glbls->h = (void *)cube_trcio_grid3DDesc (cube);
  }
  return retval;
}

