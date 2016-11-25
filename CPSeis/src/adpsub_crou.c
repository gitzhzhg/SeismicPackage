/****
!<CPS_v1 type="AUXILIARY_FILE"/>
****/

/*----------------------------- adpsub_crou.c ------------------------------*/
/*----------------------------- adpsub_crou.c ------------------------------*/
/*----------------------------- adpsub_crou.c ------------------------------*/

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
!                   C P S   P R I M I T I V E   F I L E
!
! Name       : ADPSUB_CROU
! Category   : miscellaneous
! Written    : 2003-11-20   by: B. Lucas
! Revised    : 2006-01-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Adaptive subtraction.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
! 10. 2006-01-03  Stoeckley  More fixes to compile with C++.
!  9. 2005-05-31  Stoeckley  Fix to compile with C++.
!  8. 2004-09-07  B. Lucas   Switched MasterMem size to 100 from 110.
!  7. 2004-04-16  B. Lucas   Fixed bug in DEFAULT path to work like IBSMA.
!                            Fixed missing error checks in pfio reads/seeks.
!  6. 2004-02-25  B. Lucas   Revised error checking on pfio read/writes.
!                            Switched off_t sizes to long-long.
!  5. 2004-02-17  B. Lucas   Added print statement in subtraction loop.
!  4. 2003-12-12  Y. Shen    added general description and advice for users
!  3. 2003-11-20  B. Lucas   Added MEMORY_SIZE.
!  2. 2003-11-10  B. Lucas   Revised version.
!  1. 2003-08-14  B. Lucas   Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


char ADPSUB_CROU_IDENT[100] =
"$Id: adpsub_crou.c,v 1.10 2006/01/03 12:36:23 Stoeckley prod sps $";

#include "c2f_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>

#include "unix.h"
#include "pfio.h"

extern int signgam;
/*
int __allmul;
*/
extern int __allmul;

/*---------------------- fortran spelling adjustments ----------------------*/
/*---------------------- fortran spelling adjustments ----------------------*/
/*---------------------- fortran spelling adjustments ----------------------*/

#ifdef NEED_UNDERSCORE
#define adpsub_initx     adpsub_initx_
#define adpsub_dataprep1 adpsub_dataprep1_
#define adpsub_dataprep2 adpsub_dataprep2_
#define adpsub_dataprep3 adpsub_dataprep3_
#define adpsub_dataprep4 adpsub_dataprep4_
#define adpsub_dataprep5 adpsub_dataprep5_
#define adpsub_subtract  adpsub_subtract_
#define adpsub_output1   adpsub_output1_
#define adpsub_output2   adpsub_output2_
#define adpsub_output3   adpsub_output3_
#define adpsub_dumpparms adpsub_dumpparms_
#elif defined NEED_CAPITALS
#define adpsub_initx     ADPSUB_INITX
#define adpsub_dataprep1 ADPSUB_DATAPREP1
#define adpsub_dataprep2 ADPSUB_DATAPREP2
#define adpsub_dataprep3 ADPSUB_DATAPREP3
#define adpsub_dataprep4 ADPSUB_DATAPREP4
#define adpsub_dataprep5 ADPSUB_DATAPREP5
#define adpsub_subtract  ADPSUB_SUBTRACT
#define adpsub_output1   ADPSUB_OUTPUT1
#define adpsub_output2   ADPSUB_OUTPUT2
#define adpsub_output3   ADPSUB_OUTPUT3
#define adpsub_dumpparms ADPSUB_DUMPPARMS
#endif

/* definition moved from dot.h file */
#define  STRING_LENGTH     200
#define  NBYTES            8
#define  MasterMem  100*1000*1000   /* in 8 bytes */

#ifndef  PI
#define  PI 3.141592653589793
#endif

#ifndef  Max 
#define  Max(a,b) ((a)>(b)?(a):(b))
#endif

#ifndef  Min 
#define  Min(a,b) ((a)>(b)?(b):(a))
#endif

#ifndef  NINT
#define  NINT(x) ((int)((x)>0.0?(x)+0.5:(x)-0.5))
#endif

#ifndef CHARACTER
#define CHARACTER char
#endif

typedef struct TRACEINFO{
  char  INPUT_SEGY[STRING_LENGTH];      
  char  INPUT_MLTP[STRING_LENGTH];
  char  OUTPUT_SEGY[STRING_LENGTH];    
  char  WORKDIR[STRING_LENGTH];
  int   nshot;
  int   nrecv;
  int   nt;                      
  float dt;                      
  float t0;                      
} TraceInfo;

typedef struct ADPSUBINFO{
  int   restart;
  int   domain;
  int   iopt;
  float s_time;
  float e_time;
  float t_win;
  int   lwx;
  float f_len;
  int   f_lenx;
  int   fc_max;
  int   iscale;
  int   niter;
} AdpsubInfo;


typedef struct MISCINFO{
  int   ngroup;
  int   groupsize;
  int   grouplast;
} MiscInfo;

static TraceInfo  trsinfo;
static AdpsubInfo adpsubinfo;
static MiscInfo   miscinfo;
static char filename1[STRING_LENGTH];
static char filename2[STRING_LENGTH];
static char filename3[STRING_LENGTH];
static int shift1;
static int shift2;
static int shift3;
static int group;
static int groupsize;
static int nhead;
static int cleanup;
static int MemScalar;
static char hostname[120];

static double *trhead;
static float  *trace;
static float  *data_io;

static int ofd_data;
static int ofd_head;
static int ofd_out;


#ifdef __cplusplus
extern "C" {
#endif


void adpsub_dumpparms(TraceInfo  trsinfo,
                      AdpsubInfo adpsubinfo,
                      MiscInfo   miscinfo);
void adpsub_initx (INTEGER *ier, CHARACTER *wrkdr, INTEGER *memsize,
        INTEGER *cable,
        INTEGER *restart, INTEGER *domain, INTEGER *nshot, INTEGER *nrecv, 
        INTEGER *iopt, REAL *s_time, REAL *e_time, REAL *t_win, INTEGER *lwx,
        REAL *f_len, INTEGER *f_lenx, INTEGER *fc_max, INTEGER *iscale, 
        INTEGER *niter, INTEGER *nt, REAL *dt, REAL *t0, INTEGER *nwih, 
        void (*SUBR) (REAL *a, INTEGER *b, INTEGER *c, INTEGER *d, INTEGER *e, 
        INTEGER *f, INTEGER *g, INTEGER *h, INTEGER *i, INTEGER *j, INTEGER *k, 
        INTEGER *l, INTEGER *m, INTEGER *n, REAL *o, INTEGER *p, INTEGER *q,
        REAL *r, REAL *s, REAL *t, REAL *u),
        INTEGER *ngroups, INTEGER *groupsize0, INTEGER *groupsize1);
void adpsub_dataprep1 (INTEGER *ier,DOUBLE *header,REAL *trace,INTEGER *trndx);
void adpsub_dataprep2 (INTEGER *ier);
void adpsub_dataprep3 (INTEGER *ier);
void adpsub_dataprep4 (INTEGER *ier);
void adpsub_dataprep5 (INTEGER *ier);
void adpsub_output1 (INTEGER *ier);
void adpsub_output2 (INTEGER *ier, INTEGER *shotg, INTEGER *shot,
                     INTEGER *recv, DOUBLE *hd, REAL *tr);
void adpsub_output3 (INTEGER *ier);
void adpsub_subtract (INTEGER *ier);
void (*SUBRG)(REAL *a, INTEGER *b, INTEGER *c, INTEGER *d, INTEGER *e,
              INTEGER *f, INTEGER *g, INTEGER *h, INTEGER *i, INTEGER *j,
              INTEGER *k, INTEGER *l, INTEGER *m,
                INTEGER *n, REAL *o, INTEGER *p, INTEGER *q,
                REAL *r, REAL *s, REAL *t, REAL *u);


/*--------------------------- adpsub_init ------------------------------*/
/*--------------------------- adpsub_init ------------------------------*/
/*--------------------------- adpsub_init ------------------------------*/
void adpsub_initx (INTEGER *ier, CHARACTER *wrkdr, INTEGER *memsize,
        INTEGER *cable,
        INTEGER *restart, INTEGER *domain, INTEGER *nshot, INTEGER *nrecv, 
        INTEGER *iopt, REAL *s_time, REAL *e_time, REAL *t_win, INTEGER *lwx,
        REAL *f_len, INTEGER *f_lenx, INTEGER *fc_max, INTEGER *iscale, 
        INTEGER *niter, INTEGER *nt, REAL *dt, REAL *t0, INTEGER *nwih, 
        void (*SUBR)(REAL *a, INTEGER *b, INTEGER *c, INTEGER *d, INTEGER *e, 
        INTEGER *f, INTEGER *g, INTEGER *h, INTEGER *i, INTEGER *j, INTEGER *k, 
        INTEGER *l, INTEGER *m, INTEGER *n, REAL *o, INTEGER *p, INTEGER *q,
        REAL *r, REAL *s, REAL *t, REAL *u),
        INTEGER *ngroups, INTEGER *groupsize0, INTEGER *groupsize1)
{
   /* Local variables. */
   int nfatal = 0;
   int size;
   int isDefault = 0;

   SUBRG = SUBR;        

   /* Initialization section. */
   *ier = 0;
   
   MemScalar = *memsize;

   nhead = *nwih;

   /* Read and parse input parameters. */
   /* cptr = getenv("HOSTNAME"); */
   
   
   if(!strcasecmp(wrkdr, "default") ) {
      sprintf(trsinfo.WORKDIR, "~/cpstemp/");
      isDefault = 1;
      cleanup = 1;
   } else {
      sprintf(trsinfo.WORKDIR, "%s", wrkdr);
      cleanup = 0;
   }        

   trsinfo.nshot = *nshot;
   trsinfo.nrecv = *nrecv;
   trsinfo.nt = *nt;
   trsinfo.dt = *dt;
   trsinfo.t0 = *t0;

   adpsubinfo.restart = *restart;
   adpsubinfo.domain  = *domain;
   adpsubinfo.iopt    = *iopt;
   adpsubinfo.s_time  = *s_time;
   adpsubinfo.e_time  = *e_time;
   adpsubinfo.t_win   = *t_win;
   adpsubinfo.lwx     = *lwx;
   adpsubinfo.f_len   = *f_len;
   adpsubinfo.f_lenx  = *f_lenx;
   adpsubinfo.fc_max  = *fc_max;
   adpsubinfo.iscale  = *iscale;
   adpsubinfo.niter   = *niter;

   /* Determine how many shots can be hold in memory at once:    */
   /* We do adpative subtraction in either common-offset or      */
   /* common-shot domain. We read in data, write them to a       */
   /* temporal file in offset/shot order. Then we read back      */
   /* one offset/shot data at a time to process and write the    */
   /* result to another temporal file. After all offsets are     */
   /* finished, we read them back, and write out as common-shot  */
   /* gather in SEGY format.                                     */
   size = trsinfo.nrecv*trsinfo.nt*sizeof(float)      /* data_in */
         + trsinfo.nrecv*nhead*sizeof(double);       /* header */
   miscinfo.groupsize = MasterMem*NBYTES*MemScalar/size;
   miscinfo.ngroup = (trsinfo.nshot+miscinfo.groupsize-1)
                   / miscinfo.groupsize;
   miscinfo.grouplast = trsinfo.nshot - (miscinfo.ngroup-1)
                      * miscinfo.groupsize;
   if(miscinfo.ngroup==1) miscinfo.groupsize = miscinfo.grouplast;

   *ngroups = miscinfo.ngroup;
   *groupsize0 = miscinfo.groupsize;
   *groupsize1 = miscinfo.grouplast;

   /* Dump parameters. */
   adpsub_dumpparms(trsinfo, adpsubinfo, miscinfo);
  
   unix_get_hostname_c(hostname,sizeof(hostname));

   /* Set temporary filename. */
   if(isDefault) {
      int pid = getpid();
      sprintf(filename1, "%s/adpsub.%d.input_%s_%d",  trsinfo.WORKDIR, *cable,
         hostname, pid);
      sprintf(filename2, "%s/adpsub.%d.trhead_%s_%d", trsinfo.WORKDIR, *cable,
          hostname, pid);
      sprintf(filename3, "%s/adpsub.%d.output_%s_%d", trsinfo.WORKDIR, *cable,
          hostname, pid);
   } else {
      int pid = getpid(); 
      sprintf(filename1, "%s/adpsub.%d.input_%d",  trsinfo.WORKDIR,*cable,pid);
      sprintf(filename2, "%s/adpsub.%d.trhead_%d", trsinfo.WORKDIR,*cable,pid);
      sprintf(filename3, "%s/adpsub.%d.output_%d", trsinfo.WORKDIR,*cable,pid); 
   }

   if(adpsubinfo.restart == 0) {

      /* Allocate arrays. */
      trace = (float *)malloc(trsinfo.nt*sizeof(float));
      if(trace == (float *)NULL) {
         printf("Data prep: Failed to allocate a trace!\n");
         nfatal++;
      }

      size = miscinfo.groupsize*trsinfo.nrecv*nhead;
      trhead = (double *)malloc(size*sizeof(double));
      if(trhead == (double *)NULL) {
         printf("Data prep: Failed to allocate trhead!\n");
         nfatal++;
      }

      size = miscinfo.groupsize*trsinfo.nrecv*trsinfo.nt;
      data_io = (float *)malloc(size*sizeof(float));
      if(data_io == (float *)NULL) {
         printf("Data prep: Failed to allocate data_io.\n");
         nfatal++;
      }

      /* If there have been any fatal errors, quit the job. */
      if(nfatal > 0) {
         printf("Data prep: Fatal errors = %d.\n", nfatal);
         *ier = 3;
      }                

      /* Open output binary data and trace head files. */
      ofd_data = pfio_open(filename1, 'w');
      if(ofd_data < 0) {
         printf("Data prep: Failed to open temp data file %s.\n", filename1);
         *ier = 4;
         return;
      }
      ofd_head = pfio_open(filename2, 'w');
      if(ofd_head < 0) {
         printf("Data prep: Failed to open temp header file %s.\n", filename2);
         *ier = 5;
         return;
      }
   }

   group = 1;
   if(group < miscinfo.ngroup) {
      groupsize = miscinfo.groupsize;
   } else {
      groupsize = miscinfo.grouplast;
   }

   return;
}


/*-------------------------- adpsub_dataprep1 ---------------------------*/
/*-------------------------- adpsub_dataprep1 ---------------------------*/
/*-------------------------- adpsub_dataprep1 ---------------------------*/
void adpsub_dataprep1 (INTEGER *ier, DOUBLE *header, REAL *trace, 
                       INTEGER *trndx)
{
   int j, k;
   int it;

   *ier = 0;        

   /* Copy trace header. */
   shift1 = nhead * (*trndx-1);
   for(it = 0; it < nhead; it++) {
      trhead[it+shift1] = header[it];
   }

   /* Copy trace data to data_io. */
   j = (*trndx - 1) / trsinfo.nrecv;
   k = (*trndx - 1) % trsinfo.nrecv;
   if(adpsubinfo.domain) {        /* Sort to offset gather. */
      shift2 = (k*groupsize+j)*trsinfo.nt;
      for(it = 0; it < trsinfo.nt; ++it) 
         data_io[shift2+it] = trace[it];
   } else {        /* Keep in shot gather. */
      shift2 = (k+j*trsinfo.nrecv)*trsinfo.nt;
      for(it = 0; it < trsinfo.nt; ++it)
         data_io[shift2+it] = trace[it];
   }        
   return;        
}


/*-------------------------- adpsub_dataprep2 ---------------------------*/
/*-------------------------- adpsub_dataprep2 ---------------------------*/
/*-------------------------- adpsub_dataprep2 ---------------------------*/
void adpsub_dataprep2 (INTEGER *ier)
{
   int j;
   int itemp;
   int istat;
   long nbytes;
   long lsize;
   long long offset;

   *ier = 0;

   /* Write output trace header. */
   itemp = groupsize * trsinfo.nrecv * nhead;
   lsize = itemp * sizeof(double);
   nbytes = pfio_write(ofd_head, (char *)trhead, lsize);
   if(nbytes != lsize) {
      printf("Data prep2: Failed to write to temp header file.\n");
      *ier = 1;
      return;
   }

   /* Write output data. */
   if(adpsubinfo.domain) { /* Work in offset domain. */
      offset = (long long)(group-1) * (long long)miscinfo.groupsize * 
               (long long)trsinfo.nt * (long long)sizeof(float);
      istat = pfio_seek_via_origin(ofd_data, offset, SEEK_SET);
      if(istat != 0) {
         printf("Data prep2: Failed to seek in temp data file.\n");
         *ier = 1;
         return;
      }
      offset = (long long)(2*trsinfo.nshot - groupsize) * 
               (long long)trsinfo.nt * (long long)sizeof(float);
      itemp = groupsize*trsinfo.nt;
      for(j = 0; j < trsinfo.nrecv; ++j) {
         shift3 = j*groupsize*trsinfo.nt;
         lsize = itemp * sizeof(float);
         nbytes = pfio_write(ofd_data, (char*)(data_io+shift3), lsize);
         if(nbytes != lsize) {
            printf("Data prep2: Failed to write to temp data file.\n");
            *ier = 1;
            return;
         }
         istat = pfio_seek_via_origin(ofd_data, offset, SEEK_CUR);
         if(istat != 0) {
            printf("Data prep2: Failed to seek in temp data file.\n");
            *ier = 1;
            return;
         }
      }

   } else { /* Work in shot domain. */
      itemp = trsinfo.nrecv*trsinfo.nt;
      offset = (long long)itemp * (long long)sizeof(float);
      for(j = 0; j < groupsize; ++j) {
         shift3 = j*itemp;
         lsize = itemp * sizeof(float);
         nbytes = pfio_write(ofd_data, (char *)(data_io+shift3), lsize);
         if(nbytes != lsize) {
            printf("Data prep2: Failed to write to temp data file.\n");
            *ier = 1;
            return;
         }
         istat = pfio_seek_via_origin(ofd_data, offset, SEEK_CUR);
         if(istat != 0) {
            printf("Data prep2: Failed to seek in temp data file.\n");
            *ier = 1;
            return;
         }
      }                
   }

   group++;
   if(group < miscinfo.ngroup) {
      groupsize = miscinfo.groupsize;
   } else {
      groupsize = miscinfo.grouplast;
   }
   shift1 = 0;
   shift2 = trsinfo.nrecv * groupsize;

   return;        
}


/*-------------------------- adpsub_dataprep3 ---------------------------*/
/*-------------------------- adpsub_dataprep3 ---------------------------*/
/*-------------------------- adpsub_dataprep3 ---------------------------*/
void adpsub_dataprep3 (INTEGER *ier)
{
   int istat;
   long long offset;

   *ier = 0; 

   /* When working on common-shot gathers, */
   /* set file pointer to right position.  */
   if(!adpsubinfo.domain) {
      offset = (long long)trsinfo.nrecv * (long long)trsinfo.nt *
               (long long)sizeof(float);
      istat = pfio_seek_via_origin(ofd_data, offset, SEEK_SET);        
      if(istat != 0) {
         printf("Data prep3: Failed to seek in temp data file.\n");
         *ier = 1;
         return;
      }
   }

   group = 1;
   if(group < miscinfo.ngroup) {
      groupsize = miscinfo.groupsize;
   } else {
      groupsize = miscinfo.grouplast;
   }

   return;        
}


/*-------------------------- adpsub_dataprep4 ---------------------------*/
/*-------------------------- adpsub_dataprep4 ---------------------------*/
/*-------------------------- adpsub_dataprep4 ---------------------------*/
void adpsub_dataprep4 (INTEGER *ier)
{
   int j;
   int itemp;
   int istat = 0;
   long nbytes;
   long lsize;
   long long offset;

   *ier = 0;
   
   /* Write output data. */
   if(adpsubinfo.domain) { /* Work in offset domain. */
      offset = (long long)(trsinfo.nshot+(group-1)*miscinfo.groupsize) * 
               (long long)trsinfo.nt * (long long)sizeof(float);
      istat = pfio_seek_via_origin(ofd_data, offset, SEEK_SET);
      if(istat != 0) {
         printf("Data prep4: Failed to seek in temp data file.\n");
         *ier = 1;
         return;
      }
      offset = (long long)(2*trsinfo.nshot - groupsize) * 
               (long long)trsinfo.nt * (long long)sizeof(float);
      itemp = groupsize*trsinfo.nt;
      for(j = 0; j < trsinfo.nrecv; ++j) {
         shift3 = j*groupsize*trsinfo.nt;
         lsize = itemp * sizeof(float);
         nbytes = pfio_write(ofd_data, (char *)(data_io+shift3), lsize);
         if(nbytes != lsize) {
            printf("Data prep4: Failed to write to temp data file.\n");
            *ier = 1;
            return;
         }
         istat = pfio_seek_via_origin(ofd_data, offset, SEEK_CUR);
         if(istat != 0) {
            printf("Data prep4: Failed to seek in temp data file.\n");
            *ier = 1;
            return;
         }
      }
   } else { /* Work in shot domain. */
      itemp = trsinfo.nrecv*trsinfo.nt;
      offset = (long long)itemp * (long long)sizeof(float);
      for(j = 0; j < groupsize; ++j) {
         shift3 = j*itemp;
         lsize = itemp * sizeof(float);
         nbytes = pfio_write(ofd_data, (char *)(data_io+shift3), lsize);
         if(nbytes != lsize) {
            printf("Data prep4: Failed to write to temp data file.\n");
            *ier = 1;
            return;
         }
         istat = pfio_seek_via_origin(ofd_data, offset, SEEK_CUR);
         if(istat != 0) {
            printf("Data prep4: Failed to seek in temp data file.\n");
            *ier = 1;
            return;
         }
      }                
   }

   group++;
   if(group  <  miscinfo.ngroup) {
      groupsize = miscinfo.groupsize;
   } else {
      groupsize = miscinfo.grouplast;
   }
   shift1 = 0;
   shift2 = trsinfo.nrecv * groupsize;

   return;
}


/*-------------------------- adpsub_dataprep5 ----------------------------*/
/*-------------------------- adpsub_dataprep5 ----------------------------*/
/*-------------------------- adpsub_dataprep5 ----------------------------*/
void adpsub_dataprep5 (INTEGER *ier)
{        
   int istat = 0;
   *ier = 0;

   /* Print info. */
   printf("\n***** Data preparation: completed *****\n\n");

   /* Free space. */
   free(trace);
   free(trhead);
   free(data_io);

   /* Reset file pointer. */
   istat = pfio_seek_via_origin(ofd_data, 0L, SEEK_SET);
   if(istat != 0) {
      printf("Data prep5: Failed to seek in temp data file.\n");
      *ier = 1;
      return;
   }
   istat = pfio_seek_via_origin(ofd_head, 0L, SEEK_SET);
   if(istat != 0) {
      printf("Data prep5: Failed to seek in temp header file.\n");
      *ier = 1;
      return;
   }

   return;        
}


/*-------------------------- adpsub_subtract ----------------------------*/
/*-------------------------- adpsub_subtract ----------------------------*/
/*-------------------------- adpsub_subtract ----------------------------*/
void adpsub_subtract (INTEGER *ier)
{
   int gathersize, loopsize, size;
   float *gather, *work1, *work2;
   int ntr, ip1, iv1, idi, ist, nsamp;
   int iopt, lop, lopx, n;
   int lwt, lwx, itemp, iscale, nit, i, ierr;
   int istat;
   long nbytes;
   long lsize;
   float *dttrace, *noise, fc_max;
   *ier = 0;

   if(adpsubinfo.restart==1) {
      printf("Restart from adpative subtraction.\n");

      ofd_data = pfio_open(filename1, 'r');
      if(ofd_data < 0) {
         printf("Subtract: Failed to open temp data file %s\n", filename1);
         *ier = 1;
         return;
      }

      ofd_head = pfio_open(filename2, 'r');
      if(ofd_head < 0) {
         printf("Subtract: Failed to open temp header file %s\n", filename2);
         *ier = 2;
         return;
      }
   }

   /* Adpative subtraction section:             */
   /* loop over offset/shot gathers             */
   /* for each gather, do the following         */
   /* (1) read in a gather, including both data */
   /*     and predicted multiples               */
   /* (2) call adpative subtraction             */
   /* (3) write out subtraction result          */

   /* open output file */
   ofd_out = pfio_open(filename3, 'w');
   if(ofd_out < 0) {
      printf("Subtract: Failed to open temp output file %s\n", filename3);
      *ier = 1;
      return;
   }

   /* Determine gather size. */
   if(adpsubinfo.domain) {
      gathersize = trsinfo.nshot;
      loopsize = trsinfo.nrecv;
   } else {
      gathersize = trsinfo.nrecv;
      loopsize = trsinfo.nshot;
   }


   /* Allocate arrays. */
   /* Gather holds both data & predicted multiple. */
   size = 2*gathersize*trsinfo.nt;
   gather = (float *)malloc(size*sizeof(float));
   if(gather == (float*)NULL) {
      printf("Subtract: Failed to allocate gather.\n");
      *ier = 1;
      return;
   }

   /* Working array holds input data. */
   size = gathersize*trsinfo.nt;
   dttrace = (float *)malloc(size*sizeof(float));
   if(dttrace == (float*)NULL) {
      printf("Subtract: Failed to allocate dttrace.\n");
      *ier = 2;
      return;
   }

   noise = (float *)malloc(size*sizeof(float));
   if(noise == (float*)NULL) {
      printf("Subtract: Failed to allocate noise.\n");
      *ier = 3;
      return;
   }

   /* Two working arrays for adpative subtraction. */
   size = 7*gathersize*trsinfo.nt;
   work1 = (float *)malloc(size*sizeof(float));
   if(work1 == (float*)NULL) {
      printf("Subtract: Failed to allocate work1.\n");
      *ier = 4;
      return;
   }

   size = gathersize*trsinfo.nt;
   work2 = (float *)malloc(size*sizeof(float));
   if(work2 == (float*)NULL) {
      printf("Subtract: Failed to allocate work2.\n");
      *ier = 5;
      return;
   }

   /* Parameters used by fortran subroutine 'ASUBB'. */
   n = trsinfo.nt;
   ntr = gathersize;
   ip1 = 1;
   iv1 = 1 + gathersize;
   idi = 1;
   ist = (adpsubinfo.s_time-trsinfo.t0)/trsinfo.dt + 1;
   itemp = (adpsubinfo.e_time-trsinfo.t0)/trsinfo.dt + 1;
   nsamp = itemp - ist + 1;
   itemp = NINT(adpsubinfo.t_win/trsinfo.dt);
   lwt = 2*(itemp/2);
   lwx = 2*(adpsubinfo.lwx/2) + 1;
   itemp = NINT(adpsubinfo.f_len/trsinfo.dt);
   iopt = adpsubinfo.iopt;
   lop = 2*(itemp/2) + 1;
   lopx = Max(adpsubinfo.f_lenx, 1);
   iscale = adpsubinfo.iscale;
   fc_max = adpsubinfo.fc_max;
   nit = adpsubinfo.niter;
   ierr = 0;

   /* Check parameters used for calling 'ASUBB'. */
   printf("Check parameters used for calling ASUBB.\n");
   printf("n = %d\n", n);
   printf("ntr = %d\n", ntr);
   printf("ip1 = %d\n", ip1);
   printf("iv1 = %d\n", iv1);
   printf("idi = %d\n", idi);
   printf("ist = %d\n", ist);
   printf("nsamp = %d\n", nsamp);
   printf("lwt = %d\n", lwt);
   printf("lwx = %d\n", lwx);
   printf("iopt = %d\n", iopt);
   printf("lop = %d\n", lop);
   printf("lopx = %d\n", lopx);
   printf("iscale = %d\n", iscale);
   printf("fc_max = %g\n", fc_max);
   printf("nit = %d\n", nit);
   printf("ierr = %d\n\n", ierr);  

   /* Loop over all common-offset gathers. */
   size = 2*gathersize*trsinfo.nt;
   itemp = size/2;
   for(i = 0; i < loopsize; ++i) {

      printf("Working on gather %d of %d\n", i+1, loopsize);

      lsize = size * sizeof(float);
      nbytes = pfio_read(ofd_data, (char *)gather, lsize);
      if(nbytes != lsize) {
         printf("Subtract: Failed to read from temp data file.\n");
         *ier = 1;
         return;
      }
      ierr = 0;
      SUBRG(gather, &n, &ntr, &ip1, &iv1, &idi,
         &ist, &nsamp, &lwt, &lwx, &iopt, &lop, &lopx,
         &iscale, &fc_max, &nit, &ierr,
         dttrace, noise, work1, work2);
      if(ierr > 0) {
         printf("Subtract: Failed.\n");
         *ier = 1;
         return;
      }
      lsize = itemp * sizeof(float);
      nbytes = pfio_write(ofd_out, (char *)gather, lsize);
      if(nbytes != lsize) {
         printf("Subtract: Failed to write to temp output file.\n");
         *ier = 1;
         return;
      }
   }

   /* Print info. */
   printf("\n***** Adaptive subtraction: completed *****\n\n");

   /* Free space. */
   free(gather);
   free(dttrace);
   free(noise);
   free(work1);
   free(work2);

   /* Close file. */
   istat = pfio_close(ofd_data);
   if(istat < 0) {
      printf("Subtract: Failed to close temp data file.\n");
      *ier = 1;
      return;
   }
   if(cleanup) {
      pfio_delete(filename1);                
   }

   /* Reset file. */
   istat = pfio_seek_via_origin(ofd_out, 0L, SEEK_SET); 
   if(istat != 0) {
      printf("Subtract: Failed to seek in temp output file.\n");
      *ier = 1;
      return;
   }       

   return;
}


/*------------------------- adpsub_output1 ------------------------------*/
/*------------------------- adpsub_output1 ------------------------------*/
/*------------------------- adpsub_output1 ------------------------------*/
void adpsub_output1 (INTEGER *ier)
{
   int size;
   int nfatal = 0;
   *ier = 0;  

   if(adpsubinfo.restart == 2) {
      printf("Restart from output.\n");

      ofd_head = pfio_open(filename2, 'r');
      if(ofd_head < 0) {
          printf("Output: Failed to open file %s.\n", filename2);
          *ier = 2;
          return;
      }        

      ofd_out = pfio_open(filename3, 'r');
      if(ofd_out < 0) {
          printf("Output: Failed to open file %s.\n", filename3);
          *ier = 3;
          return;
      }

   }

   /***************************************************/
   /*                Output result                    */
   /* (1) read back adpative subtraction result       */
   /* (2) write out common-shot gather in SEGY format */
   /***************************************************/
   nfatal = 0;

   /* Allocate arrays. */
   trace = (float *)malloc(trsinfo.nt*sizeof(float));
   if(trace == (float*)NULL) {
      printf("Output: Failed to allocate a trace.\n");
      nfatal++;
   }

   size = miscinfo.groupsize*trsinfo.nrecv*nhead;
   trhead = (double *)malloc(size*sizeof(double));
   if(trhead == (double*)NULL) {
      printf("Output: Failed to allocate trhead.\n");
      nfatal++;
   }

   size = miscinfo.groupsize*trsinfo.nrecv*trsinfo.nt;
   data_io = (float *)malloc(size*sizeof(float));
   if(data_io == (float*)NULL) {
      printf("Output: Failed to allocate data_io.\n");
      nfatal++;
   }

   if(nfatal > 0) {
      printf("Output: Fatal errors = %d.\n", nfatal);
      *ier = 1;
      return;
   }

   return;
}


/*------------------------- adpsub_output2 -------------------------------*/
/*------------------------- adpsub_output2 -------------------------------*/
/*------------------------- adpsub_output2 -------------------------------*/
void adpsub_output2 (INTEGER *ier, INTEGER *group, INTEGER *shot,
                     INTEGER *recv, DOUBLE *hd, REAL *tr)
{
   int itemp, i, j, k, it, size;
   int ishot, irecv;
   int istat;
   long nbytes;
   long lsize;
   long long offset;
   double v[256];

   *ier = 0;  

   /* Read back the complete result at once. */
   if(miscinfo.ngroup==1) {

      if(*shot == 1 && *recv == 1) goto startGroup1;
      if(*shot != 1 && *recv == 1) goto startShot1;
      if(*recv != 1) goto startRecv1;

startGroup1:                                                
      /* Read back trace head. */
      itemp = miscinfo.groupsize*trsinfo.nrecv*nhead;
      lsize = itemp * sizeof(double);
      nbytes = pfio_read(ofd_head, (char *)trhead, lsize);
      if(nbytes != lsize) {
         printf("Output2: Failed to read from temp header file.\n");
         *ier = 1;
         return;
      }

      /* Read back data. */
      itemp = miscinfo.groupsize*trsinfo.nrecv*trsinfo.nt;
      lsize = itemp * sizeof(float);
      nbytes = pfio_read(ofd_out, (char *)data_io, lsize);
      if(nbytes != lsize) {
         printf("Output2: Failed to read from temp output file.\n");
         *ier = 1;
         return;
      }

      startShot1:
      shift1 = 0;

startRecv1:
      ishot = *shot - 1;
      irecv = *recv - 1;
      j = ishot;
      k = irecv;

      /* Copy trace head. */
      shift1 = (k + trsinfo.nrecv * j) * nhead;
      for(it = 0; it < nhead; ++it) {
         v[it] = trhead[it+shift1];
      }

      /* Set trace header. */
      itemp = 0;
      for(it = 0; it < nhead; it++) {
         hd[it] = v[it];
      }
      if(itemp != 0) {
         *ier = 1;
         return;
      }

      /* Copy trace. */
      if(adpsubinfo.domain) {
         shift2 = (j + k*trsinfo.nshot)*trsinfo.nt;
         for(it = 0; it < trsinfo.nt; ++it) 
            trace[it] = data_io[shift2+it];
      } else {
         shift2 = (j*trsinfo.nrecv + k)*trsinfo.nt;
         for(it = 0; it < trsinfo.nt; ++it) 
            trace[it] = data_io[shift2+it];
      }

      /* Write SEGY trace. */
      itemp = 0;
      for(it = 0; it < trsinfo.nt; it++) {
         tr[it] = trace[it];
      }
      if(itemp != 0) {
         *ier = 1;
         return;
      }

      /* We don't have enough memory, we need to read back */
      /* a portion of the result at a time. */
   } else {

      i = *group - 1;

      if(i < (miscinfo.ngroup-1))
         size = miscinfo.groupsize;
      else
         size = miscinfo.grouplast;

      if(*shot == 1 && *recv == 1) goto startGroupN;
      if(*shot != 1 && *recv == 1) goto startShotN;
      if(*recv != 1) goto startRecvN;

startGroupN:

      /* Read back portion of result which can be used */
      /* to assemble miscinfo.groupsize shot gathers   */
      /* first, trace head.                            */
      itemp = size*trsinfo.nrecv*nhead;
      lsize = itemp * sizeof(double);
      nbytes = pfio_read(ofd_head, (char *)trhead, lsize);
      if(nbytes != lsize) {
         printf("Output2: Failed to read from temp header file.\n");
         *ier = 1;
         return;
      }

      if(adpsubinfo.domain) { /* Data is in offset domain. */
         offset = (long long)(i*miscinfo.groupsize) *
                  (long long)trsinfo.nt * (long long)sizeof(float);
         istat = pfio_seek_via_origin(ofd_out, offset, SEEK_SET);
         if(istat != 0) {
            printf("Output2: Failed to seek in temp output file.\n");
            *ier = 1;
            return;
         }
         offset = (long long)(trsinfo.nshot - size) *
                  (long long)trsinfo.nt * (long long)sizeof(float);
         itemp = size*trsinfo.nt;
         for(j = 0; j < trsinfo.nrecv; ++j) {
            shift3 = j*size*trsinfo.nt;
            lsize = itemp * sizeof(float);
            nbytes = pfio_read(ofd_out, (char *)(data_io+shift3), lsize);
            if(nbytes != lsize) {
               printf("Output2: Failed to read from temp output file.\n");
               *ier = 1;
               return;
            }
            istat = pfio_seek_via_origin(ofd_out, offset, SEEK_CUR);
            if(istat != 0) {
               printf("Output2: Failed to seek in temp output file.\n");
               *ier = 1;
               return;
            }
         }
      } else { /* Data is in shot domain. */
         itemp = size*trsinfo.nrecv*trsinfo.nt;
         lsize = itemp * sizeof(float);
         nbytes = pfio_read(ofd_out, (char *)data_io, lsize);
         if(nbytes != lsize) {
            printf("Output2: Failed to read from temp output file.\n");
            *ier = 1;
            return;
         }
      }

startShotN:
      /* Write 'miscinfo.groupsize' SEGY shot gathers. */
      shift1 = 0;

      startRecvN:
      ishot = *shot - 1;
      irecv = *recv - 1;
      j = ishot;
      k = irecv;

      /* Copy trace head. */
      shift1 = (k + trsinfo.nrecv * j) * nhead;
      for(it = 0; it < nhead; ++it) {
         v[it] = trhead[it+shift1];
      }

      /* Set trace head. */
      itemp = 0;
      for(it = 0; it < nhead; it++) {
         hd[it] = v[it];
      }                
      if(itemp != 0) {
         *ier = 1;
         return;
      }

      /* Copy trace. */
      if(adpsubinfo.domain) {
         shift2 = (k*size+j)*trsinfo.nt;
         for(it = 0; it < trsinfo.nt; ++it) 
            trace[it] = data_io[shift2+it];
      } else {
         shift2 = (k+j*trsinfo.nrecv)*trsinfo.nt;
         for(it = 0; it < trsinfo.nt; ++it) 
            trace[it] = data_io[shift2+it];
      }

      /* Write SEGY trace. */
      itemp = 0;
      for(it = 0; it < trsinfo.nt; it++) {
         tr[it] = trace[it];
      }
      if(itemp != 0) {
        *ier = 1;
         return;
      }
   }
   return;
}


/*------------------------- adpsub_output3 ------------------------------*/
/*------------------------- adpsub_output3 ------------------------------*/
/*------------------------- adpsub_output3 ------------------------------*/
void adpsub_output3 (INTEGER *ier)
{
   int istat = 0;
   int nfatal = 0;
   *ier = 0;        

   /* Print info. */
   printf("\n***** Output: completed *****\n\n");

   /* Free space. */
   free(trace);
   free(trhead);
   free(data_io);

   /* Close files. */
   istat = pfio_close(ofd_head);
   if(istat < 0) {
      printf("Output3: Failed to close temp header file.\n");
      nfatal++;
      return;
   }
   istat = pfio_close(ofd_out);
   if(istat < 0) {
      printf("Output3: Failed to close temp output file.\n");
      nfatal++;
      return;
   }

   if(cleanup) {
      pfio_delete(filename2);
      pfio_delete(filename3);
   }

   return;
}


/*------------------------- adpsub_dumpparms --------------------------------*/
/*------------------------- adpsub_dumpparms --------------------------------*/
/*------------------------- adpsub_dumpparms --------------------------------*/

void adpsub_dumpparms(TraceInfo  trsinfo,
                      AdpsubInfo adpsubinfo,
                      MiscInfo   miscinfo)
{
   printf("\n");
   printf("***** ADPSUB parameter list *****\n");
   printf("\n");

   printf("Trace information:\n");
   printf("WORKDIR     = %s\n", trsinfo.WORKDIR);
   printf("nshot       = %d\n", trsinfo.nshot);
   printf("nrecv       = %d\n", trsinfo.nrecv);
   printf("nt          = %d\n", trsinfo.nt);
   printf("dt          = %g\n", trsinfo.dt);
   printf("t0          = %g\n", trsinfo.t0);

   printf("\nAdpative Subtraction information:\n");
   printf("restart     = %d\n", adpsubinfo.restart);
   printf("domain      = %d\n", adpsubinfo.domain);
   printf("iopt        = %d\n", adpsubinfo.iopt);
   printf("s_time      = %g\n", adpsubinfo.s_time);
   printf("e_time      = %g\n", adpsubinfo.e_time);
   printf("t_win       = %g\n", adpsubinfo.t_win);
   printf("lwx         = %d\n", adpsubinfo.lwx);
   printf("f_len       = %g\n", adpsubinfo.f_len);
   printf("f_lenx      = %d\n", adpsubinfo.f_lenx);
   printf("fc_max      = %d\n", adpsubinfo.fc_max);
   printf("iscale      = %d\n", adpsubinfo.iscale);
   printf("niter       = %d\n", adpsubinfo.niter);

   printf("\nMisc. information:\n");
   printf("groupsize   = %d\n", miscinfo.groupsize);
   printf("ngroup      = %d\n", miscinfo.ngroup);
   printf("grouplast   = %d\n", miscinfo.grouplast);

   fflush(stdout);
}


#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

