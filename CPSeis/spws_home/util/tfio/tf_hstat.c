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
#include "tf_hstat.h"
#include "tf_global.h"

/* Keep statistics on the trace headers.
  * hd.... The input header array.
  * stat.. Statistics on the headers already scanned.
  *        headers from last trace, number scanned,
  *        min value, max value, minimum non-zero header
  *        difference from consecutive traces.
  */
int tf_hstats_upd(float *hd, HdrStats *stat)
{
 float diff,nil=999.2;
 int   i,num;

 if(stat==NULL || hd==NULL) return 0;
 if(stat->nhdr > NumCPShd) return 0;
 num = stat->nhdr;
 if(stat->count==0)
  {memcpy(stat->hold,hd,num*sizeof(float));
   memcpy(stat->hmin,hd,num*sizeof(float));
   memcpy(stat->hmax,hd,num*sizeof(float));
   for(i=0;i<num;i++) stat->hbin[i] = nil;
   stat->count=1;
  }
 else
  {
   for(i=0;i<num;i++)
    { stat->hmin[i] = (hd[i]< stat->hmin[i]) ? hd[i] : stat->hmin[i];
      stat->hmax[i] = (hd[i]> stat->hmax[i]) ? hd[i] : stat->hmax[i];
    }
   stat->count += 1;
  }

 for(i=0;i<num;i++)
  {if(hd[i] != stat->hold[i])
    {diff = (float) fabs(hd[i] - stat->hold[i]); 
     stat->hbin[i] = (diff < stat->hbin[i]) ? diff : stat->hbin[i];
     if(stat->hbin[i] <= 0.002) stat->hbin[i]=nil;
    }
   stat->hold[i] = hd[i];
  }
 return 1;
}

void tf_hstats_pr(char *file, HdrStats *stat)
{int i,num;
 float nil=999.2;
 if(!stat || !file) return;
 num = stat->nhdr;
 printf("#  Header Statistics for file: %s\n",file);
 printf("#  Number of headers scanned : %d\n",stat->count);
 printf("#  Hd No.   Min. Value    Max. Value   Bin Value\n");
 for(i=0;i<num;i++)
  {if(stat->hbin[i]==nil)
    printf("# %2d  %12.5g  %12.5g  %12.5g\n",
    i+1,stat->hmin[i],stat->hmax[i],0.0);
   else
    printf("# %2d  %12.5g  %12.5g  %12.5g\n",
    i+1,stat->hmin[i],stat->hmax[i],stat->hbin[i]);
  }

}

HdrStats *tf_hstats_new()
{HdrStats *st;
 st = (HdrStats *) calloc(1,sizeof(HdrStats));
 if(st)
  {st->nhdr = 64;}
 return st;
}

void tf_hstats_del(HdrStats *st)
{
 if(st)
  { st->nhdr=0;
    st->count=0.0;
    st->hd = NULL;
    free(st);
  }
}

HdrStats *tf_hstats_ginfo(char *name, TF_Global *global) {
 int ntraces=0;
 ntraces = tf_global_get_ntrfil(global);
 return tf_hstats_ginfo1(name, global, ntraces);
}

HdrStats *tf_hstats_ginfo1(char *name, TF_Global *global, int nscan)
{/* needs links with libtfio.a */
 FILE          *trace_file;
 HdrStats      *stats=NULL;
 float cpshd[128];
 long ncells,byt_per_tr,ntrrd,nbyrd;
 int  ntrcll,ntrfil,ndptr,count=0,lwdtyp;
 int  cell_to_scan,istat;
 char *trbuff=NULL,*hdbuff=NULL,*hdr;
 char hostname[32],os[32],msg[96];
 int errn,errflag=1,i,j,k,eight=8;
 size_t one = 1,zero=0;

 if(name==NULL || global == NULL) return NULL;
 trace_file = fopen(name,"r");
 if(!trace_file) goto error;
 trbuff = (char *) malloc(global->nbycll+10);
 if(!trbuff) goto error;
 hdbuff = (char *) malloc(global->nbyhd*global->nhdwd+10);
 if(!hdbuff) goto error;
 stats  = tf_hstats_new();
 fseek(trace_file,tf_global_get_grecsiz(global),SEEK_SET);

 lwdtyp = netw_lnode(hostname, os);
 ntrfil = tf_global_get_ntrfil(global);
 ntrcll = tf_global_get_ntrcll(global);
 ndptr  = tf_global_get_ndptr(global);
 ncells = ntrfil/ntrcll;
 if(nscan > ntrfil) nscan=ntrfil;
 cell_to_scan = nscan/ntrcll;
 

 for(i=0;i<cell_to_scan;i++)
  {
   nbyrd = fread(trbuff, one, global->nbycll, trace_file);
   errn = ferror(trace_file);
   if(errn) goto error;
   ntrrd = (nbyrd < global->nbycll) ? global->nbycll/nbyrd  : ntrcll;

   for(j=0;j<ntrrd;j++)
    {byt_per_tr =global->nbyhd*global->nhdwd + ndptr*global->nbydp;
     hdr = trbuff + j*byt_per_tr;
     count++;

     if(strncmp(global->ftyp,"DSEGY",5) == 0 )
      {for(k=0;k<NumCPShd;k++) cpshd[k]=0;
       wrdc_segy_tr_header((unsigned char *) hdr, cpshd);
       cpshd[0] = count;
       cpshd[1] = cpshd[1]/(1000.*global->srval);
       cpshd[63]= ndptr;
       istat = wrdc_fcnvrt_(&global->wdtyp,hdr+240,&lwdtyp,hdr+240,
                &ndptr,msg);
       float_data_lav_(hdr+240,&zero, &one, &ndptr,cpshd+24);

      }

     if(strncmp(global->ftyp,"STROT",5) == 0 ||
        strncmp(global->ftyp,"DTROT",5) == 0 )
      { if(strncmp(os,"UNICOS",6) != 0)
         { strcpy(msg,"STROT and DTROT only valid on cray");
           istat = 2; goto error;
         }
        if(global->nbydp != sizeof(float))
         {
#ifdef CRAY
          EXPAND21(&global->nhdwd,hdr,cpshd,&eight);
#endif
          cpshd[0] = count;
         }
      }

     if(strncmp(global->ftyp,"TFILE",5)==0 )
      {
       if(global->nbyhd == 1)
        {strcpy(msg,"1 byte header for a tfile????\n");
        }
       else
        {int l,n,wdtyp,nhdwd;
         long nhd=64;
         wdtyp = (int) global->wdtyp;
         if(wdtyp==WBYTE) wdtyp=WIEEE;
         if(wdtyp==WSBYT) wdtyp=WIEEE;
         nhdwd = (int) global->nhdwd;
         l = wrdc_fcnvrt_(&wdtyp, hdr, &lwdtyp, (char *) cpshd,
             &nhdwd,msg);
/*
         if(global->nbyhd > 1 && global->wdtyp==WIEEE)
              n = bswap_(&nhdwd, (unsigned char *) cpshd);
*/
        }
      }

     if(strncmp(global->ftyp,"CBYTE",5)==0 )
      {int   l,nhdrs;
       float tasf;
       l = (int) dcodehd1(&istat, global, &nhdrs, hdr, cpshd, &tasf);
      }

     tf_hstats_upd(cpshd, stats);
    }
  }
 
 errflag = 0;
 error:
 if(errflag) 
  { printf("%s\n",msg);
    tf_hstats_del(stats);
    stats=NULL;
  }
 if(hdbuff) free(hdbuff);
 if(trbuff) free(trbuff);
 return stats;
}

