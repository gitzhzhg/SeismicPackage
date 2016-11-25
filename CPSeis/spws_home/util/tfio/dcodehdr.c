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

#if VMS
#include <stdlib.h>
#include <stddef.h>
#endif

#include "tfio.h"
#include "tfdefs.h"

#define MUTHDR  1
#define TAILHDR 64
#define HDROUT  64

/*------------------------------------------------------------------
C\USER DOC
 *Name   : dcodehdr
 *Purpose: Converts byte header data from a CPS byte file to floats.
 *Author : R. Day
 *Date   : 3/28/91
 *
 *Function Definition:        ( Language = C )
 * int  dcodehdr(int  *istat, struct GLBL *Glbls,struct Cntrl *Cl,
 *      int  *nhdrs, char *hd, float *TASF)
 * *istat    output    0 if no errors occur.
 * *Glbls    input     from get_global data
 * *Cl       in&out    Controls which traces and samples will be read.
 *                     See tfio.h for structure definition.
 * *nhdrs    out       Number of valid headers found on each trace.
 *                     Values will be placed in vector of size 64.
 * *hd      in & out   Will contain floating point headers from the
 *                     traces packed head to tail.
 * TASF      out       True amplitude scale factors.
 *                     Client must allocate memory(1 per trace + 1 extra).
 *                     True float amplitude = (byte-128) * TASF
 *
 *NOTES:
 * 1. This routine will not work if the ASCII header uses fewer
 *    bytes of storage than the converted float values.
 *
 *Revisions:
 *   DATE      WHO         DESCRIPTION
 *   --------  --------    --------------------------------------------
 *1. 92/04/20  R.S.Day     
C\END DOC
 *------------------------------------------------------------------*/
int  dcodehdr(int  *istat, struct GLBL *Glbls, struct Cntrl *Cl,
     int  *nhdrs, char *hd, float *TASF)
{ char hdwrk[640];
  float *fhdr;
  int  nch, nflds, i, j,  n,nhout;
  int  hoffset, hput;
  int  m;
  int  nc, sizf;
  int  bytes_in_header;
  int  js,jinc;
  *istat=4;
  *nhdrs=0;

/***************** HEADER TRANSLATION & TASFs ********************
 * Convert the ASCII header data to floating point values.
 * Do not attempt conversion if nhdwd<1 or nbyhd!=1 .
 * For ASCII headers nhdwd= No. of bytes per trace header string.
 * nflds...Is the true number of headers+1 per trace. The extra
 *         value is the largest absolute value for each trace. 
 * Check that TASF is non-null  */
   bytes_in_header = Glbls->nhdwd;
   if(Glbls->nbyhd!=1)     { *istat=3; return *istat; }
   if(Glbls->nbydp!=1)     { *istat=4; return *istat; }
   if(bytes_in_header<=0) { *istat=2; return *istat; }
   if(TASF==NULL)         { *istat=5; return *istat; }
   sizf = sizeof(float);

   nch=0;
   nflds=0;
   for(n=0;n<=bytes_in_header;n++)
     { if( hd[n]==' ' || hd[n]==',' || hd[n]=='\t' )
         { if(nch>0) {nflds += 1; nch=0; } }
       else
         { nch += 1; }
     }
   *nhdrs = nflds  < 1 ? 0 : nflds - 1;
   if(nflds*sizf > bytes_in_header) { *istat=1; *nhdrs=0; return *istat; }
   if(bytes_in_header > 640) { *istat=1; *nhdrs=0; return *istat; }

   nhout = HDROUT;
   if(nhout*sizeof(float) < bytes_in_header)
     { js = 0;  jinc = 1; }
   else
     { js = Cl->ntot-1; jinc = -1; }
/*   nhout = nhdrs;  */
   j = js;
   while( j >= 0 && j < Cl->ntot)
     { hoffset = j*bytes_in_header;
       strncpy(hdwrk,hd+hoffset,bytes_in_header);
       hdwrk[bytes_in_header]='\0';
       nch=0;
       nc=0;
       hput=j*nhout*sizf;
       for(i=0;i<bytes_in_header;i++) hd[hoffset+i]='\0';
       for(n=0;n<=bytes_in_header;n++)
       { if( hdwrk[n]==' ' || hdwrk[n]==',' || hdwrk[n]=='\t' )
         { if(nch>0) 
             { nc += 1;
               if(nc<nflds) sscanf(hdwrk+n-nch,"%f",hd+hput);
                else sscanf(hdwrk+n-nch,"%f",TASF+j);
               hput += sizf;
               nch=0; }
         }
         else  nch += 1;
       }
       if(Cl->samp1>1)  /* fix mutes for time windows */
         {m = j*nhout*sizf+sizf*(MUTHDR-1); fhdr = (float *)  (hd+m);
          *fhdr = (*fhdr - Cl->samp1 + 1)/Cl->sdec;
          if(*fhdr < .999) *fhdr = 1.0;
          m = j*nhout*sizf+sizf*(TAILHDR-1); fhdr = (float *)  (hd+m);
          *fhdr = (*fhdr - Cl->samp1 + 1)/Cl->sdec;
          if(*fhdr < .999) *fhdr = 1.0;
         }
       j += jinc;
     }

   *nhdrs = HDROUT; 

  *istat=0;
  return *istat;
}

int  dcodehd1(int  *istat, struct GLBL *Glbls,
     int  *nhdrs, char *hd, float *cpshd, float *TASF)
{ char hdwrk[640];
  int  nch, nflds, i, n,nhout;
  int  nc, sizf;
  int  bytes_in_header;
  *istat=4;
  *nhdrs=0;

/***************** HEADER TRANSLATION & TASFs ********************
 * Convert the ASCII header data to floating point values.
 * Do not attempt conversion if nhdwd<1 or nbyhd!=1 .
 * For ASCII headers nhdwd= No. of bytes per trace header string.
 * nflds...Is the true number of headers+1 per trace. The extra
 *         value is the largest absolute value for each trace. 
 * Check that TASF is non-null  */
   bytes_in_header = Glbls->nhdwd;
   if(Glbls->nbyhd!=1)     { *istat=3; return *istat; }
   if(Glbls->nbydp!=1)     { *istat=4; return *istat; }
   if(bytes_in_header<=0) { *istat=2; return *istat; }
   if(TASF==NULL)         { *istat=5; return *istat; }
   sizf = sizeof(float);

   nch=0;
   nflds=0;
   for(n=0;n<=bytes_in_header;n++)
     { if( hd[n]==' ' || hd[n]==',' || hd[n]=='\t' )
         { if(nch>0) {nflds += 1; nch=0; } }
       else
         { nch += 1; }
     }
   *nhdrs = nflds  < 1 ? 0 : nflds - 1;
   if(nflds*sizeof(float) > bytes_in_header)
    { *istat=1; *nhdrs=0; return *istat; }
   if(bytes_in_header > 640) { *istat=1; *nhdrs=0; return *istat; }

   nhout = HDROUT;
   strncpy(hdwrk,hd,bytes_in_header);
   hdwrk[bytes_in_header]='\0';
   nch=0;
   nc=0;
   for(i=0;i<HDROUT;i++) cpshd[i]= 0.0;
   for(n=0;n<=bytes_in_header;n++)
       { if( hdwrk[n]==' ' || hdwrk[n]==',' || hdwrk[n]=='\t' )
          {if(nch>0) 
            { nc += 1;
              if(nc<nflds) sscanf(hdwrk+n-nch,"%f",&cpshd[nc-1]);
               else sscanf(hdwrk+n-nch,"%f",TASF);
              nch=0;
            }
          }
         else  nch += 1;
       }

   *nhdrs = HDROUT; 

  *istat=0;
  return *istat;
}
