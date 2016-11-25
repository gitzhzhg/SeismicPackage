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
#include "tfdefs.h"
/* See tfdefs.h for prototypes */

#define ZEROBYT 128
#define MUTHDR  2
#define LAVHDR  25
#define TAILHDR 64
#define HDROUT  64

#define NORMYES  0
#define NORMNO   1

static float *labv_in_twin = NULL;
static int  length_of_arr = 0;
int read_dataMadj();
/*------------------------------------------------------------------
C\USER DOC
 *Name   : read_byt_
 *Purpose: Reads in trace and header data from a CPS byte file.
 *Author : R. Day
 *Date   : 98/11/09
 *
 *Function Definition:        ( Language = C )
 * int  read_byt_(char name[], int  *istat, struct Cntrl *Cl, int  *norm,
 *      int  *nhdrs, char hd[], unsigned char tr[],float *TASF)
 * *istat    output    0 if no errors occur.
 * name      in        Name of the CPS byte file.
 * *Cl       in&out    Controls which traces and samples will be read.
 *                     See tfio.h for structure definition.
 * *nhdrs    out       Number of valid headers found on each trace.
 *                     Values will be placed in vector of size 64.
 * *norm     in        Chooses normalizaion method.
 * hd[]      out       Will contain floating point headers from the
 *                     traces packed head to tail.
 * tr[]      out       Will contain the requested traces packed head
 *                     to tail.
 * TASF      out       True amplitude scale factors.
 *                     Client must allocate memory(1 per trace + 1 extra).
 *                     True amplitude value = (byte-128) * TASF
 *
 *NOTES:
 * 1. read_byt attempts to open name if it finds no data base entry
 * 2. This routine will not work if the ASCII header uses fewer
 *    bytes of storage than the converted float values.
 * 3. This routine calls tf_arr_rd_ which returns the headers in
 *    their ASCII form. It goes on to convert all headers to floats.
 *    Use tf_arr_rd for unconverted headers.
 * 4. Call read_byt for big requests, not for 1 trace at a time.
 *    Use tf_tr_rd for single trace requests.
 *
 *Revisions:
 *   DATE      WHO         DESCRIPTION
 *   --------  --------    --------------------------------------------
 *7. 98/11/09  R.S.Day     Allow names with glbl or byt extension.
 *6. 93/11/09  R.S.Day     Eliminated MAXSCR check
 *5. 92/11/10  R.S.Day     Fixed problem with mute adjustments.
 *4. 92/10/16  R.S.Day     Added TASF to argument and deactivated norm
 *                         Output 64 headers per trace.
 *3. 92/10/15  R.S.DAY     Reset Mute and Tail headers for tmin>0.
 *2. 92/06/02  R.S.Day     Clipped sc to avoid divide by zero on Cray.
 *1. 92/04/20  R.S.Day     hdwrk array increased from 600 to 640 bytes
C\END DOC
 *------------------------------------------------------------------*/
int  read_byt_(char name[], int  *istat, struct Cntrl *Cl, int  *norm,
     int  *nhdrs, char hd[], unsigned char tr[] ,float *TASF)
{ char msg[80], hdwrk[640], file_byt[80];
  char ext[8];
  float labv_in_win, lav_in_twin,lav_in_win;
  float sc,sc2;
  float *fhdr;
  int  ierr, nch, nflds, i, j,  n,nhout;
  int  pop[256], hoffset, hput;
  int  m, start, mode;
  int  lun, nc, sizf, op_stat;
  int  bytes_in_header;
  int  js,jinc;
  struct GLBL Glbls;
  *istat=4;
  *nhdrs=0;
/*  if(Cl->ntot > MAXSCR) Cl->ntot=MAXSCR; */
  if(Cl->sdec <= 0) Cl->sdec = 1;
/*
 * add byt extension to file name if it is missing */
  strcpy(ext,"byt");
  strcpy(file_byt,name);
  if(strstr(name,".BYT")!=0) {
     strcpy(ext,"BYT");
  }
  if(strstr(name,".glbl")!=0) {
     strcpy(ext,"byt");
  }
  if(strstr(name,".GLBL")!=0) {
     strcpy(ext,"BYT");
  }
  addext_rep_(file_byt,  ext,  &ierr);
 
  strcpy(Glbls.ftyp,"CBYTE");

/*
 * Check to see if file=name is open already. 
 * If it is not in the data base then:
 *     Crack the glbl file for the Glbls information. */
  get_global_data_(file_byt, &Glbls, &ierr);
  if(ierr > 0)
   { printf("read_byt: ierr=%d from get_global_data\n",ierr);
     *istat = ierr; return *istat; }

 if(ierr==0)
  {op_stat=0;
   tf_open_(&lun, file_byt,&Glbls,&op_stat,msg );
     if(strncmp(msg,"OK",2)!=0 && strncmp(msg,"03",2)!=0)
       { printf("read_byt: msg=$s from tf_open\n",msg);
         *istat=6; return *istat; }
  }
 else lun = Glbls.lun;

/******************** DATA RETRIEVAL ***************************
 * Read in the requested seismic data from the CPS BYUTE file */
 if(Cl->samp1+Cl->nsamp-1 > Glbls.ndptr) Cl->nsamp=Glbls.ndptr-Cl->samp1+1;
 tf_arr_rd_(&lun, Cl, hd,(char *) tr, msg);
 if(strncmp(msg,"OK",2) != 0 )
   {printf("%s \n",msg); *istat=3; return *istat;}

/***************** HEADER TRANSLATION & TASFs ********************
 * Convert the ASCII header data to floating point values.
 * Do not attempt conversion if nhdwd<1 or nbyhd!=1 .
 * For ASCII headers nhdwd= No. of bytes per trace header string.
 * nflds...Is the true number of headers+1 per trace. The extra
 *         value is the largest absolute value for each trace. 
 * Check that TASF is non-null  */
   bytes_in_header = Glbls.nhdwd;
   if(Glbls.nbyhd!=1)     { *istat=3; return *istat; }
   if(Glbls.nbydp!=1)     { *istat=4; return *istat; }
   if(bytes_in_header<=0) { *istat=2; return *istat; }
   if(TASF==NULL)         { *istat=5; return *istat; }
   sizf = sizeof(sc);

   nch=0;
   nflds=0;
   for(n=0;n<bytes_in_header;n++)
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
       if(Cl->samp1>1 && read_dataMadj()==1)  /* fix mutes for time windows */
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
/********************* BYTE LAVs ****************************
 * Scan byte values for the largest byte value in each trace*
 * Get the largest absolute byte value in each trace window *
 * pop = population curve for the byte image.               *
 *    0 <= labv_in_twin <= 127                              *
 ***********************************************************/
   if(length_of_arr < Cl->ntot+1 )
     { if(labv_in_twin != NULL ) free(labv_in_twin);
       labv_in_twin = (float *) malloc(( 1+ Cl->ntot) * sizf);
       length_of_arr = Cl->ntot;
     }
   if(Cl->trnsps==0)
    {mode = 0;
     start= 0;
     byte_data_lavs_(tr, &Cl->nsamp,&Cl->ntot,
          &Cl->nsamp, &start, &mode, labv_in_twin,pop,msg);}
   else
    {mode = 1;
     start= 0;
     byte_data_lavs_(tr, &Cl->ntot,&Cl->nsamp,
          &Cl->ntot, &start, &mode, labv_in_twin,pop,msg);}
   labv_in_win=labv_in_twin[Cl->ntot]; /* extra value is max for all */
   if(labv_in_win ==0 ) labv_in_win=1;
/***********************************************************/

/************** SCAN TRACES AND SET HEADER 25 **************/
/*
 * Scale the traces and compute the factors to bring the scaled
 * traces back to true amplitude values. */
   lav_in_win=0.0;
   for(i=0; i < Cl->ntot ; i++ )
     { 
       sc2 = (labv_in_twin[i]/127.0);
       if(sc2>1.0) sc2 = 1.0;
       lav_in_twin = *(TASF+i) * sc2 ;
       memcpy(hd+(i*HDROUT + LAVHDR-1)*sizf,&lav_in_twin,sizf); 
       if( lav_in_twin > lav_in_win) lav_in_win =lav_in_twin;
       sc = 1.0;
/*
       if(*norm == NORMYES || *norm == 5 )
         sc=1.0;
       else if(*norm == NORMNO || *norm == 4 ) 
         { sc= *(TASF+i)/Glbls.trmaxg; if(sc > 1. ) sc=1.;
           if(sc == 0.) sc = 1.0; }
       else if(*norm == 2 || *norm == 6 ) 
         { sc=1.0; if(labv_in_twin[i] > 0 ) sc=127.0/labv_in_twin[i]; }
       else if(*norm == 3 || *norm == 7 ) 
         { sc= 127.0/labv_in_win; if(sc < 1.) sc=1.0; }
       else      
         { sc=1.0; if(labv_in_twin[i] > 0 ) sc=1.0/labv_in_twin[i]; }

       mstrt = (Cl->trnsps==0) ? i*Cl->nsamp : i;
       mlast = (Cl->trnsps==0) ? mstrt+Cl->nsamp-1 : mstrt+Cl->ntot*Cl->nsamp-1;
       mstride = (Cl->trnsps==0) ? 1 : Cl->ntot ;
       if(sc != 1.0) for(m=mstrt; m<=mlast; m += mstride ) 
          { ic = ( tr[m]- ZEROBYT);
            tr[m] = ZEROBYT + (unsigned char) ( sc * ic + 0.5);
          }
*/
       *(TASF+i) = *(TASF+i)/(sc*127.0);
       labv_in_twin[i]=sc*labv_in_twin[i];
     }

   TASF[Cl->ntot] = lav_in_win/127.0;

  *istat=0;
  return *istat;
}

