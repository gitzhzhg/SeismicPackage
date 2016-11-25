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
#include <time.h>
#include <unistd.h>
#include <assert.h>

#include "tfdefs.h"
#include "tf_global.h"
#include "ebcdic.h"
#include "wrdcnvrt.h"
#include "dio.h"
#ifdef __hpux
#include "spws_hpux.h"
#endif



#define LAVHDR 25
#define SIGN(b) ((b) >= 0.0 ? 1.0 : -1.0)

/*
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C        1         2         3         4         5         6         7  |
C23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C  Primitive name:  DIO_    (DINT/DOUT UTILITIES)
C          Author:  Richard Day
C         Written:  96/02/22
C    Last revised:  99/06/26  Day
C
C  Purpose:   Set of C routines used by the DINT and DOUT processes.
C-----------------------------------------------------------------------
C                              NOTES
C
C  1.  The names of all routines in ths primitive start with the
C      characters DIO_.
C
C-----------------------------------------------------------------------
C Prototypes
C
C      char *dio_segy_h1ascii(char *h3200)
C        - build ascii version of 3200 byte segy header.
C      char *dio_segy_h3200(char *h3200, char *template)
C        - build ascii version of 3200 byte segy header using a template
C        - template, if non null, is a 3200 byte file to use.
C        - if template=="NONE" or NULL, then calls dio_segy_h1ascii
C      int dio_segy_h2set(char *h400,float *dt,int  *nt, int  *wdtyp,
C           int  *mf)
C        - sets key parameters for the segy 400 byte binary header.
C      int dio_segy_h2get(char *h400,float *dt,int  *nt, int  *wdtyp,
C           int  *mf)
C        - gets key parameters for the segy 400 byte binary header.
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C12.  99/06/26 Day        Cured a history card problem for TFILES
C11.  99/06/24 Day        Removed lseek & write calls in dio_hist
C                         and replaced with dskio_... calls.
C10.  99/03/08 Day        Changed byte offset from int to long.
C 9.  99/01/29 Day        Generalized to deal with SEGY files
C                         that may be either unsigned or signed
C                         byte data. Corrected = to  ==  bug.
C                         Conditional include of spws_hpux.h
C 8.  98/11/30 Day        dio_segy_h3200 added.
C                         Byte versions of segy files supported
C 7.  98/07/17 Day        Corrected logic which inserted extra
C                         blank lines into DOUT tfile histories.
C 6.  98/06/17 Day        Corrected T3E bug for binary segy header
C 5.  98/02/19 Goodger    Change what goes into SEGY words 7, 28B,
c                         and 29A to reflect SEGY standard.
C 4.  98/01/08 Day        Some revisions for Cray t3e support.
C                         Avoiding unsatisfied calls on the t3e.
C 3.  96/01/16 Day        Update for compatibility with SPWS
C 2.  96/10/21 Day        Incorporated SEGY changes a la STM
C                         and other small changes.
C 1.  96/07/17 Vunderink  Inserted into the conlib library.
C---------------------------------------------------------------------------
C                         PROGRAMMING NOTES
C---------------------------------------------------------------------------
C
C\END DOC
*/


/* OPEN A NEW FILE FOR OUTPUT */
/* fstyle = 8 for TFILE or 9 for DSEGY
 * byte   > 0 for byte data or =0 for non byte.
 * wdtyp  = 1-5,see wrdcnvrt.h for details.
 */
int  dio_open_(char *lfil,int  *ostat, int  *fstyle, int  *byte,
    int  *wdtyp, int  *mf, float *t0,  int  *nout, float *sr, int  *istat)
{
 CPSG *G;     /* CPS globals common block */
 TF_Global glbl;
 int  lun,ierr;
 int  nbyhd,nbydp,nbycll;
 char msg[81],buf1[3604],*s1=NULL;
 int  dpos=0;

 lun = 0;
 *istat = 0;
 if(lfil == NULL )
  { printf("dio_open: NULL local file name\n");
    *istat=1 ; return 0;
  }

/* retrieve cps globals */
/*wmm removed  G = (CPSG *) cps_global_addr_(); */
 /*dio_globals_(&G); */
 nbyhd = 4;
 nbydp = 4;
 if(*wdtyp==WIBM2) nbydp=2;
 if(*byte > 0 ) nbydp = 1;
 nbycll = *nout * nbydp + G->nwih * nbyhd;
 if(*fstyle==TFILE_TYPE)
  {
   glbl.nbycll  = *nout * nbydp + G->nwih * nbyhd;
   glbl.grecsiz = GRECSIZ;
   glbl.nbydp   = nbydp;
   glbl.nbyhd   = 4;
   glbl.wdtyp   = *wdtyp;/* see wrdcnvrt.h  */
   glbl.nhdwd   = G->nwih;
   glbl.hdtyp   = 0;     /* cps header type */
   strcpy(glbl.ftyp,"TFILE");
  }
 else
  {
   glbl.nbycll  = 240 + *nout * nbydp;
   glbl.grecsiz = 3600;
   glbl.nbydp   = nbydp;
   glbl.nbyhd   = 1;
   if(*wdtyp != WIBM && *wdtyp != WIBM2) *wdtyp = WIBM;
   glbl.wdtyp   = *wdtyp;/* see wrdcnvrt.h  */
   glbl.nhdwd   = 240;
   glbl.hdtyp   = 2;     /* segy header type */
   strcpy(glbl.ftyp,"DSEGY");
  }
 glbl.ntrfil  = 0;
 glbl.ntrcll  = 1;
 glbl.ntb     = 0;
 glbl.numhc   = 0;
 glbl.trmaxg  = 0.;
 glbl.dunits  = *mf;
 glbl.lun     = 0;
 glbl.opstat  = *ostat;       /* store opstat for tf_close_ */
 glbl.ndptr   = *nout;
 glbl.srval   = *sr;
 glbl.tstrt   = *t0;
 glbl.xorg    = G->xorg;
 glbl.yorg    = G->yorg;
 memcpy(glbl.dx0,G->dx0,4*sizeof(float));
 memcpy(glbl.dn0,G->dn0,4*sizeof(float));
 
 lun    = 0;
 /* ostat , 4= new & r-w ,2= old & r-w*/
 *istat = tf_open_(&lun,lfil,&glbl,ostat,msg);
 if(*istat != 0)
  { printf("dio_open: failed tf_open_() call, file=%s\n",lfil);
    printf("dio_open: %s\n",msg);
    *istat = 4; return 0;
  }

 if(*fstyle==0)
  {
    ierr = tf_glbl_wr_( &lun, &glbl, msg );
  }
 else
  { dio_segy_h1ascii(buf1);
    s1 = dio_segy_h1asceb(buf1,0);
    memcpy(buf1,s1,3200);
    dio_segy_h2set(buf1+3200, sr,nout,wdtyp,&glbl.dunits);
    dskiocwr_(&lun,buf1,&glbl.grecsiz,&dpos,&ierr);
    if(s1) free(s1);
  }

 *istat= 0;
 return lun;
}

void dio_max_(int  *lun, float *trmaxg)
{
 int  istat,one=1;
 if( lun==NULL || trmaxg == NULL) return;
 if(*lun <= 0) return;
 istat = tf_glbl_add1_("trmaxg",&one,(char *) trmaxg, &istat,lun);
}

/*
 * Construct a 3200 byte ascii string for the segy header
 */
char *dio_segy_h1ascii(char *h3200)
{ char card[82],*timex;
  time_t tp;
  int i;
  tp = time(NULL);
  timex = ctime(&tp);
  if(!h3200) return NULL;
  h3200[0] = '\0';
  for(i=0;i<40;i++)
   {/* sprintf(card,"C%-78d\n",i+1); */
    sprintf(card,"C%2d%76s\n",i+1," ");
    if(i==0) strncpy(card+7,"CONOCO SEGY FILE",16);
    if(i==1) strncpy(card+7,timex,strlen(timex)-1);
    if(i==2) strncpy(card+7,"IBM 32 BIT FLOAT",16);
    if(i==39) strncpy(card+4,"END EBCDIC",10);
    strcat(h3200,card);
   }
 h3200[3200]='\0';
 return h3200;

}

char *dio_segy_h3200(char *h3200, char *template)
{ FILE *fp;
  char card[82];
  int  i;
  if(!template) return dio_segy_h1ascii(h3200);
  if(strcmp(template,"NONE")==0) return dio_segy_h1ascii(h3200);
  h3200[0] = '\0';
  if(!access(template,R_OK)) {
    fp = fopen(template,"r");
    for(i=0;i<40;i++) {
      if (fgets(card,82,fp) != NULL)  {
        strncat(h3200,card,80);
      }
      else
        break;
    }
     fclose(fp);
 }
 h3200[3200]='\0';
 return h3200;
}

/*
 * char *segy_cnvrth1(char *ascii, char *ebcdic);
 * Purpose: translate between ascii and ebcdic.
 * swap 3200 bytes of ascii and ebcdic data in the buffers.
 * convert data in ebcdic to ascii and hold in ascii.
 * convert data in ascii to ebcdic and hold in ebcdic.
 * returned char * is null terminated. The input buffers
 * may or may not be null terminated.
 * If one buffer is NULL then an allocated string is
 * returned with the translated data.
 */
char *dio_segy_h1asceb(char *ascii, char *ebcdic)
{char c,*s=NULL;
 int i;
 assert(!(ascii==NULL && ebcdic==NULL));
 if(ascii==NULL || ebcdic==NULL)
  { s = (char *) malloc(3201); }

 for (i = 0; i < 3200; i++)
 if(ebcdic==NULL)
  {
   for (i = 0; i < 3200; i++)
    {s[i] = ascii_to_ebcdic[ascii[i]]; }
   s[3200]  = '\0';
  }
 else if(ascii==NULL)
  {
   for (i = 0; i < 3200; i++)
    {s[i] = ebcdic_to_ascii[ebcdic[i]]; }
    s[3200]  = '\0';
  }
 else
  {
   for (i = 0; i < 3200; i++)
    {c = ebcdic[i];
     ebcdic[i] = ascii_to_ebcdic[ascii[i]];
     ascii[i]  = ebcdic_to_ascii[c]; 
    }
   ebcdic[3200] = '\0';
   ascii[3200]  = '\0';
  }

 return s;
}

/*
 * Put data to the 400byte binary segy header buffer.
 */
int dio_segy_h2set(char *h400,float *dt,int  *nt, int  *wdtyp, int  *mf)
{int bp=0,ier,i,idt,code,ifor,doit=1;
 int  one=1,zero=0,two=2,natlen,forlen,type;
 if(!h400) return 0;

 ier=0;
 idt = *dt*1000000;
 if(*wdtyp==WIBM)  code =1;
 if(*wdtyp==WIBM2) code =3;
 if(*wdtyp==WSBYT) code =5;
 if(*wdtyp==WBYTE) code =6;
 for(i=0;i<400;i++) h400[i] = '\0';
#ifdef _CRAY1
 bp=17; USICTI(&idt, h400,&bp,&one,&two,&ier); 
 bp=21; USICTI(nt, h400,&bp,&one,&two,&ier);
 bp=25; USICTI(&code, h400,&bp,&one,&two,&ier);
 bp=29; USICTI(&two, h400,&bp,&one,&two,&ier);
 bp=55; USICTI(mf, h400,&bp,&one,&two,&ier);
 doit=0;
#endif
#ifdef _CRAYMPP
 type=2;
 natlen=64;
 forlen=16;
 ier = CRI2IBM(&type,&one,&ifor,&zero,&idt,&one,&natlen,&forlen);
 memcpy(h400+16,&ifor,2);
 ier = CRI2IBM(&type,&one,&ifor,&zero,nt,&one,&natlen,&forlen);
 memcpy(h400+20,&ifor,2);
 ier = CRI2IBM(&type,&one,&ifor,&zero,&code,&one,&natlen,&forlen);
 memcpy(h400+24,&ifor,2);
 ier = CRI2IBM(&type,&one,&ifor,&zero,&two,&one,&natlen,&forlen);
 memcpy(h400+28,&ifor,2);
 ier = CRI2IBM(&type,&one,&ifor,&zero,mf,&one,&natlen,&forlen);
 memcpy(h400+54,&ifor,2);
 doit=0;
#endif
 if(doit==1) {
   wrdc_bldstr(&idt,(unsigned char *)(h400+16),2);
   wrdc_bldstr(nt,(unsigned char *)(h400+20),2);
   wrdc_bldstr(&code,(unsigned char *)(h400+24),2);
   wrdc_bldstr(&two,(unsigned char *)(h400+28),2);
   wrdc_bldstr(mf,(unsigned char *)(h400+54),2);
 }
 return 1;
}

/*
 * Get data from the 400byte binary segy header buffer
 */
int dio_segy_h2get(char *h400,float *dt,int  *nt, int  *wdtyp, int  *mf)
{int idt,code;
 
 *dt=0;
 *wdtyp=0;
 if(!h400) return 0;
 wrdc_convstr((unsigned char *) &h400[20],(int *) nt,2);
 wrdc_convstr((unsigned char *) &h400[16], &idt,2);
 wrdc_convstr((unsigned char *) &h400[24], &code,2);
 wrdc_convstr((unsigned char *) &h400[54], (int *) mf,2);
 *dt = idt * 1.e-06;
 if(code==1) *wdtyp = WIBM;
 if(code==3) *wdtyp = WIBM2;
 if(code==5) *wdtyp = WSBYT;
 if(code>5)  *wdtyp = WBYTE;

 return 1;
}


void dio_segy_hbld(float *hd, int *count,float *dt,
     int *nsout,char *buf)
{
  int itd[80];
  int j,nsgy=60,zero=0,one=1,two=2,four=4,type=1;
  int iv,ier,l,doit=1;
  for(j=0;j<nsgy;j++) itd[j]=0;

  itd[0] = *count;
  itd[1] = *count;       /*Seq. No. within reel*/
  itd[2] =hd[8]+SIGN(hd[8])*0.5;    /*Original field record No.*/
  itd[3] =hd[9]+.5;    /*Trace No. within field record*/
  itd[4] =hd[36]+SIGN(hd[36])*0.5;   /*Energy source point number*/
  itd[5] =hd[6]+SIGN(hd[6])*0.5;    /*CDP ensemble number*/
  itd[6] =hd[3]+.5;    /*Trace number within current group */
  itd[9]=hd[5]+SIGN(hd[5])*0.5;   /*S to R distance-line direction=+*/
  itd[10]=hd[15]+SIGN(hd[15])*0.5;   /*Receiver group elevation*/
  itd[11]=hd[12]+SIGN(hd[12])*0.5;   /*Surface elevation at source*/
  itd[12]=hd[19]+SIGN(hd[19])*0.5;   /*Source depth below surface*/
  itd[15]=hd[12]+SIGN(hd[12])*0.5;   /*soruce water depth STM */
  itd[16]=hd[15]+SIGN(hd[15])*0.5;   /*receiver water depth STM */
  itd[18]=hd[10]+SIGN(hd[10])*0.5;   /*soruce x*/
  itd[19]=hd[11]+SIGN(hd[11])*0.5;   /*soruce Y*/
  itd[20]=hd[13]+SIGN(hd[13])*0.5;   /*receiver x*/
  itd[21]=hd[14]+SIGN(hd[14])*0.5;   /*receiver y*/
  itd[45]=hd[18]+SIGN(hd[18])*0.5;   /*Elevation*/

  itd[47]=hd[16]+SIGN(hd[16])*0.5;   /*Mid-point x Loc. bsmt x*/
  itd[48]=hd[17]+SIGN(hd[17])*0.5;   /*Mid-point y Loc. bsmt y*/

  itd[58]=hd[16]+.5;   /*Mid-point x Loc. bsmt x for EXXON 1986*/
  itd[59]=hd[7]+.5;    /*Mid-point y Loc. for Loumos*/

/*************  Use optional headers to store some statics ******/
/*************  and sequential ground position *****************/
  itd[49]=hd[38]+SIGN(hd[38])*0.5;   /*Shift Pre STM */
  itd[50]=hd[39]+SIGN(hd[39])*0.5;   /*Shift Post STM */
  itd[51]=hd[55]+SIGN(hd[55])*0.5;   /*Shift RPre STM */
  itd[52]=hd[56]+SIGN(hd[56])*0.5;   /*Shift RPOST STM */
  itd[53]=hd[45]+SIGN(hd[45])*0.5;   /*Shift Seq Source G. Pos. */
  itd[54]=hd[46]+SIGN(hd[46])*0.5;   /*Shift Seq Rec G. Pos. STM */

  ier=0;
  l = nsgy;
  iv=1;
#ifdef _CRAY1
  USICTI(itd, buf,&one,&l,&four,&ier); /*scilib*/
  l=29; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 8A*/
  l=31; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 8B*/
  l=33; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 9A*/
  l=35; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 9B*/
  l=69; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 18A*/
  l=71; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 18B*/
  l=89; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 23A*/
  iv=hd[43]+.5;
  l=95; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 24B*/
  iv=hd[44]+.5;
  l=97; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 25A*/
  /*
  FIX SEGY hd 29A MUTE TIME FORM CRAY hd 2 MUTE INDEX
  29A=(HDR2-2)*(RATE*1000)
  */
  iv=hd[42]+SIGN(hd[42])*0.5;
  l=103;USICTI(&iv,  buf,&l,&one,&two,&ier);
  iv=0;
  l=111; USICTI(&iv,  buf,&l,&one,&two,&ier);
  iv=(hd[1])*(*dt*1000)+.00005; /* mute end time */
  l=113; USICTI(&iv,  buf,&l,&one,&two,&ier); /*SEGY 29A */
  iv= *nsout;
  l=115; USICTI(&iv,  buf,&l,&one,&two,&ier);/*SEGY 29B*/
  iv= *dt*1000000.;
  l=117;   USICTI(&iv,  buf,&l,&one,&two,&ier);/*SEGY 30A*/
  iv=hd[4]+.5;
  l=31; USICTI(&iv,  buf,&l,&one,&two,&ier);/*SEGY 8B*/
  /*      Put CRAY HDR 43 into SEGY HDR 51 as a FP static value*/
  /*******REMOVE and put into byte 103 STM ***********/
 doit=0;

#endif
  if(doit==1) {
    for(l=0;l<nsgy;l++) {
     wrdc_bldstr(itd+l,(unsigned char *) (buf+4*l),4);
    }
    wrdc_bldstr(&one,(unsigned char *) (buf+28),2);
    wrdc_bldstr(&one,(unsigned char *) (buf+30),2);
    wrdc_bldstr(&one,(unsigned char *) (buf+32),2);
    wrdc_bldstr(&one,(unsigned char *) (buf+34),2);
    wrdc_bldstr(&one,(unsigned char *) (buf+68),2);
    wrdc_bldstr(&one,(unsigned char *) (buf+70),2);
    wrdc_bldstr(&one,(unsigned char *) (buf+88),2);
    iv=hd[43]+.5;
    wrdc_bldstr(&iv,(unsigned char *) (buf+94),2);
    iv=hd[44]+.5;
    wrdc_bldstr(&iv,(unsigned char *) (buf+96),2);
    iv=hd[42]+SIGN(hd[42])*0.5;
    wrdc_bldstr(&iv,(unsigned char *) (buf+102),2);
    iv=0;
    wrdc_bldstr(&iv,(unsigned char *) (buf+110),2);
    iv=(hd[1])*(*dt*1000)+.00005; /* mute end time */
    wrdc_bldstr(&iv,(unsigned char *) (buf+112),2);
    iv= *nsout;
    wrdc_bldstr(&iv,(unsigned char *) (buf+114),2);
    iv= *dt*1000000.;
    wrdc_bldstr(&iv,(unsigned char *) (buf+116),2);
    iv=hd[4]+.5;
    wrdc_bldstr(&iv,(unsigned char *) (buf+30),2);
  }
}


int  dio_wrtr_(char *name, int  *istat, int  *irec,
     float *hd, float *tr, int  *ns)
{char  msg[80], file_data[80];
 float lav;
 int  i,l,n,nhdwd;
 int  num, ierr, ites;
 int  wdtypi,wdtypo;
 int  zero=0, one=1;
 int  op_stat, cl_stat=0, lun;
 char  hdbuf[512];
 TF_Global G;
 char *fbuf;
 *istat=4;
#ifdef CRAY
 wdtypi=WCRAY;
#elif (POSIX || unix || _AIX || __hpux)
 wdtypi=WIEEE;
#elif VMS
 wdtypi=WVMS;
#endif
 G.lun =0;
 strcpy(file_data,name);     /* Copy file name      */
 strcpy(G.ftyp,"?????"); /* Dont know file type */
/********************************************************
 *** Check to see if file_data is open already.       *** 
 *** It must exist since dio_wrtr_ does not create.
 *** 1. open the file and get the glbls information.  ***
 *** 2. close the file( reopen with a call to tf_open)***
 *** If it is already open get_global_data_ will leave***
 ***    it open, and return the negative of the unit  ***
 ***    number for the error status. Glbls will be set***
 *******************************************************/
 l = get_global_data_(file_data,&G,&ierr);
 if(ierr > 0)
  { sprintf(msg,"dio_wrtr_: ierr=%d from get_global_data",ierr);
    printf("file=%s\n",file_data);
    *istat = ierr; goto error;
  }

 ites=1;
 if(G.wdtyp < 1) G.wdtyp=WIEEE;
 if(strncmp(G.ftyp,"TFILE",5)==0 ) ites=0;
 if(strncmp(G.ftyp,"DSEGY",5)==0 ) ites=0;
 if(ites!=0 ) 
  { strcpy(msg,"dio_wrtr_: invalid file format for write");
     *istat = 2; goto error;
  }
/******************** DATA CONVERSION ********************
 * 2. tf_open is aware of files that are already open *** 
 * 3. bypass tf_open if we know the file is open.     ***
 *******************************************************/
 lun = G.lun; /* file already open */
 if(ierr==0 )
  {op_stat=2;
   tf_open_(&lun, file_data,&G,&op_stat,msg );
     if(strncmp(msg,"OK",2)!=0 && strncmp(msg,"03",2)!=0)
       { printf("dio_wrtr_: msg=%s from tf_open\n",msg);
         *istat=6; goto error;
       }
  }

/*************************************************************
 * Convert data to foreign format                          ***
 ************************************************************/
 n = *ns;
 lav = fabs(tr[0]);
 for(i=0;i<n;i++)
   lav = (fabs(tr[i]) > lav) ? fabs(tr[i]) : lav;
 hd[LAVHDR-1] = lav;

 n = G.ndptr*G.nbydp;
 fbuf = dio_iobuf(n);
 for(i=0;i<n;i++) fbuf[i] = '\0';

 wdtypo = G.wdtyp;
/* Convert trace values
 * Will convert only what fits into trace buffer
 */
 num = (*ns > G.ndptr) ? G.ndptr : *ns;
 if(G.nbydp==1)
   {
    float_to_byt_(tr,&num,(unsigned char *) fbuf, &num, &lav);
   }
 else if(wdtypo==WIBM2)
   {
    float_to_bit16_(tr,&num,(char *) fbuf, &lav);
   }
 else
   {
    l = wrdc_fcnvrt_(&wdtypi,(char *) tr,&wdtypo, fbuf, &num, msg);
    if(l != 0) {goto error; }
   }

/* convert header values */
 if(strcmp(G.ftyp,"TFILE")==0)
   {nhdwd = (int) G.nhdwd;
    l = wrdc_fcnvrt_(&wdtypi,(char *) hd,&wdtypo, hdbuf, &nhdwd, msg);
    if(l != 0) {goto error; }
   }
 else
   {/* build segy header */
    int nn  = G.ndptr;
    dio_segy_hbld(hd, irec, &G.srval, &nn,(char *) hdbuf);
   }

/* write data to output file */
 tf_tr_wr_(&lun, irec, &one, (char *) hdbuf,(char *)  fbuf, msg);
 if(strncmp(msg,"OK",2) != 0 )
   { *istat=3; goto error;}



 *istat=0;
 return *istat;

error:
 printf("dio_wrtr_: ERROR\ndio_wrtr_: %s\n",msg);
 if(lun != 0 )
   {cl_stat=0; /* keep the file */
    tf_close_(&lun,&cl_stat,msg);
    lun=0;
    G.lun =0;
   }
 return *istat;

}


/*------------------------------------------------------------
C\USER DOC
 *Name   : dio_wr_
 *Purpose: Convert a buffer of floats and output to disk
 *Author : R. Day
 *Date   : 96/02/22
 *
 *Function Definition:        ( Language = C )
 * void dio_wr_(int *fd, int *num, long *off, float *buf,
 *              int *iby,float *lav);
 *
 * fd   in  file descriptor of an open file
 * num  in  number of data points to output.
 * off  in  offset in bytes from start of the file.
 * buf  in  floating point input data.
 * iby  in  if 1 then convert the floats to bytes.
 * lav  out returns the largest abs. value from buf
 *NOTES:
 * 1. The input data is converted to IEEE f.p. or 8bit bytes
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    -------------------------------------
C\END DOC
 *-----------------------------------------------------------*/
void dio_wr_(int  *fd,int  *num,long  *off,float *buf,int  *iby,float *lav)
{int   i,l,n;
 int   wdtypi,wdtypo = WIEEE;
 int nby;
 char  *fbuf,msg[96];
 float av;

#ifdef CRAY
 wdtypi=WCRAY;
#elif (POSIX || unix || _AIX || __hpux)
 wdtypi=WIEEE;
#elif VMS
 wdtypi=WVMS;
#endif
 n = *num;
 av = fabs(buf[0]);
 for(i=0;i<n;i++)
   av = (fabs(buf[i]) > av) ? fabs(buf[i]) : av;
 *lav = av;

 fbuf = dio_iobuf(n);
 if(*iby!=0)
   {
    float_to_byt_(buf,&n,(unsigned char *) fbuf, &n, &av);
   }
 else
   {
    l = wrdc_fcnvrt_(&wdtypi,(char *) buf,&wdtypo, fbuf, &n, msg);
    if(l != 0) {goto error; }
   }

 nby=n;
 if(iby ==0) nby = 4*n;
 dskiowr_(fd, off,&nby,fbuf);
 return;
  error:
 printf("dio_wr_: we had a problem, offset=%d\n",*off);
 return;
}

/*
 * Write the ascii header for a Gocad Voxet
 * Storage order for GRID is:
 * order=0 z,x,y (i.e. n1=z,n2=x,n3=y)
 * order=1 z,y,x
 */
int dio_wrvoxhdr_(char *hfile, char *dfile, char *pname, char *obj_name,
    float  *o1, float  *o2, float  *o3,
    int   *n1, int   *n2, int   *n3,
    float *d1, float *d2, float *d3, 
    char *lab1, char *lab2, char *lab3, int  *order,
    int  *wtype)
{FILE *fp=NULL;
 int   dsize;
 float b1,b2,b3;

 if(!hfile)  return 0;
 dsize = 1;
 if(*wtype==WIEEE || *wtype==WIBM) dsize=4;
 if(strcmp(dfile,"SAME") == 0 ) return 0;
 if(strcmp(dfile,"NONE") == 0 ) return 0;

 if(*n1<=0 || *n2<=0 || *n3<=0) return 0;
 if(*d1==0.) *d1=1.0;
 if(*d2==0.) *d2=1.0;
 if(*d3==0.) *d3=1.0;
 fp = fopen(hfile,"a+");
 if(!fp)  goto error;

 b1 = *o1 + *d1*(*n1-1);
 b2 = *o2 + *d2*(*n2-1);
 b3 = *o3 + *d3*(*n3-1);
 fprintf(fp,"GOCAD Voxet 1.0\n");
 fprintf(fp,"HEADER {\n");
 if(obj_name) fprintf(fp,"name:%s\n",obj_name);
 else         fprintf(fp,"name: voxdata\n");
 fprintf(fp,"*axis:on\n");
 fprintf(fp,"*grid:off\n");
 fprintf(fp,"}\n");
 fprintf(fp,"AXIS_O %f %f %f\n",*o1,*o2,*o3);
 if(*order==0)
  {
   fprintf(fp,"AXIS_U %f %f %f\n",0.0,0.0,b1);
   fprintf(fp,"AXIS_V %f %f %f\n",b2,0.0,0.0);
   fprintf(fp,"AXIS_W %f %f %f\n",0.0,b3,0.0);
  }
 else if(*order==1)
  {
   fprintf(fp,"AXIS_U %f %f %f\n",0.0,0.0,b1);
   fprintf(fp,"AXIS_V %f %f %f\n",0.0,b2,0.0);
   fprintf(fp,"AXIS_W %f %f %f\n",b3,0.0,0.0);
  }
 fprintf(fp,"AXIS_N %d %d %d\n",*n1,*n2,*n3);
 fprintf(fp,"AXIS_MIN %f %f %f\n",0.0,0.0,0.0);
 fprintf(fp,"AXIS_MAX %f %f %f\n",1.0,1.0,1.0);
 fprintf(fp,"AXIS_NAME \"%s\" \"%s\" \"%s\"\n",lab1,lab2,lab3); 
 fprintf(fp,"AXIS_TYPE even even even\n");
 fprintf(fp,"\n");
 if(pname) fprintf(fp,"PROPERTY  1 \"%s\"\n",pname);
 if(dfile) fprintf(fp,"PROP_FILE 1 %s\n",dfile);
 fprintf(fp,"PROP_ESIZE 1 %d\n",dsize);
 if(*wtype==WIEEE) fprintf(fp,"PROP_ETYPE 1 IEEE\n");
 if(*wtype==WIBM ) fprintf(fp,"PROP_ETYPE 1 IBM\n");
 if(*wtype==WBYTE) fprintf(fp,"PROP_ETYPE 1 BYTE\n");
 if(*wtype==WSBYT) fprintf(fp,"PROP_ETYPE 1 BYTE\n");
 fprintf(fp,"END");
 fflush(fp);

 return 1;
 error:
 if(fp) fclose(fp);
 return 0;
}


char *dio_iobuf(int nbytes)
{static int  iobufsiz;
 static char *fbuf;
 int  bsiz;

 if(fbuf == NULL || (nbytes > iobufsiz) )
  {bsiz = nbytes;
   if(bsiz < 32768) bsiz = 32768;
   if(fbuf) { free(fbuf); fbuf = NULL; }
   fbuf = (char *) calloc(1,bsiz);
   iobufsiz = bsiz;
  }
 return fbuf;
}
/*
 * name = name of the temporary history file
 * lun = file descriptor for the output TFILE
 * read in history lines, pad to 80 bytes  */
int dio_hist_(int  *lun, char *name)
{FILE *fp=NULL;
 int   one=1,hcsiz=80,hcpos=0,start=0;
 long  loff;
 int   i,j,nbuf=0,bsiz=1024;
 int   nc=0,nc0=1,ntowr,istat,i_err;
 char  card[160],dbuf[1024],msg[120],*cpntr;
 TF_Global G;

 if(!name ) return 0;
 if(!lun)   return 0;
 fp = fopen(name,"r");
 if(!fp) {
   printf("dio_hist: open failure, file=%s\n",name);
   return 0;
 }
 i_err =  tf_glbl_get_( &istat, lun, &G);
 if(i_err!=0) {
   printf("dio_hist: tf_glbl_get failure, file=%s\n",name);
   return 0;
 }
 if(strstr(G.ftyp,"TFILE") == NULL) {
   printf("dio_hist: not a tfile, \n");
   return 0;
 }

 hcpos = tf_global_first_hcbyt(&G) +  nc*hcsiz;
 
 loff=hcpos;
 dskio_xsk_(lun,&loff,&i_err);
 if(i_err==-1) return 0;

 dbuf[0]='\0';
 hcpos=0;
 card[0]=' ';
 while((cpntr= fgets(card+1,hcsiz+2,fp)) != NULL)
 {
  card[81]='\0';
  if(ferror(fp)) break;
  if(start==0)
   {if(strstr(card,"Current history record")!= NULL) start=1;}
  if( start)
   {card[0] = '#'; /* makes history card a comment for dcd */
    for(i=0;i<hcsiz;i++) 
      if(card[i]=='\n') break;
    for(j=i;j<hcsiz;j++) card[j]=' ';
    if(i> 0)
     { 
       memcpy(dbuf+hcpos,card,hcsiz);
       nc++;
       hcpos += hcsiz;
     }
    if(hcpos >= bsiz - hcsiz)
     {ntowr = hcpos/hcsiz;
      i_err = tf_hc_wr_(lun,&nc0,&ntowr,dbuf,msg);
      if(i_err) printf(" %s\n",msg);
      hcpos=0;
      nc0=nc;
     }
   }
  if(feof(fp)) break;
 }

 ntowr = hcpos/hcsiz;
 if(hcpos>0 && start)
   i_err = tf_hc_wr_(lun,&nc0,&ntowr,dbuf,msg);
 fclose(fp);
 return nc;
}

#undef SIGN

