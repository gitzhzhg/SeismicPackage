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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "ebcdic.h"

#ifdef NEED_CAPITALS
#define get_inithis_addr_ GET_INITHIS_ADDR
#define chist_rdall_  CHIST_RDALL
#define chistcl_ CHISTCL
#define chistwr_ CHISTWR
#define chistrd_ CHISTRD
#define chistwr_file_ CHISTWR_FILE
#define chistrd_file_ CHISTRD_FILE
#define chistrew_ CHISTREW
#define chist_snam_ CHIST_SNAM
#define chist_name_ CHIST_NAME
#define chist_mv_    CHIST_MV
#define chist_get_reel_  CHIST_GET_REEL
#define chist_put_reel_  CHIST_PUT_REEL
#define chist_date_sort_ CHIST_DATE_SORT
#define chist_coni_ CHIST_CONI
#define chist_gettp_ CHIST_GETTP
#define chist_dsasc_ CHIST_DSASC
#define chist_u6064_ CHIST_U6064
#define chist_int6064_ CHIST_INT6064
#define chist_fp6064_ CHIST_FP6064
#define chist_usictc_ CHIST_USICTC
#define chist_usicti_ CHIST_USICTI
#endif
#if (VMS || _AIX || hpux)
#define get_inithis_addr_ get_inithis_addr
#define chist_rdall_  chist_rdall
#define chistcl_ chistcl
#define chistwr_ chistwr
#define chistrd_ chistrd
#define chistwr_file_ chistwr_file
#define chistrd_file_ chistrd_file
#define chistrew_ chistrew
#define chist_snam_ chist_snam
#define chist_name_ chist_name
#define chist_mv_    chist_mv
#define chist_get_reel_    chist_get_reel
#define chist_put_reel_    chist_put_reel
#define chist_date_sort_ chist_date_sort
#define chist_coni_ chist_coni
#define chist_gettp_ chist_gettp
#define chist_dsasc_ chist_dsasc
#define chist_u6064_ chist_u6064
#define chist_int6064_ chist_int6064
#define chist_fp6064_ chist_fp6064
#define chist_usictc_ chist_usictc
#define chist_usicti_ chist_usicti
#endif
#ifdef sun
#include <spws_sunos.h>
#endif

#define MAXIPN 199

/* C-structure that is an Image of the INITHIS Common Block */
typedef struct inithis_com
   {long nprocn;
    char procn[8*MAXIPN];
    long nprocc[MAXIPN];
    long nhrbad;
   } InitHistCom;

/* Hooks for maintaining multiple historys */
typedef struct HTABLE
 { long ipn;         /* process number    */
   char name[16];    /* history file name */
   FILE *ifile;      /* STREAM pointer    */
   long mode;        /* open mode 0,1 - w+,r+ */
   long ncards;      /* card count in name */
   InitHistCom   *chist;
   struct HTABLE *prev;
   struct HTABLE *next;
 } Htable;

typedef struct HCHAIN {
   Htable *first;
   Htable *last;
   long   nlink;     /* count of open files */
} Hchain;

typedef struct _KeyDate {
 int key;
 int date; } KeyDate;

/* Prototypes of methods defined in this file **
 **********************************************/
int   inithis_nproc(InitHistCom *h);
char *inithis_proc(long ipn, InitHistCom *h);
FILE *chistop(char *name,long  mode);
void  chistcl_(char *name);
int   chistwr_  (long *ipn, char *card);
int   chistrd_ (long *ipn,char *pname, char *card);
int   chistwr_file_(char *file, char *card);
int   chistrd_file_(char *file, char *card);
int   chistwr_base(FILE *ifile, char *buf);
int   chist_rdall_(int *mipn, long *nhc, int *mcrd, char *buf);
void  chistrew_(long *ipn, long *ncards);
void  chist_snam_(char *name);
char *chist_gnam();
Htable *chist_getby_ipn(long ipn);
Htable *chist_getby_name(char *name);
void chist_ipn2file(long *ipn,char *file);
void chist_ipn2procn(long *ipn,char *procn);
void chist_name_(long *ipn, char *procn, char *file);
void chist_get_reel_(char *proj, char *reel);
void chist_put_reel_(char *proj, char *reel);
void chist_date_sort_(int *num, int *dates, int *indices);
int  chist_dcmp(const void *dk,const  void *d);
/* wrappers around cray pvp only routines */
void chist_coni_(char *bufin, char *bufout, int *NHISTE,
                 int *NHISTO, int *NHISTD, int *NHISLAV,
                 int *NHISTT, char *itape);
void chist_gettp_(char *unit, int *n, int *infotap,int *ierr);
int  chist_dsasc_(char *data, int *off, char *out, int *n);
int  chist_u6064_(char *data, int *off, char *out, int *n);
int  chist_int6064_(char *data,  char *out, int *n);
int  chist_fp6064_(char *data,  char *out, int *n);
int  chist_usictc_(char *data, int *sby, char *out, int *n,int *len);
int  chist_usicti_(char *data, char *out, int *sby, int *n,int *len);


Htable *Htable_new();
void    Htable_destroy(Htable *h);
Htable *Htable_next(Htable *h);
Htable *Htable_prev(Htable *h);
void    Htable_gipn_name(Htable *h, char *procn);
char   *Htable_gname(Htable *h);
void    Htable_sname(Htable *h, char *name);
void    Htable_sipn(Htable *h, long ipn);
long    Htable_gipn(Htable *h);
void    Htable_sncard(Htable *h, long ndat);
long    Htable_gncard(Htable *h);
void    Htable_smode(Htable *h, long mode);
long    Htable_gmode(Htable *h);
FILE   *Htable_gfile(Htable *h);
void    Htable_sfile(Htable *h, FILE *ifile);
InitHistCom *Htable_gchist(Htable *h);
void    Htable_schist(Htable *h, InitHistCom *chist);

Htable *Hchain_gfirst(Hchain *hc);
void Hchain_sfirst(Hchain *hc, Htable *first);
void Hchain_slast(Hchain *hc, Htable *h);
long Hchain_gnlink(Hchain *hc);
void Hchain_snlink(Hchain *hc, long nlink);
void Hchain_addlnk(Hchain *hc, Htable *h);
void Hchain_rmlink(Hchain *hc, Htable *h);

/* Local variables */


static Hchain hchain;
static char current_history_file[80];

/*************************************************************************
C\USER DOC
 * -----------------------------------------------------------------------
 *                         CRAY PROCESSING SYSTEM
 *                     EXPLORATION RESEARCH DIVISION
 *                              CONOCO, INC.
 *
 *  Process name: chist*
 *        Author: R.S. Day
 *  Last revised: 98/01/18
 *
 *  Purpose: The functions in this file are for maintaining a current
 *           history file for a CPS job. There are functions to read,
 *           write, open, and close current history files. There are
 *           also routines to help sort history records, and manipulate
 *           project histories. All functions in this file are primitives
 *           to assist history construction whithin bhist, whist, etc.
 * -----------------------------------------------------------------------
 *                           INPUT PARAMETERS
 *  NO DCODE INPUT
 * -----------------------------------------------------------------------
 *  This process is re-enterable, but not stand alone.  It must be used
 *  in conjunction with INIT and BHIST.
 * -----------------------------------------------------------------------
 *                                 NOTES
 *
 *  1. See documentation for the CPS process BHIST for information 
 *     concerning the CPS/CONSEIS history. 
 *  2. The card image passed to chistwr must be blank filled.  This will 
 *     conform to the CONSEIS standard history file format.  Also do not 
 *     use the ':' character.
 *  3. The files used by chist are c stream files.
 * -----------------------------------------------------------------------
C\END DOC
C\PROG DOC
 * -----------------------------------------------------------------------
 *                           REVISION HISTORY 
 *     Date      Author    Description 
 *  5. 98/05/21  R.S. Day  chistop  checks file name before opening
 *  4. 98/01/18  R.S. Day  Corrected small logic problem in chistrd_file
 *  3. 98/01/09  R.S. Day  Put wrappers around some J90 specific calls
 *                         that are used for CONI histories by BHIST
 *  2. 98/01/05  R.S. Day  Update of documentation. Does not depend on CPU
 *                         word size. OS specific calls are preprocessor
 *                         protected.
 *  1. 92/07/29  R.S. Day  Rewrote in C-language.
 * -----------------------------------------------------------------------
 *
 *                           CALLING SEQUENCE 
 *        Language is C-language
 *    int  chistwr_(long *ipn, char *buf)              Write a history card. 
 *    int  chistrd_(long *ipn, char *pname, char *buf) Read a history card.
 *   void  chistrew_(long *ipn, long *ncards)          Rewind a history file. 
 *   FILE *chistop(char *file,long  mode);
 *   void  chistcl_(char *file);
 *   int   chistwr_(long *ipn, char *card);
 *   int   chistrd_(long *ipn,char *pname, char *card);
 *   int   chistwr_file_(char *file, char *card);
 *   int   chistrd_file_(char *file, char *card);
 *   int   chistwr_base(FILE *ifile, char *buf);
 *   int   chist_rdall_(int *mipn, long *nhc, int *mcrd, char *buf);
 *   void  chist_snam_(char *name);
 *   char *chist_gnam();
 *   Htable *chist_getby_ipn(long ipn);
 *   Htable *chist_getby_name(char *name);
 *   void  chist_ipn2file(long *ipn,char *file);
 *   void  chist_ipn2procn(long *ipn,char *procn);
 *   void  chist_name_(long *ipn, char *procn, char *file);
 *   void  chist_get_reel_(char *proj, char *reel);
 *   void  chist_put_reel_(char *proj, char *reel);
 *     Following 2 functions build a sorting index
 *   void  chist_date_sort_(int *num, int *dates, int *indices);
 *   int   chist_dcmp(const void *dk,const  void *d);
 *      WRAPPERS AROUND SOME CRAY PVP FUNCTIONS
 *   void  chist_coni_(char *bufin, char *bufout, int *NHISTE,
 *                    int *NHISTO, int *NHISTD, int *NHISLAV,
 *                    int *NHISTT, char *itape);
 *   void chist_gettp_(char *unit, int *n, int *infotap,int *ierr);
 *   int  chist_dsasc_(char *data, int *off, char *out, int *n);
 *   int  chist_u6064_(char *data, int *off, char *out, int *n);
 *   int  chist_int6064_(char *data,  char *out, int *n);
 *   int  chist_fp6064_(char *data,  char *out, int *n);
 *   int  chist_usictc_(char *data, int *sby, char *out,
 *          int *n,int *len);
 *   int  chist_usicti_(char *data, char *out, int *sby,
 *          int *n,int *len);
 *   
 *
 *  ipn     - Process number, integer. output-chistrd, input-chistwr
 *            ipn < 1 corresponds to the file %DCODE0. ipn > 0 are
 *            the files for the processes in a job stream.
 * pname    - returned process name for given ipn
 * file     - file name to be opened or closed
 *  buf     - 80 character card to input or output, may be any type, 
 *            eg. integer, real,..., that can hold character data. 
 *  ncards  - # of 80 character cards in a disk file. If = 0, then 
 *            file does not exist or never written to.  This parameter 
 *            is returned to the calling routine.  Integer. 
 *  name    - Name of the history file. A file name is generated by the
 *            chistwr. chistwr places the new file name in the fortran
 *            common block /INITHIS/ which is in the INIT process.
 *
 * chistwr returns an error status integer
 *  1 - OK, card was written
 *  0 - Failed to find address of INITHIS common block
 * -2 - No processes registerd by the CPS process INIT
 * -3 - ipn was out of the range
 * -4 - failure to open a new current-history file.
 * -5 - write failure
 * chistrd returns an error status integer
 *  1 - OK, 1 card was read
 *  0 - no file for the ipn, or file not open
 * -1 - EOF or error on read
 *
 * -----------------------------------------------------------------------
 * 
 *                                 NOTES 
 *  1. These routines assume that the names of all processes used in the 
 *     job were input to the CPS process INIT.  The names of the pro- 
 *     cesses must be placed in the HISTORY common block INITHIS.  If
 *     the number of processses is equal to zero then no current history
 *     can be generated and chistwr, chistrd, & chistrew will be no-ops.
 *  2. Card images are written to a disk file with a name like HIST#
 *     where # is a digit. The card images are 80 bytes. The
 *  3. The card images must be in ASCII coded form.
 *  4. Will be sensitive to changes in the INITHIS common
 *
 * -----------------------------------------------------------------------
 *      Dependencies
 * Header files: ebcdic.h, spws_sunos.h(only on suns)
 * functions:    Depends on some functions in the init process
 * 
 * -----------------------------------------------------------------------
 * -----------------------------------------------------------------------
C\END DOC
 ************************************************************************/
int inithis_nproc(InitHistCom *h) 
{ if(!h) return 0;
  return h->nprocn; 
}

/* Writes to a registered history file */
int chistwr_(long *ipn, char *buf)
 { char file[16];
   int  i, il, cardno;
   long cardcnt;
   InitHistCom *inithis=0;
   Htable *h=0;
   FILE   *ifile=0;
/**********************************************
 * Get address of the INITHIS common block. ***
 * Check for presence of history processes  ***
 * Is ipn from a legitimate proceedure.     ***
 *********************************************/
   get_inithis_addr_(&inithis);
   
   if(inithis == 0)        return 0;
   i = inithis_nproc(inithis);
   if (i <= 0)  return -2;
   if(*ipn >i)  return -3;
/********************************************
 * Is there a match with an existing file?***
 * If YES, then load the old common block.***
 * If NO , then use data already in common***
 * Position to end of file before write.  ***
 *******************************************/
   h = chist_getby_ipn(*ipn);
   if(!h) {
     chist_ipn2file(ipn,file);
     ifile = chistop(file,0);
     if(!ifile) {
      printf("chistwr: open error, file=%s\n",file);
      return -4;
     }
     h = Htable_new();
     Htable_sfile(h, ifile);
     Htable_sname(h, file);
     Htable_schist(h,inithis);
     Htable_sipn(h,*ipn);
     Htable_smode(h,0);
     Htable_sncard(h,0);
     Hchain_addlnk(&hchain, h);
   }

   ifile = Htable_gfile(h);
   if(ifile == NULL ) {
     printf("chistwr: no open file\n");
     return -6;
   }
   cardcnt = Htable_gncard(h);
   fseek(ifile,0, SEEK_END);

 if(chistwr_base(ifile, buf) != 1) return 0;
 cardcnt = Htable_gncard(h);
 Htable_sncard(h,cardcnt+1);
 if(*ipn>=0 && *ipn <= MAXIPN)
  inithis->nprocc[*ipn] += 1;
 /*Htable_schist(h,inithis); */

 return 1;
}

/* Writes to a non history file */
int chistwr_file_(char *file, char *buf) {
  FILE *ifile=0;
  Htable *h=0;
  long cardcnt;
  int ipn;

  h = chist_getby_name(file);
  if(!h) {
   ifile = chistop(file,0);
   if(strncmp(file,"STDOUT",6)==0) {
    ipn =206;
   } else {
    ipn =201;
   }
   if(!ifile) {
    printf("chistwr_file: open error, file=%s\n",file);
    return -4;
   }
    h = Htable_new();
    Htable_sfile(h, ifile);
    Htable_sname(h, file);
    Htable_sipn(h,ipn);
    Htable_smode(h,0);
    Htable_sncard(h,0);
    Hchain_addlnk(&hchain, h);
  }
  ifile = Htable_gfile(h);
  if(ifile == 0 ) {
    printf("chistwr_file: no open file\n");
    return -6;
  }
  fseek(ifile,0, SEEK_END);
  if(chistwr_base(ifile, buf)!=1) return 0;
  cardcnt = Htable_gncard(h);
  Htable_sncard(h,cardcnt+1);
  return 1;
}

int chistwr_base(FILE *ifile, char *buf) {
  int lnul,lsiz=80,i,wrerr;
  char line[98];
   if(!ifile) return 0;
   lnul = strlen(buf);
   if(ifile==stdout) lsiz=96;
   if(lnul > lsiz) lnul = lsiz;
   strncpy(line,buf,lnul);
   line[lnul]='\0';

   for(i=lnul;i<lsiz;i++) line[i]= 32;
   line[lsiz]='\n';
   line[lsiz+1]='\0';

   if(ifile) {
    wrerr = fputs(line,ifile);
    if(wrerr == EOF) {
     printf("chistwr_base: write error\n");
     return 0;
    }
   }
  return 1;
}
/* 
 * read a history card for process number ipn.
 * return the name of the process in pname.
 *  - pname will be null terminated
 * The card image will be returned in card
 *  - card should hold 80 characters + a null
 * returns 1 if read is successful
 */
int chistrd_(long *ipn, char *pname, char *card) {
  char line[98],file[16],*lineptr;
  int  i, lc;
  InitHistCom *inithis=0;
  FILE *ifile;
  Htable *h;

   if(card==NULL) return 0;
   card[0] = '\0';
/******************************************
 * Determine which file to read from.   ***
 * File must be open already.           ***
 * with the exception of %DCODE0        ***
 * It is possible that people may bypass***
 * NCODE to create %DCODE0              ***
 *****************************************/
   h = chist_getby_ipn(*ipn);
   if(!h) {
     if(*ipn==-1) {
      if(access("%DCODE0",F_OK) !=0) return 0;
     }
     printf("chistrd: NAUGHTY NAUGHTY, you bypassed ncode to create %DCODE0\n");
     chist_ipn2file(ipn,file);
     ifile = chistop(file,1); /* open as old file */
     if(!ifile) {
      printf("chistwr: open error, file=%s\n","%DCODE0");
      return -4;
     }
     get_inithis_addr_(&inithis);
     h = Htable_new();
     Htable_sfile(h, ifile);
     Htable_sname(h, file);
     Htable_schist(h,inithis);
     Htable_sipn(h,*ipn);
     Htable_smode(h,0);
     Htable_sncard(h,0);
     Hchain_addlnk(&hchain, h);
     fseek(ifile,0,SEEK_SET);
   }

   Htable_gipn_name(h,pname);
   ifile = Htable_gfile(h);
   if(ifile == NULL ) return 0;
/*********************************************
 * Read in a card image.
 ********************************************/
   lineptr = fgets(line,90,ifile);
   if(lineptr == NULL) return -1;
/*
   strncpy(cipn,line+4,2); cipn[2]='\0';
   strncpy(ccrd,line+6,2); ccrd[2]='\0';
   *ipn = atol(cipn);
   cardno = atol(ccrd);
*/
/**********************************************
 * Transfer card image to the output buffer ***
 * find line feed and parse embedded nulls  ***
 *********************************************/
   lc=80;
   for(i=0;i<80;i++) {
    if(line[i]=='\n') {
     card[i]=' '; lc=i+1;
    }
    else if( (int)line[i]<32 || (int)line[i]>127) 
     card[i]=32;
    else
     card[i]=line[i];
   }
/*
   lc = strlen(line);
   if(lc>80) lc = 80;
   strncpy(card,line,lc);
   for(i=lc;i<80;i++) card[i]= 32;
   */
   card[80]='\0';
 return 1;
}

int chistrd_file_(char *file, char *card) {
 char line[98],*lineptr;
 Htable *h=0;
 FILE *ifile=0;
 int i,lc,ipn,nc=0;
 long l;
  if(card==NULL) return 0;
  card[0] = '\0';
/******************************************
 * Determine which file to read from.   ***
 * Read in a line from stream           ***
 *****************************************/
  if(strncmp(file,"STDIN",5)==0) {
   ifile=stdin;
  } else {
   h = chist_getby_name(file);
   if(!h) { 
    ifile = chistop(file,1);
    if(!ifile) {
     printf("chistrd_file: open error, file=%s\n",file);
     return 0;
    }
    ipn =205;
    h = Htable_new();
    Htable_sfile(h, ifile);
    Htable_sname(h, file);
    Htable_sipn(h,ipn);
    Htable_smode(h,1);
    fseek(ifile,0,SEEK_END);
    l = ftell(ifile);
    if(l!=-1) nc= (l+1)/80;
    rewind(ifile);
    Htable_sncard(h,nc);
    Hchain_addlnk(&hchain, h);
   }
   if(h) ifile = Htable_gfile(h);
  }

  lineptr = fgets(line,96,ifile);
  if(lineptr == NULL) return -1;
  lc = strlen(line);
  if(lc>80) lc = 80;
  strncpy(card,line,lc);
  for(i=lc;i<80;i++) card[i]= 32;
  card[80]='\0';
 return 1;
}

int  chist_rdall_(int *mipn, long *nhc, int *mcrd, char *buf)
{long crdcnt=0, ipn, oldipn;
 char locbuf[96],pname[16];
 int MAXNHC=*mcrd, maxipn=*mipn;
 char h1[81];
 Htable *h=0;
 int  lc,i;
 *nhc = 0;
 if(buf == NULL) return 0;
 h = Hchain_gfirst(&hchain);
 ipn=0;
 while(h != NULL && ipn<=maxipn) {
    ipn = Htable_gipn(h);
    if(ipn>=0) {
     chistrew_(&ipn, &crdcnt);
     *nhc += crdcnt;
    }
    h = Htable_next(h);
 }
 if(*nhc <= 0) return 0;
 if(*nhc >  MAXNHC) {
    printf("chist_rdall: more than %d history cards\n",MAXNHC);
 }
 i=*nhc;
 if(i> MAXNHC) i =MAXNHC;
 buf[0]='\0';
 crdcnt = 0;
 h = Hchain_gfirst(&hchain);
 Htable_gipn_name(h, pname);
 oldipn=-1;
 ipn=-2;
 while(h != NULL && crdcnt < MAXNHC-1 && ipn<=maxipn) {
    ipn = Htable_gipn(h);
    if(ipn != oldipn && ipn!=-2) {
      Htable_gipn_name(h, pname);
      sprintf(h1,"%-8s  **** CRAY PROCESSING SYSTEM ****",pname);
      lc = strlen(h1);
      for(i=lc;i<80;i++) { h1[i]= 32;} h1[79]='\n'; h1[80]='\0';
      strcat(buf,h1);
      oldipn = ipn;
      crdcnt++;
    }
    
    if(ipn != -2) {
     while((chistrd_(&ipn, pname, locbuf)==1) && crdcnt <MAXNHC) {
      strcat(buf,locbuf);
      crdcnt++;
      buf[crdcnt*80-1]='\n';
      buf[crdcnt*80]='\0';
     }
    }
    h = Htable_next(h);
 }
 if(crdcnt < *nhc) {
  sprintf(h1," **** MORE THAN %4d HISTORY CARDS, SOME LOST ****",MAXNHC);
  lc = strlen(h1);
  for(i=lc;i<80;i++) { h1[i]= 32;} h1[79]='\n'; h1[80]='\0';
  strcat(buf,h1);
  crdcnt++;
 } 
 return (int) crdcnt;
}

void chistrew_(long *ipn, long *cards)
{/* cards = total number of cards in the file */
 FILE *ifile;
 Htable *h;

 *cards=0;
 if(*ipn >199) return;

 ifile = NULL;
 h = chist_getby_ipn(*ipn);
 if(h!= NULL) ifile = Htable_gfile(h);
 if(ifile == NULL) return;
 *cards = Htable_gncard(h);
 if (*cards <= 0) { return; }

 if( ifile==stdin || ifile==stdout) return;
 if(ftell(ifile) != 0) fseek(ifile, 0, SEEK_SET);
 return;
}

/* opens if not already open */
FILE *chistop(char *name,long  mode)
{ FILE *ifile=0;
  Htable *h=0;

  if(!name) return 0;
  if(strlen(name)==0) return 0;
  if(name[0]==' ') return ifile;
  if(strcmp(name,"NONE") ==0) return 0;
  if(strcmp(name,"STDIN")==0) return stdin;
  if(strcmp(name,"STDOUT")==0) return stdout;

  h = chist_getby_name(name);
  if(h) {
   if(Htable_gmode(h)==mode) return Htable_gfile(h);
  }

  if(mode == 0) {
     if(strstr(name,"%BYTDG")) ifile=fopen(name,"a+");
     else ifile=fopen(name,"w+");
  }
  if(mode == 1) ifile=fopen(name,"r+");
  return ifile;
}

void chistcl_(char *name)
{Htable *h=0;
 FILE   *ifile=0;
 h = chist_getby_name(name);
 ifile = Htable_gfile(h);
 if(ifile) {
   if(ifile!=stdin && ifile != stdout) {
     fclose(ifile);
     Hchain_rmlink(&hchain, h);
     Htable_destroy(h);
   }
 }
}

/* any ipn < 0 is mapped to the file %DCODE0 */
Htable *chist_getby_ipn(long ipn)
{ Htable *h;
  long ipn_h;
  h = Hchain_gfirst(&hchain);
  while(h != NULL) {
    ipn_h = Htable_gipn(h);
    if(ipn == ipn_h) return h;
    if(ipn<0 && ipn_h < 0) return h;
    h = Htable_next(h);
  }
 return 0;
}

Htable *chist_getby_name(char *name)
{ Htable *h;
  char *fname=0;
  if(name==0) return 0;
  h = Hchain_gfirst(&hchain);
  while(h != NULL) {
    fname =Htable_gname(h);
    if(fname) {
     if(strcmp(fname,name)==0) return h;
    }
    h = Htable_next(h);
  }
 return 0;
}

void chist_snam_(char *chist_name)
{ if(chist_name == NULL) return;
  if(strlen(chist_name) > 80) return;
  strcpy(current_history_file,chist_name);
}
char *chist_gnam()
{ return current_history_file; }

Htable *Htable_new()
{ Htable *h;
  h = NULL;
  h = (Htable *) calloc(1, sizeof(Htable));
  return (Htable *) h;
}

void Htable_destroy(Htable *h)
{InitHistCom *inithis;
 if(h == NULL) return;
 inithis = Htable_gchist(h);
 free(h);
}

void Htable_gipn_name(Htable *h, char *procn)
{InitHistCom *inithis=0;
 long ipn=-1;
 int i;
 procn[0]='\0';
 if(h == NULL) return;
 ipn = Htable_gipn(h);
 if(ipn==-1) { strcpy(procn,"%DCODE0"); return;}
 inithis = Htable_gchist(h);
 if(!inithis) return;
 strncpy(procn,&inithis->procn[(ipn) * 8],8);
 for(i=0;i<8;i++) { if(procn[i]==' ') procn[i]='\0';}
 procn[7]='\0';
}


FILE *Htable_gfile(Htable *h)
{if(h == NULL) return NULL;
 return h->ifile;
}
void Htable_sfile(Htable *h, FILE *ifile)
{if(h == NULL) return;
 h->ifile = ifile;
}

Htable *Htable_next(Htable *h)
{/* return the next link in the linked list */
  if(h == NULL) return (Htable *) NULL;
  return (Htable *) h->next;
}
Htable *Htable_prev(Htable *h)
{/* return the previous link in the linked list */
  if(h == NULL) return (Htable *) NULL;
  return (Htable *) h->prev;
}

InitHistCom *Htable_gchist(Htable *h)
{ if(h==NULL) return NULL;
  return h->chist;
}
void Htable_schist(Htable *h, InitHistCom *chist)
{if(h==NULL || chist == NULL) return;
  h->chist = chist;
/*
 memcpy(chist,chist,sizeof(InitHistCom));
 */
}

char *Htable_gname(Htable *h)
{ if(h==NULL) return NULL;
  return h->name;
}
void Htable_sname(Htable *h, char *name)
{ long nbytes;
  if(h==NULL) return;
  h->name[0] = '\0';
  nbytes = strlen(name) + 1;
  if(nbytes <= 1) return;
  if(nbytes >  8) {strncpy(h->name,name,7); h->name[8]='\0';}
  else strcpy(h->name,name);
  return;
}

void Htable_sipn(Htable *h, long ipn)
{ if(h == NULL) return;
  h->ipn = ipn;
}
long Htable_gipn(Htable *h)
{ if(h == NULL) return 0;
  return h->ipn;
}

void Htable_sncard(Htable *h, long ndat)
{ if(h == NULL) return;
  h->ncards = ndat;
}
long Htable_gncard(Htable *h)
{ if(h == NULL) return 0;
   return h->ncards;
}

void Htable_smode(Htable *h, long mode)
{ if(h == NULL) return;
  h->mode = mode;
}
long Htable_gmode(Htable *h)
{ if(h == NULL) return -1;
   return h->mode;
}
/******************************************
 * The following methods are for Hchain ***
 *****************************************/
Htable *Hchain_gfirst(Hchain *hc)
{if(hc == NULL) return NULL;
 return hc->first;
}
void Hchain_sfirst(Hchain *hc, Htable *first)
{if(hc == NULL) return;
 hc->first=first;
}
void Hchain_slast(Hchain *hc, Htable *h)
{if(hc == NULL) return;
 hc->last=h;
}

long Hchain_gnlink(Hchain *hc)
{if(hc == NULL) return 0;
 return hc->nlink;
}
void Hchain_snlink(Hchain *hc, long nlink)
{if(hc == NULL) return;
 hc->nlink= nlink;
}

void Hchain_addlnk(Hchain *hc, Htable *h)
{

 /* Add process on to end of list */
 if(hc == NULL) return;
 if(hc->nlink == 0)
  { hc->first = h;
    hc->last  = h;
    h->prev = NULL;
    h->next = NULL;
    hc->nlink = 1;
    return;
  }
 if(hc->last != NULL)
  { hc->last->next = h;
    if(h != NULL)
     { h->prev = hc->last;
       h->next = NULL;
       hc->last  = h;
     }
    hc->nlink += 1;
  }
 return;
}

void Hchain_rmlink(Hchain *hc, Htable *h)
{Htable *hlnk,*prev,*next;
 if( hc == NULL | h == NULL) return;
 hlnk = Hchain_gfirst(hc);
 while(h != hlnk) hlnk = Htable_next(hlnk);
 if(hlnk == h) {
    prev = Htable_prev(hlnk); 
    next = Htable_next(hlnk); 
    if(prev)
     prev->next = next;
    else
     hc->first = next;
    if(next)
     next->prev = prev;
    else
     hc->last=prev;
    hc->nlink--;
    if(hc->nlink<0) hc->nlink = 0;
 }
}

void chist_ipn2file(long *ipn,char *file) {
 char procn[16];
 chist_ipn2procn(ipn,procn);
 chist_name_(ipn, procn, file);
}

/* ipn > 0 returns name of a registered process
 * ipn = 0 is process GETV ?
 * ipn < 0 returns DCODE */
void chist_ipn2procn(long *ipn,char *procn) {
   InitHistCom *inithis=0;
   char *p;
   int i=0;
/**********************************************
 * Get address of the INITHIS common block. ***
 * Check for presence of history processes  ***
 * Is ipn from in a legitimate range.       ***
 *********************************************/
   procn[0]='\0';
   get_inithis_addr_(&inithis);
   if(inithis == 0) return;
   if(*ipn < 0) { strcpy(procn,"DCODE"); return; }
   i = inithis_nproc(inithis);
   if (i <= 0)  return;
   if(*ipn  >i ) return;
   p = &inithis->procn[(*ipn) * 8];
   strncpy(procn,p,8);
   for(i=0;i<8;i++) if(procn[i]==' ') procn[i]='\0';
   procn[7]='\0';
}

/* given a name: create a file-name consistent
 * with history file conventions */
void chist_name_(long *ipn, char *procn, char *file)
{ int i,dec=3;
  char fmt[8];
  strcpy(file,"%%%%%%%");
  file[7]='\0';
  if(!procn) return;
  if(strncmp(procn,"  ",2)==0) return;
  if(*ipn<100) dec=2;
  if(*ipn<10 ) dec=1;
  sprintf(fmt,"%%-4s%%%dd",dec);

  i=*ipn;
  if(i<0) i=0;
  sprintf(file+1,fmt,procn,i);
  for(i=0;i<7;i++) if(file[i]==' ') file[i]='%';
}

void chist_mv_(int *num, char *from, char *to) {
 int n = *num;
 if(n<=0) return;
 memcpy(to,from,n);
}

void chist_get_reel_(char *proj, char *reel) {
 register int i;
  for(i=0;i<8;i++) {
   reel[i] = proj[i+216]; /*ebcdic_to_ascii[proj[i+216]]; */
  }
}

void chist_put_reel_(char *proj, char *reel) {
 register int i;
  for(i=0;i<8;i++) {
   proj[216+i] = reel[i]; /*ascii_to_ebcdic[reel[i]];*/
  }
}


void chist_date_sort_(int *num, int *dates, int *indices) {
 int i, n=*num;
 KeyDate *KD;
 int (*dc)(const void *a,const  void *b) = &chist_dcmp;
 
 KD = (KeyDate *) malloc(n*sizeof(KeyDate));
 for(i=0;i<n;i++) {
  indices[i]=i+1;
  KD[i].key=i+1;
  KD[i].date=dates[i];
 }
 qsort(KD,n,sizeof(KeyDate),dc);
 for(i=0;i<n;i++) {
  indices[i]=KD[i].key;
 }
 if(KD) free(KD);
}

int chist_dcmp(const void *dk,const  void *d) {
 KeyDate *d1 = (KeyDate *) dk;
 KeyDate *d2 = (KeyDate *) d;
 return (d1->date - d2->date);
}

void chist_coni_(char *BUFIN, char *BUFOUT, int *NHISTE,
                 int *NHISTO, int *NHISTD, int *HISLAV,
                 int *NHISTT, char *itape){

#ifdef _CRAY1
 int one=1,three=3, eight=8,n;
 INT6064(BUFIN+24, NHISTO,&one);
 INT6064(BUFIN+424,NHISTE,&one);
 DSASC(BUFIN+432,&three,itape,&eight);
 INT6064(BUFIN+480,NHISTD,&one);
 FP6064(BUFIN+488,HISLAV,&one);
 INT6064(BUFIN+496,NHISTT,&one);
 n = 80* *NHISTE;
 DSASC(BUFIN+640,&one,BUFOUT,&n);
#else
 printf("CHIST_CONI: No support for CONI other than on J90\n");
 *NHISTE=0;
 *NHISTO=0;
 *NHISTD=0;
 *NHISTT=0;
 strcpy(itape,"NO-VOL");
#endif
 
}

void chist_gettp_(char *unit, int *n, int *infotap,int *ierr)
{int synch=1;
#ifdef _CRAY1
 GETTP(unit,n,infotap,&synch,ierr);
#else
 printf("CHIST_GETTP: No support for GETTP other than on J90\n");
 *ierr=1;
#endif
}

int chist_dsasc_(char *data, int *off, char *out, int *n)
{int ierr=0;
#ifdef _CRAY1
 DSASC(data,off,out,n);
#else
 printf("CHIST_DASC: No support for DASC other than on J90\n");
 ierr=1;
#endif
 return ierr;
}

int chist_u6064_(char *data, int *off, char *out, int *n)
{int ierr=0;
#ifdef _CRAY1
 U6064(data,off,out,n);
#else
 printf("CHIST_U6064: No support for U6064 other than on J90\n");
 ierr=1;
#endif
 return ierr;
}

int chist_int6064_(char *data,  char *out, int *n)
{int ierr=0;
#ifdef _CRAY1
 INT6064(data,out,n);
#else
 printf("CHIST_INT6064: No support for INT6064 other than on J90\n");
 ierr=1;
#endif
 return ierr;
}

int chist_fp6064_(char *data,  char *out, int *n)
{int ierr=0;
#ifdef _CRAY1
 FP6064(data,out,n);
#else
 printf("CHIST_FP6064: No support for FP6064 other than on J90\n");
 ierr=1;
#endif
 return ierr;
}

int chist_usictc_(char *data, int *sby, char *out, int *n,int *len)
{int ierr=0;
#ifdef _CRAY1
 USICTC(data,sby,out,n,len);
#else
 printf("CHIST_USICTC: No support for USICTC other than on J90\n");
 ierr=1;
#endif
 return ierr;
}

int chist_usicti_(char *data, char *out, int *sby, int *n,int *len)
{int ierr=0;
#ifdef _CRAY1
 USICTI(data,out,sby,n,len,&ierr);
#else
 printf("CHIST_USICTI: No support for USICTI other than on J90\n");
 ierr=1;
#endif
 return ierr;
}
/********************************************
 * Construct history card with card header***
   cardno = inithis->nprocc[*ipn - 1] + 1;
   strncpy(ctmp,&inithis->procn[(*ipn - 2) * 8],8);
   ctmp[7]='\0';
   if (ctmp[0] != '\0' && ctmp[0] != ' ')
     { lc = 0;
       while(ctmp[lc]!= 32 && ctmp[lc]!= 0) ++lc;
       il = lc;
       if(il > 4) il = 4;
       strcpy(histhdr,"        ");
       strncpy(histhdr,ctmp,il);
       sprintf(&histhdr[4],"%2.2u%2.2u",*ipn,cardno);
     } 
   else
     { ++inithis->nhrbad;
       strcpy(histhdr,"        ");
       sprintf(&histhdr[4],"%2.2u",*ipn);
       strcpy(cname,"HISTBAD");
     }
 *******************************************/



