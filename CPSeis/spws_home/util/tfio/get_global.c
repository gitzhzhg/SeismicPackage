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

#if( CRAY )
#include <sys/types.h>
#include <sys/unistd.h>
#include <fcntl.h>
#elif( unix || POSIX )
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#elif VMS
#include <stdlib.h>
#include <file.h>
#else
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#endif
#include <ctype.h>
#include <stdio.h>


#include "ebcdic.h"
#include "wrdcnvrt.h"
#include "tfdefs.h"
#include "dskio.h"
#include "tf_global.h"
#include "cprim.h"
#include "jsfiles.h"

#define NNAMES 7
#define SECSIZ  512
#define NUMBY  3600


#ifdef NEED_CAPITALS
#define get_global_parms_ GET_GLOBAL_PARMS
#endif
#if(VMS || _AIX || __hpux)
#define get_global_parms_ get_global_parms
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void get_global_cleanup(char *buff);
int  get_global_cbyt_parsehd(int nbuf, char *buff,int  *ndpt,int  *nbit,
    float *dt, float *t0,int  *ntrc,int  *nbih, float *trmaxg,
    char *msg);
int  get_global_parms_(char *name, int *ntrfil, int *ndpt, float *srval);
#ifdef __cplusplus
}                   // for C++
#endif


/*
C***************************** COPYRIGHT NOTICE *************************
C*                                                                      *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION             *
C*                              OF CONOCO INC.                          *
C*                      PROTECTED BY THE COPYRIGHT LAW                  *
C*                          AS AN UNPUBLISHED WORK                      *
C*                                                                      *
C***************************** COPYRIGHT NOTICE *************************
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
C  Primitive name:  get_global_data (get file parameters)
C         Library:  CONLIB
C          Author:  Richard Day          
C    Date Written:  91/05/31
C    Last revised:  00/01/04 Day
C
C Purpose: Attempts to retrieve global data from supported seismic
C          files. Supported file types are:
C          1. CBYTE   -  CPS byte file
C          2. TFILE   -  SPWS Trace File (TFILE3, TFILE3F)
C          3. STROT   -  CPS strot file(Cray only)
C          4. DTROT   -  CPS dtrot file(Cray only)
C          5. DSEGY   -  CPS SEGY file. (sequential bytes)
C          6. QTROT   -  CPS portable STROT firl
C          7. VOXET,BRICK,HGRID,HG3DL
C
C Name   : get_global_data
C Purpose: Read & retrieve a description from a recognized file.
C_______________________________________________________________________
C                                NOTES
C
C Function Definition:        ( Language = C )
C  int  get_global_data_(char *name, TF_Global *g, int  *istat)
C  *name     input      The name of the sesmic file.
C  *g        in&out     Global structure. see tfio.h for details.
C  *istat    output     0 if all is OK and file is not open.
C                      20.... Recognized but unsupported format
C                      10.... Negative or zero sample rate?
C                       9.... file doesnt exist.
C                       8.... failed to process as a byte file
C                       7.... couldnt open the file.
C                       6.... couldnt read a file header
C                       5.... couldnt decode tfile header
C                       4.... Segy format not suppported
C                       3.... Unrecognized format
C                       <0 .. Implies file is open and istat is
C                             negitive of the channel number.
C 
C  1. Sets file type = "UNKNOWN" for unrecognized file types.
C  2. Use tf_global_ftype(g) to retrieve file type.
C
C NOTES:
C  1. If file has been opened by tf_open_():
C      Recalls global information and leaves file open. Will
C      not leave file open if it is not already open.
C  2. Sets g->ftyp="UNKNOWN" if it does not recognize the type.
C  3. SEGY file supported is continuous stream of bytes. Will
C     not work with a record based SEGY file!
C  4. For remote files, get_global_data, will only pull over
C     enough data to determine the file global parameters.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 26.00/01/04  Day          Fixed byte swapping qtrot problem
C 25.99/07/06  Day          Refined logic for STROT2,STROT1 types
C 24.99/06/28  Day          Added logic for STROT2 file type
C 23.99/04/29  Day          Now returns lun as -1 rather than 0
C                           replaced exist_ with dskio_exist_.
C 22.99/01/29  Day          Replaced tf3d_iword type by wrdc_iword_type.
C                           Will deal with WBYTE & WSBYT types now.
C 21.98/11/30  Day          Files can now reside on other nodes.
C                           Eliminated call to get_global_old_byt
C                           Either .byt or .glbl file can be used.
C                           Function get_global_cbyt_parsehd added.
C 20.98/10/29  Day          Fixed a file descriptor leak for SEGY files
C 19.98/10/19  Day          Landmark 1 byte SEGY recognition
C 18.98/09/17  Day          Fixed problem for STROT files
C 17.98/09/02  Day          get_global_data sets default rotation matrix 
C                           values for SEGY data types.
C 16.98/07/23  Day          Fixed STROT, QTROT parsing.
C 15.98/02/26  Day          Set grid sizes for LAYER & HGRID files
C 14.97/06/09  Day          Support for LAYER files added.
C 13.96/08/09  Day          VOXET,BRICK,HGRID,HG3DL support
C 12.96/07/22  Vunderink    Fixed problem in get_global_data, Null
C                           terminator should be in byte nr-1 not nr.
C 11 96/07/17  Vunderink    Inserted into the conlib library.
C 10 95/07/18  Day          Recognizes 16 bit segy data type now.
C 9. 95/03/08  Day          fixed problem with tf_set_gdefs_ call
C 8. 06/21/94  Day          Corrected problem with Segy recognition
C 7. 04/15/94  Day          Checks for ASCII or EBCDIC segy header
C 6. 03/02/94  Day          Changed some names
C 5. 07/14/93  R.S.DAY      Logic to handle files less than 3600 bytes.
C 4. 10/2/91   DAY          Added STROT and DTROT support.
C 3.  91/10/16 Day          Return negative channel no. for open files
C 2.  91/10/02 Day          Added STROT and DTROT support.
C 1.  91/05/31 Day          Initial Version
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     None       
C  Functions:       get_global_data        get_global_old_byt
C  Entry points:    None
C  Common blocks:   None
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C                              none
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -
C  Heap (dynamic)  - malloc's memory for  an IO buffer and structures
C-----------------------------------------------------------------------
C\END DOC
*/
/* only return global for an open file
int  get_global_mem_(int *ufi, TF_Global *g)
{int     OK=0,NOTOK= -1,i_err;
 char    fname[160];
 TF_Global *tfg=0;
 if(*ufi <0) return NOTOK;
 dskio_chain_fname_(ufi,fname);
 tfg = (TF_Global *) dskio_chain_file_data_(ufi);
 tf_glbl_get_(&i_err, ufi,  g );
 if(i_err !=0) return NOTOK;
 return OK;
}
*/

int  get_global_parms_(char *name, int *ntrfil, int *ndpt, float *srval)
{ TF_Global *g=0;
 g = get_global_info(name);
 if(!g) return -1;
 *srval = g->srval;
 *ndpt  = g->ndptr;
 *ntrfil= g->ntrfil;
 if(g) free(g);
 return 0;
}

int get_global_keyword_(char *filename, char *search_for_keyword, 
                        char *returned_keyword_value             )
{
int stat;
   int lun = jsfiles_getlun (filename, "r");
   jsfiles_open (lun);
   if (jsfiles_isa(lun) != 0) {
     printf ("Getting global keyword\n");
     printf ("   %s\n", search_for_keyword);
     printf ("   from a JavaSeis File is unsupported\n");
     jsfiles_close (lun);
     return -1;
   }

   trciof77wrapper_get_keyword_(filename, 
                                search_for_keyword,
                                returned_keyword_value,
                                &stat);

return stat;
}


int  get_global_data_(char *name, TF_Global *g, int  *istat)
{
  int   i,nr,ierr;
  int   nbret;
  int   wrdsiz;
  char *buff=0, msg[160];
  int   nbit;
  int   n;
  long  nbytes;
  int   ndpt,idt,code,type,mf=0;
  char  header_name[96], ext[6];
  float fval;
  float dt, tmin;
  int ndpts, nwih, nbits, nbits_hd, lun;
  int stat, data_start_position, num_traces;
  char wtype[10];
  double trmaxg;
  double xorigin, yorigin, dx11, dx12, dx21, dx22;
  TF_Global *temp;

  /* Try CPS types first */
  temp = workstation_globals_get_globals(name, istat);
  if(temp)
    {
      memcpy(g, temp, sizeof(TF_Global));
      return *istat;
    }
      


  *istat = 0;
  g->lun  = -1;
  g->grecsiz=0;
  g->ntrfil=0;
  g->nbycll=1024;
  g->ntrcll=1;
  g->wdtyp=WIEEE;
  g->nbydp=1;
  g->nbyhd=1;
  g->hdtyp=0;
  g->nhdwd=0;
  g->ndptr=1024;
  g->ntb  =0;
  g->numhc=0;
  g->h    =0;
  wrdsiz = sizeof(float);

/*******************************************************
 *** Check to see if it is already opened.           ***
 *** channel will be active if it is already active  ***
 ******************************************************/
 tf_glbl_getn_(&ierr, name,  g );
 if( ierr == 0 )   /* it is already open,so return*/
   { *istat= -g->lun; return *istat; }
 
/*******************************************************
 *** CBYTE_TYPE files must have the extension .byt,.BYT*
 *** .glbl, or .GLBL. Consistent case usage is assumed.*
 ******************************************************/
 strcpy(g->path,name);    /* the trace data file */
 strcpy(header_name,name); 


/*******************************************************
 *** Check the file access and type                  ***
 ******************************************************/
 buff = get_global_buff(10000,&nbret);
 if(!buff) {
   *istat=1; return *istat;
 }
 buff[0]='\0';
 nr = getgl_ftype_hdata(header_name,nbret,buff,&nbytes,&type);
 strcpy(g->ftyp,getgl_ftype_to_str(type));

 /*If and old byte type set up the .glbl file reading */
 if(type == UNKWN_TYPE) {
   if(strstr(name,".byt")!=0) {
     strcpy(ext,"glbl");
     addext_rep_(header_name,  ext,  &ierr);
   }
   if(strstr(name,".BYT")!=0 ) {
     strcpy(ext,"GLBL");
     addext_rep_(header_name,  ext,  &ierr);
   }
   if(strstr(name,".glbl")!=0) {
     strcpy(ext,"byt");
     addext_rep_(g->path,  ext,  &ierr);
   }
   if(strstr(name,".GLBL")!=0) {
     strcpy(ext,"BYT");
     addext_rep_(g->path,  ext,  &ierr);
   }

   nr = getgl_ftype_hdata(header_name,nbret,buff,&nbytes,&type);
   strcpy(g->ftyp,getgl_ftype_to_str(type));
 }


 if(nr<=0) {
   get_global_cleanup(buff);
   *istat=6; return *istat;
 }
 if(type== BADFN_TYPE) {
   get_global_cleanup(buff);
   *istat=9; return *istat;
 }
 if(type== UNKWN_TYPE) {
   get_global_cleanup(buff);
   *istat=3; return *istat;
 }
 if(nr>0) buff[nr-1] = '\0';

/*******************************************************
 *** Try to parse the header on the file             ***
 ******************************************************/

  *istat=0;
  switch(type) {

   case CBYTE_TYPE:
     i = get_global_cbyt_parsehd(nr,buff, &g->ndptr,&nbit,
    &g->srval, &g->tstrt,&g->ntrfil,&g->nhdwd, &g->trmaxg, msg);
     g->lun= -1;
     if(strncmp(msg,"OK",2)!=0) {
       *istat=8; return *istat;
     }
     if(g->trmaxg==0.) g->trmaxg=1.0;
     g->nbycll=nbit + g->nhdwd;
     *istat=0;
     return *istat;

   case TFILE_TYPE:
   case TF3D_TYPE:
   case TF3DF_TYPE:
 
     g->h = (void *) get_global_tf_parsehd(g,name, buff);
     get_global_cleanup(buff);
     g->lun= -1;
     return *istat; 


   case HGRID_TYPE:

   case HG3DL_TYPE:

   case LAYER_TYPE:

     g->h = (void *) get_global_rmod_parsehd(name,type, buff);
     if(!g->h) break;
     get_global_grid_to_tfio(g,type, g->h);
     get_global_cleanup(buff);
     return *istat;


   case VOXET_TYPE:

   case BRICK_TYPE:

     g->h = (void *) get_global_govo_parsehd(name,type, buff);
     if(!g->h) break;
     get_global_grid_to_tfio(g,type, g->h);
     get_global_cleanup(buff);
     return *istat;


   case STROT_TYPE:

     strcpy(g->path,name);
     wrdsiz=8;
     if(strncmp(&buff[8],"YES",3)==0) wrdsiz=4;
     g->hdtyp  = 0;
     g->nbydp  = wrdsiz;
     g->nbyhd  = wrdsiz;
     g->ntrcll = 1;
     g->ntrfil = *(int *) buff;
     g->nhdwd  = *(int *) &buff[24];
     g->ndptr  = *(int *) &buff[32];
     g->nbycll = 8* *(int *) &buff[144];
     g->grecsiz= 8* *(int *) &buff[152];
     if(g->grecsiz<=0) g->grecsiz=4096;
     g->ntb    = 0;
     g->numhc  = 0;
     memcpy(&g->srval,&buff[40],sizeof(struct CPSglbl));
     get_global_cleanup(buff);
     return *istat; 

   case DTROT_TYPE:
  
     wrdsiz=8;
     if(strncmp(&buff[8],"YES",3)==0) wrdsiz=4;
     g->hdtyp  = 0;
     g->nbydp  = wrdsiz;
     g->nbyhd  = wrdsiz;
     g->ntrcll = 1;
     g->ntrfil = *(int *) buff;
     g->nhdwd  = *(int *) &buff[24];
     g->ndptr  = *(int *) &buff[32];
     g->nbycll = 8* *(int *) &buff[144];
     g->grecsiz= g->nbycll;
     if(g->grecsiz<=0) g->grecsiz=4096;
     g->ntb    = 0;
     g->numhc  = 0;
     memcpy(&g->srval,&buff[40],sizeof(struct CPSglbl));
     get_global_cleanup(buff);
     return *istat; 


   default:
     *istat=3;
     break;

  }

 printf("get_global_data: %s type not supported\n",
 getgl_ftype_to_str(type));
 return *istat;
}

/*------------------------------------------------------------------
C\USER DOC
 *Name   : get_global_info
 *Purpose: Read and retrieve TF_Global info from a trace file.
 *Author : R. Day
 *Date   : 95/03/31   Initial version
 *
 *Function Definition:        ( Language = C )
 *TF_Global *get_global_info(char *name);
 *  name      in        Name of the trace file to scan for TF_Global data.
 *
 *NOTES:
 * 1. Return value is NULL if it fails.
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
C\END DOC
 *------------------------------------------------------------------*/
TF_Global *get_global_info(char *name)
{int  l,ierr;
 TF_Global *global=NULL;
/*
 * Lets get some information about our file!
 */
 global = (TF_Global *) calloc(1,sizeof(TF_Global));
 if(!global) return global;
 strcpy(global->ftyp,"UNKNOWN");
 l = get_global_data_(name,global,&ierr);
 if(ierr > 0)
   { printf("get_global_info: ierr=%d from get_global_data_\n",ierr);
     if(global) free(global);
     global = 0;
   }
 return global;
}


/*------------------------------------------------------------------
C\USER DOC
 *Name   : get_global_old_byt
 *Purpose: Read and retrieve the globals from a CPS glbl file.
 *Author : R. Day
 *Date   : 91/04/09
 *
 *Function Definition:        ( Language = C )
 * int  get_global_old_byt_(char filnam[], char msg[],int  *ndpt,int  *nbit,
 *   float *dt, float *t0,int  *ntrc,int  *nbih, float *trmaxg)
 *  filnam    in        Name of the CPS byte file to read. We assume the CPS
 *                      global file has an extension of .glbl at the end.
 *  msg       out       OK, or an error message.
 *  Rest of the arguments are CPS byte globals.
 *
 *NOTES:
 * 1. Return value is 0 or an integer error number.
 * 2. Also calls: addext_rep_ and  exist_ .
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 *95/04/04  R. Day      Null terminated local string buff
C\END DOC
 *------------------------------------------------------------------*/

int  get_global_old_byt_(char *filnam, char *msg,int  *ndpt,int  *nbit,
    float *dt, float *t0,int  *ntrc,int  *nbih, float *trmaxg)
{ int    i,istat,nr, ng=162;
  FILE   *ifile;
  char   file_name_glbl_tmp[96], file_name_glbl[96], ext[6];
  int    file_exist;
  char   buff[164];
  static char *fmt = "%d %d %f %f %d %d %f";
  static char *names[8] = {"ndpt","nbit","dt","tstrt","ntrc",
                           "nbih","trmaxg" };

  strcpy(msg,"OK");
/*
 * Add GLBL or glbl extension to file name if it is missing */
   strcpy(ext,"glbl");
   strcpy(file_name_glbl_tmp,filnam);
   addext_rep_(  file_name_glbl_tmp,  ext,  &istat );
   file_exist = dskio_exist_(file_name_glbl_tmp);
   if( file_exist == 0 ) {
      strcpy(file_name_glbl,file_name_glbl_tmp);
   } else {
      strcpy(ext,"GLBL");
      strcpy(file_name_glbl_tmp,filnam);
      addext_rep_(  file_name_glbl_tmp,  ext,  &istat );
      file_exist = dskio_exist_(file_name_glbl_tmp);
      if( file_exist == 0) strcpy(file_name_glbl,file_name_glbl_tmp);
   }

/*
 * Check if file exists, open it, read 2 card images */
   if( file_exist  == -1 ) {
     strcpy(msg,"01:get_global_old_byt: no old file=");
     strcat(msg,file_name_glbl);
     goto error;
   }
  ifile=fopen(file_name_glbl,"r");
  if(!ifile) {
    strcpy(msg,"02:get_global_old_byt: failed reading global cards");
    goto error;
  }
  buff[162]='\0';
  nr = fread(buff, sizeof(char), ng, ifile);
  fclose(ifile);
  if(nr!=ng) {
    strcpy(msg,"03:get_global_old_byt: failed reading global cards");
    goto error;
  }
/*
 * Dcode the global information
 */
 i = get_global_cbyt_parsehd(nr , buff,ndpt,nbit,
    dt, t0,ntrc,nbih, trmaxg, msg);
 if(i< 0) goto error;
 return 0;
 error:
 return msg[1]-'0';
}

int get_global_cbyt_parsehd(int nbuf, char *buff,int  *ndpt,int  *nbit,
    float *dt, float *t0,int  *ntrc,int  *nbih, float *trmaxg,
    char *msg)
{int    i;
 char   parse[160], *jp, *js, *jf, cr[2];
 static char *fmt = "%d %d %f %f %d %d %f";
 static char *names[NNAMES] = {"ndpt","nbit","dt","tstrt","ntrc",
                           "nbih","trmaxg" };
 strcpy(msg,"OK");
/*
 * Gather the numeric values corresponding to names into parse.
 * Order the values in the same order as the names list. */
 if(nbuf<1) return -1;
 buff[162]='\0';
 parse[0] ='\0';
 cr[0] = '\n'; cr[1]='\0';
 for (i=0;i<NNAMES; i++) {
   jp = strstr(buff,names[i]);
   if(jp==NULL) {
     strcpy(msg,"04: get_global_cbyt_parsehd: decode name not found");
     return -1;
   }
   js= strstr(jp,"=");
   jf= strstr(jp,",");
   if(!jf) jf= strstr(jp,cr);
   if( js==NULL || jf==NULL ) {
     if(i==NNAMES-1) jf= buff+strlen(buff);
    /* if( (jf=strstr(jp, cr)) == NULL) { */
     if( jf == NULL) {
       strcpy(msg,"05: get_global_cbyt_parsehd: bad decode format");
       return -1;
     }
   }
   strncat(parse,js+1,jf-js-1);
   strcat(parse," ");
 }
/*
 * Convert the ASCII numeric values to true numeric values */
 sscanf(parse,fmt,ndpt,nbit,dt,t0,ntrc,nbih,trmaxg); 
 return 1;
}

/*
 C\USER DOC
 C Name:     get_global_buff
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Allocate a static buffer to handle IO
 C
 C Prototype:
 C     char *get_global_buff(int newsiz, int *retsiz)
 C     newsiz .... requested buffer size
 C     retsiz .... size actually allocated
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C
 C\END DOC
 */
char *get_global_buff(int newsiz, int *retsiz)
{ static char *b;
  static int buff_siz;
  if(newsiz > 100000) newsiz=100000;
  if(newsiz==0)
   { if(b) free(b); b=0;
     buff_siz=0; *retsiz=0; return b;
   }
  if(newsiz <   3600) newsiz=3600;

  
  if(buff_siz < newsiz)
   { if(b) free(b);
     b=0;
     b = (char *) malloc(newsiz);
     if(!b)
       { printf("get_global_buff: allocation error, size=%d\n",newsiz);
         buff_siz = 0;
       }
     else  buff_siz = newsiz;
   }

 *retsiz = buff_siz;
 return (char *) b;
}

/*
 C\USER DOC
 C Name:     get_global_govo_parsehd
 C Library:  libtfio.a
 C Language: C++
 C Author:   Richard S. Day
 C Purpose:  Read a BRICK or VOXET header file
 C           These files are in the keyword-value(s) style.
 C
 C Prototype:
 C     Grid3DDesc *get_global_govo_parsehd(char *hfile,
 C     int ftype, char *in)
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C
 C\END DOC
 * If file is 0 the info. is echoed to stdout.
 */
Grid3DDesc *get_global_govo_parsehd(char *hfile,int ftype, char *in)
{Grid3DDesc *h=0;

/* Check the file type */
 ftype = getgl_ftype(hfile);
 h = tf3d_create_desc_from_gostr(hfile,ftype,in);
 return h;
}

/*
 C\USER DOC
 C Name:     get_global_rmod_parsehd
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Dcode some information from RMOD header file
 C
 C Prototype:
 C     Grid3DDesc *get_global_rmod_parsehd(char *hfile, int type,
 C              char *in)
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C
 C\END DOC
 */
Grid3DDesc *get_global_rmod_parsehd(char *hfile,int type,char *in)
{
 void    *val=0;
 /*float   *fval; */
 long     nr;
 int      i,ws=4,wdtyp;
 int      Z=0,X=1,Y=2;
 Grid3DDesc *h=0;

 if(!in) return h;
 if(type==HGRID_TYPE || type==LAYER_TYPE) {X=1; Y=2; Z=0;}
 else if (type==HG3DL_TYPE) {X=0; Y=1; Z=2;}
 else return h;
 h = (Grid3DDesc *) calloc(1,sizeof(Grid3DDesc));
 if(!h) return h;
 strcpy(h->header_file,hfile);
 tf3d_rmod_init(h);
 h->ftype = type;
 for(i=0;i<h->nkey;i++)
   {
    nr = dcdut_dcode( (void**) &val, h->keys[i] ,h->fmt[i],in);
    
    if(!val) continue;

    switch(i) {

     case 0:
         h->N.v[Z] = *(int *) val; break;
     case 1:
         h->N.v[X] = *(int *) val; break;
     case 2:
         h->N.v[Y] = *(int *) val; break;
     case 3:
         h->O.v[Z] = *(float *) val; break;
     case 4:
         h->O.v[X] = *(float *) val; break;
     case 5:
         h->O.v[Y] = *(float *) val; break;
     case 6:
         h->D.v[Z] = *(float *) val; break;
     case 7:
         h->D.v[X] = *(float *) val; break;
     case 8:
         h->D.v[Y] = *(float *) val; break;
     case 9:
         h->MI.v[Z] = *(float *) val; break;
     case 10:
         h->MI.v[X] = *(float *) val; break;
     case 11:
         h->MI.v[Y] = *(float *) val; break;
     case 12:
         h->MA.v[Z] = *(float *) val; break;
     case 13:
         h->MA.v[X] = *(float *) val; break;
     case 14:
         h->MA.v[Y] = *(float *) val; break;
     case 15:
         strcpy(h->axis.v[Z],val);  break;
     case 16:
         strcpy(h->axis.v[X],val);  break;
     case 17:
         strcpy(h->axis.v[Y],val);  break;
     case 18:
         h->zdatum = *(float *) val;  break;
     case 19:
         h->np=1;
         strcpy(h->P.file,val);
         dskio_cppath(hfile,h->P.file);
         break;
     case 20:
         h->tran_file = (char *) malloc(120);
         strcpy(h->tran_file,val);
         dskio_cppath(hfile,h->tran_file);
         break;
     case 21:
         strcpy(h->P.etype,val);  break;
    }
    if(val) free(val);
    val=0;
   }

 wdtyp = wrdc_iword_type(h->P.etype);
 if(wdtyp==WIBM2) ws=2;
 if(wdtyp==WCRAY) ws=8;
 if(wdtyp==WBYTE) ws=1;
 if(wdtyp==WSBYT) ws=1;
 h->P.esize=ws;
 return h;
}

/*
 C\USER DOC
 C Name:     get_global_tfile_parsehd
 C Library:  libtfio.a
 C Language: C++
 C Author:   Richard S. Day
 C Purpose:  Read a TFILE header and parse the parm values.
 C           Parameters are decode formatted. There are 2D
 C           and 3D versions of TFILES!
 C
 C Prototype:
 C     TF_Global *get_global_tf_parsehd(TF_Global *g,
 C      char *tfile, char *in)
 C
 C  g     ... input  Structure to receive parameters.
 C  tfile ... input  Name of the data file
 C  in    ... input  First 3600 bytes from the tfile.
 C  ftype ... input  Flag denoting the file type.
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C
 C\END DOC
 */
void *get_global_tfile_parsehd(TF_Global *g, char *tfile,
            int ftype, char *in) {
 return get_global_tf_parsehd(g, tfile, in);
}

void *get_global_tf_parsehd(TF_Global *g, char *tfile,
            char *in)
{char      *targ=0,msg[120];
 long       nr;
 int        j,n1=1,n2=1,n3=1;
 GlobalFmt *gfmt;
 Grid3DDesc   *h=0;
 void      *val=0;

/* Check the file type */
 if(!g) return (void *) h;
/*
 if(ftype!=TFILE_TYPE && ftype!=TF3D_TYPE && ftype!=TF3DF_TYPE) return g;
*/
 targ = strstr(in,"*GLOBAL");
 if(!targ) targ = strstr(in,"*global");
 if(!targ) {
   printf("..._parsehd: tfile inconsistency detected for %s\n",tfile);
   return (void *) h;
 }

/* Get traditional TFILE parameters */
 gfmt = tf_set_gdefs_();
 j = ccode_(gfmt->fmt,gfmt->names,gfmt->goff,gfmt->nkey,
     (char *) g, in, msg);
 if(gfmt) free(gfmt);
 strcpy(g->path,tfile);
 if(strcmp(g->ftyp,getgl_ftype_to_str(TFILE_TYPE))==0) return h;

/* get additional 3D parameters(TF3D_TYPE & TF3DF_TYPE)*/
 h = (Grid3DDesc *) calloc(1,sizeof(Grid3DDesc));
 if(g->h)
  { if(h->tran_file) free(h->tran_file);
    free(g->h);
  }
 g->h = (void *) h;
 tf3d_tfile_init( h);  /* initialize keys */
 strcpy(h->header_file,tfile);
 strcpy(h->P.file,tfile);
 h->ftype = getgl_str_to_ftype(g->ftyp);


/* Scan the input string for parameter settings */
   j=0;
   while(j<h->nkey)
   {if( (targ=strstr(in,h->keys[j])) )
     {nr = dcdut_dcode( (void**) &val, h->keys[j] ,h->fmt[j],in);
      if(!val)
       { /*
         printf("get_global_tfile_parsehd: null val, for key=%s\n",
         h->keys[j]);
         */
         j++;
         continue;
       }
      switch(j) {
       case 0:
         h->N.v[0] = *(int *) val; break;
       case 1:
         h->N.v[1] = *(int *) val; break;
       case 2:
         h->N.v[2] = *(int *) val; break;
       case 3:
         h->O.v[0] = *(float *) val; break;
       case 4:
         h->O.v[1] = *(float *) val; break;
       case 5:
         h->O.v[2] = *(float *) val; break;
       case 6:
         h->D.v[0] = *(float *) val; break;
       case 7:
         h->D.v[1] = *(float *) val; break;
       case 8:
         h->D.v[2] = *(float *) val; break;
        case 9:
         strcpy(h->axis.v[0],val);  break;
       case 10:
         strcpy(h->axis.v[1],val);  break;
       case 11:
         strcpy(h->axis.v[2],val);  break;
       case 12:
         strcpy(h->P.file,val);
         dskio_cppath(tfile,h->P.file); break;
       case 13:
         strcpy(h->P.name,val);  break;
       case 14:
         strcpy(h->P.etype,val);
         g->wdtyp = wrdc_iword_type(h->P.etype);  break;
      case 15:
         if(nr>0) memcpy(h->U.v , val, nr*sizeof(float));
         break;
      case 16:
         if(nr>0) memcpy(h->V.v , val, nr*sizeof(float));
         break;
      case 17:
         if(nr>0) memcpy(h->W.v , val, nr*sizeof(float));
         break;
       case 18:
         h->hd.v[0] = *(int *) val; break;
       case 19:
         h->hd.v[1] = *(int *) val; break;
       case 20:
         h->hd.v[2] = *(int *) val; break;
      }
      if(val) free(val);
      val=0;
     }
    j++;
   }

 h->MI.v[0] =h->O.v[0]; 
 h->MI.v[1] =h->O.v[1]; 
 h->MI.v[2] =h->O.v[2]; 
 h->MA.v[0] =h->O.v[0] + (h->N.v[0]-1)*h->D.v[0]; 
 h->MA.v[1] =h->O.v[1] + (h->N.v[1]-1)*h->D.v[1]; 
 h->MA.v[2] =h->O.v[2] + (h->N.v[2]-1)*h->D.v[2]; 
 if(g->wdtyp==WIBM2) h->P.esize=2;
 if(g->wdtyp==WCRAY) h->P.esize=8;
 if(g->wdtyp==WBYTE) h->P.esize=1;
 if(g->wdtyp==WSBYT) h->P.esize=1;
 return (void *) h;
}


int get_global_grid_to_tfio(TF_Global *g,int ftype, void *h)
{/* Given a grid description, set g in a consistent manner */
 int      X=1,Y=2,Z=0;
 Grid3DDesc *gh=0;
 if(!h || !g) return 0;
 gh = (Grid3DDesc *) h;
 g->grecsiz=0;
 g->numhc = 0;
 g->nhdwd = 0;
 g->wdtyp = WIEEE;
 g->ntrcll= 1;
 g->ndptr = gh->N.v[0];
 g->ntrfil= gh->N.v[1] * gh->N.v[2];
 g->srval = gh->D.v[0];
 strcpy(tf_global_ftype(g),getgl_ftype_to_str(ftype));
  switch(ftype) {
   case HGRID_TYPE:
   case HG3DL_TYPE:
   case LAYER_TYPE:
    X=1;Y=2;Z=0;
    break;
   case VOXET_TYPE:
   case BRICK_TYPE:
    X=0;Y=1;Z=2;
    break;
   default:
    return 0;
  }
 strcpy(g->path,gh->P.file);
 g->wdtyp = wrdc_iword_type(gh->P.etype);
 g->nbydp = gh->P.esize;
 g->nbyhd = gh->P.esize;
 g->xorg  = gh->O.v[X];
 g->yorg  = gh->O.v[Y];
 g->tstrt = gh->O.v[Z];
 g->nbycll= g->nhdwd*g->nbyhd + g->ndptr*g->nbydp;
 if(g->trmaxg==0.) g->trmaxg=1.0;
 return 1;
}

void get_global_cleanup(char *buff)
{int nret;
  if(buff) {
   buff = get_global_buff(0,&nret);
  }
}

