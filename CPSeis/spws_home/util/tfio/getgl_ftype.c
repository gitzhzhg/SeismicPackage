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
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#if( CRAY )
#include <sys/types.h>
#include <sys/stat.h>
/* #include <sys/unistd.h> */
#include <unistd.h>
#include <fcntl.h>
#elif( unix || POSIX )
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#elif VMS
#include <stdlib.h>
#include <unixio.h>
#include <unixlib.h>
#include <file.h>
#include <types.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#endif

#include "cprim.h"
#include "dskio.h"
#include "ebcdic.h"
#include "tfdefs.h"

#ifdef __sgi
#define vfork fork
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
int getgl_ftype_gethdr(char *file,int nbuf, char *buff, long *nbytes);
int getgl_ftype_base(char *file, int nbuf,char *buff,long nbytes);
#ifdef __cplusplus
}
#endif

#define NUMBY  3600

/*
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E S
C
C  Primitive name:  getgl,...
C         Library:  CONLIB and libtfio.a
C          Author:  Richard Day
C    Date Written:  96/07/17
C    Last revised:  99/07/01   Day
C
C         Purpose:  Return the file type if recognized.
C                   ( See tfio.h for type definitions)
C
C      Prototypes:
C
C     int getgl_ftype(char *file);
C
C     int getgl_ftype_hdata(char *file,int nbuf, char *buff,
C              long *nbytes,int *type);
C     int getgl_ftype_gethdr(char *file,int nbuf, char *buff,
C              long *nbytes);
C     int getgl_ftype_base(char *file, int nbuf,char *buff,
C              long nbytes);
C     int getgl_str_to_ftype(char *str);
C   char *getgl_ftype_to_str(int type);
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C   Date       Author      Description
C -----------  --------    --------------------------------------------
C 14.99/07/01  R.Day       Added flag STROT2 as a legit QTROT style
C 13.99/06/09  R. Day      alias protection needed more leaning toothpick
C                          protection(for rsh of ls command)
C 12.99/02/17  R. Day      getgl_ftype_gethdr Had to add -ln option to
C                          rsh of ls, and protect against user aliases.
C 11.99/02/01  R. Day      Updating Conlib & getgl_get_fhdr
C                          changed to getgl_ftype_gethdr
C 10.99/01/29  R. Day      getgl_get_fhdr checks that buffer does
C                          not overflow before reading from pipe.
C 9. 98/12/10  R. Day      Check fopen for valid FILE pointer
C 8. 98/11/30  R. Day      Added more primitive functions.
C                          Illiminated use of unions.
C                          Can now check files on other unix nodes.
C                          Both .byt & .glbl extensions flagged as CBYTE
C                          Fixed minor problem with HGRID files.
C 7. 98/09/02  R. Day      More general recognition of SEGY type.
C                          Can deal with missing ebcdic header now.
C 6. 98/07/23  R. Day      added QTROT_TYPE as a valid file type
C 5. 98/05/07  R. Day      more general STROT/DTROT  recognition 
C 4. 98/02/24  R. Day      sp[n-1] = '\0' rather than sp[n], line 172 ;
C 3. 97/06/09  R. Day      Added LAYER as a recognized type. 
C 2. 96/10/23  R. Day      Added TF3D and TF3DF types.
C 1. 96/07/17  Vunderink   Inserted into the conlib library.
C-----------------------------------------------------------------------
C NOTES
C  Language: C
C Libraries: CONLIB(conlib.a) and libtfio.a         (placed into)
C  Includes: cprim.h  ebcdic.h  tfdefs.h dskio.h
C
C  1. With the revision of 98/11/30 getgl_ftype(file) can now
C     handle files that reside on remote nodes. The syntax
C     of the file name for remote nodes is either of the following:
C      a) user@node:file_name
C      b) node::user;;file_name
C  2. For remote files, may fork a child process which will
C     issue a system call to execute an rsh call to the unix
C     dd command. Pipes are used to communicate with the remote
C     node. This lets getgl_ftype check the file type of a
C     remote file without having to transfer the whole file.
C     !!! On the crays rsh is wired to /usr/ucb/rsh !!!
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -
C  Heap (dynamic)  - mallocs and frees 3600 bytes.
C-----------------------------------------------------------------------
C\END DOC
*/

/*
 * returns the file type. ( See tfio.h )
 */
int getgl_ftype(char *file)
{int        cnt,type=UNKWN_TYPE;
 char      *buff;
 long       nbytes;
 if(!file) return type;
 buff = (char *) malloc(3600);
 if(!buff) return type;
 cnt=getgl_ftype_gethdr(file,3600,buff,&nbytes);
 if(cnt > 0) {
   type= getgl_ftype_base(file,cnt,buff,nbytes);
 }
 free(buff);
 return type;
}

/*
 * Given a work buffer,buff, of size nbuf,
 * returns number of bytes read into buff from file.
 * The argument, type, is the file type( See tfio.h ).
 */
int getgl_ftype_hdata(
    char *file,int nbuf, char *buff, long *nbytes,int *type)
{int cnt=0;
 *type=UNKWN_TYPE;
 if(!file) return 0;
 cnt=getgl_ftype_gethdr(file,nbuf,buff,nbytes);
 if(cnt>0) {
   *type = getgl_ftype_base(file,nbuf,buff,*nbytes);
 }
 return cnt;
}

/*
 * int getgl_ftype_gethdr(char *file,int nbuf, char *buff, long *nbytes);
 * file   = file name, which can be a network name.
 * nbuf   = the maximum number of bytes to read into buff.
 * nbytes = the number of bytes in file.
 * Returns number of bytes actually read or -1 for error.
 */
int getgl_ftype_gethdr(char *file,int nbuf, char *buff, long *nbytes)
{FILE  *fpntr;
 int    i,nn,ln,lu,lf,nrd,ntord,namlen=80;
 int    pid,pipefds[2],local_node=0,status;
 char   n[64],u[64],f[96],line[120],*val;
 char   host[80],rsh[32];
 struct stat statbuf;
 *nbytes=0;
 ln = dskio_node_(file,n);
 lu = dskio_userid_(file,u);
 lf = dskio_file_(file,f);
 buff[0]='\0';
 if(ln>0) { /* possible remote file */
   if(strcmp(n,"NONE")==0) {
     local_node=1;
   } else {
#ifdef VMS
     val = getenv("HOST");
     if(val) { strcpy(host,val);}
     i = strlen(host);
#else
     i=gethostname(host,namlen);
#endif
     if(i>=0)  {
      if(strcmp(n,host)==0) {
        /* printf("host is the same: %s\n",n); */
        local_node=1;
      }
     }
   }
 } else local_node=1;

 if(local_node==1) { /* file resides locally */
   if(access(f,F_OK) <0) return -1;
   fpntr= fopen(f,"r");
   if(!fpntr) {
    printf("getgl_ftype: fopen failure\n");
    return -1;
   }
   fseek(fpntr,0L,SEEK_END);
   *nbytes=ftell(fpntr);
   fseek(fpntr,0L,SEEK_SET);
   ntord = (*nbytes > nbuf) ? nbuf : *nbytes;
   nrd = fread(buff,sizeof(char), ntord, fpntr);
   fclose(fpntr);
   return nrd;
 } else {  /* file on remote (unix?) node */
   if(strcmp(n,"pogun")==0 || strcmp(n,"POGUN")==0) {
    return -1;
   }
   if(pipe(pipefds) < 0) {
     perror("# bad pipe");
     return -1;
   }
   if( (pid=vfork()) < 0 ) {
     perror("# vfork failed");
     printf(" rcpxfr: fork failed no child \n");
     return -1;
   }
   
   if(pid==0) { /* child executes this */
   strcpy(rsh,"rsh");
#ifdef CRAY
   strcpy(rsh,"/usr/ucb/rsh");
#endif
     close(1); /* make childs stdout the pipes stdout */
     dup(pipefds[1]);
     if(lu>0)
      sprintf(line,
      "%s %s -l %s \\\"ls\\\" -ln %s | awk \'{print \"NBYTES=\"$5}\'",
       rsh,n,u,f);
     else
      sprintf(line,
      "%s %s \\\"ls\\\" -ln %s | awk \'{print \"NBYTES=\"$5}\'",
       rsh,n,f);
     system(line);
     if(lu>0)
      sprintf(line,
      "%s %s -l %s dd bs=%d count=1 if=\"%s\"",
       rsh,n,u,nbuf-24,f);
     else
      sprintf(line,"%s %s dd bs=%d count=1 if=\"%s\"",
       rsh,n,nbuf-24,f);
     system(line);
     _exit(1);

   }

   while(wait(&status) != pid) {/* hang until rsh completes */
   }
   i = fstat(pipefds[0], &statbuf);
   nrd=0;
   ntord=0;
   if(i>=0) {
    ntord = statbuf.st_size;
    if(ntord > 0 && ntord < nbuf) {
     fpntr=fdopen(pipefds[0],"r");
     nrd=fread(buff,1,ntord,fpntr);
     fclose(fpntr);
     if(strncmp(buff,"NBYTES=",7)==0) {
      sscanf(buff,"NBYTES=%d",nbytes);
      nn =0;
      while(buff[nn] != '\n' && nn<nrd) nn++;
      nn++;
      memcpy(buff,buff+nn,nrd-nn);
      nrd= nrd-nn;
     }
    } else {
       printf("getgl_ftype_gethdr: buffer is too small?,%d %d\n",nbuf,ntord);
    }
   }
   close(pipefds[0]);
   close(pipefds[1]);
 }
 return nrd;
}
/*
 * Returns the file type given nbuf bytes from the file.
 * Normally the 1st 3600 bytes.
 * The argument buff is from the head of the file.
 */
int getgl_ftype_base(char *file, int nbuf,char *buff,long nbytes)
{
 int        type = UNKWN_TYPE;
 int        ndpt,ntrfil,ws=4,code;
 void      *val=0;
 unsigned char *sp, tbuff[3600];
 char      *hit1, *hit2, *hit3, *hit4, *wt;
 int        cnt,n;
 

 if(!buff) return type;
 if(!file) return type;

 cnt= (nbuf>3600) ? 3600 : nbuf;

 if(cnt>0) {
   buff[cnt-1]='\0';
   memcpy(tbuff,buff,cnt);
   sp = (unsigned char *) tbuff;
   while(sp[0]) {  sp[0] = toupper(sp[0]); sp++; }
   sp = (unsigned char *) tbuff;
 } else { return BADFN_TYPE;}

 /*Cannot do this any more since users want to write trot files with 
   a .byt extension. M.L.Sherrill 12/00
 if(strstr(file,".byt")!=NULL || strstr(file,".BYT")!=NULL )
   { type = CBYTE_TYPE;
     goto jump;
   }
 */

 if(strstr(file,".glbl")!=NULL || strstr(file,".GLBL")!=NULL )
   { type = CBYTE_TYPE;
     goto jump;
   }

 if( (hit1 = strstr((char *)sp,"PICKS")) != 0)
   { if((unsigned char *)hit1 -sp < 160)
      { type = GWS_TYPE; goto jump;}
   }

 if( (hit1 = strstr((char *)sp,"#<CPS_V1")) != 0)
   {
      type = TROT_TYPE; goto jump;
   }

 if( (hit1 = strstr((char *)sp,"*GLOBAL ")) != 0)
   { type = TFILE_TYPE;
     dcdut_dcode( &val,"FTYP", 'c', (char *) sp);
     if(val) {
      if(strcmp((char *) val,"TFILE3")==0) type=TF3D_TYPE;
      if(strcmp((char *) val,"TFILE3F")==0) type=TF3DF_TYPE;
      free(val);
     }
     goto jump;
   }

 if( (hit1 = strstr((char *)sp,"*HEADER")) != 0 ||
             (strstr((char *)sp,"XCOORDINATE") != 0))
   {type=RMOD_TYPE;
    hit1=(char *)sp;
    if( (hit2=strstr(hit1,"TYPE")) != 0 ) {
      if( (hit3=strstr(hit2,"=")) != 0) {
        if(hit3-hit2 < 20) {
          if( (hit4=strstr(hit3,"GRID")) != 0) {
            if(hit4-hit3 < 8)   type=HGRID_TYPE;
             if( (wt=strstr(hit1,"WORDTYPE")) != 0) {
              hit1 = strstr(wt,"AGRID");
              if(hit1 && (hit1>wt)) type = AGRID_TYPE;
             }
            }
            if( (hit4=strstr(hit3,"LAYER")) != 0)
             if(hit4-hit3 < 8)  type=LAYER_TYPE;
            if( (hit4=strstr(hit3,"G3DL")) != 0)
             if(hit4-hit3 < 8)   type=HG3DL_TYPE;
        }
      }
    }
    goto jump;
   }

 if( (hit1 = strstr((char *)sp,"GOCAD ")) != 0)
   { type = GOCAD_TYPE;
     if( (hit2=strstr(hit1,"VOXET ")) != 0 )
       if(hit2-hit1 < 60)  type=VOXET_TYPE;
     if( (hit2=strstr(hit1,"BRICK ")) !=0)
       if(hit2-hit1 < 60)  type=BRICK_TYPE;
     goto jump;
   }

 if(strncmp((char *) (sp+136),"STROT",5) == 0 )
   { type  = STROT_TYPE;
     if(sp[141]=='1' || sp[141]=='2') type=QTROT_TYPE;
     goto jump;
   }

 if(strncmp((char *) sp+136,"DTROT",5) == 0 )
   { type  = DTROT_TYPE;
     goto jump;
   }

 if(sp[0]=='C' && sp[80] == 'C' && sp[160]== 'C')
   { type = SEGY_TYPE;
     goto jump;
   }
 if(nbytes <=3600) goto jump;
 wrdc_convstr((unsigned char *) &sp[3220], &ndpt,2);
 wrdc_convstr((unsigned char *) &sp[3224], &code,2);
 if(code==3) ws=2;
 if(code>=5) ws=1;

 if(ndpt <=0) goto jump;
 ntrfil = (nbytes-3600)/(240 + ws*ndpt);
 if(nbytes == (3600 + ntrfil*(240+ws*ndpt))) {
  type=SEGY_TYPE;
  goto jump;
 }
 for (n = 0; n < 240; n++)
   { sp[n] = ebcdic_to_ascii[sp[n]]; }
 sp[n-1] = '\0';
 if(sp[0] == 'C' && sp[80] == 'C' && sp[160]== 'C')
   { type=SEGY_TYPE; goto jump;}

 jump:
 return type;
}

/*
 C\USER DOC
 C Name:     getgl_ftype_to_str
 C Library:  CONLIB and libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Convert an integer file type into a string
 C
 C Prototype:
 C     char *getgl_ftype_to_str(int type);
 C\END DOC
 */
char *getgl_ftype_to_str(int type)
{ static char str[8];

  switch(type) {
  case BADFN_TYPE :
    strcpy(str,"BADTYPE"); break;
  case UNKWN_TYPE :
    strcpy(str,"UNKNOWN"); break;
  case GOCAD_TYPE :
    strcpy(str,"GOCAD"); break;
  case RMOD_TYPE  :
    strcpy(str,"RMOD"); break;
  case BRICK_TYPE :
    strcpy(str,"BRICK"); break;
  case VOXET_TYPE :
    strcpy(str,"VOXET"); break;
  case HGRID_TYPE :
    strcpy(str,"HGRID"); break;
  case LAYER_TYPE :
    strcpy(str,"LAYER"); break;
  case HG3DL_TYPE :
    strcpy(str,"HG3DL"); break;
  case TFILE_TYPE :
    strcpy(str,"TFILE"); break;
  case SEGY_TYPE  :
    strcpy(str,"DSEGY"); break;
  case STROT_TYPE:
    strcpy(str,"STROT"); break;
  case QTROT_TYPE:
    strcpy(str,"QTROT"); break;
  case TROT_TYPE:
    strcpy(str,"TROT"); break;
  case DTROT_TYPE :
    strcpy(str,"DTROT"); break;
  case CBYTE_TYPE :
    strcpy(str,"CBYTE"); break;
  case AGRID_TYPE :
    strcpy(str,"AGRID"); break;
  case TF3D_TYPE :
    strcpy(str,"TFILE3"); break;
  case TF3DF_TYPE :
    strcpy(str,"TFILE3F"); break;
  default:
    strcpy(str,"BADTYPE"); break;
  }

 return str;
}

/*
 C\USER DOC
 C Name:     getgl_str_to_ftype
 C Library:  CONLIB and libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Convert a string into an integer file type
 C
 C Prototype:
 C     int  getgl_str_to_ftype(char *str);
 C\END DOC
 */
int getgl_str_to_ftype(char *str)
{
  if(!str || strlen(str) == 0) return UNKWN_TYPE;
  if(strcmp(str,"TFILE")==0) return TFILE_TYPE;
  if(strcmp(str,"VOXET")==0) return VOXET_TYPE;
  if(strcmp(str,"HGRID")==0) return HGRID_TYPE;
  if(strcmp(str,"LAYER")==0) return LAYER_TYPE;
  if(strstr(str,"G3DL")) return HG3DL_TYPE;
  if(strcmp(str,"TFILE3")==0) return TF3D_TYPE;
  if(strcmp(str,"TFILE3F")==0) return TF3DF_TYPE;
  if(strcmp(str,"UNKNOWN")==0) return UNKWN_TYPE;
  if(strcmp(str,"BRICK")==0) return BRICK_TYPE;
  if(strcmp(str,"CBYTE")==0) return CBYTE_TYPE;
  if(strcmp(str,"STROT")==0) return STROT_TYPE;
  if(strcmp(str,"STROT1")==0) return QTROT_TYPE;
  if(strcmp(str,"TROT")==0) return TROT_TYPE;
  if(strcmp(str,"DSEGY")==0) return SEGY_TYPE;
  if(strcmp(str,"BADTYPE")==0) return BADFN_TYPE;

 return UNKWN_TYPE;
}


