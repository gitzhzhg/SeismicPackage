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
/*
*C**************************** COPYRIGHT NOTICE ********************************
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
C  Primitive name:  FTPXFR    (Transfer a disk file)        
C         Library:  CONLIB
C          Author:  Richard Day          
C    Date Wirtten:  91/10/28
C    Last revised:  93/09/30   Day
C
C  Purpose:  Transfer a disk file from the local node to a remote node
C            or vice-versa
C
C  Related Documentation:  other similar routines...
C_______________________________________________________________________
C                                NOTES
C
C  1. ftpxfr uses ftp and will only work between nodes that support
C     ftp file transfer.
C  2. Login information must be defined for the remote node in a
C    .netrc file on the local node.
C
C Function Definition:        ( Language = C )
C void ftpxfr_(char lfile[],char *rfile, char *rnode,
C              int *dir, int *istat)
C  lfile     input      Name of the file on the local node.
C  rfile     input      Name of the file on the remote node.
C  rnode     input      Name of the remote node.
C  dir       input      0 gets the file from the remote node, and
C                       1 puts the file to the remote node.
C  istat     output     0 if no errors occur, transfer is OK.
C-----------------------------------------------------------------------
C                            REVISION HISTORY
C     Date      Author       Description
C 8.  93/09/30  Day          Changed exit call to _exit.
C 7.  93/09/28  Troutt       Replace use of "fork" with "vfork".  This
C                            avoids unnecessary doubling of memory.
C 6.  93/06/08  Troutt       Added pwd command to shell script so that
C                            the remote path would appear in cpr file.
C 5.  93/05/21  Day          Changed value of mode to 0700
C 4.  92/10/06  Day          Removed sunique command from script shell.
C 3.  92/03/06  Day          Suppress sunique for cray to pogun transfers
C 2.  92/03/05  Day          Dump script file to standard out.
C 1.  91/10/28  Day          Original version
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     none 
C  Functions:       FTPXFR 
C  Entry points: 
C  Common blocks:   
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C                              none
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -
C  Heap (dynamic)  -
C-----------------------------------------------------------------------
C\END DOC
*/
#include <stdio.h>
#if (VMS)
#include <stdlib.h>
#include <stddef.h>
#include <file.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#endif
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "ftpxfr.h"

void ftpxfr_(char lfile[],char *rfile, char *rnode, 
            int *dir, int *istat)
{ char line[80], *jf;
  static char *args[2];
  int i,n,l, mode;
  int pid,status;
  FILE *fscrip,*ferr;
  int stde,stdo;
/*
 * output a script file for an ftp transfer */
 fscrip = fopen("ftpscrip","w");
 if(fscrip == NULL)
   { perror("fopen");
     *istat=1;
     return;
   }

 strcpy(line,"#\n");                   fputs(line,fscrip); printf("%s",line);
 strcpy(line,"# ftp script shell\n");  fputs(line,fscrip); printf("%s",line);
 strcpy(line,"ftp "); strcat(line,rnode);
 strcat(line," << EOF\n");             fputs(line,fscrip); printf("%s",line);
 strcpy(line,"verbose\n");             fputs(line,fscrip); printf("%s",line);
 strcpy(line,"pwd\n");                 fputs(line,fscrip); printf("%s",line);
 strcpy(line,"binary\n");              fputs(line,fscrip); printf("%s",line);
 l = strlen(lfile);
 jf= lfile;    /* parse path from local file name */
 for(n=0;n<l;n++)
   { if(lfile[n] == '/') jf = lfile + n + 1; }
 if(rfile == NULL)
   {strcpy(rfile,jf); strcat(line,"\n"); }
 else
   {strcat(line,rfile); strcat(line,"\n");}
 if(*dir == 1)
   { strcpy(line,"#sunique\n");
    if( strncmp(rnode,"POGUN",5)==0 || strncmp(rnode,"pogun",5)==0 )
      { strcpy(line,"#sunique\n");     fputs(line,fscrip); printf("%s",line);}
    else
      { strcpy(line,"#sunique\n");     fputs(line,fscrip); printf("%s",line);}
    strcpy(line,"put ");
    strcat(line,lfile); strcat(line," ");
    strcat(line,rfile); strcat(line,"\n");
                                       fputs(line,fscrip); printf("%s",line);
   }
 else
   {strcpy(line,"runique\n");          fputs(line,fscrip); printf("%s",line);
    strcpy(line,"get ");
    strcat(line,rfile); strcat(line," ");
    strcat(line,lfile); strcat(line,"\n");
                                       fputs(line,fscrip); printf("%s",line);
   }
 strcpy(line,"bye\n");                 fputs(line,fscrip); printf("%s",line);
 strcpy(line,"EOF\n");                 fputs(line,fscrip); printf("%s",line);

 fclose(fscrip);
 mode = 0700; /* S_IRUSR | S_IXUSR | S_IWUSR; */
 chmod("ftpscrip",mode);

 if( (pid=vfork()) < 0 ) /* we are child and vfork failed */
   { perror("vfork");
     printf("errno=%d in ftpxfr.c\n",errno);
     _exit(1);
   }

 if(pid == 0)           /* we are child and vfork is ok */
   { args[0] = NULL;
     stdo = open("ftpmsg",O_CREAT | O_RDWR, 0700);
     stde=stdo;
     printf("vfork okay: stde=%d stdo=%d\n",stde,stdo);
     if(stde != 2) {  close(2); dup(stde); }
     if(stdo != 1) {  close(1); dup(stdo); }
     execvp("./ftpscrip",args);
     perror("ftpscrip");
     _exit(1);
   }
    
 while(wait(&status) != pid) { /* empty */ }

 ferr = fopen("ftpmsg","r");
 n=80;
 i=0;
 if(ferr != NULL)
   { while(fgets(line,n,ferr) != NULL)
      { l=0;
        while(line[l] != 0)
         { line[l]=toupper(line[l]); l++; }
        printf("%s",line);
        if(strstr(line,"TRANSFER COMPLETE") != NULL) i=1;}
     fclose(ferr);
   } 

     /*if(strstr(line,"Transfer complete") != NULL) i=1;}*/

 if(i > 0)
   { *istat= 0;
     printf("ftpxfr: transfer succeeded\n");
   }
 else
   { *istat=1;
     printf("ftpxfr: transfer failed\n");
     return;
   }
/*
 * Get rid of the script file */
 if(remove("ftpscrip") != 0)
   { strcpy(line,"ftpxfr: failed to remove script file=ftpscrip"); 
     printf("%s\n",line);
   }
/*
 * get rid of the message file */
 if(remove("ftpmsg") != 0)
   { strcpy(line,"ftpxfr: failed to remove message file=ftpmsg"); 
     printf("%s\n",line);
   }
 return;
}

