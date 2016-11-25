/*<CPS_v1 type="C_UTILITY_FILE",pretag="!"/>
!--------------------- filereservenetapps.c -------------------------
!            other files are:  pfio.h, cnfg.h 
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
!<brief_description>
! Name       : filereservenetapps
! Category   : io
! Written    : 2001-01-22   by: Charles C. Burch
! Revised    : 2004-06-15   by: Bill Menger
! Maturity   : beta
! Purpose    : A program called using rsh to create a file and reserve space
! References : These routines are called from within pfio.c
!</brief_description>
!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
!  3. 2004-06-15  Bill Menger    Changed to correctly call the script ;^}
!  2. 2004-05-13  Bill Menger    Modified to handle netapps servers
!  1. 2001-01-22  Chuck C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>
*/
/*******************************************************************************
*  To link this program: 
#!/bin/sh
set -x
rm *.o
if test $HOST = 'hotce20'
then
rm *.mod
cc -c -64 -Wl,-multigot -DSGI64 -DBETALIB -I/home/sps/beta/include\
-I/home/sps/production/include ../filereservenetapps.c
f90 -64 -o filereservenetapps *.o -L/usr/lib64\
/home/sps/lib/64sgi73/betalib.a -lpthread 
exit
else
echo " You are not on hotce20"
fi
if test $OSTYPE = 'linux'
then
gcc -c -O3 -Wall -DLINUX -DPRODLIB  -I/home/sps/production/include\
../filereservenetapps.c
ab80_f90 -o filereservenetapps *.o -L/home/sps/lib/linuxab80\
-lfft -lblzw -ljos -lfwcd -lmpi_stubs -llmrk_stubs -lm -lpthread\
/home/sps/lib/linuxab80/prodlib.a
else
cc -c -g -DSOLARIS -DPRODLIB -I/home/sps/production/include\
../filereservenetapps.c
f90 -o filereservenetapps *.o  -lnsl -lposix4 -lsocket\
/home/sps/lib/sol62_debug/prodlib.a
fi
*******************************************************************************/
/*
!--------------------------- start of coding ------------------------------
*/
#include <stdio.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <malloc.h>
#include <netdb.h>
#include <pthread.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <utime.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "pfio.h"
#include "cnfg.h"

char *filereservenetapps_ident =
"$Id: filereservenetapps.c,v 1.3 2004/06/15 20:17:20 Menger prod sps $";
#define RESERVE_NETAPP_SUCCESS  0
#define RESERVE_NETAPP_FAILURE -1


/********************************************************************
*  program is invoked by "filereservenetapps filename file_size"
********************************************************************/
int main (int argc, char *argv[]) {
 FILE *pf;
 char cmd[512], line[128];
 char script[256];

 if(argc != 3) {
    fprintf(stderr,"Error in number args\n");
    return(RESERVE_NETAPP_FAILURE);
 }
 /******************************************************************
  * Get the script from the sps_install_dir environment variable   *
  ******************************************************************/
 strcpy(script,cnfg_get_value_c("sps_install_dir"));
 /*printf("sps install dir = %s\n",script);*/
 strcat(script,"/scripts/reservefilenetapps.csh");
 /*printf("run script %s\n",script);*/
 /******************************************************************
  * Try a file reserve assuming this is a netapps. if it fails,    *
  * (returns 0 bytes) then use nfs below, otherwise you can exit   *
  * before calling the nfs version.                                *
  ******************************************************************/
 strcpy(cmd,script);
 strcat(cmd," -b ");
 strcat(cmd,argv[1]);
 strcat(cmd," ");
 strcat(cmd,argv[2]);
 strcat(cmd," 2>/dev/null");
 /*printf("cmd=%s\n",cmd);*/
 /******************************************************************
  * The call to a file reserve script is ready to fire off         *
  * Read the output from the script.  The last line contains       *
  * the number of bytes actually reserved.  We don't care about    *
  * any other output.                                              *
  ******************************************************************/
 if((pf=popen(cmd,"r"))==NULL){
   fprintf(stderr,"failed to popen [%s]\n",cmd);
   return(RESERVE_NETAPP_FAILURE);
 }
 /******************************************************************
  * Use a pipe-open to fork the script to another process, read the*
  * output from that process, throw everything away but the last   *
  * line.                                                          *
  ******************************************************************/
 while(fgets(line,sizeof(line),pf)!=NULL){/*printf("%s",&line[0]);*/}
 /******************************************************************
  * close the piped-output process                                 *
  ******************************************************************/
 pclose(pf);
 /******************************************************************
  * Here we compare last line of output with expected value        *
  ******************************************************************/
 if (atoll(line) == atoll(argv[2]) ) {
   return(RESERVE_NETAPP_SUCCESS);
 } else {
   return (RESERVE_NETAPP_FAILURE);
 }
}
