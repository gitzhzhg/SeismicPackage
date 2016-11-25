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
C      netw_getfil.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y
C       written in c -- designed to be called from c
C
C     Utility Name:  netw_* (functions to send files across a network)
C          Written:  94/02/14  by:  Day
C     Last revised:  98/11/30  by:  Day
C
C  Purpose:       Uses rcp to fetch a file across the network.
C
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  node         o/s       source code directory       library
C  ----         ---       ---------------------       -------
C  pospsv       ultrix    ~spws/util/tfio             libtfio.a
C  CPS      (pogun/cray)  [cpx.primitive.prim_io]     NEWLIB (on cray)
C  pogun        VMS       [cpx.zz_vax_primitives]     TUTLIB
C
C  source files     header files    include files      other files
C  ------------     ------------    -------------      -----------
C  netw_getfil.c    netw.h
C                   rcpxfr.h
C                   dskio.h
C
C  The user should include the above header file in his code.
C----------------------------------------------------------------------
C             DIFFERENCES BETWEEN CODE ON DIFFERENT MACHINES
C
C-----------------------------------------------------------------------
C                  C ROUTINES IN SOURCE FILE netw_getfil.c
C
C  Documented routines:  netw_getfil_(), netw_putfil_()
C
C-----------------------------------------------------------------------
C                 EXTERNALS REFERENCED BY THIS UTILITY
C
C rcpxfr   dskio_parse_file   netw_lnode
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  5. 98/11/30  Day        netw_netname_cps will honor explicit node user
C                          information 1st, then the _netinfo file.
C  4. 98/01/07  Day        Added function for parsing file names and getting
C                          information from _netinfo file.
C  3. 98/01/05  Day        long to int replacement. netw.h and rcpxfr.h
C                          headers used.
C  2. 94/02/17  Day        rnode = NONE returns -1
C  1. 94/02/14  Day        Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C int netw_getfil_(char *lfile, char *rnode, char *ruser, char *rfile,
C      int *wdtyp, char *msg);
C            fetches a file via rcp
C int netw_putfil_(char *lfile, char *rnode, char *ruser, char *rfile,
C      int *wdtyp, char *msg);
C            sends a file via rcp
C int  netw_netinfo_(char *rnode, char *ruser, char *path);
C            gets information from the _netinfo file
C int  netw_netname(char *c,char *n,char *u,char *f,int *bld);
C            parses or constructs a network file name
C int  netw_netname_cps(char *c,char *n,char *u,char *f,int *bld);
C            parses or constructs a network file name using _netinf
C void netw_file_cppath(char *model, char *file);
C            uses path from model to file if it is a bare file name
C
C  Type    Name    I/O     Valid     Description
C  ----    ----    ---     -----     -----------
C  char *  lfile   IN&OUT            Name of the local file
C  char *  rnode   IN                Name of a network node(e.g. pogun)
C  char *  ruser   IN                User Name at rnode.
C  char *  rfile   IN                Name of the file at rnode.
C  int  *  wdtyp   OUT               float word type at rnode for getfil
C                                    and at lfile node for putfil
C                                    (see wrdcnvrt.h )
C  char *  msg     OUT               Informative error message
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. returns 0 if there are no problems.
C  2. returns -1 if rnode = local node or rnode=NONE
C  3. if strlen(lfile) is 0 then lfile will be set from rfile name.
C  4. if strlen(rfile) is 0 then rfile will be set from lfile name.
C-----------------------------------------------------------------------
C\END DOC
*/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "dskio.h"
#include "rcpxfr.h"
#include "netw.h"


int netw_getfil_(char *lfile, char *rnode, char *ruser, char *rfile,
     int *wdtyp, char *msg)
{ /* returns -1 if rnode = local_node name*/
 int    l,ls,getit;
 int    idir,istat,local_word;
 char   lnode[32],los[32];
 char   rname[32],rpath[80];
 extern int netw_nodes_wrd(),netw_lnode();
/********************************************
 * Check if grid header file name is legal **
 * rfile=file name at remote node.        ***
 * lfile=file name at local node.         ***
 *******************************************/
  sprintf(msg,"netw_getfil: invalid input file=%s",rfile);
  if(rfile == NULL) goto error;
  ls =strlen(rfile);
  if(ls==0) goto error;
  if(strcmp(rfile,"NONE")==0) goto error;
  if(ls==1 && rfile[0] == ' ') goto error;
  msg[0]='\0';
/********************************************
 * Construct a local name,lfile, from rfile**
 * Set the wdtyp flag for the input file  ***
 * See tfio.h & wrdcnvrt.h for details.   ***
 *-Retreive file(if necessary)            ***
 *******************************************/
  local_word = netw_lnode(lnode,los);
  *wdtyp = 0;
  getit = 1;
  if(rnode==NULL) getit = 0;
  else
   {if(strncmp(rnode,"NONE",4)==0) getit = 0;
    if(strncmp(rnode,"SAME",4)==0) getit = 0;
    if(strncmp(rnode,"LOCAL",5)==0) getit = 0;
    if(strlen(rnode)==0) getit = 0;
    if(strlen(rnode)==1 && rnode[0]==' ') getit = 0;
   }
  if(getit==0)
   {/* file is a local file */
    strcpy(lfile,rfile);
    *wdtyp = local_word;
    return -1;
   }
  else
   {dskio_parse_file(rfile,rname,rpath);
    if(strlen(lfile)==0 || lfile[0]==' ')
     {dskio_parse_file(rfile,rname,rpath);
      strcpy(lfile,rname);
     }
    l = 0;
    while(rnode[l]!=0) {rnode[l] = tolower(rnode[l]); l++; }
    *wdtyp = netw_nodes_wrd(rnode);
    if(*wdtyp<=0)
     {sprintf(msg,"netw_getfil: unknown word type, file=%s",rfile);
     }
    idir = 0;
    if(strstr(lnode,rnode) != NULL)
     {strcpy(lfile,rfile); return -1; }
    rcpxfr_(lfile,rfile,rnode,ruser,&idir,&istat);
    if(istat!=0)
     {sprintf(msg,"netw_getfil: rcpxfr error , file=%s",rfile);
      goto error;
     }
   }

 return 0;
 error:
 return 1;
}

int netw_putfil_(char *lfile, char *rnode, char *ruser, char *rfile,
     int *wdtyp, char *msg)
{ /* returns -1 if rnode = local_node name*/
 int    l,ls,putit;
 int    idir,istat;
 char   lname[32],lpath[80],lnode[32],los[32];
 char   rname[32],rpath[80];
 extern int netw_nodes_wrd(),netw_lnode();
/********************************************
 * Check if grid header file name is legal **
 * rfile=file name at remote node.        ***
 * lfile=file name at local node.         ***
 *******************************************/
  sprintf(msg,"netw_putfil: invalid input file=%s",rfile);
  if(lfile == NULL) goto error;
  ls =strlen(lfile);
  if(ls==0) goto error;
  if(strcmp(lfile,"NONE")==0) goto error;
  if(ls==1 && lfile[0]==' ') goto error;
  msg[0]='\0';
/********************************************
 * Construct a remote name,rfile, from lfile**
 * Set the wdtyp flag for the input file  ***
 * See tfio.h & wrdcnvrt.h for details.   ***
 *-Retreive file(if necessary)            ***
 *******************************************/
  *wdtyp = netw_lnode(lnode,los);
  putit = 1;
  if(rnode==NULL) putit = 0;
  else
   {if(strncmp(rnode,"NONE",4)==0) putit = 0;
    if(strncmp(rnode,"SAME",4)==0) putit = 0;
    if(strncmp(rnode,"LOCAL",5)==0) putit = 0;
    if(strlen(rnode)==0) putit = 0;
    if(strlen(rnode)==1 && rnode[0]==' ') putit = 0;
   }
  if(putit==0)
   {/* file is already a local file */
    strcpy(rfile,lfile);
    return -1;
   }
  else
   {dskio_parse_file(rfile,rname,rpath);
    if(strlen(rfile)==0 || rfile[0]==' ')
     {dskio_parse_file(lfile,lname,lpath);
      strcpy(rfile,lname);
     }
    l = 0;
    while(rnode[l]!=0) {rnode[l] = tolower(rnode[l]); l++; }
    if(*wdtyp<=0)
     {sprintf(msg,"netw_putfil: unknown word type, file=%s",rfile);
     }
    if(strstr(lnode,rnode) != NULL)
     {strcpy(rfile,lfile); return -1; }
    idir = 1;
    rcpxfr_(lfile,rfile,rnode,ruser,&idir,&istat);
    if(istat!=0)
     {sprintf(msg,"netw_putfil: rcpxfr error , file=%s",rfile);
      goto error;
     }
   }

 return 0;
 error:
 return 1;
}

int netw_netinfo_(char *node, char *user, char *path)
{
/********************************************
 * Reads the file _netinfo that is prepared**
 * by the job script for jobs submitted via**
 * CPS. Returns 1 if OK, 0 if not OK       **
 ********************************************/
   FILE  *filePtr = NULL;
   char  _node[24],_user[24],_path[96];
   char  file[32];
   int   i,OK=1,NOTOK=0;

   strcpy(node,"NONE");  _node[0]='\0';
   strcpy(user,"NONE");  _user[0]='\0';
   strcpy(path,"NONE");  _path[0]='\0';

   strcpy(file,"_netinfo");
   if( (filePtr = fopen (file, "r")) == 0) {
      printf("netw_netinfo: could not open %s\n",file);
      return NOTOK;
   }

   i = fscanf(filePtr,"%s%s%s",_user,_node,_path);
   if(i== EOF) { if(filePtr) fclose(filePtr); return NOTOK; }

   strcpy(node,_node);
   if( i>1) strcpy(user,_user);
   if( i>2) strcpy(path,_path);

   if(filePtr) fclose(filePtr);
   return OK;
}

int netw_netname_cps(char *c,char *n,char *u,char *f,int *bld)
{char p[96];
 int i,l;
/********************************************
 * Same as netw_netname except will get    **
 * n,u and path from _netinfo file if this **
 * information is absent from c.           **
 ********************************************/
 if(*bld==0) {
  l=netw_netname(c,n,u,f,bld);
  if(strcmp(n,"NONE")==0) {
   i=netw_netinfo_(n,u,p);
   if(i==1) {
    netw_file_cppath(p,f);
   }
  }
 } else {
  if(strcmp(n,"NONE")==0) {
   i=netw_netinfo_(n,u,p);
   if(i==1) {
    netw_file_cppath(p,f);
   }
  }
  l=netw_netname(c,n,u,f,bld);
 }

 return l;
}

int netw_netname(char *c,char *n,char *u,char *f,int *bld)
{
 int i;
/********************************************
 * Break the name stored in c into fields  **
 * or construct c from data in n,u and f.  **
 * Assumed syntax for c -> c = n::u;;f     **
 * bld=0 to parse, bld=1 to construct.     **
 * n=node name, u=user name, f=file name   **
 ********************************************/
 if(c==NULL || n==NULL || u==NULL || f==NULL) goto error;

 if(*bld == 1) {
   if(strlen(n)==0 || strcmp(n,"NONE")==0) {
      strcpy(c,f);
      return 0;
   }
   if(strlen(u)==0 || strcmp(u,"NONE")==0) {
      sprintf(c,"%s::NONE;;%s",n,f);
      if(*bld==2) sprintf(c,"%s:\"%s\"",n,f);
   } else {
      sprintf(c,"%s::%s;;%s",n,u,f);
      if(*bld==2) sprintf(c,"%s@%s:\"%s\"",u,n,f);
   }
   return 0;
 }

 n[0]='\0';
 u[0]='\0';
 f[0]='\0';
 
 i = dskio_node_(c,n);
 if(i<=0) strcpy(n,"NONE");
 i = dskio_userid_(c,u);
 if(i<=0) strcpy(u,"NONE");
 i = dskio_file_(c,f);

 return 0;
 error:
 return 1;
}


void netw_file_cppath(char *model, char *file)
{ char mpath[80],mname[80],path[80],name[80];
/********************************************
 *--model.... a template file name.       ***
 *--file..... file name to add path to.   ***
 *--Set the path of file = path for model,***
 *--unless it has an explicit path already***
 *--Assumes standard unix or vms paths.   ***
 *******************************************/
 if(model==NULL || file==NULL) return;
 if( (strlen(file)==0) || file[0]==' ') {
   strcpy(file,model);
   return;
 }
 if(strlen(model)==0 ) return;
 if(strcmp(file,"SAME")==0) {
   strcpy(file,model);
   return;
 }

 dskio_parse_file(model,mname,mpath);
 dskio_parse_file( file, name, path);
 if(strlen(path) > 0) return; /* keep old path */
 else strcpy(path,mpath);     /* adopt model path */

 if(strcmp(name,"NONE") == 0) {
  strcpy(file,name); return;
 }
 else {sprintf(file,"%s%s",path,name); return; }

}

