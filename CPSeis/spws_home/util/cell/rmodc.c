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
#include <stdlib.h>
#include "rmodc.h"
#include "wrdcnvrt.h"
#include "dbutil.h"
#include "cprim.h"
#include "netw.h"

#ifdef SUNOS
#include <spws_sunos.h>
#endif

 static int   bsiz;
 static char *wbuf;

/* wrapper around function in tfio library */
int  rmod_lochost_(char *lname, char *los)
{ int  wdtyp;
  char hostname[32],os[16];
  hostname[0]='\0';
  os[0]='\0';
  wdtyp=netw_lnode(hostname,os);
  if(lname!=NULL ) strcpy(lname,hostname);
  if(los!=NULL ) strcpy(los,os);
  return wdtyp;
}

void rmod_rmfil_(char *file)
{ if(file==NULL) return;
  if(strlen(file)==0) return;
  remove(file);
}

int  rmod_shell_w_(char *cmd)
{int  istat;
 istat = 1;
#ifdef CRAY
 istat = ISHELL(cmd);
#endif
return istat;
}

int rmod_cray_data_(char *node, char *user, char *path)
{/* Returns 1 if running on cray, 0 otherwise.         */
 /* Reads the file _netinfo which is prepared by the   */
 /* job script for jobs submitted thru a cray station. */
   FILE  *filePtr = NULL;
   char  _node[24],_user[24],_path[96];
   char  file[16];
   int   i, icray=0;

   node[0]='\0'; _node[0]='\0';
   user[0]='\0'; _user[0]='\0';
   path[0]='\0'; _path[0]='\0';
#ifdef CRAY
   icray = 1;
/*
   system("rhostinfo > ./_netinfo");
*/
#endif
 
   if(!icray) return icray; /* return false if not cray  */

   strcpy(file,"_netinfo");
   if ( (filePtr = fopen (file, "r")) == 0)
    { printf("trouble with %s\n",file);
      return icray;
    }
 

   i = fscanf(filePtr,"%s%s%s",_user,_node,_path);
   if(i== EOF) { if(filePtr) fclose(filePtr); return icray; }

   strcpy(node,_node);
   if( i>1) strcpy(user,_user);
   if( i>2) strcpy(path,_path);

   if(filePtr) fclose(filePtr);
   return icray;
}

int rmod_name_bld_(char *namein, char *nameout)
{/* nameout = namein unless called on the cray */
 /* Construct network file name on cray */
 /* construct a name that includes node, user and path info */
 /* returns 1 if on cray , 0 otherwise */
  char n1[32],u1[16],p1[88];
  char n[32], u[16], f[88], str[120];
  char *fname, name[32],path[80];
  int   ierr,local=0;
  int   icray=0;
  extern void rmodglf_(int  *);

#ifdef CRAY
   icray = 1;
#endif
 strcpy(nameout,namein);
/*
 * get information about remote node, user, & cwd **
 * Not valid other than on the cray
 */
 if(!icray) return icray;
 if(!rmod_cray_data_(n1,u1,p1)) return icray;
 if(strlen(n1)==0) return icray;
 rmodglf_(&local);
 if(local) return icray;

/*
 * Break input name into  node, user, file parts.
 * distinguish between an explicit n=NONE and n=NONE from parse_fnet
 */
 if(strncmp(namein,"NONE",4)==0) return icray;
 parse_fnet_(namein,n,u,f,str);
 if(strcmp(n,"NONE")!=0) return icray;

/*
 * Further break file name into path and local parts.
 * Then combine passed name with the path.
 */
 fname = (char *) parse_file_( f , name, path );
 if(strlen(path) == 0 && strlen(p1)>0)
  { int l;
    strcpy(f,p1);
    l = strlen(p1);
    if(p1[l-1]!=']' && l!=0) strcat(f,"/");
    strcat(f,name);
  }
    
 ierr = parse_fbld_(str,n1,u1,f);
 if(ierr != 1) strcpy(nameout,str);
 return icray;
}

/**********************************************************************/
 int  rmodopc_ (int  *fortranUnit, char *localFile, char *remoteFile,
        char *accessType, char *status, char *formatType,
        int  *recordLength, int  *word_type)
{ /*    Opens a c-file and returns the database index; if the open is
        unsuccessful, a -1 is returned.
        -------------------------------  */

        FILE            *filePtr = 0L;


        int             firstOpenIndex = 0;
        int             indexNumber;

        char            openMode[5];


        dbutil_allcaps (formatType);
        dbutil_allcaps (status);
        dbutil_allcaps (accessType);

/*      Is the file already open?
        -------------------------  */

        if ((indexNumber = dbutil_isopen (localFile)) > 0)
        {
          *word_type = dbutil_getwordtype_(&indexNumber);
          *recordLength = dbutil_getreclength_(&indexNumber);
          return (indexNumber);
        }

/*      Are there any available data base entrys?
        --------------------------------------  */
        if ((indexNumber = dbutil_getnew_ckey_()) < 0)
         { return (indexNumber); }

/*      Open the file; return an error if there is a problem
        ----------------------------------------------------  */
        strcpy (openMode, "rb+");
        if (strcmp (status, "NEW") == 0 ||
                strcmp (status, "SCRATCH") == 0)
         { strcpy (openMode, "wb+"); }

        if ((filePtr = fopen (localFile, openMode)) == 0)
         { 
          strcpy (openMode, "rb");
          filePtr = fopen (localFile, openMode);
          if(filePtr)
           { printf("rmodopc: had to open in readonly mode\n");
           }
          else
           { indexNumber = -1L;
             return (indexNumber);
           }
         }


/*      Save the values into the database
        ---------------------------------  */
        dbutil_add_(&indexNumber, filePtr, fortranUnit, localFile,
                remoteFile, accessType, status, formatType, recordLength);
        dbutil_putwordtype_(&indexNumber,word_type);
        return (indexNumber);

}

/**********************************************************************/
 int  rmodclc_ (int  *indexNumber)
{ /*    Closes the file associated with the input database index number;
        if there is a problem closing the file, -1 is returned.  Note that
        if the database thinks the file doesn't exist (or is already closed),
        it assumes the file is already closed and passes back a 0 without
        trying to close it.
        -------------------  */

        FILE            *filePtr;
        if ((filePtr = dbutil_getfileptr(indexNumber)) == 0)
         { return (0L); }

        if (fclose (filePtr) == 0)
         { dbutil_del_(indexNumber);
           return (0L);
         }
        else
         { return (-1L); }
}
 

void rmodrdc_(int  *key,int  *irec, int  *nrec, int  *n, char *data,
     int  *wdtyp,int  *stat, char *msg)
{/* wdtyp is the word type of the input file */
 int  bytoff,reclen,dsize,j;
 int  typi,typo,nr,mbytes,maxrec;
 int ierr;
 char   *dptr;
 char name[32],os[16];
 FILE *fptr;
 *stat = 1;
 if(*irec < 1 || *n < 1) return;
 typi = *wdtyp;
 typo  = netw_lnode(name,os);
 fptr = dbutil_getfileptr(key);
 if(fptr == 0)
  {sprintf(msg,"rmodrdc: NULL file pointer\n");
   return;
  }
 reclen= dbutil_getreclength_(key);
 if(reclen <= 0)
  {sprintf(msg,"rmodrdc: record length <=0 \n");
   return; }
 dsize  = 4*(*n);
 if(typo==WCRAY) dsize = 2*dsize;
 if(*n > reclen/4)
  {sprintf(msg,"rmodrdc: 4*n > record size\n");
   return;
  }

  if(reclen > bsiz)
   {if(wbuf != NULL ) free(wbuf);
    if(typo!=WCRAY)
     { wbuf = (char *) calloc(1,reclen); bsiz=reclen; }
    else
     { wbuf = (char *) calloc(1,2*reclen); bsiz = 2*reclen; }
   }
  if(wbuf == NULL)
   {sprintf(msg,"rmodrdc: error allocating buffer %d\n",bsiz);
    bsiz = 0;
    return;
   }

 bytoff = (*irec-1)* reclen;    /*reclen is the record size in bytes */
 ierr = fseek(fptr,bytoff,SEEK_SET);
 if(ierr != 0)
  { fseek(fptr,0,SEEK_END);
    mbytes = ftell(fptr);
    maxrec = mbytes/reclen;
    sprintf(msg,"rmodrdc: irec=%d maxrec=%d\n",*irec,maxrec);
    return;
  }

 dptr  = data;
 for(j=0;j<*nrec;j++)
  { nr = fread((void *) wbuf,reclen,1,fptr);
    if(nr < 1)
     {sprintf(msg,"rmodrdc: read failed on record=%d\n",*irec+j);
      *stat=1; return;
     }
    *stat = wrdc_fcnvrt_(&typi,(char *)wbuf,&typo,(char *)dptr,n,msg);
    if(*stat != 0) { return; }
    dptr += dsize;
  }
 *stat = 0;
}

void rmodwrc_(int  *key,int  *irec, int  *nrec, int  *n, char *data,
     int  *wdtypo,int  *wdtypi,int  *stat, char *msg)
{/* wdtypo is the desired word type of the output file */
 /* wdtypi is the word type of the input data */
 int  bytoff,reclen,dsize,osize,j;
 int  typi,typo,nw,mbytes,maxrec;
 int ierr;
 char *dptr;
 char name[32],os[16];
 FILE *fptr;
 *stat = 1;
 if(*irec < 1 || *n < 1) return;
 typi = *wdtypi;
 typi  = netw_lnode(name,os);
 typo = *wdtypo;
 fptr = dbutil_getfileptr(key);
 if(fptr == 0)
  {sprintf(msg,"rmodwrc: NULL file pointer\n");
   return;
  }
 reclen= dbutil_getreclength_(key);
 if(reclen <= 0)
  {sprintf(msg,"rmodwrc: record length <=0 \n");
   return; }
 dsize  = 4*(*n);
 if(typi==WCRAY) dsize = 2*dsize;
 osize  = 4*(*n);
 if(typo==WCRAY) osize = 2*osize;
 if(osize > reclen)
  {sprintf(msg,"rmodwrc: n > record size\n");
   return;
  }

 if(reclen > bsiz)
  {if(wbuf != NULL ) free(wbuf);
   if(typo!=WCRAY)
    { wbuf = (char *) calloc(1,reclen); bsiz=reclen; }
   else
    { wbuf = (char *) calloc(1,2*reclen); bsiz = 2*reclen; }
  }
 if(wbuf == NULL)
  {sprintf(msg,"rmodwrc: error allocating buffer %d\n",bsiz);
   bsiz = 0;
   return;
  }

 bytoff = (*irec-1)* reclen;    /*reclen is the record size in bytes */
 ierr = fseek(fptr,bytoff,SEEK_SET);
 if(ierr != 0)
  { fseek(fptr,0,SEEK_END);
    mbytes = ftell(fptr);
    maxrec = mbytes/reclen;
    sprintf(msg,"rmodrdc: irec=%d maxrec=%d\n",*irec,maxrec);
    return;
  }

 dptr  = data;
 for(j=0;j<*nrec;j++)
  {*stat = wrdc_fcnvrt_(&typi,(char *)dptr,&typo,(char *)wbuf,n,msg);
   if(*stat != 0) { return; }
   nw = fwrite((void *) wbuf,reclen,1,fptr);
   if(nw < 1)
    {sprintf(msg,"rmodwrc: write failed on record=%d\n",*irec+j);
     *stat=1; return;
    }
   dptr += dsize;
  }
 *stat = 0;
}

int  rmod_canrm_(int  *key)
{/* Determine status of file when it is closed. ***
  * Get rid of files that were transferred over ***
  * the network.                                ***
  ************************************************/
 char status[16], localFile[96],*remoteFile;
 char file[80],node[32],user[16];
 int  can_remove,bld,ierr;
 can_remove = -1; /* default is can not remove */
 if(key == NULL) return can_remove;
 if(*key<= 0 ) return can_remove;
 remoteFile = dbutil_getremotefile(key);
 if(remoteFile == NULL) return can_remove;
 ierr = dbutil_getlocalfile_(key,localFile);
 if(ierr != 0) return can_remove;
 if(strcmp(localFile,remoteFile)==0) return can_remove;
 bld = 0;
 ierr = parse_netname_(remoteFile,node,user,file,&bld);
 if(ierr >0) return can_remove;
 if(strlen(node)==0 || strcmp(node,"NONE")==0) return can_remove;
 status[0]='\0';
 ierr = dbutil_getstatus_(key,status);
 if(strcmp(status,"OLD")==0 || strcmp(status,"SCRATCH")==0)
  {can_remove = 0;}
 return can_remove;
}



