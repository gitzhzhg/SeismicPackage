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
#include "cprim.h"
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
C  Primitive name:  FISIUTIL    (DINT/DOUT UTILITIES) 
C          Author:  Richard Day
C         Written:  94/08/08
C    Last revised:  96/07/17  Vunderink
C
C  Purpose:   Set of C routines used by the DINT and DOUT processes.
C-----------------------------------------------------------------------
C                              NOTES
C
C  1.  The names of all routines in ths primitive start with the
C      characters PARSE_.
C
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 1.  96/07/17 Vunderink  Inserted into the conlib library.
C---------------------------------------------------------------------------
C                         PROGRAMMING NOTES
C---------------------------------------------------------------------------
C
*/                  
long parse_netname_(char *c,char *n,char *u,char *f,long *bld);
/*
 * Break a file name into its path part and name part.
 * Return a pointer to the name part.
 * 
 */
char *parse_file_( char *file, char *name, char *path )
{
 char *loc1=NULL;
 char *ret=NULL;

 path[0]='\0';
 name[0]='\0';
 if(file == NULL || strlen(file)==0) { return ret; }

 loc1 = strrchr( file, ']' );
 if(!loc1) loc1 = strrchr( file, '>' );
 if(!loc1) loc1 = strrchr( file, '/' );
 if(!loc1) {
  loc1 = strstr( file, ";;" );
  if(loc1) loc1++;
 }
 if(!loc1) {
  loc1 = strstr( file, "::" );
  if(loc1) loc1++;
 }

 strcpy(name,file);
 if (loc1)
  { strcpy( name,++loc1);
    strncpy(path,file,loc1-file);
    path[loc1-file]='\0';
    ret= name;
  }
 return (ret);
}

/*
 * Break a file name into its root and extension.
 * name... Input name to break into parts.
 * root... Part of name up to but not including dot.
 * ext ... Extension of name not including the dot.
 * parse_ext returns pointer to location of the dot.
 */
char *parse_ext_(char *name,char *root, char *ext)
{char *dot;
 char *semic,*sep=NULL;
 ext[0]='\0';
 root[0]='\0';
 if(name) strcpy(root,name);
 if(name==NULL) return NULL;
 dot = strrchr(name,'.');
 if(dot)
  { sep = dot;
   ++dot;
    strcpy( ext,dot);
    strncpy(root,name,dot-name-1);
    root[dot-name-1]='\0';
    semic = strrchr(ext,';');
    if(semic)
     { semic[0]='\0';
     }
  }

return sep;
}

void parse_file_cppath_(char *hfile, char *file)
{ char hpath[80],hname[80],path[80],name[80];
/********************************************
 *--hfile.... a template file name.       ***
 *--file..... file name to expand.        ***
 *--Set the path for file= path for hfile,***
 *--unless it has an explicit path already***
 *******************************************/
 if(hfile==NULL || file==NULL) return;
 if( (strlen(file)==0) || file[0]==' ')
  { strcpy(file,hfile);
    /* strcpy(file,"NONE"); */
    return; }
 if(strlen(hfile)==0 ) return;
 if(strcmp(file,"SAME")==0)
  { strcpy(file,hfile);
    return; }

 parse_file_(hfile,hname,hpath);
 parse_file_( file, name, path);
 if(strlen(path) > 0) return; /* keep old path */
 else strcpy(path,hpath);    /* adopt headers path */

 if(strcmp(name,"NONE") == 0)
  { strcpy(file,name); return; }
 else
  {sprintf(file,"%s%s",path,name); return; }

}

/*
 * Decompose a string with the following syntax,
 * u= day n=pogun f=user3:[day.tfio]tfio.c
 * into its component parts.
 */
long parse_fnet_( char *netnam, char *node, char *user, char *file,
      char *msg)
{char *ret;
 char *n,*u,*f;
 long bld,ierr;
 if(msg  != NULL) msg[0]='\0';
 if(node != NULL) node[0]='\0';
 if(user != NULL) user[0]='\0';
 if(file != NULL) file[0]='\0';
 ret = NULL;
 n = NULL; u = NULL; f = NULL;
 if(netnam == NULL)
  {sprintf(msg,"parse_dnetnam: NULL input name\n");
   goto error; }
 bld = 0;
 ierr = parse_netname_(netnam,node,user,file,&bld);
/*
 n = NULL; u = NULL; f = NULL;
 i = dcdut_dcode ( (void**) &n, "n",'c',netnam);
 if(n != NULL) strcpy(node,n);
 i = dcdut_dcode ( (void**) &u, "u",'c',netnam);
 if(u != NULL) strcpy(user,u);
 i = dcdut_dcode ( (void**) &f, "f",'c',netnam);
 if(u==NULL && n==NULL && f==NULL) f=netnam;
 if(f != NULL) strcpy(file,f);
 ret=file;
*/

 return ierr;
 error:
 if(n!=NULL) free(n);
 if(u!=NULL) free(u);
 if(f!=NULL) free(f);
 return 1;
}
/*
 * Construct a network name form its component parts
 */
long parse_fbld_( char *netnam, char *node, char *user, char *file)
{
 char n[32],u[32],f[72];
 long ierr,bld;
 n[0]='\0'; u[0]='\0'; f[0]='\0';
 netnam[0]='\0';
 bld = 1;
 ierr = parse_netname_(netnam,node,user,file,&bld);
 return ierr;
}

long parse_netname_(char *c,char *n,char *u,char *f,long *bld)
{/* break the net name stored in c into seperate fields **
  * c = n::u;;f     bld=0 to parse, bld=1 to build name */
 char *loc,*loc1;
 if(c==NULL) goto error;
 if(n==NULL || u==NULL || f==NULL) goto error;

 if(*bld == 1)
  { if(strlen(n)==0 || strcmp(n,"NONE")==0)
     { sprintf(c,"%s",f);
       return 0;
     }
    if(strlen(u)==0)
     sprintf(c,"%s::NONE;;%s",n,f);
    else
     sprintf(c,"%s::%s;;%s",n,u,f);
    return 0;
  }

 n[0]='\0';
 u[0]='\0';
 f[0]='\0';
 if((loc=strstr(c,"::")) != NULL)
  {loc1 = c;
   strncpy(n,loc1,loc-loc1);
   n[loc-loc1]='\0';
   loc1 = loc+2;
   if((loc=strstr(c,";;")) != NULL)
    { strncpy(u,loc1,loc-loc1);
      u[loc-loc1]='\0';
      loc1 = loc+2;
    }
   else
    {strcpy(u,"NONE");
     /* goto error; r day Nov 22 95 */
    }
   strcpy(f,loc1);
   return 0;
  }
 else
  { strcpy(n,"NONE");
    strcpy(u,"NONE");
    strcpy(f,c);
  }
 return 0;
 error:
 return 1;
}
