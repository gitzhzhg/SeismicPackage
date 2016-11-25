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

#include "cprim.h"
#include "tfdefs.h"
#include "tf_global.h"
#include "put_global.h"

/*
 C\USER DOC
 C Name:     put_global_to_file
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Date:     10/06/96
 C Last revised:  99/03/08  R.S. Day
 C Purpose:  Given a complete TF_Global structure write the
 C           global info to the appropriate file.
 C
 C Prototype:
 C     int put_global_to_file(TF_Global *g)
 C        - write file header from info. contained in g.
 C
 C     int put_global_to_filet_(TF_Global *g,char *template)
 C        - write file header from info. contained in g.
 C        - if a SEGY file, copy ebcdic header from template
 C        - calls put_global_to_file if template is NONE
 C
 C     void put_global_bld_dfile_(int *ftyp,char *hfile,char *dfile)
 C        - set dfile from hfile and add an extension to dfile
 C          if it is != hfile.
 C
 C NOTES:
 C  1. Will handle VOXET,HGRID,HG3DL, & TFILE file types. Will
 C     return NULL if there is a failure.
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C99/03/08  R.S. Day    Now using dskio_x* routines
 C99/02/01  R.S. Day    Updating Conlib
 C98/11/30  R.S. Day    Added function put_global_to_filet_, and
 C                      put_global_to_file calls this internally.
 C98/10/01  R.S. Day    for 3D files the number of traces in the
 C                      cube is set to n2*n3 rather than to the actual
 C                      number of traces written.
 C96/10/22  Vunderink   Inserted into CONLIB.
 C96/10/06  R.S.Day     Original Version
 C\END DOC
 */


int put_global_to_file_(TF_Global *g)
{
 char template[8];
 strcpy(template,"NONE");
 return put_global_to_filet_(g, template);
}

int put_global_to_filet_(TF_Global *g, char *template)
{char *hfile=0,*dfile=0;
 int   fd,nby,opmode=4,i_err;
 long  off=0;
 int   ftyp,n1,n2,n3;
 char *str=0;
 ftyp = getgl_str_to_ftype(tf_global_ftype(g));
 if(ftyp==TF3D_TYPE || ftyp==TF3DF_TYPE) {
   tf_global_grid_sizes(g,&n1,&n2,&n3);
   tf_global_put_ntrfil ( n2*n3, g);
 }
 str = tf_global_to_char(g,&nby,template);
 if(!str) return 0;
 hfile = tf_global_header_file(g);
 dfile = tf_global_data_file(g);
 if(strcmp(dfile,hfile)==0 || strcmp(dfile,"SAME")==0)
  {
   /* header and data are in same file */
   dskio_xxskwr_(&g->lun,str, &off,&g->grecsiz,&i_err);
   /* dskiowr_(&g->lun,&off, &g->grecsiz,str);*/
  }
 else
  {
   /* header file is seperate from data file*/
   dskiocop_(&fd,hfile,&opmode);
   dskiowr_(&fd,&off,&nby,str);
   dskioccl_(&fd);
  }
 if(str) free(str);

 return 1;
}

void put_global_bld_dfile_(int *ftyp, char *hfile, char *dfile)
{char ext[8];
 int  i, ftype= *ftyp,stat;
 strcpy(dfile,hfile);
 i=dskio_ext_(hfile,ext);
 switch(ftype)
   {case VOXET_TYPE:
     strcpy(ext,"vodat");
     addext_rep_(dfile,ext,&stat);
     break;
    case BRICK_TYPE:
     strcpy(ext,"brdat");
     addext_rep_(dfile,ext,&stat);
     break;
   case HGRID_TYPE:
     strcpy(ext,"grid");
     addext_rep_(dfile,ext,&stat);
     break;
   case HG3DL_TYPE:
     strcpy(ext,"g3dl");
     addext_rep_(dfile,ext,&stat);
     break;
   case CBYTE_TYPE:
     strcpy(ext,"byt");
     addext_rep_(dfile,ext,&stat);
     break;
   case TFILE_TYPE:
     strcpy(ext,"tf");
     break;
   case TF3D_TYPE:
     strcpy(ext,"tf3");
     break;
   case TF3DF_TYPE:
     strcpy(ext,"tf3f");
     break;
   case SEGY_TYPE:
     strcpy(ext,"sgy");
     break;
   default:
     strcpy(dfile,hfile);
   }
 
}
