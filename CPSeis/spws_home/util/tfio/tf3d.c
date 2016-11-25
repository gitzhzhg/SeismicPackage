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
#include <stdlib.h>
#include "tf3d.h"
#include "tfio.h"
#include "transform.h"
#include "wrdcnvrt.h"
#include "dskio.h"

#define HDRSIZE 1024

/*
C23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                       C P S   P R I M I T I V E S
C 
C  Primitive name: tf3d_...
C         Library: CONLIB(conlib.a) and libtfio.a
C          Author: R.S. Day
C         Written: 96/12/10
C    Last revised: 99/04/29   Day
C
C Purpose:         Functions which manipulate the data carried by
C                  a Grid3DDesc structure. This data object is used
C                  for the description of certain 3D data cubes.
C                  Helps support the IO for data files of type VOXET,
C                  BRICK,HGRID,HG3Dl,TF3D, and TF3DF.
C
C                  The routines in this file are primitive routines
C                  which help support some SPWS applications as well as
C                  some CPS routines such as DINT and DOUT.
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C   Date       Author      Description
C -----------  --------    --------------------------------------------
C 5. 99/04/29  Day         parse_file_cppath replaced by dskio_cppath
C 4. 99/01/14  Day         Eliminated tf3d_cword_type, tf3d_iword_type
C 3. 97/08/18  Day         Added tf3d_print & tf3d_transform. Also
C                          added some more functions for access
C                          structure members.
C 2. 97/06/05  Day         Altered argument list for tf3d_bld_vox
C 1. 96/12/10  Day         Original Version
C-----------------------------------------------------------------------
C NOTES
C  Language: C
C Libraries: CONLIB(conlib.a) and libtfio.a         (placed into)
C  Includes: tf3d.h  tfio.h  wrdcnvrt.h  cprim.h
C
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -
C  Heap (dynamic)  - may call upon malloc & calloc for small memory 
C-----------------------------------------------------------------------
C
C\END DOC
*/

int   tf3d_getFileType(Grid3DDesc *h) { if(!h) return -1;
 return h->ftype;
}
char *tf3d_getHeaderFile(Grid3DDesc *h) { if(!h) return 0;
 return h->header_file; 
}
char *tf3d_getDataFile(Grid3DDesc *h) { if(!h) return 0;
 return h->P.file;
}
char *tf3d_getObjectName(Grid3DDesc *h) { if(!h) return 0;
 return h->name;
}
char *tf3d_getPropertyName(Grid3DDesc *h) { if(!h) return 0;
 return h->P.name;
}
Pt3Di *tf3d_getN(Grid3DDesc *h){ if(!h) return 0;
 return &h->N;
}
Pt3Df *tf3d_getO(Grid3DDesc *h){ if(!h) return 0;
 return &h->O;
}
Pt3Df *tf3d_getD(Grid3DDesc *h){ if(!h) return 0;
 return &h->D;
}
Pt3Df *tf3d_getU(Grid3DDesc *h){ if(!h) return 0;
 return &h->U;
}
Pt3Df *tf3d_getV(Grid3DDesc *h){ if(!h) return 0;
 return &h->V;
}
Pt3Df *tf3d_getW(Grid3DDesc *h){ if(!h) return 0;
 return &h->W;
}
Pt3Df *tf3d_getMin(Grid3DDesc *h){ if(!h) return 0;
 return &h->MI;
}
Pt3Df *tf3d_getMax(Grid3DDesc *h){ if(!h) return 0;
 return &h->MA;
}
Pt3Di *tf3d_getBrick(Grid3DDesc *h){ if(!h) return 0;
 return &h->brick;
}
Pt3Di *tf3d_getBlock(Grid3DDesc *h){ if(!h) return 0;
 return &h->blocks;
}
Pt3Di *tf3d_getHd(Grid3DDesc *h){ if(!h) return 0;
 return &h->hd;
}
Pt3Ds *tf3d_getAxisNames(Grid3DDesc *h){ if(!h) return 0;
 return &h->axis;
}
char *tf3d_getEtype(Grid3DDesc *h){ if(!h) return 0;
 return h->P.etype;
}
int   tf3d_getEsize(Grid3DDesc *h){ if(!h) return 0;
 return h->P.esize;
}

void tf3d_setFileType(Grid3DDesc *h, int type) {if(!h) return;
 h->ftype=type;
}
void tf3d_setHeaderFile(Grid3DDesc *h,char *name) {if(!h) return;
 if(!name) h->header_file[0]='\0';
 else strcpy(h->header_file,name);
}
void tf3d_setDataFile(Grid3DDesc *h,char *name) {if(!h) return;
 if(!name) h->P.file[0]='\0';
 else strcpy(h->P.file,name);
}
void tf3d_setObjectName(Grid3DDesc *h,char *name) {if(!h) return;
 if(!name) h->name[0]='\0';
 else strcpy(h->name,name);
}
void tf3d_setPropertyName(Grid3DDesc *h,char *name) {if(!h) return;
 if(!name) h->P.name[0]='\0';
 else strcpy(h->P.name,name);
}
void tf3d_setN(Grid3DDesc *h, Pt3Di *p3) {if(!h) return;
 memcpy(&h->N,p3,sizeof(Pt3Di));
}
void tf3d_setO(Grid3DDesc *h, Pt3Df *p3) {if(!h) return;
 memcpy(&h->O,p3,sizeof(Pt3Df));
}
void tf3d_setD(Grid3DDesc *h, Pt3Df *p3) {if(!h) return;
 memcpy(&h->D,p3,sizeof(Pt3Df));
}
void tf3d_setU(Grid3DDesc *h, Pt3Df *p3) {if(!h) return;
 memcpy(&h->U,p3,sizeof(Pt3Df));
}
void tf3d_setV(Grid3DDesc *h, Pt3Df *p3) {if(!h) return;
 memcpy(&h->V,p3,sizeof(Pt3Df));
}
void tf3d_setW(Grid3DDesc *h, Pt3Df *p3) {if(!h) return;
 memcpy(&h->W,p3,sizeof(Pt3Df));
}
void tf3d_setMin(Grid3DDesc *h, Pt3Df *p3) {if(!h) return;
 memcpy(&h->MI,p3,sizeof(Pt3Df));
}
void tf3d_setMax(Grid3DDesc *h, Pt3Df *p3) {if(!h) return;
 memcpy(&h->MA,p3,sizeof(Pt3Df));
}
void tf3d_setBrick(Grid3DDesc *h, Pt3Di *p3) {if(!h) return;
 memcpy(&h->brick,p3,sizeof(Pt3Df));
}
void tf3d_setBlock(Grid3DDesc *h, Pt3Di *p3) {if(!h) return;
 memcpy(&h->blocks,p3,sizeof(Pt3Df));
}
void tf3d_setHd(Grid3DDesc *h,Pt3Di *hd){ if(!h) return;
 memcpy(&h->hd,hd,sizeof(Pt3Di));
}
void tf3d_setAxisNames(Grid3DDesc *h, Pt3Ds *p3) {if(!h) return;
 memcpy(&h->axis,p3,sizeof(Pt3Ds));
}
void tf3d_setEtype(Grid3DDesc *h, char *type) {if(!h) return;
 strcpy(h->P.etype,type);
}
void tf3d_setEsize(Grid3DDesc *h, int size) {if(!h) return;
 h->P.esize=size;
}

/*
 C\USER DOC
 C Name:     tf3d_rmod_init
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Initialize the keyword strings in preparation
 C           for reading or writing rmod grid files.
 C
 C Prototype:
 C     void tf3d_rmod_init(Grid3DDesc *h);
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C
 C\END DOC
 */
void tf3d_rmod_init(Grid3DDesc *h)
{int i=0;
 if(!h) return;
 h->ftype=UNKWN_TYPE;
 strcpy(h->keys[i],"NZ"); h->fmt[i]='i'; i++;
 strcpy(h->keys[i],"NX"); h->fmt[i]='i'; i++;
 strcpy(h->keys[i],"NY"); h->fmt[i]='i'; i++;
 strcpy(h->keys[i],"ZMIN"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"XMIN"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"YMIN"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"ZINC"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"XINC"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"YINC"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"ZMODMIN"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"XMODMIN"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"YMODMIN"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"ZMODMAX"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"XMODMAX"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"YMODMAX"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"ZCOORDINATE"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"XCOORDINATE"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"YCOORDINATE"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"ZDATUM"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"FILE"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"TRANSFORM"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"WORDTYPE"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"TYPE"); h->fmt[i]='c'; i++;
 h->nkey= i;
 h->hd.v[0]=UNDEFHDR;
 h->hd.v[1]=UNDEFHDR;
 h->hd.v[2]=UNDEFHDR;
 strcpy(h->type.v[0],"even");
 strcpy(h->type.v[1],"even");
 strcpy(h->type.v[2],"even");
 strcpy(h->axis.v[0],"axis-1");
 strcpy(h->axis.v[1],"axis-2");
 strcpy(h->axis.v[2],"axis-3");

 h->np = 0;
 h->P.esize=4;
 strcpy(h->P.name,"UNKNOWN");
 strcpy(h->P.file,"NONE");
 strcpy(h->P.etype,"WIEEE");
 strcpy(h->P.keys[0],"PROPERTY");
 strcpy(h->P.keys[1],"FILE");
 strcpy(h->P.keys[2],"PROP_ESIZE");
 strcpy(h->P.keys[3],"WORDTYPE");
 h->P.nkey =4;
}

/*
 C\USER DOC
 C Name:     tf3d_govo_init
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Initialize the keyword strings in preparation
 C           for reading or writing Gocad grid files.
 C
 C Prototype:
 C     void tf3d_govo_init(Grid3DDesc *h);
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C
 C\END DOC
 */
void tf3d_govo_init(Grid3DDesc *h)
{int i=0;
 if(!h) return;
 h->ftype=UNKWN_TYPE;
 strcpy(h->keys[i],"AXIS_O"); i++;
 strcpy(h->keys[i],"AXIS_U"); i++;
 strcpy(h->keys[i],"AXIS_V"); i++;
 strcpy(h->keys[i],"AXIS_W"); i++;
 strcpy(h->keys[i],"AXIS_N"); i++;
 strcpy(h->keys[i],"AXIS_MIN"); i++;
 strcpy(h->keys[i],"AXIS_MAX"); i++;
 strcpy(h->keys[i],"AXIS_NAME"); i++;
 strcpy(h->keys[i],"AXIS_TYPE"); i++;
 strcpy(h->keys[i],"AXIS_D"); i++;
 strcpy(h->keys[i],"AXIS_BLOCKS"); i++;
 strcpy(h->keys[i],"AXIS_BRICK"); i++;
 h->N.v[0]=1;
 h->N.v[1]=1;
 h->N.v[2]=1;
 h->MI.v[0]=0.0;
 h->MI.v[1]=0.0;
 h->MI.v[2]=0.0;
 h->MA.v[0]=1.0;
 h->MA.v[1]=1.0;
 h->MA.v[2]=1.0;
 strcpy(h->type.v[0],"even");
 strcpy(h->type.v[1],"even");
 strcpy(h->type.v[2],"even");
 strcpy(h->axis.v[0],"axis-1");
 strcpy(h->axis.v[1],"axis-2");
 strcpy(h->axis.v[2],"axis-3");
 h->U.v[0]=0.0;
 h->U.v[1]=0.0;
 h->U.v[2]=1.0;
 h->V.v[0]=1.0;
 h->V.v[1]=0.0;
 h->V.v[2]=0.0;
 h->W.v[0]=0.0;
 h->W.v[1]=1.0;
 h->W.v[2]=0.0;

 h->nkey= i;
 h->hd.v[0]=UNDEFHDR;
 h->hd.v[1]=UNDEFHDR;
 h->hd.v[2]=UNDEFHDR;
 h->np = 0;
 h->P.esize=4;
 strcpy(h->P.name,"UNKNOWN");
 strcpy(h->P.file,"NONE");
 strcpy(h->P.etype,"WIEEE");
 strcpy(h->P.keys[0],"PROPERTY");
 strcpy(h->P.keys[1],"PROP_FILE");
 strcpy(h->P.keys[2],"PROP_ESIZE");
 strcpy(h->P.keys[3],"PROP_ETYPE");
 h->P.nkey =4;
}

/*
 C\USER DOC
 C Name:     tf3d_tfile_init
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Initialize the keyword strings in preparation
 C           for reading or writing tfile files.
 C
 C Prototype:
 C     void tf3d_tfile_init(Grid3DDesc *h);
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C
 C\END DOC
 */
void tf3d_tfile_init(Grid3DDesc *h)
{int i=0;
 if(!h) return;
 h->ftype=UNKWN_TYPE;
 strcpy(h->keys[i],"N1"); h->fmt[i]='i'; i++;
 strcpy(h->keys[i],"N2"); h->fmt[i]='i'; i++;
 strcpy(h->keys[i],"N3"); h->fmt[i]='i'; i++;
 strcpy(h->keys[i],"O1"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"O2"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"O3"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"D1"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"D2"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"D3"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"AXIS1"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"AXIS2"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"AXIS3"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"FILE"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"PROPERTY"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"WORDTYPE"); h->fmt[i]='c'; i++;
 strcpy(h->keys[i],"AXIS_U"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"AXIS_V"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"AXIS_W"); h->fmt[i]='f'; i++;
 strcpy(h->keys[i],"HD1"); h->fmt[i]='i'; i++;
 strcpy(h->keys[i],"HD2"); h->fmt[i]='i'; i++;
 strcpy(h->keys[i],"HD3"); h->fmt[i]='i'; i++;
 h->nkey= i;
 h->hd.v[0]=UNDEFHDR;
 h->hd.v[1]=UNDEFHDR;
 h->hd.v[2]=UNDEFHDR;
 strcpy(h->type.v[0],"even");
 strcpy(h->type.v[1],"even");
 strcpy(h->type.v[2],"even");
 strcpy(h->axis.v[0],"axis-1");
 strcpy(h->axis.v[1],"axis-2");
 strcpy(h->axis.v[2],"axis-3");
 h->U.v[0]=0.0;
 h->U.v[1]=0.0;
 h->U.v[2]=0.0;
 h->V.v[0]=0.0;
 h->V.v[1]=0.0;
 h->V.v[2]=0.0;
 h->W.v[0]=0.0;
 h->W.v[1]=0.0;
 h->W.v[2]=0.0;

 h->np = 0;
 h->P.esize = 4;
 strcpy(h->P.name,"UNKNOWN");
 strcpy(h->P.file,"NONE");
 strcpy(h->P.etype,"WIEEE");
 strcpy(h->P.keys[0],"PROPERTY");
 strcpy(h->P.keys[1],"FILE");
 strcpy(h->P.keys[2],"PROP_ESIZE");
 strcpy(h->P.keys[3],"WORDTYPE"); /* redundant */
 h->P.nkey =4;
}

/*
 C\USER DOC
 C Name:     tf3d_bld_vox
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Build the string for the ascii header part of a
 C           Gocad VOXET or BRICK file
 C
 C Prototype:
 C     char *tf3d_bld_vox(Grid3DDesc *h,int *nby,
 C                        int X, int Y, int Z)
 C
 C    X,Y,Z are to indicate the relative storage order of
 C    the axis. For Example, X=2,Y=3, & Z=1 indicates that
 C    the samples in the cube are stored in the order Z-X-Y
 C    (From fastest to slowest)
 C NOTES:
 C  1. Will handle VOXET file types.
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C 97/06/05 R.S.Day     Added X,Y,Z arguments
 C 96/12/10 R.S.Day     Original Version
 C\END DOC
 */
char *tf3d_bld_vox(Grid3DDesc *h,int *nby, int X, int Y, int Z)
{char    *out=0;
 char     card[80];

 *nby=0;
 if(!h ) return 0;
 if( h->ftype!=VOXET_TYPE && h->ftype!=BRICK_TYPE) return 0;

 out = (char *) malloc(HDRSIZE+1);
 *nby = HDRSIZE+1;
 out[0]='\0';
 if(h->ftype==VOXET_TYPE) {
  sprintf(card,"GOCAD VOXET 1.0\n HEADER {\n");
 } else {
  sprintf(card,"GOCAD BRICK 1.0\n HEADER {\n");
 }
 strcat(out,card);
 sprintf(card," name:%s\n",h->name);
 strcat(out,card);
 sprintf(card," *axis:on\n *grid:off\n }\n");
 strcat(out,card);
 sprintf(card,"AXIS_O %f %f %f\n",h->O.v[X-1],h->O.v[Y-1],h->O.v[Z-1]);
 strcat(out,card);
 sprintf(card,"AXIS_U %f %f %f\n",h->U.v[0],h->U.v[1],h->U.v[2]);
 strcat(out,card);
 sprintf(card,"AXIS_V %f %f %f\n",h->V.v[0],h->V.v[1],h->V.v[2]);
 strcat(out,card);
 sprintf(card,"AXIS_W %f %f %f\n",h->W.v[0],h->W.v[1],h->W.v[2]);
 strcat(out,card);
 sprintf(card,"AXIS_N %d %d %d\n",h->N.v[0],h->N.v[1],h->N.v[2]);
 strcat(out,card);
 sprintf(card,"AXIS_MIN %f %f %f\n",h->MI.v[0],h->MI.v[1],h->MI.v[2]);
 strcat(out,card);
 sprintf(card,"AXIS_MAX %f %f %f\n",h->MA.v[0],h->MA.v[1],h->MA.v[2]);
 strcat(out,card);
 sprintf(card,"AXIS_NAME \"%s\" \"%s\" \"%s\"\n",
      h->axis.v[0], h->axis.v[1], h->axis.v[2]); 
 strcat(out,card);
 sprintf(card,"AXIS_TYPE %s %s %s\n\n",
      h->type.v[0],h->type.v[1],h->type.v[2]);
 strcat(out,card);
 if(h->ftype==BRICK_TYPE) {
   sprintf(card,"AXIS_BLOCKS %d %d %d\nAXIS_BRICK %d %d %d\n",
   h->blocks.v[0],h->blocks.v[1],h->blocks.v[2],
   h->brick.v[0],h->brick.v[1],h->brick.v[2]);
   strcat(out,card);
 }


 sprintf(card,"%s  1 \"%s\"\n",h->P.keys[0],h->P.name);
 strcat(out,card);
 sprintf(card,"%s  1 %s\n",h->P.keys[1],h->P.file);
 strcat(out,card);
 sprintf(card,"%s  1 %s\n",h->P.keys[3],h->P.etype);
 strcat(out,card);
 sprintf(card,"%s  1 %d\nEND\n",h->P.keys[2],h->P.esize);
 strcat(out,card);

 return out;
}

/*
 C\USER DOC
 C Name:     tf3d_bld_hg
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Build the string for an RMOD HGRID type file
 C           (The *HEADER section of the ascii file)
 C Prototype:
 C     char *tf3d_bld_hg(Grid3DDesc *h,int *nby)
 C
 C NOTES:
 C  1. Will handle HGRID or HG3DL file types. String will not
 C     include any transform information. NULL returned for failure.
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C          R.S.Day     Original Version
 C\END DOC
 */
char *tf3d_bld_hg(Grid3DDesc *h,int *nby)
{char    *out=0;
 char     card[80];
 int      X=1,Y=2,Z=0;

 *nby=0;
 if(!h) return 0;
 if( h->ftype!=HGRID_TYPE && h->ftype!=HG3DL_TYPE) return 0;
 if(h->ftype==HG3DL_TYPE)
   { X=0; Y=1; Z=2;}

 out = (char *) malloc(HDRSIZE+1);
 out[0]='\0';
 if(out) *nby = HDRSIZE+1;
 /* see get_global.c for keys definitions */
 sprintf(card," *HEADER SECTION\n");
 strcat(out,card);
 sprintf(card," %s =%s\n",h->keys[19],h->P.file);
 strcat(out,card);
 if(h->tran_file) sprintf(card," %s =%s\n",h->keys[20],h->tran_file);
 else sprintf(card," %s =%s\n",h->keys[20],"NONE");
 strcat(out,card);
 sprintf(card," TYPE=%s\n",getgl_ftype_to_str(h->ftype)); 
 strcat(out,card);
 sprintf(card," %s =%s\n",h->keys[21],h->P.etype);
 strcat(out,card);
 sprintf(card," layered model limits\n"); 
 strcat(out,card);
 sprintf(card," %s= %10g %s= %10g  %s= %s\n", h->keys[9],h->MI.v[Z],
  h->keys[12],h->MA.v[Z], h->keys[15], h->axis.v[Z]);
 strcat(out,card);
 sprintf(card," %s= %10g %s= %10g  %s= %s\n", h->keys[10],h->MI.v[X],
  h->keys[13],h->MA.v[X], h->keys[16], h->axis.v[X]);
 strcat(out,card);
 sprintf(card," %s= %10g %s= %10g  %s= %s\n", h->keys[11],h->MI.v[Y],
  h->keys[14],h->MA.v[Y], h->keys[17], h->axis.v[Y]);
 strcat(out,card);
 sprintf(card," gridded model limits\n"); 
 strcat(out,card);
 sprintf(card," %s= %10d %s= %10g %s= %10g\n", h->keys[0],h->N.v[Z],
  h->keys[3],h->O.v[Z], h->keys[6],h->D.v[Z]);
 strcat(out,card);
 sprintf(card," %s= %10d %s= %10g %s= %10g\n", h->keys[1],h->N.v[X],
  h->keys[4],h->O.v[X], h->keys[7],h->D.v[X]);
 strcat(out,card);
 sprintf(card," %s= %10d %s= %10g %s= %10g\n", h->keys[2],h->N.v[Y],
  h->keys[5],h->O.v[Y], h->keys[8],h->D.v[Y]);
 strcat(out,card);
 sprintf(card," zdatum level\n"); 
 strcat(out,card);
 sprintf(card," %s= %g\n\n",h->keys[18],h->zdatum); 
 strcat(out,card);
 return out;
}

/*
 C\USER DOC
 C Name:     tf3d_setuvw
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Given the Grid3DDesc structure set the paramters
 C           that describe the 1-2-3 axis orientation in X,Y,Z
 C           space. 
 C
 C Prototype:
 C     void tf3d_setuvw(Grid3DDesc *h, int X, int Y, int Z)
 C
 C NOTES:
 C  1. X,Y,Z indicate which axis(1,2 or 3) are the X,Y and
 C     Z axis. For example X=1,Y=2, Z=3 implies that the fast
 C     axis is in the X direction, the 2nd fastest axis is in
 C     the Y direction, and the slowest axis is along Z.
 C  2. The TF_Global structure should have the grid size
 C     and grid increment set before this call. 
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C          R.S.Day     Original Version
 C\END DOC
 */
void tf3d_setuvw(Grid3DDesc *h, int X, int Y, int Z)
{int        nn;
 float      al[3]; /* for axis lengths */

  if(!h) return;

/* Assumes h->N.v and h->D.v have been set beforehand */
  nn = (h->N.v[0]>1) ? h->N.v[0]-1 : 1;
  al[0] = nn*h->D.v[0];
  nn = (h->N.v[1]>1) ? h->N.v[1]-1 : 1;
  al[1] = nn*h->D.v[1];
  nn = (h->N.v[2]>1) ? h->N.v[2]-1 : 1;
  al[2] = nn*h->D.v[2];
  h->U.v[0]=0.; h->U.v[1]=0.; h->U.v[2]=0.;
  h->V.v[0]=0.; h->V.v[1]=0.; h->V.v[2]=0.;
  h->W.v[0]=0.; h->W.v[1]=0.; h->W.v[2]=0.;
  switch(X) {/* set the nonzero components */
     case 1:
      h->U.v[0]  = al[X-1];
      if(Y==2) {
       h->V.v[1]  = al[Y-1];
       h->W.v[2]  = al[Z-1];
      }
      else {
       h->V.v[2]  = al[Z-1];
       h->W.v[1]  = al[Y-1];
      }
      break;
     case 2:
      h->V.v[0]  = al[X-1];
      if(Z==1) {
       h->W.v[1]  = al[Y-1];
       h->U.v[2]  = al[Z-1];
      }
      else {
       h->W.v[2]  = al[Z-1];
       h->U.v[1]  = al[Y-1];
      }
      break;
     case 3:
      h->W.v[0]  = al[X-1];
      if(Z==1) {
       h->V.v[1]  = al[Y-1];
       h->U.v[2]  = al[Z-1];
      }
      else {
       h->V.v[2]  = al[Z-1];
       h->U.v[1]  = al[Y-1];
      }
  }

}

 /*
 C\USER DOC
 C Name:     tf3d_create_desc
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Given individual grid parameters, create a
 C           Grid3DDesc structure.
 C
 C Prototype:
 C Grid3DDesc *tf3d_create_desc( 
 C  char *hfile, char *dfile,
 C  char *pname, char *obj_name,
 C  float o1, float o2, float o3,
 C  int   n1, int   n2, int   n3,
 C  float d1, float d2, float d3, 
 C  char *lab1,char *lab2,char *lab3,
 C  int X, int Y, int Z,
 C  int ftype, int  wdtype);
 C
 C     hfile ... header file name(can be same as dfile)
 C     dfile ... name of the file where binary data is kept 
 C     pname ... optional property name for the binary data
 C     oname ... optional object name for this data set..
 C     oi    ... Origin along axis-i, the fast axis.i=1,2,3
 C     di    ... grid increment along axis-i, the fast axis.
 C     ni    ... grid size along axis-i, the fast axis.
 C     labi  ... label for axis-i, the fast axis.
 C     X     ... labels axis 1,2, or 3 as the X axis
 C     Y     ... labels axis 1,2, or 3 as the Y axis
 C     Z     ... labels axis 1,2, or 3 as the Z axis
 C     ftype ... defined file types. (see tfio.h)
 C     wdtype... f.p. data type (see wrdcnvrt.h)
 C               Controls grid data only. Header words will
 C               still be float, even if data is byte.
 C NOTES:
 C  1. Will handle VOXET,HGRID,TFILEs,HG3DL file types. Will return
 C     0 if there is a failure.
 C  2. Axis 2 and 3 are the medium and slowest storage axis.
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C          R.S.Day     Original Version
 C\END DOC
 */
Grid3DDesc *tf3d_create_desc(char *hfile, char *dfile,
    char *pname, char *obj_name,
    float o1, float o2, float o3,
    int   n1, int   n2, int   n3,
    float d1, float d2, float d3, 
    char *lab1,char *lab2,char *lab3,
    int X, int Y, int Z,
    int ftype, int  wdtype) {
 Grid3DDesc *h=0;
 int dsize=1;

 if(n1<1) n1=1;
 if(n2<1) n2=1;
 if(n3<1) n3=1;
 if(d1==0.) d1=1.0;
 if(d2==0.) d2=1.0;
 if(d3==0.) d3=1.0;
 if(wdtype==WIEEE || wdtype==WIBM || wdtype==WVMS) dsize=4;
 if(wdtype==WIBM2) dsize=2;
 if(wdtype==WCRAY) dsize=8;
 h = (Grid3DDesc *) calloc(1,sizeof(Grid3DDesc));
 switch(ftype)
  { 
   case TF3D_TYPE:
   case TF3DF_TYPE:
   case SEGY_TYPE:
   case TROT_TYPE:
    tf3d_tfile_init(h);
    break;

   case HGRID_TYPE:
   case HG3DL_TYPE:
   case LAYER_TYPE:
    tf3d_rmod_init(h);
    break;

   case BRICK_TYPE:
   case VOXET_TYPE:
    tf3d_govo_init((Grid3DDesc *) h);
    h->blocks.v[0]=1;   /* initialized for VOXET only */
    h->blocks.v[1]=n2;
    h->blocks.v[2]=n3;
    h->brick.v[0]=n1;
    h->brick.v[1]=1;
    h->brick.v[2]=1;
    break;

   default:
    if(h) free(h);
    return 0;

  }
 strcpy(h->header_file,hfile);
 strcpy(h->name,obj_name);
 strcpy(h->P.name,pname);
 strcpy(h->P.file,dfile);
 strcpy(h->P.etype,wrdc_cword_type(wdtype));
 h->P.esize = dsize;
 h->ftype  = ftype;
 h->N.v[0] = n1;
 h->N.v[1] = n2;
 h->N.v[2] = n3;
 h->O.v[0] = o1;
 h->O.v[1] = o2;
 h->O.v[2] = o3;
 h->D.v[0] = d1;
 h->D.v[1] = d2;
 h->D.v[2] = d3;
 h->MI.v[0] =h->O.v[0]; 
 h->MI.v[1] =h->O.v[1]; 
 h->MI.v[2] =h->O.v[2]; 
 h->MA.v[0] =h->O.v[0] + (h->N.v[0]-1)*h->D.v[0]; 
 h->MA.v[1] =h->O.v[1] + (h->N.v[1]-1)*h->D.v[1]; 
 h->MA.v[2] =h->O.v[2] + (h->N.v[2]-1)*h->D.v[2]; 
 if(ftype==BRICK_TYPE || ftype==VOXET_TYPE) {
    h->MI.v[0] =0.0; 
    h->MI.v[1] =0.0; 
    h->MI.v[2] =0.0; 
    h->MA.v[0] =1.0; 
    h->MA.v[1] =1.0; 
    h->MA.v[2] =1.0; 
 }
 if(lab1) strcpy(h->axis.v[0],lab1);
 if(lab2) strcpy(h->axis.v[1],lab2);
 if(lab3) strcpy(h->axis.v[2],lab3);
 h->hd.v[0]=UNDEFHDR;
 h->hd.v[1]=UNDEFHDR;
 h->hd.v[2]=UNDEFHDR;
/*
 ErsTransHeader(&h->hd.v[0],lab1);
 ErsTransHeader(&h->hd.v[1],lab2);
 ErsTransHeader(&h->hd.v[2],lab3);
 */
 tf3d_setuvw( h, X, Y, Z);

 return h;
}

/*
 C\USER DOC
 C Name:     tf3d_create_desc_from_gostr
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Read a BRICK or VOXET header file
 C           These files are in the keyword-value(s) style.
 C
 C Prototype:
 C     Grid3DDesc *tf3d_create_desc_from_gostr(char *hfile,
 C     int ftype, char *in)
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C
 C\END DOC
 * If file is 0 the info. is echoed to stdout.
 */
Grid3DDesc *tf3d_create_desc_from_gostr(char *hfile,int ftype, char *in)
{char *targ=0;
 int   X=0,Y=1,Z=2;
 int   j,n1=1,n2=1,n3=1;
 int   id;
 Grid3DDesc *h=0;

/* Check the file type */
 if(ftype != BRICK_TYPE && ftype != VOXET_TYPE) return h;
 h = (Grid3DDesc *) calloc(1,sizeof(Grid3DDesc));
 if(!h) return h;
 targ = strstr(in,"GOCAD");
 if(targ) {
  targ = strstr(targ,"name:");
  if(targ) sscanf(targ+5,"%s",h->name);
 }

/* Initialize the keys */
 tf3d_govo_init( h);
 strcpy(h->header_file,hfile);
 h->ftype = ftype;

/* Scan the input string for parameter settings */
   j=0;
   while(j<h->nkey)
   {if( (targ=strstr(in,h->keys[j])) )
     {
         switch(j) {
         case 0:
          sscanf(targ+strlen(h->keys[j]),"%g%g%g",
           &h->O.v[X],&h->O.v[Y], &h->O.v[Z]);
          break;
         case 1:
          sscanf(targ+strlen(h->keys[j]),"%g%g%g",
           &h->U.v[X],&h->U.v[Y], &h->U.v[Z]);
          break;
         case 2:
          sscanf(targ+strlen(h->keys[j]),"%g%g%g",
           &h->V.v[X],&h->V.v[Y], &h->V.v[Z]);
          break;
         case 3:
          sscanf(targ+strlen(h->keys[j]),"%g%g%g",
           &h->W.v[X],&h->W.v[Y], &h->W.v[Z]);
          break;
         case 4:
          sscanf(targ+strlen(h->keys[j]),"%d%d%d",
           &h->N.v[0],&h->N.v[1], &h->N.v[2]);
           n1 = h->N.v[0];
           n2 = h->N.v[1];
           n3 = h->N.v[2];
          break;
         case 5:
          sscanf(targ+strlen(h->keys[j]),"%g%g%g",
           &h->MI.v[0],&h->MI.v[1], &h->MI.v[2]);
          break;
         case 6:
          sscanf(targ+strlen(h->keys[j]),"%g%g%g",
           &h->MA.v[0],&h->MA.v[1], &h->MA.v[2]);
          break;
         case 10:
          sscanf(targ+strlen(h->keys[j]),"%d%d%d",
           &h->blocks.v[0],&h->blocks.v[1], &h->blocks.v[2]);
          break;
         case 11:
          sscanf(targ+strlen(h->keys[j]),"%d%d%d",
           &h->brick.v[0],&h->brick.v[1], &h->brick.v[2]);
          break;
         case 7:
          sscanf(targ+strlen(h->keys[j]),"%s%s%s",
           h->axis.v[0],h->axis.v[1], h->axis.v[2]);
          break;
         case 8:
          sscanf(targ+strlen(h->keys[j]),"%s%s%s",
           h->type.v[0],h->type.v[1], h->type.v[2]);
          break;
         case 9:
          sscanf(targ+strlen(h->keys[j]),"%d%d%d",
           &h->D.v[0],&h->D.v[1], &h->D.v[2]);
          break;
         }
     }
    j++;
   }

   if(h->D.v[0]==0.0) {n1 = (n1>1) ? n1-1 : 1;
    h->D.v[0] =  sqrt( (h->U.v[0]*h->U.v[0] + h->U.v[1]*h->U.v[1] +
    h->U.v[2]*h->U.v[2] ))/n1;
   }

   if(h->D.v[1]==0.0) {n2 = (n2>1) ? n2-1 : 1;
    h->D.v[1] =  sqrt( (h->V.v[0]*h->V.v[0] + h->V.v[1]*h->V.v[1] +
    h->V.v[2]*h->V.v[2] ))/n2;
   }
   if(h->D.v[2]==0.0) {n3 = (n3>1) ? n3-1 : 1;
    h->D.v[2] =  sqrt( (h->W.v[0]*h->W.v[0] + h->W.v[1]*h->W.v[1] +
    h->W.v[2]*h->W.v[2] ))/n3;
   }
   j=0;
   while(j<h->P.nkey)
    {if( (targ=strstr(in,h->P.keys[j])) )
      {
         switch(j) {
         case 0:
          sscanf(targ+strlen(h->P.keys[j]),"%d%s",&id, h->P.name);
          h->P.id = id;
          break;
         case 1:
          sscanf(targ+strlen(h->P.keys[j]),"%d%s",&id, h->P.file);
          dskio_cppath(hfile,h->P.file);
          break;
         case 2:
          sscanf(targ+strlen(h->P.keys[j]),"%d%d",&id,&h->P.esize);
          break;
         case 3:
          sscanf(targ+strlen(h->P.keys[j]),"%d%s",&id, h->P.etype);
          break;
         }

      }
     j++;
    }
 return h;
}

/*
char *tf3d_cword_type(int wdtype)
{
 return wrdc_cword_type(wdtype);
}

int tf3d_iword_type(char *word_string)
{
 return wrdc_iword_type(word_string);
}
*/

/*
 * Perform a linear coordinate transformation(LCT) on h.
 * TR = ErsTransforms data(contains LCT definitions)
 * h  = Grid3DDesc data(the data to transform)
 * ax = output coordinate name for axis X.
 * ay = output coordinate name for axis Y.
 * az = output coordinate name for axis Z.
 * return 0/1 if transformation attempt fails/succeeds.
 */
int  tf3d_transform(char *ax, char *ay, char *az, void *TR, Grid3DDesc *h) {
 ErsTransforms *T=(ErsTransforms *) TR;
 ErsTransform *ti, *to;
 int i,X= -1,Y= -1,Z= -1;
 Pt3Ds *lab;
 lab = tf3d_getAxisNames(h);
 for(i=0;i<3;i++) {
   if(lab->v[i][0]=='X') X=i;
   if(lab->v[i][0]=='Y') Y=i;
   if(lab->v[i][0]=='T' || lab->v[i][0]=='D') Z=i;
   if(lab->v[i][0]=='M' || lab->v[i][0]=='K') Z=i;
   if(lab->v[i][0]=='F' || lab->v[i][0]=='S') Z=i;
 }
 if(X== -1 || Y== -1 || Z== -1) return 0;
 to = ErsTransGetTran(T,ax);
 ti = ErsTransGetTran(T,lab->v[X]);
 h->O.v[X] = transform_cnvrt(ti,h->O.v[X],to);
 h->U.v[0] = transform_cnvrt(ti,h->U.v[0],to);
 h->V.v[0] = transform_cnvrt(ti,h->V.v[0],to);
 h->W.v[0] = transform_cnvrt(ti,h->W.v[0],to);
 h->D.v[X] = transform_scale(ti,h->D.v[0],to);
 ErsTransHeader(&h->hd.v[X],ax);
 strcpy(h->axis.v[X],ax);
 to = ErsTransGetTran(T,ay);
 ti = ErsTransGetTran(T,lab->v[Y]);
 h->O.v[Y] = transform_cnvrt(ti,h->O.v[Y],to);
 h->U.v[1] = transform_cnvrt(ti,h->U.v[1],to);
 h->V.v[1] = transform_cnvrt(ti,h->V.v[1],to);
 h->W.v[1] = transform_cnvrt(ti,h->W.v[1],to);
 h->D.v[Y] = transform_scale(ti,h->D.v[Y],to);
 ErsTransHeader(&h->hd.v[Y],ay);
 strcpy(h->axis.v[Y],ay);
 to = ErsTransGetTran(T,az);
 ti = ErsTransGetTran(T,lab->v[Z]);
 h->zdatum = transform_cnvrt(ti,h->zdatum,to);
 h->O.v[Z] = transform_cnvrt(ti,h->O.v[Z],to);
 h->U.v[2] = transform_cnvrt(ti,h->U.v[2],to);
 h->V.v[2] = transform_cnvrt(ti,h->V.v[2],to);
 h->W.v[2] = transform_cnvrt(ti,h->W.v[2],to);
 h->D.v[Z] = transform_scale(ti,h->D.v[Z],to);
 ErsTransHeader(&h->hd.v[Z],az);
 strcpy(h->axis.v[Z],az);

 return 1;
}

void tf3d_print(Grid3DDesc *h) {
 if(!h) return;
 printf("name %s\n",h->name);
 printf("file %s\n",h->header_file);
 printf("type %5d\n",h->ftype);
 printf("N    %5d %5d %5d\n",h->N.v[0],h->N.v[1],h->N.v[2]);
 printf("O    %10f %10f %10f\n",h->O.v[0],h->O.v[1],h->O.v[2]);
 printf("D    %10f %10f %10f\n",h->D.v[0],h->D.v[1],h->D.v[2]);
 printf("U    %10f %10f %10f\n",h->U.v[0],h->U.v[1],h->U.v[2]);
 printf("V    %10f %10f %10f\n",h->V.v[0],h->V.v[1],h->V.v[2]);
 printf("W    %10f %10f %10f\n",h->W.v[0],h->W.v[1],h->W.v[2]);
 printf("MIN  %10f %10f %10f\n",h->MI.v[0],h->MI.v[1],h->MI.v[2]);
 printf("MAN  %10f %10f %10f\n",h->MA.v[0],h->MA.v[1],h->MA.v[2]);
 printf("axis %16s %16s %16s\n",h->axis.v[0],h->axis.v[1],h->axis.v[2]);
 printf("HDRS %5d %5d %5d\n",h->hd.v[0],h->hd.v[1],h->hd.v[2]);
 printf("zdatum %10f\n",h->zdatum);

}
