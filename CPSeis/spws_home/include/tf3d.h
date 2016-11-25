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
#ifndef tf3d_def
#define tf3d_def

#include "trciof77.h"









#ifdef __cplusplus
extern "C" {                 // for C++
#endif

int    tf3d_getFileType(Grid3DDesc *h);
char  *tf3d_getHeaderFile(Grid3DDesc *h);
char  *tf3d_getDataFile(Grid3DDesc *h);
char  *tf3d_getObjectName(Grid3DDesc *h);
char  *tf3d_getPropertyName(Grid3DDesc *h);
Pt3Di *tf3d_getN(Grid3DDesc *h);
Pt3Df *tf3d_getO(Grid3DDesc *h);
Pt3Df *tf3d_getD(Grid3DDesc *h);
Pt3Df *tf3d_getU(Grid3DDesc *h);
Pt3Df *tf3d_getV(Grid3DDesc *h);
Pt3Df *tf3d_getW(Grid3DDesc *h);
Pt3Df *tf3d_getMin(Grid3DDesc *h);
Pt3Df *tf3d_getMax(Grid3DDesc *h);
Pt3Di *tf3d_getBrick(Grid3DDesc *h);
Pt3Di *tf3d_getBlock(Grid3DDesc *h);
Pt3Di *tf3d_getHd(Grid3DDesc *h);
Pt3Ds *tf3d_getAxisNames(Grid3DDesc *h);
char  *tf3d_getEtype(Grid3DDesc *h);
int    tf3d_getEsize(Grid3DDesc *h);

void   tf3d_setFileType(Grid3DDesc *h, int type);
void   tf3d_setHeaderFile(Grid3DDesc *h,char *name);
void   tf3d_setDataFile(Grid3DDesc *h,char *name);
void   tf3d_setObjectName(Grid3DDesc *h,char *name);
void   tf3d_setPropertyName(Grid3DDesc *h,char *name);
void   tf3d_setN(Grid3DDesc *h, Pt3Di *p3);
void   tf3d_setO(Grid3DDesc *h, Pt3Df *p3);
void   tf3d_setD(Grid3DDesc *h, Pt3Df *p3);
void   tf3d_setU(Grid3DDesc *h, Pt3Df *p3);
void   tf3d_setV(Grid3DDesc *h, Pt3Df *p3);
void   tf3d_setW(Grid3DDesc *h, Pt3Df *p3);
void   tf3d_setMin(Grid3DDesc *h, Pt3Df *p3);
void   tf3d_setMax(Grid3DDesc *h, Pt3Df *p3);
void   tf3d_setBrick(Grid3DDesc *h, Pt3Di *p3);
void   tf3d_setBlock(Grid3DDesc *h, Pt3Di *p3);
void   tf3d_setHd(Grid3DDesc *h, Pt3Di *hd3);
void   tf3d_setAxisNames(Grid3DDesc *h, Pt3Ds *p3);
void   tf3d_setEtype(Grid3DDesc *h, char *type);
void   tf3d_setEsize(Grid3DDesc *h, int size);

void  tf3d_govo_init(Grid3DDesc *h);
void  tf3d_rmod_init(Grid3DDesc *h);
void  tf3d_tfile_init(Grid3DDesc *h);
char *tf3d_bld_vox(Grid3DDesc *h,int *nby,int X,int Y, int Z);
char *tf3d_bld_hg(Grid3DDesc *h,int *nby);
Grid3DDesc *tf3d_create_desc(char *header_file, char *data_file,
       char *prop_name, char *obj_name,
       float o1, float o2, float o3,
       int   n1, int   n2, int   n3,
       float d1, float d2, float d3, 
       char *lab1,char *lab2,char *lab3,
       int X, int Y, int Z,
       int ftype, int  wdtype);
Grid3DDesc *tf3d_create_desc_from_gostr(char *hfile,int ftype, char *in);
void  tf3d_setuvw(Grid3DDesc *h, int X, int Y, int Z);
char *tf3d_cword_type(int wdtype);
int   tf3d_iword_type(char *word_string);
int   tf3d_transform(char *ax, char *ay, char *az, void *T, Grid3DDesc *h);
void  tf3d_print(Grid3DDesc *h);


#ifdef __cplusplus
}                   // for C++
#endif

#endif
