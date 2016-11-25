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
#ifndef _ERS_TRANSFORM
#define _ERS_TRANSFORM

#include <stdio.h>

#ifndef _XLIB_H_
#define Bool int   /* copied from Xlib.h */
#define True 1
#define False 0
#endif

/* define symbols for CPS header words */
#define UNDEFHDR -99
#define SEQHDR_    1
#define OFFSET_    6
#define XGRIDHDR_  7
#define YGRIDHDR_  8
#define XBASEHDR_ 17
#define YBASEHDR_ 18
#define XANNOHDR_ 37
#define YANNOHDR_ 38
#define TIMEHDR_  -2
#define DPTHHDR_  -3
#define METER_    -4
#define KMETER_   -5
#define FEET_     -6
#define KFEET_    -7

/************************************************
 * Macros for ErsTransforms information.       **
 ***********************************************/
#define TdatGet_num(tdat)    ( (tdat)->ntrans )
#define TdatSet_num(tdat,n)  ( (tdat)->ntrans = (n) )
#define TdatGet_all(tdat)    ( &(tdat)->transforms )
#define TdatGet_nth(tdat,n)  ( &(tdat)->transforms[n] )
/************************************************
 * Macros for ErsTransform information.        **
 ***********************************************/
#define TGet_name(tran)      ( (tran)->tname )
#define TGet_x1(tran)        ( (tran)->x1 )
#define TSet_x1(tran,x)      ( (tran)->x1 = (x) )
#define TGet_x2(tran)        ( (tran)->x2 )
#define TSet_x2(tran,x)      ( (tran)->x2 = (x) )
#define TGet_hn(tran)        ( (tran)->header )       /* head number*/
#define TSet_hn(tran,n)      ( (tran)->header = (n) ) /* head number*/
#define TGet_unit(tran)      ( (tran)->units )        /* units */

typedef struct _ErsTransform
 { char   tname[16]; /* Key word identifying the transform   */
   char   units[16]; /* Physical units of x1 and x2          */
   float  x1;        /* 2 points defining the transformation */
   float  x2;
   long   header;    /* header number used for x1, and x2    */
 } ErsTransform;

typedef struct _ErsTransforms
 { long   ntrans;      /* number of defined transformations */
   ErsTransform transforms[32];
 } ErsTransforms;

/*****************************************
 * Device to User Coordinate map.      ***
 * May be different for each widget    ***
 * that is tied to the picking_record. ***
 ****************************************/
typedef struct _ErsCoordInf
 { long  xdc1, ydc1;           /*upper left device */
   long  xdc2, ydc2;           /*Lower right device*/
   float xu1, yu1;             /*upper left user   */
   float xu2, yu2;             /*Lower right user  */
   float xn1,yn1;              /*Upper right normal*/
   float xn2,yn2;              /*Lower right normal*/
   int ntran;                  /*Transformation number*/
 } ErsCoordInf;

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
 
/* Prototypes of functions defined in Transform.c */
int transforms_wr(char *file,ErsTransforms *T);
int transforms_wr_stream(FILE *fp,ErsTransforms *T);
ErsTransforms *transforms_parse(char *buf);
ErsTransforms *transforms_rd(char *file);
void ErsTransName(int header, char *name);
void ErsTransHeader(int *header, char *name);
void ErsTrans_phys_name(char *inname, char *outname);
void ErsTransSet(ErsTransforms *,char *,float ,float ,long,
            char * );
Bool ErsTransGet(ErsTransforms *,char *,float *,float *,long *,
            char *);
Bool ErsTransGetnth(ErsTransforms *,int , char *,
            float *,float *,long *,char *);
int  ErsTransAccess(ErsTransforms *tdata,int mcoord, float *xcoord,
            char *names);
long ErsTrans_count(ErsTransforms *transdata);
ErsTransform *ErsTransGetTran(ErsTransforms *, char *);
ErsTransform *ErsTransByID(ErsTransforms *, int );
ErsTransforms *new_transforms();
void destroy_transforms(ErsTransforms *);
ErsTransform *transforms_getnth(ErsTransforms *,int );
int transforms_nametopos(ErsTransforms *tdata,char *name);
int transforms_pntrtopos(ErsTransforms *tdata,ErsTransform *t);
void transforms_set_count(ErsTransforms *tdata, long count);
long transforms_count(ErsTransforms *tdata);
int transforms_rmname(ErsTransforms *tdata,char *name);
void transforms_copy(ErsTransforms *from, ErsTransforms *to);
void transforms_adddef(ErsTransforms *tdata);

ErsTransform *new_transform();
void destroy_transform(ErsTransform *);
char *transform_getname(ErsTransform *);
char *transform_getunits(ErsTransform *);
long transform_gethdr(ErsTransform *);
void transform_setname(ErsTransform *,char *);
void transform_setunits(ErsTransform *,char *);
void transform_sethdr(ErsTransform *,long);
void transform_getx(ErsTransform *t, float *x1,float *x2);
void transform_setx(ErsTransform *t, float x1,float x2);
void transform_prt(ErsTransform *t);
float transform_cnvrt(ErsTransform *ti,float xi, ErsTransform *to);
long  transform_mcnvrt(ErsTransform *ti,float *xi, int n,ErsTransform *to);
float transform_scale(ErsTransform *ti,float delta, ErsTransform *to);
int   transform_coeff(ErsTransform *ti,ErsTransform *to,float *sc,
       float *io, float *oo);


ErsCoordInf *new_coord();
void destroy_coord(ErsCoordInf *);
void Get_nc(ErsCoordInf *T, float *x);
void Set_nc(ErsCoordInf *T, float *x);
void Get_uc(ErsCoordInf *T, float *x);
void Set_uc(ErsCoordInf *T, float *x);
void Get_dc(ErsCoordInf *T, float *x);
void Set_dc(ErsCoordInf *T, float *x);
void wctodc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo);
void dctowc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo);
void wctonc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo);
void nctowc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo);
void dctonc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo);
void nctodc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo);
int Inside_Box(ErsCoordInf *T, float *x, float *y);

#ifdef __cplusplus
}                   // for C++
#endif

#endif

