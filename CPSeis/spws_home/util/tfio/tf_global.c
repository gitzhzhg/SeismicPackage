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
#include "c2f_interface.h"
#include "ebcdic.h"
#include "wrdcnvrt.h"
#include "tfdefs.h"
#include "tf_global.h"

#ifdef NEED_CAPITALS
char *dout_segy_h1ascii(char *hdr);
#define tf_global_setbuf_     TF_GLOBAL_SETBUF
#define cps_global_grotm_     CPS_GLOBAL_GROTM
#define cps_global_srotm_     CPS_GLOBAL_SROTM
#endif

#if(VMS || _AIX || __hpux)
#define tf_global_setbuf_     tf_global_setbuf
#define cps_global_grotm_     cps_global_grotm
#define cps_global_srotm_     cps_global_srotm
#endif

int tf_global_setbuf_(int *ufi, char *buf);

/*
C***************************** COPYRIGHT NOTICE *************************
C*                *CONFIDENTIAL AND PROPRIETARY INFORMATION             *
C*                              OF CONOCO INC.                          *
C*                      PROTECTED BY THE COPYRIGHT LAW                  *
C*                          AS AN UNPUBLISHED WORK                      *
C*                                                                      *
C***************************** COPYRIGHT NOTICE *************************
C        1         2         3         4         5         6         7  |
C23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E S
C
C  Primitive name:  tf_global
C         Library:  CONLIB(libcon.a) and libtfio.a
C          Author:  Richard Day
C    Date Written:  96/10/10
C    Last revised:  99/03/08   Day
C
C  Purpose:         Functions to set or get the TF_Global parameters.
C                   This is a C structure that is used by the many IO
C                   routines that manipulate trace data. The functions
C                   in this file are primitives that are used by SPWS
C                   applications and CPS processes, but are not CPS
C                   processes themselves.
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C   Date       Author      Description
C -----------  --------    --------------------------------------------
C 12.99/03/08  Day         Added method to get the iotype.
C 11.99/01/29  Day         Introduced signed byte data type as another
C                          byte data type.
C 10.98/11/30  Day         tf_global_header_file will try to deduce the
C                          correct header file from the data file name.
C                          tf_global_bld_segy3600 added.
C                          tf_global_bld_segy removed.
C                          added argument to tf_global_to_char
C 9. 98/07/17  Day         Corrected bug in tf_global_hdwrds
C 8. 98/01/08  Day         Had to change some names of called functions
C 7. 97/12/01  Day         Accessing globals common via new functions
C                          to avoid problems on the t3e.
C                          Also corrected some history bugs.
C 6. 97/08/18  Day         Added tf_global_transform method.
C 5. 97/07/16  Day         Corrected bug in tf_global_to_char
C 4. 97/06/11  Vunderink   Changed to execute dout_segy_h1ascii for Cray
C                          instead of dio_segy_h1ascii
C 3. 97/06/05  Day         Updated the calling arguments for tf3d_bld_vox
C 2. 96/12/05  Day         Added new methods for 3D files
C 1. 96/10/22  Vunderink   Inserted into CONLIB.
C-----------------------------------------------------------------------
C NOTES
C  Language: C
C Libraries: CONLIB(libcon.a) and libtfio.a         (placed into)
C  Includes: ebcdic.h  wrdcnvrt.h  tfdefs.h  tf_global.h 
C
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -
C  Heap (dynamic)  - may call upon calloc for 3600 bytes
C-----------------------------------------------------------------------
C\END DOC
*/

char *tf_global_ftype(TF_Global *g)
{ if(!g) return 0;
  return g->ftyp;
}


int tf_global_is3d(TF_Global *g)
{ if(!g) return 0;
  if(g->h) return 1;
  return 0;
}

int tf_global_byinht(TF_Global *g)
{ int byt_per_tr=0;
  if(!g) return byt_per_tr;
  byt_per_tr = g->ndptr*g->nbydp + g->nhdwd*g->nbyhd;
  return byt_per_tr;
}

int tf_global_byinhd(TF_Global *g)
{ int byt=0;
  if(!g) return 0;
  return g->nhdwd*g->nbyhd;
}

int tf_global_byintr(TF_Global *g)
{ int byt=0;
  if(!g) return 0;
  return g->ndptr*g->nbydp;
}

int tf_global_first_hcbyt(TF_Global *g)
{int n1,n2,n3,hcbyt;
 int trace_no,cell_no,pos_in_cell,byt_per_tr;
 if(!g) return -1;
 if(g->ntrfil<1) return (g->grecsiz + g->ntb);
 byt_per_tr = tf_global_byinht(g);
 
 if(strcmp(tf_global_ftype(g),"TFILE3F")==0) {
   tf_global_grid_sizes(g,&n1,&n2,&n3);
   trace_no   = n2*n3;
   cell_no    = (trace_no-1)/g->ntrcll;
   pos_in_cell= trace_no  - cell_no*g->ntrcll; /* actually 1 less */
   hcbyt      = g->grecsiz + cell_no*g->nbycll +
               pos_in_cell*byt_per_tr + n1*n2*n3*g->nbydp +
               n2*n3*g->nbyhd + g->ntb;
 }
 else {
  trace_no   = (g->ntrfil>0) ? g->ntrfil : 1;
  cell_no    = (trace_no-1)/g->ntrcll;
  pos_in_cell= trace_no  - cell_no*g->ntrcll;
  hcbyt      = g->grecsiz + cell_no*g->nbycll +
               pos_in_cell*byt_per_tr + g->ntb;
 }
 return hcbyt;
}

int tf_global_fd_(TF_Global *g)
{ if(!g) return 0;
  return g->lun;
}


char *tf_global_data_file(TF_Global *g)
{ if(!g) return 0;
  return g->path;
}

char *tf_global_header_file(TF_Global *g)
{ /*Note:header and data file are identical for some types*/
  Grid3DDesc *rh;
  char       ext[8];
  static char header[96];
  int        i_err;
  if(!g) return 0;
  if(!g->h) {
    strcpy(header,g->path);
    if(strstr(g->path,".byt") !=0) {
      strcpy(ext,"glbl");
      addext_rep_(header,  ext,  &i_err);
      return header;
    }
    if(strstr(g->path,".BYT") !=0) {
      strcpy(ext,"GLBL");
      addext_rep_(header,  ext,  &i_err);
      return header;
    }
    return g->path;
  }
  rh = (Grid3DDesc *) g->h;
  return rh->header_file;
}

int tf_global_grid_size(TF_Global *g, int axis)
{int n1,n2,n3;
 if(!g) return 0;
 tf_global_grid_sizes(g,&n1,&n2,&n3);
 if(axis==1) return n1;
 if(axis==2) return n2;
 if(axis==3) return n3;
 return 0;
}

int tf_global_grid_sizes(TF_Global *g, int *n1, int *n2, int *n3)
{Grid3DDesc *h=NULL;
 int      X=1,Y=2,Z=0;
 if(!g) return 0;
 *n1 = g->ndptr;
 *n2 = g->ntrfil;
 *n3 =  1;
 if(g->h) h = (Grid3DDesc *) g->h;
 if(h) {
  switch(h->ftype) {

   case TF3D_TYPE:
   case TF3DF_TYPE:
   case TROT_TYPE:
    *n1 = h->N.v[0]; *n2 = h->N.v[1]; *n3 = h->N.v[2];
     break;
   case HGRID_TYPE: 
   case LAYER_TYPE: 
    *n1 = h->N.v[Z]; *n2 = h->N.v[X]; *n3 = h->N.v[Y];
     break;
   case HG3DL_TYPE:
     X=0; Y=1; Z=2;
    *n1 = h->N.v[X]; *n2 = h->N.v[Y]; *n3 = h->N.v[Z];
     break;
   case VOXET_TYPE:
   case BRICK_TYPE:
    *n1 = h->N.v[0]; *n2 = h->N.v[1]; *n3 = h->N.v[2];
     break;
  }
 }
return 1;
}

int tf_global_grid_delta(TF_Global *g, float *d1, float *d2, float *d3)
{Grid3DDesc *h=NULL;
 int      X=1,Y=2,Z=0;
 if(!g) return 0;
 *d1 = g->srval;
 *d2 = 1.0;
 *d3 = 1.0;
 if(g->h) h = (Grid3DDesc *) g->h;
 if(h) {
  switch(h->ftype) {

   case TF3D_TYPE:
   case TF3DF_TYPE:
   case TROT_TYPE:
    *d1 = h->D.v[0]; *d2 = h->D.v[1]; *d3 = h->D.v[2];
     break;
   case HGRID_TYPE: 
   case LAYER_TYPE: 
    *d1 = h->D.v[Z]; *d2 = h->D.v[X]; *d3 = h->D.v[Y];
     break;
   case HG3DL_TYPE:
     X=0; Y=1; Z=2;
    *d1 = h->D.v[X]; *d2 = h->D.v[Y]; *d3 = h->D.v[Z];
     break;
   case VOXET_TYPE:
   case BRICK_TYPE:
    *d1 = h->D.v[0]; *d2 = h->D.v[1]; *d3 = h->D.v[2];
     break;
  }
 }
return 1;
}

int tf_global_grid_orgs(TF_Global *g, float *o1, float *o2, float *o3)
{Grid3DDesc *h=NULL;
 int      X=1,Y=2,Z=0;
 if(!g) return 0;
 *o1 = g->tstrt;
 *o2 = g->xorg;
 *o3 = g->yorg;
 if(g->h) h = (Grid3DDesc *) g->h;
 if(h) {
  switch(h->ftype) {

   case TF3D_TYPE:
   case TF3DF_TYPE:
   case TROT_TYPE:
    *o1 = h->O.v[0]; *o2 = h->O.v[1]; *o3 = h->O.v[2];
     break;
   case HGRID_TYPE: 
   case LAYER_TYPE: 
    *o1 = h->O.v[Z]; *o2 = h->O.v[X]; *o3 = h->O.v[Y];
     break;
   case HG3DL_TYPE:
     X=0; Y=1; Z=2;
    *o1 = h->O.v[X]; *o2 = h->O.v[Y]; *o3 = h->O.v[Z];
     break;
   case VOXET_TYPE:
   case BRICK_TYPE:
    *o1 = h->O.v[0]; *o2 = h->O.v[1]; *o3 = h->O.v[2];
     break;
  }
 }
return 1;
}

void tf_global_put_hdwrds_(TF_Global *g, int *hd)
{Grid3DDesc *h=0;
 Pt3Di hdwrds;
 if(!g) return;
 if(!g->h) return;
 h = (Grid3DDesc *) g->h;
 hdwrds.v[0]=hd[0];
 hdwrds.v[1]=hd[1];
 hdwrds.v[2]=hd[2];
 tf3d_setHd(h,&hdwrds);
}

int tf_global_hdwrd(TF_Global *g, int axis) {
 int h1,h2,h3;
 tf_global_hdwrds(g,&h1,&h2,&h3);
 if(axis==1) return h1;
 if(axis==2) return h2;
 if(axis==3) return h3;
 return -99;
}

void tf_global_hdwrds(TF_Global *g, int *h1, int *h2, int *h3) {
 Grid3DDesc *h=0;
 Pt3Di *hd=0;

 *h1= -99; *h2= -99; *h3= -99;
 if(!g) return;
 h = (Grid3DDesc *) g->h;
 hd= tf3d_getHd(h);
 if(!hd) return;
 *h1 = hd->v[0];
 *h2 = hd->v[1];
 *h3 = hd->v[2];
}

char *tf_global_axis(TF_Global *g, int axis)
{Grid3DDesc *h=NULL;
/* TFILE_TYPE,TF3D_TYPE,TF3DF_TYPE,TROT_TYPE,... are valid */
 if(!g) return 0;
 if(!g->h) return 0;
 h = (Grid3DDesc *) g->h;
 switch(axis) {

  case 1:
   return h->axis.v[0];
  case 2:
   return h->axis.v[1];
  case 3:
   return h->axis.v[2];
 }
 return 0;
}

void tf_global_put_block_(TF_Global *g, int *b)
{Grid3DDesc *h=0;
 if(!g) return;
 if(!g->h) return;
 if(strcmp(tf_global_ftype(g),"BRICK")!= 0) return;
 h = (Grid3DDesc *) g->h;
 h->blocks.v[0]=b[0];
 h->blocks.v[1]=b[1];
 h->blocks.v[2]=b[2];
}

int  tf_global_block(TF_Global *g, int *b)
{Grid3DDesc *h=0;
 if(!g) return 0;
 if(!g->h) return 0;
 if(strcmp(tf_global_ftype(g),"BRICK")!= 0) return 0;
 h = (Grid3DDesc *) g->h;
 b[0] = h->blocks.v[0];
 b[1] = h->blocks.v[1];
 b[2] = h->blocks.v[2];
 return 1;
}

void tf_global_put_brick_(TF_Global *g, int *b)
{Grid3DDesc *h=0;
 if(!g) return;
 if(!g->h) return;
 if(strcmp(tf_global_ftype(g),"BRICK")!= 0) return;
 h = (Grid3DDesc *) g->h;
 h->brick.v[0]=b[0];
 h->brick.v[1]=b[1];
 h->brick.v[2]=b[2];
}

int  tf_global_brick(TF_Global *g, int *b)
{Grid3DDesc *h=0;
 if(!g) return 0;
 if(!g->h) return 0;
 if(strcmp(tf_global_ftype(g),"BRICK")!= 0) return 0;
 h = (Grid3DDesc *) g->h;
 b[0] = h->brick.v[0];
 b[1] = h->brick.v[1];
 b[2] = h->brick.v[2];
 return 1;
}

int tf_global_get_iotype ( TF_Global *g )
{if(!g) return -1;
 return DskioChainIOtype(g->lun);
}

void tf_global_put_ntrfil ( int ntrfil, TF_Global *g )
{if(g) g->ntrfil = ntrfil;}

int tf_global_get_ntrfil ( TF_Global *g )
{return ( g->ntrfil );}

void tf_global_put_nbycll ( int nbycll, TF_Global *g )
{if(g) g->nbycll = nbycll;}

int tf_global_get_nbycll ( TF_Global *g )
{return ( g->nbycll );}

void tf_global_put_ntrcll ( int ntrcll, TF_Global *g )
{if(g) g->ntrcll = ntrcll;}

int tf_global_get_ntrcll ( TF_Global *g )
{return ( g->ntrcll );}

void tf_global_put_grecsiz ( int grecsiz, TF_Global *g )
{if(g) g->grecsiz = grecsiz;}
void tf_global_put_grecsizf_( int *grecsiz, TF_Global *g )
{if(g) g->grecsiz = *grecsiz;}

int tf_global_get_grecsiz ( TF_Global *g )
{return ( g->grecsiz );}
int tf_global_grecsizf_( TF_Global *g )
{return ( g->grecsiz );}

void tf_global_put_ntb ( int ntb, TF_Global *g )
{if(g) g->ntb = ntb;}

int tf_global_get_ntb ( TF_Global *g )
{return ( g->ntb );}

void tf_global_put_numhc ( int numhc, TF_Global *g )
{if(g) g->numhc = numhc;}

int tf_global_get_numhc ( TF_Global *g )
{return ( g->numhc );}

void tf_global_put_nbydp ( int nbydp, TF_Global *g )
{if(g) g->nbydp = nbydp;}

void tf_global_put_nbyhd ( int nbyhd, TF_Global *g )
{if(g) g->nbyhd = nbyhd;}

int tf_global_get_nbydp ( TF_Global *g )
{return ( g->nbydp );}

int tf_global_get_nbyhd ( TF_Global *g )
{return ( g->nbyhd );}

void tf_global_put_hdtyp ( int hdtyp, TF_Global *g )
{if(g) g->hdtyp = hdtyp;}

int tf_global_get_hdtyp ( TF_Global *g )
{return ( g->hdtyp );}

int tf_global_get_wdtyp ( TF_Global *g )
{if(!g) return WIEEE;
 return g->wdtyp;
}

void tf_global_put_wdtyp ( int wdtyp, TF_Global *g )
{ g->wdtyp = wdtyp;}

void tf_global_put_nhdwd ( int nhdwd, TF_Global *g )
{if(g) g->nhdwd = nhdwd;}

int tf_global_get_nhdwd ( TF_Global *g )
{return ( g->nhdwd );}

void tf_global_put_ndptr ( int ndptr, TF_Global *g )
{Grid3DDesc *h=0;
 if(!g) return;
 g->ndptr = ndptr;
 if(g->h)
  { h = (Grid3DDesc *) g->h;
    h->N.v[0] = ndptr;
  }
}

int tf_global_get_ndptr ( TF_Global *g )
{return ( g->ndptr );}

void tf_global_put_srval ( float srval, TF_Global *g )
{if(g) g->srval = srval;}

float tf_global_get_srval ( TF_Global *g )
{return  g->srval ;}

void tf_global_put_tstrt ( float tstrt, TF_Global *g )
{if(g) g->tstrt = tstrt;}

float tf_global_get_tstrt ( TF_Global *g )
{return  g->tstrt ;}

int tf_global_get_dunits( TF_Global *g )
{/* dunit=1 for meters
    dunit=2 for feet */
  return g->dunits;
}

void  tf_global_put_dunits( int dunits, TF_Global *g )
{ if(g) g->dunits=dunits; }

void tf_global_prhd_(TF_Global *g)
{ char *ftype;
  if(!g) return;
  ftype = g->ftyp;
  if(!ftype) return;

  printf("# File Global Parameters\n*global\n");
  printf(" path=%s ,\n",g->path);
  printf(" grecsiz=%8d , wdtyp=%8d,\n",g->grecsiz,g->wdtyp);
  printf(" ftyp=%s , ntrfil=%8d , trmaxg=%g,\n",g->ftyp,g->ntrfil,g->trmaxg);
  printf(" ndptr=%8d , nbydp=%8d,\n",g->ndptr,g->nbydp);
  printf(" nhdwd=%8d , nbyhd=%8d , hdtyp =%8d,\n",g->nhdwd,g->nbyhd,g->hdtyp);
  printf(" nbycll=%8d , ntrcll=%8d\n",g->nbycll,g->ntrcll);
  printf(" numhc=%8d , ntb=%8d,\n",g->numhc,g->ntb);
  printf(" srval=%f , tstrt=%f,\n",g->srval,g->tstrt);
  printf(" xorg =%f , yorg =%f,\n",g->xorg,g->yorg);
  printf(" dx0=%f , %f , %f , %f,\n",g->dx0[0],g->dx0[1],g->dx0[2],g->dx0[3]);
  printf(" dn0=%f , %f , %f , %f,\n",g->dn0[0],g->dn0[1],g->dn0[2],g->dn0[3]);
  printf(" dunits=%8d\n",g->dunits);


  if(strcmp(ftype,"TFILE3")==0 || strcmp(ftype,"TFILE3F")==0)
   tf_global_tf_prhd((Grid3DDesc *) g->h);
  if(strcmp(ftype,"HGRID")==0 || strcmp(ftype,"HG3DL")==0 ||
     strcmp(ftype,"LAYER")==0)
   tf_global_rmod_prhd((Grid3DDesc *) g->h);
  if(strcmp(ftype,"VOXET")==0 || strcmp(ftype,"BRICK")==0)
   tf_global_govo_prhd((Grid3DDesc *) g->h);
}

void tf_global_tf_prhd(Grid3DDesc *h)
{ int X=1,Y=2,Z=0;
  float ftes=0;
  if(!h) return;
  
  printf(" %s =%s\n",h->keys[12],h->P.file);
  printf(" %s =%s\n",h->keys[14],h->P.etype);
  printf(" gridded model limits\n");
  printf(" %s= %10d %s= %10g %s= %10g %s= %s\n", h->keys[0],h->N.v[Z],
  h->keys[3],h->O.v[Z], h->keys[6],h->D.v[Z],h->keys[9],h->axis.v[Z]);
  printf(" %s= %10d %s= %10g %s= %10g %s= %s\n", h->keys[1],h->N.v[X],
  h->keys[4],h->O.v[X], h->keys[7],h->D.v[X],h->keys[10],h->axis.v[X]);
  printf(" %s= %10d %s= %10g %s= %10g %s= %s\n", h->keys[2],h->N.v[Y],
  h->keys[5],h->O.v[Y], h->keys[8],h->D.v[Y],h->keys[11],h->axis.v[Y]);

  ftes = h->U.v[0]+h->U.v[1]+h->U.v[2];
  if(ftes !=0)
   printf("%s= %10f %10f %10f\n", h->keys[15],h->U.v[0],h->U.v[1],h->U.v[2]);
  ftes = h->V.v[0]+h->V.v[1]+h->V.v[2];
  if(ftes !=0)
   printf("%s= %10f %10f %10f\n", h->keys[16],h->V.v[0],h->V.v[1],h->V.v[2]);
  ftes = h->W.v[0]+h->W.v[1]+h->W.v[2];
  if(ftes !=0)
   printf("%s= %10f %10f %10f\n", h->keys[17],h->W.v[0],h->W.v[1],h->W.v[2]);
  printf("\n");
}

void tf_global_rmod_prhd(Grid3DDesc *h)
{ int X=1,Y=2,Z=0;
  if(!h) return;
  if (h->ftype==HG3DL_TYPE) {X=0; Y=1; Z=2;}

  printf(" %s =%s\n",h->keys[19],h->P.file);
  if(h->tran_file) printf(" %s =%s\n",h->keys[20],h->tran_file);
  else printf(" %s =%s\n",h->keys[20],"NONE");
  printf(" %s =%s\n",h->keys[21],h->P.etype);
  printf(" layered model limits\n");
  printf(" %s= %10g %s= %10g  %s= %s\n", h->keys[9],h->MI.v[Z],
  h->keys[12],h->MA.v[Z], h->keys[15], h->axis.v[Z]);
  printf(" %s= %10g %s= %10g  %s= %s\n", h->keys[10],h->MI.v[X],
  h->keys[13],h->MA.v[X], h->keys[16], h->axis.v[X]);
  printf(" %s= %10g %s= %10g  %s= %s\n", h->keys[11],h->MI.v[Y],
  h->keys[14],h->MA.v[Y], h->keys[17], h->axis.v[Y]);
  printf(" gridded model limits\n");
  printf(" %s= %10d %s= %10g %s= %10g\n", h->keys[0],h->N.v[Z],
  h->keys[3],h->O.v[Z], h->keys[6],h->D.v[Z]);
  printf(" %s= %10d %s= %10g %s= %10g\n", h->keys[1],h->N.v[X],
  h->keys[4],h->O.v[X], h->keys[7],h->D.v[X]);
  printf(" %s= %10d %s= %10g %s= %10g\n", h->keys[2],h->N.v[Y],
  h->keys[5],h->O.v[Y], h->keys[8],h->D.v[Y]);
  printf(" datum level\n");
  printf(" %s= %g\n", h->keys[18],h->zdatum);
  printf("\n");
}

void tf_global_govo_prhd(Grid3DDesc *h)
{ if(!h) return;

  if(h->ftype==VOXET_TYPE)
   printf(" GOCAD VOXET 0.01\n HEADER {\n");
  else if (h->ftype==BRICK_TYPE)
   printf(" GOCAD BRICK 0.01\n HEADER {\n");
  else return;

  printf(" name: %s\n }\n",h->name);
  printf(" %s  %10g  %10g %10g\n", h->keys[0],h->O.v[0],
         h->O.v[1],h->O.v[2]);
  printf(" %s  %10g  %10g %10g\n", h->keys[1],h->U.v[0],
         h->U.v[1],h->U.v[2]);
  printf(" %s  %10g  %10g %10g\n", h->keys[2],h->V.v[0],
         h->V.v[1],h->V.v[2]);
  printf(" %s  %10g  %10g %10g\n", h->keys[3],h->W.v[0],
         h->W.v[1],h->W.v[2]);
  printf(" %s  %10d  %10d %10d\n", h->keys[4],h->N.v[0],
         h->N.v[1],h->N.v[2]);
  printf(" %s  %10g  %10g %10g\n", h->keys[5],h->MI.v[0],
         h->MI.v[1],h->MI.v[2]);
  printf(" %s  %10g  %10g %10g\n", h->keys[6],h->MA.v[0],
         h->MA.v[1],h->MA.v[2]);
  printf(" %s  %10s  %10s %10s\n", h->keys[7],h->axis.v[0],
         h->axis.v[1],h->axis.v[2]);
  printf(" %s  %10s  %10s %10s\n", h->keys[8],h->type.v[0],
         h->type.v[1],h->type.v[2]);
  printf(" %s  %10g  %10g %10g\n", h->keys[9],h->D.v[0],
         h->D.v[1],h->D.v[2]);

  if(h->ftype==BRICK_TYPE)
   {
    printf(" %s  %10d  %10d %10d\n", h->keys[10],h->blocks.v[0],
         h->blocks.v[1],h->blocks.v[2]);
    printf(" %s  %10d  %10d %10d\n", h->keys[11],h->brick.v[0],
         h->brick.v[1],h->brick.v[2]);
   }

  printf(" \n");
  printf(" %s %d %s\n",h->P.keys[0],h->P.id,h->P.name);
  printf(" %s %d %s\n",h->P.keys[1],h->P.id,h->P.file);
  printf(" %s %d %d\n",h->P.keys[2],h->P.id,h->P.esize);
  printf(" %s %d %s\n",h->P.keys[3],h->P.id,h->P.etype);
  printf("END\n");
}

int  tf_global_transform(char *ax, char *ay, char *az,
     void *T, TF_Global *g) {
 Grid3DDesc *h;
 if(!g) return 0;
 h = (Grid3DDesc *) g->h;
 return tf3d_transform(ax,ay,az,T, h);
}


 /*
 C\USER DOC
 C Name:     tf_global_from_grid_
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Given individual grid parameters, fill in the
 C           TF_Global structure for a 3D file description.
 C
 C Prototype:
 C int tf_global_from_grid_(TF_Global *g, char *hfile,
 C     char *dfile, char *pname, char *obj_name,
 C     float *o1, float *o2, float *o3,
 C     int   *n1, int   *n2, int   *n3,
 C     float *d1, float *d2, float *d3, 
 C     char *lab1,char *lab2,char *lab3,
 C     int X, int Y, int Z,
 C     int *ftype, int  *wdtype, int *dunits)
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
 C     dunits    Distance units flag. 1=meters, 2=feet.
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
int tf_global_from_grid_(TF_Global *g, char *hfile, char *dfile,
    char *pname, char *obj_name,
    float *o1, float *o2, float *o3,
    int   *n1, int   *n2, int   *n3,
    float *d1, float *d2, float *d3, 
    char *lab1,char *lab2,char *lab3,
    int *XA, int *YA, int *ZA,
    int *ftype, int  *wdtype, int *dunits)
{
 Grid3DDesc *h=0;
 int        dsize=1,X= *XA,Y= *YA,Z= *ZA;

 if(!hfile)  return 0;
 if(*n1<1) *n1=1;
 if(*n2<1) *n2=1;
 if(*n3<1) *n3=1;
 if(*d1==0.) *d1=1.0;
 if(*d2==0.) *d2=1.0;
 if(*d3==0.) *d3=1.0;
 if(*wdtype==WIEEE || *wdtype==WIBM || *wdtype==WVMS) dsize=4;
 if(*wdtype==WIBM2) dsize=2;
 if(*wdtype==WCRAY) dsize=8;
 if(*wdtype==WSBYT) dsize=1;

/* set up TF_Global for the file you want to write */
 if(!g) return 0;
 if(*ftype==CBYTE_TYPE) return 0;
 strcpy(g->ftyp,(char *) getgl_ftype_to_str(*ftype));
 strcpy(g->path,dfile);   /* set name of the output file  */
 if(strcmp(dfile,"SAME") == 0 ) strcpy(g->path,hfile);
 g->lun    = 0;
 g->wdtyp  = *wdtype;     /* see wrdcnvrt.h for types     */
 g->ntb    = 0;
 g->numhc  = 0;
 g->trmaxg = 0.;
 g->ntrcll = 1;           /* number of traces per cell    */
 g->nhdwd  = 0;           /* header words per trace       */
 g->nbyhd  = 4;           /* header words per trace       */
 g->hdtyp  = 0;
 g->grecsiz= 0;           /* bytes reserved for global data*/
 g->nbydp  = dsize;       /* number of bytes per trace word */
 g->ndptr  = *n1;
 g->srval  = *d1;
 g->tstrt  = *o1;
 g->dunits = *dunits;
 if(*ftype==TFILE_TYPE || *ftype==TF3D_TYPE || *ftype==TF3DF_TYPE)
  { g->nhdwd=64;
    g->nbyhd=4;    /* will use WIEEE for headers */
    g->grecsiz=GRECSIZ;
    g->hdtyp  = 0;
  }
 if(*ftype==SEGY_TYPE)
  { g->nhdwd=240;
    g->nbyhd=1;
    g->grecsiz=3600;
    g->hdtyp  =2;
    g->wdtyp = WIBM;
    if(*wdtype==WIBM2) g->wdtyp=WIBM2;
    if(*wdtype==WBYTE) g->wdtyp=WBYTE;
    if(*wdtype==WSBYT) g->wdtyp=WSBYT;
  }

 g->ntrfil = 0;  /* initially 0 until written n2*n3 */
 g->nbycll = g->ntrcll*(g->nbyhd*g->nhdwd + g->ndptr*g->nbydp);
 strcpy(g->srun,"??");

 /* fill in cps rotation matrix if available */
 tf_global_cps_rotparm(g, 1);


 h = tf3d_create_desc(hfile, dfile, pname, obj_name,
    *o1, *o2, *o3,
    *n1, *n2, *n3,
    *d1, *d2, *d3, 
    lab1,lab2,lab3,
    X, Y, Z,
    *ftype, *wdtype);
 g->h = (void *) h;

 return 1;
}

/* Capture the rotation matrix from cps globals common.
 * This is useful if we are linked with other processing
 * modules that alter the gloabals
 */
void tf_global_cps_rotparm(TF_Global *g, int dir) {};
/* wmm removed this function
{CPSG      *cpsglob=0;
 if(!g) return;
 cpsglob = (CPSG *) cps_global_addr_();
 if(cpsglob) {
    if(dir>0) {
      cps_global_grotm_(&g->xorg,&g->yorg,g->dx0,g->dn0);
    } else {
      cps_global_srotm_(&g->xorg,&g->yorg,g->dx0,g->dn0);
    }
   }
 else
   {g->xorg=0.;
    g->yorg=0.;
    g->dx0[0]=1.0; g->dx0[1]=0.0;
    g->dx0[2]=0.0; g->dx0[3]=1.0;
   }
}
*/
/*
 C\USER DOC
 C Name:     tf_global_to_char
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Given a complete TF_Global structure build the appropriate
 C           string for the file type indicated by the structure.
 C
 C Prototype:
 C     char *tf_global_to_char(TF_Global *g,int *nby,char *temp)
 C          - Convert g to a file header. The number of bytes
 C            that are set is given by nby.
 C     int   tf_global_setbuf_(int *ufi, char *buf)
 C          - For fortran access. Return value is the byte count.
 C
 C NOTES:
 C  1. Will handle VOXET,BRICK,HGRID,HG3DL, & TFILE file types.
 C     Will return NULL if there is a failure.
 C  2. nby gives the allocated string size. Not its length!
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C 97/06/05 R.S.Day     Altered call to tf3d_bld_vox
 C 96/12/05 R.S.Day     Supports BRICK type as well now.
 C          R.S.Day     Original Version
 C\END DOC
 */
int tf_global_setbuf_(int *ufi, char *buf)
{int       i_err,nby=0;
 char      *abuf=0;
 TF_Global global;
 if(*ufi<0) return 0; 
 if(tf_glbl_get_(&i_err, ufi, &global) !=0) return 0;
 abuf = tf_global_to_char(&global, &nby, buf);
 if(abuf) memcpy(buf,abuf,nby);
 if(abuf) free(abuf);
 return nby;
}

char *tf_global_to_char(TF_Global *g,int *nby, char *temp)
{Grid3DDesc *h=0;
 char *tstr=0;
 int ftype;
 *nby=0;
 if(!g) return 0;
 h= (Grid3DDesc *) g->h;

 ftype = getgl_str_to_ftype(g->ftyp);
 switch(ftype)
   {
     case  VOXET_TYPE:
      tstr = tf3d_bld_vox(h,nby,2,3,1);
      return tstr;
     case  BRICK_TYPE:
      tstr = tf3d_bld_vox(h,nby,2,3,1);
      return tstr;
     case  CBYTE_TYPE:
      return tstr;
     case  HGRID_TYPE:
     case  LAYER_TYPE:
      tf3d_setuvw(h, 2, 3, 1);
      tstr = tf3d_bld_hg(h,nby);
      return tstr;
     case SEGY_TYPE:
      tstr = tf_global_bld_segy3600(g,nby, temp);
      return tstr;
     case  HG3DL_TYPE:
      tf3d_setuvw(h, 1, 2, 3);
      tstr = tf3d_bld_hg(h,nby);
      return tstr;
     case  TFILE_TYPE:
      tstr = tf_global_bld_tf(g,nby);
      return tstr;
     case  TF3D_TYPE:
      tstr = tf_global_bld_tf(g,nby);
      return tstr;

     case  TF3DF_TYPE:
      tstr = tf_global_bld_tf(g,nby);
      return tstr;
     default:
      tstr =0;
   }
 return 0;
}


/*
 C\USER DOC
 C Name:     tf_global_bld_tf
 C Library:  libtfio.a
 C Language: C
 C Author:   Richard S. Day
 C Purpose:  Build the string for the global part of a TFILE
 C
 C Prototype:
 C     char *tf_global_bld_tf(TF_Global *g,int *nby)
 C
 C NOTES:
 C  1. Will handle TFILE,TFILE3 or TFILE3F file types.
 C     Will return NULL if there is a problem.
 C  2. Just a wrapper around tf_global_to_asc
 C
 CRevisions:
 CDATE      WHO         DESCRIPTION
 C--------  --------    --------------------------------------------
 C          R.S.Day     Original Version
 C\END DOC
 */
char *tf_global_bld_tf(TF_Global *g,int *nby)
{char    *out=0;
 int     lens;
 *nby = 0;
 out = (char *) tf_glbl_to_asc(g);
 if(out) {
   lens = strlen(out);
   *nby = (lens+1 > g->grecsiz) ? g->grecsiz : lens;
 }
 return out;
}

char *tf_global_bld_segy3600(TF_Global *g,int *nby, char *temp)
{char    *out=0;
 int     i;
 *nby=0;
 out = (char *) calloc(1,3600);
 if(out) *nby=3600;
 dio_segy_h3200(out, temp);
 for (i = 0; i < 3200; i++) out[i] = ascii_to_ebcdic[out[i]];
 dio_segy_h2set(out+3200,&g->srval,&g->ndptr,&g->wdtyp,&g->dunits);
 return out;
}

/*
char *tf_global_bld_segy(TF_Global *g,int *nby)
{Grid3DDesc *h=0;
 char    *out=0;
 int i;
 out = (char *) calloc(1,3600);
 *nby=3600;

#ifdef CRAY
 dout_segy_h1ascii(out);
#else
 dio_segy_h1ascii(out);
#endif

 for (i = 0; i < 3200; i++)
  {out[i] = ascii_to_ebcdic[out[i]]; }

 dio_segy_h2set(out+3200,&g->srval,&g->ndptr,&g->wdtyp,&g->dunits);
 return out;
}
*/


char *tf_global_cword_type(int wdtype)
{ 
  return wrdc_cword_type(wdtype);
}

int tf_global_iword_type(char *word_string)
{
  return wrdc_iword_type(word_string);
}



