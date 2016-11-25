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
#include <stdio.h>
#include <string.h>
#include "model_io.h"
#include "model_desc.hh"
#include "pcardgowr.h"
#include "model_trans.h"
#include "wrdcnvrt.h"
#include "rmodc.h"
#include "dbutil.h"
#include "tfdefs.h"
#include "tf_global.h"
#include "put_global.h"
#include "netw.h"
#include "grid_data.hh"


#ifdef __cplusplus
extern "C" {                 // for C++
#endif
int  grid_wr(ModelDesc *model, char *file, int  word_out, 
     int  file_type, gridData *gdata);
int  grid_wrhd(char *hfil, char *gfil, char *mod_nam, char *prop_nam,
               int  word_type,int  file_typ,
               GridLimits *glimits, ModLimits *mlimits, ErsTransforms *tdata);

void rmodclos_(int *key);
void rmodwrhd_w_(char *hfil,char *gfil,char *tfil,char *ftyp,
                 int *opstat,int *itrans,
             char *cxi,char *cyi,char *czi,char *cxo,char *cyo,char *czo,
             int *ncoord,float *xcoord,char *coord,
             float *mxmin,float *mxmax,
             float *mymin, float *mymax,
             float *mzmin,float *mzmax, float *zdatum,
             int *nx, float *ox, float *dx,
             int *ny, float *oy, float *dy,
             int *nz, float *oz, float *dz,
             int *wd_type,int *istat);
#ifdef __cplusplus
}                   // for C++
#endif


/*********
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: grid_wr
C        Author: R.S.DAY
C       Written: 94/08/12
C  Last revised: 96/05/08
C
C  Purpose:      Writes a gridded model to disk in an RMOD format.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C  int  grid_wr(ModelDesc *model, char *file, int  word_out,
C       int  file_type, gridData *gdata)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C  model      ModelDesc in       A model with the geometry defined
C  file       char      in       Name of the output header file.
C  word_out   int       in       Desired output word type.
C                                (see wrdcnvrt.h )
C  file_type  int       in       See tfio.h . E.G. HGRID_TYPE
C  gdata      gridData  in       Description of grid in core.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. RMOD grid file consists of an ascii header file and a binary data
C    file. The binary file is written in C. 
C 2. Returns 1 if write succeeded.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 1.  94/08/12 R.S.Day    Initial version.
C-----------------------------------------------------------------------
C                                 NOTES
C
C-----------------------------------------------------------------------
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
******/
int  grid_wr(ModelDesc *moddesc, char *file, int  word_out, 
     int  file_type, gridData *gdata)
{ModLimits    *mlimits;
 GridLimits   *glimits=NULL;
 ErsTransforms *tdata;
 ErsTransform *tx,*ty,*tz;
 ErsTransform *txg,*tyg,*tzg;


 float xmin,xmax,ymin,ymax,zmin,zmax;
 int   nx,ny,nz;
 float xorg,yorg,zorg,dx,dy,dz;
 char  *garr=0;

 int   nc;
 int   key,reclen;
 int   wordo = word_out;
 char  hostname[32],os[32];
 int   opstat = 0; /* open as new */

 int   lerr,local_word;
 int   ierr;

 int   lun = -1, rec;
 int   i,typ;
 char  hfil[96],gfil[96],tfil[96],  msg[96];
 char  *gf=NULL;
 char  mod_nam[32], prop_nam[32];
 int   istat=0;

 if(!file)
  { strcpy(msg,"grid_wr: NULL file name, no output written\n");
    goto error;
  }
 if(strcmp(file,"NONE")==0 || file[0]==' ') return istat;
 if(!gdata)
  { strcpy(msg,"grid_wr: gridData object was NULL\n");
    goto error;
  }
 /****************************************
  * Check for well defined grid data.  ***
  ***************************************/
 garr = (char *) gdata->getGridData();
 if(!garr) return istat;
 gdata->getGridVals(&nz,&zorg,&dz, &nx,&xorg,&dx, &ny,&yorg,&dy);
 gdata->getGridCoords(&tzg,&txg,&tyg);
 if(nx < 1 || nz < 1 || ny < 1)
  { strcpy(msg,"grid_wr: one of nx,ny,nz is < 1\n");
    goto error;
  }
 if(dx == 0. || dz == 0. || dy ==0.)
  { strcpy(msg,"grid_wr: one of dx,dy,dz is = 0\n");
    goto error;
  }

 /****************************************
  * Check for necessary model components**
  ***************************************/
 if(moddesc == NULL )
  { strcpy(msg,"grid_wr: NULL model, no input for calculation\n");
    goto error;
   }

 /*************************************
  * Retrive old Grid limits & reset. **
  * n1 is z, n2 is x & n3 is y.      **
  ************************************/
 mlimits = moddesc->MLimits();
 mlimits_get(mlimits, &xmin, &xmax, &zmin, &zmax, &ymin, &ymax,
             &tx, &ty, &tz );
 if(tzg != tz || txg != tx || tyg != ty)
  { strcpy(msg,"grid_wr: units are not consistent\n");
    goto error;
  }
 tdata = moddesc->transforms();

 /***************************************
  * Construct new file names.          **
  * force conventional extensions .    **
  **************************************/
  strcpy(hfil,file);
  strcpy(tfil,"SAME");
  typ = file_type;
  put_global_bld_dfile_(&typ, hfil,gfil);

 /***************************************
  * Open a grid data file.             **
  **************************************/
  if(word_out < 1) word_out = WIEEE;
  key = 0;
  opstat = 0; /* try as a new file*/
  reclen = 4*nz;
  if(word_out==WBYTE) reclen = nz;
  if(word_out==WCRAY) reclen = 8*nz;
  key = rmodopc_(&lun, gfil, gfil, "STREAM","NEW","BINARY",
        &reclen,&wordo);
  if(key < 0 )
   { gf = NULL; key = 0;
     strcpy(msg,"grid_wr: data file open failed\n");
     goto error;
    }

 /**********************************************************
  * Dump grid data to file if key is set.                 **
  *********************************************************/
  int  num;
  local_word = netw_lnode(hostname,os);
  num = nx*nz;
  if(strcmp(gdata->getWord(),"FLOAT")==0) num = num*sizeof(float);
  if(key > 0)
   {char *oarr;
    for(i=0;i<ny;i++)
     {oarr = garr + i*num;
      if(local_word == WIEEE && strcmp(gdata->getWord(),"FLOAT")==0)
        nc = bswap_(&num,(unsigned char *)oarr);
      rec = (gdata->Slice()-1)*nx + i*nx + 1;
      rmodwrc_( &key,&rec, &nx, &nz, (char *) oarr,
             &wordo,&local_word,&lerr, msg);
      if(local_word == WIEEE && strcmp(gdata->getWord(),"FLOAT")==0)
        nc = bswap_(&num,(unsigned char *)oarr);
      if(lerr != 0) goto error;
     }
    rmodclc_(&key);
    key = 0;
   }

 /**********************************************************
  * Write the header file. Set grid limits before write.  **
  *********************************************************/
  glimits = new_glimits();
  glimits_set(glimits,&nz,&zorg,&dz,&nx,&xorg,&dx,&ny,&yorg,&dy, 
             tzg,txg,tyg);
  strcpy(mod_nam,"velmod");
  strcpy(prop_nam,"velocity");
  ierr = grid_wrhd(hfil, gfil, mod_nam, prop_nam,
                  wordo, file_type,
                  glimits, mlimits, tdata);
  if(!ierr) goto error;

 istat = 1;
 error:
 if(glimits) destroy_glimits(glimits);
 if(key) rmodclc_(&key);
 if(!istat) printf("%s",msg);
 return istat;
}

int  grid_wrhd(char *hfil, char *gfil, char *mod_nam, char *prop_nam,
               int  word_type,int  file_typ,
               GridLimits *glimits, ModLimits *mlimits, ErsTransforms *tdata)
{/****
  * word_type=WIEEE or WVMS
  * file_typ =HGRID_TYPE or VOXET_TYPE or BRICK_TYPE
  * Note: glimits is transformed to be consistent with mlimits.
  *****/
 TF_Global  G;
 Grid3DDesc *h3;
 Pt3Di      p3;
 float mxmin,mxmax,mymin,mymax,mzmin,mzmax;
 int  n1l,n2l,n3l;
 long header;
 int  n1,n2,n3;
 int  key, mf=1;;
 float d1,d2,d3,o1,o2,o3;
 ErsTransform *t1,*t2,*t3,*tx,*ty,*tz;

 int  wd_type,f_type,istat;
 float zdatum=0,x1,x2;
 int   i, X=2,Y=3,Z=1;
 int   itrans=1;
 int   opstat;
 int   ncoord;
 char  coord[256], string[24];
 float xcoord[32];
 char  cxi[16],cyi[16],czi[16];
 char  cxo[16],cyo[16],czo[16];
 char  tname[24],units[16];
 char tfil[24];
 strcpy(tfil,"SAME");

 if(!mlimits) return 0;
 if(!glimits) return 0;
 if(!tdata  ) return 0;

 mlimits_get(mlimits,&mxmin,&mxmax,&mzmin,&mzmax,&mymin,&mymax,&tx,&ty,&tz);

// Transform the grid limits to model limits units.
 glimits_trans(glimits, tdata,
   transform_getname(tx), transform_getname(ty), transform_getname(tz));

 glimits_get(glimits, &n1l,&o1,&d1, &n2l,&o2,&d2, &n3l,&o3,&d3,
             &t1,&t2,&t3);

 cxi[0]='\0'; cxi[1]='\0'; cxi[2]='\0';
 if(tx != NULL) strcpy(cxi,transform_getname(tx));
 if(ty != NULL) strcpy(cyi,transform_getname(ty));
 if(tz != NULL) strcpy(czi,transform_getname(tz));
 strcpy(cxo , cxi);
 strcpy(cyo , cyi);
 strcpy(czo , czi);
 ncoord  = 0;
 coord[0]='\0';
 i = 1;
 while(ErsTransGetnth(tdata,i,tname,&x1,&x2,&header,units))
  { ncoord++;
    xcoord[2*i-2]= x1;
    if(x2 == x1) x2 = x1+1.;
    xcoord[2*i-1]= x2;
    strcpy(string,"                ");
    strncpy(string,tname,strlen(tname));
    strcat(coord,string);
    i++;
  }


 n1 = n1l; n2 = n2l; n3=n3l;
 wd_type = word_type;
 f_type  = file_typ;
 switch(f_type)/* see tfio.h */
  {
   case HGRID_TYPE:

    opstat = 0; /* try as a new file*/
    rmodwrhd_w_(hfil,gfil,tfil,"GRID",&opstat, &itrans,
     cxi,cyi,czi,cxo,cyo,czo, &ncoord,xcoord,coord,
     &mxmin,&mxmax, &mymin,&mymax, &mzmin,&mzmax, &zdatum,
     &n2,&o2,&d2, &n3,&o3,&d3, &n1,&o1,&d1,
     &wd_type,&istat);
    if(istat != 0)
      {opstat = 1; /* try as a old file*/
       rmodwrhd_w_(hfil,gfil,tfil,"GRID",&opstat, &itrans,
        cxi,cyi,czi,cxo,cyo,czo, &ncoord,xcoord,coord,
        &mxmin,&mxmax, &mymin,&mymax, &mzmin,&mzmax, &zdatum,
        &n2,&o2,&d2, &n3,&o3,&d3, &n1,&o1,&d1,
        &wd_type,&istat);
      }
    key = (int) dbutil_rfile2key_(hfil);
    rmodclos_(&key);
    if(istat ==0) return 1;
    return 0;

   case VOXET_TYPE:
   case BRICK_TYPE:

    tf_global_from_grid_(&G,hfil,gfil,prop_nam,mod_nam,
      &o1,&o2,&o3,
      &n1, &n2, &n3,
      &d1, &d2, &d3,
      transform_getname(tz), transform_getname(tx), transform_getname(ty),
      &X, &Y, &Z,
      &f_type, &wd_type,&mf);
    
    // hardwire as 1 big block for present
    h3 = (Grid3DDesc *) G.h;
    p3.v[0]=n1;
    p3.v[1]=1;
    p3.v[2]=1;
    tf3d_setBrick(h3,&p3);
    p3.v[0]= 1;
    p3.v[1]=n2;
    p3.v[2]=n3;
    tf3d_setBlock(h3,&p3);
    put_global_to_file_(&G);
    return 1;

   default:
    mf = 1;
  }

 return 0;
}
