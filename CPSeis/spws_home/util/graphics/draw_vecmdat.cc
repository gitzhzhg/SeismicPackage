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
#include "draw_mod.h"
#include "vect/ll_seis_vect.hh"


/********
C\USER DOC
C-----------------------------------------------------------------------
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C       written in c -- designed to be called from c
C
C     Utility Name:  draw_vmdat
C          Written:  93/02/14  by:  R.S.Day
C     Last revised:            by:  
C
C  Purpose:     Plot the material control point data in ErsMaterial.
C
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine            source code directory       library
C  -------            ---------------------       -------
C  ultrix             ~spws/util/graphics         graphlib.a
C
C  c files          c++ files       fortran files      other files
C  -------          ---------       -------------      -----------
C  none             draw_vecmdat.cc                  
C----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:
C  Header files: model.h draw_mod.h(vector.hh modbase_data.hh)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 93/01/01  R.S.Day    Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C  int draw_vmdats(void *VL,ErsMaterials *mats,int phd,int idraw,
C        char *msg )
C  int draw_vmdat(void *vl,ErsMaterial *mat,int phd,
C        int idraw, char *msg );
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C   int    phd     IN      0-64      Flags header matching x-coordinates
C   int    idraw   IN      0,1       Undraw, Draw the the data
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. draw_vmdat & draw_vmdats return the number of Vectors drawn.
C-----------------------------------------------------------------------
C\END DOC
****/
int draw_vmdats(void *VL,ErsMaterials *mats,
    int phd,int shd,int thd,int zeit,int idraw,char *msg)
{ ErsMaterial *mat;
  int i,nplotted;
  nplotted=0;
  for(i=0;i<materials_count(mats);i++)
   {mat = mats_get_nth(mats,i);
    nplotted += draw_vmdat(VL,mat,phd,shd,thd,zeit,idraw, msg );
   }
 return nplotted;
}

int draw_vmdat(void *VL,ErsMaterial *mat,
    int phd,int shd,int thd,int zeit,int idraw,char *msg)
{
  ModBaseData **objarr;
  Vector **vectarr;
  SeisVectLinkedList *vl;


  int   n,lstyle,mstyle,count;
  char  *hname,cname[32];
  unsigned int   lwidth,mwidth,msize;



  strcpy(cname, material_getcolor(mat));
  if(strlen(cname)==0) strcpy(cname,"green");
  hname  = material_getname(mat);
  lstyle = Vector::SolidLine;
  lwidth = 3;
  mstyle = Vector::FilledTriangleMarker;
  msize  = 2*lwidth + 3;
  mwidth = 1;

  vl = (SeisVectLinkedList *) VL;
  if(mat == NULL )
   { sprintf(msg,"draw_vmdat: no data to plot\n");
     return 0; }
/*--draw the material coordinates */
  if(mat != NULL)
   {
    vectarr = (Vector **) materialGetVectA(mat);
    if(vectarr == NULL)
     {if(vl == NULL )
       { sprintf(msg,"draw_vmdat: no drawable specified\n");
         return 0;
       }
       count  = MaterialToBaseData(mat,&objarr,phd,shd,thd,zeit);
       if(count == 0)
        {delete objarr[0]; /* deletes pointer in array */
         delete []objarr; /* deletes array pointer */
         return count;
        }
       vectarr= new Vector *[count+1];
       vectarr[count] = NULL;
       materialSetVectA(mat, (void *) vectarr);
       for(n=0;n<count;n++)
        {vectarr[n] = vl->add(hname,objarr[n],cname,lwidth,False,
             (Vector::VectorStyle) lstyle,
             (Vector::VectorMarker) mstyle,msize,mwidth);
         if(vectarr[n] == NULL) return n;
        }
        MaterialSetVisible(mat,idraw);

     }
    else
     {lstyle = (int)  vectarr[0]->getStyle();
      lwidth = vectarr[0]->getWidth();
      strcpy(cname,vectarr[0]->getColor());
      vectarr[0]->getMarker((Vector::VectorMarker *) &mstyle,&msize,&mwidth); 
      MatSetAttributes(mat,lstyle, lwidth,
        mstyle, msize, mwidth,cname,idraw);
     }
   }

 return count;
}

int MaterialToBaseData(ErsMaterial *mat, ModBaseData ***objarr,
    int phd,int shd,int thd,int zeit)
{
 ModBaseData **array;
 int   i,n, num,count;

 float *s=NULL,*t=NULL;
 if(mat == NULL) return 0;
 //shd = NILKEY; carry as dummy arrays for now
 thd = NILKEY;

 count= 0; /* number of segments   */
 num  = 0; /* pts within a segment */
 
   { int  *stype,*segid,ndof,mid;
     int   npts;
     float *x,*z,*pv; /* pv saving valid for ndof =1 only */
//     material_get(mat,&ndof,&mid,&npts,&stype,&x,&z,&pv);
     material_get3(mat,&ndof,&mid,&npts,&stype,&segid,
       &x,&s,&z,&pv);
     num  =0;
     count=0;
     i = 0;
     num++;
     for(n=1;n<npts;n++)
      { if(stype[n] != stype[n-1])
         {
          if(count==0)
           array = (ModBaseData **) malloc(sizeof(ModBaseData *));
          else
           array = (ModBaseData **) realloc(array,
                   (size_t) (count+1) * sizeof(ModBaseData *));
          array[count] = new ModBaseData(num,
                   phd,x+i,shd,s,thd,t,zeit,z+i,pv+i);
          array[count]->setid(mid);
          i   = n;
          num = 0;
          count ++;
         }
        num++;
      }
     if(num != 0)
      {
       if(count==0)
        array = (ModBaseData **) malloc(sizeof(ModBaseData *));
       else
        array = (ModBaseData **) realloc(array,
                (size_t) (count + 1) * sizeof(ModBaseData *));
       array[count] = new ModBaseData(num,
                phd,x+i,shd,s,thd,t,zeit,z+i,pv+i);
       array[count]->setid(mid);
       count ++;
      }

  }

 *objarr = new ModBaseData *[count+1];
 memcpy(*objarr,array, (size_t) count*sizeof(ModBaseData *));
 (*objarr)[count]=NULL;
 free(array);

 return count;
}

SeisVectLinkedList *MdataToVectorList(ErsMaterials *mats,
                int phd, int shd, int thd, int zeit)
{ SeisVectLinkedList *vl;
  Vector **vectarr;
  ModBaseData **objarr;
  ErsMaterial *mat;
  int  nplotted;


  int   i,n,lstyle,mstyle,count;
  char  hname[32],cname[32];
  unsigned int   lwidth,mwidth,msize;

  if(mats==NULL) return NULL;
  if(materials_count(mats) < 1) return NULL;
  vl = new SeisVectLinkedList;
  if(vl==NULL) return NULL;
  lstyle = Vector::SolidLine;
  lwidth = 3;
  mstyle = Vector::FilledTriangleMarker;
  msize  = 2*lwidth + 3;
  mwidth = 1;
  nplotted=0;
  strcpy(cname,"green");
  for(i=0;i<materials_count(mats);i++)
   {mat = mats_get_nth(mats,i);
    if(mat==NULL) break;
    strcpy(cname, material_getcolor(mat));
    if(strlen(cname)==0) strcpy(cname,"green");
    strcpy(hname,material_getname(mat));
    count  = MaterialToBaseData(mat,&objarr,phd,shd,thd,zeit);
    if(count == 0)
     {delete objarr[0]; /* deletes pointer in array */
      delete []objarr; /* deletes array pointer */
     }
    else
     { 
       vectarr= new Vector *[count+1];
       materialSetVectA(mat, (void *) vectarr);
       for(n=0;n<count;n++)
        {vectarr[n] = vl->add(hname,objarr[n],cname,lwidth,False,
             (Vector::VectorStyle) lstyle,
             (Vector::VectorMarker) mstyle,msize,mwidth);
         vectarr[n]->makeVisible();
         materialSetVectA(mat, (void *) vectarr);
        }
      nplotted += count;
     }
   }

 if(nplotted == 0) return NULL;
 else return vl;
}

ErsMaterials *VectorListToMdata(void *vlm)
{SeisVectLinkedList *vlist;
 Vector *vect,**vectarr;
 ModBaseData *dobj;
 ErsMaterials *mats;
 ErsMaterial  *mat;
 int i,j,m,n,num,nsegs,npts;
 int mid;
 int ndof,*stype, *segid;
 float *x,*y,*z,*pv;
 char  name[96],color[64];
 char *names[199];

 ndof = 1;
 if(vlm == NULL) return NULL;
 vlist = (SeisVectLinkedList *) vlm;
 i = UniqueVectNameList(vlm ,&num ,199, names);
 if(i <= 0) return NULL;
 mats = new_materials();

 n= 0;
 while(names[n] != NULL)
  {nsegs = vlist->find(names[n],&vectarr);
   m = 0; npts = 0;
   while(vectarr[m] != NULL && m < nsegs)
    {dobj = (ModBaseData *) vectarr[m]->getData();
     npts += dobj->getNumPts();
     m++;
    }
   stype = (int *) calloc(1,npts*sizeof(float));
   segid = (int *) calloc(1,npts*sizeof(float));
   x = (float *) calloc(1,npts*sizeof(float));
   y = (float *) calloc(1,npts*sizeof(float));
   z = (float *) calloc(1,npts*sizeof(float));
   pv= (float *) calloc(1,npts*ndof*sizeof(float));

   if(nsegs > 0) mat = new_material();
   m = 0;
   npts = 0;
   while(vectarr[m] != NULL && m < nsegs)
    {vect = vectarr[m];
     dobj = (ModBaseData *) vect->getData();
     if(m==0)
      {strcpy(name,"dummy");
       if(vect->getName() != NULL) strcpy(name,vect->getName());
       strcpy(color,"green");
       if(vect->getColor() != NULL) strcpy(color,vect->getColor());
       mid  = dobj->getid();
      }
     for(i=npts;i<npts+dobj->getNumPts();i++) stype[i] = m+1;
     for(i=npts;i<npts+dobj->getNumPts();i++) segid[i] = 1;
     j = dobj->getNx(0,dobj->getNumPts(),x+npts);
     j = dobj->getNs(0,dobj->getNumPts(),y+npts);
     j = dobj->getNy(0,dobj->getNumPts(),z+npts);
     j = dobj->getNuser(0,dobj->getNumPts(),pv+npts);

     npts += dobj->getNumPts();

     m++;
    }
    material_set3(mat,ndof,mid,npts,stype,segid,x,y,z,pv);
    material_setname(mat,name);
    material_setcolor(mat,color);
    material_setid(mat,mid);
    materialSetVectA(mat,(void *) vectarr); /*small mem leak */
    materials_add(mats,mat);
    free(x);
    free(z);
    free(pv);
    free(stype);
   n++;
  }
 UniqueVectNameFree(names);
 return mats;
}

void MaterialSetVisible(ErsMaterial *mat,int visible)
{ Vector **vectarr;
  int n;
  if(mat == NULL) return;
  vectarr = (Vector **) materialGetVectA(mat);
  if(vectarr==NULL) return;
  n=0;
  while(vectarr[n] != NULL)
   {vectarr[n]->makeInvisible();
    if(visible != 0) vectarr[n]->makeVisible();
    else vectarr[n]->makeInvisible();
    n++;
   }
}

void MatSetAttributes(ErsMaterial *mat,int lstyle, unsigned int lwidth,
     int mstyle, unsigned int msize, unsigned int mwidth,char *color,
     int visible)
{Vector **vectarr;
 Bool   isvisible;
 int n;
 if(mat == NULL) return;
 vectarr = (Vector **) materialGetVectA(mat);
 if(vectarr==NULL) return;
  n=0;
  while(vectarr[n] != NULL)
   {isvisible = vectarr[n]->isVisible();
    vectarr[n]->makeInvisible();
    vectarr[n]->setColor(color);
    vectarr[n]->setWidth(lwidth);
    if(lstyle >= 0)
     vectarr[n]->setStyle((Vector::VectorStyle) lstyle);
    if(mstyle >= 0)
     vectarr[n]->setMarker((Vector::VectorMarker) mstyle,msize,mwidth);
    if(visible != 0) vectarr[n]->makeVisible();
    else vectarr[n]->makeInvisible();
    n++;
   }
 material_setcolor(mat,color);
}

