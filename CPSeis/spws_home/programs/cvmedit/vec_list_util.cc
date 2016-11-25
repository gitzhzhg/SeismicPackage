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
#include <stdlib.h>
#include "vec_list_util.hh"
#include "model.h"
#include "uvect.hh"

 
void  KillVectorList( void *vl,void *sp)
{Vector *vect;
 ModBaseData *dobj;
 SeisPlot *splot;
 SeisVectLinkedList *vlist;
 void *p;
 int n=0;
 vlist = (SeisVectLinkedList *) vl;
 if(vlist == NULL) return;
 splot = (SeisPlot *) sp;
 if(vlist != NULL) vlist->removePlot( splot);
  vect = vlist->top(&p);
  while(vect != NULL)
   { dobj = (ModBaseData *) vect->getData();
     if(dobj != NULL) delete dobj;
     vect = vlist->next(&p);
     n++;
   }
  delete vlist;
// printf("KillVectorList: No. of vectors deleted=%d\n",n);
}

void VectorListSetVis(void *list,int vis)
{
 SeisVectLinkedList *vlist;
 

 vlist = (SeisVectLinkedList *) list;
 if(vlist == NULL) return;
 if(vis) vlist->makeVisible();
 else vlist->makeInvisible();
}
void VectorListSetVisByID(void *list,int id,int vis)
{Vector *vect;
 ModBaseData *dobj;
 SeisVectLinkedList *vlist;
 void *p;
 int n=0;
 vlist = (SeisVectLinkedList *) list;
 if(vlist == NULL) return;
 vect = vlist->top(&p);
 while(vect != NULL)
   { dobj = (ModBaseData *) vect->getData();
     if(dobj->getid()==id)
      {if(vis)
        vect->makeVisible();
       else
        vect->makeInvisible();
      }
     vect = vlist->next(&p);
     n++;
   }

}
void VectorListSetVisByName(void *list,char *name,int vis)
{Vector *vect;

 SeisVectLinkedList *vlist;
 void *p;
 int n=0;
 vlist = (SeisVectLinkedList *) list;
 if(vlist == NULL || name == NULL) return;
 if(strlen(name)==0) return;
 vect = vlist->top(&p);
 while(vect != NULL)
   {if(strcmp(name,vect->getName())== 0)
     {if(vis)
       vect->makeVisible();
       else
        vect->makeInvisible();
      }
     vect = vlist->next(&p);
     n++;
   }
}

int VectorListCount(void *list)
{// return the vector count for the list
 VectorLinkedList *vll = (VectorLinkedList *) list;
 Vector *vect;
 void *p;
 int n=0;
 if(vll == NULL) return n;
 vect = vll->top(&p);
 while(vect != NULL)
   { vect = vll->next(&p);
     n++;
   }
 return n;
}

int VectorListPointCount(void  *list)
{// return the total point count for the list
 VectorLinkedList *vll = (VectorLinkedList *) list;
 Vector      *vect;
 ModBaseData *dobj;
 void        *p;
 int          ptcnt=0;
 if(vll == NULL) return ptcnt;
 vect = vll->top(&p);
 while(vect != NULL)
   { dobj = (ModBaseData *) vect->getData();
     if(dobj) ptcnt += dobj->getNumPts();
     vect = vll->next(&p);
   }
 return ptcnt;
}

void  VectorListSetCoord(VectorLinkedList *vll, int comp, int hdr)
{// Set the header id flag for a data component
 Vector      *vect;
 ModBaseData *dobj;
 void        *p;
 if(!vll) return;
 vect = vll->top(&p);
 while(vect != NULL)
   { dobj = (ModBaseData *) vect->getData();
     if(dobj)
      {  if(comp==ModBaseData::primary) dobj->setphdr(hdr);
         if(comp==ModBaseData::secondary) dobj->setshdr(hdr);
         if(comp==ModBaseData::tertiary) dobj->setthdr(hdr);
         if(comp==ModBaseData::time) dobj->setzeit(hdr);
      }
     vect = vll->next(&p);
   }
 return;
}

void VectorListSetIdent(void *list, int ident)
{// reset the vector ident flags
 VectorLinkedList *vll = (VectorLinkedList *) list;
 Vector *vect;
 void *p;
 long new_ident = ident;
 if(vll == NULL) return;
 vect = vll->top(&p);
 while(vect != NULL)
   { vect = vll->next(&p);
     vect->setId(new_ident);
   }
}


void VectorListResetID(void *list,int old_id,int new_id)
{Vector *vect;
 ModBaseData *dobj;
 SeisVectLinkedList *vlist;
 void *p;
 vlist = (SeisVectLinkedList *) list;
 if(vlist == NULL) return;
 vect = vlist->top(&p);
 while(vect != NULL)
   { dobj = (ModBaseData *) vect->getData();
     if(dobj->getid()==old_id) dobj->setid(new_id);
     vect = vlist->next(&p);
   }

}

void VectorListResetLabel(void *list,int old_id,int new_id)
{Vector *vect;
 ModBaseData *dobj;
 SeisVectLinkedList *vlist;
 char font[72],str[16];
 void *p;
 strcpy(font,"-adobe-*-bold-r-*--20-*-*-*-m-*-iso8859-1");
 vlist = (SeisVectLinkedList *) list;
 if(vlist == NULL) return;
 if(old_id == new_id) return;
 vect = vlist->top(&p);
 while(vect != NULL)
   { dobj = (ModBaseData *) vect->getData();
     if(dobj->getid()==old_id)
      { dobj->setid(new_id);
        sprintf(str,"%d",new_id);
        vect->setLabel(str,font);
      }
     vect = vlist->next(&p);
   }

}

int VectorList3DTransform(void *list, ErsTransforms *tdata,
 char *xname, char *yname, char *zname)
{// Transform to the output coordinate systems labeled
 // by xname,yname,zname.
 SeisVectLinkedList *vlist = (SeisVectLinkedList *) list;
 Vector      *vector;
 ModBaseData *dobj;
 void   *p;
 int    i, old_vis;
 
 if(list==NULL || tdata==NULL) return 0;
 if(xname==NULL || zname==NULL) return 0;
// VectorListSetVis(vlist,0);
 vector = vlist->top(&p);
 while(vector != NULL)
  {old_vis = 0;
   if(vector->isVisible()) old_vis = 1;
   vector->makeInvisible();
   dobj = (ModBaseData *) vector->getData();
   i = dobj->trans_data_comps(tdata,xname,yname,NULL,zname);
   if(old_vis) vector->makeVisible();
   vector = vlist->next(&p);
  }
//VectorListSetVis(vlist,1);
 return 1;
}


Vector *VectorListAdd(VectorLinkedList *vll,char *hname,char *cname,
        int phd, int shd, int thd, int zeit,
        int id, float xo, float yo, float zo,
        int n, float *x,float *y, float *z)
{
 ModBaseData *dobj;
 Vector      *vect=NULL;
 int          lstyle,mstyle;
 unsigned int lwidth=3,msize,mwidth=1;

 if(!vll) return NULL;
 dobj = new ModBaseData((int) n,phd,x,shd,y,thd,NULL,zeit,z,NULL);
 dobj->setid((int) id);
 dobj->setpt(xo,zo,yo);
 msize  = 2*lwidth+5;
 lstyle = Vector::SolidLine;
 mstyle = Vector::NoMarker;
 vect   = vll->add(hname,dobj,cname,lwidth,False,
           (Vector::VectorStyle) lstyle,
           (Vector::VectorMarker) mstyle,msize,mwidth);
 return vect;
}

int VectorListMaccess(void *vlm, int *rdof, int *nvcrd,
      int *nmats, int **rid, int **mpntr, int **mcntr,
      int **hflag, float **xv, float **yv, float **zv, float **pv)
{// Convert data in vector list of materials to arrays.
 // Return the total point count for all vectors.
 VectorLinkedList *vlist = (VectorLinkedList *) vlm;
 Vector *vect,**vectarr;
 ModBaseData  *dobj;
 int i,j,m,n,count,nsegs,npts;

 int ndof=1;

 int   *lp;
 float *fp;

 char  *names[199];

 *nvcrd = 0;
 *nmats = 0;
 *rdof  = ndof; // need to remove this restriction
 if(vlm == NULL) return 0;
// Total point-count in segments.
 *nvcrd = (int) VectorListPointCount(vlist);

// Determine number of horizons and names.
 *nmats = UVectNameList(vlm ,(int *) nmats ,199, names);
 if(*nmats <= 0) return 0;

// Allocate memory for arrays.
 *rid   = (int *) malloc((unsigned int) *nmats*sizeof(int));
 *mpntr = (int *) malloc((unsigned int) *nmats*sizeof(int));
 *mcntr = (int *) malloc((unsigned int) *nmats*sizeof(int));
 *hflag = (int *) malloc((unsigned int) *nvcrd*sizeof(int));
 *xv    = (float *) malloc((unsigned int) *nvcrd*sizeof(float));
 *yv    = (float *) malloc((unsigned int) *nvcrd*sizeof(float));
 *zv    = (float *) malloc((unsigned int) *nvcrd*sizeof(float));
 *pv    = (float *) malloc((unsigned int) *nvcrd*1*sizeof(float));
 n= 0;
 count = 0;
 while(names[n] != NULL)
  {
   nsegs = vlist->find(names[n],&vectarr);

   m = 0;
   npts = 0;
   while(vectarr[m] != NULL && m < nsegs)
    {vect = vectarr[m];
     dobj = (ModBaseData *) vect->getData();
     if(m==0) (*rid)[n]  = dobj->getid();
     lp = (*hflag) + count;
     for(i=npts;i<npts+dobj->getNumPts();i++) lp[i] = m+1;
     fp = (*xv) + count + npts;
     j = dobj->getNx(0,dobj->getNumPts(),fp);
     fp = (*zv) + count + npts;
     j = dobj->getNy(0,dobj->getNumPts(),fp);
     fp = (*yv) + count + npts;
     j = dobj->getNs(0,dobj->getNumPts(),fp);
     fp = (*pv) + count + npts;
     j = dobj->getNuser(0,dobj->getNumPts(),fp);

     npts += dobj->getNumPts();

     m++;
    }
   (*mpntr)[n] = (n>0) ? count  : 0 ;
   count += npts;
   (*mcntr)[n] = npts;
   n++;
   delete []vectarr;
  }
 UVectNameFree(names);
 return count;

}

                                //rdof
int VectorListMreset(void *vlm, int , int nvcrd,
      int nmats, int *rid, int *mpntr, int *,
      int *, float *xv, float *yv, float *zv, float *pv)
{// Convert data in arrays to vector list of materials.
 // Return the total point count for all vectors.
 VectorLinkedList *vlist = (VectorLinkedList *) vlm;
 Vector *vect,**vectarr;
 ModBaseData  *dobj;
 int i,m,n,count=0,nsegs,npts;
 int nhor,istat=0,offset;

 void  *p;
 char  *names[199];

 if(vlm == NULL) return istat;
//    Total point-count in segments.
 vect = vlist->top(&p);
 while(vect != NULL)
  { dobj = (ModBaseData *) vect->getData();
    if(dobj) count  += dobj->getNumPts();
    vect = vlist->next(&p);
  }
//    Determine number of horizons and names.
 i = UVectNameList(vlm ,(int *) &nhor ,199, names);
//    Check that arrays are isomorphic to the VectorLinkedList.
 if(nmats != nhor || count != nvcrd) goto error;

//    Map the arrays back into the VectorLinkedList.
 n= 0;
 while(names[n] != NULL)
  {nsegs = vlist->find(names[n],&vectarr);

   m = 0;
   npts = 0;
   offset = (int) mpntr[n];
   while(vectarr[m] != NULL && m < nsegs)
    {vect = vectarr[m];
     dobj = (ModBaseData *) vect->getData();
     if(dobj) npts = dobj->getNumPts();
     if(rid[n] == dobj->getid())
      {
       dobj->replace(0, npts, &xv[offset], &zv[offset],
                 &yv[offset], NULL, &pv[offset]);
      }
     else {delete []vectarr; goto error; }

     offset += npts;
     m++;
    }
   n++;
   delete []vectarr;
  }
 istat = 1;
error:
 UVectNameFree(names);
 return istat;

}

int VectorListRefill(VectorLinkedList *vlist, int *nrep,
      int nseg, int *pntr, int *cntr,
      float *x, float *y, float *z, float *udata, int islab)
{// Place data in arrays back into linked lists.
 // For use after transformations when there is
 // still an isomorphic mapping to the linked list.
 // Return total number of points replaced.
 Vector *vect;
 ModBaseData  *dobj;
 int i=0,n,count=0;
 int offset;
 void  *p;

 *nrep = 0;
 if(vlist == NULL) return 0;
 if(nseg != VectorListCount((void *) vlist)) return 0;
//
 vect = vlist->top(&p);
 while(vect != NULL)
  { dobj = (ModBaseData *) vect->getData();
    n=0;
    if(dobj) n  = dobj->getNumPts();
    offset=pntr[i];
    if(n==cntr[i])
      {// number of points agrees.
       if(udata)
        {
         if(islab!= 0)
          dobj->setpt(x[offset],y[offset],z[offset]);
         dobj->replace(0, n, x+offset, z+offset,
                y+offset, NULL, udata+offset);
        }
       else
        {
         if(islab!= 0)
          dobj->setpt(x[offset],y[offset],z[offset]);
         dobj->replace(0, n, x+offset, z+offset, y+offset, NULL, NULL);
        }
       count += n;
      }
    else
      {// Uh Oh!
      }
    i++;
    vect = vlist->next(&p);
  }
 *nrep = i;
 return count;
}

int VectorListCPaccess(void *list,int *nump,int **id,
     float **xp,float **yp, float **zp)
{
 VectorLinkedList *vlist = (VectorLinkedList *) list;
 Vector       *vect;
 ModBaseData  *dobj;
 int   n=0;
 int   count=0;
 void  *p;
 *nump = 0;
 if(list == NULL) return 0;
 *nump = (int) VectorListCount(list);
 if(*nump<=0) return 0;

 *id = (int *)  malloc((unsigned int) *nump*sizeof(int));
 *xp = (float *) malloc(*nump*sizeof(float));
 *yp = (float *) malloc(*nump*sizeof(float));
 *zp = (float *) malloc(*nump*sizeof(float));

 n=0;          // vector count
 count = 0;    // count of vectors with data
 vect = vlist->top(&p);
 while(vect != NULL)
  { dobj = (ModBaseData *) vect->getData();
    if(dobj)
     { (*id)[count]  = dobj->getid();
       dobj->getpt(*xp+count, *zp+count, *yp+count);
       count++;
     }
    n++;
    vect = vlist->next(&p);
  }
 *nump = count;
 return count;
}

int VectorListCaccess(void *list,int *ncell,int **id,int **ipntr,
     int **icntr,float **xcell,float **ycell, float **zcell)
{// Convert data in vector list of cell boundaries to arrays.
 // Return the cell count for all vectors.
 VectorLinkedList *vlist = (VectorLinkedList *) list;
 Vector *vect;
 ModBaseData  *dobj;
 int   j,n=0,npts;
 int   count=0;
 float *fp;
 void *p;
 *ncell = 0;
 if(list == NULL) return 0;
 *ncell = (int) VectorListCount(list);
 if(*ncell<=0) return 0;

// Total point-count in segments.
 count = VectorListPointCount(vlist);

 *ipntr = (int *) malloc((unsigned int) *ncell*sizeof(int));
 *icntr = (int *) malloc((unsigned int) *ncell*sizeof(int));
 *id    = (int *) malloc((unsigned int) *ncell*sizeof(int));
 *xcell = (float *) malloc(count*sizeof(float));
 *ycell = (float *) malloc(count*sizeof(float));
 *zcell = (float *) malloc(count*sizeof(float));

 n=0;
 count = 0;
 (*ipntr)[n] = 0;
 vect = vlist->top(&p);
 while(vect != NULL)
  { dobj = (ModBaseData *) vect->getData();
    npts = 0;
    if(dobj)
     { npts = dobj->getNumPts();
       (*id)[n]  = dobj->getid();
       (*icntr)[n] = npts;
        fp = (*xcell) + count;
        j = dobj->getNx(0,npts,fp);
        fp = (*ycell) + count;
        j = dobj->getNs(0,npts,fp);
        fp = (*zcell) + count;
        j = dobj->getNy(0,npts,fp);
     }

    (*ipntr)[n] = count;
    count += npts;
    n++;
    vect = vlist->next(&p);
  }

 return VectorListCount(list);
}

int VectorListSaccess(void *list,int *nvec,int **ipntr,
     int **icntr,float **xval,float **yval, float **zval)
{// Convert vector list of structure boundaries to arrays.
 VectorLinkedList *vll = (VectorLinkedList *) list;
 Vector           *vect;
 ModBaseData      *dobj;
 int              id;
 int               j,n=0,npts;
 int               count=0;
 float            *fp;
 void *p;

// Total count of segments.
 *nvec = (int) VectorListCount(list);
 if(*nvec<=0) return 0;
// Total point-count in segments.
 count = VectorListPointCount(vll);

 *ipntr = (int *) malloc((unsigned int) *nvec*sizeof(int));
 *icntr = (int *) malloc((unsigned int) *nvec*sizeof(int));
 *xval = (float *) malloc(count*sizeof(float));
 *zval = (float *) malloc(count*sizeof(float));
 *yval = (float *) malloc(count*sizeof(float));

 n=0;
 count = 0;
 (*ipntr)[n] = 0;
 vect = vll->top(&p);
 while(vect != NULL)
  { dobj = (ModBaseData *) vect->getData();
    npts = 0;
    if(dobj)
     { npts = dobj->getNumPts();
       id  = dobj->getid();
       (*icntr)[n] = npts;
        fp = (*xval) + count;
        j = dobj->getNx(0,npts,fp);
        fp = (*zval) + count;
        j = dobj->getNy(0,npts,fp);
        fp = (*yval) + count;
        j = dobj->getNs(0,npts,fp);
     }

    (*ipntr)[n] = count;
    count += npts;
    n++;
    vect = vll->next(&p);
  }

 return VectorListCount(vll);
}

int VectorListAccess(VectorLinkedList *vll,int *cnt, int **pntr,
     int **cntr,float **x,float **y, float **z, float **udata)
{// Convert vector linked list to arrays.
 // Retrieve coordinates and user data. Return point count.
 Vector           *vect;
 ModBaseData      *dobj;
 int              id,j,n=0,npts,count=0;
 float            *fp;
 void *p;

 *pntr=NULL;
 *cntr=NULL;
 *x=NULL;
 *y=NULL;
 *z=NULL;
 *udata=NULL;
 *cnt = 0;
 if(!vll) return 0;
// Total count of segments.
 *cnt = (int) VectorListCount(vll);
 if(*cnt<=0) return 0;
// Total point-count in segments.
 count = VectorListPointCount(vll);

 *pntr = (int *) malloc((unsigned int) *cnt*sizeof(int));
 *cntr = (int *) malloc((unsigned int) *cnt*sizeof(int));
 *x = (float *) malloc(count*sizeof(float));
 *y = (float *) malloc(count*sizeof(float));
 *z = (float *) malloc(count*sizeof(float));
 vect = vll->top(&p);
 dobj = (ModBaseData *) vect->getData();
 if(dobj->isUserData())
  *udata = (float *) malloc(count*sizeof(float));

 count=0;
 (*pntr)[n] = 0;
 while(vect != NULL)
  { dobj = (ModBaseData *) vect->getData();
    npts = 0;
    if(dobj)
     { npts = dobj->getNumPts();
       id  = dobj->getid();
       (*cntr)[n] = npts;
        fp = (*x) + count;
        j = dobj->getNx(0,npts,fp);
        fp = (*z) + count;
        j = dobj->getNy(0,npts,fp);
        fp = (*y) + count; *fp=0;
        j = dobj->getNs(0,npts,fp);
        if(dobj->isUserData())
         {
          fp = (*udata) + count;
          j = dobj->getNuser(0,npts,fp);
         }
     }

    (*pntr)[n] = count;
    count += npts;
    n++;
    vect = vll->next(&p);
  }

 return count;
}
