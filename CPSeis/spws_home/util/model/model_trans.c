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
#include "model_trans.h"

/****************************************************
 * Method to transform a 2D ErsMaterials object   ***
 ***************************************************/
int ErsMaterials_trans(ErsMaterials *mats, ErsTransforms *tdata,
 char *xname, char *zname)
{ErsTransform *to[3],*ti[3];
 ErsMaterial *mat;
 int   i,ierr,istat;

 if(mats==NULL || tdata==NULL) return 1;
 if(xname==NULL || zname==NULL) return 1;
 ti[0] = mats_get_transx(mats);
 ti[1] = mats_get_transy(mats);
 ti[2] = mats_get_transz(mats);
 to[0] = ErsTransGetTran(tdata,xname);
 to[1] = ti[1];
 to[2] = ErsTransGetTran(tdata,zname);
 istat = 0;
 for(i=0;i<materials_count(mats);i++)
  {mat  = mats_get_nth(mats,i);
   ierr = transform_mcnvrt(ti[0],mat->x,material_getnpts(mat),to[0]);
   istat += ierr;
   ierr = transform_mcnvrt(ti[1],mat->y,material_getnpts(mat),to[1]);
   istat += ierr;
   ierr = transform_mcnvrt(ti[2],mat->z,material_getnpts(mat),to[2]);
   istat += ierr;
  }
 if(istat==0)
  {if(to[0] != NULL) mats_set_transx(mats,to[0]);
   if(to[1] != NULL) mats_set_transy(mats,to[1]);
   if(to[2] != NULL) mats_set_transz(mats,to[2]);
  }
 return istat;
}
void ErsMaterials_prt(ErsMaterials *mdata)
{ErsTransform *tx,*ty,*tz;
 if(mdata == NULL) return;
 tx= mats_get_transx(mdata);
 ty= mats_get_transz(mdata);
 tz= mats_get_transz(mdata);
 printf(" ErsMaterials_prt: Y-Name=%s, Y-Name=%s Z-Name=%s\n",
        transform_getname(tx),transform_getname(ty),transform_getname(tz));
 printf(" ErsMaterials_prt:  Number of Materials =%d\n",
        materials_count(mdata));

}

/****************************************************
 * Method to transform a 2D ErsCells object       ***
 ***************************************************/
int ErsCells_trans(ErsCells *cells, ErsTransforms *tdata,
 char *xname, char *zname)
{ErsTransform *to[3],*ti[3];
 ErsCell *cell;
 long    npts,cell_id;
 float   *xb,*zb,*xi,*zi;
 int   i,ierr,istat;
 int   one=1;
 if(cells==NULL || tdata==NULL) return 1;
 if(xname==NULL || zname==NULL) return 1;
 ti[0] = cells_get_transx(cells);
 ti[1] = cells_get_transy(cells);
 ti[2] = cells_get_transz(cells);
 to[0] = ErsTransGetTran(tdata,xname);
 to[1] = ti[1];
 to[2] = ErsTransGetTran(tdata,zname);
 istat = 0;
 for(i=0;i<cells_count(cells);i++)
  {cell = cells_get_nth(cells,i);
   ierr = transform_mcnvrt(ti[0],cell->xc,cell->nbndpt,to[0]);
   istat += ierr;
   ierr = transform_mcnvrt(ti[1],cell->yc,cell->nbndpt,to[1]);
   istat += ierr;
   ierr = transform_mcnvrt(ti[2],cell->zc,cell->nbndpt,to[2]);
   istat += ierr;
   ierr = transform_mcnvrt(ti[0],&cell->xin,one,to[0]);
   istat += ierr;
   ierr = transform_mcnvrt(ti[1],&cell->yin,one,to[1]);
   istat += ierr;
   ierr = transform_mcnvrt(ti[2],&cell->zin,one,to[2]);
   istat += ierr;
  }
 if(istat==0)
  {if(to[0] != NULL) cells_set_transx(cells,to[0]);
   if(to[1] != NULL) cells_set_transy(cells,to[1]);
   if(to[2] != NULL) cells_set_transz(cells,to[2]);
  }
 return istat;
}

/* Method to transform PR_ object(i.e. a Picking Record) */
/* Get the destination transforms */
/* Get the transforms currently used */
int PR_trans(PR_ *dat, ErsTransforms *tdata,
 char *pname, char *sname, char *tname, char *zname)
{ErsTransform *to[4],*ti[4];
 ErsHorizon   *horiz;
 ErsPoint     *point;
 int          pkey,skey,tkey;
 int          i,ierr,istat,nhor;
 char         cpname[32],csname[32],ctname[32],czname[32];

 if(dat==NULL || tdata==NULL) return 1;
 to[0] = NULL; to[1] = NULL; to[2] = NULL; to[3] = NULL;
 ti[0] = NULL; ti[1] = NULL; ti[2] = NULL; ti[3] = NULL;
 if(pname != NULL) to[0] = ErsTransGetTran(tdata,pname);
 if(sname != NULL) to[1] = ErsTransGetTran(tdata,sname);
 if(tname != NULL) to[2] = ErsTransGetTran(tdata,tname);
 if(zname != NULL) to[3] = ErsTransGetTran(tdata,zname);
 pkey = PR_GetPhdr(dat);
 if(pkey != UNDEFINED_KEY)
  {ErsTransName(pkey,cpname);
   ti[0] = ErsTransGetTran(tdata,cpname);
  }
 skey = PR_GetShdr(dat);
 if(skey != UNDEFINED_KEY)
  {ErsTransName(skey,csname);
   ti[1] = ErsTransGetTran(tdata,csname);
  }
 tkey = PR_GetThdr(dat);
 if(tkey != UNDEFINED_KEY)
  {ErsTransName(tkey,ctname);
   ti[2] = ErsTransGetTran(tdata,ctname);
  }
 ti[3] = dat->transz;
 istat = 0;
 for(i=0;i<PR_HorizonCount(dat);i++)
  { istat = 0;
    horiz = ErsHorizonGet(dat,i+1);
    istat = ErsHorizon_trans(horiz,ti,to);
    if(istat != 0) break;
  }
 if(istat == 0)
  {
   if(ti[0] != NULL && to[0] != NULL)
    {ErsTransHeader(&dat->Phdr,transform_getname(to[0]));
     dat->transx = to[0];
    }
   if(ti[1] != NULL && to[1] != NULL)
    ErsTransHeader(&dat->Shdr,transform_getname(to[1]));
   if(ti[2] != NULL && to[2] != NULL)
    ErsTransHeader(&dat->Thdr,transform_getname(to[2]));
   if(ti[3] != NULL && to[3] != NULL) dat->transz = to[3];
  }
 return istat;
}

int ErsHorizon_trans(ErsHorizon *horizon,
     ErsTransform **ti, ErsTransform **to)
{ErsPoint *point;
 int ierr;
 int  one=1;
 point = horizon->first_point;
 ierr = 0;
 while(point!= NULL)
  {ierr = ErsPoint_trans(point,ti,to);
   point = point->next;
   if(ierr != 0) return ierr;
  }

 ErsHorizonMinMax(horizon,&horizon->min_pkey,&horizon->max_pkey,
     &horizon->min_time, &horizon->max_time);
 return ierr;
}

int ErsPoint_trans(ErsPoint *point, ErsTransform **ti, ErsTransform **to)
{/* ti[4], and to[4] define the transforms */
 /* index 0 = primary key */
 /* index 1 = secondary key */
 /* index 2 = tertiary  key */
 /* index 3 = z-t key */
 int ierr;
 int  one=1,i;
 if(point==NULL) return 1;
 if(ti == NULL || to == NULL) return 1;
 ierr  = 0;
 if(ti[0] != NULL && to[0] != NULL)
  { ierr += transform_mcnvrt(ti[0],&point->pkey,one,to[0]);}
 if(ti[1] != NULL && to[1] != NULL)
  { ierr += transform_mcnvrt(ti[1],&point->skey,one,to[1]);}
 if(ti[2] != NULL && to[2] != NULL)
  { ierr += transform_mcnvrt(ti[2],&point->tkey,one,to[2]);}
 if(ti[3] != NULL && to[3] != NULL)
  { ierr += transform_mcnvrt(ti[3],&point->time,one,to[3]);}
 if(ierr != 0) return ierr;
 if(point->npicks > 0 && point->pick_list != NULL)
  {for(i=0;i<point->npicks;i++)
    {
     if(ti[0] != NULL && to[0] != NULL)
      ierr += transform_mcnvrt(ti[0],&point->pick_list[i].pkey,one,to[0]);
     if(ti[1] != NULL && to[1] != NULL)
      ierr += transform_mcnvrt(ti[1],&point->pick_list[i].skey,one,to[1]);
     if(ti[2] != NULL && to[2] != NULL)
      ierr += transform_mcnvrt(ti[2],&point->pick_list[i].tkey,one,to[2]);
     if(ti[3] != NULL && to[3] != NULL)
      ierr += transform_mcnvrt(ti[3],&point->pick_list[i].time,one,to[3]);
     if(ierr != 0) return ierr;
    }
  }
 return ierr;
}
