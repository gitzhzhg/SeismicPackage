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
#include "oprim/modbase_data.hh"
#include "vect/ll_seis_vect.hh"
#include "vl_data.hh"
#include "coord_data.hh"
#include "pick.h"
#include "draw_mod.h"
#include "model_desc.hh"
#include "grid_data.hh"
#include "vec_list_util.hh"

ModelDesc::ModelDesc()
{
  setup();
}

ModelDesc::ModelDesc(ErsModel *ersmod)
{
 /* Constructor to build ModelDesc given an ErsModel
    Copy the data so that we can destroy the ErsModel
    without repercussions if we want to.
  */
 SeisVectLinkedList *piklist,*cbndlist,*cpntrlist,*mdatalist;
 ModBaseData   *dobj;
 Vector        *vector;
 PR_           *prdat;
 ErsMaterials  *mats;
 ErsCells      *cells;
 ErsTransforms *tdata;
 ErsTransform  *tx, *ty,*tz;
 char          *xn=NULL,*yn=NULL,*zn=NULL;
// Default Coordinates - see transform.h for definitions
 int           phd=XBASEHDR_,shd=YBASEHDR_,thd=NILKEY,zeit=TIMEHDR_;
 int           num=0;
 float         x,s,t,z,v;

 setup();

 if(!ersmod) return;
 model_gettrans(ersmod,&tx,&ty,&tz);
 xn = transform_getname(tx);
 yn = transform_getname(ty);
 zn = transform_getname(tz);
 tdata = transforms();
 tx = ErsTransGetTran(tdata,xn);
 ty = ErsTransGetTran(tdata,yn);
 tz = ErsTransGetTran(tdata,zn);

 // Copy data 
 transforms_copy(model_gettdata(ersmod), tdata);
 memcpy(_mlim, model_getmlim(ersmod), sizeof(ModLimits));
 mlimits_set_trans(_mlim,tx,ty,tz);
 memcpy(_glim, model_getglim(ersmod), sizeof(GridLimits));
 glimits_set_trans(_glim,tz,tx,ty);
 if(model_getname(ersmod)) setName(model_getname(ersmod));

 // Convert ErsModel data to vector lists.
 prdat = (PR_ *) model_getpdata(ersmod);
 mats  = model_getmdata(ersmod);
 cells = model_getcdata(ersmod);

 if(tx) phd = (int ) transform_gethdr(tx);
 if(ty) shd = (int ) transform_gethdr(ty);
 if(tz) zeit= (int ) transform_gethdr(tz);
 piklist   = PR_ToVectorList(prdat,   phd,shd,thd,zeit);
 if(!piklist) // provide some initial data if needed
  { piklist = new SeisVectLinkedList;
    dobj = new ModBaseData(num,phd,&x,shd,&s,thd,&t,zeit,&z,&v);
    dobj->setid(1);
    vector = piklist->add("HORIZ1",dobj,"red",3,False,
             Vector::SolidLine, Vector::NoMarker,9,1);
  }
 mdatalist = MdataToVectorList(mats,  phd,shd,thd,zeit);
 if(!mdatalist) // provide some initial data if needed
  { mdatalist = new SeisVectLinkedList;
    dobj = new ModBaseData(num,phd,&x,shd,&s,thd,&t,zeit,&z,&v);
    dobj->setid(1);
    vector = mdatalist->add("MAT1",dobj,"blue",3,False,
             Vector::SolidLine, Vector::FilledTriangleMarker,9,1);
  }

 cbndlist  = CellsToVectorList(cells, phd,shd,thd,zeit);
 if(!cbndlist) // provide some initial data if needed
  { cbndlist = new SeisVectLinkedList;
    dobj = new ModBaseData(num,phd,&x,shd,&s,thd,&t,zeit,&z,&v);
    dobj->setid(1);
    vector = cbndlist->add("CELL1",dobj,"green",3,False,
             Vector::SolidLine, Vector::NoMarker,9,1);
  }

 cpntrlist = CellsToVectorList2(cells,phd,shd,thd,zeit);
 if(!cpntrlist) // provide some initial data if needed
  { cpntrlist = new SeisVectLinkedList;
    dobj = new ModBaseData(num,phd,&x,shd,&s,thd,&t,zeit,&z,&v);
    dobj->setid(1);
    vector = cpntrlist->add("CLAB1",dobj,"green",3,False,
             Vector::SolidLine, Vector::FilledTriangleMarker,9,1);
  }

 _sdata = new VectorListData((VectorLinkedList *) piklist,NULL);
 _mdata = new VectorListData((VectorLinkedList *) mdatalist,NULL);
 _pdata = new VectorListData((VectorLinkedList *) cpntrlist,NULL);
 _bdata = new VectorListData((VectorLinkedList *) cbndlist,NULL);
 
}

void ModelDesc::setup()
{
 ErsTransforms *tdata=NULL;
 _name = NULL;
 _csys_data=NULL;
 _grid_data=NULL;
 _mlim =NULL;
 _glim =NULL;
 _pdata=NULL;
 _sdata=NULL;
 _mdata=NULL;
 _bdata=NULL;

 tdata = new_transforms();
 transforms_adddef(tdata);
 _csys_data = new CoordData(tdata);
 _mlim = new_mlimits();
 _glim = new_glimits();
 mlimits_setdef(_mlim, tdata);
 glimits_setdef(_glim, _mlim);
}

ModelDesc::~ModelDesc()
{ErsTransforms *tdata=0;
 destroy_mlimits(_mlim);
 _mlim =NULL;
 destroy_glimits(_glim);
 _glim =NULL;
 tdata = transforms();
 if(tdata) free(tdata);
 delete _csys_data;
 _csys_data=NULL;

// The following deletes VectorListData but not
// the VectorLinkedList
 delete _pdata;
 _pdata=NULL;
 delete _sdata;
 _sdata=NULL;
 delete _mdata;
 _mdata=NULL;
 delete _bdata;
 _bdata=NULL;
 if(_name) free(_name);
}

void ModelDesc::setName(char *name)
{
 if(!name) return;
 if(strlen(name) > 0)
   {if(_name) free(_name);
    _name = (char *) malloc(strlen(name)+1);
    strcpy(_name,name);
   }
}

void ModelDesc::setVLDComponent(int id, VectorListData *vld)
{
 if(id==MOD_STRUC) _sdata=vld;
 if(id==MOD_CELLP) _pdata=vld;
 if(id==MOD_CELLB) _bdata=vld;
 if(id==MOD_MDATA) _mdata=vld;
}

VectorListData *ModelDesc::VLDComponent(int id)
{
 if(id==MOD_STRUC) return _sdata;
 if(id==MOD_CELLP) return _pdata;
 if(id==MOD_CELLB) return _bdata;
 if(id==MOD_MDATA) return _mdata;
 return NULL;
}

VectorLinkedList *ModelDesc::VLLComponent(int id)
{
 if(id==MOD_STRUC && _sdata) return _sdata->getDataObject();
 if(id==MOD_CELLP && _pdata) return _pdata->getDataObject();
 if(id==MOD_CELLB && _bdata) return _bdata->getDataObject();
 if(id==MOD_MDATA && _mdata) return _mdata->getDataObject();
 return NULL;
}

ErsTransforms *ModelDesc::transforms()
{
 if(_csys_data) return _csys_data->getDataObject();
 return NULL;
}

void ModelDesc::gettrans(ErsTransform **tx,ErsTransform  **ty,
     ErsTransform **tz)
{ 
 *tx=NULL; *ty=NULL; *tz=NULL;
 if(!_mlim) return;
 mlimits_get_trans(_mlim,tx,ty,tz);
}

int  ModelDesc::transform(char *xname, char *yname, char *zname)
{// Transform the data to a new coordinate system
 ErsTransforms *tdata;
 int stat;
 if(!xname) return 0;
 if(!yname) return 0;
 if(!zname) return 0;
 tdata=transforms();
 if(!tdata) return 0;
 stat = mlimits_trans(_mlim,tdata,xname,yname,zname);
 if(stat != 0) return 0;
 stat = glimits_trans(_glim,tdata,xname,yname,zname);
 if(_sdata) _sdata->transform(tdata,xname,yname,zname);
 if(_pdata) _pdata->transform(tdata,xname,yname,zname);
 if(_bdata) _bdata->transform(tdata,xname,yname,zname);
 if(_mdata) _mdata->transform(tdata,xname,yname,zname);
 modDone(); //signal everyone that changes are done

 return 1;
}

ErsModel *ModelDesc::toErsModel()
{ //Creates new ErsModel and fills it with data.
  ErsModel      *ersmod=NULL;
  VectorLinkedList *vll;
  ErsTransforms *tdata=NULL;
  ErsTransform  *tx=NULL,*ty=NULL,*tz=NULL;
  char          *xn=NULL,*yn=NULL,*zn=NULL;
  PR_           *pr;
  ErsCells      *cells;
  ErsMaterials  *mats;
  int            nv;

  ersmod = new_model();
  if(!ersmod) return ersmod;

  gettrans(&tx,&ty,&tz);
  xn = transform_getname(tx);
  yn = transform_getname(ty);
  zn = transform_getname(tz);
  tdata = model_gettdata(ersmod);
  tx = ErsTransGetTran(tdata,xn);
  ty = ErsTransGetTran(tdata,yn);
  tz = ErsTransGetTran(tdata,zn);

 // Copy name, transform, model and grid limits data 
 transforms_copy(transforms(), model_gettdata(ersmod));
 memcpy(model_getmlim(ersmod), _mlim, sizeof(ModLimits));
 mlimits_set_trans(model_getmlim(ersmod),tx,ty,tz);
 memcpy(model_getglim(ersmod), _glim, sizeof(GridLimits));
 glimits_set_trans(model_getglim(ersmod),tz,tx,ty);
 if(_name) model_setname(ersmod,_name);

 // Convert to ErsModel data from vector lists.

 vll = VLLComponent(MOD_STRUC);
 if(vll)
  { pr = VectorListToPR_((void *) vll);
    if(pr != NULL)
     {PR_setx_transf(pr, tx);
      PR_setz_transf(pr, tz);
      model_rep_pdata(ersmod, (void *) pr);
     }
  }

 vll = VLLComponent(MOD_MDATA);
 if(vll)
  { mats = VectorListToMdata(vll);
    materials_setdim(mats, 3);
    if(mats != NULL)
     {mats_set_transx(mats,tx);
      mats_set_transz(mats,tz);
      mats_set_transy(mats,ty);
      model_rep_mdata(ersmod,mats);
     }
  }

 vll = VLLComponent(MOD_CELLB); 
 nv = VectorListCount(vll);
 if(nv==0) vll = VLLComponent(MOD_CELLP); // should contain pointer
 nv = VectorListCount(vll);
 if(vll)
  { cells = VectorListToCells(vll);
    cells_setdim(cells, 3);
    if(cells != NULL)
     {cells_set_transx(cells,tx);
      cells_set_transz(cells,tz);
      cells_set_transy(cells,ty);
      model_rep_cdata(ersmod,cells);
     }
  }

 return ersmod;
}

ModelDesc *ModelDesc::copy()
{ ModelDesc     *out=NULL;
  VectorLinkedList *vll;
  SeisVectLinkedList *dup_vll;
  Vector        *v,*dup_v;
  ModBaseData   *dobj,*dup_dobj;
  ErsTransforms *tdata=NULL;
  ErsTransform  *tx=NULL,*ty=NULL,*tz=NULL;
  char          *xn=NULL,*yn=NULL,*zn=NULL;
  char           str[8],font[72];
  void          *p;

  out = new ModelDesc();
  if(!out) return NULL;
 
  gettrans(&tx,&ty,&tz);
  xn = transform_getname(tx);
  yn = transform_getname(ty);
  zn = transform_getname(tz);
  tdata = out->transforms();
  tx = ErsTransGetTran(tdata,xn);
  ty = ErsTransGetTran(tdata,yn);
  tz = ErsTransGetTran(tdata,zn);

 // Copy name, transform, model andvl_data_user grid limits data 
 transforms_copy( transforms(), tdata);
 memcpy(out->MLimits(), _mlim, sizeof(ModLimits));
 mlimits_set_trans(out->MLimits(),tx,ty,tz);
 memcpy(out->GLimits(), _glim, sizeof(GridLimits));
 glimits_set_trans(out->GLimits(),tz,tx,ty);
 if(_name) out->setName(_name);

 // Copy the VectorLinkedlist's and VectorListData's
 unsigned int msz,mlw;
 Vector::VectorMarker vm;
 strcpy(font,"-adobe-*-bold-r-*--20-*-*-*-m-*-iso8859-1");

 for(int i = MOD_STRUC;i<=MOD_MDATA;i++)
  {
   dup_vll = new SeisVectLinkedList;
   if(i==MOD_STRUC) 
    out->_sdata = new VectorListData((VectorLinkedList *) dup_vll,NULL);
   if(i==MOD_CELLP) 
    out->_pdata = new VectorListData((VectorLinkedList *) dup_vll,NULL);
   if(i==MOD_CELLB) 
    out->_bdata = new VectorListData((VectorLinkedList *) dup_vll,NULL);
   if(i==MOD_MDATA) 
    out->_mdata = new VectorListData((VectorLinkedList *) dup_vll,NULL);
   vll = VLLComponent(i);
   v = vll->top(&p);
   while(v != NULL)
    {dobj = (ModBaseData *) v->getData();
     dup_dobj = new ModBaseData(dobj);
     v->getMarker(&vm,&msz,&mlw);
     dup_v = dup_vll->add(v->getName(),dup_dobj, v->getColor(),
             v->getWidth(),False, v->getStyle(), vm,msz,mlw);

     if(i==MOD_CELLP) {
       sprintf(str,"%d",dobj->getid());
       dup_v->setLabel(str,font);
     }
     v = vll->next(&p);
    }
  }

// Make a copy of the grid data.
 out->_grid_data = this->_grid_data->copy();
 return out;
}




