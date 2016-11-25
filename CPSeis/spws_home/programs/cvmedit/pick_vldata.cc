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
#include <stdlib.h>
#include <string.h>
#include "vl_data.hh"
#include "vl_data_user.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "oprim/base_data.hh"
#include "plot/pick_base.hh"
#include "pick_vldata.hh"
#include "plot/plot_base.hh"
#include "sp/seis_plot.hh"
#include "sl/prim_support.hh"
#include "seis_transf.hh"
#include "tfdefs.h"

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
int VectorListCount(void *list);
#ifdef __cplusplus
}                   // for C++
#endif


PickVLData::PickVLData(VectorListData *vldata, PlotBase *plot)
	: PickBase(plot), _vldata(vldata)
{Vector * editVector;
 int num;
 _vectors = vldata->getDataObject(); // the VectorLinkedList
 num = VectorListCount((void *) _vectors);
 editVector =  _vldata->getEditObject();
 if(!editVector)
  {// set an edit object when there is an obvious choice
   if(num==1) vldata->setEditObject(_vectors->top());
  }
}

PickVLData::~PickVLData()
{
 //setEditVector((Vector *) NULL);
}

void PickVLData::shiftButtonOnePress(int x, int y)
{Vector *editVector = _vldata->getEditObject();

  _rbnVector = NULL;
  if (editVector)
   {float fx[3],fy[3],s=0,t=0;
    if( _vectors->find(editVector)==NULL)
     { setEditVector((Vector *) NULL); return; }
    _editData  = (ModBaseData *) editVector->getData();
    if(_editData->getNumPts() < 1)
     { return; }
    _editIndex = editVector->closestIndex(x, y, getPlot());

    if (_editData->getNumPts() == 1)
     {assert(_editIndex == 0);
      MapToWorld(x, y, fx, &s, &t, fy);
      _rbnData = startData(_editData, _editIndex,1,fx,fy);
      _rbnVector = _vectors->add(_rbnData, editVector->getColor(), 2, True);
     }
    else if (_editIndex == 0)
     {
      fx[0] = _editData->getX(1);
      fy[0] = _editData->getY(1);
      MapToWorld(x, y, fx+1, &s, &t, fy+1);
      _rbnData = startData(_editData, _editIndex,2,fx,fy);
      _rbnVector = _vectors->add(_rbnData, editVector->getColor(), 2, True);
     }
    else if (_editIndex == _editData->getNumPts() - 1)
     {
      fx[0] = _editData->getX(_editIndex - 1);
      fy[0] = _editData->getY(_editIndex - 1);
      MapToWorld(x, y, fx+1, &s, &t, fy+1);
      _rbnData = startData(_editData, _editIndex-1,2,fx,fy);
      _rbnVector = _vectors->add(_rbnData, editVector->getColor(), 2, True);
     }
    else
     {
      fx[0] = _editData->getX(_editIndex - 1);
      fy[0] = _editData->getY(_editIndex - 1);
      MapToWorld(x, y, fx+1, &s, &t, fy+1);
      fx[2] = _editData->getX(_editIndex + 1);
      fy[2] = _editData->getY(_editIndex + 1);
      _rbnData = startData(_editData, _editIndex-1,3,fx,fy);
      _rbnVector = _vectors->add(_rbnData, editVector->getColor(), 2, True);
     }
   }
  else
   {
     printf("No Selected Vector\n");
     XBell(XtDisplay(getPlot()->getWidget()), 100);
   }
}

void PickVLData::shiftButtonOneMotion(int , int x2, int , int y2)
{Vector *editVector = _vldata->getEditObject();
 if (editVector && _rbnVector)
  {float p=0,z=0,s=0,t=0,user=0;
   MapToWorld(x2, y2, &p, &s, &t, &z);
   if (_rbnData->getNumPts() == 1)
    _rbnData ->replace(0, 1, &p, &z, &s, &t, &user);
   else
    _rbnData ->replace(1, 1, &p, &z, &s, &t, &user);
  }
}

void PickVLData::shiftButtonOneRelease(int , int x2, int , int y2)
{Vector *editVector = _vldata->getEditObject();
 if (editVector && _rbnVector)
  {float p=0,zeit=0,s=0,t=0,user=_editData->getUser(_editIndex);
   _vectors->remove(_rbnVector);
   _rbnVector = NULL;
   delete _rbnData;
   _rbnData = NULL;
   s = _editData->getS(_editIndex);
   MapToWorld(x2, y2, &p, &s, &t, &zeit);
   _editData ->replace(_editIndex, 1, &p, &zeit, &s, &t, &user);
   _vldata->changeMemberLoc(editVector,p,s,zeit);
  PrimSupport::updateEverything();
  }
}

void PickVLData::noModButtonTwoPress(int /*x*/, int /*y*/)
{Vector *editVector = _vldata->getEditObject();
 /* deletes the selected vector from the list */
 if (editVector)
  {
   if( _vectors->find(editVector)==NULL)
    { setEditVector((Vector *) NULL); return; }
   _editData  = (ModBaseData *) editVector->getData();
   _vldata->removeMember(editVector);
   PrimSupport::updateEverything();
   delete _editData;
  }
}

void PickVLData::cntlButtonOnePress(int x, int y)
{/* Selects the closest vector */
 Vector *vector;
 vector = _vectors->closest(x, y, getPlot());
 if(vector==NULL ||_vldata == NULL) return;
 if(vector==_vldata->getEditObject()) return;

 setEditVector(vector);
}


void PickVLData::setEditVector(Vector *vector)
{Vector *editVector = _vldata->getEditObject();
 if(vector==editVector) return;
 _vldata->setEditObject(vector);
}


void PickVLData::setData(VectorListData *data)
{
 _vldata = data;
 _vectors = NULL;
 if(data) _vectors = _vldata->getDataObject();
}

ModBaseData *PickVLData:: startData(ModBaseData *oldData, int index, int num,
     float *fx, float *fy)
{
 ModBaseData *data;
 float s[6],t[6],u[6];
 int   i,iend,npts;
 int   phd=17;
 int   shd=NILKEY;
 int   thd=NILKEY;
 int   zeit=NILKEY;
 u[0]= 0.0; u[1]= 0.0; u[2]= 0.0; u[3]= 0.0; u[4] = 0.;
 s[0]= 0.0; s[1]= 0.0; s[2]= 0.0; s[3]= 0.0; s[4] = 0.;
 t[0]= 0.0; t[1]= 0.0; t[2]= 0.0; t[3]= 0.0; t[4] = 0.;
 if(oldData == NULL)
  { return NULL;
  }
 else
  {
   phd=oldData->getphdr();
   shd=oldData->getshdr();
   thd=oldData->getthdr();
   zeit=oldData->getzeit();
   npts = oldData->getNumPts();
   iend = index + num;
   if(iend >= npts) iend = npts;
   for(i=index;i<iend;i++)
    {u[i-index] = oldData->getUser(i);
     s[i-index] = oldData->getS(i);
     t[i-index] = oldData->getT(i);
    }
  }

 if(oldData->isUserData())
  data = new ModBaseData(num, phd, fx, shd, s, thd, t,fy,&u);
 else
  data = new ModBaseData(num, phd, fx, shd, s, thd, t,fy,NULL);
 data->setzeit(zeit);
 return data;
}

void PickVLData::MapToWorld(int xx, int yy,
     float *p, float *s, float *t, float *zeit)
{// convert pixel values (xx,yy) into user data values.
 // map xx --> (p,s,t)|nd yy --> zeit
 SeisTransf *tr=NULL;
 SeisPlot   *sp=NULL;
 sp = (SeisPlot *) getPlot();
 *p = 0;
// *s = 0;
 *t = 0;
 *zeit=0;
 *p = sp->xWC(xx);
 *zeit = sp->yWC(yy);
 if(sp->plotType() >= PlotImage::PlotCOLOR) return;
 tr = (SeisTransf *) sp->transform();
 if(tr)
  {int trace= (int) sp->getTraceFromPixel( ( long) xx);
   tr->TraceToKeys(sp,trace,p,s,t);
  }
}


