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
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "oprim/base_data.hh"
#include "plot/pick_base.hh"
#include "pick_vectori.hh"
#include "plot/plot_base.hh"
#include "vl_data.hh"
#include "sp/seis_plot.hh"
#include "seis_transf.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

PickVectorI::PickVectorI(VectorListData *vldata, PlotBase *plot)
	: PickVector(vldata,plot)
{
}

PickVectorI::~PickVectorI()
{
}

void PickVectorI::noModButtonOnePress(int x, int y)
{Vector *editVector = _vldata->getEditObject();

  _rbnVector = NULL;
  _rbnData   = NULL;
  if (editVector)
   {int npts;
    float fx[3],fy[3],s=0,t=0,user=0;
    float xx,yy,test;
    int index1, index2,nr;
    if( _vectors->find(editVector)==NULL)
     { setEditVector((Vector *) NULL); return; }
    _editData  = (ModBaseData *) editVector->getData();
    if(_editData==NULL) { return; }

    npts = _editData->getNumPts();
    MapToWorld(x, y, &xx, &s, &t, &yy);
    if(npts==0)
     {_editData->insert(0, 1,&xx,&yy,&s,&t,&user);
      return;
     }

    editVector->closestIndices(x, y, &index1, &index2, getPlot());
    test = (xx-_editData->getX(index1))*(xx-_editData->getX(index2));
    if(npts == 1) test = 0.;

    if (index1 == 0)
     {
      _editIndex = 0;

      fx[0] = _editData->getX(index1);
      fy[0] = _editData->getY(index1);
      MapToWorld(x, y, fx+1, &s, &t, fy+1);
      if(test >= 0.)
       {nr=2; _editIndex = 0; }
      else
       {nr=3; _editIndex = 1;
        fx[2] = _editData->getX(1);
        fy[2] = _editData->getY(1);
       }

      _rbnData = startData(_editData, index1,nr,fx,fy);
      _rbnVector = _vectors->add(_rbnData,editVector->getColor(), 2, True);
     }
    else if (index1 == npts - 1)
     {
      _editIndex = index1 + 1;

      fx[0] = _editData->getX(index1);
      fy[0] = _editData->getY(index1);
      MapToWorld(x, y, fx+1, &s, &t, fy+1);
      if(test >= 0.)
       {nr=2; _editIndex = npts; }
      else
       {nr=3; _editIndex = npts - 1;
        fx[2] = _editData->getX(npts-2);
        fy[2] = _editData->getY(npts-2);
       }


      _rbnData = startData(_editData, index1,nr,fx,fy);
      _rbnVector = _vectors->add(_rbnData,editVector->getColor(), 2, True);
     }
    else
     {
      _editIndex = (index1 > index2) ? index1 : index2;

      fx[0] = _editData->getX(_editIndex - 1);
      fy[0] = _editData->getY(_editIndex - 1);
      MapToWorld(x, y, fx+1, &s, &t, fy+1);
      fx[2] = _editData->getX(_editIndex);
      fy[2] = _editData->getY(_editIndex);

      _rbnData = startData(_editData, _editIndex - 1,3,fx,fy);
      _rbnVector = _vectors->add(_rbnData,editVector->getColor(), 2, True);
     }
   }
  else
   {
    printf("no selected vectors\n");
    XBell(XtDisplay(getPlot()->getWidget()), 100);
   }
}

void PickVectorI::noModButtonOneMotion(int , int x2, int , int y2 )
{Vector *editVector = _vldata->getEditObject();
 if (editVector && _rbnData)
  {float p=0,z=0,s=0,t=0,user=0;
   MapToWorld(x2, y2, &p, &s, &t, &z);
   _rbnData ->replace(1, 1, &p, &z, &s, &t, &user);
  }
}

void PickVectorI::noModButtonOneRelease(int , int x2, int , int y2)
{Vector *editVector = _vldata->getEditObject();

 if (editVector && _rbnData)
  {float p,z,s=0,t=0,user=0;
   float u1,u2;
   int   i1,i2,xx,yy;
   int n = _editData->getNumPts();
   if(_editIndex == 0)
    { user = _editData->getUser(_editIndex);
      s = _editData->getS(_editIndex); }
   else if(_editIndex == n)
    { user = _editData->getUser(n-1);
      s = _editData->getS(n-1); }
   else
    { 
      user=0.0;
      if(_editIndex > 0 && _editIndex < n)
       {i1 = _editIndex;
        i2 = (_editIndex < 1) ? 0 : _editIndex - 1;
        u1 = _editData->getUser(i1);
        u2 = _editData->getUser(i2);
        user = (u1 + u2) *0.5;
        s = _editData->getS(i1);
       }
    }

   if(_rbnVector) _vectors->remove(_rbnVector);
   _rbnVector = NULL;
   if(_rbnData) delete _rbnData;
   _rbnData = NULL;
   snapToData(x2,y2,&xx,&yy);
   MapToWorld(xx,yy,&p,&s,&t,&z);
//   if(n==1) _editIndex +=1;
   _editData->insert(_editIndex, 1, &p, &z,&s,&t,&user);
  }
}

