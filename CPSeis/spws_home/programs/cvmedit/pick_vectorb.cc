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
#include "vl_data.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "oprim/base_data.hh"
#include "plot/pick_base.hh"
#include "pick_vectorb.hh"
#include "plot/plot_base.hh"
#include "sp/seis_plot.hh"
#include "seis_transf.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

PickVectorB::PickVectorB(VectorListData *vldata, PlotBase *plot)
	: PickVector(vldata,plot)
{
}

PickVectorB::~PickVectorB()
{
}

void PickVectorB::noModButtonOnePress(int x, int y)
{Vector *editVector = _vldata->getEditObject();

  _rbnVector = NULL;
  if (editVector)
   {int npts;
    float fx[3],fy[3],s=0,t=0,user=0;
    if( _vectors->find(editVector)==NULL)
     { setEditVector((Vector *) NULL); return; }
    _editData  = (ModBaseData *) editVector->getData();
    if(_editData==NULL) { return; }
    _editIndex = 0;

    npts = _editData->getNumPts();
    MapToWorld(x, y, &fx[1], &s, &t, &fy[1]);
    if(npts==0)
     {_editData->insert(0, 1,&fx[1],&fy[1],&s,&t,&user);
      return;
     }
    fx[0] = _editData->getX(0);
    fy[0] = _editData->getY(0);

    _rbnData = startData(_editData, 0,2,fx,fy);
    _rbnVector = _vectors->add(_rbnData,editVector->getColor(), 2, True);
   }
  else
   {
    printf("no selected vectors\n");
    XBell(XtDisplay(getPlot()->getWidget()), 100);
   }
}

void PickVectorB::noModButtonOneMotion(int , int x2, int , int y2 )
{Vector *editVector = _vldata->getEditObject();
 if (editVector)
  {float p=0,z=0,s=0,t=0,user=0;
   MapToWorld(x2, y2, &p, &s, &t, &z);
   _rbnData ->replace(1, 1, &p, &z, &s, &t, &user);
  }
}

void PickVectorB::noModButtonOneRelease(int , int x2, int , int y2)
{Vector *editVector = _vldata->getEditObject();

 if (editVector && _rbnVector)
  {float p=0,z=0,s=0,t=0,user=0;
   int n = _editData->getNumPts(),xx,yy;
   snapToData(x2,y2,&xx,&yy);
   s = _editData->getS(0);
   MapToWorld(xx,yy,&p,&s,&t,&z);
   if(n > 0) user = _editData->getUser(0);
   _vectors->remove(_rbnVector);
   delete _rbnData;
   _rbnVector = NULL; _rbnData = NULL;
   _editData->insert(0, 1, &p, &z,&s,&t,&user);
  }
}

