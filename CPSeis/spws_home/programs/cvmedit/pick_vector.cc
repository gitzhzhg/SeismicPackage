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
#include "pick_vector.hh"
#include "plot/plot_base.hh"
#include "sp/seis_plot.hh"
#include "seis_transf.hh"
#include "tfdefs.h"

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
int VectorListCount(void *list);
#ifdef __cplusplus
}                   // for C++
#endif


PickVector::PickVector(VectorListData *vldata, PlotBase *plot)
	: PickBase(plot), _vldata(vldata)
{Vector * editVector;
 int num;
 _vectors = vldata->getDataObject(); // the VectorLinkedList
 _snap = SNAP_NONE;
 num = VectorListCount((void *) _vectors);
 editVector =  _vldata->getEditObject();
 if(!editVector)
  {// set an edit object when there is an obvious choice
   if(num==1) vldata->setEditObject(_vectors->top());
  }
}

PickVector::~PickVector()
{
 //setEditVector((Vector *) NULL);
}

void PickVector::shiftButtonOnePress(int x, int y)
{Vector *editVector = _vldata->getEditObject();
 float       fx[3],fy[3],s=0,t=0;

  _rbnVector = NULL;
  if (editVector)
   {
    if( _vectors->find(editVector)==NULL)
     { setEditVector((Vector *) NULL); return; }
    _editData  = (ModBaseData *) editVector->getData();
    if(_editData->getNumPts() < 1)
     { return; }
    _editIndex = editVector->closestIndex(x, y, getPlot());

    if (_editData->getNumPts() == 1)
     {assert(_editIndex == 0);
      MapToWorld(x, y, fx, &s, &t, fy);
//      float fx = getPlot()->xWC(x);
//      float fy = getPlot()->yWC(y);

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

void PickVector::shiftButtonOneMotion(int , int x2, int , int y2)
{Vector *editVector = _vldata->getEditObject();
 if (editVector && _rbnVector)
  {float x,y,s=0,t=0,user=0;

   MapToWorld(x2, y2, &x, &s, &t, &y);
   if (_rbnData->getNumPts() == 1)
    _rbnData ->replace(0, 1, &x, &y, &s, &t, &user);
   else
    _rbnData ->replace(1, 1, &x, &y, &s, &t, &user);

  }
}

void PickVector::shiftButtonOneRelease(int , int x2, int , int y2)
{Vector *editVector = _vldata->getEditObject();
 int xx,yy;
 if (editVector && _rbnVector)
  {float p,z,s=0,t=0,user=_editData->getUser(_editIndex);
   _vectors->remove(_rbnVector);
   _rbnVector = NULL;
   delete _rbnData;
   _rbnData = NULL;
   snapToData(x2, y2, &xx, &yy);

   s = _editData->getS(_editIndex);
   MapToWorld(xx, yy, &p, &s, &t, &z);
   _editData ->replace(_editIndex, 1, &p, &z, &s, &t, &user);
  }
}

void PickVector::noModButtonTwoPress(int x, int y)
{Vector *editVector = _vldata->getEditObject();
 /* deletes closest point of selected vector */
 if (editVector)
  {
   int index1, index2;
   if( _vectors->find(editVector)==NULL)
    { setEditVector((Vector *) NULL); return; }
   _editData  = (ModBaseData *) editVector->getData();
   if(_editData->getNumPts()<=0) return;
   editVector->closestIndices(x, y, &index1, &index2, getPlot());
   _editData->remove(index1,1);
  }
}

void PickVector::cntlButtonOnePress(int x, int y)
{/* Selects the closest vector */
 Vector *vector;
 vector = _vectors->closest(x, y, getPlot());
 if(vector==NULL ||_vldata == NULL) return;
 if(vector==_vldata->getEditObject()) return;

 setEditVector(vector);
}

void PickVector::cntlButtonTwoPress(int x, int y)
{Vector *vector;
 ModBaseData *data;
 int   phd;
 float fx[5],fy[5];
 fx[0] = getPlot()->xWC(x);
 fy[0] = getPlot()->yWC(y);
 fx[4] = fx[3] = fx[2] = fx[1] = fx[0];
 fy[4] = fy[3] = fy[2] = fy[1] = fy[0];
 vector = _vectors->top();
 if(vector==NULL) return;
 data = (ModBaseData *) vector->getData();
 phd=data->getphdr();
 _rbnData = new ModBaseData(5, phd, fx, 0, NULL, 0, NULL,fy,NULL);
 _rbnVector = _vectors->add(_rbnData, "red", 2, True);
}

void PickVector::cntlButtonTwoMotion(int , int x2, int , int y2)
{ float fx[5],fy[5];
  fx[0] = _rbnData->getX(0);
  fx[1] = fx[0];
  fx[2] = getPlot()->xWC(x2);
  fx[3] = fx[2];
  fx[4] = fx[0];
  fy[0] = _rbnData->getY(0);
  fy[1] = getPlot()->yWC(y2);
  fy[2] = fy[1];
  fy[3] = fy[0];
  fy[4] = fy[0];
   _rbnData ->replace(1, 4,&fx[1],&fy[1], NULL, NULL, NULL);
}

void PickVector::cntlButtonTwoRelease(int , int x2, int , int y2)
{Vector *editVector = _vldata->getEditObject();
 float fx[2],fy[2],width,height,cx,cy;
 fx[0] = _rbnData->getX(0);
 fy[0] = _rbnData->getY(0);
 fx[1] = getPlot()->xWC(x2);
 fy[1] = getPlot()->yWC(y2);
 width = fx[0]-fx[1];
 if(width<0) width= -width;
 height= fy[0]-fy[1];
 if(height<0) height= -height;
 cx = (fx[0]<fx[1]) ? fx[0] : fx[1];
 cy = (fy[0]<fy[1]) ? fy[0] : fy[1];
 if(editVector)
   {
    _editData  = (ModBaseData *) editVector->getData();
    if(_editData->getNumPts() < 1)
     { return; }
    else
     {int N = _editData->getNumPts();
      int i,cnt=0;
      int   *ind= new int[N];
      float xp,yp;
      for(i=0;i<N;i++)
        {xp = _editData->getX(i);
         yp = _editData->getY(i);
         if(PointInside(xp,yp,cx,cy,width,height))
          {ind[cnt]=i;
           cnt++;
          }
        }
      _editData->remove(ind,cnt);
      delete []ind;
     }
   }
   _vectors->remove(_rbnVector);
   _rbnVector = NULL;
   delete _rbnData;
   _rbnData = NULL;
}

void PickVector::setEditVector(Vector *vector)
{Vector *editVector = _vldata->getEditObject();
 if(vector==editVector) return;
 _vldata->setEditObject(vector);
}

void PickVector::setSnap(long snap)
{ if(snap < 0 || snap > 4) return;
  _snap = (int) snap;
}

void PickVector::snapToData(int x, int y, int *xo, int *yo)
{ SeisPlot *sp = (SeisPlot *) getPlot();
  const float *ftr=NULL;
  float *fptr=NULL;
  float ftrno;
  int   msamp,nsamp,trace,sample;
  int   search=14,t1,t2,i1,i2;
  const unsigned char *btr=NULL;
  *xo = x;
  *yo = y;
  if(sp == NULL || _snap == SNAP_NONE) return;
  ftr= sp->floatTraceData();
  btr= sp->byteTraceData();
  if(btr==NULL && ftr==NULL) return;
  nsamp= (int) sp->samplesPerTrace();
  trace= (int) sp->getTraceFromPixel( ( long) x);
  ftrno = (float) trace;
  if(btr != NULL)
   {float maxv=1.0;
    fptr = (float *) malloc(sizeof(float) * nsamp);
    btr = btr + trace*nsamp;
    byte_to_float_((unsigned char *) btr, &nsamp,&nsamp, &maxv, fptr);
    ftr = fptr;
   }
  else ftr = ftr + trace*nsamp;
  msamp= (int) sp->ySampleNumFromPixel( y);
  sample = msamp;
  t1  = (msamp > search ) ? msamp - search : 0;
  t2  = (msamp + search < nsamp ) ? msamp + search : nsamp-1;
  switch(_snap)
   { case SNAP_NONE:
      return;
     case SNAP_PEAK:
      for (i1=msamp; i1<t2 ; i1++) if (ftr[i1+1] < ftr[i1]) break;
      for (i2=msamp; i2>t1; i2--)  if (ftr[i2-1] < ftr[i2]) break;
      sample  = (ftr[i1] > ftr[i2]) ?  i1 : i2;
      if (ftr[msamp] == ftr[sample]) sample = msamp;
      sp->xyFromTraceAndSample(ftrno,sample,xo,yo);
      break;
     case SNAP_TROUGH:
      for (i1=msamp; i1<t2 ; i1++) if (ftr[i1+1] > ftr[i1]) break;
      for (i2=msamp; i2>t1; i2--)  if (ftr[i2-1] < ftr[i2]) break;
      sample  = (ftr[i1] < ftr[i2]) ?  i1 : i2;
      if (ftr[msamp] == ftr[sample]) sample = msamp;
      sp->xyFromTraceAndSample(ftrno,sample,xo,yo);
      break;
     case SNAP_PM:
      sample = msamp;
      i2 = -1;
      for (i1=t1; i1< t2; i1++)
       {if (ftr[i1] > 0 && ftr[i1+1] <= 0)
         {if (i2 == -1 || abs(i2-msamp) > abs(i1-msamp)) i2 = i1; }
       }
      if (i2 != -1) sample = i2;
      ftrno = trace;
      sp->xyFromTraceAndSample(ftrno,sample,xo,(int *) &t1);
      sp->xyFromTraceAndSample(ftrno,sample+1,xo,(int *) &t2);
      *yo =(int ) (t1 - (t2-t1)*(ftr[sample]/(ftr[sample+1]-ftr[sample])));
 
      break;
     case SNAP_MP:
      sample = msamp;
      i2 = -1;
      for (i1=t1; i1< t2; i1++)
       {if (ftr[i1] <= 0 && ftr[i1+1] > 0)
         {if (i2 == -1 || abs(i2-msamp) > abs(i1-msamp)) i2 = i1;
        }
       }
      if (i2 != -1) sample = i2;
      ftrno = trace;
      sp->xyFromTraceAndSample(ftrno,sample,xo,(int *) &t1);
      sp->xyFromTraceAndSample(ftrno,sample+1,xo,(int *) &t2);
      *yo =(int ) (t1 - (t2-t1)*(ftr[sample]/(ftr[sample+1]-ftr[sample])));
     
   }

 if(fptr) free(fptr);
}

void PickVector::setData(VectorListData *data)
{
 _vldata = data;
 _vectors = NULL;
 if(data) _vectors = _vldata->getDataObject();
}

ModBaseData *PickVector:: startData(ModBaseData *oldData, int index, int num,
     float *fx, float *fy)
{// starts a vector which will be used for rubber-banding.
 ModBaseData *data;
 float s[6],t[6],u[6];
 int   i,iend,npts;
 int   phd=XBASEHDR_;
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

void PickVector::MapToWorld(int xx, int yy,
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
 if(sp) tr = (SeisTransf *) sp->transform();
 if(tr)
  {int trace= (int) sp->getTraceFromPixel( ( long) xx);
   tr->TraceToKeys(sp,trace,p,s,t);
  }
}

int PickVector::sliceKey()
{// Determine the orientation of the seismic data.
 // Check if seismic is compatible with primary or
 // secondary key of the Vector data.
 SeisTransf  *tr=NULL;
 SeisPlot    *sp=NULL;
 Vector      *vector=NULL;
 ModBaseData *data=NULL;
 int    key=0,keyhd,mems,meme;
 int    nhd,ndisp;
 float  p1,p2,tol=0.01;
 const  float *hd=NULL;

 if(_vectors) vector = _vectors->top();
 if(vector==NULL) return key;
 data = (ModBaseData *) vector->getData();
 if(data) key = data->getphdr();
 sp = (SeisPlot *) getPlot();
 if(sp) tr = (SeisTransf *) sp->transform();
 else return key;
 hd    = sp->headers();
 nhd   = (int) sp->numHeaders();
 ndisp = (int) sp->displayedTraces();
 if(tr) tol = tr->tol();

 if(ndisp <= 0 || hd == NULL) return key;
 mems = (int) (sp->firstTrace() + sp->currentFrame()*sp->originalTraces());
 meme = ndisp -1 + mems;

 keyhd = data->getshdr();
 if(keyhd != NILKEY) {
  p1 = hd[(mems-1)*nhd+ keyhd-1];
  p2 = hd[(meme-1)*nhd+ keyhd-1];
  if(fabs(p2-p1) > tol) key = keyhd;
 }

 keyhd = data->getphdr();
 if(keyhd != NILKEY) {
  p1 = hd[(mems-1)*nhd+ keyhd-1];
  p2 = hd[(meme-1)*nhd+ keyhd-1];
  if(fabs(p2-p1) > tol) key = keyhd;
 }

 return key;
}


int PickVector::PointInside(float px,float py,
    float ex,float ey,float width,float height)
{
  if (px >= ex && px <= ex+width &&
      py >= ey && py <= ey+height) return 1;
  return 0;
}
