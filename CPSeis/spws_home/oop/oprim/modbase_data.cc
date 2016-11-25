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
#include "oprim/data_user.hh"
#include "oprim/ll_data_user.hh"
#include "transform.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

ModBaseData::ModBaseData(int numPts, int phd, float *pv,
        int shd, float *sv, int thd, float *tv,int zeit, float *y,
        void *user,long ident) : 
   BaseData() ,  _ident(ident)
{// phd,shd,thd,zeit flag coordinate systems
 initialize(numPts, phd, pv, shd, sv, thd, tv,zeit, y, user);
}

ModBaseData::ModBaseData(int numPts, int phd, float *pv,
        int shd, float *sv, int thd, float *tv,float *y,
        void *user,long ident) : 
   BaseData() ,  _ident(ident)
{// phd,shd,thd,zeit flag coordinate systems
 int zeit = NILKEY;
 initialize(numPts, phd, pv, shd, sv, thd, tv,zeit, y, user);
}

ModBaseData::ModBaseData(ModBaseData *mbd) : BaseData()
{
 if(!mbd) return;
 _ident = mbd->_ident;
 initialize(mbd->_numPts, mbd->_phdr, mbd->_xData, mbd->_shdr, mbd->_sData,
   mbd->_thdr, mbd->_tData,mbd->_zeit, mbd->_yData, mbd->_user_data);
 _id = mbd->_id;
 _xo = mbd->_xo;
 _yo = mbd->_yo;
 _so = mbd->_so;
}

void ModBaseData::initialize(int numPts, int phd, float *pv,
        int shd, float *sv, int thd, float *tv,int zeit, float *y,
        void *user) 
{
   _numPts  = 0;
   _numkeys = 0;
   _phdr  = NILKEY;
   _shdr  = NILKEY;
   _thdr  = NILKEY;
   _zeit  = zeit;
   _xData = (float *) NULL;
   _yData = (float *) NULL;
   _sData = (float *) NULL;
   _tData = (float *) NULL;
   _user_data = (float *) NULL;
   _has_user_data = 0;
   if(user) _has_user_data = 1;
   if(phd != NILKEY) {_phdr = phd; _numkeys++; }
   if(shd != NILKEY) {_shdr = shd; _numkeys++; }
   if(thd != NILKEY) {_thdr = thd; _numkeys++; }
   if (numPts)
    { _numPts = init_data( numPts,pv,sv,tv,y,user);
    }

}

ModBaseData::~ModBaseData()
{
 if (_xData) { delete [] _xData; _xData = NULL; }
 if (_yData) { delete [] _yData; _yData = NULL; }
 if (_sData) { delete [] _sData; _sData = NULL; }
 if (_tData) { delete [] _tData; _tData = NULL; }
 if (_user_data) { delete [] _user_data;_user_data = NULL; }
 _phdr  = NILKEY;
 _shdr  = NILKEY;
 _thdr  = NILKEY;
 _zeit  = NILKEY;
 _numPts = 0;
}

int ModBaseData::init_data(int numPts, float *pv,
         float *sv, float *tv,float *z, void *user) 
{
  float *user_data;

  if(numPts < 1) return 0; // nothing to initialize
  if(_xData) return 0;    // already initialized
  if(_yData) return 0;    // already initialized
  _numPts = numPts;

  _xData = new float[ _numPts];
  _yData = new float[ _numPts];
  for (int i = 0; i < _numPts; i++)
   { _xData[i] = pv[i];
     _yData[i] = z[i];
   }

  if(_shdr != NILKEY && sv != NULL)
   {_sData = new float[ numPts];
    for (int i = 0; i < numPts; i++)
     { _sData[i] = sv[i]; }
   }
  if(_thdr != NILKEY && tv != NULL)
   {_tData = new float[ numPts];
    for (int i = 0; i < numPts; i++)
     { _tData[i] = tv[i]; }
   }
  if(user != NULL)
   {_user_data =  new float[numPts];
    _has_user_data = 1;
    user_data = (float *) user;
            for (int i = 0; i < numPts; i++)
             { _user_data[ i ] =  user_data[i];
             }
   }
  else _user_data = (float *) NULL;

 return _numPts;
}

float ModBaseData::getX(int i,long ident)
{assert(i >= 0 && i < _numPts);
 if(ident== defaultId && _xData) return _xData[i];
 if(ident==_phdr && _xData) return _xData[i];
 if(ident==_shdr && _sData) return _sData[i];
 if(_xData) return _xData[i];
 return 0;
}

float ModBaseData::getY(int i,long /*ident*/)
{assert(i >= 0 && i < _numPts);
//if(ident== defaultId && _yData) return _yData[i];
// if(ident==_phdr && _xData) return _xData[i];
// if(ident==_shdr && _sData) return _sData[i];
 if(_yData) return _yData[i];
 return 0;
}
 
float ModBaseData::getS(int i, long ident)
{assert(i >= 0 && i < _numPts);
 if(ident== defaultId && _sData) return _sData[i];
 if(_sData && ident==_shdr) return _sData[i];
 if(_xData && ident==_phdr) return _xData[i];
 if(_sData ) return _sData[i];
 return 0;
}

float ModBaseData::getT(int i)
{assert(i >= 0 && i < _numPts);
 if(!_tData || _thdr==NILKEY ) return 0;
 return (_tData[ i]);
}

float ModBaseData::getUser(int i)
{assert(i >= 0 && i < _numPts);
 if(!_user_data) return 0.;
 return (_user_data[ i]);
}

int  ModBaseData::getNx(int i,int num, float *out,long ident)
{ int j;
  if(_numPts<1 || out==NULL) return 0;
  assert(i >= 0 && i < _numPts);
  num = (i+num > _numPts) ? _numPts-i : num;
  for(j=i;j<i+num;j++)
   { out[j-i] = getX(j,ident); }
  return (num);
}
int  ModBaseData::getNy(int i,int num, float *out, long ident)
{ int j;
  if(_numPts<1 || out==NULL) return 0;
  assert(i >= 0 && i < _numPts);
  num = (i+num > _numPts) ? _numPts-i : num;
  for(j=i;j<i+num;j++)
   { out[j-i] = getY(j, ident); }
  return (num);
}
int  ModBaseData::getNs(int i,int num, float *out, long ident)
{ int j;
  if(_numPts<1 || out==NULL) return 0;
  assert(i >= 0 && i < _numPts);
  num = (i+num > _numPts) ? _numPts-i : num;
  if(_shdr==NILKEY) return 0;
  for(j=i;j<i+num;j++)
   { out[j-i] = getS(j,ident); }
  return (num);
}
int  ModBaseData::getNt(int i,int num, float *out)
{ int j;
  if(_numPts<1 || out==NULL) return 0;
  if(_tData == NULL) return 0;
  assert(i >= 0 && i < _numPts);
  num = (i+num > _numPts) ? _numPts-i : num;
  if(_thdr==NILKEY) return 0;
  for(j=i;j<i+num;j++)
   { out[j-i] = _tData[j]; }
  return (num);
}

int  ModBaseData::getNuser(int i,int num, float *xout)
{ if(_user_data==NULL) return 0;
  assert(i >= 0 && i < _numPts);
  num = (i+num > _numPts) ? _numPts-i : num;
  memcpy(xout,_user_data + i, num*sizeof(float));
  return (num);
}

Bool ModBaseData::isUserData()
{ if(_has_user_data ) return True;
  else return False;
}

void ModBaseData::insert(int index, int numIns, float *x, float *y,
 float *s, float *t, void *user)
{int newnum;
 if(numIns < 1) return;
 if(_numPts <= 0)
  { _numPts = init_data( numIns,x,s,t,y,user);
    modIndicesAfter(index, numIns,_ident);
    modDone(_ident);
    return;
  }

 assert(index >= 0 && index <= _numPts );
 if (numIns)
  {
    float *oldXData = _xData;
    float *oldYData = _yData;
    float *oldSData = _sData;
    float *oldTData = _tData;
    float *olduser  = _user_data;

    newnum   = _numPts + numIns;

    _xData = new float[newnum];
    _yData = new float[newnum];
    memcpy(_xData, oldXData, (size_t) index * sizeof(float));
    memcpy(_yData, oldYData, (size_t) index * sizeof(float));
    for (int i = 0; i < numIns; i++)
     {_xData[i + index] = x[i];
      _yData[i + index] = y[i];
     }
    memcpy(_xData + index + numIns, oldXData + index,
           (size_t) (newnum - index - numIns) * sizeof(float));
    memcpy(_yData + index + numIns, oldYData + index,
           (size_t) (newnum - index - numIns) * sizeof(float));
    delete [] oldXData;
    delete [] oldYData;

    if(_shdr != NILKEY)
      {_sData   = new float[newnum];
       memcpy(_sData, oldSData, (size_t) index * sizeof(float));
       for (int i = 0; i < numIns; i++) { _sData[i + index] = s[i];}
       memcpy(_sData+index+numIns, oldSData+index,
        (size_t) (newnum - index - numIns) * sizeof(float));
       delete oldSData;
      }
    if(_thdr != NILKEY)
      {_tData   = new float[newnum];
       memcpy(_tData, oldTData, (size_t) index * sizeof(float));
       for (int i = 0; i < numIns; i++) { _tData[i + index] = t[i];}
       memcpy(_tData+index+numIns, oldTData+index,
        (size_t) (newnum - index - numIns) * sizeof(float));
       delete oldTData;
      }
            
    if(_user_data != NULL)
      { float *fval = (float *) user;
        _user_data = (float *) new float[newnum];
        memcpy(_user_data, olduser, (size_t) index * sizeof(float));
	for (int i = 0; i < numIns; i++)
 	 { if(user != NULL)
               _user_data[(i + index)    ] = (float) fval[i];
           else
               _user_data[(i + index)    ] = 0.0;
         }
	memcpy(_user_data + (index + numIns), olduser + index,
		 (size_t)(newnum - index - numIns) * sizeof(float));
        delete olduser;
      }

    _numPts += numIns;
    modIndicesAfter(index, numIns,_ident);    // check new points
    modDone(_ident);
  }
}

void ModBaseData::remove(int index, int rem)
{int numRem,newnum;
 if(_numPts < 1 || rem < 1) return;
 if(index < 0 || index > _numPts -1) return;
 numRem    = (rem >= _numPts - index) ? _numPts - index : rem;
 newnum = (numRem >= _numPts) ? 0 : _numPts - numRem;
 assert(index >= 0 && index < _numPts
	&& numRem >= 0 && numRem <= _numPts - index);

 if(numRem > 0 && numRem == _numPts)
  { // getting rid of all data points
   modIndicesBefore(index, numRem,_ident);    // check new points
   if (_xData) { delete [] _xData; _xData = NULL; }
   if (_yData) { delete [] _yData; _yData = NULL; }
   if (_sData) { delete [] _sData; _sData = NULL; }
   if (_tData) { delete [] _tData; _tData = NULL; }
   if (_user_data) { delete [] _user_data;_user_data = NULL; }
   _numPts = 0;
   modDone(_ident);
   return;
  }

 if (numRem)
  { // getting rid of some data points but not all
    modIndicesBefore(index, numRem,_ident);    // check new points
    float *oldXData = _xData;
    float *oldYData = _yData;
    float *oldSData = _sData;
    float *oldTData = _tData;
    float *olduser  = _user_data;

    _xData = new float[newnum];
    _yData = new float[newnum];
    memcpy(_xData, oldXData, (size_t) index * sizeof(float));
    memcpy(_xData + index, oldXData + index + numRem,
          (size_t) (newnum - index) * sizeof(float));
    memcpy(_yData, oldYData, (size_t) index * sizeof(float));
    memcpy(_yData + index, oldYData + index + numRem,
          (size_t) (newnum - index) * sizeof(float));
    delete [] oldXData;
    delete [] oldYData;

    if(_shdr != NILKEY)
      {_sData = new float[newnum];
        memcpy(_sData, oldSData, (size_t) index * sizeof(float));
        memcpy(_sData + index, oldSData + index + numRem,
                        (size_t) (newnum - index) * sizeof(float));
        delete [] oldSData;
      }
    if(_thdr != NILKEY)
      {_tData = new float[newnum];
        memcpy(_tData, oldTData, (size_t) index * sizeof(float));
        memcpy(_tData + index, oldTData + index + numRem,
                        (size_t) (newnum - index) * sizeof(float));
        delete [] oldTData;
      }
    if(_user_data != NULL)
      { _user_data = (float *) new float[newnum];
        memcpy(_user_data, olduser, (size_t) index * sizeof(float));
        memcpy(_user_data + index, olduser + (index + numRem),
           (size_t) (newnum - index) * sizeof(float));
        delete olduser;
      }
     _numPts = newnum;
     modDone(_ident);
  }
}


void ModBaseData::remove(int *index, int rem)
{int numRem,newnum;
 if(_numPts < 1 || rem < 1) return;
 numRem =  rem;
 newnum = _numPts - numRem;

 if(numRem > 0 && numRem == _numPts)
  { // getting rid of all data points
   modIndicesBefore(0, numRem,_ident);    // check new points
   if (_xData) { delete [] _xData; _xData = NULL; }
   if (_yData) { delete [] _yData; _yData = NULL; }
   if (_sData) { delete [] _sData; _sData = NULL; }
   if (_tData) { delete [] _tData; _tData = NULL; }
   if (_user_data) { delete [] _user_data;_user_data = NULL; }
   _numPts = 0;
   modDone(_ident);
   return;
  }

   // getting rid of some data points but not all
    modIndicesBefore(0, _numPts,_ident);    // check new points
    float *oldXData = _xData;
    float *oldYData = _yData;
    float *oldSData = _sData;
    float *oldTData = _tData;
    float *olduser  = _user_data;

    _xData = new float[newnum];
    _yData = new float[newnum];
    if(_shdr != NILKEY) _sData = new float[newnum];
    if(_thdr != NILKEY) _tData = new float[newnum];
    if(_user_data)  _user_data = (float *) new float[newnum];
    int i,cnt = 0;
    for(i=0;i<newnum;i++)
     {
      if(index[i]>=0 && index[i]<_numPts)
       {
        _xData[cnt] = oldXData[index[i]];
        _yData[cnt] = oldYData[index[i]];
        if(_shdr!=NILKEY) _sData[cnt] = oldSData[index[i]];
        if(_thdr!=NILKEY) _tData[cnt] = oldTData[index[i]];
        if(_user_data) _user_data[cnt] = olduser[index[i]];
        cnt++;
       }
     }
    delete [] oldXData;
    delete [] oldYData;
    if(_shdr != NILKEY) delete [] oldSData;
    if(_thdr != NILKEY) delete [] oldTData;
    if(_user_data) delete []olduser;

    _numPts = cnt;
     modDone(_ident);
}

void ModBaseData::replace(int index, int numRep, float *x, float *y,
     float *s, float *t, void *user)
{ // only replace existing data
 if(_numPts <= 0 || numRep < 1) return;
 if(index < 0 || index > _numPts -1) return;
 assert(numRep <= _numPts - index);

 if (numRep)
  {
   modIndicesBefore(index, numRep,_ident); // check old points

   for (int i = 0; i < numRep; i++)
    {
     _xData[i + index] = x[i];
     _yData[i + index] = y[i];
     if(_shdr != NILKEY) _sData[i + index] = s[i];
     if(_thdr != NILKEY) _tData[i + index] = t[i];
    }

   if(_user_data != NULL && user != NULL)
     { float *fval = (float *) user;
       for (int i = 0; i < numRep; i++)
 	{ if(user != NULL)
           _user_data[i + index ] =  fval[i];
          else
           _user_data[i + index ] =  0.0;
        }
     }

    modIndicesAfter(index, numRep,_ident); // check new points
    modDone(_ident);
  }
}

void ModBaseData::replace(int index, int numRep, float *x, float *y)
{ // Only replace x & y of existing data
 if(_numPts <= 0 || numRep < 1) return;
 if(index < 0 || index > _numPts -1) return;
 assert(numRep <= _numPts - index);

 if (numRep)
  {
   modIndicesBefore( index, numRep,_ident); // check old points
   for (int i = 0; i < numRep; i++)
    {_xData[i + index] = x[i];
     _yData[i + index] = y[i];
    }
    modIndicesAfter(index, numRep, _ident);// check new points
    modDone(_ident);
  }
}


void ModBaseData::replace(int index, int numRem, int numIns, float *x, float *y,
     float *s, float *t, void *user)
{int oldnum,newnum;
 if(_numPts <= 0)
  { _numPts = init_data( numIns,x,s,t,y,user);
    modIndicesAfter(index, numIns,_ident);
    modDone(_ident);
    return;
  }

 if (numRem == numIns)
  {
   if(index == _numPts)
    insert(index, numIns, x, y,s,t,user);
   else if(index < _numPts)
    replace(index, numRem, x, y,s,t,user);
  }
 else if (numRem == 0)
  {
   insert(index, numIns, x, y,s,t,user);
  }
 else if (numIns == 0)
  {
   remove(index, numRem);
  }
 else
  {int i;
   oldnum = _numPts;
   newnum = _numPts + numIns - numRem;
   assert(index >= 0 && index < _numPts
	&& numRem > 0 && numRem <= _numPts - index
	&& numIns > 0);

   modIndicesBefore(index, numRem,_ident);	// check old points

   float *oldXData = _xData;
   float *oldYData = _yData;
   float *oldSData = _sData;
   float *oldTData = _tData;
   float *olduser  = _user_data;

   _xData = new float[newnum];
   memcpy(_xData, oldXData, (size_t) index * sizeof(float));
   for ( i = 0; i < numIns; i++) { _xData[i + index] = x[i]; }
   memcpy(_xData + index + numIns, oldXData + index + numRem,
            (size_t) (newnum - index - numIns) * sizeof(float));
   delete  [] oldXData;

   _yData = new float[newnum];
   memcpy(_yData, oldYData, (size_t) index * sizeof(float));
   for ( i = 0; i < numIns; i++) { _yData[i + index] = y[i]; }
   memcpy(_yData + index + numIns, oldYData + index + numRem,
            (size_t) (newnum - index - numIns) * sizeof(float));
   delete  [] oldYData;

   if(_shdr != NILKEY)
    { _sData = new float[newnum];
     memcpy(_sData, oldSData, (size_t) index * sizeof(float));
     for (i = 0; i < numIns; i++) { _sData[i + index] = s[i]; }
     memcpy(_sData + index + numIns, oldSData + index + numRem,
            (size_t) (newnum - index - numIns) * sizeof(float));
     delete  [] oldSData;
    }
   if(_thdr != NILKEY)
    { _tData = new float[newnum];
     memcpy(_tData, oldTData, (size_t) index * sizeof(float));
     for (i = 0; i < numIns; i++) { _tData[i + index] = s[i]; }
     memcpy(_tData + index + numIns, oldTData + index + numRem,
            (size_t) (newnum - index - numIns) * sizeof(float));
     delete  [] oldTData;
    }

   if(_user_data != NULL)
    {float *fval = (float *) user;
     _user_data = new float[newnum];
     memcpy(_user_data, olduser, (size_t) index * sizeof(float));
     for ( i = 0; i < numIns; i++)
      {if(user != NULL)
        _user_data[i + index] = fval[i];
       else
        _user_data[i + index] = 0.0;
      }
     memcpy(_user_data + index + numIns, olduser + index + numRem,
           (size_t)(newnum - index - numIns) * sizeof(float));
     delete olduser;
    }

    _numPts = newnum;
    modIndicesAfter(index, numIns,_ident); // check new points
    modDone(_ident);
  }
}

/*
  See the file transform.h
  _phdr flags the _xData array
  _shdr flags the _sData array
  _thdr flags the _tData array
  _zeit flags the _yData array
  Each flag is associated with a string which labels
  a conventional coordinate system
*/

void ModBaseData:: setphdr(int header)
{_phdr = header; }

void ModBaseData:: setshdr(int header)
{_shdr = header; }

void ModBaseData:: setthdr(int header)
{_thdr = header; }


void ModBaseData:: sethdrs(int phd,int shd,int thd)
{
 _numkeys = 0;
 _phdr = NILKEY, _shdr = NILKEY; _thdr = NILKEY;
 if(phd != NILKEY) {_phdr = phd; _numkeys++; }
 if(shd != NILKEY) {_shdr = shd; _numkeys++; }
 if(thd != NILKEY) {_thdr = thd; _numkeys++; }
}

int ModBaseData::trans_data_comps(ErsTransforms *tdata,
    char *p, char *s, char *t, char *zeit)
{// Transform all data components.
 int err;
 if(tdata==NULL) return 0;
 if(_numPts<=0) return 0;
 modIndicesBefore( 0, _numPts,_ident); // check old points
 if(p)
  err =trans_data_axis(ModBaseData::primary,tdata,p);
 if(s)
 err =trans_data_axis(ModBaseData::secondary,tdata,s);
 err =trans_data_axis(ModBaseData::tertiary,tdata,t);
 err =trans_data_axis(ModBaseData::time,tdata,zeit);
 modIndicesAfter(0, _numPts, _ident);// check new points
 modDone(_ident);
 return 1;
}

int ModBaseData::trans_data_comp(int data_comp, ErsTransforms *tdata, char *nameo)
{// Transform the data component flagged by data_comp.
 int err;
 if(tdata==NULL || nameo == NULL) return 0;
 if(_numPts<=0) return 0;
 modIndicesBefore( 0, _numPts,_ident); // check old points
 err = trans_data_axis(data_comp, tdata, nameo);
 modIndicesAfter(0, _numPts, _ident);// check new points
 modDone(_ident);
 return err;
}

int ModBaseData::trans_data_axis(int data_comp, ErsTransforms *tdata, char *nameo)
{// Transform the data component flagged by data_comp.
 // Note: Data Users are not notified by this method.
 // Choose from the palette of transforms in tdata.
 // The desired output coordinate system has the name nameo.
 int i,ierr,key, outkey;
 ErsTransform *to=NULL,*from=NULL;
 float sc, io, oo;

 if(tdata==NULL || nameo==NULL) return 0;
 if(_numPts<=0) return 0;
 to = ErsTransGetTran(tdata,nameo);
 outkey = (int) transform_gethdr(to);
 key = NILKEY;
 if(data_comp == ModBaseData::primary) key = _phdr;
 if(data_comp == ModBaseData::secondary) key = _shdr;
 if(data_comp == ModBaseData::tertiary) key = _thdr;
 if(data_comp == ModBaseData::time) key = _zeit;
 if(key == NILKEY)
   { from = NULL;
   }
 else
   { // Ambiguous results if non-unique header keys
     from = ErsTransByID(tdata,key);
   }
 // from can be NULL
 if(to==NULL) { return 0; }

 ierr = transform_coeff(from,to,&sc, &io, &oo);
 if(ierr != 0 ) return 0; 
 switch (data_comp) {
    case(ModBaseData::primary):
     if(_xData)
      for(i=0;i<_numPts;i++) _xData[i]  = oo + sc*(_xData[i] - io);
     _xo =  oo + sc*(_xo - io);
     _phdr = outkey;
     break;
    case(ModBaseData::time):
     if(_yData)
      for(i=0;i<_numPts;i++) _yData[i]  = oo + sc*(_yData[i] - io);
     _yo =  oo + sc*(_yo - io);
     _zeit = outkey;
     break;
    case(ModBaseData::secondary):
     if(_sData)
      for(i=0;i<_numPts;i++) _sData[i]  = oo + sc*(_sData[i] - io);
     _so =  oo + sc*(_so - io);
     _shdr = outkey;
     break;
    case(ModBaseData::tertiary):
     if(_tData)
      for(i=0;i<_numPts;i++) _tData[i]  = oo + sc*(_tData[i] - io);
     _thdr = outkey;
     break;
 }

 return 1;
}

