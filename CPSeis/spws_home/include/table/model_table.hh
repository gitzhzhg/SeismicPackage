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

//---------------------- model_table.hh ------------------------------//
//---------------------- model_table.hh ------------------------------//
//---------------------- model_table.hh ------------------------------//

//         header file for the ExampleThree class


#ifndef _MODEL_TABLE_HH_
#define _MODEL_TABLE_HH_


#include "table/table_base.hh"


#define MAXPTS       250
#define MAXROW       10
#define MAXNUMKEYS   5
#define MAXCHAR      15
#define INTEGER1      1


class ModBaseData;


class ModelTable : public TableBase
{

public:

  ModelTable( ModBaseData *modelData,   char *xTitle, char *yTitle,
              char *sTitle, char *tTitle, char *uTitle,
              long ident=BaseData::defaultId );
  virtual ~ModelTable();

  int    getNumPts()  { return _numPts ; }
  float  getX( int index ) { return _x[index] ; }
  float  getY( int index ) { return _y[index] ; }
  float  getS( int index ) { return _sKey[index] ; }
  float  getT( int index ) { return _tKey[index] ; }
  float  getU( int index ) { return _userData[index] ; }

  void   setNumPts( int numPts) {_numPts = numPts;}
  void   setX( int index, float x ) { _x[index] = x ; }
  void   setY( int index, float y ) { _y[index] = y ; }
  void   setS( int index, float s ) { _sKey[index] = s ; }
  void   setT( int index, float t ) { _tKey[index] = t ; }
  void   setU( int index, float u ) { _userData[index] = u ; }

  void   setXTitle (char *xTitle) { strcpy(_varName[0], xTitle); }
  void   setYTitle (char *yTitle) { strcpy(_varName[1], yTitle); }
  void   setSTitle (char *sTitle) { strcpy(_varName[2], sTitle); }
  void   setTTitle (char *tTitle) { strcpy(_varName[3], tTitle); }
  void   setUTitle (char *uTitle) { strcpy(_varName[4], uTitle); }


  ModBaseData *getModBaseData() { return (ModBaseData *) _modelData;  }
  void replace ( ModBaseData *newData );
  void modDone(BaseData *, long ident );

//--------------------- data ---------------------------//

protected:

  long         _numPts, _nKey, _n, _nmax, _isSkey, _isUkey, _isTkey;
  long         _data_type[MAXNUMKEYS];
  long         _ident;
  char         _varName[MAXNUMKEYS][MAXCHAR];
  int          _switch[MAXNUMKEYS], _nchar[MAXNUMKEYS], _ndec[MAXNUMKEYS];
  float        _x[MAXPTS],    _y[MAXPTS];
  float        _sKey[MAXPTS], _tKey[MAXPTS], _userData[MAXPTS];
  ModBaseData *_modelData;
 
//--------------------- functions ---------------------------//

private:

  virtual void contents(void);
};

#endif

//---------------------------- end ------------------------------//




