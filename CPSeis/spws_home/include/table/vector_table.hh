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


#ifndef _VECTOR_TABLE_HH_
#define _VECTOR_TABLE_HH_


#include "table_base.hh"


#define MAXPTS       100
#define MAXNUMKEYS   5
#define MAXCHAR      15
//#define INTEGER      1


class Vector;


class VectorTable : public TableBase
{

public:

  
  VectorTable( class Vector  *vectorData );
  virtual ~VectorTable();

  char   *getName()    { return _name   ; }
  char   *getColor()   { return _color  ; }
//  char   *getMarker()  { return _marker  ; }
  int     getWidth()   { return _width  ; }
  int     getNumPts()  { return _numPts ; }

  float   getx( int index )          { return _x[index] ; }
  float   gety( int index )          { return _y[index] ; }
  void    setx( int index, float x ) { _x[index] = x ; }
  void    sety( int index, float y ) { _y[index] = y ; }

//  VectorStyle       getStyle()  { return _style  ; }
  Vector      *getVectorData()  { return _vectorData ; }
  void modDone( );

//--------------------- data ---------------------------//

protected:

  long    _width, _numPts, _nKey, _n, _sKeyFlg, _tKeyFlg, _uKeyFlg;
  long    _data_type[MAXNUMKEYS];
  char    _name[MAXCHAR], _color[MAXCHAR];
  char    _varName[MAXNUMKEYS][MAXCHAR];
  int     _switch[MAXNUMKEYS], _nchar[MAXNUMKEYS], _ndec[MAXNUMKEYS];
  float   _x[MAXPTS],  _y[MAXPTS];
  float   _sKey[MAXPTS], _tKey[MAXPTS], _userData[MAXPTS];
  Vector *_vectorData;
 
//--------------------- functions ---------------------------//

private:

  virtual void contents(void);
};

#endif

//---------------------------- end ------------------------------//



