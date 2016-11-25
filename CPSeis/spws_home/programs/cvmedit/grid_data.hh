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
//------------------- gridData.hh -------------------------//
//------------------- gridData.hh -------------------------//

//            header file for gridData
//                 derived from the BaseData class


#ifndef _GRID_DATA_H
#define _GRID_DATA_H

#include <stdio.h>
#include <string.h>
#include "glimits.h"


class gridData
{
 private:

 protected:
   char       *_garr;
   int         _owns_data;
   GridLimits *_glim;
   char        _word_string[8]; // FLOAT or BYTE
   int         _axis;
   int         _slice;

 public:

   gridData(float *garr,GridLimits *glim);
   gridData(char  *garr,GridLimits *glim);
   gridData(float *garr,
            int  n1,float o1, float d1,
            int  n2,float o2, float d2,
            int  n3,float o3, float d3,
            ErsTransform *t1, ErsTransform *t2, ErsTransform *t3);
   gridData(char *garr,
            int  n1,float o1, float d1,
            int  n2,float o2, float d2,
            int  n3,float o3, float d3,
            ErsTransform *t1, ErsTransform *t2, ErsTransform *t3);
   gridData *copy();
  ~gridData();

  int         getGridHiLo(float *lo, float *hi);
  void        setGridData(float *data) {_garr = (char *) data;}
  void        setGridData(char *data)  {_garr = data;}
  void       *getGridData() {return (void *) _garr;}

  void        setWord(char *ws) { if(ws) strcpy(_word_string,ws); }
  char       *getWord() { return _word_string; }
  void        setAxis(int ax) { if(ax >0 && ax < 4) _axis= ax; }
  int         Axis()  { return _axis; }
  void        setSlice(int sl) {  _slice = sl; }
  int         Slice() { return _slice; }
  void        setGlimData(GridLimits *glim);
  GridLimits *getGlimData() {return _glim;}
  void        getGridVals(int  *n1,float *o1,float *d1,
               int  *n2,float *o2,float *d2,int  *n3,float *o3,float *d3);
  void        getGridSize(int  *n1, int  *n2, int  *n3);
  int         getGridLabels(char **lab1, char **lab2, char **lab3);
  int         getGridCoords( ErsTransform **t1, ErsTransform **t2,
                             ErsTransform **t3);
  int         ownsData() { return _owns_data; }
  void        setGridIncs(float d1, float d2, float d3);
  void        setGridOrgs(float o1, float o2, float o3);
  void        setGridSizes(int  n1, int  n2, int  n3);
  int         setGridCoords(ErsTransform *t1,ErsTransform *t2,
                            ErsTransform *t3);
  void        gridDataTrans(ErsTransforms *tdata, char *n1,
                            char *n2, char *n3);
};


#endif


