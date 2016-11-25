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
#ifndef _PICK_VLDATA
#define _PICK_VLDATA

#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "oprim/modbase_data.hh"
#include "plot/pick_base.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

class PlotBase;
class VectorListData;

class PickVLData : public PickBase
{
  public:

//    PickVector() { assert(False); }
    PickVLData(VectorListData *vldata, PlotBase *plot);
    ~PickVLData();
    void setEditVector(Vector *vector);


  protected:

    VectorListData   *_vldata;
    VectorLinkedList *_vectors;
    int               _editIndex;
    Vector           *_rbnVector;
    ModBaseData      *_editData;
    ModBaseData      *_rbnData;
    float            *_movingX;
    float            *_movingY;
    int               _numMoving;
    unsigned int      _oldWidth;

    void shiftButtonOnePress    (int x , int y  );
    void shiftButtonOneMotion   (int x1, int x2,int y1 ,int y2);
    void shiftButtonOneRelease  (int x1, int x2, int y1,int y2);
    void noModButtonTwoPress    (int x , int y );
    void cntlButtonOnePress    (int x , int y  );

    void setData(VectorListData   *data);
    ModBaseData *startData(ModBaseData *, int , int , float *, float *);
    void MapToWorld(int xx, int yy,
     float *p, float *s, float *t, float *zeit);

  private:

};

#endif


