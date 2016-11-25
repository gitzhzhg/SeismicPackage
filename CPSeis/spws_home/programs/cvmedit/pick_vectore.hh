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
#ifndef _PICK_VECTORE_HH
#define _PICK_VECTORE_HH

#include "plot/pick_base.hh"
#include "pick_vector.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

class VectorListData;
class PlotBase;

class PickVectorE : public PickVector
{
  public:

//    PickVectorE() { assert(False); }
    PickVectorE(VectorListData *vldata, PlotBase *plot);
    ~PickVectorE();

  protected:

    void noModButtonOnePress    (int x , int y );
    void noModButtonOneMotion   (int x1, int x2, int y1, int y2);
    void noModButtonOneRelease  (int x1, int x2, int y1, int y2);

  private:

};

#endif
