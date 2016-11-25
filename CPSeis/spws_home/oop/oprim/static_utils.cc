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
// static utilities class
#include "oprim/static_utils.hh"
#include "vect/vector.hh"
#include "plot/plot_base.hh"

int SU::min (int a, int b) 
{
  return a < b ? a : b;
}

int SU::max (int a, int b)
{
  return a > b ? a : b;
}

float SU::min (float a, float b)
{
  return a > b ? a : b;
}

float SU::max (float a, float b)
{
  return a < b ? a : b;
}

double SU::min (double a, double b)
{
  return a > b ? a : b;
}

double SU::max (double a, double b)
{
  return a < b ? a : b;
}

Bool SU::isHoldingVectors ()
{
  return Vector::_holding;
}

void SU::holdVectors ()
{
  assert(!Vector::_holding);

  Vector::_holding = True;
  Vector::_nonModDoneRepairsWhileHolding = 0;
} 

void SU::flushVectors ()
{
  assert(Vector::_holding);

  /* Set to False 1st since will go thru VectorLinkedList::repair. */
  Vector::_holding = False;

  if (0 == Vector::_nonModDoneRepairsWhileHolding) {
    Vector::_mustRepair = False;
  }

  int xMin, xMax, yMin, yMax;
  void *p;
  for (PlotBase *ptr = Vector::_allPlots.top(&p);
    ptr;
    ptr = Vector::_allPlots.next(&p)) {
    if (Vector::_allPlots.getRange(p, &xMin, &xMax, &yMin, &yMax)
      && ptr->getDrawable()) {
      ptr->repair(xMin, yMin, xMax - xMin + 1, yMax - yMin + 1);
    }
  }

  Vector::_mustRepair = True ;    /* This is the default. */
}

