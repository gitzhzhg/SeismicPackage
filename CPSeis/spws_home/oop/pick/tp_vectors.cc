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

//------------------------ tp_vectors.cc --------------------------//
//------------------------ tp_vectors.cc --------------------------//
//------------------------ tp_vectors.cc --------------------------//

//          implementation file for the TpVectors class
//           derived from the SeisVectLinkedList class
//                       subdirectory pick

//    This is a base class for all sets of vectors which are drawn
//    on a SeisPlot, with one point for each seismic trace.  See the
//    documentation in tp_popup_base.cc for more information.

//    The minimum number of vectors is TP_CURR          (one).
//    The maximum number of vectors is TP_MAXNUMVECTORS (currently five).
//    The actual  number of vectors is _numvectors      (set by constructor).
//    The ivector (index of a vector) must be between 0 and _numvectors-1.

#include <string.h>
#include "pick/tp_vectors.hh"
#include "pick/tp_resources.hh"
#include "vect/vector.hh"
#include "vect/vect_ydata.hh"
#include "sp/seis_plot.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>


static int   MARKER_SIZE    = 9;
static int   ZERO_MARKER    = Vector::FilledDiamondMarker;
static int   MISSING_MARKER = Vector::FilledSquareMarker;
static int   ZERO_OFFSET    = 5;
static int   MISSING_OFFSET = 1;


//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//

            // called from TpPopupBase

TpVectors::TpVectors(SeisPlot *sp, const int numvectors,
                                 float zero_pick, float missing_pick)
          : SeisVectLinkedList(),
                    _numvectors      (numvectors),
                    _numpicks        (0)
{
  assert(sp);
  assert(_numvectors >= 1 && _numvectors <= TP_MAXNUMVECTORS);
  addPlot(sp);

  int ivector;
  for(ivector = TP_CURR; ivector < TP_MAXNUMVECTORS; ivector++)
      {
      _data      [ivector] = NULL;
      _vector    [ivector] = NULL;
      _visibility[ivector] = FALSE;
      }
  for(ivector = _numvectors - 1; ivector >= TP_CURR; ivector--)
      {
      _data  [ivector] = new VectYdata(0, NULL);
      _vector[ivector] = add(_data[ivector],
                             TpResources::getColor(ivector),
                             TpResources::getLineWidth(),
                             FALSE, Vector::SolidLine,
                             Vector::DataSpecifiedMarker, MARKER_SIZE);
      _vector[ivector]->makeInvisible();
      _data  [ivector]->useSpecialMarker(zero_pick, ZERO_MARKER,
                                                    ZERO_OFFSET, TRUE);
      _data  [ivector]->useSpecialMarker(missing_pick, MISSING_MARKER,
                                                       MISSING_OFFSET, TRUE);
      _vector[ivector]->setXYOffsets(Vector::IsNotOffset,
                                     Vector::DataSpecifiedOffset);
      }
}


TpVectors::~TpVectors()
{
  for(int ivector = TP_CURR; ivector < _numvectors; ivector++)
     {
  ///   remove(_vector[ivector]);  // the base class destructor will do this.
     delete _data  [ivector];   // ok to do this before the vector goes away.
     }
}


//---------------- set special zero value and coord ----------------//
//---------------- set special zero value and coord ----------------//
//---------------- set special zero value and coord ----------------//

 // value  = special value in pick array (and in VectYdata).
 // coord  = coord location to plot the pick (returned by VectYdata::getY).

void TpVectors::setSpecialZeroValueAndCoord(float value, float coord)
{
  for(int ivector = 0; ivector < _numvectors; ivector++)
      {
      _data[ivector]->useSpecialMarker(value, ZERO_MARKER, coord, FALSE);
      }
}



//--------------- get and set vector visibility -----------------//
//--------------- get and set vector visibility -----------------//
//--------------- get and set vector visibility -----------------//


Boolean TpVectors::getVectorVisibility(int ivector)  const
{
  assert(ivector >= TP_CURR && ivector < _numvectors);
  // return _vector[ivector]->isVisible();
  return _visibility[ivector];
}


void TpVectors::setVectorVisibility(int ivector, Boolean visibility)
{
  assert(ivector >= TP_CURR && ivector < _numvectors);
  if(visibility) _vector[ivector]->makeVisible();
  else           _vector[ivector]->makeInvisible();
  _visibility[ivector] = visibility;
}


void TpVectors::redrawVector(int ivector)
{
  assert(ivector >= TP_CURR && ivector < _numvectors);
  _vector[ivector]->redraw();
}



//------------ move picks to/from vector data object ----------//
//------------ move picks to/from vector data object ----------//
//------------ move picks to/from vector data object ----------//


float TpVectors::getSinglePick(int ivector, long index) const
{
  assert(ivector >= TP_CURR && ivector < _numvectors);
  assert(index   >= 0       && index   < _numpicks  );
  return _data[ivector]->fetch((int)index);
}



//    the caller must know the number of picks:

void TpVectors::getAllPicks(int ivector, float *picks) const
{
  assert(ivector >= TP_CURR && ivector < _numvectors && picks);
  int n = _data[ivector]->getNumPts();
  assert (n == _numpicks);
  _data[ivector]->fetch(0, n, picks);
}



//    the number of picks can change:

//    If the number of picks changes, this method must be called
//    (with the same value of n) for each vector.  The "official"
//    number of picks is reset to the value of n.

void TpVectors::replaceAllPicks(int ivector, float *picks, long n)
{
  assert(ivector >= TP_CURR && ivector < _numvectors);
  int old_number = _data[ivector]->getNumPts();
  _data[ivector]->replace(0, old_number, (int)n, picks);
  _numpicks = n;
}



//    the caller must know the number of picks:

void TpVectors::getSomePicks(int ivector, float *picks,
                                long index, long numget) const
{
  assert(ivector >= TP_CURR && ivector < _numvectors && picks);
  int n = _data[ivector]->getNumPts();
  assert (n == _numpicks);
  _data[ivector]->fetch((int)index, (int)numget, picks);
}



//    the caller must know the number of picks:
//    the number of picks cannot change:

void TpVectors::replaceSomePicks(int ivector, float *picks,
                                    long index, long numrep)
{
  assert(ivector >= TP_CURR && ivector < _numvectors);
  int n = _data[ivector]->getNumPts();
  assert (n == _numpicks);
  _data[ivector]->replace((int)index, (int)numrep, picks);
}



//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
