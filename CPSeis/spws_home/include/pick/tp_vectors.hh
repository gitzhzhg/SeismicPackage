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

//------------------------ tp_vectors.hh --------------------------//
//------------------------ tp_vectors.hh --------------------------//
//------------------------ tp_vectors.hh --------------------------//

//              header file for the TpVectors class
//           derived from the SeisVectLinkedList class
//                       subdirectory pick

#ifndef _TP_VECTORS_HH_
#define _TP_VECTORS_HH_

#include "vect/ll_seis_vect.hh"
#include "pick/tp_resources.hh"
#include <stdlib.h>


class TpVectors : public SeisVectLinkedList
{

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

private:

  const int       _numvectors;    // number of vectors.
  long            _numpicks;      // number of picks.

  class VectYdata *_data  [TP_MAXNUMVECTORS]; // data for each vector.
  class Vector    *_vector[TP_MAXNUMVECTORS]; // each vector.
  Boolean      _visibility[TP_MAXNUMVECTORS]; // visibility of each vector.

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:    // constructor and destructor

  TpVectors(class SeisPlot *sp, const int numvectors,
                              float zero_pick, float missing_pick);
  virtual ~TpVectors();

public:

  void    setSpecialZeroValueAndCoord (float value, float coord);

  Boolean getVectorVisibility (int ivector)  const;
  void    setVectorVisibility (int ivector, Boolean visibility);
  void    redrawVector        (int ivector);

public:   //  move picks to/from vector data object

  long   getNumPicks()  const  { return _numpicks; }

  float  getSinglePick    (int ivector, long index)    const;
  void   getAllPicks      (int ivector, float *picks)  const;
  void   replaceAllPicks  (int ivector, float *picks, long n);
  void   getSomePicks     (int ivector, float *picks,
                                    long index, long numget)  const;
  void   replaceSomePicks (int ivector, float *picks,
                                    long index, long numrep);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
