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

//------------------- header_dump_vectors.hh ---------------------//
//------------------- header_dump_vectors.hh ---------------------//
//------------------- header_dump_vectors.hh ---------------------//

//           header file for the HeaderDumpVectors class
//            derived from the SeisVectLinkedList class
//                         subdirectory pick

#ifndef _HEADER_DUMP_VECTORS_HH_
#define _HEADER_DUMP_VECTORS_HH_

#include "vect/ll_seis_vect.hh"
#include <stdlib.h>

class SeisPlot;

class HeaderDumpVectors : public SeisVectLinkedList
{

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

private:

  class SLTableView        *_view;
  class Vector             *_sel_vector;
  class Vector             *_loc_vector;
  class VectData           *_sel_data;
  class VectData           *_loc_data;
  Boolean                   _visible;
  float                     _ymin;
  float                     _yperpix;

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:    // constructor and destructor

  HeaderDumpVectors(SeisPlot *sp, SLTableView *view);
  virtual ~HeaderDumpVectors();

public:    // other methods

  void makeVisible();
  void makeInvisible();
  void updateSelVector();
  void updateLocVector();

private:    // traps registered with SLTableView

  SeisPlot *_sp;
  static void selectTrap (void *data, long column, Boolean selected);
  static void locateTrap (void *data, long first, long last, long active);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
