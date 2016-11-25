// display_processor_base.cc:  Impl. file for DisplayProcessorBase class
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

#include "dp/display_processor_base.hh"

DisplayProcessorBase::DisplayProcessorBase (int coding_levels) :
  RGBZGenerator (),
  Decoder (coding_levels),
  _n_tree             (0),
  _needs_initializing (1),
  _temp_array         (0),
  _error_status       (DPB_SUCCESSFUL)
{
}

DisplayProcessorBase::~DisplayProcessorBase ()
{
  reclaimMemory ();
}

int DisplayProcessorBase::failed ()
{
  return (int) (_error_status != DPB_SUCCESSFUL || RGBZGenerator::failed()
    || Decoder::failed());
}

GridErrorCodes DisplayProcessorBase::errorStatus ()
{
  if (RGBZGenerator::failed())
    return RGBZGenerator::errorStatus ();
  else if (Decoder::failed())
    return Decoder::errorStatus ();
  else
    return _error_status;
}

void DisplayProcessorBase::reset ()
{
  reclaimMemory ();
  _needs_initializing = 1;
  _error_status = DPB_SUCCESSFUL;
  Decoder::reset ();
  RGBZGenerator::reset();
}

void DisplayProcessorBase::reclaimMemory ()
{
  if (_n_tree)     delete    _n_tree,     _n_tree     = 0;
  if (_temp_array) delete [] _temp_array, _temp_array = 0;
}
