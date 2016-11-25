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

//------------------------ fg_group.hh -----------------------//
//------------------------ fg_group.hh -----------------------//
//------------------------ fg_group.hh -----------------------//

//             header file for the FgGroup class
//                 not derived from any class
//                    subdirectory geom


     //  This class contains information about one source
     //  ("group") in field geometry.
     //  This class is ignorent of the information it contains.


#ifndef _FG_GROUP_HH_
#define _FG_GROUP_HH_


class FgGroup
{

  friend class FgTraceValues;

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  long             _group;
  long             _ixpp;
  class PpCard    *_pp_card;
  long             _ixg;

  long             _ixl_source;          // source
  long             _ixf_source;          // source
  class SeisLine  *_sline;               // source
  class FieldFlag *_sflag;               // source
  int              _source_error;

  long             _ixl_chan1;           // receiver
  long             _ixf_chan1;           // receiver
  long             _chan1_line;          // receiver
  long             _chan1_mgp;           // receiver
  int              _rec_error;

  double           _source_xloc;         // midpoint
  double           _source_yloc;         // midpoint
  long             _ixf_source_closest;  // midpoint
  long             _source_mgp_closest;  // midpoint
  int              _cmp_error;

  long             _unplaced;            // receiver
/*
  long             _field_file;          // additional
  long             _source_line;         // additional
  float            _source_shot;         // additional
  float            _source_static;       // additional
  double           _source_dist;         // additional
  long             _source_cgp;          // additional
  long             _source_mgp;          // additional
  float            _source_elev;         // additional
  float            _source_hd;           // additional
  float            _source_tuh;          // additional
*/

//------------------ functions ----------------------------------//
//------------------ functions ----------------------------------//
//------------------ functions ----------------------------------//

public:      // constructor and destructor.

  FgGroup();
  virtual ~FgGroup();

  long getIxpp           ()  const  { return _ixpp; }
  long getIxlSource      ()  const  { return _ixl_source; }
  long getIxfSource      ()  const  { return _ixf_source; }
  long getIxlChan1       ()  const  { return _ixl_chan1; }
  long getIxfChan1       ()  const  { return _ixf_chan1; }
  long numUnplacedTraces ()  const  { return _unplaced; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
