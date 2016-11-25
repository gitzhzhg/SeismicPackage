
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
//---------------------- fg_group.cc ------------------------//
//---------------------- fg_group.cc ------------------------//
//---------------------- fg_group.cc ------------------------//

//           implementation file for the FgGroup class
//                   not derived from any class
//                        subdirectory geom


#include "geom/fg_group.hh"
#include "cprim.h"
#include "named_constants.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


FgGroup::FgGroup()
         :
             _group                (INIL),
             _ixpp                 (-1),
             _pp_card              (NULL),
             _ixg                  (-1),

             _ixl_source           (-1),        // source
             _ixf_source           (-1),        // source
             _sline                (NULL),      // source
             _sflag                (NULL),      // source
             _source_error         (TRUE),

             _ixl_chan1            (-1),        // receiver
             _ixf_chan1            (-1),        // receiver
             _chan1_line           (INIL),      // receiver
             _chan1_mgp            (INIL),      // receiver
             _rec_error            (TRUE),

             _source_xloc          (DNIL),      // midpoint
             _source_yloc          (DNIL),      // midpoint
             _ixf_source_closest   (-1),        // midpoint
             _source_mgp_closest   (INIL),      // midpoint
             _cmp_error            (TRUE),

             _unplaced             (0)          // receiver
{
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


FgGroup::~FgGroup()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
