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

//-------------------------- sl_row.cc -------------------------------//
//-------------------------- sl_row.cc -------------------------------//
//-------------------------- sl_row.cc -------------------------------//

//            implementation file for the SLRow class
//               derived from the SLRowColumn class
//                        subdirectory sl


#include "sl/sl_row.hh"


//--------------------- constructors and destructors ----------------//
//--------------------- constructors and destructors ----------------//
//--------------------- constructors and destructors ----------------//


SLRow::SLRow(  SLDelay *slparent,
               char    *name,
               HelpCtx  hctx,
               Boolean  doframe,
               Boolean  make_if_can,
               Boolean  manage_now  )
       : SLRowColumn(slparent, name, hctx, doframe,
                                     make_if_can, manage_now,
                                     SLRowColumn::_HORIZONTAL)
{
}


SLRow::SLRow(  Widget   wparent,
               char    *name,
               HelpCtx  hctx,
               Boolean  doframe,
               Boolean  make_now,
               Boolean  manage_now  )
       : SLRowColumn(wparent, name, hctx, doframe,
                                    make_now, manage_now,
                                    SLRowColumn::_HORIZONTAL)
{
}


SLRow::SLRow(  Widget   w,
               HelpCtx  hctx,
               Boolean  make_now,
               Boolean  manage_now  )
       : SLRowColumn(w, hctx,
                        make_now, manage_now,
                        SLRowColumn::_HORIZONTAL)
{
}


SLRow::~SLRow()
{
}


//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
