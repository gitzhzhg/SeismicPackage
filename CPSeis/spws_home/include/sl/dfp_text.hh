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


//------------------------- dfp_text.hh -------------------------------//
//------------------------- dfp_text.hh -------------------------------//
//------------------------- dfp_text.hh -------------------------------//

//              header file for the DFpText class
//                derived from the DFpBase class
//                      subdirectory sl

#ifndef _DFP_TEXT_HH_
#define _DFP_TEXT_HH_

#include "sl/dfp_base.hh"


class DFpText : public DFpBase
{

//---------------------- data -------------------------//
//---------------------- data -------------------------//
//---------------------- data -------------------------//

// no data

//--------------------- functions -------------------------//
//--------------------- functions -------------------------//
//--------------------- functions -------------------------//

public:

  DFpText (SLDatabox *databox, char *name, long ident,
    long type = _CHAR,
    long irow = 0, long icol = 0, long nchar = 0, long ndec = 999);

  virtual ~DFpText (void);

  void showNormalAppearance      () { _iswtrue = 1; _iswfalse = -22;
                                      updateSelf(TRUE); }
  void showLabelAppearance       () { _iswtrue = 0; _iswfalse = -55;
                                      updateSelf(TRUE); }
  void showFramedLabelAppearance () { _iswtrue = 5; _iswfalse =  -5;
                                      updateSelf(TRUE); }
  void showWhiteAppearance       () { _iswtrue = -33; _iswfalse = -44;
                                      updateSelf(TRUE); }
  void showColoredAppearance     () { _iswtrue = -34; _iswfalse = -77;
                                      updateSelf(TRUE); }

//--------------------- end of functions ------------------------//
//--------------------- end of functions ------------------------//
//--------------------- end of functions ------------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
