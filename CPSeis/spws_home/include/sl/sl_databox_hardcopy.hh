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

//--------------------- sl_databox_hardcopy.hh --------------------------//
//--------------------- sl_databox_hardcopy.hh --------------------------//
//--------------------- sl_databox_hardcopy.hh --------------------------//

//          header file for the SLDataboxHardcopy class
//               derived from the SLDialog class
//                       subdirectory sl

        // Create this class when ready to save a hardcopy file.
        // This class deletes itself when finished.


#ifndef _SL_DATABOX_HARDCOPY_HH_
#define _SL_DATABOX_HARDCOPY_HH_

#include "sl/sl_dialog.hh"


class SLDataboxHardcopy : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class SLDatabox *_databox;
  char             _filename[222];

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  SLDataboxHardcopy  (SLDelay *slparent, class SLDatabox *databox,
                      int numlines, const char *filename);

  virtual ~SLDataboxHardcopy  ();

  SLDatabox  *getDatabox  ()  const  { return _databox; }
  const char *getFilename ()  const  { return _filename; }

  void        setFilename (const char *filename);

private:   // overriding SLDialog.

  virtual Boolean okNotify();
  virtual Boolean cancelNotify();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
