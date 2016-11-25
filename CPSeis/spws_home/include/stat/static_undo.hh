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

//--------------------------- static_undo.hh --------------------------//
//--------------------------- static_undo.hh --------------------------//
//--------------------------- static_undo.hh --------------------------//

//              header file for the StaticUndo class
//                derived from the UndoBase class
//                         subdirectory stat

   // This class maintains an "undo file" for a static dataset.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _STATIC_UNDO_HH_
#define _STATIC_UNDO_HH_


#include "oprim/undo_base.hh"


class StaticUndo : public UndoBase
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  class StaticKernal *_kernal;


//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public: // constructor and destructor.

           StaticUndo (class StaticKernal *kernal);
  virtual ~StaticUndo ();

private:   // overriding virtual functions.
           // these return error = TRUE or FALSE.

  virtual int  virtualSaveUndoFile    (const char *filename);
  virtual int  virtualReadUndoFile    (const char *filename);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
