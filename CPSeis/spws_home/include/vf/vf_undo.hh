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

//--------------------------- vf_undo.hh --------------------------//
//--------------------------- vf_undo.hh --------------------------//
//--------------------------- vf_undo.hh --------------------------//

//                header file for the VfUndo class
//                derived from the UndoBase class
//                         subdirectory vf

   // This class maintains an "undo file" for a velocity dataset.
   // This undo file might be internal storage or a velocity workfile.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _VF_UNDO_HH_
#define _VF_UNDO_HH_


#include "oprim/undo_base.hh"


class VfUndo : public UndoBase
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  class VfKernal      *_kernal;        // pointer to external object.
  class HistoryCards  *_history;       // pointer to external object.
  class VfKernal      *_storage;       // owned by this class.
  class HistoryCards  *_hstorage;      // owned by this class.
  class VfDiskfile    *_diskfile;      // owned by this class.


//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public: // constructor and destructor.

           VfUndo (VfKernal *kernal, HistoryCards *history);
  virtual ~VfUndo ();

private:   // overriding virtual functions.
           // these return error = TRUE or FALSE.

  virtual int  virtualSaveUndoFile    (const char *filename);
  virtual int  virtualReadUndoFile    (const char *filename);
  virtual void notifyRemovingUndoFile (const char *filename);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
