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

//------------------------ vfpop_edit.hh -----------------------------//
//------------------------ vfpop_edit.hh -----------------------------//
//------------------------ vfpop_edit.hh -----------------------------//

//                header file for the VfpopEditBase class
//                  derived from the SLDialog class
//                         subdirectory vfgui

        // This is a popup designed to be used for editing
        // an object derived from VfEditBase.  This popup
        // creates an editor derived from VfguiEditBase.

        // This class is not to be derived from.  This class decides
        // what editor to create based on the name passed to the
        // constructor of this class.  The editor then creates
        // its own _edit object which it allows this class to access.


#ifndef _VFPOP_EDIT_HH_
#define _VFPOP_EDIT_HH_

#include "sl/sl_dialog.hh"
#include <X11/Intrinsic.h>


class VfpopEdit : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class VfManager      *_manager;  // pointer to data object.
  class VfguiEditBase  *_editor;   // owned by this class.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  VfpopEdit  (SLDelay *slparent, char *name, class VfManager *manager,
                       class ContainerList *clist);

  virtual ~VfpopEdit  ();

  VfManager    *manager  ()  const  { return _manager; }

private:     // creates and returns gui derived from VfguiEditBase.

  class VfguiEditBase *build (const char *name, SLDelay *work);

private:     // these override SLDialog.

  virtual void    postUnmanageNotify();
  virtual void    applyNotify();
  virtual void    undoNotify();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
