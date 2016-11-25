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

//------------------------ vfpop_edit_base.hh -----------------------------//
//------------------------ vfpop_edit_base.hh -----------------------------//
//------------------------ vfpop_edit_base.hh -----------------------------//

//                header file for the VfpopEditBase class
//                  derived from the SLDialog class
//                         subdirectory vfgui

   // This is the base class for dialog boxes designed to
   // edit the active velocity dataset.  Derived classes must
   // do these two things in the constructor:
   //  (1) add children to _editor, which has already been created
   //        in the base class constructor.
   //  (2) create the object _edit of a class derived from VfEditBase.
   // The _edit object will be deleted in the base class destructor.


#ifndef _VFPOP_EDIT_BASE_HH_
#define _VFPOP_EDIT_BASE_HH_

#include "sl/sl_dialog.hh"
#include <X11/Intrinsic.h>


class VfpopEditBase : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

protected:

  class VfManager       *_manager;   // pointer to data object.
  class VfEditBase      *_edit;      // owned by this class.
  class SLSmartForm     *_editor;    // owned by this class.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  VfpopEditBase  (SLDelay *slparent, char *name,
                  VfManager *manager, class ContainerList *clist,
                  const char *helpcode);

  virtual ~VfpopEditBase  ();

  VfManager      *manager         ()  const  { return _manager; }
  VfEditBase     *edit            ()  const  { return _edit; }

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
