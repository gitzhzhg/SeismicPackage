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

//------------------------ vfpop_read.hh ---------------------------------//
//------------------------ vfpop_read.hh ---------------------------------//
//------------------------ vfpop_read.hh ---------------------------------//

//                header file for the VfpopRead class
//                  derived from the SLDialog class
//                  derived from the VfInform class
//                         subdirectory vfgui


#ifndef _VFPOP_READ_HH_
#define _VFPOP_READ_HH_

#include "sl/sl_dialog.hh"
#include "vf/vf_inform.hh"


class VfpopRead : public SLDialog, public VfInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class VfManager       *_manager;     // pointer to external data object.
  class VfFileBase      *_file;        // owned by this class.
  class SLFileChoice    *_choice;      // owned by this class.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  VfpopRead  (SLDelay *slparent, char *name, class VfManager *manager,
                       class ContainerList *clist);

  virtual ~VfpopRead  ();

  VfManager      *getManager      ()  const  { return _manager; }
  VfFileBase     *getFileBase     ()  const  { return _file; }

public:      // this overrides SLDelay.

  Boolean notifyComplex(SLDelay *sender, int ident);

private:     // these override SLDialog.

  virtual Boolean preManageNotify();
  virtual void    postManageNotify();
  virtual void    postUnmanageNotify();
  virtual Boolean okNotify();
  virtual void    applyNotify();
  virtual void    undoNotify();
  virtual Boolean cancelNotify();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
