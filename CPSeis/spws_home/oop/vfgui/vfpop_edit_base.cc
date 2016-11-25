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

//------------------------- vfpop_edit_base.cc ---------------------------//
//------------------------- vfpop_edit_base.cc ---------------------------//
//------------------------- vfpop_edit_base.cc ---------------------------//

//        implementation file for the VfpopEditBase class
//                 derived from the SLDialog class
//                       subdirectory vfgui


#include "vfgui/vfpop_edit_base.hh"
#include "vfgui/vfgui_status.hh"
#include "vf/vf_edit_base.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <stream.h>
#include <assert.h>



//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.

void VfpopEditBase::postUnmanageNotify()
{
  _manager->maybeDeleteUndoFiles(this);
}


void VfpopEditBase::applyNotify()
{
  assert(_edit);
  _manager->activeDataset()->editDataset(_edit, this);
}


void VfpopEditBase::undoNotify()
{
  _manager->activeDataset()->maybeReadUndoFile(this);
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//


static long apply_sense_upfun(void *data)
{
  VfpopEditBase *pop = (VfpopEditBase*)data;
  return pop->manager()->activeDataset()->isEditable();
}


static long undo_sense_upfun(void *data)
{
  VfpopEditBase *pop = (VfpopEditBase*)data;
  if(pop->manager()->activeDataset()->allowReadDeleteUndoFile(pop))
                                                           return TRUE;
  return FALSE;
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        "*vfpop_editor.borderWidth:         2",
        "*vfpop_editor*background:          powder blue",
            NULL };



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopEditBase::VfpopEditBase (SLDelay *slparent, char *name,
                              VfManager *manager, ContainerList *clist,
                              const char *helpcode)
            : SLDialog(slparent, name, NULL, FALSE),
                    _manager    (manager),
                    _edit       (NULL),
                    _editor     (NULL)
{
  assert(manager && clist && helpcode);
  setFallbackResources(defres);

/////// populate work area:

  SLSmartForm   *work = workArea();
  VfguiStatus *status = new VfguiStatus(work, _manager, clist);
              _editor = new SLSmartForm(work, "vfpop_editor");

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(status , work   , work ,  work  ,  NULL,  0,  0);
  work->attach(_editor, work   , work ,  status,  work,  0,  0, 10);

/////// populate bottom area:

  SLpPush *apply  = addBottomApply  ();
  SLpPush *undo   = addBottomUndo   ();
                    addBottomRemove ();
                    addBottomHelp   ((char*)helpcode);

  apply ->setupSenseFun          (apply_sense_upfun, this);
  undo  ->setupSenseFun          (undo_sense_upfun , this);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopEditBase::~VfpopEditBase()
{
  assert(_edit);
  delete _edit;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
