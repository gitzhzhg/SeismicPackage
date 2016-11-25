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

//------------------------- vfpop_edit.cc ---------------------------//
//------------------------- vfpop_edit.cc ---------------------------//
//------------------------- vfpop_edit.cc ---------------------------//

//           implementation file for the VfpopEdit class
//                 derived from the SLDialog class
//                       subdirectory vfgui


#include "vfgui/vfpop_edit.hh"
#include "vfgui/vfgui_status.hh"
#include "vfgui/vfgui_edit_base.hh"

////#include "vfgui/vfgui_set.hh"
////#include "vfgui/vfgui_vfid.hh"
////#include "vfgui/vfgui_headers.hh"
    #include "vfgui/vfgui_resample.hh"
////#include "vfgui/vfgui_latsample.hh"
////#include "vfgui/vfgui_raytrace.hh"
////#include "vfgui/vfgui_delete.hh"
////#include "vfgui/vfgui_mult.hh"
////#include "vfgui/vfgui_misc.hh"

#include "vf/vf_edit_base.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include "str.h"
#include <stream.h>
#include <iostream.h>
#include <assert.h>



//----------------------------- macro ----------------------------//
//----------------------------- macro ----------------------------//
//----------------------------- macro ----------------------------//


#define MAKE(match, VfguiXxxx)                                   \
                                                                 \
  if(strcmp(name, match) == 0)                                   \
      {                                                          \
      return new VfguiXxxx(work, _manager->utilities());         \
      }



//-------------------------- build --------------------------------//
//-------------------------- build --------------------------------//
//-------------------------- build --------------------------------//

       // private.
       // creates and returns editor derived from VfguiEditBase.

VfguiEditBase *VfpopEdit::build (const char *name, SLDelay *work)
{

////  MAKE ("types"    , VfguiTypes    )
////  MAKE ("vfid"     , VfguiVfid     )
////  MAKE ("headers"  , VfguiHeaders  )
      MAKE ("resample" , VfguiResample )
////  MAKE ("latsample", VfguiLatsample)
////  MAKE ("raytrace" , VfguiRaytrace )
////  MAKE ("delete"   , VfguiDelete   )
////  MAKE ("multiply" , VfguiMultiply )
////  MAKE ("misc"     , VfguiMisc     )

  return NULL;
}




//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.

void VfpopEdit::postUnmanageNotify()
{
  _manager->maybeDeleteUndoFiles(this);
}


void VfpopEdit::applyNotify()
{
  _manager->activeDataset()->editDataset(_editor->edit(), this);
}


void VfpopEdit::undoNotify()
{
  _manager->activeDataset()->maybeReadUndoFile(this);
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//


static long apply_sense_upfun(void *data)
{
  VfpopEdit *pop = (VfpopEdit*)data;
  return pop->manager()->activeDataset()->isEditable();
}


static long undo_sense_upfun(void *data)
{
  VfpopEdit *pop = (VfpopEdit*)data;
  if(pop->manager()->activeDataset()->allowReadDeleteUndoFile(pop))
                                                           return TRUE;
  return FALSE;
}



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopEdit::VfpopEdit(SLDelay *slparent, char *name, VfManager *manager,
                                ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                    _manager    (manager),
                    _editor     (NULL)
{
  assert(_manager);

/////// populate work area:

  SLSmartForm *work   = workArea();
  VfguiStatus *status = new VfguiStatus(work, _manager, clist);

  _editor = build(name, work);
  assert(_editor);
  assert(_editor->edit());

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(status , work   , work ,  work  ,  NULL,  0,  0);
  work->attach(_editor, work   , work ,  status,  work,  0,  0, 10);

/////// populate bottom area:

  char helpcode[80];
  strcpy(helpcode, name);
  strcat(helpcode, "_OVERVIEW");
/*
  convert_case_to_upper(helpcode, helpcode);
*/
  str_to_upper(helpcode, helpcode);

  SLpPush *apply  = addBottomApply  ();
  SLpPush *undo   = addBottomUndo   ();
                    addBottomRemove ();
                    addBottomHelp   (helpcode);

  apply ->setupSenseFun          (apply_sense_upfun, this);
  undo  ->setupSenseFun          (undo_sense_upfun , this);

  update();
}




//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopEdit::~VfpopEdit()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
