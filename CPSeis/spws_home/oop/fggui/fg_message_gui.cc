
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
//---------------------- fg_message_gui.cc ------------------------//
//---------------------- fg_message_gui.cc ------------------------//
//---------------------- fg_message_gui.cc ------------------------//

//         implementation file for the FgMessageGui class
//               derived from the SLSmartForm class
//                derived from the FgInform class
//                        subdirectory fggui


#include "fggui/fg_message_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/slp_text.hh"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//


FgMessageGui::FgMessageGui(  SLDelay             *slparent,
                             char                *name,
                             class FieldGeometry *fg,
                             HelpCtx              hctx,
                             Boolean              doframe,
                             Boolean              make_if_can,
                             Boolean              manage_now  )
       : SLSmartForm(slparent, name, hctx, doframe, make_if_can, manage_now),
         FgInform(fg),
                 _text        (NULL),
                 _clear_flag  (FALSE)
{
  constructorHelper();
}



FgMessageGui::FgMessageGui(  Widget               wparent,
                             char                *name,
                             class FieldGeometry *fg,
                             HelpCtx              hctx,
                             Boolean              doframe,
                             Boolean              make_now,
                             Boolean              manage_now  )
       : SLSmartForm(wparent, name, hctx, doframe, make_now, manage_now),
         FgInform(fg),
                 _text        (NULL),
                 _clear_flag  (FALSE)
{
  constructorHelper();
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


FgMessageGui::~FgMessageGui()
{
}




//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//

      // SLDelay does not have to be made

static Widget get_any_widget(SLDelay *gui)
{
  assert(gui);
  if(gui->W      ()) return gui->W      ();
  if(gui->wParent()) return gui->wParent();
  return get_any_widget(gui->slParent());
}



//--------------------- default resources --------------------//
//--------------------- default resources --------------------//
//--------------------- default resources --------------------//

static char *defres[] = {
           "*fg_message_gui.foreground: brown"  ,
/*
           "*fg_message_gui.foreground: dark orange"  ,
           "*fg_message_gui.fontList: fixed",
*/
           "*fg_message_gui.fontList: 8x13bold",
            NULL };

static char *defres1[] = {
            ".foreground: brown",
/*
            ".foreground: dark orange",
*/
            ".fontList:   8x13bold",
            NULL };



//------------------- constructor helper ----------------------//
//------------------- constructor helper ----------------------//
//------------------- constructor helper ----------------------//

        // private.

void FgMessageGui::constructorHelper()
{
  Widget any = get_any_widget(this);
  setDefRes(XtDisplay(any), "fg_message_gui", defres1);
  setDefRes(XtDisplay(any), (char*)instanceName(), defres);

  _text = new SLpText(this, "fg_message_gui", 0,
                                         SLpText::_CHAR, 55);

  _text->showLabelAppearance();
  _text->flushWhenValueChanges();

//                 left     right   top    bottom

  attach(_text   , this  ,  this ,  this , this);

  if(made()) make();
}



//--------------- overriding virtual functions -------------------//
//--------------- overriding virtual functions -------------------//
//--------------- overriding virtual functions -------------------//


void FgMessageGui::showMessage(FieldGeometry*, char *msg)
{
  const char *old = _text->cvar();
  if(strings_equal(msg, (char*)old)) _text->setCvar(" ");
  _text->setCvar(msg);
  _clear_flag = FALSE;
}



void FgMessageGui::returningToEventLoop(FieldGeometry*)
{
  if(_clear_flag) _text->setCvar(" ");
  _clear_flag = TRUE;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

