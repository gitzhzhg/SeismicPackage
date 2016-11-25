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

//---------------------- sl_smart_form.cc -----------------------//
//---------------------- sl_smart_form.cc -----------------------//
//---------------------- sl_smart_form.cc -----------------------//

//        implementation file for the SLSmartForm class
//                derived from the SLDelay class
//                      subdirectory sl


#include "sl/sl_smart_form.hh"
#include "sl/attachment_list.hh"


//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//


SLSmartForm::SLSmartForm(  SLDelay *slparent,
                           char    *name,
                           HelpCtx  hctx,
                           Boolean  doframe,
                           Boolean  make_if_can,
                           Boolean  manage_now  )
            : SLDelay(slparent,name, hctx, doframe),
                 _manage_now        (manage_now),
                 _show_even_spacing (FALSE)
{
  if     (slparent->made() && make_if_can) make();
  else if(slparent->topWidget()) supportUnmadeDefaults(slparent->topWidget());
  else if(slparent->pW       ()) supportUnmadeDefaults(slparent->pW());
}


SLSmartForm::SLSmartForm(  Widget   wparent,
                           char    *name,
                           HelpCtx  hctx,
                           Boolean  doframe,
                           Boolean  make_now,
                           Boolean  manage_now  )
            : SLDelay(wparent, name, hctx, doframe),
                 _manage_now        (manage_now),
                 _show_even_spacing (FALSE)
{
  if(make_now) make();
  else supportUnmadeDefaults(wparent);
}


SLSmartForm::SLSmartForm(  Widget   w,
                           HelpCtx  hctx,
                           Boolean  make_now,
                           Boolean  manage_now  )
            : SLDelay(XtName(w), hctx),
                 _manage_now        (manage_now),
                 _show_even_spacing (FALSE)
{
  setTopWidget(w);
  if(make_now) make();
}


SLSmartForm::~SLSmartForm()
{
  unattach(topWidget());
}



//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//

Widget SLSmartForm::make(Widget p)
{
  if(!made())
       {
       Widget w = SLDelay::make(p);
       if(!w)
           {
           w = XmCreateForm(makeFrameIfNeeded(wParent()), _name, NULL, 0);
           setTopWidget(w);
           }
       if(_manage_now) XtManageChild(w);
       if(_show_even_spacing) add_spacing_event_handler(w);
       }
  makeChildren();
  _attlist.tryAllAttachments();
  return topWidget();
}


//--------------------- show even spacing ----------------------//
//--------------------- show even spacing ----------------------//
//--------------------- show even spacing ----------------------//

void SLSmartForm::showEvenSpacing()
{
  if(_show_even_spacing) return;
  _show_even_spacing = TRUE;
  if(made()) add_spacing_event_handler(topWidget());
}


//------------------------- attach -----------------------------//
//------------------------- attach -----------------------------//
//------------------------- attach -----------------------------//

void SLSmartForm::attach(SLDelay *gui, SLDelay *left, SLDelay *right ,
                                       SLDelay *top , SLDelay *bottom,
                                       int oleft, int oright ,
                                       int otop , int obottom)
{
  _attlist.add(gui, left,  right,  top,  bottom,
                   oleft, oright, otop, obottom);
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
