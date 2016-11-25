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

//---------------------- slp_label.cc ----------------------------------//
//---------------------- slp_label.cc ----------------------------------//
//---------------------- slp_label.cc ----------------------------------//

//         implementation file for the SLpLabel derived class
//                      subdirectory sl

#include "sl/slp_label.hh"
#include <Xm/Label.h>
 

//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//


SLpLabel::SLpLabel (SLDelay *slparent, char *name, long ident, char *label)
    : SLpBase(slparent, name, xmLabelWidgetClass, ident, _INACTIVE)
{
  createCvarResource();
  createSenseResource();
  setupCvarValue(label);
  if(slparent->topWidget()) make();
}


SLpLabel::SLpLabel (Widget wparent, char *name, long ident, char *label)
    : SLpBase(wparent, name, xmLabelWidgetClass, ident, _INACTIVE)
{
  createCvarResource();
  createSenseResource();
  setupCvarValue(label);
  make();
}


SLpLabel::SLpLabel (Widget w, long ident, char *label)
    : SLpBase(w, ident, _INACTIVE)
{
  createCvarResource();
  createSenseResource();
  setupCvarValue(label);
  make();
}


//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLpLabel::~SLpLabel(void)
{
}



//---------------------- other functions --------------------//
//---------------------- other functions --------------------//
//---------------------- other functions --------------------//


void SLpLabel::setCvarResource(void)
{
  setCompoundStringResource(XmNlabelString);
}


Widget SLpLabel::make(Widget p)
{
  if(!made())
     {
     Widget w = SLDelay::make(p);         
     if(!w)
        {
        Arg args[5];
        int i = 0;
        w = XmCreateLabel(wParent(), (char*)instanceName(), args, i);
        setTopWidget(w);
        XtManageChild(w);
        }
     // install_help was added on February 11, 2000 by Tom and Bob.
     // Tom later had second thoughts about the advisability of 
     // this addition, particularly for the potentially large number
     // of labels that would trigger help and for which no help
     // text would exist.  The install_help for labels was removed
     // by Bob on February 14, 2000.
     //install_help();
     updateSelf(TRUE);
     }
  return topWidget();
}


//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
