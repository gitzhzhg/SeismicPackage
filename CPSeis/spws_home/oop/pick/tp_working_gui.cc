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

//------------------------ tp_working_gui.cc --------------------------//
//------------------------ tp_working_gui.cc --------------------------//
//------------------------ tp_working_gui.cc --------------------------//

//         implementation file for the TpWorkingGui class
//                 derived from the SLpLabel class
//                        subdirectory pick

    // Displays a one-line working message, then flushes the buffer.


#include "pick/tp_working_gui.hh"
#include "pick/tp_resources.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>


//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//

TpWorkingGui::TpWorkingGui(SLDelay *slparent, char *name)
                  : SLpText(slparent, name)
{
  TpResources::startup(this);
  showLabelAppearance();
  flushWhenValueChanges(TRUE);
  setCvar(" ");
}


TpWorkingGui::~TpWorkingGui()
{
}



//---------------------------- make ---------------------------//
//---------------------------- make ---------------------------//
//---------------------------- make ---------------------------//

static String defres[]= {
    "*fontList: 8x13bold",
    NULL };


Widget TpWorkingGui::make(Widget p)
{
  if(!made())
     {
     setDefaultResources(TpResources::getDisplay(), instanceName(), defres);
     Widget w = SLpText::make(p);
     Pixel pixel = TpResources::getPixelGreen();
     XtVaSetValues(topWidget(), XmNforeground,  pixel, NULL);
     }
  return topWidget();
}


//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
