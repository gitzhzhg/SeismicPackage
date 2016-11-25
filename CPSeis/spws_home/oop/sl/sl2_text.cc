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

//---------------------- sl2_text.cc ---------------------------------//
//---------------------- sl2_text.cc ---------------------------------//
//---------------------- sl2_text.cc ---------------------------------//

//         implementation file for the SL2Text class
//            derived from the SLSmartForm class
//                      subdirectory sl

#include "sl/sl2_text.hh"
#include <Xm/Xm.h>


//---------------- constructors -----------------------------------//
//---------------- constructors -----------------------------------//
//---------------- constructors -----------------------------------//

SL2Text::SL2Text (SLDelay *slparent, char *name, long ident,
                  char *label, long type, long nchar, long ndec)
           : SLSmartForm(slparent, name),
              _label (NULL),
              _text  (NULL)
{
  _label = new SLpLabel(this, "label", -ident, label);
  _text  = new SLpText (this, "text" ,  ident, type, nchar, ndec);
  attach(_label,   this, NULL, this, this);
  attach(_text , _label, this, this, this);
}


SL2Text::SL2Text (Widget   wparent, char *name, long ident,
                  char *label, long type, long nchar, long ndec)
           : SLSmartForm(wparent, name),
              _label (NULL),
              _text  (NULL)
{
  _label = new SLpLabel(this, "label", -ident, label);
  _text  = new SLpText (this, "text" ,  ident, type, nchar, ndec);
  attach(_label,   this, NULL, this, this);
  attach(_text , _label, this, this, this);
}



//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SL2Text::~SL2Text(void)
{
  delete _label;
  delete _text;
}


//--------------------- make -----------------------//
//--------------------- make -----------------------//
//--------------------- make -----------------------//

Widget SL2Text::make(Widget p)
{
  if(!made())
     {
     Widget w = SLSmartForm::make(p);
     XmRemoveTabGroup(w);     
     }
  makeChildren();
  return topWidget();
}



//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
