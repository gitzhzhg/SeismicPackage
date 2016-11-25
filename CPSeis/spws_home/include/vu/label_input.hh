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
#ifndef LABELINPUT_HH
#define LABELINPUT_HH


#include "sl/sl_form_pop.hh"

class SeisLabel;
class SL2Text;
class Vector;
class SLFontSelect;
class SLRadioBox;
class SLColorOption;


class LabelInput : public SLFPopSep {
    protected:
       virtual void    DoAction();
       virtual void    UndoInput();
       virtual Boolean ValidInput();

       SeisLabel      *_sl;
       SL2Text        *_label;
       Vector         *_current_vect;
       SLFontSelect   *_font;
       SLRadioBox     *_ltype;
       SLColorOption  *_color;
       Boolean        _new_label; 

    public:
       LabelInput( Widget    p,
                   char      *name,
                   HelpCtx   hctx,
                   SeisLabel *sl );
       
       ~LabelInput();
       virtual Widget make(Widget p);
       virtual void manage();
       void setCurrentVector(Vector *v) {_current_vect= v;}
};



#endif

