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
#include <Xm/Label.h>
#include "cprim.h"
#include "vu/label_input.hh"
#include "vu/seis_label.hh"
#include "sl/sl2_text.hh"
#include "vect/vector.hh"
#include "sl/sl_font_select.hh"
#include "sl/sl_color_option.hh"
#include "sl/sl_radio_box.hh"

static SLPush colors_ary[] = {
                { "red",     1},
                { "orange",  2},
                { "purple",  3},
                { "blue",    4},
                { "white",   5},
                { "black",   6},
        };

enum { NORMAL, FLOATING};
static SLRadio rads[]  = {
         { "normal",   NORMAL },
         { "floating", FLOATING },
       };

static String  defres[]= {
     "*select*XmText.columns:   35",
     "*normal.set:              True",
     "*normal.labelString:      Normal Label",
     "*floating.labelString:    Floating Label",
    NULL, };
//   ".height:                  220",
//   ".width:                   500",

LabelInput::LabelInput( Widget    p,
                        char      *name,
                        HelpCtx   hctx,
                        SeisLabel *sl ) : 
      SLFPopSep(p,name,FP_DOALL,hctx,False,False), _sl(sl),
      _current_vect(NULL), _new_label(True)
{
  char *color_str;
  _label= new SL2Text( this, "select", 0, "Label:" );
  _font=  new SLFontSelect( this, "font");
  _color= new SLColorOption(this,"colop", colors_ary, XtNumber(colors_ary) );
  _ltype= new SLRadioBox( this, "ltype", NULL,
                          rads, XtNumber(rads), NULL, True );
  setDefaultResources( p, name, defres);
  color_str= _color->whichSelectedString();
  _sl->setInsertColor(color_str);
  free(color_str);
}

LabelInput::~LabelInput()
{
  delete _label;
  delete _font;
  delete _color;
  delete _ltype;
}

Widget LabelInput::make(Widget p)
{
   if ( made() ) return topWidget();
   SLFPopSep::make(p);
   p= wParent();

   XtVaSetValues( _label->W(), XmNtopAttachment,    XmATTACH_FORM,
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNtopOffset,        10,
                               XmNrightOffset,      5,
                               XmNleftOffset,       10,
                               NULL);

   XtVaSetValues( _font->W(),  XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _label->W(),
                               XmNtopOffset,        7,
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNleftOffset,       10,
                               NULL);

   XtVaSetValues( _color->W(), XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,        _font->W(),
                               XmNleftAttachment,   XmATTACH_WIDGET,
                               XmNleftWidget,       _font->W(),
                               XmNleftOffset,       10,
                               NULL);

   XtVaSetValues( _ltype->W(), XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _color->W(),
                               XmNtopOffset,        7,
                               XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,       _color->W(),
                               NULL);

   Widget tmp=  XtVaCreateManagedWidget("", xmLabelWidgetClass, topWidget(),
                                XmNtopAttachment,    XmATTACH_WIDGET,
                                XmNtopWidget,        _ltype->W(),
                                XmNbottomAttachment, XmATTACH_WIDGET,
                                XmNbottomWidget,     bottomSeparator(),
                                XmNleftAttachment,   XmATTACH_FORM,
                                XmNleftOffset,       5,
                                XmNtopOffset,        5,
                                XmNbottomOffset,     25,
                                NULL);

   defaultButtonOK(True);
   return topWidget();
}



void LabelInput::manage()
{ 
  const char *current_str;
  current_str= _current_vect->getLabel();
  if (strcmp(current_str, _sl->noLabelString()) != 0) {
          _label->setCvar((char*)current_str);
          _new_label= False;
  }
  else {
          _label->setCvar("");
          _new_label= True;
  }
  SLBase::manage();
}


Boolean LabelInput::ValidInput()
{
  return True;
}


void LabelInput::DoAction()
{
  char *labelstr= _label->getCvar();
  if ( (strlennull(labelstr) > 0) || (whichButton() == FP_OK) ) {
       char *fontstr= _font->selectedFont();
       char *colorstr= _color->currentColor();
       Boolean do_floating = (_ltype->WhichSelected() == FLOATING); 
       _sl->doNameLabel( labelstr, _current_vect, fontstr, 
                         colorstr, do_floating );
       _sl->setInsertColor(colorstr);
       free (fontstr);
       free (colorstr);
  }
}


void LabelInput::UndoInput()
{ 
  if (_new_label) _sl->doNameLabel( "", _current_vect);
}
