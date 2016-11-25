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
// class that creates the attribute range gui for a color bar builder

#include "color/color_bar_builder_pop.hh"
#include "color/cbb_attr_rng_gui.hh"
#include "color/cbb_levels_gui.hh"
#include "color/cbb_col_set_gui.hh"
#include "color/cbb_cps_amp_proc.hh"
#include "color/color_selector_pop.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "sl/sl_push_box.hh"
#include <Xm/Label.h>

static String defres[] = {
  "*ar_title.labelString:      Set Attribute Range",
  "*from.labelString:          From:",
  "*to.labelString:            To:  ",
  "*insert.labelString:        Insert",
  "*interpolate.labelString:   Interpolate",
  "*extrapolate.labelString:   Extrapolate",
  "*extract.labelString:       Extract",
  0};

static SLPush buttons[] = {
  { "insert",      CBBAttrRngGui::CBB_INSERT      },
  { "interpolate", CBBAttrRngGui::CBB_INTERPOLATE },
  { "extrapolate", CBBAttrRngGui::CBB_EXTRAPOLATE },
  { "extract",     CBBAttrRngGui::CBB_EXTRACT     }
};

CBBAttrRngGui::CBBAttrRngGui (Widget parent, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (parent, name, hctx, True),
  _cbb_pop  (cbb_pop)
{
// set the default resources
  setDefaultResources (parent, name, defres);
  init ();
}

CBBAttrRngGui::CBBAttrRngGui (SLDelay *container, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (container, name, hctx, True),
  _cbb_pop  (cbb_pop)
{
// set the default resources
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
    make (container->topWidget());
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
  init ();
}

void CBBAttrRngGui::init ()
{
// define the attribute range
  int levs  = _cbb_pop->_amp->levels ();
  _attr_min = _cbb_pop->_amp->middleOfLevel (0);
  _attr_max = _cbb_pop->_amp->middleOfLevel (levs-1);
  float attr_inc = (_attr_max - _attr_min) / (float)(levs - 1);

// beginning attribute specifier
  _from = _attr_min;
  _from_arrow = new SLScaleTextArrow (this, "fromA", getHelpCtx(), &_from,
    True, _from, _attr_min, _attr_max, attr_inc, (int)3, (int)6,
    XmHORIZONTAL, "from");

  _from_arrow->setComplexNotify (this);

// ending attribute specifier
  _to = _attr_max;
  _to_arrow = new SLScaleTextArrow (this, "toA", getHelpCtx(), &_to,
    True, _to, _attr_min, _attr_max, attr_inc, (int)3, (int)6,
    XmHORIZONTAL, "to");

  _to_arrow->setComplexNotify (this);

// parameter selection push buttons
  _fill_box = new SLPushBox (this, "fill", getHelpCtx(), buttons,
    XtNumber(buttons));

  _fill_box->setComplexNotify (this);

}

CBBAttrRngGui::~CBBAttrRngGui ()
{
  if (_fill_box)   delete _fill_box,   _fill_box   = 0;
}

Widget CBBAttrRngGui::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make (parent);

  Widget lab = XtVaCreateManagedWidget ("ar_title",
                         xmLabelWidgetClass,  topWidget(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNrightAttachment,  XmATTACH_FORM,
                         NULL);

  XtVaSetValues (_from_arrow->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        lab,
                         XmNtopOffset,        1,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       lab,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      lab,
                         NULL);

  XtVaSetValues (_to_arrow->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _from_arrow->W(),
                         XmNtopOffset,        1,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _from_arrow->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _from_arrow->W(),
                         NULL);

  XtVaSetValues (_fill_box->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _to_arrow->W(),
                         XmNtopOffset,        1,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _from_arrow->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _from_arrow->W(),
                         NULL);

  return topWidget ();

}

Boolean CBBAttrRngGui::notifyComplex (SLDelay *obj, int ident)
{
  if (obj == _from_arrow) {
// from attribute value has changed
    _cbb_pop->_col_set_gui->
      valueEstablished (CBBColorSet::ESTABLISHED_FIRST_INDEX);
  }
  else if (obj == _to_arrow) {
// to attribute value has changed
    _cbb_pop->_col_set_gui->
      valueEstablished (CBBColorSet::ESTABLISHED_SECOND_INDEX);
  }
  else if (obj == _fill_box) {
// push button was pushed
    if (ident == CBB_INTERPOLATE) {
// fill in the color bar by interpolation starting with color at from to color
//   at to
      _cbb_pop->_col_set_gui->activate ();
      _cbb_pop->_col_set_gui->interpolateColors ();
    }
    else if (ident == CBB_EXTRAPOLATE) {
// fill in the color bar by extrapolation starting with active color from from
//   to to
      _cbb_pop->_col_set_gui->activate ();
      _cbb_pop->_col_set_gui->extrapolateColors ();
    }
    else if (ident == CBB_INSERT) {
// fill in the color bar by extrapolation starting with active color from from
//   to to
      _cbb_pop->_col_set_gui->activate ();
      _cbb_pop->_col_set_gui->insertColor ();
    }
    else if (ident == CBB_EXTRACT) {
// fill in the color bar by extrapolation starting with active color from from
//   to to
      float red, green, blue;
      _cbb_pop->_col_set_gui->extractColor (&red, &green, &blue);
      _cbb_pop->_col_sel_pop->setActiveColor (red, green, blue);
    }
  }
  return True;
}

void CBBAttrRngGui::setFrom (float value)
{
  _from_arrow->setValue (value);
}

void CBBAttrRngGui::setFrom ()
{
  _from_arrow->setValue (_attr_min);
}

void CBBAttrRngGui::setTo (float value)
{
  _to_arrow->setValue (value);
}

void CBBAttrRngGui::setTo ()
{
  _to_arrow->setValue (_attr_max);
}

void CBBAttrRngGui::redoAttributeRange ()
{
// define the attribute range
  CBBCPSAmpProc *amp = _cbb_pop->_col_set_gui->amp();
  int index;
  int levs  = amp->levels ();
  _attr_min = amp->middleOfLevel (0);
  _attr_max = amp->middleOfLevel (levs-1);
  float attr_inc = (_attr_max - _attr_min) / (float)(levs - 1);

// beginning attribute specifier
  _from_arrow->setIncrement (attr_inc);
  _from_arrow->setRange (_attr_min, _attr_max);
  index = amp->levelOfValue (_from_arrow->getValue());
  _from_arrow->setValue (amp->middleOfLevel(index));

// ending attribute specifier
  _to_arrow->setIncrement (attr_inc);
  _to_arrow->setRange (_attr_min, _attr_max);
  index = amp->levelOfValue (_to_arrow->getValue());
  _to_arrow->setValue (amp->middleOfLevel(index));

}
