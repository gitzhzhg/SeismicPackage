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
#include "sp/seis_lav_gui.hh"
#include "sp/seis_lav_pop.hh"
#include "sl/slp_text.hh"
#include "sl/psuedo_widget.hh"
#include "cprim.h"
#include "wproc.h"
#include <float.h>
#include <Xm/Label.h>

#define MAX_NAME_LENGTH 30
#define MAX_VALUE_LENGTH 15
#define DECIMAL_PREC 8

static String defres[] = {
  "*local.labelString:      ;  Err:  ave = ",
  0};

SeisLavGui::SeisLavGui (Widget parent, char *name, HelpCtx hctx,
  SeisLavPop *lav_pop) :
  SLForm (parent, name, hctx, False, False),
  _lav_pop  (lav_pop),
  _values   (NULL),
  _names    (NULL)
{
  setDefaultResources (parent, name, defres);
  make ();
}

SeisLavGui::SeisLavGui (SLDelay *container, char *name, HelpCtx hctx,
  SeisLavPop *lav_pop) :
  SLForm (container, name, hctx, False, False),
  _lav_pop  (lav_pop),
  _values   (NULL),
  _names    (NULL)
{
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
    make ();
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
}

SeisLavGui::~SeisLavGui ()
{
  if (_values) free (_values);
  if (_names ) free (_names );
}

Widget SeisLavGui::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make ();

  // count must be even and not greater than four
  assert ((int)(_lav_pop->count()/2)*2 == _lav_pop->count());
  assert (!((int)_lav_pop->count() > 4));

  _names  = (SLpText **)malloc (_lav_pop->count()*sizeof(SLpText *));
  _values = (SLpText **)malloc (_lav_pop->count()*sizeof(SLpText *));

  Widget empty_label =  XtVaCreateManagedWidget ("",
                               xmLabelWidgetClass,  topWidget(),
                               XmNtopAttachment,    XmATTACH_FORM,
			       XmNtopOffset,        5,
                               XmNleftAttachment,   XmATTACH_FORM,
                               NULL);

  Widget widget = empty_label;

  int k2, ident;
  for (k2 = 0, ident = 0; k2 < _lav_pop->count(); k2 += 2) {
    // Overlay creation
    _names[k2]  = new SLpText (this, "lav_name" , ident++,
      PrimSupport::_CHAR, MAX_NAME_LENGTH);
    _names[k2]->showLabelAppearance ();
    _values[k2] = new SLpText (this, "lav_value", ident++,
      PrimSupport::_DOUBLE, MAX_VALUE_LENGTH, DECIMAL_PREC);
    _values[k2]->showLabelAppearance ();

    // Underlay creation
    _names[k2+1]  = new SLpText (this, "lav_name" , ident++,
      PrimSupport::_CHAR, MAX_NAME_LENGTH);
    _names[k2+1]->showLabelAppearance ();
    _values[k2+1] = new SLpText (this, "lav_value", ident++,
      PrimSupport::_DOUBLE, MAX_VALUE_LENGTH, DECIMAL_PREC);
    _values[k2+1]->showLabelAppearance ();

    // Overlay attachment
    XtVaSetValues (_names[k2]->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		         XmNtopWidget,        widget,
                         XmNleftAttachment,   XmATTACH_WIDGET,
		         XmNleftWidget,       widget,
		         XmNleftOffset,       5,
		         XmNalignment,        XmALIGNMENT_END,
                         NULL);

    XtVaSetValues (_values[k2]->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
		         XmNtopWidget,        _names[k2]->W(),
		         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
		         XmNleftWidget,       _names[k2]->W(),
		         XmNalignment,        XmALIGNMENT_BEGINNING,
                         NULL);
    // Underlay attachment
    XtVaSetValues (_names[k2+1]->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
		         XmNtopWidget,        _values[k2]->W(),
		         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
		         XmNleftWidget,       _values[k2]->W(),
		         XmNalignment,        XmALIGNMENT_END,
                         NULL);

    XtVaSetValues (_values[k2+1]->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
		         XmNtopWidget,        _names[k2+1]->W(),
		         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
		         XmNleftWidget,       _names[k2+1]->W(),
		         XmNalignment,        XmALIGNMENT_BEGINNING,
                         NULL);

    widget = _names[k2]->W();
  }
  for (k2 = 0; k2 < _lav_pop->count()/2; k2++) {
    clear (k2);
  }
  return topWidget ();
}

void SeisLavGui::clear (int ident)
{
  setName (ident);
  setValue (ident);
}

void SeisLavGui::setName (int ident, char *name)
{
  assert (ident > -1 && ident < _lav_pop->count());
  if (!name) {
    //    _names[ident]->unmanage ();
    wprocVAShowMsg (_names [ident]->W(), " ");
  }
  else {
    int len = strlen (name);
    char *loc_name = (char *)malloc (MAX_NAME_LENGTH*sizeof(char));
    if (len >= MAX_NAME_LENGTH) {
      strcpy (loc_name, &name[len-MAX_NAME_LENGTH]);
    }
    else {
      strcpy (loc_name, name);
    }
    wprocVAShowMsg (_names [ident]->W(), loc_name);
    //    _names[ident]->manage ();
    free (loc_name);
  }
}

void SeisLavGui::setValue (int ident, double *value)
{
  assert (ident > -1 && ident < _lav_pop->count());
  if (!value) {
    //    _values[ident]->unmanage ();
    wprocVAShowMsg (_values[ident]->W(), " ");
  }
  else {
    assert (MAX_VALUE_LENGTH == 15 && DECIMAL_PREC == 8);
    wprocVAShowMsg (_values[ident]->W(), "%15.8g", *value);
    //    _values[ident]->manage ();
  }
}
