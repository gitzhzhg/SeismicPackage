// header_word_menu.cc
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

#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"
#include "fgqc/header_word_menu.hh"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_pop.hh"
#include "fgqc/fgqc_header_type.hh"
#include "geom/field_geometry.hh"
#include "sp/do_abort.hh"
#include <Xm/Label.h>
#include "named_constants.h"

#define ParentClass SLFPopSep

//========================================================================
//======================== Header Word Sub Menu ==========================
//========================================================================
static String  header_defres[]= {
    "*atCMP.labelString:               Locate at CMP",
    "*atSource.labelString:            Locate at Source",
    "*atReceiver.labelString:          Locate at Receiver",
    NULL};

HeaderWordMenu::HeaderWordMenu (Widget              p,
                                char                *name,
                                HelpCtx             hctx, 
                                FgQcPop             *fgqc_pop)
                                : FgQcPopSubMenu (p, name, hctx, fgqc_pop),
                                _pop (fgqc_pop)

{
  static SLText header_texts[]  = {
    {"headerword",  "range:1 9999999, default:43", NULL, SLType_int,
      HEADER_WORD_SELECTED},
  };
  _header_word = (int)_pop->getHeaderWord ();
  header_texts[0].target= &_header_word;

  static SLRadio headerattypes[] =
    {
      {"atCMP",      PLOT_AT_CMP},
      {"atSource",   PLOT_AT_SOURCE},
      {"atReceiver", PLOT_AT_RECEIVER},
    };

  setDefaultResources (p, name, header_defres);

  _header_box = new SLTextBox (this, "header_box", _hctx,
                               header_texts, XtNumber(header_texts), 
                               True, 1, True, False);
  _header_box->setAltLosingAction ((SLTextfunc)HeaderWLosingFocusAction, this);

  _headeratrbox = new SLRadioBox (this, "headeratrbox", getHelpCtx(),
    headerattypes, XtNumber(headerattypes), NULL, True, False);
  _headeratrbox->setAltChoiceAction
    ((SLRadioButtonfunc)HeaderAtAction, this);
}

HeaderWordMenu::~HeaderWordMenu()
{
}

Widget HeaderWordMenu::make(Widget p)
{
  SLFPopSep::make(p);

  XtVaSetValues (_header_box->W(),
                                 XmNtopAttachment,    XmATTACH_FORM,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       5,
                                 NULL);

  _header_value_loc = (DataLocation)_pop->getDataLocation ();
  _headeratrbox->SetRadio ((long)_header_value_loc);
  FieldGeometry *fg = _pop->fg ();

  if (fg->midpointGathersOutOfDate())
    XtSetSensitive (_headeratrbox->GetRadioWidget(0), False);
  else
    XtSetSensitive (_headeratrbox->GetRadioWidget(0), True);
  if (fg->receiverGathersOutOfDate())
    XtSetSensitive (_headeratrbox->GetRadioWidget(2), False);
  else
    XtSetSensitive (_headeratrbox->GetRadioWidget(2), True);
  if (fg->sourceGathersOutOfDate())
    XtSetSensitive (_headeratrbox->GetRadioWidget(1), False);
  else
    XtSetSensitive (_headeratrbox->GetRadioWidget(1), True);

  XtVaSetValues (_headeratrbox->W(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _header_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       5,
                                 NULL);

  Widget rightphantomlabel = XtVaCreateManagedWidget ("",
                                  xmLabelWidgetClass, topWidget(),
                                  XmNleftAttachment,  XmATTACH_WIDGET,
                                  XmNleftWidget,      _headeratrbox->W(),
                                  XmNrightAttachment, XmATTACH_FORM,
                                  NULL);

  Widget bottomphantomlabel = XtVaCreateManagedWidget ("",
                                  xmLabelWidgetClass,  topWidget(),
                                  XmNtopAttachment,    XmATTACH_WIDGET,
                                  XmNtopWidget,        _headeratrbox->W(),
                                  XmNbottomAttachment, XmATTACH_WIDGET,
                                  XmNbottomWidget,     bottomSeparator(),
                                  NULL);

  return(topWidget());
}


void HeaderWordMenu::manage()
{
  if(_first_time)
    {
    wprocShowMsg (_header_box->LabW(HEADER_WORD_SELECTED), "Header Word: ");
    _header_box->SetValue (HEADER_WORD_SELECTED, (long)43);
    _first_time = False;
    }

  ParentClass::manage();

}

void HeaderWordMenu::HeaderWLosingFocusAction (void *data, long /*which*/)
{
  HeaderWordMenu *obj = (HeaderWordMenu *)data;
  obj->_active_plot->setHeaderWord (obj->_header_word);
}

void HeaderWordMenu::HeaderAtAction (void *data, long which)
{
  HeaderWordMenu *obj = (HeaderWordMenu *)data;
  obj->_header_value_loc = (DataLocation)which;
  obj->_active_plot->setDataLocation (which);
}

char *HeaderWordMenu::questionText ()
{
  char num_pnt_str[12];
  static char string[200];
  float xmin = _pop->getUserLeft ();
  float xmax = _pop->getUserRight ();
  float ymin = _pop->getUserTop ();
  float ymax = _pop->getUserBottom ();
  DoAbort *do_abort = new DoAbort (topWidget());
  do_abort->setNewAction ();
  long num_points
    = FgQcHeaderType::computeNumPoints (MinimumValue(xmin,xmax),
                                        MaximumValue(xmin,xmax),
                                        MinimumValue(ymin,ymax),
                                        MaximumValue(ymin,ymax),
                                        _pop->fg(),
                                        (long)_pop->getCoordinateSystem(),
                                        _pop->getDataLocation(),
                                        do_abort);
  do_abort->actionComplete ();
  delete do_abort;
  if (num_points > 5000) {
    sprintf (num_pnt_str, "%d", num_points);
    strcpy (string, "It could take a while to compute ");
    strcat (string, num_pnt_str);
    strcat (string, " trace headers.\n----\nComputing just 5000 is quick.\n");
    strcat (string, "----\nDo you want to proceed?");
    return string;
  }
  else
    return 0;
}
