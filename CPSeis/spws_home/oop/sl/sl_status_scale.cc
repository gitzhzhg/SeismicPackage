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
#include "sl/sl_status_scale.hh"
#include "sl/sl_scale.hh"
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <string.h>
#include <stdlib.h>

SLStatusScale::SLStatusScale (Widget p, char *name, HelpCtx hctx,
  char *title) :
  SLFPopSep (p, name, FP_DOOK, hctx, False, False),
  _percent  (0),
  _title    (0)
{
  if (title) {
    _title = (char *)malloc (strlen(title)+1);
    strcpy (_title, title);
  }
  _scale = new SLScale (this, "status_scale", getHelpCtx(), 0, False);
}

SLStatusScale::~SLStatusScale ()
{
  delete _scale;
  free (_title);
}

Widget SLStatusScale::make (Widget p)
{
  if (made()) return topWidget ();
  p = p ? p : wParent ();
  SLFPopSep::make (p);

  _scale->make (W());

  XtVaSetValues (_scale->W(),
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     bottomSeparator(),
                         XmNbottomOffset,     25,
                         NULL);
  
  int width;
  if (_title) {
    width = (int)strlen(_title) * 9;
    if (width < 100) width = 100;
    setTitle (_title);
  }
  else {
    width = 100;
  }

  XtVaSetValues (_scale->W(),
                         XmNscaleHeight,      10,
                         XmNscaleWidth,       width,
                         XmNshowValue,        True,
                         NULL);

  Widget phantomlabR = XtVaCreateManagedWidget (
                         "",                  xmLabelWidgetClass, 
                         topWidget(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _scale->W(),
                         XmNleftOffset,       5,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNrightOffset,      5,
                         NULL);

  Widget phantomlabT = XtVaCreateManagedWidget (
                         "",                  xmLabelWidgetClass, 
                         topWidget(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNtopOffset,        15,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _scale->W(),
                         XmNbottomOffset,     25,
                         NULL);

  return topWidget ();
}

// percent is from 0.0 to 100.0
void SLStatusScale::setPercent (float percent)
{
// boost the resolution to 2 decimal places of one percent
  _percent = percent * 100.0 + 0.5;
  if (_percent <= 0  ) _percent = 1;
  if (_percent > 10000) _percent = 10000;
  _scale->setScaleValue ((int)_percent);
  XFlush (XtDisplay(_scale->W()));
}

void SLStatusScale::DoAction ()
{
  unmanage ();
}

void SLStatusScale::managing ()
{
  _scale->manage ();
  _scale->setRange (0, 10000);
  setPercent (_percent);
}
