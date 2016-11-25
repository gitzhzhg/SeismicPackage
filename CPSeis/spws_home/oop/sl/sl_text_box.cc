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
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>
#include <X11/Xatom.h>
#include "wproc.h"
#include "sl/sl_text_box.hh"
#include "sl/psuedo_widget.hh"

extern "C" {

  long dotextcb
    (Widget,
     struct CB*,
     XmTextVerifyCallbackStruct*);

  enum chk_actions get_op
    (char str[],
     long setary[],
     void *low,
     void *high,
     void *def,
     long *deci_places,
     long type);
}


static String defres[] = {
  NULL,
  ".orientation:         HORIZONTAL",
  ".packing:             PACK_COLUMN",
  ".spacing:             0",
  ".XmText.columns:      8",
  ".XmText.marginHeight: 1",
  ".XmText.marginWidth:  1",
  ".XmLabel.alignment:   ALIGNMENT_BEGINNING",
  NULL
};


static char COLRESSTR[] =  ".numColumns: %d";


SLTextBox::SLTextBox (Widget p, char *name, HelpCtx hctx, SLTextAry textary,
  unsigned int arycnt, Boolean DoLabel, long number_col, Boolean doframe,
  Boolean DoTitles, Boolean make_now) :
  SLDelay (name, hctx, doframe),
  _arycnt           (arycnt), 
  _last_valid       (True),
  _altFocusAction   (NULL),
  _altLosingAction  (NULL),
  _DoLabel          (DoLabel),
  _DoTitles         (DoTitles), 
  _number_col       (number_col),
  _last_reason      (TextNone),
  _external_width   (0)
{
  supportUnmadeDefaults (p);
  init (XtDisplay(p), textary);
  if (make_now) {
    make(p);
  }
  else {
    get_targets(True);
  }
}


SLTextBox::SLTextBox (SLDelay *contain, char *name, HelpCtx hctx,
  SLTextAry textary, unsigned int arycnt, Boolean DoLabel, long number_col,
  Boolean doframe, Boolean DoTitles, Boolean make_if_can) :
  SLDelay (contain, name, hctx, doframe),
  _arycnt           (arycnt), 
  _last_valid       (True),
  _altFocusAction   (NULL),
  _altLosingAction  (NULL),
  _DoLabel          (DoLabel),
  _DoTitles         (DoTitles), 
  _number_col       (number_col),
  _last_reason      (TextNone),
  _external_width   (0)
{
  supportUnmadeDefaults (contain);
  init ((contain->pW())->display(), textary);
  if (contain->made() && make_if_can) {
    make (contain->topWidget());
  }
  else   {
    get_targets (True);
  }
}


SLTextBox::SLTextBox (PsuedoWidget *pw, char *name, HelpCtx hctx,
  SLTextAry textary, unsigned int arycnt, Boolean DoLabel, long number_col,
  Boolean doframe, Boolean DoTitles) :
  SLDelay (pw, name, hctx, doframe),
  _arycnt           (arycnt), 
  _last_valid       (True),
  _altFocusAction   (NULL),
  _altLosingAction  (NULL),
  _DoLabel          (DoLabel), 
  _DoTitles         (DoTitles), 
  _number_col       (number_col),
  _last_reason      (TextNone),
  _external_width   (0)
{
  init (pw->display(), textary);
  supportUnmadeDefaults (pw);
  get_targets (True);
}


void SLTextBox::init (const Display *dpy, SLTextAry textary)
{
  long cnt;
  char colres[30];

  cnt = _DoTitles ? (_arycnt/_number_col)+1 : _arycnt/_number_col;

  sprintf (colres, COLRESSTR, cnt);
  defres[0] = colres;
  setDefaultResources (dpy, _name, defres);

  _textary = (SLTextAryW)calloc (_arycnt, sizeof(SLTextW));
  for (int i = 0; i < _arycnt; i++) {
    _textary[i].type   = textary[i].type;
    _textary[i].target = textary[i].target;
    if (textary[i].target == NULL) {
      switch (_textary[i].type) {
      case SLType_uchar :
	_textary[i].target = &_textary[i].value.cval;
	break;
      case SLType_sint :
	_textary[i].target = &_textary[i].value.sval;
	break;
      case SLType_int :
	_textary[i].target = &_textary[i].value.ival;
	break;
      case SLType_float :
      case SLType_gfloat :
	_textary[i].target = &_textary[i].value.fval;
	break;
      case SLType_long :
	_textary[i].target = &_textary[i].value.lval;
	break;
      case SLType_double :
      case SLType_gdouble :
	_textary[i].target = &_textary[i].value.dval;
	break;
      default:
	assert (0);
	break;
      }
    } // end if
    else  {
      _textary[i].target = textary[i].target;
    } // end else
    _textary[i].ident     = textary[i].ident;
    _textary[i].tw        = NULL;
    _textary[i].lw        = NULL;
    _textary[i].name      = newstr (textary[i].name);
    _textary[i].value_set = False;
    if (textary[i].range) {
      _textary[i].range = newstr (textary[i].range);
    }
    else {
      _textary[i].range = NULL;
    }
  }
}


void SLTextBox::get_targets (Boolean first)
{
  PsuedoWidget *pw = NULL;
  char *str = NULL;

  unsigned char cval = 0;
  short    sval = 0;
  int      ival = 0;
  float    fval = 0.0;
  long     lval = 0;
  double   dval = 0.0;

  for (int i = 0; i < _arycnt; i++) {
    pw  = new PsuedoWidget (_pw_topw, _textary[i].name, xmTextWidgetClass);
    str = pw->textDef ();
    _textary[i].value_set = False;
    if (str) {
      switch (_textary[i].type) {
      case SLType_uchar :
	*(unsigned char *)_textary[i].target = check_uchar (str, &cval)
          ? cval : 0;
        break;
      case SLType_sint :
	*(short *)_textary[i].target = check_sint (str, &sval) ? sval : 0;
        break;
      case SLType_int :
	*(int *)_textary[i].target = check_int (str, &ival) ? ival : 0;
	break;
      case SLType_float :
      case SLType_gfloat :
	*(float *)_textary[i].target = check_flt (str, &fval) ? fval : 0.0;
	break;
      case SLType_long :
	*(long *)_textary[i].target = check_long (str, &lval) ? lval : 0;
	break;
      case SLType_double :
      case SLType_gdouble :
	*(double *)_textary[i].target = check_dbl (str, &dval) ? dval : 0.0;
	break;
      default:
	assert (0);
	break;
      }
    }
    else {
      switch (_textary[i].type) {
      case SLType_uchar :
        *(unsigned char *)_textary[i].target = 0;
	break;
      case SLType_sint :
	*(short *)_textary[i].target = 0;
	break;
      case SLType_int :
	*(int *)_textary[i].target = 0;
	break;
      case SLType_float :
      case SLType_gfloat :
	*(float *)_textary[i].target = 0.0;
	break;
      case SLType_long :
	*(long *)_textary[i].target = 0;
	break;
      case SLType_double :
      case SLType_gdouble :
	*(double *)_textary[i].target = 0.0;
	break;
      default:
	assert (0);
	break;
      }
    }
    if (first) {
      switch (_textary[i].type) {
      case SLType_uchar :
        _textary[i].orgin_value.cval = *(unsigned char *)_textary[i].target;
	break;
      case SLType_sint :
        _textary[i].orgin_value.sval = *(short *)_textary[i].target;
	break;
      case SLType_int :
        _textary[i].orgin_value.ival = *(int *)_textary[i].target;
	break;
      case SLType_float :
      case SLType_gfloat :
        _textary[i].orgin_value.fval = *(float *)_textary[i].target;
	break;
      case SLType_long :
        _textary[i].orgin_value.lval = *(long *)_textary[i].target;
	break;
      case SLType_double :
      case SLType_gdouble :
        _textary[i].orgin_value.dval = *(double *)_textary[i].target;
	break;
      default:
	assert (0);
	break;
      }
    }
    delete pw;
  }
}

void SLTextBox::setWidth (Dimension external_width)
{
  assert (external_width > 0);
  _external_width = (Dimension *)malloc (sizeof(Dimension));
  *_external_width = external_width;
}

Widget SLTextBox::make(Widget p)
{
  const int LNAMELEN = 100;
  char label_name[LNAMELEN];
  wunion wary[1];
  Boolean rowlabel;
  long colcnt;
  char lstr[30]; 
  char tstr[30];
  unsigned char tmpc;
  short tmps;
  int tmpi;
  float tmpf;
  long  tmpl;
  double tmpd;
  Widget w;
 
  if (made()) return topWidget ();
  SLDelay::make (p);

  p = wParent ();
  w = XtVaCreateManagedWidget (_name, topClass(), makeFrameIfNeeded(p),
    XmNisAligned, False, NULL);

  setTopWidget(w);

  XtAddEventHandler (topWidget(), NoEventMask, True, 
    (XtEventHandler)highlightHandler, (XtPointer)this);

  if (_DoTitles) {
    Pixel color;            
    Widget bw;
    bw = XtVaCreateManagedWidget ("", xmLabelWidgetClass, topWidget(), NULL);
    XtVaGetValues (bw, XmNbackground, &color, NULL);
    XtVaSetValues (bw, XmNforeground, color, NULL);
    show_msg (bw, "");
    for (int i = 0; i < _number_col; i++) {
      sprintf (lstr, "T%d", i);
      XtVaCreateManagedWidget (lstr, xmLabelWidgetClass, topWidget(), 
        XmNmarginLeft, 0, XmNmarginRight, 0, NULL);
                                 //XmNalignment,   XmALIGNMENT_CENTER,
    }
  } 

  _popbox = make_okbox (topWidget(), "popbox",  XmDIALOG_ERROR);
  XtAddCallback (_popbox, XmNokCallback, okCallback, this);

  rowlabel = _DoLabel;
  colcnt = 0;

  for (int i= 0; i < _arycnt; i++) {
    if (_textary[i].name) {
      if (rowlabel) {
	if (strlen(_textary[i].name) < LNAMELEN) {
	  sprintf (label_name, "%sL", _textary[i].name);
	}
	else {
	  label_name[LNAMELEN-1] = 'L';
	}
      } // End if
      else {
	label_name[0] = '\0';
      }

      if (_textary[i].value_set) {
	switch (_textary[i].type) {
	case SLType_uchar :
	  tmpc = *(unsigned char *)_textary[i].target;
	  break;
	case SLType_sint :
	  tmps = *(short *)_textary[i].target;
	  break;
	case SLType_int :
	  tmpi = *(int *)_textary[i].target;
	  break;
	case SLType_long :
	  tmpl = *(long *)_textary[i].target;
	  break;
	case SLType_float :
	case SLType_gfloat :
	  tmpf = *(float *)_textary[i].target;
	  break;
	case SLType_double :
	case SLType_gdouble :
	  tmpd = *(double *)_textary[i].target;
	  break;
	default:
	  assert (0);
	}
      }
      set_TEXT (wary, _textary[i].name, DoTextCallback, this,
	DoTextCallback, this, label_name, topWidget(), _textary[i].type,
        _textary[i].target, 0);
      wary[0].textw.wconst = i;
      _textary[i].tw = create_text (&wary[0].textw);
      _textary[i].lw = wary[0].textw.label;
      XtFree (wary[0].textw.initval);
#ifdef _AIX
      short col;
      XtVaGetValues (_textary[i].tw, XmNcolumns, &col, NULL);
      col = col / 2;
      XtVaSetValues (_textary[i].tw, XmNcolumns, col, NULL);
#endif
      install_help (_textary[i].tw );

      if (_textary[i].value_set) {
	switch (_textary[i].type) {
	case SLType_uchar :
	  TEXT_set_uchar (_textary[i].tw,tstr, tmpc);
	  *((unsigned char *)_textary[i].target) = tmpc;
	  break;
	case SLType_sint :
	  TEXT_set_sint (_textary[i].tw,tstr, tmps);
	  *((short *)_textary[i].target) = tmps;
	  break;
	case SLType_int :
	  TEXT_set_int (_textary[i].tw,tstr, tmpi);
	  *((int *)_textary[i].target) = tmpi;
	  break;
	case SLType_long :
	  TEXT_set_long (_textary[i].tw,tstr, tmpl);
	  *((long *)_textary[i].target) = tmpl;
	  break;
	case SLType_float :
	case SLType_gfloat :
	  TEXT_set_flt (_textary[i].tw,tstr, tmpf);
	  *((float *)_textary[i].target) = tmpf;
	  break;
	case SLType_double :
	case SLType_gdouble :
	  TEXT_set_dbl (_textary[i].tw,tstr, tmpd);
	  *((double *)_textary[i].target) = tmpd;
	  break;
	default:
	  assert (0);
	}
      }
    }
    else {
      // install a blank
    }
    colcnt++;
    rowlabel = False;
    if (colcnt == _number_col)  {
      colcnt = 0;
      rowlabel = _DoLabel;
    }

    if (_external_width && _textary[i].tw) {
        XtVaSetValues (_textary[i].tw, XmNwidth, *_external_width, NULL);
    }
  }
  return topWidget ();
}


SLTextBox::~SLTextBox ()

{
  for (int i = 0; i < _arycnt; i++) {
    if (_textary[i].range) free (_textary[i].range);
    if (_textary[i].name ) free (_textary[i].name );
  }
  free (_textary);

  if (_external_width) free (_external_width);
}



void SLTextBox::DoTextCallback (Widget w, XtPointer udata,
  XmTextVerifyCallbackStruct *CBdata)
{
  SLTextBox *obj = (SLTextBox *)udata;
  long idx;
  XtPointer temp = (XtPointer)(&idx);
  XtVaGetValues (w, XmNuserData, temp, NULL);
  obj->_curr_text = (int)idx;

  if (CBdata->reason == XmCR_FOCUS) {
    obj->_last_reason = GainingFocus;
    obj->DoTextFocus (w, udata, (XmAnyCallbackStruct*)CBdata);
    if (obj->_altFocusAction) obj->_altFocusAction (obj->_altFocusData, 
      obj->_textary[idx].ident);
  }
  else if (CBdata->reason == XmCR_ACTIVATE     ||  
           CBdata->reason == XmCR_LOSING_FOCUS   ) {
    if (CBdata->reason == XmCR_ACTIVATE) {
      obj->_last_reason = Activate;
    }
    else {
      obj->_last_reason = LosingFocus;
    }
    obj->DoText (w, udata, CBdata);
  }
}

void SLTextBox::okCallback (Widget, XtPointer udata, XtPointer)
{
  SLTextBox *obj = (SLTextBox *)udata;
  obj->_last_valid = True;
}



void SLTextBox::DoText (Widget w, XtPointer,
  XmTextVerifyCallbackStruct *CBdata)
{
  struct CB cb[1];
  long idx;
  XtPointer temp = (XtPointer)(&idx);
  XtVaGetValues (w, XmNuserData, temp, NULL);

  set_CB (cb, 0, _textary[idx].range, _popbox, _textary[idx].target,
    _textary[idx].type);
  _last_valid = dotextcb (w, &cb[0], CBdata);
  if (_last_valid) {
    TextAction (_textary[idx].ident);
    if (_altLosingAction) {
      _altLosingAction (_altLosingData, _textary[idx].ident);
    }
    callNotifyComplex ((int)_textary[idx].ident);
  }
}


Boolean SLTextBox::validate ()
{
  long valid;
  int i;
  struct CB cb[1];
  XmTextVerifyCallbackStruct cbs;

  if (made()) {
    cbs.reason = XmCR_ACTIVATE;
    cbs.event = NULL;

    for (i= 0, valid = _last_valid; i < _arycnt && valid ; i++) {
      set_CB (cb, 0, _textary[i].range, _popbox, _textary[i].target,
        _textary[i].type);
      valid = dotextcb (_textary[i].tw, &cb[0], &cbs);
     }
  }
  else {
    valid = True;
  }

  _last_valid= True;
  return (Boolean)valid; 
}


void SLTextBox::load ()
{
  int i;
  struct CB cb[1];
  XmTextVerifyCallbackStruct cbs;

  cbs.reason = XmCR_ACTIVATE;
  cbs.event= NULL;

  for (i= 0; i < _arycnt; i++) {
    set_CB (cb, 0, _textary[i].range, NULL, _textary[i].target,
      _textary[i].type);
    dotextcb (_textary[i].tw, &cb[0], &cbs);
  }
}


void SLTextBox::popError (char *str)
{
  show_msg (_popbox, str);
  _last_valid = False;
}


void SLTextBox::DoTextFocus (Widget w, XtPointer, XmAnyCallbackStruct*)
{
  char *str;
  int len;
  long idx;
  XtPointer temp = (XtPointer)(&idx);
  XtVaGetValues (w, XmNuserData, temp, NULL);
  //XtVaGetValues (w, XmNuserData, &idx, NULL);

  str = XmTextGetString (w);
  len=strlen (str);
  XtFree (str);
  XmTextSetSelection (w, 0, len, CurrentTime);
  TextActionFocus (_textary[idx].ident);
}

int SLTextBox::retidx (int ident)
{
  Boolean found;
  long i = 0;

  for (i = 0, found = False; i < _arycnt && !found; i++) {
    if (_textary[i].ident == ident) found = True;
  }

  if (!found) {
    i = 1;
    printf ("Invalid ident passed to SLTextBox value - %d\n", ident);
  }

  return (i - 1);
}


int SLTextBox::retidx (char *name)
{
  Boolean found;
  long i = 0;

  for (i = 0, found = False; i < _arycnt && !found; i++) {
    if (strcmp(_textary[i].name,name) == 0) found = True;
  }

  if (!found) {
    i = 1;
    printf ("Invalid name passed to SLTextBox: name value - %s\n", name);
  }

  return (i - 1);
}


/*
 * ==========================================================================
 * ================== For Getting Widget ID's ==========================
 * ==========================================================================
 */
Widget SLTextBox::TxtW (int ident)
{
  return _textary[retidx(ident)].tw;
}

Widget SLTextBox::TxtW (char *name)
{
  return _textary[retidx(name)].tw;
}

Widget SLTextBox::LabW (int ident)
{
  return _textary[retidx(ident)].lw;
}

Widget SLTextBox::LabW (char *name)
{
  return _textary[retidx(name)].lw;
}

/*
 * ==========================================================================
 * ================== For Setting Validation Range ==========================
 * ==========================================================================
 */
void SLTextBox::wkSetRange (int i, String range)
{
  if (made()) {
    if (_textary[i].range) XtFree (_textary[i].range);

    if (range) _textary[i].range = XtNewString (range);
    else       _textary[i].range = NULL;
  }
 else 
   printf ("SLTextBox::SetRange: Class must be made.\n");
}

void SLTextBox::wkSetRange (int i, void *low, void *high, void *def,
  long precision)
{
  unsigned char lc, hc, dc;
  short ls, hs, ds;
  int li, hi, di;
  long ll, hl, dl;
  float lf, hf, df;
  double ld, hd, dd;

  long deci_places = 2;
  char fstr[200], tstr[200] = "";
  long setary[60];

  // use long for uchar, short, int, and long
  // use double for float and double
  switch (_textary[i].type) {
  case SLType_uchar :
    if (_textary[i].range) {
      get_op (_textary[i].range, setary, (void *)(&lc), (void *)(&hc),
        (void *)(&dc), &deci_places, _textary[i].type);
      free (_textary[i].range);
    }
    if (*((long *)low) == same) {
      *((long *)low) = (long)lc;
    }
    if (*((long *)high) == same) {
      *((long *)high) = (long)hc;
    }
    if (*((long *)def) == same) {
      *((long *)def) = (long)dc;
    }
    break;
  case SLType_sint :
    if (_textary[i].range) {
      get_op (_textary[i].range, setary, (void *)(&ls), (void *)(&hs),
        (void *)(&ds), &deci_places, _textary[i].type);
      free (_textary[i].range);
    }
    if (*((long *)low) == same) {
      *((long *)low) = (long)ls;
    }
    if (*((long *)high) == same) {
      *((long *)high) = (long)hs;
    }
    if (*((long *)def) == same) {
      *((long *)def) = (long)ds;
    }
    break;
  case SLType_int :
    if (_textary[i].range) {
      get_op (_textary[i].range, setary, (void *)(&li), (void *)(&hi),
        (void *)(&di), &deci_places, _textary[i].type);
      free (_textary[i].range);
    }
    if (*((long *)low) == same) {
      *((long *)low) = (long)li;
    }
    if (*((long *)high) == same) {
      *((long *)high) = (long)hi;
    }
    if (*((long *)def) == same) {
      *((long *)def) = (long)di;
    }
    break;
  case SLType_long :
    if (_textary[i].range) {
      get_op (_textary[i].range, setary, (void *)(&ll), (void *)(&hl),
        (void *)(&dl), &deci_places, _textary[i].type);
      free (_textary[i].range);
    }
    if (*((long *)low) == same) {
      *((long *)low) = ll;
    }
    if (*((long *)high) == same) {
      *((long *)high) = hl;
    }
    if (*((long *)def) == same) {
      *((long *)def) = dl;
    }
    break;
  case SLType_gfloat :
  case SLType_float :
    if (_textary[i].range) {
      get_op (_textary[i].range, setary, (void *)(&lf), (void *)(&hf),
        (void *)(&df), &deci_places, _textary[i].type);
      free (_textary[i].range);
    }
    if (*((double *)low) == same) {
      *((double *)low) = (double)lf;
    }
    if (*((double *)high) == same) {
      *((double *)high) = (double)hf;
    }
    if (*((double *)def) == same) {
      *((double *)def) = (double)df;
    }
    break;
  case SLType_double :
  case SLType_gdouble :
    if (_textary[i].range) {
      get_op (_textary[i].range, setary, (void *)(&ld), (void *)(&hd),
        (void *)(&dd), &deci_places, _textary[i].type);
      free (_textary[i].range);
    }
    if (*((double *)low) == same) {
      *((double *)low) = ld;
    }
    if (*((double *)high) == same) {
      *((double *)high) = hd;
    }
    if (*((double *)def) == same) {
      *((double *)def) = dd;
    }
    break;
  default:
    assert (0);
    break;
  }

  switch (_textary[i].type) {
  case SLType_uchar :
  case SLType_sint :
    sprintf (fstr, "range:%%hd %%hd,default:%%hd");
    sprintf (tstr, fstr, *(long *)low, *(long *)high, *(long *)def);
    break;
  case SLType_int :
    sprintf (fstr, "range:%%d %%d,default:%%d");
    sprintf (tstr, fstr, *(long *)low, *(long *)high, *(long *)def);
    break;
  case SLType_long :
    sprintf (fstr, "range:%%ld %%ld,default:%%ld");
    sprintf (tstr, fstr, *(long *)low, *(long *)high, *(long *)def);
    break;
  case SLType_float :
  case SLType_double :
    if (precision == same) {
      precision = deci_places;
    }
    sprintf (fstr, "range:%%%1d.%1df %%%1d.%1df,default:%%%1d.%1df",
      precision+1, precision, precision+1, precision, precision+1, precision);
    sprintf (tstr, fstr, *(double *)low, *(double *)high, *(double *)def);
    break;
  case SLType_gfloat :
  case SLType_gdouble :
    if (precision == same) {
      precision = deci_places;
    }
    sprintf (fstr, "range:%%%1d.%1dg %%%1d.%1dg,default:%%%1d.%1dg",
      precision+7, precision, precision+7, precision, precision+7, precision);
    sprintf (tstr, fstr, *(double *)low, *(double *)high, *(double *)def);
    break;
  default:
    assert (0);
    break;
  }

  _textary[i].range = newstr (tstr);
}


void SLTextBox::SetRange (char *name, double low, double high, double def,
  long precision)
{
  wkSetRange (retidx(name), (void *)(&low), (void *)(&high), (void *)(&def),
    precision);
}


void SLTextBox::SetRange (int ident, double low, double high, double def,
  long precision)
{
  wkSetRange (retidx(ident), (void *)(&low), (void *)(&high), (void *)(&def),
    precision);
}


void SLTextBox::SetRange (char *name, float low, float high, float def,
  int precision)
{
  double lo, hi, df;
  long pr;

  if (low == (float)same) {
    lo = same;
  }
  else {
    lo = (double)low;
  }
  if (high == (float)same) {
    hi = same;
  }
  else {
    hi = (double)high;
  }
  if (def == (float)same) {
    df = same;
  }
  else {
    df = (double)def;
  }
  if (precision == (int)same) {
    pr = same;
  }
  else {
    pr = (long)precision;
  }

  wkSetRange (retidx(name), (void *)(&lo), (void *)(&hi), (void *)(&df),
    pr);
}


void SLTextBox::SetRange (int ident, float low, float high, float def,
  int precision)
{
  double lo, hi, df;
  long pr;

  if (low == (float)same) {
    lo = same;
  }
  else {
    lo = (double)low;
  }
  if (high == (float)same) {
    hi = same;
  }
  else {
    hi = (double)high;
  }
  if (def == (float)same) {
    df = same;
  }
  else {
    df = (double)def;
  }
  if (precision == (int)same) {
    pr = same;
  }
  else {
    pr = (long)precision;
  }

  wkSetRange (retidx(ident), (void *)(&lo), (void *)(&hi), (void *)(&df),
    pr);
}


void SLTextBox::SetRange (char *name, long low, long high, long def)
{
  wkSetRange (retidx(name), (void *)(&low), (void *)(&high), (void *)(&def));
}


void SLTextBox::SetRange (int ident, long low, long high, long def)
{
  wkSetRange (retidx(ident), (void *)(&low), (void *)(&high), (void *)(&def));
}


void SLTextBox::SetRange (int ident, String range)
{
  wkSetRange (retidx(ident), range);
}


void SLTextBox::SetRange (char *name, String range)
{
  wkSetRange (retidx(name), range);
}

/*
 * ==========================================================================
 * ================== For Setting Sensitive ==========================
 * ==========================================================================
 */
void SLTextBox::wkSetSensitive( int i, Boolean set )
{
 if ( made() ) {
    XtSetSensitive( _textary[i].tw, set);
    if(_textary[i].lw) XtSetSensitive( _textary[i].lw, set);
 }
 else 
    printf( "SLTextBox::SetSensitive: Class must be made.\n");
}

void SLTextBox::SetSensitive( char *name, Boolean set )
{ wkSetSensitive( retidx(name), set ); }

void SLTextBox::SetSensitive( int  ident, Boolean set )
{ wkSetSensitive( retidx(ident), set ); }


/*
 * ================== For Setting Integer Values ==========================
 */
void SLTextBox::wkSetValue (int i, long lval)
{
  char strval[100];

  if (made()) {
    switch (_textary[i].type) {
    case SLType_uchar :
      *(unsigned char *)_textary[i].target = (unsigned char)lval;
      TEXT_set_uchar (_textary[i].tw, strval, (unsigned char)lval);
      break;
    case SLType_sint :
      *(short *)_textary[i].target = (short)lval;
      TEXT_set_sint (_textary[i].tw, strval, (short)lval);
      break;
    case SLType_int :
      TEXT_set_int (_textary[i].tw, strval, (int)lval);
      *(int *)_textary[i].target = (int)lval;
      break;
    case SLType_long :
      *(long *)_textary[i].target = lval;
      TEXT_set_long (_textary[i].tw, strval, lval);
      break;
    default:
      printf
        ("Error in SLTextBox::SetValue, long passed for float value\n");
      printf ("      Name - %s\n      Ident - %d\n", XtName(_textary[i].tw),
        _textary[i].ident);
    }
  }
  else  {
    switch (_textary[i].type) {
    case SLType_uchar :
      *(unsigned char *)_textary[i].target = (unsigned char)lval;
      break;
    case SLType_sint :
      *(short *)_textary[i].target = (short)lval;
      break;
    case SLType_int :
      *(int *)_textary[i].target = (int)lval;
      break;
    case SLType_long :
    default:
      *(long *)_textary[i].target = lval;
      break;
    }
    _textary[i].value_set = True;
  }
}

void SLTextBox::SetValue (int ident, int ival)
{
  wkSetValue (retidx(ident), (long)ival);
}

void SLTextBox::SetValue (char *name, int ival)
{
  wkSetValue (retidx(name), (long)ival);
}

void SLTextBox::SetValue (int ident, long lval)
{
  wkSetValue (retidx(ident), lval);
}

void SLTextBox::SetValue (char *name, long lval) 
{
  wkSetValue (retidx(name), lval);
}


/*
 * ================== For Setting Float Values ==========================
 */
void SLTextBox::wkSetValue (int i, double dval)
{
  float lf, hf, df;
  double ld, hd, dd;

  long setary[60];
  long deci_places = 2;
  char format_str[20];
  char strval[100];

  Boolean error = False;

  if (made()) {
    switch (_textary[i].type) {
    case SLType_float :
    case SLType_gfloat :
      if (_textary[i].range) {
	get_op (_textary[i].range, setary, (void *)(&lf), (void *)(&hf),
          (void *)(&df), &deci_places, _textary[i].type);
      }
      break;
    case SLType_double :
    case SLType_gdouble :
      if (_textary[i].range) {
	get_op (_textary[i].range, setary, (void *)(&ld), (void *)(&hd),
          (void *)(&dd), &deci_places, _textary[i].type);
      }
      break;
    default:
      printf
        ("Error in SLTextBox::SetValue, double passed for integer value\n");
      printf ("     Name - %s\n      Ident - %d\n", XtName(_textary[i].tw),
        _textary[i].ident);
      error = True;
    }
    if (!error) {
      switch (_textary[i].type) {
      case SLType_float :
	sprintf (format_str, "%%%1d.%1df", deci_places+2, deci_places);
	*(float *)_textary[i].target = (float)dval;
	break;
      case SLType_double :
	sprintf (format_str, "%%%1d.%1df", deci_places+2, deci_places);
	*(double *)_textary[i].target = dval;
	break;
      case SLType_gfloat :
	sprintf (format_str, "%%%1d.%1dg", deci_places+2, deci_places);
	*(float *)_textary[i].target = (float)dval;
	break;
      case SLType_gdouble :
	sprintf (format_str, "%%%1d.%1dg", deci_places+2, deci_places);
	*(double *)_textary[i].target = dval;
	break;
      }
      sprintf (strval, format_str, dval);
      XmTextSetString (_textary[i].tw, strval);
    }
  }
  else  {
    switch (_textary[i].type) {
    case SLType_float :
    case SLType_gfloat :
    default:
      *(float *)_textary[i].target = (float)dval;
      break;
    case SLType_double :
    case SLType_gdouble :
      *(double *)_textary[i].target = dval;
      break;
    }
    _textary[i].value_set = True;
  }
}

void SLTextBox::SetValue (int ident, float fval)
{
  wkSetValue (retidx(ident), (double)fval); 
}

void SLTextBox::SetValue (char *name, float fval)
{
  wkSetValue (retidx(name), (double)fval); 
}

void SLTextBox::SetValue (int ident, double dval)
{
  wkSetValue (retidx(ident), dval); 
}

void SLTextBox::SetValue (char *name, double dval)
{
  wkSetValue (retidx(name), dval); 
}

void SLTextBox::clear (int ident)
{
  long i = retidx (ident);
  if (_textary[i].tw) wprocShowMsg (_textary[i].tw, "");
}

void SLTextBox::clear (char *name)
{
  long i = retidx (name);
  if (_textary[i].tw) wprocShowMsg (_textary[i].tw, "");
}


/*
 * ================== For Getting Values ==========================
 */
long SLTextBox::wkGetInt (int i)
{
  long retval;

  switch (_textary[i].type) {
  case SLType_uchar :
    retval = (long)(*(unsigned char *)_textary[i].target);
    break;
  case SLType_sint :
    retval = (long)(*(short *)_textary[i].target);
    break;
  case SLType_int :
    retval = (long)(*(int *)_textary[i].target);
    break;
  case SLType_long :
    retval = *(long *)_textary[i].target;
    break;
  default:
    assert (0);
    break;
  }
  return retval;
}

long SLTextBox::GetInt (char *name)
{
  return wkGetInt (retidx(name));
}

long SLTextBox::GetInt (int ident)
{
  return wkGetInt (retidx(ident));
}

double SLTextBox::wkGetFloat (int i)
{
  double retval;

  switch (_textary[i].type) {
  case SLType_float :
  case SLType_gfloat :
    retval = (double)(*(float *)_textary[i].target);
    break;
  case SLType_double :
  case SLType_gdouble :
    retval = (*(double *)_textary[i].target);
    break;
  default:
    assert (0);
    break;
  }
  return retval;
}

double SLTextBox::GetFloat (char *name) 
{
  return wkGetFloat (retidx(name));
}

double SLTextBox::GetFloat (int ident)
{
  return wkGetFloat (retidx(ident));
}


/*
 * ================== reload defaults ==========================
 */
void SLTextBox::reloadDefaults(Boolean do_method)
{
  Widget save_popbox;
  int i;

  if ( made() ) {
     for(i= 0; (i< _arycnt); i++) {
         DefLoadWValue(_textary[i].tw);
     }

     _last_valid= False;
     save_popbox= _popbox;
     _popbox= NULL;
     load();
     _popbox= save_popbox;

     for(i= 0; (i< _arycnt); i++) {
         if (do_method) {
            TextAction(_textary[i].ident);
            if (_altLosingAction) 
                  _altLosingAction(_altLosingData, _textary[i].ident);
         }
     } // End loop
  } // End if
  else get_targets();

}

/*
void SLTextBox::operator=(int value)  
{ 
  printf("in =\n");
  //SetValue(_last_idx,value);
}

void SLTextBox::operator=(float value) { SetValue(_last_idx,value);}


SLTextBox *SLTextBox::operator[](const int idx)
{ 
  printf("in []\n");
  _last_idx= idx; return this;
};
*/


/*
void SLTextBox::operator=(int value)  { SetValue(_last_idx,value);}
void SLTextBox::operator=(float value) { SetValue(_last_idx,value);}
SLTextBox *SLTextBox::operator[](const int idx) { _last_idx= idx; return this;};
*/




void SLTextBox::highlight(int  ident)
{
 sendHighlight(retidx(ident));
}

void SLTextBox::highlight(char *name)
{
 sendHighlight(retidx(name));
}

void SLTextBox::sendHighlight(long i)
{
 Status stat;
 XClientMessageEvent ev;
 ev.type         = ClientMessage;
 ev.data.l[0]    = i;
 ev.window       = XtWindow(topWidget());
 ev.display      = XtDisplay(topWidget());
 ev.message_type = XA_INTEGER;
 ev.format       = 32;



 stat= XSendEvent(XtDisplay(topWidget()), XtWindow(topWidget()), 
                            True, NoEventMask,(XEvent*)&ev);
}

void SLTextBox::highlightHandler(Widget w, XtPointer udata, XEvent *event)
{
  SLTextBox *obj = (SLTextBox *)udata;
  obj->doHighlight(w, udata, event);
}


void SLTextBox::doHighlight(Widget w, XtPointer, XEvent *event)
{
  if (event->type == ClientMessage) {
        XmUpdateDisplay(W());
        XFlush(XtDisplay(w));
        XClientMessageEvent *ev= (XClientMessageEvent *)event;
        Widget tw= _textary[ev->data.l[0]].tw; 
        char *str= XmTextGetString(tw);
        int  len=strlen(str);
        XtFree(str);
        XmTextSetSelection( tw, 0, len, CurrentTime);
  } // End if
}

SLTextBox::TextReason SLTextBox::lastNotifyReason()
{
  return _last_reason;
}
