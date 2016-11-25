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
#include "cenv.h"
#include <math.h>
#include <ctype.h>
#include <limits.h>
#include <float.h>
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Text.h>
#include <X11/Shell.h>
#include "wproc.h"
#include <assert.h>

#define Istr "Must be a positive Integer"
#define SIstr "Must be an integer"
#define Rstr "Must be a real"
#define RIstr "Must be an integer between %d and %d"
#define RIGstr "Must be an integer greater than %d"
#define RIlstr "Must be an integer between %ld and %ld"
#define RIGlstr "Must be an integer greater than %ld"
#define RRstr "Must be a real between %s and %s"
#define RRGstr "Must be an real greater than %s"

enum chk_actions get_op
  (char str[],
   long setary[],
   void *low,
   void *high,
   void *def,
   long *deci_places,
   long type);


static long load_data (struct CB *udata, char *argval, Widget w,
  XmTextVerifyCallbackStruct *winfo)
{
  enum chk_actions chk_oper;

  unsigned char lc, hc, dc;
  short ls, hs, ds;
  int   li, hi, di;
  float lf, hf, df;
  long  ll, hl, dl, deci_places = 2;
  double ld, hd, dd;

  long    setary[40];
  Boolean res;
  long    succ;

  unsigned char valc;
  short vals;
  int   vali;
  long  vall;
  float valf;
  double vald;

  char Mstr[300], defstr[30], outRRstr[90];
  char outstr[20], format_str[20];

  chk_oper = NOCHK;
  succ = False;

  switch (udata->type) {
  case TYPE_UCHAR :
    if (udata->info) {
      chk_oper = get_op (udata->info, setary, &lc, &hc, &dc, &deci_places,
	udata->type);
    }
    else {
      chk_oper=NOCHK;
    }
    sprintf (defstr, "%d", dc);
    res = check_uchar (argval, &valc);
    if (res) {
      if (chk_oper == NOCHK) {
	*(unsigned char *)udata->fldptr = valc;
	succ = True;
	sprintf (outstr, "%d", valc);
      }
      else if (chk_oper == RANGECHK) {
	if (valc >= lc && valc <= hc) {
	  *(unsigned char *)udata->fldptr = valc;
	  succ = True;
	  sprintf (outstr, "%d", valc);
	}
	else {
	  if (udata->popbox) {
	    if (hc == UCHAR_MAX) { 
	      wprocVAShowMsg (udata->popbox, RIGstr, lc);
	    }
	    else {
	      wprocVAShowMsg (udata->popbox, RIstr, lc, hc);
	    }
	  }
	}
      }
    }
    break;
  case TYPE_SINT :
    if (udata->info) {
      chk_oper = get_op (udata->info, setary, &ls, &hs, &ds, &deci_places,
	udata->type);
    }
    else {
      chk_oper=NOCHK;
    }
    sprintf (defstr, "%d", ds);
    res = check_sint (argval, &vals);
    if (res) {
      if (chk_oper == NOCHK) {
	*(short *)udata->fldptr = vals;
	succ = True;
	sprintf (outstr, "%d", vals);
      }
      else if (chk_oper == RANGECHK) {
	if (vals >= ls && vals <= hs) {
	  *(short *)udata->fldptr = vals;
	  succ = True;
	  sprintf (outstr, "%d", vals);
	}
	else {
	  if (udata->popbox) {
	    if (hs == SHRT_MAX) { 
	      wprocVAShowMsg (udata->popbox, RIGstr, ls);
	    }
	    else {
	      wprocVAShowMsg (udata->popbox, RIstr, ls, hs);
	    }
	  }
	}
      }
    }
    break;
  case TYPE_INT :
    if (udata->info) {
      chk_oper = get_op (udata->info, setary, &li, &hi, &di, &deci_places,
        udata->type);
    }
    else {
      chk_oper=NOCHK;
    }
    sprintf (defstr, "%d", di);
    res = check_int (argval, &vali);
    if (res) {
      if (chk_oper == NOCHK) {
	*(int *)udata->fldptr = vali;
	succ = True;
	sprintf (outstr, "%d", vali);
      }
      else if (chk_oper == RANGECHK) {
	if (vali >= li && vali <= hi) {
	  *(int *)udata->fldptr = vali;
	  succ = True;
	  sprintf (outstr, "%d", vali);
	}
	else {
	  if (udata->popbox) {
	    if (hi == INT_MAX) { 
	      wprocVAShowMsg (udata->popbox, RIGstr, li);
	    }
	    else {
	      wprocVAShowMsg (udata->popbox, RIstr, li, hi);
	    }
	  }
	}
      }
    }
    break;
  case TYPE_LONG :
    if (udata->info) {
      chk_oper = get_op (udata->info, setary, &ll, &hl, &dl, &deci_places,
	udata->type);
    }
    else {
      chk_oper=NOCHK;
    }
    sprintf (defstr, "%ld", dl);
    res = check_long (argval, &vall);
    if (res) {
      if (chk_oper == NOCHK) {
	*(long *)udata->fldptr = vall;
	succ = True;
	sprintf (outstr, "%ld", vall);
      }
      else if (chk_oper == RANGECHK) {
	if (vali >= ll && vall <= hl) {
	  *(long *)udata->fldptr = vall;
	  succ = True;
	  sprintf (outstr, "%ld", vall);
	}
	else {
	  if (udata->popbox) {
	    if (hl == LONG_MAX) { 
	      wprocVAShowMsg (udata->popbox, RIGlstr, ll);
	    }
	    else {
	      wprocVAShowMsg (udata->popbox, RIlstr, ll, hl);
	    }
	  }
	}
      }
    }
    break;
  case TYPE_FLT  :
  case TYPE_GFLT :
    if (udata->info) {
      chk_oper = get_op (udata->info, setary, &lf, &hf, &df, &deci_places,
	udata->type);
    }
    else {
      chk_oper=NOCHK;
    }
    if (udata->type == TYPE_FLT) {
      sprintf (format_str, "%%%1d.%1df", deci_places+2, deci_places);
    }
    else /* if (udata->type == TYPE_GFLT) */ {
      sprintf (format_str, "%%%1d.%1dg", deci_places+2, deci_places);
    }
    sprintf (defstr, format_str, df);
    res = check_flt (argval, &valf);
    if (res) {
      if (chk_oper == NOCHK) {
	*(float *)udata->fldptr = valf;
	succ = True;
	sprintf (outstr, format_str, valf);
      }
      else if (chk_oper == RANGECHK) {
	if (valf >= lf && valf <= hf) {
	  *(float *)udata->fldptr = valf;
	  succ = True;
	  sprintf (outstr, format_str, valf);
	}
	else {
	  if (hf != FLT_MAX) {
	    sprintf (outRRstr, RRstr, format_str, format_str);
	  }
	  else {
	    sprintf (outRRstr, RRGstr, format_str);
	  }
	  sprintf (Mstr, outRRstr, lf, hf);
	  if (udata->popbox) {
	    show_msg (udata->popbox, Mstr);
	  }
	}
      }
    }
    break;
  case TYPE_DBL  :
  case TYPE_GDBL :
    if (udata->info) {
      chk_oper = get_op (udata->info, setary, &ld, &hd, &dd, &deci_places,
	udata->type);
    }
    else {
      chk_oper=NOCHK;
    }
    if (udata->type == TYPE_DBL) {
      sprintf (format_str, "%%%1d.%1df", deci_places+2, deci_places);
    }
    else /* if (udata->type == TYPE_GDBL) */ {
      sprintf (format_str, "%%%1d.%1dg", deci_places+2, deci_places);
    }
    sprintf (defstr, format_str, dd);
    res = check_dbl (argval, &vald);
    if (res) {
      if (chk_oper == NOCHK) {
	*(double *)udata->fldptr = vald;
	succ = True;
	sprintf (outstr, format_str, vald);
      }
      else if (chk_oper == RANGECHK) {
	if (vald >= ld && vald <= hd) {
	  *(double *)udata->fldptr = vald;
	  succ = True;
	  sprintf (outstr, format_str, vald);
	}
	else {
	  if (dd != DBL_MAX) {
	    sprintf (outRRstr, RRstr, format_str, format_str);
	  }
	  else {
	    sprintf (outRRstr, RRGstr, format_str);
	  }
	  sprintf (Mstr, outRRstr, ld, hd);
	  if (udata->popbox) {
	    show_msg (udata->popbox, Mstr);
	  }
	}
      }
    }
    break;
  default:
    assert (0);
    break;
  }

  if (succ && udata->more_func) {
    succ = (long)udata->more_func (w, udata, winfo);
  }

  if (succ) {
    XmTextSetString (w, outstr);
    if (winfo->reason != XmCR_ACTIVATE) {
      winfo->currInsert=0;
      winfo->newInsert=0;
    }
  }
  else {
    if (winfo->reason != XmCR_ACTIVATE) {
      if (winfo->event) {
	if (winfo->event->type == KeyPress) {
	  winfo->doit = FALSE;
	}
      }
      winfo->currInsert = 0;
      winfo->newInsert = 0;
    }
    XmTextSetString (w, defstr);
  }
  return succ;
}



long dotextcb (Widget w, struct CB *udata, XmTextVerifyCallbackStruct *winfo)
{
  char *argval;
  long i;
  int rev;
  Window curr_win;     /* current focus window */
  Window shell_wid;    /* shell window id */
  Widget sw;           /* tempory widget id to find shell widget */
  long stat = 0;
  long cb_type;

/* 06-13-02 (KCC & MLS) to fix a problem with GpSeisModelPop.cc -- start */
  cb_type = udata->type;
  argval  = XmTextGetString (w);   /* get the string in the text widget */

  switch (cb_type) {

  case TYPE_UCHAR :
  case TYPE_SINT  :
  case TYPE_INT   :
  case TYPE_LONG  :
  case TYPE_FLT   :
  case TYPE_GFLT  :
  case TYPE_DBL   :
  case TYPE_GDBL  :
    if (winfo->reason == XmCR_ACTIVATE     ||
	winfo->reason == XmCR_LOSING_FOCUS   ) {
      stat = load_data (udata, argval, w, winfo);
    }
    break;

  default:
/* 06-13-02 (KCC & MLS) to fix a problem with GpSeisModelPop.cc -- stop */
    XGetInputFocus( XtDisplay(w), &curr_win, &rev );
 /*
  * find the shell widget id but just to this iterations up to 40 times if 
  * we try more than 40 time something is wrong
  */
    for (sw = w, i = 0; !XtIsShell(sw) && i < 40; sw = XtParent(sw), i++);
    shell_wid = XtWindow (sw);
    if (shell_wid == curr_win || winfo->reason == XmCR_ACTIVATE) {
      switch (cb_type) {

/* 06-13-02 (KCC & MLS) to fix a problem with GpSeisModelPop.cc -- start
 * this logic caused an error in the GEOPRESS(tm) application because in
 * GpModelSliceGui the user could enter data into a _c_box text widget
 * and seeing his entry, he could click on the GpModelGridGui resolution
 * option menu (i.e. lose focus) and the entered data was never loaded
 * into the target variable.  Thus the above code was implemented
     case TYPE_SINT :
     case TYPE_INT  :
     case TYPE_FLT  :
     case TYPE_GFLT :
       stat= load_data( udata, argval, w, winfo );
       break;
 * 06-13-02 (KCC & MLS) to fix a problem with GpSeisModelPop.cc -- stop */
       
      case TYPE_STR :
	strcpy (udata->fldptr, argval);
	if (udata->more_func) udata->more_func (w, udata, winfo);
	break;
       
      case TYPE_FILE :
	stat = dofilecb (w, udata, winfo);
	break;
       
      }
    }
/* 06-13-02 (KCC & MLS) to fix a problem with GpSeisModelPop.cc -- start */
  }
  XtFree (argval);     /* free the memory that holds the string */
/* 06-13-02 (KCC & MLS) to fix a problem with GpSeisModelPop.cc -- stop */

  return stat;
}
