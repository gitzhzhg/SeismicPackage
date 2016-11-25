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
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "wproc.h"
#include <assert.h>

#define SETCMD "set:"
#define SETLEN 4
#define RNGCMD "range:"
#define RNGLEN 6
#define DEFCMD "default:"
#define DEFLEN 8
#define CESTR "get_op error: could not convert string - %s\n"



enum chk_actions get_op (char str[], long setary[], void *low, void *high,
  void *def, long *deci_places, long type)
{
  enum chk_actions stat;
  char wstr[300];
  long cnvstat;
  long len, i;
  char *cstart, *comma, *defstr= NULL, *tmpstr= NULL;
  char hilow[200];
  long hilowlen;
  long dp0 = 0, dp1 = 0;

  if (!isascii(str[0]) ) {
    stat = NOCHK;
    fprintf (stderr, "get_op error: bad string passed.\n");
  }
  else {             /* what was passed was a string */
    stat = NOCHK;
    if (str[0]=='*') {
      /* add code to get a string out of the resource file */
    }
    else {
      strcpy (wstr, str);
    }
    len = strlen (wstr);
    for (i = 0; i < len; i++) {
      wstr[i]= tolower(wstr[i]);
    }
    cstart= strstr( wstr, SETCMD );
    /*
     * we are looking at a set string
     */
    if (cstart) {   /* start of set stuff
                     * do set stuff here - NOT YET IMPLEMENTED */
      stat = SETCHK;
    }               /* end of set stuff */
    /*
     * we are looking at a range string
     */
    else {          /* start of range stuff */
      cstart = strstr (wstr, RNGCMD);
      /*
       * find the start of the high and the low numbers
       */
      if (cstart) {
	cstart += RNGLEN;
	stat = RANGECHK;
	comma= strchr (wstr, ',');
	/*
	 * if a comma we know that a default value should be there
	 */
	if (comma) {
	  hilowlen = comma - cstart;
	  defstr = strstr (comma, DEFCMD); 
	  /*
	   * get the default value
	   */
	  if (defstr) {
	    defstr += DEFLEN;
	    switch (type) {
	    case TYPE_UCHAR :
	      cnvstat = sscanf (defstr, "%hd", (unsigned char *)def);
	      break;
	    case TYPE_SINT :
	      cnvstat = sscanf (defstr, "%hd", (short *)def);
	      break;
	    case TYPE_INT :
	      cnvstat = sscanf (defstr, "%d", (int *)def);
	      break;
	    case TYPE_LONG :
	      cnvstat = sscanf (defstr, "%ld", (long *)def);
	      break;
	    case TYPE_FLT :
	      cnvstat = sscanf (defstr, "%f", (float *)def);
	      break;
	    case TYPE_GFLT :
	      cnvstat = sscanf (defstr, "%g", (float *)def);
	      break;
	    case TYPE_DBL :
	      cnvstat = sscanf (defstr, "%lf", (double *)def);
	      break;
	    case TYPE_GDBL :
	      cnvstat = sscanf (defstr, "%lg", (double *)def);
	      break;
	    default:
	      assert (0);
	      break;
	    }
	    if (cnvstat != 1) {
	      fprintf (stderr, CESTR, str);
	      stat= NOCHK;
	    }
	  }
	  else {     /* if not default string set defaults to 0 */
	    switch (type) {
	    case TYPE_UCHAR :
	      *(unsigned char *)def = 0;
	      break;
	    case TYPE_SINT :
	      *(short *)def = 0;
	      break;
	    case TYPE_INT :
	      *(int *)def = 0;
	      break;
	    case TYPE_LONG :
	      *(short *)def = 0;
	      break;
	    case TYPE_FLT :
	    case TYPE_GFLT :
	      *(float *)def = 0;
	      break;
	    case TYPE_DBL :
	    case TYPE_GDBL :
	      *(double *)def = 0;
	      break;
	    default:
	      assert (0);
	      break;
	    }
	  }
	}
	else {
	  hilowlen = strlen(cstart);
	}
	/*
	 * get the high and low value 
	 */
	strncpy (hilow, cstart, hilowlen);
	hilow[hilowlen] = '\0';

	switch (type) {
	case TYPE_UCHAR :
	  *(unsigned char *)low = UCHAR_MAX;
	  cnvstat = sscanf (hilow, "%hd %hd", (unsigned char *)low,
            (unsigned char *)high);
	  if ((cnvstat == 1) && ((*(unsigned char *)low != UCHAR_MAX))) {
            if (strchr(hilow,'*')) *(unsigned char *)high = UCHAR_MAX;
	  }
	  else if (cnvstat != 2) {
	    fprintf (stderr, CESTR, str);
	    stat = NOCHK;
	  }
	  break;
	case TYPE_SINT :
	  *(short *)low = SHRT_MAX;
	  cnvstat = sscanf (hilow, "%hd %hd", (short *)low, (short *)high);
	  if ((cnvstat == 1) && ((*(short *)low != SHRT_MAX))) {
            if (strchr(hilow,'*')) *(short *)high = SHRT_MAX;
	  }
	  else if (cnvstat != 2) {
	    fprintf (stderr, CESTR, str);
	    stat = NOCHK;
	  }
	  break;
	case TYPE_INT :
	  *(int *)low = INT_MAX;
	  cnvstat = sscanf (hilow, "%d %d", (int *)low, (int *)high);
	  if ((cnvstat == 1) && ((*(int *)low != INT_MAX))) {
            if (strchr(hilow,'*')) *(int *)high = INT_MAX;
	  }
	  else if (cnvstat != 2) {
	    fprintf (stderr, CESTR, str);
	    stat = NOCHK;
	  }
	  break;
	case TYPE_LONG :
	  *(long *)low = LONG_MAX;
	  cnvstat = sscanf (hilow, "%ld %ld", (long *)low, (long *)high);
	  if ((cnvstat == 1) && ((*(long *)low != LONG_MAX))) {
            if (strchr(hilow,'*')) *(long *)high = LONG_MAX;
	  }
	  else if (cnvstat != 2) {
	    fprintf (stderr, CESTR, str);
	    stat = NOCHK;
	  }
	  break;
	case TYPE_FLT :
	case TYPE_GFLT :
	  *(float *)low = FLT_MAX;
	  if (type == TYPE_FLT) {
	    cnvstat = sscanf (hilow, "%f %f", (float *)low, (float *)high);
	  }
	  else {
	    cnvstat = sscanf (hilow, "%g %g", (float *)low, (float *)high);
	  }
	  if ((cnvstat == 1) && ((*(float *)low != FLT_MAX))) {
            if (strchr(hilow,'*')) *(float *)high = FLT_MAX;
	  }
	  else if (cnvstat != 2) {
	    fprintf (stderr, CESTR, str);
	    stat = NOCHK;
	  }
	  break;
	case TYPE_DBL :
	case TYPE_GDBL :
	  *(double *)low = DBL_MAX;
	  if (type == TYPE_DBL) {
	    cnvstat = sscanf (hilow, "%lf %lf", (double *)low,
              (double *)high);
	  }
	  else {
	    cnvstat = sscanf (hilow, "%lg %lg", (double *)low,
              (double *)high);
	  }
	  if ((cnvstat == 1) && ((*(double *)low != DBL_MAX))) {
            if (strchr(hilow,'*')) *(double *)high = DBL_MAX;
	  }
	  else if (cnvstat != 2) {
	    fprintf (stderr, CESTR, str);
	    stat = NOCHK;
	  }
	  break;
	default:
	  assert (0);
	  break;
	}
      } /* end of range stuff */
    }
  }

  switch (type) {
  case TYPE_FLT :
  case TYPE_GFLT :
    if (stat != NOCHK) {
      /* pick a precision that considers both minimum and default */
      if (defstr) {
	tmpstr= strchr (defstr, '.');
        if (tmpstr) {
	  tmpstr++;
 	  for (dp0=1; ((*tmpstr) && isdigit(*tmpstr));
	    tmpstr++, dp0++);
        }
      }
      if (hilow) {
	tmpstr= strchr (hilow, '.');
        if (tmpstr) {
	  tmpstr++;
 	  for (dp1=1; ((*tmpstr) && isdigit(*tmpstr));
	    tmpstr++, dp1++);
        }
      }
      if (dp0 < 2 && dp1 < 2) {
	*deci_places = 1; /* neither min nor def had any decimals */
      }
      else {
	/* do not allow decimal places exceed machine precision */
	*deci_places = dp1 > dp0
                       ?
                         (dp1 > FLT_DIG ? FLT_DIG : dp1)
                       :
	                 (dp0 > FLT_DIG ? FLT_DIG : dp0);
      }
    }
    else {
      *deci_places= 0;
    }
    break;

  case TYPE_DBL :
  case TYPE_GDBL :
    if (stat != NOCHK) {
      /* pick a precision that considers both minimum and default */
      if (defstr) {
	tmpstr= strchr (defstr, '.');
        if (tmpstr) {
	  tmpstr++;
 	  for (dp0=1; ((*tmpstr) && isdigit(*tmpstr));
	    tmpstr++, dp0++);
        }
      }
      if (hilow) {
	tmpstr= strchr (hilow, '.');
        if (tmpstr) {
	  tmpstr++;
 	  for (dp1=1; ((*tmpstr) && isdigit(*tmpstr));
	    tmpstr++, dp1++);
        }
      }
      if (dp0 < 2 && dp1 < 2) {
	*deci_places = 1; /* neither min nor def had any decimals */
      }
      else {
	/* do not allow decimal places exceed machine precision */
	*deci_places = dp1 > dp0
                       ?
                         (dp1 > DBL_DIG ? DBL_DIG : dp1)
                       :
	                 (dp0 > DBL_DIG ? DBL_DIG : dp0);
      }
    }
    else {
      *deci_places= 0;
    }
    break;
  default:
    *deci_places = 0;
    break;
  }

  return (stat);
}
