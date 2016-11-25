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

//-------------------------- vf_edit_sort.cc ---------------------------//
//-------------------------- vf_edit_sort.cc ---------------------------//
//-------------------------- vf_edit_sort.cc ---------------------------//

//            implementation file for the VfEditSort class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_sort.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_function.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_function_array.hh"
#include "oprim/fast_sort.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//


VfEditSort::VfEditSort()
           : VfEditBase(INFORM_TOTAL, "sort"),
             _xdirwant      (DIR_EITHER),
             _ydirwant      (DIR_EITHER),
             _xfast         (TRUE)
{
}



VfEditSort::~VfEditSort()
{
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditSort::setXdirection (int   value)
{
  assert(value == DIR_ASCENDING  ||
         value == DIR_DESCENDING ||
         value == DIR_EITHER);
  _xdirwant = value;
}


void   VfEditSort::setYdirection (int   value)
{
  assert(value == DIR_ASCENDING  ||
         value == DIR_DESCENDING ||
         value == DIR_EITHER);
  _ydirwant = value;
}


//----------------------- cat directions -------------------------//
//----------------------- cat directions -------------------------//
//----------------------- cat directions -------------------------//

     // appends information to msg (which must already be set).
     // gets information from values of xdir and ydir.

static void cat_directions(char *msg, int xdir, int ydir)
{
  if     (xdir ==  2) strcat(msg, " - X increasing");
  else if(xdir == -2) strcat(msg, " - X decreasing");
  else if(ydir ==  1) strcat(msg, " - X unsorted");
  else if(ydir == -1) strcat(msg, " - X unsorted");
  else                strcat(msg, " - X all same");
  if     (ydir ==  2) strcat(msg, " - Y increasing");
  else if(ydir == -2) strcat(msg, " - Y decreasing");
  else if(ydir ==  1) strcat(msg, " - Y unsorted");
  else if(ydir == -1) strcat(msg, " - Y unsorted");
  else                strcat(msg, " - Y all same");
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditSort::virtualCheck (VfKernal *kernal, char *msg)
{
  long nfun = kernal->numVelocityFunctions();
  if(nfun == 1)
      {
      strcpy(msg, "only one velocity function to sort");
      return TRUE;
      }
  int sorted, xdir, ydir;
  sorted = checkSort(kernal, &xdir, &ydir);
                                  // sets xdir and ydir to current order.
  if(sorted)
      {
      strcpy(msg, "velocity functions already sorted");
      cat_directions(msg, xdir, ydir);
      return TRUE;
      }
  strcpy(msg, "sorting velocity functions...");
  return FALSE;
}



//------------------------ check sort -------------------------------//
//------------------------ check sort -------------------------------//
//------------------------ check sort -------------------------------//

    // private.

    // sets *xdir to  2 if xbins are sorted to ascending order.
    // sets *xdir to -2 if xbins are sorted to descending order.
    // sets *xdir to  1 if xbins are not sorted but initially ascending.
    // sets *xdir to -1 if xbins are not sorted but initially descending.
    // sets *xdir to  0 if no velocity functions or all xbins are the same.

    // sets *ydir to  2 if ybins are sorted to ascending order.
    // sets *ydir to -2 if ybins are sorted to descending order.
    // sets *ydir to  1 if ybins are not sorted but initially ascending.
    // sets *ydir to -1 if ybins are not sorted but initially descending.
    // sets *ydir to  0 if no velocity functions or all ybins are the same.

    // if already sorted consistent with _xdirwant and _ydirwant and _xfast,
    //   returns sorted = TRUE.
    // otherwise returns sorted = FALSE.

int VfEditSort::checkSort (VfKernal *kernal, int *xdir, int *ydir)
{
/*
//////////  int sorted = kernal->checkSort(xdir, ydir);   ///// alternative.

  *xdir = kernal->checkXdirection();
  *ydir = kernal->checkYdirection();
  int sorted = TRUE;
  if(*xdir == 1 || *xdir == -1)                                sorted = FALSE;
  if(*ydir == 1 || *ydir == -1)                                sorted = FALSE;
//////// above 5 lines not needed if use kernal->checkSort.
*/
  int sorted = kernal->checkSort(xdir, ydir, &_xfast);

  if(_xdirwant == DIR_ASCENDING  && *xdir !=  2 && *xdir != 0) sorted = FALSE;
  if(_xdirwant == DIR_DESCENDING && *xdir != -2 && *xdir != 0) sorted = FALSE;
  if(_ydirwant == DIR_ASCENDING  && *ydir !=  2 && *ydir != 0) sorted = FALSE;
  if(_ydirwant == DIR_DESCENDING && *ydir != -2 && *ydir != 0) sorted = FALSE;
  return sorted;
}



//--------------------- get sort key ----------------------------//
//--------------------- get sort key ----------------------------//
//--------------------- get sort key ----------------------------//

static float      XCENTER;
static float      YCENTER;
static float      XWIDTH;
static float      YWIDTH;

static long       IXMIN;
static long       IXMAX;
static long       IYMIN;
static long       IYMAX;

static int        XDIR;      // >=0 (ascending) or <0 (descending).
static int        YDIR;      // >=0 (ascending) or <0 (descending).
static int        XFAST;     // true or false.

static float get_sort_key(void *data, long index)
{
  VfFunctionArray *array = (VfFunctionArray*)data;
  VfFunction *velfun = array->velfun(index);
  float xloc = velfun->getXloc();
  float yloc = velfun->getYloc();

  long ixbin = BinNumber(xloc, XCENTER, XWIDTH);
  long iybin = BinNumber(yloc, YCENTER, YWIDTH);

  long ixkey;
  long iykey;
  if(XDIR >= 0) ixkey = ixbin - IXMIN;
  else          ixkey = IXMAX - ixbin;
  if(YDIR >= 0) iykey = iybin - IYMIN;
  else          iykey = IYMAX - iybin;
  assert(ixkey >= 0);
  assert(iykey >= 0);

  long key;
  if(XFAST) key = (IXMAX - IXMIN + 1) * iykey + ixkey;
  else      key = (IYMAX - IYMIN + 1) * ixkey + iykey;
  return (float)key;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditSort::virtualEdit (VfKernal *kernal, char *msg)
{
  long nfun = kernal->numVelocityFunctions();
  int sorted, xdir, ydir;
  sorted = checkSort(kernal, &xdir, &ydir);
                                  // sets xdir and ydir to current order.
  XCENTER    = kernal->utilities()->getXcenter();
  YCENTER    = kernal->utilities()->getYcenter();
  XWIDTH     = kernal->utilities()->getXwidth ();
  YWIDTH     = kernal->utilities()->getYwidth ();
  float xmin = kernal->minimumXloc();
  float xmax = kernal->maximumXloc();
  float ymin = kernal->minimumYloc();
  float ymax = kernal->maximumYloc();
  IXMIN      = BinNumber(xmin, XCENTER, XWIDTH);
  IXMAX      = BinNumber(xmax, XCENTER, XWIDTH);
  IYMIN      = BinNumber(ymin, YCENTER, YWIDTH);
  IYMAX      = BinNumber(ymax, YCENTER, YWIDTH);
  if     (_xdirwant == DIR_ASCENDING ) XDIR =  1;
  else if(_xdirwant == DIR_DESCENDING) XDIR = -1;
  else                                 XDIR = xdir;
  if     (_ydirwant == DIR_ASCENDING ) YDIR =  1;
  else if(_ydirwant == DIR_DESCENDING) YDIR = -1;
  else                                 YDIR = ydir;
  XFAST = _xfast;

  kernal->beforeSettingSeveralSelectFlags();
  FastSort *sort = new FastSort (kernal->array(), get_sort_key);
  sort->sort(1);
  delete sort;
  kernal->afterSettingSeveralSelectFlags();

  sorted = checkSort(kernal, &xdir, &ydir);
                                  // sets xdir and ydir to current order.
  if(sorted) strcpy(msg, "velocity functions successfully sorted");
  else       strcpy(msg, "failed to properly sort velocity functions");
  cat_directions(msg, xdir, ydir);
  return !sorted;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

