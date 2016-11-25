
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
//---------------------- ld_table_gui.cc -----------------------//
//---------------------- ld_table_gui.cc -----------------------//
//---------------------- ld_table_gui.cc -----------------------//

//          implementation file for the LdTableGui class
//                derived from the SLDatabox class
//                       subdirectory fggui


#include "fggui/ld_table_gui.hh"
#include "fggui/ld_top_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/sl_prim.hh"
#include "wbox.h"
#include "cprim.h"
#include "str.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { LD_S       = LAST_FIELD_FLAG_VARIABLE  +  1 };
enum { LD_R       = LAST_FIELD_FLAG_VARIABLE  +  2 };
enum { LD_DIAMOND = LAST_FIELD_FLAG_VARIABLE  +  3 };
enum { LD_GP1     = LAST_FIELD_FLAG_VARIABLE  +  4 };
enum { LD_GP2     = LAST_FIELD_FLAG_VARIABLE  +  5 };
enum { LD_GP3     = LAST_FIELD_FLAG_VARIABLE  +  6 };
enum { LD_UNIFORM = LAST_FIELD_FLAG_VARIABLE  +  7 };
enum { LD_ERROR   = LAST_FIELD_FLAG_VARIABLE  +  8 };
enum { LD_SDEAD   = LAST_FIELD_FLAG_VARIABLE  +  9 };
enum { LD_RDEAD   = LAST_FIELD_FLAG_VARIABLE  + 10 };

enum { NCOLUMNS = NUM_FIELD_FLAG_VARIABLES + 10 };

enum { USE_FIXDIST = 888, USE_AVERAGE, USE_THIS };


         // later we must decide whether to use:
         //      both LD_GP1 and LD_GP2,
         //      or instead just LD_GP3.


static const double DZERO  = 0.0;
static       int    XYCHAR = 8;
static       int    XYDEC  = 3;     // changed from 2  9/8/00.


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


LdTableGui::LdTableGui(SLDelay *slparent, char *name, FieldGeometry *fg,
                                       LdTopGui *top, int table_option)
           : SLDatabox(slparent, name, NULL, 4),
             FgInform(fg),
                 _top              (top),
                 _table_option     (table_option),
                 _sw               (NULL),
                 _use_which        (USE_AVERAGE),
                 _use_this         (0.0),
                 _inc_percent      (10),
                 _cum_percent      (50),
                 _azim_change      (20)
{
  assert(fg);
  _sw = new long [NCOLUMNS];
  for(long i = 0; i < NCOLUMNS; i++) { _sw[i] = -1; }
  _sw[FG_SHOT    - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[FG_DIST    - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[FG_XLOC    - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[FG_YLOC    - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[FG_ELEV    - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[LD_ERROR   - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[FG_CUM     - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[LD_UNIFORM - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[FG_AZIM    - FIRST_FIELD_FLAG_VARIABLE] = 1;
  _sw[LD_GP3     - FIRST_FIELD_FLAG_VARIABLE] = 1;
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

LdTableGui::~LdTableGui()
{
  delete [] _sw;
}



//------------- create and delete test inform --------------------//
//------------- create and delete test inform --------------------//
//------------- create and delete test inform --------------------//

      // for testing purposes.
      // use ^T on the tables to create or delete.

void create_or_delete_test_inform(FieldGeometry *fg)
{
  static FgInform *test_inform = NULL;
  if(test_inform)
      {
      delete test_inform;
      test_inform = NULL;
      }
  else
      {
      test_inform = new FgInform(fg, TRUE);
      }
}





//-------------------- post new active line ----------------------//
//-------------------- post new active line ----------------------//
//-------------------- post new active line ----------------------//

       // virtual function overriding FgInform.

void LdTableGui::postNewActiveFlag(FieldGeometry *fg, long ixl)
{
  void *box = getBox();
  long active_ixl = fg->getActiveLineIndex();
  if(active_ixl == -1) return;
  if(active_ixl != ixl) return;
  long ixf = fg->getActiveFlagIndexOnLine(ixl);
  if(ixf >= 0) wbox_set_focus(box, LD_DIAMOND, (int)ixf+1);
  _sw[LD_DIAMOND - FIRST_FIELD_FLAG_VARIABLE] = 1;
}


//----------------- maybe tack on digits ---------------------//
//----------------- maybe tack on digits ---------------------//
//----------------- maybe tack on digits ---------------------//

static double maybe_tack_on_digits(FieldGeometry *fg,
              int ident, long ixl, long ixf, double value, long nread)
{
  if(ixf == 0) return value;
  if(fg->flagValueIsDependent(ixl, ixf-1, ident)) return value;
  double prev;
  switch(ident)
      {
      case FG_XLOC:  prev = fg->getXloc(ixl, ixf-1); break;
      case FG_YLOC:  prev = fg->getYloc(ixl, ixf-1); break;
      default:       return value;
      }
  if(value == DNIL) return value;
  if(prev  == DNIL) return value;
  if(value <  0.0) return value;
  if(prev  == 0.0) return value;
  if(value != (double)NearestInteger(value)) return value;
  if(prev  != (double)NearestInteger(prev )) return value;
  char cvalue[80];
  char cprev [80];
/*
  convert_dd2ss(&value, cvalue, &XYCHAR, &XYDEC);
  convert_dd2ss(&prev , cprev , &XYCHAR, &XYDEC);
*/
  str_dd2ss(value, cvalue, XYCHAR, XYDEC);
  str_dd2ss(prev , cprev , XYCHAR, XYDEC);
  long lvalue = strlen(cvalue);
  long lprev  = strlen(cprev );
  if(nread  >= lprev) return value;
  if(lvalue >= lprev) return value;
  if(lvalue ==   0  ) return value;
  char *test;
  test = strpbrk(cvalue, ".*e+-");  if(test) return value;
  test = strpbrk(cprev , ".*e+" );  if(test) return value;
  cprev[lprev - lvalue] = '\0';
  strcat(cprev, cvalue);
  double value2;
  int istat;
/*
  convert_ss2dd(cprev, &value2, &istat);
*/
  str_ss2dd(cprev, &value2, &istat);
  if(istat <= 0) return value;
  return value2;
}



//----------------- append flag if necessary ---------------------//
//----------------- append flag if necessary ---------------------//
//----------------- append flag if necessary ---------------------//

         // to be called from a trap.
         // tries to append a flag if index == n.
         // tries to insert/remove flag if endkey so dictates.
         // returns -1 if active line index is -1.
         // returns -1 if unsuccessfully tried to append flag.
         // returns active line index if not necessary to
         //   append/insert/remove flag.
         // returns active line index if flag is successfully
         //   appended/inserted/removed.

         // 9/23/96 changed so that, if the active line index is -1
         //   (which can occur only if there are no lines), a new
         //   line with number 0 is added if something is entered
         //   at index 0.

static long append_flag_if_necessary(FieldGeometry *fg,
                           long index, long nread, char *endkey)
{
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1 && index == 0 && nread > 0)          // new 9/23/96
      {                                             // new 9/23/96
      fg->appendNewLine();                          // new 9/23/96
      ixl = fg->getActiveLineIndex();               // new 9/23/96
      if(ixl != -1) fg->setLineNumber(ixl, 0);      // new 9/23/96
      }                                             // new 9/23/96
  if(ixl == -1) return -1;
  long n = fg->numFlagsOnLine(ixl);
  if(nread == 0)
      {
      if(strings_equal(endkey, "INSERT"))
          {
          long ixf = fg->insertNewFlagOnLineFromBuffer(ixl, index);
          if(ixf == -1) return -1;
          }
      else if(index < n && strings_equal(endkey, "REMOVE"))
          {
          fg->deleteFlagFromLineToBuffer(ixl, index);
          }
      else if(strings_equal(endkey, "^Q"))
          {
          create_or_delete_test_inform(fg);
          }
      }
  else if(index == n)
      {
      long ixf = fg->appendNewFlagToLine(ixl);
      if(ixf == -1) return -1;
      }
  return ixl;
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


/*
static void dead_trap(void *data, long / *ident* /, long / *index* /,
                            char* / *value* /, long nread, char *endkey)
{
  if(nread != 0) return;
  if(strings_equal(endkey, "FIND") ||
     strings_equal(endkey, "^V")   ||
     strings_equal(endkey, "^M")   ||
     strings_equal(endkey, "^N"))
      {
      LdTableGui *table = (LdTableGui*)data;
      FieldGeometry *fg = table->getFieldGeometry();
      long ixl = fg->getActiveLineIndex();
      if(ixl == -1) return;
      long n = fg->numFlagsOnLine(ixl);
      if(n > 500) fg->preSlowOperations();
      }
   // this doesn't work because preSlowOperations does not have
   // a corresponding postSlowOperations.  as a result, the
   // watch cursor never comes on later.
}
*/


static void diamond_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  long ixl = append_flag_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
  if(ixl >= 0 && nread > 0)
      {
      fg->setActiveFlagIndexOnLine(ixl, index);
      }
  fg->postMultipleOperations();
}


static void select_trap(void *data, long /*ident*/, long index,
                            char* /*value*/, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  long ixl = append_flag_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
  if(ixl >= 0 && strings_equal(endkey, "RETURN"))
      {
      long n = fg->numFlagsOnLine(ixl);
      if(index < n) fg->incrementFlagSelectValue(ixl, index);
      }
  fg->postMultipleOperations();
}


static void table_trap(void *data, long ident, long index,
                            double value, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  long ixl = append_flag_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
  if(ixl >= 0 && nread > 0)
      {
      value = maybe_tack_on_digits(fg, (int)ident,
                                             ixl, index, value, nread);
      if(value == DNIL)
             fg->setDependentFlagValue(ixl, index, (int)ident, DZERO);
      else   fg->setFlagValue         (ixl, index, (int)ident, value);
      }
  fg->postMultipleOperations();
}


void LdTableGui::promptTrap(void *data, long ident, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  if(!strings_equal(endkey, "RETURN")) return;
  LdTableGui *table = (LdTableGui*)data;
        table->_sw[-ident - FIRST_FIELD_FLAG_VARIABLE]
    = - table->_sw[-ident - FIRST_FIELD_FLAG_VARIABLE]; 
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


static char *select_update(void *data, long /*ident*/, long index)
{
  static char buffer[2];
  buffer[0] = ' ';
  buffer[1] = '\0';
  LdTableGui    *table = (LdTableGui*)data;
  LdTopGui      *top   = table->getLdTopGui();
  FieldGeometry *fg    = table->getFieldGeometry();
  long ixl             = fg->getActiveLineIndex();
  if(ixl >= 0) buffer[0] = fg->getFlagSelectValue(ixl, index);
  return buffer;
}


static long gp_update(void *data, long ident, long index)
{
  LdTableGui    *table = (LdTableGui*)data;
  LdTopGui      *top   = table->getLdTopGui();
  FieldGeometry *fg    = table->getFieldGeometry();
  long ixl             = fg->getActiveLineIndex();
  if(ixl == -1) return INIL;
  if(!top->showDependentValues()) return INIL;
  switch(ident)
      {
      case LD_GP1: return fg->getCumulativeGroundPosition(ixl, index);
      case LD_GP2: return fg->getMatchableGroundPosition (ixl, index);
      case LD_GP3: return fg->getGroundPosition          (ixl, index);
      default    : assert(FALSE);
      }
  assert(FALSE);
  return INIL;
}


static double table_update(void *data, long ident, long index)
{
  LdTableGui    *table = (LdTableGui*)data;
  LdTopGui      *top   = table->getLdTopGui();
  FieldGeometry *fg    = table->getFieldGeometry();
  long ixl             = fg->getActiveLineIndex();
  if(ixl == -1) return DNIL;
  if(top->showDependentValues()
            || ident == FG_SHOT
            || !fg->flagValueIsDependent(ixl, index, (int)ident))
       {
       return fg->getFlagValue(ixl, index, (int)ident);
       }
  return DNIL;
}


static long n_update(void *data)
{
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long ixl             = fg->getActiveLineIndex();
  if(ixl == -1) return 0;
  return fg->numFlagsOnLine(ixl);
}


static long nmax_update(void *data)
{
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long ixl             = fg->getActiveLineIndex();
//if(ixl == -1) return 0;                         // removed 9/23/96
  if(ixl == -1) return 1;                         //     new 9/23/96
  return fg->numFlagsOnLine(ixl) + 1;
}



static long diamond_update(void *data, long /*ident*/, long index)
{
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long ixl             = fg->getActiveLineIndex();
  if(ixl == -1) return FALSE;
  long ixf             = fg->getActiveFlagIndexOnLine(ixl);
  return (index == ixf);
}



static char *source_update(void *data, long /*ident*/, long index)
{
  static char buffer[2];
  buffer[0] = ' ';
  buffer[1] = '\0';
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  if(fg->sourceGathersOutOfDate())
      {
      buffer[0] = '?';
      return buffer;
      }
  long ixl             = fg->getActiveLineIndex();
  if(ixl == -1) return buffer;
  long n = fg->numSourcesAtFlag(ixl, index);
  if(n <= 0) return buffer;
  if(n <= 9) sprintf(buffer, "%d", n);
  else       buffer[0] = '*';
  return buffer;
}



static char *receiver_update(void *data, long /*ident*/, long index)
{
  static char buffer[2];
  buffer[0] = ' ';
  buffer[1] = '\0';
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  if(fg->receiverGathersOutOfDate())
      {
      buffer[0] = '?';
      return buffer;
      }
  long ixl             = fg->getActiveLineIndex();
  if(ixl == -1) return buffer;
  if(fg->flagHasReceiver(ixl, index)) buffer[0] = 'R';
  return buffer;
}



static char *dead_update(void *data, long ident, long index)
{
  static char buffer[3];
  buffer[0] = '?';
  buffer[1] = '\0';
  buffer[2] = '\0';
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long ixl             = fg->getActiveLineIndex();
  if(ixl == -1) return buffer;
  int dead_code, maybe;
  if(ident == LD_SDEAD)
      {
      dead_code = fg->getDeadSourceCode  (ixl, index);
      maybe     = fg->sourceMaybeDead    (ixl, index);
      }
  else                 
      {
      dead_code = fg->getDeadReceiverCode(ixl, index);
      maybe     = fg->receiverMaybeDead  (ixl, index);
      }
  switch(dead_code)
      {
      case ZT_CODE_NONE: buffer[0] = '?'; break;
      case ZT_CODE_LIVE: buffer[0] = ' '; break;
      case ZT_CODE_ZERO: buffer[0] = 'D'; break;
      case ZT_CODE_MISS: buffer[0] = 'M'; break;
      case ZT_CODE_REV : buffer[0] = 'R'; break;
      default: assert(FALSE);
      }
  if(maybe) buffer[1] = '*';
  else      buffer[1] = ' ';
  return buffer;
}



//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//

//  switch update functions might be called even when index >= n:

long LdTableGui::tableSwitchUpdate(void *data, long ident, long index)
{
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long           ixl   = fg->getActiveLineIndex();
  long           sw    = table->_sw[ident - FIRST_FIELD_FLAG_VARIABLE];
  if(ident == FG_CUM   || ident == FG_AZIM  ||
     ident == LD_GP1   || ident == LD_GP2   || ident == LD_GP3 ||
     ident == LD_ERROR || ident == LD_UNIFORM)
      {
      if(ixl == -1) return -5;
      long n = fg->numFlagsOnLine(ixl);
      if(index >= n) return -5;
      return 5 * sw;
      }
  if(ident == FG_SHOT && ixl == -1 && index == 0)    // new 9/23/96
      {                                              // new 9/23/96
      return sw;                                     // new 9/23/96
      }                                              // new 9/23/96
  if(ixl == -1) return -1;
  if(!fg->allowSettingValue((int)ident)) sw = -1;
  long           n     = fg->numFlagsOnLine(ixl);
  if(index >= n) return sw;
  if(fg->flagValueIsDependent(ixl, index, (int)ident)) return 22 * sw;
  return sw;
}



long LdTableGui::diamondSwitchUpdate(void *data, long ident, long index)
{
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long           ixl   = fg->getActiveLineIndex();
  if(ixl == -1) return -4;
  long           n     = fg->numFlagsOnLine(ixl);
  if(index >= n) return -4;
  return 4 * table->_sw[ident - FIRST_FIELD_FLAG_VARIABLE];
}



long LdTableGui::selectSwitchUpdate(void *data, long ident, long index)
{
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long           ixl   = fg->getActiveLineIndex();
  if(ixl == -1) return -2;
  long           n     = fg->numFlagsOnLine(ixl);
  if(index >= n) return -2;
  long           sw    = table->_sw[ident - FIRST_FIELD_FLAG_VARIABLE];
  if(fg->flagIsSelected(ixl, index)) return 6 * sw;
  return 2 * sw;
}



long LdTableGui::srSwitchUpdate(void *data, long ident, long /*index*/)
{
  LdTableGui    *table = (LdTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long           ixl   = fg->getActiveLineIndex();
  if(ixl == -1) return -5;
  return 5 * table->_sw[ident - FIRST_FIELD_FLAG_VARIABLE];
}



long prompt_switch_update(void *data, long ident, long /*index*/)
{
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  if(fg->allowSettingValue(-(int)ident)) return 2;
  return -5;
}



//-------------- add qc choices traps and update -------------//
//-------------- add qc choices traps and update -------------//
//-------------- add qc choices traps and update -------------//


static long fixdist_switch_upfun(void *data, long /*ident*/, long /*index*/)
{
  LdTableGui *table = (LdTableGui*)data;
  if(table->getUseWhich() == USE_FIXDIST) return 5;
  return -5;
}


static long average_switch_upfun(void *data, long /*ident*/, long /*index*/)
{
  LdTableGui *table = (LdTableGui*)data;
  if(table->getUseWhich() == USE_AVERAGE) return 5;
  return -5;
}


static long this_switch_upfun(void *data, long /*ident*/, long /*index*/)
{
  LdTableGui *table = (LdTableGui*)data;
  if(table->getUseWhich() == USE_THIS) return 1;
  return -22;
}


static float fixdist_upfun(void *data, long /*ident*/, long /*index*/)
{
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  return fg->getFixdist();
}


static float average_upfun(void *data, long /*ident*/, long /*index*/)
{
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return 0.0;
  long n = fg->numFlagsOnLine(ixl);
  if(n <= 1) return 0.0;
  return ( (fg->getFlagValue(ixl, n-1, FG_CUM) -
            fg->getFlagValue(ixl,   0, FG_CUM)) / (n-1) );
}


static void this_trap(void *data, long /*ident*/, long /*index*/,
                        float newvar, long nread, char* /*endkey*/)
{
  LdTableGui *table = (LdTableGui*)data;
  if(nread > 0) table->setUseThis(newvar);
}


static float this_upfun(void *data, long /*ident*/, long /*index*/)
{
  LdTableGui *table = (LdTableGui*)data;
  return table->getUseThis();
}


static void use_trap(void *data, long ident, long /*index*/,
                          long /*ivar*/, long nread, char* /*endkey*/)
{
  LdTableGui *table = (LdTableGui*)data;
  if(nread > 0) table->setUseWhich(ident);
}

static long use_upfun(void *data, long ident, long /*index*/)
{
  LdTableGui *table = (LdTableGui*)data;
  return (ident == table->getUseWhich());
}



//---------------- uniform and error update ----------------//
//---------------- uniform and error update ----------------//
//---------------- uniform and error update ----------------//

static float inc_dist(void *data, long index)
{
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return 0.0;
  long n = fg->numFlagsOnLine(ixl);
  if(n <= 1) return 0.0;
  return AbsoluteValue(fg->getFlagValue(ixl, index, FG_DIST));
}


static float cum_update(void *data, long /*ident*/, long index)
{
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return 0.0;
  long n = fg->numFlagsOnLine(ixl);
  if(n <= 1) return 0.0;
  float cum     = fg->getFlagValue(ixl, index, FG_CUM);
  float fixdist = fg->getFixdist();
  if(fixdist != 0.0)
      {
      long fmgp = fg->firstMatchableGroundPosition(ixl);
      if(fmgp != INIL) cum += AbsoluteValue(fixdist) * (fmgp - 1);
      }
  return cum;
}


static float azimuth_update(void *data, long /*ident*/, long index)
{
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return 0.0;
  long n = fg->numFlagsOnLine(ixl);
  if(n <= 1) return 0.0;
  return fg->getAzimuth(ixl, index);
}


static float azimuth_error(void *data, long index)
{
  if(index == 0) return 0.0;
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return 0.0;
  long n = fg->numFlagsOnLine(ixl);
  if(n <= 1) return 0.0;
  float azim1 = azimuth_update(data, 0, index);
  float azim2 = azimuth_update(data, 0, index-1);
  float azimuth_error  = AbsoluteValue(azim1 - azim2);
  float azimuth_error2 = AbsoluteValue(azim1 - azim2 + 360);
  float azimuth_error3 = AbsoluteValue(azim1 - azim2 - 360);
  azimuth_error = MinimumValue(azimuth_error, azimuth_error2);
  azimuth_error = MinimumValue(azimuth_error, azimuth_error3);
  return azimuth_error;
}


static float increment(void *data)
{
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  switch(table->getUseWhich())
      {
      case USE_FIXDIST: return fg->getFixdist();
      case USE_AVERAGE: return average_upfun(data, 0, 0);
      case USE_THIS   : return table->getUseThis();
      default: break;
      }
  return 0.0;   //  should not get to here.
}


float uniform_update(void *data, long /*ident*/, long index)
{
  LdTableGui *table = (LdTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return 0.0;
  long n = fg->numFlagsOnLine(ixl);
  if(n <= 1) return 0.0;
  float cum     = increment(data) * index;
  float fixdist = fg->getFixdist();
  if(fixdist != 0.0)
      {
      long fmgp = fg->firstMatchableGroundPosition(ixl);
      if(fmgp != INIL) cum += AbsoluteValue(fixdist) * (fmgp - 1);
      }
  return cum;
}



char *error_update(void *data, long /*ident*/, long index)
{
  static char buffer[10];
  strcpy(buffer, "     ");
  if(index == 0) return buffer;
  LdTableGui *table = (LdTableGui*)data;
  float inc        = increment(data);
  float inc_error  = inc_dist(data, index) - inc;
  float cum_error  = cum_update    (data, 0, index) -
                     uniform_update(data, 0, index);
  float azim_error = azimuth_error(data, index);
  float inc_factor = 0.01 * table->getIncPercent();
  float cum_factor = 0.01 * table->getCumPercent();
  inc_error = AbsoluteValue(inc_error);
  cum_error = AbsoluteValue(cum_error);
  if(inc_error  > inc_factor * inc)       buffer[0] = 'I';
  if(cum_error  > cum_factor * inc)       buffer[2] = 'C';
  if(azim_error > table->getAzimChange()) buffer[4] = 'A';
  return buffer;
}



//--------------------- add qc choices ---------------------//
//--------------------- add qc choices ---------------------//
//--------------------- add qc choices ---------------------//

int LdTableGui::addQcChoices()
{
  static long zero =  0;
  static long one  =  1;
  static long m5   = -5;
  static long four =  4;
  static int cola = 20;
  static int colb = 14;

  dbox_ireg3(USE_FIXDIST, "use  FIXDIST  for distance error checking:  ",
                  &zero, &four, 1, cola, 2, 0);
  dbox_set_itrap(USE_FIXDIST, use_trap);
  dbox_set_ifun (USE_FIXDIST, use_upfun);
  dbox_freg(111, &m5, -1, -1, 6, 2);
  dbox_set_ffun (111, fixdist_upfun);
  dbox_set_sfun (111, fixdist_switch_upfun);

  dbox_ireg3(USE_AVERAGE, "use average horizontal incremental distance:",
                  &zero, &four, 2, cola, 2, 0);
  dbox_set_itrap(USE_AVERAGE, use_trap);
  dbox_set_ifun (USE_AVERAGE, use_upfun);
  dbox_freg(222, &m5, -1, -1, 6, 2);
  dbox_set_ffun (222, average_upfun);
  dbox_set_sfun (222, average_switch_upfun);

  dbox_ireg3(USE_THIS   , "use this value for distance error checking: ",
                  &zero, &four, 3, cola, 2, 0);
  dbox_set_itrap(USE_THIS, use_trap);
  dbox_set_ifun (USE_THIS, use_upfun);
//wbox_freg(NULL, 333, &_use_this, &one, -1, -1, 6, 2);
  dbox_freg(333, &m5, -1, -1, 6, 2);
  dbox_set_ftrap(333, this_trap);
  dbox_set_ffun (333, this_upfun);
  dbox_set_sfun (333, this_switch_upfun);
  
  wbox_freg2(NULL, 444, "Flag incremental distance errors exceeding",
                &zero, &_inc_percent, &one, 4, colb, 3, 0);
  wbox_creg(NULL, 4441, "% of above distance.", &zero, -1, -1, 0, 0);

  wbox_freg2(NULL, 555, "Flag  cumulative distance errors exceeding",
                &zero, &_cum_percent, &one, 5, colb, 3, 0);
  wbox_creg(NULL, 5551, "% of above distance.", &zero, -1, -1, 0, 0);

  wbox_freg2(NULL, 666, "Flag  changes in  azimuth angle  exceeding",
                &zero, &_azim_change, &one, 6, colb, 3, 0);
  wbox_creg(NULL, 6661, "degrees.", &zero, -1, -1, 0, 0);
  wbox_mreg("<<<<<<<<<<<<< QC >>>>>>>>>>>>>", 0, 59);
  return 8;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//


void LdTableGui::makeHelper()
{
  int rowstart = 1;

  if(_table_option == 3)
      {
      rowstart = addQcChoices();
      }

  static long two  =  2; 
  static long zero =  0; 
  static long junk =  0; 
  static long five =  5; 

         //  ID          SWITCH   ROW      COL NCHAR NDEC
  dbox_creg(-LD_DIAMOND,  &two, rowstart,   6,   6,   0);
  dbox_creg(-FG_SEL    ,  &two, rowstart,  16,   6,   0);

  void *box = getBox();
  wbox_cnewreg(box, -LD_DIAMOND, "active");
  wbox_cnewreg(box, -FG_SEL    , "select");

  dbox_set_ctrap(-LD_DIAMOND, promptTrap);
  dbox_set_ctrap(-FG_SEL    , promptTrap);

    //       PROMPT                ROW     COL
  wbox_mreg("INCR"             , rowstart, 26);
  wbox_mreg("--COORDINATES--"  ,   -1,     35);

  if(_table_option == 1)         // standard LD information
      {
      wbox_mreg("----GRID-----"    , -1,  60);
      wbox_mreg("--HOLE--"         , -1,  77);
      wbox_mreg("--RECEIVER SKID--", -1,  88);
      wbox_mreg("dead"             , -1, 107);
      }
  else if(_table_option == 2)    // additional LD information
      {
///// wbox_mreg("        "             , -1, 59);   // LD_GP3.
      wbox_mreg("---ground position---", -1, 59);   // LD_GP1 and LD_GP2.
      wbox_mreg("  STATIC"             , -1, -1);
      }
  else if(_table_option == 3)    // quality control LD information
      {
 ///  wbox_mreg("error  CUMULATIVE DISTANCE", -1, 58);
      wbox_mreg("error   CUMULATIVE DIST", -1, 59);
      wbox_mreg("ground"                 , -1, 91);   // LD_GP3.
      wbox_mreg(" STATIC"                , -1, 101);
      }
  else
      {
      assert(FALSE);
      }

         //    N       NMAX       ROW COL NCHAR MAXROWS
  dbox_rega(n_update, nmax_update, 0,  0,   6,    35);

         //  ID          PROMPT     SWITCH   SWITCH  COL NCHAR NDEC
  dbox_irega(LD_DIAMOND, "| "      , &zero,  &junk,   0,   2,   0);
  dbox_drega(FG_SHOT   , "  SP#  " , &two ,  &junk,   0,   7,   3);
  dbox_crega(FG_SEL    , "|"       , &zero,  &junk,   0,   1,   0);
  dbox_crega(LD_S      , "S"       , &two ,  &junk,   0,   1,   0);
  dbox_crega(LD_R      , "R"       , &two ,  &junk,   0,   1,   0);
  dbox_drega(FG_DIST   , " DIST   ", &junk,  &junk,   0, XYCHAR, XYDEC);
  dbox_drega(FG_XLOC   , " XLOC   ", &junk,  &junk,   0, XYCHAR, XYDEC);
  dbox_drega(FG_YLOC   , " YLOC   ", &two ,  &junk,   0, XYCHAR, XYDEC);
  dbox_drega(FG_ELEV   , "ELEV "   , &two ,  &junk,   0,   5,   0);
  if(_table_option == 1)
      {
      dbox_drega(FG_XGRID  , " XGRID " , &junk,  &junk,  59,   7,   2);
      dbox_drega(FG_YGRID  , " YGRID " , &junk,  &junk,   0,   7,   2);
      dbox_drega(FG_HD     , " HD "    , &two ,  &junk,  76,   4,   0);
      dbox_drega(FG_TUH    , " TUH "   , &two ,  &junk,   0,   5,   3);
      dbox_drega(FG_XSKID  , " XSD "   , &two ,  &junk,  88,   5,   0);
      dbox_drega(FG_YSKID  , " YSD "   , &two ,  &junk,   0,   5,   0);
      dbox_drega(FG_ESKID  , "ELSD "   , &two ,  &junk,   0,   5,   0);
      dbox_crega(LD_SDEAD  , "S "      , &two ,  &junk, 107,   2,   0);
      dbox_crega(LD_RDEAD  , "R "      , &two ,  &junk,   0,   2,   0);
      }
  else if(_table_option == 2)
      {
///   dbox_irega(LD_GP1    , "cumulative", &two ,  &junk,  59,  10,   0);
///   dbox_irega(LD_GP2    , "matchable ", &two ,  &junk,   0,  10,   0);
      dbox_irega(LD_GP1    , "fixdist<=0", &two ,  &junk,  59,  10,   0);
      dbox_irega(LD_GP2    , "fixdist>0 ", &two ,  &junk,   0,  10,   0);
///// dbox_irega(LD_GP3    , "grnd pos"  , &two ,  &junk,  59,   8,   0);
///// dbox_drega(FG_RSTAT  , " TR "      , &two ,  &junk,  69,   4,   3);
      dbox_drega(FG_RSTAT  , " TR "      , &two ,  &junk,  82,   4,   3);
      dbox_drega(FG_SSTAT  , " TS "      , &two ,  &junk,   0,   4,   3);
      }
  else if(_table_option == 3)
      {
      dbox_crega(LD_ERROR  , "flags"     , &zero,  &five,  59,   5,   1);
      dbox_frega(FG_CUM    , " actual "  , &zero,  &five,  66,   8,   1);
      dbox_frega(LD_UNIFORM, "uniform "  , &zero,  &five,  75,   8,   1);
      dbox_frega(FG_AZIM   , "AZIM"      , &zero,  &five,  85,   4,   0);
      dbox_irega(LD_GP3    , "position"  , &zero,  &five,  91,   8,   0);
      dbox_drega(FG_RSTAT  , " TR "      , &two ,  &junk, 101,   4,   3);
      dbox_drega(FG_SSTAT  , " TS "      , &two ,  &junk,   0,   4,   3);
      }
  else
      {
      assert(FALSE);
      }

  for(int i = 0; i < NCOLUMNS; i++)
       {
       int ident = (int)FIRST_FIELD_FLAG_VARIABLE + i;
       if(ident == LD_S)
           {
           dbox_set_ctrap(-ident, promptTrap);
           dbox_set_cfun ( ident, source_update);
           dbox_set_sfun ( ident, srSwitchUpdate);
           }
       else if(ident == LD_R)
           {
           dbox_set_ctrap(-ident, promptTrap);
           dbox_set_cfun ( ident, receiver_update);
           dbox_set_sfun ( ident, srSwitchUpdate);
           }
       else if(ident == LD_DIAMOND)
           {
           dbox_set_ctrap(-ident, promptTrap);
           dbox_set_itrap( ident, diamond_trap);
           dbox_set_ifun ( ident, diamond_update);
           dbox_set_sfun ( ident, diamondSwitchUpdate);
           }
       else if(ident == FG_SEL)
           {
           dbox_set_ctrap(-ident, promptTrap);
           dbox_set_ctrap( ident, select_trap);
           dbox_set_cfun ( ident, select_update);
           dbox_set_sfun ( ident, selectSwitchUpdate);
           }
       else if(ident == LD_GP1 || ident == LD_GP2 || ident == LD_GP3)
           {
           dbox_set_ifun ( ident, gp_update);
           }
       else if(ident == LD_UNIFORM)
           {
           dbox_set_ffun ( ident, uniform_update);
           }
       else if(ident == LD_ERROR)
           {
           dbox_set_cfun ( ident, error_update);
           }
       else if(ident == FG_CUM)
           {
           dbox_set_ffun ( ident, cum_update);
           }
       else if(ident == FG_AZIM)
           {
           dbox_set_ffun ( ident, azimuth_update);
           }
       else if(ident == LD_SDEAD || ident == LD_RDEAD)
           {
           dbox_set_ctrap(-ident, promptTrap);
           dbox_set_cfun ( ident, dead_update);
           dbox_set_sfun ( ident, srSwitchUpdate);
           }
       else
           {
           dbox_set_ctrap(-ident, promptTrap);
           dbox_set_dtrap( ident, table_trap);
           dbox_set_dfun ( ident, table_update);
           dbox_set_sfun ( ident, tableSwitchUpdate);
           dbox_set_sfun (-ident, prompt_switch_update);
           }
       }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
