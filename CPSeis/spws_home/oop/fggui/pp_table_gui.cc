
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
//---------------------- pp_table_gui.cc -----------------------//
//---------------------- pp_table_gui.cc -----------------------//
//---------------------- pp_table_gui.cc -----------------------//

//          implementation file for the PpTableGui class
//                derived from the SLDatabox class
//              also derived from the FgInform class
//                       subdirectory fggui


#include "fggui/pp_table_gui.hh"
#include "fggui/pp_top_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_prim.hh"
#include "wbox.h"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


static const double FZERO = 0.0;

enum { DIAMOND = LAST_PP_VARIABLE  + 1 };

//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


PpTableGui::PpTableGui(SLDelay *slparent, char *name, FieldGeometry *fg,
                                                      PpTopGui *top)
           : SLDatabox(slparent, name, NULL, 4),
             FgInform(fg),
                 _top        (top)
{
  assert(fg);
  assert(NCOLUMNS == NUM_PP_VARIABLES + 1);
  for(long i = 0; i < NCOLUMNS; i++) { _sw[i] = -1; }
  _sw[PP_SSHOT   - FIRST_PP_VARIABLE] = 1;
  _sw[PP_RSHOT   - FIRST_PP_VARIABLE] = 1;
  _sw[PP_PAT     - FIRST_PP_VARIABLE] = 1;
  _sw[PP_THRU_GR - FIRST_PP_VARIABLE] = 1;
  _sw[PP_SMOVE   - FIRST_PP_VARIABLE] = 1;
  _sw[PP_RMOVE   - FIRST_PP_VARIABLE] = 1;
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

PpTableGui::~PpTableGui()
{
}



//-------------------- post new active pp card -------------------//
//-------------------- post new active pp card -------------------//
//-------------------- post new active pp card -------------------//

       // virtual function overriding FgInform.

void PpTableGui::postNewActivePpCard(FieldGeometry *fg)
{
  void *box = getBox();
  long ixpp = fg->getActivePpCardIndex();
  if(ixpp >= 0) wbox_set_focus(box, DIAMOND, (int)ixpp+1);
  _sw[DIAMOND - FIRST_PP_VARIABLE] = 1;
}



//------------------ get switch --------------------------------//
//------------------ get switch --------------------------------//
//------------------ get switch --------------------------------//

      // public.

long PpTableGui::getSwitch(int i)  const
{
  assert(i >= 0 && i < NCOLUMNS);
  return _sw[i];
}



//-------------- maybe add new card with defaults ----------------//
//-------------- maybe add new card with defaults ----------------//
//-------------- maybe add new card with defaults ----------------//

void maybe_add_new_card_with_defaults(long ident, long index,
                 char *endkey, FieldGeometry *fg, PpTableGui *table)
{
  long n = fg->numPpCards();
  if(index != n - 1) return;
  switch(ident)
      {
      case DIAMOND: return;
      case PP_SSHOT:
      case PP_RSHOT:
            if(strings_equal(endkey, "DOWN")) break;
      default:
            if(!strings_equal(endkey, "RIGHT")) return;
            {
            long istart = ident      - FIRST_PP_VARIABLE + 1;
            long istop  = PP_THRU_TR - FIRST_PP_VARIABLE;
            for(long i = istart; i <= istop; i++)
                {
                long sw = table->getSwitch((int)i);
                if(sw > 0) return;
                }
            }
            break;
      }
  long  sou_line      = fg->getSourceLine          (index);
  long  rec_line      = fg->getReceiverLine        (index);
  long  ixl_sou       = fg->findMatchingLineNumber (sou_line);
  long  ixl_rec       = fg->findMatchingLineNumber (rec_line);
  if(ixl_sou == -1 || ixl_rec == -1) return;
  float sou_shotpoint = fg->getSourceShotpoint     (index);
  float rec_shotpoint = fg->getReceiverShotpoint   (index);
  long  ixf_sou = fg->findMatchingShotpointOnLine  (ixl_sou, sou_shotpoint);
  long  ixf_rec = fg->findMatchingShotpointOnLine  (ixl_rec, rec_shotpoint);
  if(ixf_sou == -1 || ixf_rec == -1) return;
  long  sou_move      = fg->getSourceMove          (index);
  long  rec_move      = fg->getReceiverMove        (index);
  long  ngroups       = fg->getNumGroupsOnCard     (index);
  ixf_sou += sou_move * ngroups;
  ixf_rec += rec_move * ngroups;
  long  nflags_sou = fg->numFlagsOnLine            (ixl_sou);
  long  nflags_rec = fg->numFlagsOnLine            (ixl_rec);
  if(ixf_sou < 0 || ixf_sou >= nflags_sou) return;
  if(ixf_rec < 0 || ixf_rec >= nflags_rec) return;
  long ixpp = fg->appendNewPpCard();
  if(ixpp == -1) return;
  sou_shotpoint = fg->getShotpoint(ixl_sou, ixf_sou);
  rec_shotpoint = fg->getShotpoint(ixl_rec, ixf_rec);
  fg->setSourceShotpoint  (ixpp, sou_shotpoint);
  fg->setReceiverShotpoint(ixpp, rec_shotpoint);
}



//----------------- append card if necessary ---------------------//
//----------------- append card if necessary ---------------------//
//----------------- append card if necessary ---------------------//

     // to be called from a trap.
     // tries to append a PP card if index == n.
     // tries to insert/remove PP card if endkey so dictates.
     // returns -1 if unsuccessfully tried to append PP card.
     // returns 0 if not necessary to append/insert/remove PP card.
     // returns 0 if PP card is successfully appended/inserted/removed.

static int append_card_if_necessary(FieldGeometry *fg,
                           long index, long nread, char *endkey)
{
  long n = fg->numPpCards();
  if(nread == 0)
      {
      if(strings_equal(endkey, "INSERT"))
          {
          long ixpp = fg->insertNewPpCardFromBuffer(index);
          if(ixpp == -1) return -1;
          }
      else if(index < n && strings_equal(endkey, "REMOVE"))
          {
          fg->deletePpCardToBuffer(index);
          }
      }
  else if(index == n)
      {
      long ixpp = fg->appendNewPpCard();
      if(ixpp == -1) return -1;
      }
  return 0;
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void diamond_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  PpTableGui    *table = (PpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  int error = append_card_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
  long n = fg->numPpCards();
  if(error || nread == 0 || index >= n)
      {
      fg->postMultipleOperations();
      return;
      }
/*
  fg->setActivePpCardIndex(index);
*/
  long trace = fg->getFirstTraceNumber(index);
  fg->setActiveSourceIndices(trace);
  fg->postMultipleOperations();
}



static void table_trap(void *data, long ident, long index,
                            double value, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  PpTableGui *table = (PpTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  int error = append_card_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
  if(error || nread == 0)
      {
      if(!error)
           maybe_add_new_card_with_defaults(ident, index, endkey, fg, table);
      fg->postMultipleOperations();
      return;
      }
  if(ident == PP_THRU_GR)
      {
      if(value == FNIL) value = fg->getPpValue(index, PP_THRU_GR);
      long n = fg->numPpCards();
/*
      float last_value = 0.0;
*/
      double last_value = 0.0;
      if(index >   0) last_value = fg->getPpValue(index-1, PP_THRU_GR);
/*
      float next_value = value + 1.0;
*/
      double next_value = value + 1.0;
      if(index < n-1) next_value = fg->getPpValue(index+1, PP_THRU_GR);
      if(value <= last_value || value >= next_value)
          {
          fg->showMessage("THRU GROUP# outside of valid range");
          fg->ringBell();
          fg->postMultipleOperations();
          return;
          }
/*
      float incr = value - fg->getPpValue(index, PP_THRU_GR);
      float group = fg->getPpValue(index, PP_NGROUPS);
*/
      double incr = value - fg->getPpValue(index, PP_THRU_GR);
      double group = fg->getPpValue(index, PP_NGROUPS);
      fg->setPpValue(index, PP_NGROUPS, group + incr);
      if(index < n-1)
          {
/*
          float next_group = fg->getPpValue(index+1, PP_NGROUPS);
*/
          double next_group = fg->getPpValue(index+1, PP_NGROUPS);
          fg->setPpValue(index+1, PP_NGROUPS, next_group - incr);
          }
      maybe_add_new_card_with_defaults(ident, index, endkey, fg, table);
      fg->postMultipleOperations();
      return;
      }
  if(value == FNIL) fg->setDependentPpValue(index, (int)ident, FZERO);
  else                fg->setPpValue         (index, (int)ident, value);
  if(ident == PP_SSHOT && index >= 1
           && !fg->ppValueIsDependent(index  , PP_SSHOT)
           &&  fg->ppValueIsDependent(index  , PP_RSHOT)
           && !fg->ppValueIsDependent(index-1, PP_SSHOT)
           && !fg->ppValueIsDependent(index-1, PP_RSHOT))
      {
/*
      float sshot_prev = fg->getPpValue(index-1, PP_SSHOT);
      float rshot_prev = fg->getPpValue(index-1, PP_RSHOT);
      float sshot      = fg->getPpValue(index  , PP_SSHOT);
      float rshot      = sshot + rshot_prev - sshot_prev;
*/
      double sshot_prev = fg->getPpValue(index-1, PP_SSHOT);
      double rshot_prev = fg->getPpValue(index-1, PP_RSHOT);
      double sshot      = fg->getPpValue(index  , PP_SSHOT);
      double rshot      = sshot + rshot_prev - sshot_prev;
      fg->setPpValue(index, PP_RSHOT, rshot);
      }
  maybe_add_new_card_with_defaults(ident, index, endkey, fg, table);
  fg->postMultipleOperations();
}


void PpTableGui::promptTrap(void *data, long ident, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  if(!strings_equal(endkey, "RETURN")) return;
  PpTableGui *table = (PpTableGui*)data;
        table->_sw[-ident - FIRST_PP_VARIABLE]
    = - table->_sw[-ident - FIRST_PP_VARIABLE]; 
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


static long diamond_update(void *data, long /*ident*/, long index)
{
  PpTableGui    *table = (PpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long active_index = fg->getActivePpCardIndex();
  return (active_index == index);
}



static double table_update(void *data, long ident, long index)
{
  PpTableGui    *table = (PpTableGui*)data;
  PpTopGui      *top   = table->getPpTopGui();
  FieldGeometry *fg    = table->getFieldGeometry();
  if(top->showDependentValues()
            || ident == PP_THRU_GR
            || !fg->ppValueIsDependent(index, (int)ident))
       {
       return fg->getPpValue(index, (int)ident);
       }
  return FNIL;
}


static long n_update(void *data)
{
  PpTableGui    *table = (PpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  return fg->numPpCards();
}


static long nmax_update(void *data)
{
  return n_update(data) + 1;
}



//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//

//  switch update functions might be called even when index >= n:

long PpTableGui::tableSwitchUpdate(void *data, long ident, long index)
{
  PpTableGui    *table = (PpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long           sw    = table->_sw[ident - FIRST_PP_VARIABLE];
  if(ident == PP_THRU_TR || ident == PP_NTRACES) return 5 * sw;
  if(ident == PP_THRU_GR) return sw;
  long           n     = fg->numPpCards();
  if(index >= n) return sw;
  if(fg->ppValueIsDependent(index, (int)ident)) return 22 * sw;
  return sw;
}


long PpTableGui::diamondSwitchUpdate(void *data, long ident, long index)
{
  PpTableGui    *table = (PpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long           n     = fg->numPpCards();
  if(index >= n) return -77;
  return 4 * table->_sw[ident - FIRST_RP_VARIABLE];
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void PpTableGui::makeHelper()
{
  static long two  = 2; 
  static long four = 4; 
  static long junk = 0; 

    //       PROMPT                      ROW COL
  wbox_mreg("first "                   ,  0, 11);
  wbox_mreg("---SOURCE-----"           , -1, -1);
  wbox_mreg("--RECEIVER----"           , -1, -1);
  wbox_mreg("       --SOURCE-SKID---"  , -1, -1);
  wbox_mreg("------NEW-------"         , -1, -1);
  wbox_mreg("--MOVE---"                , -1, -1);
  wbox_mreg("---number-of---"          , -1, -1);
  wbox_mreg(" THRU    THRU  "          , -1, -1);

         //    N       NMAX       ROW COL NCHAR MAXROWS
  dbox_rega(n_update, nmax_update, 0,  0,   6,    35);

         //  ID          PROMPT     SWITCH   SWITCH  COL NCHAR NDEC
  dbox_irega(DIAMOND   , "  "      , &two ,  &four,   0,   2,   0);
  dbox_drega(PP_FILE   , "file# "  , &two ,  &junk,   0,   6,   0);
  dbox_drega(PP_SSHOT  , "  SP#  " , &two ,  &junk,   0,   7,   3);
  dbox_drega(PP_SLINE  , "line# "  , &two ,  &junk,   0,   6,   0);
  dbox_drega(PP_RSHOT  , "  SP#  " , &two ,  &junk,   0,   7,   3);
  dbox_drega(PP_RLINE  , "line# "  , &two ,  &junk,   0,   6,   0);
  dbox_drega(PP_PAT    , "pat#"    , &two ,  &junk,   0,   6,   0);
  dbox_drega(PP_XSKID  , "XSKID"   , &two ,  &junk,   0,   5,   0);
  dbox_drega(PP_YSKID  , "YSKID"   , &two ,  &junk,   0,   5,   0);
  dbox_drega(PP_HOLD   , "HOLD"    , &two ,  &junk,   0,   4,   0);
  dbox_drega(PP_ELEV   , "ELEV "   , &two ,  &junk,   0,   5,   0);
  dbox_drega(PP_HD     , " HD "    , &two ,  &junk,   0,   4,   0);
  dbox_drega(PP_TUH    , " TUH "   , &two ,  &junk,   0,   5,   3);
  dbox_drega(PP_SMOVE  , "SRC "    , &two ,  &junk,   0,   4,   0);
  dbox_drega(PP_RMOVE  , "REC "    , &two ,  &junk,   0,   4,   0);
  dbox_drega(PP_NGROUPS, "groups"  , &two ,  &junk,   0,   6,   0);
  dbox_drega(PP_NTRACES, " traces ", &two ,  &junk,   0,   8,   0);
  dbox_drega(PP_THRU_GR, "group#"  , &two ,  &junk,   0,   6,   0);
  dbox_drega(PP_THRU_TR, " trace# ", &two ,  &junk,   0,   8,   0);

  for(int i = 0; i < NUM_PP_VARIABLES; i++)
       {
       int ident = (int)FIRST_PP_VARIABLE + i;
       dbox_set_ctrap(-ident, promptTrap);
       dbox_set_dtrap( ident, table_trap);
       dbox_set_dfun ( ident, table_update);
       dbox_set_sfun ( ident, tableSwitchUpdate);
       }
  dbox_set_itrap( DIAMOND, diamond_trap);
  dbox_set_ifun ( DIAMOND, diamond_update);
  dbox_set_sfun ( DIAMOND, diamondSwitchUpdate);
  dbox_set_ctrap(-DIAMOND, promptTrap);
}




//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
