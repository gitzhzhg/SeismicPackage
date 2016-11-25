
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
//---------------------- fg_clear_pop.cc ------------------------------//
//---------------------- fg_clear_pop.cc ------------------------------//
//---------------------- fg_clear_pop.cc ------------------------------//

//           implementation file for the FgClearPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to clear field geometry data.


#include "fggui/fg_clear_pop.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_toggle.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>


enum { TOG_LD = 0, TOG_RP, TOG_PP,
       TOG_ZT1, TOG_ZT2, TOG_ZT3, TOG_ZT4, NTOGGLES };


//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//


static void cancel_trap(void *data, long /*ident*/)
{
  FgClearPop    *pop = (FgClearPop*)data;
  pop->unmanage();
}



static void yes_clear_trap2(void *data)   // apply
{
  FgClearPop    *pop = (FgClearPop*)data;
  FieldGeometry *fg  = pop->getFieldGeometry();
  fg->preMultipleOperations();
  if(pop->getClearFlag(TOG_LD )) fg->deleteAllLines   ();
  if(pop->getClearFlag(TOG_RP )) fg->deleteAllRpCards ();
  if(pop->getClearFlag(TOG_PP )) fg->deleteAllPpCards ();
  if(pop->getClearFlag(TOG_ZT1)) fg->deleteAllZt1Cards();
  if(pop->getClearFlag(TOG_ZT2)) fg->deleteAllZt2Cards();
  if(pop->getClearFlag(TOG_ZT3)) fg->deleteAllZt3Cards();
  if(pop->getClearFlag(TOG_ZT4)) fg->deleteAllZt4Cards();
  fg->postMultipleOperations();
}



static void no_clear_trap2(void * /*data*/)   // apply
{
}



static void yes_clear_trap1(void *data)     // ok
{
  FgClearPop    *pop = (FgClearPop*)data;
  yes_clear_trap2(data);
  pop->unmanage();
}



static void no_clear_trap1(void *data)     // ok
{
  FgClearPop    *pop = (FgClearPop*)data;
  no_clear_trap2(data);
  pop->unmanage();
}


static void phrase2(long nlines,
                    long num, long cflag, char *spot, char *msg, int *yes)
{
  char piece[50];
  if((nlines > 0 || num > 0) && cflag)
      {
      sprintf(piece, "%d %s cards (%d lines)\n", num, spot, nlines);
      strcat(msg, piece);
      *yes = TRUE;
      }
}


static void phrase(long num, long cflag, char *spot, char *msg, int *yes)
{
  char piece[50];
  if(num > 0 && cflag)
      {
      sprintf(piece, "%d %s cards\n", num, spot);
      strcat(msg, piece);
      *yes = TRUE;
      }
}


          // ident == 1 means  ok   button.
          // ident == 2 means apply button.

static void ok_trap(void *data, long ident)
{
  FgClearPop    *pop = (FgClearPop*)data;
  FieldGeometry *fg  = pop->getFieldGeometry();
  if(!fg->allowDeletingData())
      {
      if(ident == 1) pop->unmanage();
      return;
      }
  char msg[500];
  int yes = FALSE;
  strcpy(msg, "The following cards\nin your field geometry data\n");
  strcat(msg, "will disappear\nif you continue with this action:\n----\n");
  phrase2(fg->numLines  (),
         fg->totNumFlags(), pop->getClearFlag(TOG_LD ), "LD" , msg, &yes);
  phrase(fg->numRpCards (), pop->getClearFlag(TOG_RP ), "RP" , msg, &yes);
  phrase(fg->numPpCards (), pop->getClearFlag(TOG_PP ), "PP" , msg, &yes);
  phrase(fg->numZt1Cards(), pop->getClearFlag(TOG_ZT1), "ZT1", msg, &yes);
  phrase(fg->numZt2Cards(), pop->getClearFlag(TOG_ZT2), "ZT2", msg, &yes);
  phrase(fg->numZt3Cards(), pop->getClearFlag(TOG_ZT3), "ZT3", msg, &yes);
  phrase(fg->numZt4Cards(), pop->getClearFlag(TOG_ZT4), "ZT4", msg, &yes);
  if(!yes)
      {
      if(ident == 1) pop->unmanage();
      return;
      }
  strcat(msg, "----\n");
  if(fg->dataNeedsSaving())
      {
      strcat(msg, "You have data which\nhas NOT been saved.\n----\n");
      }
  strcat(msg, "Are you sure you want\nto clear this data?");
  if(ident == 1)
    new SLQuestPop(pop, "Question", msg, FALSE,
                            yes_clear_trap1, no_clear_trap1, pop);
  else
    new SLQuestPop(pop, "Question", msg, FALSE,
                            yes_clear_trap2, no_clear_trap2, pop);
}



static void clear_trap(void *data, long /*ident*/)
{
  FgClearPop    *pop = (FgClearPop*)data;
  int cflag = !pop->getClearFlag(0);
  for(int index = 0; index < NTOGGLES; index++)
      {
      pop->setClearFlag(index, cflag);
      }
}



//----------------- get and set clear flag ----------------------//
//----------------- get and set clear flag ----------------------//
//----------------- get and set clear flag ----------------------//

       // public.

long FgClearPop::getClearFlag(int index)  const
{
  assert(index >= 0 && index < NTOGGLES);
  return _cflags[index];
}


void FgClearPop::setClearFlag(int index, int cflag)
{
  assert(index >= 0 && index < NTOGGLES);
  _cflags[index] = cflag;
}




//----------------------- update functions -------------------//
//----------------------- update functions -------------------//
//----------------------- update functions -------------------//


#define TEXT_UPDATE(nld_upfun, totNumFlags)    \
static long nld_upfun(void *data)              \
{                                              \
  FieldGeometry *fg = (FieldGeometry*)data;    \
  return fg->totNumFlags();                    \
}


TEXT_UPDATE(nlines_upfun, numLines)
TEXT_UPDATE(   nld_upfun, totNumFlags)
TEXT_UPDATE(   nrp_upfun, numRpCards)
TEXT_UPDATE(   npp_upfun, numPpCards)
TEXT_UPDATE(  nzt1_upfun, numZt1Cards)
TEXT_UPDATE(  nzt2_upfun, numZt2Cards)
TEXT_UPDATE(  nzt3_upfun, numZt3Cards)
TEXT_UPDATE(  nzt4_upfun, numZt4Cards)



//------------------ sense update functions -------------------//
//------------------ sense update functions -------------------//
//------------------ sense update functions -------------------//


static long ok_sense_upfun(void *data)
{
  FgClearPop    *pop = (FgClearPop*)data;
  FieldGeometry *fg  = pop->getFieldGeometry();
  if(!fg->allowDeletingData()) return FALSE;
  if(fg->numLines   () > 0 && pop->getClearFlag(TOG_LD )) return TRUE;
  if(fg->totNumFlags() > 0 && pop->getClearFlag(TOG_LD )) return TRUE;
  if(fg->numRpCards () > 0 && pop->getClearFlag(TOG_RP )) return TRUE;
  if(fg->numPpCards () > 0 && pop->getClearFlag(TOG_PP )) return TRUE;
  if(fg->numZt1Cards() > 0 && pop->getClearFlag(TOG_ZT1)) return TRUE;
  if(fg->numZt2Cards() > 0 && pop->getClearFlag(TOG_ZT2)) return TRUE;
  if(fg->numZt3Cards() > 0 && pop->getClearFlag(TOG_ZT3)) return TRUE;
  if(fg->numZt4Cards() > 0 && pop->getClearFlag(TOG_ZT4)) return TRUE;
  return FALSE;
}


static long simple_sense_upfun(void *data)
{                                        
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->allowDeletingData();
}



#define SENSEFUN2(nld_sense_upfun, totNumFlags)                \
static long nld_sense_upfun(void *data)                        \
{                                                              \
  FieldGeometry *fg = (FieldGeometry*)data;                    \
  return (fg->allowDeletingData() && fg->totNumFlags() > 0);   \
}


SENSEFUN2(nlines_sense_upfun, numLines)
SENSEFUN2(   nld_sense_upfun, totNumFlags)
SENSEFUN2(   nrp_sense_upfun, numRpCards)
SENSEFUN2(   npp_sense_upfun, numPpCards)
SENSEFUN2(  nzt1_sense_upfun, numZt1Cards)
SENSEFUN2(  nzt2_sense_upfun, numZt2Cards)
SENSEFUN2(  nzt3_sense_upfun, numZt3Cards)
SENSEFUN2(  nzt4_sense_upfun, numZt4Cards)



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


FgClearPop::FgClearPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                               FieldGeometry *fg)
            : SLDialog(slparent, name, hctx, TRUE),
                    _fg         (fg),
                    _cflags     (NULL)
{
  assert(_fg);

  setTitle("Clear Data");
////////  setModal(SLShellContainer::FullAppModal); // not made yet.

  SLSmartForm *work = workArea();

  SLpPush   *clear     = new SLpPush  (work, "clear", 0, "clear all/none");
  SLpToggle *tog_ld    = new SLpToggle(work, "clear all lines" );
  SLpToggle *tog_rp    = new SLpToggle(work, "clear all RP cards" );
  SLpToggle *tog_pp    = new SLpToggle(work, "clear all PP cards" );
  SLpToggle *tog_zt1   = new SLpToggle(work, "clear all ZT1 cards");
  SLpToggle *tog_zt2   = new SLpToggle(work, "clear all ZT2 cards");
  SLpToggle *tog_zt3   = new SLpToggle(work, "clear all ZT3 cards");
  SLpToggle *tog_zt4   = new SLpToggle(work, "clear all ZT4 cards");

  SL2Text   *n_lines = new SL2Text  (work, "clr_xxx", 0, "#lines: ",
                                                       SLpText::_LONG, 7);
  SL2Text   *n_ld    = new SL2Text  (work, "clr_xxx", 0, "#LD cards: ",
                                                       SLpText::_LONG, 7);
  SL2Text   *n_rp    = new SL2Text  (work, "clr_xxx", 0, "#RP cards: ",
                                                       SLpText::_LONG, 7);
  SL2Text   *n_pp    = new SL2Text  (work, "clr_xxx", 0, "#PP cards: ",
                                                       SLpText::_LONG, 7);
  SL2Text   *n_zt1   = new SL2Text  (work, "clr_xxx", 0, "#ZT1 cards: ",
                                                       SLpText::_LONG, 7);
  SL2Text   *n_zt2   = new SL2Text  (work, "clr_xxx", 0, "#ZT2 cards: ",
                                                       SLpText::_LONG, 7);
  SL2Text   *n_zt3   = new SL2Text  (work, "clr_xxx", 0, "#ZT3 cards: ",
                                                       SLpText::_LONG, 7);
  SL2Text   *n_zt4   = new SL2Text  (work, "clr_xxx", 0, "#ZT4 cards: ",
                                                       SLpText::_LONG, 7);

  _cflags = new long [NTOGGLES];

  for(int index = 0; index < NTOGGLES; index++)  { _cflags[index] = 0; }

  clear    ->setAtrap     (clear_trap, this);

  tog_ld   ->setupIvarPoint(&_cflags[TOG_LD ]);
  tog_rp   ->setupIvarPoint(&_cflags[TOG_RP ]);
  tog_pp   ->setupIvarPoint(&_cflags[TOG_PP ]);
  tog_zt1  ->setupIvarPoint(&_cflags[TOG_ZT1]);
  tog_zt2  ->setupIvarPoint(&_cflags[TOG_ZT2]);
  tog_zt3  ->setupIvarPoint(&_cflags[TOG_ZT3]);
  tog_zt4  ->setupIvarPoint(&_cflags[TOG_ZT4]);

  clear    ->setupSenseFun(simple_sense_upfun, _fg);
  tog_ld   ->setupSenseFun(simple_sense_upfun, _fg);
  tog_rp   ->setupSenseFun(simple_sense_upfun, _fg);
  tog_pp   ->setupSenseFun(simple_sense_upfun, _fg);
  tog_zt1  ->setupSenseFun(simple_sense_upfun, _fg);
  tog_zt2  ->setupSenseFun(simple_sense_upfun, _fg);
  tog_zt3  ->setupSenseFun(simple_sense_upfun, _fg);
  tog_zt4  ->setupSenseFun(simple_sense_upfun, _fg);

  n_lines->setupSenseFun(nlines_sense_upfun, _fg);
  n_ld   ->setupSenseFun(   nld_sense_upfun, _fg);
  n_rp   ->setupSenseFun(   nrp_sense_upfun, _fg);
  n_pp   ->setupSenseFun(   npp_sense_upfun, _fg);
  n_zt1  ->setupSenseFun(  nzt1_sense_upfun, _fg);
  n_zt2  ->setupSenseFun(  nzt2_sense_upfun, _fg);
  n_zt3  ->setupSenseFun(  nzt3_sense_upfun, _fg);
  n_zt4  ->setupSenseFun(  nzt4_sense_upfun, _fg);

  n_lines->showLabelAppearance();
  n_ld   ->showLabelAppearance();
  n_rp   ->showLabelAppearance();
  n_pp   ->showLabelAppearance();
  n_zt1  ->showLabelAppearance();
  n_zt2  ->showLabelAppearance();
  n_zt3  ->showLabelAppearance();
  n_zt4  ->showLabelAppearance();

  n_lines->setupIvarFun(nlines_upfun, _fg);
  n_ld   ->setupIvarFun(   nld_upfun, _fg);
  n_rp   ->setupIvarFun(   nrp_upfun, _fg);
  n_pp   ->setupIvarFun(   npp_upfun, _fg);
  n_zt1  ->setupIvarFun(  nzt1_upfun, _fg);
  n_zt2  ->setupIvarFun(  nzt2_upfun, _fg);
  n_zt3  ->setupIvarFun(  nzt3_upfun, _fg);
  n_zt4  ->setupIvarFun(  nzt4_upfun, _fg);

  SLpPush *ok    = addBottomOK     (1, ok_trap    , this);
  SLpPush *apply = addBottomApply  (2, ok_trap    , this);
                   addBottomCancel (0, cancel_trap, this);
                   addBottomHelp   ("CLEAR_DATA");

  ok   ->setupSenseFun(ok_sense_upfun, this);
  apply->setupSenseFun(ok_sense_upfun, this);

           //             LEFT     RIGHT     TOP         BOTTOM
  work->attach(clear    , work   , NULL   ,  work     ,  NULL,  30);
  work->attach(tog_ld   , work   , NULL   ,  clear    ,  NULL,  10);
  work->attach(tog_rp   , work   , NULL   ,  tog_ld   ,  NULL,  10);
  work->attach(tog_pp   , work   , NULL   ,  tog_rp   ,  NULL,  10);
  work->attach(tog_zt1  , work   , NULL   ,  tog_pp   ,  NULL,  10);
  work->attach(tog_zt2  , work   , NULL   ,  tog_zt1  ,  NULL,  10);
  work->attach(tog_zt3  , work   , NULL   ,  tog_zt2  ,  NULL,  10);
  work->attach(tog_zt4  , work   , NULL   ,  tog_zt3  ,  work,  10);
  work->attach(n_lines  , tog_zt4, work   ,  NULL     ,  n_ld,  20);
  work->attach(n_ld     , tog_zt4, work   ,  clear    ,  NULL,  20);
  work->attach(n_rp     , tog_zt4, work   ,  tog_ld   ,  NULL,  20);
  work->attach(n_pp     , tog_zt4, work   ,  tog_rp   ,  NULL,  20);
  work->attach(n_zt1    , tog_zt4, work   ,  tog_pp   ,  NULL,  20);
  work->attach(n_zt2    , tog_zt4, work   ,  tog_zt1  ,  NULL,  20);
  work->attach(n_zt3    , tog_zt4, work   ,  tog_zt2  ,  NULL,  20);
  work->attach(n_zt4    , tog_zt4, work   ,  tog_zt3  ,  NULL,  20);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


FgClearPop::~FgClearPop()
{
  delete [] _cflags;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
