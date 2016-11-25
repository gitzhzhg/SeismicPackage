
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
//---------------------- fg_control_pop.cc ----------------------//
//---------------------- fg_control_pop.cc ----------------------//
//---------------------- fg_control_pop.cc ----------------------//

//          implementation file for the FgControlPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used as a data control panel for
      // field geometry data.


#include "fggui/fg_control_pop.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "fggui/fg_lock_gui.hh"
#include "fggui/fg_clear_pop.hh"
#include "fggui/fg_user_abort_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/sl_error_pop.hh"
#include "sl/slp_push.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "cprim.h"
#include "named_constants.h"
#include <math.h>
#include <iostream.h>
#include <assert.h>




//----------------------- time required ---------------------------//
//----------------------- time required ---------------------------//
//----------------------- time required ---------------------------//

   // These constants represent the estimated time (in minutes)
   // required to create source, receiver, and midpoint gathers
   // when the dataset contains a million traces.

static float   source_factor = 0.02;   // was 0.25
static float receiver_factor = 2.00;   // was 7.00
static float midpoint_factor = 0.60;   // was 1.00


   // This function returns a message regarding the estimated time
   // required to create source, receiver, and midpoint gathers
   // for this dataset.

   // data   = void pointer to the FieldGeometry object.
   // type   = "source" or "receiver" or "CMP".
   // quest  = TRUE if this message is for a question dialog.

   // if quest is TRUE:
   //   NULL will be returned if the question box is not necessary.

static char *time_required(void *data, char *type, int quest)
{
  static char buffer[600], gbuffer[400];
  static char *units_second  = "second";
  static char *units_minute  = "minute";
  static char *units_seconds = "seconds";
  static char *units_minutes = "minutes";
  FieldGeometry *fg = (FieldGeometry*)data;
  float factor;
  if(strings_equal(type, "source"))
      {
      factor = source_factor;
      }
  else if(strings_equal(type, "receiver"))
      {
      factor = receiver_factor;
      if(quest && fg->sourceGathersOutOfDate()) factor += source_factor;
      }
  else // if(strings_equal(type, "CMP"))
      {
      factor = midpoint_factor;
      if(fg->numTraces() > 100000)
          {
          factor *= (1.0 + log(fg->numTraces() / 100000.0));
          }
      if(quest && fg->receiverGathersOutOfDate()) factor += receiver_factor;
      if(quest && fg->sourceGathersOutOfDate  ()) factor += source_factor;
      }
  float minutes = fg->numTraces() * factor / 1000000.0;
  long number;
  char *units;
  if(minutes <= 0.1)
      {
      number = NearestInteger(60.0 * minutes);
      if(number == 1) units = units_second;
      else            units = units_seconds;
      }
  else if(minutes <= 1.0)
      {
      number = 5 * NearestInteger(12.0 * minutes);
      units = units_seconds;
      }
  else if(minutes <= 10.0)
      {
      number = NearestInteger(minutes);
      if(number == 1) units = units_minute;
      else            units = units_minutes;
      }
  else
      {
      number = 5 * NearestInteger(minutes / 5.0);
      units = units_minutes;
      }
  if(quest)
      {
      if(strings_equal(type, "receiver"))
          {
          sprintf(buffer,"Creating %s gathers\nmay take about\n%d %s.\n",
                                              type, number, units);
          if(fg->sourceGathersOutOfDate())
            strcat(buffer, "(this also includes creating source gathers)\n");
          }
      else if(strings_equal(type, "CMP"))
          {
          sprintf(buffer,
                  "Creating %s gathers\nmay take\n%d %s (or less).\n",
                                              type, number, units);
          if(fg->sourceGathersOutOfDate())
            strcat(buffer, "(this also includes creating source gathers)\n");
          if(fg->receiverGathersOutOfDate())
            strcat(buffer, "(this also includes creating receiver gathers)\n");
          }
      else // if(strings_equal(type, "source"))
          {
          sprintf(buffer,"Creating %s gathers\nmay take about\n%d %s.\n",
                                              type, number, units);
          }
      strcat(buffer, "----\n");
      if(strings_equal(type, "CMP"))
          {
          double xorigin = fg->getXorigin();
          double yorigin = fg->getYorigin();
          double xwidth  = fg->getXgridWidth();
          double ywidth  = fg->getYgridWidth();
          double angle   = fg->getRotationAngle();
          static char *right_handed = "right handed";
          static char *left_handed  = "left handed";
          char *handedness;
          if(fg->isRightHanded()) handedness = right_handed;
          else                    handedness = left_handed;
          sprintf(gbuffer,
     "%s\n%s\n%s\n%s%f\n%s%f\n%s%f\n%s%f\n%s%f\n%s\n%s\n%s\n%s\n",
                      "Your grid transform currently",
                      "contains the following values:",
                      "----",
                      "X origin:  "      , xorigin,
                      "Y origin:  "      , yorigin,
                      "rotation angle:  ", angle,
                      "X grid width:  "  , xwidth,
                      "Y grid width:  "  , ywidth,
                      handedness,
                      "----",
                      "Make sure the transform is correct",
                      "before continuing.");
          strcat(buffer, gbuffer);
          }
      else
          {
          if(minutes < 0.5) return NULL;
       ///////////   gbuffer[0]='\0';
          }
      strcat(buffer, "----\nDo you wish to continue?");
      }
  else if(strings_equal(type, "CMP"))
      {
      sprintf(buffer, "Creating %s gathers may take %d %s (or less).",
                          type, number, units);
      }
  else
      {
      sprintf(buffer, "Creating %s gathers may take about %d %s.",
                          type, number, units);
      }
  return buffer;
}



//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//


static void freeze_trap(void *data, long /*ident*/)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->dependentUpdatesFrozen()) fg->resumeDependentUpdates();
  else                             fg->freezeDependentUpdates();
}


static void yes_s_gathers_trap(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->updateSourceGathers();
}


static void yes_r_gathers_trap(void *data)
{
  FgControlPop   *dcp = (FgControlPop*)data;
  FieldGeometry  *fg  = dcp->getFieldGeometry();
  FgUserAbortGui *uag = new FgUserAbortGui(fg, dcp->topWidget());
  fg->updateReceiverGathers();
  delete uag;
}


static void yes_m_gathers_trap(void *data)
{
  FgControlPop   *dcp = (FgControlPop*)data;
  FieldGeometry  *fg  = dcp->getFieldGeometry();
  FgUserAbortGui *uag = new FgUserAbortGui(fg, dcp->topWidget());
  long ncmps = fg->updateMidpointGathers();
  delete uag;
  if(ncmps <= 0) return;   // no error or user abort.
  float average = (float)fg->numTraces() / (float)ncmps;
  char msg1[80], msg2[80], msg3[300];
  sprintf(msg1, "You are trying to create %d CMP gathers\n", ncmps);
  sprintf(msg2, "with an average fold of only %f.\n----\n", average);
  strcpy(msg3, msg1);
  strcat(msg3, msg2);
  strcat(msg3, "Please modify your GRID TRANSFORM\n");
  strcat(msg3, "or (for 2-D) the FIXDIST parameter\n");
  strcat(msg3, "and try again.\n");
  new SLErrorPop(dcp, "Problem trying to create CMP gathers", msg3);
}




static void maybe_s_gathers_trap(void *data, long /*ident*/)
{
  FgControlPop  *dcp = (FgControlPop*)data;
  FieldGeometry *fg  = dcp->getFieldGeometry();
  char *msg = time_required(fg, "source", TRUE);
  if(msg) new SLQuestPop(dcp, "Question", msg, FALSE,
                 yes_s_gathers_trap, NULL, fg);
  else yes_s_gathers_trap(fg);
}


static void maybe_r_gathers_trap(void *data, long /*ident*/)
{
  FgControlPop  *dcp = (FgControlPop*)data;
  FieldGeometry *fg  = dcp->getFieldGeometry();
  char *msg = time_required(fg, "receiver", TRUE);
  if(msg) new SLQuestPop(dcp, "Question", msg, FALSE,
                 yes_r_gathers_trap, NULL, dcp);
  else yes_r_gathers_trap(dcp);
}


static void maybe_m_gathers_trap(void *data, long /*ident*/)
{
  FgControlPop  *dcp = (FgControlPop*)data;
  FieldGeometry *fg  = dcp->getFieldGeometry();
  char *msg = time_required(fg, "CMP", TRUE);
  if(msg) new SLQuestPop(dcp, "Question", msg, FALSE,
                 yes_m_gathers_trap, NULL, dcp);
  else yes_m_gathers_trap(dcp);
}


static void upfold_trap(void *data, long /*ident*/)
{
  FgControlPop   *dcp = (FgControlPop*)data;
  FieldGeometry  *fg  = dcp->getFieldGeometry();
  FgUserAbortGui *uag = new FgUserAbortGui(fg, dcp->topWidget());
  fg->updateLiveFold();
  delete uag;
}


static void ve_trap(void *data, long /*ident*/,
                 float /*oldvar*/, float newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(newvar <= 0)
      {
      fg->ringBell();
      return;
      }
  fg->setVe(newvar);
}


static void ref_trap(void *data, long /*ident*/,
                 float /*oldvar*/, float newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setRef(newvar);
}


static void fixdist_trap(void *data, long /*ident*/,
                 float /*oldvar*/, float newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setFixdist(newvar);
}



//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//

static char *freeze_upfun(void *data)
{
  static char *buffer1 = " Freeze Automatic Updates ";
  static char *buffer2 = " Resume Automatic Updates ";
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->dependentUpdatesFrozen()) return buffer2;
  return buffer1;
}


static long clear_sense_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->allowDeletingData();
}


static long s_gathers_sense_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return ( fg->numTraces() > 0 &&
           fg->sourceGathersOutOfDate());
}


static long r_gathers_sense_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return ( fg->numTraces() > 0 &&
           fg->receiverGathersOutOfDate());
}


static long m_gathers_sense_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return ( fg->numTraces() > 0 &&
           fg->midpointGathersOutOfDate());
}


static long upfold_sense_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return ( fg->numTraces() > 0 &&
          !fg->midpointGathersOutOfDate() &&
           fg->liveFoldOutOfDate());
}


static float ve_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getVe();
}


static float ref_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getRef();
}


static float fixdist_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getFixdist();
}


static char *chain_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  static char *buffer0 = "???";
  static char *buffer1 = "horizontal chaining";
  static char *buffer2 = "slope chaining";
  static char *buffer3 = "no chaining";
  int chaining = fg->getChaining();
  switch(chaining)
      {
      case HORIZONTAL_CHAINING: return buffer1;
      case SLOPE_CHAINING     : return buffer2;
      case NO_CHAINING        : return buffer3;
      }
  return buffer0;
}



//------------- gathers status update functions ---------------//
//------------- gathers status update functions ---------------//
//------------- gathers status update functions ---------------//

static char *status1 = "(no traces)";
static char *status2 = "out of date";
static char *status3 = "created";
static char *status4 = " ";
static char *status5 = "frozen";
static char *status6 = "incomplete";


static char *s_gathers_status_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numTraces() == 0)          return status1;
  if(fg->sourceGathersOutOfDate())  return status2;
  if(fg->sourceGathersIncomplete()) return status6;
  return status3;
}


static char *r_gathers_status_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numTraces() == 0)            return status1;
  if(fg->receiverGathersOutOfDate())  return status2;
  if(fg->receiverGathersIncomplete()) return status6;
  return status3;
}


static char *m_gathers_status_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numTraces() == 0)            return status1;
  if(fg->midpointGathersOutOfDate())  return status2;
  if(fg->midpointGathersIncomplete()) return status6;
  return status3;
}


static char *freeze_status_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->dependentValuesOutOfDate()) return status2;
  if(fg->dependentUpdatesFrozen())   return status5;
  return status4;
}



//------------- gathers message update functions ----------------//
//------------- gathers message update functions ----------------//
//------------- gathers message update functions ----------------//


static char *s_gathers_msg_upfun(void *data)
{
  return time_required(data, "source", FALSE);
}


static char *r_gathers_msg_upfun(void *data)
{
  return time_required(data, "receiver", FALSE);
}


static char *m_gathers_msg_upfun(void *data)
{
  return time_required(data, "CMP", FALSE);
}



//-------------------- unplaced update functions ----------------//
//-------------------- unplaced update functions ----------------//
//-------------------- unplaced update functions ----------------//

static char unplaced[40];
static char *blank = " ";

static char *s_unplaced_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->sourceGathersOutOfDate()) return blank;
  long number = fg->numUnplacedSources();
  if(number == 0) return blank;
  sprintf(unplaced, "#unplaced sources: %d", number);
  return unplaced;
}


static char *r_unplaced_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->receiverGathersOutOfDate()) return blank;
  long number = fg->numUnplacedTraces();
  if(number == 0) return blank;
  sprintf(unplaced, "#unplaced traces : %d", number);
  return unplaced;
}


static char *m_unplaced_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->midpointGathersOutOfDate()) return blank;
  long number = fg->numUnplacedTraces();
  if(number == 0) return blank;
  sprintf(unplaced, "#unplaced traces : %d", number);
  return unplaced;
}




//------------------ text update functions -------------------//
//------------------ text update functions -------------------//
//------------------ text update functions -------------------//


#define UPFUN(nlin_upfun, numLines)            \
static long nlin_upfun(void *data)             \
{                                              \
  FieldGeometry *fg = (FieldGeometry*)data;    \
  return fg->numLines();                       \
}


UPFUN(nlin_upfun, numLines)
UPFUN(ngrp_upfun, numGroups)
UPFUN(ntrc_upfun, numTraces)
UPFUN(nld_upfun , totNumFlags)
UPFUN(nrp_upfun , numRpCards)
UPFUN(npp_upfun , numPpCards)
UPFUN(nzt1_upfun, numZt1Cards)
UPFUN(nzt2_upfun, numZt2Cards)
UPFUN(nzt3_upfun, numZt3Cards)
UPFUN(nzt4_upfun, numZt4Cards)
UPFUN(nsg_upfun , totNumSources)
UPFUN(nrg_upfun , totNumReceivers)
UPFUN(nmg_upfun , numCmpGathers)



//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//

      // SLDelay does not have to be made.

static Widget get_any_widget(SLDelay *gui)
{
  assert(gui);
  if(gui->W      ()) return gui->W      ();
  if(gui->wParent()) return gui->wParent();
  return get_any_widget(gui->slParent());
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//

static char *defres[] =
               {
               ".foreground:     green4",
               ".fontList:       8x13bold",
               NULL
               };

static char *defres2[] =
               {
               "*fontList:       8x13bold",
               NULL
               };

static char *defres3[] =
               {
               ".foreground:     red",
               ".fontList:       8x13bold",
               NULL
               };


FgControlPop::FgControlPop(SLDelay *slparent, char *name, HelpCtx hctx,
                          FieldGeometry *fg,
                          ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
                        _fg    (fg),
                        _gt    (NULL)
{
  assert(_fg);
  setTitle("Data Control Panel");

  Widget any = get_any_widget(this);
  setDefaultResources(XtDisplay(any), "cp_xxx"     , defres2);
  setDefaultResources(XtDisplay(any), "s_gthr_stat", defres);
  setDefaultResources(XtDisplay(any), "r_gthr_stat", defres);
  setDefaultResources(XtDisplay(any), "m_gthr_stat", defres);
  setDefaultResources(XtDisplay(any), "freeze_stat", defres);
  setDefaultResources(XtDisplay(any), "s_gthr_msg" , defres);
  setDefaultResources(XtDisplay(any), "r_gthr_msg" , defres);
  setDefaultResources(XtDisplay(any), "m_gthr_msg" , defres);
  setDefaultResources(XtDisplay(any), "cp_yyy"     , defres3);

  FgClearPop *clear_pop = new FgClearPop(this, "clear_pop", hctx, fg);

  SLSmartForm *work   = workArea();
  SLSmartForm *work1  = new SLSmartForm(work, "work1", hctx, TRUE);
  SLSmartForm *work2  = new SLSmartForm(work, "work2", hctx, TRUE);
  SLSmartForm *work3  = new SLSmartForm(work, "work3", hctx, TRUE);

  SL2Text *nlin = new SL2Text (work1, "cp_xxx", 0, "#lines :"    ,
                                                       SLpText::_LONG, 8);
  SL2Text *ngrp = new SL2Text (work1, "cp_xxx", 0, "#groups:"    ,
                                                       SLpText::_LONG, 8);
  SL2Text *ntrc = new SL2Text (work1, "cp_xxx", 0, "#traces:"    ,
                                                       SLpText::_LONG, 8);

  SL2Text *nld  = new SL2Text (work1, "cp_xxx", 0, "#LD cards:",
                                                       SLpText::_LONG, 7);
  SL2Text *nrp  = new SL2Text (work1, "cp_xxx", 0, "#RP cards:",
                                                       SLpText::_LONG, 7);
  SL2Text *npp  = new SL2Text (work1, "cp_xxx", 0, "#PP cards:",
                                                       SLpText::_LONG, 7);

  SL2Text *nzt1 = new SL2Text (work1, "cp_xxx", 0, "#ZT1 cards:",
                                                       SLpText::_LONG, 4);
  SL2Text *nzt2 = new SL2Text (work1, "cp_xxx", 0, "#ZT2 cards:",
                                                       SLpText::_LONG, 4);
  SL2Text *nzt3 = new SL2Text (work1, "cp_xxx", 0, "#ZT3 cards:",
                                                       SLpText::_LONG, 4);
  SL2Text *nzt4 = new SL2Text (work1, "cp_xxx", 0, "#ZT4 cards:",
                                                       SLpText::_LONG, 4);

  SL2Text *nsg  = new SL2Text (work1, "cp_xxx", 0, "#source gathers  :",
                                                       SLpText::_LONG, 8);
  SL2Text *nrg  = new SL2Text (work1, "cp_xxx", 0, "#receiver gathers:",
                                                       SLpText::_LONG, 8);
  SL2Text *nmg  = new SL2Text (work1, "cp_xxx", 0, "#CMP gathers     :",
                                                       SLpText::_LONG, 8);

  SLDelay  *msg   = new FgMessageGui (work, "dcp_msg"  , _fg);
  SLDelay  *stat  = new FgStatusGui  (work, "dcp_stat" , _fg, clist);

  SLDelay  *lock      = new FgLockGui    (work2, "dcp_lock", _fg);
  SLpPush  *freeze    = new SLpPush      (work2, "freeze");
  SLpPush  *clear     = new SLpPush      (work2, "Clear Data...");

  SLpPush  *s_gathers = new SLpPush      (work2, "Create Source Gathers");
  SLpPush  *r_gathers = new SLpPush      (work2, "Create Receiver Gathers");
  SLpPush  *m_gathers = new SLpPush      (work2, "Create CMP Gathers");
  SLpPush  *upfold    = new SLpPush      (work2, "update live fold");

  SLpText  *s_stat = new SLpText (work2, "s_gthr_stat", 0, SLpText::_CHAR, 11);
  SLpText  *r_stat = new SLpText (work2, "r_gthr_stat", 0, SLpText::_CHAR, 11);
  SLpText  *m_stat = new SLpText (work2, "m_gthr_stat", 0, SLpText::_CHAR, 11);
  SLpText  *f_stat = new SLpText (work2, "freeze_stat", 0, SLpText::_CHAR, 11);

  SLpText  *s_msg  = new SLpText (work2, "s_gthr_msg");
  SLpText  *r_msg  = new SLpText (work2, "r_gthr_msg");
  SLpText  *m_msg  = new SLpText (work2, "m_gthr_msg");
 
  SLpText  *s_unplaced  = new SLpText (work2, "cp_yyy", 0, SLpText::_CHAR, 26);
  SLpText  *r_unplaced  = new SLpText (work2, "cp_yyy", 0, SLpText::_CHAR, 26);
  SLpText  *m_unplaced  = new SLpText (work2, "cp_yyy", 0, SLpText::_CHAR, 26);
 
        addBottomRemove();
  _gt = addBottomPush("Grid Transformation...");
        addBottomHelp("DATA_CONTROL_PANEL");

  clear     ->manageShellWhenPressed(clear_pop);
  nlin      ->showLabelAppearance();
  ngrp      ->showLabelAppearance();
  ntrc      ->showLabelAppearance();
  nld       ->showLabelAppearance();
  nrp       ->showLabelAppearance();
  npp       ->showLabelAppearance();
  nzt1      ->showLabelAppearance();
  nzt2      ->showLabelAppearance();
  nzt3      ->showLabelAppearance();
  nzt4      ->showLabelAppearance();
  nsg       ->showLabelAppearance();
  nrg       ->showLabelAppearance();
  nmg       ->showLabelAppearance();
  s_stat    ->showLabelAppearance();
  r_stat    ->showLabelAppearance();
  m_stat    ->showLabelAppearance();
  f_stat    ->showLabelAppearance();
  s_msg     ->showLabelAppearance();
  r_msg     ->showLabelAppearance();
  m_msg     ->showLabelAppearance();
  s_unplaced->showLabelAppearance();
  r_unplaced->showLabelAppearance();
  m_unplaced->showLabelAppearance();

  freeze   ->setAtrap       (freeze_trap          , _fg);
  s_gathers->setAtrap       (maybe_s_gathers_trap , this);
  r_gathers->setAtrap       (maybe_r_gathers_trap , this);
  m_gathers->setAtrap       (maybe_m_gathers_trap , this);
  upfold   ->setAtrap       (upfold_trap          , this);

  nlin->setupIvarFun(nlin_upfun, _fg);
  ngrp->setupIvarFun(ngrp_upfun, _fg);
  ntrc->setupIvarFun(ntrc_upfun, _fg);
  nld ->setupIvarFun(nld_upfun , _fg);
  nrp ->setupIvarFun(nrp_upfun , _fg);
  npp ->setupIvarFun(npp_upfun , _fg);
  nzt1->setupIvarFun(nzt1_upfun, _fg);
  nzt2->setupIvarFun(nzt2_upfun, _fg);
  nzt3->setupIvarFun(nzt3_upfun, _fg);
  nzt4->setupIvarFun(nzt4_upfun, _fg);
  nsg ->setupIvarFun(nsg_upfun , _fg);
  nrg ->setupIvarFun(nrg_upfun , _fg);
  nmg ->setupIvarFun(nmg_upfun , _fg);

  freeze    ->setupLabelFun  (freeze_upfun          , _fg);
  clear     ->setupSenseFun  (clear_sense_upfun     , _fg);
  s_gathers ->setupSenseFun  (s_gathers_sense_upfun , _fg);
  r_gathers ->setupSenseFun  (r_gathers_sense_upfun , _fg);
  m_gathers ->setupSenseFun  (m_gathers_sense_upfun , _fg);
  upfold    ->setupSenseFun  (upfold_sense_upfun    , _fg);
  s_stat    ->setupCvarFun   (s_gathers_status_upfun, _fg);
  r_stat    ->setupCvarFun   (r_gathers_status_upfun, _fg);
  m_stat    ->setupCvarFun   (m_gathers_status_upfun, _fg);
  f_stat    ->setupCvarFun   (freeze_status_upfun,    _fg);
  s_msg     ->setupCvarFun   (s_gathers_msg_upfun   , _fg);
  r_msg     ->setupCvarFun   (r_gathers_msg_upfun   , _fg);
  m_msg     ->setupCvarFun   (m_gathers_msg_upfun   , _fg);
  s_unplaced->setupCvarFun   (s_unplaced_upfun      , _fg);
  r_unplaced->setupCvarFun   (r_unplaced_upfun      , _fg);
  m_unplaced->setupCvarFun   (m_unplaced_upfun      , _fg);

  work3->showEvenSpacing();
  SL2Text *ve      = new SL2Text
                       (work3, "ve"     , 0, "Ve:"     , SLpText::_FLOAT, 6);
  SL2Text *ref     = new SL2Text
                       (work3, "ref"    , 0, "Ref:"    , SLpText::_FLOAT, 6);
  SL2Text *fixdist = new SL2Text
                       (work3, "fixdist", 0, "Fixdist:", SLpText::_FLOAT, 6);
  SLpText *chain   = new SLpText 
                       (work3, "chain"  , 0,             SLpText::_CHAR, 20);

  chain->showLabelAppearance();

  ve     ->setFtrap     (     ve_trap, fg);
  ref    ->setFtrap     (    ref_trap, fg);
  fixdist->setFtrap     (fixdist_trap, fg);

  ve     ->setupFvarFun (     ve_update, fg);
  ref    ->setupFvarFun (    ref_update, fg);
  fixdist->setupFvarFun (fixdist_update, fg);
  chain  ->setupCvarFun (  chain_update, fg);

               /////       LEFT     RIGHT      TOP        BOTTOM

  work->attach(work1     , work  ,  work     , work     ,  NULL );
  work->attach(work3     , work  ,  work     , work1    ,  NULL, 0, 0, 10);
  work->attach(msg       , work  ,  work     , work3    ,  NULL );
  work->attach(stat      , work  ,  work     , msg      ,  NULL );
  work->attach(work2     , work  ,  work     , stat     ,  work );

  work->attach(nlin      , work1 ,  NULL     , work1    ,  NULL , 10);
  work->attach(ngrp      , work1 ,  NULL     , nlin     ,  NULL , 10);
  work->attach(ntrc      , work1 ,  NULL     , ngrp     ,  NULL , 10);
  work->attach(nld       , nlin  ,  NULL     , work1    ,  NULL , 10);
  work->attach(nrp       , nlin  ,  NULL     , nld      ,  NULL , 10);
  work->attach(npp       , nlin  ,  NULL     , nrp      ,  NULL , 10);
  work->attach(nzt1      , nld   ,  NULL     , work1    ,  NULL , 10);
  work->attach(nzt2      , nld   ,  NULL     , nzt1     ,  NULL , 10);
  work->attach(nzt3      , nld   ,  NULL     , nzt2     ,  NULL , 10);
  work->attach(nzt4      , nld   ,  NULL     , nzt3     ,  work1, 10);
  work->attach(nsg       , nzt1  ,  work1    , work1    ,  NULL , 10);
  work->attach(nrg       , nzt1  ,  work1    , nsg      ,  NULL , 10);
  work->attach(nmg       , nzt1  ,  work1    , nrg      ,  NULL , 10);

  work->attach(lock      , work2 ,  r_gathers, work2    ,  NULL );
  work->attach(freeze    , work2 ,  NULL     , lock     ,  NULL , 10);
  work->attach(f_stat    , freeze,  NULL     , lock     ,  NULL );
  work->attach(clear     , work2 ,  NULL     , freeze   ,  NULL , 10);
  work->attach(s_gathers , NULL  ,  s_stat   , work2    ,  NULL );
  work->attach(r_gathers , NULL  ,  r_stat   , s_gathers,  NULL );
  work->attach(m_gathers , NULL  ,  m_stat   , r_gathers,  NULL );
  work->attach(upfold    , NULL  ,  m_gathers, r_gathers,  NULL , 0, 10);
  work->attach(s_stat    , NULL  ,  work2    , work2    ,  NULL );
  work->attach(r_stat    , NULL  ,  work2    , s_gathers,  NULL );
  work->attach(m_stat    , NULL  ,  work2    , r_gathers,  NULL );
  work->attach(s_msg     , work2 , s_unplaced, clear    ,  NULL , 10);
  work->attach(r_msg     , work2 , r_unplaced, s_msg    ,  NULL , 10);
  work->attach(m_msg     , work2 , m_unplaced, r_msg    ,  work2, 10);
  work->attach(s_unplaced, NULL  ,  work2    , clear    ,  NULL );
  work->attach(r_unplaced, NULL  ,  work2    , s_msg    ,  NULL );
  work->attach(m_unplaced, NULL  ,  work2    , r_msg    ,  work2);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


FgControlPop::~FgControlPop()
{
}



//---------------------- add transform pop -----------------------//
//---------------------- add transform pop -----------------------//
//---------------------- add transform pop -----------------------//

      // public.

void FgControlPop::addTransformPop(TransformPop *tp)
{
  _gt->manageShellWhenPressed((SLShellContainer*)tp);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
