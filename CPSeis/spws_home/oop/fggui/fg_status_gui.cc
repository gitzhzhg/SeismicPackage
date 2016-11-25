
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
//---------------------- fg_status_gui.cc ------------------------//
//---------------------- fg_status_gui.cc ------------------------//
//---------------------- fg_status_gui.cc ------------------------//

//         implementation file for the FgStatusGui class
//               derived from the SLSmartForm class
//                derived from the FgInform class
//                        subdirectory fggui


#include "fggui/fg_status_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/slp_text.hh"
#include "sl/container_list.hh"
#include "sl/sl_poplist_button.hh"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//


FgStatusGui::FgStatusGui(  SLDelay             *slparent,
                           char                *name,
                           class FieldGeometry *fg,
                           class ContainerList *clist,
                           HelpCtx              hctx,
                           Boolean              doframe,
                           Boolean              make_if_can,
                           Boolean              manage_now  )
       : SLSmartForm(slparent, name, hctx, doframe, make_if_can, manage_now),
         FgInform(fg),
                 _stat1   (NULL),
                 _stat2   (NULL),
                 _stat3   (NULL),
                 _stat4   (NULL)
{
  constructorHelper(clist);
}



FgStatusGui::FgStatusGui(  Widget               wparent,
                           char                *name,
                           class FieldGeometry *fg,
                           class ContainerList *clist,
                           HelpCtx              hctx,
                           Boolean              doframe,
                           Boolean              make_now,
                           Boolean              manage_now  )
       : SLSmartForm(wparent, name, hctx, doframe, make_now, manage_now),
         FgInform(fg),
                 _stat1   (NULL),
                 _stat2   (NULL),
                 _stat3   (NULL),
                 _stat4   (NULL)
{
  constructorHelper(clist);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


FgStatusGui::~FgStatusGui()
{
}




//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//

      // SLDelay does not have to be made

static Widget get_any_widget(SLDelay *gui)
{
  assert(gui);
  if(gui->W      ()) return gui->W      ();
  if(gui->wParent()) return gui->wParent();
  return get_any_widget(gui->slParent());
}



//--------------------- default resources --------------------//
//--------------------- default resources --------------------//
//--------------------- default resources --------------------//


static char *defres[] = {
           "*fg_status_gui_1.foreground: blue"  ,
           "*fg_status_gui_2.foreground: green4",
           "*fg_status_gui_3.foreground: red"   ,
           "*fg_status_gui_4.foreground: purple",
           "*fg_status_gui_1.fontList: fixed",
           "*fg_status_gui_2.fontList: fixed",
           "*fg_status_gui_3.fontList: fixed",
           "*fg_status_gui_4.fontList: fixed",
/*
           "*fg_status_gui_1.fontList: -*-*-bold-r-*-*-*-60-*-*-f-*-*-*",
           "*fg_status_gui_2.fontList: -*-*-bold-r-*-*-*-60-*-*-f-*-*-*",
           "*fg_status_gui_3.fontList: -*-*-bold-r-*-*-*-60-*-*-f-*-*-*",
           "*fg_status_gui_4.fontList: -*-*-bold-r-*-*-*-60-*-*-f-*-*-*",
           "*fg_status_gui_1.fontList: 8x13bold",
           "*fg_status_gui_2.fontList: 8x13bold",
           "*fg_status_gui_3.fontList: 8x13bold",
           "*fg_status_gui_4.fontList: 8x13bold",
*/
            NULL };

static char *defres1[] = {
            ".foreground: blue"  ,
            ".fontList:   fixed",
  ///       ".fontList: -*-*-bold-r-*-*-*-60-*-*-f-*-*-*",
            NULL };

static char *defres2[] = {
            ".foreground: green4"  ,
            ".fontList:   fixed",
  ///       ".fontList: -*-*-bold-r-*-*-*-60-*-*-f-*-*-*",
            NULL };

static char *defres3[] = {
            ".foreground: red"  ,
            ".fontList:   fixed",
  ///       ".fontList: -*-*-bold-r-*-*-*-60-*-*-f-*-*-*",
            NULL };

static char *defres4[] = {
            ".foreground: purple"  ,
            ".fontList:   fixed",
  ///       ".fontList: -*-*-bold-r-*-*-*-60-*-*-f-*-*-*",
            NULL };


    /// Thinge I do not understand:
    /// (1) If I use defres, but not defres1,2,3,4:
    ///       The resources never get set.
    /// (2) If I use defres1,2,3,4, but not defres:
    ///       The resources get set if the instance name is
    ///           "status" or "dcp_stat", but the colors all
    ///           turn out to be red if the instance name is "stat".
    /// (3) If I use both defres and defres1,2,3,4:
    ///       Everything is OK.




//--------------------- update functions ----------------------------//
//--------------------- update functions ----------------------------//
//--------------------- update functions ----------------------------//


char *stat4_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  static char *buffer1 = "AUTO-UPDATE VALUES are OUT-OF-DATE";
  static char *buffer2 = "AUTOMATIC UPDATES are FROZEN";
  static char *buffer3 = " ";
  if(fg->dependentValuesOutOfDate()) return buffer1;
  if(fg->dependentUpdatesFrozen  ()) return buffer2;
  return buffer3;
}






//------------------- constructor helper ----------------------//
//------------------- constructor helper ----------------------//
//------------------- constructor helper ----------------------//

        // private.

void FgStatusGui::constructorHelper(ContainerList *clist)
{
  Widget any = get_any_widget(this);
  setDefRes(XtDisplay(any), "fg_status_gui_1", defres1);
  setDefRes(XtDisplay(any), "fg_status_gui_2", defres2);
  setDefRes(XtDisplay(any), "fg_status_gui_3", defres3);
  setDefRes(XtDisplay(any), "fg_status_gui_4", defres4);
  setDefRes(XtDisplay(any), (char*)instanceName(), defres);

  _stat1 = new SLpText(this, "fg_status_gui_1", 0, SLpText::_CHAR, 45);
  _stat2 = new SLpText(this, "fg_status_gui_2", 0, SLpText::_CHAR, 55);
  _stat3 = new SLpText(this, "fg_status_gui_3", 0, SLpText::_CHAR, 19);
  _stat4 = new SLpText(this, "fg_status_gui_4", 0, SLpText::_CHAR, 36);

  _stat1->showLabelAppearance();
  _stat2->showLabelAppearance();
  _stat3->showLabelAppearance();
  _stat4->showLabelAppearance();

  _stat4->setupCvarFun(stat4_upfun, _fg);

  SLPopListButton *go_to = NULL;
  if(clist) go_to = new SLPopListButton(this, "goto", clist);

          //  this  <-stat4->   stat1->  this
          //  this  <-stat2->   stat3->  this

//                     left     right   top    bottom

  if(go_to)
      {
      attach(_stat4  , this  , _stat1,  this , NULL);
      attach(_stat1  , NULL  ,  go_to,  this , NULL);
      attach(_stat2  , this  , _stat3, _stat1, this);
      attach(_stat3  , NULL  ,  go_to, _stat1, this);
      attach(go_to   , NULL  ,  this ,  this , this, 0, 4, 4, 4);
      }
  else
      {
      attach(_stat4  , this  , _stat1,  this , NULL);
      attach(_stat1  , NULL  ,  this ,  this , NULL);
      attach(_stat2  , this  , _stat3, _stat1, this);
      attach(_stat3  , NULL  ,  this , _stat1, this);
      }

  if(made()) make();
  showStatus1();
  showStatus2();
  showStatus3();
//showStatus4();
}



//----------------------- finished changes -----------------------//
//----------------------- finished changes -----------------------//
//----------------------- finished changes -----------------------//

      // protected virtual function overriding FgInform.

void FgStatusGui::finishedChanges(FieldGeometry*)
{
  showStatus1();
  showStatus2();
  showStatus3();
//showStatus4();
}



//------------------------- show status1 ------------------------//
//------------------------- show status1 ------------------------//
//------------------------- show status1 ------------------------//

         // private.

void FgStatusGui::showStatus1()
{
/*
  int frz = _fg->dependentUpdatesFrozen();
  int dep = _fg->dependentValuesOutOfDate();
*/
  int ss  = _fg->sourceGathersOutOfDate();
  int rr  = _fg->receiverGathersOutOfDate();
  int mm  = _fg->midpointGathersOutOfDate();

  char msg[121];
  msg[0] = '\0';
/*
  if(dep)
      {
      strcat(msg, "AUTO-UPDATE VALUES ");
      if(ss || rr || mm) strcat(msg, "and ");
      }
  else if(frz)
      {
      strcat(msg, "AUTOMATIC UPDATES are FROZEN ");
      if(ss || rr || mm) strcat(msg, "and ");
      }
*/
  if     (ss && rr && mm) strcat(msg, "SOURCE/RECEIVER/CMP GATHERS ");
  else if(ss && rr      ) strcat(msg,     "SOURCE/RECEIVER GATHERS ");
  else if(ss       && mm) strcat(msg,          "SOURCE/CMP GATHERS ");
  else if(      rr && mm) strcat(msg,        "RECEIVER/CMP GATHERS ");
  else if(ss            ) strcat(msg,              "SOURCE GATHERS ");
  else if(      rr      ) strcat(msg,            "RECEIVER GATHERS ");
  else if(            mm) strcat(msg,                 "CMP GATHERS ");
////  if(dep || ss || rr || mm) strcat(msg, "are OUT-OF-DATE");
  if(ss || rr || mm) strcat(msg, "are OUT-OF-DATE");
  _stat1->setCvar(msg);
}



//----------------------- show status2 ------------------------------//
//----------------------- show status2 ------------------------------//
//----------------------- show status2 ------------------------------//

          // private.

void FgStatusGui::showStatus2()
{
  int lock = _fg->getDataLock();
  switch(lock)
     {
     case LOCK_NONE:
       _stat2->setCvar("you may change any data you wish");
       break;
     case LOCK_DEL:
       _stat2->setCvar("you may NOT delete any data");
       break;
     case LOCK_S:
       _stat2->setCvar("you may NOT change Source Gathers");
       break;
     case LOCK_S_R:
       _stat2->setCvar("you may NOT change Source or Receiver Gathers");
       break;
     case LOCK_S_R_CMP:
       _stat2->setCvar("you may NOT change Source or Receiver or CMP Gathers");
       break;
     case LOCK_ALL:
       _stat2->setCvar("you may NOT change any data");
       break;
     default:
       _stat2->setCvar("I have no idea what you can and cannot do...");
       break;
     }
}



//----------------------- show status3 ------------------------------//
//----------------------- show status3 ------------------------------//
//----------------------- show status3 ------------------------------//

          // private.

void FgStatusGui::showStatus3()
{
  if(_fg->dataNeedsSaving()) _stat3->setCvar("DATA NEEDS SAVING");
  else                       _stat3->setCvar(" ");
}



//----------------------- show status4 ------------------------------//
//----------------------- show status4 ------------------------------//
//----------------------- show status4 ------------------------------//

      // private.
      // replaced by stat4_upfun.
      // because _frozen is set in FieldGeometry::freezeDependentUpdates()
      //   AFTER calling _informer->freezingDependentUpdates(),
      //   and therefore AFTER calling _informer->finishedChanges(),
      //   and therefore AFTER showStatus4() is called.

void FgStatusGui::showStatus4()
{
/*
  int frz = _fg->dependentUpdatesFrozen();
  int dep = _fg->dependentValuesOutOfDate();

  if     (dep) _stat4->setCvar("AUTO-UPDATE VALUES are OUT-OF-DATE");
  else if(frz) _stat4->setCvar("AUTOMATIC UPDATES are FROZEN");
  else         _stat4->setCvar(" ");
*/
}






//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

