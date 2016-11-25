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
#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include "sp/seis_lav_pop.hh"
#include "sp/seis_lav_gui.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_radio_box.hh"

static String  defres[]= {
     "_popup.title:                    Find Seismic LAV",
     "*donotfindlav.labelString:       Do not find Largest Absolute Value",
     "*findlavbyfile.labelString:      Find Largest Absolute Value by File",
     "*findlavbypanel.labelString:     Find Largest Absolute Value by Panel",
     "*findlavtypeframe.topPosition:   10",
     "*donotfindlav.set:               True",
     "*findlavtype_Frame.leftPosition: 20",
    NULL};

static SLRadio radios[]  = {
  { "donotfindlav",   SeisPlot::NOLAV, },
//  { "findlavbyfile",  SeisPlot::LAVBYFILE, },
  { "findlavbypanel", SeisPlot::LAVBYPANEL, },
};

#define FINDLAVTYPEFRAME "findlavtypeframe"

SeisLavPop::SeisLavPop (Widget p, char *name, HelpCtx hctx) :
  SLFPopSep (p, name, FP_DOREMOVE, hctx, True, False)
{
  //PsuedoWidget *tpw;
  Display *dpy = XtDisplay (p);

  setDefaultResources (p, name, defres);

  _lavtype = new SLRadioBox (this, "findlavtype", getHelpCtx(), radios,
    XtNumber(radios), NULL, True);

  int k2;
  for (k2 = 0; k2 < COUNT; k2++) {
    _informs  [k2] = new SeisLavInform (this);
    _sps      [k2] = NULL;
    _filenames[k2] = NULL;
    _lavs     [k2] = -1.0;
  }

  _current_type = _lavtype->WhichSelected ();
}

SeisLavPop::~SeisLavPop ()
{
/*
// tried to be conscientious and clean up but the following caused a crash
//   in ~CbytWindow!
  int k2;
  for (k2 = 0; k2 < COUNT; k2++) {
    delete _informs[k2];
  }
  delete _informs;
*/
}

void SeisLavPop::add (SeisPlot *sp, int which)
{
  assert (which > UNSET && which < COUNT);
  if (sp) {
    _informs[which]->addSeisPlot (sp);
    if (_sps[which] == NULL) _sps[which] = sp;
  }
}

void SeisLavPop::remove (SeisPlot *sp, int which)
{
  assert (which > UNSET && which < COUNT);
  if (sp) {
    _informs[which]->delSeisPlot (sp);
    if (_sps[which] == sp) {
      _sps      [which] = NULL;
      _filenames[which] = NULL;
      _lavs     [which] = -1.0;
    }
  }
}

void SeisLavPop::setCurrent (SeisPlot *sp)
{
  int type = which (sp);
  assert (type != UNSET);
  _sps[type] = sp;
}

void SeisLavPop::clearCurrent (SeisPlot *sp)
{
  int type = which (sp);
  if (type != UNSET) {

  // if given a MAIN or TIE overlay, reset the underlays
    if (type == MAIN) {
      clearCurrent (MAIN_UNDER);
    }
    else if (type == TIE) {
      clearCurrent (TIE_UNDER);
    }

    if (_sps[type] == sp) {
      _filenames[type] = NULL;
      _lavs     [type] = -1.0;
    }
  }
}

void SeisLavPop::clearCurrent (int which)
{
  assert (which > UNSET && which < COUNT);
  _filenames[which] = NULL;
  _lavs     [which] = -1.0;
}

int SeisLavPop::which (SeisPlot *sp)
{
  int retval;

  if      (_informs[MAIN      ]->find(sp)) retval = MAIN;
  else if (_informs[MAIN_UNDER]->find(sp)) retval = MAIN_UNDER;
  else if (_informs[TIE       ]->find(sp)) retval = TIE;
  else if (_informs[TIE_UNDER ]->find(sp)) retval = TIE_UNDER;
  else                                     retval = UNSET;

  return retval;
}

Widget SeisLavPop::make (Widget p)
{
   if (made()) return topWidget ();
   p = p ? p : wParent ();
   ShellStatMsg bld_info (p,"Building Find LAV Popup...");
   SLFPopSep::make (p);

   XtVaSetValues (_lavtype->W(), XmNtopAttachment,   XmATTACH_FORM,
                                 XmNtopOffset,       30,
                                 XmNleftAttachment,  XmATTACH_POSITION, NULL);
   _lavtype->setComplexNotify (this);

   // create widget to show filename & LAV for up to COUNT results
   //   a make is done in the constructor of SeisLavGui
   _lav_gui = new SeisLavGui (topWidget(), "lavgui", getHelpCtx(), this);

   XtVaSetValues (_lav_gui->W(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _lavtype->W(),
		               XmNleftAttachment,   XmATTACH_POSITION, NULL);

   Widget tmp1 = XtVaCreateManagedWidget ("", xmLabelWidgetClass, topWidget(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _lav_gui->W(),
                               XmNtopOffset,        5,
                               XmNbottomAttachment, XmATTACH_WIDGET,
                               XmNbottomWidget,     bottomSeparator(),
                               XmNbottomOffset,     5,
                               NULL);

   //defaultButtonOK (True);
   return topWidget ();
}

int SeisLavPop::count ()
{
  return COUNT;
}

int SeisLavPop::type ()
{
  return (int)_current_type;
}

void SeisLavPop::setLav (SeisPlot *sp)
{
  int type = which (sp);
  assert (type != UNSET);
  if (_sps[type] && _sps[type] != sp) {
    // clear entry
    clearCurrent (sp);
    // caught in transition; go ahead and set current
    //   this is to keep SeisWinMan::setCurrentSP ignorant of SeisLavPop
    setCurrent (sp);
  }

  // if given a MAIN or TIE overlay, reset the underlays
  if (type == MAIN) {
    clearCurrent (MAIN_UNDER);
  }
  else if (type == TIE) {
    clearCurrent (TIE_UNDER);
  }

  double lav = sp->findLav (_current_type);
  _filenames[type] = sp->filename ();
  _lavs     [type] = lav;

  // set the new underlay if necessary
  SeisPlot *sp_under = sp->getChainedSP ();
  if (sp_under != NULL) {
    setLav (sp_under);
  }
}

void SeisLavPop::manage ()
{
  SLBase::manage();
  _lavtype->SetRadio (_current_type);
  display ();
}

void SeisLavPop::display ()
{
  if (!made()) return;
  int k2;
  for (k2 = 0; k2 < 4; k2 += 2) {
    if (_current_type  != SeisPlot::NOLAV &&
        _sps      [k2] != NULL            &&
        _filenames[k2] != NULL            &&
        _lavs     [k2] >= 0.0               ) { // manage left one
      _lav_gui->setName  (k2,  _filenames[k2]);
      _lav_gui->setValue (k2, &_lavs[k2]     );
    }
    else { // unmanage left one
      _lav_gui->clear (k2);
    }

    if (_current_type    != SeisPlot::NOLAV &&
        _sps      [k2+1] != NULL            &&
        _filenames[k2+1] != NULL            &&
        _lavs     [k2+1] >= 0.0               ) { // manage right one
      _lav_gui->setName  (k2+1,  _filenames[k2+1]);
      _lav_gui->setValue (k2+1, &_lavs[k2+1]     );
    }
    else { // unmanage right one
      _lav_gui->clear (k2+1);
    }
  }
}

Boolean SeisLavPop::notifyComplex (SLDelay * /*obj*/, int /*ident*/)
{
  _current_type = _lavtype->WhichSelected ();
  initialize ();
  display ();
  return True;
}

void SeisLavPop::reloadDefaults (Boolean /*do_method*/)
{
  _lavtype->reloadDefaults ();
  display ();
}

void SeisLavPop::reloadSystemDefaults (Boolean /*do_method*/)
{
  _lavtype->SetRadio (SeisPlot::NOLAV);
  display ();
}

void SeisLavPop::initialize ()
{
  if (_current_type == SeisPlot::NOLAV) return;

  void *p;
  SeisPlot *sp;
  double lav;
  int k2;
  for (k2 = 0; k2 < 4; k2++) {
    if (_sps      [k2] != NULL &&
        _filenames[k2] != NULL &&
        _lavs     [k2] <  0.0    ) {
      _lavs[k2] = _sps[k2]->findLav (_current_type);
    }
  }
}

SeisLavInform::SeisLavInform (SeisLavPop *lav_pop) :
  SeisInform (),
  _lav_pop  (lav_pop)
{
}

void SeisLavInform::newPlot (SeisPlot *sp)
{
  update (sp);
}

void SeisLavInform::postScan (SeisPlot *sp, SeisPlot::ScanDir)
{
  update (sp);
}

void SeisLavInform::notCurrentInWindow (SeisPlot *sp)
{
  _lav_pop->clearCurrent (sp);
  _lav_pop->display ();
}

void SeisLavInform::nowCurrentInWindow (SeisPlot *sp)
{
  update (sp);
}

void SeisLavInform::removingTie (SeisPlotTie * /*sp*/, SeisPlot *tie_sp)
{
  _lav_pop->clearCurrent (SeisLavPop::TIE);
  _lav_pop->clearCurrent (SeisLavPop::TIE_UNDER);
  _lav_pop->display ();
}

void SeisLavInform::postMovie (SeisPlot *sp, SeisPlot::MovieDir)
{
  update (sp);
}

void SeisLavInform::update (SeisPlot *sp)
{
  _lav_pop->setLav (sp);
  _lav_pop->display ();
}
