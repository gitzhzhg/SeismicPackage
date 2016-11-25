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
#include <stdio.h>
#include <assert.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>

#include "sp/seis_multiplot_control.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_plot_tie.hh"
#include "sp/seis_plot_under.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_select_pop.hh"
#include "sp/seis_tie_pop.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_zoomop_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "sp/seis_difference_pop.hh"
#include "sp/seis_lav_pop.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_arrow_scale.hh"
#include "sl/shell_watch.hh"
#include "sl/error_handler.hh"
#include "sl/colorset_collection.hh"
#include "vu/seis_ovjd.hh"

#include "cprim.h"


enum { ADD, DEL, MOD, DIFF };



static String  defres[]= {
    "_popup.titleString:     Multi-file Selection",
    "*add.labelString:       Add",
    "*add.labelString:       Add",
    "*del.labelString:       Delete",
    "*mod.labelString:       Modify",
    "*main.labelString:      Main Plot",
    "*diff.labelString:      Difference Plot...",
    "*tie.labelString:       Tie Plot",
    "*main.fontList:         -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*tie.fontList:          -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*XmScale.titleString:   File Movie Control",
    "*XmScale.scaleMultiple: 1",

    NULL};

static SLPush buttons[]  = {
 { "add",   ADD },
 { "del",   DEL },
 { "mod",   MOD },
};


static SLPush diff_button[]  = {
 { "diff",   DIFF },
};


SeisMultiPlotControl::SeisMultiPlotControl( Widget          p,
                                            char           *name,
                                            HelpCtx         hctx,
                                            SeisPlotTie    *sp,
                                            SeisPlot       *tie_sp,
                                            SeisSelectPop  *select_pop,
                                            SeisTiePop     *tie_pop,
                                            SeisColorPop   *color_pop,
                                            SeisZoomOpPop  *zoom_pop,
                                            SeisCbarPop    *cbar_pop,
                                            SeisOvjd       *ovjd,
                                            SeisLavPop     *lav_pop)
            : SLFPopSep(p,name,FP_DOREMOVE,hctx,False,False),
              SeisInform(sp), _sp(sp), _tie_sp(tie_sp),
              _prev_main_value(1), _prev_tie_value(1), 
              _select_pop(select_pop), _tie_pop(tie_pop),
              _color_pop(color_pop), _main_filelab(NULL),
              _tie_filelab(NULL), _master_sp(sp), _zoom_pop(zoom_pop),
              _cbar_pop(cbar_pop), _ovjd(ovjd), _lav_pop(lav_pop)
{
    setDefaultResources( p, name, defres);
    _main_push= new SLPushBox(this, "main_push", NULL, buttons,
                           XtNumber(buttons));

    _diff_push = new SLPushBox(this, "diff", NULL, diff_button,
                           XtNumber(diff_button));

    _tie_push=  new SLPushBox(this, "tie_push",  NULL, buttons,
                           XtNumber(buttons));
 
    _main_control= new SLArrowScale(this, "main_control", hctx, NULL, True,
                                    True, True);
    _tie_control=  new SLArrowScale(this, "tie_control",  hctx, NULL, True,
                                    True, True);
    _difference_control = new SeisDifferencePop(p, "diff_control",
                                    hctx, _main_control, this);

}


SeisMultiPlotControl::~SeisMultiPlotControl()
{
   delete _main_push;
   delete _tie_push;
   delete _main_control;
   delete _main_control;
   delete _difference_control;
}


Widget SeisMultiPlotControl::make(Widget p)
{

  if ( made() ) return topWidget();
  SLFPopSep::make(p);
  p= wParent();



  Widget main= XtVaCreateManagedWidget( "main", xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNtopOffset,       15,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      5,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     5,
                                   NULL);

  XtVaSetValues( _main_push->W(),  XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget,       main,
                                   XmNtopOffset,       15, 
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      15, 
                                   XmNorientation,     XmHORIZONTAL,
                                   NULL);

  _main_filelab= XtVaCreateManagedWidget("main_filelab", xmLabelWidgetClass,
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _main_push->W(),
                                   XmNtopOffset,       15,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      5,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     5,
                                   NULL);

  XtVaSetValues( _main_control->W(),  XmNtopAttachment,   XmATTACH_WIDGET,
                                      XmNtopWidget,       _main_filelab,
                                      XmNtopOffset,       15, 
                                      XmNleftAttachment,  XmATTACH_FORM,
                                      XmNleftOffset,      15, 
                                      NULL);

  XtVaSetValues( _diff_push->W(),     XmNtopAttachment, XmATTACH_WIDGET,
                                      XmNtopOffset,     15,
                                      XmNtopWidget,      _main_control->W(),
                                      XmNleftAttachment, XmATTACH_FORM,
                                      XmNleftOffset,     50,
                                      NULL);
  XtSetSensitive(_diff_push->W(), False);


  Widget sep= make_attached_sep(topWidget(), "sep");
  XtVaSetValues( sep,              XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopOffset,       15,
                                   XmNtopWidget,     _diff_push->W(),
                                   NULL);

  Widget tie= XtVaCreateManagedWidget( "tie", xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       sep,
                                   XmNtopOffset,       15,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      5,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     5,
                                   NULL);

  XtVaSetValues( _tie_push->W(),  XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget,       tie,
                                   XmNtopOffset,       15, 
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      15, 
                                   XmNorientation,     XmHORIZONTAL,
                                   NULL);

  _tie_filelab= XtVaCreateManagedWidget("tie_filelab", xmLabelWidgetClass,
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _tie_push->W(),
                                   XmNtopOffset,       15,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      5,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     5,
                                   NULL);

  XtVaSetValues( _tie_control->W(),  XmNtopAttachment,   XmATTACH_WIDGET,
                                     XmNtopWidget,       _tie_filelab,
                                     XmNtopOffset,       15, 
                                     XmNleftAttachment,  XmATTACH_FORM,
                                     XmNleftOffset,      15, 
                                     NULL);

  Widget tmp= XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                       XmNtopAttachment,    XmATTACH_WIDGET,
                                       XmNtopWidget,        _tie_control->W(),
                                       XmNbottomAttachment, XmATTACH_WIDGET,
                                       XmNbottomWidget,     bottomSeparator(),
                                       XmNleftAttachment,   XmATTACH_FORM,
                                       XmNleftOffset,       5,
                                       XmNtopOffset,        5,
                                       XmNbottomOffset,     25,
                                       NULL);

  _main_control->setRange(1,1);
  _tie_control->setRange(1,1);
  wprocShowMsg(_tie_filelab,  "Filename: none" );
  wprocShowMsg(_main_filelab, "Filename: none" );
  return topWidget();


}

void SeisMultiPlotControl::managing() {
    XtSetSensitive( _tie_push->pushW(DEL), _sp->doingTie() );

    if (_sp->getSeisWinMan()->count() == 1)
         XtSetSensitive( _main_push->pushW(DEL),False );
    else
         XtSetSensitive( _main_push->pushW(DEL),True );

    XtSetSensitive(_diff_push->W(), (_sp->getSeisWinMan()->count() > 1) );

    setFilename(_tie_filelab, _tie_sp);
    setFilename(_main_filelab, _sp);
}

Boolean SeisMultiPlotControl::notifyComplex(SLDelay *obj, int ident)
{
   if (obj == _main_push) {
         switch (ident) {
           case ADD:   _main_control->clearSelections(); 
                       _difference_control->clearSelections();
                       addNewMain();
                       break;
           case DEL:   _main_control->clearSelections();
                       _difference_control->clearSelections();
                       delMain(); 
                       break;
           case MOD:   _select_pop->makeAndManage(); break;
          } // end switch

   }
   else if (obj == _tie_push) {
         switch (ident) {
           case ADD:   _tie_control->clearSelections(); addNewTie(); break;
           case DEL:   _tie_control->clearSelections(); delTie(); break;
           case MOD:   ; _tie_pop->makeAndManage(); break;
          } // end switch

   }
   else if (obj == _main_control) {
      changeMain(ident);
   }
   else if (obj == _tie_control) {
      changeTie(ident);
   }
   else if (obj == _diff_push) {
     _difference_control->makeAndManage();
   }


   return True;
}

void SeisMultiPlotControl::addNewMain()
{
   SeisWinMan *widget_manager= _sp->getSeisWinMan();
   int units= _sp->units();
   if (!widget_manager->isLocked()) {
      if (_sp->isPlotDisplayed()) {
            SeisPlot *sp_under;
            Boolean do_under= (_sp->getChainedSP() != NULL);
            if (!widget_manager->inList(_master_sp)) {
              _sp = _master_sp;
              widget_manager->addSeisPlot (_sp);
              sp_under = _sp->getChainedSP ();
              if (sp_under) {
                sp_under->setUnits (units);
                if(_color_pop) _color_pop->addSP (sp_under);
              }
            }
            else {
/*
///////////////////// old //////////////////
              _sp = SeisPlotTie *sp= new SeisPlotTie(_sp);
///////////////////// old //////////////////
*/
///////////////////// new //////////////////
	      if (ColorsetCollection::readOnly(_sp->getColInfo())) {
		_sp = new SeisPlotTie (_master_sp);
	      }
	      else {
		_sp = new SeisPlotTie (_sp);
	      }
///////////////////// new //////////////////
              if(_color_pop) _color_pop->addSP(_sp);
              if(_color_pop) _color_pop->setColorOnSeisPlot(_sp);
              _cbar_pop->addSP(_sp);
              _zoom_pop->addControl(_sp);
              _ovjd->addSeisPlot(_sp);
	      _lav_pop->add (_sp);
              if (do_under) {
                sp_under = new SeisPlotUnder(_sp);
                sp_under->setUnits(units);
                if(_color_pop) _color_pop->addSP(sp_under);
                if(_color_pop) _color_pop->setColorOnSeisPlot(sp_under);
                _cbar_pop->addSP(sp_under);
                _zoom_pop->addControl(sp_under);
                _ovjd->addSeisPlot(sp_under);
	        _lav_pop->add (sp_under, SeisLavPop::MAIN_UNDER);
              }
            }
            _sp->setUnits(units);
	    _lav_pop->setCurrent (_sp);
            widget_manager->setCurrentSP(_sp);
            addSeisPlot(_sp);
            wprocShowMsg(_main_filelab,  "Filename: NEW" );
            _main_control->setRange(1,widget_manager->count());
            _main_control->setValue(widget_manager->count());
            if (widget_manager->count() > 1) {
              if (_main_push->made()) {
                         XtSetSensitive( _main_push->pushW(DEL), True );
                         XtSetSensitive( _diff_push->W(), True);
              }
            } // end if
      } // end if
      _select_pop->makeAndManage();
   } // end if
   else {
     ErrorHandler err= W();
     err.deliverError(widget_manager->getLockReason());
   } // end else

}

void SeisMultiPlotControl::addNewTie()
{
   SeisWinMan *widget_manager= _tie_sp->getSeisWinMan();
   int units= _sp->units();
   if (!widget_manager->isLocked()) {
      if (_tie_sp->isPlotDisplayed()) {
            SeisPlotUnder *sp_under;
            Boolean do_under= (_tie_sp->getChainedSP() != NULL);
            _tie_sp= new SeisPlot(_tie_sp);
            _tie_sp->setUnits(units);
            //Neccesary to get colors to share if not the first tie plot
            if(_color_pop) _color_pop->addSP(_tie_sp);
            if(_color_pop) _color_pop->setColorOnSeisPlot(_tie_sp);
            _cbar_pop->addSP(_tie_sp);
            _zoom_pop->addControl(_tie_sp);
            _ovjd->addSeisPlot(_tie_sp);
	    _lav_pop->add (_tie_sp, SeisLavPop::TIE);
            if (do_under) {
                  sp_under= new SeisPlotUnder(_tie_sp);
                  sp_under->setUnits(units);
                  if(_color_pop) _color_pop->addSP(sp_under);
                  if(_color_pop) _color_pop->setColorOnSeisPlot(_sp);
                  _cbar_pop->addSP(sp_under);
                  _zoom_pop->addControl(sp_under);
                  _ovjd->addSeisPlot(sp_under);
		  _lav_pop->add (sp_under, SeisLavPop::TIE_UNDER);
            }
	    _lav_pop->setCurrent (_tie_sp);
            widget_manager->setCurrentSP(_tie_sp);
            addSeisPlot(_tie_sp);
            wprocShowMsg(_tie_filelab,  "Filename: NEW" );
            _tie_control->setRange(1,widget_manager->count());
            _tie_control->setValue(widget_manager->count());
      } // end if
      _tie_pop->makeAndManage();
   }
   else {
      ErrorHandler err= W();
      err.deliverError(widget_manager->getLockReason());
   }
}

void SeisMultiPlotControl::delMain() 
{
  SeisWinMan *widget_manager= _sp->getSeisWinMan();
  if (widget_manager->count() > 1) {
     if (!widget_manager->isLocked()) {
         widget_manager->setSPNotCurrent(_sp);
         if (_sp == _master_sp) {
           _sp->cleanup ();
           widget_manager->delSeisPlot (_sp);
         }
         else {
           SeisPlot *under_sp = _sp->getChainedSP();
           if (under_sp)
             {
             if(_color_pop) _color_pop->removeSP(under_sp);
             _cbar_pop->removeSP(under_sp);
             _zoom_pop->removeControl(under_sp);
             _ovjd->delSeisPlot(under_sp);
             _lav_pop->remove (under_sp, SeisLavPop::MAIN_UNDER);
             delete under_sp;
             }
           if(_color_pop) _color_pop->removeSP(_sp);
           _cbar_pop->removeSP(_sp);
           _zoom_pop->removeControl(_sp);
           _ovjd->delSeisPlot(_sp);
	   _lav_pop->remove (_sp);
           delete _sp;
         }
         if (widget_manager->count() == 1) {
           if (_main_push->made()) {
             XtSetSensitive (_main_push->pushW(DEL),False);
           }
         }

         XtSetSensitive( _diff_push->W(),(widget_manager->count() > 1) );

         _sp= (SeisPlotTie *)widget_manager->currentSP();
         setFilename(_main_filelab, _sp);
     } // end if
     else {
         ErrorHandler err= W();
         err.deliverError(widget_manager->getLockReason());
     } // end else
  }  // end if
  else {
      printf("can't delete last one\n");
  }
  _main_control->setRange(1,widget_manager->count());
}

void SeisMultiPlotControl::delTie() 
{
  SeisWinMan *widget_manager= _tie_sp->getSeisWinMan();
  if (!widget_manager->isLocked()) {
     if (widget_manager->count() > 1) {
          widget_manager->setSPNotCurrent (_tie_sp);
          SeisPlot *under_sp=_tie_sp->getChainedSP();
          if(_color_pop) _color_pop->removeSP(under_sp);
          _cbar_pop->removeSP(under_sp);
          _zoom_pop->removeControl(under_sp);
          _ovjd->delSeisPlot(under_sp);
          _lav_pop->remove (under_sp, SeisLavPop::MAIN_UNDER);
          delete under_sp;
          if(_color_pop) _color_pop->removeSP(_tie_sp);
          _cbar_pop->removeSP(_tie_sp);
          _zoom_pop->removeControl(_tie_sp);
          _ovjd->delSeisPlot(_tie_sp);
          _lav_pop->remove (_tie_sp);
          delete _tie_sp;
          _tie_sp= (SeisPlotTie *)widget_manager->currentSP();
          setFilename(_tie_filelab, _tie_sp);
     } // end if
     else {
         _sp->delTie();
     } // end else
     if (_tie_control->made())
             _tie_control->setRange(1,widget_manager->count());
  } // end if
  else {           // locked
      ErrorHandler err= W();
      err.deliverError(widget_manager->getLockReason());
  } // end else
}





void SeisMultiPlotControl::changeMain(int val)
{
  SeisWinMan *widget_manager= _sp->getSeisWinMan();
  Boolean sensitize_main_delete = widget_manager->count() > 1;
  if (!widget_manager->isLocked()) {
      setCurrentSP(widget_manager, val);
      _prev_main_value= val;
      _sp= (SeisPlotTie*)widget_manager->currentSP();
      setFilename(_main_filelab, _sp);
      if (_main_push->made()) {
          XtSetSensitive (_main_push->pushW(DEL), sensitize_main_delete);
          XtSetSensitive( _diff_push->W(),(widget_manager->count() > 1) );
      }
  }
  else {
      ErrorHandler err= W();
      err.deliverError(widget_manager->getLockReason());
      _main_control->setValue(_prev_main_value); 
  }
}

void SeisMultiPlotControl::changeTie(int val)
{
  SeisWinMan *widget_manager= _tie_sp->getSeisWinMan();
  if (!widget_manager->isLocked()) {
      setCurrentSP(widget_manager, val);
      _prev_tie_value= val;
      _tie_sp= widget_manager->currentSP();
      setFilename(_tie_filelab, _tie_sp);
  } // end if
  else {
      ErrorHandler err= W();
      err.deliverError(widget_manager->getLockReason());
      _tie_control->setValue(_prev_main_value); 
  }
}


void SeisMultiPlotControl::newPlot(SeisPlot *sp)
{
   if (_main_filelab) setFilename(_main_filelab, sp);
}

void SeisMultiPlotControl::addingTie(SeisPlotTie *sp, SeisPlot *tiesp )
{
   setFilename(_main_filelab, sp);
   setFilename(_tie_filelab, tiesp);
   if (_tie_push->made()) XtSetSensitive( _tie_push->pushW(DEL),True );
}

void SeisMultiPlotControl::removingTie(SeisPlotTie *, SeisPlot *)
{
   setFilename(_tie_filelab, NULL);
   if (_tie_push->made()) XtSetSensitive( _tie_push->pushW(DEL),False );
}

void SeisMultiPlotControl::notCurrentInWindow(SeisPlot *)
{
}

void SeisMultiPlotControl::newSeisPlotCreatedInWindow(SeisPlot *)
{
}

void SeisMultiPlotControl::setFilename(Widget w, SeisPlot *sp)
{
   if (w) {
        if (!sp) {
               wprocShowMsg(w,  "Filename: none" );
        }
        else if (strlen(sp->filename()) == 0) {
               wprocShowMsg(w,  "Filename: none" );
        }
        else {
           char outstr[256];
           char junk[256];
           parse_file_(sp->filename(), outstr, junk);
           wprocVAShowMsg(w,  "Filename: %s", outstr );
           setMovieOrderFilenames();
        }
   } // end if

   
}

void SeisMultiPlotControl::setMovieOrderFilenames()
{
SeisWinMan *sp_manager    = _sp->getSeisWinMan();
SeisWinMan *tiesp_manager = _tie_sp->getSeisWinMan();
SeisPlot   *sp;
int        i;
char       outstr[256];
char       junk[256];
void       *p;

 for(sp = sp_manager->top(&p), i = 0; sp; sp = sp_manager->next(&p), i++)
   {
   parse_file_(sp->filename(), outstr, junk);
   _main_control->setFilename(i, outstr);
   }

 for(sp = tiesp_manager->top(&p), i = 0; sp; sp = tiesp_manager->next(&p), i++)
   {
   parse_file_(sp->filename(), outstr, junk);
   _tie_control->setFilename(i, outstr);
   }
  

}


SeisPlot *SeisMultiPlotControl::getSeisPlot(char *filename)
{
SeisPlot   *sp; 
SeisWinMan *sp_manager    = _sp->getSeisWinMan();
void       *p;
int        i;
char       outstr[256];
char       junk[256];

  for(sp = sp_manager->top(&p), i = 0; sp; sp = sp_manager->next(&p), i++)
   {
     parse_file_(sp->filename(), outstr, junk);
     if(strcmp(outstr,filename) == 0)
         return sp;
   }

  return NULL;
}

void SeisMultiPlotControl::setCurrentSP(SeisWinMan *mgr, int val)
{
SeisPlot *sp;
void *p;
int i;

  for(sp = mgr->top(&p), i = 0; sp; sp = mgr->next(&p), i++)
    {
    if(i + 1 == val)
      {
      _lav_pop->setCurrent (sp);
      mgr->setCurrentSP(sp);
      return;
      }
    }

  assert(0);

}
