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
// $Id: va_input_pop.cc,v 1.3 2004/08/05 13:08:58 cornkc Exp $
// $Name: new_file_choice $

#include "vaplots/va_input_pop.hh"
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_plot.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_grid_plot.hh"
#include "oprim/file_base.hh"
#include "vaplots/va_trace_file.hh"
#include "sl/sl_text_box.hh"
#include "sl/slp_file.hh"
#include "sl/sl_file_selection_pop.hh"
#include "sl/sl_option_menu.hh"
#include "sl/sl_file_choice.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/error_handler.hh"
#include "sl/error_handler.hh"
#include "sl/sl_centering_form.hh"
#include "sl/sl_form.hh"
#include "sl/prim_support.hh"
#include "sl/error_handler.hh"
#include "sl/shell_watch.hh"
#include "vf/vf_file_base.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "vfgui/vfgui_status.hh"
#include "oprim/file_base.hh"
#include <Xm/Label.h>
#include <Xm/XmP.h>




static String  defres[]= {
    "_popup.title:                   File Input",
    "*vactions.optionCount:     4",
    "*vactions.optionElement_0: add to velocity functions in active dataset",
    "*vactions.optionElement_1: replace velocity functions in active dataset",
    "*vactions.optionElement_2: put data into new comparison dataset",
    "*vactions.optionElement_3: Do not Re-read data",
    "*vactions.value:           add to velocity functions in active dataset",
    "*vorder.optionCount:       2",
    "*vorder.optionElement_0:   2",
    "*vorder.optionElement_1:   4",
    "*vorder.value:             2",

    "*file_part*gvs_file*Fileshell*dirMask:    *vs*.trc*",
    "*file_part*cmp_file*Fileshell*dirMask:    *svat.trc*",
    "*file_part*sem_file*Fileshell*dirMask:    *svas.trc*",
    "*vel_part*vel_file*Fileshell*dirMask:     *.vel",

    "*isem.labelString:   Irregular",
    "*csem.labelString:   Irregular",
    "*gsem.labelString:   Irregular",

    "*other_menus*orientation:     HORIZONTAL",
    "*other_menus*packing:         PACK_COLUMN",

    "*pfl.fontList:        -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*vfl.fontList:        -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*pfl.labelString:     Plot Files",
    "*vfl.labelString:     Velocity Files",

    "*iso.labelString:        ISO Velocity...",
    "*grid.labelString:       Grid...",
    "*semblance.labelString:  Semblance...",
    "*gvs.labelString:        GVS/CVST...",
    "*cmp.labelString:        CMP...",

    "*undo.labelString:       Undo",
    "*setall.labelString:     Set All Files",
    "*setall.set:             True",
    NULL};

enum { UNDO, H78, H87, H1718, H1817, H3738, H3837,
       ADDACT, REPLACEACT, NEWDATA, NOTHING, SETALL,
       ISOB, GRIDB, SEMB, GVSB, CMPB, ISEM, ICMP, IGVS };

#define HMM \
     "The Header words in the input file must match the current dataset."

static SLPush head_options[]  = {
 { "nhx: 7     nhy: 8",  H78   },
 { "nhx: 8     nhy: 7",  H87   },
 { "nhx: 17    nhy: 18", H1718 },
 { "nhx: 18    nhy: 17", H1817 },
 { "nhx: 37    nhy: 38", H3738 },
 { "nhx: 38    nhy: 37", H3837 },
};

static SLPush order_options[]  = {
 { "2",  2   },
 { "4",  4   },
};

static SLTog setall_togs[]  = {
         { "setall",  NULL, SETALL },
};

static SLPush vchoice_options[]  = {
 { "add to velocity functions in active dataset",   READ_ADD      },
 { "replace velocity functions in active dataset",  READ_REPLACE  },
 { "put data into new comparison dataset",          READ_NEW      },
 { "Do not Re-read data",                           READ_NOTHING  },
};

static SLPush buttons[]  = {
 { "undo",    UNDO },
};


static SLTog irregular_sem[] = {
  {"isem", NULL, ISEM },
};

static SLTog irregular_cmp[] = {
  {"csem", NULL, ICMP },
};

static SLTog irregular_gvs[] = {
  {"gsem", NULL, IGVS },
};


VaInputPop::VaInputPop( Widget             p,
                        char              *name,
                        HelpCtx            hctx,
                        ContainerList     *clist,
                        VaPlotControl     *plot_ctl)
              : SLFPopSep(p,name,FP_DOALL,hctx,False,False),
                VfInform(plot_ctl->manager()),
                _plot_ctl(plot_ctl), _first_time(True)
{


  setFallbackResources( (const char**)defres);
  _file_part= new  SLSmartForm(this, "file_part", hctx, True);
  _vel_part = new  SLSmartForm(this, "vel_part",  hctx, True);

  _gvs_file= new SLFileChoice( _file_part, "gvs_file", SLpFile::_INPUT, 
                               plot_ctl->gvsFile(),
                               "GVS File:", hctx, False);
  _gvs_file->getFileField()->resetPattern ("*vs*.");

  _cmp_file= new SLFileChoice( _file_part, "cmp_file", SLpFile::_INPUT, 
                               plot_ctl->cmpFile(),
                               "CMP File:", hctx, False);
  _cmp_file->getFileField()->resetPattern ("*svat.");

  _sem_file= new SLFileChoice( _file_part, "sem_file", SLpFile::_INPUT, 
                               plot_ctl->semFile(),
                               "SEM File:", hctx, False);
  _sem_file->getFileField()->resetPattern ("*svas.");

  _irregular_sem = new SLTogBox(_file_part, "isem", hctx,
                                 irregular_sem, XtNumber(irregular_sem));

  _irregular_cmp = new SLTogBox(_file_part, "csem", hctx,
                                 irregular_cmp, XtNumber(irregular_cmp));

  _irregular_gvs = new SLTogBox(_file_part, "igvs", hctx,
                                 irregular_gvs, XtNumber(irregular_gvs));

  _file_part->attach(_irregular_sem,  NULL,  _file_part, _file_part, NULL,
                        1, 5, 12, 5);
  _file_part->attach(_sem_file, _file_part, _irregular_sem, _file_part, NULL,
                        5, 1, 5, 5);

  _file_part->attach(_irregular_cmp,  NULL,  _file_part, _sem_file, NULL,
                        1, 5, 12, 5);
  _file_part->attach(_cmp_file, _file_part, _irregular_cmp, _sem_file, NULL,
                        5, 1, 5, 5);


  _file_part->attach(_irregular_gvs,  NULL,  _file_part, _cmp_file, NULL,
                        1, 5, 12, 5);
  _file_part->attach(_gvs_file, _file_part, _irregular_gvs, _cmp_file, NULL,
                        5, 1, 5, 5);





  _vel_file= new SLFileChoice( _vel_part, "vel_file", SLpFile::_INPUT, 
                                plot_ctl->velFile(), "VEL File:", hctx, False);

  _undo_box = new SLPushBox(_vel_part, "undo_box", hctx,
                                buttons, XtNumber(buttons));

  _status= new VfguiStatus(this, plot_ctl->manager(), clist, True);

  
  _vel_action_options = new SLOptionMenu(_vel_part, "vactions", hctx, 
                                 vchoice_options, XtNumber(vchoice_options) );

  _cform= new SLCenteringForm(this,"form", hctx);
  _header_options =     new SLOptionMenu(_cform, "headers",  hctx,
                                 head_options,    XtNumber(head_options) );
  _vorder_options =     new SLOptionMenu(_cform, "vorder",  hctx, 
                                 order_options,   XtNumber(order_options) );
  _setall= new SLTogBox( _cform, "psettings", hctx,
                            setall_togs, XtNumber(setall_togs));
  _other_menus = new SLPushBox(_cform, "other_menus", hctx, NULL, 0, True);
  _other_menus->addButton("iso", ISOB, _plot_ctl->iso()->getDialog() );
  _other_menus->addButton("grid", GRIDB, _plot_ctl->grid()->getDialog()  );
  _other_menus->addButton("semblance", SEMB, 
                          _plot_ctl->semblance()->getDialog() );
  _other_menus->addButton("gvs", GVSB, _plot_ctl->gvs()->getDialog() );
  _other_menus->addButton("cmp", CMPB, _plot_ctl->cmp()->getDialog() );
  _other_menus->setComplexNotify(this);

  _vel_part->attach(_vel_file, _vel_part, _vel_part, _vel_part, NULL,
                        5, 5, 5, 5);
  _vel_part->attach(_vel_action_options, _vel_part, NULL, _vel_file, NULL,
                        5, 5, 5, 5);
  _vel_part->attach(_undo_box, _vel_action_options, NULL, _vel_file, NULL,
                        30, 5, 5, 5);

  _gvs_file->setComplexNotify(this);
  _cmp_file->setComplexNotify(this);
  _sem_file->setComplexNotify(this);
  _vel_file->setComplexNotify(this);
  _undo_box->setComplexNotify(this);
  _irregular_sem->setComplexNotify(this);
  _irregular_cmp->setComplexNotify(this);
  _irregular_gvs->setComplexNotify(this);

  _vel_action_options->setComplexNotify(this);

  _vel_action_options->setLabel("Action: ");
  _header_options->setLabel("Headers: ");
  _vorder_options->setLabel("Order: ");

}


VaInputPop::~VaInputPop()
{
  delete _file_part;
  delete _vel_part;

  delete _gvs_file->getFileBase();
  delete _gvs_file;

  delete _sem_file->getFileBase();
  delete _sem_file;

  delete _cmp_file->getFileBase();
  delete _cmp_file;

  delete _vel_file->getFileBase();
  delete _vel_file;

  delete _vel_action_options;
  delete _header_options;

  delete _status;
  delete _undo_box;
}




Widget VaInputPop::make(Widget p)
{
   if ( made() ) return topWidget();
   SLFPopSep::make(p);
   p= wParent();

   Widget pfl=  XtVaCreateManagedWidget( "pfl", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,  XmATTACH_FORM,
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopOffset,      5,
                                   XmNleftOffset,     5,
                                   XmNrightOffset,    5,
                                   NULL);


   XtVaSetValues( _file_part->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       pfl,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopOffset,       5,
                                   XmNleftOffset,      5,
                                   XmNrightOffset,     5,
                                   NULL);

   Widget sep= make_attached_sep(topWidget(), "sep");
   XtVaSetValues( sep, XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget,     _file_part->W(),
                       XmNtopOffset,     5, NULL);


   Widget vfl=  XtVaCreateManagedWidget( "vfl", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       sep,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopOffset,      30,
                                   XmNleftOffset,     5,
                                   XmNrightOffset,    5,
                                   NULL);

   XtVaSetValues( _vel_part->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                                  XmNtopWidget,       vfl,
                                  XmNleftAttachment,  XmATTACH_FORM,
                                  XmNrightAttachment, XmATTACH_FORM,
                                  XmNbottomAttachment,   XmATTACH_NONE,
                                  XmNtopOffset,       5,
                                  XmNleftOffset,      5,
                                  XmNrightOffset,     5,
                                  NULL);

   XtVaSetValues( _cform->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _vel_part->W(),
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNrightAttachment, XmATTACH_FORM,
                               XmNbottomAttachment,XmATTACH_NONE,
                               XmNtopOffset,       25,
                               XmNleftOffset,      5,
                               XmNrightOffset,     5,
                               NULL);

   XtVaSetValues( _header_options->W(), XmNtopAttachment,   XmATTACH_FORM,
                                        XmNleftAttachment,  XmATTACH_WIDGET,
                                        XmNbottomAttachment,XmATTACH_NONE,
                                        XmNtopOffset,       5,
                                        XmNleftOffset,      5,
                                        NULL);

   XtVaSetValues( _vorder_options->W(), 
                           XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,       _header_options->W(),
                           XmNleftAttachment,  XmATTACH_WIDGET,
                           XmNleftWidget,       _header_options->W(),
                           XmNleftOffset,      40,
                           NULL);
 

   XtVaSetValues( _setall->W(), 
                           XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,       _header_options->W(),
                           XmNleftAttachment,  XmATTACH_WIDGET,
                           XmNleftWidget,       _vorder_options->W(),
                           XmNleftOffset,      40,
                           NULL);

   XtVaSetValues( _other_menus->W(),
                           XmNtopAttachment,   XmATTACH_WIDGET,
                           XmNtopWidget,       _header_options->W(),
                           XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                           XmNleftWidget,       _header_options->W(),
                           XmNtopOffset,       10,
                           NULL );

   XtVaSetValues( _status->W(), 
                           XmNtopAttachment,   XmATTACH_WIDGET,
                           XmNtopWidget,       _cform->W(),
                           XmNleftAttachment,  XmATTACH_FORM,
                           XmNrightAttachment, XmATTACH_FORM,
                           XmNbottomAttachment,XmATTACH_NONE,
                           XmNtopOffset,       10,
                           NULL);

   Widget tmp=  XtVaCreateManagedWidget( "", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _status->W(),
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNbottomAttachment,XmATTACH_WIDGET,
                                   XmNbottomWidget,    bottomSeparator(),
                                   XmNtopOffset,      5,
                                   XmNleftOffset,     5,
                                   XmNbottomOffset,   25,
                                   NULL);


  return topWidget();
}

void VaInputPop::DoAction()
{
  _plot_ctl->velFile()->setReadChoice(_vel_action_options->whichSelected());

  setHeaderWordsAndOrder();

  if ( (_vel_file->getFileBase()->inputValidity() == FileBase::VALID_YES) &&
       (_vel_action_options->whichSelected() != READ_NOTHING) )
             _vel_file->takeAction(this, FP_APPLY);

  if (_gvs_file->getFileBase()->inputValidity() == FileBase::VALID_YES)
             _gvs_file->takeAction(this, FP_APPLY);
  if (_cmp_file->getFileBase()->inputValidity() == FileBase::VALID_YES)
             _cmp_file->takeAction(this, FP_APPLY);
  if (_sem_file->getFileBase()->inputValidity() == FileBase::VALID_YES)
             _sem_file->takeAction(this, FP_APPLY);

  VaPlotControl::PlotStat succ= _plot_ctl->plot(VaPlotControl::ALLPLOTS);

  if (succ != VaPlotControl::VaSuccess) {
         manage();
         ErrorHandler err= W();
         err.setEtype(ErrorHandler::Error, ErrorHandler::CUI, False);
         char *errstr= _plot_ctl->getFailString();
         err.deliverError(errstr);
  }
  managing();
}

void VaInputPop::doPlot()
{

  _plot_ctl->velFile()->setReadChoice(_vel_action_options->whichSelected());
  setHeaderWordsAndOrder();
  if (_gvs_file->getFileBase()->inputValidity() == FileBase::VALID_YES)
             _gvs_file->takeAction(this, FP_APPLY);
  if (_cmp_file->getFileBase()->inputValidity() == FileBase::VALID_YES)
             _cmp_file->takeAction(this, FP_APPLY);
  if (_sem_file->getFileBase()->inputValidity() == FileBase::VALID_YES)
             _sem_file->takeAction(this, FP_APPLY);

  VaPlotControl::PlotStat succ= _plot_ctl->plot(VaPlotControl::ALLPLOTS);

  if ( (_vel_file->getFileBase()->inputValidity() == FileBase::VALID_YES) &&
       (_vel_action_options->whichSelected() != READ_NOTHING) )
             _vel_file->takeAction(this, FP_APPLY);

  if (succ != VaPlotControl::VaSuccess) {
         manage();
         ErrorHandler err= W();
         err.setEtype(ErrorHandler::Error, ErrorHandler::CUI, False);
         char *errstr= _plot_ctl->getFailString();
         err.deliverError(errstr);
  }
  managing();
}



void VaInputPop::setHeaderWordsAndOrder()
{
  VfDataset *dataset= _plot_ctl->manager()->activeDataset(); 
  int oldx= dataset->getNhx();
  int oldy= dataset->getNhy();
  if (_header_options->whichSelected() == H78)
  {
     dataset->setNhx(7);
     dataset->setNhy(8);
     if ((oldx != 7) && (oldy != 8)) _plot_ctl->setReplotAll();
  }
  else if (_header_options->whichSelected() == H87)
  {
     dataset->setNhx(8);
     dataset->setNhy(7);
     if ((oldx != 8) && (oldy != 7)) _plot_ctl->setReplotAll();
  }
  else if (_header_options->whichSelected() == H1718)
  {
     dataset->setNhx(17);
     dataset->setNhy(18);
     if ((oldx != 17) && (oldy != 18)) _plot_ctl->setReplotAll();
  }
  else if (_header_options->whichSelected() == H1817)
  {
     dataset->setNhx(18);
     dataset->setNhy(17);
     if ((oldx != 18) && (oldy != 17)) _plot_ctl->setReplotAll();
  }
  else if (_header_options->whichSelected() == H3738)
  {
     dataset->setNhx(37);
     dataset->setNhy(38);
     if ((oldx != 37) && (oldy != 38)) _plot_ctl->setReplotAll();
  }
  else if (_header_options->whichSelected() == H3837)
  {
     dataset->setNhx(38);
     dataset->setNhy(37);
     if ((oldx != 38) && (oldy != 37)) _plot_ctl->setReplotAll();
  }
  else assert(0);

  _plot_ctl->manager()->activeDataset()->setMoveoutOrder(
                   _vorder_options->whichSelected());
 
}

void VaInputPop::getHeaderWordsAndOrder(Boolean from_file)
{
  int x;
  int y;
  int order;

  if (from_file  &&
     (_vel_file->getFileBase()->inputValidity() == FileBase::VALID_YES)){
       VfFileBase *vel_file= _plot_ctl->velFile();
       x= vel_file->getNhx();
       y= vel_file->getNhy();
       order= vel_file->getMoveoutOrder();
  }
  else {
       VfDataset *dataset= _plot_ctl->manager()->activeDataset(); 
       x= dataset->getNhx();
       y= dataset->getNhy();
       order= dataset->getMoveoutOrder();
  }

  if      ((x==7)  && (y==8))   _header_options->setButton(H78);
  else if ((x==8)  && (y==7))   _header_options->setButton(H87);
  else if ((x==17) && (y==18))  _header_options->setButton(H1718);
  else if ((x==18) && (y==17))  _header_options->setButton(H1817);
  else if ((x==37) && (y==38))  _header_options->setButton(H3738);
  else if ((x==38) && (y==37))  _header_options->setButton(H3837);
  else assert(0);

  _vorder_options->setButton(order);

}

void VaInputPop::UndoInput()
{
}

Boolean VaInputPop::ValidInput()
{
  VfDataset *dataset= _plot_ctl->manager()->activeDataset(); 
  int x= dataset->getNhx();
  int y= dataset->getNhy();
  Boolean retval= True;
  if (_vel_action_options->whichSelected() != READ_NOTHING) {
      if ( ( (_plot_ctl->velFile()->getNhx()     == x)  &&
             (_plot_ctl->velFile()->getNhy()     == y) ) || 
           (_vel_action_options->whichSelected() == READ_REPLACE) ) {
                  retval= True;
      }
      else {
                  retval= False;
                  ErrorHandler err= W();
                  err.setEtype(ErrorHandler::Error, ErrorHandler::CUI, False);
                  err.deliverError( HMM );
      }
  } // end if

  return retval;
}

void VaInputPop::managing()
{
  updateFiles();
  _vel_action_options->setButton(_plot_ctl->velFile()->getReadChoice());
  getHeaderWordsAndOrder(False);
  _setall->SetTog(SETALL, True);
  _iso_man= False;
  _grid_man= False;
  _sem_man= False;
  _gvs_man= False;
  _cmp_man= False;
  checkDialogs();
   _plot_ctl->manager()->maybeDeleteUndoFiles(_plot_ctl->velFile());
  updateUndo();
  if (_first_time) {
      _first_time= False;
      checkForDefaults();
  }
}

void VaInputPop::checkForDefaults()
{
/*
 *wprocFileChoiceValidate(_sem_file->getFileField()->W(), True);
 *wprocFileChoiceValidate(_cmp_file->getFileField()->W(), True);
 *wprocFileChoiceValidate(_gvs_file->getFileField()->W(), True);
 *wprocFileChoiceValidate(_vel_file->getFileField()->W(), True);
 */
}

void VaInputPop::updateDirs(SLFileChoice *file)
{
  if (file->getFileField()->filebox()) {
    char *dir = file->getFileField()->filebox()->directory ();
    _sem_file->getFileField()->filebox()->setDirectory (dir);
    _cmp_file->getFileField()->filebox()->setDirectory (dir);
    _gvs_file->getFileField()->filebox()->setDirectory (dir);
    _vel_file->getFileField()->filebox()->setDirectory (dir);
  }
}

void VaInputPop::updateFiles()
{
  _sem_file->updateFieldsFromFileBase();
  _cmp_file->updateFieldsFromFileBase();
  _vel_file->updateFieldsFromFileBase();
  _gvs_file->updateFieldsFromFileBase();
}

Boolean VaInputPop::notifyComplex(SLDelay *obj, int ident)
{
char junk[200];

  if ((obj == _sem_file) && (ident == FileBase::VALID_YES))
       updateAllFiles(_sem_file);
  else if ((obj == _cmp_file)  && (ident == FileBase::VALID_YES))
       updateAllFiles(_cmp_file);
  else if ((obj == _vel_file)  && (ident == FileBase::VALID_YES)) {
       updateAllFiles(_vel_file);
  }
  else if ((obj == _gvs_file)  && (ident == FileBase::VALID_YES))
       updateAllFiles(_gvs_file);
  else if (obj == _other_menus)
       updateButtons(ident);
  else if (obj == _undo_box) {
       _plot_ctl->manager()->activeDataset()->maybeReadUndoFile(
                                               _plot_ctl->velFile());
       PrimSupport::updateEverything();
  }
  else if (obj == _header_options)  {
  }
  else if (obj == _vel_action_options)  {
      int read_choice= _vel_action_options->whichSelected();
      if (read_choice == READ_REPLACE)  getHeaderWordsAndOrder(True);
      else                              getHeaderWordsAndOrder(False);
  }
  else if (obj == _irregular_sem) {
      _plot_ctl->semblance()->SP()->setSelectorParameters(
                                            _irregular_sem->IsSelected(ISEM));
      if(strlen(_plot_ctl->semblance()->SP()->filename()))
        {
        ShellWatch watch= topWidget();
        _plot_ctl->semblance()->getFileParameters(
                                    _plot_ctl->semblance()->SP()->filename());
        _plot_ctl->semFile()->setInformationLabel(
                                     _plot_ctl->semblance()->getFileLabel());
        _sem_file->updateFields();
        }
  }
  else if (obj == _irregular_cmp) {
      _plot_ctl->cmp()->SP()->setSelectorParameters(
                                            _irregular_cmp->IsSelected(ICMP));
      if(strlen(_plot_ctl->cmp()->SP()->filename()))
        {
        ShellWatch watch= topWidget();
        _plot_ctl->cmp()->getFileParameters(
                                    _plot_ctl->cmp()->SP()->filename());
        _plot_ctl->cmpFile()->setInformationLabel(
                                     _plot_ctl->cmp()->getFileLabel());
        _cmp_file->updateFields();
        }
      
  }
  else if (obj == _irregular_gvs) {
      _plot_ctl->gvs()->SP()->setSelectorParameters(
                                            _irregular_gvs->IsSelected(IGVS));
      
      if(strlen(_plot_ctl->gvs()->SP()->filename()))
        {
        ShellWatch watch= topWidget();
        _plot_ctl->gvs()->getFileParameters(
                                    _plot_ctl->gvs()->SP()->filename());
        _plot_ctl->gvsFile()->setInformationLabel(
                                     _plot_ctl->gvs()->getFileLabel());
        _gvs_file->updateFields();
        }
  }


  return (True);
}

void VaInputPop::updateButtons(int ident) 
{
  switch (ident) {
     case ISOB:    _iso_man= True;  break;
     case GRIDB:   _grid_man= True; break;
     case SEMB:    _sem_man= True;  break;
     case GVSB:    _gvs_man= True;  break;
     case CMPB:    _cmp_man= True;  break;
     default:      assert(0);       break;
  } // end switch
}

void VaInputPop::checkDialogs() 
{
  if (!XtIsManaged(W())) {
       if (_iso_man)   _plot_ctl->iso()->getDialog()->unmanage();
       if (_grid_man)  _plot_ctl->grid()->getDialog()->unmanage();
       if (_sem_man)   _plot_ctl->semblance()->getDialog()->unmanage();
       if (_gvs_man)   _plot_ctl->gvs()->getDialog()->unmanage();
       if (_cmp_man)   _plot_ctl->cmp()->getDialog()->unmanage();
  }
}

void VaInputPop::updateAllFiles(SLFileChoice *file) 
{
  updateDirs(file);
  if (_setall->IsSelected(SETALL) ) {
        _plot_ctl->updateOthers(file->getFileBase());
        updateFiles();
  } // end if

  int read_choice= _plot_ctl->velFile()->getReadChoice();
  if (read_choice == READ_REPLACE)  getHeaderWordsAndOrder(True);
  else                              getHeaderWordsAndOrder(False);
  _vel_action_options->setButton(read_choice);


  getHeaderWordsAndOrder(True);
  if (_plot_ctl->manager()->activeDataset()->numVelocityFunctions() == 0)
          setHeaderWordsAndOrder();
}

void VaInputPop::updateUndo()
{
  _undo_box->setSensitivity(
         _plot_ctl->manager()->activeDataset()->allowReadDeleteUndoFile(
                       _plot_ctl->velFile()));
}

void VaInputPop::postTotalChanges(VfDataset *) {}

void VaInputPop::afterChanges()
{
  updateUndo();
}
void VaInputPop::postNewActiveDataset()
{
  updateUndo();
}



void VaInputPop::setSemFile(char *filename)
{
   _plot_ctl->semFile()->setInputFilename(filename);
   _sem_file->updateFieldsFromFileBase();
   updateAllFiles(_sem_file);
   notifyComplex(_sem_file, 0);
}

void VaInputPop::setCMPFile(char *filename)
{
   _plot_ctl->cmpFile()->setInputFilename(filename);
   _cmp_file->updateFieldsFromFileBase();
   updateAllFiles(_cmp_file);
   notifyComplex(_cmp_file, 0);
}

void VaInputPop::setGVSFile(char *filename)
{
   _plot_ctl->gvsFile()->setInputFilename(filename);
   _gvs_file->updateFieldsFromFileBase();
   updateAllFiles(_gvs_file);
   notifyComplex(_gvs_file, 0);
}

void VaInputPop::setVelFile(char *filename)
{
   _plot_ctl->velFile()->setInputFilename(filename);
   _vel_file->updateFieldsFromFileBase();
   updateAllFiles(_vel_file);
   notifyComplex(_vel_file, 0);
}

void VaInputPop::plotNow()
{
  make();
  doPlot();
}
