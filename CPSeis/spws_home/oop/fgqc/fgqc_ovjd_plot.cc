//************************************************************************
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
//***                         The QC Ovjd Plot Class                   ***
//***                       Michael L. Sherrill 10/95                  ***
//************************************************************************

#include <Xm/Separator.h>
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_app.hh"
#include "sl/sl_bb.hh"
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "sp/seis_loc_out.hh"
#include "fgqc/fgqc_ovjd_plot.hh"
#include "fgqc/fgqc_ovjd_pop.hh"
#include "vu/seis_ovjd.hh"
#include "vu/header_dump_pop.hh"
#include "fgqc/fgseis_shift.hh"
#include "pick/seis_shift_pop.hh"
#include "named_constants.h"


#define ParentClass SLFormHelpPop
#define OFFSET_HEADER 5
#define KILLED_HEADER 25
#define SMALL_OFFSET .001


static String  defres[]= 
    {
    ".resizePolicy:              RESIZE_NONE",
    "*menu_op.labelString:       Setup/Change Plot...",
    "*ovjd_op.labelString:       Ovjd Options...",
    "*color_op.labelString:      Color Options...",
    "*cbar_op.labelString:       Color Bar",
    "*zoom_separate.labelString: Zoom Separate Window",
    "*zoom_up.labelString:       Zoom Up",
    "*zoom_down.labelString:     Zoom Down",
    "*zoom_original.labelString: Original Size",
    "*header_dump.labelString:   Header Dump",
    "*shift_op.labelString:      Shift Options...",
    NULL
    };

enum {MENU_OP, OVJD_OP, COLOR_OP, CBAR_OP, ZOOMUP, ZOOMDOWN, ZOOMORIGINAL, 
      ZOOMUPSEPARATE, MENU_POP, OPTIONS = 12, HEADER_DUMP, SHIFT_OP};

static SLPush buttons[]  = 
    {
      { "menu_op",       MENU_OP},
      { "ovjd_op",       OVJD_OP},
      { "color_op",      COLOR_OP},
      { "cbar_op",       CBAR_OP },
      { "header_dump",   HEADER_DUMP },
      { "shift_op",      SHIFT_OP },
      { "zoom_separate", ZOOMUPSEPARATE},
      { "zoom_up",       ZOOMUP },
      { "zoom_down",     ZOOMDOWN },
      { "zoom_original", ZOOMORIGINAL },
    };


//============================================================================
//====================== Constructor for own window  =========================
//============================================================================
FgQcOvjdPlot::FgQcOvjdPlot(Widget               p,
                           char                 *name,
                           HelpCtx              hctx, 
                           class FieldGeometry  *fg,
                           class FgSeisPlotList *fgsp_list,
                           int                  numcolors)
               : SLFormHelpPop(p, name, FP_DOHELP | FP_DOREMOVE,
	                       hctx, True, 2, False, True, numcolors, True),
                               FgInform(fg),_fg(fg),  _hctx(hctx),
                               _fgsp_list(fgsp_list), _numcolors(numcolors)
{

  setDefaultResources( p, name, defres);
  addExtraButton("Options...",OPTIONS);

  _changing        = False;
  _inform_action   = NO_ACTION;
  _first_time      = True;
  _sp              = NULL;
  _header_dump_pop = NULL;
  _cbar_pop        = NULL;
  _options_menu     = NULL;
  _color_pop       = NULL;
  _cbar_pop        = NULL;
  _pop             = NULL;
  _ovjd            = NULL;
  _ovjd_pop        = NULL;
  _header_dump_pop = NULL;
  _shift_pop       = NULL;
  _control_area    = NULL;
}

//============================================================================
//====================== Constructor for application window ==================
//============================================================================
FgQcOvjdPlot::FgQcOvjdPlot(Widget               p,
                           char                 *name,
                           HelpCtx              hctx, 
                           SLApp                *app,
                           class FieldGeometry  *fg,
                           class FgSeisPlotList *fgsp_list,
                           int                  numcolors)
                           : SLFormHelpPop(p, name, FP_DOHELP | FP_DOREMOVE,
	                              hctx, False, 2, False, False, numcolors),
                           FgInform(fg),_fg(fg),  _hctx(hctx),
                          _fgsp_list(fgsp_list), _numcolors(numcolors), 
                          _app(app)
{

  setDefaultResources( p, name, defres);

  _pulldown= new SLPullPop("Seismic", NULL, app);
  _pulldown->addPush("Seismic Menu", MENU_POP);
  _pulldown->addPush("Color Options...", COLOR_OP);
  _pulldown->addPush("Color Bar", CBAR_OP);
  _pulldown->addPush("Header Dump", HEADER_DUMP);
  _pulldown->setComplexNotify(this);

  _changing        = False;
  _inform_action   = NO_ACTION;
  _first_time      = True;
  _sp              = NULL;
  _header_dump_pop = NULL;
  _cbar_pop        = NULL;
  _options_menu    = NULL;
  _color_pop       = NULL;
  _cbar_pop        = NULL;
  _pop             = NULL;
  _ovjd            = NULL;
  _ovjd_pop        = NULL;
  _shift_pop       = NULL;
  _control_area    = NULL;

  make(p);

  unmanage();
}
//============================================================================
//====================== Destructor               =========================
//============================================================================
FgQcOvjdPlot::~FgQcOvjdPlot()
{

  if(_color_pop)          delete _color_pop;
  if(_cbar_pop)           delete _cbar_pop;
  if(_options_menu)       delete _options_menu;
  if(_sp)                 delete _sp;
  if(_header_dump_pop)    delete _header_dump_pop;
  if(_pop)                delete _pop;
  if(_ovjd)               delete _ovjd;
  if(_ovjd_pop)           delete _ovjd_pop;
  if(_shift_pop)          delete _shift_pop;
  if(_control_area)       delete _control_area;

}


//============================================================================
//====================== Make Plot and Popup classes =========================
//============================================================================
Widget FgQcOvjdPlot::make(Widget p)
{
 
   if ( made() ) return topWidget();

   Widget parent = p ? p : wParent();
   ShellStatMsg bld_info(parent, "Building Ovjd Plot...");


   SLFormHelpPop::make(p);

   _options_menu = new SLFPopSep(topWidget(),"Options Menu",
                                 FP_DOOK|FP_DOHELP,_hctx, True,False);

   _options_menu->make(topWidget());

   _buttons = new SLPushBox(_options_menu,"push_buttons",_hctx,
                            buttons,XtNumber(buttons));
   _buttons->setAltPushAction( (SLPushButtonfunc)button_control, this);

   _sp = new FgSeisPlot(topWidget(), "qcplot", _fgsp_list, False);


   _color_pop = new FgSeisColorPop(topWidget(), "Color Options", _sp, _hctx);
   _cbar_pop  = new SeisCbarPop (topWidget(),   "Color Bar",     _sp, _hctx);
  
   _pop = new FgQcOvjdSeisSelectPop(topWidget(), "Seismic Select", _hctx, this, 
                                    _sp, NULL);
   _pop->colorOpEnabled(_numcolors);
   _pop->setColorPop(_color_pop);

   _ovjd = new SeisOvjd(_sp);
   _ovjd->useExternalHeaders(True);
   _ovjd_pop= new FgSeisOvjdPop(topWidget(),"Offset Velocity",
                               _sp,_ovjd,_hctx,this);

   _header_dump_pop=  new HeaderDumpPop(topWidget(),"header_dump",_sp,_hctx);

   _seis_shift = new FgSeisShift(_sp, _ovjd_pop, _fg);
   _shift_pop  = new SeisShiftPop(topWidget(), "Seismic Shift", _sp,
                                  _seis_shift, _hctx);
   

   _control_area= new SeisControl(topWidget(), "control_area", _sp, NULL);

   _sp->pullPop()->addPush("menu_op",    (SLSelectionFunc)button_control,
                            this, MENU_OP);
   _sp->pullPop()->addPush("ovjd_op",    (SLSelectionFunc)button_control,
                            this, OVJD_OP);
   _sp->pullPop()->addPush("color_op",   (SLSelectionFunc)button_control,
                            this, COLOR_OP);
   _sp->pullPop()->addPush("cbar_op",    (SLSelectionFunc)button_control,
                            this, CBAR_OP);
   _sp->pullPop()->addPush("header_dump",(SLSelectionFunc)button_control,
                            this, HEADER_DUMP);
   _sp->pullPop()->addPush("shift_op",    (SLSelectionFunc)button_control,
                            this, SHIFT_OP);

   if(!inApplicationWindow())
      XtVaSetValues(topWidget(),XmNwidth,850,XmNheight,700,NULL);

   XtVaSetValues(topWidget(), XmNresizePolicy, XmRESIZE_NONE, NULL);

   if(!inApplicationWindow())
     XtVaSetValues(_options_menu->topWidget(),XmNwidth,225,XmNheight,400,NULL);
   else
     XtVaSetValues(_options_menu->topWidget(),XmNwidth,225,XmNheight,290,NULL);

   if (!inApplicationWindow())
     {
     XtVaSetValues(buttonContainer(),
                              XmNmarginHeight,     5,
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       220,
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNrightOffset,      320,
                              NULL);
     }
   else
     {
     _control_area->addPush("Options...",OPTIONS);
     _control_area->setAltPushAction((SLPushButtonfunc)button_control, this);
     XtSetSensitive(_control_area->getPush(OPTIONS), False);
     XtSetSensitive(_sp->pullPop()->topWidget(), False);
     }

   if (!inApplicationWindow())
     XtVaSetValues( _sp->W(), XmNleftAttachment,   XmATTACH_FORM,
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNbottomAttachment, XmATTACH_WIDGET,
                              XmNbottomWidget,     bottomSeparator(), 
                              NULL);
   else
     XtVaSetValues( _sp->W(), XmNleftAttachment,   XmATTACH_FORM,
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNbottomAttachment, XmATTACH_WIDGET,
                              XmNbottomWidget,     _control_area->W(), 
                              NULL);

   if (!inApplicationWindow())
     XtVaSetValues(_control_area->W(), XmNleftAttachment,   XmATTACH_FORM,
                                       XmNrightAttachment,  XmATTACH_FORM,
                                       XmNbottomAttachment, XmATTACH_FORM, 
                                       XmNtopAttachment,    XmATTACH_NONE,NULL);

   else
     XtVaSetValues(_control_area->W(), XmNleftAttachment,   XmATTACH_FORM,
                                       XmNrightAttachment,  XmATTACH_FORM,
                                       XmNbottomAttachment, XmATTACH_FORM, 
                                       XmNtopAttachment,    XmATTACH_NONE,NULL);
   XtVaSetValues( _buttons->W(),  
                            XmNtopAttachment,  XmATTACH_FORM,
                            XmNtopOffset,      5,
                            XmNleftAttachment, XmATTACH_FORM, 
                            XmNleftOffset,     5, 
                            XmNrightAttachment, XmATTACH_FORM, 
                            XmNrightOffset,    5, NULL); 


   if(_numcolors == 0)
     {
     XtSetSensitive(_buttons->pushW(CBAR_OP),  False);
     XtSetSensitive(_buttons->pushW(COLOR_OP), False);
     XtSetSensitive(_sp->pullPop()->getWidget(CBAR_OP), False);
     XtSetSensitive(_sp->pullPop()->getWidget(COLOR_OP), False);
     }
   else
     {
     _sp->setLoadableColors(MaximumValue(33,_numcolors));
     }

   if(!inApplicationWindow())
     {
     XtVaSetValues(_lowsep, XmNbottomOffset,  22, NULL);
     XtSetSensitive(_buttons->pushW(HEADER_DUMP),  False);
     XtSetSensitive(_sp->pullPop()->getWidget(HEADER_DUMP), False);
     }
   else
     {
     if(!_numcolors)
       {
       XtSetSensitive(_pulldown->getWidget(COLOR_OP),False);
       XtSetSensitive(_pulldown->getWidget(CBAR_OP),False); 
       }
     XtSetSensitive(_buttons->pushW(HEADER_DUMP),  False);
     XtSetSensitive(_sp->pullPop()->getWidget(HEADER_DUMP), False);
     XtSetSensitive(_pulldown->getWidget(HEADER_DUMP),False);
     XtUnmanageChild(_buttons->pushW(ZOOMUP));
     XtUnmanageChild(_buttons->pushW(ZOOMDOWN));
     XtUnmanageChild(_buttons->pushW(ZOOMORIGINAL));
     XtUnmanageChild(_buttons->pushW(ZOOMUPSEPARATE));
     }

  return topWidget();

}



//============================================================================
//====================== Return true if drawable is   ========================
//====================== in the main application window ======================
//====================== instead of it's own drawable ========================
//============================================================================
Boolean FgQcOvjdPlot::inApplicationWindow()
{
  if(SLFormPop::isDialog())
    return False;
  else
    return True;
}


//============================================================================
//====================== Notify for managing menu    =========================
//============================================================================
Boolean FgQcOvjdPlot::notifyComplex(SLDelay *obj, int ident)
{
  if(obj == _pulldown && ident == MENU_POP) _pop->makeAndManage();

  if(obj == _pulldown && ident == COLOR_OP) _color_pop->makeAndManage();

  if(obj == _pulldown && ident == HEADER_DUMP)_header_dump_pop->makeAndManage();

  return(True);
}


//============================================================================
//====================== Option button control       =========================
//============================================================================
void FgQcOvjdPlot::button_control( void *data, long button)
{
FgQcOvjdPlot *obj = (FgQcOvjdPlot *)data;

  switch(button) 
    {
    case MENU_OP:
      obj->_pop->manage();
      break;

    case OVJD_OP:
      obj->_ovjd_pop->makeAndManage();
      break;
 
    case COLOR_OP:
      obj->manageColorPop();
      break;

    case CBAR_OP:
      obj->manageColorBar();
      break;

    case HEADER_DUMP:
      obj->_header_dump_pop->makeAndManage();
      break;

    case SHIFT_OP:
      obj->_shift_pop->makeAndManage();
      break;

    case ZOOMUP:
      obj->_sp->zoomUp();
      break;

    case ZOOMUPSEPARATE:
      obj->_sp->zoomUpSeparateWin();
      break;

    case ZOOMDOWN:
      obj->_sp->zoomDown();
      break;

    case ZOOMORIGINAL:
      obj->_sp->originalSize();
      break;

    case OPTIONS:
      obj->_options_menu->makeAndManage();
      break;

    }
        
}

void FgQcOvjdPlot::manageColorPop()
{
  _color_pop->makeAndManage();
}


void FgQcOvjdPlot::manageColorBar()
{
  _cbar_pop->makeAndManage();
}

//============================================================================
//====================== Remove this class           =========================
//============================================================================
void FgQcOvjdPlot::removeButton()
{
  ParentClass::removeButton();
  _sp->cleanup();
}

//============================================================================
//====================== Extra buttons called        =========================
//============================================================================
void FgQcOvjdPlot::extraButton(int ident)
{
  switch(ident)
    {
    case OPTIONS:
      _options_menu->manage();
    break;
    }
}

//============================================================================
//====================== Inform Work                 =========================
//== NOTE: I originally intended to handle informs on a case by case basis ===
//== however, it became apparent that there are so many things that require ==
//== the ovjd to be replotted that I decided to always redraw the ovjd marks =
//== on a finishedChanges inform (redraw is very fast).                      =
//============================================================================
void FgQcOvjdPlot::startingChanges(FieldGeometry *fg)
{
  if( !made())return;
  assert(fg == _fg && !_changing);
  _changing = True;
}


void FgQcOvjdPlot::finishedChanges(FieldGeometry *fg)
{

  if(!made())return;
  assert(fg == _fg && _changing);

//I have circumvented specific informs - see note above
   preparePlot();
/*
   switch(_inform_action)
     {
     case EDIT_PLOT:
       editPlot();
     break;
 
     case MAKE_NEW_PLOT:
       preparePlot();
     break;

     case NEW_ACTIVE_LINE:
     break;
    }
*/
  _inform_action = NO_ACTION;
  _changing = False;
}


void FgQcOvjdPlot::preFlagValuesChanged(FieldGeometry * /*fg*/, long ixl,
                                        int /*ident*/, long index, 
                                        long /*nrem*/, long /*nins*/)
{
  if(!made())return;
  _line_index = ixl;
  _flag_index = index;
}


void FgQcOvjdPlot::postFlagValuesChanged(FieldGeometry * /*fg*/, long ixl,
                                    int ident, long index, 
                                    long nrem, long nins)
{

    if(!made()) return;

    switch (ident)
    {
    case FG_XLOC:
    case FG_YLOC:
    case FG_XGRID:
    case FG_YGRID:
    case FG_XSKID:
    case FG_YSKID:
      _line_index = ixl;
      _flag_index = index;

      if(nrem != nins)
        _inform_action = MAKE_NEW_PLOT;
      else
        _inform_action = EDIT_PLOT;
      break;





    default:
      break;
    }


}


void FgQcOvjdPlot::prePpValuesChanged(FieldGeometry * /*fg*/,
	                              int /*ident*/, long /*index*/, 
                                      long /*nrem*/,long /*nins*/)
{
}



void FgQcOvjdPlot::postPpValuesChanged(FieldGeometry * /*fg*/,
	                               int /*indent*/, long /*index*/, 
                                       long nrem, long nins)
{

  if(!made()) return;

  if(nrem != nins)
    _inform_action = MAKE_NEW_PLOT;
  else
    _inform_action = EDIT_PLOT;

}


void FgQcOvjdPlot::preRemoveInsertPpCards (FieldGeometry * /*fg*/,
                                           long /*index*/, long /*nrem*/, 
                                           long /*nins*/)
{
}

void FgQcOvjdPlot::postRemoveInsertPpCards(FieldGeometry * /*fg*/,
                                           long /*index*/, long /*nrem*/, 
                                           long /*nins*/)
{
  if(!made())return;
  _inform_action = MAKE_NEW_PLOT;
} 


void FgQcOvjdPlot::preRpValuesChanged(FieldGeometry * /*fg*/,
	                              int /*ident*/, long /*index*/, 
                                      long /*nrem*/,long /*nins*/)
{
}



void FgQcOvjdPlot::postRpValuesChanged(FieldGeometry * /*fg*/,
	                               int /*ident*/, long /*index*/, 
                                       long nrem, long nins)
{
  if(!made())return;

  if(nrem != nins)
    _inform_action = MAKE_NEW_PLOT;
  else
    _inform_action = EDIT_PLOT;

}


void FgQcOvjdPlot::preRemoveInsertRpCards (FieldGeometry * /*fg*/,
                                           long /*index*/, long /*nrem*/, 
                                           long /*nins*/)
{
}

void FgQcOvjdPlot::postRemoveInsertRpCards(FieldGeometry * /*fg*/,
                                           long /*index*/, long /*nrem*/, 
                                           long /*nins*/)
{
  if(!made())return;
  _inform_action = MAKE_NEW_PLOT;
}  

void FgQcOvjdPlot::preRemoveInsertLines(FieldGeometry * /*fg*/,
	                            long /*index*/, long /*nrem*/, 
                                    long /*nins*/)
{
}

void FgQcOvjdPlot::postRemoveInsertLines(FieldGeometry * /*fg*/,
	                             long /*index*/, long /*nrem*/, 
                                     long /*nins*/)
{
  if(!made())return;
  _inform_action = MAKE_NEW_PLOT;
}


void FgQcOvjdPlot::preNewActiveLine(FieldGeometry * /*fg*/)
{
}


void FgQcOvjdPlot::postNewActiveLine(FieldGeometry * /*fg*/)
{
}


void FgQcOvjdPlot::preNewActiveFlag(FieldGeometry * /*fg*/, long /*ixl*/)
{
}


void FgQcOvjdPlot::postNewActiveFlag(FieldGeometry * /*fg*/, long /*ixl*/)
{
}




//============================================================================
//====================== Main plotting function        =======================
//============================================================================
int FgQcOvjdPlot::preparePlot()
{
int error;
long i;
int j;
long primary_index, secondary_index;
long trace, images;
const float *sphd = _sp->headers(); 

  if(!_fg->numLines() || !_sp->imageIsDisplayed()) return(False);
  if(inApplicationWindow())
    XtSetSensitive(_control_area->getPush(OPTIONS), True);
  XtManageChild(topWidget());
  XSync(XtDisplay(topWidget()),False);

  _fg->startHeadersFromScratch ();

  images = _sp->movie() ? _sp->frames() : 1;  

  for(i=0; i < _sp->plottedNplt() * images; i++)
    {
    if(_ovjd_pop->matchHeaderMode()) //using header to match
      {
      primary_index = i * _sp->numHeaders() + _ovjd_pop->primaryHeader() - 1;
      secondary_index = i * _sp->numHeaders() +
                        _ovjd_pop->secondaryHeader() - 1;
      if(!_ovjd_pop->secondaryHeader())// using only one header to match
        {
        error = _fg->calculateHeaderWords((long)sphd[primary_index],False);
        }
      else//using header pairs to match
        {
        trace = _fg->findTraceNumber((long)sphd[primary_index],
                                (long)sphd[secondary_index]);
        error = _fg->calculateHeaderWords(trace,False);
        }
      }
    else //using the header label skip option to match
      {
      primary_index = i + _ovjd_pop->skipHeaders() + 1;
      error = _fg->calculateHeaderWords(primary_index,False);
      }
    if(!error)
      {
       for(j=1; j <= _sp->numHeaders(); j++)
         _ovjd->setExternalHeaders(i, j-1, (float)_fg->getHeaderWordValue(j));
       //dead so reset offset to near zero so ovjd mark will be near zero time 
       if(_fg->getHeaderWordValue(KILLED_HEADER) == 0)
         _ovjd->setExternalHeaders(i, OFFSET_HEADER, SMALL_OFFSET);
      }
    }

  _ovjd->post(_sp);

  return(True);

}




//============================================================================
//====================== Edit existing plot            =======================
//============================================================================
int FgQcOvjdPlot::editPlot()
{
//currently just get the new headers and replot
  preparePlot();

  _inform_action = NO_ACTION;

  return(True);
}



//============================================================================
//====================== FG Ovjd Specific SeisSelectPop Class ================
//============================================================================
void FgQcOvjdSeisSelectPop::DoAction()
{

  _fop->manage();
  XSync(XtDisplay(_fop->topWidget()),False);

  SeisSelectPop::DoAction();

  if(!_fop->sp()->isPlotDisplayed()) 
    {
    //_fop->unmanage();
    XtSetSensitive(_fop->_buttons->pushW(HEADER_DUMP),  False);
    XtSetSensitive(_fop->sp()->pullPop()->getWidget(HEADER_DUMP), False);
    if(_fop->inApplicationWindow())
      {
      XtSetSensitive(_fop->_pulldown->getWidget(HEADER_DUMP),False);
      XtSetSensitive(_fop->_control_area->getPush(OPTIONS), False);
      XtSetSensitive(_fop->_sp->pullPop()->topWidget(), False);
      }
    }
  else
    {
    XtSetSensitive(_fop->_buttons->pushW(HEADER_DUMP),  True);
    XtSetSensitive(_fop->sp()->pullPop()->getWidget(HEADER_DUMP), True);
    if(_fop->inApplicationWindow())
      {
      XtSetSensitive(_fop->_pulldown->getWidget(HEADER_DUMP),True);
      XtSetSensitive(_fop->_control_area->getPush(OPTIONS), True);
      XtSetSensitive(_fop->_sp->pullPop()->topWidget(), True);
      }
    }
}
