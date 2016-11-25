
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

//************************************************************************
//***                         The QC Plot Class                        ***
//***                     Michael L. Sherrill 06/95                    ***
//************************************************************************

#include <Xm/Separator.h>
#include <Xm/Xm.h>

#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_pop.hh"
#include "dp/control_points.hh"
#include "fgqc/fgqc_elevation_type.hh"
#include "fgqc/fgqc_fold_type.hh"
#include "fgqc/fgqc_statics_type.hh"
#include "fgqc/fgqc_header_type.hh"
#include "fgqc/fgqc_cmp_type.hh"
#include "fgqc/fgqc_bin_type.hh"
#include "fgqc/fgqc_pro_type.hh"
#include "fgqc/fgqc_bincenter_type.hh"
#include "fgqc/fgqc_compute_grid_type.hh"
#include "fgqc/fgqc_shifts_type.hh"
#include "fgqc/fgqc_hole_type.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sp/seis_plot.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/sl_app.hh"
#include "sl/sl_client_message.hh"
#include "sl/sl_error_pop.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/fgxp_2d_plot.hh"
#include "vect/ll_tag.hh"
#include "vect/tag.hh"
#include "dp/hill_shader_pop.hh"
#include "dp/grid_error_handler.hh"
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "fgmap/fg_loc_out.hh"
#include "fgmap/fg_map.hh"
#include "fgmap/fg_map_pick.hh"
#include "sp/do_abort.hh"
#include "fgqc/fgqc_option_pop.hh"
#include "named_constants.h"


#define locked "Data changes are prohibited.\nUnlock with Data Control Menu."


#define ParentClass SLFormHelpPop

#define OPTIONS 11
#define ACTIVE  12
#define APP_OPTIONS 13
#define AZIMUTHS 14









//============================================================================
//====================== FgQcPlot definitions =========================
//============================================================================
static String  defres[]= 
    {
    ".resizePolicy:              RESIZE_NONE",
    ".height:                    425",
    ".width:                     550",
    "*Active.background:         red",
    "*option_button.labelString: Options...",
//    "*Options_Menu.width:        225",
    "*Options_Menu.height:       400",
    "*menu_op.labelString:       QC Plot Menu...",
    "*color_op.labelString:      Color Options...",
    "*cbar_op.labelString:       Color Bar",
    "*zoom_separate.labelString: Zoom Separate Window",
    "*zoom_up.labelString:       Zoom Up",
    "*zoom_down.labelString:     Zoom Down",
    "*zoom_original.labelString: Original Size",
    "*hill_shader.labelString:   Hill Shader",
    "*map_overlay.labelString:   Overlay Map",
    "*picking_menu.labelString:  Transform Picking...",
    NULL
    };



static SLPush buttons[]  = 
    {
      { "menu_op",       MENU_OP},
      { "color_op",      COLOR_OP},
      { "cbar_op",       CBAR_OP },
      { "zoom_separate", ZOOMUPSEPARATE},
      { "zoom_up",       ZOOMUP },
      { "zoom_down",     ZOOMDOWN },
      { "zoom_original", ZOOMORIGINAL },
      { "hill_shader",   HILL_SHADER},
      { "map_overlay",   MAP_OVERLAY},
      { "picking_menu",  PICKING_MENU},
    };

static SLPush buttons_app[]  = 
    {
      { "menu_op",       MENU_OP},
      { "color_op",      COLOR_OP},
      { "cbar_op",       CBAR_OP },
      { "hill_shader",   HILL_SHADER},
      { "map_overlay",   MAP_OVERLAY},
      { "picking_menu",  PICKING_MENU},
    };

static SLPush app_buttons[] =
    {
      { "Toggle Azimuths",    AZIMUTHS   },
      { "option_button",      APP_OPTIONS},
    }; 

//============================================================================
//====================== Constructor with own window =========================
//============================================================================
FgQcPlot::FgQcPlot(Widget               p,
                   char                 *name,
                   HelpCtx              hctx, 
                   class FieldGeometry  *fg,
                   class FgQcPop        *pop,
                   class FgSeisPlotList *fgsp_list,
                   int                 numcolors)
                   : SLFormHelpPop(p, name, FP_DOHELP | FP_DOREMOVE,
	                       hctx, True, 2, False, True, numcolors, True),
                               FgInform(fg),_fg(fg),  _pop(pop),_hctx(hctx),
                               _fgsp_list(fgsp_list), _numcolors(numcolors)
{

  setDefaultResources( p, name, defres);
  addExtraButton("Options...",OPTIONS);
  addExtraButton("Active",ACTIVE);
  addExtraButton("Toggle Azimuths",AZIMUTHS);

  _been_managed           = False;
  _changing               = False;
  _inform_action          = NO_ACTION;
  _num_attributes         = 3;
  _plot_class             = NULL;
  _overlay                = NULL;
  _hill_shader_pop        = NULL;
  _options_menu           = NULL;
  _cbar_pop               = NULL;
  _color_pop              = NULL;
  _overlay_list           = NULL;
  _first_time             = True;
  _sp                     = NULL;
  _fgmap                  = NULL;
  _grid_error             = False;
  _fgmap_picker           = NULL;
  _limit_offsets_azimuths = 1;
  _percent                = 100.0;
  _plot_type              = ELEVATIONS;
  _reset_colors           = True;
}

//============================================================================
//================== Constructor used in  application window =================
//============================================================================
FgQcPlot::FgQcPlot(Widget               p,
                   char                 *name,
                   HelpCtx              hctx, 
                   class FieldGeometry  *fg,
                   class FgQcPop        *pop,
                   class FgSeisPlotList *fgsp_list,
                   int                  numcolors,
                   SLApp                *app)
                   : SLFormHelpPop(p, name, FP_DOHELP | FP_DOREMOVE,
	                       hctx, False, 2, False, False, numcolors),
                               FgInform(fg),_fg(fg),  _pop(pop),_hctx(hctx),
                               _fgsp_list(fgsp_list), _numcolors(numcolors)
{

  setDefaultResources( p, name, defres);

  _been_managed = False;
  _pulldown= new SLPullPop("QC Plots", NULL, app);
  _pulldown->addPush("QC Menu...", MENU_POP);
  _pulldown->addPush("Color Options...", COLOR_OP);
  _pulldown->addPush("Color Bar", CBAR_OP);
  _pulldown->addPush("Hill Shader", HILL_SHADER);
  _pulldown->addPush("Overlay Map", MAP_OVERLAY);
  _pulldown->addPush("Picking Menu", PICKING_MENU);
  _pulldown->setComplexNotify(this);

  _changing               = False;
  _inform_action          = NO_ACTION;
  _num_attributes         = 3;
  _plot_class             = NULL;
  _overlay                = NULL;
  _hill_shader_pop        = NULL;
  _options_menu           = NULL;
  _cbar_pop               = NULL;
  _color_pop              = NULL;
  _overlay_list           = NULL;
  _first_time             = True;
  _sp                     = NULL;
  _fgmap                  = NULL;
  _grid_error             = False;
  _fgmap_picker           = NULL;
  _limit_offsets_azimuths = 1;
  _percent                = 100.0;
  _plot_type              = ELEVATIONS;
  _reset_colors           = True;
}

//============================================================================
//====================== Destructor               =========================
//============================================================================
FgQcPlot::~FgQcPlot()
{

  if(_fgmap_picker)       delete _fgmap_picker;
  if(_fgmap)              delete _fgmap;
  if(_overlay)            delete _overlay;
  if(_overlay_list)       delete _overlay_list;
  if(_hill_shader_pop)    delete _hill_shader_pop;
  if(_plot_class)         delete _plot_class;
  if(_tag)                delete _tag;
  if(_tagll)              delete _tagll;
  if(_loc)                delete _loc;
  if(_cbar_pop)           delete _cbar_pop;
  if(_color_pop)          delete _color_pop;
  if(_options_menu)       delete _options_menu;
  if(_sp)
    new SLClientMessage(topWidget(), "junk", 
                        postDestructorClientMessageFunc, (void *) _sp);
}


//============================================================================
//====================== Make Popup                  =========================
//============================================================================
Widget FgQcPlot::make(Widget p)
{
 
   if ( made() ) return topWidget();

   Widget parent = p ? p : wParent();
   ShellStatMsg bld_info(parent, "Building Aerial Plot...");

   SLFormHelpPop::make(p);

   _options_menu = new FgQcOptionPop(this, "Options_Menu", _hctx);

   if(inApplicationWindow())
   {
	_buttons = new SLPushBox(_options_menu,"push_buttons",_hctx,
		buttons_app,XtNumber(buttons_app));

	_app_buttons = new SLPushBox(topWidget(),"app_buttons",_hctx,
		app_buttons,XtNumber(app_buttons));
	_app_buttons->setAltPushAction( (SLPushButtonfunc)button_control, this);
   }
   else
   {
	_buttons = new SLPushBox(_options_menu,"push_buttons",_hctx,
                              buttons,XtNumber(buttons));
   }

   _buttons->setAltPushAction( (SLPushButtonfunc)button_control, this);

   _sp = new FgSeisPlot(topWidget(), "qcplot", _fgsp_list, False);
   _overlay_list = new FgXpPlotLinkedList(_fgsp_list->defaultLineColor (),
                                          _fgsp_list->sourceLineColor  (),
                                          _fgsp_list->receiverLineColor(),
                                          _fgsp_list->bothLineColor    (),
                                          _fgsp_list->selectedLineColor(),
                                          _fgsp_list->activeLineColor  (),
                                          _fgsp_list->defaultFlagColor (),
                                          _fgsp_list->computedFlagColor(),
                                          _fgsp_list->selectedFlagColor(),
                                          _fgsp_list->activeFlagColor  (),
                                          _fgsp_list->useActiveLine    (),
                                          _fgsp_list->useSelectedLine  (),
                                          _fgsp_list->useBothLine      (),
                                          _fgsp_list->useReceiverLine  (),
                                          _fgsp_list->useSourceLine    (),
                                          _fgsp_list->useActiveFlag    (),
                                          _fgsp_list->useSelectedFlag  (),
                                          _fgsp_list->useComputedFlag  (),
                                          _fgsp_list->flagMode         () );
   _sp->setXpList(_overlay_list);
   _sp->setLoadableColors(MaximumValue(33,_numcolors));
   _color_pop = new FgSeisColorPop(topWidget(), "Color Options", _sp, _hctx);
   _color_pop->make();
   _cbar_pop  = new SeisCbarPop (topWidget(),    "Color Bar",    _sp, _hctx);
   _sp->initArrayTypePlot();
   _sp->cancelArrayTypeData();

   _sp->pullPop()->addPush("menu_op",    (SLSelectionFunc)button_control,
                            this, MENU_OP);
   _sp->pullPop()->addPush("color_op",   (SLSelectionFunc)button_control,
                            this, COLOR_OP);
   _sp->pullPop()->addPush("cbar_op",    (SLSelectionFunc)button_control,
                            this, CBAR_OP);
   _sp->pullPop()->addPush("hill_shader",(SLSelectionFunc)button_control,
                            this, HILL_SHADER);
   _sp->pullPop()->addPush("map_overlay",(SLSelectionFunc)button_control,
                            this, MAP_OVERLAY);
   _sp->pullPop()->addPush("picking_menu",(SLSelectionFunc)button_control,
                            this, PICKING_MENU);

   _tagll = new TagLinkedList(_sp);
   _tag = NULL;

   _loc = new FgLocOut(topWidget(), "qc_out", NULL, _fg, _sp);
   _loc->setTranslator(_fgsp_list->translator());

   // Unmap AZIMUTHS to start.
   // Map in plot if displaying CMP_DISTRIBUTION.
   //
   Widget azw = (inApplicationWindow())	? _app_buttons->pushW(AZIMUTHS)
					: getWidget          (AZIMUTHS);

   if (XtWindow(azw))
   {
      XtUnmapWidget(azw);
   }
   else
   {
      XtVaSetValues(azw,
         XmNmappedWhenManaged, False,
         NULL);
   }

  return topWidget();
  
}


//============================================================================
//====================== Manage Popup                =========================
//============================================================================
void FgQcPlot::manage()
{

  //The following attachements were put into this area because we get
  //a circular dependency error if it is destroyed and never managed
  //and the attachments have been registered.
  if(_been_managed == False)
   { 
   XtVaSetValues( topWidget(), XmNresizePolicy, XmRESIZE_NONE, NULL);

   if(!inApplicationWindow())
     {
     XtVaSetValues(topWidget(),XmNwidth,850,XmNheight,700,NULL);
     XtVaSetValues( _sp->W(), XmNleftAttachment,   XmATTACH_FORM,
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNbottomAttachment, XmATTACH_WIDGET,
                              XmNbottomWidget,     bottomSeparator(), 
                              NULL);

     XtVaSetValues(_loc->W(), XmNrightAttachment,  XmATTACH_FORM,
                              XmNbottomAttachment, XmATTACH_FORM,
                              NULL);

     XtVaSetValues(buttonContainer(),
                              XmNmarginHeight,     5,
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNrightAttachment,  XmATTACH_WIDGET,
                              XmNrightWidget,      _loc->W(),
                              XmNrightOffset,      5,
                              NULL);

     XtVaGetValues(getWidget(OPTIONS),XmNforeground,
                             &_foreground_no_hilite_color,   NULL);
     XtVaGetValues(getWidget(OPTIONS),XmNbackground,
                             &_background_no_hilite_color,   NULL);
     XtVaGetValues(getWidget(OPTIONS),XmNtopShadowColor,
                             &_topshadow_no_hilite_color,    NULL);
     XtVaGetValues(getWidget(OPTIONS),XmNbottomShadowColor,
                             &_bottomshadow_no_hilite_color, NULL);
     XtVaGetValues(getWidget(ACTIVE),XmNforeground,
                             &_foreground_hilite_color,      NULL);
     XtVaGetValues(getWidget(ACTIVE),XmNbackground,
                             &_background_hilite_color,      NULL);
     XtVaSetValues(_lowsep, XmNbottomOffset,  22, NULL);
     }
   else
     {
     XtVaSetValues(_sp->W(),         XmNleftAttachment,   XmATTACH_FORM,
                                     XmNrightAttachment,  XmATTACH_FORM,
                                     XmNtopAttachment,    XmATTACH_FORM,
                                     XmNbottomAttachment, XmATTACH_WIDGET,
                                     XmNbottomWidget,     _loc->W(),
                                     NULL);

     XtVaSetValues(_loc->W(),        XmNrightAttachment,  XmATTACH_FORM,
                                     XmNbottomAttachment, XmATTACH_FORM,
                                     XmNleftAttachment,XmATTACH_WIDGET,
                                     XmNleftWidget, _app_buttons->W(), 
                                     XmNtopAttachment,    XmATTACH_NONE,
                                     NULL);

     XtVaSetValues(_app_buttons->W(),XmNleftAttachment,   XmATTACH_FORM,
                                     XmNleftOffset,       300,
                                     XmNbottomAttachment, XmATTACH_FORM, 
                                     XmNtopAttachment,    XmATTACH_NONE,
                                     XmNbottomOffset,     20,
                                     XmNorientation,      XmHORIZONTAL,
                                     NULL);

     XtSetSensitive(_app_buttons->pushW(APP_OPTIONS),False);
     XtSetSensitive(_sp->pullPop()->topWidget(),False);
     XtSetSensitive(_pulldown->getWidget(CBAR_OP),False);
     XtSetSensitive(_pulldown->getWidget(HILL_SHADER),False);
     XtSetSensitive(_pulldown->getWidget(MAP_OVERLAY),False);
     }

     
   //Temporarily turn off the map overlay option until available
   XtSetSensitive(_sp->pullPop()->getWidget(MAP_OVERLAY),False);
   _options_menu->setOptions(MAP_OVERLAY,False);
  
   _been_managed = True;

   }//end never been managed


  ParentClass::manage();

}


//============================================================================
//====================== Return true if drawable is   ========================
//====================== in the main application window ======================
//====================== instead of it's own drawable ========================
//============================================================================
Boolean FgQcPlot::inApplicationWindow()
{
  if(SLFormPop::isDialog())
    return False;
  else
    return True;
}


//============================================================================
//====================== Notify for managing pulldown menus  =================
//============================================================================
Boolean FgQcPlot::notifyComplex(SLDelay *obj, int ident)
{
  if(!made()) return(False);

  if(obj == _pulldown && ident == MENU_POP)
     _pop->makeAndManage();

  if(obj == _pulldown && ident == COLOR_OP &&
     _plot_type != NORMALIZED_BINNING      &&
     _plot_type != BIN_CENTERS)
     {
     _color_pop->makeAndManage();
     _color_pop->setPlotClass (_plot_class);
     }

  return(True);
}
//============================================================================
//====================== Option button control       =========================
//============================================================================
void FgQcPlot::button_control( void *data, long button)
{
FgQcPlot *obj = (FgQcPlot *)data;

  switch(button) 
    {
    case MENU_OP:
      if(obj->_plot_class->getPickerMenu()
       && obj->_plot_class->getPickerMenu()->made()
       && XtIsManaged(obj->_plot_class->getPickerMenu()->W()))
        { 
        obj->_plot_class->getPickerMenu()->unmanage();
        obj->_plot_class->getPickerMenu()->removePicker();
        }

      if(obj->_hill_shader_pop) obj->_hill_shader_pop->unmanage();

      obj->_pop->manage();
      break;

    case COLOR_OP:
      if(obj->_plot_type != NORMALIZED_BINNING &&
         obj->_plot_type != BIN_CENTERS)
         obj->manageColorPop();
      break;

    case CBAR_OP:
      obj->manageColorBar();
      break;

    case ZOOMUP:
      obj->_sp->zoomUp();
      obj->redrawExtras();
      break;

    case ZOOMUPSEPARATE:
      obj->_sp->zoomUpSeparateWin();
      obj->redrawExtras();
      break;

    case ZOOMDOWN:
      obj->_sp->zoomDown();
      obj->redrawExtras();
      break;

    case ZOOMORIGINAL:
      obj->_sp->originalSize();
      obj->redrawExtras();
      break;
  
    case HILL_SHADER:
      if(obj->_hill_shader_pop != NULL) 
        {
        obj->_hill_shader_pop->make(obj->topWidget());   
        obj->_hill_shader_pop->setTitle(obj->_title);
        obj->_hill_shader_pop->makeAndManage();
        break;
        }
      if(obj->_plot_class->getHillShader() != NULL)
        {
        obj->_fg->preSlowOperations();
        obj->_cbar_pop->unmanage();
        if(obj->_color_pop->beenManaged()) obj->_color_pop->disableOptions();
        DoAbort *do_abort = obj->_plot_class->getFloatGrid()->getDoAbort ();
        if (do_abort) do_abort->setNewAction ();
        obj->_plot_class->getHillShader()->initialize(
                                             obj->_plot_class->getFloatGrid());
        if (do_abort) do_abort->actionComplete ();
        if(obj->_plot_class->getHillShader()->failed())
	  {
          new GridErrorHandler(obj->gridError(),"Error",
                             obj->_plot_class->getHillShader()->errorStatus());
          obj->_plot_class->getHillShader()->reset ();
          obj->_plot_class->removeHillShader();
          return;
          }
        obj->_hill_shader_pop = new HillShaderPop (obj->topWidget(),
                                            "hill_shader_pop",
                                             obj->_plot_class->getHillShader(),
                                             obj->_sp, obj->_hctx);
        if(obj->_hill_shader_pop->failed())
          {
          new GridErrorHandler(obj->gridError(),"Error",
                               obj->_hill_shader_pop->errorStatus());
          delete obj->_hill_shader_pop;
          obj->_hill_shader_pop = NULL;
          return;
          }
        if(!obj->_hill_shader_pop->initialize())
          {
          new GridErrorHandler(obj->gridError(),"Error",
                               obj->_hill_shader_pop->errorStatus());
          delete obj->_hill_shader_pop;
          obj->_hill_shader_pop = NULL;
          return;
          }
        obj->_plot_class->applyHillShader();
        obj->_hill_shader_pop->make(obj->topWidget());   
        obj->_hill_shader_pop->setTitle(obj->_title);
        obj->_hill_shader_pop->makeAndManage();
        obj->_color_pop->assignHillShaderPop(obj->_hill_shader_pop);
        obj->_options_menu->setOptions(CBAR_OP, False);
        XtSetSensitive(obj->_sp->pullPop()->getWidget(CBAR_OP), False);
        if(obj->inApplicationWindow()) 
           XtSetSensitive(obj->_pulldown->getWidget(CBAR_OP), False);
        obj->_fg->postSlowOperations();
        obj->_sp->drawColorBarOnHardCopy(SeisPlot::Off);
        break;
        }
      break;

    case MAP_OVERLAY:
      break;

    case PICKING_MENU:
       if(obj->_plot_class->getPickerMenu())
         {
         obj->_plot_class->getPickerMenu()->makeAndManage();
         }
       break;

    case APP_OPTIONS:
       obj->_options_menu->make(obj->topWidget());   
       obj->_options_menu->setTitle(obj->_title);
       obj->_options_menu->manage();
      break;

    case AZIMUTHS:
      ((FgQcCmpType *) obj->_plot_class)->showAzimuths();
      break;

    }
        
}

//===========================================================================
//============ Redraw any extra information in image ========================
//============ Really need to do this on a postZoom inform ==================
//===========================================================================
void FgQcPlot::redrawExtras()
{
FgQcBinType *bin_plot;
FgQcBinCenterType  *bin_center_plot;

  if(_plot_type == NORMALIZED_BINNING)
    {
    bin_plot = (FgQcBinType *)_plot_class;
    bin_plot->drawColorBox();
    }
  else if(_plot_type == BIN_CENTERS)
    {
    bin_center_plot = (FgQcBinCenterType *)_plot_class;
    bin_center_plot->drawColorBox();
    }
  

}


void FgQcPlot::manageColorPop()
{
 if(_plot_type != NORMALIZED_BINNING &&
    _plot_type != BIN_CENTERS)
    {
    _color_pop->makeAndManage();
    _color_pop->setPlotClass (_plot_class);
    }
}


void FgQcPlot::unmanageColorPop()
{
  _color_pop->unmanage();
}


void FgQcPlot::enableColorOptions()
{
  _color_pop->enableOptions();
}


void FgQcPlot::manageColorBar()
{
  _cbar_pop->make(topWidget());   
  _cbar_pop->setTitle(_title);
  _cbar_pop->manage();
}

//============================================================================
//====================== Remove this class           =========================
//============================================================================
void FgQcPlot::removeButton()
{
  ParentClass::removeButton();
  _pop->setActivePlot(this,True,False);
  delayDelete();
}

//============================================================================
//====================== Extra buttons called        =========================
//============================================================================
void FgQcPlot::extraButton(int ident)
{
FgQcCmpType *cmp_plot;

  switch(ident)
    {
    case OPTIONS:
      _options_menu->make(topWidget());   
      _options_menu->setTitle(_title);
      _options_menu->manage();
    break;

    case ACTIVE:
      _pop->setActivePlot(this, False,False);
      setActive();
    break;

    case AZIMUTHS:
      cmp_plot = (FgQcCmpType *)_plot_class;
      cmp_plot->showAzimuths();
    break;
    }     
}

//============================================================================
//====================== Set this plot inactive      =========================
//============================================================================
void FgQcPlot::setInActive()
{
  if(inApplicationWindow()) return;

  XtVaSetValues(getWidget(ACTIVE), XmNforeground, 
                _foreground_no_hilite_color, NULL);
  XtVaSetValues(getWidget(ACTIVE), XmNbackground, 
                _background_no_hilite_color, NULL);
  XtVaSetValues(getWidget(ACTIVE), XmNtopShadowColor, 
                _topshadow_no_hilite_color, NULL);
  XtVaSetValues(getWidget(ACTIVE), XmNbottomShadowColor, 
                _bottomshadow_no_hilite_color, NULL);
}

//============================================================================
//====================== Set this plot active        =========================
//============================================================================
void FgQcPlot::setActive()
{

  if(inApplicationWindow()) return;

  XtVaSetValues(getWidget(ACTIVE), XmNbackground, 
                _background_no_hilite_color, NULL);
  XtVaSetValues(getWidget(ACTIVE), XmNbackground, 
                _background_hilite_color, NULL);
}


//============================================================================
//====================== The Inform Work Begins Here =========================
//============================================================================
void FgQcPlot::startingChanges(FieldGeometry * /*fg*/)
{
  if(!made()) return;
  _changing = True;
}


void FgQcPlot::finishedChanges(FieldGeometry * /*fg*/)
{

  if(_sp == NULL)
    {
    _pop->updateCoordinates();
    return;
    }

  if(!_sp->imageIsDisplayed())
    {
    _pop->updateCoordinates();
    return;
    }

  if(!made()) return;
  if(!_sp->imageIsDisplayed())  return; 


  switch(_inform_action)
    {
    case EDIT_PLOT:
      editPlot();
    break;
 
    case MAKE_NEW_PLOT:
      if(_plot_type == SOURCE_PROGRESSION)
        {
        plot(_plot_type);
        break;
        }

      makeTag();
    break;

    default:
    break;
    }

  _inform_action = NO_ACTION;
  _changing = False;

}


void FgQcPlot::preFlagValuesChanged(FieldGeometry * /*fg*/, long ixl,
                                    int ident, long index, 
                                    long nrem, long /*nins*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed()) return; 
  _line_index = ixl;
  _flag_index = index;
  if(nrem) getPreRegion(ixl, ident, index, nrem);
}


void FgQcPlot::postFlagValuesChanged(FieldGeometry *fg, long ixl,
                                    int ident, long index, 
                                    long nrem, long nins)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed()) return; 

  _line_index = ixl;
  _flag_index = index;

  if(nrem != nins) 
     _inform_action = MAKE_NEW_PLOT;

  if(_inform_action == MAKE_NEW_PLOT) return;

   _inform_action = _plot_class->ValuesChanged(fg,ixl,ident,index,nrem,nins);

  if(_inform_action == EDIT_PLOT) getPostRegion(ixl, ident, index, nins);
}

void FgQcPlot::postNewGridTransform(FieldGeometry * /*fg*/)
{
FgQcCmpType *cmp_plot;

  if(!made()) return;
  if(!_sp->imageIsDisplayed())  return; 

  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:
    case HEADER_WORD:
    case BIN_CENTERS:
      _inform_action = MAKE_NEW_PLOT;
      if(_plot_type == CMP_DISTRIBUTION) 
        {
        cmp_plot = (FgQcCmpType *)_plot_class;
        cmp_plot->modifyGridMatrix();
        }
      break;
    }
}

void FgQcPlot::postNewTestingGridTransform(FieldGeometry * /*fg*/)
{
FgQcCmpType *cmp_plot;
FgQcBinCenterType *fold_plot;

  if(_plot_type == CMP_DISTRIBUTION) 
    {
    cmp_plot = (FgQcCmpType *)_plot_class;
    cmp_plot->modifyTestingGridMatrix();
    }
  else if(_plot_type == BIN_CENTERS)
    {
    fold_plot = (FgQcBinCenterType *)_plot_class;
    fold_plot->modifyTestingGridMatrix();
    }

}



void FgQcPlot::sourceGathersOutOfDate(FieldGeometry * /*fg*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed())  return; 

  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:    
    case HEADER_WORD:
    case BIN_CENTERS:
      _inform_action = MAKE_NEW_PLOT;
      break;
    }
}


void FgQcPlot::receiverGathersOutOfDate(FieldGeometry * /*fg*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed())  return; 

  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:    
    case HEADER_WORD:
    case BIN_CENTERS:
      _inform_action = MAKE_NEW_PLOT;
      break;
    }
}

void FgQcPlot::midpointGathersOutOfDate(FieldGeometry * /*fg*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed())  return; 

  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:    
    case HEADER_WORD:
    case BIN_CENTERS:
      _inform_action = MAKE_NEW_PLOT;
      break;
    }
}
void FgQcPlot::liveFoldOutOfDate(FieldGeometry * /*fg*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed())  return; 

  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:    
    case HEADER_WORD:
    case BIN_CENTERS:
      _inform_action = MAKE_NEW_PLOT;
      break;
    }
}

void FgQcPlot::postUpdateSourceGathers(FieldGeometry * /*fg*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed())  return; 

  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:    
    case HEADER_WORD:
    case BIN_CENTERS:
      _inform_action = MAKE_NEW_PLOT;
      break;
    }
}

void FgQcPlot::postUpdateReceiverGathers(FieldGeometry * /*fg*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed())  return; 

  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:    
    case HEADER_WORD:
    case BIN_CENTERS:
      _inform_action = MAKE_NEW_PLOT;
      break;
    }
}

void FgQcPlot::postUpdateMidpointGathers(FieldGeometry * /*fg*/)
{
  if(!made()) return; 
  if(!_sp->imageIsDisplayed())  return; 

  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:    
    case HEADER_WORD:
    case BIN_CENTERS:
      _inform_action = MAKE_NEW_PLOT;
      break;
    }
}


void FgQcPlot::preRemoveInsertLines(FieldGeometry * /*fg*/,
	                            long /*index*/, long /*nrem*/, 
                                    long /*nins*/)
{
}

void FgQcPlot::postRemoveInsertLines(FieldGeometry * /*fg*/,
	                             long /*index*/, long /*nrem*/, 
                                     long /*nins*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed()) return; 
  _inform_action = MAKE_NEW_PLOT;
}


void FgQcPlot::preNewActiveLine(FieldGeometry * /*fg*/)
{
}


void FgQcPlot::postNewActiveLine(FieldGeometry * /*fg*/)
{
  if(!made()) return;
  if(!_sp->imageIsDisplayed()) return;
  if(_fg->numLines() < 2) return;
  if(_fg->getActiveLineIndex() == -1)
    _overlay = (FgXp2DPlot *) NULL;

  new SLClientMessage(topWidget(), "junk", 
                      postActiveLineClientMessageFunc, (void *) this);  
}

void FgQcPlot::postActiveLineClientMessageFunc(void *obj)
{
FgQcPlot *plt = (FgQcPlot *)obj;

  if(!plt->made()) return;
  if(!plt->_sp->imageIsDisplayed()) return;
  if(plt->_fg->numLines() < 2) return;
  plt->makeOverlay();

}

//This function used to prevent watches from being 
//removed on destroyed seisplots
void FgQcPlot::postDestructorClientMessageFunc(void *obj)
{
FgSeisPlot *sp = (FgSeisPlot *)obj;

  if(sp) delete sp;

}


void FgQcPlot::preNewActiveFlag(FieldGeometry * /*fg*/, long /*ixl*/)
{
}


void FgQcPlot::postNewActiveFlag(FieldGeometry *fg, long ixl)
{
//may need this function later
 if(!made()) return;
 if(!_sp->imageIsDisplayed()) return;
 _plot_class->postNewActiveFlag(fg, ixl);
}

//============================================================================
//====================== Mark image out of date      =========================
//============================================================================
void FgQcPlot::makeTag()
{
int x, y, width, height;
float tempx, tempy;
float tagx, tagy;


  if(_tag) delete(_tag);

  _sp->getVisibleArea(&x, &y, &width, &height);  

  tempx = (float)( (float)x + ((float)width  * .50) );
  tempy = (float)( (float)y + ((float)height * .50) );
  tagx  = _sp->xWC((int)tempx);
  tagy  = _sp->yWC((int)tempy);

  _tag = new Tag(_tagll, 
                 " Plot may be out of date. Click here to remove this message ",
                 tagx,tagy);
}

//============================================================================
//====================== Create active line markers  =========================
//============================================================================
void FgQcPlot::makeOverlay()
{

  if(_fg->numLines() < 2) return;

  if(!_overlay && _fg->getActiveLineIndex() != -1)
  {
    int x_coord, y_coord;

    if(getCoordinateSystem() == SURVEYSYSTEM)
      {
      x_coord = FG_XLOC;
      y_coord = FG_YLOC;
      }
    else
      {
      x_coord = FG_XGRID;
      y_coord = FG_YGRID;
      }

    _overlay = new FgXp2DPlot(_fg, _overlay_list,
                              ActiveLine, 
                              Lines,1,
                              LdCardType,
                              x_coord, -1, 
                              LdCardType,
                              y_coord, -1);
    _overlay->addPlot(_sp, True);
  }

}

//============================================================================
//====================== Define pre-editted region   =========================
//============================================================================
void FgQcPlot::getPreRegion(long /*ixl*/, long /*ident*/, 
                            long /*index*/, long nins)
{
long i;
float xloc, yloc;
float minx = MinimumValue(_user_left,_user_right);
float maxx = MaximumValue(_user_left,_user_right);
float miny = MinimumValue(_user_top,_user_bottom);
float maxy = MaximumValue(_user_top,_user_bottom);

  _pre_minx =  MAX_NUM;
  _pre_maxx =  MIN_NUM;
  _pre_miny =  MAX_NUM;
  _pre_maxy =  MIN_NUM;

  for(i=0; i<nins; i++)
    {
    xloc = _fg->getXloc(_line_index,_flag_index + i);
    yloc = _fg->getYloc(_line_index,_flag_index + i);
    if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy )
      {
      _pre_minx = _pre_minx > xloc ? xloc : _pre_minx;
      _pre_maxx = _pre_maxx < xloc ? xloc : _pre_maxx;
      _pre_miny = _pre_miny > yloc ? yloc : _pre_miny;
      _pre_maxy = _pre_maxy < yloc ? yloc : _pre_maxy;           
      }
    }

  _pre_minx -= _x_reach;
  _pre_maxx += _x_reach;
  _pre_miny -= _y_reach;
  _pre_miny += _y_reach;
}


//============================================================================
//====================== Define post-editted region  =========================
//============================================================================
void FgQcPlot::getPostRegion(long /*ixl*/, long /*ident*/, 
                             long /*index*/, long nins)
{
long i;
float xloc, yloc;
float minx = MinimumValue(_user_left,_user_right);
float maxx = MaximumValue(_user_left,_user_right);
float miny = MinimumValue(_user_top,_user_bottom);
float maxy = MaximumValue(_user_top,_user_bottom);

  _post_minx =  MAX_NUM;
  _post_maxx =  MIN_NUM;
  _post_miny =  MAX_NUM;
  _post_maxy =  MIN_NUM;

  for(i=0; i<nins; i++)
    {
    xloc = _fg->getXloc(_line_index,_flag_index + i);
    yloc = _fg->getYloc(_line_index,_flag_index + i);
    if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy )
      {
      _post_minx = _post_minx > xloc ? xloc : _post_minx;
      _post_maxx = _post_maxx < xloc ? xloc : _post_maxx;
      _post_miny = _post_miny > yloc ? yloc : _post_miny;
      _post_maxy = _post_maxy < yloc ? yloc : _post_maxy;           
      }
    }

  _post_minx -= _x_reach;
  _post_maxx += _x_reach;
  _post_miny -= _y_reach;
  _post_maxy += _y_reach;

  _post_minx = MinimumValue(_pre_minx,_post_minx);
  _post_maxx = MaximumValue(_pre_maxx,_post_maxx);
  _post_miny = MinimumValue(_pre_miny,_post_miny);
  _post_maxy = MaximumValue(_pre_maxy,_post_maxy);

  if (_user_left > _user_right) {
    _post_minx = MaximumValue(_user_right,_post_minx);
    _post_maxx = MinimumValue(_user_left,_post_maxx);
  }
  else {
    _post_minx = MaximumValue(_user_left,_post_minx);
    _post_maxx = MinimumValue(_user_right,_post_maxx);
  }

  if (_user_top > _user_bottom) {
    _post_miny = MaximumValue(_user_bottom,_post_miny);
    _post_maxy = MinimumValue(_user_top,_post_maxy);
  }
  else {
    _post_miny = MaximumValue(_user_top,_post_miny);
    _post_maxy = MinimumValue(_user_bottom,_post_maxy);
  }
}


//============================================================================
//====================== Determine image array size    =======================
//============================================================================

void FgQcPlot::computeArraySize()
{
long hpix_per_inch;
long vpix_per_inch;

  hpix_per_inch =  _sp->getHorizontalPixelsPerInch(XtDisplay(_sp->getWidget()), 
                                DefaultScreen(XtDisplay(_sp->getWidget())));
  vpix_per_inch =  _sp->getVerticalPixelsPerInch(XtDisplay(_sp->getWidget()), 
                                DefaultScreen(XtDisplay(_sp->getWidget())));

  _numx = (long)(_pwidth  * (float)hpix_per_inch ) + 1;
  _numy = (long)(_pheight * (float)vpix_per_inch ) + 1;
}


//============================================================================
//====================== Main plotting function        =======================
//============================================================================

int FgQcPlot::plot(int plot_type)
{
int stat = False;

  if(_hill_shader_pop != NULL) {
    delete _hill_shader_pop, _hill_shader_pop = NULL;
    enableColorOptions ();
  }

  _color_pop->assignHillShaderPop  (_hill_shader_pop);

  if(_plot_class != NULL) delete _plot_class;

  if(!_fg->numLines()) return(False);

  if(_tag) {delete _tag; _tag = NULL;}


  _color_pop->dontPlotYet(True);
//  _color_pop->make();
//  _color_pop->dontPlotYet(False);


  _pwidth      = _pop->_pwidth;
  _pheight     = _pop->_pheight;
  _user_left   = _pop->_left;
  _user_right  = _pop->_right;
  _user_top    = _pop->_top;
  _user_bottom = _pop->_bottom;
  _plot_type   = plot_type;

  _coordinate_system = _pop->getCoordinateSystem();


  //If we have previously plotted a plot type that uses the sector type
  //color bar we need to reset it to the standard color bar
  if(_sp->getPreDefColor() == PlotImage::SECTOR)
    {
    _sp->setPreDefColor(PlotImage::STANDARD);
    _sp->setTopBorder(50);
    }

  switch (_plot_type)
    {
    case ELEVATIONS:
      computeArraySize();
      _plot_class = new FgQcElevationType(this, FgQcElevationType::FLAG);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;  

    case SOURCE_ELEVATIONS:
      computeArraySize();
      _plot_class = new FgQcElevationType(this, FgQcElevationType::SOURCE);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;  

    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
      computeArraySize();
      _plot_class = new FgQcFoldType(this);

      if(_plot_type == FOLD_OF_STACK) {
        _limit_offsets_azimuths = 1;
        _limit_type = _pop->getOffsetLimitType ();
        setOffsets (_pop->getOffsetMinimum(), _pop->getOffsetMaximum());
      }
      else /* if(_plot_type == AZIMUTHAL_DISTRIBUTION) */ {
        _limit_offsets_azimuths = 2;
        _limit_type = _pop->getAzimuthLimitType ();
        setOffsets(_pop->getOffsetMinimum(), _pop->getOffsetMaximum());
        setAzimuths (_pop->getAzimuthMinimum(), _pop->getAzimuthMaximum());
      }

      _loc->setAttr1(SP_Z_OUTPUT);
     break;

    case BIN_CENTERS:
      computeArraySize();
      _plot_class = new FgQcBinCenterType(this);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;

    case STATICS:
      computeArraySize();
      _plot_class = new FgQcStaticsType(this);
      ((FgQcStaticsType *)_plot_class)->setStaticsFilename(_statics_filename);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;


    case CMP_DISTRIBUTION:
      computeArraySize();
      _plot_class = new FgQcCmpType(this);
      _loc->setAttr1(SP_Z_OUTPUT);

//    Handle this below.  ehs --- 01sep00
//    addExtraButton("Toggle Azimuths",AZIMUTHS);

      break;

    case NORMALIZED_BINNING:
      computeArraySize();
      _plot_class = new FgQcBinType(this);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;

    case HEADER_WORD:
      computeArraySize();
      _plot_class = new FgQcHeaderType(this);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;

    case SOURCE_PROGRESSION:
      computeArraySize();
      _plot_class = new FgQcProgressionType(this);
      break;

    case DIFFERENCES:
      computeArraySize();
      _plot_class = new FgQcComputeGridType(this);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;

    case RECEIVER_SHIFTS:
      computeArraySize();
      _plot_class = new FgQcShiftsType(this, FgQcShiftsType::RECEIVER);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;

    case SOURCE_SHIFTS:
      computeArraySize();
      _plot_class = new FgQcShiftsType(this, FgQcShiftsType::SOURCE);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;

    case UPHOLES:
      computeArraySize();
      _plot_class = new FgQcHoleType(this, FgQcHoleType::TIME);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;

    case HOLE_DEPTHS:
      computeArraySize();
      _plot_class = new FgQcHoleType(this, FgQcHoleType::DEPTH);
      _loc->setAttr1(SP_Z_OUTPUT);
      break;

    default:
      break;
    }

  if(_plot_class == NULL) return(stat);

  // Map and unmap "Toggle Azimuths" button as necessary.   ehs --- 01sep00
  //
  Widget azw = (inApplicationWindow())	? _app_buttons->pushW(AZIMUTHS)
					: getWidget          (AZIMUTHS);

  if (XtWindow(azw))
  {
	if (_plot_type == CMP_DISTRIBUTION)
		XtMapWidget  (azw);
	else
		XtUnmapWidget(azw);
  }
  else
  {
	if (_plot_type == CMP_DISTRIBUTION)
		XtVaSetValues(azw,
			XmNmappedWhenManaged, True ,
			NULL);
	else
		XtVaSetValues(azw,
			XmNmappedWhenManaged, False,
			NULL);
  }

  if(!inApplicationWindow())
    {
    manage();
    XSync(XtDisplay(topWidget()),False);
    }

  if(_plot_type != SOURCE_PROGRESSION) _sp->setPlotType(PlotImage::PlotARRAY);

  _fg->preSlowOperations();

  _sp->drawColorBarOnHardCopy(SeisPlot::On);
  stat = _plot_class->plot();
  _fg->postSlowOperations();

  if(stat) 
    {
    makeOverlay();
    if(_plot_type == CMP_DISTRIBUTION)
     { 
     _sp->drawColorBarOnHardCopy(SeisPlot::Off);
     }
    if(_plot_type == BIN_CENTERS || _plot_type == NORMALIZED_BINNING ||
       _plot_type == SOURCE_PROGRESSION)
     {
     _sp->drawColorBarOnHardCopy(SeisPlot::Square9);
     _options_menu->setOptions(COLOR_OP, False);
     _options_menu->setOptions(CBAR_OP, False);
     XtSetSensitive(_sp->pullPop()->getWidget(COLOR_OP), False);
     XtSetSensitive(_sp->pullPop()->getWidget(CBAR_OP), False);
     }
    else
     {
     _options_menu->setOptions(COLOR_OP, True);
     _options_menu->setOptions(CBAR_OP, True);
     XtSetSensitive(_sp->pullPop()->getWidget(COLOR_OP), True);
     XtSetSensitive(_sp->pullPop()->getWidget(CBAR_OP), True);
     }

    if(_plot_type == BIN_CENTERS || _plot_type == CMP_DISTRIBUTION)
     {
     _options_menu->setOptions(PICKING_MENU, True);
     XtSetSensitive(_sp->pullPop()->getWidget(PICKING_MENU), True);
     }
    else
     {
     _options_menu->setOptions(PICKING_MENU, False);
     XtSetSensitive(_sp->pullPop()->getWidget(PICKING_MENU), False);
     }

    _options_menu->setOptions(HILL_SHADER, _pop->shaderRequested());
    XtSetSensitive(_sp->pullPop()->getWidget(HILL_SHADER),
                   _pop->shaderRequested());
    if(inApplicationWindow())
       {
       XtSetSensitive(_pulldown->getWidget(CBAR_OP),True);
       XtSetSensitive(_pulldown->getWidget(HILL_SHADER),
                      _pop->shaderRequested());
       XtSetSensitive(_sp->pullPop()->topWidget(),True);
       XtSetSensitive(_app_buttons->pushW(APP_OPTIONS),True);
       }
    if(_fgmap_picker == NULL)
      {
      if(_pop->getCoordinateSystem() == SURVEYSYSTEM)
        _fgmap_picker= new FgMapPick(_sp, _fg, _fgsp_list->translator(), False);
      else
        _fgmap_picker= new FgMapPick(_sp, _fg, _fgsp_list->translator(), True);
      }

    if(_options_menu->made())
      {
      if(_plot_type == BIN_CENTERS && inApplicationWindow() == False &&
         XtIsManaged(_buttons->pushW(ZOOMUP)) )
        {
        XtUnmanageChild(_buttons->pushW(ZOOMUP));
        XtUnmanageChild(_buttons->pushW(ZOOMDOWN));
        XtUnmanageChild(_buttons->pushW(ZOOMORIGINAL));
        XtUnmanageChild(_buttons->pushW(ZOOMUPSEPARATE));
        }
      else if(inApplicationWindow() == False && 
         XtIsManaged(_buttons->pushW(ZOOMUP)) )
        {
        XtManageChild(_buttons->pushW(ZOOMUP));
        XtManageChild(_buttons->pushW(ZOOMDOWN));
        XtManageChild(_buttons->pushW(ZOOMORIGINAL));
        XtManageChild(_buttons->pushW(ZOOMUPSEPARATE));
        }
      }
    }
  else
    {
//if(_app == NULL) unmanage();
    if(inApplicationWindow())
      {
      XtSetSensitive(_pulldown->getWidget(CBAR_OP),False);
      XtSetSensitive(_app_buttons->pushW(APP_OPTIONS),False);
      XtSetSensitive(_sp->pullPop()->topWidget(),False);
      }
    }


  return(stat);
}




//============================================================================
//====================== Edit existing plot            =======================
//============================================================================
int FgQcPlot::editPlot()
{
int stat = True;

  stat = _plot_class->editData();

  _inform_action = NO_ACTION;

  return(True);
}


//============================================================================
//================= Make a plot indirectly.                         ==========
//================= Creation is done in  the FgQcPop class          ==========
//================= since it has to keep track of active plot, etc. ==========
//============================================================================
int FgQcPlot::makeIndirectPlot(int plot_type, float width, float height,
                                    float xmin, float ymin, float xmax,
                                    float ymax, int coord_system)
{

 return _pop->makeIndirectPlot(plot_type,  width, height,
                                    xmin, ymin, xmax,
                                    ymax, coord_system);

}

void FgQcPlot::setTitles()
{
char hdrwrd[4];


  strcpy (_title, "Attribute: ");
  strcat (_title, _pop->getAttributeName(_plot_type));
  switch (_plot_type) {
    case HEADER_WORD:
      sprintf (hdrwrd, " %d", getHeaderWord());
      strcat (_title, hdrwrd);
      break;
    default:
      break;
  }

  titleVersion (_title, _pop->getVersionNumber(this));

  setTitle(_title);

//When color pop is capable remove the following if and do it only at the
//time we need the color pop
  if(_color_pop)
    {
    _color_pop->make(topWidget());
    _color_pop->setPlotClass (_plot_class);
    _color_pop->setTitle(_title);
    }


}

char *FgQcPlot::titleVersion (char *title, int version_num)
{
  char version[4];
  strcpy (title, _title);
  sprintf (version, ";%d", version_num);
  return strcat (title, version); 
} 


void FgQcPlot::setStaticsFilename(char *filename)
{
  strcpy(_statics_filename, filename);
}

void FgQcPlot::reduceColors (int reduction)
{
   if (!made()) {
     _numcolors = _numcolors > reduction ? _numcolors - reduction : 0;
   }
}
