//=============================================================================
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
//=============          M.L. Sherrill 11/95            =======================
//============= Put cfg qc plot pick menus and pickers in this file ===========
//=============================================================================

#include "fgqc/fgqc_pick_menu.hh"
#include "fgqc/fgqc_cmp_type.hh"
#include "fgqc/fgqc_bincenter_type.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"
#include "geom/grid_lines.hh"
#include "cprim.h"
#include "named_constants.h"
#include "sl/prim_support.hh"
#include "sl/sl_error_pop.hh"

#include <Xm/Xm.h>

#define locked "Data changes are prohibited.\nUnlock with Data Control Menu."

static char *picking_mode = "Mode: Picking";

static const char * const move_grid_token = "MOVEGRID";
static char *move_grid_help= "mouse*MOVEGRID:  BTN#1: Move Grid X and Y, \
BTN#2: None, BTN#3: None";

static const char * const center_grid_token = "CENTERGRID";
static char *center_grid_help= "mouse*CENTERGRID:  BTN#1: Center grid at mouse click, \
BTN#2: None, BTN#3: None";

static const char * const change_angle_token = "CHANGEANGLE";
static char *change_angle_help= "mouse*CHANGEANGLE:  BTN#1: Drag a new angle, \
BTN#2: None, BTN#3: None";

static const char * const refine_angle_token = "REFINEANGLE";
static char *refine_angle_help= "mouse*REFINEANGLE:  BTN#1: Refine angle, \
BTN#2: None, BTN#3: None";

static const char * const define_origin_token = "DEFINEORIGIN";
static char *define_origin_help= "mouse*DEFINEORIGIN:  BTN#1: Make clicked location grid coordinates 1,1 , \
BTN#2: None, BTN#3: None";

static const char * const select_cmp_token = "SELECTCMP";
static char *select_cmp_help= "mouse*SELECTCMP:  BTN#1: Scrolls cmp table to location you click, \
BTN#2: None, BTN#3: None";

static const char * const cmp_distribution_token = "CMPDISTRIBUTION";
static char *cmp_distribution_help= "mouse*CMPDISTRIBUTION:  BTN#1: Click and drag an area to generate a cmp distribution plot, \
BTN#2: None, BTN#3: None";
//=============================================================================
//============= Grid Definition type plot menu          =======================
//=============================================================================

FgQcGridDefinitionPickerMenu::FgQcGridDefinitionPickerMenu( 
                                   Widget            p,
                                   char              *name,
                                   HelpCtx           hctx,
                                   FgQcPlot          *fgqc_plot,
                                   FgQcPlotType      *plot_class,
                                   int               plot_type)
                             : FgQcPickerMenu(p,name,hctx,fgqc_plot,
                                              plot_class,plot_type)
{
static SLRadio radiobox[] = 
  {
    { "Move Grid",             MOVE_GRID },
    { "Center Grid",           CENTER_GRID},
    { "Change Angle",          CHANGE_ANGLE},
    { "Refine Angle",          REFINE_ANGLE},
    { "Define Origin",         DEFINE_ORIGIN},
    { "Select Table Cmp",      SELECT_CMP },
    { "Cmp Distribution Plot", CMP_PLOT }
  };


static SLTog showbox[] = 
  {
    { "Show Original Transform", NULL, SHOW_ORIGINAL },
    { "Show New Transform",      NULL, SHOW_NEW},
  };

static SLPush buttons[]  = 
  {
    { "Save Transform",             NEW_TRANSFORM  },
    { "Restore Original Transform", ORIGINAL_TRANSFORM},
  };


  _radiobox = new SLRadioBox(this,"radiobox",getHelpCtx(),radiobox,
                            XtNumber(radiobox), NULL, True, False);
  _radiobox->setAltChoiceAction((SLRadioButtonfunc)GridSelectAction,this);


  _showbox = new SLTogBox(this, "show_box",getHelpCtx(),showbox,
                            XtNumber(showbox), True, False, False );
  _showbox->setAltChoiceAction( (SLToggleButtonfunc)ShowBoxAction, this);


  _buttons = new SLPushBox(this,"trans_button",getHelpCtx(),
                           buttons,XtNumber(buttons));
  _buttons->setAltPushAction( (SLPushButtonfunc)button_control, this);

  _first_time = True;
  _fgqc_picker = NULL;

}

FgQcGridDefinitionPickerMenu::~FgQcGridDefinitionPickerMenu()
{
  if(_fgqc_picker != NULL) delete _fgqc_picker;
}


void FgQcGridDefinitionPickerMenu::createPicker(int can_show)
{

  assert(_fgqc_picker == NULL);
  _fgqc_picker = new FgQcGridDefinitionPicker(_fgqc_plot->sp(), this, can_show,
                                              picking_mode, move_grid_token,
                                              move_grid_help);
}


void FgQcGridDefinitionPickerMenu::removePicker()
{
  assert(_fgqc_picker != NULL);
  delete _fgqc_picker;
  _fgqc_picker = NULL;
}


Widget FgQcGridDefinitionPickerMenu::make(Widget p)
{

  if ( made() ) return topWidget();

  SLFPopSep::make(p);

//Moved to manage.  ehs --- 07sep00
//_radiobox->SetRadio(MOVE_GRID);
//
//if(_plot_type != BIN_CENTERS)
//  {
//  _showbox->SetTog(SHOW_ORIGINAL, True);
//  _showbox->SetTog(SHOW_NEW, True);
//  }
//else//Dont default grid lines on if bin centers plot
//  {
//  _showbox->SetTog(SHOW_ORIGINAL, False);
//  _showbox->SetTog(SHOW_NEW, False);
//  }

  XtVaSetValues(topWidget(),    XmNwidth,            240,
                                XmNheight,           440, NULL);


  XtVaSetValues( _radiobox->W(),XmNleftAttachment,   XmATTACH_FORM,
                                XmNleftOffset,       5,
                                XmNrightAttachment,  XmATTACH_FORM,
                                XmNrightOffset,      5,
                                XmNtopAttachment,    XmATTACH_FORM, 
                                XmNtopOffset,        10, NULL);

  XtVaSetValues( _showbox->W(), XmNleftAttachment,   XmATTACH_FORM,
                                XmNleftOffset,       5,
                                XmNrightAttachment,  XmATTACH_FORM,
                                XmNrightOffset,      5,
                                XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET, 
                                XmNtopWidget,        _radiobox->W(),
                                XmNtopOffset,        205, NULL);

  XtVaSetValues(_buttons->W(), XmNleftAttachment,   XmATTACH_FORM,
                               XmNleftOffset,       5,
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNrightOffset,      5,
                               XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,        _showbox->W(),
                               XmNtopOffset,        70, NULL);

  setTitle(_fgqc_plot->getTitle());

  return topWidget();
}


void FgQcGridDefinitionPickerMenu::manage()
{
  // FgQcGridDefinitionPicker is only created when the menu is managed and
  // deleted when the menu is dismissed.  Prior to managing the menu, the
  // QC plot (either bin center distribution or cmp) has the map picker.
  //
  if (!XtIsManaged(W()))	// Don't create two pickers.
  {
    int show_new_bins;

    if (_first_time)
      show_new_bins = (_plot_type != BIN_CENTERS);
    else
      show_new_bins = (int) _showbox->IsSelected(SHOW_NEW);

    createPicker(show_new_bins);
  }

  if (_first_time)
  {
    // Moving SetRadio from make to manage accomplished two things:
    //	1. GridSelectAction is the callback enacted by SetRadio and it
    //		expects the picker to be created which is done here in manage.
    //	2. Since the picker created in manage is always created in the
    //		MOVE_GRID mode, we want the radio displaying MOVE_GRID also.
    // ehs --- 07sep00
    //
    _radiobox->SetRadio(MOVE_GRID);

    // Moved SetTog from make to manage because ShowBoxAction might need picker.
    // ehs --- 07sep00
    //
    if(_plot_type != BIN_CENTERS)
      {
      _showbox->SetTog(SHOW_ORIGINAL, True);
      _showbox->SetTog(SHOW_NEW, True);
      }
    else//Dont default grid lines on if bin centers plot
      {
      _showbox->SetTog(SHOW_ORIGINAL, False);
      _showbox->SetTog(SHOW_NEW, False);
      }

    _first_time = False;
  }

  SLFPopSep::manage();

// I think this was here because the SetTogs used to be in make where
// ShowBoxAction used an old instances of the picker the used to be
// created in FgQcCmpType's and FgQcBinCenterType's plot.
//
//if(_plot_type == BIN_CENTERS)
//  {
//  if(_showbox->IsSelected(SHOW_ORIGINAL)) ShowBoxAction(this,SHOW_ORIGINAL);
//  if(_showbox->IsSelected(SHOW_NEW     )) ShowBoxAction(this,SHOW_NEW);
//  }
}
 

void FgQcGridDefinitionPickerMenu::GridSelectAction( void *data, long which )
{
FgQcGridDefinitionPickerMenu *obj = (FgQcGridDefinitionPickerMenu *)data;

  // Picker must be created.
  assert(obj->_fgqc_picker);

  obj->_mode = which;

  switch(obj->_mode)
    {
    case MOVE_GRID:
      obj->_fgqc_picker->changeHelpToken(move_grid_token, move_grid_help);
      break;

    case CENTER_GRID:       
      obj->_fgqc_picker->changeHelpToken(center_grid_token, center_grid_help);
      break;

    case CHANGE_ANGLE:
      obj->_fgqc_picker->changeHelpToken(change_angle_token, change_angle_help);
      break;

    case REFINE_ANGLE:
      obj->_fgqc_picker->changeHelpToken(refine_angle_token, refine_angle_help);
      break;

    case DEFINE_ORIGIN:
      obj->_fgqc_picker->changeHelpToken(define_origin_token, 
                                                            define_origin_help);
      break;

    case SELECT_CMP:
      obj->_fgqc_picker->changeHelpToken(select_cmp_token, select_cmp_help);
      break;

    case CMP_PLOT:
      obj->_fgqc_picker->changeHelpToken(cmp_distribution_token, 
                                                        cmp_distribution_help);
      break;

    }


// I have no idea why anyone would want to do this.
// ehs --- 07sep00
//
////Turn on the grid lines if this is a bin centers plot
//   if(obj->_mode != SELECT_CMP && obj->_mode != CMP_PLOT && 
//      obj->_plot_type == BIN_CENTERS)
//     {
//     obj->_showbox->SetTog(SHOW_ORIGINAL, True);
//     obj->_showbox->SetTog(SHOW_NEW, True);
//     }
}


void FgQcGridDefinitionPickerMenu::ShowBoxAction( void *data, long which )
{
FgQcGridDefinitionPickerMenu *obj = (FgQcGridDefinitionPickerMenu *)data;
FgQcGridDefinitionPicker *picker= (FgQcGridDefinitionPicker *)obj->_fgqc_picker;
FgQcCmpType *cmp_plot;
FgQcBinCenterType  *bin_center_plot;

  
  

  

  if(obj->_plot_type == CMP_DISTRIBUTION)
    {
    cmp_plot = (FgQcCmpType *)obj->fgqcPlotClass(); 
    switch(which)
      {
      case SHOW_ORIGINAL:
        if(obj->_showbox->IsSelected(SHOW_ORIGINAL))
           cmp_plot->ShowGridMatrix(True);
        else
           cmp_plot->ShowGridMatrix(False);
        break;

      case SHOW_NEW:
        if(obj->_showbox->IsSelected(SHOW_NEW))
           picker->ShowVectors(True);
        else
           picker->ShowVectors(False);
        break;
      }
    }
  else
    {
    bin_center_plot = (FgQcBinCenterType *)obj->fgqcPlotClass();
    switch(which)
      {
      case SHOW_ORIGINAL:
        if(obj->_showbox->IsSelected(SHOW_ORIGINAL))
           bin_center_plot->ShowGridMatrix(True);
        else
           bin_center_plot->ShowGridMatrix(False);
        break;

      case SHOW_NEW:
        if(obj->_showbox->IsSelected(SHOW_NEW))
           picker->ShowVectors(True);
        else
           picker->ShowVectors(False);
        break;
      } 
    }
}

void FgQcGridDefinitionPickerMenu::button_control( void *data, long which )
{
FgQcGridDefinitionPickerMenu *obj = (FgQcGridDefinitionPickerMenu *)data;
FgQcGridDefinitionPicker *picker= (FgQcGridDefinitionPicker *)obj->_fgqc_picker;

  switch(which)
    {
    case NEW_TRANSFORM:
      picker->saveTransform();
    break;

    case ORIGINAL_TRANSFORM:
      picker->originalTransform();
    break;


    }

}



void FgQcGridDefinitionPickerMenu::removeButton()
{
  
  removePicker();
  SLFPopSep::removeButton();

}

Boolean FgQcGridDefinitionPickerMenu::createVectors()
{
  if (_fgqc_picker != NULL)
    return(_fgqc_picker->createVectors());
  else
    return True;	// FgQcGridDefinitionPicker::createVectors returns True
			// if _can_show is False.
}


//=============================================================================
//============= Grid Definition type plot picker        =======================
//=============================================================================

FgQcGridDefinitionPicker::FgQcGridDefinitionPicker(FgSeisPlot *sp,
                             FgQcPickerMenu  *menu,
                             int can_show,
                             char               *mode,
                             const char * const helpToken,
                             const char * const helpFallback) 
                           : FgQcPicker(sp,menu,mode,helpToken,helpFallback),
			     _can_show(can_show)
                             
{
  _grid_lines         = NULL;
  _xvect              = NULL;
  _yvect              = NULL;
  _Vector             = NULL;
  _vect_data          = NULL;
  _angle_xvect        = NULL;
  _angle_yvect        = NULL;
  _angle_Vector       = NULL;
  _angle_vect_data    = NULL; 
  _previous_points    = 0;
  _vect_ll = new SeisVectLinkedList();
  _vect_ll->addPlot(sp,True);
  _menu->fgqcPlotClass()->getCoordinates(&_image_xmin, &_image_xmax, 
                                         &_image_ymin, &_image_ymax); 
  _vector_stat = False;

// Moved to parameter and initializer.
// ehs --- 14sep00
//
//_can_show = True;

  _old_pick_mode = MOVE_GRID;

  if (_can_show)
    createVectors();
}


FgQcGridDefinitionPicker::~FgQcGridDefinitionPicker()
{
void *p;
Vector *ptr;
  
  if(_Vector != NULL)  _vect_ll->remove(_Vector);

  if(_grid_lines != NULL)  delete _grid_lines;
  if(_xvect != NULL)       free(_xvect);
  if(_yvect != NULL)       free(_yvect);
  if(_angle_xvect != NULL) free(_angle_xvect);
  if(_angle_yvect != NULL) free(_angle_yvect);

  for(ptr = _vect_ll->top(&p); ptr; ptr = _vect_ll->next(&p))
    if( ptr->getData() != NULL ) delete ptr->getData();

  delete _vect_ll; 

  if(_vect_data != NULL)  delete _vect_data;
  if(_angle_vect_data != NULL) delete _angle_vect_data;

}


//=============================================================================
//============= Catch-all for pick button actions ==============================
//=============================================================================
void FgQcGridDefinitionPicker::buttonAny(int x1, int x2, int y1, int y2, 
                                         int button, Action action, 
                                         Modifier /*modifier*/)
{


  switch(_menu->_mode)
    {
    case MOVE_GRID:
      moveGrid(x1, x2, y1, y2, button, action);
      break;

    case CENTER_GRID:       
      centerGrid(x1, x2, y1, y2, button, action);
      break;

    case CHANGE_ANGLE:
      changeAngle(x1, x2, y1, y2, button, action);
      break;

    case REFINE_ANGLE:
      refineAngle(x1, x2, y1, y2, button, action);
      break;

    case DEFINE_ORIGIN:
      defineOrigin(x1, x2, y1, y2, button, action);
      break;

    case SELECT_CMP:
      selectCmp(x1, x2, y1, y2, button, action);
      break;

    case CMP_PLOT:
      selectPlot(x1, x2, y1, y2, button, action);
      break;

    }



}



//=============================================================================
//============= Slide the grid location =======================================
//=============================================================================
void FgQcGridDefinitionPicker::moveGrid(int x1, int x2, int y1, int y2, 
                                        int button, Action action)
{
unsigned int line_width = 1;

  if(button == 1 && action == press)
    {
    if(_grid_lines == NULL)
      {
      _grid_lines = new GridLines(_menu->fgqcPlot()->fg());
      if(_menu->fgqcPlot()->getPlottedCoordinateSystem() == SURVEYSYSTEM)
        _grid_lines->usingDistanceSystem();
      else
        _grid_lines->usingGridSystem();
      }
    _grid_lines->buttonDown();
    _start_x =  _sp->xWC(x1);
    _start_y =  _sp->yWC(y1);
    }

  if(button == 1 && action == motion)
    {
    if(_angle_xvect == NULL || _angle_yvect == NULL)
      {
      _angle_xvect = (float *)calloc(1,(unsigned int)(2*sizeof(float)));
      _angle_yvect = (float *)calloc(1,(unsigned int)(2*sizeof(float)));
      }

    _angle_xvect[0] = _sp->xWC(x1);
    _angle_xvect[1] = _sp->xWC(x2);
    _angle_yvect[0] = _sp->yWC(y1);
    _angle_yvect[1] = _sp->yWC(y2);

    if(_angle_vect_data == NULL || _angle_Vector == NULL)
      {
      if(_angle_vect_data == NULL)
        _angle_vect_data = new VectData((int)(2),_angle_xvect,_angle_yvect);
      if(_angle_Vector == NULL)
        _angle_Vector = _vect_ll->add(_angle_vect_data,"red",line_width,True);
      }
    else
      {
      _angle_vect_data->replace(0,(int)2,(int)2, 
                          &_angle_xvect[0],&_angle_yvect[0]);
      } 
    _grid_lines->buttonMotion();
    _end_x =  _sp->xWC(x2);
    _end_y =  _sp->yWC(y2); 
    _grid_lines->changeOrigin (_start_x, _start_y, _end_x, _end_y);
    }

  if(button == 1 && action == release)
    {
    _grid_lines->buttonUp();
    _end_x =  _sp->xWC(x2);
    _end_y =  _sp->yWC(y2); 
    _grid_lines->changeOrigin (_start_x, _start_y, _end_x, _end_y);
    if(_angle_Vector != NULL)
      {
      _vect_ll->remove(_angle_Vector);
      _angle_Vector = NULL;
      }
    if(_angle_xvect != NULL)
      { 
      free(_angle_xvect); 
      _angle_xvect = NULL;
      }
    if(_angle_yvect != NULL)
      {
      free(_angle_yvect); 
      _angle_yvect = NULL;
      }
    } 

}


//=============================================================================
//============= Center a grid bin at mouse click location  ====================
//=============================================================================
void FgQcGridDefinitionPicker::centerGrid(int /*x1*/,int x2, int/*y1*/,int y2, 
                                          int button, Action action)
{
  if(button == 1 && action == press)
    {
    if(_grid_lines == NULL)
      {
      _grid_lines = new GridLines(_menu->fgqcPlot()->fg());
      if(_menu->fgqcPlot()->getPlottedCoordinateSystem() == SURVEYSYSTEM)
        _grid_lines->usingDistanceSystem();
      else
        _grid_lines->usingGridSystem();
      }
    }

  if(button == 1 && action == release)
    {
    _end_x =  _sp->xWC(x2);
    _end_y =  _sp->yWC(y2); 
    _grid_lines->refineBinCenter (_end_x, _end_y);
    }


}



//=============================================================================
//============= Change the angle of the grid transform  =======================
//=============================================================================
void FgQcGridDefinitionPicker::changeAngle(int x1, int x2, int y1, int y2, 
                                           int button, Action action)
{
unsigned int line_width = 1;


  if(button == 1 && action == press)
    {
    if(_grid_lines == NULL)
      {
      _grid_lines = new GridLines(_menu->fgqcPlot()->fg());
      if(_menu->fgqcPlot()->getPlottedCoordinateSystem() == SURVEYSYSTEM)
        _grid_lines->usingDistanceSystem();
      else
        _grid_lines->usingGridSystem();
      }
    _start_x =  _sp->xWC(x1);
    _start_y =  _sp->yWC(y1);
    }

  if(button == 1 && action == motion)
    {
    if(_angle_xvect == NULL || _angle_yvect == NULL)
      {
      _angle_xvect = (float *)calloc(1,(unsigned int)(2*sizeof(float)));
      _angle_yvect = (float *)calloc(1,(unsigned int)(2*sizeof(float)));
      }

    _angle_xvect[0] = _sp->xWC(x1);
    _angle_xvect[1] = _sp->xWC(x2);
    _angle_yvect[0] = _sp->yWC(y1);
    _angle_yvect[1] = _sp->yWC(y2);

    if(_angle_vect_data == NULL || _angle_Vector == NULL)
      {
      if(_angle_vect_data == NULL)
        _angle_vect_data = new VectData((int)(2),_angle_xvect,_angle_yvect);
      if(_angle_Vector == NULL)
        _angle_Vector = _vect_ll->add(_angle_vect_data,"red",line_width,True);
      }
    else
      {
      _angle_vect_data->replace(0,(int)2,(int)2, 
                          &_angle_xvect[0],&_angle_yvect[0]);
      }    
    _end_x =  _sp->xWC(x2);
    _end_y =  _sp->yWC(y2);
    _grid_lines->defineRotationAngle (_start_x, _start_y, _end_x, _end_y);
    }

  if(button == 1 && action == release)
    {
    _end_x =  _sp->xWC(x2);
    _end_y =  _sp->yWC(y2); 
    _grid_lines->defineRotationAngle (_start_x, _start_y, _end_x, _end_y);
    if(_angle_Vector != NULL)
      {
      _vect_ll->remove(_angle_Vector);
      _angle_Vector = NULL;
      }
    if(_angle_xvect != NULL)
      { 
      free(_angle_xvect); 
      _angle_xvect = NULL;
      }
    if(_angle_yvect != NULL)
      {
      free(_angle_yvect); 
      _angle_yvect = NULL;
      }
    } 

}


//=============================================================================
//============= Refine the angle of the grid transform  =======================
//=============================================================================
void FgQcGridDefinitionPicker::refineAngle(int /*x1*/,int x2,int/*y1*/,int y2, 
                                           int button, Action action)
{
  if(button == 1 && action == press)
    {
    if(_grid_lines == NULL)
      {
      _grid_lines = new GridLines(_menu->fgqcPlot()->fg());
      if(_menu->fgqcPlot()->getPlottedCoordinateSystem() == SURVEYSYSTEM)
        _grid_lines->usingDistanceSystem();
      else
        _grid_lines->usingGridSystem();
      }
    }

  if(button == 1 && action == release)
    {
    _end_x =  _sp->xWC(x2);
    _end_y =  _sp->yWC(y2); 
    _grid_lines->refineRotationAngle (_end_x, _end_y);
    }


}



//=============================================================================
//============= Make the mouse click 0,0 of the grid bin ======================
//=============================================================================
void FgQcGridDefinitionPicker::defineOrigin(int/*x1*/,int x2,int/*y1*/,int y2, 
                                            int button, Action action)
{
  if(button == 1 && action == press)
    {
    if(_grid_lines == NULL)
      {
      _grid_lines = new GridLines(_menu->fgqcPlot()->fg());
      if(_menu->fgqcPlot()->getPlottedCoordinateSystem() == SURVEYSYSTEM)
        _grid_lines->usingDistanceSystem();
      else
        _grid_lines->usingGridSystem();
      }
    }

  if(button == 1 && action == release)
    {
    _end_x =  _sp->xWC(x2);
    _end_y =  _sp->yWC(y2); 
    _grid_lines->defineOrigin(_end_x, _end_y);
    }


}




//=============================================================================
//============= Update the cmp gathers table to mouse location selected =======
//=============================================================================

void FgQcGridDefinitionPicker::selectCmp(int x1,int /*x2*/,int y1, int /*y2*/, 
                                         int /*button*/, Action action)
{
double xloc, yloc;
FieldGeometry *fg;
long i;

  if(action == release)
    {
    fg = _menu->fgqcPlot()->fg();
    xloc = _sp->xWC(x1);
    yloc = _sp->yWC(y1);
    if(_menu->fgqcPlot()->getPlottedCoordinateSystem() == SURVEYSYSTEM)
      i = fg->findNearestCmp(xloc ,yloc);
    else
      i = fg->findNearestCmpUsingGrid(xloc, yloc);
    if(i >= 0) fg->setActiveCmpIndex(i);
    }
}



//=============================================================================
//============= Select an area and create a cmp distribution plot  ============
//=============================================================================
void FgQcGridDefinitionPicker::selectPlot(int x1, int x2, int y1, int y2, 
                                          int button, Action action)
{
unsigned int line_width = 1;
float xmin, xmax, ymin, ymax;
  if(button == 1 && action == press)
    {
    if(_angle_xvect == NULL || _angle_yvect == NULL)
      {
      _angle_xvect = (float *)calloc(1,(unsigned int)(5*sizeof(float)));
      _angle_yvect = (float *)calloc(1,(unsigned int)(5*sizeof(float)));
      }
    else//necessary because this is the same vector used in drawing from
        //origin in other methods that are only 2 elements
      {
      _angle_xvect = (float *)realloc(_angle_xvect,
                                      (unsigned int)(5*sizeof(float)));
      _angle_yvect = (float *)realloc(_angle_yvect,
                                      (unsigned int)(5*sizeof(float)));
      } 
    _angle_xvect[0] = _sp->xWC(x1);
    _angle_yvect[0] = _sp->yWC(y1);
    _angle_xvect[1] = _sp->xWC(x1);
    _angle_yvect[3] = _sp->yWC(y1);
    _angle_xvect[4] = _sp->xWC(x1);
    _angle_yvect[4] = _sp->yWC(y1);
    }


  if(button == 1 && action == motion)
    {
    _angle_yvect[1] = _sp->yWC(y2);
    _angle_xvect[2] = _sp->xWC(x2);
    _angle_yvect[2] = _sp->yWC(y2);
    _angle_xvect[3] = _sp->xWC(x2);
    if(_angle_vect_data == NULL || _angle_Vector == NULL)
      {
      if(_angle_vect_data == NULL)
        _angle_vect_data = new VectData((int)(5),_angle_xvect,_angle_yvect);
      if(_angle_Vector == NULL)
        _angle_Vector = _vect_ll->add(_angle_vect_data,"red",line_width,True);
      }
    else
      {
      _angle_vect_data->replace(0,_angle_vect_data->getNumPts(),(int)5, 
                          &_angle_xvect[0],&_angle_yvect[0]);
      } 
    }


  if(button == 1 && action == release)
    {
    if(_angle_Vector != NULL)
      {
      _vect_ll->remove(_angle_Vector);
      _angle_Vector = NULL;
      }
    if(_angle_xvect != NULL)
      { 
      free(_angle_xvect); 
      _angle_xvect = NULL;
      }
    if(_angle_yvect != NULL)
      {
      free(_angle_yvect); 
      _angle_yvect = NULL;
      }

    //Make sure area selected is at least 5 pixels high and wide
    int xtemp = x2 - x1;
    xtemp =  (xtemp > 0) ? xtemp : -xtemp;
    int ytemp = y2 - y1;
    ytemp =  (ytemp > 0) ? ytemp : -ytemp;
    if( (xtemp < 5) || (ytemp < 5) )
      {
      SLErrorPop *temp = 
        new SLErrorPop(_menu->topWidget(), "Error", "Too small area selected");
      return;
      }

    xmin = MinimumValue(_sp->xWC(x1),_sp->xWC(x2));
    xmax = MaximumValue(_sp->xWC(x1),_sp->xWC(x2));
    ymin = MaximumValue(_sp->yWC(y1),_sp->yWC(y2));
    ymax = MinimumValue(_sp->yWC(y1),_sp->yWC(y2));

    _menu->fgqcPlot()->makeIndirectPlot(CMP_DISTRIBUTION, 8.0, 8.0, 
                               xmin, ymin, xmax, ymax, 
                               _menu->fgqcPlot()->getPlottedCoordinateSystem());


    } 

}

//=============================================================================
//============= Create the rubber band grid vectors   =========================
//=============================================================================
void FgQcGridDefinitionPicker::saveTransform()
{
FgQcCmpType *cmp_plot;
FgQcBinCenterType  *bin_center_plot;


//  if(_menu->fgqcPlot()->fg()->getDataLock()) 
    if(!_menu->fgqcPlot()->fg()->allowModifyingGridTransform()) 
    {
    SLErrorPop *temp = new SLErrorPop(_menu->topWidget(), "Error", locked);
    return;
    }

  if(_grid_lines != NULL)
    {
    _grid_lines->saveNewTransform();
    if(_menu->_plot_type == CMP_DISTRIBUTION)
      {
      cmp_plot =  (FgQcCmpType *)_menu->fgqcPlotClass();
      cmp_plot->modifyGridMatrix();
      }
    else
      {
      bin_center_plot =  (FgQcBinCenterType *)_menu->fgqcPlotClass();
      bin_center_plot->modifyGridMatrix();
      }
    if(_Vector != NULL) _vect_ll->remove(_Vector);
    _Vector = NULL;
    if(_vect_data != NULL) delete _vect_data;
    _vect_data = NULL;
    PrimSupport::updateEverything();
    }

}

//=============================================================================
//============= Create the rubber band grid vectors   =========================
//=============================================================================
void FgQcGridDefinitionPicker::originalTransform()
{
FgQcCmpType *cmp_plot;
FgQcBinCenterType  *bin_center_plot;

  if(_grid_lines != NULL)
    {
    _grid_lines->resetTestingTransform();
    if(_menu->_plot_type == CMP_DISTRIBUTION)
      {
      cmp_plot =  (FgQcCmpType *)_menu->fgqcPlotClass();
      cmp_plot->modifyGridMatrix();
      }
    else
      {
      bin_center_plot =  (FgQcBinCenterType *)_menu->fgqcPlotClass();
      bin_center_plot->modifyGridMatrix();
      }
    if(_Vector != NULL) _vect_ll->remove(_Vector);
    _Vector = NULL;
    if(_vect_data != NULL) delete _vect_data;
    _vect_data = NULL;
    PrimSupport::updateEverything();
    }

}
//=============================================================================
//============= Create the rubber band grid vectors   =========================
//=============================================================================
Boolean FgQcGridDefinitionPicker::createVectors()
{
long i;
const float *xgrid, *ygrid;
unsigned int line_width = 1;
int stride = 1;

  //do not draw grid lines if it has been turned off with the menu
  if(!_can_show) return(True);

  if(_grid_lines == NULL)
    {
    _grid_lines = new GridLines(_menu->fgqcPlot()->fg());
    if(_menu->fgqcPlot()->getPlottedCoordinateSystem() == SURVEYSYSTEM)
       _grid_lines->usingDistanceSystem();
    else
       _grid_lines->usingGridSystem();  
    } 

  _grid_lines->calculateGridLines(_image_xmin, _image_ymin, 
                                  _image_xmax, _image_ymax, stride);



  if(_xvect == NULL)
     {
     _xvect = (float *)calloc(1,(unsigned int)(_grid_lines->nPoints()
                                 *sizeof(float)));
     _yvect = (float *)calloc(1,(unsigned int)(_grid_lines->nPoints()
                                 *sizeof(float)));
     }
  else if(_previous_points != _grid_lines->nPoints()){
     _xvect  =(float *)realloc(_xvect,(unsigned int)(_grid_lines->nPoints()
                               *sizeof(float)));
     _yvect  =(float *)realloc(_yvect,(unsigned int)(_grid_lines->nPoints()
                               *sizeof(float)));
     }

  if(_xvect == NULL || _yvect == NULL)
    {
    printf("Couldnt allocate vectors for showing grid boxes");
    return(_vector_stat = False);
    }

  



  xgrid = _grid_lines->xValues();
  ygrid = _grid_lines->yValues();
  for(i = 0; i < _grid_lines->nPoints(); i++)
    {
    _xvect[i] = xgrid[i];
    _yvect[i] = ygrid[i];
    }


  if(_vect_data == NULL || _Vector == NULL)
    {
    if(_vect_data == NULL)
      _vect_data = new VectData((int)(_grid_lines->nPoints()),_xvect,_yvect);
    if(_Vector == NULL)
      _Vector = _vect_ll->add(_vect_data,"red",line_width, True);
    }
  else
    {
    _vect_data->replace(0,(int)_previous_points,(int)_grid_lines->nPoints(),
                        &_xvect[0],&_yvect[0]);
    }   

  _previous_points = _grid_lines->nPoints();


  return(_vector_stat = True);
}


//=============================================================================
//============= Hide or show the new rubber band grid =========================
//============= Have to delete or create since rubber =========================
//============= band vectors do not support visible calls =====================
//=============================================================================
void FgQcGridDefinitionPicker::ShowVectors(Boolean show)
{


  if(show)
    {
    _can_show = True;
    createVectors();
    }
  else 
    {
    _can_show = False;
    if(_Vector != NULL)
      {
      _vect_ll->remove(_Vector);
      _Vector = NULL;
      if(_vect_data != NULL) delete _vect_data;
      _vect_data = NULL;
      }
    }

}
