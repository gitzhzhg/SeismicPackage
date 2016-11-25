//The following class allows the option menu to set it's buttons attachements
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
//which prevents circular dependency warnings

#include "fgqc/fgqc_option_pop.hh"

FgQcOptionPop::FgQcOptionPop(FgQcPlot *p, char *name,HelpCtx  hctx)
             : SLFPopSep(p, name, FP_DOOK|FP_DOHELP, hctx, False, False),
               _qc_plot(p)
{

  
  _menu_op        = True;
  _color_op       = True;
  _cbar_op        = True;
  _zoomupseparate = True;
  _zoomup         = True;
  _zoomdown       = True;
  _zoomoriginal   = True;
  _hill_shader    = True;
  _map_overlay    = True;
  _picking_menu   = True;

}





Widget FgQcOptionPop::make(Widget p)
{ 


  if (! made() )
    {
    SLFPopSep::make(p);
    _buttons = _qc_plot->_buttons;
    XtVaSetValues( _buttons->W(),  
                   XmNtopAttachment,  XmATTACH_FORM,
                   XmNtopOffset,      5,
                   XmNleftAttachment, XmATTACH_FORM, 
                   XmNleftOffset,     5, 
                   XmNrightAttachment, XmATTACH_FORM, 
                   XmNrightOffset,    5, NULL);
    setOptions(MENU_OP,_menu_op,True);//does not change anything, insures set
    }
  return topWidget();
}


void FgQcOptionPop::setOptions(int ident, Boolean state, Boolean set_all)
{

  _buttons = _qc_plot->_buttons;
  if(!set_all)
    {
    switch(ident)
      {
      case MENU_OP:
      _menu_op = state;
      if(made()) XtSetSensitive(_buttons->pushW(MENU_OP), _menu_op);
      break;

      case COLOR_OP:
      _color_op = state;
      if(made()) XtSetSensitive(_buttons->pushW(COLOR_OP), _color_op);
      break;

      case CBAR_OP:
      _cbar_op = state;
      if(made()) XtSetSensitive(_buttons->pushW(CBAR_OP), _cbar_op);
      break;

      case ZOOMUPSEPARATE:
      _zoomupseparate = state;
      if(made())XtSetSensitive(_buttons->pushW(ZOOMUPSEPARATE),_zoomupseparate);
      break;

      case ZOOMUP:
      _zoomup = state;
      if(made()) XtSetSensitive(_buttons->pushW(ZOOMUP), _zoomup);
      break;

      case ZOOMDOWN:
      _zoomdown = state;
      if(made()) XtSetSensitive(_buttons->pushW(ZOOMDOWN), _zoomdown);
      break;

      case ZOOMORIGINAL:
      _zoomoriginal = state;
      if(made()) XtSetSensitive(_buttons->pushW(ZOOMORIGINAL), _zoomoriginal);
      break;

      case HILL_SHADER:
      _hill_shader = state;
      if(made()) XtSetSensitive(_buttons->pushW(HILL_SHADER), _hill_shader);
      break;

      case MAP_OVERLAY:
      _map_overlay = state;
      if(made()) XtSetSensitive(_buttons->pushW(MAP_OVERLAY), _map_overlay);
      break;

      case PICKING_MENU:
      _picking_menu = state;
      if(made()) XtSetSensitive(_buttons->pushW(PICKING_MENU), _picking_menu);
      break;

      }
    }
  else
    {
    if(made())
      {
      XtSetSensitive(_buttons->pushW(MENU_OP),        _menu_op);
      XtSetSensitive(_buttons->pushW(COLOR_OP),       _color_op);
      XtSetSensitive(_buttons->pushW(CBAR_OP),        _cbar_op);
      XtSetSensitive(_buttons->pushW(ZOOMUPSEPARATE), _zoomupseparate);
      XtSetSensitive(_buttons->pushW(ZOOMUP),         _zoomup);
      XtSetSensitive(_buttons->pushW(ZOOMDOWN),       _zoomdown);
      XtSetSensitive(_buttons->pushW(ZOOMORIGINAL),   _zoomoriginal);
      XtSetSensitive(_buttons->pushW(HILL_SHADER),    _hill_shader);
      XtSetSensitive(_buttons->pushW(MAP_OVERLAY),    _map_overlay);
      XtSetSensitive(_buttons->pushW(PICKING_MENU),   _picking_menu);  
      }
    }

  


}
