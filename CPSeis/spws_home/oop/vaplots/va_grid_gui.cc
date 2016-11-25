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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//=========================================================================
//========== Semblance plot gui class                            ==========
//========== Author Michael L. Sherrill 08/97                    ==========
//=========================================================================

// $Id: va_grid_gui.cc,v 1.3 2005/12/13 16:21:22 spws Exp $
// $Name: 12-13-2005 $

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Xm.h>
#include "vaplots/va_grid_gui.hh"
#include "vaplots/va_grid_plot.hh"
#include "sl/paintset_collection.hh"


static String  defres[]= {
  "_popup.title:                    Velocity Grid Menu",
  "*pwidth.value:                   1.0",
  "*pheight.value:                  1.0",
  NULL, };


//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaGridGui::VaGridGui( Widget            p,
                      char              *name,
                      HelpCtx           hctx,
                      VaGridPlot        *plot)
                : SeisGridPop(p,name,plot->SP(),hctx),
                  _plot(plot)

{
  setDefaultResources( p, name, defres);
  _first_time = True;

}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaGridGui::~VaGridGui()
{

}

//=========================================================================
//============== The Seisplot in the window has changed ===================
//=========================================================================
void VaGridGui::seisPlotChanged()
{
  printf("need to test this gui's seisPlotChanged method\n");
  _header_box->SetValue(PWIDTH, _sp->gridWidth());
  _header_box->SetValue(PHEIGHT,_sp->gridHeight());
  _header_box->SetValue(PLEFT,  _sp->gridX1());
  _header_box->SetValue(PRIGHT, _sp->gridX2());
  _header_box->SetValue(PTOP,   _sp->gridY2());
  _header_box->SetValue(PBOTTOM,_sp->gridY1());
}

//=========================================================================
//===================== Make       ========================================
//=========================================================================
Widget VaGridGui::make(Widget p)
{
  if ( made() ) return topWidget();

  SeisGridPop::make(p);

  _enforcebox->SetTog(ENFORCE,False);
  //  _header_box->SetValue(PWIDTH,  (float)4.0 );
  // _header_box->SetValue(PHEIGHT, (float)4.0 );

  return topWidget();
}


//=========================================================================
//========================== Check for valid input ========================
//=========================================================================
Boolean VaGridGui::ValidInput()
{
 Boolean stat;

 if (made()) 
   {
    stat= SLFPopSep::ValidInput();
    /*
    if (stat)
         stat= _params_box->validate();

    if ( (_vmin >= _vmax) && (stat) ) 
      {
      _params_box->popError( "vmax must be greater than vmin");
      stat= False;
      }

    if ( (_pdmin >= _pdmax) && (stat) ) 
      {
      _params_box->popError( "pdmax must be greater than pdmin");
      stat= False;
      }
      */
   }
 else//not made yet
   { 
   stat= True;
   }

 return (stat); 
}


//=========================================================================
//===================== Take action on Ok Apply etc========================
//=========================================================================
void VaGridGui::DoAction()
{
  
  setParameters();

  SeisGridPop::DoAction();

  // _plot->plot(); plot method is called from SeisGridPop's DoAction

}


//=========================================================================
//===================== Set parameters for plotting =======================
//=========================================================================
void VaGridGui::setParameters()
{
float range;

//make sure there is some plot width
  if(_left == _right) 
    {
    _right  += 1.0;
    }
  if(_top  == _bottom) 
    {
    _top    += 1.0;
    }

  //make plot 10 percent larger than user coordinates
  range  = _right - _left;
  _left  = _left  - (range * .05);
  _right = _right + (range * .05);
  
  range  = _bottom - _top;
  _top   = _top  - (range * .05);
  _bottom= _bottom + (range * .05);

  

  _sp->setTimingLines(0.0,0.0);
  _sp->setSymetricalAnnotation(_left,_right,_top,_bottom);
  _sp->setDrawXlines(False);
  _sp->setDrawYlines(False);
  _sp->setGridXYS(_left, _right, _bottom, _top);
  _sp->setGridWidth(_pwidth);
  _sp->setGridHeight(_pheight);

  //make image black
  _sp->setGridColor(PaintsetCollection::black(XtScreen(_sp->imageGraphic())));
}

//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaGridGui::setToFileDefaults()
{

}


//=========================================================================
//====================  Update public method    ===========================
//=========================================================================
void VaGridGui::updateParams(Boolean /*update_file_limits*/)
{
  //suggest appropriate plot size based on 2d or 3d
  //if(_plot->minXbin() == _plot->maxXbin()) 
  //  _header_box->SetValue( PWIDTH,  1.0F );
  //if(_plot->minYbin() == _plot->maxYbin()) 
  // _header_box->SetValue( PHEIGHT, 1.0F ); 

  _header_box->SetValue( PLEFT,   _plot->minXbin() );
  _header_box->SetValue( PRIGHT,  _plot->maxXbin() );
  _header_box->SetValue( PTOP,    _plot->maxYbin() );
  _header_box->SetValue( PBOTTOM, _plot->minYbin() );
}



