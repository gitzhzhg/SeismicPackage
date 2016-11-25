//********************************************
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
//Author Michael L. Sherrill 05/97
//Creates menu to control random line generation
//********************************************
#include <Xm/Label.h>
#include "sp/seis_plot.hh"
#include "cube/cube.hh"
#include "cube/cube_display.hh"
#include "cube/cube_random_line_pop.hh"
#include "cube/cube_random_line.hh"
#include "cube/cube_random_line_picker.hh"
#include "cube/cube_random_line_xytable.hh"
#include "cube/cube_random_line_plot.hh"
#include "cube/cube_master.hh"
#include "cprim.h"
#include "sl/shell_stat_msg.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_prim.hh"
#include "vect/ll_seis_vect.hh"


enum{START_OVER = 98};

//============================================================================
//====================== Constructor =========================================
//============================================================================
CubeRandomLinePop::CubeRandomLinePop( Widget              p,
                                      char                *name,
                                      HelpCtx             hctx,
                                      CubeRandomLine      *cube_random_line,
                                      CubeDisplay         *cube_display)
      : SLFPopSep(p,name,FP_DOAPPLY|FP_DOREMOVE,hctx,False,False)
{

  _creation_widget = p;
  _cube_random_line = cube_random_line;
  _cube_display = cube_display;
  _cube_number = 0;
  _hctx = hctx;  
  _xy_table = NULL;
  _cube_random_line_plot = _cube_random_line->cubeRandomLinePlot(
                                p,"Random Line Plot", _hctx, this, 
                                NUMCOLORS, _cube_display);
  addExtraButton("Start Over", START_OVER);

}



//============================================================================
//====================== Destructor                  =========================
//============================================================================
CubeRandomLinePop::~CubeRandomLinePop()
{
  CubeMaster::instance()->delInformer(this);
}


//============================================================================
//====================== Change the active random line =======================
//============================================================================
void CubeRandomLinePop::changeActiveRandomLine(CubeRandomLine *crl)
{
  //clearXYTable(_cube_random_line);

  //unmanage the previous randomline plot if it is managed
  if (_cube_random_line_plot) _cube_random_line_plot->unmanage();

  //unmange this popup
  unmanage();

  if (_cube_random_line) {
    //hide any existing random line vectors on the previous cube.
    _cube_random_line->hideVectors();

    //remove pickbase from plot
    _cube_random_line->removePicker();
  }

  //assign new randomline to this popup
  _cube_random_line = crl;

  //hide any existing random line vectors on this cube.
  _cube_random_line->hideVectors();

  //create and/or manage the new randomline plot popup
  _cube_random_line_plot = _cube_random_line->cubeRandomLinePlot(
                                    _creation_widget,"Random Line Plot", 
                                    _hctx, this, NUMCOLORS, _cube_display);
  _cube_random_line_plot->make(_creation_widget);  
  _cube_random_line_plot->display();
}


//============================================================================
//====================== Make popup         ==================================
//============================================================================
Widget CubeRandomLinePop::make(Widget p)
{
static SLTog sort_picks[]      = { {"",  NULL, 0 },};
sort_picks[0].target= &_enforce_sort;

  if ( made() ) return topWidget();

  _cube_random_line_plot->make(p); 
 
  sprintf(_cube_id, "Cube %d = ",_cube_number);

  _xy_table = new CubeRandomLineXYTable(this,"xytable",_cube_random_line);

  if(_cube_random_line->getPicker())
    _cube_random_line->getPicker()->setXYTable(_xy_table);
  Widget parent = p ? p : wParent();
  ShellStatMsg bld_info(parent, "Building Random Line Menu...");

  _sort_toggle = new SLTogBox(this, "Enforce Sort",getHelpCtx(),
                              sort_picks, XtNumber(sort_picks), 
                              False, False, True );
  _sort_toggle->setAltChoiceAction((SLToggleButtonfunc)sortActionToggle,
                                    this);
  SLFPopSep::make(p);

  defaultButton(FP_DOAPPLY, False);

  wprocShowMsg(_sort_toggle->TogW(0)," ");

  setAttachments();

  return topWidget();

}


//============================================================================
//====================== Manage popup ========================================
//============================================================================
void CubeRandomLinePop::manage()
{
  if(getDisplayedCube() == NULL) return;

  if(_cube_random_line->getPicker())
    {
    _cube_random_line->getPicker()->setXYTable(_xy_table);
    resetXYTable(_cube_random_line);
    }

 _cube_random_line->showVectors();

  SLFPopSep::manage();

  updateCubeInfo();
}


//============================================================================
//====================== Make and manage popup       =========================
//============================================================================
void CubeRandomLinePop::makeAndManage(Widget p)
{
Widget parent;

  parent = p ? p : wParent();
  make(parent);
  manage();
  SLFPopSep::makeAndManage();

}


//============================================================================
//======================  attachments       ==================================
//============================================================================
void CubeRandomLinePop::setAttachments()
{
  XtVaSetValues(_xy_table->W(), XmNtopAttachment,     XmATTACH_FORM,
                                XmNleftAttachment,    XmATTACH_FORM,
                                XmNrightAttachment,   XmATTACH_FORM,
                                NULL);

  XtVaSetValues(_sort_toggle->W(),
                                XmNtopAttachment,     XmATTACH_WIDGET,
                                XmNtopWidget,         _xy_table->W(),
                                XmNtopOffset,         10,
                                XmNleftAttachment,    XmATTACH_FORM,
                                XmNleftOffset,        10, NULL);

  Widget sort_label = XtVaCreateManagedWidget("Enforce Sorting", 
                                xmLabelWidgetClass,   topWidget(),
                                XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET,
                                XmNtopWidget,         _sort_toggle->W(),
                                XmNtopOffset,         5,
                                XmNleftAttachment,    XmATTACH_WIDGET,
                                XmNleftWidget,        _sort_toggle->W(), NULL);  

  _cube_id_label = XtVaCreateManagedWidget(_cube_id, 
                                xmLabelWidgetClass,   topWidget(),
                                XmNtopAttachment,     XmATTACH_WIDGET,
                                XmNtopWidget,         _sort_toggle->W(),
                                XmNleftAttachment,    XmATTACH_FORM,
                                XmNleftOffset,        10,
                                XmNbottomAttachment,  XmATTACH_WIDGET,
                                XmNbottomWidget,      bottomSeparator(),
                                XmNbottomOffset,      10, NULL);

  
  _sort_toggle->SetTog(0,True);  

  setTitle("Random Line Generator");
}


//============================================================================
//====================== Clear selections         ============================
//============================================================================
void CubeRandomLinePop::extraButton(int ident)
{
  if(ident == START_OVER)
    clearXYTable(_cube_random_line);
}

//============================================================================
//====================== Generate the plot        ============================
//============================================================================
void CubeRandomLinePop::DoAction()
{
int stat;

  stat = _cube_random_line->findTraces(True);

  if(stat) 
    {
    stat = _cube_random_line_plot->plot();
    if(stat != PlotImage::PlotSuccess)
      _cube_random_line->errorPopUp("Error in plot generation.");
    }
  else
    {
    _cube_random_line->errorPopUp("Requested data not in range.");
    }


}

//============================================================================
//====================== Remove the popup and picker =========================
//============================================================================
void CubeRandomLinePop::removeButton()
{
  //clearXYTable(_cube_random_line);

  _cube_random_line->removePicker();

  SLFPopSep::removeButton();
}

//============================================================================
//====================== Use in future if needed  ============================
//============================================================================
Boolean CubeRandomLinePop::ValidInput()
{
Boolean stat = True;

  return(stat);  
}

//============================================================================
//====================== Use in future if needed  ============================
//============================================================================
void CubeRandomLinePop::UndoInput()
{

}



//============================================================================
//====================== Handle the sort button setting     ==================
//============================================================================
void CubeRandomLinePop::sortActionToggle( void *data, long /*which*/ )
{
 CubeRandomLinePop *obj = (CubeRandomLinePop *)data;
 obj->_cube_random_line->enforceSort(obj->_enforce_sort);
 if(obj->_enforce_sort)
  {
  obj->_cube_random_line->sortSegments();
  obj->resetXYTable(obj->_cube_random_line);
  }
}


//============================================================================
//====================== Update this popup  =================================
//============================================================================
void CubeRandomLinePop::updateCubeInfo()
{
char filename_only[256];
char junk[256];
Cube *cube = getDisplayedCube();
Cube *temp;
int i = 1;

  if(cube)
    _cube_random_line = cube->randomLine();

  if( ! made() ) return;

  for(temp = top(); temp; temp = next(), i++)
     if(cube == temp) 
         _cube_number = i;

  if(strlen(primaryFilename()) > 1)
    {
    parse_file_(primaryFilename(), filename_only, junk);
    sprintf(_cube_id, "Cube %d = ",_cube_number);
    strcat(_cube_id,filename_only);
    wprocShowMsg( _cube_id_label, _cube_id );
    }

}


//============================================================================
//====================== Get the displayed cube   ============================
//============================================================================
Cube *CubeRandomLinePop::getDisplayedCube()
{
Cube *cube;

 for( cube = top(); cube; cube = next() )
   if( cube->isCurrentInWindow() && 
      (cube->currentLine()      != Cube::NoLinePlotted || 
       cube->currentCrossLine() != Cube::NoLinePlotted || 
       cube->currentTimeSlice() != Cube::NoLinePlotted) ) 
        return cube;

 return NULL;
}

//============================================================================
//====================== Get the timeslice seisplot ==========================
//============================================================================
SeisPlot *CubeRandomLinePop::getSeisPlot()
{
  return getDisplayedCube()->timesliceSP();
}

//============================================================================
//====================== Get current displayed cube filename =================
//============================================================================ 
char * CubeRandomLinePop::primaryFilename()
{
  return getDisplayedCube() ? getDisplayedCube()->primaryFilename() : (char*)"";
}


//============================================================================
//====================== Inform to add a new cube           ==================
//============================================================================
void CubeRandomLinePop::newCubeCreated(Cube *cube)
{
Cube *temp;
long i = 0;

  addCube(cube);

  //count the cubes, if first cube only has been created, dont do anything
  for(temp = top(); temp; temp = next(), i++){}
  if(i <= 1) return;

  //hide current random line's vectors if any
  _cube_random_line->hideVectors();

  //uninstall previous picker
  _cube_random_line->removePicker();

  //unmanage a random line plot if one exists
  if(_cube_random_line) _cube_random_line_plot->unmanage();

  //assign new random line
  _cube_random_line = cube->randomLine();

  //instantiate the new pick vectors (none will really show yet)
  _cube_random_line->showVectors(); 


  updateCubeInfo();

  unmanage();
}


//============================================================================
//====================== Inform to activate a cube============================
//============================================================================
void CubeRandomLinePop::cubeIsCurrent(Cube *cube)
{
  changeActiveRandomLine(cube->randomLine());
  updateCubeInfo();
}


//============================================================================
//====================== Inform of new line plotted    =======================
//============================================================================
void CubeRandomLinePop::newInLinePlot(Cube * /*cube*/, int /*slice*/)
{
  if(_cube_random_line_plot)
     _cube_random_line_plot->replotIfNeeded();
}

//============================================================================
//====================== Inform of new line plotted    =======================
//============================================================================
void CubeRandomLinePop::newCrossLinePlot(Cube * /*cube*/ , int /*slice*/)
{
  if(_cube_random_line_plot)
     _cube_random_line_plot->replotIfNeeded();
}


//============================================================================
//====================== Inform of new timeslice plotted    ==================
//============================================================================
void CubeRandomLinePop::newTimeSlicePlot(Cube * /*cube*/ , int /*slice*/)
{
  if(!_cube_random_line->vectLinkedList()->isAdded(getSeisPlot()))
      _cube_random_line->vectLinkedList()->addPlot(getSeisPlot());
  updateCubeInfo();
}

//============================================================================
//====================== Inform of new file selected        ==================
//============================================================================
void CubeRandomLinePop::newFilename(Cube * /*cube*/)
{
  //remove picks
  clearXYTable(_cube_random_line);

  //hide current random line's vectors if any
  _cube_random_line->hideVectors();

  //uninstall previous picker
  _cube_random_line->removePicker();

  //unmanage a random line plot if one exists
  if(_cube_random_line) _cube_random_line_plot->unmanage();

  updateCubeInfo();

  unmanage();

}


//============================================================================
//====================== Inform that a cube is deleted =======================
//============================================================================
void CubeRandomLinePop::destroyed(Cube * /*cube*/)
{
  //delCube(cube);
  if (!getDisplayedCube()) {
    _cube_random_line = NULL;
    _cube_random_line_plot = NULL;
  }
  updateCubeInfo ();
}

//============================================================================
//====================== Method to plot new data       =======================
//============================================================================
void CubeRandomLinePop::acceleratorRequest(long /*which*/)
{
//not used yet
}


//============================================================================
//====================== Method to zoom the data       =======================
//============================================================================
void CubeRandomLinePop::zoomUpSeparateWin()
{
  if(_cube_random_line_plot->_sp)
    if(_cube_random_line_plot->_sp->imageIsDisplayed())
      _cube_random_line_plot->_sp->zoomUpSeparateWin(); 
}


//============================================================================
//====================== Method to clear the xy table data ===================
//============================================================================
void CubeRandomLinePop::clearXYTable(CubeRandomLine *crl)
{
int i;
int num_segments = crl->pickerSegments();
float *xarray = _cube_random_line->xarray();
float *yarray = _cube_random_line->yarray();


  if(_xy_table == NULL) return;


  for(i = 0; i < num_segments; i++)
    {
    xarray[i] = 0.0;
    yarray[i] = 0.0;
    }

  for(i = 0; i < num_segments; i++)
    {
    _xy_table->valueTrap(_xy_table, CubeRandomLineXYTable::XARRAY,
                         i, 0.0, 1, "REMOVE");   
    }
  

}


//============================================================================
//====================== Method to populate the xy table =====================
//============================================================================
void CubeRandomLinePop::resetXYTable(CubeRandomLine *crl)
{
int i;
float *xarray = crl->xarray();
float *yarray = crl->yarray();

  if(_xy_table == NULL) return;

  _xy_table->changeActiveRandomLine(crl);

  _xy_table->drawVectors(False);

  for(i = 0; i < crl->pickerSegments(); i++)
    {
    //redraw vectors on last valueTrap update only for better speed
    if(i + 1 == crl->pickerSegments()) _xy_table->drawVectors(True);
    _xy_table->valueTrap(_xy_table, CubeRandomLineXYTable::XARRAY,
                         i, xarray[i], 1, "NOACTION");
    _xy_table->valueTrap(_xy_table, CubeRandomLineXYTable::YARRAY,
                         i, yarray[i], 1, "NOACTION");
    }
 
  _xy_table->drawVectors(True);//make sure this is set
}
