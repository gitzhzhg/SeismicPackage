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
//***                      Cube Random Line image                      ***
//***                     Michael L. Sherrill 06/97                    ***
//************************************************************************


#include "cube/cube_random_line_pop.hh"
#include "cube/cube_random_line.hh"
#include "cube/cube_random_line_plot.hh"
#include "cube/cube_section_gui.hh"
#include "cube/cube_annotation_gui.hh"
#include "cube/cube_amplitude_gui.hh"
#include "cube/cube_master.hh"
#include "cube/cube_display.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_color_pop.hh"
#include "sl/sl_client_message.hh"
#include "sl/shell_stat_msg.hh"
#include "sp/seis_loc_out.hh"


#define ParentClass SLFormHelpPop


static String  defres[]= 
    {
    ".resizePolicy:              RESIZE_NONE",
    ".height:                    425",
    ".width:                     550",
    NULL
    };

//============================================================================
//========================== Constructor =====================================
//============================================================================
CubeRandomLinePlot::CubeRandomLinePlot(Widget               p,
                                       char                 *name,
                                       HelpCtx              hctx, 
                                       CubeRandomLinePop    *pop,
                                       int                  numcolors,
                                       CubeDisplay          *cube_display)
                   : SLFormHelpPop(p, name, FP_DOHELP | FP_DOREMOVE,
	                       hctx, True, 2, False, True, numcolors, True)
{

  setDefaultResources( p, name, defres);
  _cube_display = cube_display;
  _pop = pop;
  _hctx = hctx;
  _numcolors = numcolors;
  _changing               = False;
  _first_time             = True;
  _sp                     = NULL;
  _loc                    = NULL;
  _good_seis_plot_object  = False;
}

//============================================================================
//====================== Destructor               =========================
//============================================================================
CubeRandomLinePlot::~CubeRandomLinePlot()
{

  if(_sp)
    {
    if(_loc) _loc->removeControl(_sp);
    new SLClientMessage(topWidget(), "junk", 
                        postDestructorClientMessageFunc, (void *)this);
    }
  if(_loc) delete _loc;
  
}


//============================================================================
//====================== Make Popup                  =========================
//============================================================================
Widget CubeRandomLinePlot::make(Widget p)
{
CubeMaster *cm = CubeMaster::instance();

  if ( made() ) return topWidget();

  Widget parent = p ? p : wParent();
  ShellStatMsg bld_info(parent, "Building Random Line Plot...");

  SLFormHelpPop::make(p);

  _sp = new SeisPlot(topWidget(), "Random Line Plot");
  cm->colorPop(_cube_display)->addSP(_sp);

  //Transfer all color parameters from any SeisPlot since color params
  //are the same for all. This is necessary due to the fact that
  //this SeisPlot's color params will never be set until DoAction on
  //the SeisColorPop is called and it could have already been called
  //by the user before this SeisPlot ever came into existence.
  SeisPlot *isp = _cube_display->currentDisplayedCube()->inlineSP();
  _sp->setDoMedian(isp->domedian());
  _sp->setDoColor(isp->docolor());
  _sp->setDoPercent(isp->dopercent());
  _sp->setDoAmplitude(isp->doamplitude());
  _sp->setGradeVert(isp->gradeVert());
  _sp->setGradeHorz(isp->gradeHorz());
  _sp->setPNC(isp->pnc());
  _sp->setPPC(isp->ppc());
  _sp->setMinColorAmp(isp->minColorAmp());
  _sp->setMaxColorAmp(isp->maxColorAmp());

  _good_seis_plot_object = True;
  _loc= new SeisLocOut( topWidget(), "xyloc", _sp, getHelpCtx()); 

  return topWidget();
  
}


//============================================================================
//====================== Manage Popup                =========================
//============================================================================
void CubeRandomLinePlot::manage()
{


  //The following attachements were put into this area because we get
  //a circular dependency error if it is destroyed, never managed,
  //and the attachments have been registered.
  XtVaSetValues( topWidget(), XmNresizePolicy,     XmRESIZE_NONE, NULL);
  XtVaSetValues(topWidget(),  XmNwidth,700,        XmNheight,700,NULL);
  XtVaSetValues( _sp->W(),    XmNleftAttachment,   XmATTACH_FORM,
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNbottomAttachment, XmATTACH_WIDGET,
                              XmNbottomWidget,     bottomSeparator(), NULL);


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
  XtVaSetValues(_lowsep, XmNbottomOffset,  22, NULL);

  ParentClass::manage();

}


//============================================================================
//====================== Remove this class           =========================
//============================================================================
void CubeRandomLinePlot::removeButton()
{
CubeMaster *cm   = CubeMaster::instance();

  unmanage();
  _pop->removeButton();
  if(_sp)
    {
    if(_loc) _loc->removeControl(_sp);
    cm->colorPop(_cube_display)->destroyed(_sp); 
    new SLClientMessage(topWidget(), "junk", 
                        postDestructorClientMessageFunc, (void *)this);
    }
  _good_seis_plot_object = False;
  _pop->_cube_random_line->hideVectors();
}



//============================================================================
//====================== Main plotting function        =======================
//============================================================================

int CubeRandomLinePlot::plot()
{
int stat = False;
CubeSectionGui    *cp   = _pop->getPlotParameters();
CubeAnnotationGui *ca   = _pop->getAnnotationParameters();
Cube              *cube = _pop->getDisplayedCube();
CubeMaster        *cm   = CubeMaster::instance();


  if(!_good_seis_plot_object)
    {
    _sp = new SeisPlot(topWidget(), "Random Line Plot");
    cm->colorPop(_cube_display)->addSP(_sp);
    _good_seis_plot_object = True;
    _loc->addControl(_sp);
    }
  _sp->setFilename(_pop->primaryFilename());
  _sp->setTmin(cp->getTmin());
  _sp->setTmax(cp->getTmax());
  _sp->setIS(cp->getIS());
  _sp->setTI(cp->getRandomLinesPerInch());
  _sp->setCT(cp->getCT());
  _sp->setPlotType((int)cp->getPlotType());
  _sp->setRtoL(cp->getRandomLineRightToLeft());
  _sp->setInvert(cp->getRandomLineInvertVertical());
  _sp->setFirstLbl((long)ca->getStartingLine());
  _sp->setLblInc((long)ca->getCrossLineIncrement());
  _sp->setHeader1((int)ca->getPrimaryLineHeader());
  _sp->setHeader2((int)ca->getPrimaryCrossLineHeader());
  _sp->setPrimTimingLine(ca->getPrimaryHorizontalIncrement());
  _sp->setSecTimingLine(ca->getSecondayHorizontalIncrement());
  _sp->setNPlt(_pop->_cube_random_line->getNumberTraces());
  _sp->setSelectorParameters(1);
  _sp->setFilename(_pop->primaryFilename());
  _sp->setNorm(cube->getNormForSP(cube->getAmplitudeType()));
  //_sp->setNorm(cube->getAmplitudeType());
  if(cube->getAmplitudeType() == Cube::ExternalAmp)
    _sp->setExternalAmp(cube->getAmplitude());

  manage();
  XSync(XtDisplay(topWidget()),False);

  stat = _sp->plot();

  return(stat);
}


//============================================================================
//====================== Change the active plot        =======================
//============================================================================

void CubeRandomLinePlot::display()
{
  if(_sp == NULL || _good_seis_plot_object == False) return;
  if(_sp->imageIsDisplayed()) manage();
}


//============================================================================
//====================== Replot if needed              =======================
//============================================================================
int CubeRandomLinePlot::replotIfNeeded()
{
CubeSectionGui    *cp   = _pop->getPlotParameters();
CubeAnnotationGui *ca   = _pop->getAnnotationParameters();
Cube              *cube = _pop->getDisplayedCube();
CubeMaster        *cm   = CubeMaster::instance();


  //PlotImage's PlotSuccess is returned so that no error needs to be handled
  if(!_good_seis_plot_object) return (PlotImage::PlotSuccess);
  if(!_sp->imageIsDisplayed()) return (PlotImage::PlotSuccess);

  if(_sp->tmin() !=  cp->getTmin()) return plot();
  if(_sp->tmax() !=  cp->getTmax()) return plot();
  if(_sp->is() != cp->getIS()) return plot();
  if(_sp->ti() != cp->getRandomLinesPerInch()) return plot();
  if(_sp->ct() != cp->getCT()) return plot();
  if(_sp->plotType() != (int)cp->getPlotType()) return plot();
  if(_sp->rToL() != cp->getRandomLineRightToLeft()) return plot();
  if(_sp->invert() != cp->getRandomLineInvertVertical()) return plot();
  if(_sp->firstLbl() != (long)ca->getStartingLine()) return plot();
  if(_sp->lblInc() != (long)ca->getCrossLineIncrement()) return plot();
  if(_sp->header1() != (int)ca->getPrimaryLineHeader()) return plot();
  if(_sp->header2() != (int)ca->getPrimaryCrossLineHeader()) return plot();
  if(_sp->primTimingLine() != ca->getPrimaryHorizontalIncrement()) 
     return plot();
  if(_sp->secTimingLine() != ca->getSecondayHorizontalIncrement()) 
     return plot();
  if(_sp->norm()  != cube->getNormForSP(cube->getAmplitudeType()) ||
     (_sp->norm() == cube->getNormForSP(Cube::ExternalAmp)        &&
      _sp->externalAmp() != cube->getAmplitude()                    ))
     return plot ();

  return(PlotImage::PlotSuccess);
}


//This function used to prevent watches from being 
//removed on destroyed seisplots
void CubeRandomLinePlot::postDestructorClientMessageFunc(void *obj)
{
CubeRandomLinePlot *SIHT = (CubeRandomLinePlot *)obj;

  if(SIHT->_sp)
    {
    delete SIHT->_sp;
    SIHT->_sp = NULL;
    }
}

SeisPlot *CubeRandomLinePlot::SP ()
{
CubeMaster *cm = CubeMaster::instance();

  if(!made()) return NULL;

  if(!_good_seis_plot_object)
    {
    _sp = new SeisPlot(topWidget(), "Random Line Plot");
    cm->colorPop(_cube_display)->addSP(_sp);
    _good_seis_plot_object = True;
    _loc->addControl(_sp);
    }

  return _sp;
}
