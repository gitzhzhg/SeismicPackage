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
// Plot Information Pop Up
// Michael L. Sherrill (class conversion of Trey Roby's cbyt routine)
// 03/15/94


#include <stdio.h>
#include <stdlib.h>
#include "sp/seis_info_pop.hh"
#include "sp/seis_plot.hh"
#include "sl/shell_stat_msg.hh"
#include <Xm/Label.h>



static String  defres[]= {
    "*popup.title:                   Plot Information",
    ".height:                        650",
    ".width:                         500",
    "*infolab.fontList:              -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*infolab.labelString:           Plot Information",
    NULL};


SeisInfoPop::SeisInfoPop( Widget            p,
                          char              *name,
                          SeisPlot          *sp,
                          HelpCtx           hctx)
            : SLFPopSep(p,name,FP_DOOK,hctx,True,False),
                            _sp(sp), _first_time(True), _str(NULL)
 
{
  setDefaultResources( p, name, defres);
  _sipi = new SeisInfoPopInform(sp,this);
}


SeisInfoPop::~SeisInfoPop()
{
  if (_str) free(_str);
  delete _sipi;
}

SeisInfoPopInform::~SeisInfoPopInform()
{
}

Widget SeisInfoPop::make(Widget p)
{
  if ( made() ) return topWidget();
  p= p ? p : wParent();
  ShellStatMsg  bld_info(p,"Building Plot Information Popup...");
  SLFPopSep::make(p);

  Widget infolab = XtVaCreateManagedWidget("infolab",xmLabelWidgetClass, 
                                           topWidget(),
                              XmNleftAttachment,   XmATTACH_POSITION,
                              XmNleftPosition,     28,
                              XmNtopAttachment,    XmATTACH_POSITION,
                              XmNtopPosition,      3,  NULL);

  _pinfo = XtVaCreateManagedWidget("pinfo",xmLabelWidgetClass, topWidget(),
                              XmNleftAttachment,   XmATTACH_POSITION,
                              XmNleftPosition,     5,
                              XmNtopAttachment,    XmATTACH_WIDGET,
                              XmNtopWidget,        infolab,  
                              XmNtopOffset,        5,
                              XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                              XmNbottomWidget,     _lowsep,
                              XmNalignment,        XmALIGNMENT_BEGINNING,
                              XmNtraversalOn,      False,NULL);

  return topWidget();
}

void SeisInfoPop::manage()
{
  if(!_sp->imageIsDisplayed())return;

  if(_first_time)
      {
      _str = (char *)calloc(1, 1000 * sizeof(char) );
      if(_str == NULL) 
         {
         printf("not enough memory for plot information widget\n");
         return;
         }
      _first_time = False;
      }

  getPlotInfo(False);
  XtManageChild(topWidget());
}






void SeisInfoPop::getPlotInfo(Boolean inform_is_calling)
{


char tmpstr[100];
char pt[20];
float ti,is; 
Boolean morestuff;


   if( _first_time ) return;

   if( inform_is_calling)
      if( !XtIsManaged(topWidget()) ) return;
  
   if( !_sp->imageIsDisplayed() ) return;

   if (!_sp->isZoomed() ) 
      {
      ti = _sp->ti();
      is = _sp->is();
      morestuff= False;
      }
   else 
      {
      ti = _sp->originalTI();
      is = _sp->originalIS();
      morestuff= True;
      }

   sprintf( _str, "Filename:    %s\n", _sp->filename());
   sprintf( tmpstr, "Number to plot: %6d\n", _sp->nplt());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Initial Skip: %6d\n", _sp->iskp());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Number of traces to do: %6d\n", _sp->ndo());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Number of traces to skip: %6d\n", _sp->nskp());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Data minimum: %6.2f\n", _sp->tmin());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Data maximum: %6.2f\n", _sp->tmax());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Sample rate: %3.3f\n", _sp->srval());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Data Decimation: %6d\n", _sp->tdec());
   strcat(_str, tmpstr);
   if (_sp->units() == PlotMetric)
       sprintf( tmpstr, "Input Traces/Cm : %6.2f\n", ti);
   else
       sprintf( tmpstr, "Input Traces/Inch: %6.2f\n", ti);
   strcat(_str, tmpstr);
   if (_sp->units() == PlotMetric)
       sprintf( tmpstr, "Input Cm/Sec: %6.2f\n", is);
   else
       sprintf( tmpstr, "Input Inch/Sec: %6.2f\n", is);
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Channels per trace: %6.2f\n", _sp->ct());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Reverse Polarity: %s\n", _sp->rp() ? "Yes" : "No" );
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Normalize: %s\n",  _sp->norm() ? "Yes" : "No" );
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Reverse Plot Direction: %s\n", _sp->rToL() ? "Yes" : "No");
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Primary Annotation Header Word: %2d\n", _sp->header1());
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Secondary Annotation Header Word: %2d\n", _sp->header2());
   strcat(_str, tmpstr);

   switch (_sp->plotType()) 
      {
      case PlotImage::PlotGS    : strcpy(pt, "Variable Density" ); break;
      case PlotImage::PlotWFILL : strcpy(pt, "Variable Area" ); break;
      case PlotImage::PlotWONLY : strcpy(pt, "Wiggles Only" ); break;
      }

   sprintf( tmpstr, "Plot Type: %s\n", pt );
   strcat(_str, tmpstr);

   if (!morestuff) 
      {
      strcat(_str, "\nZoom Status: Plot is at Original Size\n" );
      }
   else 
      {
      strcat(_str, "\nZoom Status: Plot Zoomed\n" );
      sprintf( tmpstr, "Zoom Factor: %6.1f\n", _sp->zoomFactor());
      strcat(_str, tmpstr);
      if (_sp->units() == PlotMetric)
         sprintf( tmpstr, "Current Traces/Cm : %6.2f\n", _sp->ti());
      else
         sprintf( tmpstr, "Current Traces/Inch: %6.2f\n", _sp->ti());
      strcat(_str, tmpstr);
      if (_sp->units() == PlotMetric)
         sprintf( tmpstr, "Current Cm/Sec : %6.2f\n", _sp->is());
      else
         sprintf( tmpstr, "Current Inch/Sec: %6.2f\n", _sp->is());
      strcat(_str, tmpstr);
      }

   sprintf( tmpstr, "\n\nTo change a plotting parameter select\n");
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Setup/Change Plot under the File menu.\n");
   strcat(_str, tmpstr);
   sprintf( tmpstr, "\nTo change a Zoom setting select\n");
   strcat(_str, tmpstr);
   sprintf( tmpstr, "Zoom-Scan Options under the Zoom-Scan menu.\n");
   strcat(_str, tmpstr);

   show_msg( _pinfo, _str);

}

void SeisInfoPopInform::newPlot(SeisPlot *)
{
  _sip->getPlotInfo(True);
}

void SeisInfoPopInform::postScan(SeisPlot *, SeisPlot::ScanDir)
{
  _sip->getPlotInfo(True);
}

void SeisInfoPopInform::postZoom(SeisPlot*, SeisPlot::ZoomDir)
{
  _sip->getPlotInfo(True);
}

void SeisInfoPopInform::unitChange(SeisPlot*, int)
{
  _sip->getPlotInfo(True);
}

void SeisInfoPopInform::notCurrentInWindow(SeisPlot *sp)
{
  _sip->getPlotInfo(True);
  addSeisPlot(sp->currentSPInWindow());
  _sip->_sp= sp->currentSPInWindow();
}
