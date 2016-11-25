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


//===========================================================================
//========== Creates an independent popup that displays the       ===========
//========== color bar used for contours. This is similiar to     ===========
//========== the SeisCbarPop class but currently has less         ===========
//========== functionality.                                       ===========
//========== Michael L. Sherrill 11/98                            ===========
//===========================================================================


#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include "sl/sl_error_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "sp/seis_contour_cbar_pop.hh"
#include "sp/seis_ctype.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_scale.hh"

static String  defres[]= {
    "_popup.title:                   Contour Color Bar",
   "*intensitylab.labelString:      Intensity",
    "*compressionlab.labelString:    Compression", 
    ".resizePolicy:                  RESIZE_NONE",
    NULL};



#define ParentClass SLFPopSep
#define CBAR_WARNING "Warning... independent color bars for each movie\n\
panel are not yet supported, the color bar\nis only accurate on the last \
panel\nwhen using the color percentages option"

SeisContourCbarPop::SeisContourCbarPop( Widget            p,
                                        char              *name,
                                        SeisPlot          *sp,
                                        HelpCtx           hctx) 
       : SLFPopSep(p,name,FP_DOREMOVE,hctx,False,False),
                            _sp(sp), _first_time(True), _color_num(1)

{

   setDefaultResources( p, name, defres);
   _ci = new CbarInform(_sp);

   _cbar = new SeisCbar(this, "cbar", _sp, False, True, True, SeisPlot::CONT);

   _intensityscale =
                new IntensityScale(this, "intensityscale", getHelpCtx(), 
                                   NULL, _cbar);

   _compressionscale =
                new CompressionScale(this,"compressionscale",getHelpCtx(),
                                     NULL, _cbar );
   _ci->setCbar(_cbar);
}


SeisContourCbarPop::~SeisContourCbarPop()
{
   delete _ci;
   delete _cbar;
   delete _intensityscale;
   delete _compressionscale;
}


Widget SeisContourCbarPop::make(Widget p)
{

   if ( made() ) return topWidget();

   SLFPopSep::make(p);

   return topWidget();

}


void SeisContourCbarPop::DoAction()
{
   ParentClass::DoAction();
}



void SeisContourCbarPop::manage()
{
  if(_first_time)
    {
    _cbar->make(topWidget());

   _intensityscale->make(topWidget());
   _compressionscale->make(topWidget());


   Widget compressionlab= XtVaCreateManagedWidget( "compressionlab",
                              xmLabelWidgetClass,   topWidget(),
                              XmNleftAttachment,    XmATTACH_FORM,
                              XmNleftOffset,        2,
                              XmNrightAttachment,   XmATTACH_FORM,
                              XmNrightOffset,       2,
                              XmNbottomAttachment,  XmATTACH_WIDGET,
                              XmNbottomWidget,      bottomSeparator() ,NULL );

   XtVaSetValues( _compressionscale->W(),
                              XmNbottomAttachment,  XmATTACH_WIDGET,
                              XmNbottomWidget,      compressionlab,
                              XmNleftAttachment,    XmATTACH_OPPOSITE_WIDGET,
                              XmNleftWidget,        compressionlab,
                              XmNrightAttachment,   XmATTACH_FORM,
                              XmNrightOffset,       2,
                              XmNminimum,           -100,
                              XmNmaximum,           100,
                              XmNvalue,             0,
                              XmNdecimalPoints,     0,NULL );


   Widget intensitylab= XtVaCreateManagedWidget( "intensitylab",
                              xmLabelWidgetClass,   topWidget(),
                              XmNleftAttachment,    XmATTACH_OPPOSITE_WIDGET,
                              XmNleftWidget,        _compressionscale->W(),
                              XmNrightAttachment,   XmATTACH_FORM,
                              XmNrightOffset,       2,
                              XmNbottomAttachment,  XmATTACH_WIDGET,
                              XmNbottomWidget,      _compressionscale->W(), 
                              NULL );

   XtVaSetValues( _intensityscale->W(),  
                              XmNbottomAttachment,  XmATTACH_WIDGET,
                              XmNbottomWidget,      intensitylab, 
                              XmNleftAttachment,    XmATTACH_OPPOSITE_WIDGET,
                              XmNleftWidget,        intensitylab, 
                              XmNrightAttachment,   XmATTACH_FORM,
                              XmNrightOffset,       2,
                              XmNminimum,           1,
                              XmNmaximum,           20,
                              XmNvalue,             10,
                              XmNdecimalPoints,     1, NULL );


    XtVaSetValues(_cbar->W(), XmNtopAttachment,     XmATTACH_FORM,
                              XmNleftAttachment,    XmATTACH_FORM,
                              XmNrightAttachment,   XmATTACH_FORM,
                              XmNleftOffset,        2,
                              XmNrightOffset,       2,
                              XmNtopOffset,         2,
                              XmNbottomAttachment,  XmATTACH_WIDGET,
                              XmNbottomWidget,      _intensityscale->W(),
                              XmNheight,            410,
                              XmNwidth,             200, NULL);


  
   _first_time = False; 
  } 

  if(_sp->dopercent() && _sp->movie())
    {
    SLErrorPop *errpop = new SLErrorPop(topWidget(),"Warning",CBAR_WARNING);
    }

  _cbar->updateAmps();
  XtManageChild(topWidget()); 
}


void SeisContourCbarPop::addSP(SeisPlot *sp)
{
  if(sp) _ci->addSeisPlot(sp);
}

