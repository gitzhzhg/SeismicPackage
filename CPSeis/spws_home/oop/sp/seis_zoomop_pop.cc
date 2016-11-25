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
#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include "sp/seis_zoomop_pop.hh"
#include "sl/psuedo_widget.hh"
#include "sp/seis_plot.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_scale.hh"
#include "sl/sl_radio_box.hh"

static String  defres[]= {
     "_popup.title:              Zoom Options",
     "*zoompt.labelString:       Zoom by Pointer Selection",
     "*zoomarea.labelString:     Zoom by Area",
     "*zfact.titleString:        Select Zoom Factor",
     "*area_size.titleString:    Select Zoom Area Size",
     "*ztypeframe.topPosition:   10",
     "*zfact.minimum:            20",
     "*zfact.maximum:            100",
     "*zfact.decimalPoints:      1",
     "*area_size.minimum:        50",
     "*area_size.maximum:        500",
     "*zoompt.set:               True",
     "*area_size.value:          300",
     "*zfact.value:              20",
     "*ztype_Frame.leftPosition: 20",
    NULL};



static SLRadio radios[]  = {
                     { "zoompt",   SeisPlot::Pointer, },
                     { "zoomarea", SeisPlot::Box, },
             };

   

#define ZTYPEFRAME "ztypeframe"

SeisZoomOpPop::SeisZoomOpPop( Widget   p,
                              char     *name,
                              HelpCtx  hctx,
                              SeisPlot *sp)

     : SLFPopSep(p,name,FP_DOALL,hctx,True,False)
{
  //PsuedoWidget *tpw;
  Display *dpy= XtDisplay(p);

  setDefaultResources( p, name, defres);

  _ztype= new SLRadioBox( this, "ztype", getHelpCtx(),
                          radios, XtNumber(radios), NULL, True );

  _zfact= new SLScale( this, "zfact", getHelpCtx(), NULL );
  _area_size= new SLScale( this, "area_size", getHelpCtx(), NULL );

 
  _current_type= _ztype->WhichSelected();
  long tmp= _zfact->getScaleValue();
  _zoom_factor= (float)tmp;
  _zoom_factor*= 0.1;
  _zoom_area= _area_size->getScaleValue();

  addControl(sp); 
}


void SeisZoomOpPop::addControl(SeisPlot *sp)
{
  if (sp) {
      _inform.addSeisPlot(sp);
      DoAction();
  }
}

void SeisZoomOpPop::removeControl(SeisPlot *sp)
{
  if (sp) {
      _inform.delSeisPlot(sp);
  }
}



Widget SeisZoomOpPop::make(Widget p)
{

   if ( made() ) return topWidget();
   p= p ? p : wParent();
   ShellStatMsg  bld_info(p,"Building Zoom Options Popup...");
   SLFPopSep::make(p);

   XtVaSetValues( _ztype->W(), XmNtopAttachment,   XmATTACH_FORM,
                               XmNtopOffset,       30,
                               XmNleftAttachment,  XmATTACH_POSITION, NULL );

   XtVaSetValues( _zfact->W(),  XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftOffset,     20,
                                XmNtopAttachment,  XmATTACH_WIDGET, 
                                XmNtopWidget,      _ztype->W(), 
                                XmNtopOffset,      30, 
                                NULL );

   XtVaSetValues( _area_size->W(),  
                                XmNtopAttachment,  XmATTACH_OPPOSITE_WIDGET, 
                                XmNleftAttachment, XmATTACH_WIDGET, 
                                XmNrightAttachment, XmATTACH_FORM, 
                                XmNrightOffset,    20, 
                                XmNtopWidget,      _zfact->W(), 
                                XmNleftWidget,     _zfact->W(), 
                                XmNleftOffset,     40, 
                                NULL );

   Widget tmp1=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _zfact->W(),
                               XmNbottomAttachment, XmATTACH_WIDGET,
                               XmNbottomWidget,     bottomSeparator(),
                               XmNtopOffset,        5,
                               XmNbottomOffset,     20,
                               NULL);

   _ztype->setComplexNotify(this);
   defaultButtonOK(True);
   return topWidget();
}




void SeisZoomOpPop::DoAction()
{
  long tmp; 
  SeisPlot *sp;
  void *vp;

  tmp= _zfact->getScaleValue();
  _zoom_factor= (float)tmp;
  _zoom_factor*= 0.1;
  _zoom_area= _area_size->getScaleValue();
  _current_type= _ztype->WhichSelected();


  for(sp= _inform.top(&vp); (sp); sp= _inform.next(&vp) ) {
        sp->setZoomFactor( _zoom_factor);
        sp->setZoomUpType( (int)_current_type);
        sp->setZoomBoxSize( (int)_zoom_area);
  }
}

void SeisZoomOpPop::UndoInput()
{

  int tmp=  (int)(_zoom_factor * 10);

  _zfact->setScaleValue(tmp);
  _area_size->setScaleValue( (int)_zoom_area);
}

void SeisZoomOpPop::manage()
{
 SLBase::manage();
 _ztype->SetRadio(_current_type);
 if (_current_type == SeisPlot::Box) 
         _area_size->manage();
 else
         _area_size->unmanage();
}


Boolean SeisZoomOpPop::notifyComplex(SLDelay*, int ident)
{
  switch (ident) {
      case SeisPlot::Box:     _area_size->manage(); break;
      case SeisPlot::Pointer: _area_size->unmanage(); break;
  }
  return True;
}

Boolean SeisZoomOpPop::zoomByArea()
{
 Boolean result;

 if ( _current_type == SeisPlot::Box )
      result= True;
 else
      result= False;

 return result;
}

void SeisZoomOpPop::reloadDefaults(Boolean)
{
  _zfact->reloadDefaults();
  _area_size->reloadDefaults();
  _ztype->reloadDefaults();
  DoAction();
}

void SeisZoomOpPop::reloadSystemDefaults(Boolean)
{
  _ztype->SetRadio(SeisPlot::Box);
  _area_size->setScaleValue(300);
  _zfact->setScaleValue(20);
  DoAction();
}
