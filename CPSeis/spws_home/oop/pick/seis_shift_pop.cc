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
// Author Michael L. Sherrill 08/94
// Install shift menu to be used with SeisPlot


#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include "pick/seis_shift_pop.hh"
#include "pick/seis_shift.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_text_box.hh"
#include <math.h>


#define    CHANGE_VELOCITY  5000.0
#define    CHANGE_HEADER    6
#define    CHANGE_TIME      0.2

static String defres[]= {
    "*popup.title:                   Linear Moveout",
    ".height:                        500",
    ".width:                         400",
    "*shiftvelocitylab.labelString:  Shift Type",
    "*shiftvelocitylab.fontList:     -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*shiftlab.labelString:          Shift Direction",
    "*shiftlab.fontList:             -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*flatlab.labelString:           Flatten Time",
    "*flatlab.fontList:              -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*vellab.labelString:            Velocity",
    "*hdrlab.labelString:            Header #",
    "*timelab.labelString:           Seconds",
    "*velocitybox.leftOffset:        275",
    "*velocitybox.topOffset:         60",
    "*timebox.leftOffset:            190",
    "*timebox.topOffset:             340",
    "*sep_1.topPosition:             45",
    "*sep_1.leftOffset:              10",
    "*sep_1.rightOffset:             10",
    "*sep_1.bottomOffset:            10",
    "*sep_2.topPosition:             35",
    "*sep_2.leftOffset:              10",
    "*sep_2.rightOffset:             10",
    "*sep_2.bottomOffset:            10",
    "*shift_by_velocity.labelString: Velocity Type ->",
    "*shift_by_header.labelString:   Header # ----->",
    "*forward.labelString:           Forward",
    "*reverse.labelString:           Reverse",
    "*stypebox_Frame.topPosition:    12",
    "*stypebox_Frame.leftPosition:   7",
    "*mtypebox_Frame.topPosition:    42",
    "*mtypebox_Frame.leftPosition:   35",
    "*original_data.labelString:     Reread Data From Disk",
    NULL};

enum {VELOCITY,HEADER};
enum {MFORWARD,MREVERSE};
enum {SHIFT_BY_VELOCITY,SHIFT_BY_HEADER};
enum {ORIGINAL_DATA};
enum {FLATTIME};
enum {SHIFT_ON_SCAN};

#define ParentClass SLFPopSep


SeisShiftPop::SeisShiftPop( Widget            p,
                            char              *name,
                            SeisPlot          *sp,
                            SeisShift         *ss,
                            HelpCtx           hctx) 

      : SLFPopSep(p,name,FP_DOALL,hctx,True,False),
                            _plot_on_doaciton(False), _new_file(True),
                            _use_file_defaults(False), _new_appdefaults(True),
                            _sp(sp),_ss(ss),
                            _new_velocity(ss->getVelocity()),
                            _new_header(ss->getHeader()),
                            _new_time(ss->getFlattenTime()),
                            _first_time(True)
{
       
  static SLText textb1[]  = {
    {"change_velocity", NULL, NULL, SLType_float, VELOCITY},
    {"change_header",   NULL, NULL,   SLType_int,   HEADER  },
  };
  textb1[0].target= &_new_velocity;
  textb1[1].target= &_new_header;

  static SLText textb2[]  = {
    {"change_time", NULL, NULL, SLType_float, FLATTIME},
  };
  textb2[0].target= &_new_time;

  static SLRadio stypes[]  = {
    { "shift_by_velocity",  SHIFT_BY_VELOCITY },
    { "shift_by_header",    SHIFT_BY_HEADER },
  };

  static SLRadio mtypes[]  = {
      { "forward",  MFORWARD },
      { "reverse",  MREVERSE },
    };

  static SLPush pushb[]  = {
    { "original_data",   ORIGINAL_DATA },
  };


  static SLTog togb[]  = { 
     {"Shift On Scan",  NULL, SHIFT_ON_SCAN  },};
     togb[0].target= &_shift_on_scan;

   setDefaultResources( p, name, defres);

   
   _velocitybox= new SLTextBox( this, "velocitybox", getHelpCtx(),
                            textb1, XtNumber(textb1), False, 1, False, False );

   _velocitybox->setAltLosingAction( (SLTextfunc)velhdrchange, this);

   _timebox= new SLTextBox( this, "timebox", getHelpCtx(),
                            textb2, XtNumber(textb2), False, 1, False, False );

   _timebox->setAltLosingAction( (SLTextfunc)timechange, this);

   _stypebox= new SLRadioBox(this, "stypebox",getHelpCtx(),stypes,
                             XtNumber(stypes), NULL, True, False );

   _stypebox->setAltChoiceAction( (SLRadioButtonfunc)settype, this);

   _mtypebox= new SLRadioBox(this, "mtypebox",getHelpCtx(),mtypes,
                             XtNumber(mtypes), NULL, True, False );

   _original_data= new SLPushBox(this,"original_data",NULL, pushb,
                                XtNumber(pushb), False,False,False );

   _original_data->setAltPushAction( (SLPushButtonfunc)original, this);

   _shift_scan_tog = new SLTogBox(this, "Shift On Scan",getHelpCtx(),
                            togb, XtNumber(togb), False, False, False );
   _shift_scan_tog->setAltChoiceAction((SLToggleButtonfunc)scanToggleAction,
                            this);

   DoAction();     
}


SeisShiftPop::~SeisShiftPop()
{
  _sp->setExternalFunction( NULL, NULL );
}


Widget SeisShiftPop::make(Widget p)
{

  if ( made() ) return topWidget();
  SLFPopSep::make(p);

  XtVaSetValues( _velocitybox->W(), 
                           XmNleftAttachment, XmATTACH_POSITION,
                           XmNtopAttachment,  XmATTACH_POSITION, NULL);

  XtVaSetValues( _timebox->W(),
                           XmNleftAttachment, XmATTACH_POSITION,
                           XmNtopAttachment,  XmATTACH_POSITION, NULL);

  Widget vellab = XtVaCreateManagedWidget("vellab",xmLabelWidgetClass,
                           topWidget(), 
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       204,
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        64, NULL);

  Widget hdrlab =  XtVaCreateManagedWidget("hdrlab",xmLabelWidgetClass,
                           topWidget(),
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       204,
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        94, NULL);

  Widget timelab=  XtVaCreateManagedWidget("timelab",xmLabelWidgetClass,
                           topWidget(),
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       115,
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        345, NULL);


  Widget shiftvelocitylab= XtVaCreateManagedWidget(
                          "shiftvelocitylab",   xmLabelWidgetClass, topWidget(),
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       120, 
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        10,NULL );

  Widget shiftlab= XtVaCreateManagedWidget(
                           "shiftlab",      xmLabelWidgetClass, topWidget(),
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        160,
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       105, NULL );

  Widget flatlab=  XtVaCreateManagedWidget(
                           "flatlab",      xmLabelWidgetClass, topWidget(),
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNtopOffset,        310,
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       115, NULL );

  Widget sep_1 = XtVaCreateManagedWidget("sep_1", xmSeparatorWidgetClass,
                           topWidget(),
                           XmNbottomAttachment, XmATTACH_WIDGET,
                           XmNbottomWidget,     shiftlab,
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNrightAttachment,  XmATTACH_FORM, NULL);

  Widget sep_2 = XtVaCreateManagedWidget("sep_2", xmSeparatorWidgetClass,
                           topWidget(),
                           XmNbottomAttachment, XmATTACH_WIDGET,
                           XmNbottomWidget,     flatlab,
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNrightAttachment,  XmATTACH_FORM, NULL);


  XtVaSetValues( _stypebox->W(), 
                           XmNleftAttachment, XmATTACH_POSITION,
                           XmNtopAttachment,  XmATTACH_POSITION, NULL);

  XtVaSetValues( _mtypebox->W(), 
                           XmNleftAttachment, XmATTACH_POSITION,
                           XmNtopAttachment,  XmATTACH_POSITION, NULL);
  
  XtVaSetValues( _original_data->W(), XmNtopAttachment, XmATTACH_FORM,
                           XmNtopOffset,        395,
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNleftOffset,       25, NULL);

  XtVaSetValues( _shift_scan_tog->W(), 
                           XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                           XmNtopWidget,        _original_data->W(),
                           XmNleftAttachment,   XmATTACH_WIDGET,
                           XmNleftWidget,       _original_data->W(),
                           XmNleftOffset,       10, NULL);

  _stypebox->SetRadio(SHIFT_BY_VELOCITY);

  _mtypebox->SetRadio(MREVERSE);

  return topWidget();

}


Boolean SeisShiftPop::ValidInput()
{
 Boolean stat;

  if (made())
     stat= ParentClass::ValidInput();
  else
     stat= True;

  return (stat); 
}



void SeisShiftPop::UndoInput()
{
  _sp->setExternalFunction( NULL, NULL );
  SLFormPop::UndoInput();
}

void SeisShiftPop::scanToggleAction( void *data, long /*which*/ )
{
SeisShiftPop *obj = (SeisShiftPop *)data;

  if(obj->_shift_on_scan)
    {
    obj->_sp->setExternalFunction( obj->scanShift, obj );
    }
  else
    {
    obj->_sp->setExternalFunction( NULL, NULL );
    }
}


void SeisShiftPop::scanShift(void *obj)
{
SeisShiftPop *ssp = (SeisShiftPop *)obj;
int replot;

 switch( ssp->_mtypebox->WhichSelected() )
   {
   case MFORWARD:
     if(ssp->_stypebox->WhichSelected() == SHIFT_BY_VELOCITY)
       replot = ssp->_ss->linearShift(True,False);
     else
       replot = ssp->_ss->nonlinearShift(True,NULL,False);
     break;

   case MREVERSE:
     if(ssp->_stypebox->WhichSelected() == SHIFT_BY_VELOCITY)
       replot = ssp->_ss->linearShift(False,False);
     else
       replot = ssp->_ss->nonlinearShift(False,NULL,False);
     break;
   }
}

void SeisShiftPop::DoAction()
{
 ParentClass::DoAction();
 Boolean replot = False;

  if(!_sp->imageIsDisplayed())return; 


  if(_ss->getVelocity() != _new_velocity) 
     {
     _ss->changeVelocity(_new_velocity);
     replot = True;
     }

  if(_ss->getHeader() != _new_header)
     {
     _ss->changeHeader(_new_header);
     replot = True;
     }

  if(_ss->getFlattenTime() != _new_time)
    {
     _ss->setFlattenTime(_new_time);
     replot = True;
   }

  if(!_first_time)
     {
     switch (whichButton())
        {
        case FP_OK:
        case FP_APPLY:
          if(_shift_on_scan)
            {
            _sp->setExternalFunction( scanShift, this );
            }
          else
            {
            _sp->setExternalFunction( NULL, NULL );
            }

          if(_sp->imageIsDisplayed())
            {
            replot = True;
            }
        break;
        }
     }

  if(replot) 
     {
     switch( _mtypebox->WhichSelected() )
        {
        case MFORWARD:
                      if(_stypebox->WhichSelected() == SHIFT_BY_VELOCITY)
                         replot = _ss->linearShift(True);
                      else
                         replot = _ss->nonlinearShift(True,NULL);
                      break;
        case MREVERSE:
                      if(_stypebox->WhichSelected() == SHIFT_BY_VELOCITY)
                         replot = _ss->linearShift(False);
                      else
                         replot = _ss->nonlinearShift(False,NULL);
                      break;
        }

     //if(replot) _sp->plot();
     }

}




void SeisShiftPop::manage()
{

  if(_first_time) 
     {
     _velocitybox->SetValue(VELOCITY, (float)_ss->getVelocity());
     _velocitybox->SetValue(HEADER,   (long)_ss->getHeader());
     _timebox->SetValue(FLATTIME, (float)_ss->getFlattenTime());
     _old_time = CHANGE_TIME;
     }

  XtManageChild(topWidget()); 

  _first_time = False;


}

void SeisShiftPop::reloadDefaults(Boolean)
{
  SLFPopSep::reloadDefaults();
  _velocitybox->reloadDefaults();
  _timebox->reloadDefaults();
  _stypebox->reloadDefaults();
  _mtypebox->reloadDefaults();
  DoAction();
}


void SeisShiftPop::reloadSystemDefaults(Boolean do_method)
{
  SLFPopSep::reloadSystemDefaults(do_method);
  _velocitybox->SetValue(VELOCITY, (float)CHANGE_VELOCITY);
  _velocitybox->SetValue(HEADER,   (long)CHANGE_HEADER);
  _timebox->SetValue(FLATTIME,   (float)CHANGE_TIME);
  _stypebox->SetRadio(SHIFT_BY_VELOCITY);
  _mtypebox->SetRadio(MFORWARD);
  DoAction();
}


void SeisShiftPop::settype( void *data, long button)
{
  SeisShiftPop *obj = (SeisShiftPop *)data;
  if(button == SHIFT_BY_VELOCITY)
    obj->_velocitybox->SetValue(HEADER,   (long)CHANGE_HEADER);
}


void SeisShiftPop::original( void *data, long )
{
 SeisShiftPop *obj = (SeisShiftPop *)data;

  if(!obj->_sp->imageIsDisplayed())return;

  if(obj->_sp->isZoomed())
    obj->_ss->removeShift(True);
  else
    obj->_sp->plotFromFile();
}

// use later to apply shift on a scan or something ?
void SeisShiftPop::newData()
{
  if(!_sp->imageIsDisplayed())return;
  if(_first_time) return;
}


void SeisShiftPop::velhdrchange( void *data, long)
{
  SeisShiftPop *obj = (SeisShiftPop *)data;

  if(obj->_new_velocity == 0.0) 
      obj->_velocitybox->SetValue(VELOCITY, (float)1.0);

  if(obj->_new_header < 1)
      obj->_velocitybox->SetValue(HEADER,(long)1);

}

void SeisShiftPop::timechange( void *data, long )
{
  SeisShiftPop *obj = (SeisShiftPop *)data;

  if(obj->_ss->dataShifted())
     {
     obj->_timebox->SetValue(FLATTIME, obj->_old_time);
     return;
     }

  if( obj->_new_time < obj->_sp->memTmin() )
     obj->_timebox->SetValue(FLATTIME, obj->_sp->memTmin());

  if( obj->_new_time > obj->_sp->memTmax() )
     obj->_timebox->SetValue(FLATTIME, obj->_sp->memTmax());


  obj->_old_time = obj->_new_time;
}
