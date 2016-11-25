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
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <stdio.h>
#include <stdlib.h>
#include "cprim.h"
#include "cprim.h"
#include "sp/slave_pop.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/psuedo_widget.hh"


static String  defres[]= {
     "_popup.title:          Remote Display",
     "*display.marginHeight:          1",
     "*display.columns:               35",
     "*display.marginWidth:           1",
     "*displayL.topPosition:  5",
     "*displayL.leftPosition: 4",
     "*displayL.labelString: Remote Display:",
     "*remove.labelString:    Remove Slave",
    NULL };

enum {REMOVE};

static SLPush pushes[]  = {
 { "remove",    REMOVE },
};



class SlavePush : public SLPushBox {
  private:
     SlavePop *_slave_pop;
  public:
     SlavePush( SLDelay            *contain,
                char               *name,
                HelpCtx            hctx,
                SLPushAry          pushary,
                const unsigned int arycnt,
                Boolean            doframe =False,
                Boolean            dotitle =False,
                Boolean            make_if_can=True ) :
          SLPushBox(contain,name,hctx,pushary,arycnt,
                    doframe,dotitle,make_if_can) {}
     virtual void  pushAction( long ident);
     void setSlavePop( SlavePop *slave_pop) { _slave_pop= slave_pop; }
};






SlavePop::SlavePop(  Widget             p, 
                     char               *name, 
                     HelpCtx            hctx,
                     SlaveDisplayLinkedList *slaves,
                     Boolean            make_now) 

          : SLFPopSep(p,name,FP_DOALL,hctx,False,make_now), _dname(NULL),
            _slaves(slaves)
{
 setDefaultResources( p, name, defres);
 if (make_now) make(p);
 else {
     supportUnmadeDefaults(p);
     _dname= newstr(_pw_topw->childTextDef("display"));
     _buttons= new SlavePush(this, "slave_push", getHelpCtx(),
                             pushes, XtNumber(pushes));
     _buttons->setSlavePop(this);
 }
}


SlavePop::~SlavePop()
{
 if (_dname) XtFree(_dname);
}


Widget SlavePop::make(Widget p)
{
   Widget lab;
   if ( made() ) return topWidget();
   p= p ? p : wParent();
   ShellStatMsg  bld_info(p,"Building Slave Popup...");
   SLFPopSep::make(p);

   lab= XtVaCreateManagedWidget("displayL", xmLabelWidgetClass, topWidget(),
                                XmNtopAttachment,   XmATTACH_FORM,
                                XmNtopOffset,       10,
                                XmNleftOffset,      20,
                                XmNleftAttachment,  XmATTACH_FORM, NULL);

   _display_w= XtVaCreateManagedWidget("display",xmTextWidgetClass, topWidget(),
                                XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                XmNleftAttachment,  XmATTACH_WIDGET,
                                XmNtopWidget,       lab,
                                XmNleftWidget,      lab, NULL);

   _slave_stat= XtVaCreateManagedWidget("slave_stat", 
                                xmLabelWidgetClass, topWidget(),
                                XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopWidget,       lab,
                                XmNleftWidget,       lab,
                                XmNtopOffset,       15,
                                XmNleftAttachment,  XmATTACH_WIDGET,
                                NULL);

   XtVaSetValues( _buttons->W(), XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNleftAttachment,XmATTACH_OPPOSITE_WIDGET,
                                 XmNtopWidget,     _slave_stat,
                                 XmNleftWidget,    _slave_stat,
                                 XmNtopOffset,       10, NULL);

   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                         XmNtopAttachment, XmATTACH_WIDGET,
                                         XmNtopWidget,     _buttons->W(),
                                         XmNbottomAttachment, XmATTACH_WIDGET,
                                         XmNbottomWidget,    bottomSeparator(),
                                         XmNleftAttachment,  XmATTACH_FORM,
                                         XmNleftOffset,       5,
                                         XmNtopOffset,        5,
                                         NULL);
   Widget tmp1=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                         XmNleftAttachment, XmATTACH_WIDGET,
                                         XmNleftWidget,     _display_w,
                                         XmNleftOffset,       5,
                                         XmNrightAttachment, XmATTACH_FORM,
                                         XmNrightOffset,       5,
                                         XmNtopAttachment,   XmATTACH_FORM,
                                         XmNtopOffset,        5,
                                         NULL);


   setStatus();
   _slaves->setErrorNotify(this);

   return (topWidget());
}


void SlavePop::setStatus()
{
 if (_slaves->find( _dname)) {
       wprocShowMsg( _slave_stat, "Slave Status:   Showing");
       XtSetSensitive( _buttons->W(), True);
 }
 else {
       wprocShowMsg( _slave_stat, "Slave Status:   Not Showing");
       XtSetSensitive( _buttons->W(), False);
 }
}




void SlavePop::manage()
{
 SLBase::manage();
 setStatus();
}

Boolean SlavePop::ValidInput()
{
 Boolean stat=True;
 
 if (_dname) XtFree(_dname);
 _dname= XmTextGetString(_display_w);
 return (stat);
}

void SlavePop::DoAction()
{
 if (!_slaves->find( _dname)) _slaves->add( _dname);
 setStatus();
}

void  SlavePush::pushAction( long )
{
 _slave_pop->_slaves->remove(_slave_pop->_dname); 
 _slave_pop->setStatus();
}

#define DISPLAY_ERR "Unable to open a window on %s.\n\n \
\n\
Check spelling of the name and the X security\n\
of the machine you would like to open a window on."


void SlavePop::errorNotify(int stat)
{
 puts("errorHandler");
 setStatus();
 if (stat == _ERROR_BAD_NODE_NAME) {
       manage();
       ErrorHandler err= W();
       char tmpstr[500];
       sprintf(tmpstr, DISPLAY_ERR, _dname);
       err.setEtype(ErrorHandler::Warning, ErrorHandler::CUI, False);
       err.deliverError(tmpstr, ErrorHandler::Warning);
 }
}
