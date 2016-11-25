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
#ifndef SLOPTIONMENU_H
#define SLOPTIONMENU_H

#include "sl/sl_push_box.hh"


class SLOptionMenu;

class OptionPushBox : public SLPushBox {
 private:
     SLOptionMenu *_opm;
 public:
     OptionPushBox( Widget             p,
                    char               *name,
                    HelpCtx            hctx,
                    SLPushAry          pushary,
                    const unsigned int arycnt,
                    SLOptionMenu       *opm,
                    Boolean            make_now=True ) :
             SLPushBox(p,name,hctx,pushary, arycnt,False,False,make_now),
             _opm(opm) { setPushType(SLPushBox::PULLDOWN); }


     OptionPushBox( const PsuedoWidget *pw,
                    char               *name,
                    HelpCtx            hctx,
                    SLPushAry          pushary,
                    const unsigned int arycnt,
                    SLOptionMenu       *opm) :
             SLPushBox(pw,name,hctx,pushary,arycnt,False,False),
             _opm(opm) { setPushType(SLPushBox::PULLDOWN); }

    virtual void  pushAction( long ident);
};






class SLOptionMenu : public SLDelay {

  private:
    friend class OptionPushBox;
    SLPushButtonfunc _altPushAction;
    void             *_altPushData;
    static void  doMapCallback(Widget, XtPointer, XEvent*);


  protected:
    SLPushBox    *_buttons;
    long          _curr_button;
    SLPushAryW   _pushary;
    unsigned int _arycnt;
    long         *_target;
    char         *_def_value;
    char         *_label_value;

    void init( const Display *dpy);
    Widget createOptionMenu(Widget    p, 
                            char     *name, 
                            Widget    poprc, 
                            XmString  label);
    void getDefValueAndIdent(SLPushAry pushary);
    void getDefValueAndIdent(SLPushAryW pushary);
    SLPushAry makeDefaultsMenu();

      
  public:
    SLOptionMenu( Widget             p,
                  char               *name,
                  HelpCtx            hctx,
                  SLPushAry          pushary,
                  const unsigned int arycnt,
                  long               *target =NULL,
                  Boolean            doframe =False,
                  Boolean            make_now=True );

    SLOptionMenu( SLDelay            *contain,
                  char               *name,
                  HelpCtx            hctx,
                  SLPushAry          pushary,
                  const unsigned int arycnt,
                  long               *target =NULL,
                  Boolean            doframe =False,
                  Boolean            make_if_can=True );
    virtual ~SLOptionMenu();

     void setButton( long );  // the select_value is passed
     Widget buttonW( long );  // the select_value is passed
     int whichSelected();
     char *whichSelectedString();
     void setLabel(char *label);
     void setAltPushAction( SLPushButtonfunc action =NULL, void *data =NULL)
                                   { _altPushAction= action;
                                     _altPushData  = data; };
     virtual void optionAction(int ident);

     virtual Widget make(Widget p =NULL);
     virtual WidgetClass topClass() { return(xmRowColumnWidgetClass); };
     virtual void reloadDefaults(Boolean do_method= True);
     void addButton(char *name, int ident);
     void delButton(int ident);
     virtual Boolean notifyComplex(SLDelay*, int ident);
};


     inline void SLOptionMenu::reloadDefaults(Boolean) {};
#endif
