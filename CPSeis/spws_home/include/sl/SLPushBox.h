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
#ifndef WPROCPUSHBOX_H
#define WPROCPUSHBOX_H

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include "wproc.h"
#include "sl/sl_delay.hh"


class SLColorOption;
class SLOptionMenu;
class SLShellContainer;

typedef struct _SLPush {
                            char      *name;
                            long       ident;
                            
                       } SLPush, *SLPushAry;


typedef struct _SLPushW {
                            Widget            w;
                            long              ident;
                            char             *name;
                            SLShellContainer *contain;
                        } SLPushW, *SLPushAryW;

typedef void (*SLPushButtonfunc)(void*,long);

class SLPushBox : public SLDelay {

  friend class SLOptionMenu;
  private:

    static void DoPushCallback(Widget, XtPointer, XmPushButtonCallbackStruct*);
    static void DoMBAction(Widget, XtPointer, XEvent*);
    SLPushButtonfunc _altPushAction;
    void             *_altPushData;

  protected:
    virtual void  DoPush(Widget, XtPointer, XmPushButtonCallbackStruct*);
    virtual void  MBAction(Widget, XtPointer, XEvent*);
    virtual void  pushAction( long ident);
    long          retidx( long ident);
    Boolean       checkidx( long ident);
    void  preCreate(int &);
    SLPushAryW   _pushary;
    unsigned int _arycnt;
    Boolean      _dotitle;
    long         _box_type;
    void init( const Display *dpy,  SLPushAry pushary);


  public:
     friend class SLColorOption;
     SLPushBox( const Widget           p,
                char                  *name,
                const HelpCtx          hctx,
                const SLPushAry        pushary,
                const unsigned int     arycnt,
                const Boolean          doframe =False,
                const Boolean          dotitle =False,
                const Boolean          make_now=True );
     SLPushBox(       SLDelay          *contain,
                      char             *name,
                const HelpCtx          hctx,
                const SLPushAry        pushary,
                const unsigned int     arycnt,
                const Boolean          doframe =False,
                const Boolean          dotitle =False,
                const Boolean          make_if_can=True );
     SLPushBox( const PsuedoWidget    *pw,
                char                  *name,
                const HelpCtx          hctx,
                const SLPushAry        pushary,
                const unsigned int     arycnt,
                const Boolean          doframe =False,
                const Boolean          dotitle =False );
    ~SLPushBox();
     Widget pushW( long ident );      
     virtual WidgetClass topClass() { return(xmRowColumnWidgetClass); };
     virtual Widget make(Widget p =NULL);
     void setAltPushAction( SLPushButtonfunc action =NULL, void *data =NULL)
                                   { _altPushAction= action;
                                     _altPushData  = data; };
     void setPushType(long t) { _box_type= t; }
     enum _ptype {POPUP, PULLDOWN, NORMAL};
     void addButton(char *name, int ident, SLShellContainer *contain =NULL);
     void addButton(char *name, SLShellContainer *contain);
     void delButton(int ident);
     int getAnyIdent(){ return _pushary[0].ident;}
     Boolean pushExist(int ident);
};

#endif
