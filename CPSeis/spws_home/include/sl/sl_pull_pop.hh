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
#ifndef SLPULLPOP_HH
#define SLPULLPOP_HH

// $Id: sl_pull_pop.hh,v 1.2 2004/06/07 13:49:18 wjdone Exp $
// $Name:  $


typedef void (*SLSelectionFunc)(void*,long ident);
//typedef void (*SLPushButtonFunc)(void*,long ident);

#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include "sl/sl_delay.hh"

class SLApp;
class SLPrim;
class SLShellContainer;
class RadioList;


typedef struct _SLPushPop {
                            SLPrim           *obj;
                            SLSelectionFunc func;
                            void             *data;
                            int              element_type;
                          } SLPushPop, *SLPushPopAry;



class SLPullPop : public SLDelay {
  private:
    static SLPullPop *_last_obj;
    static void  oneWidget(Widget w, void *data);
    static void  DoMBAction(Widget, XtPointer, XEvent*);
    static void  doMapCallback(Widget, XtPointer, XEvent*);



  protected:
    Widget            _cascade;
    SLSelectionFunc  _default_func;
    void             *_default_data;
    SLPushPopAry     _ppary; 
    unsigned int     _arycnt; 
    int              _last_ident;
    const char       *_last_name;
    Boolean          _radio_set;
    Boolean          _toggle_added;
    int              _menu_type;
    Widget           _popup_title;
    Widget           _rc;
    int              _ident;
    SLApp           *_slapp;
    void           (*_mapCallbackProc)(void*);
    void            *_mapCallbackInstance;

    int          retidx( const int ident) const;
    int          retidx( const char* const name) const;
    virtual void MBAction(Widget, XtPointer, XEvent*);
    void         new_and_copy();
    void         checkUnique(const int idx);
    void         addNewElement(int etype);
    void         init(Widget p, Boolean inpull, Widget work= NULL);
    void         doPulldown(Widget p);
    void         doPopup(Widget work);

  public:
     enum {PUSH, PUSHUP, TOG, RADIO, CASCADE, NOIDENT=99912343};

     /*
      * Constructor for putting pulldown menus or popup menus on a SLApp
      */
     SLPullPop(char *name, HelpCtx hctx, SLApp *app, Boolean ispull =True);

     /*
      * Constructor for putting cascade menus on a SLPullPop
      */
     SLPullPop(char *name, SLPullPop *pullpop, const int ident =NOIDENT);

     /*
      * Constructor for popup menus on any Widget
      */
     SLPullPop(char *name, Widget w, HelpCtx hctx);

     /*
      * Constructor for cascade buttons or popups on any SL Class
      */
     SLPullPop(SLDelay *contain, 
               char    *name, 
               Boolean  is_cascade =True,
               Boolean  make_if_can =True);


     ~SLPullPop();

     virtual Widget make(Widget p =NULL);

     /*
      * function to add Push Buttons that will popup a SLShellContainer
      */
     void addPushUp( char *name, SLShellContainer *contain,
                     const int wconst =NOIDENT);
     void addPushUp( char *name, SLShellContainer *contain,
                     SLSelectionFunc func,
                     void *pdata =NULL, const int wconst =NOIDENT);

     /*
      * function to add Push Buttons 
      */
     void addPush( char *name, const int wconst);
     void addPush( char *name, SLSelectionFunc func =NULL, 
                   void *pdata =NULL, const int wconst =NOIDENT);

     /*
      * functions for radio buttons and functio to get & set values
      */
     void addRadio( char *name, const int wconst);
     void addRadio( char *name, SLSelectionFunc func =NULL, 
                    void *pdata =NULL, const int wconst =NOIDENT);
     int  radioValue() const;
     void setRadioValue(const int);
     void setRadioValue(const char* const name);

     /*
      * functions for toggle buttons and functio to get & set values
      */
     void addTog( char *name, const int wconst);
     void addTog( char *name, SLSelectionFunc func =NULL, 
                  void *pdata =NULL, const int wconst =NOIDENT);
     Boolean toggleValue(const int ) const;
     Boolean toggleValue(const char* const name) const;
     void    setToggleValue(const int, const Boolean set);
     void    setToggleValue(const char* const name, const Boolean set);


     /*
      * set title on Popup Menu
      */
     void setTitle(char *title);

     Widget addSep(char *name ="separator");


     /*
      * general
      */
     void setAltPushAction( SLSelectionFunc action =NULL, void *data =NULL)
                                   { _default_func= action;
                                     _default_data= data; }

     long lastIdent() const                  {return _last_ident;}
     const char *lastName()  const           {return _last_name;}
     void sensitive(Boolean, int, ...);   // ends with -1
     void sensitive(Boolean, char*, ...); // ends with NULL
     static SLPullPop *lastPushedObj() {return _last_obj; }
     const Widget getWidget(const int ident) const;
     const Widget getWidget(const char *name) const;

     SLPrim *primObj(const int ident) const;
     SLPrim *primObj(const char *name) const;

     void setNumColumns(const int nc);

     virtual WidgetClass topClass() { return(xmRowColumnWidgetClass); };
     virtual Boolean isContainer() { return True; };

     virtual Boolean notify(SLPrim *obj);
     virtual Boolean notifyComplex(SLDelay*, int ident);


     void manageCascade();
     void unmanageCascade();
     void unmapCascade();
     void mapCascade();

     void handleMapCallback();
     void setMapCallbackProc(void (*f)(void*), void *instance);
};

#endif
