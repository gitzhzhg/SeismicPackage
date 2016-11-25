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
#ifndef SLBASE_H
#define SLBASE_H
#include <Xm/Xm.h>
#include "wproc.h"

class SLBase; 


//typedef void (SLBase::*SLBaseAnyMethod)();

class SLBase {

  private:

    static void widgetDestroyedCallback ( Widget, XtPointer, XtPointer );
    static void delayDeleteHandler(Widget w, XtPointer udata, XEvent *event);

  private:
    Widget   _topw;
    Widget   _p_of_child;  // could be same as _topw
    Widget   _framew;
    HelpCtx  _hctx;
    Boolean  _destroy_widget;
    Boolean  _us_unmap_instead_unmange;
  protected:
    Display  *_dpy;
    static const char *SLBlankSpace;
    static const char *FRAME_EXT;
    int      _num_events;
    SLBase ();
    SLBase (const HelpCtx);
    virtual ~SLBase();
    void installDestroyHandler(); // Easy hook for derived classes
    virtual void widgetDestroyed(Widget w);
                           // Called by widgetDestroyedCallback()
    void setDefaultResources ( const Widget,  const char   *name,
                               const String *, Boolean prepend_star= True);
    void setDefaultResources ( const Display*, const char *name, 
                               const String *, Boolean prepend_star= True);
    void setResources ( const Display*, const char*, const String *);
    void getResources ( const XtResourceList, const int );

    // two routine to add help.  the first takes a widget and adds help
    // to that widget, the other adds help to topw

    void install_help(Widget w){if (_hctx) add_HELP(w,helper,_hctx); };
    Widget doFrame(Widget p, char *name);
    virtual void managing() {};       // called when manage happens
    virtual void unmanaging() {};     // called when unmanage happens

    void setTopWidget (const Widget w);
    void setParentOfChildren(const Widget w) {_p_of_child= w;}

    void destroyWidget(Boolean b) {_destroy_widget= b;}
    void doDelayDelete(Widget w);

    // for outputing defaults


  public:

    // Public access functions

    virtual const char *const className () const { return "SLBase"; }
    virtual void manage();                     // Manage widget tree
    virtual void unmanage();                   // Unmanage widget tree
    const Widget frameW() const { return _framew; } // Get frame Widget ID
    const Widget parentOfChildren() const 
                 { return _p_of_child ? _p_of_child : _topw; }

    const Widget topWidget() const 
           { return _p_of_child ? _p_of_child : _topw; }
    const Widget W() const { return (_framew ? _framew : _topw ); }
    HelpCtx getHelpCtx() { return _hctx;}
    void    setHelpCtx(HelpCtx hctx) {_hctx= hctx;}

    virtual void setSensitivity(Boolean s) 
                { if (_topw) XtSetSensitive(_topw,s);}
    virtual Boolean sensitivity() 
                { if (_topw) return XtIsSensitive(_topw); else return True;}
    void delayDelete (int num_events = 1);
    void useUnmapInsteadOfUnmanage(Boolean u)
                  {_us_unmap_instead_unmange= u;}
    virtual Boolean isDialog()     { return False; }
    virtual Boolean isContainer()  { return False; }
    virtual Boolean isTopLevel()   { return False; }
    virtual Boolean isWidgetBased(){ return True; }
    virtual WidgetClass dialogShellType() {return (WidgetClass)NULL;}

    Dimension oWidth();
    Dimension oHeight();
    Position xPos();
    Position yPos();
    Display *display();

    void install_help();
};
#endif
