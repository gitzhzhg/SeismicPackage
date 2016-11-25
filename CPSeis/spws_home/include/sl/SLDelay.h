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
#ifndef SLDELAY_H
#define SLDELAY_H


#include <assert.h>
#include "wproc.h"
#include "sl/sl_base.hh"
#include "sl/delay_list.hh"


class PsuedoWidget;
class SLPrim;


class SLDelay : public SLBase {

  protected:
    char              *_name;
    Boolean           _widget_tree_created;
    PsuedoWidget      *_pw_topw;
    Boolean           _do_frame;
    DelayList         _child_list;    // used only with containers
    static DelayList  _orphan_list;  
    Widget            _wparent;
    SLDelay           *_sl_parent;
    SLDelay           *_complex_notify_obj;
    int                _complex_ident;
    Boolean            _widget_sensitive;

    SLDelay(const char *name, 
            const HelpCtx          =NULL, 
            const Boolean do_frame =False);
    SLDelay(const Widget w, 
            const char *name, 
            const HelpCtx = NULL, 
            const Boolean do_frame =False);
    SLDelay(const PsuedoWidget *pw, 
            const char *name, 
            const HelpCtx = NULL, 
            const Boolean do_frame =False);
    SLDelay(SLDelay *contain, 
            const char *name, 
            const HelpCtx = NULL, 
            const Boolean do_frame =False);
    Widget makeFrameIfNeeded() 
                  { assert (_widget_tree_created);
                    return ( _do_frame ? doFrame(_wparent,_name) : _wparent );}
    Widget makeFrameIfNeeded(Widget p) 
                  { return ( _do_frame ? doFrame(p,_name) : p ); }


    virtual ~SLDelay();


  public:

    // make widget tree & manage it
    virtual void makeAndManage(Widget p =NULL) { make(p); manage();}

    // make widget tree
    virtual Widget make(Widget p =NULL); 

    void makeChildren();

    const char *instanceName() const { return _name; }

    const Boolean made() const { return (_widget_tree_created); }
    const Boolean canDefault() const { return (_pw_topw ? True : False); }
    void addChild(SLDelay *c) { _child_list.add(c); }
    // --- todo ---
    SLDelay *topChild(void **x = (void **)0)  { return _child_list.top(x);}
    SLDelay *nextChild(void **x = (void **)0) { return _child_list.next(x);}
    static SLDelay *topOrphan(void **x = (void **)0) 
                           { return _orphan_list.top(x);}
    static SLDelay *nextOrphan(void **x = (void **)0)
                           { return _orphan_list.next(x);}
    // --- todo ---


    virtual void supportUnmadeDefaults(const Widget p);
    virtual void supportUnmadeDefaults(const PsuedoWidget *pw);
    virtual void supportUnmadeDefaults(const SLDelay *sl_parent);

    virtual void reloadDefaults(Boolean do_method= True)
                            { reloadChildrenDefaults(do_method); };
    void reloadChildrenDefaults(Boolean do_method= True);
    static void reloadAllDefaults(Boolean do_method= True);

    virtual  void reloadSystemDefaults(Boolean do_method= True)
                            { reloadChildrenSystemDefaults(do_method); };
    void reloadChildrenSystemDefaults(Boolean do_method= True);
    static void reloadAllSystemDefaults(Boolean do_method= True);

    virtual  void update() { updateChildren();}
    void updateChildren();
    static void updateAll();
    static void makeAll();

    const PsuedoWidget *pW() const { return  (_pw_topw);} 
    SLDelay *slParent() const      { return _sl_parent;}
    const Widget wParent () const
     {return _wparent ? _wparent : 
               (_sl_parent ? _sl_parent->parentOfChildren() : (Widget)NULL);}

    virtual Boolean notify(SLPrim*) {return True;}
    virtual Boolean notifyComplex(SLDelay*, int ident);
    void setComplexNotify(SLDelay *obj);
    Boolean callNotifyComplex(int ident);
    Boolean callNotifyComplex();  // this will pass the _complex_ident
 

    void setComplexIdent(int ident);
    void complexIdent(int ident);

    virtual WidgetClass topClass()= 0;
    virtual Boolean isDialog()     { return False; }
    virtual Boolean isContainer()  { return False; }
    virtual Boolean isTopLevel()   { return False; }
    virtual Boolean isWidgetBased(){ return True; }

    virtual void setSensitivity (Boolean s);
    virtual Boolean sensitivity ();

    void setFallbackResources (const char **defres);
};
#endif
