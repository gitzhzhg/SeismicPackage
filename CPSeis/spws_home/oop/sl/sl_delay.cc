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
#include "sl/sl_delay.hh"
#include "sl/psuedo_widget.hh"
#include <Xm/DialogS.h>
#include <Xm/Frame.h>
#include <assert.h>
#include "cprim.h"

DelayList SLDelay::_orphan_list;  


SLDelay::SLDelay(const char *name, 
                 const HelpCtx hctx, 
                 const Boolean do_frame)  :
                    SLBase(hctx), _widget_tree_created(False), 
                    _pw_topw(NULL), _do_frame(do_frame), _wparent(NULL),
                    _sl_parent(NULL), _complex_notify_obj(NULL),
                    _complex_ident(0), _widget_sensitive(True)
{
  _orphan_list.add(this);
  _name= newstr(name);
  _dpy= NULL;
}

SLDelay::SLDelay(const Widget p, 
                 const char *name, 
                 const HelpCtx hctx,
                 const Boolean do_frame) :

   SLBase(hctx), _widget_tree_created(False), 
   _pw_topw(NULL), _do_frame(do_frame), _wparent(p),
   _sl_parent(NULL), _complex_notify_obj(NULL), _complex_ident(0),
   _widget_sensitive(True)
{
  _orphan_list.add(this);
  _name= newstr(name);
  _dpy= XtDisplay(p);
}


// should not be used
SLDelay::SLDelay(const PsuedoWidget *, 
                 const char         *name, 
                 const HelpCtx hctx,
                 const Boolean do_frame) :

   SLBase(hctx), _widget_tree_created(False), 
   _pw_topw(NULL), _do_frame(do_frame), _wparent(NULL),
   _sl_parent(NULL), _complex_notify_obj(NULL), _complex_ident(0),
   _widget_sensitive(True)
{
  //supportUnmadeDefaults(pw);
  _orphan_list.add(this);
  _name= newstr(name);
}


SLDelay::SLDelay(SLDelay *contain, 
                 const char *name, 
                 const HelpCtx hctx,
                 const Boolean do_frame) :

   SLBase(hctx), _widget_tree_created(False), _pw_topw(NULL), 
   _do_frame(do_frame), _wparent(NULL), _sl_parent(contain),
   _complex_notify_obj(NULL), _complex_ident(0), _widget_sensitive(True)
{
  if (contain) {
     if (contain->isContainer()) {
          contain->addChild(this); 
          _wparent= contain->wParent();
          if (!getHelpCtx()) setHelpCtx(contain->getHelpCtx());

          if (_wparent)           _dpy= XtDisplay(_wparent);
          else if (canDefault())  _dpy= XtDisplay(contain->pW()->anyW());
          else                    _dpy= NULL;
     }
     else 
          printf("SLDelay: class %s is not a container.\n", contain->_name);
  }
  else {
     _orphan_list.add(this);
     _dpy= NULL;
  }
  _name= newstr(name);
}


SLDelay::~SLDelay()
{
  for(SLDelay *q= _child_list.top(); (q!=NULL); q= _child_list.top() )
                delete q;
  if (_sl_parent)  _sl_parent->_child_list.remove(this);
  else             _orphan_list.remove(this);
  if (_pw_topw) delete _pw_topw; 
  if (_name)    free(_name);
}


#define MIXPARENTS \
   "SLDelay::make: parent Widget is not the the same as SLDelay parent.\n"
#define NOPARENTS \
   "SLDelay::make: no parent Widget has been set for the class.\n"
#define NOSLPARENTS \
   "SLDelay::make: no SLDelay parent for none Widget based class.\n"


Widget SLDelay::make(Widget p) 
{ 
   /*
    * The following code gets rather complesx.  A parent widget can come from 
    * one of three places in order of precidents:  The SLDelay parent, 
    * the passed widget, or the parent already set in this class.  If the
    * passed parent widget does no match the SLDelay parent 
    * widget (if it exist) then an * error is signaled.  
    * If no parent is set (except for toplevel) an error is signaled.  
    *
    */
  if (!_widget_tree_created) {
       Widget the_parent= NULL;

       if (_sl_parent) {
            the_parent= _sl_parent->parentOfChildren();
            if (!getHelpCtx()) setHelpCtx(_sl_parent->getHelpCtx());
       }
       else 
            the_parent= p;
 
       if (p) {
            if (p!=the_parent) {
                printf(MIXPARENTS);
                assert(False);
            }
            _wparent= the_parent;
       }
       else if (the_parent) {
            _wparent= the_parent;
            }
       else {
            the_parent= _wparent;
       }

       _widget_tree_created= True; 

       if (!the_parent) {
          if (isWidgetBased()) {
               if (!isTopLevel()) { // no parent if top level 
                  printf(NOPARENTS);
                  assert(False);
               } // end else
          } // end if
          else {
             if (_sl_parent) {
                  if (_sl_parent->isWidgetBased()) {
                         printf(NOPARENTS);
                         assert(False);
                  } //end if
             } // end if
             else {
                  printf(NOSLPARENTS);
                  assert(False);
             } // end else
          } // end else
       } // end if (!the_parent)
       else {
         setSensitivity (_widget_sensitive); 
       } // end else if (the_parent)
  }
  return parentOfChildren();
}


void SLDelay::makeChildren()
{
  for(SLDelay *q= _child_list.top(); (q!=NULL); q= _child_list.next() )
          q->make(parentOfChildren());
}




void SLDelay::supportUnmadeDefaults(const Widget p)
{
 if (_pw_topw) delete _pw_topw;

 if ( (isDialog()) && (_do_frame) ) {
       char dname[200];
       char frame_name[300];
       sprintf(dname, "%s_popup", _name);
       sprintf(frame_name, "%s%s",_name, FRAME_EXT);
       _pw_topw= new PsuedoWidget( p, dname,      xmDialogShellWidgetClass, 
                                      frame_name, xmFrameWidgetClass,
                                      _name,      topClass(),      NULL );
 }
 else if (_do_frame) {
       char frame_name[300];
       sprintf(frame_name, "%s%s", _name, FRAME_EXT);
       _pw_topw= new PsuedoWidget( p, frame_name, xmFrameWidgetClass, 
                                      _name, topClass(),      NULL );
 }
 else if ( isDialog() ) {
       char dname[200];
       sprintf(dname, "%s_popup", _name);
       /*
        * create the shell and the topClass PsuedoWidget .  Always
        * call the shell PsuedoWidget a dialog shell nomatter what shell
        * is really will be.  This will allows setting resources that
        * can help determine the shell type.
        */
       _pw_topw= new PsuedoWidget( p, dname, xmDialogShellWidgetClass, 
                                      _name, topClass(),      NULL );
 }
 else
      _pw_topw= new PsuedoWidget(p, _name, topClass(), NULL );
 _dpy= (Display*)_pw_topw->display();

}

void SLDelay::supportUnmadeDefaults(const SLDelay *sl_parent)
{
  if (sl_parent->made()) {
          supportUnmadeDefaults(sl_parent->topWidget());
  }
  else {
          supportUnmadeDefaults(sl_parent->pW());
  }

}

void SLDelay::supportUnmadeDefaults(const PsuedoWidget *pw)
{
 PsuedoWidget *parent; 

 if (_pw_topw) delete _pw_topw;

 if (_do_frame) {
       char frame_name[300];
       sprintf(frame_name, "%s%s", _name, FRAME_EXT);
       parent=  new PsuedoWidget( pw,  frame_name, xmFrameWidgetClass );
 }
 else parent= (PsuedoWidget*)pw;

 if ( isDialog() ) {
       char dname[200];
       PsuedoWidget *tpw;
       sprintf(dname, "%s_popup", _name);
       /*
        * create the shell PsuedoWidget then the topClass.  Always
        * call the shell PsuedoWidget a dialog shell nomatter what shell
        * is really will be.  This will allows setting resources that
        * can help determine the shell type.
        */
       tpw=      new PsuedoWidget( parent,  dname, xmDialogShellWidgetClass );
       _pw_topw= new PsuedoWidget( tpw, _name,  topClass() );
       delete tpw;
 }
 else {
      _pw_topw= new PsuedoWidget( parent, _name, topClass() );
 }
 if (_do_frame) delete parent;
}


void SLDelay::reloadChildrenDefaults(Boolean do_method)
{
  void *x;
  SLDelay *q;
  for(q= _child_list.top(&x);  (q); q= _child_list.next(&x) )
            q->reloadDefaults(do_method);
}

void SLDelay::reloadAllDefaults(Boolean do_method)
{
  void *x;
  SLDelay *q;
  for(q= _orphan_list.top(&x);  (q); q= _orphan_list.next(&x) )
            q->reloadDefaults(do_method);
}



void SLDelay::reloadChildrenSystemDefaults(Boolean do_method)
{
  void *x;
  SLDelay *q;
  for(q= _child_list.top(&x);  (q); q= _child_list.next(&x) )
            q->reloadSystemDefaults(do_method);
}

void SLDelay::reloadAllSystemDefaults(Boolean do_method)
{
  void *x;
  SLDelay *q;
  for(q= _orphan_list.top(&x);  (q); q= _orphan_list.next(&x) )
            q->reloadSystemDefaults(do_method);
}


void SLDelay::updateChildren()
{
  void *x;
  SLDelay *q;
  for(q= _child_list.top(&x);  (q); q= _child_list.next(&x) ) q->update();
}

void SLDelay::updateAll()
{
  void *x;
  SLDelay *q;
  for(q= _orphan_list.top(&x);  (q); q= _orphan_list.next(&x) ) q->update();
}

void SLDelay::makeAll()
{
  void *x;
  SLDelay *q;
  for(q= _orphan_list.top(&x);  (q); q= _orphan_list.next(&x) ) q->make();
}



void SLDelay::setComplexNotify(SLDelay *obj)
{
  _complex_notify_obj= obj;
}


void SLDelay::setComplexIdent(int ident)
{
 _complex_ident= ident;
}


Boolean SLDelay::callNotifyComplex(int ident)
{
  Boolean doit = True;
  if(_complex_notify_obj)doit= _complex_notify_obj->notifyComplex(this,ident);
  else if(slParent())    doit= (slParent())->notifyComplex(this,ident);
  return doit;
}

Boolean SLDelay::callNotifyComplex()
{
  return callNotifyComplex(_complex_ident);
}

Boolean SLDelay::notifyComplex(SLDelay*, int) {return True;}

void SLDelay::setSensitivity (Boolean s)
{
  _widget_sensitive = s;
  SLBase::setSensitivity (_widget_sensitive);
}

Boolean SLDelay::sensitivity ()
{
  return _widget_sensitive;
}

void SLDelay::setFallbackResources (const char **defres)
{
  assert(_pw_topw);
  Widget any = _pw_topw->anyW();
  assert(any);
  Widget w = get_toplevel_shell(any);
  assert(w);
  char name[222];
/*
  get_full_name(w, name);
  strcat(name, "*");
  strcat(name, instanceName());
*/
  strcpy(name, XtName(w));
  if(!isTopLevel())
      {
      strcat(name, "*");
      strcat(name, _name);
      }
  setDefaultResources(XtDisplay(any), name, (char**)defres, False);
}

