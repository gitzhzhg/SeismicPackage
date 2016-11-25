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
/*
 * Name        : SLBase
 * File        : SLBase.cxx
 * Executable  : -
 * Author      : Trey Roby
 * Date        : 2/1/93
 *
 * This class is the root class for all other class in the library
 * The concepts and the initial pass are based on Doug Young's 
 * "Object Oriented Programming with C++ and OSF/Motif" book.
 *
 * 
 * CHANGES:
 */
#include <assert.h>
#include <stdio.h>
#include "sl/sl_base.hh"
#include <Xm/Frame.h>
#include <Xm/DialogS.h>
#include <stdio.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>


#define DELAY_DELETE  3434533


const char *SLBase::SLBlankSpace= "SLBlankSpace";
const char *SLBase::FRAME_EXT= "_Frame";

SLBase::SLBase () : _topw(NULL),  _hctx(NULL), _framew(NULL),
                    _p_of_child(NULL),
                    _destroy_widget(True), _dpy(NULL),
                    _us_unmap_instead_unmange(False)
{ }

SLBase::SLBase (const HelpCtx hctx) :  _topw(NULL),_hctx(hctx), _framew(NULL),
                                       _p_of_child(NULL),
                                       _destroy_widget(True), _dpy(NULL),
                                       _us_unmap_instead_unmange(False)
{ }

SLBase::~SLBase()
{

    if ( _topw ) {
        XtRemoveCallback ( _topw, XmNdestroyCallback,
                          &SLBase::widgetDestroyedCallback,
                          (XtPointer) this );
        if (_destroy_widget) {
            if ((!_framew) && (XtIsShell(XtParent(_topw)))) {
               Widget shell= XtParent(_topw);
#ifdef LINUX
// this is to resolve a bug that took 4 weeks and 3 days to find & fix.
//   it occured only on linux, the GEOPRESS(tm) program was crashing when
//   a independent plot was removed.  the crash occured deep in X in
//   XmGetClassExtensionPtr and seemed to be related to functions 
//   Destroy/Phase2Destroy/Recursive around a SetCascadeField.  Schmauch
//   discovered that it occured when destroying a shell widget that was NOT
//   a window manager shell.  he made the following 1-line fix to resolve it.
//   he was not worried about a memory leak because he believed the parent
//   widget (which could be a window manager shell) would clean up these NON 
//   WM shells recursively thereby not causing a memory leak.  types of NON
//   WM shells include popup menus off of MB3 and cascade menus off of the
//   menu bar.  Schmauch believed this was a linux motif implementation bug
//   but he would not rule out the possibility that our popup or cascade
//   creation code was faulty.  we simply didn't have the luxury of time to
//   pursue it. (KCC)
// 
               if (XtIsWMShell(shell))
#endif
                 XtDestroyWidget(shell);
            } 
            else
              {
               XtDestroyWidget ( _topw );
              }
        } // end if _destroy_widget
    } //End if
    if ( ( _framew )&&(_destroy_widget) ) 
      {
      XtDestroyWidget ( _framew );
      }
}



Widget SLBase::doFrame(Widget p, char *name)
{
 char frame_name[300];
 sprintf(frame_name, "%s%s", name, FRAME_EXT);
 _framew= XtVaCreateManagedWidget( frame_name, xmFrameWidgetClass, p, NULL);
 return (_framew);
}

void SLBase::install_help()
{
  if (_hctx) {
     if (_topw) add_HELP(_topw,helper,_hctx);
     else 
      printf("SLBase::install_help: Cannot add help - no top Widget set.\n");
  }
}



void SLBase::widgetDestroyedCallback ( Widget w, XtPointer CBdata, XtPointer)
{
    SLBase *obj = (SLBase *) CBdata;
    obj->widgetDestroyed(w);
}


void SLBase::widgetDestroyed(Widget w)
{

  /*
   * Because of synchronization it is posible that the class has already
   * been deleted and realloced into the same address before the destroy
   * can be called.  In that case _topw is not right widget and the wrong
   * class instance is being called.  Therefore if the passed widget is not
   * _topw don't do any thing.
   */

  if (w == _topw) {
          _topw= NULL;
          _framew= NULL;
          _p_of_child= NULL;
  }
}

void SLBase::setTopWidget(const Widget w)
{ 
  if (w) {
      if (_topw)
            XtRemoveCallback ( _topw, XmNdestroyCallback, 
                           &SLBase::widgetDestroyedCallback, (XtPointer) this);
      _topw=w; 
      _dpy= XtDisplay(w);
      if (!_p_of_child) _p_of_child=w;
      installDestroyHandler();
  }
  else {
      _p_of_child= NULL;
      _topw=NULL; 
  }
} 

void SLBase::installDestroyHandler()
{
   assert ( _topw != NULL );
   XtAddCallback ( _topw, XmNdestroyCallback, 
                   &SLBase::widgetDestroyedCallback, (XtPointer) this );
}

void SLBase::manage()
{
    assert ( _topw != NULL );
    assert ( XtHasCallbacks ( _topw, XmNdestroyCallback ) ==
            XtCallbackHasSome );
    if (_framew) XtManageChild ( _framew );
    XtManageChild ( _topw );
    if (_us_unmap_instead_unmange)
           XtVaSetValues(_topw, XmNmappedWhenManaged, True, NULL);
    if (isDialog()) {
           assert(dialogShellType());
           if (dialogShellType() != xmDialogShellWidgetClass) {
                      XtPopup(XtParent(_topw), XtGrabNone);
           }
    }
    if (isDialog()) {
       Window window= XtWindow(XtParent(_topw));
       if (window) {
             XMapWindow( XtDisplay(_topw), window);
             XRaiseWindow(XtDisplay(_topw), window);
       }
    }
    managing();
}




void SLBase::unmanage()
{
    if (_topw != NULL ) {
         if (_framew) XtUnmanageChild ( _framew );
         if (_us_unmap_instead_unmange)
                   XtVaSetValues(_topw, XmNmappedWhenManaged, False, NULL);
         else
                   XtUnmanageChild ( _topw );
         if (isDialog()) {
                assert(dialogShellType());
                if (dialogShellType() != xmDialogShellWidgetClass) {
                      XtPopdown(XtParent(_topw));
                }
         }
    }
    unmanaging();
}


void SLBase::getResources ( const XtResourceList res,
                               const int            num )
{
    // Check for errors

    assert ( _topw != NULL );
    assert ( res != NULL );

    // Retrieve the requested resources relative to the
    // parent of this object's base widget

    XtGetSubresources ( XtParent( _topw ), (XtPointer) this,
                       XtName(_topw), className(),
                       res, num, NULL, 0 );
}



void SLBase::setDefaultResources ( const Widget w,
                                   const char   *name,
                                   const String *resourceSpec,
                                         Boolean prepend_star )
{
   setDefaultResources(XtDisplay(w), name, resourceSpec, prepend_star);
}


void SLBase::setDefaultResources ( const Display *dpy,
                                   const char    *name,
                                   const String  *resourceSpec,
                                         Boolean prepend_star )
{
   char buf[1000];
   XrmDatabase system_db, new_res_db;
   int i, j;


   /*
    *  Add the Component resources, prepending the name of the component
    */
#if (XlibSpecificationRelease>=5)
    /*
     * if the release is 5 or later then when must support multiple
     * screens.
     */
   for(i=0; (i<ScreenCount(dpy)); i++) {
       new_res_db = XrmGetStringDatabase ( "" );  // make empty res database
       for(j=0; ( resourceSpec[j] != NULL ); j++) {
           if (prepend_star)
                sprintf(buf, "*%s%s", name, resourceSpec[j]);
           else 
                sprintf(buf, "%s%s", name, resourceSpec[j]);
           XrmPutLineResource( &new_res_db, buf );
       }
       /*
        * Merge them into the Xt database, with lowest precendence
        */
       if (new_res_db) {
           system_db = XtScreenDatabase(ScreenOfDisplay(dpy,i));
           XrmCombineDatabase(new_res_db, &system_db, FALSE);
       }
    } // end loop
#else
   /*
    * if the release is before 5 then just do the one database.
    */
   new_res_db = XrmGetStringDatabase ( "" );  // make empty res database
   for(i=0; ( resourceSpec[i] != NULL ); i++)
   {
       sprintf(buf, "*%s%s", name, resourceSpec[i]);
       if (prepend_star)
            sprintf(buf, "*%s%s", name, resourceSpec[i]);
       else
            sprintf(buf, "%s%s", name, resourceSpec[i]);
       XrmPutLineResource( &new_res_db, buf );
   }
   /*
    * Merge them into the Xt database, with lowest precendence
    */
   if (new_res_db) {
         XrmMergeDatabases ( dpy->db, &new_res_db );
         ((Display*)dpy)->db = new_res_db;
   } // End if
#endif
}

void SLBase::setResources ( const Display *dpy,
                            const char    *topname,
                            const String  *resourceSpec )
{
   char buf[1000];

   XrmDatabase rdb = XrmGetStringDatabase ( "" );  // make empty res database

   /*
    *  Add the Component resources, prepending the name of the component
    */
   for(int i=0; ( resourceSpec[i] != NULL ); i++)
   {
       sprintf(buf, "*%s%s", topname, resourceSpec[i]);
       XrmPutLineResource( &rdb, buf );
   }

   /*
    * Merge them into the Xt database, with highest precendence
    */
   if ( rdb ) {
#        if (XlibSpecificationRelease>=5)
              XrmDatabase db = XtDatabase((Display*)dpy);
              XrmCombineDatabase(rdb, &db, True);
#        else
              XrmMergeDatabases ( rdb, &((Display*)dpy)->db);
#        endif
   } // End if
}


Dimension SLBase::oWidth()  
{
   assert(_topw);
   Dimension w;  
   XtVaGetValues( _topw, XmNwidth, &w, NULL);
   return w;
}

Dimension SLBase::oHeight()
{

   assert(_topw);
   Dimension h;  
   XtVaGetValues( _topw, XmNheight, &h, NULL);
   return h;
}

Position SLBase::xPos()
{
   assert(_topw);
   Position x;  
   XtVaGetValues( _topw, XmNx, &x, NULL);
  return x;
}


Position SLBase::yPos()
{
  assert(_topw); 
  Position y;  
  XtVaGetValues( _topw, XmNy, &y, NULL); 
  return y;
}

void SLBase::delayDelete (int num_events)
{
 Status stat;
 XClientMessageEvent ev;
 ev.type         = ClientMessage;
 ev.data.l[0]    = DELAY_DELETE;
 ev.window       = XtWindow(_topw);
 ev.display      = XtDisplay(_topw);
 ev.message_type = XA_INTEGER;
 ev.format       = 32;

 XtAddEventHandler(_topw, NoEventMask, True,
                   (XtEventHandler)delayDeleteHandler, (XtPointer)this );

 stat= XSendEvent(XtDisplay(_topw), XtWindow(_topw),
                  True, NoEventMask,(XEvent*)&ev);
 assert(stat);

 _num_events = num_events;
}


void SLBase::delayDeleteHandler(Widget w, XtPointer udata, XEvent *event)
{
  SLBase *obj = (SLBase *)udata;
  if ( (event->type == ClientMessage) &&
       (event->xclient.data.l[0] == DELAY_DELETE) ) {
         obj->doDelayDelete(w);
  }
}

void SLBase::doDelayDelete(Widget w)
{
   XtRemoveEventHandler(_topw, NoEventMask, True,
              (XtEventHandler)delayDeleteHandler, (XtPointer)this );
   if (w == _topw) {
     if (--_num_events == 0) {
       delete this;
     }
     else {
       delayDelete (_num_events);
     }
   }
}

Display *SLBase::display() { return _dpy;}
