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

//---------------------- wbox_allbox.cc -------------------------//
//---------------------- wbox_allbox.cc -------------------------//
//---------------------- wbox_allbox.cc -------------------------//

//          implementation file for the WboxAllbox class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_screen.hh"
#include "wbox/wbox_box.hh"
#include "oprim/array_bucket.hh"
#include "named_constants.h"
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <Xm/DialogS.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>



//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxAllbox::WboxAllbox()
         :
            _startup           (TRUE),
            _totnumbox         (0),
            _totnumscreens     (0),
            _debug             (0),
            _tinysize          (0),
            _keymode           (0),
            _scrollflag        (0),
            _scrollwidth       (0),
            _maxrows           (0),
            _box_being_created (NULL),
            _trap_box_number   (0),
            _entering_text     (FALSE),
            _box_list          (NULL),
            _screen_list       (NULL),
            _updatename        (NULL),
            _updatedata        (NULL)
{
  _box_list    = new ArrayBucket(10);
  _screen_list = new ArrayBucket(2);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxAllbox::~WboxAllbox()
{
  for(int ibox = 1; ibox <= _totnumbox; ibox++)
      {
      WboxBox *box = getBoxPointer(ibox);
      if(box) delete box;
      }
  for(int iscreen = 1; iscreen <= _totnumscreens; iscreen++)
      {
      WboxScreen *screen = getScreenPointer(iscreen);
      delete screen;
      }
  delete _box_list;
  delete _screen_list;
}



//------------------------- get pointer ---------------------------//
//------------------------- get pointer ---------------------------//
//------------------------- get pointer ---------------------------//

      // private.
      // the first  function   might    return NULL.
      // the second function will never return NULL.
      // the third  function   might    return NULL.
      // the first  function will assert if out of range.
      // the second function will assert if out of range.
      // the second function will assert if the array element is NULL.

WboxBox *WboxAllbox::getBoxPointer(int ibox)  const
{
  if(ibox < 1 || ibox > _box_list->numElements()) return NULL;
  if(!_box_list->validateElement(ibox - 1)) return NULL;
  return (WboxBox*)_box_list->fetchElement(ibox - 1);
}


WboxScreen *WboxAllbox::getScreenPointer(int iscreen)  const
{
  return (WboxScreen*)_screen_list->fetchElement(iscreen - 1);
}


WboxBox *WboxAllbox::findBoxPointer(const char *boxname)  const
{
  for(int ibox = 1; ibox <= _totnumbox; ibox++)
      {
      WboxBox *box = getBoxPointer(ibox);
      if(box != NULL && !strcmp(boxname, box->getBoxName())) return box;
      }
  return NULL;
}



//----------------------- change global resources ----------------------//
//----------------------- change global resources ----------------------//
//----------------------- change global resources ----------------------//


void WboxAllbox::setMaxRows(int maxrows)
{
  _maxrows = MaximumValue(maxrows, 2);
}


   // debug=0 prints nothing.
   // debug=1 prints a line just before calling boxtrap (except motion
   //             events and printable-keypress events).
   // debug=2 also prints printable-keypress and expose details.
   // debug=3 also prints motion events.

void WboxAllbox::setDebug(int debug)
{
  _debug = MaximumValue(debug, 0);
}


void WboxAllbox::setKeymode(int keymode)
{
  _keymode = ConstrainValue(keymode, 1, 3);
}


void WboxAllbox::toggleKeymode()
{
  if     (_keymode <= 1) _keymode = 2;
  else if(_keymode == 2) _keymode = 3;
  else                   _keymode = 1;
}


void WboxAllbox::setTrapBoxNumber(int ibox)
{
  assert(ibox >= 0 && ibox <= _totnumbox);
  _trap_box_number = ibox;
}


void WboxAllbox::textBeingEntered()
{
  _entering_text = TRUE;
}


void WboxAllbox::textEntryCompleted()
{
  _entering_text = FALSE;
}



//------------------------- create1 -------------------------------//
//------------------------- create1 -------------------------------//
//------------------------- create1 -------------------------------//

  // public.

WboxBox *WboxAllbox::create1(WboxGenericTrap *default_trap,
                             int default_traptype, int omit, int nrows_init)
{
  assert(_box_being_created == NULL);
  WboxBox *box = new WboxBox(this, (int)_totnumbox + 1,
                             default_trap, default_traptype, omit, nrows_init);
  _box_list->addElement(box);
  _totnumbox = _box_list->numElements();
  _box_being_created = box;
  return box;
}



//------------------------- create2 -------------------------------//
//------------------------- create2 -------------------------------//
//------------------------- create2 -------------------------------//

  // public.
  // (if hctx = NULL, helpfile and helptitle are used to create help)
  // (if w = NULL, drawing area widget is created)
  // (if parent & w = NULL, dialog box is created to contain drawing area)
  // (if parent & w = NULL, toplevel will be the parent of dialog box)
  // (if parent & w = NULL, dialog box will be the parent of drawing area)
  // (at least one of w, parent, and toplevel must not be NULL)
  // (toplevel does not have to be the actual toplevel shell)


Widget WboxAllbox::create2(const char *boxname,
               Widget toplevel, Widget parent, Widget w,
               HelpCtx hctx, const char *helpfile, const char *helptitle)
{
  assert(_box_being_created != NULL);
  if(parent == NULL && w == NULL)
       {
       if(toplevel == NULL)
            { printf("wbox: toplevel, parent, and w cannot all be NULL\n");
              exit(0); }
       Arg args[3];
       int i = 0;
       parent = XmCreateDialogShell(toplevel, (char*)boxname, args, i);
       }
  
  if(toplevel == NULL) toplevel = parent;
  if(toplevel == NULL) toplevel = w;
  assert(toplevel);
  toplevel = get_toplevel_shell(toplevel);

  Widget widget = parent;
  if(widget == NULL) widget = w;
  assert(widget);

  if(_startup)
       {
       getResources(widget);
       _startup = FALSE;
       }

  WboxScreen *screen = NULL;
  for(int iscreen = 1; iscreen <= _totnumscreens; iscreen++)
       {
       WboxScreen *screen2 = getScreenPointer(iscreen);
       if(screen2->sameScreen(widget)) screen = screen2;
       }
  if(screen == NULL)
       {
       screen = new WboxScreen(this, widget);
       _screen_list->addElement(screen);
       _totnumscreens = _screen_list->numElements();
       }

  WboxBox *box = _box_being_created;
  box->completeCanvas (screen, boxname, parent, w,
                       hctx, toplevel, helpfile, helptitle);

  _box_being_created = NULL;
  return box->getW();
}



//----------------- inform windowbox destroyed -----------------------//
//----------------- inform windowbox destroyed -----------------------//
//----------------- inform windowbox destroyed -----------------------//

            // called from WboxEvents destroy callback.

void WboxAllbox::informWindowboxDestroyed(WboxBox *box)
{
  int ibox = box->getIbox();
  _box_list->removeElements(ibox - 1, ibox - 1);
  _totnumbox = _box_list->numElements();
  delete box;
}



//---------------------------- misc functions ------------------------//
//---------------------------- misc functions ------------------------//
//---------------------------- misc functions ------------------------//


void WboxAllbox::registerAdditionalUpdate
                     (WboxUpdate *updatename, void *updatedata)
{
  _updatename = updatename;
  _updatedata = updatedata;
}


void WboxAllbox::doAdditionalUpdate()
{
  if(_updatename && !_startup) _updatename(_updatedata);
}



void WboxAllbox::flushBuffer()
{
  for(int iscreen = 1; iscreen <= _totnumscreens; iscreen++)
      {
      WboxScreen *screen = getScreenPointer(iscreen);
      screen->flushBuffer();
      }
}



void WboxAllbox::ringBell()
{
  if(_totnumscreens >= 1)
      {
      WboxScreen *screen = getScreenPointer(1);
      screen->ringBell();
      }
}



void WboxAllbox::wasteTime(int n)  const
{
  for(int i = 0; i < 3 * n; i++)
      {
      double j = tan(1.23456789);
      }
}




//------------------ custom resources structure --------------------//
//------------------ custom resources structure --------------------//
//------------------ custom resources structure --------------------//

struct Custom
{
  int debug, tinysize, keymode, scrollflag, scrollwidth, maxrows;
};



//------------------------- resource array ---------------------------//
//------------------------- resource array ---------------------------//
//------------------------- resource array ---------------------------//

#define III XtRInt   , sizeof(int   )
#define PPP XtRPixel , sizeof(Pixel )
#define SSS XtRString, sizeof(String)
#define JJ  XtRImmediate
#define SS  XtRString
#define RRR(n,i,p,j,c)  { n, "Wbox", i, XtOffsetOf(Custom,p), j, c },


static XtResource resources[] = {
  RRR("wboxdebug"       ,III,debug       ,JJ,(XtPointer)0  )
  RRR("wboxtiny"        ,III,tinysize    ,JJ,(XtPointer)0  )
  RRR("wboxkeymode"     ,III,keymode     ,JJ,(XtPointer)1  )
  RRR("wboxscrollbars"  ,III,scrollflag  ,JJ,(XtPointer)2  )
  RRR("wboxscrollwidth" ,III,scrollwidth ,JJ,(XtPointer)17 )
  RRR("wboxmaxrows"     ,III,maxrows     ,JJ,(XtPointer)10 )
};



//------------------------ get resources ------------------------------//
//------------------------ get resources ------------------------------//
//------------------------ get resources ------------------------------//


void WboxAllbox::getResources(Widget widget)
{
  Custom custom;
  XtGetApplicationResources(widget, &custom, resources,
                                XtNumber(resources), NULL, 0);
//_debug       = custom.debug;
  _tinysize    = custom.tinysize;
//_keymode     = custom.keymode;
  _scrollflag  = custom.scrollflag;
  _scrollwidth = custom.scrollwidth;
//_maxrows     = custom.maxrows;
  setMaxRows(custom.maxrows);
  setDebug  (custom.debug);
  setKeymode(custom.keymode);
}



//---------------------- update array visibilities ------------------//
//---------------------- update array visibilities ------------------//
//---------------------- update array visibilities ------------------//

      // private.

void WboxAllbox::updateArrayVisibilities()
{
  for(int ibox = 1; ibox <= _totnumbox; ibox++)
      {
      WboxBox *box = getBoxPointer(ibox);
      if(box) box->updateArrayVisibilities();
      }
}



//---------------------- update sensitivities -----------------------//
//---------------------- update sensitivities -----------------------//
//---------------------- update sensitivities -----------------------//

      // private.

void WboxAllbox::updateSensitivities()
{
  for(int ibox = 1; ibox <= _totnumbox; ibox++)
      {
      WboxBox *box = getBoxPointer(ibox);
      if(box) box->updateSensitivity();
      }
}



//------------------------ update scrollbars -----------------------//
//------------------------ update scrollbars -----------------------//
//------------------------ update scrollbars -----------------------//


void WboxAllbox::updateScrollbars()
{
  for(int ibox = 1; ibox <= _totnumbox; ibox++)
      {
      WboxBox *box = getBoxPointer(ibox);
      if(box) box->updateScrollbars();
      }
}



//----------------------------- update -------------------------------//
//----------------------------- update -------------------------------//
//----------------------------- update -------------------------------//

         // public.
         // to be called by the user whenever desired.
         // the additional update is not done here.

void WboxAllbox::update()
{
  if(_startup)              return;
  if(_totnumbox       == 0) return;
  if(_trap_box_number != 0) return;
  if(_entering_text)        return;
  if(_debug >= 1)
      {
      if(_debug >= 2) printf(" \n");
      printf(" --  0 ---------- UPDATE user-requested update\n");
      }
  WboxBox *active_box = NULL;
  int      dummy      = 0;
  privateUpdate(active_box, "UPDATE", dummy, dummy, dummy, dummy);
}



//------------------------- update fields ----------------------------//
//------------------------- update fields ----------------------------//
//------------------------- update fields ----------------------------//

         // public.
         // to be called by a windowbox.
         // the additional update is done here.

void WboxAllbox::updateFields(WboxBox *active_box, const char *endkey,
                              int irow, int icol, int irow2, int icol2)
{
  assert(active_box);
  _trap_box_number = active_box->getIbox();
  doAdditionalUpdate();
  privateUpdate(active_box, endkey, irow, icol, irow2, icol2);
  _trap_box_number = 0;
}



//---------------------- private update ------------------------------//
//---------------------- private update ------------------------------//
//---------------------- private update ------------------------------//

         // private.

void WboxAllbox::privateUpdate(WboxBox *active_box, const char *endkey,
                              int irow, int icol, int irow2, int icol2)
{
  updateArrayVisibilities();
  updateSensitivities();

  if(_debug >= 2)
      {
      const char *nobox   = "-----";
      const char *boxname = nobox;
      int         ibox    = 0;
      if(active_box) ibox    = active_box->getIbox   ();
      if(active_box) boxname = active_box->getBoxName();
      printf("%6d %-10s %-8s updating fields\n", ibox, boxname, endkey);
      }

  for(int ibox = 1; ibox <= _totnumbox; ibox++)
      {
      WboxBox *box = getBoxPointer(ibox);
      if(box && box->isManaged())
          {
          if(box == active_box)
              {
              box->eraseFields();
              }
          else
              {
              if(!strcmp(endkey, "SCROLL")) continue;
              if(!strcmp(endkey, "EXPOSE")) continue;
              }
          box->updateFields(endkey, irow, icol, irow2, icol2);
          }
      }

           //// do not update scrollbars if endkey == SCROLL:
  if(strcmp(endkey, "SCROLL")) updateScrollbars();
}



//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//

