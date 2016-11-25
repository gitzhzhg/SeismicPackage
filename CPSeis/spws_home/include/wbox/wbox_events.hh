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

//----------------------- wbox_events.hh -------------------------//
//----------------------- wbox_events.hh -------------------------//
//----------------------- wbox_events.hh -------------------------//

//              header file for the WboxEvents class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_EVENTS_HH_
#define _WBOX_EVENTS_HH_

#include <X11/Intrinsic.h>
#include <Xm/DrawingA.h>


class WboxEvents
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class WboxBox  *_box;


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  WboxEvents (WboxBox *box);

  virtual ~WboxEvents();

  void  sendClientMessageEvent (const char *endkey)  const;
  void  sendImmediateEvent     (const char *endkey);

private:

  void  informKeypressEvent  (const char *string, int state);

  void  informButtonEvent    (const char *endkey,
                              int irow, int icol, int ibutton);

  void  informMotionEvent    (int irow, int icol);

  void  informExposeEvent    (int irow, int icol, int irow2, int icol2,
                              int count);

  void  informConfigureEvent (int irow, int icol, int irow2, int icol2);

  void  informGenericEvent   (const char *endkey);

  static void eventHandlerShell  (Widget, WboxEvents *events, XEvent *event);
  static void eventHandlerTiny   (Widget, WboxEvents *events, XEvent *event);
  static void eventHandler       (Widget, WboxEvents *events, XEvent *event);
  static void destroyBoxCallback (Widget, WboxEvents *events,
                                               XmDrawingAreaCallbackStruct*);

  int   specialAction (const char *endkey);

  void  callBoxtrap   (const char *charr, const char *endkey,
                       int irow, int icol, int irow2, int icol2);

  void  getCharrAndEndkey  (const char *string,
                            char *charr, char *endkey);

  int   addModifier   (int state, char *charr, char *endkey);

  void  adjustEndkey  (char *endkey);



//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


