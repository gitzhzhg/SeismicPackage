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

//---------------------- wbox_events.cc -------------------------//
//---------------------- wbox_events.cc -------------------------//
//---------------------- wbox_events.cc -------------------------//

//          implementation file for the WboxEvents class
//                  not derived from any class
//                       subdirectory wbox


       // This class contains callbacks and event handlers.
       // In return, this class call the BOXTRAP routine.


#include "wbox/wbox_events.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_screen.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>


//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//

       // the constructor registers event handlers.

WboxEvents::WboxEvents(WboxBox *box)
         :
            _box   (box)
{
  assert(_box);
  Widget w          = _box->getW();
  Widget shellchild = _box->getShellChild();
  Widget tiny       = _box->getTinyPushbutton();
  assert(w && shellchild && tiny);

  XtAddEventHandler(w, ButtonPressMask     | ExposureMask      |
                       StructureNotifyMask | ButtonReleaseMask |
                       ButtonMotionMask    | PointerMotionMask, FALSE,
                       (XtEventHandler)eventHandler, this);

  XtAddEventHandler(w, NoEventMask, TRUE,
                      (XtEventHandler)eventHandler, this);

  XtAddCallback    (w, XmNdestroyCallback,
                      (XtCallbackProc)destroyBoxCallback, this);

  if(shellchild != NULL)
       {
       XtAddEventHandler(shellchild, StructureNotifyMask, FALSE,
                      (XtEventHandler)eventHandlerShell, this);
       }

  XtAddEventHandler(tiny, KeyPressMask | FocusChangeMask, FALSE,
                      (XtEventHandler)eventHandlerTiny, this);
}



//----------------------- destructor -----------------------------//
//----------------------- destructor -----------------------------//
//----------------------- destructor -----------------------------//


WboxEvents::~WboxEvents()
{
}



//------------------ send client message event ---------------------//
//------------------ send client message event ---------------------//
//------------------ send client message event ---------------------//

      // public.
      // sends a client message event to this windowbox.

void WboxEvents::sendClientMessageEvent(const char *endkey)  const
{
  XClientMessageEvent event;
  event.type         = ClientMessage;
  event.serial       = 0;
  event.send_event   = TRUE;
  event.display      = _box->getScreenPointer()->getDisplay();
  event.window       = _box->getWindow();
  event.message_type = (Atom)0;
  event.format       = 32;
  strncpy(event.data.b, endkey, 8);
  event.data.b[8] = '\0';
  XSendEvent(event.display, event.window, FALSE, NoEventMask,
                                                     (XEvent*)(&event));
}



//-------------------- send immediate event -------------------------//
//-------------------- send immediate event -------------------------//
//-------------------- send immediate event -------------------------//

      // public.
      // sends an (unreal) event to this windowbox immediately,
      //   without going thru the server.
      // should not be called from a trap or update function.

void WboxEvents::sendImmediateEvent(const char *endkey)
{
  informGenericEvent(endkey);
}



//------------------ event handler shell ------------------------//
//------------------ event handler shell ------------------------//
//------------------ event handler shell ------------------------//

     // private static function.

void WboxEvents::eventHandlerShell(Widget, WboxEvents *events,
                                                       XEvent *event)
{
  if( event->type == UnmapNotify )
     {
     events->informGenericEvent("UNMAP");
     }
}



//---------------------- event handler tiny ----------------------------//
//---------------------- event handler tiny ----------------------------//
//---------------------- event handler tiny ----------------------------//

     // private static function.

void WboxEvents::eventHandlerTiny(Widget, WboxEvents *events, XEvent *event)
{
  if( event->type == KeyPress )
     {
     KeySym keysym;
     char   str[24];
     int    nby=24;
     char  *string;
     XLookupString((XKeyEvent*)event, str, nby, &keysym, NULL);
     string = XKeysymToString(keysym);
     if(string) events->informKeypressEvent(string   , event->xkey.state);
     else       events->informKeypressEvent("unknown", event->xkey.state);
     }
  else if( event->type == FocusIn )
     {
     events->informGenericEvent("FOCUSIN");
     }
  else if( event->type == FocusOut )
     {
     events->informGenericEvent("FOCUSOUT");
     }
}



//---------------------- event handler ---------------------------------//
//---------------------- event handler ---------------------------------//
//---------------------- event handler ---------------------------------//

     // private static function.

void WboxEvents::eventHandler(Widget w, WboxEvents *events, XEvent *event)
{
  WboxBox    *box    = events->_box;
  WboxScreen *screen = box->getScreenPointer();
  WboxAllbox *allbox = box->getAllboxPointer();

  if( event->type == ButtonPress )
     {
     int irow = screen->getIrow(event->xbutton.y);
     int icol = screen->getIcol(event->xbutton.x);
     events->informButtonEvent("BUTTON", irow, icol, event->xbutton.button);
     box->gainFocus();
     }
  else if( event->type == ButtonRelease )
     {
     int irow = screen->getIrow(event->xbutton.y);
     int icol = screen->getIcol(event->xbutton.x);
     events->informButtonEvent("RELEASE", irow, icol, event->xbutton.button);
     }
  else if( event->type == MotionNotify )
     {
     int irow = screen->getIrow(event->xmotion.y);
     int icol = screen->getIcol(event->xmotion.x);
     events->informMotionEvent(irow, icol);
     }
  else if( event->type == Expose )
     {
     if(allbox->getDebug() >= 3)
         printf("wbox: %d x/y=%d %d  width/height=%d %d\n",
               box->getIbox(), event->xexpose.x, event->xexpose.y,
               event->xexpose.width, event->xexpose.height);
     int irow  = screen->getIrow(event->xexpose.y);
     int icol  = screen->getIcol(event->xexpose.x);
     int irow2 = screen->getIrow(event->xexpose.y + event->xexpose.height);
     int icol2 = screen->getIcol(event->xexpose.x + event->xexpose.width);
     events->informExposeEvent(irow, icol, irow2, icol2, event->xexpose.count);
     }
  else if( event->type == ConfigureNotify )
     {

     int irow  = 0;
     int icol  = 0;
     int irow2 = screen->getIrow(event->xconfigure.height);
     int icol2 = screen->getIcol(event->xconfigure.width);
     events->informConfigureEvent(irow, icol, irow2, icol2);
     }
  else if( event->type == ClientMessage )
     {
     XClientMessageEvent *event2 = (XClientMessageEvent*)event;
     if(event2->data.b[0] != '\0' && event2->data.b[8] == '\0')
                     events->informGenericEvent(event2->data.b);
     else
                     events->informGenericEvent("CLIENT");
     }
}



//------------------ destroy box callback ------------------------//
//------------------ destroy box callback ------------------------//
//------------------ destroy box callback ------------------------//

     // private static function.

void WboxEvents::destroyBoxCallback(Widget, WboxEvents *events,
                   XmDrawingAreaCallbackStruct*)
{
  events->informGenericEvent("DESTROY");
  WboxBox    *box    = events->_box;
  WboxAllbox *allbox = box->getAllboxPointer();
  if(allbox->getDebug() >= 1) printf("wbox: %d %s destroyed\n",
                                      box->getIbox(), box->getBoxName());
  allbox->informWindowboxDestroyed(box);
}



//------------------------ special action ------------------------//
//------------------------ special action ------------------------//
//------------------------ special action ------------------------//

  // private.
  // takes special action for certain values of endkey.
  // returns TRUE if special action was taken and boxtrap is not to be called.
  // returns FALSE otherwise.

int WboxEvents::specialAction(const char *endkey)
{
  WboxAllbox *allbox = _box->getAllboxPointer();
  if     (!strcmp(endkey, "IGNORE")) return TRUE;
  else if(!strcmp(endkey, "^0"    )) allbox->setDebug(0);
  else if(!strcmp(endkey, "^1"    )) allbox->setDebug(1);
  else if(!strcmp(endkey, "^2"    )) allbox->setDebug(2);
  else if(!strcmp(endkey, "^3"    )) allbox->setDebug(3);
  else if(!strcmp(endkey, "^S"    )) allbox->toggleKeymode();
  else                               return FALSE;
  return TRUE;
}



//----------------------- call boxtrap -----------------------//
//----------------------- call boxtrap -----------------------//
//----------------------- call boxtrap -----------------------//

        // private.

void WboxEvents::callBoxtrap(const char *charr, const char *endkey,
                             int irow, int icol, int irow2, int icol2)
{
  assert(strlen(charr ) == 1);
  assert(strlen(endkey) >= 1);
  assert(strlen(endkey) <= 8);
  if(specialAction(endkey)) return;
  _box->boxtrap(charr, endkey, irow, icol, irow2, icol2);
}



//------------------------ get charr and endkey ----------------------//
//------------------------ get charr and endkey ----------------------//
//------------------------ get charr and endkey ----------------------//

  // private.
  // convert Xlib string to character and endkey.
  // if   printable   key is pressed, charr = non-blank and endkey = blank.
  // if non-printable key is pressed, charr = blank and endkey = non-blank.

#define DOIT1(c1,c2)  if(!strcmp(string,c1)) { strcpy(charr ,c2); return; }
#define DOIT2(c1,c2)  if(!strcmp(string,c1)) { strcpy(endkey,c2); return; }

void WboxEvents::getCharrAndEndkey(const char *string,
                                   char *charr, char *endkey)
{
  int length = strlen(string);
  strcpy(charr , " ");
  strcpy(endkey, " ");
  if(length == 0)
      {
      return;               // probably should not happen.
      }
  if(length == 1)
      {
      charr[0] = string[0];
      return;
      }
  DOIT1("space"        ,  " "   );
  DOIT1("KP_0"         ,  "0"   );
  DOIT1("KP_1"         ,  "1"   );
  DOIT1("KP_2"         ,  "2"   );
  DOIT1("KP_3"         ,  "3"   );
  DOIT1("KP_4"         ,  "4"   );
  DOIT1("KP_5"         ,  "5"   );
  DOIT1("KP_6"         ,  "6"   );
  DOIT1("KP_7"         ,  "7"   );
  DOIT1("KP_8"         ,  "8"   );
  DOIT1("KP_9"         ,  "9"   );
  DOIT1("KP_Decimal"   ,  "."   );
  DOIT1("KP_Subtract"  ,  "-"   );
  DOIT1("KP_Separator" ,  ","   );
  DOIT1("asciitilde"   ,  "~"   );
  DOIT1("grave"        ,  "`"   );
  DOIT1("exclam"       ,  "!"   );
  DOIT1("at"           ,  "@"   );
  DOIT1("numbersign"   ,  "#"   );
  DOIT1("dollar"       ,  "$"   );
  DOIT1("percent"      ,  "%"   );
  DOIT1("asciicircum"  ,  "^"   );
  DOIT1("ampersand"    ,  "&"   );
  DOIT1("asterisk"     ,  "*"   );
  DOIT1("parenleft"    ,  "("   );
  DOIT1("parenright"   ,  ")"   );
  DOIT1("underscore"   ,  "_"   );
  DOIT1("minus"        ,  "-"   );
  DOIT1("plus"         ,  "+"   );
  DOIT1("equal"        ,  "="   );
  DOIT1("braceleft"    ,  "{"   );
  DOIT1("bracketleft"  ,  "["   );
  DOIT1("braceright"   ,  "}"   );
  DOIT1("bracketright" ,  "]"   );
  DOIT1("colon"        ,  ":"   );
  DOIT1("semicolon"    ,  ";"   );
  DOIT1("quotedbl"     ,  "\""  );
  DOIT1("apostrophe"   ,  "'"   );
  DOIT1("bar"          ,  "|"   );
  DOIT1("backslash"    ,  "\\"  );
  DOIT1("question"     ,  "?"   );
  DOIT1("slash"        ,  "/"   );
  DOIT1("comma"        ,  ","   );
  DOIT1("period"       ,  "."   );
  DOIT1("less"         ,  "<"   );
  DOIT1("greater"      ,  ">"   );
  DOIT2("KP_F1"        ,  "HELP"    );
  DOIT2("KP_F2"        ,  "PF2"     );
  DOIT2("KP_F3"        ,  "PF3"     );
  DOIT2("KP_F4"        ,  "PF4"     );
  DOIT2("Help"         ,  "HELP"    );
  DOIT2("F1"           ,  "HELP"    );
  DOIT2("F3"           ,  "IGNORE"  );   // F3 (invokes terminal setup).
  DOIT2("F20"          ,  "JUMP"    );
  DOIT2("Delete"       ,  "DELETE"  );
  DOIT2("BackSpace"    ,  "DELETE"  );   // needed for HP.
  DOIT2("Return"       ,  "RETURN"  );
  DOIT2("Select"       ,  "RETURN"  );
  DOIT2("Find"         ,  "FIND"    );
  DOIT2("KP_Enter"     ,  "ENTER"   );
  DOIT2("Tab"          ,  "TAB"     );
  DOIT2("Menu"         ,  "DO"      );
  DOIT2("Insert"       ,  "INSERT"  );
  DOIT2("_Remove"      ,  "REMOVE"  );
  DOIT2("DRemove"      ,  "REMOVE"  );
  DOIT2("Dead_Grave"   ,  "REMOVE"  );   // needed for sun.
  DOIT2("apLineDel"    ,  "REMOVE"  );   // needed for SGI.
  DOIT2("Prior"        ,  "PREV"    );
  DOIT2("Next"         ,  "NEXT"    );
  DOIT2("Up"           ,  "UP"      );
  DOIT2("Down"         ,  "DOWN"    );
  DOIT2("Left"         ,  "LEFT"    );
  DOIT2("Right"        ,  "RIGHT"   );
  DOIT2("Hyper_R"      ,  "IGNORE"  );   // F3 (invokes terminal setup).
  DOIT2("Control_L"    ,  "IGNORE"  );   // modifier key.
  DOIT2("Control_R"    ,  "IGNORE"  );   // modifier key.
  DOIT2("Caps_Lock"    ,  "IGNORE"  );   // modifier key.
  DOIT2("Shift_L"      ,  "IGNORE"  );   // modifier key.
  DOIT2("Shift_R"      ,  "IGNORE"  );   // modifier key.
  DOIT2("Alt_L"        ,  "IGNORE"  );   // modifier key.
  DOIT2("Alt_R"        ,  "IGNORE"  );   // modifier key.
  DOIT2("Multi_key"    ,  "IGNORE"  );   // modifier key.
  strncpy(endkey, string, 8);
  endkey[8] = '\0';
}



//-------------------------- add modifier -----------------------------//
//-------------------------- add modifier -----------------------------//
//-------------------------- add modifier -----------------------------//

  //  private.

  //  state = sum of the following integers:
  //                0 = no modifier keys pressed.
  //                1 = "shift"     key pressed.   Shift_L    Shift_R
  //                2 = "caps lock" key pressed.   Caps_Lock
  //                4 = "control"   key pressed.   Control_L  Control_R
  //  Tektronix->   8 = "compose"   key pressed.   Alt_L      Alt_R
  //        DEC->   8 = "compose"   key pressed.   Multi_key
  //        DEC->  16 = "alt"       key pressed.   Alt_L      Alt_R
  //
  //  k = state+1     if k is out of range, ENDKEY prefix is < or >.
  //
  //  a[k] = ENDKEY prefix when modifier pressed (for printable key).
  //       = ^ when control key is pressed.
  //       = @ when alt     key is pressed.
  //       = & when both control and alt keys are pressed.
  //       (shift key and cap lock key are irrelevant)
  //
  //  b[k] = ENDKEY prefix when modifier pressed (for non-printable key).
  //       = $ when shift   key is pressed.
  //       = ^ when control key is pressed.
  //       = @ when alt     key is pressed.
  //       = # when both shift and control keys are pressed.
  //       = % when both shift and alt keys are pressed.
  //       = & when both control and alt keys are pressed.
  //       = * when shift and control and alt keys are all pressed.
  //       (cap lock key is irrelevant)
  //
  //  wherever the alt key is mentioned above, it refers to the compose
  //        key or alt key or both.

        //                   1111111111222222222233    <-- state
        //         01234567890123456789012345678901    <-- state
        //         --------------------------------
static char *a = "<    ^^^^@@@@&&&&@@@@&&&&@@@@&&&&>";
static char *b = "< $ $^#^#@%@%&*&*@%@%&*&*@%@%&*&*>";
        //         --------------------------------
static char *a1= "<-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S>";
static char *a2= "<--LL--LL--LL--LL--LL--LL--LL--LL>";
static char *a3= "<----^^^^----^^^^----^^^^----^^^^>";
static char *a4= "<--------CCCCCCCC--------CCCCCCCC>";
static char *a5= "<----------------AAAAAAAAAAAAAAAA>";
        //         --------------------------------
        //          1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1      "shift"
        //           22  22  22  22  22  22  22  22      "caps lock"
        //             4444    4444    4444    4444      "control"
        //                 88888888        88888888      "compose"
        //                         FFFFFFFFFFFFFFFF      "alt"


int WboxEvents::addModifier(int state, char *charr, char *endkey)
{
  int k = state + 1;
  int n = strlen(a);
  if(k < 0)   k = 0;
  if(k > n-1) k = n-1;
  if(state >= 8)                      // do not deal with ALT key.
      {
      strcpy(endkey, "IGNORE");
      charr [0] = ' ';
      }
  if(!strcmp(endkey, "IGNORE")) return k;
  if(!strcmp(endkey, " ") && a[k] != ' ')
      {
      endkey[0] = a[k];
      endkey[1] = toupper(charr[0]);
      endkey[2] = '\0';
      charr [0] = ' ';
      }
  else if(strcmp(endkey, " ") && b[k] != ' ')
      {
      char temporary[10];
      int k2 = k;
      if(k == 6 || k == 8) k2 = 9;    // make SHIFT+CTRL look like ALT.
      strncpy(temporary, endkey, 7);
      temporary[7] = '\0';
      endkey[0] = b[k2];
      strcpy(&endkey[1], temporary);
      }
  return k;
}



//----------------------------- adjust endkey ------------------------//
//----------------------------- adjust endkey ------------------------//
//----------------------------- adjust endkey ------------------------//

         // private.

void WboxEvents::adjustEndkey(char *endkey)
{
  int keymode = _box->getAllboxPointer()->getKeymode();
  static char direction[10] = "RIGHT";
  if     (!strcmp(endkey, "^I"   )) strcpy(endkey, "INSERT");
  else if(!strcmp(endkey, "^R"   )) strcpy(endkey, "REMOVE");
  else if(!strcmp(endkey, "^F"   )) strcpy(endkey, "FIND");
  else if(!strcmp(endkey, "^D"   )) strcpy(endkey, "DO");
  else if(!strcmp(endkey, "^H"   )) strcpy(endkey, "HARDCOPY");
  else if(!strcmp(endkey, "F12"  )) strcpy(endkey, "DO");
  else if(!strcmp(endkey, "F16"  )) strcpy(endkey, "DO");
  else if(!strcmp(endkey, "PREV" )) strcpy(endkey, "^UP");
  else if(!strcmp(endkey, "NEXT" )) strcpy(endkey, "^DOWN");
  else if(!strcmp(endkey, "^T"   )) strcpy(endkey, "JUMP");
  else if(!strcmp(endkey, "$HELP")) strcpy(endkey, "OVERVIEW");
  else if(!strcmp(endkey, "^HELP")) strcpy(endkey, "KEYHELP");
  else if(!strcmp(endkey, "ENTER")) strcpy(endkey, direction);
  else if(!strcmp(endkey, "UP"   )) strcpy(direction, endkey);
  else if(!strcmp(endkey, "DOWN" )) strcpy(direction, endkey);
  else if(!strcmp(endkey, "LEFT" )) strcpy(direction, endkey);
  else if(!strcmp(endkey, "RIGHT")) strcpy(direction, endkey);
  if(keymode <= 1)
      {
      if(!strcmp(endkey, "TAB"  )) strcpy(endkey, "JUMP"  );
      if(!strcmp(endkey, "$TAB" )) strcpy(endkey, "$JUMP" );
      }
  else if(keymode == 2)
      {
      if(!strcmp(endkey, "LEFT" )) strcpy(endkey, "$LEFT" );
      if(!strcmp(endkey, "RIGHT")) strcpy(endkey, "$RIGHT");
      if(!strcmp(endkey, "TAB"  )) strcpy(endkey, "RIGHT" );
      if(!strcmp(endkey, "$TAB" )) strcpy(endkey, "LEFT"  );
      }
}



//------------------ inform keypress event -------------------//
//------------------ inform keypress event -------------------//
//------------------ inform keypress event -------------------//

         // private.

void WboxEvents::informKeypressEvent(const char *string, int state)
{
  char charr[2];
  char endkey[10];
  int  k;
  int  debug = _box->getAllboxPointer()->getDebug();
  getCharrAndEndkey (string, charr, endkey);
  k = addModifier   (state,  charr, endkey);
  adjustEndkey                     (endkey);
  if(debug >= 1)
      {
      if(debug >= 2) printf("----------\n");
      printf("Box %2d %-10s %-8s %1s state= %2d %c%c%c%c%c string= %s\n",
         _box->getIbox(), _box->getBoxName(), endkey, charr,
         state, a1[k], a2[k], a3[k], a4[k], a5[k], string);
      }
  callBoxtrap(charr, endkey, 0, 0, 0, 0);
}



//------------------ inform button event -------------------//
//------------------ inform button event -------------------//
//------------------ inform button event -------------------//
//------------------ inform button event -------------------//

         // private.

void WboxEvents::informButtonEvent(const char *endkey2,
                          int irow, int icol, int ibutton)
{
  char  endkey[10];
  int   debug = _box->getAllboxPointer()->getDebug();
  strncpy(endkey, endkey2, 7);
  endkey[7] = '\0';
  if(ibutton == 2) strcat(endkey, "2");
  if(ibutton == 3) strcat(endkey, "3");
  if(debug >= 1)
      {
      if(debug >= 2) printf("----------\n");
      printf
   ("Box %2d %-10s %-8s row/col= %3d %3d  button= %2d\n",
         _box->getIbox(), _box->getBoxName(),
         endkey, irow, icol, ibutton);
      }
  callBoxtrap(" ", endkey, irow, icol, 0, 0);
}



//------------------ inform motion event -------------------//
//------------------ inform motion event -------------------//
//------------------ inform motion event -------------------//

         // private.

void WboxEvents::informMotionEvent(int irow, int icol)
{
  char *endkey = "MOTION";
  int    debug = _box->getAllboxPointer()->getDebug();
  if(debug >= 3)
      {
      printf ("Box %2d %-10s %-8s row/col= %3d %3d\n",
               _box->getIbox(), _box->getBoxName(),
               endkey, irow, icol);
      }
  callBoxtrap(" ", endkey, irow, icol, 0, 0);
}



//------------------ inform expose event -------------------//
//------------------ inform expose event -------------------//
//------------------ inform expose event -------------------//

         // private.

void WboxEvents::informExposeEvent(int irow, int icol,
                                   int irow2, int icol2, int count)
{
  static int regionflag = 0;
  static int regionr, regionc, regionr2, regionc2;
  char *endkey = "EXPOSE";
  int   debug  = _box->getAllboxPointer()->getDebug();
  if(debug >= 2)
      {
 printf("Box %2d %-10s %-8s row/col= %3d %3d row/col= %3d %3d, count= %2d\n",
         _box->getIbox(), _box->getBoxName(),
         endkey, irow, icol, irow2, icol2, count);
      }
  if(regionflag == 0)
      {
      regionr  = irow;
      regionc  = icol;
      regionr2 = irow2;
      regionc2 = icol2;
      regionflag = 1;
      }
  else
      {
      if(irow  < regionr ) regionr  = irow;
      if(icol  < regionc ) regionc  = icol;
      if(irow2 > regionr2) regionr2 = irow2;
      if(icol2 > regionc2) regionc2 = icol2;
      }
  if(count > 0) return;
  regionflag = 0;
  if(debug >= 1)
      {
      if(debug >= 2) printf("----------\n");
      printf("Box %2d %-10s %-8s row/col= %3d %3d row/col= %3d %3d\n",
         _box->getIbox(), _box->getBoxName(),
         endkey, regionr, regionc, regionr2, regionc2);
      }
  callBoxtrap(" ", endkey, regionr, regionc, regionr2, regionc2);
}



//------------------ inform configure event -------------------//
//------------------ inform configure event -------------------//
//------------------ inform configure event -------------------//

         // private.

void WboxEvents::informConfigureEvent(int irow, int icol,
                                      int irow2, int icol2)
{
  char *endkey = "CONFIGUR";
  int   debug  = _box->getAllboxPointer()->getDebug();
  if(debug >= 1)
      {
      if(debug >= 2) printf("----------\n");
      printf("Box %2d %-10s %-8s row/col= %3d %3d row/col= %3d %3d\n",
         _box->getIbox(), _box->getBoxName(),
         endkey, irow, icol, irow2, icol2);
      }
  callBoxtrap(" ", endkey, irow, icol, irow2, icol2);
/*****
//////// this is intended to optimize the height slightly to match nrow better:
//////// new 3/07/2002 below (does not work):
  sendClientMessageEvent("reg-ht");
//////// new 3/07/2002 above (does not work).
*****/
}



//------------------ inform generic event -----------------------//
//------------------ inform generic event -----------------------//
//------------------ inform generic event -----------------------//

         // private.

void WboxEvents::informGenericEvent(const char *endkey)
{
  int ibox  = _box->getAllboxPointer()->getTrapBoxNumber();
  int debug = _box->getAllboxPointer()->getDebug();
  if(ibox > 0) return;
  if(debug >= 1)
      {
      if(debug >= 2) printf("----------\n");
      printf("Box %2d %-10s %-8s\n",
                            _box->getIbox(), _box->getBoxName(), endkey);
      }
/*****
//////// this is intended to optimize the height slightly to match nrow better:
//////// new 3/07/2002 below (does not work):
  if(strcmp(endkey, "reg-ht") == 0)
      {
      Widget      w      = _box->getW();
      WboxScreen *screen = _box->getScreenPointer();
      int         nrow   = _box->getNrow();
      Dimension   height = (nrow + 1) * screen->getCelly() - 1;
      if(nrow == 0) height = 1;
      Arg args[3];
      int i=0;
      XtSetArg (args[i], XmNheight, height); i++;
      XtSetValues(w, args, i);
      return;
      }
//////// new 3/07/2002 above (does not work).
*****/
  callBoxtrap(" ", endkey, 0, 0, 0, 0);
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

