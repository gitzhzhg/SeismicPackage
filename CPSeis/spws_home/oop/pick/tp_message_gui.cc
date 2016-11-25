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

//------------------------ tp_message_gui.cc --------------------------//
//------------------------ tp_message_gui.cc --------------------------//
//------------------------ tp_message_gui.cc --------------------------//

//         implementation file for the TpMessageGui class
//                 derived from the SLpLabel class
//                        subdirectory pick

      // Displays a two-line message.
      // The message string should contain a \n within it.

      // If the public methods are called with a NULL object,
      // the methods simply return without aborting.  This
      // makes it possible to avoid creating an object if
      // it is not needed, and not have to check for a NULL
      // object before calling a method.

#include "pick/tp_message_gui.hh"
#include "pick/tp_resources.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>


enum { SHOW_BLANK, SHOW_MESSAGE, SHOW_ERROR };


//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//

TpMessageGui::TpMessageGui(SLDelay *slparent, char *name)
                  : SLpLabel(slparent, name),
                        _show  (SHOW_BLANK)
{
  TpResources::startup(this);
  showBlank();
}


TpMessageGui::~TpMessageGui()
{
}



//----------- show blank or message or error ------------------//
//----------- show blank or message or error ------------------//
//----------- show blank or message or error ------------------//


void TpMessageGui::showBlank()
{
  if(!this) return;
  setColor(SHOW_BLANK);
  setLabel("xx\nxx");
}



void TpMessageGui::showMessage(const char *msg)
{
  if(!this) return;
  setColor(SHOW_MESSAGE);
  setLabel((char*)msg);
}



void TpMessageGui::showError(const char *msg)
{
  if(!this) return;
  setColor(SHOW_ERROR);
  setLabel((char*)msg);
}


//------------------ private set color -------------------//
//------------------ private set color -------------------//
//------------------ private set color -------------------//

void TpMessageGui::setColor(int show)
{
  _show = show;
  if(topWidget())
       {
       Pixel pixel;
       switch(_show)
            {
            case SHOW_BLANK:
                  XtVaGetValues(topWidget(), XmNbackground, &pixel, NULL);
                  break;
            case SHOW_MESSAGE:
                  pixel = TpResources::getPixelWork();
                  break;
            case SHOW_ERROR:
                  pixel = TpResources::getPixelError();
                  break;
            default:
                  return;
            }
       XtVaSetValues(topWidget(), XmNforeground,  pixel, NULL);
       }
}


//------------------ private message helpers -----------------//
//------------------ private message helpers -----------------//
//------------------ private message helpers -----------------//


int TpMessageGui::getPickModePrefix(long pickmode, char *msg)
{
  switch(pickmode)
     {
     case PEAK    : strcpy(msg, "peak "  ); break;
     case TROUGH  : strcpy(msg, "trough "); break;
     case POSITIVE: strcpy(msg, "positive zero crossing "); break;
     case NEGATIVE: strcpy(msg, "negative zero crossing "); break;
     default:
            showError(
       "picking action attempted\n with unknown picking mode");
            return TRUE;
     }
  return FALSE;
}


int TpMessageGui::addAutoModePortion(long automode, char *msg)
{
  switch(automode)
     {
     case FOLLOW_LINE:
            strcat(msg, "AUTO picking (follow line)\n");
            break;
     case FOLLOW_SLOPE:
            strcat(msg, "AUTO picking (follow slope)\n");
            break;
     case FOLLOW_CURVE:
            strcat(msg, "AUTO picking (follow curve)\n");
            break;
     case FIRST_BREAK:
            strcat(msg, "AUTO first break picking\n");
            break;
     case FIRST_BREAK_NO_SNAP:
            strcpy(msg, "no-snap AUTO first break picking\n");
            break;
     case FIRST_BREAK_CORR:
            strcpy(msg, "AUTO first break picking with correlation\n");
            break;
     case HURST_BREAK:
            strcat(msg, "AUTO Hurst 1st break picking\n");
            break;
     case HURST_BREAK_NO_SNAP:
            strcpy(msg, "no-snap AUTO Hurst 1st break picking\n");
            break;
     case HURST_CORR:
            strcpy(msg, "AUTO Hurst 1st break picking with correlation\n");
            break;
     case COMBO:
            strcpy(msg, "AUTO combo 1st break picking\n");
            break;
     case COMBO_CORR:
            strcpy(msg, "AUTO combo 1st break picking with correlation\n");
            break;
     case PICK_CORR:
            strcpy(msg, "AUTO picking with correlation\n");
            break;
     case CORRELATE:
            strcpy(msg, "AUTO correlate from non-zero picks\n");
            break;
     default:
            showError(
       "AUTO picking action attempted\n with unknown autopick method");
            return TRUE;
     }
  return FALSE;
}


int TpMessageGui::addSnapPortion(long snap, char *msg)
{
  switch(snap)
     {
     case TP_CURR:
            strcat(msg, "SNAP to current picks\n");
            break;
     case TP_ORIG:
            strcat(msg, "SNAP to original picks\n");
            break;
     case TP_PREV:
            strcat(msg, "SNAP to previous profile\n");
            break;
     case TP_NEXT:
            strcat(msg, "SNAP to next profile\n");
            break;
     case TP_SEL:
            strcat(msg, "SNAP to selected profile\n");
            break;
     case TP_NONE:
            showError(
       "SNAP picking action attempted\n with nothing to snap to");
            return TRUE;
     default:
            showError(
       "SNAP picking action attempted\n with unknown snap choice");
            return TRUE;
     }
  return FALSE;
}



//-------------------- show action message -----------------------//
//-------------------- show action message -----------------------//
//-------------------- show action message -----------------------//

              // calls showMessage or showError

void TpMessageGui::showActionMessage(long action,
               long pickmode, long automode, long snap, char *post)
{
  if(!this) return;
  char msg[100];
  int error;
  switch(action)
     {
     case ZERO:
            strcpy(msg, "DELETE picking action\n");
            break;
     case MANUAL:
            strcpy(msg, "MANUAL picking action\n");
            break;
     case AUTOMATIC:
            error = getPickModePrefix (pickmode, msg); if(error) return;
            error = addAutoModePortion(automode, msg); if(error) return;
            break;
     case SNAP:
            error = getPickModePrefix (pickmode, msg); if(error) return;
            error = addSnapPortion    (snap    , msg); if(error) return;
            break;
     default:
            showError("unknown picking action\n attempted");
            return;
     }
  strcat(msg, post);
  showMessage(msg);
}

                                                                                
//---------------------------- make ---------------------------//
//---------------------------- make ---------------------------//
//---------------------------- make ---------------------------//

static String defres[]= {
    "*fontList: 8x13bold",
    NULL };


Widget TpMessageGui::make(Widget p)
{
  if(!made())
     {
     setDefaultResources(TpResources::getDisplay(), instanceName(), defres);
     Widget w = SLpLabel::make(p);
     setColor(_show);
     }
  return topWidget();
}


//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
