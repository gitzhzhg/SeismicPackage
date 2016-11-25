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

//------------------------ custom_resources.cc -----------------------//
//------------------------ custom_resources.cc -----------------------//
//------------------------ custom_resources.cc -----------------------//

//         implementation file for the CustomResources class
//                  not derived from any class
//                       subdirectory pick


//    The purpose of this class is to provide a simple mechanism
//    for getting names and pixels of commonly-used colors
//    which can be modified in an app-defaults file if desired.
//    A few additional general-purpose capabilities are also
//    included.

//    The immediate purpose is to supply pixels which can be
//    used to set the foreground or background of a widget,
//    or to supply names of colors which can be supplied to
//    a vector class for drawing lines.

//    It is the intention to add to this class when additional
//    custom resouces are needed which are likely to have general use.

//    Although there are a public constructor and a public
//    destructor, no instance of this class need ever be created.
//    All information resides in non-member static variables, and
//    is accessed through static member functions.

//    Either one of the following two functions must be called at
//    least once by the application, before calling any of the functions
//    to get any specific resource.  Any call after the first call has
//    no effect.
//      CustomResources::startup(SLDelay *gui)  // using any existing gui
//      CustomResources::startup(Widget     w)  // using any existing w


#include "pick/custom_resources.hh"
#include "sl/sl_delay.hh"
#include "cprim.h"
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <assert.h>


//-------------------- custom resource structure -----------------//
//-------------------- custom resource structure -----------------//
//-------------------- custom resource structure -----------------//

struct Custom
  {
  int     line_width;        // width of lines
  char   *color_rubber;      // name of color of rubberband
  char   *cursor_name;       // name of font cursor for picking

  Pixel   pixel_red;         // pixel of color (default red)
  Pixel   pixel_orange;      // pixel of color (default orange)
  Pixel   pixel_yellow;      // pixel of color (default yellow)
  Pixel   pixel_green;       // pixel of color (default green)
  Pixel   pixel_blue;        // pixel of color (default blue)
  Pixel   pixel_brown;       // pixel of color (default brown)

  char   *color_red;         // name of color (default red)
  char   *color_orange;      // name of color (default orange)
  char   *color_yellow;      // name of color (default yellow)
  char   *color_green;       // name of color (default green)
  char   *color_blue;        // name of color (default blue)
  char   *color_brown;       // name of color (default brown)
  };


//------------------------ static variables ----------------------//
//------------------------ static variables ----------------------//
//------------------------ static variables ----------------------//

static Custom custom;
static Boolean starting = True;
static Widget any_widget = NULL;


//------------------- shorthand macros -----------------------------//
//------------------- shorthand macros -----------------------------//
//------------------- shorthand macros -----------------------------//


#define III XtRInt   , sizeof(int   )
#define PPP XtRPixel , sizeof(Pixel )
#define SSS XtRString, sizeof(String)
#define JJ  XtRImmediate
#define SS  XtRString
#define RRR(n,i,p,j,c) \
    { n, "CustomResources", i, XtOffsetOf(Custom,p), j, c },


//--------------- static resources array ------------------------//
//--------------- static resources array ------------------------//
//--------------- static resources array ------------------------//


static XtResource resources[] = {
    RRR("custom_line_width"  , III, line_width    , JJ, (XtPointer)3  )
    RRR("custom_color_rubber", SSS, color_rubber  , SS, (XtPointer)"orange")
    RRR("custom_cursor"      , SSS, cursor_name   , SS, (XtPointer)"center_ptr")

    RRR("custom_color_red"   , PPP, pixel_red     , SS, (XtPointer)"red")
    RRR("custom_color_orange", PPP, pixel_orange  , SS, (XtPointer)"orange")
    RRR("custom_color_yellow", PPP, pixel_yellow  , SS, (XtPointer)"yellow3")
    RRR("custom_color_green" , PPP, pixel_green   , SS, (XtPointer)"green4")
    RRR("custom_color_blue"  , PPP, pixel_blue    , SS, (XtPointer)"blue")
    RRR("custom_color_brown" , PPP, pixel_brown   , SS, (XtPointer)"brown")

    RRR("custom_color_red"   , SSS, color_red     , SS, (XtPointer)"red")
    RRR("custom_color_orange", SSS, color_orange  , SS, (XtPointer)"orange")
    RRR("custom_color_yellow", SSS, color_yellow  , SS, (XtPointer)"yellow3")
    RRR("custom_color_green" , SSS, color_green   , SS, (XtPointer)"green4")
    RRR("custom_color_blue"  , SSS, color_blue    , SS, (XtPointer)"blue")
    RRR("custom_color_brown" , SSS, color_brown   , SS, (XtPointer)"brown")
    };


//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//

      // SLDelay does not have to be made

static Widget get_any_widget(SLDelay *gui)
{
  assert(gui);
  if(gui->W      ()) return gui->W      ();
  if(gui->wParent()) return gui->wParent();
  return get_any_widget(gui->slParent());
}



//--------------------------- startup ---------------------------//
//--------------------------- startup ---------------------------//
//--------------------------- startup ---------------------------//


void CustomResources::startup(SLDelay *gui)
{
  if(starting)
       {
       Widget w = get_any_widget(gui);
       startup(w);
       }
}



void CustomResources::startup(Widget w)
{
  if(starting)
       {
       assert(w);
       XtGetApplicationResources(w, &custom, resources,
                           XtNumber(resources), NULL, 0);
       any_widget = w;
       starting = False;
       }
}



//------------------- get display or any widget -----------------//
//------------------- get display or any widget -----------------//
//------------------- get display or any widget -----------------//

Display *CustomResources::getDisplay()
{
  return XtDisplay(getAnyWidget());
}



Widget CustomResources::getAnyWidget()
{
  assert(!starting);
  return any_widget;
}



//------------------- get specified resource -----------------------//
//------------------- get specified resource -----------------------//
//------------------- get specified resource -----------------------//


int CustomResources::getLineWidth()
{
  assert(!starting);
  return custom.line_width;
}


const char *CustomResources::getColorRubber()
{
  assert(!starting);
  assert(custom.color_rubber);
  return custom.color_rubber;
}


Cursor CustomResources::getCursor()
{
  assert(!starting);
  assert(custom.cursor_name);
  if(strings_equal(custom.cursor_name, "tcross"    )) return XC_tcross;
  if(strings_equal(custom.cursor_name, "target"    )) return XC_target;
  if(strings_equal(custom.cursor_name, "dotbox"    )) return XC_dotbox;
  if(strings_equal(custom.cursor_name, "crosshair" )) return XC_crosshair;
  if(strings_equal(custom.cursor_name, "cross"     )) return XC_cross;
  if(strings_equal(custom.cursor_name, "plus"      )) return XC_plus;
  if(strings_equal(custom.cursor_name, "left_ptr"  )) return XC_left_ptr;
  if(strings_equal(custom.cursor_name, "center_ptr")) return XC_center_ptr;
  return XC_circle;
}


#define GET_COLOR(getColorNominalRed, color_red)    \
const char *CustomResources::getColorNominalRed()   \
{                                                   \
  assert(!starting);                                \
  assert(custom.color_red);                         \
  return custom.color_red;                          \
}

GET_COLOR(getColorNominalRed   , color_red   )
GET_COLOR(getColorNominalOrange, color_orange)
GET_COLOR(getColorNominalYellow, color_yellow)
GET_COLOR(getColorNominalGreen , color_green )
GET_COLOR(getColorNominalBlue  , color_blue  )
GET_COLOR(getColorNominalBrown , color_brown )



#define GET_PIXEL(getPixelNominalRed, pixel_red)    \
Pixel CustomResources::getPixelNominalRed()         \
{                                                   \
  assert(!starting);                                \
  return custom.pixel_red;                          \
}

GET_PIXEL(getPixelNominalRed   , pixel_red   )
GET_PIXEL(getPixelNominalOrange, pixel_orange)
GET_PIXEL(getPixelNominalYellow, pixel_yellow)
GET_PIXEL(getPixelNominalGreen , pixel_green )
GET_PIXEL(getPixelNominalBlue  , pixel_blue  )
GET_PIXEL(getPixelNominalBrown , pixel_brown )



//---------------- set foreground or background ----------------//
//---------------- set foreground or background ----------------//
//---------------- set foreground or background ----------------//

void CustomResources::setForeground(Pixel pixel, SLDelay *gui)
{
  assert(!starting);
  assert(gui && gui->W());
  XtVaSetValues(gui->W(), XmNforeground, pixel, NULL);
}


void CustomResources::setBackground(Pixel pixel, SLDelay *gui)
{
  assert(!starting);
  assert(gui && gui->W());
  XtVaSetValues(gui->W(), XmNbackground, pixel, NULL);
}


//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
