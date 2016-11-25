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

//------------------------ tp_resources.cc -----------------------//
//------------------------ tp_resources.cc -----------------------//
//------------------------ tp_resources.cc -----------------------//

//         implementation file for the TpResources class
//                  not derived from any class
//                       subdirectory pick


//  This class obtains custom resources for use by classes with
//  names beginning with Tp... (such as TpPopupBase and TpVectors).  

//  No instance of this class need ever be created.  All information
//  resides in non-member static variables, and is accessed thru
//  static member functions.

//  Either one of the following two functions must be called at least
//  once by the application, before calling any of the functions to get
//  any specific resource.  Any call after the first call has no effect.
//    TpResources::startup(SLDelay *gui)  // using any existing gui
//    TpResources::startup(Widget     w)  // using any existing w

//  Defined constants in tp_resources.h:
//
//    minimum allowed number of vectors = 1.
//                                                6
//    maximum allowed number of vectors = TP_MAXNUMVECTORS.
//
//    actual number of vectors = _numvectors (not a variable in this class).
//
//                           0        1        2        3        4     5
//    index of a vector = TP_CURR, TP_ORIG, TP_PREV, TP_NEXT, TP_SEL TP_REF
//                             (but not to exceed _numvectors-1)
//
//                    -1       0        1        2        3        4    5
//    snap index = TP_NONE, TP_CURR, TP_ORIG, TP_PREV, TP_NEXT, TP_SEL TP_REF
//                             (but not to exceed _numvectors-1)


#include "pick/tp_resources.hh"
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
  int     line_width;               // width of lines
  Pixel   pixel_work;               // pixel of color of work message
  Pixel   pixel_error;              // pixel of color of error message
  Pixel   pixel_green;              // pixel of color of green message
  char   *color[TP_MAXNUMVECTORS];  // names of colors of lines
  Pixel   pixel[TP_MAXNUMVECTORS];  // pixels of colors of lines
  char   *color_rubber;             // name of color of rubberband
  char   *cursor_name;              // name of font cursor for picking
  };

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
    { n, "TpResources", i, XtOffsetOf(Custom,p), j, c },


//--------------------- resources array ------------------------//
//--------------------- resources array ------------------------//
//--------------------- resources array ------------------------//


static XtResource resources[] = {
    RRR("tp_line_width"  , III, line_width    , JJ, (XtPointer)3  )
    RRR("tp_color_work"  , PPP, pixel_work    , SS, (XtPointer)"blue")
    RRR("tp_color_error" , PPP, pixel_error   , SS, (XtPointer)"red")
    RRR("tp_color_green" , PPP, pixel_green   , SS, (XtPointer)"green4")
    RRR("tp_color_curr"  , SSS, color[TP_CURR], SS, (XtPointer)"red")
    RRR("tp_color_orig"  , SSS, color[TP_ORIG], SS, (XtPointer)"brown")
    RRR("tp_color_prev"  , SSS, color[TP_PREV], SS, (XtPointer)"green4")
    RRR("tp_color_next"  , SSS, color[TP_NEXT], SS, (XtPointer)"blue")
    RRR("tp_color_sel"   , SSS, color[TP_SEL ], SS, (XtPointer)"orange")
    RRR("tp_color_ref"   , SSS, color[TP_REF ], SS, (XtPointer)"purple")
    RRR("tp_color_curr"  , PPP, pixel[TP_CURR], SS, (XtPointer)"red")
    RRR("tp_color_orig"  , PPP, pixel[TP_ORIG], SS, (XtPointer)"brown")
    RRR("tp_color_prev"  , PPP, pixel[TP_PREV], SS, (XtPointer)"green4")
    RRR("tp_color_next"  , PPP, pixel[TP_NEXT], SS, (XtPointer)"blue")
    RRR("tp_color_sel"   , PPP, pixel[TP_SEL ], SS, (XtPointer)"orange")
    RRR("tp_color_ref"   , PPP, pixel[TP_REF ], SS, (XtPointer)"purple")
    RRR("tp_color_rubber", SSS, color_rubber  , SS, (XtPointer)"orange")
    RRR("tp_cursor"      , SSS, cursor_name   , SS, (XtPointer)"center_ptr")
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



//---------------------- preset the resources -----------------//
//---------------------- preset the resources -----------------//
//---------------------- preset the resources -----------------//

     // Just in case the resources array does not refer to
     // all of the variables in the custom resource structure.

static void preset_the_resources()
{
  custom.line_width  = 3;
  custom.pixel_work  = 0;
  custom.pixel_error = 0;
  for(int ivector = 0; ivector < TP_MAXNUMVECTORS; ivector++)
      {
      custom.color[ivector] = NULL;
      custom.pixel[ivector] = 0;
      }
  custom.color_rubber = NULL;
  custom.cursor_name  = NULL;
}



//--------------------------- startup ---------------------------//
//--------------------------- startup ---------------------------//
//--------------------------- startup ---------------------------//


void TpResources::startup(SLDelay *gui)
{
  if(starting)
       {
       Widget w = get_any_widget(gui);
       startup(w);
       }
}



void TpResources::startup(Widget w)
{
  if(starting)
       {
       preset_the_resources();
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

Display *TpResources::getDisplay()
{
  return XtDisplay(getAnyWidget());
}



Widget TpResources::getAnyWidget()
{
  assert(!starting);
  return any_widget;
}



//------------------- get specified resource -----------------------//
//------------------- get specified resource -----------------------//
//------------------- get specified resource -----------------------//


int TpResources::getLineWidth()
{
  assert(!starting);
  return custom.line_width;
}


Pixel TpResources::getPixelWork()
{
  assert(!starting);
  return custom.pixel_work;
}


Pixel TpResources::getPixelError()
{
  assert(!starting);
  return custom.pixel_error;
}


Pixel TpResources::getPixelGreen()
{
  assert(!starting);
  return custom.pixel_green;
}


const char *TpResources::getColor(int ivector)
{
  assert(ivector >= 0 && ivector < TP_MAXNUMVECTORS);
  assert(!starting);
  assert(custom.color[ivector]);
  return custom.color[ivector];
}


Pixel TpResources::getPixel(int ivector)
{
  assert(ivector >= 0 && ivector < TP_MAXNUMVECTORS);
  assert(!starting);
  return custom.pixel[ivector];
}


const char *TpResources::getColorRubber()
{
  assert(!starting);
  assert(custom.color_rubber);
  return custom.color_rubber;
}


Cursor TpResources::getCursor()
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


//---------- set fallback foreground or background ----------------//
//---------- set fallback foreground or background ----------------//
//---------- set fallback foreground or background ----------------//

         // name should be sufficiently unique.
         // asterisk will be added before and after name.

static void set_fallback_resource(const char *name,
                      const char *resource, const char *value)
{
  assert(!starting);
  static String defres[]= { NULL, NULL };
  defres[0] = newstrcat("*", (char*)resource, ": ", (char*)value, NULL);
  setDefRes(TpResources::getDisplay(), (char*)name, defres);
  free(defres[0]);
}


void TpResources::setFallbackForeground(const char *name, int ivector)
{
  assert(ivector >= 0 && ivector < TP_MAXNUMVECTORS);
  set_fallback_resource(name, "foreground", getColor(ivector));
}


void TpResources::setFallbackBackground(const char *name, int ivector)
{
  assert(ivector >= 0 && ivector < TP_MAXNUMVECTORS);
  set_fallback_resource(name, "background", getColor(ivector));
}



//---------------- set foreground or background ----------------//
//---------------- set foreground or background ----------------//
//---------------- set foreground or background ----------------//

void TpResources::setForeground(int ivector, SLDelay *gui)
{
  assert(ivector >= 0 && ivector < TP_MAXNUMVECTORS);
  assert(!starting);
  assert(gui && gui->W());
  XtVaSetValues(gui->W(), XmNforeground, getPixel(ivector), NULL);
}


void TpResources::setBackground(int ivector, SLDelay *gui)
{
  assert(ivector >= 0 && ivector < TP_MAXNUMVECTORS);
  assert(!starting);
  assert(gui && gui->W());
  XtVaSetValues(gui->W(), XmNbackground, getPixel(ivector), NULL);
}


//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
