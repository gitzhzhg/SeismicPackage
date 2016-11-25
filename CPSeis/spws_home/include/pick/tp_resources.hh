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

//----------------------- tp_resources.hh -------------------------//
//----------------------- tp_resources.hh -------------------------//
//----------------------- tp_resources.hh -------------------------//

//              header file for the TpResources class
//                   not derived from any class
//                       subdirectory pick


//    Although there are a public constructor and a public
//    destructor, it is not necessary to create an instance
//    of this class.  See comments in tp_resources.cc for
//    more information.

//    The enum constants defined below are needed by several classes
//    with names starting with Tp...  These are defined outside
//    of a class, rather than being an enum within a class, to
//    reduce the need to include large header files in other files,
//    and to improve the clarity of code where they are used.


#ifndef _TP_RESOURCES_HH_
#define _TP_RESOURCES_HH_

#include <X11/Intrinsic.h>


enum { TP_NONE = -1, TP_CURR, TP_ORIG, TP_PREV, TP_NEXT, TP_SEL,
       TP_REF, TP_MAXNUMVECTORS };


class TpResources
{

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

            // no data

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:

  TpResources() {}
  virtual ~TpResources() {}

  static void        startup (Widget w);
  static void        startup (class SLDelay *gui);

  static Display    *getDisplay    ();
  static Widget      getAnyWidget  ();

  static int         getLineWidth  ();
  static Pixel       getPixelWork  ();
  static Pixel       getPixelError ();
  static Pixel       getPixelGreen ();
  static const char *getColor      (int ivector);
  static Pixel       getPixel      (int ivector);
  static const char *getColorRubber();
  static Cursor      getCursor     ();

  static void setFallbackForeground (const char *name, int ivector);
  static void setFallbackBackground (const char *name, int ivector);

  static void        setForeground (int ivector, class SLDelay *gui);
  static void        setBackground (int ivector, class SLDelay *gui);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
