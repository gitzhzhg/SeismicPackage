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

//----------------------- custom_resources.hh -------------------------//
//----------------------- custom_resources.hh -------------------------//
//----------------------- custom_resources.hh -------------------------//

//            header file for the CustomResources class
//                   not derived from any class
//                       subdirectory pick


//    See comments in the implementation file to learn the
//    purpose of this class and how it is to be used.


#ifndef _CUSTOM_RESOURCES_HH_
#define _CUSTOM_RESOURCES_HH_

#include <X11/Intrinsic.h>


class CustomResources
{

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

            // no data

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:

  CustomResources() {}             // actually not needed
  virtual ~CustomResources() {}    // actually not needed

  static void        startup (Widget w);
  static void        startup (class SLDelay *gui);

  static Display    *getDisplay    ();
  static Widget      getAnyWidget  ();

  static int         getLineWidth  ();
  static const char *getColorRubber();
  static Cursor      getCursor     ();

  static const char *getColorNominalRed   ();
  static const char *getColorNominalOrange();
  static const char *getColorNominalYellow();
  static const char *getColorNominalGreen ();
  static const char *getColorNominalBlue  ();
  static const char *getColorNominalBrown ();

  static Pixel       getPixelNominalRed   ();
  static Pixel       getPixelNominalOrange();
  static Pixel       getPixelNominalYellow();
  static Pixel       getPixelNominalGreen ();
  static Pixel       getPixelNominalBlue  ();
  static Pixel       getPixelNominalBrown ();

  static void        setForeground (Pixel pixel, class SLDelay *gui);
  static void        setBackground (Pixel pixel, class SLDelay *gui);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
