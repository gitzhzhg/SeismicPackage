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
#ifndef XVISUAL_COLLECTION_HH
#define XVISUAL_COLLECTION_HH

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>

class XvisualCollection {

public:
  static class Xvisual *fetch
    (Display *display, 
     int iscreen);

  static class Xvisual *fetch
    (Screen *screen);

  static class Xvisual *fetch
    (Widget widget);

  static class Xvisual *fetchExisting
    (Display *display, 
     int iscreen);

  static class Xvisual *fetchExisting
    (Screen *screen);

  static class Xvisual *fetchExisting
    (Widget widget);

  static void addResources
    (Arg *arglist,
     int *n,
     Display *display, 
     int iscreen);

  static void addResources
    (Arg *arglist,
     int *n,
     Screen *screen);

  static void addResources
    (Arg *arglist,
     int *n,
     Widget widget);

  static Boolean matches
    (class Xvisual *xvisual,
     Display *display, 
     int iscreen);

  static Boolean matches
    (class Xvisual *xvisual,
     Screen *screen);

  static Boolean matches
    (class Xvisual *xvisual,
     Widget widget);
};

#endif
