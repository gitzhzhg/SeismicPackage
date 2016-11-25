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

//----------------------- wbox_screen.hh -------------------------//
//----------------------- wbox_screen.hh -------------------------//
//----------------------- wbox_screen.hh -------------------------//

//             header file for the WboxScreen class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_SCREEN_HH_
#define _WBOX_SCREEN_HH_

#include <X11/Intrinsic.h>
#include "wproc.h"

class WboxBox;
class WboxField;

class WboxScreen
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class WboxAllbox   *_allbox;
  Widget              _widget;
  Display            *_display;
  Screen             *_screen;
  Window              _root;
  XFontStruct        *_font;

  Dimension _cellx;
  Dimension _celly;
  Dimension _ascent;
  Dimension _descent;
  Dimension _xstart;
  Dimension _ystart;

  Pixel     _topshadow;
  Pixel     _bottomshadow;
  Pixel     _peak;
  Pixel     _trough;
  Pixel     _background;

  GC        _normaltext;
  GC        _entertext;
  GC        _topside;
  GC        _erase;
  GC        _highlight;
  GC        _bottomside;
  GC        _button;
  GC        _buttontext;
  GC        _enterbox;
  GC        _messagetext;
  GC        _messagebox;
  GC        _messageloud;
  GC        _pressed;
  GC        _dimtext;
  GC        _enterdimtext;
  GC        _pressedtext;


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  WboxScreen (WboxAllbox *allbox, Widget widget);

  virtual ~WboxScreen();

  WboxAllbox   *getAllboxPointer()  const  { return _allbox; }
  Display      *getDisplay      ()  const  { return _display; }
  Dimension     getCellx        ()  const  { return _cellx; }
  Dimension     getCelly        ()  const  { return _celly; }
  Dimension     getAscent       ()  const  { return _ascent; }
  Pixel         getTopShadow    ()  const  { return _topshadow; }
  Pixel         getBottomShadow ()  const  { return _bottomshadow; }
  Pixel         getPeak         ()  const  { return _peak; }
  Pixel         getTrough       ()  const  { return _trough; }
  Pixel         getBackground   ()  const  { return _background; }
  Pixel         getBlackPixel   ()  const;

  int      getIrow      (Cardinal y)  const;
  int      getIcol      (Cardinal x)  const;
  void     flushBuffer  ();
  void     ringBell     ();
  int      sameScreen   (Widget w)  const;   // returns TRUE or FALSE.

  void  drawHighlightBox  (WboxBox *box);
  void  eraseHighlightBox (WboxBox *box);
  void  draw3      (WboxBox *box, WboxField *field, const char *text);
  void  draw2      (WboxBox *box, WboxField *field, const char *text,
                               int svar, int draw_text_only = 0);  // FALSE
  void  draw       (WboxBox *box, int irow, int icol, const char *text,
                    int nchar, int svar, int draw_text_only = 0);  // FALSE

private:

  Pixel getPixel (const char *colorname, const char *grayname, int dark);
  GC    createGC (XFontStruct *font, Pixel foreground, Pixel background);
  char *wash     (const char *text2, int nchar, int svar);

  void  drawRect  (Window, GC gc, int xlo, int ylo, int width, int height);
  void  fillRect  (Window, GC gc, int xlo, int ylo, int width, int height);
  void  fillRadio (Window, GC gc, int xlo, int ylo, int width, int height);
  void  drawLine  (Window, GC gc, int xlo, int ylo, int xup  , int yup);

  void  drawString(Window, GC gc, int x, int y, const char *text, int nchar);
  void  drawImageString
               (Window, int svar, int x, int y, const char *text, int nchar);

  void  drawShadow (Window, GC top, GC bottom,
                                        int xlo, int ylo, int xup, int yup);
  void  drawRshadow(Window, GC top, GC bottom,
                                        int xlo, int ylo, int xup, int yup);

  void  drawHistograms(Window wind, const char *text, int nchar,
                                int x, int ylo, int yup, int height);

  void  drawHistogram (Window wind, int primary, int bar,
                                int nchar, int plus, int minus,
                                int x, int ylo, int yup, int height);

  void  getResources();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


