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

//---------------------- wbox_screen.cc -------------------------//
//---------------------- wbox_screen.cc -------------------------//
//---------------------- wbox_screen.cc -------------------------//

//          implementation file for the WboxScreen class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_screen.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_field.hh"
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


/******
    changes within comment delimiters like this were made 12/30/97.
******/


//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxScreen::WboxScreen(WboxAllbox *allbox, Widget widget)
         :
            _allbox            (allbox),
            _widget            (widget),
            _display           (NULL),
            _screen            (NULL),
            _root              (NULL),
            _font              (NULL),

            _cellx             (0),
            _celly             (0),
            _ascent            (0),
            _descent           (0),
            _xstart            (0),
            _ystart            (0),

            _topshadow         (0),
            _bottomshadow      (0),
            _peak              (0),
            _trough            (0),
            _background        (0),

            _normaltext        (NULL),
            _entertext         (NULL),
            _topside           (NULL),
            _erase             (NULL),
            _highlight         (NULL),
            _bottomside        (NULL),
            _button            (NULL),
            _buttontext        (NULL),
            _enterbox          (NULL),
            _messagetext       (NULL),
            _messagebox        (NULL),
            _messageloud       (NULL),
            _pressed           (NULL),
            _dimtext           (NULL),
            _enterdimtext      (NULL),
            _pressedtext       (NULL)
{
  assert(_allbox);
  assert(_widget);
  _display = XtDisplay          (_widget);
  _screen  = XtScreen           (_widget);
  _root    = RootWindowOfScreen (_screen); // in case toplevel not realized.
  getResources();
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxScreen::~WboxScreen()
{
  if(_display && _font) XFreeFont(_display, _font);
}



//--------------------------- small functions --------------------------//
//--------------------------- small functions --------------------------//
//--------------------------- small functions --------------------------//

           // public.

Pixel  WboxScreen::getBlackPixel()  const
{
  return BlackPixelOfScreen(_screen);
}


int  WboxScreen::getIrow(Cardinal y)  const
{
  int numy = (int)y + 2 * (int)_celly - (int)_ystart - (int)_descent;
  return (numy / (int)_celly);
}



int  WboxScreen::getIcol(Cardinal x)  const
{
  int numx = (int)x + (int)_cellx - (int)_xstart;
  return (numx / (int)_cellx);
}



void WboxScreen::flushBuffer()
{
  XFlush(_display);
}


void WboxScreen::ringBell()
{
  XBell (_display, 50);
}


int WboxScreen::sameScreen(Widget w)  const
{
  if(XtDisplay(w) == _display && XtScreen(w) == _screen) return TRUE;
  return FALSE;
}



//------------------------ get pixel ---------------------------------//
//------------------------ get pixel ---------------------------------//
//------------------------ get pixel ---------------------------------//

             // private.
             // set dark to TRUE  if this pixel should be dark.
             // set dark to FALSE if this pixel should be light.

Pixel WboxScreen::getPixel(const char *colorname,
                             const char *grayname, int dark)
{
  return get_named_color(_widget, 0, (char*)colorname, (char*)grayname, dark);
}



//----------------------- create GC --------------------------------------//
//----------------------- create GC --------------------------------------//
//----------------------- create GC --------------------------------------//

       // private.

GC WboxScreen::createGC(XFontStruct *font,
                                Pixel foreground, Pixel background)
{
  XGCValues values;
  long mask = GCForeground | GCBackground | GCLineWidth | GCFont;
  values.foreground = foreground;
  values.background = background;
  values.line_width = 1;
  values.font       = font->fid;
  return XCreateGC(_display, _root, mask, &values);
}



//------------------ custom resources structure --------------------//
//------------------ custom resources structure --------------------//
//------------------ custom resources structure --------------------//

struct Custom
{
  char *fontname;
  char *background, *peak, *valley, *trough;
  char *topshadow, *bottomshadow;
  char *ntext, *ebox, *mtext, *mloud, *mbox, *dim;
};



//------------------------- resource array ---------------------------//
//------------------------- resource array ---------------------------//
//------------------------- resource array ---------------------------//

#define III XtRInt   , sizeof(int   )
#define PPP XtRPixel , sizeof(Pixel )
#define SSS XtRString, sizeof(String)
#define JJ  XtRImmediate
#define SS  XtRString
#define QQQ(n,i,p,j,c)  { n, "Wbox", i, XtOffsetOf(Custom,p), j, c },

static XtResource resources[] = {
  QQQ("wboxfont"        , SSS, fontname    ,SS, (XtPointer)"8x13bold")// 10/15/97 was 9x15.
  QQQ("wboxbackground"  , SSS, background  ,SS, (XtPointer)"gray80")  // 10/15/97 was gray.
  QQQ("wboxbutton"      , SSS, peak        ,SS, (XtPointer)"light blue") // was ivory.
  QQQ("wboxhighlight"   , SSS, trough      ,SS, (XtPointer)"light blue") // was ivory3.
  QQQ("wboxpressed"     , SSS, valley      ,SS, (XtPointer)"gold" )
  QQQ("wboxtopshadow"   , SSS, topshadow   ,SS, (XtPointer)"white")
  QQQ("wboxbottomshadow", SSS, bottomshadow,SS, (XtPointer)"black")  // was slate gray.
  QQQ("wboxnormaltext"  , SSS, ntext       ,SS, (XtPointer)"black")
  QQQ("wboxdimtext"     , SSS, dim         ,SS, (XtPointer)"gray50")// 10/15/97 was gray40.
  QQQ("wboxenterbox"    , SSS, ebox        ,SS, (XtPointer)"red"  )
  QQQ("wboxmessagetext" , SSS, mtext       ,SS, (XtPointer)"blue")  // 10/15/97 was white.
  QQQ("wboxmessageloud" , SSS, mloud       ,SS, (XtPointer)"blue" )
  QQQ("wboxmessagebox"  , SSS, mbox        ,SS, (XtPointer)"black")
};



//------------------------ get resources ------------------------------//
//------------------------ get resources ------------------------------//
//------------------------ get resources ------------------------------//

           // private.

#define CREATE_FONT(fn)                                        \
   if(font == NULL)                                            \
       {                                                       \
       if(debug >= 1) printf("wbox: trying font %s\n", fn);    \
       font = XLoadQueryFont(_display, fn);                    \
       }


void WboxScreen::getResources()
{
  Custom custom;
  XtGetApplicationResources(_widget, &custom, resources,
                                XtNumber(resources), NULL, 0);
  int debug = _allbox->getDebug();
  XFontStruct *font = NULL;

  CREATE_FONT(custom.fontname)
  CREATE_FONT("-Adobe-Courier-Bold-R-Normal--14-140-75-75-M-90-ISO8859-1")
  CREATE_FONT("-Adobe-Courier-Bold-R-Normal--18-180-75-75-M-110-ISO8859-1")
  CREATE_FONT("9x15")
  CREATE_FONT("8x13bold")
  CREATE_FONT("8x13")
  CREATE_FONT("10x20")
  CREATE_FONT("fixed")
  if(font == NULL) { printf("wbox: font not found. \n"); exit(0); }
  if(debug >= 1) printf("wbox: font found. \n");

  _cellx   = font->max_bounds.width;
  _celly   = font->ascent + font->descent + 6;
  _ascent  = font->ascent;
  _descent = font->descent;
  _xstart  = 1 + _cellx;
  _ystart  = 1 + _celly/2 + font->ascent;

       _background   = getPixel(custom.background  , "gray75" , FALSE);
       _peak         = getPixel(custom.peak        , "gray85" , FALSE);
       _trough       = getPixel(custom.trough      , "gray85" , FALSE);
  Pixel valley       = getPixel(custom.valley      , "gray95" , FALSE);
       _topshadow    = getPixel(custom.topshadow   , "gray100", FALSE);
       _bottomshadow = getPixel(custom.bottomshadow, "gray20" , TRUE );
  Pixel ntext        = getPixel(custom.ntext       , "gray0"  , TRUE );
  Pixel dim          = getPixel(custom.dim         , "gray40" , TRUE );
  Pixel ebox         = getPixel(custom.ebox        , "gray10" , TRUE );
  Pixel mtext        = getPixel(custom.mtext       , "gray100", TRUE );
  Pixel mloud        = getPixel(custom.mloud       , "gray10" , TRUE );
  Pixel mbox         = getPixel(custom.mbox        , "gray0"  , TRUE );

  _erase        = createGC(font, _background  , _background);
  _normaltext   = createGC(font,  ntext       , _background);
  _dimtext      = createGC(font,  dim         , _background);
  _topside      = createGC(font, _topshadow   , _background);
  _bottomside   = createGC(font, _bottomshadow, _background);
  _enterbox     = createGC(font,  ebox        , _background);
  _messagetext  = createGC(font,  mtext       , _background);
  _messageloud  = createGC(font,  mloud       , _background);
  _messagebox   = createGC(font,  mbox        , _background);
  _buttontext   = createGC(font,  ntext       , _peak);
  _pressedtext  = createGC(font,  ntext       ,  valley);
  _entertext    = createGC(font,  ntext       , _trough);
  _enterdimtext = createGC(font,  dim         , _trough);
  _highlight    = createGC(font, _trough      ,  ntext);
  _pressed      = createGC(font,  valley      ,  ntext);
  _button       = createGC(font, _peak        ,  ntext);

///////  XFreeFontInfo(NULL, font, 0);
///////    the above frees the information structure but not the font resource.
///////    XFreeFont (in the destructor) does both.
  _font = font;
}



//----------------------- wash -----------------------------------//
//----------------------- wash -----------------------------------//
//----------------------- wash -----------------------------------//

      // private.
      // text2 = string with any number of characters null-terminated,
      //         or at least nchar characters not necessarily
      //         null-terminated.
      //         (text2 might be a fortran character variable)
      // nchar  = number of characters to draw (>= 1).
      // returns pointer to text which contains exactly nchar characters
      //         (blank-filled or truncated if necessary) followed by
      //         a null-termination.
      // returns right-justified text if svar is -45 (new 2001-08-08).
      // returns centered text if svar is 6 2 -2 -6 81 82 -56 -57
      //                                                 (new 2002-03-14).


#define NBUF  (UCHAR_MAX + 1)   // size of temporary character storage.


char *WboxScreen::wash(const char *text2, int nchar, int svar)
                                              // svar added 2001-08-08
{
  static char text[NBUF];
  assert(text2);
  assert(nchar >= 1 && nchar < NBUF);
  int finished = FALSE;
  for(int i = 0; i < nchar; i++)
      {
      if(finished == FALSE && text2[i] == '\0') finished = TRUE;
      if(finished) text[i] = ' ';
      else         text[i] = text2[i];
      }
  text[nchar] = '\0';
  if (svar == -45)                                       // new 2001-08-08
      {
      int trimmed_length = nchar;
      for(int i = nchar-1; i >= 0; i--)
          {
          if (text[i] != ' ') break;
          trimmed_length = i;
          }
      if (trimmed_length < nchar && trimmed_length > 0)
          {
          int offset = nchar - trimmed_length;
          int i;
          for(i = nchar - 1; i >= offset; i--)
              {
              text[i] = text[i - offset];
              }
          for(i = 0; i < offset; i++)
              {
              text[i] = ' ';
              }
          }
      }
  else if (svar ==  -2 || svar == -6 || svar ==  2 ||
           svar ==   6 || svar == 81 || svar == 82 ||
           svar == -56 || svar == -57)                  // new 2002-03-14
      {
      int i1 = -1;
      int i2 = -1;
      for(int i = 0; i < nchar; i++)
          {
          if (text[i] == ' ') continue;
          if(i1 == -1) i1 = i;
          i2 = i;
          }
      if(i1 >= 0)
          {
          int occupied  = i2 - i1 + 1;
          int gap1      = (nchar - occupied) / 2;
          int gap2      = nchar - occupied - gap1;
          int shiftleft = i1 - gap1;
          int i;
          if(shiftleft > 0)
              {
              for(i = i1; i <= i2; i++)
                  {
                  text[i - shiftleft] = text[i];
                  }
              for(i = i2 - shiftleft + 1; i <= i2; i++)
                  {
                  text[i] = ' ';
                  }
              }
          else if(shiftleft < 0)
              {
              for(i = i2; i >= i1; i--)
                  {
                  text[i - shiftleft] = text[i];
                  }
              for(i = i1; i <= i1 - shiftleft - 1; i++)
                  {
                  text[i] = ' ';
                  }
              }
          }
      }
  return text;
}



//--------------------- switch values ---------------------------------//
//--------------------- switch values ---------------------------------//
//--------------------- switch values ---------------------------------//

//   svar = 70    draw red highlight box around entered field             
//   svar = 60    erase red highlight box around entered field             
//   svar = 50    draw insert curser                          
//   svar = 40    draw replace/overstrike curser       (reverse video)
//   svar = 22    enterable editable dimmed text field (bright background)
//   svar = 14    enterable non-editable radio button
//   svar = 13    enterable non-editable toggle button
//   svar = 12    enterable non-editable text field    (label look-alike)
//   svar = 11    enterable non-editable text field
//   svar = 7     enterable editable text field        (gold background)
//   svar = 6     enterable editable gold pushbutton   (gold background)
//   svar = 5     enterable non-editable text field    (bright background)
//   svar = 4     enterable editable radio button      (bright or gold)
//   svar = 3     enterable editable toggle button     (bright or gold)
//   svar = 2     enterable editable pushbutton        (bright background)
//   svar = 1     enterable editable text field        (bright background)
//   svar = 0     prompt or other message or information  
//   svar = -1    bypassed text field                      
//   svar = -2    bypassed pushbutton                       
//   svar = -3    bypassed toggle button                     
//   svar = -4    bypassed radio button                       
//   svar = -5    bypassed non-editable text field             
//   svar = -6    bypassed pushbutton
//   svar = -7    bypassed text field                      
//   svar = -11   bypassed non-editable text field
//   svar = -12   bypassed non-editable text field (label look-alike)
//   svar = -13   bypassed non-editable toggle button
//   svar = -14   bypassed non-editable radio button
//   svar = -22   bypassed dimmed text field                     
//   svar = -25   histogram plot
//   svar = -33   special white message in a box (wboxmessagetext) 
//   svar = -34   special  red  message in a box (wboxmessageloud)  
//   svar = -44   special white message          (wboxmessagetext)   
//   svar = -45   special white message right justified (new 2001-08-08)
//   svar = -55   dimmed prompt                          
//   svar = -56   centered prompt                          
//   svar = -57   centered bypassed non-editable text field
//   svar = -66   vertical line in middle of field width given by nchar.
//   svar = -77   blank field displayed                   
//   svar <= -99  nothing displayed at all (nothing is drawn)
//   other switch values have undefined behavior.              

//   the following switch values should not be used by the user,
//   because they are reserved for use by the windowbox routines:
//      70, 60, 50, 40, -999, 81, 82.



//---------------------- draw3 -----------------------------------//
//---------------------- draw3 -----------------------------------//
//---------------------- draw3 -----------------------------------//

           // public.

void WboxScreen::draw3(WboxBox *box, WboxField *field, const char *text)
{
  int irow    = field->getIrow ();
  int icol    = field->getIcol ();
  int nchar   = field->getNchar();
  int svar    = field->getSvar ();
  draw(box, irow, icol, text, nchar, svar);
}




//---------------------- draw2 -----------------------------------//
//---------------------- draw2 -----------------------------------//
//---------------------- draw2 -----------------------------------//

           // public.

void WboxScreen::draw2(WboxBox *box, WboxField *field, const char *text,
                                int svar, int draw_text_only)
{
  int irow  = field->getIrow ();
  int icol  = field->getIcol ();
  int nchar = field->getNchar();
  if(svar == 1)
      {
      int svar2 = field->getSvar();
      if     (svar2 == 2) svar = 81;        // centered text.
      else if(svar2 == 6) svar = 82;        // centered gold text.
      }
  draw(box, irow, icol, text, nchar, svar, draw_text_only);
}



//------------------ draw or erase highlight box ------------------//
//------------------ draw or erase highlight box ------------------//
//------------------ draw or erase highlight box ------------------//

           // public.

void WboxScreen::drawHighlightBox(WboxBox *box)
{
  WboxField *active = box->getActiveFieldPointer();
  int          svar = active->getSvar();
  if(box->hasFocus() && svar >= 1) draw2(box, active, " ", 70);
  else                             draw2(box, active, " ", 60);
}


void WboxScreen::eraseHighlightBox(WboxBox *box)
{
  WboxField *active = box->getActiveFieldPointer();
  draw2(box, active, " ", 60);
}



//------------------ small private drawing functions ----------------//
//------------------ small private drawing functions ----------------//
//------------------ small private drawing functions ----------------//

            // private.

void WboxScreen::drawRect(Window wind, GC gc,
                            int xlo, int ylo, int width, int height)
{
  XDrawRectangle(_display, wind, gc, xlo, ylo, width, height);
}



void WboxScreen::fillRect(Window wind, GC gc,
                            int xlo, int ylo, int width, int height)
{
  XFillRectangle(_display, wind, gc, xlo, ylo, width, height);
}



void WboxScreen::fillRadio(Window wind, GC gc,
                             int xlo, int ylo, int width, int height)
{
  XPoint points[4];
  points[0].x = xlo;
  points[1].x = xlo + width / 2;
  points[2].x = xlo + width;
  points[3].x = xlo + width / 2;
  points[0].y = ylo + height / 2;
  points[1].y = ylo;
  points[2].y = ylo + height / 2;
  points[3].y = ylo + height;
  XFillPolygon(_display, wind, gc, points, 4, Convex, CoordModeOrigin);
}



void WboxScreen::drawLine(Window wind, GC gc,
                            int xlo, int ylo, int xup, int yup)
{
  XDrawLine(_display, wind, gc, xlo, ylo, xup, yup);
}



void WboxScreen::drawString(Window wind, GC gc, int x, int y,
                                   const char *text, int nchar)
{
  XDrawString(_display, wind, gc, x, y, text, nchar);
}



void WboxScreen::drawImageString(Window wind, int svar, int x, int y,
                                   const char *text, int nchar)
{
  GC gc;
  switch(svar)
      {
      case   2: gc = _buttontext  ; break;
      case   6: gc = _pressedtext ; break;
      case   7: gc = _pressedtext ; break;   // new 2001-11-20
      case  82: gc = _pressedtext ; break;   // new 2002-03-14
      case   1: gc = _entertext   ; break;
      case   5: gc = _entertext   ; break;
      case  81: gc = _entertext   ; break;   // new 2002-03-14
      case -33: gc = _messagetext ; break;
      case -44: gc = _messagetext ; break;
      case -45: gc = _messagetext ; break;   // new 2001-08-08
      case -34: gc = _messageloud ; break;
      case -55: gc = _dimtext     ; break;
      case -22: gc = _dimtext     ; break;
      case  22: gc = _enterdimtext; break;
      default:  gc = _normaltext  ; break;
      }
  XDrawImageString(_display, wind, gc, x, y, text, nchar);
}



void WboxScreen::drawShadow(Window wind, GC top, GC bottom,
                              int xlo, int ylo, int xup, int yup)
{
  drawLine(wind, top   , xlo, ylo, xlo  , yup+1);
  drawLine(wind, top   , xlo, ylo, xup+1, ylo  );
  drawLine(wind, bottom, xup, ylo, xup  , yup+1);
  drawLine(wind, bottom, xlo, yup, xup+1, yup  );
}



void WboxScreen::drawRshadow(Window wind, GC top, GC bottom,
                               int xlo, int ylo, int xup, int yup)
{
  int xmid = (xlo + xup) / 2;
  int ymid = (ylo + yup) / 2;
  drawLine(wind, top   , xlo  , ymid  , xmid, ylo );
  drawLine(wind, top   , xmid , ylo   , xup , ymid);
  drawLine(wind, bottom, xup+1, ymid-1, xmid, yup );
  drawLine(wind, bottom, xmid , yup   , xlo , ymid);
}



//--------------------- draw histograms -----------------------------//
//--------------------- draw histograms -----------------------------//
//--------------------- draw histograms -----------------------------//

     // private.
     // if there is a |:
     //   foreground histogram is drawn from | to the right to P.
     //   foreground histogram is drawn from | to the left  to M.
     //   background histogram is drawn from | to the right to p.
     //   background histogram is drawn from | to the left  to m.
     //   both     histograms are drawn from | to the right to +.
     //   both     histograms are drawn from | to the left  to -.
     //   a bar is drawn at |.
     // if there is no |:
     //   foreground histogram is drawn from left  edge to P.
     //   foreground histogram is drawn from right edge to M.
     //   background histogram is drawn from left  edge to p.
     //   background histogram is drawn from right edge to m.
     //   both     histograms are drawn from left  edge to +.
     //   both     histograms are drawn from right edge to -.

void WboxScreen::drawHistograms(Window wind, const char *text, int nchar,
                                int x, int ylo, int yup, int height)
{
  int plus1  = -1;
  int plus2  = -1;
  int plus3  = -1;
  int minus1 = -1;
  int minus2 = -1;
  int minus3 = -1;
  int bar    = -1;
  for(int i = 0; i < nchar; i++)
      {
      if     (text[i] == 'P') plus1  = i;
      else if(text[i] == 'p') plus2  = i;
      else if(text[i] == '+') plus3  = i;
      else if(text[i] == 'M') minus1 = i;
      else if(text[i] == 'm') minus2 = i;
      else if(text[i] == '-') minus3 = i;
      else if(text[i] == '|') bar    = i;
      }
  drawHistogram(wind, FALSE, bar, nchar, plus2, minus2, x, ylo, yup, height);
  drawHistogram(wind, FALSE, bar, nchar, plus3, minus3, x, ylo, yup, height);
  drawHistogram(wind, TRUE , bar, nchar, plus1, minus1, x, ylo, yup, height);
  drawHistogram(wind, TRUE , bar, nchar, plus3, minus3, x, ylo, yup, height);
}



//--------------------- draw histogram -----------------------------//
//--------------------- draw histogram -----------------------------//
//--------------------- draw histogram -----------------------------//

     // private.
     // if primary is TRUE, draws narrow bright histogram.
     // if primary is FALSE, draws wider dim histogram.
     // if bar >= 0:
     //   histogram is drawn from index bar to the right to index plus.
     //   histogram is drawn from index bar to the left  to index minus.
     //   a bar is drawn at index bar
     // if bar == -1:
     //   histogram is drawn from left  edge to index plus.
     //   histogram is drawn from right edge to index minus.

void WboxScreen::drawHistogram(Window wind, int primary, int bar,
                               int nchar, int plus, int minus,
                               int x, int ylo, int yup, int height)
{
  GC gc;
  int y2, h2;
  if(primary) { gc = _pressed  ; y2 = ylo+3; h2 = height-6; }
  else        { gc = _highlight; y2 = ylo+1; h2 = height-2; }
  if(bar == -1)
      {
      if(plus != -1)
          {
          int x2 = x;
          int w2 = plus * _cellx + _cellx / 2;
          fillRect(wind, gc, x2, y2, w2, h2);
          }
      if(minus != -1)
          {
          int x2 = x + nchar * _cellx;
          int w2 = (nchar - minus - 1) * _cellx + _cellx / 2;
          fillRect(wind, gc, x2 - w2, y2, w2, h2);
          }
      }
  else
      {
      int x2 = x + bar * _cellx + _cellx / 2;
      if(plus != -1 && plus > bar)
          {
          int w2 = (plus - bar) * _cellx;
          fillRect(wind, gc, x2, y2, w2, h2);
          }
      if(minus != -1 && minus < bar)
          {
          int w2 = (bar - minus) * _cellx;
          fillRect(wind, gc, x2 - w2, y2, w2, h2);
          }
      drawLine(wind, _normaltext, x2, ylo, x2, yup);
      }
}



//------------------------- draw --------------------------------//
//------------------------- draw --------------------------------//
//------------------------- draw --------------------------------//

     // public.
     // if draw_text_only is TRUE, this routine has permission
     //   to draw only the text, and to omit the border stuff.
     // draw_text_only defaults to FALSE.
     // the user may set draw_text_only to TRUE only if these
     //   circumstances are true:
     //    (1) the switch value has not changed.
     //    (2) this is not an EXPOSE event.
     //    (3) this is not the active datafield in this windowbox.
     // this routine ignores the value of draw_text_only unless
     //   the datafield is a text field or a pushbutton.
     // the purpose of the draw_text_only argument is to improve
     //   performance (re-draw speed) when the border does not have
     //   to be redrawn.  this is particularly desirable when
     //   scrolling linked arrays.

void WboxScreen::draw(WboxBox *box, int irow, int icol,
                      const char *text2, int nchar, int svar,
                      int draw_text_only)
{
  if(nchar <= 0) return;
  if(box == NULL) return;
  if(!box->isManaged()) return;
  if(svar <= -99) return;

  if(svar ==  3 || svar ==  -3 || svar ==  4 || svar ==  -4 ||
     svar == 13 || svar == -13 || svar == 14 || svar == -14)
      {
      if(nchar > 2) nchar = 2;
      }

  char *text = wash(text2, nchar, svar);    // svar added 2001-08-08
  Window wind = box->getWindow();
  GC fill, top, bottom;

  unsigned int width  = nchar * _cellx + 2;
/*****
  unsigned int width  = nchar * _cellx + 3;
*****/
  unsigned int height =         _celly - 5;
  int          x      = _xstart + (icol - 1) * _cellx;
  int          y      = _ystart + (irow - 1) * _celly;
/*****
  int          xlo    = x - 1;
  int          xup    = x - 1 + width;
*****/
  int          xlo    = x - 2;
  int          xup    = x - 2 + width;
  int          ylo    = y - _ascent -1;
  int          yup    = y + _descent;

  if(svar == 70)             // draw box around entered field.
     {
     if(_cellx >= 8)
         {
         drawRect(wind, _enterbox, xlo-2, ylo-2, width+4, height+4);
         drawRect(wind, _enterbox, xlo-3, ylo-3, width+6, height+6);
         }
     else       // new for narrow fonts.
         {
         drawRect(wind, _enterbox, xlo-2, ylo-2, width+4, height+4);
         drawRect(wind, _enterbox, xlo-2, ylo-3, width+4, height+6);
         }
     return;
     }

  else if(svar == 60)       // erase box around entered field.
     {
     if(_cellx >= 8)
         {
         drawRect(wind, _erase, xlo-2, ylo-2, width+4, height+4);
         drawRect(wind, _erase, xlo-3, ylo-3, width+6, height+6);
         }
     else       // new for narrow fonts.
         {
         drawRect(wind, _erase, xlo-2, ylo-2, width+4, height+4);
         drawRect(wind, _erase, xlo-2, ylo-3, width+4, height+6);
         }
/*****
     drawRect(wind, _erase, xlo-2, ylo-2, width+4, height+4);
     drawRect(wind, _erase, xlo-3, ylo-3, width+6, height+6);
*****/
     return;
     }

  else if(svar == 50)                 // insert curser.
     {
/*****
     drawLine(wind, _normaltext, x  , ylo, x  , yup);
     drawLine(wind, _normaltext, x+1, ylo, x+1, yup);
*****/
     drawLine(wind, _normaltext, x-1, ylo+1, x-1, yup);
     drawLine(wind, _normaltext, x  , ylo+1, x  , yup);
     return;
     }

  else if(svar == 40)            // replace/overstrike curser.
     {
     fillRect  (wind, _normaltext, xlo+1, ylo+1, width-1, height-1);
/*****
     fillRect  (wind, _normaltext, xlo+1, ylo, width-1, height);
*****/
     drawString(wind, _highlight, x, y, text, nchar);
     return;
     }

  else if(svar == -66)     // vertical line at first char (nchar == 1),
                           // or between first and second char (nchar == 2),
                           // or in general in middle of range of nchar.
     {
     int x2 = x + (nchar * _cellx) / 2;
     fillRect(wind, _erase, xlo-1, ylo-1, width+3 , height+3);
     drawLine(wind, _normaltext, x2, ylo, x2, yup);
     return;
     }

  else if(svar == -25)             // histogram plot.
     {
     fillRect       (wind, _erase, xlo-1, ylo-1, width+3 , height+3);
     drawHistograms (wind, text, nchar, x, ylo, yup, height);
     return;
     }

  else if(svar ==  3 || svar ==  -3 ||
          svar == 13 || svar == -13)              // toggle button.
     {
/********
     /////// future:
     drawErase (wind, xlo, ylo, width, height);
     drawToggle(wind, text, svar, xlo, ylo, width, height, xup, yup);
********/
     if(text[0] == '0' || text[0] == ' ')
        { fill   = _button;
          top    = _topside;
          bottom = _bottomside; }
     else
        { fill   = _pressed;  
          top    = _bottomside; 
          bottom = _topside; }
     if(svar < 0) fill = _erase;
     fillRect  (wind, _erase     , xlo-1, ylo-1, width+3 , height+3);
/*****
     fillRect  (wind, fill       , xlo+4, ylo+2, width-9 , height-4);
     drawShadow(wind, top, bottom, xlo+4, ylo+2, xup-5, yup-2);
     drawShadow(wind, top, bottom, xlo+3, ylo+1, xup-4, yup-1);
*****/
     fillRect  (wind, fill       , xlo+4, ylo+2, width-7 , height-4);
     drawShadow(wind, top, bottom, xlo+4, ylo+2, xup-3, yup-2);
     drawShadow(wind, top, bottom, xlo+3, ylo+1, xup-2, yup-1);
     return;
     }

  else if(svar ==  4 || svar ==  -4 ||
          svar == 14 || svar == -14)              // radio button.
     {
/********
     /////// future:
     drawErase(wind, xlo, ylo, width, height);
     drawRadio(wind, text, svar, xlo, ylo, width, height, xup, yup);
********/
     if(text[0] == '0' || text[0] == ' ')
        { fill   = _button;
          top    = _topside;
          bottom = _bottomside; }
     else
        { fill   = _pressed;  
          top    = _bottomside; 
          bottom = _topside; }
     if(svar < 0) fill = _erase;
     fillRect   (wind, _erase, xlo-1, ylo-1, width+3 , height+3);
/*****
     fillRadio  (wind, fill  , xlo+3, ylo+1, width-7 , height-2);
     drawRshadow(wind, top, bottom, xlo+3, ylo+1, xup-4, yup-1);
     drawRshadow(wind, top, bottom, xlo+2, ylo  , xup-3, yup  );
*****/
     fillRadio  (wind, fill  , xlo+3, ylo+1, width-5 , height-2);
     drawRshadow(wind, top, bottom, xlo+3, ylo+1, xup-2, yup-1);
     drawRshadow(wind, top, bottom, xlo+2, ylo  , xup-1, yup  );
     return;
     }

  else if(!draw_text_only)      // text or pushbutton with changed switch.
     {
     if(svar == 2)      fill = _button;    // enterable pushbutton.
     if(svar == 6)      fill = _pressed;   // enterable gold pushbutton.
     else if(svar >= 1) fill = _highlight; // enterable text.
     else               fill = _erase;     // bypass field.
     fillRect(wind, fill, xlo, ylo, width, height);

     if     (svar == 2  || svar == -2 ||
             svar == 6  || svar == -6 )                // pushbutton.
           { top    = _topside;
             bottom = _bottomside; }

     else if(svar == 1  || svar == -1  ||
             svar == 22 || svar == -22 ||
             svar == 81 || svar ==  82 ||
             svar == 7  || svar == -7)                 // text.
           { top    = _bottomside;
             bottom = _topside; }
     else                                              // something else.
           { top    = _erase;
             bottom = _erase; }
     drawShadow(wind, top, bottom, xlo  , ylo  , xup  , yup  );
     drawShadow(wind, top, bottom, xlo-1, ylo-1, xup+1, yup+1);
     }

  if(svar == -77) return;
  drawImageString(wind, svar, x, y, text, nchar);
  if(!draw_text_only)
      {
      if(svar == -33 || svar == -34 || svar == 5 ||
         svar ==  -5 || svar ==  11 || svar == -11 || svar == -57)
             drawRect(wind, _messagebox, xlo, ylo, width, height);
      }
}



//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//

