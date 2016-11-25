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
#ifndef SLCOLOROPTION_HH
#define SLCOLOROPTION_HH

#include "sl/sl_option_menu.hh"

typedef struct _SLPixel {
                            Pixel background;
                            Pixel foreground;
                            long  ident;
                           } SLPixel, *PixelAry;


class SLColorOption : public SLOptionMenu {

  private:
      PixelAry   _pix_ary; 
      Pixel foregroundPixel(Widget w, Pixel pixel);
      int  retidx( int ident);

  public:
       SLColorOption( Widget             p,
                      char               *name,
                      HelpCtx            hctx,
                      SLPushAry          pushary,
                      const unsigned int arycnt,
                      Boolean            doframe =False,
                      Boolean            make_now=True );

       SLColorOption( SLDelay            *contain,
                      char               *name,
                      SLPushAry          pushary,
                      const unsigned int arycnt,
                      Boolean            doframe =False,
                      Boolean            make_now=True );
       ~SLColorOption();
       Widget make(Widget p);
       virtual void optionAction(int ident);

       char *currentColor();
       Pixel currentPixel();
       const int   currentIndex() const;
};

#endif
