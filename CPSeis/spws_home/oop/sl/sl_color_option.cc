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
#include <stdlib.h>
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include "wproc.h"
#include "sl/psuedo_widget.hh"
#include "sl/sl_color_option.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"




SLColorOption::SLColorOption( Widget             p,
                              char               *name,
                              HelpCtx            hctx,
                              SLPushAry          pushary,
                              const unsigned int arycnt,
                              Boolean            doframe,
                              Boolean            make_now ) :
               SLOptionMenu(p,name,hctx,pushary,arycnt,NULL,doframe,False)

{
  if (make_now) make(p);
}



SLColorOption::SLColorOption( SLDelay            *contain,
                              char               *name,
                              SLPushAry          pushary,
                              const unsigned int arycnt,
                              Boolean            doframe,
                              Boolean            make_if_can ) :
               SLOptionMenu(contain,name,contain->getHelpCtx(),
                            pushary,arycnt,NULL,doframe,False),
                            _pix_ary(NULL)
{
 if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}


SLColorOption::~SLColorOption()
{
  if (_pix_ary) {
    free(_pix_ary);
  }
}



Widget SLColorOption::make(Widget p)
{
  int stat;
  XColor cell_def;
  XColor rgb_def;
  Colormap cmap;

  if ( made() ) return topWidget();
  SLOptionMenu::make(p);
  p= wParent();

  _pix_ary= (PixelAry)calloc( _buttons->_arycnt, sizeof ( SLPixel) );

  Paintset *paintset;
  Pixel pixel;
  int red, green, blue;
  for(int i=0; (i<_buttons->_arycnt); i++) {
        _pix_ary[i].ident= _buttons->_pushary[i].ident;
/*
        XtVaGetValues(_buttons->_pushary[i].w, XmNcolormap, &cmap, NULL);
        stat= XAllocNamedColor( XtDisplay(_buttons->_pushary[i].w), cmap, 
                                _buttons->_pushary[i].name, 
                                &cell_def, &rgb_def);
        _pix_ary[i].background= cell_def.pixel;
        _pix_ary[i].foreground= foregroundPixel(p,&cell_def);
        XtVaSetValues( _buttons->_pushary[i].w, 
                             XmNbackground, (Pixel)cell_def.pixel,
                             XmNarmColor  , (Pixel)cell_def.pixel,
                             XmNforeground, _pix_ary[i].foreground, NULL);
*/
	paintset = PaintsetCollection::fetchExistingByColormap (
          _buttons->_pushary[i].w);
	pixel = paintset->getBackgroundPixelFromName (
          _buttons->_pushary[i].name);
	paintset->splitPixel (pixel, &red, &green, &blue);
        _pix_ary[i].background = pixel;
        _pix_ary[i].foreground = foregroundPixel (p, pixel);
        XtVaSetValues (_buttons->_pushary[i].w, 
		       XmNbackground, pixel,
		       XmNarmColor  , pixel,
		       XmNforeground, _pix_ary[i].foreground, NULL);
  }
  SLColorOption::optionAction(whichSelected());


  return topWidget();
}

Pixel SLColorOption::currentPixel() 
{
 int i= retidx(whichSelected());
 return _pix_ary[i].background;
}

char *SLColorOption::currentColor() 
{
 char *str= NULL;
 if (made()) {
       Widget w= buttonW(whichSelected());
       str= get_simp_labelstrptr(w);
 }
 else {
       str= newstr(_def_value);
 }
 return str;
}


int SLColorOption::retidx( int ident)
{
  Boolean found;
  long i=0;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( _pix_ary[i].ident == ident) {
                found= True;
      } // End if
  } // End loop

  if (!found) i= 0;
  return (i-1);
}



void SLColorOption::optionAction(int ident)
{
  if (_pix_ary) {
      int i= retidx(ident); 
      if (i>-1) {
         XtVaSetValues( topWidget(), XmNbackground, _pix_ary[i].background,  
                                     XmNarmColor  , _pix_ary[i].background,  
                                     XmNforeground, _pix_ary[i].foreground, 
                                     NULL);
         WidgetList children;
         Cardinal   num;
         XtVaGetValues( topWidget(), XmNnumChildren, &num,
                                     XmNchildren,    &children, 
                                     NULL);
         for (int i= 0; (i<num); i++) {
              if (XtClass(children[i]) == xmCascadeButtonWidgetClass) {
                      XtVaSetValues( children[i], 
                            XmNbackground, _pix_ary[i].background,  
                            NULL );
              }
         }
      }
                     
  } // end if

}


Pixel SLColorOption::foregroundPixel(Widget w, Pixel pixel)
{
//Screen *scr= XtScreen(w);
  Paintset *paintset = PaintsetCollection::fetchExistingByColormap (w);
  Pixel fground;
  int red, green, blue;
  paintset->splitPixel (pixel, &red, &green, &blue);
  if ((red + blue + green) > 98303) {
    fground = paintset->black ();
  }
  else {
    fground = paintset->white ();
  }
  return fground;
}
