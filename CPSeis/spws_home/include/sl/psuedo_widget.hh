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
#ifndef PSUEDOWIDGET_h
#define PSUEDOWIDGET_h

#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include "wproc.h"
#include "cprim.h"



class PsuedoWidget{


  protected:
      Widget _w;
      char  *_istr;
      char  *_cstr;

  public:
      enum { NoScreenResource= -1, OtherScreen= -2};
      PsuedoWidget( const Widget w);
      PsuedoWidget( const PsuedoWidget *pw, char *name, 
                    const WidgetClass classptr);
      PsuedoWidget( const PsuedoWidget *pw, char *name, 
                    const class SLDelay *classptr);
      PsuedoWidget( const Widget w, ...);
      ~PsuedoWidget(); 

      Widget anyW() const { return _w; };
      const Display *display() const { return XtDisplay(_w); }

    // name your own resource
      Boolean anyBooleanDef(const char *inst_res_str,
                            const char *class_res_str) const; // any Boolean
      int anyIntDef(const char *inst_res_str,
                    const char *class_res_str) const; // any int
      char *anyDef(const char *inst_res_str,
                   const char *class_res_str) const; // return any resource

    // Toggle Widget
      Boolean togDef() const;
      Boolean childTogDef(char *name) const;    //return value child toggle

    // Text Widget
      char   *textDef() const;
      char   *childTextDef(char *name) const;   //return value child text

    // FileChoice Widget
      char   *fileChoiceDef() const;            //return value FileChoice
      char   *childFileChoiceDef(char *name) const;  

    // Scale Widget
      int    scaleDef() const;
      int    childScaleDef(char *name) const;   //return value child scale

    // form Widget
      int    formScreenDef() const;

    // Option Widget
      char *optionDef() const;
      char *childOptionDef(char *name) const;  //return value child scale

    // List Widget
      int    listCountDef() const;
      int    childListCountDef(char  *name) const;
      XmStringTable itemsDef() const;
      XmStringTable childItemsDef(char  *name) const;
      XmStringTable selectedItemDef() const;
      XmStringTable childSelectedItemDef(char  *name) const;
      int    selectedCountDef() const;
      int    childSelectedCountDef(char  *name) const;
};

#endif
