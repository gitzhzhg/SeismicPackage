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
#include "wproc.h"
#include "sl/sl_form_pop.hh"
#include "sl/sl_prim.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_scale.hh"
#include "va.h"



class ZoomOp :  public SLFPopSep {


  protected:
       virtual void    DoAction();
       virtual void    UndoInput();
       static  void    show_area( void *data, long which );
       SLScale     *_zfact;
       SLScale     *_area_size;
       SLRadioBox  *_ztype;
       float       _zoom_factor;
       long        _zoom_area;
       long        _current_type;

   public:
       ZoomOp( Widget            p,
               char              *name,
               HelpCtx           hctx);
       virtual Widget make(Widget p);
       virtual void manage();
       Boolean zoomByArea();
       float   zoomFactor()    { return _zoom_factor; }
       long    zoomPixelArea() { return _zoom_area; }
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method);

};
