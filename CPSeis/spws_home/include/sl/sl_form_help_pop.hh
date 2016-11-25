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
#ifndef SLFORMHELPPOP_HH
#define SLFORMHELPPOP_HH

#include "sl/sl_form_pop.hh"

class SLFormHelpPop : public SLFPopSep {
  private:
  protected:
      Widget _helpline;
      Widget _low2sep;

  public:
     SLFormHelpPop(  Widget        p,
                     char          *name,
                     unsigned long buttons,
                     HelpCtx       hctx,
                     Boolean       small_on_dpi    =True,
                     const int     help_rows       =2,
                     Boolean       make_now        =True,
                     const Boolean is_really_popup =True,
                     const int     num_colors      =0,
                     const int     may_icon        =UseResource,
                     const int     screen_number   =UseResource);

     SLFormHelpPop(  SLDelay       *contain,
                     char          *name,
                     unsigned long buttons,
                     HelpCtx       hctx,
                     Boolean       small_on_dpi    =True,
                     const int     help_rows       =2,
                     Boolean       make_if_can     =True,
                     const Boolean is_really_popup =True,
                     const int     num_colors      =0,
                     const int     may_icon        =UseResource,
                     const int     screen_number   =UseResource);
     virtual ~SLFormHelpPop();
     virtual Widget make(Widget p =NULL);
     Widget  helpLine() {return _helpline;}
     void    init( const Display *dpy, char *name, const int help_rows);
     virtual Widget bottomSeparator();
};
#endif
