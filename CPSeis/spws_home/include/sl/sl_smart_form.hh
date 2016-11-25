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

//---------------------- sl_smart_form.hh -----------------------//
//---------------------- sl_smart_form.hh -----------------------//
//---------------------- sl_smart_form.hh -----------------------//

//               header file for the SLSmartForm class
//                  derived from the SLDelay class
//                        subdirectory sl

#ifndef _SL_SMART_FORM_HH_
#define _SL_SMART_FORM_HH_

#include "sl/sl_delay.hh"
#include "sl/attachment_list.hh"
#include <Xm/Form.h>


class SLSmartForm : public SLDelay
{

//---------------------- data --------------------------//
//---------------------- data --------------------------//
//---------------------- data --------------------------//

protected:

  Boolean  _manage_now;           // TRUE or FALSE
  Boolean  _show_even_spacing;    // TRUE or FALSE
  AttachmentList _attlist;        // linked list of attachments.

//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//

public:

  SLSmartForm(  SLDelay *slparent,
                char    *name,
                HelpCtx  hctx        = NULL,
                Boolean  doframe     = FALSE,
                Boolean  make_if_can = TRUE,
                Boolean  manage_now  = TRUE  );

  SLSmartForm(  Widget   wparent,
                char    *name,
                HelpCtx  hctx       = NULL,
                Boolean  doframe    = FALSE,
                Boolean  make_now   = TRUE,
                Boolean  manage_now = TRUE  );

  SLSmartForm(  Widget   w,
                HelpCtx  hctx       = NULL,
                Boolean  make_now   = TRUE,
                Boolean  manage_now = TRUE  );

  virtual ~SLSmartForm();

//--------------------- other functions ----------------------//
//--------------------- other functions ----------------------//
//--------------------- other functions ----------------------//

public:

  virtual WidgetClass topClass() { return xmFormWidgetClass; };
  virtual Boolean  isContainer() { return TRUE; };

  Widget make(Widget p = NULL);

  void showEvenSpacing();
  void attach(SLDelay *gui, SLDelay *left      , SLDelay *right  = NULL,
                            SLDelay *top = NULL, SLDelay *bottom = NULL,
                            int oleft = 0, int oright  = 0,
                            int otop  = 0, int obottom = 0);

//------------------------ end functions ---------------------//
//------------------------ end functions ---------------------//
//------------------------ end functions ---------------------//

};

#endif

//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
