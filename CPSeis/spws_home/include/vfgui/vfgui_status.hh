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

//---------------------- vfgui_status.hh ---------------------------//
//---------------------- vfgui_status.hh ---------------------------//
//---------------------- vfgui_status.hh ---------------------------//

//              header file for the VfguiStatus class
//                derived from the SLSmartForm class
//                 derived from the VfInform class
//                       subdirectory vfgui

#ifndef _VFGUI_STATUS_HH_
#define _VFGUI_STATUS_HH_

#include "sl/sl_smart_form.hh"
#include "vf/vf_inform.hh"


class VfguiStatus : public SLSmartForm, public VfInform
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:    // also public manager() in VfInform.

  int            _clear_flag;
  class SLpText *_msg;
  class SLpText *_status2;
  class SLpText *_status3;
  class SLpText *_status4;
  class SLpText *_status5;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  VfguiStatus(  SLDelay             *slparent,
                class VfManager     *manager,
                class ContainerList *clist,
                int                  wide = TRUE);

  VfguiStatus(  Widget               wparent,
                class VfManager     *manager,
                class ContainerList *clist,
                int                  wide = TRUE);

  virtual ~VfguiStatus();

/*******
  void maybeClearMessage();                               // method C.
*******/

private:    // overriding VfInform or SLSmartForm.

  virtual void  showMessage          (const char *msg);
/*******
  virtual void  returningToEventLoop ();                  // method A.
*******/
  virtual void  update               ();                  // method B.

private:

  void constructorHelper (ContainerList *clist, int wide);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
