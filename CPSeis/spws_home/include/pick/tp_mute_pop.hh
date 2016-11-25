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

//------------------------ tp_mute_pop.hh ----------------------------//
//------------------------ tp_mute_pop.hh ----------------------------//
//------------------------ tp_mute_pop.hh ----------------------------//

//                header file for the TpMutePop class
//                derived from the TpPopupBase class
//                         subdirectory pick

//         The prefix TpMute... means "mute trace picking"

#ifndef _TP_MUTE_POP_HH_
#define _TP_MUTE_POP_HH_

#include "pick/tp_popup_base.hh"


class TpMutePop : public TpPopupBase
{


//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
public:          // constructor and destructor

  TpMutePop (SLDelay *slparent, char *name,
                HelpCtx hctx, class SeisPlot *sp);

  virtual ~TpMutePop();

  Widget make(Widget p);

  class TpScanGui        *_scan;
  class TpMuteAutoGui    *_auto;


  virtual void setBreakState(Boolean set);
  virtual void setBreakSensitivity(Boolean set);
  virtual void setSnapState(Boolean set);
  virtual void setSnapSensitivity(Boolean set);

private:       
  
  class SLpToggle        *_auto_tog;




//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//



//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
