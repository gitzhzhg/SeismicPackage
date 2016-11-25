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
//------------------- tred_pick_gui.hh ----------------------//
//------------------- tred_pick_gui.hh ----------------------//
//------------------- tred_pick_gui.hh ----------------------//

//         header file for the TredPickGui class
//            derived from the SLSmartForm class
//                    subdirectory tred

#include "sl/sl_smart_form.hh"

#ifndef _TRED_PICK_GUI_HH_
#define _TRED_PICK_GUI_HH_

#define MIN_HWD 0
#define MAX_HWD 64 /*only used as a default*/
#define NUM_HWDS 3

class TredTablePop;
class SLpRadio;
class RadioList;
class SLpText;

class TredPickGui : public SLSmartForm
{

//---------------------------- data ------------------------//
//---------------------------- data ------------------------//
//---------------------------- data ------------------------//

  private:

    long      _irad;
    long      _del_hwd[NUM_HWDS];
    long      _kill_hwd[NUM_HWDS];
    long      _rev_hwd[NUM_HWDS];
    long      _flag_hwd[NUM_HWDS];
    long      _min_hwd;
    long      _max_hwd;
    RadioList *_radios;
    SLpRadio  *_drad;
    SLpRadio  *_krad;
    SLpRadio  *_rrad;
    SLpRadio  *_frad;
    SLpText   *_del_hwd_text[NUM_HWDS], *_kill_hwd_text[NUM_HWDS],
              *_rev_hwd_text[NUM_HWDS], *_flag_hwd_text[NUM_HWDS];
//---------------------------- functions ------------------------//
//---------------------------- functions ------------------------//
//---------------------------- functions ------------------------//

  public:

    TredPickGui (SLDelay *slparent, TredTablePop *pop, char *name,
      HelpCtx hctx);
    virtual ~TredPickGui (void);
    virtual Widget make (Widget p = NULL);
    void updateNumHeaders(long num_headers);
};

#endif

//---------------------------- end ------------------------//
//---------------------------- end ------------------------//
//---------------------------- end ------------------------//
