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
#ifndef SLAVEPOP_H
#define SLAVEPOP_H

#include "sl/sl_form_pop.hh"
#include "sl/sl_push_box.hh"
#include "oprim/handles_errors.hh"
#include "ipc/ll_sd.hh"


class SlaveDisplayMaster;
class SlavePush;

class SlavePop : public SLFPopSep, public HandlesErrors {

  protected:
     char *_dname;
     char *_host_name;
     virtual void    DoAction();
     virtual void    manage();
     void            setStatus();
     virtual Boolean ValidInput();
     SlavePush       *_buttons;
     Widget          _display_w;
     Widget          _slave_stat;
     SlaveDisplayLinkedList *_slaves;

  public:
    SlavePop(  Widget             p, 
               char               *name, 
               HelpCtx            hctx,
               SlaveDisplayLinkedList *slaves,
               Boolean            make_now =True); 
     virtual ~SlavePop();
     virtual Widget make(Widget p);
     void    removeSlave();
     virtual void    errorNotify(int);
     friend class SlavePush;
};

#endif
