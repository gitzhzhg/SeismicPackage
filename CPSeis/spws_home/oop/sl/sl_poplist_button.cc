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
#include "sl/sl_poplist_button.hh"
#include "sl/container_list.hh"
#include "sl/sl_shell_container.hh"


SLPopListButton::SLPopListButton(SLDelay       *contain,
                                 char          *name,
                                 ContainerList *all_pops,
                                 Boolean        make_if_can) :
               SLPullPop(contain,name,True,False),
               _all_pops(all_pops)
{
    if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}



Widget SLPopListButton::make(Widget p)
{
  SLShellContainer *q;
  if (!made()) {
    SLPullPop::make(p);
    for(q= _all_pops->top(); (q); q= _all_pops->next() ) {
          addPushUp( (char*)q->instanceName(), q);
          if (_all_pops->needsSeparator()) addSep();
    } // end loop
  } // end if
  return topWidget();
}
