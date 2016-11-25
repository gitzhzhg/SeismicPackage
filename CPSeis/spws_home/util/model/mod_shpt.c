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
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include "cenv.h"
#include "mod_pop.h"


void mod_shpt(Widget             w, 
              struct mod_popinfo *mod,
              ErsPoint           *point)
{
  XmString cstring;
  char string[256];
  char pval[16],sval[16],tval[16];
  Arg arg[3];
  float time;

  if(mod->pikrec->Phdr == UNDEFINED_KEY )
   strcpy(pval,"UNDEFINED");
  else
   sprintf(pval,"%f",point->pkey);

  if(mod->pikrec->Shdr == UNDEFINED_KEY )
   strcpy(sval,"UNDEFINED");
  else
   sprintf(sval,"%f",point->skey);

  if(mod->pikrec->Thdr == UNDEFINED_KEY )
   strcpy(tval,"UNDEFINED");
  else
   sprintf(tval,"%f",point->tkey);
 
  time = point->time;
  sprintf(string, "__________________________________\n\
Pick Time    = %f\n\
Trace Number = %d\n\
Primary   Key= %s\n\
Secondary Key= %s\n\
Tertiary  Key= %s\n", time, point->tn,pval,sval,tval);


  show_msg( mod->infobox, string);
}
