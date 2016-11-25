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
/* -------------------------------------------------------------------------*/

#include "cenv.h"
#include "mod_pop.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Scale.h>




/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void mod_sclcb(Widget w,
               struct CB    *udata,
               XmScaleCallbackStruct *CBdata )

{
 struct mod_popinfo  *mod;

 mod= (struct mod_popinfo *)udata->info;


 switch (udata->wconst) {      

    case SEGSCL :


       ErsSegmentChangeAttributes(mod->pikrec,
                                  mod->chorptr->segary[mod->chorptr->cseg],
                                  mod->chorptr->hor_name,
                                  mod->chorptr->col_str,
                                  OTHER_LINEW );
       mod->chorptr->cseg= CBdata->value-1;
       ErsSegmentChangeAttributes(mod->pikrec,
                                  mod->chorptr->segary[mod->chorptr->cseg],
                                  mod->chorptr->hor_name,
                                  mod->chorptr->col_str,
                                  ACTIVE_LINEW );
      PR_SetCurrentHorizon(
                  mod->pikrec, mod->chorptr->segary[mod->chorptr->cseg] );

/* R.S. Day 4/21/93
      set_label( mod->mwig[DELSEL].w, "Delete Segment" );
      XtSetSensitive( mod->mwig[DELSEL].w, True );
*/
      XtSetSensitive( mod->delseg, True );
      XtSetSensitive( mod->delhor, True );
      mod->deltyp= delete_seg;

      break;


 ENDswitch

}
