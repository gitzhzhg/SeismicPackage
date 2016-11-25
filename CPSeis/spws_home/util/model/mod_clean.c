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
#include "pcard.h"

/********
C\USER DOC
C-----------------------------------------------------------------------
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y
C       written in c -- designed to be called from c
C
C     Utility Name:  mod_clean
C          Written:  92/09/01  by:  T. Roby
C     Last revised:  93/03/30  by:  R.S. Day
C
C  Purpose:       Clean up the model picking control panel in
C                 preperation for end or restarting picking.
-----------------------------------------------------------------------
C                                NOTES
C
C  1. ersclean = True is a flag to call DisconnectGUI(), which will
C     disconnect from all widgets and do a conditional model destroy.
C     mod_clean destroys all horizons unless specifically forbidden
C     by the can_destroy_model flag.
C-----------------------------------------------------------------------
C\END DOC
****/

void mod_clean( struct mod_popinfo  *mod,
                Boolean             ersclean )


{

  Widget widget;
  struct horizon_info *p, *q;
  int                 wcount; /* number of connected widgets */


  if(mod == NULL) return;

  if (ersclean)
   {/*Disconnect from all widgets, and do conditional model destroy */
      DisconnectGUI(mod);
      refresh(mod->image, 0, 0, ImageAll, ImageAll);
   } /* END if */

  for(p=q= mod->hor_head; (p!= NULL); q=p )
   {
     if (!ersclean )
      {/* delete all horizons and reset horizon count */
       /* was done by DisconnectGUI if ersclean = True */
       if(mod->can_destroy_model) del_ers_hor_and_segs(p,mod->pikrec);
      }
     p= p->nxt;
     free(q);
   } /* ENDloop */

  mod->chorptr= NULL;
  mod->hor_head= NULL;
  mod->hor_tail= NULL;
  mod->last_hor_name[0]= '\0';
  mod->hor_tot= 0;
  mod->auto_cnt= 0;
/* reset output file name if we are stopping picking */
  if(mod->can_destroy_model == True)
    strcpy(mod->outfile,"untitled.pck");
  wcount = 0;
  if(mod->pikrec != NULL)
  wcount= PR_WidgetCount(mod->pikrec);
  if(wcount == 0) mod->Spik_or_Vpik = MODPICK;

  XtUnmanageChild( mod->mwig[SEGSCL].w );
  XtSetSensitive(  mod->mwig[ENDSEG].w, True );

  XtVaSetValues(  mod->hor_list, XmNrows, 1,  NULL );

  XbaeMatrixSetCell(  mod->hor_list, 0, 0, "");
  XbaeMatrixSetCell(  mod->hor_list, 0, 1, "");


}
