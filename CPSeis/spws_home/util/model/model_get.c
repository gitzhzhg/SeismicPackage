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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>
#include "file_choice.h"
#include "image.h"
#include "ers_seis.h"
#include "model_io.h"
#include "pick.h"
#include "pcard.h"
#include "wproc.h"

#define LOADSTR "Loading model file, please wait..."
static void model_update(ModInfo *mod);

/************************************************************
 * model_get()  success function called by file selection
 * Purpose:     Read in an old picking_record
 ************************************************************/
void model_get (Widget                          w,
                struct CB                       *udata,
                wprocFileChoiceSuccCallbackPtr CBdata )
{
 PR_ *pikrec;
 Boolean          success;
 ModInfo          *mod;
 Widget           shell, pshell;
 char             *tempstr;
 int              i,ityp;

    printf(" ----------------- MODEL_GET ---------------------\n");
    mod= (ModInfo *)udata->info;


    if (!mod->load_in_progress) {
        printf("file= %s, save file= %s\n", mod->infile, CBdata->sav_filename);
        if(strlen(mod->infile)==0) {
           wproc_setsen( mod->mwig, True, 3, PHEAD, SHEAD, THEAD);
           return;
        }

        mod_clean( mod, False);
        pikrec = mod->pikrec;

        shell = (Widget)XutilGetShell(w);
        pshell = (Widget)XutilGetShell( XtParent(shell) );
        wprocCursorSet( shell,  mod->watch_cur);
        wprocCursorSet( pshell,  mod->watch_cur);

        tempstr= wprocPushMsg(  mod->help_text, LOADSTR);
        XSync(XtDisplay(shell), False);

        PR_SetHkeys(I_gphWidget(mod->image),
                                 mod->phead,
                                 mod->shead,
                                 mod->thead );
        success = pcardrd(mod->model,mod->infile,&ityp);
        wprocPopMsg(  mod->help_text, tempstr);

       /*
        * Update the information on the user interface 
        */
        if(success == True) {
                mod->load_in_progress= True;
                model_update( mod);
                mod->load_in_progress= False;
         for(i=0;i<pikrec->horizon_count;i++)
          { ErsHorizonSetColor(pikrec,pikrec->horizon_list[i]);
          }
         if (pikrec->horizon_count > 0) {
                 pikrec->current_horizon = pikrec->horizon_list[0];
                 PR_Draw(pikrec,I_gphWidget(mod->image),NULL);
         } /* End if */

        }
        CBdata->doit= Success;
/*
        wprocFileChoiceSetFile( mod->mwig[OFIL].w, CBdata->filename, True);
*/
        wprocCursorNone(shell);
        wprocCursorNone(pshell);
    }
    else
        CBdata->doit= False;
 return;
}


void sync_picking( struct mod_popinfo  *mod)
{
 PR_ *pikrec;
 int i;

    if(mod == NULL) return;
    pikrec = mod->pikrec;
    mod->load_in_progress= True;
    mod_clean( mod, False);

    PR_SetHkeys(I_gphWidget(mod->image),
                              mod->phead,
                              mod->shead,
                              mod->thead );
    model_update( mod);



   for(i=0;i<pikrec->horizon_count;i++)
    { 
     ErsHorizonSetColor(pikrec,pikrec->horizon_list[i]);
    }

   if (pikrec->horizon_count > 0)
    {
      pikrec->current_horizon = pikrec->horizon_list[0];
      PR_Draw(pikrec,I_gphWidget(mod->image),NULL);
    }
   if(mod->chorptr != NULL)
    {PR_SetCurrentHorizon(pikrec,mod->chorptr->horptr);
    }

   mod->load_in_progress= False;
}


/*************************************************************
 * model_update()
 * Purpose:     Updates the user intrface after the read.
 ************************************************************/
static void model_update( struct mod_popinfo  *mod)
{
 ErsModel   *model;
 PR_ *pikrec;
 ErsHorizon *hlist[199];
 int        i,nhor,nseg,N;
 char    tmpstr[30];
 static long call_cnt=0;

 if(mod == NULL) return;
 printf(" ----------------- MODEL_UPDATE ---------------------\n");
 call_cnt++;
 model= mod->model;
 if(model == NULL) return;
 if(mod->Spik_or_Vpik == MODPICK)
   pikrec = (PR_ *) model_getpdata(model);
 else
   pikrec = mod->pikrec;
 if(pikrec== NULL) return;

 mod->phead = PR_GetPhdr(pikrec);
 mod->shead = PR_GetShdr(pikrec);
 mod->thead = PR_GetThdr(pikrec);
 TEXT_set_int( mod->mwig[PHEAD].w, tmpstr, mod->phead);
 TEXT_set_int( mod->mwig[SHEAD].w, tmpstr, mod->shead);
 TEXT_set_int( mod->mwig[THEAD].w, tmpstr, mod->thead);
 wproc_setsen( mod->mwig, False, 3, PHEAD, SHEAD, THEAD);
 mod->pikrec= pikrec;
/*
 * Get list of unique horizon names  (hlist)
 */
 ErsHorizonGetHList(pikrec, &nhor , hlist);
 for(i=0;i<nhor;i++)
  {
   mod->row= i;
   set_curr_color( mod, hlist[i]->color_name );
   add_new( mod, i, hlist[i]->horizon_name, False ); 
  }
 set_opm_to_color(mod);
/*
 * Set a current horizon and update GUI and graphics
 */
 if (nhor > 0 )
  { set_segdisp( mod->mwig[SEGSCL].w, mod->chorptr->num_segs, 
                                      mod->chorptr->cseg);
    mod->chorptr->horptr = mod->chorptr->segary[mod->chorptr->cseg];
    pikrec->current_horizon = mod->chorptr->horptr;
    show_current( mod, mod->chorptr->hor_name);
  }
 else /* Create 1st new horizon for convenience */
  { hlist[0] = ErsHorizonCreate(mod->pikrec, "dummy", "red",
                  ACTIVE_LINEW, True);
    add_new( mod, 0, "dummy", False ); 
    mod->chorptr->horptr= hlist[0];
    pikrec->current_horizon = hlist[0];
  }
 return;
}



set_curr_color( ModInfo    *mod,
                char       *newcol )

{
  Boolean found;
  int i;

  for( i=0, found= False; ((i< mod->tot_good_cells) && (!found) ); i++ ) {
      if (strcmp( newcol, mod->color_strary[i]) == 0)
             found= True;
  }
  i--;
  if (found) {
       strcpy( mod->curr_color, mod->color_strary[i]);
       mod->curr_cell= mod->good_cells[i];
  }
  else {  /* if not found then set to black */

       strcpy( mod->curr_color, mod->color_strary[mod->tot_good_cells-1]);
       mod->curr_cell= mod->good_cells[mod->tot_good_cells-i];
  }

}
