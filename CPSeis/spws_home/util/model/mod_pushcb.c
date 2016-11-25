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
#if (!VMS)
#include <unistd.h>
#endif
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include "matrix.h"
#include "cenv.h"
#include "mod_pop.h"


void auto_add_new( struct mod_popinfo  *mod);

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void mod_pushcb(Widget                     w,
                struct CB                  *udata,
                XmPushButtonCallbackStruct *CBdata)


/*
 *   this function is call when one of the four bottom push button are
 *   pushed. - OK, apply, cancel, help
 */


{
 struct mod_popinfo  *mod;
 Arg arglist[3];
 long i, n;
 long wconst;
 long stat;
 void delhor();
 char *str;
 struct horizon_info *chorptr;
 /* wmm static void change_hor_color();
 static void nxtcell();
*/
 void change_hor_color();
 void nxtcell();

 


 mod= (struct mod_popinfo *)udata->info;

 /*
  * determine which push button
  */

 if (udata->wconst == MODPOP) {
      n=0;
      XtSetArg( arglist[n], XmNuserData, &wconst ); n++;
      XtGetValues( w, arglist, n);
      switch (wconst) {
         case FP_OK:
                      /* stat= doerrchk( mod); */
                      if (mod->input_disp) {
                            stat= wproc_calltx( mod->mwig, mod->mcb,
                                                PHEAD, SHEAD, THEAD );
                            if (stat) {
                               XtUnmanageChild( mod->mwig[MODPOP].w );
                               mod->push_func( mod->push_data, wconst);
                               PR_SetHkeys(I_gphWidget(mod->image),
                                                        mod->phead,
                                                        mod->shead,
                                                        mod->thead );
                               wproc_setsen(mod->mwig,False,4,
                                            PHEAD,SHEAD,THEAD,IFIL);
                               mod->input_disp= False;
                               if (mod->chorptr) choose_trans( mod);

                            ENDif
                      ENDif
                      else {
                            wproc_setsen(mod->mwig,False,4,
                                         PHEAD,SHEAD,THEAD,IFIL);
                            XtUnmanageChild( mod->mwig[MODPOP].w );   
                      ENDelse
                      break;
         case FP_CANCEL: 
                      XtUnmanageChild( mod->mwig[MODPOP].w );
                      mod->push_func( mod->push_data, wconst);
                      mod_clean(mod, True);

                      break;
         case FP_HELP:
                     overview_help("overview_mod_pop", mod->helpctx);
                     break;

      ENDswitch
 ENDif
 else {
      switch (udata->wconst) {
         case NEWHORT: {
/*
                      Boolean more_trying = True;

                      for(mod->auto_cnt; (more_trying); mod->auto_cnt++ ) {
                         sprintf( tmp_horname, "%s%1d", 
                                                 DEFHORIZON, mod->auto_cnt );
                         more_trying= !is_unique_h( mod->hor_head, tmp_horname);
                      ENDloop
                      XbaeMatrixEditCell(  mod->hor_list, mod->hor_tot, 0);
                      nxtcell(mod);
                      XbaeMatrixSetCell(mod->hor_list, mod->row, 0, 
                                        tmp_horname);
                      add_new(mod, mod->row, tmp_horname, True);
                      show_current( mod, tmp_horname);
*/
                      auto_add_new(mod);

                      }
                     break;

         case REHORT:
                     break;

         case PCKTYP:
                     break;

         case EDIT:
                     break;

         case ENDSEG:
                     add_new_seg( mod);
                     break;

         case DELSEL:
                     switch (mod->deltyp) {
                             case delete_hor  : mod_delhor(mod); break;
                             case delete_seg  : mod_delseg(mod); break;
                             case delete_pt   : break;
                     ENDswitch
                     break;

         case UNDO:
                     break;

         case C0:     case C1:
         case C2:     case C3:
         case C4:     case C5:
         case C6:     case C7:
         case C8:     case C9:
                    /* mod->curr_cell= mod->line_cells[udata->wconst-C0]; */
                    XtVaGetValues( w,  XmNbackground, &mod->curr_cell, NULL);
                    get_simp_labelstr(w,mod->curr_color);
                    change_hor_color( mod);
                    XbaeMatrixSetCellColor( mod->hor_list, 
                                            mod->row, 1, mod->curr_cell);
                    
                    break;

         case PEAK:   case TROFF:
         case PTOM:   case MTOP:
                    mod->snap_mode=  udata->wconst;
                    break;

         case MANNO:
                    mod->pick_mode=  udata->wconst;
                    XtSetSensitive( mod->snaprc, False );
                    choose_trans(mod);
                    break;
         case MANWITH:
         case AUTO:
                    mod->pick_mode=  udata->wconst;
                    XtSetSensitive( mod->snaprc, True );
                    choose_trans(mod);
                    break;
        
      ENDswitch
 ENDelse


}



void auto_add_new( struct mod_popinfo  *mod)

{
  Boolean more_trying = True;
  /* wmm static Boolean is_unique_h(); */
  Boolean is_unique_h();
  char tmp_horname[50];
  /* wmm static void nxtcell();  */      /*   PSH - added for VMS version  */
  void nxtcell();        /*   PSH - added for VMS version  */

  for(mod->auto_cnt; (more_trying); mod->auto_cnt++ ) {
     sprintf( tmp_horname, "%s%1d", DEFHORIZON, mod->auto_cnt );
     more_trying= !is_unique_h( mod->hor_head, tmp_horname);
  } /* End loop */
  XbaeMatrixEditCell(  mod->hor_list, mod->hor_tot, 0);
  nxtcell(mod);
  XbaeMatrixSetCell(mod->hor_list, mod->row, 0, 
  tmp_horname);
  add_new(mod, mod->row, tmp_horname, True);
  show_current( mod, tmp_horname);
}



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
/* wmm static void change_hor_color( struct mod_popinfo  *mod) */
void change_hor_color( struct mod_popinfo  *mod)

{
 struct horizon_info *chorptr;
 chorptr= mod->chorptr;

 XbaeMatrixSetCell(  mod->hor_list, mod->row, 1, mod->curr_color);
 if (chorptr)
  { strcpy(chorptr->col_str, mod->curr_color);
    /*
     *   This operates on all segments in a Horizon
     */ 
    ErsHorizonChangeAttributes( mod->pikrec, chorptr->horptr,
                                chorptr->hor_name, chorptr->col_str,
                                ACTIVE_LINEW );
  }

}




/*
 * ------------------------------ FUNCTION ----------------------------------
 */
/* wmm static Boolean is_unique_h( struct horizon_info *hor_head, */
Boolean is_unique_h( struct horizon_info *hor_head,
                            char                *horstr )

{
  struct horizon_info *p;
  Boolean found= False;

  for (p= hor_head; ( (p!=NULL) && (!found) ); ) {
     if (strcmp(horstr, p->hor_name) == 0 )
              found= True;
     else
              p= p->nxt;
  ENDloop

  return (!found);    /* if it is found if is not unique otherwise it is */

}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
/* wmm static void nxtcell( struct mod_popinfo  *mod ) */
void nxtcell( struct mod_popinfo  *mod )

{
  mod->nxt_good_cell=  (mod->nxt_good_cell < mod->tot_good_cells-1 ) ?
                               mod->nxt_good_cell+1 : 0;

  mod->curr_cell= mod->good_cells[mod->nxt_good_cell];
  strcpy(mod->curr_color, mod->color_strary[mod->nxt_good_cell] );
}

