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
/*
 * Name        : mod_cb
 * File        : mod_cb.c
 * Executable  :  - 
 * Author      : Trey Roby
 * Date        : 11/1/92
 *
 * This functions contain all the applications specific callbacks for the 
 * model popup.   
 *
 * CHANGES:
 */

/* -------------------------------------------------------------------------*/

#include <stdio.h>
#if (!VMS)
#include <unistd.h>
#endif
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/XmP.h>
#include <X11/Shell.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/Separator.h>
#include <Xm/CascadeBG.h>
#include <Xm/Form.h>
#include "matrix.h"
#include "file_choice.h"
#include "cenv.h"
#include "mod_pop.h"
#include "image.h"


extern dotextcb();


void change_deltyp();

 
#define EXIST_MSG "Your output file exists and will be overwritten\n\
when you save your picks\n\n Is this OK ???"

#define Nofile_msg "Mute input file does not exist"
#define Nofile_outmsg "Mute output file is required."



#define ADDENDSTR "Btn#1: Pick,  Btn#2: Unpick,  Btn#3: Popup Menu,\n\
Ctl-Btn#1: Hor. Sel,  Ctl-Btn#2: Area Del.,  Shft-Btn#1: Pt Move"

#define ADDBEGSTR "Btn#1: Pick,  Btn#2: Unpick,  Btn#3: Popup Menu,\n\
Ctl-Btn#1: Hor. Sel,  Ctl-Btn#2: Area Del.,  Shft-Btn#1: Pt Move"

#define MOVSEGSTR "Btn#1: Move Segment, Btn#2: Unpick,\nBtn#3: Popup Menu\n"

#define INSPCKSTR "Btn#1: Move Point,  Btn#2: Unpick,  Btn#3: Popup Menu,\n\
Ctl-Btn#1: Hor. Sel,  Ctl-Btn#2: Area Del.,  Shft-Btn#1: Pt Move"

#define SHPCKSTR "Btn#1: Show Pick Information,  Btn#3: Popup Menu,\n\
Ctl-Btn#1: Hor. Sel,  Ctl-Btn#2: Area Del.,  Shft-Btn#1: Pt Move"

#define AUTOSTR "Btn#1: Auto Pick,  Btn#3: Popup Menu,\n\
Ctl-Btn#1: Hor. Sel,  Ctl-Btn#2: Area Del.,  Shft-Btn#1: Pt Move"


void choose_trans(  struct mod_popinfo  *mod)


{

 if (mod->may_pick) {
   if (!mod->input_disp) { 

      switch (mod->pick_mode) {
          case MANNO:
                 mod->pikrec->snap_mode=  ErsNO_SNAP;
             break;

          case MANWITH:
                 switch (mod->snap_mode) {
                          case PEAK:
                                    mod->pikrec->snap_mode = ErsSNAP_MAXIMUM; 
                                    mod->pikrec->track_mode= ErsTRACK_MAXIMUM; 
                                    break;
                          case TROFF: 
                                    mod->pikrec->snap_mode = ErsSNAP_MINIMUM; 
                                    mod->pikrec->track_mode= ErsTRACK_MINIMUM; 
                                    break;
                          case PTOM: 
                                    mod->pikrec->snap_mode= 
                                                     ErsSNAP_MP_ZERO_CROSSING; 
                                    mod->pikrec->track_mode= 
                                                     ErsTRACK_MP_ZERO_CROSSING; 
                                    break;
                          case MTOP:
                                    mod->pikrec->snap_mode= 
                                                     ErsSNAP_PM_ZERO_CROSSING; 
                                    mod->pikrec->track_mode= 
                                                     ErsTRACK_PM_ZERO_CROSSING; 
                                    break;
                 }  /* ENDswitch */
             break;
          case AUTO:
                 PR_SetTranslations( mod->pikrec,
                                                  mod->tranpars.auto_pck,
                                                  ErsOVERRIDE_TRANSLATIONS);
                  show_msg(mod->help_text, AUTOSTR);
             break;
      } /* ENDswitch */

      if ( (mod->pick_mode == MANNO) || (mod->pick_mode == MANWITH) ) {
          switch (mod->pcktyp) {
                case ADDBEG:
                  PR_SetTranslations( mod->pikrec,
                                                   mod->tranpars.add_beg,
                                                   ErsOVERRIDE_TRANSLATIONS);
                  show_msg(mod->help_text, ADDBEGSTR);
                   break;
                case ADDEND:
                  PR_SetTranslations( mod->pikrec,
                                                   mod->tranpars.add_end,
                                                   ErsOVERRIDE_TRANSLATIONS);
                  show_msg(mod->help_text, ADDENDSTR);
                  break;
                case INSPCK:
                  PR_SetTranslations( mod->pikrec,
                                                   mod->tranpars.ins_pt,
                                                   ErsOVERRIDE_TRANSLATIONS);
                  show_msg(mod->help_text, INSPCKSTR);
                  break;
                case MOVSEG:
                  PR_SetTranslations( mod->pikrec,
                                                   mod->tranpars.seg_mov,
                                                   ErsOVERRIDE_TRANSLATIONS);
                  show_msg(mod->help_text, MOVSEGSTR);
                  break;
                case SHPCK:
                  PR_SetTranslations( mod->pikrec,
                                                   mod->tranpars.show_pt,
                                                   ErsOVERRIDE_TRANSLATIONS);
                  show_msg(mod->help_text, SHPCKSTR);

                  break;
          } /* ENDswitch */
      } /* END if */
   }  /* END if */
   else
       XtUninstallTranslations( I_gphWidget(mod->image) );
 }  /* END if */


}


mod_picking_set( Widget  modpop,
                 Boolean on      )

{
  struct mod_popinfo  *mod;
  int i;

  if (modpop) {
      XtVaGetValues( modpop,  XmNuserData, &mod, NULL );
      if (mod) {
          if (on){
               mod->may_pick= True;
               choose_trans( mod);
                for(i=0; (i<mod->pikrec->horizon_count); i++) {
                       ErsHorizonDraw( mod->pikrec, 
                                       mod->pikrec->horizon_list[i],
                                       I_gphWidget(mod->image), 0, 0, 
                                       I_gphWid(mod->image), 
                                       I_gphHght(mod->image) );
                } /* End for loop */
          }  /* END if */
          else {    
               XtUninstallTranslations( I_gphWidget(mod->image) );
               mod->may_pick= False;
          }  /* End else */
      }  /* End if */
  }  /* End if */
}




static void tog(Widget                       w,
                struct CB                    *udata,
                XmToggleButtonCallbackStruct *CBdata)

{

 struct mod_popinfo  *mod;
 mod= (struct mod_popinfo *)udata->info;


  switch (udata->type) {

     case TYPE_TOGGLE : 
                        *(long *)(udata->fldptr)= CBdata->set;
                        switch (udata->wconst) {
                        ENDswitch
                        break;
     case TYPE_RADIO : 
                        if (CBdata->set) {
                            *(int *)(udata->fldptr)= udata->wconst;
                            choose_trans( mod);
                        ENDif
                        break;
  ENDswitch





}



static void map(Widget              w,
                struct CB           *udata,
                XmAnyCallbackStruct *CBdata)


{
 struct mod_popinfo  *mod;
 Arg arglist[3];
 long i, n;
 long wconst;
 unsigned long flags;
 Widget shell;

  mod= (struct mod_popinfo *)udata->info;

 
  n=0;
  XtSetArg (arglist[n], XmNrows, mod->hor_tot+1  ) ; n++;
  XtSetValues( mod->hor_list, arglist, n);
/* R.S. Day 4/13/93  Commented out
  shell = (Widget)XutilGetShell(w);
  shell = (Widget)XutilGetShell( XtParent(shell) );

  if (mod->input_disp) wprocFileChoiceValidate( mod->mwig[IFIL].w, True );

  PR_SetHkeys(I_gphWidget(mod->image),
                                 mod->phead,
                                 mod->shead,
                                 mod->thead );

  if (mod->hor_head == NULL) {
               auto_add_new(mod);
  ENDif
*/


}


/********************************************************/


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void trav_cell(Widget              w,
                      struct CB           *udata,
                      XbaeMatrixTraverseCellCallbackStruct *CBdata)

{
 struct mod_popinfo  *mod;
 Arg arglist[3];
 long i, n;
 long wconst;


 mod= (struct mod_popinfo *)udata->info;

 if (CBdata->param) {
   if (strcmp( CBdata->param, "Pointer") == 0 ) {
           CBdata->next_column= 0;
   ENDif
   else if (strcmp( CBdata->param, "Left") == 0) {
           CBdata->next_column= 0;
           if (CBdata->row == 0 )
                  CBdata->next_row= CBdata->num_rows-1;
   ENDelse
   else {
      if (CBdata->next_column == 1) {
           CBdata->next_column= 0;
           if (CBdata->num_rows > CBdata->next_row+1 )
                   CBdata->next_row++;
            else
                   CBdata->next_row= 0;
      ENDif
   ENDelse
 ENDif
}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void find_ers_hor(  struct mod_popinfo  *mod )

{
  struct horizon_info *curr;
  int cnt;


   curr=  mod->chorptr;
   ErsHorizonGetSegments(  mod->pikrec, curr->hor_name, &cnt, curr->segary);
   curr->horptr= curr->segary[curr->cseg];

   PR_SetCurrentHorizon(mod->pikrec, curr->segary[curr->cseg] );

}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void set_opm_to_color(  struct mod_popinfo  *mod )

{
  WidgetList child;
  Cardinal   num_child;
  Widget     cas;
  long       i;
  char *target_str;

  XtVaGetValues( mod->mwig[COLOP].opm.menuw, XmNchildren,    &child,
                                             XmNnumChildren, &num_child, NULL);


  for (i=0; (i<num_child); i++ ) {
      if ( XtClass(child[i]) == xmCascadeButtonGadgetClass )
               cas= child[i];
  ENDloop

  if (mod->chorptr)
      set_label( cas, mod->chorptr->col_str );
  else
      set_label( cas, mod->curr_color );

}



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void show_current(   struct mod_popinfo  *mod,
                     char                *horstr )

{

  long horlen;
  char wkstr[150], wkstr1[150];



  if (horstr)
      horlen= strlen(horstr);
  else
      horlen= 0;
  
  if (horlen > 0) {
     if ( (strcmp (mod->last_hor_name, mod->chorptr->hor_name) != 0 ) OR
           (mod->last_cseg != mod->chorptr->cseg) ) {

         mod->last_cseg= mod->chorptr->cseg;
         strcpy (mod->last_hor_name, mod->chorptr->hor_name);
         sprintf(wkstr, "Horizon:    %s", mod->chorptr->hor_name);
         sprintf(wkstr1,"Segment\nTotal:  %d", mod->chorptr->num_segs);
         find_ers_hor(mod);
         choose_trans( mod);
         /*
          * this needs only happen on the current segment
          */
         ErsHorizonChangeAttributes(mod->pikrec, 
                                    mod->chorptr->segary[mod->chorptr->cseg],
                                    mod->chorptr->hor_name, 
                                    mod->chorptr->col_str,
                                    ACTIVE_LINEW );
  

         if (mod->chorptr->num_segs > 1) {
             XtManageChild( mod->mwig[SEGSCL].w );
             XtVaSetValues(  mod->mwig[SEGSCL].w, 
                                  XmNmaximum, mod->chorptr->num_segs,
                                  XmNvalue,   mod->chorptr->cseg+1,   NULL );
         ENDif
         else
             XtUnmanageChild( mod->mwig[SEGSCL].w );
  
  
         set_label(mod->chor_lab, wkstr);
         wproc_setsen( mod->mwig, True, 1, ENDSEG );
         change_deltyp( mod, delete_hor);
     ENDif
  ENDif
  else {
       strcpy(wkstr, "Horizon:    NONE");
       strcpy(wkstr1, "Segment\nTotal:  NONE");
       XtUnmanageChild( mod->mwig[SEGSCL].w );
       wproc_setsen( mod->mwig, False, 1,  ENDSEG );
         change_deltyp( mod, delete_none);
       XtUninstallTranslations( I_gphWidget(mod->image) );
       set_label(mod->chor_lab, wkstr);
       mod->last_hor_name[0]= '\0';
  ENDif
  set_opm_to_color( mod);



}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static struct horizon_info *find_current( struct horizon_info *hor_head,
                                          long                row )

{
  struct horizon_info *p;
  Boolean found= False;

  for (p= hor_head; ( (p!=NULL) && (!found) ); ) {
     if (row == p->hor_num-1) 
              found= True;
     else
              p= p->nxt;
  ENDloop

  return (p);


}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static struct horizon_info *find_current_bystr( struct horizon_info *hor_head,
                                                char                *horstr )

{
  struct horizon_info *p;
  Boolean found= False;

  for (p= hor_head; ( (p!=NULL) && (!found) ); ) {
     if (strcmp( horstr, p->hor_name) == 0 ) 
              found= True;
     else
              p= p->nxt;
  ENDloop

  return (p);
}



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static char *which_colstr(  struct mod_popinfo  *mod )

{  return (mod->curr_color); }


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void del_ers_hor_and_segs(  struct horizon_info     *del,
                            PR_        *pikrec )

{

  int i, cnt;

   /*
    * we are going to have to loop to do this but not till we support segs.
    */
     ErsHorizonGetSegments(  pikrec, del->hor_name, &cnt, del->segary);

     ErsHorizonDestroy( pikrec, del->segary );
}



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void mod_delhor( struct mod_popinfo  *mod )

{


  struct horizon_info *p, *q, *r;
  Boolean found= False;
  int delrow, i;


  delrow= mod->chorptr->hor_num-1;

  if (mod->chorptr == mod->hor_head) {
       mod->hor_head= mod->chorptr->nxt;
       del_ers_hor_and_segs(  mod->chorptr, mod->pikrec);
       free (mod->chorptr);
       mod->chorptr=  mod->hor_head;
       if (mod->hor_head == NULL) {           /* no more horizons */
             change_deltyp( mod, delete_none);
             XtUnmanageChild( mod->mwig[SEGSCL].w );
             mod->hor_tail= NULL;
       ENDif
       for(p= mod->hor_head, i=1; ( (p!=NULL) ); p->hor_num= i++, p= p->nxt );

  ENDif
  else {
       for (q= p= mod->hor_head, i=1; ( (p!=NULL) );  ) {
             if (p == mod->chorptr) {
                    q->nxt= p->nxt; 
                    if (mod->hor_tail == p) mod->hor_tail= q;
                    r= q;
                    q= p;
                    p= p->nxt;
                    del_ers_hor_and_segs(  mod->chorptr, mod->pikrec);
                    free( mod->chorptr); 
                    mod->chorptr= r;
             ENDif
             else {
                    p->hor_num= i++; 
                    q= p;
                    p= p->nxt;
             ENDelse
             
       ENDloop
  ENDelse
  mod->hor_tot--;


  XbaeMatrixDeleteRows( mod->hor_list, delrow, 1);

  show_current( mod, tstnull(mod->chorptr, mod->chorptr->hor_name)  );

}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void mod_delseg(  struct mod_popinfo  *mod )

{struct horizon_info *chorptr;
 if(mod == NULL) return;
 chorptr= mod->chorptr;

     ErsHorizonGetSegments(  mod->pikrec,        chorptr->hor_name, 
                             &chorptr->num_segs, chorptr->segary);
     if (chorptr->num_segs == 1)
           mod_delhor(mod);
     else {
           ErsHorizonSegmentDestroy(mod->pikrec, 
                                 mod->chorptr->segary[mod->chorptr->cseg] );

           ErsHorizonGetSegments(  mod->pikrec,
                                   chorptr->hor_name, 
                                   &chorptr->num_segs,
                                   chorptr->segary);
           chorptr->cseg= chorptr->num_segs-1;
           PR_SetCurrentHorizon(mod->pikrec, 
                                             chorptr->segary[chorptr->cseg]);
           chorptr->horptr = chorptr->segary[chorptr->cseg];
           /*
            * macro (in mod_pop.h) sets the scale bar to right number of segs
            */
           set_segdisp( mod->mwig[SEGSCL].w, chorptr->num_segs, chorptr->cseg);
     }
}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void add_new_seg( struct mod_popinfo  *mod)


{
 struct horizon_info *chorptr;

  chorptr= mod->chorptr;

/*
  ErsSegmentChangeAttributes( mod->pikrec, 
                              mod->chorptr->segary[chorptr->cseg],
                              chorptr->hor_name, chorptr->col_str,
                              OTHER_LINEW );
*/


  chorptr->horptr = ErsHorizonCreate(mod->pikrec, chorptr->hor_name, 
                               chorptr->col_str, ACTIVE_LINEW, True);

  /* Allocate a named color */
  ErsHorizonSetColor(mod->pikrec, chorptr->horptr);
  /* New horizon will be made the selected one */
  PR_SetCurrentHorizon(mod->pikrec,chorptr->horptr);


  ErsHorizonGetSegments(  mod->pikrec, chorptr->hor_name, 
                          &chorptr->num_segs, chorptr->segary);


  chorptr->cseg= chorptr->num_segs-1;


  ErsSegmentChangeAttributes( mod->pikrec, 
                              mod->chorptr->segary[chorptr->cseg],
                              chorptr->hor_name, chorptr->col_str,
                              ACTIVE_LINEW );

  /*
   * macro (in mod_pop.h) sets the scale bar to right number of segments
   */
  set_segdisp( mod->mwig[SEGSCL].w, chorptr->num_segs, chorptr->cseg);

}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
struct horizon_info *add_new( struct mod_popinfo  *mod,
                              long                 row,
                              char                *horstr,
                              Boolean             cre_ers)

{
  struct horizon_info *p;
  void mod_cb();



  if (mod->hor_head) {
      p= mod->hor_tail->nxt= 
           (struct horizon_info *)calloc(1, sizeof (struct horizon_info) );
      mod->hor_tail= p;
   }
  else
   { p= mod->hor_head= mod->hor_tail=
          (struct horizon_info *)calloc(1, sizeof (struct horizon_info) );
   }

  /*nxtcell(mod); */

  strcpy( p->hor_name, horstr);
  p->hor_num = row+1;
  p->num_segs= 1;
  p->cseg    = 0;
  strcpy(p->col_str, which_colstr( mod) );
  p->cpixel  = mod->curr_idx;
  p->new     = True;
  p->nxt     = NULL;
  mod->hor_tot++;
  mod->chorptr= p;

  if (cre_ers)
   {    p->horptr = ErsHorizonCreate(mod->pikrec, p->hor_name, 
                                     p->col_str, ACTIVE_LINEW, True);
       /* Allocate a named color */
       ErsHorizonSetColor(mod->pikrec, p->horptr);
       /* New horizon will be made the selected one */
       PR_SetCurrentHorizon(mod->pikrec,p->horptr);
   }

  ErsHorizonGetSegments(  mod->pikrec, p->hor_name, 
                          &p->num_segs, p->segary);

  XbaeMatrixSetCellColor( mod->hor_list, mod->row, 1, mod->curr_cell );

  XbaeMatrixSetCell(mod->hor_list, mod->row, 1, which_colstr( mod)  );
  XbaeMatrixSetCell( mod->hor_list, mod->row, 0, horstr);

  mod->target_row= mod->row;
  mod->cb_enabled= False;
  XbaeMatrixAddRows( mod->hor_list, mod->hor_tot, NULL, NULL, NULL, 1);
  /*XbaeMatrixEditCell(  mod->hor_list, mod->target_row, 0); */
  XbaeMatrixEditCell(  mod->hor_list, mod->row, 0);
  mod->cb_enabled= True;

  return (p);
}





/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void enter_cell(Widget              w,
                       struct CB           *udata,
                       XbaeMatrixEnterCellCallbackStruct *CBdata)

{
 struct mod_popinfo  *mod;
 Arg arglist[3];
 long i, n;
 long wconst, horlen;
 String horstr;


 mod= (struct mod_popinfo *)udata->info;

 horstr= XbaeMatrixGetCell(mod->hor_list, CBdata->row, CBdata->column );
 horlen= strlen(horstr);

 if ( mod->chorptr) {
       /*
        * this needs only happen on the current segment
        */
      ErsHorizonChangeAttributes(mod->pikrec, 
                                 mod->chorptr->segary[mod->chorptr->cseg],
                                 mod->chorptr->hor_name, mod->chorptr->col_str,
                                 OTHER_LINEW );
 ENDif


  mod->chorptr= find_current( mod->hor_head, CBdata->row );

  if (mod->chorptr) {
       change_deltyp( mod, delete_hor);
  ENDif

  mod->row=  CBdata->row;
  mod->col=  CBdata->column;



  show_current( mod, horstr);


}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void change_deltyp(  struct mod_popinfo  *mod,
                     enum   delete_types deltyp )
{

    mod->deltyp= deltyp;

    switch (deltyp) {

       case delete_hor:
             XtSetSensitive(mod->delhor, True);
             XtSetSensitive(mod->delseg, True);
             mod->deltyp= delete_hor;
             break;

       case delete_seg:
             XtSetSensitive(mod->delhor, True);
             XtSetSensitive(mod->delseg, True);
             mod->deltyp= delete_seg;
             break;

       case delete_none:
             XtSetSensitive(mod->delseg, False);
             XtSetSensitive(mod->delhor, False);
             mod->deltyp= delete_none;
             break;

    } /* END switch */


}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void leave_cell(Widget              w,
                       struct CB           *udata,
                       XbaeMatrixLeaveCellCallbackStruct *CBdata)
{
 struct mod_popinfo  *mod;
 Arg arglist[3];
 long i, n;
 long wconst, horlen;
 String horstr;
 static char retstr[100];
 static String strrows[2]= { "", "nocolor"};
 Boolean add_a_new_row= False;

  mod= (struct mod_popinfo *)udata->info;

  horstr= CBdata->value;
  horlen= strlen(horstr);

  /*
   * if we should add the first horizon
   */
  if ( (mod->hor_head == NULL) AND ( horlen > 0) ) {
       mod->chorptr= add_new( mod, mod->row, horstr, True);
       add_a_new_row= True;
       CBdata->doit= False;
       change_deltyp( mod, delete_hor);
  ENDif
  /*
   * if we should add a new horizon
   */
  else if ( (mod->row == mod->hor_tot) AND ( horlen > 0) ){
       if (!mod->chorptr) mod->chorptr= add_new( mod, mod->row, horstr, True);
       add_a_new_row= True;
       CBdata->doit= False;
  ENDif
  /*
   * if the user is editing the name of a current horizon
   * use new name or put the old name back
   */
  else if (mod->row < mod->hor_tot) {
         if ( horlen == 0) {
              CBdata->value=  mod->chorptr->hor_name; 
         ENDif
         else if (strcmp(  mod->chorptr->hor_name, horstr) != 0 ) {
               if (!find_current_bystr( mod->hor_head, horstr)) {
                    strcpy(  mod->chorptr->hor_name, horstr);
                    ErsHorizonChangeName(mod->pikrec, mod->chorptr->segary[0],
                                         mod->chorptr->hor_name); 
               ENDif
               else {
                    CBdata->value=  XtNewString( mod->chorptr->hor_name);
               ENDelse
         ENDelse
  ENDif
 

  
  show_current( mod, horstr);

  
}





/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void losing(Widget                     w,
                   struct CB                  *udata,
                   XmTextVerifyCallbackStruct *CBdata)


{
 struct mod_popinfo  *mod;
 static String params= "False";

  mod= (struct mod_popinfo *)udata->info;

 XbaeMatrixCommitEdit(  mod->hor_list, False);


}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void ptr_sel_hor( Widget w,
                  struct mod_popinfo  *mod,
                  ErsHorizon          *horizon )

{
 struct horizon_info *p;

   p= find_current_bystr(mod->hor_head, horizon->horizon_name);
   ErsHorizonMemberList( mod->pikrec,  horizon,
                         &p->num_segs, &p->cseg, p->segary );
   p->cseg--;
   XbaeMatrixEditCell(  mod->hor_list, p->hor_num-1, 0);
   change_deltyp( mod, delete_seg);

}


void ChangePickType( Widget    w,
                     XEvent   *ev,
                     String   *params,
                     Cardinal num_params )
{
 int stat;
 long whichop;
 PR_   *pikrec;
 struct mod_popinfo *mod;


 mod = (ModInfo *) pikrec->user_data;

 puts("ChangePickType");

 if (num_params == 1) {
     stat= sscanf( params[0], "%d", &whichop );
     if (stat != EOF) {

         switch (whichop) {
              case 1 : XmToggleButtonSetState( mod->mwig[ADDBEG] ); break;
              case 2 : XmToggleButtonSetState( mod->mwig[ADDEND] ); break;
              case 3 : XmToggleButtonSetState( mod->mwig[MOVSEG] ); break;
              case 4 : XmToggleButtonSetState( mod->mwig[INSPCK] ); break;
              case 5 : XmToggleButtonSetState( mod->mwig[SHPCK] ); break;

              default: printf("ChangePickType doesn't know %d\n", whichop); 
                       break;
         } /* End switch */
     } /* End if */
     else
       printf("Parameter passwd to ChangePickType must be an int\n");
 } /* End if */
 else 
    printf("Wrong number of parameters passed to ChangePickType\n");


}
                    


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void mod_cb(Widget w,
             struct CB    *udata,
             XmAnyCallbackStruct *CBdata )

  
 /*
  * this function is the only callback.  It calls all the other functions
  * by determining the callback type.
  */


{
 struct mod_popinfo  *mod;


  mod= (struct mod_popinfo *)udata->info;
  if (udata->wconst==WDESTROY) 
  {
    puts("mod free");
    XFreeCursor( XtDisplay(w), mod->watch_cur );
    mod_clean(mod, True);
    free( mod );

  }
  else 
  {
    if (mod->cb_enabled)  {
     switch (CBdata->reason) {

           case XmCR_ACTIVATE: mod_pushcb(w, udata,
                                    (XmPushButtonCallbackStruct *)CBdata);
                                break;

           case XmCR_MAP:    map(w, udata, CBdata );
                                break;

           case XmCR_OK:  puts("in mod CR_OK");
                          break;

           case XmCR_VALUE_CHANGED:  tog(w, udata, 
                                     (XmToggleButtonCallbackStruct *)CBdata);
                          break;

           case XbaeLeaveCellReason:  leave_cell(w, udata, 
                                 (XbaeMatrixLeaveCellCallbackStruct* )CBdata);
                          break;

           case XmCR_LOSING_FOCUS : losing(w, udata, 
                                 (XmTextVerifyCallbackStruct* )CBdata);
                          break;

           case XbaeEnterCellReason:  enter_cell(w, udata,
                                 (XbaeMatrixEnterCellCallbackStruct* )CBdata);
                          break;

           case XbaeTraverseCellReason :  trav_cell(w, udata,
                             (XbaeMatrixTraverseCellCallbackStruct*) CBdata);
                          break;
     ENDswitch
    ENDif
  ENDelse


}
