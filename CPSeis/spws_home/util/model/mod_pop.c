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
 * Name        : mod_pop
 * File        : mod_pop.c
 * Executable  : -
 * Author      : Trey Roby
 * Date        : 10/1/92
 *
 * CHANGES:
 */

/* -------------------------------------------------------------------------*/
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/Frame.h>
#include "file_choice.h"
#include "matrix.h"
#include "cenv.h"
#include "mod_pop.h"
#include "image.h"
#include "tfio.h"


extern Widget pickfile();
extern mod_cb();
extern mod_sclcb();
extern mod_shpt();
extern Widget PointInfoGUI();
extern void PointInfoShow();
extern setcurr();
extern dotextcb();
extern dofilecb();
extern long mute_filein();
extern PR_ *mod_init();
extern ptr_sel_hor();
extern long model_get();
extern void ChangePickType();
extern void PickComponentCB();
extern void SaveAsCB();



void mod_segctl(Widget W, ModInfo *modgui, caddr_t b);
/*
static void ModValid(Widget W, caddr_t client_d , caddr_t call_d);
*/

#define TIT_CTLSTR "Model Control Panel"
#define TIT_INPSTR "Model Picking"

/*----------------------------------------------------------*/
static char add_to_end_trans[] =
 "!Shift<Btn1Down>:      PointStartMove(m) \n\
        <Btn1Motion>:    PointMove(m) \n\
        <Btn1Up>:        PointEndMove(m) \n\
  !Ctrl <Btn1Down>:      HorizonSelect() \n\
  !Ctrl <Btn2Down>:      InitAreaSelection() \n\
        <Btn2Motion>:    ExtendAreaSelection() \n\
        <Btn2Up>:        EndAreaSelection() \n\
  None<Btn1Down>:        PointStartMove(e) \n\
  None<Btn2Down>:        PointDelete()  \n\
  <Key>1:                ChangePickType(1) \n\
  <Key>2:                ChangePickType(2) \n\
  <Key>3:                ChangePickType(3) \n\
  <Key>4:                ChangePickType(4) \n\
  <Key>5:                ChangePickType(5)";

static char add_to_beg_trans[] =
 "!Shift<Btn1Down>:      PointStartMove(m) \n\
        <Btn1Motion>:    PointMove(m) \n\
        <Btn1Up>:        PointEndMove(m) \n\
  !Ctrl <Btn1Down>:      HorizonSelect() \n\
  !Ctrl <Btn2Down>:      InitAreaSelection() \n\
        <Btn2Motion>:    ExtendAreaSelection() \n\
        <Btn2Up>:        EndAreaSelection() \n\
  None<Btn1Down>:        PointStartMove(b) \n\
  None<Btn2Down>:        PointDelete()  \n\
  <Key>1:                ChangePickType(1) \n\
  <Key>2:                ChangePickType(2) \n\
  <Key>3:                ChangePickType(3) \n\
  <Key>4:                ChangePickType(4) \n\
  <Key>5:                ChangePickType(5)";

static char auto_pck_trans[] =
 "!Ctrl <Btn2Down>:      InitAreaSelection() \n\
        <Btn2Motion>:    ExtendAreaSelection() \n\
        <Btn2Up>:        EndAreaSelection() \n\
  None<Btn1Down>:        AutoPick() \n\
  None<Btn2Down>:        PointDelete()  \n\
  <Key>1:                ChangePickType(1) \n\
  <Key>2:                ChangePickType(2) \n\
  <Key>3:                ChangePickType(3) \n\
  <Key>4:                ChangePickType(4) \n\
  <Key>5:                ChangePickType(5)";

static char ins_pt_trans[] =
 "!Shift<Btn1Down>:      PointStartMove(m) \n\
        <Btn1Motion>:    PointMove(m) \n\
        <Btn1Up>:        PointEndMove(m) \n\
  !Ctrl <Btn1Down>:      HorizonSelect() \n\
  !Ctrl <Btn2Down>:      InitAreaSelection() \n\
        <Btn2Motion>:    ExtendAreaSelection() \n\
        <Btn2Up>:        EndAreaSelection() \n\
  None<Btn1Down>:        PointStartMove(i) \n\
  None<Btn2Down>:        PointDelete()  \n\
  <Key>1:                ChangePickType(1) \n\
  <Key>2:                ChangePickType(2) \n\
  <Key>3:                ChangePickType(3) \n\
  <Key>4:                ChangePickType(4) \n\
  <Key>5:                ChangePickType(5)";

static char seg_move_trans[] =
 "!<Btn1Down>:           HorizonStartMove() \n\
  !<Btn1Motion>:         HorizonMove() \n\
  !<Btn1Up>:             HorizonEndMove()\n\
  None<Btn2Down>:        PointDelete()  \n\
  <Key>1:                ChangePickType(1) \n\
  <Key>2:                ChangePickType(2) \n\
  <Key>3:                ChangePickType(3) \n\
  <Key>4:                ChangePickType(4) \n\
  <Key>5:                ChangePickType(5)";

static char pt_info_trans[] =
 "!<Btn1Down>:           PointSelect() \n\
  !Ctrl <Btn1Down>:      HorizonSelect() \n\
  None<Btn2Down>:        PointDelete()  \n\
  <Key>1:                ChangePickType(1) \n\
  <Key>2:                ChangePickType(2) \n\
  <Key>3:                ChangePickType(3) \n\
  <Key>4:                ChangePickType(4) \n\
  <Key>5:                ChangePickType(5)";


/*----------------------------------------------------------*/
#if 0
static char add_to_end_trans[] =
 "!Shift<Btn1Down>:      PointStartMove(m) \n\
        <Btn1Motion>:    PointMove(m) \n\
        <Btn1Up>:        PointEndMove(m) \n\
  !Ctrl Shift<Btn1Down>: HorizonSelect() \n\
  !Shift<Btn2Down>:      InitAreaSelection() \n\
        <Btn2Motion>:    ExtendAreaSelection() \n\
        <Btn2Up>:        EndAreaSelection() \n\
  None<Btn1Down>:        PointStartMove(e) \n\
      <Btn1Motion>:      PointMove(e) \n\
      <Btn1Up>:          PointEndMove(e) \n\
  None<Btn2Down>:        PointDelete()";

static char add_to_beg_trans[] =
 "!Shift<Btn1Down>:      PointStartMove(m) \n\
        <Btn1Motion>:    PointMove(m) \n\
        <Btn1Up>:        PointEndMove(m) \n\
  !Ctrl Shift<Btn1Down>: HorizonSelect() \n\
  !Shift<Btn2Down>:      InitAreaSelection() \n\
        <Btn2Motion>:    ExtendAreaSelection() \n\
        <Btn2Up>:        EndAreaSelection() \n\
  None<Btn1Down>:        PointStartMove(b) \n\
      <Btn1Motion>:      PointMove(b) \n\
      <Btn1Up>:          PointEndMove(b) \n\
  None<Btn2Down>:        PointDelete()";

static char auto_pck_trans[] =
 "!Shift<Btn2Down>:      InitAreaSelection() \n\
        <Btn2Motion>:    ExtendAreaSelection() \n\
        <Btn2Up>:        EndAreaSelection() \n\
  None<Btn1Down>:        AutoPick() \n\
  None<Btn2Down>:        PointDelete()";

static char ins_pt_trans[] =
 "!Shift<Btn1Down>:      PointStartMove(m) \n\
  <Btn1Motion>:          PointMove(m) \n\
  <Btn1Up>:              PointEndMove(m) \n\
  !Ctrl Shift<Btn1Down>: HorizonSelect() \n\
  !Shift<Btn2Down>:      InitAreaSelection() \n\
        <Btn2Motion>:    ExtendAreaSelection() \n\
        <Btn2Up>:        EndAreaSelection() \n\
  None<Btn1Down>:        PointStartMove(i) \n\
      <Btn1Motion>:      PointMove(i) \n\
      <Btn1Up>:          PointEndMove(i) \n\
  None<Btn2Down>:        PointDelete()";

static char seg_move_trans[] =
 "!<Btn1Down>:           HorizonStartMove() \n\
   <Btn1Motion>:         HorizonMove() \n\
   <Btn1Up>:             HorizonEndMove()\n\
  None<Btn2Down>:        PointDelete()";

static char pt_info_trans[] =
 "!<Btn1Down>:           PointSelect() \n\
  !Ctrl Shift<Btn1Down>: HorizonSelect()";


static char no_trans[] = "";

#endif


/*----------------------------------------------------------*/

#if 0

              COMMENTED OUT


static char add_to_end_trans[] =
 "!Shift<Btn1Down>:      PointStartMove() \n\
  <Btn1Motion>:          PointMove() \n\
  <Btn1Up>:              PointEndMove() \n\
  !Ctrl Shift<Btn1Down>: HorizonSelect() \n\
  None<Btn1Down>:        PointInsert(e) \n\
  None<Btn2Down>:        PointDelete()";

static char add_to_beg_trans[] =
 "!Shift<Btn1Down>:      PointStartMove() \n\
  <Btn1Motion>:          PointMove() \n\
  <Btn1Up>:              PointEndMove() \n\
  !Ctrl Shift<Btn1Down>: HorizonSelect() \n\
  None<Btn1Down>:        PointInsert(b) \n\
  None<Btn2Down>:        PointDelete()";

static char auto_pck_trans[] =
 "None<Btn1Down>:        AutoPick() \n\
  None<Btn2Down>:        PointDelete()";

static char ins_pt_trans[] =
 "!Shift<Btn1Down>:      PointStartMove() \n\
  <Btn1Motion>:          PointMove() \n\
  <Btn1Up>:              PointEndMove() \n\
  !Ctrl Shift<Btn1Down>: HorizonSelect() \n\
  None<Btn1Down>:        PointInsert(i) \n\
  None<Btn2Down>:        PointDelete()";

static char seg_move_trans[] =
 "!<Btn1Down>:           HorizonStartMove() \n\
  !<Btn1Motion>:         HorizonMove() \n\
  !<Btn1Up>:             HorizonEndMove()\n\
  None<Btn2Down>:        PointDelete()";

static char pt_info_trans[] =
 "!<Btn1Down>:           PointSelect() \n\
  !Ctrl Shift<Btn1Down>: HorizonSelect()";


static char no_trans[] = "";

#endif

static struct COLOR_SCELL c_cells[NUMCOLS]= {
                      { "red",         "gray90",  True, 0 },
                      { "green",       "gray80",  True, 0 },
                      { "blue",        "gray70",  True, 0 },
                      { "yellow",      "gray60",  True, 0 },
                      { "purple",      "gray50",  True, 0 },
                      { "DarkGreen",   "gray40",  True, 0 },
                      { "NavyBlue",    "gray30",  True, 0 },
                      { "cyan",        "gray22",  True, 0 },
                      { "DarkKhaki",   "gray10",  True, 0 },
                      { "black",       "black",   True, 0 },
                   };



static XtActionsRec actionsTable[] = {
         { "ChangePickType", ChangePickType },
       };

#define BUILDSTR "Building model popup..."




Widget mod_pop( Widget            parent,       /* parent of new widgets */
                struct PlotImage  *image,
                Widget            *modpop,
                Boolean           file_input,
                Widget            help_text,
                HelpCtx           helpctx,     /* let help be displayed */
                void              (*push_func)(),/*routine to call on cancel*/
                void              *push_data,
                void              *model ) /* model can be NULL */


{ 
  /* wmm static Widget cre_mod_pop(); */
  Widget cre_mod_pop();
  struct mod_popinfo  *mod;
  long n;
  Arg arglist[2];

  if (*modpop == NULL)
       *modpop= cre_mod_pop( parent, image, help_text, 
                             helpctx, push_func, push_data);


  n=0;
  XtSetArg( arglist[n], XmNuserData, &mod ); n++;
  XtGetValues( *modpop, arglist, n);
  if(mod == NULL) return;

  mod->input_disp= file_input;

  mod->pikrec= mod_init( mod, mod->tranpars.add_end, model,image );
  if(mod->pikrec == NULL) { return *modpop; }
  PR_HorizonCallback( mod->pikrec, ptr_sel_hor, mod,
                                   NULL,        NULL,
                                   NULL,        NULL);
  PR_PointCallback(mod->pikrec, PointInfoShow, mod->pikrec, 
                                NULL, NULL, NULL, NULL);

  if (model)
   { sync_picking(mod); }

  if (file_input) {
        XtVaSetValues( XtParent(*modpop), XmNtitle, TIT_INPSTR, NULL);
        set_label( mod->mwig[MODPOP].fp.okb, "OK" );
        XtVaSetValues(mod->mwig[MODPOP].w, XmNheight, mod->org_h, NULL  );
        /*XtUnmanageChild( mod->mwig[ENDSEG].w ); */
        XtSetSensitive( mod->endseg,False);
        XtManageChild( mod->mwig[MODPOP].fp.canb );
        XtManageChild( mod->mwig[IFIL].w );
        XtManageChild( mod->keyrc);
        XtVaSetValues( mod->save_pb,
                       XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget,     mod->keyrc,
                       XmNtopOffset,     6,  NULL); 
        XtVaSetValues( mod->comp_pb,
                       XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget,     mod->keyrc,
                       XmNtopOffset,     6,  NULL); 
        XtManageChild( mod->input_form );
        XtVaSetValues( mod->ctl_form,
                       XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget,     mod->input_form, NULL); 
        XtManageChild( *modpop);
        XtUninstallTranslations(I_gphWidget(mod->image) );

  ENDif
  else {
        XtVaSetValues( XtParent(*modpop), XmNtitle, TIT_CTLSTR, NULL);
        XtManageChild( mod->input_form );
        XtVaSetValues( mod->ctl_form,
                       XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget,     mod->input_form, NULL); 

        set_label( mod->mwig[MODPOP].fp.okb, "Remove" );
        XtVaSetValues(mod->mwig[MODPOP].w, 
                         XmNheight, (Dimension)(mod->org_h*(0.70)), NULL);
/*
        XtVaSetValues( mod->ctl_form,  XmNtopAttachment, XmATTACH_FORM, NULL);
        XtUnmanageChild( mod->input_form );
*/
        XtSetSensitive( mod->endseg,True);
      /*  XtManageChild( mod->mwig[ENDSEG].w ); */
        XtManageChild( *modpop);
        XtUnmanageChild( mod->mwig[MODPOP].fp.canb );
        XtUnmanageChild( mod->mwig[IFIL].w );
        XtUnmanageChild( mod->keyrc);
        XtVaSetValues( mod->save_pb,
                       XmNtopAttachment, XmATTACH_FORM,
                       XmNtopOffset,     6,  NULL); 
        XtVaSetValues( mod->comp_pb,
                       XmNtopAttachment, XmATTACH_FORM,
                       XmNtopOffset,     6,  NULL); 
        choose_trans(mod);
  ENDelse
         

  return( *modpop );


}





/* wmm static Widget cre_mod_pop( Widget           parent,  */  /* parent widget */
Widget cre_mod_pop( Widget           parent,    /* parent widget */
                    struct PlotImage *image,
                    Widget           help_text,
                    struct HELPCTX   *helpctx,     /* let help be displayed */
                    void             (*push_func)(), /*routine to call on push*/
                    void              *push_data )

{

  XmString  xstr;
  Arg arglist[12];
  long n, i, j, stat;
  Widget terc, oprc, hrcframe, hrc, fileform, fpop;
  Widget inputsep,ctlsep, tempw, txtw, anw;
  Widget ptyprcframe,ptyprc;
  Widget shptlab,shpttxt;
  struct mod_popinfo  *mod;
  char labstr[100], *tempstr;
  XmString Xmstr;
  Boolean first;
  static short cwids[2]= { 12, 6};
  static char *clab[2]= { "Horizon", "Color"};
  WidgetList child;
  Cardinal   num_child;
  Pixel  line_cells;
  Widget shell;

  Widget ctlhz,ctlhz_pb[4];
  static char *hzpblab[4] = {"NewHorizon", "DelHorizon",
                             "DelSegment", "EndSegment"};
  static char *hzpbnam[4] = {"newhort", "delhor",
                             "delseg", "endseg"};







  /*
   * malloc and initialize the struct to hold all the widgets and the
   * data we are using.  This struct is later freed in a destroy callback
   */

  mod= (struct mod_popinfo *)calloc( 1, sizeof(struct mod_popinfo) );

  mod->push_func= push_func;
  mod->push_data= push_data;
  mod->helpctx  = helpctx;
  mod->model    = NULL;
  mod->pikrec   = NULL;
  mod->outtyp   = LAYER_TYPE;
  strcpy(mod->outfile,"untitled.pck");
  mod->hor_head = NULL;
  mod->hor_tail = NULL;
  mod->hor_tot  = 0;
  mod->image    = image;
  mod->auto_cnt = 1;
  mod->load_in_progress= False;
  mod->cb_enabled= True;
  mod->pick_mode=  MANNO;
  mod->snap_mode=  PEAK;
  mod->help_text=  help_text;
  mod->may_pick=   True;
  mod->Spik_or_Vpik= MODPICK;
  mod->can_destroy_model= True;

  shell = (Widget)XutilGetShell(parent);
  mod->watch_cur= XCreateFontCursor( XtDisplay(shell), XC_watch);
  wprocCursorSet( shell,  mod->watch_cur);
  tempstr= wprocPushMsg(  mod->help_text, BUILDSTR);


  XtAppAddActions( XtWidgetToApplicationContext(shell),
                   actionsTable, XtNumber(actionsTable) );

  mod->tranpars.add_end  = XtParseTranslationTable(add_to_end_trans);
  mod->tranpars.add_beg  = XtParseTranslationTable(add_to_beg_trans);
  mod->tranpars.seg_mov  = XtParseTranslationTable(seg_move_trans);
  mod->tranpars.ins_pt   = XtParseTranslationTable(ins_pt_trans);
  mod->tranpars.show_pt  = XtParseTranslationTable(pt_info_trans);
  mod->tranpars.auto_pck = XtParseTranslationTable(auto_pck_trans);



  /*
   * call the utility routine to create the form popup with the basic buttons
   */
  set_Form_popup( mod->mwig, "modpop", mod_cb, &mod->mcb[MODPOP], 
                  FP_DOOK|FP_DOCANCEL|FP_DOHELP,parent, MODPOP );
  cre_frm_pop( &mod->mwig[MODPOP], True, helpctx);


  XtVaGetValues(mod->mwig[MODPOP].w,  XmNwidth,  &mod->org_w, 
                                      XmNheight, &mod->org_h, NULL  );




  XtVaSetValues(mod->mwig[MODPOP].w,  XmNuserData,  mod, NULL );

  /*
   * add a map callback to the form popup 
   */
  set_CB(mod->mcb, MODPOP, mod, mod->errbox, NULL, TYPE_FPOPUP );
  XtAddCallback( mod->mwig[MODPOP].w, XmNmapCallback,
                        (XtCallbackProc)mod_cb, &mod->mcb[MODPOP]);
  /*
   * create a error dialog and a question dialog widget
   */
  mod->errbox= make_okbox(mod->mwig[MODPOP].w,"errbox", XmDIALOG_ERROR);
  mod->infobox= PointInfoGUI(mod->mwig[MODPOP].w);

/*
  set_CB( mod->mcb, MUTE_Q, mod, NULL, dwin, TYPE_NONE );
  mod->qbox= make_qbox( mod->mwig[CMFORM].w, "mute_q", cmute_cb,
                         NULL, &mod->mcb[MUTE_Q]);
*/

 /*
  * top form with filenames, etc.
  */
  mod->input_form= XtVaCreateManagedWidget("input_form",
                   xmFormWidgetClass, mod->mwig[MODPOP].w, 
                   XmNtopAttachment,    XmATTACH_FORM,
                   XmNrightAttachment,  XmATTACH_FORM,
                   XmNleftAttachment,   XmATTACH_FORM, NULL );

 /*
  * bottom form with most of the controls
  */
  mod->ctl_form= XtVaCreateManagedWidget("ctl_form",
                 xmFormWidgetClass, mod->mwig[MODPOP].w,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        mod->input_form,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     mod->mwig[MODPOP].fp.button_ctl, NULL);



  ctlsep= XtVaCreateManagedWidget("ctlsep",
          xmSeparatorWidgetClass, mod->ctl_form, 
          XmNrightOffset,      5,
          XmNleftOffset,       5,
          XmNbottomOffset,     5,
          XmNrightAttachment,  XmATTACH_FORM,
          XmNleftAttachment,   XmATTACH_FORM,
          XmNbottomAttachment, XmATTACH_FORM, NULL );

 /*
  * manager widgets for all the primitive widgets
  */

  hrcframe= XtVaCreateManagedWidget("hrcframe",
               xmFrameWidgetClass, mod->ctl_form, NULL );

  hrc= XtVaCreateManagedWidget("hrc",
       xmRowColumnWidgetClass, hrcframe, NULL );


 /*
  * CREATE PICK OPTION TOGGLE BUTTONS
  * ptyprcframe & ptyprc = manager widgets for pick options
  * create ptypL, ptyprc and radio buttons in ptyprc for picking
  */
  mod->ptypL= XtVaCreateManagedWidget("ptypL",
              xmLabelWidgetClass,  mod->ctl_form,
              XmNtopAttachment,    XmATTACH_WIDGET,
              XmNtopWidget,        hrcframe,  NULL );

  ptyprcframe= XtVaCreateManagedWidget("ptyprcframe",
               xmFrameWidgetClass, mod->ctl_form,
               XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
               XmNleftWidget,      mod->ptypL,
               XmNtopAttachment,   XmATTACH_WIDGET,
               XmNtopWidget,       mod->ptypL,
               NULL );

  ptyprc= XtCreateManagedWidget( "ptyprc",
          xmRowColumnWidgetClass, ptyprcframe,  NULL, 0);


  set_CB( mod->mcb, ADDBEG, mod, NULL, &mod->pcktyp, TYPE_RADIO);
  mk_togb_cb( mod->mwig, mod->mcb, ptyprc, (XtCallbackProc)mod_cb, 
              &mod->mcb[ADDBEG], TYPE_RADIO, 5,
              ADDBEG, "addbeg", NULL,
              ADDEND, "addend", NULL,
              MOVSEG, "movseg", NULL,
              INSPCK, "inspck", NULL,
              SHPCK,  "shpck",  NULL );

  /*
   * set up the callbacks struct, the file information struct to the
   * text widgets that hold the input and output files.  The callback that
   * is used is a utility routine for handling files.  The set_FI macro 
   * sets a struct that defines how this routine works.
   */




 /*
  ***************************************************************
  *            CREATE THE OPTION MENU FOR COLORS
  * create option menu for color selsecion and the pushbuttons 
  * that go in it.  Allocate the colors and determine how many
  * colors we can get.  Only manage a color pushbutton if
  * we can allocate the color
  */

  tempw=cre_opm(&mod->mwig[COLOP], hrc, "col_op", "colrc", "Color:", COLOP);

  qset_CB( mod->mcb, C0, mod, mod->errbox);
  mk_pshb_cb( mod->mwig, mod->mcb, mod->mwig[COLOP].w,
              (XtCallbackProc)mod_cb, &mod->mcb[C0], 10,
                 C0, "c0",       C1, "c1",
                 C2, "c2",       C3, "c3",
                 C4, "c4",       C5, "c5",
                 C6, "c6",       C7, "c7",
                 C8, "c8",       C9, "c9"  );


  first= True;
  for(i=0,j=0;(i<NUMCOLS); i++) {
           line_cells= alloc_scell( XtDisplay(parent), 
                                            I_gphCmap( image),
                                            &c_cells[i], False,
                                            NULL, &stat );
           if (stat == SCELL_COLOR) {
                    set_label(  mod->mwig[C0+i].w, c_cells[i].cname );
                    XtVaSetValues(  mod->mwig[C0+i].w, 
                                    XmNbackground,  line_cells ,NULL);
                    mod->color_strary[j]= c_cells[i].cname;
                    mod->good_cells[j]= line_cells;
                    mod->good_cells_widx[j++]= C0+i;
           ENDif
           else if (stat == SCELL_GCOLOR) {
                    set_label(  mod->mwig[C0+i].w, c_cells[i].gsname );
                    XtVaSetValues(  mod->mwig[C0+i].w, 
                                    XmNbackground,  line_cells, NULL );
                    mod->color_strary[j]= c_cells[i].gsname;
                    mod->good_cells[j]= line_cells;
                    mod->good_cells_widx[j++]= C0+i;
           ENDif
           else {
               XtUnmanageChild( mod->mwig[C0+i].w );
           ENDelse
  ENDloop
  mod->tot_good_cells= j;
  mod->nxt_good_cell= 0;
  mod->curr_cell= mod->good_cells[0];
  strcpy(mod->curr_color, mod->color_strary[0] );

 /*
  ***************************************************************
  *                         PICKING OPTIONS MENUS
  *                 Create option menus for picking modes
  */

  tempw=cre_opm(&mod->mwig[MODEOP], hrc, "modeop", "moderc", 
                   "Pick:", MODEOP);

  qset_CB( mod->mcb, MANNO, mod, NULL);
  mk_pshb_cb( mod->mwig, mod->mcb, mod->mwig[MODEOP].w,
              (XtCallbackProc)mod_cb, &mod->mcb[MANNO], 3,
                            MANNO,   "manno",       
                            MANWITH, "manwith",       
                            AUTO,    "auto"     );

  mod->snaprc=cre_opm(&mod->mwig[SNAPOP], hrc, "snapop", "snaprc", 
                      "Data:", SNAPOP);

  mk_pshb_cb( mod->mwig, mod->mcb, mod->mwig[SNAPOP].w,
              (XtCallbackProc)mod_cb, &mod->mcb[MANNO], 4,
                            PEAK,   "peak",       
                            TROFF,  "troff",       
                            PTOM,   "ptom",       
                            MTOP,   "mtop"     );

 /*
  ***************************************************************
  *            CREATE SEVERAL ROW COLUMNS AND WIDGETS IN THEM
  * create  hzctlrc
  *
  * hzctlrc is for adding new horizons and changing picking type.
  */

 /*
  * create hzctlrc and push buttons in hzctlrc
  */
  n=0;
  tempw = XmCreatePulldownMenu( hrc, "hzctlrc", arglist, n);
  xstr = (XmString) XmStringCreateLtoR("Segment\nControl:",
         XmSTRING_DEFAULT_CHARSET);
  n=0;
  XtSetArg (arglist[n], XmNsubMenuId, tempw ); n++;
  XtSetArg (arglist[n], XmNlabelString, xstr ); n++;
  mod->hzmenu = XmCreateOptionMenu( hrc, "hzctlop", arglist, n);
  XtManageChild( mod->hzmenu);
  XmStringFree(xstr);

  for(i=0;i<XtNumber(ctlhz_pb);i++)
   {xstr = XmStringCreateSimple(hzpblab[i]);
    ctlhz_pb[i] = XtVaCreateManagedWidget(hzpbnam[i],
                  xmPushButtonWidgetClass, tempw, NULL);
    XmStringFree(xstr);
    XtAddCallback( ctlhz_pb[i], XmNactivateCallback, 
                   (XtCallbackProc)mod_segctl, mod); 
   }
 mod->newhort = ctlhz_pb[0];
 mod->delhor  = ctlhz_pb[1];
 mod->delseg  = ctlhz_pb[2];
 mod->endseg  = ctlhz_pb[3];
 mod->mwig[ENDSEG].w  = ctlhz_pb[3];


 /*******************************************************************/


  set_CB( mod->mcb, SEGSCL, mod, NULL, &mod->cseg, TYPE_NONE);
  set_SCALE( mod->mwig, "segscl", mod_sclcb, &mod->mcb[SEGSCL], 0, 
             mod->ctl_form, TYPE_NONE, NULL, SEGSCL );


 /*
  ***************************************************************
  *            CREATE HORIZON LIST MATRIX WIDGET
  * create the xbaeMatix widget to list our horizons.  Add the callbacks to
  * the widget then also figure out the text field widget id that is uses
  * to do cell editing.  Add a losing focus callback to this widget.
  */

 /*
  * Attach a label widget to the top of the formwidget.
  */
  mod->chor_lab =XtVaCreateManagedWidget("chor_lab",
                 xmLabelWidgetClass,  mod->ctl_form,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        hrcframe,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNleftOffset,       20,  NULL );

  mod->hor_list =XtVaCreateManagedWidget("hor_list",
                 xbaeMatrixWidgetClass, mod->ctl_form,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        mod->chor_lab,
                 XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                 XmNleftWidget,       mod->chor_lab,
                 XmNrightAttachment,  XmATTACH_POSITION,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     ctlsep,
                 XmNcolumns,          2,
                 XmNrows,             1,
                 XmNcolumnWidths,     cwids,
                 XmNcolumnLabels,     clab ,     NULL );

  qset_CB( mod->mcb, HOR_LIST, mod, mod->errbox);
  XtAddCallback( mod->hor_list, XmNenterCellCallback, 
                 (XtCallbackProc)mod_cb, &mod->mcb[HOR_LIST] );
  XtAddCallback( mod->hor_list, XmNleaveCellCallback, 
                 (XtCallbackProc)mod_cb, &mod->mcb[HOR_LIST] );
  XtAddCallback( mod->hor_list, XmNtraverseCellCallback, 
                 (XtCallbackProc)mod_cb, &mod->mcb[HOR_LIST] );

  XtVaGetValues( mod->hor_list, XmNchildren,    &child,
                                XmNnumChildren, &num_child,  NULL);

  for (i=0; (i<num_child); i++ ) {
      if ( XtClass(child[i]) == xmTextFieldWidgetClass )
               mod->mat_tf= child[i];
  ENDloop
  add_HELP( mod->hor_list, helper, helpctx);
  add_HELP( mod->mat_tf, helper, helpctx);

  XtAddCallback( mod->mat_tf, XmNlosingFocusCallback, 
                 (XtCallbackProc)mod_cb,  &mod->mcb[HOR_LIST]);

/*********************************************************************/


 /*
  * Pushbutton to invoke "Save As..." popup
  */
 xstr = XmStringCreateSimple("Save As...");
 mod->save_pb = XtVaCreateManagedWidget("savesasPB",
                xmPushButtonWidgetClass, mod->input_form,
                XmNlabelString,     xstr,
                XmNleftAttachment,  XmATTACH_FORM,
                XmNleftOffset,      20, NULL );
 XmStringFree(xstr);
 XtAddCallback(mod->save_pb,XmNactivateCallback,SaveAsCB,mod);
 /*
  * Pushbutton to invoke popup to select model component
  */
 xstr = XmStringCreateSimple("Model Component...");
 mod->comp_pb = XtVaCreateManagedWidget("componentPB",
                xmPushButtonWidgetClass, mod->input_form,
                XmNlabelString,     xstr,
                XmNleftAttachment,  XmATTACH_WIDGET,
                XmNleftOffset,      5,
                XmNleftWidget,      mod->save_pb, NULL );
 XmStringFree(xstr);
 XtAddCallback(mod->comp_pb,XmNactivateCallback,PickComponentCB,mod);
/*
 inputsep = XtVaCreateManagedWidget("inputsep",
            xmSeparatorWidgetClass, mod->input_form, 
            XmNrightOffset,      5,
            XmNleftOffset,       5,
            XmNbottomOffset,     5,
            XmNrightAttachment,  XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_FORM,
            XmNtopAttachment,    XmATTACH_WIDGET,
            XmNtopWidget,        mod->save_pb, NULL );

 XtVaSetValues( inputsep, 
                 XmNtopAttachment,   XmATTACH_WIDGET,
                 XmNtopWidget,       mod->save_pb, NULL );
*/

 /*
  ***************************************************************
  *                  FILE IO WIDGETS
  * use the convience routine to create popup box.  Set attachments
  * for the lables and text widgets.
  */

 qset_CB( mod->mcb, IFIL, mod, NULL);
 mod->mwig[IFIL].w= XtVaCreateManagedWidget("ifil",
                    fileChoiceWidgetClass, mod->input_form,
                    XmNtopAttachment,   XmATTACH_FORM,
                    XmNtopOffset,       4,
                    XmNleftAttachment,  XmATTACH_FORM,
                    XmNleftOffset,      20,
                    XmNrightAttachment, XmATTACH_FORM,
                    XmNrightOffset,     20,
                    XmNfileTargetAddr,  &mod->infile[0],   NULL );
 XtAddCallback( mod->mwig[IFIL].w, XmNsuccessCallback, 
                (XtCallbackProc)model_get, &mod->mcb[IFIL]); 
/*
 XtAddCallback(mod->mwig[IFIL].w,XmNfileSelCallback,ModValid,NULL);
*/

 wprocFileChoiceRetWidgets( mod->mwig[IFIL].w, &txtw, &anw, NULL, NULL );
 add_HELP( txtw, helper, helpctx);
 add_HELP( anw, helper, helpctx);

 /*
  * Widgets to support Header Key fields
  */
  terc= XtVaCreateManagedWidget("terc",
        xmRowColumnWidgetClass, mod->input_form, 
        XmNtopAttachment,     XmATTACH_WIDGET,
        XmNtopWidget,         mod->mwig[IFIL].w,
        XmNleftAttachment,    XmATTACH_FORM,
        XmNleftOffset,        20 ,   NULL );
  mod->keyrc = terc;

  set_CB( mod->mcb, PHEAD, NULL, mod->errbox, &mod->phead, TYPE_INT);
  set_TEXT(mod->mwig, "phead", setcurr, &mod->fld_info, dotextcb,
                       &mod->mcb[PHEAD], "pheadL", terc,
                       TYPE_INT, &mod->phead,  PHEAD );

  set_CB( mod->mcb, SHEAD, NULL, mod->errbox, &mod->shead, TYPE_INT);
  set_TEXT(mod->mwig, "shead", setcurr, &mod->fld_info, dotextcb,
                       &mod->mcb[SHEAD], "sheadL", terc,
                       TYPE_INT, &mod->shead,  SHEAD );

  set_CB( mod->mcb, THEAD, NULL, mod->errbox, &mod->thead, TYPE_INT);
  set_TEXT(mod->mwig, "thead", setcurr, &mod->fld_info, dotextcb,
                       &mod->mcb[THEAD], "theadL", terc,
                       TYPE_INT, &mod->thead,  THEAD );

 /*
  * Create all the priitive widgets
  */
  create_prim_widgets( mod->mwig, MAX_MOD_WIG, NULL ); 

  XtVaSetValues( mod->mwig[SEGSCL].w, 
                 XmNbottomAttachment,   XmATTACH_WIDGET,
                 XmNbottomWidget,       ctlsep,
                 XmNrightAttachment, XmATTACH_WIDGET,
                 XmNrightWidget,     mod->ptypL, NULL );
  XtUnmanageChild(mod->mwig[SEGSCL].w);
/****************************************************************/


  qset_CB( mod->mcb, WDESTROY, mod, NULL);
  XtAddCallback( mod->mwig[MODPOP].w, XmNdestroyCallback,
                        (XtCallbackProc)mod_cb, &mod->mcb[WDESTROY] );

  XtSetSensitive(  mod->endseg, False );

  XtSetSensitive( mod->snaprc, False );

  change_deltyp( mod, delete_none);

  wprocPrimHelp( mod->mwig, MAX_MOD_WIG, helpctx ); 

 /***************************************************************
  *                          DONE
  */
  wprocCursorNone(shell);
  wprocPopMsg(  mod->help_text, tempstr);

  return (mod->mwig[MODPOP].w ); /* return the new widget */


}

void mod_segctl(Widget W, ModInfo *modgui, caddr_t b)
{
 if(modgui == NULL || W == NULL) return;
 if( W == modgui->newhort)
  {auto_add_new(modgui);
  }
 else if(W == modgui->delhor)
  {mod_delhor(modgui);
  }
 else if(W == modgui->delseg)
  {mod_delseg(modgui);
  }
 else if(W == modgui->endseg)
  {add_new_seg(modgui);
  }
 else return;

}

/*
static void ModValid(Widget W, caddr_t client_d , caddr_t call_d)
{
 wprocFileChoiceValidate(W,True);
 return;
}
*/
