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
#include "cenv.h"
#include "wproc.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>



static String  defres[]= {
    "*autohelp.width:       500",
    "*autohelp.height:      500",
    "*autohelp*button_control.OK.labelString:    Remove",
    "*autohelp.scrtext.topOffset:            5",
    "*autohelp.scrtext.leftOffset:           5",
    "*autohelp.scrtext.rightOffset:          5",
    "*autohelp*button_control.shadowThickness:   0",
    "*autohelp*button_control.rightOffset:       10",
    "*autohelp*button_control.leftOffset:        10",
    "*autohelp*button_control.bottomOffset:      15",
    "*autohelp.paned.topOffset:            5",
    "*autohelp.paned.leftOffset:           5",
    "*autohelp.paned.rightOffset:          5",
    "*autohelp.paned.sashWidth:            80",
    "*autohelp.paned.spacing:              20",
    "*autohelp*scrtext.height:             210",
    "*autohelp*shadowThickness:            2",
NULL };


extern destroy_help();

struct HELPCTX *setup_help( Widget         p,
                            XrmDatabase   *data,
                            char          filename[],
                            char          title_str[])

{
  struct HELPCTX *helpctx;
  int n;
  Arg arglist[15];
  Widget tempw, pane;


   setDefRes( XtDisplay(p), XtName(p), defres);

   helpctx= (struct HELPCTX *)calloc( 1, sizeof(struct HELPCTX) );

   helpctx->over_t= (char *)malloc ( sizeof (OV_TITLE_STR) );
   helpctx->ctx_t= (char *)malloc ( sizeof (CST_TITLE_STR) );
   strcpy( helpctx->over_t, OV_TITLE_STR );
   strcpy( helpctx->ctx_t, CST_TITLE_STR );
   helpctx->mhelp_ary= NULL;
   helpctx->num_mhelp= 0;

   set_Form_popup(helpctx->autohelp, "autohelp", helper, 
                  &helpctx->help_cbdata[HELP_REMOVE], FP_DOOK,p,0 );
   cre_frm_pop( &helpctx->autohelp[0], True, NULL  );
   n=0;
   XtSetArg (arglist[n], XmNtopAttachment, XmATTACH_FORM) ; n++;
   XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM) ; n++;
   XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM) ; n++;
   XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
   XtSetArg (arglist[n], XmNbottomWidget,  
             helpctx->autohelp[0].fp.button_ctl) ; n++;
   XtSetArg (arglist[n], XmNbottomOffset, 10 ); n++;
   pane= XtCreateManagedWidget( "paned", xmPanedWindowWidgetClass,
                                 helpctx->autohelp[0].any.w, 
                                 arglist, n);


   n=0;
   tempw= XtCreateManagedWidget( "scrtext", xmScrolledWindowWidgetClass,
                                 pane, arglist, n);
   n=0;
   XtSetArg (arglist[n], XmNeditMode, XmMULTI_LINE_EDIT) ; n++;
   XtSetArg (arglist[n], XmNeditable, False)             ; n++;
   XtSetArg (arglist[n], XmNshadowThickness, 2)          ; n++;
   XtSetArg (arglist[n], XmNwordWrap, True)              ; n++;
   helpctx->helptext= XtCreateManagedWidget( "helptext", xmTextWidgetClass,
                                              tempw, 
                                              arglist, n);
   XmAddTabGroup( helpctx->autohelp[0].fp.okb);

   n=0;
   tempw= XtCreateManagedWidget( "scrtext1", xmScrolledWindowWidgetClass,
                                 pane, arglist, n);
   n=0;
   XtSetArg (arglist[n], XmNeditMode, XmMULTI_LINE_EDIT) ; n++;
   XtSetArg (arglist[n], XmNeditable, False)             ; n++;
   XtSetArg (arglist[n], XmNshadowThickness, 2)          ; n++;
   XtSetArg (arglist[n], XmNwordWrap, True)              ; n++;
   helpctx->helptext_ov= XtCreateManagedWidget( "helptext_ov", 
                                                xmTextWidgetClass,
                                                tempw, arglist, n);
   XmAddTabGroup( helpctx->autohelp[0].fp.okb);




  set_CB(helpctx->help_cbdata,HELP_EVENT,True,
         helpctx->autohelp[0].any.w,helpctx,TYPE_NONE);

  set_CB(helpctx->help_cbdata,HELP_OV_EVENT,True,
         helpctx->autohelp[0].any.w,helpctx,TYPE_NONE);

  set_CB(helpctx->help_cbdata,HELP_DISP,False, 
         helpctx->autohelp[0].any.w, helpctx,TYPE_NONE);

  set_CB(helpctx->help_cbdata,HELP_REMOVE,False, 
         helpctx->autohelp[0].any.w, helpctx,TYPE_NONE);

  set_CB(helpctx->help_cbdata,HELP_M_EVENT,True,
         helpctx->autohelp[0].any.w,helpctx,TYPE_NONE);

  if (filename) {
      strcpy( helpctx->dbfile, filename);
      helpctx->delay_init= True;
  ENDif
  else
      helpctx->delay_init= False;

  if (data) helpctx->dbase= data;
  else      helpctx->dbase= &helpctx->real_dbase;

  n=0;
  if (title_str) {
      XtSetArg(arglist[n], XmNtitle, title_str ); n++;
  ENDif
  else {
      XtSetArg(arglist[n], XmNtitle, "Help" ); n++;
  ENDelse
  XtSetValues( XtParent( helpctx->autohelp[0].any.w ), arglist, n );


  XtAddEventHandler(  XtParent(helpctx->autohelp[0].any.w), 
                      StructureNotifyMask, False,
                      helper, &helpctx->help_cbdata[HELP_EVENT] );

  XtAddCallback( helpctx->autohelp[0].any.w, XmNdestroyCallback, 
                 (XtCallbackProc)destroy_help, 
                 &helpctx->help_cbdata[HELP_DISP] );

  return (helpctx);
}




void ctxhMouseHelp( HelpCtx hctx,
                    Widget  text,
                    Widget  win,
                    char    *token )
{

 if (hctx->num_mhelp == 0) {
   hctx->mhelp_ary= (MhelpEle*)calloc( 1, sizeof (MhelpEle) );
 }
 else {
   hctx->mhelp_ary= (MhelpEle*)realloc( hctx->mhelp_ary, 
                                  (hctx->num_mhelp+1) * sizeof(MhelpEle) );
 }

 if (hctx->mhelp_blankstr) free(hctx->mhelp_blankstr);
 hctx->mhelp_blankstr= NULL;
 hctx->mhelp_ary[hctx->num_mhelp].twin= win;
 hctx->mhelp_ary[hctx->num_mhelp].outtext= text;
 hctx->mhelp_ary[hctx->num_mhelp].tokenstr= NULL;

 if (token) ctxh_set(token, &hctx->mhelp_ary[hctx->num_mhelp].tokenstr);
 else       ctxh_set(" ", &hctx->mhelp_ary[hctx->num_mhelp].tokenstr);

 hctx->num_mhelp++;


 XtAddEventHandler(win, EnterWindowMask|LeaveWindowMask, False, helper,
                &hctx->help_cbdata[HELP_M_EVENT] );

}


void ctxhMergeHelpLine( HelpCtx hctx,
                        char    *help_text)
{
 XrmDatabase rdb, db;


 /*
  * initialize if necessary
  */
 init_help(hctx);


 if (*hctx->dbase) {
     rdb = XrmGetStringDatabase ( help_text );  /* make empty res database */

       /*
        * Merge them into the Xt database, with lowest precendence
        */
     if ( rdb ) {
#        if (XlibSpecificationRelease>=5)
                  db = *hctx->dbase;
                  XrmCombineDatabase(rdb, &db, FALSE);
#        else
                  XrmMergeDatabases ( *hctx->dbase, &rdb );
                  *hctx->dbase = rdb;
#        endif
     }
  } /* End if */
}


char *ctxhGetTolken( HelpCtx hctx,
                     Widget  win)
{
   int i;
   char *retstr;
  
   if (!hctx) return NULL;

   i= get_mhelp_index(win, hctx);
   if (i>-1) {
      retstr= hctx->mhelp_ary[i].tokenstr;
   }
   else 
      retstr= NULL;

   return retstr;
}


void ctxhChangeTolken( HelpCtx hctx,
                       Widget  win,
                       char    *token )
{
 int i;
 char wstr[400];

 i= get_mhelp_index(win, hctx);

 if (i> -1) {
     ctxh_set(token, &hctx->mhelp_ary[i].tokenstr);
 } /* end if */
 else {
     printf("ctxhChangeTolken: No mouse help enabled for widget %x\n", win);
 } /* end else */
 

 if (win == hctx->curr_mouse_widget) {
     make_wtree( win, wstr, wprocALL );
     display_help(wstr, &hctx->help_cbdata[HELP_M_EVENT], 
                       HELP_MHELP, win, NULL );
 } /* end if */

}
