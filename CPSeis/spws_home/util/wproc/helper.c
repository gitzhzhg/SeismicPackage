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
/*#include "cenv.h"*/
#include "wproc.h"
#include <ctype.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <stdio.h>


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void manage_msg( Widget w,
                        Widget txt,
                        char   strt[],
                        char   str[] )

/* 
 * manage the help popup, and display the string.
 */

{

 if (txt)
   {
     if (strt) 
       {
	 XmTextSetString( txt, strt);
	 XmTextInsert( txt, strlen(strt), str );
       }
     else
       {
	 XmTextSetString( txt, str);
       }
   } /* end if txt */
   
 if (w) 
   {
     XtManageChild (w); 
      /*
       * the following line is here because if the popup is unmanaged when its
       * parent is unmanaged for some reason the widget will not remanage. 
       * Therefore I got radical and mapped the shell.
       */
     XMapWindow(  XtDisplay(w), XtWindow(XtParent(w)) );
      /*
       * Always on top.  ehs 29apr99
       */
     XRaiseWindow(XtDisplay(w), XtWindow(XtParent(w)));
   } /* end if w */

}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void addto_msg( Widget w,
                       Widget txt,
                       char   str[] )

/* 
 * add to the displayed help
 */

{
 if ((txt) && (str)) {
      XmTextInsert( txt, XmTextGetLastPosition(txt), str );
 }
}




int get_mhelp_index( Widget w, 
                     HelpCtx hctx)
{
  Boolean found;
  int i;

  for(i= 0, found= False; ( (i<hctx->num_mhelp) && (!found) ); i++)
            if  ( hctx->mhelp_ary[i].twin == w) found= True;

  i--;
  if (!found) {
  /*   printf("Ctx: Mouse help: Window widget not registered: %x\n", w); */
     i= -1;
  }

  return (i);

}





#define OVER_TITLE_NAME "over.title"
#define HELP_TITLE_NAME "help.title"
#define ATTNSTR "ATTNHELP:"

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void init_help(HelpCtx hctx)
{
 long stat;
 char *str_type;
 XrmValue  helptext;

 /*
  * PART 1 - initialize if necessary
  */

 if ( (!*hctx->dbase) && (hctx->delay_init) ) {
     if (strlen(hctx->dbfile) > 0)
           *hctx->dbase= XrmGetFileDatabase( hctx->dbfile );
     if (!*hctx->dbase) {
             printf("HelpCtx: Help file not found or not accessible\n" );
             printf("HelpCtx: Filename: %s\n", hctx->dbfile);
             *hctx->dbase= XrmGetStringDatabase("");
     }
     if  (*hctx->dbase)
       {
	 stat = XrmGetResource( *hctx->dbase, OVER_TITLE_NAME,
				"Help.Data", &str_type, &helptext );
	 if (stat) ctxh_set( helptext.addr, &hctx->over_t );
	 
	 stat = XrmGetResource( *hctx->dbase, HELP_TITLE_NAME,
				"Help.Data", &str_type, &helptext );
	 if (stat) ctxh_set( helptext.addr, &hctx->ctx_t );
       }
     
 }

}



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
Boolean display_help( char *wname,
                      struct CB *showhelp,
                      long      helptype,
                      Widget    ev_widget,
                      Widget    passed_outwig )

/*
 *     do the database initialization, determine type of help,
 *     and display the help.  This routine may be called by helper(),
 *     or by one of the help macros, or by itself recursivly.
 */



{
 char                res[400]; /* increased dim from 200 to 400, ehs 24mar99 */
 char                res2[400];/* extra string to hold \n        RwL 10-6-99 */
 int                 ii, jj;   /* counters for res/res2 operations           */
 char                *str_type, *help_title, *hlpptr;
 XrmValue            helptext;
 long                stat, len, i=0;
 Widget              outwig;
 struct HELPCTX     *helpctx;
 char                teststr[20];
 static long         attlen= sizeof(ATTNSTR)-1;
 char                wstr[400];
 Widget              popbox;
 char                *str;
 long                recursive_limit;
 Boolean             retval= True;


 helpctx= (struct HELPCTX *)showhelp->fldptr;

 init_help(helpctx);


 /*
  * PART 2 - set parameters for type of help
  */

 switch (helptype) {
     case HELP_DSP_MANOVER :
                        sprintf(res, "overview.%s", wname);
                        outwig=  helpctx->helptext_ov;
                        help_title= helpctx->over_t;
                        popbox= showhelp->popbox;
                        recursive_limit= HELP_ANY;
                        break;

     case HELP_DSP_MOREMAN :
                        sprintf(res, "overview.%s", wname);
                        outwig=  helpctx->helptext_ov;
                        help_title= NULL;
                        popbox= showhelp->popbox;
                        recursive_limit= HELP_ANY;
                        break;

     case HELP_DSP_AUTOOVER :
                        sprintf(res, "auto_over.%s", wname);
                        outwig=  helpctx->helptext_ov;
                        help_title= helpctx->over_t;
                        popbox= showhelp->popbox;
                        recursive_limit= HELP_ANY;
                        break;

     case HELP_DSP_MOREAUTO :
                        sprintf(res, "auto_over.%s", wname);
                        outwig=  helpctx->helptext_ov;
                        help_title= NULL;
                        popbox= showhelp->popbox;
                        recursive_limit= HELP_ANY;
                        break;

     case HELP_DSPCTX :
                        sprintf(res, "help.%s", wname);
                        outwig=  helpctx->helptext;
                        help_title= helpctx->ctx_t;
                        popbox= showhelp->popbox;
                        recursive_limit= HELP_ANY;
                        break;
     case HELP_MHELP :
                        i= get_mhelp_index(ev_widget, helpctx);
                        if (i>-1) {
                            str=  helpctx->mhelp_ary[i].tokenstr;
                            outwig=  helpctx->mhelp_ary[i].outtext;
                        }
                        else {
                            str=  " ";
                            outwig= NULL;
                        }
                        sprintf(res, "mouse.%s.%s", wname, 
                                       helpctx->mhelp_ary[i].tokenstr);
                        help_title= NULL;
                        popbox= NULL;
                        recursive_limit= HELP_ANY;
                        break;

     case HELP_ANY :    /* only enter this case when being call recursivly */
     default:
                        strcpy(res, wname);
                        outwig=  passed_outwig;
                        if ( outwig ==  helpctx->helptext ) {
                               help_title= helpctx->ctx_t;
                               popbox= showhelp->popbox;
                        }
                        else if (outwig ==  helpctx->helptext_ov ) {
                               help_title= helpctx->over_t;
                               popbox= showhelp->popbox;
                        }
                        else {               /* else for mouse help */
                               help_title= NULL;
                               popbox= NULL;
                        }

                        for(i=0, len= strlen(res); (i<len); i++ ) 
                                 if (res[i] == '*') res[i]= '.';
                        recursive_limit= helptype+1;
                        break;
 }

 /*
  * PART 3 - display the help or an error message if there is a problem.
  */

 if (*helpctx->dbase) 
   {
     stat = XrmGetResource( *helpctx->dbase, res, "Help.Data", 
                            &str_type, &helptext );
     if (!stat) 
       {
         if (helptype == HELP_MHELP) 
	   {
	     sprintf(res, "No mouse help found for - %s.%s", 
		     wname, helpctx->mhelp_ary[i].tokenstr );
	     manage_msg( popbox, outwig, help_title, res);
	   }
         else if ( (helptype != HELP_DSP_MOREMAN) &&
                   (helptype != HELP_DSP_MOREAUTO) ) 
	   {
	     /* Following loop breaks wname at .'s to improve readability */
	     /* of long names                                             */
	       jj = 0;
	       for (ii=0; ii<strlen(wname)+1; ii++)
		 {
		   res2[ii + jj] = wname[ii];
		   if ((ii > 60*(jj+1)) && (wname[ii] == '.'))
		     {
		       jj++;
		       res2[ii + jj] = '\n';
		     }
		 }
	       /*sprintf(res, "No Help found for... \n\n%s", wname );*/
	       sprintf(res, "No Help found for... \n\n%s", res2 );
               manage_msg( popbox, outwig, help_title, res);
	   }
         retval= False;
       }
     else 
       {
	 hlpptr= (char *)(helptext.addr);
	 strncpy( teststr, hlpptr, attlen );
	 teststr[attlen] = '\0';
	 if (strcmp( teststr, ATTNSTR) == 0) 
	   {
	     hlpptr+= attlen;
	     for(; ( (isspace(*hlpptr)) && (*hlpptr != '\0') ); hlpptr++);
	     if (strlen( hlpptr) == 0) 
	       {
		 sprintf(res, "No Help found for - %s", wname );
		 manage_msg( popbox, outwig, help_title,  res );
	       }
	     else 
	       {
		 strcpy( wstr, hlpptr );
		 if (recursive_limit < 20)
		   display_help( wstr, showhelp, recursive_limit, 
				 ev_widget, outwig); 
		 else
		   manage_msg( popbox, outwig, help_title,  
			  "No help found- ATTNHELP exceeds recursive limits!");
	       } /* END else */
	   } /* END if */
	 else if ( (helptype == HELP_DSP_AUTOOVER) ||
		   (helptype == HELP_DSP_MANOVER) )
	   {
	     char new_wstr[1000];
	     Boolean retstat;
	     manage_msg( popbox, outwig, help_title, helptext.addr);
		  /*
		   * look for another one
		   */
              
	     for (i= 1, retstat= True;(retstat); i++  ) 
	       {
		 sprintf( new_wstr, "%s.%1d", wname, i);
		 if (helptype == HELP_DSP_MANOVER) {
		   retstat= display_help( new_wstr, showhelp, 
					  HELP_DSP_MOREMAN, NULL, NULL);
		 }
		 else 
		   {
		     retstat= display_help( new_wstr, showhelp, 
                                            HELP_DSP_MOREAUTO, NULL, NULL);
		   }
	       }

	   } /* END else if */
	 else if ( (helptype == HELP_DSP_MOREMAN)  ||
                   (helptype == HELP_DSP_MOREAUTO)    ) 
	   {
	     addto_msg( popbox, outwig, helptext.addr);
	   } /* END else if */
	 else 
	   {
	     manage_msg( popbox, outwig, help_title, helptext.addr);
	   } /* END else */
	 retval=True;
	      
       }
   }
 else 
   {
     manage_msg( popbox, outwig, help_title,
		 "Help is not enabled!");
     retval=False;
   }

 return (retval);

}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void helper( Widget w,
             struct CB   *showhelp,
             void   *stuff)

/*
 * callback or event handler that does preprocessing and if the user
 * wants help then it call display_help.  This routine is called on any
 * help callback, any enter window event handler, any focus callback 
 * event handler, or any activate callback.
 */

{
 XFocusChangeEvent   *ev;
 XmAnyCallbackStruct *anycb;
 char wstr[400];
 int i;
 struct HELPCTX     *helpctx;

 /*
  * if we are an event handler
  */
 if (showhelp->info) {
      ev= (XFocusChangeEvent *)stuff;
      switch (ev->type) {
        case EnterNotify: 
               helpctx= (struct HELPCTX *)showhelp->fldptr;
               helpctx->curr_mouse_widget= w;
        case FocusIn: 
               if (showhelp->wconst == HELP_M_EVENT) {
                      make_wtree( w, wstr, wprocALL );
                      display_help(wstr, showhelp, HELP_MHELP, w, NULL );
               } /* End if */
               else {
                  if (XtIsManaged(showhelp->popbox)) {
                      make_wtree( w, wstr, wprocALL );
                      switch( showhelp->wconst) {
                             case HELP_EVENT:
                                  display_help(wstr, showhelp, 
                                                  HELP_DSPCTX, w, NULL );
                                  break;
                             case HELP_OV_EVENT:
                                  display_help(wstr, showhelp, 
                                                  HELP_DSP_AUTOOVER, w, NULL );
                                  break;
                      } /* END switch */
                  } /* END if */
               } /* end else */
               break;
        case UnmapNotify: 
                  XtUnmanageChild(showhelp->popbox);
               break;
        case LeaveNotify: 
                  if ( ((XCrossingEvent *)ev)->detail != NotifyInferior) {
                     helpctx= (struct HELPCTX *)showhelp->fldptr;
                     i= get_mhelp_index( w, helpctx);
                     if (i>-1) {
                        if (helpctx->mhelp_blankstr)
                              show_msg( helpctx->mhelp_ary[i].outtext, 
                                          helpctx->mhelp_blankstr );
                        else
                              show_msg( helpctx->mhelp_ary[i].outtext, "" );
                     } /* end if (i>-1) */
                  } /* end if ev */
               break;
      } /* END switch */
 } /* End if */
 /*
  * else we are a callback
  */
 else {
     anycb= (XmAnyCallbackStruct *)stuff;
     switch (anycb->reason)
       {
       case XmCR_ACTIVATE:
       case XmCR_HELP:
	 make_wtree( w, wstr, wprocALL );
	 display_help(wstr, showhelp, HELP_DSPCTX, w, NULL );
	 if (showhelp->wconst == HELP_REMOVE)
	   {
	     XtUnmanageChild( showhelp->popbox );
	   }
	 break;
       } /* END switch */
 } /* End else */


}



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void destroy_help( Widget w,
                   struct CB   *showhelp,
                   void   *stuff)

/*
 * called when the help widget tree is destroyed.  It frees all allocated
 * memory
 */

{
  HelpCtx hctx;
  int i;

  hctx= (HelpCtx)showhelp->fldptr;
  showhelp->fldptr= NULL;
  if (hctx) {
      if (hctx->over_t != NULL)          free (hctx->over_t);
      if (hctx->ctx_t != NULL)           free (hctx->ctx_t);
      if (hctx->mhelp_blankstr != NULL)  free (hctx->mhelp_blankstr);

      if ( hctx->mhelp_ary)  {
           for(i=0; (i<hctx->num_mhelp); i++) 
                if (hctx->mhelp_ary[i].tokenstr) 
                      free (hctx->mhelp_ary[i].tokenstr);
           free (hctx->mhelp_ary);
      }
      XrmDestroyDatabase(*hctx->dbase);
      free (hctx);
  }
}
