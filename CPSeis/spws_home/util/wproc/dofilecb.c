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
 * Name        : dofilecb
 * File        : dofilecb.c
 * Executable  : -
 * Author      : Trey Roby
 * Date        : 2/1/92
 *
 * This routine is a catch all file callback that is highly customizable 
 * and with do all major functions  that are desired with specifiying an
 * input or an output file.
 *
*/

/*
 * system headers
 */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>

#ifdef VAXC
#include <unixio.h>	/* when we are VMS headers are a little different */
#else
#include <fcntl.h>	/* these headers for Unix or VMS GNU */
#include <unistd.h>
#endif

/*
 * Xlib Headers
 */


#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
/*#include <Xm/Xm.h> */
#include <X11/Shell.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/Separator.h>
/*#include <Xm/Form.h> */
#include <X11/IntrinsicP.h>
#include <Xm/XmP.h>
#include <Xm/FormP.h>
#include "file_choice_p.h"
#include "exptilde_crou.h"

/*
 * local headers
 */
#include "wproc.h"
/*#include "cenv.h" */

/*
 * list of strings we output
 */
#define REQUIRE_STR     " is required."
#define READ_STR        " does not exist or is not readable."
#define WRITE_STR       " exist but is protected from writing."
#define CNC_STR         " cannot be created -"
#define FORMAT_STR      " is in a bad format."

#define OVERWRITE_STR   " ALREADY EXIST and will be overwritten\n\n\n\
Overwrite the file?"

#define OVERWRITE_NOY_STR " ALREADY EXIST and will be overwritten."

static void mcb();
Boolean wprocFileChoiceNoFile();
static void squeeze_out_blanks(char *filename);
/*
void exp_tilde();
*/

struct test_env { 
                  Boolean found;
                  Window  search_win;
                };

static void display_warning(struct FILE_INFO *fi, 
                            Widget           popbox);


void test_for_window(Widget w, void *data) 
{
  struct test_env *env= (struct test_env*)data;
  if ( env->search_win == XtWindow(w) ) env->found= True;
}


static int myAccess(struct FILE_INFO *fi, char *fname, int mask)
{
  int retval;
  if (fi->net_file) {
     retval= fi->net_file->access_func(*fi->net_file->objptr, fname, mask);
  }
  else {
     retval= access(fname, mask);
  }
  return retval;
}

static time_t getModTime( struct FILE_INFO *fi, char *fname)
{
  int stat_ret; 
  struct stat file_stats;
  time_t mod_time= 0;

  if (fi->net_file) {
      mod_time= fi->net_file->mod_type_func(*fi->net_file->objptr, fname); 
  }
  else {
      stat_ret= stat(fname,&file_stats);
      if (stat_ret == 0) mod_time= file_stats.st_mtime; 
      else               mod_time= 0;
  }
  return mod_time;
}


long dofilecb(Widget                     w,
              struct CB                  *udata,
              XmTextVerifyCallbackStruct *winfo )


{


#define set_failure(msg, det) ( fi->fail_message= (msg), \
                                show_warning=     True,  \
                                fi->detail=       (det), \
                                happy=            False )


 
 struct FILE_INFO *fi;
 int bad;
 char tfile[FILENAMELEN];
 char *wstr;               /* string to hold the value to the text widget */
 char str[400];            /* string to hold the file name */
 char tmpstr[200];
 char *tmpptr= NULL;
 long i;
 long len;                  /* len - file name length         */
 long wlen;                 /* wlen - length of widget string */
 Window curr_win;           /* current focus window */
 Window shell_wid;          /* shell window id */
 Widget sw, tempw;          /* tempory widget id to find shell widget */
 Widget popbox;             /* the error msg widget- dialog, text, label, etc*/
 long    happy= False;      /* boolean set true when we are successful */
 int rev;
 int fileid;
 time_t mod_time;
 struct test_env w_search_env;
 Boolean nofile= False; 
 Boolean show_warning= False;   /*set when we will display a warning message */
 Boolean force_check= False;    /*if set file validation will occur */

 /*
  * these booleans are set according to user specified flags
  */
 Boolean must_exist;         /* the file must exist */
 Boolean is_required;        /* the file name is required */
 Boolean writable;           /* the file must be writable */
 Boolean ignor_focus;        /* do callback event when we don't have focus */
 Boolean msgblk= False;      /* NEW - block all messages */
 Boolean no_always_act_chk;  /* don't call on every activate 
                              *    (only call if file name has changed) */
 Boolean add_ext_always;     /* if set - add extension always, 
                              * otherwise add extension only when
                              * file without extension does 
                              * not exist */
 Boolean overwrt_warn;       /* Warn the user that file exist and
                              * will be overwritten */
 Boolean expand_file;        /* expand the file to it's full path */
 Boolean squeeze_blanks;     /* squeeze out all blank spaces in file name*/


 fi= (struct FILE_INFO *) udata->info;
 ignor_focus = (fi->check_flags & FI_IGNORE_FOC)     ? True : False;
 XGetInputFocus( XtDisplay(w), &curr_win, &rev );

 if (!fi->init) {
       if ( XtClass(udata->popbox) == xmMessageBoxWidgetClass) {
            XtAddCallback(udata->popbox, XmNcancelCallback, mcb, udata);
            XtAddCallback(udata->popbox, XmNokCallback, mcb, udata);
       } /* End if */
       fi->init= True;
 } /* END if */

 sw= get_shell_widget(w);
 shell_wid= XtWindow(sw);
 w_search_env.found= False;
 w_search_env.search_win= curr_win;
 wprocTravWTree(sw, test_for_window, &w_search_env);
 
 if ( (shell_wid == curr_win ) || w_search_env.found || ignor_focus ) {

     /*
      * determine which flags are set
      */
     must_exist       = (fi->check_flags & FI_MUST_EXIST)     ? True : False;
     is_required      = (fi->check_flags & FI_IS_REQUIRED)    ? True : False;
     writable         = (fi->check_flags & FI_WRITABLE)       ? True : False;
     msgblk           = (fi->check_flags & FI_NOMSG_BLK)      ? True : False;
     ignor_focus      = (fi->check_flags & FI_IGNORE_FOC)     ? True : False;
     no_always_act_chk= (fi->check_flags & FI_NO_ALWAYS_ACT_CHK) ? True : False;
     add_ext_always   = (fi->check_flags & FI_ADD_EXT_ALWAYS) ? True : False;
     overwrt_warn     = (fi->check_flags & FI_OVERWRT_WARN)   ? True : False;
     expand_file      = (fi->check_flags & FI_EXPAND_FILE)    ? True : False;
     squeeze_blanks   = (fi->check_flags & FI_SQUEEZE_BLANKS) ? True : False;

     wstr = XmTextGetString( w);   /* get the string in the text widget */

     if (squeeze_blanks) {
          squeeze_out_blanks (wstr);
          XmTextSetString( w, wstr);
     }

     wlen= strlen(wstr);

     fi->do_yn= False;

     popbox= udata->popbox;

     fi->detail= wprocNoDetail;
     fi->fail_message= NULL;

     /*
      * An activate callback will force file validate to happen even if
      * the file name does not change.  
      */
     if ((winfo->reason == XmCR_ACTIVATE) && (!no_always_act_chk))
                  force_check= True;

     /*
      * determine if there is a file in the text widget
      */
     nofile= wprocFileChoiceNoFile( fi , wstr);

     /*
      *   do any file name translation or expansion
      */
     if (nofile) {
             strcpy( str, "" );
             set_nofile( fi ,w );
     } /* End if */
     else {
             if (expand_file)  {
                 char bufferstr[400];
                 if (fi->net_file) {
                        fi->net_file->exp_tilde_func(*fi->net_file->objptr,
                                                     bufferstr, wstr);
                        fi->net_file->exp_file_func( *fi->net_file->objptr,
                                                     str, bufferstr);
                 }
                 else {
                        exptilde_crou2( bufferstr, wstr);
                        exp_file      ( str, bufferstr);
                 }
             }
             else  {
                 if (fi->net_file)
                     fi->net_file->exp_tilde_func(*fi->net_file->objptr,
                                                  str, wstr);
                 else
                     exptilde_crou2( str, wstr);
             }
     } /* End else */

     len= strlen(str);

     /*
      *  Begin the validation process
      */
     mod_time= getModTime(fi,str);
     if (mod_time == 0) mod_time= fi->file_mod_time;
     if (  (strcmp( str, fi->sav_filename) != 0 ) 
          || (mod_time != fi->file_mod_time)
          || (force_check) ) {
         /*
          * REQUIRED TEST
          */
         fi->file_mod_time= mod_time;
         if ( (nofile) && (is_required) ) {
                set_failure( REQUIRE_STR, wprocWasRequired);
         } /* End if */
         else {
                happy= True;
         } /* End else */

         if (!nofile) {              /* if we have a file specified */
             happy= True;
             strcpy(fi->filename, str);
             /*
              * ADD EXTENSION 
              *
              * if an extension is specified then add the extension to
              * the file:
              * CASE 1: if add_ext_always is set the alway put an extension
              *         on the file.
              * CASE 2: if not set then only put an extension on the file if
              *         either (1) the file with the extension already exist or
              *                (2) the file does not exist at all
              */
             if (strlen(fi->ext)) {
                      if (add_ext_always) {
                               add_ext( fi->filename, fi->ext, &bad);
                      }
                      else  {
                          strncpy(tfile, fi->filename, FILENAMELEN );
                          add_ext( tfile, fi->ext, &bad);

                          if  (myAccess(fi, tfile, F_OK)== 0) {
                                strcpy(fi->filename, tfile);
                          }
                          else if (myAccess(fi, fi->filename, F_OK)== -1) {
                                strcpy(fi->filename, tfile);
                          }
                      } /* end else */
             } /* end if */
             XmTextSetString(w, fi->filename);       

             /*
              * EXISTANCE TEST
              */
             if (must_exist) {
                      if (myAccess(fi, fi->filename, R_OK)==-1) {
                          set_failure( READ_STR, wprocDoesNotExist);
                      } /* END if */
                 /*
                  *   else if (writable) {
                  *       if (myAccess(fi, fi->filename, W_OK)==-1) {
                  *               set_failure( WRITE_STR, wprocNotWritable);
                  *       }
                  *   }
                  */
             } /* END if */

             /*
              * WRITABLE TEST
              */
             else if (writable) {
                 if (myAccess(fi, fi->filename, F_OK)==0) {
                       if (myAccess(fi, fi->filename, W_OK)==-1) {
                           set_failure( WRITE_STR, wprocNotWritable);
                       }
                 } /* END if */
                 else {
                       fileid= creat( fi->filename, 0666);
                       if (fileid == -1) {
                           sprintf(tmpstr, "%s\n%s", CNC_STR, strerror(errno) );
                           set_failure( tmpstr, wprocNotWritable);
                       } /* END if */
                       else {
                           close(fileid); 
                           remove (fi->filename);
                       } /* END else */
                 } /* ENDelse */
             } /* END if */

             /*
              * OVERWRITE WARING TEST 
              */ 
             if  ( (overwrt_warn) && (happy) && (!msgblk) ) { 
                  if (myAccess(fi, fi->filename, F_OK)==0) { 
                     if ( XtClass(udata->popbox) == xmMessageBoxWidgetClass) {
                        tempw=XmMessageBoxGetChild( popbox, 
                                                    XmDIALOG_CANCEL_BUTTON );
                        XtManageChild(tempw);
                        set_label( tempw, NOSTR);

                        tempw=XmMessageBoxGetChild( popbox, 
                                                    XmDIALOG_OK_BUTTON );
                        XtManageChild(tempw);
                        set_label( tempw, YESSTR);
                    
                        XtVaSetValues(popbox, 
                                      XmNuserData,   w,
                                      XmNdialogType, XmDIALOG_QUESTION, NULL );
                        set_failure( OVERWRITE_STR, wprocWillOverWrite);
                        happy= True;
                        fi->do_yn= True;
                     } /* End if */
                     else {
                        set_failure( OVERWRITE_NOY_STR, wprocWillOverWrite);
                        happy= True;
                     } /* End else */
                  } /* End if */
             } /* END if */
             if (happy) {
                 if (fi->twigs)
                    for(i=0,tempw= fi->twigs[0]; (tempw); tempw=fi->twigs[i++])
                            XmTextSetString(tempw, str);       
             } /* END else if */
                      
            
         } /* End if  (!nofile) */
         else {
             strcpy( fi->filename, "");
         }


         *fi->good_file= happy;
         if ( happy && fi->succfunc ) {
              happy= fi->succfunc( w, fi, fi->succdata, winfo); 
         } /* End if */
         else if ( !happy && fi->failfunc ) {
              tmpptr= fi->fail_message= XtNewString(fi->fail_message);
              show_warning= fi->failfunc( w, fi, fi->succdata, winfo); 
         } /* End else */

         /*
          * if we have a real event and this is not an activate callback
          * then enter the first if
          *
          * if we are not happy (encountered a problem) and we have a key
          * press event then enter the second if.
          */

         if ( (winfo->event)  && (winfo->reason != XmCR_ACTIVATE) ){ 
              if ( (!happy)&&(winfo->event->type==KeyPress) ) {
                      winfo->doit = msgblk ? True : False;
              } /* End if */
         } /* End if */
 
         XmTextSetInsertionPosition( w, wlen);

         *fi->good_file= happy;

         if ( (show_warning) && (!msgblk) ) display_warning(fi, popbox); 

     } /* End if (  (strcmp( str, fi->sav_filename) != 0 ) */

     strcpy( fi->sav_filename, fi->filename);

     if (tmpptr) XtFree(tmpptr);
     XtFree(wstr);


 } /* end if */
 
 return (happy);

}

static void mcb(Widget              w,
                struct CB           *udata,
                XmAnyCallbackStruct *winfo )
{
  Widget tempw, text;
  struct FILE_INFO *fi;

  fi= (struct FILE_INFO *) udata->info;

  if (fi->do_yn) {
      XtVaGetValues( w, XmNuserData, &text, NULL );

      if ( winfo->reason == XmCR_OK ) {
             fi->succfunc( text, fi, fi->succdata, winfo); 
      } /* END if */
      else {
             show_msg( text, "" );
             strcpy( fi->filename, "");
             strcpy( fi->sav_filename, "");
             /* wprocFileChoiceValidate( XtParent(text), False ); */
             XmProcessTraversal(text, XmTRAVERSE_CURRENT); 
             if (fi->check_flags & wprocIsRequiredMask)
                 *fi->good_file= False;
      } /* END if */
    


      tempw=XmMessageBoxGetChild( w, XmDIALOG_CANCEL_BUTTON );
      XtUnmanageChild(tempw);
    
      tempw=XmMessageBoxGetChild( w, XmDIALOG_OK_BUTTON );
      set_label( tempw, OKSTR);
      XtVaSetValues(udata->popbox, XmNuserData,   NULL,
                    XmNdialogType, XmDIALOG_ERROR, NULL );
      fi->do_yn= False;
  } /* END if */
}


Boolean wprocFileChoiceNoFile( struct FILE_INFO *fi,
                               char             *fname )

{
   int len, i;
   Boolean nofile;

    len= strlen(fname);

    if (len) {
       if ( ( strcmpnull(fi->none_str, fname) == 0 ) && (fi->use_none) ) {
          nofile= True;
       } /* End if */
       else {
         for(nofile= True,i= 0;
               ( (i<len) && (nofile = isspace(fname[i])) );i++);
       } /* End else */
    } /* End if */
    else {
       nofile= True;
    } /* End else */

  return (nofile);
}


static void squeeze_out_blanks(char *filename)
{
  int i, j, k;

  if(!filename) return;
  j = 0;
  k = strlen(filename);
  for(i = 0; i < k; i++)
       {
       if(filename[i] != ' ') { filename[j] = filename[i]; j++; }
       }
  filename[j] = '\0';
}


static void display_warning(struct FILE_INFO *fi, 
                            Widget           popbox) 
{
  char *umsg= NULL;
  Boolean did_malloc= False;

  if (fi->fail_message) {
      if (strlen(fi->file_desc) > 0) {
           umsg= (char *)malloc( strlen(fi->file_desc) +
                                   strlen(fi->fail_message) + 5 );
           if (umsg) {
               sprintf( umsg, "%s%s", fi->file_desc, fi->fail_message );
               did_malloc= True;
           }
           else 
               puts( "FileChoice: display_warning: malloc failed");
      }
      else {
           umsg= fi->fail_message;
      }
      wprocShowMsg( popbox, umsg );
  }
  if ((did_malloc) && (umsg))  free(umsg);
}
