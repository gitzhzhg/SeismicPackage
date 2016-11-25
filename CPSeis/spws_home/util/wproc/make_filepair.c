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
C      make_filepair.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C             written in c -- designed to be called from c
C
C     Utility Name:  make_filepair    (make a pair of file choice widgets)
C          Written:  93/09/05  by:  Tom Stoeckley
C     Last revised:  95/09/04  by:  Tom Stoeckley
C
C  Purpose:     To create a user interface which allows entry of an
C               input and an output file name.  This interface
C               also displays information about the status of each
C               file and the action which will occur when the user
C               presses an OK button in the application.  Certain
C               procedures are enforced in order to protect any
C               pre-existing output file.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/wproc   (shared)
C  library:                wproc.a            (shared)
C  header file:            wproc.h            (shared)
C  source file:            make_filepair.c
C
C  static functions:       get_custom_resources
C                          show
C                          show_messages
C                          destroy_callback
C                          enter_callback
C                          make_filepair_helper
C
C  documented functions:   make_filepair
C                          make_alternate_filepair
C                          update_filepair
C                          get_filepair_status
C                          get_filepair_message
C                          change_filepair_message
C                          filepair_register_update_function
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     wproc.a    cprim.a
C  header files:  wproc.h    cprim.h   file_choice.h
C
C  functions:     get_toplevel_shell
C                 wprocFileChoiceSetFile
C                 wprocFileChoiceRetWidgets
C                 attach_widget
C                 inquire_files_combo
C                 inquire_files_combo_alternate
C                 set_compound_resource
C                 add_HELP
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  3. 95/09/04  Stoeckley  Add the following functions:
C                             make_alternate_filepair
C                             get_filepair_status 
C                             get_filepair_message
C                             filepair_register_update_function
C  2. 94/08/31  Stoeckley  Add change_filepair_message.
C  1. 93/09/05  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C
C  For pointers, the flag (i,o,b) refers to the contents pointed to
C  by the pointer, not to the value of the pointer itself.  The pointer
C  value is required upon INPUT in all cases.
C-----------------------------------------------------------------------
C  To create a filepair widget:
C
C                                      i      i       i
C    wpair = make_filepair          (parent, name, filetype,
C       ext, hctx, trap, data, filename1, filename2, required1, required2)
C        i    i     i     i        i          i          i          i
C
C                                         i      i       i          i
C    wpair = make_nonmatching_filepair (parent, name, filetype1, filetype2,
C          ext1, ext2, suffix2,
C           i     i       i 
C          hctx, trap, data, filename1, filename2, required1, required2)
C           i     i     i        i          i          i          i
C
C                                       i      i       i
C    wpair = make_alternate_filepair (parent, name, filetype,
C       ext, hctx, trap, data, filename1, filename2, required1, required2)
C        i    i     i     i        i          i          i          i
C
C  Widget parent = parent of filepair widget to create.
C  char    *name = name of filepair widget.
C  char *filetype  = type of files (e.g. "SCRS pickfile" or "CPS static file".
C  char *filetype1 = type of input file.
C  char *filetype2 = type of output file.
C  char *ext     = default extension for files (e.g. "cst" or "fish" or "").
C  char *ext1    = default extension for input file.
C  char *ext2    = default extension for output file.
C  char *suffix2 = default suffix for end of output file proper name.
C  struct HELPCTX *hctx = pointer to help context.
C  void (*trap)() = trap function to call when user enters file name.
C  void *data = pointer to data to be passed to trap function.
C  char *filename1 = pointer to name of input file.
C  char *filename2 = pointer to name of output file.
C  long required1 = TRUE if input file is required.
C  long required2 = TRUE if output file is required.
C  Widget wpair = name of created and managed filepair widget.
C
C  The filepair widget is a form widget with two filechoice widgets 
C    and three label widgets as children.  The filechoice widgets are
C    for entering and displaying input and output file names, and the
C    label widgets are for displaying messages regarding the status of
C    the two files.
C
C  The filepair widget remembers the addresses of the two filenames in
C    the user area.  It gets the names from the user area whenever
C    update_filepair is called, and it resets these names whenever
C    the user enters a file name.  The trap function can reset these
C    names if it wishes to (but this could confuse the user).
C
C  The userData resource of the filepair widget must not be used, since
C    it is already being used by this utility.
C
C  The second function is to be used if an alternate set of messages is
C    requested.
C-----------------------------------------------------------------------
C  The user-written trap function must have the following form:
C
C                i      o       o       o      o         o
C     void trap(data, valid1, valid2, info1, info2, same_datasets)
C
C  long *valid1 = whether the input file is a valid file.
C  long *valid2 = whether the output file is a valid file.
C  char *info1 = string describing input file when valid1 = YES.
C  char *info2 = string describing output file when valid1 = YES.
C  long same_datasets = TRUE if the files appear to refer to the
C                         same dataset (needed only if valid1 and
C                         valid2 are both YES).
C
C  valid        description
C  -----        -----------
C  YES          file is known to be a valid file of appropriate type.
C  NO           file is known NOT to be a valid file of appropriate type.
C  MAYBE        do not know and do not care.
C
C  The user-written trap function can be NULL.  Here are the default
C    values of the arguments, which will be used if the trap function
C    does not set the values, or if the trap function is NULL:
C          valid1 and valid2 = MAYBE
C          info1 and info2   = ""
C          same_datasets     = TRUE
C
C  The trap function will have to reference the file names (reachable
C    in the user area through the data pointer) in order to determine
C    whether the files are of appropriate type and refer to the same
C    dataset.
C
C  The above constants are defined in the cprim.h header file.
C-----------------------------------------------------------------------
C  To register a function to be called just before returning to
C  the event loop from callbacks:
C
C          void filepair_register_update_function(upfun, updata)
C
C  The user-written function must have the following form:
C
C                 void (*upfun)(void *updata)
C-----------------------------------------------------------------------
C  To update the filechoice widgets and associated messages:
C
C                                       i     o
C            status = update_filepair(wpair, msg)
C
C  Widget wpair = the widget returned by make_filepair.
C  char    *msg = returned message describing the returned status.
C  long  status = the status of the two files (returned).
C
C  status          description
C  ------          -----------
C  FILE_CREATE     output file will be created from scratch (no input file).
C  FILE_READ_ONLY  input file will be read-only (no output file).
C  FILE_COPY       input file will be copied to output file.
C  FILE_UPDATE     input file will be updated (input file = output file).
C  FILE_ERROR      there is an error (any other situation).
C  FILE_CHANGES    changes occurred in files or their status since messages
C                    were last displayed.
C
C  The above constants are defined in the cprim.h header file.
C
C  This routine should be called whenever the filepair widget is first
C    made visible, whenever one or the other file name is changed,
C    or whenever the status of the files should be re-checked.
C  This routine is also called automatically whenever the user inputs
C    a new file name.
C  This routine calls the trap function.
C
C  The returned message may be displayed in an error box if the
C    returned status is FILE_ERROR.  This need be done only when
C    further action is imminent (e.g. when the user presses an OK
C    button).  The error box is not necessary otherwise because the
C    error is displayed in the filepair widget itself.  If you do not
C    need the returned message, you can use a NULL pointer for msg.
C
C  Situations in which a FILE_ERROR status will be returned include:
C    (not completed...)
C-----------------------------------------------------------------------
C  To get information after update_filepair has been called:
C  To get the status of the files:
C  To get the corresponding messages:
C                                               i      i
C    long     status = get_filepair_status   (wpair, which)
C    const char *msg = get_filepair_message  (wpair, which)
C
C  Widget wpair = the widget returned by make_filepair.
C  int    which = which status or message to get (1 or 2 or 3).
C
C  which == 1 refers to the input file.
C  which == 2 refers to the output file.
C  which == 3 refers to the combined file information.
C
C  If which == 3, the status is the same as that returned by
C  update_filepair, unless update_filepair returned FILE_CHANGES.
C-----------------------------------------------------------------------
C  To change the message in one of the filepair label fields:
C
C                                           i      i     i
C           void  change_filepair_message(wpair, which, msg)
C
C  Widget wpair = the widget returned by make_filepair.
C  int    which = which label field to replace (1 or 2 or 3).
C  char    *msg = the message you want to display.
C
C  which == 1 refers to the input file.
C  which == 2 refers to the output file.
C  which == 3 refers to the combined file information.
C
C  This is not normally needed, but can be used if desired.  For
C  example, it might be sensible to blank out the labels if the
C  information is no longer relevant.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC
*/


#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <X11/StringDefs.h>
#include "wproc.h"
#include "file_choice.h"
#include "cprim.h"
#include "inquire.h"



/*-------------- data structure ----------------------------------------*/
/*-------------- data structure ----------------------------------------*/
/*-------------- data structure ----------------------------------------*/
/*-------------- data structure ----------------------------------------*/
/*-------------- data structure ----------------------------------------*/


typedef struct _ShortStruct
   {
   MakeFilepairTrap *trap;
   void *data;
   char filetype1[40];
   char filetype2[40];
   char ext1[20];
   char ext2[20];
   char suffix2[20];
   long required1, required2, alternate;
   char *filename1, *filename2;
   Widget wmsg1, wmsg2, wmsg3;
   Widget wpair, wfile1, wfile2;
   char old_msg1[300], old_msg2[300], old_msg3[300];
   char old_filename1[100], old_filename2[100];
   Pixel pixel_info, pixel_error, pixel_action;
   long status1, status2, status3;
   MakeFilepairUpfun *upfun;
   void *updata;
   } ShortStruct;



/*-------------------- get custom resources --------------------------*/ 
/*-------------------- get custom resources --------------------------*/ 
/*-------------------- get custom resources --------------------------*/ 
/*-------------------- get custom resources --------------------------*/ 
/*-------------------- get custom resources --------------------------*/ 


#define III XtRInt   , sizeof(int   )
#define PPP XtRPixel , sizeof(Pixel )
#define SSS XtRString, sizeof(String)
#define JJ  XtRImmediate
#define SS  XtRString
#define RRR(n,i,p,j,c)  { n, "Filepair", i, XtOffsetOf(Custom,p), j, c },


static void get_custom_resources(ShortStruct *ss, Widget parent)
{
  typedef struct _Custom
       {
       int line_width;                         /* not used */
       char *fontname;                         /* not used */
       Pixel pixel_info, pixel_error, pixel_action;
       } Custom;
  static XtResource resources[] = {
       RRR("filepair_line_width"    ,III,line_width  ,JJ,(XtPointer)3  )
       RRR("filepair_fontname"      ,SSS,fontname    ,SS,"9x15")
       RRR("filepair_color_info"    ,PPP,pixel_info  ,SS,"green4")
       RRR("filepair_color_error"   ,PPP,pixel_error ,SS,"red")
       RRR("filepair_color_action"  ,PPP,pixel_action,SS,"blue")
       };
  Widget toplevel;
  static Custom custom;
  static Boolean starting = True;

  if(starting)
       {
       toplevel = get_toplevel_shell(parent);
       XtGetApplicationResources(toplevel, &custom, resources,
                           XtNumber(resources), NULL, 0);
       starting = False;
       }
  ss->pixel_info   = custom.pixel_info;
  ss->pixel_error  = custom.pixel_error;
  ss->pixel_action = custom.pixel_action;
}

  
  
/*-------------------- misc static functions -------------------------*/ 
/*-------------------- misc static functions -------------------------*/ 
/*-------------------- misc static functions -------------------------*/ 
/*-------------------- misc static functions -------------------------*/ 
/*-------------------- misc static functions -------------------------*/ 


static void show_messages(ShortStruct *ss,
                              char* msg1, char *msg2, char *msg3,
                              long status1, long status2, long status3)
{
  Pixel pixel1 = ss->pixel_info;
  Pixel pixel2 = ss->pixel_info;
  Pixel pixel3 = ss->pixel_action;

  if(status1 == FILE_ERROR) pixel1 = ss->pixel_error;
  if(status2 == FILE_ERROR) pixel2 = ss->pixel_error;
  if(status3 == FILE_ERROR) pixel3 = ss->pixel_error;
  if(status1 == FILE_ERROR || status2 == FILE_ERROR)
           XtVaGetValues(ss->wmsg3, XmNbackground, &pixel3, NULL);
  XtVaSetValues(ss->wmsg1, XmNforeground, pixel1, NULL);
  XtVaSetValues(ss->wmsg2, XmNforeground, pixel2, NULL);
  XtVaSetValues(ss->wmsg3, XmNforeground, pixel3, NULL);
  set_compound_resource(ss->wmsg1, XmNlabelString, msg1);
  set_compound_resource(ss->wmsg2, XmNlabelString, msg2);
  set_compound_resource(ss->wmsg3, XmNlabelString, msg3);
}


/*----------------- change message -----------------------------*/
/*----------------- change message -----------------------------*/
/*----------------- change message -----------------------------*/
/*----------------- change message -----------------------------*/
/*----------------- change message -----------------------------*/

void change_filepair_message(Widget wpair, int which, char *msg)
{
  ShortStruct *ss;

  XtVaGetValues(wpair, XmNuserData, &ss, NULL);
  switch(which)
    {
    case 1: set_compound_resource(ss->wmsg1, XmNlabelString, msg); break;
    case 2: set_compound_resource(ss->wmsg2, XmNlabelString, msg); break;
    case 3: set_compound_resource(ss->wmsg3, XmNlabelString, msg); break;
    default: break;
    }
}



/*----------- filepair register update function --------------*/
/*----------- filepair register update function --------------*/
/*----------- filepair register update function --------------*/
/*----------- filepair register update function --------------*/
/*----------- filepair register update function --------------*/

void filepair_register_update_function
                   (Widget wpair, MakeFilepairUpfun *upfun, void *updata)
{
  ShortStruct *ss;

  XtVaGetValues(wpair, XmNuserData, &ss, NULL);
  ss->upfun = upfun;
  ss->updata = updata;
}



/*-------------- update filechoice pair ------------------------------*/ 
/*-------------- update filechoice pair ------------------------------*/ 
/*-------------- update filechoice pair ------------------------------*/ 
/*-------------- update filechoice pair ------------------------------*/ 
/*-------------- update filechoice pair ------------------------------*/ 


long update_filepair(Widget wpair, char *msg)
{
  ShortStruct *ss;

  long valid1 = INQUIRE_VALID_MAYBE;
  long valid2 = INQUIRE_VALID_MAYBE;
  long same_datasets = TRUE;
  char msg1[300], msg2[300], msg3[300];
  char info1[100], info2[100];
  long status;

  XtVaGetValues(wpair, XmNuserData, &ss, NULL);
  info1[0] = '\0';
  info2[0] = '\0';
  if(ss->trap) ss->trap(ss->data, &valid1, &valid2,
                                        info1, info2, &same_datasets);
  if(ss->alternate)
      {
/*
      ss->status3 = inquire_files_combo_alternate(ss->filename1, ss->filename2,
          ss->filetype1, ss->required1, ss->required2, valid1, valid2,
          info1, info2, same_datasets, &ss->status1, &ss->status2,
          msg1, msg2, msg3);
*/
      ss->status1 = inquire_input_file (ss->filename1, ss->required1, valid1,
                                        ss->filetype1, info1, msg1);
      ss->status2 = inquire_output_file(ss->filename2, ss->required2, valid2,
                                        ss->filetype2, info2, msg2);
      ss->status3 = inquire_files_alternate (ss->filename1, ss->filename2,
                                   ss->status1, ss->status2,
                                   same_datasets, msg1, msg2, msg3);
      }
  else
      {
/*
      ss->status3 = inquire_files_combo(ss->filename1, ss->filename2,
          ss->filetype1, ss->required1, ss->required2, valid1, valid2,
          info1, info2, same_datasets, &ss->status1, &ss->status2,
          msg1, msg2, msg3);
*/
      ss->status1 = inquire_input_file (ss->filename1, ss->required1, valid1,
                                        ss->filetype1, info1, msg1);
      ss->status2 = inquire_output_file(ss->filename2, ss->required2, valid2,
                                        ss->filetype2, info2, msg2);
      if(!strcmp(ss->ext1, ss->ext2))
          {
          ss->status3 = inquire_files (ss->filename1, ss->filename2,
                                       ss->status1, ss->status2,
                                       same_datasets, msg1, msg2, msg3);
          }
      else
          {
          ss->status3 = inquire_files_nonmatching(ss->filename1, ss->filename2,
                                       ss->status1, ss->status2,
                                       same_datasets, msg1, msg2, msg3);
          }
      }
  if(strcmp(ss->old_msg1, msg1) ||
     strcmp(ss->old_msg2, msg2) ||
     strcmp(ss->old_msg3, msg3) ||
     strcmp(ss->old_filename1, ss->filename1) ||
     strcmp(ss->old_filename2, ss->filename2))
       {
       if(msg) strcpy(msg, "The files or their status have changed.\n");
       if(msg) strcat(msg, "Check them before continuing.");
       show_messages(ss, msg1, msg2, msg3,
                         ss->status1, ss->status2, ss->status3);
       status = FILE_CHANGES;
       strcpy(ss->old_msg1, msg1);
       strcpy(ss->old_msg2, msg2);
       strcpy(ss->old_msg3, msg3);
       strcpy(ss->old_filename1, ss->filename1);
       strcpy(ss->old_filename2, ss->filename2);
       wprocFileChoiceSetFile(ss->wfile1, ss->filename1, False);
       wprocFileChoiceSetFile(ss->wfile2, ss->filename2, False);
       }
  else
       {
       if(msg) strcpy(msg, msg3);
       status = ss->status3;
       }
  return status;
}




/*-------------------- get file status or message -------------------*/ 
/*-------------------- get file status or message -------------------*/ 
/*-------------------- get file status or message -------------------*/ 
/*-------------------- get file status or message -------------------*/ 
/*-------------------- get file status or message -------------------*/ 


long get_filepair_status(Widget wpair, int which)
{
  ShortStruct *ss;
  XtVaGetValues(wpair, XmNuserData, &ss, NULL);
  switch(which)
      {
      case 1: return ss->status1;
      case 2: return ss->status2;
      case 3: return ss->status3;
      default: assert(FALSE);
      }
  assert(FALSE);
  return 0;
}


const char *get_filepair_message(Widget wpair, int which)
{
  ShortStruct *ss;
  XtVaGetValues(wpair, XmNuserData, &ss, NULL);
  switch(which)
      {
      case 1: return ss->old_msg1;
      case 2: return ss->old_msg2;
      case 3: return ss->old_msg3;
      default: assert(FALSE);
      }
  assert(FALSE);
  return NULL;
}


/*----------------------- add before extension ---------------------*/
/*----------------------- add before extension ---------------------*/
/*----------------------- add before extension ---------------------*/

    /* if suffix is not blank, it is added to the end of        */
    /* filename AFTER any existing extension is first removed.  */

static void add_before_extension(char *filename, const char *suffix)
{
  int len = strlen(filename);
  int last = -1;
  int i;
  if(strlen(suffix) == 0) return;
  for(i = 0; i < len; i++)
      {
      if(filename[i] == '.') last = i;
      if(filename[i] == '/') last = -1;
      }
  if(last != -1) filename[last] = '\0';
  strcat(filename, suffix);
}




/*------------------------- callbacks --------------------------------*/ 
/*------------------------- callbacks --------------------------------*/ 
/*------------------------- callbacks --------------------------------*/ 
/*------------------------- callbacks --------------------------------*/ 
/*------------------------- callbacks --------------------------------*/ 


static void destroy_callback(Widget wbegin, ShortStruct *ss,
                                           XmAnyCallbackStruct *call)
{
  free(ss);
}




static void enter_callback(Widget w, ShortStruct *ss,
                                 wprocFileChoiceSuccCallbackStruct *call)
{
  static char junk[] = "junk";
  char msg[300];
  long status;

  if(w == ss->wfile1)
       {
       strcpy(ss->filename1, call->filename);
       if(ss->filename1[0] != '\0')            /* this line added 10/25/94 */
           {
           int istat;                                     /* added 1/22/97 */
           strcpy(ss->filename2, call->filename);
           add_before_extension(ss->filename2, ss->suffix2); /* added 2/12/97 */
           addext_rep_(ss->filename2, ss->ext2, &istat);  /* added 1/22/97 */
           }
       }
  else
       {
       strcpy(ss->filename2, call->filename);
       }
  status = update_filepair(ss->wpair, msg);
  call->fail_str = junk;
  if(ss->upfun) ss->upfun(ss->updata);
}



/*----------------- make filepair helper -----------------------------*/ 
/*----------------- make filepair helper -----------------------------*/ 
/*----------------- make filepair helper -----------------------------*/ 
/*----------------- make filepair helper -----------------------------*/ 
/*----------------- make filepair helper -----------------------------*/ 


static Widget make_filepair_helper(Widget parent, char *name,
        char *filetype1, char *filetype2,
        char *ext1, char *ext2, char *suffix2,
        struct HELPCTX *hctx, void (*trap)(), void *data,
        char *filename1, char *filename2,
        long required1, long required2, long alternate)
{
  Widget w, wdummy, fsbpopw;
  ShortStruct *ss;
  char filetype[80];

  ss = (ShortStruct*)calloc(1, sizeof(ShortStruct));  /*preset to zeroes*/
  if(!ss) return NULL;

  get_custom_resources(ss, parent);

  strcpy(ss->filetype1, filetype1);
  strcpy(ss->filetype2, filetype2);
  strcpy(ss->ext1, ext1);
  strcpy(ss->ext2, ext2);
  strcpy(ss->suffix2, suffix2);
  ss->trap              = trap;
  ss->data              = data;
  ss->filename1         = filename1;
  ss->filename2         = filename2;
  ss->required1         = required1;
  ss->required2         = required2;
  ss->alternate         = alternate;
  ss->status1           = FILE_ERROR;
  ss->status2           = FILE_ERROR;
  ss->status3           = FILE_ERROR;
  ss->upfun             = NULL;
  ss->updata            = NULL;
  ss->old_filename1[0]  = '\0';
  ss->old_filename2[0]  = '\0';
  ss->old_msg1[0]       = '\0';
  ss->old_msg2[0]       = '\0';
  ss->old_msg3[0]       = '\0';

  ss->wpair = w = XmCreateForm (parent, name          , NULL, 0);
  wdummy        = XmCreateLabel(w, "filepair_dummy"   , NULL, 0);
  ss->wmsg1     = XmCreateLabel(w, "filepair_message1", NULL, 0);
  ss->wmsg2     = XmCreateLabel(w, "filepair_message2", NULL, 0);
  ss->wmsg3     = XmCreateLabel(w, "filepair_message3", NULL, 0);

  ss->wfile1 = XtVaCreateWidget( "filepair_choice1",fileChoiceWidgetClass, w,
           XmNfileExtension, ext1,
           XmNfileDescription, "",
           XmNfileFlags, wprocExpandFileMask | wprocSqueezeBlanksMask,
           XmNlabelString,"Input File...",
           XmNannoType, wprocPushButton,
           XmNvalidateOnSelection, True,
           XmNerrorWidget, wdummy,
           NULL);

  ss->wfile2 = XtVaCreateWidget( "filepair_choice2",fileChoiceWidgetClass, w,
           XmNfileExtension, ext2,
           XmNfileDescription, "",
           XmNfileFlags, wprocExpandFileMask | wprocSqueezeBlanksMask,
           XmNlabelString,"Output File:",
           XmNannoType, wprocLabel,
           XmNerrorWidget, wdummy,
           NULL);

  set_compound_resource(ss->wmsg1, XmNlabelString, "Wg\nWg"    );
  set_compound_resource(ss->wmsg2, XmNlabelString, "Wg\nWg"    );
  set_compound_resource(ss->wmsg3, XmNlabelString, "Wg\nWg\nWg");

  XtVaSetValues(ss->wmsg1 , XmNrecomputeSize,  False, NULL);
  XtVaSetValues(ss->wmsg2 , XmNrecomputeSize,  False, NULL);
  XtVaSetValues(ss->wmsg3 , XmNrecomputeSize,  False, NULL);

  XtManageChild(ss->wmsg1 );
  XtManageChild(ss->wmsg2 );
  XtManageChild(ss->wmsg3 );
  XtManageChild(ss->wfile1);
  XtManageChild(ss->wfile2);
  XtManageChild(ss->wpair );

  if(strcmp(ss->filetype1, ss->filetype2) == 0)
      {
      strcpy(filetype, ss->filetype1);
      }
  else
      {
      strcpy(filetype, ss->filetype1);
      strcat(filetype, " to "       );
      strcat(filetype, ss->filetype2);
      }
  wprocFileChoiceRetWidgets(ss->wfile1, NULL, NULL, &fsbpopw, NULL);
  XtVaSetValues(XtParent(fsbpopw), XmNtitle   , filetype, NULL);
  XtVaSetValues(ss->wpair        , XmNuserData, ss      , NULL);

  if(hctx) add_HELP(ss->wfile1, helper, hctx);
  if(hctx) add_HELP(ss->wfile2, helper, hctx);

  attach_widget(ss->wfile1, w, w, w         , NULL , 0,0, 0,0);
  attach_widget(ss->wmsg1 , w, w, ss->wfile1, NULL , 0,0, 0,0);
  attach_widget(ss->wfile2, w, w, ss->wmsg1 , NULL , 0,0, 0,0);
  attach_widget(ss->wmsg2 , w, w, ss->wfile2, NULL , 0,0, 0,0);
  attach_widget(ss->wmsg3 , w, w, ss->wmsg2 , w    , 0,0, 0,0);

  XtAddCallback(ss->wpair , XmNdestroyCallback,
                       (XtCallbackProc)destroy_callback, (XtPointer)ss);
  XtAddCallback(ss->wfile1, XmNsuccessCallback,
                       (XtCallbackProc)enter_callback  , (XtPointer)ss);
  XtAddCallback(ss->wfile1, XmNfailCallback   ,
                       (XtCallbackProc)enter_callback  , (XtPointer)ss);
  XtAddCallback(ss->wfile2, XmNsuccessCallback,
                       (XtCallbackProc)enter_callback  , (XtPointer)ss);
  XtAddCallback(ss->wfile2, XmNfailCallback   ,
                       (XtCallbackProc)enter_callback  , (XtPointer)ss);

  update_filepair(ss->wpair, NULL);
  return ss->wpair;
}
  
  
  

/*--------------------- make filepair --------------------------*/
/*--------------------- make filepair --------------------------*/
/*--------------------- make filepair --------------------------*/
/*--------------------- make filepair --------------------------*/
/*--------------------- make filepair --------------------------*/


Widget make_filepair(Widget parent, char *name, char *filetype,
        char *ext, struct HELPCTX *hctx, void (*trap)(), void *data,
        char *filename1, char *filename2,
        long required1, long required2)
{
  return make_filepair_helper(parent, name, filetype, filetype,
        ext, ext, "", hctx,
        trap, data, filename1, filename2, required1, required2, FALSE);
}


Widget make_nonmatching_filepair(Widget parent, char *name,
        char *filetype1, char *filetype2,
        char *ext1, char *ext2, char *suffix2,
        struct HELPCTX *hctx, void (*trap)(), void *data,
        char *filename1, char *filename2,
        long required1, long required2)
{
  return make_filepair_helper(parent, name, filetype1, filetype2,
        ext1, ext2, suffix2, hctx,
        trap, data, filename1, filename2, required1, required2, FALSE);
}


Widget make_alternate_filepair(Widget parent, char *name, char *filetype,
        char *ext, struct HELPCTX *hctx, void (*trap)(), void *data,
        char *filename1, char *filename2,
        long required1, long required2)
{
  return make_filepair_helper(parent, name, filetype, filetype,
        ext, ext, "", hctx,
        trap, data, filename1, filename2, required1, required2, TRUE);
}




/*--------------------------- end ------------------------------------*/ 
/*--------------------------- end ------------------------------------*/ 
/*--------------------------- end ------------------------------------*/ 
/*--------------------------- end ------------------------------------*/ 
/*--------------------------- end ------------------------------------*/ 

