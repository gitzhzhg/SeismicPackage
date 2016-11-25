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
/* See /home/spws/util/wproc/make_filepair.c for documentation */
#include "sl/slp_filepair.hh"

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <X11/StringDefs.h>
//#include "file_choice.h"
#include "sl/slp_file.hh"
#include "cprim.h"
#include "inquire.h"
#include "tfdefs.h"

enum {
  SLP_FILE1,
  SLP_FILE2
};


/*-------------- data structure ----------------------------------------*/
/*-------------- data structure ----------------------------------------*/
/*-------------- data structure ----------------------------------------*/
/*-------------- data structure ----------------------------------------*/
/*-------------- data structure ----------------------------------------*/


typedef struct _ShortStruct
   {
   MakeSLpFilepairTrap *trap;
   void *data;
   char filetype1[40];
   char filetype2[40];
   char ext1[20];
   char ext2[20];
   char suffix2[20];
   long required1, required2, alternate;
   SLpFile *slp_file1, *slp_file2;
   char *filename1, *filename2;
   Widget wmsg1, wmsg2, wmsg3;
   Widget wpair, wfile1, wfile2;
   char old_msg1[300], old_msg2[300], old_msg3[300];
   char old_filename1[100], old_filename2[100];
   Pixel pixel_info, pixel_error, pixel_action;
   long status1, status2, status3;
   MakeSLpFilepairUpfun *upfun;
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
#define RRR(n,i,p,j,c)  { n, (char*)"Filepair", i, XtOffsetOf(Custom,p), j, c },


static void get_custom_resources(ShortStruct *ss, Widget parent)
{
  typedef struct _Custom
       {
       int line_width;                         /* not used */
       char *fontname;                         /* not used */
       Pixel pixel_info, pixel_error, pixel_action;
       } Custom;
  static XtResource resources[] = {
       RRR((char*)"filepair_line_width"    ,III,line_width  ,JJ,(XtPointer)3  )
       RRR((char*)"filepair_fontname"      ,SSS,fontname    ,SS,(char*)"9x15")
       RRR((char*)"filepair_color_info"    ,PPP,pixel_info  ,SS,(char*)"green4")
       RRR((char*)"filepair_color_error"   ,PPP,pixel_error ,SS,(char*)"red")
       RRR((char*)"filepair_color_action"  ,PPP,pixel_action,SS,(char*)"blue")
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

void change_slp_filepair_message(Widget wpair, int which, char *msg)
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

void slp_yfilepair_register_update_function
                   (Widget wpair, MakeSLpFilepairUpfun *upfun, void *updata)
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


long update_slp_filepair(Widget wpair, char *msg)
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
       ss->slp_file1->setFilename (ss->filename1);
       ss->slp_file2->setFilename (ss->filename2);
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


long get_slp_filepair_status(Widget wpair, int which)
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


const char *get_slp_filepair_message(Widget wpair, int which)
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



static void enter_callback (void *data, long ident, char *oldvar,
  char *newvar)
{
  char msg[300];
  long status;

  ShortStruct *ss = (ShortStruct *)data;
  if (ident == SLP_FILE1) {
    strcpy (ss->filename1, newvar);
    ss->slp_file1->setFilename (ss->filename1);
    if (!SLpFile::isAnEmptyFilename(ss->filename1)) {
      int istat;
      strcpy (ss->filename2, newvar);
      add_before_extension (ss->filename2, ss->suffix2);
      addext_rep_ (ss->filename2, ss->ext2, &istat);
      ss->slp_file2->setFilename (ss->filename2);
    }
  }
  else if (ident == SLP_FILE2) {
    strcpy (ss->filename2, newvar);
    ss->slp_file2->setFilename (ss->filename2);
  }
  status = update_slp_filepair(ss->wpair, msg);
  //call->fail_str = junk;
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
        struct HELPCTX *hctx, MakeSLpFilepairTrap *trap, void *data,
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
  ss->trap              = (void(*)(void*,long*,long*,char*,char*,long*))trap;
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

  ss->slp_file1 = new SLpFile (w, "filepair_choice1", SLP_FILE1,
    "Input File...", filetype1, ext1, SLpFile::_INPUT);
  ss->slp_file1->setCtrap (enter_callback, ss);
  ss->slp_file1->setFilename (ss->filename1);
  ss->wfile1 = ss->slp_file1->W();

  ss->slp_file2 = new SLpFile (w, "filepair_choice2", SLP_FILE2,
    "Output File:", filetype2, ext2, SLpFile::_OUTPUT);
  ss->slp_file2->setCtrap (enter_callback, ss);
  ss->slp_file2->setFilename (ss->filename2);
  ss->wfile2 = ss->slp_file2->W();

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
/*
 * The smart form simply will not have a title: "filetype1 to filetype2"
 *  wprocFileChoiceRetWidgets(ss->wfile1, NULL, NULL, &fsbpopw, NULL);
 *  XtVaSetValues(XtParent(fsbpopw), XmNtitle   , filetype, NULL);
 */

  XtVaSetValues(ss->wpair        , XmNuserData, ss      , NULL);

  if(hctx) add_HELP(ss->wfile1, helper, hctx);
  if(hctx) add_HELP(ss->wfile2, helper, hctx);

  attach_widget(ss->wfile1, w, w, w         , NULL , 0,0, 0,0);
  attach_widget(ss->wmsg1 , w, w, ss->wfile1, NULL , 0,0, 0,0);
  attach_widget(ss->wfile2, w, w, ss->wmsg1 , NULL , 0,0, 0,0);
  attach_widget(ss->wmsg2 , w, w, ss->wfile2, NULL , 0,0, 0,0);
  attach_widget(ss->wmsg3 , w, w, ss->wmsg2 , w    , 0,0, 0,0);

/*
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
*/

  update_slp_filepair(ss->wpair, NULL);
  return ss->wpair;
}
  
  
  

/*--------------------- make filepair --------------------------*/
/*--------------------- make filepair --------------------------*/
/*--------------------- make filepair --------------------------*/
/*--------------------- make filepair --------------------------*/
/*--------------------- make filepair --------------------------*/


Widget make_slp_filepair(Widget parent, char *name, char *filetype,
        char *ext,
        struct HELPCTX *hctx, MakeSLpFilepairTrap *trap,
	/*void(*trap)(void*,long*,long*,char*,char*,long*),*/
        void *data, char *filename1, char *filename2,
        long required1, long required2)
{
  return make_filepair_helper(parent, name, filetype, filetype,
        ext, ext, "", hctx,
        trap, data, filename1, filename2, required1, required2, FALSE);
}


Widget make_nonmatching_slp_filepair(Widget parent, char *name,
        char *filetype1, char *filetype2,
        char *ext1, char *ext2, char *suffix2,
        struct HELPCTX *hctx, MakeSLpFilepairTrap *trap,
	/*void(*trap)(void*,long*,long*,char*,char*,long*),*/
        void *data, char *filename1, char *filename2,
        long required1, long required2)
{
  return make_filepair_helper(parent, name, filetype1, filetype2,
        ext1, ext2, suffix2, hctx,
        trap, data, filename1, filename2, required1, required2, FALSE);
}


Widget make_alternate_slp_filepair(Widget parent, char *name, char *filetype,
        char *ext,
        struct HELPCTX *hctx, MakeSLpFilepairTrap *trap,
	/*void(*trap)(void*,long*,long*,char*,char*,long*),*/
        void *data, char *filename1, char *filename2,
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
