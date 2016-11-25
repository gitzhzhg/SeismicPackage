
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
#ifndef _FileChoicP_h
#define _FileChoicP_h




#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <X11/Core.h>



#include "file_choice.h"
#include "wproc.h"



/* 
 * New fields for the FileChoice widget CLASS record 
 */

typedef struct {
        int make_compiler_happy;        /* keep compiler happy */
        char    *default_path;
        Boolean  use_default_path;
} FileChoiceClassPart;


/* 
 * Full class record declaration 
 */
typedef struct _FileChoiceClassRec {
   CoreClassPart       core_class;
   CompositeClassPart  composite_class;
   ConstraintClassPart constraint_class;
   XmManagerClassPart  manager_class;
   FileChoiceClassPart     fileChoice_class;
} FileChoiceClassRec;

extern  FileChoiceClassRec fileChoiceClassRec;



/* 
 * New fields for the FileChoice widget INSTANCE record 
 */
typedef struct {
    /* Resources */
    wprocAnnoType  anno_type;       /*annotation (label, pushb, none)*/
    Widget         errbox;          /* error display widget */
    char           *anno_labelstr;  /* label on pushb or label widget */
    char           *filenamePtr;    /* filename */
    char           *file_addr;      /* user specified address */
    char           *ext;            /* file ext that will if none exist*/
    CTXH_HELPCTX   helpctx;         /* ADD - helpctx */
    Boolean        use_none;        /* use NONE to represent no file*/ 
    char           *none_str;       /* string to use for NONE */
    unsigned long  file_flags;      /* file flags */
    char           *file_desc;      /* file description that will popup */
    WidgetList     fcwigs;          /* ADD - list of fc widget to update*/
    int            text_offset;     /* text offset */
    int            percent_right;   /* percent right of left edge */
    int            offset_right;    /* offset right of left edge */
    Boolean        validate_on_sel; /* validate on selection */
    int            text_align_type; /* wprocOffsetAnno, wprocOffsetAuto,
                                     * wprocOffsetRight,
                                     * wprocOffsetWithWidget   (not implem.),
                                     * wprocOffsetWithFCWidget (not implem.)
                                     */
    XtCallbackList succ_callback;   /* file succfully choosen  */
    XtCallbackList fail_callback;   /* bad file choosen  */
    XtCallbackList filesel_callback;/*file choosen out of file sel box*/
    
    /* Private */
    Widget           textw;         /* text widget */
    Widget           annow;         /* annotation widget(label or push) */
    Widget           fsbpopw;       /* file sel box widget popup */
    Boolean doanno;
    struct FILE_INFO file_info;
    char             filename[FILENAMELEN];
    struct CB        tcb[1];
    Dimension        init_height;
    Dimension        init_width;
    long             good_file;    /* ADD */
    XmSearchProc     default_dir_proc;
    XmSearchProc     default_file_proc;

} FileChoicePart;

/*
 * Full instance record declaration
 */
typedef struct _FileChoiceRec {
   CorePart         core;
   CompositePart    composite;
   ConstraintPart   constraint;
   XmManagerPart    manager;
   FileChoicePart   fileChoice;
} FileChoiceRec;


#define OKSTR   "OK"
#define NOSTR   "No"
#define YESSTR  "Yes"

#define set_nofile(fi,tw)                                                 \
           if ((fi)->use_none) {                                          \
                      if ( (fi)->none_str ) {                             \
                              XmTextSetString( (tw), (fi)->none_str);  }  \
                      else {  XmTextSetString( (tw), "" ); }              \
          } else  XmTextSetString( (tw), "" )

#endif
/* DON'T ADD STUFF AFTER THIS #endif */
