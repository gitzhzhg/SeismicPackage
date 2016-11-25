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
 * Widget Name : FileChoice
 * File        : FileChoice.c
 * Author      : Trey Roby
 * Date        : 12/1/92
 * Last revised: 99/03/09 Day
 *
 *      This widget is used for selecting a file for input or output.
 *
 * CHANGES:
 *1.  99/03/09   Day  changed an XtFree in wprocFileChoiceSBSetDir
 *               to a free call
 */

#ifdef _AIX
#endif

 
#include <X11/IntrinsicP.h> 
#include <X11/StringDefs.h>
#include <Xm/XmP.h>
#include <Xm/FormP.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/MessageB.h>
#include <Xm/TextF.h>
#include <assert.h>
#include "file_choice_p.h"
#include "wproc.h"
#include "cprim.h"
#include "cenv.h"

#include <stdio.h>
#include <string.h>

#if defined(VMS) && defined(__GNUC__)
#include <stdlib.h>	/* strtok is in stdlib.h for VMS GNU */
#endif

/*
 * declare some constants
 */

#define FC_DEBUG 0



/*
 * =======================  RESOURCES SECTION =============================
 */

#define offset(field)   XtOffsetOf(FileChoiceRec, field)


static XtResource resources[] = {
      { 
        XmNannoType,       XmCAnnoType,     wprocRAnnoType, sizeof(char *), 
        offset(fileChoice.anno_type), XtRImmediate, 
        (XtPointer)wprocPushButton 
      },

      { 
        XmNerrorWidget,    XmCErrorWidget,  XmRWidget,      sizeof(Widget),
        offset(fileChoice.errbox),          XtRImmediate,   (XtPointer)0 
      },

      {
         XmNfilename,       XmCFilename,     XtRString, sizeof(char *),
         offset(fileChoice.filenamePtr), XtRString, "" 
      },

      {
         XmNfailCallback, XmCFailCallback, XmRCallback,
         sizeof(XtCallbackList), offset(fileChoice.fail_callback), 
         XmRCallback,    NULL 
      },

      {
         XmNfileDescription, XmCFileDescription, XtRString, sizeof(char *),
         offset(fileChoice.file_desc),   XtRString,     "File"
      },

      {
         XmNfileExtension,  XmCFileExtension,   XtRString, sizeof(char *),
         offset(fileChoice.ext),         XtRString,     "" 
      },

      { 
        XmNfileFlags,       XmCFileFlags, wprocRFileFlgType,  
        sizeof(unsigned long), offset(fileChoice.file_flags),   
        XtRImmediate,      (XtPointer)0 
      },

      { 
         XmNfileSelCallback, XmCFileSelCallback, XmRCallback,
         sizeof(XtCallbackList), offset(fileChoice.filesel_callback), 
         XmRCallback,    NULL 
      },

      {
         XmNfileTargetAddr, XmCFileTargetAddr,    XtRInt, sizeof(char *),
         offset(fileChoice.file_addr), XtRImmediate, NULL
      },

      { 
        XmNlabelString,       XmCLabelString,  XtRString, sizeof(char *),
        offset(fileChoice.anno_labelstr), XtRString,
        "anno"
      },

      {
         XmNnoneStr,     XmCNoneStr,   XtRString,         sizeof(char *),
         offset(fileChoice.none_str),  XtRString,         NOFILESTR
      },

      { 
         XmNoffsetRight,       XmCOffsetRight,   XtRInt, sizeof(int),
         offset(fileChoice.offset_right), XtRImmediate,      (XtPointer) 0 
      },

      {
         XmNpercentRight,        XmCPercentRight,   XtRInt, sizeof(int),
         offset(fileChoice.percent_right), XtRImmediate,      (XtPointer) 0 
      },

      {
         XmNsuccessCallback, XmCSuccessCallback, XmRCallback,
         sizeof(XtCallbackList), offset(fileChoice.succ_callback), 
         XmRCallback,    NULL 
      },

      {
        XmNtextAlignType,   XmCTextAlignType, wprocRAlignType, sizeof(char *),
        offset(fileChoice.text_align_type), XtRImmediate,      
        (XtPointer) wprocOffsetAuto
      },

      { 
        XmNtextOffset,      XmCTextOffset,   XtRInt, sizeof(int),
        offset(fileChoice.text_offset), XtRImmediate,      (XtPointer) 4
      },

      {
         XmNuseNoneStr,     XmCUseNoneStr,   XtRBoolean, sizeof(Boolean),
         offset(fileChoice.use_none), XtRImmediate,    (XtPointer)True 
      },

      {
         XmNvalidateOnSelection, XmCValidateOnSelection,   XtRBoolean, 
         sizeof(Boolean), offset(fileChoice.validate_on_sel), 
         XtRImmediate,    (XtPointer)True 
      },


   };


/*
 * ======= DECLARATIONS - Methods, Private & Public FUNCTIONS SECTION =======
 */

/* 
 * Declaration of methods
 */
static void Initialize();
static Boolean SetValues();
static void Resize();
static void ChangeManaged();
static void GetValuesHook();
static void ClassInitialize();
static XtGeometryResult QueryGeometry();
static XtGeometryResult GeometryManager();


/* these Core methods not needed by FileChoice.c
 *
 * static void Realize();
 * static void Redisplay();
 * static void Destroy();
 */


/* 
 * Declaration of private functions 
 */
static void dolayout( Widget w);
static Position decide_align( Widget w);
static void gainfoc();
static long call_succ_cb();
static long call_fail_cb();
static void call_filesel_cb();
static void set_path_cb(Widget w, XtPointer, XtPointer);
static void file_search_proc(Widget w, XmFileSelectionBoxCallbackStruct *cbs );
static void dir_search_proc(Widget w, XmFileSelectionBoxCallbackStruct *cbs );


/* 
 * Declaration of converter functions
 */
static Boolean CvtStrToAnnoType();
static Boolean CvtStrToAlignType();
static Boolean CvtStrToFileFlgType();

/* 
 * Declaration of external functions 
 */
extern dofilecb();
extern Widget pickfile();
Boolean wprocFileChoiceNoFile( struct FILE_INFO *fi,
                               char             *fname );


/*
 *=======================  CLASS REC INIT SECTION =============================
 */

FileChoiceClassRec fileChoiceClassRec = {
    {
    /* core_class fields */
        /* superclass                   */ (WidgetClass) &xmManagerClassRec,
        /* class_name                   */ "FileChoice",
        /* widget_size                  */ sizeof(FileChoiceRec),
        /* class_initialize             */ ClassInitialize,
        /* class_part_initialize        */ NULL,
        /* class_inited                 */ False,
        /* initialize                   */ Initialize,
        /* initialize_hook              */ NULL,
        /* realize                      */ XtInheritRealize,
        /* actions                      */ NULL,   /* actions */
        /* num_actions                  */ 0,       /* XtNumber(actions), */
        /* resources                    */ resources,
        /* num_resources                */ XtNumber(resources),
        /* xrm_class                    */ NULLQUARK,
        /* compress_motion              */ True,
        /* compress_exposure            */ XtExposeCompressMultiple,
        /* compress_enterleave          */ True,
        /* visible_interest             */ False,
        /* destroy                      */ NULL,
        /* resize                       */ Resize,
        /* expose                       */ NULL,       /* Redisplay, */
        /* set_values                   */ SetValues,
        /* set_values_hook              */ NULL,
        /* set_values_almost            */ XtInheritSetValuesAlmost,
        /* get_values_hook              */ GetValuesHook,
        /* accept_focus                 */ NULL,
        /* version                      */ XtVersion,
        /* callback_private             */ NULL,
        /* tm_table                     */ NULL,   /* defaultTranslations, */
        /* query_geometry               */ QueryGeometry,
        /* display_accelerator          */ NULL,
        /* extension                    */ NULL
    },

    {
    /* composite_class fields */
        /* geometry_manager             */ GeometryManager,
        /* change_managed               */ ChangeManaged,
        /* insert_child                 */ XtInheritInsertChild,
        /* delete_child                 */ XtInheritDeleteChild,
        /* extension                    */ NULL,
    },
    {
    /* constraint_class fields */
        /* resources                    */ NULL,
        /* num_resources                */ 0,
        /* constraint_size              */ 0,
        /* initialize                   */ NULL,
        /* destroy                      */ NULL,
        /* set_values                   */ NULL,
        /* extension                    */ NULL
    },
    {
    /* manager_class fields */
        /* translations                 */  XtInheritTranslations,
        /* syn_resources                */  NULL,
        /* num_syn_resources            */  0,
        /* syn_constraint_resources     */  NULL,
        /* num_syn_constraint_resources */  0,
        /* parent_process               */  XmInheritParentProcess,
        /* extension                    */  NULL
    },
   {
    /* FileChoice class fields */
        /* make_compiler_happy          */ 1,
        /* default_path                 */ NULL,
        /* use_default_path             */ True
   }
};

/*
 * declare widget class
 */

WidgetClass fileChoiceWidgetClass = (WidgetClass)&fileChoiceClassRec;



#define SVMGS       "WprocFileChoice: %s cannot be set by XtSetValues."
#define FILESTRNULL \
  "FileChoice: wprocFileChoiceGetFileByStr: filename parameter NULL."

#define CANNOT_CHANGE_ANNOSTR \
  "FileChoice: The XmNannoType resource may only be set at creation time."

#define CANNOT_CHANGE_ERRBOXSTR \
  "FileChoice: The XmNerrorWidget resource may only be set at creation time."

#define BAD_ERRWIDGET_STR \
     "FileChoice: the XmNerrorWidget is not set to a supported widget type.\n"




/*
 * ========================================================================== 
 * ================================== METHODS ===============================
 * ========================================================================== 
 */ 

/*
 * ========================== ClassInitialize METHOD ========================
 */ 
static void ClassInitialize()
{

#if (FC_DEBUG)
  puts( "ClassInit called"); 
#endif

  XtSetTypeConverter( XtRString, wprocRAnnoType, CvtStrToAnnoType,
                      (XtConvertArgList) NULL, 0, XtCacheAll , NULL );

  XtSetTypeConverter( XtRString, wprocRAlignType, CvtStrToAlignType,
                      (XtConvertArgList) NULL, 0, XtCacheAll , NULL );

  XtSetTypeConverter( XtRString, wprocRFileFlgType, CvtStrToFileFlgType,
                      (XtConvertArgList) NULL, 0, XtCacheAll , NULL );

}



/*
 * ============================ Initialize METHOD ======================== 
 */ 
static void Initialize( Widget    treq, 
                        Widget    tnew, 
                        ArgList   args,
                        Cardinal  *num_args )

{
  Dimension aw, ah;


  FileChoiceWidget fio= (FileChoiceWidget) tnew;

#if (FC_DEBUG)
  puts( "init called"); 
#endif

  
   if (!fio->fileChoice.errbox)
          fio->fileChoice.errbox= make_okbox( (Widget) fio, "Error",  
                                               XmDIALOG_ERROR);
   else {
        Widget w= fio->fileChoice.errbox;
        if (!( (XtClass(w) == xmMessageBoxWidgetClass) ||
               (XtClass(w) == xmTextFieldWidgetClass)  ||
               (XtClass(w) == xmTextWidgetClass)       ||
               (XtClass(w) == xmLabelWidgetClass)      ||
               (XtClass(w) == xmLabelGadgetClass)      ||
               (XtClass(w) == xmPushButtonWidgetClass) ||
               (XtClass(w) == xmPushButtonGadgetClass) ) ) {
                    XtWarning( BAD_ERRWIDGET_STR );
                    fio->fileChoice.errbox= 
                            make_okbox( (Widget)fio, "Error",  XmDIALOG_ERROR);
        }
   }

  /*
   * create a text widget and file io callback 
   */
   strcpy( fio->fileChoice.filename, fio->fileChoice.filenamePtr);
   fio->fileChoice.filenamePtr= fio->fileChoice.filename;
   fio->fileChoice.file_info.filename= fio->fileChoice.filename;
   strcpy(fio->fileChoice.file_info.sav_filename, "@#$%#$#(+" );
   strcpy( fio->fileChoice.file_info.ext,  fio->fileChoice.ext );
   strcpy( fio->fileChoice.file_info.file_desc, fio->fileChoice.file_desc );

   fio->fileChoice.file_info.none_str= fio->fileChoice.none_str;
   fio->fileChoice.file_info.use_none= fio->fileChoice.use_none;
   fio->fileChoice.file_info.file_mod_time= 0;

   fio->fileChoice.file_info.good_file= &fio->fileChoice.good_file;
   fio->fileChoice.file_info.succfunc= call_succ_cb;
   fio->fileChoice.file_info.failfunc= call_fail_cb;
   fio->fileChoice.file_info.succdata= (void*)fio;;
   fio->fileChoice.file_info.twigs= NULL;
   fio->fileChoice.file_info.net_file= NULL;

   set_CB( fio->fileChoice.tcb, 0, &fio->fileChoice.file_info, 
           fio->fileChoice.errbox, fio->fileChoice.filename, TYPE_FILE);
           
   fio->fileChoice.textw= XtCreateManagedWidget("text", xmTextWidgetClass, 
                                     (Widget)fio, NULL, 0);
   XtAddCallback( fio->fileChoice.textw, XmNfocusCallback, gainfoc, NULL);
   XtAddCallback( fio->fileChoice.textw, XmNlosingFocusCallback,
                  (XtCallbackProc)dofilecb, (XtPointer)&fio->fileChoice.tcb[0]);
   XtAddCallback( fio->fileChoice.textw, XmNactivateCallback,
                 (XtCallbackProc)dofilecb, (XtPointer)&fio->fileChoice.tcb[0]);


  /*
   * create a push button or a label or nothing
   */
  if (fio->fileChoice.anno_type == wprocLabel) {
      if (fio->fileChoice.file_flags == 0) {
          wprocFileChoiceSetFlags( (Widget)fio, 
                                    wprocIsRequiredMask|wprocWritableMask);
      } /* End if*/
      else
          fio->fileChoice.file_info.check_flags= fio->fileChoice.file_flags;
      fio->fileChoice.doanno= True;
      fio->fileChoice.annow= 
         XtCreateManagedWidget("anno", xmLabelWidgetClass, (Widget)fio, NULL,0);
      set_label( fio->fileChoice.annow, fio->fileChoice.anno_labelstr );
      fio->fileChoice.fsbpopw= NULL;
  }
  else if (fio->fileChoice.anno_type == wprocPushButton) {
        if (fio->fileChoice.file_flags == 0) {
            wprocFileChoiceSetFlags( (Widget)fio, wprocMustExistMask);
        ENDif
        else
              fio->fileChoice.file_info.check_flags=
                                        fio->fileChoice.file_flags;
        fio->fileChoice.doanno= True;
        fio->fileChoice.annow= XtCreateManagedWidget( "anno", 
                                                 xmPushButtonWidgetClass,
                                                 (Widget)fio, NULL, 0);
        set_label( fio->fileChoice.annow, fio->fileChoice.anno_labelstr );
        fio->fileChoice.fsbpopw= pickfile(  fio->fileChoice.textw,  
                                            fio->fileChoice.annow,
                                            fio->fileChoice.filename, 
                                            call_filesel_cb, fio );
        XtVaGetValues(fio->fileChoice.fsbpopw, 
                        XmNdirSearchProc,  &fio->fileChoice.default_dir_proc,
                        XmNfileSearchProc, &fio->fileChoice.default_file_proc,
                        NULL);
        XtVaSetValues(fio->fileChoice.fsbpopw, 
                        XmNdirSearchProc,  dir_search_proc, 
                        XmNfileSearchProc, file_search_proc,
                        XmNuserData,       (XtPointer)fio,
                        NULL);
        XtAddCallback(fio->fileChoice.annow, XmNactivateCallback,
                      (XtCallbackProc)set_path_cb, (XtPointer)fio);
  }
  else {
     fio->fileChoice.doanno= False;
  }

  if ( fio->fileChoice.doanno) {
       ah= fio->fileChoice.annow->core.height;
       aw= fio->fileChoice.annow->core.width;
  }
  else {
       ah= 0;
       aw= 0;
  }



  fio->fileChoice.init_height= (ah > fio->fileChoice.textw->core.height) ? 
                            ah : fio->fileChoice.textw->core.height;
  fio->fileChoice.init_height+= fio->core.border_width;
  fio->core.height= fio->fileChoice.init_height;

  fio->fileChoice.init_width= aw + fio->fileChoice.textw->core.width +
                                          fio->core.border_width;

  wprocFileChoiceSetFile( tnew, fio->fileChoice.filename, False );
  strcpy(fio->fileChoice.file_info.sav_filename, "@#$%#$#(+" );
  fio->fileChoice.good_file= False;
  fio->fileChoice.file_info.init= False;
}


/*
 * ============================ QueryGeometry METHOD ======================== 
 */ 
static XtGeometryResult QueryGeometry( Widget w,
                                       XtWidgetGeometry *request, 
                                       XtWidgetGeometry *reply_return)
{ 
    FileChoiceWidget fio= (FileChoiceWidget) w;
    XtGeometryResult result= XtGeometryYes;

#if (FC_DEBUG)
    puts( "query geometry called");
#endif

    request->request_mode &= (CWWidth | CWHeight);

    if (request->request_mode == 0)
            result= XtGeometryYes;
    /* if proposed size is large enough, accept it.  Otherwise,
     * suggest our arbitrary initial size. 
     */
    else {
       if (request->request_mode & CWHeight) {
            if (request->height < fio->fileChoice.init_height) {
                  result = XtGeometryAlmost;
                  reply_return->height = fio->fileChoice.init_height;
                  reply_return->request_mode &= CWHeight;
            ENDif
            else
                  result = XtGeometryYes;
       ENDif
       if (request->request_mode & CWWidth) {
            if (request->width < fio->fileChoice.init_width) {
                 result = XtGeometryAlmost;
                 reply_return->width = fio->fileChoice.init_width;
                 reply_return->request_mode &= CWWidth;
            ENDif
            else
                 result = XtGeometryYes;
       ENDif

     ENDelse


    return(result);


}

/*
 * ============================ ChangeManaged METHOD ======================== 
 */
static void ChangeManaged(Widget w)

{
#if (FC_DEBUG)
 puts( "ChangeManaged called");
#endif
 dolayout( w);
}


/*
 * ============================ Resize METHOD ======================== 
 */
static void Resize(Widget w)

{
#if (FC_DEBUG)
 puts( "resize called");
#endif
 dolayout( w);
}


/*
 * ============================ GeometryManager METHOD ======================== 
 */
static XtGeometryResult GeometryManager( Widget           w,
                                         XtWidgetGeometry *request,
                                         XtWidgetGeometry *reply )


{ 
#if (FC_DEBUG)
puts( "geometry manager called"); 
#endif

return XtGeometryYes;

}

/*
 * ============================ SetValues METHOD ======================== 
 */
static Boolean SetValues( Widget   current, 
                          Widget   request,
                          Widget   new  )


 {
  FileChoiceWidget fiocurr= (FileChoiceWidget) current;
  FileChoiceWidget fionew= (FileChoiceWidget)  new;
  FileChoicePart *fupcurr=  &fiocurr->fileChoice;
  FileChoicePart *fupnew=   &fionew->fileChoice;
  Boolean retval= False;




#if (FC_DEBUG)
  puts( "set values called"); 
#endif

   if (fupnew->anno_type != fupcurr->anno_type) {
            fupnew->anno_type= fupcurr->anno_type;
            XtWarning(CANNOT_CHANGE_ANNOSTR);
   ENDif

   if (fupnew->errbox != fupcurr->errbox) {
            fupnew->anno_type= fupcurr->anno_type;
            XtWarning(CANNOT_CHANGE_ERRBOXSTR);
   ENDif

   if ( fupnew->filenamePtr) {
          if (strcmp( fupnew->filenamePtr, fupcurr->filename ) != 0) {
                   wprocFileChoiceSetFile( new, fupnew->filenamePtr, True);
          ENDif
   ENDif
   else
          wprocFileChoiceSetFile( new, fupnew->filenamePtr, True);

   if (strcmpnull ( fupnew->anno_labelstr, fupcurr->anno_labelstr ) != 0) {
        set_label( fupnew->annow, fupnew->anno_labelstr );
        retval= True;
   ENDif

   if ( fupnew->file_flags != fupcurr->file_flags ) {
        fupnew->file_info.check_flags= fupnew->file_flags;
   ENDif


   if (strcmpnull ( fupnew->file_desc, fupcurr->file_desc ) != 0) {
        if (fupnew->file_desc)
            strncpy( fupnew->file_info.file_desc, fupnew->file_desc, DESCLEN);
        else fupnew->file_info.file_desc[0]= '\0';
   ENDif

   if (strcmpnull( fupnew->ext, fupcurr->ext ) != 0) {
        if (fupnew->ext)
            strncpy( fupnew->file_info.ext, fupnew->ext, NAMELEN );
        else fupnew->file_info.ext[0]= '\0';
   ENDif

   if (strcmpnull( fupnew->none_str, fupcurr->none_str ) != 0) {
            fupnew->file_info.none_str= fupnew->none_str;
   ENDif

   if ( fupnew->use_none != fupcurr->use_none ) {
        fupnew->file_info.use_none= fupnew->use_none;
   ENDif

   if ( ( fupnew->text_align_type != fupcurr->text_align_type ) OR
        ( fupnew->offset_right    != fupcurr->offset_right    ) OR
        ( fupnew->percent_right   != fupcurr->percent_right   ) ){
        retval= True;
   ENDif

   return True;
}


/*
 * ================== FUNCTION dolayout ===============================
 */
static void dolayout( Widget w)

{
 Position ax, ay;          /* anno widget x & y */
 Position tx, ty;          /* text widget x & y */
 Dimension tw, th;         /* text widget width & height */
 FileChoiceWidget fio= (FileChoiceWidget) w;

/*
 * set sizes
 */
 if (fio->fileChoice.doanno) {
    ax=0;
    ay=0;
    ty= 0;
    tx= decide_align( w );
    tw= fio->core.width - (2 * fio->core.border_width) - tx;
    XtMoveWidget(fio->fileChoice.annow, ax, ay);
 ENDif
 else {
    ty= 0;
    tx= fio->fileChoice.text_offset;
    tw= fio->core.width - (2 * fio->core.border_width);
    th= fio->core.height - (2 * fio->core.border_width);
 ENDelse

 XtResizeWidget(fio->fileChoice.textw, tw, fio->fileChoice.textw->core.height, 1);
 XtMoveWidget(fio->fileChoice.textw, tx, ty);


}

/*
 * ================== FUNCTION decide_align ==========================
 */
static Position decide_align( Widget   w) 

{
 Position tx=0;          /* text widget x & y */
 FileChoiceWidget fio= (FileChoiceWidget) w;
 FileChoiceWidget tmpfio;
 long max= -1;
 long tst, i;
 Widget p;
 Cardinal wcnt;
 WidgetList children;


 switch ( fio->fileChoice.text_align_type ) {

    case wprocOffsetAuto:
              p= fio->core.parent;
              XtVaGetValues( p,  XmNchildren,    &children,
                                 XmNnumChildren, &wcnt, NULL);

              for(i=0;(i<wcnt); i++) {
                  if ( XtClass( children[i]) == fileChoiceWidgetClass ) {
                         tmpfio= (FileChoiceWidget)children[i];
                         if (tmpfio->fileChoice.doanno) {
                             tst=  tmpfio->fileChoice.annow->core.width +
                                   tmpfio->fileChoice.text_offset;
                             if (max < tst)
                                     max= tst;
                         ENDif
                  ENDif
              ENDloop
              tx= max;

              break;

    case wprocOffsetAnno:
              tx= fio->fileChoice.annow->core.width+ 
                                   fio->fileChoice.text_offset;
              break;

    case wprocOffsetRight:
              tx= fio->fileChoice.text_offset;
              break;

    case wprocOffsetWithWidget:
              tx= fio->fileChoice.text_offset;
                         break;
    case wprocOffsetWithFCWidget:
              tx= fio->fileChoice.text_offset;
                         break;

 ENDswitch


 return tx;

}

/*
 * ================== FUNCTION set_path_cb ==================================
 */
static void set_path_cb(Widget w, XtPointer user, XtPointer dummy)
{
  FileChoiceWidget fio= (FileChoiceWidget)user;
  FileChoiceClassPart *fclass= &fileChoiceClassRec.fileChoice_class;

  if (fclass->use_default_path && fclass->default_path) {
      wprocFileChoiceSBSetDir((Widget)fio, fclass->default_path);
  }

}

/*
 * ================== FUNCTION gainfoc ==================================
 */
static void gainfoc( Widget              w,
                     struct CURRFLD_INFO *curr,
                     XmAnyCallbackStruct *cbinfo )
    /*
     * gaining focuse callback on text widget to set highlight on
     */

{
 long len;
 char *str;

 str= XmTextGetString(w);
 len=strlen(str);
 XtFree(str);
 XmTextSetSelection( w, 0, len, CurrentTime);

}




/*
 * ================== FUNCTION dir_search_proc ==============================
 */
static void dir_search_proc(Widget w, XmFileSelectionBoxCallbackStruct *cbs )
{
  FileChoiceWidget fio;
  void *user_data;
  unsigned char stat;
  AltFileAccess *net;
  XmStringTable updated_list;

  XtVaGetValues( w, XmNuserData, &user_data, NULL);
  fio= (FileChoiceWidget)user_data;
  net= fio->fileChoice.file_info.net_file;
  assert(fio);
  if ( net && *net->objptr) {
      int count, i;
      char **dir_list= NULL;
      char *dname= get_string_from_xmstr(cbs->dir);
      updated_list= NULL;
      dir_list= net->get_dir_dir_func(*net->objptr, dname, &count, &stat);
      if (dir_list || stat) {
           if (count > 0) {
               updated_list= malloc( sizeof(XmString) *  count );
               for (i=0; (i<count); i++) {
                    updated_list[i]= 
                      XmStringCreateLtoR(dir_list[i],XmSTRING_DEFAULT_CHARSET);
               }
           }
           XtVaSetValues(w, XmNdirListItems,      updated_list,
                            XmNdirListItemCount,  count,
                            XmNdirectoryValid,    True,
                            XmNlistUpdated,       True, NULL);
           for (i=0; (i<count); i++) {
                free(dir_list[i]);
                /* XmStringFree(updated_list[i]); */
           }
           /* if (updated_list) free(updated_list);  */
           if (dir_list) free(dir_list); 
      }
      else {
           XtVaSetValues(w, XmNdirectoryValid, False,
                            XmNlistUpdated,    False, NULL);
      }

      free(dname);
  } 
  else {
      fio->fileChoice.default_dir_proc(w,(XtPointer)cbs); 
  }
}



/*
 * ================== FUNCTION file_search_proc ============================
 */
static void file_search_proc(Widget w, XmFileSelectionBoxCallbackStruct *cbs )
{
  FileChoiceWidget fio;
  void *user_data;
  AltFileAccess *net;
  unsigned char stat;
  XmStringTable updated_list= NULL;

  XtVaGetValues( w, XmNuserData, &user_data, NULL);
  fio= (FileChoiceWidget)user_data;
  net= fio->fileChoice.file_info.net_file;
  assert(fio);
  if ( net && *net->objptr) {
      char *fullpath;
      /*char fullpath[1000];
       *char *dir, *pattern;
       */
      int count, i;
      char **file_list= NULL;
      updated_list= NULL;

      /*dir= get_string_from_xmstr(cbs->dir);
       *pattern= get_string_from_xmstr(cbs->pattern);
       *sprintf(fullpath, "%s%s", dir, pattern);
       */
      fullpath= get_string_from_xmstr(cbs->mask);
      /*
       * build file mask
       */
      file_list= net->get_dir_func(*net->objptr, fullpath, &count, &stat);
      if (file_list || stat) {
           if (count > 0) {
               updated_list= malloc( sizeof(XmString) *  count );
               for (i=0; (i<count); i++) {
                    updated_list[i]= 
                      XmStringCreateLtoR(file_list[i],XmSTRING_DEFAULT_CHARSET);
               }
           }


           XtVaSetValues(w, XmNfileListItems,      updated_list,
                            XmNfileListItemCount,  count,
                            XmNlistUpdated,       True, NULL);
           for (i=0; (i<count); i++) {
                free(file_list[i]);
                /* XmStringFree(updated_list[i]); */
           }
           /* if (updated_list) free(updated_list); */
           if (file_list) free(file_list); 

      }
      else
           XtVaSetValues(w, XmNlistUpdated,       True, NULL);
  } 
  else {
      fio->fileChoice.default_file_proc(w,(XtPointer)cbs); 
  }
}




/*
 * ================== FUNCTION call_succ_cb ==================================
 */
static long call_succ_cb( Widget               w,
                          struct FILE_INFO    *fi,
                          long                *nothing,
                          XmAnyCallbackStruct *cbinfo )

     /*
      * call success callbacks
      */
{
  wprocFileChoiceSuccCallbackStruct send;
  FileChoiceWidget fio;
  FileChoiceClassPart *fclass= &fileChoiceClassRec.fileChoice_class;
  char *bc_filename;

  fio= (FileChoiceWidget) XtParent(w);

  send.reason      = wprocSuccessCallbackReason; 
  send.doit        = True; 
  send.filename    = bc_filename = XtNewString(fio->fileChoice.filename);
  send.sav_filename= fio->fileChoice.file_info.sav_filename;
  send.check_flags = fio->fileChoice.file_flags;
  send.fail_str    = NULL;
  send.tw          = cbinfo;


  if (fio->fileChoice.file_addr)
              strcpy( fio->fileChoice.file_addr, fio->fileChoice.filename ); 

  XtCallCallbackList( (Widget)fio, 
                       fio->fileChoice.succ_callback, (XtPointer)&send);


  if (send.doit) {
        wprocFileChoiceSetFile( (Widget)fio, send.filename, False);
        if (fclass->use_default_path) {
               if (fclass->default_path) free(fclass->default_path);
               fclass->default_path= wprocFileChoiceSBGetDir((Widget)fio) ;
        }
  }

  /*
   * call the show error routine.  If the string is NULL nothing will be 
   * displayed.  This allows the user to display an informational message. 
   */
  if (send.fail_str) {
          wprocFileChoiceShowErr( (Widget)fio, send.fail_str);
          fi->fail_message= NULL;
  }

  XtFree( bc_filename);

  return (long)send.doit;
}

/*
 * ================== FUNCTION call_fail_cb ==================================
 */
static long call_fail_cb( Widget               w,
                          struct FILE_INFO    *fi,
                          long                *nothing,
                          XmAnyCallbackStruct *cbinfo )

     /*
      * call fail callbacks
      */
{
  wprocFileChoiceFailCallbackStruct send;
  FileChoiceWidget fio;
  char *bc_filename;


  fio= (FileChoiceWidget) XtParent(w);

  send.reason      = wprocFailCallbackReason; 
  send.doit        = True; 
  send.filename    = bc_filename = XtNewString(fio->fileChoice.filename);
  send.sav_filename= fio->fileChoice.file_info.sav_filename;
  send.check_flags = fio->fileChoice.file_flags;
  send.fail_str    = fi->fail_message;
  send.tw          = cbinfo;


  XtCallCallbackList( (Widget)fio, fio->fileChoice.fail_callback, 
                      (XtPointer)&send);

  fi->fail_message= send.fail_str;

  if (strcmp(fio->fileChoice.filename, send.filename) != 0) {
      strcpy( fio->fileChoice.filename, send.filename);
      XmTextSetString( fio->fileChoice.textw, fio->fileChoice.filename );
  }

  XtFree( bc_filename);

  return (long)send.doit;
}



/*
 * ================== FUNCTION call_filesel_cb ===============================
 */
static void call_filesel_cb( Widget              w,
                             FileChoiceWidget    fio,
                             XmAnyCallbackStruct *cbinfo )

{
  wprocFileChoiceFileSelCallbackStruct send;

  send.reason      = wprocFileSelCallbackReason; 
  send.filename    = fio->fileChoice.filename;
  send.sbw         = cbinfo;


  XtCallCallbackList( (Widget)fio, fio->fileChoice.filesel_callback, (XtPointer)&send);


  if (fio->fileChoice.validate_on_sel)  {
        wprocFileChoiceValidate( (Widget)fio, False);
  }
}


/*
 * ============================ GetValuesHook METHOD ======================== 
 */
static void GetValuesHook( Widget  w,
                           ArgList args,
                           Cardinal *num_args_ptr )
{
    Cardinal num_args = *num_args_ptr;
    FileChoiceWidget fio= (FileChoiceWidget) w;
    int i;

#if (FC_DEBUG)
    puts( "GetValuesHook");
#endif

    for (i = 0; i < num_args; i++) {
        if (strcmp(args[i].name, XmNfilename) == 0) {
           *((XtPointer *)args[i].value) = (XtPointer)fio->fileChoice.filename;
        }
    }

}


/*
 *  ========================================================================== 
 *  =========================== TYPE CONVERTERS ============================== 
 *  ========================================================================== 
 */



/*
 *  ====================== TYPE CONVERTER CvtStrToAnnoType =================== 
 */
static Boolean CvtStrToAnnoType( Display      *dpy,
                                 XrmValuePtr  args, 
                                 Cardinal     *num_args,
                                 XrmValuePtr  fromVal, 
                                 XrmValuePtr  toVal,
                                 XtPointer    cvt_data  )

{
 char instr[100];
 static wprocAnnoType retval;
 int i;

#if (FC_DEBUG)
  puts( "CvtStrToAnnoType");
#endif


  strncpy(instr, (char *)fromVal->addr, 100);

  if (*num_args != 0)
      XtWarningMsg( "wrongParameters", "CvtStrToAnnoType", "FileChoice error",
                    "No extra arguments needed", (String *) NULL,
                    (Cardinal *) NULL );

  
  for (i=0; (instr[i] != '\0'); instr[i]= toupper(instr[i]), i++ );

  if      (strcmp(instr, wprocLabelStr) == 0)      retval= wprocLabel;
  else if (strcmp(instr, wprocPushButtonStr) == 0) retval=wprocPushButton;
  else if (strcmp(instr, wprocNoAnnoStr) == 0)     retval=wprocNoAnno;
  else {
          retval= wprocPushButton;           
          XtDisplayStringConversionWarning( dpy, (char *)fromVal->addr, 
                                            wprocRAnnoType);
  ENDelse
 
  if ( (toVal->addr) AND (toVal->size >= sizeof (wprocAnnoType)) ) {
        *(wprocAnnoType *)toVal->addr= retval;
  ENDif
  else {
        toVal->addr= (caddr_t)&retval;
  ENDelse
  toVal->size= sizeof (wprocAnnoType);

  return True;

}

/*
 *  ====================== TYPE CONVERTER CvtStrToAlignType=================== 
 */
static Boolean CvtStrToAlignType( Display      *dpy,
                                  XrmValuePtr  args, 
                                  Cardinal     *num_args,
                                  XrmValuePtr  fromVal, 
                                  XrmValuePtr  toVal,
                                  XtPointer    cvt_data  )

{
 char instr[100];
 static wprocAlignType retval;
 int i;

#if (FC_DEBUG)
  puts( "CvtStrToAlignType");
#endif


  strncpy(instr, (char *)fromVal->addr, 100);

  if (*num_args != 0)
      XtWarningMsg( "wrongParameters", "CvtStrToAnnoType", "FileChoice error",
                    "No extra arguments needed", (String *) NULL,
                    (Cardinal *) NULL );

  
  for (i=0; (instr[i] != '\0'); instr[i]= toupper(instr[i]), i++ );

  if      (strcmp(instr, wprocOffsetAnnoStr) == 0)   retval= wprocOffsetAnno;
  else if (strcmp(instr, wprocOffsetAutoStr) == 0)   retval=wprocOffsetAuto;
  else if (strcmp(instr, wprocOffsetRightStr) == 0)  retval=wprocOffsetRight;
  else if (strcmp(instr, wprocOffsetWithWidgetStr) == 0) 
                                               retval=wprocOffsetWithWidget;
  else if (strcmp(instr, wprocOffsetWithFCWidgetStr) == 0) 
                                               retval=wprocOffsetWithFCWidget;
  else {
          retval= wprocOffsetAuto;           
          XtDisplayStringConversionWarning( dpy, (char *)fromVal->addr, 
                                            wprocRAlignType);
  ENDelse
 
  if ( (toVal->addr) AND (toVal->size >= sizeof (wprocAlignType)) ) {
        *(wprocAlignType *)toVal->addr= retval;
  ENDif
  else {
        toVal->addr= (caddr_t)&retval;
  ENDelse
  toVal->size= sizeof (wprocAlignType);

  return True;

}

/*
 *  =================== TYPE CONVERTER CvtStrToFileFlgType =================== 
 */
static Boolean CvtStrToFileFlgType( Display      *dpy,
                                    XrmValuePtr  args, 
                                    Cardinal     *num_args,
                                    XrmValuePtr  fromVal, 
                                    XrmValuePtr  toVal,
                                    XtPointer    cvt_data  )

{
 char instr[200], *wkstr, upstr[100];
 static wprocFileFlgType retval= 0;
 int i;
 static char demlimstr[7]= " \t\n";
 

#if (FC_DEBUG)
  puts( "CvtStrToFileFlgType");
#endif


  strncpy(instr, (char *)fromVal->addr, 200);

  if (*num_args != 0)
      XtWarningMsg( "wrongParameters", "CvtStrToAnnoType", "FileChoice error",
                    "No extra arguments needed", (String *) NULL,
                    (Cardinal *) NULL );
  
  wkstr= strtok( instr, demlimstr);

  strncpy(upstr, wkstr, 100);
  for (i=0; (upstr[i] != '\0'); upstr[i]= toupper(upstr[i]), i++ );
  
  for (retval= 0; (wkstr);  wkstr= strtok( NULL, demlimstr) ) {

      strncpy(upstr, wkstr, 100);
      for (i=0; (upstr[i] != '\0'); upstr[i]= toupper(upstr[i]), i++ );
  
      if      (strcmp(upstr, wprocMustExistStr) == 0) 
                    retval|= wprocMustExistMask;

      else if (strcmp(upstr, wprocIsRequiredStr) == 0) 
                    retval|= wprocIsRequiredMask;

      else if (strcmp(upstr, wprocWritableStr) == 0) 
                    retval|= wprocWritableMask;

      else if (strcmp(upstr, wprocMsgBlkStr) == 0) 
                    retval|= wprocMsgBlkMask;

      else if (strcmp(upstr, wprocIgnoreFocStr) == 0) 
                    retval|= wprocIgnoreFocMask;

      else if (strcmp(upstr, wprocNoAlwaysActChkStr) == 0) 
                    retval|= wprocNoAlwaysActChkMask;

      else if (strcmp(upstr, wprocAddExtAlwaysStr) == 0) 
                    retval|= wprocAddExtAlwaysMask;

      else if (strcmp(upstr, wprocOverWriteWarnStr) == 0) 
                    retval|= wprocOverWriteWarnMask;

      else if (strcmp(upstr, wprocExpandFileStr) == 0) 
                    retval|= wprocExpandFileMask;

      else if (strcmp(upstr, wprocSqueezeBlanksStr) == 0) 
                    retval|= wprocSqueezeBlanksMask;

      else {
           XtDisplayStringConversionWarning( dpy, wkstr, wprocRFileFlgType);
      ENDelse
  
  ENDloop
 
  if ( (toVal->addr) AND (toVal->size >= sizeof (wprocAlignType)) ) {
        *(wprocFileFlgType *)toVal->addr= retval;
  ENDif
  else {
        toVal->addr= (caddr_t)&retval;
  ENDelse
  toVal->size= sizeof (wprocFileFlgType);

  return True;

}



/*
 *  ========================================================================== 
 *  =========================== PUBLIC FUNCTIONS =============================
 *  ========================================================================== 
 */

/*
 *  ================= PUBLIC FUNCTION wprocFileChoiceShowErr ==================
 */
void wprocFileChoiceShowErr( Widget w,
                             char   *errstr )

{
   char *umsg= NULL;
   Boolean did_malloc= False;
   FileChoiceWidget fio= (FileChoiceWidget) w;
   struct FILE_INFO *fi= &fio->fileChoice.file_info;


  if (errstr) {
      if (strlen(fi->file_desc) > 0) {
           umsg= (char *)malloc( strlen(fi->file_desc) + strlen(errstr) + 5 );
           if (umsg) {
               sprintf( umsg, "%s%s", fi->file_desc, errstr );
               did_malloc= True;
           }
           else 
               puts( "FileChoice: display_warning: malloc failed");
      }
      else {
           umsg= errstr;
      }
      wprocShowMsg( fio->fileChoice.errbox, umsg );
  }
  if ((did_malloc) && (umsg)) free(umsg);
}


/*
 *  ================= PUBLIC FUNCTION wprocFileChoiceSetFile ===================
 */
void wprocFileChoiceSetFile( Widget  w,
                             char    *filename,
                             Boolean docheck     )

{
   int i, len;
   char *dirstr;
   FileChoiceWidget fio= (FileChoiceWidget) w;
   char dirtok;
   Boolean found;

#if VMS
   dirtok ='}';
#else
   dirtok ='/';
#endif


   if (filename)
         strcpy(fio->fileChoice.filename, filename);
   else
         fio->fileChoice.filename[0]= '\0';

   fio->fileChoice.filenamePtr= fio->fileChoice.filename;


   if ( wprocFileChoiceNoFile ( &fio->fileChoice.file_info, 
                                 fio->fileChoice.filenamePtr)  ) {
        set_nofile( &fio->fileChoice.file_info, fio->fileChoice.textw );
   } /* end if */
   else
        XmTextSetString( fio->fileChoice.textw, fio->fileChoice.filenamePtr);

   if (fio->fileChoice.file_addr)
              strcpy( fio->fileChoice.file_addr, fio->fileChoice.filename ); 

   if (docheck) {
         wprocFileChoiceValidate(w, False);
   }
   else {
         fio->fileChoice.good_file= True;
         strcpy( fio->fileChoice.file_info.sav_filename, filename);
   }

   len= strlen(fio->fileChoice.filename);    
   dirstr= newstr(fio->fileChoice.filename);
   for(i= len-1, found= False; ((i>-1)&&(!found)); i--) {
        if (dirstr[i] == dirtok) { 
               found= True;
               dirstr[i+1]= '\0'; 
        } /* end if */
   } /* end loop */ 
   if (found)  wprocFileChoiceSBSetDir(w,dirstr);
   free(dirstr);

}



/*
 *  ================== PUBLIC FUNCTION wprocFileChoiceGetFile ==================
 */
char *wprocFileChoiceGetFile( Widget  w )
{
   FileChoiceWidget fio= (FileChoiceWidget) w;
   char *filename;

   filename= XtMalloc( strlen (fio->fileChoice.filename)+ 2 );
   strcpy(filename, fio->fileChoice.filename);
   return (filename);
}

/*
 * ================= PUBLIC FUNCTION wprocFileChoiceGoodFile ==================
 */
Boolean wprocFileChoiceGoodFile( Widget  w )
{
   Boolean retval;
   FileChoiceWidget fio= (FileChoiceWidget) w;
 
   retval= (Boolean) fio->fileChoice.good_file;
   return (retval);
}

/*
 * ================= PUBLIC FUNCTION wprocFileChoiceGetFlags ==================
 */
unsigned long wprocFileChoiceGetFlags( Widget  w )
{
   FileChoiceWidget fio= (FileChoiceWidget) w;
   return (fio->fileChoice.file_flags);
}

/*
 * ================= PUBLIC FUNCTION wprocFileChoiceSetFlags ==================
 */
unsigned long wprocFileChoiceSetFlags( Widget        w,
                                       unsigned long flgs )
{
   FileChoiceWidget fio= (FileChoiceWidget) w;
   fio->fileChoice.file_flags= fio->fileChoice.file_info.check_flags= flgs;
   return flgs;
}

/*
 *  ================ PUBLIC FUNCTION wprocFileChoiceGetFileByStr =============
 */
Boolean wprocFileChoiceGetFileByStr( Widget  w,
                                 char   *filename )
{
   FileChoiceWidget fio= (FileChoiceWidget) w;
   Boolean retval;

   if (filename)
         strcpy(filename, fio->fileChoice.filename);
   else
         XtWarning(FILESTRNULL);
       
   retval= (Boolean) fio->fileChoice.good_file;

   return (retval);
}

/*
 *  ================== PUBLIC FUNCTION wprocFileChoiceValidate ================
 */
Boolean wprocFileChoiceValidate( Widget  w,
                                 Boolean evenif )
{
   FileChoiceWidget fio= (FileChoiceWidget) w;
   XmAnyCallbackStruct cbs;
   Boolean retval;

   cbs.reason= XmCR_ACTIVATE;
   cbs.event= NULL;



   if (!evenif)
        fio->fileChoice.file_info.check_flags |=  wprocNoAlwaysActChkMask;

   fio->fileChoice.file_info.check_flags |=  wprocIgnoreFocMask;

   dofilecb( fio->fileChoice.textw, &fio->fileChoice.tcb[0], &cbs);

   retval= (Boolean) fio->fileChoice.good_file;

   fio->fileChoice.file_info.check_flags= fio->fileChoice.file_flags;

   return (retval);
}

/*
 *  ================== PUBLIC FUNCTION wprocFileChoiceRetWidgets =============
 */
void wprocFileChoiceRetWidgets( Widget  w,
                            Widget  *textw,
                            Widget  *annow,
                            Widget  *fsbpopw,
                            Widget  *errbox )
                               
{
   FileChoiceWidget fio= (FileChoiceWidget) w;

   if (textw)   *textw= fio->fileChoice.textw;
   if (annow)   *annow= fio->fileChoice.annow;
   if (fsbpopw) *fsbpopw= fio->fileChoice.fsbpopw;
   if (errbox)  *errbox= fio->fileChoice.errbox;
}

/*
 *  ================== PUBLIC FUNCTION wprocFileChoiceSBSetPath =============
 */
void wprocFileChoiceSBSetPath( Widget     w,
                               char      *str )
{
 FileChoiceWidget fio= (FileChoiceWidget) w;


 if (fio->fileChoice.fsbpopw) {
         XmString xmstr;
         xmstr= XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET );
         XtVaSetValues(fio->fileChoice.fsbpopw, XmNdirMask, xmstr, NULL);
         XmStringFree( xmstr);        /*  free memory */
 } /* End If */


}

/*
 *  ================== PUBLIC FUNCTION wprocFileChoiceSBGetPath =============
 */
char *wprocFileChoiceSBGetPath( Widget     w)
{

 XmString xmstr;
 char *retstr;
 FileChoiceWidget fio= (FileChoiceWidget) w;
 XmStringContext   ctx;
 XmStringCharSet   cset;
 XmStringDirection dir;
 Boolean           stat;
 Boolean           sep;



 if (fio->fileChoice.fsbpopw) {
         XtVaGetValues(fio->fileChoice.fsbpopw, XmNdirMask, &xmstr, NULL);
         if (!XmStringInitContext( &ctx, xmstr)) {
              retstr= NULL;
         ENDif
         else {
              stat= XmStringGetNextSegment( ctx, &retstr, &cset, &dir, &sep );
              XtFree(cset);
         ENDelse
         XmStringFreeContext( ctx);
 } /* End If */

 return (retstr);

}

/*
 *  ================== PUBLIC FUNCTION wprocFileChoiceSBChangeDir ===========
 */
void wprocFileChoiceSBSetDir( Widget     w,
                              char      *str )
{

 FileChoiceClassPart *fclass= &fileChoiceClassRec.fileChoice_class;
 FileChoiceWidget fio= (FileChoiceWidget) w;

 if (fio->fileChoice.fsbpopw) {
         XmString xmstr;
         xmstr= XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET );
         XtVaSetValues(fio->fileChoice.fsbpopw, XmNdirectory, xmstr, NULL);
         XmStringFree( xmstr);        /*  free memory */
 } /* End If */

 if (fclass->use_default_path) {
       if (str != fclass->default_path) {
             if (fclass->default_path) free(fclass->default_path);
             fclass->default_path= newstr(str);
       }
 }


}
/*
 *  ================== PUBLIC FUNCTION wprocFileChoiceSBChangeDir ===========
 */
char *wprocFileChoiceSBGetDir( Widget     w)
{

 XmString xmstr;
 char *retstr= NULL;
 FileChoiceWidget fio= (FileChoiceWidget) w;
 XmStringContext   ctx;
 XmStringCharSet   cset;
 XmStringDirection dir;
 Boolean           stat;
 Boolean           sep;

 if (fio->fileChoice.fsbpopw) {
         XtVaGetValues(fio->fileChoice.fsbpopw, XmNdirectory, &xmstr, NULL);
         if (!XmStringInitContext( &ctx, xmstr)) {
              retstr= NULL;
         ENDif
         else {
              stat= XmStringGetNextSegment( ctx, &retstr, &cset, &dir, &sep );
              XtFree(cset);
         ENDelse
         XmStringFreeContext( ctx);
         if (xmstr) XtFree(xmstr);
 } /* End If */

 return (retstr);

}

/*
 *  ========== PUBLIC FUNCTION wprocFileChoiceSetDirectoryUpdate ===========
 */
void wprocFileChoiceSetDirectoryUpdate(Boolean do_update)
{
  fileChoiceClassRec.fileChoice_class.use_default_path= do_update;
}

/*
 *  ========== PUBLIC FUNCTION wprocFileChoiceGetDirectoryUpdate ===========
 */
Boolean wprocFileChoiceGetDirectoryUpdate()
{
  return fileChoiceClassRec.fileChoice_class.use_default_path;
}


/*
 *  ========== PUBLIC FUNCTION wprocFileChoiceSetAltFileAccess ===========
 */
void wprocFileChoiceSetAltFileAccess(Widget w, AltFileAccess *net_file)
{
 FileChoiceWidget fio= (FileChoiceWidget) w;
 fio->fileChoice.file_info.net_file= net_file;
}


