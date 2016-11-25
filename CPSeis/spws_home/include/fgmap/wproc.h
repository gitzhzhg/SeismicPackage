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
#ifndef _WPROCDEF_H_

#define _WPROCDEF_H_

/*
 * Name        : wproc.h
 * Library     : wproc
 * Author      : Trey Roby
 * Date        : 11/1/91
 *
 * Header file for the wproc (Widget procedures) library.  The wproc library
 * includes the following utilities:
 *
 *           - create primitive widgets in consistent manner with callbacks
 *           - create common combinations of widgets
 *           - context sensitive help routines
 *           - color allocation and visual information routines
 *           - callbacks for text widgets doing floats or integers
 *           - callbacks for text widgets doing input or output files
 *           - short convenience routines to manipulate widgets
 *
 * CHANGES:
 */


#include "c2f_interface.h"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#ifdef sun
#ifndef SOLARIS
#include "spws_sunos.h"
#endif
#endif
#ifdef VMS
#include "spws_vms.h"
#endif
#ifdef _AIX
#include "spws_aix.h"
#endif
#ifdef __hpux
#include "spws_hpux.h"
#endif
#ifdef __sgi
#include "spws_sgi.h"
#endif


#define TYPE_NONE   0
#define TYPE_INT    1
#define TYPE_FLT    2
#define TYPE_STR    3
#define TYPE_RADIO  4
#define TYPE_TOGGLE 5
#define TYPE_SINT   6
#define TYPE_FPOPUP 7
#define TYPE_FILE   8
#define TYPE_GFLT   9
#define TYPE_UCHAR  10
#define TYPE_LONG   11
#define TYPE_DBL    12
#define TYPE_GDBL   13

 /*
  * -----------------------------------------------------------------------
  * the following constants specify widget types of conbinations
  */

#define ARROW_BUTTON    1
#define TEXT            2
#define TOGGLE_BUTTON   3
#define CAS_PULL        4
#define PUSH_BUTTON     5
#define SCALEW          6
#define FORM_POPUP      7
#define SEPARATOR       8
#define SCROLL_BAR      9
#define OPTION_MENU    10


 /*
  * -----------------------------------------------------------------------
  * these constants specify which button was pushed in a form popup
  * in the activate callback.  It is set in user data.
  */
#define FP_OK         0
#define FP_CANCEL     1
#define FP_APPLY      2
#define FP_HELP       3

 /*
  * -----------------------------------------------------------------------
  * the following flags are sent to the form popup creation routines to
  * specify which buttons to create.  The can be bit or'ed together.
  * -----------------------------------------------------------------------
  */
#define FP_DOOK        (1<<0)   
#define FP_DOCANCEL    (1<<1)
#define FP_DOAPPLY     (1<<2)
#define FP_DOHELP      (1<<3)
#define FP_DOALL       0xFF 

 /*
  * -----------------------------------------------------------------------
  * the following flags are sent to the file callback to determine what
  * actions to take.
  * -----------------------------------------------------------------------
  */
#define FI_MUST_EXIST        (1<<0)   /* requires file to exist */
#define FI_IS_REQUIRED       (1<<1)   /* a file name is required */
#define FI_WRITABLE          (1<<2)   /* file must be writeable */
#define FI_NOMSG_BLK         (1<<3)
#define FI_IGNORE_FOC        (1<<4)    /* do callback even without focus*/
#define FI_NO_ALWAYS_ACT_CHK (1<<5)    /* don't call on every activate (only 
                                        * call if file name has changed) */
#define FI_ADD_EXT_ALWAYS    (1<<6)    /* if set - add extension always, 
                                        * otherwise add extension only when
                                        * file without extension does 
                                        * not exist */
#define FI_OVERWRT_WARN      (1<<7)    /* Warn the user that file exist and 
                                        * will be overwritten */
#define FI_EXPAND_FILE       (1<<8)    /* expand the file to it's full path */
#define FI_SQUEEZE_BLANKS    (1<<9)    /* squeeze blanks out of file */
 

 /*
  * -----------------------------------------------------------------------
  * the following flags are used internally in the help routines
  * -----------------------------------------------------------------------
  */
#define HELP_EVENT    0
#define HELP_OV_EVENT 1
#define HELP_DISP     2
#define HELP_REMOVE   3
#define HELP_M_EVENT  4

#define HELP_DSPCTX       0
#define HELP_DSP_MANOVER  1
#define HELP_DSP_AUTOOVER 2
#define HELP_MHELP        3
#define HELP_ANY          4
#define HELP_DSP_MOREMAN  5
#define HELP_DSP_MOREAUTO 6


       /*  these are used externally */

#define CTXH_ALLWAYS      1    
#define CTXH_IFDISP       0

 /*
  * -----------------------------------------------------------------------
  * default help titles
  * -----------------------------------------------------------------------
  */
#define CST_TITLE_STR "                     CONTEXT SENSITIVE HELP\n\n"
#define OV_TITLE_STR "                     OVERVIEW HELP\n\n"

#define NAMELEN 40
#define DESCLEN 400
#define FILENAMELEN 200



 /*
  * -----------------------------------------------------------------------
  * the following 6 constants define what the visual can do with colors
  * in the test_vis routine
  * -----------------------------------------------------------------------
  */
           /*
            * MUST_BW is mutually exclusive with everything except MUST_SHR.
            * MUST_SHR may be set with anything.
            *
            */
#define MUST_BW         (1<<0)   /* may only do B & W drawing */
#define MUST_SHR        (1<<1)   /* set when colors are readonly */

           /*
            *             LARGE SCALE ALLOCATION 
            * These next two can be set at the same time.  
            * They will be set only if depth is greater than 4.  
            * They indicate you may do large scale allocation of colors 
            * or gray scale.  
            */
#define MAY_GS          (1<<2)   /* may do large gray scale color allocation */
#define MAY_COL         (1<<3)   /* may do large color - color allocation */


           /*
            *             AT LEAST SMALL SCALE ALLOCATION 
            * These next two are mutually exclusive and are set when you
            * can allocate AT LEAST a small amount of cells typically for
            * drawing lines.  They may be set alone or when the large 
            * scale allocation flags are set.
            *
            * They are are mutually exclusive.  The MAY_CGS_LINE means
            * doing color and gray scale; the MAYONLY_GSLINE means only 
            * gray scale.
            * 
            */
#define MAY_CGS_LINE    (1<<4)   /* may do color or gray scale line*/
#define MAYONLY_GSLINE  (1<<5)   /* may do gray scale line but not color */

 /*
  * -----------------------------------------------------------------------
  */



#define WPROC_NONE -1
#define wprocALL   -9999 


enum chk_actions { NOCHK=0, SETCHK, RANGECHK };



struct CB {
               long    wconst;
               void    *info;
               Widget  popbox;
               void    *fldptr;
               void    *(*more_func)(); 
               long    type;
          };


struct COLOR_INFO {
                    Boolean       colorsafe;
                    long          numplanes;
                    Colormap      cmap;
                    unsigned int  cnum;
                    unsigned long pmsk[24]; 
                    unsigned long pix[500];
                    Boolean       shared;   /*if true - then allocate shared*/
                  };

typedef struct COLOR_INFO ColorInfo;
typedef struct COLOR_INFO *ColorInfoPtr;


#define SCELL_BW       1
#define SCELL_COLOR    2
#define SCELL_GCOLOR   3


struct COLOR_SCELL {
                     char           cname[70];
                     char           gsname[70];
                     Boolean        defblack;
                     unsigned long  pixel ;          /* returned */
                   };

typedef struct COLOR_SCELL  ColorScell;
typedef struct COLOR_SCELL *ColorScellPtr;





struct TextW {          /* text widget structure */
               Widget         text;                          /* returned */ 
               long           wconst;
               long           wclass;
               char           text_name[NAMELEN];
               Widget         parent;
               XtCallbackProc entry_cb;
               void           *entry_info;
               XtCallbackProc exit_cb;
               void           *exit_info;
               char           *initval;                     /* returned */ 
               Widget         label;                        /* returned */ 
               char           label_name[NAMELEN];
               long           type;
               void           *target;
             };
               

struct ScaleW {                     /* Scale widget structure */
               Widget         scale;                      /* returned */ 
               long           wconst;
               long           wclass;
               char           scale_name[NAMELEN];
               Widget         parent;
               XtCallbackProc valchange_cb;
               void           *valchange_info;
               int           initval;                    /* returned */ 
               long           type;
               int            *target;
             };

struct ToggleW {                     /* toggle widget structure */
               Widget         toggle;                      /* returned */ 
               long           wconst;
               long           wclass;
               char           toggle_name[NAMELEN];
               Widget         parent;
               XtCallbackProc valchange_cb;
               void           *valchange_info;
               Boolean        initval;                    /* returned */ 
               long           type;
               void           *target;
             };
               

struct PushW {                         /* push widget structure */
               Widget         push;                     /* returned */
               long           wconst;
               long           wclass; 
               char          push_name[NAMELEN];
               Widget         parent;
               XtCallbackProc active_cb;
               void           *active_info;
             };

struct AnyW {                         /* any widget structure */
               Widget w;                     /* returned */
               long   wconst;
               long   wclass;   /* not widget class but combination type */
               char  name[NAMELEN];
               Widget parent;
             };


struct Cas_PullW {                    /* cascade pulldown widget structure */
               Widget   pdown;                       /* returned */ 
               long     wconst;
               long     wclass; 
               char     pdown_name[NAMELEN];
               Widget   parent;
               Widget   cascade;                     /* returned */ 
               char     cas_name[NAMELEN];
             };

struct op_menuW {                         /* option menu widget structure */
               Widget   rc;                       /* returned */ 
               long     wconst;
               long     wclass; 
               Widget   menuw;                     /* returned */ 
             };

struct Form_popup {
               Widget         form;          /* returned */ 
               long           wconst;
               long           wclass; 
               char           form_name[NAMELEN];
               Widget         parent;
               Widget         okb;           /* returned */ 
               Widget         appb;          /* returned */ 
               Widget         canb;          /* returned */ 
               Widget         helpb;         /* returned */ 
               Widget         button_ctl;    /* returned */ 
               long           whichb;
               XtCallbackProc active_cb;
               void           *active_info;
               long           numb;          /* returned */
               Boolean        not_a_pop;     /* don't create dialog shell*/ 
             };

struct sbW {                         /* Scroll Bar widget structure */
               Widget         scrollb;                     /* returned */
               long           wconst;
               long           wclass; 
               char           sb_name[NAMELEN];
               Widget         parent;
               XtCallbackProc cbs;
               void           *cbs_info;
             };




typedef union _wunion {
                        Widget w;
                        struct AnyW        any;
                        struct PushW       pushw;
                        struct TextW       textw;
                        struct ToggleW     togw;
                        struct Cas_PullW   cpw;
                        struct ScaleW      sclw;
                        struct sbW         sbw;
                        struct Form_popup  fp;
                        struct op_menuW    opm;
                      } wunion;


#define  TOKENLEN 80

typedef struct _MhelpEle {
               Widget  twin;
               char    *tokenstr;
               Widget  outtext;
            } MhelpEle;


struct HELPCTX {
               XrmDatabase real_dbase; 
               XrmDatabase *dbase;
               char        dbfile[80];
               Boolean     delay_init;
               wunion      autohelp[1];
               struct CB   help_cbdata[5];
               Widget      helptext;
               Widget      helptext_ov;
               char        *over_t;
               char        *ctx_t;
               MhelpEle    *mhelp_ary;
               char        *mhelp_blankstr;
               int         num_mhelp;
               Widget      curr_mouse_widget;
          };

typedef struct HELPCTX CTXH_HELPCTX;
typedef struct HELPCTX *HelpCtx;


struct CURRFLD_INFO {
                       Widget curr_textw;
                       long curr_text;
                    };

#define NOFILESTR "NONE"


typedef struct NetEnv *NetEnvPtr;

typedef int accessFunc( NetEnvPtr obj  ,char *fname, int mask);
typedef void expTildefunc( NetEnvPtr obj, char *expstr, char *instr);
typedef time_t modTimeFunc(NetEnvPtr netenv, char *fname);
typedef char  **getDirFunc(NetEnvPtr       netenv, 
                           char           *dirname, 
                           int            *length,
                           unsigned char  *stat);
typedef void expFileFunc(NetEnvPtr netenv, char *fullfile, char *infile);



typedef struct _AltFileAccess {
            NetEnvPtr      *objptr;
            accessFunc     *access_func;
            expTildefunc   *exp_tilde_func;
            modTimeFunc    *mod_type_func;
            getDirFunc     *get_dir_func;
            getDirFunc     *get_dir_dir_func;
            expFileFunc    *exp_file_func;
} AltFileAccess;



struct FILE_INFO {
                      char     *filename;
                      char     sav_filename[200];
                      long     detail;
                      char     ext[NAMELEN];
                      long     check_flags;
                      Boolean  do_yn;
                      Boolean  init;
                      char     *none_str;
                      char     *fail_message;
                      time_t   file_mod_time; 
                      Boolean  use_none;
                      char     file_desc[DESCLEN];  /* to use in err messages */
                      long     *good_file;      /* true if conditions met */
                      Widget   *twigs;          /* text widgets to update */
                      long     (*succfunc)();   /* routine called on success*/
                      long     (*failfunc)();   /* routine called on fail*/
                      void     *succdata;       /* data passed on success*/
                      AltFileAccess  *net_file; /* using network layer 
                                            * to access files not defined 
                                            * as a resource but is
                                            * settable with access function
                                            * wprocFileChoiceSetAltFileAccess
                                            */
                 };



/*
 * -----------------------------------------------------------------------------
 * ------------- MANY, MANY MACROS ---------------------------------------------
 * -----------------------------------------------------------------------------
 *
 *4/1/92 extensive documantation needs to be done here...hope I don't die soon.
 *12/10/92 help macro are now documented
 *12/10/92 utility macros are now documented
 */

#define set_FI( sct, fn, ex, chk, mstr, bool)                         \
                      (  (sct).filename= fn,                          \
                         strcpy ( (sct).ext, (ex) ),                  \
                         (sct).check_flags = (chk),                   \
                         strcpy( (sct).file_desc, (mstr) ),           \
                         (sct).good_file= &(bool),                    \
                         (sct).none_str= NOFILESTR,                   \
                         (sct).use_none= True,                        \
                         (sct).init= False,                           \
                         (sct).file_mod_time= 0,                      \
                         strcpy( (sct).sav_filename, "$$$$$$$" ),     \
                         (sct).twigs= NULL )




#define set_Form_popup(stc,fname,acb,ast,wb,p,cst)                      \
                      (  (stc)[(cst)].fp.form=NULL,                     \
                         (stc)[(cst)].fp.okb=NULL,                      \
                         (stc)[(cst)].fp.appb=NULL,                     \
                         (stc)[(cst)].fp.canb=NULL,                     \
                         (stc)[(cst)].fp.helpb=NULL,                    \
                         strcpy( (stc)[(cst)].fp.form_name, (fname) ),  \
                         (stc)[(cst)].fp.active_cb= (XtCallbackProc)(acb), \
                         (stc)[(cst)].fp.active_info=(void *)(ast),     \
                         (stc)[(cst)].fp.whichb=(wb),                  \
                         (stc)[(cst)].fp.parent=(p),                   \
                         (stc)[(cst)].fp.wconst=(cst),                 \
                         (stc)[(cst)].fp.not_a_pop= False,             \
                         (stc)[(cst)].any.wclass=FORM_POPUP     )



#define set_Cas_Pull(stc,cname,pname,p,cst)                            \
                 (  (stc)[(cst)].cpw.cascade=NULL,                     \
                    (stc)[(cst)].cpw.pdown=NULL,                       \
                    strcpy( (stc)[(cst)].cpw.cas_name, (cname) ),      \
                    strcpy( (stc)[(cst)].cpw.pdown_name, (pname) ),    \
                    (stc)[(cst)].cpw.parent=(p),                       \
                    (stc)[(cst)].cpw.wconst=(cst),                     \
                    (stc)[(cst)].any.wclass=CAS_PULL     )


#define set_ARROW(stc,pn,acb,ast,p,cst)                                  \
                 (  (stc)[(cst)].pushw.push=NULL,                        \
                    strcpy( (stc)[(cst)].pushw.push_name, (pn) ),        \
                    (stc)[(cst)].pushw.active_cb= (XtCallbackProc)(acb), \
                    (stc)[(cst)].pushw.active_info=(void *)(ast),        \
                    (stc)[(cst)].pushw.parent=(p),                       \
                    (stc)[(cst)].pushw.wconst=(cst),                     \
                    (stc)[(cst)].any.wclass=ARROW_BUTTON     )


#define set_PUSH(stc,pn,acb,ast,p,cst)                                   \
                (  (stc)[(cst)].pushw.push=NULL,                         \
                    strcpy( (stc)[(cst)].pushw.push_name, (pn) ),        \
                    (stc)[(cst)].pushw.active_cb= (XtCallbackProc)(acb), \
                    (stc)[(cst)].pushw.active_info=(void *)(ast),        \
                    (stc)[(cst)].pushw.parent=(p),                       \
                    (stc)[(cst)].pushw.wconst=(cst),                     \
                    (stc)[(cst)].any.wclass=PUSH_BUTTON     )

#define set_SEP(stc,pn,p,cst)                                  \
                (  (stc)[(cst)].any.w=NULL,                    \
                    strcpy( (stc)[(cst)].any.name, (pn) ),     \
                    (stc)[(cst)].any.parent=(p),               \
                    (stc)[(cst)].any.wconst=(cst),             \
                    (stc)[(cst)].any.wclass=SEPARATOR     )

#define set_SB(stc,sbn,cb,cbst,p,cst)                                \
                (  (stc)[(cst)].sbw.scrollb=NULL,                    \
                    strcpy( (stc)[(cst)].sbw.sb_name, (sbn) ),       \
                    (stc)[(cst)].sbw.cbs= (XtCallbackProc)(cb),      \
                    (stc)[(cst)].sbw.cbs_info=(void *)(cbst),        \
                    (stc)[(cst)].sbw.parent=(p),                     \
                    (stc)[(cst)].sbw.wconst=(cst),                   \
                    (stc)[(cst)].any.wclass=SCROLL_BAR     )


#define set_CB(stc,cst,inf,w,val,t)                              \
                             ( (stc)[(cst)].wconst=(cst),        \
                               (stc)[(cst)].info=(void *)(inf),  \
                               (stc)[(cst)].popbox=(w),          \
                               (stc)[(cst)].fldptr= (val),       \
                               (stc)[(cst)].more_func= NULL,     \
                               (stc)[(cst)].type= (t)   )

#define qset_CB(stc,cst,inf,w)                                   \
                             ( (stc)[(cst)].wconst=(cst),        \
                               (stc)[(cst)].info=(void *)(inf),  \
                               (stc)[(cst)].popbox=(w),          \
                               (stc)[(cst)].fldptr= NULL,        \
                               (stc)[(cst)].more_func= NULL,     \
                               (stc)[(cst)].type= 0   )


/*
 * ------------- HELP MACROS --------(which are now documented) ----------
 */

/*
 * add_HELP
 * =================================================================
 * Add context sensitive help to a widget.  It Takes three parameters.
 * w    - Widget
 * hcb  - help callback (almost always helper)
 * hst  - help context
 */
#define add_HELP(w,hcb,hst)                                              \
( XtAddCallback((w), XmNhelpCallback,(XtCallbackProc)(hcb),              \
                &((hst)->help_cbdata[HELP_DISP])),                       \
  XtAddEventHandler((w), FocusChangeMask|EnterWindowMask,False,          \
                 (XtEventHandler)(hcb),                                \
                &((hst)->help_cbdata[HELP_EVENT])) )

/*
 * add_over_HELP
 * =================================================================
 * Add overview help to a widget.  It Takes three parameters.
 * w    - Manager Widget
 * hcb  - help callback (almost always helper)
 * hst  - help context
 */
#define add_over_HELP(w,hcb,hst)                         \
( XtAddEventHandler((w), EnterWindowMask,False,(XtEventHandler)(hcb),    \
                &((hst)->help_cbdata[HELP_OV_EVENT])) )

#define overview_help(str,hctx) display_help( (str),                        \
                     &( (struct HELPCTX *)(hctx) )->help_cbdata[HELP_DISP], \
                                              HELP_DSP_MANOVER, NULL, NULL )

/*
 * this macro is superseded by ctxh_context_help
 */
#define context_help(str,hctx) display_help( (str),                         \
                     &( (struct HELPCTX *)(hctx) )->help_cbdata[HELP_DISP], \
                                              HELP_DSPCTX, NULL, NULL )


/*
 * ctxh_context_help
 * =================================================================
 * Display context sensitive help for a string
 * str    - string to do lookup on
 * hctx   - help context
 * always - Boolean: if True popup the help box and display help
 *                   if False display the help only if the help box is 
 *                   already displayed.
 */
#define ctxh_context_help(str,hctx,always)                                 \
  if ( (always) ||                                                         \
  (XtIsManaged( ((struct HELPCTX *)(hctx))->help_cbdata[HELP_EVENT].popbox)) ) \
       {  display_help( (str),                                           \
          &( (struct HELPCTX *)(hctx) )->help_cbdata[HELP_DISP],         \
            HELP_DSPCTX, NULL, NULL ); }

/*
 * add_cshelpcb
 * =================================================================
 * Add a callback to a push button to display the help box.  When it is
 * pushed.
 * w    - Widget
 * hst  - help context
 */
#define add_cshelpcb(w,hctx)  XtAddCallback((w), XmNactivateCallback, \
                                        (XtCallbackProc)helper, \
                                        &(hctx)->help_cbdata[HELP_DISP] );

#define setup_helpdata(fn)  (void *)XrmGetFileDatabase( (fn) )


/*
 * ctxh_set_overstr
 * =================================================================
 * set the title string for overview help
 * str  - title string
 * hctx - help context
 */
#define ctxh_set_overstr(str,hctx)    ctxh_set( (str), &hctx->over_t);

/*
 * ctxh_set_csstr
 * =================================================================
 * set the title string for context sensitive help
 * str  - title string
 * hctx - help context
 */
#define ctxh_set_csstr(str,hctx)      ctxh_set( (str), &hctx->ctx_t);


/*
 * ctxhSetMhelpOtherStr
 * =================================================================
 * set the output string for mouse context sensitive help
 * str  - ourput string
 * hctx - help context
 */
#define ctxhSetMhelpOtherStr(str,hctx) ctxh_set( (str), &hctx->mhelp_blankstr);

/*
 * ---------- END HELP MACROS ----------------------------------------------
 */


#define set_TEXT(stc,tn,ecb,est,excb,exst,ln,p,typ,tar,cst)       \
                 (  (stc)[(cst)].textw.text=NULL,                 \
                    strcpy( (stc)[(cst)].textw.text_name, (tn) ), \
                    (stc)[(cst)].textw.entry_cb= (XtCallbackProc)(ecb),   \
                    (stc)[(cst)].textw.entry_info=(void *)(est),  \
                    (stc)[(cst)].textw.exit_cb=(XtCallbackProc)(excb),    \
                    (stc)[(cst)].textw.exit_info=(void *)(exst),  \
                    (stc)[(cst)].textw.initval=NULL,              \
                    (stc)[(cst)].textw.label=NULL,                \
                    strcpy( (stc)[(cst)].textw.label_name,(ln) ), \
                    (stc)[(cst)].textw.parent=(p),                \
                    (stc)[(cst)].textw.type=(typ),                \
                    (stc)[(cst)].textw.target=(tar),              \
                    (stc)[(cst)].textw.wconst=(cst),              \
                    (stc)[(cst)].any.wclass=TEXT     )



#define set_SCALE(stc,sn,vccb,vcst,init,p,typ,tar,cst)               \
                 (  (stc)[(cst)].sclw.scale=NULL,                    \
                    strcpy( (stc)[(cst)].sclw.scale_name, (sn) ),    \
                    (stc)[(cst)].sclw.valchange_cb= (XtCallbackProc)(vccb),\
                    (stc)[(cst)].sclw.valchange_info=(void *)(vcst), \
                    (stc)[(cst)].sclw.initval= (init),               \
                    (stc)[(cst)].sclw.parent=(p),                    \
                    (stc)[(cst)].sclw.type=(typ),                    \
                    (stc)[(cst)].sclw.target=(tar),                  \
                    (stc)[(cst)].sclw.wconst=(cst),                  \
                    (stc)[(cst)].any.wclass=SCALEW     )


#define set_TOGGLE(stc,tn,vccb,vcst,init,p,typ,tar,cst)              \
                 (  (stc)[(cst)].togw.toggle=NULL,                   \
                    strcpy( (stc)[(cst)].togw.toggle_name, (tn) ),   \
                    (stc)[(cst)].togw.valchange_cb= (XtCallbackProc)(vccb),\
                    (stc)[(cst)].togw.valchange_info=(void *)(vcst), \
                    (stc)[(cst)].togw.initval= (init),               \
                    (stc)[(cst)].togw.parent=(p),                    \
                    (stc)[(cst)].togw.type=(typ),                    \
                    (stc)[(cst)].togw.target=(tar),                  \
                    (stc)[(cst)].togw.wconst=(cst),                  \
                    (stc)[(cst)].any.wclass=TOGGLE_BUTTON     )


/*
 * TEXT_set_int
 * =================================================================
 * This macro sets an int into a text widget.  It takes Three parameters
 *  w   - widget
 *  str - a temporary work string (about 20 characters)
 *  in  - the integer
 */
#define TEXT_set_uchar(w,str,uc)                                     \
         ( sprintf((str),"%hd",(uc)),XmTextSetString((w),(str)) )

#define TEXT_set_sint(w,str,sin)                                     \
         ( sprintf((str),"%hd",(sin)),XmTextSetString((w),(str)) )

#define TEXT_set_int(w,str,in)                                     \
         ( sprintf((str),"%d",(in)),XmTextSetString((w),(str)) )

#define TEXT_set_long(w,str,lng)                                     \
         ( sprintf((str),"%ld",(lng)),XmTextSetString((w),(str)) )

/*
 * TEXT_set_flt
 * =================================================================
 * This macro sets an float into a text widget.  It takes Three parameters
 *  w   - widget
 *  str - a temporary work string (about 20 characters)
 *  in  - the float
 */
#define TEXT_set_flt(w,str,flt)                                     \
         ( sprintf((str),"%4.2f",(flt)),XmTextSetString((w),(str)) )

#define TEXT_set_gflt(w,str,gflt)                                     \
         ( sprintf((str),"%4.3g",(gflt)),XmTextSetString((w),(str)) )

#define TEXT_set_dbl(w,str,dbl)                                     \
         ( sprintf((str),"%8.4f",(dbl)),XmTextSetString((w),(str)) )

#define TEXT_set_gdbl(w,str,gdbl)                                     \
         ( sprintf((str),"%8.6g",(gdbl)),XmTextSetString((w),(str)) )

#ifdef PixelsPerInch
#undef PixelsPerInch
#endif
#define PixelsPerInch(dsp, scrn) (int) (((float) DisplayWidth(dsp, scrn)) / \
                                 (0.0394 * DisplayWidthMM(dsp, scrn)))

/*
 * wprocCursorSet
 * =================================================================
 * set the cursor to some Font Cursor (created with XCreateFontCursor).
 * w   - widget
 * cur - Cursor
 */
#define wprocCursorSet(w,cur) XDefineCursor( XtDisplay((w)),        \
                                             XtWindow((w)), (cur) )

/*
 * wprocCursorNone
 * =================================================================
 * set the cursor to back to original cursor
 * w   - widget
 */
#define wprocCursorNone(w) XDefineCursor( XtDisplay((w)), XtWindow((w)), None)

/*
 * -----------------------------------------------------------------------------
 * ------------- END OF MACROS ---------------------------------------------
 * -----------------------------------------------------------------------------
 *
 */
#ifdef __cplusplus  
extern "C" {                          /* for C++ */
#endif


extern Widget make_ebox();
Widget make_okbox( Widget, char [],
#ifdef CRAY
                   long  type );
#else
                   unsigned char  type );
#endif
extern Widget make_qbox();

extern Widget create_push( );
extern Widget create_toggle( );
Widget create_text( struct TextW   *ts);
extern Widget create_sb( );
extern Widget cre_opm( );

/*extern struct HELPCTX *setup_help( );*/
extern struct HELPCTX *setup_help( Widget         p,
                            XrmDatabase   *data,
                            char          filename[],
                            char          title_str[]);
extern void helper( );
void ctxhMergeHelpLine( HelpCtx hctx,
                    char    *help_text);
char *ctxhGetTolken( HelpCtx hctx,
                     Widget  win);



Boolean check_flt( char str[], float  *val );
Boolean check_sint( char str[], short  *val );
Boolean check_int( char str[], int  *val );
Boolean check_uchar( char str[], unsigned char *val );
Boolean check_long( char str[], long  *val );
Boolean check_dbl( char str[], double  *val );

void set_label( Widget w, char   str[] );
char *get_simp_labelstrptr( Widget w );
char *get_simp_labelstr( Widget w, char str[] );
char *get_string_from_xmstr( XmString xmstr );



char *wprocPushMsg( Widget w, char   *str);
void  wprocPopMsg( Widget w, char   *str);

/*
 * show_msg has been replaced by wprocShowMsg.
 * routines in show_msg.c
 */
void show_msg( Widget w, char str[] );
void wprocShowMsg( Widget w, char str[] );
void wprocVAShowMsg( Widget w, char *format, ...);
char *wprocGetMsg( Widget w );


Widget mk_togb_cb( wunion[], struct  CB[], Widget, XtCallbackProc,
                   struct CB*, long, long, ... );
Widget mk_pshb_cb( wunion[], struct  CB[], Widget, XtCallbackProc,
                   struct CB*, long, ... );


int strcmpnull( char *s1, char *s2 );

char *strip_file( char *lfile, char *retfile );


void wprocPrimHelp( wunion wu[],
                    long   cnt,
                    HelpCtx helpctx);

long test_vis( Display *dpy, long *visclass, long *tot_col );

Colormap newcmap_andcpy( Widget w, long   copy_colors );
Colormap newcmap_andcpy_onscreen( Display *dpy,
                                  Screen  *scr,
                                  long     copy_colors );

long alloc_gray_colors( Display *dpy, struct COLOR_INFO  *col);

unsigned long alloc_scell( Display            *dpy,
                           Colormap           cmap,
                           struct COLOR_SCELL *sc,
                           Boolean            printerr,
                           XColor             *cdef,    
                           long               *ret_stat);

void ctxh_set( char  *str, char  **tit_str);

Boolean display_help( char *wname,
                      struct CB *showhelp,
                      long      helptype,
                      Widget    ev_widget,
                      Widget    passed_outwig );


int get_mhelp_index( Widget w, HelpCtx hctx);


void ctxhMouseHelp( HelpCtx hctx,
                    Widget  text,
                    Widget  win,
                    char    *token );

void ctxhChangeTolken( HelpCtx hctx,
                       Widget  win,
                       char    *token );

void create_prim_widgets( wunion wu[],
                          long   cnt,
                          HelpCtx helpctx);

Widget cre_frm_pop( wunion   *wu,
                    Boolean  reduce_on_small,
                    HelpCtx  helpctx );

void wproc_setsen( wunion   *wary, 
                   Boolean  sensitive,
                   long     cnt, ...);

char *getdir();
Widget wprocGetShell(Widget);
Widget wprocGetShellChild(Widget);

void setDefRes( Display *dpy,
                char    *name,
                String  *resourceSpec );

void setDefResWithClass(
                Display *dpy,
                char    *class_name,
                char    *name,
                String  *resourceSpec );

char *wproc_get_resource(Widget  w, 
                         char   *inststr, 
                         char   *clstr,
                         char   *default_res);

char *make_wctree( Widget w,
                   char   wstr[],
                   char   cstr[],
                   int    level );

char *make_wtree( Widget w,
                  char   wstr[],
                  int    level );


int store_events( XExposeEvent *ev );


void wprocTravWTree(Widget w,
                    void   (*func)(Widget, void*),
                    void   *data );

/*-------------------------- widget_p_util.c ------------------------------*/
char *class_name( Widget w);
char *classptr_name( WidgetClass wc);
void appContextToDpyList(XtAppContext, Display ***, int *);
void get_popup_list(Widget w, WidgetList *plist, Cardinal *nump);

/*-------------------------- defsave.c ------------------------------*/

typedef struct DefaultInfo *DefInfo;

DefInfo DefFileInit( char *fname, int level, Boolean in_stand_dir);
DefInfo DefStandardInit( Widget w, int level);
void DefSaveWidget( DefInfo def, Widget w );
void DefSave( DefInfo def, Widget w);
void DefLevel( DefInfo def, int level);
void DefEnd( DefInfo def);
void DefLoadFileRes( Widget w, char *fname, Boolean in_stand_dir);
void DefLoadStandardRes(Widget w);
char *DefGetStrValue( Display *dpy, char *istr, char *cstr);
Boolean boolstr_value( char *str);
char *DefgetWValue( Widget w);
char *DefLoadWValue( Widget w);
char *DefLoadGetWValue( Widget w, Boolean doload, Boolean inform);
char *Deffile_path(char *infile, char *outfile);
void DefLoadValue( Widget w);






/*------------ from here on, we have stuff added by Tom Stoeckley -------*/
/*------------ from here on, we have stuff added by Tom Stoeckley -------*/
/*------------ from here on, we have stuff added by Tom Stoeckley -------*/
/*------------ from here on, we have stuff added by Tom Stoeckley -------*/

/*
    These utilities have the following requirements:

     -- They all reside in library wproc.a .
     -- They all reside in source files in ~spws/util/wproc .
     -- They are all written in C.
     -- Each utility may contain one or more functions.
     -- Each utility must reside on a single source file, with
            documentation at the beginning of the file.
     -- They do not reference any routines on any libraries
            other than their own library (wproc.a), cprim.a, the
            X/Xt/Motif libraries, and the standard C libraries.
     -- None of them require any header files other than this
            one (wproc.h), cprim.h, the X/Xt/Motif header
            files, and the standard C header files.
     -- All of them make some reference to Xlib, Xt, or Motif.
            (Otherwise they should be in library cprim.a.)
*/


/*-------------------------- widget_util.c ------------------------------*/

void   add_change_case_actions  (Widget w);
void   add_spacing_event_handler(Widget w);

Widget raise_widget      (Widget w);
void   attach_widget     (Widget w,
          Widget wleft, Widget wright, Widget wtop, Widget wbottom,
          int    oleft, int    oright, int    otop, int    obottom);

void   sensitize_arrow   (Widget w, Boolean flag);
void   sensitize_scale   (Widget w, Boolean flag);
void   sensitize_text    (Widget w, long      sn);

void   set_widget_sense    (Widget w, long      sn);
void   set_widget_cvar     (Widget w, char   *cvar);
void   set_widget_ivar     (Widget w, long    ivar);
void   set_widget_fvar     (Widget w, float   fvar, long ndec);
void   set_widget_dvar     (Widget w, double  dvar, long ndec);
void   set_widget_radio    (Widget w, long    ivar, long id);
void   set_widget_opt      (Widget w, long    ivar, long id);
void   set_widget_minmax   (Widget w, long  minvar, long  maxvar);
void   set_widget_minmaxvar(Widget w, long  minvar, long  maxvar, long  ivar);
void   set_widget_minvar   (Widget w, long  minvar);
void   set_widget_maxvar   (Widget w, long  maxvar);

void   get_widget_sense    (Widget w, long     *sn);
void   get_widget_cvar     (Widget w, char   *cvar);
void   get_widget_ivar     (Widget w, long   *ivar);
void   get_widget_fvar     (Widget w, float  *fvar);
void   get_widget_dvar     (Widget w, double *dvar);
void   get_widget_radio    (Widget w, long   *ivar, long id);
void   get_widget_minmax   (Widget w, long *minvar, long *maxvar);
void   get_widget_minmaxvar(Widget w, long *minvar, long *maxvar, long *ivar);
void   get_widget_minvar   (Widget w, long *minvar);
void   get_widget_maxvar   (Widget w, long *maxvar);

void   set_scale_values  (Widget w, long ivar, long first, long last);
void   get_scale_values  (Widget w, long*ivar, long*first, long*last);

void   set_scroll_values (Widget w, long ivar,long first,long last,long step);
void   get_scroll_values (Widget w, long*ivar,long first,long*last,long*step);

void   set_compound_resource(Widget w, char *resname, char *cvar);
void   get_compound_resource(Widget w, char *resname, char *cvar);

Widget get_shell_widget  (Widget w);
Widget get_toplevel_shell(Widget w);
Widget get_shell_child   (Widget w);

void   define_cursor         (Widget w, Cursor cursor);
void   undefine_cursor       (Widget w);
void   set_cursor_on_shells  (Widget w, Cursor cursor); /* use this */
void   unset_cursor_on_shells(Widget w);                /* use this */
void   create_watch_cursor   (Widget w);
void   start_watch_cursor    (void);
void   stop_watch_cursor     (void);

void   get_full_name         (Widget w, char *name);
struct HELPCTX  *get_help    (Widget w);
void   manage_widget         (Widget w);

long reduce_segments               (long x[], long y[], long number);
void draw_segments(Widget w, GC gc, long x[], long y[], long number,
                                     int x1, int y1, int x2, int y2);

Pixel get_named_color(Widget w, Colormap cmap,
                          char *cname, char *gsname, Boolean defblack);
GC    get_line_gc    (Widget w, Colormap cmap, int line_width,
                          char *cname, char *gsname, Boolean defblack);
GC    get_rubber_gc  (Widget w, int line_width);

typedef struct _RubberbandStruct RubberbandStruct;

RubberbandStruct *rubberband_start(XButtonEvent *event, Widget w, GC gc);
void rubberband_move(RubberbandStruct *rs, XMotionEvent *event);
void rubberband_stop(RubberbandStruct *rs, XButtonEvent *event,
          long *x1, long *y1, long *x2, long *y2, long *direction);
void unattach(Widget w);

/*-------------------------- make_containers.c --------------------------*/

#define WWNN  Widget parent, String name

Widget make_pulldown       (WWNN, char *label, struct HELPCTX *helpctx);
Widget make_option         (WWNN, char *label, struct HELPCTX *helpctx);
Widget make_popup          (WWNN, char *label, struct HELPCTX *helpctx);
Widget make_shell          (WWNN, char *label);
Widget make_frame          (WWNN);
Widget make_bottom         (WWNN);
Widget make_small          (WWNN);

Widget make_form           (WWNN);
Widget make_bull           (WWNN);
Widget make_draw           (WWNN);
Widget make_row            (WWNN);
Widget make_column         (WWNN);
Widget make_radiobox       (WWNN);

Widget make_framed_form    (WWNN);
Widget make_framed_bull    (WWNN);
Widget make_framed_draw    (WWNN);
Widget make_framed_row     (WWNN);
Widget make_framed_column  (WWNN);
Widget make_framed_radiobox(WWNN);

Widget make_hline          (WWNN);
Widget make_vline          (WWNN);

#undef WWNN



/*-------------------------- make_widgets.c -----------------------------*/

#if (ultrix || sun)
#define update_widgets   update_widgets_
#elif  NEED_CAPITALS
#define update_widgets   UPDATE_WIDGETS
#endif

#define WWNN     Widget parent, String name
#define LL       char   *label
#define SS       long   *sn
#define HH       struct HELPCTX *helpctx
#define DD       void   *data
#define FUN      void(*fun )(void *data, long   *error)
#define FUNI     void(*funi)(void *data, long   *old_ivar)
#define FUNF     void(*funf)(void *data, float  *old_fvar)
#define FUND     void(*fund)(void *data, double *old_dvar)
#define FUNC     void(*func)(void *data, char   *old_cvar)
#define CVAR     char   *cvar
#define IVAR     long   *ivar
#define FVAR     float  *fvar
#define DVAR     double *dvar

void   update_widgets      (void);
void   add_update_function (void (*updatefun)(void *data), void *updatedata);
void   put_user_data       (Widget w, void *data);
void  *get_user_data       (Widget w);

Widget make_label    (WWNN,LL,SS);

Widget make_uarrow   (WWNN,   SS,HH,DD,FUNI, IVAR, long*minv, long*maxv);
Widget make_darrow   (WWNN,   SS,HH,DD,FUNI, IVAR, long*minv, long*maxv);
Widget make_larrow   (WWNN,   SS,HH,DD,FUNI, IVAR, long*minv, long*maxv);
Widget make_rarrow   (WWNN,   SS,HH,DD,FUNI, IVAR, long*minv, long*maxv);
Widget make_harrow2  (WWNN,   SS,HH,DD,FUNI, IVAR, long*minv, long*maxv);

Widget make_questbox (WWNN,LL,   HH,DD,FUN ,       Widget wpop);
Widget make_filebox  (WWNN,LL,   HH,DD,FUNI,       Widget wtext);

Widget make_pushhelp (WWNN,LL,SS,HH);
Widget make_push     (WWNN,LL,SS,HH,DD,FUN ,       Widget wpop);
Widget make_pushquest(WWNN,LL,SS,HH,DD,FUN ,       Widget wpop);
Widget make_pushfile (WWNN,LL,SS,HH,DD,FUN ,       Widget wtext);

Widget make_hscale   (WWNN,LL,SS,HH,DD,FUNI, IVAR, long*minv, long*maxv);
Widget make_vscale   (WWNN,LL,SS,HH,DD,FUNI, IVAR, long*minv, long*maxv);
Widget make_hscale2  (WWNN,LL,SS,HH,DD,FUNI, IVAR, long*minv, long*maxv);

Widget make_toggle   (WWNN,LL,SS,HH,DD,FUNI, IVAR);
Widget make_radio    (WWNN,LL,SS,HH,DD,FUNI, IVAR, long id);
Widget make_opt      (WWNN,LL,SS,HH,DD,FUNI, IVAR, long id);

Widget make_ctext    (WWNN,   SS,HH,DD,FUNC, CVAR, long nchar, long nvar);
Widget make_filetext (WWNN,   SS,HH,DD,FUNC, CVAR, long nchar, long nvar);
Widget make_itext    (WWNN,   SS,HH,DD,FUNI, IVAR, long nchar);
Widget make_ftext    (WWNN,   SS,HH,DD,FUNF, FVAR, long nchar, long ndec);
Widget make_dtext    (WWNN,   SS,HH,DD,FUND, DVAR, long nchar, long ndec);

Widget make_ctext2   (WWNN,LL,SS,HH,DD,FUNC, CVAR, long nchar, long nvar);
Widget make_ctext3   (WWNN,LL,SS,HH,DD,FUNC, CVAR, long nchar, long nvar);
Widget make_filetext2(WWNN,LL,SS,HH,DD,FUNC, CVAR, long nchar, long nvar);
Widget make_filetext4(WWNN,LL,SS,HH,DD,FUNC, CVAR, long nchar, long nvar);
Widget make_itext2   (WWNN,LL,SS,HH,DD,FUNI, IVAR, long nchar);
Widget make_itext3   (WWNN,LL,SS,HH,DD,FUNI, IVAR, long nchar);
Widget make_ftext2   (WWNN,LL,SS,HH,DD,FUNF, FVAR, long nchar, long ndec);
Widget make_ftext3   (WWNN,LL,SS,HH,DD,FUNF, FVAR, long nchar, long ndec);
Widget make_dtext2   (WWNN,LL,SS,HH,DD,FUND, DVAR, long nchar, long ndec);
Widget make_dtext3   (WWNN,LL,SS,HH,DD,FUND, DVAR, long nchar, long ndec);

void   add_startup_function  (Widget w, void (*startupfun )());
void   add_verify_function   (Widget w, void (*verifyfun  )());
void   add_focusin_function  (Widget w, void (*focusinfun )());
void   add_focusout_function (Widget w, void (*focusoutfun)());

Widget make_attached_sep(Widget p, char *name);

#undef WWNN
#undef LL
#undef SS
#undef HH
#undef DD
#undef FUN
#undef FUNI
#undef FUNF
#undef FUND
#undef FUNC
#undef CVAR
#undef IVAR
#undef FVAR
#undef DVAR



/*------------------------ make_font_cursor.c ---------------------------*/

typedef struct _FontCursorStruct FontCursorStruct;

FontCursorStruct *make_font_cursor          (Widget w, unsigned int shape);
FontCursorStruct *make_font_cursor_watch    (Widget w);
FontCursorStruct *make_font_cursor_crosshair(Widget w);
FontCursorStruct *make_font_cursor_circle   (Widget w);
FontCursorStruct *destroy_font_cursor (FontCursorStruct *fcs);
void              add_font_cursor     (FontCursorStruct *fcs, Widget w);
void              remove_font_cursor  (FontCursorStruct *fcs, Widget w);
void              start_font_cursor   (FontCursorStruct *fcs);
void              stop_font_cursor    (FontCursorStruct *fcs);



/*------------------------ make_filepair.c ------------------------------*/

typedef void MakeFilepairTrap (void *data, long *valid1, long *valid2,
                       char *info1, char *info2, long *same_datasets);
typedef void MakeFilepairUpfun (void *updata);

Widget make_filepair(Widget parent, char *name, char *filetype,
      char *ext, struct HELPCTX *hctx, MakeFilepairTrap *trap, void *data,
      char *filename1, char *filename2, long required1, long required2);

Widget make_nonmatching_filepair(Widget parent, char *name,
      char *filetype1, char *filetype2, char *ext1, char *ext2, char *suffix2,
      struct HELPCTX *hctx, MakeFilepairTrap *trap, void *data,
      char *filename1, char *filename2, long required1, long required2);

Widget make_alternate_filepair(Widget parent, char *name, char *filetype,
      char *ext, struct HELPCTX *hctx, MakeFilepairTrap *trap, void *data,
      char *filename1, char *filename2, long required1, long required2);

void filepair_register_update_function
                       (Widget wpair, MakeFilepairUpfun *upfun, void *updata);
long update_filepair            (Widget wpair, char *msg4);
long        get_filepair_status (Widget wpair, int which);
const char *get_filepair_message(Widget wpair, int which);
void change_filepair_message    (Widget wpair, int which, char *msg);



/*-------------------------- make_plots.c -------------------------------*/

Widget make_plot     (Widget parent, String name, struct HELPCTX *hctx);
void   register_xaxis(Widget w, String label, float *array, long *n,
                                               long ndec1, long ndec2);
void   register_yaxis(Widget w, String label, float *array, long *n,
                                               long ndec1, long ndec2);
void   do_plot       (Widget w);



/*------------ end of stuff added by Tom Stoeckley ----------------------*/
/*------------ end of stuff added by Tom Stoeckley ----------------------*/
/*------------ end of stuff added by Tom Stoeckley ----------------------*/
/*------------ end of stuff added by Tom Stoeckley ----------------------*/

/* ehs   6-Jan-94 */
Window Select_Window(Display *dpy);
Window Select_Window_With_Pos(Display *dpy, int *retX, int *retY);
Window Find_Child(Display *dpy, Window window, int x, int y);
Window Select_Sub_Window(Display *dpy);
Pixmap wprocCreatePixmap(Display *display, Window window,
	unsigned int width, unsigned int height, unsigned int depth);
void wpGetVisArea(Widget da, int *x, int *y, int *width, int *height);
void wpCopyArea(Display *display, Drawable src, Drawable dst, GC gc,
	int srcX, int srcY, int width, int height, int dstX, int dstY);
char *wpDisplayString(Display *display);
int  wpSystem(char *command);    /* returns True-success or False-Falure */
Bool wpMoreEvents(Display *display, Window window, int type);
void wpPutImage(Display *display, Drawable drawable, GC gc, XImage *image,
        int src_x, int src_y, int dest_x, int dest_y,
        unsigned width, unsigned height);
Bool wpDoesSaveUnders(Screen *screen);

Bool wpIsXServer(const char *name);
int wpAccess(const char *prog);

#ifdef __cplusplus  
}
#endif


#endif       /* _WPROCDEF_H_ */
