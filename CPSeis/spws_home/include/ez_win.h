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
#ifndef EZwindef
#define EZwindef

#include "c2f_interface.h"

#include "wproc.h"
/**************************************************************
 *            EZmotif: A simple motif interface??
 * TYPEs of Window objects recognized by EZwinbld().
 *************************************************************/
#define TYP_NONE  -15
#define TYP_BYPC  -4   /* BYPASS FIELD for text       */
#define TYP_BYPD  -3   /* BYPASS FIELD for text       */
#define TYP_BYPF  -2   /* BYPASS FIELD for text       */
#define TYP_BYPI  -1   /* BYPASS FIELD for text       */
#define TYP_LABEL 0    /* Label widget type - comment */
#define TYP_INT   1    /* Text widget - scalar integer*/
#define TYP_FLOAT 2    /* Text widget - scalar float  */
#define TYP_DBL   3    /* Text widget - scalar double */
#define TYP_CHAR  4    /* Text widget - string,1 line */
#define TYP_PBCAN 5    /* Pushbutton  - CANCEL type   */
#define TYP_PBHLP 6    /* Pushbutton  - HELP   type   */
#define TYP_PBOK  7    /* Pushbutton  - OK type       */
#define TYP_PBAPP 8    /* Pushbutton  - APPLY  type   */
#define TYP_PBFS  9    /* PB+Text for file selection  */
#define TYP_ARR   10   /* Drawing area for array object*/
#define TYP_PBP   11   /* Previous button for EZED scr*/
#define TYP_PBN   12   /* Next button for EZED screen */
#define TYP_PBSE  13   /* EZED session exit button    */
#define CSTRING   0
#define FSTRING   1

/**************************************************************
 * Some parameters defining EZmotif limits and behavior.
 *************************************************************/
#define MaxWindows 30   /* Maximum EZ windows per session    */
#define MaxObjects 500  /* Maximum EZ objects per session    */
#define MaxOPerW   90   /* Maximum objects per window        */
#define MaxSes     20   /* Maximum EZ Sessions per client    */
#define MAXCH      80
#define MXLNK      10   /* Max. no. of links in linked arrays*/
#define ez_IMMEDIATE   1   /* value for update flag             */
#define ez_DELAY       2   /* value for update flag             */

/**************************************************************
 *         EZmotif: Structures for Window definition.
 * EZwlist A structure that records active windows in a session.
 * EZolist A structure that records active objects in a session.
 * EZwin   A structure that defines a single window.
 * WINobj  A structure that defines a window scalar-object  
 * arrwin  A structure that defines a window array-object.
 * UserCB  A structure for user defined callback functions.
 * UCB     A pointer to a UserCB structure.
 * EZ_trap_args  A structure that support an EZED trap function
 *           typedef'd to *EZtrap
 * EZtrap  A pointer to an EZED trap structure(heirarchical)
 *************************************************************/
struct UserCB { XtCallbackProc UCBfunc;
                String         UCBname;
                int            UCBnbyt;  /* bytes in UCBdata */
                void          *UCBdata; };
typedef struct UserCB *UCB;
/*************************************************************
 *    EZmotif:   Structures to help support user traps
 *************************************************************/
typedef struct EZ_trap_args { /* define an EZED trap structure */
 void (*func)(void *,void *,void *,void *,void *); /* trap pointer*/
 int  ttype; /*flag for type of ezed trap */
 void *next; /*pointer to next trap       */
 long sid;   /* session id      */} *EZtrap;


struct WINobj 
     { char  *name;   /* name to assign to the widget       */
       int   type;    /* Flag for type of window object     */
       int   objid;   /* ID value to identify the object    */
       int   update;  /* Controls EZget_text behavior       */
       void  *udata;  /* Pointer to user program variable   */
       void  *wdata;  /* Pointer to window copy of the data */
       void  *wstrptr;/* Pointer to the parent window structure*/
       char  *hlp;    /* text for user help                 */
       UCB   Ucb;     /* pointer to user callback function  */
       EZtrap eztrap; /* can be used for ezed trap storage  */ 
       void  *anydata;/* Pointer to any other user data     */
       Widget wid;    /* Widget ID for this object          */
       long  regnum;  /* Registration number                */
     };

struct arrwin 
    { long x;         /* Initial x-position of the arrays */
      long y;         /* Initial y-position of the arrays */
      long ncols;     /* Width of arrays in cells         */
      long nrows;     /* Height of arrays in cells        */
      long nmax;      /* Maximum size of arrays           */
      long *nent;     /* No. of array entrys              */
      long nlink;     /* No. of linked arrays ( < 10 )    */
      long atyp[MXLNK];/* flag for type of array entry    */
      long asiz[MXLNK];/* Array size in cells             */
      long apos[MXLNK];/* Cell x-position of a eack link  */
      long *aswc[MXLNK];/* Array switch control flag      */
      long adec[MXLNK];/* No. of decimals to display      */
      void *(avar[MXLNK]); /* pointer to data               */
      char *pr[MXLNK];/* Prompt label for each link entry */
      void (*(aet[MXLNK]))(); /* array element traps      */
      XFontStruct *Ft;/* font to use in arrays            */
      long cell_x;    /* x-size of cell in pixels         */
      void *box;      /* box structure?                   */
      long cell_y;    /* y-size of cell in pixels         */
      GC   gcnv;
      GC   gcrv;
    };

struct EZwin             /* A description of 1 window      */
    { Widget bbwid;     /* widget ID of the bulletin board */
      Widget hlpbox;    /* Widget ID of information popup  */
      Widget errbox;    /* Widget ID of error popup        */
      int    numobj;    /* No. of objects on the window    */
      long   focus;     /* Window item having focus        */
      EZtrap wtrap;     /* Pointer to window trap ,for ezed*/
      void  *helpctx;   /* Pointer to a HELPCTX structure  */
      long   sesid;     /* window belongs to session sesid */
      struct WINobj *winobj[MaxOPerW];
    };

struct EZolist { /* A list of all registered objects on all windows */
      long sesid;
      long regnum;      /* number of registered objects */
      long winno[MaxObjects];
      long objno[MaxObjects]; };

struct EZwlist { /* A list of the active EZmotif windows */
      long   sesid;
      int    numwin;
      long   stype;   /* flags fortran or c string behavior */
      long   nchars;
      char   chars[80];
      void   (*sesi)();
      EZtrap sest;
      void   *help_data;
      char   help_file[80];
      char   help_title[40];
      KeySym lastkey;
      struct EZwin *Awin[MaxWindows]; }; 
/*
 * A list of all existing sessions. Active and inactive */
struct EZslist {
      long numses;
      long sesid[MaxSes];
      struct EZwlist *AWindows[MaxSes];
      struct EZolist *AObjects[MaxSes]; };

struct Err   { 
      Widget w;
      char   msg[320]; };

/* Aliases of methods for CRAY and VMS machines */
#ifdef NEED_CAPITALS
#define find_ezed_prompt_  FIND_EZED_PROMPT
#define convert_prompt_to_screen_item_ CONVERT_PROMPT_TO_SCREEN_ITEM
#define goto_ezed_prompt_  GOTO_EZED_PROMPT
#define crt_mess_          CRT_MESS
#define crt_line_          CRT_LINE
#define turn_off_promptc_  TURN_OFF_PROMPTC
#define turn_on_promptc_   TURN_ON_PROMPTC
#define get_ezed_pfsw_     GET_EZED_PFSW
#define set_ezed_dirs_     SET_EZED_DIRS
#endif

#if (VMS || _AIX || __hpux)
#define find_ezed_prompt_  find_ezed_prompt
#define convert_prompt_to_screen_item_ convert_prompt_to_screen_item
#define goto_ezed_prompt_  goto_ezed_prompt
#define crt_mess_          crt_mess
#define crt_line_          crt_line
#define turn_off_promptc_  turn_off_promptc
#define turn_on_promptc_   turn_on_promptc
#define get_ezed_pfsw_     get_ezed_pfsw
#define set_ezed_dirs_     set_ezed_dirs
#endif
 
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* Prototypes for functions in EZmotif.c */
void scantraverse(struct EZwin *Cwin);
void ez_create_ses(long *, long *, struct EZolist **,
                   struct EZwlist **, char fh[], char ft[]);
void EZsetstype(struct EZwlist *, long);
void EZgetstype(struct EZwlist *, long *);
void ez_create_win_(struct EZwin **, long *);
long ezregobj_(char *, int *, int *, char *, void *, UCB, struct EZwin **);
long ezreg_arr_(char *, int *,  char *, void *(a[]), long *, long *, long *,
     long atyp[], long asiz[], long apos[],long *aswc[], long adec[],
     char *pr[], void (*(func[]))(), struct EZwin **);
void ezwinbld_(Widget *, struct EZwin **, Widget *, char *,
               HelpCtx *);
void ezwinbld2_(int opt,  Widget Parent, struct EZwin *Cwin ,
                Widget *EZbb, char *rname, HelpCtx *HCTX);
void EZhelp_init(struct EZwin *Cwin, HelpCtx *HCTX);
void ez_ptrap_(Widget, EZtrap , XEvent *);
void ezadd_cb(void (*a1)(),int nbyt,void *cbdata,char *cbname,UCB *CB);
void ezadd_ptrap(void (*f)(),int , char *, UCB *);
void ezadd_eztrap(void (*f)(),int , long *, EZtrap);
struct WINobj *getwobj(long *, long *);
long getrnum( Widget ,long *, int *, int *);
long getobjno(  long *, long *);
void EZwin_free(struct EZwin *);
void EZfree_winobj(Widget , XtPointer  WINobj, XtPointer );
void EZfree_window(Widget , XtPointer  EZwin, XtPointer );
void EZfree_session(Widget , struct EZwlist **, XtPointer );
void EZmsg(Widget,  char *msg);
int  ezget_parent_( Widget );
Boolean check_trav_(long *, long *, long *, long *, long );
Boolean ezcanvisit(struct WINobj *);

void find_ezed_prompt_(char *, long *);
void convert_prompt_to_screen_item_(long *, long *, long *);
void goto_ezed_prompt_(char *str, long *nxtscr, long *nxtitm);
void crt_mess_(long *, char *);
void crt_line_(long *, char *);
void turn_off_promptc_(char *);
void turn_on_promptc_(char *, void (*pt)(), long *);
void get_ezed_pfsw_(long *);
void set_ezed_dirs_(long *, long *, long *);
void set_actives(long *);

void   EZpop(Widget, Widget , XtPointer);
void   EZinit_text(Widget, XtPointer , XtPointer );
void   EZset_text (Widget, XtPointer WINobj , XtPointer);
void   EZrefresh_text(Widget, XtPointer WINobj , XtPointer);
void   EZget_text (Widget, XtPointer winobj, XtPointer );
void   EZget_res  (Widget, XtPointer winobj, XtPointer );
void   EZset_win  (Widget, XtPointer EZwin  , XtPointer);
void   EZget_win  (Widget, XtPointer EZwin  , XtPointer);
Widget EZgetbyname(struct EZwin *,char *name);
void   EZvalidate (Widget, XtPointer   , XtPointer);
void   EZDoCAN    (Widget, XtPointer EZwin  , XtPointer);
void   EZDoOK     (Widget, XtPointer EZwin  , XtPointer);
void   EZDoAPP    (Widget, XtPointer EZwin  , XtPointer);
void   EZDoNext   (Widget, XtPointer EZwin  , XtPointer);
void   EZDoPrev   (Widget, XtPointer EZwin  , XtPointer);
void   EZhelpCB   (Widget, XtPointer EZwin  , XtPointer);
void   EZwinmsg   (struct EZwin * , char *);
void   EZdeol(Widget, XKeyEvent *,String *, Cardinal * );
void   EZdbol(Widget, XKeyEvent *,String *, Cardinal * );
void   EZsav_sesi(void (*f)(), long *sesid);
void  *EZget_sesi(long sesid);
void   EZsav_sese(void (*f)(), long *);
void  *EZget_sese(long sesid);

void   EZKeyH     (Widget, XtPointer ezwin, XEvent *,Boolean *);
void   EZget_winH (Widget, struct EZwin * , XEvent *);
void   EZset_winH (Widget, XtPointer EZwin, XEvent *, Boolean *);

/* Prototypes for methods defined in EZarr.c */
void ezarrwin_(Widget , struct WINobj *, HelpCtx );
void ezarrwin2(Widget parent, struct WINobj *wobj, HelpCtx);
void ezarr_cntrl(Widget , void * , XEvent *, Boolean *);
void ezarr_set(long narr, Widget *warr, struct EZwin *cwin);

/* Prototypes for methods defined in EZarr.c */
void EZfont_siz(Widget , long *, long *);
void EZset_xy(Widget W[], long *, long *, long *);
void EZget_maxxy(Widget W[], int , int *, int *);
void EZwidget_xshift(Widget W[], long * );

#ifdef __cplusplus
}                   // for C++
#endif

#endif
