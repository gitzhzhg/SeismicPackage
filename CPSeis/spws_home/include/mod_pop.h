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
#ifndef _MODPOP_H

#define _MODPOP_H

/*
 *    mod_pop.h - 
 */

#include "wproc.h"
#include "image.h"
#include "model_io.h"
#include "model.h"
#include "pick.h"

/* ---------------------------------------------------------- */
#define DEFAULT_LINE_COLOR "red"
#define MODPICK 0
#define VELPICK 1

#define MAX_MOD_WIG 60

#define IFIL       0
#define OFIL       1
#define PHEAD      2
#define SHEAD      3
#define THEAD      4

#define VBLD       5
#define GWSG       6
#define TOMP       7
#define INT        8

#define NEWHORT    9
#define REHORT    10
#define PCKTYP    11
#define EDIT      12
#define ENDSEG    13
#define DELSEL    14
#define UNDO      15


#define COLOR     19
#define INFO      20
#define MSG       21

#define FBUT      22
#define MODPOP    23

#define HORTEXT   24
#define SEGTEXT   25
#define SEGSCL    26
#define COLOP     27


/*
 * The following are the color widgets in the option menu.
 * For the routine to work the following colors must always be numbered 
 * together consecitivly and C0 must be the first in the group.
 */
#define C0   28      
#define C1   29
#define C2   30
#define C3   31
#define C4   32
#define C5   33
#define C6   34
#define C7   35
#define C8   36
#define C9   37



#define WDESTROY  40

#define ADDBEG    41
#define ADDEND    42
#define MOVSEG    43
#define INSPCK    44
#define SHPCK     45
#define AUTOP     48
 

#define MOD_Q     46
#define HOR_LIST  47

#define SNAPOP     48
#define MODEOP     49

#define MANNO     50
#define MANWITH   51
#define AUTO      52

#define PEAK      53 
#define TROFF     54
#define PTOM      55
#define MTOP      56

/* -------------------------------------------------------- */

#define NUM_HORTS 40
#define NUMCOLS  10



/*
 * type of deletes we can do
 */
enum delete_types  { delete_hor, delete_seg, delete_pt, delete_none };



#define ACTIVE_LINEW  3
#define OTHER_LINEW   2

#define CSTRLEN 50
#define MAXSEG 100

#define DEFHORIZON "Horizon-"




struct horizon_info{
                     char  hor_name[CSTRLEN];
                     long  hor_num;
                     int   num_segs;
                     int   cseg;
                     char  col_str[CSTRLEN];
                     long  cpixel;
                     Boolean  new;
                     ErsHorizon          *horptr;
                     ErsHorizon          *segary[MAXSEG];
                     struct horizon_info *nxt;
                    };


struct PickTransParsed {
                          XtTranslations add_end;
                          XtTranslations add_beg;
                          XtTranslations seg_mov;
                          XtTranslations ins_pt;
                          XtTranslations cre_del_hor;
                          XtTranslations show_pt;
                          XtTranslations auto_pck;
                          XtTranslations no_tran;
                       };




struct mod_popinfo {
                      wunion     mwig[MAX_MOD_WIG];
                      struct CB  mcb[MAX_MOD_WIG]; 
                      Cursor     watch_cur;
                      Widget     input_form;          /* at the top */
                      Widget     ctl_form;            /* at the bottom */
                      Widget     errbox;
                      Widget     infobox;
                      Widget     chor_lab; 
                      Widget     ptypL; 
                      Widget     hor_list;   /* hoizon scrolled list widget */
                      Widget     seg_list;   /* segment scrolled list widget */
                      Widget     mat_tf;
                      Widget     snaprc,keyrc;
                      Widget     help_text;
                      Widget     hzmenu,newhort,delhor,delseg,endseg;
                      Widget     comp_pb,comp_form;
                      Widget     save_pb,save_form;
                      Dimension  org_w, org_h;
                      Boolean    cb_enabled;
                      Boolean    may_pick;
                      struct PlotImage    *image;
                      HelpCtx    helpctx;
                      struct CURRFLD_INFO fld_info;
                      void       (*push_func)();
                      void       *push_data;
                      struct PickTransParsed tranpars;
                      Boolean    input_disp;
                      Boolean    load_in_progress;
                      char       last_hor_name[CSTRLEN];
                      int        last_cseg;
                      long       row, col;    /* current row and column  */
                      long       target_row;  /* where we want to be */

                      long       curr_idx;    /* allocated color index   */
                      char       curr_cnum;   /* number off chosen button*/
                      long       hor_tot;     /* total # of horizons     */
                      long       cseg;        /* current segment         */
                      long       chort;       /* current horizon         */
                      long       auto_cnt;    /* used for making horizons*/
                      long       pick_mode;   /* AUTO, MANNO, MANWITH */
                      long       snap_mode;   /* PEAK, TROFF, PTOM, MTOP */
                      struct horizon_info *chorptr;
                      struct horizon_info *hor_head;
                      struct horizon_info *hor_tail;
                      enum   delete_types deltyp;
                      char                *color_strary[NUMCOLS];
                      Pixel               good_cells[NUMCOLS];
                      long                good_cells_widx[NUMCOLS];
                      long                tot_good_cells;
                      long                nxt_good_cell;
                      Pixel               curr_cell;
                      char                curr_color[CSTRLEN];
                      /* data */
                      char        infile[200];
                      char        outfile[200];
                      long        phead;
                      long        shead;
                      long        thead;
                      long        pcktyp;
                      long        outtyp;
                      ErsModel    *model;
                      PR_ *pikrec;
                      Boolean     can_destroy_model; /* does GUI own model?*/
                      int         Spik_or_Vpik;      /* model component */
                     };

typedef struct mod_popinfo ModInfo;

/*
 * Macro to show the number of segments and the current segment on a 
 * scale bar.  if less than 2 segments the scale bar is unmanaged.
 * params: w- scale bar widget, n- total num of segs, c- current segment
 */
#define set_segdisp(w,n,c) if ((n)>1)                                        \
                                XtManageChild( (w) ),                        \
                                XtVaSetValues( (w), XmNmaximum, (n),         \
                                                    XmNvalue, ((c)+1), NULL);\
                             else  XtUnmanageChild( (w) );        
                                


#endif
