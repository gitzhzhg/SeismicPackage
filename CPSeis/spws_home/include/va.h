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
#ifndef VA_H
#define VA_H
/*
 *                va.h                Header File for va.
 */

#include <stdio.h>
#include "cenv.h"
#include "wproc.h"
#include "image.h"
#include "va_const.h"
#include "vel_data.h"
#include "vel_popups.h"
#include "vel_bridge.h"
#include "va_message.h"
#include "global_pop.h"
#include <X11/Intrinsic.h>
#include <Xm/Text.h>


#define VA_VERSION  "VA - Version: 2.0, Revision: 11  October 6, 2003"

#define VA_VERSION_WIDGET "version_2_0"

#define VA_INFO "\n--------------------------------------------------------\n\
Version Notes:\n\
   1. Relinked to use self-defining ASCII files.\n\


#define WIN_TITLE "Velocity Analysis"
#define ICON_TITLE "Velocity Analysis"
#define VELAN_APPNAME  "va"
#define VELAN_APPCLASS "Va"




#define TEST 0
#define CONGRAPH 0
#define TRACEFILE 1

#ifdef __cplusplus
class UserCtl;
class PopCtl;
class SLFormPop;
class va_inpop;
class SLArrowScale;
class ViewWin;
class WinCtl;
class VersionInfo;
#else
typedef void *va_inpop;
typedef void *SLFormPop;
typedef void *UserCtl;
typedef void *PopCtl;
typedef void *SLArrowScale;
typedef void *ViewWin;
typedef void *WinCtl;
typedef void *VersionInfo;
#endif

/* -------------------------- BEGIN Widget Constants ---------------- */

#define MAX_WIDGET 108    /* total number of primitive widgets */

#define FILEPD            3
#define EDITPD            4
#define WINDOWPD          5
#define DISPLAYPD         6
#define VIEWPD            7
#define OPTIONPD          8
#define CUSTOMPD          9
#define HELPPD           10

#define NUM_CLINES        9

#define REDC              0
#define BLUEC             1
#define ORANGEC           2 
#define ZOOMC             3
#define GREENC            4
#define MAGENTAC          5
#define YELLOWC           6
#define TURQUOISEC        7
#define HOTPINKC          8
                              /* under the FILE menu */
#define READ             12
#define READ2            13
#define SAVE             14
#define QSEP             15
#define QUIT             16

                             /* elements under the WINDOW menu */
#define ISOON            18         /* toggle */
#define GVSON            19         /* toggle */
#define SEMON            20         /* toggle */
#define CMPON            21         /* toggle */
#define GRIDON           22         /* toggle */

                              /* under the EDIT menu */
#define PICK             23
#define FUN              24
#define SEP1             25        
#define SET              26
#define VFID             27
#define HEAD             28
#define RES              29
#define LAT              30
#define LAT2             31
#define RAY              32
#define DEL              33
#define MULT             34 
#define MISC             35
#define SEP2             36        
#define OFFMUTE          37

                              /* under the DISPLAY menu */
#define GVSOP            38
#define SEMOP            39
#define ISOOP            40
#define GRIDOP           41
#define ANNOOP           42
#define MOVIEOP          43

                             /* elements under the VIEW menu */
#define INFO             44
#define SEP3             45        
#define MIG              46
#define BREADTH          47
#define ERR              48
#define RESID            49
#define FREZNEL          50
#define DIP              51
#define VSEP             52
#define SEMCBAR          53
#define ISOCBAR          54

                             /* elements under the OPTION menu */
#define CVSTFLIP         55       /* toggle */
#define MOUSEMOVIE       56
#define OVERLAY          57
#define ISOUPDATE        58       /* toggle */
#define SEP4             59        
#define ZOOMOP           60       /* push */
#define TOL              61       /* push */
#define GLOBALS          62
#define USEFILEDEFS      63       /* toggle */

#define SEP5             64       /* separator */
#define SAVEDEF          65       /* push */
#define SAVETOFILE       66       /* push */
#define SYSDEF           67       /* push */
#define RELOADDEF        68       /* push */
#define RELOADFILE       69       /* push */
#define SAVEONQUIT       70       /* toggle */

                              /* under the HELP menu */
#define OVERVIEW         71
#define VELHELP          72
#define CTXSEN           73
#define SETSTATLINE      74
#define VERSION          75

                              /* more stuff */
#define STATUSW          76
#define VBAR             77
#define HBAR             78
#define VBARGVS          79
#define VBARISO          80
#define VBARSEM          81
#define VBARCMP          82
#define VBARGRID         83
#define HBARGVS          84
#define HBARISO          85
#define HBARSEM          86
#define HBARCMP          87
#define HBARGRID         88

                             /*mouse readout widgets*/
#define XOUT             89
#define YOUT             90
#define VOUT             91

                             /*main window bottom control buttons*/
#define CNTRLPNL         92
#define VIEWSLCT         93
#define ZOOMCTRL         94
#define APPLYNMOL        95
#define REMOVENMOL       96
#define HIDESHOWL        97
                             /*mouse popup menu buttons*/
#define ZOOMOP_PM        98
#define APPLYNMO_PM      99
#define REMOVENMO_PM    100
#define REFRESHVEL_PM   101
#define HIDESHOW_PM     102
#define CNTRLPNL_PM     103
#define SHOWHYPER_PM    104
#define SEP6            105
#define HEADERSHIFT     106
#define OUTPUTNMC       107


                              /*Gvs structure defines*/
#define CPS_MAX_PICKS   200
#define ADDITIVE          0
#define MULTIPLY          1
/* -------------------------- END of Widget Constants ---------------- */



/*--------------------------- status line display strings ------------*/
#define MD_VIEW    "Mode: View"
#define MD_VIEW_MH "BTN#1: None, BTN#2: None,\n\
BTN#3: Popup Menu"



#define COPY_COLORS   20

#define PICK_TOLERANCE .025


typedef struct _appres {
                         char    *graph_font;       /* bold font in plot */
                         char    *graph_small_font;   /* small font in plot */
                         Boolean private_cmap;  /* use a private color map */
                         long    num_gs_colors;   /* num gray scale colors */
                         long    num_sem_colors;  /* num contour colors */
                         long    num_cont_colors; /* num contour colors */
                         long    num_iso_colors;  /* num iso vel colors */
                         long    zline_wid;       /* zoom line width */
                         char    *zline_color;    /* zoom line color */
                         char    *help_file;     /* name of help file */
                         char    *def_file;      /* name of defaults file */
                         Boolean nodefs;         /* don't use user defaults */
                         Boolean wigonly;        /* only do wiggle traces */
                         Boolean showhelp;      /* show help for options */
                         Boolean dobacking;     /* do backing store */
                         Boolean auto_size;     /* auto size to screen - 10%*/
                      } va_res;
 





struct SEP_POSITION {
                      long formlen;
                      long formwid;
                      long form_y;
                      long form_x;
                      long set_pos;
                      long vbar_lpos;
                      long vbar_rlim;

                    };

struct DISPLAY_SCROLL {
                  Widget            sw;           /* scroll widget */
                  Widget            drawa;        /* drawing area widget */
                  Widget            vsb;          /* vertical SB */
                  Widget            hsb;          /* horizontal SB */
                  long              disp;         /* is window displayed */
                  long              disp_req;     /* user requested this plot*/
                  long              wid;          /* width of window */
                  long              height;       /* height of draw area */
                  struct PlotImage  image;        /* for the plot */
                  struct ImageInput user;         /* data for the plot */
                  ViewWin           *vw;
                      };
                        



struct Color_control {
                      long      color_flags;
                      long      org_cflgs;
                      long      visclass;
                      Boolean   always_private;
                      Boolean   priv_alloc;
                      long      cline[NUM_CLINES];
    /*  0 */          struct    COLOR_INFO col_gs;  /* gs color info*/
    /* 50 */          struct    COLOR_INFO col_sem; /* semblence color info*/
    /* 16 */          struct    COLOR_INFO col_cont;/* sem contour info */
    /* 32 */          struct    COLOR_INFO col_iso; /* isovel color info */
                     };


typedef struct _IsoStruct {      /* structure for iso-velocity plot */
          long    veltype;      /* type 0 thru 9 for iso-velocity plot */
          long iso_out_of_date; /* flag as to whether iso is out of date */
          Widget  redraw_iso;   /* pushbutton managed when iso outofdate */
          float plot_height;
      } IsoStruct;
          

struct PickRec
      {
          long            pick_mode;
          long            pick_index;
          long            hyper_index;
          long            hyper_oldindex;
          long            trace_index;
          long            x1;
          long            x2;
          long            y1;
          long            y2;
          long            x_prev;      /* x previous function to post */
          long            x_next;      /* x next function to post */
          long            y_prev;      /* y previous function to post */
          long            y_next;      /* y next function to post */
          long            refr;        /* reference function to post*/
          long            current;     /* show current x function*/
          long            post_hyper;  /* show hyperbolae - T or F */
          long            post_dpyRef; /* show display only reference */
          float           pick_time;
          float           pick_velocity;
          Boolean         semb_move; 
          Boolean         hyper_move;
          Boolean         cvst_move;
          Boolean         moved_pick;
          GC              pick_gc;
          XSegment        *pick_seg;
      };

struct VaTranslations 
      {
          XtTranslations  pick_sem_actions;
          XtTranslations  pick_zoom_sem_actions;
          XtTranslations  zoom_sem_actions;
          XtTranslations  zoom_button_actions;
          XtTranslations  variable_zoom_actions;
          XtTranslations  cmp_actions;
          XtTranslations  cvst_actions;
          XtTranslations  grid_actions;
      };

struct GvsStruct     /*This structure is populated if gvs data is detected*/
      {
          Boolean       is_gvs;            /*Is this gvs data*/
          int           vmod_type;         /*ADDITIVE or MULTIPLY*/
          float         *vmod_array;       /*Array of modifiers*/
          float         *ref_times;        /*Array of reference func times*/
          float         *ref_velocities;   /*Array of reference func vels*/   
          float         *trace_velocities; /*Array of interpolated velocities*/
          int           num_ref_picks;     /*Number of picks*/
      };

typedef struct velan_window{
          boolean      file_displayed;
          Widget       shell;                     /* toplevel shell */
          Widget       velform;                   /* top form*/
          Widget       vbar;                      /* vertical scroll bar*/
          Widget       hbar;                      /* horizontal scroll bar */
          struct DISPLAY_SCROLL  draw[MAX_DRAW];  /* drawing area info */
          struct PlotImage      *iso_under_image; /* for the plot */
          struct CB    cb[MAX_WIDGET];
          wunion       vwig[MAX_WIDGET];
          Widget       input_popup;
          Widget       version_info;    /* displays version specific info*/
          Widget       errbox;          /* for errors */
          Widget       ibox;            /* for information */
          Widget       ginfo_box;       /* for graph information */
          Widget       qbox;            /* for questions */
          Widget       scanform;        /* form for scan arrows */
          Widget       status_text;     /* text widget for status popup */
          long         stat_tpos;       /* len of string in status widget */
          Widget       statline;        /* text widget for button status */
          Widget       mode_disp;       /* text widget for display status*/
          Widget       mhelp;           /* main window help widget*/
          Widget       popm;            /* mouse button popup menu*/
          HelpCtx      helpctx;
          struct SEP_POSITION pos;
          struct Color_control cc;
          GC           xorGC;
          long         aryon;
          long         curr_vsb;
          long         curr_hsb;
          long         use_filedefs;    /* use file defaults on load */
          long         save_onquit;     /* save defaults when quitting */
          Cursor       watch_cur;
          Boolean      doing_drag;
          Boolean      doing_zoom;
          Boolean      sem_zoomed;
          long         nmo_applied;
          long         auto_loc_cvst;      /* makes movie flip to current vel*/
          long         auto_mouse_cvst;    /* permit movie by mouse motion*/ 
          long         auto_update_isovel; /* update iso plot while picking */
          long         gvs_overlay;        /* put iso plot under gvs plot */
          int          vel_type;           /* velocity type */
          int          nmo_mode;        /*1=forward,-1=reverse, 0=rev then for*/
          long         doppler;         /* T or F */
          float        doppler_val;     /* doppler value */
          float        nmc_sign;        /* 1 normal -1 non-hyperbolic*/
          float        nmc_power;       /* 2 normal 4 or 8 for non_hyperbolic*/
          float        user_mm;         /* pick tolerance in millimeters*/
          long         x_pixel_tol;     /* x grid pick tolerance*/
          long         y_pixel_tol;     /* y grid pick tolerance*/
         /* -------------- class declarations for input-------- */
          UserCtl      *uc;            /* user control class */
          PopCtl       *popctl;        /* popup control class */
          WinCtl       *winctl;
          va_inpop     *inpop;
         /* ---------------------- */
          int          view_color;     /*original background of view button*/
          SLArrowScale *selectscale;   /*select scale widget*/
          int          selectval;      /*select scale widget value*/
          SLArrowScale *gvsmoviescale; /*select scale widget*/
          SLArrowScale *isomoviescale; /*select scale widget*/
          int          gvsmovieval;    /*gvs movie val*/
          int          isomovieval;    /*iso movie val*/
          Widget       gvsmovie;       /*pushbutton widget that selects gvs*/
          Widget       isomovie;       /*pushbutton widget that selects iso*/
          Widget       movieselect;    /*container for last 2 widgets*/
          va_res       resources;
          VdStruct *vd;                /* pointer to velocity function data */
          VdStruct *vdDpyRef;          /* pointer to reference function data */
          VwStruct  vw;                /* structure needed by vel_popups.c  */
          IsoStruct iso;               /* structure for iso-velocity plot   */
          struct VaTranslations trans; /* picking action translations*/
          struct PickRec        p_rec; /* picking record structure*/
          struct global_info    gp;    /* global history structure*/
          struct GvsStruct      gvs;   /* if gvs data instead of cvst*/
          /* -------------- File Control Data ---------- */
          long         snfun;         /*number of semblance panels*/
          long         cnfun;         /*number of cmp panels*/
          float        semxloc[NFUNMAX]; /*x location of sem gathers */
          float        semyloc[NFUNMAX]; /*y location of sem gathers */
          float        cmpxloc[NFUNMAX]; /*x location of cmp gathers */
          float        cmpyloc[NFUNMAX]; /*y location of cmp gathers */
          float        gvsxloc[NFUNMAX]; /*x location of gvs gathers */
          float        gvsyloc[NFUNMAX]; /*y location of gvs gathers */
          long         number_func[MAX_DRAW]; /* number function returned */
          Boolean      blank_cmp;
          Boolean      blank_sem;
          VersionInfo  *vip;          
      } VelStruct;


Boolean va_docheck( VelStruct *vel,
                    long       flgs );

Boolean va_doplot( VelStruct *vel,
                   long       flgs );

void call_plotter( VelStruct *vel, long button);

#endif
