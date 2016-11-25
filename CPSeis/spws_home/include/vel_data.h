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



/*--------------------------- vel_data.h --------------------------------*/

/* the defined constants must exactly match corresponding parameters in the
   Fortran include file vel_data.inc  */

/* the structure VdStruct must exactly match common block vd in vel_data.inc */
/* the structure VpStruct must exactly match common block vp in vel_data.inc */

#ifndef _VEL_DATA_H
#define _VEL_DATA_H

#include "c2f_interface.h"

/*-------- fortran subroutine spelling adjustments for VMS --------------*/

#if (VMS || _AIX || __hpux)
#define start_vel_data_            start_vel_data
#define delete_select_codes_       delete_select_codes
#define fix_select_codes_          fix_select_codes
#define initialize_common_block_   initialize_common_block
#define initialize_empty_file_     initialize_empty_file
#define initialize_new_velfun_     initialize_new_velfun
#define update_this_velfun_        update_this_velfun
#define register_this_velfun_      register_this_velfun
#define previous_function_         previous_function
#define next_function_             next_function
#define get_select_counts_         get_select_counts
#define get_errmsg_counts_         get_errmsg_counts
#define get_raymsg_counts_         get_raymsg_counts
#endif

/*-------- fortran subroutine spelling adjustments for CRAY -------------*/

#ifdef NEED_CAPITALS
#define start_vel_data_            START_VEL_DATA
#define delete_select_codes_       DELETE_SELECT_CODES
#define fix_select_codes_          FIX_SELECT_CODES
#define initialize_common_block_   INITIALIZE_COMMON_BLOCK
#define initialize_empty_file_     INITIALIZE_EMPTY_FILE
#define initialize_new_velfun_     INITIALIZE_NEW_VELFUN
#define update_this_velfun_        UPDATE_THIS_VELFUN
#define register_this_velfun_      REGISTER_THIS_VELFUN
#define previous_function_         PREVIOUS_FUNCTION
#define next_function_             NEXT_FUNCTION
#define get_select_counts_         GET_SELECT_COUNTS
#define get_errmsg_counts_         GET_ERRMSG_COUNTS
#define get_raymsg_counts_         GET_RAYMSG_COUNTS
#endif



/*-------------- defined constants for array lengths -------------------*/

#define NFUNMAX  5000  /* maximum number of velocity functions in a file */
#define NMAX     200   /* maximum number of picks in a velocity function */
#define NMUTEMAX 20    /* maximum number of mute indices                 */
#define NRAYMAX  200   /* maximum number of rays to trace                */
#define NUMTYPES 11    /* number of velocity function types              */


/*-------- constants corresponding to velocity function types ----------*/

#define FIRSTTYPE 0L
#define VTNM  0L /* veltype[0] = "VTNM" (NMO velocity      vs 2-way time) */
#define VTRM  1L /* veltype[1] = "VTRM" (RMS velocity      vs 2-way time) */
#define VZRM  2L /* veltype[2] = "VZRM" (RMS velocity      vs depth)      */
#define VLRM  3L /* veltype[3] = "VLRM" (RMS velocity      vs thickness)  */
#define VTAV  4L /* veltype[4] = "VTAV" (average velocity  vs 2-way time) */
#define VZAV  5L /* veltype[5] = "VZAV" (average velocity  vs depth)      */
#define VLAV  6L /* veltype[6] = "VLAV" (average velocity  vs thickness)  */
#define VTIN  7L /* veltype[7] = "VTIN" (interval velocity vs 2-way time) */
#define VZIN  8L /* veltype[8] = "VZIN" (interval velocity vs depth)      */
#define VLIN  9L /* veltype[9] = "VLIN" (interval velocity vs thickness)  */
#define VTDP 10L /* veltype[10]= "VTDP" (depth             vs 2-way time) */
#define LASTTYPE (NUMTYPES-1L)


/*-------------- common block vd ---------------------------------------*/

typedef struct _VdStruct
    {
    long  nfun;      /* number of velocity functions in the file      */
    long  ifun;      /* current active velocity function in the file  */
    long  nhx;       /* header word for X-bin location                */
    long  nhy;       /* header word for Y-bin location                */
    long  order;     /* order    of moveout (2 or 4)                  */
    float nhosign;   /* sign     of moveout (normally 1.0 or -1.0)    */
    float nhoexp;    /* exponent of moveout (normally 2.0 or  4.0)    */
    long  inil;      /* integer value interpreted as a nil            */
    float fnil;      /* floating point value interpreted as a nil     */
    long changeflag; /* flag set to true when change made since saving */

    float offmin;           /* minimum offset for ray tracing          */
    float offmax;           /* maximum offset for ray tracing          */
    long  muteflag;         /* mute flag (1 or 2 or 3) for ray tracing */
    long  nmute;            /* number of offset/time mute pairs        */
    float omute[NMUTEMAX];  /* mute offsets for ray tracing            */
    float tmute[NMUTEMAX];  /* mute times for ray tracing              */
    float startvel;         /* default starting velocity.              */
    float bottomtime;       /* default ending time.                    */
    float timetol;          /* time tolerance for assuming equality.   */
    float depthtol;         /* depth tolerance for assuming equality.  */
    float xcenter, xwidth;  /* any xbin center, xbin width */
    float ycenter, ywidth;  /* any ybin center, ybin width */
    float xtol;             /* xbin  tolerance for assuming equality.  */
    float ytol;             /* ybin  tolerance for assuming equality.  */
    float xtol_request;     /* user-requested xbin tolerance.          */
    float ytol_request;     /* user-requested ybin tolerance.          */
    long  auto_xtol;        /* 1 = auto calc xtol; 0 = use xtol_request. */
    long  auto_ytol;        /* 1 = auto calc ytol; 0 = use ytol_request. */
    float ybin_request;     /* user-requested ybin value to use.       */
    long  xdirwant;         /* xbin sorting preference */
    long  ydirwant;         /* ybin sorting preference */

    long  nray;             /* number of traced rays       */
    float oray[NRAYMAX];    /* raytraced offsets           */
    float tray[NRAYMAX];    /* raytraced times             */

    float xbin  [NFUNMAX+3];     /* X-bin location                     */
    float ybin  [NFUNMAX+3];     /* Y-bin location                     */
    long  n     [NFUNMAX+3];     /* number of velocity function picks  */
    long  point [NFUNMAX+3];     /* pointer to structure VpStruct      */

    char vfid   [NFUNMAX+3][8];     /* velocity function name             */
    char select [NFUNMAX+3][6];     /* code indicating selected functions */
    char type   [NFUNMAX+3][4];     /* velocity function type (see below) */
    char errmsg [NFUNMAX+3][1];     /* code indicating conversion error   */
    char raymsg [NFUNMAX+3][1];     /* code indicating whether raytraced  */

    char project[NFUNMAX+3][10];    /* project name           */
    char line   [NFUNMAX+3][10];    /* line name              */
    char rdate  [NFUNMAX+3][5];     /* recording date         */
    char pdate  [NFUNMAX+3][5];     /* processing date        */
    char userid [NFUNMAX+3][3];     /* user identification    */
    char comment[NFUNMAX+3][15];    /* comment string         */

    char veltype[NUMTYPES][4];      /* velocity function types  */
    } VdStruct;


/*-------- macro to return pointers to velocity function picks ---------*/

  /*  Usage:
               PickPoints(vpoint, tpoint, vel->vd->point[i], veltype);
      Where:
        i  =  desired velocity function number (0 thru vel->vd->nfun - 1).
        veltype  =  desired velocity function type   (0 thru 9).
        float *vpoint  (returned)  =  pointer to the first velocity pick.
        float *tpoint  (returned)  =  pointer to the first time/depth pick.
  */

   
#define PickPoints(VPOINT,TPOINT,POINTI,K)                                   \
   VPOINT = TPOINT = (float*)POINTI;                                         \
   if     (K ==  0)                    VPOINT = VPOINT + NMAX * 6; /*NMO  */ \
   else if(K <=  3)                    VPOINT = VPOINT + NMAX * 3; /*RMS  */ \
   else if(K <=  6)                    VPOINT = VPOINT + NMAX * 4; /*AV   */ \
   else if(K <=  9)                    VPOINT = VPOINT + NMAX * 5; /*INT  */ \
   else if(K == 10)                    VPOINT = VPOINT + NMAX * 0; /*depth*/ \
   if     (K <= 1 || K == 4 || K == 7 || K == 10)                            \
                                       TPOINT = TPOINT + NMAX * 1; /*time */ \
   else if(K == 2 || K == 5 || K == 8) TPOINT = TPOINT + NMAX * 0; /*depth*/ \
   else if(K == 3 || K == 6 || K == 9) TPOINT = TPOINT + NMAX * 2  /*thick*/ 
   


/*-------------- common block vp (velocity function picks) -------------*/

typedef struct _VpStruct
    {
    float depths [NMAX];       /* depth picks               VZxx  */
    float times  [NMAX];       /* two-way time picks        VTxx  */
    float thick  [NMAX];       /* thickness picks           VLxx  */
    float vrms   [NMAX];       /* RMS velocity picks        xxRM  */
    float vav    [NMAX];       /* average velocity picks    xxAV  */
    float vint   [NMAX];       /* interval velocity picks   xxIN  */
    float vnmo   [NMAX];       /* NMO velocity picks        xxNM  */
    float offpick[NMAX];       /* maximum raytraced offset        */
    } VpStruct;




/*------ structure containing all info about one velocity function -----*/

typedef struct _FunStruct
    {
    float xbin;
    float ybin;
    long  n;
    long  point;  /* pointer to structure VpStruct containing picks */

    char vfid   [8];
    char select [6];
    char type   [4];
    char errmsg [1];
    char raymsg [1];

    char project[10];
    char line   [10];
    char rdate  [5];
    char pdate  [5];
    char userid [3];
    char comment[15];
    } FunStruct;

  

/*------------------ c function prototypes -----------------------------*/

#ifdef __cplusplus
extern "C" {                          /* for C++ */
#endif

void sort_velfuns  (VdStruct *vd, char *msg, long *error);
float get_velocity(VdStruct *vd, long veltype, float time, long which,
                                                          int vint_grade);
/*
void get_xbin_vels (VdStruct *vd, long veltype,
            double xbin, double time, float *y, float *v, long *n);
*/
void get_vel_limits(VdStruct *vd,
            float *vmin, float *vmax, float *tmin, float *tmax, long veltype);
void get_bin_limits(VdStruct *vd,
            float *xbinmin, float *xbinmax, float *ybinmin, float *ybinmax);
void get_velfun_list(VdStruct *vd, long xlist_flag,
    double binmin, double binmax, double bin_choice,  long *list, long *nlist);
int  find_nearby_match(long i, long direction,
            double ycenter, double ywidth, float *x, float *y, long n);
int  find_match(double xbin, double ybin,
     double xcenter, double xwidth, double ycenter, double ywidth,
                                    float *x, float *y, long n);
int  find_where(double xbin, double ybin, double xflag, double ycenter,
        double ywidth, float *x, float *y, long n);

       /* the first two return 0 if no error */
       /* the first two return 1 if error - parameters set for normal NMO */
       /* the first two each set all 3 parameters order,nhosign,nhoexp */
       /* currently order must be 2 or 4 */
       /* currently nhosign,nhoexp must be 1,2 or -1,4 */
int   set_order  (VdStruct *vd, long order);
int   set_nonhyp (VdStruct *vd, float nhosign, float nhoexp);
long  get_order  (VdStruct *vd);
float get_nhosign(VdStruct *vd);
float get_nhoexp (VdStruct *vd);

#ifdef __cplusplus
}
#endif

 


/*---------------- fortran function prototypes -------------------------*/
    /* prototypes without arg lists contain fortran character args */
          /* this section not yet completed */
 
 

#endif

/*--------------------------- end --------------------------------------*/

