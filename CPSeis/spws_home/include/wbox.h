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

/*--------------------------- wbox.h -------------------------------*/
/*--------------------------- wbox.h -------------------------------*/
/*--------------------------- wbox.h -------------------------------*/

    /* this is the public header file for the windowbox routines */


#ifndef _WBOX_H_
#define _WBOX_H_

#include "wproc.h"



/********************************************************************
 Commented out in order to get rid of compile errors on Solaris 6.2
#ifdef __cplusplus
extern "C" {              
#endif
**********************************************************************/


/*-------------------- miscellaneous functions ------------------------*/
/*-------------------- miscellaneous functions ------------------------*/
/*-------------------- miscellaneous functions ------------------------*/

       /* Note that some functions take void *box (pointer to
          windowbox object) and others take int ibox (integer
          identification of windowbox object).  These differences
          are retained for backward compatibility. */

void   wbox_set_maxrows    (int maxrows);                 /* resource */
int    wbox_get_maxrows    (void);                        /* resource */
void   wbox_set_debug      (int debug);                   /* resource */
int    wbox_get_debug      (void);                        /* resource */
int    wbox_get_tiny_size  (void);                        /* resource */
void   wbox_set_keymode    (int keymode);                 /* resource */
int    wbox_get_keymode    (void);                        /* resource */
void   wbox_toggle_keymode (void);                        /* resource */

void   wbox_maybe_show_message       (const char *msg);   /* maybe  show */
void   wbox_show_message             (const char *msg);   /* always show */
void   wbox_immediate_message        (const char *msg);   /* always show */
void   wbox_messageline   (void *box, const char *msg);   /* always show */

void   wbox_save_table    (void *box, const char *filename); /* fnm NULL OK */

void   wbox_get_name      (void *box, char *boxname);
int    wbox_get_ibox      (void *box);
int    wbox_get_num_boxes (void);
void  *wbox_get_pointer   (int ibox);
void  *wbox_find_pointer  (const char *boxname);
Widget wbox_get_tiny      (void *box);

int    wbox_managed       (int ibox);                     /* TRUE/FALSE */
int    wbox_in_trap       (void);                         /* TRUE/FALSE */
int    wbox_is_visible    (void *box, int ident);         /* TRUE/FALSE */

void   wbox_manage        (const char *boxname);
void   wbox_unmanage      (const char *boxname);
void   wbox_destroy       (const char *boxname);
void   wbox_manage_box    (void *box);
void   wbox_unmanage_box  (void *box);
void   wbox_destroy_box   (void *box);

void   wbox_flush_buffer  (void);
void   wbox_ring_bell     (void);
void   wbox_waste_time    (int n);
void   wbox_set_focus     (void *box, int ident, int index);
void   wbox_update        (void);

void   wbox_event         (void *box, const char *endkey); /* not thru srvr */
void   wbox_send_event    (void *box, const char *endkey); /*   thru srvr   */
void   wbox_send_message  (int ibox);                      /*   thru srvr   */

void   wbox_restore_previous_user_value
                       (void *box, long *ident, long *index, long *istat);
void   ibox_restore_previous_user_value
                       (void *box, int  *ident, int  *index, int  *istat);


/*--------------------------- typedefs ----------------------------*/
/*--------------------------- typedefs ----------------------------*/
/*--------------------------- typedefs ----------------------------*/

typedef const char *WboxHardcopy
                     (void *data, int numlines, const char *default_filename);

typedef   void  WboxGenericTrap (void *box, long *ident, long *index,
                                  char *text, long *nread, char *endkey);
typedef   void  WboxFortranTrap (int *ibox, int *ident, int *index,
                                  char *text, int *nread, char *endkey);
typedef   void  WboxEzedTrap    (char *text, int *nread, int *index,
                                  int *ident, int *iexsw);
typedef   void  WboxClangTrap   (void *box, long *ident, long *index,
                                  char *text, long *nread, char *endkey);
typedef   void  WboxIvarTrap    (void *data, long ident, long index,
                                  long   ivar, long nread, char *endkey);
typedef   void  WboxFvarTrap    (void *data, long ident, long index,
                                  float  fvar, long nread, char *endkey);
typedef   void  WboxDvarTrap    (void *data, long ident, long index,
                                  double dvar, long nread, char *endkey);
typedef   void  WboxCvarTrap    (void *data, long ident, long index,
                                  char  *cvar, long nread, char *endkey);

typedef   long  WboxIupdateFun  (void *data, long ident, long index); 
typedef  float  WboxFupdateFun  (void *data, long ident, long index); 
typedef double  WboxDupdateFun  (void *data, long ident, long index); 
typedef   char *WboxCupdateFun  (void *data, long ident, long index); 
typedef   long  WboxNupdateFun  (void *data); 
typedef   void  WboxUpdate      (void *data); 

      
/*--------------------- create windowbox ---------------------------*/
/*--------------------- create windowbox ---------------------------*/
/*--------------------- create windowbox ---------------------------*/

 /* value to assign to default_traptype: */

   enum { STANDARD_FORTRAN_TRAPTYPE = 0,      /* type WboxFortranTrap */
            SIMPLE_FORTRAN_TRAPTYPE = 1,      /* type WboxFortranTrap */
              EZED_FORTRAN_TRAPTYPE = 2,      /* type WboxEzedTrap    */
                STANDARD_C_TRAPTYPE = 3 };    /* type WboxClangTrap   */

   /* WboxGenericTrap is an unspecified type which is actually any one
      of the three types specified by the above four enum constants.
      It is defined to match WboxClangTrap to reduce the need for casting,
      since usually it will actually be type WboxClangTrap.
      This anachronism is retained for backward compatibility. */

void  *wbox_create1_more(WboxGenericTrap *default_trap, int default_traptype,
                         int omit, int nrows_init);
void  *wbox_create1     (WboxGenericTrap *default_trap, int default_traptype);
Widget wbox_create2
          (const char *boxname, Widget toplevel, Widget parent, Widget w,
           HelpCtx hctx, const char *helpfile, const char *helptitle);

void  *dbox_create1  (void *userdata);
Widget dbox_create2  (Widget parent, const char *boxname, HelpCtx hctx);

void   wbox_put_userdata (void *box, void *userdata);
void  *wbox_get_userdata (void *box);

void   wbox_additional_update (WboxUpdate *updatename, void *updatedata);


/*------------------- create linked array set ---------------------*/
/*------------------- create linked array set ---------------------*/
/*------------------- create linked array set ---------------------*/

void  wbox_rega  (long *npoint, long *nmaxpoint, int irow, int numrow);
void  ibox_rega  (int  *npoint, int  *nmaxpoint, int irow, int numrow);
void  dbox_rega  (WboxNupdateFun *n, WboxNupdateFun *nmax,
                         int irow, int icol, int nchar, int numrow);
void  dbox_rega2 (WboxNupdateFun *n, WboxNupdateFun *nmax,
                         int irow, int icol, int nchar, int numrow,
                         int numrow_init);


/*---------------------- create datafields -------------------------*/
/*---------------------- create datafields -------------------------*/
/*---------------------- create datafields -------------------------*/

void wbox_mreg       (char *msg, int irow, int icol);
void wbox_message    (char *msg);
void wbox_blank_line (void);

#define TTT  WboxGenericTrap *trap
#define III  int ident
#define PPP  char *prompt, long *jsw
#define SSS                long *isw
#define QQQ  int ncharp, int lengthp
#define RRR  int irow
#define CCC  int icol, int nchar

void bbox_ireg   (     III,                        RRR, CCC            );
void bbox_freg   (     III,                        RRR, CCC, int ndec  );
void bbox_dreg   (     III,                        RRR, CCC, int ndec  );
void bbox_creg   (     III,                        RRR, CCC, int length);

void bbox_ireg2  (     III,     QQQ,               RRR, CCC            );
void bbox_freg2  (     III,     QQQ,               RRR, CCC, int ndec  );
void bbox_dreg2  (     III,     QQQ,               RRR, CCC, int ndec  );
void bbox_creg2  (     III,     QQQ,               RRR, CCC, int length);
void bbox_ireg3  (     III,     QQQ,               RRR, CCC            );

void bbox_irega  (     III,     QQQ,                    CCC            );
void bbox_frega  (     III,     QQQ,                    CCC, int ndec  );
void bbox_drega  (     III,     QQQ,                    CCC, int ndec  );
void bbox_crega  (     III,     QQQ,                    CCC, int length);

void wbox_ireg   (TTT, III,        long *var, SSS, RRR, CCC, int dummy );
void wbox_freg   (TTT, III,       float *var, SSS, RRR, CCC, int ndec  );
void wbox_dreg   (TTT, III,      double *var, SSS, RRR, CCC, int ndec  );
void wbox_creg   (TTT, III,        char *var, SSS, RRR, CCC, int dummy );

void wbox_ireg2  (TTT, III, PPP,   long *var, SSS, RRR, CCC, int dummy );
void wbox_freg2  (TTT, III, PPP,  float *var, SSS, RRR, CCC, int ndec  );
void wbox_dreg2  (TTT, III, PPP, double *var, SSS, RRR, CCC, int ndec  );
void wbox_creg2  (TTT, III, PPP,   char *var, SSS, RRR, CCC, int dummy );
void wbox_ireg3  (TTT, III, PPP,   long *var, SSS, RRR, CCC, int dummy );

void wbox_xrega  (TTT, III, PPP,   void *var, SSS,      CCC, int dummy );
void wbox_rrega  (TTT, III, PPP,   long *var, SSS,      CCC, int dummy );
void wbox_irega  (TTT, III, PPP,   long *var, SSS,      CCC, int dummy );
void wbox_frega  (TTT, III, PPP,  float *var, SSS,      CCC, int ndec  );
void wbox_drega  (TTT, III, PPP, double *var, SSS,      CCC, int ndec  );
void wbox_crega  (TTT, III, PPP,   char *var, SSS,      CCC, int dummy );

void dbox_ireg   (     III,                   SSS, RRR, CCC, int dummy );
void dbox_freg   (     III,                   SSS, RRR, CCC, int ndec  );
void dbox_dreg   (     III,                   SSS, RRR, CCC, int ndec  );
void dbox_creg   (     III,                   SSS, RRR, CCC, int dummy );

void dbox_ireg2  (     III, PPP,              SSS, RRR, CCC, int dummy );
void dbox_freg2  (     III, PPP,              SSS, RRR, CCC, int ndec  );
void dbox_dreg2  (     III, PPP,              SSS, RRR, CCC, int ndec  );
void dbox_creg2  (     III, PPP,              SSS, RRR, CCC, int dummy );
void dbox_ireg3  (     III, PPP,              SSS, RRR, CCC, int dummy );

void dbox_irega  (     III, PPP,              SSS,      CCC, int dummy );
void dbox_frega  (     III, PPP,              SSS,      CCC, int ndec  );
void dbox_drega  (     III, PPP,              SSS,      CCC, int ndec  );
void dbox_crega  (     III, PPP,              SSS,      CCC, int dummy );

#undef TTT
#undef III
#undef PPP
#undef SSS
#undef QQQ
#undef RRR
#undef CCC


/*--------------- initial registrations ----------------------------*/
/*--------------- initial registrations ----------------------------*/
/*--------------- initial registrations ----------------------------*/

void  dbox_set_generic_trap    (int ident, WboxGenericTrap *trap);
void  dbox_set_fortran_trap    (int ident, WboxFortranTrap *trap);
void  dbox_set_simple_trap     (int ident, WboxFortranTrap *trap);
void  dbox_set_ezed_trap       (int ident, WboxEzedTrap    *trap);
void  dbox_set_clanguage_trap  (int ident, WboxClangTrap   *trap);
void  dbox_set_itrap           (int ident, WboxIvarTrap    *trap);
void  dbox_set_ftrap           (int ident, WboxFvarTrap    *trap);
void  dbox_set_dtrap           (int ident, WboxDvarTrap    *trap);
void  dbox_set_ctrap           (int ident, WboxCvarTrap    *trap);

void  dbox_set_npoint          (int ident, long   *point);
void  dbox_set_nmaxpoint       (int ident, long   *point);
void  dbox_set_spoint          (int ident, long   *point);
void  dbox_set_index_behavior  (int ident);
void  dbox_set_rpoint          (int ident, long   *point);
void  dbox_set_ipoint          (int ident, long   *point);
void  dbox_set_fpoint          (int ident, float  *point);
void  dbox_set_dpoint          (int ident, double *point);
void  dbox_set_cpoint          (int ident, char   *point);

void  ibox_set_npoint          (int ident, int    *point);
void  ibox_set_nmaxpoint       (int ident, int    *point);
void  ibox_set_spoint          (int ident, int    *point);
void  ibox_set_rpoint          (int ident, int    *point);
void  ibox_set_ipoint          (int ident, int    *point);

void  dbox_set_nfun            (int ident, WboxNupdateFun *fun);
void  dbox_set_nmaxfun         (int ident, WboxNupdateFun *fun);
void  dbox_set_sfun            (int ident, WboxIupdateFun *fun);
void  dbox_set_ifun            (int ident, WboxIupdateFun *fun);
void  dbox_set_ffun            (int ident, WboxFupdateFun *fun);
void  dbox_set_dfun            (int ident, WboxDupdateFun *fun);
void  dbox_set_cfun            (int ident, WboxCupdateFun *fun);


/*-------------------- later registrations -------------------------*/
/*-------------------- later registrations -------------------------*/
/*-------------------- later registrations -------------------------*/

void  wbox_reg_hardcopy       (void *box, WboxHardcopy *trap);

void  wbox_reg_fortran_trap   (void *box, int ident, WboxFortranTrap *trap);
void  wbox_reg_simple_trap    (void *box, int ident, WboxFortranTrap *trap);
void  wbox_reg_ezed_trap      (void *box, int ident, WboxEzedTrap    *trap);
void  wbox_reg_clanguage_trap (void *box, int ident, WboxClangTrap   *trap);
void  wbox_reg_itrap          (void *box, int ident, WboxIvarTrap    *trap);
void  wbox_reg_ftrap          (void *box, int ident, WboxFvarTrap    *trap);
void  wbox_reg_dtrap          (void *box, int ident, WboxDvarTrap    *trap);
void  wbox_reg_ctrap          (void *box, int ident, WboxCvarTrap    *trap);

void  wbox_nnewreg  /*old*/   (void *box, int ident, long   *point);
void  wbox_inewreg  /*old*/   (void *box, int ident, long   *point);
void  wbox_fnewreg  /*old*/   (void *box, int ident, float  *point);
void  wbox_dnewreg  /*old*/   (void *box, int ident, double *point);
void  wbox_cnewreg  /*old*/   (void *box, int ident, char   *point);

void  wbox_reg_npoint         (void *box, int ident, long   *point);
void  wbox_reg_nmaxpoint      (void *box, int ident, long   *point);
void  wbox_reg_spoint         (void *box, int ident, long   *point);
void  wbox_reg_index_behavior (void *box, int ident);
void  wbox_reg_rpoint         (void *box, int ident, long   *point);
void  wbox_reg_ipoint         (void *box, int ident, long   *point);
void  wbox_reg_fpoint         (void *box, int ident, float  *point);
void  wbox_reg_dpoint         (void *box, int ident, double *point);
void  wbox_reg_cpoint         (void *box, int ident, char   *point);

void  ibox_reg_npoint         (void *box, int ident, int    *point);
void  ibox_reg_nmaxpoint      (void *box, int ident, int    *point);
void  ibox_reg_spoint         (void *box, int ident, int    *point);
void  ibox_reg_rpoint         (void *box, int ident, int    *point);
void  ibox_reg_ipoint         (void *box, int ident, int    *point);

void  wbox_reg_nfun           (void *box, int ident, WboxNupdateFun *fun);
void  wbox_reg_nmaxfun        (void *box, int ident, WboxNupdateFun *fun);
void  wbox_reg_sfun           (void *box, int ident, WboxIupdateFun *fun);
void  wbox_reg_ifun           (void *box, int ident, WboxIupdateFun *fun);
void  wbox_reg_ffun           (void *box, int ident, WboxFupdateFun *fun);
void  wbox_reg_dfun           (void *box, int ident, WboxDupdateFun *fun);
void  wbox_reg_cfun           (void *box, int ident, WboxCupdateFun *fun);


/*---------------------- end of information ------------------------*/
/*---------------------- end of information ------------------------*/
/*---------------------- end of information ------------------------*/



/********************************************************************
 Commented out in order to get rid of compile errors on Solaris 6.2
#ifdef __cplusplus
}                      
#endif
*********************************************************************/

 
#endif

/*--------------------------- end -------------------------------------*/
/*--------------------------- end -------------------------------------*/
/*--------------------------- end -------------------------------------*/

