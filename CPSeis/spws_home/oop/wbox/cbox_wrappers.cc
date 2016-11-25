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

//------------------------- cbox_wrappers.cc -----------------------//
//------------------------- cbox_wrappers.cc -----------------------//
//------------------------- cbox_wrappers.cc -----------------------//

  //  This file contains private Fortran-callable C-language
  //  routines which are called directly from Fortran wrappers
  //  in the fbox_wrappers.f file.

  //  Routines in this file call public and private functions
  //  in the wbox.cc file.

  //  note - some of the wbox routines take integer ibox, whereas
  //  others take void *box in their argument lists.


#include "wbox.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>


#ifdef VMS
#include <descrip.h>
#endif


typedef void WboxFortranTrapHandler (WboxGenericTrap *trap, int traptype,
                                     int ibox, int *ident, int *index,
                                     char *text, int nread, char *endkey);

void ubox_register_fortran_trap_handler(WboxFortranTrapHandler *handler);



extern "C" {


//----------- interfaces from Fortran to C or vice versa --------------//
//----------- interfaces from Fortran to C or vice versa --------------//
//----------- interfaces from Fortran to C or vice versa --------------//


/*******************************************
           ALTERNATIVE METHOD
#if (ultrix || sun || __sgi)
#define cbox_set_maxrows    cbox_set_maxrows_
#endif

#ifdef CRAY
#define cbox_set_maxrows    CBOX_SET_MAXROWS
#endif
*******************************************/



#if (VMS || _AIX || __hpux)
#define cbox_verify_fortran_integer_    cbox_verify_fortran_integer
#define cbox_verify_fortran_real_       cbox_verify_fortran_real
#define cbox_verify_fortran_double_     cbox_verify_fortran_double
#define cbox_verify_fortran_wrapup_     cbox_verify_fortran_wrapup
#define fbox_boxtrap11a_                fbox_boxtrap11a
#define fbox_boxtrap22a_                fbox_boxtrap22a

#define cbox_set_maxrows_    cbox_set_maxrows
#define cbox_get_maxrows_    cbox_get_maxrows
#define cbox_set_debug_      cbox_set_debug
#define cbox_get_debug_      cbox_get_debug
#define cbox_set_keymode_    cbox_set_keymode
#define cbox_get_keymode_    cbox_get_keymode
#define cbox_toggle_keymode_ cbox_toggle_keymode

#define cbox_maybe_          cbox_maybe
#define cbox_message_        cbox_message
#define cbox_immediate_      cbox_immediate
#define cbox_messageline_    cbox_messageline

#define cbox_get_name_       cbox_get_name
#define cbox_get_num_boxes_  cbox_get_num_boxes
#define cbox_managed_        cbox_managed
#define cbox_in_trap_        cbox_in_trap
#define cbox_is_visible_     cbox_is_visible

#define cbox_manage_         cbox_manage
#define cbox_unmanage_       cbox_unmanage
#define cbox_destroy_        cbox_destroy
#define cbox_manage_box_     cbox_manage_box
#define cbox_unmanage_box_   cbox_unmanage_box
#define cbox_destroy_box_    cbox_destroy_box

#define cbox_flush_buffer_   cbox_flush_buffer
#define cbox_ring_bell_      cbox_ring_bell
#define cbox_waste_time_     cbox_waste_time
#define cbox_set_focus_      cbox_set_focus
#define cbox_update_         cbox_update

#define cbox_event_          cbox_event
#define cbox_send_event_     cbox_send_event
#define cbox_send_message_   cbox_send_message

#define cbox_ireg_           cbox_ireg
#define cbox_freg_           cbox_freg
#define cbox_dreg_           cbox_dreg
#define cbox_creg_           cbox_creg
#define cbox_mreg_           cbox_mreg

#define cbox_ireg2_          cbox_ireg2
#define cbox_freg2_          cbox_freg2
#define cbox_dreg2_          cbox_dreg2
#define cbox_creg2_          cbox_creg2
#define cbox_ireg3_          cbox_ireg3

#define cbox_rega_           cbox_rega
#define cbox_irega_          cbox_irega
#define cbox_frega_          cbox_frega
#define cbox_drega_          cbox_drega
#define cbox_crega_          cbox_crega

#define cbox_set_generic_trap_    cbox_set_generic_trap
#define cbox_set_spoint_          cbox_set_spoint
#define cbox_set_index_behavior_  cbox_set_index_behavior
#define cbox_set_rpoint_          cbox_set_rpoint
#define cbox_set_ipoint_          cbox_set_ipoint
#define cbox_set_fpoint_          cbox_set_fpoint
#define cbox_set_dpoint_          cbox_set_dpoint
#define cbox_set_cpoint_          cbox_set_cpoint

#define cbox_nnewreg_        cbox_nnewreg
#define cbox_cnewreg_        cbox_cnewreg
#define cbox_inewreg_        cbox_inewreg
#define cbox_fnewreg_        cbox_fnewreg
#define cbox_dnewreg_        cbox_dnewreg

#define cbox_restore_previous_value_    cbox_restore_previous_value
#endif


#ifdef CRAY
#define cbox_verify_fortran_integer_    CBOX_VERIFY_FORTRAN_INTEGER
#define cbox_verify_fortran_real_       CBOX_VERIFY_FORTRAN_REAL
#define cbox_verify_fortran_double_     CBOX_VERIFY_FORTRAN_DOUBLE
#define cbox_verify_fortran_wrapup_     CBOX_VERIFY_FORTRAN_WRAPUP
#define fbox_boxtrap11a_                FBOX_BOXTRAP11A
#define fbox_boxtrap22a_                FBOX_BOXTRAP22A

#define cbox_set_maxrows_    CBOX_SET_MAXROWS
#define cbox_get_maxrows_    CBOX_GET_MAXROWS
#define cbox_set_debug_      CBOX_SET_DEBUG
#define cbox_get_debug_      CBOX_GET_DEBUG
#define cbox_set_keymode_    CBOX_SET_KEYMODE
#define cbox_get_keymode_    CBOX_GET_KEYMODE
#define cbox_toggle_keymode_ CBOX_TOGGLE_KEYMODE

#define cbox_maybe_          CBOX_MAYBE
#define cbox_message_        CBOX_MESSAGE
#define cbox_immediate_      CBOX_IMMEDIATE
#define cbox_messageline_    CBOX_MESSAGELINE

#define cbox_get_name_       CBOX_GET_NAME
#define cbox_get_num_boxes_  CBOX_GET_NUM_BOXES
#define cbox_managed_        CBOX_MANAGED
#define cbox_in_trap_        CBOX_IN_TRAP
#define cbox_is_visible_     CBOX_IS_VISIBLE

#define cbox_manage_         CBOX_MANAGE
#define cbox_unmanage_       CBOX_UNMANAGE
#define cbox_destroy_        CBOX_DESTROY
#define cbox_manage_box_     CBOX_MANAGE_BOX
#define cbox_unmanage_box_   CBOX_UNMANAGE_BOX
#define cbox_destroy_box_    CBOX_DESTROY_BOX

#define cbox_flush_buffer_   CBOX_FLUSH_BUFFER
#define cbox_ring_bell_      CBOX_RING_BELL
#define cbox_waste_time_     CBOX_WASTE_TIME
#define cbox_set_focus_      CBOX_SET_FOCUS
#define cbox_update_         CBOX_UPDATE

#define cbox_event_          CBOX_EVENT
#define cbox_send_event_     CBOX_SEND_EVENT
#define cbox_send_message_   CBOX_SEND_MESSAGE

#define cbox_ireg_           CBOX_IREG
#define cbox_freg_           CBOX_FREG
#define cbox_dreg_           CBOX_DREG
#define cbox_creg_           CBOX_CREG
#define cbox_mreg_           CBOX_MREG

#define cbox_ireg2_          CBOX_IREG2
#define cbox_freg2_          CBOX_FREG2
#define cbox_dreg2_          CBOX_DREG2
#define cbox_creg2_          CBOX_CREG2
#define cbox_ireg3_          CBOX_IREG3

#define cbox_rega_           CBOX_REGA
#define cbox_irega_          CBOX_IREGA
#define cbox_frega_          CBOX_FREGA
#define cbox_drega_          CBOX_DREGA
#define cbox_crega_          CBOX_CREGA

#define cbox_set_generic_trap_    CBOX_SET_GENERIC_TRAP
#define cbox_set_spoint_          CBOX_SET_SPOINT
#define cbox_set_index_behavior_  CBOX_SET_INDEX_BEHAVIOR
#define cbox_set_rpoint_          CBOX_SET_RPOINT
#define cbox_set_ipoint_          CBOX_SET_IPOINT
#define cbox_set_fpoint_          CBOX_SET_FPOINT
#define cbox_set_dpoint_          CBOX_SET_DPOINT
#define cbox_set_cpoint_          CBOX_SET_CPOINT

#define cbox_nnewreg_        CBOX_NNEWREG
#define cbox_cnewreg_        CBOX_CNEWREG
#define cbox_inewreg_        CBOX_INEWREG
#define cbox_fnewreg_        CBOX_FNEWREG
#define cbox_dnewreg_        CBOX_DNEWREG

#define cbox_restore_previous_value_    CBOX_RESTORE_PREVIOUS_VALUE
#endif



//------------------- type matchups ----------------------------//
//------------------- type matchups ----------------------------//
//------------------- type matchups ----------------------------//

   //  "most machines" below means the following machines:
   //      DEC   IBM   sun   SGI 32-bit mode


   //  Machine           Size     Fortran            C-language
   //  -------           ----     -------            ----------
   //  Cray                8      integer            int
   //  Cray                8      integer            long
   //  Cray                8      real               float
   //  Cray                8      double precision   float
   //  Cray               16       -                 double
   //  Cray                8       -                 void*  
   //  -------           ----     -------            ----------
   //  SGI 64-bit mode     4      integer            int
   //  SGI 64-bit mode     8       -                 long
   //  SGI 64-bit mode     4      real               float
   //  SGI 64-bit mode     8      double precision   double
   //  SGI 64-bit mode     8       -                 void*  
   //  -------           ----     -------            ----------
   //  most machines       4      integer            int
   //  most machines       4      integer            long
   //  most machines       4      real               float
   //  most machines       8      double precision   double
   //  most machines       4       -                 void*  


#define C_UNKNOWN   0
#define C_INT       1
#define C_LONG      2
#define C_FLOAT     3
#define C_DOUBLE    4


#ifdef CRAY
#define F_INTEGER   C_INT
#define F_REAL      C_FLOAT
#define F_DOUBLE    C_FLOAT
#else
#define F_INTEGER   C_INT
#define F_REAL      C_FLOAT
#define F_DOUBLE    C_DOUBLE
#endif


#ifdef CRAY
#define INTEGER  int
#define REAL     float
#define DOUBLE   float
#else
#define INTEGER  int
#define REAL     float
#define DOUBLE   double
#endif



//--------------------- verify fortran types -------------------------//
//--------------------- verify fortran types -------------------------//
//--------------------- verify fortran types -------------------------//

     // called from fortran.
     // these verify the above machine-dependent definitions.
     // these can be called at any time.

static int SIZE_PRINTED   = FALSE;
static int f_integer      = C_UNKNOWN;
static int f_real         = C_UNKNOWN;
static int f_double       = C_UNKNOWN;
static int f_integer_size = 0;
static int f_real_size    = 0;
static int f_double_size  = 0;


void cbox_verify_fortran_integer_(void *a1, void *a2)
{
  if(f_integer_size != 0) return;
  f_integer_size = (char*)a2 - (char*)a1;
  if     (f_integer_size == sizeof(int )) f_integer = C_INT;
  else if(f_integer_size == sizeof(long)) f_integer = C_LONG;
}


void cbox_verify_fortran_real_(void *a1, void *a2)
{
  if(f_real_size != 0) return;
  f_real_size = (char*)a2 - (char*)a1;
  if     (f_real_size == sizeof(float )) f_real = C_FLOAT;
  else if(f_real_size == sizeof(double)) f_real = C_DOUBLE;
}


void cbox_verify_fortran_double_(void *a1, void *a2)
{
  if(f_double_size != 0) return;
  f_double_size = (char*)a2 - (char*)a1;
  if     (f_double_size == sizeof(float )) f_double = C_FLOAT;
  else if(f_double_size == sizeof(double)) f_double = C_DOUBLE;
}



void cbox_verify_fortran_wrapup_()
{
  if(SIZE_PRINTED) return;
  if(wbox_get_debug() >= 1)
    {
    printf("size of fortran integer = %d\n", f_integer_size);
    printf("size of fortran real    = %d\n", f_real_size);
    printf("size of fortran double  = %d\n", f_double_size);
    printf("size of      int        = %d\n", sizeof(int));
    printf("size of      long       = %d\n", sizeof(long));
    printf("size of      float      = %d\n", sizeof(float));
    printf("size of      double     = %d\n", sizeof(double));
    printf("size of      void*      = %d\n", sizeof(void*));
    if(F_INTEGER == C_INT    ) printf("assumed fortran integer == int   \n");
    if(F_INTEGER == C_LONG   ) printf("assumed fortran integer == long  \n");
    if(f_integer == C_INT    ) printf("actual  fortran integer == int   \n");
    if(f_integer == C_LONG   ) printf("actual  fortran integer == long  \n");
    if(F_REAL    == C_FLOAT  ) printf("assumed fortran real    == float \n");
    if(F_REAL    == C_DOUBLE ) printf("assumed fortran real    == double\n");
    if(f_real    == C_FLOAT  ) printf("actual  fortran real    == float \n");
    if(f_real    == C_DOUBLE ) printf("actual  fortran real    == double\n");
    if(F_DOUBLE  == C_FLOAT  ) printf("assumed fortran double  == float \n");
    if(F_DOUBLE  == C_DOUBLE ) printf("assumed fortran double  == double\n");
    if(f_double  == C_FLOAT  ) printf("actual  fortran double  == float \n");
    if(f_double  == C_DOUBLE ) printf("actual  fortran double  == double\n");
    if(f_integer == F_INTEGER) printf("assumed fortran integer is correct  \n");
    else                       printf("assumed fortran integer is incorrect\n");
    if(f_real    == F_REAL   ) printf("assumed fortran real    is correct  \n");
    else                       printf("assumed fortran real    is incorrect\n");
    if(f_double  == F_DOUBLE ) printf("assumed fortran double  is correct  \n");
    else                       printf("assumed fortran double  is incorrect\n");
    SIZE_PRINTED = TRUE;
    }
  assert(f_integer == F_INTEGER);
  assert(f_real    == F_REAL);
  assert(f_double  == F_DOUBLE);
}



//------------------- prototypes of fortran routines --------------------//
//------------------- prototypes of fortran routines --------------------//
//------------------- prototypes of fortran routines --------------------//

   // these are fortran subroutines which call a fortran trap.
   // these subroutines reside in fbox_wrappers.f.
   // these subroutines are called only from this file.

void fbox_boxtrap11a_(WboxFortranTrap *otrap,
                      INTEGER *ibox, INTEGER *ident, INTEGER *index,
                      char *text, INTEGER *nread, char *endkey);
void fbox_boxtrap22a_(WboxEzedTrap *etrap,
                      INTEGER *ident, INTEGER *index,
                      char *text, INTEGER *nread, char *endkey);



//----------------------- fortran trap handler ----------------------//
//----------------------- fortran trap handler ----------------------//
//----------------------- fortran trap handler ----------------------//

      // registered with the windowbox objects.
      // called from the windowbox objects.
      // ident, index, and endkey might be changed in this routine.

static void fortran_trap_handler(WboxGenericTrap *trap, int traptype, 
                                 int ibox, int *ident, int *index,
                                 char *text, int nread, char *endkey)
{
  if(traptype == SIMPLE_FORTRAN_TRAPTYPE ||
     traptype == EZED_FORTRAN_TRAPTYPE)
      {
      if(!strcmp(endkey, "REDRAW") ||
         !strcmp(endkey, "ARRIVED")) return;
      }

  INTEGER ibox2  = (INTEGER)ibox;
  INTEGER ident2 = (INTEGER)*ident;
  INTEGER index2 = (INTEGER)*index;
  INTEGER nread2 = (INTEGER)nread;

  if(traptype == STANDARD_FORTRAN_TRAPTYPE ||
     traptype == SIMPLE_FORTRAN_TRAPTYPE)
      {
      fbox_boxtrap11a_((WboxFortranTrap*)trap,
              &ibox2, &ident2, &index2,   text, &nread2, endkey);
      }
  else if(traptype == EZED_FORTRAN_TRAPTYPE)
      {
      fbox_boxtrap22a_((WboxEzedTrap*)trap,
              &ident2, &index2,   text, &nread2, endkey);
      }

  *ident = (int)ident2;
  *index = (int)index2;
}



//----------------- register fortran trap handler ----------------//
//----------------- register fortran trap handler ----------------//
//----------------- register fortran trap handler ----------------//

    // register the above routine with the windowbox objects.

static void rrr()
{
  ubox_register_fortran_trap_handler(fortran_trap_handler);
}



//-------------------- set and get resources --------------------//
//-------------------- set and get resources --------------------//
//-------------------- set and get resources --------------------//


void cbox_set_maxrows_ (INTEGER *maxrows) { wbox_set_maxrows ((int)*maxrows); }
void cbox_set_debug_   (INTEGER *debug  ) { wbox_set_debug   ((int)*debug  ); }
void cbox_set_keymode_ (INTEGER *keymode) { wbox_set_keymode ((int)*keymode); }

void cbox_get_maxrows_ (INTEGER *maxrows)
                          { *maxrows = (INTEGER)wbox_get_maxrows(); }
void cbox_get_keymode_ (INTEGER *keymode)
                          { *keymode = (INTEGER)wbox_get_keymode(); }
void cbox_get_debug_   (INTEGER *debug  )
                          { *debug   = (INTEGER)wbox_get_debug  (); }

void cbox_toggle_keymode_ ()           { wbox_toggle_keymode(); }



//---------- display message at bottom of windowbox --------------//
//---------- display message at bottom of windowbox --------------//
//---------- display message at bottom of windowbox --------------//


void cbox_maybe_    (const char *msg) { wbox_maybe_show_message (msg); }
void cbox_message_  (const char *msg) { wbox_show_message       (msg); }
void cbox_immediate_(const char *msg) { wbox_immediate_message  (msg); }

void cbox_messageline_(INTEGER *ibox, const char *msg)
   { void *box = wbox_get_pointer((int)*ibox);
     wbox_messageline(box, msg); }



//------------------- get information --------------------------//
//------------------- get information --------------------------//
//------------------- get information --------------------------//


void cbox_get_name_     (INTEGER *ibox, char *boxname)
                { void *box = wbox_get_pointer((int)*ibox);
                  wbox_get_name(box, boxname); }

INTEGER cbox_get_num_boxes_ (void)  { return (INTEGER)wbox_get_num_boxes(); }

INTEGER  cbox_managed_   (INTEGER *ibox)
           { return (INTEGER)wbox_managed(*ibox); }

INTEGER cbox_in_trap_ ()      { return (INTEGER)wbox_in_trap(); }

INTEGER cbox_is_visible_  (INTEGER *ibox, INTEGER *ident)
            { void *box = wbox_get_pointer((int)*ibox);
              return (INTEGER)wbox_is_visible(box, (int)*ident); }



//--------------- manage or unmanage or destroy -----------------//
//--------------- manage or unmanage or destroy -----------------//
//--------------- manage or unmanage or destroy -----------------//


void     cbox_manage_    (char *boxname) { wbox_manage  (boxname); }
void     cbox_unmanage_  (char *boxname) { wbox_unmanage(boxname); }
void     cbox_destroy_   (char *boxname) { wbox_destroy (boxname); }

void     cbox_manage_box_    (INTEGER *ibox)
           { void *box = wbox_get_pointer((int)*ibox);
             wbox_manage_box  (box); }
void     cbox_unmanage_box_  (INTEGER *ibox)
           { void *box = wbox_get_pointer((int)*ibox);
             wbox_unmanage_box(box); }
void     cbox_destroy_box_   (INTEGER *ibox)
           { void *box = wbox_get_pointer((int)*ibox);
             wbox_destroy_box (box); }


//--------------------- miscellaneous ----------------------//
//--------------------- miscellaneous ----------------------//
//--------------------- miscellaneous ----------------------//


void cbox_flush_buffer_ (void)        { wbox_flush_buffer (); }
void cbox_ring_bell_    (void)        { wbox_ring_bell    (); }

void cbox_waste_time_   (INTEGER *n)  { wbox_waste_time((int)*n); }

void    cbox_set_focus_(INTEGER *ibox, INTEGER *ident, INTEGER *index)
         { void *box = wbox_get_pointer((int)*ibox);
           wbox_set_focus(box, (int)*ident, (int)*index); }

void    cbox_update_  ()      { wbox_update(); }



//----------------------- send event ------------------------//
//----------------------- send event ------------------------//
//----------------------- send event ------------------------//


void    cbox_event_(INTEGER *ibox, char *endkey)
          { void *box = wbox_get_pointer((int)*ibox);
            wbox_event(box, endkey); }

void    cbox_send_event_(INTEGER *ibox, char *endkey)
          { void *box = wbox_get_pointer((int)*ibox);
            wbox_send_event(box, endkey); }

void    cbox_send_message_(INTEGER *ibox) { wbox_send_message((int)*ibox); }



//-------------------- register single scalars --------------------//
//-------------------- register single scalars --------------------//
//-------------------- register single scalars --------------------//


void cbox_ireg_(INTEGER *id,
                   INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER*)
{
  bbox_ireg((int)*id, (int)*irow, (int)*icol, (int)*nchar);
  rrr();
}



void cbox_freg_(INTEGER *id,
                   INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER *ndec)
{
#if F_REAL == C_FLOAT
  bbox_freg((int)*id, (int)*irow, (int)*icol, (int)*nchar, (int)*ndec);
#elif F_REAL == C_DOUBLE
  bbox_dreg((int)*id, (int)*irow, (int)*icol, (int)*nchar, (int)*ndec);
#else
  assert(FALSE);
#endif
  rrr();
}



void cbox_dreg_(INTEGER *id,
                   INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER *ndec)
{
#if F_DOUBLE == C_FLOAT
  bbox_freg((int)*id, (int)*irow, (int)*icol, (int)*nchar, (int)*ndec);
#elif F_DOUBLE == C_DOUBLE
  bbox_dreg((int)*id, (int)*irow, (int)*icol, (int)*nchar, (int)*ndec);
#else
  assert(FALSE);
#endif
  rrr();
}



void cbox_creg_(INTEGER *id, INTEGER *length,
                   INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER*)
{
  int nchar2 = (int)*nchar;
  if(*nchar == 0) nchar2 = (int)*length;
  bbox_creg((int)*id, (int)*irow, (int)*icol, nchar2, (int)*length);
  rrr();
}



void cbox_mreg_(INTEGER *lenp, INTEGER *irow, INTEGER *icol)
{
  bbox_creg(0, (int)*irow, (int)*icol, (int)*lenp, (int)*lenp);
  rrr();
}



//--------------- register pairs of scalars ------------------------//
//--------------- register pairs of scalars ------------------------//
//--------------- register pairs of scalars ------------------------//


void cbox_ireg2_(INTEGER *id, INTEGER *lenp,
                 INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER*)
{
  int ncharp = (int)*lenp;
  bbox_ireg2((int)*id,
             ncharp, (int)*lenp,
             (int)*irow, (int)*icol, (int)*nchar);
  rrr();
}



void cbox_freg2_(INTEGER *id, INTEGER *lenp,
                 INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER *ndec)
{
  int ncharp = (int)*lenp;
#if F_REAL == C_FLOAT
  bbox_freg2((int)*id,
             ncharp, (int)*lenp,
             (int)*irow, (int)*icol, (int)*nchar, (int)*ndec);
#elif F_REAL == C_DOUBLE
  bbox_dreg2((int)*id,
             ncharp, (int)*lenp,
             (int)*irow, (int)*icol, (int)*nchar, (int)*ndec);
#else
  assert(FALSE);
#endif
  rrr();
}



void cbox_dreg2_(INTEGER *id, INTEGER *lenp,
                 INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER *ndec)
{
  int ncharp = (int)*lenp;
#if F_DOUBLE == C_FLOAT
  bbox_freg2((int)*id,
             ncharp, (int)*lenp,
             (int)*irow, (int)*icol, (int)*nchar, (int)*ndec);
#elif F_DOUBLE == C_DOUBLE
  bbox_dreg2((int)*id,
             ncharp, (int)*lenp,
             (int)*irow, (int)*icol, (int)*nchar, (int)*ndec);
#else
  assert(FALSE);
#endif
  rrr();
}



void cbox_creg2_(INTEGER *id, INTEGER *lenp, INTEGER *length,
                 INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER*)
{
  int ncharp = (int)*lenp;
  int nchar2 = (int)*nchar;
  if(nchar2 == 0) nchar2 = *length;
  bbox_creg2((int)*id,
             ncharp, (int)*lenp,
             (int)*irow, (int)*icol, nchar2, (int)*length);
  rrr();
}



void cbox_ireg3_(INTEGER *id, INTEGER *lenp,
                 INTEGER *irow, INTEGER *icol, INTEGER *nchar, INTEGER*)
{
  int ncharp = (int)*lenp;
  bbox_ireg3((int)*id,
             ncharp, (int)*lenp,
             (int)*irow, (int)*icol, (int)*nchar);
  rrr();
}



//-------------------- register linked arrays ----------------------//
//-------------------- register linked arrays ----------------------//
//-------------------- register linked arrays ----------------------//


void cbox_rega_(INTEGER *n, INTEGER *nmax, INTEGER *irow, INTEGER *numrow)
{
#if F_INTEGER == C_INT
  ibox_rega(n, nmax, (int)*irow, (int)*numrow);
#elif F_INTEGER == C_LONG
  wbox_rega(n, nmax, (int)*irow, (int)*numrow);
#else
  assert(FALSE);
#endif
  rrr();
}



void cbox_irega_(INTEGER *id, INTEGER *lenp,
                            INTEGER *icol, INTEGER *nchar, INTEGER*)
{
  int ncharp = (int)*lenp;
  bbox_irega((int)*id,
             ncharp, (int)*lenp,
             (int)*icol, (int)*nchar);
  rrr();
}



void cbox_frega_(INTEGER *id, INTEGER *lenp,
                            INTEGER *icol, INTEGER *nchar, INTEGER *ndec)
{
  int ncharp = (int)*lenp;
#if F_REAL == C_FLOAT
  bbox_frega((int)*id,
             ncharp, (int)*lenp,
             (int)*icol, (int)*nchar, (int)*ndec);
#elif F_REAL == C_DOUBLE
  bbox_drega((int)*id,
             ncharp, (int)*lenp,
             (int)*icol, (int)*nchar, (int)*ndec);
#else
  assert(FALSE);
#endif
  rrr();
}



void cbox_drega_(INTEGER *id, INTEGER *lenp,
                            INTEGER *icol, INTEGER *nchar, INTEGER *ndec)
{
  int ncharp = (int)*lenp;
#if F_DOUBLE == C_FLOAT
  bbox_frega((int)*id,
             ncharp, (int)*lenp,
             (int)*icol, (int)*nchar, (int)*ndec);
#elif F_DOUBLE == C_DOUBLE
  bbox_drega((int)*id,
             ncharp, (int)*lenp,
             (int)*icol, (int)*nchar, (int)*ndec);
#else
  assert(FALSE);
#endif
  rrr();
}



void cbox_crega_(INTEGER *id, INTEGER *lenp, INTEGER *length,
                            INTEGER *icol, INTEGER *nchar, INTEGER*)
{
  int ncharp = (int)*lenp;
  int nchar2 = (int)*nchar;
  if(*nchar == 0) nchar2 = (int)*length;
  bbox_crega((int)*id,
             ncharp, (int)*lenp,
             (int)*icol, nchar2, (int)*length);
  rrr();
}



//------------------- initial registrations ---------------------//
//------------------- initial registrations ---------------------//
//------------------- initial registrations ---------------------//


void  cbox_set_generic_trap_   (INTEGER *ident, WboxGenericTrap *trap)
{
  dbox_set_generic_trap((int)*ident, trap);
}


void  cbox_set_spoint_         (INTEGER *ident, INTEGER *point)
{
#if F_INTEGER == C_INT
  ibox_set_spoint((int)*ident, point);
#elif F_INTEGER == C_LONG
  dbox_set_spoint((int)*ident, point);
#else
  assert(FALSE);
#endif
}


void  cbox_set_index_behavior_ (INTEGER *ident)
{
  dbox_set_index_behavior((int)*ident);
}


void  cbox_set_rpoint_         (INTEGER *ident, INTEGER *point)
{
#if F_INTEGER == C_INT
  ibox_set_rpoint((int)*ident, point);
#elif F_INTEGER == C_LONG
  dbox_set_rpoint((int)*ident, point);
#else
  assert(FALSE);
#endif
}


void  cbox_set_ipoint_         (INTEGER *ident, INTEGER *point)
{
#if F_INTEGER == C_INT
  ibox_set_ipoint((int)*ident, point);
#elif F_INTEGER == C_LONG
  dbox_set_ipoint((int)*ident, point);
#else
  assert(FALSE);
#endif
}


void  cbox_set_fpoint_         (INTEGER *ident, REAL    *point)
{
  dbox_set_fpoint((int)*ident, point);
#if F_REAL == C_FLOAT
  dbox_set_fpoint((int)*ident, point);
#elif F_REAL == C_DOUBLE
  dbox_set_dpoint((int)*ident, point);
#else
  assert(FALSE);
#endif
}


void  cbox_set_dpoint_         (INTEGER *ident, DOUBLE  *point)
{
#if F_DOUBLE == C_FLOAT
  dbox_set_fpoint((int)*ident, point);
#elif F_DOUBLE == C_DOUBLE
  dbox_set_dpoint((int)*ident, point);
#else
  assert(FALSE);
#endif
}


void  cbox_set_cpoint_         (INTEGER *ident, char    *point)
{
#ifdef VMS
  struct dsc$descriptor_s *descriptor = (struct dsc$descriptor_s*)point;
  point = descriptor->dsc$a_pointer;
#endif
  dbox_set_cpoint((int)*ident, point);
}



//-------------------- later registrations ----------------------//
//-------------------- later registrations ----------------------//
//-------------------- later registrations ----------------------//


void cbox_nnewreg_(INTEGER *ibox, INTEGER *ident, INTEGER *n)
{
  void *box = wbox_get_pointer((int)*ibox);
#if F_INTEGER == C_INT
  ibox_reg_npoint(box, (int)*ident, n);
#elif F_INTEGER == C_LONG
  wbox_reg_npoint(box, (int)*ident, n);
#else
  assert(FALSE);
#endif
}


void cbox_inewreg_(INTEGER *ibox, INTEGER *ident, INTEGER *ivar)
{
  void *box = wbox_get_pointer((int)*ibox);
#if F_INTEGER == C_INT
  ibox_reg_ipoint(box, (int)*ident, ivar);
#elif F_INTEGER == C_LONG
  wbox_reg_ipoint(box, (int)*ident, ivar);
#else
  assert(FALSE);
#endif
}


void cbox_fnewreg_(INTEGER *ibox, INTEGER *ident, REAL *fvar)
{
  void *box = wbox_get_pointer((int)*ibox);
#if F_REAL == C_FLOAT
  wbox_reg_fpoint(box, (int)*ident, fvar);
#elif F_REAL == C_DOUBLE
  wbox_reg_dpoint(box, (int)*ident, fvar);
#else
  assert(FALSE);
#endif
}


void cbox_dnewreg_(INTEGER *ibox, INTEGER *ident, DOUBLE *dvar)
{
  void *box = wbox_get_pointer((int)*ibox);
#if F_DOUBLE == C_FLOAT
  wbox_reg_fpoint(box, (int)*ident, dvar);
#elif F_DOUBLE == C_DOUBLE
  wbox_reg_dpoint(box, (int)*ident, dvar);
#else
  assert(FALSE);
#endif
}


void cbox_cnewreg_(INTEGER *ibox, INTEGER *ident, char *cvar)
{
  void *box = wbox_get_pointer((int)*ibox);
#ifdef VMS
  struct dsc$descriptor_s *descriptor = (struct dsc$descriptor_s*)cvar;
  cvar = descriptor->dsc$a_pointer;
#endif
  wbox_reg_cpoint(box, (int)*ident, cvar);
}



//------------------ restore previous value -----------------------//
//------------------ restore previous value -----------------------//
//------------------ restore previous value -----------------------//

void cbox_restore_previous_value_
        (INTEGER *ibox, INTEGER *ident, INTEGER *index, INTEGER *istat)
{
  void *box = wbox_get_pointer((int)*ibox);
#if F_INTEGER == C_INT
  ibox_restore_previous_user_value(box, ident, index, istat);
#elif F_INTEGER == C_LONG
  wbox_restore_previous_user_value(box, ident, index, istat);
#else
  assert(FALSE);
#endif
}



//-------------------------- end of functions ---------------------//
//-------------------------- end of functions ---------------------//
//-------------------------- end of functions ---------------------//


}  // end extern "C"


//----------------------------- end -------------------------------//
//----------------------------- end -------------------------------//
//----------------------------- end -------------------------------//

