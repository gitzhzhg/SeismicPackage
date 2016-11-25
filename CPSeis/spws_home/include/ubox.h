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

/*----------------------- ubox.h ------------------------------*/
/*----------------------- ubox.h ------------------------------*/
/*----------------------- ubox.h ------------------------------*/

  /* this is a temporary private header file for the windowbox routines */
  /* this header file will be deleted eventually */


#ifndef _UBOX_H_
#define _UBOX_H_

#include "c2f_interface.h"

class WboxBox;

extern "C" {              /* for C++ */



/*--------------- machine-dependent adjustments ------------------*/
/*--------------- machine-dependent adjustments ------------------*/
/*--------------- machine-dependent adjustments ------------------*/


#if (VMS || _AIX || __hpux)
#define ubox_draw_                ubox_draw
#define ubox_draw2_               ubox_draw2
#define ubox_draw3_               ubox_draw3
#define fbox_set_focus_temporary_     fbox_set_focus_temporary
#define fbox_update_temporary_        fbox_update_temporary

#define windowbox_put_n_                 windowbox_put_n
#define cc2vv_                           cc2vv
#define check_text_for_validity_         check_text_for_validity
#define vv2cc_                           vv2cc
#define getpoint_or_getfun_switch_       getpoint_or_getfun_switch
#define pp2vv_                           pp2vv
#define vv2pp_                           vv2pp
#define cc2pp_                           cc2pp
#define pp2cc_                           pp2cc
#define pp2pp_                           pp2pp
#define step_radio_array_value_          step_radio_array_value
#define compare_                         compare
#define save_previous_value_             save_previous_value
#define restore_previous_value_          restore_previous_value
#define wbox_location_                   wbox_location
#define wbox_clocation_                  wbox_clocation
#define wbox_null_                       wbox_null
#define wbox_pcopy_                      wbox_pcopy
#define wbox_pointer_is_null_            wbox_pointer_is_null
#define wbox_pointers_are_equal_         wbox_pointers_are_equal
#define windowbox_index_                 windowbox_index
#define fbox_boxtrap11a_                 fbox_boxtrap11a
#define fbox_boxtrap22a_                 fbox_boxtrap22a
#define boxtrap99_                       boxtrap99
#define boxtrap88_                       boxtrap88

#define box_get_irnext_                  box_get_irnext
#define box_get_icnext_                  box_get_icnext
#define box_get_jcnext_                  box_get_jcnext
#define box_get_irow9_                   box_get_irow9
#define box_get_icnext9_                 box_get_icnext9
#define box_get_numrow9_                 box_get_numrow9
#define box_get_itab9_                   box_get_itab9

#define box_get_kmin_                    box_get_kmin
#define box_get_kmax_                    box_get_kmax
#define box_get_lmin_                    box_get_lmin
#define box_get_lmax_                    box_get_lmax
#define box_get_focusflag_               box_get_focusflag
#define box_get_managed_                 box_get_managed
#define box_get_traptype_                box_get_traptype
#define box_get_nrow_                    box_get_nrow
#define box_get_ncol_                    box_get_ncol
#define box_get_ksave_                   box_get_ksave
#define box_get_lsave_                   box_get_lsave
#define box_get_oldtext_                 box_get_oldtext
#define box_get_newtext_                 box_get_newtext
#define box_get_itoggle_                 box_get_itoggle
#define box_get_nloc_                    box_get_nloc
#define box_get_kscalar_                 box_get_kscalar

#define box_get_ident_keep_              box_get_ident_keep
#define box_get_index_keep_              box_get_index_keep
#define box_get_n_keep_                  box_get_n_keep
#define box_get_nread_keep_              box_get_nread_keep
#define box_get_iswi_                    box_get_iswi
#define box_get_iswv_                    box_get_iswv
#define box_get_kfind_                   box_get_kfind
#define box_get_value_keep_              box_get_value_keep

#define box_set_irnext_                  box_set_irnext
#define box_set_icnext_                  box_set_icnext
#define box_set_jcnext_                  box_set_jcnext
#define box_set_irow9_                   box_set_irow9
#define box_set_icnext9_                 box_set_icnext9
#define box_set_numrow9_                 box_set_numrow9
#define box_set_itab9_                   box_set_itab9

#define box_set_kmin_                    box_set_kmin
#define box_set_kmax_                    box_set_kmax
#define box_set_lmin_                    box_set_lmin
#define box_set_lmax_                    box_set_lmax
#define box_set_focusflag_               box_set_focusflag
#define box_set_managed_                 box_set_managed
#define box_set_traptype_                box_set_traptype
#define box_set_nrow_                    box_set_nrow
#define box_set_ncol_                    box_set_ncol
#define box_set_ksave_                   box_set_ksave
#define box_set_lsave_                   box_set_lsave
#define box_set_oldtext_                 box_set_oldtext
#define box_set_newtext_                 box_set_newtext
#define box_set_itoggle_                 box_set_itoggle
#define box_set_nloc_                    box_set_nloc
#define box_set_kscalar_                 box_set_kscalar

#define box_set_ident_keep_              box_set_ident_keep
#define box_set_index_keep_              box_set_index_keep
#define box_set_n_keep_                  box_set_n_keep
#define box_set_nread_keep_              box_set_nread_keep
#define box_set_iswi_                    box_set_iswi
#define box_set_iswv_                    box_set_iswv
#define box_set_kfind_                   box_set_kfind
#define box_set_value_keep_              box_set_value_keep

#define link_get_iarow_                  link_get_iarow
#define link_get_narow_                  link_get_narow
#define link_get_ifirst_                 link_get_ifirst
#define link_get_karray_                 link_get_karray
#define link_get_iacol_                  link_get_iacol
#define link_get_nacol_                  link_get_nacol
#define link_get_nkeep_                  link_get_nkeep

#define link_set_narow_                  link_set_narow
#define link_set_ifirst_                 link_set_ifirst
#define link_set_karray_                 link_set_karray
#define link_set_iacol_                  link_set_iacol
#define link_set_nacol_                  link_set_nacol
#define link_set_nkeep_                  link_set_nkeep
#define link_make_index_visible_         link_make_index_visible

#define field_get_ident_                 field_get_ident
#define field_get_ibox_                  field_get_ibox
#define field_get_itab_                  field_get_itab
#define field_get_irow_                  field_get_irow
#define field_get_icol_                  field_get_icol
#define field_get_nchar_                 field_get_nchar
#define field_get_ndec_                  field_get_ndec
#define field_get_length_                field_get_length
#define field_get_svar_                  field_get_svar
#define field_get_ivar_                  field_get_ivar
#define field_get_fvar_                  field_get_fvar
#define field_get_dvar_                  field_get_dvar
#define field_get_cvar_                  field_get_cvar
#define field_get_index_                 field_get_index
#define field_get_n_                     field_get_n
#define field_get_nmax_                  field_get_nmax

#define field_is_ivar_                   field_is_ivar 
#define field_is_fvar_                   field_is_fvar 
#define field_is_dvar_                   field_is_dvar 
#define field_is_cvar_                   field_is_cvar 
#define field_is_radio_                  field_is_radio
#define field_is_index_                  field_is_index
#define field_set_svar_                  field_set_svar
#define field_set_irow_                  field_set_irow

#define fbox_get_trap_box_number_        fbox_get_trap_box_number
#define fbox_set_trap_box_number_        fbox_set_trap_box_number

#define hello_find_value_                hello_find_value
#define screenlasthello_                 screenlasthello
#define screenlasthellomaybe_            screenlasthellomaybe
#define windowbox_trap3_                 windowbox_trap3
#endif

#ifdef NEED_CAPITALS
#define ubox_draw_                UBOX_DRAW
#define ubox_draw2_               UBOX_DRAW2
#define ubox_draw3_               UBOX_DRAW3
#define fbox_set_focus_temporary_     FBOX_SET_FOCUS_TEMPORARY
#define fbox_update_temporary_        FBOX_UPDATE_TEMPORARY

#define windowbox_put_n_                 WINDOWBOX_PUT_N
#define cc2vv_                           CC2VV
#define check_text_for_validity_         CHECK_TEXT_FOR_VALIDITY
#define vv2cc_                           VV2CC
#define getpoint_or_getfun_switch_       GETPOINT_OR_GETFUN_SWITCH
#define pp2vv_                           PP2VV
#define vv2pp_                           VV2PP
#define cc2pp_                           CC2PP
#define pp2cc_                           PP2CC
#define pp2pp_                           PP2PP
#define step_radio_array_value_          STEP_RADIO_ARRAY_VALUE
#define compare_                         COMPARE
#define save_previous_value_             SAVE_PREVIOUS_VALUE
#define restore_previous_value_          RESTORE_PREVIOUS_VALUE
#define wbox_location_                   WBOX_LOCATION
#define wbox_clocation_                  WBOX_CLOCATION
#define wbox_null_                       WBOX_NULL
#define wbox_pcopy_                      WBOX_PCOPY
#define wbox_pointer_is_null_            WBOX_POINTER_IS_NULL
#define wbox_pointers_are_equal_         WBOX_POINTERS_ARE_EQUAL
#define windowbox_index_                 WINDOWBOX_INDEX
#define fbox_boxtrap11a_                 FBOX_BOXTRAP11A
#define fbox_boxtrap22a_                 FBOX_BOXTRAP22A
#define boxtrap99_                       BOXTRAP99
#define boxtrap88_                       BOXTRAP88

#define box_get_irnext_                  BOX_GET_IRNEXT
#define box_get_icnext_                  BOX_GET_ICNEXT
#define box_get_jcnext_                  BOX_GET_JCNEXT
#define box_get_irow9_                   BOX_GET_IROW9
#define box_get_icnext9_                 BOX_GET_ICNEXT9
#define box_get_numrow9_                 BOX_GET_NUMROW9
#define box_get_itab9_                   BOX_GET_ITAB9

#define box_get_kmin_                    BOX_GET_KMIN
#define box_get_kmax_                    BOX_GET_KMAX
#define box_get_lmin_                    BOX_GET_LMIN
#define box_get_lmax_                    BOX_GET_LMAX
#define box_get_focusflag_               BOX_GET_FOCUSFLAG
#define box_get_managed_                 BOX_GET_MANAGED
#define box_get_traptype_                BOX_GET_TRAPTYPE
#define box_get_nrow_                    BOX_GET_NROW
#define box_get_ncol_                    BOX_GET_NCOL
#define box_get_ksave_                   BOX_GET_KSAVE
#define box_get_lsave_                   BOX_GET_LSAVE
#define box_get_oldtext_                 BOX_GET_OLDTEXT
#define box_get_itoggle_                 BOX_GET_ITOGGLE
#define box_get_nloc_                    BOX_GET_NLOC
#define box_get_kscalar_                 BOX_GET_KSCALAR

#define box_get_ident_keep_              BOX_GET_IDENT_KEEP
#define box_get_index_keep_              BOX_GET_INDEX_KEEP
#define box_get_n_keep_                  BOX_GET_N_KEEP
#define box_get_nread_keep_              BOX_GET_NREAD_KEEP
#define box_get_iswi_                    BOX_GET_ISWI
#define box_get_iswv_                    BOX_GET_ISWV
#define box_get_kfind_                   BOX_GET_KFIND
#define box_get_value_keep_              BOX_GET_VALUE_KEEP

#define box_set_irnext_                  BOX_SET_IRNEXT
#define box_set_icnext_                  BOX_SET_ICNEXT
#define box_set_jcnext_                  BOX_SET_JCNEXT
#define box_set_irow9_                   BOX_SET_IROW9
#define box_set_icnext9_                 BOX_SET_ICNEXT9
#define box_set_numrow9_                 BOX_SET_NUMROW9
#define box_set_itab9_                   BOX_SET_ITAB9

#define box_set_kmin_                    BOX_SET_KMIN
#define box_set_kmax_                    BOX_SET_KMAX
#define box_set_lmin_                    BOX_SET_LMIN
#define box_set_lmax_                    BOX_SET_LMAX
#define box_set_focusflag_               BOX_SET_FOCUSFLAG
#define box_set_managed_                 BOX_SET_MANAGED
#define box_set_traptype_                BOX_SET_TRAPTYPE
#define box_set_nrow_                    BOX_SET_NROW
#define box_set_ncol_                    BOX_SET_NCOL
#define box_set_ksave_                   BOX_SET_KSAVE
#define box_set_lsave_                   BOX_SET_LSAVE
#define box_set_oldtext_                 BOX_SET_OLDTEXT
#define box_set_newtext_                 BOX_SET_NEWTEXT
#define box_set_itoggle_                 BOX_SET_ITOGGLE
#define box_set_nloc_                    BOX_SET_NLOC
#define box_set_kscalar_                 BOX_SET_KSCALAR

#define box_set_ident_keep_              BOX_SET_IDENT_KEEP
#define box_set_index_keep_              BOX_SET_INDEX_KEEP
#define box_set_n_keep_                  BOX_SET_N_KEEP
#define box_set_nread_keep_              BOX_SET_NREAD_KEEP
#define box_set_iswi_                    BOX_SET_ISWI
#define box_set_iswv_                    BOX_SET_ISWV
#define box_set_kfind_                   BOX_SET_KFIND
#define box_set_value_keep_              BOX_SET_VALUE_KEEP

#define link_get_iarow_                  LINK_GET_IAROW
#define link_get_narow_                  LINK_GET_NAROW
#define link_get_ifirst_                 LINK_GET_IFIRST
#define link_get_karray_                 LINK_GET_KARRAY
#define link_get_iacol_                  LINK_GET_IACOL
#define link_get_nacol_                  LINK_GET_NACOL
#define link_get_nkeep_                  LINK_GET_NKEEP
#define link_set_narow_                  LINK_SET_NAROW
#define link_set_ifirst_                 LINK_SET_IFIRST
#define link_set_karray_                 LINK_SET_KARRAY
#define link_set_iacol_                  LINK_SET_IACOL
#define link_set_nacol_                  LINK_SET_NACOL
#define link_set_nkeep_                  LINK_SET_NKEEP
#define link_make_index_visible_         LINK_MAKE_INDEX_VISIBLE

#define field_get_ident_                 FIELD_GET_IDENT
#define field_get_ibox_                  FIELD_GET_IBOX
#define field_get_itab_                  FIELD_GET_ITAB
#define field_get_irow_                  FIELD_GET_IROW
#define field_get_icol_                  FIELD_GET_ICOL
#define field_get_nchar_                 FIELD_GET_NCHAR
#define field_get_ndec_                  FIELD_GET_NDEC
#define field_get_length_                FIELD_GET_LENGTH
#define field_get_svar_                  FIELD_GET_SVAR
#define field_get_ivar_                  FIELD_GET_IVAR
#define field_get_fvar_                  FIELD_GET_FVAR
#define field_get_dvar_                  FIELD_GET_DVAR
#define field_get_cvar_                  FIELD_GET_CVAR
#define field_get_index_                 FIELD_GET_INDEX
#define field_get_n_                     FIELD_GET_N
#define field_get_nmax_                  FIELD_GET_NMAX
#define field_is_ivar_                   FIELD_IS_IVAR 
#define field_is_fvar_                   FIELD_IS_FVAR 
#define field_is_dvar_                   FIELD_IS_DVAR 
#define field_is_cvar_                   FIELD_IS_CVAR 
#define field_is_radio_                  FIELD_IS_RADIO
#define field_is_index_                  FIELD_IS_INDEX
#define field_set_svar_                  FIELD_SET_SVAR
#define field_set_irow_                  FIELD_SET_IROW
#define fbox_get_trap_box_number_        FBOX_GET_TRAP_BOX_NUMBER
#define fbox_set_trap_box_number_        FBOX_SET_TRAP_BOX_NUMBER

#define hello_find_value_                HELLO_FIND_VALUE
#define screenlasthello_                 SCREENLASTHELLO
#define screenlasthellomaybe_            SCREENLASTHELLOMAYBE
#define windowbox_trap3_                 WINDOWBOX_TRAP3
#endif



/*
     /// this might be here to avoid including wbox.h in some file
     /// somwhere:
void  dbox_set_fortran_trap    (int ident, WboxFortranTrap *trap);
*/

typedef void WboxFortranTrapHandler (WboxGenericTrap *trap, int traptype,
          int ibox, int *ident, int *index,
          char *text, int nread, char *endkey);

void ubox_register_fortran_trap_handler(WboxFortranTrapHandler *handler);



//---------------------- prototypes --------------------------//
//---------------------- prototypes --------------------------//
//---------------------- prototypes --------------------------//

void fbox_set_focus_temporary_   (int *ibox, int *ident, int *index);
void fbox_update_temporary_      (void);


class WboxAllbox *ubox_get_allbox_pointer();
void ubox_windowbox_destroyed (WboxBox *box);


void ubox_keypress_event  (WboxBox *box, const char *string, int state);
void ubox_button_event    (WboxBox *box, const char *endkey2,
                  int irow, int icol, int ibutton);
void ubox_expose_event    (WboxBox *box,
                  int irow, int icol, int irow2, int icol2, int count);
void ubox_configure_event (WboxBox *box,
                  int irow, int icol, int irow2, int icol2);
void ubox_any_event       (WboxBox *box, const char *endkey);

void ubox_set_ifirst      (int l, int ifirst2);


WboxBox       *ubox_get_box_pointer  (int ibox);
class WboxLink  *ubox_get_link_pointer (const WboxBox* const box, int itab);
class WboxField *ubox_get_field_pointer(const WboxBox* const box, int ifield);

void windowbox_put_n_          (int *ibox, int *itab, int *n);

void cc2vv_  (int *ibox, int *ifield, char *textbuf,
              int *ivar, float *fvar, double *dvar, char *hvar, int *istat);
void check_text_for_validity_
             (int *ibox, int *ifield, char *textbuf, int *istat);
void vv2cc_  (int *ibox, int *ifield, char *textbuf);
void getpoint_or_getfun_switch_(int *ibox, int *ifield, int *is);
void pp2vv_  (int *ibox, int *ifield, int *index,
              int *ivar, float *fvar, double *dvar, char *hvar);
void vv2pp_  (int *ibox, int *ivar, float *fvar, double *dvar, char *hvar,
                     int *ifield, int *index);
void cc2pp_  (int *ibox, char *textbuf, int *ifield, int *index,   int *istat);
void pp2cc_  (int *ibox, int *ifield, int *index, char *textbuf);
void pp2pp_  (int *ibox, int *ifield, int *index1, int *index2);
void step_radio_array_value_(int *ibox, int *ifield, int *index, char *endkey);
void compare_(int *ibox, int *ifield, int *index, int *ianswer);
void save_previous_value_(int *ibox, int *ifield);
void restore_previous_value_(int *ibox, int *ifield, int *istat,
                int *ivar, float *fvar, double *dvar, char *cvar);

void wbox_location_(void* value, void** point);
void wbox_clocation_(char* value, char** point);
void wbox_null_(void** point);
void wbox_pcopy_(void** point1, void** point2);
int  wbox_pointer_is_null_(int *ibox, int *ifield);
int  wbox_pointers_are_equal_(int *ibox, int *ifield1, int *ifield2);

void windowbox_index_(int *ibox, int *ifield,   int *index, int *n, int *nmax);
void boxtrap99_(int *ibox,
                int *ifield, int *ivar, float *fvar, double *dvar, char *cvar,
                int *ident, int *index,
                char *text, int *nread, char *endkey);
void boxtrap88_(int *ibox, int *ifield, int *ident, int *index,
                char *text, int *nread, char *endkey);

int    box_get_irnext_     (int *ibox);
int    box_get_icnext_     (int *ibox);
int    box_get_jcnext_     (int *ibox);
int    box_get_irow9_      (int *ibox);
int    box_get_icnext9_    (int *ibox);
int    box_get_numrow9_    (int *ibox);
int    box_get_itab9_      (int *ibox);

int    box_get_kmin_       (int *ibox);   /* ibox >= 1 */
int    box_get_kmax_       (int *ibox);
int    box_get_lmin_       (int *ibox);
int    box_get_lmax_       (int *ibox);
int    box_get_focusflag_  (int *ibox);
int    box_get_managed_    (int *ibox);
int    box_get_traptype_   (int *ibox);
int    box_get_nrow_       (int *ibox);
int    box_get_ncol_       (int *ibox);
int    box_get_ksave_      (int *ibox);
int    box_get_lsave_      (int *ibox);
void   box_get_oldtext_    (int *ibox, char *oldtext);
void   box_get_newtext_    (int *ibox, char *newtext);
int    box_get_itoggle_    (int *ibox);
int    box_get_nloc_       (int *ibox);
int    box_get_kscalar_    (int *ibox);

int    box_get_ident_keep_ (int *ibox);
int    box_get_index_keep_ (int *ibox);
int    box_get_n_keep_     (int *ibox);
int    box_get_nread_keep_ (int *ibox);
int    box_get_iswi_       (int *ibox);
int    box_get_iswv_       (int *ibox);
int    box_get_kfind_      (int *ibox);
void   box_get_value_keep_ (int *ibox, char *value_keep);

void   box_set_irnext_     (int *ibox, int  *value);
void   box_set_icnext_     (int *ibox, int  *value);
void   box_set_jcnext_     (int *ibox, int  *value);
void   box_set_irow9_      (int *ibox, int  *value);
void   box_set_icnext9_    (int *ibox, int  *value);
void   box_set_numrow9_    (int *ibox, int  *value);
void   box_set_itab9_      (int *ibox, int  *value);

void   box_set_kmin_       (int *ibox, int  *value);
void   box_set_kmax_       (int *ibox, int  *value);
void   box_set_lmin_       (int *ibox, int  *value);
void   box_set_lmax_       (int *ibox, int  *value);
void   box_set_focusflag_  (int *ibox, int  *value);
void   box_set_managed_    (int *ibox, int  *value);
void   box_set_traptype_   (int *ibox, int  *value);
void   box_set_nrow_       (int *ibox, int  *value);
void   box_set_ncol_       (int *ibox, int  *value);
void   box_set_ksave_      (int *ibox, int  *value);
void   box_set_lsave_      (int *ibox, int  *value);
void   box_set_oldtext_    (int *ibox, const char *oldtext);
void   box_set_newtext_    (int *ibox, const char *newtext);
void   box_set_itoggle_    (int *ibox, int  *value);
void   box_set_nloc_       (int *ibox, int  *value);
void   box_set_kscalar_    (int *ibox, int  *value);

void   box_set_ident_keep_ (int *ibox, int  *value);
void   box_set_index_keep_ (int *ibox, int  *value);
void   box_set_n_keep_     (int *ibox, int  *value);
void   box_set_nread_keep_ (int *ibox, int  *value);
void   box_set_iswi_       (int *ibox, int  *value);
void   box_set_iswv_       (int *ibox, int  *value);
void   box_set_kfind_      (int *ibox, int  *value);
void   box_set_value_keep_ (int *ibox, const char *value_keep);

int    link_get_iarow_    (int *ibox, int *itab);   /* itab >= 1 */
int    link_get_narow_    (int *ibox, int *itab);
int    link_get_ifirst_   (int *ibox, int *itab);
int    link_get_karray_   (int *ibox, int *itab);
int    link_get_iacol_    (int *ibox, int *itab);
int    link_get_nacol_    (int *ibox, int *itab);
int    link_get_nkeep_    (int *ibox, int *itab);
void   link_set_narow_    (int *ibox, int *itab, int *narow);
void   link_set_ifirst_   (int *ibox, int *itab, int *ifirst);
void   link_set_karray_   (int *ibox, int *itab, int *karray);
void   link_set_iacol_    (int *ibox, int *itab, int *iacol);
void   link_set_nacol_    (int *ibox, int *itab, int *nacol);
void   link_set_nkeep_    (int *ibox, int *itab, int *nkeep);
void   link_make_index_visible_ (int *ibox, int *itab, int *index);

int    field_get_ident_  (int *ibox, int *ifield);    /* ifield >= 1 */
int    field_get_ibox_   (int *ibox, int *ifield);
int    field_get_itab_   (int *ibox, int *ifield);
int    field_get_irow_   (int *ibox, int *ifield);
int    field_get_icol_   (int *ibox, int *ifield);
int    field_get_nchar_  (int *ibox, int *ifield);
int    field_get_ndec_   (int *ibox, int *ifield);
int    field_get_length_ (int *ibox, int *ifield);
int    field_get_svar_   (int *ibox, int *ifield);
int    field_get_ivar_   (int *ibox, int *ifield);
float  field_get_fvar_   (int *ibox, int *ifield);
double field_get_dvar_   (int *ibox, int *ifield);
void   field_get_cvar_   (int *ibox, int *ifield, char *cvar);
int    field_get_index_  (int *ibox, int *ifield);
int    field_get_n_      (int *ibox, int *ifield);
int    field_get_nmax_   (int *ibox, int *ifield);
int    field_is_ivar_    (int *ibox, int *ifield);
int    field_is_fvar_    (int *ibox, int *ifield);
int    field_is_dvar_    (int *ibox, int *ifield);
int    field_is_cvar_    (int *ibox, int *ifield);
int    field_is_radio_   (int *ibox, int *ifield);
int    field_is_index_   (int *ibox, int *ifield);

void   field_set_svar_   (int *ibox, int *ifield, int *svar);
void   field_set_irow_   (int *ibox, int *ifield, int *irow);

int    fbox_get_trap_box_number_();
void   fbox_set_trap_box_number_(int *trap_box_number);

void hello_find_value_(int *ibox, int *nread, int *k,
                       int *index, int *n,
                       char *cvalue,   int *istat);

void screenlasthello_      (int *ibox, int *i, int *n, int *itab);
void screenlasthellomaybe_ (int *ibox, int *i, int *n, int *itab);

void windowbox_trap3_(int *ibox, int *ident, int *index,
                      int *nread, char *endkey);


void ubox_draw3_(int *ibox, int *ifield, const char *text);
void ubox_draw2_(int *ibox, int *ifield, const char *text, int *iswitch);
void ubox_draw_(int *ibox, int *irow, int *icol,
                          const char *text, int *nchar, int *iswitch);


/*-------------- end of private header file information ---------------*/
/*-------------- end of private header file information ---------------*/
/*-------------- end of private header file information ---------------*/


}      /* for C++ */


#endif

/*--------------------------- end -------------------------------------*/
/*--------------------------- end -------------------------------------*/
/*--------------------------- end -------------------------------------*/

