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
//---------------------- PCW_fortran_prototypes.hh ------------------------//
//---------------------- PCW_fortran_prototypes.hh ------------------------//
//---------------------- PCW_fortran_prototypes.hh ------------------------//

// This header file contains the Fortran prototypes
// needed by the PCW implementation file.  They are
// placed in this file instead of the implementation
// file simply because the list is so long.

//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//

#ifndef _PCW_FORTRAN_PROTOTYPES_HH_
#define _PCW_FORTRAN_PROTOTYPES_HH_

#include "c2f_interface.h"
#include "named_constants.h"

//-------------------------- define macros --------------------------------//
//-------------------------- define macros --------------------------------//
//-------------------------- define macros --------------------------------//

#define KEY         const char    *keyword
#define ACT         const char    *action

#define GSCALAR        F90Pointer *fpoint
#define ISCALAR           INTEGER *scalar
#define FSCALAR           REAL    *scalar
#define DSCALAR           DOUBLE  *scalar
#define CSCALAR           char    *scalar
#define LSCALAR           INTEGER *scalar

#define IELEMENT          INTEGER *element
#define FELEMENT          REAL    *element
#define DELEMENT          DOUBLE  *element
#define CELEMENT          char    *element
#define LELEMENT          INTEGER *element

#define IARRAY            INTEGER *array
#define FARRAY            REAL    *array
#define DARRAY            DOUBLE  *array
#define CARRAY            char    *array
#define LARRAY            INTEGER *array

#define CARD              char    *card
#define CARDS             char    *cards
#define ERRMSG            char    *errmsg
#define NELEMENTS         INTEGER *nelements
#define NCARDS            INTEGER *ncards

#define IOPTIONS    const INTEGER *options
#define FOPTIONS    const REAL    *options
#define DOPTIONS    const DOUBLE  *options
#define COPTIONS    const char    *options
#define LOPTIONS    const INTEGER *options

#define NCHAR       const INTEGER *nchar
#define NCHARDEC    const INTEGER *nchar, const INTEGER *ndec
#define NWORDS      const INTEGER *nwords
#define NSIZE       const INTEGER *nsize
#define NOPTIONS    const INTEGER *noptions

//-------------------------- end of define macros ------------------------//
//-------------------------- end of define macros ------------------------//
//-------------------------- end of define macros ------------------------//

extern "C" {

//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//

int  pc_frou_exists ();

void pc_frou_frontend_update ();
void pc_frou_backend_update  ();
void pc_frou_gui_update      ();
void pc_frou_quick_update    ();

void pc_frou_frontend_update_noprint ();
void pc_frou_backend_update_noprint  ();
void pc_frou_gui_update_noprint      ();
void pc_frou_quick_update_noprint    ();

void pc_frou_clear                   ();
void pc_frou_restore                 ();
void pc_frou_next                    ();
void pc_frou_backend_execute         ();
void pc_frou_continue_backend_update ();

int  pc_frou_get_update_state      ();
void pc_frou_set_backend_no_exec   ();
void pc_frou_set_backend_yes_exec  ();
int  pc_frou_get_ipn               ();
int  pc_frou_previous_error        ();
void pc_frou_set_ipn               (const INTEGER *ipn);

int  pc_frou_do_not_process_traces ();

int  pc_frou_update_error          ();
void pc_frou_error                 (const char *msg);
void pc_frou_warning               (const char *msg);
void pc_frou_info                  (const char *msg);
void pc_frou_print                 (const char *msg);

                       ///////////////////////////////

void pc_frou_print_process_cards ();
void pc_frou_print_global_cards  ();
void pc_frou_print_control_cards ();
void pc_frou_print_pdata_cards   ();
void pc_frou_print_jdata_cards   ();
void pc_frou_print_gui_cards     ();

void pc_frou_info_process_cards ();
void pc_frou_info_global_cards  ();
void pc_frou_info_control_cards ();
void pc_frou_info_pdata_cards   ();
void pc_frou_info_jdata_cards   ();
void pc_frou_info_gui_cards     ();

                       ///////////////////////////////

int  pc_frou_num_elements_process (KEY);
int  pc_frou_num_elements_global  (KEY);
int  pc_frou_num_elements_control (KEY);
int  pc_frou_num_elements_gui     (KEY,ACT);
int  pc_frou_num_elements_pdata   (KEY);
int  pc_frou_num_elements_jdata   (KEY);

int  pc_frou_nature_process       (KEY);
int  pc_frou_nature_global        (KEY);
int  pc_frou_nature_control       (KEY);
int  pc_frou_nature_gui           (KEY,ACT);
int  pc_frou_nature_pdata         (KEY);
int  pc_frou_nature_jdata         (KEY);

int  pc_frou_vartype_process      (KEY);
int  pc_frou_vartype_global       (KEY);
int  pc_frou_vartype_control      (KEY);
int  pc_frou_vartype_gui          (KEY,ACT);
int  pc_frou_vartype_pdata        (KEY);
int  pc_frou_vartype_jdata        (KEY);

                       ///////////////////////////////

void pc_frou_get_gscalar         (KEY, GSCALAR);
void pc_frou_get_iscalar         (KEY, ISCALAR);
void pc_frou_get_fscalar         (KEY, FSCALAR);
void pc_frou_get_dscalar         (KEY, DSCALAR);
void pc_frou_get_lscalar         (KEY, LSCALAR);
void pc_frou_get_cscalar         (KEY, CSCALAR);

void pc_frou_get_process_gscalar (KEY, GSCALAR);
void pc_frou_get_process_iscalar (KEY, ISCALAR);
void pc_frou_get_process_fscalar (KEY, FSCALAR);
void pc_frou_get_process_dscalar (KEY, DSCALAR);
void pc_frou_get_process_lscalar (KEY, LSCALAR);
void pc_frou_get_process_cscalar (KEY, CSCALAR);

void pc_frou_get_global_gscalar  (KEY, GSCALAR);
void pc_frou_get_global_iscalar  (KEY, ISCALAR);
void pc_frou_get_global_fscalar  (KEY, FSCALAR);
void pc_frou_get_global_dscalar  (KEY, DSCALAR);
void pc_frou_get_global_lscalar  (KEY, LSCALAR);
void pc_frou_get_global_cscalar  (KEY, CSCALAR);

void pc_frou_get_control_gscalar (KEY, GSCALAR);
void pc_frou_get_control_iscalar (KEY, ISCALAR);
void pc_frou_get_control_fscalar (KEY, FSCALAR);
void pc_frou_get_control_dscalar (KEY, DSCALAR);
void pc_frou_get_control_lscalar (KEY, LSCALAR);
void pc_frou_get_control_cscalar (KEY, CSCALAR);

void pc_frou_get_gui_gscalar (KEY,ACT, GSCALAR);
void pc_frou_get_gui_iscalar (KEY,ACT, ISCALAR);
void pc_frou_get_gui_fscalar (KEY,ACT, FSCALAR);
void pc_frou_get_gui_dscalar (KEY,ACT, DSCALAR);
void pc_frou_get_gui_lscalar (KEY,ACT, LSCALAR);
void pc_frou_get_gui_cscalar (KEY,ACT, CSCALAR);

void pc_frou_get_pdata_gscalar   (KEY, GSCALAR);
void pc_frou_get_pdata_iscalar   (KEY, ISCALAR);
void pc_frou_get_pdata_fscalar   (KEY, FSCALAR);
void pc_frou_get_pdata_dscalar   (KEY, DSCALAR);
void pc_frou_get_pdata_lscalar   (KEY, LSCALAR);
void pc_frou_get_pdata_cscalar   (KEY, CSCALAR);

void pc_frou_get_jdata_gscalar   (KEY, GSCALAR);
void pc_frou_get_jdata_iscalar   (KEY, ISCALAR);
void pc_frou_get_jdata_fscalar   (KEY, FSCALAR);
void pc_frou_get_jdata_dscalar   (KEY, DSCALAR);
void pc_frou_get_jdata_lscalar   (KEY, LSCALAR);
void pc_frou_get_jdata_cscalar   (KEY, CSCALAR);

                       ///////////////////////////////

void pc_frou_get_iarray         (KEY, NSIZE, IARRAY, NELEMENTS);
void pc_frou_get_farray         (KEY, NSIZE, FARRAY, NELEMENTS);
void pc_frou_get_darray         (KEY, NSIZE, DARRAY, NELEMENTS);
void pc_frou_get_larray         (KEY, NSIZE, LARRAY, NELEMENTS);
void pc_frou_get_carray         (KEY, NSIZE, CARRAY, NELEMENTS, NWORDS);

void pc_frou_get_process_iarray (KEY, NSIZE, IARRAY, NELEMENTS);
void pc_frou_get_process_farray (KEY, NSIZE, FARRAY, NELEMENTS);
void pc_frou_get_process_darray (KEY, NSIZE, DARRAY, NELEMENTS);
void pc_frou_get_process_larray (KEY, NSIZE, LARRAY, NELEMENTS);
void pc_frou_get_process_carray (KEY, NSIZE, CARRAY, NELEMENTS, NWORDS);

void pc_frou_get_global_iarray  (KEY, NSIZE, IARRAY, NELEMENTS);
void pc_frou_get_global_farray  (KEY, NSIZE, FARRAY, NELEMENTS);
void pc_frou_get_global_darray  (KEY, NSIZE, DARRAY, NELEMENTS);
void pc_frou_get_global_larray  (KEY, NSIZE, LARRAY, NELEMENTS);
void pc_frou_get_global_carray  (KEY, NSIZE, CARRAY, NELEMENTS, NWORDS);

void pc_frou_get_control_iarray (KEY, NSIZE, IARRAY, NELEMENTS);
void pc_frou_get_control_farray (KEY, NSIZE, FARRAY, NELEMENTS);
void pc_frou_get_control_darray (KEY, NSIZE, DARRAY, NELEMENTS);
void pc_frou_get_control_larray (KEY, NSIZE, LARRAY, NELEMENTS);
void pc_frou_get_control_carray (KEY, NSIZE, CARRAY, NELEMENTS, NWORDS);

void pc_frou_get_gui_iarray (KEY,ACT, NSIZE, IARRAY, NELEMENTS);
void pc_frou_get_gui_farray (KEY,ACT, NSIZE, FARRAY, NELEMENTS);
void pc_frou_get_gui_darray (KEY,ACT, NSIZE, DARRAY, NELEMENTS);
void pc_frou_get_gui_larray (KEY,ACT, NSIZE, LARRAY, NELEMENTS);
void pc_frou_get_gui_carray (KEY,ACT, NSIZE, CARRAY, NELEMENTS, NWORDS);

void pc_frou_get_pdata_iarray   (KEY, NSIZE, IARRAY, NELEMENTS);
void pc_frou_get_pdata_farray   (KEY, NSIZE, FARRAY, NELEMENTS);
void pc_frou_get_pdata_darray   (KEY, NSIZE, DARRAY, NELEMENTS);
void pc_frou_get_pdata_larray   (KEY, NSIZE, LARRAY, NELEMENTS);
void pc_frou_get_pdata_carray   (KEY, NSIZE, CARRAY, NELEMENTS, NWORDS);

void pc_frou_get_jdata_iarray   (KEY, NSIZE, IARRAY, NELEMENTS);
void pc_frou_get_jdata_farray   (KEY, NSIZE, FARRAY, NELEMENTS);
void pc_frou_get_jdata_darray   (KEY, NSIZE, DARRAY, NELEMENTS);
void pc_frou_get_jdata_larray   (KEY, NSIZE, LARRAY, NELEMENTS);
void pc_frou_get_jdata_carray   (KEY, NSIZE, CARRAY, NELEMENTS, NWORDS);

                       ///////////////////////////////

void pc_frou_get_process_ielement (KEY, const INTEGER *indx, IELEMENT);
void pc_frou_get_process_felement (KEY, const INTEGER *indx, FELEMENT);
void pc_frou_get_process_delement (KEY, const INTEGER *indx, DELEMENT);
void pc_frou_get_process_lelement (KEY, const INTEGER *indx, LELEMENT);
void pc_frou_get_process_celement (KEY, const INTEGER *indx, CELEMENT);

void pc_frou_get_global_ielement  (KEY, const INTEGER *indx, IELEMENT);
void pc_frou_get_global_felement  (KEY, const INTEGER *indx, FELEMENT);
void pc_frou_get_global_delement  (KEY, const INTEGER *indx, DELEMENT);
void pc_frou_get_global_lelement  (KEY, const INTEGER *indx, LELEMENT);
void pc_frou_get_global_celement  (KEY, const INTEGER *indx, CELEMENT);

void pc_frou_get_control_ielement (KEY, const INTEGER *indx, IELEMENT);
void pc_frou_get_control_felement (KEY, const INTEGER *indx, FELEMENT);
void pc_frou_get_control_delement (KEY, const INTEGER *indx, DELEMENT);
void pc_frou_get_control_lelement (KEY, const INTEGER *indx, LELEMENT);
void pc_frou_get_control_celement (KEY, const INTEGER *indx, CELEMENT);

void pc_frou_get_gui_ielement (KEY,ACT, const INTEGER *indx, IELEMENT);
void pc_frou_get_gui_felement (KEY,ACT, const INTEGER *indx, FELEMENT);
void pc_frou_get_gui_delement (KEY,ACT, const INTEGER *indx, DELEMENT);
void pc_frou_get_gui_lelement (KEY,ACT, const INTEGER *indx, LELEMENT);
void pc_frou_get_gui_celement (KEY,ACT, const INTEGER *indx, CELEMENT);

void pc_frou_get_pdata_ielement   (KEY, const INTEGER *indx, IELEMENT);
void pc_frou_get_pdata_felement   (KEY, const INTEGER *indx, FELEMENT);
void pc_frou_get_pdata_delement   (KEY, const INTEGER *indx, DELEMENT);
void pc_frou_get_pdata_lelement   (KEY, const INTEGER *indx, LELEMENT);
void pc_frou_get_pdata_celement   (KEY, const INTEGER *indx, CELEMENT);

void pc_frou_get_jdata_ielement   (KEY, const INTEGER *indx, IELEMENT);
void pc_frou_get_jdata_felement   (KEY, const INTEGER *indx, FELEMENT);
void pc_frou_get_jdata_delement   (KEY, const INTEGER *indx, DELEMENT);
void pc_frou_get_jdata_lelement   (KEY, const INTEGER *indx, LELEMENT);
void pc_frou_get_jdata_celement   (KEY, const INTEGER *indx, CELEMENT);

                       ///////////////////////////////

int  pc_frou_pressed         (KEY);
void pc_frou_activated       (KEY);

int  pc_frou_verify_scalar   (KEY);
int  pc_frou_verify_element  (KEY, INTEGER *indx, INTEGER *action);
int  pc_frou_verify_array    (KEY);
int  pc_frou_verify_arrayset (KEY);
int  pc_frou_verify_screen   (KEY);
int  pc_frou_verify_end      ();

                       ///////////////////////////////

void pc_frou_put_gscalar          (KEY, const GSCALAR, NCHARDEC);
void pc_frou_put_iscalar          (KEY, const ISCALAR, NCHAR);
void pc_frou_put_fscalar          (KEY, const FSCALAR, NCHARDEC);
void pc_frou_put_dscalar          (KEY, const DSCALAR, NCHARDEC);
void pc_frou_put_lscalar          (KEY, const LSCALAR);
void pc_frou_put_cscalar          (KEY, const CSCALAR);

void pc_frou_put_process_gscalar  (KEY, const GSCALAR, NCHARDEC);
void pc_frou_put_process_iscalar  (KEY, const ISCALAR, NCHAR);
void pc_frou_put_process_fscalar  (KEY, const FSCALAR, NCHARDEC);
void pc_frou_put_process_dscalar  (KEY, const DSCALAR, NCHARDEC);
void pc_frou_put_process_lscalar  (KEY, const LSCALAR);
void pc_frou_put_process_cscalar  (KEY, const CSCALAR);

void pc_frou_put_global_gscalar   (KEY, const GSCALAR, NCHARDEC);
void pc_frou_put_global_iscalar   (KEY, const ISCALAR, NCHAR);
void pc_frou_put_global_fscalar   (KEY, const FSCALAR, NCHARDEC);
void pc_frou_put_global_dscalar   (KEY, const DSCALAR, NCHARDEC);
void pc_frou_put_global_lscalar   (KEY, const LSCALAR);
void pc_frou_put_global_cscalar   (KEY, const CSCALAR);

void pc_frou_put_control_gscalar  (KEY, const GSCALAR, NCHARDEC);
void pc_frou_put_control_iscalar  (KEY, const ISCALAR, NCHAR);
void pc_frou_put_control_fscalar  (KEY, const FSCALAR, NCHARDEC);
void pc_frou_put_control_dscalar  (KEY, const DSCALAR, NCHARDEC);
void pc_frou_put_control_lscalar  (KEY, const LSCALAR);
void pc_frou_put_control_cscalar  (KEY, const CSCALAR);

void pc_frou_put_gui_gscalar  (KEY,ACT, const GSCALAR, NCHARDEC);
void pc_frou_put_gui_iscalar  (KEY,ACT, const ISCALAR, NCHAR);
void pc_frou_put_gui_fscalar  (KEY,ACT, const FSCALAR, NCHARDEC);
void pc_frou_put_gui_dscalar  (KEY,ACT, const DSCALAR, NCHARDEC);
void pc_frou_put_gui_lscalar  (KEY,ACT, const LSCALAR);
void pc_frou_put_gui_cscalar  (KEY,ACT, const CSCALAR);

void pc_frou_put_gui_only_gscalar (KEY, const GSCALAR, NCHARDEC);
void pc_frou_put_gui_only_iscalar (KEY, const ISCALAR, NCHAR);
void pc_frou_put_gui_only_fscalar (KEY, const FSCALAR, NCHARDEC);
void pc_frou_put_gui_only_dscalar (KEY, const DSCALAR, NCHARDEC);
void pc_frou_put_gui_only_lscalar (KEY, const LSCALAR);
void pc_frou_put_gui_only_cscalar (KEY, const CSCALAR);

void pc_frou_put_pdata_gscalar    (KEY, const GSCALAR, NCHARDEC);
void pc_frou_put_pdata_iscalar    (KEY, const ISCALAR, NCHAR);
void pc_frou_put_pdata_fscalar    (KEY, const FSCALAR, NCHARDEC);
void pc_frou_put_pdata_dscalar    (KEY, const DSCALAR, NCHARDEC);
void pc_frou_put_pdata_lscalar    (KEY, const LSCALAR);
void pc_frou_put_pdata_cscalar    (KEY, const CSCALAR);

void pc_frou_put_jdata_gscalar    (KEY, const GSCALAR, NCHARDEC);
void pc_frou_put_jdata_iscalar    (KEY, const ISCALAR, NCHAR);
void pc_frou_put_jdata_fscalar    (KEY, const FSCALAR, NCHARDEC);
void pc_frou_put_jdata_dscalar    (KEY, const DSCALAR, NCHARDEC);
void pc_frou_put_jdata_lscalar    (KEY, const LSCALAR);
void pc_frou_put_jdata_cscalar    (KEY, const CSCALAR);

                       ///////////////////////////////

void pc_frou_put_iarray          (KEY, const IARRAY, const NELEMENTS, NCHAR);
void pc_frou_put_farray          (KEY, const FARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_darray          (KEY, const DARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_larray          (KEY, const LARRAY, const NELEMENTS);
void pc_frou_put_carray          (KEY, const CARRAY, const NELEMENTS, NWORDS);

void pc_frou_put_process_iarray  (KEY, const IARRAY, const NELEMENTS, NCHAR);
void pc_frou_put_process_farray  (KEY, const FARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_process_darray  (KEY, const DARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_process_larray  (KEY, const LARRAY, const NELEMENTS);
void pc_frou_put_process_carray  (KEY, const CARRAY, const NELEMENTS, NWORDS);

void pc_frou_put_global_iarray   (KEY, const IARRAY, const NELEMENTS, NCHAR);
void pc_frou_put_global_farray   (KEY, const FARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_global_darray   (KEY, const DARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_global_larray   (KEY, const LARRAY, const NELEMENTS);
void pc_frou_put_global_carray   (KEY, const CARRAY, const NELEMENTS, NWORDS);

void pc_frou_put_control_iarray  (KEY, const IARRAY, const NELEMENTS, NCHAR);
void pc_frou_put_control_farray  (KEY, const FARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_control_darray  (KEY, const DARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_control_larray  (KEY, const LARRAY, const NELEMENTS);
void pc_frou_put_control_carray  (KEY, const CARRAY, const NELEMENTS, NWORDS);

void pc_frou_put_gui_iarray  (KEY,ACT, const IARRAY, const NELEMENTS, NCHAR);
void pc_frou_put_gui_farray  (KEY,ACT, const FARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_gui_darray  (KEY,ACT, const DARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_gui_larray  (KEY,ACT, const LARRAY, const NELEMENTS);
void pc_frou_put_gui_carray  (KEY,ACT, const CARRAY, const NELEMENTS, NWORDS);

void pc_frou_put_gui_only_iarray (KEY, const IARRAY, const NELEMENTS, NCHAR);
void pc_frou_put_gui_only_farray (KEY, const FARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_gui_only_darray (KEY, const DARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_gui_only_larray (KEY, const LARRAY, const NELEMENTS);
void pc_frou_put_gui_only_carray (KEY, const CARRAY, const NELEMENTS, NWORDS);

void pc_frou_put_pdata_iarray    (KEY, const IARRAY, const NELEMENTS, NCHAR);
void pc_frou_put_pdata_farray    (KEY, const FARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_pdata_darray    (KEY, const DARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_pdata_larray    (KEY, const LARRAY, const NELEMENTS);
void pc_frou_put_pdata_carray    (KEY, const CARRAY, const NELEMENTS, NWORDS);

void pc_frou_put_jdata_iarray    (KEY, const IARRAY, const NELEMENTS, NCHAR);
void pc_frou_put_jdata_farray    (KEY, const FARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_jdata_darray    (KEY, const DARRAY, const NELEMENTS, NCHARDEC);
void pc_frou_put_jdata_larray    (KEY, const LARRAY, const NELEMENTS);
void pc_frou_put_jdata_carray    (KEY, const CARRAY, const NELEMENTS, NWORDS);

                       ///////////////////////////////

void pc_frou_register_array_names  (KEY, const char *arrays,
                                         const INTEGER *narrays, NWORDS);

void pc_frou_put_options_iscalar   (KEY, IOPTIONS, NOPTIONS, NCHAR);
void pc_frou_put_options_fscalar   (KEY, FOPTIONS, NOPTIONS, NCHARDEC);
void pc_frou_put_options_dscalar   (KEY, DOPTIONS, NOPTIONS, NCHARDEC);
void pc_frou_put_options_lscalar   (KEY, LOPTIONS, NOPTIONS);
void pc_frou_put_options_cscalar   (KEY, COPTIONS, NOPTIONS, NWORDS);

void pc_frou_put_options_iarray    (KEY, IOPTIONS, NOPTIONS, NCHAR);
void pc_frou_put_options_farray    (KEY, FOPTIONS, NOPTIONS, NCHARDEC);
void pc_frou_put_options_darray    (KEY, DOPTIONS, NOPTIONS, NCHARDEC);
void pc_frou_put_options_larray    (KEY, LOPTIONS, NOPTIONS);
void pc_frou_put_options_carray    (KEY, COPTIONS, NOPTIONS, NWORDS);

void pc_frou_put_sns_field_flag    (KEY, const INTEGER *sensitive);
void pc_frou_put_sns_array_flag    (KEY, const INTEGER *sensitive);
void pc_frou_put_sns_arrayset_flag (KEY, const INTEGER *sensitive);
void pc_frou_put_sns_screen_flag   (KEY, const INTEGER *sensitive);

void pc_frou_put_visible_flag      (KEY, const INTEGER *visible);

void pc_frou_put_minsize_array     (KEY, const INTEGER *minsize);
void pc_frou_put_minsize_arrayset  (KEY, const INTEGER *minsize);
void pc_frou_put_maxsize_array     (KEY, const INTEGER *maxsize);
void pc_frou_put_maxsize_arrayset  (KEY, const INTEGER *maxsize);

                       ///////////////////////////////

int  pc_frou_num_process_cards ();
int  pc_frou_num_global_cards  ();
int  pc_frou_num_control_cards ();
int  pc_frou_num_pdata_cards ();
int  pc_frou_num_jdata_cards ();
int  pc_frou_num_gui_cards ();

                       ///////////////////////////////

void pc_frou_get_process_cards (NSIZE, CARDS, NCARDS, ERRMSG, NWORDS);
void pc_frou_get_global_cards  (NSIZE, CARDS, NCARDS, ERRMSG, NWORDS);
void pc_frou_get_control_cards (NSIZE, CARDS, NCARDS, ERRMSG, NWORDS);
void pc_frou_get_pdata_cards   (NSIZE, CARDS, NCARDS, ERRMSG, NWORDS);
void pc_frou_get_jdata_cards   (NSIZE, CARDS, NCARDS, ERRMSG, NWORDS);
void pc_frou_get_gui_cards     (NSIZE, CARDS, NCARDS, ERRMSG, NWORDS);

void pc_frou_get_process_card  (const INTEGER *icard, CARD, ERRMSG);
void pc_frou_get_global_card   (const INTEGER *icard, CARD, ERRMSG);
void pc_frou_get_control_card  (const INTEGER *icard, CARD, ERRMSG);
void pc_frou_get_pdata_card    (const INTEGER *icard, CARD, ERRMSG);
void pc_frou_get_jdata_card    (const INTEGER *icard, CARD, ERRMSG);
void pc_frou_get_gui_card      (const INTEGER *icard, CARD, ERRMSG);

void pc_frou_put_process_cards (const CARDS, const NCARDS, NWORDS);
void pc_frou_put_global_cards  (const CARDS, const NCARDS, NWORDS);
void pc_frou_put_control_cards (const CARDS, const NCARDS, NWORDS);
void pc_frou_put_pdata_cards   (const CARDS, const NCARDS, NWORDS);
void pc_frou_put_jdata_cards   (const CARDS, const NCARDS, NWORDS);
void pc_frou_put_gui_cards     (const CARDS, const NCARDS, NWORDS);

void pc_frou_put_process_card  (const CARD);
void pc_frou_put_global_card   (const CARD);
void pc_frou_put_control_card  (const CARD);
void pc_frou_put_pdata_card    (const CARD);
void pc_frou_put_jdata_card    (const CARD);
void pc_frou_put_gui_card      (const CARD);

void pc_frou_add_process_card  (const CARD);
void pc_frou_add_global_card   (const CARD);
void pc_frou_add_control_card  (const CARD);
void pc_frou_add_pdata_card    (const CARD);
void pc_frou_add_jdata_card    (const CARD);
void pc_frou_add_gui_card      (const CARD);

                       ///////////////////////////////

void pc_frou_clear_process_cards     ();
void pc_frou_clear_global_cards      ();
void pc_frou_clear_control_cards     ();
void pc_frou_clear_pdata_cards       ();
void pc_frou_clear_jdata_cards       ();
void pc_frou_clear_gui_cards         ();

int  pc_frou_process_keyword_present (KEY);
int  pc_frou_global_keyword_present  (KEY);
int  pc_frou_control_keyword_present (KEY);
int  pc_frou_pdata_keyword_present   (KEY);
int  pc_frou_jdata_keyword_present   (KEY);
int  pc_frou_gui_action_present      (KEY,ACT);

int  pc_frou_num_process_keywords    ();
int  pc_frou_num_global_keywords     ();
int  pc_frou_num_control_keywords    ();
int  pc_frou_num_pdata_keywords      ();
int  pc_frou_num_jdata_keywords      ();
int  pc_frou_num_gui_keywords        ();

void pc_frou_get_process_keyword     (const INTEGER *indx, char *keyword);
void pc_frou_get_global_keyword      (const INTEGER *indx, char *keyword);
void pc_frou_get_control_keyword     (const INTEGER *indx, char *keyword);
void pc_frou_get_pdata_keyword       (const INTEGER *indx, char *keyword);
void pc_frou_get_jdata_keyword       (const INTEGER *indx, char *keyword);
void pc_frou_get_gui_keyword         (const INTEGER *indx, char *keyword);
void pc_frou_get_gui_action          (const INTEGER *indx, char *action);

void pc_frou_remove_process_keyword  (KEY);
void pc_frou_remove_global_keyword   (KEY);
void pc_frou_remove_control_keyword  (KEY);
void pc_frou_remove_pdata_keyword    (KEY);
void pc_frou_remove_jdata_keyword    (KEY);
void pc_frou_remove_gui_action       (KEY,ACT);

//-------------------------- end of prototypes ---------------------------//
//-------------------------- end of prototypes ---------------------------//
//-------------------------- end of prototypes ---------------------------//

}   // end extern "C"

//--------------------------- undefine macros -----------------------------//
//--------------------------- undefine macros -----------------------------//
//--------------------------- undefine macros -----------------------------//

#undef KEY      
#undef ACT       

#undef ISCALAR    
#undef FSCALAR     
#undef DSCALAR      
#undef CSCALAR       
#undef LSCALAR        

#undef IELEMENT 
#undef FELEMENT  
#undef DELEMENT   
#undef CELEMENT    
#undef LELEMENT     

#undef IARRAY   
#undef FARRAY    
#undef DARRAY     
#undef CARRAY      
#undef LARRAY       

#undef CARD  
#undef CARDS  
#undef ERRMSG  
#undef NELEMENTS
#undef NCARDS 

#undef IOPTIONS
#undef FOPTIONS
#undef DOPTIONS 
#undef COPTIONS  
#undef LOPTIONS   

#undef NCHAR  
#undef NCHARDEC
#undef NWORDS 
#undef NSIZE   
#undef NOPTIONS 

//------------------------ end of undefine macros -------------------------//
//------------------------ end of undefine macros -------------------------//
//------------------------ end of undefine macros -------------------------//

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
