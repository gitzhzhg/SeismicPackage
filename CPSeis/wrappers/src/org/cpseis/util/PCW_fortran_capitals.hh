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
//------------------------- PCW_fortran_capitals.hh ------------------------//
//------------------------- PCW_fortran_capitals.hh ------------------------//
//------------------------- PCW_fortran_capitals.hh ------------------------//

// This header file contains the spelling adjustments
// for Fortran routines when the name of the routine
// must be spelled with capitals.  These are needed by
// the PCW implementation file. They are placed in this
// file instead of the implementation file simply because
// the list is so long.

//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//

#ifndef _PCW_FORTRAN_CAPITALS_HH_
#define _PCW_FORTRAN_CAPITALS_HH_

#include "c2f_interface.h"
#include "named_constants.h"

#if NEED_CAPITALS

//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//

#define pc_frou_exists                   PC_FROU_EXISTS

#define pc_frou_frontend_update          PC_FROU_FRONTEND_UPDATE
#define pc_frou_backend_update           PC_FROU_BACKEND_UPDATE
#define pc_frou_gui_update               PC_FROU_GUI_UPDATE
#define pc_frou_quick_update             PC_FROU_QUICK_UPDATE

#define pc_frou_frontend_update_noprint  PC_FROU_FRONTEND_UPDATE_NOPRINT
#define pc_frou_backend_update_noprint   PC_FROU_BACKEND_UPDATE_NOPRINT
#define pc_frou_gui_update_noprint       PC_FROU_GUI_UPDATE_NOPRINT
#define pc_frou_quick_update_noprint     PC_FROU_QUICK_UPDATE_NOPRINT

#define pc_frou_clear                    PC_FROU_CLEAR
#define pc_frou_restore                  PC_FROU_RESTORE
#define pc_frou_next                     PC_FROU_NEXT
#define pc_frou_backend_execute          PC_FROU_BACKEND_EXECUTE
#define pc_frou_continue_backend_update  PC_FROU_CONTINUE_BACKEND_UPDATE

#define pc_frou_get_update_state         PC_FROU_GET_UPDATE_STATE
#define pc_frou_set_backend_no_exec      PC_FROU_SET_BACKEND_NO_EXEC
#define pc_frou_set_backend_yes_exec     PC_FROU_SET_BACKEND_YES_EXEC
#define pc_frou_get_ipn                  PC_FROU_GET_IPN
#define pc_frou_previous_error           PC_FROU_PREVIOUS_ERROR
#define pc_frou_set_ipn                  PC_FROU_SET_IPN

#define pc_frou_do_not_process_traces    PC_FROU_DO_NOT_PROCESS_TRACES

#define pc_frou_update_error             PC_FROU_UPDATE_ERROR
#define pc_frou_error                    PC_FROU_ERROR
#define pc_frou_warning                  PC_FROU_WARNING
#define pc_frou_info                     PC_FROU_INFO
#define pc_frou_print                    PC_FROU_PRINT

                       ///////////////////////////////

#define pc_frou_print_process_cards      PC_FROU_PRINT_PROCESS_CARDS
#define pc_frou_print_global_cards       PC_FROU_PRINT_GLOBAL_CARDS
#define pc_frou_print_control_cards      PC_FROU_PRINT_CONTROL_CARDS
#define pc_frou_print_pdata_cards        PC_FROU_PRINT_PDATA_CARDS
#define pc_frou_print_jdata_cards        PC_FROU_PRINT_JDATA_CARDS
#define pc_frou_print_gui_cards          PC_FROU_PRINT_GUI_CARDS

#define pc_frou_info_process_cards       PC_FROU_INFO_PROCESS_CARDS
#define pc_frou_info_global_cards        PC_FROU_INFO_GLOBAL_CARDS
#define pc_frou_info_control_cards       PC_FROU_INFO_CONTROL_CARDS
#define pc_frou_info_pdata_cards         PC_FROU_INFO_PDATA_CARDS
#define pc_frou_info_jdata_cards         PC_FROU_INFO_JDATA_CARDS
#define pc_frou_info_gui_cards           PC_FROU_INFO_GUI_CARDS

                       ///////////////////////////////

#define pc_frou_num_elements_process     PC_FROU_NUM_ELEMENTS_PROCESS
#define pc_frou_num_elements_global      PC_FROU_NUM_ELEMENTS_GLOBAL
#define pc_frou_num_elements_control     PC_FROU_NUM_ELEMENTS_CONTROL
#define pc_frou_num_elements_gui         PC_FROU_NUM_ELEMENTS_GUI
#define pc_frou_num_elements_pdata       PC_FROU_NUM_ELEMENTS_PDATA
#define pc_frou_num_elements_jdata       PC_FROU_NUM_ELEMENTS_JDATA

#define pc_frou_nature_process           PC_FROU_NATURE_PROCESS
#define pc_frou_nature_global            PC_FROU_NATURE_GLOBAL
#define pc_frou_nature_control           PC_FROU_NATURE_CONTROL
#define pc_frou_nature_gui               PC_FROU_NATURE_GUI
#define pc_frou_nature_pdata             PC_FROU_NATURE_PDATA
#define pc_frou_nature_jdata             PC_FROU_NATURE_JDATA

#define pc_frou_vartype_process          PC_FROU_VARTYPE_PROCESS
#define pc_frou_vartype_global           PC_FROU_VARTYPE_GLOBAL
#define pc_frou_vartype_control          PC_FROU_VARTYPE_CONTROL
#define pc_frou_vartype_gui              PC_FROU_VARTYPE_GUI
#define pc_frou_vartype_pdata            PC_FROU_VARTYPE_PDATA
#define pc_frou_vartype_jdata            PC_FROU_VARTYPE_JDATA

                       ///////////////////////////////

#define pc_frou_get_gscalar              PC_FROU_GET_GSCALAR
#define pc_frou_get_iscalar              PC_FROU_GET_ISCALAR
#define pc_frou_get_fscalar              PC_FROU_GET_FSCALAR
#define pc_frou_get_dscalar              PC_FROU_GET_DSCALAR
#define pc_frou_get_lscalar              PC_FROU_GET_LSCALAR
#define pc_frou_get_cscalar              PC_FROU_GET_CSCALAR

#define pc_frou_get_process_gscalar      PC_FROU_GET_PROCESS_GSCALAR
#define pc_frou_get_process_iscalar      PC_FROU_GET_PROCESS_ISCALAR
#define pc_frou_get_process_fscalar      PC_FROU_GET_PROCESS_FSCALAR
#define pc_frou_get_process_dscalar      PC_FROU_GET_PROCESS_DSCALAR
#define pc_frou_get_process_lscalar      PC_FROU_GET_PROCESS_LSCALAR
#define pc_frou_get_process_cscalar      PC_FROU_GET_PROCESS_CSCALAR

#define pc_frou_get_global_gscalar       PC_FROU_GET_GLOBAL_GSCALAR
#define pc_frou_get_global_iscalar       PC_FROU_GET_GLOBAL_ISCALAR
#define pc_frou_get_global_fscalar       PC_FROU_GET_GLOBAL_FSCALAR
#define pc_frou_get_global_dscalar       PC_FROU_GET_GLOBAL_DSCALAR
#define pc_frou_get_global_lscalar       PC_FROU_GET_GLOBAL_LSCALAR
#define pc_frou_get_global_cscalar       PC_FROU_GET_GLOBAL_CSCALAR

#define pc_frou_get_control_gscalar      PC_FROU_GET_CONTROL_GSCALAR
#define pc_frou_get_control_iscalar      PC_FROU_GET_CONTROL_ISCALAR
#define pc_frou_get_control_fscalar      PC_FROU_GET_CONTROL_FSCALAR
#define pc_frou_get_control_dscalar      PC_FROU_GET_CONTROL_DSCALAR
#define pc_frou_get_control_lscalar      PC_FROU_GET_CONTROL_LSCALAR
#define pc_frou_get_control_cscalar      PC_FROU_GET_CONTROL_CSCALAR

#define pc_frou_get_gui_gscalar          PC_FROU_GET_GUI_GSCALAR
#define pc_frou_get_gui_iscalar          PC_FROU_GET_GUI_ISCALAR
#define pc_frou_get_gui_fscalar          PC_FROU_GET_GUI_FSCALAR
#define pc_frou_get_gui_dscalar          PC_FROU_GET_GUI_DSCALAR
#define pc_frou_get_gui_lscalar          PC_FROU_GET_GUI_LSCALAR
#define pc_frou_get_gui_cscalar          PC_FROU_GET_GUI_CSCALAR

#define pc_frou_get_pdata_gscalar        PC_FROU_GET_PDATA_GSCALAR
#define pc_frou_get_pdata_iscalar        PC_FROU_GET_PDATA_ISCALAR
#define pc_frou_get_pdata_fscalar        PC_FROU_GET_PDATA_FSCALAR
#define pc_frou_get_pdata_dscalar        PC_FROU_GET_PDATA_DSCALAR
#define pc_frou_get_pdata_lscalar        PC_FROU_GET_PDATA_LSCALAR
#define pc_frou_get_pdata_cscalar        PC_FROU_GET_PDATA_CSCALAR

#define pc_frou_get_jdata_gscalar        PC_FROU_GET_JDATA_GSCALAR
#define pc_frou_get_jdata_iscalar        PC_FROU_GET_JDATA_ISCALAR
#define pc_frou_get_jdata_fscalar        PC_FROU_GET_JDATA_FSCALAR
#define pc_frou_get_jdata_dscalar        PC_FROU_GET_JDATA_DSCALAR
#define pc_frou_get_jdata_lscalar        PC_FROU_GET_JDATA_LSCALAR
#define pc_frou_get_jdata_cscalar        PC_FROU_GET_JDATA_CSCALAR

                       ///////////////////////////////

#define pc_frou_get_iarray               PC_FROU_GET_IARRAY
#define pc_frou_get_farray               PC_FROU_GET_FARRAY
#define pc_frou_get_darray               PC_FROU_GET_DARRAY
#define pc_frou_get_larray               PC_FROU_GET_LARRAY
#define pc_frou_get_carray               PC_FROU_GET_CARRAY

#define pc_frou_get_process_iarray       PC_FROU_GET_PROCESS_IARRAY
#define pc_frou_get_process_farray       PC_FROU_GET_PROCESS_FARRAY
#define pc_frou_get_process_darray       PC_FROU_GET_PROCESS_DARRAY
#define pc_frou_get_process_larray       PC_FROU_GET_PROCESS_LARRAY
#define pc_frou_get_process_carray       PC_FROU_GET_PROCESS_CARRAY

#define pc_frou_get_global_iarray        PC_FROU_GET_GLOBAL_IARRAY
#define pc_frou_get_global_farray        PC_FROU_GET_GLOBAL_FARRAY
#define pc_frou_get_global_darray        PC_FROU_GET_GLOBAL_DARRAY
#define pc_frou_get_global_larray        PC_FROU_GET_GLOBAL_LARRAY
#define pc_frou_get_global_carray        PC_FROU_GET_GLOBAL_CARRAY

#define pc_frou_get_control_iarray       PC_FROU_GET_CONTROL_IARRAY
#define pc_frou_get_control_farray       PC_FROU_GET_CONTROL_FARRAY
#define pc_frou_get_control_darray       PC_FROU_GET_CONTROL_DARRAY
#define pc_frou_get_control_larray       PC_FROU_GET_CONTROL_LARRAY
#define pc_frou_get_control_carray       PC_FROU_GET_CONTROL_CARRAY

#define pc_frou_get_gui_iarray           PC_FROU_GET_GUI_IARRAY
#define pc_frou_get_gui_farray           PC_FROU_GET_GUI_FARRAY
#define pc_frou_get_gui_darray           PC_FROU_GET_GUI_DARRAY
#define pc_frou_get_gui_larray           PC_FROU_GET_GUI_LARRAY
#define pc_frou_get_gui_carray           PC_FROU_GET_GUI_CARRAY

#define pc_frou_get_pdata_iarray         PC_FROU_GET_PDATA_IARRAY
#define pc_frou_get_pdata_farray         PC_FROU_GET_PDATA_FARRAY
#define pc_frou_get_pdata_darray         PC_FROU_GET_PDATA_DARRAY
#define pc_frou_get_pdata_larray         PC_FROU_GET_PDATA_LARRAY
#define pc_frou_get_pdata_carray         PC_FROU_GET_PDATA_CARRAY

#define pc_frou_get_jdata_iarray         PC_FROU_GET_JDATA_IARRAY
#define pc_frou_get_jdata_farray         PC_FROU_GET_JDATA_FARRAY
#define pc_frou_get_jdata_darray         PC_FROU_GET_JDATA_DARRAY
#define pc_frou_get_jdata_larray         PC_FROU_GET_JDATA_LARRAY
#define pc_frou_get_jdata_carray         PC_FROU_GET_JDATA_CARRAY

                       ///////////////////////////////

#define pc_frou_get_process_ielement     PC_FROU_GET_PROCESS_IELEMENT
#define pc_frou_get_process_felement     PC_FROU_GET_PROCESS_FELEMENT
#define pc_frou_get_process_delement     PC_FROU_GET_PROCESS_DELEMENT
#define pc_frou_get_process_lelement     PC_FROU_GET_PROCESS_LELEMENT
#define pc_frou_get_process_celement     PC_FROU_GET_PROCESS_CELEMENT

#define pc_frou_get_global_ielement      PC_FROU_GET_GLOBAL_IELEMENT
#define pc_frou_get_global_felement      PC_FROU_GET_GLOBAL_FELEMENT
#define pc_frou_get_global_delement      PC_FROU_GET_GLOBAL_DELEMENT
#define pc_frou_get_global_lelement      PC_FROU_GET_GLOBAL_LELEMENT
#define pc_frou_get_global_celement      PC_FROU_GET_GLOBAL_CELEMENT

#define pc_frou_get_control_ielement     PC_FROU_GET_CONTROL_IELEMENT
#define pc_frou_get_control_felement     PC_FROU_GET_CONTROL_FELEMENT
#define pc_frou_get_control_delement     PC_FROU_GET_CONTROL_DELEMENT
#define pc_frou_get_control_lelement     PC_FROU_GET_CONTROL_LELEMENT
#define pc_frou_get_control_celement     PC_FROU_GET_CONTROL_CELEMENT

#define pc_frou_get_gui_ielement         PC_FROU_GET_GUI_IELEMENT
#define pc_frou_get_gui_felement         PC_FROU_GET_GUI_FELEMENT
#define pc_frou_get_gui_delement         PC_FROU_GET_GUI_DELEMENT
#define pc_frou_get_gui_lelement         PC_FROU_GET_GUI_LELEMENT
#define pc_frou_get_gui_celement         PC_FROU_GET_GUI_CELEMENT

#define pc_frou_get_pdata_ielement       PC_FROU_GET_PDATA_IELEMENT
#define pc_frou_get_pdata_felement       PC_FROU_GET_PDATA_FELEMENT
#define pc_frou_get_pdata_delement       PC_FROU_GET_PDATA_DELEMENT
#define pc_frou_get_pdata_lelement       PC_FROU_GET_PDATA_LELEMENT
#define pc_frou_get_pdata_celement       PC_FROU_GET_PDATA_CELEMENT

#define pc_frou_get_jdata_ielement       PC_FROU_GET_JDATA_IELEMENT
#define pc_frou_get_jdata_felement       PC_FROU_GET_JDATA_FELEMENT
#define pc_frou_get_jdata_delement       PC_FROU_GET_JDATA_DELEMENT
#define pc_frou_get_jdata_lelement       PC_FROU_GET_JDATA_LELEMENT
#define pc_frou_get_jdata_celement       PC_FROU_GET_JDATA_CELEMENT

                       ///////////////////////////////

#define pc_frou_pressed                  PC_FROU_PRESSED
#define pc_frou_activated                PC_FROU_ACTIVATED

#define pc_frou_verify_scalar            PC_FROU_VERIFY_SCALAR
#define pc_frou_verify_element           PC_FROU_VERIFY_ELEMENT
#define pc_frou_verify_array             PC_FROU_VERIFY_ARRAY
#define pc_frou_verify_arrayset          PC_FROU_VERIFY_ARRAYSET
#define pc_frou_verify_screen            PC_FROU_VERIFY_SCREEN
#define pc_frou_verify_end               PC_FROU_VERIFY_END

                       ///////////////////////////////

#define pc_frou_put_gscalar              PC_FROU_PUT_GSCALAR
#define pc_frou_put_iscalar              PC_FROU_PUT_ISCALAR
#define pc_frou_put_fscalar              PC_FROU_PUT_FSCALAR
#define pc_frou_put_dscalar              PC_FROU_PUT_DSCALAR
#define pc_frou_put_lscalar              PC_FROU_PUT_LSCALAR
#define pc_frou_put_cscalar              PC_FROU_PUT_CSCALAR

#define pc_frou_put_process_gscalar      PC_FROU_PUT_PROCESS_GSCALAR
#define pc_frou_put_process_iscalar      PC_FROU_PUT_PROCESS_ISCALAR
#define pc_frou_put_process_fscalar      PC_FROU_PUT_PROCESS_FSCALAR
#define pc_frou_put_process_dscalar      PC_FROU_PUT_PROCESS_DSCALAR
#define pc_frou_put_process_lscalar      PC_FROU_PUT_PROCESS_LSCALAR
#define pc_frou_put_process_cscalar      PC_FROU_PUT_PROCESS_CSCALAR

#define pc_frou_put_global_gscalar       PC_FROU_PUT_GLOBAL_GSCALAR
#define pc_frou_put_global_iscalar       PC_FROU_PUT_GLOBAL_ISCALAR
#define pc_frou_put_global_fscalar       PC_FROU_PUT_GLOBAL_FSCALAR
#define pc_frou_put_global_dscalar       PC_FROU_PUT_GLOBAL_DSCALAR
#define pc_frou_put_global_lscalar       PC_FROU_PUT_GLOBAL_LSCALAR
#define pc_frou_put_global_cscalar       PC_FROU_PUT_GLOBAL_CSCALAR

#define pc_frou_put_control_gscalar      PC_FROU_PUT_CONTROL_GSCALAR
#define pc_frou_put_control_iscalar      PC_FROU_PUT_CONTROL_ISCALAR
#define pc_frou_put_control_fscalar      PC_FROU_PUT_CONTROL_FSCALAR
#define pc_frou_put_control_dscalar      PC_FROU_PUT_CONTROL_DSCALAR
#define pc_frou_put_control_lscalar      PC_FROU_PUT_CONTROL_LSCALAR
#define pc_frou_put_control_cscalar      PC_FROU_PUT_CONTROL_CSCALAR

#define pc_frou_put_gui_gscalar          PC_FROU_PUT_GUI_GSCALAR
#define pc_frou_put_gui_iscalar          PC_FROU_PUT_GUI_ISCALAR
#define pc_frou_put_gui_fscalar          PC_FROU_PUT_GUI_FSCALAR
#define pc_frou_put_gui_dscalar          PC_FROU_PUT_GUI_DSCALAR
#define pc_frou_put_gui_lscalar          PC_FROU_PUT_GUI_LSCALAR
#define pc_frou_put_gui_cscalar          PC_FROU_PUT_GUI_CSCALAR

#define pc_frou_put_gui_only_gscalar     PC_FROU_PUT_GUI_ONLY_GSCALAR
#define pc_frou_put_gui_only_iscalar     PC_FROU_PUT_GUI_ONLY_ISCALAR
#define pc_frou_put_gui_only_fscalar     PC_FROU_PUT_GUI_ONLY_FSCALAR
#define pc_frou_put_gui_only_dscalar     PC_FROU_PUT_GUI_ONLY_DSCALAR
#define pc_frou_put_gui_only_lscalar     PC_FROU_PUT_GUI_ONLY_LSCALAR
#define pc_frou_put_gui_only_cscalar     PC_FROU_PUT_GUI_ONLY_CSCALAR

#define pc_frou_put_pdata_gscalar        PC_FROU_PUT_PDATA_GSCALAR
#define pc_frou_put_pdata_iscalar        PC_FROU_PUT_PDATA_ISCALAR
#define pc_frou_put_pdata_fscalar        PC_FROU_PUT_PDATA_FSCALAR
#define pc_frou_put_pdata_dscalar        PC_FROU_PUT_PDATA_DSCALAR
#define pc_frou_put_pdata_lscalar        PC_FROU_PUT_PDATA_LSCALAR
#define pc_frou_put_pdata_cscalar        PC_FROU_PUT_PDATA_CSCALAR

#define pc_frou_put_jdata_gscalar        PC_FROU_PUT_JDATA_GSCALAR
#define pc_frou_put_jdata_iscalar        PC_FROU_PUT_JDATA_ISCALAR
#define pc_frou_put_jdata_fscalar        PC_FROU_PUT_JDATA_FSCALAR
#define pc_frou_put_jdata_dscalar        PC_FROU_PUT_JDATA_DSCALAR
#define pc_frou_put_jdata_lscalar        PC_FROU_PUT_JDATA_LSCALAR
#define pc_frou_put_jdata_cscalar        PC_FROU_PUT_JDATA_CSCALAR

                       ///////////////////////////////

#define pc_frou_put_iarray               PC_FROU_PUT_IARRAY
#define pc_frou_put_farray               PC_FROU_PUT_FARRAY
#define pc_frou_put_darray               PC_FROU_PUT_DARRAY
#define pc_frou_put_larray               PC_FROU_PUT_LARRAY
#define pc_frou_put_carray               PC_FROU_PUT_CARRAY

#define pc_frou_put_process_iarray       PC_FROU_PUT_PROCESS_IARRAY
#define pc_frou_put_process_farray       PC_FROU_PUT_PROCESS_FARRAY
#define pc_frou_put_process_darray       PC_FROU_PUT_PROCESS_DARRAY
#define pc_frou_put_process_larray       PC_FROU_PUT_PROCESS_LARRAY
#define pc_frou_put_process_carray       PC_FROU_PUT_PROCESS_CARRAY

#define pc_frou_put_global_iarray        PC_FROU_PUT_GLOBAL_IARRAY
#define pc_frou_put_global_farray        PC_FROU_PUT_GLOBAL_FARRAY
#define pc_frou_put_global_darray        PC_FROU_PUT_GLOBAL_DARRAY
#define pc_frou_put_global_larray        PC_FROU_PUT_GLOBAL_LARRAY
#define pc_frou_put_global_carray        PC_FROU_PUT_GLOBAL_CARRAY

#define pc_frou_put_control_iarray       PC_FROU_PUT_CONTROL_IARRAY
#define pc_frou_put_control_farray       PC_FROU_PUT_CONTROL_FARRAY
#define pc_frou_put_control_darray       PC_FROU_PUT_CONTROL_DARRAY
#define pc_frou_put_control_larray       PC_FROU_PUT_CONTROL_LARRAY
#define pc_frou_put_control_carray       PC_FROU_PUT_CONTROL_CARRAY

#define pc_frou_put_gui_iarray           PC_FROU_PUT_GUI_IARRAY
#define pc_frou_put_gui_farray           PC_FROU_PUT_GUI_FARRAY
#define pc_frou_put_gui_darray           PC_FROU_PUT_GUI_DARRAY
#define pc_frou_put_gui_larray           PC_FROU_PUT_GUI_LARRAY
#define pc_frou_put_gui_carray           PC_FROU_PUT_GUI_CARRAY

#define pc_frou_put_gui_only_iarray      PC_FROU_PUT_GUI_ONLY_IARRAY
#define pc_frou_put_gui_only_farray      PC_FROU_PUT_GUI_ONLY_FARRAY
#define pc_frou_put_gui_only_darray      PC_FROU_PUT_GUI_ONLY_DARRAY
#define pc_frou_put_gui_only_larray      PC_FROU_PUT_GUI_ONLY_LARRAY
#define pc_frou_put_gui_only_carray      PC_FROU_PUT_GUI_ONLY_CARRAY

#define pc_frou_put_pdata_iarray         PC_FROU_PUT_PDATA_IARRAY
#define pc_frou_put_pdata_farray         PC_FROU_PUT_PDATA_FARRAY
#define pc_frou_put_pdata_darray         PC_FROU_PUT_PDATA_DARRAY
#define pc_frou_put_pdata_larray         PC_FROU_PUT_PDATA_LARRAY
#define pc_frou_put_pdata_carray         PC_FROU_PUT_PDATA_CARRAY

#define pc_frou_put_jdata_iarray         PC_FROU_PUT_JDATA_IARRAY
#define pc_frou_put_jdata_farray         PC_FROU_PUT_JDATA_FARRAY
#define pc_frou_put_jdata_darray         PC_FROU_PUT_JDATA_DARRAY
#define pc_frou_put_jdata_larray         PC_FROU_PUT_JDATA_LARRAY
#define pc_frou_put_jdata_carray         PC_FROU_PUT_JDATA_CARRAY

                       ///////////////////////////////

#define pc_frou_register_array_names     PC_FROU_REGISTER_ARRAY_NAMES

#define pc_frou_put_options_iscalar      PC_FROU_PUT_OPTIONS_ISCALAR
#define pc_frou_put_options_fscalar      PC_FROU_PUT_OPTIONS_FSCALAR
#define pc_frou_put_options_dscalar      PC_FROU_PUT_OPTIONS_DSCALAR
#define pc_frou_put_options_lscalar      PC_FROU_PUT_OPTIONS_LSCALAR
#define pc_frou_put_options_cscalar      PC_FROU_PUT_OPTIONS_CSCALAR

#define pc_frou_put_options_iarray       PC_FROU_PUT_OPTIONS_IARRAY
#define pc_frou_put_options_farray       PC_FROU_PUT_OPTIONS_FARRAY
#define pc_frou_put_options_darray       PC_FROU_PUT_OPTIONS_DARRAY
#define pc_frou_put_options_larray       PC_FROU_PUT_OPTIONS_LARRAY
#define pc_frou_put_options_carray       PC_FROU_PUT_OPTIONS_CARRAY

#define pc_frou_put_sns_field_flag       PC_FROU_PUT_SNS_FIELD_FLAG
#define pc_frou_put_sns_array_flag       PC_FROU_PUT_SNS_ARRAY_FLAG
#define pc_frou_put_sns_arrayset_flag    PC_FROU_PUT_SNS_ARRAYSET_FLAG
#define pc_frou_put_sns_screen_flag      PC_FROU_PUT_SNS_SCREEN_FLAG

#define pc_frou_put_visible_flag         PC_FROU_PUT_VISIBLE_FLAG

#define pc_frou_put_minsize_array        PC_FROU_PUT_MINSIZE_ARRAY
#define pc_frou_put_minsize_arrayset     PC_FROU_PUT_MINSIZE_ARRAYSET
#define pc_frou_put_maxsize_array        PC_FROU_PUT_MAXSIZE_ARRAY
#define pc_frou_put_maxsize_arrayset     PC_FROU_PUT_MAXSIZE_ARRAYSET

                       ///////////////////////////////

#define pc_frou_num_process_cards        PC_FROU_NUM_PROCESS_CARDS
#define pc_frou_num_global_cards         PC_FROU_NUM_GLOBAL_CARDS
#define pc_frou_num_control_cards        PC_FROU_NUM_CONTROL_CARDS
#define pc_frou_num_pdata_cards          PC_FROU_NUM_PDATA_CARDS
#define pc_frou_num_jdata_cards          PC_FROU_NUM_JDATA_CARDS
#define pc_frou_num_gui_cards            PC_FROU_NUM_GUI_CARDS

                       ///////////////////////////////

#define pc_frou_get_process_cards        PC_FROU_GET_PROCESS_CARDS
#define pc_frou_get_global_cards         PC_FROU_GET_GLOBAL_CARDS
#define pc_frou_get_control_cards        PC_FROU_GET_CONTROL_CARDS
#define pc_frou_get_pdata_cards          PC_FROU_GET_PDATA_CARDS
#define pc_frou_get_jdata_cards          PC_FROU_GET_JDATA_CARDS
#define pc_frou_get_gui_cards            PC_FROU_GET_GUI_CARDS

#define pc_frou_get_process_card         PC_FROU_GET_PROCESS_CARD
#define pc_frou_get_global_card          PC_FROU_GET_GLOBAL_CARD
#define pc_frou_get_control_card         PC_FROU_GET_CONTROL_CARD
#define pc_frou_get_pdata_card           PC_FROU_GET_PDATA_CARD
#define pc_frou_get_jdata_card           PC_FROU_GET_JDATA_CARD
#define pc_frou_get_gui_card             PC_FROU_GET_GUI_CARD

#define pc_frou_put_process_cards        PC_FROU_PUT_PROCESS_CARDS
#define pc_frou_put_global_cards         PC_FROU_PUT_GLOBAL_CARDS
#define pc_frou_put_control_cards        PC_FROU_PUT_CONTROL_CARDS
#define pc_frou_put_pdata_cards          PC_FROU_PUT_PDATA_CARDS
#define pc_frou_put_jdata_cards          PC_FROU_PUT_JDATA_CARDS
#define pc_frou_put_gui_cards            PC_FROU_PUT_GUI_CARDS

#define pc_frou_put_process_card         PC_FROU_PUT_PROCESS_CARD
#define pc_frou_put_global_card          PC_FROU_PUT_GLOBAL_CARD
#define pc_frou_put_control_card         PC_FROU_PUT_CONTROL_CARD
#define pc_frou_put_pdata_card           PC_FROU_PUT_PDATA_CARD
#define pc_frou_put_jdata_card           PC_FROU_PUT_JDATA_CARD
#define pc_frou_put_gui_card             PC_FROU_PUT_GUI_CARD

#define pc_frou_add_process_card         PC_FROU_ADD_PROCESS_CARD
#define pc_frou_add_global_card          PC_FROU_ADD_GLOBAL_CARD
#define pc_frou_add_control_card         PC_FROU_ADD_CONTROL_CARD
#define pc_frou_add_pdata_card           PC_FROU_ADD_PDATA_CARD
#define pc_frou_add_jdata_card           PC_FROU_ADD_JDATA_CARD
#define pc_frou_add_gui_card             PC_FROU_ADD_GUI_CARD

                       ///////////////////////////////

#define pc_frou_clear_process_cards      PC_FROU_CLEAR_PROCESS_CARDS
#define pc_frou_clear_global_cards       PC_FROU_CLEAR_GLOBAL_CARDS
#define pc_frou_clear_control_cards      PC_FROU_CLEAR_CONTROL_CARDS
#define pc_frou_clear_pdata_cards        PC_FROU_CLEAR_PDATA_CARDS
#define pc_frou_clear_jdata_cards        PC_FROU_CLEAR_JDATA_CARDS
#define pc_frou_clear_gui_cards          PC_FROU_CLEAR_GUI_CARDS

#define pc_frou_process_keyword_present  PC_FROU_PROCESS_KEYWORD_PRESENT
#define pc_frou_global_keyword_present   PC_FROU_GLOBAL_KEYWORD_PRESENT
#define pc_frou_control_keyword_present  PC_FROU_CONTROL_KEYWORD_PRESENT
#define pc_frou_pdata_keyword_present    PC_FROU_PDATA_KEYWORD_PRESENT
#define pc_frou_jdata_keyword_present    PC_FROU_JDATA_KEYWORD_PRESENT
#define pc_frou_gui_action_present       PC_FROU_GUI_ACTION_PRESENT

#define pc_frou_num_process_keywords     PC_FROU_NUM_PROCESS_KEYWORDS
#define pc_frou_num_global_keywords      PC_FROU_NUM_GLOBAL_KEYWORDS
#define pc_frou_num_control_keywords     PC_FROU_NUM_CONTROL_KEYWORDS
#define pc_frou_num_pdata_keywords       PC_FROU_NUM_PDATA_KEYWORDS
#define pc_frou_num_jdata_keywords       PC_FROU_NUM_JDATA_KEYWORDS
#define pc_frou_num_gui_keywords         PC_FROU_NUM_GUI_KEYWORDS

#define pc_frou_get_process_keyword      PC_FROU_GET_PROCESS_KEYWORD
#define pc_frou_get_global_keyword       PC_FROU_GET_GLOBAL_KEYWORD
#define pc_frou_get_control_keyword      PC_FROU_GET_CONTROL_KEYWORD
#define pc_frou_get_pdata_keyword        PC_FROU_GET_PDATA_KEYWORD
#define pc_frou_get_jdata_keyword        PC_FROU_GET_JDATA_KEYWORD
#define pc_frou_get_gui_keyword          PC_FROU_GET_GUI_KEYWORD
#define pc_frou_get_gui_action           PC_FROU_GET_GUI_ACTION

#define pc_frou_remove_process_keyword   PC_FROU_REMOVE_PROCESS_KEYWORD
#define pc_frou_remove_global_keyword    PC_FROU_REMOVE_GLOBAL_KEYWORD
#define pc_frou_remove_control_keyword   PC_FROU_REMOVE_CONTROL_KEYWORD
#define pc_frou_remove_pdata_keyword     PC_FROU_REMOVE_PDATA_KEYWORD
#define pc_frou_remove_jdata_keyword     PC_FROU_REMOVE_JDATA_KEYWORD
#define pc_frou_remove_gui_action        PC_FROU_REMOVE_GUI_ACTION

//---------------------- end of spelling adjustments ---------------------//
//---------------------- end of spelling adjustments ---------------------//
//---------------------- end of spelling adjustments ---------------------//

#endif
#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
