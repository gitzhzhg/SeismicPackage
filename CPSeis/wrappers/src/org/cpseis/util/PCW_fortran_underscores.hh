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
//---------------------- PCW_fortran_underscores.hh ------------------------//
//---------------------- PCW_fortran_underscores.hh ------------------------//
//---------------------- PCW_fortran_underscores.hh ------------------------//

// This header file contains the spelling adjustments
// for Fortran routines when the name of the routine
// must end with an underscore.  These are needed by
// the PCW implementation file. They are placed in this
// file instead of the implementation file simply because
// the list is so long.

//----------------------- start of coding --------------------------------//
//----------------------- start of coding --------------------------------//
//----------------------- start of coding --------------------------------//

#ifndef _PCW_FORTRAN_UNDERSCORES_HH_
#define _PCW_FORTRAN_UNDERSCORES_HH_

#include "c2f_interface.h"
#include "named_constants.h"

#if NEED_UNDERSCORE

//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//

#define pc_frou_exists                   pc_frou_exists_

#define pc_frou_frontend_update          pc_frou_frontend_update_
#define pc_frou_backend_update           pc_frou_backend_update_
#define pc_frou_gui_update               pc_frou_gui_update_
#define pc_frou_quick_update             pc_frou_quick_update_

#define pc_frou_frontend_update_noprint  pc_frou_frontend_update_noprint_
#define pc_frou_backend_update_noprint   pc_frou_backend_update_noprint_
#define pc_frou_gui_update_noprint       pc_frou_gui_update_noprint_
#define pc_frou_quick_update_noprint     pc_frou_quick_update_noprint_

#define pc_frou_clear                    pc_frou_clear_
#define pc_frou_restore                  pc_frou_restore_
#define pc_frou_next                     pc_frou_next_
#define pc_frou_backend_execute          pc_frou_backend_execute_
#define pc_frou_continue_backend_update  pc_frou_continue_backend_update_

#define pc_frou_get_update_state         pc_frou_get_update_state_
#define pc_frou_set_backend_no_exec      pc_frou_set_backend_no_exec_
#define pc_frou_set_backend_yes_exec     pc_frou_set_backend_yes_exec_
#define pc_frou_get_ipn                  pc_frou_get_ipn_
#define pc_frou_previous_error           pc_frou_previous_error_
#define pc_frou_set_ipn                  pc_frou_set_ipn_

#define pc_frou_do_not_process_traces    pc_frou_do_not_process_traces_

#define pc_frou_update_error             pc_frou_update_error_
#define pc_frou_error                    pc_frou_error_
#define pc_frou_warning                  pc_frou_warning_
#define pc_frou_info                     pc_frou_info_
#define pc_frou_print                    pc_frou_print_

                       ///////////////////////////////

#define pc_frou_print_process_cards      pc_frou_print_process_cards_
#define pc_frou_print_global_cards       pc_frou_print_global_cards_
#define pc_frou_print_control_cards      pc_frou_print_control_cards_
#define pc_frou_print_pdata_cards        pc_frou_print_pdata_cards_
#define pc_frou_print_jdata_cards        pc_frou_print_jdata_cards_
#define pc_frou_print_gui_cards          pc_frou_print_gui_cards_

#define pc_frou_info_process_cards       pc_frou_info_process_cards_
#define pc_frou_info_global_cards        pc_frou_info_global_cards_
#define pc_frou_info_control_cards       pc_frou_info_control_cards_
#define pc_frou_info_pdata_cards         pc_frou_info_pdata_cards_
#define pc_frou_info_jdata_cards         pc_frou_info_jdata_cards_
#define pc_frou_info_gui_cards           pc_frou_info_gui_cards_

                       ///////////////////////////////

#define pc_frou_num_elements_process     pc_frou_num_elements_process_
#define pc_frou_num_elements_global      pc_frou_num_elements_global_
#define pc_frou_num_elements_control     pc_frou_num_elements_control_
#define pc_frou_num_elements_gui         pc_frou_num_elements_gui_
#define pc_frou_num_elements_pdata       pc_frou_num_elements_pdata_
#define pc_frou_num_elements_jdata       pc_frou_num_elements_jdata_

#define pc_frou_nature_process           pc_frou_nature_process_
#define pc_frou_nature_global            pc_frou_nature_global_
#define pc_frou_nature_control           pc_frou_nature_control_
#define pc_frou_nature_gui               pc_frou_nature_gui_
#define pc_frou_nature_pdata             pc_frou_nature_pdata_
#define pc_frou_nature_jdata             pc_frou_nature_jdata_

#define pc_frou_vartype_process          pc_frou_vartype_process_
#define pc_frou_vartype_global           pc_frou_vartype_global_
#define pc_frou_vartype_control          pc_frou_vartype_control_
#define pc_frou_vartype_gui              pc_frou_vartype_gui_
#define pc_frou_vartype_pdata            pc_frou_vartype_pdata_
#define pc_frou_vartype_jdata            pc_frou_vartype_jdata_

                       ///////////////////////////////

#define pc_frou_get_gscalar              pc_frou_get_gscalar_
#define pc_frou_get_iscalar              pc_frou_get_iscalar_
#define pc_frou_get_fscalar              pc_frou_get_fscalar_
#define pc_frou_get_dscalar              pc_frou_get_dscalar_
#define pc_frou_get_lscalar              pc_frou_get_lscalar_
#define pc_frou_get_cscalar              pc_frou_get_cscalar_

#define pc_frou_get_process_gscalar      pc_frou_get_process_gscalar_
#define pc_frou_get_process_iscalar      pc_frou_get_process_iscalar_
#define pc_frou_get_process_fscalar      pc_frou_get_process_fscalar_
#define pc_frou_get_process_dscalar      pc_frou_get_process_dscalar_
#define pc_frou_get_process_lscalar      pc_frou_get_process_lscalar_
#define pc_frou_get_process_cscalar      pc_frou_get_process_cscalar_

#define pc_frou_get_global_gscalar       pc_frou_get_global_gscalar_
#define pc_frou_get_global_iscalar       pc_frou_get_global_iscalar_
#define pc_frou_get_global_fscalar       pc_frou_get_global_fscalar_
#define pc_frou_get_global_dscalar       pc_frou_get_global_dscalar_
#define pc_frou_get_global_lscalar       pc_frou_get_global_lscalar_
#define pc_frou_get_global_cscalar       pc_frou_get_global_cscalar_

#define pc_frou_get_control_gscalar      pc_frou_get_control_gscalar_
#define pc_frou_get_control_iscalar      pc_frou_get_control_iscalar_
#define pc_frou_get_control_fscalar      pc_frou_get_control_fscalar_
#define pc_frou_get_control_dscalar      pc_frou_get_control_dscalar_
#define pc_frou_get_control_lscalar      pc_frou_get_control_lscalar_
#define pc_frou_get_control_cscalar      pc_frou_get_control_cscalar_

#define pc_frou_get_gui_gscalar          pc_frou_get_gui_gscalar_
#define pc_frou_get_gui_iscalar          pc_frou_get_gui_iscalar_
#define pc_frou_get_gui_fscalar          pc_frou_get_gui_fscalar_
#define pc_frou_get_gui_dscalar          pc_frou_get_gui_dscalar_
#define pc_frou_get_gui_lscalar          pc_frou_get_gui_lscalar_
#define pc_frou_get_gui_cscalar          pc_frou_get_gui_cscalar_

#define pc_frou_get_pdata_gscalar        pc_frou_get_pdata_gscalar_
#define pc_frou_get_pdata_iscalar        pc_frou_get_pdata_iscalar_
#define pc_frou_get_pdata_fscalar        pc_frou_get_pdata_fscalar_
#define pc_frou_get_pdata_dscalar        pc_frou_get_pdata_dscalar_
#define pc_frou_get_pdata_lscalar        pc_frou_get_pdata_lscalar_
#define pc_frou_get_pdata_cscalar        pc_frou_get_pdata_cscalar_

#define pc_frou_get_jdata_gscalar        pc_frou_get_jdata_gscalar_
#define pc_frou_get_jdata_iscalar        pc_frou_get_jdata_iscalar_
#define pc_frou_get_jdata_fscalar        pc_frou_get_jdata_fscalar_
#define pc_frou_get_jdata_dscalar        pc_frou_get_jdata_dscalar_
#define pc_frou_get_jdata_lscalar        pc_frou_get_jdata_lscalar_
#define pc_frou_get_jdata_cscalar        pc_frou_get_jdata_cscalar_

                       ///////////////////////////////

#define pc_frou_get_iarray               pc_frou_get_iarray_
#define pc_frou_get_farray               pc_frou_get_farray_
#define pc_frou_get_darray               pc_frou_get_darray_
#define pc_frou_get_larray               pc_frou_get_larray_
#define pc_frou_get_carray               pc_frou_get_carray_

#define pc_frou_get_process_iarray       pc_frou_get_process_iarray_
#define pc_frou_get_process_farray       pc_frou_get_process_farray_
#define pc_frou_get_process_darray       pc_frou_get_process_darray_
#define pc_frou_get_process_larray       pc_frou_get_process_larray_
#define pc_frou_get_process_carray       pc_frou_get_process_carray_

#define pc_frou_get_global_iarray        pc_frou_get_global_iarray_
#define pc_frou_get_global_farray        pc_frou_get_global_farray_
#define pc_frou_get_global_darray        pc_frou_get_global_darray_
#define pc_frou_get_global_larray        pc_frou_get_global_larray_
#define pc_frou_get_global_carray        pc_frou_get_global_carray_

#define pc_frou_get_control_iarray       pc_frou_get_control_iarray_
#define pc_frou_get_control_farray       pc_frou_get_control_farray_
#define pc_frou_get_control_darray       pc_frou_get_control_darray_
#define pc_frou_get_control_larray       pc_frou_get_control_larray_
#define pc_frou_get_control_carray       pc_frou_get_control_carray_

#define pc_frou_get_gui_iarray           pc_frou_get_gui_iarray_
#define pc_frou_get_gui_farray           pc_frou_get_gui_farray_
#define pc_frou_get_gui_darray           pc_frou_get_gui_darray_
#define pc_frou_get_gui_larray           pc_frou_get_gui_larray_
#define pc_frou_get_gui_carray           pc_frou_get_gui_carray_

#define pc_frou_get_pdata_iarray         pc_frou_get_pdata_iarray_
#define pc_frou_get_pdata_farray         pc_frou_get_pdata_farray_
#define pc_frou_get_pdata_darray         pc_frou_get_pdata_darray_
#define pc_frou_get_pdata_larray         pc_frou_get_pdata_larray_
#define pc_frou_get_pdata_carray         pc_frou_get_pdata_carray_

#define pc_frou_get_jdata_iarray         pc_frou_get_jdata_iarray_
#define pc_frou_get_jdata_farray         pc_frou_get_jdata_farray_
#define pc_frou_get_jdata_darray         pc_frou_get_jdata_darray_
#define pc_frou_get_jdata_larray         pc_frou_get_jdata_larray_
#define pc_frou_get_jdata_carray         pc_frou_get_jdata_carray_

                       ///////////////////////////////

#define pc_frou_get_process_ielement     pc_frou_get_process_ielement_
#define pc_frou_get_process_felement     pc_frou_get_process_felement_
#define pc_frou_get_process_delement     pc_frou_get_process_delement_
#define pc_frou_get_process_lelement     pc_frou_get_process_lelement_
#define pc_frou_get_process_celement     pc_frou_get_process_celement_

#define pc_frou_get_global_ielement      pc_frou_get_global_ielement_
#define pc_frou_get_global_felement      pc_frou_get_global_felement_
#define pc_frou_get_global_delement      pc_frou_get_global_delement_
#define pc_frou_get_global_lelement      pc_frou_get_global_lelement_
#define pc_frou_get_global_celement      pc_frou_get_global_celement_

#define pc_frou_get_control_ielement     pc_frou_get_control_ielement_
#define pc_frou_get_control_felement     pc_frou_get_control_felement_
#define pc_frou_get_control_delement     pc_frou_get_control_delement_
#define pc_frou_get_control_lelement     pc_frou_get_control_lelement_
#define pc_frou_get_control_celement     pc_frou_get_control_celement_

#define pc_frou_get_gui_ielement         pc_frou_get_gui_ielement_
#define pc_frou_get_gui_felement         pc_frou_get_gui_felement_
#define pc_frou_get_gui_delement         pc_frou_get_gui_delement_
#define pc_frou_get_gui_lelement         pc_frou_get_gui_lelement_
#define pc_frou_get_gui_celement         pc_frou_get_gui_celement_

#define pc_frou_get_pdata_ielement       pc_frou_get_pdata_ielement_
#define pc_frou_get_pdata_felement       pc_frou_get_pdata_felement_
#define pc_frou_get_pdata_delement       pc_frou_get_pdata_delement_
#define pc_frou_get_pdata_lelement       pc_frou_get_pdata_lelement_
#define pc_frou_get_pdata_celement       pc_frou_get_pdata_celement_

#define pc_frou_get_jdata_ielement       pc_frou_get_jdata_ielement_
#define pc_frou_get_jdata_felement       pc_frou_get_jdata_felement_
#define pc_frou_get_jdata_delement       pc_frou_get_jdata_delement_
#define pc_frou_get_jdata_lelement       pc_frou_get_jdata_lelement_
#define pc_frou_get_jdata_celement       pc_frou_get_jdata_celement_

                       ///////////////////////////////

#define pc_frou_pressed                  pc_frou_pressed_
#define pc_frou_activated                pc_frou_activated_

#define pc_frou_verify_scalar            pc_frou_verify_scalar_
#define pc_frou_verify_element           pc_frou_verify_element_
#define pc_frou_verify_array             pc_frou_verify_array_
#define pc_frou_verify_arrayset          pc_frou_verify_arrayset_
#define pc_frou_verify_screen            pc_frou_verify_screen_
#define pc_frou_verify_end               pc_frou_verify_end_

                       ///////////////////////////////

#define pc_frou_put_gscalar              pc_frou_put_gscalar_
#define pc_frou_put_iscalar              pc_frou_put_iscalar_
#define pc_frou_put_fscalar              pc_frou_put_fscalar_
#define pc_frou_put_dscalar              pc_frou_put_dscalar_
#define pc_frou_put_lscalar              pc_frou_put_lscalar_
#define pc_frou_put_cscalar              pc_frou_put_cscalar_

#define pc_frou_put_process_gscalar      pc_frou_put_process_gscalar_
#define pc_frou_put_process_iscalar      pc_frou_put_process_iscalar_
#define pc_frou_put_process_fscalar      pc_frou_put_process_fscalar_
#define pc_frou_put_process_dscalar      pc_frou_put_process_dscalar_
#define pc_frou_put_process_lscalar      pc_frou_put_process_lscalar_
#define pc_frou_put_process_cscalar      pc_frou_put_process_cscalar_

#define pc_frou_put_global_gscalar       pc_frou_put_global_gscalar_
#define pc_frou_put_global_iscalar       pc_frou_put_global_iscalar_
#define pc_frou_put_global_fscalar       pc_frou_put_global_fscalar_
#define pc_frou_put_global_dscalar       pc_frou_put_global_dscalar_
#define pc_frou_put_global_lscalar       pc_frou_put_global_lscalar_
#define pc_frou_put_global_cscalar       pc_frou_put_global_cscalar_

#define pc_frou_put_control_gscalar      pc_frou_put_control_gscalar_
#define pc_frou_put_control_iscalar      pc_frou_put_control_iscalar_
#define pc_frou_put_control_fscalar      pc_frou_put_control_fscalar_
#define pc_frou_put_control_dscalar      pc_frou_put_control_dscalar_
#define pc_frou_put_control_lscalar      pc_frou_put_control_lscalar_
#define pc_frou_put_control_cscalar      pc_frou_put_control_cscalar_

#define pc_frou_put_gui_gscalar          pc_frou_put_gui_gscalar_
#define pc_frou_put_gui_iscalar          pc_frou_put_gui_iscalar_
#define pc_frou_put_gui_fscalar          pc_frou_put_gui_fscalar_
#define pc_frou_put_gui_dscalar          pc_frou_put_gui_dscalar_
#define pc_frou_put_gui_lscalar          pc_frou_put_gui_lscalar_
#define pc_frou_put_gui_cscalar          pc_frou_put_gui_cscalar_

#define pc_frou_put_gui_only_gscalar     pc_frou_put_gui_only_gscalar_
#define pc_frou_put_gui_only_iscalar     pc_frou_put_gui_only_iscalar_
#define pc_frou_put_gui_only_fscalar     pc_frou_put_gui_only_fscalar_
#define pc_frou_put_gui_only_dscalar     pc_frou_put_gui_only_dscalar_
#define pc_frou_put_gui_only_lscalar     pc_frou_put_gui_only_lscalar_
#define pc_frou_put_gui_only_cscalar     pc_frou_put_gui_only_cscalar_

#define pc_frou_put_pdata_gscalar        pc_frou_put_pdata_gscalar_
#define pc_frou_put_pdata_iscalar        pc_frou_put_pdata_iscalar_
#define pc_frou_put_pdata_fscalar        pc_frou_put_pdata_fscalar_
#define pc_frou_put_pdata_dscalar        pc_frou_put_pdata_dscalar_
#define pc_frou_put_pdata_lscalar        pc_frou_put_pdata_lscalar_
#define pc_frou_put_pdata_cscalar        pc_frou_put_pdata_cscalar_

#define pc_frou_put_jdata_gscalar        pc_frou_put_jdata_gscalar_
#define pc_frou_put_jdata_iscalar        pc_frou_put_jdata_iscalar_
#define pc_frou_put_jdata_fscalar        pc_frou_put_jdata_fscalar_
#define pc_frou_put_jdata_dscalar        pc_frou_put_jdata_dscalar_
#define pc_frou_put_jdata_lscalar        pc_frou_put_jdata_lscalar_
#define pc_frou_put_jdata_cscalar        pc_frou_put_jdata_cscalar_

                       ///////////////////////////////

#define pc_frou_put_iarray               pc_frou_put_iarray_
#define pc_frou_put_farray               pc_frou_put_farray_
#define pc_frou_put_darray               pc_frou_put_darray_
#define pc_frou_put_larray               pc_frou_put_larray_
#define pc_frou_put_carray               pc_frou_put_carray_

#define pc_frou_put_process_iarray       pc_frou_put_process_iarray_
#define pc_frou_put_process_farray       pc_frou_put_process_farray_
#define pc_frou_put_process_darray       pc_frou_put_process_darray_
#define pc_frou_put_process_larray       pc_frou_put_process_larray_
#define pc_frou_put_process_carray       pc_frou_put_process_carray_

#define pc_frou_put_global_iarray        pc_frou_put_global_iarray_
#define pc_frou_put_global_farray        pc_frou_put_global_farray_
#define pc_frou_put_global_darray        pc_frou_put_global_darray_
#define pc_frou_put_global_larray        pc_frou_put_global_larray_
#define pc_frou_put_global_carray        pc_frou_put_global_carray_

#define pc_frou_put_control_iarray       pc_frou_put_control_iarray_
#define pc_frou_put_control_farray       pc_frou_put_control_farray_
#define pc_frou_put_control_darray       pc_frou_put_control_darray_
#define pc_frou_put_control_larray       pc_frou_put_control_larray_
#define pc_frou_put_control_carray       pc_frou_put_control_carray_

#define pc_frou_put_gui_iarray           pc_frou_put_gui_iarray_
#define pc_frou_put_gui_farray           pc_frou_put_gui_farray_
#define pc_frou_put_gui_darray           pc_frou_put_gui_darray_
#define pc_frou_put_gui_larray           pc_frou_put_gui_larray_
#define pc_frou_put_gui_carray           pc_frou_put_gui_carray_

#define pc_frou_put_gui_only_iarray      pc_frou_put_gui_only_iarray_
#define pc_frou_put_gui_only_farray      pc_frou_put_gui_only_farray_
#define pc_frou_put_gui_only_darray      pc_frou_put_gui_only_darray_
#define pc_frou_put_gui_only_larray      pc_frou_put_gui_only_larray_
#define pc_frou_put_gui_only_carray      pc_frou_put_gui_only_carray_

#define pc_frou_put_pdata_iarray         pc_frou_put_pdata_iarray_
#define pc_frou_put_pdata_farray         pc_frou_put_pdata_farray_
#define pc_frou_put_pdata_darray         pc_frou_put_pdata_darray_
#define pc_frou_put_pdata_larray         pc_frou_put_pdata_larray_
#define pc_frou_put_pdata_carray         pc_frou_put_pdata_carray_

#define pc_frou_put_jdata_iarray         pc_frou_put_jdata_iarray_
#define pc_frou_put_jdata_farray         pc_frou_put_jdata_farray_
#define pc_frou_put_jdata_darray         pc_frou_put_jdata_darray_
#define pc_frou_put_jdata_larray         pc_frou_put_jdata_larray_
#define pc_frou_put_jdata_carray         pc_frou_put_jdata_carray_

                       ///////////////////////////////

#define pc_frou_register_array_names     pc_frou_register_array_names_

#define pc_frou_put_options_iscalar      pc_frou_put_options_iscalar_
#define pc_frou_put_options_fscalar      pc_frou_put_options_fscalar_
#define pc_frou_put_options_dscalar      pc_frou_put_options_dscalar_
#define pc_frou_put_options_lscalar      pc_frou_put_options_lscalar_
#define pc_frou_put_options_cscalar      pc_frou_put_options_cscalar_

#define pc_frou_put_options_iarray       pc_frou_put_options_iarray_
#define pc_frou_put_options_farray       pc_frou_put_options_farray_
#define pc_frou_put_options_darray       pc_frou_put_options_darray_
#define pc_frou_put_options_larray       pc_frou_put_options_larray_
#define pc_frou_put_options_carray       pc_frou_put_options_carray_

#define pc_frou_put_sns_field_flag       pc_frou_put_sns_field_flag_
#define pc_frou_put_sns_array_flag       pc_frou_put_sns_array_flag_
#define pc_frou_put_sns_arrayset_flag    pc_frou_put_sns_arrayset_flag_
#define pc_frou_put_sns_screen_flag      pc_frou_put_sns_screen_flag_

#define pc_frou_put_visible_flag         pc_frou_put_visible_flag_

#define pc_frou_put_minsize_array        pc_frou_put_minsize_array_
#define pc_frou_put_minsize_arrayset     pc_frou_put_minsize_arrayset_
#define pc_frou_put_maxsize_array        pc_frou_put_maxsize_array_
#define pc_frou_put_maxsize_arrayset     pc_frou_put_maxsize_arrayset_

                       ///////////////////////////////

#define pc_frou_num_process_cards        pc_frou_num_process_cards_
#define pc_frou_num_global_cards         pc_frou_num_global_cards_
#define pc_frou_num_control_cards        pc_frou_num_control_cards_
#define pc_frou_num_pdata_cards          pc_frou_num_pdata_cards_
#define pc_frou_num_jdata_cards          pc_frou_num_jdata_cards_
#define pc_frou_num_gui_cards            pc_frou_num_gui_cards_

                       ///////////////////////////////

#define pc_frou_get_process_cards        pc_frou_get_process_cards_
#define pc_frou_get_global_cards         pc_frou_get_global_cards_
#define pc_frou_get_control_cards        pc_frou_get_control_cards_
#define pc_frou_get_pdata_cards          pc_frou_get_pdata_cards_
#define pc_frou_get_jdata_cards          pc_frou_get_jdata_cards_
#define pc_frou_get_gui_cards            pc_frou_get_gui_cards_

#define pc_frou_get_process_card         pc_frou_get_process_card_
#define pc_frou_get_global_card          pc_frou_get_global_card_
#define pc_frou_get_control_card         pc_frou_get_control_card_
#define pc_frou_get_pdata_card           pc_frou_get_pdata_card_
#define pc_frou_get_jdata_card           pc_frou_get_jdata_card_
#define pc_frou_get_gui_card             pc_frou_get_gui_card_

#define pc_frou_put_process_cards        pc_frou_put_process_cards_
#define pc_frou_put_global_cards         pc_frou_put_global_cards_
#define pc_frou_put_control_cards        pc_frou_put_control_cards_
#define pc_frou_put_pdata_cards          pc_frou_put_pdata_cards_
#define pc_frou_put_jdata_cards          pc_frou_put_jdata_cards_
#define pc_frou_put_gui_cards            pc_frou_put_gui_cards_

#define pc_frou_put_process_card         pc_frou_put_process_card_
#define pc_frou_put_global_card          pc_frou_put_global_card_
#define pc_frou_put_control_card         pc_frou_put_control_card_
#define pc_frou_put_pdata_card           pc_frou_put_pdata_card_
#define pc_frou_put_jdata_card           pc_frou_put_jdata_card_
#define pc_frou_put_gui_card             pc_frou_put_gui_card_

#define pc_frou_add_process_card         pc_frou_add_process_card_
#define pc_frou_add_global_card          pc_frou_add_global_card_
#define pc_frou_add_control_card         pc_frou_add_control_card_
#define pc_frou_add_pdata_card           pc_frou_add_pdata_card_
#define pc_frou_add_jdata_card           pc_frou_add_jdata_card_
#define pc_frou_add_gui_card             pc_frou_add_gui_card_

                       ///////////////////////////////

#define pc_frou_clear_process_cards      pc_frou_clear_process_cards_
#define pc_frou_clear_global_cards       pc_frou_clear_global_cards_
#define pc_frou_clear_control_cards      pc_frou_clear_control_cards_
#define pc_frou_clear_pdata_cards        pc_frou_clear_pdata_cards_
#define pc_frou_clear_jdata_cards        pc_frou_clear_jdata_cards_
#define pc_frou_clear_gui_cards          pc_frou_clear_gui_cards_

#define pc_frou_process_keyword_present  pc_frou_process_keyword_present_
#define pc_frou_global_keyword_present   pc_frou_global_keyword_present_
#define pc_frou_control_keyword_present  pc_frou_control_keyword_present_
#define pc_frou_pdata_keyword_present    pc_frou_pdata_keyword_present_
#define pc_frou_jdata_keyword_present    pc_frou_jdata_keyword_present_
#define pc_frou_gui_action_present       pc_frou_gui_action_present_

#define pc_frou_num_process_keywords     pc_frou_num_process_keywords_
#define pc_frou_num_global_keywords      pc_frou_num_global_keywords_
#define pc_frou_num_control_keywords     pc_frou_num_control_keywords_
#define pc_frou_num_pdata_keywords       pc_frou_num_pdata_keywords_
#define pc_frou_num_jdata_keywords       pc_frou_num_jdata_keywords_
#define pc_frou_num_gui_keywords         pc_frou_num_gui_keywords_

#define pc_frou_get_process_keyword      pc_frou_get_process_keyword_
#define pc_frou_get_global_keyword       pc_frou_get_global_keyword_
#define pc_frou_get_control_keyword      pc_frou_get_control_keyword_
#define pc_frou_get_pdata_keyword        pc_frou_get_pdata_keyword_
#define pc_frou_get_jdata_keyword        pc_frou_get_jdata_keyword_
#define pc_frou_get_gui_keyword          pc_frou_get_gui_keyword_
#define pc_frou_get_gui_action           pc_frou_get_gui_action_

#define pc_frou_remove_process_keyword   pc_frou_remove_process_keyword_
#define pc_frou_remove_global_keyword    pc_frou_remove_global_keyword_
#define pc_frou_remove_control_keyword   pc_frou_remove_control_keyword_
#define pc_frou_remove_pdata_keyword     pc_frou_remove_pdata_keyword_
#define pc_frou_remove_jdata_keyword     pc_frou_remove_jdata_keyword_
#define pc_frou_remove_gui_action        pc_frou_remove_gui_action_

//---------------------- end of spelling adjustments ---------------------//
//---------------------- end of spelling adjustments ---------------------//
//---------------------- end of spelling adjustments ---------------------//

#endif
#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
