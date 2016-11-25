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
//------------------------------- PCW.cc --------------------------------//
//------------------------------- PCW.cc --------------------------------//
//------------------------------- PCW.cc --------------------------------//

// This class is a C++ wrapper around the parameter cache.

#include "PCW.hh"
#include "PCW_fortran_underscores.hh"
#include "PCW_fortran_capitals.hh"
#include "PCW_fortran_prototypes.hh"
#include "GTW.hh"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

static const int NWORDS = PCW::LENGTH / sizeof(INTEGER);
          // number of Fortran integers in a string array element.

//---------------------------- static functions ------------------------//
//---------------------------- static functions ------------------------//
//---------------------------- static functions ------------------------//

static void static_assert()
{
  assert(sizeof(INTEGER) == sizeof(int   ));         // to simplify coding.
  assert(sizeof(REAL   ) == sizeof(float ));         // to simplify coding.
  assert(sizeof(DOUBLE ) == sizeof(double));         // to simplify coding.

  assert(NWORDS * sizeof(INTEGER) == PCW::LENGTH);
     // to guarantee that PCW::LENGTH is an integral number of Fortran
     // integer words.
}

//------------------------------ functions ------------------------------//
//------------------------------ functions ------------------------------//
//------------------------------ functions ------------------------------//

int PCW::exists()
{
  return pc_frou_exists();
}

                  ////////////////////////////////

void PCW::frontendUpdate ()
{
  static_assert();
  pc_frou_frontend_update ();
}


void PCW::backendUpdate  ()
{
  static_assert();
  pc_frou_backend_update  ();
}


void PCW::guiUpdate  ()
{
  static_assert();
  pc_frou_gui_update  ();
}


void PCW::quickUpdate  ()
{
  static_assert();
  pc_frou_quick_update  ();
}

                  ////////////////////////////////


void PCW::frontendUpdateNoprint ()
{
  static_assert();
  pc_frou_frontend_update_noprint ();
}


void PCW::backendUpdateNoprint  ()
{
  static_assert();
  pc_frou_backend_update_noprint  ();
}


void PCW::guiUpdateNoprint  ()
{
  static_assert();
  pc_frou_gui_update_noprint  ();
}


void PCW::quickUpdateNoprint  ()
{
  static_assert();
  pc_frou_quick_update_noprint  ();
}


                  ////////////////////////////////


void PCW::clear ()
{
  pc_frou_clear  ();
}


void PCW::restore ()
{
  pc_frou_restore  ();
}


void PCW::next  ()
{
  pc_frou_next ();
}


void PCW::backendExecute  ()
{
  pc_frou_backend_execute ();
}


void PCW::continueBackendUpdate ()
{
  pc_frou_continue_backend_update ();
}


                  ////////////////////////////////


int PCW::getUpdateState ()
{
  return pc_frou_get_update_state ();
}


void PCW::setBackendNoExec ()
{
  pc_frou_set_backend_no_exec  ();
}


void PCW::setBackendYesExec  ()
{
  pc_frou_set_backend_yes_exec ();
}


int PCW::getIpn ()
{
  return pc_frou_get_ipn ();
}


int PCW::previousError  ()
{
  return pc_frou_previous_error ();
}


void PCW::setIpn (int ipn)
{
  pc_frou_set_ipn (&ipn);
}

                  ////////////////////////////////


int PCW::doNotProcessTraces ()
{
  return pc_frou_do_not_process_traces ();
}

                  ////////////////////////////////


int PCW::updateError()
{
  return pc_frou_update_error ();
}


void PCW::error  (const char *msg)
{
  pc_frou_error (msg);
}


void PCW::warning  (const char *msg)
{
  pc_frou_warning (msg);
}


void PCW::info (const char *msg)
{
  pc_frou_info  (msg);
}


void PCW::print (const char *msg)
{
  pc_frou_print  (msg);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::printAllDataCards  ()
{
  printf("\n");
  printProcessCards ();
  printGlobalCards  ();
  printControlCards ();
  printPdataCards   ();
  printJdataCards   ();
  printGuiCards     ();
  printf("\n");
}


void PCW::printProcessCards  ()
{
  printf("Process data cards:\n");
  fflush(stdout);
  if(numProcessCards() == 0) printf("    none\n");
  else                       pc_frou_print_process_cards ();
  fflush(stdout);
}


void PCW::printGlobalCards ()
{
  printf("Global data cards:\n");
  fflush(stdout);
  if(numGlobalCards() == 0) printf("    none\n");
  else                      pc_frou_print_global_cards  ();
  fflush(stdout);
}


void PCW::printControlCards  ()
{
  printf("Control data cards:\n");
  fflush(stdout);
  if(numControlCards() == 0) printf("    none\n");
  else                       pc_frou_print_control_cards ();
  fflush(stdout);
}


void PCW::printPdataCards  ()
{
  printf("ProjectData data cards:\n");
  fflush(stdout);
  if(numPdataCards() == 0) printf("    none\n");
  else                     pc_frou_print_pdata_cards ();
  fflush(stdout);
}


void PCW::printJdataCards  ()
{
  printf("JobData data cards:\n");
  fflush(stdout);
  if(numJdataCards() == 0) printf("    none\n");
  else                     pc_frou_print_jdata_cards ();
  fflush(stdout);
}


void PCW::printGuiCards  ()
{
  printf("Gui data cards:\n");
  fflush(stdout);
  if(numGuiCards() == 0) printf("    none\n");
  else                   pc_frou_print_gui_cards ();
  fflush(stdout);
}

                  ////////////////////////////////


void PCW::infoProcessCards ()
{
  pc_frou_info_process_cards ();
}


void PCW::infoGlobalCards  ()
{
  pc_frou_info_global_cards  ();
}


void PCW::infoControlCards ()
{
  pc_frou_info_control_cards ();
}


void PCW::infoPdataCards ()
{
  pc_frou_info_pdata_cards ();
}


void PCW::infoJdataCards ()
{
  pc_frou_info_jdata_cards ();
}


void PCW::infoGuiCards ()
{
  pc_frou_info_gui_cards ();
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


int PCW::numElementsProcess (const char *keyword)
{
  return pc_frou_num_elements_process (keyword);
}


int PCW::numElementsGlobal  (const char *keyword)
{
  return pc_frou_num_elements_global  (keyword);
}


int PCW::numElementsControl (const char *keyword)
{
  return pc_frou_num_elements_control (keyword);
}


int PCW::numElementsGui (const char *keyword, const char *action)
{
  return pc_frou_num_elements_gui     (keyword, action);
}


int PCW::numElementsPdata (const char *keyword)
{
  return pc_frou_num_elements_pdata   (keyword);
}


int PCW::numElementsJdata (const char *keyword)
{
  return pc_frou_num_elements_jdata   (keyword);
}

                  ////////////////////////////////


int PCW::natureProcess  (const char *keyword)
{
  return pc_frou_nature_process (keyword);
}


int PCW::natureGlobal (const char *keyword)
{
  return pc_frou_nature_global  (keyword);
}


int PCW::natureControl  (const char *keyword)
{
  return pc_frou_nature_control (keyword);
}


int PCW::natureGui  (const char *keyword, const char *action)
{
  return pc_frou_nature_gui     (keyword, action);
}


int PCW::naturePdata  (const char *keyword)
{
  return pc_frou_nature_pdata   (keyword);
}


int PCW::natureJdata  (const char *keyword)
{
  return pc_frou_nature_jdata   (keyword);
}

                  ////////////////////////////////


int PCW::vartypeProcess  (const char *keyword)
{
  return pc_frou_vartype_process (keyword);
}


int PCW::vartypeGlobal (const char *keyword)
{
  return pc_frou_vartype_global  (keyword);
}


int PCW::vartypeControl  (const char *keyword)
{
  return pc_frou_vartype_control (keyword);
}


int PCW::vartypeGui  (const char *keyword, const char *action)
{
  return pc_frou_vartype_gui     (keyword, action);
}


int PCW::vartypePdata  (const char *keyword)
{
  return pc_frou_vartype_pdata   (keyword);
}


int PCW::vartypeJdata  (const char *keyword)
{
  return pc_frou_vartype_jdata   (keyword);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::get (const char *keyword, GTW *scalar)
{
  F90Pointer *fpoint = scalar->getFpoint();
  pc_frou_get_gscalar (keyword, fpoint);
}


void PCW::get (const char *keyword, int *scalar)
{
  pc_frou_get_iscalar (keyword, scalar);
}


void PCW::get (const char *keyword, float *scalar)
{
  pc_frou_get_fscalar (keyword, scalar);
}


void PCW::get (const char *keyword, double *scalar)
{
  pc_frou_get_dscalar (keyword, scalar);
}


void PCW::getL  (const char *keyword, int *scalar)
{
  pc_frou_get_lscalar (keyword, scalar);
}


void PCW::get (const char *keyword, char *scalar)
{
  pc_frou_get_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::getProcess  (const char *keyword, GTW *scalar)
{
  F90Pointer *fpoint = scalar->getFpoint();
  pc_frou_get_process_gscalar (keyword, fpoint);
}


void PCW::getProcess  (const char *keyword, int *scalar)
{
  pc_frou_get_process_iscalar (keyword, scalar);
}


void PCW::getProcess  (const char *keyword, float *scalar)
{
  pc_frou_get_process_fscalar (keyword, scalar);
}


void PCW::getProcess  (const char *keyword, double *scalar)
{
  pc_frou_get_process_dscalar (keyword, scalar);
}


void PCW::getProcessL (const char *keyword, int *scalar)
{
  pc_frou_get_process_lscalar (keyword, scalar);
}


void PCW::getProcess  (const char *keyword, char *scalar)
{
  pc_frou_get_process_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::getGlobal (const char *keyword, GTW *scalar)
{
  F90Pointer *fpoint = scalar->getFpoint();
  pc_frou_get_global_gscalar (keyword, fpoint);
}


void PCW::getGlobal (const char *keyword, int *scalar)
{
  pc_frou_get_global_iscalar (keyword, scalar);
}


void PCW::getGlobal (const char *keyword, float *scalar)
{
  pc_frou_get_global_fscalar (keyword, scalar);
}


void PCW::getGlobal (const char *keyword, double *scalar)
{
  pc_frou_get_global_dscalar (keyword, scalar);
}


void PCW::getGlobalL  (const char *keyword, int *scalar)
{
  pc_frou_get_global_lscalar (keyword, scalar);
}


void PCW::getGlobal (const char *keyword, char *scalar)
{
  pc_frou_get_global_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::getControl  (const char *keyword, GTW *scalar)
{
  F90Pointer *fpoint = scalar->getFpoint();
  pc_frou_get_control_gscalar (keyword, fpoint);
}


void PCW::getControl  (const char *keyword, int *scalar)
{
  pc_frou_get_control_iscalar (keyword, scalar);
}


void PCW::getControl  (const char *keyword, float *scalar)
{
  pc_frou_get_control_fscalar (keyword, scalar);
}


void PCW::getControl  (const char *keyword, double *scalar)
{
  pc_frou_get_control_dscalar (keyword, scalar);
}


void PCW::getControlL (const char *keyword, int *scalar)
{
  pc_frou_get_control_lscalar (keyword, scalar);
}


void PCW::getControl  (const char *keyword, char *scalar)
{
  pc_frou_get_control_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::getGui  (const char *keyword, const char *action,
                                           GTW *scalar)
{
  F90Pointer *fpoint = scalar->getFpoint();
  pc_frou_get_gui_gscalar (keyword, action, fpoint);
}


void PCW::getGui  (const char *keyword, const char *action, int *scalar)
{
  pc_frou_get_gui_iscalar (keyword, action, scalar);
}


void PCW::getGui  (const char *keyword, const char *action, float *scalar)
{
  pc_frou_get_gui_fscalar (keyword, action, scalar);
}


void PCW::getGui  (const char *keyword, const char *action, double *scalar)
{
  pc_frou_get_gui_dscalar (keyword, action, scalar);
}


void PCW::getGuiL (const char *keyword, const char *action, int *scalar)
{
  pc_frou_get_gui_lscalar (keyword, action, scalar);
}


void PCW::getGui  (const char *keyword, const char *action, char *scalar)
{
  pc_frou_get_gui_cscalar (keyword, action, scalar);
}

                  ////////////////////////////////


void PCW::getPdata  (const char *keyword, GTW *scalar)
{
  F90Pointer *fpoint = scalar->getFpoint();
  pc_frou_get_pdata_gscalar (keyword, fpoint);
}


void PCW::getPdata  (const char *keyword, int *scalar)
{
  pc_frou_get_pdata_iscalar (keyword, scalar);
}


void PCW::getPdata  (const char *keyword, float *scalar)
{
  pc_frou_get_pdata_fscalar (keyword, scalar);
}


void PCW::getPdata  (const char *keyword, double *scalar)
{
  pc_frou_get_pdata_dscalar (keyword, scalar);
}


void PCW::getPdataL (const char *keyword, int *scalar)
{
  pc_frou_get_pdata_lscalar (keyword, scalar);
}


void PCW::getPdata  (const char *keyword, char *scalar)
{
  pc_frou_get_pdata_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::getJdata  (const char *keyword, GTW *scalar)
{
  F90Pointer *fpoint = scalar->getFpoint();
  pc_frou_get_jdata_gscalar (keyword, fpoint);
}


void PCW::getJdata  (const char *keyword, int *scalar)
{
  pc_frou_get_jdata_iscalar (keyword, scalar);
}


void PCW::getJdata  (const char *keyword, float *scalar)
{
  pc_frou_get_jdata_fscalar (keyword, scalar);
}


void PCW::getJdata  (const char *keyword, double *scalar)
{
  pc_frou_get_jdata_dscalar (keyword, scalar);
}


void PCW::getJdataL (const char *keyword, int *scalar)
{
  pc_frou_get_jdata_lscalar (keyword, scalar);
}


void PCW::getJdata  (const char *keyword, char *scalar)
{
  pc_frou_get_jdata_cscalar (keyword, scalar);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::get  (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_iarray (keyword, &nsize, array, nelements);
}


void PCW::get  (const char *keyword, int nsize, float *array, int *nelements)
{
  pc_frou_get_farray (keyword, &nsize, array, nelements);
}


void PCW::get  (const char *keyword, int nsize, double *array, int *nelements)
{
  pc_frou_get_darray (keyword, &nsize, array, nelements);
}


void PCW::getL (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_larray (keyword, &nsize, array, nelements);
}


void PCW::get
      (const char *keyword, int nsize, char *array, int *nelements)
{
  pc_frou_get_carray (keyword, &nsize, array, nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::getProcess
       (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_process_iarray (keyword, &nsize, array, nelements);
}


void PCW::getProcess
       (const char *keyword, int nsize, float *array, int *nelements)
{
  pc_frou_get_process_farray (keyword, &nsize, array, nelements);
}


void PCW::getProcess
       (const char *keyword, int nsize, double *array, int *nelements)
{
  pc_frou_get_process_darray (keyword, &nsize, array, nelements);
}


void PCW::getProcessL
      (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_process_larray (keyword, &nsize, array, nelements);
}


void PCW::getProcess
     (const char *keyword, int nsize, char *array, int *nelements)
{
  pc_frou_get_process_carray (keyword, &nsize, array, nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::getGlobal
      (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_global_iarray (keyword, &nsize, array, nelements);
}


void PCW::getGlobal
      (const char *keyword, int nsize, float *array, int *nelements)
{
  pc_frou_get_global_farray (keyword, &nsize, array, nelements);
}


void PCW::getGlobal
      (const char *keyword, int nsize, double *array, int *nelements)
{
  pc_frou_get_global_darray (keyword, &nsize, array, nelements);
}


void PCW::getGlobalL
       (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_global_larray (keyword, &nsize, array, nelements);
}


void PCW::getGlobal
     (const char *keyword, int nsize, char *array, int *nelements)
{
  pc_frou_get_global_carray (keyword, &nsize, array, nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::getControl
       (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_control_iarray (keyword, &nsize, array, nelements);
}


void PCW::getControl
       (const char *keyword, int nsize, float *array, int *nelements)
{
  pc_frou_get_control_farray (keyword, &nsize, array, nelements);
}


void PCW::getControl
       (const char *keyword, int nsize, double *array, int *nelements)
{
  pc_frou_get_control_darray (keyword, &nsize, array, nelements);
}


void PCW::getControlL
      (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_control_larray (keyword, &nsize, array, nelements);
}


void PCW::getControl
    (const char *keyword, int nsize, char *array, int *nelements)
{
  pc_frou_get_control_carray (keyword, &nsize, array, nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::getGui  (const char *keyword, const char *action, int nsize,
                   int *array, int *nelements)
{
  pc_frou_get_gui_iarray (keyword, action, &nsize, array, nelements);
}


void PCW::getGui  (const char *keyword, const char *action, int nsize,
                   float *array, int *nelements)
{
  pc_frou_get_gui_farray (keyword, action, &nsize, array, nelements);
}


void PCW::getGui  (const char *keyword, const char *action, int nsize,
                   double *array, int *nelements)
{
  pc_frou_get_gui_darray (keyword, action, &nsize, array, nelements);
}


void PCW::getGuiL (const char *keyword, const char *action, int nsize,
                   int *array, int *nelements)
{
  pc_frou_get_gui_larray (keyword, action, &nsize, array, nelements);
}


void PCW::getGui  (const char *keyword, const char *action, int nsize,
                   char *array, int *nelements)
{
  pc_frou_get_gui_carray (keyword, action, &nsize, array, nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::getPdata
       (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_pdata_iarray (keyword, &nsize, array, nelements);
}


void PCW::getPdata
       (const char *keyword, int nsize, float *array, int *nelements)
{
  pc_frou_get_pdata_farray (keyword, &nsize, array, nelements);
}


void PCW::getPdata
       (const char *keyword, int nsize, double *array, int *nelements)
{
  pc_frou_get_pdata_darray (keyword, &nsize, array, nelements);
}


void PCW::getPdataL
      (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_pdata_larray (keyword, &nsize, array, nelements);
}


void PCW::getPdata
    (const char *keyword, int nsize, char *array, int *nelements)
{
  pc_frou_get_pdata_carray (keyword, &nsize, array, nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::getJdata
       (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_jdata_iarray (keyword, &nsize, array, nelements);
}


void PCW::getJdata
       (const char *keyword, int nsize, float *array, int *nelements)
{
  pc_frou_get_jdata_farray (keyword, &nsize, array, nelements);
}


void PCW::getJdata
       (const char *keyword, int nsize, double *array, int *nelements)
{
  pc_frou_get_jdata_darray (keyword, &nsize, array, nelements);
}


void PCW::getJdataL
      (const char *keyword, int nsize, int *array, int *nelements)
{
  pc_frou_get_jdata_larray (keyword, &nsize, array, nelements);
}


void PCW::getJdata
    (const char *keyword, int nsize, char *array, int *nelements)
{
  pc_frou_get_jdata_carray (keyword, &nsize, array, nelements, &NWORDS);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::getProcess  (const char *keyword, int indx, int *element)
{
  pc_frou_get_process_ielement (keyword, &indx, element);
}


void PCW::getProcess  (const char *keyword, int indx, float *element)
{
  pc_frou_get_process_felement (keyword, &indx, element);
}


void PCW::getProcess  (const char *keyword, int indx, double *element)
{
  pc_frou_get_process_delement (keyword, &indx, element);
}


void PCW::getProcessL (const char *keyword, int indx, int *element)
{
  pc_frou_get_process_lelement (keyword, &indx, element);
}


void PCW::getProcess  (const char *keyword, int indx, char *element)
{
  pc_frou_get_process_celement (keyword, &indx, element);
}

                  ////////////////////////////////


void PCW::getGlobal (const char *keyword, int indx, int *element)
{
  pc_frou_get_global_ielement (keyword, &indx, element);
}


void PCW::getGlobal (const char *keyword, int indx, float *element)
{
  pc_frou_get_global_felement (keyword, &indx, element);
}


void PCW::getGlobal (const char *keyword, int indx, double *element)
{
  pc_frou_get_global_delement (keyword, &indx, element);
}


void PCW::getGlobalL  (const char *keyword, int indx, int *element)
{
  pc_frou_get_global_lelement (keyword, &indx, element);
}


void PCW::getGlobal (const char *keyword, int indx, char *element)
{
  pc_frou_get_global_celement (keyword, &indx, element);
}

                  ////////////////////////////////


void PCW::getControl  (const char *keyword, int indx, int *element)
{
  pc_frou_get_control_ielement (keyword, &indx, element);
}


void PCW::getControl  (const char *keyword, int indx, float *element)
{
  pc_frou_get_control_felement (keyword, &indx, element);
}


void PCW::getControl  (const char *keyword, int indx, double *element)
{
  pc_frou_get_control_delement (keyword, &indx, element);
}


void PCW::getControlL (const char *keyword, int indx, int *element)
{
  pc_frou_get_control_lelement (keyword, &indx, element);
}


void PCW::getControl  (const char *keyword, int indx, char *element)
{
  pc_frou_get_control_celement (keyword, &indx, element);
}

                  ////////////////////////////////


void PCW::getGui
  (const char *keyword, const char *action, int indx, int *element)
{
  pc_frou_get_gui_ielement (keyword, action, &indx, element);
}


void PCW::getGui
  (const char *keyword, const char *action, int indx, float *element)
{
  pc_frou_get_gui_felement (keyword, action, &indx, element);
}


void PCW::getGui
  (const char *keyword, const char *action, int indx, double *element)
{
  pc_frou_get_gui_delement (keyword, action, &indx, element);
}


void PCW::getGuiL
 (const char *keyword, const char *action, int indx, int *element)
{
  pc_frou_get_gui_lelement (keyword, action, &indx, element);
}


void PCW::getGui
  (const char *keyword, const char *action, int indx, char *element)
{
  pc_frou_get_gui_celement (keyword, action, &indx, element);
}

                  ////////////////////////////////


void PCW::getPdata  (const char *keyword, int indx, int *element)
{
  pc_frou_get_pdata_ielement (keyword, &indx, element);
}


void PCW::getPdata  (const char *keyword, int indx, float *element)
{
  pc_frou_get_pdata_felement (keyword, &indx, element);
}


void PCW::getPdata  (const char *keyword, int indx, double *element)
{
  pc_frou_get_pdata_delement (keyword, &indx, element);
}


void PCW::getPdataL (const char *keyword, int indx, int *element)
{
  pc_frou_get_pdata_lelement (keyword, &indx, element);
}


void PCW::getPdata  (const char *keyword, int indx, char *element)
{
  pc_frou_get_pdata_celement (keyword, &indx, element);
}

                  ////////////////////////////////


void PCW::getJdata  (const char *keyword, int indx, int *element)
{
  pc_frou_get_jdata_ielement (keyword, &indx, element);
}


void PCW::getJdata  (const char *keyword, int indx, float *element)
{
  pc_frou_get_jdata_felement (keyword, &indx, element);
}


void PCW::getJdata  (const char *keyword, int indx, double *element)
{
  pc_frou_get_jdata_delement (keyword, &indx, element);
}


void PCW::getJdataL (const char *keyword, int indx, int *element)
{
  pc_frou_get_jdata_lelement (keyword, &indx, element);
}


void PCW::getJdata  (const char *keyword, int indx, char *element)
{
  pc_frou_get_jdata_celement (keyword, &indx, element);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


int PCW::pressed (const char *keyword)
{
  return pc_frou_pressed(keyword);
}


void PCW::activated (char *keyword)
{
  pc_frou_activated (keyword);
}

                  ////////////////////////////////


int PCW::verifyScalar (const char *keyword)
{
  return pc_frou_verify_scalar (keyword);
}


int PCW::verifyElement  (const char *keyword, int *indx, int *action)
{
  return pc_frou_verify_element (keyword, indx, action);
}


int PCW::verifyArray  (const char *keyword)
{
  return pc_frou_verify_array (keyword);
}


int PCW::verifyArrayset (const char *keyword)                   
{
  return pc_frou_verify_arrayset (keyword);
}


int PCW::verifyScreen (const char *keyword)                   
{
  return pc_frou_verify_screen (keyword);
}


int PCW::verifyEnd  ()                      
{
  return pc_frou_verify_end ();
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::put (const char *keyword, const GTW *scalar,
               int nchar, int ndec)
{
  const F90Pointer *fpoint = scalar->getConstFpoint();
  pc_frou_put_gscalar (keyword, fpoint, &nchar, &ndec);
}


void PCW::put (const char *keyword, int scalar, int nchar)
{
  pc_frou_put_iscalar (keyword, &scalar, &nchar);
}


void PCW::put (const char *keyword, float scalar, int nchar, int ndec)
{
  pc_frou_put_fscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::put (const char *keyword, double scalar, int nchar, int ndec)
{
  pc_frou_put_dscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putL  (const char *keyword, int scalar)
{
  pc_frou_put_lscalar (keyword, &scalar);
}


void PCW::put (const char *keyword, const char *scalar)
{
  pc_frou_put_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::putProcess  (const char *keyword, const GTW *scalar,
                       int nchar, int ndec)
{
  const F90Pointer *fpoint = scalar->getConstFpoint();
/*
printf("PCW.cc putProcess: fpoint = %p\n", fpoint);
printf("PCW.cc putProcess: xorigin = %lf\n", scalar->getXorigin());
printf("PCW.cc putProcess: yorigin = %lf\n", scalar->getYorigin());
printf("PCW.cc putProcess: xwidth = %lf\n", scalar->getXgridWidth());
printf("PCW.cc putProcess: ywidth = %lf\n", scalar->getYgridWidth());
printf("PCW.cc putProcess: angle = %lf\n", scalar->getRotationAngle());
printf("PCW.cc putProcess: handedness = %d\n", scalar->getHandedness());
*/
  pc_frou_put_process_gscalar (keyword, fpoint, &nchar, &ndec);
}


void PCW::putProcess  (const char *keyword, int scalar, int nchar)
{
  pc_frou_put_process_iscalar (keyword, &scalar, &nchar);
}


void PCW::putProcess  (const char *keyword, float scalar, int nchar, int ndec)
{
  pc_frou_put_process_fscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putProcess  (const char *keyword, double scalar, int nchar, int ndec)
{
  pc_frou_put_process_dscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putProcessL (const char *keyword, int scalar)
{
  pc_frou_put_process_lscalar (keyword, &scalar);
}


void PCW::putProcess  (const char *keyword, const char *scalar)
{
  pc_frou_put_process_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::putGlobal (const char *keyword, const GTW *scalar,
                     int nchar, int ndec)
{
  const F90Pointer *fpoint = scalar->getConstFpoint();
/*
printf("PCW.cc putGlobal: fpoint = %p\n", fpoint);
printf("PCW.cc putGlobal: xorigin = %lf\n", scalar->getXorigin());
printf("PCW.cc putGlobal: yorigin = %lf\n", scalar->getYorigin());
printf("PCW.cc putGlobal: xwidth = %lf\n", scalar->getXgridWidth());
printf("PCW.cc putGlobal: ywidth = %lf\n", scalar->getYgridWidth());
printf("PCW.cc putGlobal: angle = %lf\n", scalar->getRotationAngle());
printf("PCW.cc putGlobal: handedness = %d\n", scalar->getHandedness());
*/
  pc_frou_put_global_gscalar (keyword, fpoint, &nchar, &ndec);
}


void PCW::putGlobal (const char *keyword, int scalar, int nchar)
{
  pc_frou_put_global_iscalar (keyword, &scalar, &nchar);
}


void PCW::putGlobal (const char *keyword, float scalar, int nchar, int ndec)
{
  pc_frou_put_global_fscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putGlobal (const char *keyword, double scalar, int nchar, int ndec)
{
  pc_frou_put_global_dscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putGlobalL  (const char *keyword, int scalar)
{
  pc_frou_put_global_lscalar (keyword, &scalar);
}


void PCW::putGlobal (const char *keyword, const char *scalar)
{
  pc_frou_put_global_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::putControl  (const char *keyword, const GTW *scalar,
                       int nchar, int ndec)
{
  const F90Pointer *fpoint = scalar->getConstFpoint();
  pc_frou_put_control_gscalar (keyword, fpoint, &nchar, &ndec);
}


void PCW::putControl  (const char *keyword, int scalar, int nchar)
{
  pc_frou_put_control_iscalar (keyword, &scalar, &nchar);
}


void PCW::putControl  (const char *keyword, float scalar, int nchar, int ndec)
{
  pc_frou_put_control_fscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putControl  (const char *keyword, double scalar, int nchar, int ndec)
{
  pc_frou_put_control_dscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putControlL (const char *keyword, int scalar)
{
  pc_frou_put_control_lscalar (keyword, &scalar);
}


void PCW::putControl  (const char *keyword, const char *scalar)
{
  pc_frou_put_control_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::putGui  (const char *keyword, const char *action,
                   const GTW *scalar, int nchar, int ndec)
{
  const F90Pointer *fpoint = scalar->getConstFpoint();
  pc_frou_put_gui_gscalar (keyword, action, fpoint, &nchar, &ndec);
}


void PCW::putGui  (const char *keyword, const char *action,
                   int scalar, int nchar)
{
  pc_frou_put_gui_iscalar (keyword, action, &scalar, &nchar);
}


void PCW::putGui  (const char *keyword, const char *action,
                   float scalar, int nchar, int ndec)
{
  pc_frou_put_gui_fscalar (keyword, action, &scalar, &nchar, &ndec);
}


void PCW::putGui  (const char *keyword, const char *action,
                   double scalar, int nchar, int ndec)
{
  pc_frou_put_gui_dscalar (keyword, action, &scalar, &nchar, &ndec);
}


void PCW::putGuiL (const char *keyword, const char *action,
                   int scalar)
{
  pc_frou_put_gui_lscalar (keyword, action, &scalar);
}


void PCW::putGui  (const char *keyword, const char *action,
                   const char *scalar)
{
  pc_frou_put_gui_cscalar (keyword, action, scalar);
}

                  ////////////////////////////////


void PCW::putGuiOnly  (const char *keyword,
                       const GTW *scalar, int nchar, int ndec)
{
  const F90Pointer *fpoint = scalar->getConstFpoint();
  pc_frou_put_gui_only_gscalar (keyword, fpoint, &nchar, &ndec);
}


void PCW::putGuiOnly  (const char *keyword, int scalar, int nchar)
{
  pc_frou_put_gui_only_iscalar (keyword, &scalar, &nchar);
}


void PCW::putGuiOnly  (const char *keyword, float scalar, int nchar, int ndec)
{
  pc_frou_put_gui_only_fscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putGuiOnly  (const char *keyword, double scalar, int nchar, int ndec)
{
  pc_frou_put_gui_only_dscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putGuiOnlyL (const char *keyword, int scalar)
{
  pc_frou_put_gui_only_lscalar (keyword, &scalar);
}


void PCW::putGuiOnly  (const char *keyword, const char *scalar)
{
  pc_frou_put_gui_only_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::putPdata  (const char *keyword, const GTW *scalar,
                     int nchar, int ndec)
{
  const F90Pointer *fpoint = scalar->getConstFpoint();
  pc_frou_put_pdata_gscalar (keyword, fpoint, &nchar, &ndec);
}


void PCW::putPdata  (const char *keyword, int scalar, int nchar)
{
  pc_frou_put_pdata_iscalar (keyword, &scalar, &nchar);
}


void PCW::putPdata  (const char *keyword, float scalar, int nchar, int ndec)
{
  pc_frou_put_pdata_fscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putPdata  (const char *keyword, double scalar, int nchar, int ndec)
{
  pc_frou_put_pdata_dscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putPdataL (const char *keyword, int scalar)
{
  pc_frou_put_pdata_lscalar (keyword, &scalar);
}


void PCW::putPdata  (const char *keyword, const char *scalar)
{
  pc_frou_put_pdata_cscalar (keyword, scalar);
}

                  ////////////////////////////////


void PCW::putJdata  (const char *keyword, const GTW *scalar,
                     int nchar, int ndec)
{
  const F90Pointer *fpoint = scalar->getConstFpoint();
  pc_frou_put_jdata_gscalar (keyword, fpoint, &nchar, &ndec);
}


void PCW::putJdata  (const char *keyword, int scalar, int nchar)
{
  pc_frou_put_jdata_iscalar (keyword, &scalar, &nchar);
}


void PCW::putJdata  (const char *keyword, float scalar, int nchar, int ndec)
{
  pc_frou_put_jdata_fscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putJdata  (const char *keyword, double scalar, int nchar, int ndec)
{
  pc_frou_put_jdata_dscalar (keyword, &scalar, &nchar, &ndec);
}


void PCW::putJdataL (const char *keyword, int scalar)
{
  pc_frou_put_jdata_lscalar (keyword, &scalar);
}


void PCW::putJdata  (const char *keyword, const char *scalar)
{
  pc_frou_put_jdata_cscalar (keyword, scalar);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::put (const char *keyword, const int *array, int nelements, int nchar)
{
  pc_frou_put_iarray (keyword, array, &nelements, &nchar);
}


void PCW::put (const char *keyword, const float *array, int nelements,
               int nchar, int ndec)
{
  pc_frou_put_farray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::put (const char *keyword, const double *array, int nelements,
               int nchar, int ndec)
{
  pc_frou_put_darray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putL  (const char *keyword, const int *array, int nelements)
{
  pc_frou_put_larray (keyword, array, &nelements);
}


void PCW::put (const char *keyword, const char *array, int nelements)
{
  pc_frou_put_carray (keyword, array, &nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::putProcess  (const char *keyword, const int *array, int nelements,
                       int nchar)
{
  pc_frou_put_process_iarray (keyword, array, &nelements, &nchar);
}


void PCW::putProcess  (const char *keyword, const float *array, int nelements,
                       int nchar, int ndec)
{
  pc_frou_put_process_farray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putProcess  (const char *keyword, const double *array, int nelements,
                       int nchar, int ndec)
{
  pc_frou_put_process_darray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putProcessL (const char *keyword, const int *array, int nelements)
{
  pc_frou_put_process_larray (keyword, array, &nelements);
}


void PCW::putProcess  (const char *keyword, const char *array, int nelements)
{
  pc_frou_put_process_carray (keyword, array, &nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::putGlobal (const char *keyword, const int *array, int nelements,
                     int nchar)
{
  pc_frou_put_global_iarray (keyword, array, &nelements, &nchar);
}


void PCW::putGlobal (const char *keyword, const float *array, int nelements,
                     int nchar, int ndec)
{
  pc_frou_put_global_farray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putGlobal (const char *keyword, const double *array, int nelements,
                     int nchar, int ndec)
{
  pc_frou_put_global_darray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putGlobalL  (const char *keyword, const int *array, int nelements)
{
  pc_frou_put_global_larray (keyword, array, &nelements);
}


void PCW::putGlobal (const char *keyword, const char *array, int nelements)
{
  pc_frou_put_global_carray (keyword, array, &nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::putControl  (const char *keyword, const int *array, int nelements,
                       int nchar)
{
  pc_frou_put_control_iarray (keyword, array, &nelements, &nchar);
}


void PCW::putControl  (const char *keyword, const float *array, int nelements,
                       int nchar, int ndec)
{
  pc_frou_put_control_farray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putControl  (const char *keyword, const double *array, int nelements,
                       int nchar, int ndec)
{
  pc_frou_put_control_darray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putControlL (const char *keyword, const int *array, int nelements)
{
  pc_frou_put_control_larray (keyword, array, &nelements);
}


void PCW::putControl  (const char *keyword, const char *array, int nelements)
{
  pc_frou_put_control_carray (keyword, array, &nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::putGui  (const char *keyword, const char *action, const int *array,
                   int nelements, int nchar)
{
  pc_frou_put_gui_iarray (keyword, action, array, &nelements, &nchar);
}


void PCW::putGui  (const char *keyword, const char *action, const float *array,
                   int nelements, int nchar, int ndec)
{
  pc_frou_put_gui_farray (keyword, action, array, &nelements, &nchar, &ndec);
}


void PCW::putGui (const char *keyword, const char *action, const double *array,
                   int nelements, int nchar, int ndec)
{
  pc_frou_put_gui_darray (keyword, action, array, &nelements, &nchar, &ndec);
}


void PCW::putGuiL (const char *keyword, const char *action, const int *array,
                   int nelements)
{
  pc_frou_put_gui_larray (keyword, action, array, &nelements);
}


void PCW::putGui  (const char *keyword, const char *action, const char *array,
                   int nelements)
{
  pc_frou_put_gui_carray (keyword, action, array, &nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::putGuiOnly  (const char *keyword, const int *array, int nelements,
                       int nchar)
{
  pc_frou_put_gui_only_iarray (keyword, array, &nelements, &nchar);
}


void PCW::putGuiOnly  (const char *keyword, const float *array, int nelements,
                       int nchar, int ndec)
{
  pc_frou_put_gui_only_farray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putGuiOnly  (const char *keyword, const double *array, int nelements,
                       int nchar, int ndec)
{
  pc_frou_put_gui_only_darray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putGuiOnlyL (const char *keyword, const int *array, int nelements)
{
  pc_frou_put_gui_only_larray (keyword, array, &nelements);
}


void PCW::putGuiOnly  (const char *keyword, const char *array, int nelements)
{
  pc_frou_put_gui_only_carray (keyword, array, &nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::putPdata  (const char *keyword, const int *array, int nelements,
                     int nchar)
{
  pc_frou_put_pdata_iarray (keyword, array, &nelements, &nchar);
}


void PCW::putPdata  (const char *keyword, const float *array, int nelements,
                     int nchar, int ndec)
{
  pc_frou_put_pdata_farray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putPdata  (const char *keyword, const double *array, int nelements,
                     int nchar, int ndec)
{
  pc_frou_put_pdata_darray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putPdataL (const char *keyword, const int *array, int nelements)
{
  pc_frou_put_pdata_larray (keyword, array, &nelements);
}


void PCW::putPdata  (const char *keyword, const char *array, int nelements)
{
  pc_frou_put_pdata_carray (keyword, array, &nelements, &NWORDS);
}

                  ////////////////////////////////


void PCW::putJdata  (const char *keyword, const int *array, int nelements,
                     int nchar)
{
  pc_frou_put_jdata_iarray (keyword, array, &nelements, &nchar);
}


void PCW::putJdata  (const char *keyword, const float *array, int nelements,
                     int nchar, int ndec)
{
  pc_frou_put_jdata_farray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putJdata  (const char *keyword, const double *array, int nelements,
                     int nchar, int ndec)
{
  pc_frou_put_jdata_darray (keyword, array, &nelements, &nchar, &ndec);
}


void PCW::putJdataL (const char *keyword, const int *array, int nelements)
{
  pc_frou_put_jdata_larray (keyword, array, &nelements);
}


void PCW::putJdata  (const char *keyword, const char *array, int nelements)
{
  pc_frou_put_jdata_carray (keyword, array, &nelements, &NWORDS);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::registerArrayNames (const char *keyword, const char *arrays,
                              int narrays)
{
  pc_frou_register_array_names (keyword, arrays, &narrays, &NWORDS);
}

                  ////////////////////////////////


void PCW::putOptions (const char *keyword, const int *options, int noptions,
                      int nchar)
{
  pc_frou_put_options_iscalar (keyword, options, &noptions, &nchar);
}


void PCW::putOptions (const char *keyword, const float *options, int noptions,
                      int nchar, int ndec)
{
  pc_frou_put_options_fscalar (keyword, options, &noptions, &nchar, &ndec);
}


void PCW::putOptions (const char *keyword, const double*options, int noptions,
                      int nchar, int ndec)
{
  pc_frou_put_options_dscalar (keyword, options, &noptions, &nchar, &ndec);
}


void PCW::putOptionsL  (const char *keyword, const int *options, int noptions)
{
  pc_frou_put_options_lscalar (keyword, options, &noptions);
}


void PCW::putOptions (const char *keyword, const char  *options, int noptions)
{
  pc_frou_put_options_cscalar (keyword, options, &noptions, &NWORDS);
}

                  ////////////////////////////////


void PCW::putOptionsA (const char *keyword, const int *options, int noptions,
                       int nchar)
{
  pc_frou_put_options_iarray (keyword, options, &noptions, &nchar);
}


void PCW::putOptionsA (const char *keyword, const float *options, int noptions,
                       int nchar, int ndec)
{
  pc_frou_put_options_farray (keyword, options, &noptions, &nchar, &ndec);
}


void PCW::putOptionsA (const char *keyword, const double*options, int noptions,
                       int nchar, int ndec)
{
  pc_frou_put_options_darray (keyword, options, &noptions, &nchar, &ndec);
}


void PCW::putOptionsAL (const char *keyword, const int *options, int noptions)
{
  pc_frou_put_options_larray (keyword, options, &noptions);
}


void PCW::putOptionsA (const char *keyword, const char  *options, int noptions)
{
  pc_frou_put_options_carray (keyword, options, &noptions, &NWORDS);
}

                  ////////////////////////////////


void PCW::putSensitiveFieldFlag  (const char *keyword, int sensitive)
{
  pc_frou_put_sns_field_flag    (keyword, &sensitive);
}


void PCW::putSensitiveArrayFlag  (const char *keyword, int sensitive)
{
  pc_frou_put_sns_array_flag    (keyword, &sensitive);
}


void PCW::putSensitiveArraysetFlag (const char *keyword, int sensitive)
{
  pc_frou_put_sns_arrayset_flag (keyword, &sensitive);
}


void PCW::putSensitiveScreenFlag (const char *keyword, int sensitive)
{
  pc_frou_put_sns_screen_flag   (keyword, &sensitive);
}

                  ////////////////////////////////


void PCW::putVisibleFlag   (const char *keyword, int visible)
{
  pc_frou_put_visible_flag (keyword, &visible);
}

                  ////////////////////////////////


void PCW::putMinsizeArray  (const char *keyword, int minsize)
{
  pc_frou_put_minsize_array    (keyword, &minsize);
}


void PCW::putMinsizeArrayset (const char *keyword, int minsize)
{
  pc_frou_put_minsize_arrayset (keyword, &minsize);
}


void PCW::putMaxsizeArray  (const char *keyword, int maxsize)
{
  pc_frou_put_maxsize_array    (keyword, &maxsize);
}


void PCW::putMaxsizeArrayset (const char *keyword, int maxsize)
{
  pc_frou_put_maxsize_arrayset (keyword, &maxsize);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


int PCW::numProcessCards ()
{
  return pc_frou_num_process_cards ();
}


int PCW::numGlobalCards  ()
{
  return pc_frou_num_global_cards  ();
}


int PCW::numControlCards ()
{
  return pc_frou_num_control_cards ();
}


int PCW::numPdataCards ()
{
  return pc_frou_num_pdata_cards   ();
}


int PCW::numJdataCards ()
{
  return pc_frou_num_jdata_cards   ();
}


int PCW::numGuiCards ()
{
  return pc_frou_num_gui_cards     ();
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::getProcessCards (int nsize, char *cards, int *ncards)
{
  char errmsg[LENGTH];
  pc_frou_get_process_cards (&nsize, cards, ncards, errmsg, &NWORDS);
}


void PCW::getGlobalCards  (int nsize, char *cards, int *ncards)
{
  char errmsg[LENGTH];
  pc_frou_get_global_cards  (&nsize, cards, ncards, errmsg, &NWORDS);
}


void PCW::getControlCards (int nsize, char *cards, int *ncards)
{
  char errmsg[LENGTH];
  pc_frou_get_control_cards (&nsize, cards, ncards, errmsg, &NWORDS);
}


void PCW::getPdataCards (int nsize, char *cards, int *ncards)
{
  char errmsg[LENGTH];
  pc_frou_get_pdata_cards   (&nsize, cards, ncards, errmsg, &NWORDS);
}


void PCW::getJdataCards (int nsize, char *cards, int *ncards)
{
  char errmsg[LENGTH];
  pc_frou_get_jdata_cards   (&nsize, cards, ncards, errmsg, &NWORDS);
}


void PCW::getGuiCards (int nsize, char *cards, int *ncards)
{
  char errmsg[LENGTH];
  pc_frou_get_gui_cards     (&nsize, cards, ncards, errmsg, &NWORDS);
}

                  ////////////////////////////////


void PCW::getProcessCard  (int icard, char *card)
{
  char errmsg[LENGTH];
  pc_frou_get_process_card (&icard, card, errmsg);
}


void PCW::getGlobalCard (int icard, char *card)
{
  char errmsg[LENGTH];
  pc_frou_get_global_card  (&icard, card, errmsg);
}


void PCW::getControlCard  (int icard, char *card)
{
  char errmsg[LENGTH];
  pc_frou_get_control_card (&icard, card, errmsg);
}


void PCW::getPdataCard  (int icard, char *card)
{
  char errmsg[LENGTH];
  pc_frou_get_pdata_card   (&icard, card, errmsg);
}


void PCW::getJdataCard  (int icard, char *card)
{
  char errmsg[LENGTH];
  pc_frou_get_jdata_card   (&icard, card, errmsg);
}


void PCW::getGuiCard  (int icard, char *card)
{
  char errmsg[LENGTH];
  pc_frou_get_gui_card     (&icard, card, errmsg);
}

                  ////////////////////////////////


void PCW::putProcessCards (const char *cards, int ncards)
{
  pc_frou_put_process_cards (cards, &ncards, &NWORDS);
}


void PCW::putGlobalCards  (const char *cards, int ncards)
{
  pc_frou_put_global_cards  (cards, &ncards, &NWORDS);
}


void PCW::putControlCards (const char *cards, int ncards)
{
  pc_frou_put_control_cards (cards, &ncards, &NWORDS);
}


void PCW::putPdataCards (const char *cards, int ncards)
{
  pc_frou_put_pdata_cards   (cards, &ncards, &NWORDS);
}


void PCW::putJdataCards (const char *cards, int ncards)
{
  pc_frou_put_jdata_cards   (cards, &ncards, &NWORDS);
}


void PCW::putGuiCards (const char *cards, int ncards)
{
  pc_frou_put_gui_cards     (cards, &ncards, &NWORDS);
}

                  ////////////////////////////////


void PCW::putProcessCard  (const char *card)
{
  pc_frou_put_process_card (card);
}


void PCW::putGlobalCard (const char *card)
{
  pc_frou_put_global_card  (card);
}


void PCW::putControlCard  (const char *card)
{
  pc_frou_put_control_card (card);
}


void PCW::putPdataCard  (const char *card)
{
  pc_frou_put_pdata_card   (card);
}


void PCW::putJdataCard  (const char *card)
{
  pc_frou_put_jdata_card   (card);
}


void PCW::putGuiCard  (const char *card)
{
  pc_frou_put_gui_card     (card);
}

                  ////////////////////////////////


void PCW::addProcessCard  (const char *card)
{
  pc_frou_add_process_card (card);
}


void PCW::addGlobalCard (const char *card)
{
  pc_frou_add_global_card  (card);
}


void PCW::addControlCard  (const char *card)
{
  pc_frou_add_control_card (card);
}


void PCW::addPdataCard  (const char *card)
{
  pc_frou_add_pdata_card   (card);
}


void PCW::addJdataCard  (const char *card)
{
  pc_frou_add_jdata_card   (card);
}


void PCW::addGuiCard  (const char *card)
{
  pc_frou_add_gui_card     (card);
}

                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////
                  ////////////////////////////////


void PCW::clearProcessCards ()
{
  pc_frou_clear_process_cards ();
}


void PCW::clearGlobalCards  ()
{
  pc_frou_clear_global_cards  ();
}


void PCW::clearControlCards ()
{
  pc_frou_clear_control_cards ();
}


void PCW::clearPdataCards ()
{
  pc_frou_clear_pdata_cards   ();
}


void PCW::clearJdataCards ()
{
  pc_frou_clear_jdata_cards   ();
}


void PCW::clearGuiCards ()
{
  pc_frou_clear_gui_cards     ();
}

                  ////////////////////////////////


int PCW::processKeywordPresent (const char *keyword)
{
  return pc_frou_process_keyword_present (keyword);
}


int PCW::globalKeywordPresent  (const char *keyword)
{
  return pc_frou_global_keyword_present  (keyword);
}


int PCW::controlKeywordPresent (const char *keyword)
{
  return pc_frou_control_keyword_present (keyword);
}


int PCW::pdataKeywordPresent (const char *keyword)
{
  return pc_frou_pdata_keyword_present   (keyword);
}


int PCW::jdataKeywordPresent (const char *keyword)
{
  return pc_frou_jdata_keyword_present   (keyword);
}


int PCW::guiActionPresent  (const char *keyword, const char *action)
{
  return pc_frou_gui_action_present      (keyword, action);
}

                  ////////////////////////////////


int PCW::numProcessKeywords  ()
{
  return pc_frou_num_process_keywords ();
}


int PCW::numGlobalKeywords ()
{
  return pc_frou_num_global_keywords  ();
}


int PCW::numControlKeywords  ()
{
  return pc_frou_num_control_keywords ();
}


int PCW::numPdataKeywords  ()
{
  return pc_frou_num_pdata_keywords   ();
}


int PCW::numJdataKeywords  ()
{
  return pc_frou_num_jdata_keywords   ();
}


int PCW::numGuiKeywords  ()
{
  return pc_frou_num_gui_keywords     ();
}

                  ////////////////////////////////


void PCW::getProcessKeyword (int indx, char *keyword)
{
  pc_frou_get_process_keyword (&indx, keyword);
}


void PCW::getGlobalKeyword  (int indx, char *keyword)
{
  pc_frou_get_global_keyword  (&indx, keyword);
}


void PCW::getControlKeyword (int indx, char *keyword)
{
  pc_frou_get_control_keyword (&indx, keyword);
}


void PCW::getPdataKeyword (int indx, char *keyword)
{
  pc_frou_get_pdata_keyword   (&indx, keyword);
}


void PCW::getJdataKeyword (int indx, char *keyword)
{
  pc_frou_get_jdata_keyword   (&indx, keyword);
}


void PCW::getGuiKeyword (int indx, char *keyword)
{
  pc_frou_get_gui_keyword     (&indx, keyword);
}


void PCW::getGuiAction  (int indx, char *action)
{
  pc_frou_get_gui_action      (&indx, action);
}

                  ////////////////////////////////


void PCW::removeProcessKeyword  (const char *keyword)
{
  pc_frou_remove_process_keyword (keyword);
}


void PCW::removeGlobalKeyword (const char *keyword)
{
  pc_frou_remove_global_keyword  (keyword);
}


void PCW::removeControlKeyword  (const char *keyword)
{
  pc_frou_remove_control_keyword (keyword);
}


void PCW::removePdataKeyword  (const char *keyword)
{
  pc_frou_remove_pdata_keyword   (keyword);
}


void PCW::removeJdataKeyword  (const char *keyword)
{
  pc_frou_remove_jdata_keyword   (keyword);
}


void PCW::removeGuiAction (const char *keyword, const char *action)
{
  pc_frou_remove_gui_action      (keyword, action);
}

//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
