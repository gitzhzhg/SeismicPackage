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
/****
!<CPS_v1 type="PRIMITIVE"/>
****/
//--------------------------- pjar_wrapper.cc ---------------------------//
//--------------------------- pjar_wrapper.cc ---------------------------//
//--------------------------- pjar_wrapper.cc ---------------------------//

    // other files are:  pjar_wrapper.hh  pjar.f90  pjar_frou.f90

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : PJAR_WRAPPER
! Category   : character
! Written    : 2001-05-24   by: Tom Stoeckley
! Revised    : 2001-05-24   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Parameter container using a CARDSETLIST object.
! Portability: See limitations below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! See the documentation in the Fortran PJAR file.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
! See the header file and the documentation in the Fortran PJAR file.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  2.
!  1. 2001-05-24  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! This code will work correctly only if the following data types match
! between Fortran and C:
!
!     Fortran integer           must match  C int
!     Fortran real              must match  C float
!     Fortran double precision  must match  C double
!
! These restrictions are accepted to make the code simpler.
! Whenever we start using a platform for which these restrictions are
! invalid, this object will assert.  In that case, the code must be
! enhanced according to the instructions in the c2f_interface header file.
!
!-------------------------------------------------------------------------------
!</portability_doc>
****/


//-------------------------- start of module ------------------------------//
//-------------------------- start of module ------------------------------//
//-------------------------- start of module ------------------------------//


char PJAR_WRAPPER_IDENT[100] =
"$Id: pjar_wrapper.cc,v 1.1 2002/07/16 21:34:33 wjdone Exp $";

#include "oprim/pjar_wrapper.hh"
#include "geom/grid_transform.hh"
#include "str.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//---------------------- fortran spelling adjustments ----------------------//
//---------------------- fortran spelling adjustments ----------------------//
//---------------------- fortran spelling adjustments ----------------------//


#if NEED_UNDERSCORE
#define pjar_frou_create                pjar_frou_create_
#define pjar_frou_delete                pjar_frou_delete_
#define pjar_frou_clear                 pjar_frou_clear_
#define pjar_frou_copy                  pjar_frou_copy_
#define pjar_frou_print                 pjar_frou_print_
#define pjar_frou_num_sections          pjar_frou_num_sections_
#define pjar_frou_find_section          pjar_frou_find_section_
#define pjar_frou_choose_section_indx   pjar_frou_choose_section_indx_
#define pjar_frou_choose_section_name   pjar_frou_choose_section_name_
#define pjar_frou_choose_no_section     pjar_frou_choose_no_section_
#define pjar_frou_get_secname           pjar_frou_get_secname_
#define pjar_frou_get_secnames          pjar_frou_get_secnames_
#define pjar_frou_status                pjar_frou_status_
#define pjar_frou_num_elements          pjar_frou_num_elements_
#define pjar_frou_nature                pjar_frou_nature_
#define pjar_frou_getg                  pjar_frou_getg_
#define pjar_frou_geti                  pjar_frou_geti_
#define pjar_frou_getf                  pjar_frou_getf_
#define pjar_frou_getd                  pjar_frou_getd_
#define pjar_frou_getl                  pjar_frou_getl_
#define pjar_frou_getc                  pjar_frou_getc_
#define pjar_frou_geti_element          pjar_frou_geti_element_
#define pjar_frou_getf_element          pjar_frou_getf_element_
#define pjar_frou_getd_element          pjar_frou_getd_element_
#define pjar_frou_getl_element          pjar_frou_getl_element_
#define pjar_frou_getc_element          pjar_frou_getc_element_
#define pjar_frou_getii                 pjar_frou_getii_
#define pjar_frou_getff                 pjar_frou_getff_
#define pjar_frou_getdd                 pjar_frou_getdd_
#define pjar_frou_getll                 pjar_frou_getll_
#define pjar_frou_getcc                 pjar_frou_getcc_
#define pjar_frou_putg                  pjar_frou_putg_
#define pjar_frou_puti                  pjar_frou_puti_
#define pjar_frou_putf                  pjar_frou_putf_
#define pjar_frou_putd                  pjar_frou_putd_
#define pjar_frou_putc                  pjar_frou_putc_
#define pjar_frou_putl                  pjar_frou_putl_
#define pjar_frou_puti_element          pjar_frou_puti_element_
#define pjar_frou_putf_element          pjar_frou_putf_element_
#define pjar_frou_putd_element          pjar_frou_putd_element_
#define pjar_frou_putc_element          pjar_frou_putc_element_
#define pjar_frou_putl_element          pjar_frou_putl_element_
#define pjar_frou_insert_element        pjar_frou_insert_element_
#define pjar_frou_remove_element        pjar_frou_remove_element_
#define pjar_frou_clear_buffer          pjar_frou_clear_buffer_
#define pjar_frou_find                  pjar_frou_find_        
#define pjar_frou_find_add              pjar_frou_find_add_
#define pjar_frou_putii                 pjar_frou_putii_
#define pjar_frou_putff                 pjar_frou_putff_
#define pjar_frou_putdd                 pjar_frou_putdd_
#define pjar_frou_putcc                 pjar_frou_putcc_
#define pjar_frou_putll                 pjar_frou_putll_
#define pjar_frou_num_cards             pjar_frou_num_cards_
#define pjar_frou_clear_cards           pjar_frou_clear_cards_
#define pjar_frou_get_cards             pjar_frou_get_cards_
#define pjar_frou_put_cards             pjar_frou_put_cards_
#define pjar_frou_get_card              pjar_frou_get_card_
#define pjar_frou_add_card              pjar_frou_add_card_
#define pjar_frou_new_card              pjar_frou_new_card_
#define pjar_frou_keyword_present       pjar_frou_keyword_present_
#define pjar_frou_num_keywords          pjar_frou_num_keywords_
#define pjar_frou_get_keyword           pjar_frou_get_keyword_
#define pjar_frou_remove_keyword        pjar_frou_remove_keyword_
#elif NEED_CAPITALS
#define pjar_frou_create                PJAR_FROU_CREATE
#define pjar_frou_delete                PJAR_FROU_DELETE
#define pjar_frou_clear                 PJAR_FROU_CLEAR
#define pjar_frou_copy                  PJAR_FROU_COPY
#define pjar_frou_print                 PJAR_FROU_PRINT
#define pjar_frou_num_sections          PJAR_FROU_NUM_SECTIONS
#define pjar_frou_find_section          PJAR_FROU_FIND_SECTION
#define pjar_frou_choose_section_indx   PJAR_FROU_CHOOSE_SECTION_INDX
#define pjar_frou_choose_section_name   PJAR_FROU_CHOOSE_SECTION_NAME
#define pjar_frou_choose_no_section     PJAR_FROU_CHOOSE_NO_SECTION
#define pjar_frou_get_secname           PJAR_FROU_GET_SECNAME
#define pjar_frou_get_secnames          PJAR_FROU_GET_SECNAMES
#define pjar_frou_status                PJAR_FROU_STATUS
#define pjar_frou_num_elements          PJAR_FROU_NUM_ELEMENTS
#define pjar_frou_nature                PJAR_FROU_NATURE
#define pjar_frou_getg                  PJAR_FROU_GETG
#define pjar_frou_geti                  PJAR_FROU_GETI
#define pjar_frou_getf                  PJAR_FROU_GETF
#define pjar_frou_getd                  PJAR_FROU_GETD
#define pjar_frou_getl                  PJAR_FROU_GETL
#define pjar_frou_getc                  PJAR_FROU_GETC
#define pjar_frou_geti_element          PJAR_FROU_GETI_ELEMENT
#define pjar_frou_getf_element          PJAR_FROU_GETF_ELEMENT
#define pjar_frou_getd_element          PJAR_FROU_GETD_ELEMENT
#define pjar_frou_getl_element          PJAR_FROU_GETL_ELEMENT
#define pjar_frou_getc_element          PJAR_FROU_GETC_ELEMENT
#define pjar_frou_getii                 PJAR_FROU_GETII
#define pjar_frou_getff                 PJAR_FROU_GETFF
#define pjar_frou_getdd                 PJAR_FROU_GETDD
#define pjar_frou_getll                 PJAR_FROU_GETLL
#define pjar_frou_getcc                 PJAR_FROU_GETCC
#define pjar_frou_putg                  PJAR_FROU_PUTG
#define pjar_frou_puti                  PJAR_FROU_PUTI
#define pjar_frou_putf                  PJAR_FROU_PUTF
#define pjar_frou_putd                  PJAR_FROU_PUTD
#define pjar_frou_putc                  PJAR_FROU_PUTC
#define pjar_frou_putl                  PJAR_FROU_PUTL
#define pjar_frou_puti_element          PJAR_FROU_PUTI_ELEMENT
#define pjar_frou_putf_element          PJAR_FROU_PUTF_ELEMENT
#define pjar_frou_putd_element          PJAR_FROU_PUTD_ELEMENT
#define pjar_frou_putc_element          PJAR_FROU_PUTC_ELEMENT
#define pjar_frou_putl_element          PJAR_FROU_PUTL_ELEMENT
#define pjar_frou_insert_element        PJAR_FROU_INSERT_ELEMENT
#define pjar_frou_remove_element        PJAR_FROU_REMOVE_ELEMENT
#define pjar_frou_clear_buffer          PJAR_FROU_CLEAR_BUFFER
#define pjar_frou_find                  PJAR_FROU_FIND         
#define pjar_frou_find_add              PJAR_FROU_FIND_ADD
#define pjar_frou_putii                 PJAR_FROU_PUTII
#define pjar_frou_putff                 PJAR_FROU_PUTFF
#define pjar_frou_putdd                 PJAR_FROU_PUTDD
#define pjar_frou_putcc                 PJAR_FROU_PUTCC
#define pjar_frou_putll                 PJAR_FROU_PUTLL
#define pjar_frou_num_cards             PJAR_FROU_NUM_CARDS
#define pjar_frou_clear_cards           PJAR_FROU_CLEAR_CARDS
#define pjar_frou_get_cards             PJAR_FROU_GET_CARDS
#define pjar_frou_put_cards             PJAR_FROU_PUT_CARDS
#define pjar_frou_get_card              PJAR_FROU_GET_CARD
#define pjar_frou_add_card              PJAR_FROU_ADD_CARD
#define pjar_frou_new_card              PJAR_FROU_NEW_CARD
#define pjar_frou_keyword_present       PJAR_FROU_KEYWORD_PRESENT
#define pjar_frou_num_keywords          PJAR_FROU_NUM_KEYWORDS
#define pjar_frou_get_keyword           PJAR_FROU_GET_KEYWORD
#define pjar_frou_remove_keyword        PJAR_FROU_REMOVE_KEYWORD
#endif


//--------------------------- helpful macros -------------------------------//
//--------------------------- helpful macros -------------------------------//
//--------------------------- helpful macros -------------------------------//


#define FPTR      F90Pointer    *fpoint
#define KEY       const char    *keyword
#define NSECTIONS       int     *nsections
#define NELEMENTS       int     *nelements
#define INDX            int     *indx
#define NCARDS          int     *ncards
#define NALLOC          int     *nalloc
#define NWORDS    const int     *nwords

static int nwords = 51;     // enough for 201 characters.


//-------------------------- fortran prototypes ----------------------------//
//-------------------------- fortran prototypes ----------------------------//
//-------------------------- fortran prototypes ----------------------------//


extern "C"
{
  void pjar_frou_create (FPTR);
  void pjar_frou_delete (FPTR);
  void pjar_frou_clear  (FPTR);
  void pjar_frou_copy   (const F90Pointer *fpoint1, F90Pointer *fpoint2);
  void pjar_frou_print  (FPTR);

  int  pjar_frou_num_sections        (const FPTR);
  int  pjar_frou_find_section        (FPTR, const char *name);
  void pjar_frou_choose_section_indx (FPTR, const int  *indx);
  void pjar_frou_choose_section_name (FPTR, const char *name);
  void pjar_frou_choose_no_section   (FPTR);

  void pjar_frou_get_secname  (const FPTR, char *secname);
  void pjar_frou_get_secnames (const FPTR, char *secnames,
                                                   NSECTIONS, NALLOC, NWORDS);
  void pjar_frou_status       (const FPTR, char *errmsg);

  int  pjar_frou_num_elements (FPTR, KEY);
  int  pjar_frou_nature       (FPTR, KEY);

  void pjar_frou_getg  (FPTR, KEY, F90Pointer *scalar);
  void pjar_frou_geti  (FPTR, KEY, int        *scalar);
  void pjar_frou_getf  (FPTR, KEY, float      *scalar);
  void pjar_frou_getd  (FPTR, KEY, double     *scalar);
  void pjar_frou_getl  (FPTR, KEY, int        *scalar);
  void pjar_frou_getc  (FPTR, KEY, char       *scalar);

  void pjar_frou_geti_element  (FPTR, KEY, INDX, int        *element);
  void pjar_frou_getf_element  (FPTR, KEY, INDX, float      *element);
  void pjar_frou_getd_element  (FPTR, KEY, INDX, double     *element);
  void pjar_frou_getl_element  (FPTR, KEY, INDX, int        *element);
  void pjar_frou_getc_element  (FPTR, KEY, INDX, char       *element);

  void pjar_frou_getii (FPTR, KEY, int     *array, NELEMENTS, NALLOC);
  void pjar_frou_getff (FPTR, KEY, float   *array, NELEMENTS, NALLOC);
  void pjar_frou_getdd (FPTR, KEY, double  *array, NELEMENTS, NALLOC);
  void pjar_frou_getll (FPTR, KEY, int     *array, NELEMENTS, NALLOC);
  void pjar_frou_getcc (FPTR, KEY, char    *array, NELEMENTS, NALLOC, NWORDS);

  void pjar_frou_putg  (FPTR, KEY, const F90Pointer *scalar);
  void pjar_frou_puti  (FPTR, KEY, const    int     *scalar);
  void pjar_frou_putf  (FPTR, KEY, const    float   *scalar);
  void pjar_frou_putd  (FPTR, KEY, const    double  *scalar);
  void pjar_frou_putc  (FPTR, KEY, const    char    *scalar);
  void pjar_frou_putl  (FPTR, KEY, const    int     *scalar);

  void pjar_frou_puti_element  (FPTR, KEY, INDX, const    int     *element);
  void pjar_frou_putf_element  (FPTR, KEY, INDX, const    float   *element);
  void pjar_frou_putd_element  (FPTR, KEY, INDX, const    double  *element);
  void pjar_frou_putc_element  (FPTR, KEY, INDX, const    char    *element);
  void pjar_frou_putl_element  (FPTR, KEY, INDX, const    int     *element);

  void pjar_frou_insert_element  (FPTR, KEY, INDX);
  void pjar_frou_remove_element  (FPTR, KEY, INDX);
  void pjar_frou_clear_buffer    (FPTR, KEY);

  int  pjar_frou_find            (FPTR, KEY, const char *element);
  int  pjar_frou_find_add        (FPTR, KEY, const char *element);

  void pjar_frou_putii (FPTR, KEY, const int    *array, const NELEMENTS);
  void pjar_frou_putff (FPTR, KEY, const float  *array, const NELEMENTS);
  void pjar_frou_putdd (FPTR, KEY, const double *array, const NELEMENTS);
  void pjar_frou_putcc (FPTR, KEY, const char   *array, const NELEMENTS,NWORDS);
  void pjar_frou_putll (FPTR, KEY, const int    *array, const NELEMENTS);

  int  pjar_frou_num_cards   (FPTR);
  void pjar_frou_clear_cards (FPTR);
  void pjar_frou_get_cards   (FPTR,       char *cards, NCARDS, NALLOC, NWORDS);
  void pjar_frou_put_cards   (FPTR, const char *cards, const NCARDS, NWORDS,
                                    const char *progname);

  void pjar_frou_get_card  (FPTR, const int  *indx, char *card);
  void pjar_frou_add_card  (FPTR, const char *card);
  void pjar_frou_new_card  (FPTR, const char *progname);

  int  pjar_frou_keyword_present (FPTR, KEY);
  int  pjar_frou_num_keywords    (FPTR);
  void pjar_frou_get_keyword     (FPTR, const int *indx, KEY);
  void pjar_frou_remove_keyword  (FPTR, KEY);
}


//-------------------- create and delete and clear -------------------------//
//-------------------- create and delete and clear -------------------------//
//-------------------- create and delete and clear -------------------------//


PjarWrapper::PjarWrapper()
{
  assert(sizeof(INTEGER) == sizeof(int   ));
  assert(sizeof(REAL   ) == sizeof(float ));
  assert(sizeof(DOUBLE ) == sizeof(double));
  memset(&_fpoint, 0, sizeof(_fpoint));
  pjar_frou_create (&_fpoint);
}



PjarWrapper::~PjarWrapper()
{
  pjar_frou_delete (&_fpoint);
}



void PjarWrapper::clear()
{
  pjar_frou_clear (&_fpoint);
}



void PjarWrapper::copy(const PjarWrapper *pjar)

{
  pjar_frou_copy (&pjar->_fpoint, &_fpoint);
}



void PjarWrapper::print()
{
  pjar_frou_print (&_fpoint);
}



//------------------------ choose an active section ------------------------//
//------------------------ choose an active section ------------------------//
//------------------------ choose an active section ------------------------//


int    PjarWrapper::numSections      ()  const
{
  return pjar_frou_num_sections(&_fpoint);
}


int    PjarWrapper::findSection       (const char *secname)
{
  return pjar_frou_find_section (&_fpoint, secname);
}


void   PjarWrapper::chooseSection       (int indx)
{
  pjar_frou_choose_section_indx (&_fpoint, &indx);
}


void   PjarWrapper::chooseSection      (const char *secname)
{
  pjar_frou_choose_section_name (&_fpoint, secname);
}


void   PjarWrapper::chooseNoSection ()
{
  pjar_frou_choose_no_section (&_fpoint);
}


void   PjarWrapper::getSecname (char *secname)  const
{
  pjar_frou_get_secname (&_fpoint, secname);
}


void   PjarWrapper::getSecnames (char **secnames, int *nsections)  const
{
  int   nalloc = pjar_frou_num_sections(&_fpoint);
  int   nbytes = nwords * sizeof(int);
  char *buffer = new char [nalloc * nbytes + 1];
  pjar_frou_get_secnames (&_fpoint, buffer, nsections, &nalloc, &nwords);
  for(int i = 0; i < *nsections; i++)
       {
       secnames[i] = str_newstr(&buffer[nbytes*i]);
       }
  delete [] buffer;
}


//---------------------------- status ---------------------------------------//
//---------------------------- status ---------------------------------------//
//---------------------------- status ---------------------------------------//


void   PjarWrapper::status (char *errmsg)  const
{
  pjar_frou_status (&_fpoint, errmsg);
}


//------------------------ num elements -----------------------------------//
//------------------------ num elements -----------------------------------//
//------------------------ num elements -----------------------------------//


int    PjarWrapper::numElements   (const char *keyword)
{
  return pjar_frou_num_elements (&_fpoint, keyword);
}


//--------------------------- nature --------------------------------------//
//--------------------------- nature --------------------------------------//
//--------------------------- nature --------------------------------------//


int    PjarWrapper::nature        (const char *keyword)
{
  return pjar_frou_nature (&_fpoint, keyword);
}


//---------------------- get scalars --------------------------------------//
//---------------------- get scalars --------------------------------------//
//---------------------- get scalars --------------------------------------//


void   PjarWrapper::getScalar  (const char *keyword, GridTransform *scalar)
{
  F90Pointer *scalar9 = scalar->getFpoint();
  pjar_frou_getg  (&_fpoint, keyword, scalar9);
}



void   PjarWrapper::getScalar  (const char *keyword, int    *scalar)
{
  pjar_frou_geti  (&_fpoint, keyword, scalar);
}


void   PjarWrapper::getScalar  (const char *keyword, float  *scalar)
{
  pjar_frou_getf  (&_fpoint, keyword, scalar);
}


void   PjarWrapper::getScalar  (const char *keyword, double *scalar)
{
  pjar_frou_getd  (&_fpoint, keyword, scalar);
}


void   PjarWrapper::getScalar  (const char *keyword, char   *scalar)
{
  pjar_frou_getc  (&_fpoint, keyword, scalar);
}


void   PjarWrapper::getLogical (const char *keyword, int    *scalar)
{
  pjar_frou_getl  (&_fpoint, keyword, scalar);
}



//---------------------- get elements --------------------------------------//
//---------------------- get elements --------------------------------------//
//---------------------- get elements --------------------------------------//


void   PjarWrapper::getElement (const char *keyword, int indx, int    *element)
{
  pjar_frou_geti_element (&_fpoint, keyword, &indx, element);
}


void   PjarWrapper::getElement (const char *keyword, int indx, float  *element)
{
  pjar_frou_getf_element (&_fpoint, keyword, &indx, element);
}


void   PjarWrapper::getElement (const char *keyword, int indx, double *element)
{
  pjar_frou_getd_element (&_fpoint, keyword, &indx, element);
}


void   PjarWrapper::getElement (const char *keyword, int indx, char   *element)
{
  pjar_frou_getc_element (&_fpoint, keyword, &indx, element);
}


void   PjarWrapper::getLogical (const char *keyword, int indx, int    *element)
{
  pjar_frou_getl_element (&_fpoint, keyword, &indx, element);
}



//----------------------- get arrays -----------------------------------//
//----------------------- get arrays -----------------------------------//
//----------------------- get arrays -----------------------------------//


void   PjarWrapper::getArray
                       (const char *keyword, int    *array, int *nelements)
{
  int nalloc = pjar_frou_num_elements(&_fpoint, keyword);
  pjar_frou_getii (&_fpoint, keyword, array, nelements, &nalloc);
}


void   PjarWrapper::getArray
                       (const char *keyword, float  *array, int *nelements)
{
  int nalloc = pjar_frou_num_elements(&_fpoint, keyword);
  pjar_frou_getff (&_fpoint, keyword, array, nelements, &nalloc);
}


void   PjarWrapper::getArray
                       (const char *keyword, double *array, int *nelements)
{
  int nalloc = pjar_frou_num_elements(&_fpoint, keyword);
  pjar_frou_getdd (&_fpoint, keyword, array, nelements, &nalloc);
}


void   PjarWrapper::getArray
                       (const char *keyword, char  **array, int *nelements)
{
  int   nalloc = pjar_frou_num_elements(&_fpoint, keyword);
  int   nbytes = nwords * sizeof(int);
  char *buffer = new char [nalloc * nbytes + 1];
  pjar_frou_getcc (&_fpoint, keyword, buffer, nelements, &nalloc, &nwords);
  for(int i = 0; i < *nelements; i++)
       {
       array[i] = str_newstr(&buffer[nbytes*i]);
       }
  delete [] buffer;
}


void   PjarWrapper::getLogical
                       (const char *keyword, int    *array, int *nelements)
{
  int nalloc = pjar_frou_num_elements(&_fpoint, keyword);
  pjar_frou_getll (&_fpoint, keyword, array, nelements, &nalloc);
}



//------------------------- put scalars -----------------------------------//
//------------------------- put scalars -----------------------------------//
//------------------------- put scalars -----------------------------------//


void   PjarWrapper::putScalar
                      (const char *keyword, const GridTransform *scalar)
{
  if(!scalar) return;
  const F90Pointer *scalar9 = scalar->getConstFpoint();
  pjar_frou_putg  (&_fpoint, keyword, scalar9);
}



void   PjarWrapper::putScalar  (const char *keyword, int         scalar)
{
  pjar_frou_puti  (&_fpoint, keyword, &scalar);
}


void   PjarWrapper::putScalar  (const char *keyword, float       scalar)
{
  pjar_frou_putf  (&_fpoint, keyword, &scalar);
}


void   PjarWrapper::putScalar  (const char *keyword, double      scalar)
{
  pjar_frou_putd  (&_fpoint, keyword, &scalar);
}


void   PjarWrapper::putScalar  (const char *keyword, const char *scalar)
{
  if(!scalar) return;
  pjar_frou_putc  (&_fpoint, keyword,  scalar);
}


void   PjarWrapper::putLogical (const char *keyword, int         scalar)
{
  pjar_frou_putl  (&_fpoint, keyword, &scalar);
}



//---------------------- put elements --------------------------------------//
//---------------------- put elements --------------------------------------//
//---------------------- put elements --------------------------------------//


void   PjarWrapper::putElement (const char *keyword, int indx, int    element)
{
  pjar_frou_puti_element (&_fpoint, keyword, &indx, &element);
}


void   PjarWrapper::putElement (const char *keyword, int indx, float  element)
{
  pjar_frou_putf_element (&_fpoint, keyword, &indx, &element);
}


void   PjarWrapper::putElement (const char *keyword, int indx, double element)
{
  pjar_frou_putd_element (&_fpoint, keyword, &indx, &element);
}


void   PjarWrapper::putElement
                          (const char *keyword, int indx, const char *element)
{
  if(!element) return;
  pjar_frou_putc_element (&_fpoint, keyword, &indx, element);
}


void   PjarWrapper::putLogical (const char *keyword, int indx, int    element)
{
  pjar_frou_putl_element (&_fpoint, keyword, &indx, &element);
}



//------------------------- access buffer ------------------------------//
//------------------------- access buffer ------------------------------//
//------------------------- access buffer ------------------------------//


void   PjarWrapper::insertElement (const char *keyword, int indx)
{
  pjar_frou_insert_element (&_fpoint, keyword, &indx);
}


void   PjarWrapper::removeElement (const char *keyword, int indx)
{
  pjar_frou_remove_element (&_fpoint, keyword, &indx);
}


void   PjarWrapper::clearBuffer (const char *keyword)
{
  pjar_frou_clear_buffer (&_fpoint, keyword);
}


//---------------------------- find ------------------------------------//
//---------------------------- find ------------------------------------//
//---------------------------- find ------------------------------------//


int  PjarWrapper::find (const char *keyword, const char *element)
{
  return pjar_frou_find (&_fpoint, keyword, element);
}


//---------------------------- find add --------------------------------//
//---------------------------- find add --------------------------------//
//---------------------------- find add --------------------------------//


int  PjarWrapper::findAdd (const char *keyword, const char *element)
{
  return pjar_frou_find_add (&_fpoint, keyword, element);
}


//------------------------- put arrays ---------------------------------//
//------------------------- put arrays ---------------------------------//
//------------------------- put arrays ---------------------------------//


void   PjarWrapper::putArray
               (const char *keyword, const int    *array, int nelements)
{
  pjar_frou_putii (&_fpoint, keyword, array,   &nelements);
}


void   PjarWrapper::putArray
               (const char *keyword, const float  *array, int nelements)
{
  pjar_frou_putff (&_fpoint, keyword, array,   &nelements);
}


void   PjarWrapper::putArray
               (const char *keyword, const double *array, int nelements)
{
  pjar_frou_putdd (&_fpoint, keyword, array,   &nelements);
}


void   PjarWrapper::putArray
               (const char *keyword, char * const *array, int nelements)
{
  int   nbytes = nwords * sizeof(int);
  char *buffer = new char [nelements * nbytes + 1];
  for(int i = 0; i < nelements; i++)
       {
       strcpy(&buffer[nbytes*i], array[i]);
       }
  pjar_frou_putcc (&_fpoint, keyword, buffer, &nelements, &nwords);
  delete [] buffer;
}


void   PjarWrapper::putLogical
               (const char *keyword, const int    *array, int nelements)
{
  pjar_frou_putll (&_fpoint, keyword, array,   &nelements);
}



//------------------------ get and put data cards -------------------------//
//------------------------ get and put data cards -------------------------//
//------------------------ get and put data cards -------------------------//


int    PjarWrapper::numCards ()
{
  return pjar_frou_num_cards (&_fpoint);
}


void   PjarWrapper::clearCards ()
{
  pjar_frou_clear_cards (&_fpoint);
}


void   PjarWrapper::getCards (char **cards, int *ncards)
{
  int   nalloc = pjar_frou_num_cards(&_fpoint);
  int   nbytes = nwords * sizeof(int);
  char *buffer = new char [nalloc * nbytes + 1];
  pjar_frou_get_cards (&_fpoint, buffer, ncards, &nalloc, &nwords);
  for(int i = 0; i < *ncards; i++)
       {
       cards[i] = str_newstr(&buffer[nbytes*i]);
       }
  delete [] buffer;
}


void   PjarWrapper::putCards
              (char * const *cards, int ncards, const char *progname)
{
  assert(progname);
  int   nbytes = nwords * sizeof(int);
  char *buffer = new char [ncards * nbytes + 1];
  for(int i = 0; i < ncards; i++)
       {
       strcpy(&buffer[nbytes*i], cards[i]);
       }
  pjar_frou_put_cards (&_fpoint, buffer, &ncards, &nwords, progname);
  delete [] buffer;
}



void   PjarWrapper::getCard  (int indx, char *card)
{
  pjar_frou_get_card  (&_fpoint, &indx, card);

}


void   PjarWrapper::addCard  (const char *card)
{
  pjar_frou_add_card  (&_fpoint, card);
}


void   PjarWrapper::newCard  (const char *progname)
{
  pjar_frou_new_card  (&_fpoint, progname);
}



//------------------------ get keyword information -----------------------//
//------------------------ get keyword information -----------------------//
//------------------------ get keyword information -----------------------//


int    PjarWrapper::keywordPresent           (const char *keyword)
{
  return pjar_frou_keyword_present (&_fpoint, keyword);
}


int    PjarWrapper::numKeywords    ()
{
  return pjar_frou_num_keywords    (&_fpoint);
}


void   PjarWrapper::getKeyword     (int indx,       char *keyword)
{
  pjar_frou_get_keyword     (&_fpoint, &indx, keyword);
}


void   PjarWrapper::removeKeyword            (const char *keyword)
{
  if(keywordPresent(keyword)) pjar_frou_remove_keyword (&_fpoint, keyword);
  ///// The above conditional test should be put into cardset.f90.
  ///// This test avoids unnecessarily updating the parameters (and
  ///// potentially causing the datacards to subsequently be recreated
  ///// from the parameters) in the event that the keyword is not present.
  ///// This is important for the following case:
  /////  (1) The pickle jar originally contains data cards which are not
  /////       keyword encoded.
  /////  (2) The "encoding" parameter is requested, causing datacards to be
  /////       converted to parameters, and nil to be returned since the
  /////       parameter is not present.
  /////  (3) Subsequently attempting to remove the keyword will then cause
  /////       the original datacards to become out of date, even though
  /////       they are not really out of date because the parameter was not
  /////       present in the first place.
}



//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

