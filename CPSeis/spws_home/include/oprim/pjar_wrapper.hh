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
!<CPS_v1 type="HEADER_FILE"/>
****/
//--------------------------- pjar_wrapper.hh --------------------------//
//--------------------------- pjar_wrapper.hh --------------------------//
//--------------------------- pjar_wrapper.hh --------------------------//

    // other files are:  pjar_wrapper.cc  pjar.f90  pjar_frou.f90

/****


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  2.
!  1. 2001-05-24  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//


#ifndef _PJAR_WRAPPER_HH_
#define _PJAR_WRAPPER_HH_


#include "c2f_interface.h"


class PjarWrapper
{

//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//

public:   // these must be the same as in parameter.f90.

  enum { NATURE_MISSING = 1, NATURE_SCALAR = 2, NATURE_ARRAY = 3 };

private:

  F90Pointer  _fpoint;

//----------------------------- functions ----------------------------------//
//----------------------------- functions ----------------------------------//
//----------------------------- functions ----------------------------------//

public:

           PjarWrapper();
  virtual ~PjarWrapper();
  void     clear      ();
  void     copy       (const PjarWrapper *pjar);
  void     print      ();

        F90Pointer *getFpoint()              { return &_fpoint; }
  const F90Pointer *getConstFpoint()  const  { return &_fpoint; }

  int    numSections     ()  const;
  int    findSection     (const char *secname);
  void   chooseSection   (int indx);
  void   chooseSection   (const char *secname);
  void   chooseNoSection ();
  void   getSecname      (char *secname)  const;
  void   getSecnames     (char **secnames, int *nsections)  const;

  void   status        (char *errmsg)  const;

  int    numElements   (const char *keyword);
  int    nature        (const char *keyword);   // returns above enums.

  void   getScalar (const char *keyword,       class GridTransform *scalar);
  void   getScalar (const char *keyword,       int                 *scalar);
  void   getScalar (const char *keyword,       float               *scalar);
  void   getScalar (const char *keyword,       double              *scalar);
  void   getScalar (const char *keyword,       char                *scalar);
  void   getLogical(const char *keyword,       int                 *scalar);

  void   putScalar (const char *keyword, const class GridTransform *scalar);
  void   putScalar (const char *keyword,       int                  scalar);
  void   putScalar (const char *keyword,       float                scalar);
  void   putScalar (const char *keyword,       double               scalar);
  void   putScalar (const char *keyword, const char                *scalar);
  void   putLogical(const char *keyword,       int                  scalar);

  void   getElement(const char *keyword, int indx,       int     *element);
  void   getElement(const char *keyword, int indx,       float   *element);
  void   getElement(const char *keyword, int indx,       double  *element);
  void   getElement(const char *keyword, int indx,       char    *element);
  void   getLogical(const char *keyword, int indx,       int     *element);

  void   putElement(const char *keyword, int indx,       int      element);
  void   putElement(const char *keyword, int indx,       float    element);
  void   putElement(const char *keyword, int indx,       double   element);
  void   putElement(const char *keyword, int indx, const char    *element);
  void   putLogical(const char *keyword, int indx,       int      element);

  void   insertElement (const char *keyword, int indx);
  void   removeElement (const char *keyword, int indx);
  void   clearBuffer   (const char *keyword);

  int    find      (const char *keyword, const char *element);
  int    findAdd   (const char *keyword, const char *element);

  void   getArray  (const char *keyword, int    *array, int *nelements);
  void   getArray  (const char *keyword, float  *array, int *nelements);
  void   getArray  (const char *keyword, double *array, int *nelements);
  void   getArray  (const char *keyword, char  **array, int *nelements);
  void   getLogical(const char *keyword, int    *array, int *nelements);

  void   putArray  (const char *keyword, const int    *array, int  nelements);
  void   putArray  (const char *keyword, const float  *array, int  nelements);
  void   putArray  (const char *keyword, const double *array, int  nelements);
  void   putArray  (const char *keyword, char * const *array, int  nelements);
  void   putLogical(const char *keyword, const int    *array, int  nelements);

  int    numCards  ();
  void   clearCards();
  void   getCards  (char **cards, int *ncards);
  void   putCards  (char * const *cards, int ncards, const char *progname);

  void   getCard   (int indx,       char *card);
  void   addCard   (          const char *card);
  void   newCard   (const char *progname);

  int    keywordPresent           (const char *keyword);   // returns T or F.
  int    numKeywords    ();
  void   getKeyword     (int indx,       char *keyword);
  void   removeKeyword            (const char *keyword);


  // Warning: The functions in column 1 below must be called before the
  // functions in column2 so that the user can allocate sufficient memory
  // for the array returned from the functions in column 2:
  //
  //     numSections()            getSecnames(secnames, nsections)
  //     numElements(keyword)     getArray   (keyword, array, nelements)
  //     numElements(keyword)     getLogical (keyword, array, nelements)
  //     numCards   ()            getCards   (cards, ncards)
  //
  // Note: Arrays of strings input to the following functions must be
  // arrays of NELEMENTS or NCARDS pointers to char:
  //
  //          putArray    putCards
  //
  // Note: Arrays of strings returned from the following functions must
  // have previously been allocated  to NELEMENTS or NCARDS or NSECTIONS
  // pointers to char.  The following functions will then allocate sufficient
  // memory for each string returned by calling str_newstr.  The user is
  // responsible for freeing each string when no longer needed.
  //
  //          getArray    getCards   getSecnames


//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//

};

#endif

//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
