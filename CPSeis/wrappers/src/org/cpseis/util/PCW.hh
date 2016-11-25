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
//------------------------------- PCW.hh ---------------------------------//
//------------------------------- PCW.hh ---------------------------------//
//------------------------------- PCW.hh ---------------------------------//

// This class is a C++ wrapper around the parameter cache.

// The name of this class stands for "parameter cache wrapper".
// The name of this class is short because all functions are static.
// Functions ending in L use logical (true/false) variables.
// The argument n is short for nelements.
//
// Arrays of strings must provide exactly PCW::LENGTH bytes
// of space for each string array element.  Each string array
// element in the array must be null-terminated as usual.

//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//

#ifndef _PCW_HH_
#define _PCW_HH_

#include "c2f_interface.h"
#include "named_constants.h"

class GTW;

//--------------------------- define macros ------------------------------//
//--------------------------- define macros ------------------------------//
//--------------------------- define macros ------------------------------//

#define KEY      const char *keyword
#define ACT      const char *action
#define NCHAR    int nchar = 0
#define NDEC     int ndec = 99

//------------------------- start of class --------------------------------//
//------------------------- start of class --------------------------------//
//------------------------- start of class --------------------------------//

class PCW
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//

public:

   enum { FRONTEND         =   1 };    // update state.
   enum { GUI              =   2 };    // update state.
   enum { BACKEND          =   3 };    // update state.
   enum { EXECUTE          =   4 };    // update state.
   enum { QUICK            =   5 };    // update state.
   enum { BACKEND_NO_EXEC  =   6 };    // update state

   enum { INSERT           =   1 };    // array element action.
   enum { REMOVE           =   2 };    // array element action.
   enum { MODIFY           =   3 };    // array element action.
   enum { NOACTION         =   4 };    // array element action.

   enum { MISSING          =   1 };    // nature of parameter.
   enum { SCALAR           =   2 };    // nature of parameter.
   enum { ARRAY            =   3 };    // nature of parameter.

   enum { TYPE_INTEGER     =   1 };    // variable type of parameter.
   enum { TYPE_FLOAT       =   2 };    // variable type of parameter.
   enum { TYPE_DOUBLE      =   3 };    // variable type of parameter.
   enum { TYPE_STRING      =   4 };    // variable type of parameter.
   enum { TYPE_LOGICAL     =   5 };    // variable type of parameter.
   enum { TYPE_GTW         =   6 };    // variable type of parameter.

/*
   enum { DATACARD_LENGTH  =  80 };    // datacard length.
   enum { DATACARD_PADDING =   8 };    // datacard padding.
   enum { KEYWORD_LENGTH   =  80 };    // keyword length.
*/
   enum { LENGTH           = 160 };    // scalar or array element length.

//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//

public:

  static int  exists();                             // returns TRUE or FALSE.

  static void frontendUpdate ();
  static void backendUpdate  ();
  static void guiUpdate      ();
  static void quickUpdate    ();

  static void frontendUpdateNoprint ();
  static void backendUpdateNoprint  ();
  static void guiUpdateNoprint      ();
  static void quickUpdateNoprint    ();

  static void clear                 ();
  static void restore               ();
  static void next                  ();
  static void backendExecute        ();
  static void continueBackendUpdate ();

  static int  getUpdateState     ();            // returns update state enum.
  static void setBackendNoExec   ();
  static void setBackendYesExec  ();
  static int  getIpn             ();
  static int  previousError      ();                // returns TRUE or FALSE.
  static void setIpn             (int ipn);

  static int  doNotProcessTraces ();                // returns TRUE or FALSE.

  static int  updateError        ();                // returns TRUE or FALSE.
  static void error              (const char *msg);
  static void warning            (const char *msg);
  static void info               (const char *msg);
  static void print              (const char *msg);

                       ///////////////////////////////

  static void printAllDataCards  ();

  static void printProcessCards  ();
  static void printGlobalCards   ();
  static void printControlCards  ();
  static void printPdataCards    ();
  static void printJdataCards    ();
  static void printGuiCards      ();

  static void infoProcessCards   ();
  static void infoGlobalCards    ();
  static void infoControlCards   ();
  static void infoPdataCards     ();
  static void infoJdataCards     ();
  static void infoGuiCards       ();

                       ///////////////////////////////

  static int  numElementsProcess (KEY);
  static int  numElementsGlobal  (KEY);
  static int  numElementsControl (KEY);
  static int  numElementsGui     (KEY,ACT);
  static int  numElementsPdata   (KEY);
  static int  numElementsJdata   (KEY);

  static int  natureProcess      (KEY);               // returns nature enum.
  static int  natureGlobal       (KEY);               // returns nature enum.
  static int  natureControl      (KEY);               // returns nature enum.
  static int  natureGui          (KEY,ACT);           // returns nature enum.
  static int  naturePdata        (KEY);               // returns nature enum.
  static int  natureJdata        (KEY);               // returns nature enum.

  static int  vartypeProcess     (KEY);               // returns vartype enum.
  static int  vartypeGlobal      (KEY);               // returns vartype enum.
  static int  vartypeControl     (KEY);               // returns vartype enum.
  static int  vartypeGui         (KEY,ACT);           // returns vartype enum.
  static int  vartypePdata       (KEY);               // returns vartype enum.
  static int  vartypeJdata       (KEY);               // returns vartype enum.

                       ///////////////////////////////

  static void get         (KEY, GTW    *scalar);
  static void get         (KEY, int    *scalar);
  static void get         (KEY, float  *scalar);
  static void get         (KEY, double *scalar);
  static void getL        (KEY, int    *scalar);
  static void get         (KEY, char   *scalar);

  static void getProcess  (KEY, GTW    *scalar);
  static void getProcess  (KEY, int    *scalar);
  static void getProcess  (KEY, float  *scalar);
  static void getProcess  (KEY, double *scalar);
  static void getProcessL (KEY, int    *scalar);
  static void getProcess  (KEY, char   *scalar);

  static void getGlobal   (KEY, GTW    *scalar);
  static void getGlobal   (KEY, int    *scalar);
  static void getGlobal   (KEY, float  *scalar);
  static void getGlobal   (KEY, double *scalar);
  static void getGlobalL  (KEY, int    *scalar);
  static void getGlobal   (KEY, char   *scalar);

  static void getControl  (KEY, GTW    *scalar);
  static void getControl  (KEY, int    *scalar);
  static void getControl  (KEY, float  *scalar);
  static void getControl  (KEY, double *scalar);
  static void getControlL (KEY, int    *scalar);
  static void getControl  (KEY, char   *scalar);

  static void getGui  (KEY,ACT, GTW    *scalar);
  static void getGui  (KEY,ACT, int    *scalar);
  static void getGui  (KEY,ACT, float  *scalar);
  static void getGui  (KEY,ACT, double *scalar);
  static void getGuiL (KEY,ACT, int    *scalar);
  static void getGui  (KEY,ACT, char   *scalar);

  static void getPdata    (KEY, GTW    *scalar);
  static void getPdata    (KEY, int    *scalar);
  static void getPdata    (KEY, float  *scalar);
  static void getPdata    (KEY, double *scalar);
  static void getPdataL   (KEY, int    *scalar);
  static void getPdata    (KEY, char   *scalar);

  static void getJdata    (KEY, GTW    *scalar);
  static void getJdata    (KEY, int    *scalar);
  static void getJdata    (KEY, float  *scalar);
  static void getJdata    (KEY, double *scalar);
  static void getJdataL   (KEY, int    *scalar);
  static void getJdata    (KEY, char   *scalar);

                       ///////////////////////////////

  static void get         (KEY, int nsize, int    *array, int *n);
  static void get         (KEY, int nsize, float  *array, int *n);
  static void get         (KEY, int nsize, double *array, int *n);
  static void getL        (KEY, int nsize, int    *array, int *n);
  static void get         (KEY, int nsize, char   *array, int *n);

  static void getProcess  (KEY, int nsize, int    *array, int *n);
  static void getProcess  (KEY, int nsize, float  *array, int *n);
  static void getProcess  (KEY, int nsize, double *array, int *n);
  static void getProcessL (KEY, int nsize, int    *array, int *n);
  static void getProcess  (KEY, int nsize, char   *array, int *n);

  static void getGlobal   (KEY, int nsize, int    *array, int *n);
  static void getGlobal   (KEY, int nsize, float  *array, int *n);
  static void getGlobal   (KEY, int nsize, double *array, int *n);
  static void getGlobalL  (KEY, int nsize, int    *array, int *n);
  static void getGlobal   (KEY, int nsize, char   *array, int *n);

  static void getControl  (KEY, int nsize, int    *array, int *n);
  static void getControl  (KEY, int nsize, float  *array, int *n);
  static void getControl  (KEY, int nsize, double *array, int *n);
  static void getControlL (KEY, int nsize, int    *array, int *n);
  static void getControl  (KEY, int nsize, char   *array, int *n);

  static void getGui  (KEY,ACT, int nsize, int    *array, int *n);
  static void getGui  (KEY,ACT, int nsize, float  *array, int *n);
  static void getGui  (KEY,ACT, int nsize, double *array, int *n);
  static void getGuiL (KEY,ACT, int nsize, int    *array, int *n);
  static void getGui  (KEY,ACT, int nsize, char   *array, int *n);

  static void getPdata    (KEY, int nsize, int    *array, int *n);
  static void getPdata    (KEY, int nsize, float  *array, int *n);
  static void getPdata    (KEY, int nsize, double *array, int *n);
  static void getPdataL   (KEY, int nsize, int    *array, int *n);
  static void getPdata    (KEY, int nsize, char   *array, int *n);

  static void getJdata    (KEY, int nsize, int    *array, int *n);
  static void getJdata    (KEY, int nsize, float  *array, int *n);
  static void getJdata    (KEY, int nsize, double *array, int *n);
  static void getJdataL   (KEY, int nsize, int    *array, int *n);
  static void getJdata    (KEY, int nsize, char   *array, int *n);

                       ///////////////////////////////

  static void getProcess  (KEY, int indx, int    *element);
  static void getProcess  (KEY, int indx, float  *element);
  static void getProcess  (KEY, int indx, double *element);
  static void getProcessL (KEY, int indx, int    *element);
  static void getProcess  (KEY, int indx, char   *element);

  static void getGlobal   (KEY, int indx, int    *element);
  static void getGlobal   (KEY, int indx, float  *element);
  static void getGlobal   (KEY, int indx, double *element);
  static void getGlobalL  (KEY, int indx, int    *element);
  static void getGlobal   (KEY, int indx, char   *element);

  static void getControl  (KEY, int indx, int    *element);
  static void getControl  (KEY, int indx, float  *element);
  static void getControl  (KEY, int indx, double *element);
  static void getControlL (KEY, int indx, int    *element);
  static void getControl  (KEY, int indx, char   *element);

  static void getGui  (KEY,ACT, int indx, int    *element);
  static void getGui  (KEY,ACT, int indx, float  *element);
  static void getGui  (KEY,ACT, int indx, double *element);
  static void getGuiL (KEY,ACT, int indx, int    *element);
  static void getGui  (KEY,ACT, int indx, char   *element);

  static void getPdata    (KEY, int indx, int    *element);
  static void getPdata    (KEY, int indx, float  *element);
  static void getPdata    (KEY, int indx, double *element);
  static void getPdataL   (KEY, int indx, int    *element);
  static void getPdata    (KEY, int indx, char   *element);

  static void getJdata    (KEY, int indx, int    *element);
  static void getJdata    (KEY, int indx, float  *element);
  static void getJdata    (KEY, int indx, double *element);
  static void getJdataL   (KEY, int indx, int    *element);
  static void getJdata    (KEY, int indx, char   *element);

                       ///////////////////////////////

  static int  pressed   (KEY);           // returns true or false.
  static void activated (char *keyword); // returns keyword which was activated.

  static int  verifyScalar   (KEY);                            // returns T/F.
  static int  verifyElement  (KEY, int *indx, int *action);    // returns T/F.
  static int  verifyArray    (KEY);                            // returns T/F.
  static int  verifyArrayset (KEY);                            // returns T/F.
  static int  verifyScreen   (KEY);                            // returns T/F.
  static int  verifyEnd      ();                               // returns T/F.

                       ///////////////////////////////

  static void put         (KEY, const GTW  *scalar, NCHAR, NDEC);
  static void put         (KEY, int         scalar, NCHAR);
  static void put         (KEY, float       scalar, NCHAR, NDEC);
  static void put         (KEY, double      scalar, NCHAR, NDEC);
  static void putL        (KEY, int         scalar);
  static void put         (KEY, const char *scalar);

  static void putProcess  (KEY, const GTW  *scalar, NCHAR, NDEC);
  static void putProcess  (KEY, int         scalar, NCHAR);
  static void putProcess  (KEY, float       scalar, NCHAR, NDEC);
  static void putProcess  (KEY, double      scalar, NCHAR, NDEC);
  static void putProcessL (KEY, int         scalar);
  static void putProcess  (KEY, const char *scalar);

  static void putGlobal   (KEY, const GTW  *scalar, NCHAR, NDEC);
  static void putGlobal   (KEY, int         scalar, NCHAR);
  static void putGlobal   (KEY, float       scalar, NCHAR, NDEC);
  static void putGlobal   (KEY, double      scalar, NCHAR, NDEC);
  static void putGlobalL  (KEY, int         scalar);
  static void putGlobal   (KEY, const char *scalar);

  static void putControl  (KEY, const GTW  *scalar, NCHAR, NDEC);
  static void putControl  (KEY, int         scalar, NCHAR);
  static void putControl  (KEY, float       scalar, NCHAR, NDEC);
  static void putControl  (KEY, double      scalar, NCHAR, NDEC);
  static void putControlL (KEY, int         scalar);
  static void putControl  (KEY, const char *scalar);

  static void putGui  (KEY,ACT, const GTW  *scalar, NCHAR, NDEC);
  static void putGui  (KEY,ACT, int         scalar, NCHAR);
  static void putGui  (KEY,ACT, float       scalar, NCHAR, NDEC);
  static void putGui  (KEY,ACT, double      scalar, NCHAR, NDEC);
  static void putGuiL (KEY,ACT, int         scalar);
  static void putGui  (KEY,ACT, const char *scalar);

  static void putGuiOnly  (KEY, const GTW  *scalar, NCHAR, NDEC);
  static void putGuiOnly  (KEY, int         scalar, NCHAR);
  static void putGuiOnly  (KEY, float       scalar, NCHAR, NDEC);
  static void putGuiOnly  (KEY, double      scalar, NCHAR, NDEC);
  static void putGuiOnlyL (KEY, int         scalar);
  static void putGuiOnly  (KEY, const char *scalar);

  static void putPdata    (KEY, const GTW  *scalar, NCHAR, NDEC);
  static void putPdata    (KEY, int         scalar, NCHAR);
  static void putPdata    (KEY, float       scalar, NCHAR, NDEC);
  static void putPdata    (KEY, double      scalar, NCHAR, NDEC);
  static void putPdataL   (KEY, int         scalar);
  static void putPdata    (KEY, const char *scalar);

  static void putJdata    (KEY, const GTW  *scalar, NCHAR, NDEC);
  static void putJdata    (KEY, int         scalar, NCHAR);
  static void putJdata    (KEY, float       scalar, NCHAR, NDEC);
  static void putJdata    (KEY, double      scalar, NCHAR, NDEC);
  static void putJdataL   (KEY, int         scalar);
  static void putJdata    (KEY, const char *scalar);

                       ///////////////////////////////

  static void put         (KEY, const int    *array, int n, NCHAR);
  static void put         (KEY, const float  *array, int n, NCHAR, NDEC);
  static void put         (KEY, const double *array, int n, NCHAR, NDEC);
  static void putL        (KEY, const int    *array, int n);
  static void put         (KEY, const char   *array, int n);

  static void putProcess  (KEY, const int    *array, int n, NCHAR);
  static void putProcess  (KEY, const float  *array, int n, NCHAR, NDEC);
  static void putProcess  (KEY, const double *array, int n, NCHAR, NDEC);
  static void putProcessL (KEY, const int    *array, int n);
  static void putProcess  (KEY, const char   *array, int n);

  static void putGlobal   (KEY, const int    *array, int n, NCHAR);
  static void putGlobal   (KEY, const float  *array, int n, NCHAR, NDEC);
  static void putGlobal   (KEY, const double *array, int n, NCHAR, NDEC);
  static void putGlobalL  (KEY, const int    *array, int n);
  static void putGlobal   (KEY, const char   *array, int n);

  static void putControl  (KEY, const int    *array, int n, NCHAR);
  static void putControl  (KEY, const float  *array, int n, NCHAR, NDEC);
  static void putControl  (KEY, const double *array, int n, NCHAR, NDEC);
  static void putControlL (KEY, const int    *array, int n);
  static void putControl  (KEY, const char   *array, int n);

  static void putGui  (KEY,ACT, const int    *array, int n, NCHAR);
  static void putGui  (KEY,ACT, const float  *array, int n, NCHAR, NDEC);
  static void putGui  (KEY,ACT, const double *array, int n, NCHAR, NDEC);
  static void putGuiL (KEY,ACT, const int    *array, int n);
  static void putGui  (KEY,ACT, const char   *array, int n);

  static void putGuiOnly  (KEY, const int    *array, int n, NCHAR);
  static void putGuiOnly  (KEY, const float  *array, int n, NCHAR, NDEC);
  static void putGuiOnly  (KEY, const double *array, int n, NCHAR, NDEC);
  static void putGuiOnlyL (KEY, const int    *array, int n);
  static void putGuiOnly  (KEY, const char   *array, int n);

  static void putPdata    (KEY, const int    *array, int n, NCHAR);
  static void putPdata    (KEY, const float  *array, int n, NCHAR, NDEC);
  static void putPdata    (KEY, const double *array, int n, NCHAR, NDEC);
  static void putPdataL   (KEY, const int    *array, int n);
  static void putPdata    (KEY, const char   *array, int n);

  static void putJdata    (KEY, const int    *array, int n, NCHAR);
  static void putJdata    (KEY, const float  *array, int n, NCHAR, NDEC);
  static void putJdata    (KEY, const double *array, int n, NCHAR, NDEC);
  static void putJdataL   (KEY, const int    *array, int n);
  static void putJdata    (KEY, const char   *array, int n);

                       ///////////////////////////////

  static void registerArrayNames (KEY, const char *arrays, int narrays);

  static void putOptions   (KEY, const int   *options, int noptions,NCHAR);
  static void putOptions   (KEY, const float *options, int noptions,NCHAR,NDEC);
  static void putOptions   (KEY, const double*options, int noptions,NCHAR,NDEC);
  static void putOptionsL  (KEY, const int   *options, int noptions);
  static void putOptions   (KEY, const char  *options, int noptions);

  static void putOptionsA  (KEY, const int   *options, int noptions,NCHAR);
  static void putOptionsA  (KEY, const float *options, int noptions,NCHAR,NDEC);
  static void putOptionsA  (KEY, const double*options, int noptions,NCHAR,NDEC);
  static void putOptionsAL (KEY, const int   *options, int noptions);
  static void putOptionsA  (KEY, const char  *options, int noptions);

  static void putSensitiveFieldFlag    (KEY, int sensitive);
  static void putSensitiveArrayFlag    (KEY, int sensitive);
  static void putSensitiveArraysetFlag (KEY, int sensitive);
  static void putSensitiveScreenFlag   (KEY, int sensitive);

  static void putVisibleFlag           (KEY, int visible);

  static void putMinsizeArray          (KEY, int minsize);
  static void putMinsizeArrayset       (KEY, int minsize);
  static void putMaxsizeArray          (KEY, int maxsize);
  static void putMaxsizeArrayset       (KEY, int maxsize);

                       ///////////////////////////////

  static int  numProcessCards ();
  static int  numGlobalCards  ();
  static int  numControlCards ();
  static int  numPdataCards   ();
  static int  numJdataCards   ();
  static int  numGuiCards     ();

                       ///////////////////////////////

  static void getProcessCards(int nsize, char *cards, int *ncards);
  static void getGlobalCards (int nsize, char *cards, int *ncards);
  static void getControlCards(int nsize, char *cards, int *ncards);
  static void getPdataCards  (int nsize, char *cards, int *ncards);
  static void getJdataCards  (int nsize, char *cards, int *ncards);
  static void getGuiCards    (int nsize, char *cards, int *ncards);

  static void getProcessCard  (int icard, char *card);
  static void getGlobalCard   (int icard, char *card);
  static void getControlCard  (int icard, char *card);
  static void getPdataCard    (int icard, char *card);
  static void getJdataCard    (int icard, char *card);
  static void getGuiCard      (int icard, char *card);

  static void putProcessCards (const char *cards, int ncards);
  static void putGlobalCards  (const char *cards, int ncards);
  static void putControlCards (const char *cards, int ncards);
  static void putPdataCards   (const char *cards, int ncards);
  static void putJdataCards   (const char *cards, int ncards);
  static void putGuiCards     (const char *cards, int ncards);

  static void putProcessCard  (const char *card);
  static void putGlobalCard   (const char *card);
  static void putControlCard  (const char *card);
  static void putPdataCard    (const char *card);
  static void putJdataCard    (const char *card);
  static void putGuiCard      (const char *card);

  static void addProcessCard  (const char *card);
  static void addGlobalCard   (const char *card);
  static void addControlCard  (const char *card);
  static void addPdataCard    (const char *card);
  static void addJdataCard    (const char *card);
  static void addGuiCard      (const char *card);

                       ///////////////////////////////

  static void clearProcessCards     ();
  static void clearGlobalCards      ();
  static void clearControlCards     ();
  static void clearPdataCards       ();
  static void clearJdataCards       ();
  static void clearGuiCards         ();

  static int  processKeywordPresent (KEY);          // returns true or false.
  static int  globalKeywordPresent  (KEY);          // returns true or false.
  static int  controlKeywordPresent (KEY);          // returns true or false.
  static int  pdataKeywordPresent   (KEY);          // returns true or false.
  static int  jdataKeywordPresent   (KEY);          // returns true or false.
  static int  guiActionPresent      (KEY,ACT);      // returns true or false.

  static int  numProcessKeywords    ();
  static int  numGlobalKeywords     ();
  static int  numControlKeywords    ();
  static int  numPdataKeywords      ();
  static int  numJdataKeywords      ();
  static int  numGuiKeywords        ();

  static void getProcessKeyword     (int indx, char *keyword);
  static void getGlobalKeyword      (int indx, char *keyword);
  static void getControlKeyword     (int indx, char *keyword);
  static void getPdataKeyword       (int indx, char *keyword);
  static void getJdataKeyword       (int indx, char *keyword);
  static void getGuiKeyword         (int indx, char *keyword);
  static void getGuiAction          (int indx, char *action);

  static void removeProcessKeyword  (KEY);
  static void removeGlobalKeyword   (KEY);
  static void removeControlKeyword  (KEY);
  static void removePdataKeyword    (KEY);
  static void removeJdataKeyword    (KEY);
  static void removeGuiAction       (KEY,ACT);

//-------------------------- end of class -------------------------------//
//-------------------------- end of class -------------------------------//
//-------------------------- end of class -------------------------------//

};

//--------------------------- undefine macros -----------------------------//
//--------------------------- undefine macros -----------------------------//
//--------------------------- undefine macros -----------------------------//

#undef KEY
#undef ACT
#undef NCHAR
#undef NDEC

//-------------------------- end of undefine macros -----------------------//
//-------------------------- end of undefine macros -----------------------//
//-------------------------- end of undefine macros -----------------------//

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
