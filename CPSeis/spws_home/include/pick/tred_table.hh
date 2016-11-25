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

//---------------------- tred_table.hh ------------------------------//
//---------------------- tred_table.hh ------------------------------//
//---------------------- tred_table.hh ------------------------------//

//         header file for the Tred_Table class
//              derived from the SLDatabox class


#ifndef _TRED_TABLE_HH_
#define _TRED_TABLE_HH_

#include "sl/sl_databox.hh"
#include "vect/vector.hh"
#include "pick/tred_data.hh"
#include "pick/tred_table_pop.hh"
#include "tredfile_object.h"

class SeisVectLinkedList;
class TredTablePop;
class SLErrorPop;

class TredTable: public SLDatabox
{

//------------------------ data -----------------------------//
//------------------------ data -----------------------------//
//------------------------ data -----------------------------//
private:
  enum ident_list { DO=51, CODE, HDRWRD1, STRTVLU1, ENDVLU1, HDRWRD2, STRTVLU2,
    ENDVLU2, HDRWRD3, STRTVLU3, ENDVLU3 };
  long               _n, _nmax, _size;
  long               _ivara[TRED_TABLE_NMAX];
  TredFile           *_tf;
  char               *_doptr[TRED_TABLE_NMAX];
  char               *_codptr[TRED_TABLE_NMAX];
  long               _numpoints[TRED_VECT_COUNT];
  float              *_xdata[TRED_VECT_COUNT];
  float              _ydata[TRED_VECT_COUNT];
  SeisVectLinkedList *_vect_ll;
  Vector             *_V[TRED_VECT_COUNT];
  TredData           *_tred_data[TRED_VECT_COUNT];
  TredTablePop       *_pop;
  SLErrorPop         *_errpop;

//------------------------ functions -----------------------------//
//------------------------ functions -----------------------------//
//------------------------ functions -----------------------------//

  void doInitRowAndPost (char *next_do, long *idptr, long idxm1);
  void codeInitRowAndPost (char *next_code, long *idptr, long idxm1);
  void initRowAndPost (long *idptr, long idxm1, Bool post_ok);
  Bool verifyOne   (long idxm1);
  Bool verifyTwo   (long idxm1);
  Bool verifyThree (long idxm1);
  void doFilePut   (void);

public:
  TredTable (SLDelay *slparent, char *name, TredTablePop *pop);
  TredTable (Widget parent, char *name, TredTablePop *pop);
  virtual ~TredTable (void);
  TredFile *getTredFile ();
  int fileClear ();
  void sensitizeTable ();
  void desensitizeTable ();
  SeisVectLinkedList *createVectors (SeisPlot *sp);
  void post (SeisPlot *sp);
  void deleteVectors (void);
  void updatePick (SeisPlot *sp, const Bool undo, const char *code,
    const Bool use_hwd1, const long hwd1, const float min1, const float max1,
    const Bool use_hwd2, const long hwd2, const float min2, const float max2,
    const Bool use_hwd3, const long hwd3, const float min3, const float max3);
  void findPickMinMax (SeisPlot *sp, const long first_trace,
    const long last_trace, const long header_word, float *min, float *max);
  int fileReset (void);

//--------------------- traps -------------------------------//
//--------------------- traps -------------------------------//
  static void trapDo   (void *box, long *ident, long *index,
                        char *text, long *nread, char *endkey);
  static void trapCode (void *box, long *ident, long *index,
                        char *text, long *nread, char *endkey);
  static void trapHdr  (void *box, long *ident, long *index,
                        char *text, long *nread, char *endkey);
  static void trapVlu  (void *box, long *ident, long *index,
                        char *text, long *nread, char *endkey);

protected:

//----------------- overriding method makeHelper -----------------//
//----------------- overriding method makeHelper -----------------//
  virtual void makeHelper();

  void constructorHelper();

//--------------------- end of functions -------------------------//
//--------------------- end of functions -------------------------//
//--------------------- end of functions -------------------------//

};

#endif

//---------------------------- end ------------------------------//
//---------------------------- end ------------------------------//
//---------------------------- end ------------------------------//
