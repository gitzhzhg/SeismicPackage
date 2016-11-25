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
//------------------- tred_table_pop.hh ------------------------------//
//------------------- tred_table_pop.hh ------------------------------//
//------------------- tred_table_pop.hh ------------------------------//

//              header file for the TredTablePop class
//                 derived from the StpPopupBase class
//                        subdirectory tred

#ifndef _TRED_TABLE_POP_
#define _TRED_TABLE_POP_

#include "pick/stp_popup_base.hh"

#define HWDS 3

class SeisPlot;
class TredTable;
class TredFilePair;
class TredPickGui;
class SeisVectLinkedList;
class SLFilePairPlus;

class TredTablePop : public StpPopupBase
{
//------------------ beginning of class--------------------------//
//------------------ beginning of class--------------------------//

public:

  enum TredType {DELTYPE=1, KILLTYPE, REVTYPE, FLAGTYPE};

  TredTablePop (SLDelay *slparent, char *name, HelpCtx Hctx, SeisPlot *sp);
  virtual ~TredTablePop (void);
  void     setType  (TredType type);
  TredType getType (void) { return _tred_type; }
  long getHeaderWord (TredType type, long id);
  void setDeleteHeaderWord  (const long hwd, const long id);
  void setKillHeaderWord    (const long hwd, const long id);
  void setReverseHeaderWord (const long hwd, const long id);
  void setFlagHeaderWord    (const long hwd, const long id);
  int filePut (void);

//The following routines may not be used any more
  TredFilePair *getTredFilePairObject (void);
  TredTable *getTredTableObject(void);

protected:

  virtual void updateVectors (SeisPlot *sp, Why why);
  virtual void updateFile (SeisPlot *sp, Why why);
  virtual SeisVectLinkedList *createVectors (SeisPlot *sp);
  virtual void deleteVectors (void);
  virtual SLFilePairPlus *getFilePairPlus (void);
  virtual void pickingActionCompleted (SeisPlot *sp, int button,
    PickBase::Modifier modifier, long direction, long first_trace,
    long last_trace, float first_time, float last_time);

private:

  void modifyCurrentPicks (SeisPlot *sp, const Bool undo,
    const char *code, const long *hwds, const long first_trace,
    const long last_trace);
  void setDefaultHeaderWords (long *hwds);
  long getHeaderWordHelper (long *hwds, long id) {return hwds[id];}

  TredPickGui  *_pk_gui;
  TredTable    *_ttable;
  TredFilePair *_pair;
  TredType     _tred_type;
  long         _tred_del_hwds [HWDS];
  long         _tred_kill_hwds[HWDS];
  long         _tred_rev_hwds [HWDS];
  long         _tred_flag_hwds[HWDS];

//--------------------- end of class ----------------------------//
//--------------------- end of class ----------------------------//
} ;

#endif

//------------------------ end --------------------------------//
//------------------------ end --------------------------------//
