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

//---------------------- tp_ref_pair.hh -------------------------//
//---------------------- tp_ref_pair.hh -------------------------//
//---------------------- tp_ref_pair.hh -------------------------//

//             header file for the TpRefPair class
//              derived from the TpPairBase class
//                       subdirectory pick

#ifndef _TP_REF_PAIR_HH_
#define _TP_REF_PAIR_HH_

#include "pick/tp_pair_base.hh"


class TpRefPair : public TpPairBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class TpPopupBase    *_pop;
/*
  class SeisPlot       *_sp;
*/
  class RefractionData *_refdata;
  long                  _typegp;    // SEQU or GRID ground positions
  long                  _align;     // overlay alignment
  long                  _selgrp;    // selected shot profile
  long                  _firstgrp;  // first shot profile
  long                  _lastgrp;   // last shot profile
  long                  _latest;    // latest shot profile updated
  long                  _disp1;     // first shot profile displayed
  long                  _disp2;     // last shot profile displayed
  int                   _pickhead;  // ftn-style header word containing picks.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:          // SeisPlot *dummy no longer used.

  TpRefPair (SLDelay *slparent, TpPopupBase *pop, class SeisPlot *dummy);

  virtual ~TpRefPair();

  void  setTypeGP           (long typegp);
  void  setOverlayAlignment (long align);
  void  setSelectedProfile  (long selgrp);
  void  setPickhead         (int pickhead);

  long  getTypeGP                ()  const  { return _typegp; }
  long  getOverlayAlignment      ()  const  { return _align; }
  long  getSelectedProfile       ()  const  { return _selgrp; }
  long  getFirstProfile          ()  const  { return _firstgrp; }
  long  getLastProfile           ()  const  { return _lastgrp; }
  long  getLatestProfileUpdated  ()  const  { return _latest; }
  long  getFirstDisplayedProfile ()  const  { return _disp1; }
  long  getLastDisplayedProfile  ()  const  { return _disp2; }
  int   getPickhead              ()  const  { return _pickhead; }

protected:   // overriding virtual functions  (from SLFilePairPlus)

  virtual void doValidate (const char *filename1,
                           const char *filename2,
                           long *valid1, long *valid2,
                           char *info1, char *info2,
                           long *same_datasets);

  virtual int doOpen (long status,
                      const char *filename1,
                      const char *filename2,
                      Boolean required1, Boolean required2,
                      FppWorkingMessage *working_message_trap,
                      void              *working_message_data,
                      char *msg);

  virtual void doClose ();

public:      // overriding virtual functions (from TpPairBase)

  virtual void doReadCurrentPicks   (float *picks,
                        const float *head, long nwords, long n);

  virtual void doReadPreviousPicks  (float *picks,
                        const float *head, long nwords, long n);

  virtual void doReadNextPicks      (float *picks,
                        const float *head, long nwords, long n);

  virtual void doReadSelectedPicks  (float *picks,
                        const float *head, long nwords, long n);

  virtual void doSaveCurrentPicks   (float *picks,
                        const float *head, long nwords, long n, long action);

  virtual void doUpdateFile        ();

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
