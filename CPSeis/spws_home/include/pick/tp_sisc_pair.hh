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

//---------------------- tp_sisc_pair.hh -------------------------//
//---------------------- tp_sisc_pair.hh -------------------------//
//---------------------- tp_sisc_pair.hh -------------------------//

//             header file for the TpSiscPair class
//              derived from the TpPairBase class
//                       subdirectory pick

                      // also used by FISH

#ifndef _TP_SISC_PAIR_HH_
#define _TP_SISC_PAIR_HH_

#include "pick/tp_statfile_pair.hh"


class TpSiscPair : public TpStatfilePair
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:      // no data

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  TpSiscPair (SLDelay *slparent, class TpPopupBase *pop,
                          SeisPlot *sp,
                          const char * const filetype,
                          const char * const extension,
                          const Boolean      required1,
                          const Boolean      required2,
                          const char * const program,
                          const char * const default_stattype);

  virtual ~TpSiscPair();

  virtual void doReadCurrentPicks   (float *picks,
                   const float *head, long nwords, long n);

  virtual void doReadPreviousPicks  (float *picks,
                   const float *head, long nwords, long n);

  virtual void doSaveCurrentPicks   (float *picks,
                   const float *head, long nwords, long n, long action);

private:

  void convertPicksToOrFromFile     (float *picks,
                   const float *head, long nwords, long n);

  void adjustPicksByAction          (float *picks,
                   const float *head, long nwords, long n, long action);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
