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

//------------------------ tp_pair_base.hh ---------------------//
//------------------------ tp_pair_base.hh ---------------------//
//------------------------ tp_pair_base.hh ---------------------//

//               header file for the TpPairBase class
//               derived from the SLFilePairPlus class
//                         subdirectory pick


#ifndef _TP_PAIR_BASE_HH_
#define _TP_PAIR_BASE_HH_

#include "sl/sl_file_pair_plus.hh"
#include <X11/Intrinsic.h>


class TpPairBase : public SLFilePairPlus
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

protected:     // no data

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructor and destructor

  TpPairBase(SLDelay *slparent, char *name,
                        const char * const filetype,
                        const char * const extension,
                        const Boolean      required1,
                        const Boolean      required2);
  
  virtual ~TpPairBase();

/***************
protected:     // virtual functions to override (from SLFilePairPlus)
               // repeated here for reference

  virtual void doValidate (const char *filename1,
                           const char *filename2,
                           long *valid1, long *valid2,
                           char *info1, char *info2,
                           long *same_datasets) {}

  virtual int doOpen (long status,
                      const char *filename1,
                      const char *filename2,
                      Boolean required1, Boolean required2,
                      FppWorkingMessage *working_message_trap,
                      void              *working_message_data,
                      char *msg) {}

  virtual void doClose () {}
***************/

public:     // virtual functions to override

  virtual void doReadCurrentPicks  (float *picks,
                   const float *head, long nwords, long n) = 0;

  virtual void doReadPreviousPicks (float *picks,
                   const float *head, long nwords, long n) = 0;

  virtual void doReadNextPicks     (float *picks,
                   const float *head, long nwords, long n) = 0;

  virtual void doReadSelectedPicks (float *picks,
                   const float *head, long nwords, long n) = 0;

  virtual void doSaveCurrentPicks  (float *picks,
                   const float *head, long nwords, long n, long action) = 0;

  virtual void doUpdateFile        () = 0;

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
