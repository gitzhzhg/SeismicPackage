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

//---------------------- tred_file_pair.hh -------------------------//
//---------------------- tred_file_pair.hh -------------------------//
//---------------------- tred_file_pair.hh -------------------------//

//             header file for the TredFilePair class
//              derived from the SLFilePairPlus class

#ifndef _TRED_FILE_PAIR_HH_
#define _TRED_FILE_PAIR_HH_

#include "sl/sl_file_pair_plus.hh"

class TredFilePair : public SLFilePairPlus
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:
  class TredTable    *_tredtable;
  class TredTablePop *_tredtablepop;
  class SLErrorPop   *_errpop;
  class SLDelay      *_slparent;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:
  TredFilePair (SLDelay *slparent, class TredTable *tredtable,
                class TredTablePop *tredtablepop);
  virtual ~TredFilePair ();
  void goUpdateFile ();
  int filePut ();

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

public:

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
