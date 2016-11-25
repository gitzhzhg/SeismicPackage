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

//------------------- tp_encoding_desire.hh ----------------------------//
//------------------- tp_encoding_desire.hh ----------------------------//
//------------------- tp_encoding_desire.hh ----------------------------//

//           header file for the TpEncodingDesire class
//           derived from the FileSupportInterface class
//             derived from the SLEncodingDesire class
//                       subdirectory pick

               // there is no implementation file

#ifndef _TP_ENCODING_DESIRE_HH_
#define _TP_ENCODING_DESIRE_HH_

#include "oprim/file_support_interface.hh"
#include "sl/sl_encoding_desire.hh"
#include "pick/tp_statfile_pair.hh"
#include "named_constants.h"
#include <assert.h>


class TpEncodingDesire : public FileSupportInterface, public SLEncodingDesire
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  TpStatfilePair *_pair;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  TpEncodingDesire (SLDelay *slparent, TpStatfilePair *pair)
                       : FileSupportInterface(),
                         SLEncodingDesire(slparent, this, "static"),
                         _pair(pair)  { assert(_pair); }

  virtual ~TpEncodingDesire() {}

  virtual int         allowOldcps ()  const  { return TRUE; }
  virtual int         allowAscii  ()  const  { return TRUE; }
  virtual int         allowBinary ()  const  { return TRUE; }
  virtual int         allowHybrid ()  const  { return TRUE; }
  virtual const char *getEncoding ()         { return _pair->getEncoding(); }
  virtual void        setEncoding (const char *encoding)
                                             { _pair->setEncoding(encoding); }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
