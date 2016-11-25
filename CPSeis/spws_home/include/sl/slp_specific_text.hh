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

//---------------------- slp_specific_text.hh -----------------------------//
//---------------------- slp_specific_text.hh -----------------------------//
//---------------------- slp_specific_text.hh -----------------------------//

//                header file for the SLpItext class
//                header file for the SLpFtext class
//                header file for the SLpDtext class
//                header file for the SLpCtext class
//                  derived from the SLpText class
//                         subdirectory sl

      // There are no implementation files for these classes.

          // These are specific classes for text widgets
          // which display long, float, double, and char.


#ifndef _SLP_SPECIFIC_TEXT_HH_
#define _SLP_SPECIFIC_TEXT_HH_

#include "sl/slp_text.hh"


//----------------------------- SLpItext ----------------------------------//
//----------------------------- SLpItext ----------------------------------//
//----------------------------- SLpItext ----------------------------------//

class SLpItext : public SLpText
{
public:

  SLpItext (SLDelay *slparent, char *name, long ident = 0,
            long nchar = 0)
                : SLpText(slparent, name, ident, _LONG, nchar) {}

  SLpItext (Widget   wparent , char *name, long ident = 0,
            long nchar = 0)
                : SLpText(wparent, name, ident, _LONG, nchar) {}

  SLpItext (Widget   w,                    long ident = 0,
            long nchar = 0)
                : SLpText(w, ident, _LONG, nchar) {}

  virtual ~SLpItext ();
};

//----------------------------- SLpFtext ----------------------------------//
//----------------------------- SLpFtext ----------------------------------//
//----------------------------- SLpFtext ----------------------------------//

class SLpFtext : public SLpText
{
public:

  SLpFtext (SLDelay *slparent, char *name, long ident = 0,
            long nchar = 0, long ndec = 999)
                : SLpText(slparent, name, ident, _FLOAT, nchar, ndec) {}

  SLpFtext (Widget   wparent , char *name, long ident = 0,
            long nchar = 0, long ndec = 999)
                : SLpText(wparent, name, ident, _FLOAT, nchar, ndec) {}

  SLpFtext (Widget   w,                    long ident = 0,
            long nchar = 0, long ndec = 999)
                : SLpText(w, ident, _FLOAT, nchar, ndec) {}

  virtual ~SLpFtext ();
};

//----------------------------- SLpDtext ----------------------------------//
//----------------------------- SLpDtext ----------------------------------//
//----------------------------- SLpDtext ----------------------------------//

class SLpDtext : public SLpText
{
public:

  SLpDtext (SLDelay *slparent, char *name, long ident = 0,
            long nchar = 0, long ndec = 999)
                : SLpText(slparent, name, ident, _DOUBLE, nchar, ndec) {}

  SLpDtext (Widget   wparent , char *name, long ident = 0,
            long nchar = 0, long ndec = 999)
                : SLpText(wparent, name, ident, _DOUBLE, nchar, ndec) {}

  SLpDtext (Widget   w,                    long ident = 0,
            long nchar = 0, long ndec = 999)
                : SLpText(w, ident, _DOUBLE, nchar, ndec) {}

  virtual ~SLpDtext ();
};

//----------------------------- SLpCtext ----------------------------------//
//----------------------------- SLpCtext ----------------------------------//
//----------------------------- SLpCtext ----------------------------------//

class SLpCtext : public SLpText
{
public:

  SLpCtext (SLDelay *slparent, char *name, long ident = 0,
            long nchar = 0)
                : SLpText(slparent, name, ident, _CHAR, nchar) {}

  SLpCtext (Widget   wparent , char *name, long ident = 0,
            long nchar = 0)
                : SLpText(wparent, name, ident, _CHAR, nchar) {}

  SLpCtext (Widget   w,                    long ident = 0,
            long nchar = 0)
                : SLpText(w, ident, _CHAR, nchar) {}

  virtual ~SLpCtext ();
};



//--------------------------- end of classes -----------------------------//
//--------------------------- end of classes -----------------------------//
//--------------------------- end of classes -----------------------------//

#endif

//------------------------------- end --------------------------------------//
//------------------------------- end --------------------------------------//
//------------------------------- end --------------------------------------//

