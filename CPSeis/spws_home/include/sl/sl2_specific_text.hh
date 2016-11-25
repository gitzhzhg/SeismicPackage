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

//---------------------- sl2_specific_text.hh -----------------------------//
//---------------------- sl2_specific_text.hh -----------------------------//
//---------------------- sl2_specific_text.hh -----------------------------//

//                header file for the SL2Itext class
//                header file for the SL2Ftext class
//                header file for the SL2Dtext class
//                header file for the SL2Ctext class
//                  derived from the SL2Text class
//                         subdirectory sl

      // There are no implementation files for these classes.

          // These are specific classes for text widgets
          //         (preceded by a label)
          // which display long, float, double, and char.


#ifndef _SL2_SPECIFIC_TEXT_HH_
#define _SL2_SPECIFIC_TEXT_HH_

#include "sl/sl2_text.hh"


//----------------------------- SL2Itext ----------------------------------//
//----------------------------- SL2Itext ----------------------------------//
//----------------------------- SL2Itext ----------------------------------//

class SL2Itext : public SL2Text
{
public:

  SL2Itext (SLDelay *slparent, char *name, long ident = 0,
            char *label = NULL, long nchar = 0)
              : SL2Text(slparent, name, ident, label, _LONG, nchar) {}

  SL2Itext (Widget   wparent , char *name, long ident = 0,
            char *label = NULL, long nchar = 0)
              : SL2Text(wparent, name, ident, label, _LONG, nchar) {}

  virtual ~SL2Itext ();
};

//----------------------------- SL2Ftext ----------------------------------//
//----------------------------- SL2Ftext ----------------------------------//
//----------------------------- SL2Ftext ----------------------------------//

class SL2Ftext : public SL2Text
{
public:

  SL2Ftext (SLDelay *slparent, char *name, long ident = 0,
            char *label = NULL, long nchar = 0, long ndec = 999)
              : SL2Text(slparent, name, ident, label, _FLOAT, nchar, ndec) {}

  SL2Ftext (Widget   wparent , char *name, long ident = 0,
            char *label = NULL, long nchar = 0, long ndec = 999)
              : SL2Text(wparent, name, ident, label, _FLOAT, nchar, ndec) {}

  virtual ~SL2Ftext ();
};

//----------------------------- SL2Dtext ----------------------------------//
//----------------------------- SL2Dtext ----------------------------------//
//----------------------------- SL2Dtext ----------------------------------//

class SL2Dtext : public SL2Text
{
public:

  SL2Dtext (SLDelay *slparent, char *name, long ident = 0,
            char *label = NULL, long nchar = 0, long ndec = 999)
              : SL2Text(slparent, name, ident, label, _DOUBLE, nchar, ndec) {}

  SL2Dtext (Widget   wparent , char *name, long ident = 0,
            char *label = NULL, long nchar = 0, long ndec = 999)
              : SL2Text(wparent, name, ident, label, _DOUBLE, nchar, ndec) {}

  virtual ~SL2Dtext ();
};

//----------------------------- SL2Ctext ----------------------------------//
//----------------------------- SL2Ctext ----------------------------------//
//----------------------------- SL2Ctext ----------------------------------//

class SL2Ctext : public SL2Text
{
public:

  SL2Ctext (SLDelay *slparent, char *name, long ident = 0,
            char *label = NULL, long nchar = 0)
              : SL2Text(slparent, name, ident, label, _CHAR, nchar) {}

  SL2Ctext (Widget   wparent , char *name, long ident = 0,
            char *label = NULL, long nchar = 0)
              : SL2Text(wparent, name, ident, label, _CHAR, nchar) {}

  virtual ~SL2Ctext ();
};



//--------------------------- end of classes -----------------------------//
//--------------------------- end of classes -----------------------------//
//--------------------------- end of classes -----------------------------//

#endif

//------------------------------- end --------------------------------------//
//------------------------------- end --------------------------------------//
//------------------------------- end --------------------------------------//

