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

//---------------------- static_decode.hh ---------------------//
//---------------------- static_decode.hh ---------------------//
//---------------------- static_decode.hh ---------------------//

//              header file for the StaticDecode class
//                  not derived from any class
//                      subdirectory stat


       // decodes a card which contains static values.
       // owned by (and called only by) StaticGenericIO.


#ifndef _STATIC_DECODE_HH_
#define _STATIC_DECODE_HH_


class StaticDecode
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  enum { COLUMN_ORIENTED, ORDER_ORIENTED, BAD_CODE };

  char        *_how;
  int          _orientation;      // one of the above enums.
  char        *_decode;           // for order orientation.
  int          _xwhich;           // for order orientation.
  int          _ywhich;           // for order orientation.
  int          _swhich;           // for order orientation.
  int          _mwhich;           // for order orientation.
  int          _xfirst;           // for column orientation.
  int          _yfirst;           // for column orientation.
  int          _sfirst;           // for column orientation.
  int          _mfirst;           // for column orientation.
  int          _xlast;            // for column orientation.
  int          _ylast;            // for column orientation.
  int          _slast;            // for column orientation.
  int          _mlast;            // for column orientation.

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  StaticDecode();
  virtual ~StaticDecode();

  void        howDecode   (const char *how);
  const char *howDecode   ()  const  { return _how; }
  int         badDecode   ()  const  { return (_orientation == BAD_CODE); }

  int         decodeValue (const char *card,
                                float *xbin, float *ybin, float *value);

private:

  int  useOrderOrientation  (const char *card,
                                float *xbin, float *ybin, float *value);
  int  useColumnOrientation (const char *card,
                                float *xbin, float *ybin, float *value);
  int  tryOrderOrientation  ();
  int  tryColumnOrientation ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
