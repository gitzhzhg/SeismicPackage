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

//----------------------- wbox_find.hh -------------------------//
//----------------------- wbox_find.hh -------------------------//
//----------------------- wbox_find.hh -------------------------//

//              header file for the WboxFind class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_FIND_HH_
#define _WBOX_FIND_HH_


class WboxFind
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  enum { WBOX_VSIZE = 102 };

  class WboxBox      *_box;
  class WboxMessages *_messages;
  class WboxField    *_index_field;
  class WboxField    *_value_field;
  class WboxField    *_field_keep;
  int                 _ident_keep;
  long                _index_keep;
  int                 _n_keep;
  int                 _nread_keep;
  long                _iswi;                    // switch for index field.
  long                _iswv;                    // switch for value field.
  char                _value_keep[WBOX_VSIZE];


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  WboxFind (WboxBox *box, WboxMessages *messages);

  virtual ~WboxFind();

  void  useFindFields (int *ident, int *index, int *nread, char *endkey);

private:

  int   findValue (int nread, int *index);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


