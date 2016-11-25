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

//----------------------- wbox_roll.hh -------------------------//
//----------------------- wbox_roll.hh -------------------------//
//----------------------- wbox_roll.hh -------------------------//

//              header file for the WboxRoll class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_ROLL_HH_
#define _WBOX_ROLL_HH_


class WboxRoll
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class WboxBox *_box;

  int    _ksave;       // number of current active datafield.

  int    _kscalar;     // number of last active scalar datafield
                       //   [in the scalar tab group].

                       // (number of last active array datafield
                       //   [in each linked-array tab group]
                       //   resides in each WboxLink object.)


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  WboxRoll (WboxBox *box);

  virtual ~WboxRoll();

  class WboxField *getActiveFieldPointer()  const;

  void gotoNewDatafield       (const char *endkey, int irow, int icol);
  void gotoSpecifiedDatafield (int ident, int index);

private:

  void enforceSwitchValue       ();
  void rememberActiveDatafield  ();

  void rollWithArrows     (const char *endkey);
  void rollWithTab        (const char *endkey);
  void rollForArrays      (const char *endkey, int ksave_keep);
  int  rollToMatch        (const char *endkey, int index);
  void rollToNewIndex     (int index);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


