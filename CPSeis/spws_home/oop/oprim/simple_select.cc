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

//---------------------- simple_select.cc -----------------------//
//---------------------- simple_select.cc -----------------------//
//---------------------- simple_select.cc -----------------------//

//         implementation file for the SimpleSelect class
//                  not derived from any class
//                     subdirectory oprim


#include "oprim/simple_select.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


#define  SELECT_MAYBE_NO     ' '
#define  SELECT_MAYBE_YES    '*'
#define  SELECT_YES_ALWAYS   'Y'
#define  SELECT_NO_ALWAYS    'n'
#define  SELECT_YES_TOP      'T'
#define  SELECT_YES_BOTTOM   'B'



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


SimpleSelect::SimpleSelect()
           :
                _nselect    (0),
                _sa         (NULL)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

SimpleSelect::~SimpleSelect()
{
}


//--------------- increment select value ---------------------//
//--------------- increment select value ---------------------//
//--------------- increment select value ---------------------//

                     // public

void SimpleSelect::incrementSelectValue(long i)
{
  char value = getSelectValue(i);
  switch(value)
      {
      case SELECT_MAYBE_NO   : value = SELECT_YES_ALWAYS; break;
      case SELECT_MAYBE_YES  : value = SELECT_YES_ALWAYS; break;
      case SELECT_YES_ALWAYS : value = SELECT_NO_ALWAYS;  break;
      case SELECT_NO_ALWAYS  : value = SELECT_YES_TOP;    break;
      case SELECT_YES_TOP    : value = SELECT_YES_BOTTOM; break;
      case SELECT_YES_BOTTOM : value = SELECT_MAYBE_NO;   break;
      default                : assert(FALSE);
      }
  setSelectValue(i, value);
}



//---------------------- is selected ------------------------//
//---------------------- is selected ------------------------//
//---------------------- is selected ------------------------//

                    // public

int SimpleSelect::isSelected(long i)  const
{
  char value = getSelectValue(i);
  switch(value)
      {
      case SELECT_MAYBE_NO   : return FALSE;
      case SELECT_MAYBE_YES  : return TRUE;
      case SELECT_YES_ALWAYS : return TRUE;
      case SELECT_NO_ALWAYS  : return FALSE;
      case SELECT_YES_TOP    : return TRUE;
      case SELECT_YES_BOTTOM : return TRUE;
      default                : break;
      }
  return FALSE;
}



//------------------- clear selections --------------------//
//------------------- clear selections --------------------//
//------------------- clear selections --------------------//

                       // public

void SimpleSelect::clearSelections(long n)
{
  for(long i = 0; i < n; i++)
      {
      setSelectValue(i, SELECT_MAYBE_NO);
      }
  _nselect = 0;
}



//------------------- set all selections --------------------//
//------------------- set all selections --------------------//
//------------------- set all selections --------------------//

                       // public

void SimpleSelect::setAllSelections(long n)
{
  clearSelections(n);
  if(n > 0) setSelectValue(0, SELECT_YES_TOP);
  updateSelections(n);
  _nselect = n;
}



//------------------- toggle all selections --------------------//
//------------------- toggle all selections --------------------//
//------------------- toggle all selections --------------------//

                       // public

void SimpleSelect::toggleAllSelections(long n)
{
  if(n == 0) return;
  if(isSelected(0)) clearSelections (n);
  else              setAllSelections(n);
}



//-------------------- update selections -------------------------//
//-------------------- update selections -------------------------//
//-------------------- update selections -------------------------//

                       // public

void SimpleSelect::updateSelections(long n)
{
  _nselect = 0;
  char remember = SELECT_MAYBE_NO;
  for(long i = 0; i < n; i++)
      {
      char value = getSelectValue(i);
      switch(value)
          {
          case SELECT_MAYBE_NO   : setSelectValue(i, remember); break;
          case SELECT_MAYBE_YES  : setSelectValue(i, remember); break;
          case SELECT_YES_ALWAYS : break;
          case SELECT_NO_ALWAYS  : break;
          case SELECT_YES_TOP    : remember = SELECT_MAYBE_YES; break;
          case SELECT_YES_BOTTOM : remember = SELECT_MAYBE_NO;  break;
          default                : setSelectValue(i, remember); break;
          }
      if(isSelected(i)) _nselect++;
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

