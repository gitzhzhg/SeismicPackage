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

//------------------------ sl_column_number.cc ----------------------//
//------------------------ sl_column_number.cc ----------------------//
//------------------------ sl_column_number.cc ----------------------//

//        implementation file for the SLColumnNumber class
//                 derived from the SLpOption class
//                       subdirectory sl


#include "sl/sl_column_number.hh"
#include "oprim/file_support_interface.hh"
#include "named_constants.h"
#include "str.h"
#include <stream.h>
#include <ctype.h>
#include <assert.h>



//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//


static void column_trap                                
              (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{                                                                      
  SLColumnNumber *THIS = (SLColumnNumber*)data;                
  int column = (int)newvar;                                  
  int indx   = THIS->interface()->findField(THIS->field());
  THIS->interface()->setColumnNumber(indx, column);
}



//----------------------------- update functions ---------------------------//
//----------------------------- update functions ---------------------------//
//----------------------------- update functions ---------------------------//


static long column_upfun(void *data)                     
{                                                         
  SLColumnNumber *THIS = (SLColumnNumber*)data;                
  if(!THIS->interface()->fieldsArePresent()) return 1;
  return THIS->interface()->findField(THIS->field()) + 1;
}



//----------------------- sense update functions ---------------------------//
//----------------------- sense update functions ---------------------------//
//----------------------- sense update functions ---------------------------//


static long column_sense(void *data)                     
{                                                         
  SLColumnNumber *THIS = (SLColumnNumber*)data;                
  if(THIS->interface()->numHeaderSections() == 0) return FALSE;
  const char *encoding = THIS->interface()->getEncoding();
  if(strcmp(encoding, "oldcps") == 0) return FALSE;
  if(strcmp(encoding,       "") == 0) return FALSE;
  return TRUE;
}



//------------------------------ constructor -----------------------------//
//------------------------------ constructor -----------------------------//
//------------------------------ constructor -----------------------------//


SLColumnNumber::SLColumnNumber (SLDelay *slparent, const char *field,
                                FileSupportInterface *interface,
                                int ncolumns)
            : SLpOption(slparent, (char*)field, 0, (char*)field),
                      _field       (str_newstr(field)),
                      _interface   (interface)
{
  assert(_field && _interface);

  for(int column = 1; column <= ncolumns; column++)
      {
      char buffer[22];
      sprintf(buffer, "  %d  ", column);
      addOption(buffer, column);
      }

  setItrap      (  column_trap  , this);
  setupIvarFun  (  column_upfun , this);
  setupSenseFun (  column_sense , this);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


SLColumnNumber::~SLColumnNumber()
{
  free(_field);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
