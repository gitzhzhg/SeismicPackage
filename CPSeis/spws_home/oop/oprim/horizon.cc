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


//-------------------------- horizon.cc ---------------------------------//
//-------------------------- horizon.cc ---------------------------------//
//-------------------------- horizon.cc ---------------------------------//

//           implementation file for the Horizon class
//           derived from the SeveralFloatArrays class
//                       subdirectory oprim


#include "oprim/horizon.hh"
#include "oprim/floatio_wrapper.hh"
#include "str.h"
#include "swapio.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


#define  NBUF        222
#define  NUM_CARDS   80
#define  STEP        2000



//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//


Horizon::Horizon(const char *filetype)
     : SeveralFloatArrays(5, filetype),
       _selected   (TRUE),
       _name       (str_newstr("none" )),
       _color      (str_newstr("red"  )),
       _picktype   (str_newstr("unset")),
       _units      (str_newstr("unset"))
{
}



//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//


Horizon::~Horizon()
{
  free(_name);
  free(_color);
  free(_picktype);
  free(_units);
}



//------------------------------ get values -----------------------------//
//------------------------------ get values -----------------------------//
//------------------------------ get values -----------------------------//

   // public.



//----------------------------- set values -------------------------------//
//----------------------------- set values -------------------------------//
//----------------------------- set values -------------------------------//

   // public.

void Horizon::setName(const char *name)
{
  assert(name);
  char buffer[77];
  str_remove_all_blanks(buffer, (char*)name);
  if(buffer[0] == '\0') return;
  free(_name);
  _name = str_newstr(buffer);
}


void Horizon::setColor(const char *color)
{
  assert(color);
  char buffer[77];
  str_remove_all_blanks(buffer, (char*)color);
  if(buffer[0] == '\0') return;
  free(_color);
  _color = str_newstr(buffer);
}


void Horizon::setPicktype(const char *picktype)
{
  assert(picktype);
  char buffer[77];
  str_remove_all_blanks(buffer, (char*)picktype);
  if(buffer[0] == '\0') return;
  free(_picktype);
  _picktype = str_newstr(buffer);
}


void Horizon::setUnits(const char *units)
{
  assert(units);
  char buffer[77];
  str_remove_all_blanks(buffer, (char*)units);
  if(buffer[0] == '\0') return;
  free(_units);
  _units = str_newstr(buffer);
}


/*
//----------------------- virtual validate header -------------------------//
//----------------------- virtual validate header -------------------------//
//----------------------- virtual validate header -------------------------//


int Horizon::virtualValidateHeader (class FloatioWrapper* floatio, char* info)
{
  return FALSE;
}



//----------------------- virtual import header -------------------------//
//----------------------- virtual import header -------------------------//
//----------------------- virtual import header -------------------------//


int Horizon::virtualImportHeader   (class FloatioWrapper* floatio, char* msg)
{
  int indx = floatio->findField("pick");
  setName    (floatio->getScalarString("name" ));
  setColor   (floatio->getScalarString("color"));
  setPicktype(floatio->getFieldtype   (indx   ));
  setUnits   (floatio->getUnits       (indx   ));
  if(strcmp(_units, "seconds") == 0 && 
     strcmp(floatio->getConverter(indx), "1000") == 0)
      {
      setUnits("millisec");
      }
  return FALSE;
}



//----------------------- virtual export header -------------------------//
//----------------------- virtual export header -------------------------//
//----------------------- virtual export header -------------------------//


int Horizon::virtualExportHeader   (class FloatioWrapper* floatio, char* msg)
{
  int indx = floatio->findField("pick");
  floatio->setScalarString("name" , _name    );
  floatio->setScalarString("color", _color   );
  floatio->setFieldtype   (indx,    _picktype);
  floatio->setUnits       (indx,    _units   );
  return FALSE;
}
*/


//-------------------- virtual read binary header ------------------------//
//-------------------- virtual read binary header ------------------------//
//-------------------- virtual read binary header ------------------------//

     // protected.

int Horizon::virtualReadBinaryHeader(FILE *stream, char* /*msg*/)
{
  char buffer[NBUF];
/***
  int num = fread(buffer, NBUF, 1, stream);
  int error = (num != 1);
***/
  int num = swapio_fread_char(buffer, NBUF, stream);
  int error = (num != NBUF);
  if(!error)
      {
      free(_color);
      _color = str_newstr(buffer);
      }
  return error;
}



//-------------------- virtual save binary header ------------------------//
//-------------------- virtual save binary header ------------------------//
//-------------------- virtual save binary header ------------------------//

     // protected.

int Horizon::virtualSaveBinaryHeader(FILE *stream, char* /*msg*/)
{
  char buffer[NBUF];
  strcpy(buffer, _color);
/***
  int num = fwrite(buffer, NBUF, 1, stream);
  int error = (num != 1);
***/
  int num = swapio_fwrite_char(buffer, NBUF, stream);
  int error = (num != NBUF);
  return error;
}



//---------------------------------- end -----------------------------------//
//---------------------------------- end -----------------------------------//
//---------------------------------- end -----------------------------------//

