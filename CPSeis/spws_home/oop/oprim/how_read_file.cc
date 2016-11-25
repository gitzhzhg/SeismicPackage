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

//--------------------------- how_read_file.cc ------------------------------//
//--------------------------- how_read_file.cc ------------------------------//
//--------------------------- how_read_file.cc ------------------------------//

//             implementation file for the HowReadFile class
//                     not derived from any class
//                         subdirectory oprim



#include "oprim/how_read_file.hh"
#include "named_constants.h"
#include "str.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//--------------------------- constructor ---------------------------------//
//--------------------------- constructor ---------------------------------//
//--------------------------- constructor ---------------------------------//


HowReadFile::HowReadFile()
           :
               _encoding      (str_newstr("ascii")),
               _nilstring     (str_newstr("nil"  )),
               _templat       (str_newstr(""     )),
               _wrap          (1),
               _firstline     (1),
               _nfields       (0),
               _nmaxchars     (0),
               _fields        (NULL),
               _maxchars      (NULL)
{
}



//----------------------------- destructor -------------------------------//
//----------------------------- destructor -------------------------------//
//----------------------------- destructor -------------------------------//


HowReadFile::~HowReadFile()
{
  if(_encoding ) free(_encoding );
  if(_nilstring) free(_nilstring);
  if(_templat  ) free(_templat  );
  setNfields  (0);
  setNmaxchars(0);
}



//--------------------------------- clear ----------------------------------//
//--------------------------------- clear ----------------------------------//
//--------------------------------- clear ----------------------------------//


void HowReadFile::clear()
{
  setEncoding  ("ascii");
  setNilstring ("nil"  );
  setTemplat   (""     );
  setWrap      (1      );
  setFirstline (1      );
  setNfields   (0      );
  setNmaxchars (0      );
}



//----------------------------- get values ---------------------------------//
//----------------------------- get values ---------------------------------//
//----------------------------- get values ---------------------------------//


const char *HowReadFile::getField     (int indx)  const
{
  assert(indx >= 0 && indx < _nfields);
  return _fields[indx];
}


long        HowReadFile::getMaxchars  (int indx)  const
{
  assert(indx >= 0 && indx < _nmaxchars);
  return _maxchars[indx];
}



//----------------------------- set values ---------------------------------//
//----------------------------- set values ---------------------------------//
//----------------------------- set values ---------------------------------//


void HowReadFile::setEncoding  (const char *encoding           )
{
  assert(encoding);
  char *buffer = str_newstr(encoding);
  str_remove_all_blanks(buffer, buffer);
  str_to_lower         (buffer, buffer);
  if(strcmp(buffer, "ascii" ) == 0 ||
     strcmp(buffer, "binary") == 0 ||
     strcmp(buffer, "oldcps") == 0)
      {
      if(_encoding) free(_encoding);
      _encoding = str_newstr(buffer);
      }
  free(buffer);
}


void HowReadFile::setNilstring (const char *nilstring          )
{
  assert(nilstring);
  char *buffer = str_newstr(nilstring);
  str_remove_all_blanks(buffer, buffer);
  if(strcmp(buffer, "" ) != 0)
      {
      if(_nilstring) free(_nilstring);
      _nilstring = str_newstr(buffer);
      }
  free(buffer);
}


void HowReadFile::setTemplat   (const char *templat            )
{
  if(_templat) free(_templat);
  if(templat) _templat = str_newstr(templat);
  else        _templat = str_newstr("");
}


void HowReadFile::setWrap      (long        wrap               )
{
  _wrap = MaximumValue(wrap, 1);
}


void HowReadFile::setFirstline (long        firstline          )
{
  _firstline = MaximumValue(firstline, 1);
}


void HowReadFile::setNfields   (long        nfields            )
{
  assert(nfields >= 0);
  if(_fields)
      {
      for(int indx = 0; indx < _nfields; indx++)
          {
          if(_fields[indx]) free(_fields[indx]);
          }
      delete [] _fields;
      }
  _nfields = nfields;
  if(_nfields > 0)
      {
      _fields  = new char* [nfields];
      for(int indx = 0; indx < _nfields; indx++)
          {
          _fields[indx] = str_newstr("");
          }
      }
  else
      {
      _fields = NULL;
      }
}


void HowReadFile::setNmaxchars (long        nmaxchars          )
{
  assert(nmaxchars >= 0);
  if(_maxchars) delete [] _maxchars;
  _nmaxchars = nmaxchars;
  if(_nmaxchars > 0)
      {
      _maxchars  = new long [nmaxchars];
      for(int indx = 0; indx < _nmaxchars; indx++)
          {
          _maxchars[indx] = 0;
          }
      }
  else
      {
      _maxchars = NULL;
      }
}


void HowReadFile::setField     (const char *field    , int indx)
{
  assert(field);
  assert(indx >= 0 && indx < _nfields);
  char *buffer = str_newstr(field);
  str_remove_all_blanks(buffer, buffer);
  if(strcmp(buffer, "" ) != 0)
      {
      if(_fields[indx]) free(_fields[indx]);
      _fields[indx] = str_newstr(buffer);
      }
  free(buffer);
}


void HowReadFile::setMaxchars  (long        maxchars , int indx)
{
  assert(indx >= 0 && indx < _nmaxchars);
  _maxchars[indx] = maxchars;
}


void HowReadFile::setFields    (char * const *fields   , int nfields)
{
  assert(fields);
  setNfields(nfields);
  for(int indx = 0; indx < nfields; indx++)
      {
      setField(fields[indx], indx);
      }
}


void HowReadFile::setMaxchars  (long       *maxchars , int nmaxchars)
{
  assert(maxchars);
  setNmaxchars(nmaxchars);
  for(int indx = 0; indx < nmaxchars; indx++)
      {
      setMaxchars(maxchars[indx], indx);
      }
}


//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

