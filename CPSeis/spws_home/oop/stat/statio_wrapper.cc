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

//-------------------------- statio_wrapper.cc ------------------------------//
//-------------------------- statio_wrapper.cc ------------------------------//
//-------------------------- statio_wrapper.cc ------------------------------//

//             implementation file for the StatioWrapper class
//                 derived from the PjarFileSupport class
//                          subdirectory stat


#include "stat/statio_wrapper.hh"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


static const int   nfields  = 3;
static const char *fields[] = { "xcoord", "ycoord", "static" };

static const int   nrequired  = 1;
static const char *required[] = { "static" };


//-------------------- constructor and destructor ----------------------//
//-------------------- constructor and destructor ----------------------//
//-------------------- constructor and destructor ----------------------//


StatioWrapper::StatioWrapper(int io)
       : PjarFileSupport(io, "static", fields, nfields, required, nrequired)
{
}



StatioWrapper::~StatioWrapper()
{
}



//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//


#if NEED_UNDERSCORE
#define statio_frou_verify                  statio_frou_verify_
#define statio_frou_augment                 statio_frou_augment_
#define statio_frou_read_header             statio_frou_read_header_
#define statio_frou_read_file               statio_frou_read_file_
#define statio_frou_write_file              statio_frou_write_file_
#define statio_frou_scan_foreign            statio_frou_scan_foreign_
#define statio_frou_read_foreign            statio_frou_read_foreign_
#elif NEED_CAPITALS
#define statio_frou_verify                  STATIO_FROU_VERIFY
#define statio_frou_augment                 STATIO_FROU_AUGMENT
#define statio_frou_read_header             STATIO_FROU_READ_HEADER
#define statio_frou_read_file               STATIO_FROU_READ_FILE
#define statio_frou_write_file              STATIO_FROU_WRITE_FILE
#define statio_frou_scan_foreign            STATIO_FROU_SCAN_FOREIGN
#define statio_frou_read_foreign            STATIO_FROU_READ_FOREIGN
#endif


//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//

extern "C" {

void statio_frou_verify       (                      F90Pointer  *pjar,
                               const char *secname ,
                               int        *err     , char        *msg);

void statio_frou_augment      (                      F90Pointer  *pjar,
                               const char *secname );

void statio_frou_read_header  (const char *filename, F90Pointer  *pjar,
                               const char *secname ,
                               int        *err     , char        *msg);

void statio_frou_read_file    (const char *filename, F90Pointer  *pjar,
                               const char *secname , float       *statics,
                               int        *err     , char        *msg);

void statio_frou_write_file   (const char *filename, F90Pointer  *pjar,
                               const char *secname , const float *statics,
                               int        *err     , char        *msg);

void statio_frou_scan_foreign (const char *filename, F90Pointer  *pjar,
                               const char *secname ,
                               int        *err     , char        *msg);

void statio_frou_read_foreign (const char *filename, F90Pointer  *pjar,
                               const char *secname , float       *statics,
                               int        *err     , char        *msg);

}   // end extern "C"


//---------------------------- set nhx and nhy ---------------------------//
//---------------------------- set nhx and nhy ---------------------------//
//---------------------------- set nhx and nhy ---------------------------//


void StatioWrapper::setNhx(int value)
{
  setComboHdr("nhx", "xcoord", value);
}


void StatioWrapper::setNhy(int value)
{
  setComboHdr("nhy", "ycoord", value);
}



//---------------- virtual functions overriding PjarFileSupport -------------//
//---------------- virtual functions overriding PjarFileSupport -------------//
//---------------- virtual functions overriding PjarFileSupport -------------//


void StatioWrapper::setHdr (int indx, int value)
{
  const char *field = getField(indx);
  if(strcmp(field, "xcoord") == 0)
      {
      setComboHdr("nhx", "xcoord", value);
      }
  else if(strcmp(field, "ycoord") == 0)
      {
      setComboHdr("nhy", "ycoord", value);
      }
  else
      {
      PjarFileSupport::setHdr(indx, value);
      }
}



int StatioWrapper::virtualValidate (const char *filename, char *msg)
{
  int err;
  statio_frou_read_header (filename, pjar(), defname(), &err, msg);
  return (err != STATUS_OK);
}



void StatioWrapper::virtualAugment ()
{
  statio_frou_augment (pjar(), secname());
}



int StatioWrapper::virtualVerify (char *msg)
{
  int err;
  statio_frou_verify (pjar(), secname(), &err, msg);
  return (err != STATUS_OK);
}



//----------------------- read and write files ---------------------------//
//----------------------- read and write files ---------------------------//
//----------------------- read and write files ---------------------------//


void StatioWrapper::readFile    (const char *filename, float *statics,
                                 int *err, char *msg)
{
  statio_frou_read_file (filename, pjar(), secname(), statics, err, msg);
}



void StatioWrapper::writeFile   (const char *filename, const float *statics,
                                 int *err, char *msg)
{
  statio_frou_write_file (filename, pjar(), secname(), statics, err, msg);
}



void StatioWrapper::scanForeign (const char *filename, int *err, char *msg)
{
  statio_frou_scan_foreign (filename, pjar(), secname(), err, msg);
}



void StatioWrapper::readForeign (const char *filename, float *statics,
                                 int *err, char *msg)
{
  statio_frou_read_foreign (filename, pjar(), secname(), statics, err, msg);
}



//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

