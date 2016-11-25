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

//-------------------------- floatio_wrapper.cc ---------------------------//
//-------------------------- floatio_wrapper.cc ---------------------------//
//-------------------------- floatio_wrapper.cc ---------------------------//

//           implementation file for the FloatioWrapper class
//                derived from the PjarFileSupport class
//                          subdirectory oprim


#include "oprim/floatio_wrapper.hh"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//------------------ constructor and destructor -----------------------//
//------------------ constructor and destructor -----------------------//
//------------------ constructor and destructor -----------------------//


FloatioWrapper::FloatioWrapper(int io, const char *defname,
                               const char **fields  , int nfields,
                               const char **required, int nrequired)
       : PjarFileSupport (io, defname, fields, nfields, required, nrequired),
                  _fields               (fields),
                  _nfields              (nfields),
                  _ncolumns             (0),
                  _vline                (NULL),
                  _colindex             (NULL)
{
}



FloatioWrapper::~FloatioWrapper()
{
  closeFile();
}



//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//


#if NEED_UNDERSCORE
#define floatio_frou_verify                  floatio_frou_verify_
#define floatio_frou_augment                 floatio_frou_augment_
#define floatio_frou_open_read               floatio_frou_open_read_
#define floatio_frou_open_write              floatio_frou_open_write_
#define floatio_frou_open_foreign            floatio_frou_open_foreign_
#define floatio_frou_close                   floatio_frou_close_
#define floatio_frou_read_line               floatio_frou_read_line_  
#define floatio_frou_write_line              floatio_frou_write_line_  
#elif NEED_CAPITALS
#define floatio_frou_verify                  FLOATIO_FROU_VERIFY
#define floatio_frou_augment                 FLOATIO_FROU_AUGMENT
#define floatio_frou_open_read               FLOATIO_FROU_OPEN_READ  
#define floatio_frou_open_write              FLOATIO_FROU_OPEN_WRITE 
#define floatio_frou_open_foreign            FLOATIO_FROU_OPEN_FOREIGN
#define floatio_frou_close                   FLOATIO_FROU_CLOSE
#define floatio_frou_read_line               FLOATIO_FROU_READ_LINE
#define floatio_frou_write_line              FLOATIO_FROU_WRITE_LINE
#endif


//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//

extern "C" {

void floatio_frou_verify       (F90Pointer *pjar  , const char *secname,
                                int        *err   , char       *msg);

void floatio_frou_augment      (F90Pointer *pjar  , const char *secname);

void floatio_frou_open_read    (F90Pointer *fpoint, const char *filename,
                                F90Pointer *pjar  , const char *secname,
                                int        *err   , char       *msg);

void floatio_frou_open_write   (F90Pointer *fpoint, const char *filename,
                                F90Pointer *pjar  , const char *secname,
                                int        *err   , char       *msg);

void floatio_frou_open_foreign (F90Pointer *fpoint, const char *filename,
                                F90Pointer *pjar  , const char *secname,
                                int        *err   , char       *msg);

void floatio_frou_close        (F90Pointer *fpoint);

void floatio_frou_read_line    (F90Pointer  *fpoint ,
                                int         *err    , char      *msg,
                                float       *vline  , const int *ncolumns);

void floatio_frou_write_line   (F90Pointer  *fpoint ,
                                int         *err    , char      *msg,
                                const float *vline  , const int *ncolumns);

}   // end extern "C"



//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//


int FloatioWrapper::virtualValidate (const char *filename, char *msg)
{
  int err;
  floatio_frou_open_read(fpoint(), filename, pjar(), defname(), &err, msg);
  floatio_frou_close    (fpoint());
  return (err != STATUS_OK);
}



void FloatioWrapper::virtualAugment ()
{
  floatio_frou_augment (pjar(), secname());
}



int FloatioWrapper::virtualVerify (char *msg)
{
  int err;
  floatio_frou_verify (pjar(), secname(), &err, msg);
  return (err != STATUS_OK);
}



//----------------------- open input file -------------------------//
//----------------------- open input file -------------------------//
//----------------------- open input file -------------------------//


int   FloatioWrapper::openInputFile       (const char *filename, char *msg)
{
  assert(!_vline);
  assert(!_colindex);

  int error = verifyParameters(msg);
  if(error) return TRUE;

  int err;
  floatio_frou_open_foreign(fpoint(), filename, pjar(), secname(), &err, msg);

  if(err != STATUS_OK)
      {
      closeFile();
      return TRUE;
      }

  _ncolumns = getNcolumns();
  _vline    = new float [_ncolumns];

  if(_fields)
      {
      _colindex = new int [_nfields];
      for(int icol = 0; icol < _nfields; icol++)
          {
          _colindex[icol] = findField(_fields[icol]);
          }
      }
  return FALSE;
}


//----------------------- open output file -------------------------//
//----------------------- open output file -------------------------//
//----------------------- open output file -------------------------//


int   FloatioWrapper::openOutputFile      (const char *filename, char *msg)
{
  assert(!_vline);
  assert(!_colindex);

  int error = verifyParameters(msg);
  if(error) return TRUE;

  int err;
  floatio_frou_open_write(fpoint(), filename, pjar(), secname(), &err, msg);

  if(err != STATUS_OK)
      {
      closeFile();
      return TRUE;
      }

  _ncolumns = getNcolumns();
  _vline    = new float [_ncolumns];

  if(_fields)
      {
      _colindex = new int [_nfields];
      for(int icol = 0; icol < _nfields; icol++)
          {
          _colindex[icol] = findField(_fields[icol]);
          }
      }

  return FALSE;
}


//----------------------- close file -------------------------//
//----------------------- close file -------------------------//
//----------------------- close file -------------------------//


void  FloatioWrapper::closeFile()
{
  if(_vline   ) delete [] _vline;
  if(_colindex) delete [] _colindex;
  _vline    = NULL;
  _colindex = NULL;
  floatio_frou_close (fpoint());
}



//----------------------- read line -------------------------//
//----------------------- read line -------------------------//
//----------------------- read line -------------------------//


void  FloatioWrapper::readLine
                          (int *err, char *msg, float *values, int nvalues)
{
  assert(_vline);
  floatio_frou_read_line (fpoint(), err, msg, _vline, &_ncolumns);
  if(*err != STATUS_OK) return;

  if(_fields)
      {
      assert(_colindex);
      for(int icol = 0; icol < nvalues; icol++)
          {
          if(icol < _nfields)
              {
              int icol2 = _colindex[icol];
              if(icol2 >= 0 && icol2 < _ncolumns)
                  {
                  values[icol] = _vline[icol2];
                  }
              else
                  {
                  values[icol] = FNIL;
                  }
              }
          else
              {
              values[icol] = FNIL;
              }
          }
      }
  else
      {
      for(int icol = 0; icol < nvalues; icol++)
          {
          if(icol < _ncolumns)
              {
              values[icol] = _vline[icol];
              }
          else
              {
              values[icol] = FNIL;
              }
          }
      }
}


//----------------------- write line -------------------------//
//----------------------- write line -------------------------//
//----------------------- write line -------------------------//


void  FloatioWrapper::writeLine
                       (int *err, char *msg, const float *values, int nvalues)
{
  assert(_vline);

  int icol;

  if(_fields)
      {
      assert(_colindex);
      for(icol = 0; icol < _ncolumns; icol++)
          {
          _vline[icol] = FNIL;
          }
      for(icol = 0; icol < nvalues; icol++)
          {
          if(icol < _nfields)
              {
              int icol2 = _colindex[icol];
              if(icol2 >= 0 && icol2 < _ncolumns)
                  {
                  _vline[icol2] = values[icol];
                  }
              }
          }
      }
  else
      {
      for(icol = 0; icol < _ncolumns; icol++)
          {
          if(icol < nvalues)
              {
              _vline[icol] = values[icol];
              }
          else
              {
              _vline[icol] = FNIL;
              }
          }
      }

  floatio_frou_write_line (fpoint(), err, msg, _vline, &_ncolumns);
}



//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

