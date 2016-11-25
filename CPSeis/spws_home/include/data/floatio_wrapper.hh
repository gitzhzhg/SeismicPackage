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

//-------------------------- floatio_wrapper.hh ----------------------------//
//-------------------------- floatio_wrapper.hh ----------------------------//
//-------------------------- floatio_wrapper.hh ----------------------------//

//              header file for the FloatioWrapper class
//               derived from the PjarFileSupport class
//                          subdirectory oprim


       // This class provides self defining ascii file I/O for
       // any data object which maintains a fixed number of
       // linked floating point arrays, such as horizons, faults,
       // or well logs.


#ifndef _FLOATIO_WRAPPER_HH_
#define _FLOATIO_WRAPPER_HH_

#include "oprim/pjar_file_support.hh"


class FloatioWrapper : virtual public PjarFileSupport
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//

private:

  const char **_fields;      // list of field names (or NULL).
  const int    _nfields;     // number of field names (or 0).

private:  // these change with each file read or written.

  int    _ncolumns;    // number of columns on the file.
  float *_vline;       // values on one line read or written.
  int   *_colindex;    // column indices for each named field.


//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//

public:  // io must be 0 for input and 1 for output.
         // defname should be "horizon" or "fault" or "well-log" or similar.
         // if fields[nfields] is specified, exactly these fields are used,
         //  and the names correspond to the columnar arrays to read or write.
         // otherwise, field names are irrelevant and the number can vary.
         // fields[] must be in permanent static memory in the calling program.

           FloatioWrapper (int io, const char *defname,
                           const char **fields   = NULL, int nfields   = 0,
                           const char **required = NULL, int nrequired = 0);
  virtual ~FloatioWrapper ();


//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//

protected:

  virtual int   virtualValidate (const char *filename, char *msg);
  virtual void  virtualAugment  ();
  virtual int   virtualVerify   (char *msg);

public:

  virtual int   allowOldcps         ()  const  { return FALSE; }
  virtual int   allowAscii          ()  const  { return TRUE ; }
  virtual int   allowBinary         ()  const  { return FALSE; }
  virtual int   allowHybrid         ()  const  { return FALSE; }

  virtual int   showFieldtypes      ()  const  { return TRUE ; }
  virtual int   showUnits           ()  const  { return TRUE ; }
  virtual int   showHdrs            ()  const  { return FALSE; }
  virtual int   showDefaults        ()  const  { return FALSE; }
  virtual int   showComments        ()  const  { return FALSE; }
  virtual int   showWidths          ()  const  { return TRUE ; }
  virtual int   showConverters      ()  const  { return FALSE; }
  virtual int   showMaxchars        ()  const  { return TRUE ; }

  virtual int   switchFields        (int indx)  { return     2; }
  virtual int   switchFieldtypes    (int indx)  { return     2; }
  virtual int   switchUnits         (int indx)  { return     2; }
  virtual int   switchColumnNumbers (int indx)  { return     1; }


//------------------------ read and write disk files --------------------//
//------------------------ read and write disk files --------------------//
//------------------------ read and write disk files --------------------//

public:   // read and write disk file.
          // openInputFile reads file using parameters from pickle jar.
          // openOutputFile uses and writes parameters from pickle jar.
          // these set msg.
          // these set err = STATUS_OK, STATUS_ERROR, STATUS_EOF.
          // these return error = TRUE or FALSE.
          // values[] must be dimensioned[nvalues].

  int   openInputFile  (const char *filename, char *msg);
  int   openOutputFile (const char *filename, char *msg);
  void  closeFile      ();

  void  readLine  (int *err, char *msg, float       *values, int nvalues);
  void  writeLine (int *err, char *msg, const float *values, int nvalues);


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

};

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
