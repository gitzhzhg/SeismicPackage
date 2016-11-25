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

//-------------------------- statio_wrapper.hh ------------------------------//
//-------------------------- statio_wrapper.hh ------------------------------//
//-------------------------- statio_wrapper.hh ------------------------------//

//              header file for the StatioWrapper class
//              derived from the PjarFileSupport class
//                        subdirectory stat


//    This class reads and writes CPS static files.
//    Separate instances should be used for each of the following:
//     (1) input files.
//     (2) data to be written to output files.
//     (3) existing output files which will be overwritten.


#ifndef _STATIO_WRAPPER_HH_
#define _STATIO_WRAPPER_HH_


#include "oprim/pjar_file_support.hh"


class StatioWrapper : public PjarFileSupport
{

//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//

private:

//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//

public:  // io must be 0 for input and 1 for output.

           StatioWrapper (int io);
  virtual ~StatioWrapper ();

  void setNhx (int value);   // sets "hdrs" array element plus "nhx".
  void setNhy (int value);   // sets "hdrs" array element plus "nhy".


//------------- virtual functions overriding PjarFileSupport ---------------//
//------------- virtual functions overriding PjarFileSupport ---------------//
//------------- virtual functions overriding PjarFileSupport ---------------//

protected:

  virtual int   virtualValidate (const char *filename, char *msg);
  virtual void  virtualAugment  ();
  virtual int   virtualVerify   (char *msg);

public:

  virtual int   allowOldcps ()  const  { return TRUE; }
  virtual int   allowAscii  ()  const  { return TRUE; }
  virtual int   allowBinary ()  const  { return TRUE; }
  virtual int   allowHybrid ()  const  { return TRUE; }

  virtual int   showFieldtypes      ()  const  { return FALSE; }
  virtual int   showUnits           ()  const  { return FALSE; }
  virtual int   showHdrs            ()  const  { return TRUE ; }
  virtual int   showDefaults        ()  const  { return FALSE; }
  virtual int   showComments        ()  const  { return FALSE; }
  virtual int   showWidths          ()  const  { return TRUE ; }
  virtual int   showConverters      ()  const  { return TRUE ; }
  virtual int   showMaxchars        ()  const  { return TRUE ; }

  virtual int   switchFields        (int indx)  { return   2; }
  virtual int   switchColumnNumbers (int indx)  { return   1; }
  virtual int   switchFieldtypes    (int indx)  { return   1; }
  virtual int   switchUnits         (int indx)  { return   1; }
  virtual int   switchHdrs          (int indx)  { return (isInput() ? 1 : -5); }
  virtual int   switchDefaults      (int indx)  { return   1; }

/****
  ////// not needed:
  virtual void        stepFieldtype       (int indx, int step);
  virtual void        stepUnits           (int indx, int step);
  virtual void        stepHdr             (int indx, int step);
  virtual void        stepDefault         (int indx, int step);
****/

  ////// overridden a second time:
           // sets "hdrs" array element plus either "nhx" or "nhy".

  virtual void        setHdr              (int indx, int value);


//-------------------------- read or write file ---------------------------//
//-------------------------- read or write file ---------------------------//
//-------------------------- read or write file ---------------------------//

public:   // read and write disk file.
          // scanForeign reads file using parameters from pickle jar.
          // readForeign reads file using parameters from pickle jar.
          // readFile  puts parameters from file into pickle jar.
          // writeFile uses and writes parameters from pickle jar.
          // these set msg.
          // these set err = STATUS_OK, STATUS_ERROR, STATUS_EOF.

public:  // statics[] must be allocated to sufficient memory before calling
         //  readFile or readForeign.

  void readFile    (const char *filename, float       *statics,
                    int *err, char *msg);

  void writeFile   (const char *filename, const float *statics,
                    int *err, char *msg);

  void scanForeign (const char *filename,
                    int *err, char *msg);

  void readForeign (const char *filename, float       *statics,
                    int *err, char *msg);


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

};

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
