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

//-------------------------- velio_wrapper.hh ------------------------------//
//-------------------------- velio_wrapper.hh ------------------------------//
//-------------------------- velio_wrapper.hh ------------------------------//

//              header file for the VelioWrapper class
//              derived from the PjarFileSupport class
//                        subdirectory vf


#ifndef _VELIO_WRAPPER_HH_
#define _VELIO_WRAPPER_HH_

#include "oprim/pjar_file_support.hh"


class VelioWrapper : public PjarFileSupport
{

//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//

private:


//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//

public:  // io must be 0 for input and 1 for output.

           VelioWrapper (int io, const char *defname = "velocity");
  virtual ~VelioWrapper ();


//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//

protected:

  virtual int   virtualValidate (const char *filename, char *msg);
  virtual void  virtualAugment  ();
  virtual int   virtualVerify   (char *msg);

public:

  virtual int   allowOldcps ()  const  { return TRUE; }
  virtual int   allowAscii  ()  const  { return TRUE; }
  virtual int   allowBinary ()  const  { return TRUE; }
  virtual int   allowHybrid ()  const  { return FALSE; }

  virtual int   showFieldtypes      ()  const  { return FALSE; }
  virtual int   showUnits           ()  const  { return FALSE; }
  virtual int   showHdrs            ()  const  { return FALSE; }
  virtual int   showDefaults        ()  const  { return FALSE; }
  virtual int   showDescriptions    ()  const  { return TRUE ; }
  virtual int   showWidths          ()  const  { return TRUE ; }
  virtual int   showConverters      ()  const  { return TRUE ; }
  virtual int   showMaxchars        ()  const  { return TRUE ; }

  virtual int   switchFields        (int indx)  { return   2; }
  virtual int   switchColumnNumbers (int indx)  { return   1; }
  virtual int   switchFieldtypes    (int indx)  { return   1; }
  virtual int   switchUnits         (int indx)  { return   1; }
  virtual int   switchHdrs          (int indx)  { return (isInput() ? 1 : -5); }
  virtual int   switchDefaults      (int indx)  { return   1; }

  virtual const char *getDescription   (int indx);

/****
  ////// not needed:
  virtual void  stepFieldtype       (int indx, int step);
  virtual void  stepUnits           (int indx, int step);
  virtual void  stepHdr             (int indx, int step);
  virtual void  stepDefault         (int indx, int step);
****/

  ////// overridden a second time:

  virtual void  setFieldtype        (int indx, const char *value);
  virtual void  setUnits            (int indx, const char *value);
  virtual void  setHdr              (int indx, int         value);
  virtual void  setDefault          (int indx, const char *value);


//-------------- new functions accessing the pickle jar -----------------//
//-------------- new functions accessing the pickle jar -----------------//
//-------------- new functions accessing the pickle jar -----------------//

public:  // get values.

  const char        *getName            ();
  float              getDefaultXcoord   ();
  float              getDefaultYcoord   ();
  int                getType            ();
  const char        *getTypeSymbol      ();
  int                getNhx             ();
  int                getNhy             ();
  int                getMoveoutOrder    ();
  float              getNhosign         ();
  float              getNhoexp          ();
  int                getDistanceUnits   ();
  const char        *getUnitsSymbol     ();
  const char        *getAttributeName   ();
  const char        *getAttributeUnits  ();
  const char        *getTimeDepthUnits  ();

public:  // set values.

  void        setName             (const char *value);
  void        setDefaultXcoord    (float       value);
  void        setDefaultYcoord    (float       value);
  void        setType             (int         value);
  void        setTypeFromSymbol   (const char *value);
  void        setNhx              (int         value);
  void        setNhy              (int         value);
  void        setMoveoutOrder     (int         value);
  void        setDistanceUnits    (int         value);
  void        setUnitsFromSymbol  (const char *value);
  void        setAttributeName    (const char *value);
  void        setAttributeUnits   (const char *value);
  void        setTimeDepthUnits   (const char *value);


//------------------------ read and write disk files --------------------//
//------------------------ read and write disk files --------------------//
//------------------------ read and write disk files --------------------//

public:   // read and write disk file.
          // openInputFile reads file using parameters from pickle jar.
          // openOutputFile uses and writes parameters from pickle jar.
          // these set msg.
          // these return error = TRUE or FALSE.

  int   openInputFile  (const char *filename, char *msg);
  int   openOutputFile (const char *filename, char *msg);
  void  closeFile      ();

public:   // read or write a velocity function.
          // maxpicks is the allocated size of tpicks[] and vpicks[].
          // these set msg.
          // these set err = STATUS_OK, STATUS_ERROR, STATUS_EOF.

  void readVelfun  (float *xcoord , float *ycoord,
                    int   *npicks , int    maxpicks,
                    float *tpicks , float *vpicks,
                    int   *err    , char  *msg,
                    char  *velname, char  *veltype,
                    char  *project, char  *line,
                    char  *rdate  , char  *pdate,
                    char  *userid , char  *comment);

  void writeVelfun (      float  xcoord ,       float  ycoord,
                          int    npicks ,
                    const float *tpicks , const float *vpicks,
                          int   *err    ,       char  *msg,
                    const char  *velname, const char  *veltype,
                    const char  *project, const char  *line,
                    const char  *rdate  , const char  *pdate,
                    const char  *userid , const char  *comment);


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

};

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
