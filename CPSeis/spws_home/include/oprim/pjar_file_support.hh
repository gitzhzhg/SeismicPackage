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

//------------------------ pjar_file_support.hh -------------------------//
//------------------------ pjar_file_support.hh -------------------------//
//------------------------ pjar_file_support.hh -------------------------//

//               header file for the PjarFileSupport class
//              derived from the FileSupportInterface class
//                          subdirectory oprim

       // This is a base class for I/O classes such as StatioWrapper
       // and VelioWrapper, which interface to F90 I/O classes.
       // The derived class should have the life of the application.


       // Separate instances should be used for input and output.
       //
       // If used for input, the pickle jar contents are used for:
       //  (1) validating a potential input file.
       //  (2) modifying the input file parameters.
       //  (3) using the input file parameters to read the file.
       //
       // If used for output, the pickle jar contents are used for:
       //  (1) setting the output file parameters.
       //  (2) using the output file parameters to write the file.


       // This class owns an instance of PjarWrapper, rather than being
       // derived from PjarWrapper, in order to remove any possibility
       // of calling any PjarWrapper functions directly.


       // This class checks and guarantees that int, float, and double
       // are the same as INTEGER, REAL, and DOUBLE PRECISION to
       // simplify writing the interface between C and Fortran.
       // So far we have been safe in doing this.  If in the future
       // an exception is found, interfaces between C and Fortran
       // will have to be modified.


       // Warning: The functions which return const char* return a pointer
       // to static memory whose contents will be replaced the next time
       // the same function (or another similar function) is called.
       // Therefore these contents must be used befor calling the same
       // function (or another similar function) again.


#ifndef _PJAR_FILE_SUPPORT_HH_
#define _PJAR_FILE_SUPPORT_HH_

#include "named_constants.h"
#include "c2f_interface.h"
#include "oprim/file_support_interface.hh"


class PjarFileSupport : public FileSupportInterface
{

//------------------------------- data -----------------------------------//
//------------------------------- data -----------------------------------//
//------------------------------- data -----------------------------------//

public:  // these are used for the returned error flag (same as in FIO).

  enum { STATUS_OK = 0, STATUS_ERROR = -7, STATUS_EOF = -1 };

private:

  int                 _io;           // 0 for input and 1 for output.
  char                _defname[44];  // default section name.
  char                _secname[44];  // current active section name.
  const char        **_fields;       // pointer supplied to this class.
  const int           _nfields;      // number of _fields (active fields).
  const char        **_required;     // pointer supplied to this class.
  const int           _nrequired;    // number of _required (required fields).
  class PjarWrapper  *_pjar;         // owned by this class.
  class GenericCards *_cards;        // owned by this class.
  F90Pointer          _fpoint;       // pointer to the F90 I/O routine.


//---------------------------- functions --------------------------------//
//---------------------------- functions --------------------------------//
//---------------------------- functions --------------------------------//

public:  // io = 0 for input and 1 for output.
         // defname = default header section name.
         // fields[nfields] = list of fields considered to be active.
         // fields[] must be in permanent static memory in the calling program.
         // if fields[nfields] is not specified, all fields are active.
         // required[nrequired] = list of required fields.
         // required[] must be in permanent static memory in calling program.
         // if required[nrequired] is not specified, no fields are required.

           PjarFileSupport (int io, const char *defname,
                            const char **fields   = NULL, int nfields   = 0,
                            const char **required = NULL, int nrequired = 0);
  virtual ~PjarFileSupport ();

  void print();
  void clear();

public:  // these automatically access the HISTORY section without changing
         // the active section.

  int         numHistoryCards  ();       
  const char *getHistoryCard   (int indx);

  void getHistoryCardsFromPjar (class HistoryCards *history);
  void putHistoryCardsIntoPjar (const HistoryCards *history, int skiphist);

public:  // in case the starting card can be modified for user convenience.

  void replaceStartingCard (int index, const char *card);

protected:  // for derived class to pass to Fortran90 I/O routines.

  F90Pointer  *fpoint ()  { return &_fpoint; }
  F90Pointer  *pjar   ();
  const char  *secname()  { return _secname; }  // when reading or writing file.
  const char  *defname()  { return _defname; }  // when validating a file.

public:  // to validate an input file.
         // this function may set msg and will return error true or false.
         // msg should be preset before calling this function.
         // if retain is true, the previous contents of the pickle jar will
         //  be retained if the file is not found or the filename is blank
         //  or the file is a foreign file.
         // this function valls virtualValidate() and virtualAugment().

  int validateFile (const char *filename, char *msg, int retain = FALSE);

public:  // to verify parameters when preparing to read or save a file.
         // this function may set msg and will return error true or false.
         // msg should be preset before calling this function.
         // this function valls virtualVerify().

  int verifyParameters (char *msg);

public:  // to initialize output parameters at any time.
         // this function should be called after creating the object.
         // this function valls virtualAugment().

  void initializeOutputParameters();

public:  // to augment output parameters at any time.
         // normally called when the data object changes.
         // this function valls virtualAugment().

  void augmentOutputParameters();

private:

  void chooseMostLikelySection ();
  void chooseActiveSection     ();
  void privateValidateEncoding ();
  void privateInsert           (int index);
  void privateRemove           (int index);


//-------------------------- new virtual functions ------------------------//
//-------------------------- new virtual functions ------------------------//
//-------------------------- new virtual functions ------------------------//

protected:  // called from validateFile().
            // this function should put parameters from the file into the
            //  pickle jar.
            // this function may set msg and must return error true or false.
            // msg will be preset before calling this function.

  virtual int virtualValidate (const char *filename, char *msg) = 0;

protected:  // called from validateFile() and initializeOutputParameters()
            //  and augmentOutputParameters().
            // this function should augment any parameters not understood
            //  by PjarFileSupport.

  virtual void virtualAugment() {}

protected:  // called from verifyParameters().
            // this function should verify the legitimacy of any parameters
            //  not understood by PjarFileSupport, but should not change any.
            // this function may set msg and must return error true or false.
            // msg will be preset before calling this function.

  virtual int virtualVerify (char *msg)  { return FALSE; }


//-------------------------- convenience functions -------------------------//
//-------------------------- convenience functions -------------------------//
//-------------------------- convenience functions -------------------------//

public:  // returns phrase such as "old style CPS velocity file" or
         // "self defining ascii static file" or "self defining binary file"
         // or "file" or "geometry file" made from encoding and secname and
         // defname.

  const char *filetype();

public:  // get pickle jar values.
         // these always reference the active section.
         // these get the scalar value or array element.
         // SKEY is the keyword for a scalar value.
         // AKEY is the keyword for an array.
         // FIELD is the name of an array element in the fields array.

  int         keywordPresent   (const char *keyword);
  void        getGridTransform (class GridTransform *grid);

    // these return the scalar value with the specified keyword:

  const char *getScalarString  (const char *skey);      
  int         getScalarInteger (const char *skey);       
  float       getScalarFloat   (const char *skey);        
  int         getScalarLogical (const char *skey);         

    // these return the array element with the specified keyword and index:

  const char *getElementString  (const char *akey, int indx);
  int         getElementInteger (const char *akey, int indx);
  float       getElementFloat   (const char *akey, int indx);
  int         getElementLogical (const char *akey, int indx);

    // these return the array element with the specified keyword and field:

  const char *getElementString  (const char *akey, const char *field);
  int         getElementInteger (const char *akey, const char *field);
  float       getElementFloat   (const char *akey, const char *field);
  int         getElementLogical (const char *akey, const char *field);

    // these return the array element corresponding to the specified field:

  const char *getElementFieldtype      (const char *field);
  const char *getElementUnits          (const char *field);
  int         getElementHdr            (const char *field);
  const char *getElementDefaultString  (const char *field);
  int         getElementDefaultInteger (const char *field);
  float       getElementDefaultFloat   (const char *field);

    // these return the scalar value with the specified keyword:
    // if the scalar is nil, the array element corresponding to the
    //   specified field is returned instead:

  const char *getComboFieldtype      (const char *skey, const char *field);
  const char *getComboUnits          (const char *skey, const char *field);
  int         getComboHdr            (const char *skey, const char *field);
  const char *getComboDefaultString  (const char *skey, const char *field);
  int         getComboDefaultInteger (const char *skey, const char *field);
  float       getComboDefaultFloat   (const char *skey, const char *field);

public:  // set pickle jar values.
         // these always reference the active section.
         // these set the scalar value or array element.
         // SKEY is the keyword for a scalar value.
         // AKEY is the keyword for an array.
         // FIELD is the name of an array element in the fields array.

  void        removeKeyword    (const char *key);
  void        setGridTransform (const class GridTransform *grid);

    // these set the scalar value with the specified keyword:

  void setScalarString  (const char *key,           const char *value);
  void setScalarInteger (const char *key,           int         value);
  void setScalarFloat   (const char *key,           float       value);
  void setScalarLogical (const char *key,           int         value);

    // these set the array element with the specified keyword and index:

  void setElementString  (const char *akey, int indx, const char *value);
  void setElementInteger (const char *akey, int indx, int         value);
  void setElementFloat   (const char *akey, int indx, float       value);
  void setElementLogical (const char *akey, int indx, int         value);

    // these set the array element with the specified keyword and field:

  void setElementString  (const char*akey, const char*field, const char*value);
  void setElementInteger (const char*akey, const char*field, int        value);
  void setElementFloat   (const char*akey, const char*field, float      value);
  void setElementLogical (const char*akey, const char*field, int        value);

    // these set the array element corresponding to the specified field:

  void setElementFieldtype      (const char *field, const char *value);
  void setElementUnits          (const char *field, const char *value);
  void setElementHdr            (const char *field, int         value);
  void setElementDefaultString  (const char *field, const char *value);
  void setElementDefaultInteger (const char *field, int         value);
  void setElementDefaultFloat   (const char *field, float       value);

    // these set the scalar value with the specified keyword:
    // these also set the array element corresponding to the specified field:

  void setComboFieldtype     (const char*skey,const char*field,const char*val);
  void setComboUnits         (const char*skey,const char*field,const char*val);
  void setComboHdr           (const char*skey,const char*field,int        val);
  void setComboDefaultString (const char*skey,const char*field,const char*val);
  void setComboDefaultInteger(const char*skey,const char*field,int        val);
  void setComboDefaultFloat  (const char*skey,const char*field,float      val);


//------------ virtual functions not overridden by this class ------------//
//------------ virtual functions not overridden by this class ------------//
//------------ virtual functions not overridden by this class ------------//

/*****
public:  // these originate in FileSupportInterface.
         // these should be overridden by the derived class if necessary.
         // the step functions always reference the active section.

  virtual int         allowOldcps          ()  const  { return TRUE; }
  virtual int         allowAscii           ()  const  { return TRUE; }
  virtual int         allowBinary          ()  const  { return TRUE; }
  virtual int         allowHybrid          ()  const  { return TRUE; }

  virtual int         showFieldtypes       ()  const  { return TRUE ; }
  virtual int         showUnits            ()  const  { return TRUE ; }
  virtual int         showHdrs             ()  const  { return TRUE ; }
  virtual int         showDefaults         ()  const  { return TRUE ; }
  virtual int         showDescriptions     ()  const  { return FALSE; }
  virtual int         showConverters       ()  const  { return TRUE ; }
  virtual int         showMaxchars         ()  const  { return TRUE ; }
  virtual int         showWidths           ()  const  { return TRUE ; }

  virtual int         switchFields         (int indx)  const  { return   0; }
  virtual int         switchColumnNumbers  (int indx)  const  { return -44; }
  virtual int         switchFieldtypes     (int indx)  const  { return   1; }
  virtual int         switchUnits          (int indx)  const  { return   1; }
  virtual int         switchHdrs           (int indx)  const  { return   1; }
  virtual int         switchDefaults       (int indx)  const  { return   1; }

  virtual void        stepFieldtype        (int indx, int step) {}
  virtual void        stepUnits            (int indx, int step) {}
  virtual void        stepHdr              (int indx, int step) {}
  virtual void        stepDefault          (int indx, int step) {}
*****/


//--------------------- miscellaneous virtual functions -------------------//
//--------------------- miscellaneous virtual functions -------------------//
//--------------------- miscellaneous virtual functions -------------------//

public:  // these override FileSupportInterface.
         // these need not be further overridden.
         // the functions marked /// always reference the active section.

  virtual int         isInput              ()   const  { return (_io == 0); }
  virtual int         isOutput             ()   const  { return (_io == 1); }
  virtual int         inputFieldIsSkipped  (int indx);     ///

  virtual int         numDataCards         ();             ///
  virtual const char *getDataCard          (int indx);     ///

  virtual int         numStartingCards     ();       
  virtual const char *getStartingCard      (int indx);


//-------------------- get and set header sections ------------------------//
//-------------------- get and set header sections ------------------------//
//-------------------- get and set header sections ------------------------//

public:  // these override FileSupportInterface.
         // these need not be further overridden.

  virtual int         numHeaderSections      ();
  virtual const char *getActiveHeaderSection ()  { return _secname; }
  virtual const char *getHeaderSection       (int indx);
  virtual void        setActiveHeaderSection (int indx);
  virtual void        setActiveHeaderSection (const char *secname);


//-------------------- get and set scalar parameters ----------------------//
//-------------------- get and set scalar parameters ----------------------//
//-------------------- get and set scalar parameters ----------------------//

public:  // these override FileSupportInterface.
         // these may not need to be further overridden.
         // these always reference the active section.
         // the GET functions get the scalar value.
         // the SET and STEP functions set the scalar value.

  virtual const char *getEncoding  ();
  virtual const char *getNilstring ();
  virtual int         getWrap      ();
  virtual int         getNcolumns  ();
  virtual int         getFirstline ();
  virtual int         getFillout   ();
  virtual int         getNoheaders ();
  virtual const char *getTemplate  ();

  virtual void        setEncoding  (const char *value);
  virtual void        setNilstring (const char *value);
  virtual void        setWrap      (int         value);
  virtual void        setNcolumns  (int         value);
  virtual void        setFirstline (int         value);
  virtual void        setFillout   (int         value);
  virtual void        setNoheaders (int         value);
  virtual void        setTemplate  (const char *value);

  virtual void        stepEncoding (int step);


//-------------------- get and set array parameters ----------------------//
//-------------------- get and set array parameters ----------------------//
//-------------------- get and set array parameters ----------------------//

public:  // these override FileSupportInterface.
         // these may not need to be further overridden.
         // these always reference the active section.
         // findField returns the index (column-1) of the specified field.
         // the GET functions get the array element.
         // the SET and STEP functions set the array element.

  virtual int         findField        (const char *field);
  virtual int         findAddField     (const char *field);
  virtual int         fieldsArePresent ();          // returns TRUE or FALSE.
  virtual int         fieldIsActive    (int indx);  // returns TRUE or FALSE.
  virtual void        removeField      (int indx);

  virtual int         numFields        ();
  virtual const char *getField         (int indx);
  virtual const char *getFieldtype     (int indx);
  virtual const char *getUnits         (int indx);
  virtual int         getHdr           (int indx);
  virtual const char *getDefault       (int indx);
  virtual const char *getDescription   (int indx);
  virtual const char *getConverter     (int indx);
  virtual int         getMaxchars      (int indx);
  virtual int         getWidth         (int indx);

  virtual void        setField         (int indx, const char *value);
  virtual void        setColumnNumber  (int indx, int         value);
  virtual void        setFieldtype     (int indx, const char *value);
  virtual void        setUnits         (int indx, const char *value);
  virtual void        setHdr           (int indx, int         value);
  virtual void        setDefault       (int indx, const char *value);
  virtual void        setDescription   (int indx, const char *value);
  virtual void        setConverter     (int indx, const char *value);

  virtual void        stepField        (int indx, int step);
  virtual void        stepColumnNumber (int indx, int step);
  virtual void        stepConverter    (int indx, int step);
  virtual void        stepMaxchars     (int indx, int step);
  virtual void        stepWidth        (int indx, int step);


//------------------------- end of functions -----------------------------//
//------------------------- end of functions -----------------------------//
//------------------------- end of functions -----------------------------//

} ;

#endif

//------------------------------- end -----------------------------------//
//------------------------------- end -----------------------------------//
//------------------------------- end -----------------------------------//
