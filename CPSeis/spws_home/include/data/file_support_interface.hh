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

//------------------------ file_support_interface.hh ---------------------//
//------------------------ file_support_interface.hh ---------------------//
//------------------------ file_support_interface.hh ---------------------//

//            header file for the FileSupportInterface class
//                      not derived from any class
//                          subdirectory oprim

// this is an abstract interface needed by SLHowReadWrite and SLStartingCards.
                 // there is no implementation file.

// It is expected that the derived class will maintain the parameters
// referenced by the virtual functions in this interface, plus any additional
// parameters which will be read or written.  The parameters are of two
// types: those which reside on the file and are copied to or from a data
// object, and those which describe how to read or write the file.

// If this interface is used for file input, the derived class should maintain
// the parameters read from the file and subsequently updated by the user
// before the file is read into a data object.

// If this interface is used for file output, the derived class should
// maintain the parameters which will be written to the file, and those
// which describe how to write the file.

// One instance of this class should be used for file input, and another
// instance for file output.  The derived class should have a lifetime
// equal to the lifetime of the application.  If this class is used for
// file output, the parameters in this class which come from a data object
// should be updated when the data object is updated or replaced.

 
#ifndef _FILE_SUPPORT_INTERFACE_HH_
#define _FILE_SUPPORT_INTERFACE_HH_

#include "named_constants.h"


class FileSupportInterface
{

//----------------------------- data ------------------------------------//
//----------------------------- data ------------------------------------//
//----------------------------- data ------------------------------------//

private:


//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//

public:

           FileSupportInterface() {}
  virtual ~FileSupportInterface() {}


//--------------------- miscellaneous virtual functions -------------------//
//--------------------- miscellaneous virtual functions -------------------//
//--------------------- miscellaneous virtual functions -------------------//

public:

  virtual int         isInput              ()   const  { return FALSE; }
  virtual int         isOutput             ()   const  { return FALSE; }
  virtual int         inputFieldIsSkipped  (int indx)  { return FALSE; }
  virtual int         wantAugment          ()   const  { return TRUE; }

  virtual int         allowOldcps          ()   const  { return TRUE; }
  virtual int         allowAscii           ()   const  { return TRUE; }
  virtual int         allowBinary          ()   const  { return TRUE; }
  virtual int         allowHybrid          ()   const  { return TRUE; }

  virtual int         numDataCards         ()          { return 0; }
  virtual const char *getDataCard          (int indx)  { return NULL; }

  virtual int         numStartingCards     ()          { return 0; }
  virtual const char *getStartingCard      (int indx)  { return NULL; }


//--------------------- get and set header sections -----------------------//
//--------------------- get and set header sections -----------------------//
//--------------------- get and set header sections -----------------------//

public:

  virtual int         numHeaderSections      ()          { return 0   ; }
  virtual const char *getActiveHeaderSection ()          { return NULL; }
  virtual const char *getHeaderSection       (int indx)  { return NULL; }
  virtual void        setActiveHeaderSection (int indx)  {}
  virtual void        setActiveHeaderSection (const char *secname) {}


//--------------------- get and set scalar parameters ---------------------//
//--------------------- get and set scalar parameters ---------------------//
//--------------------- get and set scalar parameters ---------------------//

public:

  virtual const char *getEncoding  ()  { return NULL       ; }
  virtual const char *getNilstring ()  { return NULL       ; }
  virtual int         getWrap      ()  { return 1          ; }
  virtual int         getNcolumns  ()  { return numFields(); }
  virtual int         getFirstline ()  { return INIL       ; }  // input only
  virtual int         getFillout   ()  { return FALSE      ; }  // output only
  virtual int         getNoheaders ()  { return FALSE      ; }  // output only
  virtual const char *getTemplate  ()  { return NULL       ; }  // input only

  virtual void        setEncoding  (const char *value)  {}
  virtual void        setNilstring (const char *value)  {}
  virtual void        setWrap      (int         value)  {}
  virtual void        setNcolumns  (int         value)  {}
  virtual void        setFirstline (int         value)  {}
  virtual void        setFillout   (int         value)  {}
  virtual void        setNoheaders (int         value)  {}
  virtual void        setTemplate  (const char *value)  {}

  virtual void        stepEncoding (int step)  {}


//--------------------- get and set array parameters ---------------------//
//--------------------- get and set array parameters ---------------------//
//--------------------- get and set array parameters ---------------------//

public:  // returned switch values must be 1 or 2 or any insensitive switch.
         // returned switch values = 1 will call the set functions.
         // returned switch values = 2 will call the step functions.
         // findField should return the index (column-1) of the specified
         //  field, or -1 if the field does not exist.

  virtual int         showFieldtypes      ()  const  { return TRUE ; }
  virtual int         showUnits           ()  const  { return TRUE ; }
  virtual int         showHdrs            ()  const  { return TRUE ; }
  virtual int         showDefaults        ()  const  { return TRUE ; }
  virtual int         showDescriptions    ()  const  { return FALSE; }
  virtual int         showConverters      ()  const  { return TRUE ; }
  virtual int         showMaxchars        ()  const  { return TRUE ; }
  virtual int         showWidths          ()  const  { return TRUE ; }

  virtual int         switchFields        (int indx)  { return   0; }
  virtual int         switchColumnNumbers (int indx)  { return -44; }
  virtual int         switchFieldtypes    (int indx)  { return   1; }
  virtual int         switchUnits         (int indx)  { return   1; }
  virtual int         switchHdrs          (int indx)  { return   1; }
  virtual int         switchDefaults      (int indx)  { return   1; }

  virtual int         findField        (const char *field)  { return -1; }
  virtual int         fieldsArePresent ()                   { return FALSE; }
  virtual int         fieldIsActive    (int indx)           { return TRUE; }

  virtual int         numFields        ()          { return 0   ; }
  virtual const char *getField         (int indx)  { return NULL; }
  virtual const char *getFieldtype     (int indx)  { return NULL; }
  virtual const char *getUnits         (int indx)  { return NULL; }
  virtual int         getHdr           (int indx)  { return 0   ; }
  virtual const char *getDefault       (int indx)  { return NULL; }
  virtual const char *getDescription   (int indx)  { return NULL; }
  virtual const char *getConverter     (int indx)  { return NULL; }  // input
  virtual int         getMaxchars      (int indx)  { return 0   ; }  // input
  virtual int         getWidth         (int indx)  { return 0   ; }  // output

  virtual void        setField         (int indx, const char *value)  {}
  virtual void        setColumnNumber  (int indx, int         value)  {}
  virtual void        setFieldtype     (int indx, const char *value)  {}
  virtual void        setUnits         (int indx, const char *value)  {}
  virtual void        setHdr           (int indx, int         value)  {}
  virtual void        setDefault       (int indx, const char *value)  {}
  virtual void        setDescription   (int indx, const char *value)  {}
  virtual void        setConverter     (int indx, const char *value)  {}

  virtual void        stepField        (int indx, int step)  {}
  virtual void        stepColumnNumber (int indx, int step)  {}
  virtual void        stepFieldtype    (int indx, int step)  {}
  virtual void        stepUnits        (int indx, int step)  {}
  virtual void        stepHdr          (int indx, int step)  {}
  virtual void        stepDefault      (int indx, int step)  {}
  virtual void        stepConverter    (int indx, int step)  {}
  virtual void        stepMaxchars     (int indx, int step)  {}
  virtual void        stepWidth        (int indx, int step)  {}


//------------------------- end of functions -----------------------------//
//------------------------- end of functions -----------------------------//
//------------------------- end of functions -----------------------------//

} ;

#endif

//------------------------------- end -----------------------------------//
//------------------------------- end -----------------------------------//
//------------------------------- end -----------------------------------//
