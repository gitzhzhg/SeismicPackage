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

//------------------------- horizon.hh ----------------------------//
//------------------------- horizon.hh ----------------------------//
//------------------------- horizon.hh ----------------------------//

//                 header file for the Horizon class
//             derived from the SeveralFloatArrays class
//                         subdirectory oprim

                // used for both horizons and faults.

       // This class contains all of the picks for an individual
       // seismic horizon (or fault), and functions for reading
       // and writing horizon (or fault) files.

       // If this class is used for validating a horizon file,
       // it should not be used for any other purpose.


#ifndef _HORIZON_HH_
#define _HORIZON_HH_

#include "oprim/several_float_arrays.hh"


class Horizon  :  public SeveralFloatArrays
{

//-------------------------- data ------------------------------------//
//-------------------------- data ------------------------------------//
//-------------------------- data ------------------------------------//

public:

  enum { XLOC = 0, YLOC, TIME, SHOT, LINE };

private:

  int   _selected;     // TRUE or FALSE.
  char *_name;         // identification for this horizon.
  char *_picktype;     // type of pick values ("time" or "depth").
  char *_units;        // units of pick values ("millisec" or "seconds" or
                       //                       "feet" or "meters").

protected:
  char *_color;        // color to draw this horizon.

//---------------------------- functions -----------------------------//
//---------------------------- functions -----------------------------//
//---------------------------- functions -----------------------------//

public:

           Horizon (const char *filetype = "horizon");
  virtual ~Horizon ();

public:   // get values.

  long        numPicks      ()       const  { return numElements(); }
  long        getActivePick ()       const  { return getActiveIndex(); }
  int         isSelected    ()       const  { return _selected; }
  const char *getName       ()       const  { return _name; }
  const char *getColor      ()       const  { return _color; }
  const char *getPicktype   ()       const  { return _picktype; }
  const char *getUnits      ()       const  { return _units; }

  float getXloc       (long ipick)  const  { return getValue(XLOC, ipick); }
  float getYloc       (long ipick)  const  { return getValue(YLOC, ipick); }
  float getTime       (long ipick)  const  { return getValue(TIME, ipick); }
  float getShotpoint  (long ipick)  const  { return getValue(SHOT, ipick); }
  float getLineNumber (long ipick)  const  { return getValue(LINE, ipick); }

  float minimumXloc       ()  const  { return minimumValue(XLOC); }
  float minimumYloc       ()  const  { return minimumValue(YLOC); }
  float minimumTime       ()  const  { return minimumValue(TIME); }
  float minimumShotpoint  ()  const  { return minimumValue(SHOT); }
  float minimumLineNumber ()  const  { return minimumValue(LINE); }

  float maximumXloc       ()  const  { return maximumValue(XLOC); }
  float maximumYloc       ()  const  { return maximumValue(YLOC); }
  float maximumTime       ()  const  { return maximumValue(TIME); }
  float maximumShotpoint  ()  const  { return maximumValue(SHOT); }
  float maximumLineNumber ()  const  { return maximumValue(LINE); }

public:   // set values.

  void        setNumPicks   (long npicks)  { setNumElements(npicks); }
  void        setActivePick (long active)  { setActiveIndex(active); }
  void        setSelected   (int selected)      { _selected = selected; }
  void        setName       (const char *name);
  void        setColor      (const char *color);
  void        setPicktype   (const char *picktype);
  void        setUnits      (const char *units);

public:   // get segment-specific values.

  long   numPicksInSegment      (long iseg)   const
                                      { return numElementsInSegment(iseg); }

  float  getXlocInSegment       (long iseg, long ipick)  const
                            { return getValueInSegment(iseg, XLOC, ipick); }
  float  getYlocInSegment       (long iseg, long ipick)  const
                            { return getValueInSegment(iseg, YLOC, ipick); }
  float  getTimeInSegment       (long iseg, long ipick)  const
                            { return getValueInSegment(iseg, TIME, ipick); }
  float  getShotpointInSegment  (long iseg, long ipick)  const
                            { return getValueInSegment(iseg, SHOT, ipick); }
  float  getLineNumberInSegment (long iseg, long ipick)  const
                            { return getValueInSegment(iseg, LINE, ipick); }

public:   // read and write disk file.
          // validate/import/export... access a text file.
          // read/save... access a binary file.
          // validate... puts parameters from file into floatio.
          // import... reads file using parameters in floatio.
          // export... writes file using parameters in floatio.
          // these set info or msg.
          // these return error TRUE or FALSE.

  int  validateHorizonFile  (class FloatioWrapper *floatio,
                             const char *filename, char *info)
                 { return validateFile(floatio, filename, info); }

  int  importHorizonFile    (class FloatioWrapper *floatio,
                             const char *filename, char *msg)
                 { return importFile(floatio, filename, msg); }

  int  exportHorizonFile    (class FloatioWrapper *floatio,
                             const char *filename, char *msg)
                 { return exportFile(floatio, filename, msg); }

  int  readHorizonFile      (const char *filename, char *msg)
                 { return readBinaryFile(filename, msg); }

  int  saveHorizonFile      (const char *filename, char *msg)
                 { return saveBinaryFile(filename, msg); }


protected:  // overriding virtual functions.

/*
  virtual int virtualValidateHeader (class FloatioWrapper* floatio, char* info);
  virtual int virtualImportHeader   (class FloatioWrapper* floatio, char* msg);
  virtual int virtualExportHeader   (class FloatioWrapper* floatio, char* msg);
*/

  virtual int virtualReadBinaryHeader (FILE* stream, char* msg);
  virtual int virtualSaveBinaryHeader (FILE* stream, char* msg);


//------------------------- end of functions --------------------------//
//------------------------- end of functions --------------------------//
//------------------------- end of functions --------------------------//

} ;

#endif

//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
