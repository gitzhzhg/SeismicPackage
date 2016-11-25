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

//------------------------ statics_file.hh ----------------------------//
//------------------------ statics_file.hh ----------------------------//
//------------------------ statics_file.hh ----------------------------//

//           header file for the Statics File class
//             derived from the FileBase class
//             derived from the FgInform class
//                    subdirectory fgqc

//    This class reads CPS statics files.

#ifndef STATICS_FILE_HH
#define STATICS_FILE_HH

#include "oprim/file_base.hh"
#include "geom/fg_inform.hh"
#include "stat/static_kernal.hh"
#include "cprim.h"

#include <stdio.h>

class FieldGeometry;

class StaticsFile  :  public FileBase, public FgInform
{
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:

  enum { UNRECOGNIZEDSYSTEM, GRIDSYSTEM, SURVEYSYSTEM, GROUNDPOSITION,
         GROUPSYSTEM, GRIDSYSTEM2D, SURVEYSYSTEM2D };

  StaticsFile					// constructor
    (FieldGeometry *fg);			//   Field geometry pointer

  virtual ~StaticsFile ();			// destructor

  char *getType () const 			// return type of statics file
    { return (char*)_ss_data->getStattype(); }

  const char *getCurrentFilename () const	// return pointer to current fi
    { return (const char *)_data_filename; }

  long getHeaderNumX () const			// return hdr # used for X gndp
    { return _ss_data->getNhx(); }

  long getHeaderNumY () const			// return hdr # used for Y gndp
    { return _ss_data->getNhy(); }

  float getFirstX () const			// return 1st X ground position
    { return _ss_data->getX1(); }

  float getFirstY () const			// return 1st Y ground position
    { return _ss_data->getY1(); }

  float getIncrementX () const			// return incr for X grnd posns
    { return _ss_data->getXinc(); }

  float getIncrementY () const			// return incr for Y grnd posns
    { return _ss_data->getYinc(); }

  float getLastX () const			// return last X ground posn
    { return _ss_data->getXend(); }

  float getLastY () const			// return last Y ground posn
    { return _ss_data->getYend(); }

  long getNumX () const				// return # ground posns in X
    { return _ss_data->getNx(); }

  long getNumY () const				// return # ground posns in Y
    { return _ss_data->getNy(); }

  float getAPoint				// return a statics value
    (long ix,					//   X ground position index
     long iy) const				//   Y ground position index
    { return _ss_data->getValue(ix-1, iy-1); }

  void getFirstPoint				// return first statics point
    (float *x,					//   Statics X-coordinate
     float *y,					//   Statics Y-coordinate
     float *value);				//   Statics value

  void getNextPoint				// return next statics point
    (float *x,					//   Statics X-coordinate
     float *y,					//   Statics Y-coordinate
     float *value);				//   Statics value

  float interpolatePoint			// return a interp statics valu
    (float x,					//   statics X-coordinate
     float y) const				//   statics Y-coordinate
    { return _ss_data->getTerpValue(x, y); }

  int inputFileIsValid ()		    // rtrns TRUE if lst chckd file OK
    { return _check_file_was_valid; }

  int inputFileIsReadable ()		    // rtrns TRUE if curnt file read
    { return _data_file_was_read; }

  void setSmoothStatics ()		    // set smooth flag to TRUE
    { _smooth_flag = 1; }

  void setInsertStatics ()		    // set smooth flag to FALSE
    { _smooth_flag = 0; }

  int smoothStatics ()			    // return TRUE if smooth flag TRUE
    { return _smooth_flag; }

  int getCoordinateSystem () const          // return file coord system
    { return _data_coordinate_system; }

  int readFile(char *filename, char *errmsg);


protected:   // overriding virtual functions.

  virtual void preNewFilename ();
  virtual void postNewFilename ();

  virtual Prepare  virtualPrepareRead (const char *filename, char *errmsg);
  virtual Result   virtualRead        (const char *filename, char *errmsg);
  virtual Validity virtualValidate    (const char *filename, char *info);

private:

  int findCoordinateSystem			// determine coordinate sys
    (StaticKernal *ss);				//   StaticKernal to use

  float
    _x,						// current X ground posn read
    _y;						// current Y ground posn read

  int
    _check_coordinate_system,                   // type of coord sys in chkd file
    _data_coordinate_system;                    // type of coord sys in rd file

  char
    *_check_filename,				// last checked filename
    *_data_filename;				// current file already read

  long
    _check_file_was_valid,			// last chkd file validity flag
    _data_file_was_read,			// curnt rd file validity flag
    _smooth_flag,				// display file as smoothed
    _ix,					// current X-bin posn read
    _iy;					// current Y-bin posn read

  StaticKernal
    *_ss_data,					// data structure for lst chkd
    *_ss_check;					// data struct for curnt rd fil

  FieldGeometry
    *_fg;					// Field geometry pointer


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
