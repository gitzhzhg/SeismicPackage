
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
//---------------------- statics_file.cc ------------------------//
//---------------------- statics_file.cc ------------------------//
//---------------------- statics_file.cc ------------------------//

//          implementation file for the StaticsFile class
//                 derived from the FileBase class
//                 derived from the FgInform class
//                        subdirectory fgqc


#include "fgqc/statics_file.hh"
#include "oprim/file_base.hh"
#include "geom/field_geometry.hh"
#include "cprim.h"
#include "inquire.h"
#include "named_constants.h"

#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>

#define FILETYPE   "statics file"
#define EXTENSION  "stat"
#define NOTSUPPORTED \
              "The given statics file coordinate system is not supported."

//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//

//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//

StaticsFile::StaticsFile (FieldGeometry *fg):
  FileBase (FILETYPE, EXTENSION),
  FgInform (fg),
  _fg                         (fg),
  _check_file_was_valid       (FALSE),
  _data_file_was_read         (FALSE),
  _smooth_flag                (FALSE),
  _ix                         (0),
  _iy                         (0),
  _check_coordinate_system    (UNRECOGNIZEDSYSTEM),
  _data_coordinate_system     (UNRECOGNIZEDSYSTEM)
{
  _ss_data  = new StaticKernal("CFG");
  _ss_check = new StaticKernal("CFG");

  _check_filename = (char *) calloc (2, sizeof(char));
  strcpy (_check_filename, "");
  _data_filename = (char *) calloc (2, sizeof(char));
  strcpy (_data_filename, "");
}

//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//

StaticsFile::~StaticsFile ()
{
  delete _ss_data;
  delete _ss_check;
  free (_check_filename);
  free (_data_filename);
}

//------------------------ get values -----------------------------//
//------------------------ get values -----------------------------//
//------------------------ get values -----------------------------//


void StaticsFile::getFirstPoint (float *x, float *y, float *value)
{
  _ix = 1;
  _iy = 1;
  *value  = _ss_data->getValue(_ix - 1, _iy - 1);
  *x = _x = _ss_data->getX1();
  *y = _y = _ss_data->getY1();
}

void StaticsFile::getNextPoint (float *x, float *y, float *value)
{
  _ix++;
  if (_ix > _ss_data->getNx() || _ix == 1) {
    _ix = 1;
    *x = _x = _ss_data->getX1();
    _iy++;
    if (_iy > _ss_data->getNy() || _iy == 1) {
      _iy = 1;
      _y = _ss_data->getY1();
    }
    else {
      _y += _ss_data->getYinc();
    }
  }
  else {
    *x = _x += _ss_data->getXinc();
  }
  *y = _y;
  *value = _ss_data->getValue(_ix - 1, _iy - 1);
}

//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//

    // protected virtual function overriding FileBase.
    // sets info to a string of information about the file.

FileBase::Validity StaticsFile::virtualValidate (const char *filename,
  char *info)
{
  if (ioIndex() == 1) return FileBase::VALID_NO;  // an output file is never valid

  int same_file;
  if (!strcmp(filename,_check_filename)) {
    same_file = 1;
  }
  else {
    same_file = 0;
  }

                                              // skip reading static values.
                                              //                |
  _check_file_was_valid = !_ss_check->readFile(filename, info, TRUE);

  if (_check_file_was_valid) {
    _check_coordinate_system = findCoordinateSystem (_ss_check);
    if (_check_coordinate_system != UNRECOGNIZEDSYSTEM &&
      !same_file                                         ) {
      char *temp = (char *) realloc (_check_filename, strlen(filename)+1);
      if (temp) {
        _check_filename = temp;
        strcpy (_check_filename, filename);
      }
    }
      return FileBase::VALID_YES;
  }
  else {
    _check_coordinate_system = UNRECOGNIZEDSYSTEM;
    return FileBase::VALID_NO;
  }
}

//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//

    // protected virtual function overriding FileBase.

void StaticsFile::preNewFilename ()
{
  _fg->preSlowOperations ();
}

void StaticsFile::postNewFilename()
{
  _fg->showMessage ("statics file validation completed");
  _fg->postSlowOperations ();
}

int StaticsFile::findCoordinateSystem (StaticKernal *ss)
{
  StaticKernal *temp;
  temp = _ss_data;
  _ss_data = ss;

  long header_num_x = getHeaderNumX ();
  long header_num_y = getHeaderNumY ();

  _ss_data = temp;

  if      ((header_num_x ==  7 && header_num_y ==  8) ||
           (header_num_x == 33 && header_num_y == 34) ||
           (header_num_x == 35 && header_num_y == 36)   )return GRIDSYSTEM;
  else if ((header_num_x == 11 && header_num_y == 12) ||
           (header_num_x == 14 && header_num_y == 15) ||
           (header_num_x == 17 && header_num_y == 18)   )return SURVEYSYSTEM;
  else if ( header_num_x == 46 || header_num_x == 47)    return GROUNDPOSITION;
  else if ( header_num_x == 9)                           return GROUPSYSTEM;
  else if ((header_num_x ==  7 && header_num_y == 0)  ||
           (header_num_x == 33 && header_num_y == 0)  ||
           (header_num_x == 35 && header_num_y == 0)    )return GRIDSYSTEM2D;
  else if ((header_num_x == 11 && header_num_y == 0) ||
           (header_num_x == 14 && header_num_y == 0) ||
           (header_num_x == 17 && header_num_y == 0)   ) return SURVEYSYSTEM2D;
  else return UNRECOGNIZEDSYSTEM;
}


// Public method to read a file

int StaticsFile::readFile (char *filename, char *errmsg)
{
FileBase::Result result;

  result = virtualRead((const char *)filename, errmsg);
  if(result == FAILURE)
    return 1;
  else
    return 0;
}

//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//

    // protected virtual function overriding FileBase.
    // this function does not put anything into FieldGeometry.

FileBase::Prepare StaticsFile::virtualPrepareRead
  (const char * /* filename */, char *errmsg)
{
  if (_check_coordinate_system == UNRECOGNIZEDSYSTEM) {
    errmsg = NOTSUPPORTED;
    return PROHIBIT;
  }
  else {
    errmsg = 0;
    return GODSPEED;
  }
}

//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//

    // protected virtual function overriding FileBase.
    // currently not needed.

//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//

    // protected virtual function overriding FileBase.
    // this function does not put anything into FieldGeometry.

FileBase::Result StaticsFile::virtualRead (const char *filename, char *errmsg)
{
  _fg->preSlowOperations();
  int error = _ss_data->readFile(filename, errmsg);
  _fg->postSlowOperations();
  if (error) {
    _data_coordinate_system = UNRECOGNIZEDSYSTEM;
    _data_file_was_read = FALSE;
    return FAILURE;
  }
  else {
    char *temp = (char *) realloc (_data_filename, strlen(filename)+1);
    if (temp) {
      _data_filename = temp;
      strcpy (_data_filename, filename);
    }
    _data_coordinate_system = findCoordinateSystem (_ss_data);
    _data_file_was_read = TRUE;
    return SUCCESS;
  }
}

//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//

    // protected virtual function overriding FileBase.
    // currently not needed.

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
