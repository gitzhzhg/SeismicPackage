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

//------------------------ survey_file.hh ----------------------------//
//------------------------ survey_file.hh ----------------------------//
//------------------------ survey_file.hh ----------------------------//

//           header file for the SurveyFile class
//             derived from the FileBase class
//             derived from the FgInform class
//                    subdirectory geom


//    This class reads and writes CPS field geometry survey files.
//    This class accesses the FieldGeometry class.


#ifndef _SURVEY_FILE_HH_
#define _SURVEY_FILE_HH_

#include "oprim/file_base.hh"
#include "geom/fg_inform.hh"
#include <stdio.h>


class SurveyFile  :  public FileBase, public FgInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

     // also protected _fg in FgInform base class.
     // also int io = ioIndex() from FileBase base class.

public:

  enum { ADD_ALL_NEW_LINES = 1,
         ADD_SPECIFIED_NEW_LINE,
         SKIP_ALL_NEW_LINES };

  enum { REPLACE_ALL_MATCHING_LINES = 1,
         REPLACE_SELECTED_MATCHING_LINES,
         SKIP_ALL_MATCHING_LINES };

  enum { SAVE_ALL_LINES = 1,
         SAVE_SELECTED_LINES };

private:  // information about survey file to be read or overwritten.

  int       _file_contains_line_numbers[2];   // TRUE or FALSE.
  char     *_cards[2];   // pointers to arrays first few cards in each file.
  const int _ncards;     // number of cards in each file stored in arrays.

private:  // directives on which lines on survey file to read in.
          // these flags are used when readFile is called.

  int   _add_option;     // one of the first set of enums.
  int   _replace_option; // one of the second set of enums.
  long  _line_to_add;    // line to add if ADD_SPECIFIED_NEW_LINE chosen.
  long  _default_line;   // default line number if file doesn't contain them.

  float _sp_add;         // constant to add to shotpoints.
  float _x_add;          // constant to add to X coordinates.
  float _y_add;          // constant to add to Y coordinates.
  float _sp_multiply;    // constant to multiply shotpoints by.
  float _x_multiply;     // constant to multiply X coordinates by.
  float _y_multiply;     // constant to multiply Y coordinates by.

private:  // directives on which lines to save onto survey file.
          // these flags are used when saveFile is called.

  int   _save_option;      // one of the third set of enums.

//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:

  SurveyFile (class FieldGeometry *fg);
  virtual ~SurveyFile();

public:      // get values.

  int   inputFileContainsLineNumbers()   const
                                 { return _file_contains_line_numbers[0]; }
  int   outputFileContainsLineNumbers()   const
                                 { return _file_contains_line_numbers[1]; }
  const char *inputCard        (int index)   const;
  const char *outputCard       (int index)   const;
  int   numCardsRetained       ()   const   { return _ncards; }

  int   getAddOption           ()   const   { return _add_option; }
  int   getReplaceOption       ()   const   { return _replace_option; }
  long  getLineNumberToAdd     ()   const   { return _line_to_add; }
  long  getDefaultLineNumber   ()   const   { return _default_line; }

  float getShotpointAdd        ()   const   { return _sp_add; }
  float getXcoordAdd           ()   const   { return _x_add; }
  float getYcoordAdd           ()   const   { return _y_add; }
  float getShotpointMultiply   ()   const   { return _sp_multiply; }
  float getXcoordMultiply      ()   const   { return _x_multiply; }
  float getYcoordMultiply      ()   const   { return _y_multiply; }

  int   getSaveOption          ()   const   { return _save_option; }

public:      // set values.

  void setAddOption               (int   value)  { _add_option     = value; }
  void setReplaceOption           (int   value)  { _replace_option = value; }
  void setLineNumberToAdd         (long  value)  { _line_to_add    = value; }
  void setDefaultLineNumber       (long  value)  { _default_line   = value; }

  void setShotpointAdd       (float value)  { _sp_add         = value; }
  void setXcoordAdd          (float value)  { _x_add          = value; }
  void setYcoordAdd          (float value)  { _y_add          = value; }
  void setShotpointMultiply  (float v)  { if(v != 0.0) _sp_multiply = v; }
  void setXcoordMultiply     (float v)  { if(v != 0.0) _x_multiply  = v; }
  void setYcoordMultiply     (float v)  { if(v != 0.0) _y_multiply  = v; }

  void setSaveOption              (int   value)  { _save_option    = value; }

protected:   // overriding virtual functions.

  virtual Prepare  virtualPrepareRead (const char *filename, char *errmsg);
//virtual Prepare  virtualPrepareSave (const char *filename, char *errmsg);
  virtual Result   virtualRead        (const char *filename, char *errmsg);
  virtual Result   virtualSave        (const char *filename, char *errmsg);
  virtual Validity virtualValidate    (const char *filename, char *info);
  virtual void     preNewFilename     ();
  virtual void     postNewFilename    ();

private:   // main private functions.

  int  readSurveyCards      (FILE *stream, char *errmsg);
  int  saveSurveyCards      (FILE *stream, char *errmsg);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
