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
// class that creates the file I/O gui for a color bar builder
#ifndef CBB_COL_FIO_GUI_HH
#define CBB_COL_FIO_GUI_HH

#include "sl/sl_form.hh"

class CBBColFIOGui : public SLForm {

public:
  enum {
    NONE,					// a file was not requested
    GOOD,					// requested file was good
    BAD						// requested file was bad
  };

  CBBColFIOGui					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     class ColorBarBuilderPop *cbb_pop);	//   CBB pop up

  CBBColFIOGui					// constructor
    (SLDelay *container,			//   container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     class ColorBarBuilderPop *cbb_pop);	//   CBB pop up

  ~CBBColFIOGui ();				// destructor

  void init ();					// constructor helper

  virtual Widget make				// make GUI function
    (Widget parent = 0);			//   parent widget

  virtual void manage ();			// manage GUI function

  char *outputFile ();				// return name of output file

  Boolean setInFilename				// set input filename
    (char *infile);				//   given name

  Boolean setInFile				// set input filename & display
    (char *filename);				//   given name

  Boolean setOutFile				// set output filename & disply
    (char *filename);				//   given name

  Boolean inFileNew				// check if in file is new
    (char *check_file);				//   given name

  Boolean inFileDifferent			// check for diff with in file
    (char *check_file);				//   given name

  Boolean good					// rtn False if file not good
    (Boolean is_output = True);			//   given whether output file

  Boolean none					// rtn True if file specified
    (Boolean is_output = True);			//   given whether output file

private:
  void set ();					// set the I/O file names

  void filein					// called after input file gvn
    (long ident,				//   ident for which file
     char *oldvar,				//   previous name
     char *newvar);				//   new name

  virtual Boolean ValidInput ();		// check validity of input

  static void fileCallback			// file selection call back
    (void *data,				//   calling object
     long ident,				//   ident for which file
     char *oldvar,				//   previous name
     char *newvar);				//   new name

  Boolean setOutFilename			// set output filename
    (char *outfile);				//   given name

  class ColorBarBuilderPop
    *_cbb_pop;					// color bar builder popup

  class SLpFile
    *_infile,					// input file
    *_outfile;					// output file

  time_t
    _file_mod_time;				// when file last modified
 
  Boolean
    _first_time;				// True until managed once

  int
    _in_file_state,				// one of three file states
    _out_file_state;				// one of three file states

  char
    _input_file[300],				// given color bar input file
    _output_file[300],				// given color bar output file
    _in_filename[300],				// expanded input file name
    _out_filename[300];				// expanded output file name

};

#endif
