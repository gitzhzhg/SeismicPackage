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

//---------------------- sl_file_pair.hh -----------------------//
//---------------------- sl_file_pair.hh -----------------------//
//---------------------- sl_file_pair.hh -----------------------//

//              header file for the SLFilePair class
//                 derived from the SLDelay class
//                        subdirectory sl

     // This is a wrapper around make_filepair.  The major
     // difference, besides being a C++ class derived from
     // SLDelay, is that filenames are passed thru argument
     // lists, and pointers to filenames in the user area
     // are not used.

#ifndef _SL_FILE_PAIR_HH_
#define _SL_FILE_PAIR_HH_

#include "sl/sl_delay.hh"
#include <Xm/Form.h>

typedef void FilePairTrap (void *user_data,
                           char *filename1, char *filename2,
                           long *valid1, long *valid2,
                           char *info1, char *info2,
                           long *same_datasets);


class SLFilePair : public SLDelay
{

//---------------------- data --------------------------//
//---------------------- data --------------------------//
//---------------------- data --------------------------//

private:

  enum { NCHAR = 200 };

  const char * const _filetype1;  // type of input file.
  const char * const _filetype2;  // type of output file.
  const char * const _extension1; // default input file extension.
  const char * const _extension2; // default output file extension.
  FilePairTrap      *_user_trap;  // user trap.
  void              *_user_data;  // user data.
  char      _filename1[NCHAR+1];  // input file name.
  char      _filename2[NCHAR+1];  // output file name.
  char      _basename [NCHAR+1];  // base name for making file names.
  const Boolean      _required1;  // whether input file is required.
  const Boolean      _required2;  // whether output file is required.
  const char * const _suffix1;    // default suffix for input filename proper.
  const char * const _suffix2;    // default suffix for output filename proper.

//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//

public:

  SLFilePair(  SLDelay           *slparent,
               char              *name,
               const char * const filetype,
               const char * const extension,
               FilePairTrap      *user_trap,
               void              *user_data,
               char              *filename1,
               char              *filename2   = NULL,
               const Boolean      required1   = TRUE,
               const Boolean      required2   = FALSE,
               HelpCtx            hctx        = NULL,
               Boolean            doframe     = FALSE,
               Boolean            make_if_can = TRUE  );

  SLFilePair(  Widget             wparent,
               char              *name,
               const char * const filetype,
               const char * const extension,
               FilePairTrap      *user_trap,
               void              *user_data,
               char              *filename1,
               char              *filename2   = NULL,
               const Boolean      required1   = TRUE,
               const Boolean      required2   = FALSE,
               HelpCtx            hctx        = NULL,
               Boolean            doframe     = FALSE,
               Boolean            make_now    = TRUE  );

  SLFilePair(  SLDelay           *slparent,
               char              *name,
               const char * const filetype1,
               const char * const filetype2,
               const char * const extension1,
               const char * const extension2,
               FilePairTrap      *user_trap,
               void              *user_data,
               char              *filename1,
               char              *filename2   = NULL,
               const Boolean      required1   = TRUE,
               const Boolean      required2   = FALSE,
               const char * const suffix1     = "",
               const char * const suffix2     = "",
               HelpCtx            hctx        = NULL,
               Boolean            doframe     = FALSE,
               Boolean            make_if_can = TRUE  );

  virtual ~SLFilePair();

//--------------------- other functions ----------------------//
//--------------------- other functions ----------------------//
//--------------------- other functions ----------------------//

public:

  const char * const inputFile ()  const  { return _filename1; }
  const char * const outputFile()  const  { return _filename2; }
  Boolean        inputRequired ()  const  { return _required1; }
  Boolean        outputRequired()  const  { return _required2; }

  virtual WidgetClass topClass() { return xmFormWidgetClass; };
  virtual Boolean  isContainer() { return TRUE; };
  virtual Widget make(Widget p = NULL);

  long updateFilePair (char *filename1, char *filename2,
                                                    char *msg = NULL);
  long updateFilePair                              (char *msg = NULL);
  long updateFilePairFromBaseName  (char *basename, char *msg = NULL);
  long updateFilePairIfNewBaseName (char *basename, char *msg = NULL);

  void changeMessage1(char *msg) { changeMessage(1, msg); }
  void changeMessage2(char *msg) { changeMessage(2, msg); }
  void changeMessage3(char *msg) { changeMessage(3, msg); }

private:

  void changeMessage(int which, char *msg);

  static void staticTrap(void *data,
                         long *valid1, long *valid2,
                         char *info1, char *info2,
                         long *same_datasets);

//------------------------ end functions ---------------------//
//------------------------ end functions ---------------------//
//------------------------ end functions ---------------------//

};

#endif

//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
