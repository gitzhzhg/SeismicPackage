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
#ifndef SLP_FILE_DATA_HH
#define SLP_FILE_DATA_HH

#include "sl/slp_file.hh"

class SLpFileData {
public:
  SLpFileData
    (char *name = "file",
     long ident = 0,
     const char *label = "label",
     const char *filetype = "file",
     const char *extension = "dat",
     int io = SLpFile::_INPUT,
     const char *pattern = NULL);

  ~SLpFileData ();

  char *name ();

  long ident ();

  const char *label ();

  const char *filetype ();

  const char *extension ();

  int io ();

  const char *pattern ();

private:
  char
    *_name;

  const char
    *_label,
    *_filetype,
    *_extension,
    *_pattern;

  long
    _ident;

  int
    _io;
};

#endif
