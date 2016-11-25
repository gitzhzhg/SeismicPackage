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
#ifndef SEIS_COLOR_FILE_IO_HH
#define SEIS_COLOR_FILE_IO_HH

#include "color/color_file_io.hh"

class SeisColorFileIO :  public ColorFileIO {

public:
  SeisColorFileIO				// constructor
    (class SeisCtype *sct);			//   seisCtype object

  virtual ~SeisColorFileIO ();			// destructor

  virtual void setFileOut 			// communicates current output
    (char *filename);				//   filename to tell about

  virtual int readColorFile 			// read a color file
    (class ColorBarBuilderPop *cbb,		//   color bar builder pop
     char *filename);				//   given file name

  virtual int writeColorFile 			// writes output file
    (class ColorBarBuilderPop *cbb,		//   color bar builder pop
     char *filename,				//   given file name
     class CBBRGBSet *rgb);			//   RGB set containing data

private:
  float *squishRGBSet				// removes redundancies
    (class CBBRGBSet *rgb);			//   RGB set containing data

  class SeisCtype
    *_sct;					// SeisCtype object
};

#endif
