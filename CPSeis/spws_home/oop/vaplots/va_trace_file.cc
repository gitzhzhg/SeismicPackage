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
// $Id: va_trace_file.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <stdio.h>
#include "sp/seis_plot.hh"
#include "vaplots/va_plot.hh"
#include "vaplots/va_trace_file.hh"
#include "plot_image.hh"




VaTraceFile::VaTraceFile(VaPlot *plot, char *description) :
                 FileBase(description,"trc*"), 
                      _plot(plot),
                      _do_validation(True),
                      _xhead(0),
                      _yhead(0)
{
}

VaTraceFile::~VaTraceFile()
{
}


FileBase::Result VaTraceFile::virtualRead(const char *, char *)
{
  long x= _plot->getActiveXheader();
  long y= _plot->getActiveYheader();
  if ((_xhead != x) || (_yhead != y))  {
          _plot->getFileParameters((char *)inputFilename());
          printf("virtualRead: rescanning file\n");
          _xhead= _plot->getActiveXheader();
          _yhead= _plot->getActiveYheader();
  } 
  return FileBase::SUCCESS;
}

FileBase::Validity VaTraceFile::virtualValidate(const char *filename, char *info)
{
  //The following line added by MLS on 02/25/99 to prevent file validations
  //occuring on blank filenames
  if(strlen(filename) == 0) return (FileBase::VALID_NO);

  if (ioIndex() == 1) return (FileBase::VALID_YES);
  Validity retval= FileBase::VALID_YES;
  SeisPlot *sp= _plot->SP();
  if ( !lastInputFilenameValidated() ) {
      if (_plot->getFileParameters((char *)filename) == PlotImage::DEFS_OK) {
              _xhead= _plot->getActiveXheader();
              _yhead= _plot->getActiveYheader();
              _first_time= False;
              retval= FileBase::VALID_YES;
      }
      else {
              retval= FileBase::VALID_NO;
      }
  } // end if
  if (retval== FileBase::VALID_YES) {
     sprintf(info, "Panels: %d,  Traces/Panel: %d,  tmin: %4.3f,  tmax: %4.3f",
               _plot->numberLocationsInFile(), _plot->tracesPerGroup(), 
               sp->minTmin(), sp->maxTmax());
  }
  return retval;

}


void VaTraceFile::postNewFilename()
{
}

void VaTraceFile::setDoValidation(int v)
{
  _do_validation= v;
}

FileBase::Prepare
VaTraceFile::virtualPrepareRead (const char* /*filename*/, char* /*errmsg*/)
{
  return GODSPEED;
}
