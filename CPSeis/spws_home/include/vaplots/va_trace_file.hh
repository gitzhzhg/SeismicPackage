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
#ifndef VA_TRACE_FILE_HH
#define VA_TRACE_FILE_HH

#include "oprim/file_base.hh"
#include "vaplots/va_plot_control.hh"

class SeisPlot;
class VaPlot;


class VaTraceFile : public FileBase {

 private: 
 protected:
      VaPlot                       *_plot;
      int                           _first_time;    // boolean value
      int                           _do_validation; // boolean value
      long                          _xhead;
      long                          _yhead;
 public:
      VaTraceFile(VaPlot *plot, char *description);
      virtual ~VaTraceFile();

      virtual Result virtualRead(const char *filename, char *errmsg);
      virtual Validity virtualValidate(const char *filename, char *info);
      virtual Prepare virtualPrepareRead (const char* , char* );
      virtual void postNewFilename();
      void setDoValidation(int v);
};

#endif
