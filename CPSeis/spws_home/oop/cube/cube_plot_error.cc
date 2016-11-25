#include "cube/cube_plot_error.hh"
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
#include "cube/cube.hh"
#include "sp/seis_plot.hh"
#include "sl/error_handler.hh"
#include "cprim.h"

CubePlotError::CubePlotError (Widget parent, Cube *cube)
{
  if (parent == 0) return;

  //Handle case of user requesting a line from the control panel
  //before having any data displayed
  if(cube == 0)
    {
      ErrorHandler err = parent;
      err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
      err.deliverError("Data must be displayed first.");
      return;
    }
    


  long status = cube->lastPlotStat ();

// build up the error string
  char *error_string = 0, *temp_string0 = 0, *temp_string1 = 0;

  if (!(error_string = (char *)realloc (error_string, (size_t)2))) return;
  error_string[0] = '\0';
  if (status & CubeInlineFail) {
    if (!(temp_string0 = newstr ("Line plot error:  "))) return;
    if (!(temp_string1 = cube->inlineSP()->lastError ())) return;
    if (!(error_string = (char *)realloc (error_string,
      strlen(temp_string0)+strlen(temp_string1)+2))) return;
    strcat (error_string, (const char *)temp_string0);
    free (temp_string0), temp_string0 = 0;
    strcat (error_string, (const char *)temp_string1);
    free (temp_string1), temp_string1 = 0;
  }
  if (status & CubeCrosslineFail) {
    if (error_string) {
      if (!(temp_string0 = newstr ("\n\nCrossline plot error:  "))) return;
    }
    else {
      if (!(temp_string0 = newstr ("Crossline plot error:  "))) return;
    }
    if (!(temp_string1 = cube->crosslineSP()->lastError ())) return;
    if (!(error_string = (char *)realloc (error_string,
      strlen(error_string)+strlen(temp_string0)+strlen(temp_string1)+2)))
      return;
    strcat (error_string, (const char *)temp_string0);
    free (temp_string0), temp_string0 = 0;
    strcat (error_string, (const char *)temp_string1);
    free (temp_string1), temp_string1 = 0;
  }
  if (status & CubeTimeSliceFail) {
    if (error_string) {
      if (!(temp_string0 = newstr ("\n\nTimeslice plot error:  "))) return;
    }
    else {
      if (!(temp_string0 = newstr ("Timeslice plot error:  "))) return;
    }
    if (!(temp_string1 = cube->timesliceSP()->lastError ())) return;
    if (!(error_string = (char *)realloc (error_string,
      strlen(error_string)+strlen(temp_string0)+strlen(temp_string1)+2)))
      return;
    strcat (error_string, (const char *)temp_string0);
    free (temp_string0), temp_string0 = 0;
    strcat (error_string, (const char *)temp_string1);
    free (temp_string1), temp_string1 = 0;
  }

  ErrorHandler err = parent;
  err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
  err.deliverError (error_string);
  free (error_string);
}

CubePlotError::~CubePlotError ()
{
}
