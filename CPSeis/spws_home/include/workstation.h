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
#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#include "trciof77.h"
#include "jsfiles.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

TF_Global *workstation_globals_get_globals(char *filename, int *istat);
char *workstation_globals_ftype(TF_Global *g);
int workstation_globals_get_nhdwd ( TF_Global *g );
char *workstation_globals_data_file(TF_Global *g);
int workstation_globals_get_wdtyp ( TF_Global *g );
int workstation_globals_grid_sizes(TF_Global *g, int *n1, int *n2, int *n3);
float workstation_globals_get_srval ( TF_Global *g );
float workstation_globals_get_tstrt ( TF_Global *g );
int workstation_globals_get_nbydp ( TF_Global *g );
char *workstation_get_jseis_fprop (char *filename);
void workstation_force_jseis_close (char *filename);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

