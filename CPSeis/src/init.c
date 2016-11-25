/****
!<CPS_v1 type=AUXILIARY_FILE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : init
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2001-04-16   by: Donna K. Vunderink
! Maturity   : beta
! Purpose    : CGM library.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char init_ident[100] = 
"$Id: init.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

void cgmGopks(int terr)
{
  char debug_logical[256];
  char *debug_logical_ptr = debug_logical;
  char err_file[21];

  max_pointer = 65535;
  pointer = -1;

/* Open the error log file */
  cgmdebug = FALSE;
  debug_logical_ptr = getenv("CGM_DEBUG");
  if (debug_logical_ptr != NULL) {
     if (strcmp(debug_logical_ptr,"TRUE") == 0) cgmdebug = TRUE;
  }
  sprintf(err_file,"fort%d",terr);
  err_unit= fopen(err_file,"w");
  
  if (cgmdebug) fprintf(err_unit,"cgm_gopks: %d \n",terr);
  cgmTransInit();

}

void cgmGacwk(int iwk)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gacwk: %d \n",iwk);
  current_wk = iwk;
}

void cgmGdawk(int iwk)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gdawk: %d \n",iwk);
}

void cgmGclwk(int iwk)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gclwk: %d \n",iwk);

/* End Picture */
  cgmPutData(0,5);

/* End Metafile */
  cgmPutData(0,2);
}

void cgmGopwk(int iwk, char *outfile2, char *type2)
{
  char outfile[256];
  char *outfile_ptr = outfile;

  if (outfile2[1] == 0) {
     outfile_ptr = getenv("CGM_FILE_NAME");
     if (outfile[1] == 0) strcpy(outfile,"default.cgm");
  }
  else {
     strcpy(outfile,outfile2);
  }
  if (cgmOpenFileWrite(outfile) != 0) {
     fprintf(err_unit,"Error Opening File - %s \n\n",outfile);
  }
  if (cgmdebug) fprintf(err_unit,"cgm_gopwk: %s \n",outfile);

/* Initialize all settings */
  if (strcmp(type2,"CGMPIP") == 0) cgmPipInit();
  cgmSetDefaults();
}

void cgmGclks()
{
  int i;
  if (cgmdebug) fprintf(err_unit,"cgm_gclks: \n");

/* Output the tables */
  for (i=0;i<4096;i++) {
     cgmPutByteNow(0);
  }
  cgmCloseFileWrite();
  fclose (err_unit);
  pointer = -1;
}
