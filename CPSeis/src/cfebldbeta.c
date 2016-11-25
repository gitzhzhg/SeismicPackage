/****
!<CPS_v1 type="PROGRAM"/>
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
!                       C P S   P R O G R A M
!
! Name       : cfebldbeta
! Category   : stand-alone
! Written    : 1999-07-08   by: Donna K. Vunderink
! Revised    : 2005-05-03   by: Karen Goodger
! Maturity   : beta
! Purpose    : Build CPS Jobfile
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! Builds a CPS jobfile from a CFE workfile.  CFE workfiles are named 
! "jobname.wrk" and CPS jobfiles are name "jobname.job".
!
!
! Usage:  cfebldbeta [-i] [-p platform] [-m mpi_version] jobname
!
!         -i             Build a CPS job for interactive execution
!
!         -p             Use the betalib for the specified platform
!                        Choices for platform are:
!                          default - default platform for machine (default)
!                          debug   - debug platform for machine
!                          new     - new compiler platform for machine
!                          linuxp  - Porland Group compiler for linux
!                   linuxab80_prof - Absoft profile platform.
!                   linuxab80_xeon - Absoft xeon platform.
!
!         -m             Version of lam-mpi to use
!                        Choices for mpi_version are:
!                          default - default version
!                          new     - newest version
!
!         jobname        Name of CPS job
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 16. 2005-05-03  Goodger      Add 64linuxp52 and 64linuxp52_debug.
! 15. 2004-04-30  Goodger      Add linuxab80_xeon.
! 14. 2003-10-02  Goodger      Add linuxab80_prof to help message.
! 13. 2003-04-03  Goodger      Fix help message.
! 12. 2003-03-24  Goodger      Added include files string.h and stdlib.h.
! 11. 2002-06-07  Vunderink    Added support for different mpi versions.
! 10. 2002-05-08  Vunderink    Changed program name to cfebldbeta
!  9. 2002-02-13  Vunderink    Abort if jobname missing
!  8. 2001-12-17  Vunderink    Make subroutine naming changes.
!  7. 2001-11-07  Vunderink    Removed batch queue options and added option to
!                                select platform 
!  6. 2001-08-02  Vunderink    Changed program name to cfebldtest
!  5. 2001-02-13  Vunderink    Changed default to PBS
!  4. 2000-12-20  Vunderink    Added NQS or PBS option
!  3. 2000-08-26  Vunderink    Added usage in doc
!  2. 2000-04-18  Vunderink    Initialized variable i
!  1. 1999-07-08  Vunderink    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!------------------------- start of program ------------------------------!!
!!------------------------- start of program ------------------------------!!
!!------------------------- start of program ------------------------------!!
****/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define cfebld_f        cfebld_f_
#endif

#ifdef NEED_CAPITALS
#define cfebld_f        CFEBLD_F
#endif

char cfebldbeta_ident[100] =
"$Id: cfebldbeta.c,v 1.16 2005/05/02 20:54:57 Goodger beta sps $";

void cfebld_f (char *filename,char *jobmode,char *batchsystem,char *platform,
               char *mpi);

int main(argc,argv)
int argc;
char *argv[];
{

  int  i;
  int  iflag = 0;
  char *filename;
  char jobmode[12]    = "BATCH";
  char batchsystem[4] = "PBS";
  char platform[21]   = "default";
  char mpi[21]        = "default";

  if (argc == 1) {
    printf("Usage:  cfebld [-i] [-p platform] [-m mpi_version] jobname\n");
    printf("\n");
    printf("-i         Build a job for interactive execution\n");
    printf("\n");
    printf("-m         Version of lam-mpi to use\n");
    printf("\n");
    printf("           Choices for mpi_version are:\n");
    printf("               default - default version\n");
    printf("               new     - newest version\n");
    printf("\n");
    printf("-p         Use the betalib for the specified platform\n");
    printf("\n");
    printf(" Choices for platform are:\n");
    printf("   default          - default platform for machine (default)\n");
    printf("   debug            - debug platform for default machine\n");
    printf("   linuxp           - Portland Group compiler for linux\n");
    printf("   64linuxp52       - Portland Group compiler for linux 64 bit\n");
    printf("   64linuxp62_debug - Portland Group compiler for linux 64 bit\n");
    printf("   linuxi           - Intel compiler for linux\n");
    printf("   linuxi_debug     - Intel compiler for linux\n");
    printf("   linuxab80_prof   - Absoft profile platform\n");
    printf("   linuxab80_xeon   - Absoft xeon platform\n");
    printf("   sol62            - Solaris compiler \n");
    printf("   sol62_debug      - Solaris compiler \n");
    printf("\n");
    printf("jobname    Name of CPS job\n");
    exit(1);
  }

  filename = NULL;

  i = 1;
  while (i < argc) {
    if (argv[i][0] != '-') {
      if (iflag == 1)
        strcpy(mpi,argv[i]);
      else if (iflag == 2)
        strcpy(platform,argv[i]);
      else
        filename = argv[i];
      iflag = 0;
    }
    else {
      switch(toupper(argv[i][1])) {
         case 'I':
               strcpy(jobmode,"INTERACTIVE");
               break;
         case 'M':
               iflag = 1;
               break;
         case 'P':
               iflag = 2;
               break;
         default:
               printf("Invalid option \nType 'cfebld' to see options\n");
               exit(1);
      }
    }
  i++;
  }


  if (filename == NULL) {
    printf("Missing jobname \nType 'cfebld' to see options\n");
    exit(1);
  }

  /* cfebld_f is a fortran routine in cfebld_frou.f90.  It calls buildjob
     which is in file buildjob.f90 */
  cfebld_f (filename,jobmode,batchsystem,platform,mpi);
  return 0;
}
