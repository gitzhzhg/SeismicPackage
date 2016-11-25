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
/****
!<CPS_v1 type="PROGRAM"/>


!<brief_doc>
!-------------------------------------------------------------------------------
!                       C P S   P R O G R A M 
!
! Name       : bldsup
! Category   : stand-alone
! Written    : 2000-01-26   by: Donna K. Vunderink
! Revised    : 2000-09-26   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : Add a new process to super.f90.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Usage:  bldsup -p process [-c catagory] [-m maturity] [-d] [file]
!
!         file            source code file name for super.f90
!                         default: 
                            /usr/app/user/sps/beta/programs/cfecustom/super.f90
!                         allowed: char*255
!                         OPTIONAL
!
!         -p process      process name
!                         default:
!                         allowed:char*12
!                         REQUIRED
!
!         -c catagory     process catagory
!                         default: MISCELLANEOUS
!                         allowed: char*20
!                         OPTIONAL
!
!         -m maturity     process maturity
!                         default: RAW
!                         allowed: RAW, ALPHA, BETA, PRODUCTION
!                         OPTIONAL
!
!         -d              delete process from super.f90
!                         OPTIONAL
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
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
!  4. 2000-09-26  Vunderink    Added delete option.
!  3. 2000-08-26  Vunderink    Change default location of super.f90 to cfecustom
!  2. 2000-03-17  Vunderink    Added defaults.
!  1. 2000-01-26  Vunderink    Initial version.
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
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
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


!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
****/


#include <stdlib.h>
#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define build_super        build_super_
#endif

#ifdef NEED_CAPITALS
#define build_super        BUILD_SUPER
#endif

main(argc,argv)
int argc;
char *argv[];
{

  int  i;
  char *filename;
  char *process;
  char *catagory;
  char *maturity;
  char def_catagory[14] = "miscellaneous";
  char def_maturity[4]  = "raw";
  char def_file[52]     = "/usr/app/user/sps/beta/programs/cfecustom/super.f90";
  char key  = 0;
  long idel = 0;

  filename = def_file;
  process  = NULL;
  catagory = def_catagory;
  maturity = def_maturity;

  if (argc == 1) {
    printf("Usage:\n");
    printf("  bldsup -p process [-c catagory] [-m maturity] [-d] [file]\n");
    printf("\n");
    printf("  file            source code file name for super.f90 \n");
    printf("                  default: /usr/app/user/sps/beta/programs/cfecustom/super.f90\n");
    printf("                  allowed: char*255 \n");
    printf("                  optional          \n");
    printf("\n");
    printf("  -p process      process name     \n");
    printf("                  default:         \n");
    printf("                  allowed: char*12 \n");
    printf("                  required         \n");
    printf("\n");
    printf("  -c catagory     process catagory       \n");
    printf("                  default: MISCELLANEOUS \n");
    printf("                  allowed: char*20       \n");
    printf("                  optional               \n");
    printf("\n");
    printf("  -m maturity     process maturity                      \n");
    printf("                  default: RAW                          \n");
    printf("                  allowed: RAW, ALPHA, BETA, PRODUCTION \n");
    printf("                  optional                              \n");
    printf("\n");
    printf("  -d              delete process from super.f90\n");
    printf("                  optional                     \n");
    printf("\n");
    exit(1);
  }

  i = 1;
  while (i < argc) {
    if (argv[i][0] != '-') {
      if (key == 0)
        filename = argv[i];
      else if (key == 'P')
        process = argv[i];
      else if (key == 'C')
        catagory = argv[i];
      else if (key == 'M')
        maturity = argv[i];
      key = 0;
    }
    else {
      switch(toupper(argv[i][1])) {
         case 'D':
               idel     = 1;
               key      = 0;
               break;
         case 'P':
               process  = argv[i];
               key      = 'P';
               break;
         case 'C':
               catagory = argv[i];
               key      = 'C';
               break;
         case 'M':
               maturity = argv[i];
               key      = 'M';
               break;
         default:
               printf("Invalid option \n Type 'bldsup' to see options\n");
               exit(1);
      }
    }
  i++;
  }

  if (process  == NULL) {
      printf("ERROR.... -p process must be specified\n");
      exit(1);
  }
  if (strlen(filename) > 255) {
      printf("ERROR.... file name too long\n");
      exit(1);
  }
  if (strlen(process) > 12) {
      printf("ERROR.... process name too long\n");
      exit(1);
  }
  if (strlen(catagory) > 20) {
      printf("ERROR.... catagory string too long\n");
      exit(1);
  }
  if (strlen(maturity) > 79) {
      printf("ERROR.... maturity string too long\n");
      exit(1);
  }

  build_super (&idel,process,catagory,maturity,filename);

}

