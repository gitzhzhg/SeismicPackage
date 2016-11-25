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
! Name       : ezgui
! Category   : stand-alone
! Written    : 2003-01-22   by: Charles C Burch
! Revised    : 2003-08-18   by: SMCook
! Maturity   : beta
! Purpose    : Produces xml files from layout files or F90 files
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This is a c frontend to ezgui_frou.f90 which is the main portion of the code.
! This is needed to allow access to the command parameters & certain c routines.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Value of maturity switch defaults to "-custom".  To set it to a specific
! maturity, use one of these:
!  -custom or omitted  (sets it to 0)
!  -alpha              (sets it to 1)
!  -beta               (sets it to 2)
!  -production         (sets it to 3)
! This information is passed to ezgui_frou.f90, which then uses it to decide
! what docbasepath to put into the xml files.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2003-08-18  SMCook       Added command line maturity setting for the
!                               purpose of incorporating a docbasepath --
!                               without a docbasepath approach, html would need
!                               to specify full paths to images rather than
!                               relative paths, complicating development and
!                               maintenance.
!  2. 2003-08-04  Stoeckley    Increase N_DIRS from 12 to 112.
!  1. 2003-01-26  CC Burch     Initial version.
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
! Use mkexe to compile and link.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! Currently, doc base paths for images are set to
!   file:///home/username/                                   (for custom)
!   file:///usr/app/vendors/int/Conoco/xml/CFEalpha/help/    (for alpha)
!   file:///usr/app/vendors/int/Conoco/xml/CFEbeta/help/     (for beta)
!   file:///usr/app/vendors/int/Conoco/xml/help/             (for production)
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
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define ezgui_f        ezgui_f_
#define ezgui_sleep    ezgui_sleep_
#define ezgui_system   ezgui_system_
#endif

#ifdef NEED_CAPITALS
#define ezgui_f        EZGUI_F
#define ezgui_sleep    EZGUI_SLEEP
#define ezgui_system   EZGUI_SYSTEM
#endif

#define N_DIRS 112

char ezgui_ident[100] =
"$Id: ezgui.c,v 1.1 2008/02/15 15:08:27 mengewm Exp $";

void ezgui_f (INTEGER*, INTEGER*, char*, INTEGER*, char*, char*);

void ezgui_sleep(INTEGER *SECS) {
  sleep(*SECS);
  return;
}

void ezgui_system(char *command) {
  system(command);
  return;
}

void ezgui_char_move(char *to, char *from, int n) {
  while(n-->0) {
    if((*from)==' ') break;
    (*to++)=(*from++);
  }
  (*to)='\0';
  return;
}

void ezgui_print_parms(INTEGER *CHECK_HELP, char *dirs, INTEGER *NDIRS,
    char *file_in, char *file_out){
  int i, n_dirs;
  char line[81];

  n_dirs=(*NDIRS);
  printf("n_dirs=%d, check_help=%d\n",n_dirs,(int) (*CHECK_HELP));

  for (i=0; i<n_dirs; i++) {
    ezgui_char_move(line, dirs+80*i, 80);
    printf("dirs[%d]=%s\n",i+1, line);
  }
  ezgui_char_move(line,file_in,80);
  printf("file_in=%s\n",line);

  ezgui_char_move(line,file_out,80);
  printf("file_out=%s\n",line);
  return;
}

int main(int argc, char *argv[]) {

  char file_in[80], file_out[80], dirs[80*N_DIRS];
  char command_arg[80], command_arg_upper_case[80];
  int maturity = 0;
 int i1, i;
  INTEGER n_dirs, check_help_sw;

  memset(dirs,     ' ', sizeof(dirs));
  memset(file_in,  ' ', sizeof(file_in));
  memset(file_out, ' ', sizeof(file_out));

  n_dirs=0;
  check_help_sw=1;

  /*get all the -I commands */
  i1=1;
  while(i1<argc) {
    strcpy(command_arg, argv[i1++]);
    if(command_arg[0]!='-'){  
      /* get file_in and file_out */
      i=strlen(command_arg);
      if(file_in[0]==' ') {
        memcpy(file_in,command_arg,i);
      } else if(file_out[0]==' ') {
        memcpy(file_out,command_arg,i);
      } else {
        printf("parameter(%s) appears to be extraneous and is skipped\n",
         command_arg);
      }
      continue;
    }

    for(i=0;command_arg[i]!='\0';i++) {
      command_arg_upper_case[i]=toupper(command_arg[i]);
    }
    command_arg_upper_case[i]='\0';

    if(strcmp(command_arg_upper_case,"-CUSTOM")==0) {
      maturity = 0;
      continue;
    }
    if(strcmp(command_arg_upper_case,"-ALPHA")==0) {
      maturity = 1;
      continue;
    }
    if(strcmp(command_arg_upper_case,"-BETA")==0) {
      maturity = 2;
      continue;
    }
    if(strcmp(command_arg_upper_case,"-PRODUCTION")==0) {
      maturity = 3;
      continue;
    }

    if(strcmp(command_arg_upper_case,"-CHECKHELP")==0  ||
       strcmp(command_arg_upper_case,"-HELPCHECK")==0  ) {
      check_help_sw=1;
      continue;
    }

    if(strcmp(command_arg_upper_case,"-NOCHECKHELP")==0  ||
       strcmp(command_arg_upper_case,"-NOHELPCHECK")==0  ) {
      check_help_sw=0;
      continue;
    }

    /* allow -I dir or -Idir */
    if(strcmp(command_arg_upper_case,"-I")==0) {
      if(i1>=argc) {
        printf("WARNING -I command found with no directory specified\n");
        break;
      }
      strcat(command_arg,argv[i1++]);
    }

    if(command_arg_upper_case[0]=='-' && command_arg_upper_case[1]=='I') {
      if(n_dirs<N_DIRS-2) {
        i=strlen(command_arg)-2;
        memcpy(dirs+80*n_dirs,command_arg+2,i);
        if(dirs[80*n_dirs+i-1]!='/') dirs[80*n_dirs+i]='/';
        n_dirs++;
      } else {
        printf("Too many -I commands:%s skipped\n", command_arg);
      }
      continue;
    }

    printf("Invalid -command option found: %s skipped\n",command_arg);
  }

  /* add three default directories */
  memcpy(dirs+80*n_dirs++,"~/cpseis/etc/",13);
  memcpy(dirs+80*n_dirs++,"~/cpseis/src/",13);
  memcpy(dirs+80*n_dirs++,"~/workspace/cpseis/etc/",23);
  memcpy(dirs+80*n_dirs++,"~/workspace/cpseis/src/",23);
  memcpy(dirs+80*n_dirs++,"./",2);

  /*ezgui_print_parms(&check_help_sw, dirs, &n_dirs, file_in, file_out);*/
  fflush(stdout);

  /* printf("ezgui target maturity in c code is set to %i\n", maturity); */

  ezgui_f(&check_help_sw, &maturity, dirs, &n_dirs, file_in, file_out);
  return(0);
}
