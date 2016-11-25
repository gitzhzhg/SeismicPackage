/****
!<CPS_v1 type="AUXILIARY_FILE"/>
****/
/*--------------------------- exptilde_crou.c ------------------------------*/
/*--------------------------- exptilde_crou.c ------------------------------*/
/*--------------------------- exptilde_crou.c ------------------------------*/
 
            /* other files are:  exptilde.f90  exptilde_crou.h */

/****
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
!                        C P S   P R I M I T I V E
!
! Name       : exptilde
! Category   : io
! Written    : 2000-01-03   by: Tom Stoeckley
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Expand the tilde (~) in a filename to an absolute path.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2005-05-31  Stoeckley  Fix to compile with C++.
!  1. 2000-10-19  Stoeckley  Initial version.
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
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


char EXPTILDE_CROU_IDENT[100] =
"$Id: exptilde_crou.c,v 1.2 2005/05/31 13:04:08 Stoeckley prod sps $";


#include "exptilde_crou.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#if (!VMS) || defined(_POSIX_SOURCE)
#include <pwd.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/


void  exptilde_crou1 (char *filename)
{
  char filename1[200];

  strcpy         (filename1, filename );
  exptilde_crou2 (filename , filename1);
}



void  exptilde_crou2 (char *filename2, const char *filename1)
{
  char *homestr, *restptr;
  const char *wrkptr;
  char  username[200];
  int diff;

#if ((!VMS) || defined(_POSIX_SOURCE))
     for(wrkptr= filename1; ( (*wrkptr) && (isspace(*wrkptr)) ); wrkptr++ );
     if (*wrkptr == '~')
         {
         if ( ( *(wrkptr+1) == '/' )
                           || ( *(wrkptr+1) == '\0' )
                           || ( *(wrkptr+1) == ' '  ) )
             {
             homestr= getenv ( "HOME");

             if (homestr) sprintf(filename2, "%s%s", homestr, wrkptr+1);
             else         strcpy (filename2, filename1);
             }
         else
             {
             struct passwd *pw;
             restptr= strchr (wrkptr, '/' );
             if (restptr)
                   {
                   diff= (int)(restptr-wrkptr);
                   strncpy( username, wrkptr, restptr-wrkptr );
                   username[diff]= '\0';
                   }
             else
                   {
                   strcpy( username, wrkptr);
                   }
             pw= getpwnam( &username[1] );

             if (pw) sprintf(filename2, "%s%s", pw->pw_dir, restptr );
             else    strcpy (filename2, filename1);
             }
         }
     else
         {
         strcpy(filename2, filename1);
         }
#else
     strcpy(filename2, filename1);
#endif
}


#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

