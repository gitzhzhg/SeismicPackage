/*<CPS_v1 type="PROGRAM"/>

!------------------------------- filereserve.c ---------------------------------
!------------------------------- filereserve.c ---------------------------------
!------------------------------- filereserve.c ---------------------------------
!
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
! Name       : FILERESERVE
! Category   : io
! Written    : 2003-05-27   by: Charles C. Burch
! Revised    : 2003-05-25   by: Charles C. Burch
! Maturity   : production
! Purpose    : A program called using rsh to create a file and reserve space
! References : This routine are called from within pfio.c
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! This program creates a file of a specified size all filled with zeroes to 
!   actually commit the disk space.
! program is invoked by "filereserve filename file_size"
!-------------------------------------------------------------------------------
!</descript_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
!  1. 2003-05-25  C C Burch      Initial version
!
!  This replaces reserve_file_space.c to allow file size > 2Gb
!  History of reserve_file_space is below:
!  1. 2001-01-22  Chuck C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>
*/
/*********************************************************************
*  To link this program: 
*    gcc -o filereserve filereserve.c pfio.o bfio.o skio.o
*  pfio.c, bfio.c and skio.c are CPS primitives
*  One could extract the needed subroutines by linking to CPS libraries
**********************************************************************/
/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

#include <stdlib.h>

#include "str.h"
#include "pfio.h"

char *filereserve_ident =
"$Id: filereserve.c,v 1.1 2003/07/28 13:13:05 Burch prod sps $";

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/


int main (int argc, char *argv[]) {
 
 if(argc<3) {
    return(-1);
  }
  return(pfio_reserve_file_space(argv[1],(void *) str_atoll(argv[2])));
}

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
