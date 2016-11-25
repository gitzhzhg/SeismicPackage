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


/*--------------------------------- bswap.c ----------------------------------*/
/*--------------------------------- bswap.c ----------------------------------*/
/*--------------------------------- bswap.c ----------------------------------*/


/*
!<CPS_v1 type="PRIMITIVE"/>

!<brief_doc>
!-------------------------------------------------------------------------------
!<center>                C P S   P R I M I T I V E               </center>
!
! Name       : bswap 
! Category   : prim_io
! Written    : 1991-10-16   by: Richard S. Day
! Revised    : 1999-08-31   by: Donna K. Vunderink
! Maturity   : production   2000-06-30
! Purpose    : Swap the bytes in an array of 4 byte words.
! Portability: Requires 4-byte word sizes.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!<center>                 GENERAL DESCRIPTION                   </center>
!
!<pm>
! This primitive is used to swap bytes in an array of 4 byte words.
! If the machine is not byte swapped this primitive does nothing.
! If the machine is a Cray this primitive does nothing.
!</pm>
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!<center>              INPUT AND OUTPUT ARGUMENTS               </center>
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!<center>                  CALLING SEQUENCE                      </center>
!
!
!                  o             i     b
!                istat = bswap (num, ieee)
!
! int*           num      = number of floats in ieee
! unsigned char  ieee[]   = an array of 4 byte numbers
! int            istat    = always returns a zero
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!<center>                    ADVICE FOR USERS                    </center>
!
!<pm>
!</pm>
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<hist_doc>
!-------------------------------------------------------------------------------
!<center>                   REVISION HISTORY                     </center>
!<tf pos=
!" 1  2           3            4">
!     Date        Author       Description
!     ----        ------       -----------
!  3.
!  2. 1999-08-31  Vunderink    Converted from old system.
!  1. 1991-10-16  Day          Initial version.
!</tf>
!
!
!-------------------------------------------------------------------------------
!</hist_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!<center>                 PORTABILITY LIMITATIONS                </center>
!
! This process requires 4-byte word sizes.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!<center>              SPECIAL COMPILING REQUIREMENTS             </center>
!
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!<center>           ALGORITHM DESCRIPTION FOR DEVELOPERS          </center>
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!<center>                   PROGRAMMING NOTES                    </center>
!
!<pm>
!</pm>
!
!-------------------------------------------------------------------------------
!</programming_doc>
*/

#ifdef __cplusplus
extern "C"
{
#endif

#include "bswap.h"

int  bswap ( int  *num,unsigned char ieee[])
{
  unsigned long swaptest = 1;
  unsigned char c1;
  register int i,i1,i2,i3,i4;
#ifdef CRAY
if(*num > 0 ) return 0;
#endif

  if  (*(char *) &swaptest)
  { for (i=0; i<*num; i++)
    {
      /* byte swapping */
      i1 = i*4;
      i2 = i1+1;
      i3 = i1+2;
      i4 = i1+3;
      c1 = ieee[i1];
      ieee[i1] = ieee[i4];
      ieee[i4] = c1;
      c1 = ieee[i2];
      ieee[i2] = ieee[i3];
      ieee[i3] = c1;

    }
  }
  else return 0;

 return *num;
}
#ifdef __cplusplus
}
#endif
