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
/*
!<CPS_v1 type="PRIMITIVE"/>
!              SEE ALSO:  swap_frou.f90  swap.h
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E 
!
! Name       : swap 
! Category   : io
! Written    : 1993-06-01   by: Jens Hartmann
! Revised    : 2007-03-13   by: Kruger Corn
! Maturity   : beta
! Purpose    : Swap bytes for any words between BIG and LITTLE endian
!              machines.
! Portability: No known limitations.
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
! These routines are necessary for reversing the byte order of binary data
! for transportation between big-endian and little-endian machines. Examples
! of big-endian machines are IBM RS6000, SUN, NeXT. Examples of little
! endian machines are PC's and DEC.
! 
! These routines have been tested with PC data and run on PC's running
! several PC versions of UNIX, but have not been tested on DEC.
!
! The "_unk" routines were added so that F90 could call generic function with
! one "type" but swap bytes as if it were another type. (see notes below.)
!-------------------------------------------------------------------------------
!</descript_doc>
!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!  
!  FORTRAN INTERFACE:
!
!   integer    :: endian ! 1 = BIG endian, 0 = LITTLE endian.
!   integer    :: size   ! # of bytes in each word of "x" (may differ from 
!                          the "declared type" of "x") (it is ok to do so.)
!   integer    :: lgth   ! # of words to swap, starting at address of "x".
!   real | double | int | short :: x OR x(:) (scalar or vector)
!
!   endian = swap_endian()
!                   b 
!   call swap_bytes(x) (x may be scalar or vector (:) )
!                   b i
!   call swap_bytes(x,lgth) (x cannot be (:), must be scalar)
!                       b i
!   call swap_bytes_unk(x,size) (x may be scalar or vector (:) )
!                       b i    i
!   call swap_bytes_unk(x,size,lgth) (x cannot be (:), must be scalar)
!   EXAMPLES:
!
!    real :: x(100)
!    call swap_bytes(x)
!    call swap_bytes(x(1),100)
!    call swap_bytes_unk(x,4)
!    call swap_bytes_unk(x(1),4,100)
!
!    Why so many variations?  What if I pack a double into an int*2?  Now
!    I need to swap bytes before writing a file, but I only want to swap
!    every other byte (for the int*2.)  I COULD equivalence an integer*2 with
!    the double precision (which is 8) but that may not be standard Fortran
!    in the future, so what I can do is:
!
!    double precision :: x(100)
!    call pack_8_into_2 (x) (hypothetical packer)
!    call swap_bytes_unk(x,2) OR
!    call swap_bytes_unk(x(1),2,100)
!    **** I CANNOT CALL swap_bytes(x) because it will swap on an 8-byte word
!    **** boundary since the "type" of "x" is double precision!!!!!!!!
!
!    swap_bytes_unk IS SLOWER than swap_bytes, because it has to do a test
!    before the swap.  (not much slower, however, since only one test per
!    call, regardless of the length of the vector.)
!
!  "C" "C++" INTERFACE:
!
!     Each first argument in the argument lists is of the type shown in the
!     function prototype below, and is passed by reference.
!
!     int size       ! size is the size in bytes of the word(s) to be swapped.
!     int lgth       ! lgth is the length of a vector with start address of 
!                    ! the first argument. 
!     int endian     ! Endian is 1 for BIG ENDIAN machine, 0 for LITTLE ENDIAN.
!          endian  = swap_endian()
!          Purpose: To tell if the machine is BIG or LITTLE endian. 1=big,0=lit.
!
!
!                                    b 
!                    swap_short_2  (short *tni2);
!                    swap_u_short_2(unsigned short *tni2);
!                    swap_int_4    (int *tni4);
!                    swap_u_int_4  (unsigned int *tni4);
!                    swap_long_4   (int32_t *tni4);
!                    swap_u_long_4 (int32_t *tni4);
!                    swap_long_8   (int64_t *tni8);
!                    swap_u_long_8 (uint64_t *tni8);
!                    swap_float_4  (float *tnf4);
!                    swap_double_8 (double *tndd8);
!                                             b            i
!                    swap_double_8_cvec (double *tndd8,int * lgth);
!                    swap_int_4_cvec    (int *tni4,    int * lgth);
!                    swap_long_4_cvec   (int32_t *tni4,   int * lgth);
!                    swap_long_8_cvec   (int64_t *tni8,   int * lgth);
!                    swap_short_2_cvec  (short *tni2,  int * lgth);
!                    swap_float_4_cvec  (float *tnf4,  int * lgth);
!                                         b             i          i
!                    swap_unk          (void * tn, int * size)     
!                    swap_unk_cvec     (void * tn, int * size, int * lgth)
!                    swap_unk_2i       (void * tn, int * size)     
!                    swap_unk_cvec_2i  (void * tn, int * size, int * lgth)
!                    swap_unk_4i       (void * tn, int * size)     
!                    swap_unk_cvec_4i  (void * tn, int * size, int * lgth)
!                    swap_unk_4f       (void * tn, int * size)     
!                    swap_unk_cvec_4f  (void * tn, int * size, int * lgth)
!                    swap_unk_8f       (void * tn, int * size)     
!                    swap_unk_cvec_8f  (void * tn, int * size, int * lgth)
!          Purpose of all the above: To swap bytes from one endian to the other
!                                    for the variable that was passed in the
!                                    first argument.
!          (swapping is done in place.)
!
!-------------------------------------------------------------------------------
!</calling_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
!  7. 2007-03-13  Corn           Added _long_8 routines.
!  6. 2002-06-10  Stoeckley      Added swap_long_4_cvec.
!  5. 2000-09-15  Bill Menger    Removed tabs.
!  4. 1999-10-21  Bill Menger    Added 2-argument calling capability for vectors
!  3. 1999-09-30  Bill Menger    Converted from CWP.
!  2. 1994-01-01  John Stockwell CWP, Colorado School of Mines
!  1. 1993-06-01  Jens Hartmann  Institut fur Geophysik, Hamburg
!
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
! This process expects 4-byte (32-bit) word sizes.
!-------------------------------------------------------------------------------
!</portability_doc>
*/

#include "swap.h"

char *swap_ident = 
"$Id: swap.c,v 1.6 2002/06/07 14:47:07 Stoeckley prod sps $";

int swap_endian() {
    /* BIG_ENDIAN = 1, LITTLE_ENDIAN=0 */
    /* SUN = big, INTEL and VAX = 0 */
    uint32_t swaptest = 1;
    if    (*(char *) &swaptest)return 0;
    return 1;
}

void swap_unk(void *tn, int * size)
/**************************************************************************
swap_unk        swap a word of "size" bytes.
                size MUST be 2, 4, or 8 currently
                    or
                nothing will happen.
***************************************************************************/
{
    short  * tni2 = (short  * ) tn;
    int    * tni4 = (int    * ) tn;
    char   * tnd8 = (char   * ) tn;
    char     tnc;
    
    switch (*size) {
        case 2:
                 *tni2=(((*tni2>>8)&0xff) | ((*tni2&0xff)<<8));  
                break;
        case 4:
                 *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
                       ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));  
                break;
        case 8:
                tnc= *tnd8;
                *tnd8= *(tnd8+7);
                *(tnd8+7)=tnc;
                
                tnc= *(tnd8+1);
                *(tnd8+1)= *(tnd8+6);
                *(tnd8+6)=tnc;
                
                tnc= *(tnd8+2);
                *(tnd8+2)= *(tnd8+5);
                *(tnd8+5)=tnc;
                
                tnc= *(tnd8+3);
                *(tnd8+3)= *(tnd8+4);
                *(tnd8+4)=tnc;
                break;
        default:;
    }
}

void swap_unk_cvec(void * tn, int * size, int * lgth)
/**************************************************************************
swap_unk        swap a vector of length lgth, consisting of words 
                of "size" bytes
                size MUST be 2, 4, or 8 currently.
                lgth MUST be >0 
                    or
                nothing will happen.
***************************************************************************/
{
    int    i = -1;
    short  * tni2     = (short  * ) tn;
    int    * tni4     = (int    * ) tn;
    double * tnd8dble = (double * ) tn;
    char   * tnd8;
    char     tnc;
    
    switch (*size) {
        case 2:
                while(++i <*lgth) {
                     *tni2=(((*tni2>>8)&0xff) | ((*tni2&0xff)<<8));  
                    tni2++;
                }
                break;
        case 4:
                while(++i <*lgth) {
                     *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
                           ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));  
                    tni4++;
                }
                break;
        case 8:
                while(++i <*lgth) {
                    tnd8 = (char * ) tnd8dble;
                    tnc= *tnd8;
                    *tnd8= *(tnd8+7);
                    *(tnd8+7)=tnc;
                    
                    tnc= *(tnd8+1);
                    *(tnd8+1)= *(tnd8+6);
                    *(tnd8+6)=tnc;
                    
                    tnc= *(tnd8+2);
                    *(tnd8+2)= *(tnd8+5);
                    *(tnd8+5)=tnc;
                    
                    tnc= *(tnd8+3);
                    *(tnd8+3)= *(tnd8+4);
                    *(tnd8+4)=tnc;
                    tnd8dble++;
                }
                break;
        default:;
    }
}

void swap_unk_2i    (void * tn, int * size)
{
    swap_unk (tn,size);
}
void swap_unk_4i    (void * tn, int * size)
{
    swap_unk (tn,size);
}
void swap_unk_4f    (void * tn, int * size)
{
    swap_unk (tn,size);
}
void swap_unk_8f    (void * tn, int * size)
{
    swap_unk (tn,size);
}
void swap_unk_cvec_2i  (void * tn, int * size, int * lgth)
{
    swap_unk_cvec (tn,size,lgth);
}
void swap_unk_cvec_4i  (void * tn, int * size, int * lgth)
{
    swap_unk_cvec (tn,size,lgth);
}
void swap_unk_cvec_4f  (void * tn, int * size, int * lgth)
{
    swap_unk_cvec (tn,size,lgth);
}
void swap_unk_cvec_8f  (void * tn, int * size, int * lgth)
{
    swap_unk_cvec (tn,size,lgth);
}


void swap_short_2(short *tni2)
/**************************************************************************
swap_short_2        swap a short integer
***************************************************************************/
{
 *tni2=(((*tni2>>8)&0xff) | ((*tni2&0xff)<<8));  
}

void swap_short_2_cvec(short *tni2,int * lgth)
/**************************************************************************
swap_short_2        swap a short integer of length lgth
***************************************************************************/
{
 short *x = tni2;
 int    i = -1;
 while(++i <*lgth) {
   *x = (((*x>>8)&0xff) | ((*x&0xff)<<8));  
   x++;
 }
}

void swap_u_short_2(unsigned short *tni2)
/**************************************************************************
swap_u_short_2        swap an unsigned short integer
***************************************************************************/
{
 *tni2=(((*tni2>>8)&0xff) | ((*tni2&0xff)<<8));  
}

void swap_int_4(int *tni4)
/**************************************************************************
swap_int_4        swap a 4 byte integer
***************************************************************************/
{
 *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
        ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));  
}

void swap_int_4_cvec(int *tni4, int *lgth)
/**************************************************************************
swap_int_4        swap a 4 byte integer vector of length lgth
***************************************************************************/
{
 int *x = tni4;
 int  i = -1;
 while( ++i < *lgth) {
   *x=(((*x>>24)&0xff) | ((*x&0xff)<<24) |
       ((*x>>8)&0xff00) | ((*x&0xff00)<<8));  
   x++;
 }
}

void swap_long_4_cvec(int32_t *tni4, int *lgth)
/**************************************************************************
swap_long_4             swap a 4 byte integer vector of length lgth
***************************************************************************/
{
 int *x = (int *)tni4;
 int  i = -1;
 while( ++i < *lgth) {
   *x=(((*x>>24)&0xff) | ((*x&0xff)<<24) |
          ((*x>>8)&0xff00) | ((*x&0xff00)<<8));
   x++;
 }
}

void swap_long_8_cvec(int64_t *tni8, int *lgth)
/**************************************************************************
swap_long_8             swap an 8 byte integer vector of length lgth
***************************************************************************/
{
 int64_t *x = (int64_t *)tni8;
 int  i = -1;
 while( ++i < *lgth) {
   *x=(((*x>>56)&0xff      ) | ((*x&0xff      )<<56) |
       ((*x>>40)&0xff00    ) | ((*x&0xff00    )<<40) |
       ((*x>>24)&0xff0000  ) | ((*x&0xff0000  )<<24) |
       ((*x>>8 )&0xff000000) | ((*x&0xff000000)<<8 )  );
   x++;
 }
}

void swap_u_int_4(unsigned int *tni4)
/**************************************************************************
swap_u_int_4        swap an unsigned integer
***************************************************************************/
{
 *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
        ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));  
}

void swap_long_4(int32_t *tni4)
/**************************************************************************
swap_long_4        swap a long integer
***************************************************************************/
{
 *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
        ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));  
}

void swap_u_long_4(uint32_t *tni4)
/**************************************************************************
swap_u_long_4        swap a 4 byte unsigned long integer
***************************************************************************/
{
 *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
        ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));  
}

void swap_long_8(int64_t *tni8)
/**************************************************************************
swap_long_8             swap an 8 byte long integer
***************************************************************************/
{
 *tni8=(((*tni8>>56)&0xff      ) | ((*tni8&0xff      )<<56) |
        ((*tni8>>40)&0xff00    ) | ((*tni8&0xff00    )<<40) |
        ((*tni8>>24)&0xff0000  ) | ((*tni8&0xff0000  )<<24) |
        ((*tni8>>8 )&0xff000000) | ((*tni8&0xff000000)<<8 )  );
}

void swap_u_long_8(uint64_t *tni8)
/**************************************************************************
swap_u_long_8             swap an 8 byte unsigned long integer
***************************************************************************/
{
 *tni8=(((*tni8>>56)&0xff      ) | ((*tni8&0xff      )<<56) |
        ((*tni8>>40)&0xff00    ) | ((*tni8&0xff00    )<<40) |
        ((*tni8>>24)&0xff0000  ) | ((*tni8&0xff0000  )<<24) |
        ((*tni8>>8 )&0xff000000) | ((*tni8&0xff000000)<<8 )  );
}

void swap_float_4(float *tnf4)
/**************************************************************************
swap_float_4        swap a float
***************************************************************************/
{
 int *tni4=(int *)tnf4;
 *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
        ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));  
}

void swap_float_4_cvec(float *tnf4, int *lgth)
/**************************************************************************
swap_float_4        swap a float vector of length lgth
***************************************************************************/
{
 int     i = -1;
 int *tni4=(int *)tnf4;
 while (++i < *lgth) {
   *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
         ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));  
   tni4++;
 }
}

void swap_double_8(double *tndd8)
/**************************************************************************
swap_double_8        swap a double
***************************************************************************/
{
  char *tnd8=(char *)tndd8;
  char tnc;

  tnc= *tnd8;
  *tnd8= *(tnd8+7);
  *(tnd8+7)=tnc;

  tnc= *(tnd8+1);
  *(tnd8+1)= *(tnd8+6);
  *(tnd8+6)=tnc;

  tnc= *(tnd8+2);
  *(tnd8+2)= *(tnd8+5);
  *(tnd8+5)=tnc;

  tnc= *(tnd8+3);
  *(tnd8+3)= *(tnd8+4);
  *(tnd8+4)=tnc;
}

void swap_double_8_cvec(double *tndd8, int * lgth)
/**************************************************************************
swap_double_8        swap a double vector of length lgth
***************************************************************************/
{
  int  i = -1;
  double *x = tndd8;
  char *tnd8=(char *)tndd8;
  char tnc;
  while (++i < *lgth) {
    tnd8 = (char * )x;
    tnc= *tnd8;
    *tnd8= *(tnd8+7);
    *(tnd8+7)=tnc;
  
    tnc= *(tnd8+1);
    *(tnd8+1)= *(tnd8+6);
    *(tnd8+6)=tnc;
  
    tnc= *(tnd8+2);
    *(tnd8+2)= *(tnd8+5);
    *(tnd8+5)=tnc;
  
    tnc= *(tnd8+3);
    *(tnd8+3)= *(tnd8+4);
    *(tnd8+4)=tnc;
    x++;
  }
}

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

