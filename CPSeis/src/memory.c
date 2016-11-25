/****
!<CPS_v1 type="PRIMITIVE"/>
****/

/*------------------------------ memory.c ----------------------------------*/
/*------------------------------ memory.c ----------------------------------*/
/*------------------------------ memory.c ----------------------------------*/
 
                      /* other files are:  memory.h */

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
! Name       : MEMORY
! Category   : memory
! Written    : 1994-12-03   by: Tom Stoeckley
! Revised    : 1999-11-17   by: Tom Stoeckley
! Maturity   : production   2000-07-27
! Purpose    : To perform various C-language memory management functions.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION          
!
! Functions in this primitive are designed to perform various C-language
! memory management functions such as inserting, removing, or copying data,
! and reallocating memory.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE             
!
!  1.  The functions whose names end with the suffix "_generic"
!      work with void* pointers to arrays, and take an argument
!      which is the size of one element in the array.
!
!  2.  The functions whose names end with other suffixes work with
!      other types of pointers.  These functions call the corresponding
!      "_generic" functions and therefore work exactly like the
!      "_generic" functions.  The following functions are available:
!
!               suffix       type of pointer to arrays
!               ------       -------------------------
!               _float             float*
!               _char              char*
!
!  3.  The functions whose names end with the suffix "_chars"
!      work with char* pointers to arrays.  These functions call
!      the corresponding "_generic" functions and therefore work
!      exactly like the "_generic" functions.
!
!  4.  The functions which remove or insert or replace values in an
!      array will do this in-place, without changing the location
!      of the array in memory.  It is assumed that the allocated size
!      of the array is large enough to be able to grow without
!      overflowing memory.  The following functions work this way:
!
!                memory_insert_generic   or  ..._floats
!                memory_remove_generic   or  ..._floats
!                memory_replace_generic  or  ..._floats
!                memory_rem_ins_generic  or  ..._floats
!
!  5.  If an array is about to grow, but its allocated size is not
!      large enough (or if the array is NULL), you must call one
!      of the following functions.  These will allocate or reallocate
!      the requested space, and will copy the existing data to the
!      new memory location if the memory is moved.
!
!                memory_alloc_generic    or  ..._floats
!
!  6.  See the header file for details of available functions and their
!      specific arguments.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  5. 1999-11-17  Stoeckley  Add ident string for RCS.
!  4. 1999-09-10  Stoeckley  Add reference to other files.
!  3. 1999-09-09  Stoeckley  Converted from old system.
!                              Name changed from memory_util to memory.
!  2. 1995-05-17  Stoeckley  Change the way memory_copy_generic works,
!                            such that if the input array is NULL, the
!                            routine will behave as if the input array
!                            was filled with zeroes.  Previously, this
!                            routine would assert if the input array
!                            was NULL.
!  1. 1994-12-03  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


#include "memory.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


char MEMORY_IDENT[100] = "$Id: memory.c,v 1.5 2000/07/26 12:28:07 sps prod sps $";


/*--------------- routines dealing with floats -----------------*/
/*--------------- routines dealing with floats -----------------*/
/*--------------- routines dealing with floats -----------------*/


#define SFL  sizeof(float)


float *memory_alloc_floats(float *array, long n, int *error)
{
  return (float*)memory_alloc_generic(array, n, SFL, error);
}


void memory_copy_floats(float *array2, long index2,
                        float *array1, long index1, long ncopy)
{
  memory_copy_generic(array2, index2,
                      array1, index1, ncopy, SFL);
}


long memory_insert_floats(float *array, long n, long index,
                          long nins, float *values)
{
  return memory_insert_generic(array, n, SFL, index,
                               nins, values);
}


long memory_remove_floats(float *array, long n, long index,
                          long nrem)
{
  return memory_remove_generic(array, n, SFL, index,
                               nrem);
}


long memory_replace_floats(float *array, long n, long index,
                           long nrep, float *values)
{
  return memory_replace_generic(array, n, SFL, index,
                                nrep, values);
}


long memory_rem_ins_floats(float *array, long n, long index,
                           long nrem, long nins, float *values)
{
  return memory_rem_ins_generic(array, n, SFL, index,
                                nrem, nins, values);
}


void memory_fetch_floats(float *array, long n, long index,
                         long nget, float *values)
{
  memory_fetch_generic(array, n, SFL, index,
                       nget, values);
}



/*--------------- routines dealing with chars -----------------*/
/*--------------- routines dealing with chars -----------------*/
/*--------------- routines dealing with chars -----------------*/


#define SCH  sizeof(char)


char *memory_alloc_chars(char *array, long n, int *error)
{
  return (char*)memory_alloc_generic(array, n, SCH, error);
}


void memory_copy_chars(char *array2, long index2,
                       char *array1, long index1, long ncopy)
{
  memory_copy_generic(array2, index2,
                      array1, index1, ncopy, SCH);
}


long memory_insert_chars(char *array, long n, long index,
                         long nins, char *values)
{
  return memory_insert_generic(array, n, SCH, index,
                               nins, values);
}


long memory_remove_chars(char *array, long n, long index,
                         long nrem)
{
  return memory_remove_generic(array, n, SCH, index,
                               nrem);
}


long memory_replace_chars(char *array, long n, long index,
                          long nrep, char *values)
{
  return memory_replace_generic(array, n, SCH, index,
                                nrep, values);
}


long memory_rem_ins_chars(char *array, long n, long index,
                          long nrem, long nins, char *values)
{
  return memory_rem_ins_generic(array, n, SCH, index,
                                nrem, nins, values);
}


void memory_fetch_chars(char *array, long n, long index,
                        long nget, char *values)
{
  memory_fetch_generic(array, n, SCH, index,
                       nget, values);
}



/*---------------- memory alloc generic ---------------------*/
/*---------------- memory alloc generic ---------------------*/
/*---------------- memory alloc generic ---------------------*/

/*
    Allocates or reallocates generic array to length n times size.
    Frees the array if n == 0.
    If successful and the contents are moved:
        The contents are copied to the new address.
        The new pointer is returned, pointing to n array elements.
        *error is set to 0.
    If successful and the contents are not moved:
        The old pointer is returned, pointing to n array elements.
        *error is set to 0.
    If unsuccessful:
        The old pointer is returned, pointing to the previous memory
           size and contents.
        *error is set to 1.
*/


void *memory_alloc_generic(void *array, long n, long size, int *error)
{
  void *new_array;
  assert(n >= 0);
  assert(size > 0);
  assert(error);
  if(n == 0)
      {
      if(array) free(array);
      *error = 0;
      return NULL;
      }
  if(array) new_array = (void*)realloc(array, n * size);
  else      new_array = (void*) malloc(       n * size);
  if(new_array)
     {
     *error = 0;
     return new_array;
     }
  *error = 1;
  return array;
}



/*----------------- memory copy generic ------------------*/
/*----------------- memory copy generic ------------------*/
/*----------------- memory copy generic ------------------*/

/*
        Copies specified number of elements of specified size
                from array1 (beginning at index1)
                to   array2 (beginning at index2).
        The two arrays may overlap.

        If array1 is NULL, the specified part of array2 will be filled
          with zeroes.  This is equivalent to the case where array1
          is filled with zeroes.  This feature was added 5/17/95.
          Originally, this routine would assert if array1 was NULL.
*/


void memory_copy_generic(void *array2, long index2,
                         void *array1, long index1, long ncopy, long size)
{
  char *vvv2;
  char *vvv1;
  assert(index2 >= 0);
  assert(index1 >= 0);
  assert(ncopy  >= 0);
  assert(size   >  0);
  if(ncopy == 0) return;
  assert(array2);
/*
  assert(array1);
*/
  if(array1)
      {
      vvv2 = (char*)array2 + size * index2;
      vvv1 = (char*)array1 + size * index1;
      if(vvv2 == vvv1) return;
      memmove(vvv2, vvv1, size * sizeof(char) * ncopy);
      }
  else
      {
      vvv2 = (char*)array2 + size * index2;
      memset(vvv2, 0, size * sizeof(char) * ncopy);
      }
}



/*---------------- memory insert generic ------------------*/
/*---------------- memory insert generic ------------------*/
/*---------------- memory insert generic ------------------*/

       /* the length of the array might grow */
       /* the changes are made in-place to the existing array */
       /* the array is assumed to have enough space to grow */
       /* returns new length of array */


long memory_insert_generic(void *array, long n, long size, long index,
                           long nins, void *values)
{
  return memory_rem_ins_generic(array, n, size, index,
                                0, nins, values);
}



/*--------------- memory remove generic -------------------*/
/*--------------- memory remove generic -------------------*/
/*--------------- memory remove generic -------------------*/

       /* the length of the array might shrink */
       /* the changes are made in-place to the existing array */
       /* returns new length of array */


long memory_remove_generic(void *array, long n, long size, long index,
                           long nrem)
{
  return memory_rem_ins_generic(array, n, size, index,
                                nrem, 0, NULL);
}



/*---------------- memory replace generic ------------------*/
/*---------------- memory replace generic ------------------*/
/*---------------- memory replace generic ------------------*/

       /* the length of the array will not change */
       /* the changes are made in-place to the existing array */
       /* returns length of array anyway */


long memory_replace_generic(void *array, long n, long size, long index,
                            long nrep, void *values)
{
  return memory_rem_ins_generic(array, n, size, index,
                                nrep, nrep, values);
}



/*------------ memory remove/insert generic ----------------*/
/*------------ memory remove/insert generic ----------------*/
/*------------ memory remove/insert generic ----------------*/

       /* the length of the array might grow or shrink */
       /* the changes are made in-place to the existing array */
       /* the array is assumed to have enough space to grow */
       /* returns new length of array */


long memory_rem_ins_generic(void *array, long n, long size, long index,
                            long nrem, long nins, void *values)
{
  assert(index >= 0 && index <= n);
  assert(nrem  >= 0 && nrem  <= n - index);
  assert(nins  >= 0);
  assert(n     >= 0);

  memory_copy_generic(array , index + nins,
                      array , index + nrem, n - index - nrem, size);
  memory_copy_generic(array , index       ,
                      values, 0           , nins            , size);
  return n + nins - nrem;
}



/*------------------- memory fetch generic ---------------*/
/*------------------- memory fetch generic ---------------*/
/*------------------- memory fetch generic ---------------*/


void memory_fetch_generic(void *array, long n, long size, long index,
                          long nget, void *values)
{
  if(nget == 0) return;

  assert(index   >= 0 && index   <  n);
  assert(nget >= 0 && nget <= n - index);
  assert(n       >= 0);

  memory_copy_generic(values, 0    ,
                      array , index, nget, size);
}



/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/

