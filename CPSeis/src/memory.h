
/*------------------------------- memory.h --------------------------------*/
/*------------------------------- memory.h --------------------------------*/
/*------------------------------- memory.h --------------------------------*/

                    /* other files are:  memory.c */

/****
!<CPS_v1 type="HEADER_FILE"/>
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


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY 
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 1999-09-10  Stoeckley    Add reference to other files.
!  1. 1999-09-02  Stoeckley    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _MEMORY_H_
#define _MEMORY_H_


#ifdef __cplusplus
extern "C" {
#endif


/*------------------------ start of information --------------------------*/
/*------------------------ start of information --------------------------*/
/*------------------------ start of information --------------------------*/

 
void  *memory_alloc_generic (void *array, long n, long size, int *error);
void   memory_copy_generic  (void *array2, long index2,
                             void *array1, long index1, long ncopy, long size);

long   memory_insert_generic  (void *array, long n, long size, long index,
                                 long nins, void *values);
long   memory_remove_generic  (void *array, long n, long size, long index,
                                 long nrem);
long   memory_replace_generic (void *array, long n, long size, long index,
                                 long nrep, void *values);
long   memory_rem_ins_generic (void *array, long n, long size, long index,
                                 long nrem, long nins, void *values);
void   memory_fetch_generic   (void *array, long n, long size, long index,
                                 long nget, void *values);
 
float *memory_alloc_floats    (float *array, long n, int *error);
void   memory_copy_floats     (float *array2, long index2,
                               float *array1, long index1, long ncopy);

long   memory_insert_floats   (float *array, long n, long index,
                                 long nins, float *values);
long   memory_remove_floats   (float *array, long n, long index,
                                 long nrem);
long   memory_replace_floats  (float *array, long n, long index,
                                 long nrep, float *values);
long   memory_rem_ins_floats  (float *array, long n, long index,
                                 long nrem, long nins, float *values);
void   memory_fetch_floats    (float *array, long n, long index,
                                 long nget, float *values);
 
char  *memory_alloc_chars     (char *array, long n, int *error);
void   memory_copy_chars      (char *array2, long index2,
                               char *array1, long index1, long ncopy);

long   memory_insert_chars    (char *array, long n, long index,
                                 long nins, char *values);
long   memory_remove_chars    (char *array, long n, long index,
                                 long nrem);
long   memory_replace_chars   (char *array, long n, long index,
                                 long nrep, char *values);
long   memory_rem_ins_chars   (char *array, long n, long index,
                                 long nrem, long nins, char *values);
void   memory_fetch_chars     (char *array, long n, long index,
                                 long nget, char *values);
 

/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/


#ifdef __cplusplus
}
#endif


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

