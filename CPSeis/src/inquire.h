/****
!<CPS_v1 type="HEADER_FILE"/>

!!------------------------------- inquire.h --------------------------------!!
!!------------------------------- inquire.h --------------------------------!!
!!------------------------------- inquire.h --------------------------------!!

!                         other files are:  inquire.c

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
! Name       : INQUIRE
! Category   : io
! Written    : 1993-08-31   by: Tom Stoeckley
! Revised    : 2004-08-23   by: Tom Stoeckley
! Maturity   : production
! Purpose    : This is a C-language primitive to inquire about a disk file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  7. 2004-08-23  Stoeckley  Added inquire_file_quickly, inquire_input_quickly,
!                             and inquire_output_quickly.
!  6. 2003-07-10  Stoeckley  Added inquire_fetch_brief_message and
!                             inquire_fetch_message.
!  5. 2000-10-19  Stoeckley  Change VALID_YES, VALID_NO, and VALID_MAYBE to
!                             INQUIRE_VALID_YES, INQUIRE_VALID_NO, and
!                             INQUIRE_VALID_MAYBE to resolve a conflict in
!                             ~spws.
!  4. 2000-10-06  Stoeckley  Add MSG argument to inquire_file, inquire_input,
!                             and inquire_output.
!  3. 1999-10-20  Stoeckley  Add functions inquire_input and inquire_output,
!                             and change some char* arguments to const char*.
!  2. 1999-09-10  Stoeckley  Add reference to other files in this primitive.
!  1. 1999-09-02  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _INQUIRE_H_
#define _INQUIRE_H_


#ifdef __cplusplus
extern "C" {
#endif


/*------------------------ start of information --------------------------*/
/*------------------------ start of information --------------------------*/
/*------------------------ start of information --------------------------*/

 
#define INQUIRE_VALID_YES    1  /* file is a valid file of appropriate type */
#define INQUIRE_VALID_NO    -1  /* file is NOT valid file of appropriate type */
#define INQUIRE_VALID_MAYBE  0  /* do not know and do not care */
 
#define FILE_BLANK           1  /* filename is "NONE" or "" (etcetera) */
#define FILE_NOT_FOUND       2  /* file does not exist but is writeable */
#define FILE_NOT_CREATEABLE  3  /* file does not exist and is not writeable */
#define FILE_FOUND           4  /* file exists and is readable/writeable */
#define FILE_NOT_READABLE    5  /* file exists but is not readable */
#define FILE_NOT_WRITEABLE   6  /* file exists but is not writeable */
#define FILE_NOT_READ_WRITE  7  /* file exists but is not readable/writeable */
#define FILE_VALID          21  /* file exists, is readable/writeable, and
                                     is a valid file of appropriate type */
#define FILE_ERROR          41  /* a file error occurred */
#define FILE_CREATE         42  /* output file will be created from scratch */
#define FILE_READ_ONLY      43  /* input file will be read-only */
#define FILE_COPY           44  /* input file will be copied to output file */
#define FILE_UPDATE         45  /* input file will be updated */
#define FILE_CHANGES        61  /* changes occurred in files or their status */
 
const char *inquire_fetch_message();
const char *inquire_fetch_brief_message();
 
long inquire_file        (const char *filename, char *msg);
long inquire_input       (const char *filename, char *msg);
long inquire_output      (const char *filename, char *msg);
 
long inquire_file_quickly        (const char *filename, char *msg);
long inquire_input_quickly       (const char *filename, char *msg);
long inquire_output_quickly      (const char *filename, char *msg);
 
long inquire_input_file  (const char *filename, long required, long valid,
                          const char *filetype, const char *info, char *msg);
 
long inquire_output_file (const char *filename, long required, long valid,
                          const char *filetype, const char *info, char *msg);
 
long inquire_files       (const char *filename1, const char *filename2,
                          long status1, long status2, long same_datasets,
                          char *msg1, char *msg2, char *msg3);
 
long inquire_files_alternate
               (const char *filename1, const char *filename2,
                long status1, long status2, long same_datasets,
                char *msg1, char *msg2, char *msg3);
 
long inquire_files_nonmatching
               (const char *filename1, const char *filename2,
                long status1, long status2, long same_datasets,
                char *msg1, char *msg2, char *msg3);
 
long inquire_files_combo
               (const char *filename1, const char *filename2,
                const char *filetype, long required1, long required2,
                long valid1, long valid2, const char *info1, const char *info2,
                long same_datasets, long *status1, long *status2,
                char *msg1, char *msg2, char *msg3);
 
long inquire_files_combo_alternate
               (const char *filename1, const char *filename2,
                const char *filetype, long required1, long required2,
                long valid1, long valid2, const char *info1, const char *info2,
                long same_datasets, long *status1, long *status2,
                char *msg1, char *msg2, char *msg3);
 
 
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

