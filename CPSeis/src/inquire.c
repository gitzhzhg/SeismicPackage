/****
!<CPS_v1 type="PRIMITIVE"/>

!!------------------------------- inquire.c ----------------------------------!!
!!------------------------------- inquire.c ----------------------------------!!
!!------------------------------- inquire.c ----------------------------------!!
 
!                      other files are:  inquire.h 

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


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! Inquire about the status of a disk file to learn whether
! the file exists, is readable, is writeable, is a valid
! file of the desired type, etc.  Also inquire about a
! pair of input/output files for compatibility.  Also get
! messages which can be displayed regarding the files.
!
! It is recommended that the file name does not contain any embedded
! white space characters or any unprintable characters.
!
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
!  To inquire about a file:
!                                             i      o
!                 status = inquire_file   (filename,msg)
!                 status = inquire_input  (filename,msg)
!                 status = inquire_output (filename,msg)
!
!                                                     i      o
!                 status = inquire_file_quickly   (filename,msg)
!                 status = inquire_input_quickly  (filename,msg)
!                 status = inquire_output_quickly (filename,msg)
!
!                 long   = inquire_fetch_message()
!                 brief  = inquire_fetch_brief_message()
!
!  const char* filename = name of file.
!  char*            msg = a message referring to the status of the file.
!  char*           long = a message referring to the status of the file.
!  const char*    brief = a brief message referring to the status of the file.
!  long          status = the status of the file (see below).
!
!  MSG can be NULL.
!
!  LONG is the last MSG returned (or potentially returned) by the last call
!  to one of the first three functions above.
!
!  BRIEF is a short version of the last MSG returned (or potentially returned)
!  by the last call to one of the first three functions above.
!
!  First three functions make a full inquiry (following links if necessary).
!  Next three functions do not follow links (to circumvent automount errors).
!
!  status (inquire_file)   description
!  ---------------------   -----------
!  FILE_BLANK              filename is blank ("NONE" or "none" or "" or NULL
!                            pointer or contains only whitespace characters).
!  FILE_NOT_FOUND          file does not exist but is writeable.
!  FILE_NOT_CREATEABLE     file does not exist and is not writeable.
!  FILE_FOUND              file exists and is readable and writeable.
!  FILE_NOT_READABLE       file exists but is not readable (but is writeable).
!  FILE_NOT_WRITEABLE      file exists but is not writeable (but is readable).
!  FILE_NOT_READ_WRITE     file exists but is not readable or writeable.
!
!  status (inquire_input)  description
!  ------------            -----------
!  FILE_FOUND              file exists and can be read.
!  FILE_ERROR              any other condition.
!
!  status (inquire_output) description
!  -------------           -----------
!  FILE_FOUND              file exists and can be overwritten.
!  FILE_NOT_FOUND          file does not exist and can be created.
!  FILE_ERROR              any other condition.
!
!  status (inquire_file_quickly)   description
!  -----------------------------   -----------
!  FILE_BLANK              filename is blank ("NONE" or "none" or "" or NULL
!                            pointer or contains only whitespace characters).
!  FILE_NOT_FOUND          file does not exist.
!  FILE_FOUND              file (or link) exists.
!
!  status (inquire_input_quickly)  description
!  ------------------------------  -----------
!  FILE_FOUND              file (or link) exists.
!  FILE_ERROR              any other condition.
!
!  status (inquire_output_quickly)  description
!  -------------------------------  -----------
!  FILE_FOUND              file (or link) exists.
!  FILE_NOT_FOUND          file does not exist.
!  FILE_ERROR              any other condition.
!
!-------------------------------------------------------------------------------
!  To inquire (more extensively) about an input or output file:
!
!                                  i         i        i       i       i     o
! status = inquire_input_file  (filename, required, valid, filetype, info, msg)
! status = inquire_output_file (filename, required, valid, filetype, info, msg)
!
!  const char* filename = name of input or output file.
!  long        required = TRUE if a filename must be specified.
!  long           valid = whether the file is a valid file (see below).
!  const char* filetype = type of file
!                           (e.g. "SCRS pickfile" or "CPS static file").
!  const char*     info = string to tack onto msg if status = FILE_VALID.
!  char*            msg = a message referring to the status of the file.
!  long          status = the status of the file (see below).
!
!  MSG cannot be null.
!
!  valid               description
!  -----               -----------
!  INQUIRE_VALID_YES   file is known to be a valid file of appropriate type.
!  INQUIRE_VALID_NO    file is known NOT to be a valid file of appropriate type.
!  INQUIRE_VALID_MAYBE do not know and do not care.
!
!  input status    description
!  ------------    -----------
!  FILE_BLANK      filename is blank (only if required = FALSE).
!  FILE_VALID      file is readable (only if INQUIRE_VALID_YES or MAYBE).
!  FILE_ERROR      there is an error (any other situation).
!
!  output status   description
!  -------------   -----------
!  FILE_BLANK      filename is blank (only if required = FALSE).
!  FILE_NOT_FOUND  file not found but can be created.
!  FILE_VALID      file is readable/writeable (only if INQUIRE_VALID_YES).
!  FILE_ERROR      there is an error (any other situation).
!
!  The above constants are defined in the inquire.h header file.
!  Note that a pre-existing output file can be overwritten or updated only if
!    valid = INQUIRE_VALID_YES (i.e. the pre-existing output file is known
!    to be of the appropriate type).
!  The filetype argument is needed for msg if valid = INQUIRE_VALID_YES or
!    INQUIRE_VALID_NO.
!  The info argument is needed for msg if valid = INQUIRE_VALID_YES and the
!    returned input or output status is FILE_VALID.
!  The info argument can be an empty string, or NULL.
!  The msg argument should be dimensioned at least [200].
!  The msg argument will contain two lines of description (with one \n).
!  The second line of msg (after the \n) will contain the info argument
!    if valid = INQUIRE_VALID_YES and the input or output status = FILE_VALID,
!    and will be empty otherwise (or if the info argument is NULL or empty).
!  These functions call inquire_file.
!
!-------------------------------------------------------------------------------
!  To inquire about a pair of input/output files:
!
!                                          i          i         i        i
!    status = inquire_files           (filename1, filename2, status1, status2,
!             inquire_files_alternate (filename1, filename2, status1, status2,
!
!               same_datasets, msg1, msg2, msg3)
!               same_datasets, msg1, msg2, msg3)
!                    i          i     i     o
!
!  const char* filename1 = name of input file.
!  const char* filename2 = name of output file.
!  long          status1 = status returned by inquire_input_file.
!  long          status2 = status returned by inquire_output_file.
!  long    same_datasets = TRUE if the files appear to refer to the
!                            same dataset (needed only if status1 and
!                            status2 are both FILE_VALID).
!  char*            msg1 = message returned by inquire_input_file.
!  char*            msg2 = message returned by inquire_output_file.
!  char*            msg3 = message referring to both files.
!  long           status = the status of the two files together (see below).
!
!  MSG1, MSG2, and MSG3 cannot be null.
!
!  status          description
!  ------          -----------
!  FILE_CREATE     output file will be created from scratch (no input file).
!  FILE_READ_ONLY  input file will be read-only (no output file).
!  FILE_COPY       input file will be copied to output file.
!  FILE_UPDATE     input file will be updated (input file = output file).
!  FILE_ERROR      there is an error (any other situation).
!
!  It is assumed that both file names have been expanded to their full
!    absolute path name, so that comparing the two names will
!    unambiguously determine whether the two names point to the
!    same file.
!  The above constants are defined in the inquire.h header file.
!  The msg3 argument will contain three lines of description (with
!     two occurrences of \n), although the second or third line might
!     be empty.
!  The msg3 argument should be dimensioned at least [300].
!
!  The second function is to be used if an alternate set of messages
!     is desired.
!
!-------------------------------------------------------------------------------
!  Combination convenience routine:
!
!                                                i          i         i
!    status = inquire_files_combo           (filename1, filename2, filetype,
!    status = inquire_files_combo_alternate (filename1, filename2, filetype,
!
!       required1, required2, valid1, valid2, info1, info2,
!       required1, required2, valid1, valid2, info1, info2,
!           i          i        i       i       i      i
!
!       same_datasets, &status1, &status2, msg1, msg2, msg3)
!       same_datasets, &status1, &status2, msg1, msg2, msg3)
!             i            o         o      o     o     o
!
!  const char* filename1 = name of input file.
!  const char* filename2 = name of output file.
!  const char*  filetype = type of file
!                            (e.g. "SCRS pickfile" or "CPS static file").
!  long        required1 = TRUE if the input file must be specified.
!  long        required2 = TRUE if the output file must be specified.
!  long           valid1 = whether the input file is a valid file (see below).
!  long           valid2 = whether the output file is a valid file (see below).
!  const char*     info1 = string to tack onto msg1 if status1 = FILE_VALID.
!  const char*     info2 = string to tack onto msg2 if status2 = FILE_VALID.
!  long    same_datasets = TRUE if the files appear to refer to the
!                            same dataset (needed only if status1 and
!                            status2 are both FILE_VALID).
!  long          status1 = the status of the input file (see below).
!  long          status2 = the status of the output file (see below).
!  long           status = the status of the two files together (see below).
!  char*            msg1 = message referring to the status of the input file.
!  char*            msg2 = message referring to the status of the output file.
!  char*            msg3 = message referring to both files.
!
!  MSG1, MSG2, and MSG3 cannot be null.
!
!  valid1 or 2         description
!  -----------         -----------
!  INQUIRE_VALID_YES   file is known to be a valid file of appropriate type.
!  INQUIRE_VALID_NO    file is known NOT to be a valid file of appropriate type.
!  INQUIRE_VALID_MAYBE do not know and do not care.
!
!  status1        description
!  -------        -----------
!  FILE_BLANK     filename1 is blank (only if required1 = FALSE).
!  FILE_VALID     input file is readable (only if INQUIRE_VALID_YES or MAYBE).
!  FILE_ERROR     there is an input file error (any other situation).
!
!  status2        description
!  -------        -----------
!  FILE_BLANK     filename2 is blank (only if required2 = FALSE).
!  FILE_NOT_FOUND output file not found but can be created.
!  FILE_VALID     output file is readable/writeable (only if INQUIRE_VALID_YES).
!  FILE_ERROR     there is an output file error (any other situation).
!
!  status         description
!  ------         -----------
!  FILE_CREATE    output file will be created from scratch (no input file).
!  FILE_READ_ONLY input file will be read-only (no output file).
!  FILE_COPY      input file will be copied to output file.
!  FILE_UPDATE    input file will be updated (input file = output file).
!  FILE_ERROR     there is an error (any other situation).
!
!  FILE_READ_ONLY will never be returned if required2 = TRUE.
!  FILE_CREATE    will never be returned if required1 = TRUE.
!  Note that a pre-existing output file can be overwritten or updated only if
!    valid2 = INQUIRE_VALID_YES (i.e. the pre-existing output file is known
!    to be of the appropriate type), and if both files appear to refer
!    to the same dataset.
!  The filetype argument is needed for msg1 if valid1 = INQUIRE_VALID_YES or
!    INQUIRE_VALID_NO.
!  The filetype argument is needed for msg2 if valid2 = INQUIRE_VALID_YES or
!    INQUIRE_VALID_NO.
!  The info1 argument is needed for msg1 if valid1 = INQUIRE_VALID_YES and the
!    returned input status is FILE_VALID.
!  The info2 argument is needed for msg2 if valid2 = INQUIRE_VALID_YES and the
!    returned output status = FILE_VALID.
!  The info1 and/or info2 arguments can be empty strings, or NULL.
!  The msg1 and msg2 arguments should be dimensioned at least [200].
!  The msg3          argument  should be dimensioned at least [300].
!  The msg1 and msg2 arguments will each contain two lines of description
!    (with one \n).
!  The second line of msg1 (after the \n) will contain the info1 argument if
!    valid1 = INQUIRE_VALID_YES and status1 = FILE_VALID, and will be empty
!    otherwise (or if info1 is NULL or empty).  Similarly for msg2.
!  The msg3 argument will contain three lines of description (with
!     two occurrences of \n), although the second or third line might
!     be empty.
!  The above constants are defined in the inquire.h header file.
!  This routine calls inquire_input_file, inquire_output_file, and
!    inquire_files (or inquire_files_alternate).
!
!  The second function is to be used if an alternate set of messages
!     is desired.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                              REVISION HISTORY      
!
!     Date        Author     Description
!     ----        ------     -----------
! 12. 2004-08-23  Stoeckley  Added inquire_file_quickly, inquire_input_quickly,
!                             and inquire_output_quickly.
! 11. 2003-07-10  Stoeckley  Added inquire_fetch_brief_message and
!                             inquire_fetch_message.
! 10. 2003-05-19  C C Burch  Changed test on file existence and not creatable.
!  9. 2002-01-02  Stoeckley  Change test as to whether a nonexistent file is
!                             creatable so as to protect an existing file
!                             which appears not to exist due to NFS problems.
!  8. 2000-10-19  Stoeckley  Change VALID_YES, VALID_NO, and VALID_MAYBE to
!                             INQUIRE_VALID_YES, INQUIRE_VALID_NO, and
!                             INQUIRE_VALID_MAYBE to resolve a conflict in
!                             ~spws.
!  7. 2000-10-06  Stoeckley  Add MSG argument to inquire_file, inquire_input,
!                              and inquire_output.
!  6. 1999-11-17  Stoeckley  Add ident string for RCS.
!  5. 1999-10-20  Stoeckley  Add more choices for blank filename, and add
!                              functions inquire_input and inquire_output,
!                              and change some char* arguments to const char*.
!  4. 1999-09-10  Stoeckley  Add reference to other files in this primitive.
!  3. 1999-08-31  Stoeckley  Converted from old system.
!                              Name changed from inquire_file to inquire.
!  2. 1995/09/04  Stoeckley  Add functions inquire_files_alternate and
!                            inquire_files_combo_alternate.
!  1. 1993/08/31  Stoeckley  Initial version.
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


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


#include "inquire.h"
#include "named_constants.h"
#include "str.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>


char INQUIRE_IDENT[100] = "$Id: inquire.c,v 1.12 2004/08/23 13:15:54 Stoeckley prod sps $";


/*---------------------- inquire file static quickly ----------------------*/
/*---------------------- inquire file static quickly ----------------------*/
/*---------------------- inquire file static quickly ----------------------*/


static long inquire_file_static_quickly (const char *filename) {
  int    exists;
  struct stat buf;

  if(filename                     == NULL) return FILE_BLANK;
  if(str_trimmed_length(filename) ==    0) return FILE_BLANK;
  if(strcmp(filename, "NONE")     ==    0) return FILE_BLANK;
  if(strcmp(filename, "none")     ==    0) return FILE_BLANK;
  if(strcmp(filename, "None")     ==    0) return FILE_BLANK;
  
  exists = lstat(filename,&buf);

  if(exists != 0) return FILE_NOT_FOUND;
  return FILE_FOUND;
}


/*---------------------- inquire file static -----------------------------*/
/*---------------------- inquire file static -----------------------------*/
/*---------------------- inquire file static -----------------------------*/

/* this logic rewritten March 2003 by Charles c Burch */

static long inquire_file_static (const char *filename) {
  FILE   *stream;
  int    exists, kount;
  char   junkname[450], hostname[64];
  long   status;

  if(filename                     == NULL) return FILE_BLANK;
  if(str_trimmed_length(filename) ==    0) return FILE_BLANK;
  if(strcmp(filename, "NONE")     ==    0) return FILE_BLANK;
  if(strcmp(filename, "none")     ==    0) return FILE_BLANK;
  if(strcmp(filename, "None")     ==    0) return FILE_BLANK;
  
  exists=access(filename, F_OK);
  if(exists<0) {
    /*file not exists, see if can create a file and also force disk remount*/
    /*add _jinq_host_pid to filename to form junkname*/
    if(gethostname(hostname,sizeof(hostname))<0) strcpy(hostname,"z-9+q");
    sprintf(junkname,"%s_jinq_%s_%ld",filename,hostname,(long)getpid());

    for (kount = 0; kount < 10; kount++) {
      stream = fopen(junkname, "w+");
      if(stream != NULL) {
        fclose(stream);  /*able to open new file*/
        remove(junkname);
        break;
      }        
    }
    
    if(kount>0) {
      if(kount==10) {
        /*never able to create file*/
        fprintf(stderr,
          "Warning:%s-Unable to create file(%s)[%d:%s]\n",
         "inquire_file_static",filename,errno,strerror(errno));
        return(FILE_NOT_CREATEABLE);  
      }        
      fprintf(stderr,
        "Warning:%s-multiple attempts(%d) required to create file(%s)[%d:%s]\n",
        "inquire_file_static",kount+1,filename,errno,strerror(errno));
    }
    exists=access(filename, F_OK); /*try again to see if exists*/
  }
  
  if(exists<0) return(FILE_NOT_FOUND);

  /***** file exists, now check readable/writeable                       *****/ 
  if(access(filename, R_OK)==0) {                  /*see if readable         */
    if(access(filename, W_OK)==0) {
      status=FILE_FOUND;                            /*readable and writeable */
    } else {
      status=FILE_NOT_WRITEABLE;                    /*readable, not writable */
    }
  } else {                                          /*not readable           */
    if(access(filename, W_OK)==0) {
      status=FILE_NOT_READABLE;                     /*writeable, not readable*/ 
    } else {  
      status=FILE_NOT_READ_WRITE;                   /*not writeable/readable */
    }
  }

  return(status);
}


/*------------------- fetch brief or long message -------------------------*/
/*------------------- fetch brief or long message -------------------------*/
/*------------------- fetch brief or long message -------------------------*/


static char lng[81];
static char brf[81];
static int  starting = TRUE;

const char *inquire_fetch_message()
{
  if(starting)
      {
      strcpy(lng, "no file inquiry has yet been made");
      starting = FALSE;
      }
  return lng;
}


const char *inquire_fetch_brief_message()
{
  if(starting)
      {
      strcpy(brf, "");
      starting = FALSE;
      }
  return brf;
}



/*----------------- inquire about a file quickly -------------------------*/
/*----------------- inquire about a file quickly -------------------------*/
/*----------------- inquire about a file quickly -------------------------*/


long inquire_file_quickly (const char *filename, char *msg)
{
  long status = inquire_file_static_quickly(filename);

  switch(status)
      {
      case FILE_BLANK          :
            strcpy(lng, "no file name specified");
            strcpy(brf, "");
            break;
      case FILE_FOUND          :
            strcpy(lng, "file (or link) exists");
            strcpy(brf, "found");
            break;
      case FILE_NOT_FOUND      :
            strcpy(lng, "file does not exist");
            strcpy(brf, "not found");
            break;
      default                  :
            strcpy(lng, "asdfgh whoops asdfgh");
            strcpy(brf, "whoops asdfgh");
            break;
      }

  starting = FALSE;
  if(msg) strcpy(msg,lng);
  return status;
}


long inquire_input_quickly (const char *filename, char *msg)
{
  long status = inquire_file_static_quickly(filename);

  switch(status)
      {
      case FILE_BLANK          :
            strcpy(lng, "no input file specified");
            strcpy(brf, " ");
            break;
      case FILE_FOUND          :
            strcpy(lng, "input file (or link) found");
            strcpy(brf, "found");
            break;
      case FILE_NOT_FOUND      :
            strcpy(lng, "input file not found");
            strcpy(brf, "not found");
            break;
      default                  :
            strcpy(lng, "asdfgh whoops asdfgh");
            strcpy(brf, "whoops asdfgh");
            break;
      }

  switch(status)
      {
      case FILE_BLANK          : status = FILE_ERROR; break;
      case FILE_FOUND          : status = FILE_FOUND; break;
      case FILE_NOT_FOUND      : status = FILE_ERROR; break;
      default                  : status = FILE_ERROR; break;
      }

  starting = FALSE;
  if(msg) strcpy(msg,lng);
  return status;
}


long inquire_output_quickly (const char *filename, char *msg)
{
  long status = inquire_file_static_quickly(filename);

  switch(status)
      {
      case FILE_BLANK          :
            strcpy(lng, "no output file specified");
            strcpy(brf, " ");
            break;
      case FILE_FOUND          :
            strcpy(lng, "output file (or link) already exists");
            strcpy(brf, "will overwrite");
            break;
      case FILE_NOT_FOUND      :
            strcpy(lng, "new output file will be created");
            strcpy(brf, "will create");
            break;
      default                  :
            strcpy(lng, "output asdfgh whoops asdfgh");
            strcpy(brf, "whoops asdfgh");
            break;
      }

  switch(status)
      {
      case FILE_BLANK          : status = FILE_ERROR    ; break;
      case FILE_FOUND          : status = FILE_FOUND    ; break;
      case FILE_NOT_FOUND      : status = FILE_NOT_FOUND; break;
      default                  : status = FILE_ERROR    ; break;
      }

  starting = FALSE;
  if(msg) strcpy(msg,lng);
  return status;
}



/*----------------- inquire about a file ---------------------------------*/
/*----------------- inquire about a file ---------------------------------*/
/*----------------- inquire about a file ---------------------------------*/


long inquire_file (const char *filename, char *msg)
{
  long status = inquire_file_static(filename);

  switch(status)
      {
      case FILE_BLANK          :
            strcpy(lng, "no file name specified");
            strcpy(brf, "");
            break;
      case FILE_FOUND          :
            strcpy(lng, "file exists and is readable and writeable");
            strcpy(brf, "found");
            break;
      case FILE_NOT_READABLE   :
            strcpy(lng, "file exists but is not readable (but is writeable)");
            strcpy(brf, "not readable");
            break;
      case FILE_NOT_WRITEABLE  :
            strcpy(lng, "file exists but is not writeable (but is readable)");
            strcpy(brf, "not writeable");
            break;
      case FILE_NOT_READ_WRITE :
            strcpy(lng, "file exists but is not readable or writeable");
            strcpy(brf, "not read/write");
            break;
      case FILE_NOT_CREATEABLE :
            strcpy(lng, "file does not exist and is not writeable");
            strcpy(brf, "not creatable");
            break;
      case FILE_NOT_FOUND      :
            strcpy(lng, "file does not exist but is writeable");
            strcpy(brf, "not found");
            break;
      default                  :
            strcpy(lng, "asdfgh whoops asdfgh");
            strcpy(brf, "whoops asdfgh");
            break;
      }

  starting = FALSE;
  if(msg) strcpy(msg,lng);
  return status;
}


long inquire_input (const char *filename, char *msg)
{
  long status = inquire_file_static(filename);

  switch(status)
      {
      case FILE_BLANK          :
            strcpy(lng, "no input file specified");
            strcpy(brf, " ");
            break;
      case FILE_FOUND          :
            strcpy(lng, "input file found");
            strcpy(brf, "found");
            break;
      case FILE_NOT_READABLE   :
            strcpy(lng, "input file exists but is not readable");
            strcpy(brf, "not readable");
            break;
      case FILE_NOT_WRITEABLE  :
            strcpy(lng, "input file found");
            strcpy(brf, "found");
            break;
      case FILE_NOT_READ_WRITE :
            strcpy(lng, "input file exists but is not readable");
            strcpy(brf, "not readable");
            break;
      case FILE_NOT_CREATEABLE :
            strcpy(lng, "input file not found");
            strcpy(brf, "not found");
            break;
      case FILE_NOT_FOUND      :
            strcpy(lng, "input file not found");
            strcpy(brf, "not found");
            break;
      default                  :
            strcpy(lng, "asdfgh whoops asdfgh");
            strcpy(brf, "whoops asdfgh");
            break;
      }

  switch(status)
      {
      case FILE_BLANK          : status = FILE_ERROR; break;
      case FILE_FOUND          : status = FILE_FOUND; break;
      case FILE_NOT_READABLE   : status = FILE_ERROR; break;
      case FILE_NOT_WRITEABLE  : status = FILE_FOUND; break;
      case FILE_NOT_READ_WRITE : status = FILE_ERROR; break;
      case FILE_NOT_CREATEABLE : status = FILE_ERROR; break;
      case FILE_NOT_FOUND      : status = FILE_ERROR; break;
      default                  : status = FILE_ERROR; break;
      }

  starting = FALSE;
  if(msg) strcpy(msg,lng);
  return status;
}


long inquire_output (const char *filename, char *msg)
{
  long status = inquire_file_static(filename);

  switch(status)
      {
      case FILE_BLANK          :
            strcpy(lng, "no output file specified");
            strcpy(brf, " ");
            break;
      case FILE_FOUND          :
            strcpy(lng, "output file already exists and will be overwritten");
            strcpy(brf, "will overwrite");
            break;
      case FILE_NOT_READABLE   :
            strcpy(lng, "output file already exists and will be overwritten");
            strcpy(brf, "will overwrite");
            break;
      case FILE_NOT_WRITEABLE  :
            strcpy(lng, "output file exists but is not writeable");
            strcpy(brf, "not writeable");
            break;
      case FILE_NOT_READ_WRITE :
            strcpy(lng, "output file exists but is not writeable");
            strcpy(brf, "not writeable");
            break;
      case FILE_NOT_CREATEABLE :
            strcpy(lng, "output file does not exist and is not writeable");
            strcpy(brf, "not creatable");
            break;
      case FILE_NOT_FOUND      :
            strcpy(lng, "new output file will be created");
            strcpy(brf, "will create");
            break;
      default                  :
            strcpy(lng, "output asdfgh whoops asdfgh");
            strcpy(brf, "whoops asdfgh");
            break;
      }

  switch(status)
      {
      case FILE_BLANK          : status = FILE_ERROR    ; break;
      case FILE_FOUND          : status = FILE_FOUND    ; break;
      case FILE_NOT_READABLE   : status = FILE_FOUND    ; break;
      case FILE_NOT_WRITEABLE  : status = FILE_ERROR    ; break;
      case FILE_NOT_READ_WRITE : status = FILE_ERROR    ; break;
      case FILE_NOT_CREATEABLE : status = FILE_ERROR    ; break;
      case FILE_NOT_FOUND      : status = FILE_NOT_FOUND; break;
      default                  : status = FILE_ERROR    ; break;
      }

  starting = FALSE;
  if(msg) strcpy(msg,lng);
  return status;
}



/*------------- inquire about an input file ---------------------------*/
/*------------- inquire about an input file ---------------------------*/
/*------------- inquire about an input file ---------------------------*/


long inquire_input_file (const char *filename, long required, long valid,
                         const char *filetype, const char *info, char *msg)
{
  long status = inquire_file_static(filename);

  if(status == FILE_NOT_WRITEABLE )  status = FILE_FOUND;
  if(status == FILE_NOT_READ_WRITE)  status = FILE_NOT_READABLE;
  if(status == FILE_NOT_CREATEABLE)  status = FILE_NOT_FOUND;
  if(status == FILE_BLANK && required)
       {
       strcpy(msg, "Input file name must be specified.\n");
       status = FILE_ERROR;
       }
  else if(status == FILE_BLANK)       /* && !required */
       {
       strcpy(msg, "No input file specified.\n");
       }
  else if(status == FILE_NOT_FOUND)
       {
       strcpy(msg, "Input file not found.\n");
       status = FILE_ERROR;
       }
  else if(status == FILE_NOT_READABLE)
       {
       strcpy(msg, "Input file not readable.\n");
       status = FILE_ERROR;
       }
  else if(status == FILE_FOUND && valid == INQUIRE_VALID_NO)
       {
       sprintf(msg, "The input file is not a valid %s.\n", filetype);
       status = FILE_ERROR;
       }
  else if(status == FILE_FOUND && valid == INQUIRE_VALID_YES)
       {
       sprintf(msg, "Input %s found.\n", filetype);
       if(info && info[0]) strcat(msg, info);
       status = FILE_VALID;
       }
  else if(status == FILE_FOUND && valid == INQUIRE_VALID_MAYBE)
       {
       strcpy(msg, "Input file found.\n");
       status = FILE_VALID;
       }
  else
       {
       strcpy(msg, "Illegal value of 'status' or 'valid'.\n");
       status = FILE_ERROR;
       }
  return status;
}


/*------------- inquire about an output file --------------------------*/
/*------------- inquire about an output file --------------------------*/
/*------------- inquire about an output file --------------------------*/


long inquire_output_file (const char *filename, long required, long valid,
                          const char *filetype, const char *info, char *msg)
{
  long status = inquire_file_static(filename);

  if(status == FILE_BLANK && required)
       {
       strcpy(msg, "Output file name must be specified.\n");
       status = FILE_ERROR;
       }
  else if(status == FILE_BLANK)       /* && !required */
       {
       strcpy(msg, "No output file specified.\n");
       }
  else if(status == FILE_NOT_FOUND)
       {
       strcpy(msg, "Output file does not yet exist.\n");
       }
  else if(status == FILE_NOT_READABLE)
       {
       strcpy(msg, "Output file is not readable.\n");
       status = FILE_ERROR;
       }
  else if(status == FILE_NOT_WRITEABLE)
       {
       strcpy(msg, "Output file is not writeable.\n");
       status = FILE_ERROR;
       }
  else if(status == FILE_NOT_CREATEABLE)
       {
       strcpy(msg, "Output file cannot be created.\n");
       status = FILE_ERROR;
       }
  else if(status == FILE_NOT_READ_WRITE)
       {
       strcpy(msg, "Output file is not readable or writeable.\n");
       status = FILE_ERROR;
       }
  else if(status == FILE_FOUND && valid == INQUIRE_VALID_NO)
       {
       if(filetype[0] == 'S')
          sprintf(msg, "Existing output file is NOT an %s.\n", filetype);
       else
          sprintf(msg, "Existing output file is NOT a %s.\n", filetype);
       status = FILE_ERROR;
       }
  else if(status == FILE_FOUND && valid == INQUIRE_VALID_YES)
       {
       sprintf(msg, "Output %s already exists.\n", filetype);
       if(info && info[0]) strcat(msg, info);
       status = FILE_VALID;
       }
  else if(status == FILE_FOUND && valid == INQUIRE_VALID_MAYBE)
       {
       strcpy(msg, "Output file of unknown type already exists.\n");
       status = FILE_ERROR;
       }
  else
       {
       strcpy(msg, "Illegal value of 'status' or 'valid'.\n");
       status = FILE_ERROR;
       }
  return status;
}


/*-------- inquire about a pair of input/output files -----------------*/
/*-------- inquire about a pair of input/output files -----------------*/
/*-------- inquire about a pair of input/output files -----------------*/


long inquire_files (const char *filename1, const char *filename2,
                    long status1, long status2, long same_datasets,
                    char *msg1, char *msg2, char *msg3)
{
  long status;

  if(status1 == FILE_ERROR && status2 == FILE_ERROR)
       {
       sprintf(msg3, "%s%s", msg1, msg2);
       status = FILE_ERROR;
       }
  else if(status1 == FILE_ERROR)
       {
       sprintf(msg3, "%s\n", msg1);
       status = FILE_ERROR;
       }
  else if(status2 == FILE_ERROR)
       {
       sprintf(msg3, "%s\n", msg2);
       status = FILE_ERROR;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_BLANK)
       {
       strcpy(msg3, "An input and/or output file must be specified.\n\n");
       status = FILE_ERROR;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_NOT_FOUND)
       {
       strcpy(msg3, "Output file will be created from scratch.\n\n");
       status = FILE_CREATE;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_VALID)
       {
       strcpy(msg3, "We do not want to overwrite the output file,\n");
       strcat(msg3, "since we do not know whether it is from this data set.\n");
       status = FILE_ERROR;
       }
  else if(status1 == FILE_VALID && status2 == FILE_BLANK)
       {
       strcpy(msg3, "Input file will be read-only.\n\n");
       status = FILE_READ_ONLY;
       }
  else if(status1 == FILE_VALID && status2 == FILE_NOT_FOUND)
       {
       strcpy(msg3, "The input file will be copied to the output file,\n");
       strcat(msg3, "and then you will be updating the output file.\n");
       status = FILE_COPY;
       }
  else if(status1 != FILE_VALID || status2 != FILE_VALID)
       {
       strcpy(msg3, "Illegal value of status1 or status2.\n\n");
       status = FILE_ERROR;
       }
  else if(!strcmp(filename1, filename2))           /* names are same */
       {
       strcpy(msg3, "Input and output files are the same file.\n");
       strcat(msg3, "The file will be updated.\n");
       status = FILE_UPDATE;
       }
  else if(same_datasets)
       {
       strcpy(msg3, "THE OUTPUT FILE WILL BE OVERWRITTEN.\n");
       strcat(msg3, "The input file will be copied to the output file,\n");
       strcat(msg3, "and then you will be updating the output file.");
       status = FILE_COPY;
       }
  else
       {
       strcpy(msg3, "The output file appears to be from a\n");
       strcat(msg3, "different data set from the input file.\n");
       strcat(msg3, "We do not want to overwrite it.");
       status = FILE_ERROR;
       }
  return status;
}


/*-------- inquire about a pair of input/output files -----------------*/
/*-------- inquire about a pair of input/output files -----------------*/
/*-------- inquire about a pair of input/output files -----------------*/


long inquire_files_alternate
                    (const char *filename1, const char *filename2,
                     long status1, long status2, long same_datasets,
                     char *msg1, char *msg2, char *msg3)
{
  long status;

  if(status1 == FILE_ERROR && status2 == FILE_ERROR)
       {
       sprintf(msg3, "%s%s", msg1, msg2);
       status = FILE_ERROR;
       }
  else if(status1 == FILE_ERROR)
       {
       sprintf(msg3, "%s\n", msg1);
       status = FILE_ERROR;
       }
  else if(status2 == FILE_ERROR)
       {
       sprintf(msg3, "%s\n", msg2);
       status = FILE_ERROR;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_BLANK)
       {
       strcpy(msg3, "An input and/or output file should be specified.\n\n");
       status = FILE_ERROR;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_NOT_FOUND)
       {
       strcpy(msg3, "A new output file will be created.\n\n");
       status = FILE_CREATE;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_VALID)
       {
       strcpy(msg3, "We do not want to overwrite the output file,\n");
       strcat(msg3, "since it might be a file you want.\n");
       strcat(msg3, "Please delete the file to re-use its name.");
       status = FILE_ERROR;
       }
  else if(status1 == FILE_VALID && status2 == FILE_BLANK)
       {
       strcpy(msg3, "No output file specified.\n\n");
       status = FILE_READ_ONLY;
       }
  else if(status1 == FILE_VALID && status2 == FILE_NOT_FOUND)
       {
       strcpy(msg3, "Input and output files are different.\n");
       strcat(msg3, "A new output file will be created.\n");
       status = FILE_COPY;
       }
  else if(status1 != FILE_VALID || status2 != FILE_VALID)
       {
       strcpy(msg3, "Illegal value of status1 or status2.\n\n");
       status = FILE_ERROR;
       }
  else if(!strcmp(filename1, filename2))           /* names are same */
       {
       strcpy(msg3, "Input and output files are the same file.\n");
       strcat(msg3, "The contents of the file will be replaced.\n");
       status = FILE_UPDATE;
       }
  else if(same_datasets)
       {
       strcpy(msg3, "Input and output files are different.\n");
       strcat(msg3, "THE OUTPUT FILE WILL BE OVERWRITTEN.\n");
       status = FILE_COPY;
       }
  else
       {
       strcpy(msg3, "We do not want to overwrite the output file,\n");
       strcat(msg3, "since it might be a file you want.\n");
       strcat(msg3, "Please delete the file to re-use its name.");
       status = FILE_ERROR;
       }
  return status;
}


/*-------- inquire about a pair of input/output files -----------------*/
/*-------- inquire about a pair of input/output files -----------------*/
/*-------- inquire about a pair of input/output files -----------------*/


long inquire_files_nonmatching
                      (const char *filename1, const char *filename2,
                       long status1, long status2, long same_datasets,
                       char *msg1, char *msg2, char *msg3)
{
  long status;

  if(status1 == FILE_ERROR && status2 == FILE_ERROR)
       {
       sprintf(msg3, "%s%s", msg1, msg2);
       status = FILE_ERROR;
       }
  else if(status1 == FILE_ERROR)
       {
       sprintf(msg3, "%s\n", msg1);
       status = FILE_ERROR;
       }
  else if(status2 == FILE_ERROR)
       {
       sprintf(msg3, "%s\n", msg2);
       status = FILE_ERROR;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_BLANK)
       {
       strcpy(msg3, "An input and/or output file must be specified.\n\n");
       status = FILE_ERROR;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_NOT_FOUND)
       {
       strcpy(msg3, "Output file will be created from scratch.\n\n");
       status = FILE_CREATE;
       }
  else if(status1 == FILE_BLANK && status2 == FILE_VALID)
       {
       strcpy(msg3, "We do not want to overwrite the output file,\n");
       strcat(msg3, "since we do not know whether it is from this data set.\n");
       status = FILE_ERROR;
       }
  else if(status1 == FILE_VALID && status2 == FILE_BLANK)
       {
       strcpy(msg3, "No output file will be created.\n\n");
       status = FILE_READ_ONLY;
       }
  else if(status1 == FILE_VALID && status2 == FILE_NOT_FOUND)
       {
       strcpy(msg3, "The output file will be created,\n");
       strcat(msg3, "using information from the input file.\n");
       status = FILE_COPY;
       }
  else if(status1 != FILE_VALID || status2 != FILE_VALID)
       {
       strcpy(msg3, "Illegal value of status1 or status2.\n\n");
       status = FILE_ERROR;
       }
  else if(!strcmp(filename1, filename2))           /* names are same */
       {
       strcpy(msg3, "Input and output files are the same file.\n");
       strcat(msg3, "The file will be updated.\n");
       status = FILE_UPDATE;
       }
  else if(same_datasets)
       {
       strcpy(msg3, "THE OUTPUT FILE WILL BE OVERWRITTEN.\n");
       strcat(msg3, "The output file will be replaced,\n");
       strcat(msg3, "using information from the input file.");
       status = FILE_COPY;
       }
  else
       {
       strcpy(msg3, "The output file appears to be from a\n");
       strcat(msg3, "different data set from the input file.\n");
       strcat(msg3, "We do not want to overwrite it.");
       status = FILE_ERROR;
       }
  return status;
}


/*------------------ inquire files combo routine ----------------------*/
/*------------------ inquire files combo routine ----------------------*/
/*------------------ inquire files combo routine ----------------------*/


long inquire_files_combo
              (const char *filename1, const char *filename2,
               const char *filetype, long required1, long required2,
               long valid1, long valid2, const char *info1, const char *info2,
               long same_datasets, long *status1, long *status2,
               char *msg1, char *msg2, char *msg3)
{
  long status;

  *status1 = inquire_input_file (filename1, required1, valid1,
                                                filetype, info1, msg1);
  *status2 = inquire_output_file(filename2, required2, valid2,
                                                filetype, info2, msg2);
  status   = inquire_files (filename1, filename2, *status1, *status2,
                    same_datasets, msg1, msg2, msg3);
  return status;
}


/*------------------ inquire files combo routine ----------------------*/
/*------------------ inquire files combo routine ----------------------*/
/*------------------ inquire files combo routine ----------------------*/


long inquire_files_combo_alternate
              (const char *filename1, const char *filename2,
               const char *filetype, long required1, long required2,
               long valid1, long valid2, const char *info1, const char *info2,
               long same_datasets, long *status1, long *status2,
               char *msg1, char *msg2, char *msg3)
{
  long status;

  *status1 = inquire_input_file (filename1, required1, valid1,
                                                filetype, info1, msg1);
  *status2 = inquire_output_file(filename2, required2, valid2,
                                                filetype, info2, msg2);
  status   = inquire_files_alternate(filename1, filename2, *status1, *status2,
                    same_datasets, msg1, msg2, msg3);
  return status;
}



/*------------------------------ end ---------------------------------------*/
/*------------------------------ end ---------------------------------------*/
/*------------------------------ end ---------------------------------------*/

/*-------------------------- test programs ---------------------------------*/

/****
#include "inquire.h"
#include <stdlib.h>
#include <stdio.h>

long inquire_file_static(char*);

int main() {
  char file[300];
  
  system("rm -f file0.inq");
  system("rm -f file1.inq");
  system("rm -f file2.inq");
  system("rm -f file3.inq");
  system("rm -f file4.inq");
  system("mkdir inq");
  system("rm -f inq/file5.inq");

  system("chmod a-w inq");
  system("touch file1.inq");
  system("touch file2.inq");
  system("touch file3.inq");
  system("touch file4.inq");

  system("chmod a-r file2.inq");
  system("chmod a-w file3.inq");
  system("chmod a-r file4.inq");
  system("chmod a-w file4.inq");
  system("chmod a-w inq");
  
  system("ls -al file*.inq");
  system("ls -al inq");
    
  printf(" FILE_BLANK=%d\n",FILE_BLANK);
  printf(" FILE_NOT_FOUND=%d\n",FILE_NOT_FOUND);
  printf(" FILE_NOT_CREATEABLE=%d\n",FILE_NOT_CREATEABLE);
  printf(" FILE_FOUND=%d\n",FILE_FOUND);
  printf(" FILE_NOT_READABLE=%d\n",FILE_NOT_READABLE);
  printf(" FILE_NOT_WRITEABLE=%d\n",FILE_NOT_WRITEABLE);
  printf(" FILE_NOT_READ_WRITE=%d\n",FILE_NOT_READ_WRITE);
  
  strcpy(file,"");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  strcpy(file,"file0.inq");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  strcpy(file,"file1.inq");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  strcpy(file,"file2.inq");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  strcpy(file,"file3.inq");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  strcpy(file,"file4.inq");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  strcpy(file,"inq");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  strcpy(file,"inq/file5.inq");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  strcpy(file,"inq/inq/file5.inq");
  printf("inq(%s)=%ld\n",file,inquire_file_static(file));

  system("rm -f file1.inq");
  system("rm -f file2.inq");
  system("rm -f file3.inq");
  system("rm -f file4.inq");
  system("rmdir inq");
  return(0);
 }
****/
