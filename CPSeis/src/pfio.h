/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- pfio.h ----------------------------------
!------------------------------- pfio.h ----------------------------------
!------------------------------- pfio.h ----------------------------------
!other files are: pfiocodes.h pfio.c
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
! Name       : PFIO.H
! Category   : io
! Written    : 1999-09-15   by: Charles C. Burch
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Provides an interface into various methods of "big" file i/o.
! References : These routines are called from within cio_crou.c
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
! 27. 2007-03-27  Kruger Corn     Upgraded to 64 bit architecture.
! 26. 2007-01-25  Bill Menger     Added pfio_max_nodename=40 to match lgc.
! 25. 2006-11-14  Bill Menger     Modified the remote shell calls, removed the
!                                 cleanup argument.
! 24. 2005-12-20  Burch           Removed skio related items 
! 23. 2005-11-29  Burch/Macy      Added pfio_add_desc_flags, pfio_get_filename,
!                                 pfio_get_fd, pfio_get_fileno
! 22. 2005-07-11  Bill Menger     Added project_name function prototypes.
!                                 Removed CPSWORK access.
! 21. 2004-01-21  Bill Menger     Moved definitions from pfio.c
! 20. 2003-05-28  C C Burch       Addded pfio_set_base_process
!                                 Made changes for extents >2Gb.
! 19. 2002-07-19  Chuck C. Burch  Changed pfio_lock/unlock parameters
!                                 Added pfio_set_cpsdisk_control
! 18. 2002-06-18  Chuck C. Burch  Added pfio_gets
! 17. 2002-05-20  Chuck C. Burch  Added pfio_set_lock_type
! 16. 2002-03-27  Chuck C. Burch  Added pfio_remote_command/remote_command_retry
! 15. 2001-12-21  Chuck C. Burch  Added pfio_get_extent_info,pfio_set_debug_mode
! 14. 2001-08-27  Chuck C. Burch  Added pfio_set_file_lock_control
!                                 Added pfio_get_file_info
!                                 Added pfio_set_file_auto_delete
! 13. 2001-05-07  Chuck C. Burch  Use c2f_interface.h
! 12. 2001-05-01  Chuck C. Burch  Added pfio_get_var and pfio_put_var
! 11. 2001-04-30  Chuck C. Burch  Added pfio_write_message_file,
!                                  pfio_checksum, pfio_get_file_node
!                                  and pfio_update_file_time
! 10. 2001-04-03  Chuck C. Burch  Added pfio_exit, pfio_get_file_num_lines
!                                 and pfio_set_trace_mode/pfio_trace_dump
!  9. 2001-03-26  Chuck C. Burch  Added pfio_dump_lock_file and
!                                 pfio_unlock_file
!  8. 2001-02-27  Chuck C. Burch  Added set_file_region_lock entry
!  7. 2001-02-19  Chuck C. Burch  Added new write_error_recover entry
!  6  2001-01-24  Chuck C. Burch  Added reserve_file_space and
!                                 Alphabetize lists of functions
!  5. 2000-12-07  Chuck C. Burch  Added new pfio functions.
!  4. 2000-10-20  Chuck C. Burch  restructured code.
!  3. 2000-08-31  Chuck C. Burch  Modified for robust socket support.
!  2. 2000-05-11  Bill Menger     Modified CPS documentation
!  1. 2000-04-05  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _PFIO_H_
#define _PFIO_H_

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define pfio_exit   PFIO_EXIT
#endif

#ifdef NEED_UNDERSCORE
#define pfio_exit   pfio_exit_
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*char *pfio_h_ident =*/
/*"$Id: pfio.h,v 1.27 2007/03/28 15:09:44 Corn beta sps $";*/

/**************************** Defined Variables ****************************/
#define DEFAULT_EXTENT_SIZE 256000000 /*default file size of file extents  */
#define pfio_num_cpsdisks   2         /*number of cpsdisk types */
#define pfio_cpsdata_num    0         /*key for cpsdata disks   */
#define pfio_cpstemp_num    1         /*key for cpstemp disks   */
#define pfio_cpswork_num    2         /*key for cpswork disks   */
#define PFIO_MAX_NODENAME   40        /*max node name length */


/*************************** pfio global variables *************************/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/****************************** pfio.h ********************
*      Include file for pfio  routines                    *
*                                                         *
*      Written June 1999 by Charles C Burch               *
**********************************************************/
#include <time.h>
#include <stdio.h>

#include "upgrade264.h"

/*        function prototypes for pfio  routines    */
int       pfio_add_desc_flags(int ifile, int32_t flags);
void      pfio_checksum(INTEGER*, INTEGER*, INTEGER*);
int       pfio_chmod(char*, int);
int       pfio_close(int);
void      pfio_completion_bar(int64_t, int64_t, int64_t*);
int       pfio_delete(char*);
int       pfio_delete_empty_directories(char*, int);
int       pfio_disable_read_ahead(int);
int       pfio_dump_lock_file();
int       pfio_enable_read_ahead(int);
int       pfio_ensure_dir_exists(char*, int);
void      pfio_exit();
int       pfio_expand_file_name(char*, char*, int);
int64_t   pfio_ext_flsz(char *fn);
int32_t   pfio_fetch(int, int64_t, int32_t);
int64_t   pfio_file_size(char*);
void      pfio_form_fn_ext(char*, int, char*);
int64_t   pfio_flsz(char*);
int       pfio_flush(int);
int64_t   pfio_get_current_file_size(int);
int64_t   pfio_get_extent_info(char*, int32_t*, int64_t*);
int64_t   pfio_get_ext_size(int);
int       pfio_get_file_info(char*, char*, char*);
char*     pfio_get_filename(int);
FILE*     pfio_get_fd(int, int);
int       pfio_get_fileno(int, int);
void      pfio_get_file_node(char*, char*);
int       pfio_get_file_num_lines(char*);
time_t    pfio_get_file_time(char*);
int       pfio_get_number_of_projects();
char*     pfio_get_project_name();
char**    pfio_get_valid_project_names();
void      pfio_get_var(char*, char*);
int32_t   pfio_gets(int, char*, int32_t);
int       pfio_lock_file(char*, char*, int32_t);
void      pfio_log_writer(char*, int, char*); /*only called by c*/
int       pfio_open(char*, char);
void      pfio_print_file_stats(int);
void      pfio_put_var(char*, char*);
int32_t   pfio_read(int, char*, int32_t);
int       pfio_remote_command(char*, char*);
int       pfio_remote_command_retry(char*, char*, int, int);
int       pfio_rename_file(char*, char*);
int       pfio_repair_lock_file(char*, int);
int       pfio_reserve_file_space(char*, int64_t*);
int       pfio_rwx_to_chmod(char*);
int       pfio_seek(int,int64_t);
int       pfio_seek_via_origin(int, int64_t, int);
int       pfio_setbufsz(int, int32_t);
void      pfio_set_base_process(char*, int);
void      pfio_set_cpsdisk_control(int);
void      pfio_set_debug_mode(int);
int       pfio_set_ext_size(int64_t);
void      pfio_set_file_auto_delete(int);
void      pfio_set_file_lock_control(int);
int       pfio_set_file_region_locking(int, int);
void      pfio_set_file_space_commit(int);
void      pfio_set_lock_type(char);
void      pfio_set_project_name(char*);
void      pfio_set_remote_access(int);
int       pfio_set_trace_mode(int);
int       pfio_set_write_error_recovery(int, int);
int       pfio_str_compress(char*, char*);
int64_t   pfio_tell(int);
int       pfio_trace_dump(char*, int64_t, int64_t);
char      pfio_try_locking_file(char*, char*, int32_t, char);
int       pfio_truncate(char*, int64_t);
int       pfio_unlock_file(char*, char*);
void      pfio_update_file_time(char *);
int32_t   pfio_write(int, char*, int32_t);
void      pfio_write_file_to_logfile(char*);
void      pfio_write_message_file(char*, char*);

/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/


#ifdef __cplusplus
}
#endif

#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/



