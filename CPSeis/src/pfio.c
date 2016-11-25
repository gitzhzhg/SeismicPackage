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
/*<CPS_v1 type="PRIMITIVE"/>
!----------------------------- pfio.c -------------------------------
!----------------------------- pfio.c -------------------------------
!----------------------------- pfio.c -------------------------------
!            other files are:  pfio.h bfio.h

!<brief_doc>
!-------------------------------------------------------------------------------
! Name       : pfio
! Category   : io
! Written    : 1999-09-15   by: Charles C. Burch
! Revised    : 2009-01-27   by: Bill Menger
! Maturity   : beta
! Purpose    : Provides an interface into various methods of "big" file i/o.
! References : These routines are called from within cio_crou.c
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!****************************** PFIO routines *******************
! Support routines for pfio (large files and socket remote io)
!
!  pfio_chmod(char *filename, int mode)
!    chmod a BF file filename to the value mode
!
!  pfio_close(int ifile)
!    closes a file opened with pfio_open
!    ifile=file number associated with file using pfio_open
!    returns remaining times file opened by others, <0: error
!
!  pfio_delete(char *flnm)
!    deletes file flnm (along with any extensions, links, locks
!
!  pfio_disable_read_ahead(int ifile)
!    deactivates read_ahead for file ifile
!
!  pfio_enable_read_ahead(int ifile)
!    activates read_ahead for file ifile
!
!  pfio_fetch(int ifile, int64_t offset, int32_t nbytes)
!    fetches data into cache
!    ifile-file number of file
!    offset-offset where to start read
!    nbytes = number of bytes to read
!
!  int64_t pfio_get_extent_info(char *fn, int32_t *n_exts, int32_t *extsz)
!    get number extents and max extent size of file fn.
!    returns file size of fn
!
!  int pfio_get_file_info(char *file_in, char *file_out, char *node)
!    get final file name and node associated with a file name that
!    might be a link
!    return(0) on success and -1 on error
!
!  pfio_flsz(char *flnm)
!    returns file size of file flnm and any extensions
!
!  pfio_flush(int ifile)
!    flush data buffers of file ifile
!
!  pfio_get_current_file_size(int ifile, int64_t size)
!    get internal pfio setting so working file size is <=size
!
!  pfio_get_ext_size(int ifile)  (return int32_t)
!    get extent size to pass out to fortran
!    Original version written by Bill Menger on Dec 03, 2000
!
!  pfio_get_file_time(char *filename)
!    returns time stamp of specified file
!
!  pfio_get_project_name()
!    returns (char *) project name. (see pfio_set_project_name)
!    project name can also be set with CPS_PROJECT_NAME environment var.
!
!  pfio_get_valid_project_names()
!    returns pointer to array of character strings, each is a valid project
!    name (read from cpsdata_nodes.dat file). (char **)
!
!  pfio_get_number_of_projects()
!    returns (int) number of valid projects.  One project can be the blank
!    or NULL project.
!
!  pfio_get_var(char *var, char *value)
!    extracts first occurrence of value of var in var_list
!    and remove this occurrence from var_list
!    Essentially this in the get of a first in-first out queue-
!      see pfio_put_var
!
!  pfio_lock_file(char *lock_file, char *flnm, int32_t seconds)
!    creates a lock on the file flnm for "seconds" seconds
!
!  pfio_open(char *fn, char mode)
!    opens file using pfio system
!    fn=file name to open, mode=R(read) W(write) U(update)
!    returns file number associated with open, <=0 error
!
!  pfio_print_file_stats(int ifile)
!    prints stats for file ifile
!
!  pfio_put_var(char *var, char *value)
!    Insert var and value into var_list
!    Essentially this in the put of a first in-first out queue-
!      see pfio_get_var
!
!  pfio_read(int ifile, char *buff, int32_t nbytes)
!    reads information from file
!    ifile=file number of file
!    buff=buffer to read to into
!    nbytes=number bytes to read  (at current offset-see pfio_seek)
!    returns number bytes actual read (as int32_t), 0=EOF, <0 error
!
!  pfio_remote_command(char *node, char *cmd)
!   issues command(cmd) on node(node)
!   return 0 if no error and -1 if error
!
!  pfio_rename_file(char *old_name char *new_name)
!     renames old name to new name
!
!  pfio_seek_via_origin(int ifile, int64_t offset, int origin)
!    seeks to position in ifile using offset from origin
!   where origin ae the same as used with C rtn fseek
!
!  pfio_seek(int ifile, int64_t offset)
!    seeks to position in ifile using offset from start of file
!
!  pfio_setbufsz(int ifile, int64_t bufsz)
!    set buffer size(bufsz)  for file ifile (bufsz<0 turns off buffering)
!
!  pfio_set_ext_size(int64_t size)
!    Sets size(in bytes) of extents for new open, if it is a new file
!     on file opened update-it sets extent size if the file has no
!      extents yet and the file size is < requested extent size
!
!  pfio_set_file_auto_delete(int isw)
!    set file auto delete for next file opened
!    isw=0 means to do not delete file (default for non-cpstemp files)
!       =1 means to delete on program exit(default for cpstemp files)
!       =2 means to delete on file closing
!
!  pfio_set_file_lock control(int isw)
!    only applied to next file opened and that file is not read-only
!    set file lock control for next file opened
!    isw=0 means do not lock file, =1 means lock as needed
!
!  pfio_set_project_name(char *)
!    return void, put the project name in the char* string.  Also you can
!    set the project name by using CPS_PROJECT_NAME=projectname (bash, sh) OR
!    setenv CPS_PROJECT_NAME projectname (tcsh, csh).
!
!  pfio_set_region_file_locking(int ifile, int mode)
!     set file region locks on or off while doing reads and writes
!     mode:1=yes, 0=no
!
!  pfio_set_file_space_commit(int isw)
!    only applied to next file opened and that file is write
!    if (isw=-1) do not commit file space for next file
!    if(isw=1) commit file space for next file
!    If(isw=0 default) commit file space for cpstemp/cpsdata
!                      do not commit file sace for other files
!     any new file extends preallocated to max file size  will
!     be truncated to spaced actually used when closed
!
!    if isw!=0 the next file opened if opened for write will have
!     any new file extends preallocated to max file size that will
!     be truncated to spaced actually used when closed
!
!  pfio_set_remote_access(int isw)
!    enables(isw=1) or disables(isw=0) use of sockets
!    to do file io on system where file exits
!
! pfio_set_trace_mode(int mode)
!   set trace mode: 1=start tracing,
!                -1 stop tracing-purge traced info
!                 0 stop tracing-print/purge traced info
!
!  pfio_tell(int ifile)
!    returns current offset from start of file
!
!  int pfio_trace_dump(int ifile, char rw, int64_t beg, int64_t end)
!    dump the areas accessed with the trace information
!    ifile is file number rw=r or w, beg/end area to dump
!
!  pfio_unlock_file(char *lock_file, char *flnm)
!     frees a lock on the file flnm
!
!  pfio_write(int ifile, char *buff, int64_t nbytes)
!    writes data to file
!    ifile=file number to write to
!    buff=buffer to get data from to write
!    nbytes=number bytes to write (at current offset-see pfio_seek)
!
!  pfio_write_file_to_logfile(char *filename)
!   write contents of filename to CPS logfile and deletes filename
!
!  pfio_write_message_file(char *file, char *message)
!    write message to file
!
!  pfio_set_write_error_recovery(int ifile, int imode) {
!    set write error recovery mode for file:ifile (1=yes, 0=no)
!    default is yes
!
! The following are general-purpose routine, not specific to pfio:
!
!  pfio_checksum(INTEGER *a, INTEGER *n, INTEGER *chksm)
!    calculates checksum of n elements of a place results into chksm
!    this is meant primarily for diagnostic usage
!
!  pfio_completion_bar(int64_t size_done, int64_t cur_pos,
!        int64_t *work_area)
!    prints completion bar as cur_pos advances
!    needs to be called first with cur-pos=0,
!    and last with >size_done
!
! The following routines are primarily calls to a similar bfio routine:
!
!  pfio_check_if_directory(char *filename)
!    returns 1 if directory,
!            0 if exist but not directory,
!           -1 if nonexistent
!
!  pfio_delete_empty_directories(char *filename, int pos)
!   deletes any directories in filename[0:pos] that are empty
!
!  pfio_ensure_dir_exists(char *dirname, int pos)
!    creates any directories past position pos that do not exit
!    returns # directories created
!
!  pfio_expand_filename(char *flnm, char *expflnm)
!    expands flnm to full explicit file name
!
!  pfio_truncate(char *filename, int64_t size)
!    truncates a file so it is no biger than "size"
!
!  pfio_update_file_time(char *file_name)
!   modifies file time stamp to current time
!   This will idle a second if file current time stamp is current time
!
!  pfio routines originally written June 1999 by Charles C Burch
!  New version written August 2000 by Charles C Burch
!
!  Note: some of the routines might be changed as Linux supports
!   larger file sizes.
!
!*****************************************************************
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
! Access to these routines is through cio.  Do not call these routines.
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!    Date       Author       Description
!    ----       ------       -----------
! 86 2009-01-27 Bill Menger  Fixed the 1-byte extent problem that's been around
!                            forever.  Thanks Dave Glover and Karen Goodger!
! 85 2007-11-29 Gunther      Change code to not call mtio_fclose on a null file
!                            pointer.
! 84 2007-11-27 Menger       Move line 3599 to before 3592 so that base_fn is
!                            set prior to calling reserve_local.
! 83 2007-10-23 Menger       Modified pfio_reserve_extent to not attempt to limit
!                            reserves when the node name is NULL or when the file
!                            size is small.  Also swapped two lines to keep from
!                            locking the lockfile twice in a row possibly (which
!                            could cause deadlock -- jobs hanging).
! 82 2007-03-27 Corn         Update to 64 bit architecture. Basically replace
!                            long with int32_t and long long with int64_t
! 81 2007-03-01 Glover       Set istat=0 in pfio_remote_command_retry to avoid
!                            cpslog error messages: Unable to rsh touch command
! 80 2007-02-15 Menger       Removed the /tmp file system as a viable place for
!                            temporary files.  Requires /tmp/hostname, which helps
!                            by allowing us to mount a separate file system as
!                            /tmp/hostname and leaves the system disk (with /tmp
!                            on it) alone!
! 79 2007-01-25 Menger       Modified to be consistent with LGC's changes and
!                            to correctly handle node names when the temp space
!                            runs out for cpstemp or cpsdata so that it doesn't
!                            try to rsh to node "username" by incorrectly parsing
!                            the file name to get the node name.
! 78 2006-10-10 Menger       Added test3 unit test enabler to test beyond the
!                            extent size of 2 gigs.  Documented the unit test.
! 77 2006-10-03 Menger       Removed lots of remote small file i/o for rsh
!                            command tracking.
! 76 2006-09-26 Menger       Added calls to bfio_readlink_to_end and 
!                            bfio_delete_link_chain.
! 75 2006-07-11 Menger       Modified reserve file space thread to not abort if
!                            the thread creation fails, but to do the reserve
!                            within the main thread.
! 74 2006-05-16 Menger       Removed print at line 5433(+/-) (commented out),
!                            Added pfio_init calls to every routine.(*)
!                            Fixed problem that kept project names from being
!                            saved (see * above) and modified all usage of
!                            disk_threshold to be in "bytes".  The data files
!                            to configure file systems for use in cps have a
!                            field in "megabytes" but the code now converts this
!                            to bytes to use in the structure in memory.
! 73 2006-03-30 Menger       Fix error wherein if hostname is not defined, 
!                            pfio_init aborts with SIGSEGV.
! 72 2006-03-28 Menger       Add default cpsdata_nodes.dat file with warning if
!                            none exist. (also cpstemp_nodes.dat)
! 71 2005-12-20 Burch        Remove obsolete skio code
! 70 2005-11-29 Burch        Added reserve count lock & pfioUnitTest. Fixed disk
!                            threshold & no cpsdata space bug. Changed cpsdata 
!                            allocation logic. Removed disk_is_full logic.
!                            Added Brian Macy's pfio_add_desc_flags.
!                            Added get_fd, get_fileno, get_filename.
! 69 2005-10-06 Menger       In pfio_reserve_extent modified variable ext_size
!                            from being *ext_size=0 to ext_size=0 and changed
!                            references to it on lines 2959, 3960 to reflect
!                            the change.  This was attempting to write data onto
!                            address 0x00000000 which is not a good idea.
! 68 2005-09-01 Menger       Changed call to bfio_get_disk_info to use
!                            disk_dir instead of disk_name.
! 67 2005-08-23 Menger       Back to 3 reserves/node.
! 66 2005-08-16 Stoeckley    Fix problem where null pointer was used (marked
!                             in the code with identifier TRS).
! 65 2005-07-26 Stoeckley    Fix to compile with C++.
! 64 2005-07-19 Menger       Fixed bug where I was writing to an array that
!                            used an index for pfio_cpswork_disk (=2) but
!                            I had shortened the array to only index(0-1).
! 63 2005-06-16 Menger       Modified abort calls, added netapps ld95,96
! 62 2005-06-14 Menger       Changed to 6 reserves/node, 200Mb/sec i/o rate,
!                            and 6/12 minutes wait instead of 30/60 for resrvs.
!                            Added CPS_PROJECT_NAME feature, added 4 routines
!                            to allow access to list of projects, number of
!                            projects, set project name, and get project name.
!                            Restricted access to cpsdata disks where project
!                            name doesn't match.  Give out feedback at job
!                            start to show how much space is available for the
!                            job's data prior to execution. Set the read-ahead
!                            flag to 1 for all files, this allows prefetching.
!                            ALSO removed cpswork availability and left in one
!                            printout for amount of disk space available.
! 61 2005-04-26 Menger       Added mtio dependency. Removed fflush calls and
!                            moved debug output to stderr.
! 60 2005-03-24 Stoeckley    Fix so will compile with C++.
! 59 2004-08-23 Menger       Fixed the inability to do pmv on files where
!                            the CPS disk has been removed from the cpsdata-
!                            nodes.dat file.
! 58 2004-07-27 Menger       Fixed bug wherein if all disks were busy doing
!                            reserves, then even if there was space, the
!                            incorrect disk name was passed back to pfio and
!                            data was written to the local file system.
! 57 2004-07-15 Menger       Added message to syslog on cpstemp abort and to
!                            tell PBS which node to take off line because of
!                            lack of temp space.
!                            Added random logic to randomize disk selection
!                            in a cleaner and more mathematically correct way.
! 56 2004-07-01 Menger       Added logic to abort when out of cpstemp space.
! 55 2004-06-22 Menger       Added oper flag and modified reserve logic to not
!                            use as many locks, removed some informative msg.
! 54 2004-06-17 Menger       Removed file reserve logic for all nodes except
!                            for ld and tt nodes, fixed compiler warnings.
! 53 2004-06-10 Menger       Modified logic on reserves for cpstemp. (print
!                            statements left in place)
! 52 2004-05-20 Menger       Fixed delete-a-file message when file doesn't
!                            exist.  Added error message for other errors.
! 51 2004-05-18 Menger       Added a log message for call to netapps prog.
! 50 2004-05-17 Menger       * Added definition -DNOPTHREADS to allow us to
!                            run without pthread library.
!                            * Added code to leave links in place when a file
!                            cannot be removed.  Also provides warning.
!                            Used Richard's stat call instead of a popen in
!                            pfio_df_tmp call.
!                            * Added pfio_reserve_file_space_na variable to hld
!                            the name of the network appliance version of the
!                            filereserve program, currently defaulted to:
!                            filereservenetapps.  This program calls the script
!                            reservefilenetapps.csh in the .../scripts dir.
!                            * modified code to reserve file space wherein it
!                            tries the netapps program after trying a remote
!                            login, and before it tries a local (nfs) reserve.
! 49  2004-03-03 Bill Menger Changed commit_sw to always be off if asking for
!                            greater than 20480*1000000 bytes extent size.
! 48  2004-01-21 SMCook      Eliminated hardwired rw-r--r-- protections on newly
!                            created non-cpsdata files.  There is one more place
!                            where OrwGrAr is used that I did not change because
!                            I do not yet understand its ramifications.
! 47  2003-11-19 RSDay       long long corrections to pfio_create_next_extent
!                            and pfio_force_max_extent_size.
! 46  2003-10-08 Bill Menger Changed documentation set_ext_size(long long)
! 45  2003-09-23 Bill Menger Added a check variable to completion_bar to fix an
!                            infinite loop problem on small files.
! 44  2003-08-15 Bill Menger Moved definitions to .h file
! 43  2003-08-08 C. C. Burch Changes to extend small reopened files and
!                            change default protections.
! 42  2003-05-27 C. C. Burch Added max_reserves_per_node, >2Gb file extents.
!                            Extract link list routines into lnklst.
!                            Use common read/write lock file routines.
!                            Use bfio_remount_file_disk instead of
!                            bfio_get_disk_space to do disk remounts.
!                            Moved various support routines to bfio.
!                            Added warning when file unlocked while deleted.
! 41  2003-04-01 C. C Burch  Added pfio_repair_lock_file.
!                            Fixed potential special condition lockfile deadlock
! 40  2003-03-14 R.S.Day     Put missing int declaration in argument of
!                            pfio_print_file_stats.
! 39  2002-07-23 C. C. Burch Retain /0's in pfio_gets.
!                            Use lock control for internal file deletes
!                            Write-reserve_file_space waits based on 20Mb/sec IO
!                            Add file activity check for locking expired locks
!                            Add randomness to lock waits.
!                            Use cps config file to get fixed file names
!                            put string functions into str.c
!                            Use pthread_self to get unique identifier for rsh
!                            Add pfio_set_cpsdisk_control.
! 38  2002-06-18 C. C. Burch Replace log_writer by cpslog.
!                            Added pfio_gets and retries in get_user_name.
! 37  2002-06-12 C. C. Burch Change log_writer node to avoid overloading sn03
! 36  2002-05-20 C. C. Burch Replace exit calls to unix_abort_c calls
!                            Add retry-after-a-second for file lock open
!                            Check rsh_timeout error file for rsh_timeout errs
!                            Use log_writer pthread/rsh for log file messages
!                            Disallow reading/writing zero bytes
! 35  2002-04-02 C. C. Burch Removed lock missing/expiring warning messages
!                            Added multiple create file link retries
!                            Added get_error_info to get last io error info
!                            Slow down reserve disk IO for better system IO
!                            Added timeouts/reties to rsh, delete assoc. files
!                            Recognize poeptt disk nodes
!                            Include cpstemp/work/data in linked name
!                            Fix couple "minor" memory leaks
!                            Handle error messages with simultaneous mkdirs
!                            prod/beta/alpha/test versions reserve_disk space
!                            Check new extent sizes and resize if possible
!                            Lengthen reserve space timeout.
!                            Do not expire filelock if file IO active
!                            Abort if reserve space inactive for 30 mins
!                            Put lock/unlock lockfile errors into log file.
! 34  2001-12-21 C. C. Burch Remove limit on number files opened.
!                            Number extents can now be up to 100000.
!                            Redid cpsdisk logic. Added cpswork.
!                            Added optional lock file type.
!                            Reserve disk now done in background.
!                            Added retries for opens.
! 33  2001-11-08 C. C. Burch Modified STATFS ifdef for other -DLINUXx's
! 32  2001-10-16 C. C. Burch Use a script to remove files using rsh
! 31. 2001-09-26 C. C. Burch Fix free pointer bug in pfio_read_cpsdisk_file
! 30. 2001-09-24 C. C. Burch Changed reserve disk space logic to prevent
!                            looping in case of abnormal conditions
! 29. 2001-09-21 C. C. Burch Added get_disk_space calls to remount disks
!                            Added 2nd attempt of statfs in get_disk_space
! 28. 2001-09-19 C. C. Burch Modified reserve disk space messages
!                            Added \ at begin of unix commands used with rsh
! 27. 2001-09-18 C. C. Burch Modified pfio_delete to handle daggling links
!                            Modified pfio_form_fn_ext to work with up to
!                            9999 extents
! 26. 2001-08-09 C. C. Burch Remove calls to delete empty directories
!                            Added pfio_set_file_lock_control
!                            Set lock control to none for read-only files
!                            Added pfio_get_file_info
!                            Changed pfio_delete, pfio_flsz to work with
!                            linked non-NFS mounted files
!                            Added pfio_set_file_auto_delete
! 25. 2001-08-01 C. C. Burch Use separate locks for message and log file
! 24. 2001-06-27 C. C. Burch Only delete empty directory when owner is
!                             current user
!                            Set default error recovery to no on a file
!                            Fixed bug in write message incorectly writing
!                              to log file.
! 23. 2001-05-16 C. C. Burch Fix bug in pfio_exit when unlocking a file
!                             that gets unlocked by another process
! 22. 2001-05-01 C. C. Burch Added chmod on created dirs to set protection
!                            Added first-in/first out queue lists through
!                              pfio_get_var and pfio_put_var
! 21. 2001-04-30 C. C. Burch Added pfio_checksum for diagnostic use,
!                             pfio_update_file_time and
!                             pfio_write_message_file
! 20. 2001-04-16 Ed Schmauch Reordered elements in cpsdisk_struct and
!                             modified pfio_read_cpsdisk_file to make sure
!                             cpsdisk_struct.disk_thresholds is always on
!                             an 8-byte boundary.
!                            Fixed pfio_remote_command to properly send
!                             stderr
! 19. 2001-04-03 C. C. Burch Added pfio_exit to wrap up pfio processing
!                             deletes cpstemp file, unlock locked files,
!                             flush any tracing, close remote links
!                            Changed cpsdata/temp_nodes.dat handling to
!                               allocate req. space
!                            Added remote command rtn to start/stop pfserver
!                            Added pfio tracing tools
!                            Added link list utilities
!                            Added no buffering support
! 18. 2001-03-26 C. C. Burch allow -1 file_commit state which prohibits
!                            reserve disk space on next opened file
!                            Changed protection of created directories
!                            Added fflush on reserve disk printouts
! 17. 2001-03-21 C. C. Burch Use reserve disk space for cpsdata & cpstemp
!                            output files
! 16. 2001-03-12 Vunderink   Increased MAX_CPSDISK_NODES to 100.
! 15. 2001-02-27 C. C. Burch Added file region lock option for files.
!                            Modified unlocking of the lockfile.
!                            Avoid ceating new cpsdata/cpstemp links
!                              on read-only files.
! 14. 2001-02-19 C. C. Burch Added write error recovery for cpstemp/cpsdata
!                            Deleted logging file locking/unlocking
!                            Eliminated some locking for read-only files
! 13. 2001-02-13 Bill Menger Change max_cpsdisk_nodes to 50 from 20
! 12. 2001-02-01 C. C. Burch Handle case where lockfile or logfiles
!                            are not set up--for non-Ponca CPS systems.
!                            Send rsh reserve_file_space output to /dev/null
! 11. 2001-01-25 C. C. Burch Modified bfio_get_disk_space to overcome
!                            Solaris perculiarity
! 10. 2001-01-22 C. C. Burch Modified pfio_delete on cpsdata files to rename
!                              links that are actual files
!                            Abort job if no space for cpsdata files
!                            Added cpslog_message
!                            Use rsh to commit space for remote files
!  9. 2001-01-15 C. C. Burch Fixed warning messages from Solaris
!  8. 2001-01-09 C. C. Burch Increased file locking time limit.
!  7. 2001-01-05 C. C. Burch Fixed bug in removing locks when a different
!                            file is unlocked.
!                            Lock file size now allowed to be > than 65000
!                            Added pfio_dump_lock_file diagnostic
!  6. 2000-12-07 C. C. Burch Fixed bug with chmod/rename with CPSDISK
!                            Added support for arb extent size
!                            Added support for CPSTEMP
!  5. 2000-11-20 C. C. Burch Changed to more random CPSDISK node allocator
!                            Added file space commit/truncate capability
!                            Changed file locking logic for LINUX NFS bug
!  4. 2000-10-20 C. C Burch  Added seek via origin, pfio_rename_file,
!                            completion bar
!  3. 2000-08-31 C. C. Burch Added robust socket support,modified the
!                            host name parsing.
!  2. 2000-05-11 Bill Menger Working on a set of ifdefs to disable bfio.
!  1. 2000-04-10 C. C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
! As disks get faster, pfio_max_io_rate may need to be increased
! On systems with 32-bit file systems use -DNOFSEEK0 flag in the compile
!-------------------------------------------------------------------------------
!</portability_doc>
*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

char *pfio_ident =
"$Id: pfio.c,v 1.638 2009/01/23 22:04:10 mengewm Exp $";

#include "bfio.h"  /*This needs to be first*/
#include "mtio.h"
#include "pfio.h"

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <malloc.h>
#include <netdb.h>
#include <pthread.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <utime.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "unix.h"
#include "cpslog.h"
#include "cnfg.h"
#include "str.h"
/*#include "mth.h"*/
#include "lnklst.h"
/*#include "cgetsys.h"*/
#include <limits.h>

#ifdef __cplusplus
extern "C" {
#endif


/******************** Internally called functions **********************/
int     pfio_actual_delete(char*, int);
int32_t pfio_actual_write(int, int, int64_t, char*, int32_t);
void    pfio_check_ext_info_size(int, int);
int     pfio_alloc_file_node(char*, char*, int*, int64_t);
int     pfio_check_file_fd(int, char*);
int     pfio_check_if_cpsdisk(char*, int*);
int     pfio_check_if_netapp_node(char*);
int     pfio_check_reserve_file_space_size(char*, int64_t, char*);
int     pfio_close_file_extent(int, int);
int     pfio_create_a_file(char*, char *);
int     pfio_create_next_extent(int);
int     pfio_create_new_extent(int, int, char*, int*);
int     pfio_create_symbolic_link(char*, char*);
int64_t pfio_current_file_size(char*);
int     pfio_debug_check(int, int);
int     pfio_decode_old_filename(char*, char*, char*, int*);
int     pfio_delete_a_file(char*);
int     pfio_delete_a_file_extent(char*, int);
int     pfio_delete_file_extent(int, int);
int     pfio_df_tmp();
void    pfio_dump_ext_info(int);
int64_t pfio_file_size(char*);
void    pfio_form_cpsdisk_linked_name(char*, char*, int, char*);
int     pfio_get_cpsdata_file(char*, char*, char*);
int     pfio_get_ext_info(int);
void    pfio_get_ext_num(int, int64_t, int*, int64_t*, int64_t*);
int     pfio_force_extent_max_size(int, int);
int64_t pfio_get_extent_size(int, int, int64_t);
int     pfio_get_node_reserves(char*);
int     pfio_get_filenum();
int     pfio_get_node_from_filename(char*, char*, char*, int);
int     pfio_get_user_name(char*, int);
void    pfio_init();
void    pfio_init_file_info(int);
int     pfio_is_file_zero(char*, int64_t, int64_t);
void    pfio_limit_active_reserves(char*);
int     pfio_local_reserve_file_space(char*, char*, int64_t*);
int     pfio_open_actual(char*, char, char*, int);
int     pfio_open_file_extent(int, int);
int64_t pfio_read_cpsdisk_file(int);
int32_t pfio_read_data(int, char*, int32_t);
int     pfio_remote_command(char*, char*);
int     pfio_remote_command_retry(char*, char*, int, int);
int     pfio_remove_file(char*);
void    pfio_reserve_check(int);
int     pfio_reserve_file_space_no_sync(char*, int64_t*);
int     pfio_reserve_file_space_sync(char*, int64_t*);
void   *pfio_reserve_sub(void*);
int     pfio_setbufsz_extent(int, int);
int     pfio_set_ext_region_lock(int, int, int) ;
int     pfio_trace_dump1(char*,char,int64_t,int64_t);
int     pfio_wait_for_file(char*, int);
int32_t pfio_write_error_recover(int, int, int64_t, char*, int32_t, int32_t);
char*   pfio_get_project_name(); /* get this project's name from memory */
void    pfio_set_project_name(char*);/* used on cpsdisk_node.dat file to restrict
                                      output only to those disks with project
                                      name match. */
int     pfio_get_number_of_projects(); /* return number projects from file */
char  **pfio_get_valid_project_names(); /* read from data file to get list */


char pfio_this_project_name[17] = "abcdefghijklmno\0";
char **pfio_valid_project_names=NULL;
int  pfio_number_of_projects=0;
int  pfio_project_name_changed=0;

char cps_lock_file[255];          /*value read from cps_config file */
char cps_main_host[PFIO_MAX_NODENAME];      /*value read from cps_config file */
char cps_mess_lock_file[255];     /*value read from cps_config file */

char cps_node_name1[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name2[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name3[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name4[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name5[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name6[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name7[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name8[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name9[PFIO_MAX_NODENAME];     /*value read from cps_config file */
char cps_node_name10[PFIO_MAX_NODENAME];    /*value read from cps_config file */

char netapp_node_name1[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name2[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name3[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name4[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name5[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name6[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name7[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name8[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name9[PFIO_MAX_NODENAME];  /*value read from cps_config file */
char netapp_node_name10[PFIO_MAX_NODENAME]; /*value read from cps_config file */

char  cps_default_disk_node[PFIO_MAX_NODENAME]; /*default disk read from cps_config file  */
char  reserve_file_space[255];    /*which reserve_file_space_version to use */
char  reserve_file_space_na[255]; /*which reserve_file_space 4 netapps 2use */
char  rsh_timeout[255];           /*which rsh_timeout to use                */
/*char  pfio_server_cmd[255]; */  /*command to start pfserver-from cnfg     */

/*extent size for next opened file, if new*/
int     pfio_max_io_rate=200000000;     /*assumes 200Mb/sec fastest io rate*/
int64_t pfio_default_extent_size      = DEFAULT_EXTENT_SIZE;
int64_t static pfio_file_extent_size  = DEFAULT_EXTENT_SIZE;
int64_t static pfio_extent_size       = DEFAULT_EXTENT_SIZE;

int   pfio_init_sw=0;            /*set to 1 after initialization           */
int   pfio_remote_sw=0;          /*0:Uses NFS, 1:use sockets               */
int   pfio_num_active_hosts =0;  /*#active server hosts                    */
pid_t pfio_pid=-1;               /*current pid                             */
int   pfio_lock_count=0;         /*lock count status of the_lock_file      */
int   pfio_lock_mess_sw=0;       /*switch to only print lock error once    */
int   pfio_lock_control=1;       /*store lock control before opening file  */
int   pfio_cpsdisk_control=1;    /*store cpsdisk control prior opening file*/
int   pfio_auto_delete=-1;       /*store auto delete setting               */
char  pfio_lock_type='H';        /*lock type for next lock file operation  */
char  pfio_host[PFIO_MAX_NODENAME]; /*host name                            */
char  pfio_user_name[80];        /*user name                               */
char  pfio_file_space_commit_sw=0; /* if 1, set space_commit for next open */
int   pfio_trace_mode=-1;        /*0:no trace, 1-trace                     */
char  pfio_error_string[512];    /*string decribing io status of last io op*/
char  pfio_filescrub_cmd[255];    /*delete shell script from cnfg file      */

/***************************** HOST VARIABLES *******************************/

char pfio_base_host[PFIO_MAX_NODENAME]; /*used for socket io port assignments*/
char pfio_base_pid=0;

/********************** RESERVE FILE SPACE VARIABLES **********************/
struct reserve_struct {
  int64_t size;
  int32_t ifile;
  int32_t ext;
  int     done;
  char    fn[260];
  char    base_fn[260];
  char    host[PFIO_MAX_NODENAME];
  char    ext_node[PFIO_MAX_NODENAME];
};

int max_reserves_per_node=3;     /*Maximum simultaneous reserves per node*/

/******************************** PFIO_INFO ******************************/
struct ext_info_struct {
  int64_t size;
  short   host_ifile; /*host ifile for extents         */
};
struct file_info_struct {
  int64_t flsz;            /*Maximum file size known to pfio             */
  int64_t offset;          /*Current offset position                     */
  int64_t fetch_offset1;   /*beg offset where data has been fetched      */
  int64_t fetch_offset2;   /*end offset where data has been fetched      */
  int64_t ext_size;        /*size of file extents                        */
  int32_t bufsz;               /*bufsz to use with setbufsz                  */
  int32_t read_ahead;          /* if > 0 read ahead 65000 bytes at a time    */
  int32_t nreads;              /*number of reads that have beeb requested    */
  int32_t nwrites;             /*number of writes that have been requested   */
  int32_t cache_hits;          /*number times cache used rather reading      */
  int32_t lock_control;        /* 0:do not lock file, 1:lock file as needed  */
  int32_t num_exts;            /*Maximum extent of file known to pfio        */
  int32_t desc_flags;          /*file descriptor flags to set                */
  int     auto_delete;         /*0 do not auto delete, 1 auto delete         */
  int     cpsdisk_state;       /*-1 if not cpsdisk/cpsdata/cpstemp_num       */
  int     num_disks;           /* Number of disks for simultaneous IO        */
  struct  ext_info_struct *ext_info; /* extent information                  */
  struct  reserve_struct  *reserve_info; /*reserve disk space info          */
  pthread_t reserve_thread;  /*thread for reserve_file_space               */
  char  *fetch_ptr;          /*work area to use for cache data             */
  char  *disks;              /*disks for simultaneous IO                   */
  char  fn[256];             /*file name of opened file                    */
  char  cpsdisk_dir[255];     /*dir used last with cpsdata/temp             */
  char  cpsdisk_node[PFIO_MAX_NODENAME];/*node used last with cpsdata/temp */
  char  file_space_commit;   /*if 1, prewrite disk space to ensure it fits */
  char  rwu;                 /*r-read, w-write, u-update                   */
  char  file_type;           /* D NFS/localdisk-others may be added later  */
  char  write_err_recover;   /*1 to try to recover, 0 to not               */
  char  region_lock;         /*1-do locking, 0=do not do locks             */
  char  file_extended;       /*0 file not extended, 1 if file extended     */
};

int pfio_number_files=0;          /*Number files info allocated */
struct file_info_struct **file_info=NULL;

/************************** LINK LIST VARIABLES *************************/
struct lnklst_struct *delete_files[2] ={NULL,NULL};
struct lnklst_struct *locked_files[2] ={NULL,NULL};
struct lnklst_struct *trace_entries[2]={NULL,NULL};

/************************** CPSDISK VARIABLES ****************************/
struct cpsdisk_info_struct {
  int64_t disk_threshold; /** this is the size (in bytes) of space that
                                should be left on a disk ... if the disk
                                has less than this amount of free space then
                                do not use this disk for creating a file 
                                extent. **/
  char      disk_name[80]; /**  Example: /ptmp/hoepld84 **/
  char      node_name[PFIO_MAX_NODENAME]; /**  Example: hoepld84 **/
  char      disk_dir[80];  /**  Example: /ptmp/hoepld84/ **/
  char      project_name[32]; /** This contains a project name that will be
                                  associated with this particular file system*/
  char      filler[6];         /*padding to make divisible by 8 #bytes */
};
struct cpsdisk_struct {
  time_t    file_time;
  int       num_nodes;
  char      name[8];
  char      file1[80];
  char      file2[80];
  char      reserve_mode;    /* R-random select, S-once selected-use again */
  char      exist_msg;       /*N-exist messsage not printed, Y-printed     */
  int64_t   available_space; /* this will hold the available space         */
  char      filler[2];       /* padding to make even # bytes               */
  struct cpsdisk_info_struct *disk_info;
};

struct cpsdisk_struct cpsdisks[pfio_num_cpsdisks];

int pfio_debug_mode=0;  /*debug mode controls debug print outs, 0 for none*/
int pfio_oper_flag=0;   /*if 1, output file needs to go into the oper path */
int pfio_disk_full_flag =0; /* if out of space on cps disks, set to 1 */
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

/****************General purpose routines, not specific pfio*****************/

/*************** PFIO_CHECKSUM ****************
* basic checksum for diagnostics
*
* Written April 2001 by Charles C Burch
***********************************************/
void pfio_checksum(INTEGER *a, INTEGER *n, INTEGER *res) {
  int n1;
  INTEGER chksum;

  n1=(*n);
  chksum=0;
  while( (--n1)>=0) chksum ^= (*a++);
  (*res)=chksum;
  return;
}

/******************PFIO_COMPLETION_BAR**********************
* A simple little %completion bar display
*   size_done=input(int64_t) of what size means to be done
*   cur_pos=input(int64_t) of current status
*     (needs to first called with zero and when >=size)
*   icheck=ADDRESS of int64_t work area supplied by caller
*   jcheck added to keep from infinite loops on small files. wmm
*
* Written Sep 2000 by Charles C Burch
***********************************************************/
void pfio_completion_bar(int64_t size_done, int64_t cur_pos,
                         int64_t *i_check) {
  char bar[80];
  int i,j;
  int64_t j_check;


  pfio_error_string[0]='\0';
  if(cur_pos==0) {
    strcpy(bar,"---------------------------------");
    strcat(bar," % complete ");
    strcat(bar,"---------------------------------");
    fprintf(stderr," %s\n",bar);
    if(size_done>0) {
      fprintf(stderr," X");
      (*i_check)=size_done/77;
    } else {
      fprintf(stderr," size is non positive, so all done\n");
      (*i_check)=-1;
    }
    return;
  }
  if((*i_check)<0) return;

  while(cur_pos>=(*i_check) ){
    j_check = (*i_check);
    fprintf(stderr,"X");
    i=((77*(*i_check))+(size_done/2))/size_done;
    (*i_check)=((i+1)*size_done)/77;
    if(j_check == (*i_check) ) {
      for(j=i+1;j<77;j++){ fprintf(stderr,"X"); }
      break;
    }
  }

  if(cur_pos>=size_done) {
    fprintf(stderr,"\n");
    (*i_check)=-1;
  }

  return;
}

/**************PFIO_SET_OPER_FLAG *************************
*
*      Called from pftools, pcp option, if being used by newdb
*      account.  This flags that the output file must be written
*      to the oper path.
******************************************************************/
  void pfio_set_oper_flag(){

    if(pfio_init_sw==0) pfio_init();
    pfio_oper_flag=1;
    return;
}

/***************PFIO_CALENDAR2TIME ******************
* Convert calendar time to time_t format
*
* Written March 2001 by Charles C Burch
*****************************************************/
time_t pfio_calendar2time(int year, int mo, int day, int hr, int min, int sec){
  int32_t t_time, temp;
  struct tm tm1;

  tm1.tm_year=year-1900;
  tm1.tm_mon=mo-1;
  tm1.tm_mday=day;
  tm1.tm_hour=hr;
  tm1.tm_min=min;
  tm1.tm_sec=sec;
  tm1.tm_isdst=-1;

  mktime(&tm1);
  t_time=tm1.tm_yday*86400L+sec+60L*(60*hr+min)+(int32_t)timezone;
  temp=0;
  while((--tm1.tm_year)>=70) {
    temp+=365;
    if((tm1.tm_year%4)==0) temp++;
  }
  t_time+=temp*86400L;

  return((time_t) t_time);
}

/***************lnklst routines still in pfio for historical reasons**********/

/*************** PFIO_PUT_VAR **********************
* insert var and value into var_list
*
* Written April 2001 by Charles C Burch
***************************************************/
void pfio_put_var(char *var, char *value) {

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_put_var: var=%s, value=%s, ts=%f\n",
        var, value, (float)unix_get_wtime_stamp_c());
  }

  lnklst_put_var_list(var,value);
  return;
}

/******************* PFIO_GET_VAR *********************
* Extract value for first occurence of var in var_list
*
* Written April 2001 by Charles C Burch
*******************************************************/
void pfio_get_var(char *var, char *value) {
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_get_var: var=%s, ts=%f\n",
        var, (float)unix_get_wtime_stamp_c());
  }
  lnklst_get_var_list(var,value);
  return;
}

/*************unix/bfio interface routines for historical reasons************/

/*********** PFIO_CHECK_IF_DIRECTORY_EMPTY ************
* see if is directory and is empty
*  returns(1) if yes
*          -1 if not a directory
*           0 if directory and is nonempty
*           1 if directory and empty
*
* Written September 2000 by Charles C Burch
******************************************************/
int pfio_check_if_directory_empty(char *flnm) {
  return(bfio_check_if_directory_empty(flnm));
}

/*************** PFIO_DELETE_EMPTY_DIRECTORIES ************
* delete empty directories in flnm ending at pos
*  returns # directories deleted
*
* Written September 2000 by Charles C Burch
**********************************************************/
int pfio_delete_empty_directories(char *flnm, int pos) {
  return(bfio_delete_empty_directories(flnm, pos));
}

/***************** PFIO_ENSURE_DIR_EXISTS ******************
* Ensure directory exists
*
* Written August 2000 by Charles C Burch
**********************************************************/
int pfio_ensure_dir_exists(char *fn, int pos) {
  return(bfio_ensure_dir_exists(fn, pos));
}

/******************** PFIO_EXPAND_FILE_NAME ***************
* expands file name fn_in to file name fn_out[fn_out_len]
*  return -1 if any errors
*  expands current working directory, ~/, ~user, ./ ../
*
* Written by Charles C Burch  August 2000
***********************************************************/
int pfio_expand_file_name(char *fn_in, char *fn_out, int fn_out_len) {
  return(bfio_expand_file_name(fn_in, fn_out, fn_out_len));
}

/************* PFIO_GET_FILE_TIME ************
* Returns date file last modified (-1 if error)
*
* Written August 2000 by Charles C Burch
*********************************************/
time_t pfio_get_file_time(char *fn) {
  return(bfio_get_file_time(fn));
}

/**************** PFIO_GET_USER_NAME **********************
* obtains user name into un, un_len=max size of un
*  return 0 if successful or <0 if error
*
* Written by Charles C Burch  January 2001
***********************************************************/
int pfio_get_user_name(char *un, int un_len){
  if(strlen(unix_user_name_c())>=un_len) {
    strncpy(un, unix_user_name_c(), un_len);
    return(-1);
  }
  strcpy(un, unix_user_name_c());
  return(0);
}

/***************** PFIO_RWX_TO_CHMOD **************************
* convert chmod string to int value
*     String subset of A:rwx, G:rwx, O:rwx
*      : , and blank are optional
*      A-anyone, G-Group, O-owner, r-read, w-write, x-execute
*      No A G and O means to set each to specified value
*
* Written May 2000 by Charles C Burch
**************************************************************/
int pfio_rwx_to_chmod(char *rwx) {
 return(bfio_rwx_to_chmod(rwx));
}

/*************** General purpose support routines: ********************/

/*************************log file support routines********************/

/************** PFIO_WRITE_MESSAGE_FILE ********************
* write message msg to a file flnm
*
* Written April 2001 by Charles C Burch and Donna Vunderink
**********************************************************/
void pfio_write_message_file(char *flnm, char * msg) {
  FILE *fd;
  char mess[512];
  int fd_lock_file, n, n1;

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_write_message_file: fn=%s, msg=%s, ts=%f\n",
        flnm, msg, (float)unix_get_wtime_stamp_c());
  }
  if(pfio_init_sw==0) pfio_init();

  fd_lock_file=bfio_lock_the_lockfile(cps_mess_lock_file,flnm, mess);
  if((fd=mtio_fopen(flnm,"a+"))==NULL) {
    bfio_unlock_the_lockfile(fd_lock_file);
    sprintf(mess,"Error opening in pfio_write_message_file [%d:%s]:",
        errno,strerror(errno));
    strcat(mess,flnm);
    cpslog_message(mess);
    return;
  }

  n=strlen(msg);
  strcpy(mess,msg);
  if(mess[n-1]!='\n') {
    strcat(mess,"\n");
    n++;
  }

  n1=mtio_fwrite(mess,1,n,fd);
  mtio_fclose(fd); /* must close to ensure lock, since mtio is asynch.*/
  bfio_unlock_the_lockfile(fd_lock_file);

  if(n1!=n) {
    sprintf(mess,"Error writing  pfio_write_message_file[stat=%d]:",n1);
    strcat(mess,flnm);
    cpslog_message(mess);
  }

  return;
}

/******* PFIO_WRITE_FILE_TO_LOGFILE ***********
* write contents of a file to the logfile
*  and then delete the file
* Written March 2001 by Charles C Burch
***********************************************/
void pfio_write_file_to_logfile(char *flnm) {
  FILE *fd;
  char line[160];

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_write_file_to_logfile: fn=%s, ts=%f\n",
        flnm, (float)unix_get_wtime_stamp_c());
  }
  if((fd=mtio_fopen(flnm,"r"))==NULL) return;

  while(mtio_feof(fd)==0) {
    if(bfio_get_str(fd, line, 160)>0) {
      cpslog_message(line);
    }
  }
  mtio_fclose(fd);
  bfio_remove_file(flnm);
  return;
}

/*********************Lock file support routines*********************/

/*******************************************************************
*  lock type
*  blank     file is not locked
*  N         return only: No lock file - locks are ignored
*  E         return only: filed got locked but by old lock expiring
*  H         hard lock-anyone can delete expired locks
*  R         Reserve file lock
*  X         Extended lock-only given file requests
*            can expire locks unless older than 100 days
********************************************************************/

/******* PFIO_TRY_LOCKING_FILE ***********
* try locking file_name for timeout seconds
* Note if timeout<0, it means hours
* return lock type if locked
*
* Written November 2001 by Charles C Burch
*****************************************/
char pfio_try_locking_file(char *lock_file, char *file_name_raw,
    int32_t timeout, char lock_type) {

  if(pfio_init_sw==0) pfio_init();

  if(lock_file==NULL) lock_file=cps_lock_file;
  if(lock_file[0]==' ' || lock_file[0]=='\0') lock_file=cps_lock_file;

  return(bfio_try_locking_file(lock_file, file_name_raw, timeout, lock_type));
}

/************** PFIO_LOCK_FILE ***********************
* lock file_name for timeout seconds
* returns 2  if lock file not present
* returns 0  if file gets lock by file not locked
* returns 1  if file gets locked but old lock expired
*
* Written     November 2000 by Charles C Burch
* Logic redid November 2001 by Charles C Burch
*****************************************************/
int pfio_lock_file(char *lock_file, char *file_name, int32_t timeout) {
  int istat;

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_lock_file: lf=%s, fn=%s, timeout=%"PRId32", ts=%f\n",
        lock_file, file_name, timeout, (float)unix_get_wtime_stamp_c());
  }

  if(pfio_init_sw==0) pfio_init();

  if(lock_file==NULL) lock_file=cps_lock_file;
  if(lock_file[0]==' ' || lock_file[0]=='\0') lock_file=cps_lock_file;

 istat=bfio_lock_file(lock_file, file_name, timeout, pfio_lock_type);
 pfio_lock_type='H';
 return(istat);
}

/******** PFIO_UNLOCK_FILE ******************************
* unlock file_name
* returns 2  if lock file not present
* returns 0  if file gets unlocked and was locked
* returns 1  if file gets locked but was not locked
*
* Written November 2000 by Charles C Burch
*******************************************************/
int pfio_unlock_file(char *lock_file, char *file_name_raw) {

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_unlock_file: lf=%s, fn=%s, ts=%f\n",
        lock_file, file_name_raw, (float)unix_get_wtime_stamp_c());
  }

  if(pfio_init_sw==0) pfio_init();

  if(lock_file==NULL) lock_file=cps_lock_file;
  if(lock_file[0]==' ' || lock_file[0]=='\0') lock_file=cps_lock_file;

  return(bfio_unlock_file(lock_file, file_name_raw));
}

/********** PFIO_DUMP_LOCK_FILE **********
* dump the lock file
*
* Written December 2000 by Charles C Burch
*****************************************/
int pfio_dump_lock_file() {

  if(pfio_init_sw==0) pfio_init();
  bfio_dump_lock_file(cps_lock_file);
  return(1);
}

/********************* PFIO_REPAIR_LOCK_FILE *********************
* Scan through lock file and remove any invalid entries
*
* Written April 2003 by Charles C Burch
******************************************************************/
int pfio_repair_lock_file(char *lock_file, int mode){
  int n_errs, nerrs, n_lockfile_recs, i_rec, num_lock_files, ifile, isw;
  char   lockfile[260], file_name[260];
  char   *p_date, *p_time, *expire_time, locktype, *host, *pid, *file;
  struct bfio_lock_struct lock_parms;

  if(pfio_init_sw==0) pfio_init();
  strcpy(lock_parms.file_name,"bfio_repair_lock_file");
  if(lock_file==NULL) lock_file=cps_lock_file;
  if(lock_file[0]==' ' || lock_file[0]=='\0') lock_file=cps_lock_file;

  num_lock_files=bfio_adjust_lockfile_name(lock_file, lockfile, 0);
  nerrs=0;

  for(ifile=0; ifile<num_lock_files; ifile++) {
    bfio_adjust_lockfile_name(lock_file, lockfile, ifile);
    lock_parms.fd=bfio_lock_the_lockfile(lockfile, lock_parms.file_name,
        lock_parms.lock_file);
    n_lockfile_recs=bfio_read_lock_file(lock_parms.file_name, &lock_parms);

    n_errs=0;
    for (i_rec=0; i_rec<n_lockfile_recs; i_rec++) {
      bfio_get_lock_file_rec(&lock_parms, i_rec, &p_date, &p_time, &expire_time,
        &locktype, &host, &pid, &file);
      if(str_is_string_blank(file)==1) continue;

      isw=0;
      if(str_is_string_valid_date(p_date)==0) isw|=1;
      if(str_is_string_valid_time(p_time)==0) isw|=2;
      if(str_is_string_valid_integer(expire_time)==0) isw|=4;
      if(str_is_string_blank(pid)==0) {
        if(str_is_string_valid_integer(pid)==0) isw|=8;
      }
      if(locktype!=' ' && locktype!='R' && locktype!='X' && locktype!='H')
        isw|=16;

      if(isw!=0) {
        str_compress(file_name,file);
        printf("lock file entry invalid:%s %s %s %c %s %s %s\n",
         p_date, p_time, expire_time, locktype, host, pid, file_name);
        if((isw& 1)!=0) printf("  Invalid date\n");
        if((isw& 2)!=0) printf("  Invalid time\n");
        if((isw& 4)!=0) printf("  Invalid expiration time\n");
        if((isw& 8)!=0) printf("  Invalid pid\n");
        if((isw&16)!=0) printf("  Invalid lock type\n");

        bfio_zap_lock_file_rec(&lock_parms, i_rec);
        n_errs++;
      }
    }

    if(n_errs>0 && mode==1) bfio_write_lock_file(lock_parms.file_name,
      &lock_parms, n_lockfile_recs);
    bfio_unlock_the_lockfile(lock_parms.fd);
    free(lock_parms.buff);
    nerrs+=n_errs;
  }

  /** printf("nerrs=%d, mode=%d\n",nerrs, mode); **/
  return(nerrs);
}

/************************Basic file suport routines************************/

/**************** PFIO_CHECK_IF_NETAPP_NODE ********************
*  returns 1 if node is a netapp disk, 0 otherwise
*
*  Written  November 2005 by Charles C Burch
**************************************************************/
int pfio_check_if_netapp_node(char *node) {
  if(pfio_init_sw==0) pfio_init();

  if(strstr(node,netapp_node_name1 ) != NULL) return(1);
  if(strstr(node,netapp_node_name2 ) != NULL) return(1);
  if(strstr(node,netapp_node_name3 ) != NULL) return(1);
  if(strstr(node,netapp_node_name4 ) != NULL) return(1);
  if(strstr(node,netapp_node_name5 ) != NULL) return(1);
  if(strstr(node,netapp_node_name6 ) != NULL) return(1);
  if(strstr(node,netapp_node_name7 ) != NULL) return(1);
  if(strstr(node,netapp_node_name8 ) != NULL) return(1);
  if(strstr(node,netapp_node_name9 ) != NULL) return(1);
  if(strstr(node,netapp_node_name10) != NULL) return(1);
  return(0);
}

/**************** PFIO_CREATE_SYMBOLIC_LINK ********************
*  creates symbolic link to a file - retries periodically
*  return 0 if no error and <0 if error
*
*  Written  March 2002 by Charles C Burch
**************************************************************/
int pfio_create_symbolic_link(char *fn, char *fn_link) {
  int i_wait, istat;
  char work[1024];

  if(pfio_init_sw==0) pfio_init();
  if((istat=symlink(fn, fn_link))<0) {  /*create link*/
    remove(fn_link);                   /*just in case old link exists*/
    bfio_remount_file_disk(fn_link);   /*remount disk*/
    istat=symlink(fn, fn_link);
  }
  if(istat<0) {
    sprintf(work,
    "Warning: Retrying to create link(%s) to (%s) in %s [%d:%s]",
     fn_link,fn,"pfio_create_symbolic_link",errno,strerror(errno));
    cpslog_message(work);
  }

  i_wait=0;
  while(istat<0 && i_wait<1800) {
    if(i_wait<30) {                    /*wait a bit and retry*/
      sleep(1);
      i_wait++;
    } else if(i_wait<300) {
      sleep(5);
      i_wait+=5;
    } else {
      sleep(15);
      i_wait+=15;
    }
    istat=symlink(fn, fn_link);
    if(i_wait>0) {
      if(istat<0) {
        sprintf(work, "Error: Unable to create link(%s) to (%s) in %s[%d:%s]",
         fn_link,fn,"pfio_create_symbolic_link",errno,strerror(errno));
      } else {
        sprintf(work,
         "Warning: Retries(wait=%d) created link(%s) to (%s) in %s",
         i_wait,fn_link,fn,"pfio_create_symbolic_link");
      }
      cpslog_message(work);
    }

  }
  return(istat);
}

/****************** PFIO_IS_FILE_ZERO *************************
*  Checks to see if a file segment is all zero
*  fn=input file name, beg-beginning offset to test, end-ending
*  end=-1 means to use last byte of file as end
*  Returns 1 if file segment zero,
*          0 if eof found but no nonzero data found
*          -1 if nonzero data found
*          -2 if unable to open file
*          -3 if read error during check
*
*  Written March 2002 by Charles C Burch
**************************************************************/
int pfio_is_file_zero(char *fn, int64_t beg, int64_t end) {
  FILE *fd;
  int64_t offset, nbytes, nread;
  char buff [4096];

  if(pfio_init_sw==0) pfio_init();
  nbytes=pfio_file_size(fn);
  if(nbytes<0) return(-2);
  if(end<0) {
    end=nbytes;
  } else if(end>(nbytes-1)) {
    end=nbytes=-1;
  }
  if(beg>(nbytes-1)) return(0);
  if((fd=mtio_fopen(fn,"r"))==NULL) return(-2);

  offset=beg;
  if(mtio_fseek(fd,offset,SEEK_SET)<0) {
    mtio_fclose(fd);
    return(-3);
  }

  while(offset<=end) {
    nbytes=(end-offset+1);
    if(nbytes>sizeof(buff)) nbytes=sizeof(buff);
    nread=mtio_fread(buff, 1, (int32_t) nbytes, fd);
    if(nread<0) {
      mtio_fclose(fd);
      return(-3);
    } if(nread==0) {
      break;
    }

    for(nbytes=0;nbytes<nread; nbytes++) {
      if(buff[nbytes]!='\0') {
        mtio_fclose(fd);
        return(-1);
      }
    }
    offset+=nread;
  }
  mtio_fclose(fd);
  return(1);
}

/********************* PFIO_REMOVE_FILE ***************************
* remove a file name-trying rsh if file remote and NFS fails
*   return 0 if succesful
*
* Written August 2001 by Charles C Burch
*******************************************************************/
int pfio_remove_file(char *filename) {
  int istat;
  char node[PFIO_MAX_NODENAME], rm_cmd[260];

  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_remove_file: fn=%s, ts=%f\n",
        filename, (float)unix_get_wtime_stamp_c());
  }
  if((istat=remove(filename))==0) goto exit;

  if(pfio_init_sw==0) pfio_init();
  strcpy(node,"");
  strcpy(rm_cmd,"");

  pfio_get_file_node(filename,node);

  if(node[0]=='\0' || node[0]==' ' || strcmp(node,pfio_host)==0) {
    istat=-1;
    goto exit;
  }

  strcpy(rm_cmd,pfio_filescrub_cmd);
  strcat(rm_cmd,filename);

  if((istat=pfio_remote_command_retry(node,rm_cmd,3,90))<0) {
    sprintf(rm_cmd,"Error: Unable to rsh delete file %s on node %s",
     filename,node);
    fprintf(stderr,"%s\n",rm_cmd);
    cpslog_message(rm_cmd);
  }

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_remove_file-exit: %d, ts=%f\n",
        istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/******************** PFIO_WAIT_FOR_FILE *********************
* waits up to secs seconds to see if a file becomes available
* Returns -1 if files does not appears after secs seconds
* Returns #seconds before file becomes available
*
* Written March 2002 by Charles C Burch
**************************************************************/
int pfio_wait_for_file(char *fn, int secs) {
  int isecs;

  if(pfio_init_sw==0) pfio_init();
  if(bfio_file_size(fn)>=0) return(0);

  for(isecs=1; isecs<=secs; isecs++) {
    sleep(1);
    if(pfio_current_file_size(fn)>=0) return(isecs);
  }
  return(-1);
}

/********************* PFIO_FILE_SIZE *************************
* get size of a single file filename
* file can be symbolic link
* target file can be non-NFS mounted if CPS filename
*
* Written August 2001 by Charles C Burch
**************************************************************/
int64_t pfio_file_size(char *filename) {
  int64_t flsz;
  char flnm[260], node[PFIO_MAX_NODENAME], cmd[360], work[360];
  char dm[64];
  int i;
  FILE *fp;
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_file_size: fn=%s, ts=%f\n",
        filename, (float)unix_get_wtime_stamp_c());
  }

  strcpy(flnm,"");
  strcpy(node,"");
  strcpy(cmd,"") ;
  strcpy(work,"");

/*** First try way that should work on local or NFS disks ***/
  if((flsz=bfio_file_size(filename))>=0) goto exit;

/*** try remount and then check file size ***/
  bfio_remount_file_disk(filename);
  if((flsz=bfio_file_size(filename))>=0) goto exit;

/*** file either not NFS/local or does not exist  ***/
  if(pfio_init_sw==0) pfio_init();
  pfio_get_file_info(filename, flnm, node);

/********** return -1 if file not remote **************/
  flsz=-1;

  if(node[0]=='\0' || node[0]==' ' || strcmp(node,pfio_host)==0) goto exit;
  if(str_find_str(flnm,0,"/tmp/")!=0) goto exit;

  sprintf(cmd,"rsh_timeout %s 90 \\ls -al %s 2>/dev/null",node,flnm);

  for(i=1; i <= 3 ;i++) {
    if((fp=popen(cmd,"r")) != NULL ) {
      fgets(work,sizeof(work),fp);
      pclose(fp);
    } else {
     /* popen error */
      sprintf(work,"pfio_file_size: popen error cmd=%s\n",cmd);
      goto exit;
    }
    if(strlen(work) > 0 ) {
      sscanf(work,"%s %s %s %s %"PRId64"",dm,dm,dm,dm,&flsz);
    goto exit;
    }
  }
  
exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_file_size-exit: flsz=%"PRId64", ts=%f\n",
        flsz, (float)unix_get_wtime_stamp_c());
  }
  return(flsz);
}

/****************** PFIO_CURRENT_FILE_SIZE ********************
* get current size of a single file filename
* file can be symbolic link
* target file can be non-NFS mounted if CPS filename
*
* Written December 2001 by Charles C Burch
**************************************************************/
int64_t pfio_current_file_size(char *fn) {
  int64_t flsz;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_current_file_size: fn=%s, ts=%f\n",
        fn, (float)unix_get_wtime_stamp_c());
  }
  pfio_update_file_time(fn);
  flsz=pfio_file_size(fn);

  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_current_file_size-exit: flsz=%"PRId64", ts=%f\n",
        flsz, (float)unix_get_wtime_stamp_c());
  }
  return(flsz);
}

/*********************** PFIO_ACTUAL_DELETE **************************
* Deletes a bf file-fn=file name with lock_control
*   return number of extents deleted>=0, if no error
*
* Written August 2000 by Charles C Burch
********************************************************************/
int pfio_actual_delete(char *fn, int lock_sw) {
  int ifile, n_exts, cpsdisk_sw, itemp;
  int64_t isize;
  char fn_ext[260], base_fn[260], full_fn[260];
  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_actual_delete: fn=%s, lock_sw=%d, ts=%f\n",
        fn, lock_sw, (float)unix_get_wtime_stamp_c());
  }

  if(pfio_trace_mode>0) {
    sprintf(fn_ext,"d %s",fn);
    lnklst_put_list_entry(trace_entries,fn_ext,"");
  }

  fn=bfio_get_base_filename(fn);
  bfio_expand_file_name(fn, full_fn, sizeof(full_fn));
  pfio_check_if_cpsdisk(full_fn, &cpsdisk_sw);

  if(cpsdisk_sw==pfio_cpstemp_num) {
    /*  delete from delete_files list  */
    lnklst_delete_list_entry(delete_files,fn);
  }

  /***          Delete file extensions                      ***/
  ifile=-1;
  n_exts=0;
  while (1) {
    pfio_form_fn_ext(full_fn, ++ifile, fn_ext);
    isize=pfio_file_size(fn_ext);

    if(lock_sw) {
      if(bfio_check_lock_file(cps_lock_file, fn_ext)!=' ') {
        
        sprintf(pfio_error_string,
          "Warning: file(%s) being unlocked while being deleted",fn_ext);
          /*fprintf(stderr,"pfio_actual_delete:%d: %s\n",
            __LINE__,pfio_error_string);*/
        /*cpslog_message(pfio_error_string);*/
        pfio_unlock_file(cps_lock_file, fn_ext);
      }
    }

    if(isize<0) {                           /*see if file exists*/
      /*** file does not exist as a real file... could be a link ***/
      if(cpsdisk_sw<0) 
        break;               /*does not exist,non cpsdisk-break*/
      /*** is a cps file but does not exist ***/
      itemp=readlink(fn_ext, base_fn, sizeof(base_fn)); /*does exist */
      if(itemp<=0) break;  /*is not a link and does not exist: break*/
      /*** is a link but does not exist: delete it ***/
      if(remove(fn_ext)!=0) { /*remove link */
      } else {
      }
      continue;
    }

    if(cpsdisk_sw<0) {                    
      /***        non-cps non-linked file--delete it directly            ***/
      if(pfio_delete_a_file(fn_ext)!=0) { /*remove link */
      } else {
        n_exts++;
      }

    } else {
      /***  cps file ***/ 
      /***  Linked file:
            read link, then delete file first and then the link    ***/
      /*** remove base file ***/
      itemp=bfio_readlink_to_end(fn_ext, base_fn, sizeof(base_fn));

      if(itemp>0) {                           /*** then fn_ext is a link ***/
        base_fn[itemp]='\0';

        if(pfio_delete_a_file(fn_ext) != 0) {
        } else {
          n_exts++; /* file was successfully removed */
        }


      } else {
        /** No link found-fn_ext is a file-rename it .del  **/
        strcpy(base_fn,fn_ext);
        strcat(base_fn,".del");
        fprintf(stderr, 
        "pfio_actual_delete:%d: Invalid cpsdata/cpstemp file(%s) being deleted\n  File renamed %s\n",
         __LINE__,fn_ext,base_fn);
        rename(fn_ext,base_fn);
      }
    }
  }
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_actual_delete-exit: result=%d, ts=%f\n",
        n_exts, (float)unix_get_wtime_stamp_c());
  }
  return(n_exts);
}

/******************* PFIO_DELETE_A_FILE_ **********************
* Delete a specific file  (Internal pfio use only)
*
* Error returns -1, success returns 0
* Written December 2001 by Charles C Burch
**************************************************************/
int pfio_delete_a_file(char *fn) {
  int istat;

  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_delete_a_file: fn=%s, ts=%f\n",
        fn, (float)unix_get_wtime_stamp_c());
  }
  istat=-1;
  if(pfio_init_sw==0) pfio_init();
  bfio_remount_file_disk(fn); /*force remount*/
  if(bfio_delete_link_chain(fn) != 0 ) {
    if(errno == ENOENT ) goto exit; /* no such file or directory */
    goto exit;
  } else {
    istat=0;
    goto exit;
  }

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_delete_a_file-exit: fn=%s, state=%d, ts=%f\n",
        fn, istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/******************* PFIO_CREATE_A_FILE_ **********************
* Create a specific file  (Internal pfio use only)
*
* Written December 2001 by Charles C Burch
**************************************************************/
int pfio_create_a_file(char *fn, char *prot) {
  int fd, istat;
  char zero='\0';

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_create_a_file: fn=%s, prot=%s, ts=%f\n",
        fn, prot, (float)unix_get_wtime_stamp_c());
  }

  fd=bfio_open(fn,'w');        /*create the file */
  if(fd<0) {
    bfio_remount_file_disk(fn); /*force remount*/
    fd=bfio_open(fn,'w');
  }

  istat=-1;
  if(fd>=0) {
    /* can I write to the file?  If so, then truncate... */
    if(bfio_write(fd,0,&zero,1)==1) {
        if(strcmp(prot,"") !=0) {
          if(bfio_chmod(fd, bfio_rwx_to_chmod(prot)) == 0) {
            /* Now close the file, if successful, set istat = 0 */
            if(bfio_close(fd) >= 0 ) istat = 0;
          } 
          
        } else {
          /* Now close the file, if successful, set istat = 0 */
          if(bfio_close(fd) >= 0 ) istat = 0;
        }
    }
  }
  /* istat=0 means I could open, write, and close the file */
  /*         It optionally means I could change protections */

  if(fd<0) istat = -1;
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_create_a_file-exit: fn=%s, stat=%d, ts=%f\n",
        fn,istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/********** PFIO_GET_CURRENT_FILE_SIZE ************************
* get the current working file size for given ifile
*
* Written November 2000 by Charles C Burch
**************************************************************/
int64_t pfio_get_current_file_size(int ifile){

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_get_current_file_size: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  if(pfio_init_sw==0) pfio_init();
  if( (ifile=pfio_check_file_fd(ifile,"pfio_get_current_file_size")) < 0)
    return (int64_t)(ifile);
    /* printf("File size %"PRId64" \n",file_info[ifile]->flsz); */
  return (int64_t)file_info[ifile]->flsz;
}

/********************* PFIO_OPEN_ACTUAL ***********************
*       Opens a diskfile-fn=file name
*         rwu=r:read, w:write, u:update
*         file id of opened file >0, if no error
*
*       Written December 2001 by Charles C Burch
**************************************************************/
int pfio_open_actual(char *fn_in, char rwu, char *disks, int num_disks) {
  int  ifile, jfile, file_space_commit_sw;
  char work[260];

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf (stderr,"pfio_open, exfn=%s, rwu=%c, ts=%f\n",
        fn_in, rwu, (float) unix_get_wtime_stamp_c());
  }

  pfio_error_string[0]='\0';
  if((ifile=pfio_get_filenum())<0) {
    sprintf(pfio_error_string,
        "pfio_open error in get_filenum, ifile=%d",ifile);
    printf("%s\n",pfio_error_string);
    ifile=-1;
    goto exit;
  }

  file_info[ifile]->ext_info=NULL;
  file_info[ifile]->num_exts=0;


  if(bfio_expand_file_name(fn_in, file_info[ifile]->fn, 260)<0) {
    sprintf(pfio_error_string,
        "bfio_expand_file_name error in pfio_open, fn=%s", fn_in);
    ifile=-4;
    goto exit;
  }

  if(pfio_cpsdisk_control==1) {
    pfio_check_if_cpsdisk(file_info[ifile]->fn,
     &file_info[ifile]->cpsdisk_state);
  } else {
     file_info[ifile]->cpsdisk_state=-1;
  }

  file_info[ifile]->file_type='D';
  file_info[ifile]->lock_control=pfio_lock_control;
  file_info[ifile]->ext_size=pfio_extent_size;
  file_info[ifile]->file_extended=0;

  switch ( (int) rwu) {
  case 'R':
    rwu='r';
  case 'r':
    file_info[ifile]->lock_control=0;     /*do not lock read only files*/
    break;

  case 'W':
    rwu='w';
  case 'w':
    pfio_actual_delete(file_info[ifile]->fn, file_info[ifile]->lock_control);
    if(bfio_ensure_dir_exists(bfio_get_base_filename(
                                        file_info[ifile]->fn),0)<0) {
      sprintf(pfio_error_string,
          "pfio_open:Error in ensure_dir_exists, file=%s",
          file_info[ifile]->fn);
      ifile=-5;
      goto exit;
    }
    break;

  case 'U':
    rwu='u';
  case 'u':
    if(bfio_ensure_dir_exists(bfio_get_base_filename(
                                        file_info[ifile]->fn),0)<0) {
      sprintf(pfio_error_string,
          "pfio_open:Error in ensure_dir_exists, file=%s",
       file_info[ifile]->fn);
      ifile=-5;
      goto exit;
    }
    break;

  default:
    sprintf(pfio_error_string,"Incorrect rwu(%c) in pfio_open",rwu);
    ifile=-6;
    goto exit;
  }

  pfio_get_ext_info(ifile);
  file_info[ifile]->rwu=rwu;
  file_info[ifile]->bufsz=0;
  file_info[ifile]->offset=0;
  file_info[ifile]->fetch_ptr=NULL;
  file_info[ifile]->fetch_offset1=-1;
  file_info[ifile]->fetch_offset2=-1;
  file_info[ifile]->nreads=0;
  file_info[ifile]->cache_hits=0;
  file_info[ifile]->nwrites=0;

/****************************************************************
 for output files-file_space_commit=1 for cpsdata & cpstemp and
      will be whatever it is set to for other files.
      cpstemp can be overwritten to 0 if specified to be -1
********************************************************************/
  file_space_commit_sw=pfio_file_space_commit_sw;

  if(rwu!='w') {
    file_space_commit_sw=0;   /* no file commit on non-write files*/
  } else {
    if(file_info[ifile]->cpsdisk_state==pfio_cpstemp_num ||
       /* file_info[ifile]->cpsdisk_state==pfio_cpswork_num || */
       file_info[ifile]->cpsdisk_state==pfio_cpsdata_num) {

      if(file_space_commit_sw==(-1)) {
        file_space_commit_sw=0;
      } else {
        file_space_commit_sw=1;
      }
    } else {
      if(file_space_commit_sw==(-1)) file_space_commit_sw=0;
    }
  } /* endif rwu != 'w' */

  /**** wmm *** Modify to not reserve files > 20 gigs *****************/
  if(
     (file_info[ifile]->lock_control==0)           ||
     (pfio_extent_size > ((int64_t)20480)*1000000)
    ) file_space_commit_sw=0;

  file_info[ifile]->file_space_commit=file_space_commit_sw;

/* --- save auto deletion status --- */
  if(rwu=='r') {
    if(pfio_auto_delete==-1) pfio_auto_delete=0;
  } else {
/*  default cpstemp is yes, except for cpstemp */
    if(file_info[ifile]->cpsdisk_state==pfio_cpstemp_num &&
       pfio_auto_delete==-1) pfio_auto_delete=1;
    if(pfio_auto_delete==-1) pfio_auto_delete=0;
  }

  file_info[ifile]->auto_delete=pfio_auto_delete;

  if(pfio_auto_delete==1)
    lnklst_put_list_entry(delete_files,
     bfio_get_base_filename(file_info[ifile]->fn),"");

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_open_actual, ifile=%d, fn=%s, cpsdisk=%d\n",
     ifile,file_info[ifile]->fn,file_info[ifile]->cpsdisk_state);
    pfio_debug_check(1,0);
    fprintf(stderr,
     "  size=%"PRId64", extsz=%"PRId64", numexts=%"PRId32", commit=%d, autodel=%d\n",
     file_info[ifile]->flsz, file_info[ifile]->ext_size,
     file_info[ifile]->num_exts, file_info[ifile]->file_space_commit,
     file_info[ifile]->auto_delete);
  }

  pfio_get_ext_info(ifile);
  if(file_info[ifile]->num_exts==0) {
    if(file_info[ifile]->rwu=='r') {
      file_info[ifile]->file_type=' ';
      ifile=-1;
      goto exit;
    }
    file_info[ifile]->file_extended=1;
    if(pfio_create_next_extent(ifile)<0) {
      file_info[ifile]->file_type=' ';
      ifile=-1;
      goto exit;
    }
  }

  if( (jfile=pfio_open_file_extent(ifile,0))<0) {
    file_info[ifile]->file_type=' ';
    ifile=jfile;
    goto exit;
  }

  if(pfio_trace_mode>0) {
    sprintf(work,"o %d %c %s",ifile+1,rwu,file_info[ifile]->fn);
    lnklst_put_list_entry(trace_entries,work,"");
  }
  ifile++;

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_open-exit: %d, ts=%f\n",
        ifile,(float)unix_get_wtime_stamp_c());
  }
  return(ifile);
}

/**************************RSH support routines*************************/

/****************** PFIO_REMOTE_COMMAND_RETRY *****************
* pfio timeout rsh Issue a command(cmd) on node (node)
* with max-retries each with "timeout"
*
* Written March 2002 by Charles C Burch
**************************************************************/
int pfio_remote_command_retry(char *node, char *cmd, 
                              int max_retries, int timeout) {
  char rsh_cmd[260], work[240];
  int istat, i_try;
  FILE *fp;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,
    "%s: node=%s, cmd=%s, retries=%d, timeout=%d, ts=%f\n",
     "pfio_remote_command_rety",node,cmd,max_retries, timeout, 
     (float)unix_get_wtime_stamp_c());
  }
  strcpy(rsh_cmd,"");
  strcpy(work,"");

  sprintf(rsh_cmd,"rsh_timeout %s %d %s >/dev/null", node, timeout, cmd);

  istat=-1;
  for (i_try=1; i_try<=max_retries; i_try++) {
    strcpy(work,"");
    if((fp=popen(rsh_cmd,"r")) != NULL) {
      fgets(work,sizeof(work),fp);
      pclose(fp);
    } else {
      sprintf(work,"pfio_remote_command_retry: popen error: cmd=%s\n",rsh_cmd);
    }
    if(strlen(work) == 0 ) {
    /** no errors! **/
      istat=0;
      break;
    }

    /*  if rsh_err has TIMEOUT or rsh_timeout--it is a rsh_timeout error */
    if(strstr(work,"TIMEOUT") == NULL && strstr(work,"rsh_timeout") == NULL){
      /* now I know it was a command or popen error, don't try again */
      cpslog_message(work);
      break;
    }
    cpslog_message(work);
    sprintf(work,
      "Warning: rsh_timeout failed at try %d of %d",i_try,max_retries);
    cpslog_message(work);
    sleep(i_try); /* wait a bit, then retry */
  }

  if(i_try>max_retries) {
    /** failed after all retries-write to log file **/
    sprintf(work, "Error: rsh command(%s) failed after %d tries",
     rsh_cmd, max_retries);
    cpslog_message(work);

  } else if(i_try>1) {
    /** write to log file if any failures occurred **/
    sprintf(work, "Inform: rsh command worked after %d tries", i_try);
    istat=0;
    cpslog_message(work);
  }

  return(istat);
}
/*************** PFIO_REMOTE_COMMAND *************
* Issue a command(cmd) on node (node)
*
* Written March 2001 by Charles C Burch
*************************************************/
int pfio_remote_command(char *node, char *cmd) {
  char rsh_cmd[260], work[260];
  FILE *fp;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_remote_command: node=%s, cmd=%s, ts=%f\n",
     node,cmd, (float)unix_get_wtime_stamp_c());
  }

  strcpy(rsh_cmd,"");
  strcpy(work,"");

  sprintf(rsh_cmd,"rsh %s -n %s >/dev/null", node, cmd);
  if((fp=popen(rsh_cmd,"r")) != NULL) {
    fgets(work,sizeof(work),fp);
    pclose(fp);
  } else {
    sprintf(work,"pfio_remote_command: popen error, rsh_cmd=%s\n",rsh_cmd);
  }
  if(strlen(work) == 0 ) return (0);
  cpslog_message(work);
  return(-1);
}

/*********************CPSDISK Support routines*********************/

/************** PFIO_READ_CPSDISK_FILE *********************
* Read df file to see what disks to use and their info
*
* Written Aug 2000 by Charles C Burch
***********************************************************/
int64_t pfio_read_cpsdisk_file(int disk_num) {
  FILE *fd;
  char str[260];
  int str_len, n, i, itmp, num_nodes, status;
  char cpsdisk_file[80];
  time_t file_time;
  int64_t temp, Mb=1000000;
  double gig = 1024.*1024.*1024., dtemp=0.;
  struct statvfs fs;
  /*uint64_t f_fsid = 9999;*/

  /* I will need to look at my project name and check it against
  ** entries in the disk farm (only for cpsdata).  If it is not
  ** the same as the project assigned to the server, then I will
  ** not be seeing that disk in my list (because we are going to
  ** skip it here!).  PROJECT NAME IS from ENV VARIABLE:
  ** CPS_PROJECT_NAME (use setenv with csh or tcsh) , with bash or
  ** sh use CPS_PROJECT_NAME=myprojectname
  ** ADD this project name to the appropriate disk entries in the
  ** cpsdata_nodes.dat file (field 5) to restrict those servers
  ** from getting data for other projects.
  **/

  if(pfio_debug_check(1,1))  {
        fprintf(stderr,"pfio_read cpsdisk_file, disk_num=%d, ts=%f\n",
        disk_num, (float)unix_get_wtime_stamp_c());
  }
  if(pfio_init_sw==0) pfio_init();

  num_nodes=0;
  if(disk_num<0 || disk_num>=pfio_num_cpsdisks) goto exit;


  if( (file_time=bfio_get_file_time(cpsdisks[disk_num].file1)) <0) {
    strcpy(cpsdisk_file, cpsdisks[disk_num].file2);
    if((file_time=bfio_get_file_time(cpsdisk_file))<0) {
      if(cpsdisks[disk_num].exist_msg=='N') {
        sprintf(str,
       "Warning: Unable to open %s_nodes.dat file: pfio_read_cpsdisk_file(%s)",
         cpsdisks[disk_num].name, cpsdisk_file);
        cpslog_message(str);
        fprintf(stderr,"%s\n",str);
        cpsdisks[disk_num].exist_msg='Y';
      }
      cpsdisks[disk_num].num_nodes=0;
      goto exit;
    }
  } else {
    strcpy(cpsdisk_file,cpsdisks[disk_num].file1);
  }

  /*** do not reread if file time as same as previous file read
       UNLESS the user has changed the project name
  ***/
  if( (file_time==cpsdisks[disk_num].file_time) &&
      (pfio_project_name_changed==0)          ) {
    num_nodes=cpsdisks[disk_num].num_nodes;
    goto exit; /*exit if filetime same AND project hasn't changed.*/
  }

  /*reset on exit because we are reading the file anyway, and the entire
    reason for setting the project_name_changed is so that we can trigger
    (above) a re-read of the file even if file_time is ==. */
  cpsdisks[disk_num].file_time=file_time;
  cpsdisks[disk_num].num_nodes=0;
  cpsdisks[disk_num].available_space = 0;

  n=bfio_get_file_num_lines(cpsdisk_file);
  if(pfio_debug_check(2,0))
    fprintf(stderr,"n=%d cpsdisk_file=%s\n",n,cpsdisk_file);

  if(strcmp(cpsdisks[disk_num].name,"cpstemp")==0) {


    /*** if this is a "temporary" disk -- for temp files, then... */
    itmp=pfio_df_tmp(); /* return 0 if no temp device present, 1 for 1, and
                           3 for 2 devices present */
    /*allow space for default cpstemp disk entries*/
    if( (itmp & 1) != 0); n++; /* itmp&1->true if /tmp/ present */
    if( (itmp & 2) != 0); n++; /* timp&2->true if pfio_host /tmp/hostname present */
  } else {
    itmp=0; /* set if no temp devices present */
  }

  if(n==0) goto exit; /* Then no temp devices present and none from file */
  if(cpsdisks[disk_num].disk_info!=NULL) {
    free(cpsdisks[disk_num].disk_info);
    cpsdisks[disk_num].disk_info=NULL;  /*just in case code get changed*/
  }
  if((cpsdisks[disk_num].disk_info=(struct cpsdisk_info_struct*)
       malloc( n*(sizeof(struct cpsdisk_info_struct) )))==NULL) {
    unix_abort_c("Unable to allocate memory in pfio_read_cpsdisk_file");
  }
  for(i=0; i<n; i++) {
    strcpy(cpsdisks[disk_num].disk_info[i].node_name,pfio_host);
    cpsdisks[disk_num].disk_info[i].disk_threshold =   1 ; /* 1 byte! */
    strcpy(cpsdisks[disk_num].disk_info[i].disk_name,"\0");
    strcpy(cpsdisks[disk_num].disk_info[i].disk_dir,"\0");
    strcpy(cpsdisks[disk_num].disk_info[i].project_name,"\0");
  }

   /***** Insert /tmp and /tmp/host *******************/
   /* temp file space is designed to be on two volumes by default:
      first is /tmp/hostname on the local host with 1e9 bytes held in reserve.  This volume is 
        hopefully on a separate disk with /tmp/hostname as a mount point.
      second is /tmp on the local host with 1e9 bytes held in reserve.  This volume could be 
        shared with the system, so don't let it fill up!
  */
  if(itmp>0) {   /** add /tmp/host **/
    strcpy(cpsdisks[disk_num].disk_info[num_nodes].node_name,pfio_host);
    cpsdisks[disk_num].disk_info[num_nodes].disk_threshold=1000*Mb; /* 1000 mil. bytes (not megabytes) */
    strcpy(str,"/tmp/");
    strcat(str,pfio_host);
    strcpy(cpsdisks[disk_num].disk_info[num_nodes].disk_name,str);
    strcat(str,"/");
    strcpy(cpsdisks[disk_num].disk_info[num_nodes].disk_dir,str);

    if(pfio_debug_check(2,0))  {
      fprintf(stdout,
       "%d:disk_num=%d,node=%s, min(bytes)=%"PRId64", disk=%s, dir=%s proj=[%s]\n",
       __LINE__,disk_num,
       cpsdisks[disk_num].disk_info[num_nodes].node_name,
       cpsdisks[disk_num].disk_info[num_nodes].disk_threshold,
       cpsdisks[disk_num].disk_info[num_nodes].disk_name,
       cpsdisks[disk_num].disk_info[num_nodes].disk_dir,
       cpsdisks[disk_num].disk_info[num_nodes].project_name);
    }
    status=statvfs(cpsdisks[disk_num].disk_info[num_nodes].disk_name,&fs);
    if(status == 0 ) {
      /* f_fsid = fs.f_fsid; *//* store the file system id away for later */
      dtemp=(fs.f_bavail*((int64_t)fs.f_bsize))/gig;
      dtemp-= (double) 
       (1.*(cpsdisks[disk_num].disk_info[num_nodes].disk_threshold)/gig);
       dtemp = dtemp > 0 ? dtemp : 0; 
      if(pfio_debug_check(2,0))
        fprintf(stdout,"%s %g Gbytes free\n",
          cpsdisks[disk_num].disk_info[num_nodes].disk_name,dtemp);
      temp = (int64_t)dtemp*gig;
      cpsdisks[disk_num].available_space += temp;
      num_nodes++;
    } else {
      if(pfio_debug_check(2,0)) fprintf(stdout," [Not used]\n");
    }
  }
  /************************* remove /tmp from the list. 2/15/2007 wmm ***************************************
  if(itmp==3) {     *//**add /tmp **//*
    strcpy(cpsdisks[disk_num].disk_info[num_nodes].node_name,pfio_host);
    cpsdisks[disk_num].disk_info[num_nodes].disk_threshold=1000*Mb; *//* 1000 mi. bytes (could be sys disk)*//*
    strcpy(cpsdisks[disk_num].disk_info[num_nodes].disk_name,"/tmp");
    strcpy(cpsdisks[disk_num].disk_info[num_nodes].disk_dir,"/tmp/");
    if(pfio_debug_check(2,0))  {
         fprintf(stdout,
           "i=%d,node=%s, min(bytes)=%"PRId64", disk=%s, dir=%s proj=[%s]",
           disk_num,
           cpsdisks[disk_num].disk_info[num_nodes].node_name,
           cpsdisks[disk_num].disk_info[num_nodes].disk_threshold,
           cpsdisks[disk_num].disk_info[num_nodes].disk_name,
           cpsdisks[disk_num].disk_info[num_nodes].disk_dir,
           cpsdisks[disk_num].disk_info[num_nodes].project_name);
    }

    status=statvfs(cpsdisks[disk_num].disk_info[num_nodes].disk_name,&fs);
    if(status == 0 ) {
      if(f_fsid != fs.f_fsid) { *//* then this is another disk, otherwise
                                    it is the same as above and shouldn't
                                    be counted. *//*
        dtemp=(fs.f_bavail*((int64_t)fs.f_bsize))/gig;
        dtemp-= (double) 
          1.* (cpsdisks[disk_num].disk_info[num_nodes].disk_threshold/Mb);
         dtemp = dtemp > 0 ? dtemp : 0;
        if(pfio_debug_check(2,0))
          fprintf(stdout,"%s %g Gbytes free\n",
            cpsdisks[disk_num].disk_info[num_nodes].disk_name,dtemp);
        temp = (int64_t)dtemp*gig;
        cpsdisks[disk_num].available_space+=temp;
        num_nodes++;
      } else {
        if(pfio_debug_check(2,0)) fprintf(stdout," [Not used]\n");
      }
    } else { *//* same here, do nothing if can't read the file system *//*
      if(pfio_debug_check(2,0)) fprintf(stdout," [Not used]\n");
    }
  }
  *************************** end of surgical cut on 2/15/2007 wmm ******************************************
  ***********************************************************************************************************/

  cpsdisks[disk_num].num_nodes=num_nodes;

  /** read the data file with disk servers on it **/
  if ( (fd=mtio_fopen(cpsdisk_file,"r"))==NULL) goto exit;


  while(mtio_feof(fd)==0) {
    if((str_len=bfio_get_str(fd, str, sizeof(str)))>0) {
      if(pfio_debug_check(2,0))  {
        fprintf(stdout,"strlen=%d, str=%s, ts=%f\n",
            str_len, str, (float)unix_get_wtime_stamp_c());
      }

      if(num_nodes==n) {
        printf(
          "Warning:insufficient space for disk names in read_cpsdisk_file\n");
        break;
      }

      /* skip comments and blank lines (only check 1st char) */
      if(str[0] == '#' || str[0] == ' ') continue;

      /** initialize the cpsdisks[disk_num].disk_info[num_nodes] information**/

      strcpy(cpsdisks[disk_num].disk_info[num_nodes].node_name,"\0");
      strcpy(cpsdisks[disk_num].disk_info[num_nodes].disk_name,"\0");
      strcpy(cpsdisks[disk_num].disk_info[num_nodes].disk_dir,"\0");
      strcpy(cpsdisks[disk_num].disk_info[num_nodes].project_name,"\0");
      cpsdisks[disk_num].disk_info[num_nodes].disk_threshold=0;
      temp=0;
      sscanf(str,"%s %"PRId64" %s %s %s",
        cpsdisks[disk_num].disk_info[num_nodes].node_name,
        &temp,
        cpsdisks[disk_num].disk_info[num_nodes].disk_name,
        cpsdisks[disk_num].disk_info[num_nodes].disk_dir,
        cpsdisks[disk_num].disk_info[num_nodes].project_name);
      cpsdisks[disk_num].disk_info[num_nodes].disk_threshold = temp*Mb;/*bytes*/
      /* add code to replace local host with pfio_host */
      if (strcmp(cpsdisks[disk_num].disk_info[num_nodes].node_name,"localhost") == 0) {
        strcpy(cpsdisks[disk_num].disk_info[num_nodes].node_name,pfio_host);
      }
      if(pfio_debug_check(2,0))  { 
        fprintf(stdout,
         "%d:i=%d,node=%s,min(bytes)=%"PRId64", disk=%s, dir=%s proj=%s\n",
         __LINE__,disk_num,
         cpsdisks[disk_num].disk_info[num_nodes].node_name,
         cpsdisks[disk_num].disk_info[num_nodes].disk_threshold,
         cpsdisks[disk_num].disk_info[num_nodes].disk_name,
         cpsdisks[disk_num].disk_info[num_nodes].disk_dir,
         cpsdisks[disk_num].disk_info[num_nodes].project_name);
      }

      /* check project name for cpsdata disks */
      if((disk_num != pfio_cpsdata_num) ||
         (strcmp(cpsdisks[disk_num].disk_info[num_nodes].project_name,
                 pfio_this_project_name) == 0))
      {
        if(pfio_debug_check(2,0)) fprintf(stdout,"%d: [MATCH] ",__LINE__);
        status = 0;
        fs.f_bavail = 0;
        fs.f_bsize  = 0;
        /* Removed statvfs as it took too long, only active if debug=2 */
        if(pfio_debug_check(2,0)) {
         status=statvfs(cpsdisks[disk_num].disk_info[num_nodes].disk_name,&fs);
         if(status != 0 ) 
           fprintf(stdout,"pfio.c[%d]: filesystem=%s status=%d\n",
           __LINE__,cpsdisks[disk_num].disk_info[num_nodes].disk_name,status);
         dtemp=(fs.f_bavail*((int64_t)fs.f_bsize))/gig;
         fprintf(stdout,"pfio.c[%d]: dtemp=%g threshold(gb)=%g\n",
          __LINE__,dtemp,
          (double) (1.*(cpsdisks[disk_num].disk_info[num_nodes].disk_threshold)
          /gig));
         dtemp-=(double) 
          (1.*(cpsdisks[disk_num].disk_info[num_nodes].disk_threshold)/gig);
         dtemp = dtemp > 0 ? dtemp : 0;
         /* if no space on dev, then can't use this device, set status = -1 */
         if(dtemp == 0 ) status = -1;
        }
        if(status == 0 ) {
          if(pfio_debug_check(2,0)) 
            fprintf(stdout," %g Gbytes free",dtemp);
          temp = (int64_t)dtemp * gig;
          cpsdisks[disk_num].available_space +=temp;
          if(pfio_debug_check(2,0)) fprintf(stdout,"(%d): [OK]\n",__LINE__);
          num_nodes++;
        } else {
          /* do not add this to the list, couldn't get a statvfs from it */
          fprintf(stdout,"%s is not a good file system. Free Space=%g Gbytes\n",
              cpsdisks[disk_num].disk_info[num_nodes].disk_name, dtemp);
        }
      }
    }
  }
  cpsdisks[disk_num].num_nodes=num_nodes;
  mtio_fclose(fd);

exit:
  if(disk_num<0 || disk_num>=pfio_num_cpsdisks) return (int64_t)0;
  if(pfio_project_name_changed == 1) {
     if(pfio_debug_check(2,0)) { 
      fprintf(stderr,"pfio_info:CPS PROJECT SET TO [%s].\n",
       pfio_get_project_name());
      fprintf(stderr,"pfio_info:CPSDATA for prj (%s) has %g Gigabytes avail.\n",
       pfio_get_project_name(),
       (double) cpsdisks[pfio_cpsdata_num].available_space/gig);
     } 
    pfio_project_name_changed = 0;
  }
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_read cpsdisk_file exit: num_nodes=%d, ts=%f\n",
      cpsdisks[disk_num].num_nodes, (float)unix_get_wtime_stamp_c());
  }
  return cpsdisks[disk_num].available_space;
}

/***************** PFIO_GET_NODE_FROM_FILENAME ********************
* get cpsdisk node name and base directory from a cpsdisk file name
*   return blanks in node and base , if not cpsdisk file name
*   returns cpsdisk node number if cpsdisk and -1 if not
*
* Written August 2000 by Charles C Burch
*******************************************************************/
int pfio_get_node_from_filename(char *fn, char *node, char *base,
 int disk_num){
  int i, istat=-1,j,k,l;
  char link_name[260],file_name[260];
  /*int64_t dummy_ll;*/

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"%d:get_node:fn=%s, disk_num=%d, ts=%f\n",
        __LINE__,fn, disk_num, (float)unix_get_wtime_stamp_c());
  }
  pfio_read_cpsdisk_file(disk_num);
  strcpy(link_name,fn);
  if(bfio_readlink_to_end(link_name, file_name, sizeof(file_name)));
  strcpy(node,"");
  strcpy(base,"");
  if(disk_num<0 || disk_num>=pfio_num_cpsdisks) goto exit;
  for(i=0; i<cpsdisks[disk_num].num_nodes; i++) {
    if( str_find_str(file_name,0, cpsdisks[disk_num].disk_info[i].disk_dir)==0) {
      strcpy(node,cpsdisks[disk_num].disk_info[i].node_name);
      strcpy(base, cpsdisks[disk_num].disk_info[i].disk_dir);
      istat=i;
      break;
    }
  }

  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"get_node-exit:node=%s, base=%s, result=%d, ts=%f\n",
        node, base, istat, (float)unix_get_wtime_stamp_c());
  }

  if (istat >= 0 ) goto exit;

  /* add this code below to reconstruct node name if it is no longer in list */
  l=strlen(file_name);
  k=str_find_str(file_name,0,"CPSTEMP");
  if(k < l ) {
    /* create the node from the fn */
    i=str_find_str(file_name,0,"/");
    j=str_find_str(file_name,i+1,"/");
    i += (j + 1);
    k=str_find_str(file_name,i,"/")-i;
    node=strncpy(node,&file_name[i],k);
    *(node + k) ='\0';
    i = str_find_str(file_name,0,node);
    k = k+i+1;
    strncpy(base,file_name,k);
    base[k]='\0';
    /*strcpy(&base[k],"\0");*/
    istat = 0;
    goto exit;
  }

  i=str_find_str(file_name,0,"CPSDATA");
  j=str_find_str(file_name,0,"CPSWORK");
  k = (i < j) ? i : j;
  if(k < l ) {
    /* create the node from the filename */
    i=str_find_str(file_name,0,"/");
    j=str_find_str(file_name,i+1,"/");
    i += (j + 1);
    k=str_find_str(file_name,i,"/")-i;
    node=strncpy(node,&file_name[i],k);
    node[k] ='\0';
    /* Create the base from the file_name */
    i = str_find_str(file_name,0,"scratch") ;
    if(i >= l) {
      istat = -1;
      goto exit;
    }
    k = i+8;
    strncpy(base,file_name,k);
    base[k]='\0';
    istat = 0;
  }else{
   istat = -1;
  }
exit:
  return(istat);
}

/******************** PFIO_GET_FILE_NODE **************************
* get node name from a file name
*   return blanks in node if node can not be found
*
* Written March 2001 by Charles C Burch
*******************************************************************/
void pfio_get_file_node(char *file_name, char *node) {
  char base[30];
  int i,disknum;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_get_file_node: fn=%s, ts=%f\n",
        file_name, (float)unix_get_wtime_stamp_c());
  }

  if(pfio_check_if_cpsdisk(file_name, &disknum));

  if(pfio_get_node_from_filename(file_name,node,base, disknum)>=0) goto exit;

  /* don't need to do this if we already know the disk num (i in this case)
  for(i=0; i<pfio_num_cpsdisks; i++) {
    if(pfio_get_node_from_filename(file_name,node,base, i)>=0) goto exit;
  }
  */
  
  strcpy(node,"");
  i=                      str_find_str(file_name,0,cps_node_name1);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name2);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name3);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name4);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name5);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name6);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name7);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name8);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name9);
  if(file_name[i]!='/') i=str_find_str(file_name,0,cps_node_name10);
  /* Take this out... this is looking for second part of file path to extract node from it...
     requires files to have a /tmp/hostname or /ptmp/hostname ... pattern, which may not be
     correct!  For instance, what about /home/username/file...
  fprintf(stderr,"%d: i=%d file=[%s]\n",__LINE__,i,file_name);
  if(file_name[i]=='/') {
    i=str_find_str(file_name,i+1,"/");
    if(file_name[i]=='/') {
      i1=str_find_str(file_name,i+1,"/");
      if(file_name[i1]=='/' && (i1-i)<PFIO_MAX_NODENAME)
        str_substr(file_name, i+1,i1-1,node,PFIO_MAX_NODENAME);
    }
  }
  fprintf(stderr,"%d: i=%d i1=%d node=%s file=[%s]\n",__LINE__,i,i1,node,file_name);
  */

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_get_file_node exit, node=%s, ts=%f\n",
        node, (float)unix_get_wtime_stamp_c());
  }
  return;
}
/******************** PFIO_GET_CPSDATA_FILE ***********************
* get node and a cpsdata filename using a seed name base_flnm
*
* Written March 2001 by Charles C Burch
*******************************************************************/

/*** NOTE This routine is not being used and has not been tested***/

int pfio_get_cpsdata_file(char *base_flnm, char *node, char *flnm) {
  int i;

  if(pfio_init_sw==0) pfio_init();
  node[0]='\0';
  flnm[0]='\0';
  i=pfio_cpsdata_num;
  if(pfio_alloc_file_node(node, flnm, &i,0)<0) {
    sprintf(pfio_error_string,
     "Abort:Unable to alloc file node in pfio_get_cpsdata_file");
    printf("%s\n",pfio_error_string);
    cpslog_message(pfio_error_string);
    unix_abort_c(pfio_error_string);
  }
  i=strlen(flnm);
  strcat(flnm,"/");
  strcat(flnm,base_flnm);
  if(bfio_ensure_dir_exists(flnm,i)<0) {
    sprintf(pfio_error_string,
     "Error: Unable to create directory(%s) in pfio_get_cpsdata_file[%d:%s]",
     flnm,errno,strerror(errno));
    cpslog_message(pfio_error_string);
    unix_abort_c(pfio_error_string);
  }
  return(0);
}

/*********************** PFIO_ALLOC_FILE_NODE ********************
* finds an available disk and return in node_name and disk dir
*  returns cpsdisk node num, if successful, -1 otherwise
*  returns -2 if no disk space left! wmm 
*  Note-on cpstemp-the local disks for the host are searched
*  first, then the rest of the cpstemp disks.
*
* Logic changed for more random results & check extent size 09/2005
* CPSTEMP added  Nov 2000
* max reserve per node logic added Feb 2003
*
* Written Aug 2000 by Charles C Burch
******************************************************************/
int pfio_alloc_file_node(char *node_name, char *disk_dir, int *disknum,
                         int64_t extsz) {
  int i, j, k, disk_num, istat, n, use_this_disk, min_reserves;
  int *indices=NULL, cpsdisk_read=0;
  char cmdstr[260];
  char *pbsreqid,*hostname,*pidno,*disknode;
  int64_t temp;
  static int disknum_old=-1;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,
     "pfio_alloc_file_node, disk_num=%d, node=%s, dir=%s, extsx=%"PRId64", ts=%f\n",
     *disknum, node_name, disk_dir, extsz, (float)unix_get_wtime_stamp_c());
  }
  pfio_disk_full_flag = 0; /* This is set to 1 if all disks for this type of 
                              data (cpsdata, cpstemp, cpswork) are full */

  /*use_this_disk & min_reserves used to get disk with minimum reserves,
    if all num reserves too large */
  use_this_disk=-1;
  min_reserves=INT_MAX;

  istat=-1;
  disk_num=(*disknum);
  if(disk_num<0 || disk_num>=pfio_num_cpsdisks) goto ERROR;

  /* no need to reread cpstemp_nodes if already previously done */
  if(disknum_old==pfio_cpstemp_num) cpsdisk_read=1;
  if(disk_num!=disknum_old) {
    pfio_read_cpsdisk_file(disk_num);   /*read disk info */
    disknum_old=disk_num;
    cpsdisk_read=1;
  }

  if(cpsdisks[disk_num].reserve_mode=='R') {
    node_name[0]='\0';       /* strcpy(node_name,""); */
    disk_dir[0]='\0';        /*strcpy(disk_dir,"");   */
  }

  /*try to use same disk if such specified*/
  if(node_name[0]!='\0' && disk_dir[0]!='\0') {
    for(i=0;i<cpsdisks[disk_num].num_nodes;i++) {
      if(strcmp(node_name,cpsdisks[disk_num].disk_info[i].node_name)==0 &&
         strcmp(disk_dir, cpsdisks[disk_num].disk_info[i].disk_dir )==0) {
        /*found node and disk--see if it has enough space*/
        temp=bfio_get_disk_space(cpsdisks[disk_num].disk_info[i].disk_dir);
        if(temp>=cpsdisks[disk_num].disk_info[i].disk_threshold && temp>=extsz){
          /*old node and disk works-use it independent of #active reserves*/
          istat=i;
          goto EXIT;
        }
      }
    }
  }

  /* if here, not enough space on same node OR first hit for a file */
  while(1) {
    /* read disk file for the type of disk (cpsdata=0,cpstemp=1, cpswork=2) */
    /* Hopefully here is where an array of valid disk names was read!
       and cpsdisk file has not changed                                     */
    if( disk_num!=disknum_old || !cpsdisk_read) {
      pfio_read_cpsdisk_file(disk_num); /*read disk info if not already done*/
      disknum_old=disk_num;
      cpsdisk_read=1;
    }

    /*    first try local disks if cpstemp  */
    if(disk_num==pfio_cpstemp_num) {
      if(pfio_debug_check(2,0)) {
        fprintf(stderr,"trying cpstemp local disks, ts=%f\n",
            (float)unix_get_wtime_stamp_c());
      }

      for (i=0;i<cpsdisks[disk_num].num_nodes; i++) {
        if(strcmp(cpsdisks[disk_num].disk_info[i].node_name,pfio_host)==0) {
          /*we have right node-now see if enough space*/
          temp=bfio_get_disk_space(cpsdisks[disk_num].disk_info[i].disk_dir);
          if(pfio_debug_check(2,0)) {
            fprintf(stderr,
             "%d:disk space=%"PRId64", threshold=%"PRId64", ts=%f\n",__LINE__,
             temp,cpsdisks[disk_num].disk_info[i].disk_threshold,
             (float)unix_get_wtime_stamp_c());
          }
          if(temp>=cpsdisks[disk_num].disk_info[i].disk_threshold &&
             temp>=extsz){
            /*found a disk to use*/
            strcpy(node_name,cpsdisks[disk_num].disk_info[i].node_name);
            strcpy(disk_dir,cpsdisks[disk_num].disk_info[i].disk_dir);
            (*disknum)=pfio_cpstemp_num;
            istat=i;
            goto EXIT;
          }
        }
      }
      goto disks_full;
    }

    /* not cpstemp--try disks for disk_num in a random manner */
    if(pfio_debug_check(2,0)) {
      fprintf(stderr,"trying %s, ts=%f\n",
          cpsdisks[disk_num].name, (float)unix_get_wtime_stamp_c());
    }

    if(indices!=NULL) free(indices);
    if((indices=(int*) malloc(cpsdisks[disk_num].num_nodes*sizeof(int)))==NULL){
      sprintf(pfio_error_string,
       "Abort: Unable to allocate indices array in pfio_alloc_file_node[%d:%s]",
        errno,strerror(errno));
      (*disknum) = -1;
      goto ERROR;
    }
    n=cpsdisks[disk_num].num_nodes;
    for(i=0;i<n;i++) indices[i]=i;

    /* cycle through disks randomly to find avail space */
    for(j=0;j<n;j++) {
      /* pick a random disk*/
      k=(n-j)*((float)rand())/RAND_MAX;
      if(k == (n-j)) k=0;
      i=indices[k];   /*Try disk i*/
      indices[k]=indices[n-1-j];

      if(pfio_debug_check(2,0)) {
        fprintf(stderr,"<%s> disk space=%"PRId64", threshold=%"PRId64", ts=%f\n",
         cpsdisks[disk_num].disk_info[i].disk_name,
         bfio_get_disk_space(cpsdisks[disk_num].disk_info[i].disk_dir),
         cpsdisks[disk_num].disk_info[i].disk_threshold,
         (float)unix_get_wtime_stamp_c());
      }

      temp=bfio_get_disk_space(cpsdisks[disk_num].disk_info[i].disk_dir);
      if(temp>=cpsdisks[disk_num].disk_info[i].disk_threshold && temp>=extsz) {
        /*found a disk with enough space, check num reserves*/
        k=pfio_get_node_reserves(cpsdisks[disk_num].disk_info[i].node_name);
        if(k<max_reserves_per_node) {
          strcpy(node_name, cpsdisks[disk_num].disk_info[i].node_name);
          strcpy(disk_dir,  cpsdisks[disk_num].disk_info[i].disk_dir);
          istat=i;      /*OK to use*/
          goto EXIT;
        }

        /* Set "use_this_disk" so that we have a disk to fall back on
            in case all of the available disk space is on
            servers with too many reserves running on them.
            It is preset to -1 so I can check below.
        */
        if(k<min_reserves){
          min_reserves=k;
          use_this_disk=i;
        }
      }
    }
    /* I am here if the above loop (for (j=0...) fails to find a disk
       system that has enough space AND has few or no reserves running
       on it.  In the case where there IS disk space but all the servers are
       busy, I want to assign the FIRST disk I found, and I've saved that
       value in "use_this_disk"
    */

    if(use_this_disk >= 0 ) {
      istat = use_this_disk; /* Use even though too many reserves are on it */
      strcpy(node_name, cpsdisks[disk_num].disk_info[istat].node_name);
      strcpy(disk_dir,  cpsdisks[disk_num].disk_info[istat].disk_dir);
      goto EXIT;
    }

  /**** Unable to allocate to cpsdisk disk_num requested, trying backup ****/

disks_full:
    if(disk_num==pfio_cpstemp_num) {
      /*Out of cpstemp space -- request cpu removed if extsz reasonable */
      if(extsz<(10*((int64_t)1073741824)) ) {
        sprintf(pfio_error_string,
          "CALL SYSADMIN: PBS SHOULD REMOVE %s -- It is out of cpstemp space",pfio_host);
        cpslog_message(pfio_error_string);
        fprintf(stderr,"(pfio.c:%d):%s\n",__LINE__,pfio_error_string);
        if((disknode=getenv("DISKNODE"))==NULL) disknode=cps_default_disk_node;
        if((pbsreqid=getenv("PBS_REQID"))==NULL) pbsreqid="pbsreqid";
        if((hostname=getenv("HOSTNAME"))==NULL) hostname=pfio_host;
        if((pidno=getenv("PID"))==NULL) pidno="pid";
        sprintf(cmdstr,"touch /btmp/%s/batchtmp/%s.%s.%s",disknode,
              pbsreqid,hostname,pidno);
        cpslog_message(cmdstr);
        system(cmdstr);
      }
      (*disknum)=-2; /* -2 ==> no disk space left */
      goto ERROR;

    } else if(disk_num==pfio_cpswork_num) {
      /*cpswork-try cpsdata*/
      disk_num=pfio_cpsdata_num;
      (*disknum)=disk_num;

      sprintf(pfio_error_string,
       "Informative warning: Unable to allocate cpswork space, trying cpsdata");
      cpslog_message(pfio_error_string);
      fprintf(stderr,"%s\n",pfio_error_string);

    } else {
      /* cpsdata-no space*/
      (*disknum)=-2; /* -2 ==> no disk space left */
      goto ERROR;
    }
  }

ERROR:
  strcpy(node_name,"");
  strcpy(disk_dir,"");
  istat=-1;
  sprintf(pfio_error_string,"(pfio.c:%d): Unable to allocate disk space",__LINE__);
  pfio_disk_full_flag = 1;
  goto EXIT;  /*redundant but just in case code gets added*/

EXIT:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_alloc_file_node-exit: stat=%d, ts=%f\n",
        istat, (float)unix_get_wtime_stamp_c());
  }
  if(indices!=NULL) free(indices);
  if(istat < 0 ) {
    cpslog_message(pfio_error_string);
    fprintf(stderr,"%s\n",pfio_error_string);
  }
  return(istat);
}

/******************** PFIO_CHECK_IF_CPSDISK ********************
* Check if filename is a cpsdisk filename
*   if not return -1, otherwise where base filename starts
*   disknum=pfio_cpsdata_num if cpsdata,
*           pfio_cpstemp_num if cpstemp,
*           pfio_cpswork_num if cpswork !!! removed by Menger
*           and -1 if neither
*
* Written August 2000 by Charles C Burch
***************************************************************/
int pfio_check_if_cpsdisk(char *fn, int *disknum) {
  int jpos, disk_num, istat,i;
  char cpsdisk1[20],cpsdiskup[20];

  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_check_if_cpsdisk: fn=%s, ts=%f\n",
        fn, (float)unix_get_wtime_stamp_c());
  }
  if(pfio_init_sw==0) pfio_init();


  (*disknum)=-1;
  istat=-1;
  for(disk_num=0; disk_num<pfio_num_cpsdisks; disk_num++) {
    strcpy(cpsdisk1,"/");
    strcat(cpsdisk1,cpsdisks[disk_num].name);
    strcat(cpsdisk1,"/");
    for(i=0;i<strlen(cpsdisk1);i++)cpsdiskup[i]=toupper(cpsdisk1[i]);

/*  first check for cpsdata, cpswork or cpstemp/  */
    jpos=str_find_str(fn,0,cpsdisk1+1);
    if(fn[jpos] == '\0') jpos=str_find_str(fn,0,cpsdiskup+1);
    if(fn[jpos]=='\0') continue;

    if(jpos==0) {
      (*disknum)=disk_num;
      istat=0;
      break;
    }
/* Now check for /cpsdata/ /cpswork/ or /cpstemp/ */
    jpos=str_find_str(fn,0,cpsdisk1);
    if(fn[jpos] == '\0') jpos=str_find_str(fn,0,cpsdiskup);
    if(fn[jpos]!='\0') {
      (*disknum)=disk_num;
      istat=jpos+1;
      break;
    }
  }

  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_check_if_cpsdisk-exit: disknum=%d, result=%d, ts=%f\n",
     *disknum,istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/********************** PFIO_FORM_CPSDISK_LINKED_NAME *********************
* form linked cpsdisk file name(fn_new)
*  from base, filename(fn_old), pos where cpsdata/cpstemp/cpswork occurs
*   fn_new will be the base catenated with directories in
*   fn_old before location jpos with cpsdata, cpswork or cpstemp capitalized
*
*   For example if base is AAAA and fn_old is BBBB.../CCC/cpsdata/DDD...
*     fn_new will be AAAA/CCC/CPSDATA/DDD...
*
* Written Sep 2000 by Charles C Burch
***************************************************************************/
void pfio_form_cpsdisk_linked_name(char *base, char *fn_old,
   int pos, char *fn_new) {
  int i, n;
  char work[260];

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,
     "pfio_form_cpsdisk_linked_name: base=%s, fn=%s, pos=%d, ts=%f\n",
     base, fn_old, pos, (float)unix_get_wtime_stamp_c());
  }
  strcpy(fn_new, base);            /*get base file name    */
  /*printf(" fn_new = %s\n",fn_new);*/
  if(pfio_oper_flag == 1)strcat(fn_new,"oper/");
  if(pos>1) {                      /*add dir prior to jpos */
    str_substr(fn_old,0,pos-2, work, sizeof(work)); /* setting work */
    if((n=str_find_last_str(work,0,"/"))==(pos-1)) n=-1;
    strcat(fn_new,work+n+1);   /* here user name added to fn_new */

    strcat(fn_new,"/");
  }
/*  strcat(fn_new,fn_old+pos+8);*/ /*add file name past cpsdata/cpstemp */
  n=strlen(fn_new);
  strcat(fn_new,fn_old+pos);       /*add file name with cpsdata/cpstemp */

  for(i=0;i<8;i++) {
   if(fn_new[n+i]>='a' && fn_new[n+i]<='z') fn_new[n+i] -= ('a'-'A');
  }

  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_form_cpsdisk_linked_name: fn_new=%s, ts=%f\n",
        fn_new, (float)unix_get_wtime_stamp_c());
  }
  return;
}

/******************** PFIO_GET_FILE_INFO ******************************
* get final file name and node associated with a file name that
* might be a link
* return(0) on success and -1 on error
*
* Written August 2001 by Charles C Burch
***********************************************************************/
int pfio_get_file_info(char *file_in, char *file_out, char *node) {
  char flnm[260], flnm1[260];
  int n, istat;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_get_file_info: fn=%s, ts=%f\n",
        file_in, (float)unix_get_wtime_stamp_c());
  }
  str_compress(flnm, file_in);

  if(bfio_expand_file_name(flnm, flnm1, sizeof(flnm1))<0) {
    strcpy(file_out,"");
    strcpy(node,"");
    istat=-1;
    goto EXIT;
  }
  /*n=readlink(flnm1, flnm, sizeof(flnm));*/
  n=bfio_readlink_to_end(flnm1, flnm, sizeof(flnm));
  if(n<=0) {
    strcpy(file_out, flnm1);
  } else {
    flnm[n]='\0';
    strcpy(file_out,flnm);
  }

  pfio_get_file_node(file_out,node);
  istat=0;
EXIT:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_get_file_info-exit: stat=%d, ts=%f\n",
        istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/************** PFIO_DECODE_OLD_FILENAME *************************
* decode filename (fn) into node and base_fn
* filenames can be
*  node:fn         :node is where file will reside
*  local:fn        :node="" specifying use local node
*  ---/cpsdata/--- :node picked from cpsdata_nodes.dat
*                   file will be disk on chosen node catenated
*                   with filename past entry before cpsdata/
*  ---------       :use raw fn for base_fn set node to ""
*  returns -1 if not cpsdata file and cpsdata node num if cpsdata
*
* Written August 2000 by Charles C Burch
* Rewritten for separate version for old and new files Nov 2001 CCB
*******************************************************************/
int pfio_decode_old_filename(char *fn_in, char *node, char *base_fn,
 int *disk_num) {
  int jpos, fn_len, n, i_node;
  char fn[260], /* work[260],*/ node_1[PFIO_MAX_NODENAME];

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_decode_old_filename: fn=%s, ts=%f\n",
        fn_in, (float)unix_get_wtime_stamp_c());
  }
  strcpy(fn,"");
  fn_len=str_compress(fn, fn_in); /*if blanks form tmp name, local node*/
  strcpy(node_1, "");

  /***       See entry has node:--- and if node=local       ***/
  i_node=-1;
  (*disk_num=-1);
  if((jpos=str_find_str(fn,0,":")) <fn_len) {
    if(jpos>0) str_substr(fn,0,jpos-1,node_1,PFIO_MAX_NODENAME);
    strcpy(fn,fn+jpos+1);
    fn_len-=(jpos+1);
    if(strcmp(node_1,"local")==0 || pfio_remote_sw==0) strcpy(node_1,"");
  }

   /***               See if cpsdisk file                   ***/
  jpos=pfio_check_if_cpsdisk(fn, disk_num);

  if((*disk_num)<0) {             /* handle  if non-cpsdisk entry */
    if(node_1[0]!='\0') {
      strcpy(node,node_1);
    } else {
      strcpy(node,"");
    }
    strcpy(base_fn,fn);
    goto EXIT;
  }
  n=bfio_readlink_to_end(fn,base_fn, 260);/*cpsdisk entry--see if link exists*/
  if(n<=0) {
    if(node_1[0]!='\0') {      /* No link */
      strcpy(node,node_1);
    } else {
      strcpy(node,"");
    }
    strcpy(base_fn,fn);
  } else {
    /***          if link exist, use it to get info        ***/
    base_fn[n]='\0';
    /*fprintf(stderr,"pfio decode-cpsdisk link exist, base=%s\n",base_fn);
    i_node=pfio_get_node_from_filename(base_fn,node, work, (*disk_num));*/

    /*fprintf(stderr,"inode from get_node=%d\n",i_node);*/
  }

EXIT:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,
     "pfio_decode_old_filename-exit: node=%s, base=%s, inode=%d, ts=%f\n",
     node,base_fn,i_node, (float)unix_get_wtime_stamp_c());
  }
  return(i_node);
}
/******************** PFIO_FORM_FN_EXT *************************
*                    Internal pfio use only
*       Utility to form bf extended file name
*       fn_in is the base file name
*       n is the file extension number
*       fn_out is the output file name
*
*       Written August 2000 by Charles C Burch
***************************************************************/
void pfio_form_fn_ext(char *fn_in, int iext, char *fn_out) {
  int iwork;
  char *s;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_form_fn_ext: fn=%s, ext=%d, ts=%f\n",
        fn_in, iext, (float)unix_get_wtime_stamp_c());
  }
  iwork=str_compress(fn_out, fn_in);
  if(iext<1) return;          /* if ext is 0, then use base filename */

  if(iext>99999) {
    sprintf(pfio_error_string,
     "pfio_form_fn_ext currently only supports 99999 extents, iext=%d",
      iext);
    unix_abort_c(pfio_error_string);
  }

  s=fn_out+iwork;
  (*s++)='.';

  if(iext>=10000) {
    iwork=iext/10000;
    (*s++)=(char) (iwork+ (int)'0');      /* the ten thousands digit */
    iext=iext-10000*iwork;
  }

  if(iext>=1000) {
    iwork=iext/1000;
    (*s++)=(char) (iwork+ (int)'0');      /* the thousands digit */
    iext=iext-1000*iwork;
  }

  iwork=iext/100;
  (*s++)=(char) (iwork+ (int)'0');        /* the hundreds digit */
  iext=iext-100*iwork;

  iwork=iext/10;
  (*s++)=(char) (iwork+(int)'0');         /* the tens digit     */

  (*s++)=(char) (iext-10*iwork+(int)'0'); /* the ones digit     */
  (*s)='\0';
  return;
}

/******************** PFIO_SET_BASE_PROCESS **************************
* Sets host name and process pid for base process so all pfio port
* assignments have same base process
*
* Written December 2002 by Charles C Burch
********************************************************************/
void pfio_set_base_process(char *node, int pid) {
  strcpy(pfio_base_host, node);
  pfio_base_pid=pid;
  return;
}

/**************************pfio info support routines************************/
/************************** PFIO_GET_FILENUM *********************
* get file num for use with pfio rtns, expands pfio_info as needed
*
* Written August 2000 by Charles C Burch
*****************************************************************/
int pfio_get_filenum() {
  int ifile, j, n;

  if(pfio_debug_check(1,1)) {
    fprintf(stderr,
    "pfio_get_filenum, ts=%f\n", (float)unix_get_wtime_stamp_c());
  }

  /*** Initialize structures if not already done ***/
  if(pfio_init_sw==0) pfio_init();

  /************* find available file id *************/
  for (ifile=pfio_number_files-1; ifile>=0; ifile--) {
    if(file_info[ifile]->file_type==' ') {
      pfio_init_file_info(ifile);
      goto EXIT;
    }
  }

  n=32;
  if(pfio_number_files<=0) {
    pfio_number_files = 0; /* should never be less than zero... */
    if((file_info=(struct file_info_struct**)
       malloc((n)*sizeof(struct file_info_struct*)))==NULL) {
      sprintf(pfio_error_string,
       "Unable to malloc pfio_info(%d) in pfio_get_filenum",n);
      unix_abort_c(pfio_error_string);
    }
  } else {
    if((file_info=(struct file_info_struct**)
       realloc((char*) file_info,
         (n+pfio_number_files)*sizeof(struct file_info_struct*)))==NULL) {
      sprintf(pfio_error_string,
       "Unable to realloc pfio_info(%d) in pfio_get_filenum",
       pfio_number_files+n);
      unix_abort_c(pfio_error_string);
    }
  }
  for (j=0;j<n;j++) {
    if((file_info[pfio_number_files]=(struct file_info_struct*) malloc(
     sizeof(struct file_info_struct)))==NULL) {
      unix_abort_c("Unable to malloc pfio_struct in pfio_get_filenum");
    }
    if(pfio_number_files==256) {
      fprintf(stderr,
        "Informative Warning:More than 256 files opened simultaneously\n");
    }
    pfio_init_file_info(pfio_number_files);
    pfio_number_files++;
  }

  ifile=pfio_number_files-n;

EXIT:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_get_filenum: exit, result=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  return(ifile);
}

/************** PFIO_INIT_FILE_INFO ********************
* init a given occurrence of file_info structure
*
*  Written November 2002 by Charles C Burch
********************************************************/
void pfio_init_file_info(int ifile) {
  if(pfio_init_sw==0) pfio_init();
  file_info[ifile]->file_type=' ';
  file_info[ifile]->rwu=' ';
  file_info[ifile]->offset=0;
  file_info[ifile]->bufsz=0;
  file_info[ifile]->fetch_offset1=-1;
  file_info[ifile]->fetch_offset2=-1;
  file_info[ifile]->read_ahead=1; /* wmm changed 6/10/2005 */
  file_info[ifile]->write_err_recover=1;
  file_info[ifile]->region_lock=0;
  file_info[ifile]->num_exts=0;
  file_info[ifile]->num_disks=0;
  file_info[ifile]->desc_flags=0;

  file_info[ifile]->fetch_ptr=NULL;
  file_info[ifile]->ext_info=NULL;
  file_info[ifile]->reserve_info=NULL;
  file_info[ifile]->disks=NULL;

  strcpy(file_info[ifile]->cpsdisk_dir,"");
  strcpy(file_info[ifile]->cpsdisk_node,"");
  strcpy(file_info[ifile]->fn,"");
  return;
}

/************** PFIO_CHECK_FILE_NUM ********************
*  check validity of file number ifile from rtn rtn_name
*    and return adjusted file num
*
*  Written August 2000 by Charles C Burch
********************************************************/
int pfio_check_file_fd(int ifile, char *rtn_name) {

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,
      "pfio_check_file_fd, ts=%f\n", (float)unix_get_wtime_stamp_c());
  }
  if(ifile<1 || ifile>pfio_number_files) {
    sprintf(pfio_error_string,
      "Invalid file number in %s (%d)",rtn_name,ifile);
    return (-1);
  }
  if(file_info[--ifile]->file_type==' ') {
    sprintf(pfio_error_string,
      "Attempting to %s to unopened file(%d)in pfio_check_file_fd",
      rtn_name,ifile+1);
    return(-2);
  }
  return(ifile);
}

/************************cps_node.dat support routines**********************/
/*********************** PFIO_DF_TMP **************************
* see if certain disks are local
* current disks being checked for are /tmp and /tmp/hostname
* returns set bits: 1 if device 1 present, 2 if device 2, etc
*
* Written November 2001 by Charles C Burch
* Modified May 2004 by Richard Day installed by Bill Menger
**************************************************************/

int pfio_df_tmp() {
  char tmp_host[PFIO_MAX_NODENAME];
  int i, j;
  struct stat buf;

  if(pfio_init_sw==0) pfio_init();
  i=0;
  strcpy(tmp_host,"/tmp/");
  j = stat(tmp_host, &buf);
  if(j==0) i |= 1;
  strcat(tmp_host,pfio_host);
  j = stat(tmp_host, &buf);
  if(j==0) i |= 2;
  if(pfio_debug_check(2,0)) 
    fprintf(stderr,"pfio.c[%d]:j=%d i=%d pfio_host=%s\n",
    __LINE__,j,i,pfio_host);
  return (i);
}

/********************Extent support routines**********************/

/********** PFIO_FORCE_EXTENT_MAX_SIZE **********
* Ensure an extent is the maximum extent size
*
* Written August 2000 by Charles C Burch
*************************************************/
int pfio_force_extent_max_size(int ifile, int i_ext) {
  int istat, old_ifile;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_force_extent_max_size: ifile=%d, ext=%d, ts=%f\n",
        ifile, i_ext, (float)unix_get_wtime_stamp_c());
    pfio_debug_check(1,-1);
    fprintf(stderr,"pfio_force_extent_max_size: size=%"PRId64"\n",
        file_info[ifile]->ext_size);
  }

  if((old_ifile=file_info[ifile]->ext_info[i_ext].host_ifile)<0) {
    pfio_open_file_extent(ifile,i_ext);
  }
  istat=bfio_force_file_max_size(file_info[ifile]->ext_info[i_ext].host_ifile,
           (int64_t)file_info[ifile]->ext_size);
  if(old_ifile<0) pfio_close_file_extent(ifile,i_ext);
  return(istat);
}

/*************** PFIO_SETBUFSZ_EXTENT ***************
* setbufsz for a specific file extent
*
* Written August 2000 by Charles C Burch
****************************************************/
int pfio_setbufsz_extent(int ifile, int i_ext) {
  int istat;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_setbufsz_extent: ifile=%d, ext=%d, ts=%f\n",
        ifile, i_ext, (float)unix_get_wtime_stamp_c());
  }
  if(i_ext>=file_info[ifile]->num_exts){
    sprintf(pfio_error_string,
     "ERROR: attempt to access invalid file segment(%d) in %s",
     i_ext,"pfio_setbufsz_extent");
    return(-3);
  }

  istat=bfio_setbufsz(file_info[ifile]->ext_info[i_ext].host_ifile,
                        file_info[ifile]->bufsz);
  return(istat);
}

/****************** PFIO_DELETE_FILE_EXTENT **********************
* Delete a file extent of pfio opened file(Internal pfio use only)
*
* Written November 2000 by Charles C Burch
*****************************************************************/
int pfio_delete_file_extent(int ifile, int i_ext) {

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_delete_file_extent: ifile=%d, ext=%d, ts=%f\n",
        ifile, i_ext, (float)unix_get_wtime_stamp_c());
  }
  return(pfio_delete_a_file_extent(file_info[ifile]->fn, i_ext));
}

/******************* PFIO_DELETE_A_FILE_EXTENT ****************
* Delete a specific file  (Internal pfio use only)
*
* Written December 2001 by Charles C Burch
**************************************************************/
int pfio_delete_a_file_extent(char *file_name, int i_ext) {
  char fn_ext[260];

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_delete_a_file_extent: fn=%s, ext=%d, ts=%f\n",
        file_name, i_ext, (float)unix_get_wtime_stamp_c());
  }
  pfio_form_fn_ext(file_name, i_ext, fn_ext);
  return(pfio_delete_a_file(fn_ext));
}
/********** PFIO_CHECK_EXT_INFO_SIZE *****************
* Ensure ext_info large enough for extent i_ext
*
* Written November 2001 by Charles C Burch
*****************************************************/
void pfio_check_ext_info_size(int ifile, int i_ext) {
  int i;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_check_ext_info_size: ifile=%d, ext=%d, ts=%f\n",
        ifile,i_ext, (float)unix_get_wtime_stamp_c());
  }
  if(i_ext>=file_info[ifile]->num_exts) {
    if(file_info[ifile]->num_exts<=0) {
      if(file_info[ifile]->ext_info!=NULL)
        free(file_info[ifile]->ext_info);
      if((file_info[ifile]->ext_info=(struct ext_info_struct*)
         malloc((i_ext+1)*sizeof(struct ext_info_struct)))==NULL) {
        unix_abort_c("Unable to malloc ext_struct in pfio_check_ext_info_size");
      }
    } else {
      if((file_info[ifile]->ext_info=(struct ext_info_struct*)
         realloc((char*) file_info[ifile]->ext_info,
           (i_ext+1)*sizeof(struct ext_info_struct)))==NULL) {
        unix_abort_c(
        "Unable to realloc ext_struct in pfio_check_ext_info_size");
       }
    }
    for(i=file_info[ifile]->num_exts; i<=i_ext; i++) {
      file_info[ifile]->ext_info[i].host_ifile=-1;
      file_info[ifile]->ext_info[i].size=0;
    }
    file_info[ifile]->num_exts=i_ext+1;
  }
  return;
}

/******************* PFIO_GET_EXT_NUM *************************
* Convert offset to ext number, local offset and size of extent
*
* Written November 2001 by Charles C Burch
**************************************************************/
void pfio_get_ext_num(int ifile, int64_t offset, int *i_ext,
    int64_t *offset_ext, int64_t *ext_size){
  int i;
  int64_t extsz, ext_offset, temp;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_get_ext_num: ifile=%d, offset=%"PRId64", ts=%f\n",
        ifile,offset, (float)unix_get_wtime_stamp_c());
  }

  ext_offset=-1;        /*offset at end of previous file extent*/
  if(offset<0) offset=0;

  for(i=0;i<file_info[ifile]->num_exts; i++) {
    extsz=file_info[ifile]->ext_info[i].size;

    if(extsz<0) {
/*************** reserve disk space happening *******************/
/*****************************************************************
 if offset does not fit with space already reserved, wait until
 it does or when reserve disk done with current extent
******************************************************************/
      if((ext_offset-extsz)<offset) {
        if( (temp=(offset-(ext_offset+1)) ) >= file_info[ifile]->ext_size)
             temp=file_info[ifile]->ext_size-1;
        pfio_get_extent_size(ifile, i, temp);
        extsz=file_info[ifile]->ext_info[i].size;
      }

      if(extsz<0) extsz=-extsz;
    }
    ext_offset+=extsz;
    if(ext_offset>=offset) {
      (*i_ext)=i;
      (*offset_ext)=offset-(ext_offset+1-extsz);
      (*ext_size)=extsz;
      goto exit;
    }
  }
  (*i_ext)=-1;
  (*offset_ext)=0;
  (*ext_size)=0;

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,
      "pfio_get_ext_num-exit: ext=%d, offset=%"PRId64", extsz=%"PRId64", ts=%f\n",
      *i_ext, *offset_ext, *ext_size, (float)unix_get_wtime_stamp_c());
  }
  return;
}

/********************** PFIO_GET_EXT_INFO ***************************
* get extent information for a file
* returns number extends, their size and total file size in file_info
*
* Written November 2001 by Charles C Burch
*********************************************************************/
int pfio_get_ext_info(int ifile) {
  int i_ext;
  int64_t isize, flsz;
  char fn_ext[260], *fn;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_get_ext_info: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }


  fn=bfio_get_base_filename(file_info[ifile]->fn);

  i_ext=-1;
  flsz=0;
  while (1) {
    pfio_form_fn_ext(fn, ++i_ext, fn_ext);
    isize=bfio_file_size(fn_ext);
    /*fprintf(stderr,"isize=%"PRId64", ext=%d\n",isize, i_ext);*/
    if(isize<0) {
      bfio_remount_file_disk(fn_ext); /*force remount*/
      isize=bfio_file_size(fn_ext);
    }
    if(isize<0) break;

    flsz+=isize;
    pfio_check_ext_info_size(ifile,i_ext); /*expands ext_info, sets num_exts*/
    file_info[ifile]->ext_info[i_ext].size=isize;
  }

  file_info[ifile]->flsz=flsz;
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_get_ext_info-exit: numexts=%"PRId32", flsz=%"PRId64", ts=%f\n",
     file_info[ifile]->num_exts, file_info[ifile]->flsz,
     (float)unix_get_wtime_stamp_c());
    for (i_ext=0;i_ext<file_info[ifile]->num_exts; i_ext++) {
      fprintf(stderr,"ext=%d, size=%"PRId64"\n",
          i_ext,file_info[ifile]->ext_info[i_ext].size);
    }
    pfio_debug_check(1,-1);
    fprintf(stderr,"\n");
  }
  return(i_ext);
}

/************* PFIO_OPEN_FILE_EXTENT *****************
* Opens an extent for given ifile
*
* Written November 2000 by Charles C Burch
*****************************************************/
int pfio_open_file_extent(int ifile, int i_ext) {
  int istat, jfile=0;
  char  fn_ext[260], rwu;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0))  {
    fprintf(stderr,"pfio_open_file_extent, ifile=%d, ext=%d, ts=%f\n",
        ifile, i_ext, (float)unix_get_wtime_stamp_c());
  }

  if(i_ext>=file_info[ifile]->num_exts) return(-1);

  if(file_info[ifile]->ext_info[i_ext].host_ifile>=0)
    return(file_info[ifile]->ext_info[i_ext].host_ifile);

  pfio_form_fn_ext(file_info[ifile]->fn, i_ext, fn_ext);

  rwu=file_info[ifile]->rwu;
  if(rwu=='w') rwu='u';

  /*** open the file  ***/
  bfio_remount_file_disk(fn_ext); /*force remount*/
  jfile=bfio_open(fn_ext,rwu);
  if(jfile<0) {
    istat=errno;
    bfio_remount_file_disk(fn_ext); /*open failed, try one more time*/
    jfile=bfio_open(fn_ext,rwu);
    if(jfile>=0) {
      sprintf(pfio_error_string,
       "Warning: OPEN failure(%s) [%d:%s], but worked on retry",
       fn_ext,istat,strerror(istat));
    } else {
      sprintf(pfio_error_string,
       "Warning: OPEN failure on extent %s, ifile=%d [%d:%s]",
       fn_ext,jfile,errno,strerror(errno));
    }
    cpslog_message(pfio_error_string);
  }

  /* set file descriptor flags */
  if(jfile>=0 && file_info[ifile]->desc_flags!=0) {
    istat=bfio_add_desc_flags(jfile, file_info[ifile]->desc_flags);
    if(istat<0) {
      sprintf(pfio_error_string, "Warning: bad exit status from "
        "bfio_add_desc_flags on extent %s, ifile=%d [%d]",
        fn_ext,jfile,istat);
      cpslog_message(pfio_error_string);
    }
  }


  file_info[ifile]->ext_info[i_ext].host_ifile=jfile;
  if(file_info[ifile]->bufsz!=0 && jfile>=0) pfio_setbufsz_extent(ifile, i_ext);
  if(file_info[ifile]->region_lock!=0) pfio_set_ext_region_lock(ifile,i_ext,1);
  return(jfile);
}

/*********** PFIO_CLOSE_FILE_EXT***********************
* Close a file extension
*
* Written August 2000 by Charles C Burch
******************************************************/
int pfio_close_file_extent(int ifile, int i_ext) {
  int istat = 0;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_close_file_extent: ifile=%d, ext=%d, ts=%f\n",
        ifile, i_ext, (float)unix_get_wtime_stamp_c());
  }
  if(i_ext>=file_info[ifile]->num_exts){
    sprintf(pfio_error_string,
     "ERROR: attempt to access invalid file segment(%d) in %s",
     i_ext,"pfio_close_file_extent");
    cpslog_message(pfio_error_string);
    return(-3);
  }
  if(file_info[ifile]->ext_info[i_ext].host_ifile<0) return(0);

  istat=bfio_close(file_info[ifile]->ext_info[i_ext].host_ifile);
  file_info[ifile]->ext_info[i_ext].host_ifile=-1;
  return(istat);
}

/******************** PFIO_RESERVE_EXTENT***********************
* either do or start a file reserve space op on a file extent
*
* Extracted from create new extent June 2003 by Charles C Burch
***************************************************************/
int pfio_reserve_extent(int ifile, int i_ext, char *fn_ext) {
  char base_fn[260], reserve_node[PFIO_MAX_NODENAME], num_reserves_lock[80];
  int istat;
  int64_t flsz,ext_size=0;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_reserve_extent, ifile=%d, i_ext=%d, ts=%f\n",
        ifile, i_ext, (float)unix_get_wtime_stamp_c());
  }

  if(file_info[ifile]->reserve_info==NULL) {
    if((file_info[ifile]->reserve_info=
     (struct reserve_struct*)malloc(sizeof(struct reserve_struct)))==NULL) {
      sprintf(pfio_error_string,
       "Abort:Unable to malloc reserve_struct in %s [%d:%s]",
        "pfio_reserve_extent",errno,strerror(errno));
      cpslog_message(pfio_error_string);
      unix_abort_c(pfio_error_string);
      }
    file_info[ifile]->reserve_info->ifile=-1;
  }

  file_info[ifile]->reserve_info->done=1;
  if(file_info[ifile]->file_space_commit!=1 ) {
    pfio_force_extent_max_size(ifile,i_ext);
     file_info[ifile]->ext_info[i_ext].size=file_info[ifile]->ext_size;
    file_info[ifile]->ext_info[i_ext].size=file_info[ifile]->ext_size;
    if(file_info[ifile]->lock_control!=0)
      pfio_unlock_file(cps_lock_file, fn_ext);
    return(0);
  }

  flsz=1048576; /* 1 megabyte or smaller, just reserve locally */
  pfio_get_file_info(fn_ext, base_fn, reserve_node);
  if(file_info[ifile]->ext_size<=flsz) { 
    ext_size = (int64_t)file_info[ifile]->ext_size;
    pfio_local_reserve_file_space(fn_ext, base_fn,&ext_size);
    flsz=pfio_file_size(fn_ext);
    file_info[ifile]->ext_info[i_ext].size=flsz;
    pfio_unlock_file(cps_lock_file, fn_ext);
    return(0);
  }
  /* at this point have file commit and lock control active */


  /* If node is == "", then I don't care about limiting reserves because
     I am on the local node (I think)  wmm 10/18/2007. 
  */
  if (strcmp(reserve_node,"") != 0 ) { /* then limit reserves maybe */
    /*locking num reserves on node*/
    sprintf(num_reserves_lock,"GettingNumReserves:Node=%s",reserve_node);
    pfio_lock_file(cps_lock_file, num_reserves_lock, 240L);
    pfio_limit_active_reserves(reserve_node);
    pfio_unlock_file(cps_lock_file,num_reserves_lock); /*Unlocking num reserves*/
    bfio_change_lock_type(cps_lock_file, fn_ext,'R');
  } else {
    bfio_change_lock_type(cps_lock_file, fn_ext,'R');
  }  
/****************** thread to do reserve space **********************/
  pfio_reserve_check(ifile); /*ensure any previous thread done*/

  file_info[ifile]->reserve_info->ifile=ifile;
  file_info[ifile]->reserve_info->ext=i_ext;
  file_info[ifile]->reserve_info->done=0; /*thread will set to 1 when done*/
  strcpy(file_info[ifile]->reserve_info->fn, fn_ext);
  file_info[ifile]->reserve_info->size=file_info[ifile]->ext_size;
  strcpy(file_info[ifile]->reserve_info->base_fn,base_fn);
  strcpy(file_info[ifile]->reserve_info->host,pfio_host);
  strcpy(file_info[ifile]->reserve_info->ext_node, reserve_node);
  if((istat=pthread_create(&file_info[ifile]->reserve_thread, NULL,
   pfio_reserve_sub, (void *) file_info[ifile]->reserve_info)) != 0) {
    sprintf(pfio_error_string,
     "Error: Unable to create reserve thread in %s, istat=%d [%d:%s]",
     "pfio_create_next_extent",istat,errno,strerror(errno));
    cpslog_message(pfio_error_string);
    fprintf(stderr,"%s\n",pfio_error_string);
   file_info[ifile]->reserve_thread = (pthread_t) NULL;
   pfio_reserve_sub( (void *) file_info[ifile]->reserve_info);
  }
  return(1);
}

/**************** PFIO_EXTEND_LAST_EXTENT *********************
*  Extend last extent of a file if not normal ext size already
*
*  Written June 2003 by Charles C Burch
**************************************************************/
int pfio_extend_last_extent(int ifile) {
  char fn_ext[260], lock_type, work[260];
  int64_t flsz;
  int  i_ext=-1;
  int  istat, disk_num;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_extend_last_extent, ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  /* have I already been here? If so, then leave now!!! */
  if(file_info[ifile]->file_extended==1) {
    istat=1;
    goto done;
  }
  /* Set the "i've been here" flag */
  file_info[ifile]->file_extended=1;
  /* Get the extent number of the extent we need to check */
  i_ext=file_info[ifile]->num_exts-1;
  /* See if the extent is already at maximum size for an extent on this file*/
  if(file_info[ifile]->ext_info[i_ext].size==file_info[ifile]->ext_size) {
    istat=2;
    /* ... and leave if it is that big already */
    goto done;
  }

  /* Close the actual file extent !  */
  pfio_close_file_extent(ifile, i_ext);
  /* Form a new file name to reopen the same file */
  pfio_form_fn_ext(file_info[ifile]->fn,i_ext,fn_ext);
/*coordinate with other processes to ensure only one creates the extent*/
  while(1) {
    if(file_info[ifile]->lock_control==0) {
      lock_type='N';
    } else {
      lock_type=pfio_try_locking_file(cps_lock_file, fn_ext,7200L,'H');
    }
    /* find the size of the extent you want to extend */
    flsz=pfio_file_size(fn_ext);

    /** fprintf(stderr,"new extent(%s), size=%"PRId32", lock_type=%c\n",
         fn_ext, flsz, lock_type);
    **/

    if(lock_type=='N') {               /*see if no lock*/
      file_info[ifile]->file_space_commit=0;
      lock_type=' ';
    }

    if(lock_type=='R') {              /*someone else is reserving disk space*/
      if(flsz<0) {
        /*something wrong-R lock, no file*/
        pfio_unlock_file(cps_lock_file, fn_ext);
        continue;
      } else {
        if(flsz==file_info[ifile]->ext_size) {
          file_info[ifile]->ext_info[i_ext].size=flsz;
        } else {
          file_info[ifile]->ext_info[i_ext].size=-flsz;
        }
        istat=3;
        goto done;
      }
    }

    if(lock_type==' ') {    /*we locked it-so no one can lock it*/
      if(i_ext==0 && flsz==0) {
        if(pfio_create_new_extent(ifile, i_ext, work, &disk_num)<=-2) {
          istat=-1;
          pfio_unlock_file(cps_lock_file, fn_ext);
          goto done;
        }
      }

      pfio_reserve_extent(ifile, i_ext, fn_ext);
      flsz=pfio_current_file_size(fn_ext);


      if(flsz<=0) {
        sprintf(pfio_error_string,"ERROR-fn_ext not created:%s",fn_ext);
        cpslog_message(pfio_error_string);
        /*unix_abort_c(pfio_error_string);*/
       istat = -7; /* wmm 6/16/05 added this to give error message not abort*/
       pfio_unlock_file(cps_lock_file, fn_ext);
       goto done;
      }

      /**  fprintf(stderr,"after reserve init loop, istat=%d, flsz(%s)=%"PRId32"\n",
        istat, fn_ext, flsz);  **/

      if(flsz==file_info[ifile]->ext_size) {
        file_info[ifile]->ext_info[i_ext].size=flsz;
      } else {
        file_info[ifile]->ext_info[i_ext].size=-flsz;
      }
      istat=0;
      goto done;

    } else {
      sleep(5);           /*non reserve space lock on file*/
      continue;
    }
  }

done:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_extend_last_extent-exit: ext=%d, istat=%d, ts=%f\n",
        i_ext,istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/**************** PFIO_CREATE_NEXT_EXTENT *********************
*  creates next extent and starts reserve disk space if needed
*  return 0 if no error and -1 if error
*
*  Written  December 2001 by Charles C Burch
**************************************************************/
int pfio_create_next_extent(int ifile) {
  char fn_ext[260], lock_type, work[260];
  int64_t flsz, i_ext;
  int disk_num, istat,create_status;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_create_next_extent, ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }

  i_ext=file_info[ifile]->num_exts;
  pfio_check_ext_info_size(ifile, i_ext);   /*enlarge ext_info*/
  pfio_form_fn_ext(file_info[ifile]->fn,i_ext,fn_ext);

/*coordinate with other processes to ensure only one creates the extent*/
  while(1) {
    if(file_info[ifile]->lock_control==0) {
      lock_type='N';
    } else {
      lock_type=pfio_try_locking_file(cps_lock_file, fn_ext,7200L,'H');
    }
    flsz=pfio_file_size(fn_ext);

    if(lock_type=='N') {               /*see if no lock*/
      file_info[ifile]->file_space_commit=0;
      lock_type=' ';
    }

    if(lock_type=='R') {              /*someone else is reserving disk space*/
      if(flsz<0) {
        /*something wrong-R lock, no file*/
        pfio_unlock_file(cps_lock_file, fn_ext);
        continue;
      } else {
        if(flsz==file_info[ifile]->ext_size) {
          file_info[ifile]->ext_info[i_ext].size=flsz;
        } else {
          file_info[ifile]->ext_info[i_ext].size=-flsz;
        }
        istat=0;
        break;
      }
    }

    if(lock_type==' ') {    /*we locked it-so no one can lock it*/
      if(flsz>=0) {
        flsz=pfio_current_file_size(fn_ext);
        file_info[ifile]->ext_info[i_ext].size=flsz;  /*file already exits*/
        if(file_info[ifile]->lock_control!=0) {
          pfio_unlock_file(cps_lock_file, fn_ext);
        }
        istat=0;
        break;
      }

      /*********** need to create the file -no one else has created it *******/
      create_status=pfio_create_new_extent(ifile, i_ext, work, &disk_num);
      if(create_status <= -2) {
        istat=-1;
        pfio_unlock_file(cps_lock_file, fn_ext);
        break;
      }
      pfio_reserve_extent(ifile, i_ext, fn_ext);
      flsz=pfio_file_size(fn_ext);

      if(flsz<0) {
        sprintf(pfio_error_string,"ERROR-fn_ext not created:%s",fn_ext);
        cpslog_message(pfio_error_string);
        /*unix_abort_c(pfio_error_string);*/
        istat = -7;
        pfio_unlock_file(cps_lock_file, fn_ext);
        goto done;
      }

      istat=0;
      while(flsz<=0) {
        if(istat<10) {
          sleep(1);
          istat++;
        } else if(istat<120) {
          sleep(2);
          istat+=2;
        } else if(istat<1800) {
          sleep(5);
          istat+=5;
        } else {
          sprintf(work,
           "Error:Unable to reserve any file space(%s) in 30 minutes",
           fn_ext);
          fprintf(stderr,"%s\n",work);
          cpslog_message(work);
          flsz=1;
          break;
        }
        flsz=pfio_current_file_size(fn_ext);

        if(flsz==file_info[ifile]->ext_size ||
           bfio_check_lock_file(cps_lock_file, fn_ext)!='R') break;
      }

      /**  fprintf(stderr,"after reserve init loop, istat=%d, flsz(%s)=%"PRId32"\n",
        istat, fn_ext, flsz);  **/

      if(flsz==file_info[ifile]->ext_size) {
        file_info[ifile]->ext_info[i_ext].size=flsz;
      } else {
        file_info[ifile]->ext_info[i_ext].size=-flsz;
      }
      istat=0;
      break;

    } else {
      sleep(5);           /*non reserve space lock on file*/
      continue;
    }
  }
done:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_create_next_extent-exit: ext=%"PRId64", istat=%d, ts=%f\n",
        i_ext,istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/************************ PFIO_CREATE_NEW_EXTENT ************************
* Creates a new file extention and ensure it is at least one byte
*  if cpsdisk file:find disk to put file, and establish symbolic link
* returns node where file created, >=0 from disk_num, -1 non cpsdisk,
*   -2 error
*
* Written December 2001 by Charles C Burch
************************************************************************/
int pfio_create_new_extent(int ifile, int i_ext, char *base_fn, int *disk_num) {
  int jpos, fn_len, len_base, i_node; 
  int try_counter,max_tries=10,wmm;
  char fn[260], work[260], node[PFIO_MAX_NODENAME],cpsdisktype[8];
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_create_new_extent, ifile=%d, iext=%d, ts=%f\n",
        ifile, i_ext, (float)unix_get_wtime_stamp_c());
  }

  if(pfio_init_sw==0) pfio_init();
  pfio_form_fn_ext(file_info[ifile]->fn, i_ext, fn);
  fn_len=str_compress(fn, fn); /*if blanks form tmp name, local node*/

/***       See entry has node:--- and if node=local       ***/
  i_node=-2;
  strcpy(node,"");
  if((jpos=str_find_str(fn,0,":")) <fn_len) {
    if(jpos>0) str_substr(fn,0,jpos-1,node,PFIO_MAX_NODENAME);
    strcpy(fn,fn+jpos+1);
    fn_len-=(jpos+1);
    if(strcmp(node,"local")==0 || pfio_remote_sw==0) strcpy(node,"");
  }

  if(fn_len==0) {
    /*unix_abort_c("blank file name in pfio_create_new_extent");*/
    cpslog_message("blank file name in pfio_create_new_extent");
    i_node = -2;
    goto EXIT;
  }
  pfio_delete_a_file(fn);

/***                       See if cpsdisk                     ***/
  if(file_info[ifile]->cpsdisk_state==-1) {
    jpos=-1;                  /*cpsdisk disabled*/
    (*disk_num)=-1;
  } else {
    jpos=pfio_check_if_cpsdisk(fn, disk_num);
  }
  if(pfio_debug_check(2,0)) {
    fprintf(stderr,"cpsdisk check=%d,disk_num=%d, ts=%f\n",
        jpos, *disk_num, (float)unix_get_wtime_stamp_c());
  }
/***          handle  if non-cpsdata entry               ***/
  if((*disk_num)<0) {
    if(node[0]!='\0') {
      strcpy(file_info[ifile]->cpsdisk_node,node);
    } else {
      strcpy(file_info[ifile]->cpsdisk_node,"");
    }
    strcpy(file_info[ifile]->cpsdisk_dir,"");
    strcpy(base_fn,fn);
    wmm=pfio_create_a_file(base_fn,"");
    if(wmm>=0)i_node=-1; 
    goto EXIT;
  }
/********************* cpsdata/cpstemp *************************/
/************ try to find node where file can be created *******/
  strncpy(cpsdisktype,cpsdisks[*disk_num].name,sizeof(cpsdisktype));
  try_counter=0;
  while(try_counter++ < max_tries) {
    i_node=pfio_alloc_file_node(file_info[ifile]->cpsdisk_node,
      file_info[ifile]->cpsdisk_dir, disk_num, file_info[ifile]->ext_size);
    if((*disk_num) == -2 ){ /* then out of space! */
     i_node = (*disk_num); /* pass -2 out to caller */

    }
    if(pfio_debug_check(1,0)) {
       fprintf(stderr,"create_new_ext:inode=%d, node=%s,dir=%s, dn=%d, ts=%f\n",
       i_node, file_info[ifile]->cpsdisk_node, file_info[ifile]->cpsdisk_dir,
       *disk_num, (float)unix_get_wtime_stamp_c());
    }

    if(i_node<0) {
      sprintf(pfio_error_string,
      "ABORT:No space available on %s--contact SPS system support",cpsdisktype);
      cpslog_message(pfio_error_string);
      fprintf(stderr,"(pfio.c:%d):%s\n",__LINE__,pfio_error_string);
      /*unix_abort_c(pfio_error_string);*/
      goto EXIT;
    }
    len_base=strlen(file_info[ifile]->cpsdisk_dir);
    pfio_form_cpsdisk_linked_name(file_info[ifile]->cpsdisk_dir, fn, jpos,
     base_fn);

    if(pfio_debug_check(1,0)) {
       fprintf(stderr,"create_new_extent:fn=%s,%s base_fn=%s, ts=%f\n",
          fn, cpsdisktype,base_fn, (float)unix_get_wtime_stamp_c());
    }
    bfio_ensure_dir_exists(base_fn, len_base);

    if(pfio_create_symbolic_link(base_fn,fn)<0) {
      sprintf(pfio_error_string,
        "Abort: Unable to create link(%s) in pfio_decode_filename", fn);
      /*unix_abort_c(pfio_error_string);*/
      i_node = -2;
      goto EXIT;
    }
    if(file_info[ifile]->cpsdisk_state==pfio_cpstemp_num) {
      if(pfio_create_a_file(base_fn,"rw"     )>=0) goto EXIT;
    } else {
      if(pfio_create_a_file(base_fn,"OrwGrAr")>=0) goto EXIT;
    }
    sprintf(work,"Warning:Unable to create %s file on %s",
      cpsdisktype,file_info[ifile]->cpsdisk_dir);
    cpslog_message(work);
    file_info[ifile]->cpsdisk_node[0]='\0';
    file_info[ifile]->cpsdisk_dir[0]='\0';
    pfio_delete_a_file(fn);
  }

EXIT:
  if(pfio_debug_check(1,-1)) {
     fprintf(stderr,
     "pfio_create_new_extent-exit: node=%s, base=%s, inode=%d, ts=%f\n",
     file_info[ifile]->cpsdisk_node, base_fn,i_node,
     (float)unix_get_wtime_stamp_c());
  }
  return(i_node);
}

/********************** PFIO_DUMP_EXT_INFO **************************
* Dump extext info
*
* Written Decemeber 2001 by Charles C Burch
*********************************************************************/
void pfio_dump_ext_info(int ifile) {
  int i;

  if(pfio_init_sw==0) pfio_init();
  fprintf(stderr,"Dump of extent info for ifile=%d, n_exts=%"PRId32"\n",
   ifile,file_info[ifile]->num_exts);
  for(i=0;i<file_info[ifile]->num_exts;i++) {
    fprintf(stderr,
     "  ext#=%d, size=%"PRId64"\n",i,file_info[ifile]->ext_info[i].size);
  }
  return;
}

/********************** PFIO_GET_EXTENT_SIZE *************************
* Get working extent size and
* wait if needed for ending offset to be avaiable
* while reserve space works
*
* Written Decemeber 2001 by Charles C Burch
*********************************************************************/
int64_t pfio_get_extent_size(int ifile, int i_ext, int64_t ending) {
  char   lock_type, fn_ext[260];
  int64_t flsz, flsz_old;
  time_t t_start;
  int    t_sleep, no_lock_mess_sw, t_wait;

/*** Note the time for inside loop MUST be less than outside loop **/
  int outside_loop_time = 720;   /*12 minutes*/
  int inside_loop_time   =360;   /*6 minutes*/



  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_get_extent_size: ifile=%d, ext=%d, off=%"PRId64", ts=%f\n",
     ifile,i_ext,ending, (float)unix_get_wtime_stamp_c());
  }


  lock_type='U';
  if(i_ext>=file_info[ifile]->num_exts) {
    /*unix_abort_c("extent creation error on pfio_check_extent");*/
    cpslog_message("extent creation error on pfio_check_extent");
    goto exit;
  }

/* if not reserving disk space-return ext size  */

  /*fprintf(stderr,"pfio_get_extent_size: ext=%d ext_size=%"PRId64", size=%"PRId64"\n",
    i_ext, file_info[ifile]->ext_size,file_info[ifile]->ext_info[i_ext].size);
  */
  if(file_info[ifile]->ext_info[i_ext].size>0) {
    return(file_info[ifile]->ext_info[i_ext].size);
  }
  if(ending<1) ending=1; /*need file to be >1 byte*/

/* if reserve disk active-return if enough space reserved*/
  if((ending+file_info[ifile]->ext_info[i_ext].size+1)<=0) {
    return(-file_info[ifile]->ext_info[i_ext].size);
  }

/* need to wait for reserve disk to finish or get enough space */

  pfio_form_fn_ext(file_info[ifile]->fn,i_ext,fn_ext);

  t_start=time(NULL);
  flsz_old=0;
  t_sleep=1;
  no_lock_mess_sw=0;

/*wait up to outside_loop_time with no file activity for file to get to offset*/

  while(difftime(time(NULL),t_start)<outside_loop_time) {
    lock_type=bfio_check_lock_file(cps_lock_file, fn_ext);
    flsz=pfio_file_size(fn_ext);

    /*  fprintf(stderr,
        "file(%s) write wait: ending=%"PRId64", old flsz=%"PRId64", new flsz=%"PRId64"\n",
        fn_ext, ending,file_info[ifile]->ext_info[i_ext].size,flsz);
    */

    if(flsz==file_info[ifile]->ext_size) {
      /*if file size==extent size: reserve must be done*/
      file_info[ifile]->ext_info[i_ext].size=flsz;
      return(file_info[ifile]->ext_info[i_ext].size);
    }
    file_info[ifile]->ext_info[i_ext].size=-flsz;

    if((ending+file_info[ifile]->ext_info[i_ext].size+1)<=0) {
      return(-file_info[ifile]->ext_info[i_ext].size);
    }

    if(flsz>flsz_old) {  /*if file increasing, restart check time*/

      flsz_old=flsz;
      t_start=time(NULL);
    } else {

/*if lock not present and file is not expanding for inside_loop_time then
  assume reserve done*/
      if(lock_type==' ') {
        if(difftime(time(NULL),t_start)>inside_loop_time){
          sprintf(pfio_error_string,
           "Warning: %s(%s) assumed done, unlocked, same size %d mins",
           "reserve disk_space",fn_ext,inside_loop_time/60);
          cpslog_message(pfio_error_string);
          file_info[ifile]->reserve_info->done=1;
        }

        if(file_info[ifile]->reserve_info->done==1) {
          file_info[ifile]->ext_info[i_ext].size=flsz;
          return(file_info[ifile]->ext_info[i_ext].size);
        }
      }
    }

    if(lock_type==' ' && no_lock_mess_sw==0) {
      sprintf(pfio_error_string,
       "Warning: lock expired during reserve disk space(%s)",fn_ext);
      cpslog_message(pfio_error_string);
      no_lock_mess_sw=1;   /*Only do message once*/
    }
/** wait based on reserve file space writing max io rate **/
    t_wait=(ending+file_info[ifile]->ext_info[i_ext].size+1)/pfio_max_io_rate;
    if(t_wait<t_sleep) t_wait=t_sleep;
/**  fprintf(stderr,"t_wait(reserve space)=%d, ending=%"PRId32", current=%"PRId32"\n",
      t_wait, ending, -file_info[ifile]->ext_info[i_ext].size);  **/
    sleep(t_wait);

    if(t_sleep<10) t_sleep++;
    pfio_update_file_time(fn_ext);
  }

  sprintf(pfio_error_string,
   "Abort: reserve disk space(%s) inactive %d minutes in %s",
   fn_ext, outside_loop_time/60, "pfio_get_extent_size");
  fprintf(stderr,"%s\n",pfio_error_string);
  cpslog_message(pfio_error_string);

  sprintf(pfio_error_string,"  lock=%c, ending=%"PRId64", size=%"PRId64"",
      lock_type, ending, file_info[ifile]->ext_info[i_ext].size);
  cpslog_message(pfio_error_string);
  /*unix_abort_c(pfio_error_string);*/
  goto exit;

exit:
  unix_abort_c("pfio_get_extent_size failure");
  return(-1);
}

/************** PFIO_PRINT_FILE_STATS ************
* print file states for file ifile
*
* Written August 2000 by Charles C Burch
*************************************************/
void pfio_print_file_stats(int ifile) {

  if(pfio_init_sw==0) pfio_init();
  if( (ifile=pfio_check_file_fd(ifile,"pfio_print_cache_stats"))<0) {
    fprintf(stderr,"Invalid file#(%d) in pfio_print_file_stats\n", ifile+1);
  } else {
    fprintf(stderr,"#reads on ifile(%d)=%"PRId32", #writes=%"PRId32", #cache hits=%"PRId32"\n",
     ifile+1, file_info[ifile]->nreads,file_info[ifile]->nwrites,
     file_info[ifile]->cache_hits);
  }
  return;
}

/********************** Basic io support routines **********************/

/************ PFIO_READ_DATA ***********************
* read data primitive
*
* Written August 2000 by Charles C Burch
****************************************************/
int32_t pfio_read_data(int ifile, char *buff, int32_t nread) {
  int32_t nbytes, nread_current, nread_actual;
  int64_t offset, ext_size, i_seek;
  int istat = 0, i_ext;
  char *buff_current;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_read_data: ifile=%d, n=%"PRId32", ts=%f\n",
        ifile, nread, (float)unix_get_wtime_stamp_c());
  }
  if( (offset=file_info[ifile]->offset) >= file_info[ifile]->flsz) return(-1);

  nread_actual=0;
  buff_current=buff;
  if(offset+nread<=file_info[ifile]->flsz) {
    nread_current=nread;
  } else {
    nread_current=file_info[ifile]->flsz-offset;
  }
  while(nread_current>0) {
/** fprintf(stderr,"read:offset=%"PRId64", n=%d\n",offset, nread_current); **/
    pfio_get_ext_num(ifile,offset,&i_ext,&i_seek,&ext_size);
    if(i_ext<0) break;

    if((i_seek+nread_current)<=ext_size) {
      nbytes=nread_current;
    } else {
      nbytes=ext_size-i_seek;
    }
/**   fprintf(stderr,"Read file=%d, ext %d, iseek=%"PRId32", nbytes=%"PRId32"\n",
      ifile+1,i_ext, i_seek, nbytes);  ***/

    if(file_info[ifile]->ext_info[i_ext].host_ifile<0) {
      istat=pfio_open_file_extent(ifile,i_ext);
      if(istat<0) break;
    }

    istat=bfio_read(file_info[ifile]->ext_info[i_ext].host_ifile,i_seek,
        buff_current, nbytes);
    if(istat<=0) break;

    buff_current  += istat;
    nread_current -= istat;
    offset        += istat;
    nread_actual  += istat;
    if(istat!=nbytes) break;
  }
  file_info[ifile]->offset+=nread_actual;
  if(nread_actual==0) return(istat);
  return(nread_actual);
}

/***** THE CODE BELOW IS EXPERIMENTAL/THREAD SAFE AT THIS POINT-DO NOT USE*****/
/* This require changes to cps signal handler to work properly with sigterm   */
int io_timeout=0;
pthread_t pfio_pthread_read_timeout;
static sigjmp_buf pfio_timeout_env;

struct io_timeout_struct {
  int ifile;
  char *buff;
  int32_t nread;
  int32_t stat;
};
void* pfio_read_data_sub(void*);
void  pfio_timeout_term(int);
void  pfio_timeout_alarm(int);

void* pfio_read_data_sub(void *ptr){
  struct io_timeout_struct *timeout;

  timeout=(struct io_timeout_struct*) ptr;
  timeout->stat=pfio_read_data(timeout->ifile,timeout->buff, timeout->nread);
  return(ptr);
}

void pfio_timeout_term(int sig) {
  int32_t istat;
  istat=-2;
  pthread_exit((void*) &istat);
}

void pfio_timeout_alarm(int sig) {
  signal(SIGALRM, SIG_IGN);
  alarm(0);
  signal(SIGALRM, SIG_DFL);
  pthread_kill(pfio_pthread_read_timeout, SIGTERM);
  siglongjmp(pfio_timeout_env, 1);
}

int32_t pfio_read_data_timeout(int ifile, char *buff, int32_t nread,
  int timeout) {
  struct io_timeout_struct time_out;
  int32_t istat;

  time_out.ifile=ifile;
  time_out.buff=buff;
  time_out.nread=nread;

  signal(SIGALRM, pfio_timeout_alarm);            /*set up timeout alrm trap */
  signal(SIGTERM, pfio_timeout_term);    /*set up timeout term trap */
  if(sigsetjmp(pfio_timeout_env,1) == 0) {
    alarm(timeout);                          /*set alarm timeout   */
    istat=pthread_create(&pfio_pthread_read_timeout,NULL,
                          pfio_read_data_sub, (void*) &time_out);
    istat=pthread_join(pfio_pthread_read_timeout, NULL);
    alarm(0);
    signal(SIGALRM, SIG_DFL);
    istat=time_out.stat;
  } else {
    istat=-2;
  }

  signal(SIGTERM, SIG_DFL);
  return(istat);
}
/********** END OF EXPERIMENTAL CODE ************/

/************************ PFIO_ACTUAL_WRITE *******************
*       pfio_actual_write writes to a pf file
*         ifile = file id of file to write
*         iext = extent number
*         i_seek = offset to write at
*         nbytes=number of bytes to write
*         buff=buffer to write data from
*         returns <0 if error
*         return >=0 if no error and number bytes written
*
*       Written November 2000 by Charles C Burch
**************************************************************/
int32_t pfio_actual_write(int ifile, int i_ext, int64_t i_seek,
  char *buff, int32_t nbytes) {
  int32_t istat;


  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,
     "pfio_actual_write: ifile=%d, ext=%d, off=%"PRId64", n=%"PRId32", ts=%f\n",
     ifile, i_ext, i_seek, nbytes, (float)unix_get_wtime_stamp_c());
  }
  istat=bfio_write(file_info[ifile]->ext_info[i_ext].host_ifile,i_seek,
    buff,nbytes);
  /*
  if(istat < 0 )
    istat=pfio_write_error_recover(ifile,i_ext,i_seek,buff,nbytes,istat);
  */

  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_actual_write-exit:%"PRId32", ts=%f\n",
        istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/********************* PFIO_WRITE_ERROR_RECOVERY *****************
*    This code is for synchronous IO, and NOT asynchronous
*       pfio_write_error_recover
*         ifile = file id of file to write
*         iext = extent number
*         i_seek = offset to write at
*         nbytes=number of bytes to write
*         buff=buffer to write data from
*         istat=error from last disk write
*
*         returns <0 if error (fromlast disk write)
*         return >=0 if no error and number bytes written
*
**** Note-This code is not active-it REQUIRES AYNCHRONOUS IO ***
*
*       Written February 2001 by Charles C Burch
*****************************************************************/
int32_t pfio_write_error_recover(int ifile, int i_ext, int64_t i_seek,
 char *buff, int32_t nbytes, int32_t istat) {

  int     jpos, disk_num, i_node, disk_num_1, i_node_1;
  char    fn_ext[260], base_fn[260], node[PFIO_MAX_NODENAME],
          new_base_fn[260], disk_dir[260];
  char    work[64000];
  int32_t j_seek, n, n1, last_stat, n_size;
  FILE    *fd1 = NULL, *fd2;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_write_error_recovery, ts=%f\n",
        (float)unix_get_wtime_stamp_c());
  }
  if(file_info[ifile]->write_err_recover==0) return(istat);

  pfio_form_fn_ext(file_info[ifile]->fn,i_ext, fn_ext); /*form extent filename*/
  sprintf(work,"attempt write error(%"PRId32") recovery  on file:%s",
    istat,fn_ext);
  cpslog_message(work);                  /*log write error*/

  jpos=pfio_check_if_cpsdisk(fn_ext, &disk_num);     /*see if cpsdisk        */
  /*** fprintf(stderr,"disk_num=%d\n",(int)disk_num); ***/
  if(disk_num<0) return(istat);       /*return old stat if not cpsdata/cpstemp*/
  /*n=readlink(fn_ext, base_fn, sizeof(base_fn));*/
  n=bfio_readlink_to_end(fn_ext, base_fn, sizeof(base_fn));
  /*get file name from link*/
  /*** fprintf(stderr,"readlink=%d\n",(int)n); ***/
  if(n<=0) return(istat);                     /*return last error if link bad*/


  base_fn[n]='\0';                     /*get node used from actual file name*/
  /*** fprintf(stderr,"base fn=%s\n",base_fn); ***/
  i_node=pfio_get_node_from_filename(base_fn, node, work, disk_num);
  /*** fprintf(stderr,"inode=%d\n",i_node); ***/

  strcpy(work, "write error was actually on file:");
  strcat(work, base_fn);
  cpslog_message(work);     /*log which file error happened*/

  disk_num_1=disk_num;
  /*get new disk node*/
  i_node_1=pfio_alloc_file_node(node,disk_dir,&disk_num_1,0);
  fprintf(stderr,"%d:new node=%d\n",__LINE__,i_node_1); 

  /*return last error if unable to get node or same as old file*/
  if(i_node_1<0) return(istat);
  if(disk_num==disk_num_1 && i_node==i_node_1) return(istat);

  /* --- form new file name and ensure all sub directories exist ---*/
  pfio_form_cpsdisk_linked_name(disk_dir, fn_ext, jpos, new_base_fn);
  /*** fprintf(stderr,"new name=%s\n",new_base_fn); ***/
  bfio_ensure_dir_exists(new_base_fn,strlen(disk_dir));

  pfio_close_file_extent(ifile, i_ext);   /*close file extent that had error*/
  n_size=bfio_file_size(base_fn);                         /*get size to copy*/
  if(n_size>i_seek && n_size<=(i_seek+nbytes)) n_size=i_seek;

  /* ---open new file name, return error if can not open */
  if((fd2=mtio_fopen(new_base_fn,"w+b"))==NULL) return(istat);

  if(n_size>0) {                            /*copy old file info to new file*/
    if((fd1=mtio_fopen(base_fn,"rb"))==NULL) goto new_write_error;
    j_seek=0;
    while(j_seek<n_size){    /*copying */
      n=n_size-j_seek;
      if(n>65000) n=65000;
      n1=mtio_fread (work, 1, n, fd1);
      if(n1==n) n1=mtio_fwrite(work, 1, n, fd2);
      if(n1!=n) goto new_write_error;
      j_seek+=n;
    }
    mtio_fclose(fd1);
  }
  /* --- write data that created original error ---*/
  mtio_fseek(fd2,i_seek,SEEK_SET);
  last_stat=mtio_fwrite(buff,1,nbytes,fd2);
  /*** fprintf(stderr,"last_stat=%d\n",(int)last_stat); ***/
  if(last_stat!=nbytes) goto new_write_error;

  mtio_fclose(fd2);                                         /*close new file */
  pfio_remove_file(base_fn);                            /*remove the old file*/
  /*pfio_delete_empty_directories(base_fn,0); */
  remove(fn_ext);                /*remove the link for the old file*/

  symlink(new_base_fn,fn_ext);    /*link the file extent to the new file name*/
  pfio_open_file_extent(ifile,i_ext);        /* open the extent with new file*/

  strcpy(work,"write error recovery successful with file:");
  strcat(work,new_base_fn);
  cpslog_message(work);      /*log write error recovery good*/

  return(last_stat);       /*return the write status of write of current data*/

new_write_error: /*error on new file-close files-remove new file-return error*/
/*** fprintf(stderr,"new write error\n"); ***/
  mtio_fclose(fd1);
  mtio_fclose(fd2);
  pfio_remove_file(new_base_fn);
/*pfio_delete_empty_directories(new_base_fn,0); */
  return(istat);
}

/**********************Reserve support routines**********************/

/********************* PFIO_GET_NODE_RESERVES *********************
* get number of reserves_disk space ocurring on given node
* mode=0-no check on dead processes, 1-check on dead processes
*
* Written July 2002 by Charles C Burch
******************************************************************/
int pfio_get_node_reserves(char *reserve_node){
  /* wmm don't use mode anymore.  we don't use lockfile except for netapps. */
  int    n_lockfile_recs, i_rec, n_reserves, num_lock_files, ifile;
  char   node[PFIO_MAX_NODENAME], lockfile[260], work[260],cmd[132],line[132];
  char   *p_date, *p_time, *expire_time, locktype, *host, *pid, *file;
  int    is_netapps=0;
  struct bfio_lock_struct lock_parms;
  FILE   *fp;

  n_reserves=0;

  if(pfio_init_sw==0) pfio_init();
  /****** This will cause the routine to return unless on LD or TT nodes *****/
  if(
     (strstr(reserve_node,"ld") == NULL ) &&
     (strstr(reserve_node,"tt") == NULL ) ){
     /*
     strcpy(work,"");
     sprintf(work,"Do not care about reserves on node %s\n",reserve_node);
     cpslog_message(work);
     */
     return n_reserves;
  }

  strcpy(node,"");
  strcpy(lockfile,"");
  strcpy(work,"");
  strcpy(cmd,"");
  strcpy(line,"");

  /*** Try to log in to the node and see how many reserves are running.
       If we get a good answer, exit without touching log file.  If not,
       continue with old logic.
  ***/

  strcpy(work,"rsh_timeout ");
  strcat(work,reserve_node);

  strcpy(cmd,"rsh_timeout ");
  strcat(cmd,reserve_node);
  strcat(cmd," 15 \" uname -s\" 2>/dev/null");
  if((fp=popen(cmd,"r")) != NULL){
    fgets(line,sizeof(line),fp); /* no mtio call on popen file */
    pclose(fp);
    /*cpslog_message(line);*/
    if(strstr(line,"Linux") !=NULL ){
      strcat(work," 45 \"ps -auxw\" ");
      strcat(work,"2>/dev/null |awk '{print $11}' |grep filereserve");
    } else {
      if(strstr(line,"IRIX64")!=NULL ){
        strcat(work," 45 \"ps -Af\" ");
        strcat(work,"2>/dev/null |awk '{print $8 $9}' |grep filereserve");
      } else {
        is_netapps++;
      }
    }
  }
  /*cpslog_message(work);*/

  if(is_netapps == 0 ) {
    /* I must be on Linux or SGI disk server.  Look at running procs. */
    if((fp=popen(work,"r"))!=NULL) {
      while(fgets(line,sizeof(line),fp)!=NULL) {/*no mtio calls on popen file*/
        /*cpslog_message(line);*/
        if(strstr(line,"TIMEOUT") != NULL) n_reserves+=max_reserves_per_node-1;
        /* (above line) If timed out, the server is too busy for another res.*/
        n_reserves++;
        /*
        sprintf(work,"%s %s\n",reserve_node,line);
        cpslog_message(work);
        */
      }
      pclose(fp);
    }
    /*
    if(n_reserves >0) {
      strcpy(work,"");
         sprintf(work,
         "reserve_node=%s n_reserves=%d\n",
         reserve_node,n_reserves);
         cpslog_message(work);
    }
    */
    return(n_reserves);
  }
  /* I must be on a netapps, since SGI and Linux didn't hit */
  /* Since I can't log in, I must look in lock files to see about reserves*/
  /*cpslog_message("in the section where I think I am a netapps."); */
  /*** must lock all files to ensure atomic transaction ***/

  strcpy(lock_parms.file_name,"pfio_get_node_reserves");
  num_lock_files=bfio_adjust_lockfile_name(cps_lock_file, lockfile, 0);

  for(ifile=0; ifile<num_lock_files; ifile++) {
    bfio_adjust_lockfile_name(cps_lock_file, lockfile, ifile);
    lock_parms.fd=bfio_lock_the_lockfile(lockfile, lock_parms.file_name,
                  lock_parms.lock_file);
    n_lockfile_recs=bfio_read_lock_file(lock_parms.file_name, &lock_parms);
    /* lock file is now in memory in lock_parms.buff, which was allocated
       in the bfio_read_lock_file call
    */
    bfio_unlock_the_lockfile(lock_parms.fd);

    for (i_rec=0; i_rec<n_lockfile_recs; i_rec++) {
      bfio_get_lock_file_rec(&lock_parms, i_rec, &p_date, &p_time,
                             &expire_time,
                             &locktype, &host, &pid, &file);
      if(locktype=='R' && file[0]!='\0') { /* active reserve happening?    */
        is_netapps = pfio_get_file_info(file, work, node);
        /* is_netapps is being used above as a dummy variable */
        if(strcmp(node,reserve_node)==0) {  /* happening in reserve_node?  */
          if(strstr(work,"CPSTEMP") != NULL ) {  /* then cpstemp file */
            if(strcmp(node,host)==0)       { /* ... and on same node as host*/
              /*                                see if maximum reserves hit */
              n_reserves++;
            }
          } else {                           /* not cpstemp */
              n_reserves++;
          }
        }
      }
    }
    free(lock_parms.buff);
  }

  /*
  if(n_reserves >0) {
      strcpy(work,"");
      sprintf(work,
      "reserve_node=%s n_reserves=%d\n",
      reserve_node,n_reserves);
      cpslog_message(work);
  }
  */
  return(n_reserves);
}

/**************** PFIO_LIMIT_ACTIVE_RESERVES *********************
*  Limit number of active reserve disk space on a given node
*  wait for some to finish if need be
*
*  Written  July 2002 by Charles C Burch
******************************************************************/
void pfio_limit_active_reserves(char *reserve_node) {
  int i_sleep, i_wait;
  char work[260];

  if(pfio_init_sw==0) pfio_init();
  /*** fprintf(stderr,"max reserves=%d, reserves(%s)=%d\n",
   max_reserves_per_node, reserve_node,
   pfio_get_node_reserves(reserve_node)); ***/

  i_sleep=0;
  i_wait=0;
  while(pfio_get_node_reserves(reserve_node)>=max_reserves_per_node) {
    if(i_sleep==0) {
      sprintf(work,
        "Warning: Maximum Reserves per node occurring on node(%s)-Waiting",
        reserve_node);
      cpslog_message(work);
    }
    if(i_sleep<30) i_sleep+=10;
    i_wait+=i_sleep;
    sleep(i_sleep);
  }
  if(i_wait>0) {
    sprintf(work,"Warning: Max Reserves/node wait completed after %d seconds",
          i_wait);
    cpslog_message(work);
  }
  return;
}

/********************** PFIO_RESERVE_CHECK **************************
* Checks if reserve disk thread working on ifile
*   if so wait for it to complete
*
* Written Decemeber 2001 by Charles C Burch
*********************************************************************/
void pfio_reserve_check(int ifile) {
  int istat;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_reserve_check: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }

  if(file_info[ifile]->reserve_info==NULL) goto exit;
  if(file_info[ifile]->reserve_info->ifile!=ifile) goto exit;
  if(file_info[ifile]->reserve_thread == (pthread_t) NULL ) goto exit;

  if((istat=pthread_join(file_info[ifile]->reserve_thread,NULL))!=0) {
    sprintf(pfio_error_string,
     "Error with pthread_join in pfio_reserve_check(%d) [%d-%s]\n",
     istat, errno, strerror(errno));
    unix_abort_c(pfio_error_string);
  }
  file_info[ifile]->reserve_thread = (pthread_t) NULL;

  if(file_info[ifile]->ext_info[file_info[ifile]->reserve_info->ext].size<0){
    file_info[ifile]->ext_info[file_info[ifile]->reserve_info->ext].size=
     pfio_current_file_size(file_info[ifile]->reserve_info->fn);
  }
  file_info[ifile]->reserve_info->ifile=-1;

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_reserve_check-exit, ts=%f\n",
        (float)unix_get_wtime_stamp_c());
  }
  return;
}

/********************** PFIO_RESERVE_SUB ****************************
* Thread code to reserve disk space
**************** Note This code must be thread safe *****************
*`
* Written December 2001 by Charles C Burch
*********************************************************************/
void* pfio_reserve_sub(void *ptr) {
  struct reserve_struct *reserve;
  int istat,jstat;
  char work[260],lockfile[260];

  reserve=(struct reserve_struct*) ptr;
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_reserve_sub: nd=%s, fn=%s, bs=%s, sz=%"PRId64", ts=%f\n",
     reserve->ext_node, reserve->fn, reserve->base_fn, reserve->size,
     (float)unix_get_wtime_stamp_c());
  }
  if(pfio_init_sw==0) pfio_init();
  istat=0;
  strcpy(work,"");
  strcpy(lockfile,"");
  /* if local reserve makes more sense, do that preferentially */
  /************** local  ****************/
  if(strcmp(reserve->ext_node,"") == 0 ||
     strcmp(reserve->ext_node,"localhost") == 0 ||
     strcmp(reserve->ext_node,reserve->host) == 0 ) {
     pfio_local_reserve_file_space(reserve->fn,reserve->base_fn,&reserve->size);
     goto EXIT;
  }
  /* disable remote reserving.  16 March 2010 wmm */
  /* the local machine will need to be able to reserve disk space via NFS */
  pfio_local_reserve_file_space(reserve->fn,reserve->base_fn,&reserve->size);
  goto EXIT;

  /************** remote ***************/
    sprintf(work,"%s reserving disk space (%"PRId64":%s)",
    reserve->ext_node, reserve->size, reserve->fn);
    cpslog_message(work);

    /*** this remote command will reserve file space on the remote nodes
     *** but if it fails, will return a code < 0.  A netapps will return
     *** with <0 if it fails also.
     ***/

    /*** First look to see if on a Netapps node ***/
    if(pfio_check_if_netapp_node(reserve->ext_node)) {
      /*sprintf(work,"need to reserve on netapps %s\n",reserve->ext_node);*/
      /*cpslog_message(work);*/
      sprintf(work,"%s %s %"PRId64"",reserve_file_space_na,
       reserve->base_fn, reserve->size);
      if ( (jstat=system(work)) == 0 ) {
        bfio_remount_file_disk( reserve->ext_node);  /*force remount if needed*/
        istat=pfio_check_reserve_file_space_size(reserve->fn, reserve->size,
         work);
        cpslog_message(work);
      } else {
        sprintf(work,"Error: Remote reserve_file_space on netapps %s fail:%d",
         reserve->ext_node, jstat);
        cpslog_message(work);
        if(pfio_current_file_size(reserve->fn)>1) istat=1;
      }

    } else {
      /*** Next look to see if on a non-netapps node ***/
      /*sprintf(work,"need to reserve on ld-node %s\n",reserve->ext_node); */
      sprintf(work,"%s %s %"PRId64"",
      reserve_file_space, reserve->base_fn, reserve->size);
      if(pfio_remote_command(reserve->ext_node,work)>=0) {
        bfio_remount_file_disk( reserve->ext_node);  /*force remount if needed*/
        istat=pfio_check_reserve_file_space_size(reserve->fn, reserve->size,
         work);
        cpslog_message(work);
      } else {
          sprintf(work,"Error: Unable to rsh reserve_file_space on node %s",
           reserve->ext_node);
          cpslog_message(work);
          if(pfio_current_file_size(reserve->fn)>1) istat=1;
      }
    }
  
EXIT:
  reserve->done=1;  /*This tells main program that the reserve is done*/
  pfio_unlock_file(lockfile, reserve->fn);
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_reserve_sub-exit: fn=%s, ts=%f\n",
        reserve->fn, (float)unix_get_wtime_stamp_c());
  }
  return(ptr);
}

/********************** PFIO_RESERVE_FILE_SPACE ***************
* force a file to be a certain size and reserve the space
* Either call pfio_reserve_file_space_sync or pfio_reserve_file_space_no_sync
*
* Written December 2000 by Charles C Burch
**************************************************************/
int pfio_reserve_file_space( char *fn, int64_t *n_bytes) {
  int istat;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"PFIO_Reserve_file_space: fn=%s, n=%"PRId64", ts=%f\n",
        fn, *n_bytes, (float)unix_get_wtime_stamp_c());
  }

  istat=0;                   /* use no_sync  */
  if(istat==0) {
    istat=pfio_reserve_file_space_sync(fn,n_bytes);
  } else {
    istat=pfio_reserve_file_space_no_sync(fn, n_bytes);
  }
  return(istat);
}
/**************** PFIO_RESERVE_FILE_SPACE_NO_SYNC ****************
* reserve the space for a given file using asyncronous buffered IO
*
* Written February 2002 by Charles C Burch
*****************************************************************/
int pfio_reserve_file_space_no_sync( char *fn, int64_t *n_bytes) {
/* #define BUFFSZ 262144     */    /*256*1024*/
#define RESERVE_BUFFSZ 4194304
  char *buff;
  size_t n_write, i;
  FILE *fd;
  int istat;
  int64_t n_bytes_left;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_reserve_file_space_no_sync: fn=%s, n=%"PRId64", ts=%f\n",
        fn, *n_bytes, (float)unix_get_wtime_stamp_c());
  }

  if((buff=(char*)malloc((size_t) RESERVE_BUFFSZ))==NULL) {
    fprintf(stderr,
      "Unable to malloc buff in pfio_reserve_file_no_sync [%d:%s]\n",
      errno, strerror(errno));
    unix_abort_c("buff malloc error");
  }
  memset(buff,0,(size_t) RESERVE_BUFFSZ);

  if(bfio_file_size(fn)<0) { /* then the file doesn't exist */
    fd=mtio_fopen(fn,"w");   /* so we open it "new" (truncated) */
  } else {
    fd=mtio_fopen(fn,"r+");  /* Otherwise, we open it in update mode*/
  }

  if(fd==NULL) { /* then no file was opened. */
    istat=-2;
  } else {
    istat=0;
    mtio_fseek(fd,0,SEEK_END); /* put cursor at end of file */
    n_bytes_left = *n_bytes;
    n_bytes_left -=(int64_t)mtio_ftell(fd);/* Reduce number of bytes to write
                                             by the size of the file */
    while(n_bytes_left>0) {                  /* Continue to write chunks to the
                                             file to reserve the file space*/
      n_write= (size_t) RESERVE_BUFFSZ;
      if((int64_t) n_write > n_bytes_left)n_write=(int32_t)n_bytes_left;
                                             /*reduce the
                                             number of bytes to write to no
                                             more than the remaining bytes
                                             needed to finish the requested
                                             size. */
      i=mtio_fwrite(buff, 1, n_write, fd);
      if(i != n_write) {
        istat=-3;
        break;
      }
      n_bytes_left -= (int64_t)n_write;
    }
    /* go to end of file */
    mtio_fseek(fd,0,SEEK_END);
    *n_bytes = mtio_ftell(fd);
    mtio_fclose(fd);
  }
  free(buff);
  return(istat);
}

/***************** PFIO_RESERVE_FILE_SPACE_SYNC ***************
* reserve the space for a given file using synchronous IO
*
* Written February 2002 by Charles C Burch
**************************************************************/
int pfio_reserve_file_space_sync(char *fn, int64_t *n_bytes) {
/* #define BUFFSZ 262144        */ /*256*1024*/
#define RESERVE_BUFFSZ 4194304
  char *buff;
  size_t n_write, i;
  FILE *fp;
  off_t n_size;
  int return_code = 0;
  if(pfio_init_sw==0) pfio_init();
  buff = (char *) malloc((size_t) RESERVE_BUFFSZ);

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_reserve_file_space_sync: fn=%s, n=%"PRId64", ts=%f\n",
        fn, *n_bytes, (float)unix_get_wtime_stamp_c());
  }
  memset(buff,0,(size_t) RESERVE_BUFFSZ);

  /*if((fd=open(fn,O_WRONLY|O_CREAT|O_SYNC))<0)  return (-2) */
  /*n_size=lseek(fd,0,SEEK_END);*/

  if( (fp=mtio_fopen(fn,"w+")) == NULL ) {
    return_code -= 2;
    goto exit;
  }

  if(mtio_fseek(fp,0,SEEK_END) <0 ) {
    return_code -=1;
    goto exit;
  }
  if((n_size = (off_t) mtio_ftell(fp)) <0) {
    return_code -=1;
    goto exit;
  }

  *n_bytes -= n_size;
  while(*n_bytes > (int64_t)0) {
    n_write= (size_t) RESERVE_BUFFSZ;
    if(n_write > *n_bytes) n_write = (size_t) *n_bytes;
    i = mtio_fwrite(buff,1,n_write,fp);
    if(i != n_write) {
      return_code -=3;
      goto exit;
    }
    *n_bytes -= (int64_t)n_write;
  }

exit:
  mtio_fseek(fp,0,SEEK_END);
  *n_bytes = mtio_ftell(fp);
  if( mtio_fclose(fp) < 0 ) return_code -= 1;
  free(buff);
  return(return_code);
}

/************* PFIO_CHECK_RESERVE_FILE_SPACE_SIZE *************
* locally force a file to be a certain size and reserve the space
*
*       Written December 2001 by Charles C Burch
***************************************************************/
int pfio_check_reserve_file_space_size(char *fn, int64_t nbytes, char *msg) {
  int64_t flsz;
  int  istat;

  if(pfio_init_sw==0) pfio_init();
  flsz=bfio_file_size(fn);
  if(flsz==nbytes) {
    istat=1;
    sprintf(msg,"----- Reserve file space done (%s)", fn);
  } else if(flsz>0) {
    istat=1;
    sprintf(msg,"----- Reserve file space incorrect size (%"PRId64":%s)",
     flsz,fn);
  } else {
    istat=0;
    sprintf(msg,
      "----- Reserve file space failed-file not created (%s)", fn);
  }
  return(istat);
}

/**************** PFIO_LOCAL_RESERVE_FILE_SPACE ****************
* locally force a file to be a certain size and reserve the space
*
*       Written December 2001 by Charles C Burch
***************************************************************/
int pfio_local_reserve_file_space(char *fn, char *base_fn, int64_t *n_bytes) {
  char msg[260];
  int istat;
  int64_t n_bytes_internal;

  if(pfio_init_sw==0) pfio_init();
  sprintf(msg,"Reserving disk space (%"PRId64":%s)",*n_bytes,fn);
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_local_reserve_file_space: fn=%s, n=%"PRId64", ts=%f\n",
        fn, *n_bytes, (float)unix_get_wtime_stamp_c());
    pfio_debug_check(1,0);
    fprintf(stderr,"pfio_local_reserve_file_space: msg=%s\n",msg);
  }
  cpslog_message(msg);

  n_bytes_internal = *n_bytes;
  pfio_reserve_file_space_no_sync(base_fn,&n_bytes_internal);

  bfio_remount_file_disk(base_fn);   /*force remount if needed*/
  istat=pfio_check_reserve_file_space_size(fn, *n_bytes, msg);

  cpslog_message(msg);
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_local_reserve_file_space-exit: msg=%s, ts=%f\n",
        msg, (float)unix_get_wtime_stamp_c());
  }
  *n_bytes = n_bytes_internal;
  return(istat);
}

/*********************Operation Trace Support routines ******************/

/******************* PFIO_TRACE_DUMP ***********************
* dump the areas accessed with the trace information
*  ifile is file number rw=r or w, beg/end area to dump
*
* Written March 2001 by Charles C Burch
***********************************************************/
int pfio_trace_dump(char *file_name_in, int64_t beg, int64_t end){
  char *line1, *flnm1, type, flnm[260], file_name[260];
  int ifile;
  struct lnklst_struct *ptr, *files[2];

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  str_compress(file_name, file_name_in);
/* form list of matches in trace with filename */
  files[0]=NULL;
  files[1]=NULL;

  ptr=trace_entries[0];
  lnklst_scan_list(&ptr, &line1,&flnm1);
  while(line1!=NULL){
    if(line1[0]=='o') {
      sscanf(line1,"%c %d %c %s",&type, &ifile, &type, flnm);
      if(flnm[str_find_str(flnm,0,file_name)]!='\0'|| file_name[0]=='\0') {
        if(lnklst_search_list(files,flnm)<0) {
          lnklst_put_list_entry(files,flnm,"");
        }
      }
    }
    lnklst_scan_list(&ptr, &line1,&flnm1);
  }

  if((ptr=files[0])==NULL) {     /*see if search was empty */
    fprintf(stderr,"trace_dump file search was empty\n");
    return(0);
  }

/* search was not empty, now dump infor for search items */

  lnklst_scan_list(&ptr, &line1,&flnm1);
  while(line1!=NULL){
    pfio_trace_dump1(line1,'w',beg,end);
    pfio_trace_dump1(line1,'r',beg,end);
    lnklst_scan_list(&ptr, &line1,&flnm1);
  }

  lnklst_delete_list(files);
  return(1);
}
/*********************PFIO_TRACE_DUMP1 *************************
* slave dump routine for pfio_trace_dump
*
* Written March 2001 by Charles C Burch
****************************************************************/
int pfio_trace_dump1(char *file_name, char rw, int64_t beg, int64_t end){
  char *buff, line[260], flnm[260], r_w, *line1, *flnm1;
  int64_t beg1, end1, tot_bytes;
  int jfile, i, j, files[257], ntimes, open_sw;
  int32_t n, n1, n_tot;
  struct lnklst_struct *ptr;
  if(pfio_init_sw==0) pfio_init();
/******************cygwin patch
  int32_t n2;

  fprintf(stderr,"cygwin bug fix in\n");
*******************************/

  if((end-beg)>999999) {
    fprintf(stderr,"dump range can only be 1000000, end value adjusted\n");
    end=beg+999999;
  }
  n=(int32_t)(end-beg+1);
  if((buff=(char*)malloc(n))==NULL) {
    fprintf(stderr,"Unable to malloc work space in pfio_trace_dump\n");
    return(-1);
  }
  memset(buff,0,n);
  ntimes=0;
  n_tot=0;
  tot_bytes=0;
  open_sw=0;

  ptr=trace_entries[0];
  lnklst_scan_list(&ptr, &line1,&flnm1);
  while(1) {

    if(line1==NULL) {
      break; /* at end of list */

    } else if(line1[0]=='c' && ntimes>0) {
/* action is close  see if for current file */
      sscanf(line1,"%1c %d", &r_w, &jfile);
      for (i=0; i<ntimes;i++) {
        if(files[i]==jfile) {
          files[i]=files[ntimes-1];
          ntimes--;
          break;
        }
      }
    } else if(line1[0]=='o') {
/* see if current action is open and for current file */
      sscanf(line1,"%c %d %c %s",&r_w, &jfile, &r_w, flnm);
      if(strcmp(flnm,file_name)==0) {
        if(r_w==rw || r_w=='u') {
          open_sw=1;
          files[ntimes++]=jfile;
        }
      }

    } else if(line1[0]==rw && ntimes>0)  {
/* see if current actio r/w and for currenbt file stats */
      r_w=' ';
      jfile=-1;
      sscanf(line1,"%1c %d",&r_w, &jfile);
      for (i=0;i<ntimes;i++) {
        if(files[i]==jfile) {
          beg1=-1;
          n1=-1;
          sscanf(line1,"%1c %d %"PRId64" %"PRId32"",&r_w, &jfile, &beg1, &n1);
/**********cygwin patch
          sscanf(line1,"%1c %d %"PRId32" %"PRId32"",&r_w, &jfile, &n2, &n1);
          beg1=n2;
*********************************/
          end1=beg1+n1-1;
          n_tot++;
          tot_bytes+=n1;
          if(beg1<beg) beg1=beg;
          if(end1>end) end1=end;
          n1=beg1-beg;
          while((beg1++)<=end1) buff[n1++]++;
        }
      }
    }

/* go to next trace entry */
    lnklst_scan_list(&ptr, &line1,&flnm1);
  }

/* See if file trace info is to be displayed */
  if(open_sw!=0) {

    if(rw=='r') {
      strcpy(line,"read");
    } else {
      strcpy(line,"write");
    }
    fprintf(stderr,"pfio trace %s dump for file(%s) from %"PRId64" to %"PRId64"\n",
      line,file_name,beg,end);

    if(rw=='r') {
      strcpy(line,"read");
    } else {
      strcpy(line,"written");
    }

    i=0;
    while(i<n){
      j=i++;
      while(buff[j]==buff[i] && i<n) i++;
      if(buff[j]==0) {
        fprintf(stderr,"  area not %s=%"PRId64" to %"PRId64"\n",line,beg+j,beg+i-1);
      } else if(buff[j]==1) {
        fprintf(stderr,"  area %s once=%"PRId64" to %"PRId64"\n",line,beg+j,beg+i-1);
      } else {
        fprintf(stderr,"  area %s %d times =%"PRId64" to %"PRId64"\n",
         line,(int)buff[j],beg+j,beg+i-1);
      }
    }
    if(rw=='r') {
      strcpy(line,"reads");
    } else {
      strcpy(line,"writes");
    }
    n1=n_tot; if(n1==0) n1=1;
    fprintf(stderr,"Stats: number %s=%"PRId32", #bytes=%"PRId64", average=%"PRId64"\n\n",
      line,n_tot,tot_bytes, (int64_t)(tot_bytes/n1));
  }

  free(buff);
  return(0);
}

/************************** Debug support routines ***********************/

/****************** PFIO_DEBUG_CHECK **********************
* checks if debug active and idents blanks or flushed
* if level>=pfio_debug_mode return 1 else 0
* if results is true--print leading indentation
* if mode  >0 increase indentation
* if(mode ==0 leave indentation alone
* if mode  <0 decrease identation
*
* Written December 2001 by Charles C Burch
***********************************************************/
int pfio_debug_check(int debug_level, int mode) {
  static int level=0, old_mode=0;
  char blanks[42];

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_mode<debug_level) return(0);

  if(old_mode<0) {
    if((--level)<0) level=0;
    old_mode=0;
  }
  if(old_mode>0) {
    if((++level)>40) level=40;
  }

  old_mode=mode;

  memset(blanks,' ',41);
  blanks[level]='\0';
  fprintf(stderr,"%s",blanks);
  return(1);
}

/* Now for user callable pfio routines:*/

/*********************** PFIO_SET_DEBUG_MODE ***********************
*  Set pfio_debug_mode isw=0 means bypasss debug
*
*  Written December 2001 by Charles C Burch
********************************************************************/
void pfio_set_debug_mode(int isw) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_debug_mode: isw=%d, ts=%f\n",
        isw, (float)unix_get_wtime_stamp_c());
  }
  pfio_debug_mode=isw;
  return;
}

/************************Remote IO support routines***********************/

/** NOTE THIS ROUTINE CAN BE REMOVED AFTER ITS CALLS ARE REMOVED **/

/*********************** PFIO_SET_REMOTE_ACCESS ********************
*  Set pfio_remote_sw   isw=0:socket io off, 1: socket io on
*
*  Written August 2000 by Charles C Burch
********************************************************************/
void pfio_set_remote_access(int isw) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_remote_access: isw=%d, ts=%f\n",
        isw, (float)unix_get_wtime_stamp_c());
  }
  /*  pfio_remote_sw=isw;  make this routine a no op */
  return;
}


/*********************Actual user callable routines**********************/

/************* PFIO_INIT *****************
* initialize pfio variables
*
* Written November 2000 by Charles C Burch
*****************************************/
void pfio_init() {
  int  ifile;
  char *s;
  int num_threads=0,num_in_queue, status;
  struct stat    *fd     =NULL;
  char           *tmpname=NULL;
  FILE           *file   =NULL;
  mode_t mode;
  int  n,i;
  char        str[200];
  char this_project[32],dummy[100];
  char *project;
  /*int64_t temp;*/

  if(pfio_init_sw==1) return;
  pfio_init_sw=1;

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_init, ts=%f\n", (float)unix_get_wtime_stamp_c());
  }


  /*** This tells mtio how many write behind threads to use ***/
  #ifdef __sgi
     num_threads += sysconf(_SC_NPROC_CONF);
  #else
     num_threads += sysconf(_SC_NPROCESSORS_CONF);
   #endif
  if(num_threads < 3) num_threads = 3;
  /*** This tells mtio how large to make the i/o write-behind queue ***/
  num_in_queue=1024/num_threads;
  if(mtio_init(&num_threads,&num_in_queue) != 0 ) {
    fprintf(stderr,"pfio_init: Error initializing mtio\n");
  }

  pfio_error_string[0]='\0';         /*make last error empty*/
  /* We need to initialize the random number generator for disk randomness */
  pfio_pid=getpid();
  srand((unsigned int) (time(NULL)*pfio_pid));

  if(unix_get_hostname_c(pfio_host,40)<0) {
    unix_abort_c("Error in unix_get_hostname_c in pfio_init");
  }

  if(pfio_get_user_name(pfio_user_name,25)<0) {
    unix_abort_c("Error in pfio_get_user_name in pfio_init");
  }

  /*** add code here to make the directory /tmp/hostname if it doesn't
       exist (wmm)
  ***/
  tmpname = (char *) malloc(strlen(pfio_host)+6);
  fd  = (struct stat *) malloc((size_t) sizeof(struct stat) );
  strcpy(tmpname,"/tmp/\0");
  strcat(tmpname,pfio_host);
  status=stat(tmpname,fd);
  if( ! S_ISDIR(fd->st_mode) ) {
    /*** create directory now ***/
    mode = S_IRWXU|S_IRWXG|S_IRWXO;
    status=mkdir(tmpname,mode);
    status=chmod(tmpname,mode);
    if(status != 0 ) {
      fprintf(stderr,"Unable to create cpstemp directory %s on this node.\n",
              tmpname);
    } else {
      fprintf(stderr,"Created cpstmp directory %s.\n",tmpname);
    }
  } else {
    /*** fprintf(stderr,"cpstmp directory %s exists.\n",tmpname);***/
  }
  /*** clean up temporary variables ***/
  free(tmpname);
  free(fd);

  strcpy(pfio_filescrub_cmd, cnfg_get_value_c("filescrub_script"));
  if(pfio_filescrub_cmd[0]=='\0' || pfio_filescrub_cmd[0]==' ') {
    /*
    fprintf(stderr,"filescrub_script not defined in cnfg file-default used\n");
    */
    strcpy(pfio_filescrub_cmd,"/usr/app/vendors/sps/scripts/filescrub");
  }
  strcat(pfio_filescrub_cmd," ");

/*** removed need for this when the socket layer was removed from code**********
* strcpy(pfio_server_cmd, cnfg_get_value_c("pfio_pfserver_command"));
* if(pfio_server_cmd[0]=='\0' || pfio_server_cmd[0]==' ') {
*   fprintf(stderr,
*     "Warning: pfio_pfserver_command not defined in cnfg file-default used\n");
*   strcpy(pfio_server_cmd, "/home/sps/production/programs/pfserver/pfserver");
* }
*******************************************************************************/
  pfio_default_extent_size=str_atoll(cnfg_get_value_c("pfio_file_extent_size"));
  if(pfio_default_extent_size==0) {
    /*
    fprintf(stderr,"pfio_extent_size not defined in cnfg file-default used\n");
    */
    pfio_default_extent_size=DEFAULT_EXTENT_SIZE;
  }
  pfio_file_extent_size=pfio_default_extent_size;
  pfio_extent_size=pfio_default_extent_size;

  s=cnfg_get_value_c("pfio_max_reserves_per_node");
  if(s[0]=='\0') {
    /*
    printf(
     "Warning: pfio_max_reserves_per_node not in cnfg file-default used\n");
    */
    max_reserves_per_node=6;
  } else {
    max_reserves_per_node=atoi(s);
  }

  pfio_max_io_rate=str_atol(cnfg_get_value_c("pfio_max_io_rate"));
  if(pfio_max_io_rate==0) {
    /*
    fprintf(stderr,"pfio_max_io_rate not defined in cnfg file-default used\n");
    */
    pfio_max_io_rate=200000000;
  }


  strcpy(cps_mess_lock_file,cnfg_get_value_c("cps_mess_lock_file"));
  strcpy(cps_main_host,cnfg_get_value_c("cps_main_host"));

  strcpy(cps_node_name1, cnfg_get_value_c("cps_node_name1"));
  strcpy(cps_node_name2, cnfg_get_value_c("cps_node_name2"));
  strcpy(cps_node_name3, cnfg_get_value_c("cps_node_name3"));
  strcpy(cps_node_name4, cnfg_get_value_c("cps_node_name4"));
  strcpy(cps_node_name5, cnfg_get_value_c("cps_node_name5"));
  strcpy(cps_node_name6, cnfg_get_value_c("cps_node_name6"));
  strcpy(cps_node_name7, cnfg_get_value_c("cps_node_name7"));
  strcpy(cps_node_name8, cnfg_get_value_c("cps_node_name8"));
  strcpy(cps_node_name9, cnfg_get_value_c("cps_node_name9"));
  strcpy(cps_node_name10,cnfg_get_value_c("cps_node_name10"));

  strcpy(netapp_node_name1, cnfg_get_value_c("netapp_node_name1"));
  if(netapp_node_name1[0]=='\0') strcpy(netapp_node_name1,"hoepld84");
  
  strcpy(netapp_node_name2, cnfg_get_value_c("netapp_node_name2"));
  if(netapp_node_name2[0]=='\0') strcpy(netapp_node_name2,"hoepld76");
  
  strcpy(netapp_node_name3, cnfg_get_value_c("netapp_node_name3"));
  if(netapp_node_name3[0]=='\0') strcpy(netapp_node_name3,"hoepld95");
  
  strcpy(netapp_node_name4, cnfg_get_value_c("netapp_node_name4"));
  if(netapp_node_name4[0]=='\0') strcpy(netapp_node_name4,"hoepld96");
  
  strcpy(netapp_node_name5, cnfg_get_value_c("netapp_node_name5"));
  if(netapp_node_name5[0]=='\0') strcpy(netapp_node_name5,"hoepld85");
  
  strcpy(netapp_node_name6, cnfg_get_value_c("netapp_node_name6"));
  if(netapp_node_name6[0]=='\0') strcpy(netapp_node_name6,"hoepld86");
  
  strcpy(netapp_node_name7, cnfg_get_value_c("netapp_node_name7"));
  if(netapp_node_name7[0]=='\0') strcpy(netapp_node_name7,"UNKNOWN?");
  
  strcpy(netapp_node_name8, cnfg_get_value_c("netapp_node_name8"));
  if(netapp_node_name8[0]=='\0') strcpy(netapp_node_name8,"UNKNOWN?");
  
  strcpy(netapp_node_name9, cnfg_get_value_c("netapp_node_name9"));
  if(netapp_node_name9[0]=='\0') strcpy(netapp_node_name9,"UNKNOWN?");
  
  strcpy(netapp_node_name10,cnfg_get_value_c("netapp_node_name10"));
  if(netapp_node_name10[0]=='\0') strcpy(netapp_node_name10,"UNKNOWN?");

  strcpy(cps_default_disk_node, cnfg_get_value_c("cps_default_disk_node"));
  if(cps_default_disk_node[0]=='\0') strcpy(cps_default_disk_node,"hoepld02");
  
  /* set up this project's name from environmental variable */
  if((project = getenv("CPS_PROJECT_NAME")) != NULL){
    strcpy(pfio_this_project_name,project);
    if(pfio_debug_check(2,0)) 
      fprintf(stderr,"pfio_info:CPS_PROJECT_NAME=[%s]\n",
        pfio_get_project_name());
  } else {
    strcpy(pfio_this_project_name,"\0");
    if(pfio_debug_check(2,0))
      fprintf(stderr,"pfio_info:CPS_PROJECT_NAME not set.\n");
  }

  cpsdisks[pfio_cpsdata_num].disk_info=NULL;
  strcpy(cpsdisks[pfio_cpsdata_num].name,"cpsdata");
  cpsdisks[pfio_cpsdata_num].file_time=0;
  cpsdisks[pfio_cpsdata_num].num_nodes=0;
  cpsdisks[pfio_cpsdata_num].reserve_mode='S';
  cpsdisks[pfio_cpsdata_num].exist_msg='N';
  strcpy(cpsdisks[pfio_cpsdata_num].file1,
      cnfg_get_value_c("cpsdata_nodes_file"));
  sprintf(cpsdisks[pfio_cpsdata_num].file2,"/home/%s/cpsdata_nodes.dat",
    pfio_user_name);
  if(strcmp(cpsdisks[pfio_cpsdata_num].file1,"\0")==0){
    strcpy(cpsdisks[pfio_cpsdata_num].file1,cpsdisks[pfio_cpsdata_num].file2);
  }

  if(pfio_debug_check(2,0)) 
    fprintf(stderr,"pfio_init: cpsdata_nodes_file = %s\n",
      cpsdisks[pfio_cpsdata_num].file1);

  /* load up list of projects from cpsdata_nodes_file */
  n = bfio_get_file_num_lines(cpsdisks[pfio_cpsdata_num].file1);
  /* read the file */
  if((file=mtio_fopen(cpsdisks[pfio_cpsdata_num].file1,"r"))==NULL){
    if((file=mtio_fopen(cpsdisks[pfio_cpsdata_num].file1,"w"))==NULL){
      /* fprintf(stderr,"cpsdata_nodes_file = %s\n",cpsdisks[pfio_cpsdata_num].file1); */
      unix_abort_c("Unable to read or create cpsdata_nodes_file in pfio_init.");
    } else {
      sprintf(str,"#Node\tMinMbytes\tDiskName\tBaseDir\tProject\n");
      mtio_fwrite(str,strlen(str),1,file);
      sprintf(str,
       "%s\t%d\t%s\t%s/\t%s\n",pfio_host,6000,getenv("HOMEMOUNT"),getenv("HOME"),
       pfio_get_project_name());
      mtio_fwrite(str,strlen(str),1,file);
      mtio_fclose(file);
      fprintf(stderr,"WARNING:  MISSING cpsdata_nodes.dat file created!\n");
      fprintf(stderr,
      "\t\tData will link to files in directory %s/%s/CPSDATA.\n",
      getenv("HOME"),pfio_user_name);
      if((file=mtio_fopen(cpsdisks[pfio_cpsdata_num].file1,"r"))==NULL){
        unix_abort_c("Unable to write a cpsdata_nodes_file in pfio_init.");
      }
    }
  }
  /* allocate memory (free this in pfio_exit) */
  pfio_valid_project_names = (char ** ) malloc (n*sizeof(char *));
  /* nullify pointers */
  for(i=0;i<n;i++)pfio_valid_project_names[i]=NULL;
  /* set first name to blank for the -none- project */
  n = 0;
  pfio_valid_project_names[n]=(char*)malloc(2*sizeof(char));
  strcpy(pfio_valid_project_names[n],"\0");
  /* read the data file, line by line */
  while(mtio_feof(file) == 0 ) {
    if(bfio_get_str(file,str,sizeof(str))>0) {
      /* skip blank or commented lines, check 1st char only! */
      if(str[0] == '#' || str[0] == ' ') continue;
      /* reset this_project to blank */
      strcpy(this_project,"\0");
      /* skip most fields, only read the project field (if exists) */
      sscanf(str,"%s %s %s %s %s",dummy,dummy,dummy,dummy,this_project);
      /* go through current list and see if a match exists, if so, move on.*/
      for(i=0;i<=n;i++){
        if(strcmp(this_project,pfio_valid_project_names[i])==0)goto match_found;
      }
      /* no match found, add this project to the list */
      if(pfio_debug_check(2,0))
        fprintf(stderr,
        "size=[%d] name=[%s]\n",(int) strlen(this_project),this_project);
      pfio_valid_project_names[++n]=
        (char*)malloc((1+strlen(this_project))*sizeof(char));
      strcpy(pfio_valid_project_names[n],this_project);
    }
    match_found: continue;
  }
  /* set the number of projects in the pfio constants for this run */
  pfio_number_of_projects = n+1;

  /* done scanning file, close it up */
  mtio_fclose(file);

  /*** clean up temporary variables ***/

  /*** diagnostic printouts ***/
  if(pfio_debug_check(2,0)) {
     fprintf(stderr,"PROJECT LIST : there are %d projects.\n",
       pfio_number_of_projects);
     for(i=0;i<pfio_number_of_projects;i++){
       fprintf(stderr,"pfio_init: Project (%d)=[%s]\n",i+1,
       pfio_valid_project_names[i]);
     }
  }

  pfio_project_name_changed=0; /* not important to set on init */
  cpsdisks[pfio_cpstemp_num].disk_info=NULL;
  strcpy(cpsdisks[pfio_cpstemp_num].name,"cpstemp");
  cpsdisks[pfio_cpstemp_num].file_time=0;
  cpsdisks[pfio_cpstemp_num].num_nodes=0;
  cpsdisks[pfio_cpstemp_num].reserve_mode='R';
  cpsdisks[pfio_cpstemp_num].exist_msg='N';
  strcpy(cpsdisks[pfio_cpstemp_num].file1,
      cnfg_get_value_c("cpstemp_nodes_file"));
  sprintf(cpsdisks[pfio_cpstemp_num].file2,"/home/%s/cpstemp_nodes.dat",
    pfio_user_name);
  if(strcmp(cpsdisks[pfio_cpstemp_num].file1,"\0")==0){
   strcpy(cpsdisks[pfio_cpstemp_num].file1,cpsdisks[pfio_cpstemp_num].file2);
  }

  if((file=mtio_fopen(cpsdisks[pfio_cpstemp_num].file1,"r"))==NULL){
    if((file=mtio_fopen(cpsdisks[pfio_cpstemp_num].file1,"w"))==NULL){
      unix_abort_c("Unable to read or create cpstemp_nodes_file in pfio_init.");
    } else {
      sprintf(str,"#Node\tMinMbytes\tDiskName\tBaseDir\n");
      mtio_fwrite(str,strlen(str),1,file);
      sprintf(str,"%s\t%d\t/tmp\t/tmp/%s/\n",pfio_host,6000,pfio_host);
      mtio_fwrite(str,strlen(str),1,file);
      mtio_fclose(file);
      fprintf(stderr,"WARNING:  MISSING cpstemp_nodes.dat file! Created...");
      fprintf(stderr," but only for local node %s.\n",pfio_host);
      if((file=mtio_fopen(cpsdisks[pfio_cpstemp_num].file1,"r"))==NULL){
        unix_abort_c("Unable to write a cpstemp_nodes_file in pfio_init.");
      } else {
        mtio_fclose(file);
      }
    }
  } else {
    mtio_fclose(file);
  }

  /*** removed because cpswork is not used
        cpsdisks[pfio_cpswork_num].disk_info=NULL;
        strcpy(cpsdisks[pfio_cpswork_num].name,"cpswork");
        cpsdisks[pfio_cpswork_num].file_time=0;
        cpsdisks[pfio_cpswork_num].num_nodes=0;
        cpsdisks[pfio_cpswork_num].reserve_mode='R';
        cpsdisks[pfio_cpswork_num].exist_msg='N';
        strcpy(cpsdisks[pfio_cpswork_num].file1,
            cnfg_get_value_c("cpswork_nodes_file"));
        strcpy(cpsdisks[pfio_cpswork_num].file2, "cpswork_nodes.dat");
  ***/

  strcpy(rsh_timeout,cnfg_get_value_c("rsh_timeout_command"));
  if(rsh_timeout[0]=='\0' || rsh_timeout[0]==' ') {
    /*
    fprintf(stderr,
      "Warning: rsh_timeout not defined in cnfg file-default used\n");
    */
    strcpy(rsh_timeout,"rsh_timeout");
  }

  strcpy(reserve_file_space,cnfg_get_value_c("pfio_reserve_file_space"));
  if(reserve_file_space[0]=='\0' || reserve_file_space[0]==' ') {
    /*
    printf(
      "Warning reserve_file_space not defined in cnfg file-default used\n");
    */
    strcpy(reserve_file_space,"filereserve");
}


  strcpy(reserve_file_space_na,cnfg_get_value_c("pfio_reserve_file_space_na"));
  if(reserve_file_space_na[0]=='\0' || reserve_file_space_na[0]==' ') {
    /*
    printf(
      "Warning reserve_file_space_na not defined in cnfg file-default used\n");
    */
    strcpy(reserve_file_space_na,"filereservenetapps");
}


  strcpy(cps_lock_file, cnfg_get_value_c("cps_lock_file"));
  if(cps_lock_file[0]=='\0' || cps_lock_file[0]==' ') {
    /*
    fprintf(stderr,
      "Warning: cps_lock_file not defined in cnfg file-default used\n");
    */
    strcpy(cps_lock_file,"/usr/applinux/cps_log/cps_lockfile.dat");
}

  for (ifile=0; ifile<pfio_number_files; ifile++) {
    file_info[ifile]->file_type=' ';
    file_info[ifile]->fetch_ptr=NULL;
  }

  delete_files[0]=NULL;
  delete_files[1]=NULL;
  locked_files[0] =NULL;
  locked_files[1] =NULL;
  trace_entries[0]=NULL;
  trace_entries[1]=NULL;

/***************************************
  fprintf(stderr,"cps_lock_file=|%s|\n",cps_lock_file);
  fprintf(stderr,"cps_mess_lock_file=|%s|\n",cps_mess_lock_file);
  fprintf(stderr,"cps_main_host=|%s|\n",cps_main_host);

  fprintf(stderr,"cpsdata_nodes_file=|%s|\n",cpsdisks[pfio_cpsdata_num].file1);
  fprintf(stderr,"cpstemp_nodes_file=|%s|\n",cpsdisks[pfio_cpstemp_num].file1);

  !fprintf(stderr,"cpswork_nodes_file=|%s|\n",cpsdisks[pfio_cpswork_num].file1);

  fprintf(stderr,"cps_node_name1=|%s|\n",cps_node_name1);
  fprintf(stderr,"cps_node_name2=|%s|\n",cps_node_name2);
  fprintf(stderr,"cps_node_name3=|%s|\n",cps_node_name3);
  fprintf(stderr,"cps_node_name4=|%s|\n",cps_node_name4);
  fprintf(stderr,"cps_node_name5=|%s|\n",cps_node_name5);
  fprintf(stderr,"cps_node_name6=|%s|\n",cps_node_name6);
  fprintf(stderr,"cps_node_name7=|%s|\n",cps_node_name7);
  fprintf(stderr,"cps_node_name8=|%s|\n",cps_node_name8);
  fprintf(stderr,"cps_node_name9=|%s|\n",cps_node_name9);
  fprintf(stderr,"cps_node_name10=|%s|\n",cps_node_name10);
*****************************************/
  /*** first time through, read the cps_disk files and do a stat on the
       disk servers
  ***/
  for(i=0;i<pfio_num_cpsdisks;i++){
    pfio_read_cpsdisk_file(i);
  }
  if(pfio_debug_check(2,0)) {
    fprintf(stderr,"cpsdata_nodes_file=%s\n",cpsdisks[pfio_cpsdata_num].file1);
    fprintf(stderr,"cpstemp_nodes_file=%s\n",cpsdisks[pfio_cpstemp_num].file1);
    fprintf(stderr,"pfio_info:CPSTEMP has %g Gigabytes free.\n",
    (double) cpsdisks[pfio_cpstemp_num].available_space/(1024.*1024.*1024.));
    /*
    fprintf(stderr,"pfio:CPSWORK has %g Gigabytes free.\n",
    !(double) cpsdisks[pfio_cpswork_num].available_space/(1024.*1024.*1024.));
    */
    fprintf(stderr,"pfio_info:CPSDATA for prj (%s) has %g Gigabytes free.\n",
    pfio_get_project_name(),
    (double) cpsdisks[pfio_cpsdata_num].available_space/(1024.*1024.*1024.));
 }

  cpslog_init();

  return;
}

/************ PFIO_EXIT ******************
* wrap up pfio
*
* Written March 2001 by Charles C Burch
*****************************************/
void pfio_exit() {
  int i  /*,n*/;
  char name[260], str[260];
  /*char ** projnames;*/

  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_exit, ts=%f\n", (float)unix_get_wtime_stamp_c());
  }
/* --- exit if pfio_init not called or exit already called --- */
  if(pfio_init_sw!=1) return;

/* close any opened files */
  for(i=0;i<pfio_number_files; i++) {
    if(file_info[i]->file_type!=' ') {
      pfio_close(i+1);
    }
  }
/*** This is the unit test for the two routines called within the block
  n=pfio_get_number_of_projects();
  fprintf(stderr,"pfio_exit: number_of_projects=%d\n",n);
  projnames = pfio_get_valid_project_names();
  for (i=0;i<n;i++)fprintf(stderr,"%s\n",projnames[i]);
***/

/*free any allocated memory*/
  for(i=0;i<pfio_num_cpsdisks;i++) {
    if(cpsdisks[i].disk_info!=NULL) {
      if(file_info!=NULL)                            /*TRS*/
        {                                            /*TRS*/
        if(file_info[i]!=NULL)                       /*TRS*/
          {                                          /*TRS*/
          if(file_info[i]->reserve_info!=NULL)
            {                                        /*TRS*/
            free(file_info[i]->reserve_info);
            file_info[i]->reserve_info=NULL;         /*TRS*/
            }                                        /*TRS*/
          }                                          /*TRS*/
        }                                            /*TRS*/
      free(cpsdisks[i].disk_info);
    }
  }

/*delete any created undeleted cpstemp files */
  lnklst_get_list_entry(delete_files, name, str,-1);
  while(strlen(name)>0){
    pfio_delete(name);
/*  fprintf(stderr,"pfio_exit deleting cpstemp file %s\n",name); */
    lnklst_get_list_entry(delete_files, name, str,-1);
  }

/* Unlock and files lockes that have not been unlocked */
  lnklst_get_list_entry(locked_files, name, str,-1);
  while(strlen(name)>0){
/*  fprintf(stderr,"pfio_exit unlocking file %s\n",name); */
    pfio_unlock_file(cps_lock_file, name);
    lnklst_get_list_entry(locked_files, name,str,-1);
  }

  pfio_set_trace_mode(0);     /*flush any trace entries*/

/* free file_info space */
  for(i=0;i<pfio_number_files; i++) {
    if(file_info[i]!=NULL) free(file_info[i]);
    file_info[i]=NULL;
  }
  free(file_info);
  file_info=NULL;
  pfio_number_files=0;

  cpslog_exit();

  fflush(stdout);
  pfio_init_sw=0;

  bfio_exit();
  /*mtio_exit(); --- this is called in bfio_exit...*/

  for (i=0;i<pfio_get_number_of_projects();i++){
    free(pfio_valid_project_names[i]);
  }
  free (pfio_valid_project_names);
  return;
}

/********** pfio_get_project_name *********************
* Bill Menger 6/6/05 gets THIS project's name from
* memory.
******************************************************/
char* pfio_get_project_name(){
  /* overrides the environment variable (if set) */
  if(pfio_init_sw==0) pfio_init();
  return pfio_this_project_name;
}

/***********pfio_set_project_name ********************
* Bill Menger 6/6/05 sets a project name for THIS
* project, so it can be validated against the data
* file when writing data.
******************************************************/
void pfio_set_project_name(char* project_name){
  if(pfio_init_sw==0) pfio_init();
  strcpy(pfio_this_project_name,project_name);
  pfio_project_name_changed=1;
}

/********* pfio_get_number_of_projects ***************
* Bill Menger 6/6/05 returns number of elements in
* the list of projects from pfio's cpsdata_nodes.dat
* file.
******************************************************/
int pfio_get_number_of_projects(){
  /*fprintf(stderr,"get_number_of_projects: %d\n",pfio_number_of_projects);*/
  if(pfio_init_sw==0) pfio_init();
  return pfio_number_of_projects;
}

/********* pfio_get_valid_project_names **************
* Bill Menger 6/6/05 returns pointer to list of
* valid project names, the list is loaded at init time
* into an array of character pointers.
******************************************************/
char ** pfio_get_valid_project_names(){
  /*** This is a test of functionality ... not needed for execution.
      int i;
  if(pfio_init_sw==0) pfio_init();
      for(i=0;i<pfio_get_number_of_projects();i++){
        fprintf(stderr,"pfio_get_valid_project_names: %s\n",
        pfio_valid_project_names[i]);
      }
  ***/
  return pfio_valid_project_names;
}

/************** PFIO_EXT_FLSZ ************************
*       Obtain size of a file extents -fn=file name
*
*       Written November 2000 by Charles C Burch
******************************************************/
int64_t pfio_ext_flsz(char *fn) {
  int64_t ext_size;
  int32_t num_exts;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,
    "pfio_ext_flsz: fn=%s, ts=%f\n",fn, (float)unix_get_wtime_stamp_c());
  }
  pfio_get_extent_info(fn, &num_exts, &ext_size);
  return(ext_size);
}

/***************** PFIO_GET_EXTENT_INFO ***************
* Get number extents and max ext size
*   returns file size
*
* Written December 2001 by Charles C Burch
*******************************************************/
int64_t pfio_get_extent_info(char *fn, int32_t *num_exts,
  int64_t *ext_size) {
  int ifile;
  int64_t isize, extsz, f_size;
  char fn_ext[260], full_fn[260];

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_get_extent_info: fn=%s, ts=%f\n",
        fn, (float)unix_get_wtime_stamp_c());
  }

  bfio_expand_file_name(fn, full_fn, sizeof(full_fn));
  fn=bfio_get_base_filename(full_fn);

  /***           add in size of file extensions                 ***/
  f_size=0;
  ifile=-1;
  extsz=1;
  while (1) {
    pfio_form_fn_ext(fn, ++ifile, fn_ext);
    isize=pfio_file_size(fn_ext);
    /*  fprintf(stderr,"checking  file size of %s (%"PRId32")\n",fn_ext,isize);*/
    if(isize<0) break;

    f_size+=isize;
    if(isize>extsz) extsz=isize;
  }

  (*num_exts)=ifile;
  (*ext_size)=extsz;

  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,
    "pfio_get_extent_info-exit: #exts=%"PRId32", extsz=%"PRId64", file size=%"PRId64", ts=%f\n",
     *num_exts,*ext_size,f_size, (float)unix_get_wtime_stamp_c());
  }
  return(f_size);
}

/****************** PFIO_FLSZ ***********************
*       Obtain file size of a bf file-fn=file name
*         returns file size
*
*       Written August 2000 by Charles C Burch
****************************************************/
int64_t pfio_flsz(char *fn) {
  int ifile;
  int64_t isize, f_size;
  char fn_ext[260], full_fn[260];

  if(pfio_debug_check(1,1)) {
    fprintf(stderr,
    "pfio_flsz: fn=%s, ts=%f\n",fn, (float)unix_get_wtime_stamp_c());
  }

  if(pfio_init_sw==0) pfio_init();

  bfio_expand_file_name(fn, full_fn, sizeof(full_fn));
  fn=bfio_get_base_filename(full_fn);

  /***           add in size of file extensions                 ***/
  f_size=0;
  ifile=-1;
  while (1) {
    pfio_form_fn_ext(fn, ++ifile, fn_ext);
    isize=pfio_file_size(fn_ext);
    /*  fprintf(stderr,"checking  file size of %s (%"PRId32")\n",fn_ext,isize);*/
    if(isize<0) break;

    f_size+=isize;
  }

  if(ifile==0 && f_size==0) f_size=-1;

  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_flsz-exit: fn=%s, file size=%"PRId64", ts=%f\n",
        fn,f_size, (float)unix_get_wtime_stamp_c());
  }
  return(f_size);
}

/*********************** PFIO_DELETE **************************
* Deletes a bf file-fn=file name
*   return number of extents deleted>=0, if no error
*
* Written August 2000 by Charles C Burch
**************************************************************/
int pfio_delete(char *fn) {
  int istat;

  if(pfio_debug_check(1,1)) {
    fprintf(stderr,
    "pfio_delete: fn=%s, ts=%f\n",fn, (float)unix_get_wtime_stamp_c());
  }

  if(pfio_init_sw==0) pfio_init();
  istat=pfio_actual_delete(fn,1); /*delete with unlock*/

  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_delete-exit: result=%d, ts=%f\n",
        istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/************* PFIO_SET_FILE_SPACE_COMMIT *********************
* set file space commit sw for next open
*   if isw=-1 do not commit disk space for next file opened
*   if isw=1 commit disk space for next file ,if write
*   if isw=0 commit disk space for cpstemp/cpsdata
*            do not commit disk space for other files
*   file extents are opened for first time with write, they will
*   be filled with zeroes to commit the file space.
*   The commit sw is reset after the open
*   so next open default is 0--this function must be explicitly
*   called to override the default for each open
*
* Written November 2000 by Charles C Burch
**************************************************************/
void pfio_set_file_space_commit(int isw) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_file_space_commit: isw=%d, ts=%f\n",
        isw, (float)unix_get_wtime_stamp_c());
  }
  if(isw<-1 || isw>1) isw=0;
  pfio_file_space_commit_sw=isw;
  return;
}

/***************** PFIO_SET_FILE_LOCK_CONTROL ****************
* set file lock control for next file opened
*  isw=0 means do not lock file, =1 means lock as needed
*  isw =-1 set to default value 1
*
* Written August 2001 by Charles C Burch
**************************************************************/
void pfio_set_file_lock_control(int isw) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_file_lock_control: isw=%d, ts=%f\n",
        isw, (float)unix_get_wtime_stamp_c());
  }
  if(isw<0 || isw>1) isw=1;
  pfio_lock_control=isw;
  return;
}

/***************** PFIO_SET_CPSDISK_CONTROL ****************
* set cpsdisk control for next file opened
*  isw=0 means bypass cpsdisk handling,
*     =1 means normal  handling
*
* Written July 2002 by Charles C Burch
************************************************************/
void pfio_set_cpsdisk_control(int isw) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_cpsdisk_control: isw=%d, ts=%f\n",
        isw, (float)unix_get_wtime_stamp_c());
  }
  if(isw<0 || isw>1) isw=1;
  pfio_cpsdisk_control=isw;
  return;
}

/*************** PFIO_SET_FILE_AUTO_DELETE *******************
* set file auto delete on closing for next file opened
*  isw=0 means do not auto delete,
*     =1 means auto delete on program exit
*     =2 means auto delete when file is closed
*
* Written August 2001 by Charles C Burch
**************************************************************/
void pfio_set_file_auto_delete(int isw) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_file_auto_delete: isw=%d, ts=%f\n",
        isw, (float)unix_get_wtime_stamp_c());
  }
  if(isw<0 || isw>2) isw=0;
  pfio_auto_delete=isw;
  return;
}


/******************** PFIO_SET_EXT_SIZE **************************
* Sets size(in bytes) of extents for new open, if it is a new file
*  on file opened update-it sets extent size if the file has no
*  extents yet and the file size is < requested extent size
*
* Written November 2000 by Charles C Burch
******************************************************************/
int pfio_set_ext_size(int64_t size) {

  if(pfio_init_sw==0) pfio_init();

  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_ext_size: ext_flsz=%"PRId64", ts=%f\n",
        size, (float)unix_get_wtime_stamp_c());
  }
  if(size<=0) return(-1);
  if(size>BF_FLSZ) return(-2);
  pfio_extent_size=size;
  return(0);
}

/********************* PFIO_OPEN ******************************
*       pfio_open Opens a diskfile-fn=file name
*         rwu=r:read, w:write, u:update
*         file id of opened file >0, if no error
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int pfio_open(char *fn_in, char rwu) {
  int i, ifile=-1; /*initialization to bypass invalid SUN warning*/
  char fn[260];

  if(pfio_init_sw==0) pfio_init();

  if((i=str_compress(fn, fn_in))>0) {
    if(strcmp(fn,"pf:")==0 || strcmp(fn,"PF:")==0) i=0;
  }

  if(i==0) {   /*form tmpnam */
    strcat(fn,bfio_form_tmpnam("pfio"));
  }

  for (i=0;i<3;i++) {
    if((ifile=pfio_open_actual(fn, rwu, (char*)NULL, 0))>=0) break;
    if(ifile==-4 || ifile==-6) break;
    if(pfio_disk_full_flag == 1 ) break;
    bfio_remount_file_disk(fn); /*force remount*/
    if(i==2) sleep(1);
  }

  if(i>0 && ifile>=0) {
    sprintf(pfio_error_string,
        "Warning: Open failure(%s), but worked on retry %d [%d:%s]",
        fn,i,errno,strerror(errno));
    cpslog_message(pfio_error_string);
  }

  if(ifile<0) {
    sprintf(pfio_error_string,
        "Warning: Open failure on file(%s), ifile=%d [%d:%s]",
        fn,ifile,errno,strerror(errno));
    cpslog_message(pfio_error_string);
  }

  pfio_auto_delete=-1;
  pfio_lock_control=1;                /*set default to lock as needed*/
  pfio_file_space_commit_sw=0;        /*set to default for next open */
  pfio_extent_size=pfio_file_extent_size;
  pfio_cpsdisk_control=1;

  return(ifile);
}

/****************** PFIO_CLOSE ********************************
*       Close pf
*         ifile = file id of file to close
*         returns <0 if error
*         return >=0 (#extents), if no error
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int pfio_close(int ifile) {
  int i, num_exts;
  char buff[260], fn_ext[260];
  int64_t flsz;

  if(pfio_init_sw==0) pfio_init();

  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_close: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }

  pfio_error_string[0]='\0';
  if( (ifile=pfio_check_file_fd(ifile,"pfio_close")) < 0) {
    num_exts=ifile;
    goto exit;
  }

  if(pfio_trace_mode>0) {
    sprintf(buff,"c %d",ifile+1);
    lnklst_put_list_entry(trace_entries,buff,"");
  }

  pfio_reserve_check(ifile); /*ensure any previous thread done*/
  num_exts=file_info[ifile]->num_exts;
  for(i=0;i<num_exts;i++) {
    if(file_info[ifile]->ext_info[i].host_ifile>=0) {
      pfio_close_file_extent(ifile,i);
    }
  }

  if(file_info[ifile]->fetch_ptr!=NULL) {
    free(file_info[ifile]->fetch_ptr);
    file_info[ifile]->fetch_ptr=NULL;
  }

  if(file_info[ifile]->rwu!='r' && file_info[ifile]->file_space_commit==1 ) {
    for(i=0;i<num_exts;i++) {          /*ensure extents correct size*/
      pfio_form_fn_ext(file_info[ifile]->fn, i, fn_ext);
      flsz=pfio_file_size(fn_ext);

      if(flsz>file_info[ifile]->ext_info[i].size) {
        sprintf(buff,
         "Warning: %s file(%s) size expected=%"PRId64", actual=%"PRId64"",
         "pfio_close_file_extent",file_info[ifile]->fn,
         file_info[ifile]->ext_info[i].size, flsz);
        cpslog_message(buff);

        if(pfio_is_file_zero(fn_ext,file_info[ifile]->ext_info[i].size,-1)>=0) {
          sprintf(buff,
           "Warning: %s file(%s) successfully truncated",
           "pfio_close_file_extent",file_info[ifile]->fn);
           cpslog_message(buff);
          truncate(fn_ext, (off_t) file_info[ifile]->ext_info[i].size);
        } else {
          fprintf(stderr,"%s\n",buff);
          sprintf(buff,
           "ERROR: %s file(%s) to be truncated was not all zero",
           "pfio_close_file_extent",file_info[ifile]->fn);
           cpslog_message(buff);
        }
      }
    }
  }

  if(file_info[ifile]->rwu!='r') {
    pfio_truncate(file_info[ifile]->fn, file_info[ifile]->flsz);
  }

  if(file_info[ifile]->auto_delete==2)    /*delete if auto delete set */
    pfio_actual_delete(file_info[ifile]->fn, file_info[ifile]->lock_control);

  if(file_info[ifile]->reserve_info!=NULL) {
    free(file_info[ifile]->reserve_info);
    file_info[ifile]->reserve_info=NULL;
  }

  if(num_exts>0) {
    free(file_info[ifile]->ext_info);
    file_info[ifile]->ext_info=NULL;
    file_info[ifile]->num_exts=0;
  }
  file_info[ifile]->file_type=' ';

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_close exit: status=%d, ts=%f\n",
        num_exts, (float)unix_get_wtime_stamp_c());
  }

  return(num_exts);
}

/****************** PFIO_TRUNCATE *********************
* Truncates a file to specified file size, if it bigger
*
* Written November 2000 by Charles C Burch
******************************************************/
int pfio_truncate(char *file_name, int64_t flsz) {
  int64_t actual_flsz, size;
  char fn_ext[360], fn[360];
  int i_ext, delete_sw;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_truncate: fn=%s, flsz=%"PRId64", ts=%f\n",
        file_name, flsz, (float)unix_get_wtime_stamp_c());
  }
  if(flsz<0) flsz=0;

  bfio_expand_file_name(file_name, fn, sizeof(fn));
  actual_flsz=pfio_flsz(fn);
  if(actual_flsz<0) return(-1);
  if(actual_flsz<=flsz) return(1);

  i_ext=-1;
  actual_flsz=0;
  delete_sw=0;
  while (1) {
    pfio_form_fn_ext(fn, ++i_ext, fn_ext);
    size=pfio_file_size(fn_ext);
    if(size<0) break;

    if(delete_sw!=0) {
      pfio_delete_a_file_extent(fn, i_ext);
    } else {
      actual_flsz+=size;
      if(actual_flsz>=flsz) {
        delete_sw=1;
        size=flsz-(actual_flsz-size);
        truncate(fn_ext, (off_t) size);
      }
    }
  }
  return(0);
}

/************************ PFIO_READ ***************************
*       pfio_read a pf file
*         ifile = file id of file to read
*         nbytes=number of bytes to read
*         buff=buffer to read data into
*         returns <0 if error
*         returns >=0 if no error and nbytes read
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int32_t pfio_read(int ifile, char *buff, int32_t nread) {
  int64_t offset;
  int32_t istat;
  char line[260];

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_read: ifile=%d, n=%"PRId32", ts=%f\n",
        ifile, nread, (float)unix_get_wtime_stamp_c());
  }

  if( (ifile=pfio_check_file_fd(ifile,"pfio_read")) < 0) {
    istat=ifile;
    goto exit;
  }

  if(nread<=0) {
    sprintf(pfio_error_string,"byte count(%"PRId32")<=0 in pfio_read",
        nread);
    istat=-2;
    goto exit;
  }

  offset=file_info[ifile]->offset;
  if(offset>=file_info[ifile]->flsz) {
    istat=-1;
    goto exit;
  }

  if(pfio_trace_mode>0) {
    sprintf(line,"r %d %"PRId64" %"PRId32"",ifile+1,offset,nread);
    lnklst_put_list_entry(trace_entries,line,"");
  }

/***            See if data is in cache            ***/

  file_info[ifile]->nreads++;
/***  fprintf(stderr,"offset=%"PRId64", nread=%"PRId32", cache offsets=(%"PRId64", %"PRId64")\n",
      offset, nread, file_info[ifile]->fetch_offset1,
      file_info[ifile]->fetch_offset2);  ***/

  if(offset           >= file_info[ifile]->fetch_offset1 &&
     (offset+nread-1) <= file_info[ifile]->fetch_offset2) {
/***fprintf(stderr,"cache hit, offset, nread=%"PRId64", %"PRId32"\n", offset, nread); ***/
    memcpy(buff,
      file_info[ifile]->fetch_ptr+(offset-file_info[ifile]->fetch_offset1),
      nread);
    file_info[ifile]->offset+=nread;
    file_info[ifile]->cache_hits++;
    istat=nread;
    goto exit;
  }

/**************** do read ahead if active **************/

  if(file_info[ifile]->read_ahead>0 && nread<=65000) {
  /*fprintf(stderr,"doing read ahead, offset=%"PRId64"\n",offset);*/
    istat=pfio_fetch(ifile+1, offset, 65000);
    if(offset           >= file_info[ifile]->fetch_offset1 &&
      (offset+nread-1) <= file_info[ifile]->fetch_offset2) {
      memcpy(buff, file_info[ifile]->fetch_ptr, nread);
      file_info[ifile]->offset+=nread;
      istat=nread;
      goto exit;
    }
  }

  if(io_timeout==0) {
    istat=pfio_read_data(ifile,buff, nread);
  } else {
    istat=pfio_read_data_timeout(ifile,buff, nread, io_timeout);
  }

  if(nread!=istat && pfio_error_string[0]=='\0') {
    sprintf(pfio_error_string,"pfio_read, nbytes=%"PRId32", actual=%"PRId32"",
        nread, istat);
  }

exit:
  io_timeout=0;
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_read-exit: istat=%"PRId32", ts=%f\n",
        istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/************************ PFIO_GETS ***************************
*       pfio_reads a line terminated by \n from a pf file
*         ifile = file id of file to read
*         nbytes=maximum number of bytes to read
*         buff=buffer to read data into
*         returns <0 if error
*                  0 if EOF
*                 >0 if no error and \n read
*
*       Written June 2002 by Charles C Burch
**************************************************************/
int32_t pfio_gets(int ifile, char *buff, int32_t nread) {
  int64_t offset, i_seek, ext_size;
  int32_t istat, nread_actual;
  char line[260], *buff_current;
  int32_t nbytes, nread_current;
  int  i_ext, i_eol;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_gets: ifile=%d, n=%"PRId32", ts=%f\n",
        ifile, nread, (float)unix_get_wtime_stamp_c());
  }

  buff[0]='\0';
  if( (ifile=pfio_check_file_fd(ifile,"pfio_gets")) < 0) {
    istat=ifile;
    goto exit;
  }

  if(nread<=0) {
    sprintf(pfio_error_string,"byte count(%"PRId32")<=0 in pfio_gets",
        nread);
    istat=-2;
    goto exit;
  }
  offset=file_info[ifile]->offset;
  if(offset>=file_info[ifile]->flsz) {
    if(offset==file_info[ifile]->flsz) {
      istat=0;
    } else {
      istat=-1;
    }
    goto exit;
  }

  if(pfio_trace_mode>0) {
    sprintf(line,"r %d %"PRId64" %"PRId32"",ifile+1,offset,nread);
    lnklst_put_list_entry(trace_entries,line,"");
  }

  file_info[ifile]->nreads++;
/****** note cache and read aheads inactive for gets ******/
  nread_actual=0;
  buff_current=buff;
  /* note mtio_fgets will read 1 less character than requested*/
  if((offset+nread)<file_info[ifile]->flsz) {
    nread_current=nread;
  } else {
    nread_current=file_info[ifile]->flsz-offset+1;
  }
  i_eol=0;

  while(nread_current>1 && i_eol==0) {
    /** fprintf(stderr,"gets:offset=%"PRId64", n=%"PRId32"\n",offset, nread_current); **/
    pfio_get_ext_num(ifile,offset,&i_ext,&i_seek,&ext_size);
    if(i_ext<0) {
      break;
    }

    if((i_seek+nread_current)<ext_size) {
      nbytes=nread_current;
    } else {
      nbytes=ext_size-i_seek+1;
    }
    /** fprintf(stderr,"Gets file=%d, ext %d, i_seek=%"PRId64", nbytes=%"PRId32"\n",
      ifile+1,i_ext, i_seek, nbytes); ***/

    if(file_info[ifile]->ext_info[i_ext].host_ifile<0) {
      istat=pfio_open_file_extent(ifile,i_ext);
      if(istat<0) break;
    }
    istat=bfio_gets(file_info[ifile]->ext_info[i_ext].host_ifile,i_seek,
        buff_current, nbytes);

    /* fprintf(stderr,"istat from bfio_gets=%d\n",istat);  */
    if(istat>=0) {
      i_eol=1;
    } else if(istat==-5) {
      istat=nbytes-1;
    } else {
      break;
    }
    buff_current += istat;
    nread_current -= istat;
    offset += istat;
    nread_actual += istat;
    /** fprintf(stderr,"Enter 1:"); scanf("%"PRId32"",&istat);  **/
  }
  /** fprintf(stderr,"gets: nread_act=%d, ieol=%d\n",nread_actual, i_eol); **/
  file_info[ifile]->offset+=nread_actual;
  if(i_eol==1) {
    istat=nread_actual;
    if(istat>0) {
      if(buff[istat-1]!='\n') istat=-1;
    }
  } else {
    istat=-1;
    if(pfio_error_string[0]=='\0') {
      sprintf(pfio_error_string,"pfio_gets, nbytes=%"PRId32", actual=%"PRId32", istat=%"PRId32"",
        nread, nread_actual,istat);
    }
  }

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_gets-exit: istat=%"PRId32", ts=%f\n",
        istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

/************************** PFIO_FETCH ************************
*       fetches data from  a pf file into cache-no data returned
*         ifile = file id of file to read
*         nbytes=number of bytes to read
*         buff=work buffer to read data into
*         returns <0 if error
*         returns >=0 if no error and nbytes read
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int32_t pfio_fetch(int ifile, int64_t offset, int32_t nread) {
  int  istat;
  int64_t offset_save;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_fetch: ifile=%d, offset=%"PRId64", n=%"PRId32", ts=%f\n",
        ifile, offset,nread, (float)unix_get_wtime_stamp_c());
  }
/*** fprintf(stderr,"pfio_fetch, offset=%"PRId64", nread=%"PRId32"\n", offset, nread); ***/

  if( (ifile=pfio_check_file_fd(ifile,"pfio_fetch")) < 0) return(ifile);
  if(file_info[ifile]->rwu=='w') return(-3);
  if(nread<0) {
    return(-2);
  } else if(nread==0) {
    return(0);
  }

  if(file_info[ifile]->fetch_ptr==NULL) {
    if( (file_info[ifile]->fetch_ptr= (char*) malloc(65000))==NULL) return(0);
  }

  if(offset<0) return(-4);
  if(nread>65000) nread=65000;

/*** save old offset, prep for read, restore old offset, adjust fetch info ***/

  offset_save=file_info[ifile]->offset;
  file_info[ifile]->offset=offset;
  istat=pfio_read_data(ifile,file_info[ifile]->fetch_ptr, nread);
  file_info[ifile]->offset=offset_save;

  if(istat<=0) {
    file_info[ifile]->fetch_offset2=-1;
    return(istat);   /* return error, if one found */
  }
  file_info[ifile]->fetch_offset1=offset;
  file_info[ifile]->fetch_offset2=offset+istat-1;
/*  fprintf(stderr,"fetch offsets=%"PRId64", %"PRId64"\n",
    file_info[ifile]->fetch_offset1, file_info[ifile]->fetch_offset2); */
  return(istat);
}

/*************************** PFIO_WRITE ***********************
*       pfio_write writes to a pf file
*         ifile = file id of file to write
*         nbytes=number of bytes to write
*         buff=buffer to write data from
*         returns <0 if error
*         return >=0 if no error and number bytes written
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int32_t pfio_write(int ifile, char *buff, int32_t nwrite) {
  int32_t nwrite_actual, nwrite_current, istat=0;
  int  i_ext;
  int64_t i_seek, ext_size, nbytes;
  char *buff_current, line[260];
  int64_t offset;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_write: ifile=%d, n=%"PRId32", ts=%f\n",
        ifile, nwrite, (float)unix_get_wtime_stamp_c());
  }

  if( (ifile=pfio_check_file_fd(ifile,"pfio_write")) < 0) {
    nwrite_actual=ifile;
    goto exit;
  }

  if(file_info[ifile]->rwu=='r') {
    sprintf(pfio_error_string,"Attempt to write to read-only file");
    nwrite_actual=-3;
    goto exit;
  }

  if(nwrite<=0) {
    sprintf(pfio_error_string,"byte count(%"PRId32")<=0 in pfio_write",
     nwrite);
    nwrite_actual=-2;
    goto exit;
  }

  if(pfio_trace_mode>0) {
    sprintf(line,"w %d %"PRId64" %"PRId32"",
      ifile+1,file_info[ifile]->offset,nwrite);
    lnklst_put_list_entry(trace_entries,line,"");
  }

/***            See if data overlaps with cache            ***/

  file_info[ifile]->nwrites++;
  offset=file_info[ifile]->offset;
  if(offset           <= file_info[ifile]->fetch_offset2 &&
    (offset+nwrite-1) >= file_info[ifile]->fetch_offset1) {
/***     cache overlap-indicate data in cache not valid    ***/
    file_info[ifile]->fetch_offset1=file_info[ifile]->fetch_offset2=-1;
  }

  nwrite_actual=0;
  buff_current=buff;
  nwrite_current=nwrite;

  while(nwrite_current>0) {

    if(file_info[ifile]->file_extended==0) {
      if((offset+nwrite_current)>file_info[ifile]->flsz) {
        if(pfio_extend_last_extent(ifile)<0) {
          nwrite_actual=-4;
          goto exit;
        }
      }
    }

    pfio_get_ext_num(ifile,offset,&i_ext, &i_seek, &ext_size);
    /*fprintf(stderr,"pfio_write1:ifile=%d, i_ext=%d, i_seek=%"PRId64", sz=%"PRId64"\n",
        ifile,i_ext, i_seek, ext_size);
    */
    while(i_ext<0) {
      if(pfio_create_next_extent(ifile)<0){
        nwrite_actual=-4;
        goto exit;
      }
      pfio_get_ext_num(ifile,offset,&i_ext, &i_seek, &ext_size);
      /**
       fprintf(stderr,"pfio_write2:ifile=%d, i_ext=%d, i_seek=%"PRId64", sz=%"PRId64"\n",
       ifile,i_ext, i_seek, ext_size);  ***/
    }

    if((i_seek+nwrite_current)<=file_info[ifile]->ext_size) {
      nbytes=nwrite_current;
    } else {
      nbytes=file_info[ifile]->ext_size-i_seek;
    }

    nbytes=pfio_get_extent_size(ifile, i_ext, i_seek+nbytes-1)-i_seek;
    if(nbytes>nwrite_current) nbytes=nwrite_current;

    if(pfio_debug_check(2,0)) {
      fprintf(stderr,
       "pfio_write file=%d, ext %d, iseek=%"PRId64", nbytes=%"PRId64", ts=%f\n",
       ifile+1,i_ext, i_seek, nbytes, (float)unix_get_wtime_stamp_c());
    }

    if(file_info[ifile]->ext_info[i_ext].host_ifile<0) {
      istat=pfio_open_file_extent(ifile,i_ext);
      if(istat<0) break;
      file_info[ifile]->ext_info[i_ext].host_ifile=istat;
    }

    istat=pfio_actual_write(ifile, i_ext, i_seek, buff_current,
      (int32_t)nbytes);
    if(istat>0) {
      buff_current   += istat;
      nwrite_current -= istat;
      offset         += istat;
      nwrite_actual  += istat;
    }

    if(pfio_debug_check(2,0)) {
      fprintf(stderr,"offset=%"PRId64", istat=%"PRId32", n=%"PRId32", ts=%f\n",
          offset,istat,nwrite_current, (float)unix_get_wtime_stamp_c());
    }
    if(istat!=nbytes) break;
  }

  file_info[ifile]->offset+=nwrite_actual;
  if(file_info[ifile]->offset>file_info[ifile]->flsz){
    file_info[ifile]->flsz=file_info[ifile]->offset;
  }
  if(nwrite_actual==0)  nwrite_actual=istat;
  if(nwrite_actual>0 && nwrite_actual!=nwrite && pfio_error_string[0]=='\0') {
    sprintf(pfio_error_string,"pfio_write: byte requested=%"PRId32", actual=%"PRId32"",
        nwrite, nwrite_actual);
  }

exit:
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_write-exit: %"PRId32", ts=%f\n",
        nwrite_actual, (float)unix_get_wtime_stamp_c());
  }

  return(nwrite_actual);
}

/********************** PFIO_SEEK *****************************
* set seek offset for given file
*   ifile = file id of file
*   offset=file offset to seek to
*
* Written August 2000 by Charles C Burch
**************************************************************/
int pfio_seek(int ifile, int64_t offset) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_seek: ifile=%d, offset=%"PRId64", ts=%f\n",
        ifile, offset, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_seek")) < 0) return(ifile);

  if(offset<0) offset=0;
  file_info[ifile]->offset=offset;
  return(0);
}

/***************** PFIO_SEEK_VIA_ORIGIN ***********************
*       set seek offset for given file  as with c fseek
*         ifile = file id of file
*         offset=file offset to seek to
*         origin (SEEK_SET, SEEK_CUR, SEEK_END)
*
*       Written September 2000 by Charles C Burch
**************************************************************/
int pfio_seek_via_origin(int ifile, int64_t offset, int origin) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,
      "pfio_seek_via_origin: ifile=%d, offset=%"PRId64", origin=%d, ts=%f\n",
      ifile, offset,origin, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_seek")) < 0) return(ifile);

  switch(origin) {
  case(SEEK_SET):
    if(offset<0) offset=0;
    file_info[ifile]->offset=offset;
    return(0);

  case(SEEK_CUR):
    offset=file_info[ifile]->offset+offset;
    if(offset<0) offset=0;
    file_info[ifile]->offset=offset;
    return(0);

  case(SEEK_END):
    offset=offset+file_info[ifile]->flsz;
    if(offset<0) offset=0;
    file_info[ifile]->offset=offset;
    return(0);
  }
  sprintf(pfio_error_string,"Invalid origin(%d) in pfio_seek_via_origin",
   origin);
  return(-1);
}

/************************ PFIO_TELL ***************************
*       get current offset for given file
*         ifile = file id of file
*         returns int64_t offset
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int64_t pfio_tell(int ifile) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_tell: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_tell")) < 0) return(ifile);

  return(file_info[ifile]->offset);
}

/****************** PFIO_ENABLE_READ_AHEAD ********************
* activates read_ahead for file ifile
*
* Written August 2000 by Charles C Burch
**************************************************************/
int pfio_enable_read_ahead(int ifile) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_enable_read_ahead: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_enable_read_ahead")) < 0)
    return(ifile);

  file_info[ifile]->read_ahead=1;
  return(0);
}

/************** PFIO_SET_FILE_REGION_LOCKING ******************
* enables/disables region file locks during reads/write
*
* Written February 2001 by Charles C Burch
**************************************************************/
int  pfio_set_file_region_locking(int ifile, int mode){
  int i;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_file_region_locking: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_set_region_file_lock")) < 0)
    return(ifile);

  if(mode!=0) mode=1;
  if(mode==file_info[ifile]->region_lock) return(0);

  file_info[ifile]->region_lock=mode;
  for(i=0; i<file_info[ifile]->num_exts; i++) {
        /*change the mode on opened extents*/
    if(file_info[ifile]->ext_info[i].host_ifile>=0)
      pfio_set_ext_region_lock(ifile,i, mode);
  }
  return(0);
}
/********* PFIO_SET_EXT_REGION_LOCK *************
* set extent region lock
*
* Written Februrary 2001 by Charles C Burch
*************************************************/
int pfio_set_ext_region_lock(int ifile, int i_ext, int mode) {
  int istat = 0;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_ext_region_lock: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  istat=bfio_set_file_region_lock(
     file_info[ifile]->ext_info[i_ext].host_ifile,mode);
  return(istat);
  }

/***************** PFIO_DISABLE_READ_AHEAD ********************
* deactivates read_ahead for file ifile
*
* Written August 2000 by Charles C Burch
**************************************************************/
int pfio_disable_read_ahead(int ifile) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_disable_read_ahead: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_disable_read_ahead")) < 0)
    return(ifile);

  file_info[ifile]->read_ahead=0;
  return(0);
}

/*********************** PFIO_FLUSH ***************************
*       flush  given file
*         ifile = file id of file
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int pfio_flush(int ifile) {
  int    istat = 0, i;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_flush: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_flush")) < 0) return(ifile);

  for(i=0; i<file_info[ifile]->num_exts; i++) {
    if(file_info[ifile]->ext_info[i].host_ifile>=0) {
      istat=bfio_flush(file_info[ifile]->ext_info[i].host_ifile);
    }
  }
  return(istat);
}

/***************** PFIO_SETBUFSZ **********************
*       set pf buffer size
*         ifile = file id of file, bufsz=buffer size
*         if bufsz<0 turn off buffering
*
*       Written August 2000 by Charles C Burch
******************************************************/
int pfio_setbufsz(int ifile, int32_t bufsz) {
  int i;

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_setbufsz: ifile=%d, bufsz=%"PRId32", ts=%f\n",
        ifile, bufsz, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_setbufsz")) < 0) return(ifile);

  if(bufsz>0 && bufsz<file_info[ifile]->bufsz) return(0);

  file_info[ifile]->bufsz=bufsz;

  for(i=0; i<file_info[ifile]->num_exts; i++) {
    if(file_info[ifile]->ext_info[i].host_ifile>=0) {
      pfio_setbufsz_extent(ifile, i);
    }
  }
  return(0);
}

/********************** PFIO_RENAME_FILE **********************
* Renames a bf filename old_name to new_name
*   return >=0, if no error
*
* Written  Sep 2000 by Charles C Burch
**************************************************************/
int pfio_rename_file(char *fnold, char *fnnew) {
  int cpsdisk_sw_new, cpsdisk_sw_old, cpsdisk_pos_new;
  int ifile, ipos, disk_num;
  char fn_ext_new[260],  fn_ext_old[260], node[PFIO_MAX_NODENAME], work[260];
  char base_fn_new[260], base_fn_old[260];
  char *fn_old, *fn_new, full_fn_old[260], full_fn_new[260];

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_rename_file: old=%s, new=%s, ts=%f\n",
        fnold, fnnew, (float)unix_get_wtime_stamp_c());
  }

  bfio_expand_file_name(fnold, full_fn_old, sizeof(full_fn_old));
  fn_old=bfio_get_base_filename(full_fn_old);

  bfio_expand_file_name(fnnew, full_fn_new, sizeof(full_fn_new));
  fn_new=bfio_get_base_filename(full_fn_new);
  /*fprintf(stderr,
    "pfio_rename_file, fn_old=%s, fn_new=%s\n", fn_old, fn_new);
  */

  if(bfio_file_size(fn_old)<0){
    sprintf(pfio_error_string,
     "old filename(%s) does not exist in pfio_rename_file",fn_old);
    fprintf(stderr,"%s\n",pfio_error_string);
    return(-1);  /*old name does not exist-error*/
  }

  if(bfio_file_size(fn_new)>=0){
    sprintf(pfio_error_string,
     "new filename(%s) already exist in pfio_rename_file",fn_new);
    fprintf(stderr,"%s\n",pfio_error_string);
    return(-2);  /*new name exist-error*/
  }

/** both files cpsdisk or not cpsdisk, else error**/

  pfio_check_if_cpsdisk(fn_old, &cpsdisk_sw_old);
  cpsdisk_pos_new=pfio_check_if_cpsdisk(fn_new, &cpsdisk_sw_new);
  /*fprintf(stderr,"cpsdisk_pos_new=%d\n",cpsdisk_pos_new);*/

  if(cpsdisk_sw_old>=0 && cpsdisk_sw_new<0) {
    sprintf(pfio_error_string,
     "Renaming cpsdisk file to non-cpsdisk file not allowed in %s",
     "pfio_rename_file");
    fprintf(stderr,"%s\n",pfio_error_string);
    return(-3);
  }
  if(cpsdisk_sw_old<0 && cpsdisk_sw_new>=0) {
    sprintf(pfio_error_string,
     "Renaming non-cpsdisk file to cpsdisk file not allowed in %s",
     "pfio_rename_file");
    fprintf(stderr,"%s\n",pfio_error_string);
    return(-4);
  }

  ipos=str_find_str(fn_new,1,"/");
  if(fn_new[ipos]!='\0') bfio_ensure_dir_exists(fn_new,ipos+1);

/***          Rename file extensions                      ***/
  ifile=-1;
  while (1) {
    pfio_form_fn_ext(fn_old, ++ifile  , fn_ext_old);
    pfio_form_fn_ext(fn_new, ifile, fn_ext_new);

    if(bfio_file_size(fn_ext_old)<0) break;

    if(cpsdisk_sw_old<0) {

/***        non linked file--rename it directly            ***/
      /*fprintf(stderr,"old=%s new=%s\n",fn_ext_old,fn_ext_new);*/
      if( rename(fn_ext_old, fn_ext_new) != 0 ) {
        sprintf(pfio_error_string,
          "Unable to rename file(%s) to (%s) in pfio_rename_file",
          fn_ext_old, fn_ext_new);
        fprintf(stderr,"%s\n",pfio_error_string);
        return(-5);
       }
    } else {

/***    Linked file, rename file first and then the link    ***/

      memset(node,0,sizeof(node));
      pfio_decode_old_filename(fn_ext_old,node,base_fn_old, &disk_num);
      pfio_get_node_from_filename(base_fn_old, node, work, disk_num);
      ipos=strlen(work);
      pfio_form_cpsdisk_linked_name(work, fn_ext_new,
        cpsdisk_pos_new, base_fn_new);
      bfio_ensure_dir_exists(base_fn_new,ipos);
      if(rename(base_fn_old, base_fn_new)!=0) {
        sprintf(pfio_error_string,
         "Unable to rename(%s) to (%s) in pfio_rename_file",
          base_fn_old, base_fn_new);
        fprintf(stderr,"%s\n",pfio_error_string);
        return(-6);
      }
/*    pfio_delete_empty_directories(base_fn_old,ipos);  */

      remove(fn_ext_old); /*  rename link */
      remove(fn_ext_new);
      if(symlink(base_fn_new,fn_ext_new)<0) {
        sprintf(pfio_error_string,
         "Unable to rename link (%s) to (%s) in pfio_rename_file",
          fn_ext_old, fn_ext_new);
        fprintf(stderr,"%s\n",pfio_error_string);
        return(-7);
      }
    }
  }
/*pfio_delete_empty_directories(fn_old,0);  */
  return(ifile);
}

/******************** PFIO_CHMOD ******************************
*       chmod a bf file-fn=file name to chmod value mode
*         return >=0, if no error
*
*       Written May 2000 by Charles C Burch
**************************************************************/
int pfio_chmod(char *fn, int mode) {
  int ifile, n, cpsdisk_sw, itemp;
  int64_t isize;
  char fn_ext[260], full_fn[260], base_fn[260], node[PFIO_MAX_NODENAME];

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_chmod: fn=%s, mode=%d, ts=%f\n",
        fn, mode, (float)unix_get_wtime_stamp_c());
  }
  bfio_expand_file_name(fn, full_fn, sizeof(full_fn));
  fn=bfio_get_base_filename(full_fn);
  pfio_check_if_cpsdisk(fn, &cpsdisk_sw);

  ifile=-1;
  n=0;
  while (1) {
    pfio_form_fn_ext(fn, ++ifile, fn_ext);
    isize=bfio_file_size(fn_ext);
    /**fprintf(stderr,
       "isize=%"PRId32", extsz=%"PRId32", ifile=%d\n",isize, ext_size, ifile-1);
    **/
    if(isize<0) break;

    if(cpsdisk_sw<0) {

      /**fprintf(stderr,
         "chmod file=%s, size=%"PRId32", mode=%d\n",fn_ext,isize,mode);
      **/
      if( chmod(fn_ext, (mode_t) mode) == 0 ) n++;
    } else {
      memset(node,0,sizeof(node));
      pfio_decode_old_filename(fn_ext,node,base_fn, &itemp);
/**   fprintf(stderr,"fn=%s, base=%s/n",fn_ext, base_fn);  ***/
      if( chmod(base_fn, (mode_t) mode) == 0 ) n++;
/**   chmod(fn_ext, (mode_t) mode); **/
    }
  }
  return(n);
}

/****************** PFIO_GET_EXT_SIZE **********************
* get extent size to pass out to fortran
*
* Bill Menger, Dec 03, 2000
***********************************************************/
int64_t pfio_get_ext_size(int ifile) {

  if(pfio_init_sw==0) pfio_init();

  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_get_ext_size: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_get_ext_size")) < 0) return(-1L);
  return(file_info[ifile]->ext_size);
}

/************ PFIO_UPDATE_FILE_TIME **********
* Update file date stamp
*
* Written April 2001 by Charles C Burch
*********************************************/
void pfio_update_file_time(char *fn) {
  char full_fn[260], fn_ext[260], node[PFIO_MAX_NODENAME], base_fn[260], cmd[290];
  int  ifile, disk_num;
  int64_t isize;

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_update_file_time, fn=%s, ts=%f\n",
        fn, (float)unix_get_wtime_stamp_c());
  }

  strcpy(full_fn,"");
  strcpy(fn_ext,"");
  strcpy(node,"");
  strcpy(base_fn,"");
  strcpy(cmd,"");

/* get expanded file name */
  bfio_expand_file_name(fn, full_fn, sizeof(full_fn));
  fn=bfio_get_base_filename(full_fn);

/* cycle through file extensions and change time stamp*/
  ifile=-1;
  while (1) {
    pfio_form_fn_ext(fn, ++ifile, fn_ext);
    isize=pfio_file_size(fn_ext);
    if(isize<0) break;

/* ensure current time different than file time */
    if(bfio_get_file_time(fn_ext)==time(NULL)) sleep(1);
/* see if file node known and remote versus local or not known*/
    memset(node,0,sizeof(node));
    pfio_decode_old_filename(fn_ext,node,base_fn, &disk_num);
    if(strcmp(node,"")==0) {
      pfio_get_file_node(fn_ext,node);
      strcpy(base_fn, fn_ext);
    }
    if(pfio_debug_check(1,0)) {
      fprintf(stderr,"node=%s, pfio_host=%s, basefn=%s, ts=%f\n",
          node, pfio_host, base_fn, (float)unix_get_wtime_stamp_c());
    }

    if(strcmp(node,"")!=0 && strcmp(node,pfio_host)!=0) {     /*remote*/
      sprintf(cmd,"%s %s", "\\touch", base_fn);
      /* fprintf(stderr,"cmd=%s\n",cmd); */
      /*** Some nodes can't be used for remote commands.  They are Netapps
           or other servers that a user can't log into.  We must have a way
           to simulate the remote command for them that doesn't break the
           system but that might accomplish what is needed.
       ***/
      if(pfio_check_if_netapp_node(node)) {
        /*fprintf(stderr,"NODE=%s CMD=%s\n",node,cmd);*/
        /*** Then the node is a netapps... need a better way to do this from
         *** a configuration file
         ***/
         if(system(cmd) < 0 ) {
          sprintf(cmd,"Error: Unable to %s %s","\\touch",base_fn);
          cpslog_message(cmd);
         }
      } else {
        if(pfio_remote_command_retry(node,cmd,3,90)<0) {
          sprintf(cmd,"Error: Unable to rsh touch command on node %s", node);
          cpslog_message(cmd);
        }
      }
    } else {
      utime(base_fn,NULL);
    }
  }
  return;
}

/******** PFIO_SET_LOCK_TYPE *************
* Set lock type for next file lock
* gets reset after lock occurs
*
* Written November 2001 by Charles C Burch
*****************************************/
void pfio_set_lock_type(char lock_type) {

  if(pfio_init_sw==0) pfio_init();
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_lock_type: type=%c, ts=%f\n",
        lock_type, (float)unix_get_wtime_stamp_c());
  }
  if(lock_type==' ') {
    fprintf(stderr,
    "lock type can not be blank in pfio_set_lock_type-call ignored\n");
  } else {
    pfio_lock_type=lock_type;
  }
  return;
}

/******************* PFIO_SET_WRITE_ERROR_RECOVERY *********
* set write_error recovery mode (1=yes, 0=no)
*
* Written February 2001 by Charles C Burch
***********************************************************/
int pfio_set_write_error_recovery(int ifile, int mode) {

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_write_error_recovery: ifile=%d, mode=%d, ts=%f\n",
        ifile,mode, (float)unix_get_wtime_stamp_c());
  }
  if( (ifile=pfio_check_file_fd(ifile,"pfio_set_write_error_recovery")) < 0)
   return(-1);

  if(mode!=0) mode=1;
  file_info[ifile]->write_err_recover=mode;

  /* fprintf(stderr,
     "pfio_set_write_error_recovery ifile=%d, mode=%d\n",ifile,mode);
  */

  return(0);
}

/****************** PFIO_SET_TRACE_MODE ********************
* set trace mode: 1=start tracing,
*                -1 stop tracing-purge traced info
*                 0 stop tracing-print/purge traced info
*
* Written March 2001 by Charles C Burch
***********************************************************/
int pfio_set_trace_mode(int mode) {
  char name[260],str[260];

  if(pfio_init_sw==0) pfio_init();
  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,0)) {
    fprintf(stderr,"pfio_set_trace_mode: mode=%d, ts=%f\n",
        mode, (float)unix_get_wtime_stamp_c());
  }
  if(mode<0) {  /*turn off tracing and purge traced info*/
    mode=0;
    lnklst_delete_list(trace_entries);
  }

  if(mode>0) mode=1;
  pfio_trace_mode=mode;

  if(pfio_trace_mode==0) {   /*turn off tracing and print/purge traced info*/
    lnklst_get_list_entry(trace_entries, name,str,-1);   /*delete entry also*/
    while(strlen(name)>0){
      fprintf(stderr,"pfio_trace: %s\n",name);
      lnklst_get_list_entry(trace_entries, name,str,-1); /*delete entry also*/
    }
  }

  return(0);
}

/************************* PFIO_GET_ERROR_INFO ********************
* get error info
*
* Written Feb 2002 by Charles C Burch
*******************************************************************/
void pfio_get_error_info(char *str, INTEGER *N) {
  int i, n;

  if(pfio_init_sw==0) pfio_init();
  n=(*N);
  for(i=0; i<n; i++) {
    if(pfio_error_string[i]=='\0') break;
    str[i]=pfio_error_string[i];
  }

  while(i<n) {
    str[i++]=' ';
  }
  return;
}

/************************* PFIO_GET_FD ************************
* return file pointer associated with an opened file ifile
* returns NULL if ifile is invalid 
*
*       Written November 2005 by Chuck Burch
**************************************************************/
FILE * pfio_get_fd(int ifile, int iext) {
  if(pfio_init_sw==0) pfio_init();
  if( (ifile=pfio_check_file_fd(ifile,"pfio_get_fd")) < 0)return(NULL);
  if(iext<0 || iext>=file_info[ifile]->num_exts) return(NULL);
  return(bfio_get_fd(file_info[ifile]->ext_info[iext].host_ifile));
}

/************************ PFIO_GET_FILENO ***********************
* return fileno associated with an opened file ifile
* returns -1 if ifile is invalid 
*
*       Written November 2005 by Chuck Burch
*******************************************************************/
int pfio_get_fileno(int ifile, int iext) {
  FILE *fd;
  if(pfio_init_sw==0) pfio_init();
  fd=pfio_get_fd(ifile, iext);
  if(fd==NULL) return(-1);
  return(mtio_fileno(fd));
}

/********************** PFIO_GET_FILENAME *****************
* return pointer to file name associated with an opened file ifile
* returns empty string if ifile is invalid 
*
*       Written November 2005 by Chuck Burch
**************************************************************/
char *pfio_get_filename(int ifile) {
  static char empty[ ]=""; 
  if(pfio_init_sw==0) pfio_init();
  if( (ifile=pfio_check_file_fd(ifile,"pfio_get_filename")) < 0) 
    return(empty);
  return(file_info[ifile]->fn);
}

/******************** PFIO_ADD_DESC_FLAGS *********************
*       add to file descriptor flags
*         ifile = file id of file to read
*         flags = the value of flags to add to the file descriptor
*         returns <0 if error
*         returns =0 if no error
*
*       Written November 2005 by Brian Macy
**************************************************************/
int pfio_add_desc_flags(int ifile, int32_t flags) {
  int istat;
  int i_ext;

  if(pfio_init_sw==0) pfio_init();
  istat=0;
  /* pfio_set_debug_mode(1); */

  pfio_error_string[0]='\0';
  if(pfio_debug_check(1,1)) {
    fprintf(stderr,"pfio_add_desc_flags: ifile=%d, ts=%f\n",
        ifile, (float)unix_get_wtime_stamp_c());
  }

  if(flags==0) goto exit;

  if( (ifile=pfio_check_file_fd(ifile,"pfio_add_desc_flags")) < 0) {
    istat=ifile;
    goto exit;
  }  

  /* add file descriptor flags */
  file_info[ifile]->desc_flags|=flags;

  /* check for open extents and modify descriptor flags as needed */
  for (i_ext=0; i_ext<file_info[ifile]->num_exts; i_ext++) {
    if(file_info[ifile]->ext_info[i_ext].host_ifile>=0) {
      istat=bfio_add_desc_flags(file_info[ifile]->ext_info[i_ext].host_ifile,
        flags);
    }
  }

exit:
  io_timeout=0;
  if(pfio_debug_check(1,-1)) {
    fprintf(stderr,"pfio_add_desc_flags-exit: istat=%d, ts=%f\n",
        istat, (float)unix_get_wtime_stamp_c());
  }
  return(istat);
}

# ifdef pfioUnitTest
/******************************************************************************
* To compile with UnitTest, include -DpfioUnitTest in the gcc statement.
* gcc -o pfioUnitTest -g -Wall -DLINUXA -D_REENTRANT \
*    -D_FILE_OFFSET_BITS=64 -DpfioUnitTest\
*    pfio.c bfio.c mtio.c cb.c str.c unix-crou.c lnklst_crou.c exptilde_crou.c\
*    cpslog.c cnfg_crou.c
*    [or ...betalib.a]  -lm -lpthread
* and you need these .h files:
* pfio.h bfio.h cb.h cpslog.h exptilde_crou.h mtio.h str.h c2f_interface.h cnfg.h
* unix.h lnklst.h named_constants.h cgetsys.h
* [or ... -I to an include directory for cps]
*
* pfioUnitTest executes a series of tests. It quits at first error and 
* indicates the error found. If no errors are found, it says so.
*
* As of 11/2005 there are four tests:
*  test0-tests basic single file and multiple file operations
*  test1-tests multiple-file-extent files for normal, cpsdata & cpstemp
*  test2-tests specifying extent size, file size and filespace reserve
*  test3-tests larger file operations-useful with custom cpsdata_nodes.dat
*        to test extent allocation 
*
* The default tests run are test0, test1, test2, and test3.
* To run specfic tests execute pfioUnitTest [-0 [-1 [-2 [-3]]], where
* -0 means to run test0, etc. You can specify 1 2 3 or 4 tests to run.
*  
* Additional tests should be added as time allows, when new features 
* are added or when bugs are found that the current tests do not find.
*
* Original version written October 2005 by Charles Burch
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include "pfio.h"
#include "mtio.h"

/*Note CYGWIN a/o 11/2005 does not have O_SYNC defined */
#ifndef O_ASYNC
#define O_ASYNC O_NONBLOCK
#endif

int pfio_test0() {
#define NFILES 10
  int i,j, errs, ifiles[NFILES], flsz, fileno;
  char buff[80], fn[80];
  int32_t flags;

  errs=0;
  for(i=0;i<NFILES;i++) ifiles[i]=-1;
  printf("test0:Testing basic single file pfio_open/write/read/close\n");

  sprintf(fn, "test%2.2d.dat", 0);
  pfio_delete(fn);   /*just to unlock file, if needed*/

  fprintf(stderr,"  testing basic open(%s)\n",fn);
  if( (ifiles[0]=pfio_open(fn,'u')) < 0) {
    fprintf(stderr,"pfio_open error: filename=%s, ifile=%d\n", fn, ifiles[0]);
    errs++;
    goto done;
  }

  fprintf(stderr,"  testing basic write\n");
  for(i=0;i<80;i++) buff[i]=i;
  if( (i=pfio_write(ifiles[0],buff,80)) !=80) {
    fprintf(stderr,"pfio_write error: returned=%d, should be 80\n",i);
    errs++;
    goto done;
  }

  fprintf(stderr,"  testing basic seek\n");
  if( (i=pfio_seek(ifiles[0],0)) != 0) {
    fprintf(stderr,"pfio_seek error: returned=%d, should be 0\n",i);
    errs++;
    goto done;
  }

  fprintf(stderr,"  testing basic read\n");
  memset(buff,0,80);
  if( (i=pfio_read(ifiles[0],buff,80)) != 80){
    fprintf(stderr,"pfio_read error: returned=%d, should be 80\n",i);
    errs++;
    goto done;
  }

  for(i=0;i<80;i++) {
    if(buff[i] != i) {
      fprintf(stderr,"pfio_read error at pos=%d: read=%d, should=%d\n",
             i, i, (int)buff[i]);
      errs++;
    }
  }
  if(errs>0) goto done;

  for(j=79;j>=0;j--) {
    if( (i=pfio_seek(ifiles[0],j)) != 0) {
      fprintf(stderr,"pfio_seek error1, pos=%d: returned=%d, should be 0\n", 
       j, i);
      errs++;
      goto done;
    }

    memset(buff,0,80);
    if( (i=pfio_read(ifiles[0],buff,80)) != (80-j) ){
      fprintf(stderr,"pfio_read error1, pos=%d: returned=%d, should be %d\n",
        j, i, 80-j);
      errs++;
      goto done;
    }

    for (i=j; i<80; i++) {
      if(buff[i-j] != i) {
       fprintf(stderr,"pfio_read error1,pos=%d, index=%d: read=%d, should=%d\n",
            j, i, i, (int)buff[i-j]);
        errs++;
      }
    }
  }

  fprintf(stderr,"  Testing seek via origin\n");
  j=40;      /* SEEK_SET*/
  if( (i=pfio_seek_via_origin(ifiles[0],j, SEEK_SET)) != 0) {
    fprintf(stderr,"pfio_seek SEEK_SET error: returned=%d, should be 0\n",i);
    errs++;
    goto done;
  }

  if( (i=pfio_tell(ifiles[0])) != j) {
    fprintf(stderr,"pfio_tell SEEK_SET error: returned=%d, should be %d\n",i,j);
    errs++;
    goto done;
  }

  buff[0]=-1;
  if( (i=pfio_read(ifiles[0],buff,1)) != 1) {
    fprintf(stderr,
     "pfio_read SEEK_SET error, pos=%d: returned=%d, should be %d\n",
      j, i, j);
    errs++;
    goto done;
  }

  if((int)buff[0] != j) {
    fprintf(stderr,
      "pfio_read SEEK_SET data error,pos=%d: read=%d, should=%d\n",
      j, (int)buff[0], j);
    errs++;
    goto done;
  }

  j=20;      /* SEEK_CUR */
  if( (i=pfio_seek_via_origin(ifiles[0],-21, SEEK_CUR)) != 0) {
    fprintf(stderr,"pfio_seek SEEK_CUR error: returned=%d, should be 0\n",i);
    errs++;
    goto done;
  }

  if( (i=pfio_tell(ifiles[0])) != j) {
    fprintf(stderr,"pfio_tell SEEK_CUR error: returned=%d, should be %d\n",i,j);
    errs++;
    goto done;
  }

  buff[0]=-1;
  if( (i=pfio_read(ifiles[0],buff,1)) != 1) {
    fprintf(stderr,
      "pfio_read SEEK_CUR error, pos=%d: returned=%d, should be %d\n",
      j, i, j);
    errs++;
    goto done;
  }

  if((int)buff[0] != j) {
    fprintf(stderr,
      "pfio_read SEEK_CUR data error,pos=%d: read=%d, should=%d\n",
      j, (int)buff[0], j);
    errs++;
    goto done;
  }

  j=79;      /* SEEK_END */
  if( (i=pfio_seek_via_origin(ifiles[0],-1, SEEK_END)) != 0) {
    fprintf(stderr,"pfio_seek SEEK_CUR error: returned=%d, should be 0\n",i);
    errs++;
    goto done;
  }

  if( (i=pfio_tell(ifiles[0])) != j) {
    fprintf(stderr,"pfio_tell SEEK_END error: returned=%d, should be %d\n",i,j);
    errs++;
    goto done;
  }

  buff[0]=-1;
  if( (i=pfio_read(ifiles[0],buff,1)) != 1) {
    fprintf(stderr,
      "pfio_read SEEK_END error, pos=%d: returned=%d, should be %d\n",
      j, i, j);
    errs++;
    goto done;
  }

  if((int)buff[0] != j) {
    fprintf(stderr,
      "pfio_read SEEK_END data error,pos=%d: read=%d, should=%d\n",
      j, (int)buff[0], j);
    errs++;
    goto done;
  }


  if( (i=pfio_close(ifiles[0])) < 0) {
    fprintf(stderr,"pfio_close error: status=%d, should be >=0\n", i);
    errs++;
    goto done;
  }
  ifiles[0]=-1;
  if(errs>0) goto done;

  fprintf(stderr,"  Testing multiple file opens\n");
  for(j=1; j<NFILES; j++) {
    sprintf(fn,"test%2.2d.dat",j);
    pfio_delete(fn);   /*just to unlock file, if needed*/
    if( (ifiles[j]=pfio_open(fn,'u')) < 0) {
      fprintf(stderr,"pfio_open error: filename=%s, ifile=%d\n", fn, ifiles[j]);
      errs++;
      goto done;
    }
  }

  fprintf(stderr,"  Testing multiple file writes\n");
  for(j=1; j<NFILES; j++) {
    for(i=0;i<80;i++) buff[i]=i;
    if( (i=pfio_write(ifiles[j],buff,80)) !=80) {
      fprintf(stderr,"pfio_write error, file#=%d: returned=%d, should be 80\n",
       j, i);
      errs++;
      goto done;
    }
  }

  fprintf(stderr,"  Testing multiple file seeks\n");
  for(j=1; j<NFILES; j++) {
    if( (i=pfio_seek(ifiles[j],0)) != 0) {
      fprintf(stderr,"pfio_seek error, file#=%d: returned=%d, should be 0\n",
        j, i);
      errs++;
      goto done;
    }
  }

  fprintf(stderr,"  Testing multiple file reads\n");
  for(j=1; j<NFILES; j++) {
    memset(buff,0,80);
    if( (i=pfio_read(ifiles[j],buff,80)) != 80){
      fprintf(stderr,"pfio_read error, file#=%d: returned=%d, should be 80\n",
         j, i);
      errs++;
      goto done;
    }

    for(i=0;i<80;i++) {
      if(buff[i] != i) {
        fprintf(stderr,"pfio_read error at pos=%d: read=%d, should=%d\n",
             i, i, (int)buff[i]);
        errs++;
      }
    }
    if(errs>0) goto done;
  }
  fprintf(stderr,"  Testing multiple file closes, get_filename & file sizes\n");
  for(j=1; j<NFILES; j++) {
    sprintf(fn,"test%2.2d.dat",j);
    if(strstr(pfio_get_filename(ifiles[j]),fn)==NULL) {
      fprintf(stderr,"pfio_get_filename error, got=%s, expected=%s\n",
        pfio_get_filename(ifiles[j]), fn);
      errs++;
    }
    if( (i=pfio_close(ifiles[j])) < 0) {
      fprintf(stderr,"pfio_close error on file#%d, status=%d, should >=0\n",
       j,i);
      errs++;
    }
    strcpy(fn,"");
    if(strcmp(pfio_get_filename(ifiles[j]),fn)!=0) {
      fprintf(stderr,"pfio_get_filename error, got=%s, expected=%s\n",
        pfio_get_filename(ifiles[j]), fn);
      errs++;
    }
    ifiles[j]=-1;
    if(errs>0) goto done;
  }

  for(j=0; j<NFILES; j++) {
    sprintf(fn,"test%2.2d.dat",j);
    if( (flsz=pfio_flsz(fn))!=80) {
      fprintf(stderr,"file size error on %s, size=%d, should be 80\n",fn,flsz);
      errs++;
    }
  }

  fprintf(stderr,"  testing add_desc_flags\n");
  sprintf(fn, "test%2.2d.dat", 0);
  if( (ifiles[0]=pfio_open(fn,'u')) < 0) {
    fprintf(stderr,"pfio_open error: filename=%s, ifile=%d\n", fn, ifiles[0]);
    errs++;
    goto done;
  }

  if( (fileno=pfio_get_fileno(ifiles[0],0))<0){
    fprintf(stderr,"pfio_get_fd error\n");
    errs++;
    goto done;
  }

  flags=fcntl(fileno,F_GETFL);
  if((flags & O_NONBLOCK) !=0 || (flags & O_ASYNC)!=0) { 
    fprintf(stderr,
     "Warning: pfio_add_desc_flags already set=%x, O_NONBLOCK=%x, O_ASYNC=%x\n",
     (int)flags, O_NONBLOCK, O_ASYNC);
  }

  if((i=pfio_add_desc_flags(ifiles[0],O_NONBLOCK))!=0) {
    fprintf(stderr,"pfio_add_desc_flags(O_NONBLOCK) error\n");
    errs++;
    goto done;
  }

  if((i=pfio_add_desc_flags(ifiles[0],O_ASYNC))!=0) {
    fprintf(stderr,"pfio_add_desc_flags(O_ASYNC) error\n");
    errs++;
    goto done;
  }

  flags=fcntl(fileno,F_GETFL);
  if((flags & O_NONBLOCK) ==0 || (flags & O_ASYNC)==0) { 
    fprintf(stderr,
     "pfio_add_desc_flags error\flags set=%x, O_NONBLOCK=%x, O_ASYNC=%x\n", 
     (int)flags, O_NONBLOCK, O_ASYNC);
    errs++;
    goto done;
  }

  if( (i=pfio_close(ifiles[0])) < 0) {
    fprintf(stderr,"pfio_close error: status=%d, should be >=0\n", i);
    errs++;
    goto done;
  }
  ifiles[0]=-1;
  if(errs>0) goto done;

  

done:
  for(j=0; j<NFILES; j++) {
    if(ifiles[j]>=0) pfio_close(ifiles[j]);
    sprintf(fn,"test%2.2d.dat%c",j,'\0');
    pfio_delete(fn);
  }

  return(errs);
}

int pfio_test1(int mode) {
  int i,j, errs, flsz, ifile, commit_sw, cpsdata_sw, nexts, fileno;
  char buff[80], fn[80];
  int32_t ext_sz, flags;

  errs=0;
  commit_sw=0;
  cpsdata_sw=0;
  ext_sz=0;
  strcpy(fn,"test.dat");
  if(mode==0) {
    fprintf(stderr,"test1:Testing basic multiple extent file operations\n");
  } else if(mode==1) {
    fprintf(stderr,"  Testing basic operations with multiple extent file\n");
    ext_sz=10;
  } else if(mode==2) {
    fprintf(stderr,
      "  Testing basic operation with multiple extent file/file reserve\n");
    ext_sz=10;
    commit_sw=1;
  } else if(mode==3) {
    fprintf(stderr,"  Testing cpstemp basic operation\n");
    ext_sz=10;
    strcpy(fn,"./cpstemp/test.dat");
  } else if(mode==4) {
    fprintf(stderr,"  Testing cpsdata basic operation\n");
    ext_sz=10;
    commit_sw=1;
    strcpy(fn,"./cpsdata/test.dat");
  } else {
    return(errs);
  }

  if(ext_sz>0) pfio_set_ext_size(ext_sz);
  pfio_set_file_space_commit(commit_sw);
  if( (ifile=pfio_open(fn,'w')) < 0) {
    fprintf(stderr,"pfio_open error: filename=%s, ifile=%d\n", fn, ifile);
    errs++;
    goto done;
  }

  for(i=0;i<80;i++) buff[i]=i;
  if( (i=pfio_write(ifile,buff,80)) !=80) {
    fprintf(stderr,"pfio_write error: returned=%d, should be 80\n",i);
    errs++;
    goto done;
  }

  if( (i=pfio_seek(ifile,0)) != 0) {
    fprintf(stderr,"pfio_seek error: returned=%d, should be 0\n",i);
    errs++;
    goto done;
  }

  memset(buff,0,80);
  if( (i=pfio_read(ifile,buff,80)) != 80){
    fprintf(stderr,"pfio_read error: returned=%d, should be 80\n",i);
    errs++;
    goto done;
  }

  for(i=0;i<80;i++) {
    if(buff[i] != i) {
      fprintf(stderr,"pfio_read error at pos=%d: read=%d, should=%d\n",
             i, i, (int)buff[i]);
      errs++;
    }
  }
  if(errs>0) goto done;

  for(j=79;j>=0;j--) {
    if( (i=pfio_seek(ifile,j)) != 0) {
      fprintf(stderr,"pfio_seek error1, pos=%d: returned=%d, should be 0\n",
        j, i);
      errs++;
      goto done;
    }

    memset(buff,0,80);
    if( (i=pfio_read(ifile,buff,80)) != (80-j) ){
      fprintf(stderr,"pfio_read error1, pos=%d: returned=%d, should be %d\n",
        j, i, 80-j);
      errs++;
      goto done;
    }

    for (i=j; i<80; i++) {
      if(buff[i-j] != i) {
        fprintf(stderr,
            "pfio_read error1,pos=%d, index=%d: read=%d, should=%d\n",
            j, i, i, (int)buff[i-j]);
        errs++;
      }
    }
  }

  nexts=1;
  if(ext_sz>0) nexts=(80+ext_sz-1)/ext_sz;

  if(mode==1) {
    fprintf(stderr,"  testing multiple extent add_desc_flags\n");
    if((i=pfio_add_desc_flags(ifile,O_NONBLOCK))!=0) {
      fprintf(stderr,"pfio_add_desc_flags(O_NONBLOCK) error\n");
      errs++;
      goto done;
    }

    if((i=pfio_add_desc_flags(ifile,O_ASYNC))!=0) {
      fprintf(stderr,"pfio_add_desc_flags(O_ASYNC) error\n");
      errs++;
      goto done;
    }

    for(j=0; j<nexts; j++) {
      if( (fileno=pfio_get_fileno(ifile,j))<0){
        fprintf(stderr,"pfio_get_fileno error, ifile=%d, j=%d\n", ifile,j);
        errs++;
        goto done;
      }

      flags=fcntl(fileno,F_GETFL);
      if((flags & O_NONBLOCK) ==0 || (flags & O_ASYNC)==0) { 
        fprintf(stderr,
         "pfio_add_desc_flags error, ifile=%d, ext=%d\n",ifile,j);
        fprintf(stderr, "  flags set=%x, O_NONBLOCk=%x, O_ASYNC=%x\n", 
         (int)flags, O_NONBLOCK, O_ASYNC);
        errs++;
        goto done;
      }
    }
  }

  i=pfio_close(ifile);
  if(i!=nexts) {
    if(i<nexts) {
      fprintf(stderr,"pfio_close error: status=%d, should be %d\n", i,nexts);
      errs++;
      goto done;
    } else {
      fprintf(stderr,
       " Warning pfio close:#extents=%d, normally would=%d\n",i,nexts);
    }
  }
  ifile=-1;

  if( (flsz=pfio_flsz(fn))!=80) {
    fprintf(stderr,"file size error on %s, size=%d, should be 80\n",fn,flsz);
    errs++;
  }


done:
  if(ifile>=0) pfio_close(ifile);
  pfio_delete(fn);

  return(errs);
}

int pfio_test2(char *file_type, int64_t f_size, int32_t extsz,
  int commit_sw) {
  int ifile, i, j, n, errs, cnt;
  char filename[80], buff[80];
  int64_t offset, lstat;
  int32_t jump;

  /*pfio_set_debug_mode(1); */
  sprintf(filename,"./%s/test.dat",file_type);
  errs=0;
  if(f_size<3) f_size=3;
  fprintf(stderr,
    "  testing %s functions: extsz=%"PRId32", flsz=%"PRId64", commit=%d\n",
    file_type, extsz, f_size, commit_sw);

  pfio_delete(filename);
  pfio_set_ext_size(extsz);
  pfio_set_file_space_commit(commit_sw);
  if( (ifile=pfio_open(filename,'w'))<0) {
    fprintf(stderr,"pfio_open(%s) error:status=%d\n",filename,ifile);
    errs++;
    goto done;
  }

  jump=extsz/2;
  if(jump>f_size)  jump=f_size/2;

  offset=jump;
  cnt=1;
  while(offset<f_size){
    if((offset+2) > f_size) {
      n=f_size-(offset-1);
    } else {
      n=3;
    }

    if( (j=pfio_seek(ifile,offset-1))!=0) {
      fprintf(stderr,"pfio_seek(%"PRId64") error: stat=%d\n",offset-1, j);
      errs++;
      goto done;
    }

    if( (lstat=pfio_tell(ifile))!=(offset-1)) {
      fprintf(stderr,
        "pfio_tell error before write: stat=%"PRId64", should be %"PRId64"\n",
        lstat, offset-1);
      errs++;
      goto done;
    }

    buff[0]=(cnt++)%256;
    buff[1]=(cnt++)%256;
    buff[2]=(cnt++)%256;
    if( (j=pfio_write(ifile,buff,n))!=n) {
      fprintf(stderr,"pfio_write error, off=%"PRId64", stat=[%d]/should be %d\n",
          offset, j, n);
      errs++;
      goto done;
    }

    if( (lstat=pfio_tell(ifile))!=(offset-1+n)) {
      fprintf(stderr,"pfio_tell error after write: status=%"PRId64", should=%"PRId64"\n",
        lstat,offset-1+n);
      errs++;
      goto done;
    }

    if((offset+n)>=f_size) break;
    offset+=jump;
    if(offset>=f_size) offset=f_size-1;
  }


  if( (lstat=pfio_tell(ifile))!=f_size) {
    fprintf(stderr,
      "pfio_tell error after all writes: status=%"PRId64", should=%"PRId64"\n",
      lstat,f_size);
    errs++;
    goto done;
  }

  if( (i=pfio_seek_via_origin(ifile,-jump,SEEK_CUR))!=0) {
    fprintf(stderr,
        "pfio_seek_via_origin(%"PRId32",SEEK_CUR) error: status=%d, should=0\n",
        -jump, i);
    errs++;
    goto done;
  }

  if( (lstat=pfio_tell(ifile))!=(f_size-jump)) {
    fprintf(stderr,"pfio_tell error after SEEK_CUR: status=%"PRId64", should=%"PRId64"\n",
      lstat,f_size-jump);
    errs++;
    goto done;
  }

  if( (i=pfio_seek_via_origin(ifile,0,SEEK_END))!=0) {
    fprintf(stderr,
     "pfio_seek_via_origin(0,SEEK_END) error: status=%d, should=0\n",i);
    errs++;
    goto done;
  }

  if( (lstat=pfio_tell(ifile))!=f_size) {
    fprintf(stderr,"pfio_tell error after SEEK_END: status=%"PRId64", should=%"PRId64"\n",
      lstat,f_size);
    errs++;
    goto done;
  }

  if( (i=pfio_seek_via_origin(ifile,0,SEEK_SET))!=0) {
    fprintf(stderr,
     "pfio_seek_via_origin(0,SEEK_SET) error: status=%d, should=0\n",i);
    errs++;
    goto done;
  }

  if( (lstat=pfio_tell(ifile))!=0) {
    fprintf(stderr,
     "pfio_tell error after SEEK_SET: status=%"PRId64", should=0\n", lstat);
    errs++;
    goto done;
  }

  cnt=1;
  offset=jump;
  while(offset<f_size) {
    if((offset+2) > f_size) {
      n=f_size-(offset-1);
    } else {
      n=3;
    }

    if( (j=pfio_seek(ifile,offset-1))!=0) {
      fprintf(stderr,"pfio_seek(%"PRId64") error: stat=%d\n",offset-1, j);
      errs++;
      goto done;
    }

    if( (lstat=pfio_tell(ifile))!=offset-1) {
      fprintf(stderr,"pfio_tell error before read: stat=%"PRId64", should be %"PRId64"\n",
        lstat, offset-1);
      errs++;
      goto done;
    }

    if( (j=pfio_read(ifile,buff,n))!=n) {
      fprintf(stderr,"pfio_read error, off=%"PRId64", stat=[%d]/should be %d\n",
          offset, j, n);
      errs++;
      goto done;
    }

    if( (lstat=pfio_tell(ifile))!=(offset-1+n)) {
      fprintf(stderr,"pfio_tell error after read: status=%"PRId64", should=%"PRId64"\n",
        lstat,offset-1+n);
      errs++;
      goto done;
    }

    for(i=0; i<3 && offset-1+i<f_size; i++) {
      if(buff[i] != (cnt+i)%256) {
        fprintf(stderr,
          "pfio_read data error, offset=%"PRId64", i=%d, read=%d, should=%d\n",
          offset-1,i, buff[i], (cnt+i)%256);
        errs++;
      }
    }
    if(errs>0) goto done;
    cnt+=3;

    if((offset+n)>=f_size) break;
    offset+=jump;
    if(offset>=f_size) offset=f_size-1;
  }


  if( (lstat=pfio_tell(ifile))!=f_size) {
    fprintf(stderr,
      "pfio_tell error after all reads: status=%"PRId64", should=%"PRId64"\n",
      lstat,f_size);
    errs++;
    goto done;
  }
/*
 sprintf(buff,"ls -al %s*",filename);
 system(buff);
 sprintf(buff,"ls -alL %s*",filename);
 system(buff);
 */
  j=(f_size+extsz-1)/extsz;
  i=pfio_close(ifile);
  if(i!=j) {
    if(i<j) {
      fprintf(stderr,"pfio_close error: status=%d, should be %d\n", i,j);
      errs++;
      goto done;
    } else {
      fprintf(stderr,
       " Warning pfio close:#extents=%d, normally would=%d\n",i,j);
    }
  }

  if( (lstat=pfio_flsz(filename)) != f_size) {
    fprintf(stderr,"file size error after close: status=%"PRId64", should=%"PRId64"\n",
        lstat, f_size);
    errs++;
    goto done;
  }

  if( (i=pfio_truncate(filename, f_size/2))!=0) {
    fprintf(stderr,"pfio_truncate error: status=%d, should=0\n",i);
    errs++;
    goto done;
  }

  if( (lstat=pfio_flsz(filename)) != f_size/2) {
    fprintf(stderr,"file size error after close: status=%"PRId64", should=%"PRId64"\n",
        lstat, f_size/2);
    errs++;
    goto done;
  }

  j=(f_size/2+extsz-1)/extsz;
  if( (i=pfio_delete(filename))<j) {
    fprintf(stderr,"pfio_delete error:status=%d, should=%d\n",i,j);
    errs++;
    goto done;
  }

done:
  if(ifile>=0) pfio_close(ifile);
  pfio_delete(filename);

  return(errs);
}

int pfio_test3(char *file_type, int64_t f_size, int64_t extsz,
  int commit_sw) {
#define NBUFF 10000
  int ifile, i, j, n, errs;
  char filename[80], cmd[80];
  int64_t offset, lstat, buff[NBUFF];

  /*pfio_set_debug_mode(1); */
  sprintf(filename,"./%s/test.dat",file_type);
  errs=0;
  n=sizeof(int64_t);
  if(f_size<n) f_size=n;
  f_size=((f_size+n-1)/n)*n;
  fprintf(stderr,
    "  testing %s functions: extsz=%"PRId64", flsz=%"PRId64", commit=%d\n",
    file_type, extsz, f_size, commit_sw);

  pfio_delete(filename);
  pfio_set_ext_size(extsz);
  pfio_set_file_space_commit(commit_sw);
  if( (ifile=pfio_open(filename,'w'))<0) {
    fprintf(stderr,"pfio_open(%s) error:status=%d\n",filename,ifile);
    errs++;
    goto done;
  }

  offset=0;
  while(offset<f_size){
    for(i=0; i<NBUFF; i++) {
      buff[i]=offset+i;
    }
    n=sizeof(buff);
    if((offset+n)>f_size) n=f_size-offset;
    if( (j=pfio_write(ifile,(char*)buff,n))!=n) {
      fprintf(stderr,"pfio_write error, off=%"PRId64", stat=[%d]/should be %d\n",
          offset, j, n);
      errs++;
      goto done;
    }
    offset+=n;
  }
  fprintf(stderr,"  test3, write completed\n"); fflush(stdout);


  if( (lstat=pfio_tell(ifile))!=f_size) {
    fprintf(stderr,
      "pfio_tell error after all writes: status=%"PRId64", should=%"PRId64"\n",
      lstat,f_size);
    errs++;
    goto done;
  }

  if( (i=pfio_seek_via_origin(ifile,0,SEEK_SET))!=0) {
    fprintf(stderr,
      "pfio_seek_via_origin(0,SEEK_SET) error: status=%d, should=0\n",i);
    errs++;
    goto done;
  }

  if( (lstat=pfio_tell(ifile))!=0) {
    fprintf(stderr,
     "pfio_tell error after SEEK_SET: status=%"PRId64", should=0\n", lstat);
    errs++;
    goto done;
  }

  offset=0;
  while(offset<f_size) {
    n=sizeof(buff);
    if((offset+n)>f_size) n=f_size-offset;
    if( (j=pfio_read(ifile,(char*)buff,n))!=n) {
      fprintf(stderr,"pfio_read error, off=%"PRId64", stat=[%d]/should be %d\n",
          offset, j, n);
      errs++;
      goto done;
    }

    for(i=0; i<n/sizeof(int64_t); i++) {
      if(buff[i] != (offset+i)) {
        fprintf(stderr,
          "pfio_read data error, offset=%"PRId64", i=%d, read=%"PRId64", should=%"PRId64"\n",
          offset,i, buff[i], offset+i);
        errs++;
      }
    }
    if(errs>0) goto done;
    offset+=n;
  }

  j=(f_size+extsz-1)/extsz;
  i=pfio_close(ifile);
  ifile=-1;
  if(i!=j) {
    if(i<j) {
      fprintf(stderr,"pfio_close error: status=%d, should be %d\n", i,j);
      errs++;
      goto done;
    } else {
      fprintf(stderr,
       " Warning pfio close:#extents=%d, normally would=%d\n",i,j);
    }
  }

  sprintf(cmd,"ls -al %s*",filename);
  system(cmd);
  sprintf(cmd,"ls -alL %s*",filename);
  system(cmd);

done:
  if(ifile>=0) pfio_close(ifile);
  pfio_delete(filename);
  return(errs);
}

int main(int argc, char *argv[]) {
  int errs, nerrs, i, do_test0=1, do_test1=1, do_test2=1, do_test3=1;
  char *cptr;

  /*************************************************
  pfio_init();
  printf("netapp_node_name1=%s\n",netapp_node_name1);
  printf("netapp_node_name2=%s\n",netapp_node_name2);
  printf("netapp_node_name3=%s\n",netapp_node_name3);
  printf("netapp_node_name4=%s\n",netapp_node_name4);
  printf("netapp_node_name5=%s\n",netapp_node_name5);
  printf("netapp_node_name6=%s\n",netapp_node_name6);
  printf("netapp_node_name7=%s\n",netapp_node_name7);
  printf("netapp_node_name8=%s\n",netapp_node_name8);
  printf("netapp_node_name9=%s\n",netapp_node_name9);
  printf("netapp_node_name10=%s\n",netapp_node_name10);
  printf("cps_default_disk_node=%s\n",cps_default_disk_node);

  cptr="/hoepld96/temp.dat";
  printf("pfio_check_if_netapp(%s)=%d\n",cptr,pfio_check_if_netapp_node(cptr));
  cptr="/hoepld76/temp.dat";
  printf("pfio_check_if_netapp(%s)=%d\n",cptr,pfio_check_if_netapp_node(cptr));
  cptr="/hoepld26/temp.dat";
  printf("pfio_check_if_netapp(%s)=%d\n",cptr,pfio_check_if_netapp_node(cptr));
  **********************************************/
  nerrs=0;
  fprintf(stderr,"Start of pfioUnitTest\n\n");
  /** mtio_set_debug_mode(1); **/
  /** pfio_set_debug_mode(1); **/

  if(argc>1) {
    do_test0=0;
    do_test1=0;
    do_test2=0;
    do_test3=0;

    for(i=1; i<argc; i++) {
      cptr=argv[i];
      if(cptr[0]=='-') cptr++;
      if(strcmp(cptr,"0")==0) do_test0=1;
      if(strcmp(cptr,"1")==0) do_test1=1;
      if(strcmp(cptr,"2")==0) do_test2=1;
      if(strcmp(cptr,"3")==0) do_test3=1;
    }
  }

  if(do_test0) {
    errs=pfio_test0();
    if( (nerrs+=errs) ==0) {
      fprintf(stderr,"Test0 completed with no errors\n\n");
    } else {
      fprintf(stderr,"Test0 stopped with errors=%d\n\n", nerrs);
      goto done;
    }
  }

  if(do_test1) {
    errs =pfio_test1(0);     /*basic operations*/
    if(errs==0) errs+=pfio_test1(1);     /*file extents*/
    if(errs==0) errs+=pfio_test1(2);     /*file extents/commit*/
    if(errs==0) errs+=pfio_test1(3);     /*cpstemp*/
    if(errs==0) errs+=pfio_test1(4);     /*cpsdata*/
    if( (nerrs+=errs) ==0) {
      fprintf(stderr,"Test1 completed with no errors\n\n");
    } else {
      fprintf(stderr,"Test1 stopped with errors=%d\n\n", nerrs);
      goto done;
    }
  }

  if(do_test2) {
    fprintf(stderr,"Performing test2\n");
    /*cpsdata tests-small/nocom*/
    errs=pfio_test2("cpsdata",85, 10, -1);
    /*cpsdata tests-small/commit*/
    if(errs==0) errs+=pfio_test2("cpsdata",85, 10, 1);
    /*cpsdata tests-mid/npcom*/
    if(errs==0) errs+=pfio_test2("cpsdata",900000,300000,-1);
    /*cpsdata tests-mid/commit*/
    if(errs==0) errs+=pfio_test2("cpsdata",900000,300000,1);

    /*cpstemp tests-small/nocom*/
    if(errs==0)errs+=pfio_test2("cpstemp",85, 10, -1);
    /*cpstemp tests-small/commit*/
    if(errs==0) errs+=pfio_test2("cpstemp",85, 10, 1);
    /*cpstemp tests-mid/npcom*/
    if(errs==0) errs+=pfio_test2("cpstemp",900000,300000,-1);
    /*cpstemp tests-mid/commit*/
    if(errs==0) errs+=pfio_test2("cpstemp",900000,300000,1);

    if( (nerrs+=errs) ==0) {
      fprintf(stderr,"Test2 completed with no errors\n\n");
    } else {
      fprintf(stderr,"Test2 stopped with errors=%d\n\n", nerrs);
      goto done;
    }
  }

  if(do_test3) {
    fprintf(stderr,"Performing test3\n");
    errs=pfio_test3("cpsdata",20000000000, 11000000000, 1);
    if( (nerrs+=errs) ==0) {
      fprintf(stderr,"Test3 completed with no errors\n\n");
    } else {
      fprintf(stderr,"Test3 stopped with errors=%d\n\n", nerrs);
      goto done;
    }
  }

done:
  pfio_exit();
  if(nerrs==0) {
    fprintf(stderr,"pfioUnitTest completed with no errors\n");
  } else {
    fprintf(stderr,"pfioUnitTest stopped with errors=%d\n", nerrs);
  }

  return(-nerrs);
}

#endif

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
