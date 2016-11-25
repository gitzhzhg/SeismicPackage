!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- path.f90 --------------------------------!!
!!---------------------------- path.f90 --------------------------------!!
!!---------------------------- path.f90 --------------------------------!!


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
! Name       : PATH
! Category   : io
! Written    : 1996-12-06   by: Tom Stoeckley
! Revised    : 2007-08-30   by: R.S.Day
! Maturity   : beta
! Purpose    : File path manipulation utility.
! Portability: No known limitations, but see notes below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This primitive is used to check and adjust file name syntax.  It does not
! check for the status of any actual file with the specified name.  For that
! purpose, the FINQUIRE primitive should be used.
!
! CPS standards specify that when a user provides to a process module a
! pathname with a relative directory path (here defined as not starting
! with / or ~), the PATHNAME_DIR parameter from the Job Data screen will
! be prepended to the pathname.  The user of course can then edit this
! pathname as desired.
!
! A relative pathname is dangerous because it would be relative to the
! directory in which the job is executed, and for batch jobs this might be
! a temporary directory which does not exist before or after the job runs.
! But if the pathname is entered in an interactive environment (such as CFE),
! this pathname will be expanded immediately to an absolute path, so that
! when this pathname is subsequently used in a batch job, it will no longer
! be a relative path.
!
!-------------------------------------------------------------------------------
!               WHEN TO USE THE ROUTINES IN THIS PRIMITIVE
!
! PATH_FIXUP_FILENAME should be called whenever a user supplies a pathname
! to the program.  For example, this routine is called from the PATHCHECK
! primitive, and can be called whenever the user enters a pathname in an
! interactive program such as CBYT.
!
! PATH_FIXUP_DIRECTORY should be called whenever a user supplies a directory
! path to the program.  For example, this routine can be called from the
! Job Data screen in CFE.
!
! All other routines in this primitive are probably no longer needed except
! when used in conjunction with the RCPFILE primitive to access files on a
! remote node that is not NFS mounted.  These other routines are used to
! check and adjust more complicated file name syntax which is understood by
! RCPFILE and the RCP remote node file copy utility.  They may be removed
! in the future, and those needed by RCPFILE moved to that primitive.  Any
! other code which calls this PATH primitive should be reviewed to determine
! whether the calls are necessary.
!
!-------------------------------------------------------------------------------
!                 ROUTINES USED IN CONJUNCTION WITH RCPFILE
!
! This primitive contains several small utilities for validating and
! manipulating names of disk files on remote nodes and the parts into which
! these names can be broken.
!
! The parts of a full file name (called a PATHNAME) can include the
! following components:
!
!              USERID = user ID on the remote node.
!              NODE   = name of the remote node.
!              DIR    = relative or absolute directory path.
!              FILE   = proper file name (including extension).
!
! The format of PATHNAME should be any format which is accepted by the
! unix cp or rcp commands.  This means that PATHNAME should look like
! this (with some components optionally missing):
!
!                      userid@node:dir/file
!
! The slash (/) before FILE is considered part of DIR.
!
!                            +++++++++++++++++++
!
! USERID details:
!  (1) Must consist of lower case letters and digits only.
!  (2) Can be omitted if the user on the remote node is the same as on the
!       local node, or if DIR starts with a tilde (identifying the home
!       directory) or a slash (absolute path).
!  (3) Must be omitted if NODE is omitted.
!  (4) The at sign (@) in PATHNAME is not considered part of USERID.
!  (5) The at sign (@) in PATHNAME must be omitted if USERID is omitted.
!
! NODE details:
!  (1) Must consist of lower case letters and digits only, beginning with
!       a letter.
!  (2) Can be omitted if the remote node is the same as the local node, or
!       if the file system is NFS-mounted to the local node and therefore
!       appears as if it is on the local node.
!  (3) The colon (:) in PATHNAME is not considered part of USERID.
!  (4) The colon (:) in PATHNAME must be omitted if NODE is omitted.
!
! DIR details:
!  (1) Must contain only letters and digits and underscores (_) and hyphens
!       (-) and periods (.) and slashes (/), and can optionally begin with
!       a tilde (~).
!  (2) The slash (/) in the PATHNAME format above should be considered part
!       of the directory name.
!  (3) If DIR is /, it refers to the root directory.
!  (4) If DIR begins with /, it is an absolute path from the root directory.
!  (5) If DIR begins with ~, it is a relative path from the home directory.
!  (6) If DIR is omitted, it refers to the home directory (on a remote
!       node) or the local directory (on the local node).
!  (7) If DIR does not begin with /, it is a relative path from the home
!       directory (on a remote node) or the local directory (on the local
!       node).
!
! FILE details:
!  (1) Must contain only letters and digits and underscores (_) and hyphens
!       (-) and periods (.).
!  (2) If FILE is omitted, PATHNAME must end in / or : (or be completely
!       blank) to indicate that PATHNAME does not include a proper file name.
!
!                            +++++++++++++++++++
!
! Operating systems might accept variations not listed above.  Experience
! will dictate whether this primitive should be modified to allow any such
! variations.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!           TO VALIDATE A PATHNAME ON A LOCAL OR NFS MOUNTED DISK
!
!                                             opt  opt   opt    opt
!                                      b       i    i     o      o 
!        CALL PATH_FIXUP_FILENAME  (PATHNAME, DIR, EXT, STATUS, MSG)
!        CALL PATH_FIXUP_DIRECTORY (DIRECTORY,          STATUS, MSG)
!
! character(len=*) PATHNAME  = file name to validate (including full path).
! character(len=*) DIRECTORY = directory to validate (including full path).
! character(len=*) DIR       = default directory path to use.
! character(len=*) EXT       = default extension to use.
! integer          STATUS    = status of the syntax of PATHNAME or DIRECTORY.
! character(len=*) MSG       = message regarding the status.
!
! The PATHNAME and DIRECTORY and DIR parameters:
!
!   If PATHNAME or DIRECTORY is unspecified, it is reset to the named
!   constant PATH_EMPTY.
!
!   If PATHNAME or DIRECTORY or DIR contains a user ID or node name, these
!   are removed from PATHNAME or DIRECTORY or DIR.
!
!   If PATHNAME or DIRECTORY or DIR starts with a tilde (~), the tilde is
!   expanded to an absolute path.
!
!   If PATHNAME begins with a relative path (here defined as not starting
!   with slash / or tilde ~), DIR is prepended to PATHNAME.
!
!   The PATHNAME or DIRECTORY is then checked for correct syntax, and possibly
!   modified to fix minor problems such as unprintable characters and embedded
!   blanks.
!
!   A slash (/) will be appended to DIRECTORY if necessary.
!
! The EXT parameter:
!
!   If EXT is specified and begins with a period, it is appended to PATHNAME
!   if PATHNAME has no extension, or it replaces any extension in PATHNAME.
!   This is the correct action if the extension is required (such as .byt
!   for byte files).
!
!   If EXT is specified and does not begin with a period, it (with a period)
!   is appended to PATHNAME if PATHNAME has no extension, but it does not
!   replace an existing extension in PATHNAME.  This is the correct action
!   if the extension is preferred but not required (such as .vel for
!   velocity files).
!
! The STATUS parameter:
!
!   STATUS = PATH_VALID       if PATHNAME is specified with valid syntax.
!   STATUS = PATH_UNSPECIFIED if PATHNAME is unspecified (no file name).
!   STATUS = PATH_INVALID     if PATHNAME is specified with invalid syntax.
!   STATUS = PATH_INCOMPLETE  if PATHNAME is specified with valid syntax,
!                               but does not include a filename (ends in a
!                               slash (/).
!
!   STATUS = PATH_VALID       if DIRECTORY is specified with valid syntax.
!   STATUS = PATH_UNSPECIFIED if DIRECTORY is unspecified (no directory name).
!   STATUS = PATH_INVALID     if DIRECTORY is specified with invalid syntax.
!
!   Invalid syntax may occur if the tilde (~) cannot be expanded (implying
!   an invalid user ID), or the output version of PATHNAME or DIRECTORY
!   contains invalid characters.
!
!                           +++++++++++++++++
!
! PATHNAME and DIRECTORY and DIR and EXT are considered to be unspecified
! if they begin with the character char(0), or are blank, or are set to a
! case-insensitive equivalent of the named constant PATH_EMPTY.
!
! The named constant PATH_EMPTY is set to 'NONE', which is the same as the
! named constant STRING_EMPTY.  Users should use this constant instead of
! the string 'NONE' in case this constant is changed in the future.
!
!-------------------------------------------------------------------------------
!              TO VALIDATE A PATHNAME FOR THE RCPFILE PRIMITIVE
!
! To validate the pathname or its components:
!
!                                          b        o      o
!            CALL PATH_VALIDATE        (PATHNAME, STATUS, MSG)
!            CALL PATH_VALIDATE_USERID (USERID  , STATUS, MSG)
!            CALL PATH_VALIDATE_NODE   (NODE    , STATUS, MSG)
!            CALL PATH_VALIDATE_DIR    (DIR     , STATUS, MSG)
!            CALL PATH_VALIDATE_FILE   (FILE    , STATUS, MSG)
!
! character(len=*) PATHNAME = fully constructed file name.
! character(len=*) USERID   = user ID on the remote node.
! character(len=*) NODE     = name of the remote node.
! character(len=*) DIR      = relative or absolute directory path.
! character(len=*) FILE     = proper file name (including extension).
! integer          STATUS   = status of PATHNAME.
! character(len=*) MSG      = message regarding status of PATHNAME.
!
! The first argument is checked for correct syntax.  There is no effort to
! check for the existence of an actual item with this name.
!
! STATUS = PATH_VALID       if first argument is specified with valid syntax.
! STATUS = PATH_UNSPECIFIED if first argument is unspecified.
! STATUS = PATH_INVALID     if first argument is specified with invalid syntax.
! STATUS = PATH_INCOMPLETE  if PATHNAME is specified with valid syntax,
!                             but does not include a filename.
!
! The first argument is considered to be unspecified if it begins with the
!   character char(0), or is blank, or is set to a case-insensitive equivalent
!   of the named constant PATH_EMPTY.
! If the first argument contains leading or embedded blanks, they are removed.
! If the first argument is unspecified, it is reset to the named constant
!   PATH_EMPTY.
! If DIR is specified but does not end in a slash, a slash is appended.
! Otherwise the first argument is not changed.
!
! Currently, this subroutine may not be as picky as it ultimately should be,
! and might sometimes return PATH_VALID when it should return PATH_INVALID.
!
!                            +++++++++++++++++
!
! The named constant PATH_EMPTY is set to 'NONE', which is the same as
! the named constant STRING_EMPTY.  Users should use this constant instead
! of the string 'NONE' in case this constant is changed in the future.
!
!-------------------------------------------------------------------------------
!             TO MANIPULATE A PATHNAME FOR THE RCPFILE PRIMITIVE
!
! To parse a full pathname into its components:
! To build a full pathname from its components:
!
!                            i          o      o     o    o
!        CALL PATH_PARSE (PATHNAME,   USERID, NODE, DIR, FILE)
!        CALL PATH_BUILD (PATHNAME,   USERID, NODE, DIR, FILE)
!                            o          i      i     i    i
!
!                o                          i
!              USERID = PATH_GET_USERID (PATHNAME)
!              NODE   = PATH_GET_NODE   (PATHNAME)
!              DIR    = PATH_GET_DIR    (PATHNAME)
!              FILE   = PATH_GET_FILE   (PATHNAME)
!
! character(len=*) PATHNAME = fully constructed file name.
! character(len=*) USERID   = user ID on the remote node.
! character(len=*) NODE     = name of the remote node.
! character(len=*) DIR      = relative or absolute directory path.
! character(len=*) FILE     = proper file name (including extension).
!
! Any input argument is considered to be unspecified if it begins with the
! character char(0), or is blank, or is set to a case-insensitive equivalent
! of the named constant PATH_EMPTY.  If all input arguments are unspecified,
! all output arguments will be set to the named constant PATH_EMPTY.
!
! PATH_PARSE:
!
!  (1) The PATHNAME will be parsed, and its components will be fully
!       distributed into all four output arguments.
!  (2) An output argument will be set to the named constant PATH_EMPTY
!       if the corresponding component is missing in PATHNAME.
!  (3) If PATHNAME contains a directory, DIR will always terminate in a slash.
!
! PATH_BUILD:
!
!  (1) NODE can optionally include USERID@NODE     (including @).
!  (2) DIR  can optionally include USERID@NODE:DIR (including @ and :).
!  (2) DIR  can optionally include NODE:DIR        (including :).
!  (3) DIR  can optionally terminate in a slash (/).
!  (4) FILE can optionally include DIR.
!  (5) FILE can optionally include USERID and NODE if it also includes DIR.
!  (6) If FILE is unspecified, PATHNAME will end in a slash, meaning that
!       it is a partial PATHNAME which ends in a directory.
!
! These two subroutines do not check the input or output arguments for
! validity.  To do so, PATH_VALIDATE should be called.
!
! If any input arguments are invalid, a PARSE followed by a BUILD, or a
! BUILD followed by a PARSE, might not return to the original values.
!
! Even if all input arguments are valid, a PARSE followed by a BUILD, or a
! BUILD followed by a PARSE, might not return to the original values,
! although in this case the original and final values should be equivalent
! and valid.  (For example, if the input DIR is empty and the input FILE
! is 'aaa/bbb', after building and parsing, the output DIR will be 'aaa/'
! and the output FILE will be 'bbb'.)
!
!                            +++++++++++++++++
!
! The named constant PATH_EMPTY is set to 'NONE', which is the same as
! the named constant STRING_EMPTY.  Users should use this constant instead
! of the string 'NONE' in case this constant is changed in the future.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
! 13. 2007-08-30  R.S.Day    Fix @ sign logic in path_parse to deal with Voxet
!                            model conventions
! 12. 2003-11-18  Stoeckley  Replace reverse INDEX intrinsic functions with
!                             STRING_INDEX to make the Portland Group compiler
!                             happy.
! 11. 2003-06-16  Menger     Modified the use string_module to explicitly 
!                             use only those items it needs (for intel ftn).
! 10. 2000-10-19  Stoeckley  Add PATH_FIXUP_FILENAME and PATH_FIXUP_DIRECTORY,
!                             which do not use USERID and NODE, and expand
!                             tilde to an absolute path, and incorporate some
!                             code previously in PATHCHECK.
!  9. 2000-02-15  Stoeckley  Relaxed USERID format to allow digits.
!  8. 2000-01-28  Stoeckley  Add reference to named constant FILENAME_LENGTH.
!  7. 2000-01-17  Stoeckley  Fixed bug regarding directory names beginning
!                             with slash (/); clean up some inaccurate logic;
!                             add subroutines to validate path components.
!  6. 2000-01-07  Stoeckley  Added named constant PATH_EMPTY; fixed bug
!                             involving tilde (~) in DIR; improved validity
!                             testing.
!  5. 1999-12-29  Stoeckley  Converted from old system with extensive changes.
!                             Name changed from FILENAME_UTIL to PATH.
!  4. 1999-01-25  Goodger    Begin using the fortran90 compiler.
!  3. 1996-12-16  Stoeckley  Correct misspelling of PATH_SPLIT_NAME.
!  2. 1996-12-13  Goodger    Inserted PROG DOC cards for ease in
!                             printing a CPS manual.  Did not recompile.
!  1. 1996-12-06  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! The Intel compiler required modifications in the code as follows:
!
!         Modified the USE STRING_MODULE statement to explicitly 
!         use only those items it needs.
!
! The Portland Group compiler required modifications in the code as follows:
!
!         Replaced reverse INDEX intrinsic function calls with STRING_INDEX.
!
!-------------------------------------------------------------------------------
!</portability_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module path_module
      use string_module, only: string_validate,string_is_alphanum, &
                               string_is_one,string_to_lower,&
                               string_upper_compare,&
                               string_is_print,&
                               string_is_alpha,&
                               string_index,&
                               STRING_EMPTY
      use named_constants_module
      use addext_module
      use exptilde_module
      implicit none
      public
      private :: path_private_check_syntax
      private :: path_private_cleanup_file
      private :: path_private_cleanup_ext

      character(len=100),public,save :: PATH_IDENT = &
       '$Id: path.f90,v 1.13 2007/08/31 13:55:04 R.S.Day beta sps $'

      integer         ,parameter,public :: PATH_INVALID     = -1
      integer         ,parameter,public :: PATH_UNSPECIFIED =  0
      integer         ,parameter,public :: PATH_VALID       =  1
      integer         ,parameter,public :: PATH_INCOMPLETE  =  2
      character(len=4),parameter,public :: PATH_EMPTY       = STRING_EMPTY

      contains


!!-------------------- path private check syntax ---------------------------!!
!!-------------------- path private check syntax ---------------------------!!
!!-------------------- path private check syntax ---------------------------!!

! NAME should be 'pathname' or 'directory'.


      subroutine path_private_check_syntax (name,pathname,status,msg)
      implicit none
      character(len=*),intent(in)           :: name,pathname     ! arguments
      integer         ,intent(out),optional :: status            ! arguments
      character(len=*),intent(out),optional :: msg               ! arguments
      integer                               :: length,status2,i  ! local
      character(len=80)                     :: msg2              ! local
      character(len=1)                      :: single            ! local

      length = len_trim(pathname)

      if (pathname == STRING_EMPTY) then
           status2 = PATH_UNSPECIFIED
           msg2    = trim(name)//' not specified'
      else if (pathname(1:1) == '~') then
           status2 = PATH_INVALID
           msg2    = 'invalid user ID in '//name
      else if (name(1:1) /= 'd' .and. pathname(length:length) == '/') then
           status2 = PATH_INCOMPLETE
           msg2    = 'pathname contains a directory path but no file name'
      else if (pathname(1:1) == '/') then
           status2 = PATH_VALID
           msg2    = trim(name)//' contains an absolute path'
      else
           status2 = PATH_VALID
           msg2    = trim(name)//' contains a relative path'
      end if

      do i = 1,length
           single = pathname(i:i)
           if (string_is_alphanum(single)  ) cycle
           if (string_is_one(single,'.-_/')) cycle
           if (i == 1 .and. single == '~'  ) cycle
           status2 = PATH_INVALID
           msg2    = trim(name)//' contains the invalid character '//single
           exit
      end do

      if (present(status)) status = status2
      if (present(msg   )) msg    = msg2
      return
      end subroutine path_private_check_syntax


!!-------------------- path private cleanup file ---------------------------!!
!!-------------------- path private cleanup file ---------------------------!!
!!-------------------- path private cleanup file ---------------------------!!

! This routine removes the following characters:
!    all characters preceding the last : or @ character.
!    duplicate consecutive / characters.
!    all ~ characters unless it becomes the first character.
!    all unprintable characters and the space character.
! If the result is unspecified (blank), it is set to 'NONE'.
! Otherwise, if the first character is ~, it is expanded to an absolute path.


      subroutine path_private_cleanup_file (pathname)
      implicit none
      character(len=*),intent(inout)     :: pathname             ! arguments
      integer                            :: i,j,length           ! local
      character(len=1)                   :: single               ! local

      length = len_trim(pathname)
      j = 0

      do i = 1,length
           single = pathname(i:i)
           select case (single)
                case (' ') ; cycle
                case ('~') ; if (j > 0) cycle
                case ('/') ; if (j > 0) then
                               if (pathname(j:j) == '/') cycle
                             end if
                case ('@') ; j = 0 ; cycle
                case (':') ; j = 0 ; cycle
           end select
           if (string_is_print(single)) then
                j = j + 1
                pathname(j:j) = single
           end if
      end do

      pathname(j+1:) = ' '

      if (pathname == ' ') then
           pathname = STRING_EMPTY
           return
      else if (string_upper_compare(pathname,STRING_EMPTY)) then
           pathname = STRING_EMPTY
           return
      end if

      if (pathname(1:1) == '~') call exptilde (pathname)
      return
      end subroutine path_private_cleanup_file


!!----------------------- path private cleanup ext ------------------------!!
!!----------------------- path private cleanup ext ------------------------!!
!!----------------------- path private cleanup ext ------------------------!!

! This routine removes all characters except .-_ and alphanumeric characters.
! This routine removes . unless it is the first character.
! If the result is unspecified (blank), it is set to 'NONE'.


      subroutine path_private_cleanup_ext (ext)
      implicit none
      character(len=*),intent(inout)     :: ext                  ! arguments
      integer                            :: i,j,length           ! local
      character(len=1)                   :: single               ! local

      length = len_trim(ext)
      j = 0
      do i = 1,length
           single = ext(i:i)
           if (i > 1 .and. single == '.') cycle
           if (string_is_alphanum(single) .or. string_is_one(single,'.-_')) then
                j = j + 1
                ext(j:j) = single
           end if
      end do
      ext(j+1:) = ' '
      if (ext == ' ') then
           ext = STRING_EMPTY
      else if (string_upper_compare(ext,STRING_EMPTY)) then
           ext = STRING_EMPTY
      end if
      return
      end subroutine path_private_cleanup_ext


!!----------------------- path fixup filename ------------------------------!!
!!----------------------- path fixup filename ------------------------------!!
!!----------------------- path fixup filename ------------------------------!!


      subroutine path_fixup_filename (pathname,dir,ext,status,msg)
      implicit none
      character(len=*),intent(inout)        :: pathname           ! arguments
      character(len=*),intent(in) ,optional :: dir,ext            ! arguments
      integer         ,intent(out),optional :: status             ! arguments
      character(len=*),intent(out),optional :: msg                ! arguments
      character(len=FILENAME_LENGTH)        :: dir2,ext2          ! local
      integer                               :: length             ! local

      call path_private_cleanup_file (pathname)

      if (pathname /= STRING_EMPTY) then

           if (present(ext)) then
                length = len_trim(pathname)
                if (pathname(length:length) /= '/') then
                     ext2 = ext
                     call path_private_cleanup_ext (ext2)
                     if (ext2 /= STRING_EMPTY) then
                          if (ext2(1:1) == '.') then
                               call addext (pathname, ext2(2:), .true.)
                          else
                               call addext (pathname, ext2, .false.)
                          end if
                     end if
                end if
           end if

           if (present(dir)) then
                if (pathname(1:1) /= '/' .and. pathname(1:1) /= '~') then
                     dir2 = dir
                     call path_private_cleanup_file (dir2)
                     if (dir2 /= STRING_EMPTY) then
                          length = len_trim(dir2)
                          if (dir2(length:length) == '/') then
                               pathname = trim(dir2)//pathname
                          else
                               pathname = trim(dir2)//'/'//pathname
                          end if
                     end if
                end if
           end if

      end if

      call path_private_check_syntax ('pathname',pathname,status,msg)
      return
      end subroutine path_fixup_filename


!!----------------------- path fixup directory ------------------------------!!
!!----------------------- path fixup directory ------------------------------!!
!!----------------------- path fixup directory ------------------------------!!


      subroutine path_fixup_directory (directory,status,msg)
      implicit none
      character(len=*),intent(inout)        :: directory          ! arguments
      integer         ,intent(out),optional :: status             ! arguments
      character(len=*),intent(out),optional :: msg                ! arguments
      integer                               :: length             ! local

      call path_private_cleanup_file (directory)

      if (directory /= STRING_EMPTY) then
           length = len_trim(directory)
           if (directory(length:length) .ne. '/') directory(length+1:) = '/'
      end if

      call path_private_check_syntax ('directory',directory,status,msg)
      return
      end subroutine path_fixup_directory


                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!------------------------ path validate ---------------------------------!!
!!------------------------ path validate ---------------------------------!!
!!------------------------ path validate ---------------------------------!!


      subroutine path_validate (pathname,   status,msg)
      implicit none
      character(len=*),intent(inout) :: pathname                    ! arguments
      integer         ,intent(out)   :: status                      ! arguments
      character(len=*),intent(out)   :: msg                         ! arguments
      integer                        :: indx_atsign,indx_atsign2    ! local
      integer                        :: indx_colon ,indx_colon2     ! local
      integer                        :: length                      ! local
      integer                        :: stat1,stat2,stat3,stat4     ! local
      character(len=FILENAME_LENGTH) :: userid,node,dir,file        ! local
      character(len=100)             :: msg1,msg2,msg3,msg4         ! local

!!!!!!!!!! get started:

      call string_validate (pathname)

      if (pathname == PATH_EMPTY) then
           status = PATH_UNSPECIFIED
           msg = 'path not specified'
           return
      end if

!!!!!!!!!! check for proper placement of atsign and colon:

      length          = len_trim(pathname)
      indx_atsign     = index        (pathname, '@')
!!!   indx_atsign2    = index        (pathname, '@', .true.)
      indx_atsign2    = string_index (pathname, '@', .true.)
      indx_colon      = index        (pathname, ':')
!!!   indx_colon2     = index        (pathname, ':', .true.)
      indx_colon2     = string_index (pathname, ':', .true.)

      if (indx_atsign2 /= indx_atsign) then
         status = PATH_INVALID
         msg = 'path cannot contain more than one at-sign (@)'
         return
      end if

      if (indx_colon2 /= indx_colon) then
         status = PATH_INVALID
         msg = 'path cannot contain more than one colon (:)'
         return
      end if

      if (indx_atsign == 1) then
         status = PATH_INVALID
         msg = 'path must have user ID before at-sign (@)'
         return
      end if

      if (indx_colon == 1) then
         status = PATH_INVALID
         msg = 'path must have node name before colon (:)'
         return
      end if

      if (indx_atsign == length) then
         status = PATH_INVALID
         msg = 'path must have node name after at-sign (@)'
         return
      end if

      if (indx_atsign > 0 .and. indx_colon == 0) then
         status = PATH_INVALID
         msg = 'path cannot have user ID without node name'
         return
      end if

      if (indx_atsign > 0 .and. indx_colon > 0) then
        if (indx_colon < indx_atsign) then
           status = PATH_INVALID
           msg = 'at-sign (@) must come before colon (:) in path'
           return
        end if

        if (indx_colon - indx_atsign <= 1) then
           status = PATH_INVALID
           msg = 'path cannot have user ID without node name'
           return
        end if
      end if

!!!!!!!!!! check for invalid characters in each component:

      call path_parse (pathname,   userid,node,dir,file)

      call path_validate_userid (userid,  stat1,msg1)
      call path_validate_node   (node  ,  stat2,msg2)
      call path_validate_dir    (dir   ,  stat3,msg3)
      call path_validate_file   (file  ,  stat4,msg4)

      call path_build (pathname,   userid,node,dir,file)

      if (stat1 == PATH_INVALID) then
             status = stat1
             msg    = msg1
             return
      else if (stat2 == PATH_INVALID) then
             status = stat2
             msg    = msg2
             return
      else if (stat3 == PATH_INVALID) then
             status = stat3
             msg    = msg3
             return
      else if (stat4 == PATH_INVALID) then
             status = stat4
             msg    = msg4
             return
      end if

!!!!!!!!!! finish up:

      if (file == PATH_EMPTY) then
           status = PATH_INCOMPLETE
           msg = 'path does not include a filename'
           return
      end if

      status = PATH_VALID
      msg = 'path appears to be valid'
      return
      end subroutine path_validate


!!------------------- validate a single component -----------------------!!
!!------------------- validate a single component -----------------------!!
!!------------------- validate a single component -----------------------!!


      subroutine path_validate_userid (userid,   status,msg)
      implicit none
      character(len=*),intent(inout) :: userid                    ! arguments
      integer         ,intent(out)   :: status                    ! arguments
      character(len=*),intent(out)   :: msg                       ! arguments
      character(len=1)               :: single                    ! local
      integer                        :: length,indx               ! local

      call string_to_lower (userid)
      call string_validate (userid)

      if (userid == PATH_EMPTY) then
           status = PATH_UNSPECIFIED
           msg = 'user ID not specified'
           return
      end if

      if (.not.string_is_alpha(userid(1:1))) then       ! new 2/14/00
           msg = 'user ID must begin with a letter.'    ! new 2/14/00
           status = PATH_INVALID                        ! new 2/14/00
           return                                       ! new 2/14/00
      end if                                            ! new 2/14/00

      length = len_trim(userid)
      do indx = 1,length
           single = userid(indx:indx)
           if (.not.string_is_alphanum(single)) then
                msg = 'user ID contains  '//single//  &
                              '  but must contain only letters and digits.'
! the following 3 lines replaced by the above 3 lines 2/14/00.
!          if (.not.string_is_alpha(single)) then
!               msg = 'user ID contains  '//single//  &
!                               '  but must contain only letters.'
                status = PATH_INVALID
                return
           end if
      end do

      status = PATH_VALID
      msg = 'user ID appears to be valid'
      return
      end subroutine path_validate_userid


                                !!!!!!!!!!!!!


      subroutine path_validate_node (node,   status,msg)
      implicit none
      character(len=*),intent(inout) :: node                      ! arguments
      integer         ,intent(out)   :: status                    ! arguments
      character(len=*),intent(out)   :: msg                       ! arguments
      character(len=1)               :: single                    ! local
      integer                        :: length,indx               ! local

      call string_to_lower (node)
      call string_validate (node)

      if (node == PATH_EMPTY) then
           status = PATH_UNSPECIFIED
           msg = 'node not specified'
           return
      end if

      if (.not.string_is_alpha(node(1:1))) then
           msg = 'node name must begin with a letter.'
           status = PATH_INVALID
           return
      end if

      length = len_trim(node)
      do indx = 1,length
           single = node(indx:indx)
           if (.not.string_is_alphanum(single)) then
                msg = 'node name contains  '//single//  &
                              '  but must contain only letters and digits.'
                status = PATH_INVALID
                return
           end if
      end do

      status = PATH_VALID
      msg = 'node appears to be valid'
      return
      end subroutine path_validate_node


                                !!!!!!!!!!!!!


      subroutine path_validate_dir (dir,   status,msg)
      implicit none
      character(len=*),intent(inout) :: dir                       ! arguments
      integer         ,intent(out)   :: status                    ! arguments
      character(len=*),intent(out)   :: msg                       ! arguments
      character(len=1)               :: single                    ! local
      integer                        :: length,indx               ! local

      call string_validate (dir)

      if (dir == PATH_EMPTY) then
           status = PATH_UNSPECIFIED
           msg = 'directory not specified'
           return
      end if

      length = len_trim(dir)
      do indx = 1,length
           single = dir(indx:indx)
           if (single == '~') then
                if (indx > 1) then
                     msg = 'directory cannot contain tilde (~) except&
                                           & as the first character.'
                     status = PATH_INVALID
                     return
                else
                     cycle
                end if
           end if
           if (string_is_alphanum (single       )) cycle
           if (string_is_one      (single,'.-_/')) cycle
           msg = 'directory contains the invalid character '//single
           status = PATH_INVALID
           return
      end do
      if (dir(length:length) /= '/') dir(length+1:length+1) = '/'

      status = PATH_VALID
      msg = 'directory appears to be valid'
      return
      end subroutine path_validate_dir


                                !!!!!!!!!!!!!


      subroutine path_validate_file (file,   status,msg)
      implicit none
      character(len=*),intent(inout) :: file                      ! arguments
      integer         ,intent(out)   :: status                    ! arguments
      character(len=*),intent(out)   :: msg                       ! arguments
      character(len=1)               :: single                    ! local
      integer                        :: length,indx               ! local

      call string_validate (file)

      if (file == PATH_EMPTY) then
           status = PATH_UNSPECIFIED
           msg = 'file not specified'
           return
      end if

      length = len_trim(file)
      do indx = 1,length
           single = file(indx:indx)
           if (string_is_alphanum (single      )) cycle
           if (string_is_one      (single,'.-_')) cycle
           msg = 'proper file name contains the invalid character '//single
           status = PATH_INVALID
           return
      end do

      status = PATH_VALID
      msg = 'file appears to be valid'
      return
      end subroutine path_validate_file


!!---------------------------- path parse --------------------------------!!
!!---------------------------- path parse --------------------------------!!
!!---------------------------- path parse --------------------------------!!


      subroutine path_parse (pathname,  userid,node,dir,file)
      implicit none
      character(len=*),intent(in)    :: pathname                  ! argument
      character(len=*),intent(out)   :: userid,node,dir,file      ! argument
      integer                        :: indx_atsign               ! local
      integer                        :: indx_colon                ! local
      integer                        :: indx_slash                ! local
      character(len=FILENAME_LENGTH) :: pathname9                 ! local

      userid = PATH_EMPTY
      node   = PATH_EMPTY
      dir    = PATH_EMPTY
      file   = PATH_EMPTY

! Voxet models may contain @ characters in the file name (RSDay) 
! if @ exists but no colon, then assume @ is part of file name
! Obsolete logic checks for userid@node:dir/file syntax
      pathname9 = pathname
      call string_validate (pathname9)

      if (pathname9 == PATH_EMPTY) return

      indx_atsign = index        (pathname, '@')
      indx_colon  = index        (pathname, ':')
!!!   indx_slash  = index        (pathname, '/', .true.)
      indx_slash  = string_index (pathname, '/', .true.)

!!!   string_index replaces index above because index returns 1 instead
!!!   of 28 for strings such as the following:
!!!        /home/stoectr/engine/linuxp/simple.wrk

!!!   print *, 'indx_slash = ',indx_slash,'  ',trim(pathname)

      if (indx_colon  > 0) then
        if (indx_atsign > 0 .and. indx_colon < indx_atsign)  &
                          indx_colon = indx_atsign

        if (indx_atsign > 0) userid = pathname(1:indx_atsign-1)
        if (indx_colon  > 0) node   = pathname(indx_atsign+1:indx_colon-1)
        if (indx_slash  > 0) dir    = pathname(indx_colon+1:indx_slash)
        if (indx_slash  > 0) file   = pathname(indx_slash+1:)
        if (indx_slash == 0) file   = pathname(indx_colon+1:)
      else
        if (indx_slash  > 0) dir    = pathname(1:indx_slash)
        if (indx_slash  > 0) file   = pathname(indx_slash+1:)
        if (indx_slash == 0) file   = pathname
      endif

      call string_validate (userid)   ! purposely not path_validate_userid.
      call string_validate (node  )   ! purposely not path_validate_node.
      call string_validate (dir   )   ! purposely not path_validate_dir.
      call string_validate (file  )   ! purposely not path_validate_file.
      return
      end subroutine path_parse


!!------------------------------ path build -------------------------------!!
!!------------------------------ path build -------------------------------!!
!!------------------------------ path build -------------------------------!!


      subroutine path_build (pathname,  userid,node,dir,file)
      implicit none
      character(len=*),intent(out)   :: pathname                   ! argument
      character(len=*),intent(in)    :: userid,node,dir,file       ! argument
      character(len=FILENAME_LENGTH) :: userid9,node9,dir9,file9   ! local
      character(len=100)             :: msg                        ! local
      integer                        :: length,status              ! local

      userid9 = userid
      node9   = node  
      dir9    = dir   
      file9   = file  

      call path_validate_userid (userid9, status, msg)
      call path_validate_node   (node9  , status, msg)
      call path_validate_dir    (dir9   , status, msg)
      call path_validate_file   (file9  , status, msg)

      if (userid9 /= PATH_EMPTY .and. node9 == PATH_EMPTY) then
           node9 = ''
      end if

      if (dir9 /= PATH_EMPTY) then
           length = len_trim(dir9)
           if (dir9(length:length) /= '/' .and.  &
               dir9(length:length) /= ':' .and.  &
               dir9(length:length) /= '@') dir9(length+1:length+1) = '/'
      end if

      pathname = ' '
      if (userid9 /= PATH_EMPTY) pathname = trim(pathname)//trim(userid9)//'@'
      if (node9   /= PATH_EMPTY) pathname = trim(pathname)//trim(node9  )//':'
      if (dir9    /= PATH_EMPTY) pathname = trim(pathname)//trim(dir9   )
      if (file9   /= PATH_EMPTY) pathname = trim(pathname)//trim(file9  )
      return
      end subroutine path_build


!!------------------------ get a single component ------------------------!!
!!------------------------ get a single component ------------------------!!
!!------------------------ get a single component ------------------------!!


      function path_get_userid (pathname) result (userid)
      implicit none
      character(len=*),intent(in)    :: pathname             ! argument
      character(len=FILENAME_LENGTH) :: userid,node,dir,file ! result and local

      call path_parse (pathname,  userid,node,dir,file)
      return
      end function path_get_userid



      function path_get_node (pathname) result (node)
      implicit none
      character(len=*),intent(in)    :: pathname             ! argument
      character(len=FILENAME_LENGTH) :: userid,node,dir,file ! result and local

      call path_parse (pathname,  userid,node,dir,file)
      return
      end function path_get_node



      function path_get_dir (pathname) result (dir)
      implicit none
      character(len=*),intent(in)    :: pathname             ! argument
      character(len=FILENAME_LENGTH) :: userid,node,dir,file ! result and local

      call path_parse (pathname,  userid,node,dir,file)
      return
      end function path_get_dir



      function path_get_file (pathname) result (file)
      implicit none
      character(len=*),intent(in)    :: pathname             ! argument
      character(len=FILENAME_LENGTH) :: userid,node,dir,file ! result and local

      call path_parse (pathname,  userid,node,dir,file)
      return
      end function path_get_file


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module path_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

