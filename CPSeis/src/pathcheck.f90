
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- pathcheck.f90 --------------------------------!!
!!---------------------------- pathcheck.f90 --------------------------------!!
!!---------------------------- pathcheck.f90 --------------------------------!!


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
! Name       : PATHCHECK
! Category   : io
! Written    : 1999-12-29   by: Tom Stoeckley
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : Facilitate checking and updating a pathname.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This is a convenience routine to facilitate checking and updating a pathname
! using the parameter cache.  This primitive accesses the PATH primitive and
! the PC primitive, neither of which know about each other.
!
! This primitive is used to check and adjust file name syntax.  It will also
! optionally use the FINQUIRE primitive to check for the status of the actual
! file with the specified name.
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
!                           CALLING SEQUENCE
!
!                                      opt    opt      opt     opt    opt
!                      i        b       i      i        i       o      i
!   CALL PATHCHECK (KEYWORD, PATHNAME, EXT, REQUIRED, SCREEN, STATUS, SHOW)
!
!   CALL PATHCHECK_FETCH_MESSAGE       (MSG)
!   CALL PATHCHECK_FETCH_BRIEF_MESSAGE (BRIEF)
!                                        o
!
! character(len=*) KEYWORD  = keyword of the pathname in the process module.
! character(len=*) PATHNAME = file name to validate (including full path).
! character(len=*) EXT      = default extension to use.
! logical          REQUIRED = true if PATHNAME cannot be left unspecified.
! character(len=*) SCREEN   = keyword of screen on which PATHNAME resides.
! integer          STATUS   = status of PATHNAME.
! integer          SHOW     = type of information to show.
! character(len=*) MSG      = message regarding previous call to pathcheck.
! character(len=*) BRIEF    = brief msg regarding previous call to pathcheck.
!
! The PATHNAME parameter:
!
!   If PATHNAME is unspecified, it is reset to the named constant
!   PATHCHECK_EMPTY.
!
!   If PATHNAME contains a user ID or node name, these are removed from
!   PATHNAME.
!
!   If PATHNAME starts with a tilde (~), the tilde is expanded to an
!   absolute path.
!
!   If PATHNAME begins with a relative path (here defined as not starting
!   with slash / or tilde ~), the Job Data parameter PATHNAME_DIR is
!   prepended to PATHNAME.
!
!   PATHNAME is then checked for correct syntax, and possibly modified to
!   fix minor problems such as unprintable characters and embedded blanks.
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
!   STATUS = PATHCHECK_VALID       if PATHNAME is specified with valid syntax.
!   STATUS = PATHCHECK_UNSPECIFIED if PATHNAME is unspecified (no file name).
!   STATUS = PATHCHECK_INVALID     if PATHNAME is specified with invalid syntax.
!   STATUS = PATHCHECK_INCOMPLETE  if PATHNAME is specified with valid syntax,
!                                   but does not include a filename (ends in a
!                                   slash (/)).
!
!   Invalid syntax may occur if the tilde (~) cannot be expanded (implying
!   an invalid user ID), or the output version of PATHNAME contains invalid
!   characters.
!
! The SHOW parameter:
!
!   SHOW = PATHCHECK_INFO_INPUT   to show status as an input file.
!   SHOW = PATHCHECK_INFO_OUTPUT  to show status as an output file.
!   SHOW = PATHCHECK_INFO_GENERAL to show status as an input or output file.
!   SHOW = PATHCHECK_INFO_NONE    not to show status of the file (default).
!
!   The status of the file is shown in the GUI using a keyword which is the
!   same as KEYWORD with "_info" appended.
!
! Errors:
!
!   If PATHNAME is invalid or incomplete, or if PATHNAME is unspecified and
!   REQUIRED is present and true, an error message (identified by KEYWORD)
!   is reported to the parameter cache.  Any other action based on the optional
!   returned STATUS can be taken by the process module itself.
!
!   Errors are always reported to the parameter cache on initial and final
!   front-end updates and on back-end updates.  On GUI updates, errors will
!   be reported only if the user has just modified the PATHNAME, or if SCREEN
!   is present and the user is leaving the screen.
!
!   If SCREEN is present and the user is leaving the screen in the GUI, errors
!   will cause the user to remain on the current screen.
!
!                           +++++++++++++++++
!
! PATHNAME and EXT are considered to be unspecified if they begin with the
! character char(0), or are blank, or are set to a case-insensitive equivalent
! of the named constant PATHCHECK_EMPTY.
!
! The named constant PATHCHECK_EMPTY is set to 'NONE', which is the same as
! the named constants PATH_EMPTY and STRING_EMPTY.  Users should use this
! constant instead of the string 'NONE' in case this constant is changed in
! the future.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                     HOW TO USE THIS PRIMITIVE
!
! Process modules should take the following steps to use this primitive
! effectively, for a fully-constructed filename stored in the obj%pathname
! variable.  This example also shows how to use a file selection dialog box
! using the PATHCHOOSE primitive.
!
!-------------------------------------------------------------------------------
! (1) In the gui_def section of the process module:
!
!     Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                    [PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!     <PARMS PATHNAME[/ML=128/XST]>
!
!-------------------------------------------------------------------------------
! (2) In the HelpSection of the process module:
!
!        <Help KEYWORD="PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!        <Tip> Status of PATHNAME. </Tip>
!        </Help>
!
!        <Help KEYWORD="SELECT_PATHNAME">
!        <Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!        </Help>
!
!        <Help KEYWORD="PATHNAME">
!        <Tip> Pathname for whatever you want to say here. </Tip>
!         Default = NONE
!         Allowed = char
!
!         Additional information can go here.
!        </Help>
!
!-------------------------------------------------------------------------------
! (3) In the process module USE statement area:
!
!         use pathcheck_module
!         use pathchoose_module
!
!-------------------------------------------------------------------------------
! (4) In the process module data structure (length is in named constants):
!
!         character(len=FILENAME_LENGTH)  :: pathname
!         type(pathchoose_struct),pointer :: pathchoose
!
!-------------------------------------------------------------------------------
! (5) In the process module create routine:
!
!         call pathchoose_create (obj%pathchoose, 'pathname', ext)
!
!-------------------------------------------------------------------------------
! (6) In the process module initialize routine:
!
!         obj%pathname = PATHCHECK_EMPTY
!
!-------------------------------------------------------------------------------
! (7) In the process module update routine:
!
!     Before any other parameter cache calls:
!
!         if (pathchoose_update(obj%pathchoose, obj%pathname)) return
!
!     In the normal locations (note the new SHOW argument to pathcheck):
!
!         call pc_get    (keyword,obj%pathname)
!         call pathcheck (keyword,obj%pathname,ext,required,screen,status,show)
!         call pc_put    (keyword,obj%pathname)
!
!    If you need to set GUI sensitivity:
!
!         call pc_put_sensitive_field_flag ('pathname'       , sense)
!         call pc_put_sensitive_field_flag ('select_pathname', sense)
!         call pc_put_sensitive_field_flag ('pathname_info'  , sense)
!
!-------------------------------------------------------------------------------
! (8) In the process module delete routine:
!
!         call pathchoose_delete (obj%pathchoose)
!
!-------------------------------------------------------------------------------
!
! If the PATHCHECK subroutine is called from inside traps:
!   (a) It should be called from a scalar trap with the SCREEN argument absent.
!   (b) If there are multiple screens, it should be called from a screen trap
!         with the SCREEN argument present.
!
! If the PATHCHECK subroutine is called from outside any trap:
!   (a) If there are multiple screens, the SCREEN argument should be present.
!
! Error messages are reported at the following times:
!   (a) For GUI updates, error messages will not be reported unless the user
!         changed the pathname or the user is leaving the screen.
!   (b) For initial and final frontend updates (i.e. when the dialog box is
!         popped up or down, or a workfile is being updated), error messages
!         are always reported.
!   (b) For backend updates, error messages are always reported.
!
!                           +++++++++++++++++
!
! It would be a simple matter to include the calls to PC_GET and PC_PUT in
! the PATHCHECK subroutine, but it has been decided not to do so because this
! would obscure the fact that the pathname is being obtained and reported
! like all other process parameters.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!010. 2006-04-25  B. Menger   Removed Unused Variables.
!  9. 2003-07-10  Stoeckley  Add PATHCHECK_FETCH_MESSAGE and
!                             PATHCHECK_FETCH_BRIEF_MESSAGE.
!  8. 2002-06-03  Stoeckley  Add test for presence of SCREEN optional argument.
!  7. 2001-10-16  Stoeckley  Add option for displaying file status, and add
!                             documentation showing how to use this primitive
!                             with the PATHCHOOSE primitive.
!  6. 2000-10-19  Stoeckley  Change so as not to use USERID and NODE, and
!                             to expand tilde to an absolute path.
!  5. 2000-06-27  Stoeckley  Add reference to named constant FILENAME_LENGTH.
!  4. 2000-01-24  Stoeckley  Improved documentation regarding error messages,
!                             and changed to call new parameter cache routines
!                             PC_VERIFY_SCALAR and PC_VERIFY_SCREEN.
!  3. 2000-01-17  Stoeckley  Changed to prepend PATHNAME_USERID,
!                             PATHNAME_NODE, and PATHNAME_DIR to file names
!                             which were input with a relative path;
!                             made several logic changes to conform with
!                             updated standards; removed the constant
!                             PATHCHECK_DEFAULT; and modified the subroutine
!                             arguments with several new optional arguments.
!  2. 2000-01-06  Stoeckley  Changed job data keywords to PATHNAME_USERID,
!                             PATHNAME_NODE, and PATHNAME_DIR; added
!                             named constants PATHCHECK_EMPTY and
!                             PATHCHECK_DEFAULT; improved accuracy of error
!                             messages; fixed bug when optional argument
!                             DEFVAL is missing.
!  1. 1999-12-29  Stoeckley  Initial version.
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module pathcheck_module
      use path_module
      use pc_module
      use string_module
      use named_constants_module
      use finquire_module
      implicit none
      public

      character(len=100),public,save :: PATHCHECK_IDENT = &
       '$Id: pathcheck.f90,v 1.10 2006/04/25 13:24:00 Menger prod sps $'

      integer     ,parameter,public :: PATHCHECK_INVALID     = PATH_INVALID
      integer     ,parameter,public :: PATHCHECK_UNSPECIFIED = PATH_UNSPECIFIED
      integer     ,parameter,public :: PATHCHECK_VALID       = PATH_VALID
      integer     ,parameter,public :: PATHCHECK_INCOMPLETE  = PATH_INCOMPLETE

      character(4),parameter,public :: PATHCHECK_EMPTY       = PATH_EMPTY

      integer     ,parameter,public :: PATHCHECK_INFO_INPUT   = 1
      integer     ,parameter,public :: PATHCHECK_INFO_OUTPUT  = 2
      integer     ,parameter,public :: PATHCHECK_INFO_GENERAL = 3
      integer     ,parameter,public :: PATHCHECK_INFO_NONE    = 4

      character(80),private,save :: INFO  = ' '
      character(80),private,save :: BRIEF = ' '

      contains


!!------------------------- fetch messages ---------------------------------!!
!!------------------------- fetch messages ---------------------------------!!
!!------------------------- fetch messages ---------------------------------!!


      function pathcheck_fetch_message() result (message)
      implicit none
      character(len=80)            :: message       ! result

      message = INFO
      return
      end function pathcheck_fetch_message



      function pathcheck_fetch_brief_message() result (message)
      implicit none
      character(len=80)            :: message       ! result

      message = BRIEF
      return
      end function pathcheck_fetch_brief_message


!!--------------------------- pathcheck ----------------------------------!!
!!--------------------------- pathcheck ----------------------------------!!
!!--------------------------- pathcheck ----------------------------------!!


      subroutine pathcheck &
                    (keyword,pathname,ext,required,screen,status,show)
      implicit none
      character(len=*),intent(in)           :: keyword            ! arguments
      character(len=*),intent(inout)        :: pathname           ! arguments
      character(len=*),intent(in) ,optional :: ext                ! arguments
      logical         ,intent(in) ,optional :: required           ! arguments
      character(len=*),intent(in) ,optional :: screen             ! arguments
      integer         ,intent(out),optional :: status             ! arguments
      integer         ,intent(in) ,optional :: show               ! arguments
      character(len=FILENAME_LENGTH)        :: pathname_dir       ! local

      character(len=80)                     :: msg                ! local
      character(len=20)                     :: word               ! local
      integer                               :: status2,stat,show2 ! local
      logical                               :: required2          ! local
      logical                               :: report_errors      ! local

!!!!!!!!!! get started:

      if (present(required)) then
           required2 = required
      else
           required2 = .false.
      end if

      if (present(show)) then
           show2 = show
      else
           show2 = PATHCHECK_INFO_NONE
      end if

!!!!!!!!!! update the pathname:

      pathname_dir = ' '
      call pc_get_jdata        ('PATHNAME_DIR', pathname_dir)
      call path_fixup_filename (pathname, pathname_dir, ext, status2, msg)
      if (present(status)) status = status2

!!!!!!!!!! modify the message returned by PATH:

      if (msg(1:15) == 'invalid user ID') then
            msg = ' (invalid user ID)'
      else if (msg(1:21) == 'pathname contains the') then
            msg = ' ('//trim(msg(23:))//')'
      else
            msg = ' ('//trim(msg)//')'
      end if

!!!!!!!!!! get and show the status message:

      select case (show2)

         case (PATHCHECK_INFO_INPUT)

            select case (status2)
               case (PATH_INVALID)
                  INFO  = 'invalid input file name'//msg
                  BRIEF = 'invalid'
                  word  = 'INVALID'
               case (PATH_INCOMPLETE)
                  INFO  = 'incomplete input file name (directory path only)'
                  BRIEF = 'incomplete'
                  word  = 'INCOMPLETE'
               case (PATH_UNSPECIFIED)
                  if (required2) then
                       INFO  = 'input file name must be specified'
                       BRIEF = 'unspecified'
                       word  = 'UNSPECIFIED'
                  else
                       INFO  = ' '
                       BRIEF = ' '
                       word  = ' '
                  end if
               case default
                  stat  = finquire_input (pathname,INFO)
                  BRIEF = finquire_fetch_brief_message()
                  word  = ' '
            end select
            call pc_put_gui_only (trim(keyword)//'_info', INFO)

         case (PATHCHECK_INFO_OUTPUT)

            select case (status2)
               case (PATH_INVALID)
                  INFO  = 'invalid output file name'//msg
                  BRIEF = 'invalid'
                  word  = 'INVALID'
               case (PATH_INCOMPLETE)
                  INFO  = 'incomplete output file name (directory path only)'
                  BRIEF = 'incomplete'
                  word  = 'INCOMPLETE'
               case (PATH_UNSPECIFIED)
                  if (required2) then
                       INFO  = 'output file name must be specified'
                       BRIEF = 'unspecified'
                       word  = 'UNSPECIFIED'
                  else
                       INFO  = ' '
                       BRIEF = ' '
                       word  = ' '
                  end if
               case default
                  stat  = finquire_output (pathname,INFO)
                  BRIEF = finquire_fetch_brief_message()
                  word  = ' '
            end select
            call pc_put_gui_only (trim(keyword)//'_info', INFO)

         case (PATHCHECK_INFO_GENERAL)

            select case (status2)
               case (PATH_INVALID)
                  INFO  = 'invalid file name'//msg
                  BRIEF = 'invalid'
                  word  = 'INVALID'
               case (PATH_INCOMPLETE)
                  INFO  = 'incomplete file name (directory path only)'
                  BRIEF = 'incomplete'
                  word  = 'INCOMPLETE'
               case (PATH_UNSPECIFIED)
                  if (required2) then
                       INFO  = 'file name must be specified'
                       BRIEF = 'unspecified'
                       word  = 'UNSPECIFIED'
                  else
                       INFO  = ' '
                       BRIEF = ' '
                       word  = ' '
                  end if
               case default
                  stat  = finquire_file (pathname,INFO)
                  BRIEF = finquire_fetch_brief_message()
                  word  = ' '
            end select
            call pc_put_gui_only (trim(keyword)//'_info', INFO)

         case default

            select case (status2)
               case (PATH_INVALID)
                  INFO  = 'invalid file name'//msg
                  BRIEF = 'invalid'
                  word  = 'INVALID'
               case (PATH_INCOMPLETE)
                  INFO  = 'incomplete file name (directory path only)'
                  BRIEF = 'incomplete'
                  word  = 'INCOMPLETE'
               case (PATH_UNSPECIFIED)
                  if (required2) then
                       INFO  = 'file name must be specified'
                       BRIEF = 'unspecified'
                       word  = 'UNSPECIFIED'
                  else
                       INFO  = ' '
                       BRIEF = ' '
                       word  = ' '
                  end if
               case default
                  INFO  = ' '
                  BRIEF = ' '
                  word  = ' '
            end select

      end select

!!!!!!!!!! generate error message:

!!!!!!! errors are reported if the pathname exists in the parameter cache,
!!!!!!! or if this is a frontend or backend update, or if leaving the screen.

      if (word /= ' ') then
           report_errors = pc_verify_scalar (keyword)

           if (present(screen)) then
                if (screen /= ' ') then
                     if (pc_verify_screen(screen)) then
                           call pc_jump_screen (screen)
                           report_errors = .true.
                     end if
                end if
           end if

           if (report_errors) then
                call pc_error (word,keyword,'=',pathname)
                call pc_error (INFO)
           end if
      end if
      return
      end subroutine pathcheck


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module pathcheck_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

