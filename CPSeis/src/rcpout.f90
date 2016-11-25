!<CPS_v1 type="PROCESS"/>

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
!                         C P S   P R O C E S S             
!
! Name       : rcpout
! Category   : miscellaneous
! Written    : 2002-09-10   by: Donna K. Vunderink
! Revised    : 2006-12-04   by: D. Glover
! Maturity   : production
! Purpose    : Transfer disk files to a remote node via rcp.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! RCPOUT uses rcp to transfer disk files to a remote node.  The disk files may
! be specified in one of two ways.
!
!     1. Enter the individual pathnames of the files to transfer in the
!        PATHNAMES_FROM array parameter.
!
!     2. Specify, in PATH_LIST_FROM, the pathname of a listfile containing
!        pathnames of files to transfer.  This file must be an ascii file with
!        one pathname on each line.  (Listfiles can be written and edited with
!        the Build Listfile utility in CFE.)
!
!
!   **** NOTE **** NOTE **** NOTE **** NOTE **** NOTE **** NOTE **** NOTE ****
!   *                                                                        *
!   *  Before submitting any CPS batch job that contains the RCPOUT process, *
!   *  the user MUST setup a .rhost file in the REMOTE_USER account on the   *
!   *  REMOTE_NODE.  The .rhost MUST allow remote command execution by rsh   *
!   *  from any node assigned to the R batch queue.                          *
!   *                                                                        *
!   **** NOTE **** NOTE **** NOTE **** NOTE **** NOTE **** NOTE **** NOTE ****
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
! PATHNAME COMPLETION
! -------------------
!
! Any "from" filename that is not a complete pathname is pre-affixed with the
! JOB_DATA parameter PATHNAME_TRCIO.  Likewise, any "to" that is not a complete
! pathname is pre-affixed with the RCPOUT parameter PATH_DIR_TO.
!
!
! FILE TRANSFER
! -------------
!
! RCPOUT will use the prcp program to transfer a disk file from the local node
! to a remote node.  The format of the pathname on the remote node is:
!
!                       REMOTE_USER@REMOTE_NODE:filename
!
! where
!       REMOTE_USER = userid on the remote node
!       REMOTE_NODE = name of the remote machine
!       filename    = full pathname for the file on the remote node
!
! The RCPOUT process submits a separate batch job to perform the file transfer.
! This batch job runs in the R batch queue.  Before submitting any CPS batch
! job that contains the RCPOUT process, the user must setup a .rhost file in
! the REMOTE_USER account on the REMOTE_NODE.  The .rhost must allow remote
! command execution by rsh from any node assigned to the R batch queue.
!
! The separate batch job for the file transfer will be placed in the same
! directory as the originating job and called 
!
!                            rcpoutREQID_IPN.job
!
! where
!      REQID = the PBS request id of the originating job
!      IPN   = the IPN number of the RCPOUT process in the originating job
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a setup process only.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! Process is a setup process only.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name             Description                             Action taken
! ----             -----------                             ------------
! PATHNAME_TRCIO   JOB_DATA path for trace data            Used
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! NONE
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
! 3.  2006-12-04  D. Glover  Added NULLIFY statements for Intel compiler.
! 2.  2006-06-06  Stoeckley  Add call to pc_register_array_names for SeisSpace.
! 1.  2002-09-23  Vunderink  Initial version.
!
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


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS        
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS       
!
! Process is a setup process only.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS     
!
!  None provided.
!
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS    
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!<gui_def>
!<NS RCPOUT Process/NC=92>
!                                 RCP disk files after job executes.
!
!  OPT_FILE =`CCCCCCCCC                     WARNING: You must have a .rhost file setup on the remote node
!
!  `- Input Files -------------------------------+    `- Output Files ----------------------------+
!                                                      REMOTE_USER =`SSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                                                      REMOTE_NODE =`SSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                                                      PATH_DIR_TO =`SSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   Select PATHNAME[SELECT_PATHNAMES_FROM]`P
!   STATUS PATHNAMES_FROM                               PATHNAMES_TO
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   `SSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS         `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!   Select PATH_LIST_FROM[SELECT_PATH_LIST_FROM]`P                            Select PATH_LIST_TO[SELECT_PATH_LIST_TO]`P
!   [PATH_LIST_FROM]`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS        [PATH_LIST_TO]`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   [PATH_LIST_FROM_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        [PATH_LIST_TO_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!  `---------------------------------------------+    `-------------------------------------------+
!
!<PARMS REMOTE_USER[/XST]>
!<PARMS REMOTE_NODE[/XST]>
!<PARMS PATH_DIR_TO[/ML=140/XST]>
!<PARMS SELECT_PATHNAMES_FROM[/XST]>
!<PARMS STATUS_ARRAYSET[/YST]>
!<PARMS STATUS[/EN]>
!<PARMS PATHNAMES_FROM[/ML=140]>
!<PARMS PATHNAMES_TO[/ML=140/YST]>
!<PARMS SELECT_PATH_LIST_TO[/XST]>
!<PARMS PATH_LIST_TO[/ML=140/XST]>
!<PARMS PATH_LIST_TO_INFO[/XST]>
!<PARMS SELECT_PATH_LIST_FROM[/XST]>
!<PARMS PATH_LIST_FROM[/ML=140/XST]>
!<PARMS PATH_LIST_FROM_INFO[/XST]>
!</gui_def>


!<HelpSection>
!
!<Help KEYWORD="OPT_FILE">
!<Tip> Source of input files. </Tip>
! Default = PATHNAMES
! Allowed = PATHNAMES, PATH_LIST
! If OPT_FILE = PATHNAMES, file(s) listed in PATHNAMES array.
! If OPT_FILE = PATH_LIST, file(s) listed in file indicated by PATH_LIST.
! When using PATHNAMES, if no file suffix is specified an appropriate file
! suffix will be added.
! When using PATH_LIST, no file suffix is added.  If the user wants file
! suffixes, he/she must specify them with each entry in PATH_LIST file.
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAMES_FROM">
!<Tip> Choose PATHNAME_FROM using a file selection dialog box. </Tip>
! Choose PATHNAME_FROM using a file selection dialog box. 
!</Help>
!
!<Help KEYWORD="STATUS">
!<Tip> File status. </Tip>
! R   -- File found and readable.
! RE  -- File not found or not readable.
!</Help>
!
!<Help KEYWORD="PATHNAMES_FROM">
!<Tip> Pathname(s) for the file(s) to be copied using rcp. </Tip>
! Default = blank
! Allowed = char
! Pathname of file(s) to be copied to remote location via rcp.
!</Help>
!
!<Help KEYWORD="PATH_LIST_FROM">
!<Tip> Pathname of file containing pathnames of files to be copied. </Tip>
! Default =  -
! Allowed = char
! Pathname of file containing list of pathnames of files to be copied.  This
! file must be an ascii file with one pathname on each line.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_LIST_FROM">
!<Tip> This button accesses the Select PATH_LIST_FROM dialog box. </Tip>
! Choose a PATH_LIST_FROM via a file selection dialog.
!</Help>
!
!<Help KEYWORD="PATH_LIST_FROM_INFO">
!<Tip> File status for PATH_LIST_FROM. </Tip>
! Pathname of ASCII list file.
!</Help>
!
!<Help KEYWORD="REMOTE_USER">
!<Tip>Remote logon id that will be used for the rcp.</Tip>
! Default = Current user id
! Allowed = char*40
!
! Remote logon id that will be used for the rcp command.
!</Help>
!
!<Help KEYWORD="REMOTE_NODE">
!<Tip>Remote machine name that will be used for the rcp.</Tip>
! Default = hotce03.ho.conoco.com
! Allowed = char*40
!
! Remote machine name that will be used for the rcp command.
!</Help>
!
!<Help KEYWORD="PATH_DIR_TO">
!<Tip>The default directory name appended to remote pathnames.</Tip>
! Default = /t/REMOTE_USER
! Allowed = char*160
!
! The default directory name appended to remote pathnames.
!</Help>
!
!<Help KEYWORD="PATHNAMES_TO">
!<Tip> Pathname(s) for the remote file(s) to be created. </Tip>
! Default = blank
! Allowed = char
! Pathname of remote file(s) to be created via rcp.
!</Help>
!
!<Help KEYWORD="PATH_LIST_TO">
!<Tip> Pathname of file containing pathnames of remote files. </Tip>
! Default =  -
! Allowed = char
! Pathname of file containing list of pathnames of remote files to be created.
! This file must be an ascii file with one pathname on each line.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_LIST_TO">
!<Tip> This button accesses the Select PATH_LIST_TO dialog box. </Tip>
! Choose a PATH_LIST_TO via a file selection dialog.
!</Help>
!
!<Help KEYWORD="PATH_LIST_TO_INFO">
!<Tip> File status for PATH_LIST_TO. </Tip>
! Pathname of ASCII list file.
!</Help>
!</HelpSection>
!
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module rcpout_module

      use cio_module
      use finquire_module
      use getsys_module
      use named_constants_module
      use path_module
      use pathcheck_module
      use pathchoose_module
      use pc_module
      use putsys_module
      use string_module

      implicit none

      private

      public :: rcpout_create
      public :: rcpout_initialize
      public :: rcpout_update
      public :: rcpout_delete
      public :: rcpout_wrapup


      character(len=100),public,save :: rcpout_IDENT = &
'$Id: rcpout.f90,v 1.3 2006/12/04 13:29:55 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: rcpout_struct              
 
        private
        logical                                :: skip_wrapup !wrapup flag
        character(len=9)                       :: opt_file    !process parameter
        integer                                :: nopt_file_menu
        character(len=9)                       :: opt_file_menu(2)
        character(len=FILENAME_LENGTH)         :: pathname_trcio
                                                              !process parameter
        character(len=FILENAME_LENGTH)         :: path_dir_to !process parameter
        character(len=40)                      :: remote_user !process parameter
        character(len=40)                      :: remote_node !process parameter
        character(len=FILENAME_LENGTH)         :: path_list_from
                                                             ! process parameter
        character(len=FILENAME_LENGTH)         :: path_list_to
                                                             ! process parameter
        character(len=FILENAME_LENGTH),pointer :: pathnames_from(:) 
                                                             ! process parameter
        character(len=FILENAME_LENGTH),pointer :: pathnames_to(:) 
                                                             ! process parameter
        integer                                :: npathnames_from
                                                             ! process parameter
        integer                                :: npathnames_to
                                                             ! process parameter
        type(pathchoose_struct),pointer        :: pathnames_from_dialog
                                                             ! process parameter
        type(pathchoose_struct),pointer        :: path_list_from_dialog
                                                             ! process parameter
        type(pathchoose_struct),pointer        :: path_list_to_dialog
                                                             ! process parameter
        character(len=2),pointer               :: status(:)
        integer                                :: nstatus
        character(len=32)                      :: rcpfile 
        character(len=4)                       :: cipn

      end type rcpout_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(rcpout_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine rcpout_create (obj)
      implicit none
      type(rcpout_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%pathnames_from)
      nullify (obj%pathnames_to)
      nullify (obj%pathnames_from_dialog) ! jpa
      nullify (obj%path_list_from_dialog) ! jpa
      nullify (obj%path_list_to_dialog) ! jpa
      nullify (obj%status)

      allocate(obj%pathnames_from(1))
      allocate(obj%pathnames_to(1))
      allocate(obj%status(1))

      call pathchoose_create (obj%pathnames_from_dialog,'PATHNAMES_FROM','trc')
      call pathchoose_create (obj%path_list_from_dialog,'PATH_LIST_FROM','lst')
      call pathchoose_create (obj%path_list_to_dialog  ,'PATH_LIST_TO'  ,'lst')
      call rcpout_initialize (obj)
      return
      end subroutine rcpout_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine rcpout_delete (obj)
      implicit none
      type(rcpout_struct),pointer :: obj       ! arguments

      call rcpout_wrapup (obj)

      if (associated(obj%pathnames_from)) deallocate(obj%pathnames_from)
      if (associated(obj%pathnames_to))   deallocate(obj%pathnames_to)
      if (associated(obj%status))         deallocate(obj%status)
      call pathchoose_delete (obj%pathnames_from_dialog)
      call pathchoose_delete (obj%path_list_from_dialog)
      call pathchoose_delete (obj%path_list_to_dialog)

      deallocate(obj)

      return
      end subroutine rcpout_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine rcpout_initialize (obj)
      implicit none
      type(rcpout_struct),intent(inout)             :: obj         ! arguments

      character(len=10)                             :: request_id  ! local

      obj%nopt_file_menu    = 2
      obj%opt_file_menu(1)  = 'PATHNAMES'
      obj%opt_file_menu(2)  = 'PATH_LIST'
      obj%opt_file          = 'PATHNAMES'
      obj%path_list_from    = PATH_EMPTY
      obj%path_list_to      = PATH_EMPTY
      obj%npathnames_from   = 0
      obj%npathnames_to     = 0
      obj%nstatus           = 0
      obj%remote_node       = 'hotce03.ho.conoco.com'
      call getsys_username (obj%remote_user)
      obj%path_dir_to       = '/t/'//trim(obj%remote_user)
      obj%pathname_trcio    = ' '

      call string_ii2cc (pc_get_ipn(),obj%cipn)
      call getsys_env ('PBS_REQID',request_id)
      if (trim(request_id) .eq. 'NONE') request_id = '$PBS_REQID'
      obj%rcpfile = 'rcpout'//trim(request_id)//'_'//trim(obj%cipn)

      call rcpout_update (obj)

      return
      end subroutine rcpout_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine rcpout_update (obj)
      implicit none
      type(rcpout_struct),intent(inout),target     :: obj          ! arguments

      integer                                       :: i           ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%pathnames_from_dialog,obj%pathnames_from,  &
                                                      obj%npathnames_from, &
                                                      rcpout_etrap)) return
      if (pathchoose_update(obj%path_list_from_dialog,obj%path_list_from,  &
                                                      rcpout_trap)) return
      if (pathchoose_update(obj%path_list_to_dialog,obj%path_list_to,      &
                                                      rcpout_trap)) return

      call pc_register_array_names ("status_arrayset", (/  &
                                    "status        ",      &
                                    "pathnames_from" /))

      call pc_get_jdata ('PATHNAME_TRCIO',obj%pathname_trcio)

      call pc_get   ('OPT_FILE'      ,obj%opt_file      ,rcpout_trap)
      call pc_get   ('REMOTE_USER'   ,obj%remote_user   ,rcpout_trap)
      call pc_get   ('REMOTE_NODE'   ,obj%remote_node   ,rcpout_trap)
      call pc_get   ('PATH_DIR_TO'   ,obj%path_dir_to   ,rcpout_trap)
      call pc_get   ('PATH_LIST_FROM',obj%path_list_from,rcpout_trap)
      call pc_get   ('PATH_LIST_TO'  ,obj%path_list_to  ,rcpout_trap)
      call pc_alloc ('PATHNAMES_FROM',obj%pathnames_from,obj%npathnames_from,  &
                     rcpout_etrap)
      call pc_alloc ('PATHNAMES_TO'  ,obj%pathnames_to  ,obj%npathnames_to,  &
                      rcpout_etrap)

      call pc_call_array_trap ('PATHNAMES_FROM', rcpout_trap)
      call pc_call_array_trap ('PATHNAMES_TO'  , rcpout_trap)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      select case (trim(obj%opt_file))
        case ('PATHNAMES')
          if (obj%nstatus .ne. obj%npathnames_from .or.  &
              obj%nstatus .ne. obj%npathnames_to) then
            i = min(obj%nstatus,obj%npathnames_from,obj%npathnames_to)
            obj%nstatus         = i
            obj%npathnames_from = i
            obj%npathnames_to   = i
          endif
          obj%path_list_from      = PATH_EMPTY
          obj%path_list_to        = PATH_EMPTY
        case ('PATH_LIST')
          obj%nstatus         = 0
          obj%npathnames_from = 0
          obj%npathnames_to   = 0
        case default
          call pc_error('OPT_FILE must be PATHNAMES or PATH_LIST.')
      end select

      call pathcheck ('PATH_LIST_FROM', obj%path_list_from, '.lst',  &
                      required=.false., show=PATHCHECK_INFO_INPUT)
      call pathcheck ('PATH_LIST_TO'  , obj%path_list_to  , '.lst',  &
                      required=.false., show=PATHCHECK_INFO_INPUT)

      call pc_call_end_trap (rcpout_end_trap)


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      select case (trim(obj%opt_file))
        case ('PATHNAMES')
          call pc_put_sensitive_field_flag('SELECT_PATHNAMES_FROM', .true. )
          call pc_put_sensitive_field_flag('STATUS_ARRAYSET'      , .true. )
          call pc_put_sensitive_field_flag('PATHNAMES_TO'         , .true. )
          call pc_put_sensitive_field_flag('SELECT_PATH_LIST_FROM', .false.)
          call pc_put_sensitive_field_flag('PATH_LIST_FROM'       , .false.)
          call pc_put_sensitive_field_flag('PATH_LIST_FROM_INFO'  , .false.)
          call pc_put_sensitive_field_flag('SELECT_PATH_LIST_TO'  , .false.)
          call pc_put_sensitive_field_flag('PATH_LIST_TO'         , .false.)
          call pc_put_sensitive_field_flag('PATH_LIST_TO_INFO'    , .false.)
        case ('PATH_LIST')
          call pc_put_sensitive_field_flag('SELECT_PATHNAMES_FROM', .false.)
          call pc_put_sensitive_field_flag('STATUS_ARRAYSET'      , .false.)
          call pc_put_sensitive_field_flag('PATHNAMES_TO'         , .false.)
          call pc_put_sensitive_field_flag('SELECT_PATH_LIST_FROM', .true. )
          call pc_put_sensitive_field_flag('PATH_LIST_FROM'       , .true. )
          call pc_put_sensitive_field_flag('PATH_LIST_FROM_INFO'  , .true. )
          call pc_put_sensitive_field_flag('SELECT_PATH_LIST_TO'  , .true. )
          call pc_put_sensitive_field_flag('PATH_LIST_TO'         , .true. )
          call pc_put_sensitive_field_flag('PATH_LIST_TO_INFO'    , .true. )
      end select

      call pc_put_options_field('OPT_FILE',obj%opt_file_menu,obj%nopt_file_menu)

      call pc_put ('OPT_FILE'       ,obj%opt_file      )
      call pc_put ('REMOTE_USER'    ,obj%remote_user   )
      call pc_put ('REMOTE_NODE'    ,obj%remote_node   )
      call pc_put ('PATH_DIR_TO'    ,obj%path_dir_to   )
      call pc_put ('PATH_LIST_FROM' ,obj%path_list_from)
      call pc_put ('PATH_LIST_TO'   ,obj%path_list_to  )
      call pc_put ('PATHNAMES_FROM' ,obj%pathnames_from,obj%npathnames_from)
      call pc_put ('PATHNAMES_TO'   ,obj%pathnames_to  ,obj%npathnames_to  )
      call pc_put_gui_only ('STATUS',obj%status        ,obj%nstatus        )

      call pc_put_control ('SETUP_ONLY', .true.)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine rcpout_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine rcpout_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword   ! argument

      integer                      ::  i         ! local
      integer                      ::  istat     ! local

      select case (keyword)

        case ('OPT_FILE')
          call string_to_upper(object%opt_file)

        case ('PATHNAMES_FROM')
          if (object%npathnames_from .gt. 0) then
            if (associated(object%status)) deallocate(object%status)
            object%nstatus = object%npathnames_from
            allocate(object%status(object%nstatus),stat=istat)
            do i=1,object%npathnames_from
              call path_fixup_filename(object%pathnames_from(i),  &
                                       object%pathname_trcio,'trc')
              select case (finquire_input(object%pathnames_from(i)))
                case(FINQUIRE_FOUND)
                  object%status(i) = 'R'
                case default
                  object%status(i) = 'RE'
              end select
            enddo
          endif

        case ('PATHNAMES_TO')
          if (object%npathnames_to .gt. 0) then
            do i=1,object%npathnames_to
              call path_fixup_filename(object%pathnames_to(i),  &
                                       object%path_dir_to,'trc')
            enddo
          endif

        case ('SELECT_PATH_LIST_FROM','PATH_LIST_FROM')
          if (len_trim(object%path_list_from) .eq. 0 .or.       &
              trim(object%path_list_from)     .eq. PATH_EMPTY)  return
          if (len_trim(object%path_list_to) .eq. 0 .or.       &
              trim(object%path_list_to)     .eq. PATH_EMPTY)  &
              object%path_list_to = trim(object%path_list_from)

        case ('SELECT_PATH_LIST_TO','PATH_LIST_TO')
          if (len_trim(object%path_list_to) .eq. 0 .or.       &
              trim(object%path_list_to)     .eq. PATH_EMPTY)  return
          if (len_trim(object%path_list_from) .eq. 0 .or.       &
              trim(object%path_list_from)     .eq. PATH_EMPTY)  &
              object%path_list_from = trim(object%path_list_to)

        case default

      end select

      end subroutine rcpout_trap


      subroutine rcpout_etrap (keyword,indx,action)
      implicit none
      character(len=*),intent(in)  ::  keyword   ! argument
      integer         ,intent(in)  ::  indx      ! argument
      integer         ,intent(in)  ::  action    ! argument

      character(len=FILENAME_LENGTH) :: value    ! local

      select case (keyword)
        case ('PATHNAMES_FROM','SELECT_PATHNAMES_FROM')
          if (indx.ge.1 .and. indx.le.object%npathnames_from) then
            call path_fixup_filename (object%pathnames_from(indx),  &
                                      object%pathname_trcio,'trc')
            select case (action)
              case (PC_INSERT)
                value = path_get_file(object%pathnames_from(indx))
                call path_fixup_filename(value,object%path_dir_to)
                call rcpout_insert_array_element(object%pathnames_to,  &
                                                 object%npathnames_to, &
                                                 value,indx)
                select case (finquire_input(object%pathnames_from(indx)))
                  case(FINQUIRE_FOUND)
                    value = 'R'
                  case default
                    value = 'RE'
                end select
                call rcpout_insert_array_element(object%status,  &
                                                 object%nstatus, &
                                                 value,indx)
              case (PC_MODIFY)
                value = path_get_file(object%pathnames_from(indx))
                call path_fixup_filename(value,object%path_dir_to)
                object%pathnames_to(indx) = value
                select case (finquire_input(object%pathnames_from(indx)))
                  case(FINQUIRE_FOUND)
                    object%status(indx) = 'R'
                  case default
                    object%status(indx) = 'RE'
                end select
              case (PC_REMOVE)
                call rcpout_remove_array_element(object%pathnames_to,  &
                                                 object%npathnames_to, &
                                                 indx)
            end select
          endif

        case ('PATHNAMES_TO','SELECT_PATHNAMES_TO')
          if (indx.ge.1 .and. indx.le.object%npathnames_to) then
            call path_fixup_filename(object%pathnames_to(indx),  &
                                     object%path_dir_to,'trc')
            select case (action)
              case (PC_INSERT)
                value = path_get_file(object%pathnames_to(indx))
                call path_fixup_filename (value,object%pathname_trcio,'trc')
                call rcpout_insert_array_element(object%pathnames_from,  &
                                                 object%npathnames_from, &
                                                 value,indx)
                select case (finquire_input(object%pathnames_from(indx)))
                  case(FINQUIRE_FOUND)
                    value = 'R'
                  case default
                    value = 'RE'
                end select
                call rcpout_insert_array_element(object%status,  &
                                                 object%nstatus, &
                                                 trim(value),indx)
              case (PC_REMOVE)
                call rcpout_remove_array_element(object%pathnames_from,  &
                                                 object%npathnames_from, &
                                                 indx)
                call rcpout_remove_array_element(object%status,  &
                                                 object%nstatus, &
                                                 indx)
            end select
          endif

      end select

      end subroutine rcpout_etrap

      subroutine rcpout_end_trap

      integer                                       :: i           ! local
      character(len=FILENAME_LENGTH)                :: ctmp        ! local
      logical                                       :: do_rcp      ! local

      do_rcp = .false.
      select case (trim(object%opt_file))
        case ('PATHNAMES')
          if (object%npathnames_from .gt. 0) then
            i = 1
            do
              if (i .gt. object%npathnames_from) exit
              call string_squeeze_blanks(object%pathnames_from(i))
              call string_squeeze_blanks(object%pathnames_to(i))
              call string_to_upper (object%pathnames_from(i), ctmp)
              if (trim(ctmp) .eq. 'NONE') object%pathnames_from(i) = ' '
              if (len_trim(object%pathnames_from(i)) .ne. 0 .and.  &
                  len_trim(object%pathnames_to(i))   .ne. 0 ) then
                  i = i + 1
                  cycle
              endif
              call rcpout_remove_array_element (object%status,  &
                                                object%nstatus, i)
              call rcpout_remove_array_element (object%pathnames_from,  &
                                                object%npathnames_from, i)
              call rcpout_remove_array_element (object%pathnames_to,  &
                                                object%npathnames_to, i)
            enddo
          endif
          if (object%npathnames_from .gt. 0) do_rcp = .true.
        case ('PATH_LIST')
          if (len_trim(object%path_list_from) .gt. 0 .and.  &
              trim(object%path_list_from) .ne. PATHCHECK_EMPTY) do_rcp = .true.
      end select

      if (.not. do_rcp) then
        call pc_info ('No files to rcpout.')
      endif

      end subroutine rcpout_end_trap


!!------------------------ insert_array_element ----------------------------!!
!!------------------------ insert_array_element ----------------------------!!
!!------------------------ insert_array_element ----------------------------!!


      subroutine rcpout_insert_array_element (array,narray,value,indx)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument
      integer         ,intent(in)              :: indx                 !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) then
        narray = 1
        allocate(array(narray))
        array(1) = value
      else
        if (narray .gt. 0) then
          allocate(temp_array(narray))
          temp_array(1:narray) = array(1:narray)
          deallocate(array)
          allocate(array(narray+1))
          array(1:indx-1) = temp_array(1:indx-1)
          array(indx) = value
          if (indx .le. narray) array(indx+1:narray+1) = temp_array(indx:narray)
          narray = narray + 1
          deallocate(temp_array)
        else
          deallocate(array)
          narray = 1
          allocate(array(narray))
          array(1) = value
        endif
      endif

      return
      end subroutine rcpout_insert_array_element


!!------------------------ remove_array_element ----------------------------!!
!!------------------------ remove_array_element ----------------------------!!
!!------------------------ remove_array_element ----------------------------!!


      subroutine rcpout_remove_array_element (array,narray,indx)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      integer         ,intent(in)              :: indx                 !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (associated(array)) then
        if (narray .gt. 0) then
          allocate(temp_array(narray))
          temp_array(1:narray) = array(1:narray)
          deallocate(array)
          allocate(array(narray-1))
          if (indx .eq. 1) then
            array(1:narray-1) = temp_array(2:narray)
          else
            array(1:indx-1) = temp_array(1:indx-1)
            if (indx .le. narray) array(indx:narray-1)=temp_array(indx+1:narray)
          endif
          narray = narray - 1
          deallocate(temp_array)
        endif
      endif

      return
      end subroutine rcpout_remove_array_element


!!-------------------------- remove_blank_rows -----------------------------!!
!!-------------------------- remove_blank_rows -----------------------------!!
!!-------------------------- remove_blank_rows -----------------------------!!


      subroutine rcpout_remove_blank_rows (array,narray)

      character(len=*),pointer                 :: array(:)
      integer                                  :: narray

      integer                                  :: i                    !local
      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) return
      if (narray .eq. 0) return

      do i = 1, narray
        if (len_trim(array(i)) .ne. 0) cycle
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray-1))
        if (i .gt. 1) then
          array(1:i-1) = temp_array(1:i-1)
          array(i:narray-1) = temp_array(i+1:narray)
        else
          array(1:narray-1) = temp_array(i+1:narray)
        endif
        narray = narray - 1
        deallocate(temp_array)
      enddo

      return
      end subroutine rcpout_remove_blank_rows


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine rcpout_wrapup (obj)
      implicit none
      type(rcpout_struct),pointer                   :: obj         ! arguments

      integer                                       :: i           ! local
      integer                                       :: istat       ! local
      integer                                       :: nfiles      ! local
      integer                                       :: lun         ! local
      integer                                       :: lun_to      ! local
      integer                                       :: lun_from    ! local
      integer                                       :: nstr_from   ! local
      integer                                       :: nstr_to     ! local
      character(len=2*FILENAME_LENGTH+50)           :: string      ! local
      character(len=FILENAME_LENGTH)                :: str_from    ! local
      character(len=FILENAME_LENGTH)                :: str_to      ! local
      logical                                       :: do_rcp      ! local
      integer                                       :: temp

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      do_rcp = .false.
      select case (trim(obj%opt_file))
        case ('PATHNAMES')
          if (obj%npathnames_from .ne. obj%npathnames_to) then
            i = min(obj%npathnames_from,obj%npathnames_to)
            obj%npathnames_from = i
            obj%npathnames_to   = i
          endif
          if (obj%npathnames_from .gt. 0) do_rcp = .true.
        case ('PATH_LIST')
          if (len_trim(obj%path_list_from) .gt. 0 .and.  &
              trim(obj%path_list_from) .ne. PATHCHECK_EMPTY) do_rcp = .true.
        case default
          call pc_error('OPT_FILE must be PATHNAMES or PATH_LIST.')
      end select

      if (do_rcp) then
        nfiles = 0
        lun = cio_fopen(trim(obj%rcpfile),'w',  &
                        file_space_commit=PREALLOCATE_FILE_SPACE_DISABLED, &
                        file_lock=FILE_LOCK_DISABLED)

        if (lun .lt. 100) then
          call pc_error('Error opening file '//trim(obj%rcpfile))
          return
        endif
        select case (trim(obj%opt_file))
          case ('PATHNAMES')
            if (obj%npathnames_from .gt. 0) then
              do i=1,obj%npathnames_from
                if (finquire_input(obj%pathnames_from(i)) .eq.  &
                    FINQUIRE_ERROR) then
                  call pc_info('RCPOUT (IPN='//trim(obj%cipn)//  &
                               ') File '//trim(obj%pathnames_from(i))//  &
                               ' not found or not readable')
                  cycle
                endif
                string = 'prcp '//trim(obj%pathnames_from(i))//' '//  &
                          trim(obj%remote_user)//'@'//trim(obj%remote_node)//  &
                         ':"'//trim(obj%pathnames_to(i))//'"'
                call pc_info('RCPOUT (IPN='//trim(obj%cipn)//') '//trim(string))
                temp  = len_trim(string)
                istat = cio_fputline(string,temp,lun)
                nfiles = nfiles + 1
              enddo
            endif
          case ('PATH_LIST')
            if (len_trim(obj%path_list_from) .gt. 0 .and.  &
                trim(obj%path_list_from) .ne. PATHCHECK_EMPTY) then
              lun_from = cio_fopen(trim(obj%path_list_from),'r')
              if (lun_from .lt. 100) then
                call pc_error('Error opening file '//trim(obj%path_list_from))
                return
              endif
              lun_to   = cio_fopen(trim(obj%path_list_to)  ,'r')
              if (lun_to .lt. 100) then
                call pc_error('Error opening file '//trim(obj%path_list_to))
                return
              endif
              do
                nstr_from = cio_fgetline(str_from,FILENAME_LENGTH,lun_from)
                if (nstr_from .lt. 0) exit
                nstr_to   = cio_fgetline(str_to  ,FILENAME_LENGTH,lun_to)
                if (nstr_to   .lt. 0) exit
                call path_fixup_filename (str_from,obj%pathname_trcio)
                call path_fixup_filename (str_to  ,obj%path_dir_to)
                if (finquire_input(str_from) .eq. FINQUIRE_ERROR) then
                  call pc_info('RCPOUT (IPN='//trim(obj%cipn)//  &
                               ') File '//trim(str_from)//  &
                               ' not found or not readable')
                  cycle
                endif
                string = 'prcp '//trim(str_from)//' '//  &
                          trim(obj%remote_user)//'@'//trim(obj%remote_node)//  &
                         ':"'//trim(str_to)//'"'
                call pc_info('RCPOUT (IPN='//trim(obj%cipn)//') '//trim(string))
                temp  = len_trim(string)
                istat = cio_fputline(string,temp,lun)
                nfiles = nfiles + 1
              enddo
              istat = cio_fclose(lun_from)
              istat = cio_fclose(lun_to)
            endif
        end select

        istat = cio_fclose(lun)
        if (nfiles .gt. 0) then
          call putsys_texec ('/usr/app/vendors/sps/scripts/rcpjob '//  &
                             trim(obj%rcpfile),istat)
          if (istat .ne. 0) call pc_info('RCPOUT (IPN='//trim(obj%cipn)//') '//&
                                         ' Error: putsys_texec')
        endif
      endif

      end subroutine rcpout_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module rcpout_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

