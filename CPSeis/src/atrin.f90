!<CPS_v1 type="PROCESS"/>
!!------------------------------- atrin.f90 ---------------------------------!!
!!------------------------------- atrin.f90 ---------------------------------!!
!!------------------------------- atrin.f90 ---------------------------------!!


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
! Name       : ATRIN                     (Ascii Trace Input)
! Category   : io
! Written    : 2004-06-08   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Read traces from an ascii file.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Read traces from an ascii file.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This is a trace-supplying process.
! Traces are read into the CPS job from an ascii file.
! The file does not contain any trace headers.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs traces in a gather.
! The gather contains all traces on the input file.
!
! The number of traces on the file and in the gather will be NUM_COLUMNS if
! the file does not contain a time column, or NUM_COLUMNS - 1 if the file
! does contain a time column.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set to #traces on file.
! GATHERED  whether traces are a legitimate gather  set to TRUE.
! NWIH      number of words in trace header         used but not changed.
! NDPT      number of sample values in trace        used but not changed.
! TSTRT     starting time on trace                  used but not changed.
! DT        trace sample interval                   used but not changed.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!   1     sequence number            set appropriately.
!   2     top mute                   set to 1.
!   3     current gather             set to 1.
!   4     current channel            set to trace number (1 thru NUMTR).
!   5     fold                       set to 1.
!   9     original gather            set to 1.
!  10     original channel           set to trace number (1 thru NUMTR).
!  25     largest absolute value     set appropriately.
!  64     bottom mute                set to NDPT.
!
! All other header words are set to zero by this process.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!003. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!  2. 2005-02-21  Stoeckley  Modify to set trace samples to an average of the
!                             values of the same sample on the file instead
!                             of the last value.
!  1. 2004-06-08  Stoeckley  Initial version.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! Upon input, NTR must have one of these values:
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
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


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS ATRIN Process/NC=80>
!                           Ascii Trace Input
!
! NUM_COLUMNS~~~~~=`II              [/L]Number of columns on file.
! FIRSTLINE~~~~~~~=`IIII            [/L]Row on file to start reading.
! NILSTRING~~~~~~~=`SSSSSSSSSSSSSS  [/L]Symbol for nil value on file.
! INTERPOLATE_NILS=`KKK    [/L]Whether to replace nils with interpolated values.
!
! `---------------------------
!  TIMES_IN_COLUMN=`KKK        [/L]Whether times are in a column on the file.
!
!  TIME_COLUMN=`II             [/L]Column number containing time values on file.
!  TIME_UNITS =`CCCCCCCCCCCCC  [/L]Units of times in TIME_COLUMN on file.
!
!  FILE_TSTRT =`FFFFFFFFFFFF   [/L]Trace starting time on file (seconds).
!  FILE_DT~~~~=`FFFFFFFFFFFF   [/L]Trace sample interval on file (seconds).
!
!  numtr~~~~~~=`XX             [/L]Number of traces on file and in gather.
! `---------------------------
!
! Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [pathname_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS PATHNAME[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="FILE_DT">
!<Tip>Trace sample interval on file (seconds).</Tip>
! Default = 0.004
! Allowed = any positive floating point value
!
! Used only if TIMES_IN_COLUMN is NO.
!</Help>
!
!
!<Help KEYWORD="FIRSTLINE">
!<Tip>Row on file to start reading.</Tip>
! Default = 1
! Allowed = integer >= 1
!
! This parameter allows header information to be skipped when reading the file.
! Blank lines and any lines beginning with a pound sign (#) are always skipped.
!</Help>
!
!
!<Help KEYWORD="NILSTRING">
!<Tip>Symbol for nil value on file.</Tip>
! Default = nil
! Allowed = any character string containing no blank characters
!</Help>
!
!
!<Help KEYWORD="FILE_TSTRT">
!<Tip>Trace starting time on file (seconds).</Tip>
! Default = 0.0
! Allowed = any floating point value
!
! Used only if TIMES_IN_COLUMN is NO.
!</Help>
!
!
!<Help KEYWORD="NUM_COLUMNS">
!<Tip>Number of columns on file.</Tip>
! Default = 1
! Allowed = any integer >= 2 (if TIMES_IN_COLUMN is YES).
! Allowed = any integer >= 1 (if TIMES_IN_COLUMN is NO).
!</Help>
!
!
!<Help KEYWORD="INTERPOLATE_NILS">
!<Tip>Whether to replace nils with interpolated values.</Tip>
! Default = yes
! Allowed = yes or no
!</Help>
!
!
!<Help KEYWORD="TIMES_IN_COLUMN">
!<Tip>Whether times are in a column on the file.</Tip>
! Default = NO
! Allowed = YES or NO.
!</Help>
!
!
!<Help KEYWORD="PATHNAME">
!<Tip>Name of input file containing ascii traces.</Tip>
! Default = NONE
! Allowed = valid file name.
!
! This must be an ascii file containing one or more columns of numbers.
! The number of columns on the file must be NUM_COLUMNS.
!
! If TIMES_IN_COLUMN is NO, each column corresponds to a trace.
! If TIMES_IN_COLUMN is YES, each column except one corresponds to a trace.
!</Help>
!
!
!<Help KEYWORD="NUMTR" TYPE="DISPLAY_ONLY">
!<Tip>Number of traces on file and in gather.</Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_INFO" TYPE="DISPLAY_ONLY">
!<Tip>Status of PATHNAME.</Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip>Choose PATHNAME using a file selection dialog box.</Tip>
!</Help>
!
!
!<Help KEYWORD="TIME_COLUMN">
!<Tip>Column number containing time values on file.</Tip>
! Default = 1
! Allowed = 1 - NUM_COLUMNS
!
! Used only if TIMES_IN_COLUMN is YES.
!</Help>
!
!
!<Help KEYWORD="TIME_UNITS">
!<Tip>Units of times in TIME_COLUMN on file.</Tip>
! Default = seconds
! Allowed = seconds
! Allowed = milliseconds
!
! Used only if TIMES_IN_COLUMN is YES.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module atrin_module
      use pc_module
      use named_constants_module
      use pathchoose_module
      use pathcheck_module
      use floatio_module
      use terputil_module
      use lav_module

      implicit none
      private
      public :: atrin_create
      public :: atrin_initialize
      public :: atrin_update
      public :: atrin_delete
      public :: atrin            ! main trace processing routine.
      public :: atrin_wrapup

      character(len=100),public,save :: ATRIN_IDENT = &
'$Id: atrin.f90,v 1.3 2006/09/18 13:32:37 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: atrin_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

        integer                    :: numtr    ! max number of input traces.
        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).

        integer                        :: firstline        ! process parameter
        character(len=20)              :: nilstring        ! process parameter
        integer                        :: num_columns      ! process parameter
        logical                        :: interpolate_nils ! process parameter
        logical                        :: times_in_column  ! process parameter
        integer                        :: time_column      ! process parameter
        character(len=20)              :: time_units       ! process parameter
        real                           :: file_tstrt       ! process parameter
        real                           :: file_dt          ! process parameter
        character(len=FILENAME_LENGTH) :: pathname         ! process parameter

        integer                         :: sequence      ! dependent
        type(pathchoose_struct),pointer :: pathchoose    ! dependent

      end type atrin_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                   ,save :: lunprint  ! unit number for printing.
      type(atrin_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine atrin_create (obj)
      type(atrin_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()
      allocate (obj)
      nullify (obj%pathchoose) ! jpa

      call pathchoose_create (obj%pathchoose, 'PATHNAME', '*')
      call atrin_initialize  (obj)
      end subroutine atrin_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine atrin_delete (obj)
      type(atrin_struct),pointer :: obj       ! arguments

      call atrin_wrapup      (obj)
      call pathchoose_delete (obj%pathchoose)

      deallocate(obj)
      end subroutine atrin_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine atrin_initialize (obj)
      type(atrin_struct),intent(inout) :: obj       ! arguments

      obj%firstline        = 1
      obj%nilstring        = 'nil'
      obj%num_columns      = 1
      obj%interpolate_nils = .true.
      obj%times_in_column  = .false.
      obj%time_column      = 1
      obj%time_units       = 'seconds'
      obj%file_tstrt       = 0.0
      obj%file_dt          = 0.004
      obj%pathname         = PATHCHECK_EMPTY

      call atrin_update (obj)
      end subroutine atrin_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine atrin_update (obj)
      type(atrin_struct),intent(inout),target :: obj             ! arguments

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%pathchoose,obj%pathname)) return

      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)

      call pc_get ('firstline        ', obj%firstline)
      call pc_get ('nilstring        ', obj%nilstring)
      call pc_get ('NUM_COLUMNS      ', obj%num_columns)
      call pc_get ('interpolate_nils ', obj%interpolate_nils)
      call pc_get ('times_in_column  ', obj%times_in_column)
      call pc_get ('TIME_COLUMN      ', obj%time_column)
      call pc_get ('TIME_UNITS       ', obj%time_units)
      call pc_get ('FILE_TSTRT       ', obj%file_tstrt)
      call pc_get ('FILE_DT          ', obj%file_dt)
      call pc_get ('PATHNAME         ', obj%pathname)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%times_in_column) obj%num_columns = max(obj%num_columns,2)

      if (obj%firstline   <                1) obj%firstline   = 1
      if (obj%nilstring   ==             ' ') obj%nilstring   = 'nil'
      if (obj%num_columns <                1) obj%num_columns = 1
      if (obj%time_column <                1) obj%time_column = 1
      if (obj%time_column >  obj%num_columns) obj%time_column = obj%num_columns
      if (obj%time_units  /=       'seconds') obj%time_units  = 'milliseconds'
      if (obj%file_dt     <=             0.0) obj%file_dt     = 0.004

      call pathcheck ('PATHNAME', obj%pathname, required=.true.,  &
                      show=PATHCHECK_INFO_INPUT)

      obj%numtr = obj%num_columns

      if (obj%times_in_column) obj%numtr = obj%num_columns - 1


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('TIME_UNITS', (/'seconds      ',     &
                                                 'milliseconds '/) )

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', .true.)

      call pc_put ('firstline        ', obj%firstline)
      call pc_put ('nilstring        ', obj%nilstring)
      call pc_put ('NUM_COLUMNS      ', obj%num_columns)
      call pc_put ('interpolate_nils ', obj%interpolate_nils)
      call pc_put ('times_in_column  ', obj%times_in_column)
      call pc_put ('TIME_COLUMN      ', obj%time_column)
      call pc_put ('TIME_UNITS       ', obj%time_units)
      call pc_put ('FILE_TSTRT       ', obj%file_tstrt)
      call pc_put ('FILE_DT          ', obj%file_dt)
      call pc_put ('PATHNAME         ', obj%pathname)

      call pc_put_gui_only ('numtr', obj%numtr)

      call pc_put_control ('need_label'   , .true.)

      call pc_put_sensitive_field_flag ('time_column',      obj%times_in_column)
      call pc_put_sensitive_field_flag ('time_units' ,      obj%times_in_column)
      call pc_put_sensitive_field_flag ('file_tstrt' , .not.obj%times_in_column)
      call pc_put_sensitive_field_flag ('file_dt'    , .not.obj%times_in_column)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%sequence = 0


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine atrin_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine atrin (obj,ntr,hd,tr)
      type(atrin_struct),intent(inout) :: obj                    ! arguments
      integer           ,intent(inout) :: ntr                    ! arguments
      double precision  ,intent(inout) :: hd(:,:)                ! arguments
      real              ,intent(inout) :: tr(:,:)                ! arguments
      type(floatio_struct),pointer     :: floatio                ! local
      character(len=7)    ,parameter   :: secname = 'FOREIGN'    ! local
      integer                          :: err,irow,indx,itr      ! local
      integer                          :: icolumn                ! local
      integer                          :: nlines,ncolumns        ! local
      real                             :: time                   ! local
      character(len=80)                :: msg                    ! local
      real                             :: vline(obj%num_columns) ! local
      integer                       :: kount(obj%ndpt,obj%numtr) ! local

      nullify (floatio) ! jpa

      if (obj%sequence > 0) then
           ntr = NO_MORE_TRACES
           call atrin_wrapup (obj)
           return
      endif

      call floatio_easy_read (floatio,obj%pathname,nlines,ncolumns,err,msg, &
                              obj%firstline,obj%nilstring)

      if (err /= FLOATIO_OK) then
           call floatio_close (floatio)
           call pc_error ('ATRIN: error opening',obj%pathname)
           call pc_error ('ATRIN:',msg)
           call atrin_wrapup (obj)
           ntr = FATAL_ERROR
           return
      endif

      ntr = obj%numtr

      hd   (:,:) = 0.0
      tr   (:,:) = FNIL
      kount(:,:) = 0
                
      irow = 0
      do

           call floatio_read_line  (floatio,err,msg,vline)

           if (err == FLOATIO_EOF) exit

           if (err == FLOATIO_ERROR) then
                call floatio_close (floatio)
                call pc_error ('ATRIN: error reading',obj%pathname)
                call pc_error ('ATRIN:',msg)
                call atrin_wrapup (obj)
                ntr = FATAL_ERROR
                return
           endif

           if (obj%times_in_column) then
                time = vline(obj%time_column)
                if (time == FNIL) then
                     call floatio_close (floatio)
                     call pc_error ('ATRIN: illegal nil time on file')
                     call atrin_wrapup (obj)
                     ntr = FATAL_ERROR
                     return
                endif
                if (obj%time_units == 'milliseconds') time = 0.001 * time
           else
                irow = irow + 1
                time = obj%file_tstrt + (irow - 1) * obj%file_dt
           endif

           indx = 1 + nint((time - obj%tstrt) / obj%dt)
           if (indx < 1 .or. indx > obj%ndpt) cycle

           itr = 0
           do icolumn = 1,obj%num_columns
                if (obj%times_in_column) then
                     if (icolumn == obj%time_column) cycle
                endif
                itr = itr + 1
                if (vline(icolumn) == FNIL) cycle
                if (kount(indx,itr) == 0) then
                     tr(indx,itr) = vline(icolumn)
                else
                     tr(indx,itr) = tr(indx,itr) + vline(icolumn)
                endif
                kount(indx,itr) = kount(indx,itr) + 1
           enddo

      enddo

      call floatio_close (floatio)

      do itr = 1,ntr
      do indx = 1,obj%ndpt
           if (kount(indx,itr) > 1) &
                           tr(indx,itr) = tr(indx,itr) / kount(indx,itr)
      enddo
      enddo

      if (obj%interpolate_nils) then
           do itr = 1,ntr
                call terputil_replace_nils (tr(:,itr), obj%ndpt)
           enddo
      endif

      do itr = 1,ntr
           obj%sequence = obj%sequence + 1
           hd( 1,itr) = obj%sequence
           hd( 2,itr) = 1
           hd( 3,itr) = 1
           hd( 4,itr) = itr
           hd( 5,itr) = 1
           hd( 9,itr) = 1
           hd(10,itr) = itr
           hd(64,itr) = obj%ndpt
      enddo

      call lav_set_hdr (hd, tr, obj%ndpt, ntr)

      end subroutine atrin


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine atrin_wrapup (obj)
      type(atrin_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine atrin_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module atrin_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

