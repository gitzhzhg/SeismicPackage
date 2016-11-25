!<CPS_v1 type="PROCESS"/>
!!----------------------------- valuedump.f90 -------------------------------!!
!!----------------------------- valuedump.f90 -------------------------------!!
!!----------------------------- valuedump.f90 -------------------------------!!


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
! Name       : VALUEDUMP
! Category   : diagnostics
! Written    : 2004-01-21   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Dump trace values to file.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Dump trace values to file.
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
! Process is a single-trace process.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs the same traces as it receives (unaltered).
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of trace header words            used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#      Description                               Action taken
! ----      -----------                               ------------
! HDR_FLAG  Header word denoting flagged traces.      used but not changed
! HEADER    Header words listed in the HEADER array.  used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!003. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  2. 2005-01-03  Stoeckley  Improve some defaults.
!  1. 2004-01-21  Stoeckley  Initial version.
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
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
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
!<NS CC3D Process/NC=80>
!                    VALUEDUMP (dump trace values to file)
!
!  HDR_FLAG~~~~~~=`III          [/L]Header word denoting flagged traces.
!
!  TIME_INIT~~~~~=`FFFFFFFFFFF  [/L]First trace time to dump.      [/L]TSTRT=`XXXXXXXXX
!  TIME_INC~~~~~~=`FFFFFFFFFFF  [/L]Trace time increment to dump.  [/L]DT~~~=`XXXXXXXXX
!  TIME_LAST~~~~~=`FFFFFFFFFFF  [/L]Last trace time to dump.       [/L]TSTOP=`XXXXXXXXX
!
!  SELF_DEFINING =`KKK          [/L]Put self-defining header section on the file.
!
!  Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!          LABEL           HEADER WIDTH DECIMALS MULTIPLIER
!          `SSSSSSSSSSSSSSS`SSSSSS`IIIII`IIIIIIII`FFFFFFFFFFFF
!          `SSSSSSSSSSSSSSS`SSSSSS`IIIII`IIIIIIII`FFFFFFFFFFFF
!          `SSSSSSSSSSSSSSS`SSSSSS`IIIII`IIIIIIII`FFFFFFFFFFFF
!          `SSSSSSSSSSSSSSS`SSSSSS`IIIII`IIIIIIII`FFFFFFFFFFFF
!          `SSSSSSSSSSSSSSS`SSSSSS`IIIII`IIIIIIII`FFFFFFFFFFFF
!          `SSSSSSSSSSSSSSS`SSSSSS`IIIII`IIIIIIII`FFFFFFFFFFFF
!          `SSSSSSSSSSSSSSS`SSSSSS`IIIII`IIIIIIII`FFFFFFFFFFFF
!          `SSSSSSSSSSSSSSS`SSSSSS`IIIII`IIIIIIII`FFFFFFFFFFFF
!
!<PARMS pathname[/ML=128/XST]>
!<PARMS LABEL_ARRAYSET[/XST/YST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! If HDR_FLAG = 0, then all traces are dumped.
! Otherwise, only traces with a flag set in header word HDR_FLAG are dumped.
!</Help>
!
!
!<Help KEYWORD="PATHNAME">
!<Tip> Name for the output file containing trace value dump. </Tip>
! Default = NONE
! Allowed = char
!
! A file name must be specified.
! The extension .valuedump will always be appended.
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_INFO">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!
!<Help KEYWORD="TIME_INIT">
!<Tip> First trace time to dump. </Tip>
! Default = first time on trace.
! Allowed = real (in seconds).
!</Help>
!
!
!<Help KEYWORD="TIME_INC">
!<Tip> Trace time increment to dump. </Tip>
! Default = 0.1
! Allowed = real (in seconds).
!</Help>
!
!
!<Help KEYWORD="TIME_LAST">
!<Tip> Last trace time to dump. </Tip>
! Default = last time on trace.
! Allowed = real (in seconds).
!</Help>
!
!
!<Help KEYWORD="TSTRT">
!<Tip> First time on trace. </Tip>
!</Help>
!
!
!<Help KEYWORD="DT">
!<Tip> Trace sample interval. </Tip>
!</Help>
!
!
!<Help KEYWORD="TSTOP">
!<Tip> Last time on trace. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELF_DEFINING">
!<Tip> Whether to put a self-defining header section on the file. </Tip>
! Default = YES
! Allowed = YES
! Allowed = NO
!
! If this parameter is YES, a self-defining header section, plus various
! tags, will be put on the file.
!
! If this parameter is NO, only the columns of numbers will be put on the
! file, with the first line of the file containing the labels for the
! columns.  This choice is useful if the file will be given to an outside
! contractor.
!</Help>
!
!
!<Help KEYWORD="LABEL">
!<Tip> Label identifying the values in each column on the file. </Tip>
! Default = names built from the corresponding HEADER entries.
! Allowed = any single word.
!
! This is the identifier at the top of each column in the file.
!</Help>
!
!
!<Help KEYWORD="HEADER">
!<Tip> CPS header word for the values in each column on the file. </Tip>
! Default = the six values 7, 8, 37, 38, T, and V.
! Allowed = any valid header word (1 - NWIH) or the letter T or V.
!
! Letter should be T (upper/lower case) for the column containing trace times.
! Letter should be V (upper/lower case) for the column containing trace values.
!
! Columns containing trace times and trace values are both required if either
! one is specified.  In this case, several lines will be written to the file
! for each trace.  Each line will correspond to a different trace time as
! specified with the parameters TIME_INIT, TIME_INC, and TIME_LAST.
!
! If both trace times and trace values are omitted, then only one line will
! be written to the file for each trace, with the result that the file will
! be a trace header dump only.  In this case the parameters TIME_INIT,
! TIME_INC, and TIME_LAST are not used.
!</Help>
!
!
!<Help KEYWORD="WIDTH">
!<Tip> Width for each column on the file. </Tip>
! Default = 12
! Allowed = integer (>= 0).
!
! If this parameter is zero for any column, numbers in that column will take
! up as much column width as required, with the result that the columns will
! not be lined up vertically.
!
! If this parameter is greater than zero for any column, all the numbers
! in that column will be constrained to fit within the specified width if
! possible, by lowering the number of significant figures if necessary.
! It will be a fatal error if the column width is too narrow to do this.
!
! This parameter should be greater than zero for all columns if you want
! all columns to line up vertically.
!</Help>
!
!
!<Help KEYWORD="DECIMALS">
!<Tip> Maximum number of decimals for the values in each column. </Tip>
! Default = 0 for header words, 3 for time, and 5 for trace value.
! Allowed = integer (>= 0).
!
! No constraints are placed on the number of decimals if this parameter is
! nearly as large as, or larger than, the column width.
!</Help>
!
!
!<Help KEYWORD="MULTIPLIER">
!<Tip> Number to multiply each value by before writing it to the file. </Tip>
! Default = 1.0
! Allowed = any floating point number except zero.
!
! Each column has a separate multiplier.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module valuedump_module
      use pc_module
      use named_constants_module
      use pathchoose_module
      use pathcheck_module
      use floatio_module
      use pjar_module
      use mth_module
      use string_module

      implicit none
      private
      public :: valuedump_create
      public :: valuedump_initialize
      public :: valuedump_update
      public :: valuedump_delete
      public :: valuedump            ! main trace processing routine.
      public :: valuedump_wrapup

      character(len=100),public,save :: VALUEDUMP_IDENT = &
'$Id: valuedump.f90,v 1.3 2006/10/17 13:45:49 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: valuedump_struct

        private
        logical                         :: skip_wrapup    ! wrapup flag.

        integer                         :: nwih           ! global
        integer                         :: ndpt           ! global
        real                            :: tstrt          ! global
        real                            :: dt             ! global

        integer                         :: hdr_flag       ! process parameter
        character(len=FILENAME_LENGTH)  :: pathname       ! process parameter
        character(len=80)               :: pathname_info  ! process parameter
        real                            :: time_init      ! process parameter
        real                            :: time_inc       ! process parameter
        real                            :: time_last      ! process parameter
        logical                         :: self_defining  ! process parameter

        integer                         :: ncolumns       ! process parameter
        character(len=16)      ,pointer :: label     (:)  ! process parameter
        character(len=8)       ,pointer :: header    (:)  ! process parameter
        integer                ,pointer :: width     (:)  ! process parameter
        integer                ,pointer :: decimals  (:)  ! process parameter
        double precision       ,pointer :: multiplier(:)  ! process parameter

        integer                ,pointer :: hdrs      (:)  ! dependent parameter
        type(pathchoose_struct),pointer :: pathchoose     ! dependent parameter
        type(floatio_struct)   ,pointer :: floatio        ! dependent parameter
        integer                         :: itime_init     ! dependent parameter
        integer                         :: itime_inc      ! dependent parameter
        integer                         :: itime_last     ! dependent parameter
        integer                         :: itime_column   ! dependent parameter
        integer                         :: ivalue_column  ! dependent parameter

      end type valuedump_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                       ,save :: lunprint
      type(valuedump_struct),pointer,save :: object    ! needed for traps.

      character(len=9),parameter,private :: SECNAME  = 'valuedump'

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine valuedump_create (obj)
      type(valuedump_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()

      allocate (obj)

      nullify (obj%label     )
      nullify (obj%header    )
      nullify (obj%width     )
      nullify (obj%decimals  )
      nullify (obj%multiplier)
      nullify (obj%hdrs      )
      nullify (obj%floatio)
      nullify (obj%pathchoose) ! jpa

      call pathchoose_create (obj%pathchoose, 'pathname', '.valuedump')

      call valuedump_initialize (obj)
      end subroutine valuedump_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine valuedump_delete (obj)
      type(valuedump_struct),pointer :: obj       ! arguments

      call valuedump_wrapup (obj)

      if (associated(obj%label     )) deallocate (obj%label     )
      if (associated(obj%header    )) deallocate (obj%header    )
      if (associated(obj%width     )) deallocate (obj%width     )
      if (associated(obj%decimals  )) deallocate (obj%decimals  )
      if (associated(obj%multiplier)) deallocate (obj%multiplier)
      if (associated(obj%hdrs      )) deallocate (obj%hdrs      )

      call floatio_close     (obj%floatio)
      call pathchoose_delete (obj%pathchoose)

      deallocate(obj)
      end subroutine valuedump_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine valuedump_initialize (obj)
      type(valuedump_struct),intent(inout) :: obj              ! arguments
      real                                 :: tstop            ! local

      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)

      tstop = obj%tstrt + (obj%ndpt - 1) * obj%dt

      if (associated(obj%label     )) deallocate (obj%label     )
      if (associated(obj%header    )) deallocate (obj%header    )
      if (associated(obj%width     )) deallocate (obj%width     )
      if (associated(obj%decimals  )) deallocate (obj%decimals  )
      if (associated(obj%multiplier)) deallocate (obj%multiplier)

      obj%hdr_flag          = 0
      obj%pathname          = ' '
      obj%time_init         = obj%tstrt
      obj%time_inc          = 0.1
      obj%time_last         = tstop
      obj%self_defining     = .true.
      obj%ncolumns          = 6

      allocate (obj%label     (obj%ncolumns))
      allocate (obj%header    (obj%ncolumns))
      allocate (obj%width     (obj%ncolumns))
      allocate (obj%decimals  (obj%ncolumns))
      allocate (obj%multiplier(obj%ncolumns))

      obj%label     (:)     = ' '
      obj%header    (:)     = ' '
      obj%width     (:)     = 12
      obj%decimals  (:)     = 0
      obj%multiplier(:)     = 1.0

      obj%header    (1)     = '7'
      obj%header    (2)     = '8'
      obj%header    (3)     = '37'
      obj%header    (4)     = '38'
      obj%header    (5)     = 'T'
      obj%header    (6)     = 'V'

      obj%decimals  (1)     = 0
      obj%decimals  (2)     = 0
      obj%decimals  (3)     = 0
      obj%decimals  (4)     = 0
      obj%decimals  (5)     = 3
      obj%decimals  (6)     = 5

      call valuedump_update (obj)
      end subroutine valuedump_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine valuedump_update (obj)
      type(valuedump_struct),intent(inout),target :: obj          ! arguments
      integer                                     :: err,icolumn  ! local
      real                                        :: tstop        ! local
      integer                                     :: head         ! local
      character(len=80)                           :: msg          ! local
      type(pjar_struct),pointer                   :: pjar         ! local

      nullify (pjar) ! jpa
      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%pathchoose, obj%pathname)) return

      call pc_register_array_names ('label_arrayset', (/'label     ', &
                                                        'header    ', &
                                                        'width     ', &
                                                        'decimals  ', &
                                                        'multiplier'/))

      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)

      tstop = obj%tstrt + (obj%ndpt - 1) * obj%dt

      call pc_get ('HDR_FLAG         ', obj%hdr_flag)
      call pc_get ('PATHNAME         ', obj%pathname)
      call pc_get ('TIME_INIT        ', obj%time_init)
      call pc_get ('TIME_INC         ', obj%time_inc)
      call pc_get ('TIME_LAST        ', obj%time_last)
      call pc_get ('self_defining', obj%self_defining)

      call pc_alloc ('label'     , obj%label     , obj%ncolumns)
      call pc_alloc ('header'    , obj%header    , obj%ncolumns)
      call pc_alloc ('width'     , obj%width     , obj%ncolumns)
      call pc_alloc ('decimals'  , obj%decimals  , obj%ncolumns)
      call pc_alloc ('multiplier', obj%multiplier, obj%ncolumns)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call mth_constrain (obj%hdr_flag, 0, obj%nwih)

      call pathcheck ('pathname', obj%pathname, '.valuedump', &
                      required=.true., show=PATHCHECK_INFO_OUTPUT)

      call mth_constrain (obj%time_init, obj%tstrt, tstop, obj%dt)
      call mth_constrain (obj%time_inc, obj%dt, tstop - obj%time_init, obj%dt)
      call mth_constrain (obj%time_last, obj%time_init, tstop, obj%time_inc)

      obj%itime_column  = 0
      obj%ivalue_column = 0

      do icolumn = 1,obj%ncolumns
           if (obj%width(icolumn) == INIL) then
                obj%width(icolumn) = 0
           else
                call mth_constrain (obj%width(icolumn), 0, 30)
           end if

           if (obj%multiplier(icolumn) == DNIL .or. &
               obj%multiplier(icolumn) == 0.0) then
                obj%multiplier(icolumn) = 1.0
           end if

           if (obj%header(icolumn)(1:1) == 'T' .or. &
               obj%header(icolumn)(1:1) == 't') then
                obj%header(icolumn) = 'T'
                obj%itime_column = icolumn
           else if (obj%header(icolumn) == 'V' .or. &
               obj%header(icolumn)(1:1) == 'v') then
                obj%header(icolumn) = 'V'
                obj%ivalue_column = icolumn
           else
                head = string_ss2ii(obj%header(icolumn))
                call mth_constrain (head, 1, obj%nwih)
                obj%header(icolumn) = string_ii2ss(head)
           end if

           if (obj%label(icolumn)(1:3) == 'hdr' .or. &
               obj%label(icolumn) == ' ') then
                if (obj%header(icolumn) == 'T') then
                     obj%label(icolumn) = 'time'
                else if (obj%header(icolumn) == 'V') then
                     obj%label(icolumn) = 'value'
                else
                     obj%label(icolumn) = 'hdr'//obj%header(icolumn)
                end if
           end if

           if (obj%decimals(icolumn) == INIL) then
                if (obj%header(icolumn)(1:1) == 'T') then
                     obj%decimals(icolumn) = 3
                else if (obj%header(icolumn) == 'V') then
                     obj%decimals(icolumn) = 5
                else
                     obj%decimals(icolumn) = 0
                end if
           else
                call mth_constrain (obj%decimals(icolumn), 0, 99)
           end if
      end do
      
      if (pc_verify_end()) then
           if (obj%itime_column == 0 .and. obj%ivalue_column == 0) then
                   call pc_print ("file will be a trace header dump only")
                   call pc_print ("(since columns for trace times and samples&
                                  & are not requested)")
           else if (obj%itime_column > 0 .and. obj%ivalue_column > 0) then
                   call pc_print ("file will contain trace times and samples")
           else
                   call pc_error ("columns for trace times and samples&
                                  & must both be present or both be absent")
           end if
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put ('HDR_FLAG         ', obj%hdr_flag)
      call pc_put ('PATHNAME         ', obj%pathname)
      call pc_put ('TIME_INIT        ', obj%time_init)
      call pc_put ('TIME_INC         ', obj%time_inc)
      call pc_put ('TIME_LAST        ', obj%time_last)
      call pc_put ('self_defining    ', obj%self_defining)

      call pc_put_gui_only ('tstrt   ', obj%tstrt)
      call pc_put_gui_only ('dt      ', obj%dt)
      call pc_put_gui_only ('tstop   ', tstop)

      call pc_put ('label'     , obj%label     , obj%ncolumns)
      call pc_put ('header'    , obj%header    , obj%ncolumns)
      call pc_put ('width'     , obj%width     , obj%ncolumns)
      call pc_put ('decimals'  , obj%decimals  , obj%ncolumns)
      call pc_put ('multiplier', obj%multiplier, obj%ncolumns)

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .false.)
      call pc_put_control ('need_label'   , .false.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)
      call pc_put_control ('parallel_safe', .false.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%hdrs)) deallocate (obj%hdrs)

      call floatio_close (obj%floatio)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%itime_init = 1 + nint((obj%time_init - obj%tstrt) / obj%dt)
      obj%itime_inc  =     nint( obj%time_inc               / obj%dt)
      obj%itime_last = 1 + nint((obj%time_last - obj%tstrt) / obj%dt)

      allocate (obj%hdrs(obj%ncolumns))

      do icolumn = 1,obj%ncolumns
           if (obj%header(icolumn) == 'T') then
                obj%hdrs(icolumn) = 0
           else if (obj%header(icolumn) == 'V') then
                obj%hdrs(icolumn) = 0
           else
                obj%hdrs(icolumn) = string_ss2ii(obj%header(icolumn))
           end if
      end do

      call pjar_create         (pjar)
      call pjar_choose_section (pjar, SECNAME)

      call pjar_put (pjar, 'encoding'   , FIO_ASCII)
      call pjar_put (pjar, 'ncolumns'   , obj%ncolumns)
      call pjar_put (pjar, 'fields'     , obj%label      , obj%ncolumns)
      call pjar_put (pjar, 'hdrs'       , obj%hdrs       , obj%ncolumns)
      call pjar_put (pjar, 'widths'     , obj%width      , obj%ncolumns)
      call pjar_put (pjar, 'decimals'   , obj%decimals   , obj%ncolumns)
      call pjar_put (pjar, 'noheaders'  , .not.obj%self_defining)

      call floatio_open_write  (obj%floatio,obj%pathname,pjar,SECNAME,err,msg)
      call pjar_delete         (pjar)

      if (err == FLOATIO_ERROR) call pc_error (msg)

      if (obj%self_defining) then
        call pc_print ("file will contain a self-defining header section")
      else
        call pc_print ("file will not contain a self-defining header section")
      end if

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine valuedump_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine valuedump (obj,ntr,hd,tr)
      type(valuedump_struct),intent(inout) :: obj                 ! arguments
      integer               ,intent(inout) :: ntr                 ! arguments
      double precision      ,intent(inout) :: hd(:,:)             ! arguments
      real                  ,intent(inout) :: tr(:,:)             ! arguments
      integer                              :: err,itr,indx        ! local
      character(len=80)                    :: msg                 ! local
      double precision                     :: vline(obj%ncolumns) ! local
      integer                              :: icolumn,ihdr        ! local

      do itr = 1,ntr
           if (obj%hdr_flag > 0) then
                if (hd(obj%hdr_flag,itr) == 0.0) cycle
           end if

           do icolumn = 1,obj%ncolumns
                ihdr = obj%hdrs(icolumn)
                if (ihdr > 0) then
                     vline(icolumn) = hd(ihdr,itr) * obj%multiplier(icolumn)
                else
                     vline(icolumn) = 0.0   ! will be replaced.
                end if
           end do

           if (obj%itime_column == 0 .or. obj%ivalue_column == 0) then
                call floatio_write_line (obj%floatio, err, msg, vline)
                if (err == FLOATIO_ERROR) then
                     write(lunprint,*) 'VALUEDUMP: ',trim(msg)
                     call valuedump_wrapup (obj)
                     ntr = FATAL_ERROR
                     return
                end if
                cycle
           end if

           do indx = obj%itime_init,obj%itime_last,obj%itime_inc

                vline(obj%itime_column ) = (obj%tstrt + (indx-1) * obj%dt) &
                                            * obj%multiplier(obj%itime_column)
                vline(obj%ivalue_column) = tr(indx,itr) &
                                            * obj%multiplier(obj%ivalue_column)

                call floatio_write_line (obj%floatio, err, msg, vline)
                if (err == FLOATIO_ERROR) then
                     write(lunprint,*) 'VALUEDUMP: ',trim(msg)
                     call valuedump_wrapup (obj)
                     ntr = FATAL_ERROR
                     return
                end if
           end do
      end do

      end subroutine valuedump


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine valuedump_wrapup (obj)
      type(valuedump_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call floatio_close (obj%floatio)

      end subroutine valuedump_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module valuedump_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

