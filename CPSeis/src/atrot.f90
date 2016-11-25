!<CPS_v1 type="PROCESS"/>
!!------------------------------- atrot.f90 ---------------------------------!!
!!------------------------------- atrot.f90 ---------------------------------!!
!!------------------------------- atrot.f90 ---------------------------------!!


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
! Name       : ATROT                     (Ascii Trace Output)
! Category   : io
! Written    : 2004-06-08   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Write traces to an ascii file.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Write traces to an ascii file.
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
! Input traces should be gathered, unless you want only one trace on the file.
! Only the first gather will be output to the file.
!
! The output file will contain one column for each trace in the gather, plus
! a column (the first column) containing the trace sample times in seconds.
!
! The output file will not contain any header words.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process does not change the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
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
!
! No header words are changed by this process.
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
!  2. 2005-02-21  Stoeckley  Add parameters NOHEADERS and NILSTRING.
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
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
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
!<NS ATROT Process/NC=80>
!                           Ascii Trace Output
!
! NOHEADERS=`KKK             [/L]Whether to inhibit writing header info to file.
! NILSTRING=`SSSSSSSSSSSSSS  [/L]Symbol for nil value on file.
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
!<Help KEYWORD="PATHNAME">
!<Tip>Name of output file containing ascii traces.</Tip>
! Default = NONE
! Allowed = valid file name.
!
! Only the first trace gather will be output to the file.
!
! The output file will contain one column for each trace in the gather, plus
! a column (the first column) containing the trace sample times in seconds.
!
! The output file will not contain any header words.
!</Help>
!
!
!<Help KEYWORD="NOHEADERS">
!<Tip>Whether to inhibit writing header info to the file.</Tip>
! Default = no
! Allowed = yes or no
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
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module atrot_module
      use pc_module
      use named_constants_module
      use pathchoose_module
      use pathcheck_module
      use floatio_module

      implicit none
      private
      public :: atrot_create
      public :: atrot_initialize
      public :: atrot_update
      public :: atrot_delete
      public :: atrot            ! main trace processing routine.
      public :: atrot_wrapup

      character(len=100),public,save :: ATROT_IDENT = &
'$Id: atrot.f90,v 1.3 2006/09/18 13:32:37 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: atrot_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).

        logical                        :: noheaders       ! process parameter
        character(len=20)              :: nilstring       ! process parameter
        character(len=FILENAME_LENGTH) :: pathname        ! process parameter

        logical                         :: finished       ! dependent
        type(pathchoose_struct),pointer :: pathchoose     ! dependent

      end type atrot_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                   ,save :: lunprint  ! unit number for printing.
      type(atrot_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine atrot_create (obj)
      type(atrot_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()
      allocate (obj)
      nullify (obj%pathchoose) ! jpa

      call pathchoose_create (obj%pathchoose, 'PATHNAME', '*')
      call atrot_initialize  (obj)
      end subroutine atrot_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine atrot_delete (obj)
      type(atrot_struct),pointer :: obj       ! arguments

      call atrot_wrapup      (obj)
      call pathchoose_delete (obj%pathchoose)

      deallocate(obj)
      end subroutine atrot_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine atrot_initialize (obj)
      type(atrot_struct),intent(inout) :: obj       ! arguments

      obj%noheaders = .false.
      obj%nilstring = 'nil'
      obj%pathname  = PATHCHECK_EMPTY

      call atrot_update (obj)
      end subroutine atrot_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine atrot_update (obj)
      type(atrot_struct),intent(inout),target :: obj             ! arguments

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

      call pc_get ('noheaders        ', obj%noheaders)
      call pc_get ('nilstring        ', obj%nilstring)
      call pc_get ('PATHNAME         ', obj%pathname)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%nilstring == ' ') obj%nilstring = 'nil'

      call pathcheck ('PATHNAME', obj%pathname, required=.true.,  &
                      show=PATHCHECK_INFO_OUTPUT)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put ('noheaders        ', obj%noheaders)
      call pc_put ('nilstring        ', obj%nilstring)
      call pc_put ('PATHNAME         ', obj%pathname)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%finished = .false.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine atrot_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine atrot (obj,ntr,hd,tr)
      type(atrot_struct),intent(inout) :: obj                    ! arguments
      integer           ,intent(inout) :: ntr                    ! arguments
      double precision  ,intent(inout) :: hd(:,:)                ! arguments
      real              ,intent(inout) :: tr(:,:)                ! arguments
      type(floatio_struct),pointer     :: floatio                ! local
      integer                          :: err,indx,itr           ! local
      character(len=80)                :: msg                    ! local
      real                             :: vline(ntr)             ! local

      nullify (floatio) ! jpa

      if (ntr == NO_MORE_TRACES) then
           call atrot_wrapup (obj)
           return
      endif

      if (obj%finished) then
           call atrot_wrapup (obj)
           return
      endif

      call floatio_easy_write (floatio,obj%pathname,ntr+1,err,msg, &
                               obj%noheaders,obj%nilstring)

      if (err /= FLOATIO_OK) then
           call floatio_close (floatio)
           call pc_error ('ATROT: error opening',obj%pathname)
           call pc_error ('ATROT:',msg)
           call atrot_wrapup (obj)
           ntr = FATAL_ERROR
           return
      endif

      do indx = 1,obj%ndpt

           vline(1) = obj%tstrt + (indx - 1) * obj%dt

           do itr = 1,ntr
                vline(itr + 1) = tr(indx,itr)
           enddo

           call floatio_write_line  (floatio,err,msg,vline)

           if (err == FLOATIO_ERROR) then
                call floatio_close (floatio)
                call pc_error ('ATROT: error writing',obj%pathname)
                call pc_error ('ATROT:',msg)
                call atrot_wrapup (obj)
                ntr = FATAL_ERROR
                return
           endif

      enddo

      call floatio_close (floatio)

      obj%finished = .true.

      end subroutine atrot


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine atrot_wrapup (obj)
      type(atrot_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine atrot_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module atrot_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

