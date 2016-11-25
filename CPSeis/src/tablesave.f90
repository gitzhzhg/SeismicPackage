!<CPS_v1 type="PROCESS"/>
!!------------------------- tablesave.f90 ---------------------------------!!
!!------------------------- tablesave.f90 ---------------------------------!!
!!------------------------- tablesave.f90 ---------------------------------!!


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
! Name       : TABLESAVE                 (trace file table save)
! Category   : sorts
! Written    : 2001-12-28   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Create a trace file table for a trace file output by TROT.
! Portability: No known limitations.
! Parallel   : No.
!
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This process writes to disk a table of traces placed on disk by the
! TROT process.  This process should immediately precede or follow the TROT
! process which saves a TRCIO file for which this table is wanted.  The file
! name specified to this process should be the same as the name provided
! to the preceding or following TROT process to clarify the relationship
! between the TRCIO file and this corresponding table file.  The extension
! of this table file will be ".table".
!
! If the preceding TROT process outputs only flagged traces, the user must
! provide the same flag header word to this process so that this table
! will correspond to the traces actually output by TROT.  A do-skip pattern
! must not be specified in TROT.
!
! The table output by this process can be used later to read the associated
! TRCIO file in any of several desired sort orders, reducing or eliminating
! the need to sort the data using TSORT or other comparable method.  The
! table can itself be sorted to the desired order either by the TRINSORT
! process which will read in the trace data, or by the TABLESORT setup-only
! process prior to reading the trace data.
!
! Any of several user-specified header words can be used to choose the
! order in which the trace data can be read in a future processing job.
! These header words must be specified in this process.  The default header
! words are 6, 7, 8, 9, and 10.
!
!-------------------------------------------------------------------------------
!                         TRIO OF PROCESSES
!
! This process is one of a trio of processes designed to allow reading trace
! files randomly in order to reduce the need to sort data using TSORT:
!
! TABLESAVE:  Saves a trace file table to disk.  This process should
!             immediately precede or follow TROT in the job.  TROT will
!             save the trace file as usual.
!
! TABLESORT:  Reads, sorts, and saves a trace file table in a different
!             order (setup-only process).  This is optional, since TRINSORT
!             (below) can also sort (but not output) the table.
!
! TRINSORT:   Reads a trace file in desired sort order using the trace file
!             table.  This process should be used instead of TRIN to read
!             the trace file.  TRINSORT can read traces in the order
!             specified on the table, or in a user-specified order.
!
! If you wish to create a trace file table for an existing TROT file, you
! can read the traces using TRIN followed by TABLESAVE.
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
! This process allows traces to be input one at a time or in gathers.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process outputs the same traces as it receives (without any changes).
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                         Action taken
! ----      -----------                         ------------
! NWIH      Number of words in header           used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#            Description                    Action taken
! ----            -----------                    ------------
! HDRS(list)      Default:  6,7,8,9,10           Used but not changed.
! HDR_FLAG        User-defined header word       Used but not changed.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!002. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  1. 2002-02-04  Stoeckley  Initial version.
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
!<NS TABLESAVE Process/NC=80>
!                    Write trace file table to disk
!
!                            HDR_FLAG=~~~`II
!
!
! Select TABLEPATH[TABLEPATH]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [TABLEPATH_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!
!                               HDRS
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!                               `IIIIIII
!
!<PARMS TABLEPATH[/ML=128/XST]>
!<PARMS HDRS[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="TABLEPATH_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of TABLEPATH. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_TABLEPATH">
!<Tip> Choose TABLEPATH using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> User defined header word containing a flag. </Tip>
! Default = 0      
! Allowed = 48-55 or 65-NWIH or 0
!
! This header word must be the same as that specified to the preceding or
! following TROT process which saves the traces to the file to be associated
! with this table.
!</Help>
!
!
!<Help KEYWORD="TABLEPATH">
!<Tip> Name of trace file table to create. </Tip>
! Default = NONE
! Allowed = char
!
! This file name should be the same as that name provided to the preceding
! or following TROT process which saves the traces to the file to be associated
! with this table.  The preceding TROT process should save traces to a TRCIO
! file.
!
! Any specified file extension will be changed to ".table".
!</Help>
!
!
!<Help KEYWORD="HDRS">
!<Tip> Header words which can be used for future sorts. </Tip>
! Default = 6, 7, 8, 9, 10.
! Allowed = 2-NWIH
!
! This list of header words should include at least the default values.
! Future trace sorts will be limited to the header words specified here.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module tablesave_module
      use pc_module
      use named_constants_module
!<execute_only>
      use tftable_module
!</execute_only>
      use pathcheck_module
      use pathchoose_module
      use mem_module
      implicit none
      private
      public :: tablesave_create
      public :: tablesave_initialize
      public :: tablesave_update
      public :: tablesave_delete
!<execute_only>
      public :: tablesave
      public :: tablesave_wrapup
!</execute_only>


      character(len=100),public,save :: TABLESAVE_IDENT = &
'$Id: tablesave.f90,v 1.2 2006/10/17 13:45:48 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: tablesave_struct              
 
        private
        logical                         :: skip_wrapup  ! wrapup flag.

        integer                         :: hdr_flag     ! process parameters.
        character(len=FILENAME_LENGTH)  :: tablepath    ! process parameters.
        integer                ,pointer :: hdrs(:)      ! process parameters.
        integer                         :: nhdrs        ! process parameters.

        type(pathchoose_struct),pointer :: pathchoose   ! dependent.
!<execute_only>
        type(tftable_struct)   ,pointer :: tftable      ! dependent.
!</execute_only>

      end type tablesave_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(tablesave_struct),pointer,save :: object      ! needed for traps.

      integer,parameter :: nheaders          = 5
      integer,parameter :: headers(nheaders) = (/6,7,8,9,10/)

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine tablesave_create (obj)
      implicit none
      type(tablesave_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%hdrs)
!<execute_only>
      nullify (obj%tftable)
!</execute_only>
      nullify (obj%pathchoose) ! jpa

      call pathchoose_create (obj%pathchoose, 'tablepath', 'table')

      call tablesave_initialize (obj)
      return
      end subroutine tablesave_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine tablesave_delete (obj)
      implicit none
      type(tablesave_struct),pointer :: obj       ! arguments

      call mem_free (obj%hdrs)
!<execute_only>
      call tablesave_wrapup (obj)
      call tftable_close    (obj%tftable)
!</execute_only>

      call pathchoose_delete (obj%pathchoose)

      deallocate(obj)
      return
      end subroutine tablesave_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine tablesave_initialize (obj)
      implicit none
      type(tablesave_struct),intent(inout) :: obj       ! arguments

      obj%nhdrs     = 5
      obj%hdr_flag  = 0
      obj%tablepath = PATHCHECK_EMPTY
      call mem_alloc (obj%hdrs,obj%nhdrs)
      obj%hdrs(:)   = (/6,7,8,9,10/)

      call tablesave_update (obj)
      return
      end subroutine tablesave_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine tablesave_update (obj)
      implicit none
      type(tablesave_struct),intent(inout),target :: obj          ! arguments
      integer                                       :: nwih,nhist   ! local
      integer                                       :: i,j          ! local
      character(len=80)                             :: msg          ! local
      character(len=80),pointer                     :: hist(:)      ! local

      nullify (hist) ! jpa
      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if (pathchoose_update(obj%pathchoose, obj%tablepath)) return

      call pc_get_global ('nwih', nwih)

      call pc_get   ('hdr_flag'  , obj%hdr_flag)
      call pc_get   ('tablepath' , obj%tablepath)
      call pc_alloc ('hdrs'      , obj%hdrs, obj%nhdrs)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%hdr_flag == 0) then
           continue
      else if (obj%hdr_flag >= 48 .and. obj%hdr_flag <= 55) then
           continue
      else if (obj%hdr_flag >= 65 .and. obj%hdr_flag <= nwih) then
           continue
      else
          call pc_warning &
                   ('HDR_FLAG must be 0 or 48-55 or 65-NWIH  --  reset to 0')
          obj%hdr_flag = 0
      end if

      call pathcheck ('tablepath', obj%tablepath, ext='.table', &
                      required=.true., show=PATHCHECK_INFO_OUTPUT)

      if (pc_verify_array('hdrs')) then

           if (obj%nhdrs == 0) then
               call pc_error ('you must specify trace sort header words HDRS')
           end if

           do i = 1,obj%nhdrs
                if (obj%hdrs(i) <= 1 .or. obj%hdrs(i) > nwih) then
                    call pc_error &
                     ('trace sort header words HDRS must be between 2 and NWIH')
                end if
           end do

           do i =  1 ,obj%nhdrs
                do j = i+1,obj%nhdrs
                     if (obj%hdrs(i) == obj%hdrs(j)) then
                         call pc_error &
                          ('duplicate trace sort header words HDRS specified')
                     end if
                end do
           end do

      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put  ('hdr_flag'  , obj%hdr_flag)
      call pc_put  ('tablepath' , obj%tablepath)
      call pc_put  ('hdrs'      , obj%hdrs, obj%nhdrs)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>


      call tftable_close (obj%tftable)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call pc_alloc_process_cards (hist,nhist)

      call tftable_open_write (obj%tftable,obj%tablepath,msg,'TABLESAVE',  &
                               obj%hdrs,obj%nhdrs,hist,nhist)

      if (msg /= ' ') call pc_error (msg)


!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine tablesave_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>


      subroutine tablesave (obj,ntr,hd,tr)
      implicit none
      type(tablesave_struct),intent(inout) :: obj              ! arguments
      integer                 ,intent(inout) :: ntr              ! arguments
      double precision        ,intent(in)    :: hd(:,:)          ! arguments
      real                    ,intent(in)    :: tr(:,:)          ! arguments
      integer                                :: i,ntr2           ! local
      character(len=80)                      :: msg              ! local

      ntr2 = ntr
      do i = 1,ntr2
           if (obj%hdr_flag > 0) then
                 if (hd(obj%hdr_flag,i) == 0) cycle
           end if
           call tftable_add_trace (obj%tftable,hd(:,i),msg)
           if (msg /= ' ') then
                call pc_error (msg)
                ntr = FATAL_ERROR
                exit
           end if
      end do

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call tablesave_wrapup (obj)
      end if
      return
      end subroutine tablesave

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine tablesave_wrapup (obj)
      implicit none
      type(tablesave_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call tftable_close (obj%tftable)
      return
      end subroutine tablesave_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module tablesave_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

