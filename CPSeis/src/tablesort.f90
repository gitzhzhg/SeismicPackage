!<CPS_v1 type="PROCESS"/>
!!--------------------------- tablesort.f90 ---------------------------------!!
!!--------------------------- tablesort.f90 ---------------------------------!!
!!--------------------------- tablesort.f90 ---------------------------------!!


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
! Name       : TABLESORT                 (trace file table sort)
! Category   : sorts
! Written    : 2001-12-28   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Read, sort, and save a trace file table in desired order.
! Portability: No known limitations, but see note regarding pgf90 bug below.
! Parallel   : No.
!
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This process reads a trace file table from disk, sorts it into a
! user-specified order, and saves it back to disk under a different name.
! The original trace file table is not changed.
!
! You must have previously saved the trace file table using the TABLESAVE
! process at the time you saved the trace file itself using the TROT process.
! The trace file saved by TROT must be a TRCIO file.
!
! Any of the header words which were saved on the table can be used to choose
! the order in which the trace data is to be read in this or another processing
! job.
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
! Not applicable (this is a setup-only process).
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! Not applicable (this is a setup-only process).
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                         Action taken
! ----      -----------                         ------------
! none
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Not applicable (this is a setup-only process).
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!005. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!004. 2006-01-10  B. Menger  Removed Unused Variables.
!  3. 2005-03-07  Stoeckley  Set NUMTR so that ICPS will work when this
!                             setup process is the only process in the job.
!  2. 2003-12-09  Stoeckley  Change real to double precision.
!  1. 2002-02-04  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS           
!
! No known limitations, but I had to work around a Portland Group compiler
! bug in the verify parameters section, where you can see a comment.
! The Portland Group compiler claims the following:
!   PGF90-S-0099-Illegal use of derived type (../tablesort.f90: 460)
!   PGF90-S-0099-Illegal use of derived type (../tablesort.f90: 460)
! for a test for inequality between two triplesort_ints variables.  The
! compiler seems not to be able to deal with some overloaded operator
! interfaces in the triplesort_module.  The compiler will not complain if
! any of several unrelated parts of the code in this module are commented
! out.  A small test program also does not complain.
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
! SETUP_ONLY     true      whether this process is setup-only.    
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
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
!<NS TABLESORT Process/NC=80>
!                  Sort trace file table to desired order
!
!
! Select TABLEPATH_IN [TABLEPATH_IN]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                      [TABLEPATH_IN_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                      [INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! Select TABLEPATH_OUT[TABLEPATH_OUT]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                      [TABLEPATH_OUT_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!
!<include tsortparams.f90>
!
!<PARMS TABLEPATH_IN [/ML=128/XST]>
!<PARMS TABLEPATH_OUT[/ML=128/XST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="TABLEPATH_IN_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of TABLEPATH_IN. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_TABLEPATH_IN">
!<Tip> Choose TABLEPATH_IN using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Information regarding contents of TABLEPATH_IN. </Tip>
!</Help>
!
!
!<Help KEYWORD="TABLEPATH_OUT_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of TABLEPATH_OUT. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_TABLEPATH_OUT">
!<Tip> Choose TABLEPATH_OUT using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="TABLEPATH_IN">
!<Tip> Name of input trace file table to read. </Tip>
! Default = NONE
! Allowed = char
!
! Any specified file extension will be changed to ".table".
!</Help>
!
!
!<Help KEYWORD="TABLEPATH_OUT">
!<Tip> Name of output trace file table to save. </Tip>
! Default = NONE
! Allowed = char
!
! Any specified file extension will be changed to ".table".
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module tablesort_module
      use pc_module
      use named_constants_module
      use tftable_module
      use pathcheck_module
      use pathchoose_module
      use tsortparams_module
      use triplesort_module
      implicit none
      private
      public :: tablesort_create
      public :: tablesort_initialize
      public :: tablesort_update
      public :: tablesort_delete
      public :: tablesort_wrapup


      character(len=100),public,save :: TABLESORT_IDENT = &
'$Id: tablesort.f90,v 1.5 2006/10/17 13:45:48 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: tablesort_struct              
 
        private
        logical                         :: skip_wrapup    ! wrapup flag.

        character(len=FILENAME_LENGTH)  :: tablepath_in   ! process parameters.
        character(len=FILENAME_LENGTH)  :: tablepath_out  ! process parameters.
        character(len=32)               :: opt_sort       ! process parameters.
        type(triplesort_ints)           :: hdr            ! process parameters.
        type(triplesort_doubles)        :: init           ! process parameters.
        type(triplesort_doubles)        :: inc            ! process parameters.
        character(len=60)               :: info           ! gui-only parameter.

        type(pathchoose_struct) ,pointer :: pathchoose1   ! dependent.
        type(pathchoose_struct) ,pointer :: pathchoose2   ! dependent.
        type(tsortparams_struct),pointer :: tsortparams   ! dependent.

      end type tablesort_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(tablesort_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine tablesort_create (obj)
      implicit none
      type(tablesort_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%pathchoose1) ! jpa
      nullify (obj%pathchoose2) ! jpa
      nullify (obj%tsortparams) ! jpa

      call pathchoose_create    (obj%pathchoose1, 'tablepath_in' , 'table')
      call pathchoose_create    (obj%pathchoose2, 'tablepath_out', 'table')
      call tsortparams_create   (obj%tsortparams)
      call tablesort_initialize (obj)
      return
      end subroutine tablesort_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine tablesort_delete (obj)
      implicit none
      type(tablesort_struct),pointer :: obj       ! arguments

      call tablesort_wrapup   (obj)
      call pathchoose_delete  (obj%pathchoose1)
      call pathchoose_delete  (obj%pathchoose2)
      call tsortparams_delete (obj%tsortparams)

      deallocate(obj)
      return
      end subroutine tablesort_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine tablesort_initialize (obj)
      implicit none
      type(tablesort_struct),intent(inout) :: obj       ! arguments

      obj%tablepath_in     = PATHCHECK_EMPTY
      obj%tablepath_out    = PATHCHECK_EMPTY
      obj%info             = ' '

      call tsortparams_initialize (obj%tsortparams)
      call tablesort_update       (obj)
      return
      end subroutine tablesort_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine tablesort_update (obj)
      implicit none
      type(tablesort_struct),intent(inout),target :: obj            ! arguments
      integer                                     ::         nhist ! local
      character(len=80)                           :: msg            ! local
      character(len=FILENAME_LENGTH)              :: tablekeep_in   ! local
      character(len=80),pointer                   :: hist(:)        ! local
      type(triplesort_ints)                       :: keep,hdr       ! local
      type(triplesort_doubles)                    :: init,inc       ! local

      nullify (hist) ! jpa
      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      tablekeep_in = obj%tablepath_in

      if (pathchoose_update(obj%pathchoose1, obj%tablepath_in )) return
      if (pathchoose_update(obj%pathchoose2, obj%tablepath_out)) return

      call pc_get ('tablepath_in'  , obj%tablepath_in    )
      call pc_get ('tablepath_out' , obj%tablepath_out   )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call pathcheck ('tablepath_in', obj%tablepath_in, '.table',  &
                      required=.true., show=PATHCHECK_INFO_INPUT)

      call pathcheck ('tablepath_out', obj%tablepath_out, '.table',  &
                      required=.true., show=PATHCHECK_INFO_OUTPUT)

      keep = tsortparams_get_hdr (obj%tsortparams)

      call tsortparams_update (obj%tsortparams)

      hdr  = tsortparams_get_hdr (obj%tsortparams)

 !!!  if (obj%tablepath_in /= tablekeep_in .or. hdr /= keep) then  ! pgf90 bug.

      if (obj%tablepath_in /= tablekeep_in   .or. &     ! pgf90 bug workaround.
          hdr%primary      /= keep%primary   .or. &     ! pgf90 bug workaround.
          hdr%secondary    /= keep%secondary .or. &     ! pgf90 bug workaround.
          hdr%tertiary     /= keep%tertiary) then       ! pgf90 bug workaround.

           call tftable_checkout (obj%tablepath_in,msg,obj%info,hdr)
           call pc_print (obj%info)
           if (msg /= ' ') call pc_error (msg)
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_global   ('numtr', 1)     ! needed for icps to work.

      call pc_put          ('tablepath_in'  , obj%tablepath_in    )
      call pc_put          ('tablepath_out' , obj%tablepath_out   )
      call pc_put_gui_only ('info'          , obj%info            )

      call pc_put_control  ('setup_only' , .true.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call pc_alloc_process_cards (hist,nhist)

      hdr  = tsortparams_get_hdr  (obj%tsortparams)
      init = tsortparams_get_init (obj%tsortparams)
      inc  = tsortparams_get_inc  (obj%tsortparams)

      call tftable_copy_sort (obj%tablepath_in,obj%tablepath_out,msg,      &
                              'TABLESORT',                                 &
                              hdr,init,inc,hist,nhist)

      deallocate (hist)

      if (msg /= ' ') call pc_error (msg)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine tablesort_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!



!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine tablesort_wrapup (obj)
      implicit none
      type(tablesort_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine tablesort_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module tablesort_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

