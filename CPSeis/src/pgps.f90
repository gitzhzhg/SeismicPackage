!<CPS_v1 type="PROCESS"/>
!!------------------------------- pgps.f90 ---------------------------------!!
!!------------------------------- pgps.f90 ---------------------------------!!
!!------------------------------- pgps.f90 ---------------------------------!!

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
! Name       : PGPS
! Category   : io
! Written    : 2002-06-28   by: Karen Goodger
! Revised    : 2007-07-10   by: Bill Menger
! Maturity   : beta
! Purpose    : Prepare grid files for permanent save.
! Portability: No known limitations. 
! Parallel   : No. 
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! PGPS reads grid files containing velocity models and converts them to trcio
! files.  The resulting output files will have the text "_vmod" appended to
! the file name, and the output file extension will be ".trc".
! Multiple _vmod.trc files can be input into TRIN.  TRIN checks the input
! file names for the string "_vmod.trc" to determine if this is a velocity
! model permsave job.  If all files names have this string, TRIN will set
! header word 31 to a 1 for the first trace in every file.  TTROT will use
! this header word to determine the separation between files, and will write
! an eof between each vmod file.  TRIN also writes a file called 
! %trin_filenames_host_pid, which contains the names of the input files.  This file
! is saved along with the files specified in the TTROT PATH_PS parameter.
! TTRIN uses the %trin_filenames_host_pid file to know how to name the vmod files
! upon restore.
! (host and pid above will vary with each job).
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  It is best if the grid and hgrid files to be saved are in a directory 
!  separate from other files.  
!
!                     To permanent save velocity model files
!  1) Use PGPS to Prepare Grid files for Permanent Save.  The output will be
!     a .trc file and a .modgrid file.
!  2) Use permsave to prepare the list of hgrid and modgrid files.  There 
!     should be nothing in the list but hgrid and modgrid files.
!  3) Build a TRIN/TTROT job.  Input to TRIN will be the _vmod files from
!     step 1.  In TTROT, set the TYPE_OUT parameter to PS_VMOD.  Fill in
!     PATH_PS with the list from step 2.
!
!                     To restore velocity model files
!  1) Build a job with TTRIN only.  Fill in PATH_VMOD.  The vmod, hgrid, and
!     modgrid files will be written to this directory.  It must be answered.
!                   
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       
! GATHERED  whether traces are a legitimate gather  
! NWIH      number of words in trace header        
! NDPT      number of sample values in trace        
! TSTRT     starting time on trace                 
! DT        trace sample interval               
! GRID      grid transformation structure      
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
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
!  7. 2007-07-10  Bill Menger    Changed documentation to cover new file names.
!  6. 2006-09-11  Stoeckley      Repl pc_register_tab_group w HelpSection line.
!  5. 2006-08-24  D. Glover      Added NULLIFY statements for Intel compiler.
!  4. 2006-06-20  Stoeckley      Add pc_register_tab_group for SeisSpace.
!  3. 2005-04-11  Karen Goodger  Set numtr global to make the parameter
!                                cache happy.
!  2. 2002-08-12  Karen Goodger  Add more documentation.
!  1. 2002-06-28  Karen Goodger  Initial version.
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
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
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
!<NS PGPS Process/NC=80>
!
!               Prepare Grid files for Permanent Save
!
!                       PGPS File Select
!
!<include mfile.f90>
!                     
!<NS PGPS Parameters/NC=80>
!
! PATH_OUTFILES=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! SORT_ORDER=`SS
!
! X1=`IIIIIIIII              Y1=`IIIIIIIII
! X2=`IIIIIIIII              Y2=`IIIIIIIII
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!    tabgroup = PGPS Parameters
!
!<Help KEYWORD="PATH_OUTFILES">
!<Tip> Path name for output trcio files</Tip>
! Default = SAME
! Allowed = SAME or Valid path
! If PATH_OUTFILES=SAME, the output file will be put in the same directory as
! its corresponding input file.  If a path is given, all output files will
! be put in the directory indicated by path.
!</Help>
!
!<Help KEYWORD="SORT_ORDER">
!<Tip> The output order of the resulting trcio files</Tip>
! Default = ZXY
! Allowed = Any combination of Z, X, and Y.
! Z value is depth, X value is XGRID, and Y value is YGRID.
!</Help>
!
!<Help KEYWORD="X1">
!<Tip> First value to accept along the X-axis</Tip>
! Default = 1
! Allowed = Integer
!</Help>
!
!<Help KEYWORD="X2">
!<Tip> Last value to accept along the Y-axis</Tip>
! Default = 2**30
! Allowed = Integer
!</Help>
!
!<Help KEYWORD="Y1">
!<Tip> First value to accept along the Y-axis</Tip>
! Default = 1
! Allowed = Integer
!</Help>
!
!<Help KEYWORD="Y2">
!<Tip> Last value to accept along the Y-axis</Tip>
! Default = 2**30
! Allowed = Integer
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module pgps_module

      use getlun_module
      use mfile_module
      use modgrid_module
      use named_constants_module
      use pathcheck_module       
      use pc_module


      implicit none
      private
      public :: pgps_create
      public :: pgps_initialize
      public :: pgps_update
      public :: pgps_delete
      public :: pgps_wrapup
  character(len=100),public,save :: pgps_ident = &
  '$Id: pgps.f90,v 1.7 2007/07/11 14:07:23 Menger beta sps $'



!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type, public                 :: pgps_struct
        private
        logical                    :: skip_wrapup

        character(len=FILENAME_LENGTH)          :: path_outfiles
        character(len=3)           :: sort_order
        integer                    :: x1
        integer                    :: x2
        integer                    :: y1
        integer                    :: y2
        character(len=FILENAME_LENGTH), dimension(:), pointer :: filenames
        integer                                               :: num_files

    type(mfile_struct),pointer :: mfile


      end type pgps_struct



!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(pgps_struct),pointer,save :: object      ! needed for traps.

      integer :: stdo

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine pgps_create (obj)
      implicit none
      type(pgps_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%filenames) ! jpa

      call mfile_create  (obj%mfile  )
      call   mfile_set_type(obj%mfile,MFILE_READ_ANY_FILE)
      call pgps_initialize (obj)
      return
      end subroutine pgps_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine pgps_delete (obj)
      implicit none
      type(pgps_struct),pointer :: obj       ! arguments

      call   mfile_delete    (obj%mfile  )
      call pgps_wrapup (obj)


      deallocate(obj)
      return
      end subroutine pgps_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine pgps_initialize (obj)
      implicit none
      type(pgps_struct),intent(inout) :: obj       


      obj%path_outfiles='SAME'
      obj%sort_order='ZXY'
      obj%x1=1
      obj%x2=2**30
      obj%y1=1
      obj%y2=2**30


      stdo=pc_get_lun()

      call pgps_update (obj)
      return
      end subroutine pgps_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine pgps_update (obj)
      implicit none
      type(pgps_struct),intent(inout),target :: obj             ! arguments

      integer :: i,istat,k1,k2,lunin,lunout,numtr=1
      character(len=FILENAME_LENGTH) :: modfile,outfile
      character(len=80) :: card
      logical :: found

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.




!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get('PATH_OUTFILES', obj%path_outfiles, pgps_path_outfiles)
      call pc_get('SORT_ORDER   ', obj%sort_order   , pgps_sort_order)
      call pc_get('X1           ', obj%x1           , pgps_x1)
      call pc_get('X2           ', obj%x2           , pgps_x2)
      call pc_get('Y1           ', obj%y1           , pgps_y1)
      call pc_get('Y2           ', obj%y2           , pgps_y2)

      call mfile_set_pathnames_ext(obj%mfile,'hgrid')
      call mfile_update(obj%mfile  )

      call pc_call_end_trap(pgps_end)



!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!




!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!




      call pc_put('PATH_OUTFILES', obj%path_outfiles)
      call pc_put('SORT_ORDER   ', obj%sort_order)
      call pc_put('X1           ', obj%x1)
      call pc_put('X2           ', obj%x2)
      call pc_put('Y1           ', obj%y1)
      call pc_put('Y2           ', obj%y2)

!         Make the parameter cache happy
      call pc_put_global('numtr',numtr)


      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .false.)
      call pc_put_control ('need_label'   , .false.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .true.)
      call pc_put_control ('parallel_safe', .false.)



!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!



      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.



      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      if (.not. mfile_get_filenames(obj%mfile, &
                              obj%filenames, obj%num_files)) then
        call pc_error('PGPS:  bad filenames')
        return
      endif
      outfile=' '
      call getlun(lunin,istat)
      if(istat.ne.0)then
        call pc_error('Unable to get unit number for vmod file')
      endif
      call getlun(lunout,istat)
      if(istat.ne.0)then
        call pc_error('Unable to get unit number for modgrid header file')
      endif

      DO i=1,obj%num_files
        k2=index(obj%filenames(i),'.hgrid')
        if(obj%path_outfiles.eq.'SAME')then
          outfile=obj%filenames(i)(1:k2-1) // '_vmod.trc'
          modfile=obj%filenames(i)(1:k2-1) // '_vmod.modgrid'
        else
          k1=index(obj%filenames(i),'/',.true.)
          outfile=trim(obj%path_outfiles) // obj%filenames(i)(k1:k2-1) // &
                  '_vmod.trc'
          modfile=trim(obj%path_outfiles) // obj%filenames(i)(k1:k2-1) // &
                  '_vmod.modgrid'
        endif
        write(stdo,*)' '
        write(stdo,*)'Converting file ',trim(obj%filenames(i))
        write(stdo,*)' '
        istat=modgrid_saveas(obj%filenames(i),stdo,outfile,obj%x1,&
                             obj%x2,obj%y1,obj%y2,32000000,0,obj%sort_order)
!           Read the output file and save the modgrid header
        open(lunin,iostat=istat,status='OLD',file=outfile)
        if(istat.ne.0)then
          call pc_error('Unable to open ',outfile)
        endif
        open(lunout,iostat=istat,status='NEW',file=modfile)
        if(istat.ne.0)then
          call pc_error('Error opening ',trim(modfile),' as a new file')
        endif
        found=.false.
        do
          read(lunin,9001,iostat=istat)card
          if(istat.lt.0)exit
          if(card(1:14).eq.'#<HDR_modgrid>')found=.true.
          if(found)write(lunout,9001)card
          if(card(1: ).eq.'#</modgrid>')exit
        enddo
        close(lunin)
        close(lunout)
      ENDDO


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


 9001 format(A)
      end subroutine pgps_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine pgps_path_outfiles(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      if(object%path_outfiles.eq.'SAME')return
      call pathcheck('path_outfiles',object%path_outfiles)
      end subroutine pgps_path_outfiles


      subroutine pgps_sort_order(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      integer :: i

      DO i=1,3
        if(object%sort_order(i:i).ne.'X'.and.object%sort_order(i:i).ne.'Y'&
           .and.object%sort_order(i:i).ne.'Z')then
          call pc_error('Valid sort order characters are X,Y, and Z')
        endif
      ENDDO
      return
      end subroutine pgps_sort_order


      subroutine pgps_x1(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine pgps_x1


      subroutine pgps_x2(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine pgps_x2


      subroutine pgps_y1(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine pgps_y1

      subroutine pgps_y2(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine pgps_y2


      subroutine pgps_end
      implicit none

      if(object%x2.le.object%x1)call pc_error('X2 must be greater than X1')
      if(object%y2.le.object%y1)call pc_error('Y2 must be greater than Y1')
      
      return
      end subroutine pgps_end




!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine pgps_wrapup (obj)
      implicit none
      type(pgps_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      return
      end subroutine pgps_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module pgps_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

