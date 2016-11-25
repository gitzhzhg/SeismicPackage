!<CPS_v1 type="PROCESS"/>
!!----------------------------- slicer.f90 ---------------------------------!!
!!----------------------------- slicer.f90 ---------------------------------!!
!!----------------------------- slicer.f90 ---------------------------------!!


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
! Name       : SLICER                            (slice traces)
! Category   : miscellaneous
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Slice 3D seismic trace volumes along horizons.
! Portability: No known limitations, but see note below.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process slices 3D seismic traces along horizons.
!
! The horizons are defined by horizon files which are in static file
! format.
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
! This process is a single-trace process.
!
! Input traces can arrive in any order, one or more at a time.  However,
! all input traces received in a single gather must reside at the same
! (X,Y) coordinate location.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs the same traces it inputs, in the same order and in
! the same gather status, plus one depth trace added to the gather.  The
! output traces are much shorter than the input traces.  Each trace sample
! in the output traces corresponds to a single sliced horizon.  Each trace
! sample in the depth trace corresponds to the time or depth of the horizon.
! The depth trace is output at the end of the gather.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name    Description                        Action taken
! ----    -----------                        ------------
! NUMTR   number of traces in a gather       increased by one.
! NWIH    number of header words             used but not changed.
! NDPT    number of sample values in trace   reset to number of horizon slices.
! TSTRT   starting time on trace             reset to one.
! DT      trace sample interval              reset to one.
!
! By setting TSTRT and DT as shown above, the "time" on the trace then
! becomes the horizon number, counting from 1 to the number of horizon
! slices represented on the trace.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#        Description                Action taken
! ----        -----------                ------------
!  2          top mute                   reset to 1.
! 64          bottom mute                reset to NDPT.
! 25          largest absolute value     reset as necessary.
! HDR_IDENT   trace identification       reset to 1 in the newly created
!                                         depth trace if HDR_IDENT is not zero.
!
! Additional header words containing the X and Y coordinates of the
! traces will be used.  These header words are specified in the modspec file
! (or the static files) used to specify the horizons.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!006. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!005. 2006-01-10  B. Menger  Removed Unused Variables.
!  4. 2005-01-31  Stoeckley  Add nullify calls before the memman_nullify calls
!                             to satisfy Portland Group compiler.
!  3. 2003-07-30  Stoeckley  Add process parameter HDR_IDENT.
!  2. 2003-07-28  Stoeckley  Add GUI information field modspec_nhor; fix
!                             problem with reading modspec files; change
!                             default for FILETYPE to MODSPEC FILE; change
!                             FILETYPE kombobox to a combobox.
!  1. 2003-06-19  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! The Portland Group compiler aborts on memman_nullify calls unless the
! pointer has previously been nullified.  Therefore the extra nullify
! statements have been added.  This is a Portland Group compiler bug,
! since it is legitimate to pass an undefined pointer to a subroutine
! which is expecting a pointer.  The only thing that would be illegimate
! in such a case would be for the subroutine to use the ASSOCIATED or
! DEALLOCATE statement on the pointer unless the ALLOCATE or NULLIFY
! statement is used first.
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
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
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
!<NS Input and Output/NC=80>
!
!                    Slice Seismic Traces along Horizons
!
! HDR_IDENT =`II        [/L]Header word containing trace identification.
! FILETYPE~~=`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! `----------------------------------------------------------------------------
!  Select MODSPEC[MODSPEC]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [MODSPEC_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                 [MODSPEC_NHOR]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! `----------------------------------------------------------------------------
!
! `-------------------------------------------------------------------------------
!  Select HORIZON[HORIZON]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [HORIZON_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!  STATUS  HORIZONS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  `XXXXXXX`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `-------------------------------------------------------------------------------
!
!<PARMS MODSPEC     [/XST/ML=140]>
!<PARMS HORIZON     [/XST/ML=140]>
!<PARMS MODSPEC_INFO[/XST/ML=140]>
!<PARMS MODSPEC_NHOR[/XST/ML=140]>
!<PARMS HORIZON_INFO[/XST/ML=140]>
!<PARMS STATUS_ARRAYSET[/XST/YST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="HDR_IDENT">
!<Tip> Header word containing trace identification. </Tip>
! Default = 48
! Allowed = 0-NWIH   (normally 0 or 48-55 or 65-NWIH)
!
! If HDR_IDENT is not 0, the number 1 will be placed into the specified header
! word for the time/depth trace created by this process.  This procedure is
! a courtesy for the DABRA process.  Other than this simple procedure, this
! process is completely ignorent of the DABRA process.
!</Help>
!
!
!<Help KEYWORD="FILETYPE">
!<Tip> Type of horizon file (or files) to read in. </Tip>
! Default = MODSPEC FILE
! Allowed = MODSPEC FILE     (single modspec file containing all horizons)
! Allowed = STATIC FILES     (each horizon in a separate "static" file)
!
! If FILETYPE is "MODSPEC FILE", all horizons are in a single modspec file.
!
! If FILETYPE is "STATIC FILES", each horizon is in a separate file in
! the CPS static file format.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="MODSPEC_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of MODSPEC. </Tip>
!</Help>
!
!<Help KEYWORD="MODSPEC_NHOR" TYPE= "DISPLAY_ONLY">
!<Tip> Number of horizons on MODSPEC file. </Tip>
!</Help>
!
!<Help KEYWORD="HORIZON_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of HORIZON. </Tip>
!</Help>
!
!<Help KEYWORD="STATUS" TYPE= "DISPLAY_ONLY">
!<Tip> Status of HORIZONS. </Tip>
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="SELECT_MODSPEC">
!<Tip> Choose a MODSPEC file using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_HORIZON">
!<Tip> Choose a HORIZON file using a file selection dialog box. </Tip>
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="MODSPEC">
!<Tip> Pathname for a modspec file containing a set of horizons. </Tip>
! Default = NONE
! Allowed = char
!
! If this file is specified, the horizons on this file will be used to slice
! the data.
!
! This file is required if FILETYPE is "MODSPEC FILE".
! This file is not used if FILETYPE is "STATIC FILES".
!</Help>
!
!
!<Help KEYWORD="HORIZON">
!<Tip> Pathname for horizon in static file format. </Tip>
! Default = NONE
! Allowed = char
!
! If this file is specified, the horizon represented on this file will be
! used to slice the data.
!
! This file will be placed into the HORIZONS array.
!
! The HORIZONS array is required if FILETYPE is "STATIC FILES".
! The HORIZONS array is not used if FILETYPE is "MODSPEC FILE".
!</Help>
!
!
!<Help KEYWORD="HORIZONS">
!<Tip> Pathnames for horizons in static file format. </Tip>
! Default = NONE
! Allowed = char
!
! These files will be used to slice the data.
!
! These files can be entered directly in this array or in the HORIZON field,
! optionally using a file selection dialog box.
!
! These files are required if FILETYPE is "STATIC FILES".
! These files are not used if FILETYPE is "MODSPEC FILE".
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module slicer_module
      use pc_module
      use named_constants_module
      use kadabra_module
      use memman_module
      use lav_module
      use pathcheck_module
      use pathchoose_module
      use statclasses_module
      use statclass_module
      use dbug_module

      implicit none
      private
      public :: slicer_create
      public :: slicer_initialize
      public :: slicer_update
      public :: slicer_delete
      public :: slicer            ! main trace processing routine.
      public :: slicer_wrapup

      character(len=100),public,save :: SLICER_IDENT = &
'$Id: slicer.f90,v 1.6 2006/10/17 13:45:47 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: slicer_struct

        private
        logical            :: skip_wrapup                 ! wrapup flag.

        integer                                :: hdr_ident    ! process param
        character(len=40)                      :: filetype     ! process param
        character(len=FILENAME_LENGTH)         :: modspec      ! process param
        character(len=FILENAME_LENGTH)         :: horizon      ! process param
        character(len=FILENAME_LENGTH),pointer :: horizons(:)  ! process param
        integer                                :: nhorizons    ! process param

        type(pathchoose_struct) ,pointer :: pathchoose_modspec
        type(pathchoose_struct) ,pointer :: pathchoose_horizon
        type(statclasses_struct),pointer :: statclasses
        integer                          :: nhorizons_modspec
        integer                          :: nclass

      end type slicer_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                    ,save :: lunprint  ! unit number for printing.
      type(slicer_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine slicer_create (obj)
      type(slicer_struct),pointer :: obj       ! arguments
      integer                     :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
           call pc_error ("Unable to allocate obj in slicer_create")
           return
      end if

      nullify (obj%horizons)
      nullify (obj%statclasses)
      nullify (obj%pathchoose_modspec) ! jpa
      nullify (obj%pathchoose_horizon) ! jpa

!!! The above nullify statement must precede the memman_nullify call
!!! for the Portland Group Compiler.

      call memman_nullify    (obj%horizons, "slicer_horizons")
      call pathchoose_create (obj%pathchoose_modspec, 'modspec', 'modspec')
      call pathchoose_create (obj%pathchoose_horizon, 'horizon', '*')

      call slicer_initialize (obj)
      end subroutine slicer_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine slicer_delete (obj)
      type(slicer_struct),pointer :: obj            ! arguments
      integer                     :: ierr           ! local

      call slicer_wrapup (obj)

      call memman_free        (obj%horizons)
      call pathchoose_delete  (obj%pathchoose_modspec)
      call pathchoose_delete  (obj%pathchoose_horizon)
      call statclasses_delete (obj%statclasses)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) &
             call pc_warning ("error deallocating obj in slicer_delete")
      end subroutine slicer_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine slicer_initialize (obj)
      type(slicer_struct),intent(inout) :: obj            ! arguments

      obj%hdr_ident = 48
      obj%filetype  = 'MODSPEC FILE'
      obj%nhorizons = 0
      obj%modspec   = PATHCHECK_EMPTY
      obj%horizon   = PATHCHECK_EMPTY

      obj%nhorizons_modspec = 0

      call slicer_update (obj)
      end subroutine slicer_initialize


!!-------------------------- start of update -------------------------------!!
!!-------------------------- start of update -------------------------------!!
!!-------------------------- start of update -------------------------------!!


      subroutine slicer_update (obj)
      type(slicer_struct),intent(inout),target :: obj                ! arguments
      integer                                  :: ierr,ihor          ! local
      logical                                  :: whoops,match,mspec ! local
      character(len=80)                        :: msg                ! local
      character(len=80)                        :: modspec_nhor       ! local
      character(len=FILENAME_LENGTH)           :: horizon_keep       ! local
      character(len=FILENAME_LENGTH)           :: modspec_keep       ! local

      character(len=30)     ,allocatable       :: status(:)          ! local
      integer                                  :: numtr,nwih,ndpt    ! local
      integer                                  :: ndpt_output        ! local
      real                                     :: tstrt,dt           ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!---------------------------- read parameters -----------------------------!!
!!---------------------------- read parameters -----------------------------!!
!!---------------------------- read parameters -----------------------------!!

      horizon_keep = obj%horizon
      modspec_keep = obj%modspec

      if (pathchoose_update(obj%pathchoose_modspec, obj%modspec)) return
      if (pathchoose_update(obj%pathchoose_horizon, obj%horizon)) return

      call pc_register_array_names ('STATUS_ARRAYSET',         &
                                               (/'STATUS   ',  &
                                                 'HORIZONS '/))

      call pc_get_global ('numtr'     , numtr)
      call pc_get_global ('nwih'      , nwih)
      call pc_get_global ('ndpt'      , ndpt)
      call pc_get_global ('tstrt'     , tstrt)
      call pc_get_global ('dt'        , dt)

      call pc_get        ('hdr_ident' , obj%hdr_ident)
      call pc_get        ('modspec'   , obj%modspec)
      call pc_get        ('horizon'   , obj%horizon)
      call pc_alloc      ('horizons'  , obj%horizons, obj%nhorizons)

      call kadabra_get_put_options ('filetype',obj%filetype,   &
                                      (/'MODSPEC FILE ',       &
                                        'STATIC FILES '/))


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%hdr_ident < 0 .or. obj%hdr_ident > nwih) then
           call pc_warning ('Bad HDR_IDENT number - reset to 48')
           obj%hdr_ident = 48
      end if

      call pathcheck ('modspec',obj%modspec,show=PATHCHECK_INFO_INPUT)
      call pathcheck ('horizon',obj%horizon,show=PATHCHECK_INFO_INPUT)

      if (obj%horizon /= horizon_keep .and. &
          obj%horizon /= PATHCHECK_EMPTY) then
           match = .false.
           do ihor = 1,obj%nhorizons
                if (obj%horizons(ihor) == obj%horizon) match = .true.
           end do
           if (.not.match) then
                obj%nhorizons = obj%nhorizons + 1
                call memman_reallocate (obj%horizons,obj%nhorizons,ierr)
                obj%horizons(obj%nhorizons) = obj%horizon
           end if
      end if

      if (obj%modspec /= modspec_keep) then
            if (obj%modspec == PATHCHECK_EMPTY) then
                 obj%nhorizons_modspec = 0
            else
                 obj%nhorizons_modspec = &
                        statclasses_num_modspec_layers (obj%modspec,lunprint)
            end if
      end if

      allocate (status(obj%nhorizons))

      do ihor = 1,obj%nhorizons
           call pathcheck ('horizons',obj%horizons(ihor), &
                                           show=PATHCHECK_INFO_INPUT)
           status(ihor) = pathcheck_fetch_brief_message()
      end do

      mspec = (obj%filetype == 'MODSPEC FILE')

      modspec_nhor = trim(string_ii2ss(obj%nhorizons_modspec))// &
                            ' horizons on modspec file'

      if (mspec) then
           ndpt_output = obj%nhorizons_modspec
      else
           ndpt_output = obj%nhorizons
      end if

      if (pc_verify_end()) then
           if (mspec) then
               if (obj%modspec == PATHCHECK_EMPTY) then
                 call pc_error ('a modspec file must be specified.')
               else if (obj%nhorizons_modspec == 0) then
                 call pc_error ('modspec file not found or has no horizons.')
               end if
           else
               if (obj%nhorizons == 0) then
                 call pc_error ('at least one horizon file must be specified.')
               end if
           end if
           if (ndpt_output == 0) then
               call pc_error ('there are no horizon slices to output.')
           end if
      end if


!!---------------------------- write parameters ----------------------------!!
!!---------------------------- write parameters ----------------------------!!
!!---------------------------- write parameters ----------------------------!!


      call pc_put_global   ('numtr'        , numtr + 1)
      call pc_put_global   ('ndpt'         , max(ndpt_output,1))
      call pc_put_global   ('tstrt'        , 1.0)
      call pc_put_global   ('dt'           , 1.0)

      call pc_put          ('hdr_ident'    , obj%hdr_ident)
      call pc_put          ('modspec'      , obj%modspec)
      call pc_put          ('horizons'     , obj%horizons, obj%nhorizons)
      call pc_put_gui_only ('status'       , status      , obj%nhorizons)

      call pc_put_gui_only ('horizon'      , obj%horizon)
      call pc_put_gui_only ('modspec_nhor' , modspec_nhor)

      call pc_put_sensitive_field_flag    ('modspec'        ,      mspec)
      call pc_put_sensitive_field_flag    ('select_modspec' ,      mspec)
  !!  call pc_put_sensitive_field_flag    ('modspec_info'   ,      mspec)
  !!  call pc_put_sensitive_field_flag    ('modspec_nhor'   ,      mspec)
      call pc_put_sensitive_field_flag    ('horizon'        , .not.mspec)
      call pc_put_sensitive_field_flag    ('select_horizon' , .not.mspec)
  !!  call pc_put_sensitive_field_flag    ('horizon_info'   , .not.mspec)
      call pc_put_sensitive_arrayset_flag ('status_arrayset', .not.mspec)

      deallocate (status)


!!------------------------- prepare for execution --------------------------!!
!!------------------------- prepare for execution --------------------------!!
!!------------------------- prepare for execution --------------------------!!


      call statclasses_delete (obj%statclasses)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call statclasses_create (obj%statclasses,nwih,ndpt,tstrt,dt,lunprint)

      select case (obj%filetype)

        case ('STATIC FILES')

            call statclasses_read_static_files &
                     (obj%statclasses,obj%horizons,obj%nhorizons,whoops,msg)
            if (whoops) call pc_error (msg)

        case ('MODSPEC FILE')

            call statclasses_read_modspec_file &
                                (obj%statclasses,obj%modspec,whoops,msg)
            if (whoops) call pc_error (msg)

      end select

      obj%nclass = statclasses_num_classes(obj%statclasses)
      if (obj%nclass == 0) then
           call pc_error ('no horizons for slicing')
      else if (obj%nclass /= ndpt_output) then
           call pc_error ('wrong number of horizons for slicing', &
                          obj%nclass,'- expected',ndpt_output)
      end if


!!----------------------------- finish update ------------------------------!!
!!----------------------------- finish update ------------------------------!!
!!----------------------------- finish update ------------------------------!!


      end subroutine slicer_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine slicer (obj,ntr,hd,tr)
      type(slicer_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(inout) :: ntr                   ! arguments
      double precision   ,intent(inout) :: hd(:,:)               ! arguments
      real               ,intent(inout) :: tr(:,:)               ! arguments
      integer                           :: itr,iclass,imute      ! local
      real                              :: tro(obj%nclass)       ! local
      real                              :: dep(obj%nclass)       ! local
      type(statclass_struct),pointer    :: statclass             ! local

      if (ntr < 1) return

      call dbug_set_message ('slicer (entering)')
      do itr = 1,ntr
        do iclass = 1,obj%nclass
          statclass   => statclasses_fetch_statclass (obj%statclasses,iclass)
          imute       = statclass_get_mute_index   (statclass,hd(:,itr))
          dep(iclass) = statclass_get_static_value (statclass,hd(:,itr))
          tro(iclass) = tr(imute,itr)
        end do
        tr(1:obj%nclass,itr) = tro(:)
        if (itr == 1) then
             tr(1:obj%nclass,ntr+1) = dep(:)
             hd(1:          ,ntr+1) = hd(1:,1)
             if (obj%hdr_ident > 0) hd(obj%hdr_ident,ntr+1) = 1.0
        end if
      end do
      ntr = ntr + 1
      call lav_set_hdr (hd,tr,obj%nclass,ntr)
      call dbug_set_message ('slicer (exiting)')

      end subroutine slicer


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine slicer_wrapup (obj)
      type(slicer_struct),intent(inout) :: obj              ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine slicer_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module slicer_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

