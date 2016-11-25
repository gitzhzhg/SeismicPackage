!<CPS_v1 type="PROCESS"/>
!!------------------------------ pickset.f90 --------------------------------!!
!!------------------------------ pickset.f90 --------------------------------!!
!!------------------------------ pickset.f90 --------------------------------!!


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
! Name       : PICKSET
! Category   : io
! Written    : 2004-09-14   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Set picks into headers from refraction statics pick file.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process reads picks out of a refraction statics pick file (.cst file)
! and puts them into trace headers.
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
! Shot profile traces must be in the same order as the shot profiles on
! the refraction statics pick file.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! Output traces are same as input traces.
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
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#       Description                           Action taken
! ----       -----------                           ------------
! HDR_PICK   header containing pick values (ms).   reset.
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
!  1. 2005-03-07  Stoeckley  Initial version.
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
!<NS PICKSET Process/NC=80>
!
!                               PICKSET
!
! This process reads picks out of a refraction statics pick file (.cst file) and puts them into trace headers.
!
!
!       HDR_PICK =`III [/L]Header word to receive pick values (milliseconds).
!
!       HDR_XSRC =`III [/L]Header word containing source X ground position.
!       HDR_YSRC =`III [/L]Header word containing source Y ground position.
!
!       HDR_XREC =`III [/L]Header word containing receiver X ground position.
!       HDR_YREC =`III [/L]Header word containing receiver Y ground position.
!
!
! Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [pathname_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS pathname      [/ML=128/XST]>
!<PARMS pathname_info [/ML=128/XST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="HDR_PICK">
!<Tip> Header word to receive pick values (milliseconds).</Tip>
! Default = 48
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_XSRC">
!<Tip> Header word containing source X ground position.</Tip>
! Default = 46
! Allowed = 1 - NWIH
!
! Normally the source ground positions are (9,0) or (46,0) or (33,34).
!</Help>
!
!
!<Help KEYWORD="HDR_YSRC">
!<Tip> Header word containing source Y ground position.</Tip>
! Default = 0
! Allowed = 1 - NWIH
!
! Normally the source ground positions are (9,0) or (46,0) or (33,34).
!</Help>
!
!
!<Help KEYWORD="HDR_XREC">
!<Tip> Header word containing receiver X ground position.</Tip>
! Default = 47
! Allowed = 1 - NWIH
!
! Normally the receiver ground positions are (47,0) or (35,36).
!</Help>
!
!
!<Help KEYWORD="HDR_YREC">
!<Tip> Header word containing receiver Y ground position.</Tip>
! Default = 0
! Allowed = 1 - NWIH
!
! Normally the receiver ground positions are (47,0) or (35,36).
!</Help>
!
!
!<Help KEYWORD="PATHNAME">
!<Tip> CPS Refraction statics pick file (.cst file).</Tip>
! Default = NONE
! Allowed = Valid file name.
!
! If DIRECTION is HEADERS TO FILE, PATHNAME will be an output file.
! If DIRECTION is FILE TO HEADERS, PATHNAME will be an input file.
!</Help>
!
!
!<Help KEYWORD="PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module pickset_module
      use pc_module
      use named_constants_module
      use pathchoose_module
      use pathcheck_module
      use pickio_module

      implicit none
      private
      public :: pickset_create
      public :: pickset_initialize
      public :: pickset_update
      public :: pickset_delete
      public :: pickset            ! main trace processing routine.
      public :: pickset_wrapup

      character(len=100),public,save :: PICKSET_IDENT = &
'$Id: pickset.f90,v 1.2 2006/10/17 13:45:46 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: pickset_struct

        private
        logical                         :: skip_wrapup   ! wrapup flag.

        integer                         :: hdr_pick      ! process parameter
        integer                         :: hdr_xsrc      ! process parameter
        integer                         :: hdr_ysrc      ! process parameter
        integer                         :: hdr_xrec      ! process parameter
        integer                         :: hdr_yrec      ! process parameter
        character(len=FILENAME_LENGTH)  :: pathname      ! process parameter

        type(pathchoose_struct),pointer :: pathchoose
        type(pickio_struct)    ,pointer :: pickio
        integer                         :: nch
        real                            :: shot(10)
        integer                ,pointer :: iarriv(:)
        integer                ,pointer :: ixgp(:)
        integer                ,pointer :: iygp(:)
        integer                ,pointer :: ioff(:)
        integer                ,pointer :: ielev(:)
        logical                         :: starting,finished


      end type pickset_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                     ,save :: lunprint  ! unit number for printing.
      type(pickset_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine pickset_create (obj)

      type(pickset_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()
      allocate (obj)

      nullify (obj%iarriv)
      nullify (obj%ixgp)
      nullify (obj%iygp)
      nullify (obj%ioff)
      nullify (obj%ielev)
      nullify (obj%pickio)
      nullify (obj%pathchoose) ! jpa

      call pathchoose_create (obj%pathchoose, 'PATHNAME', 'cst')

      call pickset_initialize (obj)

      end subroutine pickset_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine pickset_delete (obj)

      type(pickset_struct),pointer :: obj       ! arguments

      call pickset_wrapup (obj)

      if (associated(obj%iarriv)) deallocate (obj%iarriv)
      if (associated(obj%ixgp  )) deallocate (obj%ixgp)
      if (associated(obj%iygp  )) deallocate (obj%iygp)
      if (associated(obj%ioff  )) deallocate (obj%ioff)
      if (associated(obj%ielev )) deallocate (obj%ielev)

      call pathchoose_delete (obj%pathchoose)

      call pickio_close (obj%pickio)

      deallocate(obj)

      end subroutine pickset_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine pickset_initialize (obj)

      type(pickset_struct),intent(inout) :: obj       ! arguments

      obj%hdr_pick      = 48
      obj%hdr_xsrc      = 46
      obj%hdr_ysrc      = 0
      obj%hdr_xrec      = 47
      obj%hdr_yrec      = 0
      obj%pathname      = PATHCHECK_EMPTY

      call pickset_update (obj)

      end subroutine pickset_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine pickset_update (obj)

      type(pickset_struct),intent(inout),target :: obj            ! arguments
      integer                                   :: nwih
      integer                                   :: err 
      character(len=80)                         :: msg
      integer                                   :: ngrp,itmax
      integer                                   :: mxxgp,mxygp,mnxgp,mnygp
      integer                                   :: mxo,incgp
      real                                      :: rinc,trsh    
      character(len=8)                          :: izc
      integer                                   :: nxpw,nypw,npkwn
      integer                                   :: npwoff,ipkwhx,ipkwhy
      real                                      :: xpw(PICKIO_MAX_NXPW)
      real                                      :: ypw(PICKIO_MAX_NYPW)
      real                                      :: pkwn(PICKIO_MAX_NPKWN)

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%pathchoose,obj%pathname)) return

      call pc_get_global ('nwih'    , nwih)

      call pc_get ('hdr_pick     ', obj%hdr_pick)
      call pc_get ('hdr_xsrc     ', obj%hdr_xsrc)
      call pc_get ('hdr_ysrc     ', obj%hdr_ysrc)
      call pc_get ('hdr_xrec     ', obj%hdr_xrec)
      call pc_get ('hdr_yrec     ', obj%hdr_yrec)
      call pc_get ('pathname     ', obj%pathname)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%hdr_pick <= 0 .or. obj%hdr_pick > nwih) then
           call pc_warning ('Illegal value of HDR_PICK - reset to 48')
           obj%hdr_pick = 48
      endif

      if (obj%hdr_xsrc <= 0 .or. obj%hdr_xsrc > nwih) then
           call pc_warning ('Illegal value of HDR_XSRC - reset to 46')
           obj%hdr_xsrc = 46
      endif

      if (obj%hdr_ysrc < 0 .or. obj%hdr_ysrc > nwih) then
           call pc_warning ('Illegal value of HDR_YSRC - reset to 0')
           obj%hdr_ysrc = 0
      endif

      if (obj%hdr_xrec <= 0 .or. obj%hdr_xrec > nwih) then
           call pc_warning ('Illegal value of HDR_XREC - reset to 47')
           obj%hdr_xrec = 47
      endif

      if (obj%hdr_yrec < 0 .or. obj%hdr_yrec > nwih) then
           call pc_warning ('Illegal value of HDR_YREC - reset to 0')
           obj%hdr_yrec = 0
      endif

      call pathcheck ('PATHNAME', obj%pathname, 'cst', required=.true., &
                      show=PATHCHECK_INFO_INPUT)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put ('hdr_pick     ', obj%hdr_pick)
      call pc_put ('hdr_xsrc     ', obj%hdr_xsrc)
      call pc_put ('hdr_ysrc     ', obj%hdr_ysrc)
      call pc_put ('hdr_xrec     ', obj%hdr_xrec)
      call pc_put ('hdr_yrec     ', obj%hdr_yrec)
      call pc_put ('pathname     ', obj%pathname)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%iarriv)) deallocate (obj%iarriv)
      if (associated(obj%ixgp  )) deallocate (obj%ixgp)
      if (associated(obj%iygp  )) deallocate (obj%iygp)
      if (associated(obj%ioff  )) deallocate (obj%ioff)
      if (associated(obj%ielev )) deallocate (obj%ielev)

      call pickio_close (obj%pickio)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call pickio_open_read  (obj%pickio,obj%pathname,lunprint,err,msg,  &
                              ngrp,itmax,                                &
                              mxxgp,mxygp,mnxgp,mnygp,                   &
                              mxo,incgp,                                 &
                              rinc,obj%nch,trsh,izc,                     &
                              nxpw,nypw,npkwn,                           &
                              npwoff,ipkwhx,ipkwhy,                      &
                              xpw,ypw,pkwn)

      if (err /= PICKIO_OK) then
           call pc_error (msg)
           return
      endif

      allocate (obj%iarriv(obj%nch))
      allocate (obj%ixgp  (obj%nch))
      allocate (obj%iygp  (obj%nch))
      allocate (obj%ioff  (obj%nch))
      allocate (obj%ielev (obj%nch))

      obj%shot  (:) = 0.0
      obj%iarriv(:) = 0
      obj%ixgp  (:) = 0
      obj%iygp  (:) = 0
      obj%ioff  (:) = 0
      obj%ielev (:) = 0

      obj%starting = .true.
      obj%finished = .false.

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine pickset_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine pickset (obj,ntr,hd,tr)

      type(pickset_struct),intent(inout) :: obj               ! arguments
      integer             ,intent(inout) :: ntr               ! arguments
      double precision    ,intent(inout) :: hd(:,:)           ! arguments
      real                ,intent(inout) :: tr(:,:)           ! arguments
      integer                            :: itr               ! local
      logical                            :: whoops            ! local

      do itr = 1,ntr
           call pickset_work (obj,hd(:,itr),whoops)
           if (whoops) then
                call pickset_wrapup (obj)
                ntr = FATAL_ERROR
                return
           endif
      enddo

      if (ntr == 0) call pickset_wrapup (obj)

      end subroutine pickset


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine pickset_wrapup (obj)

      type(pickset_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine pickset_wrapup


!!-------------------------------- work -----------------------------------!!
!!-------------------------------- work -----------------------------------!!
!!-------------------------------- work -----------------------------------!!


      subroutine pickset_work (obj,hd,whoops)

      type(pickset_struct),intent(inout) :: obj               ! arguments
      double precision    ,intent(inout) :: hd(:)             ! arguments
      logical             ,intent(out)   :: whoops            ! arguments
      integer                            :: ich               ! local
      integer                            :: xsrc,ysrc         ! local
      integer                            :: xrec,yrec         ! local
      integer                            :: err               ! local
      character(len=80)                  :: msg               ! local

      if (obj%finished) then
           hd(obj%hdr_pick) = 0
           whoops = .false.
           return
      endif

      xsrc = nint(hd(obj%hdr_xsrc))
      xrec = nint(hd(obj%hdr_xrec))

      ysrc = 0
      yrec = 0

      if (obj%hdr_ysrc > 0) ysrc = nint(hd(obj%hdr_ysrc))
      if (obj%hdr_yrec > 0) yrec = nint(hd(obj%hdr_yrec))

      do

           if (.not.obj%starting         .and. &
               xsrc == nint(obj%shot(1)) .and. &
               ysrc == nint(obj%shot(8))) exit

           obj%starting = .false.

           call pickio_read_group (obj%pickio,err,msg,obj%shot,   &
                                   obj%iarriv,obj%ixgp,obj%iygp,  &
                                   obj%ioff,obj%ielev)

           if (err == PICKIO_EOF) then
                hd(obj%hdr_pick) = 0
                obj%finished = .true.
                whoops = .false.
                return
           elseif (err /= PICKIO_OK) then
                call pc_error ('PICKSET:',msg)
                whoops = .true.
                return
           endif

      enddo

      do ich = 1,obj%nch

           if (xrec == obj%ixgp(ich) .and. &
               yrec == obj%iygp(ich)) then

                hd(obj%hdr_pick) = obj%iarriv(ich)
                whoops = .false.
                return
           endif

      enddo

      hd(obj%hdr_pick) = 0
      whoops = .false.

      end subroutine pickset_work


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module pickset_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

