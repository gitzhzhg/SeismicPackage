!<CPS_v1 type="PRIMITIVE"/>


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
!------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : TIMESEL            (time select)
! Category   : math
! Written    : 2003-10-02   by: Tom Stoeckley
! Revised    : 2004-05-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Change the time range of traces.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! This primitive is designed to change the starting and ending times
! on a trace.
!
! This primitive does not change the sample interval on the trace.
! This primitive does not deal with trace header words.
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
!                             SUBROUTINES
!
!                          o       i          i      i
!    call timesel_create (obj, old_tstrt, old_tstop, dt,
!                              new_tstrt, new_tstop, new_ndpt)
!                                  i          i         o
!
!                                        opt     opt
!                          b    i    o    b       b
!    call timesel_adjust (obj, tri, tro, mtop, mbottom)  ! call for each trace.
!
!                          b
!    call timesel_delete (obj)
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(timesel_struct)      obj = pointer to the timesel object.
! real                old_tstrt = starting time          on input  trace TRI.
! real                new_tstrt = starting time          on output trace TRO.
! real                old_tstop = ending   time          on input  trace TRI.
! real                new_tstop = ending   time          on output trace TRO.
! integer              old_ndpt = number of trace values on input  trace TRI.
! integer              new_ndpt = number of trace values on output trace TRO.
! real                       dt = trace sample interval.
! real            tri(old_ndpt) = input  trace values.
! real            tro(new_ndpt) = output trace values.
! integer                  mtop = head mute index of trace (changed).
! integer               mbottom = tail mute index of trace (changed).
!
! The arguments are related as follows:
!
!             OLD_NDPT = 1 + nint( (OLD_TSTOP - OLD_TSTRT) / DT )
!             NEW_NDPT = 1 + nint( (NEW_TSTOP - NEW_TSTRT) / DT )
!
!             OLD_TSTOP = OLD_TSTRT + (OLD_NDPT - 1) * DT
!             NEW_TSTOP = NEW_TSTRT + (NEW_NDPT - 1) * DT
!
! OLD_NDPT is not in the argument list.
!
! Trace samples will be lost at the start of traces if NEW_TSTRT > OLD_TSTRT.
! Trace samples will be lost at the  end  of traces if NEW_TSTOP < OLD_TSTOP.
!
! Zeros will be padded at the start of traces if NEW_TSTRT < OLD_TSTRT.
! Zeros will be padded at the  end  of traces if NEW_TSTOP > OLD_TSTOP.
!
! MTOP and MBOTTOM are adjusted so that any changes to the timings on the
! trace (due to changing the starting or ending time) are also applied to
! these mute indices.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!  2. 2004-05-03 Stoeckley  Add optional arguments MTOP and MBOTTOM.
!  1. 2003-10-02 Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module timesel_module
      use mth_module
      implicit none
      public


      type,public :: timesel_struct
        private

        integer :: new_ndpt
        integer :: old_istrt    ! index of beginning of overlap on old trace.
        integer :: old_istop    ! index of    end    of overlap on old trace.
        integer :: new_istrt    ! index of beginning of overlap on new trace.
        integer :: new_istop    ! index of    end    of overlap on new trace.
        integer :: ishift

      end type timesel_struct


      character(len=100),public :: timesel_ident = &
        "$Id: timesel.f90,v 1.2 2004/05/03 11:29:40 Stoeckley prod sps $"


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine timesel_create (obj, old_tstrt, old_tstop, dt,  &
                                      new_tstrt, new_tstop, new_ndpt)
      implicit none
      type(timesel_struct),pointer     :: obj             ! arguments
      real                ,intent(in)  :: old_tstrt       ! arguments
      real                ,intent(in)  :: old_tstop       ! arguments
      real                ,intent(in)  :: dt              ! arguments
      real                ,intent(in)  :: new_tstrt       ! arguments
      real                ,intent(in)  :: new_tstop       ! arguments
      integer             ,intent(out) :: new_ndpt        ! arguments
      integer                          :: old_ndpt        ! local

      allocate (obj)

      old_ndpt   = nint((old_tstop - old_tstrt) / dt) + 1
      new_ndpt   = nint((new_tstop - new_tstrt) / dt) + 1
      obj%ishift = nint((new_tstrt - old_tstrt) / dt)

      obj%new_ndpt  = new_ndpt
      obj%old_istrt = max (1, 1 - obj%ishift)
      obj%old_istop = min (new_ndpt, old_ndpt - obj%ishift)
      obj%new_istrt = obj%old_istrt + obj%ishift
      obj%new_istop = obj%old_istop + obj%ishift
      return
      end subroutine timesel_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine timesel_delete (obj)
      implicit none
      type(timesel_struct),pointer :: obj                 ! arguments

      if (associated(obj)) deallocate(obj)
      return
      end subroutine timesel_delete


!!-------------------------------- adjust ----------------------------------!!
!!-------------------------------- adjust ----------------------------------!!
!!-------------------------------- adjust ----------------------------------!!


      subroutine timesel_adjust (obj, tri, tro, mtop, mbottom)
      implicit none
      type(timesel_struct),intent(in)    :: obj               ! arguments
      real                ,intent(in)    :: tri(:)            ! arguments
      real                ,intent(out)   :: tro(:)            ! arguments
      integer    ,optional,intent(inout) :: mtop,mbottom      ! arguments

      tro(obj%old_istrt:obj%old_istop)  = tri(obj%new_istrt:obj%new_istop)
      tro(1:obj%old_istrt-1)            = 0.0
      tro(obj%old_istop+1:obj%new_ndpt) = 0.0

      if (present(mtop)) then
           mtop = mtop - obj%ishift
           call mth_constrain (mtop, 1, obj%new_ndpt)
      end if

      if (present(mbottom)) then
           mbottom = mbottom - obj%ishift
           call mth_constrain (mbottom, 1, obj%new_ndpt)
      end if
      return
      end subroutine timesel_adjust


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module timesel_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

