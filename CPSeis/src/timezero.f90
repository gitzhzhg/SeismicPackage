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
! Name       : TIMEZERO
! Category   : math
! Written    : 2003-10-02   by: Tom Stoeckley
! Revised    : 2006-10-17   by: D. Glover
! Maturity   : production
! Purpose    : To adjust a trace to zero time.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! This primitive is used to shift a trace to start at zero time.  This
! primitive also performs the reverse operation to restore the trace to
! the original starting time.  The ending time of the trace is not changed
! by either the forward or the reverse operation.
!
! The purpose of this primitive is to prepare the trace for any process
! which requires the trace to start at zero time.
!
! This primitive uses the TIMESEL primitive to shift the trace.
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
!                           o    i      i    i     o      o     o
!   call timezero_create  (obj, ndpt, tstrt, dt, nzero, error, msg)
!   call timezero_delete  (obj)
!                           b
!
!                                            opt     opt
!                           i    i       o    b       b
!   call timezero_forward (obj, tri, trzero, mtop, mbottom)
!   call timezero_reverse (obj, trzero, tro, mtop, mbottom)
!                           i    i       o    b       b
!                                            opt     opt
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(timezero_struct)  obj = pointer to the timezero object.
! integer               ndpt = number of input and output trace values.
! real                 tstrt = starting time on input and output trace.
! real                    dt = trace sample interval on input and output trace.
! integer              nzero = number of trace values in trace TRZERO.
! logical              error = true if a parameter error is discovered.
! character(len=*)       msg = message for possible printing.
! real             tri(ndpt) = input trace.
! real             tro(ndpt) = output trace.
! real         trzero(nzero) = trace starting at zero time.
! integer               mtop = head mute index of trace (changed).
! integer            mbottom = tail mute index of trace (changed).
!
! TRI, TRZERO, and TRO all have the same ending time.
! If TSTRT is zero, no shifting is done.
!
! MTOP and MBOTTOM are adjusted so that any changes to the timings on the
! trace (due to changing the starting time) are also applied to these mute
! indices.
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
!  3. 2006-10-17 D. Glover  Added NULLIFY statements for Intel compiler.
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


      module timezero_module
      use timesel_module
      implicit none
      public

      type,public :: timezero_struct
        private

        type(timesel_struct),pointer :: ttt_forward
        type(timesel_struct),pointer :: ttt_reverse

      end type timezero_struct

      character(len=100),public :: timezero_ident = &
        "$Id: timezero.f90,v 1.3 2006/10/17 13:45:48 Glover prod sps $"

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine timezero_create (obj,ndpt,tstrt,dt,nzero)
      implicit none
      type(timezero_struct),pointer     :: obj               ! arguments
      integer              ,intent(in)  :: ndpt              ! arguments
      real                 ,intent(in)  :: tstrt,dt          ! arguments
      integer              ,intent(out) :: nzero             ! arguments
      integer                           :: ndummy            ! local
      real                              :: tstop             ! local

      allocate (obj)
      nullify (obj%ttt_forward) ! jpa
      nullify (obj%ttt_reverse) ! jpa
      tstop = tstrt + (ndpt - 1) * dt

      call timesel_create (obj%ttt_forward,tstrt,tstop,dt,  0.0,tstop,nzero)
      call timesel_create (obj%ttt_reverse,  0.0,tstop,dt,tstrt,tstop,ndummy)
      return
      end subroutine timezero_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine timezero_delete (obj)
      implicit none
      type(timezero_struct),pointer :: obj                     ! arguments

      call timesel_delete (obj%ttt_forward)
      call timesel_delete (obj%ttt_reverse)

      deallocate(obj)
      return
      end subroutine timezero_delete


!!--------------------------- forward ------------------------------------!!
!!--------------------------- forward ------------------------------------!!
!!--------------------------- forward ------------------------------------!!


      subroutine timezero_forward (obj, tri, trzero, mtop, mbottom)
      implicit none
      type(timezero_struct),intent(in)    :: obj               ! arguments
      real                 ,intent(in)    :: tri(:)            ! arguments
      real                 ,intent(out)   :: trzero(:)         ! arguments
      integer     ,optional,intent(inout) :: mtop,mbottom      ! arguments


      call timesel_adjust (obj%ttt_forward, tri, trzero, mtop, mbottom)
      return
      end subroutine timezero_forward


!!--------------------------- reverse ------------------------------------!!
!!--------------------------- reverse ------------------------------------!!
!!--------------------------- reverse ------------------------------------!!


      subroutine timezero_reverse (obj, trzero, tro, mtop, mbottom)
      implicit none
      type(timezero_struct),intent(in)    :: obj               ! arguments
      real                 ,intent(in)    :: trzero(:)         ! arguments
      real                 ,intent(out)   :: tro(:)            ! arguments
      integer     ,optional,intent(inout) :: mtop,mbottom      ! arguments

      call timesel_adjust (obj%ttt_reverse, trzero, tro, mtop, mbottom)
      return
      end subroutine timezero_reverse


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module timezero_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

