!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- ka.f90 ---------------------------------!!
!!------------------------------- ka.f90 ---------------------------------!!
!!------------------------------- ka.f90 ---------------------------------!!


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
!                        C P S   P R I M I T I V E
!
! Name       : STATISTICS
! Category   : math
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Gather and print statistics of seismic trace amplitudes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Gather and print statistics of seismic trace amplitudes.
! The absolute values of the trace amplitudes are used.
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
!                          CALLING SEQUENCE
!
!                                     o    i      i    i     i       i
!         call statistics_create    (obj, ndpt, tstrt, dt, valmin, valmax)
!
!                                                 opt
!                                     b   i        i
!         call statistics_gather    (obj, tr, use_abs_values)
!
!                                     b    i      i
!         call statistics_print     (obj, lun, message)
!
!        o                            i   i       i
!      value = statistics_get_value (obj,time,percentile)
!
!                                     b
!         call statistics_delete    (obj)
!
! type(statistics_struct)    obj = pointer to the STATISTICS object.
! integer                   ndpt = number of data points per trace.
! real                     tstrt = starting time on trace (seconds).
! real                        dt = sample interval time on trace (seconds).
! real                    valmin = minimum trace value for analysis.
! real                    valmax = maximum trace value for analysis.
! real                  tr(ndpt) = seismic trace.
! logical         use_abs_values = use absolute values if present and true.
! integer                    lun = logical unit number for printing (> 0).
! character(len=*)       message = message for printing.
! real                      time = reference time on trace.
! real                percentile = percentile (0 to 100) at specified time.
! real                     value = corresponding seismic trace value.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!003. 2006-06-20  B. Menger   Removed Unused Variables.
!  2. 2004-12-06  Stoeckley  Fix problem of using absolute values when value
!                             is nil.
!  1. 2003-06-19  Stoeckley  Initial version.
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


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module statistics_module
      use mth_module
      use terputil_module

      implicit none
      public

      character(len=100),public,save :: STATISTICS_IDENT = &
'$Id: statistics.f90,v 1.3 2006/06/20 13:12:09 Menger prod sps $'


!!------------------------------- data ------------------------------------!!
!!------------------------------- data ------------------------------------!!
!!------------------------------- data ------------------------------------!!


 integer,private,parameter :: NWIN    = 21   ! number of analysis windows.
 integer,private,parameter :: NP      =  6   ! number of printed bin centers.
 integer,private,parameter :: NW      = 10   ! width of each printed bin center.
 integer,private,parameter :: NBINS   = 1+(NP-1)*NW    ! number of bins.


      type,public :: statistics_struct

        private
        integer          :: ndpt       ! global.
        real             :: tstrt      ! global.
        real             :: dt         ! global.
        real             :: valmin     ! center of first trace amplitude bin.
        real             :: valmax     ! center of last trace amplitude bin.

        real             :: awidth     ! number of samples per analysis window.
        real             :: vwidth     ! width of each trace amplitude bin.

        integer          :: total_count   (NWIN)
        integer          :: found_nil     (NWIN)
        integer          :: found_zero    (NWIN)
        real             :: min_value     (NWIN)
        real             :: max_value     (NWIN)
        integer          :: missed_small  (NWIN)
        integer          :: missed_large  (NWIN)
        integer          :: values_binned (NWIN)
        real             :: sum_values    (NWIN)
        real             :: sum_squared   (NWIN)
        integer          :: histo   (NBINS,NWIN)
        real             :: pc      (NBINS,NWIN)

      end type statistics_struct


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine statistics_create (obj,ndpt,tstrt,dt,valmin,valmax)
      type(statistics_struct),pointer    :: obj          ! arguments
      integer                ,intent(in) :: ndpt         ! arguments
      real                   ,intent(in) :: tstrt        ! arguments
      real                   ,intent(in) :: dt           ! arguments
      real                   ,intent(in) :: valmin       ! arguments
      real                   ,intent(in) :: valmax       ! arguments


      allocate(obj)

      obj%ndpt   = ndpt
      obj%tstrt  = tstrt
      obj%dt     = dt
      obj%valmin = valmin
      obj%valmax = valmax

      obj%awidth = real(obj%ndpt-1) / real(NWIN - 1)
      obj%vwidth = (obj%valmax - obj%valmin) / (NBINS - 1)

      obj%total_count   = 0       
      obj%found_nil     = 0      
      obj%found_zero    = 0     
      obj%min_value     = FNIL 
      obj%max_value     = FNIL
      obj%missed_small  = 0  
      obj%missed_large  = 0 
      obj%values_binned = 0
      obj%sum_values    = 0.0   
      obj%sum_squared   = 0.0  
      obj%histo         = 0   
      obj%pc            = 0   

      end subroutine statistics_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine statistics_delete (obj)
      type(statistics_struct),pointer :: obj       ! arguments

      if (.not.associated(obj)) return

      deallocate(obj)
      end subroutine statistics_delete


!!----------------------------- gather -------------------------------------!!
!!----------------------------- gather -------------------------------------!!
!!----------------------------- gather -------------------------------------!!


      subroutine statistics_gather (obj,tr,use_abs_values)
      type(statistics_struct),intent(inout) :: obj                ! arguments
      real                   ,intent(in)    :: tr(:)              ! arguments
      logical   ,optional    ,intent(in)    :: use_abs_values     ! arguments
      integer                               :: iwin,indx,ibin     ! local
      real                                  :: value              ! local

      do indx = 1,obj%ndpt

           iwin = mth_bin_number(1.0,obj%awidth,real(indx))
           call mth_constrain (iwin,1,NWIN)

           value = tr(indx)

           if (.not.present(use_abs_values)) then
                value = tr(indx)
           else if (use_abs_values .and. value /= FNIL) then
                value = abs(tr(indx))
           else
                value = tr(indx)
           end if

           obj%total_count(iwin) = obj%total_count(iwin) + 1

           if (value == FNIL) then
                obj%found_nil(iwin) = obj%found_nil(iwin) + 1
                cycle
           end if

           if (value == 0.0) then
                obj%found_zero(iwin) = obj%found_zero(iwin) + 1
                cycle
           end if

           if (obj%min_value(iwin) == FNIL) then
                obj%min_value(iwin) = value
                obj%max_value(iwin) = value
           else
                obj%min_value(iwin) = min(obj%min_value(iwin),value)
                obj%max_value(iwin) = max(obj%max_value(iwin),value)
           end if

           ibin = mth_bin_number(obj%valmin,obj%vwidth,value)

           if (ibin < 1) then
                obj%missed_small(iwin) = obj%missed_small(iwin) + 1
                cycle
           end if

           if (ibin > NBINS) then
                obj%missed_large(iwin) = obj%missed_large(iwin) + 1
                cycle
           end if

           obj%values_binned (iwin) = obj%values_binned (iwin) + 1
           obj%sum_values    (iwin) = obj%sum_values    (iwin) + value
           obj%sum_squared   (iwin) = obj%sum_squared   (iwin) + value**2
           obj%histo    (ibin,iwin) = obj%histo    (ibin,iwin) + 1

      end do

      end subroutine statistics_gather


!!----------------------------- print -----------------------------------!!
!!----------------------------- print -----------------------------------!!
!!----------------------------- print -----------------------------------!!


      subroutine statistics_print (obj,lun,message)
      type(statistics_struct),intent(inout) :: obj                 ! arguments
      integer                ,intent(in)    :: lun                 ! arguments
      character(len=*)       ,intent(in)    :: message             ! arguments
      integer                               :: iwin,ibin           ! local
      real                                  :: average,time        ! local
      real                                  :: rms,factor,pclast   ! local
      character(len=111)                    :: title               ! local
      character(len=1)                      :: header(NBINS)       ! local
      integer                               :: ipc   (NBINS)       ! local
      integer                               :: total (NBINS)       ! local

!!!!!!!!!!!!!!! integrate the histogram:

      do iwin = 1,NWIN
           do ibin = 1,NBINS
             if (ibin == 1) then
               obj%pc(ibin,iwin) = obj%histo(ibin,iwin)
             else
               obj%pc(ibin,iwin) = obj%histo(ibin,iwin) + obj%pc(ibin-1,iwin)
             end if
           end do
           
           do ibin = 1,NBINS
             pclast = obj%pc(NBINS,iwin)
             if (pclast <= 0.0) then
               obj%pc(ibin,iwin) = 0.0
             else
               obj%pc(ibin,iwin) = 100.0 * obj%pc(ibin,iwin) / pclast
             end if
             call mth_constrain (obj%pc(ibin,iwin),0.0,99.9)
           end do
      end do

!!!!!!!!!!!!!!! integrate the histogram summed over analysis windows:

      do ibin = 1,NBINS
           total(ibin) = sum(obj%histo(ibin,:))
      end do

      do ibin = 2,NBINS
          total(ibin) = total(ibin) + total(ibin-1)
      end do
           
      do ibin = 1,NBINS
          if (total(NBINS) <= 0) then
               total(ibin) = 0
          else
               total(ibin) = 10 * total(ibin) / total(NBINS)
          end if
          call mth_constrain (total(ibin),0,9)
      end do

!!!!!!!!!!!!!!! print the statistics:

      title = '++++++++++++ statistics for '//trim(message)//' ++++++++++++'
      header(:)          = ' '
      header(1:NBINS:10) = '|'

      write (lun,*) ' '
      write (lun,1000) title,total
      write (lun,1001) header
      write (lun,1002) (obj%valmin + (ibin-1)*obj%vwidth, ibin=1,NBINS,NW)
      write (lun,1003) header

      do iwin = 1,NWIN

           time = obj%tstrt + (iwin - 1) * obj%awidth * obj%dt

           if (obj%values_binned(iwin) > 0) then
                average = obj%sum_values (iwin) / obj%values_binned(iwin)
                rms     = obj%sum_squared(iwin) / obj%values_binned(iwin)
                rms     = sqrt(rms)
           else
                average = 0.0
                rms     = 0.0
           end if

           factor = 100.0 / max(obj%total_count(iwin),1)
           ipc(:) = 0.1 * obj%pc(:,iwin)                 ! truncated value.

           write (lun,1004) obj%total_count  (iwin) , &
              nint(factor * obj%found_nil    (iwin)), &
              nint(factor * obj%found_zero   (iwin)), &
                            obj%min_value    (iwin) , &
                            obj%max_value    (iwin) , &
              nint(factor * obj%missed_small (iwin)), &
              nint(factor * obj%missed_large (iwin)), &
              nint(factor * obj%values_binned(iwin)), &
                            average,rms,time,ipc
           end do

1000  format (1x,a80,9x,51i1)

1001  format (1x,80x,9x,51a1)

1002  format ('   total  %nils %zeroes    min      max  ',       &
              '  %too   %too  %values  average   rms  ',         &
              3x,6f10.2)

1003  format ('  #values found  found    value    value ',       &
              '  small  large  binned   value   value   TIME ',  &
              3x,51a1)

1004  format (1x,3i7,2f9.2,3i7,2f9.2,f7.3,1x,3x,51i1)

!!!!!!!!!!!!!!! finish up and return:

      write (lun,*) ' '

      end subroutine statistics_print


!!----------------------- get value from percentile -----------------------!!
!!----------------------- get value from percentile -----------------------!!
!!----------------------- get value from percentile -----------------------!!


      function statistics_get_value (obj,time,percentile) result (value)
      type(statistics_struct),intent(in)    :: obj                 ! arguments
      real                   ,intent(in)    :: time                ! arguments
      real                   ,intent(in)    :: percentile          ! arguments
      real                                  :: value               ! result
      integer                               :: iwina,i1a,i2a       ! local
      integer                               :: iwinb,i1b,i2b       ! local
      real                                  :: p1a,p2a,v1a,v2a     ! local
      real                                  :: p1b,p2b,v1b,v2b     ! local
      real                                  :: fwin                ! local
      real                                  :: valuea,valueb       ! local

      fwin  = 1.0 + (time - obj%tstrt) / (obj%awidth * obj%dt)
      iwina = fwin
      iwinb = iwina + 1
      call mth_constrain (iwina,1,NWIN)
      call mth_constrain (iwinb,1,NWIN)

 call terputil_binary_search (percentile,obj%pc(:,iwina),NBINS,i1a,i2a,p1a,p2a)
 call terputil_binary_search (percentile,obj%pc(:,iwinb),NBINS,i1b,i2b,p1b,p2b)

      v1a    = obj%valmin + (i1a - 1) * obj%vwidth
      v2a    = obj%valmin + (i2a - 1) * obj%vwidth
      valuea = terputil_root(percentile,p1a,p2a,v1a,v2a)

      v1b    = obj%valmin + (i1b - 1) * obj%vwidth
      v2b    = obj%valmin + (i2b - 1) * obj%vwidth
      valueb = terputil_root(percentile,p1b,p2b,v1b,v2b)

      value = 0.5 * (valuea + valueb)

      end function statistics_get_value


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module statistics_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

