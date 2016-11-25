!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- lav.f90 --------------------------------!!
!!------------------------------- lav.f90 --------------------------------!!
!!------------------------------- lav.f90 --------------------------------!!

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
! Name       : lav 
! Category   : miscellaneous
! Written    : 2000-01-17   by: Brad Kruse
! Revised    : 2001-03-14   by: Brad Kruse
! Maturity   : production   2001-03-21
! Purpose    : Compute and set LAV
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Compute and set the LAV (Largest Absolute Value) header word, with
! the LAV calculated from the input trace.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
! Process is a single-trace process.
!
! This process does not alter input traces.
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!  25     HDR_LAV                    Set
!
!-------------------------------------------------------------------------------
!</header_word_doc>


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
!
!                                 i   i
!                lav_value = lav (tr, ndpt)
!
! real     tr (:)    = Input trace 
! integer  ndpt      = Number of samples in trace to survey.  
!                      Defaults to size of tr.
! real     lav_value = value of the sample with the Largest 
!                                Absolute Value in array tr.
!
!-------------------------------------------------------------------------------
!
!                       Calling lav_set_hdr_single
!
!                                    b   i   i
!                  call lav_set_hdr (hd, tr, ndpt)
!
! real               tr (:) = Input trace 
! integer            ndpt   = Number of samples in trace to survey.  
!                             Defaults to size of tr.
! double precision   hd (:) = Trace headers.  Header word #25 (HDR_LAV) will
!                             be set to the value of the sample with the 
!                             Largest Absolute Value in the input trace.  
!                             Other hd entries will be unchanged.
!
!-------------------------------------------------------------------------------
!
!                       Calling lav_set_hdr_multiple
!
!                                    b   i   i     i
!                  call lav_set_hdr (hd, tr, ndpt, ntr)
!
! real               tr (:, :) = Multiple input trace 
! integer            ndpt      = Number of samples in trace to survey.  
!                                Defaults to size of 1st dimention of tr.
! integer            ntr       = Number of traces in tr and hd.  Defaults to
!                                size of 2nd dimension of tr.
! double precision   hd (:, :) = Multiple trace headers.  Header word #25 
!                                (HDR_LAV) of each trace header in hd will
!                                be set to the value of the sample with the 
!                                Largest Absolute Value in the corresponding
!                                input trace.  Other hd entries will be 
!                                unchanged.
!
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
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2001-03-21  Brad Kruse   Correct header_word_doc tag
!  4. 2000-06-19  Brad Kruse   Corrected documentation (removed 'optional' from
!                              required parameters).  Updated Maturity from raw
!                              to beta.
!  3. 2000-01-27  Brad Kruse   Corrected syntax error in panic messages.
!.                             Corrected switch of 'Written' & 'Revised' dates.
!  2. 2000-01-26  Brad Kruse   Replaced calls to PC_ERROR with PRINT and STOP 
!.                             statements, to support debugging.  Made NDPT and
!.                             NTR required.
!  1. 2000-01-17  Brad Kruse   Initial version.
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module lav_module
  !
  use named_constants_module, only:   &
        HDR_LAV
  !
  use pc_module, only:  &
        pc_error
  !
  implicit none
  private
  public :: lav
  public :: lav_set_hdr
  public :: lav_set_hdr_single
  public :: lav_set_hdr_multiple

  character(len=100),public,save :: lav_IDENT = &
       '$Id: lav.f90,v 1.5 2001/03/20 21:40:50 sps prod sps $'

  interface lav_set_hdr
    module procedure lav_set_hdr_single
    module procedure lav_set_hdr_multiple
  end interface 

contains

  !!---------------------------------- lav ---------------------------------!!
  !!---------------------------------- lav ---------------------------------!!
  !!---------------------------------- lav ---------------------------------!!


  function lav (tr, ndpt) result (lav_value)
    !
    ! - Arguments
    !
    real,    intent (in) :: tr (:)
    integer, intent (in) :: ndpt
    real                 :: lav_value
    integer              :: temp
    !
    ! - Local variables
    !
    real :: tmin
    real :: tmax
    !
    ! - Begin lav
    !
!-wmm-removed    if (ndpt > size (array = tr, dim = 1)) then
!-wmm-removed      temp = size(array=tr, dim=1)
!-wmm-removed      call pc_error (msg1 = 'LAV single: ndpt is ',            &
!-wmm-removed                     var1 = ndpt,                              &
!-wmm-removed                     msg2 = ', but overruns TR length of ',    &
!-wmm-removed                     var2 = temp)
!-wmm-removed      stop
!-wmm-removed    else if (ndpt < 1) then
!-wmm-removed      call pc_error (msg1 = 'LAV single: ndpt is invalid: ',    &
!-wmm-removed                     var1 = ndpt,                               &
!-wmm-removed                     msg2 = '.  Must be > 0')
!-wmm-removed      stop
!-wmm-removed    else
      tmin = minval (array = tr (:ndpt), dim = 1)
      tmax = maxval (array = tr (:ndpt), dim = 1)
      lav_value = max (a1 = tmax, a2 = abs (tmin))
!-wmm-removed    end if
  end function lav 


  !!-------------------------- lav_set_hdr_single --------------------------!!
  !!-------------------------- lav_set_hdr_single --------------------------!!
  !!-------------------------- lav_set_hdr_single --------------------------!!

  subroutine lav_set_hdr_single (hd, tr, ndpt)
    !
    ! - Arguments
    !
    double precision, intent (inout) :: hd (:)
    real,             intent (in)    :: tr (:)
    integer,          intent (in)    :: ndpt
    integer                          :: temp
    !
    ! - Local variables
    !
    real :: tmin
    real :: tmax
    !
    ! - Begin lav_set_hdr_single
    !
    if (ndpt > size (array = tr, dim = 1)) then
      temp = size(array=tr,dim=1)
      call pc_error (msg1 = 'LAV single: ndpt is ',            &
                     var1 = ndpt,                              &
                     msg2 = ', but overruns TR length of ',    &
                     var2 = temp)
      stop
    else if (ndpt < 1) then
      call pc_error (msg1 = 'LAV single: ndpt is invalid: ',    &
                     var1 = ndpt,                               &
                     msg2 = '.  Must be > 0.')
      stop
    else
      tmin = minval (array = tr (:ndpt), dim = 1)
      tmax = maxval (array = tr (:ndpt), dim = 1)
      hd (HDR_LAV) = dble (a = max (a1 = tmax, a2 = abs (tmin)))
    end if
    !
  end subroutine lav_set_hdr_single 


  !!------------------------- lav_set_hdr_multiple -------------------------!!
  !!------------------------- lav_set_hdr_multiple -------------------------!!
  !!------------------------- lav_set_hdr_multiple -------------------------!!


  subroutine lav_set_hdr_multiple (hd, tr, ndpt, ntr)
    !
    ! - Arguments
    !
    double precision, intent (inout) :: hd (:, :)
    real,             intent (in)    :: tr (:, :)
    integer,          intent (in)    :: ntr
    integer,          intent (in)    :: ndpt
    !
    ! - Local variables
    !
    integer :: t
    real    :: tmin
    real    :: tmax
    integer :: temp
    !
    ! - Begin lav_set_hdr_multiple
    !
    if (ntr > size (array = tr, dim = 2)) then
      temp = size(array=tr,dim=2)
      call pc_error (msg1 = 'LAV multiple: ntr is ',                   &
                     var1 = ntr,                                       &
                     msg2 = ', but overruns number of TR traces: ',    &
                     var2 = temp)
      stop
    else if (ntr < 0) then
      call pc_error (msg1 = 'LAV single: ntr is invalid: ',    &
                     var1 = ndpt,                              &
                     msg2 = '.  Must be >= 0.')
      stop
    else if (ndpt > size (array = tr, dim = 1)) then
      temp = size(array=tr,dim=1)
      call pc_error (msg1 = 'LAV multiple: ndpt is ',          &
                     var1 = ndpt,                              &
                     msg2 = ', but overruns TR length of ',    &
                     var2 = temp)
    else if (ndpt < 1) then
      call pc_error (msg1 = 'LAV single: ndpt is invalid: ',    &
                     var1 = ndpt,                               &
                     msg2 = '.  Must be > 0')
      stop
    else  
      do t = 1, ntr
        tmin = minval (array = tr (:ndpt, t), dim = 1)
        tmax = maxval (array = tr (:ndpt, t), dim = 1)
        hd (HDR_LAV, t) = dble (a = max (a1 = tmax, a2 = abs (tmin)))
      end do
    end if
    !
  end subroutine lav_set_hdr_multiple 

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


end module lav_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

