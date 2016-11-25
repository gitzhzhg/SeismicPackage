
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- binsort.f90 ------------------------------!!
!!---------------------------- binsort.f90 ------------------------------!!
!!---------------------------- binsort.f90 ------------------------------!!

 
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
!                         C P S   P R I M I T I V E
!
! Name       : BINSORT
! Category   : sorts
! Written    : 2000-04-28   by: Tom Stoeckley
! Revised    : 2006-11-14   by: D. Glover
! Maturity   : production
! Purpose    : Sort traces into bins.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used to sort seismic traces into a specified
! number of bins or panels.  Header word 24 contains a panel number.
! Traces are passed to this primitive in any order, and any number of traces
! at a time.  After all traces are passed in, traces are passed out one at
! a time and panel by panel.  All traces for the first panel are passed out
! before starting the next panel.  For any given panel, the traces will come
! out in the same order they were passed in.  Although normally there will
! probably be the same number of traces in each panel, this primitive does
! not require this. 
!
! This primitive needs header word 24 to be preset with the panel number.
! This primitive resets header words 1, 3, and 4 when passing traces out:
!   Header word 1 is set to the trace sequence number.
!   Header word 3 is set to the panel number (same as header word 24).
!   Header word 4 is set to the trace number within the panel.
!
! This primitive uses TEMPTFILE to temporarily store traces.
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
!                          o      i      i     i     i       i       o
!    call binsort_open   (obj, npanels, seed, nwih, ndpt, lunprint, err)
!
!                          b    b   b   b 
!    call binsort        (obj, ntr, hd, tr)
!
!                          b
!    call binsort_close  (obj)
!
!
! type(binsort_struct)   obj = pointer to the BINSORT structure.
! integer            npanels = number of panels to sort traces into.
! character(len=*)      seed = base name for creating temporary filename.
! integer               nwih = number of words in each trace header in HD.
! integer               ndpt = number of samples in each trace in TR.
! integer           lunprint = unit number for printing (or 0).
! integer                err = BINSORT_ERROR (if error) or BINSORT_OK.
! integer                ntr = number of traces (second dimension) in HD and TR.
! double precision   hd(:,:) = header word array.
! real               tr(:,:) = trace array.
!
! The main BINSORT routine must be called with NTR >= 1 to feed traces into
! the sorting algorithm.  These traces must have header word 24 preset to
! the panel number.  This panel number must be >= 1 and <= NPANELS.  Upon
! returning from the subroutine, NTR will be set to NEED_TRACES.
!
! When there are no more traces to be feed into the sorting algorithm,
! BINSORT must be called with NTR = NO_MORE_TRACES.  Then BINSORT will
! pass out the first sorted trace with NTR set to 1.
!
! Subsequent calls to get the rest of the traces in sorted order must be
! made with NTR preset to NEED_TRACES.  Then BINSORT will pass out the next
! trace with NTR set to 1, until there are no more traces to pass out, in
! which case BINSORT will set NTR to NO_MORE_TRACES.
!
! If at any time the main BINSORT routine suffers an error, it will set
! NTR to FATAL_ERROR.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
! 03. 2006-11-14  D. Glover  Added NULLIFY statements for Intel compiler.
!  2. 2000-10-20  Stoeckley  Remove a tab character.
!  1. 2000-04-28  Stoeckley  Initial version.
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


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module binsort_module
      use named_constants_module
      use temptfile_module
      implicit none
      private
      public :: binsort_open
      public :: binsort
      public :: binsort_close

      character(len=100),public,save :: binsort_IDENT = &
       '$Id: binsort.f90,v 1.3 2006/11/14 14:32:51 Glover prod sps $'

      type,public :: binsort_struct
           private
           type(temptfile_struct),pointer :: temptfile
           integer                        :: npanels  
           integer                        :: lunprint
           integer,pointer                :: ntraces(:) 
           integer,pointer                :: kount  (:) 
           integer                        :: ipanel  
           integer                        :: sequence
      end type binsort_struct

      integer,public,parameter :: BINSORT_OK    = TEMPTFILE_OK
      integer,public,parameter :: BINSORT_ERROR = TEMPTFILE_ERROR


      contains


!!----------------------------- open --------------------------------------!!
!!----------------------------- open --------------------------------------!!
!!----------------------------- open --------------------------------------!!


      subroutine binsort_open (obj,npanels,seed,nwih,ndpt,lunprint,err)
      implicit none
      type(binsort_struct),pointer   :: obj                 ! arguments
      integer         ,intent(in)    :: npanels             ! arguments
      character(len=*),intent(in)    :: seed                ! arguments
      integer         ,intent(in)    :: nwih,ndpt           ! arguments
      integer         ,intent(in)    :: lunprint            ! arguments
      integer         ,intent(out)   :: err                 ! arguments

      allocate (obj)
      nullify (obj%temptfile) ! jpa

      allocate (obj%ntraces(npanels))
      allocate (obj%kount  (npanels))

      obj%npanels    = npanels
      obj%lunprint   = lunprint
      obj%ntraces(:) = 0
      obj%kount  (:) = 0
      obj%ipanel     = 1
      obj%sequence   = 0

      if (lunprint > 0) then
           write (lunprint,*) 'BINSORT: sorting ',npanels,' panels'
      end if

      call temptfile_open (obj%temptfile,seed,nwih,ndpt,lunprint,err)
      return
      end subroutine binsort_open


!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!


      subroutine binsort_close (obj)
      implicit none
      type(binsort_struct),pointer :: obj                 ! arguments

      call temptfile_close (obj%temptfile)

      deallocate (obj%ntraces)
      deallocate (obj%kount)
      deallocate (obj)
      return
      end subroutine binsort_close


!!--------------------------- binsort -----------------------------------!!
!!--------------------------- binsort -----------------------------------!!
!!--------------------------- binsort -----------------------------------!!


      subroutine binsort (obj,ntr,hd,tr)
      implicit none
      type(binsort_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(inout) :: ntr                   ! arguments
      double precision    ,intent(inout) :: hd(:,:)               ! arguments
      real                ,intent(inout) :: tr(:,:)               ! arguments
      integer                            :: indx,irec,err,ipanel  ! local

!!!!!!!!!! we are receiving a trace:

      if (ntr >= 1) then
           do indx = 1,ntr
                ipanel = nint(hd(24,indx))
                if (ipanel < 1 .or. ipanel > obj%npanels) then
                    if (obj%lunprint > 0) then
                         write (obj%lunprint,*) &
                          'BINSORT: header word 24 (panel number) out of range'
                         write (obj%lunprint,*) &
                          'BINSORT: value of header word 24 is', ipanel
                    end if
                    ntr = FATAL_ERROR
                    return
                end if
                obj%ntraces(ipanel) = obj%ntraces(ipanel) + 1
                irec = ipanel + (obj%npanels * (obj%ntraces(ipanel) - 1))
                call temptfile_write  &
                        (obj%temptfile,irec,hd(1:,indx),tr(1:,indx),err)
                if (err /= TEMPTFILE_OK) then
                     ntr = FATAL_ERROR
                     return
                end if
           end do
           ntr = NEED_TRACES
           return
      end if

!!!!!!!!!! there are no more traces:

      if (ntr == NO_MORE_TRACES) then
           if (obj%lunprint > 0) then
                do ipanel = 1,obj%npanels
                    write (obj%lunprint,*) &
          'BINSORT: panel ',ipanel,' has ',obj%ntraces(ipanel),' traces'
                end do
           end if
      end if

!!!!!!!!!! we want to pass out a trace:

      do
          obj%kount(obj%ipanel) = obj%kount(obj%ipanel) + 1
          if (obj%kount(obj%ipanel) <= obj%ntraces(obj%ipanel)) exit
          obj%ipanel = obj%ipanel + 1
          if (obj%ipanel > obj%npanels) then
               ntr = NO_MORE_TRACES
               return
          end if
      end do

      irec = obj%ipanel + (obj%npanels * (obj%kount(obj%ipanel) - 1))
      call temptfile_read (obj%temptfile,irec,hd(1:,1),tr(1:,1),err)
      if (err /= TEMPTFILE_OK) then
           ntr = FATAL_ERROR
           return
      end if

      obj%sequence = obj%sequence + 1
      hd(1,1)      = obj%sequence
      hd(3,1)      = obj%ipanel
      hd(4,1)      = obj%kount(obj%ipanel)
      ntr          = 1
      return
      end subroutine binsort


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module binsort_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

