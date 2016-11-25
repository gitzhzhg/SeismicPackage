!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- sequence.f90 -------------------------------!!
!!------------------------------- sequence.f90 -------------------------------!!
!!------------------------------- sequence.f90 -------------------------------!!

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
! Name       : SEQUENCE (Reset header sequence words 1 & 4)
! Category   : miscellaneous
! Written    : 2000-04-20   by: Brad Kruse
! Revised    : 2000-04-20   by: Brad Kruse
! Maturity   : production   2000-06-29
! Purpose    : Resequence traces when traces are added or removed
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Header word 1 (HDR_SEQUENCE) must maintain a sequence counting order
! in a job.  Header word 3 denotes a group of traces, and Header word 4 is a 
! sequence within each group of traces.
!
! This SEQUENCE procedure will maintain and set the sequence for words 1 & 4,
! and reset the sequence to '1' if the value of Header word 3 changes.
! This is required anytime traces are re-ordered, traces are inserted into
! the stream of traces in a job, or if traces are removed from a job.
!
!-------------------------------------------------------------------------------
!</descript_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! 3       Current gather             Referenced.
! 4       Current channel            Renumbered.
!
!-------------------------------------------------------------------------------
!</header_word_doc


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
!  The subroutines in this module, with information about their arguments.
!
!                                 b
!         call  sequence_clear (obj)
!
!                           b    i   b
!         call  sequence (obj, ntr, hd)
!
! type (sequence_struct)  obj       = The sequence object
! integer                 ntr       = Number of traces (hd 2nd dimension)
! double precision        hd (:, :) = Two dimension array of header words
!
! Note that hd must be at least hd (1:4, 1:1), since header words 1, 3, and 4 
! are referenced or set.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! Sequence should be called to resequence traces.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2000-04-20  Brad Kruse   Initial version.
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
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


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

module sequence_module
  !
  ! - Module references
  !
  use named_constants_module
  !
  implicit none
  !
  private
  public :: sequence_clear
  public :: sequence

  character (len=100), public,save :: SEQUENCE_IDENT = &
   '$Id: sequence.f90,v 1.1 2000/06/28 17:59:52 sps prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

  type,public :: sequence_struct              
    private
    integer :: seq_hw1
    integer :: seq_hw4
    integer :: prev_hw3
  end type sequence_struct


contains

  !!----------------------------- clear -------------------------------------!!
  !!----------------------------- clear -------------------------------------!!
  !!----------------------------- clear -------------------------------------!!
  !
  ! - This subroutine will clear the data structure.
  !
  subroutine sequence_clear (obj)
    !
    ! - Arguments
    !
    type(sequence_struct) :: obj       ! arguments
    !
    ! - Begin 
    !
    obj%seq_hw1  = 0
    obj%seq_hw4  = 0
    obj%prev_hw3 = -1
    !
  end subroutine sequence_clear


  !!--------------------------- subroutines -------------------------------!!
  !!--------------------------- subroutines -------------------------------!!
  !!--------------------------- subroutines -------------------------------!!

  subroutine sequence (obj, ntr, hd)
    !
    ! - Arguments
    !
    type (sequence_struct), intent (inout) :: obj
    integer,                intent (in)    :: ntr
    double precision,       intent (inout) :: hd (:,:)
    !
    ! - Local variables
    !
    integer :: n
    integer :: group
    !
    ! - Begin 
    !
    do n = 1, ntr
      !
      group = nint (hd (HDR_CURRENT_GROUP, n))
      !
      if (obj%prev_hw3 /= group) then
        obj%seq_hw4  = 1
        obj%prev_hw3 = group
      else
        obj%seq_hw4  = obj%seq_hw4 + 1
      end if
      !
      obj%seq_hw1                 = obj%seq_hw1 + 1
      hd (HDR_SEQUENCE,        n) = dble (obj%seq_hw1)
      hd (HDR_CURRENT_CHANNEL, n) = dble (obj%seq_hw4)
      !
    end do
    !
  end subroutine sequence


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


end module sequence_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

