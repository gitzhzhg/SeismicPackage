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
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E            
!
! Name       : incutils 
! Category   : cfe
! Written    : 2000-08-15   by: Donna K. Vunderink
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Utilities used by the incjobname module.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This primitive is used by cfe.
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
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  6. 2006-06-20  B. Menger    Removed Unused Variables.
!  5. 2003-09-15  Stoeckley    Changed type to primitive; changed category to
!                               cfe.
!  4. 2002-01-04  Vunderink    Changed name of this module from inc_utils to
!                                incutils.
!  3. 2000-12-11  Vunderink    Added support for cycles.
!  2. 2000-09-14  Vunderink    Fix compiler warning by adjusting intent on
!                                argument.
!  1. 2000-08-15  Vunderink    Initial version.
!
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


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module incutils_module

      use pc_module

      implicit none

      private
      public :: incutils_start_trap
      public :: incutils_dup_trap
      public :: incutils_inc_trap
      public :: incutils_cycles_trap
      public :: incutils_end_trap
      public :: incutils_todo_trap
      public :: incutils_expand_mask
      public :: incutils_selection

      character(len=100),public,save :: incutils_ident = &
       '$Id: incutils.f90,v 1.6 2006/06/20 13:11:56 Menger prod sps $'

      character(len=1)         ,save :: carray(26)

      contains


!!------------------------------ start_trap -------------------------------!!
!!------------------------------ start_trap -------------------------------!!
!!------------------------------ start_trap -------------------------------!!


      subroutine incutils_start_trap (start,end,dup,inc,cycles,todo,  &
                                       favor_end,maxdo)
      implicit none
      character(len=*),intent(inout)      :: start                    ! argument
      character(len=*),intent(inout)      :: end                      ! argument
      integer         ,intent(in)         :: dup                      ! argument
      integer         ,intent(in)         :: inc                      ! argument
      integer         ,intent(in)         :: cycles                   ! argument
      integer         ,intent(inout)      :: todo                     ! argument
      logical         ,intent(inout)      :: favor_end                ! argument
      integer         ,optional           :: maxdo                    ! argument


      integer                             :: i1                       ! local
      integer                             :: i2                       ! local
      integer                             :: itemp                    ! local
      integer                             :: istat                    ! local


      call string_cc2ii(start,i1,istat)
      if (istat .eq. 1) then                              ! start was an integer
        if (favor_end) then                               ! favor end - set todo
          call string_cc2ii(end,i2,istat)
          if (istat .eq. 1) then                          ! end was an integer
            if (inc .ne. 0) then                          ! set todo
              itemp = todo
              todo = (i2 - i1) / inc * dup + 1
              if (present(maxdo)) then
                if (todo .gt. maxdo) then
                  todo = maxdo
                  i2 = i1 + (todo - 1) / dup * inc
                  call string_ii2cc (i2,end)
                  favor_end = .false.
                endif
              endif
              if (todo .lt. 1) then                       ! gives bad todo 
                todo = itemp                              ! must set end
                if (present(maxdo)) then
                  if (todo .gt. maxdo) todo = maxdo
                endif
                i2 = i1 + (todo - 1) / dup * inc
                call string_ii2cc (i2,end)
                favor_end = .false.
              endif
            else
              call pc_error ('Increment must be non-zero')
            endif
          else                                            ! end was alpha
            if (inc .ne. 0) then                          ! must set end
              if (present(maxdo)) then
                if (todo .gt. maxdo) todo = maxdo
              endif
              i2 = i1 + (todo - 1) / dup * inc
              call string_ii2cc (i2,end)
              favor_end = .false.
            else
              call pc_error ('Increment must be non-zero')
            endif
          endif
        else                                              ! favor todo - set end
          if (present(maxdo)) then                        ! set end
            if (todo .gt. maxdo) todo = maxdo
          endif
          i2 = i1 + (todo - 1) / dup * inc
          call string_ii2cc (i2,end)
        endif

      else                                                ! start was alpha
        call incutils_string_numeric (start,i1)
        if (favor_end) then                               ! favor end - set todo
          call string_cc2ii(end,i2,istat)
          if (istat .eq. 1) then                          ! end was an integer
            if (present(maxdo)) then                      ! must set end
              if (todo .gt. maxdo) todo = maxdo
            endif
            i2 = i1 + (todo - 1) / dup * inc
            call incutils_numeric_string (i2,end)
            favor_end = .false.
          else                                            ! end was alpha
            call incutils_string_numeric (end,i2)        ! set todo
            todo = (i2 - i1) / inc * dup + 1
            if (present(maxdo)) then
              if (todo .gt. maxdo) then
                todo = maxdo
                i2 = i1 + (todo - 1) / dup * inc
                call incutils_numeric_string (i2,end)
                favor_end = .false.
              endif
            endif
            if (todo .lt. 1) then                       ! gives bad todo 
              todo = itemp                              ! must set end
              if (present(maxdo)) then
                if (todo .gt. maxdo) todo = maxdo
              endif
              i2 = i1 + (todo - 1) / dup * inc
              call incutils_numeric_string (i2,end)
              favor_end = .false.
            endif
          endif
        else                                              ! favor todo - set end
          if (present(maxdo)) then                        ! set end
            if (todo .gt. maxdo) todo = maxdo
          endif
          i2 = i1 + (todo - 1) / dup * inc
          call incutils_numeric_string (i2,end)
        endif
      endif

      return
      end subroutine incutils_start_trap


!!------------------------------- dup_trap --------------------------------!!
!!------------------------------- dup_trap --------------------------------!!
!!------------------------------- dup_trap --------------------------------!!


      subroutine incutils_dup_trap (start,end,dup,inc,cycles,todo,  &
                                     favor_end,maxdo)
      implicit none
      character(len=*),intent(inout)      :: start                    ! argument
      character(len=*),intent(inout)      :: end                      ! argument
      integer         ,intent(in)         :: dup                      ! argument
      integer         ,intent(in)         :: inc                      ! argument
      integer         ,intent(in)         :: cycles                   ! argument
      integer         ,intent(inout)      :: todo                     ! argument
      logical         ,intent(inout)      :: favor_end                ! argument
      integer         ,optional           :: maxdo                    ! argument

      integer                             :: i1                       ! local
      integer                             :: i2                       ! local
      integer                             :: itemp                    ! local
      integer                             :: istat                    ! local


      call string_cc2ii(start,i1,istat)
      if (istat .eq. 1) then                              ! start was an integer
        if (favor_end) then                               ! favor end - set todo
          call string_cc2ii(end,i2,istat)
          if (istat .eq. 1) then                          ! end was an integer
            itemp = todo                                  ! set todo
            todo = (i2 - i1) / inc * dup + 1
            if (present(maxdo)) then
              if (todo .gt. maxdo) then
                todo = maxdo
                i2 = i1 + (todo - 1) / dup * inc
                call string_ii2cc (i2,end)
                favor_end = .false.
              endif
            endif
            if (todo .lt. 1) then                         ! gives bad todo
              todo = itemp                                ! must set end
              i2 = i1 + (todo - 1) / dup * inc
              call string_ii2cc (i2,end)
              favor_end = .false.
            endif
          else                                            ! end was alpha
            if (present(maxdo)) then                      ! must set end
              if (todo .gt. maxdo) todo = maxdo
            endif
            i2 = i1 + (todo - 1) / dup * inc
            call string_ii2cc(i2,end)
            favor_end = .false.
          endif
        else                                              ! favor todo - set end
          if (present(maxdo)) then                        ! set end
            if (todo .gt. maxdo) todo = maxdo
          endif
          i2 = i1 + (todo - 1) / dup * inc
          call string_ii2cc(i2,end)
        endif

      else                                                ! start was alpha
        call incutils_string_numeric (start,i1)
        if (favor_end) then                               ! favor end - set todo
          call string_cc2ii(end,i2,istat)
          if (istat .eq. 1) then                          ! end was an integer
            if (present(maxdo)) then                      ! must set end
              if (todo .gt. maxdo) todo = maxdo
            endif
            i2 = i1 + (todo - 1) / dup * inc
            call incutils_numeric_string (i2,end)
            favor_end = .false.
          else                                            ! end was alpha
            call incutils_string_numeric (end,i2)        ! set todo
            todo = (i2 - i1) / inc * dup + 1
            if (present(maxdo)) then
              if (todo .gt. maxdo) then
                todo = maxdo
                i2 = i1 + (todo - 1) / dup * inc
                call incutils_numeric_string (i2,end)
                favor_end = .false.
              endif
            endif
            if (todo .lt. 1) then                       ! gives bad todo
              todo = itemp                              ! must set end
              if (present(maxdo)) then
                if (todo .gt. maxdo) todo = maxdo
              endif
              i2 = i1 + (todo - 1) / dup * inc
              call incutils_numeric_string (i2,end)
              favor_end = .false.
            endif
          endif
        else                                              ! favor todo - set end
          if (present(maxdo)) then                        ! set end
            if (todo .gt. maxdo) todo = maxdo
          endif
          i2 = i1 + (todo - 1) / dup * inc
          call incutils_numeric_string (i2,end)
        endif
      endif

      return
      end subroutine incutils_dup_trap


!!------------------------------- inc_trap --------------------------------!!
!!------------------------------- inc_trap --------------------------------!!
!!------------------------------- inc_trap --------------------------------!!


      subroutine incutils_inc_trap (start,end,dup,inc,cycles,todo,  &
                                     favor_end,maxdo)
      implicit none
      character(len=*),intent(inout)      :: start                    ! argument
      character(len=*),intent(inout)      :: end                      ! argument
      integer         ,intent(in)         :: dup                      ! argument
      integer         ,intent(in)         :: inc                      ! argument
      integer         ,intent(in)         :: cycles                   ! argument
      integer         ,intent(inout)      :: todo                     ! argument
      logical         ,intent(inout)      :: favor_end                ! argument
      integer         ,optional           :: maxdo                    ! argument

      integer                             :: i1                       ! local
      integer                             :: i2                       ! local
      integer                             :: itemp                    ! local
      integer                             :: istat                    ! local


      call string_cc2ii(start,i1,istat)
      if (istat .eq. 1) then                              ! start was an integer
        if (favor_end) then                               ! favor end - set todo
          call string_cc2ii(end,i2,istat)
          if (istat .eq. 1) then                          ! end was an integer
            itemp = todo                                  ! set todo
            todo = (i2 - i1) / inc * dup + 1
            if (present(maxdo)) then
              if (todo .gt. maxdo) then
                todo = maxdo
                i2 = i1 + (todo - 1) / dup * inc
                call string_ii2cc (i2,end)
                favor_end = .false.
              endif
            endif
            if (todo .lt. 1) then                         ! gives bad todo
              todo = itemp                                ! must set end
              i2 = i1 + (todo - 1) / dup * inc
              call string_ii2cc (i2,end)
              favor_end = .false.
            endif
          else                                            ! end was alpha
            if (present(maxdo)) then                      ! must set end
              if (todo .gt. maxdo) todo = maxdo
            endif
            i2 = i1 + (todo - 1) / dup * inc
            call string_ii2cc(i2,end)
            favor_end = .false.
          endif
        else                                              ! favor todo - set end
          if (present(maxdo)) then                        ! set end
            if (todo .gt. maxdo) todo = maxdo
          endif
          i2 = i1 + (todo - 1) / dup * inc
          call string_ii2cc(i2,end)
        endif

      else                                                ! start was alpha
        call incutils_string_numeric (start,i1)
        if (favor_end) then                               ! favor end - set todo
          call string_cc2ii(end,i2,istat)
          if (istat .eq. 1) then                          ! end was an integer
            if (present(maxdo)) then                      ! must set end
              if (todo .gt. maxdo) todo = maxdo
            endif
            i2 = i1 + (todo - 1) / dup * inc
            call incutils_numeric_string (i2,end)
            favor_end = .false.
          else                                            ! end was alpha
            call incutils_string_numeric (end,i2)        ! set todo
            todo = (i2 - i1) / inc * dup + 1
            if (present(maxdo)) then
              if (todo .gt. maxdo) then
                todo = maxdo
                i2 = i1 + (todo - 1) / dup * inc
                call incutils_numeric_string (i2,end)
                favor_end = .false.
              endif
            endif
            if (todo .lt. 1) then                       ! gives bad todo
              todo = itemp                              ! must set end
              if (present(maxdo)) then
                if (todo .gt. maxdo) todo = maxdo
              endif
              i2 = i1 + (todo - 1) / dup * inc
              call incutils_numeric_string (i2,end)
              favor_end = .false.
            endif
          endif
        else                                              ! favor todo - set end
          if (present(maxdo)) then                        ! set end
            if (todo .gt. maxdo) todo = maxdo
          endif
          i2 = i1 + (todo - 1) / dup * inc
          call incutils_numeric_string (i2,end)
        endif
      endif

      return
      end subroutine incutils_inc_trap


!!----------------------------- cycles_trap -------------------------------!!
!!----------------------------- cycles_trap -------------------------------!!
!!----------------------------- cycles_trap -------------------------------!!


      subroutine incutils_cycles_trap (start,end,dup,inc,cycles,todo,  &
                                     favor_end,maxdo)
      implicit none
      character(len=*),intent(inout)      :: start                    ! argument
      character(len=*),intent(inout)      :: end                      ! argument
      integer         ,intent(in)         :: dup                      ! argument
      integer         ,intent(in)         :: inc                      ! argument
      integer         ,intent(in)         :: cycles                   ! argument
      integer         ,intent(inout)      :: todo                     ! argument
      logical         ,intent(inout)      :: favor_end                ! argument
      integer         ,optional           :: maxdo                    ! argument

      integer                             :: i1                       ! local
      integer                             :: i2                       ! local
      integer                             :: itemp                    ! local
      integer                             :: istat                    ! local


      call string_cc2ii(start,i1,istat)
      if (istat .eq. 1) then                              ! start was an integer
        if (favor_end) then                               ! favor end - set todo
          call string_cc2ii(end,i2,istat)
          if (istat .eq. 1) then                          ! end was an integer
            itemp = todo                                  ! set todo
            todo = (i2 - i1) / inc * dup + 1
            if (present(maxdo)) then
              if (todo .gt. maxdo) then
                todo = maxdo
                i2 = i1 + (todo - 1) / dup * inc
                call string_ii2cc (i2,end)
                favor_end = .false.
              endif
            endif
            if (todo .lt. 1) then                         ! gives bad todo
              todo = itemp                                ! must set end
              i2 = i1 + (todo - 1) / dup * inc
              call string_ii2cc (i2,end)
              favor_end = .false.
            endif
          else                                            ! end was alpha
            if (present(maxdo)) then                      ! must set end
              if (todo .gt. maxdo) todo = maxdo
            endif
            i2 = i1 + (todo - 1) / dup * inc
            call string_ii2cc(i2,end)
            favor_end = .false.
          endif
        else                                              ! favor todo - set end
          if (present(maxdo)) then                        ! set end
            if (todo .gt. maxdo) todo = maxdo
          endif
          i2 = i1 + (todo - 1) / dup * inc
          call string_ii2cc(i2,end)
        endif

      else                                                ! start was alpha
        call incutils_string_numeric (start,i1)
        if (favor_end) then                               ! favor end - set todo
          call string_cc2ii(end,i2,istat)
          if (istat .eq. 1) then                          ! end was an integer
            if (present(maxdo)) then                      ! must set end
              if (todo .gt. maxdo) todo = maxdo
            endif
            i2 = i1 + (todo - 1) / dup * inc
            call incutils_numeric_string (i2,end)
            favor_end = .false.
          else                                            ! end was alpha
            call incutils_string_numeric (end,i2)        ! set todo
            todo = (i2 - i1) / inc * dup + 1
            if (present(maxdo)) then
              if (todo .gt. maxdo) then
                todo = maxdo
                i2 = i1 + (todo - 1) / dup * inc
                call incutils_numeric_string (i2,end)
                favor_end = .false.
              endif
            endif
            if (todo .lt. 1) then                       ! gives bad todo
              todo = itemp                              ! must set end
              if (present(maxdo)) then
                if (todo .gt. maxdo) todo = maxdo
              endif
              i2 = i1 + (todo - 1) / dup * inc
              call incutils_numeric_string (i2,end)
              favor_end = .false.
            endif
          endif
        else                                              ! favor todo - set end
          if (present(maxdo)) then                        ! set end
            if (todo .gt. maxdo) todo = maxdo
          endif
          i2 = i1 + (todo - 1) / dup * inc
          call incutils_numeric_string (i2,end)
        endif
      endif

      return
      end subroutine incutils_cycles_trap


!!------------------------------- end_trap --------------------------------!!
!!------------------------------- end_trap --------------------------------!!
!!------------------------------- end_trap --------------------------------!!


      subroutine incutils_end_trap (start,end,dup,inc,cycles,todo,  &
                                     favor_end,maxdo)
      implicit none
      character(len=*),intent(inout)      :: start                    ! argument
      character(len=*),intent(inout)      :: end                      ! argument
      integer         ,intent(in)         :: dup                      ! argument
      integer         ,intent(in)         :: inc                      ! argument
      integer         ,intent(in)         :: cycles                   ! argument
      integer         ,intent(inout)      :: todo                     ! argument
      logical         ,intent(inout)      :: favor_end                ! argument
      integer         ,optional           :: maxdo                    ! argument

      integer                             :: i                        ! local
      integer                             :: j                        ! local
      integer                             :: i1                       ! local
      integer                             :: i2                       ! local
      integer                             :: istat                    ! local
      integer                             :: nchars                   ! local


      call string_cc2ii(start,i1,istat)
      if (istat .eq. 1) then                              ! start was an integer
        call string_cc2ii(end,i2,istat)
        if (istat .eq. 1) then                            ! end was an integer
          if (inc .ne. 0) then                            ! set todo
            todo = (i2 - i1) / inc * dup + 1
            favor_end = .true.
            if (present(maxdo)) then
              if (todo .gt. maxdo) then
                todo = maxdo
                i2 = i1 + (todo - 1) / dup * inc
                call string_ii2cc (i2,end)
                favor_end = .false.
              endif
            endif
          else
            call pc_error ('Increment must be non-zero')
          endif
        else                                              ! end was alpha
          call pc_error ('End must be alpha')
          return
        endif

      else                                                ! start was alpha
        call incutils_string_numeric (start,i1)
        call string_cc2ii(end,i2,istat)
        if (istat .eq. 1) then                            ! start was an integer
          call pc_error ('End must be alpha')
          return
        else                                              ! start was alpha
          call incutils_string_numeric (end,i2)          ! set todo
          todo = (i2 - i1) / inc * dup + 1
          favor_end = .true.
          if (present(maxdo)) then
            if (todo .gt. maxdo) then
              todo = maxdo
              i2 = i1 + (todo - 1) / dup * inc
              call incutils_numeric_string (i2,end)
              favor_end = .false.
            endif
          endif
        endif
        call incutils_numeric_nchars (i2,nchars)
        j = nchars - len_trim(start)
        if (j .gt. 0) then
          do i=1,j
            start = carray(1) // start
          enddo
        endif

      endif

      return
      end subroutine incutils_end_trap


!!------------------------------ todo_trap --------------------------------!!
!!------------------------------ todo_trap --------------------------------!!
!!------------------------------ todo_trap --------------------------------!!


      subroutine incutils_todo_trap (start,end,dup,inc,cycles,todo,  &
                                      favor_end,maxdo)
      implicit none
      character(len=*),intent(inout)      :: start                    ! argument
      character(len=*),intent(inout)      :: end                      ! argument
      integer         ,intent(in)         :: dup                      ! argument
      integer         ,intent(in)         :: inc                      ! argument
      integer         ,intent(in)         :: cycles                   ! argument
      integer         ,intent(inout)      :: todo                     ! argument
      logical         ,intent(inout)      :: favor_end                ! argument
      integer         ,optional           :: maxdo                    ! argument

      integer                             :: i                        ! local
      integer                             :: j                        ! local
      integer                             :: i1                       ! local
      integer                             :: i2                       ! local
      integer                             :: istat                    ! local
      integer                             :: nchars                   ! local

      if (present(maxdo)) then
        if (todo .gt. maxdo) todo = maxdo
      endif

      call string_cc2ii(start,i1,istat)
      if (istat .eq. 1) then                              ! start was an integer
        if (inc .ne. 0) then
          i2 = i1 + (todo - 1) / dup * inc
          call string_ii2cc (i2,end)
        else
          call pc_error ('Increment must be non-zero')
        endif

      else                                                ! start was alpha
        call incutils_string_numeric (start,i1)
        i2 = i1 + (todo - 1) / dup * inc
        call incutils_numeric_string (i2,end)

        call incutils_numeric_nchars (i2,nchars)
        j = nchars - len_trim(start)
        if (j .gt. 0) then
          do i=1,j
            start = carray(1) // start
          enddo
        endif
      endif
      favor_end = .false.

      return
      end subroutine incutils_todo_trap


!!---------------------------- numeric_nchars -----------------------------!!
!!---------------------------- numeric_nchars -----------------------------!!
!!---------------------------- numeric_nchars -----------------------------!!


      subroutine incutils_numeric_nchars (value,nchars)
      implicit none
      integer         ,intent(in)         :: value                    ! argument
      integer         ,intent(out)        :: nchars                   ! argument

      integer                             :: k                        ! local


      nchars = 1
      k = value
      do
        k = k/26
        if (k .gt. 0) then
          nchars = nchars + 1
        else
          exit
        endif
      enddo

      return
      end subroutine incutils_numeric_nchars


!!---------------------------- string_numeric -----------------------------!!
!!---------------------------- string_numeric -----------------------------!!
!!---------------------------- string_numeric -----------------------------!!


      subroutine incutils_string_numeric (string,value)
      implicit none
      character(len=*),intent(inout)      :: string                   ! argument
      integer         ,intent(out)        :: value                    ! argument

      integer                             :: i                        ! local
      integer                             :: i1                       ! local
      integer                             :: k                        ! local
      integer                             :: nchars                   ! local
      integer                             :: shift_factor             ! local

      nchars = len_trim(string)
      i1 = ichar(string(nchars:nchars))
      if (i1.ge.65 .and. i1.le.90) then                 ! uppercase
        shift_factor = 64
        i1 = i1 - 64
        call string_to_upper(string)
        do i=1,nchars                                   ! only A-Z allowed
          if (string(i:i) .lt. 'A') then
            string(i:i) = 'A'
          else if (string(i:i) .gt. 'Z') then
            string(i:i) = 'Z'
          endif
        enddo
        do i=1,26
          carray(i) = char(64+i)
        enddo
      else                                              ! lowercase
        shift_factor = 96
        i1 = i1 - 96
        call string_to_lower(string)
        do i=1,nchars                                   ! only a-z allowed
          if (string(i:i) .lt. 'a') then
            string(i:i) = 'a'
          else if (string(i:i) .gt. 'z') then
            string(i:i) = 'z'
          endif
        enddo
        do i=1,26
          carray(i) = char(96+i)
        enddo
      endif

      value = 0
      do i=nchars,1,-1
        k = ichar(string(i:i)) - shift_factor
        value = value + 26**(nchars-i)*(k-1)
      enddo

      return
      end subroutine incutils_string_numeric


!!---------------------------- numeric_string -----------------------------!!
!!---------------------------- numeric_string -----------------------------!!
!!---------------------------- numeric_string -----------------------------!!


      subroutine incutils_numeric_string (value,string)
      implicit none
      integer         ,intent(in)         :: value                    ! argument
      character(len=*),intent(out)        :: string                   ! argument

      integer                             :: j,temp                   ! local


      string = ' '
      j      = value
      temp   = len(string)
      call  incutils_load_string (string,temp,j)

      string = adjustl(string)

      return
      end subroutine incutils_numeric_string


!!----------------------------- load_string -------------------------------!!
!!----------------------------- load_string -------------------------------!!
!!----------------------------- load_string -------------------------------!!


      recursive subroutine incutils_load_string (ctemp,indx,j)
      implicit none
      character(len=*),intent(inout)      :: ctemp                    ! argument
      integer         ,intent(in)         :: indx                     ! argument
      integer         ,intent(in)         :: j                        ! argument

      integer                             :: i                        ! local
      integer                             :: k                        ! local

      select case (j)

        case (0)

        case (1:)
          k = mod(j,26) + 1
          ctemp(indx:indx) = carray(k)
          k = j/26
          i = indx - 1
          call incutils_load_string (ctemp,i,k)

        case default

      end select


      return
      end subroutine incutils_load_string


!!----------------------------- expand_mask -------------------------------!!
!!----------------------------- expand_mask -------------------------------!!
!!----------------------------- expand_mask -------------------------------!!


      subroutine incutils_expand_mask (mask_in,mask_out)
      implicit none
      character(len=*),intent(in)         :: mask_in                  ! argument
      character(len=*),intent(out)        :: mask_out                 ! argument

      integer                           ::  i                          !local
      integer                           ::  j                          !local
      integer                           ::  i2                         !local
      integer                           ::  nctemp2                    !local
      character(len=PC_LENGTH)          ::  ctemp                      !local
      character(len=PC_LENGTH)          ::  ctemp2                     !local
      logical                           ::  expand_next                !local

      mask_out = mask_in
      call string_strip_blanks (mask_out)
      if (scan(mask_out,'^$*') .eq. 0) then
        i2 = len_trim(mask_out) + 1
        mask_out(i2:i2) = '*'
      endif
      
      if (scan(mask_out,'0123456789') .gt. 0) then
        ctemp       = mask_out
        i2          = 1
        ctemp2      = ""
        nctemp2     = 0
        expand_next = .false.
        do i=1,len_trim(ctemp)
          if (string_is_digit(ctemp(i:i))) then
            nctemp2 = nctemp2 + 1
            ctemp2(nctemp2:nctemp2) = ctemp(i:i)
            expand_next = .true.
          else if (expand_next) then
            call string_cc2ii (ctemp2,nctemp2)
            do j=1,nctemp2
              mask_out(i2:i2) = ctemp(i:i)
              i2 = i2 + 1
            enddo
            ctemp2      = ""
            nctemp2     = 0
            expand_next = .false.
          else
            mask_out(i2:i2) = ctemp(i:i)
            i2 = i2 + 1
          endif
        enddo
      endif

      return
      end subroutine incutils_expand_mask


!!------------------------------ selection --------------------------------!!
!!------------------------------ selection --------------------------------!!
!!------------------------------ selection --------------------------------!!


      subroutine incutils_selection (root,mask,start,dup,inc,cycles,todo,  &
                                      array,narray,astart,ainc,atodo)
      implicit none
      character(len=*),intent(in)         :: root                     ! argument
      character(len=*),intent(in)         :: mask                     ! argument
      character(len=*),intent(inout)      :: start                    ! argument
      integer         ,intent(in)         :: dup                      ! argument
      integer         ,intent(in)         :: inc                      ! argument
      integer         ,intent(in)         :: cycles                   ! argument
      integer         ,intent(in)         :: todo                     ! argument
      character(len=*),pointer            :: array(:)                 ! argument
      integer         ,intent(in)         :: narray                   ! argument
      integer         ,intent(in)         :: astart                   ! argument
      integer         ,intent(in)         :: ainc                     ! argument
      integer         ,intent(in)         :: atodo                    ! argument

      integer                             :: i                        ! local
      integer                             :: j                        ! local
      integer                             :: k                        ! local
      integer                             :: ii                       ! local
      integer                             :: kk                       ! local
      integer                             :: i1                       ! local
      integer                             :: i2                       ! local
      integer                             :: icnt                     ! local
      integer                             :: idup                     ! local
      integer                             :: nchars                   ! local
      integer                             :: ido                      ! local
      integer                             :: nzeros                   ! local
      character(len=PC_LENGTH)            :: local_mask               ! local
      character(len=PC_LENGTH)            :: ctemp                    ! local
      character(len=PC_LENGTH)            :: ctemp2                   ! local
      character(len=PC_LENGTH)            :: cstart                   ! local
      integer                             :: nfmt                     ! local
      character(len=PC_LENGTH)            :: fmt                      ! local
      integer                             :: istat                    ! local


      call incutils_expand_mask (mask,local_mask)

      call string_cc2ii(start,i1,istat)
      if (istat .eq. 1) then                              ! start was an integer
        nchars = len_trim(start)
        nzeros = 0
        if (nchars .gt. 1) then
          do i=1,nchars
            if (start(i:i) .ne. '0') exit
            nzeros = nzeros + 1
          enddo
        endif
        if (nzeros .eq. 0) then
          ido = astart
          do k=1,cycles
            icnt = 0
            idup = dup
            do i=1,todo
              if (ido .gt. min(astart+(atodo-1)*ainc,narray)) exit
              ctemp = array(ido)
              select case (trim(local_mask))
                case ('^')
                  call string_ii2cc (i1+icnt,ctemp2)
                  array(ido) = trim(root) // trim(ctemp) // trim(ctemp2)
                case ('$')
                  call string_ii2cc (i1+icnt,ctemp2)
                  array(ido) = trim(root) // trim(ctemp2) // trim(ctemp)
                case default
                  i2 = 1
                  do j=1,len_trim(local_mask)
                    select case (local_mask(j:j))
                      case ('#')
                        array(ido)(i2:) = ctemp(j:j)
                      case ('*')
                        call string_ii2cc (i1+icnt,ctemp2)
                        array(ido)(i2:) = trim(root) // trim(ctemp2)
                      case ('&')
                        call string_ii2cc (i1+icnt,ctemp2)
                        array(ido)(i2:i2) = trim(root) // trim(ctemp2)
                      case default
                    end select
                    i2 = len_trim(array(ido)) + 1
                  enddo
              end select
              idup = idup - 1
              if (idup .eq. 0) then
                idup = dup
                icnt = icnt + inc
              endif
              ido = ido + ainc
            enddo
            if (ido .gt. min(astart+(atodo-1)*ainc,narray)) exit
          enddo
        else
          call string_ii2cc (i1+(todo-1)/dup*inc,ctemp2)
          nchars = max(len_trim(start),len_trim(ctemp2))
          write (fmt,'(A,I10,A,I10,A)') '(I',nchars,'.',nchars,')'
          call string_strip_blanks (fmt,nfmt)
          ido = astart
          do k=1,cycles
            icnt = 0
            idup = dup
            do i=1,todo
              if (ido .gt. min(astart+(atodo-1)*ainc,narray)) exit
              ctemp = array(ido)
              select case (trim(local_mask))
                case ('^')
                  write (ctemp2,fmt(1:nfmt)) i1+icnt
                  array(ido) = trim(root) // trim(ctemp) // trim(ctemp2)
                case ('$')
                  write (ctemp2,fmt(1:nfmt)) i1+icnt
                  array(ido) = trim(root) // trim(ctemp2) // trim(ctemp)
                case default
                  i2 = 1
                  do j=1,len_trim(local_mask)
                    select case (local_mask(j:j))
                      case ('#')
                        array(ido)(i2:) = ctemp(j:j)
                      case ('*')
                        write (ctemp2,fmt(1:nfmt)) i1+icnt
                        array(ido)(i2:) = trim(root) // trim(ctemp2)
                      case ('&')
                        write (ctemp2,fmt(1:nfmt)) i1+icnt
                        array(ido)(i2:i2) = trim(root) // trim(ctemp2)
                      case default
                    end select
                    i2 = len_trim(array(ido)) + 1
                  enddo
              end select
              idup = idup - 1
              if (idup .eq. 0) then
                idup = dup
                icnt = icnt + inc
              endif
              ido = ido + ainc
            enddo
            if (ido .gt. min(astart+(atodo-1)*ainc,narray)) exit
          enddo
        endif
      else                                                ! start was alpha
        call incutils_string_numeric (start,i1)
        i2 = i1 + (todo - 1) / dup * inc
        call incutils_numeric_nchars (i2,nchars)
        icnt = 0
        call incutils_numeric_string (i1+icnt,cstart)
        kk = nchars - len_trim(cstart)
        if (kk .gt. 0) then
          do ii=1,kk
            cstart = carray(1) // cstart
          enddo
        endif
        ido = astart
        do k=1,cycles
          icnt   = 0
          idup   = dup
          ctemp2 = cstart
          do i=1,todo
            if (ido .gt. min(astart+(atodo-1)*ainc,narray)) exit
            ctemp = array(ido)
            select case (trim(local_mask))
              case ('^')
                array(ido) = trim(root) // trim(ctemp) // trim(ctemp2)
              case ('$')
                array(ido) = trim(root) // trim(ctemp2) // trim(ctemp)
              case default
                i2 = 1
                do j=1,len_trim(local_mask)
                  select case (local_mask(j:j))
                    case ('#')
                      array(ido)(i2:) = ctemp(j:j)
                    case ('*')
                      array(ido)(i2:) = trim(root) // trim(ctemp2)
                    case ('&')
                      array(ido)(i2:i2) = trim(root) // trim(ctemp2)
                    case default
                  end select
                  i2 = len_trim(array(ido)) + 1
                enddo
            end select
            idup = idup - 1
            if (idup .eq. 0) then
              idup = dup
              icnt = icnt + inc
              call incutils_numeric_string (i1+icnt,ctemp2)
              kk = nchars - len_trim(ctemp2)
              if (kk .gt. 0) then
                do ii=1,kk
                  ctemp2 = carray(1) // ctemp2
                enddo
              endif
            endif
            ido = ido + ainc
          enddo
          if (ido .gt. min(astart+(atodo-1)*ainc,narray)) exit
        enddo
      endif

      return
      end subroutine incutils_selection


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module incutils_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

