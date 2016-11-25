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
!!------------------------------- ftimer.f90 --------------------------------!!
!!------------------------------- ftimer.f90 --------------------------------!!
!!------------------------------- ftimer.f90 --------------------------------!!


module ftimer_module
implicit none

type :: ftimer_struct              
    character(len=40) :: label
    double precision  :: start
    double precision  :: stop
    double precision  :: sum
    integer           :: nsum
end type ftimer_struct

contains

!!------------------------------ create ----------------------------------!!
!!------------------------------ create ----------------------------------!!
!!------------------------------ create ----------------------------------!!

  subroutine ftimer_create (obj, label)
    type(ftimer_struct),pointer       :: obj
    character(len=*)   ,intent(in)    :: label
    allocate (obj)
    obj%label = label
    call ftimer_clear (obj)
  end subroutine ftimer_create

!!------------------------------ clear ----------------------------------!!
!!------------------------------ clear ----------------------------------!!
!!------------------------------ clear ----------------------------------!!

  subroutine ftimer_clear (obj)
    type(ftimer_struct),intent(inout)  :: obj
    obj%start          = 0.0d0
    obj%stop           = 0.0d0
    obj%sum            = 0.0d0
    obj%nsum           = 0
  end subroutine ftimer_clear

!!---------------------------- start ------------------------------!!
!!---------------------------- start ------------------------------!!
!!---------------------------- start ------------------------------!!

  subroutine ftimer_start (obj)
    type(ftimer_struct),intent(inout)  :: obj

    call ftimer_crou_wtime (obj%start)

  end subroutine ftimer_start


!!---------------------------- stop -------------------------------!!
!!---------------------------- stop -------------------------------!!
!!---------------------------- stop -------------------------------!!

  subroutine ftimer_stop (obj)
    type(ftimer_struct),intent(inout)  :: obj

    call ftimer_crou_wtime (obj%stop)
    obj%sum = obj%sum + obj%stop - obj%start
    obj%nsum = obj%nsum + 1

  end subroutine ftimer_stop


  !!---------------------------- print -----------------------------!!
  !!---------------------------- print -----------------------------!!
  !!---------------------------- print -----------------------------!!

  subroutine ftimer_print (obj)
    type(ftimer_struct),intent(inout)  :: obj
    double precision                   :: average

    average = 0.0d0
    if (obj%nsum > 0) average = obj%sum / obj%nsum

    print *, trim(obj%label), '  sum     = ', obj%sum, ' seconds'
    print *, trim(obj%label), '  average = ', average, ' seconds'
    print *, trim(obj%label), '  count   = ', obj%nsum
    call ftimer_crou_print

  end subroutine ftimer_print


!!----------------------------- delete ------------------------------!!
!!----------------------------- delete ------------------------------!!
!!----------------------------- delete ------------------------------!!


  subroutine ftimer_delete (obj)
    type(ftimer_struct),pointer        :: obj

    if (associated(obj)) deallocate (obj)

  end subroutine ftimer_delete


!!---------------------------- end of module ----------------------------!!
!!---------------------------- end of module ----------------------------!!
!!---------------------------- end of module ----------------------------!!


end module ftimer_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

