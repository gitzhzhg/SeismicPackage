!<CPS_v1 type="PRIMITIVE"/>
!!------------------------ triplemerge.f90 ------------------------------!!
!!------------------------ triplemerge.f90 ------------------------------!!
!!------------------------ triplemerge.f90 ------------------------------!!


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
! Name       : TRIPLEMERGE
! Category   : sorts
! Written    : 2002-08-05   by: Tom Stoeckley
! Revised    : 2002-08-20   by: Tom Stoeckley
! Maturity   : production   2002-08-26
! Purpose    : Sort and merge a list of TRIPLESORT integer structures on disk.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive uses the TRIPLESORT primitive to save a list of
! TRIPLESORT integer structures to disk, and then to sort them is groups
! and merge them into a sorted list on disk.  This primitive is useful
! if the user does not know in advance how many integer structures there
! will be, or if the number is too large to store in memory all at once.
!
! This primitive can be particularly useful for storing and sorting
! trace header information, which might consist of a primary sort key,
! a secondary sort key, and a record number of the trace on a disk file.
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
!                                   o    o
!          call triplemerge_open  (obj,error)
!
!                                   b  i  i  i    o
!          call triplemerge_write (obj,I1,I2,I3,error)
!
!                                   b    o     o
!          call triplemerge_weof  (obj,kount,error)
!
!                                   b     i       o
!          call triplemerge_sort  (obj,lunprint,error)
!
!                                   b  o  o  o    o
!          call triplemerge_read  (obj,I1,I2,I3,error)
!
!                                   b
!          call triplemerge_close (obj)
!
!
! type(triplemerge_struct) obj = pointer to the data structure.
! logical                error = error (true or false).
! integer                   i1 = primary sort key.
! integer                   i2 = secondary sort key.
! integer                   i3 = tertiary sort key.
! integer                kount = total number of keys written to disk.
! integer             lunprint = logical unit number for printing (or 0).
!
! Take the following steps to use this primitive:
!
!  (1) Open the file.
!  (2) Write a triplet of keys for each key to be saved.
!  (3) Retrieve the count of keys which have been written.
!  (4) Optionally sort the keys.
!  (5) Read a triplet of keys for each of the KOUNT keys which had been
!       written.  The keys will be returned in the original order if they
!       were not sorted, or in the sorted order if they were sorted.
!  (6) Close the file.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2002-08-26  Stoeckley  Fix bugs where error flag was not set properly;
!                             simplify logic considerably; improve error
!                             printouts.
!  1. 2002-08-05  Stoeckley  Initial version.
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


      module triplemerge_module
      use triplesort_module
      use getlun_module
      implicit none
      public


      character(len=100),public,save :: TRIPLEMERGE_IDENT = &
'$Id: triplemerge.f90,v 1.2 2002/08/21 16:19:15 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: triplemerge_struct              
 
        private
        integer                       :: lun
        integer                       :: irec
        integer                       :: kount
        type(triplesort_ints),pointer :: buffer(:)

      end type triplemerge_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      contains


!!------------------------------ open --------------------------------------!!
!!------------------------------ open --------------------------------------!!
!!------------------------------ open --------------------------------------!!


      SUBROUTINE triplemerge_open (obj,error)
      type(triplemerge_struct),pointer       :: obj            ! arguments
      logical                 ,intent(out)   :: error          ! arguments
      integer                                :: ier            ! local

      allocate (obj)
      nullify (obj%buffer)
      call getlun (obj%lun)
      open (obj%lun,form='UNFORMATTED',status='SCRATCH',iostat=ier)
      obj%irec  = 0
      obj%kount = 0
      error     = (ier /= 0)
      end subroutine triplemerge_open


!!------------------------------ write ------------------------------------!!
!!------------------------------ write ------------------------------------!!
!!------------------------------ write ------------------------------------!!


      subroutine triplemerge_write (obj,I1,I2,I3,error)
      type(triplemerge_struct),intent(inout) :: obj          ! arguments
      integer                 ,intent(in)    :: i1,i2,i3     ! arguments
      logical                 ,intent(out)   :: error        ! arguments
      integer                                :: ier          ! local
   
      obj%kount = obj%kount + 1
      write (obj%lun,iostat=ier) i1,i2,i3
      error = (ier /= 0)
      end subroutine triplemerge_write


!!------------------------------ read ------------------------------------!!
!!------------------------------ read ------------------------------------!!
!!------------------------------ read ------------------------------------!!


      subroutine triplemerge_read (obj,   I1,I2,I3,error)
      type(triplemerge_struct),intent(inout) :: obj          ! arguments
      integer                 ,intent(out)   :: i1,i2,i3     ! arguments
      logical                 ,intent(out)   :: error        ! arguments

      obj%irec = obj%irec + 1
      if (obj%irec <= obj%kount) then
           I1 = obj%buffer(obj%irec)%primary
           I2 = obj%buffer(obj%irec)%secondary
           I3 = obj%buffer(obj%irec)%tertiary
           error = .false.
      else
           I1 = 0
           I2 = 0
           I3 = 0
           error = .true.
      end if
      end subroutine triplemerge_read


!!------------------------------ close ------------------------------------!!
!!------------------------------ close ------------------------------------!!
!!------------------------------ close ------------------------------------!!


      subroutine triplemerge_close (obj)
      type(triplemerge_struct),pointer :: obj              ! arguments

      if (.not.associated(obj)) return
      if (obj%lun > 0) close (obj%lun,status='DELETE')
      if (associated (obj%buffer)) deallocate (obj%buffer)
      deallocate(obj)
      end subroutine triplemerge_close


!!---------------------------  sort -------------------------------!!
!!---------------------------  sort -------------------------------!!
!!---------------------------  sort -------------------------------!!


      subroutine triplemerge_sort (obj,lunprint,kount,error)
      type(triplemerge_struct),intent(inout) :: obj                ! arguments
      integer                 ,intent(in)    :: lunprint           ! arguments
      integer                 ,intent(out)   :: kount              ! arguments
      logical                 ,intent(out)   :: error              ! arguments
      integer                                :: ier,indx,i1,i2,i3  ! local

!----------allocate memory.

      kount = obj%kount

      if (obj%kount == 0) then
           if (lunprint > 0) then
                write(lunprint,*) 'TRIPLEMERGE: NOTHING TO SORT'
           end if
           error = .true.
           return
      end if

      allocate (obj%buffer(obj%kount), stat=ier)
      if (ier /= 0) then
           if (lunprint > 0) then
                write(lunprint,*) &
                 'TRIPLEMERGE: ERROR ALLOCATING ',obj%kount,' TRIPLESORT WORDS'
           end if
           error = .true.
           return
      end if

!----------fill memory from file and close file.

      rewind obj%lun
      do indx = 1,obj%kount
           read (obj%lun,iostat=ier) i1,i2,i3
           if (ier /= 0) then
                if (lunprint > 0) then
                     write(lunprint,*) 'TRIPLEMERGE: ERROR READING RECORD ',indx
                end if
                error = .true.
                return
           end if
           obj%buffer(indx)%primary   = i1
           obj%buffer(indx)%secondary = i2
           obj%buffer(indx)%tertiary  = i3
      end do
      close (obj%lun,status='DELETE')
      obj%lun = 0

!----------sort memory.

      call triplesort_sort (obj%buffer,obj%kount)
      error = .false.
      end subroutine triplemerge_sort


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module triplemerge_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

