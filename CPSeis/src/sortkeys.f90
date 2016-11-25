!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- sortkeys.f90 --------------------------------!!
!!--------------------------- sortkeys.f90 --------------------------------!!
!!--------------------------- sortkeys.f90 --------------------------------!!


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
! Name       : SORTKEYS 
! Category   : sorts
! Written    : 2000-06-16   by: Tom Stoeckley
! Revised    : 2002-10-23   by: Tom Stoeckley
! Maturity   : production   2003-06-16
! Purpose    : Manage and sort an array of keys.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive manages an array of keys, and sorts this array by various
! keys.
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
!                                o     i       i     i      o
!    call sortkeys_create      (obj,nrecords,nkeys,nsorts,error)
!
!                                b
!    call sortkeys_delete      (obj)
!
!                                           opt  opt  opt  opt  opt  opt
!                                b     i     i    i    i    i    i    i
!    call sortkeys_set_keys    (obj,irecord,key1,key2,key3,key4,key5,key6)
!
!                                          opt   opt   opt   opt   opt   opt
!                                b    i     i     i     i     i     i     i
!    call sortkeys_define_sort (obj,isort,ikey1,ikey2,ikey3,ikey4,ikey5,ikey6)
!
!                                b    i
!    call sortkeys_select_sort (obj,isort)
!
!
!    irecord = sortkeys_get_record          (obj,indx)
!    key     = sortkeys_get_original_key    (obj,indx,ikey)
!    key     = sortkeys_get_prioritized_key (obj,indx,ipri)
!     o                                       i    i   i
!
!
! type(sortkeys_struct) obj        = pointer to the SORTKEYS structure.
! integer               nrecords   = number of records.
! integer               nkeys      = number of keys per record.
! integer               nsorts     = number of different sorts to allow.
! logical               error      = error flag (true if error occurs).
!
! integer  irecord                 = record number in original sort order.
! integer  indx                    = record number in current sort order.
! integer  isort                   = sort number (sort type identifier).
! integer  ikey                    = key index in original order.
! integer  ipri                    = key index in current prioritized order.
! integer  key1,key2,key3,...      = list of keys.
! integer  ikey1,ikey2,ikey3,...   = list of key indices in order of priority.
!
! SORTKEYS_CREATE:
!  (1) NRECORDS must be >= 1.
!  (2) NKEYS    must be >= 1.
!  (3) NSORTS   must be >= 0.
!  (4) If NSORTS is zero, no sorting will be allowed.
!
! SORTKEYS_SET_KEYS:
!  (1) Should be called for each record right after creating the object.
!  (2) Default value of keys is zero.
!  (3) IRECORD must be >= 1 and <= NRECORDS.
!
! SORTKEYS_DEFINE_SORT:
!  (1) Should be called for each sort order desired.
!  (2) Does an n-log-n sort of the keys in place.
!  (3) Sorts the array of keys using the specified order of priority.
!  (4) Default value of key indices is one.
!  (5) ISORT must be >= 1 and <= NSORTS.
!
! SORTKEYS_SELECT_SORT:
!  (1) Should be called to select the current sort order.
!  (2) ISORT must be >= 1 and <= NSORTS to select a sorted order.
!  (3) ISORT must be == 0 to select the original unsorted order.
!
! SORTKEYS_GET_RECORD:
!  (1) Returns the original record number for the index in current sort order.
!  (2) INDX must be >= 1 and <= NRECORDS.
!  (3) Returns the input INDX if the original unsorted order was selected.
!
! SORTKEYS_GET_ORIGINAL_KEY:
!  (1) Returns the specified key for the index in current sort order.
!  (2) INDX must be >= 1 and <= NRECORDS.
!  (3) IKEY must be >= 1 and <= NKEYS.
!  (4) IKEY is the index of the key in the order listed in SORTKEYS_SET_KEYS.
!  (5) INDX is the original record order if the original unsorted order was
!        selected.
!
! SORTKEYS_GET_PRIORITIZED_KEY:
!  (1) Returns the specified key for the index in current sort order.
!  (2) INDX must be >= 1 and <= NRECORDS.
!  (3) IPRI must be >= 1 and <= NKEYS.
!  (4) IPRI is the index of the key in the priority order defined in
!        SORTKEYS_DEFINE_KEYS.
!  (5) Is identical to SORTKEYS_GET_ORIGINAL_KEY if the original unsorted
!        order was selected.
!
!-------------------------------------------------------------------------------
!                               EXAMPLE
!
!                                       nrecords  nkeys  nsorts
!                                          |        |      |
!         call sortkeys_create      (obj,  5,       3,     4, error)
!
!                                      irecord  key1   key2   key3   key4
!                                         |      |      |      |      |
!         call sortkeys_set_keys    (obj, 1,   133.3, 155.5, 177.7       )
!         call sortkeys_set_keys    (obj, 2,   233.3, 255.5, 277.7       )
!         call sortkeys_set_keys    (obj, 4,   433.3, 455.5              )
!         call sortkeys_set_keys    (obj, 5,   533.3, 555.5, 577.7, 599.9)
!
!                                       isort
!                                         |
!         call sortkeys_define_sort (obj, 1, 1, 2, 3)
!         call sortkeys_define_sort (obj, 3, 3, 1   )
!         call sortkeys_define_sort (obj, 4, 2, 3, 1)
!
!                                             isort  indx
!                                               |     | 
!         irecord = sortkeys_get_record   (obj, 2,    4)
!
!                                             isort  indx  ikey
!                                               |     |     |
!         irecord = sortkeys_get_key      (obj, 2,    4,    3)
!
!
! Notes:
!  (1) Keys for record 3 are all zero.
!  (2) The third (and last) key for record 4 is zero.
!  (3) The last key specified for record 5 is not used.
!  (4) Sort orders 1 and 2 are the same (1,2,3).
!  (5) Sort order 3 is (3,1,1) with key 2 irrelevant.
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
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2003-06-16  Stoeckley  Change variable name from M to ISTRIDE for
!                             readability and for consistency with other
!                             similar primitives.
!  2. 2000-10-20  Stoeckley  Add required missing documentation section.
!  1. 2000-08-23  Stoeckley  Initial version.
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


      module sortkeys_module
      implicit none
      public
      private :: sortkeys_private_sort
      private :: sortkeys_private_compare

      character(len=100),public,save :: SORTKEYS_IDENT = &
       '$Id: sortkeys.f90,v 1.3 2003/06/13 16:07:32 Stoeckley prod sps $'

      integer,parameter,private :: MAXSORTS = 6
      integer,parameter,private :: lun = 6

      type,public :: sortkeys_struct
        private
        integer         :: nrecords,nkeys,nsorts,isort
        integer,pointer :: keys      (:,:)    ! (nkeys   ,nrecords)
        integer,pointer :: records   (:,:)    ! (nrecords,nsorts  )
        integer,pointer :: priorities(:,:)    ! (nkeys   ,nsorts  )
      end type sortkeys_struct


      contains


!!--------------------- sortkeys private sort ----------------------------!!
!!--------------------- sortkeys private sort ----------------------------!!
!!--------------------- sortkeys private sort ----------------------------!!

 
      subroutine sortkeys_private_sort (obj)
      implicit none
      type(sortkeys_struct),intent(inout) :: obj                ! arguments
      integer                             :: istride,k,j        ! local
      integer                             :: indx1,indx2        ! local
      integer                             :: irecord1,irecord2  ! local
      integer                             :: compare            ! local

      istride = obj%nrecords
      do
           istride = istride/2
           if (istride == 0) return
           k = obj%nrecords - istride
           do j = 1, k
                indx1 = j
                do
                     indx2 = indx1 + istride

                     irecord1 = obj%records(indx1,obj%isort)
                     irecord2 = obj%records(indx2,obj%isort)

                     compare = sortkeys_private_compare (obj,irecord1,irecord2)
                     if (compare <= 0) exit

                     obj%records(indx1,obj%isort) = irecord2        ! switch
                     obj%records(indx2,obj%isort) = irecord1        ! switch

                     if (indx1 <= istride) exit
                     indx1 = indx1 - istride
                end do
           end do
      end do
      end subroutine sortkeys_private_sort


!!--------------------- sortkeys private compare -------------------------!!
!!--------------------- sortkeys private compare -------------------------!!
!!--------------------- sortkeys private compare -------------------------!!

 
      function sortkeys_private_compare (obj,irecord1,irecord2) result (compare)
      implicit none
      type(sortkeys_struct),intent(inout) :: obj                ! arguments
      integer              ,intent(in)    :: irecord1,irecord2  ! arguments
      integer                             :: compare            ! result
      integer                             :: ikey,ipri          ! local
      integer                             :: key1,key2          ! local

      do ipri = 1,obj%nkeys  
           ikey = obj%priorities(ipri,obj%isort) 
           key1 = obj%keys(ikey,irecord1) 
           key2 = obj%keys(ikey,irecord2)
           if (key1 < key2) then         
                compare = -1            
                return                 
           else if (key1 > key2) then 
                compare = 1          
                return              
           end if                  
      end do                      
      compare = 0
      return
      end function sortkeys_private_compare


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine sortkeys_create (obj,nrecords,nkeys,nsorts,error)
      implicit none
      type(sortkeys_struct),pointer  :: obj                      ! arguments
      integer           ,intent(in)  :: nrecords,nkeys,nsorts    ! arguments
      logical           ,intent(out) :: error                    ! arguments
      integer                        :: irecord,istat            ! local

      if (nrecords < 1) then
        write(lun,*)'SORTKEYS: illegal value of NRECORDS in SORTKEYS_CREATE'
        write(lun,*)'SORTKEYS: NRECORDS must be >= 1'
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if
      if (nkeys < 1) then
        write(lun,*)'SORTKEYS: illegal value of NKEYS in SORTKEYS_CREATE'
        write(lun,*)'SORTKEYS: NKEYS must be >= 1'
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if
      if (nsorts < 0 .or. nsorts > MAXSORTS) then
        write(lun,*)'SORTKEYS: illegal value of NSORTS in SORTKEYS_CREATE'
        write(lun,*)'SORTKEYS: NSORTS must be between 0 and ',MAXSORTS
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if

      allocate (obj)

      obj%nrecords = nrecords
      obj%nkeys    = nkeys
      obj%nsorts   = nsorts
      obj%isort    = 0

      nullify (obj%keys      )
      nullify (obj%records   )
      nullify (obj%priorities)

      allocate (obj%keys(nkeys,nrecords),stat=istat)
      if (istat /= 0) then
           error = .true.
           return
      end if
      obj%keys = 0.0

      if (obj%nsorts == 0) return

      allocate (obj%records(nrecords,nsorts),stat=istat)
      if (istat /= 0) then
           error = .true.
           return
      end if

      allocate (obj%priorities(nkeys,nsorts),stat=istat)
      if (istat /= 0) then
           error = .true.
           return
      end if

      obj%priorities = 1

      do irecord = 1,nrecords
          obj%records(irecord,1:nsorts) = irecord
      end do
      error = .false.
      return
      end subroutine sortkeys_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine sortkeys_delete (obj)
      implicit none
      type(sortkeys_struct),pointer :: obj       ! arguments

      if (.not.associated(obj)) return

      if (associated(obj%keys      )) deallocate (obj%keys      )
      if (associated(obj%records   )) deallocate (obj%records   )
      if (associated(obj%priorities)) deallocate (obj%priorities)

      deallocate(obj)
      return
      end subroutine sortkeys_delete


!!--------------------- sortkeys set keys --------------------------------!!
!!--------------------- sortkeys set keys --------------------------------!!
!!--------------------- sortkeys set keys --------------------------------!!

 
      subroutine sortkeys_set_keys (obj,irecord,key1,key2,key3,key4,key5,key6)
      implicit none
      type(sortkeys_struct),intent(inout) :: obj                ! arguments
      integer              ,intent(in)    :: irecord            ! arguments
      integer     ,optional,intent(in)    :: key1,key2,key3     ! arguments
      integer     ,optional,intent(in)    :: key4,key5,key6     ! arguments

      if (irecord < 1 .or. irecord > obj%nrecords) then
        write(lun,*)'SORTKEYS: illegal value of IRECORD in SORTKEYS_SET_KEYS'
        write(lun,*)'SORTKEYS: IRECORD must be between 1 and ',obj%nrecords
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if

      obj%keys(:,irecord) = 0.0

      if(present(key1) .and. obj%nkeys >= 1) obj%keys(1,irecord) = key1
      if(present(key2) .and. obj%nkeys >= 2) obj%keys(2,irecord) = key2
      if(present(key3) .and. obj%nkeys >= 3) obj%keys(3,irecord) = key3
      if(present(key4) .and. obj%nkeys >= 4) obj%keys(4,irecord) = key4
      if(present(key5) .and. obj%nkeys >= 5) obj%keys(5,irecord) = key5
      if(present(key6) .and. obj%nkeys >= 6) obj%keys(6,irecord) = key6
      return
      end subroutine sortkeys_set_keys


!!----------------------- sortkeys define sort ---------------------------!!
!!----------------------- sortkeys define sort ---------------------------!!
!!----------------------- sortkeys define sort ---------------------------!!


      subroutine sortkeys_define_sort (obj,isort,ikey1,ikey2,ikey3,  &
                                                 ikey4,ikey5,ikey6)
      implicit none
      type(sortkeys_struct),intent(inout) :: obj                   ! arguments
      integer              ,intent(in)    :: isort                 ! arguments
      integer     ,optional,intent(in)    :: ikey1,ikey2,ikey3     ! arguments
      integer     ,optional,intent(in)    :: ikey4,ikey5,ikey6     ! arguments
      integer                             :: ipri                  ! local

      if (isort < 1 .or. isort > obj%nsorts) then
        write(lun,*)'SORTKEYS: illegal value of ISORT in SORTKEYS_DEFINE_SORT'
        write(lun,*)'SORTKEYS: ISORT must be between 1 and ',obj%nsorts
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if

      obj%isort = isort
      obj%priorities(:,isort) = 1

      if(present(ikey1) .and. obj%nkeys >= 1) obj%priorities(1,isort) = ikey1
      if(present(ikey2) .and. obj%nkeys >= 2) obj%priorities(2,isort) = ikey2
      if(present(ikey3) .and. obj%nkeys >= 3) obj%priorities(3,isort) = ikey3
      if(present(ikey4) .and. obj%nkeys >= 4) obj%priorities(4,isort) = ikey4
      if(present(ikey5) .and. obj%nkeys >= 5) obj%priorities(5,isort) = ikey5
      if(present(ikey6) .and. obj%nkeys >= 6) obj%priorities(6,isort) = ikey6

      do ipri = 1,obj%nkeys
        if (obj%priorities(ipri,isort) < 1 .or. &
            obj%priorities(ipri,isort) > obj%nkeys) then
          write(lun,*)'SORTKEYS: illegal value of IKEY in SORTKEYS_DEFINE_SORT'
          write(lun,*)'SORTKEYS: IKEY must be between 1 and ',obj%nkeys
          write(lun,*)'SORTKEYS: programming error in calling process'
          stop
        end if
      end do

      call sortkeys_private_sort (obj)
      return
      end subroutine sortkeys_define_sort


!!----------------------- sortkeys select sort ---------------------------!!
!!----------------------- sortkeys select sort ---------------------------!!
!!----------------------- sortkeys select sort ---------------------------!!


      subroutine sortkeys_select_sort (obj,isort)
      implicit none
      type(sortkeys_struct),intent(inout) :: obj                   ! arguments
      integer              ,intent(in)    :: isort                 ! arguments

      if (isort < 0 .or. isort > obj%nsorts) then
        write(lun,*)'SORTKEYS: illegal value of ISORT in SORTKEYS_SELECT_SORT'
        write(lun,*)'SORTKEYS: ISORT must be between 0 and ',obj%nsorts
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if

      obj%isort = isort
      return
      end subroutine sortkeys_select_sort


!!---------------------- sortkeys get record -----------------------------!!
!!---------------------- sortkeys get record -----------------------------!!
!!---------------------- sortkeys get record -----------------------------!!


      function sortkeys_get_record (obj,indx) result (irecord)
      implicit none
      type(sortkeys_struct),intent(in) :: obj              ! arguments
      integer              ,intent(in) :: indx             ! arguments
      integer                          :: irecord          ! result

      if (indx < 1 .or. indx > obj%nrecords) then
        write(lun,*)'SORTKEYS: illegal value of INDX in SORTKEYS_GET_RECORD'
        write(lun,*)'SORTKEYS: INDX must be between 1 and ',obj%nrecords
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if

      if (obj%isort == 0) then
           irecord = indx
      else
           irecord = obj%records(indx,obj%isort)
      end if
      return
      end function sortkeys_get_record


!!---------------------- sortkeys get prioritized key ----------------------!!
!!---------------------- sortkeys get prioritized key ----------------------!!
!!---------------------- sortkeys get prioritized key ----------------------!!


      function sortkeys_get_prioritized_key (obj,indx,ipri) result (key)
      implicit none
      type(sortkeys_struct),intent(in) :: obj              ! arguments
      integer              ,intent(in) :: indx,ipri        ! arguments
      integer                          :: key              ! result
      integer                          :: ikey             ! local

      if (ipri < 1 .or. ipri > obj%nkeys) then
        write(lun,*)'SORTKEYS: illegal value of IPRI in SORTKEYS_GET_KEY'
        write(lun,*)'SORTKEYS: IPRI must be between 1 and ',obj%nkeys
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if

      if (obj%isort == 0) then
           ikey = ipri
      else
           ikey = obj%priorities(ipri,obj%isort)
      end if
      key = sortkeys_get_original_key (obj,indx,ikey)
      return
      end function sortkeys_get_prioritized_key


!!---------------------- sortkeys get original key ------------------------!!
!!---------------------- sortkeys get original key ------------------------!!
!!---------------------- sortkeys get original key ------------------------!!


      function sortkeys_get_original_key (obj,indx,ikey) result (key)
      implicit none
      type(sortkeys_struct),intent(in) :: obj              ! arguments
      integer              ,intent(in) :: indx,ikey        ! arguments
      integer                          :: key              ! result
      integer                          :: irecord          ! local

      if (indx < 1 .or. indx > obj%nrecords) then
        write(lun,*)'SORTKEYS: illegal value of INDX in SORTKEYS_GET_KEY'
        write(lun,*)'SORTKEYS: INDX must be between 1 and ',obj%nrecords
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if
      if (ikey < 1 .or. ikey > obj%nkeys) then
        write(lun,*)'SORTKEYS: illegal value of IKEY in SORTKEYS_GET_KEY'
        write(lun,*)'SORTKEYS: IKEY must be between 1 and ',obj%nkeys
        write(lun,*)'SORTKEYS: programming error in calling process'
        stop
      end if

      irecord = sortkeys_get_record (obj,indx)
      key     = obj%keys(ikey,irecord)
      return
      end function sortkeys_get_original_key


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module sortkeys_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

