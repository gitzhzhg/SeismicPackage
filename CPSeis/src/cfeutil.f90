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
! Name       : cfeutil 
! Category   : cfe
! Written    : 1999-10-01   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : CFE Utilities Module.
! Portability: No known limitations, but see notes below.
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
!026. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 25. 2006-01-10  B. Menger    Remove Unused Variables.
! 24. 2003-11-18  Stoeckley    Remove two debug print statements accidentally
!                               left in the code; add workaround for Portland
!                               Group compiler.
! 23. 2003-09-29  Stoeckley    Added cfeutil_get_platform.
! 22. 2003-09-15  Stoeckley    Changed name from cfe_util to cfeutil;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary; added cfeutil_is_custom.
! 21. 2002-09-23  Vunderink    Changed prints to writes for checkc.
! 20. 2002-09-13  Vunderink    Added integer version for read_file and added
!                                remove argument.
! 19. 2002-09-11  Vunderink    Strip path from jobname in validate_jobname.
! 18. 2002-03-07  Vunderink    Remove file lock from write opens of mwb files.
! 17. 2002-01-04  Vunderink    Made mwb_buffer to listbuffer and mwb_list to
!                                list name change.
! 16. 2001-06-20  Vunderink    Make sure jobnames begin with an alpha character
! 15. 2001-03-15  Vunderink    Added cfeutil_sleep
! 14. 2001-02-01  Vunderink    Put exec_node in session file.
! 13. 2000-12-19  Vunderink    Made argument changes needed by
!                                mwb_buffer_get_last_list.
! 12. 2000-12-05  Vunderink    Added routine load_spreadsheet.
! 11. 2000-09-14  Vunderink    Make sure htemp array is large enough for NULL
!                                terminator.
! 10. 2000-08-15  Vunderink    Removed use of cfe_constants, changed character
!                                variables to use PC_LENGTH, and added routines
!                                cfeutil_insert_array_element,
!                                cfeutil_get_tokens, cfeutil_save_mwb_cardset,
!                                and cfeutil_get_mwb_cardset
!  9. 2000-05-26  Vunderink    Put editor choice in session file.
!  8. 2000-05-23  Vunderink    Added parameter cache calls to set minsize and
!                                maxsize of arrays.
!  7. 2000-05-09  Vunderink    Fixed bug in cfeutil_remove_array_element, and
!                                added routines cfeutil_save_session_log, 
!                                cfeutil_get_session_log, cfeutil_file_date
!                                and cfeutil_parse_for_jobname.
!  6. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH, added cfeutil_clear_array,
!                                cfeutil_find_last_requestid and
!                                cfeutil_file_datecmp, changed
!                                cfeutil_append_array_element to use narray as
!                                size of array, and fixed bug in
!                                cfeutil_remove_array_element
!  5. 2000-03-08  Vunderink    Added routine cfeutil_inquire_directory, and
!                                removed working_dir from the session defaults.
!  4. 2000-02-24  Vunderink    Added routine cfeutil_validate_jobname
!  3. 2000-02-16  Vunderink    Added routines cfeutil_read_file and
!                                cfeutil_library
!  2. 2000-02-03  Vunderink    Added routine cfeutil_remove_array_element,
!                                expanded cfeutil_append_array_element to
!                                support integers and added a repeat option,
!                                fixed problem with session parameters that
!                                appeared after change to cio_module, and
!                                generally improved cfeutil_save_session and
!                                cfeutil_get_session.
!  1. 1999-10-01  Vunderink    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! The Portland Group compiler required modifications in the code as follows:
!
!         Replaced reverse INDEX intrinsic function calls with STRING_INDEX.
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


      module cfeutil_module

      use cfestruct_module
      use cfelistbuffer_module
      use cfelist_module
      use string_module
      use getsys_module
      use putsys_module
      use cio_module
      use pc_module
      use finquire_module
      use path_module

      implicit none

      private
      public :: cfeutil_append_array_element
      public :: cfeutil_remove_array_element
      public :: cfeutil_insert_array_element
      public :: cfeutil_clear_array
      public :: cfeutil_get_tokens
      public :: cfeutil_save_session
      public :: cfeutil_get_session
      public :: cfeutil_save_session_log
      public :: cfeutil_get_session_log
      public :: cfeutil_save_mwb_cardset
      public :: cfeutil_get_mwb_cardset
      public :: cfeutil_read_file
      public :: cfeutil_get_platform
      public :: cfeutil_is_custom
      public :: cfeutil_library
      public :: cfeutil_parse_for_jobname
      public :: cfeutil_validate_jobname
      public :: cfeutil_inquire_directory
      public :: cfeutil_find_last_requestid
      public :: cfeutil_file_datecmp
      public :: cfeutil_file_date
      public :: cfeutil_load_spreadsheet
      public :: cfeutil_sleep

      character(len=100),public,save :: cfeutil_ident = &
       '$Id: cfeutil.f90,v 1.26 2006/09/18 13:32:43 Glover prod sps $'

      interface cfeutil_append_array_element
        module procedure cfeutil_append_array_element_c
        module procedure cfeutil_append_array_element_i
      end interface

      interface cfeutil_remove_array_element
        module procedure cfeutil_remove_array_element_c
        module procedure cfeutil_remove_array_element_i
      end interface

      interface cfeutil_insert_array_element
        module procedure cfeutil_insert_array_element_c
        module procedure cfeutil_insert_array_element_i
      end interface

      interface cfeutil_clear_array
        module procedure cfeutil_clear_array_c
        module procedure cfeutil_clear_array_i
      end interface

      interface cfeutil_read_file
        module procedure cfeutil_read_file_c
        module procedure cfeutil_read_file_i
      end interface

      interface cfeutil_sleep
        subroutine cfeutil_sleep_c(seconds)
          integer,intent(in) :: seconds
        end subroutine cfeutil_sleep_c
      end interface

      contains


!!----------------------- append_array_element_c ---------------------------!!
!!----------------------- append_array_element_c ---------------------------!!
!!----------------------- append_array_element_c ---------------------------!!


      subroutine cfeutil_append_array_element_c (array,narray,value,repeat)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument
      integer,optional                         :: repeat               !argument

      integer                                  :: i                    !local
      integer                                  :: nadd                 !local
      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (present(repeat)) then
        if (repeat .gt. 0) then
          nadd = repeat
        else
          nadd = 1
        endif
      else
        nadd = 1
      endif

      if (.not. associated(array)) then
        narray = nadd
        allocate(array(narray))
        do i=1,narray
          array(i) = value
        enddo
        return
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray+nadd))
        array(1:narray) = temp_array(1:narray)
        do i=1,nadd
          array(narray+i:narray+i) = value
        enddo
        narray = narray + nadd
        deallocate(temp_array)
      endif

      return
      end subroutine cfeutil_append_array_element_c


!!----------------------- append_array_element_i ---------------------------!!
!!----------------------- append_array_element_i ---------------------------!!
!!----------------------- append_array_element_i ---------------------------!!


      subroutine cfeutil_append_array_element_i (array,narray,value,repeat)
      implicit none
      integer,pointer                          :: array(:)             !argument
      integer,intent(inout)                    :: narray               !argument
      integer,intent(in)                       :: value                !argument
      integer,optional                         :: repeat               !argument

      integer                                  :: i                    !local
      integer                                  :: nadd                 !local
      integer,allocatable                      :: temp_array(:)        !local

      if (present(repeat)) then
        if (repeat .gt. 0) then
          nadd = repeat
        else
          nadd = 1
        endif
      else
        nadd = 1
      endif

      if (.not. associated(array)) then
        narray = nadd
        allocate(array(narray))
        do i=1,narray
          array(i) = value
        enddo
        return
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray+nadd))
        array(1:narray) = temp_array(1:narray)
        do i=1,nadd
          array(narray+i:narray+i) = value
        enddo
        narray = narray + nadd
        deallocate(temp_array)
      endif

      return
      end subroutine cfeutil_append_array_element_i


!!----------------------- remove_array_element_c ---------------------------!!
!!----------------------- remove_array_element_c ---------------------------!!
!!----------------------- remove_array_element_c ---------------------------!!


      subroutine cfeutil_remove_array_element_c (array,narray,start,end)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      integer         ,intent(in)              :: start                !argument
      integer         ,intent(in)              :: end                  !argument

      integer                                  :: i                    !local
      integer                                  :: ndel                 !local
      integer                                  :: istart               !local
      integer                                  :: iend                 !local
      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) return

      istart = start
      iend   = end
      narray = size(array)
      if (istart .lt. 1 .or. istart .gt. narray) then
        call pc_error ('Index out of range in remove_array_element')
        return
      endif
      if (iend .lt. 1 .or. iend .gt. narray) then
        call pc_error ('Index out of range in remove_array_element')
        return
      endif
      if (istart .gt. iend) then
        i      = istart
        istart = iend
        iend   = i
      endif
      ndel = iend - istart + 1
      if (narray .le. ndel) then
        if (associated(array)) deallocate(array)
        narray = 0
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray-ndel))
        if (istart .gt. 1) then
          array(1:istart-1) = temp_array(1:istart-1)
          array(istart:narray-ndel) = temp_array(iend+1:narray)
        else
          array(1:narray-ndel) = temp_array(iend+1:narray)
        endif
        narray = narray - ndel
        deallocate(temp_array)
      endif

      return
      end subroutine cfeutil_remove_array_element_c


!!----------------------- remove_array_element_i ---------------------------!!
!!----------------------- remove_array_element_i ---------------------------!!
!!----------------------- remove_array_element_i ---------------------------!!


      subroutine cfeutil_remove_array_element_i (array,narray,start,end)
      implicit none
      integer,pointer                          :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      integer         ,intent(in)              :: start                !argument
      integer         ,intent(in)              :: end                  !argument

      integer                                  :: i                    !local
      integer                                  :: ndel                 !local
      integer                                  :: istart               !local
      integer                                  :: iend                 !local
      integer,allocatable                      :: temp_array(:)        !local

      if (.not. associated(array)) return

      istart = start
      iend   = end
      narray = size(array)
      if (istart .lt. 1 .or. istart .gt. narray) then
        call pc_error ('Index out of range in remove_array_element')
        return
      endif
      if (iend .lt. 1 .or. iend .gt. narray) then
        call pc_error ('Index out of range in remove_array_element')
        return
      endif
      if (istart .gt. iend) then
        i      = istart
        istart = iend
        iend   = i
      endif
      ndel = iend - istart + 1
      if (narray .le. ndel) then
        if (associated(array)) deallocate(array)
        narray = 0
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray-ndel))
        if (istart .gt. 1) then
          array(1:istart-1) = temp_array(1:istart-1)
          array(istart:narray-ndel) = temp_array(iend+1:narray)
        else
          array(1:narray-ndel) = temp_array(iend+1:narray)
        endif
        narray = narray - ndel
        deallocate(temp_array)
      endif

      return
      end subroutine cfeutil_remove_array_element_i


!!----------------------- insert_array_element_c ---------------------------!!
!!----------------------- insert_array_element_c ---------------------------!!
!!----------------------- insert_array_element_c ---------------------------!!


      subroutine cfeutil_insert_array_element_c (array,narray,value,indx)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument
      integer         ,intent(in)              :: indx                 !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) then
        if (indx .ne. 1) then
          call pc_error ('Index out of range')
        else
          allocate(array(1))
          array(1) = value
          narray = 1
        endif
      else
        if (narray .gt. 0) then
          allocate(temp_array(narray))
          temp_array(1:narray) = array(1:narray)
          deallocate(array)
          allocate(array(narray+1))
          if (indx .gt. 1) array(1:indx-1) = temp_array(1:indx-1)
          array(indx) = value
          if (indx.lt.narray+1) array(indx+1:narray+1) = temp_array(indx:narray)
          narray = narray + 1
          deallocate(temp_array)
        else
          deallocate(array)
          allocate(array(1))
          array(1) = value
          narray = 1
        endif
      endif

      return
      end subroutine cfeutil_insert_array_element_c


!!----------------------- insert_array_element_i ---------------------------!!
!!----------------------- insert_array_element_i ---------------------------!!
!!----------------------- insert_array_element_i ---------------------------!!


      subroutine cfeutil_insert_array_element_i (array,narray,value,indx)
      implicit none
      integer,pointer                          :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      integer         ,intent(in)              :: value                !argument
      integer         ,intent(in)              :: indx                 !argument

      integer,allocatable                      :: temp_array(:)        !local

      if (.not. associated(array)) then
        if (indx .ne. 1) then
          call pc_error ('Index out of range')
        else
          allocate(array(1))
          array(1) = value
          narray = 1
        endif
      else
        if (narray .gt. 0) then
          allocate(temp_array(narray))
          temp_array(1:narray) = array(1:narray)
          deallocate(array)
          allocate(array(narray+1))
          if (indx .gt. 1) array(1:indx-1) = temp_array(1:indx-1)
          array(indx) = value
          if (indx.lt.narray+1) array(indx+1:narray+1) = temp_array(indx:narray)
          narray = narray + 1
          deallocate(temp_array)
        else
          deallocate(array)
          allocate(array(1))
          array(1) = value
          narray = 1
        endif
      endif

      return
      end subroutine cfeutil_insert_array_element_i


!!---------------------------- clear_array_c -------------------------------!!
!!---------------------------- clear_array_c -------------------------------!!
!!---------------------------- clear_array_c -------------------------------!!


      subroutine cfeutil_clear_array_c (array,narray)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument

      if (associated(array)) deallocate(array)
      allocate(array(1))
      array(1) = ' '
      narray = 0

      return
      end subroutine cfeutil_clear_array_c


!!---------------------------- clear_array_i -------------------------------!!
!!---------------------------- clear_array_i -------------------------------!!
!!---------------------------- clear_array_i -------------------------------!!


      subroutine cfeutil_clear_array_i (array,narray)
      implicit none
      integer,pointer                          :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument

      if (associated(array)) deallocate(array)
      allocate(array(1))
      array(1) = INIL
      narray = 0

      return
      end subroutine cfeutil_clear_array_i


!!----------------------------- get_tokens ---------------------------------!!
!!----------------------------- get_tokens ---------------------------------!!
!!----------------------------- get_tokens ---------------------------------!!


      subroutine cfeutil_get_tokens (record, tokens, ntokens)
      implicit none
      character(len=*),intent(in)     :: record                  ! argument
      character(len=*),pointer        :: tokens(:)               ! argument
      integer         ,intent(out)    :: ntokens                 ! argument

      integer                         :: i                       ! local
      integer                         :: istart                  ! local
      integer                         :: length                  ! local

      if (associated(tokens)) deallocate(tokens)

      ntokens = 0
      istart  = 0
      length  = len_trim(record)
      do i = 1,length
        if (record(i:i) .eq. ' ') then
          if (istart > 0) then
            ntokens = ntokens + 1
            istart = 0
          endif
        else if (istart .eq. 0) then
          istart = i
        endif
      enddo
      if (istart > 0) ntokens = ntokens + 1

      if (ntokens .eq. 0) then
        allocate(tokens(1))
        tokens(1) = ' '
        return
      endif

      allocate(tokens(ntokens))
      ntokens = 0
      istart  = 0
      length  = len_trim(record)
      do i = 1,length
        if (record(i:i) .eq. ' ') then
          if (istart > 0) then
            ntokens = ntokens + 1
            tokens(ntokens) = record(istart:i-1)
            if (size(tokens) == ntokens) return
            istart = 0
          endif
        else if (istart .eq. 0) then
          istart = i
        endif
      enddo
      if (istart > 0) then
        ntokens = ntokens + 1
        tokens(ntokens) = record(istart:length)
      endif

      return
      end subroutine cfeutil_get_tokens


!!---------------------------- save_session -------------------------------!!
!!---------------------------- save_session -------------------------------!!
!!---------------------------- save_session -------------------------------!!


      subroutine cfeutil_save_session (obj)
      implicit none
      type(cfestruct),pointer              :: obj                      !argument

      character(len=PC_LENGTH)             :: defaults_dir             !local
      character(len=PC_LENGTH)             :: home_dir                 !local
      character(len=PC_LENGTH)             :: filename                 !local
      character(len=PC_LENGTH),pointer     :: cards(:)                 !local
      integer                              :: ncards                   !local
      integer                              :: lun                      !local
      integer                              :: istat                    !local
      integer                              :: i                        !local
      logical                              :: exists                   !local
      type(cardset_struct),pointer         :: cardset                  !local
      character(len=2)                     :: mode                     !local
      character(len=PC_DATACARD_LENGTH)    :: string                   !local
      integer                              :: nstring,temp             !local


      nullify(cards)
      nullify(cardset)

      call getsys_env ('HOME',home_dir)
      defaults_dir = trim(home_dir)//'/.cfe_user_defaults'
      inquire (file=trim(defaults_dir),exist=exists)
      if (.not. exists) call putsys_cmd ('mkdir -p '// trim(defaults_dir),istat)

      filename = trim(defaults_dir)//'/session.def'
      mode = "w"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      call cardset_create (cardset)
      call cardset_set_packing_option (cardset,CARDSET_PACKED)

      call cardset_put_scalar (cardset,'EDITOR'    ,obj%editor)
      call cardset_put_scalar (cardset,'EXEC_NODE' ,obj%exec_node)
      call cardset_put_array  (cardset,'CUSTOM_XML',obj%custom_xml,  &
                                                    obj%ncustom_xml)

      ncards = 0
      call cardset_alloc_cards (cardset,cards,ncards)
      if (ncards .gt. 0) then
        do i=1,ncards
          string  = cards(i)
          temp = len_trim(string)
          nstring   = cio_fputline(string,temp,lun)
          if (nstring .lt. len_trim(string)) then
            write(STDOUT,*) 'Error writing session file  ',nstring,  &
                             len_trim(string)
          endif
        enddo
      endif
      istat = cio_fclose(lun)

      call cardset_delete (cardset)
      if (associated(cards)) deallocate(cards)


      return
      end subroutine cfeutil_save_session


!!----------------------------- get_session -------------------------------!!
!!----------------------------- get_session -------------------------------!!
!!----------------------------- get_session -------------------------------!!


      subroutine cfeutil_get_session (obj)
      implicit none
      type(cfestruct),pointer              :: obj                      !argument

      character(len=PC_LENGTH)             :: defaults_dir             !local
      character(len=PC_LENGTH)             :: home_dir                 !local
      character(len=PC_LENGTH)             :: filename                 !local
      character(len=PC_LENGTH)             :: errmsg                   !local
      integer                              :: istat                    !local
      integer                              :: lun = 0                  !local
      type(cardset_struct),pointer         :: cardset                  !local
      character(len=2)                     :: mode                     !local
      character(len=PC_DATACARD_LENGTH)    :: string                   !local
      integer                              :: nstring                  !local


      call getsys_env ('HOME',home_dir)
      defaults_dir = trim(home_dir)//'/.cfe_user_defaults'
      filename = trim(defaults_dir)//'/session.def'

      mode = "r"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      nullify(cardset)
      call cardset_create (cardset)
      call cardset_set_packing_option (cardset,CARDSET_PACKED)

      do
        nstring = cio_fgetline(string,PC_DATACARD_LENGTH,lun)
        if (nstring .lt. 0) exit
        call cardset_add_card (cardset,string(1:nstring))
      enddo

      istat = cio_fclose(lun)

      call cardset_get_scalar (cardset,'EDITOR'    ,obj%editor     ,errmsg)
      call cardset_get_scalar (cardset,'EXEC_NODE' ,obj%exec_node  ,errmsg)
      call cardset_alloc_array(cardset,'CUSTOM_XML',obj%custom_xml ,  &
                                                    obj%ncustom_xml,errmsg)
      call cardset_delete (cardset)

      obj%editor_temp    = obj%editor
      obj%exec_node_temp = obj%exec_node

      return
      end subroutine cfeutil_get_session


!!-------------------------- save_session_log -----------------------------!!
!!-------------------------- save_session_log -----------------------------!!
!!-------------------------- save_session_log -----------------------------!!


      subroutine cfeutil_save_session_log (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      character(len=PC_LENGTH)             :: defaults_dir             !local
      character(len=PC_LENGTH)             :: home_dir                 !local
      character(len=PC_LENGTH)             :: filename                 !local
      character(len=PC_LENGTH),pointer     :: cards(:)                 !local
      integer                              :: ncards                   !local
      integer                              :: lun                      !local
      integer                              :: istat                    !local
      integer                              :: i                        !local
      logical                              :: exists                   !local
      type(cardset_struct),pointer         :: cardset                  !local
      character(len=2)                     :: mode                     !local
      character(len=PC_DATACARD_LENGTH)    :: string                   !local
      integer                              :: nstring,temp             !local


      nullify(cards)
      nullify(cardset)

      call getsys_env ('HOME',home_dir)
      defaults_dir = trim(home_dir)//'/.cfe_user_defaults'
      inquire (file=trim(defaults_dir),exist=exists)
      if (.not. exists) call putsys_cmd ('mkdir -p '// trim(defaults_dir),istat)

      filename = trim(defaults_dir)//'/session_log.def'
      mode = "w"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      call cardset_create (cardset)
      call cardset_set_packing_option (cardset,CARDSET_PACKED)

      call cardset_put_array(cardset,'SLJobs'     ,obj%SLJobs     ,obj%SLTotal)
      call cardset_put_array(cardset,'SLBuilt'    ,obj%SLBuilt    ,obj%SLTotal)
      call cardset_put_array(cardset,'SLSubmitted',obj%SLSubmitted,obj%SLTotal)
      call cardset_put_array(cardset,'SLSeries'   ,obj%SLSeries   ,obj%SLTotal)
      call cardset_put_array(cardset,'SLCount'    ,obj%SLCount    ,obj%SLTotal)
      call cardset_put_array(cardset,'SLNext'     ,obj%SLNext     ,obj%SLTotal)

      ncards = 0
      call cardset_alloc_cards (cardset,cards,ncards)
      if (ncards .gt. 0) then
        do i=1,ncards
          string  = cards(i)
          temp = len_trim(string)
          nstring   = cio_fputline(string,temp,lun)
          if (nstring .lt. len_trim(string)) then
            write(STDOUT,*) 'Error writing session_log file  ',  &
                             nstring,len_trim(string)
          endif
        enddo
      endif
      istat = cio_fclose(lun)

      call cardset_delete (cardset)
      if (associated(cards)) deallocate(cards)


      return
      end subroutine cfeutil_save_session_log


!!--------------------------- get_session_log -----------------------------!!
!!--------------------------- get_session_log -----------------------------!!
!!--------------------------- get_session_log -----------------------------!!


      subroutine cfeutil_get_session_log (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      character(len=PC_LENGTH)             :: defaults_dir             !local
      character(len=PC_LENGTH)             :: home_dir                 !local
      character(len=PC_LENGTH)             :: filename                 !local
      character(len=PC_LENGTH)             :: mod_date                 !local
      character(len=PC_LENGTH)             :: current_date             !local
      character(len=PC_LENGTH)             :: errmsg                   !local
      integer                              :: istat                    !local
      integer                              :: lun     = 0              !local
      integer                              :: narrays = 6              !local
      type(cardset_struct),pointer         :: cardset                  !local
      character(len=2)                     :: mode                     !local
      character(len=12)                    :: arrays(6)                !local
      character(len=PC_DATACARD_LENGTH)    :: string                   !local
      integer                              :: nstring                  !local
      integer                              :: njobs                    !local
      integer                              :: nbuilt                   !local
      integer                              :: nsubmitted               !local
      integer                              :: nseries                  !local
      integer                              :: ncount                   !local
      integer                              :: nnext                    !local


      call getsys_env ('HOME',home_dir)
      defaults_dir = trim(home_dir)//'/.cfe_user_defaults'

      filename = trim(defaults_dir)//'/session_log.def'
      istat = finquire_input (trim(filename))
      if (istat .ne. FINQUIRE_FOUND) return
 
      call cfeutil_file_date (trim(filename),mod_date) 
      call string_date (current_date)
      if (trim(mod_date) .ne. trim(current_date)) return

      mode = "r"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      nullify(cardset)
      call cardset_create (cardset)
      call cardset_set_packing_option (cardset,CARDSET_PACKED)

      do
        nstring = cio_fgetline(string,PC_DATACARD_LENGTH,lun)
        if (nstring .lt. 0) exit
        call cardset_add_card (cardset,string(1:nstring))
      enddo

      istat = cio_fclose(lun)

      call cardset_alloc_array(cardset,'SLJobs'     ,obj%SLJobs     ,  &
                                                     njobs     ,errmsg)
      call cardset_alloc_array(cardset,'SLBuilt'    ,obj%SLBuilt    ,  &
                                                     nbuilt    ,errmsg)
      call cardset_alloc_array(cardset,'SLSubmitted',obj%SLSubmitted,  &
                                                     nsubmitted,errmsg)
      call cardset_alloc_array(cardset,'SLSeries'   ,obj%SLSeries   ,  &
                                                     nseries   ,errmsg)
      call cardset_alloc_array(cardset,'SLCount'    ,obj%SLCount    ,  &
                                                     ncount    ,errmsg)
      call cardset_alloc_array(cardset,'SLNext'     ,obj%SLNext     ,  &
                                                     nnext     ,errmsg)
      if (njobs.ne.nbuilt .or. njobs.ne.nsubmitted .or. njobs.ne.nseries .or.  &
          njobs.ne.ncount .or. njobs.ne.nnext) then
        call pc_error ('Error in session_log file')
        obj%SLTotal = min(njobs,nbuilt,nseries,ncount,nnext)
      else
        obj%SLTotal = njobs
      endif

      call cardset_delete (cardset)

      arrays(1) = 'SLJobs'
      arrays(2) = 'SLBuilt'
      arrays(3) = 'SLSubmitted'
      arrays(4) = 'SLSeries'
      arrays(5) = 'SLCount'
      arrays(6) = 'SLNext'

      call pc_register_array_names ('SessionArraySet',arrays,narrays)

      call pc_put_minsize_arrayset ('SessionArraySet',obj%SLtotal)
      call pc_put_maxsize_arrayset ('SessionArraySet',obj%SLtotal)

      call pc_put ('SLJobs'     ,obj%SLJobs     ,obj%SLTotal)
      call pc_put ('SLBuilt'    ,obj%SLBuilt    ,obj%SLTotal)
      call pc_put ('SLSubmitted',obj%SLSubmitted,obj%SLTotal)
      call pc_put ('SLSeries'   ,obj%SLSeries   ,obj%SLTotal)
      call pc_put ('SLCount'    ,obj%SLCount    ,obj%SLTotal)
      call pc_put ('SLNext'     ,obj%SLNext     ,obj%SLTotal)

      return
      end subroutine cfeutil_get_session_log


!!-------------------------- save_mwb_cardset -----------------------------!!
!!-------------------------- save_mwb_cardset -----------------------------!!
!!-------------------------- save_mwb_cardset -----------------------------!!


      subroutine cfeutil_save_mwb_cardset (cardset,filename)
      implicit none
      type(cardset_struct),pointer         :: cardset                  !argument
      character(len=*)    ,intent(in)      :: filename                 !argument

      integer                              :: i                        !local
      integer                              :: lun     = 0              !local
      integer                              :: ncards                   !local
      integer                              :: nstring                  !local
      integer                              :: istat                    !local
      character(len=2)                     :: mode                     !local
      character(len=PC_LENGTH),pointer     :: cards(:)                 !local
      character(len=PC_DATACARD_LENGTH)    :: string                   !local
      integer                              :: temp

      nullify (cards) ! jpa

      mode = "w"
      lun = cio_fopen(trim(filename),mode,file_lock=FILE_LOCK_DISABLED)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      ncards = 0
      call cardset_alloc_cards (cardset,cards,ncards)
      if (ncards .gt. 0) then
        do i=1,ncards
          string  = cards(i)
          temp = len_trim(string)
          nstring   = cio_fputline(string,temp,lun)
          if (nstring .lt. len_trim(string)) then
            write(STDOUT,*) 'Error writing mwb cardset file  ',nstring,  &
                             len_trim(string)
          endif
        enddo
      endif
      istat = cio_fclose(lun)

      if (associated(cards)) deallocate(cards)

      return
      end subroutine cfeutil_save_mwb_cardset


!!--------------------------- get_mwb_cardset -----------------------------!!
!!--------------------------- get_mwb_cardset -----------------------------!!
!!--------------------------- get_mwb_cardset -----------------------------!!


      subroutine cfeutil_get_mwb_cardset (cardset,filename)
      implicit none
      type(cardset_struct),pointer         :: cardset                  !argument
      character(len=*)    ,intent(in)      :: filename                 !argument

      integer                              :: lun     = 0              !local

      integer                              :: nstring                  !local
      integer                              :: istat                    !local
      character(len=2)                     :: mode                     !local

      character(len=PC_DATACARD_LENGTH)    :: string                   !local

      mode = "r"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      nullify(cardset)
      call cardset_create (cardset)
      call cardset_set_packing_option (cardset,CARDSET_PACKED)

      do
        nstring = cio_fgetline(string,PC_DATACARD_LENGTH,lun)
        if (nstring .lt. 0) exit
        call cardset_add_card (cardset,string(1:nstring))
      enddo

      istat = cio_fclose(lun)

      return
      end subroutine cfeutil_get_mwb_cardset


!!----------------------------- read_file_c -------------------------------!!
!!----------------------------- read_file_c -------------------------------!!
!!----------------------------- read_file_c -------------------------------!!


      subroutine cfeutil_read_file_c (filename,cards,ncards,title,remove)
      implicit none
      character(len=*),intent(in)     :: filename                      !argument
      character(len=*),pointer        :: cards(:)                      !argument
      integer         ,intent(out)    :: ncards                        !argument
      character(len=*),optional       :: title                         !argument
      logical         ,optional       :: remove                        !argument

      integer                         :: lun                           !local
      integer                         :: nstr                          !local
      integer                         :: istat                         !local
      character(len=1024)             :: str                           !local

      if (associated(cards)) deallocate(cards)
      ncards = 0

      lun = cio_fopen(trim(filename),'r')
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      if (present(title)) then
        call cfeutil_append_array_element (cards,ncards,trim(title))
      endif

      do
        nstr = cio_fgetline(str,1024,lun)
        if (nstr .lt. 0) exit
        call cfeutil_append_array_element (cards,ncards,trim(str))
      enddo

      istat = cio_fclose(lun)

      if (present(remove)) then
        if (remove) istat = cio_unlink(trim(filename))
      endif

      return
      end subroutine cfeutil_read_file_c


!!----------------------------- read_file_i -------------------------------!!
!!----------------------------- read_file_i -------------------------------!!
!!----------------------------- read_file_i -------------------------------!!


      subroutine cfeutil_read_file_i (filename,array,narray,remove)
      implicit none
      character(len=*),intent(in)     :: filename                      !argument
      integer,pointer                 :: array(:)                      !argument
      integer         ,intent(out)    :: narray                        !argument
      logical         ,optional       :: remove                        !argument

      integer                         :: i                             !local
      integer                         :: lun                           !local
      integer                         :: nstr                          !local
      integer                         :: istat                         !local
      character(len=1024)             :: str                           !local

      if (associated(array)) deallocate(array)
      narray = 0

      lun = cio_fopen(trim(filename),'r')
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      do
        nstr = cio_fgetline(str,1024,lun)
        if (nstr .lt. 0) exit
        call string_cc2ii (str,i)
        call cfeutil_append_array_element (array,narray,i)
      enddo

      istat = cio_fclose(lun)

      if (present(remove)) then
        if (remove) istat = cio_unlink(trim(filename))
      endif

      return
      end subroutine cfeutil_read_file_i


!!----------------------------- get_platform --------------------------------!!
!!----------------------------- get_platform --------------------------------!!
!!----------------------------- get_platform --------------------------------!!


      function cfeutil_get_platform () result (platform)
      implicit none
      character(len=60)        :: platform                      ! result
      integer                  :: hplatform(20)                 ! local

      call cfeutil_get_platform_c (hplatform)
      call string_hh2cc           (hplatform,platform)
      return
      end function cfeutil_get_platform


!!------------------------------- is_custom ---------------------------------!!
!!------------------------------- is_custom ---------------------------------!!
!!------------------------------- is_custom ---------------------------------!!


      function cfeutil_is_custom () result (custom)
      implicit none
      logical                  :: custom                        ! result
      integer                  :: icustom                       ! local
      integer,external         :: cfeutil_is_custom_c           ! local

      icustom = cfeutil_is_custom_c()
      custom = (icustom /= 0)
      return
      end function cfeutil_is_custom


!!------------------------------- library ---------------------------------!!
!!------------------------------- library ---------------------------------!!
!!------------------------------- library ---------------------------------!!


      function cfeutil_library () result (lnklib)
      implicit none
      integer                  :: lnklib                        ! result

      integer,external         :: cfeutil_library_c            ! local

      lnklib = cfeutil_library_c()

      return
      end function cfeutil_library


!!--------------------------- parse_for_jobname ---------------------------!!
!!--------------------------- parse_for_jobname ---------------------------!!
!!--------------------------- parse_for_jobname ---------------------------!!


      subroutine cfeutil_parse_for_jobname (filename,jobname)
      implicit none
      character(len=*),intent(in)    :: filename                  ! arguments
      character(len=*),intent(out)   :: jobname                   ! arguments

      integer                        :: i1                        ! local
      integer                        :: i2                        ! local

!!!   i1 = index        (filename,'/',.TRUE.)
      i1 = string_index (filename,'/',.TRUE.)
      if (i1 .eq. 0) then
        i1 = 1
      else
        i1 = i1 + 1
      endif
!!!   i2 = index        (filename,'.wrk',.TRUE.)
      i2 = string_index (filename,'.wrk',.TRUE.)
      if (i2.lt.i1) then
        i2 = len_trim(filename)
      else
        i2 = i2 - 1
      endif

      jobname = filename(i1:i2)

      return
      end subroutine cfeutil_parse_for_jobname


!!--------------------------- validate_jobname ----------------------------!!
!!--------------------------- validate_jobname ----------------------------!!
!!--------------------------- validate_jobname ----------------------------!!


      subroutine cfeutil_validate_jobname (jobname,status,msg)
      implicit none
      character(len=*),intent(inout) :: jobname                   ! arguments
      integer         ,intent(out)   :: status                    ! arguments
      character(len=*),intent(out)   :: msg                       ! arguments

      character(len=PC_LENGTH)       :: ctemp                     ! local
      character(len=1)               :: single                    ! local
      integer                        :: length,indx               ! local

      call string_replace_zeroes    (jobname)
      call string_strip_blanks      (jobname)
      call string_strip_unprintable (jobname)

      ctemp   = path_get_file(jobname)
      jobname = ctemp

      length = len_trim(jobname)
      if (length .gt. 0) then
        if (.not.string_is_alpha(jobname(1:1))) then
          msg = 'JOBNAME '//trim(jobname)//' must begin with alpha character'
          status = CFE_NAME_INVALID
          return
        endif
      endif

      do indx = 1,length
           single = jobname(indx:indx)
           if (string_is_alphanum (single      )) cycle
           if (string_is_one      (single,'._')) cycle
           msg = 'JOBNAME '//trim(jobname)// &
                                  ' contains the invalid character '//single
           status = CFE_NAME_INVALID
           return
      end do

      status = CFE_NAME_VALID
      msg = 'JOBNAME is valid'

      return
      end subroutine cfeutil_validate_jobname


!!-------------------------- inquire_directory ----------------------------!!
!!-------------------------- inquire_directory ----------------------------!!
!!-------------------------- inquire_directory ----------------------------!!


      subroutine cfeutil_inquire_directory (directory,status,msg)
      implicit none
      character(len=*),intent(inout) :: directory                 ! arguments
      integer         ,intent(out)   :: status                    ! arguments
      character(len=*),intent(out)   :: msg                       ! arguments

      integer,external               :: filebox_opendir           ! local
      character(len=PC_LENGTH)       :: ctemp                     ! local
      integer,allocatable            :: htemp(:)                  ! local
      integer                        :: nhtemp                    ! local
      integer                        :: cpi                       ! local
      integer                        :: i                         ! local
      integer                        :: j                         ! local

      cpi = string_chars_per_integer()
      nhtemp = (PC_LENGTH+1)/cpi + 1
      allocate(htemp(nhtemp))
      htemp = 0

      j = len_trim(directory)
      if (directory(j:j) .eq. '/') then
        ctemp = directory(1:j-1)
      else
        ctemp = directory
      endif
      call string_cc2hh(trim(ctemp),htemp)
      i = filebox_opendir(htemp)
      if (i .ne. 0) then
        msg = 'Invalid Directory'
        status = CFE_NAME_INVALID
        deallocate(htemp)
        return
      endif

      msg = 'Valid Directory'
      status = CFE_NAME_VALID

      deallocate(htemp)
      return
      end subroutine cfeutil_inquire_directory


!!------------------------- find_last_requestid ---------------------------!!
!!------------------------- find_last_requestid ---------------------------!!
!!------------------------- find_last_requestid ---------------------------!!


      subroutine cfeutil_find_last_requestid (cards,ncards,request_id)
      implicit none
      character(len=*),pointer       :: cards(:)                  ! arguments
      integer         ,intent(in)    :: ncards                    ! arguments
      character(len=*),intent(out)   :: request_id                ! arguments

      integer                        :: i                         ! local
      integer                        :: j1                        ! local
      integer                        :: j2                        ! local

      request_id = ' '
      do i=1,ncards
        j1 = index(cards(i),'submitted to queue')
        if (j1 .gt. 0) then
          j1 = index(cards(i),' ') + 1
          j2 = index(cards(i),'.') - 1
          if (j2.ge.j1 .and. j1.gt.0) then
             request_id = cards(i)(j1:j2)
          endif
        endif
      enddo

      return
      end subroutine cfeutil_find_last_requestid


!!---------------------------- file_datecmp -------------------------------!!
!!---------------------------- file_datecmp -------------------------------!!
!!---------------------------- file_datecmp -------------------------------!!


      function cfeutil_file_datecmp (file1,file2) result (datecmp)
      implicit none
      character(len=*),intent(in)   :: file1                         ! argument
      character(len=*),intent(in)   :: file2                         ! argument
      integer                       :: datecmp                       ! result

      integer                       :: istat                         ! local
      integer,allocatable           :: htmp1(:)                      ! local
      integer,allocatable           :: htmp2(:)                      ! local
      integer,external              :: cfeutil_file_datecmp_c       ! local

      datecmp = 0
      allocate(htmp1(string_num_integers(file1)),stat=istat)
      if (istat .eq. 0) then
        allocate(htmp2(string_num_integers(file2)),stat=istat)
        if (istat .eq. 0) then
          call string_cc2hh (file1,htmp1)
          call string_cc2hh (file2,htmp2)
          datecmp = cfeutil_file_datecmp_c(htmp1,htmp2)
          deallocate(htmp2,stat=istat)
        endif
        deallocate(htmp1,stat=istat)
      endif

      return
      end function cfeutil_file_datecmp


!!----------------------------- file_date ---------------------------------!!
!!----------------------------- file_date ---------------------------------!!
!!----------------------------- file_date ---------------------------------!!


      subroutine cfeutil_file_date (file,mdate) 
      implicit none
      character(len=*),intent(in)   :: file                          ! argument
      character(len=*),intent(out)  :: mdate                         ! argument

      integer                       :: istat                         ! local
      integer,allocatable           :: h_file(:)                     ! local
      integer,allocatable           :: h_mdate(:)                    ! local
      integer,external              :: cfeutil_file_date_c          ! local

      mdate = '           '
      allocate(h_file(string_num_integers(file)),stat=istat)
      if (istat .eq. 0) then
        allocate(h_mdate(string_num_integers(mdate)),stat=istat)
        if (istat .eq. 0) then
          call string_cc2hh (file ,h_file)
          call string_cc2hh (mdate,h_mdate)
          istat = cfeutil_file_date_c(h_file,h_mdate)
          call string_hh2cc (h_mdate,mdate)
          deallocate(h_mdate,stat=istat)
        endif
        deallocate(h_file,stat=istat)
      endif

      return
      end subroutine cfeutil_file_date


!!-------------------------- load_spreadsheet ------------------------------!!
!!-------------------------- load_spreadsheet ------------------------------!!
!!-------------------------- load_spreadsheet ------------------------------!!


      subroutine cfeutil_load_spreadsheet (masterBuffer,filename)
      implicit none
      type(cfelistbuffer_struct),pointer   :: masterBuffer             !argument
      character(len=*),intent(in)       :: filename                    !argument

      type :: list_array_struct
      type(cfelist_struct),pointer   :: list
      end type list_array_struct

      logical                           :: need_columns                !local
      logical                           :: found_error                 !local
      character(len=PC_DATACARD_LENGTH) :: string                      !local
      integer                           :: i                           !local
      integer                           :: j                           !local
      integer                           :: nstring                     !local
      integer                           :: lun = 0                     !local
      integer                           :: istat                       !local
      character(len=2)                  :: mode                        !local
      character(len=PC_DATACARD_LENGTH),pointer :: tokens(:)           !local
      integer                           :: ntokens                     !local
      character(len=PC_LENGTH),pointer  :: listnames(:)                !local
      integer                           :: nlistnames                  !local
      integer                           :: nlists                      !local
      integer                           :: indx                        !local
      type(list_array_struct),pointer   :: lists(:)                    !local
      type(cfelist_struct),pointer     :: lastList                    !local

      nullify (lists)
      nullify (lastList)
      nullify (tokens) ! jpa

      mode = "r"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening spreadsheet file '//trim(filename)//  &
                        ' lun=',lun
        call pc_error('Could not open spreadsheet file '//trim(filename))
        return
      endif

      indx = 0
      need_columns =.true.
      do
        nstring = cio_fgetline(string,PC_DATACARD_LENGTH,lun)
        if (nstring .lt. 0) exit
        if (string(1:1) .eq. '#' .or. string(1:1) .eq. '!') cycle
        if (len_trim(string) .eq. 0) cycle
        if (need_columns) then
          call cfeutil_get_tokens (string,tokens,ntokens)
          if (ntokens .gt. 0) then
            if (associated(masterBuffer)) then
              nullify(listnames)
              call cfelistbuffer_get_names (masterBuffer,listnames,nlistnames)
              if (nlistnames .gt. 0) then
                found_error = .false.
                do i=1,ntokens
                  do j=1,nlistnames
                    if (trim(tokens(i)) .eq. trim(listnames(j))) then
                      if (.not. found_error) then
                        call pc_error  &
                                 ('Could not load spreadsheet '//trim(filename))
                        found_error = .true.
                      endif
                      call pc_error ('Name '//trim(tokens(i))//' already used')
                    endif
                  enddo
                enddo
                deallocate(listnames)
                if (found_error) return
              endif
            endif
            nlists = ntokens
            allocate(lists(nlists))
            call pc_info ('Columns found in spreadsheet '//trim(filename))
            do i=1,nlists
              call pc_info ('        '//trim(tokens(i)))
              call cfelist_create      (lists(i)%list)
              call cfelist_set_name    (lists(i)%list,tokens(i))
              call cfelist_set_comment (lists(i)%list,'SPREADSHEET')
            enddo
            need_columns = .false.
          endif
        else
          call cfeutil_get_tokens (string,tokens,ntokens)
          indx = indx + 1
          if (nlists .ne. ntokens) then
            write(STDOUT,*)  'Error loading spreadsheet'//trim(filename)
            call pc_error ('Error loading spreadsheet'//trim(filename))
            if (associated(lists)) deallocate(lists)
            return
          else
            do i=1,nlists
              call cfelist_insert_string (lists(i)%list,indx,tokens(i))
            enddo
          endif
        endif
      enddo

      istat = cio_fclose(lun)

      if (nlists .eq. 0) then
        write(STDOUT,*) 'No colmuns found in spreadsheet'
        call pc_error ('No colmuns found in spreadsheet')
        return
      endif

      if (.not. associated(masterBuffer)) then
        call cfelistbuffer_create         (masterBuffer)
        call cfelistbuffer_set_first_list (masterBuffer,lists(1)%list)
        call cfelistbuffer_set_last_list  (masterBuffer,lists(1)%list)
        do i=2,nlists
          call cfelistbuffer_insert_list  (masterBuffer,lists(i)%list)
        enddo
      else
        call cfelistbuffer_get_last_list  (masterBuffer,lastList)
        if (.not. associated(lastList)) then
          call cfelistbuffer_set_first_list (masterBuffer,lists(1)%list)
          call cfelistbuffer_set_last_list  (masterBuffer,lists(1)%list)
          do i=2,nlists
            call cfelistbuffer_insert_list  (masterBuffer,lists(i)%list)
          enddo
        else
          do i=1,nlists
            call cfelistbuffer_insert_list  (masterBuffer,lists(i)%list)
          enddo
        endif
      endif

      return
      end subroutine cfeutil_load_spreadsheet


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cfeutil_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

