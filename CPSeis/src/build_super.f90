!<CPS_v1 type="AUXILIARY_FILE"/>
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
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : build_super
! Category   : stand-alone
! Written    : 2000-01-26   by: Donna K. Vunderink
! Revised    : 2000-09-26   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : Add a new process to super.f90.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2000-09-26  Vunderink    Added delete option.
!  3. 2000-07-05  Vunderink    Fixed problem of maturity missing last letter and
!                              process not added if belongs last.
!  2. 2000-03-27  Vunderink    Added error and warning messages.
!  1. 2000-01-26  Vunderink    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!!-------------------------- start of coding ------------------------------!!
!!-------------------------- start of coding ------------------------------!!
!!-------------------------- start of coding ------------------------------!!


      subroutine build_super (delflg,h_process,h_catagory,h_maturity,h_filename)

      use cio_module
      use string_module

      implicit none
      integer,intent(in)                    :: delflg           ! arguments
      integer,intent(in)                    :: h_process(*)     ! arguments
      integer,intent(in)                    :: h_catagory(*)    ! arguments
      integer,intent(in)                    :: h_maturity(*)    ! arguments
      integer,intent(in)                    :: h_filename(*)    ! arguments

      integer,parameter                     :: MAXBLANKFILL =13 ! local
      integer                               :: i                ! local
      integer                               :: j                ! local
      integer                               :: k                ! local
      integer                               :: inc = 1          ! local
      integer                               :: line_cnt = 0     ! local
      integer                               :: istat            ! local
      integer                               :: istat_all        ! local
      integer                               :: lun_in           ! local
      integer                               :: lun_out          ! local
      character(len=256)                    :: file_in          ! local
      character(len=256)                    :: file_out         ! local
      character(len=2)                      :: mode             ! local
      character(len=81 )                    :: string           ! local
      character(len=81 )                    :: string2          ! local
      integer                               :: nstring = 80     ! local
      integer                               :: nall_processes   ! local
      character(len=12),pointer             :: all_processes(:) ! local
      character(len=20),pointer             :: all_catagory(:)  ! local
      character(len=10),pointer             :: all_maturity(:)  ! local
      character(len=MAXBLANKFILL)           :: blankfill        ! local
      integer                               :: insert_id = 0    ! local
      character(len=12)                     :: old_start        ! local
      integer                               :: nblankfill       ! local
      character(len=80)                     :: process          ! local
      character(len=80)                     :: catagory         ! local
      character(len=80)                     :: maturity         ! local
      character(len=80)                     :: ctemp            ! local
      logical                               :: inserted_new     ! local
      logical                               :: matched_catagory ! local
      logical                               :: deleted_process  ! local
      integer                               :: temp

      nullify(all_processes)

      call string_hh2cc (h_process  ,process )
      call string_hh2cc (h_catagory ,catagory)
      call string_hh2cc (h_maturity ,maturity)
      call string_hh2cc (h_filename ,file_in )

      call string_to_upper (process )
      call string_to_upper (catagory)
      call string_to_upper (maturity)

      blankfill  = '              '

      if (trim(maturity).ne.'RAW'  .and. trim(maturity).ne.'ALPHA' .and.  &
          trim(maturity).ne.'BETA' .and. trim(maturity).ne.'PRODUCTION') then
        print *,'ERROR.... Invalid maturity'
        return
      endif

      file_out = 'super.f90.'//trim(process)

      mode = "r"
      lun_in = cio_fopen(trim(file_in),mode)
      if (lun_in .lt. 100) then
        print *,'ERROR.... Can not open file '//trim(file_in)//'  lun=',lun_in
        return
      endif

      mode = "wn"
      lun_out= cio_fopen(trim(file_out),mode)
      if (lun_out.lt. 100) then
        print *,'ERROR.... Can not open file '//trim(file_out)//'  lun=',lun_out
        return
      endif

      if (delflg .ne. 0) inc = -1                             !Delete process

!-------------------------------------------------------------------------------
!     Read/write to <process_list>
!     Get process_count and allocate arrays
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        if (string(1:15) .eq. '!<process_list>') exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999

      line_cnt = line_cnt + 1
      istat = cio_fgetline(string,nstring,lun_in)
      if (istat .lt. 0) goto 999
      if (trim(string(1:15)) .eq. '!PROCESS_COUNT=') then
        i = index(string,'=')
        call string_cc2ii (string(i+1:),nall_processes)
      else
        goto 999
      endif
      if (delflg .eq. 0) then                                !Add process
        call string_ii2cc (nall_processes+1,string(i+1:))

        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) goto 999
  
        allocate(all_processes(nall_processes+1))
        allocate(all_catagory(nall_processes+1))
        allocate(all_maturity(nall_processes+1))
      else
        call string_ii2cc (nall_processes-1,string(i+1:))
        temp  = len_trim(string) 
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) goto 999
        
        allocate(all_processes(nall_processes-1))
        allocate(all_catagory(nall_processes-1))
        allocate(all_maturity(nall_processes-1))
      endif

!-------------------------------------------------------------------------------
!     Load process list info into arrays
!     Insert new process
!-------------------------------------------------------------------------------

      if (delflg .eq. 0) then                                !Add process
        matched_catagory = .false.
        inserted_new = .false.
        k = 0
        do i=1,nall_processes
          line_cnt = line_cnt + 1
          istat = cio_fgetline(string,nstring,lun_in)
          j = index(string,' ')
          if (trim(process) .eq. string(2:j-1)) then
            print *,'ERROR.... Process '//trim(process)//' already in file'
            istat = cio_fclose(lun_in)
            istat = cio_fclose(lun_out)
            istat = cio_remove(trim(file_out))
            return
          else if (trim(process).lt.string(2:j-1) .and. .not.inserted_new) then
            inserted_new = .true.
            k = k + 1
            all_processes(k) = trim(process)
            all_catagory(k)  = trim(catagory)
            all_maturity(k)  = trim(maturity)
            string2 = '!'//process(1:20)//catagory(1:26)//trim(maturity)
            temp  = len_trim(string2)
            istat = cio_fputline(string2,temp,lun_out)
            if (istat .lt. 0) exit
          endif
          k = k + 1
          all_processes(k) = string(2:j-1)
          if (i .eq. 1) old_start = all_processes(k)
          temp  = len_trim(string)
          istat = cio_fputline(string,temp,lun_out)
          if (istat .lt. 0) exit
          string2 = string(j:)
          string  = adjustl(string2)
          j = index(string,' ')
          all_catagory(k) = string(1:j-1)
          if (trim(all_catagory(k)) .eq. trim(catagory)) matched_catagory=.true.
          string2 = string(j:)
          string  = adjustl(string2)
          all_maturity(k) = trim(string)
        enddo
        if (istat .lt. 0) goto 999

        if (.not.inserted_new) then
          inserted_new = .true.
          k = k + 1
          all_processes(k) = trim(process)
          all_catagory(k)  = trim(catagory)
          all_maturity(k)  = trim(maturity)
          string2 = '!'//process(1:20)//catagory(1:26)//trim(maturity)
          temp  = len_trim(string2)
          istat = cio_fputline(string2,temp,lun_out)
          if (istat .lt. 0) goto 999
        endif

        nall_processes = nall_processes + 1
        if (.not. matched_catagory) then
          print *,'WARNING.... catagory does not match any existing processes'
        endif

      else                                                   !Delete process

        deleted_process = .false.
        k = 0
        do i=1,nall_processes
          line_cnt = line_cnt + 1
          istat = cio_fgetline(string,nstring,lun_in)
          j = index(string,' ')
          if (trim(process) .eq. string(2:j-1)) then
            deleted_process =.true.
            if (i .eq. 1) old_start = process
          else
            k = k + 1
            all_processes(k) = string(2:j-1)
            if (i .eq. 1) old_start = all_processes(k)
            temp  = len_trim(string)
            istat = cio_fputline(string,temp,lun_out)
            if (istat .lt. 0) exit
            string2 = string(j:)
            string  = adjustl(string2)
            j = index(string,' ')
            all_catagory(k) = string(1:j-1)
            string2 = string(j:)
            string  = adjustl(string2)
            all_maturity(k) = trim(string)
          endif
        enddo
        nall_processes = nall_processes - 1
        if (.not. deleted_process) then
          print *,'WARNING.... did not find process to delete'
        endif
      endif

!-------------------------------------------------------------------------------
!     Read/write       use XXX_module
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,trim(old_start))
        if (i .ne. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=2,nall_processes-inc
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=1,nall_processes
        string = '      use ' // trim(all_processes(i)) // '_module'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

!-------------------------------------------------------------------------------
!     Read/write       type(XXX_struct),pointer  :: XXXobj
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,trim(old_start))
        if (i .ne. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=2,nall_processes-inc
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=1,nall_processes
        nblankfill = MAXBLANKFILL - len_trim(all_processes(i))
        string = '        type(' // trim(all_processes(i)) //     &
                 '_struct),pointer ' // blankfill(1:nblankfill) // &
                 ':: ' // trim(all_processes(i)) // 'obj'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

!-------------------------------------------------------------------------------
!     Read/write       nullify(obj%XXXobj)
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,trim(old_start))
        if (i .ne. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=2,nall_processes-inc
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=1,nall_processes
        string = '      nullify(obj%' // trim(all_processes(i)) // 'obj)'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

!-------------------------------------------------------------------------------
!     Read/write       case ('XXX')  ;call XXX_create(obj%XXXobj)
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,trim(old_start))
        if (i .ne. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=2,nall_processes-inc
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=1,nall_processes
        nblankfill = MAXBLANKFILL - len_trim(all_processes(i))
        string = '        case (''' // trim(all_processes(i)) //  &
                ''')'// blankfill(1:nblankfill) // ';call ' //    &
                trim(all_processes(i)) // '_create(obj%' //       &
                trim(all_processes(i)) // 'obj)'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

!-------------------------------------------------------------------------------
!     Read/write       case ('XXX')  ;call XXX_delete(obj%XXXobj)
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,trim(old_start))
        if (i .ne. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=2,nall_processes-inc
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=1,nall_processes
        nblankfill = MAXBLANKFILL - len_trim(all_processes(i))
        string = '        case (''' // trim(all_processes(i)) //  &
                 ''')'// blankfill(1:nblankfill) // ';call ' //    &
                 trim(all_processes(i)) // '_delete(obj%' //      &
                 trim(all_processes(i)) // 'obj)'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

!-------------------------------------------------------------------------------
!     Read/write       case ('XXX')  ;call XXX_initialize(obj%XXXobj)
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,trim(old_start))
        if (i .ne. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=2,nall_processes-inc
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=1,nall_processes
        nblankfill = MAXBLANKFILL - len_trim(all_processes(i))
        string = '        case (''' // trim(all_processes(i)) //  &
                 ''')' // blankfill(1:nblankfill) // ';call ' //  &
                 trim(all_processes(i)) // '_initialize(obj%' //  &
                 trim(all_processes(i)) // 'obj)'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

!-------------------------------------------------------------------------------
!     Read/write       case ('XXX')  ;call XXX_update(obj%XXXobj)
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,trim(old_start))
        if (i .ne. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=2,nall_processes-inc
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

      do i=1,nall_processes
        nblankfill = MAXBLANKFILL - len_trim(all_processes(i))
        string = '        case (''' // trim(all_processes(i)) //  &
                 ''')' // blankfill(1:nblankfill) // ';call ' //  &
                 trim(all_processes(i)) // '_update(obj%' //      &
                 trim(all_processes(i)) // 'obj)'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

!-------------------------------------------------------------------------------
!     Read/write subroutine super_load_list
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,'super_load_list')
        if (i .ne. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999
      string = '      subroutine super_load_list'
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999

      string = ' '
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999

      call string_ii2cc (nall_processes,ctemp)
      j = len_trim(ctemp)
      string = '      len_all = ' // trim(ctemp)
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999

      string = '      allocate(all(len_all))'
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999

      string = ' '
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999

      do i=1,nall_processes
        call string_ii2cc (i,ctemp)
        string = '      all(' // ctemp(1:j) // ')%process  = ''' //  &
                 trim(all_processes(i)) // ''''
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
        string = '      all(' // ctemp(1:j) // ')%catagory = ''' //  &
                 trim(all_catagory(i)) // ''''
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
        string = '      all(' // ctemp(1:j) // ')%maturity = SUPER_' //  &
                 trim(all_maturity(i))
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
        string = ' '
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo

      string = '      first_time = .FALSE.'
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999
      string = ' '
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999
      string = '      return'
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        i = index(string,'super_load_list')
        if (i .ne. 0) exit
      enddo
      if (istat .lt. 0) goto 999
      string = '      end subroutine super_load_list'
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun_out)
      if (istat .lt. 0) goto 999


!-------------------------------------------------------------------------------
!     Read/write rest of file
!-------------------------------------------------------------------------------

      do
        line_cnt = line_cnt + 1
        istat = cio_fgetline(string,nstring,lun_in)
        if (istat .lt. 0) exit
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun_out)
        if (istat .lt. 0) exit
      enddo
      if (istat .lt. 0) goto 999

 999  continue
      if (istat .eq. CIO_ERROR) print *,'ERROR.... on line ',line_cnt

      istat = cio_fclose(lun_in)
      istat = cio_fclose(lun_out)

      return

      end subroutine build_super

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

