program test1
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
  use ameq_module
  use swap_module
  use trcio_module
  use cpsio_module
  use cio_module
  implicit none

  type(trcio_struct),pointer  :: infile,newfil,csumfil

  real                            :: tr(7),t_tmp(7)
  integer(kind=2)                 :: itr(7)
  integer(kind=1)                 :: btr(7)
  real                            :: hdr(3)
  double precision                :: hd(3),h_tmp(3)
  integer                         :: i,j,ierr,status,k
  character(len=160)               :: str,filename
  character(len=2)                :: mode
  character(len=1)                :: c

  double precision                :: hdcs(64)

  print*,'TEST1: Create, read, write a trace file with real, int*2, and byte '
  print*,'       data.  Read a trace, Overwrite a trace, seek, tell,opn,clos.'
  print*,'       FIRST WRITE 8byte header, 4byte TRACE'
  print*,'       SECOND write 4byte header, 2byte trace'
  print*,'       THIRD  write 4 byte header, 1 byte trace.'
  print*,'       LAST, write 32 word header so we can test checksum. '
  print*,' to do checksum only, jump down...'
!  go to 1000
  ierr = 0
  ! --- create a 10 trace file.

  infile => trcio_open('trace.file','w')

  if(.not. associated(infile) ) then
    print*, ' could not open infile.'
    stop 'line 27'
  endif

  infile%nwih=3
  infile%num_values=7
  infile%nbits=32
  infile%nbits_hd=64
  infile%wtype='IEEE'
  infile%ftype='TRCIO'
  infile%endian=swap_endian()
  infile%num_traces=0
  i = trcio_writeheader(infile)
  print*,' There were ',i,' errors writing the header.'
  call trcio_headerdump(infile)

  do j = 1, 10
  
    do i = 1, size(tr)
      tr(i) = i + (j-1)*size(tr) 
    end do
  
    do i = 1, size(hd)
      hd(i) = i + (j-1)*size(hd)
    end do

    status = trcio_write_trace(infile,hd,tr)  
    if(status /= TRCIO_OK) then
       print*,' Error on Write: j=',j,' status: ',status
    endif
  end do 

  status = trcio_close(infile)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif

  infile => trcio_open('trace.file','r+')
  if(.not. associated(infile) ) then
    print*, ' could not open file.'
    stop 'line 66'
  endif

  print*,' File header follows: '
  call trcio_headerdump(infile)
  print*,'----------------------------------------------'

  do k = 0,1
    i = cpsio_position_file(infile%lun,1,'SEISMIC')
    print*,'Seismic data starts at byte# ',cio_ftell(infile%lun)
  
    do j = 1,10
  
      status = trcio_read_trace(infile,hd,tr)
      if(status /= TRCIO_OK) then
         print*,' Error on Read : j=',j,' status: ',status
      endif
  
      do i = 1, size(tr)
        if(tr(i) /= i + (j-1)*size(tr) ) ierr = ierr + 1
      end do
      do i = 1, size(hd)
        if(hd(i) /= i + (j-1)*size(hd) ) ierr = ierr + 1
      end do
      print*,' Trc: ',j
      print*,'      HD: ',hd 
      print*,'      TR: ',tr 
    end do
    status = trcio_read_trace(infile,h_tmp,t_tmp,5)
    print*,' read # 5: ',status
    print*,'      HD: ',h_tmp
    print*,'      TR: ',t_tmp 
    status = trcio_write_trace(infile,h_tmp,t_tmp,5)
    print*,' write # 5: ',status
  end do

  print*,' trcio_seek_trace 7: ',trcio_seek_trace(infile,7)
  print*,' trcio_tell_trace ?: ',trcio_tell_trace(infile)
  status = trcio_close(infile)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif

  print*,' Wrote and read infile with ',ierr,' errors out of ', &
         (j+1)*(size(tr)+size(hd)),' words.'
  
  newfil => trcio_open('trace.file','r')
  call trcio_headerdump(newfil)
  status = trcio_close(newfil)


  infile => trcio_open('tracei.file','w')

  if(.not. associated(infile) ) then
    print*, ' could not open file.'
    stop 'line 121'
  endif

  infile%nwih=3
  infile%num_values=7
  infile%nbits=16
  infile%endian=swap_endian()
  infile%wtype='IEEE'
  infile%ftype='TRCIO'
  infile%nbits_hd=32
  infile%num_traces=0
  i = trcio_writeheader(infile)
  print*,' There were ',i,' errors writing the header.'
  call trcio_headerdump(infile)
  hdr = 0 
  itr = 0
  print*,' selected_int_kind(2) = ',selected_int_kind(2)
  do j = 1, 10
  
    do i = 1, size(tr)
      tr(i) = i + (j-1)*size(tr) 
    end do
  
    do i = 1, size(hd)
      hd(i) = i + (j-1)*size(hd)
    end do

    status = trcio_write_trace(infile,hd,tr)  
    if(status /= TRCIO_OK) then
       print*,' Error on Write: j=',j,' status: ',status
    endif
  end do 

  status = trcio_close(infile)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif
 
  infile => trcio_open('tracei.file','r+')
  if(.not. associated(infile) ) then
    print*, ' could not open file.'
    stop 'line 162'
  endif
  call cio_frewind(infile%lun)


  print*,' File header follows: '
  call trcio_headerdump(infile)
  print*,'----------------------------------------------'

  do k = 0,1
    i = cpsio_position_file(infile%lun,1,'SEISMIC')
    print*,'Seismic data starts at byte# ',cio_ftell(infile%lun)
  
    do j = 1,10
  
      status = trcio_read_trace(infile,hd,tr)
      if(status /= TRCIO_OK) then
         print*,' Error on Read : j=',j,' status: ',status
      endif
  
      do i = 1, size(tr)
        if(tr(i) /= i + (j-1)*size(tr) ) ierr = ierr + 1
      end do
      do i = 1, size(hd)
        if(hd(i) /= i + (j-1)*size(hd) ) ierr = ierr + 1
      end do
      print*,' Trc: ',j
      print*,'      HD: ',hd 
      print*,'      TR: ',tr 
    end do
    status = trcio_read_trace(infile,h_tmp,t_tmp,5)
    print*,' read # 5: ',status
    print*,'      HD: ',h_tmp 
    print*,'      TR: ',t_tmp 
    status = trcio_write_trace(infile,h_tmp,t_tmp,5)
    print*,' write # 5: ',status
  end do

  print*,' trcio_seek_trace 7: ',trcio_seek_trace(infile,7)
  print*,' trcio_tell_trace ?: ',trcio_tell_trace(infile)
  status = trcio_close(infile)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif

  print*,' Wrote and read file with ',ierr,' errors out of ', &
         (j+1)*(size(tr)+size(hd)),' words.'



  infile => trcio_open('traceb.file','w')

  if(.not. associated(infile) ) then
    print*, ' could not open file.'
    stop 'line 216'
  endif

  infile%nwih=3
  infile%num_values=7
  infile%nbits=8
  infile%ftype='TRCIO'
  infile%wtype='byte'
  infile%endian=swap_endian()
  infile%nbits_hd=32
  infile%num_traces=0
  i = trcio_writeheader(infile)
  print*,' There were ',i,' errors writing the header.'
  call trcio_headerdump(infile)
  hdr = 0 
  btr = 0
  ierr = 0
  print*,' selected_int_kind(1) = ',selected_int_kind(1)

  do j = 1, 10
  
    do i = 1, size(tr)
      tr(i) = i + (j-1)*size(tr) 
    end do
  
    do i = 1, size(hd)
      hd(i) = i + (j-1)*size(hd)
    end do

    status = trcio_write_trace(infile,hd,tr)  
    if(status /= TRCIO_OK) then
       print*,' Error on Write: j=',j,' status: ',status
    endif
  end do 

  status = trcio_close(infile)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif
 
  infile => trcio_open('traceb.file','r+')
  if(.not. associated(infile) ) then
    print*, ' could not open file.'
    stop 'line 258'
  endif
  call cio_frewind(infile%lun)


  print*,' File header follows: '
  call trcio_headerdump(infile)
  print*,'----------------------------------------------'

  do k = 0,1
    i = cpsio_position_file(infile%lun,1,'SEISMIC')
    print*,'Seismic data starts at byte# ',cio_ftell(infile%lun)
  
    do j = 1,10
  
      status = trcio_read_trace(infile,hd,tr)
      if(status /= TRCIO_OK) then
         print*,' Error on Read : j=',j,' status: ',status
      endif
  
      do i = 1, size(tr)
        if(tr(i) /= i + (j-1)*size(tr) ) ierr = ierr + 1
      end do
      do i = 1, size(hd)
        if(hd(i) /= i + (j-1)*size(hd) ) ierr = ierr + 1
      end do
      print*,' Trc: ',j
      print*,'      HD: ',hd 
      print*,'      TR: ',tr 
    end do
    status = trcio_read_trace(infile,h_tmp,t_tmp,5)
    print*,' read # 5: ',status
    print*,'      HD: ',h_tmp 
    print*,'      TR: ',t_tmp 
    status = trcio_write_trace(infile,h_tmp,t_tmp,5)
    print*,' write # 5: ',status
  end do

  print*,' trcio_seek_trace 7: ',trcio_seek_trace(infile,7)
  print*,' trcio_tell_trace ?: ',trcio_tell_trace(infile)
  status = trcio_close(infile)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif

 print*,' Wrote and read file with ',ierr,' errors out of ', &
         (j+1)*(size(tr)+size(hd)),' words.'

 1000 continue
 print*,' TESTING THE CHECKSUM FEATURE.'


  csumfil => trcio_open('tracec.file','w')

  if(.not. associated(csumfil) ) then
    print*, ' could not open file.'
    stop 'line 323'
  endif

  csumfil%nwih=size(hdcs)
  csumfil%nbits_hd=64
  csumfil%num_values=7
  csumfil%nbits=32
  csumfil%ftype='TRCIO'
  csumfil%wtype='IEEE'
  csumfil%endian=swap_endian()
  csumfil%num_traces=0
  i = trcio_writeheader(csumfil)
  print*,' There were ',i,' errors writing the header.'
  hdr = 0 
  btr = 0
  ierr = 0
  do j = 1, 3
  
    do i = 1, size(tr)
      tr(i) =  1.1*(i + (j-1)*size(tr) )
    end do
  
    do i = 1, size(hdcs)
      hdcs(i) = 1.1*(i + (j-1)*size(hdcs))
    end do

    status = trcio_write_trace(csumfil,hdcs,tr)  
    if(status /= TRCIO_OK) then
       print*,' Error on Write: j=',j,' status: ',status
    endif
    write(6,'(a,7F8.3)')'HD: ',(hdcs(k),k=1,min(31,size(hdcs)),5)
    write(6,'(a,7F8.3)') 'TR: ',(tr  (k),k=1,7)
  end do 

  status = trcio_close(csumfil)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif
 
  csumfil => trcio_open('tracec.file','r+')
  if(.not. associated(csumfil) ) then
    print*, ' could not open file.'
    stop 'line 364'
  endif
  call cio_frewind(csumfil%lun)


  call trcio_headerdump(csumfil)
  print*,'----------------------------------------------'

    i = cpsio_position_file(csumfil%lun,1,'SEISMIC')
    print*,'Seismic data starts at byte# ',cio_ftell(csumfil%lun)
    do j = 1,3
  
      status = trcio_read_trace(csumfil,hdcs,tr)
      if(status /= TRCIO_OK) then
         print*,' Error on Read : j=',j,' status: ',status
      endif
  
      write(6,'(a,7F8.3)')'HD: ',(hdcs(k),k=1,min(31,size(hdcs)),5)
      write(6,'(a,7F8.3)') 'TR: ',(tr  (k),k=1,7)
    end do

  status = trcio_close(csumfil)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif

 print*,'---------------------- OPENING OTHER BYTE_ORDERED FILE----------'
 print*,'-----------------(Ensure file or link exists traceo.file)'
 print*,'----------------- traceo.file should be tracec.file from other mach.'


  csumfil => trcio_open('traceo.file','r')
  if(.not. associated(csumfil) ) then
    print*, ' could not open file.'
    stop 'line 408'
  endif
  call cio_frewind(csumfil%lun)

  ierr = 0
  call trcio_headerdump(csumfil)
  print*,'----------------------------------------------'

    i = cpsio_position_file(csumfil%lun,1,'SEISMIC')
    print*,'Seismic data starts at byte# ',cio_ftell(csumfil%lun)
  
    do j = 1,3
  
      status = trcio_read_trace(csumfil,hdcs,tr)
      if(status /= TRCIO_OK) then
         print*,' Error on Read : j=',j,' status: ',status
      endif
  
      do i = 1, size(tr)
        if(.not.ameq(tr(i), 1.1*(i + (j-1)*size(tr)),.0001  )) then
          ierr = ierr + 1
          print*,'Trace: ',j,' Sample: ',i,' = ',tr(i),' vs. ',&
                  1.1*(i + (j-1)*size(tr))
        endif
      end do
      do i = 1, min(size(hdcs),31)
        if(.not.ameq(hdcs(i),1.1d0*(i + (j-1)*size(hdcs)),.0001   )) then 
          ierr = ierr + 1
          print*,'Trace: ',j,' Header: ',i,' = ',hdcs(i),' vs. ',&
                  1.1*(i + (j-1)*size(hdcs))
        endif
      end do
      write(6,'(a,7F8.3)')'HD: ',(hdcs(k),k=1,min(31,size(hdcs)),5)
      write(6,'(a,7F8.3)') 'TR: ',(tr  (k),k=1,7)
    end do

  status = trcio_close(csumfil)
  if(status /= TRCIO_OK) then
    print*,' Error on Close: ',status
  endif

 print*,' Wrote and read file with ',ierr,' errors out of ', &
         (j+1)*(size(tr)+size(hdcs)-1),' words.'


end program test1
