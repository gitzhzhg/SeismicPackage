program testrandom
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
  use trcio_module
  use cio_module
  implicit none

  type(trcio_struct),pointer      :: filein, fileout
  real,allocatable,dimension(:)   :: tr
  double precision,dimension(:),allocatable :: hd
  integer                         :: i
  character(len=160)              :: namein,nameout
  character(len=2)                :: mode
  double precision                :: trmaxg
  integer                         :: status,j

10 namein = ""
  write(6,'(A)',advance="no")'Input Trace file: '
  read(5,'(a)') namein
  namein=trim(namein)//' '
  mode     = 'r'
 
  print*,' Opening ',trim(namein),' with mode = ',trim(mode) 
  filein => trcio_open(namein,mode)
  if(.not. associated(filein) ) then
    print*,' File not opened, try again.'
    go to 10
  endif

  call trcio_headerdump(filein)

  allocate(tr(filein%num_values))
  allocate(hd(filein%nwih))

  trmaxg = 0d0
  print*,'Estimated ',filein%num_traces,' in the file.'

  do i = filein%num_traces,filein%num_traces/2, -filein%num_traces/12
    status=trcio_read_trace(filein,hd,tr,i)
    if(status /= trcio_ok) then 
      print*,' Tried to read trace ',i,' but failed.'
      cycle
    endif
    trmaxg = max(1d0*maxval(abs(tr)),trmaxg)
    print*,'I: ',i,' TRMAXG: ',trmaxg,'H: ',hd(1),hd(25),' T: ',maxval(abs(tr))
  end do


  print*,' ready to open an output file'

  write(6,'(A)',advance="no")'Output file: '
  !read(5,'(A)')nameout
  nameout='~/cpsdata/test_random.trc'
  nameout=trim(nameout)//' '
  mode='w'

  print*,' Opening ',trim(nameout),' with mode = ',trim(mode)
  fileout => trcio_open(nameout,mode)
  if(.not. associated(fileout) ) then
    print*,' File not opened.'
    stop
  endif

  fileout%nwih=filein%nwih
  fileout%num_values=filein%num_values
  fileout%dt=filein%dt
  fileout%tmax=fileout%dt*(fileout%num_values-1)
  status=trcio_writeheader(fileout)
  call trcio_headerdump(fileout)
  
  j = 14 
  do i = filein%num_traces,1+2*filein%num_traces/3, -filein%num_traces/12
    status=trcio_read_trace(filein,hd,tr,i)
    if(status /= trcio_ok) then 
      print*,' Tried to read trace ',i,' but failed.'
      cycle
    endif
    status=trcio_write_trace(fileout,hd,tr,j)
    print*,'Wrt. tr# ',j,' hd1=',hd(1),' max(|tr|)=',maxval(abs(tr)),' status= ',status
    j = j-1
  end do

  j=9
  print*,' Numtr= ',trcio_get_number_traces(fileout)
  call trcio_headerdump(fileout)

  do i = 2*filein%num_traces/3, 1+filein%num_traces/3, -filein%num_traces/12
    status=trcio_read_trace(filein,hd,tr,i)
    if(status /= trcio_ok) then 
      print*,' Tried to read trace ',i,' but failed.'
      cycle
    endif
    status=trcio_write_trace(fileout,hd,tr,j)
    print*,'Wrt. tr# ',j,' hd1=',hd(1),' max(|tr|)=',maxval(abs(tr)),' status= ',status
    j = j-1
  end do

  print*,' Numtr= ',trcio_get_number_traces(fileout)
  call trcio_headerdump(fileout)

  j=4
  do i = filein%num_traces/3,1, -filein%num_traces/12
    status=trcio_read_trace(filein,hd,tr,i)
    if(status /= trcio_ok) then 
      print*,' Tried to read trace ',i,' but failed.'
      cycle
    endif
    status=trcio_write_trace(fileout,hd,tr,j)
    print*,'Wrt. tr# ',j,' hd1=',hd(1),' max(|tr|)=',maxval(abs(tr)),' status= ',status
    j = j-1
  end do

  print*,' Numtr= ',trcio_get_number_traces(fileout)
  call trcio_headerdump(fileout)

  if(trcio_close(filein) /= trcio_ok) stop 'Error closing input.' 
  if(trcio_close(fileout) /= trcio_ok) stop 'Error closing output.' 

  mode='r+'
  filein => trcio_open(nameout,mode)
  if(.not. associated(filein) ) then
    print*,' File not opened.'
    stop
  endif


  call trcio_headerdump(filein)
  do i = 1, 14
    status=trcio_read_trace(filein,hd,tr,i)
    if(status /= trcio_ok) then 
      print*,' Tried to read trace ',i,' but failed.'
      cycle
    endif
    print*,'Read tr# ',i,' hd1=',hd(1),' max(|tr|)=',maxval(abs(tr)),' status= ',status
  end do

  j=15
  print*,' Numtr= ',trcio_get_number_traces(filein)
  do i = 1, 10 
    status=trcio_read_trace(filein,hd,tr,i)
    if(status /= trcio_ok) then 
      print*,' Tried to read trace ',i,' but failed.'
      cycle
    endif
  print*,' hd(1) = ',hd(1),' tr(1) = ',tr(1), ' j = ',j
    status=trcio_write_trace(filein,hd,tr,j)
    print*,'Wrt. tr# ',j,' hd1=',hd(1),' max(|tr|)=',maxval(abs(tr)),' status= ',status
    j = j+1
  end do

  status=trcio_read_trace(filein,hd,tr,1)
  do i = 25, 36
    status=trcio_write_trace(filein,hd,tr,i)
    print*,'Wrt. tr# ',i,' hd1=',hd(1),' max(|tr|)=',maxval(abs(tr)),' status= ',status
  end do


  print*,' Numtr= ',trcio_get_number_traces(filein)
  call trcio_headerdump(filein)
  
  if(trcio_close(filein) /= trcio_ok) stop 'Error closing input.' 
  deallocate(tr)
  deallocate(hd)
  call cio_finalize() 
end program testrandom
