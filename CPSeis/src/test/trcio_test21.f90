program test21
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
  implicit none

  type(trcio_struct),pointer      :: file
  real,allocatable,dimension(:)   :: tr
  double precision,dimension(:),allocatable :: hd
  integer                         :: i
  character(len=160)              :: filename
  character(len=2)                :: mode
  double precision                :: trmaxg

  filename = ""
  write(6,'(A)',advance="no")'File to open: '
  read(5,'(a)') filename
  !filename = '/home/mengewm/cpsdata/test.trc'
  filename=trim(filename)//' '
  mode     = 'r'
 
  print*,' Opening ',trim(filename),' with mode = ',trim(mode) 
  file => trcio_open(filename,mode)
  if(.not. associated(file) ) then
    print*,' File not opened.'
    stop
  endif

  call trcio_headerdump(file)

  allocate(tr(file%num_values))
  allocate(hd(file%nwih))

  trmaxg = 0d0
  print*,'Estimated ',file%num_traces,' in the file.'

  do i = 1, file%num_traces
    if(trcio_read_trace(file,hd,tr) /= trcio_ok) exit
    trmaxg = max(1d0*maxval(abs(tr)),trmaxg)
    print*,'I: ',i,' TRMAXG: ',trmaxg,'H: ',hd(1),hd(25),' T: ',maxval(abs(tr))
  end do

  if(trcio_close(file) /= trcio_ok) stop 'Error closing file' 

  deallocate(tr)
  deallocate(hd)
 
end program test21
