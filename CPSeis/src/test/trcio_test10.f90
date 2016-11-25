program test10
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
  use cpsio_module
  use cio_module
  implicit none

  type(trcio_struct),pointer      :: file,scrfile

  real                            :: tr(8192)
  double precision                :: hd(64)
  integer                         :: i,j,ierr,status,k,trcnum
  character(len=160)              :: str,filename
  character(len=2)                :: mode
  character(len=1)                :: c


  write(6,'(a)',advance='no')'File: '
  read(5,'(a)')filename
  write(6,'(a)',advance='no')'Mode: '
  read(5,'(a)')mode
 
  print*,' Opening ',trim(filename),' with mode = ',trim(mode) 
  file => trcio_open(filename,mode)
  if(.not. associated(file) ) then
    print*,' File not opened.'
    stop
  endif
  call trcio_headerdump(file)
  
  print*,' Enter trace number to seek: '
  read*,trcnum
  status = trcio_seek_trace(file,trcnum)
  if(status /= 0 ) print*,' seek status: ',status
  trcnum = trcio_tell_trace(file)
  print*,'File positioned on trace ',trcnum

  status = trcio_close(file)

end program test10
