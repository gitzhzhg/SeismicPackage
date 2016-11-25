program test7
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

  type(trcio_struct),pointer      :: file,outfil

  real                            :: tr(1024)
  double precision                :: hd(64)
  integer                         :: i,j,ierr,status,k
  character(len=160)              :: str,filename
  character(len=2)                :: mode
  character(len=1)                :: c


  write(6,'(a)',advance='no')'File: '
  read(5,*)filename
  write(6,'(a)',advance='no')'Mode: '
  read(5,*)mode
 
  print*,' Opening ',trim(filename),' with mode = ',trim(mode) 
  file => trcio_open(trim(filename),trim(mode))
  call trcio_headerdump(file)
  status = cio_fseek(file%lun,file%data_start_pos,0)
  if(status /= 0 ) print*,' seek status: ',status
  print*,' start data on byte ',file%data_start_pos

  print*,' File type is ',file%ftype
  do j = 1, 10
    status = trcio_read_trace(file,hd,tr,2*j)
    print*, 'HD: ',hd(1),hd(9),hd(6),hd(file%nwih)
    print*, 'TR: ',tr(1), tr(512),tr(size(tr))
  end do

  status = trcio_seek_trace(file,1)
  print*,' seek trace 1 status = ',status
end program test7
