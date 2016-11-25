program test11
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

  integer ,parameter              :: ns =  511
  integer ,parameter              :: nhw = 64
  real                            :: tr(ns)
  double precision                :: hd(nhw)
  integer                         :: i,j,status
  character(len=160)              :: filename
  character(len=2)                :: mode

  filename = 'qtrot.dat'
  mode     = 'w'
 
  print*,' Opening ',trim(filename),' with mode = ',trim(mode) 
  file => trcio_open(filename,mode)
  if(.not. associated(file) ) then
    print*,' File not opened.'
    stop
  endif
  file%endian            = 1
  file%nbits_hd          = 32
  file%ftype             = 'QTROT'
  file%wtype             = 'IEEE'
  file%num_values        = ns
  file%nwih              = nhw
  file%recl              = 4*(file%nwih+file%num_values)
  file%dt                = .004
  file%tmin              = 0.0
  file%tmax              = file%tmin + file%dt*(file%num_values-1)
  file%common%xorigin           = 111.111
  file%common%yorigin           = 222.222
  file%common%dx11              = 1
  file%common%dx12              = 2
  file%common%dx21              = 3
  file%common%dx22              = 4
  file%num_traces        = 10

  file%data_start_pos(1) = 0
  file%data_start_pos(2) = 4096
  
  if(modulo(file%num_values,2) /= 0) file%num_values  = file%num_values+1
  call trcio_headerdump(file)

  do i = 1, file%num_traces
    do j = 2, file%nwih
      hd(j) = j
    end do
    hd(1) = i
    hd(nhw) = ns
    do j = 1, ns
      tr(j) = cos(10*3.14159*(j)*file%dt)
    end do
    if(trcio_write_trace(file,hd,tr,i) /= trcio_ok) stop'Error writing trace'  
  end do

  if(trcio_close(file) /= trcio_ok) stop 'Error closing file' 

  mode = 'r'

  nullify(file)

  print*,' Opening ',trim(filename),' with mode = ',trim(mode) 
  file => trcio_open(filename,mode)
  if(.not. associated(file) ) then
    print*,' File not opened.'
    stop
  endif

  call trcio_headerdump(file)
  !if(trcio_seek_trace(file,1)/= trcio_ok) stop 'seek err.'
  do i = 1, file%num_traces
    if(trcio_read_trace(file,hd,tr,i) /= trcio_ok) stop ' Error reading trc.'
    print*,'TRACE: ',i
    print*,'HD: ',hd
    print*,'TR: ',tr
  end do

  if(trcio_close(file) /= trcio_ok) stop 'Error closing file' 
 
end program test11
