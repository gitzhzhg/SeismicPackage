program test14
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
  use unix_module
  use cio_module
  implicit none

  type(trcio_struct),pointer      :: file

  real                            :: tr(8192)
  double precision                :: hd(64),x
  integer                         :: i,j,ierr,status,k,n,nval,unit,nread,lun
  character(len=160)              :: str,filename
  character(len=150)              :: card
  character(len=2)                :: mode
  character(len=1)                :: c


  filename='junk.dat'
  mode = 'w'
  print*,' Opening ',trim(filename),' with mode = ',trim(mode) 
  file => trcio_open(filename,mode,nwih=64,ndpt=372,nbits=32,nbitshd=64)
  if(.not. associated(file) ) then
    print*,' File not opened.'
    stop
  endif

  !call trcio_headerdump(file)

  call cio_frewind(file%lun)
  if(cio_fflush(file%lun) < 0 ) continue
  if(unix_system('cat junk.dat') < 0 ) continue
  if(unix_system('wc junk.dat') < 0 ) continue

  file%hist_start_pos = (/0,1/)
  file%hist_end_pos = (/0,2/)
  file%endian=2
  file%wtype='IBM'
  file%nbits=32
  file%nbits_hd=64
  file%tmin = 0.1
  file%tmax = 0.471
  file%dt = .001
  file%nhd1 = 1
  file%nhd2 = 2
  file%nhd3 = 3
  file%vwidth1 = 1.1
  file%vwidth2 = 2.2
  file%vwidth3 = 3.3
  file%nwih    = 64
  file%num_values = 372
  file%vbin1 = 1.11
  file%vbin2 = 2.22
  file%vbin3 = 3.33
  file%vmin1 = 1.111
  file%vmin2 = 2.222
  file%vmin3 = 3.333
  file%vmax1 = 1.1111
  file%vmax2 = 2.2222
  file%vmax3 = 3.3333
  file%common%xorigin=-111.111
  file%common%yorigin=-222.222
  file%common%dx11=11
  file%common%dx12=12
  file%common%dx21=21
  file%common%dx22=22
  !file%num_traces=10000
  !file%recl=1234
  !file%data_start_pos = (/3,3/)
  !file%data_end_pos = (/3,20000004/)
  !file%trmaxg=-1234.5678
  do i = 1, 10
    hd  = i
    tr  = sqrt(1.0*i)
    status = trcio_write_trace(file,hd,tr)
  end do


  !call trcio_headerdump(file)
  status = trcio_close(file)
  if(unix_system('cat junk.dat') < 0 ) continue
  if(unix_system('wc junk.dat') < 0 ) continue
  print*,' close status: ',status

end program test14
