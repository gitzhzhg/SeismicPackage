program test13
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
  use pc_module
  use trcio_module
  use cpsio_module
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

  nval = file%num_values

  unit=cio_fopen('~/cpsdata/junk.dat','w')
  if(unit < 0 ) then
    print*,' junk File not opened.'
    stop
  endif
  print*,' Opened a junk file on unit ',unit

  call trcio_headerdump(file)

  status = cio_fseek(file%lun,file%data_start_pos,0)
  if(status /= 0 ) print*,' seek status: ',status

  print*,' File type is ',file%ftype
  write(6,'(a)',advance='no')'Num Traces: '
  read(5,*)n


  hd = 0
  tr = 0
  call cio_frewind(unit)

  do j = 1, min(n,file%num_traces)

    write(card,'(a,i7,a,i3,a)')'error reading trace ',j,' from unit ',file%lun,'.'
    if(trcio_read_trace(file,hd,tr,j) /= trcio_ok) exit

    write(card,'(a,i7,a,i3,a)')'error seeking to hdr  ',j,' on unit ',unit,'.'
    if(cio_fseek(unit,(j-1)*(512+nval*4),0) /= cio_ok) exit
    write(card,'(a,i7,a,i3,a)')'error Writing header ',j,' to unit ',unit,'.'
    if(cio_fwrite(hd,8,64,unit) /= 64 ) exit
    write(card,'(a,i7,a,i3,a)')'error Writing trace ',j,' to unit ',unit,'.'
    if(cio_fwrite(tr,4,nval,unit) /= nval ) exit
    hd = 0;  tr = 0

  end do
  if( j < n) write(6,'(a)')trim(card)
  n = j - 1

  write(card,'(a,i7,a)')'error flushing unit ',unit,'.'
  if(cio_fflush(unit) /= cio_ok ) write(6,'(a)')card

  card = ''

  do j = 1, n
    write(card,'(a,i7,a,i3,a)')'error seeking to hdr  ',j,' on unit ',unit,'.'
    if(cio_fseek(unit,(j-1)*(512+nval*4),0) /= cio_ok) exit
    i = cio_ftell(unit);print*,' Unit ',unit,' is at byte ',i

    nread = cio_fread(hd,8,64,unit)
    write(card,'(a,i7,a,i3,a,i7)')'error reading header ',j,' from unit ',unit,'.  nread= ',nread
    if(nread < 0 ) exit

    nread = cio_fread(tr,4,nval,unit) 
    write(card,'(a,i7,a,i3,a,i7)')'error reading trace ',j,' from unit ',unit,'.  nread= ',nread
    if(nread < 0 ) exit
    x = maxval(tr)
    write(card,'(A,6F14.7)')' read HD: ',hd(1),hd(9),hd(6),hd(file%nwih),x,hd(25)
    write(6,'(a)')trim(card)
  end do
  write(6,'(a)')trim(card)

  file%ftype='JUNK'
  status = trcio_update_header(file)
  print*,'status = ',status

  status = trcio_close(file)
  print*,' close status: ',status
  status = cio_fclose(unit)
  print*,' junk close status: ',status

end program test13
