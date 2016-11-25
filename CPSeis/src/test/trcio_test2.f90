program test2
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
  use segy_module
  implicit none

  type(trcio_struct),pointer      :: file,outfl

  real                            :: tr(2751),tr2d(2751,10)
  double precision                :: hd(64),hd2d(64,10)
  integer                         :: i,j,ierr,status,k
  character(len=160)              :: str,filename
  character(len=2)                :: mode
  character(len=1)                :: c
  type(segy_ebcdic_hdr)           :: ascii
  type(segy_bin_hdr)              :: bhed

tr(:) = 0.0
hd(:) = 0d0
tr2d(:,:)=0.0
hd2d(:,:)=0d0

  write(6,'(a)',advance='no')'Read File: '
  read(5,'(a)')filename
  mode = 'r' 
  print*,' Opening ',trim(filename),' with mode = ',trim(mode) 
  file => trcio_open(trim(filename),trim(mode))
  if(.not. associated(file) ) then
    print*,' File not opened.'
    stop
  endif
  call trcio_headerdump(file)
  status = cio_fseek(file%lun,file%data_start_pos,0)
  if(status /= 0 ) print*,' seek status: ',status

  print*,' File type is ',file%ftype
  k = 0
  do j = 1, max(10,file%num_traces),max(file%num_traces/11,1)
    tr = 0
    print*, '    STATUS: ',trcio_read_trace(file,hd,tr,j)
    print*, 'HD: ',hd(1),hd(25),hd(9),hd(6),hd(file%nwih)
    print*, 'TR: ',maxloc(tr),maxval(tr), minloc(tr),minval(tr)
    k = k + 1
    if(k > 10) exit
    hd2d(:,k) = hd
    tr2d(:,k) = tr
  end do

  
  write(6,'(a)',advance='no')'Write File: '
  read(5,'(a)')filename
  mode='w'
  nullify(outfl)
  !--removed compressed file option
  !outfl => trcio_open(trim(filename),trim(mode),compressed=.true.,snr=100,&
  !         srate=.002, mtpc=10,nwih=64,ndpt=2751)
  outfl => trcio_open(trim(filename),trim(mode),srate=.002,nwih=64,ndpt=2751)
  if(associated(outfl)) then
    call trcio_headerdump(outfl)
  else
    stop ' Could not open file.'
  endif

  status = trcio_update_header(outfl) 
  if(status /= 0 ) then
    print*,' error in update header: ',status
    stop
  endif
  print*,'Write status = ',trcio_write_trace(outfl,hd2d,tr2d,ntrc=10)
  print*,'Close status (infile)  = ',trcio_close(file)
  print*,'Close status (outfile) = ',trcio_close(outfl)
  mode='r'
  nullify(outfl)
  outfl => trcio_open(trim(filename),trim(mode),nwih=64,ndpt=2751)
  if(associated(outfl)) then
    call trcio_headerdump(outfl)
  else
    stop ' Could not open file.'
  endif

  hd2d=0
  tr2d=0  
  print*, '    STATUS: ',trcio_read_trace(outfl,hd2d,tr2d,tnum=1,ntrc=10)
  do k = 1, 10
    hd = hd2d(:,k)
    tr = tr2d(:,k)
    print*, 'k: ',k,' HD: ',hd(1),hd(25),hd(9),hd(6),hd(outfl%nwih)
    print*, '   ',k,' TR: ',maxloc(tr),maxval(tr), minloc(tr),minval(tr)
  end do


  print*,'write header: ',trcio_writeheader(outfl)
  call trcio_headerdump(outfl)
  print*,'Close status (outfile) = ',trcio_close(outfl)

  print*,'Trying a SEGY file'
  outfl => trcio_open('segy.sgy','w',compressed=.false.)
  if(associated(outfl) ) then
    outfl%ftype='SEGY'
    outfl%num_values=2751
    outfl%endian=1
    outfl%nwih= 60
    outfl%nbits_hd=32
    outfl%wtype='IBM'
    outfl%nbits=32
    outfl%data_start_pos=(/0,3600/)
    outfl%recl=240+4*2751
    bhed%format=1
    bhed%jobid     = 1     
    bhed%lino      = 1     
    bhed%reno      = 1     
    bhed%ntrpr     = 1     
    bhed%nart      = 1     
    bhed%hdt       = nint(.002*1E6)
    bhed%dto       = nint(.002*1E6)
    bhed%hns       = 2751
    bhed%nso       = 2751
    ! bhed%format already done above...
    bhed%fold      = 1     
    bhed%tsort     = 1     
    bhed%vscode    = 1
    bhed%hsfs      = 0     
    bhed%hsfe      = 0     
    bhed%hslen     = 0     
    bhed%hstyp     = 0     
    bhed%schn      = 0     
    bhed%hstas     = 0     
    bhed%hstae     = 0     
    bhed%htatyp    = 0
    bhed%hcorr     = 1     
    bhed%bgrcv     = 2     
    bhed%rcvm      = 1     
    do i = 1, 40
      ascii%h(i) = 'c --------- test ascii header -------------'
    end do
    k = segy_write_ebchdr(outfl%lun,ascii)
    k = segy_write_binhdr(outfl%lun,bhed)
    if(cio_fflush(outfl%lun) /= cio_ok) stop
    if(cio_fseek(outfl%lun,0,2) /= cio_ok) stop
    print*,'Write status = ',trcio_write_trace(outfl,hd2d,tr2d,ntrc=10)
    print*,'write header: ',trcio_writeheader(outfl)
    call trcio_headerdump(outfl)
    print*,'Close status (outfile) = ',trcio_close(outfl)
  
  endif

  nullify(outfl)
  filename='junk.dat'
  mode='w'
  outfl => trcio_open(trim(filename),trim(mode),compressed=.false.,&
           srate=.002, nwih=64,ndpt=2751)
  if(associated(outfl)) then
    call trcio_headerdump(outfl)
  else
    stop ' Could not open file.'
  endif
  status = trcio_update_header(outfl) 
  if(status /= 0 ) then
    print*,' error in update header: ',status
    stop
  endif
  print*,'Write status = ',trcio_write_trace(outfl,hd2d,tr2d,ntrc=10)
  call trcio_headerdump(outfl)
  print*,'Close status (outfile) = ',trcio_close(outfl)

end program test2
