program test8
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
  !use hist_module
  implicit none

  type(trcio_struct),pointer      :: file,scrfile

  real                            :: tr(8192)
  double precision                :: hd(64)
  integer                         :: i,j,ierr,status,k,n,nval,num_global_cards
  character(len=pc_datacard_length),pointer,dimension(:) :: global_cards
  character(len=160)              :: str,filename,errmsg
  character(len=200)              :: card
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

  num_global_cards = trcio_num_global_cards(file)
  print*,' NUMBER GLOBAL CARDS = ',num_global_cards
  if(num_global_cards > 0 ) then
    allocate(global_cards(num_global_cards))
    status = trcio_read_globals(file,num_global_cards,global_cards)
    if(status /= 0 ) call pc_error('trot: error reading TRCIO job globals.')
    do i = 1, num_global_cards
      print*,trim(global_cards(i))
    end do
    deallocate(global_cards)
  endif

  nval = file%num_values
  !if(modulo(nval,2) /= 0) nval = nval + 1

  status = cio_remove('%OHISTY')
  print*,' delete status of ohisty ',status
  status = cio_remove('%NHISTY')
  print*,' delete status of nhisty ',status


  scrfile => trcio_open('~/cpsdata/junk.dat','w',.false.,file%nwih,nval,&
  !scrfile => trcio_open('junk.dat','w',.false.,file%nwih,nval,&
  file%nbits,file%nbits_hd)
  if(.not. associated(scrfile) ) then
    print*,' junk File not opened.'
    stop
  endif

  call trcio_headerdump(file)

      CALL PC_BACKEND_UPDATE(6)
      CALL PC_PUT_GLOBAL('DT',file%dt)
      CALL PC_PUT_GLOBAL('NDPT',file%num_values)
      CALL PC_PUT_GLOBAL('TSTRT',file%tmin)
      CALL PC_PUT_GLOBAL('NWIH',file%NWIH)
      CALL PC_PUT_GLOBAL('sopwith','camel')
      call pc_put_global('array',(/'1','2','3','4'/),4)
      CALL PC_SET_IPN(1)
      CALL PC_PUT_PDATA('USER_NAME','MENGEWM')
      CALL PC_SET_IPN(2)
      CALL PC_PUT_JDATA('DT',file%dt)
      CALL PC_PUT_JDATA('NDPT',file%num_values)
      CALL PC_PUT_JDATA('TSTRT',file%tmin)
      CALL PC_PUT_JDATA('NWIH',file%nwih)
      CALL PC_PUT_JDATA('JOBNAME','TESTJOB')

  call trcio_read_history_cards(file,status)
  if(status /= 0 ) print*,' read history cards status: ',status
  
  status = cio_fseek(file%lun,file%data_start_pos,0)
  if(status /= 0 ) print*,' seek status: ',status

  !call history_print('both')



  print*,' File type is ',file%ftype
  write(6,'(a)',advance='no')'Num Traces: '
  read(5,*)n
  status = trcio_writeheader(scrfile)
  if(status /= 0 ) print*,'trot: error writing TRCIO file header.'

      num_global_cards = pc_num_global_cards()
      call pc_alloc_global_cards    (global_cards, num_global_cards)
      call pc_get_global_cards(global_cards,num_global_cards,errmsg)
      status = trcio_write_globals(scrfile,num_global_cards,global_cards)
      deallocate(global_cards)
      if(status /= 0 ) call pc_error('trot: error writing TRCIO job globals.')
 

  call trcio_write_history_cards(scrfile,'ALL')
  print*,' Wrote history to file.'
  do j = 1, min(n,file%num_traces)
    tr = 0
    write(card,'(a,i7,a,i3,a)')'error reading trace ',j,' from unit ',file%lun,'.'
    if(trcio_read_trace(file,hd,tr,j) /= trcio_ok) exit
    write(card,'(a,i7,a)')'error flushing unit ',scrfile%lun,'.'
    if(cio_fflush(scrfile%lun) /= cio_ok ) exit
    write(card,'(a,i7,a,i3,a)')'error Writing trace ',j,' from unit ',scrfile%lun,'.'
    if(trcio_write_trace(scrfile,hd,tr,j) /= trcio_ok ) exit
    hd = 0
    tr = 0
    write(card,'(a,i7,a)')'error flushing unit ',scrfile%lun,'.'
    if(cio_fflush(scrfile%lun) /= cio_ok ) exit
    write(card,'(a,i7,a,i3,a)')'error reading trace ',j,' from unit ',scrfile%lun,'.'
    if(trcio_read_trace(scrfile,hd,tr,j) /= trcio_ok ) exit
    write(card,'(a,6(f10.2,1x))') &
       ' Wrote HD: ',hd(1),hd(9),hd(6),hd(file%nwih),maxval(tr),hd(25)
    write(6,'(a)')trim(card)
  end do
  if(j < n ) write(6,'(a)')trim(card)

  status = trcio_close(file)
  print*,' close status: ',status
  status = trcio_close(scrfile)
  print*,' junk close status: ',status
  call pc_restore

  !call history_phist('YES','STDOUT')
  print*,'----------------------------------------------------------------'

  scrfile => trcio_open('~/cpsdata/junk.dat','r')
  
  num_global_cards = trcio_num_global_cards(scrfile)
  print*,' NUMBER GLOBAL CARDS = ',num_global_cards
  if(num_global_cards > 0 ) then
    allocate(global_cards(num_global_cards))
    status = trcio_read_globals(scrfile,num_global_cards,global_cards)
    if(status /= 0 ) call pc_error('trot: error reading TRCIO job globals.')
    do i = 1, num_global_cards
      print*,trim(global_cards(i))
    end do
    deallocate(global_cards)
  endif

print*,' Done with that, now print traces.'  
  do j = 1, scrfile%num_traces
    print*,' Reading trace ',j
    write(card,'(a,i7,a,i3,a)')'error reading trace ',j,' from unit ',scrfile%lun,'.'
    if(trcio_read_trace(scrfile,hd,tr,j) /= trcio_ok) then
      write(6,'(a)')trim(card)
      exit
    endif
    write(6,'(a,6(f10.2,1x))') &
       ' Wrote HD: ',hd(1),hd(9),hd(6),hd(scrfile%nwih),maxval(tr),hd(25)
  end do
  
  status = trcio_close(scrfile)

  stop 'finished.'
end program test8
