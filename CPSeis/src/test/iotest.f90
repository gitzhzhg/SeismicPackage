
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
   program iotest
   use trcio_module
   use cio_module

   character(len=120) :: path
   integer  :: nt,nwrds
   integer  :: nchunks=6
   integer  :: chunks(6)
   integer  :: irecs(6)
   real     :: tr(500,62)
   real     :: dt=0.008
   double precision :: hd(64,62)
   type(trcio_struct),pointer      :: file
   integer(kind=8) :: nsize8
   integer         :: ext(2), ierr
      integer      :: num_traces
      integer      :: trace_number
   
   chunks(1)=10
   chunks(2)=13
   chunks(3)=10
   chunks(4)=9
   chunks(5)=9
   chunks(6)=11
   irecs(1)=53
   irecs(2)=40
   irecs(3)=30
   irecs(4)=21
   irecs(5)=12
   irecs(6)=1
   nt=500
   tr=0.0
   hd=0.0
   do i=1,62
     hd(1,i) = i
     hd(14,i) = 10.0*i
     hd(15,i) = 10.0*i + 5.0
     hd(16,i) = 20.0
   enddo

   nwrds = 62 * (nt + 64) + 4096
   nsize8 = 4*nwrds + 512
   ext(1) = nsize8/256000000 + 1
   ext(2) = 0
    !ext(1) = 0
    !ext(2) = 1
   ierr  = cio_set_file_ext_size(ext)

   nt = 500
   
   path ='iotest.trc'
   file => trcio_open(path,'w',.false.)
   ierr = trcio_set_num_values(file,nt)
   ierr = trcio_set_dt(file,dt)
   ierr = trcio_set_nbits(file,32)
   ierr = trcio_set_nbits_hd(file,32)
   ierr = trcio_set_tmin(file,0.0)
   ierr = trcio_set_tmax(file,(nt-1)*dt)
   ierr = trcio_set_ftype(file,'TRCIO')   !LBO2 | TRCIO | SEGY
   file%nwih       = 64

   do i=1,nchunks
      num_traces = trcio_get_number_traces(file)
      trace_number = trcio_tell_trace(file)
      print *,' num_traces=',num_traces,' trace_number=',trace_number
      irec = irecs(i)
      if(irec .ne. trace_number ) print *,' irec=',irec
      ierr = trcio_write_trace(file,hd,tr,tnum=irecs(i),ntrc=chunks(i))
      if(ierr .ne. trcio_ok) then
        print *, 'ERROR IN WRITE TRACE ', irecs(i),' i=',i
        stop
      endif
   enddo
   ierr = trcio_close(file)
   end program
