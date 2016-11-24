**
* Center for Wave Phenomena:
* $Author: jkc $ ; $Source: /usr/local/src/prop/local/Cz/RCS/cz1.f,v $
* $Revision: 2.5 $ ; $Date: 90/05/25 15:05:47 $
**

************************************************************************
*                                                                      *
* CZ1 - Weight and filter traces.                                      *
*                                                                      *
* Purpose:                                                             *
*    This is the first phase of the Cohen - Hagin velocity             *
*    inversion algorithm, i.e. the integrals over frequency and        *
*    time performed via fast fourier transform. It is our intent       *
*    that the user modify the program to take advantage of any         *
*    special hardware which can perform the fft's faster than          *
*    the cpu can.                                                      *
*                                                                      *
*                                                                      *
* Credits:                                                             *
*    Algorithm - Hagin and Cohen                                       *
*    Design, implementation, and code - Sumner                         *
*                                                                      *
* This program belongs to the Center For Wave Phenomena                *
* Colorado School of Mines.                                            *
*                                                                      *
* Copyright 1986 by Center For Wave Phenomena                          *
************************************************************************

      program cz1

************************************************************************
*
* Globals:
*
*    CCOS(), CSIN() - hold cosines and sines for fft
*    COUNT - for do loop
*    CZ1INF - unit number for runtime information file
*    CZ2DF - unit number for information validated here for CZ2
*    DF - frequency spacing
*    DT - time spacing on trace
*    FACMAX - maximum number of factors of nfft
*    FACS() - contains factors of nfft
*    FILTER() - holds weighted frequency filter
*    FLL, FHL, FLR, FHR - filter parameters
*    FLTR - holds filter values
*    I - general array index
*    IFHRP1 - ifhr plus 1
*    IFLL, IFHR - integer versions of filter parameters
*    IFLLM1 - ifll minus 1
*    NCNT - times to execute main loop
*    NFAC - number of factors of nfft
*    NFFT - number of points in fft
*    NFFTM1 - NFFT minus 1
*    NPTS - number of points on output trace
*    NPTSM1, NPTSM2, NPTSM3, NPTSM4 - NPTS minus n
*    NSKIP - number of points at the beginning of the trace to skip
*    NTRACE - number of traces
*    PTF -  unit number for processed trace file
*    PTSMAX - maximum number of points on a trace
*    STDOUT - unit number for standard output
*    TAP1, TAP2, TAP3 - weights for taper
*    TF - unit number for trace file
*    TFIN - final time on traces
*    TI(), T1I(), T2I() - hold imaginary parts of traces
*    TINIT - initial time on traces
*    TR(), T1R(), T2R() - hold real parts of traces
*    TRACES - name of input trace file
*    TSKIP - time to skip at beginning of each trace
*    WR(), WI() - work area for fft
*
************************************************************************

      real    pi     , tap1   , tap2   , tap3

      parameter (pi = 3.14159265358979,
     :           tap1 = 0.3826834324,
     :           tap2 = 0.5,
     :           tap3 = 1.0 - tap1)

      integer stdout , tf     , ptf    , cz1inf , cz2df ,
     :        ptsmax , facmax

      parameter (stdout = 6,
     :           cz1inf = 8,
     :           cz2df = 9,
     :           tf = 10,
     :           ptf = 11,
     :           ptsmax = 2047,
     :           facmax = 12)


      real    tinit  , tfin   , dt     , fll    , fhl    ,
     :        flr    , fhr    , df     , fltr   , tskip

      real
     :        ccos(0:ptsmax), csin(0:ptsmax), filter(0:ptsmax),
     :        tr(0:ptsmax),   t1r(0:ptsmax),  t2r(0:ptsmax),
     :        ti(0:ptsmax),   t1i(0:ptsmax),  t2i(0:ptsmax),
     :        wr(0:ptsmax),   wi(0:ptsmax)

      integer ntrace  , npts   , nptsm1 , nfft   , nskip  ,
     :        nfftm1  , count  , ifll   , ifllm1 , ifhr   ,
     :        ifhrp1  , i      , nfac   , ncnt   , nptsm2 , nptsm3 ,
     :        nptsm4

      integer
     :        facs(0:facmax)


      character traces*15

**
* Revision control system identifier string.
**

      character cwpid*28, progid*80, rcsid*80
      parameter (
     :    cwpid = ' Center for Wave Phenomena: ',
     :    progid =
     : ' $Source: /usr/local/src/prop/local/Cz/RCS/cz1.f,v $',
     :    rcsid = ' $Revision: 2.5 $ ; $Date: 90/05/25 15:05:47 $')

**
* Code begins. Open i/o devices.
**

*     open(unit = stdout)
      write(stdout, '(//a/a/a/a//)') '----', cwpid, progid, rcsid

**
* Get run parameters.
**

      open(unit = cz1inf, file = 'DATA1', form = 'unformatted',
     :     err = 2)
	 go to 4
   2     write(stdout, '(a/a)')
     :         ' ***Can not access run time data: DATA1',
     :         ' ***   Should have been created by CZ1IN'
	 stop
   4  continue
      rewind cz1inf
      read(cz1inf) ntrace,tinit,tfin,tskip,dt,fll,fhl,flr,fhr,traces
      close(unit=cz1inf, status = 'delete')

**
* Open trace file, make sure it's actually there.
**

      open(unit = tf, file = traces, access = 'sequential',
     :     form  = 'unformatted', status = 'old', err = 10)
      go to 20
  10     write(stdout, '(a,a)')
     :         ' ***Can not open trace file: ',traces
	 stop
  20  continue
      rewind tf


**
* Create and open CZ2 validated data file.
**

      open(unit = cz2df, file = 'CZ2DAT', access = 'sequential',
     :     form = 'unformatted',  status = 'new', err = 25)
      go to 30
  25     write(stdout, '(a,a/a)')
     :         ' ***Error creating processed trace file: CZ2DAT',
     :         ' ***   (Remove old versions)'
	 stop
  30  continue


**
* Create and open processed data file.
**

      open(unit = ptf, file = 'PTRACES', access = 'sequential',
     :     form = 'unformatted',  status = 'new', err = 35)
      go to 40
  35     write(stdout, '(a,a/a)')
     :         ' ***Error creating processed trace file: PTRACES',
     :         ' ***   (Remove old versions)'
	 stop
  40  continue

      nskip = nint(tskip/dt)
      npts = nint((tfin-tinit)/dt) + 1
      npts = npts-nskip
      nptsm1 = npts - 1
      nptsm2 = nptsm1 - 1
      nptsm3 = nptsm2 - 1
      nptsm4 = nptsm3 - 1

**
* Calculate fft size.
**

      call fftfac(npts,nfft,nfac,facs)
      nfftm1 = nfft - 1

      write(stdout,'(a,i4,a,i4)') ' Points on trace: ', npts,
     :                            ' FFT size: ', nfft


**
* Ready to process traces.  First write validated data for CZ2.
**

      write(cz2df) ntrace,tinit,tfin,tskip,dt,fll,fhl,flr,fhr


**
* Make fft and filter tables.
**

      call ffttab(nfft,ccos,csin)
      call tabfil(nfft,fll,fhl,flr,fhr,dt,filter)
      df = 1.0/(nfft*dt)
      ifll = nint(fll/df)
      ifllm1 = ifll - 1
      ifhr = nint(fhr/df)
      ifhrp1 = ifhr + 1

**
* Now ready to filter traces. We fft two real traces simultaneously.
* Notice we taper off sides and bottom to avoid a sudden drop to
* zero being migrated into the output of the next program.
**

      ncnt = ntrace/2
      do 80 count = 1, ncnt

	 call gettra(tr,tf,nskip,npts)
	 call gettra(ti,tf,nskip,npts)

	 do 60 i = npts, nfftm1
	    tr(i) = 0.0
	    ti(i) = 0.0
  60     continue

	 call fft(tr,ti,wr,wi,nfft,facs,nfac,ccos,csin)

**
* Now split traces apart and filter. we know where filter is zero.
**
	 do 65 i = 0, ifllm1
	    t1r(i) = 0.0
	    t1i(i) = 0.0
	    t2r(i) = 0.0
	    t2i(i) = 0.0
   65    continue

       do 70 i = ifll, ifhr
	    fltr = filter(i)
	    t1r(i) = fltr*(tr(i)+tr(nfft-i))
	    t2i(i) = fltr*(tr(i)-tr(nfft-i))
	    t1i(i) = -fltr*(ti(i)-ti(nfft-i))
	    t2r(i) = fltr*(ti(i)+ti(nfft-i))
  70     continue

	 do 75 i = ifhrp1, nfftm1
	    t1r(i) = 0.0
	    t1i(i) = 0.0
	    t2r(i) = 0.0
	    t2i(i) = 0.0
  75     continue

**
* Now invert the transform. Notice the complex conjugates above.
* Then write processed traces.
**

	 call fft(t1r,t1i,wr,wi,nfft,facs,nfac,ccos,csin)
	 call fft(t2r,t2i,wr,wi,nfft,facs,nfac,ccos,csin)
	 if (count .gt. 2 .and. count .lt. ncnt-1) then
	    write(ptf) (t1r(i)+t1i(i), i=0,nptsm4),
     :                 tap3*(t1r(nptsm3)+t1i(nptsm3)),
     :                 tap2*(t1r(nptsm2)+t1i(nptsm2)),
     :                 tap1*(t1r(nptsm1)+t1i(nptsm1))
	    write(ptf) (t2r(i)+t2i(i), i=0,nptsm4),
     :                 tap3*(t2r(nptsm3)+t2i(nptsm3)),
     :                 tap2*(t2r(nptsm2)+t2i(nptsm2)),
     :                 tap1*(t2r(nptsm1)+t2i(nptsm1))
	 else if (count .eq. 1) then
	    write(ptf) (tap1*(t1r(i)+t1i(i)), i=0,nptsm1)
	    write(ptf) (tap2*(t2r(i)+t2i(i)), i=0,nptsm1)
	 else if (count .eq. 2) then
	    write(ptf) (tap3*(t1r(i)+t1i(i)), i=0,nptsm1)
	    write(ptf) (t2r(i)+t2i(i), i=0,nptsm1)
	 else if (count .eq. ncnt) then
	    write(ptf) (tap2*(t1r(i)+t1i(i)), i=0,nptsm1)
	    write(ptf) (tap1*(t2r(i)+t2i(i)), i=0,nptsm1)
	 else
	    write(ptf) (t1r(i)+t1i(i), i=0,nptsm1)
	    write(ptf) (tap3*(t2r(i)+t2i(i)), i=0,nptsm1)
	 endif

  80  continue

**
*        ...There may be a last trace left and since we are tapering,
*        ...we will just write zeroes and advance the input trace
*        ...file by one.
**

         if (ntrace .ne. 2*ncnt) then
	    call gettra(tr,tf,nskip,npts)
	    write(ptf) (0.0, i=0,nptsm1)
         endif


**
* Processing finished. Close files and stop.
**

      close(unit = tf , status = 'keep')
      close(unit = ptf, status = 'keep' , err = 120)

      write(stdout, '(a)') ' Files closed. Processing completed.'
      stop

 120  write(stdout, '(a)')
     :      ' ***Error saving processed trace file: PTRACES'
      stop

**
* Thats all folks
**

      end


*****************************************************************
*
* TABFIL - Compute and store "filter"
*
*          actually stores f(freq) * const * sqrt(freq)
*
* Parameters:
*    N - length of filter
*    FLL - freq. where filter becomes non-zero
*    FHL - freq. where filter rises to 1 (scaled)
*    FLR - freq. where filter starts to drop
*    FHR - freq. where filter again zero
*    DT - time spacing on trace
*    F() - array to store filter
*
****************************************************************

      subroutine tabfil(n,fll,fhl,flr,fhr,dt,f)

      real fll,fhl,flr,fhr,dt,f(0:*)
      integer n

**
* Local variables:
*    DF - frequency interval corresponding to time interval dt
*    S - stores sine
*    C - some constant
*    K - an important constant: 1/n for the inverse transform, times
*        1/2a where a is the filter area, times 1/2 for the even-odd
*        split of the traces. The 1/2a ensures we don't mess up
*        amplitudes.
*    PIBY2 - constants
*    IFLL, IFHL, IFLR, IFHR, IFREQ - integer versions of the
*                                    parameters
**

      real df,s,c,k

      integer ifll,ifhl,iflr,ifhr,ifreq

      real piby2
      parameter (piby2 = 1.57079632679489)


**
* Calculate constants. Discretize in frequency.
* Note we don't care what is in the unused portions of the filter.
**

      df = 1.0/(n*dt)
      ifll = nint(fll/df)
      ifhl = nint(fhl/df)
      iflr = nint(flr/df)
      ifhr = nint(fhr/df)
      k = 1.0 / (n * df*(ifhr+iflr-ifhl-ifll)) * 0.5 * sqrt(df)

**
* The middle range of the filter is easy.
**

      do 20 ifreq = ifhl, iflr
	 f(ifreq) = k * sqrt(real(ifreq))
  20  continue

**
* For the sides of the filter use sin(x)**2 curves or at least
* jump only to 0.5 to lessen ringing.
*
* First the left side.
**

      if (ifll .eq. ifhl) then
	 f(ifll) = 0.5 * k * sqrt(real(ifll))
      else
	 c = piby2 / (ifhl - ifll)
	 do 30 ifreq = ifll, ifhl
	    s = sin(c*(ifreq - ifll))
	    f(ifreq) = s * s * k * sqrt(real(ifreq))
  30     continue
      endif

**
* Now the right side.
**

      if (iflr .eq. ifhr) then
	 f(iflr) = 0.5 * k * sqrt(real(iflr))
      else
	 c = piby2 / (ifhr - iflr)
	 do 40 ifreq = iflr, ifhr
	    s = sin(c * (ifhr - ifreq))
	    f(ifreq) = s * s * k * sqrt(real(ifreq))
  40     continue
      endif

**
* All done.
**

      return
      end


************************************************************************
*
* GETTRA - get a trace
*
*   * * * Note: The user should modify this subroutine to comply with
*               the particular format used to store traces at the
*               installation. This is the barest possible example.
*
* Parameters:
*   TR - where to put trace data
*   NPTS - number of points to read after skipping.
*   NSKIP - number of points to skip
*   TF - unit number of trace file
*
*************************************************************************

       subroutine gettra(tr,tf,nskip,npts)
       integer tf, nskip, npts
       real tr(0:*)

**
* Local variables:
*   SKIPD - data to skip
*   I - counter
**

      integer i
      real skipd

      read (tf) (skipd, i=1,nskip), (tr(i), i=0,npts-1)

      return
      end
