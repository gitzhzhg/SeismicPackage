**
* Center for Wave Phenomena:
**

************************************************************************
*  CZ1IN - Prepare data for CZ1                                        *
*                                                                      *
*  Purpose:                                                            *
*     This program checks input data and prepares a binary file of     *
*     runtime data for CZ1 using the bounds specified in BOUNDS.       *
*     This allows CZ1 to be a non interactive module.                  *
*                                                                      *
*  Credits:                                                            *
*     Algorithm, design and code - Sumner                              *
*                                                                      *
*  Copyright 1986 by Center for Wave Phenomena                         *
************************************************************************

      program cz1in

************************************************************************
*
* Globals:
*
*    BNDS - unit number for bounds file
*    CZ1INF - unit number for binary runtime info file for CZ1
*    DEPMAX - maximum allowable depth
*    DIE - true when input error encountered
*    DPSMAX - maximum allowable number of reflector dip samples
*    DSMAX - maximum allowable number of depth samples
*    DT - Time sample rate
*    DTMIN, DTMAX - allowable bounds on DT, the time sample rate
*    DXMIN, DXMAX - allowable bounds on DX, the spatial sample distance
*    FLL, FHL, FLR, FHR - filter parameters
*    FMIN, FMAX - allowable bounds on frequencies
*    IPWDTH - input panel width, minus 1
*    NTRACE - number of traces in time section
*    OPWDTH - output panel width, minus 1
*    SPTS - number of points to skip
*    STDIN - standard input unit number
*    STDOUT - standard output unit number
*    TINIT, TFIN - time bounds in time section
*    TMIN, TMAX - allowable bounds on times
*    TNMIN, TNMAX - allowable bounds on trace numbers
*    TPTS - number of points in time section
*    TRACES - time section trace file name
*    TRMAX - maximum length for table record, minus one
*    TSMAX - maximum allowable number of time samples
*    TSKIP - skip time at beginning of trace
*    VMIN, VMAX - allowable bounds on velocities
*    VSMAX - maximum allowable number of velocity samples
*
*******************************************************************

      integer bnds   , cz1inf , stdin  , stdout
      parameter (bnds = 8,
     :           cz1inf = 9,
     :           stdin = 5,
     :           stdout = 6)


      integer ntrace , tnmin  , tnmax  , tsmax  , dsmax  , vsmax  ,
     :        dpsmax , ipwdth , opwdth , trmax  , tpts   , spts

      real    tmin   , tmax   , dtmin  , dtmax  , fmin   , fmax   ,
     :        dxmin  , dxmax  , vmin   , vmax   ,
     :        tinit  , tfin   , dt     , fll    , fhl    , flr    ,
     :        fhr    , depmax , tskip

      logical die

      character traces*15

**
* Revision control system identifier string.
**
*
*      character cwpid*28, progid*80, rcsid*80
*      parameter (
*     :    cwpid = ' Center for Wave Phenomena: ',
*     :    progid =' ',
*     :    rcsid = ' $Revision: 1.2 $ ; $Date: 1997/01/28 20:50:28 $')
*
***
** Code begins. Open I/O devices.
***

**     open(unit = stdout)
**     open(unit = stdin)
**      write(stdout, '(//a/a/a/a//)') '----', cwpid, progid, rcsid


**
* Open bounds file.
**

      open(unit=bnds, file='BOUNDS', access='sequential', status='old',
     :	   form = 'formatted', err=2)
	   go to 4
   2       write(stdout, '(a)') ' ***Missing BOUNDS file.'
	   stop
   4  continue
      rewind bnds

**
* Read default parameters.
**

      read(bnds, *) tnmin, tnmax
      read(bnds, *) tmin, tmax
      read(bnds, *) dtmin, dtmax
      read(bnds, *) fmin, fmax
      read(bnds, *) dxmin, dxmax
      read(bnds, *) tsmax
      read(bnds, *) dsmax
      read(bnds, *) vsmax
      read(bnds, *) vmin, vmax
      read(bnds, *) depmax
      read(bnds, *) dpsmax
      read(bnds, *) ipwdth
      read(bnds, *) opwdth
      read(bnds, *) trmax

      close(unit=bnds, status='KEEP')


**
* Input and check run parameters - try to check as many as possible.
**

      die = .false.

**
* Get input trace file name.
**

      read(stdin, '(a15)') traces
      write(stdout, '(a,a15)') ' Trace file name: ', traces

**
* Get number of traces.
**

      read (stdin, *) ntrace
      write(stdout, '(a,i5)') ' Number of traces: ' , ntrace
      if (ntrace .lt. tnmin .or. ntrace .gt. tnmax) then
	 write (stdout, '(a)') ' ***Illegal number of traces'
	 die = .true.
      endif

**
* Get trace time bounds.
**

      read (stdin, *) tinit , tfin
      write(stdout, '(a,f5.3,a,f5.3)') ' Min. time: ' , tinit ,
     :                                 ' Max. time: ' , tfin
      if (tinit .lt. tmin .or. tinit .ge. tfin .or. tfin .gt. tmax) then
	 write(stdout, '(a)') ' ***Illegal time bounds'
	 die = .true.
      endif

**
* Get skip time - how much time at beginning of trace to ignore.
**

      read (stdin, *) tskip
      write(stdout, '(a,f6.4)') ' Skip time: ', tskip
      if (tskip .lt. 0.0 .or. tskip .gt. tfin-tinit) then
	 write(stdout, '(a)') ' ***Illegal skip time'
	 die = .true.
      endif

**
* Get trace time spacing.
**

      read (stdin, *) dt
      write(stdout, '(a,f6.4)') ' Time spacing: ' , dt
      if (dt .lt. dtmin .or. dt .gt. dtmax) then
	 write(stdout, '(a)') ' ***Illegal time spacing'
	 die = .true.
      endif

**
* Make sure these times make sense, i.e. dt divides them.
* Alter if necessary. check number of points per trace.
**

      tpts = nint((tfin - tinit)/dt) + 1
      write(stdout, '(a,i4)') ' Points on trace: ',tpts
      tfin = tinit + dt*real(tpts - 1)
      spts = nint(tskip/dt)
      tskip = dt*real(spts)


      if (tpts .gt. tsmax) then
	 write(stdout, '(a)') ' ***Too many points per trace.'
	 die = .true.
      endif
      
**
* Get filter parameters.
**

      read(stdin, *) fll, fhl, flr, fhr
      write(stdout, '(a,4f7.1)') ' Filter parameters: ' , fll , fhl ,
     :                                                    flr , fhr
      if (fll .lt. fmin .or. fll .gt. fhl .or. fhl .ge. flr .or.
     :    flr .gt. fhr .or. fhr .gt. fmax) then
	 write(stdout, '(a)') ' ***Illegal filter parameters'
	 die = .true.
      endif

**
* Check filter size. It cant overlap half nyquist frequency.
**

      if (fhr*dt .ge. 0.25) then
	 write(stdout, '(a)') ' ***FHR * DT > 0.25'
	 die = .true.
      endif
      
**
* If there was an error, stop now.
**

      if (die) then
	 write(stdout, '(a)') ' ***Data errors. Correct and try again.'
	 stop
      endif

**
* Write information for cz1.
**

      open(unit=cz1inf, file='DATA1', status='NEW', form='unformatted',
     :     access = 'sequential', err=6)
      go to 8
   6     write(stdout, '(a)') ' ***Can not create DATA1.'
         stop
   8  continue

      write(cz1inf) ntrace,tinit,tfin,tskip,dt,fll,fhl,flr,fhr,traces

      close(unit=cz1inf, status='KEEP', err=10)

      write(stdout, '(a)') ' CZ1IN done.'
      stop

  10  write(stdout, '(a)') ' ***Error saving DATA1. Stop.'
      stop

      end
