*************************************************************************
*                                                                        *
*      Program CXZCS:  2.5D Common Shot Inversion in CXZ Medium          *
*                                                                        *
*      Author:   Wenjie Dong                                             *
*      Date:     March, 1991                                             *
*                                                                        *
*      Right Reserved:   Center for Wave Phenomena,                      *
*                        Colorado School of Mines                        *
*                                                                        *
*      References:    1.  CWP-084:  MS thesis by Wenjie Dong             *
*                     2.  CWP-U13:  Documetation of this program         *
*                     3.  CWP-051:  Ph.D thesis by Paul Docherty         *
*                     4.  GEOPHYSICS, VOL. 52, NO.7; P. 931-942,         *
*                               paper by Norman Bleistein                *
*                     5.  CWP: Project Review 1991                       *
*                                                                        *
*************************************************************************


c    Global variables

c    ALAMBD  Continuation parameter used to deform the interfaces
c            in subroutine rayone.
c    AMP(,) or AMPS(,)  Weighting factors calculated for rays between
c             output points and receivers or source.
c    ANGLES   Angle from vertical of the ray from output point to
c             source.
c    ANGLER   Angle from vertical of the ray from output point to
c             receiver.
c    AMPCON  A constant in the inversion algorithm.
c    APREV   Value of ATMP to previous receiver in receiver
c            interpolation.
c    ATMP(), ATMPS  Weighting factors for rays from current output point
c             to receivers and source.
c    A0(,),A1(,)  Cubic spline coefficients. First index identifies
c    A2(,),A3(,)  the interface, second index the portion of the
c                 interface
c    C()     Depth of the interfaces at the source location
c            in the stratified medium. C(1) is the depth of the
c            first interface met by the ray.
c    D()     Length of ray segment within a layer.
c    DASH()  The character "-", used in printout.
c    DATIN   Unit number from which input time data is read.
c    DATOUT  Unit number to which depth section is written.
c    DDF()   Second derivative of F w.r.t. X.
c    DELHA(), DELHAS:  Difference in weighting factors between output
c                points at same depth but NSKPH traces apart
c                (horizontal interpolation parameter).
c    DELHG(), DELHGS:  Difference in ray parameters in horizontal
c                      interpolation.
c    DELHT(), DELHTS:  Difference in traveltimes to receivers between
c         output points at same depth but NSKPH output traces apart
c         (horizontal interpolation).
c    DELRA   Difference in weighting factors between adjacent raypaths.
c    DELRG   Difference in raparameter between adjacent rays.
c    DELRGT  Difference in gradient of traveltimes between adjacent
c            rays.
c    DELRT   Difference in traveltimes between adjacent raypaths
c    DELTAC()  Change in C across a layer.
c    DELTAF()  Change in F across a layer.
c    DELTAX()  x distance travelled within a layer.
c    DELTAZ()  z distance travelled within a layer.
c    DELVA(), DELVAS:  Difference in weighting factors between output
c               points in vertical direction (NSKPV points apart).
c    DELVG(), DELVGS:  Difference in ray parameters in vertical
c               interpolation.
c    DELVGT()  Difference in gradient of traveltimes in vertical
c          interpolation.
c    DELVT(), DELVTS:  Difference in traveltimes to receivers and
c              source betweenoutput points on same trace, NSKPV points
c              apart(vertical interpolation).
c    DF()    First derivative of F w.r.t. X.
c    DXDB    Geometry spreading factor dx/dbetas
c    EXIT    True if output point exits range of definition of input
c            model.
c    F()     Defining function for interfaces.
c    FMAX    Maximum frequency present in time data.
c    GEOINC  Spacing between receivers to which rays are traced.
c    GRADT(,)   Array used to store the previous value of gradient of
c            traveltimes when calculating the difference between rays.
c    GRADTAU2(,)   Array used to store the values of gradient of
c             traveltimes.
c    HSINC   Output trace spacing.
c    I       Loop variable.
c    IDIFF   Element number in TTMP and ATMP of first interpolated
c            value to use.
c    IDIFF1,2  Used in horizontal interpolation to make sure we 
c              interpolate over rays that have the same offset
c              between from source point and receiver.
c    IH      Horizontal interpolation loop variable.
c    IHOR    Loop variable over output traces in pattern.
c    INTERF  Unit number corresponding to model file.
c    IPATT   Current pattern number.
c    ITRACE  Sets the trace number from which to take time data
c            at interpolated traveltime.
c    J       Loop variable.
c    JMJP1   JVER minus JV plus 1.
c    JV      Vertical interpolation loop variable.
c    JVER    Loop variable over output points on trace.
c    JVERL   Sample number of previous point on output trace from
c            which rays were traced.
c    K       Loop variable.
c    KPREV   Number of previous receiver traced to.
c    KREC    Loop variable over receivers.
c    LOST    Total number of rays that could not be found
c            in the inversion.
c    MAXN    Maximum allowed value of N (or change dimensions).
c    MAXNP1  MAXN plus 1.
c    MAXNP3  MAXN plus 3.
c    MAXSPL  Maximum allowed number of points defining an interface.
c    MIGDAT  Name of file containing output depth section.
c    MODEL   Name of file describing the input model.
c    MXSPM1  MAXSPL minus 1.
c    N       Number of interfaces between source and receiver.
c    NDIFFI  The difference between the output trace moveup and the
c            moveup of the first receiver.
c    NHSORC  Number of output traces in current pattern.
c    NINT    Total number of interfaces in the model.
c    NOCONV  True if can't find or converge on a ray solution.
c    NOUT    Number of traces output in patterns completed so far.
c    NOUT1   Number of first output trace in current pattern
c    NOUT2   Number of last output trace in current pattern.
c    NPATTS  Number of patterns in job.
c    NPTS()  Number of points defining each interface.
c    NPT     Number of points/trace in input time data.
c    NPANMX  Maximum number of output traces in a panel. This is the
c            maximum value of NSKPH.
c    NPTMAX  Maximum number of points/trace in input time data.
c    NRANGE  Number of receivers over which horizontal interpolation
c            takes place.
c    NRCMAX  Maximum number of receivers traced to from any output
c            point in the depth section.
c    NREC    Number of receivers to which traveltimes and
c            weighting factors are calculated.
c    NRECI1  First receiver to trace to from output trace NOUT1.
c    NRECI2  First receiver to trace to from output trace NOUT2.
c    NRECF1  Last receiver to trace to from output trace NOUT1.
c    NRECF2  Last receiver to trace to from output trace NOUT2.
c    NRI1    First receiver traced to from previous output trace.
c    NRI2    First receiver traced to from current output trace.
c    NRF1    Last receiver traced to from previous output trace.
c    NRF2    Last receiver traced to from current output trace.
c    NSAMP   Integer sample number on time trace of two way traveltime.
c    NSKPH   Number of output traces to skip in interpolation.
c    NSKPH1  Initial value of NSKPH.
c    NSKPV   Number of source points to skip down the output trace.
c    NSKPV1  Initial value of nskpv.
c    NSKPR   Number of receivers to skip.
c    NSKPR1  Initial value of nskpr.
c    NSKREC  Number of records to skip over at start of time data.
c    NTR     Trace number from which to take time data.
c    NTR1    First trace number minus 1 in integration.
c    NTRACE  Number of traces read in from time data file.
c    NTRMAX  Maximum number of traces that can be read in.
c    NVPMAX  Maximum number of points in output trace.
c    NVSORC  Number of points in output trace.
c    OUTPUT(,), OUTPUT1(,):  Inversion outputs. The first is
c             reflectivity, and the second is reflectivity times
c             cosine(theta).
c    PARIN   Unit number corresponding to file PARAM.
c    PI      Pi.
c    SAMP    Sample number corresponding to two way traveltime.
c    SIG(,), SIGS(,):  Ray parameter sigmas calculated along a ray.
c    SIGMAS  Ray parameter of the ray from output point to the source.
c    SIGMAR()  Ray parameter of the rays from output point to receivers.
c    SINMAX  Max value of sine of emergence angle of ray without
c            aliasing.
c    SKIPIT  Dummy variable used to skip input records.
c    SRATE   Sample rate in seconds.
c    STAY1   Logical variable indicating a point at which to exit
c            from vertical interpolation.
c    STAY2   Logical variable indicating a point at which to exit
c            from horizontal interpolation.
c    STDERR  Unit number for run information and diagnostics.
c    SUMMAND Temporary for computing the reflectivity function.
c    T(,)    Traveltime to receivers from each point on output trace.
c    TS(,1)  Traveltime to source from each output point.
c    TPREV   Value of traveltime to previous receiver.
c    TRACE(,)   Time data to be migrated.
c    TRACES  Name of file containing time section.
c    TRUGEO  Receiver spacing.
c    TTMP()  Traveltimes from current output point to receivers.
c            These times are written into T(,) before moving to next
c            output point.
c    V()     Interval velocities. V(1) is the velocity
c            on the first ray segment.
c    VSINC   Vertical distance between points down each output trace.
c    X()     x coordinates of points of intersection
c            of ray with interfaces.
c    XR      x coordinate of receiver.
c    XR1     Same as XR.
c    XS      x coordinate of output point.
c    XS1     Shot position in experiment.
c    XSOURC  x coordinate of first output trace (or source position)
c            in a pattern.
c    XINT(,) x coordinates of points defining interfaces.
c    XSTART  x coordinate of first receiver.
c    ZS      z coordinate of output point.
c    ZSSQD   ZS squared.
c    ZINT(,) z coordinates of points defining interfaces.
c    ZSOURC  z coordinate of first output trace (or source point).


      integer   datin,  datout,  datout1,  interf,  parin,  stderr 
      parameter ( datin = 10, datout = 11, datout1= 16, interf = 12,
     :            parin = 13, stderr = 0 )
      character   traces*15,   model*15,  migdat*15,   migdat1*15  
      integer     maxn,   maxnp1,   maxnp3,   maxspl,    mxspm1
      parameter ( maxn = 50,    maxspl = 151)
      parameter ( maxnp1 = maxn + 1,
     :            maxnp3 = maxn + 3,
     :            mxspm1 = maxspl - 1)
      real        xint(maxn,maxspl),        zint(maxn,maxspl),
     :            a0(maxn,mxspm1),          a1(maxn,mxspm1),
     :            a2(maxn,mxspm1),          a3(maxn,mxspm1)
      integer     npts(maxn),    nintf

      common /a/  xint,        zint,      npts,       nintf,
     :            a0,          a1,        a2,         a3

      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1)

      integer     n


      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n

      integer     nptmax,  ntrmax,  npanmx,  nrcmax,  nvpmax
      parameter ( nptmax = 1024,   npanmx = 10,   ntrmax = 256,
     :            nrcmax = 256,    nvpmax = 1024)
      real        trace(nptmax,ntrmax),    output(nvpmax,npanmx),
     :            amp(nvpmax,nrcmax),      t(nvpmax,nrcmax),
     :            sig(nvpmax,nrcmax),      gradt(nvpmax,nrcmax),
     :            amps(nvpmax,nrcmax),     ts(nvpmax,1),
     :            sigs(nvpmax,nrcmax),   
     :            delht(nrcmax),           delha(nrcmax),
     :            delhts,                  delhas,
     :            delvt(nrcmax),           delva(nrcmax),
     :            delvts,                  delvas

      real        atmp(nrcmax),            ttmp(nrcmax),
     :            atmps,                   ttmps,
     :            delhg(nrcmax),           delvg(nrcmax),
     :            delvgt(nrcmax),          delhgt(nrcmax),
     :            delhgs,                  delvgs,
     :            output1(nvpmax,npanmx),  delrgt,
     :            sigmas,                  sigmar(nrcmax),
     :            cosbf,                   gradtau2(nrcmax) 
      real        x(0:maxnp3)
      real        ampcon,     delra,     delrt,     delrg,
     :            fmax,       geoinc,    hsinc,     
     :            rinci,     rincf,     samp,       z,
     :            sinmax,     skipit,    srate,     trugeo,
     :            vsinc,      xr,        xr1,
     :            xs,         xsourc,    xstart, 
     :            zs,         zsourc,    zssqd,     gtprev,
     :            aprev,      tprev,     gprev,     xs1,
     :            angler,     angles,    summand,   dxdb
     
      integer     i,         idiff1,     idiff2,    idiff,
     :            ih,        ihor,       ipatt,     itrace,
     :            j,         jv,         jver,      jverl,
     :            jmjp1,     k,          krec,      lost,
     :            nhsorc,    nout,       nout1,     nout2,
     :            npatts,    npt,        nrange,    nrec,
     :            nreci1,    nreci2,     nrecf1,    nrecf2,
     :            nri1,      nri2,       nrf1,      nrf2,
     :            nsamp,     nskph,      nskpr,     nskpr1,
     :            nskpv,     nskpv1,     nskrec,    ntr,
     :            ntrace,    nvsorc,     ndiffi,    kprev,
     :            ntr1

      logical     noconv,    exit,       stay1,     stay2
      character   mkplot*1,   dash*50

      real       pi
      parameter( pi = 3.141592653589)
      

      open(unit=parin,file='param',err=100)
      go to 105
100   write(stderr,'(a)') 'Can''t open file : param'
      stop
105   rewind parin

c     Reading from the data file param.
c     Read name of depth model file.
      read(parin,'(a)') model 

c     Read number of interfaces in model file.
      read(parin,*) nintf

      if(nintf.gt.0) then
         open(unit=interf,file=model,err=120)
         go to 125
120      write(stderr,'(a)') 'Can''t open file : ',model
         stop
125      rewind interf

c        Reading the points defining each interface.
c        A negative z value defines the end of the interface.
         do 155 i = 1,  nintf
            j = 1
150         read(interf,*) xint(i,j),zint(i,j)
            if(zint(i,j).lt.0.) then
               npts(i) = j - 1
            else
               j = j + 1
               go to 150
            end if
155         continue


c        Calculating the cubic spline coefficients of each interface.
         call cuspln(nintf,xint,zint,npts,a0,a1,a2,a3)

         close(unit=interf)

      end if

c     Initialising.
      noconv = .false.
      exit   = .false.

c     Initialising some constants.
      c(0) = 0.
      f(0) = 0.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc                         INVERSION                           ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c     Set dashed characters for printout.
      do 500  i = 1,  50
         dash(i:i) = '-'
500      continue

      write(stderr,'(6(/),6x,a)') 'RAY INVERSION'
      write(stderr,'(//6x,a)') 'Model Information'
      write(stderr,'(6x,a)') dash(1:17)
      write(stderr,'(//6x,a,1x,i2)')
     :     'Number of interfaces =',nintf
c     Read layer velocities (shallowest first).
      read(parin,*) (v(i),i=1,nintf+1)
      write(stderr,'(/6x,a)')
     :     'Velocities :'
     
      do 800 i = 1,  nintf + 1
         write(stderr,'(11x,f16.3)') v(i)
800      continue

      write(stderr,'(6(/),6x,a)') 'Time Section Data'
      write(stderr,'(6x,a)') dash(1:17)
      read(parin,'(a)')  traces
      open(unit=datin,form='UNFORMATTED',file=traces,err=825)
      go to 830
825   write(stderr,'(a)') 'Can''t open file : ',traces
      stop
830   rewind datin
      write(stderr,'(/6x,a,a)') 'Time section is file ',traces
      read(parin,*) nskrec
      write(stderr,'(/6x,a,a,1x,i4)')
     :     'Number of records to skip at the ',
     :     'beginning of the data set =', nskrec
      read(parin,*) ntrace
      write(stderr,'(/6x,a,1x,i4)')
     :     'Number of traces to read =',ntrace
      if(ntrace.gt.ntrmax) then
         write(stderr,'(a)')
     :   ' MAIN: too many time traces, change dimensions.'
         stop
      end if

      read(parin,*) srate
      write(stderr,'(/6x,a,1x,f6.4)')
     :     'Sample rate =',srate

      read(parin,*) npt
      write(stderr,'(/6x,a,1x,i4)')
     :     'Number of samples/trace =',npt
      if(npt.gt.nptmax) then
         write(stderr,'(a)')
     :   ' MAIN: too many time points/trace, change dimensions.'
         stop
      end if
      read(parin,*) fmax
      write(stderr,'(/6x,a,1x,f6.2)')
     :     'Maximum frequency = ',fmax
      write(stderr,'(6(/),6x,a)') 'Source Geometry'
      write(stderr,'(6x,a)') dash(1:17)
      read(parin,*) xs1
      write(stderr,'(/6x,a,f10.2)')
     :     'X-coordinate of shot position= ',xs1
      write(stderr,'(6(/),6x,a)') 'Receiver Geometry'
      write(stderr,'(6x,a)') dash(1:17)
      read(parin,*) xstart
      write(stderr,'(/6x,a,f10.2)')
     :     'X-coordinate of first receiver = ',xstart
      read(parin,*) trugeo
      write(stderr,'(/6x,a,1x,f8.2)')
     :     'Receiver spacing =',trugeo
      write(stderr,'(6(/),6x,a)') 'Depth Section Data'
      write(stderr,'(6x,a)') dash(1:18)
      read(parin,'(a)')  migdat
      open(unit=datout,form='UNFORMATTED',file=migdat,err=850)
      go to 855
850   write(stderr,'(a)') 'Can''t open file ',migdat
      stop
855   rewind datout
      write(stderr,'(/6x,a,a)') 'Reflectivity will be file ',migdat
      read(parin,'(a)')  migdat1
      open(unit=datout1,form='UNFORMATTED',file=migdat1,err=851)
      go to 856
851   write(stderr,'(a)') 'Can''t open file ',migdat1
      stop
856   rewind datout1
      write(stderr,'(/6x,a,a)') 'Reflectivity * cos will be file ',
     :      migdat1
      read(parin,*) xsourc,zsourc
      write(stderr,'(/6x,a,1x,f10.2)')
     :     'X coordinate of first output trace =',
     :      xsourc
      write(stderr,'(/6x,a,1x,f10.2)')
     :     'Depth of first output point =', zsourc

      read(parin,*) hsinc
      write(stderr,'(/6x,a,1x,f8.2)')
     :     'Output trace spacing =', hsinc
      read(parin,*) vsinc
      write(stderr,'(/6x,a,1x,f8.2)')
     :     'Vertical spacing of output points =',
     :      vsinc
      read(parin,*) nvsorc
      write(stderr,'(/6x,a,1x,i5)')
     :     'Number of points in output trace =',
     :      nvsorc
      if(nvsorc.gt.nvpmax) then
         write(stderr,'(a)')
     :   ' MAIN: too many points/output trace, change dimensions.'
         stop
      end if
      write(stderr,'(4(/),6x,a)') dash(1:50)

c     Reading in the common-shot time data.
      write(stderr,'(4(/),6x,a,1x,a)')
     :     'Reading from the data file',traces

      do 900 i = 1,  nskrec
         read(datin) skipit
900      continue

      do 910 k = 1,  ntrace
         read(datin)(trace(j,k),j=1,npt)
910      continue

      close(unit=datin)

      write(stderr,'(/6x,a,1x,i4,1x,a,1x,i4)')
     :     'Read traces',1,'-',ntrace

c     Initializing the number of rays that could not be found.
      lost = 0

c     Setting a constant used in the amplitude calculation.
      ampcon = trugeo * 4.0 * pi * sqrt(2.0)

c     Setting the aliasing criterion.
      sinmax = v(1) / ( 2. * trugeo * fmax )

c     Initializing the number of traces output.
      nout = 0

c     Read the number of patterns for this run.
      read(parin,*) npatts

      write(stderr,'(//6x,a,1x,i4)')
     :     'Begin Ray Tracing.      Number of Patterns =',
     :      npatts

c     Read the number of the first output trace and the first
c     and last receiver in the integration for the first output trace.
      read(parin,*) nout1, nreci1, nrecf1

c     Initialize the current pattern number.
      ipatt = 1

1000  if(ipatt.le.npatts) then
c        while more patterns left to do...

c        Read the skip parameters for this pattern.
         read(parin,*) nskpr, nskpv, nskph

         if(nskph.gt.npanmx) then
c           Need to increase npanmx in parameter statement.
            write(stderr,'(a)')
     :      ' MAIN: skipping too many traces, change dimensions.'
c            stopping
             go to 10000
         end if

c        Read the number of the last output trace in the pattern
c        and the receivers over which to integrate for that trace.
         read(parin,*) nout2, nreci2, nrecf2
         write(stderr,'(4(/),6x,a,i4)') 'Pattern #',ipatt
         write(stderr,'(2(/),6x,a,4x,a,4x,a)') 'output trace',
     :   'first input trace','last input trace'
         write(stderr,'(10x,i4,13x,i4,16x,i4)') nout1,nreci1,nrecf1
         write(stderr,'(10x,i4,13x,i4,16x,i4)') nout2,nreci2,nrecf2
         write(stderr,'(2(/),6x,a)') 'Skip Parameters:'
         write(stderr,'(/6x,a,1x,i4)')
     :     'Number of receivers to skip =', nskpr
         write(stderr,'(6x,a,1x,i4)')
     :     'Number of vertical output points to skip =',
     :      nskpv
         write(stderr,'(6x,a,1x,i4)')
     :     'Number of output traces to skip =',
     :      nskph

c        Find the number of output traces for this pattern 
         nhsorc  = nout2 - nout1 + 1
         write(stderr,'(//6x,a,a,1x,i4,1x,a,i4/)') 'Output traces for ',
     :   'this pattern =', nout1,'-',nout2

c        Calculate the first and last trace increments.  This 
c        is how the range of integration changes as the output
c        trace number increases.
         if(nhsorc.le.1) then
            rinci = 0.
            rincf = 0.
         else
            rinci = float( nreci2 - nreci1 ) / ( nhsorc - 1 )
            rincf = float( nrecf2 - nrecf1 ) / ( nhsorc - 1 )
         end if

c        Initialize output trace number for this pattern.
         ihor = 1
c        Set first and last receiver numbers for first output 
c        trace of first panel in current pattern.
         nri1 = nreci1
         nrf1 = nrecf1

1500     if(ihor.le.nhsorc) then
c           while within this pattern...

c           Calculate the first and last receiver numbers for the
c           last output trace in this panel.  We will ray trace
c           to this range of receivers.
            nri2 = nreci1 + ( ihor - 1 ) * rinci
            nrf2 = nrecf1 + ( ihor - 1 ) * rincf

c           Calculate the number of receivers for the last
c           output trace in this panel.
            nrec = nrf2 - nri2 + 1

c           Doing some preliminary calculations for the 
c           interpolation.  
            ndiffi = nri2 - nskph - nri1
            if(ndiffi.ge.0) then
c              The first receiver is moving up faster than
c              the output traces.
               idiff1 = ndiffi
               idiff2 = 0
               itrace = ndiffi + nri1 + nskph
            else
c              The first receiver is moving up slower than
c              the output traces.
               idiff1 =  0
               idiff2 =  - ndiffi
               itrace =  nri1 + nskph
            end if

c           Calculating the number of receivers over which
c           to interpolate.
            nrange = min(nrf2-nskph,nrf1) -
     :               max(nri2-nskph,nri1) + 1

            write(*,'(a)')'nrange='
            write(*,*)nrange
c           Updating the source (or output trace) location.
            xs = xsourc + ( ihor - 1 ) * hsinc

c           Calculating the depths of the interfaces in the
c           stratified medium at this source location.
            call strat(xs,c,exit)
            if(exit) then
               write(stderr,'(a)') ' MAIN: Source outside model.'
               stop
            end if

c           Temporarily saving the vertical skip parameter.
            nskpv1 = nskpv

c           Initializing the output depth point number.
            jver = 1

2000        if(jver.le.nvsorc) then
c              while more output depth points to calculate...


c              Finding the depth of the source (output) point.
               zs = zsourc + (jver-1) * vsinc

c              Whenever the source is moved it is possible
c              for the number of interfaces between source
c              and receivers to change. Calculating n next.
               znear = v(1)/(4*fmax) * sqrt(zs**2+(xs-xs1)**2)/zs
               call setn(zs,c,nintf,n,znear,deltac,f)

c              Setting the first receiver location for this
c              output point.
               xr = xstart + ( nri2 - 1 ) * trugeo

ccc            Begin ray tracing and interpolation for this
ccc            output point.

               if(n.eq.0) then

c                 No interfaces between source and receiver.
c                 Ray is a straight line.
c                 Do not interpolate over receivers for this case.
                  zssqd = zs**2
                  xr1 = xr

                  if(zs.lt.vsinc) then

c                    First output point on trace. Get the
c                    traveltimes for vertical and horizontal
c                    interpolation but set output at this
c                    point to zero. 

c                  ray to the source
                     d(1)=sqrt((xs1-xs)**2+zssqd)
                     call ttime(d,v,n,ttmps)
                     atmps=0.

                     do 2500 i = 1,  nrec
                        d(1) = sqrt((xr1-xs)**2 + zssqd)
                        call ttime(d,v,n,ttmp(i))
                        atmp(i) = 0.
                        xr1 = xr1 + trugeo
c                 set gradtau2=1. to avoid division by zero later
                        gradtau2(i) = 1.
2500                    continue

                  else
                     deltax(1)=xs-xs1
                     d(1)=sqrt(deltax(1)**2+zssqd)
                     call ttime(d,v,n,ttmps)
                     deltaz(1)=zs
                     call amplit(sinmax,atmps,sigmas,dxdb,0)
                     atmps = atmps * sqrt(abs(dxdb))

                     call anglesv(deltax(1),zs,angles)

                     do 2550 i = 1,  nrec
                        deltax(1) = xs - xr1
                        d(1) = sqrt(deltax(1)**2 + zssqd)

                        call ttime(d,v,n,ttmp(i))
                        deltaz(1) = zs
                  call amplit(sinmax,atmp(i),sigmar(i),dxdb,1)

                        if(dxdb.eq.0) dxdb=1
                        atmp(i)=atmp(i)/sqrt(abs(dxdb))
                        call anglesv(deltax(1),zs,angler)
                        call magnitude(angles,angler,gradtau2(i))

                        xr1 = xr1 + trugeo
2550                 continue

                  end if

               else

ccc               Trace rays and interpolate
c 
c        trace ray to shot
                  call rayone(xs1,xs,zs,n,v,c,x,noconv)
		  
		  if(noconv) then
c           Can't find ray from the shot, stop ray tracing in receivers
                      timps = 99999.
		      atmps = 0.
		      go to 4000
		  end if
		  
                  call ttime(d,v,n,ttmps)

                  call amplit(sinmax,atmps,sigmas,dxdb,0)
c           the spreading factor dbeta/dx
                  atmps = atmps * sqrt(abs(dxdb))
                  call anglesv(deltax(n+1),deltaz(n+1),angles)

c                 Temporarily saving the receiver skip parameter.
                  nskpr1 = nskpr

c                 Setting the receiver spacing for receiver 
c                 continuation.
                  geoinc = trugeo * nskpr

c                 Initializing the receiver number for this
c                 set of rays.
                  krec = 1

3000              if(krec.le.nrec) then
c                    while more receivers to trace to...

                     if(krec.eq.1.or.noconv) then

c                       No starting ray to use in receiver
c                       continuation.
                        call rayone(xr,xs,zs,n,v,c,x,noconv)

                     else

c                       Continue last ray to next location.
                        call reccon(x,n,xr,geoinc,v(1),noconv)

                        if(noconv) then
c                          Couldn't find it with receiver
c                          continuation, try to get it by
c                          continuation in interfaces.
                           xr = xr + geoinc
                           call rayone(xr,xs,zs,n,v,c,x,noconv)
                        end if

                     end if

                     if(noconv) then

c                       Can't find the ray. Set the receiver
c                       location for the next ray.
                        xr = xr + geoinc

c                       Set traveltime to large number so that
c                       it is greater than length of time data
c                       even after interpolation.
                        ttmp(krec) = 99999.

c                       Set amplitude factor to zero.
                        atmp(krec) = 0.

c                       Increment the rays lost counter.
                        lost = lost + 1

                     else

c                       Get the traveltime and amplitude info.
                        call ttime(d,v,n,ttmp(krec))
c 
             call amplit(sinmax,atmp(krec),sigmar(krec),dxdb,1)
                        if(dxdb.eq.0) dxdb=1.
                        atmp(krec)=atmp(krec)/sqrt(abs(dxdb))

                        call anglesv(deltax(n+1),deltaz(n+1),angler)
                        call magnitude(angles,angler,gradtau2(krec))

                     end if

ccc                  Interpolate to receivers between rays

                     if(krec.eq.1.or.nskpr.eq.1) then

c                       Nothing to interpolate.

                     else

c                       Set number of previous receiver traced to.
                        kprev = krec - nskpr

c                       Calculate difference in traveltime/receiver.
                        delrt = (ttmp(krec) - ttmp(kprev)) / nskpr

c                       Calculate difference in amplitude/receiver.
                        delra = (atmp(krec) - atmp(kprev)) / nskpr

c                       Calculate difference in sigmar/receiver.
                        delrg = (sigmar(krec) - sigmar(kprev)) / nskpr

c                          Calculate difference in gradtau2/receiver.
                        delrgt= (gradtau2(krec)-gradtau2(kprev))/nskpr

c                        Calculate difference in gradtau2/receiver
                        tprev = ttmp(kprev)
                        aprev = atmp(kprev)
                        gprev = sigmar(kprev)
                        gtprev = gradtau2(kprev)

                        do 3025 i = 1,  nskpr - 1

c                          Interpolating traveltimes.
                           tprev = tprev + delrt
                           ttmp(kprev+i) = tprev

c                          Interpolating amplitudes.
                           aprev = aprev + delra
                           atmp(kprev+i) = aprev

c                           Interpolating sigmar.
                           gprev = gprev + delrg
                           sigmar(kprev+i) = gprev

c                           Interpolating magnitude of gradtau
                           gtprev=gtprev+delrgt
                           gradtau2(kprev+i)=gtprev

3025                       continue

                     end if

c                    Set number of next receiver to trace to.
                     krec = krec + nskpr

                     go to 3000

                  end if

c                 Make sure we found the ray to the last receiver
c                 in this set (incrementing receiver number by 
c                 nskpr each time might not coincide with last
c                 receiver in the range).

                  krec = krec - nskpr

                  if(krec.lt.nrec) then
c                    Get ready to trace to last receiver.
                     nskpr = nrec - krec
                     krec = nrec
                     geoinc = trugeo * nskpr
                     go to 3000
                  end if

c                 Go back to original receiver skip parameter.
                  nskpr = nskpr1

               end if

ccc            End of ray tracing and receiver interpolation
ccc            for this output point.

ccc            Start the vertical and horizontal interpolation.

c              Vertical interpolation.
4000           if(jver.eq.1.or.nskpv.eq.1) then

c                 Nothing to interpolate vertically.

               else

c                 Set number of last depth point from which
c                 rays were traced.
                  jverl = jver - nskpv

c                 Calculate change in traveltime, amplitude 
c                 and ray parameter (from output point to shot point)
c                 between depth points from which rays were traced.

                  delvts=(ttmps-ts(jverl,1))/nskpv
                  delvas=(atmps-amps(jverl,1))/nskpv
                  delvgs=(sigmas-sigs(jverl,1))/nskpv
c

                  do 3075 i = 1,  nrec

c                    Calculate change in traveltime between
c                    depth points from which rays were traced.
                     delvt(i) = (ttmp(i) - t(jverl,i)) / nskpv

c                    Calculate change in amplitude between
c                    depth points from which rays were traced.
                     delva(i) = (atmp(i) - amp(jverl,i)) / nskpv

c                    Calculate change in sigmar between
c                    depth points from which rays were traced.
                     delvg(i) = (sigmar(i) - sig(jverl,i)) / nskpv

c                    Calculate change in gradtau2 between
c                    depth points from which rays were traced.
                     delvgt(i) = (gradtau2(i) - gradt(jverl,i)) / nskpv

3075                 continue

               end if

c              Prepare to enter vertical interpolation loop.
               stay1 = .true.
               jv = 1

3100           if(jv.le.nskpv.and.stay1) then
c                 while more vertical points to interpolate to...

c                 set number of current depth point
                  jmjp1 = jver - jv + 1

                  if(jv.eq.1) then
c                    we know the traveltimes at this point from 
c                    ray tracing
                  else

c                    interpolate vertically the traveltimes, amplitudes
c                     and ray parameter-- sigmas

                     ttmps=ttmps-delvts
                     atmps=atmps-delvas
                     sigmas=sigmas-delvgs
c 
                     do 3200 i = 1,  nrec
                        ttmp(i) = ttmp(i) - delvt(i)
                        atmp(i) = atmp(i) - delva(i)
                        sigmar(i) = sigmar(i) - delvg(i)
                        gradtau2(i) = gradtau2(i)-delvgt(i)
3200                    continue
                  end if

c                 Horizontal interpolation.

                  if(ihor.eq.1.or.nskph.eq.1) then

c                    No horizontal interpolation necessary.

                  else

c                    Calculate change in traveltimes and amplitudes
c                    between traces at this depth.

                     delhts=( ttmps-ts(jmjp1,1) )/nskph
                     delhas=( atmps-amps(jmjp1,1) )/nskph
                     delhgs=( sigmas-sigs(jmjp1,1) )/nskph

                     do 3300 i = 1,  nrange

                        delht(i) = ( ttmp(i+idiff2) -
     :                               t(jmjp1,i+idiff1) )/nskph

                        delha(i) = ( atmp(i+idiff2) -
     :                               amp(jmjp1,i+idiff1) )/nskph

                        delhg(i) = ( sigmar(i+idiff2) -
     :                               sig(jmjp1,i+idiff1) )/nskph

                        delhgt(i) = ( gradtau2(i+idiff2) -
     :                               gradt(jmjp1,i+idiff1) )/nskph
3300                    continue

                  end if

c                 Previous values of t and amp are no longer
c                 required.  Set them to new values found above
c                 from either ray tracing or vertical interpolation.

                  ts(jmjp1,1)=ttmps
                  amps(jmjp1,1)=atmps
                  sigs(jmjp1,1)=sigmas

                  do 3350 i = 1,  nrec

                     t(jmjp1,i)   = ttmp(i)
                     amp(jmjp1,i) = atmp(i)
                     sig(jmjp1,i) = sigmar(i)
                     gradt(jmjp1,i)=gradtau2(i)

3350                 continue

c                 Prepare to enter horizontal interpolation loop.
                  ih = 1
                  stay2 = .true.

3400              if(ih.le.nskph.and.stay2) then
c                    while more horizontal locations to interpolate to..

                     if(ih.eq.1) then
c                       we already know the traveltimes at this point
                        ntr1 = nri2 - 1
                        idiff = 0
                     else
                        idiff = idiff2
                        ntr1 = itrace - ih

c                       interpolate horizontally

                        ttmps=ttmps-delhts
                        atmps=atmps-delhas
                        sigmas=sigmas-delhgs

                        do 3500 i = 1,  nrange
                           ttmp(i+idiff2) = ttmp(i+idiff2) - delht(i)
                           atmp(i+idiff2) = atmp(i+idiff2) - delha(i)
                           sigmar(i+idiff2)=sigmar(i+idiff2)-delhg(i)
                           gradtau2(i+idiff2)=gradtau2(i+idiff2) -
     :                                        delhgt(i)
3500                       continue

                     end if

c                    Initialize output at this point.
                     output(jmjp1,ih) = 0.0
                     output1(jmjp1,ih) = 0.0

c                    Sum over receivers.
                     do 4325 i = 1,  nrange

c                       Calculate number of sample to take
c                       from time trace.
c 
                        samp  = (ttmps + ttmp(i+idiff)) / srate + 1.

                        nsamp = samp 

                        if(nsamp.gt.npt-1) then
c                          Traveltime too great.
                        else

c                          Set trace number corresponding to 
c                          this traveltime.
                           ntr = ntr1 + i

c                          Sum in this sample, weighted by amplitude
c                          factor. Since traveltime is likely to
c                          fall between two samples on time trace,
c                          linearly interpolate for the amplitude
c                          of time data.

                           summand = ampcon * atmps *
     :                              sqrt(sigmas+sigmar(i+idiff)) *
     :                              atmp(i+idiff) *
     :                      (trace(nsamp,ntr) +
     :                      (samp - nsamp) * (trace(nsamp+1,ntr) -
     :                                        trace(nsamp,ntr) ) )


c                          This output is the reflectivity function

                           output(jmjp1,ih) = output(jmjp1,ih) + summand


c                          This output is reflectivity times cos(theta)

                           output1(jmjp1,ih) = output1(jmjp1,ih)
     :                                + summand * gradtau2(i+idiff)

                        end if

4325                    continue

c                    Move across to next trace.
                     ih = ih + 1

                     if(ihor.eq.1) then
c                       prepare to exit horizontal interpolation loop
                        stay2 = .false.
                     end if

                     go to 3400

                  end if

c                 Exit horizontal interpolation loop.

c                 Move up to next depth.
                  jv = jv + 1

                  if(jver.eq.1) then
c                    prepare to exit vertical interpolation loop
                     stay1 = .false.
                  else
                     if(nskph.gt.1.and.ihor.gt.1) then
c                       Original values of ttmp and atmp have been
c                       altered by horizontal interpolation.
c                       Return to these values in order to proceed
c                       with vertical interpolation.

                        ttmps=ts(jmjp1,1)
                        atmps=amps(jmjp1,1)
                        sigmas=sigs(jmjp1,1)

                        do 4350 i = 1,  nrec
                           ttmp(i) = t(jmjp1,i)
                           atmp(i) = amp(jmjp1,i)
                           sigmar(i) = sig(jmjp1,i)
                           gradtau2(i)=gradt(jmjp1,i)
4350                       continue
                     end if

                  end if

                  go to 3100

               end if

c              Exit vertical interpolation loop.


c              Set number of next depth point to trace rays from.
               jver = jver + nskpv

               go to 2000

            end if

c           Make sure do all points on trace. Incrementing by
c           nskpv may not end up at last depth point.
            jver = jver - nskpv

            if(jver.lt.nvsorc) then
c              finish off last few depths

               nskpv = nvsorc - jver
               jver = nvsorc

               go to 2000

            end if

c           Return to original vertical skip parameter.
            nskpv = nskpv1



c           Output panel of traces.
c           Traces were calculated from right to left across
c           panel using our interpolation scheme.

            if(ihor.eq.1) then
c              only one trace to output
               itrace = 1
            else
c              nskph traces to output
               itrace = nskph
            end if

            do 4500 i = itrace,  1,  -1
c              output in reverse order

               write(datout)(output(j,i),j=1,nvsorc)
               write(datout1)(output1(j,i),j=1,nvsorc)

4500           continue


            write(stderr,'(6x,a,1x,i4,8x,a,1x,i4,a,i4)')
     :           'Output trace #',ihor+nout,
     :           'Sum over receivers',nri2,'-',nrf2

c           Last receiver range of current panel becomes
c           first receiver range of next panel.
            nri1 = nri2
            nrf1 = nrf2

c           Set number of next output trace to trace rays from.
            ihor = ihor + nskph

            go to 1500

         end if

c        Make sure we've done all traces in this pattern.
         ihor = ihor - nskph

         if(ihor.lt.nhsorc) then
c           finish off the pattern

            nskph = nhsorc - ihor
            ihor = nhsorc

            go to 1500

         end if

c        Pattern is done.

c        First output trace on next pattern is last output
c        trace of current pattern plus one.
         nout1 = nout2 + 1

c        Last range of receivers in current pattern becomes
c        first range of receivers in next pattern.
         nreci1 = nreci2
         nrecf1 = nrecf2

c        Increment total number of traces output.
         nout = nout + ihor

c        Set the first source location for the next pattern.
         xsourc = xsourc + ihor * hsinc

c        Increase the pattern number.
         ipatt = ipatt + 1

         go to 1000

      end if
 
c     Finished all patterns.

      write(stderr,'(/6x,a,1x,i4)') 'Number of rays lost = ',lost

      write(stderr,'(/6x,a,1x,a/)')
     :     'The reflectivity output is the file',
     :     migdat

      write(stderr,'(/6x,a,1x,a/)')
     :     'The reflectivity*cos(theta) is the file',migdat1

10000 close(unit=parin)
      close(unit=datout)

      stop
      end


*--------------------------------------------------------------------
