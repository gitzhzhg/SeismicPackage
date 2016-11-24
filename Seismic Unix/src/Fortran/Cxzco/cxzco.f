*************************************************************************
*      Program CXZCO:  2.5D Common Offset Inversion in CXZ Medium        *
*                      (modified from CXZCS coded by Wenjie Dong)        *
*      Revisor:        Hsu, Ching-Hsiang                                 *
*      Date:           June, 1990                                        *
*      Revisor:        Hsu, Ching-Hsiang and Zhenyue Liu                 *
*      Date:           June, 1991                                        *
*      Right Reserved: Center for Wave Phenomena,                        *
*                      Colorado School of Mines                          *
*      References:     1.  CXZCO:  A 2.5D Common Offset Inversion        *
*                          Program in a c(x,z) Medium                    *
*                      2.  CXZCS:  Program  by Wenjie Dong               *
*                      3.  GEOPHYSICS, VOL. 52, NO.7; P. 931-942,        *
*                               paper by Norman Bleistein                *
*      Note:	       							 *
*		       1.  Receiver space is equal to shot space. 	 *
*		       2.  Input parameters are the same as CXZCS.       *
*                      3.  It is highly recommended to apply smooth      *
*                          function program (The smooth.f program is     *
*                          provided by CWP) to get the second derivative *
*			   smoothed model then we can get the stable ray *
*			   paths.                                        *
*                      4.  It is recommended to use the CSHOT to generate*
*                          plots if it is necessary.                     *
**************************************************************************
cc   Global variables
c    ALAMBD  Continuation parameter used to deform the interfaces
c            in subroutine rayone.
c    AMP(,) or AMPS(,)  Weighting factors calculated for rays between
c             output points and receivers or shots.
c    ANGLES   Angle from vertical of the ray from output point to
c             shot.
c    ANGLER   Angle from vertical of the ray from output point to
c             receiver.
c    AMPCON  A constant in the inversion algorithm.
c    APREV   Value of ATMP to previous receiver in receiver
c            interpolation.
c    ATMP(), ATMPS()  Weighting factors for rays from current output point
c             to receivers and shot.
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
c    DELHA(), DELHAS():  Difference in weighting factors between output
c            points at same depth but NSKPH traces apart
c            (horizontal interpolation parameter).
c    DELHG(), DELHGS():  Difference in ray parameters in horizontal
c            interpolation.
c    DELHT(), DELHTS():  Difference in traveltimes to receivers between
c            output points at same depth but NSKPH output traces apart
c            (horizontal interpolation).
c    DELRA   Difference in weighting factors between adjacent raypaths.
c    DELRG   Difference in raparameter between adjacent rays.
c    DELRGT  Difference in gradient of traveltimes between adjacent
c            rays.
c    DELRT   Difference in traveltimes between adjacent raypaths
c    DELTAC()  Change in C across a layer.
c    DELTAF()  Change in F across a layer.
c    DELTAX()  x distance travelled within a layer.
c    DELTAZ()  z distance travelled within a layer.
c    DELVA(), DELVAS():  Difference in weighting factors between output
c               points in vertical direction (NSKPV points apart).
c    DELVG(), DELVGS():  Difference in ray parameters in vertical
c               interpolation.
c    DELVGT()  Difference in gradient of traveltimes in vertical
c               interpolation.
c    DELVT(), DELVTS:  Difference in traveltimes to receivers and
c              shot between output points on same trace, NSKPV points
c              apart(vertical interpolation).
c    DF()    First derivative of F w.r.t. X.
c    EXIT    True if output point exits range of definition of input
c            model.
c    F()     Defining function for interfaces.
c    FMAX    Maximum frequency present in time data.
c    GEOINC  Spacing between receivers(shots) to which rays are traced.
c    GRADT(,)   Array used to store the previous value of gradient of
c            traveltimes when calculating the difference between rays.
c    GRADTAU2(,) Array used to store the values of gradient of traveltimes.
c    HSINC   Output trace spacing.
c    I       Loop variable.
c    IDIFF   Element number in TTMP and ATMP of first interpolated
c            value to use.
c    IDIFF1,2  Used in horizontal interpolation to make sure we 
c              interpolate over rays that have the same offset
c              between from shot point and receiver.
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
c    LOST    Total number of rays that could not be found in the inversion.
c    MAXN    Maximum allowed value of N (or change dimensions).
c    MAXNP1  MAXN plus 1.
c    MAXNP3  MAXN plus 3.
c    MAXSPL  Maximum allowed number of points defining an interface.
c    MIGDAT  Name of file containing output depth section.
c    MKPLOT  "y" to make a (ray)plot, "n" to proceed with inversion.
c    MODEL   Name of file describing the input model.
c    MXSPM1  MAXSPL minus 1.
c    N       Number of interfaces between shot and receiver.
c    NDIFFI  The difference between the output trace moveup and the
c            moveup of the first receiver.
c    NHSORC  Number of output traces in current pattern.
c    NINTF   Total number of interfaces in the model.
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
c    OUTPUT(,), OUTPUT1(,): Inversion outputs. The first is reflectivity,
c            and the second is reflectivity times cosine(theta).
c    PARIN   Unit number corresponding to file PARAM.
c    PI      Pi.
c    SAMP    Sample number corresponding to two way traveltime.
c    SIG(,), SIGS(,):  Ray parameter sigmas calculated along a ray.
c    SIGMAS()  Ray parameter of the ray from output point to the shot.
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
c    TS(,)  Traveltime to shot from each output point.
c    TPREV   Value of traveltime to previous receiver.
c    TRACE(,)   Time data to be migrated.
c    TRACES  Name of file containing time section.
c    TRUGEO  Receiver(shot) spacing.
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
c    xs2     Same as XS1
c    XSOURC  x coordinate of first output trace (or source position)
c            in a pattern.
c    XINT(,) x coordinates of points defining interfaces.
c    XSTART  x coordinate of first receiver.
c    ZS      z coordinate of output point.
c    ZSSQD   ZS squared.
c    ZINT(,) z coordinates of points defining interfaces.
c    ZSOURC  z coordinate of first output trace (or source point).



      integer     datin,       datout,       interf, 
     :            parin,       stderr,       datout1

      parameter ( datin  = 10, datout = 11,
     :            datout1= 16, interf = 12,
     :            parin  = 13, stderr = 0 )

      character   traces*15,   model*15,    migdat*15,
     :            migdat1*15

      integer     maxn,        maxnp1,       maxnp3,
     :            maxspl,      mxspm1

      parameter ( maxn   = 50, maxspl = 151)

      parameter( maxnp1 = maxn+1, maxnp3 = maxn+3,
     :           mxspm1 = maxspl-1)

      real       xint(maxn,maxspl),        zint(maxn,maxspl),
     :           a0(maxn,mxspm1),          a1(maxn,mxspm1),
     :           a2(maxn,mxspm1),          a3(maxn,mxspm1)

      integer     npts(maxn),    nintf

      common /a/  xint,          zint,
     :            a0,            a1,        a2,         a3,
     :            npts,          nintf

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
     :            v,               n

      integer     nptmax,          ntrmax,      npanmx,
     :            nrcmax,          nvpmax        

      parameter ( nptmax = 1024, npanmx = 10,
     :            ntrmax = 256,  nrcmax = 256,
     :            nvpmax = 1024)

      real        trace(nptmax,ntrmax),    output(nvpmax,npanmx),
     :            amp(nvpmax,nrcmax),      t(nvpmax,nrcmax),
     :            sig(nvpmax,nrcmax),      gradt(nvpmax,nrcmax),
     :            amps(nvpmax,nrcmax),     ts(nvpmax,nrcmax),
     :            sigs(nvpmax,nrcmax),   
     :            delht(nrcmax),           delha(nrcmax),
     :            delhts(nrcmax),          delhas(nrcmax),
     :            delvt(nrcmax),           delva(nrcmax),
     :            delvts(nrcmax),          delvas(nrcmax),
     :            dbetas(nrcmax),          dbetar
     
      real        atmp(nrcmax),            ttmp(nrcmax),
     :            atmps(nrcmax),           ttmps(nrcmax),
     :            delhg(nrcmax),           delvg(nrcmax),
     :            delvgt(nrcmax),          delhgt(nrcmax),
     :            delhgs(nrcmax),          delvgs(nrcmax),
     :            output1(nvpmax,npanmx),  delrgt,
     :            sigmas(nrcmax),          sigmar(nrcmax),
     :            cosbf,                   gradtau2(nrcmax) 

      real        x(0:maxnp3),             xss(0:maxnp3)

      real        ampcon,     delra,     delrt,     delrg,
     :            sins(nrcmax),fmax,     geoinc,    hsinc,     
     :            rinci,      rincf,     samp,
     :            sinmax,     skipit,    srate,     trugeo,
     :            vsinc,      xr,        xr1,
     :            xs,         xsourc,    xstart,   
     :            zs,         zsourc,    zssqd,     gtprev,
     :            aprev,      tprev,     gprev,     xs1,
     :            angler,     angles(nrcmax),       summand,
     :            rleng,      xs2,       znear

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
     :            ntr1,      losts

c
      integer     na1,       na2,        na3,       krec1
      real        delsdb,    delssi,     delsan,    dbpsev,
     :            sipsev,    anpsev
     
      logical     noconv,    exit,       stay1,     stay2

      character   dash*50

      real       pi
      parameter (pi = 3.141592653589)
c*********************************************************************
c     input and initialize the parameters, the input file is ' param '
c     and ' model ' file.
c*********************************************************************
      open(unit=parin,file='param',err=100)
      go to 105
100   write(stderr,'(a)') 'Can''t open file : param'
      stop
105   rewind parin

c.....Reading from the data file:' param '.
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
155      continue
c........Calculating the cubic spline coefficients of each interface.
         call cuspln(nintf,xint,zint,npts,a0,a1,a2,a3)
         close(unit=interf)
      end if

c.....Initialising.
      noconv = .false.
      exit   = .false.
      c(0)   = 0.
      f(0)   = 0.

c******************************************************************
c     Input the parameters and traces.
c     List the input parameters.
c******************************************************************
c     Set dashed characters for printout.
      do 500  i = 1,  50
         dash(i:i) = '-'
500   continue
      write(stderr,'(6(/),6x,a)') 'RAY INVERSION'
      write(stderr,'(//6x,a)') 'Model Information'
      write(stderr,'(6x,a)') dash(1:17)
      write(stderr,'(//6x,a,1x,i2)')
     :      'Number of interfaces =',nintf

c.....Read layer velocities (shallowest first).
      read(parin,*) (v(i),i=1,nintf+1)

      write(stderr,'(/6x,a)')'Velocities :'
      do 800 i = 1,  nintf + 1
         write(stderr,'(11x,f16.3)') v(i)
800   continue

c.....Read time section data
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
     :     'X-coordinate of first shot position= ',xs1
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
     :     'Depth of first output point =', 
     :      zsourc

      read(parin,*) hsinc
      write(stderr,'(/6x,a,1x,f8.2)')
     :     'Output trace spacing =', 
     :      hsinc

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

c     Reading in the zero offset time data.
      write(stderr,'(4(/),6x,a,1x,a)')
     :     'Reading from the data file',traces

      do 900 i = 1,  nskrec
         read(datin) skipit
900   continue
      do 910 k = 1,  ntrace
         read(datin)(trace(j,k),j=1,npt)
910   continue
      close(unit=datin)
      write(stderr,'(/6x,a,1x,i4,1x,a,1x,i4)')
     :     'Read traces',1,'-',ntrace

c     Initializing the number of rays that could not be found.
      lost = 0
      losts = 0

c     Setting a constant used in the amplitude calculation.
      ampcon = trugeo * 4.0 * pi * sqrt(2.0)

c     Setting the aliasing criterion.
      sinmax = v(1) / (2. * trugeo * fmax )	!changed from 2. to 4.
      znear  = v(1) / (4. * fmax)		!lamda/2.0
c     Initializing the number of traces output.
      nout = 0

c.....Read the number of patterns for this run.
      read(parin,*) npatts
      write(stderr,'(//6x,a,1x,i4)')
     :     'Begin Ray Tracing.      Number of Patterns =',
     :      npatts

c.....Read the number of the first output trace and the first
c     and last receiver in the integration for the first output trace.
      read(parin,*) nout1, nreci1, nrecf1

c     Initialize the current pattern number.
      ipatt = 1

1000  if(ipatt.le.npatts) then
c........Read the skip parameters for this pattern.
         read(parin,*) nskpr, nskpv, nskph
         if(nskph.gt.npanmx) then
            write(stderr,'(a)')
     :      ' MAIN: skipping too many traces, change dimensions.'
             go to 10000
         end if

c........Read the number of the last output trace in the pattern
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
c     
c........Calculate the first and last trace increments.  This 
c        is how the range of integration changes as the output
c        trace number increases.
c
         if(nhsorc.le.1) then
            rinci = 0.
            rincf = 0.
         else
            rinci = float( nreci2 - nreci1 ) / ( nhsorc - 1 )
            rincf = float( nrecf2 - nrecf1 ) / ( nhsorc - 1 )
         end if
c******************************************************************
c	 Initialize output trace number for this pattern.(Horizontal)
c******************************************************************
         ihor = 1

c        Set first and last receiver numbers for first output 
c        trace of first panel in current pattern.

         nri1 = nreci1
         nrf1 = nrecf1
1500     if(ihor.le.nhsorc) then
           nri2 = nreci1 + ( ihor - 1 ) * rinci
           nrf2 = nrecf1 + ( ihor - 1 ) * rincf
           nrec = nrf2 - nri2 + 1
c           decide which traveltimes and amplitudes to be interpolated.
            ndiffi = nri2 - nskph - nri1
            if(ndiffi.ge.0) then
               idiff1 = ndiffi
               idiff2 = 0
               itrace = ndiffi + nri1 + nskph
            else
               idiff1 =  0
               idiff2 =  - ndiffi
               itrace =  nri1 + nskph
            end if
            nrange = min(nrf2-nskph,nrf1) -
     :               max(nri2-nskph,nri1) + 1

c           Updating the source (or output trace) location.
            xs = xsourc + ( ihor - 1 ) * hsinc

c           depths of the interfaces in the stratified medium.
            call strat(xs,c,exit)
            if(exit) then
               write(stderr,'(a)') ' MAIN: Source outside model.'
               stop
            end if
c******************************************************************
c 	    Initializing the output depth point number.(Vertical)
c******************************************************************
            nskpv1 = nskpv
            jver = 1
2000        if(jver.le.nvsorc) then
c              depth of the source (output) point.
               zs = zsourc + (jver-1) * vsinc

c              number of interfaces between source and receivers.
               call setn(zs,c,nintf,n,znear,deltac,f)

c              first receiver & shot location for this output point.
               xr   = xstart + ( nri2 - 1 ) * trugeo
               xss2 = xs1    + ( nri2 - 1 ) * trugeo
c
c..............Begin ray tracing and interpolation for this output point.
               if(n.eq.0) then
c                 No interfaces. Ray is a straight line. No interpolation.
                  zssqd = zs**2
                  xr1 = xr
		  xs2 = xss2
                  if(zs.lt.vsinc) then
c                    ray from source to shot
                     do 2450 i = 1,  nrec 
                        d(1) = sqrt((xs2-xs)**2 + zssqd)
                        call ttime(d,v,n,ttmps(i))
                        atmps(i) = 0.
                        xs2 = xs2 + trugeo
2450                 continue

c                    ray to the source from receiver
                     do 2500 i = 1,  nrec 
                        d(1) = sqrt((xr1-xs)**2 + zssqd)
                        call ttime(d,v,n,ttmp(i))
                        atmp(i) = 0.
                        xr1 = xr1 + trugeo
c                       set gradtau2=1. to avoid division by zero 
                        gradtau2(i) = 1.
2500                 continue
                  else
c                    ray to the source from shot
                     do 2525 i = 1,  nrec
                        deltax(1) = xs - xs2
                        d(1) = sqrt(deltax(1)**2 + zssqd)
                        call ttime(d,v,n,ttmps(i))
                        deltaz(1) = zs
                        call amplit(sinmax,sins(i),atmps(i),sigmas(i),
     *			cosbf,0)
     			sins(i) = deltax(1)/d(1)
                        dbetas(i) = cosbf/d(1)
                        call anglesv(deltax(1),zs,angles(i))
                        xs2 = xs2 + trugeo
2525                 continue
                    
c                    ray to the source from receiver
                     do 2550 i = 1,  nrec
                        deltax(1) = xs - xr1
                        d(1) = sqrt(deltax(1)**2 + zssqd)
                        call ttime(d,v,n,ttmp(i))
                        deltaz(1) = zs
                        call amplit(sinmax,sins(i),atmp(i),sigmar(i),
     *			cosbf,1)
                        dbetar = cosbf/d(1)
                        ww = sqrt(dbetar/dbetas(i))
                        call anglesv(deltax(1),zs,angler)
                        call magnitude(angles(i),angler,gradtau2(i))
                        atmp(i) = atmp(i)*(ww+1./ww)
c                       atmp(i) = atmp(i)/gradtau2(i)*(ww+1./ww)
                        xr1 = xr1 + trugeo
2550                 continue
                  end if
               else
		  na1 = nint((xstart-xs1)/trugeo)
		  na2 = na1/nskpr*nskpr
		  
c......Trace rays and interpolate rays to shots
                  nskpr1 = nskpr
                  geoinc = trugeo * nskpr
                  krec = 1

2800              if(krec.le.nrec) then
                     if(krec.eq.1.or.noconv) then
                        call rayone(xss2,xs,zs,n,v,c,xss,noconv)
                     else
                        call reccon(xss,n,xss2,geoinc,v(1),noconv)
                        if(noconv) then
                           xss2= xss2 + geoinc
                           call rayone(xss2,xs,zs,n,v,c,xss,noconv)
                        end if
                     end if

                     if(noconv) then
c                       no ray. Set shot location for the next ray.
                        xss2 = xss2 + geoinc
                        ttmps(krec) = 99999.
                        atmps(krec) = 0.
                        losts = losts + 1
                     else
                        call ttime(d,v,n,ttmps(krec))
                        rleng=0.
                        do 21 nl=1,n+1
                           rleng=rleng + d(nl)
21                      continue
             		call amplit(sinmax,sins(krec),atmps(krec),
     *      		sigmas(krec),cosbf,0)
     			sins(krec) = deltax(1)/d(1)
                        dbetas(krec)=1./rleng*cosbf
                        call anglesv(deltax(n+1),deltaz(n+1),
     *  		     angles(krec))
                     end if
		       
ccc                  Interpolate to shot location between rays
                     if(krec.eq.1.or.nskpr.eq.1) then
c                       Nothing to interpolate.
                     else
                        kprev = krec - nskpr
                        delst = (ttmps(krec) - ttmps(kprev)) / nskpr
                        delsa = (atmps(krec) - atmps(kprev)) / nskpr
                        delsg = (sigmas(krec) - sigmas(kprev)) / nskpr

                        tpsev = ttmps(kprev)
                        apsev = atmps(kprev)
                        gpsev = sigmas(kprev)
 
                        do 2825 i = 1,  nskpr - 1

                           tpsev = tpsev + delst
                           ttmps(kprev+i) = tpsev

                           apsev = apsev + delsa
                           atmps(kprev+i) = apsev

                           gpsev = gpsev + delsg
                           sigmas(kprev+i) = gpsev
2825                    continue
			if(krec.gt.na1 ) then
                           delsdb = (dbetas(krec)-dbetas(kprev))/nskpr
                           delssi = (sins(krec)-sins(kprev))/nskpr
                           delsan = (angles(krec)-angles(kprev))/nskpr
                           dbpsev = dbetas(kprev)
                           sipsev = sins(kprev)
                           anpsev = angles(kprev)
c
                           do 2835 i = 1, nskpr-1
                              dbpsev = dbpsev + delsdb
                              dbetas(kprev+i) = dbpsev

                              sipsev = sipsev + delssi
                              sins(kprev+i) = sipsev

                              anpsev = anpsev + delsan
                              angles(kprev+i) = anpsev
2835                       continue
			end if
                     end if
                     krec = krec + nskpr
                     go to 2800
                  end if

c                 Make sure we found the ray to the last shot
                  krec = krec - nskpr
                  if(krec.lt.nrec) then
                     nskpr = nrec - krec
                     krec = nrec
                     geoinc = trugeo * nskpr
                     go to 2800
                  end if
		  
		   do 2836 j = 1, nrec-na1
                  	ttmp(j)  = ttmps(na1+j)  
		     	atmp(j)  = atmps(na1+j)
                     	sigmar(j) = sigmas(na1+j)
                        dbetar    = dbetas(na1+j)
                     	sinr      = sins(na1+j)  
		     	angler    = angles(na1+j)
c
                     	gradtau2(j)=sqrt((1.+cos(angles(j)-angler))/2.)
       	             	if (abs(sinr+sins(j)).gt.sinmax) then
		     	   atmp(j) = 0.
		     	else
			   if(dbetar.eq.0.0 .or. dbetas(j).eq.0.0) then
		           	atmp(j) = 0.0
			   else
	                   	ww = sqrt(dbetar/dbetas(j))
		           	atmp(j) = atmp(j)*(ww+1./ww)
			   end if
		     	end if
2836                 continue

                  nskpr = nskpr1

c......Trace rays and interpolate rays to receivers
                  geoinc = trugeo * nskpr
c                 Initializing the receiver number for this set of rays.
    	          krec1 = max0(nrec-na1+1,1)
		  xr1 = xr + (krec1-1)*trugeo
		  krec = krec1

3000              if(krec.le.nrec) then
                     if(krec.eq.krec1.or.noconv) then
                        call rayone(xr1,xs,zs,n,v,c,x,noconv)
                     else
                        call reccon(x,n,xr1,geoinc,v(1),noconv)
                        if(noconv) then
                          xr1 = xr1 + geoinc
                           call rayone(xr1,xs,zs,n,v,c,x,noconv)
                        end if
                     end if

                     if(noconv) then
c                       No ray. Set receiver location for the next ray.
                        xr1 = xr1 + geoinc
                        ttmp(krec) = 99999.
                        atmp(krec) = 0.
                        lost = lost + 1
                     else
                       call ttime(d,v,n,ttmp(krec))
                        rleng=0.
                        do 20 nl=1,n+1
                           rleng=rleng + d(nl)
20                      continue
                        call amplit(sinmax,sins(krec),atmp(krec),
     *			sigmar(krec),cosbf,1)
                        dbetar=1./rleng*cosbf
	                ww=sqrt(dbetar/dbetas(krec))
                        call anglesv(deltax(n+1),deltaz(n+1),angler)
                        call magnitude(angles(krec),angler,
     *						gradtau2(krec))
                        atmp(krec)=atmp(krec)*(ww+1./ww)
		     end if

c                    Interpolate to receivers between rays
                     if(krec.eq.krec1.or.nskpr.eq.1) then
c                       Nothing to interpolate.
                     else
                        kprev = krec - nskpr
                        delrt = (ttmp(krec) - ttmp(kprev)) / nskpr
                        delra = (atmp(krec) - atmp(kprev)) / nskpr
                        delrg = (sigmar(krec) - sigmar(kprev)) / nskpr
                        delrgt= (gradtau2(krec)-gradtau2(kprev))/nskpr

                        tprev = ttmp(kprev)
                        aprev = atmp(kprev)
                        gprev = sigmar(kprev)
                        gtprev = gradtau2(kprev)

                        do 3025 i = 1,  nskpr - 1
                           tprev = tprev + delrt
                           ttmp(kprev+i) = tprev
                           aprev = aprev + delra
                           atmp(kprev+i) = aprev
                           gprev = gprev + delrg
                           sigmar(kprev+i) = gprev
                           gtprev=gtprev+delrgt
                           gradtau2(kprev+i)=gtprev
3025                    continue
                     end if
                     krec = krec + nskpr
                     go to 3000
                  end if

c                 Make sure we found the ray to the last receiver
                  krec = krec - nskpr
                  if(krec.lt.nrec) then
c                    Get ready to trace to last receiver.
                     nskpr = nrec - krec
                     krec = nrec
                     geoinc = trugeo * nskpr
                     go to 3000
                  end if
                  nskpr = nskpr1
               end if

ccc   End of ray tracing and receiver interpolation for output point.
c***********************************************************************
ccc            Start the vertical and horizontal interpolation.
c***********************************************************************
c .............Vertical interpolation.
               if(jver.eq.1.or.nskpv.eq.1) then
c                 Nothing to interpolate vertically.
               else
                  jverl = jver - nskpv
                  do 3050 i = 1,  nrec
                     delvts(i) = (ttmps(i) - ts(jverl,i)) / nskpv
                     delvas(i) = (atmps(i) - amps(jverl,i)) / nskpv
                     delvgs(i) = (sigmas(i) - sigs(jverl,i)) / nskpv
3050              continue

                  do 3075 i = 1,  nrec
                     delvt(i) = (ttmp(i) - t(jverl,i)) / nskpv
                     delva(i) = (atmp(i) - amp(jverl,i)) / nskpv
                     delvg(i) = (sigmar(i) - sig(jverl,i)) / nskpv
                     delvgt(i) = (gradtau2(i) - gradt(jverl,i)) / nskpv
3075              continue
               end if
               stay1 = .true.
               jv = 1

3100           if(jv.le.nskpv.and.stay1) then
                  jmjp1 = jver - jv + 1
                 if(jv.eq.1) then
c                    given traveltimes at this point from raytracing
                  else
                     do 3150 i = 1,  nrec
                        ttmps(i) = ttmps(i) - delvts(i)
                        atmps(i) = atmps(i) - delvas(i)
                        sigmas(i) = sigmas(i) - delvgs(i)
3150                 continue

                     do 3200 i = 1,  nrec
                        ttmp(i) = ttmp(i) - delvt(i)
                        atmp(i) = atmp(i) - delva(i)
                        sigmar(i) = sigmar(i) - delvg(i)
                        gradtau2(i) = gradtau2(i)-delvgt(i)
3200                 continue
                  end if

c.................Horizontal interpolation.
                  if(ihor.eq.1.or.nskph.eq.1) then
c                    No horizontal interpolation necessary.
                  else
                     do 3250 i = 1,  nrange
                        delhts(i) = ( ttmps(i+idiff2) -
     :                               ts(jmjp1,i+idiff1) )/nskph
                        delhas(i) = ( atmps(i+idiff2) -
     :                               amps(jmjp1,i+idiff1) )/nskph
                        delhgs(i) = ( sigmas(i+idiff2) -
     :                               sigs(jmjp1,i+idiff1) )/nskph
3250                 continue
c
                     do 3300 i = 1,  nrange
                        delht(i) = ( ttmp(i+idiff2) -
     :                               t(jmjp1,i+idiff1) )/nskph
                        delha(i) = ( atmp(i+idiff2) -
     :                               amp(jmjp1,i+idiff1) )/nskph
                        delhg(i) = ( sigmar(i+idiff2) -
     :                               sig(jmjp1,i+idiff1) )/nskph
                        delhgt(i) = ( gradtau2(i+idiff2) -
     :                               gradt(jmjp1,i+idiff1) )/nskph
3300                 continue
                  end if

c                 Set time and amp to new values found above
c                 from either ray tracing or vertical interpolation.
                  do 3325 i = 1,  nrec
                     ts(jmjp1,i)   = ttmps(i)
                     amps(jmjp1,i) = atmps(i)
                     sigs(jmjp1,i) = sigmas(i)
3325              continue
c                
		  do 3350 i = 1,  nrec
                     t(jmjp1,i)   = ttmp(i)
                     amp(jmjp1,i) = atmp(i)
                     sig(jmjp1,i) = sigmar(i)
                     gradt(jmjp1,i)=gradtau2(i)
3350              continue
                  ih = 1
                  stay2 = .true.

3400              if(ih.le.nskph.and.stay2) then
                     if(ih.eq.1) then
                        ntr1  = nri2 - 1
                        idiff = 0
                     else
                        idiff = idiff2
                        ntr1  = itrace - ih
                        do 3450 i = 1,  nrange
                           ttmps(i+idiff2) = ttmps(i+idiff2) - delhts(i)
                           atmps(i+idiff2) = atmps(i+idiff2) - delhas(i)
                           sigmas(i+idiff2)= sigmas(i+idiff2)- delhgs(i)
3450                    continue
c
                        do 3500 i = 1,  nrange
                           ttmp(i+idiff2) = ttmp(i+idiff2) - delht(i)
                           atmp(i+idiff2) = atmp(i+idiff2) - delha(i)
                           sigmar(i+idiff2)=sigmar(i+idiff2)-delhg(i)
                           gradtau2(i+idiff2)=gradtau2(i+idiff2) -
     :                                        delhgt(i)
3500                    continue
                     end if

                     output(jmjp1,ih)  = 0.0
                     output1(jmjp1,ih) = 0.0

                     do 4325 i = 1,  nrange
                        samp  = (ttmps(i+idiff)+ttmp(i+idiff))/srate+1.
                        nsamp = samp 
                        if(nsamp.gt.npt-1) then
c                          Traveltime too great.
                        else
                          ntr = ntr1 + i
c                          Sum in this sample, weighted by ampl factor.
                           summand = ampcon * atmps(i+idiff) *
     :                       sqrt(sigmas(i+idiff)+sigmar(i+idiff))*
     :                       atmp(i+idiff)*(trace(nsamp,ntr) +
     :                       (samp - nsamp) * (trace(nsamp+1,ntr) -
     :                       trace(nsamp,ntr) ) )
c                          reflectivity function
                           output(jmjp1,ih) = output(jmjp1,ih)+summand
c                          reflectivity times cos(theta)
                           output1(jmjp1,ih) = output1(jmjp1,ih)
     :                                   + summand *gradtau2(i+idiff)
                        end if
4325                 continue

                     ih = ih + 1
                     if(ihor.eq.1)stay2 = .false.
                     go to 3400
                  end if

                  jv = jv + 1
                  if(jver.eq.1) then
                     stay1 = .false.
                  else
                     if(nskph.gt.1.and.ihor.gt.1) then
c.................................................................
c                       Original values of ttmp and atmp have been
c                       altered by horizontal interpolation.
c                       Return to these values in order to proceed
c                       with vertical interpolation.
c.................................................................

                        do 4330 i = 1,  nrec
                           ttmps(i)  = ts(jmjp1,i)
                           atmps(i)  = amps(jmjp1,i)
                           sigmas(i) = sigs(jmjp1,i)
4330                    continue

                        do 4350 i = 1,  nrec
                           ttmp(i)  = t(jmjp1,i)
                           atmp(i)  = amp(jmjp1,i)
                           sigmar(i)= sig(jmjp1,i)
                           gradtau2(i)=gradt(jmjp1,i)
4350                    continue
                     end if
                  end if
                  go to 3100
               end if
               jver = jver + nskpv
               go to 2000
            end if

c           Make sure do all points on trace. 
            jver = jver - nskpv
            if(jver.lt.nvsorc) then
               nskpv = nvsorc - jver
               jver = nvsorc
               go to 2000
            end if
            nskpv = nskpv1
c....................................................................
c           Output panel of traces. Traces were calculated from right 
c           to left across panel using our interpolation scheme.
c....................................................................
            if(ihor.eq.1) then
               itrace = 1
            else
               itrace = nskph
            end if

c           output in reverse order
            do 4500 i = itrace,  1,  -1
               write(datout)(output(j,i),j=1,nvsorc)
               write(datout1)(output1(j,i),j=1,nvsorc)
4500        continue
c
            write(stderr,'(6x,a,1x,i4,8x,a,1x,i4,a,i4)')
     :           'Output trace #',ihor+nout,
     :           'Sum over receivers',nri2,'-',nrf2
            nri1 = nri2
            nrf1 = nrf2
            ihor = ihor + nskph
            go to 1500
         end if
 
c        Make sure we've done all traces in this pattern.
         ihor = ihor - nskph
         if(ihor.lt.nhsorc) then
            nskph = nhsorc - ihor
            ihor = nhsorc
            go to 1500
         end if
	 
c........Pattern is done.
         nout1 = nout2 + 1
         nreci1 = nreci2
         nrecf1 = nrecf2
         nout = nout + ihor
         xsourc = xsourc + ihor * hsinc
         ipatt = ipatt + 1
         go to 1000
      end if
c.....Finished all patterns.
c*****************************************************************
c     output the reflectivity and reflectivity*cos
c*****************************************************************
      write(stderr,'(/6x,a,1x,i4)') 
     :     'Number of rays lost(source)   = ',losts
      write(stderr,'(/6x,a,1x,i4)') 
     :     'Number of rays lost(receiver) = ',lost
      write(stderr,'(/6x,a,1x,a/)')
     :     'The reflectivity output is the file', migdat
      write(stderr,'(/6x,a,1x,a/)')
     :     'The reflectivity*cos(theta) is the file',migdat1
10000 close(unit=parin)
      close(unit=datout)
c
      stop
      end
*--------------------------------------------------------------------


