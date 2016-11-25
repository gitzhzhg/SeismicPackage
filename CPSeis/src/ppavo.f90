!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- ppavo.f90 --------------------------------!!
!!------------------------------- ppavo.f90 --------------------------------!!
!!------------------------------- ppavo.f90 --------------------------------!!

!
! other files are: ppavo_crou.c
!
!
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : ppavo 
! Category   : velocity
! Written    : 2003-08-26   by: Bill Lucas
! Revised    : 2007-12-13   by: Bill Menger
! Maturity   : beta
! Purpose    : AVO and velocity analysis subroutines.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive contains most of the computational subroutines used in
! the AVO Workbench of tools: AVO & Velocity Analysis, AVO Standard Suite,
! AVO Alternate Norm Suite, and AVO Velocity Iteration.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!                        SUBROUTINE ARGUMENTS
!                         SUBROUTINE DETAILS
!
! PPAVO_AVOE:
!  (1) Computes AVO residual errors.
!
!                   b  n  i   i    i    i  i   i
!  call ppavo_avoe (s,out,n,first,last,isz,no,nstg)
!
!  integer   n     = dimensioned size of the 's' and 'out' arrays.
!  integer   first = first sample to analyze.
!  integer   last  = last sample to analyze.
!  integer   isz   = number of stages.
!  integer   no    = number of output stages to generate.
!  integer   nstg  = number of calculated stages.
!  real(:,:) s     = collected weighted sums (see subroutine details).
!  real(:,:) out   = output data (see subroutine detail).
!
!
! PPAVO_CNVITT:
!  (1) Converts RMS velocities to interval transit times.
!
!                      i    o  i   i  i
!  call ppavo_cnvitt (intr,otr,ns,isi,t0)
!
!  integer ns   = size of 'intr' and 'otr' arrays.
!  integer isi  = sampling interval (usecs).
!  real(:) intr = input trace.
!  real(:) otr  = output trace.
!  real    t0   = unknown.
!
!
! PPAVO_DUMPVL:
!  (1) Dumps velocity traces.
!
!                     
!  call ppavo_dumpvl (dunit,vel,ns,fstd,lstd,inc,icdpn,isi,bulk)
!
!  integer dunit = Fortran i/o unit number.
!  integer ns    = size of velocity array.
!  integer fstd  = first sample to dump.
!  integer lstd  = last sample to dump.
!  integer inc   = increment between samples.
!  integer icdpn = this CDP number.
!  integer isi   = sampling interval (usecs).
!  integer bulk  = time of first samples (msec).
!  real(:) vel   = stacking velocity trace.
!
!
! PPAVO_HERMTP:
!  (1) Solves the set of real linear simultaneous equations.
!
!                     i i  i i b b   b
!  call ppavo_hermtp (m,t0,t,z,x,a,istat)
!
!  integer m     = order of matrix 't'.
!  integer istat = status indicator at time of exit.
!  real    t0    = scalar corresponding to real matrix element 't(0)'.
!  real(:) t     = array of N real matrix elements from the left
!                  column of the Toeplitz matrix.
!  real(:) z     = array of N+1 real elements of the right-hand-side vector.
!  real(:) x     = array of M+1 real elements of the solution vector.
!  real(:) a     = scratch array.
!
!
! PPAVO_IDAMAX:
!  (1) Finds position of largest magnitude element in an array.
!
!                        i,i, i
!  integer ppavo_idamax (n,x,inc)
!
!  integer             inc  = stride to use with 'x' array.
!  integer             n    = number of elements in 'x' array.
!  double precision(:) x    = data array.
!
!
! PPAVO_ISMAX:
!  (1) Finds the maximum value in an array.
!
!                       i,i, i
!  integer ppavo_ismax (n,a,ninc)
!
!  integer ninc = stride to use with 'a' array.
!  integer n    = number of elements in 'a' array.
!  real(:) a    = data array.
!
!
! PPAVO_ISMIN:
!  (1) Finds the minimum value in an array.
!
!                       i,i, i
!  integer ppavo_ismin (n,a,ninc)
!
!  integer ninc = stride to use with 'a' array.
!  integer n    = number of elements in 'a' array.
!  real(:) a    = data array. 
!
!
! PPAVO_MACONV:
!  (1) Generalizes the SPARC routine 'ARCON' by providing the argument 'noff'.
!      It also featurs enhanced efficienty since it vectorizes on the I loop
!      of the dimension 'nab' rather than the J loop of the dimension 'nf'.
!      Since 'nab' is usually greater than 'nf', this re-ordering results in
!      increased execution speed.
!
!                     i i b  i  i   i
!  call ppavo_maconv (a,f,b,nab,nf,noff)
!
!  integer nab  = dimensioned lengths of 'a' and 'b'.
!  integer nf   = length of filter function.
!  integer noff = index the filter function which multiples the I-th value
!                 of 'a' into the I-th value of 'b'.
!  real(:) a    = inpute vector to convolve.
!  real(:) f    = filter function to convolve with.
!  real(:) b    = convolved array.
!
!
! PPAVO_MAGPHS
!  (1) Computes squared magnitude and phase of vector of complex numbers.
!
!                     b   b     b      b    i
!  call ppavo_magphs (ar, ai, magsq, phase, ns)
!
!      integer   ns   = size of arrays.
!      real(:)   ar   = real part of input vector.
!      real(:)   ai   = imag part of input vector.
!      real(:)   magsq = output squared magnitudes vector.
!      real(:)   phase = output phase vector.
!
!
! PPAVO_MAHLBT:
!  (1) Uses the real part of the complex array A to compute its Hilbert
!      transform, which it places in the imaginary part of the array.
!
!                     b b i   i   i    i
!  call ppavo_mahlbt (a,h,nt,is1,is2,ftrlen)
!
!  integer nt     = dimensioned lengths of 'a' and 'h'.
!  integer is1    = index of first sample to transform.
!  integer is2    = index of last sample to transform.
!  integer ftrlen = length of the Hilbert transform, where valid lengths
!                   are 0,31,43,63 and 95 (0 suppresses transformation).
!  real(:) a      = input vector to Hilbert transform.
!  real(:) h      = output transformed vector.
!
!
! PPAVO_MASMTH:
!  (1) Convoles a vector with a rectangular filter.
!
!                     i    i    i  b b i
!  call ppavo_masmth (nx,istrid,id,x,y,l)
!
!  integer   nx     = length of input and output vectors.
!  iteger    istrid = stride of input vector (output stride is 1).
!  integer   id     = unknown.
!  integer   l      = half-length of boxcar smoothing filter.
!  real(:,:) x      = input vector, containing 'nx' elements.
!  real(:)   y      = output vector, to receive 'nx' smoothed elements.
!
!
! PPAVO_MATV:
!  (1) Performs AVO analysis, for either a 1, 2 or 3 term inversion.
!
!                   b  b   i  i    i     b    b
!  call ppavo_matv (s,out,num,nd,nsamp,first,last,
!                   nstg,isz,taper,avop)
!                    i    i    i    i
!
!  integer      nsamp = number of samples under analysis.
!  integer      first = first sample to compute in the 'out' array.
!  integer      last  = last sample to compute in the 'out' array.
!  integer      nstg  = number of AVO inversions to perform per time sample.
!  integer      taper = number of tapering samples at ends of analysis window.
!  integer(:,:) num   = (see subroutine for details).
!  integer(:)   nd    = (see subroutine for details).
!  real(:,:)    s     = collected weighted sums (see subroutine for details).
!  real(:,:)    out   = (see subroutine for details).
!  real         avop  = parameter related to AVO stability factor AVOPCT, where
!                       AVOP = 1[1 + 10**(AVOPCT/20)].
!
!
! PPAVO_MAXMGV:
!  (1) Locates maximum magnitude element of a vector.
!
!                     i i o o  i
!  call ppavo_maxmgv (a,i,r,lr,n)
!
!  integer n  = size of input vector
!  integer i  = stride of input vector.
!  integer lr = location of maximum maginitude.
!  real    r  = maximum magnitude.
!  real(:) a  = input vector.
!
!
! PPAVO_MCARTS
!  (1) Converts polar coords into Cartesian coords.
!
!                      b     b    b   b   i
!  call ppavo_mcarts (mag, phase, rl, im, ns)
!
!      integer ns    = size of arrays.
!      real(:) mag   = magnitude of input vector.
!      real(:) phase = phase of input vector.
!      real(:) rl    = real part of input vector.
!      real(:) im    = imag part of imput vector.
!
!
! PPAVO_MCOFGN:
!  (1) Computes 99 interpolation filters on each call, which can be used
!      to compute value of a signal to the nearest 0.001 sample interval,
!      which is adequate to maintain 30-40 db fideltiy in interpolation.
!
!                       i     i     b    b    b    b    o
!  call ppavo_mcofgn (lfltr,mxfreq,acor,ccor,fltr,work,coef)
!
!  integer lfltr = number of coefficients in the filters.
!  real    mxfreq = maximum frequency as a fraction of Nyquist.
!  real(:) acor   = array for auto-correlation.
!  real(:) ccor   = array for cross-correlation.
!  real(:) fltr   = filter array used by subroutine ppavo_mwienr.
!  real(:) work   = work array used by subroutine ppavo_mwienr.
!  real(:) coef   = coefficient array (99 filters of length 'lfltr').
!
!
! PPAVO_MPOWR
!  (1) Raises positive real vector to scalar exponent.
!
!                    i    i     o   i
!  call ppavo_mpowr (in, exp2, out, ns)
!
!      integer   ns   = size of arrays.
!      real      exp2 = exponent
!      real(:)   in   = input vector.
!      real(:)   out  = squared magnitudes.
!
!
! PPAVO_MWIENR:
!  (1) Finds solutions of single-channel normal equations which
!      arise in least-squares filter and prediction problems for
!      single-channel time series.
!
!                       i    b    b   b   b
!  call ppavo_mwienr (lfltr,acor,rhs,err,fltr)
!
!  integer lfltr = length of the filter.
!  real(:) acor  = auto-correlation coefficients.
!  real(:) rhs   = cross-correlation coefficients.
!  real(:) err   = prediction error operator.
!  real(:) fltr  = filter coefficients.
!
!
! PPAVO_NACMA:
!  (1) Multiples the complex 'b' vector by the complex conjugate of
!      the complex 'k' vector, and adds the result to the complex
!      'a' vector.
!
!                    b  b  i  i  i  i  i
!  call ppavo_nacma (ar,ai,br,bi,kr,ki,ns)
!
!  integer ns = size of complex arrays.
!  real(:) ar = real part of complex 'a' vector.
!  real(:) ai = imag part of complex 'a' vector.
!  real(:) br = real part of complex 'b' vector.
!  real(:) bi = imag part of complex 'b' vector.
!  real(:) kr = real part of complex 'k' vector.
!  real(:) ki = imag part of complex 'k' vector.
!
!
! PPAVO_NAACMS:
!  (1) Computes squared magnitude of complex vector field averaged
!      over a space-time window.
!
!                      b  b i  o   i   i  i   i    i
!  call ppavo_naacms (sum,a,w,tmp,fsa,lsa,ns,twin,fac)
!
!  integer             fsa  = first sample under analysis.
!  integer             lsa  = last sample under analysis.
!  integer             ns   = size of arrays.
!  integer             twin = time window length (samples).
!  real                fac  = combination scale factor.
!  real(:)             a    = data to use; weighted smoothed data.
!  real(:)             w    = weights for the average.
!  real(:)             tmp  = temporary array.
!  double precision(:) sum  = running sum of |A|**2.
!
!
! PPAVO_NABIGS:
!  (1) Computes imaginary part of the element by element product of
!      the A complex vector times ccomplex conjuage of the B vector.
!
!                     i  i  i  i   o  i
!  call ppavo_nabigs (ar,ai,br,bi,out,ns)
!
!  integer ns  = size of arrays.
!  real(:) ar  = real part of A vector.
!  real(:) ai  = imaginary part of A vector.
!  real(:) br  = real part of B vector.
!  real(:) bi  = imaginary part of B vector.
!  real(:) out = imaginary part of AB* vector.
!
!
! PPAVO_NABRGS:
!  (1) Computes real part of the element by element product of
!      the A complex vector times ccomplex conjuage of the B vector.
!
!                     i  i  i  i   o  i
!  call ppavo_nabrgs (ar,ai,br,bi,out,ns)
!
!  integer ns  = size of arrays.
!  real(:) ar  = real part of A vector.
!  real(:) ai  = imaginary part of A vector.
!  real(:) br  = real part of B vector.
!  real(:) bi  = imaginary part of B vector.
!  real(:) out = imaginary part of AB* vector.
!
!
! PPAVO_NAMAGS:
!  (1) Computes the magnitude of the elements of the A vector.
!
!                     i  i  o  i
!  call ppavo_namags (ar,ai,out,ns)
!
!  integer ns  = size of arrays.
!  real(:) ar  = real part of A vector.
!  real(:) ai  = imaginary part of A vector.
!  real(:) out = |A|**2 vector.
!
!
! PPAVO_NAMSUM:
!  (1) Computes sliding moving average.
!
!                      i   o   i    i
!  call ppavo_namsum (ain,aout,ns,ntwin)
!
!  integer ns    = size of arrays.
!  integer ntwin = number of samples in moving average.
!  real(:) ain   = array to average.
!  real(:) aout  = averaged array.
!
!
! PPAVO_NAPOWI:
!  (1) Computes fixed integer power of a real array.
!
!                     i i   o  i
!  call ppavo_napowi (a,ip,out,ns)
!
!  integer ns  = size of arrays.
!  integer ip  = scalar integer power.
!  real(:) a   = array to be powered.
!  real(:) out = array of a**ip.
!
!
! PPAVO_NAPOWR:
!  (1) Computes power of the elements of an array. The elements of
!      this array must be positive, other zero will be returned.
!
!                     i i  o i
!  call ppavo_napowr (a,p,out,ns)
!
!  integer ns  = size of arrays.
!  real(:) p   = array of powers.
!  real(:) a   = array to be powered.
!  real(:) out = array of a**p.
!
!
! PPAVO_NAZERO:
!  (1) Zeros out elements of the A array whose corresponding elements
!      in the Z array are also zero.
!
!                     b,i,i
!  call ppavo_nazero (a,z,ns)
!
!  integer ns = size of arrays.
!  real(:) a  = array of elements to zero.
!  real(:) z  = template array.
!
!
! PPAVO_RMSSCL:
!  (1) Computes scale factor to apply to a section to make its
!      RMS value to be 'SCLFCT' * 1000. This is good for usec
!      plotting with a gain of 12 db.
!
!                       b     i     i
!  call ppavo_rmsscl (stats,nstat,sclfct)
!
!  integer nstat  = size of statistics array.
!  real    sclfct = scale factor.
!  real(:) stats  = statistics array.
!
!
! PPAVO_RUNSC:
!  (1) Computes the RUNS statistic.
!
!                     i    b   i   i
!  call ppavo_runsc (runs,out,nsa,pos)
!
!  integer      nsa  = number of samples under analysis.
!  integer      pos  = position of the output array.
!  integer(:,:) runs = raw data.
!  real(:,:)    out  = RUNS statistic.
!
!
! PPAVO_SAAHCI
!  (1) Unknown.
!
!                    
!  call ppavo_saahci (ar,ai,br,bi,pow,hci,ax,bx,siga,sigb,alpha,ns,sclopt)
!
!  integer ns     = unknown.
!  integer sclopt = unknown.
!  real    alpha  = unknown.
!  real(:) ar     = unknown.
!  real(:) ai     = unknown.
!  real(:) br     = unknown.
!  real(:) bi     = unknown.
!  real(:) pow    = unknown.
!  real(:) hci    = unknown.
!  real(:) ax     = unknown.
!  real(:) bx     = unknown.
!  real(:) siga   = unknown.
!  real(:) sigb   = unknown.
!
!
! PPAVO_SAARVI
!  (1) Used in computation of the optimal hydrocarbon and residual
!      velocity indicators.
!
!                     i  i  b  b   i    i    o   o   o   i    i
!  call ppavo_saarvi (ar,ai,br,bi,siga,sigb,hci,rvi,amag,ns,sclopt)
!
!  integer ns     = size of all arrays.
!  integer sclopt = scaling option of indicators.
!  real(:) amag   = sqrt{HCI}.
!  real(:) br     = original RE{B}; scaled RE{B} no quadrature.
!  real(:) bi     = original IM{B}; scaled IM{B} no quadrature.
!  real(:) ar     = original RE{A}.
!  real(:) ai     = original IM{A}.
!  real(:) siga   = sqrt { avg(|A|**2) }.
!  real(:) sigb   = sqrt { avg(|B|**2) }.
!  real(:) hci    = |A|**2 + |B|**2, after scaling.
!  real(:) rvi    = IM{AB*}/amag, after scaling (res. vel. indic.)
!
!
! PPAVO_SABNFT
!  (1) Generates H1(T) and H2(T) inversion filters for NMO destretching,
!      specifically for a Gaussian bandpass wavelet.
!
!                     o  o   o     o    o    i  i    i      i    i i   o
!  call ppavo_sabnft (h1,h2,corr,cvect,work,mxh,nh,pctwns,pctcns,a,ks,ier)
!
!  integer          ier    = error flag.
!  integer          ks     = second dimension of work array.
!  integer          mxh    = dimensioned size of output inversion filters.
!  integer          nh     = actual size of output inversion filters.
!  real(:)          pctwns = additional white noise percentage.
!  real(:)          pctcns = additional colored noise percentage.
!  real(:)          cvect  = cross-correlation vector between W(T) and TW'(T).
!  real(:)          h1     = inversion filter.
!  real(:)          h2     = inversion filter.
!  real(:)          corr   = auto-correlation of the wavelet.
!  real(:)          work   = work array.
!  double precision a      = auto-regressive noise coefficient.
!
!
! PPAVO_SABWAVE:
!  (1) Computes the auto-correlation of a bandpass wavelet.
!
!                       o    o   i    o      o     i   i  i    i
!  call ppavo_sabwave (fftr,fftc,ns,wavelt,twvltp,maxh,nh,fs,wpars)
!
!  integer    ns     = determines length of 'fft' arrays
!  integer    maxh   = determines length of 'wavelt' and 'twvltp' arrays
!  integer    nh     = number of correlation lag values to compute.
!  complex(:) fftc   = complex scratch array, used for FFT.
!  real(:)    fftr   = real scratch array, used for FFT.
!  real(:)    wavelt = auto-correlation of the wavelet.
!  real(:)    twvltp = cross-correlation of the wavelet.
!  real(:)    wpars  = four bandpass wavelet cutoff frequencies.
!  real       fs     = sampling frequency (Hz).
!
!
! PPAVO_SAHCI:
!  (1) Computes various hydrocarbon indicators from collected weight sums.
!
!                    b  i i    i      i     i    b    i     i
!  call ppavo_sahci (s,h1,h2,sloths,slothi,parm,out,maxsmp,maxh,
!                    nparm,first,last,nh,isz,t0,tsamp,avopct)
!                      i     b    b   i   i  i    i     i
!
!  integer    maxsmp = dimensioned sizes of 's', 'sloths', 'slothi',
!                      'aout', and 'bout' arrays.
!  integer    maxh   = size of the 'h' array.
!  integer    nparm  = size of the 'parm' array.
!  integer    first  = first sample to compute.
!  integer    last   = last sample to compute.
!  integer    nh     = actual size of 'h' array.
!  integer    isz    = second dimension of 's' array.
!  integer(:) parm   = integer parameter array (see subroutine for details).
!  real       t0     = time of first given sample (seconds).
!  real       tsamp  = sampling interval (seconds).
!  real       avopct = AVO stabilization term, used in matrix inversion.
!  real(:)    s      = collected weighted sums (see subroutine for details).
!  real(:)    h1     = first set of inversion filter coefficients.
!  real(:)    h2     = second set of inversion filter coefficients.
!  real(:)    sloths = stacking sloth which was used.
!  real(:)    slothi = corresponding interval sloth.
!  real(:,:)  out    = output data (see subroutine for details).
!
!
! PPAVO_SAHCIC:
!  (1) Collects running sums for hydrocarbon detection.
!
!                     b   i     i      b     i   b   b   i    i
!  call ppavo_sahcic (s,trace,sloths,slothi,scratch,isz,fin,lin,fout,lout,
!                     t0,tsamp,xoff,alim1,alim2,angfmt,trcfold)
!                     i    i    i     i     i     i      i
!
!  integer             isz     = second dimension of 's' array.
!  integer             fin     = index of first live value in input trace.
!  integer             lin     = index of last live value in input trace.
!  integer             fout    = index of first output sample under analysis.
!  integer             lout    = index of last out sample under analysis.
!  integer             angfmt  = 0 for standard X-T data,
!                                2 for common-angle data.
!  real                t0      = time of first sample (seconds).
!  real                tsamp   = sample interval (seconds).
!  real                xoff    = shot-receiver offset for this trace, or
!                                the angle (degrees) for common-angle data.
!  real                alim1   = squared sine of minimum incidence angle.
!  real                alim2   = squared sine of maximum incidence angle.
!  real                trcfold = trace fold (>= 0).
!  real(:)             trace   = next trace of moved out CDP gather.
!  real(:)             sloths  = stacking sloth function used to NMO correct.
!  real(:)             slothi  = interval sloth function.
!  real(:,:)           s       = running sums of various powers of data.
!  double precision(:) scratch = scratch array.
!
!
! PPAVO_SAHCSC:
!  (1) Scales trace to have unity RMS value, if 'ifrst' = 0.
!  (2) Scales trace based on scale factor passed in.
!
!                      b    i    i     b     b
!  call ppavo_sahcsc (trin,nsin,iflv,scale,ifrst)
!
!  integer nsin  = number of samples in input trace.
!  integer iflv  = index of first live trace.
!  integer ifrst = flag denoting if this is first trace.
!  real    scale = scale factor to apply to trace.
!  real(:) trin  = input trace, output scaled trace. 
!
!
! PPAVO_SAHGRAM:
!  (1) Allows a certain predetermined percentage to be clipped, which
!      provides a more robust scaling algorithm.
!
!                       o     o     i      b      b      i     i     i
!  call ppavo_sahgram (igram,hgram,hsize,kpwrks,kpwrkd,nrecs,istrid,ioff,
!                      oh,otr,thl,ns,uscale,ascale,bin,ddp1hd,ddp1tr)
!                      o   o   b  b    i      b     o    b      b
!
!  integer(:)          igram  = data histogram (integer format).
!  real(:)             hgram  = data histogram (real format).
!  integer             hsize  = histogram size.
!  integer             kpwrks = pointer to file containing data.
!  integer             kpwrkd = pointer to file containing data.
!  integer             nrecs  = number sets of traces in work file.
!  integer             istrid = trace stride in the work file.
!  integer             ioff   = offset of traces in the work file.
!  double precision(:) oh     = scratch array for trace headers.
!  real(:)             otr    = scratch array for trace data.
!  integer             thl    = trace header length.
!  integer             ns     = trace length.
!  integer             uscale = percentage of data values to be clipped.
!  integer             ascale = max. abs. value of data
!  integer             bin    = largest bin number to be plotted.
!  real(:)             ddp1tr = scratch trace data array.
!  double precision(:) ddp1hd = scratch trace header array
!
!
! PPAVO_SAMODV:
!  (1) Modifies the velocity traces on a new iteration.
!
!                      b    b  i    i
!  call ppavo_samodv (vels,scr,ns,ivsmth)
!
!  integer ns     = length of all arrays.
!  integer ivsmth = half-length smoothing length.
!  real(:) vels   = previous unsmoothed velocities and modified velocities.
!  real(:) scr    = scratch array.
!
!
! PPAVO_SAMVOT:
!  (1) NMO corrects a trace, using prescribed stacking velocity profile.
!
!                      i     i    b     b     b     b    i      i
!  call ppavo_samvot (trin,sloth,coef,trout,trcoef,ifrc,nsin, ncoef,
!                     fsa,lsa,xoff,iflv,t0,tsamp,tpflg,nflv,nllv)
!                      i   i   i     i  i    i     i    b    b
!
!  integer   nsin   = length of 'trin' array.
!  integer   ncoef  = size of 'trcoef' and 'coef' arrays.
!  integer   fsa    = index in output trace of first sample to be moved out.
!  integer   lsa    = index in output trace of last sample to be moved out.
!  integer   iflv   = index of first live sample of trace.
!  integer   tpflg  = 0 for X-T dara, or 1 for Tau-P data.
!  integer   nflv   = index of first live value after moveout.
!  integer   nllv   = index of last live value after moveout.
!  real      xoff   = shot receiver offset (feet) for X-T data, or
!                     the P value (seconds/feet) for Tau-P data.
!  real      t0     = time of 'trin(1)' (seconds).
!  real      tsamp  = sampling interval (seconds).
!  real(:)   trin   = uncorrected seismic trace, padded with 'ncoef' zeros.
!  real(:)   sloth  = prescribed moveout sloth function.
!  real(:,:) coef   = interpolation coefficients.
!  real(:)   trout  = NMO-corrected output trace.
!  real(:,:) trcoef = work area for fully vectorized trace interp scheme.
!  real(:)   ifrc   = another work array.
!
!
! PPAVO_SANEWV:
!  (1) Computes revised stacking velocities.
!
!                     b   b   i   i  i   i  i  i   i    i     i     i      i 
!  call ppavo_sanewv (dv,vrms,ns,it0,fs,wf0,q,fsa,lsa,miniv,maxiv,minpct,maxpct,
!                     mindv,maxdv,minsv,maxsv,vtime,vpow,status,reft)
!                       i     i     i     i     i    i     o     o
!
!  integer   ns     = number of samplex in 'dz' array.
!  integer   it0    = time of first sample (msec).
!  integer   fsa    = first sample under analysis.
!  integer   lsa    = last sample under analysis.
!  integer   miniv  = minimum allowable interval velocity.
!  integer   maxiv  = maximum allowable interval velocity.
!  integer   minsv  = minimum allowable stacking velocity.
!  integer   maxsv  = maximum allowable stacking velocity.
!  integer   vtime  = velocity cutoff time (msec).
!  integer   vpow   = velocity cutoff sharpness.
!  integer   status = error flag (see subroutine for details).
!  real      fs     = sampling frequency (Hz).
!  real      wf0    = wavelet center frequency (Hz).
!  real      q      = quality factor of the media.
!  real      minpct = minimum allowed % decrement in interval velocities.
!  real      maxpct = maximum allowed % increment in interval velocities.
!  real      mindv  = minimum allowed % decrement in stacking velocities.
!  real      maxdv  = maximum allowed % increment in stacking velocities.
!  real      reft   = time the error occurred (sec).
!  real(:)   dv     = work area for fully vectorized trace interp scheme.
!  real(:)   vrms   = another work array.
!
!
! PPAVO_SARKFT:
!  (1) Generates H1(t) and H2(t) inversion filters for NMO destretching,
!      specifically for a Ricker wavelet.
!
!                     o  o   o     o    o    i  i
!  call ppavo_sarkft (h1,h2,corr,cvect,work,mxh,nh,
!                     w0t,pctwns,pctcns,a,ks,ier)
!                      i    i      i    i i   o
!
!  integer          ier    = error flag.
!  integer          ks     = second dimension of work array.
!  integer          mxh    = dimensioned size of output inversion filters.
!  integer          nh     = actual size of output inversion filters.
!  real             w0t    = omega 0 times the time step size (unitless).
!  real             pctwns = additional white noise percentage.
!  real             pctcns = additional colored noise percentage.
!  real(:)          cvect  = cross-correlation vector between W(T) and TW'(T).
!  real(:)          h1     = inversion filter.
!  real(:)          h2     = inversion filter.
!  real(:)          corr   = auto-correlation of the wavelet.
!  real(:)          work   = work array.
!  double precision a      = auto-regressive noise coefficient.
!
!
! PPAVO_SARUNS:
!  (1) Collects raw data to later compute the RUNS statistic.
!
!                       i   i i  i    b    i   i     i     i   i  i
!  call ppavo_saruns (trace,a,b,slow,runs,nsa,xoff,alim1,alim2,ts,t0)
!
!  integer      nsa   = number of samples under analysis.
!  integer(:,:) runs  = RUNS statistic.
!  real(:)      trace = moved-out pre-stack trace.
!  real(:)      a     = the A trace from a previous run.
!  real(:)      b     = the B trace from a previous run.
!  real(:)      slow  = 1/(stacking velocity)**2.
!  real         xoff  = shot-receiver offset.
!  real         alim1 = squared sine of minimum incidence angle.
!  real         alim2 = squared sine of maximum incidence angle.
!  real         ts    = sampling interval (seconds).
!  real         t0    = time of first sample (seconds).
!
!
! PPAVO_SAUAVGA:
!  (1) Computes running weighted averages.
!
!                                  b    b     b      b     b  b  b  b  b  b
!  call subroutine ppavo_sauavga (asum,bsum,absumr,absumi,wts,ar,ai,br,bi,w,
!               scr1,scr2,ns,fsa,lsa,ntwin,fac,coropt,icdpn,idb,ipr)
!                b    b   i   i   i    i    i    i      i    i   i
!
!  integer          ns     = size of arrays.
!  integer          fsa    = first sample under analysis.
!  integer          lsa    = last sample under analysis.
!  integer          ntwin  = number of samples in time window.
!  integer          coropt = weighting to apply to data.
!  integer          icdpn  = trace number being averaged.
!  integer          idb    = debug level.
!  integer          ipr    = Fortran print unit number.
!  real             fac    = scale factor to apply to running sums.
!  real             scr1   = scratch array #1.
!  real             scr2   = scratch array #1.
!  real             ar     = zero-offset vector Re{A}.
!  real             ai     = zero-offset vector Im{A}.
!  real             br     = gradient vector Re{B}.
!  real             bi     = gradient vector Im{B}.
!  real             w      = given weighting function.
!  double precision asum   = vector sum of |A|**2.
!  double precision bsum   = vector sum of |B|**2.
!  double precision absumr = vector sum of Re{AB*}.
!  double precision absumi = vector sum of Im{AB*}.
!  double precision wts    = vector sum of weights.
!
!
! PPAVO_SAUCLR:
!  (1) Clears and initializes the statistics array.
!
!                       b    i      i       i      i
!  call ppavo_sauclr (stats,nstat,mxocode,sclfct,rmsscl)
!
!  integer nstat   = 1st dimension of statistics array.
!  integer mxocode = 2nd dimension of statistics array.
!  real(:) stats   = statistics array.
!  real    sclfct  = scaling factor.
!  real    rmsscl  = rms scaling factor.
!
!
! PPAVO_SAUCORA:
!  (1) Computes AVO correlation statistics given running sums of AVEL traces.
!
!                                  i    i     i      i     i   o    o
!  call subroutine ppavo_saucora (asum,bsum,absumr,absumi,wts,siga,sigb,
!               corr,cori,temp,ns,fsa,lsa,nsmth,shift,live)
!                o    o    o   i   i   i    i     i    o
!
!  integer             ns     = size of arrays.
!  integer             fsa    = first sample under analysis.
!  integer             lsa    = last sample under analysis.
!  integer             nsmth  = length of smoothing filter.
!  integer             shift  = vertical shift of window (samples).
!  logical             live   = true if correlations are live.
!  real(:)             siga   = standard deviation of A.
!  real(:)             sigb   = standard deviation of B.
!  real(:)             corr   = real part of correlation coefficient.
!  real(:)             cori   = imaginary part of correlation coefficient.
!  real(:)             temp   = scratch array for smoothing.
!  double precision(:) asum   = sum of |A|**2.
!  double precision(:) bsum   = sum of |B|**2.
!  double precision(:) absumr = sum of Re{AB*}.
!  double precision(:) absumi = sum of Im{AB*}.
!  double precision(:) wts    = sum of weights.
!
!
! PPAVO_SAUCORB:
!  (1) Computes AVO correlation statistics given running sums of AVEL traces.
!
!                                  i    i     i      i     i   o    o
!  call subroutine ppavo_saucorb (asum,bsum,absumr,absumi,wts,siga,sigb,
!               corr,cori,coropt,alpha,cormax,ns,shift,live)
!                o    o    o       i      i   i    i    o
!
!  integer             ns     = size of arrays.
!  integer             shift  = vertical shift of window (samples).
!  integer             coropt = correlation option
!  logical             live   = true if correlations are live.
!  real                alpha  = power of normalization
!  real                cormax = maximum allowable correlation magnitude
!  real(:)             siga   = standard deviation of A.
!  real(:)             sigb   = standard deviation of B.
!  real(:)             corr   = real part of correlation coefficient.
!  real(:)             cori   = imaginary part of correlation coefficient.
!  double precision(:) asum   = sum of |A|**2.
!  double precision(:) bsum   = sum of |B|**2.
!  double precision(:) absumr = sum of Re{AB*}.
!  double precision(:) absumi = sum of Im{AB*}.
!  double precision(:) wts    = sum of weights.
!
!
! PPAVO_SAUFDP:
!  (1) Searches for previously encountered CDP.
!
!                         i    i     i
!  integer ppavo_saufdp (cdps,ncdps,icdp)
!
!  integer    icdp  = cdp number to check.
!  integer    ncdps = size of cdp array.
!  integer(:) cdps  = cdp array.
!
!
! PPAVO_SAUHCE:
!  (1) Unknown.
!
!                        i o i    i     i
!  integer ppavo_sauhce (r,p,ns,alpha,dtmin)
!
!  integer ns    = size of arrays.
!  real    alpha = aggressiveness factor.
!  real    dtmin = minimum delta fluid angle (degrees).
!  real(:) r     = real part of the correlation coefficient.
!  real(:) p     = exponentiation array.
!
!
! PPAVO_SAUOVA:
!  (1) Prepares to perform automatic overburden correction, by
!  (2) Computing 'k*', the cross-term leakage factor from b->a.
!  (3) Updating the power in the AVO slope with leakage removed.
!
!                        i  i   i   i  o  o   o    i   o  i
!  integer ppavo_sauova (sa,sb,rrc,ric,kr,ki,sapr,wrr,scr,ns)
!
!  integer ns   = size of arrays.
!  real(:) sa   = sqrt{<|A|**2?}.
!  real(:) sb   = sqrt{<|B|**2?}.
!  real(:) rrc  = re{<AB*>} / {sa*sb}.
!  real(:) ric  = im{<AB*>} / {sa*sb}.
!  real(:) kr   = re{k*}.
!  real(:) ki   = im{k*}.
!  real(:) sapr = standard deviation of A prime.
!  real(:) wrr  = real correlation coefficient.
!  real(:) src  = scratch array.
!
!
! PPAVO_SAUOVB:
!  (1) Prepares to perform automatic overburden correction, by
!  (2) Computing 'k*', the cross-term leakage factor from b->a.
!  (2) Computing velocity correction factor 'dvel'.
!  (3) Updating the power in the AVO slope with leakage removed.
!  (5) Updating the correlation coefficient between 'A' and the
!      modified AVO slope (leakage removed)
!
!                        i  i   i   i   o   o  o   o    b   b  i
!  integer ppavo_sauovb (sa,sb,rrc,ric,dvel,kr,ki,sbpr,wrr,wri,ns)
!
!  integer ns   = size of arrays.
!  real(:) sa   = sqrt{<|A|**2?}.
!  real(:) sb   = sqrt{<|B|**2?}.
!  real(:) rrc  = re{<AB*>} / {sa*sb}.
!  real(:) ric  = im{<AB*>} / {sa*sb}.
!  real(:) dvel = -im{<AB*>} / (sa*sa) = -ric / (sb/sa)
!  real(:) kr   = re{k*}.
!  real(:) ki   = im{k*}.
!  real(:) sbpr = standard deviation of A prime.
!  real(:) wrr  = real correlation coefficient.
!  real(:) wri  = imaginary correlation coefficient.
!     
!
! PPAVO_SAUSKP:
!  (1) Checks if CDP within processing range.
!
!                                 i i  i
!  character(len=1) ppavo_sauskp (i,l1,l2)
!
!  integer i  = integer to compare.
!  integer l1 = lower bound.
!  integer l2 = upper bound.
!
!
! PPAVO_SAUSTC:
!  (1) Computes the ingredients for the mean, standard deviation, and 
!      extreme values, to be printed at the end of the job.
!
!                      i    i   i    b     i     i    o
!  call ppavo_saustc (intr,fsa,lsa,stats,nstat,istat,live)
!
!  real(:)                intr  = input data vector.
!  integer                fsa   = first sample under analysis.
!  integer                lsa   = last sample under analysis.
!  double precision (:,:) stats = statistics array.
!  integer                nstat = size of 'stats' array.
!  integer                istat = specific statistic.
!  logical                live  = cdp number to check.
!
!
! PPAVO_SAUSTP:
!  (1) Computes and prints scaling statistics for the auxiliary
!      traces generated by process 'UHCI'.
!
!                                  i     i     i     i    i     i
!  call subroutine ppavo_saustp (stats,nstat,ntout,oticd,ctab,ctext
!                                mintid, maxtid, mxocode, ipr)
!                                  i       i        i      i
!
!  integer               nstat   = 1st dimension of 'stats' array.
!  integer               ntout   = 2nd dimension of 'stats' array.
!  integer               mintid  = min. index of 'ctab' array.
!  integer               maxtid  = max. index of 'ctab' array.
!  integer               mxocode = size of 'ctext' array.
!  integer               ipr     = print unit number.
!  integer(:)            oticd   = list of external codes.
!  integer(:)            ctab    = external/internal codes correspondence.
!  double precision(:,:) stats   = statistics array.
!  character(len=*)(:)   ctext   = list of internal code descriptors.
!
!
! PPAVO_SAVELC:
!  (1) Converts stacking velocity into stacking sloth, which is V**(-2),
!      and optionally converts stacking sloth into interval sloth using
!      the Dix equation.
!
!                      b     o      o    i    i      i
!  call ppavo_savelc (intr,sloths,slothi,ns,ivsmth,iviflg,
!                     t0,vinc,tsamp,ovrflg,target,ier)
!                     i   i     i     i      i     o
!
!  integer ns     = length of trace in samples.
!  integer ivsmth = length of velocity smoothing filter.
!  integer iviflg = index of new last live sample.
!  integer ovrflg = non-zero if velocities to replace with constants.
!  integer target = target zone (seconds).
!  integer ier    = index of first bad velocity, or 0 if okay.
!  real(:) sloths = computed stacking sloths (sec**2/feet**2).
!  real(:) slothi = computed interval sloths (sec**2/feet**2).
!  real(:) intr   = given stacking velocities (feet/sec).
!  real    t0     = time of first sample (seconds).
!  real    vinc   = velocity increments (feet/sec).
!  real    tsamp  = sampling interval (seconds).
!
!
! PPAVO_SUMAG
!  (1) Computes the squared magnitude of the complex number (Sr + i Si).
!
!                    i   i   i   i    i    o   i
!  call ppavo_sumag (ar, ai, br, bi, mat, out, ns)
!
!      integer   ns  = size of arrays.
!      real(:)   ar  = real part of A vector.
!      real(:)   ai  = imag part of A vector.
!      real(:)   br  = real part of B vector.
!      real(:)   bi  = imag part of B vector.
!      real(:,:) mat = coefficient matrix
!      real(:)   out = squared magnitudes.
!
!
! PPAVO_SWLEV:
!  (1) Calls Herb Swan's modified version (ppavo_hermtp) of Marple's routine.
!
!                    b  i   b  i   b  i   i  b   b
!  call ppavo_swlev (x,incx,u,incu,y,incy,n,aux,naux)
!
!  integer incx = unknown (see subroutine and ppavo_hermtp for details).
!  integer incu = unknown (see subroutine and ppavo_hermtp for details).
!  integer incy = unknown (see subroutine and ppavo_hermtp for details).
!  integer naux = unknown (see subroutine and ppavo_hermtp for details).
!  integer n    = unknown (see subroutine and ppavo_hermtp for details).
!  real(:) x    = unknown (see subroutine and ppavo_hermtp for details).
!  real(:) y    = unknown (see subroutine and ppavo_hermtp for details).
!  real(:) y    = unknown (see subroutine and ppavo_hermtp for details).
!  real(:) aux  = unknown (see subroutine and ppavo_hermtp for details).
!
!
! PPAVO_TIME_CARR:
!  (1) Prints time array.
!
!                          i    b
!  call ppavo_time_carr (idate,when)
!
!  integer           idate = date.
!  character(len=27) when  = when.
!
!
! PPAVO_TRLIVE:
!  (1) Finds first and last live (non-zero) samples in a trace.
!
!                       i   i    b      b
!  call ppavo_trlive (trace,ns,newflv,newllv)
!
!  integer   ns     = number of samples in input trace.
!  integer   newflv = new first live sample.
!  integer   newllv = new last live sample.
!  i,r,dp(:) trace  = input trace.
!
!
! PPAVO_UHCI_READ:
!  (1) Reads data from temporary storage.
!
!                          i       i      b    b  b  i     i     b      b
!  call ppavo_uhci_read (kpwrks,kpwrkd,traceno,tr,hd,lth,nsamp,ddp1tr,ddp1hd)
!
!  integer             kpwrks  = file/memory pointer.
!  integer             kpwrkd  = file/memory pointer.
!  integer             traceno = trace number.
!  integer             lth     = length of trace header.
!  integer             nsamp   = number of trace samples.
!  real(:)             ddp1tr  = temporary trace data.
!  double precision(:) ddp1hd  = temporary trace header.
!  double precision(:) hd      = trace header.
!  real(:)             tr      = trace data.
!
!
! PPAVO_UHCI_WRITE:
!  (1) Writes data to temporary storage.
!
!                           i       i      i    i  i  i     i     b      b
!  call ppavo_uhci_write (kpwrks,kpwrkd,traceno,tr,hd,lth,nsamp,ddp1tr,ddp1hd)
!
!  integer             kpwrks  = file/memory pointer.
!  integer             kpwrkd  = file/memory pointer.
!  integer             traceno = trace number.
!  integer             lth     = length of trace header.
!  integer             nsamp   = number of trace samples.
!  real(:)             ddp1tr  = temporary trace data.
!  double precision(:) ddp1hd  = temporary trace header.
!  double precision(:) hd      = trace header.
!  real(:)             tr      = trace data.
!
!
! PPAVO_VADD:
!  (1) Adds two vectors, outputs another (real only).
!
!                   i   i   i   i   o   i    i
!  call ppavo_vadd (a,astep,b,bstep,c,cstep,nsmp)
!
!  integer astep = input vector step.
!  integer bstep = input vector step.
!  integer cstep = output vector step.
!  integer nsamp = number of samples.
!  real(:) a     = input vector.
!  real(:) b     = input vector.
!  real(:) c     = output vector.
!
!
! PPAVO_VMUL:
!  (1) Multiples two vectors, outputs another (real only).
!
!                   i   i   i   i   o   i    i
!  call ppavo_vmul (a,astep,b,bstep,c,cstep,nsmp)
!
!  integer astep = input vector step.
!  integer bstep = input vector step.
!  integer cstep = output vector step.
!  integer nsamp = number of samples.
!  real(:) a     = input vector.
!  real(:) b     = input vector.
!  real(:) c     = output vector.
!
!
! PPAVO_VNEG:
!  (1) Multiples two vectors, outputs another (real only).
!
!                   i   i   o   i    i
!  call ppavo_vneg (a,astep,b,bstep,nsmp)
!
!  integer astep = input vector step.
!  integer bstep = output vector step.
!  integer nsamp = number of samples.
!  real(:) a     = input vector.
!  real(:) b     = output vector.
!
!
! PPAVO_VSUB:
!  (1) Subtracts two vectors, outputs another (real only).
!
!                   i   i   i   i   o   i    i
!  call ppavo_vsub (a,astep,b,bstep,c,cstep,nsmp)
!
!  integer astep = input vector step.
!  integer bstep = input vector step.
!  integer cstep = output vector step.
!  integer nsamp = number of samples.
!  real(:) a     = input vector.
!  real(:) b     = input vector.
!  real(:) c     = output vector.
!
!
! PPAVO_VDIVZ:
!  (1) Divides two vectors, outputs another (real only, with zero-test).
!
!                    i   i   i   i   o   i    i
!  call ppavo_vdivz (a,astep,b,bstep,c,cstep,nsmp)
!
!  integer astep = input vector step.
!  integer bstep = input vector step.
!  integer cstep = output vector step.
!  integer nsamp = number of samples.
!  real    dval  = default value, if denominator equals zero.
!  real(:) a     = input vector.
!  real(:) b     = input vector.
!  real(:) c     = output vector.
!
!
! PPAVO_VLOGZ:
!  (1) Does vector logarithm (real only, with zero-test).
!
!                    i   i   i o   i    i
!  call ppavo_vlogz (a,astep,c,b,bstep,nsmp)
!
!  integer astep = input vector step.
!  integer bstep = output vector step.
!  integer nsamp = number of samples.
!  real(:) a     = input vector.
!  real(:) b     = output vector.
!  real    c     = constant.
!
!
! PPAVO_VEXP:
!  (1) Does vector exponentiation (real only).
!
!                    i   i  o   i    i
!  call ppavo_vexp (a,astep,b,bstep,nsmp)
!
!  integer astep = input vector step.
!  integer bstep = output vector step.
!  integer nsamp = number of samples.
!  real(:) a     = input vector.
!  real(:) b     = output vector.
!
!
! PPAVO_VSADD:
!  (1) Adds scalar to a vector, outputs another (real only).
!
!                    i   i   i o   i    i
!  call ppavo_vsadd (a,astep,b,c,cstep,nsmp)
!
!  integer astep = input vector step.
!  integer cstep = output vector step.
!  integer nsamp = number of samples.
!  real    b     = scalar.
!  real(:) a     = input vector.
!  real(:) c     = output vector.
!
!
! PPAVO_VSMUL:
!  (1) Multiplies vector by scalar, outputs another (real only).
!
!                    i   i   i o   i    i
!  call ppavo_vsmul (a,astep,b,c,cstep,nsmp)
!
!  integer astep = input vector step.
!  integer cstep = output vector step.
!  integer nsamp = number of samples.
!  real    b     = scalar.
!  real(:) a     = input vector.
!  real(:) c     = output vector.
!
!
! PPAVO_VSMUL:
!  (1) Scalar multiplies and adds two vectors, outputs another (real only).
!
!                   i   i   i      i   i   o   i    i
!  call ppavo_vsma (a,astep,scalar,b,bstep,c,cstep,nsmp)
!
!  integer astep  = input vector step.
!  integer bstep  = input vector step.
!  integer cstep  = output vector step.
!  integer nsamp  = number of samples.
!  real    scalar = scalar.
!  real(:) a      = input vector.
!  real(:) b      = input vector.
!  real(:) c      = output vector.
!
!
! PPAVO_VMOVE:
!  (1) Copies a vector into another (integer, real or double precision).
!
!                      i     i     o     i     i
!  call ppavo_vmove (ivect,istep,ovect,ostep,nsamp)
!
!  integer istep = input vector step.
!  integer ostep = output vector step.
!  integer nsamp = number of samples.
!  i,r,dp(:) ivect = input vector.
!  i,r,dp(:) ovect = output vector.
!
!
! PPAVO_VFILL:
!  (1) Fills a vector with a given value (integer, real or double precision).
!
!                     i    o    i     i
!  call ppavo_vfill (fval,vect,step,nsamp)
!
!  integer   step  = vector step.
!  integer   nsamp = number of samples.
!  i,r,dp    fval  = fill value.
!  i,r,dp(:) vect  = output vector.
!
!
! PPAVO_VZERO:
!  (1) Fills a vector with zero values (integer, real or double precision).
!
!                     o    i     i
!  call ppavo_vzero (vect,step,nsamp)
!
!  integer   step  = vector step.
!  integer   nsamp = number of samples.
!  i,r,dp(:) vect  = output vector.
!
!
! PPAVO_VRVRS:
!  (1) Reverses a vector in place (integer, real or double precision).
!
!                     b    i     i
!  call ppavo_vrvrs (vect,step,nsamp)
!
!  integer   step  = vector step.
!  integer   nsamp = number of samples.
!  i,r,dp(:) vect  = input/output vector.
!
!
! PPAVO_VSQRTZ:
!  (1) Fills a vector with square root values of another vector.
!
!                       i     i     i      o     i     i
!  call ppavo_vsqrtz (ivect,istep,scalar,ovect,ostep,nsamp)
!
!  integer nsamp  = number of samples.
!  integer istep  = input step increment.
!  integer ostep  = output step increment.
!  real    scalar = scalar value.
!  real(:) ivect  = input vector.
!  real(:) ovect  = output vector.
!
!
! PPAVO_CHARS2REAL:
!  (1) Retrieves real values from a character string.
!
!                          i    i    o   o
!  call ppavo_chars2real (cstr,ilen,freq,nf)
!
!  integer ilen = input length of 'cstr'.
!  integer nf   = number of output frequencies.
!  real(:) freq = output frequencies.
!  cstr(:) cstr = character string.
!
!
! PPAVO_SLASH2DASH:
!  (1) Replaces slashes (/) with dashes (-) and returns new length.
!
!                          b    i    o
!  call ppavo_slash2dash (cstr,ilen,olen)
!
!  integer ilen = input length of 'cstr'.
!  integer olen = output length of 'cstr'.
!  character(:) = character string.
!
!
! PPAVO_UPPERCASE:
!  (1) Converts character string to uppercase.
!
!                         b    i
!  call ppavo_uppercase (cstr,clen)
!
!  integer clen = input length of 'cstr'.
!  character(:) = character string.
!
!
! PPAVO_LOWERCASE:
!  (1) Converts character string to lowercase.
!
!                         b    i
!  call ppavo_lowercase (cstr,clen)
!
!  integer clen = input length of 'cstr'.
!  character(:) = character string.
!
!
! PPAVO_STRLEN:
!  (1) Returns number of characters in a string before the first blank,
!      null, or unprintable character.
!
!                      i    i    o
!  call ppavo_strlen (cstr,ilen,olen)
!
!  integer ilen = input length of 'cstr'.
!  integer olen = output length of 'cstr'.
!  character(:) = character string.
!
!
! PPAVO_STRNDX:
!  (1) Returns index of the last non-blank character in a string,
!      or the last character before a null.
!
!                      i    i    o
!  call ppavo_strndx (cstr,ilen,olen)
!
!  integer ilen = input length of 'cstr'.
!  integer olen = output length of 'cstr'.
!  character(:) = character string.
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  7. 2007-12-13  Bill Menger Fixed a temp variable with intent(out) by setting
!                             it to 0.0.
!  6. 2006-06-20  B. Menger   Removed Unused Variables.
!  5. 2005-06-14  Goodger    Change argument corr in subroutine sabnft from
!                            intent out to intent inout to satisfy absoft
!                            9.0 compiler.
!  4. 2005-01-03  B. Lucas   Removed null char from end of ddname string.
!  3. 2004-02-26  B. Lucas   Added error codes to UHCI_READ/WRITE subroutines.
!  2. 2004-01-27  B. Lucas   Modified tempfile generation to get unique name.
!  1. 2003-08-26  B. Lucas   Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module ppavo_module
      use pc_module
      use named_constants_module
      use matfun_module
      use getlun_module
      use cio_module
      use tempname_module
      implicit none

      public :: ppavo_avoe        ! computes residual AVO errors
      public :: ppavo_cnvitt      ! convert rms vels. to int. transit times
      public :: ppavo_dumpvl      ! dumps velocity traces
      public :: ppavo_gamma       !
      public :: ppavo_getfp1      ! computes record pointer for scratch file#1
      public :: ppavo_hermtp      ! solves real linear simultaneous eqns.
      public :: ppavo_idamax      ! finds dp element having largest magnitude
      public :: ppavo_ismax       ! finds maximum value in an array
      public :: ppavo_ismin       ! finds minimum value in an array
      public :: ppavo_maconv      ! convolution operation
      public :: ppavo_magphs      ! computes sq. magnitudes and phases
      public :: ppavo_mahlbt      ! Hilbert transform operation
      public :: ppavo_masmth      ! convolves vector with rectangular filter
      public :: ppavo_matv        ! performs AVO analysis for 1, 2, or 3 terms
      public :: ppavo_maxmgv      ! located max. magnitude element of vector
      public :: ppavo_mcarts      ! converts polar coords into cartesian coords
      public :: ppavo_mcofgn      ! computes interpolation coefficients
      public :: ppavo_mpowr       ! raises pos. real array to real exponent
      public :: ppavo_mwienr      ! finds solutions of 1-channel normal eqns.
      public :: ppavo_nacma       ! complex vector multiply and add
      public :: ppavo_naacms      ! computes sq. mag. of complex vector
      public :: ppavo_nabigs      ! computes imag part of array product
      public :: ppavo_nabrgs      ! computes real part of array product
      public :: ppavo_namags      ! computes magnitude of array elements
      public :: ppavo_namsum      ! computes a sliding moving average
      public :: ppavo_napowi      ! computes integer power of array
      public :: ppavo_napowr      ! computes real power of array
      public :: ppavo_nazero      ! zeros out certain array elements
      public :: ppavo_rmsscl      ! computes RMS scale factor
      public :: ppavo_runsc       ! computes the RUNS statistic
      public :: ppavo_saahci      ! computes optimal hydrocarbon indicators
      public :: ppavo_saarvi      ! computes residual velocity indicators
      public :: ppavo_sabnft      ! builds inversion filters for bandpass
      public :: ppavo_sabwave     ! compute autocorrelation of bandpass wavelet
      public :: ppavo_sahci       ! computes variosu hydrocarbon indicators
      public :: ppavo_sahcic      ! collects run. sums for hydrocarbon detection
      public :: ppavo_sahcsc      ! performs trace scaling
      public :: ppavo_sahgram     ! allows clipping of predetermined percentage
      public :: ppavo_samodv      ! modifies velocity trace on new iteration
      public :: ppavo_samvot      ! normal moveout corrects a trace
      public :: ppavo_sanewv      ! computes revised stacking velocities
      public :: ppavo_sarkft      ! builds inversion filters for Ricker
      public :: ppavo_saruns      ! collects raw data for RUNS statistic
      public :: ppavo_sauavga     ! computes running weighted averages.
      public :: ppavo_sauavgb     ! computes running weighted averages.
      public :: ppavo_sauclr      ! clears and initializes statistics array
      public :: ppavo_saucora     ! computes AVO correlation statistics
      public :: ppavo_saucorb     ! computes AVO correlation statistics
      public :: ppavo_saufdp      ! searches for previous encountered cdp
      public :: ppavo_sauhce      ! unknown
      public :: ppavo_sauova      ! prepares for automatic overburden correction
      public :: ppavo_sauovb      ! prepares for automatic overburden correction
      public :: ppavo_sauskp      ! checks if CDP within processing range
      public :: ppavo_saustc      ! computes/prints various statistics
      public :: ppavo_saustp      ! computes/prints scaling statistics
      public :: ppavo_savelc      ! converts stacking veclotiy into sloth
      public :: ppavo_sumag       ! computes squared magnitudes
      public :: ppavo_swlev       ! calls Her Swan's ppavo_hermtp subroutine
      public :: ppavo_time_carr   ! print time array
      public :: ppavo_trlive      ! find first/last live samples in trace
      public :: ppavo_uhci_read   ! reads data from temporary storage
      public :: ppavo_uhci_write  ! writes data to temporary storage
      public :: ppavo_upawrk      ! allocates temporary direct access dataset
      public :: ppavo_uguwrk      ! deallocates temporary direct access dataset
      public :: ppavo_fowssd      ! write dataset - sequential access
      public :: ppavo_focdd       ! close dataset
      public :: ppavo_fordsd      ! read dataset - direct access
      public :: ppavo_fowdsd      ! write dataset - direct access
      public :: ppavo_vadd        ! adds two vectors
      public :: ppavo_vmul        ! multiples two vectors
      public :: ppavo_vneg        ! returns negative of a vector
      public :: ppavo_vsub        ! subtracts two vectors
      public :: ppavo_vdivz       ! divides two vectors
      public :: ppavo_vlogz       ! does vector logarithm
      public :: ppavo_vexp        ! does vector exponent
      public :: ppavo_vsadd       ! adds scalar to a vector
      public :: ppavo_vsmul       ! multiples vector by scalar
      public :: ppavo_vsma        ! scalar multiples and adds two vectors
      public :: ppavo_vmove       ! copies generic vector to another
      public :: ppavo_vdmove      ! copies dbl. precision vector to another
      public :: ppavo_vrmove      ! copies real vector to another
      public :: ppavo_vimove      ! copies integer vector to another
      public :: ppavo_vfill       ! fills generic vector with value
      public :: ppavo_vdfill      ! fills dbl. precision vector with value
      public :: ppavo_vrfill      ! fills real vector with value
      public :: ppavo_vifill      ! fills integer vector with value
      public :: ppavo_vzero       ! zeros generic vector
      public :: ppavo_vdzero      ! zeros dbl. precision vector
      public :: ppavo_vrzero      ! zeros real vector
      public :: ppavo_vizero      ! zeros integer vector
      public :: ppavo_vrvrs       ! reverses generic vector in place
      public :: ppavo_vdrvrs      ! reverses dbl. precision vector in place
      public :: ppavo_vrrvrs      ! reverses real vector in place
      public :: ppavo_virvrs      ! reverses integer vector in place
      public :: ppavo_vsqrtz      ! fills real vector with sqrt of another
      public :: ppavo_chars2real  ! read real values from a string
      public :: ppavo_slash2dash  ! replace slashes with dashes in a string
      public :: ppavo_uppercase   ! converts string to upper case
      public :: ppavo_lowercase   ! converts string to lower case
      public :: ppavo_strlen      ! finds length of character string
      public :: ppavo_strndx      ! finds length of character string
      public :: ppavo_xtrap       ! extrapolates vector extremities

      character(len=100),public,save :: ppavo_IDENT = &
'$Id: ppavo.f90,v 1.7 2007/12/14 15:05:47 Menger beta sps $'


!!------------------------ generic interfaces -----------------------------!!
!!------------------------ generic interfaces -----------------------------!!
!!------------------------ generic interfaces -----------------------------!!

      interface ppavo_vmove
        module procedure ppavo_vdmove
        module procedure ppavo_vrmove
        module procedure ppavo_vimove
      end interface

      interface ppavo_vfill
        module procedure ppavo_vdfill
        module procedure ppavo_vrfill
        module procedure ppavo_vifill
      end interface

      interface ppavo_vzero
        module procedure ppavo_vdzero
        module procedure ppavo_vrzero
        module procedure ppavo_vizero
      end interface

      interface ppavo_vrvrs
        module procedure ppavo_vdrvrs
        module procedure ppavo_vrrvrs
        module procedure ppavo_virvrs
      end interface


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

      type,public :: ppavo_ptrs
         integer,          pointer, dimension(:) :: iptr
         real,             pointer, dimension(:) :: rptr
         double precision, pointer, dimension(:) :: dptr
         integer                                 :: ihandsave
         integer                                 :: itotalsave
         character(len=256)                      :: ctmpsave
      end type ppavo_ptrs

      type,public :: ppavo_struct              
!!!   sequence                      ! delete this line if not needed.
!      private
      integer            :: i
      integer            :: init
      type(ppavo_ptrs)   :: ptrs(20)
      end type ppavo_struct

!     ppavo ProMAX header entries.
      integer       :: ppavo_nhdrpz    ! number of header slots
      integer       :: ppavo_nullpz    ! null pointer

!     ppavo ProMAX unit types.
      integer       :: ppavo_englishpz ! english units
      integer       :: ppavo_metricpz  ! metric unitz
  
!     ppavo ProMAX yes/no flags.
      integer       :: ppavo_assignedpz     ! assigned
      integer       :: ppavo_unassignedpz   ! unassigned

!     ppavo ProMAX physical primary sort flags.
      integer       :: ppavo_cdppz     ! CDP bin
      integer       :: ppavo_sinpz     ! source index number
      integer       :: ppavo_recslocpz ! receiver surface location
      integer       :: ppavo_offsetpz  ! offset
      integer       :: ppavo_chanpz    ! channel number
      integer       :: ppavo_unknownpz ! none of the above
      integer       :: ppavo_ilinepz   ! 3d inline number
      integer       :: ppavo_xlinepz   ! 3d crossline number

!     ppavo ProMAX primary data types.
      integer       :: ppavo_normalpz     ! normal unstacked data
      integer       :: ppavo_stackedpz    ! normal stacked data
      integer       :: ppavo_us_transpz   ! transformed unstacked data
      integer       :: ppavo_st_transpz   ! transformed stacked data

!     ppavo ProMAX domain types.
      integer       :: ppavo_txpz       ! normal time-space domain
      integer       :: ppavo_fxpz       ! frequency-space domain
      integer       :: ppavo_ftpz       ! frequency-time domain
      integer       :: ppavo_fkpz       ! frequency-wavenumber domain
      integer       :: ppavo_tauppz     ! tau-p domain
      integer       :: ppavo_sembpz     ! semblance domain
      integer       :: ppavo_mbtpz      ! model-based transform
      integer       :: ppavo_depthpz    ! depth-space domain
      integer       :: ppavo_tslicepz    ! time-slice domain
      integer       :: ppavo_flexedpz    ! flex bin data after duplication

!     ppavo ProMAX last-trace-in-ensemble flag.
      integer       :: ppavo_nlastpz     ! last trace in ensemble
      integer       :: ppavo_lasttrpz    ! not last trace in emsemble

!     ppavo ProMAX data trace types.
      integer       :: ppavo_auxpz        ! general auxiliary trace
      integer       :: ppavo_livepz       ! live trace
      integer       :: ppavo_deadpz       ! dead trace
      integer       :: ppavo_dummypz      ! dummy trace
      integer       :: ppavo_tbreakpz     ! time-break trace
      integer       :: ppavo_upholepz     ! uphole trace
      integer       :: ppavo_sweeppz      ! sweep trace
      integer       :: ppavo_timingpz     ! timing trace
      integer       :: ppavo_wbreakpz     ! water-break trace
      integer       :: ppavo_otherpz      ! any other trace
      integer       :: ppavo_wlogpz       ! well log trace
      integer       :: ppavo_corruptedpz  ! corrupted header

!     ppavo ProMAX tool types.
      integer       :: ppavo_simplepz     ! simple (trace in, trace out)
      integer       :: ppavo_ensemblepz   ! ensemble
      integer       :: ppavo_complexpz    ! complex

!     ppavo ProMAX trace header indices.
      integer       :: ppavo_trace_noz    ! header index of trace number
      integer       :: ppavo_head_mutez   ! header index of top mute
      integer       :: ppavo_tail_mutez   ! header index of bottom mute
      integer       :: ppavo_cdpz         ! header index of CDP
      integer       :: ppavo_seq_noz      ! header index of sequence number
      integer       :: ppavo_trc_foldz    ! header index of trace fold
      integer       :: ppavo_offsetz      ! header index of offset
      integer       :: ppavo_iline_noz    ! header index of inline number
      integer       :: ppavo_xline_noz    ! header index of crossline number
      integer       :: ppavo_end_ensz     ! header index of end of gather
      integer       :: ppavo_end_jobz     ! header index of end of job
      integer       :: ppavo_full_sz      ! header index of full start time
      integer       :: ppavo_full_ez      ! header index of full end time
      integer       :: ppavo_live_sz      ! header index of live val. start time
      integer       :: ppavo_live_ez      ! header index of live val. end time
      integer       :: ppavo_wb_timez     ! header index of waterbottom time
      integer       :: ppavo_line_noz     ! header index of line number
      integer       :: ppavo_amp_normz    ! header index of unknown
      integer       :: ppavo_lseg_endz    ! header index of
      integer       :: ppavo_lseg_seqz    ! header index of
      integer       :: ppavo_sort_runz    ! header index of
      integer       :: ppavo_repeatz      ! header index of
      integer       :: ppavo_purgez       ! header index of purge flag  
      integer       :: ppavo_lagz         ! header index of lag array
      integer       :: ppavo_subz         ! header index of

      integer       :: ppavo_trc_typez    ! header index of trace type
      integer       :: ppavo_gath_typez   ! header index of gather type
      integer       :: ppavo_stackwordz   ! header index of stack word
      integer       :: ppavo_avo_anglez   ! header index of AVO central angle
      integer       :: ppavo_dom_freqz    ! header index of dominant freq.
      integer       :: ppavo_userz        ! header index of user-defined

!     ppavo AVEL data trace types.
      integer       :: ppavo_velpz 
      integer       :: ppavo_atrpz       
      integer       :: ppavo_btrpz        
      integer       :: ppavo_hcitrpz      
      integer       :: ppavo_rvitrpz      
      integer       :: ppavo_residpz      
      integer       :: ppavo_aresidpz 
      integer       :: ppavo_bresidpz
      integer       :: ppavo_runstrpz
      integer       :: ppavo_lensaved

!     ppavo B_UHCI codes.
      integer       :: ppavo_b_maxcode
      integer       :: ppavo_b_mxocode
      integer       :: ppavo_b_jtkill
      integer       :: ppavo_b_jtlive
      integer       :: ppavo_b_jtvel
      integer       :: ppavo_b_jta
      integer       :: ppavo_b_jtb
      integer       :: ppavo_b_jtaq
      integer       :: ppavo_b_jtbq
      integer       :: ppavo_b_jtsa
      integer       :: ppavo_b_jtsb
      integer       :: ppavo_b_jtrc
      integer       :: ppavo_b_jtic
      integer       :: ppavo_b_jthci
      integer       :: ppavo_b_jtrvi
      integer       :: ppavo_b_jtano
      integer       :: ppavo_b_jtbno
      integer       :: ppavo_b_jtdv
      integer       :: ppavo_b_jtitt
      integer       :: ppavo_b_ktkill
      integer       :: ppavo_b_ktvel
      integer       :: ppavo_b_kta
      integer       :: ppavo_b_ktb
      integer       :: ppavo_b_ktaq
      integer       :: ppavo_b_ktbq
      integer       :: ppavo_b_ktsa
      integer       :: ppavo_b_ktsb
      integer       :: ppavo_b_ktrc
      integer       :: ppavo_b_ktic
      integer       :: ppavo_b_kthci
      integer       :: ppavo_b_ktrvi
      integer       :: ppavo_b_ktano
      integer       :: ppavo_b_ktbno
      integer       :: ppavo_b_ktdv
      integer       :: ppavo_b_ktitt

!     ppavo C_UHCI codes.
      integer       :: ppavo_c_maxcode
      integer       :: ppavo_c_mxocode
      integer       :: ppavo_c_jtkill
      integer       :: ppavo_c_jtlive
      integer       :: ppavo_c_jtvel
      integer       :: ppavo_c_jta
      integer       :: ppavo_c_jtb
      integer       :: ppavo_c_jtaq
      integer       :: ppavo_c_jtbq
      integer       :: ppavo_c_jtsa
      integer       :: ppavo_c_jtsb
      integer       :: ppavo_c_jtrc
      integer       :: ppavo_c_jtic
      integer       :: ppavo_c_jthci
      integer       :: ppavo_c_jtrvi
      integer       :: ppavo_c_jtano
      integer       :: ppavo_c_jtbno
      integer       :: ppavo_c_jtdv
      integer       :: ppavo_c_jtitt
      integer       :: ppavo_c_ktkill
      integer       :: ppavo_c_ktvel
      integer       :: ppavo_c_kta
      integer       :: ppavo_c_ktb
      integer       :: ppavo_c_ktaq
      integer       :: ppavo_c_ktbq
      integer       :: ppavo_c_ktsa
      integer       :: ppavo_c_ktsb
      integer       :: ppavo_c_ktrc
      integer       :: ppavo_c_ktic
      integer       :: ppavo_c_kthci
      integer       :: ppavo_c_ktrvi
      integer       :: ppavo_c_ktano
      integer       :: ppavo_c_ktbno
      integer       :: ppavo_c_ktdv
      integer       :: ppavo_c_ktitt

      integer       :: ppavo_AVO_NStd
      integer       :: ppavo_AVO_Ar
      integer       :: ppavo_AVO_Br
      integer       :: ppavo_AVO_Ai
      integer       :: ppavo_AVO_Bi
      integer       :: ppavo_AVO_Sa
      integer       :: ppavo_AVO_Sb
      integer       :: ppavo_AVO_Rr
      integer       :: ppavo_AVO_Ri

      integer       :: ppavo_HVO_Live
      integer       :: ppavo_HVO_K
      integer       :: ppavo_HVO_Ar
      integer       :: ppavo_HVO_Ai
      integer       :: ppavo_HVO_Br
      integer       :: ppavo_HVO_Bi
      integer       :: ppavo_HVO_Sa
      integer       :: ppavo_HVO_Sb
      integer       :: ppavo_HVO_Rr
      integer       :: ppavo_HVO_Ri
      integer       :: ppavo_HVO_Hc
      integer       :: ppavo_HVO_Rv
      integer       :: ppavo_HVO_Pc
      integer       :: ppavo_HVO_PcB
      integer       :: ppavo_HVO_Vs
      integer       :: ppavo_HVO_Th
      integer       :: ppavo_HVO_Ph

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      parameter (ppavo_nhdrpz      = 64)
      parameter (ppavo_nullpz      = 0)

      parameter (ppavo_englishpz   = 1)
      parameter (ppavo_metricpz    = 2)

      parameter (ppavo_cdppz       = 1)
      parameter (ppavo_sinpz       = 2)
      parameter (ppavo_recslocpz   = 3)
      parameter (ppavo_offsetpz    = 4)
      parameter (ppavo_chanpz      = 5)
      parameter (ppavo_unknownpz   = 6)
      parameter (ppavo_ilinepz     = 7)
      parameter (ppavo_xlinepz     = 8)

      parameter (ppavo_normalpz    = 1)
      parameter (ppavo_stackedpz   = 2)
      parameter (ppavo_us_transpz  = 3)
      parameter (ppavo_st_transpz  = 4)

      parameter (ppavo_txpz        = 0)
      parameter (ppavo_fxpz        = 1)
      parameter (ppavo_ftpz        = 2)
      parameter (ppavo_fkpz        = 3)
      parameter (ppavo_tauppz      = 4)
      parameter (ppavo_sembpz      = 5)
      parameter (ppavo_mbtpz       = 6)
      parameter (ppavo_depthpz     = 7)
      parameter (ppavo_tslicepz     = 8)
      parameter (ppavo_flexedpz     = 9)

      parameter (ppavo_nlastpz      = 0)
      parameter (ppavo_lasttrpz     = 1) 
     
      parameter (ppavo_auxpz       = 0)
      parameter (ppavo_livepz      = 1)
      parameter (ppavo_deadpz      = 2)
      parameter (ppavo_dummypz     = 3)
      parameter (ppavo_tbreakpz    = 4)
      parameter (ppavo_upholepz    = 5)
      parameter (ppavo_sweeppz     = 6)
      parameter (ppavo_timingpz    = 7)
      parameter (ppavo_wbreakpz    = 8)
      parameter (ppavo_otherpz     = 9)
      parameter (ppavo_wlogpz      = 10)
      parameter (ppavo_corruptedpz = 11)

      parameter (ppavo_simplepz    = 1)
      parameter (ppavo_ensemblepz  = 2)
      parameter (ppavo_complexpz   = 3)

      parameter (ppavo_trace_noz   = HDR_SEQUENCE)
      parameter (ppavo_head_mutez  = HDR_TOP_MUTE)
      parameter (ppavo_tail_mutez  = HDR_BOTTOM_MUTE)
      parameter (ppavo_cdpz        = HDR_CURRENT_GROUP)
      parameter (ppavo_seq_noz     = HDR_CURRENT_CHANNEL)
      parameter (ppavo_trc_foldz   = HDR_FOLD)
      parameter (ppavo_offsetz     = HDR_OFFSET)
      parameter (ppavo_xline_noz   = HDR_MIDPOINT_XGRID)
      parameter (ppavo_iline_noz   = HDR_MIDPOINT_YGRID)
      parameter (ppavo_end_ensz    = 9)
      parameter (ppavo_end_jobz    = 10)
      parameter (ppavo_full_sz     = 11)
      parameter (ppavo_full_ez     = 12)
      parameter (ppavo_live_sz     = 13)
      parameter (ppavo_live_ez     = 14)
      parameter (ppavo_wb_timez    = 15)
      parameter (ppavo_line_noz    = 16)
      parameter (ppavo_amp_normz   = 17)
      parameter (ppavo_lseg_endz   = 18)
      parameter (ppavo_lseg_seqz   = 19)
      parameter (ppavo_sort_runz   = 20)
      parameter (ppavo_repeatz     = 21)
      parameter (ppavo_purgez      = 22)
      parameter (ppavo_lagz        = 23)
      parameter (ppavo_subz        = 24)

      parameter (ppavo_trc_typez   = HDR_USER_49)
      parameter (ppavo_gath_typez  = HDR_USER_50)  
      parameter (ppavo_stackwordz  = HDR_USER_51)
      parameter (ppavo_avo_anglez  = HDR_USER_52)
      parameter (ppavo_dom_freqz   = HDR_USER_53)
      parameter (ppavo_userz       = HDR_USER_55)

      parameter (ppavo_velpz       = 51)
      parameter (ppavo_atrpz       = 43)
      parameter (ppavo_btrpz       = 44)
      parameter (ppavo_hcitrpz     = 42)
      parameter (ppavo_rvitrpz     = 46)
      parameter (ppavo_residpz     = 60)
      parameter (ppavo_aresidpz    = 61)
      parameter (ppavo_bresidpz    = 62)
      parameter (ppavo_runstrpz    = 55)
      parameter (ppavo_lensaved    = 488)

!     ppavo B_UHCI codes.
      parameter (ppavo_b_maxcode = 14)
      parameter (ppavo_b_mxocode = 14)
      parameter (ppavo_b_jtkill  = 2 )
      parameter (ppavo_b_jtlive  = 1 )
      parameter (ppavo_b_jtvel   = 51 )
      parameter (ppavo_b_jta     = 43 )
      parameter (ppavo_b_jtb     = 44 )
      parameter (ppavo_b_jtaq    = 17 )
      parameter (ppavo_b_jtbq    = 18 )
      parameter (ppavo_b_jtsa    = 45 )
      parameter (ppavo_b_jtsb    = 47 )
      parameter (ppavo_b_jtrc    = 48 )
      parameter (ppavo_b_jtic    = 41 )
      parameter (ppavo_b_jthci   = 42 )
      parameter (ppavo_b_jtrvi   = 46 )
      parameter (ppavo_b_jtano   = 49 )
      parameter (ppavo_b_jtbno   = 50 )
      parameter (ppavo_b_jtdv    = 51 )
      parameter (ppavo_b_jtitt   = 13 )
      parameter (ppavo_b_ktkill  = 0 )
      parameter (ppavo_b_ktvel   = 51 )
      parameter (ppavo_b_kta     = 01 )
      parameter (ppavo_b_ktb     = 02 )
      parameter (ppavo_b_ktaq    = 03 )
      parameter (ppavo_b_ktbq    = 04 )
      parameter (ppavo_b_ktsa    = 05 )
      parameter (ppavo_b_ktsb    = 06 )
      parameter (ppavo_b_ktrc    = 07 )
      parameter (ppavo_b_ktic    = 08 )
      parameter (ppavo_b_kthci   = 09 )
      parameter (ppavo_b_ktrvi   = 10 )
      parameter (ppavo_b_ktano   = 11 )
      parameter (ppavo_b_ktbno   = 12 )
      parameter (ppavo_b_ktdv    = 13 )
      parameter (ppavo_b_ktitt   = 14 )

!     ppavo C_UHCI codes.
      parameter (ppavo_c_maxcode = 9)
      parameter (ppavo_c_mxocode = 9)
      parameter (ppavo_c_jtkill  = 2 )
      parameter (ppavo_c_jtlive  = 1 )
      parameter (ppavo_c_jtvel   = 51 )
      parameter (ppavo_c_jta     = 43 )
      parameter (ppavo_c_jtb     = 44 )
      parameter (ppavo_c_jtaq    = 17 )
      parameter (ppavo_c_jtbq    = 18 )
      parameter (ppavo_c_jtsa    = 45 )
      parameter (ppavo_c_jtsb    = 47 )
      parameter (ppavo_c_jtrc    = 48 )
      parameter (ppavo_c_jtic    = 41 )

      parameter (ppavo_c_ktkill  = 0 )
      parameter (ppavo_c_kta     = 1 )
      parameter (ppavo_c_ktb     = 2 )
      parameter (ppavo_c_ktaq    = 3 )
      parameter (ppavo_c_ktbq    = 4 )
      parameter (ppavo_c_ktsa    = 5 )
      parameter (ppavo_c_ktsb    = 6 )
      parameter (ppavo_c_ktrc    = 7 )
      parameter (ppavo_c_ktic    = 8 )
      parameter (ppavo_c_ktdv    = 9 )

      parameter (ppavo_AVO_NStd  = 8)
      parameter (ppavo_AVO_Ar    = 1)
      parameter (ppavo_AVO_Br    = 2)
      parameter (ppavo_AVO_Ai    = 3)
      parameter (ppavo_AVO_Bi    = 4)
      parameter (ppavo_AVO_Sa    = 5)
      parameter (ppavo_AVO_Sb    = 6)
      parameter (ppavo_AVO_Rr    = 7)
      parameter (ppavo_AVO_Ri    = 8)

      parameter (ppavo_HVO_Live  = 1)
      parameter (ppavo_HVO_K     = 2)
      parameter (ppavo_HVO_Ar    = 43)
      parameter (ppavo_HVO_Ai    = 17)
      parameter (ppavo_HVO_Br    = 44)
      parameter (ppavo_HVO_Bi    = 18)
      parameter (ppavo_HVO_Sa    = 45)
      parameter (ppavo_HVO_Sb    = 47)
      parameter (ppavo_HVO_Rr    = 48)
      parameter (ppavo_HVO_Ri    = 41)
      parameter (ppavo_HVO_Hc    = 42)
      parameter (ppavo_HVO_Rv    = 46)
      parameter (ppavo_HVO_Pc    = 20)
      parameter (ppavo_HVO_PcB   = 21)
      parameter (ppavo_HVO_Vs    = 51)
      parameter (ppavo_HVO_Th    = 52)
      parameter (ppavo_HVO_Ph    = 53)

      contains


!!------------------------------ ppavo_avoe ----------------------------------!!
!!------------------------------ ppavo_avoe ----------------------------------!!
!!------------------------------ ppavo_avoe ----------------------------------!!

!     Purpose:
!        This subroutine computes AVO residual errors.

!     Arguments:
!        S     : Collected weighted sums.
!                 Input:  S(1,1) = S(1,SY)   = SUM { D(X) }
!                         S(1,2) = S(1,SX2Y) = SUM { D(X) * X**2 }
!                         S(1,3) = S(1,SX4Y) = SUM { D(X) * X**4 }
!                         S(1,4) = S(1,SX0)  = SUM { X**0 }
!                         S(1,5) = S(1,SX2)  = SUM { X**2 }
!                         S(1,6) = S(1,SX4)  = SUM { X**4 }
!                         S(1,7) = S(1,SX6)  = SUM { X**6 }
!                         S(1,8) = S(1,SX8)  = SUM { X**8 }
!        OUT   : Output data.
!                 OUT(1, AOUT) : The first HCI array.
!                 OUT(1, BOUT) : The second HCI array.
!                 [0<ERFLG<4] * OUT(1, RMS)  : The AVO residual array.
!                 [1<ERFLG<4] * OUT(1, AN)   : The "A" term error array.
!                 [1<ERFLG<4] * OUT(1, BN)   : The "B" term error array.
!                 Note: If ERFLG[PARM(15)] = 0, the starred subarrays are
!                 not used, and need not be allocated.
!        N     : Dimensioned sizes of the S and OUT arrays.
!        FIRST : First sample to analyze.
!        LAST  : Last sample to analyze.
!        ISZ   : Number of stages.
!        NO    : Number of outputs to generate.
!        NSTG  : Calculated stages.

      subroutine ppavo_avoe (s, out, n, first, last, isz, no, nstg)

      integer                , intent(in)    :: n
      integer                , intent(in)    :: first
      integer                , intent(in)    :: last
      integer                , intent(in)    :: isz
      integer                , intent(in)    :: no
      integer                , intent(in)    :: nstg
      real, dimension(n, isz), intent(inout) :: s
      real, dimension(n, no) , intent(inout) :: out

      integer :: i, istg, sy2, rms, nsa
      real(kind=8)  :: sum, value

      parameter (rms = 3, sy2 = 9)

      if (no .le. 2) return
      nsa = last - first + 1

!     Compute the AVO residual error.
      do 100 i = first, last
         sum = 0
         do 50 istg = 1, nstg
            sum = sum + out(i, istg) * s(i, istg)
50       continue
         value = s(i, sy2) - sum
         if (value .lt. 0) value = 0
         out(i, rms) = value
100   continue

!     Compute the estimate RMS errors in A and B.
      do 150 istg = 1, nstg
         call ppavo_vrmove(out(first, rms), 1, s(last, istg), 1, nsa)
150   continue

      return
      end subroutine ppavo_avoe


!!------------------------------ ppavo_cnvitt --------------------------------!!
!!------------------------------ ppavo_cntitt --------------------------------!!
!!------------------------------ ppavo_cnvitt --------------------------------!!

!     Purpose:
!        This subroutine converts RMS velocities to interval transit times.

!     Arguments:
!        INTR : Input trace.
!        OTR  : Output trace.
!        NS   : Size of input and output traces.
!        ISI  : Sampling interval (usecs).
!        T0   : Unknown.

      subroutine ppavo_cnvitt (intr, otr, ns, isi, t0)

      integer            , intent(in)  :: ns
      integer            , intent(in)  :: isi
      real               , intent(in)  :: t0
      real, dimension(ns), intent(in)  :: intr
      real, dimension(ns), intent(out) :: otr

      integer :: i
      double precision :: t1, t2, period
      double precision :: vr1, vr2, vi2

!     Convert to interval velocities.
      period = isi * 1e-6
      if (period .le. 0) return
      vr2 = dble(intr(1))**2
      do 100 i=2, ns
         t1  = t0 + period*(i-1)
         t2  = t1 - period
         vr1 = dble(intr(i))**2
         vi2 = (t1*vr1 - t2*vr2) / period
         if (vi2 .le. 0) vi2 = 0
         vi2 = dsqrt(vi2)
         vr2 = vr1
         otr(i) = vi2
100   continue
      otr(1) = otr(2)

      return
      end subroutine ppavo_cnvitt


!!------------------------------ ppavo_dumpvl --------------------------------!!
!!------------------------------ ppavo_dumpvl --------------------------------!!
!!------------------------------ ppavo_dumpvl --------------------------------!!

!     Purpose:
!        This subroutine converts RMS velocities to interval transit times.

!     Arguments:
!        DUNIT : Fortrain i/o unit number.
!        VEL   : Stacking velocity trace.
!        NS    : Size of velocity trace.
!        FSTD  : First sample to dump.
!        LSTD  : Last sample to dump.
!        INC   : Increment between samples.
!        ICDPN : This CDP number.
!        ISI   : Sampling interval (usecs).
!        BULK  : Time of first samples (msec).

      subroutine ppavo_dumpvl (dunit, vel, ns, fstd, lstd, inc,&
     &                        icdpn, isi, bulk)

      integer            , intent(in) :: dunit
      integer            , intent(in) :: ns
      integer            , intent(in) :: fstd
      integer            , intent(in) :: lstd
      integer            , intent(in) :: inc
      integer            , intent(in) :: icdpn
      integer            , intent(in) :: isi
      integer            , intent(in) :: bulk
      real, dimension(ns), intent(in) :: vel

!     Declare the local variables.
      integer nstd, ncard, nspc, nslc, nstc, fstc
      integer ddt, datum, tinc, vndx, i, card
      parameter (nspc=6, ddt=1000, datum=0, tinc=0)
      integer iv(nspc), it(nspc)

!     Scope the job out.
      nstd  = (lstd - fstd - 1) / inc + 1
      ncard = (nstd - 1) / nspc + 1 
      nslc  = nstd - (ncard - 1)*nspc
      nstc  = nspc
      fstc  = fstd

!     Send the first card, which is special.

!     Send the remainder of the cards
      do 100 card = 1, ncard
         if (card .eq. ncard) nstc = nslc
         do 120 i = 1, nstc
            vndx = (i-1)*inc + fstc
            iv(i) = nint(vel(vndx))
            it(i) = isi*(vndx-1)/1000 + bulk
120      continue
         write (dunit, 1100) icdpn, (it(i), iv(i), i=1,nstc)
1100     format('VELF   TVR', 13i9)
         fstc = vndx + inc
100   continue     

      return
      end subroutine ppavo_dumpvl


!!------------------------------ ppavo_gamma ---------------------------------!!
!!------------------------------ ppavo_gamma ---------------------------------!!
!!------------------------------ ppavo_gamma ---------------------------------!!

!     Purpose:
!        This function .

!     Arguments:
!        X    : Unknown.

      double precision function ppavo_gamma (x)

      double precision, intent(in) :: x

      double precision :: value

!LOOKAGAIN
      ppavo_gamma = x
      call ppavo_crou_gamma(x, value)
      ppavo_gamma = value

      end function ppavo_gamma


!!----------------------------- ppavo_getfp1 ---------------------------------!!
!!----------------------------- ppavo_getfp1 ---------------------------------!!
!!----------------------------- ppavo_getfp1 ---------------------------------!!

!     Purpose:
!        This function computes the record pointer for scratch file #1.

!     Arguments:
!        N    : Number of elements in 'X' array.
!        TYPE : Element for which to compute record pointer.
!        MPLX : Multiplex option.
!        CWIN : CDP correlation window.
!        MXCD : Max. output codes.

      integer function ppavo_getfp1 (n, rec, mplx, cwin, mxcd)

      integer, intent(in) :: n
      integer, intent(in) :: rec
      integer, intent(in) :: mplx
      integer, intent(in) :: cwin
      integer, intent(in) :: mxcd

      if(mplx .ge. 2) then
         ppavo_getfp1 = (mod(n, cwin+1) * mxcd) + rec
      else
         ppavo_getfp1 = (mxcd*n) + rec
      end if
      return

      end function ppavo_getfp1

  
!!----------------------------- ppavo_hermtp ---------------------------------!!
!!----------------------------- ppavo_hermtp ---------------------------------!!
!!----------------------------- ppavo_hermtp ---------------------------------!!

!     Purpose:
!        This subroutine solves the set of real linear simultaneous equations
!                                TX = M
!        by a variation of the Levinson algorithm. T is a complex M+1
!        by M+1 Hermitian Toeplitz matrix, Z is the known right-hand
!        side real column vector of M+1 elements, and X is the
!        solution vector of M+1 complex elements.

!     Notes:
!        External array T must be dimensioned >= M and arrays X,Z must be
!        dimensioned >= M+1 in the calling program. External array A must
!        be dimensioned >= M.

!     Arguments:
!        M     : Order of matrix T (integer).
!        T0    : Scalar corresponding to real matrix element T(0).
!        T     : Array of N real matrix elements T(1),...,T(m)
!                from the left column of the Toeplitz matrix.
!        Z     : Array of N+1 real elements of the right-hand side
!                vector. Program element Z(K+1) corresponds to text
!                element Z(K), for K=0 to K=m.
!        X     : Array of M+1 real elements of the solution vector.
!                Program element X(K+1) correspeconds to text element
!                X(K), for K=0 to K=M.
!        A     : Scratch array of dimension M.
!        ISTAT : Integer status indicator at time of exit.
!                0 for normal exit.
!                1 if P=0 (singular matrix).

      subroutine ppavo_hermtp (m, t0, t, z, x, a, istat)

      integer               , intent(in)          :: m
      real                  , intent(in)          :: t0
      real,   dimension(m)  , intent(in)          :: t
      real,   dimension(m+1), intent(in)          :: z
      real(kind=8), dimension(m+1), intent(inout) :: x
      real(kind=8), dimension(m)  , intent(inout) :: a
      integer               , intent(inout)       :: istat
      
      integer :: j
      integer :: k
      integer :: kj
      integer :: khalf
      real(kind=8)  :: p
      real(kind=8)  :: temp
      real(kind=8)  :: saved
      real(kind=8)  :: alpha
      real(kind=8)  :: beta

      p = t0
      istat = 1
      if (p .eq. 0.0) return

!     Handle M=0 as a special case.
      x(1) = z(1) / t0
      if (m .le. 0) return

!     Main recursion.
      k = 0
5     k = k+1
      saved = t(k)
      beta = x(1) * t(k)      
      if (k .eq. 1) goto 20
      do 10 j = 1, k-1
         saved = saved + a(j) * t(k-j)
         beta  = beta + x(j+1) * t(k-j)
10    continue

20    temp = -saved/p
      p = p * (1.0 - temp**2)
      if (p .le. 0.0) return

30    a(k) = temp
      alpha = (z(k+1) - beta) / p
      if (k .eq. 1) goto 50
      khalf = k / 2
      do 40 j = 1, khalf
         kj = k - j
         saved = a(j)
         a(j) = saved + temp * a(kj)
         if (j .eq. kj) goto 40
         a(kj) = a(kj) + temp * saved
40    continue
  
50    x(k+1) = alpha
      do 60 j = 1, k
         x(j) = x(j) + alpha * a(k-j+1)
60    continue

      if (k .lt. m) goto 5
      istat = 0

      return
      end subroutine ppavo_hermtp


!!----------------------------- ppavo_idamax ---------------------------------!!
!!----------------------------- ppavo_idamax ---------------------------------!!
!!----------------------------- ppavo_idamax ---------------------------------!!

!     Purpose:
!        This function returns the position of the first or last occurrence
!        of the double precision vector element having the largest magnitude.

!     Arguments:
!        N   : Number of elements in 'X' array.
!        X   : Data array.
!        INC : Stride to use with 'X' array.

      integer function ppavo_idamax (n, x, inc)

      integer,                            intent(in) :: n
      integer,                            intent(in) :: inc
      double precision, dimension(inc,n), intent(in) :: x

      integer :: i, ndx
      real    :: maxf

      ndx  = 1
      maxf = 0d0
      if (inc .gt. 0) then
         do 100 i=1, n
            if (dabs(x(1,i)) .gt. maxf) then
               ndx = i
               maxf = dabs(x(1,ndx))
            endif
100      continue
      else if (inc .lt. 0) then
         do 200 i=n, -1
            if (dabs(x(1,i)) .gt. maxf) then
               ndx = i
               maxf = dabs(x(1,ndx))
            endif
200      continue
      endif
      ppavo_idamax = ndx

      return
      end function ppavo_idamax


!!----------------------------- ppavo_ismax ----------------------------------!!
!!----------------------------- ppavo_ismax ----------------------------------!!
!!----------------------------- ppavo_ismax ----------------------------------!!

!     Purpose:
!        This function finds the maximum value in an array.

!     Arguments:
!        N    : Number of elements in 'A' array.
!        A    : Data array.
!        NINC : Stride to use with 'A' array.

      integer function ppavo_ismax (n, a, ninc)

      integer           , intent(in) :: n
      integer           , intent(in) :: ninc
      real, dimension(n), intent(in) :: a

      integer :: i, ix
      real    :: x

      ix = 1
      if (n .le. 1) goto 20
      x = a(1)

      do 10 i = 1, n, ninc
         if (a(i).le.x) goto 10
         x = a(i)
         ix = i
10    continue

20    ppavo_ismax = ix

      return
      end function ppavo_ismax


!!----------------------------- ppavo_ismin ----------------------------------!!
!!----------------------------- ppavo_ismin ----------------------------------!!
!!----------------------------- ppavo_ismin ----------------------------------!!

!     Purpose:
!        This function finds the minimum value in an array.

!     Arguments:
!        N    : Number of elements in 'A' array.
!        A    : Data array.
!        NINC : Stride to use with 'A' array.

      integer function ppavo_ismin (n, a, ninc)

      integer           , intent(in) :: n
      integer           , intent(in) :: ninc
      real, dimension(n), intent(in) :: a

      integer :: i, ix
      real    :: x

      ix = 1
      if (n .le. 1) goto 20
      x = a(1)

      do 10 i = 1, n, ninc
         if (a(i) .ge. x) goto 10
         x = a(i)
         ix = i
10    continue

20    ppavo_ismin = ix

      return
      end function ppavo_ismin


!!----------------------------- ppavo_maconv ---------------------------------!!
!!----------------------------- ppavo_maconv ---------------------------------!!
!!----------------------------- ppavo_maconv ---------------------------------!!

!     Purpose:
!        This subroutine generalizes the SPARC routine 'ARCON' by providing the
!        argument NOFF. It also features enhanced efficiency, since it
!        vectorizes on the I loop of dimension NAB, rather than the J loop
!        of dimension NF. Since NAB is usually greater than NF, this
!        re-ordering results in increased execution speed.

!     Mathematical description:
!
!                 NF
!                 __
!                 \
!        B(I)  =   > F(J) A(I-J+|NOFF|)   ,     FOR I=1,2,...,NAB
!                 /_
!                J=1

!     Pictorial example:     (NAB = 5,  NF = 3,  NOFF = 2,  I=3)
!
!         A1      A2      A3      A4      A5
!                 *       *       *
!                 F3      F2      F1
!                 V       V       V
!                  \      |(J=2) /
!                   \     |     /
!                    \    |    /
!               (J=3) \   |   / (J=1)
!                      \     /
!                         +
!                         |
!                         V
!         B1      B2      B3      B4      B5

!     Arguments:
!        A    : Input vector to convolve.
!        F    : Filter function to convolve with.
!        B    : Convolved array.
!        NAB  : Dimensioned lengths of A and B.
!        NF   : Length of filter function.
!        NOFF : Index the filter function which multiples the I-th value
!               of A into the I-th value of B.

      subroutine ppavo_maconv (a, f, b, nab, nf, noff)

      integer             , intent(in)    :: nab
      integer             , intent(in)    :: nf
      integer             , intent(in)    :: noff
      real, dimension(nab), intent(in)    :: a
      real, dimension(nf) , intent(in)    :: f
      real, dimension(nab), intent(inout) :: b

      integer :: i, j, lim1, lim2, noabs

!     Initialize the output arrays with zeros.
      noabs = abs(noff)
      do 100 i = 1, nab
         b(i) = 0.0
100   continue

!     Perform the convolution.
      do 300 j = 1, nf
         lim1 = j-noabs+1
         lim2 = j-noabs+nab
         if(lim1 .lt. 1) lim1 = 1
         if(lim2 .gt. nab) lim2 = nab
         do 200 i = lim1, lim2
            b(i) = b(i) + f(j)*a(i-j+noabs)
200      continue
300   continue

      return
      end subroutine ppavo_maconv


!!---------------------------- ppavo_magphs ----------------------------------!!
!!---------------------------- ppavo_magphs ----------------------------------!!
!!---------------------------- ppavo_magphs ----------------------------------!!

!     Purpose:
!        This subroutine computes the squared magnitude and phase of
!        a vector of complex numbers. The output vectors may be the
!        same as the input vectors.

!     Arguments:
!        AR    : Real part of input vector.
!        AI    : Imaginary part of input vector.
!        MAGSQ : Output squared magnitude vector.
!        PHASE : Output phase vector.
!        NS    : Size of all arrays.

      subroutine ppavo_magphs (ar, ai, magsq, phase, ns)

      integer,             intent(in)    :: ns
      real, dimension(ns), intent(inout) :: ar
      real, dimension(ns), intent(inout) :: ai
      real, dimension(ns), intent(inout) :: magsq
      real, dimension(ns), intent(inout) :: phase

      integer :: i
      real    :: m, p

!     Do it!
      do 100 i=1, ns
         m = ar(i)*ar(i) + ai(i)*ai(i)
         p = atan2(ai(i), ar(i))
         magsq(i) = m
         phase(i) = p
100   continue

      return
      end subroutine ppavo_magphs


!!------------------------------ ppavo_mahlbt --------------------------------!!
!!------------------------------ ppavo_mahlbt --------------------------------!!
!!------------------------------ ppavo_mahlbt --------------------------------!!

!     Purpose:
!        This subroutine uses the real part of the complex array A to
!        compute its Hilbert transform, which it places in the imaginary
!        part of the array.

!     Method:
!        The data is convolved with a FIR Hilbert transformer shown in
!        Rabiner and Scharer, "On The Behavior Of Minimax FIR Digital Hilbert
!        Transformers", Bell Syst. Tech. J. Vol. 53 No. 2, p 380, (1974).
!
!        This subroutine must be vectorized in order to run efficiently.

!     Arguments:
!        A      : Input vector to Hilbert transform.
!        H      : Transformed vector.
!        NT     : Dimensioned lengths of A and H.
!        IS1    : Index of first sample to transform.
!        IS2    : Index of last sample to transform.
!        FTRLEN : Length of the Hilbert transformer.
!                 Valid lengths: 0, 31, 43, 63, 95.
!                 A length of 0 suppresses the Hilbert transformation.

      subroutine ppavo_mahlbt (a, h, nt, is1, is2, ftrlen)

      integer            , intent(in)    :: nt
      integer            , intent(in)    :: is1
      integer            , intent(in)    :: is2
      integer            , intent(in)    :: ftrlen
      real, dimension(nt), intent(inout) :: a
      real, dimension(nt), intent(inout) :: h    

      integer :: nlen, maxlen, len1, len2, len3, len4
      integer :: l1, l2, l3, l4, maxl, i, ndx, ftsave
      integer :: l, lt1, lt2, m, m2, lim1, lim2, n

      parameter (nlen = 4, maxlen = 95)
      parameter (len1 = 95, len2 = 63, len3 = 43, len4 = 31)
      parameter (l1 = ((len1+1)/4))
      parameter (l2 = ((len2+1)/4))
      parameter (l3 = ((len3+1)/4))
      parameter (l4 = ((len4+1)/4))
      parameter (maxl = ((maxlen+1)/4))

      integer :: valid(nlen)
      real    :: hltr(-maxlen:maxlen)
      real    :: filter(maxl, nlen)
      real    :: h1(l1), h2(l2), h3(l3), h4(l4)

      equivalence (filter(1,1), h1)
      equivalence (filter(1,2), h2)
      equivalence (filter(1,3), h3)
      equivalence (filter(1,4), h4)

      save ftsave, hltr

      data valid   /len1, len2, len3, len4/      

!     Coefficients for 95 point filter, fl=0.01.
      data h1   /-0.0130099, -0.0045718, -0.0053689, -0.0062800,&
     &           -0.0072616, -0.0083873, -0.0096455, -0.0110350,&
     &           -0.0126022, -0.0143770, -0.0163895, -0.0186922,&
     &           -0.0213465, -0.0244424, -0.0281175, -0.0325665,&
     &           -0.0380864, -0.0451608, -0.0546233, -0.0680547,&
     &           -0.0888468, -0.1258168, -0.2112989, -0.6363167/

!     Coefficients for 63 point filter, fl=0.02.
      data h2   /-0.0055706, -0.0044618, -0.0062078, -0.0083775,&
     &           -0.0110475, -0.0143195, -0.0183266, -0.0232716,&
     &           -0.0294397, -0.0373177, -0.0477262, -0.0622273,&
     &           -0.0841979, -0.1224340, -0.2092445, -0.6356280/

!     Coefficients for 43 point filter, fl=0.02
      data h3   /-0.0216528, -0.0146215, -0.0196329, -0.0259590,&
     &           -0.0340917, -0.0448233, -0.0597557, -0.0821807,&
     &           -0.1209598, -0.2083547, -0.6353344/

!     Coefficients for 31 point filter, fl=0.05.
      data h4   /-0.0041956, -0.0092821, -0.0188358, -0.0344010,&
     &           -0.0595516, -0.1030376, -0.1968315, -0.6313536/

!     Initialize the output array.
      do 10 i = 1,nt
         h(i) = 0.0
10    continue

!     Is ftrlen a valid length?
      if (ftsave .ne. ftrlen) then
         do 20 i = 1,nlen
            ndx = i
            ftsave = valid(ndx)
            if(ftsave .eq. ftrlen) goto 30
20       continue
         ftsave = 0
         return

!     Intialize the filter function.
30       l = (ftsave + 1)/4
         do 40 i = 0,l-1
            hltr(i)    = -filter(l-i,ndx)
            hltr(-i-1) = -hltr(i)
40       continue
      endif

!     Perform the convolution.
      l = (ftrlen + 1)/4
      lt1 = max0(1,is1)
      lt2 = min0(nt,is2)
      if(lt1 .gt. lt2) return
      do 100 m = -l, l-1
         m2 = m*2
         lim1 = m2 + 2
         lim2 = nt + m2 + 1
         if(lim1 .lt. lt1) lim1 = lt1
         if(lim2 .gt. lt2) lim2 = lt2
         do 150 n=lim1, lim2
            h(n) = h(n) + hltr(m)*a(n-m2-1)
150      continue
100   continue

      return
      end subroutine ppavo_mahlbt


!!----------------------------- ppavo_masmth ---------------------------------!!
!!----------------------------- ppavo_masmth ---------------------------------!!
!!----------------------------- ppavo_masmth ---------------------------------!!

!     Purpose:
!        This subroutine convolves a vector with a rectangular filter.

!     Algorithm:
!        This algorithm adds and subtracts elements of a running sum.
!        Computation is of order O(NX).

!     Arguments:
!        NX     : Length of input and output vectors.
!        ISTRID : Stride of input vector. Stride of output vector is 1.
!        ID     : Unknown.
!        X      : Input vector, containing NX elements.
!        Y      : Output vector, to receive NX smoothed elements.
!        L      : Half-length of the boxcar smoothing filter.

      subroutine ppavo_masmth (nx, istrid, id, x, y, l)

      integer                   , intent(in)    :: nx
      integer                   , intent(in)    :: istrid
      integer                   , intent(in)    :: id
      integer                   , intent(in)    :: l
      real, dimension(istrid,nx), intent(inout) :: x
      real, dimension(nx)       , intent(inout) :: y

      integer :: i, j, n, ld, len
      real(kind=8) :: yi, yj

      len = min(l,nx/2)
      if (len .le. 0) then
         do 1 i = 1, nx
            y(i) = x(id,i)
1        continue
         return
      endif

      y(1)  = x(id,1)
      yi    = x(id,1)
      y(nx) = x(id,nx)
      yj    = x(id,nx)
      n     = 1
      do 2 i = 2, len
         j = nx + 1-i
         yi = yi + x(id,n+1) + x(id,n+2)
         yj = yj + x(id,nx-n-1) + x(id,nx-n)
         y(i) = yi
         y(j) = yj
         n = n + 2
         y(i) = y(i)/n
         y(j) = y(j)/n
2     continue

      ld = len + len + 1
      if (nx .lt. ld) goto 5
      yi = yi + x(id,ld-1) + x(id,ld)
      y(1+len) = yi
      do 3 i = len+2, nx-len
         yi = yi + x(id,i+len) - x(id,i-1-len)
         y(i) = yi
3     continue

      do 4 i = len+1, nx-len
         y(i) = y(i)/ld
4     continue

5     continue
      do 6 i = 1, nx
         x(id,i) = y(i)
6     continue

      return
      end subroutine ppavo_masmth


!!------------------------------ ppavo_matv ----------------------------------!!
!!------------------------------ ppavo_matv ----------------------------------!!
!!------------------------------ ppavo_matv ----------------------------------!!

!     Purpose:
!        This subroutine performs the AVO analysis, for either a 1, 2 or 3
!        term inversion.

!     Arguments:
!        S     : Collected weighted sums.
!                 Input:  S(t,1) = S(1,SY)   = SUM { D_i(X) }
!                         S(t,2) = S(1,SX2Y) = SUM { X_i**2 (t) D_i (t) }
!                         S(t,3) = S(1,SX4Y) = SUM { X_i**4 (t) D_i (t) }
!                         S(t,4) = S(1,SX0)  = SUM { X_i**0 (t) } + an(t)
!                         S(t,5) = S(1,SX2)  = SUM { X_i**2 (t) } 
!                         S(t,6) = S(1,SX4)  = SUM { X_i**4 (t) } + an(t)
!                         S(t,7) = S(1,SX6)  = SUM { X_i**6 (t) } 
!                         S(t,8) = S(1,SX8)  = SUM { X_i**8 (t) } + an(t)
!                         S(t,9) = S(1,SY2)  = SUM { D_i**2 (t) } + an(t)
!                 where, D_i(t) is the data at i-th offset at time t
!                        X_i(t) is the sine of i-th incidence angle at time t
!                        an(t) is "white noise" stabilization, given by
!                           [10 ** (-AVOPCT/20)] * S(t, SX0)
!                        AVOPCT is the AVO stability factor, a user parameter.
!        OUT    : OUT(t, ISTG) is the ISTG-th AVO inversion at time t.
!        NUM    : NUM(ISTG, J) points to that element of the following table
!                 of values which will serve as the multiplier to S(t, J)
!                 in the numerator of the ISTG-th AVO attribute at time t.
!                 1  :  0
!                 2  :  S(t,SX0)
!                 3  : -S(t,SX2)
!                 4  :  S(t,SX4)
!                 5  :  S(t,SX0)*S(t,SX4)   - S(t,SX2)*S(t,SX2)
!                 6  :  S(t,SX2)*S(t,SX4)*P - S(t,SX0)*S(t,SX6)
!                 7  :  S(t,SX2)*S(t,SX6)   - S(t,SX4)*S(t,SX4) + S(t,SX4)/P
!                 8  :  S(t,SX0)*S(t,SX8)   - [S(t,SX4)*S(t,SX4)*P]**2
!                 9  :  S(t,SX6)*S(t,SX4)*P - S(t,SX2)*S(t,SX8)
!                 10 :  S(t,SX4)*S(t,SX8)   - S(t,SX6)*S(t,SX6)
!                 where, P = 1/[1 + 10**(-AVOPCT/20)]
!        ND     : ND(ISTG) points to that element of the following table
!                 of values which will serve as the denominator (determinant)
!                 of the ISTG-th AVO inversion at time t.
!                 1 :  S(t,SX0)
!                 2 :  S(t,SX0)*S(t,SX4) - S(t,SX2)*S(t,SX2)
!                 3 :  S(t,SX8)*[S(t,SX0)*S(t,SX4)   - S(t,SX2)*S(t,SX2)] +
!                      S(t,SX6)*[S(t,SX2)*S(t,SX4)*P - S(t,SX0)*S(t,SX6)] +
!                      S(t,SX4)*[S(t,SX2)*S(t,SX6)   - S(t,SX4)*S(t,SX4) +
!                      S(t,SX4)/P] + [S(t,SX4)*P - S(t,SX2)*S(t,SX6)]/P
!                 Note: This is also the minimum necessary fold for inversion.
!        NSAMP  : The number of time samples under analysis, and the
!                 dimensioned size of the OUT and S arrays.
!        FIRST  : First sample to compute in the OUT array.
!        LAST   : Last sample to compute in the OUT array.
!        NSTG   : Number of AVO inversions to perform per time sample.
!        ISZ    : Second dimension of the S array (9).
!        TAPER  : Number of tapering samples at the ends of analysis window.
!        AVOP   : Parameter related to the AVO stability factor (AVOPCT).
!                 AVOP = 1/[1 + 10**(AVOPCT/20)].

      subroutine ppavo_matv (s, out, num, nd, nsamp, first, last,&
     &                      nstg, isz, taper, avop)

      integer                     , intent(in)    :: nsamp
      integer                     , intent(inout) :: first
      integer                     , intent(inout) :: last
      integer                     , intent(in)    :: nstg
      integer                     , intent(in)    :: isz
      integer                     , intent(in)    :: taper
      real(kind=8)                , intent(in)    :: avop
      integer, dimension(3, 3)    , intent(in)    :: num
      integer, dimension(nstg)    , intent(in)    :: nd
      real, dimension(nsamp, isz) , intent(inout) :: s
      real, dimension(nsamp, nstg), intent(inout) :: out

      integer      :: minfld, istg, i, j, nn, newfirst, newlast
      integer      :: sy, sx2y, sx4y, sx0, sx2, sx4, sx6, sx8, sy2
      real(kind=8)  :: t(10), d(3), s0, s2, s4, s6, s8, ep, s4p
      real(kind=8)  :: denom, sum, pfact, eps
      
      parameter (sy  = 1, sx2y = 2, sx4y = 3)
      parameter (sx0 = 4, sx2  = 5, sx4  = 6)
      parameter (sx6 = 7, sx8  = 8, sy2  = 9)
      parameter (eps = 1D-40)

!     Compute the minimum and maximum samples that satisfy the
!     fold requirement.
      minfld = max0(nd(1), nd(2))
      do 10 i = first, last
         newfirst = i
         if (nint(s(i, sx0)) .ge. minfld) goto 20
10    continue

20    do 30 i = last, first, -1
         newlast = i
         if (nint(s(i, sx0)) .ge. minfld) goto 40
30    continue

40    first = newfirst
      last  = newlast

!     Compute the linear AVO coefficients in OUT(I, 1..NSTG)
      t(1) = 0.0

      do 200 i=first, last
         s0 = s(i, sx0)
         s2 = s(i, sx2)
         s4 = s(i, sx4)
         s6 = s(i, sx6)
         s8 = s(i, sx8)
         ep = s4 * avop
         s4p = s4 - ep

         t(2) = s0
         t(3) =-s2
         t(4) = s4
         t(5) = s0*s4 - s2*s2
         t(6) = s2*s4p- s0*s6
         t(7) = s2*s6 - s4*s4 + s4*ep
         t(8) = s0*s8 - s4p*s4p
         t(9) = s6*s4p- s2*s8
         t(10)= s4*s8 - s6*s6
         d(1) = t(2)
         d(2) = t(5)

!        I don't know why, but this line fixes the compile problem.
         d(3) = 0
         d(3) = s8*t(5) + s6*t(6) + s4*t(7) + ep*(s4*s4p - s2*s6)

         do 160 istg = 1, nstg
            denom = d(nd(istg))
            sum = 0.0
            out(i, istg) = sum 
            if(denom .gt. eps) then
               do 150 j = 1, 3
                  nn   = num(istg, j)
                  sum  = sum + s(i, j) * t(nn)
150            continue
               out(i, istg) = sum / denom
            endif
160      continue
200   continue

!     Taper OUT at both edges.
      do 240 i = first, first+taper
         pfact = (float(i) - first) / taper
         if(i .le. nsamp) then
            do 230 istg = 1, nstg
               out(i, istg) = out(i, istg) * pfact
230         continue
         endif
240   continue

      do 260 i = last-taper+1, last
         pfact = float(last - i + 1) / taper
         if(i .gt. 0) then
            do 250 istg = 1, nstg
               out(i, istg) = out(i, istg) * pfact
250         continue
         endif
260   continue

      return
      end subroutine ppavo_matv


!!----------------------------- ppavo_maxmgv ---------------------------------!!
!!----------------------------- ppavo_maxmgv ---------------------------------!!
!!----------------------------- ppavo_maxmgv ---------------------------------!!

!     Purpose:
!        This subroutine locates the maximum magnitude element of a vector.

!     Arguments:
!        A  : Input vector.
!        I  : Stride of input vector.
!        R  : Value of maximum magnitude.
!        LR : Location of maximum magnitude.
!        N  : Size of input vector.

      subroutine ppavo_maxmgv (a, i, r, lr, n)

      integer           , intent(in)  :: n
      integer           , intent(in)  :: i
      integer           , intent(out) :: lr
      real              , intent(out) :: r
      real, dimension(n), intent(in)  :: a

      integer :: k
      real    :: m

      r = 0.0
      do k = 1, n
         m = abs(a(k*i))
         if (m .ge. r) then
            r = m
            lr = k
         end if
      end do

      return
      end subroutine ppavo_maxmgv


!!---------------------------- ppavo_mcarts ----------------------------------!!
!!---------------------------- ppavo_mcarts ----------------------------------!!
!!---------------------------- ppavo_mcarts ----------------------------------!!

!     Purpose:
!        This subroutine converts polar coordinates into Cartesian coordinates.

!     Arguments:
!        MAG   : Magnitude of input vector.
!        PHASE : Phase of input vector vector.
!        RL    : Real part of input vector.
!        IM    : Imaginary part of input vector.
!        NS    : Size of all arrays.

      subroutine ppavo_mcarts (mag, phase, rl, im, ns)

      integer,             intent(in)    :: ns
      real, dimension(ns), intent(inout) :: mag
      real, dimension(ns), intent(inout) :: phase
      real, dimension(ns), intent(inout) :: rl
      real, dimension(ns), intent(inout) :: im

      integer :: i
      real    :: m, p

!     Do it!
      do 100 i=1, ns
         m = mag(i)
         p = phase(i)
         rl(i) = m * cos(p)
         im(i) = m * sin(p)
100   continue

      return
      end subroutine ppavo_mcarts

  
!!----------------------------- ppavo_mcofgn ---------------------------------!!
!!----------------------------- ppavo_mcofgn ---------------------------------!!
!!----------------------------- ppavo_mcofgn ---------------------------------!!

!     Purpose:
!        This subroutine computes 99 interpolation filters on each call.
!        Each filter is LFLTR coefficients in length. These 99 filters
!        can be used to compute the value of a signal to the nearest 0.01
!        sample interval. Computing the value to the nearest 0.01 sample
!        interval is adequate to maintain 30 to 40 db fidelity in the
!        interpolation process.

!     Method:
!        If we have a signal with samples at locations X1,X2,X3 and X4
!        and we want the value of the signal at location P, we would
!        first compute the fractional distance by dividing the distance
!        P-X2 by the distance X3-X2 and then round the result to the
!        nearest 0.01 and multiply by 100. The result is a number
!        between 0 and 100.  This is the index to the filter array and
!        is used to determine which of the 99 filters to use in the
!        interpolation. If the result is 0 or 100, P is within plus or
!        minus 0.005 * (sample interval) of X2 or X3 and is close enough
!        to use the known value without going through the interpolation
!        step.
                                           
!        The filters computed are Wiener filters. A correlation function
!        is required in order to compute the filters. The filters are
!        computed using the matrix equation:                                    
!                                                                              
!                 A F = C                                                      
                                                                           
!        where A is auto-correlation matrix and C is cross-correlation
!        matrix. The equation is solved using a modified version of
!        subroutine EUREKA (renamed ppavo_MWIENR) from Robinson's
!        book. The correlation function used is a bandlimited sync
!        function. For details of the interpolation procedure and how
!        the parameters are related see Sicking's Ph.D. dissertation.
!        LFLTR and MXFREQ are interrelated and their values should be
!        chosen based on knowledge of the maximum frequency present in
!        the signal and the curves presented in Sicking's dissertation
!        Chapter 4. 

!     Arguments:
!        LFLTR  : Number of coefficients in the filters.
!        MXFREQ : Maximum frequency as a fraction of Nyquist.
!        ACOR   : Array for auto-correlation.
!        CCOR   : Array for cross-correlation.
!        FLTR   : Filter array used by subroutine ppavo_MWIENR.
!        WORK   : Work array used by subroutine ppavo_MWIENR.
!        COEF   : Coefficent array. 99 filters of length LFLTR.

      subroutine ppavo_mcofgn (lfltr, mxfreq, acor, ccor, fltr, work, coef)

      integer                   , intent(in)    :: lfltr
      real                      , intent(in)    :: mxfreq
      real, dimension(:)        , intent(inout) :: acor
      real, dimension(:)        , intent(inout) :: ccor
      real, dimension(:)        , intent(inout) :: fltr
      real, dimension(:)        , intent(inout) :: work
      real, dimension(lfltr,99) , intent(out)   :: coef

      integer :: i, j, k, l, m, lcn
      real    :: cord, tx, frac, cordis, val

!     Compute the auto-correlation. The auto-correlation matrix is the
!     same for all filters computed so it needs to be computed only once.
!     The parameter CORD is the lag in number of sample intervals.
      cord    = 1.0                                                       
      acor(1) = 1.0                                                       
                                                                           
      do 10 i = 2, lfltr                                                  
         tx = cord * pi * mxfreq                                         
         acor(i) = sin(tx) / tx                                          
         cord = cord + 1.0                                               
10    continue                                                            

!     Compute each of first 50 filters.

!     LCN is incremented from 1 to 50 while FRAC is incremented from
!     0.01 to 0.50. The cross-correlation array is filled, the filter
!     is computed, and the filter is stored in the output COEF array.
 
!     The parameter CORDIS is the correlation lag in sample spacings
!     between a given sample and the desired sample location.
      lcn  = 1
      frac = 0.01
20    cordis = -((lfltr / 2) - 1) - frac

!     Compute the cross-correlations.
      do 50 i = 1, lfltr
         tx = cordis * pi * mxfreq
         if (tx .eq. 0.0) goto 30
         val = sin(tx) / tx
         goto 40
30       val = 1.0
40       ccor(i) = val
         cordis = cordis + 1.0                                            
50    continue                                                             

!     Compute Wiener filter and store in COEF array.
      call ppavo_mwienr (lfltr, acor, ccor, work, fltr)          
                                                                           
      do 60 i = 1, lfltr
         coef(i, lcn) = fltr(i)
60    continue

!     Increment FRAC and LCN and return to compute the next filter.
      frac = frac + 0.01
      lcn = lcn + 1
      if (lcn .lt. 51) goto 20

!     The first 509 filters have been computed. Filter 50 is the
!     filter to be used for midpoint interpolation. There is symmetry
!     about midpoint. Reverse first 49 filters and store in last 49.
      do 80 i = 1, 49
         j = 50 - i
         k = 50 + i
         do 70 l = 1, lfltr
            m = lfltr - l + 1
            coef(m, k) = coef(l, j)
70       continue
80    continue 
         
      return
      end subroutine ppavo_mcofgn


!!----------------------------- ppavo_mpowr ---------------------------------!!
!!----------------------------- ppavo_mpowr ---------------------------------!!
!!----------------------------- ppavo_mpowr ---------------------------------!!

!     Purpose:
!        This subroutine raises a positive real vector to a scalar exponent
!        between 0 and 1.

!     Arguments:
!        IN   : Input vector.
!        EXP2 : Exponent.
!        OUT  : Output vector.
!        NS   : Size of all arrays.

      subroutine ppavo_mpowr (in, exp2, out, ns)

      integer,             intent(in)  :: ns
      real,                intent(in)  :: exp2
      real, dimension(ns), intent(in)  :: in
      real, dimension(ns), intent(out) :: out

      real :: pow, floor
      parameter (floor = -50.0)

      if (exp2 .le. 0 .or. exp2 .ge. 2.0) then
         call ppavo_vmove(in, 1, out, 1, ns)
         return
      endif

      pow = exp2 / 2.0
      call ppavo_vlogz(in, 1, floor, out, 1, ns)
      call ppavo_vsmul(out, 1, pow, out, 1, ns)
      call ppavo_vexp(out, 1, out, 1, ns)

      return
      end subroutine ppavo_mpowr


!!----------------------------- ppavo_mwienr ---------------------------------!!
!!----------------------------- ppavo_mwienr ---------------------------------!!
!!----------------------------- ppavo_mwienr ---------------------------------!!

!     Purpose:
!        This subroutine finds the solutions of single-channel normal
!        equations which arise in least-squares filtering and prediction
!        problems for single-channel time series.

!     Arguments:
!        LFLTR : Length of the filter.
!        ACOR  : Auto-correlation coefficients.
!        RHS   : Cross-correlation coefficients.
!        ERR   : Prediction error operator.
!        FLTR  : Filter coefficients.

      subroutine ppavo_mwienr (lfltr, acor, rhs, err, fltr)

      integer           , intent(in)    :: lfltr
      real, dimension(:), intent(inout) :: acor
      real, dimension(:), intent(inout) :: rhs
      real, dimension(:), intent(inout) :: err
      real, dimension(:), intent(inout) :: fltr

      integer :: i, j, k, l, l1, l2, l3
      real    :: v, d, q, hold

      v       = acor(1)
      d       = acor(2)
      err(1)  = 1.0
      fltr(1) = rhs(1) / v
      q       = fltr(1) * acor(2)
      if (lfltr .eq. 1) goto 70

      do 60 l = 2, lfltr
         err(l) = - d / v
         if (l .eq. 2) goto 30
         l1 = (l - 2) / 2
         l2 = l1 + 1
         if (l2 .lt. 2) goto 20

         do 10 j = 2, l2
            hold = err(j)
            k = l - j + 1
            err(j) = err(j) + err(l) * err(k)
            err(k) = err(k) + err(l) * hold
10       continue

20       if (2*l1 .eq. l-2) goto 30
         err(l2+1) = err(l2+1) + err(l) * err(l2+1)
30       v = v + err(l) * d
         fltr(l) = (rhs(l) - q) / v
         l3 = l - 1

         do 40 j = 1, l3
            k = l - j + 1
            fltr(j) = fltr(j) + fltr(l) * err(k)
40       continue

         if (l .eq. lfltr) goto 70
         d = 0.0
         q = 0.0

         do 50 i = 1, l
            k = l - i + 2
            d = d + err(i)  * acor(k)
            q = q + fltr(i) * acor(k)
50       continue

60    continue 
       
70    return
      end subroutine ppavo_mwienr


!!----------------------------- ppavo_nacma ----------------------------------!!
!!----------------------------- ppavo_nacma ----------------------------------!!
!!----------------------------- ppavo_nacma ----------------------------------!!

!     Purpose:
!        This subroutine multiplies the complex 'B' vector by the
!        complex conjugate of the complex 'K' vector, and adds the
!        result to the complex 'A' vector.

!     Arguments:
!        NS : Size of complex arrays.
!        AR : Real part of complex 'A' vector.
!        AI : Imaginary part of complex 'A' vector.  
!        BR : Real part of complex 'B' vector.
!        BI : Imaginary part of complex 'B' vector.  
!        KR : Real part of complex 'K' vector.
!        KI : Imaginary part of complex 'K' vector.

      subroutine ppavo_nacma (ar, ai, br, bi, kr, ki, ns)

      integer            , intent(in)    :: ns
      real, dimension(ns), intent(in)    :: br
      real, dimension(ns), intent(in)    :: bi
      real, dimension(ns), intent(in)    :: kr
      real, dimension(ns), intent(in)    :: ki
      real, dimension(ns), intent(inout) :: ar
      real, dimension(ns), intent(inout) :: ai

      integer :: i
      real    :: temp

      do 100 i = 1, ns
         temp  = ar(i) + br(i)*kr(i) + bi(i)*ki(i)
         ai(i) = ai(i) + bi(i)*kr(i) - br(i)*ki(i)
         ar(i) = temp
100   continue

      end subroutine ppavo_nacma


!!----------------------------- ppavo_naacms ---------------------------------!!
!!----------------------------- ppavo_naacms ---------------------------------!!
!!----------------------------- ppavo_naacms ---------------------------------!!

!     Purpose:
!        This subroutine computes the squared magnitude of a complex
!        vector field averaged over a space-time window.

!     Arguments:
!        SUM  : Running sum of |A|**2.
!        A    : In  -> Data to use.
!              Out -> Weighted smoothed data.
!        W    : Weights for the average.
!        TMP  : Temporary array.
!        FSA  : First sample under analysis.
!        LSA  : Last sample under analysis.
!        NS   : Dimensioned size of arrays.
!        TWIN : Time window length (samples).
!        FAC  : Combination of scale factor.

      subroutine ppavo_naacms (sum, a, w, tmp, fsa, lsa, ns, twin, fac)

      integer,                         intent(in)    :: fsa
      integer,                         intent(in)    :: lsa
      integer,                         intent(in)    :: ns
      integer,                         intent(in)    :: twin
      real,                            intent(in)    :: fac
      real,             dimension(ns), intent(inout) :: a
      real,             dimension(ns), intent(in)    :: w
      real,             dimension(ns), intent(out)   :: tmp
      double precision, dimension(ns), intent(inout) :: sum

      integer :: l3, ndx1, ndx2, nsmth, i

!     Weight the data.
      l3 = (twin - 1)/3 + 1
      ndx1 = max0(fsa-l3+1, 1)
      ndx2 = min0(lsa+l3-1, ns)
      nsmth = ndx2 - ndx1 + 1
      call ppavo_vmul(a(ndx1), 1, w(ndx1), 1, tmp(ndx1), 1, nsmth)  

!     Doubly smooth it.
      call ppavo_vzero(a, 1, ns)
      call ppavo_namsum(tmp(ndx1), a(ndx1), nsmth, twin)
      call ppavo_vmove(a, 1, tmp, 1, ns)
      call ppavo_namsum(tmp, a, ns, l3)

!     Combine with the previous running sum.
      do 150 i = fsa, lsa
         sum(i) = sum(i) + fac*a(i)
150   continue

      return
      end subroutine ppavo_naacms


!!----------------------------- ppavo_nabigs ---------------------------------!!
!!----------------------------- ppavo_nabigs ---------------------------------!!
!!----------------------------- ppavo_nabigs ---------------------------------!!

!     Purpose:
!        This subroutine computes the imaginary part of the element
!        by element product of the 'A' complex vector times the
!        complex conjugate of the 'B' vector.
!
!     Arguments:
!        AR  : Real part of the A vector.
!        AI  : Imaginary part of the A vector.
!        BR  : Real part of the B vector.
!        BI  : Imaginary part of the B vector.
!        OUT : Imaginary part of AB*.
!        NS  : Dimensioned size of arrays.

      subroutine ppavo_nabigs (ar, ai, br, bi, out, ns)

      integer,             intent(in)  :: ns
      real, dimension(ns), intent(in)  :: ar
      real, dimension(ns), intent(in)  :: ai
      real, dimension(ns), intent(in)  :: br
      real, dimension(ns), intent(in)  :: bi
      real, dimension(ns), intent(out) :: out

      integer :: i

      do 100 i = 1, ns
         out(i) = ai(i)*br(i) - ar(i)*bi(i)
100   end do

      return
      end subroutine ppavo_nabigs


!!----------------------------- ppavo_nabrgs ---------------------------------!!
!!----------------------------- ppavo_nabrgs ---------------------------------!!
!!----------------------------- ppavo_nabrgs ---------------------------------!!

!     Purpose:
!        This subroutine computes the real part of the element
!        by element product of the 'A' complex vector times the
!        complex conjugate of the 'B' vector.
!
!     Arguments:
!        AR  : Real part of the A vector.
!        AI  : Imaginary part of the A vector.
!        BR  : Real part of the B vector.
!        BI  : Imaginary part of the B vector.
!        OUT : Imaginary part of AB*.
!        NS  : Dimensioned size of arrays.

      subroutine ppavo_nabrgs (ar, ai, br, bi, out, ns)

      integer,             intent(in)  :: ns
      real, dimension(ns), intent(in)  :: ar
      real, dimension(ns), intent(in)  :: ai
      real, dimension(ns), intent(in)  :: br
      real, dimension(ns), intent(in)  :: bi
      real, dimension(ns), intent(out) :: out

      integer :: i

      do 100 i = 1, ns
         out(i) = ar(i)*br(i) + ai(i)*bi(i)
100   end do

      return
      end subroutine ppavo_nabrgs


!!----------------------------- ppavo_namags ---------------------------------!!
!!----------------------------- ppavo_namags ---------------------------------!!
!!----------------------------- ppavo_namags ---------------------------------!!

!     Purpose:
!        This subroutine computes the magnitude of the
!        elements of the A vector.
!
!     Arguments:
!        AR  : Real part of the A vector.
!        AI  : Imaginary part of the A vector.
!        BR  : Real part of the B vector.
!        BI  : Imaginary part of the B vector.
!        OUT : Imaginary part of AB*.
!        NS  : Dimensioned size of arrays.

      subroutine ppavo_namags (ar, ai, out, ns)

      integer,             intent(in)  :: ns
      real, dimension(ns), intent(in)  :: ar
      real, dimension(ns), intent(in)  :: ai
      real, dimension(ns), intent(out) :: out

      integer :: i

      do 100 i = 1, ns
         out(i) = ar(i)**2 + ai(i)**2
100   end do

      return
      end subroutine ppavo_namags


!!----------------------------- ppavo_namsum ---------------------------------!!
!!----------------------------- ppavo_namsum ---------------------------------!!
!!----------------------------- ppavo_namsum ---------------------------------!!

!     Purpose:
!        This subroutine computes a sliding moving average.
!        It differs from 'ppavo_masmth' in that no normalization
!        is performed.
!
!     Arguments:
!        AIN   : Array to average.
!        AOUT  : Averaged array.
!        NS    : Dimensioned size of arrays.
!        NTWIN : Number of samples in moving average.

      subroutine ppavo_namsum (ain, aout, ns, ntwin)

      integer,             intent(in)  :: ns
      integer,             intent(in)  :: ntwin
      real, dimension(ns), intent(in)  :: ain
      real, dimension(ns), intent(out) :: aout

      integer          :: l, l2, i, limit
      double precision :: sum

!     Round ntwin up to next odd number.
      l = (ntwin/2)*2 + 1
      l2= l / 2
      sum = 0.0
      limit = min0(l2, ns)

!     Phase 0:  no smoothing if l=1:
      if (l .eq. 1) then
         do 50 i=1, ns
50       aout(i) = ain(i)
         return
      endif

!     Phase 1/2:  set output to zero if l <= 0.
      if (l .le. 0) then
         do 70 i=1, ns
70       aout(i) = sum
         return
      endif

!     Phase 1:  collect the initial samples.  no outputs, yet.
      limit = min0(l2, ns)
      do 100 i=1, limit
100   sum = sum + ain(i)

!     Phase 1 1/2:  only if smoothed to a constant.
      if (l2 .gt. ns) then
         do 120 i=1, ns
120      aout(i) = sum
         return
      endif

!     Phase 2:  add input samples; begin outputting.
      limit = min0(l, ns)
      do 200 i=l2+1, limit
         sum = sum + ain(i)
200   aout(i-l2) = sum

!     Phase 3:  add and subtract samples; continue outputting.
      do 300 i=limit+1, ns
         sum = sum + ain(i) - ain(i-l)
300   aout(i-l2) = sum

!     Phase 4:  subtract samples; finish outputting.
      do 400 i=ns+1, ns+l2
         sum = sum - ain(i-l)
400   aout(i-l2) = sum

      return
      end subroutine ppavo_namsum


!!----------------------------- ppavo_napowi ---------------------------------!!
!!----------------------------- ppavo_napowi ---------------------------------!!
!!----------------------------- ppavo_napowi ---------------------------------!!

!     Purpose:
!        This subroutine computes a fixed integer power
!        of a real array.
!
!     Arguments:
!        A   : Array to be powered.
!        IP  : Scalar integer power.
!        OUT : A**IP
!        NS  : Dimensioned size of arrays.

      subroutine ppavo_napowi (a, ip, out, ns)

      integer,             intent(in)  :: ns
      integer,             intent(in)  :: ip
      real, dimension(ns), intent(in)  :: a
      real, dimension(ns), intent(out) :: out

      integer :: i

      do 100 i = 1, ns
         out(i) = a(i) ** ip
100   end do

      return
      end subroutine ppavo_napowi


!!----------------------------- ppavo_napowr ---------------------------------!!
!!----------------------------- ppavo_napowr ---------------------------------!!
!!----------------------------- ppavo_napowr ---------------------------------!!

!     Purpose:
!        This subroutine computes a power of the elements of a
!        real array. The elements of this array must be positive.
!        Zero will be returned otherwise.
!
!     Arguments:
!        A   : Array to be powered.
!        P   : Powers to take the array.
!        OUT : A**P
!        NS  : Dimensioned size of arrays.

      subroutine ppavo_napowr (a, p, out, ns)

      integer,             intent(in)  :: ns
      real, dimension(ns), intent(in)  :: p
      real, dimension(ns), intent(in)  :: a
      real, dimension(ns), intent(out) :: out

      integer :: i
      real    :: tmp

      do 100 i = 1, ns
         tmp = a(i)
         out(i) = 0.0
         if (tmp .gt. 0.0) out(i) = tmp ** p(i)
100   end do

      return
      end subroutine ppavo_napowr


!!----------------------------- ppavo_nazero ---------------------------------!!
!!----------------------------- ppavo_nazero ---------------------------------!!
!!----------------------------- ppavo_nazero ---------------------------------!!

!     Purpose:
!        This subroutine zeros out elements of the 'A' array whose
!        corresponding elements of the 'Z' are already zero.
!
!     Arguments:
!        A   : Array of elements to zero.
!        Z   : Template array.
!        NS  : Dimensioned size of arrays.

      subroutine ppavo_nazero (a, z, ns)

      integer,             intent(in)    :: ns
      real, dimension(ns), intent(inout) :: a
      real, dimension(ns), intent(in)    :: z

      integer :: i

      do 100 i = 1, ns
         if(z(i) .eq. 0.0) a(i) = 0.0
100   end do

      return
      end subroutine ppavo_nazero


!!------------------------------ ppavo_rmsscl -------------------------------!!
!!------------------------------ ppavo_rmsscl -------------------------------!!
!!------------------------------ ppavo_rmsscl -------------------------------!!

!     Purpose:
!        This subroutine computes a scale factor to apply to a
!        section to make its rms value be "SCLFCT * 1000".  This
!        is good for unisec plotting with a gain of 12 db.

!     Arguments:
!        STATS  : Statistics array.
!        NSTAT  : Size of statistics array.
!        SCLFCT : Scale factor.

      subroutine ppavo_rmsscl (stats, nstat, sclfct)

      integer                           , intent(in)    :: nstat
      real                              , intent(in)    :: sclfct
      double precision, dimension(nstat), intent(inout) :: stats

      double precision :: rms

      if (sclfct .le. 0.0) then
         stats(6) = -sclfct
         return
      endif

      if (stats(5) .gt. 0 .and. stats(2) .gt. 0) then
         rms  = dsqrt(stats(2)/stats(5))
         stats(6) = 1d3 * sclfct / rms
      else
         stats(6) = 1d0
      endif 

      return
      end subroutine ppavo_rmsscl


!!------------------------------ ppavo_runsc ---------------------------------!!
!!------------------------------ ppavo_runsc ---------------------------------!!
!!------------------------------ ppavo_runsc ---------------------------------!!

!     Purpose:
!        This subroutine computes the RUNS statistic.

!     Arguments:
!        RUNS : Raw data.
!        OUT  : RUNS statistic.
!        NSA  : Number of samples under analysis.
!        POS  : Position of the output array.

      subroutine ppavo_runsc (runs, out, nsa, pos)

      integer                   , intent(in)    :: nsa
      integer                   , intent(in)    :: pos
      integer, dimension(nsa, 3), intent(in)    :: runs
      real, dimension(nsa, pos) , intent(inout) :: out

      integer :: nneg, npos, nrun
      integer :: np, nn, nr, pr, sum, i
      real(kind=8)  :: z, s, s2, mu

      parameter (npos = 1, nneg = 2, nrun = 3)

      do 200 i = 1, nsa
         np  = runs(i, npos)
         nn  = runs(i, nneg)
         nr  = iabs(runs(i, nrun))
         pr  = np * nn
         sum = np + nn
         z   = 0.0
         if (sum .le. 10) goto 100

         mu = (2d0 * pr) / sum + 1
         s2 = (2d0 * pr) * (2*pr - sum)
         s2 = ((s2 / sum) / sum) / (sum - 1)
         s  = sqrt(s2)
         z  = nr - mu
         if (z .lt. 0.0) z = z + 0.5
         if (z .gt. 0.0) z = z - 0.5
         z  = z / s
100      out(i, pos) = z
200   continue

      return
      end subroutine ppavo_runsc


!!----------------------------- ppavo_saahci ---------------------------------!!
!!----------------------------- ppavo_saahci ---------------------------------!!
!!----------------------------- ppavo_saahci ---------------------------------!!

!     Purpose:
!        This subroutine....

!     Arguments:
!        AR     : Unknown.
!        AI     : Unknown.
!        BR     : Unknown.
!        BI     : Unknown.
!        POW    : Unknown.
!        HCI    : Unknown.
!        AX     : Unknown.
!        BX     : Unknown.
!        SIGA   : Unknown.
!        SIGB   : Unknown.
!        ALPHA  : Unknown.
!        NS     : Size of data arrays.
!        SCLOPT : Unknown.

      subroutine ppavo_saahci (ar, ai, br, bi, pow, hci, ax, bx,&
     &                        siga, sigb, alpha, ns, sclopt)

      integer            , intent(in)    :: ns
      integer            , intent(in)    :: sclopt
      real               , intent(in)    :: alpha
      real, dimension(ns), intent(inout) :: ar
      real, dimension(ns), intent(inout) :: ai
      real, dimension(ns), intent(inout) :: br
      real, dimension(ns), intent(inout) :: bi
      real, dimension(ns), intent(inout) :: pow
      real, dimension(ns), intent(inout) :: hci
      real, dimension(ns), intent(inout) :: ax
      real, dimension(ns), intent(inout) :: bx
      real, dimension(ns), intent(inout) :: siga
      real, dimension(ns), intent(inout) :: sigb

!     Procedure to follow if alpha .ne. 0.0.
      if (alpha .ne. 0.0) then

!        ax <-- re{a + b};  bx <-- im{a + b}
         call ppavo_vadd(ar, 1, br, 1, ax, 1, ns)
         call ppavo_vadd(ai, 1, bi, 1, bx, 1, ns)

!        ax <-- |a + b|**2 / {2 * (|a|**2 + |b|**2)}
         call ppavo_namags(ax, bx, ax, ns)
         call ppavo_vdivz(ax, 1, hci, 1, 0.0, ax, 1, ns)
         call ppavo_vsmul(ax, 1, 0.5, ax, 1, ns)

!        bx <-- (ax)**pow - 0.5
         call ppavo_napowr(ax, pow, bx, ns)
         call ppavo_vsadd(bx, 1, -0.5, bx, 1, ns)

!        hci <-- hci * bx
         call ppavo_vmul(hci, 1, bx, 1, hci, 1, ns)

!     Procedure to follow if alpha .eq. 0.0.
      else

!        hci <-- re{(ar, ai)*(br, bi)*}
         call ppavo_nabrgs(ar, ai, br, bi, hci, ns)
      endif

!     hci <-- (hci * sigb) / [siga ** (3 - sclopt)]
      call ppavo_vmul(hci, 1, sigb, 1, hci, 1, ns)
      call ppavo_napowi(siga, 3 - sclopt, ax, ns)
      call ppavo_vdivz(hci, 1, ax, 1, 0.0, hci, 1, ns)

      return
      end subroutine ppavo_saahci


!!----------------------------- ppavo_saarvi ---------------------------------!!
!!----------------------------- ppavo_saarvi ---------------------------------!!
!!----------------------------- ppavo_saarvi ---------------------------------!!

!     Purpose:
!        This is a multi-purpose routine, used in the computation
!        of the optimal hydrocarbon and residual velocity indicators.
!        It performs the following operations:
!
!        1)  Scale the complex 'B' trace to equalize its amplitude
!            with the 'A' trace:
!              BR <-- BR * (SIGA/SIGB);  BI <-- BI * (SIGA/SIGB)
!
!        2)  Compute the imaginary part of the scaled 'AB*' product:
!              RVI <-- AI*BR - AR*BI
!
!        3)  Set 'HCI' to be the squared magnitude of 'A';
!            Set 'AMAG' to be the squared scaled radius:
!              HCI <-- |A|**2 = AR**2 + AI**2
!              AMAG <-- HCI + BR**2 + BI**2 = |A|**2 + |B|**2
!
!        4)  Remove the quadrature component from 'B':
!              BR <-- BR - (AI*RVI)/HCI
!              BI <-- BI + (AR*RVI)/HCI
!
!        5)  Normalize the residual velocity indicator:
!              If (SCLOPT == 0):  RVI <-- RVI / AMAG
!              If (SCLOPT == 1):  RVI <-- RVI / SQRT {AMAG}
!              If (SCLOPT == 2):  Leave rvi alone.
!
!        6)  Compute the `radius' of |A| and scaled |B|, quad removed:
!              HCI <-- HCI + BR**2 + BI**2 = |A|**2 + |B|**2
!              AMAG <-- SQRT {HCI}
!
!   Note:  This routine should be called prior to 'ppavo_saahci',
!
!     Arguments:
!        AR     : Original RE{A}.
!        AI     : Original IM{A}.
!        BR     : Original RE{B}; Scaled RE{B} no quadrature.
!        BI     : Original IM{B}; Scaled RE{B} no quadrature.
!        SIGA   : Sqrt { AVG(|A|**2) }.
!        SIGB   : Sqrt { AVG(|B|**2) }.
!        HCI    : |A|**2 + |B|**2, after scaling
!        RVI    : Residual velocity indicator:
!                 IM {AB*} / AMAG, after scaling.
!        AMAG   : Sqrt {HCI}
!        NS     : Size of all arrays.
!        SCLOPT : Scaling option of indicators:
!                 0 -> unity scaling.
!                 1 -> scale like sqrt(|A|**2 + |B|**2)
!                 2 -> scale like (|A|**2 + |B|**2)

      subroutine ppavo_saarvi (ar, ai, br, bi, siga, sigb, hci, rvi,&
     &                        amag, ns, sclopt)

      integer            , intent(in)    :: ns
      integer            , intent(in)    :: sclopt
      real, dimension(ns), intent(out)   :: amag
      real, dimension(ns), intent(in)    :: ar
      real, dimension(ns), intent(in)    :: ai
      real, dimension(ns), intent(inout) :: br
      real, dimension(ns), intent(inout) :: bi
      real, dimension(ns), intent(in)    :: siga
      real, dimension(ns), intent(in)    :: sigb
      real, dimension(ns), intent(out)   :: hci
      real, dimension(ns), intent(out)   :: rvi

!     Step 1: Scale the complex 'B' trace to equalize its
!             amplitude with the 'A' trace.
      call ppavo_vdivz(siga, 1, sigb, 1, 0.0, amag, 1, ns)
      call ppavo_vmul(br, 1, amag, 1, br, 1, ns)
      call ppavo_vmul(bi, 1, amag, 1, bi, 1, ns)

!     Step 2: Compute the imaginary part of the scaled AB* product.
!        RVI <-- AI*BR - AR*BI
      call ppavo_nabigs(ar, ai, br, bi, rvi, ns)

!     Step 3: Set HCI to be the squared magnitude of 'A'.
!             Set 'AMAG' to be |A|**2 + |B|**2.
      call ppavo_namags(ar, ai, hci, ns)
      call ppavo_namags(br, bi, amag, ns)
      call ppavo_vadd(hci, 1, amag, 1, amag, 1, ns)

!     Step 4: Remove the quadrature component from 'B'.
!        BR <-- BR - (AI*RVI)/HCI
!        BI <-- BI + (AR*RVI)/HCI
!      do 100 i = 1, ns
!         tmp = hci(i)
!         if (tmp .gt. 0.0) then
!            tmp   = rvi(i)/tmp
!            br(i) = br(i) - ai(i)*tmp
!            bi(i) = bi(i) + ar(i)*tmp
!         endif
!100   continue

!     Step 5: Normalize the residual velocity indicator.
!        RVI <-- RVI / AMAG
!        RVI <-- RVI / SQRT{AMAG}
      if (sclopt .eq. 1) then
         call ppavo_vsqrtz(amag, 1, 0.0, amag, 1, ns)
      end if
      if (sclopt .eq. 2) then
         call ppavo_vfill(1.0, amag, 1, ns)
      end if
      call ppavo_vdivz(rvi, 1, amag, 1, 0.0, rvi, 1, ns)

!     Step 6: Compute the radius of |A| and scaled |B|, no quadrature.
!         HCI <-- HCI + BR**2 + BI**2 = |A|**2 + |B|**2
!        AMAG <-- SQRT{HCI}
      call ppavo_namags(br, bi, amag, ns)
      call ppavo_vadd(hci, 1, amag, 1, hci, 1, ns)
      call ppavo_vsqrtz(hci, 1, 0.0, amag, 1, ns)

      return
      end subroutine ppavo_saarvi


!!----------------------------- ppavo_sabnft ---------------------------------!!
!!----------------------------- ppavo_sabnft ---------------------------------!!
!!----------------------------- ppavo_sabnft ---------------------------------!!

!     Purpose:
!        This subroutine generates H1(T) and H2(T) inversion filters for
!        NMO destretching, specifically for a Gaussian bandpass wavelet.

!     Method:
!        Using the functional form for a Ricker wavelet:
!           W(T) = (1 - 0.5 * W0T^2) * exp(-0.25 * W0T^2)
!        the auto-correlation of W(T) and cross-correlation between
!        W(T) and TW'(T) are explicitly evaluated. The inversion
!        filters are then obtained through conventional Levinson
!        inversions.

!     Restrictions: (checked by IER flag)
!        1) PCTCNS > 0.0
!        2) NH <= MXH
!        3) A > 1.0

!     Arguments:  
!        MXH    : Dimensioned size of output inversion filters.
!        NH     : Actual size of output inversion filters.
!        W0T    : Omega 0 times the time step size (unitless).
!        PCTWNS : Additional white noise percentage.
!        PCTCNS : Additional colored noise percentage.
!        A      : Auto-regressive noise coefficient.
!                 RN(N) = PCTCNS * .01 * RW(0) * A ** N.
!        KS     : Second dimension of work array.
!        H1,H2  : Inversion filters.
!        CORR   : Auto-correlation of the wavelet.
!        CVECT  : Cross-correlation vector between W(T) and TW'(T).
!        WORK   : Work array.
!                 KS = 1: Cross-correlation between W(T) and TW'(T).
!                 KS > 1: Work area for subroutine ppavo_swlev.
!        IER    : Error flag.
!                 0 = Normal completion.
!                 1 = Auto-correlation matrix was singular.
!                 2 = MXH was not large enough.

      subroutine ppavo_sabnft (h1, h2, corr, cvect, work, mxh, nh,&
     &   pctwns, pctcns, a, ks, ier)

      integer                            ,intent(out)   :: ier
      integer                            ,intent(in)    :: ks
      integer                            ,intent(in)    :: mxh
      integer                            ,intent(in)    :: nh
      real                               ,intent(in)    :: pctwns
      real                               ,intent(in)    :: pctcns
      real,  dimension(-mxh:mxh)         ,intent(out)   :: cvect
      real,  dimension(-mxh:mxh)         ,intent(out)   :: h1
      real,  dimension(-mxh:mxh)         ,intent(out)   :: h2
      real,  dimension(0:2*mxh)          ,intent(inout)   :: corr
      real(kind=8),dimension(-mxh:mxh,ks),intent(out)   :: work
      double precision                   ,intent(in)    :: a
  
      integer          :: i
      integer          :: lag
      integer          :: maxlag
      integer          :: nwork
      real             :: thresh
      real             :: c0, c1

      double precision :: an, ac

      parameter (thresh = 170.0)

!     Check the array sizes.
      ier = 0
      maxlag = 2*nh
      nwork = ks * (maxlag + 1)
      if (pctwns .le. 0.0) ier = 1
      if (nh .gt. mxh) ier = 2
      if (abs(a) .ge. 1.0) ier = 3
      if (ier .ne. 0) return

!     Compute the H2(T) filter.
      c0 = corr(0) * pctwns * 1e-2
      c1 = corr(0) * pctcns * 1e-2
      corr(0) = corr(0) + c0
      an = c1

      do 350 i = 0, maxlag
         corr(i) = corr(i) + an
         an      = an * a
350   continue

      call ppavo_swlev(corr, 1, cvect(-nh:), 1, h2(-nh:), 1, maxlag+1,&
     &                work, nwork)

!     Compute the H1(T) inversion filter.
      an = c1
      do 300 lag = 0, nh
         ac = corr(lag) - an
         if(lag .eq. 0) ac = ac - c0
         cvect(lag)   = ac
         cvect(-lag)  = ac
         an           = an * a
300   continue

      call ppavo_swlev(corr, 1, cvect(-nh:), 1, h1(-nh:), 1, maxlag+1,&
     &                work, nwork)

      return
      end subroutine ppavo_sabnft
  
  
!!----------------------------- ppavo_sabwave --------------------------------!!
!!----------------------------- ppavo_sabwave --------------------------------!!
!!----------------------------- ppavo_sabwave --------------------------------!!

!     Purpose:
!        This subroutine computes the auto-correlation of a bandpass wavelet.

!     Notes:
!        WAVELT <-- W(T) * W(-T)
!        TWVLTP <-- W(T) * {-TW'(-T)}

!     Arguments:  
!        FFTR   : Real scratch array of length NS+2, where NS+1
!                 is a power of 2. Used for FFT.
!        FFTC   : Complex scratch array of length NS+2, where NS+1
!                 is a power of 2. Used for FFT.
!        NS     : Determines length of the fft array.
!        WAVELT : The auto-correlation of the wavelet.
!        TWVLTP : The cross-correlation of the W(T) and TW'(T).
!        MAXH   : Determines dimensioned length of the correlation
!                 arrays WAVELT and twvltp.
!        NH     : Number of correlation lag values to actually compute.
!        FS     : Sampling frequency (Hz).
!        WPARS  : Four bandpass wavelet cutoff frequencies. The wavelet
!                 spectra will have Gaussian tapers whose cutoff frequencies
!                 being 30 db down from the pass band (units are Hz).
!                    WPARS(1) = low cutoff frequency.
!                    WPARS(2) = low pass frequency.
!                    WPARS(3) = high pass frequency.
!                    WPARS(4) = high cutoff frequency. 

      subroutine ppavo_sabwave (fftr, fftc, ns, wavelt, twvltp, &
     &                          maxh, nh, fs, wpars)

      real, dimension(0:ns+1)    , intent(out) :: fftr
      complex, dimension(0:ns+1) , intent(out) :: fftc
      integer                    , intent(in)  :: ns
      real, dimension(0:2*maxh)  , intent(out) :: wavelt
      real, dimension(-maxh:maxh), intent(out) :: twvltp
      integer                    , intent(in)  :: maxh
      integer                    , intent(in)  :: nh
      real                       , intent(in)  :: fs
      real, dimension(1:4)       , intent(in)  :: wpars

      integer   :: lfour, i, i2, iexp, npnts
      real      ::lc, lp, hp, hc, x0, f, rat, ans, q, thresh, value
      integer   :: lksign, lnseq, linc2x, linc2y
      real      :: lscale



      parameter (iexp = 2)
      parameter (thresh = 7.0)

!     Pick up the wavelet parameters.
      lc = wpars(1)
      lp = wpars(2)
      hp = wpars(3)
      hc = wpars(4)

!     First, compute the wavelet.
      x0 = sqrt(1.5 * alog(10.0))
      do 100 i = 0, ns/2
         i2 = i * 2
         f  = i * fs / ns
         rat = 0.0
         if (f .le. lp) then
            rat = iexp * x0 * (f - lp) / (lc - lp)
            ans = 0.0
         else if (f .gt. hp) then
            rat = iexp * x0 * (f - hp) / (hc - hp)
         endif
         ans = 0.0
         if(rat .le. thresh) ans = exp(-rat**2)
         fftr(i2) = ans
         fftr(i2+1) = 0.0
         fftc(i) = cmplx(ans, 0.0)
100   continue

      npnts = 2*min0(nh, maxh) + 1

!     Replace Promax FFT calls with CPS FFT calls.
      lfour = ns
      linc2x = lfour/2+1
      linc2y = lfour
      lnseq = 1
      lksign = 1
      lscale = 1.0
!!!      call ftcr1d(fft, linc2x, fft(lfour+3), linc2y, lfour, lnseq,&
!!!     &            lksign, lscale, lier)
      call matfun_fft(lksign, lfour, fftc, fftr(lfour+3:))

      call ppavo_vrmove(fftr(lfour+3:), 1, fftr, 1, lfour+2)
      call ppavo_vrmove(fftr, 1, wavelt, 1, npnts)

!     Next, compute the wavelet derivative.
      do 300 i=0, ns/2
      i2 = i * 2
      f  = i * fs / ns
      rat = 0.0
      q   = 0.0
      if (f .le. lp) then
         rat = iexp * x0 * (f - lp) / (lc - lp)
         q   = 2.0*f*rat*x0 / (lc - lp) / iexp
         ans = 0.0
      else if(f .ge. hp) then
         rat = iexp * x0 * (f - hp) / (hc - hp)
         q   = 2.0*f*rat*x0 / (hc - hp) / iexp
      endif
      ans = 0.0
      if (rat .le. thresh) ans = -(1 - q) * exp(-rat**2)
      fftr(i2) = ans
      fftr(i2+1) = 0.0
      fftc(i) = cmplx(ans, 0.0)
300   continue

!     Replace Promax FFT calls with Omega FFT calls.
      lfour = ns 
      linc2x = lfour/2+1
      linc2y = lfour
      lnseq = 1
      lksign = 1
      lscale = 1.0
      
!!!      call ftcr1d(fft, linc2x, fft(lfour+3), linc2y, lfour, lnseq,&
!!!     &     lksign, lscale, lier)
      call matfun_fft(lksign, lfour, fftc, fftr(lfour+3:))

      call ppavo_vrmove(fftr(lfour+3:), 1, fftr, 1, lfour+2)

      npnts = min0(nh, maxh)
      do 400 i=0, npnts
         value = fftr(i)
         twvltp(i) = value
         twvltp(-i) = value
400   continue

      return
      end subroutine ppavo_sabwave


!!----------------------------- ppavo_sahci ----------------------------------!!
!!----------------------------- ppavo_sahci ----------------------------------!!
!!----------------------------- ppavo_sahci ----------------------------------!!

!     Purpose:
!        This subroutine computes various hydrocarbon indicators from
!        collected weighted sums.

!     Arguments:
!        S      : Collected weighted sums.
!                 Input:  S(1,1) = S(1,SY)   = SUM { D(X) }
!                         S(1,2) = S(1,SX2Y) = SUM { D(X) * X**2 }
!                         S(1,3) = S(1,SX4Y) = SUM { D(X) * X**4 }
!                         S(1,4) = S(1,SX0)  = SUM { X**0 }
!                         S(1,5) = S(1,SX2)  = SUM { X**2 }
!                         S(1,6) = S(1,SX4)  = SUM { X**4 }
!                         S(1,7) = S(1,SX6)  = SUM { X**6 }
!                         S(1,8) = S(1,SX8)  = SUM { X**8 }
!                 Output: Zeroed, or used as scratch by the subroutine.
!        H1     : First set of inversion filter coefficients.
!        H2     : Second set of inversion filter coefficients.
!        SLOTHS : Stacking sloth which was used. Sloth is defined
!                 to be { velocity**(-2) }.
!        SLOTHI : Corresponding interval sloth. If not available, the
!                 stacking sloth may be used.
!        PARM   : Integer parameter array.
!                 PARM(1) : Hilbert transform filter length.
!                 PARM(2) : The "A" order.
!                    0 = Unity.
!                    1 = Conventional stack.
!                    2 = Zero-offset stack, quadratic fit.
!                    3 = Zero-offset stack, quartic fit.
!                 PARM(3) : The "B" order.
!                    0 = Unity.
!                    1 = Quadratic slope stack, quadratic fit.
!                    2 = Quadratic slope stack, quartic fit.
!                    3 = Quartic slope.
!                 PARM(4) : Flag for NMO stretch correction.
!                    0 = Do not correct for NMO stretch.
!                    1 = Do correct for NMO stretch.
!                 PARM(5) : Horizontal spherical divergence flag.
!                    0 = Do not correct for horizontal spherical divergence.
!                    1 = Do correct for horizontal spherical divergence.
!                 PARM(6) : Vertical spherical divergence flag.
!                    0 = Do not correct for vertical spherical divergence.
!                    1 = Do correct for vertical spherical divergence.
!                 PARM(7) : V0 for spherical divergence correction.
!                 PARM(8) : Type of output.
!                    0) AOUT = RE{AB*}, BOUT = IM{AB*}
!                    1) AOUT = RE{A},   BOUT = IM{AB*}
!                    2) AOUT = RE{A},   BOUT = RE{AB*}
!                    3) AOUT = RE{A},   BOUT = RE{B}
!                 PARM(9) : Length of the data filter.
!                 PARM(10) : AGC window length for |A| scaling.
!                            If <0, then scale by |A|**2.
!                 PARM(11) : AGC window length for |B| scaling.
!                            If <0, then scale by |B|**2.
!                 PARM(12) : Taper length in samples at both edges
!                            of the analysis window.
!                 PARM(13) : Phase angle by which to rotate the computed
!                            slopes and intercepts.
!                 PARM(14) : Rotation angle in the A-B plane, in
!                            tenths of degrees.
!                 PARM(15) : Whether to calculate AVO errors:
!                    0 = Do not calculate errors.
!                    1 = Calculate the AVO residual errors.
!                    3 = Calculate all the AVO errors.
!                    4 = Calculate the RUNS statistic.
!        OUT    : Output data.
!                 OUT(1, AOUT) : The first HCI array.
!                 OUT(1, BOUT) : The second HCI array.
!                 [0<ERFLG<4] * OUT(1, RMS)  : The AVO residual array.
!                 [1<ERFLG<4] * OUT(1, AN)   : The "A" term error array.
!                 [1<ERFLG<4] * OUT(1, BN)   : The "B" term error array.
!                 Note: If ERFLG[PARM(15)] = 0, the starred subarrays are
!                 not used, and need not be allocated.
!        MAXSMP : Dimensioned sizes of the S, SLOTHS, SLOTHI,
!                 AOUT and BOUT arrays.
!        MAXH   : Size of the H array.
!        NPARM  : Size of the PARM array.
!        FIRST  : First sample to compute.
!        LAST   : Last sample to compute.
!        NH     : Actual size of the H array.
!        T0     : Time in seconds of the first given sample.
!        ISZ    : Second dimension of S array.
!        TSAMP  : Sampling interval (seconds).
!        AVOPCT : AVO stabilization term ,used in matrix inversion.

      subroutine ppavo_sahci (s, h1, h2, sloths, slothi, parm, out,&
     &   maxsmp, maxh, nparm, first, last, nh, isz, t0, tsamp, avopct)

      integer :: out_size
      parameter (out_size = 5)

      integer                          , intent(in)    :: maxsmp
      integer                          , intent(in)    :: maxh
      integer                          , intent(in)    :: nparm
      integer                          , intent(inout) :: first
      integer                          , intent(inout) :: last
      integer                          , intent(in)    :: nh
      integer                          , intent(in)    :: isz
      real                             , intent(in)    :: t0
      real                             , intent(in)    :: tsamp
      real                             , intent(in)    :: avopct
      integer, dimension(nparm)        , intent(in)    :: parm 
      real, dimension(maxsmp, isz)     , intent(inout) :: s
      real, dimension(-maxh:maxh)      , intent(in)    :: h1
      real, dimension(-maxh:maxh)      , intent(in)    :: h2
      real, dimension(0:maxsmp+1)      , intent(in)    :: sloths
      real, dimension(0:maxsmp+1)      , intent(in)    :: slothi 
      real, dimension(maxsmp, out_size), intent(inout) :: out
 
      integer          :: sy, sx2y, sx4y, sx0, sx2, sx4, sx6, sx8, sy2
      integer          :: aout, bout, na      
      integer          :: aindex, bindex, phrot
      integer          :: dstrch, sphflg, ftrlen, vflag, errflg
      integer          :: tndx(3,3,3), dndx(3,3), order(3), key(2, 0:3)
      integer          :: ihilb, idsmth, istg, ja, j, nstg, nsa
      integer          :: n(3,3), nd(3), i, iaflg, ibflg, taper   
      real             :: pfact, dfact, time, temp, v0
      real             :: sinpr, cospr, sv2, rotab
      double precision :: p, q, dsdt, df, avo, avop, an
      double precision :: denom, eps
      logical          :: okcorr    

      parameter (sy  = 1, sx2y = 2, sx4y = 3)
      parameter (sx0 = 4, sx2  = 5, sx4  = 6)
      parameter (sx6 = 7, sx8  = 8, sy2  = 9) 
      parameter (aout = 1, bout = 2, na = 4)
      parameter (eps = 1D-40)

!         istg=         1           2           3
!         ordr=     1   2   3   1   2   3   1   2   3
!                   <-------->  <-------->  <-------->
        data tndx/  2,  4, 10,  3,  9,  7,  1,  1,  7,&   ! * sy
     &              1,  3,  9,  2,  8,  6,  1,  1,  6,&   ! * sx2y
     &              1,  1,  7,  1,  6,  5,  1,  1,  5 /   ! * sx4y

!         istg=         1           2           3
!         ordr=     1   2   3   1   2   3   1   2   3
!                   <-------->  <-------->  <-------->

        data dndx/  1,  2,  3,  2,  3,  3,  1,  1,  3 /

        data key /  6,  7,  1,  7,  1,  6,  1,  2 /

!     Figue out what to compute.
      ftrlen   = parm(1)
      order(1) = parm(2)
      order(2) = parm(3)
      dstrch   = parm(4)
      sphflg   = parm(5)
      vflag    = parm(6)
      v0       = parm(7)
      ihilb    = parm(8)
      idsmth   = parm(9)
      iaflg    = parm(10)
      ibflg    = parm(11)
      taper    = parm(12)
      phrot    = parm(13)
      rotab    = parm(14) / 10.0
      errflg   = parm(15)
      okcorr   = order(1) .ne. 0 .and. order(2) .ne. 0
      if (errflg .gt. 3) errflg = 0
      order(3) = 3

!     Add white noise to sum(x(i)**n), for n = 0, 4, 8 & for sum(y(i)**2).
      avo = 1d1 ** (-avopct / 2d1)
      avop= avo / (1d0 + avo)
      do 50 istg = sx8, sx0, -2
         ja = min0(istg, isz)
         do 40 i = first, last
            an = s(i, sx0) * avo
            s(i, istg) = s(i, ja) + an
40       continue
50    continue

!     Figure out how many stages to compute. Normally two, but if AVO
!     errors are needed, we may need three.
      nstg = 2
      if (errflg .gt. 0) nstg = order(1)

!     Derive tables of indices: n and nd.
      do 100 istg = 1, nstg
      ja = order(istg)
         if (ja .lt. 0 .or. ja .gt. 3) ja = 0
         if (ja .eq. 0) return
         do 80 j = 1, 3
            n(istg, j) = tndx(ja, istg, j)
80       continue
         nd(istg) = dndx(ja, istg)
100   continue

!     Invert the matrix and perform the AVO analysis.
      call ppavo_matv(s, out, n, nd, maxsmp,&
     &   first, last, nstg, isz, taper, avop)
      nsa = last - first + 1
      if (nsa .lt. 1) return

!     Compute the error statistics, if requested.
      if (errflg .ne. 0)&
     &   call ppavo_avoe(s, out, maxsmp, first, last, isz, out_size, nstg)

      if (errflg .gt. 1) then
         n(1, 2) = 1
         n(1, 3) = 1
         n(2, 1) = 1
         n(2, 3) = 1
         call ppavo_matv(s, out(1, na), n, nd, maxsmp,&
     &      first, last, bout, isz, taper, avop)
      endif

      do 105 istg = 1, errflg
         j = bout + istg
         call ppavo_vsqrtz (out(first, j), 1, 0.0, out(first, j), 1, nsa)
105    continue

!     Zero out unwanted parts of S.
      call ppavo_vrzero(s, 1, isz*maxsmp)

!     Copy S(1) <-- 'A'; S(2) <-- 'B', in case no filtering is required.
      call ppavo_vrmove(out(first, aout), 1, s(first, 1), 1, nsa)
      call ppavo_vrmove(out(first, bout), 1, s(first, 2), 1, nsa)

!     Now filter both AEST(T) and BEST(T) with the H1 inversion filter.
      if (order(1) .ne. 0)&
     &   call ppavo_maconv(out(first, aout), h1(-nh), s(first,1),&
     &      nsa, 2*nh+1, nh+1)
      if (order(2) .ne. 0)&
     &   call ppavo_maconv(out(first, bout), h1(-nh), s(first,2),&
     &      nsa, 2*nh+1, nh+1)

!     Do the destretching if called for: S(3) <-- A * H2(T)
      if(dstrch .eq. 1 .and. okcorr) then
         call ppavo_maconv(out(first, aout), h2(-nh), s(first,3),&
     &                    nsa, 2*nh+1, nh+1)
      endif

!     Perform in-place correction of B.
      pfact = 0.0
      dfact = 0.0
      if (okcorr) dfact = 0.5
      if (sphflg .ne. 0 .and. okcorr) pfact = 0.5

      do 400 i = first, last
         time = (i-1)*tsamp + t0
         sv2  = sloths(i) * v0**2
         p    = pfact * (2.0 - sv2)
         if (v0 .lt. 0) p = pfact * (1.0 + sv2) / sv2

!        Herb Swan modification on visit of 5/30/2003.
         q    = sloths(i) / slothi(i)
!         q = 1
 
         dsdt = (1d0*sloths(i+1) - sloths(i-1))/tsamp/2.0
         df   = dfact*(1.0 - time*dsdt/sloths(i))
         s(i,2) = (s(i,2) + p*s(i,1) + df*s(i,3))/q

400   continue

!     Correct for vertical spherical divergence effect, if so required.
      if(vflag .ne. 0) then
         do 450 i = first, last
            time = (i-1)*tsamp + t0
            p    = time / (sloths(i) * v0**2)
            s(i,1) = s(i,1) * p
            s(i,2) = s(i,2) * p
450      continue
      endif

!     Now perform a Hilbert transform of the data.
!     S(4) <-- Hilbert{S(1)}; S(5) <-- Hilbert{S(2)}.
!     Not needed if we are in AB mode.
      if (ihilb .eq. 3 .and. phrot .eq. 0.0) goto 800
      do 500 i = 1,2
         if(order(i) .eq. 0) goto 500
         call ppavo_mahlbt(s(1,i), s(1,i+3), maxsmp, i, last, ftrlen)
500   continue

!     Rotate the results in time, if called for.
      if (phrot .ne. 0) then
         cospr = cos(pi*phrot/180.0)
         sinpr = sin(pi*phrot/180.0)
         do 550  i  = first, last
            temp   = s(i,1)*sinpr + s(i,4)*cospr
            s(i,1) = s(i,1)*cospr - s(i,4)*sinpr
            s(i,4) = temp
            temp   = s(i,2)*sinpr + s(i,5)*cospr
            s(i,2) = s(i,2)*cospr - s(i,5)*sinpr
            s(i,5) = temp
550      continue
      endif

!     Rotate the results in the A-B plane, if called for.
      if (rotab .ne. 0) then
         cospr = cos(pi*rotab/180.0)
         sinpr = sin(pi*rotab/180.0)
         do 560  i  = first, last
         temp   = s(i,2)*sinpr + s(i,1)*cospr
         s(i,2) = s(i,2)*cospr - s(i,1)*sinpr
         s(i,1) = temp
         temp   = s(i,5)*sinpr + s(i,4)*cospr
         s(i,5) = s(i,5)*cospr - s(i,4)*sinpr
         s(i,4) = temp
560      continue
      endif

!     Place real and imaginary parts of A * conjugate(B) into
!     (S(6), S(7)), respectively.
      do 600 i = first, last
         s(i,6) = s(i,1)*s(i,2) + s(i,4)*s(i,5)
         s(i,7) = s(i,4)*s(i,2) - s(i,1)*s(i,5)
600   continue

!     Smooth the product terms.
      do 650 i=1,2
         call ppavo_masmth(nsa, 1, 1, s(first,i+5), s(first,8), idsmth)
650   continue

!     Normalize by |A|, if IAFLG ^= 0.
      if(iaflg .ne. 0) then
         if (iaflg .gt. 0) then
            do 710 i = first, last
               s(i,3) = sqrt(abs(s(i,1)**2 + s(i,4)**2))
710         continue
         else
            do 715 i = first, last
                s(i,3) = s(i,1)**2 + s(i,4)**2
715         continue
         endif
         call ppavo_masmth(nsa, 1, 1, s(first,3), s(first,8), iabs(iaflg))
         do 720 i = first, last
            denom = s(i,3)
            if (denom .gt. eps) then
               s(i,6) = s(i,6) / denom
               s(i,7) = s(i,7) / denom
            endif
720      continue
      endif

!     Normalize by |B|, if IBFLG ^= 0.
      if(ibflg .ne. 0) then
         if (ibflg .gt. 0) then
            do 730 i = first, last
               s(i,4) = sqrt(abs(s(i,2)**2 + s(i,5)**2))
730         continue
         else
            do 740 i = first, last
               s(i,4) = s(i,2)**2 + s(i,5)**2
740         continue
         endif
         call ppavo_masmth(nsa, 1, 1, s(first,4), s(first,8), iabs(ibflg))
         do 750 i = first, last
            denom = s(i,4)
            if (denom .gt. eps) then
               s(i,6) = s(i,6) / denom
               s(i,7) = s(i,7) / denom
            endif
750      continue
      endif

!     Now copy the selected answers to the output arrays.
800   aindex = key(1,ihilb)
      bindex = key(2,ihilb)
      call ppavo_vrmove(s(first,aindex), 1, out(first, aout), 1, nsa)
      call ppavo_vrmove(s(first,bindex), 1, out(first, bout), 1, nsa)

!     All done!

      return
      end subroutine ppavo_sahci
  
  
!!----------------------------- ppavo_sahcic ---------------------------------!!
!!----------------------------- ppavo_sahcic ---------------------------------!!
!!----------------------------- ppavo_sahcic ---------------------------------!!

!     Purpose:
!        This subroutine collects running sums for hydrocarbon detection.

!     Arguments:
!        S       : Running sums of various powers of data sin**2(phi). This
!                  array must be initialized to 0 before start of a new CDP
!                  of velocity function.
!        TRACE   : Next trace of this moved-out CDP gather.
!        SLOTHS  : Stacking sloth function which was used to NMO correct
!                  this CDP gather. Sloth is defined to be {velocity ** (-2)}.
!        SLOTHI  : Interval sloth function.
!        SCRATCH : Scratch array.
!        ISZ     : 2nd Dimension of the S array.
!        FIN     : Index of first live value in the input trace.
!        LIN     : Index of last live value in the input trace.
!        FOUT    : Index of first output sample under analysis.
!        LOUT    : Index of last output sample under analysis.
!        T0      : Time in of the sample TRACE(1) (seconds).
!        TSAMP   : Sample interval (seconds).
!        XOFF    : Shot-receiver offset for this trace (feet). For
!                  common-angle data, the angle (degrees).
!        ALIM1   : Squared sine of the minimum incidence angle.
!        ALIM2   : Squared sine of maximum incidence angle.
!        ANGFMT  : 0 for standard X-T data. 2 for common-angle data.
!        TRCFOLD : Trace fold (>= 0.0).

      subroutine ppavo_sahcic (s, trace, sloths, slothi, scratch, isz, fin,&
     &  lin, fout, lout, t0, tsamp, xoff, alim1, alim2, angfmt, trcfold)
      
      integer                         , intent(in)    :: isz
      integer                         , intent(inout) :: fin
      integer                         , intent(inout) :: lin
      integer                         , intent(in)    :: fout
      integer                         , intent(in)    :: lout
      integer                         , intent(in)    :: angfmt
      real                            , intent(in)    :: t0
      real                            , intent(in)    :: tsamp
      real                            , intent(in)    :: xoff
      real                            , intent(in)    :: alim1
      real                            , intent(in)    :: alim2
      real                            , intent(in)    :: trcfold
      real, dimension(lin)            , intent(in)    :: trace
      real, dimension(0:lin+1)        , intent(in)    :: sloths

!     Herb Swan modification on visit of 5/30/2003.  
      real, dimension(0:lin+1)        , intent(in)    :: slothi
  
      real, dimension(fout:lout, isz) , intent(inout) :: s
      double precision, dimension(lin), intent(inout) :: scratch

      integer          :: sy, sx2y, sx4y, sx0, sx2, sx4, sx6, sx8, sy2
      integer          :: i, flv, llv, newfin, newlin
      real             :: tr, x2p, time, radian
      double precision :: x2, s0, s2, s4, s6, s8, rad
 
      parameter (sy  = 1, sx2y = 2, sx4y = 3)
      parameter (sx0 = 4, sx2  = 5, sx4  = 6)
      parameter (sx6 = 7, sx8  = 8, sy2  = 9) 
      parameter (radian = 57.29578)

      newfin = 0
      newlin = -1
      x2 = xoff * xoff

!     Find the first and last live values. This is necessary because
!     ppavo_SAMVOT does not compute moved-out live values.
      do 50 i = fin, lin
         flv = i
         if (trace(i) .ne. 0.0) goto 60
50    continue

60    do 80 i = lin, fin, -1
         llv = i
         if(trace(i) .ne. 0.0) goto 100
80    continue
100   continue

      if (flv .lt. fout) flv = fout
      if (llv .gt. lout) llv = lout

!     Handle X-T data.

!     The following vectorizable loop places sine-squared of the
!     incidence angle into the scratch array, or "-1.0" outside
!     the requested angle range.
      if (angfmt .eq. 0) then
         do 150 i = flv, llv
            time = (i-1)*tsamp + t0
            x2p  = x2 * sloths(i)
            s2   = 1.0
            if (time .gt. 0) s2 = x2p / (x2p + time*time)

!           Herb Swan modification on visit of 5/30/2003.
!            s2 = s2 * sloths(i) / slothi(i)

            scratch(i) = s2
            if (real(s2) .gt. alim2 .or. real(s2) .lt. alim1)&
     &         scratch(i) = -1.0
150      continue

!     Handle common-angle data.
      else
         rad = xoff / radian
         s2  = sin(rad) ** 2
         if (s2 .gt. alim2 .or. s2 .lt. alim1) then
            flv = lin
            llv = fin
            newfin = flv
            newlin = llv
         endif
         do 160 i = flv, llv
            scratch(i) = s2
160      continue
      endif

!     Common code for both X-T and common-angle data.

!     The following non-vectoriable loop searches for the first and last
!     live values within the mute zone.
      do 300 i = flv, llv
         if(scratch(i) .lt. 0.0) then
            if(newfin .gt. 0) goto 350
         else
            if(newfin .eq. 0) newfin = i
         newlin = i
         endif
300   continue
350   continue

!     The following vectorizable loop actually does the data collection,
!     over the range of data within the mute zone.
      do 400 i = newfin, newlin
         tr = trace(i)
         s0 = 1.0
         s2 = scratch(i)
 
!        Herb Swan modification on visit of 5/30/2003.
!         if(s2 .lt. 0.0) goto 400

         s4 = s2 * s2
         s6 = s2 * s4
         s8 = s4 * s4
         s(i, sy)   = s(i, sy)   + trcfold * tr
         s(i, sx2y) = s(i, sx2y) + trcfold * tr * s2
         s(i, sx4y) = s(i, sx4y) + trcfold * tr * s4
         s(i, sx0)  = s(i, sx0)  + trcfold * s0
         s(i, sx2)  = s(i, sx2)  + trcfold * s2
         s(i, sx4)  = s(i, sx4)  + trcfold * s4
         s(i, sx6)  = s(i, sx6)  + trcfold * s6
         s(i, sx8)  = s(i, sx8)  + trcfold * s8
         s(i, sy2)  = s(i, sy2)  + trcfold * tr * tr

400   continue

!     return the new first and last live values with the mute zone.
!     store these last, since they are used to dynamically dimension
!     dummy arrays!
      fin = newfin
      lin = newlin

      return
      end subroutine ppavo_sahcic
      
      
!!------------------------------ ppavo_sahcsc --------------------------------!!
!!------------------------------ ppavo_sahcsc --------------------------------!!
!!------------------------------ ppavo_sahcsc --------------------------------!!

!     Purpose:
!        This subroutine scales the trace to have a unity RMS value
!        if ifrst .eq. 0. It then sets ifrst = 1 for the next pass, and
!        returns the scale factor it used in scale. Otherwise, it scales
!        the trace based upon the same scale factor passed in scale.

!     Arguments:
!        TRIN  : Input trace, output scaled trace.
!        NSIN  : Number of samples in the input trace.
!        IFLV  : Index of the first live trace.
!        SCALE : Scale factor to apply to the trace.
!        IFRST : Flag denoting whether this is the first trace.
    
      subroutine ppavo_sahcsc (trin, nsin, iflv, scale, ifrst)

      integer               , intent(in)    :: nsin, iflv
      integer               , intent(inout) :: ifrst
      real                  , intent(inout) :: scale
      real, dimension(nsin) , intent(inout) :: trin

      integer          :: i      
      double precision :: sum

      if (ifrst .eq. 0) then
         if(iflv .ge. nsin) return
         sum = 0.0
         do 100 i=iflv, nsin
            sum = sum + trin(i)**2
100      continue
         if (sum .ne. 0) then
            scale = 1.0 / dsqrt(sum)
         else
            scale = 1.0
         endif
         ifrst = 1
      endif

      do 200 i=iflv, nsin
         trin(i) = trin(i) * scale
200   continue

      return
      end subroutine ppavo_sahcsc


!!----------------------------- ppavo_sahgram --------------------------------!!
!!----------------------------- ppavo_sahgram --------------------------------!!
!!----------------------------- ppavo_sahgram --------------------------------!!

!     Purpose:
!        Instead of scaling color plots based on the maximum absolute
!        value (option 'Z'), this routine allows a certain predetermined
!        percentage to be clipped.   This provides a more robust scaling
!        algorithm.

!     Arguments:
!        IGRAM  : Data histogram (integer format).
!        HGRAM  : Data histogram (real format).
!        HSIZE  : Histogram length.
!        KPWKS2 : Pointer to file containing data.
!        KPWKD2 : Pointer to file containing data.
!        NRECS  : Number of sets of trace in work file.
!        ISTRID : Trace stride of work file.
!        IOFF   : Offset of traces in work file.
!        OH     : Scratch array for trace headers.
!        OTR    : Scratch array for trace data.
!        THL    : Trace header length.
!        NS     : Trace length.
!        USCALE : Percentage of data values to be clipped.
!        ASCALE : Maximum absolute value of input data;
!                 Smallest output data value greater than 'USCALE' % of
!                 the absolute values in the file.
!        BIN    : Largest bin number (out of 'HSIZE' bins) to be plotted;
!                 Set to -1 on error.
!        DDP1HD : Scratch trace header array.
!        DDP1TR : Scratch trace data array.
    
      subroutine ppavo_sahgram (obj, igram, hgram, hsize, kpwrks, kpwrkd, &
     &   nrecs, istrid, ioff, oh, otr, thl, ns, uscale, ascale, bin, &
     &   ddp1hd, ddp1tr, ier)

      type(ppavo_struct), pointer                              :: obj
      integer                                  , intent(in)    :: hsize
      integer                                  , intent(inout) :: kpwrks
      integer,          dimension(2)           , intent(inout) :: kpwrkd
      integer                                  , intent(in)    :: nrecs
      integer                                  , intent(in)    :: istrid
      integer                                  , intent(in)    :: ioff
      integer                                  , intent(inout) :: thl
      integer                                  , intent(inout) :: ns
      real,             dimension(ns)          , intent(inout) :: ddp1tr
      double precision, dimension(thl)         , intent(inout) :: ddp1hd
      real                                     , intent(inout) :: uscale
      real                                     , intent(inout) :: ascale
      integer                                  , intent(out)   :: bin
      integer,          dimension(-hsize:hsize), intent(out)   :: igram
      real,             dimension(-hsize:hsize), intent(out)   :: hgram
      real,             dimension(ns)          , intent(out)   :: otr
      double precision, dimension(ns)          , intent(out)   :: oh
      integer                                  , intent(inout) :: ier

      integer          :: rec, i, rectmp
      real             :: scale, value, sval
      double precision :: total, sum, big

!     Initialization.
      ier = 0
      if(ascale .le. 0.0) goto 1000
      scale = hsize**2 / ascale
      call ppavo_vzero(igram(-hsize:), 1, 2*hsize + 1)
      rec   = 1
      total = 0

!     Begin reading the file.
      do while (rec .le. nrecs)
         rectmp = ioff + (rec - 1) * istrid

         call ppavo_uhci_read(obj, kpwrks, kpwrkd, rectmp, otr, oh,&
     &                        thl, ns, ddp1hd, ddp1tr, ier)
         rec = rec + 1

!        Take square root of trace to increase dynamic range (vector operation).
         do 50 i = 1, ns
            value = otr(i) * scale
            sval  = sqrt(abs(value))
            if(value .lt. 0.0) sval = -sval
50       otr(i) = sval

!        Build the histogram statistics. Don't include values falling
!        within bin 0 in the total count.
         do 100 i=1, ns
            bin = otr(i)
            if(iabs(bin) .gt. hsize) bin = isign(hsize, bin)
            igram(bin) = igram(bin) + 1
            if(bin .ne. 0) total = total + 1d0
100      continue
      enddo

!     Scan the histogram, looking for the biggest value and
!     for overflow. Overlook the spike at 0.
      big = 0d0
      do 150 i=-hsize, hsize
         if(i .ne. 0) then
            if(igram(i) .gt. big) big = igram(i)
            if(igram(i) .lt. 0) goto 1000
         endif
150   continue

!     Scan the histogram to determine the data scaling level.
      bin = 0
      sum = 0
      total = total * (100.0 - uscale) / 1d2
      do 200 i=1, hsize
         bin = i
         if(sum .ge. total) goto 220
         sum = sum + igram(bin) + igram(-bin)
200   continue


!     Generate the scaled histogram.
220   do 250 i=-hsize, hsize
         value = igram(i)
         if(value .gt. big) value = big
250   hgram(i) = value / big

!     Compute the data scaling level.
      ascale = bin**2 / scale
      return

!     Integer overflow - no histogram computed.
1000  bin = -1

      return
      end subroutine ppavo_sahgram


!!------------------------------ ppavo_samodv --------------------------------!!
!!------------------------------ ppavo_samodv --------------------------------!!
!!------------------------------ ppavo_samodv --------------------------------!!

!     Purpose:
!        This subroutine modifies the velocity traces on a new iteration.

!     Arguments:
!        VELS   : Previous unsmoothed velocities and modified velocities.
!        SCR    : Scratch array.
!        NS     : Length of all arrays.
!        IVSMTH : Half-length smoothing length.

      subroutine ppavo_samodv (vels, scr, ns, ivsmth)

      integer            , intent(in)    :: ns
      integer            , intent(in)    :: ivsmth
      real, dimension(ns), intent(inout) :: vels
      real, dimension(ns), intent(inout) :: scr

      integer :: i

!     Convert velocities to sloths (like AVEL does).
      do 100 i = 1, ns
         scr(i) = 1.0 / vels(i)**2
100   continue

!     Smooth the sloths.
      call ppavo_masmth(ns, 1, 1, scr, vels, ivsmth)

!     Convert the smoothed sloths back to velocities.
      do 200 i = 1, ns
         vels(i) = 1.0 / sqrt(vels(i))
200   continue

      return
      end subroutine ppavo_samodv


!!----------------------------- ppavo_samvot ---------------------------------!!
!!----------------------------- ppavo_samvot ---------------------------------!!
!!----------------------------- ppavo_samvot ---------------------------------!!

!     Purpose:
!        This subroutine normal moveout corrects a trace, using a
!        prescribed stacking velocity profile. It differs from the
!        ProMAX routine NMO_APPLY in that it does not require the
!        caller to move out the entire trace, but only a part of it.
!        This can save time. It also supports Tau-P data, but this
!        feature is not used here.

!     Arguments:
!        TRIN   : Uncorrected seismic trace, padded with NCOEF zeros for
!                 indices in the range (1-NCOEF:0) and (NSIN+1:NSIN+NCOEF).
!        SLOTH  : Prescribed moveout sloth function. Sloth is defined
!                 as {velocity ** (-2)}.
!        COEF   : Interpolation coefficients ala C. Sickling.
!        TROUT  : NMO-correction output trace.
!        TRCOEF : Work area for fully vectorized trace interpolation scheme.
!        IFRC   : Another work array.
!        NSIN   : Length of the TRIN array.
!        NCOEF  : Size of the TRCOEF and COEF arrays.
!        FSA    : Index in output trace of first sample to be moved out.
!        LSA    : Index in output trace of last sample to be moved out.
!        XOFF   : For X-T data   (TPFLG=0): the shot receiver offset (feet).
!                 For Tau-P data (TPFLG=1): the P value (seconds/feet).
!        IFLV   : Index of first live sample of this trace.
!        T0     : Time corresponding to TRIN(1) (seconds).
!        TSAMP  : Sampling interval (seconds).
!        TPFLG  : 0 for X-T data. 1 for Tau-P data.
!        NFLV   : Index of first live value after moveout.
!        NLLV   : Index of last live value after moveout.

      subroutine ppavo_samvot (trin, sloth, coef, trout, trcoef, ifrc,&
     &   nsin, ncoef, fsa, lsa, xoff, iflv, t0, tsamp, tpflg, nflv, nllv)

      integer                            , intent(in)    :: nsin
      integer                            , intent(in)    :: ncoef
      integer                            , intent(in)    :: fsa
      integer                            , intent(in)    :: lsa
      integer                            , intent(in)    :: iflv
      integer                            , intent(in)    :: tpflg
      integer                            , intent(inout) :: nflv
      integer                            , intent(inout) :: nllv
      real                               , intent(in)    :: xoff
      real                               , intent(in)    :: t0
      real                               , intent(in)    :: tsamp
      real, dimension(1-ncoef:nsin+ncoef), intent(in)    :: trin
      real, dimension(0:nsin+1)          , intent(in)    :: sloth
      real, dimension(ncoef, 100)        , intent(inout) :: coef
      real, dimension(lsa)               , intent(inout) :: trout
      real, dimension(fsa:lsa, ncoef, 2) , intent(inout) :: trcoef
      real, dimension(fsa:lsa)           , intent(inout) :: ifrc

      integer :: nc2, i, k, itndx, n
      integer :: jtmp, islen, nsa
      real    :: time, displ
      real    :: temp, x2, gain

      parameter (displ = 0.495)

      islen = 0
      nc2 = ncoef/2
      x2  = xoff * xoff
      nsa = lsa - fsa + 1
      call ppavo_vrzero (trcoef, 1, 2*nsa*ncoef)

!     Insert the last set of coefficients.
      if (ncoef .ne. islen) then
         islen = ncoef
         do 200 i = 1, ncoef
            temp = 0.0
            if(i .eq. nc2+1) temp = 1.0
            coef(i, 100) = temp
200      continue
      endif

!     Compute the new times for Tau-P data.
      if (tpflg .eq. 1) then
         do 250 k = fsa, lsa
            time = (k-1)*tsamp + t0
            temp = 1d0 - x2/sloth(k)
            if(temp .lt. 0) temp = 0
            trout(k) = (time * sqrt(temp) - t0)/tsamp - nc2 + displ
250      continue

!     Compute the new times for X-T data.
      else
         do 300 k = fsa, lsa
            time = (k-1)*tsamp + t0
            temp = time**2 + x2*sloth(k)
            trout(k) = (sqrt(temp) - t0)/tsamp - nc2 + displ
300      continue
      endif

!     Compute the fractions.
      do 380 k = fsa, lsa
         gain = trout(k)
!        itndx = gain  + ncoef + 0.5
!        itndx = itndx - ncoef
         itndx = gain + 0.5
         itndx = itndx
         trout(k) = nsin + 0.5
         if (itndx .lt. nsin .and. itndx .gt. iflv - ncoef)&
     &      trout(k) = itndx
         temp     = 1e2 * (gain - itndx) + 51
         if (temp .lt. 1.0) temp = 1.0
         if (temp .gt. 100.0) temp = 100.0
         ifrc(k) = temp
380   continue

!     Prepare to interpolate the trace at these new times.
!     Note: the 'n=i" statement prevents vectorization on the wrong loop.
      nflv = 0
      nllv = nflv
      do 400 i = 1, ncoef
         n = i
         do 400 k = fsa, lsa
            itndx = i + trout(k)
            jtmp  = ifrc(k)
            trcoef(k,n,1) = coef(i, jtmp)
            trcoef(k,n,2) = trin(itndx)
400   continue

!     Get the range of indices to compute.
      do 410 k = fsa, lsa
         nflv = k
         if(trcoef(k, nc2+1, 1) .ne. 0.0) goto 420
410   continue

420   do 430 k = lsa, fsa, -1
         nllv = k
         if(trcoef(k, nc2+1, 1) .ne. 0.0) goto 450
430   continue

!     Do the actual interpolation.
450   continue
      call ppavo_vrzero (trout(fsa), 1, nsa)

      do 500 i = 1, ncoef
         do 500 k = nflv, nllv
            trout(k) = trout(k) + trcoef(k,i,1)*trcoef(k,i,2)
500   continue

!     Scale the data by cos(angle) for Tau-P data.
      if (tpflg .eq. 1) then
         do 600 k = nflv, nllv
            temp = 1d0 - x2/sloth(k)
            if(temp .le. 0) temp = 0
            gain = sqrt(temp)
            trout(k) = trout(k) * gain
600      continue
      endif    
      
      return
      end subroutine ppavo_samvot


!!----------------------------- ppavo_sanewv ---------------------------------!!
!!----------------------------- ppavo_sanewv ---------------------------------!!
!!----------------------------- ppavo_sanewv ---------------------------------!!

!     Purpose:
!        This subroutine computes the revised stacking velocities,
!        based upon the smoothed imaginary part of the correlation
!        coefficient between 'A' and 'B*'.
!
!     Arguments:
!        DV     : In  -> IM{AB*} / |A|**2.
!                 Out -> Fractional change in velocity.
!        VRMS   : In  -> previous RMS velocity.
!                 Out -> revised RMS velocity.
!        NS     : Number of samples in 'DV' array.
!        IT0    : Time of first sample (msec).
!        FS     : Sampling frequency (Hz).
!        WF0    : Wavelet center frequency (Hz).
!        Q      : Q factor of the media.
!        FSA    : First sample under analysis.
!        LSA    : Last sample under analysis.
!        MINIV  : Minimum allowed interval velocity.
!        MAXIV  : Maximum allowed interval velocity.
!        MINPCT : Minimum allowed % decrement in interval velocity.
!        MAXPCT : Maximum allowed % increment in interval velocity.
!        MINDV  : Minimum allowed % decrement in stacking velocity.
!        MAXDV  : Maximum allowed % increment in stacking velocity.
!        MINSV  : Minimum allowed stacking velocity.
!        MAXSV  : Maximum allowed stacking velocity.
!        VTIME  : Velocity cutoff time (msec).
!        VPOW   : Velocity cutoff sharpness.
!        STATUS : Error flag.
!                 0 = No error.
!                 1 = Stacking velocity too low.
!                 2 = Stacking velocity too high.
!                 3 = Interval velocity too low.
!                 4 = Interval velocity too high.
!        REFT   : Time the error occurred.

      subroutine ppavo_sanewv (dv, vrms, ns, it0, fs, wf0, q, fsa, lsa,&
     &   miniv, maxiv, minpct, maxpct, mindv, maxdv, minsv, maxsv,&
     &   vtime, vpow, status, reft)

      integer,             intent(in)    :: ns
      integer,             intent(in)    :: it0
      integer,             intent(in)    :: fsa
      integer,             intent(in)    :: lsa
      integer,             intent(in)    :: miniv
      integer,             intent(in)    :: maxiv
      integer,             intent(in)    :: minsv
      integer,             intent(in)    :: maxsv
      integer,             intent(in)    :: vtime
      integer,             intent(in)    :: vpow
      integer,             intent(out)   :: status
      real,                intent(in)    :: fs
      real,                intent(in)    :: wf0
      real,                intent(in)    :: q
      real,                intent(in)    :: minpct
      real,                intent(in)    :: maxpct
      real,                intent(in)    :: mindv
      real,                intent(in)    :: maxdv
      real,                intent(out)   :: reft
      real, dimension(ns), intent(inout) :: dv
      real, dimension(ns), intent(inout) :: vrms

      integer          :: i
      real             :: maxacc, pi2, rflag
      real             :: pcup, pcdn, bulk, qp, vts, vp
      double precision :: bvm, fp, wt, vsmin, vsmax, prevv2, newv2, dvn
      double precision :: t0, t0p, op2, vlim2, oldv2, vi2, previ, previ2, rate
      double precision :: minsv2, maxsv2, miniv2, maxiv2, fsd

      parameter (maxacc=100.0)      
      parameter (pi2=6.2832)
      parameter (rflag=1.5)

!     If no center frequency, or no velocities, bail out now.
      status  = 0                             ! no errors so far
      reft    = 0.0
      if (wf0 .le. 0.0 .or. vrms(1) .le. rflag) then
         call ppavo_vfill(1.0, dv, 1, ns)
         return
      endif  

!     Compute the raw correction (vectorizable).
      bulk    = 1e-3*it0                      ! bulk time shift (sec)
      qp      = pi2 / (16.0 * q)              ! scaled inverse q
      pcup    = 1.0 + maxdv*1e-2              ! largest frac increment
      pcdn    = 1.0 - mindv*1e-2              ! largest frac decrement
      vts     = 1e-3*vtime                    ! cutoff time (s)
      vp      = 1e-1*vpow                     ! sharpness
      fsd     = fs                            ! double precision fs
      do 100 i=fsa+1, lsa
         t0  = (i-1)/fsd + bulk              ! time of sample (sec)
         bvm = qp * t0 * wf0                 ! "beta" x old cent freq  (hz)
         fp  = sqrt(1d0 + bvm**2) - bvm      ! (new cent freq)/(old cent freq)
         fp  = pi2 * fp * wf0                ! new cent freq  (rad/sec)
         wt  = t0 * fp                       ! w0 * t 
         dvn = dv(i) / wt                    ! dv / v
         dvn = dvn / (1 + (t0/vts)**vp)      ! apply the cutoff
         dvn = 1d0 + dvn                     ! 1 + dv/v
         if (dvn .gt. pcup) dvn = pcup       ! put limits on it
         if (dvn .lt. pcdn) dvn = pcdn
         dv(i)   = dvn                       ! velocity multiplier
100   continue

      dv(fsa) = dv(fsa+1)                     ! extend backwards

!     Compute limits on squared velocities.
      minsv2 = dble(minsv)**2                 ! minimum stacking vel
      maxsv2 = dble(maxsv)**2                 ! maximum stacking vel
      miniv2 = dble(miniv)**2                 ! minimum interval vel
      maxiv2 = dble(maxiv)**2                 ! maximum interval vel

!     Compute the constrained first squared velocities.
      op2    = dble(vrms(fsa))**2             ! original stacking vel
      op2    = max(op2, miniv2, minsv2)       ! constrain it
      op2    = min(op2, maxiv2, maxsv2)
      prevv2 = op2 * dble(dv(fsa))**2         ! new stacking vel
      rate   = 2d3 * maxacc / fsd             ! max change in vint
      previ2 = prevv2                         ! keep compiler happy
      previ  = sqrt(previ2)

!     Check conformance with constraints (non-vectorizable, recursive).
      do 200 i  = fsa+1, lsa

         t0    = (i-1)/fsd + bulk            ! this time
         t0p   = (i-2)/fsd + bulk            ! previous time
         oldv2 = dble(vrms(i))**2            ! original vrms

!        Make sure the old stacking velocity lies within limits.
         if (oldv2 .lt. minsv2) then         ! check constraints
            oldv2 = minsv2
            if (status .eq. 0) then
               status = 1
               reft   = t0
            endif
         endif
         if (oldv2 .gt. maxsv2) then
            oldv2 = maxsv2
            if (status .eq. 0) then
               status = 2
               reft   = t0
            endif
         endif

!        Compute the old interval velocity, checking its conformance.       
         vi2   = (t0*oldv2 - t0p*op2) * fsd  ! original vint**2
         if (vi2 .lt. miniv2) then
            vi2 = miniv2
            if (status .eq. 0) then
               status = 3
               reft   = t0
            endif
         endif
         if (vi2 .gt. maxiv2) then
            vi2 = maxiv2
            if (status .eq. 0) then
               status = 4
               reft   = t0
            endif
         endif

!        If second sample, remember previous interval velocity.
         if (i .eq. fsa+1) then
            previ2 = vi2
            previ  = sqrt(previ2)
         endif

!        Compute the new stacking vel, making sure it's not too small.
         vlim2 = vi2 * (1 - 1d-2*minpct)**2  ! floor for new vint**2
         vlim2 = max(vlim2, miniv2, previ2 - rate*previ)
         vsmin = prevv2 + (vlim2 - prevv2)/(fsd*t0)
         vsmin = max(vsmin, minsv2)
         newv2 = oldv2 * dble(dv(i))**2      ! new vrms**2
         if (newv2 .lt. vsmin) then
            newv2 = vsmin
            dv(i) = sqrt(newv2) / vrms(i)
         endif

!        Make sure it's not too large.
         vlim2 = vi2 * (1 + 1d-2*maxpct)**2  ! ceil for new vint**2
         vlim2 = min(vlim2, maxiv2, previ2 + rate*previ)
         vsmax = prevv2 + (vlim2 - prevv2)/(fsd*t0)
         vsmax = min(vsmax, maxsv2)
         if (newv2 .gt. vsmax) then
            newv2 = vsmax
            dv(i) = sqrt(newv2) / vrms(i)
         endif

!        Compute the new interval velocity, subjected to constraints.
!        Remember the new stacking velocity. Also remember the original
!        stacking velocity.
         previ2 = (t0*newv2 - t0p*prevv2) * fsd ! new vint**2
         prevv2 = newv2                         ! new vrms**2
         op2    = oldv2                         ! old vrms**2
         previ  = sqrt(previ2)                  ! new vint
200   continue

!     Extend the extrema. Apply the corrections to vrms.
      call ppavo_vfill(dv(fsa), dv, 1, fsa)
      call ppavo_vfill(dv(lsa), dv(lsa:), 1, ns-lsa+1)
      call ppavo_vmul(dv, 1, vrms, 1, vrms, 1, ns)
      
      return
      end subroutine ppavo_sanewv


!!----------------------------- ppavo_sarkft ---------------------------------!!
!!----------------------------- ppavo_sarkft ---------------------------------!!
!!----------------------------- ppavo_sarkft ---------------------------------!!

!     Purpose:
!        This subroutine generates H1(t) and H2(t) inversion filters
!        for NMO destretching, specifically for a Ricker wavelet.

!     Method:
!        Using the functional form for a Ricker wavelet:
!           W(T) = (1 - 0.5 * W0T^2) * exp(-0.25 * W0T^2)
!        the auto-correlation of W(T) and cross-correlation between
!        W(T) and TW'(T) are explicitly evaluated. The inversion
!        filters are then obtained through conventional Levinson
!        inversions.

!     Restrictions: (checked by IER flag)
!        1) PCTCNS > 0.0
!        2) NH <= MXH

!     Arguments:  
!        MXH    : Dimensioned size of output inversion filters.
!        NH     : Actual size of output inversion filters.
!        W0T    : Omega 0 times the time step size (unitless).
!        PCTWNS : Additional white noise percentage.
!        PCTCNS : Additional colored noise percentage.
!        A      : Auto-regressive noise coefficient.
!                 RN(N) = PCTCNS * .01 * RW(0) * A ** N
!        KS     : Second dimension of work array.
!        H1,H2  : Inversion filters.
!        CORR   : Auto-correlation of the wavelet.
!        CVECT  : Cross-correlation vector between W(T) and TW'(T).
!        WORK   : Work array.
!                 KS = 1: Cross-correlation between W(T) and TW'(T).
!                 KS > 1: Work area for subroutine ppavo_swlev.
!                 Warnings:
!                 1) This array must be aligned on a double word boundary.
!                 2) Since this array is passed as XCOM(WORK), it is
!                    declared to be single precision within ppavo_sarkft
!                    in order to prevent an inter-compilation conflict.
!                    This is acceptable, individual elements of WORK are
!                    never explicitly referenced, only passed to ppavo_swlev.
!                    Beware that it should never be explicitly referenced.
!        IER    : Error flag.
!                 0 = Normal completion.
!                 1 = Auto-correlation matrix was singular.
!                 2 = MXH was not large enough.      

      subroutine ppavo_sarkft (h1, h2, corr, cvect, work, mxh, nh,&
     &   w0t, pctwns, pctcns, a, ks, ier)

      integer                            , intent(out)   :: ier
      integer                            , intent(in)    :: ks
      integer                            , intent(in)    :: mxh
      integer                            , intent(in)    :: nh
      real                               , intent(in)    :: w0t
      real                               , intent(in)    :: pctwns
      real                               , intent(in)    :: pctcns
      real,  dimension(-mxh:mxh)         , intent(out)   :: cvect
      real,  dimension(-mxh:mxh)         , intent(out)   :: h1
      real,  dimension(-mxh:mxh)         , intent(out)   :: h2
      real,  dimension(0:2*mxh)          , intent(out)   :: corr
      real(kind=8),dimension(-mxh:mxh,ks), intent(out)   :: work
      double precision                   , intent(in)    :: a
  
      integer          :: i
      integer          :: lag
      integer          :: maxlag
      integer          :: nwork
      real             :: thresh
      real             :: c0, c1
      real             :: t, t2, temp
      double precision :: an

      parameter (thresh = 200.0)

!     Check the array sizes.
      ier = 0
      maxlag = 2*nh
      nwork = ks * (maxlag + 1)
      if (pctwns .le. 0.0) ier = 1
      if (nh .gt. mxh) ier = 2
      if (abs(a) .ge. 1.0) ier = 3
      if (ier .ne. 0) return

!     Zero out the full H1 and H2 arrays.
      do 100 lag = -mxh, mxh
         h1(lag) = 0.0
         h2(lag) = 0.0
100   continue

!     Compute the wavelet's auto-correlation function.
      do 200 lag = 0, maxlag
         t = lag * w0t
         t2 = t*t
         corr(lag) = 0.0
         if (t2 .lt. thresh) then
            corr(lag) = (48 - (24 - t2)*t2) * exp(-t2/8)
         endif
200   continue

!     Compute the H1(T) inversion filter.
      call ppavo_vrmove(corr, 1, cvect(0), 1, nh+1)
      call ppavo_vrmove(corr, 1, cvect(-nh), 1, nh+1)
      call ppavo_vrrvrs (cvect(-nh), 1, nh+1)

      c0 = corr(0) * pctwns * 1e-2
      c1 = corr(0) * pctcns * 1e-2
      corr(0) = corr(0) + c0
      an = c1

      do 350 i = 0, maxlag
         corr(i) = corr(i) + an
         an      = an * a
350   continue

      call ppavo_swlev(corr, 1, cvect(-nh), 1, h1(-nh), 1,&
     &                maxlag+1, work, nwork)

!     Compute the cross-correlation between W(T) and TW'(T)
      do 500 lag = -nh, nh
         t = lag * w0t
         t2 = t*t
         temp = 0.0
         if (t2 .le. thresh) then
            temp = -(((t2 - 36)*t2 + 144)*t2 + 192) * exp(-t2/8)/8
         endif
         cvect(lag)  = temp
500   continue

!     Compute the H2(T) filter.
      call ppavo_swlev(corr, 1, cvect(-nh), 1, h2(-nh), 1,&
     &                maxlag+1, work, nwork)
      an = c1
      corr(0) = corr(0) - c0
      do 600 i=0, maxlag
         corr(i) = corr(i) - an
         an      = an * a
600   continue
        
      return
      end subroutine ppavo_sarkft


!!----------------------------- ppavo_saruns ---------------------------------!!
!!----------------------------- ppavo_saruns ---------------------------------!!
!!----------------------------- ppavo_saruns ---------------------------------!!

!     Purpose:
!        This subroutine collects the raw data to later compute
!        the RUNS statistic.

!     Arguments:
!        TRACE : Moved out pre-stack trace.
!        A     : "A" trace from a previous run.
!        B     : "B" trace from a previous run.
!        SLOW  : 1/(stacking velocity)**2.
!        RUNS  : RUNS statistic.
!        NSA   : Number of samples under analysis.
!        XOFF  : Shot-receiver offset.
!        ALIM1 : Squared sine of minimum incidence angle.
!        ALIM2 : Squared sine of maximum incidence angle.
!        TS    : Sampling interval (seconds).
!        T0    : Time of first sample (seconds).

      subroutine ppavo_saruns (trace, a, b, slow, runs, nsa, xoff,&
     &                        alim1, alim2, ts, t0)

      integer                  , intent(in)    :: nsa
      integer, dimension(nsa,3), intent(inout) :: runs
      real, dimension(nsa)     , intent(in)    :: trace
      real, dimension(nsa)     , intent(in)    :: a
      real, dimension(nsa)     , intent(in)    :: b
      real, dimension(nsa)     , intent(in)    :: slow
      real                     , intent(in)    :: xoff
      real                     , intent(in)    :: alim1
      real                     , intent(in)    :: alim2
      real                     , intent(in)    :: ts
      real                     , intent(in)    :: t0

      integer :: i, npos, nneg, nrun 
      real    :: value, time
      real(kind=8)  :: x2p, s2, model, resid

      do i = 1, nsa
!     Compute the preliminaries.
         value = trace(i)
         if (value .eq. 0) cycle
         time  = (i-1)*ts + t0
         x2p   = xoff * xoff * slow(i)
         s2    = 0.0
         if (time .gt. 0) s2 = x2p / (x2p + time*time)
         if (s2 .lt. alim1) cycle
         if (s2 .gt. alim2) cycle
!     Test the sign of the residual.
         model = a(i) + b(i)*s2
         resid = value - model
         if (resid < 0 ) then
           if (runs(i, nrun) .ge. 0) runs(i, nrun) = -runs(i, nrun) - 1
           runs(i, nneg) = runs(i, nneg) + 1
         elseif (resid > 0 ) then
           if (runs(i, nrun) .le. 0) runs(i, nrun) = -runs(i, nrun) + 1
           runs(i, npos) = runs(i, npos) + 1
         endif
      end do  
  
      return
      end subroutine ppavo_saruns


!!----------------------------- ppavo_sauavga --------------------------------!!
!!----------------------------- ppavo_sauavga --------------------------------!!
!!----------------------------- ppavo_sauavga --------------------------------!!

!     Purpose:
!        This subroutine computes running weighted averages of
!        <|A|**2>, <|B|**2>, and <AB*>, for use in the optimal
!        hydrocarbon AVO indicator.

!     Arguments:
!        ASUM   : Vector sum of |A|^2.
!        BSUM   : Vector sum of |B|^2.
!        ABSUMR : Vector sum of Re{AB*}.
!        ABSUMI : Vector sum of Im{AB*}.
!        WTS    : Vector sum of weights.
!        AR     : In  -> Zero-offset vector Re{A}.
!                 Out -> Transformed attribute.
!        AI     : In  -> Zero-offset vector Im{A}.
!                 Out -> Transformed attribute.
!        BR     : In  -> AVO gradient vector Re{B}.
!                 Out -> Transformed attribute.
!        BI     : In  -> AVO gradient vector Im{B}.
!                 Out -> Transformed attribute.
!        W      : In  -> Given weighting function.
!                 Out -> W \ (|A|^2 + |B|^2), if COROPT = 1.
!        SCR1   : Scratch array #1.
!        SCR2   : Scratch array #2.
!        NS     : Dimensioned size of arrays.
!        FSA    : First sample under analysis.
!        LSA    : Last sample under analysis.
!        NTWIN  : Number of samples in time window.
!        FAC    : Scale factor to apply to running sums.
!        COROPT : Weighting to apply to data:
!                 2 -> Use alternate method to get R.
!                 1 -> L2 norm with 1/SQRT(|A|^2 + |B|^2) WTS.
!                 0 -> L2 norm with no weights.
!        ALPHA  : Power of the norm if nonzero.
!        ICDPN  : Trace number being averaged.
!        IDB    : Debug level (print diagnostic information if >= 2).
!        IPR    : Fortran print unit number.

      subroutine ppavo_sauavga (asum, bsum, absumr, absumi, wts, &
     &                          ar, ai, br, bi, w, scr1, scr2, &
     &                          ns, fsa, lsa, ntwin, fac, coropt,&
     &                          icdpn, idb, ipr)

      integer,                         intent(in)    :: ns
      integer,                         intent(in)    :: fsa
      integer,                         intent(in)    :: lsa
      integer,                         intent(in)    :: ntwin
      integer,                         intent(in)    :: coropt
      integer,                         intent(in)    :: icdpn
      integer,                         intent(in)    :: idb
      integer,                         intent(in)    :: ipr
      real,                            intent(in)    :: fac
      real,             dimension(ns), intent(inout) :: scr1
      real,             dimension(ns), intent(inout) :: scr2
      real,             dimension(ns), intent(inout) :: ar
      real,             dimension(ns), intent(inout) :: ai
      real,             dimension(ns), intent(inout) :: br
      real,             dimension(ns), intent(inout) :: bi
      real,             dimension(ns), intent(inout) :: w
      double precision, dimension(ns), intent(inout) :: asum
      double precision, dimension(ns), intent(inout) :: bsum
      double precision, dimension(ns), intent(inout) :: absumr
      double precision, dimension(ns), intent(inout) :: absumi
      double precision, dimension(ns), intent(inout) :: wts



!     Zero out the weight, if either the correpsonding
!     'ar' or 'br' element is zero.
      call ppavo_nazero(w, ar, ns)
      call ppavo_nazero(w, br, ns)

!     scr1 <-- |A|**2; scr2 <-- |B|**2.
      call ppavo_namags(ar, ai, scr1, ns)
      call ppavo_namags(br, bi, scr2, ns)

!     scr1 <-- sqrt[|A|**2 + |A|**2].
      call ppavo_vadd(scr1, 1, scr2, 1, scr1, 1, ns)

!     Adjust the weights, if called for.
      if (coropt .gt. 0) then
         if (coropt .eq. 1) then
            call ppavo_vsqrtz(scr1, 1, 0.0, scr1, 1, ns)
         end if
         call ppavo_vdivz(w, 1, scr1, 1, 0.0, w, 1, ns)
      endif 

!     Smooth and average |B|**2 for the current trace.
      call ppavo_namags(ar, ai, scr1, ns)
      call ppavo_naacms(asum, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     Smooth and average |B|**2 for the current trace.
      call ppavo_namags(br, bi, scr1, ns)
      call ppavo_naacms(bsum, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     Smooth and average re{AB*} for the current trace.
      call ppavo_nabrgs(ar, ai, br, bi, scr1, ns)
      call ppavo_naacms(absumr, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     Smooth and average im{AB*} for the current trace.
      call ppavo_nabigs(ar, ai, br, bi, scr1, ns)
      call ppavo_naacms(absumi, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     Smooth and average the weighting function.
      call ppavo_vfill(1.0, scr1, 1, ns)
      call ppavo_naacms(wts, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     Print diagnostic information, if idb > 2.
      if (idb .gt. 2) write (ipr, 1000) icdpn, fac
1000  format(' AVERAGING TRACE ', i5,' WITH FACTOR = ', f4.1)

      return
      end subroutine ppavo_sauavga


!!----------------------------- ppavo_sauavgb --------------------------------!!
!!----------------------------- ppavo_sauavgb --------------------------------!!
!!----------------------------- ppavo_sauavgb --------------------------------!!

!     Purpose:
!        This subroutine computes running weighted averages of
!        <|A|**2>, <|B|**2>, and <AB*>, for use in the optimal
!        hydrocarbon AVO indicator.

!     Arguments:
!        ASUM   : Vector sum of |A|^2.
!        BSUM   : Vector sum of |B|^2.
!        ABSUMR : Vector sum of Re{AB*}.
!        ABSUMI : Vector sum of Im{AB*}.
!        WTS    : Vector sum of weights.
!        AR     : In  -> Zero-offset vector Re{A}.
!                 Out -> Transformed attribute.
!        AI     : In  -> Zero-offset vector Im{A}.
!                 Out -> Transformed attribute.
!        BR     : In  -> AVO gradient vector Re{B}.
!                 Out -> Transformed attribute.
!        BI     : In  -> AVO gradient vector Im{B}.
!                 Out -> Transformed attribute.
!        W      : In  -> Given weighting function.
!                 Out -> W \ (|A|^2 + |B|^2), if COROPT = 1.
!        SCR1   : Scratch array #1.
!        SCR2   : Scratch array #2.
!        NS     : Dimensioned size of arrays.
!        FSA    : First sample under analysis.
!        LSA    : Last sample under analysis.
!        NTWIN  : Number of samples in time window.
!        FAC    : Scale factor to apply to running sums.
!        COROPT : Weighting to apply to data:
!                 2 -> Use alternate method to get R.
!                 1 -> L2 norm with 1/SQRT(|A|^2 + |B|^2) WTS.
!                 0 -> L2 norm with no weights.
!        ALPHA  : Power of the norm if nonzero.
!        ICDPN  : Trace number being averaged.
!        IDB    : Debug level (print diagnostic information if >= 2).
!        IPR    : Fortran print unit number.

      subroutine ppavo_sauavgb (asum, bsum, absumr, absumi, wts, &
     &                          ar, ai, br, bi, w, scr1, scr2, &
     &                          ns, fsa, lsa, ntwin, fac, coropt,&
     &                          alpha, icdpn, idb, ipr)

      integer,                         intent(in)    :: ns
      integer,                         intent(in)    :: fsa
      integer,                         intent(in)    :: lsa
      integer,                         intent(in)    :: ntwin
      integer,                         intent(in)    :: coropt
      integer,                         intent(in)    :: icdpn
      integer,                         intent(in)    :: idb
      integer,                         intent(in)    :: ipr
      real,                            intent(in)    :: fac
      real,                            intent(in)    :: alpha
      real,             dimension(ns), intent(inout) :: scr1
      real,             dimension(ns), intent(inout) :: scr2
      real,             dimension(ns), intent(inout) :: ar
      real,             dimension(ns), intent(inout) :: ai
      real,             dimension(ns), intent(inout) :: br
      real,             dimension(ns), intent(inout) :: bi
      real,             dimension(ns), intent(inout) :: w
      double precision, dimension(ns), intent(inout) :: asum
      double precision, dimension(ns), intent(inout) :: bsum
      double precision, dimension(ns), intent(inout) :: absumr
      double precision, dimension(ns), intent(inout) :: absumi
      double precision, dimension(ns), intent(inout) :: wts


      real    :: mat(2,2)

!     Zero out the weight, if either the corresponding 'ar' or
!     'br' element is zero.
      call ppavo_nazero(w, ar, ns)
      call ppavo_nazero(w, br, ns)

!     Adjust the weights, for coropt = 1.
      if (coropt .eq. 1) then
         call ppavo_namags(ar, ai, scr1, ns)
         call ppavo_namags(br, bi, scr2, ns)

!        scr1 <-- sqrt[|a|^2 + |b|^2]; w <-- w / scr1.
         call ppavo_vadd(scr1, 1, scr2, 1, scr1, 1, ns)
         call ppavo_vsqrtz(scr1, 1, 0.0, scr1, 1, ns)
         call ppavo_vdivz(w, 1, scr1, 1, 0.0, w, 1, ns)
      endif

!     Smooth and average the weighting function.
      call ppavo_vfill(1.0, scr1, 1, ns)
      call ppavo_naacms(wts, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     Part 1:
!     scr1 <-- |a + b|^2 for coropt = 2.
      mat(1, 1) = 1
      mat(1, 2) = 1
      mat(2, 1) = 1
      mat(2, 2) = 1

      if (coropt .eq. 2) then
         call ppavo_sumag(ar, ai, br, bi, mat, scr1, ns)
!        scr1 <-- |a|^2 for coropt = 0 or 1.
      else
         call ppavo_namags(ar, ai, scr1, ns)
      endif

!     Raise to the power of alpha/2 if alpha is between (0, 2).
      call ppavo_mpowr(scr1, alpha, scr1, ns)

!     Smooth and average scr1 into asum.
      call ppavo_naacms(asum, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     scr1 <-- |a - b|^2 for coropt = 2.
      if (coropt .eq. 2) then
         mat(2, 1) = -1
         mat(2, 2) = -1
         call ppavo_sumag(ar, ai, br, bi, mat, scr1, ns)

!        scr1 <-- |b|^2 for coropt = 0 or 1.
      else
         call ppavo_namags(br, bi, scr1, ns)
      endif

!     Raise to the power of alpha/2 if alpha is between (0, 2).
      call ppavo_mpowr(scr1, alpha, scr1, ns)

!     Smooth and average scr1 into bsum.
      call ppavo_naacms(bsum, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     Part 2:
!     scr1 <-- |a + ib|^2 for coropt = 2.
!     Raise to the power of alpha/2 if alpha is between (0, 2).
      if (coropt .eq. 2) then
         mat(2, 1) = -1
         mat(2, 2) =  1
         call ppavo_sumag(ar, ai, bi, br, mat, scr1, ns)
         call ppavo_mpowr(scr1, alpha, scr1, ns)

!        Alternate procedure if alpha is between (0, 2):
!        (scr1, scr2) <-- ab*
!                scr1 <-- |ab*|^2;  scr2 <-- atan2(scr1, scr2)
!                scr1 <-- cos(scr2) * scr1 ** (alpha/4)
      else if (alpha .gt. 0 .and. alpha .lt. 2.0) then
         call ppavo_nabrgs(ar, ai, br, bi, scr1, ns)
         call ppavo_nabigs(ar, ai, br, bi, scr2, ns)
         call ppavo_magphs(scr1, scr2, scr1, scr2, ns)
         call ppavo_mpowr(scr1, alpha/2, scr1, ns)
         call ppavo_mcarts(scr1, scr2, scr1, scr2, ns)
!        scr1 <-- re{ab*} if alpha is not between (0, 2).
      else
         call ppavo_nabrgs(ar, ai, br, bi, scr1, ns)
      endif

!     Smooth and average scr1 into absumr.
      call ppavo_naacms(absumr, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)

!     scr1 <-- |a - ib|^2 for coropt = 2.
!     Raise to the power of alpha/2 if alpha is between (0, 2).
      if (coropt .eq. 2) then
         mat(2, 1) =  1
         mat(2, 2) = -1
         call ppavo_sumag(ar, ai, bi, br, mat, scr1, ns)
         call ppavo_mpowr(scr1, alpha, scr1, ns)

!        Alternate procedure if alpha is between (0, 2):
!        (scr1, scr2) <-- ab*
!                scr1 <-- |ab*|^2;  scr2 <-- atan2(scr1, scr2)
!                scr1 <-- sin(scr2) * scr1 ** (alpha/4)
      else if (alpha .gt. 0 .and. alpha .lt. 2.0) then
         call ppavo_nabrgs(ar, ai, br, bi, scr1, ns)
         call ppavo_nabigs(ar, ai, br, bi, scr2, ns)
         call ppavo_magphs(scr1, scr2, scr1, scr2, ns)
         call ppavo_mpowr(scr1, alpha/2, scr1, ns)
         call ppavo_mcarts(scr1, scr2, scr2, scr1, ns)

!        scr1 <-- im{ab*} if alpha is not between (0, 2).
      else
         call ppavo_nabigs(ar, ai, br, bi, scr1, ns)
      endif

!     Smooth and average scr1 into absumi.
      call ppavo_naacms(absumi, scr1, w, scr2, fsa, lsa, ns, ntwin, fac)
!     Print diagnostic information, if idb > 2.
      if (idb .gt. 2) write (ipr, 1000) icdpn, fac
1000  format(' AVERAGING TRACE ', i10,' WITH FACTOR = ', f4.1)

      return
      end subroutine ppavo_sauavgb


!!----------------------------- ppavo_sauclr ---------------------------------!!
!!----------------------------- ppavo_sauclr ---------------------------------!!
!!----------------------------- ppavo_sauclr ---------------------------------!!

!     Purpose:
!        This subroutine clears and initializes the statistics array.

!     Arguments:
!        STATS   : Statistics array.
!        NSTAT   : 1st dimension of statistics array.
!        MCOCODE : 2nd dimension of statistics array.
!        SCLFCT  : Scale factor.
!        RMSSCL  : RMS scale factor.

      subroutine ppavo_sauclr (borc, stats, nstat, mxocode, sclfct, rmsscl)

      integer                                    , intent(in)    :: nstat
      integer                                    , intent(in)    :: mxocode
      double precision, dimension(nstat, mxocode), intent(inout) :: stats
      real                                       , intent(in)    :: sclfct
      real                                       , intent(in)    :: rmsscl
      character                                  , intent(in)    :: borc

      double precision big, sf, rf
      integer    i
      integer    kthci, ktrvi, ktdv, ktitt
      parameter  (big = 1d10)

      if(borc .eq. 'B' .or. borc .eq. 'b') then
         kthci = ppavo_b_kthci
         ktrvi = ppavo_b_ktrvi
         ktdv  = ppavo_b_ktdv
         ktitt = ppavo_b_ktitt
      else
         kthci = ppavo_c_kthci
         ktrvi = ppavo_c_ktrvi
         ktdv  = ppavo_c_ktdv
         ktitt = ppavo_c_ktitt
      end if

!     Do the work.
      sf = abs(sclfct)
      rf = abs(sclfct)
      do 100 i = 1, mxocode
         stats(1,i) =  0       ! sum of values found so far
         stats(2,i) =  0       ! sum of squared values
         stats(3,i) =  big     ! smallest value
         stats(4,i) = -big     ! largest value
         stats(5,i) =  0       ! number of values
         if (sf .eq. 0 .and. rf .eq. 0) go to 100
         stats(6,i) =  rf      ! scale factor to be applied
         if(borc .eq. 'B' .or. borc .eq. 'b') then
            if (i .eq. kthci .or. i .eq. ktrvi) stats(6,i) = sf
            if (i .eq. ktdv .or. i .eq. ktitt) stats(6,i) = 1
         else
            if (i .eq. ktdv) stats(6,i) = 1
         end if
100   continue

      return
      end subroutine ppavo_sauclr


!!----------------------------- ppavo_saucora --------------------------------!!
!!----------------------------- ppavo_saucora --------------------------------!!
!!----------------------------- ppavo_saucora --------------------------------!!

!     Purpose:
!        This subroutine computes AVO correlation statistics, given
!        running sums of the AVEL traces.

!     Arguments:
!        ASUM   : Sum(|A|**2).
!        BSUM   : Sum(|B|**2).
!        ABSUMR : Re{Sum(AB*)}.
!        ABSUMI : Im{Sum(AB*)}.
!        WTS    : Sum(weights).
!        SIGA   : Standard deviation of A.
!        SIGB   : Standard deviation of B.
!        CORR   : Real part of correlation coefficient.
!        CORI   : Imaginary part of correlation coefficient.
!        TEMP   : Scratch array used for smoothing.
!        NS     : Dimensioned size of arrays.
!        FSA    : First sample under analysis.
!        LSA    : Last sample under analysis.
!        NSMTH  : Length of smoothing filter.
!        SHIFT  : Vertical shift of window (samples).
!        LIVE   : True if correlations are live.

      subroutine ppavo_saucora (asum, bsum, absumr, absumi, wts, siga, sigb,&
     &                 corr, cori, temp, ns, fsa, lsa, nsmth, shift, live)

      integer,                         intent(in)  :: ns
      integer,                         intent(in)  :: fsa
      integer,                         intent(in)  :: lsa
      integer,                         intent(in)  :: nsmth
      integer,                         intent(in)  :: shift
      logical,                         intent(out) :: live
      real,             dimension(ns), intent(out) :: siga
      real,             dimension(ns), intent(out) :: sigb
      real,             dimension(ns), intent(out) :: corr
      real,             dimension(ns), intent(out) :: cori
      real,             dimension(ns), intent(out) :: temp
      double precision, dimension(ns), intent(in)  :: asum
      double precision, dimension(ns), intent(in)  :: bsum
      double precision, dimension(ns), intent(in)  :: absumr
      double precision, dimension(ns), intent(in)  :: absumi
      double precision, dimension(ns), intent(in)  :: wts

      integer          :: i,  nz, nsa          , ndx 
      double precision :: sa, sb, cr, ci, denom, dmax

!     Determine whether this correlation is live.
      nsa= lsa - fsa + 1
      i = ppavo_idamax(nsa, wts(fsa), 1) + fsa - 1
      dmax = wts(i)
      if (dmax .le. 0d0) then
         call ppavo_vzero(siga, 1, ns)
         call ppavo_vzero(sigb, 1, ns)
         call ppavo_vzero(corr, 1, ns)
         call ppavo_vzero(cori, 1, ns)
         live = .false.
         return
      endif

!     Zero the leading part of the output array.
      live = .true.
      nz = fsa - 1
do    50 i = 1, nz
      siga(i) = 0.0
      sigb(i) = 0.0
      corr(i) = 0.0
50    cori(i) = 0.0

!     This loop should be fully vectorized.
      do 100 i = fsa, lsa
         sa = 0.0
         sb = 0.0
         cr = 0.0
         ci = 0.0
         ndx = shift + i
         if (ndx .ge. 1 .and. ndx .le. ns) then
            denom = wts(ndx)
            if (denom .gt. 0.0) then
               sa = dsqrt(asum(ndx) / denom)
               sb = dsqrt(bsum(ndx) / denom)
            endif
            denom = sa * sb * denom
            if (denom .gt. 0.0) then
               cr = absumr(ndx) / denom
               ci = absumi(ndx) / denom
            endif
         endif
         siga(i) = sa
         sigb(i) = sb
         corr(i) = cr
         cori(i) = ci
100   continue
      temp(:) = 0.0 ! wmm added this to set the array since it is intent out.  did not check
!                     to see if this is important to be set intent-out!.
!     Now perform the smoothing:
!      l2 = nsmth / 2
!      call masmth(nsa, 1, 1, siga(fsa), temp, l2)
!      call masmth(nsa, 1, 1, sigb(fsa), temp, l2)
!      call masmth(nsa, 1, 1, corr(fsa), temp, l2)
!      call masmth(nsa, 1, 1, cori(fsa), temp, l2)

      return
      end subroutine ppavo_saucora


!!---------------------------- ppavo_saucorb ---------------------------------!!
!!---------------------------- ppavo_saucorb ---------------------------------!!
!!---------------------------- ppavo_saucorb ---------------------------------!!

!     Purpose:
!        This subroutine computes AVO correlation statistics, given
!        running sums of the AVEL traces.

!     Arguments:
!        ASUM   : Sum(|A|**2).
!        BSUM   : Sum(|B|**2).
!        ABSUMR : Re{Sum(AB*)}.
!        ABSUMI : Im{Sum(AB*)}.
!        WTS    : Sum(weights).
!        SIGA   : Standard deviation of A.
!        SIGB   : Standard deviation of B.
!        CORR   : Real part of correlation coefficient.
!        CORI   : Imaginary part of correlation coefficient.
!        COROPT : Type of correlation performed.
!        ALPHA  : The normalization power.
!        CORMAX : Maximum allowable correlation magnitude.
!        NS     : Dimensioned size of arrays.
!        SHIFT  : Vertical shift of window (samples).
!        LIVE   : True if correlations are live.

      subroutine ppavo_saucorb (asum, bsum, absumr, absumi, wts, siga, sigb,&
     &                 corr, cori, coropt, alpha, cormax, ns, shift, live)

      integer,                         intent(in)  :: ns
      integer,                         intent(in)  :: shift
      integer,                         intent(in)  :: coropt
      logical,                         intent(out) :: live
      real,                            intent(in)  :: alpha
      real,                            intent(in)  :: cormax
      real,             dimension(ns), intent(out) :: siga
      real,             dimension(ns), intent(out) :: sigb
      real,             dimension(ns), intent(out) :: corr
      real,             dimension(ns), intent(out) :: cori
      double precision, dimension(ns), intent(in)  :: asum
      double precision, dimension(ns), intent(in)  :: bsum
      double precision, dimension(ns), intent(in)  :: absumr
      double precision, dimension(ns), intent(in)  :: absumi
      double precision, dimension(ns), intent(in)  :: wts

      integer          :: i,  ndx, flv, llv, fsa, lsa
      logical          :: use_alpha
      double precision :: sa, sb, cr, ci, denom   
      double precision :: g, a2, cormag, factor, ma, mb, magsq
      double precision :: phase, denom1

!     Clear the output arrays.
      if (coropt .ne. 2) then
         call ppavo_vzero(siga, 1, ns)
         call ppavo_vzero(sigb, 1, ns)
      endif
      call ppavo_vzero(corr, 1, ns)
      call ppavo_vzero(cori, 1, ns)

!     Determine the gamma scale factor.
      g  = 1.0
      a2 = 1d0
      use_alpha = alpha .gt. 0 .and. alpha .lt. 2.0
      if (use_alpha) then
         g  = ppavo_gamma(1d0 + alpha/2d0)
         a2 = 2d0 / dble(alpha)
      endif

!     Here we go, keeping track of the first & last live values.
!     note:  "ndx" is the index where the data is coming from.
!     'i' is the index where the answers are going.
      llv = 1
      flv = ns
      fsa = max0(1  - shift, 1)
      lsa = min0(ns - shift, ns)
      do 100 i = fsa, lsa
         ndx = shift + i
         if (wts(ndx) .gt. 0) then
            if (flv .eq. ns) flv = i
            llv = i

!           Compute the standard deviations:
            denom = g * wts(ndx)
            if (denom .gt. 0.0) then
               sa = asum(ndx) / denom
               sb = bsum(ndx) / denom
            endif
            if (coropt .ne. 2) then
               sa = dsqrt(sa)
               sb = dsqrt(sb)
            endif
            if (use_alpha) then
               sa = sa ** a2
               sb = sb ** a2
            endif

!           Compute the correlation coefficient in the "normal" way:
            if (coropt .ne. 2) then
               if (.not. use_alpha) then
                  denom = sa * sb * denom
                  if (denom .gt. 0.0) then
                     cr = absumr(ndx) / denom
                     ci = absumi(ndx) / denom
                  endif

!                 'coropt' != 2, but an alternate norm was used:
               else
                  phase = datan2(absumi(ndx), absumr(ndx))
                  magsq = absumr(ndx)**2 + absumi(ndx)**2
                  if (denom .gt. 0.0) then
                     magsq = dsqrt(magsq) / denom
                     magsq = magsq ** a2
                     magsq = magsq / (sa * sb)
                     cr    = magsq * dcos(phase)
                     ci    = magsq * dsin(phase)
                  endif
               endif

!              'coropt' == 2.  use the alternate **method**:
            else
               if (denom .gt. 0.0) then
                  ma = absumr(ndx) / denom
                  mb = absumi(ndx) / denom
               endif
               if (use_alpha) then
                  ma = ma ** a2
                  mb = mb ** a2
               endif
               denom  = sa + sb
               denom1 = ma + mb
               if (denom .gt. 0 .and. denom1 .gt. 0) then
                  cr = (sa - sb) / denom
                  ci = (ma - mb) / denom1
               endif                  
            endif
         endif

!        Check the magnitude of the correlation coefficient.
!        (i truly hope this code isn't needed, since it would invalidate
!        this procedure.)
         cormag = sqrt(cr*cr + ci*ci)
         if (cormag .gt. cormax) then
            factor = cormax / cormag
            cr     = cr * factor
            ci     = ci * factor
         endif

!        Stuff the answers into their final resting spots.
!        note:  sa and sb are **not** the standard deviations of 'A'
!        and 'B' if coropt = 2.
         if (coropt .ne. 2) then
            siga(i) = sa
            sigb(i) = sb
         endif

         corr(i) = cr
         cori(i) = ci
100   continue

!     Mark dead traces as killed.
      live = flv .ne. ns
      if (.not. live) return

!     Extrapolate the extremities.
      call ppavo_xtrap(corr, ns, flv, llv)
      call ppavo_xtrap(cori, ns, flv, llv)
      if (coropt .ne. 2) then
         call ppavo_xtrap(siga, ns, flv, llv)
         call ppavo_xtrap(sigb, ns, flv, llv)
      endif

      return
      end subroutine ppavo_saucorb


!!----------------------------- ppavo_saufdp ---------------------------------!!
!!----------------------------- ppavo_saufdp ---------------------------------!!
!!----------------------------- ppavo_saufdp ---------------------------------!!

!     Purpose:
!        This function clears and initializes the statistics array.

!     Arguments:
!        STATS   : Statistics array.
!        NSTAT   : 1st dimension of statistics array.
!        MCOCODE : 2nd dimension of statistics array.
!        SCLFCT  : Scale factor.
!        RMSSCL  : RMS scale factor.

      integer function ppavo_saufdp (cdps, ncdps, icdp)

      integer                    , intent(in) :: icdp
      integer                    , intent(in) :: ncdps
      integer, dimension(0:ncdps), intent(in) :: cdps

      integer :: i

!     Perform the search backwards.
      ppavo_saufdp = ncdps
      do 100 i = ncdps-1, 0, -1
         if(abs(cdps(i)) .eq. icdp) then
            ppavo_saufdp = i
            return
         end if
100   continue
      return
      end function ppavo_saufdp


!!----------------------------- ppavo_sauhce ---------------------------------!!
!!----------------------------- ppavo_sauhce ---------------------------------!!
!!----------------------------- ppavo_sauhce ---------------------------------!!

!     Purpose:
!        This subroutine....

!     Arguments:
!        R     : Real part of the correlation coefficient.
!        P     : Exponentiation array.
!        NS    : Size of 'R' and 'P' arrays.
!        ALPHA : Aggressiveness factor.
!        DTMIN : Minimum delta fluid angle (degrees).

      subroutine ppavo_sauhce (r, p, ns, alpha, dtmin)

      integer            , intent(in)  :: ns
      real               , intent(in)  :: alpha
      real               , intent(in)  :: dtmin
      real, dimension(ns), intent(in)  :: r
      real, dimension(ns), intent(out) :: p

      integer :: i
      real    :: pi2, rad, ln2
      real    :: dtmax, rdmin, rdmax, tmp, tmp1, dth
      parameter   (pi2=1.57079)
      parameter   (rad=57.2958)
      parameter   (ln2=0.693147)

!     Fix the minimum and maximum radial limits for delta theta-f.
      dtmax = 180.0 - dtmin
      rdmin = dtmin / rad
      rdmax = dtmax / rad

!     Compute delta theta-f:
      do 100 i=1, ns
         tmp    = pi2 * (1.0 + alpha*r(i))
         if (tmp .gt. rdmax) tmp = rdmax
         if (tmp .lt. rdmin) tmp = rdmin
         p(i) = tmp
100   continue

!     Transform delta theta-f into `p':
      do 200 i = 1, ns
         dth = p(i)
         p(i) = 1.0
         tmp1 = -ln2
         tmp = (sin(0.5*dth))**2
         if (tmp .gt. 0.0) tmp1 = alog(tmp)
         if (tmp1 .ne. 0.0) p(i) = -ln2 / tmp1
200   continue

      return
      end subroutine ppavo_sauhce


!!----------------------------- ppavo_sauova ---------------------------------!!
!!----------------------------- ppavo_sauova ---------------------------------!!
!!----------------------------- ppavo_sauova ---------------------------------!!

!     Purpose:
!        This subroutine gets ready to perform automatic overburden
!        correction for the zero offset response, by doing the
!        following:
!        1) Compute "k*", which is the cross-term leakage factor from b->a.
!        2) Update the power in the avo slope with the leakage removed.
!
!     Arguments:
!        SA   : Sqrt{<|A|**2>}.
!        SB   : Sqrt{<|B|**2>}.
!        RRC  : Re{<AB*>} / (SA*SB).
!        RIC  : Im{<AB*>} / (SA*SB).
!        KR   : Re{K*}.
!        KI   : Im{K*}.
!        SBPR : Standard deviation of A prime.
!        WRR  : Real correlation coefficient.
!        SCR  : Scratch array.
!        NS   : Length of all arrays.

      subroutine ppavo_sauova (sa, sb, rrc, ric, kr, ki, sapr, wrr, scr, ns)

      integer,             intent(in)  :: ns
      real, dimension(ns), intent(in)  :: sa
      real, dimension(ns), intent(in)  :: sb
      real, dimension(ns), intent(in)  :: rrc
      real, dimension(ns), intent(in)  :: ric
      real, dimension(ns), intent(out) :: kr
      real, dimension(ns), intent(out) :: ki
      real, dimension(ns), intent(out) :: sapr
      real, dimension(ns), intent(in)  :: wrr
      real, dimension(ns), intent(out) :: scr

!     Scale the calibration traces, if necessary.

!     Set kr <-- wrr**2 + wri**2 = |rd|**2.
      call ppavo_vmul(wrr, 1, wrr, 1, kr, 1, ns)

!     Set sapr <-- 1.0; kr <-- 1.0 - |rd|**2
      call ppavo_vfill(1.0, sapr, 1, ns)
      call ppavo_vsub(sapr, 1, kr, 1, kr, 1, ns)

!     Compute the conjugated leakage factor, k*.

!     sapr <-- sqrt{(1-|r|**2) / (1-|rd|**2)}
      call ppavo_namags(rrc,  ric, scr,  ns)
      call ppavo_vsub  (sapr, 1, scr, 1, scr, 1, ns)
      call ppavo_vdivz (scr, 1, kr, 1, 0.0, sapr, 1, ns)
      call ppavo_vsqrtz(sapr, 1, 0.0, sapr, 1, ns)

!     k <-- -r* + rd*sapr: (a difference from ppavo_sauovb).
!     kr <-- wrr * sapr.
!     kr <-- kr  - rrc.
!     ki <-- ric.
      call ppavo_vmul (wrr, 1, sapr, 1, kr, 1, ns)
      call ppavo_vsma (rrc, 1, -1.0,  kr,  1, kr, 1, ns)
      call ppavo_vmove (ric, 1, ki, 1, ns)

!     scr  <--  sa / sb (another difference).
!     kr   <--  kr * scr.
!     ki   <--  ki * scr.
!     sapr <--  sa * sapr (another difference).
      call ppavo_vdivz (sa,   1, sb,  1, 0.0, scr, 1, ns)
      call ppavo_vmul  (kr,   1, scr, 1, kr,       1, ns)
      call ppavo_vmul  (ki,   1, scr, 1, ki,       1, ns)
      call ppavo_vmul  (sapr, 1, sa,  1, sapr,     1, ns)

      return
      end subroutine ppavo_sauova


!!----------------------------- ppavo_sauovb ---------------------------------!!
!!----------------------------- ppavo_sauovb ---------------------------------!!
!!----------------------------- ppavo_sauovb ---------------------------------!!

!     Purpose:
!        This subroutine gets ready to perform automatic overburden
!        correction for the optimal hydrocarbon indicator, by doing the
!        following:
!        1) Compute "K*", which is the cross-term leakage factor from A->B.
!        2) Compute a velocity correction factor, DVEL,  for 'AVEL'
!        3) Update the power in the AVO slope with the leakage removed.
!        4) Update the correlation coefficient between "A" and the modified
!        AVO slope (leakage removed).
!
!     Note:  If WRR and WRI have been scaled up for color plotting,
!            they will be scaled back down by this subroutine.
!
!     Arguments:
!        SA   : Sqrt{<|A|**2>}.
!        SB   : Sqrt{<|B|**2>}.
!        RRC  : Re{<AB*>} / (SA*SB).
!        RIC  : Im{<AB*>} / (SA*SB).
!        DVEL : -Im{<AB*>} / (SA*SA) = -RIC * SB/SA.
!        KR   : Re{K*}.
!        KI   : Im{K*}.
!        SBPR : Standard deviation of B prime.
!        WRR  : Real correlation coefficient.
!        WRI  : Imaginary correlation coefficient.
!        NS   : Length of all arrays.

      subroutine ppavo_sauovb (sa, sb, rrc, ric, dvel, kr, ki, sbpr, &
     &                         wrr, wri, ns)

      integer,             intent(in)    :: ns
      real, dimension(ns), intent(in)    :: sa
      real, dimension(ns), intent(in)    :: sb
      real, dimension(ns), intent(in)    :: rrc
      real, dimension(ns), intent(in)    :: ric
      real, dimension(ns), intent(out)   :: dvel
      real, dimension(ns), intent(out)   :: kr
      real, dimension(ns), intent(out)   :: ki
      real, dimension(ns), intent(out)   :: sbpr
      real, dimension(ns), intent(inout) :: wrr
      real, dimension(ns), intent(inout) :: wri

!     Scale the calibration traces, if necessary.

!     Set kr <-- wrr**2 + wri**2 = |rd|**2
      call ppavo_namags(wrr, wri, kr, ns)

!     Set sbpr <-- 1.0; kr <-- 1.0 - |rd|**2
      call ppavo_vfill (1.0, sbpr, 1, ns)
      call ppavo_vsub  (sbpr, 1, kr, 1, kr, 1, ns)

!     Compute the conjugated leakage factor, k*.

!     sbpr <-- sqrt((1-|r|**2) / (1-|rd|**2))
      call ppavo_namags(rrc,  ric,  dvel, ns)
      call ppavo_vsub  (sbpr, 1, dvel, 1, dvel, 1, ns)
      call ppavo_vdivz (dvel, 1, kr, 1, 0.0, sbpr, 1, ns)
      call ppavo_vsqrtz(sbpr, 1, 0.0, sbpr, 1, ns)

!     k <-- -r + rd*sbpr:
!     kr <-- wrr * sbpr
!     ki <-- wri * sbpr
!     kr <-- kr  - rrc
!     ki <-- ki  - ric
      call ppavo_vmul (wrr, 1, sbpr,  1, kr,  1, ns)
      call ppavo_vmul (wri, 1, sbpr,  1, ki,  1, ns)
      call ppavo_vsma (rrc, 1, -1.0,  kr,  1, kr, 1, ns)
      call ppavo_vsma (ric, 1, -1.0,  ki,  1, ki, 1, ns)

!     dvel <--  sb   / sa
!     kr   <--  kr   * dvel
!     ki   <--  ki   * dvel
!     dvel <-- -dvel * ric
!     sbpr <--  sb   * sbpr
      call ppavo_vdivz (sb,   1, sa,   1, 0.0, dvel, 1, ns)
      call ppavo_vmul  (kr,   1, dvel, 1, kr,        1, ns)
      call ppavo_vmul  (ki,   1, dvel, 1, ki,        1, ns)
      call ppavo_vmul  (ric,  1, dvel, 1, dvel,      1, ns)
      call ppavo_vsmul (dvel, 1, -1.0,    dvel,      1, ns)
      call ppavo_vmul  (sbpr, 1, sb,   1, sbpr,      1, ns)

      return
      end subroutine ppavo_sauovb


!!----------------------------- ppavo_sauskp ---------------------------------!!
!!----------------------------- ppavo_sauskp ---------------------------------!!
!!----------------------------- ppavo_sauskp ---------------------------------!!

!     Purpose:
!        This character function is used by process 'UHCI' in printing
!        the list of CDPS processed.  A CDP may be used in the correla-
!        tion of AB*, without hydrocarbon indicators being computed for
!        for it.  Only CDPS within the processing range given or
!        defaulted by the 'UHCI' card will have indicators computed,
!        even though all CDPS read will be correlated.  In the list
!        of CDPS processed, an 'S' will appear after those CDPS which
!        were correlated but not passed.  This function, called from
!        within an implied DO-loop, returns either this 'S', or a
!        blank, depending on whether the current CDP number is within
!        the processing range.

!     Arguments:
!        I  : Integer to compare.
!        L1 : Lower bound with which to compare I.
!        L2 : Upper bound with which to compare I.

      character(len=1) function ppavo_sauskp (i, l1, l2)

      integer, intent(in) :: i
      integer, intent(in) :: l1
      integer, intent(in) :: l2

!     Make the all-important comparison.
      ppavo_sauskp = 'S'
      if(i .le. l2 .and. i .ge. l1) ppavo_sauskp = ' '
      if(i .le. 0) ppavo_sauskp = 'K'
      return
      end function ppavo_sauskp


!!----------------------------- ppavo_saustc ---------------------------------!!
!!----------------------------- ppavo_saustc ---------------------------------!!
!!----------------------------- ppavo_saustc ---------------------------------!!

!     Purpose:
!        This subroutine computes the ingredients for the mean,
!        standard deviation, and extreme values, to be printed
!        at the end of the job by process 'UHCI'.

!     Arguments:
!        INTR  : Input data vector.
!        FSA   : First sample under analysis.
!        LSA   : Last sample under analysis.
!        STATS : Statistics array:
!                (1) -> sum of values found so far.
!                (2) -> sum of squared values.
!                (3) -> minimum value found.
!                (4) -> maximum value found.
!                (5) -> number of values found.
!        NSTAT : Size of the 'STATS' array.
!        ISTAT : Particular statistic to change.
!        LIVE  : Flag indicating if trace appears live.

      subroutine ppavo_saustc (intr, fsa, lsa, stats, nstat, istat, live)

      integer                                 , intent(in)    :: fsa
      integer                                 , intent(in)    :: lsa
      integer                                 , intent(in)    :: nstat
      integer                                 , intent(in)    :: istat
      double precision, dimension(nstat,istat), intent(inout) :: stats
      real            , dimension(lsa)        , intent(in)    :: intr
      logical                                 , intent(out)   :: live

      integer :: i   
      integer :: lmin, lmax, nsa
      real    :: value
      double precision :: bmin, bmax, sum, sum2, nv


!     Is the trace live?
      nsa = lsa - fsa + 1
      lmin = ppavo_ismin(nsa, intr(fsa:), 1) + fsa - 1
      lmax = ppavo_ismax(nsa, intr(fsa:), 1) + fsa - 1
      live = intr(lmin) .lt. intr(lmax)
      if (.not. live) return

!     Locate and return the extreme values.
      bmin = stats(3, istat)
      bmax = stats(4, istat)

!     Note: switched method of getting min/max  
      bmin = dmin1(bmin, dble(intr(lmin)))
      bmax = dmax1(bmax, dble(intr(lmax)))
!     d1 = intr(lmin)
!     d2 = intr(lmax)
!     bmin = dmin1(bmin, d1)
!     bmax = dmax1(bmax, d2)

      stats(3, istat) = bmin
      stats(4, istat) = bmax

!     Compute and return the running sums:
      sum  = 0d0
      nv   = sum
      sum2 = sum
      nsa  = lsa - fsa + 1
      do 100 i=fsa, lsa
         value = intr(i)
         if (value .ne. 0.0) then
            nv    = nv   + 1d0
            sum   = sum  + value
            sum2  = sum2 + 1d0*value*value
         endif
100   continue

      stats(1, istat) = stats(1, istat) + sum
      stats(2, istat) = stats(2, istat) + sum2
      stats(5, istat) = stats(5, istat) + nv

      return
      end subroutine ppavo_saustc


!!----------------------------- ppavo_saustp ---------------------------------!!
!!----------------------------- ppavo_saustp ---------------------------------!!
!!----------------------------- ppavo_saustp ---------------------------------!!

!     Purpose:
!        This subroutine computes and prints scaling statistics for
!        the auxiliary traces generated by process 'UHCI'.
!
!     Arguments:
!        STATS   : Statistics array.
!                  (1) sum of values found so far.
!                  (2) sum of squared values.
!                  (3) minimum value found.
!                  (4) maximum value found.
!                  (5) number of values found.
!                  (6) scale factor applied .       
!        NSTAT   : 1st dimension of 'STATS' array.
!        NTOUT   : 2nd dimension of 'STATS' array.
!        OTICD   : List of external TICD codes output.
!        CTAB    : Correspondence between external/internal codes.
!        CTEXT   : Descriptors correpsonding to internal codes.
!        MINTID  : Minimum index of 'CTAB' array.
!        MAXTID  : Maximum index of 'CTAB' array.
!        MXOCODE : Size of 'CTEXT' array.
!        IPR     : Printter unit number.

      subroutine ppavo_saustp (stats, nstat, ntout, oticd, ctab,&
     &                        ctext, mintid, maxtid, mxocode, ipr)

      integer                                   ,intent(in) :: nstat
      integer                                   ,intent(in) :: ntout
      integer                                   ,intent(in) :: mintid
      integer                                   ,intent(in) :: maxtid
      integer                                   ,intent(in) :: mxocode
      integer                                   ,intent(in) :: ipr
      integer,          dimension(ntout)        ,intent(in) :: oticd
      integer,          dimension(mintid:maxtid),intent(in) :: ctab
      double precision, dimension(nstat,ntout)  ,intent(in) :: stats
      character(len=*), dimension(mxocode)      ,intent(in) :: ctext
     
      integer :: i, extcde, intcde
      double precision :: scalef, amean, asigm, terms, bmin, bmax

!     Print the header.
      write (ipr, 7000)

!     Compute and print the results.
      do 100 i = 1, ntout
         extcde = oticd(i)
         intcde = ctab(extcde)
         amean  = 0.0
         asigm  = 0.0
         scalef = stats(6, intcde)
         terms  = stats(5, intcde)
         bmin   = stats(3, intcde)
         bmax   = stats(4, intcde)
         if (terms .gt. 0) then
            amean = stats(1, intcde) / terms
            asigm = dsqrt(dabs(stats(2,intcde)/terms - amean**2))
         endif
         write (ipr, 7700) ctext(intcde), extcde, scalef,&
     &       amean, asigm, bmin, bmax
100   continue
      write (ipr, 7800)

!     Format statements.
7000  format(/8x, 'STATISTICS OF OUTPUT TRACES PASSED:'/&
     &   8x, '-----------------------------------'//&
     &   t20, 'APPLIED'/t21, 'SCALE'/&
     &   t1, 'TRACE', t11, 'TICD', t20, 'FACTOR', t34,'MEAN',&
     &   t45, 'STD. DEV.', t59, 'MINIMUM', t71, 'MAXIMUM'/&
     &   78('-'))

7700  format(t1, a, t12, i2, t18, 1p, g12.5, t31, g12.5, t44,&
     &  g12.5, t58, g12.5, t70, g12.5)

7800  format(/)

      return
      end subroutine ppavo_saustp


!!----------------------------- ppavo_savelc ---------------------------------!!
!!----------------------------- ppavo_savelc ---------------------------------!!
!!----------------------------- ppavo_savelc ---------------------------------!!

!     Purpose:
!        This subroutine converts stacking velocity into stacking sloth,
!        which is V**(-2). It optionally converts stacking sloth into
!        interval sloth, using the Dix equation.

!     Arguments:
!        INTR   : Given stacking velocities (feet/sec).
!        SLOTHS : Computed stacking sloths (sec**2/feet**2).
!        SLOTHI : Computed interval sloths (sec**2/feet**2).
!        NS     : Length of trace in samples.
!        IVSMTH : Length of velocity smoothing filter.
!        IVIFLG : Index of new last live sample.
!                 If >0, convert stacking sloth to interval sloths,
!                 delayed by IVIFLG-1 samples
!                 If =0, simply copy stacking sloths into the
!                 interval sloth array
!        T0     : Time of the first sample (seconds).
!        VINC   : Velocity increment, to be added to each element
!                 of INTR (feet/sec).
!        TSAMP  : Sampling interval (seconds).
!        OVRFLG : Non-zero if velocities are to be replaced with constants.
!        TARGET : Target zone (seconds).
!        IER    : Index of first bad velocity. 0 if everything is Ok.

      subroutine ppavo_savelc (intr, sloths, slothi, ns, ivsmth,&
     &     iviflg, t0, vinc, tsamp, ovrflg, target, ier)

      integer                , intent(in)    :: ns
      integer                , intent(in)    :: ivsmth
      integer                , intent(in)    :: iviflg
      integer                , intent(in)    :: ovrflg
      integer                , intent(in)    :: target
      integer                , intent(out)   :: ier
      real, dimension(0:ns+1), intent(out)   :: sloths
      real, dimension(0:ns+1), intent(out)   :: slothi
      real, dimension(ns)    , intent(inout) :: intr      
      real                   , intent(in)    :: t0
      real                   , intent(in)    :: vinc
      real                   , intent(in)    :: tsamp

      integer          :: i   
      real             :: slnew, sn
      double precision :: t1, t2, temp

!     First, take a peek at these so-called velocities.
      do 50 i = 1, ns
         ier = i
         if(intr(i) .le. 0.0) return
50    continue

!     Convert to stacking sloths.
      do 100 i = 1, ns
         sloths(i) = 1.0 / (intr(i) + vinc)**2
100   continue
      sloths(0) = sloths(1)
      sloths(ns+1) = sloths(ns)

!     Next smooth the stacking sloth.
      call ppavo_masmth(ns+2, 1, 1, sloths, slothi, ivsmth)

!     If OVRFLG is non-zero, pick out one particular sloth and
!     replicate it throughout.
      if (ovrflg .gt. 0) then
         sn = target * 1e-3 / tsamp + 1
         if (sn .gt. ns+1) then
            slnew = sloths(ns)
         else if(ns .le. 1) then
            slnew = sloths(1)
         else
            i  = sn
            sn = sn - i
            slnew = sn*sloths(i+1) + (1-sn)*sloths(i)
         endif
         call ppavo_vrfill (slnew, sloths, 1, ns+2)
      endif

!     Finally, convert to interval sloths, if requested.
      if (iviflg .gt. 0 .and. ovrflg .eq. 0) then
         do 200 i = 1, ns
            t1 = (i-2)*tsamp + t0
            t2 = i*tsamp + t0
            temp = 2.0*tsamp*sloths(i-1)*sloths(i+1)
            slothi(i+iviflg-1) = temp/(t2*sloths(i-1) - t1*sloths(i+1))
200      continue

         do 300 i = 0, iviflg-1
            slothi(i) = slothi(iviflg+1)
300      continue
      else
         call ppavo_vrmove(sloths, 1, slothi, 1, ns+2)
      endif
      ier = 0

      return
      end subroutine ppavo_savelc


!!----------------------------- ppavo_sumag ----------------------------------!!
!!----------------------------- ppavo_sumag ----------------------------------!!
!!----------------------------- ppavo_sumag ----------------------------------!!

!     Purpose:
!        This subroutine computes:
!           Sr = Ar*MAT(1,1) + Br*MAT(2,1) and
!           Si = Ai*MAT(1,2) + Bi*MAT(2,2).
!        It then returns the squared magnitude of the complex number
!        (Sr + i Si). It does this for each element of the complex
!        A and B input vectors.

!     Arguments:
!        AR  : Real part of A vector.
!        AI  : Imaginary part of A vector.
!        BR  : Real part of B vector.
!        BI  : Imaginary part of B vector.
!        MAT : 2x2 coefficient matrix.
!        OUT : |Ar*M11+Br*M21) + i(Ai*M12+Bi*M22)|^2.
!        NS  : Size of all arrays.

      subroutine ppavo_sumag (ar, ai, br, bi, mat, out, ns)

      integer            ,  intent(in)  :: ns
      real, dimension(ns),  intent(in)  :: ar
      real, dimension(ns),  intent(in)  :: ai
      real, dimension(ns),  intent(in)  :: br   
      real, dimension(ns),  intent(in)  :: bi
      real, dimension(2,2), intent(in)  :: mat
      real, dimension(ns),  intent(out) :: out

      integer :: i
      real    :: m11, m12, m21, m22, sr, si

!     Unpack the coefficient matrix.
      m11 = mat(1, 1)
      m12 = mat(1, 2)
      m21 = mat(2, 1)
      m22 = mat(2, 2)

!     Do it!
      do 100 i = 1, ns
         sr = ar(i) * m11 + br(i) * m21
         si = ai(i) * m12 + bi(i) * m22
         out(i) = sr*sr + si*si
100   continue

      return
      end subroutine ppavo_sumag
  
  
!!------------------------------ ppavo_swlev ---------------------------------!!
!!------------------------------ ppavo_swlev ---------------------------------!!
!!------------------------------ ppavo_swlev ---------------------------------!!
  
      subroutine ppavo_swlev (x, incx, u, incu, y, incy, n, aux, naux)

      integer                          ,intent(in)    :: incx
      integer                          ,intent(in)    :: incu
      integer                          ,intent(in)    :: incy
      integer                          ,intent(in)    :: n
      integer                          ,intent(in)    :: naux
      real, dimension(incx,n)          ,intent(inout) :: x
      real, dimension(incu,n)          ,intent(inout) :: u
      real, dimension(incy,n)          ,intent(inout) :: y
      double precision, dimension(naux),intent(inout) :: aux

      integer :: i
      integer :: ier

      if (incx .ne. 1 .or. incu .ne. 1) then
         write(*,*) 'ppavo_swlev does not support strides > 1.'
         stop
      end if

!     The following is Herb Swan's modified version of Marple's routine.
      call ppavo_hermtp(n-1, x(1,1), x(1:,2), u, aux, aux(n+1:), ier)

      if (ier .eq. 1) then
         write(*,*) 'Warning: ier > 0 from ppavo_hermtp.'
      end if

      do 100 i = 1, n
         y(1,i) = aux(i)
100   continue

      return
      end subroutine ppavo_swlev


!!-------------------------- ppavo_time_carr ---------------------------------!!
!!-------------------------- ppavo_time_carr ---------------------------------!!
!!-------------------------- ppavo_time_carr ---------------------------------!!

!     Purpose:
!        Print the time array.

!     Arguments:
!        IDATE : Date.
!        WHEN  : When.

      subroutine ppavo_time_carr (idate, when)

      integer,           intent(in)    :: idate
      character(len=27), intent(inout) :: when




!      external time  

!      call ltime(time, tarr)

!      print *, '**************************************'
!      print *, 'SEC =', tarr(1)
!      print *, 'MIN =', tarr(2)
!      print *, 'HOUR=', tarr(3)
!      print *, 'MDAY=', tarr(4)
!      print *, 'MNTH=', tarr(5)
!      print *, 'YEAR=', tarr(6)
!      print *, 'WDAY=', tarr(7)
!      print *, 'YDAY=', tarr(8)
!      print *, 'IDSV=', tarr(9)

      when = '                           '
!      write (when, 100) tarr(7), tarr(5), tarr(4),&
!     &       tarr(3), tarr(2), tarr(1), tarr(6)
!100   format (i1, ' ', i2, ' ', i2, ' ',&
!     &   i2, ' ', i2, ' ', i2, ' ', i4) 

      end subroutine ppavo_time_carr


!!----------------------------- ppavo_trlive ---------------------------------!!
!!----------------------------- ppavo_trlive ---------------------------------!!
!!----------------------------- ppavo_trlive ---------------------------------!!

!     Purpose:
!        This subroutine finds the first and last live (non-zero) samples
!        in the input trace.

!     Arguments:
!        TRACE  : Input trace.
!        NS     : Number of samples in input trace.
!        NEWFLV : Index of new first live sample.
!        NEWLLV : Index of new last live sample.

      subroutine ppavo_trlive (trace, ns, newflv, newllv)

      integer            , intent(in)    :: ns
      integer            , intent(inout) :: newflv
      integer            , intent(inout) :: newllv
      real, dimension(ns), intent(in)    :: trace

      integer :: i   

!     Find the first live value.
      do 100 i = 1, ns
         newflv = i
         if (trace(i) .ne. 0.0) goto 200
100   continue

!     Find the last live value.
200   do 300 i = ns, 1, -1
         newllv = i
         if (trace(i) .ne. 0.0) goto 400
300   continue
  
400   return
      end subroutine ppavo_trlive


!!--------------------------- ppavo_uhci_read --------------------------------!!
!!--------------------------- ppavo_uhci_read --------------------------------!!
!!--------------------------- ppavo_uhci_read --------------------------------!!

!     Purpose:
!        This subroutine reads data from temporary storage.

      subroutine ppavo_uhci_read (obj, kpwks, kpwkd, traceno, tr, hd,&
     &                            lth, nsamp, ddp1hd, ddp1tr, ier)

      type(ppavo_struct), pointer                       :: obj
      integer,                            intent(inout) :: kpwks
      integer,          dimension(2),     intent(inout) :: kpwkd
      integer,                            intent(inout) :: traceno
      integer,                            intent(inout) :: lth
      integer,                            intent(inout) :: nsamp
      real,             dimension(nsamp), intent(inout) :: ddp1tr
      double precision, dimension(lth),   intent(inout) :: ddp1hd
      double precision, dimension(lth),   intent(inout) :: hd(lth)
      real,             dimension(nsamp), intent(inout) :: tr(nsamp)
      integer,                            intent(inout) :: ier

      if (traceno .le. 0) traceno = 1
      call ppavo_fordsd (obj, kpwks, kpwkd, traceno, ddp1hd, ddp1tr, ier)  
      call ppavo_vmove (ddp1hd, 1, hd, 1, lth)
      call ppavo_vmove (ddp1tr, 1, tr, 1, nsamp)

      end subroutine ppavo_uhci_read


!!--------------------------- ppavo_uhci_write -------------------------------!!
!!--------------------------- ppavo_uhci_write -------------------------------!!
!!--------------------------- ppavo_uhci_write -------------------------------!!

!     Purpose:
!        This subroutine writes data to temporary storage.

      subroutine ppavo_uhci_write (obj, kpwks, kpwkd, traceno, tr, hd,&
     &                             lth, nsamp, ddp1hd, ddp1tr, ier)

      type(ppavo_struct), pointer                       :: obj
      integer,                            intent(inout) :: kpwks
      integer,          dimension(2),     intent(inout) :: kpwkd
      integer,                            intent(inout) :: traceno
      integer,                            intent(inout) :: lth
      integer,                            intent(inout) :: nsamp
      real,             dimension(nsamp), intent(inout) :: ddp1tr
      double precision, dimension(lth),   intent(inout) :: ddp1hd
      double precision, dimension(lth),   intent(in)    :: hd(lth)
      real,             dimension(nsamp), intent(in)    :: tr(nsamp)
      integer,                            intent(inout) :: ier

      call ppavo_vmove (hd, 1, ddp1hd, 1, lth)
      call ppavo_vmove (tr, 1, ddp1tr, 1, nsamp)
      call ppavo_fowdsd (obj, kpwks, kpwkd, traceno, ddp1hd, ddp1tr, ier)

      end subroutine ppavo_uhci_write


!!----------------------------- ppavo_upawrk ---------------------------------!!
!!----------------------------- ppavo_upawrk ---------------------------------!!
!!----------------------------- ppavo_upawrk ---------------------------------!!

!     Purpose:
!        This subroutine allocates a temporary direct access dataset.
!
      subroutine ppavo_upawrk (obj, nrec, lrecl, charx, bsamad, bdamad,&
     &                        ddname, err, erin)

      type(ppavo_struct),           intent(inout) :: obj
      integer,                      intent(inout) :: nrec
      integer,                      intent(inout) :: lrecl
      integer,                      intent(inout) :: err
      integer,                      intent(inout) :: erin
      integer,                      intent(inout) :: bsamad
      integer, dimension(2),        intent(inout) :: bdamad
      character(len=1),             intent(in)    :: charx
      character(len=9),             intent(inout) :: ddname
      
      integer :: maxtmp
      parameter (maxtmp=20)

      integer :: i, tmpnum
      integer ::             ierr   
      integer :: itotal, itotal1, itotal2, ihandle   
      integer :: memtest      , len, len1, len2, mrec, pid 
      real    :: total

      character(len=1)   :: info
      character(len=3)   :: tmpchr3
      character(len=256) :: ctmpfile
      character(len=200) :: path
      character(len=200) :: seed

 
      data tmpnum / 0 /

      pid = 0
      info = '\0'
      path = ' '
      ddname = ' '
      ctmpfile = ' '
  
!     Work file ddname convention.
!        'tempA001'
!         :   :  :
!         :   :  :
!         :   :  :___file number.
!         :   : 
!         :   :
!         :   :______character suffix.
!         :__________process name.

!     Initialization.
      if (obj%init .eq. 0) then
         do i = 1, maxtmp
            nullify(obj%ptrs(i)%dptr)
            nullify(obj%ptrs(i)%rptr)
         end do
         obj%init = 1
         write (6,9100)
      endif
      ierr = 0

      len1 = bdamad(1) / 8
      len2 = bdamad(2) / 4
      err = 1
      erin = 0      
      mrec = iabs(nrec)
      len = lrecl / 4
      total = len * mrec
      total = (bdamad(1) + bdamad(2)) * mrec / 4
      itotal = total
      itotal1 = len1 * mrec
      itotal2 = len2 * mrec
      if (lrecl .ne. len*4) goto 1040

!     Get name of temporary file.
      ddname = '        '
      ddname(1:4) = 'temp'
      ddname(5:5) = charx
      ddname(6:7) = '00'
      tmpnum = tmpnum + 1
      if (tmpnum .gt. maxtmp) goto 1050

      write (tmpchr3,9000) tmpnum      
      if (tmpnum .ge. 0)   ddname(8:8) = tmpchr3(3:3)
      if (tmpnum .ge. 10)  ddname(7:7) = tmpchr3(2:2)
      if (tmpnum .ge. 100) ddname(6:6) = tmpchr3(1:1)
!      ddname(9:9) = char(0)

!WARNING
!      call ppavo_crou_getpid(pid)
!      write(ctmpfile, *) ddname,'.',pid
!      write(*,*) 'pid = ',ddname,pid,ctmpfile
      ctmpfile = ddname
      obj%ptrs(tmpnum)%ctmpsave = ctmpfile

!     Use CPS tempname module to get new temp filename.
      seed = ddname
      path = tempname(seed)
      ctmpfile = path


!     Open the storage file. Determine type of file.
!     Maximum size to put in memory is 'memtest'.
      memtest = 15000000
      if (memtest .lt. len) memtest = len
      if (total .le. memtest) then

!        Reserve memory.
         allocate (obj%ptrs(tmpnum)%dptr(itotal1))
         allocate (obj%ptrs(tmpnum)%rptr(itotal2))
         ihandle = tmpnum

         bdamad(1) = len1
         bdamad(2) = len2

!        Final print.
         write (6,9120) ddname, mrec, len, total

      else

!        Open the file.
         ihandle = cio_fopen(ctmpfile, 'w+', .false.)

         if (ihandle .le. 0) goto 1060
         bdamad(1) = -len1
         bdamad(2) = -len2

!        Final print
         write (6,9130) ddname, mrec, len, total

      endif

!     Set the values.
      bsamad = ihandle
      obj%ptrs(tmpnum)%ihandsave  = ihandle
      obj%ptrs(tmpnum)%itotalsave = itotal

!     All done here.
1000  continue
      return

!     Error exits.
1040  err = 6
      erin = lrecl
      goto 1000

1050  err = 7
      erin = tmpnum
      goto 1000

1060  err = 8
      erin = ierr
      goto 1000

9000  format (i3)

9100  format (5x,'                                        ',&
     &           '      RECORD      TOTAL',/,&
     &        5x,'        FILE       ALLOCATED      NUMBER',&
     &           '      LENGTH       SIZE',/,&
     &        5x,'        NAME           TO         RECORDS',&
     &           '     (WORDS)     (WORDS)',/&
     &        5x,'      --------     ---------      -------',&
     &           '     -------     -------')

9120  format (11x,a8,7x,'MEMORY',3x,i9,5x,i7,f14.0)

9130  format (11x,a8,7x,' DISK ',3x,i9,5x,i7,f14.0)

      return

      end subroutine ppavo_upawrk


!!----------------------------- ppavo_uguwrk ---------------------------------!!
!!----------------------------- ppavo_uguwrk ---------------------------------!!
!!----------------------------- ppavo_uguwrk ---------------------------------!!

!     Purpose:
!        This subroutine deallocates a temporary direct access dataset.
!
      subroutine ppavo_uguwrk (obj, bsamad, bdamad, err, erin)
      
      type(ppavo_struct),    intent(inout) :: obj
      integer,               intent(inout) :: err
      integer,               intent(inout) :: erin
      integer,               intent(inout) :: bsamad
      integer, dimension(2), intent(inout) :: bdamad

      integer :: i, tmpnum
      integer ::             ierr  
      integer :: itotal, ihandle   
      character(len=256) :: ctmpfile

!     Initialize.
      ihandle = bsamad
      do 900 i = 1, tmpnum
         if (ihandle .eq. obj%ptrs(i)%ihandsave) then
            ctmpfile = obj%ptrs(i)%ctmpsave
            itotal = obj%ptrs(i)%itotalsave
            obj%ptrs(i)%ihandsave = 0
            goto 910
         endif
900   continue
      goto 1070
910   continue

!     Remove the file.
      if (bdamad(1) .ge. 0 .and. bdamad(2) .ge. 0) then

!        Memory.         
         if (associated(obj%ptrs(ihandle)%dptr)) then
            deallocate(obj%ptrs(ihandle)%dptr)
         end if
         if (associated(obj%ptrs(ihandle)%rptr)) then
            deallocate(obj%ptrs(ihandle)%rptr)
         end if

      else

!        Disk.
         ierr = cio_remove(ctmpfile)

      endif

!     All done here.
1000  continue
      return

!     Error exits.
1070  err = 11
      erin = tmpnum
      goto 1000

      end subroutine ppavo_uguwrk


!!------------------------------ ppavo_focdd ---------------------------------!!
!!------------------------------ ppavo_focdd ---------------------------------!!
!!------------------------------ ppavo_focdd ---------------------------------!!

!     Purpose:
!        This subroutine .
!
      subroutine ppavo_focdd (obj, bsamad, bdamad)
      
      type(ppavo_struct),             intent(inout) :: obj
      integer,                        intent(inout) :: bsamad
      integer,          dimension(2), intent(inout) :: bdamad
 
      integer :: ierr
      integer :: ihandle

!     Initialize.
      ihandle  = bsamad

!     Close the file.
      if (bdamad(1) .lt. 0 .and. bdamad(2) .lt. 0) then

!        Disk.
         ierr = cio_fclose(ihandle, .false.)
      end if
  
      end subroutine ppavo_focdd


!!----------------------------- ppavo_fowssd ---------------------------------!!
!!----------------------------- ppavo_fowssd ---------------------------------!!
!!----------------------------- ppavo_fowssd ---------------------------------!!

!     Purpose:
!        This subroutine .
!
      subroutine ppavo_fowssd (obj, bsamad, bdamad, seqda, datahd, datatr, ierr)
      
      type(ppavo_struct),             intent(inout) :: obj
      integer,                        intent(inout) :: bsamad
      integer,          dimension(2), intent(inout) :: bdamad
      integer,                        intent(inout) :: seqda
      real,             dimension(:), intent(inout) :: datatr
      double precision, dimension(:), intent(inout) :: datahd
      integer,                        intent(inout) :: ierr
 
      integer :: niohd, niotr      , iorigin 
      integer :: ihandle, ioffset, ioffset1, ioffset2
      integer :: len1, len2, nio1, nio2



!     Initialize.
      ierr     = 0
      ihandle  = bsamad
      len1     = iabs(bdamad(1))
      len2     = iabs(bdamad(2))
      ioffset  = (seqda-1) * ((len1*8)+(len2*4))
      ioffset1 = ((seqda-1) * len1) + 1
      ioffset2 = ((seqda-1) * len2) + 1
      iorigin  = 0

!     Write the record.
      if (bdamad(1) .ge. 0 .and. bdamad(2) .ge. 0) then

!        Memory.         
         call ppavo_vmove(datahd, 1, obj%ptrs(ihandle)%dptr(ioffset1:), 1, len1)
         call ppavo_vmove(datatr, 1, obj%ptrs(ihandle)%rptr(ioffset2:), 1, len2)

      else  

!        Disk.
         niohd = len1
         niotr = len2
         ierr = cio_fseek(ihandle, ioffset, iorigin)
         if (ierr .ne. 0) then
            write (6,9230) ierr
            return
         endif
         nio1 = cio_fwrite(datahd, 8, niohd, ihandle)
         if(nio1 .ne. niohd) then
            write(6,9240) niohd, nio1
            ierr = 2
            return
         end if
         nio2 = cio_fwrite(datatr, 4, niotr, ihandle)
         if(nio2 .ne. niotr) then
            write(6,9240) niotr, nio2
            ierr = 3
            return
         end if
      endif

!     All done here.
      seqda = seqda + 1

      return
  
9230  format (5x,'ERROR RETURNED FROM CIO_FSEEK =',i10)

9240  format (5x,'NUMBER OF WORDS REQUESTED BY CIO_FWRITE',i10,/,&
     &           ' NOT EQUAL TO NUMBER RETURNED',i10)
  
      end subroutine ppavo_fowssd


!!----------------------------- ppavo_fordsd ---------------------------------!!
!!----------------------------- ppavo_fordsd ---------------------------------!!
!!----------------------------- ppavo_fordsd ---------------------------------!!

!     Purpose:
!        This subroutine .
!
      subroutine ppavo_fordsd (obj, bsamad, bdamad, seqda, datahd, datatr, ierr)
      
      type(ppavo_struct),             intent(inout) :: obj
      integer,                        intent(inout) :: bsamad
      integer,          dimension(2), intent(inout) :: bdamad
      integer,                        intent(inout) :: seqda
      real,             dimension(:), intent(inout) :: datatr
      double precision, dimension(:), intent(inout) :: datahd
      integer,                        intent(inout) :: ierr
 
      integer :: niohd, niotr      , iorigin 
      integer :: ihandle, ioffset, ioffset1, ioffset2
      integer :: len1, len2, nio1, nio2   

!     Initialize.
      ierr     = 0
      ihandle  = bsamad
      len1     = iabs(bdamad(1))
      len2     = iabs(bdamad(2))
      ioffset  = (seqda-1) * ((len1*8)+(len2*4))
      ioffset1 = ((seqda-1) * len1) + 1
      ioffset2 = ((seqda-1) * len2) + 1
      iorigin  = 0

!     Read the record.
      if (bdamad(1) .ge. 0 .and. bdamad(2) .ge. 0) then
!        Memory.         
         call ppavo_vmove(obj%ptrs(ihandle)%dptr(ioffset1:), 1, datahd, 1, len1)
         call ppavo_vmove(obj%ptrs(ihandle)%rptr(ioffset2:), 1, datatr, 1, len2)
      else  

!        Disk.
         niohd = len1
         niotr = len2          
         ierr = cio_fseek(ihandle, ioffset, iorigin)
         if (ierr .ne. 0) then
            write (6,9250) ierr
            return
         endif
         nio1 = cio_fread(datahd, 8, niohd, ihandle)
         if(nio1 .ne. niohd) then
            write(6,9260) niohd, nio1
            ierr = 2
            return
         end if
         nio2 = cio_fread(datatr, 4, niotr, ihandle)
         if(nio2 .ne. niotr) then
            write(6,9260) niotr, nio2
            ierr = 3
            return
         end if
      endif

!     All done here.
      seqda = seqda + 1

      return

9250  format (5x,'ERROR RETURNED FROM CIO_FSEEK =',i10)

9260  format (5x,'NUMBER OF WORDS REQUESTED BY CIO_FREAD',i10,/,&
     &           ' NOT EQUAL TO NUMBER RETURNED',i10)

      end subroutine ppavo_fordsd


!!----------------------------- ppavo_fowdsd ---------------------------------!!
!!----------------------------- ppavo_fowdsd ---------------------------------!!
!!----------------------------- ppavo_fowdsd ---------------------------------!!

!     Purpose:
!        This subroutine .
!
      subroutine ppavo_fowdsd (obj, bsamad, bdamad, seqda, datahd, datatr, ierr)
      
      type(ppavo_struct),             intent(inout) :: obj
      integer,                        intent(inout) :: bsamad
      integer,          dimension(2), intent(inout) :: bdamad
      integer,                        intent(inout) :: seqda
      real,             dimension(:), intent(inout) :: datatr
      double precision, dimension(:), intent(inout) :: datahd
      integer,                        intent(inout) :: ierr
 
      integer :: niohd, niotr      , iorigin 
      integer :: ihandle, ioffset, ioffset1, ioffset2
      integer :: len1, len2, nio1, nio2   

!     Initialize.
      ierr     = 0
      ihandle  = bsamad
      len1     = iabs(bdamad(1))
      len2     = iabs(bdamad(2))
      ioffset  = (seqda-1) * ((len1*8)+(len2*4))
      ioffset1 = ((seqda-1) * len1) + 1
      ioffset2 = ((seqda-1) * len2) + 1
      iorigin  = 0

!     Write the record.
      if (bdamad(1) .ge. 0 .and. bdamad(2) .ge. 0) then
!        Memory.         
         call ppavo_vmove(datahd, 1, obj%ptrs(ihandle)%dptr(ioffset1:), 1, len1)
         call ppavo_vmove(datatr, 1, obj%ptrs(ihandle)%rptr(ioffset2:), 1, len2)
      else  

!        Disk.
         niohd = len1
         niotr = len2
         ierr = cio_fseek(ihandle, ioffset, iorigin)
         if (ierr .ne. 0) then
            write (6,9270) ierr
            return
         endif
         nio1 = cio_fwrite(datahd, 8, niohd, ihandle)
         if(nio1 .ne. niohd) then
            write(6,9280) niohd, nio1
            ierr = 2
            return
         end if
         nio2 = cio_fwrite(datatr, 4, niotr, ihandle)
         if(nio2 .ne. niotr) then
            write(6,9280) niotr, nio2
            ierr = 3
            return
         end if
      endif

!     All done here.
      seqda = seqda + 1

      return

9270  format (5x,'ERROR RETURNED FROM CIO_FSEEK =',i10)

9280  format (5x,'NUMBER OF WORDS REQUESTED BY CIO_FWRITE',i10,/,&
     &           ' NOT EQUAL TO NUMBER RETURNED',i10)

      end subroutine ppavo_fowdsd


!!------------------------------- ppavo_vadd ---------------------------------!!
!!------------------------------- ppavo_vadd ---------------------------------!!
!!------------------------------- ppavo_vadd ---------------------------------!!

!     Purpose:
!        This subroutine adds two real vectors.

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Input vector.
!        BSTEP : Input step increment.
!        C     : Output vector.
!        CSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vadd (a, astep, b, bstep, c, cstep, nsamp)

      integer,                      intent(in)  :: nsamp
      integer,                      intent(in)  :: astep
      integer,                      intent(in)  :: bstep
      integer,                      intent(in)  :: cstep
      real, dimension(astep*nsamp), intent(in)  :: a
      real, dimension(bstep*nsamp), intent(in)  :: b
      real, dimension(cstep*nsamp), intent(out) :: c

      integer :: k

      do k = 1, nsamp
         c(k*cstep) = a(k*astep) + b(k*bstep)
      end do

      return
      end subroutine ppavo_vadd


!!------------------------------- ppavo_vmul ---------------------------------!!
!!------------------------------- ppavo_vmul ---------------------------------!!
!!------------------------------- ppavo_vmul ---------------------------------!!

!     Purpose:
!        This subroutine multiplies two real vectors.

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Input vector.
!        BSTEP : Input step increment.
!        C     : Output vector.
!        CSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vmul (a, astep, b, bstep, c, cstep, nsamp)

      integer,                      intent(in)  :: nsamp
      integer,                      intent(in)  :: astep
      integer,                      intent(in)  :: bstep
      integer,                      intent(in)  :: cstep
      real, dimension(astep*nsamp), intent(in)  :: a
      real, dimension(bstep*nsamp), intent(in)  :: b
      real, dimension(cstep*nsamp), intent(out) :: c

      integer :: k

      do k = 1, nsamp
         c(k*cstep) = a(k*astep) * b(k*bstep)
      end do

      return
      end subroutine ppavo_vmul


!!------------------------------- ppavo_vneg ---------------------------------!!
!!------------------------------- ppavo_vneg ---------------------------------!!
!!------------------------------- ppavo_vneg ---------------------------------!!

!     Purpose:
!        This subroutine returns the negative of a vector.

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Output vector.
!        BSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vneg (a, astep, b, bstep, nsamp)

      integer,                      intent(in)  :: nsamp
      integer,                      intent(in)  :: astep
      integer,                      intent(in)  :: bstep
      real, dimension(astep*nsamp), intent(in)  :: a
      real, dimension(bstep*nsamp), intent(out) :: b

      integer :: k

      do k = 1, nsamp
         b(k*bstep) = -a(k*astep)
      end do

      return
      end subroutine ppavo_vneg


!!------------------------------- ppavo_vsub ---------------------------------!!
!!------------------------------- ppavo_vsub ---------------------------------!!
!!------------------------------- ppavo_vsub ---------------------------------!!

!     Purpose:
!        This subroutine subtracts two real vectors.

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Input vector.
!        BSTEP : Input step increment.
!        C     : Output vector.
!        CSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vsub (a, astep, b, bstep, c, cstep, nsamp)

      integer,                      intent(in)  :: nsamp
      integer,                      intent(in)  :: astep
      integer,                      intent(in)  :: bstep
      integer,                      intent(in)  :: cstep
      real, dimension(astep*nsamp), intent(in)  :: a
      real, dimension(bstep*nsamp), intent(in)  :: b
      real, dimension(cstep*nsamp), intent(out) :: c

      integer :: k

      do k = 1, nsamp
         c(k*cstep) = a(k*astep) - b(k*bstep)
      end do

      return
      end subroutine ppavo_vsub


!!------------------------------- ppavo_vdivz --------------------------------!!
!!------------------------------- ppavo_vdivz --------------------------------!!
!!------------------------------- ppavo_vdivz --------------------------------!!

!     Purpose:
!        This subroutine divides two real vectors (with zero test).

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Input vector.
!        BSTEP : Input step increment.
!        DVAL  : Default value, if denominator equals 0.
!        C     : Output vector.
!        CSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vdivz (a, astep, b, bstep, dval, c, cstep, nsamp)

      integer,                      intent(in)  :: nsamp
      integer,                      intent(in)  :: astep
      integer,                      intent(in)  :: bstep
      integer,                      intent(in)  :: cstep
      real,                         intent(in)  :: dval
      real, dimension(astep*nsamp), intent(in)  :: a
      real, dimension(bstep*nsamp), intent(in)  :: b
      real, dimension(cstep*nsamp), intent(out) :: c

      integer :: k

      do k = 1, nsamp
         if (b(k*bstep) .ne. 0.0) then
            c(k*cstep) = a(k*astep) / b(k*bstep)
         else
            c(k*cstep) = dval
         end if
      end do

      return
      end subroutine ppavo_vdivz


!!------------------------------- ppavo_vlogz --------------------------------!!
!!------------------------------- ppavo_vlogz --------------------------------!!
!!------------------------------- ppavo_vlogz --------------------------------!!

!     Purpose:
!        This subroutine does vector logarithm (with zero test).

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        C     : Constant.
!        B     : Output vector.
!        BSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vlogz (a, astep, c, b, bstep, nsamp)

      integer,                      intent(in)    :: nsamp
      integer,                      intent(in)    :: astep
      integer,                      intent(in)    :: bstep
      real,                         intent(in)    :: c
      real, dimension(astep*nsamp), intent(in)    :: a
      real, dimension(bstep*nsamp), intent(inout) :: b

      integer :: k

      do k = 1, nsamp
         if(a(k*astep) .le. 0.0) then
            b(k*bstep) = c
         else
            b(k*bstep) = log(a(k*astep))
         end if
      end do 

      return
      end subroutine ppavo_vlogz


!!------------------------------- ppavo_vexp ---------------------------------!!
!!------------------------------- ppavo_vexp ---------------------------------!!
!!------------------------------- ppavo_vexp ---------------------------------!!

!     Purpose:
!        This subroutine does vector exponentiation.

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Output vector.
!        BSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vexp (a, astep, b, bstep, nsamp)

      integer,                      intent(in)    :: nsamp
      integer,                      intent(in)    :: astep
      integer,                      intent(in)    :: bstep
      real, dimension(astep*nsamp), intent(in)    :: a
      real, dimension(bstep*nsamp), intent(inout) :: b

      integer :: k

      do k = 1, nsamp
         b(k*bstep) = exp(a(k*astep))
      end do

      return
      end subroutine ppavo_vexp


!!------------------------------- ppavo_vsadd --------------------------------!!
!!------------------------------- ppavo_vsadd --------------------------------!!
!!------------------------------- ppavo_vsadd --------------------------------!!

!     Purpose:
!        This subroutine adds a scalar to a vector, outputs another.

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Scalar.
!        C     : Output vector.
!        CSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vsadd (a, astep, b, c, cstep, nsamp)

      integer,                      intent(in)  :: nsamp
      integer,                      intent(in)  :: astep
      integer,                      intent(in)  :: cstep
      real,                         intent(in)  :: b
      real, dimension(astep*nsamp), intent(in)  :: a
      real, dimension(cstep*nsamp), intent(out) :: c

      integer :: k

      do k = 1, nsamp
         c(k*cstep) = a(k*astep) + b
      end do

      return
      end subroutine ppavo_vsadd


!!------------------------------- ppavo_vsmul --------------------------------!!
!!------------------------------- ppavo_vsmul --------------------------------!!
!!------------------------------- ppavo_vsmul --------------------------------!!

!     Purpose:
!        This subroutine multiples a vector by a scalar, outputs another.

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Scalar.
!        C     : Output vector.
!        CSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vsmul (a, astep, b, c, cstep, nsamp)

      integer,                      intent(in)  :: nsamp
      integer,                      intent(in)  :: astep
      integer,                      intent(in)  :: cstep
      real,                         intent(in)  :: b
      real, dimension(astep*nsamp), intent(in)  :: a
      real, dimension(cstep*nsamp), intent(out) :: c

      integer :: k

      do k = 1, nsamp
         c(k*cstep) = a(k*astep) * b
      end do

      return
      end subroutine ppavo_vsmul


!!------------------------------- ppavo_vsma ---------------------------------!!
!!------------------------------- ppavo_vsma ---------------------------------!!
!!------------------------------- ppavo_vsma ---------------------------------!!

!     Purpose:
!        This subroutine scalar multiplies and adds vectors.

!     Arguments:
!        A     : Input vector.
!        ASTEP : Input step increment.
!        B     : Scalar.
!        C     : Output vector.
!        CSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vsma (a, astep, scalar, b, bstep, c, cstep, nsamp)

      integer,                      intent(in)  :: nsamp
      integer,                      intent(in)  :: astep
      integer,                      intent(in)  :: bstep
      integer,                      intent(in)  :: cstep
      real,                         intent(in)  :: scalar
      real, dimension(astep*nsamp), intent(in)  :: b
      real, dimension(astep*nsamp), intent(in)  :: a
      real, dimension(cstep*nsamp), intent(out) :: c

      integer :: k

      do k = 1, nsamp
         c(k*cstep) = (a(k*astep) * scalar) + b(k*bstep)
      end do

      return
      end subroutine ppavo_vsma


!!------------------------------ ppavo_vdmove --------------------------------!!
!!------------------------------ ppavo_vdmove --------------------------------!!
!!------------------------------ ppavo_vdmove --------------------------------!!

!     Purpose:
!        This subroutine copies one double precision vector into another.

!     Arguments:
!        IVECT : Input vector.
!        ISTEP : Input step increment.
!        OVECT : Output vector.
!        OSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vdmove (ivect, istep, ovect, ostep, nsamp)

      integer                                 , intent(in)  :: nsamp
      integer                                 , intent(in)  :: istep
      integer                                 , intent(in)  :: ostep
      double precision, dimension(istep*nsamp), intent(in)  :: ivect
      double precision, dimension(ostep*nsamp), intent(out) :: ovect

      integer :: k

      do k = 1, nsamp
         ovect(k*ostep) = ivect(k*istep)
      end do

      return
      end subroutine ppavo_vdmove


!!----------------------------- ppavo_vrmove ---------------------------------!!
!!----------------------------- ppavo_vrmove ---------------------------------!!
!!----------------------------- ppavo_vrmove ---------------------------------!!

!     Purpose:
!        This subroutine copies one real vector into another.

!     Arguments:
!        IVECT : Input vector.
!        ISTEP : Input step increment.
!        OVECT : Output vector.
!        OSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vrmove (ivect, istep, ovect, ostep, nsamp)

      integer                     , intent(in)  :: nsamp
      integer                     , intent(in)  :: istep
      integer                     , intent(in)  :: ostep
      real, dimension(istep*nsamp), intent(in)  :: ivect
      real, dimension(ostep*nsamp), intent(out) :: ovect

      integer :: k

      do k = 1, nsamp
         ovect(k*ostep) = ivect(k*istep)
      end do

      return
      end subroutine ppavo_vrmove


!!----------------------------- ppavo_vimove ---------------------------------!!
!!----------------------------- ppavo_vimove ---------------------------------!!
!!----------------------------- ppavo_vimove ---------------------------------!!

!     Purpose:
!        This subroutine copies one integer vector into another.

!     Arguments:
!        IVECT : Input vector.
!        ISTEP : Input step increment.
!        OVECT : Output vector.
!        OSTEP : Output step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vimove (ivect, istep, ovect, ostep, nsamp)

      integer                        , intent(in)  :: nsamp
      integer                        , intent(in)  :: istep
      integer                        , intent(in)  :: ostep
      integer, dimension(istep*nsamp), intent(in)  :: ivect
      integer, dimension(ostep*nsamp), intent(out) :: ovect

      integer :: k

      do k = 1, nsamp
         ovect(k*ostep) = ivect(k*istep)
      end do

      return
      end subroutine ppavo_vimove


!!------------------------------ ppavo_vdfill --------------------------------!!
!!------------------------------ ppavo_vdfill --------------------------------!!
!!------------------------------ ppavo_vdfill --------------------------------!!

!     Purpose:
!        This subroutine fills a double precision vector with a given value.

!     Arguments:
!        FVAL  : Fill value.
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vdfill (fval, vect, step, nsamp)

      integer                                , intent(in)  :: nsamp
      integer                                , intent(in)  :: step
      double precision, dimension(step*nsamp), intent(out) :: vect
      double precision                       , intent(in)  :: fval

      integer :: k

      do k = 1, nsamp
         vect(k*step) = fval
      end do

      return
      end subroutine ppavo_vdfill


!!------------------------------ ppavo_vrfill --------------------------------!!
!!------------------------------ ppavo_vrfill --------------------------------!!
!!------------------------------ ppavo_vrfill --------------------------------!!

!     Purpose:
!        This subroutine fills a real vector with a given value.

!     Arguments:
!        FVAL  : Fill value.
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vrfill (fval, vect, step, nsamp)

      integer                    , intent(in)  :: nsamp
      integer                    , intent(in)  :: step
      real, dimension(step*nsamp), intent(out) :: vect
      real                       , intent(in)  :: fval

      integer :: k

      do k = 1, nsamp
         vect(k*step) = fval
      end do

      return
      end subroutine ppavo_vrfill


!!------------------------------ ppavo_vifill --------------------------------!!
!!------------------------------ ppavo_vifill --------------------------------!!
!!------------------------------ ppavo_vifill --------------------------------!!

!     Purpose:
!        This subroutine fills an integer vector with a given value.

!     Arguments:
!        FVAL  : Fill value.
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vifill (fval, vect, step, nsamp)

      integer                       , intent(in)  :: nsamp
      integer                       , intent(in)  :: step
      integer, dimension(step*nsamp), intent(out) :: vect
      integer                       , intent(in)  :: fval

      integer :: k

      do k = 1, nsamp
         vect(k*step) = fval
      end do

      return
      end subroutine ppavo_vifill


!!------------------------------ ppavo_vdzero --------------------------------!!
!!------------------------------ ppavo_vdzero --------------------------------!!
!!------------------------------ ppavo_vdzero --------------------------------!!

!     Purpose:
!        This subroutine initializes a double precision vector with zero values.

!     Arguments:
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vdzero (vect, step, nsamp)

      integer                                , intent(in)  :: nsamp
      integer                                , intent(in)  :: step
      double precision, dimension(step*nsamp), intent(out) :: vect

      integer :: k

      do k = 1, nsamp
         vect(k*step) = 0.0
      end do

      return
      end subroutine ppavo_vdzero


!!------------------------------ ppavo_vrzero --------------------------------!!
!!------------------------------ ppavo_vrzero --------------------------------!!
!!------------------------------ ppavo_vrzero --------------------------------!!

!     Purpose:
!        This subroutine initializes a real vector with zero values.

!     Arguments:
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vrzero (vect, step, nsamp)

      integer                    , intent(in)  :: nsamp
      integer                    , intent(in)  :: step
      real, dimension(step*nsamp), intent(out) :: vect

      integer :: k

      do k = 1, nsamp
         vect(k*step) = 0.0
      end do

      return
      end subroutine ppavo_vrzero


!!------------------------------ ppavo_vizero --------------------------------!!
!!------------------------------ ppavo_vizero --------------------------------!!
!!------------------------------ ppavo_vizero --------------------------------!!

!     Purpose:
!        This subroutine initializes an integer vector with zero values.

!     Arguments:
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vizero (vect, step, nsamp)

      integer                       , intent(in)  :: nsamp
      integer                       , intent(in)  :: step
      integer, dimension(step*nsamp), intent(out) :: vect

      integer :: k

      do k = 1, nsamp
         vect(k*step) = 0
      end do

      return
      end subroutine ppavo_vizero


!!------------------------------ ppavo_vdrvrs --------------------------------!!
!!------------------------------ ppavo_vdrvrs --------------------------------!!
!!------------------------------ ppavo_vdrvrs --------------------------------!!

!     Purpose:
!        This subroutine reverses a double precision vector in place.

!     Arguments:
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vdrvrs (vect, step, nsamp)

      integer                                , intent(in)    :: nsamp
      integer                                , intent(in)    :: step
      double precision, dimension(step*nsamp), intent(inout) :: vect

      integer          :: k, ndx
      double precision :: temp

!     Reverse order (in place).
      do k = 1, nsamp/2
         temp = vect(k*step)
         ndx = (nsamp+1-k)*step
         vect(k*step) = vect(ndx)
         vect(ndx) = temp
      end do

      return
      end subroutine ppavo_vdrvrs


!!------------------------------ ppavo_vrrvrs --------------------------------!!
!!------------------------------ ppavo_vrrvrs --------------------------------!!
!!------------------------------ ppavo_vrrvrs --------------------------------!!

!     Purpose:
!        This subroutine reverses a real vector in place.

!     Arguments:
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_vrrvrs (vect, step, nsamp)

      integer                    , intent(in)    :: nsamp
      integer                    , intent(in)    :: step
      real, dimension(step*nsamp), intent(inout) :: vect

      integer :: k, ndx
      real    :: temp

!     Reverse order (in place).
      do k = 1, nsamp/2
         temp = vect(k*step)
         ndx = (nsamp+1-k)*step
         vect(k*step) = vect(ndx)
         vect(ndx) = temp
      end do

      return
      end subroutine ppavo_vrrvrs


!!----------------------------- ppavo_virvrs ---------------------------------!!
!!----------------------------- ppavo_virvrs ---------------------------------!!
!!----------------------------- ppavo_virvrs ---------------------------------!!

!     Purpose:
!        This subroutine reverses an integer vector in place.

!     Arguments:
!        VECT  : Input vector.
!        STEP  : Input step increment.
!        NSAMP : Number of samples.

      subroutine ppavo_virvrs (vect, step, nsamp)

      integer                       , intent(in)    :: nsamp
      integer                       , intent(in)    :: step
      integer, dimension(step*nsamp), intent(inout) :: vect

      integer :: k, ndx
      integer :: temp

!     Reverse order (in place).
      do k = 1, nsamp/2
         temp = vect(k*step)
         ndx = (nsamp+1-k)*step
         vect(k*step) = vect(ndx)
         vect(ndx) = temp
      end do

      return
      end subroutine ppavo_virvrs


!!------------------------------ ppavo_vsqrtz --------------------------------!!
!!------------------------------ ppavo_vsqrtz --------------------------------!!
!!------------------------------ ppavo_vsqrtz --------------------------------!!

!     Purpose:
!        This subroutine fills a vector with square root values of another.
!        It does perform an <0 test.

!     Arguments:
!        IVECT  : Input vector.
!        ISTEP  : Input step increment.
!        SCALAR : Scalar value.
!        OVECT  : Output vector.
!        OSTEP  : Output step increment.
!        NSAMP  : Number of samples.

      subroutine ppavo_vsqrtz (ivect, istep, scalar, ovect, ostep, nsamp)

      integer                     , intent(in)  :: nsamp
      integer                     , intent(in)  :: istep
      integer                     , intent(in)  :: ostep
      real, dimension(istep*nsamp), intent(in)  :: ivect
      real, dimension(ostep*nsamp), intent(out) :: ovect
      real                        , intent(in)  :: scalar

      integer :: k

      do 100 k = 1, nsamp
         if(ivect(k*istep) .gt. 0.0) then
            ovect(k*ostep) = sqrt(ivect(k*istep))
         else
            ovect(k*ostep) = scalar
         end if
100   continue

      return
      end subroutine ppavo_vsqrtz


!!--------------------------- ppavo_chars2real ------------------------------!!
!!--------------------------- ppavo_chars2real ------------------------------!!
!!--------------------------- ppavo_chars2real ------------------------------!!

!     Purpose:
!        This subroutine decodes real values in a character array.

!     Arguments:
!        CSTR : Character array.
!        ILEN : Input length of CSTR.
!        FREQ : Output frequencies.
!        NF   : Number of output frequencies.

      subroutine ppavo_chars2real (cstr, ilen, freq, nf)

      integer,            intent(in)  :: ilen
      integer,            intent(out) :: nf
      real, dimension(:), intent(out) :: freq
      character(len=*),   intent(in)  :: cstr

      integer :: i, i1,i2

      nf = 0
      i1 = 1
      i2 = 1

      do 100 i = 1, ilen
         if (cstr(i:i) .eq. ',' .or. i .eq. ilen) then
            if (i .eq. ilen) i2 = ilen
            if (cstr(i:i) .eq. ',') i2 = i - 1
            if (i2 .ge. i1) then
               nf = nf + 1
               read (cstr(i1:i2),*) freq(nf)
               i1 = i + 1
            end if
         end if
100   continue

      return
      end subroutine ppavo_chars2real
  
  
!!---------------------------- ppavo_slash2dash ------------------------------!!
!!---------------------------- ppavo_slash2dash ------------------------------!!
!!---------------------------- ppavo_slash2dash ------------------------------!!

!     Purpose:
!        This subroutine replaces slashes (/) with dashes(-) and
!        returns the new character string length.

!     Arguments:
!        CSTR : Character array.
!        ILEN : Input length of CSTR.
!        OLEN : Output length of CSTR.

      subroutine ppavo_slash2dash (cstr, ilen, olen)

      integer,          intent(in)    :: ilen
      integer,          intent(out)   :: olen
      character(len=*), intent(inout) :: cstr

      integer :: n

      olen = 0
      do 100 n = 1, ilen
         if (cstr(n:n) .eq. '/') cstr(n:n) = '-'
         olen = olen + 1
100   continue

      return
      end subroutine ppavo_slash2dash


!!---------------------------- ppavo_uppercase -------------------------------!!
!!---------------------------- ppavo_uppercase -------------------------------!!
!!---------------------------- ppavo_uppercase -------------------------------!!

!     Purpose:
!        This subroutine converts a string to upper case.

!     Arguments:
!        CSTR : Character array.
!        CLEN : Input length of CSTR.

      subroutine ppavo_uppercase (cstr, clen)

      integer         , intent(in)    :: clen
      character(len=*), intent(inout) :: cstr

      integer           :: n, ival
      character(len=27) :: alphabet

      alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

      do n = 1, clen
         ival = ichar(cstr(n:n))
         if (ival .ge. 97 .and. ival .le. 122) then
            ival = ival - 96
            cstr(n:n) = alphabet(ival:ival)
         endif
      end do

      return
      end subroutine ppavo_uppercase


!!---------------------------- ppavo_lowercase -------------------------------!!
!!---------------------------- ppavo_lowercase -------------------------------!!
!!---------------------------- ppavo_lowercase -------------------------------!!

!     Purpose:
!        This subroutine converts a string to lower case.

!     Arguments:
!        CSTR : Character array.
!        CLEN : Input length of CSTR.

      subroutine ppavo_lowercase (cstr, clen)

      integer         , intent(in)    :: clen
      character(len=*), intent(inout) :: cstr

      integer           :: n, ival
      character(len=27) :: alphabet

      alphabet = 'abcdefghijklmnopqrstuvwxyz'

      do n = 1, clen
         ival = ichar(cstr(n:n))
         if (ival .ge. 97 .and. ival .le. 122) then
            ival = ival - 96
            cstr(n:n) = alphabet(ival:ival)
         endif
      end do

      return
      end subroutine ppavo_lowercase


!!----------------------------- ppavo_strlen ---------------------------------!!
!!----------------------------- ppavo_strlen ---------------------------------!!
!!----------------------------- ppavo_strlen ---------------------------------!!

!     Purpose:
!        This subroutine returns the number of characters in a string before
!        the first blank, null, or unprintable character.

!     Arguments:
!        CSTR : Character array.
!        ILEN : Input length of CSTR.
!        OLEN : Output length of CSTR.

      subroutine ppavo_strlen (cstr, ilen, olen)

      integer         , intent(in)    :: ilen
      integer         , intent(out)   :: olen
      character(len=*), intent(inout) :: cstr

      integer :: n, ival
      logical :: endstr

      olen = 0
      endstr = .false.
      do 100 n = 1, ilen
         ival = ichar(cstr(n:n))
         if (.not. endstr) then
            if (cstr(n:n) .eq. ' ' .or. cstr(n:n) .eq. char(0) .or. &
     &         ival .lt. 33  .or. ival .gt. 126) then
               olen = n - 1
               endstr = .true.
            end if
         end if
100   continue

      return
      end subroutine ppavo_strlen


!!----------------------------- ppavo_strndx ---------------------------------!!
!!----------------------------- ppavo_strndx ---------------------------------!!
!!----------------------------- ppavo_strndx ---------------------------------!!

!     Purpose:
!        This subroutine returns the index of the last non-blank character
!        in a string, or the last character before a null.

!     Arguments:
!        CSTR : Character array.
!        ILEN : Input length of CSTR.
!        OLEN : Output length of CSTR.

      subroutine ppavo_strndx (cstr, ilen, olen)

      integer         , intent(in)    :: ilen
      integer         , intent(out)   :: olen
      character(len=*), intent(inout) :: cstr

      integer :: n, ival
      logical :: endstr

      olen = 0
      endstr = .false.
      do 100 n = 1, ilen
         ival = ichar(cstr(n:n))
         if (ival .eq. 0) then
            endstr = .true.
         end if
         if (.not. endstr) then
            if (cstr(n:n) .ne. ' ') then
               olen = n
            end if
         end if
100   continue

      return
      end subroutine ppavo_strndx


!!------------------------------ ppavo_xtrap ---------------------------------!!
!!------------------------------ ppavo_xtrap ---------------------------------!!
!!------------------------------ ppavo_xtrap ---------------------------------!!

!     Purpose:
!        This subroutine extrapolates extremities of a vector.

!     Arguments:
!        A  : Data array.
!        NS : Number of samples.
!        NF : Index of first live sample.
!        NL : Index of last live sample.

      subroutine ppavo_xtrap (a, ns, nf, nl)

      integer            , intent(in)    :: ns
      integer            , intent(in)    :: nf
      integer            , intent(in)    :: nl
      real, dimension(ns), intent(inout) :: a

      integer          :: i, navg, limit
      double precision :: sum

      parameter (navg=5)

!     Average the first 'navg' live samples.
      if (nf .gt. 1) then
         sum = 0
         limit = min0(nf+navg, ns)
         do 100 i=nf, limit
100      sum = sum + a(i)
         sum = sum / (limit - nf + 1)

!        Extrapolate the beginning.
         do 200 i=1, nf
200      a(i) = sum
      endif

!     Average the last navg live samples
      if (nl .lt. ns) then
         sum = 0
         limit = max0(nl-navg, 1)
         do 300 i=limit, nl
300      sum = sum + a(i)
         sum = sum / (nl - limit + 1)
!        Extrapolate the beginning.
         do 400 i=nl, ns
400      a(i) = sum
      endif

      return
      end subroutine ppavo_xtrap


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module ppavo_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

