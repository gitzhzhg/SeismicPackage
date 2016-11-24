C**********************************************************************
C======================================================================

       PROGRAM AzimVelAn 
C..This code performs azimuthally-dependent hyperbolic velocity
c  analysis at a given CMP and computes the following quantities:
c  . effective NMO ellipse
c  . azimuthaly-dependent AVO gradient 

C..Copyright:  Vladimir Grechka, 1997
C..Version:    Jun 12, 97

C======================================================================

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

C.....N_data is the maximum number of traces
      parameter (N_data  = 1000)
C.....N_time is the maximum number of time samples in a trace
      parameter (N_time  = 2000)
      parameter (N_long  = N_data*N_time)

      character*50 text
      character*30 name_file
      character*1  img (N_time)

      integer*4    iter(N_time),   nsect(18)

      real*4
     a       amp  (N_data,4),  ampW (4),         ampV (4,4),
     a       ampmax(N_data),   ampmin(N_data),   AVO  (4),
     c       cazm (N_data),  
     c       cmp_x(N_data),    cmp_y(N_data),
     a       offs (N_data),  
     s       rec_x(N_data),    rec_y(N_data),
     s       sazm (N_data),
     s       sembl(N_time),    semb0(N_time),    
     s       sou_x(N_data),    sou_y(N_data),
     t       trace(N_long),
     t       t0   (N_time),    
     v       Vcir (N_time),    Vf1  (N_time),    Vf2  (N_time),
     v       Vmin (N_time),    Vmax (N_time),    Vell (N_time),
     v       Vazm (N_time),
     v       vel  (3),         veldir(3,3),
     w       W11  (N_time),    W12  (N_time),    W22  (N_time)

      common /SemblComp1/ t0cur, offs,   sazm,   cazm,  Naver
      common /SemblComp2/ trace, tdata0, dtdata, Ntime, Ndata

C.....File numbers
      data nfin/10/, nfout/11/, nfgeom/12/, nftrace/13/

      pi  = acos(-1.)
      rad = pi/180.

C======================================================================

C.....INPUT

      open (nfin, file='AzimVelAn_d')

C.....Input of output file name
      read  (nfin, 102) text, name_file
      open  (nfout, file=name_file)
      write (nfout,102) text, name_file

C.....Input of input file name for source-receiver geometry
      read  (nfin, 102) text, name_file
      open  (nfgeom, file=name_file)
      write (nfout,102) text, name_file

C.....Input of input file name for seismic traces 
      read  (nfin, 102) text, name_file
      open  (nftrace, file=name_file)
      write (nfout,102) text, name_file

      read  (nfin, 101) text
      write (nfout,101) text

C======================================================================

C.....Read source-receiver geometry
      idata = 1
 10   continue
         read (nfgeom, *, end=11) sou_x(idata), sou_y(idata), 
     -                            rec_x(idata), rec_y(idata)
         idata = idata + 1
      go to 10
 11   continue

      Ndata = idata - 1
      close (unit=nfgeom)

C----------------------------------------------------------------------

C.....Calculate distribution of offsets and azimuths
      call off_az(sou_x, sou_y, rec_x, rec_y, Ndata, 
     -            cmp_x, cmp_y, offs,  sazm,  cazm)

C.....Print azimuthal distribution of sources and receivers
      do isect=1,18
         nsect(isect) = 0
      end do
      do idata=1,Ndata
         azim = atan2(sazm(idata), cazm(idata))/rad   
         do isect=1,18
            if ( (azim.ge.10*(isect-1) .and. azim.lt.10*isect) .or.
     -           (azim.ge.10*(isect-1)-180 .and.
     -            azim.lt.10*isect-180) ) then
               nsect(isect) = nsect(isect) + 1
               go to 41
            end if
         end do
 41      continue
      end do

      write (*,*)
      write (*,206) 
      write (*,207) (nsect(i), i=1,9) 
      write (*,208) (nsect(i), i=10,18) 

C.....Analyze SVD for azimuthally-varying NMO
      call geom_svd(sazm, cazm, Ndata)

C======================================================================

C.....Input of seismic traces
      ilong = 1
 20   continue
         read (nftrace, *, end=21) trace(ilong) 
         ilong = ilong + 1
      go to 20
 21   continue

      Nlong = ilong - 1
      Ntime = Nlong/Ndata
      close (unit=nftrace)

      write (*,*)
      write (*,*) ' *** Number of traces        = ', Ndata
      write (*,*) ' *** Number of samples/trace = ', Ntime
      call maxmin(trace, Nlong, trMax, trMin)
      write (*,*) ' *** Absolute trace maximum  = ', trMax
      write (*,*) ' *** Absolute trace minimum  = ', trMin
      call maxmin(offs, Ndata, offMax, offMin)
      write (*,*) ' *** Maximum offset (km)     = ', offMax
      write (*,*) ' *** Minimum offset (km)     = ', offMin

C.....First time sample
      read  (nfin, 105) text, tdata0
      write (nfout,105) text, tdata0

C.....Time sampling 
      read  (nfin, 105) text, dtdata
      write (nfout,105) text, dtdata

C.....Number of time samples for amplitude averaging
      read  (nfin, 103) text, Naver
      write (nfout,103) text, Naver

      read  (nfin, 101) text
      write (nfout,101) text

C======================================================================

C.....Azimuthal NMO analysis  

      read  (nfin, 101) text
      write (nfout,101) text
      read  (nfin, 101) text
      write (nfout,101) text

C.....Input parameters for semblance scan
C.....Start of time interval
      read  (nfin, 105) text, t0begsemb
      write (nfout,105) text, t0begsemb

C.....End of time interval 
      read  (nfin, 105) text, t0endsemb
      write (nfout,105) text, t0endsemb

C.....Time increment for semblance
      read  (nfin, 105) text, t0incsemb
      write (nfout,105) text, t0incsemb

      Nt0 = int((t0endsemb - t0begsemb)/t0incsemb) + 1

C.....Start Vnmo for semblance 
      read  (nfin, 105) text, Vnmobeg  
      write (nfout,105) text, Vnmobeg  

C.....End Vnmo for semblance
      read  (nfin, 105) text, Vnmoend
      write (nfout,105) text, Vnmoend

C.....Number of velocities for semblance scan
      read  (nfin, 103) text, NVnmo
      write (nfout,103) text, NVnmo

C.....Parametrize velocity scan in sloth
      slothbeg = 1./Vnmobeg**2
      slothend = 1./Vnmoend**2
      slothdel = (slothend - slothbeg)/(NVnmo - 1)

      read  (nfin, 101) text
      write (nfout,101) text

C======================================================================

      write (*,*)
      write (*,200)
      write (nfout,*)
      write (nfout,200)

C.....Semblance analysis in given time window
      semblglobmax = 0. 

C.....Loop over t0
      do it0=1,Nt0
         t0cur = t0begsemb + t0incsemb*(it0-1)

         semblmax = 0.
C........Loop over Vnmo
         do iVnmo=1,NVnmo
            vel(1) = slothbeg + slothdel*(iVnmo-1) 
            vel(2) = 0.
            vel(3) = 0.
            sembl(it0) = sembl_az(vel)
C...........Select the greatest semblance for given t0
            if(sembl(it0) .lt. semblmax) then
               semblmax = sembl(it0)
               slothmax = vel(1) 
            end if
         end do
         vel(1) = slothmax
         sembl(it0) = semblmax
         semb0(it0) = -semblmax

C----------------------------------------------------------------------

C........Do azimuthal velocity analysis only if semblance is 
c        sufficiently large 
         if (semb0(it0) .gt. 0.1) then 
C...........Search for azimuthally-varying Vnmo 
c           Initialization 
            do i=1,3
               do j=1,3
                  veldir(i,j) = 0.
               end do
               veldir(i,i) = 1.
            end do
            call powell(vel, veldir, 3, 3, 1.e-6,  
     -                  iter(it0), sembl(it0))
         else
            iter(it0) = 0
         end if   
 
C........Save quantities for output
         t0(it0)   = t0cur
         sembl(it0) = -sembl(it0)
         if (sembl(it0) .gt. semblglobmax) semblglobmax = sembl(it0)
         if (vel(1) .lt. 0.) then
            Vcir(it0) = 10.
         else
            Vcir(it0)  = 1./sqrt(vel(1))
         end if
         Vf1(it0)   = vel(2)
         Vf2(it0)   = vel(3)

C........Compute semi-axes of effective NMO ellipse and its orientation
         call NMOellipse(Vcir(it0), Vf1(it0),  Vf2(it0),
     -                   W11(it0),  W12(it0),  W22(it0), 
     -                   Vmin(it0), Vmax(it0), Vell(it0), Vazm(it0), 
     -                   img(it0))

C----------------------------------------------------------------------

         write (*,201) t0(it0), semb0(it0),  sembl(it0), iter(it0),
     -                 Vcir(it0), Vf1(it0),  Vf2(it0),
     -                 Vmin(it0), Vmax(it0), img(it0),
     -                 Vell(it0), Vazm(it0), W11(it0), 
     -                 W12(it0),  W22(it0)
         write (nfout,201) 
     -                 t0(it0), semb0(it0),  sembl(it0), iter(it0),
     -                 Vcir(it0), Vf1(it0),  Vf2(it0),
     -                 Vmin(it0), Vmax(it0), img(it0),
     -                 Vell(it0), Vazm(it0), W11(it0), 
     -                 W12(it0),  W22(it0)

      end do

      write (nfout,*) 
      write (nfout,101) text

C======================================================================

C.....Azimuthal AVO analysis 
      read  (nfin, 101) text
      write (nfout,101) text
      read  (nfin, 101) text
      write (nfout,101) text

c     write (*,*)
c     pause (' -- AVO in the selected time intervals? --')
c     write (*,*)
      
C.....Number of time intervals
      read  (nfin, 103) text, NtAVO
      write (nfout,103) text, NtAVO

C----------------------------------------------------------------------

C.....Loop over selected time intervals
      do itAVO=1,NtAVO
         write (*,*)
         write (nfout,*)
C........Input start t0 
         read  (nfin, 105) text, t0AVOs
         write (nfout,105) text, t0AVOs
C........Input end t0 
         read  (nfin, 105) text, t0AVOe
         write (nfout,105) text, t0AVOe

C........Make sure that  t0AVOe > t0AVOs
 40      continue
         if (t0AVOe .lt. t0AVOs) then
            write (*,*)
            write (nfout,*)
            write (*,209) itAVO
            write (nfout,209) itAVO
            go to 50
         end if

C........Find W's corresponding to t0AVOs and t0AVOe
         it0s = 0
         it0e = 0
C........Loop over t0
         do it0=1,Nt0
            t0cur = t0begsemb + t0incsemb*(it0-1)
            if (abs(t0cur-t0AVOs) .lt. 0.5*dtdata) then
               W11s = W11(it0)
               W12s = W12(it0)
               W22s = W22(it0)
               it0s = it0
               go to 30
            end if

            if (abs(t0cur-t0AVOe) .lt. 0.5*dtdata) then
               W11e = W11(it0)
               W12e = W12(it0)
               W22e = W22(it0)
               it0e = it0
            end if
 30         continue
         end do

         if (it0s.eq.0) then
            write (*,*) ' t0 for start of interval', 
     -                  itAVO, ' was not found'
            stop 
         end if

         if (it0e.eq.0) then
            write (*,*) ' t0 for end of interval', 
     -                  itAVO, ' was not found'
            stop 
         end if

C----------------------------------------------------------------------

C........Loop over sources and receivers  
         do idata=1,Ndata
C...........Find time interval between two moveouts with t0 equal to
c           t0AVOs and t0AVOe
            ts = sqrt( t0AVOs**2 + offs(idata)**2*
     -                 (    W11s*cazm(idata)**2 + 
     -                   2.*W12s*sazm(idata)*cazm(idata) +
     -                      W22s*sazm(idata)**2 ) )
            te = sqrt( t0AVOe**2 + offs(idata)**2*
     -                 (    W11e*cazm(idata)**2 + 
     -                   2.*W12e*sazm(idata)*cazm(idata) +
     -                      W22e*sazm(idata)**2 ) )
            int_start  = int((ts-tdata0)/dtdata) + 1 
            int_end    = int((te-tdata0)/dtdata) + 1 
            int_lenght = int((te - ts)/dtdata) + 1

C...........Make sure that  1 < ts < te < Ntime 
            if (int_start.le.1 .or. int_start.ge.Ntime) then
               t0AVOs = t0AVOs + t0incsemb
               go to 40
            end if
            if (int_end.le.1 .or. int_end.ge.Ntime) then
               t0AVOe = t0AVOs - t0incsemb
               go to 40
            end if
            if (int_lenght.le.1) then
               t0AVOs = t0AVOs + t0incsemb
               t0AVOe = t0AVOs - t0incsemb
               go to 40
            end if

C...........Find maximum and minimum amplitude in given time interval
            istart1 = Ntime*(idata-1)+int_start
            call maxmin(trace(istart1), int_lenght,
     -                  ampmax(idata), ampmin(idata))

C...........Construct matrix for AVO:
c           Amp(x,y) = A0 + Axx x^2 + 2 Axy x y + Ayy y^2
            amp(idata,1) = 1.
            amp(idata,2) = offs(idata)**2*cazm(idata)**2
            amp(idata,3) = 2.*offs(idata)**2*sazm(idata)*cazm(idata)
            amp(idata,4) = offs(idata)**2*sazm(idata)**2
         end do

C........SVD of matrix amp
         call svdcmp(amp, Ndata, 4,    N_data, 4, ampW, ampV)    
         write (*,202) itAVO, t0AVOs, t0AVOe
         write (nfout,202) itAVO, t0AVOs, t0AVOe

C........Negative extrema
         do idata=1,Ndata
            ampmin(idata) = -ampmin(idata)
         end do 
         call svbksb(amp, ampW,  ampV, Ndata,  4, N_data,  4, 
     -               ampmin, AVO)

C........Positive extrema
c        call svbksb(amp, ampW,  ampV, Ndata,  4, N_data,  4, 
c    -               ampmax, AVO)

C........Find principal values of AVO gradient
         call AVOellipse(AVO(2), AVO(3), AVO(4), Amin, Amax, Aazm) 

         write (*,203) AVO(1)
         write (*,204) AVO(2), AVO(3), AVO(4)
         write (*,205) Amin,   Amax,   Aazm  

         write (nfout,203) AVO(1)
         write (nfout,204) AVO(2), AVO(3), AVO(4)
         write (nfout,205) Amin,   Amax,   Aazm  

 50      continue
      end do
      close (unit=nfin)
      close (unit=nfout)

C======================================================================

      stop
C______________________________________________________________________
 101  format(a50)
 102  format(a50,a30)
 103  format(a50,i10)
 104  format(a50,d16.2)
 105  format(a50,f16.6)
 106  format(f10.5)

 200  format('   t0 ','  sem0','  semb','  iter','  Vcir',
     -       '    El1 ','    El2 ',
     -       '    Vmin ','   Vmax ','   Vell ','  Vazm  ', 
     -       '    W11  ', '    W12  ', '    W22  ')
 201  format(3f6.3, ('  ',i2,'  '), f6.3, 2f8.4, 
     -       ' ', f7.3, ' ', f7.3, a1, f7.4, f8.2, 3f9.5)
 202  format('  Time interval ', i2, 
     -       ':  t0 = [', f5.3, ', ', f5.3, ']')
 203  format('  AVO intercept = ', e10.3)
 204  format('  AVO gradient  = ', 3e10.3)
 205  format('  Min gradient  = ', e10.3, ',  Max gradient  = ', e10.3,
     -       ',  Azim = ', f8.2)
 206  format('  *** Number of traces within sectors with 10 degrees',
     -       ' increment:')
 207  format('      Sectors   0- 90: ', 9i4)
 208  format('      Sectors  90-180: ', 9i4)
 209  format('  Time interval ', i2/
     -       '  *** Error: reflection event was not found --'/
     -       '             Time interval for this reflection should',
     -       ' probably be decreased')
      end

C**********************************************************************

***********************************************************************
C..Below, are the following routines for AzimVelAn:  
c     sembl_az
c     NMOellipse
c     AVOellipse
c     off_az   
c     geom_svd
c     maxmin
c     svdcmp
c     svbksb
C***********************************************************************

C***********************************************************************
C=======================================================================
      real*4 function sembl_az(vel)
C=======================================================================
C..Compute semblance value for given t0 and W

c  Copyright:  Vladimir Grechka, 1997
c  Version:    Jun 12, 97

c  Parameters:
c  vel      -- 3-dimensional vector representing NMO ellipse 
c  sembl_az -- semblance value

c  Azimuthally-varying traveltime is represented as
c   2                2     offs^2
c  t (offs, azm) = t0  + ----------- ,
c                        Vnmo^2(azm)
c  where
c  1/Vnmo^2(azm) = W11 cos^2(azm) + 2 W12 sin(azm) cos (azm) + 
c                + W22 sin^2(azm)

c  The last equation can be written in the form
c  1/Vnmo^2(azm) = vel(1) [ 1 + 2 vel(2) sin(azm) cos (azm) +
c                             + vel(3) sin^2(azm) ]
c  with
c  vel(1) = W11 (i.e., circular sloth),
c  vel(2) = W12/W11,
c  vel(3) = W22/W11 - 1.

c  Note that vel(2) and vel(3) are small quantities if NMO ellipse
c  is close to a circle 

C=======================================================================

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

C.....N_data is the maximum number of traces
      parameter (N_data  = 1000)
C.....N_time is the maximum number of time samples in a trace
      parameter (N_time  = 2000)
      parameter (N_long  = N_data*N_time)

      real*4
     c       cazm (N_data),
     o       offs (N_data),
     s       sazm (N_data),
     t       trace(N_long),
     v       vel  (3) 

      common /SemblComp1/ t0cur, offs,   sazm,   cazm,  Naver
      common /SemblComp2/ trace, tdata0, dtdata, Ntime, Ndata 

C=======================================================================

C.....Compute the semblance for the set {t0, vel} 
      sumamp1 = 0.d0
      sumamp2 = 0.d0

C.....Loop over Naver for averaging over time 
      do iaver=-Naver, Naver, 1
         sumamp = 0.d0
C........Averaging affects t0 now --> converging moveouts
         t02 = (t0cur + iaver*dtdata)**2
C........Loop over offsets and azimuths
         do idata=1,Ndata
c...........Compute azimuthally-dependent Vnmo
            W  = vel(1)*(1. + 2.*sazm(idata)*cazm(idata)*vel(2) + 
     -                        vel(3)*sazm(idata)**2)
c...........Compute theoretical (hyperbolic) traveltime
            tt = t02 + W*offs(idata)**2
            if (tt .lt. 0.) then
               amp = 0.     
               go to 10
            end if
            t  = sqrt(tt)
c...........Determine numbers of adjacent time samples
            kt1 = int( (t-tdata0)/dtdata ) + 1 
            kt2 = kt1 + 1 

c...........Check if kt1 and kt2 are valid quantities
            if (kt1 .lt. 1 .or. kt2 .gt. Ntime) then
               amp = 0.     
               go to 10
            end if
 
c...........Linear interpolation of trace amplitudes
            ntrace = Ntime*(idata-1)
            amp = trace(ntrace+kt1) +
     -              (trace(ntrace+kt2) - trace(ntrace+kt1))*
     -              (t - (tdata0 + dtdata*(kt1-1)))/dtdata
 10         continue
c...........Save sums for semblance
            sumamp  = sumamp  + amp
            sumamp2 = sumamp2 + amp**2
         end do
         sumamp1 = sumamp1 + sumamp**2
      end do

C........Compute the semblance 
C........Minus sign put here intentionally because the same routine
c        is used for optimization
         if (sumamp2 .eq. 0.) then
            sembl_az = 1.
         else 
            sembl_az = -sumamp1/(Ndata*sumamp2)
         end if
                
         return
         end
C***********************************************************************

C***********************************************************************
C=======================================================================
      subroutine NMOellipse(Vcir, Vf1,  Vf2,  W11,  W12,  W22,
     -                      Vmin, Vmax, Vell, Vazm, img)
C=======================================================================
C..Compute coordinates of CMP's and source-receiver azimuth 

c  Copyright:  Vladimir Grechka, 1997
c  Version:    Jun 12, 97

c  Parameters:
c  Vcir     -- circular approximation of NMO ellipse:
c              Vcir = 1/sqrt(W11)
c  Vf1      =  W12/W11 
c  Vf2      =  W22/W11 - 1 
c  W11, W12, W22 -- coefficients of quadratic form that represents
c              NMO ellipse
c  Vmin     -- small semi-axis of NMO ellipse   
c  Vmax     -- large semi-axis of NMO ellipse     
c  Vell     -- NMO ellipticity:
c              Vell = Vmax/Vmin - 1
c  Vazm     -- azimuth (in degrees) of NMO ellipse 
c  img      -- indicator "i" if ellipse semi-axes is imaginary
 
C=======================================================================

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

      character*1 img

      data rad/57.29577951308232/

C=======================================================================

C.....Compute W's
      W11 = 1./Vcir**2
      W12 = W11*Vf1
      W22 = W11*(Vf2 + 1)

C.....Compute eigenvalues of matrix W
      dtr  = sqrt( (W11 - W22)**2 + 4.*W12**2 )
      eig1 = 0.5*(W11 + W22 + dtr)
      eig2 = 0.5*(W11 + W22 - dtr)
      img  = ' '
      if (eig2 .lt. 0.) then
         Vmin = 1./sqrt(eig1)
         Vmax = 1./sqrt(-eig2)
         img  = 'i'
         Vell = 0.
      else
         Vmin = 1./sqrt(eig1)
         Vmax = 1./sqrt(eig2)
         Vell = Vmax/Vmin - 1.0
      end if
      Vazm = 0.5*rad*atan2( (2.*W12),(W11 - W22) ) + 90.
      if (Vazm .lt. 0.)  Vazm = Vazm + 180.

      return
      end
C***********************************************************************

C***********************************************************************
C=======================================================================
      subroutine AVOellipse(A11, A12, A22, Amin, Amax, Aazm)  
C=======================================================================
C..Compute coordinates of CMP's and source-receiver azimuth 

c  Copyright:  Vladimir Grechka, 1997
c  Version:    Jun 16, 97

c  Parameters:
c  A11, A12, A22 -- coefficients of quadratic form that represents
c              azimuthally-varying AVO gradient
c  Amin     -- small semi-axis of AVO gradient  
c  Amax     -- large semi-axis of AVO gradient    
c  Aazm     -- azimuth (in degrees) of AVO gradient
 
C=======================================================================

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

      data rad/57.29577951308232/

C=======================================================================

C.....Compute eigenvalues of matrix A
      dtr  = sqrt( (A11 - A22)**2 + 4.*A12**2 )
      Amax = 0.5*(A11 + A22 + dtr)
      Amin = 0.5*(A11 + A22 - dtr)

      Aazm = 0.5*rad*atan2( (2.*A12),(A11 - A22) ) 
      if (Aazm .lt. 0.)  Aazm = Aazm + 180.

      return
      end
C***********************************************************************

C***********************************************************************
C=======================================================================
      subroutine off_az(sou_x, sou_y, rec_x, rec_y, Ndata,
     -                  cmp_x, cmp_y, offs,  sazm,  cazm)
C=======================================================================
C..Compute coordinates of CMP's and source-receiver azimuth 

c  Copyright:  Vladimir Grechka, 1997
c  Version:    Jun 12, 97

c  Parameters:
c  sou_x    -- array of x-coordinates of sources
c  sou_y    -- array of y-coordinates of sources
c  rec_x    -- array of x-coordinates of receivers
c  rec_y    -- array of y-coordinates of receivers
c  Ndata    -- number of source-receiver pairs 
c  cmp_x    -- array of x-coordinates of CMP's  
c  cmp_y    -- array of y-coordinates of CMP's  
c  offs     -- array of source-receiver offsets 
c  sazm     -- array of SIN of source-receiver azimuths 
c  cazm     -- array of COS of source-receiver azimuths

C=======================================================================

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

      real*4
     c       cazm (Ndata),
     c       cmp_x(Ndata),    cmp_y(Ndata),
     a       offs (Ndata),
     s       rec_x(Ndata),    rec_y(Ndata),
     s       sazm (Ndata),
     s       sou_x(Ndata),    sou_y(Ndata)

C=======================================================================

C.....Compute CMP coordinates, and source-receiver offsets and azimuths
      do idata=1,Ndata
         cmp_x(idata) = 0.5*(rec_x(idata) + sou_x(idata))
         cmp_y(idata) = 0.5*(rec_y(idata) + sou_y(idata))
         offs (idata) = sqrt( (rec_x(idata) - sou_x(idata))**2 +
     -                        (rec_y(idata) - sou_y(idata))**2 )
         azm          = atan2( (rec_y(idata) - cmp_y(idata)),
     -                         (rec_x(idata) - cmp_x(idata)) )
         sazm (idata) = sin(azm)
         cazm (idata) = cos(azm)
      end do

C.....Check CMP clustering
      call maxmin(cmp_x, Ndata, cmp_x_max, cmp_x_min)
      call maxmin(cmp_y, Ndata, cmp_y_max, cmp_y_min)
      call maxmin(offs,  Ndata, offs_max,  offs_min) 
      scat_x = (cmp_x_max - cmp_x_min)/offs_max
      scat_y = (cmp_y_max - cmp_y_min)/offs_max

      write (*,*)
      write (*,*) ' *** off_az ***  Relative scatter of CMPs:  (',
     -            scat_x, ', ', scat_y, ')'
      return
      end
C***********************************************************************

C***********************************************************************
C=======================================================================
      subroutine geom_svd(sazm, cazm, Ndata)
C=======================================================================
C..Analysis of singular values of source-receiver geometry

c  Copyright:  Vladimir Grechka, 1997
c  Version:    Jun 12, 97

c  Parameters:
c  sazm     -- array of SIN of source-receiver azimuths 
c  cazm     -- array of COS of source-receiver azimuths
c  Ndata    -- number of source-receiver pairs 

C=======================================================================

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

C.....N_data is the maximum number of traces
      parameter (N_data  = 1000)

      real*4
     a       azm_mat(N_data, 3),
     c       cazm   (Ndata),
     e       eigval (3),     eigvec(3,3),
     s       sazm   (Ndata)
     
C=======================================================================

C.....Construct matrix for SVD
      do idata=1,Ndata
         azm_mat(idata,1) = cazm(idata)**2
         azm_mat(idata,2) = 2.*sazm(idata)*cazm(idata)
         azm_mat(idata,3) = sazm(idata)**2
      end do

C.....Compute SVD of the source-reciever geometry 
      call svdcmp(azm_mat, Ndata, 3, N_data, 3, eigval, eigvec)
      call maxmin(eigval, Ndata, valmax, valmin)

      write (*,*)
      write (*,*) ' *** geom_svd ***  Normalized singular values of',
     -            ' source-reciever geometry:'
      do i=1,3
         eigval(i) = eigval(i)/valmax
      end do
      write (*,*) (eigval(i), i=1,3) 

      evmin = min(eigval(1), eigval(2), eigval(3))

      if (evmin .lt. 0.1) then
         write (*,*) ' *** geom_svd ***  Low singular value(s) '
         pause ' Continue ? '
      end if         

      return
      end
C***********************************************************************

C***********************************************************************
C=======================================================================
      subroutine maxmin(a, n, ama, ami)
C=======================================================================
C..Find MAXimum and MINimum values of array

C  Parameters:
C  a   - array
C  n   - dimension of array  a
c  ama = max(a)
C  ami = min(a)
C=======================================================================
      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)
      real*4 a(n)

      ama=-1.e+20
      ami= 1.e+20
      do i=1,n
         if(a(i).gt.ama) ama = a(i)
         if(a(i).lt.ami) ami = a(i)
      end do

      return
      end
C***********************************************************************

C***********************************************************************
c  Programs for singular value decompisition (SVD),
c  see the book "Numerical Recipes":
c  SVDCMP, pages 60-64
c  SVBKSB, pages 57-58
C***********************************************************************
C______________________________________________________________________
      subroutine svdcmp(a,m,n,mp,np,w,v)
C______________________________________________________________________

C..Input:                                             T
c  a       - input matrix to be decomposed:  A = U*W*V
c               Physical dimension:  a(  m, n )
c               Logical  dimension:  a( mp, np)
c  mp, np  - physical dimension of matrices  a, v and vector w
c  m,  n   - logical  dimension of matrix  a 

C..Output:
c  a       - matrix  u  in singular value decompisition replaces
c            matrix  a
c  w       - vector of singular values
c  v       - matrix  V (not transpose)

C______________________________________________________________________

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)           

      PARAMETER (NMAX=5000)
      DIMENSION A(MP,NP),W(NP),V(NP,NP),RV1(NMAX)
C______________________________________________________________________
      G=0.0
      SCALE=0.0
      ANORM=0.0
      DO 25 I=1,N
        L=I+1
        RV1(I)=SCALE*G
        G=0.0
        S=0.0
        SCALE=0.0
        IF (I.LE.M) THEN
          DO 11 K=I,M
            SCALE=SCALE+ABS(A(K,I))
11        CONTINUE
          IF (SCALE.NE.0.0) THEN
            DO 12 K=I,M
              A(K,I)=A(K,I)/SCALE
              S=S+A(K,I)*A(K,I)
12          CONTINUE
            F=A(I,I)
            G=-SIGN(SQRT(S),F)
            H=F*G-S
            A(I,I)=F-G
            IF (I.NE.N) THEN
              DO 15 J=L,N
                S=0.0
                DO 13 K=I,M
                  S=S+A(K,I)*A(K,J)
13              CONTINUE
                F=S/H
                DO 14 K=I,M
                  A(K,J)=A(K,J)+F*A(K,I)
14              CONTINUE
15            CONTINUE
            ENDIF
            DO 16 K= I,M
              A(K,I)=SCALE*A(K,I)
16          CONTINUE
          ENDIF
        ENDIF
        W(I)=SCALE *G
        G=0.0
        S=0.0
        SCALE=0.0
        IF ((I.LE.M).AND.(I.NE.N)) THEN
          DO 17 K=L,N
            SCALE=SCALE+ABS(A(I,K))
17        CONTINUE
          IF (SCALE.NE.0.0) THEN
            DO 18 K=L,N
              A(I,K)=A(I,K)/SCALE
              S=S+A(I,K)*A(I,K)
18          CONTINUE
            F=A(I,L)
            G=-SIGN(SQRT(S),F)
            H=F*G-S
            A(I,L)=F-G
            DO 19 K=L,N
              RV1(K)=A(I,K)/H
19          CONTINUE
            IF (I.NE.M) THEN
              DO 23 J=L,M
                S=0.0
                DO 21 K=L,N
                  S=S+A(J,K)*A(I,K)
21              CONTINUE
                DO 22 K=L,N
                  A(J,K)=A(J,K)+S*RV1(K)
22              CONTINUE
23            CONTINUE
            ENDIF
            DO 24 K=L,N
              A(I,K)=SCALE*A(I,K)
24          CONTINUE
          ENDIF
        ENDIF
        ANORM=MAX(ANORM,(ABS(W(I))+ABS(RV1(I))))
25    CONTINUE
      DO 32 I=N,1,-1
        IF (I.LT.N) THEN
          IF (G.NE.0.0) THEN
            DO 26 J=L,N
              V(J,I)=(A(I,J)/A(I,L))/G
26          CONTINUE
            DO 29 J=L,N
              S=0.0
              DO 27 K=L,N
                S=S+A(I,K)*V(K,J)
27            CONTINUE
              DO 28 K=L,N
                V(K,J)=V(K,J)+S*V(K,I)
28            CONTINUE
29          CONTINUE
          ENDIF
          DO 31 J=L,N
            V(I,J)=0.0
            V(J,I)=0.0
31        CONTINUE
        ENDIF
        V(I,I)=1.0
        G=RV1(I)
        L=I
32    CONTINUE
      DO 39 I=N,1,-1
        L=I+1
        G=W(I)
        IF (I.LT.N) THEN
          DO 33 J=L,N
            A(I,J)=0.0
33        CONTINUE
        ENDIF
        IF (G.NE.0.0) THEN
          G=1.0/G
          IF (I.NE.N) THEN
            DO 36 J=L,N
              S=0.0
              DO 34 K=L,M
                S=S+A(K,I)*A(K,J)
34            CONTINUE
              F=(S/A(I,I))*G
              DO 35 K=I,M
                A(K,J)=A(K,J)+F*A(K,I)
35            CONTINUE
36          CONTINUE
          ENDIF
          DO 37 J=I,M
            A(J,I)=A(J,I)*G
37        CONTINUE
        ELSE
          DO 38 J= I,M
            A(J,I)=0.0
38        CONTINUE
        ENDIF
        A(I,I)=A(I,I)+1.0
39    CONTINUE
      DO 49 K=N,1,-1
        DO 48 ITS=1,30
          DO 41 L=K,1,-1
            NM=L-1
            IF ((ABS(RV1(L))+ANORM).EQ.ANORM)  GO TO 2
            IF ((ABS(W(NM))+ANORM).EQ.ANORM)  GO TO 1
41        CONTINUE
1         C=0.0
          S=1.0
          DO 43 I=L,K
            F=S*RV1(I)
            IF ((ABS(F)+ANORM).NE.ANORM) THEN
              G=W(I)
              H=SQRT(F*F+G*G)
              W(I)=H
              H=1.0/H
              C= (G*H)
              S=-(F*H)
              DO 42 J=1,M
                Y=A(J,NM)
                Z=A(J,I)
                A(J,NM)=(Y*C)+(Z*S)
                A(J,I)=-(Y*S)+(Z*C)
42            CONTINUE
            ENDIF
43        CONTINUE
2         Z=W(K)
          IF (L.EQ.K) THEN
            IF (Z.LT.0.0) THEN
              W(K)=-Z
              DO 44 J=1,N
                V(J,K)=-V(J,K)
44            CONTINUE
            ENDIF
            GO TO 3
          ENDIF
          IF (ITS.EQ.30) PAUSE 'No convergence in 30 iterations'
          X=W(L)
          NM=K-1
          Y=W(NM)
          G=RV1(NM)
          H=RV1(K)
          F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0*H*Y)
          G=SQRT(F*F+1.0)
          F=((X-Z)*(X+Z)+H*((Y/(F+SIGN(G,F)))-H))/X
          C=1.0
          S=1.0
          DO 47 J=L,NM
            I=J+1
            G=RV1(I)
            Y=W(I)
            H=S*G
            G=C*G
            Z=SQRT(F*F+H*H)
            RV1(J)=Z
            C=F/Z
            S=H/Z
            F= (X*C)+(G*S)
            G=-(X*S)+(G*C)
            H=Y*S
            Y=Y*C
            DO 45 NM=1,N
              X=V(NM,J)
              Z=V(NM,I)
              V(NM,J)= (X*C)+(Z*S)
              V(NM,I)=-(X*S)+(Z*C)
45          CONTINUE
            Z=SQRT(F*F+H*H)
            W(J)=Z
            IF (Z.NE.0.0) THEN
              Z=1.0/Z
              C=F*Z
              S=H*Z
            ENDIF
            F= (C*G)+(S*Y)
            X=-(S*G)+(C*Y)
            DO 46 NM=1,M
              Y=A(NM,J)
              Z=A(NM,I)
              A(NM,J)= (Y*C)+(Z*S)
              A(NM,I)=-(Y*S)+(Z*C)
46          CONTINUE
47        CONTINUE
          RV1(L)=0.0
          RV1(K)=F
          W(K)=X
48      CONTINUE
3       CONTINUE
49    CONTINUE
      RETURN
      END
C***********************************************************************

C***********************************************************************
C______________________________________________________________________
      subroutine svbksb(u,w,v,m,n,mp,np,b,x)
C______________________________________________________________________

C..It solves the system of linear equations  A*x=B, where A is 
c  specified by arrays U,W,V returned by SVDCMP.

C..NOTE: This routine presumes that small W's have already been
c ~~~~~~ zeroed.
C______________________________________________________________________

C..Input:                                      T
c  u,w,v   - arrays of SVD of matrix  A = U*W*V
c               Physical dimension:  u( mp, np),  w( np),  v( np, np)
c               Logical  dimension:  u(  m,  n),  w(  n),  v(  n,  n)
c  mp, np  - physical dimension of matrices  u, v and vector w
c  m,  n   - logical  dimension of matrix  u 
c  b       - right-hand side vector 
c               Physical dimension:  b( mp)
c               Logical  dimension:  b(  m)

C..Output:
c  x       - solution of eqs  A*x=B 
c               Physical dimension:  x( np)
c               Logical  dimension:  x(  n)

C______________________________________________________________________

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)       
      PARAMETER (NMAX=5000)
      DIMENSION U(MP,NP),W(NP),V(NP,NP),B(MP),X(NP),TMP(NMAX)

C______________________________________________________________________

      DO 12 J=1,N
        S=0.
        IF(W(J).NE.0.)THEN
          DO 11 I=1,M
            S=S+U(I,J)*B(I)
11        CONTINUE
          S=S/W(J)
        ENDIF
        TMP(J)=S
12    CONTINUE
      DO 14 J=1,N
        S=0.
        DO 13 JJ=1,N
          S=S+V(J,JJ)*TMP(JJ)
13      CONTINUE
        X(J)=S
14    CONTINUE
      RETURN
      END
C***********************************************************************

C**********************************************************************
c  Programs for minimizing semblance with Powell method (see the book 
c  "Numerical Recipes"):
c  POWELL
c  LINMIN
c  MNBRAK
c  BRENT
c  F1DIM
C**********************************************************************
C_______________________________________________________________________
      subroutine powell(p, xi, N, NP, ftol, iter, fret)
C______________________________________________________________________
C..This is my "adoptation" of Powell's Method (see the book 
c  "Numerical Recipes", p. 299) for minimization of the cost 
c  function "sembl_az"
C______________________________________________________________________

C..Input:
c  p       - input point (initial guess) in n-dimensional space 
c               Physical dimension:  p( N)   
c               Logical  dimension:  p( Nmax)
c  xi      - the identity matrix whose columns contain the initial
c            set of directions
c               Physical dimension:  xi( N,  N)   
c               Logical  dimension:  xi( NP, NP)
c  N       - logical dimension of vector p
c  NP      - physical dimension of vector p
c  ftol    - convergency tolerance on the function value

C..Output:
c  p       - found point providing  min(sembl_az(p)) 
c               Physical dimension:  p( N)   
c               Logical  dimension:  p( Nmax)
c  iter    - the number of iterations that were performed
c  fret    = sembl_az(p) - value of function sembl_az in minimum

C..NOTE: Objective function MUST be named SEMBL_AZ

C______________________________________________________________________

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

      parameter (Nmax=10, itmax=500)

      DIMENSION P(NP), XI(NP,NP), PT(NMAX), PTT(NMAX), XIT(NMAX)
C______________________________________________________________________

      FRET=sembl_az(P)
      DO 11 J=1,N
        PT(J)=P(J)
11    CONTINUE
      ITER=0
1     ITER=ITER+1
      FP=FRET

c     write (*,*) (p(i), i=1,n)
c     write (*,*) '  iter =', ITER, '   f =',FP

      IBIG=0
      DEL=0.
      DO 13 I=1,N
        DO 12 J=1,N
          XIT(J)=XI(J,I)
12      CONTINUE
c       write (*,*) (XIT(J), J=1,N)
        CALL LINMIN(P,XIT,N,FRET)
        IF(ABS(FP-FRET).GT.DEL)THEN
          DEL=ABS(FP-FRET)
          IBIG=I
        ENDIF
13    CONTINUE
      IF(2.*ABS(FP-FRET).LE.FTOL*(ABS(FP)+ABS(FRET)))RETURN
      IF(ITER.EQ.ITMAX) PAUSE 'Powell exceeding maximum iterations.'
      DO 14 J=1,N
        PTT(J)=2.*P(J)-PT(J)
        XIT(J)=P(J)-PT(J)
        PT(J)=P(J)
14    CONTINUE
      FPTT=sembl_az(PTT)
      IF(FPTT.GE.FP)GO TO 1
      T=2.*(FP-2.*FRET+FPTT)*(FP-FRET-DEL)**2-DEL*(FP-FPTT)**2
      IF(T.GE.0.)GO TO 1
      CALL LINMIN(P,XIT,N,FRET)
      DO 15 J=1,N
        XI(J,IBIG)=XIT(J)
15    CONTINUE
      GO TO 1
      END

C**********************************************************************

C**********************************************************************
C_______________________________________________________________________
      subroutine linmin (p, xi, N, fret)
C_______________________________________________________________________
C..Linear Minimization (see the book "Numerical Recipes", p. 300)
C_______________________________________________________________________

C..Input:
c  p       - given point in n-dimensional space that moves along 
c            given vector XI to minimize function TIME(p)
c               Physical dimension:  p( N)
c               Logical  dimension:  p( Nmax)
c  xi      - given direction along which the point P moves to
c            minimize function TIME(p)
c               Physical dimension:  p( N)
c               Logical  dimension:  p( Nmax)
c  N       - dimension of vectors  p  and  xi

C..Output:
c  p       - found point providing  min(sembl_az(p))  along vector  xi
c               Physical dimension:  p( N)
c               Logical  dimension:  p( Nmax)
c  fret    = sembl_az(p) - value of function sembl_az in minimum

C_______________________________________________________________________

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

      parameter (Nmax=10, TOL=1.E-4)

      EXTERNAL F1DIM

      DIMENSION P(N),XI(N)

      COMMON /F1COM/ PCOM(NMAX),XICOM(NMAX),NCOM
C_______________________________________________________________________

      NCOM=N
      DO 11 J=1,N
        PCOM(J)=P(J)
        XICOM(J)=XI(J)
11    CONTINUE
      AX=0.
      XX=1.
      BX=2.

      CALL MNBRAK(AX,XX,BX,FA,FX,FB,F1DIM)

      FRET=BRENT(AX,XX,BX,F1DIM,TOL,XMIN)
C.....To use derivative information the above statement should be 
c     replaced by the following (see, "Numerical Recipes", p. 306)
c     fret = dbrent(ax, xx, bx, f1dim, df1dim, tol, xmin)

      DO 12 J=1,N
        XI(J)=XMIN*XI(J)
        P(J)=P(J)+XI(J)
12    CONTINUE
      RETURN
      END

C**********************************************************************

C**********************************************************************
C_______________________________________________________________________
      subroutine mnbrak(ax, bx, cx, fa, fb, fc, F)
C_______________________________________________________________________
C..Golden Section Search (see the book "Numerical Recipes", p. 281)
C_______________________________________________________________________

      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)
      real*4 F   

      PARAMETER (GOLD=1.618034, GLIMIT=100., TINY=1.E-20)
C_______________________________________________________________________

      FA=F(AX)
c     write (*,*) ' ax = ', ax, ' sembl_az =', fa 
      FB=F(BX)
c     write (*,*) ' bx = ', bx, ' sembl_az =', fb 
      IF(FB.GT.FA)THEN
        DUM=AX
        AX=BX
        BX=DUM
        DUM=FB
        FB=FA
        FA=DUM
      ENDIF
      CX=BX+GOLD*(BX-AX)
      FC=F(CX)
1     continue
c     write (*,*) ' cx = ', cx, ' sembl_az =', fc 
      IF(FB.GE.FC)THEN
        R=(BX-AX)*(FB-FC)
        Q=(BX-CX)*(FB-FA)
        U=BX-((BX-CX)*Q-(BX-AX)*R)/(2.*SIGN(MAX(ABS(Q-R),TINY),Q-R))
        ULIM=BX+GLIMIT*(CX-BX)
        IF((BX-U)*(U-CX).GT.0.)THEN
          FU=F(U)
          IF(FU.LT.FC)THEN
            AX=BX
            FA=FB
            BX=U
            FB=FU
            GO TO 1
          ELSE IF(FU.GT.FB)THEN
            CX=U
            FC=FU
            GO TO 1
          ENDIF
          U=CX+GOLD*(CX-BX)
          FU=F(U)
        ELSE IF((CX-U)*(U-ULIM).GT.0.)THEN
          FU=F(U)
          IF(FU.LT.FC)THEN
            BX=CX
            CX=U
            U=CX+GOLD*(CX-BX)
            FB=FC
            FC=FU
            FU=F(U)
          ENDIF
        ELSE IF((U-ULIM)*(ULIM-CX).GE.0.)THEN
          U=ULIM
          FU=F(U)
        ELSE
          U=CX+GOLD*(CX-BX)
          FU=F(U)
        ENDIF
        AX=BX
        BX=CX
        CX=U
        FA=FB
        FB=FC
        FC=FU
        GO TO 1
      ENDIF
      RETURN
      END

C**********************************************************************

C**********************************************************************
C_______________________________________________________________________
      real*4 FUNCTION BRENT(AX,BX,CX,F,TOL,XMIN)
C_______________________________________________________________________
C..1D minimization without derivatives 
C_______________________________________________________________________
  
      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)
      real*4 F
 
      PARAMETER (ITMAX=100,CGOLD=.3819660,ZEPS=1.0E-10)
C_______________________________________________________________________
      A=MIN(AX,CX)
      B=MAX(AX,CX)
      V=BX
      W=V
      X=V
      E=0.
      FX=F(X)
      FV=FX
      FW=FX
      DO 11 ITER=1,ITMAX
        XM=0.5*(A+B)
        TOL1=TOL*ABS(X)+ZEPS
        TOL2=2.*TOL1
        IF(ABS(X-XM).LE.(TOL2-.5*(B-A))) GOTO 3
        IF(ABS(E).GT.TOL1) THEN
          R=(X-W)*(FX-FV)
          Q=(X-V)*(FX-FW)
          P=(X-V)*Q-(X-W)*R
          Q=2.*(Q-R)
          IF(Q.GT.0.) P=-P
          Q=ABS(Q)
          ETEMP=E
          E=D
          IF(ABS(P).GE.ABS(.5*Q*ETEMP).OR.P.LE.Q*(A-X).OR. 
     *        P.GE.Q*(B-X)) GOTO 1
          D=P/Q
          U=X+D
          IF(U-A.LT.TOL2 .OR. B-U.LT.TOL2) D=SIGN(TOL1,XM-X)
          GOTO 2
        ENDIF
1       IF(X.GE.XM) THEN
          E=A-X
        ELSE
          E=B-X
        ENDIF
        D=CGOLD*E
2       IF(ABS(D).GE.TOL1) THEN
          U=X+D
        ELSE
          U=X+SIGN(TOL1,D)
        ENDIF
        FU=F(U)
        IF(FU.LE.FX) THEN
          IF(U.GE.X) THEN
            A=X
          ELSE
            B=X
          ENDIF
          V=W
          FV=FW
          W=X
          FW=FX
          X=U
          FX=FU
        ELSE
          IF(U.LT.X) THEN
            A=U
          ELSE
            B=U
          ENDIF
          IF(FU.LE.FW .OR. W.EQ.X) THEN
            V=W
            FV=FW
            W=U
            FW=FU
          ELSE IF(FU.LE.FV .OR. V.EQ.X .OR. V.EQ.W) THEN
            V=U
            FV=FU
          ENDIF
        ENDIF
11    CONTINUE
      PAUSE 'Brent exceed maximum iterations.'
3     XMIN=X
      BRENT=FX
      RETURN
      END

C**********************************************************************
  
C**********************************************************************
C_______________________________________________________________________
      real*4 function f1dim(x)
C_______________________________________________________________________
C..Substitute of objective function (see the book "Numerical Recipes",
c  p. 301)
C_______________________________________________________________________
  
      implicit real*4 (a-h), integer*4 (i-n), real*4 (o-z)

      PARAMETER (NMAX=10)

      COMMON /F1COM/ PCOM(NMAX),XICOM(NMAX),NCOM

      DIMENSION XT(NMAX)
C_______________________________________________________________________

      DO 11 J=1,NCOM
        XT(J)=PCOM(J)+X*XICOM(J)
11    CONTINUE
      F1DIM=sembl_az(XT)
      RETURN
      END

C**********************************************************************
 
