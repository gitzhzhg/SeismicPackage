      subroutine getarray()
*
*
*   reads array parameters
*
*   OUTPUT VARIABLES:
*
*       block "array"   source-receiver parameters:
*
*                       xs,ys		source coordinates
*                       cazi, cdip	azimuth and dip of source
*                       cr		take-off radius 
*                       cdg		initial raytube aperture
*                       xr, yr		coordinates of first receiver
*                       cg1		azimuth of recording line
*                       ail, acl	azimuth of horizontal receivers
*                       dx		receiver spacing
*                       ng		number of receivers
*                       tsamp		number of time samples
*			dt		time sampling rate
*
*	block "wavelet"	Klauder wavelet parameters:
*
*                       nsweep		sweep length (samples)
*                       ntaper		taper length (samples)
*                       leng		length of wavelet (set to 255)
*                       fl, fh		lower and upper cutoff freqs
*			wave		wavelet array
*                       hilwav          hilbert transform of wavelet 
*
*   GLOBAL VARIABLES:
*
*	block "model"	elastic and geometric parameters
*       block "io"      input/output logical unit numbers
*

      real xs, ys, cazi, cdip, cr, cdg, xr, yr, cg1, ail, acl, dx, dt
      real wave(255), hilwav(255), fl, fh, tsweep, ttaper
      real geom(0:99), param(900)
      integer nlayer, triso(100), temp1, temp2, temp3
      integer ng, tsamp, nsweep, ntaper, leng, stdin, stdout, stderr
      common /model/ param, geom, nlayer, triso
      common /array/ xs, ys, cazi, cdip, cr, cdg, xr, yr, cg1,
     &   ail, acl, dx, ng, tsamp, dt
      common /wavelet/ nsweep, ntaper, leng, fl, fh, wave, hilwav
      common /io/ stdin, stdout, stderr, temp1, temp2, temp3

      character*80 parfile
      character ifile
      integer ioerror

*     ...Hard wired length of Klauder wavelet
      leng = 255

      write(stderr,"(/,' *** ATTENTION: NOW READING ARRAY PARAMETERS')")

      write(stderr,"(/,' do you want to read the array parameters ',$)")
      write(stderr,"(' from an existing file? (y/n) ',$)")
      read(stdin,'(A)') ifile

      if (ifile .EQ. 'y') then
           write(stderr,
     &     "(/,' enter the name of the input parameter file: ',$)")
           read(stdin,100) parfile
100        format(A80)
           open(unit=temp3,file=parfile,status='unknown',iostat=ioerror
     &          ,err=1000)
           rewind(temp3)
           read(temp3,*) xs,ys
           read(temp3,*) cazi,cdip
           read(temp3,*) xr,yr
           read(temp3,*) cg1
           read(temp3,*) ng,dx

	   if (dx .le. 0.0) stop ' getarray: dx <= 0.0'
	   if (ng .le. 0) stop ' getarray: ng <= 0'

           read(temp3,*) ail,acl
           read(temp3,*) dt,tsamp
           read(temp3,*) tsweep, ttaper

	   if (dt .le. 0.0) stop ' getarray: dt <= 0.0'

           read(temp3,*) fl, fh
           if(fl .LE. 0.e0 .or. fh .LE. fl) then
		stop ' getarray: illegal fl or fh'
           endif

        
           close(temp3)
	   goto 2000
      else      
           write(stderr,
     &    "(/,' enter the name of the output parameter file: ',$)")
           read(stdin,200) parfile
200        format(A80)
           open(unit=temp3,file=parfile,status='unknown',iostat=ioerror
     &      ,err=1000)
           rewind(temp3)
           write(stderr,"(/,' enter x and y source coordinates ',$)")
           write(stderr,"(' (m) (x is North, y is East): ',$)")
           read(stdin,*) xs,ys
           write(stderr,"(/,' enter azimuth and dip of point ',$)")
           write(stderr,"(' force (degrees): ',$)")
           read(stdin,*) cazi,cdip
           write(stderr,"(/,' enter x and y coordinates of first ',$)")
           write(stderr,"(' receiver (m): ',$)")
           read(stdin,*) xr,yr
           write(stderr,
     &    "(/,' enter azimuth of recording line (degrees): ',$)")
           read(stdin,*) cg1
           write(stderr,"(/,' enter number of receivers ',$)")
           write(stderr,"(' and receiver spacing (m): ',$)")

440        read(stdin,*) ng, dx
	   if (dx .le. 0.0 .or. ng .le. 0) then
                write(stderr,"(/,' *** invalid,
     &          please reenter: ')")

                goto 440

           endif

        write(stderr,"(/,' enter the azimuth of the two ',$)") 
        write(stderr,"(' horizontal receivers (degrees): ',$)")
        read(stdin,*) ail,acl
        write(stderr,"(/,' enter time sampling (sec) and ',$)")
        write(stderr,"(' number of sample/trace: ',$)")

441        read(stdin,*) dt,tsamp
	   if (dt .le. 0.0) then
                write(stderr,"(/,' *** invalid dt <= 0.0,
     &          please reenter: ')")

                goto 441

           endif


           write(stderr,"(/,' *** WAVELET CHARACTERISTICS:')")

           write(stderr,
     &    "(/,' enter sweep length and taper length (sec) : ',$)")
           read(stdin,*) tsweep, ttaper

           write(stderr,"(/,' enter lower (>0) and upper cutoff ',$)")
           write(stderr,"(' frequencies (Hz): ',$)")

444        read(stdin,*) fl, fh
           if(fl .LE. 0.e0) then
                write(stderr,"(/,' *** invalid lower cutoff frequency,
     &          please reenter: ')")

           goto 444
	
           endif

           write(temp3,*) xs,ys,
     &     '      North and East source coordinates (m)'
           write(temp3,*) cazi,cdip,
     &     '     azimuth and dip of point force (degrees)'
           write(temp3,*) xr,yr,
     &     '      North and East coordinates of first receiver (m)'
           write(temp3,*) cg1,
     &     '               azimuth of recording line (degrees)'
           write(temp3,*) ng,dx,
     &     '     number of receivers and receiver spacing (m)'
           write(temp3,*) ail,acl,
     &     '     azimuth of the two horizontal receivers (degrees)'
           write(temp3,*) dt,tsamp,
     &     '     sampling rate (sec) and number of samples per trace'
           write(temp3,*) tsweep, ttaper,
     &     '           sweep and taper lengths (sec)'
*          write(temp3,*) tleng,'               wavelet lengths (sec)'
           write(temp3,*) fl, fh,
     &     '     lower and upper cutoff frequencies (Hz)'

           close(temp3)
           goto 2000

      endif



1000   write(stderr,*) 'ERROR opening file ',parfile,' iostat=',ioerror,
     &   'routine getarray'
      
      return

2000  nsweep = tsweep/dt
      ntaper = ttaper/dt

      if (nsweep .gt. 4096) stop ' getarray: illegal nsweep > 4096'
      if (ntaper .gt. 500) stop ' getarray: illegal ntaper > 500'

      cr = geom(1)/100.
      cdg = .01

      call klaud86(wave,leng,dt,nsweep,fl,fh,ntaper)

      call fasthilbert(wave,hilwav,leng)

      return
      end



      subroutine getmodel()

*
*   reads geometric and elastic model parameters
*
*   OUTPUT VARIABLES:
*
*       block "model"   elastic and geometric model parameters:
*
*                       param	9 elastic parameters for all layers:
*                               A, C, N, L, F, rho, a1, a2, a3
*                               where A,C,N,L,F are the elastic 
*                               rigidities, rho density, and 
*                               (a1,a2,a3) a unit vector along the 
*                               anisotropy axis.
*			geom	depth of the reflectors in the model
*                       nlayer	number of layers in the model
*                       triso   characterizes whether a layer is 
*                               isotropic or anisotropic
* 
*
*   GLOBAL VARIABLES:
*
*       block "io"      input/output logical unit numbers
*

      integer nlayer, ii, i, ioerror, triso(100), niso
      real deg2rad, lambda, mu, azi, dip, rho, thick
      real alpha, beta, gamma, delta, epsilon, Vp, Vs
      integer stdin, stdout, stderr, temp1, temp2, temp3
      character*80 parfile
      character*1 iso, ifile, choice
      real param(900), geom(0:99)
      common /model/ param, geom, nlayer, triso
      common /io/ stdin, stdout, stderr, temp1, temp2, temp3

      deg2rad = 1.7453293e-02

      write(stderr,"(/,' *** ATTENTION: NOW READING MODEL PARAMETERS')")

      write(stderr,"(/,' do you want to read the model from an ',$)")
      write(stderr,"(' existing file? (y/n) ',$)")
      read(stdin,'(A)') ifile
      
      if(ifile .EQ. 'y') then

*   opens parameter file if already exists

          write(stderr,"(/,' enter name of input parameter file: ',$)")
          read(stdin,100) parfile
100       format(A80)
          open(unit=temp1,file=parfile,status='unknown',
     &       iostat=ioerror,err=1000)
        rewind(temp1)
          read(temp1,'(1X,A)') choice
          read(temp1,*) nlayer

*   zeroth interface is always the surface: z=0.

          geom(0) = 0.
      
*   read model parameters from input file

          ii = 0

          do10 i=1,nlayer
                read(temp1,*) triso(i)
        if(triso(i) .EQ. 0) then

        if(choice .EQ. 'L' .OR. choice .EQ. 'l') then
                read(temp1,*) lambda
                read(temp1,*) mu
                read(temp1,*) rho
                rho = rho*1.e3
            param(ii+6) = rho
                    param(ii+1) = lambda+2.*mu
                    param(ii+2) = param(ii+1)
                    param(ii+3) = mu
                    param(ii+4) = mu
                    param(ii+5) = lambda
                azi = 0.e0
                dip = 0.e0
        else if(choice .EQ. 'T' .OR. choice .EQ. 't') then
                read(temp1,*) Vp
                read(temp1,*) Vs
                read(temp1,*) rho
                rho = rho*1.e3
                    param(ii+1) = rho*Vp*Vp
                    param(ii+2) = param(ii+1)
                    param(ii+3) = rho*Vs*Vs
                    param(ii+4) = param(ii+3)
                    param(ii+5) = param(ii+1)-2.*param(ii+3)
                    param(ii+6) = rho
        else
        write(stderr,
     & "(/,' *** invalid code; recommend job interruption',$)")
        endif

        else if(triso(i) .EQ. 1) then

        if(choice .EQ. 'L' .OR. choice .EQ. 'l') then
        
                read(temp1,*) param(ii+1)
                read(temp1,*) param(ii+2)
                read(temp1,*) param(ii+3)
                read(temp1,*) param(ii+4)
                read(temp1,*) param(ii+5)
        read(temp1,*) rho
        rho = rho*1.e3
        param(ii+6) = rho
        
        else if(choice .EQ. 'T' .OR. choice .EQ. 't') then

        read(temp1,*) alpha
        read(temp1,*) beta
        read(temp1,*) gamma
        read(temp1,*) delta
        read(temp1,*) epsilon
        read(temp1,*) rho
            rho = rho*1.e3
                    param(ii+2) = rho*alpha*alpha
                    param(ii+4) = rho*beta*beta
                    param(ii+1) = param(ii+2)*(1.e0+2.e0*epsilon)
                    param(ii+3) = param(ii+4)*(1.e0+2.e0*gamma)
                    param(ii+5) = param(ii+2)-param(ii+4)
                    param(ii+5) = param(ii+5)*(param(ii+5)+
     &                      2.e0*delta*param(ii+2))
                    param(ii+5) = sqrt(param(ii+5)) - param(ii+4)
                    param(ii+6) = rho

        else
        write(stderr,
     & "(/,' *** invalid code; recommend job interruption',$)")
        endif


        read(temp1,*) azi,dip

        else
        write(stderr,
     & "(/,' *** invalid code; recommend job interruption.',$)")
        endif

              azi = azi*deg2rad
              dip = dip*deg2rad
              param(ii+7) = cos(azi)*cos(dip)
              param(ii+8) = sin(azi)*cos(dip)
              param(ii+9) = sin(dip)

        if(i .NE. nlayer) then
                read(temp1,*) thick
        geom(i) = geom(i-1)+thick
        if(thick .LE. 0.e0) then
        write(stderr,"(/,' *** invalid geometry; recommend job
     & interruption.')")
        endif
        endif

                ii = ii+9
10          continue       
          
          close(temp1)
          return
      else
          write(stderr,"(/,' enter name of output parameter file: ',$)")
          read(stdin,200) parfile
200       format(A80)

          open(unit=temp1,file=parfile,status='unknown',
     &       iostat=ioerror,err=1000)
          rewind(temp1)

40       write(stderr,
     & "(/,' enter L to enter parameters using Love notations',/,
     & 3x,'or T if you prefer Thomsen notations: ',$)")
          read(stdin,'(A)') choice

          if(choice .EQ. 'L' .OR. choice .EQ. 'l') then
                write(temp1,'(1X,2A)') choice,
     &'       This file uses Love notations'
          else if(choice .EQ. 'T' .OR. choice .EQ. 't') then
                write(temp1,'(1X,2A)') choice,
     &'       This file uses Thomsen notations'
          else
        write(stderr,"(/,' *** invalid choice, please try again')")
        goto 40
          endif

          write(stderr,"(/,' enter number of layers: ',$)")
          read(stdin,*) nlayer

          write(temp1,*) nlayer, '     number of layers'

          geom(0) = 0.
          ii = 0

          do20 i=1,nlayer
                write(stderr,"(/,' is layer #',i3,
     &' isotropic? (y/n) ',$)") i
                read(stdin,'(A)') iso
                if(i .EQ. 1) then
                    iso = 'y'
                endif
                if(iso .EQ. 'y') then
                    niso = 0
                    write(temp1,*) niso,'     layer#',i,' is isotropic'
                    if(choice .EQ. 'L' .OR. choice .EQ. 'l') then
                    write(stderr,"(/,' enter lambda, mu (Pa) and 
     &rho (g/cm3): ',$)")
                    read(stdin,*) lambda,mu,rho

*   isotropic layer is a special case of transversely isotropic layer

                    write(temp1,*) lambda,
     &                             '     Lame constant lambda (Pa)'
                    write(temp1,*) mu,'     shear rigidity mu (Pa)'
                    write(temp1,*) rho,'     density (g/cm3)'
            rho = rho*1.e3
                    param(ii+1) = lambda+2.*mu
                    param(ii+2) = param(ii+1)
                    param(ii+3) = mu
                    param(ii+4) = mu
                    param(ii+5) = lambda
                    param(ii+6) = rho

                    else
                write(stderr,
     & "(/,' enter Vp (m/s), Vs (m/s), and rho (g/cm3): ',$)")
                    read(stdin,*) Vp, Vs, rho

                    write(temp1,*) Vp,
     &                        '     compressional velocity (m/s)'
                    write(temp1,*) Vs,'     shear velocity (m/s)'
                    write(temp1,*) rho,'     density (g/cm3)'
            rho = rho*1.e3
                    param(ii+1) = rho*Vp*Vp
                    param(ii+2) = param(ii+1)
                    param(ii+3) = rho*Vs*Vs
                    param(ii+4) = param(ii+3)
                    param(ii+5) = param(ii+1)-2.*param(ii+3)
                    param(ii+6) = rho

                    endif
*   axis of anisotropy is taken horizontal so that rays slowness 
*   and axis are in practice never aligned (Cf the way displacement
*   polarizations are computed in polar.f)

                    param(ii+7) = 1.
                    param(ii+8) = 0.
                    param(ii+9) = 0.
                else
                    niso = 1
                    write(temp1,*) niso,'      layer#',i,
     &                             ' is anisotropic'
                    if(choice .EQ. 'L' .OR. choice .EQ. 'l') then
                    write(stderr,"(/,' enter 5 elastic constants 
     &A,C,N,L,F (Pa), and rho (g/cm3): ',$)")
                    read(stdin,*) param(ii+1),param(ii+2),param(ii+3),
     &              param(ii+4),param(ii+5),rho
            param(ii+6) = rho*1.e3
              write(temp1,*) param(ii+1),'     A (C33) (Pa)'
              write(temp1,*) param(ii+2),'     C (C11) (Pa)'
              write(temp1,*) param(ii+3),'     N (C66) (Pa)'
              write(temp1,*) param(ii+4),'     L (C44) (Pa)'
              write(temp1,*) param(ii+5),'     F (C13) (Pa)'
        write(temp1,*) rho,'     density (g/cm3)'
                    else
              write(stderr,
     & "(/,' enter Thomsen parameters Vp (m/s), Vs (m/s),'
     &,/,/,5x,'gamma, delta, epsilon, rho (g/cm3): ',$)")
                    read(stdin,*) alpha,beta,gamma,delta,epsilon,rho
        write(temp1,*) alpha,'     Vp (slow P velocity) (m/s)'
        write(temp1,*) beta,'     Vs (slow S velocity) (m/s)'
        write(temp1,*) gamma,'     gamma (S velocity anisotropy)'
        write(temp1,*) delta,'     delta (anellipticity parameter)'
        write(temp1,*) epsilon,'     epsilon (P velocity anisotropy)'
        write(temp1,*) rho,'     density (g/cm3)'
            rho = rho*1.e3
                    param(ii+2) = rho*alpha*alpha
                    param(ii+4) = rho*beta*beta
                    param(ii+1) = param(ii+2)*(1.e0+2.e0*epsilon)
                    param(ii+3) = param(ii+4)*(1.e0+2.e0*gamma)
                    param(ii+5) = param(ii+2)-param(ii+4)
                    param(ii+5) = param(ii+5)*(param(ii+5)+
     &                      2.e0*delta*param(ii+2))
                    param(ii+5) = sqrt(param(ii+5)) - param(ii+4)
                    param(ii+6) = rho

                    endif
                    write(stderr,"(/,' enter azimuth and dip in degrees 
     &of anisotropy axis: ',$)")
                    read(stdin,*) azi,dip
            write(temp1,*) azi,dip,
     &'    azimuth and dip of anisotropy axis (degrees)'
                    azi = azi*deg2rad
                    dip = dip*deg2rad
                    param(ii+7) = cos(azi)*cos(dip)
                    param(ii+8) = sin(azi)*cos(dip)
                    param(ii+9) = sin(dip)
                endif

                if(i .NE. nlayer) then
50          write(stderr,"(/,' enter thickness of layer (m): ',$)")
                    read(stdin,*) thick
            geom(i) = geom(i-1)+thick
        if(thick .LE. 0.e0) then
        write(stderr,"(/,' *** invalid geometry, please try again')")
        goto 50
        endif
                    write(temp1,*) thick, '     thickness of layer#',i
                endif

        triso(i) = niso

                ii = ii+9

20          continue
          
          close(temp1)
          return
      endif

1000   write(stderr,*) 'ERROR in opening ',parfile,' iostat=',ioerror,
     & '. routine: getmodel'
      return
      end



      subroutine getoutput()

*
*   gets output parameters from standard input
*
*   OUTPUT VARIABLES:
*
*       block "output"  graphics/verbose parameters:
*
*                       verbose		selects verbose option
*                       trace		selects graphics option
*                       scale		scale for graphics
*                       xmin, ymin      lower left corner of plot
*
*       block "io"      input/output logical unit numbers:
*
*                       stdin		standard input logical unit #
*                       stdout		standard output logical unit #
*                       stderr		standard error logical unit #
*                       temp1		temporary logical unit number
*                       temp2		temporary logical unit number
*                       temp3		temporary logical unit number
*

      integer verbose, trace
      integer stdin, stdout, stderr, temp1, temp2, temp3
      real scale, xmin, ymin

      common /output/ verbose, trace, scale, xmin, ymin
      common /io/ stdin, stdout, stderr, temp1, temp2, temp3

      character itrace, iverb

      write(stderr,"(/,' *** TRISO: Ray Tracing over ',$)") 
      write(stderr,"(' Tabular Transversely Isotropic Media. ***',/)")
      write(stderr,"(' do you want a ray tracing display? (y/n) ',$)")
      read(stdin,'(A)') itrace

      if(itrace .EQ. 'y') then
        trace = 1
        write(stderr,"(/,' enter plot size in inches: ',$)")
        read(stdin,*) scale
        write(stderr,"(/,' do you want some verbose during run time? 
     &(y/n) ',$)")
        read(stdin,'(A)') iverb
        if(iverb .EQ. 'y') then
                verbose = 1
        else
                verbose = 0
        endif
      else
        write(stderr,"(/,' do you want some verbose during run time? 
     &(y/n) ',$)")
        read(stdin,'(A)') iverb
        if(iverb .EQ. 'y') then
                verbose = 1
        else
                verbose = 0
        endif
      endif

      return
      end



      subroutine getpath()

*
*   reads up to 200 raypaths
*
*   OUTPUT VARIABLES:
*
*       block "path"    raypath parameters:
*
*                       mode		sequence of modes along 
*                                       raypaths.
*                       interface	sequence of interfaces 
*                                       along raypaths.
*                       nsegment	number of segments in 
*                                       raypaths.
*                       npath		number of raypaths.
*
*   GLOBAL VARIABLES:
*
*       block "io"      input/output logical unit numbers
*

      integer nsegment(200), i, ioerror, j, nlayer, triso(100)
      real param(900), geom(0:99)
      integer stdin, stdout, stderr, temp1, temp2, temp3
      integer mode(200,100),interface(200,0:100), npath, ilayer
      common /path/ mode, interface, nsegment, npath
      common /model/ param, geom, nlayer, triso
      common /io/ stdin, stdout, stderr, temp1, temp2, temp3

      character*80 parfile
      character ifile
      character*2 char2(100), char1

      write(stderr,
     & "(/,' *** ATTENTION: NOW READING RAYPATH PARAMETERS')")

      write(stderr,"(/,' do you want to read the path from an ',$)")
      write(stderr,"(' existing file? (y/n) ',$)")
      read(stdin,'(A)') ifile

      if(ifile .EQ. 'y') then
        write(stderr,"(/,' enter name of input parameter file: ',$)")
        read(stdin,100) parfile
100     format(A80)
        open(unit=temp2,file=parfile,status='unknown',iostat=ioerror,
     &          err=1000)
          rewind(temp2)

        read(temp2,*) npath
        if(npath .GT. 200) then
        write(stderr,"(/,' *** too many paths: number set to 200.')")
        npath = 200
        endif

*   all rays must start from the surface

        do 150 j=1,npath

        interface(j,0) = 0

*   read the path from parfile

        read(temp2,*) nsegment(j)
        if(nsegment(j) .GT. 100) then
        write(stderr,
     & "(/,' *** #of segments in path#',i3,' too large,')") j
        write(stderr,"(/,'    recommend job interruption.')")
        nsegment(j) = 100
        endif
        
        do10 i=1,nsegment(j)
                read(temp2,'(1X,I3,3X,A)') interface(j,i), char1
                if( (abs(interface(j,i)-interface(j,i-1)).NE.
     &   1) .OR. (interface(j,i).EQ.0 .AND. i.NE.nsegment(j)) .OR. 
     &   (interface(j,i).GE.nlayer) ) then
                        write(stderr,"(/,' *** invalid ray path#',i3,
     &   'segment#',i3,': recommend job interruption...')") j,i
                endif

                ilayer = max0(interface(j,i),interface(j,i-1))
                if(triso(ilayer) .EQ. 0) then
                if(char1 .EQ. 'S' .OR. char1 .EQ. 's') then
                        mode(j,i) = 1
                else if(char1 .EQ. 'P' .OR. char1 .EQ. 'p') then
                        mode(j,i) = 2
                else
                        mode(j,i) = 2
                        write(stderr,"(/,' *** invalid mode: path#',
     &   i3,'segment#',i3,'; recommend job interruption...')") j,i
                endif
                else
                if(char1 .EQ. 'SP' .OR. char1 .EQ. 'sp') then
                        mode(j,i) = 1
                else if(char1 .EQ. 'QP' .OR. char1 .EQ. 'qp') then
                        mode(j,i) = 2
                else if(char1 .EQ. 'QS' .OR. char1 .EQ. 'qs') then
                        mode(j,i) = 3
                else
                        mode(j,i) = 2
                        write(stderr,"(/,' *** invalid mode: path#',
     &   i3,'segment#',i3,'; recommend job interruption...')") j,i
                endif

                endif
10         continue

        if(interface(j,nsegment(j)) .NE. 0) then
                write(stderr,"(/,' *** invalid raypath#',i3,
     &   ': recommend job interruption')") j
        endif
150        continue

        close(temp2)
        return
      else
        write(stderr,"(/,' enter name of output parameter file: ',$)")
        read(stdin,200) parfile
200     format(A80)
        open(unit=temp2,file=parfile,status='unknown',iostat=ioerror,
     &          err=1000)
        rewind(temp2)
222     write(stderr,
     & "(/,' enter number of events to be traced (max 200): ',
     & $)")
        read(stdin,*) npath
        if(npath .GT. 200) then
        write(stderr,"(/,' *** too many paths, please reenter:')")
        goto 222
        endif
        write(temp2,*) npath, '     # of paths'

        do250 j=1,npath

        write(stderr,"(/,' NOW READING PATH #',i3,'\n')") j
333     write(stderr,"(/,' enter number of ray segments in the ',$)")
        write(stderr,"(' path (max 100): ',$)")
        read(stdin,*) nsegment(j)
        if(nsegment(j) .GT. 100) then
        write(stderr,"(/,' *** too many segments, please reenter:')")
        goto 333
        endif 
        write(temp2,*) nsegment(j), '     # of segments in path#',j
        
*   all rays must start from the surface

        interface(j,0) = 0

*   read the path from standard input and save it in parfile

50      do20 i=1,nsegment(j)    
60           write(stderr,"(/,' enter interface index for ',$)")
             write(stderr,"(' end of segment #',i3,': ',$)") i
                read(stdin,*) interface(j,i)
                if( (abs(interface(j,i)-interface(j,i-1)).NE.
     &   1) .OR. (interface(j,i).EQ.0 .AND. i.NE.nsegment(j)) .OR. 
     &   (interface(j,i).GE.nlayer) ) then
                        write(stderr,"(/,' *** invalid ray path, please
     & reenter:')")
                        goto 60
                endif
                ilayer = max0(interface(j,i),interface(j,i-1))
70              if(triso(ilayer) .EQ. 0) then
           write(stderr,"(/,' enter propagation mode of segment #',i3 
     &,' (P or S): ',$)") i
                read(stdin,'(A)') char2(i)
                if(char2(i) .EQ. 'S' .OR. char2(i) .EQ. 's') then
                        mode(j,i) = 1
                else if(char2(i) .EQ. 'P' .OR. char2(i) .EQ. 'p') then
                        mode(j,i) = 2
                else
                        write(stderr,"(/,' *** invalid mode, please
     & reenter:')")
                        goto 70
                endif
                else
           write(stderr,"(/,' enter propagation mode of segment #',i3 
     &,' (SP, QP or QS): ',$)") i
                read(stdin,'(A)') char2(i)
                if(char2(i) .EQ. 'SP' .OR. char2(i) .EQ. 'sp') then
                        mode(j,i) = 1
                else if(char2(i) .EQ. 'QP' .OR. char2(i) .EQ. 'qp') then
                        mode(j,i) = 2
                else if(char2(i) .EQ. 'QS' .OR. char2(i) .EQ. 'qs') then
                        mode(j,i) = 3
                else
                        write(stderr,"(/,' *** invalid mode, please
     & reenter:')")
                        goto 70
                endif

                endif
20         continue
        
        if(interface(j,nsegment(j)) .NE. 0) then
                write(stderr,"(/,' *** invalid raypath (must end at 
     & surface), please reenter:')")
                goto 50
        endif

        do30 i=1,nsegment(j)

                write(temp2,"(1X,I3,3X,2A,I4)") interface(j,i),char2(i),
     &   '      destination and mode of segment#',i 

30         continue

250        continue

        close(temp2)

        return
      endif

1000   write(stderr,*) 'ERROR in opening ',parfile,' iostat=',ioerror,
     &   ' routine getpath'
      return
      end


