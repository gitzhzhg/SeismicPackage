ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c program vzestf.f   
c Zhaobo Meng / Oct 1998
c CWP/CSM
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c program vzest
c
c This program is design for postmigration velocity analysis.  It is
c bsed on Zhaobo Meng's thesis, chapter 4.  
c
c "Tetrahedral based earth models, ray tracing in tetrahedral models
c and analytical migration velocity analysis", Ph. D. thesis,
c Center for Wave Phenomena, Department of Mathematical and Computer
c Sciences, Colorado School of Mines. Jan. 1999. 
c
c vzest converts the traveltime misfits (offset??.picks)
c to depth misfits, by multiplying the velocity at the base horizon.  
c
c For vzest, not all CIGs are used (because there may not be enough data.
c Only those CIGs from nxrtbeg to nxrtend and from nyrtbeg to nyrtend are
c used for velocity and depth update.  Similarly, not all offsets are used, 
c user can specify how many offsets to use and what offsets to use.  They 
c are specified by isort(1), isort(2),..., isort(npicks).
c
c To run the program, the best way is to execuate the deck file in the 
c project directory.  Hopefully the vzest will take you to the correct 
c depth at the correct velocity. So now, BUCKLE UP and enjoy the ride!

      subroutine vzest(
     +  neqns,                   !integer neqns=5*nmva
     +  nentries,                !integer nentries=nmva*17
     +  npicks,	                 !integer npicks=2
     +  nmva,	                 !integer nmva=nxht*nyht 
     +  niterations,             !integer niterations=40
     +  vlambda,                 !real vlambda=3.0
     +  v0lambda,                !real v0lambda=1.0
     +  v1lambda,                !real v1lambda
     +  vzlambda,                !real vzlambda
     +  zlambda,                 !real zlambda
     +  w_eps,                   !real w_eps /0.1/
     +  azimuth,                 !real azimuth /0.0/
     +  xupmaxconst,             !real xupmaxconst=100
     +  xdnmaxconst,             !real xdnmaxconst=-100
     +  topcmean,                !real topcmean
     +  basecmean,               !real basecmean
     +  nxht,                    !number of samples in x for AMVA
     +  nyht,                    !number of samples in y for AMVA
     +  nxrt,                    !number of samples in x
     +  nyrt,                    !number of samples in y
     +  nxrtbeg,nxrtend,         !CIGs to be solved
     +  nyrtbeg,nyrtend,         !CIGs to be solved
     +  dxrt,                    !CIG spacing
     +  dyrt,                    !CIG spacing
     +  ixdisp,                  !ix for display
     +  iydisp,                  !iy for display
     +  iseed,                   !seed number
     +  nsort,                   !number of offsets 
     +  sort1,
     +  sort2,
     +  isort,                   !isort(2)
     +  fnames,                  !integer*120 fnames(2) misfit file names
     +  topcmigfile,             !top c0
     +  topcgmigfile,            !top cg
     +  topzgridfile,            !top z
     +  basecmigfile,            !base c
     +  basezgridfile,           !base z
     +  dvxfile,                 !dvxfile to output
     +  dvyfile,                 !dvyfile to output
     +  dvzfile,                 !dvzfile to output
     +  c0newfile,               !update c
     +  g0file,                  !output g0
     +  g1file,                  !output g1
     +  g2file,                  !output g2
     +  g3file,                  !output g3
     +  topc,       !real topc=0 then read in from file
     +  topcmig,    !real (nxrt,nyrt) velocity grid at the top for small v
     +  topcg,      !real topcg=0 then read in from file
     +  topcgmig,   !real (nxrt,nyrt) velocity gradient at the top 
     +  topcnew,    !real (nxrt,nyrt) velocity grid at the top for small v
     +  topzgrid,   !real (nxrt,nyrt) depth grid at the top
     +  basecmig,   !real (nxrt,nyrt) velocity grid at the base for small v
     +  basezgrid,  !real (nxrt,nyrt) depth grid at the base	
     +  g,	    !real (npicks,nxrt,nyrt,4) derivatives with repsect to
     +  dvz,        !real (nxrt,nyrt) derivatives with repsect to
     +  dvzold,     !real (nxrt,nyrt) derivatives with repsect to
     +  dvx,	    !real (nxrt,nyrt) derivatives with repsect to
     +  dvy,	    !real (nxrt,nyrt) derivatives with repsect to
     +  dvxwork,    !real (nxrt,nyrt) derivatives with repsect to 
     +  work,       !real (nxht,nyht) 
     +  icol,       !integer (nentries) column positions of nonzeros a entries
     +  irow,       !integer (nentries) row positions of nonzeros a entries
     +  a,          !real (nentries) nonzero a entries
     +  b,          !real (neqns) the right hand side (ixht+(iyht-1)*nxht)
     +	x,          !real (nmva) the unknown dvz (ixht+(iyht-1)*nxht)
     +  xupmax,     !real (nmva) maximum up-perturbation allowed
     +  xdnmax,     !real (nmva) maximum dn-perturbation allowed
     +  xbak,       !real (nmva) a copy of x
     +  y,          !real (neqns) working global parms
     +  q,          !real (neqns)
     +  s,          !real (neqns)
     +  p,          !real p(nmva)
     +  r,          !real r(nmva)
     +  tmisfit,    !real tmisfit(npicks,nxrt,nyrt)
     +  zmisfit)    !real zmisfit(npicks,nxrt,nyrt)

        implicit none

c NPICKS is the parameter used by VEST3D.  It is the number of offsets that
c the user has picked up for migration velocity analysis.  That is also the
c number of misfit grid files one has saved after running HPSDM.
        integer npicks

c nxrt is the number of reconstructed imaged points in x direction
c nyrt is the number of reconstructed imaged points in y direction
        integer nxrt,nyrt

        integer nxht,nyht
        integer nmva
        real vlambda
        real v0lambda
        real v1lambda
        real vzlambda
        real zlambda
        integer niterations
        real topcmean
        real basecmean
        real xupmaxconst
        real xdnmaxconst
        real dxrt,dyrt
        integer ixdisp,iydisp
        integer iseed
        integer isort(2)
        integer nsort
        real sort1
        real sort2
        integer nxrtbeg
        integer nxrtend
        integer nyrtbeg
        integer nyrtend
        real w_eps
        real azimuth

        character*120 fnames(2)
        character*120 topcmigfile
        real topc
        character*120 topcgmigfile
        real topcg
        character*120 topzgridfile
        character*120 basecmigfile
        character*120 basezgridfile
        character*120 dvxfile
        character*120 dvyfile
        character*120 dvzfile
        character*120 c0newfile
        character*120 g0file
        character*120 g1file
        character*120 g2file
        character*120 g3file

        real basecmig(nxrt,nyrt)     !velocity grid at the base for small v
        real topcmig(nxrt,nyrt)      !velocity grid at the top for small v
        real topcnew(nxrt,nyrt)      !velocity grid at the top for small v
        real topcgmig(nxrt,nyrt)     !velocity gradient at the top
        real topzgrid(nxrt,nyrt)     !depth grid at the top
        real basezgrid(nxrt,nyrt)    !depth grid at the base
	real g(npicks,nxrt,nyrt,4)   !derivatives with repsect to
	real dvz(nxrt,nyrt)          !derivatives with repsect to
	real dvzold(nxrt,nyrt)       !derivatives with repsect to
	real dvx(nxrt,nyrt)          !derivatives with repsect to
	real dvy(nxrt,nyrt)          !derivatives with repsect to
	real dvxwork(nxrt,nyrt)      !derivatives with repsect to
	real work(nxht,nyht)

c traveltime misfits for all picks at all CIGs
        real tmisfit(npicks,nxrt,nyrt)

c the measured depth misfit converted using traveltime misfit tmisfit.
c This grid will be used to set up a set of equations for mva

	!These are used by cgr

        real zmisfit(npicks,nxrt,nyrt)
        integer neqns                !number of eqns
        integer nentries             !maximum number of nonzero entries in a
        integer ientries             !actual number of nonzero entries in a
        integer icol(nentries)       !column positions of nonzeros a entries
        integer irow(nentries)       !row positions of nonzeros a entries
        real a(nentries)             !nonzero a entries
	real b(neqns)                !the right hand side (ixht+(iyht-1)*nxht)
	real x(nmva)                 !the unknown dvz (ixht+(iyht-1)*nxht)

        real xupmax(nmva)            !maximum up-perturbation allowed
        real xdnmax(nmva)            !maximum dn-perturbation allowed
        real xbak(nmva)              !a copy of x

        real y(neqns)               !working global parms
        real q(neqns)
        real s(neqns)
        real p(nmva)
        real r(nmva)

        integer npicksmax
        parameter (npicksmax=100)
	real dg(npicksmax,4)            !derivatives with repsect to
	real dvz00(npicksmax)
        real dzmisfit(npicksmax)
        real seed(npicksmax)
	real h(npicksmax),sortinc

        integer imva,ixrt,iyrt,ipick,ix,iy
	real cgrerror,cgr_vzest
        real ww(3)
	real work_read(nyrt*nxrt)

        integer iunit,ixht,iyht

	if (npicks.gt.npicksmax) then
          write(*,*)'npicksmax too small'
	  stop
	end if

	iunit=99
	write(*,*)'Now, go!'

	write(*,*)'nmva=',nmva
	write(*,*)'c0newfile=',c0newfile

c initialize dvz
	do ixrt=1,nxrt
	  do iyrt=1,nyrt
	    dvz(ixrt,iyrt)=0.0
	    dvzold(ixrt,iyrt)=0.0
	  end do
	end do

	do imva=1,nmva
          xdnmax(imva)=xdnmaxconst
          xupmax(imva)=xupmaxconst
	end do

c The seeds are defined as 
	print *,'These are the seeds...'
        do ipick=1,npicks
	  write(*,*)'isort(',ipick,')=',isort(ipick)
	  if (isort(ipick).eq.isort(1).and.ipick.ne.1) then
	    write(*,*)'error, isort is not data-ed in dvz.deck'
	    stop
          end if
          seed(ipick)=(isort(ipick)+0.5)*iseed
          print *,'seed(',ipick,')=',seed(ipick)
        enddo

	!The first call is for the slow model
	call readgrids_vzest(
     +    topc,
     +    topcg,
     +    npicks,          !number of picks
     +    nxrt,nyrt,       !project size
     +    basecmig,       !velocity grid at the base
     +    topcmig,        !velocity grid at the top
     +    topcgmig,       !velocity gradient grid at the top
     +    topzgrid,       !depth grid at the top
     +    basezgrid,      !depth grid at the base
     +    work_read,
     +    tmisfit,
     +    topcmigfile,
     +    topcgmigfile,
     +    basecmigfile,
     +    topzgridfile,
     +    basezgridfile,
     +    fnames)

        sortinc=(sort2-sort1)/nsort
        write(*,*)
        write(*,*)'Total nsort=',nsort

        do ipick=1,npicks
          h(ipick)=0.5*(sort1+(isort(ipick)-0.501)*sortinc)
	  if (h(ipick).lt.0.0) h(ipick)=0.0
          write(*,*)'isort, h=',isort(ipick),h(ipick)
        end do

	if (isort(1).eq.0) h(1)=0.0
	write(*,*)'h=',h

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c converting the traveltime misfits to depth misfits zmisfit(ipick,ix,iy)

        if (npicks.lt.2) then
	  write(*,*)'npicks must >= 2 for this algorithm'	
	  stop
	end if

c this basecmean is used by cgr

	write(*,*)'basecmean=',basecmean

c estimating the horizontal gradients of velocity
        do ix=nxrtbeg+1,nxrtend-1
          do iy=nyrtbeg+1,nyrtend-1
	    if (ix.gt.1 .and.ix.lt.nxrt .and. iy.gt.1 .and. iy.lt.nyrt) then
	      dvx(ix,iy)=(topcmig(ix+1,iy)-topcmig(ix-1,iy))/dxrt/2.0
	      dvy(ix,iy)=(topcmig(ix,iy+1)-topcmig(ix,iy-1))/dyrt/2.0
	    else
	      dvx(ix,iy)=0.0
	      dvy(ix,iy)=0.0
	    end if
	    !a 3 by 3 smooth
 	    dvx(ix,iy)=dvx(ix,iy)/9.0
 	    dvy(ix,iy)=dvy(ix,iy)/9.0
	  end do
        end do

        do ix=nxrtbeg,nxrtend
          do iy=nyrtbeg,nyrtend

	    !the initial velocity gradient
	    ww(1)=dvx(ix,iy)
	    ww(2)=dvy(ix,iy)
	    ww(3)=dvz(ix,iy)

	    !Do not change the order for ipick
            do ipick=1,npicks

              tmisfit(ipick,ix,iy)=tmisfit(ipick,ix,iy)-seed(ipick)

	      zmisfit(ipick,ix,iy)=tmisfit(ipick,ix,iy)*0.001
     *          *basecmig(ix,iy)/2.0

c estimating the g's
 	      if (ix.eq.ixdisp.and.iy.eq.iydisp) 
     *          write(*,*)'calling getg'
	      call getg(ix,iy,ipick,nxrt,nyrt,npicks,ixdisp,iydisp,
     *	        dxrt,w_eps,h,ww,g,dg,zmisfit,dzmisfit,
     *          basezgrid,topzgrid,basecmig,topcmig,dvz00(ipick))
 	      if (ix.eq.ixdisp.and.iy.eq.iydisp) then
		write(*,*)'dvz00=',dvz00(ipick)
		write(*,*)
	      end if

            enddo
          enddo
        enddo

	write(*,*)'Before calling makeab_vzest...'
	call makeab_vzest(
     *  topcmean,                    !mean velocity of migc0
     *  basecmean,                   !mean velocity of migc1
     *  v0lambda,	             !regularization factor
     *  v1lambda,	             !regularization factor
     *  vlambda,vzlambda,            !regularization factors
     *  topcmig,                     !velocity grid at the top horizon
     *  topzgrid,basezgrid,          !the two depth grids
     *  zmisfit,                     !depth misfit
     *  g,                           !the gradient coefficient
     *  nxrt,nyrt,                   !input model sizes
     *  nxrtbeg,nxrtend,             !input model sizes
     *  nyrtbeg,nyrtend,             !input model sizes
     *  nxht,nyht,nmva,              !input model sizes
     *  npicks,                      !input number of picks
     *  1,npicks,                    !the two representative offsets
     *  dvzold,                      !input initial velocity gradient in z
     *  dvx,dvy,                     !input velocity gradient in x and y
     *  dxrt,dyrt,                   !input x and y spacing
     *  nentries,                    !number of entries in a
     *  a,                           !output Jacobian matrix a
     *  b,                           !output right hand side b
     *  icol,                        !output indice of nonzero entries in a
     *  irow,                        !output indice of nonzero entries in a
     *  ientries)                    !output actual number of entries in a

	write(*,*)'nentries,ientries=',nentries,ientries
	write(*,*)'write irol and irow...'
        open(9,file='icolandrow.m',status='new')
        write(9,*)'x = ['
        write(9,*)(icol(ixrt), ixrt=1,ientries)
	write(9,*)' ];'

        write(9,*)'y = ['
        write(9,*)(irow(ixrt), ixrt=1,ientries)
	write(9,*)' ];'
        write(9,*)'plot(x,y)'
 4000   format(500000I6)
        close(9)

	write(*,*)'Before calling cgr_vzest...'
	cgrerror=cgr_vzest(a,icol,irow,x,b,
     *   nmva,ientries,niterations,
     *   y,q,s,p,r,xupmax,xdnmax,
     *   xbak,neqns)

	do ixrt=1,nxrt
          do iyrt=1,nyrt
	    dvz(ixrt,iyrt)=0.0
          end do
        end do

        do ixht=1,nxht
          ixrt=ixht+nxrtbeg-1
          do iyht=1,nyht
            iyrt=iyht+nyrtbeg-1
	    dvz(ixrt,iyrt)=x(ixht+(iyht-1)*nxht)
	    if (abs(dvz(ixrt,iyrt)).ge.1.0) dvz(ixrt,iyrt)=-9999
          end do
        end do

	call extrapolate(
     *    dvz,
     *    nxrt,nyrt,
     *    nxrtbeg+1,nyrtbeg+1,
     *    nxrtend-1,nyrtend-1)

	write(*,*)'dvz(ixdisp,iydisp)=',dvz(ixdisp,iydisp)
	write(*,*)'dvz(50,50)=',dvz(50,50)

        do ix=1,nxrt
          do iy=1,nyrt
	    if (abs(dvz(ix,iy)).ge.1.0 .or.
     *          abs(dvx(ix,iy)).ge.1.0 .or.
     *          abs(dvy(ix,iy)).ge.1.0) then
	      dvz(ix,iy)=0.0
	      dvx(ix,iy)=0.0
	      dvy(ix,iy)=0.0
	    end if
	  end do
	end do

	do ix=1,nxrt
	  do iy=1,nyrt
            topcnew(ix,iy)=0.0
	  end do
	end do
	do ix=nxrtbeg+1,nxrtend-1
	  do iy=nyrtbeg+1,nyrtend-1
	    if (dvz(ix,iy).eq.0.0) then
	      topcnew(ix,iy)=0.0
	    else
              topcnew(ix,iy)=topcmig(ix,iy)-
     *          dvz(ix,iy)*(basezgrid(ix,iy)-topzgrid(ix,iy))*0.5
            end if
	  end do
	end do

	call extrapolate(
     *    topcnew,
     *    nxrt,nyrt,
     *    nxrtbeg+1,nyrtbeg+1,
     *    nxrtend-1,nyrtend-1)

	iunit=99
        call writegrid(iunit,c0newfile,topcnew,nxrt,nyrt)
        write(*,*)
        write(*,*)'grid file has been written to',c0newfile

        do ix=nxrtbeg+1,nxrtend-1
          do iy=nyrtbeg+1,nyrtend-1
	    work(ix-nxrtbeg,iy-nyrtbeg)=
     *	      g(npicks,ix,iy,1)-g(1,ix,iy,1)
	  end do
	end do
	iunit=99
        call writegrid(iunit,g0file,work,nxht,nyht)
        write(*,*)
        write(*,*)'grid file has been written to',g0file

        do ix=nxrtbeg+1,nxrtend-1
          do iy=nyrtbeg+1,nyrtend-1
	    work(ix-nxrtbeg,iy-nyrtbeg)=g(npicks,ix,iy,2)-g(1,ix,iy,2)
	  end do
	end do
	iunit=99
        call writegrid(iunit,g1file,work,nxht,nyht)
        write(*,*)
        write(*,*)'grid file has been written to',g1file

        do ix=nxrtbeg+1,nxrtend-1
          do iy=nyrtbeg+1,nyrtend-1
	    work(ix-nxrtbeg,iy-nyrtbeg)=g(npicks,ix,iy,3)-g(1,ix,iy,3)
	  end do
	end do
	iunit=99
        call writegrid(iunit,g2file,work,nxht,nyht)
        write(*,*)
        write(*,*)'grid file has been written to',g2file

        do ix=nxrtbeg+1,nxrtend-1
          do iy=nyrtbeg+1,nyrtend-1
	    work(ix-nxrtbeg,iy-nyrtbeg)=g(npicks,ix,iy,4)-g(1,ix,iy,4)
	  end do
	end do
	iunit=99
        call writegrid(iunit,g3file,work,nxht,nyht)
        write(*,*)
        write(*,*)'grid file has been written to',g3file

        do ix=nxrtbeg,nxrtend
          do iy=nyrtbeg,nyrtend
	    !to adjust the velocity 
	    topcmig(ix,iy)=topcmig(ix,iy)-dvz(ix,iy)*
     *        (basezgrid(ix,iy)-topzgrid(ix,iy))/2.0
	  end do
	end do




	iunit=99
        call writegrid(iunit,dvzfile,dvz,nxrt,nyrt)
        write(*,*)
        write(*,*)'grid file has been written to',dvzfile

        call writegrid(iunit,dvxfile,dvx,nxrt,nyrt)
        write(*,*)
        write(*,*)'grid file has been written to',dvxfile

        call writegrid(iunit,dvyfile,dvx,nxrt,nyrt)
        write(*,*)
        write(*,*)'grid file has been written to',dvyfile

	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Let us calculate some quantities for the estimation of gradients.
c Some formulas:
c 
c x'1s=x1-x1s;    x'1g=x1-x1g
c x'2s=0;         x'2g=0
c x'3s=x3-x3s;    x'3g=x3-x3g
c

	subroutine getg(ix,iy,ipick,nxrt,nyrt,npicks,ixdisp,iydisp,
     *	  dxrt,w_eps,h,ww,g,dg,zmisfit,dzmisfit,
     *    basezgrid,topzgrid,basecmig,topcmig,dvzthis)

	  implicit none

	  integer ix,iy,ipick,nxrt,nyrt,npicks

	  integer ixdisp,iydisp
          real zmisfit(npicks,nxrt,nyrt)
	  real w_eps,dxrt
	  real h(npicks)
          real basezgrid(nxrt,nyrt)   !depth grid at the base
          real topzgrid(nxrt,nyrt)    !depth grid at the top
          real basecmig(nxrt,nyrt)    !velocity grid at the base
          real topcmig(nxrt,nyrt)     !velocity grid at the top
	  real g(npicks,nxrt,nyrt,4)   !The derivatives with repsect to
	  real dg(npicks,4)
          real dzmisfit(npicks)

	  double precision acoshf
	  real dvzthis
	  real stemp,gtemp,temp
          real xp1s,xp1g,xp2s,xp2g,xp3s,xp3g   
	  real rhos2,rhog2,rhos,rhog
	  real w,ww(3)      !the velocity gradient
	  real v0           !velocity at the imaged point
	  real cos2theta    !cosine of the opening angle
	  real costheta     !cosing of the half opening angle
	  integer ixmv      !half offset in grid
	  real v0s,v0g      !velocity at the source and geophone
          real taus,taug    !traveltime from the source/receiver

          xp1s=-h(ipick)
	  xp1g=h(ipick)
	  xp2s=0.0
	  xp2g=0.0
	  xp3s=basezgrid(ix,iy)-topzgrid(ix,iy)
	  xp3g=xp3s

	  !raypath length of the source leg
	  rhos2=xp1s*xp1s+xp2s*xp2s+xp3s*xp3s
	  rhog2=xp1g*xp1g+xp2s*xp2s+xp3g*xp3g
	  rhos=sqrt(rhos2)
	  rhog=sqrt(rhog2)


	  if (ix.eq.ixdisp .and. iy.eq.iydisp) then
	    write(*,*)
	    write(*,*)'Inside getg...'
	    write(*,*)'ipick=',ipick
	    write(*,*)'rhos2,rhog2,rhos,rhog=',rhos2,rhog2,rhos,rhog
	  end if 
	
	  !the norm of the velocity gradient
          w=sqrt(ww(1)*ww(1)+ww(2)*ww(2)+ww(3)*ww(3))

	  !the velocity at the imaged point
	  v0=basecmig(ix,iy)

	  !full opening angle
	  cos2theta=xp1s*xp1g+xp2s*xp2g+xp3s*xp3g
	  cos2theta=cos2theta/rhos/rhog

	  !half opening angle
	  costheta=sqrt((1.0+cos2theta)/2.0) 

	  if (ix.eq.ixdisp .and. iy.eq.iydisp) then
	    write(*,*)
	    write(*,*)'Inside getg...'
	    write(*,*)'ipick=',ipick
	    write(*,*)'v0,cos2theta,costheta=',v0,cos2theta,costheta
	  end if 

	  !half offset is equivalent to this many of grid points	     
	  ixmv=abs(xp1s)/dxrt+0.5

	  !the velocities at the source and receiver 
	  v0s=topcmig(MAX(ix-ixmv,   1),iy)
	  v0g=topcmig(MIN(ix+ixmv,nxrt),iy)


	  if (ix.eq.ixdisp .and. iy.eq.iydisp) then
	    write(*,*)
	    write(*,*)'Inside getg...'
	    write(*,*)'ipick=',ipick
	    write(*,*)'ixmv,v0s,v0g=',ixmv,v0s,v0g
	  end if 

	  if (w.lt.w_eps) then  !straight rays

   	    taus=rhos/(v0+v0s)*2.0
	    taug=rhog/(v0+v0g)*2.0

	    if (ix.eq.ixdisp .and. iy.eq.iydisp) then
	      write(*,*)
	      write(*,*)'Inside getg...'
	      write(*,*)'ipick=',ipick
	      write(*,*)'taus,taug=',taus,taug
	    end if 

	    stemp=rhos2/taus/v0s**3
	    gtemp=rhog2/taug/v0g**3

	    g(ipick,ix,iy,1)=v0*0.5/costheta*(stemp + gtemp)


	    temp=xp3s*rhos/2.0/v0s/v0s+xp3g*rhog/2.0/v0g/v0g

	    g(ipick,ix,iy,2)=v0*0.5/costheta*
     *         (xp1s*stemp - temp + xp1g*gtemp )

	    g(ipick,ix,iy,3)=v0*0.5/costheta*
     *         (xp2s*stemp - temp + xp2g*gtemp)

	    g(ipick,ix,iy,4)=v0*0.5/costheta*
     *         (xp3s*stemp - temp + xp3g*gtemp)


	    if (ix.eq.ixdisp .and. iy.eq.iydisp) then
	      write(*,*)
	      write(*,*)'Inside getg...'
	      write(*,*)'ipick=',ipick
	      write(*,*)'temp,xp1s*stemp,xp1g*gtemp=',
     *          temp,xp1s*stemp,xp1g*gtemp
	      write(*,*)'temp,xp2s*stemp,xp2g*gtemp=',
     *          temp,xp2s*stemp,xp2g*gtemp
	      write(*,*)'temp,xp3s*stemp,xp3g*gtemp=',
     *          temp,xp3s*stemp,xp3g*gtemp
	      write(*,*)'g(1),g(2),g(3),g(4)=',
     *          g(ipick,ix,iy,1),g(ipick,ix,iy,2),
     *          g(ipick,ix,iy,3),g(ipick,ix,iy,4)
	      write(*,*)'g(1)*375.0,g(2),g(3),g(4)=',
     *          g(ipick,ix,iy,1)*375.0,g(ipick,ix,iy,2),
     *          g(ipick,ix,iy,3),g(ipick,ix,iy,4)
	    end if 


	  else  !the circular rays

	    taus=1.0/w*acoshf(1.0+w*w*rhos2/2.0/v0s/(xp3s*w+v0s))
	    taug=1.0/w*acoshf(1.0+w*w*rhog2/2.0/v0g/(xp3g*w+v0g))

	    g(ipick,ix,iy,1)=( rhos2*(2.0*v0s+xp3s*w)
     *          /sinh(w*taus)/v0s/v0s/(v0s+w*xp3s)**2 
     *             +
     *             rhog2*(2.0*v0g+xp3g*w)
     *          /sinh(w*taug)/v0g/v0g/(v0g+w*xp3g)**2
     *                         )*w*v0/4.0/costheta

            g(ipick,ix,iy,2)=( ww(1)*(taus+taug)/w/w +
     *          (w*xp1s/v0s - ww(1)/w) *
     * rhos2*(xp3s*w+2.0*v0s)/2.0/sinh(w*taus)/v0s/(xp3s*w+v0s)**2 
     *          +
     *          (w*xp1g/v0g - ww(1)/w) *
     * rhog2*(xp3g*w+2.0*v0g)/2.0/sinh(w*taug)/v0g/(xp3g*w+v0g)**2 
     *                 )*v0/2.0/costheta

            g(ipick,ix,iy,3)=( ww(2)*(taus+taug)/w/w +
     *        (w*xp1s/v0s - ww(2)/w) *
     * rhos2*(xp3s*w+2.0*v0s)/2.0/sinh(w*taus)/v0s/(xp3s*w+v0s)**2 
     *      +
     *        (w*xp1g/v0g - ww(2)/w) *
     * rhog2*(xp3g*w+2.0*v0g)/2.0/sinh(w*taug)/v0g/(xp3g*w+v0g)**2
     *                     )*v0/2.0/costheta

            g(ipick,ix,iy,4)=( ww(3)*(taus+taug)/w/w +
     *        (w*xp1s/v0s - ww(3)/w) *
     * rhos2*(xp3s*w+2.0*v0s)/2.0/sinh(w*taus)/v0s/(xp3s*w+v0s)**2 
     *      +
     *        (w*xp1g/v0g - ww(3)/w) *
     * rhog2*(xp3g*w+2.0*v0g)/2.0/sinh(w*taug)/v0g/(xp3g*w+v0g)**2
     *                     )*v0/2.0/costheta
	  end if

	  if (ipick.ne.1) then
	    dg(ipick,1)=g(ipick,ix,iy,1)-g(1,ix,iy,1)
	    dg(ipick,2)=g(ipick,ix,iy,2)-g(1,ix,iy,2)
	    dg(ipick,3)=g(ipick,ix,iy,3)-g(1,ix,iy,3)
	    dg(ipick,4)=g(ipick,ix,iy,4)-g(1,ix,iy,4)
	    dzmisfit(ipick)=zmisfit(ipick,ix,iy)-zmisfit(1,ix,iy)
	    dvzthis=-2.0*dzmisfit(ipick)/dg(ipick,4)
	  end if

	  if (ix.eq.ixdisp.and.iy.eq.iydisp) then
	    write(*,*)
	    write(*,*)'ipick=',ipick
	    write(*,*)'xp1s,xp2s,xp3s=',xp1s,xp2s,xp3s
	    write(*,*)'xp1g,xp2g,xp3g=',xp1g,xp2g,xp3g
	    write(*,*)'w=',w
	    write(*,*)'stemp,gtemp=',stemp,gtemp
	    write(*,*)'taus,taug=',taus,taug
	    write(*,*)'v0=',v0,'costheta=',costheta
	    write(*,*)'ixmv=',ixmv
 	    write(*,*)'v0s,v0g=',v0s,v0g
	    write(*,*)'rhos,rhog=',sqrt(rhos2),sqrt(rhog2)
	    write(*,*)'g0=',g(ipick,ix,iy,1)
	    write(*,*)'g=',g(ipick,ix,iy,2),g(ipick,ix,iy,3),g(ipick,ix,iy,4)
	    if (ipick.ne.1) then
	      write(*,*)'dzmisfit=',dzmisfit(ipick)
	      write(*,*)'dg=',dg(ipick,1),dg(ipick,2),
     *          dg(ipick,3),dg(ipick,4)
	      write(*,*)'dg(ipick,1)*375.0=',dg(ipick,1)*375.0
	      write(*,*)'dvzthis at ipick:=',dvzthis,ipick
	    end if
	  end if
	return
	end

	subroutine extrapolate(
     *    work,
     *    nxrt,nyrt,
     *    ixfirst,iyfirst,
     *    ixlast,iylast)

	  implicit none

	  integer nxrt,nyrt
	  real work(nxrt,nyrt)
	  integer ixfirst,iyfirst
	  integer ixlast,iylast
	
	  integer ixrt,iyrt

	  write(*,*)'inside extrapolate'
	  write(*,*)'nxrt,nyrt=',nxrt,nyrt
	  write(*,*)'ixfirst,iyfirst=',ixfirst,iyfirst
	  write(*,*)'ixlast,iylast=',ixlast,iylast

          do iyrt=iyfirst,iylast
	    do ixrt=1,ixfirst-1
	      work(ixrt,iyrt)=work(ixfirst,iyrt)
            end do
	    do ixrt=ixlast+1,nxrt
	      work(ixrt,iyrt)=work(ixlast,iyrt)
            end do
          end do

          do ixrt=ixfirst,ixlast
	    do iyrt=1,iyfirst-1
	      work(ixrt,iyrt)=work(ixrt,iyfirst)
            end do
	    do iyrt=iylast+1,nyrt
	      work(ixrt,iyrt)=work(ixrt,iylast)
            end do
          end do

          do ixrt=1,ixfirst-1
	    do iyrt=1,iyfirst-1
              work(ixrt,iyrt)=work(ixfirst,iyfirst) 
            end do
          end do

          do ixrt=1,ixfirst-1
	    do iyrt=iylast+1,nyrt
              work(ixrt,iyrt)=work(ixfirst,iylast) 
            end do
          end do

          do ixrt=ixlast+1,nxrt
	    do iyrt=1,iyfirst-1
              work(ixrt,iyrt)=work(ixlast,iyfirst) 
            end do
          end do

          do ixrt=ixlast+1,nxrt
	    do iyrt=iylast+1,nyrt
              work(ixrt,iyrt)=work(ixlast,iylast) 
            end do
          end do
	end
      subroutine writegrid(iunit,gridfile,grid,nxrt,nyrt)
      integer iunit
      character*120 gridfile
      integer nxrt,nyrt
      real grid(nxrt,nyrt)

      open(unit=iunit,file=gridfile,status="new")

      do ix=1,nxrt
         write(iunit,'(9999e12.5)') (grid(ix,iy), iy=1,nyrt)
      end do
      close(iunit)
      return
      end

      subroutine readgrid(iunit,gridfile,grid,nxrt,nyrt)
      integer iunit
      character*120 gridfile
      integer nxrt,nyrt
      real grid(nxrt,nyrt)

      open(unit=iunit,file=gridfile,status="old")

      do ix=1,nxrt
         read(iunit, '(9999e12.5)') (grid(ix,iy), iy=1,nyrt)
      end do
      close(iunit)
      return
      end
      subroutine getparms_pracmva(
     +  npicks,                  !integer npicks=2
     +  niterations,             !integer niterations=40
     +  vlambda,                 !real vlambda=3.0
     +  v0lambda,                !real v0lambda=1.0
     +  v1lambda,                !real v1lambda
     +  vzlambda,                !real vzlambda
     +  zlambda,                 !real zlambda
     +  w_eps,                   !real w_eps /0.1/
     +  azimuth,                 !real azimuth /0.0/
     +  xupmaxconst,             !real xupmaxconst=100
     +  xdnmaxconst,             !real xdnmaxconst=-100
     +  topcmean,                !real topcmean
     +  basecmean,               !real basecmean
     +  nxrt,                    !number of samples in x
     +  nyrt,                    !number of samples in y
     +  nxrtbeg,nxrtend,         !CIGs to be solved
     +  nyrtbeg,nyrtend,         !CIGs to be solved
     +  dxrt,                    !CIG spacing
     +  dyrt,                    !CIG spacing
     +  ixdisp,                  !ix for display
     +  iydisp,                  !iy for display
     +  iseed,                   !seed number
     +  nsort,                   !number of offsets
     +  sort1,
     +  sort2,
     +  isort,                   !isort(2)
     +  fnames,                  !integer*120 fnames(2) misfit file names
     +  topcmigfile,             !top c0
     +  topcgmigfile,            !top cg
     +  topzgridfile,            !top z
     +  basecmigfile,            !base c
     +  basezgridfile,           !base z
     +  dvxfile,                 !dvxfile to output
     +  dvyfile,                 !dvyfile to output
     +  dvzfile,                 !dvzfile to output
     +  c0newfile,               !update c
     +  g0file,                  !output g0
     +  g1file,                  !output g1
     +  g2file,                  !output g2
     +  g3file,                  !output g3
     +  topc,                    !real topc=0 then read in from file
     +  topcg)                   !real topcg=0 then read in from file

      implicit none

      integer npicks
      integer nxrt,nyrt
      real vlambda
      real v0lambda
      real v1lambda
      real vzlambda
      real zlambda
      integer niterations
      real topcmean
      real basecmean
      real xupmaxconst
      real xdnmaxconst
      real dxrt,dyrt
      integer ixdisp,iydisp
      integer iseed
      integer isort(2)
      integer nsort
      real sort1
      real sort2
      integer nxrtbeg
      integer nxrtend
      integer nyrtbeg
      integer nyrtend
      real w_eps
      real azimuth

      character*120 fnames(2)
      character*120 topcmigfile
      real topc
      character*120 topcgmigfile
      real topcg
      character*120 topzgridfile
      character*120 basecmigfile
      character*120 basezgridfile
      character*120 dvxfile
      character*120 dvyfile
      character*120 dvzfile
      character*120 c0newfile
      character*120 g0file
      character*120 g1file
      character*120 g2file
      character*120 g3file

      character*120 cbuf         !read in the first line

      !local copies
      integer nc                 !length of cbuf
      integer ipick

1     continue
      read(5,'(i3,a)') nc,cbuf
      if (cbuf(1:1).eq.'*') goto 1

      write(6,*)'Input npicks=>'
      read(5,*) npicks
      write(6,*)'Input niterations=>'
      read(5,*) niterations
      write(6,*)'Input vlambda=>'
      read(5,*) vlambda
      write(6,*)'Input v0lambda=>'
      read(5,*) v0lambda
      write(6,*)'Input v1lambda=>'
      read(5,*) v1lambda
      write(6,*)'Input vzlambda=>'
      read(5,*) vzlambda
      write(6,*)'Input zlambda=>'
      read(5,*) zlambda
      write(6,*)'Input w_eps=>'
      read(5,*) w_eps
      write(6,*)'Input azimuth=>'
      read(5,*) azimuth
      write(6,*)'Input xupmaxconst=>'
      read(5,*) xupmaxconst
      write(6,*)'Input xdnmaxconst=>'
      read(5,*) xdnmaxconst
      write(6,*)'Input topcmean=>'
      read(5,*) topcmean
      write(6,*)'Input basecmean=>'
      read(5,*) basecmean
      write(6,*)'Input nxrt =>'
      read(5,*) nxrt                  !second line in .deck file
      write(6,*)'Input nyrt =>'
      read(5,*) nyrt                  !third line in .deck file
      write(6,*)'Input nxrtbeg =>'
      read(5,*) nxrtbeg               !4th line in .deck file
      write(6,*)'Input nxrtend =>'
      read(5,*) nxrtend               !5th line in .deck file
      write(6,*)'Input nyrtbeg =>'
      read(5,*) nyrtbeg               !6th line in .deck file
      write(6,*)'Input nyrtend =>'
      read(5,*) nyrtend               !7th line in .deck file
      write(6,*)'Input ixdisp=>'
      read(5,*) ixdisp
      write(6,*)'Input iydisp=>'
      read(5,*) iydisp
      write(6,*)'Input dxrt =>'
      read(5,*) dxrt                  !second line in .deck file
      write(6,*)'Input dyrt =>'
      read(5,*) dyrt                  !third line in .deck file
      write(6,*)'Input iseed=>'
      read(5,*) iseed
      write(6,*)'Input nsort=>'
      read(5,*) nsort
      write(6,*)'Input sort1=>'
      read(5,*) sort1
      write(6,*)'Input sort2=>'
      read(5,*) sort2
      write(6,*)'Input isort=> (for ipick=1,npicks)'
      do ipick=1,npicks
        read(5,*) isort(ipick)
      end do

      write(6,*)'Input fnames => (for ipick=1,npicks)'
      do ipick=1,npicks
        read(5,*) fnames(ipick)    
      end do

      write(6,*)'Input topc =>'
      read(5,*) topc
      write(6,*)'Input topcmigfile =>'
      read(5,*) topcmigfile      

      write(6,*)'Input topcg =>'
      read(5,*) topcg
      write(6,*)'Input topcgmigfile =>'
      read(5,*) topcgmigfile   

      write(6,*)'Input topzgridfile =>'
      read(5,*) topzgridfile    

      write(6,*)'Input basecmigfile =>'
      read(5,*) basecmigfile  

      write(6,*)'Input basezgridfile =>'
      read(5,*) basezgridfile   

      write(6,*)'Input dvxfile =>'
      read(5,*) dvxfile   

      write(6,*)'Input dvyfile =>'
      read(5,*) dvyfile   

      write(6,*)'Input dvzfile =>'
      read(5,*) dvzfile   

      write(6,*)'Input c0newfile =>'
      read(5,*) c0newfile      

      write(6,*)'Input g0file =>'
      read(5,*) g0file       

      write(6,*)'Input g1file =>'
      read(5,*) g1file       

      write(6,*)'Input g2file =>'
      read(5,*) g2file       

      write(6,*)'Input g3file =>'
      read(5,*) g3file       

      write(6,*)
      write(6,*)'nxrt=',nxrt           !second line in .deck file
      write(6,*)'nyrt=',nyrt           !third line in .deck file
      write(6,*)'nxrtbeg=',nxrtbeg     !4th line in .deck file
      write(6,*)'nxrtend=',nxrtend     !5th line in .deck file
      write(6,*)'nyrtbeg=',nyrtbeg     !6th line in .deck file
      write(6,*)'nyrtend=',nyrtend     !7th line in .deck file
      write(6,*)'ixdisp=',ixdisp
      write(6,*)'iydisp=',iydisp
      write(6,*)'iseed=',iseed
      write(6,*)'nsort=',nsort
      write(6,*)'sort1=',sort1
      write(6,*)'sort2=',sort2
      write(6,*)'npicks=',npicks
      write(6,*)'isort=',(isort(ipick),ipick=1,npicks)

      do ipick=1,npicks
        write(6,*)'fnames(',ipick,')=',fnames(ipick)   
      end do

      write(6,*)'topc=',topc
      write(6,*)'topcg=',topcg
      write(6,*)'topcmigfile=',topcmigfile 
      write(6,*)'topcgmigfile=',topcgmigfile 
      write(6,*)'basecmigfile=',basecmigfile
      write(6,*)'topzgridfile=',topzgridfile
      write(6,*)'basezgridfile=',basezgridfile
      write(6,*)'c0newfile=',c0newfile   
      write(6,*)'g0file=',g0file   
      write(6,*)'g1file=',g1file   
      write(6,*)'g2file=',g2file   
      write(6,*)'g3file=',g3file   

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c subroutine readgrids.f   
c Zhaobo Meng / July 1998
c CWP/CSM                    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine readgrids_vzest(
     +    topc,
     +    topcg,
     +    npicks,	  !number of picks
     +    nxrt,nyrt,      !project size
     +    basecmig,       !velocity grid at the base
     +    topcmig,        !velocity grid at the top
     +    topcgmig,       !velocity grid at the top
     +    topzgrid,       !depth grid at the top
     +    basezgrid,      !depth grid at the base
     +    work_read,
     +    tmisfit,
     +    topcmigfile,
     +    topcgmigfile,
     +    basecmigfile,
     +    topzgridfile,
     +    basezgridfile,
     +    fnames)

        implicit none
c these parameters are used to read the grids

	integer iunit

c global parms
	integer nxrt,nyrt,npicks
	real topc,topcg

        real basecmig(nxrt,nyrt)     !velocity grid at the base
        real topcmig(nxrt,nyrt)	     !velocity grid at the top
        real topcgmig(nxrt,nyrt)     !velocity grid at the top
        real topzgrid(nxrt,nyrt)     !depth grid at the top
        real basezgrid(nxrt,nyrt)    !depth grid at the base
        real tmisfit(npicks,nxrt,nyrt)

        character*120 topcmigfile
        character*120 topcgmigfile
        character*120 basecmigfile
        character*120 topzgridfile
        character*120 basezgridfile
        character*120 fnames(3)

c local parms
c work will be used to read in traveltime
	real work_read(nyrt*nxrt)
	

c work variables 
        integer ix,iy,ipick

	write(*,*)
	write(*,*)'Inside readgrids...'

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Now, read the migration velicity at the topcmig. The file name is described '
c in the deck. 

	if (topc.gt.0.00001) then
          do iy=1,nyrt
            do ix=1,nxrt
              topcmig(ix,iy)=topc
            enddo
          enddo
	else
          iunit=99
          print *,'Read in',topcmigfile
          call readgrid(iunit,topcmigfile,work_read,nxrt,nyrt)
          do iy=1,nyrt
            do ix=1,nxrt
              topcmig(ix,iy)=work_read(ix+(iy-1)*nxrt)
            enddo
          enddo
	end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Now, read the migration velicity at the topcmig. The file name is described '
c in the deck. It is also given in the modspec file.

	write(*,*)'topcg=',topcg
	if (abs(topcg).gt.0.00001) then
          do iy=1,nyrt
            do ix=1,nxrt
              topcgmig(ix,iy)=topcg
            enddo
          enddo
	else
          iunit=99
          print *,'Read in',topcgmigfile
          call readgrid(iunit,topcgmigfile,work_read,nxrt,nyrt)
          do iy=1,nyrt
            do ix=1,nxrt
              topcgmig(ix,iy)=work_read(ix+(iy-1)*nxrt)
            enddo
          enddo
	end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Now, read the migration velicity at the basecmig.  
c The file name is described in the deck.

        iunit=99
        print *,'Read in',basecmigfile 
        call readgrid(iunit,basecmigfile,work_read,nxrt,nyrt)
        do iy=1,nyrt
          do ix=1,nxrt
            basecmig(ix,iy)=work_read(ix+(iy-1)*nxrt)
          enddo
        enddo

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Now, read the topzgrid. The file name is described in the deck.

        iunit=99
        print *,'Read in topzgrid',topzgridfile
        call readgrid(iunit,topzgridfile,work_read,nxrt,nyrt)
        do iy=1,nyrt
          do ix=1,nxrt
            topzgrid(ix,iy)=work_read(ix+(iy-1)*nxrt)
          enddo
        enddo

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Now, read the basezgrid. The file name is described in the deck.

        iunit=99
        print *,'Read in basezgrid',basezgridfile
        call readgrid(iunit,basezgridfile,work_read,nxrt,nyrt)
        do iy=1,nyrt
          do ix=1,nxrt
            basezgrid(ix,iy)=work_read(ix+(iy-1)*nxrt)
          enddo
        enddo

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Now, read the traveltime misfits.  There should be npicks files,
c with file names described in the deck.

	iunit=99
        print *,'Read in tmisfit'
        do ipick=1,npicks
	  print *,fnames(ipick)
	  call readgrid(iunit,fnames(ipick),work_read,nxrt,nyrt)

	  do iy=1,nyrt
	    do ix=1,nxrt
	      tmisfit(ipick,ix,iy)=work_read(ix+(iy-1)*nxrt)
	    enddo
	  enddo
        enddo
        end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                       c
c       subroutine makeab_vzest()                                       c
c       Author: Zhaobo Meng                                             c
c       CWP/CSM                                                         c
c       Nov. 1998                                                       c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  
c     Assembly Jacobian matrix a which consists of nmva by 3*nmva.

      subroutine makeab_vzest(
     *  topcmean,                    !mean velocity of migc0
     *  basecmean,                   !mean velocity of migc1
     *  v0lambda,	             !regularization factor for top c0
     *  v1lambda,	             !regularization factor for base c1
     *  vlambda,vzlambda,	     !regularization factors
     *  topcmig,		     !velocity grid at the top horizon
     *  topzgrid,basezgrid,          !the two depth grids
     *  zmisfit,	             !depth misfit
     *  g,                           !the gradient coefficient
     *  nxrt,nyrt,                   !input model sizes
     *  nxrtbeg,nxrtend,             !input model sizes
     *  nyrtbeg,nyrtend,             !input model sizes
     *  nxht,nyht,nmva,              !input model sizes
     *  npicks,			     !input number of picks
     *  ipick1,ipick2, 	             !the two representative offsets 
     *  dvzold,	  		     !input old dvz
     *  dvx,dvy,		     !input velocity gradient in x and y 
     *  dxrt,dyrt,	             !input x and y spacing
     *  nentries,	             !number of entries in a
     *  a,                           !output Jacobian matrix a
     *  b,                           !output right hand side b
     *  icol,                        !output indice of nonzero entries in a
     *  irow,                        !output indice of nonzero entries in a
     *  ientries)                    !output actual number of entries in a

        implicit none

	integer nxrt,nyrt            !input model sizes
        integer npicks               !input number of picks
	real topcmean		     !mean velocity of migc0
	real basecmean		     !mean velocity of migc1
	real v0lambda                !regularization factors
	real v1lambda                !regularization factors
	real vlambda,vzlambda        !regularization factors
        real topcmig(nxrt,nyrt)      !c grid at the top
        real topzgrid(nxrt,nyrt)     !depth grid at the top
        real basezgrid(nxrt,nyrt)    !depth grid at the base
        real zmisfit(npicks,nxrt,nyrt)
        real g(npicks,nxrt,nyrt,4)   !derivatives with repsect to
        real dvx(nxrt,nyrt)          !derivatives with repsect to
        real dvy(nxrt,nyrt)          !derivatives with repsect to
        real dvzold(nxrt,nyrt)       !derivatives with repsect to
        real a(1)                    !Jacobian matrix
        real b(1)                    !right hand side of the linear system
        integer icol(1)              !column positions of nonzero a entries
        integer irow(1)              !row positions of nonzero a entries
        integer ientries             !actual number of nonzero entries
	integer itrunk

	integer nxrtbeg,nxrtend      !input model sizes
	integer nyrtbeg,nyrtend      !input model sizes
	integer nxht,nyht            !input model sizes
	integer ipick1,ipick2        !input number of picks
        real dxrt,dyrt	             !grid spacing
        integer nentries	     !number of entries
	integer nmva

	integer ixdisp,iydisp

                                     !local parms
        integer ixrt                 !index for base horizon point
        integer iyrt                 !index for base horizon point
        integer ixht                 !index for base horizon point
        integer iyht                 !index for base horizon point

        integer ioffset              !index for offsets

        real epsilon                 !any path cut smaller not counted
	real centry                  !center entry
	real coef,c0der2
        real dxrt2,dyrt2,dz
	integer imva
	real dg1,dg2,dg3,dg4

	ixdisp=27
        iydisp=50

        print *,'print parms in makeab_vzest'
        print *,'nxrt=',nxrt
        print *,'nyrt=',nyrt
        print *,'nmva=',nmva
        print *,'nentries=',nentries
        print *,'nxrtbeg=',nxrtbeg
        print *,'nxrtend=',nxrtend
        print *,'nyrtbeg=',nyrtbeg
        print *,'nyrtend=',nyrtend
        print *,'vlambda,vzlambda=',vlambda,vzlambda
        print *,'ipick1,ipick2=',ipick1,ipick2
	write(*,*)'dxrt,dyrt=',dxrt,dyrt
	print *,'ixdisp,iydisp=',ixdisp,iydisp

	if (nxht.ne.nxrtend-nxrtbeg+1 .or. 
     *      nyht.ne.nyrtend-nyrtbeg+1 .or.
     *      nmva.ne.nxht*nyht) then
          write(*,*)'Dimension not correct'
          stop
        end if

	if (ipick1.eq.ipick2) then
	  write(*,*)'ipick1=ipick2, not allowed'
	  stop
	end if 

	dxrt2=dxrt*dxrt
        dyrt2=dyrt*dyrt

        ientries=0
        do iyrt=nyrtbeg,nyrtend
          iyht=iyrt-nyrtbeg+1            !index for tomography area
          do ixrt=nxrtbeg,nxrtend
            ixht=ixrt-nxrtbeg+1          !index for tomography area
	    imva=ixht+(iyht-1)*nxht

	    !the vertical depth of this horizon
	    dz=basezgrid(ixrt,iyrt)-topzgrid(ixrt,iyrt)

	    dg1=g(ipick2,ixrt,iyrt,1)-g(ipick1,ixrt,iyrt,1)
	    dg2=g(ipick2,ixrt,iyrt,2)-g(ipick1,ixrt,iyrt,2)
	    dg3=g(ipick2,ixrt,iyrt,3)-g(ipick1,ixrt,iyrt,3)
	    dg4=g(ipick2,ixrt,iyrt,4)-g(ipick1,ixrt,iyrt,4)

	    !the diagonal entry of matrix a
            centry=0.5*dz*dg1-dg4

	    !display one sample
	    if (ixrt.eq.ixdisp .and. iyrt.eq.iydisp) then 
	      write(*,*)'centry=',centry
	      write(*,*)'dz=',dz
              write(*,*)'dg1,dg2,dg3,dg4=',dg1,dg2,dg3,dg4
	      write(*,*)'dg1*dz/2=',dg1*dz/2.0
	    end if

	    !second order derivative
	    c0der2=(topcmig(ixrt+1,iyrt)-2.0*topcmig(ixrt,iyrt)
     *        +topcmig(ixrt-1,iyrt))/dxrt2
	    c0der2=c0der2+(topcmig(ixrt,iyrt+1)-2.0*topcmig(ixrt,iyrt)
     *        +topcmig(ixrt,iyrt-1))/dyrt2

	    !rhs (see Zhaobo's thesis)
	    ! delta v0 = - 0.5*delta_z * delta_dvz

	    !this is the center entry, that is, the equations themselves 
            ientries=ientries+1
	    if (ientries.gt.nentries) then
	      write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	      write(*,*)'ientries,nentrices=',ientries,nentries 
	      stop
	    end if
	    a(ientries)=0.5*dz*dg1-dg4
            icol(ientries)=imva
            irow(ientries)=imva

	    !regularization should not be applied to the boundary
	    if (ixht.eq.1 .or. iyht.eq.1 .or. 
     *          ixht.eq.nxht .or. iyht.eq.nyht) go to 999

	    coef=-(1.0/dxrt2+1.0/dyrt2)

	    !this is the right entry in x
            ientries=ientries+1
            if (ientries.gt.nentries) then
              write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht
              write(*,*)'ientries,nentrices=',ientries,nentries
              stop
            end if
            a(ientries)=-0.25*dg2*dz/dxrt
            icol(ientries)=imva+1
            irow(ientries)=imva

	    !this is the left entry in x
            ientries=ientries+1
            if (ientries.gt.nentries) then
              write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht
              write(*,*)'ientries,nentrices=',ientries,nentries
              stop
            end if
            a(ientries)=0.25*dg2*dz/dxrt
            icol(ientries)=imva-1
            irow(ientries)=imva

	    !this is the front entry in y
            ientries=ientries+1
            if (ientries.gt.nentries) then
              write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht
              write(*,*)'ientries,nentrices=',ientries,nentries
              stop
            end if
            a(ientries)=-0.25*dg3*dz/dyrt
            icol(ientries)=imva+nxht
            irow(ientries)=imva

	    !this is the front entry in y
            ientries=ientries+1
            if (ientries.gt.nentries) then
              write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht
              write(*,*)'ientries,nentrices=',ientries,nentries
              stop
            end if
            a(ientries)=0.25*dg3*dz/dyrt
            icol(ientries)=imva-nxht
            irow(ientries)=imva

	    b(imva)=zmisfit(ipick2,ixrt,iyrt)-zmisfit(ipick1,ixrt,iyrt)

***************************************************************

	    itrunk=0

	    !vlambda=0 means no regularization to v0
	    if (vlambda.eq.0.0) goto 9

	    itrunk=itrunk+1
	    !Now applying regularization to velocity: the resultant
	    !top horizon velocity should be smooth.  Here, I use
	    !the second order derivative being small as a constraint.

	    !this is the center entry for padding
            ientries=ientries+1
	    if (ientries.gt.nentries) then
	      write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	      write(*,*)'ientries,nentrices=',ientries,nentries 
	      stop
	    end if
	    a(ientries)=-vlambda*dz/(dxrt2+dyrt2)
            icol(ientries)=imva
            irow(ientries)=nmva*itrunk+imva

	    !the entry in x on the left
	    if (ixrt.gt.nxrtbeg) then
	      ientries=ientries+1
	      if (ientries.gt.nentries) then
	        write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	        write(*,*)'ientries,nentrices=',ientries,nentries 
	        stop
	      end if
	      a(ientries)=vlambda*dz/dxrt2*0.5
	      icol(ientries)=imva-1
              irow(ientries)=nmva*itrunk+imva
	    end if

	    !the entry in x on the right
	    if (ixrt.lt.nxrtend) then
	      ientries=ientries+1
	      if (ientries.gt.nentries) then
	        write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	        write(*,*)'ientries,nentrices=',ientries,nentries 
	        stop
	      end if
              a(ientries)=vlambda*dz/dxrt2*0.5
              icol(ientries)=imva+1
              irow(ientries)=nmva*itrunk+imva
	    end if

	    !the entry in y in the front
	    if (iyrt.lt.nyrtend) then
	      ientries=ientries+1
	      if (ientries.gt.nentries) then
	        write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	        write(*,*)'ientries,nentrices=',ientries,nentries 
	        stop
	      end if
	      a(ientries)=vlambda*dz/dyrt2*0.5
	      icol(ientries)=imva+nxht
              irow(ientries)=nmva*itrunk+imva
	    end if

	    !the entry in y in the back
	    if (iyrt.gt.nyrtbeg) then
	      ientries=ientries+1
	      if (ientries.gt.nentries) then
	        write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	        write(*,*)'ientries,nentrices=',ientries,nentries 
	        stop
	      end if
              a(ientries)=vlambda*dz/dyrt2*0.5
              icol(ientries)=imva-nxht
              irow(ientries)=nmva*itrunk+imva
	    end if

	    !the right hand side
            b(nmva+imva)=vlambda*c0der2

 9          continue

	    if (vzlambda.eq.0.0) goto 99

	    itrunk=itrunk+1

	    !applying regularization to v_z.  I use the second order
	    !derivative of dvz being small as the constraint.
	    !this is the center entry for padding
            ientries=ientries+1
	    if (ientries.gt.nentries) then
	      write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	      write(*,*)'ientries,nentrices=',ientries,nentries 
	      stop
	    end if
	    a(ientries)=-vzlambda*2.0/(dxrt2+dyrt2)
            icol(ientries)=imva
            irow(ientries)=itrunk*nmva+imva

	    !the entry in x on the left
	    if (ixrt.gt.nxrtbeg) then
	      ientries=ientries+1
	      if (ientries.gt.nentries) then
	        write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	        write(*,*)'ientries,nentrices=',ientries,nentries 
	        stop
	      end if
	      a(ientries)=vzlambda/dxrt2
	      icol(ientries)=imva-1
              irow(ientries)=itrunk*nmva+imva
	    end if

	    !the entry in x on the right
	    if (ixrt.lt.nxrtend) then
	      ientries=ientries+1
	      if (ientries.gt.nentries) then
	        write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	        write(*,*)'ientries,nentrices=',ientries,nentries 
	        stop
	      end if
              a(ientries)=vzlambda/dxrt2
              icol(ientries)=imva+1
              irow(ientries)=itrunk*nmva+imva
	    end if

	    !the entry in y in the front
	    if (iyrt.lt.nyrtend) then
	      ientries=ientries+1
	      if (ientries.gt.nentries) then
	        write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	        write(*,*)'ientries,nentrices=',ientries,nentries 
	        stop
	      end if
	      a(ientries)=vzlambda/dyrt2
	      icol(ientries)=imva+nxht
              irow(ientries)=itrunk*nmva+imva
	    end if

	    !the entry in y in the back
	    if (iyrt.gt.nyrtbeg) then
	      ientries=ientries+1
	      if (ientries.gt.nentries) then
	        write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	        write(*,*)'ientries,nentrices=',ientries,nentries 
	        stop
	      end if
              a(ientries)=vzlambda/dyrt2
              icol(ientries)=imva-nxht
              irow(ientries)=itrunk*nmva+imva
	    end if

            b(itrunk*nmva+imva)=vzlambda*(
     *                     -(dvzold(ixrt+1,iyrt)-2.0*dvzold(ixrt,iyrt)
     *                      +dvzold(ixrt-1,iyrt))/dxrt2 
     *                     -(dvzold(ixrt,iyrt+1)-2.0*dvzold(ixrt,iyrt)
     *                      +dvzold(ixrt,iyrt-1))/dyrt2 
     *                                   )

 99   	    continue

	    if (v0lambda.eq.0.0) goto 100

	    itrunk=itrunk+1

	    !this is the center entry 
            ientries=ientries+1
	    if (ientries.gt.nentries) then
	      write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	      write(*,*)'ientries,nentrices=',ientries,nentries 
	      stop
	    end if
	    a(ientries)=v0lambda*dz*0.5
            icol(ientries)=imva
            irow(ientries)=itrunk*nmva+imva

	    !the right hand side
            b(itrunk*nmva+imva)=v0lambda*(topcmig(ixrt,iyrt)-topcmean)

 100	    continue

	    if (v1lambda.eq.0.0) goto 999

	    itrunk=itrunk+1

	    !this is the center entry 
            ientries=ientries+1
	    if (ientries.gt.nentries) then
	      write(*,*)'ientries > nentrices at ixht,iyht=',ixht,iyht 
	      write(*,*)'ientries,nentrices=',ientries,nentries 
	      stop
	    end if
	    a(ientries)=v1lambda*dz*0.5
            icol(ientries)=imva
            irow(ientries)=itrunk*nmva+imva

            b(itrunk*nmva+imva)=v1lambda*(basecmean-topcmig(ixrt,iyrt))

 999        continue

	    if (ixrt.eq.ixdisp .and. iyrt.eq.iydisp) then 
	      write(*,*)'c0der2=',c0der2
	      write(*,*)'b(',imva,')=',b(imva)
	      write(*,*)'b(',nmva+imva,')=',b(nmva+imva)
	      write(*,*)'dvx(ixrt,iyrt),dvy(ixrt,iyrt)=',
     *          dvx(ixrt,iyrt),dvy(ixrt,iyrt)
	      write(*,*)'itrunk=',itrunk
	    end if

	  end do
	end do

        print *,'a and b are made.' 
        print *,'total entry=',ientries
	print *,'nonzero entries = ',100.0*ientries/nmva/nmva,'%'
      end  


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                       c
c       real function cgr_vzest()                                       c
c       Author: Zhaobo Meng                                             c
c       CWP/CSM                                                         c
c       Nov. 1998                                                       c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      Conjugate Gradients Methos with Regularization 
c
       real function cgr_vzest(a,icol,irow,x,b,
     *   nmva,ientries,niterations,
     *   y,q,s,p,r,xupmax,xdnmax,
     *   xbak,neqns)
      
         implicit none
 
         integer ientries           !actual number of entries in matrix a
         real a(ientries)           !Jac matrix in sparse form
         integer icol(ientries)     !entry column indice
         integer irow(ientries)     !entry row indice
         real x(1)                  !perturbation of slowness
         real b(1)                  !right hand side
         integer neqns              !number of eqns
         integer nmva               !number of cells
         integer niterations        !maximum iterations allowed
         real xupmax(1)             !maximum up-perturbation allowed
         real xdnmax(1)             !maximum dn-perturbation allowed 
         real xbak(1)               !a copy of x
 
         real y(neqns)              !working global parms 
         real q(neqns)
         real s(neqns)
         real p(nmva)
         real r(nmva)
                                    !local parms
         real sigma2
         integer i,j
         real rnormold
         real rnormnew
         real rnorm2                !l-2 norm of error
         integer k                  !iteration count  
         real alpha                 !1-D optimization factor 
         real error                 !error in standard deviation
         real rmean                 !error in mean
         real work
         real beta                  !1-D optimization factor
         real cgr_vzestnew            !current cgr_vzest value

         real xmin                  !always = -xmax
         real eps                   !small number as a threshold
         parameter (eps=0.4e-30)
         real ErrorLevel            !small number as a threshold
         parameter (ErrorLevel=1.0001)

	 integer ixdisp,iydisp,imvadisp

	 ixdisp=8
         iydisp=31
	 imvadisp=ixdisp+(iydisp-1)*61

         print *,'print parameter in cgr_vzest'
         print *,'nmva=',nmva
         print *,'neqns=',neqns
         print *,'ientries=',ientries
         print *,'niterations=',niterations
         
         sigma2=0
         do j=1,neqns
           sigma2=sigma2+b(j)*b(j)
         enddo
         sigma2=sigma2/neqns        !the mean initial misfit level 

         do i=1,nmva
           x(i)=0.0                 !initial guess of the perturbation
         enddo

         call ax(a,irow,icol,x,y,neqns,ientries)

         do i=1,neqns
           s(i)=b(i)-y(i)     !residual
         enddo

         call ax(a,icol,irow,s,p,nmva,ientries)
    
         do i=1,nmva
           r(i)=p(i)
         enddo
         rnormold=rnorm2(r,nmva)

         do k=1,niterations
           call ax(a,irow,icol,p,q,neqns,ientries)
           alpha=rnormold/rnorm2(q,neqns)
           do i=1,nmva
             xbak(i)=x(i)
             x(i)=x(i)+alpha*p(i)
             x(i)=max(xdnmax(i),x(i)) !truncated if too positively large
             x(i)=min(xupmax(i),x(i)) !truncated if too negatively large
           enddo

           call ax(a,irow,icol,x,y,neqns,ientries)

           error=0.0
           rmean=0.0
           do i=1,neqns
             work=y(i)-b(i)
             rmean=rmean+work
             error=error+work*work
           enddo

           error=error/neqns
           cgr_vzestnew=error
           print *,'cgr_vzest=',error
           error=error/sigma2
           error=sqrt(error)
           rmean=rmean/neqns
           sigma2=0.0
           
           do i=1,neqns
             work=y(i)-b(i)-rmean
             sigma2=sigma2+work*work
           enddo
 
           sigma2=sigma2/(neqns)
           if (error.ge.ErrorLevel) goto 100

           do i=1,neqns           !update
             s(i)=s(i)-alpha*q(i)
           enddo
           call ax(a,icol,irow,s,r,nmva,ientries)
           rnormnew=rnorm2(r,nmva)
           beta=rnormnew/rnormold
           do i=1,nmva
             p(i)=r(i)+beta*p(i)
           enddo
           rnormold=rnormnew

           cgr_vzest=cgr_vzestnew
 
         enddo
 100     continue
         do i=1,nmva
           x(i)=xbak(i)               !the last one is what we want
         enddo

	 write(*,*)'x(',imvadisp,')=',x(imvadisp)

         !Now, let's update misfit dt:=dt-L*ds:
         !
         !     t=L*s          total traveltime
         ! => dt=dL*s+L*ds    total difference
         ! => dL*s=dt-L*ds    difference corresponding to base pert only
         !

         call ax(a,irow,icol,x,y,neqns,ientries) !y=L*ds
         do i=1,nmva
           b(i)=b(i)-y(i) !only those measured updated
         enddo 

       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c      subroutine ax()
c      
c      If to calculate y=a*x, please put irow in front of icol, and set
c      noutput=neqns;
c      on the contrary, if calculate x=a^T*y, please put icol in front of
c      irow, and set noutput=nmva
c
       subroutine ax(a,irow,icol,x,y,
     *   noutput,ientries)

         real a(1)                   !Jac matrix
         integer irow(1)             !row information
         integer icol(1)             !col information
         real x(1)                   !unknowns
         real y(1)                   !conjugate vector
         integer noutput
         integer ientries            !number of entries in a  
                                     !local parms
         integer i
         do i=1,noutput
           y(i)=0.0
         enddo
         do i=1,ientries
           y(irow(i))=y(irow(i))+a(i)*x(icol(i))
         enddo
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      real function rnorm2() 
c      
c      Calculate the rnorm of a vector. Needs the length of vector          
c
       real function rnorm2(xy,n)
                                  !global parms
         real xy(1)               !either x or y
         integer n                !size of the vector
                                  !local parms
         integer i
         real sum
   
         sum=0.0
         do i=1,n
           sum=sum+xy(i)*xy(i)
         enddo
         rnorm2=sum
       end
