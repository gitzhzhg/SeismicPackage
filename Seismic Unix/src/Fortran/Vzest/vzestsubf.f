      subroutine forsub(iflag, nbufmax, buf)        

      implicit none

      integer iflag
      integer nbufmax
      real buf(1)

      integer npicks
      save npicks

      integer nxrt,nyrt
      save nxrt,nyrt

      real vlambda
      real v0lambda
      real v1lambda
      real vzlambda
      real zlambda
      save vlambda,v0lambda,v1lambda,vzlambda,zlambda

      integer niterations
      save  niterations

      real topcmean
      real basecmean
      save topcmean,basecmean

      real xupmaxconst
      real xdnmaxconst
      save xupmaxconst,xdnmaxconst

      real dxrt,dyrt
      save dxrt,dyrt

      integer ixdisp,iydisp
      save ixdisp,iydisp

      integer iseed
      save iseed

      integer isort(2)
      save isort

      integer nsort
      real sort1
      real sort2
      save nsort,sort1,sort2 
 
      integer nxrtbeg
      integer nxrtend
      integer nyrtbeg
      integer nyrtend
      save  nxrtbeg,nxrtend,nyrtbeg,nyrtend

      real w_eps
      real azimuth
      save w_eps,azimuth

      character*120 fnames(2)
      save fnames

      character*120 topcmigfile
      save topcmigfile

      real topc
      character*120 topcgmigfile
      save topc,topcgmigfile

      real topcg
      character*120 topzgridfile
      save topcg,topzgridfile

      character*120 basecmigfile
      character*120 basezgridfile
      save basecmigfile,basezgridfile

      character*120 dvxfile
      character*120 dvyfile
      character*120 dvzfile
      save dvxfile,dvyfile,dvzfile

      character*120 c0newfile
      save c0newfile

      character*120 g0file
      character*120 g1file
      character*120 g2file
      character*120 g3file
      save g0file,g1file,g2file,g3file

      integer nxht,nyht
      integer nmva
      save nxht,nyht,nmva

      integer neqns,nentries,ngrid
      save neqns,nentries,ngrid

      integer p_topcmig    !real (nxrt,nyrt) velocity grid at the top for small v
      integer p_topcgmig   !real (nxrt,nyrt) velocity gradient at the top
      integer p_topcnew    !real (nxrt,nyrt) velocity grid at the top for small v
      integer p_topzgrid   !real (nxrt,nyrt) depth grid at the top
      integer p_basecmig   !real (nxrt,nyrt) velocity grid at the base for small v
      integer p_basezgrid  !real (nxrt,nyrt) depth grid at the base
      integer p_g          !real (npicks,nxrt,nyrt,4) derivatives with repsect to
      integer p_dvz        !real (nxrt,nyrt) derivatives with repsect to
      integer p_dvzold     !real (nxrt,nyrt) derivatives with repsect to
      integer p_dvx        !real (nxrt,nyrt) derivatives with repsect to
      integer p_dvy        !real (nxrt,nyrt) derivatives with repsect to
      integer p_dvxwork    !real (nxrt,nyrt) derivatives with repsect to
      integer p_work       !real (nxht,nyht)
      integer p_icol       !integer (nentries) column positions of nonzeros a entries
      integer p_irow       !integer (nentries) row positions of nonzeros a entries
      integer p_a          !real (nentries) nonzero a entries
      integer p_b          !real (neqns) the right hand side (ixht+(iyht-1)*nxht)
      integer p_x          !real (nmva) the unknown dvz (ixht+(iyht-1)*nxht)
      integer p_xupmax     !real (nmva) maximum up-perturbation allowed
      integer p_xdnmax     !real (nmva) maximum dn-perturbation allowed
      integer p_xbak       !real (nmva) a copy of x
      integer p_y          !real (neqns) working global parms
      integer p_q          !real (neqns)
      integer p_s          !real (neqns)
      integer p_p          !real p(nmva)
      integer p_r          !real r(nmva)
      integer p_tmisfit    !real tmisfit(npicks,nxrt,nyrt)
      integer p_zmisfit    !real zmisfit(npicks,nxrt,nyrt)

      save p_topcmig    !real (nxrt,nyrt) velocity grid at the top for small v
      save p_topcgmig   !real (nxrt,nyrt) velocity gradient at the top
      save p_topcnew    !real (nxrt,nyrt) velocity grid at the top for small v
      save p_topzgrid   !real (nxrt,nyrt) depth grid at the top
      save p_basecmig   !real (nxrt,nyrt) velocity grid at the base for small v
      save p_basezgrid  !real (nxrt,nyrt) depth grid at the base
      save p_g          !real (npicks,nxrt,nyrt,4) derivatives with repsect to
      save p_dvz        !real (nxrt,nyrt) derivatives with repsect to
      save p_dvzold     !real (nxrt,nyrt) derivatives with repsect to
      save p_dvx        !real (nxrt,nyrt) derivatives with repsect to
      save p_dvy        !real (nxrt,nyrt) derivatives with repsect to
      save p_dvxwork    !real (nxrt,nyrt) derivatives with repsect to
      save p_work       !real (nxht,nyht)
      save p_icol       !integer (nentries) column positions of nonzeros a entries
      save p_irow       !integer (nentries) row positions of nonzeros a entries
      save p_a          !real (nentries) nonzero a entries
      save p_b          !real (neqns) the right hand side (ixht+(iyht-1)*nxht)
      save p_x          !real (nmva) the unknown dvz (ixht+(iyht-1)*nxht)
      save p_xupmax     !real (nmva) maximum up-perturbation allowed
      save p_xdnmax     !real (nmva) maximum dn-perturbation allowed
      save p_xbak       !real (nmva) a copy of x
      save p_y          !real (neqns) working global parms
      save p_q          !real (neqns)
      save p_s          !real (neqns)
      save p_p          !real p(nmva)
      save p_r          !real r(nmva)
      save p_tmisfit    !real tmisfit(npicks,nxrt,nyrt)
      save p_zmisfit    !real zmisfit(npicks,nxrt,nyrt)

      integer size_topcmig    !real (nxrt,nyrt) velocity grid at the top for small v
      integer size_topcgmig   !real (nxrt,nyrt) velocity gradient at the top
      integer size_topcnew    !real (nxrt,nyrt) velocity grid at the top for small v
      integer size_topzgrid   !real (nxrt,nyrt) depth grid at the top
      integer size_basecmig   !real (nxrt,nyrt) velocity grid at the base for small v
      integer size_basezgrid  !real (nxrt,nyrt) depth grid at the base
      integer size_g          !real (npicks,nxrt,nyrt,4) derivatives with repsect to
      integer size_dvz        !real (nxrt,nyrt) derivatives with repsect to
      integer size_dvzold     !real (nxrt,nyrt) derivatives with repsect to
      integer size_dvx        !real (nxrt,nyrt) derivatives with repsect to
      integer size_dvy        !real (nxrt,nyrt) derivatives with repsect to
      integer size_dvxwork    !real (nxrt,nyrt) derivatives with repsect to
      integer size_work       !real (nxht,nyht)
      integer size_icol       !integer (nentries) column positions of nonzeros a entries
      integer size_irow       !integer (nentries) row positions of nonzeros a entries
      integer size_a          !real (nentries) nonzero a entries
      integer size_b          !real (neqns) the right hand side (ixht+(iyht-1)*nxht)
      integer size_x          !real (nmva) the unknown dvz (ixht+(iyht-1)*nxht)
      integer size_xupmax     !real (nmva) maximum up-perturbation allowed
      integer size_xdnmax     !real (nmva) maximum dn-perturbation allowed
      integer size_xbak       !real (nmva) a copy of x
      integer size_y          !real (neqns) working global parms
      integer size_q          !real (neqns)
      integer size_s          !real (neqns)
      integer size_p          !real p(nmva)
      integer size_r          !real r(nmva)
      integer size_tmisfit    !real tmisfit(npicks,nxrt,nyrt)
      integer size_zmisfit    !real zmisfit(npicks,nxrt,nyrt)

      save size_topcmig    !real (nxrt,nyrt) velocity grid at the top for small v
      save size_topcgmig   !real (nxrt,nyrt) velocity gradient at the top
      save size_topcnew    !real (nxrt,nyrt) velocity grid at the top for small v
      save size_topzgrid   !real (nxrt,nyrt) depth grid at the top
      save size_basecmig   !real (nxrt,nyrt) velocity grid at the base for small v
      save size_basezgrid  !real (nxrt,nyrt) depth grid at the base
      save size_g          !real (npicks,nxrt,nyrt,4) derivatives with repsect to
      save size_dvz        !real (nxrt,nyrt) derivatives with repsect to
      save size_dvzold     !real (nxrt,nyrt) derivatives with repsect to
      save size_dvx        !real (nxrt,nyrt) derivatives with repsect to
      save size_dvy        !real (nxrt,nyrt) derivatives with repsect to
      save size_dvxwork    !real (nxrt,nyrt) derivatives with repsect to
      save size_work       !real (nxht,nyht)
      save size_icol       !integer (nentries) column positions of nonzeros a entries
      save size_irow       !integer (nentries) row positions of nonzeros a entries
      save size_a          !real (nentries) nonzero a entries
      save size_b          !real (neqns) the right hand side (ixht+(iyht-1)*nxht)
      save size_x          !real (nmva) the unknown dvz (ixht+(iyht-1)*nxht)
      save size_xupmax     !real (nmva) maximum up-perturbation allowed
      save size_xdnmax     !real (nmva) maximum dn-perturbation allowed
      save size_xbak       !real (nmva) a copy of x
      save size_y          !real (neqns) working global parms
      save size_q          !real (neqns)
      save size_s          !real (neqns)
      save size_p          !real p(nmva)
      save size_r          !real r(nmva)
      save size_tmisfit    !real tmisfit(npicks,nxrt,nyrt)
      save size_zmisfit    !real zmisfit(npicks,nxrt,nyrt)

      if (iflag.eq.1) then        !just return the size of buf, no raytracing
      
      call getparms_pracmva(
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
     +  nsort,                   !number of offsets in vest3d
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

      !calculate the sizes of the required arrays

      neqns=5*nmva
      nentries=nmva*17
      nmva=nxht*nyht
      ngrid=nxrt*nyrt
      nxht=nxrt
      nyht=nyrt

      size_topcmig=ngrid    !real (nxrt,nyrt) velocity grid at the top for small v
      size_topcgmig=ngrid   !real (nxrt,nyrt) velocity gradient at the top
      size_topcnew=ngrid    !real (nxrt,nyrt) velocity grid at the top for small v
      size_topzgrid=ngrid   !real (nxrt,nyrt) depth grid at the top
      size_basecmig=ngrid   !real (nxrt,nyrt) velocity grid at the base for small v
      size_basezgrid=ngrid  !real (nxrt,nyrt) depth grid at the base
      size_g=npicks*ngrid*4 !real (npicks,nxrt,nyrt,4) derivatives with repsect to
      size_dvz=ngrid        !real (nxrt,nyrt) derivatives with repsect to
      size_dvzold=ngrid     !real (nxrt,nyrt) derivatives with repsect to
      size_dvx=ngrid        !real (nxrt,nyrt) derivatives with repsect to
      size_dvy=ngrid        !real (nxrt,nyrt) derivatives with repsect to
      size_dvxwork=ngrid    !real (nxrt,nyrt) derivatives with repsect to
      size_work=nmva        !real (nxht,nyht)
      size_icol=nentries    !integer (nentries) column positions of nonzeros a entries
      size_irow=nentries    !integer (nentries) row positions of nonzeros a entries
      size_a=nentries       !real (nentries) nonzero a entries
      size_b=neqns          !real (neqns) the right hand side (ixht+(iyht-1)*nxht)
      size_x=nmva           !real (nmva) the unknown dvz (ixht+(iyht-1)*nxht)
      size_xupmax=nmva      !real (nmva) maximum up-perturbation allowed
      size_xdnmax=nmva      !real (nmva) maximum dn-perturbation allowed
      size_xbak=nmva        !real (nmva) a copy of x
      size_y=neqns          !real (neqns) working global parms
      size_q=neqns          !real (neqns)
      size_s=neqns          !real (neqns)
      size_p=neqns          !real p(nmva)
      size_r=nmva           !real r(nmva)
      size_tmisfit=npicks*ngrid    !real tmisfit(npicks,nxrt,nyrt)
      size_zmisfit=npicks*ngrid    !real zmisfit(npicks,nxrt,nyrt)


      p_topcmig=1
      p_topcgmig   	= p_topcmig  	+ size_topcmig
      p_topcnew    	= p_topcgmig 	+ size_topcgmig
      p_topzgrid   	= p_topcnew  	+ size_topcnew
      p_basecmig	= p_topzgrid	+ size_topzgrid
      p_basezgrid	= p_basecmig	+ size_basecmig
      p_g		= p_basezgrid	+ size_basezgrid
      p_dvz		= p_g		+ size_g
      p_dvzold		= p_dvz		+ size_dvz
      p_dvx		= p_dvzold	+ size_dvzold
      p_dvy		= p_dvx		+ size_dvx
      p_dvxwork		= p_dvy		+ size_dvy
      p_work		= p_dvxwork	+ size_dvxwork
      p_icol		= p_work 	+ size_work
      p_irow		= p_icol	+ size_icol
      p_a		= p_irow	+ size_irow
      p_b		= p_a		+ size_a
      p_x		= p_b 		+ size_b
      p_xupmax		= p_x 		+ size_x
      p_xdnmax		= p_xupmax	+ size_xupmax
      p_xbak		= p_xdnmax	+ size_xdnmax
      p_y		= p_xbak	+ size_xbak
      p_q		= p_y 		+ size_y
      p_s		= p_q 		+ size_q
      p_p		= p_s 		+ size_s 
      p_r		= p_p		+ size_p
      p_tmisfit		= p_r 		+ size_r
      p_zmisfit		= p_tmisfit	+ size_tmisfit

      write(*,*)
      write(*,*)'p_basecmig	=',p_basecmig
      write(*,*)'p_topcmig	=',p_topcmig
      write(*,*)'p_topcgmig	=',p_topcgmig
      write(*,*)'p_topcnew	=',p_topcnew
      write(*,*)'p_topzgrid	=',p_topzgrid
      write(*,*)'p_basecmig	=',p_basecmig
      write(*,*)'p_basezgrid	=',p_basezgrid
      write(*,*)'p_g		=',p_g
      write(*,*)'p_dvz		=',p_dvz
      write(*,*)'p_dvzold	=',p_dvzold
      write(*,*)'p_dvx		=',p_dvx
      write(*,*)'p_dvy		=',p_dvy
      write(*,*)'p_dvxwork	=',p_dvxwork
      write(*,*)'p_work		=',p_work
      write(*,*)'p_icol		=',p_icol
      write(*,*)'p_irow		=',p_irow
      write(*,*)'p_a		=',p_a
      write(*,*)'p_b		=',p_b
      write(*,*)'p_x		=',p_x
      write(*,*)'p_xupmax	=',p_xupmax
      write(*,*)'p_xdnmax	=',p_xdnmax
      write(*,*)'p_xbak		=',p_xbak
      write(*,*)'p_y		=',p_y
      write(*,*)'p_q		=',p_q
      write(*,*)'p_s		=',p_s
      write(*,*)'p_p		=',p_p
      write(*,*)'p_r		=',p_r
      write(*,*)'p_tmisfit	=',p_tmisfit
      write(*,*)'p_zmisfit	=',p_zmisfit

      nbufmax=size_topcmig
     +  +  size_topcgmig
     +  +  size_topcnew
     +  +  size_topzgrid
     +  +  size_basecmig
     +  +  size_basezgrid
     +  +  size_g
     +  +  size_dvz
     +  +  size_dvzold
     +  +  size_dvx
     +  +  size_dvy
     +  +  size_dvxwork
     +  +  size_work
     +  +  size_icol
     +  +  size_irow
     +  +  size_a
     +  +  size_b
     +  +  size_x
     +  +  size_xupmax
     +  +  size_xdnmax
     +  +  size_xbak
     +  +  size_y
     +  +  size_q
     +  +  size_s
     +  +  size_p
     +  +  size_r
     +  +  size_tmisfit
     +  +  size_zmisfit

      return
      end if
 
      !second pass of subroutine.  memory available, now do it!

      call vzest(
     +  neqns,                   !integer neqns=5*nmva
     +  nentries,                !integer nentries=nmva*17
     +  npicks,                  !integer npicks=2
     +  nmva,                    !integer nmva=nxht*nyht
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
     +  nsort,                   !number of offsets in vest3d
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
     +  buf(p_topcmig),    !real (nxrt,nyrt) velocity grid at the top for small v
     +  topcg,             !real topcg=0 then read in from file
     +  buf(p_topcgmig),   !real (nxrt,nyrt) velocity gradient at the top
     +  buf(p_topcnew),    !real (nxrt,nyrt) velocity grid at the top for small v
     +  buf(p_topzgrid),   !real (nxrt,nyrt) depth grid at the top
     +  buf(p_basecmig),   !real (nxrt,nyrt) velocity grid at the base for small v
     +  buf(p_basezgrid),  !real (nxrt,nyrt) depth grid at the base
     +  buf(p_g),          !real (npicks,nxrt,nyrt,4) derivatives with repsect to
     +  buf(p_dvz),        !real (nxrt,nyrt) derivatives with repsect to
     +  buf(p_dvzold),     !real (nxrt,nyrt) derivatives with repsect to
     +  buf(p_dvx),        !real (nxrt,nyrt) derivatives with repsect to
     +  buf(p_dvy),        !real (nxrt,nyrt) derivatives with repsect to
     +  buf(p_dvxwork),    !real (nxrt,nyrt) derivatives with repsect to
     +  buf(p_work),       !real (nxht,nyht)
     +  buf(p_icol),       !integer (nentries) column positions of nonzeros a entries
     +  buf(p_irow),       !integer (nentries) row positions of nonzeros a entries
     +  buf(p_a),          !real (nentries) nonzero a entries
     +  buf(p_b),          !real (neqns) the right hand side (ixht+(iyht-1)*nxht)
     +  buf(p_x),          !real (nmva) the unknown dvz (ixht+(iyht-1)*nxht)
     +  buf(p_xupmax),     !real (nmva) maximum up-perturbation allowed
     +  buf(p_xdnmax),     !real (nmva) maximum dn-perturbation allowed
     +  buf(p_xbak),       !real (nmva) a copy of x
     +  buf(p_y),          !real (neqns) working global parms
     +  buf(p_q),          !real (neqns)
     +  buf(p_s),          !real (neqns)
     +  buf(p_p),          !real p(nmva)
     +  buf(p_r),          !real r(nmva)
     +  buf(p_tmisfit),    !real tmisfit(npicks,nxrt,nyrt)
     +  buf(p_zmisfit))    !real zmisfit(npicks,nxrt,nyrt)
 
      end
