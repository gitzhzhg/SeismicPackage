cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c program vplusz.f   
c Zhaobo Meng / July 1998
c CWP/Colorado School of Mines                    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c program vplusz
c
c This program is design for postmigration velocity analysis.  It is
c bsed on Zhaobo Meng's thesis, chapter 5.
c
c "Tetrahedral based earth models, ray tracing in tetrahedral models,
c and analytical migration velocity analysis"--Zhaobo Meng,
c Ph. D. thesis, Center for Wave Phenomena, Department of
c Mathematical and Computer Sciences,
c Colorado School of Mines, 1999.
c
c (1) vplusz converts the traveltime misfits (offset??.picks)
c to depth misfits that PSDM generates; then multiply the results
c using the velocity at the velocity at the base grid.  The final results
c are added to the base grid to get the final depth grid.  Thus, now,
c different offsets correspond to different depth grids.
c
c For vplusz, not all CIGs are used (because there may not enough data.
c Only those CIGs from nxrtbeg to nxrtend and from nyrtbeg to nyrtend are
c used.  Similarly, not all offsets are used, user can specify how many 
c offsets to use and what offsets to use.  They are specified by 
c isort(1), isort(2),..., isort(npicks).
c
c To run the program, the best way is to execuate the deck file in the 
c project directory.  Hopefully the vplusz will take you to the correct 
c depth at the correct velocity. So now, BUCKLE UP and enjoy the ride!

       subroutine vplusz(
     +  alphamin,
     +  alphamax,
     +  topc0,
     +  topc1,
     +  topcg0,
     +  topcg1,
     +  nxrt,                    !number of samples in x
     +  nyrt,                    !number of samples in y
     +  nxrtbeg,nxrtend,         !CIGs to be solved
     +  nyrtbeg,nyrtend,         !CIGs to be solved
     +  ixdisp,                  !ix for display
     +  iydisp,                  !iy for display
     +  iseed0,                  !seed number
     +  iseed1,                  !seed number
     +  nsort,                   !number of offsets in vest3d
     +  sort1,
     +  sort2,
     +  npicks,
     +  isort,                   !isort(2)
     +  fnames0,                 !misfit file name
     +  fnames1,                 !misfit file name
     +  topz0gridfile,           !top z
     +  topz1gridfile,           !top z
     +  basez0gridfile,          !base z
     +  basez1gridfile,          !base z
     +  topc0migfile,            !top c0
     +  topc1migfile,            !top c1
     +  topcg0migfile,           !top cg0
     +  topcg1migfile,           !top cg1
     +  basec0migfile,           !base c0
     +  basec1migfile,           !base c1
     +  cupdatefile,             !update c
     +  ctrc0file,               !center c0
     +  zupdatefile,             !update z
     +  topc0mig,                !(nxrt,nyrt) depth grid at the base
     +  topc1mig,                !(nxrt,nyrt) depth grid at the base
     +  ctrc0mig,                !(nxrt,nyrt) depth grid at the base
     +  topcg0mig,               !(nxrt,nyrt) depth grid at the base
     +  topcg1mig,               !(nxrt,nyrt) depth grid at the base
     +  basec0mig,               !(nxrt,nyrt) depth grid at the base
     +  basec1mig,               !(nxrt,nyrt) depth grid at the base
     +  basez0grid,              !(nxrt,nyrt) depth grid at the base
     +  basez1grid,              !(nxrt,nyrt) depth grid at the base
     +  topz0grid,               !(nxrt,nyrt) depth grid at the top
     +  topz1grid,               !(nxrt,nyrt) depth grid at the top
     +  cupdate,                 !(nxrt,nyrt) !updated c grid at the top
     +  zupdate,                 !(nxrt,nyrt) !updated depth grid at the base
     +  tmisfit0,                !(npicks,nxrt,nyrt)
     +  tmisfit1,                !(npicks,nxrt,nyrt)
     +	alpha,                   !(nxrt,nyrt)
     +	alpha_work,              !(nxrt,nyrt)
     +	work_read,               !(nxrt*nyrt)
     +  zmisfit0,                !(npicks,nxrt,nyrt)
     +  zmisfit1)                !(npicks,nxrt,nyrt)

        implicit none

c The following parameters are used by vest3d, just copy them
	integer nsort
	real sort1
	real sort2

c nxrt is the number of reconstructed imaged points in x direction
c nyrt is the number of reconstructed imaged points in y direction
        integer nxrt,nyrt
        integer ixdisp,iydisp

c output files
        character*120 zupdatefile

        integer nxrtbeg
        integer nxrtend
        integer nyrtbeg
        integer nyrtend

        integer npicks

        real basec0mig(nxrt,nyrt)     !velocity grid at the base for small v
        real basec1mig(nxrt,nyrt)     !velocity grid at the base for big v
        real topc0	              !=0 then read the following
        real topc0mig(nxrt,nyrt)      !velocity grid at the top for small v
        real topcg0	              !=0 then read the following
        real topcg0mig(nxrt,nyrt)     !velocity grid at the top for small v
        real topcg1	              !=0 then read the following
        real topcg1mig(nxrt,nyrt)     !velocity grid at the top for small v
        real ctrc0mig(nxrt,nyrt)      !velocity grid at the top for small v
        real topc1	              !=0 then read the following
        real topc1mig(nxrt,nyrt)      !velocity grid at the top for big v
        real cupdate(nxrt,nyrt)	      !updated velocity grid at the top
        real topz0grid(nxrt,nyrt)     !depth grid at the top
        real topz1grid(nxrt,nyrt)     !depth grid at the top
        real basez0grid(nxrt,nyrt)    !depth grid at the base
        real basez1grid(nxrt,nyrt)    !depth grid at the base
        real zupdate(nxrt,nyrt)       !updated depth grid at the base
        real tmisfit0(npicks,nxrt,nyrt)
        real tmisfit1(npicks,nxrt,nyrt)
        real zmisfit0(npicks,nxrt,nyrt)
        real zmisfit1(npicks,nxrt,nyrt)
	real alpha(nxrt,nyrt)
	real alpha_work(nxrt,nyrt)
	real work_read(nxrt*nyrt)

        real dzmisfit0
        real dzmisfit1,A,B

c work variables 
        integer ix,iy,ipick

c the HALF offsets and half offset increment
        integer npicksmax
        parameter (npicksmax=100)
	real h(npicksmax),sortinc
        real seed0(npicksmax)
        real seed1(npicksmax)	
        real alpha0

	integer ixrt,iyrt,iunit

        integer iseed0
        integer iseed1

c isort defines which offsets one has chosen to apply vplusz
        integer isort(npicks)

        integer nsortmax
        parameter (nsortmax=2)
c the filenames for the misfit files.  Fill in these
        character*120 fnames0(nsortmax) 
        character*120 fnames1(nsortmax) 

        character*120 topc0migfile
        character*120 topc1migfile

c the velocity gradient file at the imaging grid.
        character*120 topcg0migfile
        character*120 topcg1migfile

        character*120 topz0gridfile
        character*120 topz1gridfile

        character*120 basec0migfile
        character*120 basec1migfile

        character*120 basez0gridfile
        character*120 basez1gridfile

c output files
        character*120 cupdatefile
        character*120 ctrc0file
        real alphamin,alphamax

	iunit=99
	write(*,*)'Now, go!'

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	do ixrt=1,nxrt
	  do iyrt=1,nyrt
	    zupdate(ixrt,iyrt)=0.0
	    cupdate(ixrt,iyrt)=0.0
	  end do
	end do

        do ipick=1,npicks
          seed0(ipick)=(isort(ipick)+0.5)*iseed0
          seed1(ipick)=(isort(ipick)+0.5)*iseed1
          print *,'seed0(',ipick,')=',seed0(ipick)
          print *,'seed1(',ipick,')=',seed1(ipick)
        enddo

	call readgrids_vplusz(
     +    topc0,
     +    topcg0,
     +    npicks,         !number of picks
     +    nxrt,nyrt,      !project size
     +    basec0mig,      !velocity grid at the base
     +    topc0mig,       !velocity grid at the top
     +    topcg0mig,      !velocity gradient grid at the top
     +    topz0grid,      !depth grid at the base
     +    basez0grid,     !depth grid at the base
     +    work_read,
     +    tmisfit0,
     +    topc0migfile,
     +    topcg0migfile,
     +    basec0migfile,
     +    topz0gridfile,
     +    basez0gridfile,
     +    fnames0)

        call readgrids_vplusz(
     +    topc1,
     +    topcg1,
     +    npicks,         !number of picks
     +    nxrt,nyrt,      !project size
     +    basec1mig,      !velocity grid at the base
     +    topc1mig,       !velocity grid at the top
     +    topcg1mig,      !velocity grid at the top
     +    topz1grid,      !depth grid at the base
     +    basez1grid,     !depth grid at the base
     +    work_read,
     +    tmisfit1,
     +    topc1migfile,
     +    topcg1migfile,
     +    basec1migfile,
     +    topz1gridfile,
     +    basez1gridfile,
     +    fnames1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c calculate the half offsets.  The definitions should be consistent to
c the HPSDM algorithms

        sortinc=(sort2-sort1)/nsort
        write(*,*)
        write(*,*)'nsort=',nsort
        ! The following formula is derived from the HPSDM code
        do ipick=1,npicks
          h(ipick)=0.5*(sort1+(isort(ipick)-0.501)*sortinc)
	  if (h(ipick).lt.0.0) h(ipick)=0.0
          write(*,*)'isort, h=',isort(ipick),h(ipick)
        end do

	if (isort(1).eq.0) h(1)=0.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c converting the traveltime misfits to depth misfits zmisfit(ipick,ix,iy)

        if (npicks.lt.2) then
	  write(*,*)'npicks must >= 2 for this algorithm'	
	  stop
	end if

        write(*,*)'alphamin,alphamax=',alphamin,alphamax

c       This is only one version of the algorithm as described in Zhaobo's
c       thesis      

        do ix=nxrtbeg,nxrtend
          do iy=nyrtbeg,nyrtend
            do ipick=1,npicks
              tmisfit0(ipick,ix,iy)=tmisfit0(ipick,ix,iy)-seed0(ipick)
              tmisfit1(ipick,ix,iy)=tmisfit1(ipick,ix,iy)-seed1(ipick)

	      zmisfit0(ipick,ix,iy)=tmisfit0(ipick,ix,iy)*0.001
     *          *basec0mig(ix,iy)/2.0
	      zmisfit1(ipick,ix,iy)=tmisfit1(ipick,ix,iy)*0.001
     *          *basec1mig(ix,iy)/2.0
            enddo

	    dzmisfit0=zmisfit0(2,ix,iy)-zmisfit0(1,ix,iy)
 	    dzmisfit1=zmisfit1(2,ix,iy)-zmisfit1(1,ix,iy)

	    ipick=2

	    if (dzmisfit1-dzmisfit0.eq.0) then
              alpha(ix,iy)=0.5
            else
 	      alpha(ix,iy)=dzmisfit1/(dzmisfit1-dzmisfit0)
            end if
	    if (alpha(ix,iy).lt.alphamin) alpha(ix,iy)=alphamin
	    if (alpha(ix,iy).gt.alphamax) alpha(ix,iy)=alphamax

	    if (ix.eq.ixdisp.and.iy.eq.iydisp) then
	      write(*,*)
	      write(*,*)'Check one point...'
	      write(*,*)'at ix, iy=',ixdisp,iydisp
	      write(*,*)'A,B=',A,B
              write(*,*)'alphamin,alphamax=',alphamin,alphamax
	      write(*,*)'basez0grid =',basez0grid(ix,iy)
      	      write(*,*)'tmisfit0 1 (in ms)=',tmisfit0(ipick-1,ix,iy)
	      write(*,*)'zmisfit0 1 (in m)=',zmisfit0(ipick-1,ix,iy)
      	      write(*,*)'tmisfit0 2 (in ms)=',tmisfit0(ipick,ix,iy)
	      write(*,*)'zmisfit0 2 (in m)=',zmisfit0(ipick,ix,iy)
	      write(*,*)'dzmisfit0 (in m)=',dzmisfit0
	      write(*,*)'basez1grid =',basez1grid(ix,iy)
      	      write(*,*)'tmisfit1 (in ms)=',tmisfit1(ipick,ix,iy)
	      write(*,*)'zmisfit1 (in m)=',zmisfit1(ipick,ix,iy)
	      write(*,*)'dzmisfit1 (in m)=',dzmisfit1
	      write(*,*)'topc0mig,topc1mig=',topc0mig(ix,iy),topc1mig(ix,iy)
	    end if

          enddo
        enddo

        do ix=nxrtbeg,nxrtend
          do iy=nyrtbeg,nyrtend
	    alpha0=1.0-alpha(ix,iy)
 	    cupdate(ix,iy)=topc0mig(ix,iy)*alpha(ix,iy)+topc1mig(ix,iy)*alpha0
 	    zupdate(ix,iy)=alpha(ix,iy)*basez0grid(ix,iy)+alpha0*basez1grid(ix,iy)
          enddo
        enddo

	write(*,*)'alpha(ixdisp,iydisp), cupdate=',
     *    alpha(ixdisp,iydisp),cupdate(ixdisp,iydisp)
	write(*,*)'zupdate=',zupdate(ixdisp,iydisp)
	write(*,*)'cupdate=',cupdate(ixdisp,iydisp)

	iunit=99

        call writegrid(iunit,cupdatefile,cupdate,nxrt,nyrt)
        write(*,*)
        write(*,*)'grid file has been written to',cupdatefile

        do ix=1,nxrt
	  do iy=1,nyrt
	    ctrc0mig(ix,iy)=cupdate(ix,iy)
     *        +0.5*(zupdate(ix,iy)-topz0grid(ix,iy))*
     *        topcg0mig(ix,iy)
	  end do
	end do

        call writegrid(iunit,ctrc0file,ctrc0mig,nxrt,nyrt)
        write(*,*)
        write(*,*)'grid file has been written to',ctrc0file


        call writegrid(iunit,zupdatefile,zupdate,nxrt,nyrt)
        write(*,*)
        write(*,*)'grid file has been written to',zupdatefile

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

      subroutine forsub(
     +  iflag,                !flag for data allocation
     +  nbufmax,              !memory amount in int
     +  buf)                  !pointer

      implicit none

      integer iflag
      integer nbufmax
      real buf(1)

      save

      real topc0,topc1,topcg0,topcg1
c      save topc0,topc1,topcg0,topcg1

      real alphamin,alphamax
c      save alphamin,alphamax

      integer nsortmax
      parameter (nsortmax=2)
      character*120 fnames0(nsortmax)  !the path file
      character*120 fnames1(nsortmax)  !the path file
      character*120 topc0migfile       !the path file
      character*120 topc1migfile       !the path file
      character*120 topcg0migfile      !the path file
      character*120 topcg1migfile      !the path file
      character*120 basec0migfile      !the path file
      character*120 basec1migfile      !the path file
      character*120 topz0gridfile      !the path file
      character*120 topz1gridfile      !the path file
      character*120 basez0gridfile     !the path file
      character*120 basez1gridfile     !the path file
      character*120 ctrc0file          !the path file
      character*120 cupdatefile        !the path file
      character*120 zupdatefile        !the path file

c      save fnames0
c      save fnames1
c      save topc0migfile
c      save topc1migfile
c      save topcg0migfile
c      save topcg1migfile
c      save basec0migfile
c      save basec1migfile
c      save topz0gridfile
c      save topz1gridfile
c      save basez0gridfile
c      save basez1gridfile
c      save ctrc0file
c      save cupdatefile
c      save zupdatefile

      integer nxrt               !number of CIGs in x
      integer nyrt               !number of CIGs in y
      integer nxrtbeg,nxrtend    !CIGs to be solved
      integer nyrtbeg,nyrtend    !CIGs to be solved
      integer ixdisp,iydisp
      integer iseed0
      integer iseed1
      integer nsort
      real sort1,sort2
      integer npicks
      integer isort(nsortmax)

c      save nxrt
c      save nyrt
c      save nxrtbeg,nxrtend
c      save nyrtbeg,nyrtend
c      save ixdisp,iydisp
c      save iseed0,iseed1 
c      save nsort 
c      save sort1,sort2
c      save npicks
c      save isort

      integer nmva
c      save nmva

      integer size_topc0mig
      integer size_topc1mig
      integer size_topcg0mig
      integer size_topcg1mig
      integer size_basec0mig
      integer size_basec1mig
      integer size_topz0grid
      integer size_topz1grid
      integer size_basez0grid
      integer size_basez1grid
      integer size_cupdate
      integer size_ctrc0mig
      integer size_zupdate
      integer size_tmisfit0
      integer size_tmisfit1
      integer size_work_read
      integer size_zmisfit0
      integer size_zmisfit1
      integer size_alpha
      integer size_alpha_work

c      save size_topc0mig
c      save size_topc1mig
c      save size_topcg0mig
c      save size_topcg1mig
c      save size_basec0mig
c      save size_basec1mig
c      save size_topz0grid
c      save size_topz1grid
c      save size_basez0grid
c      save size_basez1grid
c      save size_cupdate
c      save size_ctrc0mig
c      save size_zupdate
c      save size_tmisfit0
c      save size_tmisfit1
c      save size_work_read
c      save size_zmisfit0
c      save size_zmisfit1
c      save size_alpha
c      save size_alpha_work

      integer p_topc0mig
      integer p_topc1mig
      integer p_topcg0mig
      integer p_topcg1mig
      integer p_basec0mig
      integer p_basec1mig
      integer p_topz0grid 
      integer p_topz1grid 
      integer p_basez0grid
      integer p_basez1grid
      integer p_ctrc0mig
      integer p_zupdate
      integer p_cupdate
      integer p_tmisfit0
      integer p_tmisfit1
      integer p_work_read
      integer p_zmisfit0
      integer p_zmisfit1
      integer p_alpha
      integer p_alpha_work

c      save p_topc0mig
c      save p_topc1mig
c      save p_topcg0mig
c      save p_topcg1mig
c      save p_basec0mig
c      save p_basec1mig
c      save p_topz0grid 
c      save p_topz1grid 
c      save p_basez0grid
c      save p_basez1grid
c      save p_ctrc0mig
c      save p_zupdate
c      save p_cupdate
c      save p_tmisfit0
c      save p_tmisfit1
c      save p_work_read
c      save p_zmisfit0
c      save p_zmisfit1
c      save p_alpha
c      save p_alpha_work

      if (iflag.eq.1) then        !just return the size of buf, no raytracing
         
      call getparms_vplusz(
     +  alphamin,
     +  alphamax,
     +  topc0,
     +  topc1,
     +  topcg0,
     +  topcg1,
     +  nxrt,                    !number of samples in x
     +  nyrt,                    !number of samples in y
     +  nxrtbeg,nxrtend,         !CIGs to be solved
     +  nyrtbeg,nyrtend,         !CIGs to be solved
     +  ixdisp,                  !ix for display
     +  iydisp,                  !iy for display
     +  iseed0,                  !seed number
     +  iseed1,                  !seed number
     +  nsort,                   !number of offsets in vest3d
     +  sort1,
     +  sort2,
     +  npicks,                  !number of offsets picked
     +  isort,
     +  fnames0,                 !misfit file name
     +  fnames1,                 !misfit file name
     +  topc0migfile,            !base c
     +  topc1migfile,            !base c
     +  topcg0migfile,           !base c
     +  topcg1migfile,           !base c
     +  basec0migfile,           !base c
     +  basec1migfile,           !base c
     +  topz0gridfile,           !top z
     +  topz1gridfile,           !top z
     +  basez0gridfile,          !base z
     +  basez1gridfile,          !base z
     +  cupdatefile,             !update c0
     +  ctrc0file,               !center c0
     +  zupdatefile)             !update z

      !calculate the sizes of the required arrays
      nmva=nxrt*nyrt
      size_topc0mig     = nmva 
      size_topc1mig     = nmva 
      size_topcg0mig    = nmva 
      size_topcg1mig    = nmva 
      size_basec0mig    = nmva 
      size_basec1mig    = nmva 
      size_basez0grid   = nmva 
      size_basez1grid   = nmva 
      size_topz0grid    = nmva 
      size_topz1grid    = nmva 
      size_cupdate      = nmva 
      size_ctrc0mig     = nmva 
      size_zupdate      = nmva 
      size_tmisfit0     = npicks*nmva 
      size_tmisfit1     = npicks*nmva 
      size_work_read    = nmva 
      size_zmisfit0     = npicks*nmva 
      size_zmisfit1     = npicks*nmva 
      size_alpha        = nmva
      size_alpha_work   = nmva

      p_topc0mig        = 1
      p_topc1mig        = p_topc0mig   + size_topc0mig 
      p_basec0mig       = p_topc1mig   + size_topc1mig
      p_basec1mig       = p_basec0mig  + size_basec0mig 
      p_topcg0mig       = p_basec1mig  + size_basec1mig 
      p_topcg1mig       = p_topcg0mig  + size_topcg0mig 
      p_basez0grid      = p_topcg1mig  + size_topcg1mig
      p_basez1grid      = p_basez0grid + size_basez0grid
      p_topz0grid       = p_basez1grid + size_basez1grid
      p_topz1grid       = p_topz0grid  + size_topz0grid
      p_cupdate         = p_topz1grid  + size_topz1grid
      p_ctrc0mig        = p_cupdate    + size_cupdate
      p_zupdate         = p_ctrc0mig   + size_ctrc0mig
      p_tmisfit0        = p_zupdate    + size_zupdate
      p_tmisfit1        = p_tmisfit0   + size_tmisfit0
      p_work_read       = p_tmisfit1   + size_tmisfit1
      p_zmisfit0        = p_work_read  + size_work_read
      p_zmisfit1        = p_zmisfit0   + size_zmisfit0
      p_alpha           = p_zmisfit1   + size_zmisfit1
      p_alpha_work      = p_alpha      + size_alpha

      nbufmax = size_topc0mig   +
     +          size_topc1mig   +
     +          size_topcg0mig  +
     +          size_topcg1mig  +
     +          size_basec0mig  +
     +          size_basec1mig  +
     +          size_topz0grid  +
     +          size_topz1grid  +
     +          size_basez0grid +
     +          size_basez1grid +
     +          size_cupdate    +
     +          size_ctrc0mig   +
     +          size_zupdate    +
     +          size_tmisfit0   +
     +          size_tmisfit1   +
     +          size_work_read  +
     +          size_zmisfit0   +
     +          size_zmisfit1   +
     +          size_alpha      +
     +          size_alpha_work 

      write(*,*)'nbufmax=',nbufmax
      return
      end if
 
      !second pass of subroutine.  memory available, now do it!

      call vplusz(
     +  alphamin,
     +  alphamax,
     +  topc0,
     +  topc1,
     +  topcg0,
     +  topcg1,
     +  nxrt,                    !number of samples in x
     +  nyrt,                    !number of samples in y
     +  nxrtbeg,nxrtend,         !CIGs to be solved
     +  nyrtbeg,nyrtend,         !CIGs to be solved
     +  ixdisp,                  !ix for display
     +  iydisp,                  !iy for display
     +  iseed0,                  !seed number
     +  iseed1,                  !seed number
     +  nsort,                   !number of offsets in vest3d
     +  sort1,
     +  sort2,
     +  npicks,
     +  isort,                   !isort(2)
     +  fnames0,                 !misfit file name
     +  fnames1,                 !misfit file name
     +  topz0gridfile,           !top z
     +  topz1gridfile,           !top z
     +  basez0gridfile,          !base z
     +  basez1gridfile,          !base z
     +  topc0migfile,            !top c0
     +  topc1migfile,            !top c1
     +  topcg0migfile,           !top cg0
     +  topcg1migfile,           !top cg1
     +  basec0migfile,           !base c0
     +  basec1migfile,           !base c1
     +  cupdatefile,             !update z
     +  ctrc0file,               !center c0
     +  zupdatefile,             !update z
     +  buf(p_topc0mig),         !(nxrt,nyrt) depth grid at the base
     +  buf(p_topc1mig),         !(nxrt,nyrt) depth grid at the base
     +  buf(p_ctrc0mig),         !(nxrt,nyrt) depth grid at the base
     +  buf(p_topcg0mig),        !(nxrt,nyrt) depth grid at the base
     +  buf(p_topcg1mig),        !(nxrt,nyrt) depth grid at the base
     +  buf(p_basec0mig),        !(nxrt,nyrt) depth grid at the base
     +  buf(p_basec1mig),        !(nxrt,nyrt) depth grid at the base
     +  buf(p_basez0grid),       !(nxrt,nyrt) depth grid at the base
     +  buf(p_basez1grid),       !(nxrt,nyrt) depth grid at the base
     +  buf(p_topz0grid),        !(nxrt,nyrt) depth grid at the top
     +  buf(p_topz1grid),        !(nxrt,nyrt) depth grid at the top
     +  buf(p_cupdate),          !(nxrt,nyrt) !updated c grid at the top
     +  buf(p_zupdate),          !(nxrt,nyrt) !updated depth grid at the base
     +  buf(p_tmisfit0),         !(npicks,nxrt,nyrt)
     +  buf(p_tmisfit1),         !(npicks,nxrt,nyrt)
     +  buf(p_alpha),            !(nxrt,nyrt)
     +  buf(p_alpha_work),       !(nxrt*nyrt)
     +  buf(p_work_read),        !(nxrt*nyrt)
     +  buf(p_zmisfit0),         !(npicks,nxrt,nyrt)
     +  buf(p_zmisfit1))         !(npicks,nxrt,nyrt)

      end

      subroutine getparms_vplusz(
     +  alphamin,
     +  alphamax,
     +  topc0,
     +  topc1,
     +  topcg0,
     +  topcg1,
     +  nxrt,                    !number of samples in x
     +  nyrt,                    !number of samples in y
     +  nxrtbeg,nxrtend,         !CIGs to be solved
     +  nyrtbeg,nyrtend,         !CIGs to be solved
     +  ixdisp,                  !ix for display
     +  iydisp,                  !iy for display
     +  iseed0,                  !seed number
     +  iseed1,                  !seed number
     +  nsort,                   !number of offsets in vest3d
     +  sort1,
     +  sort2,
     +  npicks,	                 !number of offsets picked
     +  isort,
     +  fnames0,                 !misfit file name
     +  fnames1,                 !misfit file name
     +  topc0migfile,            !base c
     +  topc1migfile,            !base c
     +  topcg0migfile,           !base c
     +  topcg1migfile,           !base c
     +  basec0migfile,           !base c
     +  basec1migfile,           !base c
     +  topz0gridfile,           !top z
     +  topz1gridfile,           !top z
     +  basez0gridfile,          !base z
     +  basez1gridfile,          !base z
     +  cupdatefile,             !update z
     +  ctrc0file,               !update z
     +  zupdatefile)             !update z

      implicit none

      real alphamin,alphamax
      integer nsortmax
      parameter (nsortmax=2)
c the filenames for the misfit files.  Fill in these
      character*120 fnames0(nsortmax)
      character*120 fnames1(nsortmax)
c the velocity file at the imaging grid. Copy from your SGI work directory
      character*120 topc0migfile
      character*120 topc1migfile

c the velocity gradient file at the imaging grid.
      character*120 topcg0migfile
      character*120 topcg1migfile

c the imaging grid. Copy from your SGI work directory
      character*120 topz0gridfile
      character*120 topz1gridfile

c the velocity file at the imaging grid. Copy from your SGI work directory
      character*120 basec0migfile
      character*120 basec1migfile

c the imaging grid. Copy from your SGI work directory
      character*120 basez0gridfile
      character*120 basez1gridfile

c output
      character*120 cupdatefile
      character*120 ctrc0file
      character*120 zupdatefile

      character*120 cbuf         !read in the first line

      integer nxrt               !number of CIGs in x
      integer nyrt               !number of CIGs in y
      integer nxrtbeg,nxrtend    !CIGs to be solved
      integer nyrtbeg,nyrtend    !CIGs to be solved
      integer ixdisp,iydisp
      integer iseed0
      integer iseed1
      integer nsort
      real sort1,sort2
      integer npicks 	         !number of offsets picked
      integer isort(npicks)
      real topc0,topc1,topcg0,topcg1

      !local copies
      integer nc                 !length of cbuf
      integer ipick

1     continue
      read(5,'(i3,a)') nc,cbuf
      if (cbuf(1:1).eq.'*') goto 1

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
      write(6,*)'Input iseed0=>'
      read(5,*) iseed0
      write(6,*)'Input iseed1=>'
      read(5,*) iseed1
      write(6,*)'Input nsort=>'
      read(5,*) nsort
      write(6,*)'Input sort1=>'
      read(5,*) sort1
      write(6,*)'Input sort2=>'
      read(5,*) sort2
      write(6,*)'Input npicks=>'
      read(5,*) npicks
      write(6,*)'Input isort=> (for ipick=1,npicks)'
      do ipick=1,npicks
        read(5,*) isort(ipick)
      end do

      write(6,*)'Input fnames0 => (for ipick=1,npicks)'
      do ipick=1,npicks
        read(5,*) fnames0(ipick)    
      end do

      write(6,*)'Input fnames1 => (for ipick=1,npicks)'
      do ipick=1,npicks
        read(5,*) fnames1(ipick)   
      end do

      write(6,*)'Input alphamin =>'
      read(5,*) alphamin
      write(6,*)'Input alphamax =>'
      read(5,*) alphamax

      write(6,*)'Input topc0 =>'
      read(5,*) topc0
      write(6,*)'Input topc0migfile =>'
      read(5,*) topc0migfile      

      write(6,*)'Input topc1 =>'
      read(5,*) topc1
      write(6,*)'Input topc1migfile =>'
      read(5,*) topc1migfile     

      write(6,*)'Input topcg0 =>'
      read(5,*) topcg0
      write(6,*)'Input topcg0migfile =>'
      read(5,*) topcg0migfile   

      write(6,*)'Input topcg1 =>'
      read(5,*) topcg1
      write(6,*)'Input topcg1migfile =>'
      read(5,*) topcg1migfile  

      write(6,*)'Input basec0migfile =>'
      read(5,*) basec0migfile  
      write(6,*)'Input basec1migfile =>'
      read(5,*) basec1migfile 

      write(6,*)'Input topz0gridfile =>'
      read(5,*) topz0gridfile    
      write(6,*)'Input topz1gridfile =>'
      read(5,*) topz1gridfile   

      write(6,*)'Input basez0gridfile =>'
      read(5,*) basez0gridfile   
      write(6,*)'Input basez1gridfile =>'
      read(5,*) basez1gridfile  

      write(6,*)'Input cupdatefile =>'
      read(5,*) cupdatefile      

      write(6,*)'Input ctrc0file =>'
      read(5,*) ctrc0file       

      write(6,*)'Input zupdatefile =>'
      read(5,*) zupdatefile    

      write(6,*)
      write(6,*)'nxrt=',nxrt           !second line in .deck file
      write(6,*)'nyrt=',nyrt           !third line in .deck file
      write(6,*)'nxrtbeg=',nxrtbeg     !4th line in .deck file
      write(6,*)'nxrtend=',nxrtend     !5th line in .deck file
      write(6,*)'nyrtbeg=',nyrtbeg     !6th line in .deck file
      write(6,*)'nyrtend=',nyrtend     !7th line in .deck file
      write(6,*)'ixdisp=',ixdisp
      write(6,*)'iydisp=',iydisp
      write(6,*)'iseed0=',iseed0
      write(6,*)'iseed1=',iseed1
      write(6,*)'nsort=',nsort
      write(6,*)'sort1=',sort1
      write(6,*)'sort2=',sort2
      write(6,*)'npicks=',npicks
      write(6,*)'isort=',(isort(ipick),ipick=1,npicks)

      do ipick=1,npicks
        write(6,*)'fnames0(',ipick,')=',fnames0(ipick)   
        write(6,*)'fnames1(',ipick,')=',fnames1(ipick)  
      end do

      write(6,*)'topc0=',topc0
      write(6,*)'topc1=',topc1
      write(6,*)'topcg0=',topcg0
      write(6,*)'topcg1=',topcg1
      write(6,*)'topc0migfile=',topc0migfile 
      write(6,*)'topc1migfile=',topc1migfile 
      write(6,*)'topcg0migfile=',topcg0migfile 
      write(6,*)'topcg1migfile=',topcg1migfile
      write(6,*)'basec0migfile=',basec0migfile
      write(6,*)'basec1migfile=',basec1migfile
      write(6,*)'topz0gridfile=',topz0gridfile
      write(6,*)'topz1gridfile=',topz1gridfile
      write(6,*)'basez0gridfile=',basez0gridfile
      write(6,*)'basez1gridfile=',basez1gridfile
      write(6,*)'cupdatefile=',cupdatefile   
      write(6,*)'ctrc0file=',ctrc0file      
      write(6,*)'zupdatefile=',zupdatefile 

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c subroutine readgrids.f   
c Zhaobo Meng / July 1998
c CWP/CSM
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine readgrids_vplusz(
     +    topc,
     +    topcg,
     +    npicks,	  !number of picks
     +    nxrt,nyrt,      !project size
     +    basecmig,       !velocity grid at the base
     +    topcmig,        !velocity grid at the top
     +    topcgmig,       !velocity grid at the top
     +    topzgrid,       !depth grid at the base
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

	integer iunit

c global parms
	integer nxrt,nyrt,npicks
	real topc,topcg

c The following grids are used by HPSDM.  mva3d needs this information.
        real basecmig(nxrt,nyrt)     !velocity grid at the base
        real topcmig(nxrt,nyrt)	     !velocity grid at the top
        real topcgmig(nxrt,nyrt)     !velocity grid at the top
        real topzgrid(nxrt,nyrt)     !depth grid at the base
        real basezgrid(nxrt,nyrt)    !depth grid at the base
        real tmisfit(npicks,nxrt,nyrt)
	real work_read(nyrt*nxrt)

        character*120 topcmigfile
        character*120 topcgmigfile
        character*120 basecmigfile
        character*120 topzgridfile
        character*120 basezgridfile
        integer nsortmax
        parameter (nsortmax=2)
        character*120 fnames(nsortmax)

        integer ix,iy,ipick

        write(*,*)'topc,topcg=',topc,topcg
        write(*,*)'npicks=',npicks
        write(*,*)'nxrt,nyrt=',nxrt,nyrt

        write(*,*)'topcmigfile  =',topcmigfile	
        write(*,*)'topcgmigfile =',topcgmigfile	
        write(*,*)'basecmigfile =',basecmigfile	
        write(*,*)'topzgridfile =',topzgridfile	
        write(*,*)'basezgridfile=',basezgridfile	
        do ipick=1,npicks
          write(*,*)'fnames(',ipick,')=',fnames(ipick)
        end do

	write(*,*)
	write(*,*)'Inside readgrids_vplusz...'

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

        iunit=99
        print *,'Read in',basecmigfile 
        call readgrid(iunit,basecmigfile,work_read,nxrt,nyrt)
        do iy=1,nyrt
          do ix=1,nxrt
            basecmig(ix,iy)=work_read(ix+(iy-1)*nxrt)
          enddo
        enddo

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
	  print *,'fnames(',ipick,')=',fnames(ipick)
	  call readgrid(iunit,fnames(ipick),work_read,nxrt,nyrt)

	  do iy=1,nyrt
	    do ix=1,nxrt
	      tmisfit(ipick,ix,iy)=work_read(ix+(iy-1)*nxrt)
	    enddo
	  enddo
        enddo
        end 
