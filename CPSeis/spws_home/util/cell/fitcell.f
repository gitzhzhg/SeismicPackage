c23456789012345678901234567890123456789012345678901234567890123456789012
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
      subroutine fitcell(idir,ndim,nzi,iyi,nyi,ixi,nxi,xi,yi,zi,vi
     1,no,xo,yo,zo,vo,mwk,wk,ierr)
c***************************** copyright notice ************************
c*
c*                 confidential and proprietary information
c*                              of conoco inc.
c*                      protected by the copyright law
c*                          as an unpublished work
c*
c***************************** copyright notice ************************
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c\user doc
c-----------------------------------------------------------------------
c                       conoco processing system
c                 exploration research & services division
c                              conoco inc.
c
c                        c p s   p r i m i t i v e
c
c primitive name: fitcell
c         author: harlan
c   date written: 92/09
c   last revised: 93/05/13
c
c     purpose: this routine fits samples from a data grid with a cell
c              model.
c-----------------------------------------------------------------------
c                           calling sequence
c      call fitcell(idir,nzi,ixi,nxi,xi,zi,vi,no,xo,zo,vo,wk,nwk)
c
c see notes following this list for detailed description.
c name        type*       description     *type: i=in, o=out, b=both
c ----        ----        -----------
c idir           i      1, find vo from vi; -1, vi from vo.
c nzi            i      number of horizons in vi
c ixi(nzi)       i      horizon start pointer for arrays xi,zi,vi
c nxi(nzi)       i      horizon length array
c                       horizon i starts at element ixi(i)+1 
c                       and has nxi(i) elements
c xi(ni)         i      x locations for vi (ni = ixi(nzi)+nxi(nzi))
c zi(ni)         i      z locations for vi
c vi(ni)    o or i      input model of values at positions in xi, zi.
c no             i      number of output model x,z locations
c xo(no)         i      x locations for vo
c zo(no)         i      z locations for vo
c vo(no)   o or  i      output model of values at positions in xo, zo.
c wk(nwk)        i      scratch array.
c nwk            i      set nwk = 7*nzi + 3*ni + 2*no + max(ni,no)
c                            ni = ixi(nzi)+nxi(nzi)
c
c fit some samples from a data grid with linearly interpolated model.
c if idir=1, then find data vo(no) from model vi(ni)
c set idir=-1, to reverse and find model vi from data vo.
c
c model is defined by slownesses in vi(nxi) and the spatial
c   position in xi,zi
c  the input model has nzi horizons.
c  each horizon has nxi(i) control points.  
c  the first control point of the i th horizon is at location ixi(i)+1
c  within arrays xi,zi,vi.  the i th horizon has nxi(i) control points.
c
c  the slowness at an output location is obtianed by first interpolating
c  the depth and slowness along each horizon to the x coordinate of the 
c  output location then linearly verticaly between horizons.
c
c data is defined by slownesses in vo(no) and the spatial
c   position in xo,zo
c
c this routine expects that you will interpolate vo(no)
c   linearly between adjacent values of vi(ni)
c   and as a constant off both ends.
c order of xo,zo,vo is unimportant.
c
c set nwk = 7*nzi + 3*ni + 2*no + max(ni,no); ni = ixi(nzi)+nxi(nzi)
c-----------------------------------------------------------------------
c                                 notes
c 1. there also exist lower level routines fitcel2 and fitcel1.
c    this particular version was customized for hanson's routine cell.
c-----------------------------------------------------------------------
c\end doc
c\prog doc
c-----------------------------------------------------------------------
c                         revision history
c     date      author    description
c     ----      ------    -----------
c 5.  93/09/19  hanson    add 3d fit and model modify routines
c 4.  93/05/13  hanson    accept repeated x values in fitcell1
c 3.  92/12/11  harlan    major rewrite of fitcel3
c               hanson    for new velocity definiton
c 2.  92/10/23  harlan    fix incorrect declaration of zx in fitcell()
c 1.  92/09/07  harlan    original version
c-----------------------------------------------------------------------
c   subroutine, function, entry and common block names in this module
c         subroutines:
c fitcell  fitcell1 fitcell2 fitcell3 fitcell4 fitcell5
c fitccnj2 fitcconj fitcdivm fitchadd fitchdot fitcheyu fitcmnmx fitcput 
c fitccopy fitcwork fitcsetv fitchrnd fitcran1 fitclimt
c         entries:
c fitcheyd fitcputc fitcputi fitcputo fitcputd fitcwors fitcworl
c         no functions or common blocks.
c-----------------------------------------------------------------------
c                  externals referenced by this module
c no externals.
c-----------------------------------------------------------------------
c                         memory requirements
c
c  storage       none.
c  heap(dynamic) none.
c-----------------------------------------------------------------------
c\end doc
c
c forward and least-squares inverse of vo from vi; idir=1, forward,
c  -1, get least-squares estimate of vi from vo, uses fitcell3.
c  xi,zi,vi have nzi different horizons - the first point of horizon j
c  starts at location ixi(j)+1 and has nxi(j) points
c  xi,zi,vi have a total of ixi(nzi)+nxi(nzi) points.
c  wk - work scratch space
c  nwk = 7*nzi + 3*ni + 2*no + max(ni,no); ni = ixi(nzi)+nxi(nzi)
      implicit none
      integer idir,nzi,ndim,no,mwk
      integer iyi(*),nyi(*),ixi(*),nxi(*)
      real xi(*),yi(*),zi(*),vi(*),xo(no),yo(no),zo(no),vo(no)
     1,wk(mwk)

      integer nwk,k1,k2,ki,ko,kg,iwk,ni,ng,mter,nter,iter,jdir,ierr
      integer i,ny,kilr,kwlr,kiyi,knyi,kixi,knxi,kxi,kzi,kvi,nz2,nlr
      integer n2
      integer iwk_kg,nwk_kg
      real    alpha,error
      real    xi_min,xi_max,xo_min,xo_max
      real    yi_min,yi_max,yo_min,yo_max
      real    zi_min,zi_max,zo_min,zo_max
      real    vi_min,vi_max,vo_min,vo_max
      parameter (alpha=0.00001,mter=20)

      ierr = 0

      if (ndim .eq. 3) then
        ny = iyi(nzi) + nyi(nzi)        ! # of model y values
        ni = ixi(ny) + nxi(ny)          ! # of model values - ki
      else
        ny = 1
        ni = ixi(nzi) + nxi(nzi)        ! # of model values - ki
      endif

      ng = max(ni,no)                   ! # of points in gradient
      nlr = 2**ndim

      if (idir .lt. 0) then
        write(6,'(/,'' fitcell creating model from data'')')
        write(6,'('' dimensionality               ='',i10)')ndim
        write(6,'('' number of points in data     ='',i10)')no
        write(6,'('' number of points in model    ='',i10)')ni
        write(6,'('' number of z horizons in model='',i10)')nzi
        write(6,'('' number of y horizons in model='',i10)')ny

        call fitcmnmx(xi_min,xi_max,xi,ni,1) ! min, max values in xo
        call fitcmnmx(xo_min,xo_max,xo,no,1) ! min, max values in xo
        if (ndim .eq. 3) then
        call fitcmnmx(yi_min,yi_max,yi,ni,1) ! min, max values in yo
        call fitcmnmx(yo_min,yo_max,yo,no,1) ! min, max values in yo
        endif
        call fitcmnmx(zi_min,zi_max,zi,ni,1) ! min, max values in zo
        call fitcmnmx(zo_min,zo_max,zo,no,1) ! min, max values in zo
        call fitcmnmx(vo_min,vo_max,vo,no,1) ! min, max values in vo

        write(6,'('' input  x min='',g16.9,'' max='',g16.9)')
     1xi_min,xi_max
        write(6,'('' output x min='',g16.9,'' max='',g16.9)')
     1xo_min,xo_max
        if (ndim .eq. 3) then
        write(6,'('' input  y min='',g16.9,'' max='',g16.9)')
     1yi_min,yi_max
        write(6,'('' output y min='',g16.9,'' max='',g16.9)')
     1yo_min,yo_max
        endif
        write(6,'('' input  z min='',g16.9,'' max='',g16.9)')
     1zi_min,zi_max
        write(6,'('' output z min='',g16.9,'' max='',g16.9)')
     1zo_min,zo_max
        write(6,'('' output v min='',g16.9,'' max='',g16.9)')
     11./vo_max,1./vo_min

      endif    ! if (idir .lt. 0) then

      call fitcwors(1,mwk)              ! initialize work space

c  if inverse modeling set some work counters
c  later work(iwk_kg) will be reused
      if (idir .ne. +1) then            ! get vi from vo
        call fitcwork(kg ,ng )          ! gradient
        call fitcworl(nwk_kg)           ! how much work space left
        call fitcwork(iwk_kg,nwk_kg)    ! work space for final modeling
        call fitcwors(iwk_kg,nwk_kg)    ! initialize work space
        call fitcwork(ki ,ni )          ! model perturbation copy
        call fitcwork(ko ,no )          ! data error copy
      endif    ! if (idir .ne. +1) then ! get vi from vo

      call fitcwork(kilr,nlr*no)        ! pointer array
      call fitcwork(kwlr,nlr*no)        ! weight array
      call fitcworl(nwk)                ! how much work space left
      call fitcwork(iwk  ,nwk )         ! work space
      call fitcworc(ierr)
      if (ierr .ne. 0) goto 999

c  compute coefficients and pointers for modeling vo from vi
c      if (idir .lt. 0) 
c     1write(6,'('' computing modeling coefficients'')')

      call fitcell2(ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,xi,yi,zi
     1,no,xo,yo,zo,wk(kilr),wk(kwlr),nwk,wk(iwk),ierr)
      if (ierr .ne. 0) goto 999

c  forward model vo from vi
      if (idir .eq. +1) then    ! get vo from vi

        call fitcmodl(+1,ni,vi,no,vo,nlr,wk(kilr),wk(kwlr))

      else
c  inverse model vi from vo

        call fitccopy(no,vo,wk(ko))        ! copy data points to work space

        iter = 0                           ! iteration counter
        nter = min(mter,ni+1)              ! max number of iterations
        call fitcmnmx(vo_min,vo_max,vo,no,1) ! min, max values in vo

c        write(6,'('' iterating on solution by conjugate gradient''
c     1,'' number of iterations='',i10)')nter

    1   continue
          call fitccnj2(vi,wk(ki),jdir,wk(ko),iter,wk(kg),no,ni,alpha)

          if (iter .le. nter) then

            if (jdir .eq. +1) then

          call fitcmodl(+1,ni,wk(ki),no,wk(kg),nlr,wk(kilr),wk(kwlr))
c  compute the difference between 
c  the current forward modeled data and the input data

             call fitc_compute_error(no,wk(kg),vo,error)

c        write(6,'('' after  modeling iteration='',i5
c     1,'' error='',g16.9)')iter,error

            elseif (jdir .eq. -1) then    ! if (jdir .eq. +1) then

          call fitcmodl(-1,ni,wk(kg),no,wk(ko),nlr,wk(kilr),wk(kwlr))

            endif    ! if (jdir .eq. +1) then

            goto 1

          endif    ! if (iter .le. nter) then

c  vi has been optimized, but not all elements have necessarily been set
c  model all elements of vi from those that have actualy been set.
c  mark elements of vi set during optimiziation
        call fitcmodl(-2,ni,wk(kg),no,wk(ko),nlr,wk(kilr),wk(kwlr))

c  limit vi to the range in vo
c        write(6,'('' limiting values to the original range''
c     1,/,'' vo_min='',g16.9,'' vo_max='',g16.9)')1./vo_max,1./vo_min

        call fitclimt(ni,vi,vo_min,vo_max)

c  model vi from subset of vi actually set
c        write(6,'('' before filling in unset values'')')

        call fitcell4(ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,xi,yi,zi,vi,wk(kg)
     1,nwk_kg,wk(iwk_kg),ierr,n2)
        call fitcmnmx(vi_min,vi_max,vi,ni,1) ! min, max values in vi

c        write(6,'('' after  filling in unset values'')')
        write(6,'('' number of points in input  ='',i10)')ni
        write(6,'('' number set during inversion='',i10)')n2
        write(6,'('' number filled in afterwards='',i10)')ni-n2
        write(6,'('' input  v min='',g16.9,'' max='',g16.9)')
     11./vi_max,1./vi_min
        write(6,'('' output v min='',g16.9,'' max='',g16.9)')
     11./vo_max,1./vo_min
        if (ierr .ne. 0) goto 999

      endif
c      if(ndim.eq.2) then
c      write(6,'('' i='',i5,'' ixi='',i8,'' nxi='',i8)')
c     1(i,ixi(i),nxi(i),i=1,ny)
c      write(6,'(/,'' ni='',i5,/,'' i xi yi zi vi'')')ni
c      write(6,'(1x,i5,1x,f10.2,1x,f10.2,1x,f10.4,1x,f10.2)')
c     1,(i,xi(i),0.,zi(i),1./vi(i),i=1,ni)
c      write(6,'(/,'' no='',i5,/,'' i xo yo zo vo'')')no
c      write(6,'(1x,i5,1x,f10.2,1x,f10.2,1x,f10.4,1x,f10.2)')
c     1,(i,xo(i),0.,zo(i),1./vo(i),i=1,no)
c      else
c      write(6,'(/,'' ni='',i5,/,'' i xi yi zi vi'')')ni
c      write(6,'(1x,i5,1x,f10.2,1x,f10.2,1x,f10.4,1x,f10.2)')
c     1,(i,xi(i),yi(i),zi(i),1./vi(i),i=1,ni)
c      write(6,'(/,'' no='',i5,/,'' i xo yo zo vo'')')no
c      write(6,'(1x,i5,1x,f10.2,1x,f10.2,1x,f10.4,1x,f10.2)')
c     1,(i,xo(i),yo(i),zo(i),1./vo(i),i=1,no)
c      endif

      return

  999 continue
      print'('' error in fitcell ierr='',i8)',ierr
      return

      end


c23456789012345678901234567890123456789012345678901234567890123456789012
c     ell1(

      subroutine fitcell1(xo,xeps,nxi,xi,il,ir,wl,wr)
c get nearest index to xo from xi to left and right.
c  if xi is outvide range of xo use nearest end member for both il,ir
c  xeps = min resolution in model
c  nxi = # of elements in xi to convider
c  il = nearest point to left (xi<xo)
c  ir = nearest point to right (xi>xo)
c  wl = weight to left vide
c  wr = weight to right vide
c  this code is stripped from bill harlan's fitcell1
      implicit none
      integer nxi,il,ir
      real xo,xi(nxi),xeps,wl,wr,dx
      integer i,nrepeat
      real xmin,xmax,eps
      xmin = xo
      xmax = xo
      il = 0
      ir = 0
      nrepeat = 0
      do 1 i = 1 , nxi
        xmin = min(xmin,xi(i))
        xmax = max(xmax,xi(i))
        if (xi(i) .lt. xo) then
          if (il .eq. 0) then
            il = i
          else
            if (xi(i) .gt. xi(il)) then
              il = i
            elseif (xi(i) .eq. xi(il)) then
c              goto 999
              nrepeat = nrepeat + 1
            endif
          endif
        elseif (xi(i) .gt. xo) then
          if (ir .eq. 0) then
            ir = i
          else
            if (xi(i) .lt. xi(ir)) then
              ir = i
            elseif (xi(i) .eq. xi(ir)) then
c              goto 999
              nrepeat = nrepeat + 1
            endif
          endif
        else
          il = i
          ir = i
        endif
    1 continue
c see if output point is off end of input points.
      if (il .eq. 0) il = ir
      if (ir .eq. 0) ir = il
      if (il*ir .eq. 0) then
        print'('' impossible: code bug in fitcell1'')'
        stop
      endif
c now have two points spannng output point
      dx = xi(ir) - xi(il)
      eps = max(xeps,(1.e-8)*abs(xmax-xmin))
      if (dx .gt. eps) then
        wr = (xo - xi(il)) / dx
      else
c...   same point on left and right
        wr = 0.5
      endif
      wl = 1. - wr
c      if (nrepeat .ne. 0.) goto 999
      return

c error returns
c  999 continue
c      write(6,'('' error in fitcell1: input position repeated''
c     1,'' no. repeat='',i8)')nrepeat
c      write(6,'('' xo='',f10.2,'' nxi='',i8
c     1,100(/,1x,i5,1x,f10.2))')xo,nxi,(i,xi(i),i=1,min(100,nxi))
c      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcell2(ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,xi,yi,zi
     1,no,xo,yo,zo,ilr,wlr,mwk,wk,ierr)
      implicit none
      integer ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,no,ilr,mwk
      real    xi,yi,zi,xo,yo,zo,wlr,wk(mwk)
      integer kyave,kzave,kix1,kix2,kwx1,kwx2,kiy1,kiy2,kwy1,kwy2
      integer iwk,ierr
      call fitcwors(1,mwk)
      call fitcwork(kyave,ny )
      call fitcwork(kzave,nzi)
      call fitcwork(kix1 ,ni )
      call fitcwork(kix2 ,ni )
      call fitcwork(kwx1 ,ni )
      call fitcwork(kwx2 ,ni )
      call fitcwork(kiy1 ,ny )
      call fitcwork(kiy2 ,ny )
      call fitcwork(kwy1 ,ny )
      call fitcwork(kwy2 ,ny )
      call fitcwork(iwk,1)
      call fitcworc(ierr)
      if (ierr .ne. 0) goto 999

      call fitcell3(ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,xi,yi,zi,no,xo,yo,zo
     1,ilr,wlr,wk(kyave),wk(kzave),wk(kix1),wk(kix2)
     1,wk(kwx1),wk(kwx2),wk(kiy1),wk(kiy2),wk(kwy1),wk(kwy2))

      return

  999 continue
      print'('' error in fitcell2 ierr='',i8)',ierr
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcell3(ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,xi,yi,zi
     1,no,xo,yo,zo,ilr,wlr,yave,zave,ix1,ix2,wx1,wx2,iy1,iy2,wy1,wy2)
      implicit none
      integer ndim,ny,ni,nzi,no
      integer iyi(*),nyi(*),ixi(*),nxi(*)
     1,ix1(*),ix2(*),iy1(*),iy2(*),ilr(2,ndim-1,2,no)
      real xi(*),yi(*),zi(*),xo(no),yo(no),zo(no),wlr(2,ndim-1,2,no)
     1,yave(*),zave(nzi),wx1(*),wx2(*),wy1(*),wy2(*)

      integer i,iy,iz,iz1,iz2
      real xeps,yeps,zeps,xmini,xmaxi,xmino,xmaxo
     1,ymini,ymaxi,ymino,ymaxo,zmini,zmaxi,zmino,zmaxo
      real w0,wz1,wz2
      parameter (w0=1e-4)

c      call fitcsetv(ny,yave,0.)

c compute model resolution using input and ouput vales
      call fitcmnmx(xmini,xmaxi,xi,ni ,1) ! min, max x values in xi
      call fitcmnmx(xmino,xmaxo,xo,no,1) ! min, max x values in xo
      xeps = (1.e-8)*abs(max(xmaxi,xmaxo)-min(xmini,xmino))

      if (ndim .eq. 3) then
      call fitcmnmx(ymini,ymaxi,yi,ni ,1) ! min, max y values in yi
      call fitcmnmx(ymino,ymaxo,yo,no,1) ! min, max y values in yo
      yeps = (1.e-8)*abs(max(ymaxi,ymaxo)-min(ymini,ymino))
      endif

      call fitcmnmx(zmini,zmaxi,zi,ni ,1) ! min, max z values in zi
      call fitcmnmx(zmino,zmaxo,zo,no,1) ! min, max z values in zo
      zeps = (1.e-8)*abs(max(zmaxi,zmaxo)-min(zmini,zmino))

      do i = 1 , no    ! cycle over output points
c  get indices, wieghts and depth for each horizon at this xo value
c - i1 is left, i2 is right
        if (ndim .eq. 3) then

          do iz = 1 , nzi    ! cycle over horizon

            do iy = iyi(iz)+1 , iyi(iz)+nyi(iz)

              call fitcell1(xo(i),xeps,nxi(iy),xi(ixi(iy)+1)
     1,ix1(iy),ix2(iy),wx1(iy),wx2(iy))
              ix1(iy) = ix1(iy) + ixi(iy)    ! make ix1 and ix2
              ix2(iy) = ix2(iy) + ixi(iy)    ! absolute pointers
              yave(iy) = wx1(iy) * yi(ix1(iy))
     1                 + wx2(iy) * yi(ix2(iy))

            enddo    ! do iy = iyi(iz)+1 , iyi(iz)+nyi(iz)

            call fitcell1(yo(i),yeps,nyi(iz),yave(iyi(iz)+1)
     1,iy1(iz),iy2(iz),wy1(iz),wy2(iz))

            iy1(iz) = iy1(iz) + iyi(iz)      ! make iy1 and iy2
            iy2(iz) = iy2(iz) + iyi(iz)      ! absolute pointers
            zave(iz) = wx1(iy1(iz)) * wy1(iz) * zi(ix1(iy1(iz)))
     1               + wx2(iy1(iz)) * wy1(iz) * zi(ix2(iy1(iz)))
     1               + wx1(iy2(iz)) * wy2(iz) * zi(ix1(iy2(iz)))
     1               + wx2(iy2(iz)) * wy2(iz) * zi(ix2(iy2(iz)))

          enddo    ! do iz = 1 , nzi    ! cycle over horizon

c determine which 2 z's from zave to use at zo;iz1 is above, iz2 is below
          call fitcell1(zo(i),zeps,nzi,zave,iz1,iz2,wz1,wz2)
          ilr(1,1,1,i) = ix1(iy1(iz1))
          ilr(2,1,1,i) = ix2(iy1(iz1))
          ilr(1,2,1,i) = ix1(iy2(iz1))
          ilr(2,2,1,i) = ix2(iy2(iz1))
          ilr(1,1,2,i) = ix1(iy1(iz2))
          ilr(2,1,2,i) = ix2(iy1(iz2))
          ilr(1,2,2,i) = ix1(iy2(iz2))
          ilr(2,2,2,i) = ix2(iy2(iz2))

          wlr(1,1,1,i) = wx1(iy1(iz1)) * wy1(iz1) * wz1
          wlr(2,1,1,i) = wx2(iy1(iz1)) * wy1(iz1) * wz1
          wlr(1,2,1,i) = wx1(iy2(iz1)) * wy2(iz1) * wz1
          wlr(2,2,1,i) = wx2(iy2(iz1)) * wy2(iz1) * wz1
          wlr(1,1,2,i) = wx1(iy1(iz2)) * wy1(iz2) * wz2
          wlr(2,1,2,i) = wx2(iy1(iz2)) * wy1(iz2) * wz2
          wlr(1,2,2,i) = wx1(iy2(iz2)) * wy2(iz2) * wz2
          wlr(2,2,2,i) = wx2(iy2(iz2)) * wy2(iz2) * wz2

        else    ! if (ndim .eq. 3) then

          do iz = 1 , nzi    ! cycle over horizon

            call fitcell1(xo(i),xeps,nxi(iz),xi(ixi(iz)+1)
     1,ix1(iz),ix2(iz),wx1(iz),wx2(iz))
            ix1(iz) = ix1(iz) + ixi(iz)    ! make ix1 and ix2
            ix2(iz) = ix2(iz) + ixi(iz)    ! absolute pointers
            zave(iz) = wx1(iz) * zi(ix1(iz))
     1               + wx2(iz) * zi(ix2(iz))

          enddo    ! do iz = 1 , nzi    ! cycle over horizon

c determine which 2 z's from zave to use at zo;iz1 is above, iz2 is below
          call fitcell1(zo(i),zeps,nzi,zave,iz1,iz2,wz1,wz2)

          ilr(1,1,1,i) = ix1(iz1)
          ilr(2,1,1,i) = ix2(iz1)
          ilr(1,1,2,i) = ix1(iz2)
          ilr(2,1,2,i) = ix2(iz2)

          wlr(1,1,1,i) = wx1(iz1) * wz1
          wlr(2,1,1,i) = wx2(iz1) * wz1
          wlr(1,1,2,i) = wx1(iz2) * wz2
          wlr(2,1,2,i) = wx2(iz2) * wz2
  
        endif    ! if (ndim .eq. 3) then

c  scale weights so they sum to 1, very small weights are set to 0.
        call fitcwght(w0,2**ndim,wlr(1,1,1,i))

      enddo    ! do i = 1 , no    ! cycle over output points

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcell4(ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,xi,yi,zi,vi,v0
     1,mwk,wk,ierr,n2)
      implicit none
      integer ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,mwk
      real    xi(*),yi(*),zi(*),vi(*),v0,wk(mwk)
      integer nixi,kiyi,knyi,kixi,knxi,kxi,kyi,kzi,kvi,nwk,iwk,ierr,n2

      if (ndim .eq. 3) then
        nixi = ny
      else
        nixi = nzi
      endif
      call fitcwors(1,mwk) ! initialize work
      call fitcwork(kiyi,nzi )  ! iyi copy
      call fitcwork(knyi,nzi )  ! nyi copy
      call fitcwork(kixi,nixi)  ! ixi copy
      call fitcwork(knxi,nixi)  ! nxi copy
      call fitcwork(kxi ,ni  )  ! xi copy
      call fitcwork(kyi ,ni  )  ! yi copy
      call fitcwork(kzi ,ni  )  ! zi copy
      call fitcwork(kvi ,ni  )  ! vi copy
      call fitcworl(nwk)
      call fitcwork(iwk ,nwk)  ! work space
      call fitcworc(ierr)
      if (ierr .ne. 0) goto 999

c      call fitcsetv(nwk,wk(iwk),0.)

      call fitcell5(ndim,ny,ni,nzi,iyi,nyi,ixi,nxi,xi,yi,zi,vi,v0
     1,wk(kiyi),wk(knyi),wk(kixi),wk(knxi)
     1,wk(kxi),wk(kyi),wk(kzi),wk(kvi),nwk,wk(iwk),ierr,n2)
      if (ierr .ne. 0) goto 999

      return

  999 continue
      print'('' error in fitcell4 ierr='',i8)',ierr
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcell5(ndim,ny,n1,nz1,iy1,ny1,ix1,nx1,x1,y1,z1,v1,v0
     1,iy2,ny2,ix2,nx2,x2,y2,z2,v2,mwk,wk,ierr,n2)

c copy model points which were actualy optimized
c  v1 is all values
c  v0=1 for optimized values
c  v2 is optimized values
      implicit none
      integer ndim,ny,n1,nz1,no,mwk
      integer iy1(*),ny1(*),ix1(*),nx1(*)
     1       ,iy2(*),ny2(*),ix2(*),nx2(*)
      real x1(*),y1(*),z1(*),v1(*),v0(*),x2(*),y2(*),z2(*),v2(*)
      real wk(mwk)
      integer n2,nz2,jx1,jy1,jz1,jx2,jy2,jz2,jy0,jz0,i,ierr

      integer mlr
      parameter (mlr=8)
      integer nlr,ilr(mlr),j
      real wlr(mlr)

c  copy subset of model actually optimized
      nlr = 2**ndim

      nz2 = 0
      jy2 = 0
      jx2 = 0
      do jz1 = 1 , nz1
        jz0 = 0    ! flag to mark start of z loop
        if (ndim .eq. 3) then
          do jy1 = iy1(jz1)+1 , iy1(jz1)+ny1(jz1)
            jy0 = 0
            do jx1 = ix1(jy1)+1 , ix1(jy1)+nx1(jy1)
              if (v0(jx1) .eq. 1) then
                if (jz0 .eq. 0 .and. jy0 .eq. 0) then
                  jz0 = jz0 + 1
                  nz2 = nz2 + 1
                  iy2(nz2) = jy2
                  ny2(nz2) = 0
                endif
                if (jy0 .eq. 0) then
                  jy0 = jy0 + 1
                  jy2 = jy2 + 1
                  ny2(nz2) = ny2(nz2) + 1
                  ix2(jy2) = jx2
                  nx2(jy2) = 0
                endif

                jx2 = jx2 + 1
                nx2(jy2) = nx2(jy2) + 1
                x2(jx2) = x1(jx1)
                y2(jx2) = y1(jx1)
                z2(jx2) = z1(jx1)
                v2(jx2) = v1(jx1)
              endif
            enddo    ! do jx1 = ix1(jy1)+1 , ix1(jy1)+nx1(jy1)
          enddo    ! do jy1 = iy1(jz1)+1 , iy1(jz1)+ny1(jz1)
        else    ! if (ndim .eq. 3) then
          do jx1 = ix1(jz1)+1 , ix1(jz1)+nx1(jz1)
            if (v0(jx1) .eq. 1) then
              if (jz0 .eq. 0) then
                jz0 = jz0 + 1
                nz2 = nz2 + 1
                ix2(nz2) = jx2
                nx2(nz2) = 0
              endif
              jx2 = jx2 + 1
              nx2(nz2) = nx2(nz2) + 1
              x2(jx2) = x1(jx1)
              z2(jx2) = z1(jx1)
              v2(jx2) = v1(jx1)
            endif
          enddo    ! jx1 = ix1(jz1)+1 , ix1(jz1)+nx1(jz1)
        endif    ! if (ndim .eq. 3) then
      enddo    ! do jz1 = 1 , nz1

      n2 = jx2
c  forward model v1 from v2 subset
      do i = 1 , n1
c  get pointers and coefficients
        call fitcell2(ndim,ny,n2,nz2,iy2,ny2,ix2,nx2,x2,y2,z2
     1,1,x1(i),y1(i),z1(i),ilr,wlr,mwk,wk,ierr)
        if (ierr .ne. 0) goto 999

c  forward model v1
       call fitcmodl(1,n2,v2,1,v1(i),nlr,ilr,wlr)
      enddo    ! do i = 1 , n1
      return

  999 continue
      print'('' error in fitcell2 ierr='',i8)',ierr
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcell6(nxv,iv1,iv2,iv3,xv,yv,zv,vel,icv
     1,nx,xmin,xinc,ny,ymin,yinc,nz,zmin,zinc,v,mwork,work,ierr)
      implicit none
      integer nxv,iv1,iv2(1),iv3(1),icv,nx,ny,nz,mwork
      real    xmin,xinc,ymin,yinc,zmin,zinc
      real    xv(1),yv(1),zv(1),vel(1),v(nz,nx,ny),work(mwork)
      integer nv,ivp,nzi,iyi,nyi,ixi,nxi,ix,iy,iz,nwork,iwork,ierr
      real x,y,z

      ierr = 0

c  make a list of the velocity control points
      call fitcpnt1(icv,nxv,iv1,nv,ivp)
      if (nv .eq. 0) return

c  get space for pointers
      call fitcwors(1,mwork)
      call fitcwork(iyi,nv)    ! 1st point in each line
      call fitcwork(nyi,nv)    ! # of lines in each layer
      call fitcwork(ixi,nv)    ! 1st point in each layer
      call fitcwork(nxi,nv)    ! # of points in each layer
      call fitcworc(ierr)
      if (ierr .ne. 0) goto 999

      call fitcpnt2(nv,iv2(ivp),iv3(ivp)
     1,nv,nzi,work(iyi),work(nyi),work(ixi),work(nxi))

c  get space for a list of the 2d grid values that fall within this layer
      call fitcworl(nwork)
      call fitcwork(iwork,0)

c  make a list of the 3d grid values that fall within this layer
      do 1 ix = 1 , nx
        x = (ix - 1) * xinc + xmin
        do 2 iy = 1 , ny
          y = (iy - 1) * yinc + ymin
          do 3 iz = 1 , nz
            z = (iz - 1) * zinc + zmin
            call fitcell(+1,3
     1,nzi,work(iyi),work(nyi),work(ixi),work(nxi)
     1,xv(ivp),yv(ivp),zv(ivp),vel(ivp)
     1,1,x,y,z,v(iz,ix,iy),nwork,work(iwork),ierr)
            if (ierr .ne. 0) goto 999
    3     continue
    2   continue
    1 continue
      return

  999 continue
      print'('' error in fitcell6 work allocation'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcmodl(idir,ni,vi,no,vo,nlr,ilr,wlr)
c  if idir=1 compute vo at xo,zo using xi,zi,vi - ; -1, for adjoint
c  -2, to mark model samples that affect data.
c  adjoint is not initialized.  set to zero before first vo.
      implicit none
      integer idir,ni,no,nlr,ilr(nlr,no)
      real    vi(ni),vo(no),wlr(nlr,no)
      integer i,j
      real    wsum

c  compute the output value using the four surrounding points
c      write(6,'('' fitcmodl idir='',i2,'' nlr='',i8,'' no='',i8
c     1,'' ni='',i8)')idir,nlr,no,ni
      if (idir .eq. +1) then    ! forward calculation
          do 1 j = 1 , no
            vo(j) = 0.
c      wsum = 0
            do 2 i = 1 , nlr
              vo(j) = vo(j) + wlr(i,j) * vi(ilr(i,j))
c      wsum = wsum + wlr(i,j)
    2     continue
c      write(6,'(/,'' j='',i5,'' vo='',f10.2,'' wsum='',f10.4
c     1,/,'' i  ilr  vi  wlr'')')j,1./vo(j),wsum
c      write(6,'(i5,1x,i5,1x,f10.2,1x,f10.4)')
c     1(i,ilr(i,j),1./vi(ilr(i,j)),wlr(i,j),i=1,nlr)
    1   continue
      elseif (idir .eq. -1) then    ! adjoint
        call fitcsetv(ni,vi,0.)
        do 3 i = 1 , nlr
          do 4 j = 1 , no
            vi(ilr(i,j)) = vi(ilr(i,j)) + wlr(i,j) * vo(j)
    4     continue
    3   continue
      elseif (idir .eq. -2) then    ! mark model samples that affect data
        call fitcsetv(ni,vi,0.)
        do 5 i = 1 , nlr
          do 6 j = 1 , no
            if (wlr(i,j) .ne. 0.) vi(ilr(i,j)) = 1.
    6     continue
    5   continue
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     wght(

      subroutine fitcwght(w0,nw,w)
c  set weights to avoid values <w0
      implicit none
      integer nw
      real w0,w(nw)
      integer iw
      real wsum
      wsum = 0.
      do 1  iw = 1 , nw
        if (w(iw) .le. w0) w(iw) = 0.
        wsum = wsum + w(iw)
    1 continue
c      write(6,'('' wght nw='',i5,'' wsum='',g15.9
c     1,/,'' iw  w'')')nw,wsum
      do 2 iw = 1 , nw
        w(iw) = w(iw) / wsum
c      write(6,'(1x,i5,1x,f10.4)'),iw,w(iw)
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     cnj2(

      subroutine fitccnj2(m,p,idir, e,iter, gfp,nd,nm,alpha)
c conjugate gradient algorithm, simpler arguments than conj
cndxn cnj2( n
c see conj to know what is going on
c m is initialized to zero
c dimension gfp() to larger of nd or nm
c alpha is 'fractional importance of error in a model sample with
c    respect to a sample of data error'
c    vard/varm = ((alpha*alpha*(nd))/(nm))
c set alpha = delta d / delta m
c input gfp nd nm alpha
c output: m,p,idir
c input and output: e,iter
c initialize iter=0 and e=data
c    alpha = #
c    nter = #
c    nd = #
c    nm = #
c    iter = 0
cc    nmax = max(nd,nm)
cc    nwkn = nwk
cc    call fitcwork(1,mwk)
cc    call fitcwork(km,nm)
cc    call fitcwork(kp,nm)
cc    call fitcwork(ke,nd)
cc    call fitcwork(kgfp,nmax)
cc    call fitcworl(nwkn)
cc    call fitcwork(kwk,nwkn)
cc    call fitccopy(nd,data,wk(ke))
c    e = data
c 10 continue
c    call fitccnj2(m,p,idir, e,iter, gfp,nd,nm,alpha)
c    if(iter.le.nter) then
c      if(idir.eq.1)  gfp = f (p)
c      if(idir.eq.-1) gfp = adjoint f* (e)
c      go to 10
c    endif
c
      implicit none
      integer nd,nm,idir,iter
      real m(nm),gfp(nd),p(nm),e(nd),alpha
      real varm,r
      if(iter.eq.0) call fitcsetv(nm,m,0.)
      varm = 1.
      r = ((alpha*alpha*(nd))/(nm))
      call fitcconj(e,m,gfp,p,e,gfp,nd,nm,idir,iter,r,varm)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     conj(

      subroutine fitcconj(d,m,fp,p,e,g,nd,nm,idir,iter,vard,varm)
c  conjugate gradient algorithm finds the m that inverts d = f m
cndxn conj( n
c   by minimizing (d - f m)* (d - f m)/vard + m* m/varm
c  f is a linear trasmormation of a model array m
c  d is the data array
c  asterisks indicate the transpose (adjoint) of a matrix mult
c  multiplication
c  vard is the variance of the error (noise)
c  varm is the variance of the model
c  use in a loop as follows:
c     vard = #
c     varm = #
c     nter = #
c     nd = #
c     nm = #
c     iter = 0
c     d = data
c     m = initial model (perhaps all zeros)
c  10 continue
c     call fitcconj(d,m,fp,p,e,g,nd,nm,idir,iter,vard,varm)
c     if(iter.gt.nter) goto 20
c     if(idir.eq.-1) g = f* e
c     if(idir.eq.1) fp = f p
c     goto 10
c  20 continue
c  set nter to a number of iterations greater than 0
c     nd and nm are the number of samples in d and m
c     can use data and error in same memory: destroys data
c     call fitcalso us fp and g in same memory
c if initial m is not setv then d must have subtracted non-setv
c  modeled data  d - fm
      implicit none
      integer nd,nm,idir,iter
      real d(nd),m(nm),fp(nd),p(nm),e(nd),g(nm),vard,varm
      real rm1,rm2,eps,r,beta,d1,d2,d3,d4,alpha
      data rm1 /1./
      data rm2 /1./
      data eps /1./
cif>
      if(iter.gt.0) goto 100
      call fitccopy(nd,d,e)
      call fitcsetv(nm,p,0.)
      rm1 = 1
      r = 1.e10
      call fitcdivm(eps,vard,varm,r)
      idir = -1
      iter = 1
      return
  100 continue
cif<
cif>
      if(idir.ne.-1) goto 200
      call fitchadd(g,g,-1.,m,eps,nm)
      call fitchdot(g,g,nm,rm2)
      call fitcdivm(beta,rm2,rm1,1.)
      call fitchadd(p,g,-1.,p,beta,nm)
      rm1 = rm2
      idir = 1
      return
  200 continue
c else>
      call fitchdot(fp,e,nd,d1)
      call fitchdot(p,m,nm,d2)
      call fitchdot(fp,fp,nd,d3)
      call fitchdot(p,p,nm,d4)
      d1 = d1*(nd)
      d2 = d2*(nm)
      d3 = d3*(nd)
      d4 = d4*(nm)
      alpha = 0.
c      if ((d3 + eps*d4).eq.0.) then
c      print*,' d1=',d1,' d2=',d2,' d3=',d3,' d4=',d4
c     1,' eps=',eps
c      print*,' alpha=',alpha
c      print*,' (d3 + eps*d4)=',(d3 + eps*d4)
c      print*,' (d1 - eps*d2)=',(d1 - eps*d2)
c      stop
c      endif
      if ((d3 + eps*d4) .ne. 0. .and. d4.ne.0) 
     1alpha = (d1 - eps*d2)/(d3 + eps*d4)
      call fitchadd(m,m,1.,p,alpha,nm)
      r = -alpha
      call fitchadd(e,e,1.,fp,r,nd)
      idir = -1
      iter = iter + 1
      return
cif<
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     copy(

      subroutine fitccopy(n,yin,yout)
c copy one array to another
cndxa copy( a
      implicit none
      integer n
      real yout(n),yin(n)
      integer i
c>
      do 10 i = 1,n
      yout(i) = yin(i)
   10 continue
c<
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     divm(

      subroutine fitcdivm(c, ar,br,rmaxr)
c divide arrays with clip = rmaxr, c = a/b
cndxa divm( a
c b or a can occupy same memory as c
      implicit none
      real c,ar,br,rmaxr
      real rmax,a,b,r,r2
      rmax = rmaxr
      a = ar
      b = br
cif>
      if(abs(a).lt.1.e10.or.abs(b).lt.1.e10) goto 5
      a = a/1.e10
      b = b/1.e10
cif >
      if(abs(a).lt.1.e10.or.abs(b).lt.1.e10) goto 4
      a = a/1.e10
      b = b/1.e10
    4 continue
cif <
    5 continue
cif<
      rmax = abs(rmax)
cif>
      r2 = rmax*abs(b)
      if(abs(a).ge.r2) goto 10
      r = a/b
c the following test will be true only if a and b are very small
      if(abs(r).gt.rmax) r = 0.
      goto 20
   10 continue
celse>
      r = rmax
      if((a.gt.0..and.b.lt.0.).or.(a.lt.0..and.b.gt.0.)) r = -r
   20 continue
cif<
      if(a.eq.0.) r = 0.
      c = r
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     hadd(

      subroutine fitchadd(c,a,ra,b,rb,n)
c add scaled arrays
cndxa hadd( a
      implicit none
      integer n
      real a(n),b(n),c(n),ra,rb
      integer i
c>
      do 10 i=1,n
   10 c(i) = ra*a(i) + rb*b(i)
c<
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     hdot(

      subroutine fitchdot(a,b,n,rdot)
c take dot product of arrays
cndxa hdot( a
      implicit none
      integer n
      real a(n),b(n),rdot
      real rn
      integer i
      rdot = 0.
      rn = 1./float(n)
      do 10 i =1,n
        rdot = rdot + a(i)*b(i)*rn
   10 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     heyu(

      subroutine fitcheyu(string)
      implicit none
      integer newdev
      character*(*) string
c write a message to error output; default is unit 6.
cndxa heyu( a
      integer n,idev
      data idev/6/
      n = len(string)
      if(n.gt.0) then
        n = max(1,min(80, n ))
        write(idev,1401) string(:n)
      endif
 1401 format(x,80a)
      return
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry fitcheyd(newdev)
c argument sets heyu() output to another device unit number
      idev = newdev
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     mnmx(

      subroutine fitcmnmx(rmin,rmax, a,na,ndim)
      implicit none
      integer na,ndim
      real rmin(ndim),rmax(ndim),a(ndim,na)
c find mins and maxs of multidimensional array, a(ndim,na), rmin(ndim)
cndxa mnmx( a
      integer idim,ia
      do 200 idim=1,ndim
        rmin(idim) = a(idim,1)
        rmax(idim) = a(idim,1)
        do 100 ia=1,na
          rmax(idim) = amax1(rmax(idim),a(idim,ia))
          rmin(idim) = amin1(rmin(idim),a(idim,ia))
  100     continue
  200   continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     put(

      subroutine fitcput(a,name)
c put a real, integer, or character to standard output, with label
cndxi put( i
      implicit none
      real a
      integer ia,ioff,newdev
      character*(*) name
      character*(*) ca
c
      integer loff,idev
      data loff/0/,idev/6/
        if(loff.eq.1) return
        write(idev,*) name,' ',a
      return
c................
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry fitcputc(ca,name)
        if(loff.eq.1) return
        write(idev,*) name,' ',ca
      return
c................
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry fitcputi(ia,name)
        if(loff.eq.1) return
        write(idev,*) name,' ',ia
      return
c................
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry fitcputo(ioff)
        loff = ioff
      return
c................
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry fitcputd(newdev)
        idev = newdev
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     setv(

      subroutine fitcsetv(n,x,x0)
c set an array of real numbers x(n) to x0.
cndxa setv( a
      implicit none
      integer n
      real x(n),x0
      integer i
c>
      do 10 i=1,n
        x(i) = x0
   10 continue
c<
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     hrnd(

      subroutine fitchrnd(a, n)
      implicit none
      integer n
      real a(n)
c fill an array with random numbers
cndxr hrnd( r        910917
      integer i,iseed
      data iseed/-739073/
      do 100 i=1,n
        call fitcran1(a(i), iseed)
  100 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     ran1(

      subroutine fitcran1(value,idum)
c random number generator, initialize with idum=negative seed
cndxr ran1( r
      implicit none
      integer idum
      real value
      integer m1,ia1,ic1,m2,ia2,ic2,m3,ia3,ic3,ix1,ix2,ix3,iff,j
      real rm1,rm2,rm3
      real r(97)
      parameter (m1=259200,ia1=7141,ic1=54773,rm1=3.858024691358e-6)
      parameter (m2=134456,ia2=8121,ic2=28411,rm2=7.437377283275e-6)
      parameter (m3=243000,ia3=4561,ic3=51349)
cc      parameter (m1=259200,ia1=7141,ic1=54773,rm1=1./(m1))
cc      parameter (m2=134456,ia2=8121,ic2=28411,rm2=1./(m2))
      data iff /0/,r /97*0./,ix1/0./,ix2/0./,ix3/0./
      if (idum.lt.0.or.iff.eq.0) then
        iff=1
        ix1=mod(ic1-idum,m1)
        ix1=mod(ia1*ix1+ic1,m1)
        ix2=mod(ix1,m2)
        ix1=mod(ia1*ix1+ic1,m1)
        ix3=mod(ix1,m3)
        do 11 j=1,97
          ix1=mod(ia1*ix1+ic1,m1)
          ix2=mod(ia2*ix2+ic2,m2)
          r(j)=(float(ix1)+float(ix2)*rm2)*rm1
11      continue
        idum=1
      endif
      ix1=mod(ia1*ix1+ic1,m1)
      ix2=mod(ia2*ix2+ic2,m2)
      ix3=mod(ia3*ix3+ic3,m3)
      j=1+int((97*ix3)/m3)
      if(j.gt.97.or.j.lt.1) call fitcputi(j,'ran1 err')
      value=r(j)
      r(j)=(float(ix1)+float(ix2)*rm2)*rm1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
c     limt(

      subroutine fitclimt(n,x,xmin,xmax)
c  limit array x to the range xmin,xmax
      implicit none
      integer n
      real x(n),xmin,xmax
      integer i
      do 1 i = 1 , n
        x(i) = max(xmin,min(xmax,x(i)))
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcpnt1(i0,n1,i1,n2,i2)
      implicit none
      integer i0,n1,i1(1),n2,i2
      integer i
      n2 = 0
      do 1 i = 1 , n1
        if (n2 .eq. 0 .and. i0 .eq. i1(i)) then
          i2 = i
          n2 = 1
        elseif (n2 .ne. 0 .and. i0 .eq. i1(i)) then
          n2 = n2 + 1
        elseif (n2 .ne. 0 .and. i0 .ne. i1(i)) then
          goto 2
        endif
    1 continue
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcpnt2(nv,iv2,iv3,mz,nz,iy,ny,ix,nx)
      implicit none
      integer nv,iv2(1),iv3(1),mz,nz,iy(1),ny(1),ix(1),nx(1)
      integer jy,i,iv2l,iv3l

      jy = 0
      nz = 0
      iv2l = iv2(1) - 1
      do 1 i = 1 , nv
c      print'('' i='',i3,'' i2='',i3,1x,i2,'' i3='',i3,1x,i3)'
c     1,i,iv2(i),iv2l,iv3(i),iv3l
        if (iv2(i) .ne. iv2l) then
          nz = nz + 1
          iy(nz) = jy
          ny(nz) = 0
          iv3l = iv3(i) - 1
        endif
        if (iv3(i) .ne. iv3l) then
          ny(nz) = ny(nz) + 1
          jy = jy + 1
          ix(jy) = i - 1
          nx(jy) = 0
        endif
        nx(jy) = nx(jy) + 1
        iv2l = iv2(i)
        iv3l = iv3(i)
    1 continue

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitcwork(i,n)
c  assign work space within an array
      implicit none
      integer i,n,ierr
      integer icall,i0,m
      data icall/0/,i0/1/,m/0/
      i = i0
      i0 = i0 + n
      icall = icall + 1
c      write(6
c     1,'('' fitcwork icall='',i5,'' i='',i8,'' n='',i8,'' l='',i8
c     1,'' m='',i8)')icall,i,n,m-i+1,m
      if (i0-1 .gt. m) then
        write(6,'('' error in fitcwork icall='',i8,'' asking for'',i8
     1,/,'' need more work space have'',i8,'' and need'',i8)')
     1icall,n,m,i0
      endif
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry fitcwors(i,n)
      i0 = i
      m = n
c      write(6,'('' memory setup i='',i8,'' m='',i8)')i0,m
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry fitcworl(n)
      n = m - i0 + 1
c      write(6,'('' memory left i='',i8,'' m='',i8,'' n='',i8)')i0,m,n
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry fitcworc(ierr)
      ierr = 0
      if (i0-1 .gt. m) then
        write(6,'('' error in fitcworc memory - have='',i10
     1,'' used='',i10)')m,i0
        ierr = -1
      endif    ! if (i0-1 .gt. m) then
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine fitc_compute_error(n,x1,x2,error)
      implicit none
      integer  n,i
      real     x1(n),x2(n),error
      error = 0.
      do i = 1 , n
        error = error + (x1(i) - x2(i)) ** 2
      enddo
      if (error .ne. 0.) error = sqrt(error) / max(1,n) 
      return
      end

