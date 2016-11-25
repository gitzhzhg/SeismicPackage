c      subroutine ztot
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
C***************************** COPYRIGHT NOTICE ************************
C*
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION
C*                              OF CONOCO INC.
C*                      PROTECTED BY THE COPYRIGHT LAW
C*                          AS AN UNPUBLISHED WORK
C*
C***************************** COPYRIGHT NOTICE ************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C
CPrimitive name: RMOD
C        Author: D W Hanson
C       Written: 92/10/26
C  Last revised: 93/06/08 Hanson
C
C  Purpose:  Convert models from time to depth or depth to time
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C      CALL ZTOT(FILE1,FILE2,FILE3,m_work,WORK,LU,*)
C
C ARGUMENTS
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C FILE1    I     CHAR   INPUT MODEL FILE TO USE FOR DEPTH CONVERSION
C FILE2    I     CHAR   INPUT MODEL FILE TO CONVERT
C FILE3    I     CHAR   OUTPUT MODEL FILE
C m_work    I     INT    NUMBER OF WORDS OF WORKS SPACE AVAILABLE
C WORK     I     REAL   WORK ARRAY
C LU       I     INT    LOGICAL UNIT # TO PRINT INFO TO LU<0 - NO PRINT
C *        I            ERROR RETURN ADDRESS
C
C-----------------------------------------------------------------------
C                                 NOTES
C  NOTE 1
C
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author      Description
C     ----     ------      -----------
C 3.  93/06/08 Hanson      Fix Documentation
C 2.  93/04/06 Hanson      Simplify ZTOT call.
C 1.  92/10/26 Hanson      Original version taken from routine RMOD.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C
C   SUBROUTINE NAMES IN THIS MODULE
C ZTOT     ztot_time_to_depth_2d_p ztot_convert_depth_1 ZTOTTOUT ZTOTZOU
C ztot_convert_velocity_n ZTOT_convert_velocity_1 ztot_fill ztot_line zt
C
C   ENTRY NAMES IN THIS MODULE
C
C   FUNCTION NAMES IN THIS MODULE
C
C   COMMON BLOCK NAMES IN THIS MODULE
C
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C RMODCNMM RMODCNV0 RMODPCOR CELLWORS CELLWORK CELLWORL CELLCOPY CELLSET
C
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - NONE
C  HEAP(dynamic) - NONE
C-----------------------------------------------------------------------
C\END DOC
C23456789012345678901234567890123456789012345678901234567890123456789012
c  i_dir = 1 depth to time
c  i_dir = -1 time to depth
      subroutine ztot(czi,n_cord,x_cord,cord,z_min,z_max,z_datum
     1,nb,ixb,nxb,xb,zb,nv,ixv,nxv,xv,zv,ncv,xcv,zcv,nzl,zlmin,zlinc
     1,nx_grd,x0_grd,dx_grd,nz_grd,z0_grd,dz_grd,v_grd)
      implicit none
      integer n_cord,nb,ixb,nxb,nv,ixv,nxv,ncv,nzl,nx_grd,nz_grd
      real x_cord,z_min,z_max,z_datum,xb,zb,xv,zv,xcv,zcv,zlmin,zlinc
      real x0_grd,dx_grd,z0_grd,dz_grd,v_grd
      character *(*) czi,cord
      character czo*16
      integer i_dir

c  check input depth coordinates
      if (czi .eq. 'DEPTH') then
        czo = 'TIME'
        i_dir = 1
      elseif (czi .eq. 'TIME') then
        czo = 'DEPTH'
        i_dir = -1
      else
        czo = czi
        print'('' ztot doing nothing czi='',a16)',czi
        return
      endif

c  convert the layer boundaries
      call ztot_time_to_depth_2d_p(i_dir,z_datum,nb,ixb,nxb,xb,zb
     1,nx_grd,x0_grd,dx_grd,nz_grd,z0_grd,dz_grd,v_grd)

c  convert the velocity locations
      call ztot_time_to_depth_2d_p(i_dir,z_datum,nv,ixv,nxv,xv,zv
     1,nx_grd,x0_grd,dx_grd,nz_grd,z0_grd,dz_grd,v_grd)

c  convert the cell pointers
      call ztot_time_to_depth_2d_p(i_dir,z_datum,1,0,ncv,xcv,zcv
     1,nx_grd,x0_grd,dx_grd,nz_grd,z0_grd,dz_grd,v_grd)

c  shift limits by the datum level
c  i_dir = 1 depth to time
      if (i_dir .eq. +1) then
        z_min = z_min - z_datum
        z_max = z_max - z_datum
        zlmin = zlmin - z_datum
      endif    ! if (i_dir .eq. +1) then

c  convert the layered model limits
      call rmodcnmm(z_min,z_max,czi,czo,n_cord,x_cord,cord)

c  convert the gridded model limits
      call rmodcnv0(nzl,zlmin,zlinc,czi,czo,n_cord,x_cord,cord)

c  shift limits by the datum level
c  i_dir = -1 time to depth
      if (i_dir .eq. -1) then
        z_min = z_min + z_datum
        z_max = z_max + z_datum
        zlmin = zlmin + z_datum
      endif    ! if (i_dir .eq. -1) then

      czi = czo

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_time_to_depth_2d_p(i_dir,z_datum,n,ix,nx,x,z
     1,nx_grd,x0_grd,dx_grd,nz_grd,z0_grd,dz_grd,v_grd)
c  depth convert nb pointed vectors
      implicit none
      integer i_dir,n,ix(1),nx(1),nx_grd,nz_grd
      real z_datum,x(1),z(1),x0_grd,dx_grd,z0_grd,dz_grd,v_grd
      integer i,j

      do i = 1 , n

        do j = ix(i)+1 , ix(i)+nx(i)

          call ztot_time_to_depth(i_dir,1,x(j),0.,z(j),z(j),z_datum
     1,nx_grd,x0_grd,dx_grd,1,0.,1.,nz_grd,z0_grd,dz_grd,v_grd)

        enddo    ! do j = ix(i)+1 , ix(i)+nx(i)

      enddo    ! do i = 1 , n

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_compute_columns(x,x1,x2,ix1,ix2,fx1,fx2,nx_grd
     1,x0_grd
     1,dx_grd)
c  compute what two nodes a value x lies between
      implicit none
      integer ix1,ix2,nx_grd
      real x,x1,x2,fx1,fx2,x0_grd,dx_grd

      ix1 = max(1,min(nx_grd,int((x-x0_grd)/dx_grd)+1))
      ix2 = min(nx_grd,ix1+1)
      x1 = (ix1 - 1) * dx_grd + x0_grd
      if (x1 .gt. x) ix2 = ix1
      x2 = (ix2 - 1) * dx_grd + x0_grd
      fx2 = max(0.,min(1.,(x-x1)/dx_grd))
      fx1 = 1. - fx2
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_convert_velocity_n(czi
     1,n_cord,x_cord,cord
     1,z_datum
     1,nx_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd
     1,m_work,work
     1,*)
c  convert a gridded velocity from time to depth or depth to time
      implicit none
      integer n_cord,nx_grd,nz_grd,m_work
      real x_cord,z_datum,z0_grd,dz_grd,v_grd(nz_grd,1),work(1)
      character *(*) czi,cord
      character czo*16
      integer i_work_i,i_work_n
      integer nz,nt,i_dir,iv0,izt,nx,ix,i_err
      real z_min,z_inc,tmin,tinc
c  check input depth coordinates
      nz = nz_grd
      z_min = z0_grd
      z_inc = dz_grd
      nt = nz_grd
      tmin = z0_grd
      tinc = dz_grd
      if (czi .eq. 'DEPTH') then
        czo = 'TIME'
        call rmodcnv0(nt,tmin,tinc,czi,czo,n_cord,x_cord,cord)
        i_dir = 1
      elseif (czi .eq. 'TIME') then
        czo = 'DEPTH'
        call rmodcnv0(nz,z_min,z_inc,czi,czo,n_cord,x_cord,cord)
        i_dir = -1
      else
        print'('' ztotzcol doing nothing czi='',a16)',czi
        return
      endif
c      print'('' convert_velocity_n czi='',a16,'' czo='',a16)',czi,czo
c      print*,' nz_grd=',nz_grd,' z0_grd=',z0_grd,' dz_grd=',dz_grd
c      call rmodpcor(6,n_cord,x_cord,cord)
c      print*,' nt=',nt,' tmin=',tmin,' tinc=',tinc
c      print*,' nz=',nz,' z_min=',z_min,' z_inc=',z_inc
      nx = 1
      call util_wors(i_work_i,i_work_n,m_work)   ! setup work space
      call util_work(i_work_i,i_work_n,iv0,nz)  ! set work space to after velocity
      call util_work(i_work_i,i_work_n,izt,nz)  ! set work space to after velocity
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

      do 1 ix = 1 , nx_grd
c      print'(1x,i5,1x,f10.2,1x,i5)',(i,1./v_grd(i,1),2,i=1,nz_grd)
        if (i_dir .eq. 1) then
          call ztot_convert_velocity_1(i_dir,nx,z_datum
     1,nz,z_min,z_inc,v_grd(1,ix),nt,tmin,tinc,work(iv0),work(izt))
        else
          call ztot_convert_velocity_1(i_dir,nx,z_datum
     1,nz,z_min,z_inc,work(iv0),nt,tmin,tinc,v_grd(1,ix),work(izt))
        endif
        call util_copy(nz_grd,work(iv0),v_grd(1,ix))
    1 continue
c      print'(1x,i5,1x,f10.2,1x,i5)',(i,1./v_grd(i,1),2,i=1,nz_grd)
c      print*,' czzz'
      call rmodcnv0(nz_grd,z0_grd,dz_grd,czi,czo,n_cord,x_cord,cord)
      czi = czo

      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_convert_velocity_1(i_dir
     1,nx,z_datum
     1,nz,z_min,z_inc
     1,vz
     1,nt,tmin,tinc
     1,vt
     1,tz)
c  determine the velocity vt at nt,tmin,tinc from vz at nz,z_min,z_inc
c  starting at z_datum
      implicit none
      integer i_dir,nx,nz,nt
      real z_datum,z_min,z_inc,tmin,tinc,vz(nz,1),vt(nt,1),tz(1)
      integer icall,ix
      data icall/0/
      icall = icall + 1
c  if z_datum < z_min fill in vt
c      if (icall .eq. 1) then
c      print*,' ztot id=',i_dir,' nx=',nx,' nz=',nz,' nt=',nt
c     1,' zd=',z_datum
c      print*,' nz=',nz,z_min,z_inc
c      print*,' nt=',nt,tmin,tinc
c      print*,' vz=',vz(1,1),vz(nz,1)
c      print*,' vt=',vt(1,1),vt(nt,1)
c      endif
      do 1 ix = 1 , nx
        if (i_dir .eq. 1) then     ! z to t
          call ztot_compute_zt(i_dir,z_datum,nz,z_min,z_inc,vz(1,ix),tz)
          call ztot_fill(nz,tz,vz(1,ix),nt,tmin,tinc,vt(1,ix))
        else
          call ztot_compute_zt(i_dir,z_datum,nt,tmin,tinc,vt(1,ix),tz)
          call ztot_fill(nt,tz,vt(1,ix),nz,z_min,z_inc,vz(1,ix))
        endif
    1 continue
c      if(icall.eq.1)print*,' vz=',vz(1,1),vz(nz,1)
c      if(icall.eq.1)print*,' vt=',vt(1,1),vt(nt,1)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_fill(nz,tz,vz,nt,tmin,tinc,vt)
      implicit none
      integer nz,nt
      real tmin,tinc,tz(1),vz(1),vt(1)
      integer icall,itmin,itmax,it1,it2,iz
      real t1,v0,dvdt
      data icall/0/
      icall = icall + 1
      itmin = nt
      itmax = 1
      it1 = max(int((tz(1)-tmin)/tinc)+1,1)
c      if (icall.eq.1)then
c      print*,' ztot_fill nz=',nz,' nt=',nt,' tmin=',tmin,' tinc=',tinc
c      print*,' it1=',it1
c      endif
      do 1 iz = 1 , nz-1
        it2 = min(int((tz(iz+1)-tmin)/tinc)+1,nt)
        if (tz(iz) .ne. tz(iz+1)) then
          dvdt = (vz(iz+1) - vz(iz)) / (tz(iz+1) - tz(iz))
          t1 = (it1 - 1) * tinc + tmin
          v0 = vz(iz) + (t1 - tz(iz)) * dvdt
c      if(icall.eq.1)print*,' iz=',iz,' it1=',it1,' it2=',it2
c     1,' tz=',tz(iz),tz(iz+1),' t1=',t1
          call ztot_line(it2-it1+1,vt(it1),v0,dvdt*tinc,vz(iz),vz(iz+1))
c      if(icall.eq.1)print*,' vz=',vz(iz),vz(iz+1)
c     1,' vt=',vt(it1),vt(it2)
          itmin = min(itmin,it1)
          itmax = max(itmax,it2)
          it1 = max(it1,it2+1)
        endif
    1 continue
c      if(icall.eq.1)print*,' itmin',itmin,' itmax=',itmax
c     1,' vz=',1./vz(1),1./vz(nz)
      call util_setr(itmin-1,vt,vz)
      call util_setr(nt-itmax,vt(min(nt,itmax+1)),vz(nz))
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_compute_zt(i_dir,z_datum,ng,g0,dg,v,g)
c  compute the vertical travel time to each depth location i_dir=1
c  or the depth to each travel time i_dir=-1
c  v is slowness
      implicit none
      integer i_dir,ng
      real z_datum,g0,dg,v(1),g(1)
      integer icall,ig1,ig2,ig
      real t0,z0,g1,g2
      data icall/0/
      icall = icall + 1

c  determine the 2 nodes immediately above and below t0
      t0 = 0.
      z0 = z_datum

c      if (i_dir .eq. 1) then    ! determine depth as a function of time
c        t0 = z_datum
c        z0 = 0.
c      else
c        t0 = 0.
c        z0 = z_datum
c      endif


      if (i_dir .eq. 1) then    ! time to depth point

c  g will be two way time
      ig1 = int((z0-g0)/dg) + 1
      g1 = (ig1 - 1) * dg + g0
      if (g1 .gt. t0) ig1 = ig1 - 1
      ig1 = max(1,min(ng,ig1))
      ig2 = min(ng,ig1+1)
      g1 = (ig1 - 1) * dg + g0
      if (g1 .eq. t0) ig2 = ig1
      g2 = (ig2 - 1) * dg + g0
      g(ig1) = t0 + (g1 - z0) * (v(ig1) + v(ig2))
      g(ig2) = t0 + (g2 - z0) * (v(ig1) + v(ig2))

        do ig = ig2+1 , ng
          g(ig) = g(ig-1) + dg * (v(ig) + v(ig-1))
        enddo    ! do ig = ig2+1 , ng
        do ig = ig1-1 , 1 , -1
          g(ig) = g(ig+1) - dg * (v(ig) + v(ig+1))
        enddo    ! do ig = ig1-1 , 1 , -1

      else    ! depth to time point

c g will be depth
      ig1 = int((t0-g0)/dg) + 1
      g1 = (ig1 - 1) * dg + g0
      if (g1 .gt. t0) ig1 = ig1 - 1
      ig1 = max(1,min(ng,ig1))
      ig2 = min(ng,ig1+1)
      g1 = (ig1 - 1) * dg + g0
      if (g1 .eq. t0) ig2 = ig1
      g2 = (ig2 - 1) * dg + g0
      g(ig1) = z0 + (g1 - t0) / (v(ig1) + v(ig2))
      g(ig2) = z0 + (g2 - t0) / (v(ig1) + v(ig2))

        do ig = ig2+1 , ng
          g(ig) = g(ig-1) + dg / (v(ig) + v(ig-1))
        enddo    ! do ig = ig2+1 , ng
        do ig = ig1-1 , 1 , -1
          g(ig) = g(ig+1) - dg / (v(ig) + v(ig+1))
        enddo    ! do ig = ig1-1 , 1 , -1

      endif    ! if (i_dir .eq. 1) then    ! time to depth point

c      if(icall.eq.1)then
c      print*,' i_dir=',i_dir,' ng=',ng,' g0=',g0,' dg=',dg
c     1,' z_datum=',z_datum
c      print'(1x,f12.4,1x,f12.4,1x,i5,1x,f12.4)'
c     1,(g(ig),1./v(ig),1,(ig-1)*dg+g0,ig=1,ng)
c      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_line(n,x,x0,dx,x1,x2)
      implicit none
      integer n
      real x0,dx,x1,x2,x(1)
      integer i
      real x_min,x_max
      x_min = min(x1,x2)
      x_max = max(x1,x2)
      do i = 1 , n
        x(i) = max(x_min,min(x_max,(i-1)*dx+x0))
      enddo    ! do i = 1 , n
      return
      end
 
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_tofz(x_vec,y_vec,z_vec,t_vec
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel)
c  compute travel time t_vec from location z_vec to z_datum at x locatio
      implicit none

      real     x_vec,y_vec,z_vec,t_vec

      real     z_datum

      integer  nx_vel

      real     x0_vel,dx_vel
      integer  ny_vel

      real     y0_vel,dy_vel
      integer  nz_vel

      real     z0_vel,dz_vel
      real     vel(nz_vel,nx_vel,ny_vel)

      integer ix1,ix2
      real    x1,x2,fx1,fx2

      integer iy1,iy2
      real    y1,y2,fy1,fy2

      integer iz_vec,iz_datum

      real    t_vecs,dt_vec,dts

c  determine which two columns of vel x_vec falls between
      call ztot_compute_columns(x_vec,x1,x2,ix1,ix2,fx1,fx2,nx_vel
     1,x0_vel,dx_vel)
      call ztot_compute_columns(y_vec,y1,y2,iy1,iy2,fy1,fy2,ny_vel
     1,y0_vel,dy_vel)

c  compute travel time from receiver location to grid node iz_vec1
      call ztot_delt(dt_vec,z_vec,iz_vec
     1,nz_vel,z0_vel,dz_vel
     1,fx1*fy1,vel(1,ix1,iy1)
     1,fx2*fy1,vel(1,ix2,iy1)
     1,fx1*fy2,vel(1,ix1,iy2)
     1,fx2*fy2,vel(1,ix2,iy2))

c23456789012345678901234567890123456789012345678901234567890123456789012
c  compute travel time from receiver location to grid node iz_vec1
      call ztot_delt(dts,z_datum,iz_datum
     1,nz_vel,z0_vel,dz_vel
     1,fx1*fy1,vel(1,ix1,iy1)
     1,fx2*fy1,vel(1,ix2,iy1)
     1,fx1*fy2,vel(1,ix1,iy2)
     1,fx2*fy2,vel(1,ix2,iy2))

c  compute travel time from grid node iz_vec1 to grid node iz_datum1
      call ztot_tint(t_vecs,iz_vec,iz_datum
     1,nz_vel,z0_vel,dz_vel
     1,fx1*fy1,vel(1,ix1,iy1)
     1,fx2*fy1,vel(1,ix2,iy1)
     1,fx1*fy2,vel(1,ix1,iy2)
     1,fx2*fy2,vel(1,ix2,iy2))

c  total 2 way travel time t<0 if z_vec<z_datum
      t_vec = sign(1.,z_vec-z_datum) * t_vecs + dt_vec - dts

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_zoft(x_vec,y_vec,z_vec,t_vec
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel)
c  compute travel time t_vec from location z_vec to z_datum at x locatio
      implicit none

      real     x_vec,y_vec,z_vec,t_vec

      real     z_datum

      integer  nx_vel

      real     x0_vel,dx_vel
      integer  ny_vel

      real     y0_vel,dy_vel
      integer  nz_vel

      real     z0_vel,dz_vel
      real     vel(nz_vel,nx_vel,ny_vel)

c  compute travel time t_vec from location z_vec to z_datum at x locatio
      integer ix1,ix2
      real    x1,x2,fx1,fx2

      integer iy1,iy2
      real    y1,y2,fy1,fy2

      integer iz1,iz2
      real    z1,z2

      integer iv1,iv2
      real    v1,v2

      integer iz_inc,iz_vec,iz_datum
      real    t1,t2,t_vecs,t0,dt_vec,dts


c  determine which two columns of vel x_vec falls between
      call ztot_compute_columns(x_vec,x1,x2,ix1,ix2,fx1,fx2,nx_vel
     1,x0_vel,dx_vel)
      call ztot_compute_columns(y_vec,y1,y2,iy1,iy2,fy1,fy2,ny_vel
     1,y0_vel,dy_vel)

      iz_inc = nint(sign(1.,t_vec))
      iz1 = int((z_datum-z0_vel)/dz_vel) + 1
      z1 = (iz1 - 1) * dz_vel + z0_vel
      if (z1 .gt. z_datum) iz1 = iz1 - 1
      if (iz_inc .lt. 0) iz1 = iz1 + 1
      iz2 = iz1 + iz_inc
      if (iz_inc .lt. 0 .and. iz2 .gt. nz_vel) iz2 = nz_vel
      if (iz_inc .gt. 0 .and. iz2 .lt. 1) iz2 = 1

      z1 = z_datum
      z2 = (iz2 - 1) * dz_vel + z0_vel
      iv1 = max(1,min(nz_vel,iz1))
      iv2 = max(1,min(nz_vel,iz2))
      v1 = fx1 * fy1 * vel(iv1,ix1,iy1) + fx2 * fy1 * vel(iv1,ix2,iy1)
     1   + fx1 * fy2 * vel(iv1,ix1,iy2) + fx2 * fy2 * vel(iv1,ix2,iy2)
      v2 = fx1 * fy1 * vel(iv2,ix1,iy1) + fx2 * fy1 * vel(iv2,ix2,iy1)
     1   + fx1 * fy2 * vel(iv2,ix1,iy2) + fx2 * fy2 * vel(iv2,ix2,iy2)
      t1 = 0.
      t0 = abs(t_vec)

    1 continue
      t2 = t1 + abs(z2 - z1) * (v1 + v2)

      if (t0 .ge. t1 .and. t0 .le. t2) then

        z_vec = z1 + (z2 - z1) * (t0 - t1) / (t2 - t1)

      elseif ((iz_inc .lt. 0 .and. iz2 .le. 1)
     1   .or. (iz_inc .gt. 0 .and. iz2 .ge. nz_vel)) then

        z_vec = z2 + sign(1.,z2-z_datum) * .5 * (t0 - t2) / v2

      else

        iz1 = iz2
        z1 = z2
        iz2 = iz1 + iz_inc
        z2 = (iz2 - 1) * dz_vel + z0_vel
        iv2 = max(1,min(nz_vel,iz2))
        v1 = v2
      v2 = fx1 * fy1 * vel(iv2,ix1,iy1) + fx2 * fy1 * vel(iv2,ix2,iy1)
     1   + fx1 * fy2 * vel(iv2,ix1,iy2) + fx2 * fy2 * vel(iv2,ix2,iy2)
        t1 = t2
        goto 1
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_delt(dt,z,iz,nz_vel,z0_vel,dz_vel
     1,f_11,vel_11,f_21,vel_21,f_12,vel_12,f_22,vel_22)
c  compute closest grid node above z - compute time from grid node to z
c  note time is >0
      implicit none

      integer iz
      real    dt,z

      integer nz_vel
      real    z0_vel,dz_vel

      real    f_11,vel_11(nz_vel)
      real    f_21,vel_21(nz_vel)
      real    f_12,vel_12(nz_vel)
      real    f_22,vel_22(nz_vel)

      integer iv
      real    z1,v

      iz = int((z-z0_vel)/dz_vel) + 1
      z1 = (iz - 1) * dz_vel + z0_vel
      if (z1 .gt. z) then    ! vax rounds negative numbers up
        iz = iz - 1
        z1 = (iz - 1) * dz_vel + z0_vel
      endif
      iv = max(1,min(nz_vel,iz))
      dt = 2. * abs(z-z1)
     1* (f_11 * vel_11(iv) + f_21 * vel_21(iv)
     1 + f_12 * vel_12(iv) + f_22 * vel_22(iv))
      v = .5 / (f_11 * vel_11(iv) + f_21 * vel_21(iv))

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_tint(trs,izr,izs,nz_vel,z0_vel,dz_vel
     1,f_11,vel_11,f_21,vel_21,f_12,vel_12,f_22,vel_22)
c  compute 2 way travel time from node izs to izr - note t>0
      implicit none

      real    trs
      integer izr,izs

      integer nz_vel
      real    z0_vel,dz_vel

      real    f_11,vel_11(nz_vel)
      real    f_21,vel_21(nz_vel)
      real    f_12,vel_12(nz_vel)
      real    f_22,vel_22(nz_vel)

      integer iz1,iz2,i
      real    dt,z

      iz1 = min(izr,izs)
      iz2 = max(izr,izs)

c23456789012345678901234567890123456789012345678901234567890123456789012
c  include portion outside grid limits
      trs = 2. *
     1(
     1   ( max(0,  1-   iz1) - max(0,  1-   iz2) )
     1 * ( f_11 * vel_11(     1) + f_21 * vel_21(     1)
     1   + f_12 * vel_12(     1) + f_22 * vel_22(     1) )
     1 + ( max(0,iz2-nz_vel) - max(0,iz1-nz_vel) )
     1 * ( f_11 * vel_11(nz_vel) + f_21 * vel_21(nz_vel)
     1   + f_12 * vel_12(nz_vel) + f_22 * vel_22(nz_vel) )
     1 )

      do i = max(iz1,1) , min(nz_vel,iz2)-1

        trs = trs
     1 + f_11 * (vel_11(i) + vel_11(i+1))
     1 + f_21 * (vel_21(i) + vel_21(i+1))
     1 + f_12 * (vel_12(i) + vel_12(i+1))
     1 + f_22 * (vel_22(i) + vel_22(i+1))

      enddo    ! do i = max(iz1,1) , min(nz_vel,iz2)-1

      trs = trs * dz_vel

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_time_to_depth_g3dl(czi,n_cord,x_cord,cord
     1,zlmin,zlmax,z_datum
     1,ncv,xcv,ycv,zcv,nxv,xv,yv,zv
     1,nx,x_min,xinc,ny,y_min,yinc,nz,z_min,z_inc,z,t
     1,nx_grd,x0_grd,dx_grd,ny_grd,y0_grd,dy_grd,nz_grd,z0_grd,dz_grd
     1,v_grd)
c  i_dir = 1 depth to time
c  i_dir = -1 time to depth
      implicit  none
      character czi*(*),cord*(*)
      integer   n_cord
      real      x_cord

      integer   ncv
      real      xcv(1),ycv(1),zcv(1)
      integer   nxv
      real      xv(1),yv(1),zv(1)

      integer   nx
      real      x_min,xinc
      integer   ny
      real      y_min,yinc
      integer   nz
      real      z_min,z_inc
      real      zlmin,zlmax
      real      z(nx,ny,nz),t(nx,ny,nz)
      real      z_datum

      integer   nx_grd
      real      x0_grd,dx_grd
      integer   ny_grd
      real      y0_grd,dy_grd
      integer   nz_grd
      real      z0_grd,dz_grd
      real      v_grd(nz_grd,nx_grd,ny_grd)

      character czo*16
      integer   i_dir,ix,iy,iz
      real      x,y

c  check input depth coordinates
      call rmodcaps(czi,czi)
      if (czi .eq. 'DEPTH') then
        czo = 'TIME'
        i_dir = 1
      elseif (czi .eq. 'TIME') then
        czo = 'DEPTH'
        i_dir = -1
      else
        czo = czi
        print'('' ztot doing nothing czi='',a16)',czi
        return
      endif

      do iy = 1 , ny
        y = (iy - 1) * yinc + y_min
        do ix = 1 , nx
          x = (ix - 1) * xinc + x_min
          do iz = 1 , nz
            call ztot_time_to_depth(i_dir
     1,1,x,y,z(ix,iy,iz),t(ix,iy,iz),z_datum
     1,nx_grd,x0_grd,dx_grd,ny_grd,y0_grd,dy_grd,nz_grd,z0_grd,dz_grd
     1,v_grd)
          enddo    ! iz = 1 , nz
        enddo    ! do ix = 1 , nx
      enddo    ! do iy = 1 , ny

c  convert the velocity locations
      call ztot_time_to_depth(i_dir,nxv,xv,yv,zv,zv,z_datum
     1,nx_grd,x0_grd,dx_grd,ny_grd,y0_grd,dy_grd,nz_grd,z0_grd,dz_grd
     1,v_grd)

c  convert the cell pointers
      call ztot_time_to_depth(i_dir,ncv,xcv,ycv,zcv,zcv,z_datum
     1,nx_grd,x0_grd,dx_grd,ny_grd,y0_grd,dy_grd,nz_grd,z0_grd,dz_grd
     1,v_grd)

c  convert the layered model limits
      call rmodcnmm(zlmin,zlmax,czi,czo,n_cord,x_cord,cord)

c  convert the gridded model limits
      call rmodcnv0(nz,z_min,z_inc,czi,czo,n_cord,x_cord,cord)

      czi = czo

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_time_to_depth(i_dir
     1,n_vec,x_vec,y_vec,z_vec,t_vec
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel)

c  i_dir = 1 depth to time
c  i_dir = -1 time to depth

      implicit none
      integer  i_dir

      integer  n_vec
      real     x_vec(1),y_vec(1),z_vec(1),t_vec(1)

      real     z_datum

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vel(nz_vel,nx_vel,ny_vel)

      integer  i_vec

      if (iabs(i_dir) .ne. 1) then
        print'('' error ztot_time_to_depth''
     1,'' i_dir='',i5,'' must be -1,+1'')',i_dir
        return
      endif

c  convert each x,y,z,t
      do i_vec = 1 , n_vec

        if (i_dir .eq. 1) then

          call ztot_tofz(
     1 x_vec(i_vec),y_vec(i_vec),z_vec(i_vec),t_vec(i_vec)
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel)

        else    ! if (i_dir .eq. 1) then

          call ztot_zoft(
     1 x_vec(i_vec),y_vec(i_vec),z_vec(i_vec),t_vec(i_vec)
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel)

        endif    ! if (i_dir .eq. 1) then

      enddo

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_convert_trace_n(
     1 czi
     1,n_cord,x_cord,cord
     1,z_datum
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd
     1,nx_con,x0_con,dx_con
     1,ny_con,y0_con,dy_con
     1,nz_con,z0_con,dz_con
     1,v_con
     1,m_work,work
     1,*)
c  convert a gridded velocity from time to depth or depth to time
      implicit  none

      character czi*(*)

      integer   n_cord
      real      x_cord
      character cord*(*)

      real      z_datum

      integer   nx_grd
      real      x0_grd,dx_grd

      integer   ny_grd
      real      y0_grd,dy_grd

      integer   nz_grd
      real      z0_grd,dz_grd

      real      v_grd(nz_grd,nx_grd,ny_grd)

      integer   nx_con
      real      x0_con,dx_con

      integer   ny_con
      real      y0_con,dy_con

      integer   nz_con
      real      z0_con,dz_con

      real      v_con(nz_con,nx_con,ny_con)

      integer   m_work
      real      work(m_work)

      integer    i_err

      integer    i_work_i,i_work_n
      integer    i_dir,ivc,iv0,izt
      integer    ix_grd,iy_grd
      real       x_grd,y_grd

      integer    nz
      real       z_min,z_inc

      integer    nt
      real       t_min,t_inc

      character czo*16

c  check input depth coordinates
      nz = nz_grd
      z_min = z0_grd
      z_inc = dz_grd
      nt = nz_grd
      t_min = z0_grd
      t_inc = dz_grd

      if (czi .eq. 'DEPTH') then
        czo = 'TIME'
        call rmodcnv0(nt,t_min,t_inc,czi,czo,n_cord,x_cord,cord)
        i_dir = 1
      elseif (czi .eq. 'TIME') then
        czo = 'DEPTH'
        call rmodcnv0(nz,z_min,z_inc,czi,czo,n_cord,x_cord,cord)
        i_dir = -1
      else
        print'('' ztotzcol doing nothing czi='',a16)',czi
        return
      endif
c      print'('' convert_trace_n czi='',a16,'' czo='',a16)',czi,czo
c      print*,' nz_grd=',nz_grd,' z0_grd=',z0_grd,' dz_grd=',dz_grd
c      call rmodpcor(6,n_cord,x_cord,cord)
c      print*,' nt=',nt,' t_min=',t_min,' t_inc=',t_inc
c      print*,' nz=',nz,' z_min=',z_min,' z_inc=',z_inc

      call util_wors(i_work_i,i_work_n,m_work)   ! setup work space
      call util_work(i_work_i,i_work_n,ivc,nz)  ! set work space to after velocity
      call util_work(i_work_i,i_work_n,iv0,nz)  ! set work space to after velocity
      call util_work(i_work_i,i_work_n,izt,nz)  ! set work space to after velocity
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

      do iy_grd = 1 , ny_grd

        y_grd = (iy_grd - 1) * dy_grd + y0_grd

        do ix_grd = 1 , nx_grd

          x_grd = (ix_grd - 1) * dx_grd + x0_grd

c      print'(1x,i5,1x,f10.2,1x,i5)',(i,1./v_grd(i,1),2,i=1,nz_grd)

c depth to time
        if (i_dir .eq. 1) then

c  interpolate the conversion velocity to this x,y location and this z grid
          call util_interpolate_3d(
     1 nz_con,z0_con,dz_con
     1,nx_con,x0_con,dx_con
     1,ny_con,y0_con,dy_con
     1,v_con
     1,nz,z_min,z_inc
     1,1,x_grd,dx_grd
     1,1,y_grd,dy_grd
     1,work(ivc))

          call ztot_convert_trace_1(
     1 i_dir,z_datum
     1,nz,z_min,z_inc,v_grd(1,ix_grd,iy_grd)
     1,nt,t_min,t_inc,work(iv0)
     1,work(ivc),work(izt))

c time to depth
        else

          call util_interpolate_3d(
     1 nz_con,z0_con,dz_con
     1,nx_con,x0_con,dx_con
     1,ny_con,y0_con,dy_con
     1,v_con
     1,nt,t_min,t_inc
     1,1,x_grd,dx_grd
     1,1,y_grd,dy_grd
     1,work(ivc))

          call ztot_convert_trace_1(
     1 i_dir,z_datum
     1,nz,z_min,z_inc,work(iv0)
     1,nt,t_min,t_inc,v_grd(1,ix_grd,iy_grd)
     1,work(ivc),work(izt))

        endif

        call util_copy(nz_grd,work(iv0),v_grd(1,ix_grd,iy_grd))

        enddo    ! do ix_grd = 1 , nx_grd

      enddo    ! do iy_grd = 1 , ny_grd

c      print'(1x,i5,1x,f10.2,1x,i5)',(i,1./v_grd(i,1),2,i=1,nz_grd)
c      print*,' czzz'

      call rmodcnv0(nz_grd,z0_grd,dz_grd,czi,czo,n_cord,x_cord,cord)
      czi = czo

      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ztot_convert_trace_1(
     1 i_dir,z_datum
     1,nz,z_min,z_inc
     1,xz
     1,nt,t_min,t_inc
     1,xt
     1,v_con,tz)
c  determine the velocity trace at nt,t_min,t_inc from xz at nz,z_min,z_inc
c  starting at z_datum
      implicit  none
      integer   i_dir
      real      z_datum
      integer   nz
      real      z_min,z_inc
      integer   nt
      real      t_min,t_inc
      real      xz(nz)
      real      xt(nt)
      real      v_con(*)
      real      tz(*)

      if (i_dir .eq. 1) then     ! z to t
c  compute the time to each depth point
        call ztot_compute_zt(i_dir,z_datum,nz,z_min,z_inc,v_con,tz)
        call ztot_fill(nz,tz,xz,nt,t_min,t_inc,xt)
      else
        call ztot_compute_zt(i_dir,z_datum,nt,t_min,t_inc,v_con,tz)
        call ztot_fill(nt,tz,xt,nz,z_min,z_inc,xz)
      endif

      return
      end

