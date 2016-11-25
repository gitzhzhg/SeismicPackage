C23456789012345678901234567890123456789012345678901234567890123456789012
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
c      subroutine gtol
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
CPrimitive name: GTOL
C        Author: D W Hanson
C       Written: 92/10/26
C  Last revised: 93/06/08 Hanson
C
C  Purpose:  Convert models from layer to grid or grid to layer
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C      CALL GTOL(I_DIR
C     1,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL,NC,IMC,IXC,NXC,XC,ZC
C     1,NX_GRD,X0_GRD,DX_GRD
C     1,NZ_GRD,Z0_GRD,DZ_GRD
C     1,V_GRD
C     1,M_WORK,WORK
C     1,I_ERR)
C
C ARGUMENTS
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C I_DIR    I    -1,+1   TRANSFORM DIRECTION +1 = LAYER TO GRID
C                                           -1 = GRID TO LAYER
C
C  CELL VELOCITY DEFINITON
C  ARRAYS IXV,NXV ARE DIMENSIONED NV
C  ARRAYS ITV,XV,ZV,VEL ARE DIMENSION IXV(NV)+NXV(NV)
C NV       O     INT    # OF VELOCITY DEFINITIONS
C IMV      O     ARRAY  VELOCITY ID NUMBER
C ITV      O     ARRAY  VELOCITY HORIZON NUMBER
C IXV      O     ARRAY  POINTER ARRAY FOR XV,ZV,VEL
C NXV      O     ARRAY  # OF X,Z,VEL POINTS IN EACH VELOCITY DEFINITION
C     DEFINITION I OCCUPIES ELEMENTS IXV(I)+1 - IXV(I)+NXV(I) OF XV,ZV,V
C XV       O     ARRAY  VELOCITY DEFINITION X VALUES
C ZV       O     ARRAY  VELOCITY DEFINITION Z VALUES
C VEL      O     ARRAY  VELOCITY VALUES
C
C  CELL BOUNDARY DESRIPTION
C  ARRAYS IMC,IXC,NXC ARE DIMENSIONED NC
C  ARRAYS XC,ZC ARE DIMENSION IXC(NC)+NXC(NC)
C NC       O     INT    # OF CELLS IN MODEL
C IMC      O     ARRAY  CELL VELOCITY TYPE
C                       CELL IC USES THE VELOCITY IV
C                       FOR WHICH IVC(IV) = IMC(IC)
C IXC      O     ARRAY  CELL POINTER WITHIN X,Z ARRAYS
C NXC      O     ARRAY  # OF POINTS IN EACH CELL
C     CELL I OCCUPIES ELEMENTS IXC(I)+1 - IXC(I)+NXC(I) OF XC ETC.
C XC       O     ARRAY  CELL X VALUES
C ZC       O     ARRAY  CELL Z VALUES
C                                           -1 = GRID TO LAYER
C  GRIDDED VELOCITY DEFINTION
C NX_GRD    O     INT>0  # OF X VLAUES IN GRIDDED FILE
C X0_GRD    O     REAL   MINIMUM X VLAUE IN GRIDDED FILE
C DX_GRD    O     REAL   X VLAUE SPACING IN GRIDDED FILE
C NZ_GRD    O     INT>0  # OF Z VLAUES IN GRIDDED FILE
C Z0_GRD    O     REAL   MINIMUM Z VLAUE IN GRIDDED FILE
C DZ_GRD    O     REAL   Z VLAUE SPACING IN GRIDDED FILE
C V_GRD     O     ARRAY  GRIDDED SLOWNESS DIMENSION (NZ,NX)
C
C M_WORK    I     INT    NUMBER OF WORDS OF WORKS SPACE AVAILABLE
C WORK      I     REAL   WORK ARRAY
C I_ERR     I     INT    ERROR FLAG I_ERR>0 MEANS ERROR DURING CALCULATION
C
C-----------------------------------------------------------------------
C                                 NOTES
C  NOTE 1
C  GTOL (Grid TO Layer) converts layered depth migration models
C  into gridded models and the reverse.
C
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author      Description
C     ----     ------      -----------
C 13. 93/06/08 Hanson      FIX DOCUMENTATION
C 12. 93/05/12 Hanson      Add sub interpolation for cell to grid calc.
C 11. 93/04/06 Hanson      Simplify GTOL call
C 10. 93/02/09 Hanson      Change GTOL call
C 9.  93/01/08 Hanson      Change CELLCLVP call
C 8.  92/12/11 Hanson      Adopt FITCELL velocity conventions.
C 7.  92/12/02 Hanson      Fix bug in IVRT call in FITC
C 6.  92/12/01 Hanson      Add GTOLQUAV for velocity type5.
C 5.  92/11/24 Hanson      IMV<0 means linear change in velocity.
C 4.  92/11/20 Hanson      Fix NXZ counter in GTOL_grid_to_layer.
C 3.  92/11/10 Hanson      Add GTOLTRIV for velocity type 4
C 2.  92/10/28 Hanson      Adopt linear slowness convention
C 1.  92/10/26 Hanson      Original version taken from routine RMOD.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C
C   SUBROUTINE NAMES IN THIS MODULE
C GTOL     GTOLVSUM GTOL_GRID_TO_LAYER GTOL_GRID_TO_CELL GTOL_FITCELL GT
C GTOL_FILL_VELOCITY GTOL_FILL_VELOCITY_  GTOL_FILL_VELOCITY_3 GTOL_CHEC
c GTOL_FIT_VELOCITY
C
C   ENTRY NAMES IN THIS MODULE
C GTOL_PUT_NZ_TERP GTOL_GET_NZ_TERP
C
C   FUNCTION NAMES IN THIS MODULE
C
C   COMMON BLOCK NAMES IN THIS MODULE
C
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C CELLCHP6 CELLMAX1 CELLMAX2 CELLPRNV CELLMNMX CELLNCRS FITCELL
C
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - NONE
C  HEAP(dynamic) - NONE
C-----------------------------------------------------------------------
C\END DOC
C  I_DIR=1 CELL TO GRID I_DIR=-1 GRID TO CELL
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol(i_dir
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,nc,imc,ixc,nxc,xc,zc
     1,nx_grd,x0_grd,dx_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd
     1,m_work,work
     1,i_err)

      implicit none

      integer i_dir

      integer nv,imv(1),itv(1),ixv(1),nxv(1)
      real    xv(1),zv(1),vel(1)

      integer nc,imc,ixc,nxc
      real    xc(1),zc(1)

      integer nx_grd
      real    x0_grd,dx_grd

      integer nz_grd
      real    z0_grd,dz_grd

      real    v_grd(nz_grd,nx_grd)

      integer m_work
      real    work(m_work)

      integer i_err

      integer lu

      integer nx_inc,nx_grd0,nx_inc0,nxc1
      integer nz_inc,nz_grd0,nz_inc0

      integer ivw1,ivw2,iw
      integer n_work

      integer ix_grd,jxg,iv,iv1
      integer itop,ntop,ibot,nbot

      real    xg,xc1,xc2,dxg
      real    zc1,zc2,dzg

      real    xmin,xmax
      real    zmin,zmax

      integer i_work_i,i_work_n

      data    nx_inc,nz_inc/5,5/

      i_err = 0

      call gtol_get_lu(lu)

      if (i_dir.lt.0 .and. lu.ge.0) write(lu,'(
     1 /,'' Fitting gridded velocities to layer velocity''
     1,/,'' number of velocity definitions ='',i8
     1,/,'' number of cells                ='',i8)')nv,nc

      if (i_dir.gt.0 .and. lu.ge.0) write(lu,'(
     1 /,'' Fitting layered velocities to gridded velocity''
     1,/,'' number of velocity definitions ='',i8
     1,/,'' number of cells                ='',i8)')nv,nc

      if (i_dir .eq. 1) then    ! from cell to grid

c  if creating grid from cells
c  do one x at a time to minimize memory recquirements
c  do finer z grid then avearage over fine grid to apprixmate
c  velocity distribution better

        call util_setr(nx_grd*nz_grd,v_grd,0.)
        call cellmax1(nc,ixc,nxc,xc,xc1,xc2)
        call cellmax1(nc,ixc,nxc,zc,zc1,zc2)
        dxg = (xc2 - xc1) / 1e6
        xc1 = min(xc2,xc1+dxg)
        xc2 = max(xc1,xc2-dxg)

        nz_grd0 = (nz_grd - 1 ) * nz_inc + 1
        dzg = dz_grd / nz_inc

        itop = max(1,min(nz_grd0,int((zc1-z0_grd)/dzg)+1))
        ntop = itop - 1
        ibot = max(1,min(nz_grd0,int((zc2-z0_grd)/dzg)+1))
        nbot = nz_grd0 - ibot

        call util_wors(i_work_i,i_work_n,m_work)
        call util_work(i_work_i,i_work_n,ivw1,nz_grd0)
        call util_work(i_work_i,i_work_n,ivw2,nz_grd0)
        call util_worl(i_work_i,i_work_n,n_work)
        call util_work(i_work_i,i_work_n,iw,n_work)
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 999

        do ix_grd = 1 , nx_grd     ! for each x value

          call util_setr(nz_grd0,work(ivw2),0.)

          do jxg = -nx_inc/2 , nx_inc/2

            xg = max(xc1,min(xc2
     1,(ix_grd-1)*dx_grd+x0_grd+jxg*(dx_grd/max(1,nx_inc-1))))

            call util_setr(nz_grd0,work(ivw1),0.)

c      print*,' ix_grd=',ix_grd,' xg=',xg
            do iv = 1 , nv

c      print*,' iv=',iv,' iv1=',iv1
c     1,' vel=',vel(iv1),vel(iv1+nxv(iv)-1)
              iv1 = ixv(iv) + 1

              call gtol_grid_to_layer(i_dir
     1,imv(iv),itv(iv1),nxv(iv),xv(iv1),zv(iv1),vel(iv1)
     1,nc,imc,ixc,nxc,xc,zc
     1,      1,    xg,dx_grd,nz_grd0
     1,nz_grd0,z0_grd,   dzg,      1
     1,work(ivw1)
     1,n_work,work(iw)
     1,*999)
          enddo    ! do iv = 1 , nv

          call gtol_add_sum(nz_grd0,work(ivw1),work(ivw2))

          enddo    ! do jxg = -nx_inc/2 , nx_inc/2

c  reset those grid points which lie above or below the layered model li
          call util_setr(ntop,work(ivw2+itop-1),work(ivw2))
          call util_setr(nbot,work(ivw2+ibot),work(ivw2+ibot-1))

          call gtol_ave_sum(1./nx_inc,nz_grd0,nz_inc,work(ivw2),v_grd(1
     1,ix_grd))
      if (nx_inc .ne. 1 .or. nz_inc .ne. 1)
     1call gtol_fill_velocity_1(1,nz_grd,v_grd(1,ix_grd),*999) ! v_grd i

c      do iv=1,nz_grd0,10
c      print*,' iz=',iv,' v_grd=',v_grd(iv,ix_grd)
c      enddo

        enddo    ! do ix_grd = 1 , nx_grd     ! for each x value

      else    ! if (i_dir .eq. 1) then    ! from grid to cell

        do iv = 1 , nv

          iv1 = ixv(iv) + 1

          call gtol_grid_to_layer(i_dir
     1,imv(iv),itv(iv1),nxv(iv),xv(iv1),zv(iv1),vel(iv1)
     1,nc,imc,ixc,nxc,xc,zc
     1,nx_grd,x0_grd,dx_grd,nz_grd
     1,nz_grd,z0_grd,dz_grd,1
     1,v_grd
     1,m_work,work
     1,*999)

        enddo    ! do iv = 1 , nv

      endif    ! if (i_dir .eq. 1) then

      return

  999 continue
      print'('' error in gtol'')'
      call cellmax2(nc,ixc,nxc,xc,zc,xmin,xmax,zmin,zmax)
c      call rmodwplt(xmin,xmax,zmin,zmax,nc,ixc,nxc,xc,zc,80)
      call cellprnv(' gtol',nc,imc,xc,zc,nv,imv,itv,ixv,nxv,xv,zv,vel)
      i_err = 1

      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry gtol_put_nz_terp(nz_inc0)
      nz_inc = max(1,nz_inc0)
      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry gtol_get_nz_terp(nz_inc0)
      nz_inc0 = nz_inc
      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry gtol_put_nx_terp(nx_inc0)
      nx_inc = max(1,nx_inc0)
      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry gtol_get_nx_terp(nx_inc0)
      nx_inc0 = nx_inc
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_add_sum(n,x1,x2)
c  sum x1 into x2
      implicit none
      integer n,i
      real x1(1),x2(1)
      do i = 1 , n
        x2(i) = x2(i) + x1(i)
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_ave_sum(scal,n1,ni,v1,v2)
c  average ni points
      implicit none
      integer n1,ni,n2,i1,i2,j1,j2,ni2,i1a,i1b
      real scal,v1(1),v2(1)

      if (ni .eq. 1) then

        call util_copy(n1,v1,v2)

      else

        n2 = (n1 - 1 ) / ni + 1
        ni2 = ni / 2 + 1
        do i2 = 1 , n2
          i1 = (i2 - 1) * ni + 1
          i1a = max(i1-ni2, 1)
          i1b = min(i1+ni2,n1)
          v2(i2) = 0.

          do j1 = i1a , i1b
            v2(i2) = v2(i2) + v1(j1)
          enddo    ! do j1 = i1a , i1b

          v2(i2) = scal * v2(i2) / (i1b-i1a+1)

        enddo    ! do i2 = 1 , n2

      endif


        n2 = (n1 - 1 ) / ni + 1
        ni2 = ni / 2 + 1

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_grid_to_layer(i_dir
     1,imv,itv,nxv,xv,zv,vel
     1,nc,imc,ixc,nxc,xc,zc
     1,nx_grd,x0_grd,dx_grd,nx_grdinc
     1,nz_grd,z0_grd,dz_grd,nz_grdinc
     1,v_grd
     1,m_work,work
     1,*)
c  i_dir=1 cell to grid i_dir=-1 grid to cell
      implicit none

      integer i_dir

      integer imv,itv,nxv
      real    xv(1),zv(1),vel(1)

      integer nc,imc(1),ixc(1),nxc(1)
      real    xc(1),zc(1)

      integer nx_grd,nx_grdinc
      real    x0_grd,dx_grd

      integer nz_grd,nz_grdinc
      real    z0_grd,dz_grd

      real    v_grd(1)

      integer m_work
      real    work(m_work)

      integer i_err,ic,ic1

      call gtol_grid_to_cell(1,0,0,i_dir
     1,imv,itv,nxv,xv,zv,vel
     1,nxc,xc,zc
     1,nx_grd,x0_grd,dx_grd,nx_grdinc
     1,nz_grd,z0_grd,dz_grd,nz_grdinc
     1,v_grd
     1,m_work,work
     1,i_err)
      if (i_err .ne. 0) goto 999

      do ic = 1 , nc
c  determine what velocity type should be used in this cell

        ic1 = ixc(ic) + 1
        if (imv .eq. imc(ic)) then
          call gtol_grid_to_cell(0,1,0,i_dir
     1,imv,itv,nxv,xv,zv,vel
     1,nxc(ic),xc(ic1),zc(ic1)
     1,nx_grd,x0_grd,dx_grd,nx_grdinc
     1,nz_grd,z0_grd,dz_grd,nz_grdinc
     1,v_grd
     1,m_work,work
     1,i_err)
          if (i_err .ne. 0) goto 999
        endif    ! if (imv .eq. imc(ic)) then
      enddo    ! do ic = 1 , nc

      call gtol_grid_to_cell(0,0,1,i_dir
     1,imv,itv,nxv,xv,zv,vel
     1,nxc,xc,zc
     1,nx_grd,x0_grd,dx_grd,nx_grdinc
     1,nz_grd,z0_grd,dz_grd,nz_grdinc
     1,v_grd
     1,m_work,work
     1,i_err)
      if (i_err .ne. 0) goto 999

      return

  999 continue
      print'('' error in gtol_grid_to_layer'')'
      return 1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_grid_to_cell(init,iadd,ifit,i_dir
     1,imv,itv,nxv,xv,zv,vel,nxc,xc,zc
     1,nx_grd,x0_grd,dx_grd,nx_grdinc
     1,nz_grd,z0_grd,dz_grd,nz_grdinc
     1,v_grd
     1,m_work,work
     1,i_err)
c  fit a single cell and a single velocity type to a grid model
c                           calling sequence
c      call gtol_grid_to_cell(init,iadd,ifit,i_dir
c     1,imv,itv,nxv,xv,zv,vel,nxc,xc,zc
c     1,nx_grd,x0_grd,dx_grd,nx_grdinc
c     1,nz_grd,z0_grd,dz_grd,nz_grdinc
c     1,v_grd
c     1,m_work,work
c     1,i_err)
c
c arguments
c nAME    tYPE*  vALID  dESCRIPTION         *tYPE: i=in, o=out, b=both
c ----    ----   -----  -----------
c  parameters init,iadd,ifit control different stages of
c  the model fitting process
c  to fit a single cell use init=1,iadd=1,ifit=1
c init     i    0,1     iniitalization flag
c   init=1 initalizes counters and assigns work space.
c iadd     i    0,1     add cell flag
c   iadd=1 adds the current cell to the list of x,z points to
c   be fit to current velocity function
c ifit     i    0,1     model fitting flag
c   ifit=1 fits x,z points to current velocity function
c i_dir     i    -1,+1   transform direction +1 = layer to grid
c                                           -1 = grid to layer
c
c  cell velocity definiton
c  arrays itv,xv,zv,vel are dimensioned nxv
c imv      i     array  velocity id number
c itv      i     array  velocity horizon number
c nxv      i     integer # of x,z,vel points in velocity definition
c     definition i occupies elements ixv(i)+1 - ixv(i)+nxv(i) of xv,zv,v
c xv       i     array  velocity definition x values
c zv       i     array  velocity definition z values
c vel      b     array  velocity values
c
c  cell boundary desription
c     arays xc,zc have nxc points
c nxc      i     array  # of points in cell
c xc       i     array  cell x values
c zc       i     array  cell z values
c                                           -1 = grid to layer
c  gridded velocity defintion
c nx_grd    o     int>0  # of x vlaues in gridded file
c x0_grd    o     real   minimum x vlaue in gridded file
c dx_grd    o     real   x vlaue spacing in gridded file
c nz_grd    o     int>0  # of z vlaues in gridded file
c z0_grd    o     real   minimum z vlaue in gridded file
c dz_grd    o     real   z vlaue spacing in gridded file
c v_grd     o     array  gridded slowness dimension (nz,nx)
c
c m_work    i     int    number of words of works space available
c work      i     real   work array
c i_err     i     int    error flag i_err>0 means error during calculation
c
C  Note you should initalize the gridded velocity aray v_grd to zero
C  before you use gtol_grid_to_cell  to fill in any grid values, then wh
C  you are done you should check it for any zero values that have
C  not gotten filled in. gtol_grid_to_cell only fills in the grid points
C  within the cell you pass it.
C
C  Note you pass a single velocity function with material ID IMV and
C  a single cell.  parameters IMV, NXV and NXC are integers and
C  ITV,XV,ZV,VEL, XC,ZC are arrays.
C
C  Note you still need to pass the material ID value IMV.  If IMV
C  < 0 I use a linear gradient in velocity between control points.
C  If IMV > 0 I use a linear gradient in slowness between control
C  points.
c  set the velocities for a single cell
c  i_dir=1 cell to grid i_dir=-1 grid to cell
      implicit none

      integer init,iadd,ifit,i_dir

      integer imv,itv(1),nxv,nxc(1)
      real    xv,zv,vel,xc,zc

      integer nx_grd,nx_grdinc
      real    x0_grd,dx_grd

      integer nz_grd,nz_grdinc
      real    z0_grd,dz_grd

      real    v_grd(1)

      integer m_work
      real    work(m_work)

      integer i_err

      integer ixz,nxz,mxz,n_work,i1,i2,lu,ncv

      save    ixz,nxz,mxz,i1,i2,ncv
      save    i_work_i,i_work_n,n_work
      integer i_work_i,i_work_n

      i_err = 0
      if (init .ge. 1) then
        mxz = nx_grd * nz_grd + 100
        call util_wors(i_work_i,i_work_n,m_work)
        call util_work(i_work_i,i_work_n,ixz,mxz)
        call util_worl(i_work_i,i_work_n,n_work)
        call util_work(i_work_i,i_work_n,i1,n_work/2)
        call util_work(i_work_i,i_work_n,i2,n_work/2)
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 999
        nxz = 0    ! initialize # of points using this velocity
        ncv = 0
        call util_setr(mxz,work(ixz),0)
      endif    ! if (init .ge. 1) then

      if (iadd .ge. 1) ncv = ncv + 1
      if (iadd .ge. 1) call gtolgto1(nxc,xc,zc
     1,nx_grd,x0_grd,dx_grd,nx_grdinc
     1,nz_grd,z0_grd,dz_grd,nz_grdinc
     1,mxz,nxz,work(ixz)
     1,work(i1),work(i2))

      if (ifit .ge. 1) call gtol_fitcell(i_dir
     1,imv,itv,nxv,xv,zv,vel
     1,nx_grd,nx_grdinc,x0_grd,dx_grd
     1,nz_grd,nz_grdinc,z0_grd,dz_grd
     1,v_grd
     1,nxz,work(ixz),n_work,work(i1),*999)

      call gtol_get_lu(lu)

      if (ifit.ge.1 .and. i_dir.lt.0 .and. lu.ge.0) write(lu,'(
     1 /,'' fitting gridded velocities to layer velocity id='',i8
     1,/,'' number of cells with this velocity definition  ='',i8
     1,/,'' number of points in velocity definition        ='',i8
     1,/,'' number of grid points found in cells           ='',i8)')
     1imv,ncv,nxv,nxz

      return

  999 continue
      print'('' error in gtol_grid_to_cell'')'
      i_err = 1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_fitcell(i_dir,imv,itv,nxv,xv,zv,vel
     1,nx_grd,nx_grdinc,x0_grd,dx_grd
     1,nz_grd,nz_grdinc,z0_grd,dz_grd
     1,v_grd
     1,nxz,ixz
     1,m_work,work
     1,*)
c  fit velocity to function
c  i_dir=1 cell to grid i_dir=-1 grid to cell
      implicit none

      integer  i_dir,imv,itv,nxv
      real     xv(1),zv(1)
      real     vel(1)

      integer  nx_grd,nx_grdinc
      real     x0_grd,dx_grd

      integer  nz_grd,nz_grdinc
      real     z0_grd,dz_grd

      real     v_grd(1)

      integer  nxz,ixz(1)

      integer  m_work
      real     work(1)

      integer nzi,ixi,nxi,i_work,n_work
      integer i_err
      integer i_work_i,i_work_n

      if (nxv .eq. 0 .or. nxz .eq. 0) return

c      print'('' fitting grid to layer i_dir='',i8,'' imv='',i8
c     1,'' nxv='',i8,'' nxz='',i8)',i_dir,imv,nxv,nxz

      call util_wors(i_work_i,i_work_n,m_work)       ! initialize work c
      call util_work(i_work_i,i_work_n,ixi,nxv)  ! velocity value pointe
      call util_work(i_work_i,i_work_n,nxi,nxv)  ! velocity value pointe
      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

c  determine the number of points for each horizon
      call gtol_vel_hor_size(nzi,work(ixi),work(nxi),nxv,itv)

c  fit the velocities
      call gtol_fit_velocity(imv,i_dir
     1,nzi,work(ixi),work(nxi),xv,zv,vel
     1,nx_grd,nx_grdinc,x0_grd,dx_grd
     1,nz_grd,nz_grdinc,z0_grd,dz_grd
     1,v_grd
     1,nxz,ixz
     1,n_work,work(i_work)
     1,*999)

      return

  999 continue
      print'('' error in gtol_fitcell'')'
      return 1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_vel_hor_size(nz,ix,nx,nxv,itv)
c  determine the number of different values for each horizon
c  a new horizon is defined by itv changeing value
      implicit none
      integer nz,ix(1),nx(1),nxv,itv(1)
      integer i

      nz = 1
      ix(1) = 0
      nx(1) = min(nxv,1)

      do i = 2 , nxv
c  if this flag is different from the previous
c  it is a new horizon
        if (itv(i) .ne. itv(i-1)) then
          nz = nz + 1
          ix(nz) = ix(nz-1) + nx(nz-1)
          nx(nz) = 1
        else
          nx(nz) = nx(nz) + 1
        endif
      enddo    ! do i = 2 , nxv

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtolgto1(nxc,xc,zc
     1,nx_grd,x0_grd,dx_grd,nx_grdinc
     1,nz_grd,z0_grd,dz_grd,nz_grdinc
     1,mxz,nxz,ixz,z0
     1,work)
c  determine what node points fall within cell add to ixz
      implicit none

      integer nxc
      real    xc(1),zc(1)

      integer nx_grd,nx_grdinc
      real    x0_grd,dx_grd

      integer nz_grd,nz_grdinc
      real    z0_grd,dz_grd

      integer mxz,nxz,ixz(1)
      real    z0(1)

      real    work

      integer lu,i_call,ix,ix0,ix1,ix2,iz,iz1,iz2,iz0,nz0

      real xmin,xmax,x0,x1,z1

      data lu/-1/,i_call/0/
      i_call = i_call + 1

      call cellmnmx(xmin,xmax,nxc,xc)    ! min, max values in xc
      ix1 = max(1,min(nx_grd,int((xmin-x0_grd)/dx_grd)+1))    ! first co
      x1 = (ix1 - 1) * dx_grd + x0_grd    ! loc of first column
      if (x1 .lt. xmin) ix1 = min(ix1+1,nx_grd) ! if column is left of x
      x1 = (ix1 - 1) * dx_grd + x0_grd
      ix2 = max(ix1,min(nx_grd,int((xmax-x0_grd)/dx_grd)+1)) ! second co

c  for each column within this cell determine the crossing points
      do ix = ix1 , ix2
        ix0 = (ix - 1) * nx_grdinc - nz_grdinc + 1
        x0 = (ix - ix1) * dx_grd + x1
        call cellncrs(x0,nxc,xc,zc,nz0,z0,work)

        do iz0 = 1 , nz0 , 2
          iz1 = max(1,min(nz_grd,int((z0(iz0)-z0_grd)/dz_grd)+1))
          z1 = (iz1 - 1) * dz_grd + z0_grd
          if (z1 .lt. z0(iz0)) iz1 = min(iz1+1,nz_grd)
          iz2 = max(iz1,min(nz_grd,int((z0(iz0+1)-z0_grd)/dz_grd)+1))

          do iz = iz1 , iz2
            nxz = min(mxz,nxz+1)
            ixz(nxz) = ix0 + iz * nz_grdinc
          enddo    ! do iz = iz1 , iz2

          if (nxz .ge. mxz) goto 1

        enddo    ! do iz0 = 1 , nz0 , 2

        if (nxz .ge. mxz) goto 1
      enddo    ! do ix = ix1 , ix2

    1 continue

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_fill_velocity_1(nx_grd,nz_grd,v_grd,*)
c  fill in v_grd check v_grd > 0.
      implicit none
      integer nx_grd,nz_grd
      real v_grd

      call gtol_fill_velocity_2(nz_grd,nx_grd,v_grd)
      call gtol_check_for_zero_n(nx_grd*nz_grd,v_grd,*999)

      return
  999 continue
      print'('' error in gtol_fill_velocity'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_fill_velocity_2(n1,n2,x)
c  fill in zero_n

      implicit none
      integer n1,n2,i1,i2,inc
      real x(n1,1)

      inc = n1
      do i1 = 1 , n1
        call gtol_fill_velocity_3(inc,n2,x(i1,1))
      enddo    ! do i1 = 1 , n1

      inc = 1
      do i2 = 1 , n2
        call gtol_fill_velocity_3(inc,n1,x(1,i2))
      enddo    ! do i2 = 1 , n2

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_fill_velocity_3(inc,n,x)
c  fill in non zero values
      implicit none
      integer inc,n,i,j
      real x(inc,1)

      do i = 1 , n
        if (x(1,i) .ne. 0) then
          do j = i-1 , 1 , -1
            x(1,j) = x(1,i)
          enddo    ! do j = i-1 , 1 , -1
          goto 1
        endif
      enddo    ! do i = 1 , n
    1 continue

      do i = n , 1 , -1
        if (x(1,i) .ne. 0) then
          do j = i+1 , n , 1
            x(1,j) = x(1,i)
          enddo    ! do j = i+1 , n , 1
          goto 2
        endif
      enddo    ! i = n , 1 , -1
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_check_for_zero_n(n,x,*)
c  check for x<=0
      implicit none
      integer n,n0
      real x(1)
      call gtol_check_for_zero_1(n0,n,x)
      if (n0 .ne. 0) goto 999
      return
  999 continue
      print'('' error in gtol_check_for_zero_n''
     1,/,'' found velocity values undefined''
     1,/,'' number searched ='',i8,'' number undefined ='',i8)',n0,n
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_check_for_zero_1(n0,n,x)
c  count the numer of x<=0
      implicit none
      integer n0,n,i,j
      real x(1)

      n0 = 0
      do i = 1 , n
        if (x(i) .le. 0.) n0 = n0 + 1
      enddo    ! do i = 1 , n

      if (n0 .ne. 0) then
        print'('' error in gtol_check_for_zero_1''
     1,/,'' found velocity values undefined''
     1,/,'' number searched ='',i8,'' number undefined ='',i8)',n0,n
        j = 0
        do i = 1 , n
          if (x(i) .le. 0.) then
            j = j + 1
            print'(2(1x,i5),1x,g15.9)',j,i,x(i)
          endif
        enddo    ! do i = 1 , n

      endif    ! if (n0 .ne. 0) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_fit_velocity(imv,i_dir
     1,nzi,ixi,nxi,xi,zi,vi
     1,nxo,nxoinc,xomin,xoinc
     1,nzo,nzoinc,zomin,zoinc
     1,vo
     1,nxz,ixz
     1,m_work,work
     1,*)
c  fill cell with v(x,z)
      implicit none

      integer imv,i_dir

      integer nzi,ixi(1),nxi(1)
      real    xi(1),zi(1),vi(1)

      integer nxo,nxoinc
      real    xomin,xoinc

      integer nzo,nzoinc
      real    zomin,zoinc

      real    vo(1)

      integer nxz,ixz(1)

      integer m_work
      real    work(m_work)

      integer i_call
      integer ixo,izo,ivo
      integer i_work,n_work
      integer i,nyi,iyi
      integer i_err
      real    yi,yo
      integer i_work_i,i_work_n

      data i_call/0/
      i_call = i_call + 1
c  use work space for temporary arrays xo,zo,vo,work
c  uses 3*nxz + 5 * nzi points
      call util_wors(i_work_i,i_work_n,m_work)        ! initialize work 
      call util_work(i_work_i,i_work_n,ixo,nxz  ) ! x location pointer
      call util_work(i_work_i,i_work_n,izo,nxz  ) ! z location pointer
      call util_work(i_work_i,i_work_n,ivo,nxz  ) ! velocity value point
      call util_worl(i_work_i,i_work_n,n_work)          ! amount of work
      call util_work(i_work_i,i_work_n,i_work,n_work) ! work pointer
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

      do i = 1 , nxz    ! fill the x,z location values
        work(ixo+i-1) = mod(int((ixz(i)-1)/nxoinc),nxo) * xoinc + xomin
        work(izo+i-1) = mod(int((ixz(i)-1)/nzoinc),nzo) * zoinc + zomin
        work(ivo+i-1) = vo(ixz(i))
      enddo    ! do i = 1 , nxz

      call fitcell(i_dir,2,nzi,iyi,nyi,ixi,nxi,xi,yi,zi,vi
     1,nxz,work(ixo),yo,work(izo),work(ivo),n_work,work(i_work),i_err)
      if (i_err .ne. 0) goto 999

      if (i_dir .eq. 1) then    ! transfer vo from work array
        do i = 1 , nxz
          vo(ixz(i)) = work(ivo+i-1)
        enddo    ! do i = 1 , nxz
      endif    ! if (i_dir .eq. 1) then    ! transfer vo from work array

      return

  999 continue
      print'('' error in gtol_fit_velocity'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dl_to_3dg(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd
     1,m_work,work
     1,i_err)

      implicit none

      real     util_invert_1

      integer  nx_lay
      real     x0_lay,dx_lay

      integer  ny_lay
      real     y0_lay,dy_lay

      integer  nz_lay
      real     z_lay(nx_lay,ny_lay,nz_lay)

      integer  ncv,icv
      real     xcv,ycv,zcv

      integer  nxv,iv1,iv2,iv3
      real     xv,yv,zv,vel

      integer  nx_grd
      real     x0_grd,dx_grd

      integer  ny_grd
      real     y0_grd,dy_grd

      integer  nz_grd
      real     z0_grd,dz_grd

      real     v_grd(nz_grd,nx_grd,ny_grd)

      integer  m_work
      real     work(1)

      integer  i_err

      integer  nz_inc,nz_grd_0
      integer  i_work,n_work

      integer  ix_grd,iy_grd,iv_grd_0
      real     dz_grd_0,x_grd,y_grd

      integer  i
      real     cpu_a

      integer  i_work_i,i_work_n

      integer  i_invert,j_invert
      data     i_invert/0/
      save     i_invert

      nz_inc = 5
      nz_grd_0 = (nz_grd - 1) * nz_inc + 1
      dz_grd_0 = dz_grd / nz_inc
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,iv_grd_0,nz_grd_0)
      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999
      call util_setr(nx_grd*ny_grd*nz_grd,v_grd,0.)

      print'(/,'' creating 3d gridded model from 3d layered model''
     1,/,'' nx='',i5,'' xmin='',f10.2,'' xmax='',f10.2,'' xinc='',f10.4
     1,/,'' ny='',i5,'' ymin='',f10.2,'' ymax='',f10.2,'' yinc='',f10.4
     1,/,'' nz='',i5,'' zmin='',f10.2,'' zmax='',f10.2,'' zinc='',f10.4
     1,/,'' layered model'',/,'' nz='',i5
     1,/'' nx='',i5,'' xmin='',f10.2,'' xmax='',f10.2,'' xinc='',f10.2
     1,/'' ny='',i5,'' ymin='',f10.2,'' ymax='',f10.2,'' yinc='',f10.2
     1)'
     1,nx_grd,x0_grd,x0_grd+(nx_grd-1)*dx_grd,dx_grd
     1,ny_grd,y0_grd,y0_grd+(ny_grd-1)*dy_grd,dy_grd
     1,nz_grd,z0_grd,z0_grd+(nz_grd-1)*dz_grd,dz_grd
     1,nz_lay
     1,nx_lay,x0_lay,x0_lay+(nx_lay-1)*dx_lay,dx_lay
     1,ny_lay,y0_lay,y0_lay+(ny_lay-1)*dy_lay,dy_lay
      print'('' i='',i5,'' z_lay='',f10.4)',(i,z_lay(1,1,i),i=1,nz_lay)
C      print*,' nz_grd_0=',nz_grd_0,' m_work=',m_work,' n_work=',n_work

      do ix_grd = 1 , nx_grd
        x_grd = (ix_grd - 1) * dx_grd + x0_grd

        do iy_grd = 1 , ny_grd
          y_grd = (iy_grd - 1) * dy_grd + y0_grd

c  grid up this x,y location on a finer z grid
          call gtol_3dl_to_3dga(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay,nz_lay
     1,z_lay
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,x_grd,y_grd,nz_grd_0,z0_grd,dz_grd_0,work(iv_grd_0)
     1,n_work,work(i_work),i_err)
          if (i_err .ne. 0) goto 999

          call gtol_ave_sum(1.,nz_grd_0,nz_inc,work(iv_grd_0)
     1,v_grd(1,ix_grd,iy_grd))

        enddo    ! do iy_grd = 1 , ny_grd

c        call second(cpu_a)
        cpu_a = 0
        if (i_invert .eq. 0) then
          print'('' i='',i6,'' x='',f10.2,'' c='',f10.4
     1,'' v='',6(1x,f6.0))'
     1,ix_grd,x_grd,cpu_a
     1,(util_invert_1(v_grd(i,ix_grd,ny_grd/2+1))
     1,i=1,nz_grd,max(1,nz_grd/5))
        else    ! if (i_invert .eq. 0) then
          print'('' i='',i6,'' x='',f10.2,'' c='',f10.4
     1,'' v='',6(1x,f6.0))'
     1,ix_grd,x_grd,cpu_a
     1,((v_grd(i,ix_grd,ny_grd/2+1))
     1,i=1,nz_grd,max(1,nz_grd/5))
        endif    ! if (i_invert .eq. 0) then

      enddo    ! do ix_grd = 1 , nx_grd

      return

  999 continue
      print'('' error in gtol_3dl_to_3dg i_err='',i8)',i_err
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry gtol_3dl_to_3dg_put_invert(j_invert)
      i_invert = j_invert
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry gtol_3dl_to_3dg_get_invert(j_invert)
      j_invert = i_invert
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dl_to_3dga(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,xg,yg,nz_grd,z0_grd,dz_grd
     1,v_grd
     1,m_work,work
     1,i_err)
      implicit none

      integer  nx_lay
      real     x0_lay,dx_lay

      integer  ny_lay
      real     y0_lay,dy_lay

      integer  nz_lay
      real     z_lay(nx_lay,ny_lay,nz_lay)

      integer  nxv,iv1,iv2,iv3
      real     xv,yv,zv
      real     vel(1)

      integer  ncv,icv(1)
      real     xcv(1),ycv(1),zcv(1)

      real     xg,yg

      integer  nz_grd
      real     z0_grd,dz_grd

      real     v_grd(nz_grd)

      integer  m_work
      real     work(m_work)

      integer i_err

      integer jcv,iz1,iz2,iz_lay,nx,i
      real    z1,z2,w1,w2

      call util_setr(nz_grd,v_grd,0.)

c      print'(/,'' gtol_3dl_to_3dga ncv='',i8,'' nx_lay='',i8
c     1,'' ny_lay='',i8,'' nz_lay='',i8)',ncv,nx_lay,ny_lay,nz_lay
c      print'('' i='',i5,'' z_lay='',f10.4)'
c     1,(i,z_lay(nx_lay/2+1,ny_lay/2+1,i),i=1,nz_lay)
c      print'(/,'' nz_grd='',i8,'' z0_grd='',f10.2,'' zgmax='',f10.2
c     1,'' dz_grd='',f10.4)',nz_grd,z0_grd,(nz_grd-1)*dz_grd+z0_grd,dz_g

      do jcv = 1 , ncv

c  get the layer containing this cell pointer
c  determine the layer depths at this x,y location
c  by interpolating from z_lay into work
        call gtol_interpolate(
     1 nx_lay,  x0_lay,dx_lay
     1,ny_lay,  y0_lay,dy_lay
     1,nz_lay,      0.,     1.
     1,z_lay
     1,     1,xcv(jcv),     1.
     1,     1,ycv(jcv),     1.
     1,nz_lay,      0.,     1.
     1,work)

c      print'('' nz_lay='',i5,'' w='',8(1x,g9.3))'
c     1,nz_lay,(work(i),i=1,min(nz_lay,8))

        call fitcell1(zcv(jcv),0.,nz_lay,work,iz_lay,iz2,w1,w2)

c      print'(/,'' jcv='',i5,'' icv='',i5,'' iz_lay='',i3,'' iz2='',i3
c     1,'' x='',f8.2,'' y='',f8.2,'' z='',f12.4)'
c     1,jcv,icv(jcv),iz_lay,iz2,xcv(jcv),ycv(jcv),zcv(jcv)

        if (zcv(jcv) .lt. work(iz_lay)) iz_lay = iz_lay - 1
        call gtol_3dg_to_3dlc(
     1 xg,yg
     1,nz_grd,z0_grd,dz_grd
     1,iz1,iz2,z1,z2
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay,iz_lay)
        iz1 = max(iz1,  1)
        iz2 = min(iz2,nz_grd)

        if (iz2 .ge. iz1)
     1call fitcell6(nxv,iv1,iv2,iv3,xv,yv,zv,vel,icv(jcv)
     1,          1,                   xg,     1.
     1,          1,                   yg,     1.
     1,(iz2-iz1)+1,z0_grd+(iz1-1)*dz_grd,dz_grd
     1,v_grd(iz1)
     1,m_work,work
     1,i_err)
c      print'('' iz1='',i8,'' iz2='',i8,'' v_grd='',f10.2,1x,f10.2)'
c     1,iz1,iz2,1./v_grd(iz1),1./v_grd(iz2)

        if (i_err .ne. 0) goto 999
      enddo    ! do jcv = 1 , ncv

      return

  999 continue
      print'('' error in gtol_3dl_to_3dga i_err='',i8)',i_err
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dg_to_3dl(nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay
     1,ndim
     1,nx2,x2min,x2inc
     1,ny2,y2min,y2inc
     1,nz2,z2min,z2inc
     1,v2,y2
     1,m_work,work
     1,i_err)

      implicit none
      integer nxv,iv1(1),iv2(1),iv3(1),ncv,icv(1),mv_grd,nx_lay,ny_lay
     1,nz_lay
      integer ndim,nx2,ny2,nz2,m_work
      real xv(1),yv(1),zv(1),vel(1),xcv(1),ycv(1),zcv(1)
      real x0_lay,dx_lay,y0_lay,dy_lay,z_laymin,z_layinc,z_lay(nx_lay
     1,ny_lay,nz_lay)
      real x2min,x2inc,y2min,y2inc,z2min,z2inc,v2,y2(1),work(m_work)

      integer jcv,jmv,iz1,iz2,iz_lay,iyi,nyi,ixi,nxi,ivp
      integer mg,jxg,jyg,jz_grd,jv_grd,nw0,iw0,jw0,n_work,i_work
      integer nv,nzi,ng,ix_grd,iy_grd,iz_grd,iv_grd,i_dir,ndimf
      integer  i_err
      real w1,w2,z_top,z_bot
      integer i_work_i,i_work_n

      i_dir = -1
      ndimf = 3
      print'('' gtol_3dg_to_3dl 3d grid to layer conversion'')'
      print'('' ncv='',i10,'' nxv='',i10)',ncv,nxv
      do jcv = 1 , ncv
        call cellfnmv(jmv,icv(jcv),nxv,iv1)

c  get the layer containing this cell pointer
        call gtol_interpolate(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,0.,1.
     1,z_lay
     1,1,xcv(jcv),1.
     1,1,ycv(jcv),1.
     1,nz_lay,0.,1.
     1,work)

        call fitcell1(zcv(jcv),0.,nz_lay,work,iz_lay,iz2,w1,w2)
        z_top = work(iz_lay)
        z_bot = work(iz2)

        if (zcv(jcv) .lt. work(iz_lay)) iz_lay = iz_lay - 1

c  make a list of the velocity control points
        call fitcpnt1(icv(jcv),nxv,iv1,nv,ivp)

c  get space for pointers
        call util_wors(i_work_i,i_work_n,m_work)    ! initalize work cou
        call util_work(i_work_i,i_work_n,iyi ,nv)    ! 1st point in each
        call util_work(i_work_i,i_work_n,nyi ,nv)    ! # of lines in eac
        call util_work(i_work_i,i_work_n,ixi ,nv)    ! 1st point in each
        call util_work(i_work_i,i_work_n,nxi ,nv)    ! # of points in ea
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 999
        call fitcpnt2(nv,iv2(ivp),iv3(ivp)
     1,nv,nzi,work(iyi),work(nyi),work(ixi),work(nxi))

c  get space for a list of the 2d grid values that fall within this laye
        call util_worl(i_work_i,i_work_n,nw0)
        call util_woru(i_work_i,i_work_n,iw0)
        mg = nw0 / 5
        call util_work(i_work_i,i_work_n,jxg,mg)
        call util_work(i_work_i,i_work_n,jyg,mg)
        call util_work(i_work_i,i_work_n,jz_grd,mg)
        call util_work(i_work_i,i_work_n,jv_grd,mg)
        call util_worl(i_work_i,i_work_n,n_work)
        call util_work(i_work_i,i_work_n,i_work,n_work)
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 999

c  make a list of the 2d grid values that fall within this layer
        call gtol_3dg_to_3dla(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay,iz_lay
     1,ndim
     1,nx2,x2min,x2inc
     1,ny2,y2min,y2inc
     1,nz2,z2min,z2inc
     1,v2,y2
     1,mg,ng,work(jxg),work(jyg),work(jz_grd),work(jv_grd))

      print'(/,'' cell pointer #'',i5,'' icv='',i5,'' layer='',i5,1x,i5
     1,/,'' xcv='',f10.2,'' ycv='',f10.2
     1,'' zcv='',f10.2,'' z_lay1='',f10.2,'' z_lay2='',f10.2
     1,/,'' number of grid points found in layer             ='',i10
     1,/,'' number of velocity control points for this layer ='',i10
     1,/,'' number of velocity horizons in this function     ='',i10)'
     1,jcv,icv(jcv),max(1,iz_lay),iz2
     1,xcv(jcv),ycv(jcv),zcv(jcv),z_top,z_bot
     1,ng,nv,nzi

c  shift list of values to save memory
        call util_wors(i_work_i,i_work_n,m_work)    ! reset work pointer
        call util_work(i_work_i,i_work_n,jw0,iw0)   ! reset work pointer
        call util_work(i_work_i,i_work_n,ix_grd,ng)    ! ix_grd and jxg 
        call util_work(i_work_i,i_work_n,iy_grd,ng)
        call util_work(i_work_i,i_work_n,iz_grd,ng)
        call util_work(i_work_i,i_work_n,iv_grd,ng)
        call util_worl(i_work_i,i_work_n,n_work)
        call util_work(i_work_i,i_work_n,i_work,n_work)
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 999
        call util_copy(ng,work(jyg),work(iy_grd))
        call util_copy(ng,work(jz_grd),work(iz_grd))
        call util_copy(ng,work(jv_grd),work(iv_grd))

c  fit vel using v_grd
        call fitcell(i_dir,ndimf
     1,nzi,work(iyi),work(nyi),work(ixi),work(nxi)
     1,xv(ivp),yv(ivp),zv(ivp),vel(ivp)
     1,ng,work(ix_grd),work(iy_grd),work(iz_grd),work(iv_grd)
     1,n_work,work(i_work)
     1,i_err)
      if (i_err .ne. 0) goto 999

      enddo    ! jcv = 1 , ncv

      return

  999 continue
      print'('' error in gtol_3dg_to_3dl i_err='',i8)',i_err
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dg_to_3dla(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay,iz_lay
     1,ndim
     1,nx2,x2min,x2inc
     1,ny2,y2min,y2inc
     1,nz2,z2min,z2inc
     1,v2,y2
     1,mg,ng,xg,yg,zg
     1,v_grd)
c  make alist of all 2d velocity points within layer
      implicit none
      integer nx_lay,ny_lay,nz_lay,iz_lay,ndim,nx2,ny2,nz2,mg,ng
      real x0_lay,dx_lay,y0_lay,dy_lay,x2min,x2inc,y2min,y2inc,z2min
     1,z2inc
      real z_lay(nx_lay,ny_lay,1),v2(nz2,nx2,1),y2(1),xg(1),yg(1),zg(1)
     1,v_grd(1)
      integer ig,ix,iy,iz,iz1,iz2
      real x,y,z1,z2

      ig = 1
      do iy = 1 , ny2
        if (ndim .eq. 2) then
          y = y2(iy)
        else
          y = (iy - 1) * y2inc + y2min
        endif
c  for each x vlaue get the depths from z_lay
        do ix = 1 , nx2
          x = (ix - 1) * x2inc + x2min

          call gtol_3dg_to_3dlc(
     1x,y,nz2,z2min,z2inc
     1,iz1,iz2,z1,z2
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay,iz_lay)

           ng = min(max(0,iz2-iz1+1),mg-ig)
c      print*,' ng=',ng,' iz=',iz1,iz2,' z1=',z1,' z2=',z2
           call util_setr(ng,xg(ig),x)
           call util_setr(ng,yg(ig),y)
           call util_line(ng,zg(ig),(iz1-1)*z2inc+z2min,z2inc)
           call util_copy(ng,v2(iz1,ix,iy),v_grd(ig))
           ig = ig + ng
        enddo    ! do 2 ix = 1 , nx2
      enddo    ! do iy = 1 , ny2
      ng = ig - 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dg_to_3dlc(x,y
     1,nz,zmin,zinc
     1,iz1,iz2,z1,z2
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay,iz_lay)
c  computer range of depth values for layer iz_lay
      implicit none
      integer nz,iz1,iz2,nx_lay,ny_lay,nz_lay,iz_lay
      real z1,z2,x,y,zmin,zinc,x0_lay,dx_lay,y0_lay,dy_lay,z_lay(nx_lay
     1,ny_lay,nz_lay)
      real  eps,work
      eps = 1 e-5

      call gtol_interpolate(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,1,0.,1.
     1,z_lay(1,1,max(iz_lay  ,1  ))
     1,1,x,1.
     1,1,y,1.
     1,1,0.,1.
     1,z1)

      call gtol_interpolate(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,1,0.,1.
     1,z_lay(1,1,min(iz_lay+1,nz_lay))
     1,1,x,1.
     1,1,y,1.
     1,1,0.,1.
     1,z2)

      if (iz_lay .lt.   1) z1 = zmin
      if (iz_lay .ge. nz_lay) z2 = zmin + (nz - 1) * zinc
      iz1 = max(int((z1-zmin-eps)/zinc)+1,1 )
      if ((iz1-1)*zinc+zmin .lt. z1) iz1 = iz1 + 1
      iz2 = min(int((z2-zmin+eps)/zinc)+1,nz)
      if ((iz2-1)*zinc+zmin .gt. z2) iz2 = iz2 - 1
c      write(88,'('' iz_lay='',i5,'' iz1='',i5,'' iz2='',i5
c     1,'' z1='',f10.4,'' z2='',f10.4)')iz_lay,iz1,iz2,z1,z2
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_get_lu(lu0)
      implicit none
      integer lu,lu0
      data lu/-1/
      lu0 = lu
      return
      entry gtol_put_lu(lu0)
      lu = lu0
      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_replace_grid(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd
     1,m_work,work
     1,i_err)

      implicit none
      integer nx_lay,ny_lay,nz_lay,nxv,iv1,iv2,iv3,ncv,icv,nx_grd
     1,ny_grd,nz_grd,m_work
      real x0_lay,dx_lay,y0_lay,dy_lay,z_lay(nx_lay,ny_lay,nz_lay)
      real xv,yv,zv,vel,xcv,ycv,zcv
      real x0_grd,dx_grd,y0_grd,dy_grd,z0_grd,dz_grd,v_grd(nz_grd
     1,nx_grd,ny_grd),work(1)
      integer i_work,n_work,ix_grd,iy_grd,i,i_err
      real    xg,yg
c      integer i_work_i,i_work_n

c      call util_wors(i_work_i,i_work_n,m_work)

      print'(/,'' replaceing 3d gridded model from 3d layered model''
     1,/,'' nx='',i5,'' xmin='',f10.2,'' xmax='',f10.2,'' xinc='',f10.4
     1,/,'' ny='',i5,'' ymin='',f10.2,'' ymax='',f10.2,'' yinc='',f10.4
     1,/,'' nz='',i5,'' zmin='',f10.2,'' zmax='',f10.2,'' zinc='',f10.4
     1,/,'' layered model'',/,'' nz='',i5
     1,/'' nx='',i5,'' xmin='',f10.2,'' xmax='',f10.2,'' xinc='',f10.2
     1,/'' ny='',i5,'' ymin='',f10.2,'' ymax='',f10.2,'' yinc='',f10.2
     1)'
     1,nx_grd,x0_grd,x0_grd+(nx_grd-1)*dx_grd,dx_grd
     1,ny_grd,y0_grd,y0_grd+(ny_grd-1)*dy_grd,dy_grd
     1,nz_grd,z0_grd,z0_grd+(nz_grd-1)*dz_grd,dz_grd
     1,nz_lay
     1,nx_lay,x0_lay,x0_lay+(nx_lay-1)*dx_lay,dx_lay
     1,ny_lay,y0_lay,y0_lay+(ny_lay-1)*dy_lay,dy_lay
      print'('' i='',i5,'' z_lay='',f10.4)',(i,z_lay(1,1,i),i=1,nz_lay)

      call gtol_replace_grid_1(
     1nx_lay,x0_lay,dx_lay,ny_lay,y0_lay,dy_lay,nz_lay,z_lay
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel,ncv,icv,xcv,ycv,zcv
     1,nx_grd,x0_grd,dx_grd,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd,v_grd,m_work,work,i_err)
      if (i_err .ne. 0) goto 999

      return

  999 continue
      print'('' error in gtol_replace_grid i_err='',i8)',i_err
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_replace_grid_1(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd
     1,m_work,work
     1,i_err)
      implicit none
      integer  nx_grd
      real     x0_grd,dx_grd
      integer  ny_grd
      real     y0_grd,dy_grd
      integer nx_lay,ny_lay,nz_lay,nxv
      integer iv1(1),iv2(1),iv3(1),ncv,icv(1),nz_grd,m_work
      real x0_lay,dx_lay,y0_lay,dy_lay,z_lay(nx_lay,ny_lay,nz_lay)
      real xv(1),yv(1),zv(1),vel(1)
      real xcv(1),ycv(1),zcv(1),z0_grd,dz_grd,v_grd(nz_grd,nx_grd
     1,ny_grd)
      real    work(m_work)
      integer i_err
      integer jcv,jmv,iz1,iz2,iz_lay,nx,i
      real z1,z2,w1,w2
      integer ix_grd,iy_grd,i_do,jz_grd1,jz_grd2,iz_grd1,iz_grd2
      real    xg,yg
      real    z_lay1,z_lay2,zg1,zg2
      real    vw1,vw2,vw3,vw4
      real    v_grd1,v_grd2,v_grd3,v_grd4
      integer nz_inc,nz_grd0,ivl,jvl,iv_grd,jv_grd,kv_grd,lv_grd,i_work
     1,n_work
      real    dz_grd0
      real    util_invert_1
      integer  ix_terp,iy_terp,nx_terp,ny_terp
      real     scale,x0,y0,dx_terp,dy_terp
      integer i_work_i,i_work_n

      nx_terp = max(0,min(2,int(.5*dx_grd/dx_lay)))
      if (nx_lay .eq. 1) nx_terp = 0
      dx_terp = .5 * dx_grd / max(1,nx_terp)

      ny_terp = max(0,min(2,int(.5*dy_grd/dy_lay)))
      if (ny_lay .eq. 1) ny_terp = 0
      dy_terp = .5 * dy_grd / max(1,ny_terp)

      nz_inc = 5
      nz_grd0 = (nz_grd - 1) * nz_inc + 1
      dz_grd0 = dz_grd / nz_inc

      call util_wors(i_work_i,i_work_n,m_work)    ! reset work pointer
      call util_work(i_work_i,i_work_n,ivl,nx_grd*ny_grd*nz_grd)
      call util_work(i_work_i,i_work_n,iv_grd,nx_grd*ny_grd*nz_grd)
      call util_work(i_work_i,i_work_n,jvl,nz_grd0)
      call util_work(i_work_i,i_work_n,jv_grd,nz_grd0)
      call util_work(i_work_i,i_work_n,kv_grd,nz_grd0)
      call util_work(i_work_i,i_work_n,lv_grd,nz_grd)
      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

      print'(/,'' gtol_replace_grid''
     1,'' ncv='',i8,'' nx_lay='',i8,'' ny_lay='',i8
     1,'' nz_lay='',i8)',ncv,nx_lay,ny_lay,nz_lay
      print'('' i='',i5,'' z_lay='',f10.4)'
     1,(i,z_lay(nx_lay/2+1,ny_lay/2+1,i),i=1,nz_lay)
      print'(/,'' nz_grd='',i8,'' z0_grd='',f10.2,'' zgmax='',f10.2
     1,'' dz_grd='',f10.4)',nz_grd,z0_grd,(nz_grd-1)*dz_grd+z0_grd
     1,dz_grd
      print'('' nx_terp='',i10,'' dx_terp='',f12.4)'
     1,nx_terp,dx_terp
      print'('' ny_terp='',i10,'' dy_terp='',f12.4)'
     1,ny_terp,dy_terp
c      write(88,'(/,'' gtol_replace_grid''
c     1,'' ncv='',i8,'' nx_lay='',i8,'' ny_lay='',i8
c     1,'' nz_lay='',i8)')ncv,nx_lay,ny_lay,nz_lay
c      write(88,'('' i='',i5,'' z_lay='',f10.4)')
c     1(i,z_lay(nx_lay/2+1,ny_lay/2+1,i),i=1,nz_lay)
c      write(88,'(/,'' nz_grd='',i8,'' z0_grd='',f10.2,'' zgmax='',f10.2
c     1,'' dz_grd='',f10.4)')nz_grd,z0_grd,(nz_grd-1)*dz_grd+z0_grd,dz_g

      call util_copy(nx_grd*ny_grd*nz_grd,v_grd,work(iv_grd))

      do jcv = 1 , ncv

c  get the layer containing this cell pointer
c  get the layer depths at this pointer lcoation
        call gtol_interpolate(
     1nx_lay,x0_lay,dx_lay,ny_lay,y0_lay,dy_lay,nz_lay,0.,1.,z_lay
     1,1,xcv(jcv),1.,1,ycv(jcv),1.,nz_lay,0.,1.,work)

c  get the layer containing this cell pointer
        call fitcell1(zcv(jcv),0.,nz_lay,work,iz_lay,iz2,w1,w2)
        call cellfnmv(jmv,icv(jcv),nxv,iv1)

      print'(/,'' jcv='',i5,'' icv='',i5,'' jmv='',i5
     1,'' iz_lay='',i3,'' iz2='',i3
     1,/,'' xcv='',f8.2,'' ycv='',f8.2,'' zcv='',f12.4)'
     1,jcv,icv(jcv),jmv,iz_lay,iz2,xcv(jcv),ycv(jcv),zcv(jcv)

c      write(88,'(/,'' jcv='',i5,'' icv='',i5,'' jmv='',i5
c     1,'' iz_lay='',i3,'' iz2='',i3
c     1,/,'' xcv='',f8.2,'' ycv='',f8.2,'' zcv='',f12.4)')
c     1jcv,icv(jcv),jmv,iz_lay,iz2,xcv(jcv),ycv(jcv),zcv(jcv)
c      write(88,*)' nx_terp=',nx_terp,' dx_terp=',dx_terp
c      write(88,*)' ny_terp=',ny_terp,' dy_terp=',dy_terp
c      write(88,*)' nxv=',nxv
c      do i = 1 , nxv
c      write(88,*)' i=',i,' iv=',iv1(i),iv2(i),iv3(i)
c     1,' x=',xv(i),' y=',yv(i),' z=',zv(i)
c     1,' v=',vel(i),util_invert_1(vel(i))
c      enddo

        if (jmv .le. 0) then

          print'('' could not find a velocity for this function''
     1,'' jcv='',i5,'' icv='',i5)',jcv,icv(jcv)

        else    ! if (jmv .le. 0) then

          if (zcv(jcv) .lt. work(iz_lay)) iz_lay = iz_lay - 1

c  forward model the layer velociites at the coarse grid locations
          call fitcell6(nxv,iv1,iv2,iv3,xv,yv,zv,vel,icv(jcv)
     1,nx_grd,x0_grd,dx_grd,ny_grd,y0_grd,dy_grd,nz_grd,z0_grd,dz_grd
     1,work(ivl)
     1,n_work,work(i_work),i_err)
          if (i_err .ne. 0) goto 999

c      write(88,*)' vl=', work(ivl),work(ivl+nx_grd*ny_grd*nz_grd-1)
c     1,util_invert_1(work(ivl))
c     1,util_invert_1(work(ivl+nx_grd*ny_grd*nz_grd-1))

c  for each grid x,y location
            do iy_grd = 1 , ny_grd
              yg = (iy_grd - 1) * dy_grd + y0_grd

      if (mod(iy_grd,5).eq.1
     1.or.nx_grd.lt.10.or.ny_grd.lt.10)
     1print'(''   ix       iy      iz1  ''
     1,''  vb1    va1    vb2   va2    vb3    va3'')'
              do ix_grd = 1 , nx_grd

                xg = (ix_grd - 1) * dx_grd + x0_grd

c  initialize the sum velocity to zero
c  as we cycle over ix_terp, iy_terp we will add the interpolated veloci
c  scale is the number of x,y points we sum in
        scale = 0
        call util_setr(nz_grd0,work(kv_grd),0.)
        iz_grd1 = nz_grd + 1
        iz_grd2 = 0

        do iy_terp = -ny_terp , ny_terp

          do ix_terp = -nx_terp , nx_terp
            x0 = xg - ix_terp * dx_terp
            y0 = yg - iy_terp * dy_terp

c  interpolate this coarse grid velocity trace to a fine grid
            call gtol_interpolate(
     1nz_grd,z0_grd,dz_grd,nx_grd,x0_grd,dx_grd,ny_grd,y0_grd,dy_grd
     1,work(iv_grd)
     1,nz_grd0,z0_grd,dz_grd0,1,x0,1.,1,y0,1.,work(jv_grd))

c  interpolate this coarse layer velocity trace to a fine grid
            call gtol_interpolate(
     1nz_grd,z0_grd,dz_grd,nx_grd,x0_grd,dx_grd,ny_grd,y0_grd,dy_grd
     1,work(ivl)
     1,nz_grd0,z0_grd,dz_grd0,1,x0,1.,1,y0,1.,work(jvl))

c  identify those fine grid nodes included in this layer
            call gtol_3dg_to_3dlc(
     1x0,y0,nz_grd0,z0_grd,dz_grd0,iz1,iz2,z1,z2
     1,nx_lay,x0_lay,dx_lay,ny_lay,y0_lay,dy_lay,nz_lay,z_lay,iz_lay)


            if ((iz1-1)*dz_grd0+z0_grd .lt. z1) iz1 = iz1 + 1
            if ((iz2-1)*dz_grd0+z0_grd .gt. z2) iz2 = iz2 - 1

            iz1 = max(iz1,  1)
            iz2 = min(iz2,nz_grd0)

            if (z2-z1 .gt. 1.e-2*dz_grd0) then

              jz_grd1 = max((iz1-1)/nz_inc+1,1)
              jz_grd2 = min((iz2-1)/nz_inc+1,nz_grd)
              if ((jz_grd1-1)*nz_inc+1 .gt. iz1) jz_grd1 =
     1 max(jz_grd1-1,1)
              if ((jz_grd2-1)*nz_inc+1 .lt. iz2) jz_grd2 =
     1 max(jz_grd2+1,1)

              iz_grd1 = min(iz_grd1,jz_grd1)
              iz_grd2 = max(iz_grd2,jz_grd2)

c  replace those fine grid nodes in the grid velocity that lie in the la
              call util_copy(iz2-iz1+1,work(jvl+iz1-1)
     1,work(jv_grd+iz1-1))
c      write(88,*)' iz1=',iz1,' iz2=',iz2,' z1=',z1,' z2=',z2
c      write(88,*)' vl=',work(jvl+iz1-1),work(jvl+iz2-1)
c     1,util_invert_1(work(jvl+iz1-1))
c     1,util_invert_1(work(jvl+iz2-1))
c      write(88,*)' v_grd=',work(jv_grd+iz1-1),work(jv_grd+iz2-1)
c     1,util_invert_1(work(jv_grd+iz1-1))
c     1,util_invert_1(work(jv_grd+iz2-1))

c        if (iz2 .ge. iz1)
c     1call fitcell6(nxv,iv1,iv2,iv3,xv,yv,zv,vel,icv(jcv)
c     1,1,xg,1.,1,yg,1.,(iz2-iz1)+1,z0_grd+(iz1-1)*dz_grd0,dz_grd0
c     1,work(iv_grd+iz1-1),n_work,work(i_work),i_err)
c        if (i_err .ne. 0) goto 999

            else    ! if (z2-z1 .gt. 1e-2*dz_grd0) then

c      write(88,'('' z2<z1)'')')
c      write(88,'('' ix='',i8,'' iy='',i8,'' x='',f10.2
c     1,'' y='',f10.2,'' z1='',f10.2,'' z2='',f10.2)')
c     1ix_terp,iy_terp,x0,y0,z1,z2

            endif    ! if (z2-z1 .gte. 1e-2*dz_grd0) then

c      write(88,'(1x,i6,1x,i6,1x,i6,1x,i6
c     1,1x,f6.1,1x,f6.1,1x,f7.0,1x,f7.0,1x,f8.0,1x,f8.0)')
c     1ix_grd,iy_grd,iz1,iz2,x0,y0,z1,z2
c     1,util_invert_1(work(jv_grd+iz1-1))
c     1,util_invert_1(work(jv_grd+iz2-1))
c      write(88,'('' ix='',i8,'' iy='',i8,'' x='',f10.2
c     1,'' y='',f10.2,'' z1='',f10.2,'' z2='',f10.2)')
c     1ix_terp,iy_terp,x0,y0,z1,z2
            scale = scale + 1
            call gtol_add_sum(nz_grd0,work(jv_grd),work(kv_grd))

          enddo    ! do ix_terp = -nx_terp , nx_terp

        enddo    ! do iy_terp = -ny_terp , ny_terp

        scale = 1. / scale
        call gtol_ave_sum(scale,nz_grd0,nz_inc,work(kv_grd)
     1,work(lv_grd))
        call util_scale(nz_grd0,work(kv_grd),scale)

        v_grd1 = v_grd(max(1,min(nz_grd,iz_grd1-2)),ix_grd,iy_grd)
        v_grd2 = v_grd(max(1,min(nz_grd,iz_grd1-1)),ix_grd,iy_grd)
        v_grd3 = v_grd(max(1,min(nz_grd,iz_grd1-0)),ix_grd,iy_grd)
        v_grd4 = v_grd(max(1,min(nz_grd,iz_grd1+1)),ix_grd,iy_grd)

        call util_copy(iz_grd2-iz_grd1+1
     1,work(lv_grd+iz_grd1-1),v_grd(iz_grd1,ix_grd,iy_grd))

        vw1 = v_grd(max(1,min(nz_grd,iz_grd1-2)),ix_grd,iy_grd)
        vw2 = v_grd(max(1,min(nz_grd,iz_grd1-1)),ix_grd,iy_grd)
        vw3 = v_grd(max(1,min(nz_grd,iz_grd1-0)),ix_grd,iy_grd)
        vw4 = v_grd(max(1,min(nz_grd,iz_grd1+1)),ix_grd,iy_grd)

      if ((mod(ix_grd,5).eq.1.and.mod(iy_grd,5).eq.1)
     1.or.nx_grd.lt.10.or.ny_grd.lt.10)
     1print'(11(1x,i6))',ix_grd,iy_grd,iz_grd1
     1,nint(util_invert_1(v_grd2)),nint(util_invert_1(vw2))
     1,nint(util_invert_1(v_grd3)),nint(util_invert_1(vw3))
     1,nint(util_invert_1(v_grd4)),nint(util_invert_1(vw4))
c     1,nint(util_invert_1(v_grd1)),nint(util_invert_1(vw1))

c      write(88,*)' scale=',scale
c      write(88,*)' iz1=',iz1,' iz2=',iz2
c      write(88,*)' iz_grd1=',iz_grd1,' iz_grd2=',iz_grd2
c      write(88,'(11(1x,i6))')ix_grd,iy_grd,iz_grd1
c     1,nint(util_invert_1(v_grd2)),nint(util_invert_1(vw2))
c     1,nint(util_invert_1(v_grd3)),nint(util_invert_1(vw3))
c     1,nint(util_invert_1(v_grd4)),nint(util_invert_1(vw4))
c      write(88,*)' czzz'
c      write(88,*)'  v final v original work fine worksum'
c      write(88,'(1x,i8,1x,f12.2,1x,f12.2,1x,f12.2,1x,f12.2)')
c     1(i,util_invert_1(v_grd(i,ix_grd,iy_grd))
c     1,util_invert_1(work(iv_grd+i-1+(iy_grd-1)*nx_grd*nz_grd+(ix_grd-1
c     1,util_invert_1(work(kv_grd+(i-1)*nz_inc))
c     1,util_invert_1(work(lv_grd+i-1)),i=1,nz_grd)
c      write(88,*)' czzz'
c      write(88,'(1x,i8,1x,f12.2,1x,f12.2)')
c     1(i,util_invert_1(work(iv_grd+i-1))
c     1,util_invert_1(work(kv_grd+i-1)),i=1,nz_grd0)
c      write(88,*)' czzz'

            enddo    ! do ix_grd = 1 , nx_grd

          enddo    ! do iy_grd = 1 , ny_grd

        endif    ! if (jmv .le. 0) then

      enddo    ! do jcv = 1 , ncv

      return

  999 continue
      print'('' error in gtol_replace_grid_1 i_err='',i8)',i_err
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_interpolate(
     1 na1,oa1,da1
     1,na2,oa2,da2
     1,na3,oa3,da3
     1,a
     1,nb1,ob1,db1
     1,nb2,ob2,db2
     1,nb3,ob3,db3
     1,b)
      implicit none
      integer  na1,na2,na3,nb1,nb2,nb3
      real     oa1,da1,oa2,da2,oa3,da3,a(na1,na2,na3)
      real     ob1,db1,ob2,db2,ob3,db3,b(nb1,nb2,nb3)

      integer  ib1,ib2,ib3
      real     b1,b2,b3,a1,a2,a3,da10,da20,da30
      integer  i11,i12,i21,i22,i31,i32
      real     f11,f12,f21,f22,f31,f32

      da10 = da1
      if (da10 .eq. 0.) da10 = 1.
      da20 = da2
      if (da20 .eq. 0.) da20 = 1.
      da30 = da3
      if (da30 .eq. 0.) da30 = 1.

      do ib3 = 1 , nb3

        b3 = (ib3 - 1) * db3 + ob3
        i31 = max(1,min(na3,int((b3-oa3)/da30)+1))
        i32 = max(1,min(na3,i31+int(sign(1.,da30))))
        a3 = (i31 - 1) * da3 + oa3
        f32 = max(0.,min(1.,(b3-a3)/da30))
        f31 = 1. - f32

        do ib2 = 1 , nb2

          b2 = (ib2 - 1) * db2 + ob2
          i21 = max(1,min(na2,int((b2-oa2)/da20)+1))
          i22 = max(1,min(na2,i21+int(sign(1.,da20))))
          a2 = (i21 - 1) * da2 + oa2
          f22 = max(0.,min(1.,(b2-a2)/da20))
          f21 = 1. - f22

          do ib1 = 1 , nb1

            b1 = (ib1 - 1) * db1 + ob1
            i11 = max(1,min(na1,int((b1-oa1)/da10)+1))
            i12 = max(1,min(na1,i11+int(sign(1.,da10))))
            a1 = (i11 - 1) * da1 + oa1
            f12 = max(0.,min(1.,(b1-a1)/da10))
            f11 = 1. - f12

            b(ib1,ib2,ib3) =
     1   f11 * f21 * f31 * a(i11,i21,i31)
     1 + f12 * f21 * f31 * a(i12,i21,i31)
     1 + f11 * f22 * f31 * a(i11,i22,i31)
     1 + f12 * f22 * f31 * a(i12,i22,i31)
     1 + f11 * f21 * f32 * a(i11,i21,i32)
     1 + f12 * f21 * f32 * a(i12,i21,i32)
     1 + f11 * f22 * f32 * a(i11,i22,i32)
     1 + f12 * f22 * f32 * a(i12,i22,i32)
          enddo    ! do ib1 = 1 , nb1
        enddo    ! do ib2 = 1 , nb2
      enddo    ! do ib3 = 1 , nb3

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dl_to_3dg_new(
     1 nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_sum,ny_sum,nz_sum
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd
     1,m_work,work
     1,i_err)

      implicit none

      real     util_invert_1

      integer  ncv
      integer  icv(ncv)
      real     xcv(ncv),ycv(ncv),zcv(ncv)

      integer  nxv
      integer  iv1(nxv),iv2(nxv),iv3(nxv)
      real     xv(nxv),yv(nxv),zv(nxv),vel(nxv)

      integer  nx_sum,ny_sum,nz_sum

      integer  nx_lay
      real     x0_lay,dx_lay

      integer  ny_lay
      real     y0_lay,dy_lay

      integer  nz_lay
      real     z_lay(nx_lay,ny_lay,nz_lay)

      integer  nx_grd
      real     x0_grd,dx_grd

      integer  ny_grd
      real     y0_grd,dy_grd

      integer  nz_grd
      real     z0_grd,dz_grd

      real     v_grd(nz_grd,nx_grd,ny_grd)

      integer  m_work
      real     work(1)

      integer  i_err

      integer  mz_sum
      integer  i_work,n_work

      integer  ix_grd,iy_grd
      integer  i
      real     cpu_a
      real     dx_sum,dy_sum,dz_sum
      real     x_grd,y_grd
      real     v_min,v_max

      integer  i_work_i,i_work_n

      integer  i_v_grd,n_v_grd
      integer  i_z_lay,n_z_lay
      integer  i_iz_hor,n_iz_hor
      integer  i_nz_hor,n_nz_hor
      integer  i_z_hor,n_z_hor
      integer  i_v_hor,n_v_hor

      integer  mz_hor

      integer  i_invert,j_invert
      data     i_invert/0/
      save     i_invert

      print'(/,'' gtol_3dg_to_3dl 3d grid to layer conversion''
     1,/,'' this routine assumes the input velocity functions are''
     1,/,'' on a uniform x,y grid''
     1,/,'' number of surfaces               ='',i8
     1,/,'' number of cell pointers          ='',i8
     1,/,'' number of velocity control points='',i8
     1,/,'' first velocity value             ='',g16.9
     1)'
     1,nz_lay,ncv,nxv,util_invert_1(vel(1))

c  there are nz_lay surfaces and nz_lay+1 layers
c  there should be ncv = nz_lay+1 cell pointers
      call gtol_3dl_to_3dg_get_invert(i_invert)

      mz_sum = (nz_grd - 1) * nz_sum + 1
      dz_sum = dz_grd / nz_sum
      dx_sum = dx_grd / max(1,nx_sum-1)
      dy_sum = dy_grd / max(1,ny_sum-1)

c  initalize velocity grid to zero
      call util_setr(nx_grd*ny_grd*nz_grd,v_grd,0.)

c  determine the number of velocity horizons
      call gtol_pnt0(nxv,iv1,iv2,mz_hor)

      print'(
     1 /,'' number velocity horizons mz_hor='',i8 
     1,/,'' number of layers       nz_lay+1='',i8)',mz_hor,nz_lay+1

c  copy from z_lay to work
      n_z_lay  = nx_grd * ny_grd * nz_lay
      n_iz_hor = nz_lay + 1
      n_nz_hor = nz_lay + 1
      n_z_hor  = nx_grd * ny_grd * mz_hor
      n_v_hor  = nx_grd * ny_grd * mz_hor
      n_v_grd  = mz_sum

c  get the space for the gridded velocity horizons 
c  and the layer to horizon pointers
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,i_z_lay,n_z_lay)    ! z_lay
      call util_work(i_work_i,i_work_n,i_iz_hor,n_iz_hor)  ! iz_hor
      call util_work(i_work_i,i_work_n,i_nz_hor,n_nz_hor)  ! nz_hor
      call util_work(i_work_i,i_work_n,i_z_hor,n_z_hor)    ! z_hor
      call util_work(i_work_i,i_work_n,i_v_hor,n_v_hor)    ! v_hor
      call util_work(i_work_i,i_work_n,i_v_grd,n_v_grd)    ! v_grd
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999
      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)

      print'(/,'' creating 3d gridded model from 3d layered model''
     1,/,'' nx='',i5,'' xmin='',f10.2,'' xmax='',f10.2,'' xinc='',f10.4
     1,/,'' ny='',i5,'' ymin='',f10.2,'' ymax='',f10.2,'' yinc='',f10.4
     1,/,'' nz='',i5,'' zmin='',f10.2,'' zmax='',f10.2,'' zinc='',f10.4
     1,/,'' layered model'',/,'' nz='',i5
     1,/'' nx='',i5,'' xmin='',f10.2,'' xmax='',f10.2,'' xinc='',f10.2
     1,/'' ny='',i5,'' ymin='',f10.2,'' ymax='',f10.2,'' yinc='',f10.2
     1)'
     1,nx_grd,x0_grd,x0_grd+(nx_grd-1)*dx_grd,dx_grd
     1,ny_grd,y0_grd,y0_grd+(ny_grd-1)*dy_grd,dy_grd
     1,nz_grd,z0_grd,z0_grd+(nz_grd-1)*dz_grd,dz_grd
     1,nz_lay
     1,nx_lay,x0_lay,x0_lay+(nx_lay-1)*dx_lay,dx_lay
     1,ny_lay,y0_lay,y0_lay+(ny_lay-1)*dy_lay,dy_lay
      print'('' i='',i5,'' z_lay='',f10.4)'
     1,(i,z_lay(     1,     1,i),i=1,nz_lay)
      print'(
     1 /,'' nz_sum='',i8,'' dz_sum='',f10.4,'' mz_sum='',i8
     1,/,'' nx_sum='',i8,'' dx_sum='',f10.4
     1,/,'' ny_sum='',i8,'' dy_sum='',f10.4
     1,/,'' m_work='',i8,'' n_work='',i8
     1)'
     1,nz_sum,dz_sum,mz_sum
     1,nx_sum,dx_sum
     1,ny_sum,dy_sum
     1,m_work,n_work

c  interpolate z_lay to nx_grd
      call util_interpolate_1(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,0.,1.
     1,z_lay
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_lay,0.,1.
     1,work(i_z_lay)
     1,n_work,work(i_work)
     1,i_err)
      if (i_err .ne. 0) goto 999

c  compute the gridded horizon velocity and depths 
c  and the layer to grid pointers
c      print'(/,'' before gtol_3dl_to_3dg_b_new4 mz_hor='',i8)',mz_hor
      call gtol_3dl_to_3dg_b_new(
     1 nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_lay,work(i_z_lay)
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,z_lay
     1,mz_hor
     1,work(i_iz_hor),work(i_nz_hor)
     1,work(i_z_hor),work(i_v_hor)
     1,n_work,work(i_work)
     1,i_err)
      if (i_err .ne. 0) goto 999

c      print'('' after gtol_3dl_to_3dg_b_new mz_hor='',i8)',mz_hor

c      if (ncv .ne. -999) stop

      i_err = 0

      do ix_grd = 1 , nx_grd

        do iy_grd = 1 , ny_grd

c  grid up this x,y location on a finer z grid
          call gtol_3dl_to_3dg_a_new(
     1 ix_grd,nx_sum,dx_sum
     1,iy_grd,ny_sum,dy_sum
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_lay
     1,work(i_z_lay)
     1,mz_hor
     1,work(i_iz_hor),work(i_nz_hor)
     1,work(i_z_hor),work(i_v_hor)
     1,mz_sum,z0_grd,dz_sum,work(i_v_grd)
     1,n_work,work(i_work),i_err)
          if (i_err .ne. 0) goto 999

c      if (ix_grd .eq. 1 .and. iy_grd .eq. 1)
c     1print'('' v='',f10.2,1x,f10.2)'
c     1,util_invert_1(work(i_v_grd)/nz_sum)
c     1,util_invert_1(work(i_v_grd+mz_sum-1)/nz_sum)

          call gtol_ave_sum(1.,mz_sum,nz_sum,work(i_v_grd)
     1,v_grd(1,ix_grd,iy_grd))


      cpu_a = 0.
      if (nx_grd .eq. 1) then
c        call second(cpu_a)

        if (i_invert .eq. 0) then

          print'('' i='',i6,'' y='',f10.2,'' c='',f10.4
     1,'' v='',6(1x,f6.0))'
     1,iy_grd,(iy_grd-1)*dy_grd+y0_grd,cpu_a
     1,(util_invert_1(v_grd(i,ix_grd,iy_grd))
     1,i=1,nz_grd,max(1,nz_grd/5))

c      call util_min_max(v_min,v_max,nz_grd,v_grd(1,ix_grd,iy_grd))
c      print'('' v_min='',f10.4,'' v_max='',f10.4)'
c     1,util_invert_1(v_max),util_invert_1(v_min)

        else    ! if (i_invert .eq. 0) then    ! if (i_invert .eq. 0) then

          print'('' i='',i6,'' y='',f10.2,'' c='',f10.4
     1,'' v='',6(1x,f6.0))'
     1,iy_grd,(iy_grd-1)*dy_grd+y0_grd,cpu_a
     1,((v_grd(i,ix_grd,iy_grd))
     1,i=1,nz_grd,max(1,nz_grd/5))

        endif    ! if (i_invert .eq. 0) then    ! if (i_invert .eq. 0) then

      endif    ! if (nx_grd .eq. 1) then
        enddo    ! do iy_grd = 1 , ny_grd
          if (i_err .ne. 0) goto 999

c        call second(cpu_a)

        if (i_invert .eq. 0) then

          print'('' i='',i6,'' x='',f10.2,'' c='',f10.4
     1,'' v='',6(1x,f6.0))'
     1,ix_grd,(ix_grd-1)*dx_grd+x0_grd,cpu_a
     1,(util_invert_1(v_grd(i,ix_grd,ny_grd/2+1))
     1,i=1,nz_grd,max(1,nz_grd/5))

        else    ! if (i_invert .eq. 0) then    ! if (i_invert .eq. 0) then

          print'('' i='',i6,'' x='',f10.2,'' c='',f10.4
     1,'' v='',6(1x,f6.0))'
     1,ix_grd,(ix_grd-1)*dx_grd+x0_grd,cpu_a
     1,((v_grd(i,ix_grd,ny_grd/2+1))
     1,i=1,nz_grd,max(1,nz_grd/5))

        endif    ! if (i_invert .eq. 0) then    ! if (i_invert .eq. 0) then

      enddo    ! do ix_grd = 1 , nx_grd

      return

  999 continue
      print'('' error in gtol_3dl_to_3dg i_err='',i8)',i_err
      if (i_err .ne. 0) stop
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry gtol_3dl_to_3dg_new_put_invert(j_invert)
      call gtol_3dl_to_3dg_put_invert(j_invert)
      i_invert = j_invert
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry gtol_3dl_to_3dg_new_get_invert(j_invert)
      call gtol_3dl_to_3dg_get_invert(j_invert)
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dl_to_3dg_a_new(
     1 ix_grd,nx_sum,dx_sum
     1,iy_grd,ny_sum,dy_sum
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_lay,z_lay
     1,mz_hor
     1,iz_hor,nz_hor
     1,z_hor,v_hor
     1,nz_grd,z0_grd,dz_grd
     1,v_sum
     1,m_work,work
     1,i_err)
      implicit none

      real     util_invert_1

      integer  ix_grd,nx_sum
      real     dx_sum

      integer  iy_grd,ny_sum
      real     dy_sum

      integer  nx_grd
      real     x0_grd,dx_grd

      integer  ny_grd
      real     y0_grd,dy_grd

      integer  nz_lay
      real     z_lay(nx_grd,ny_grd,nz_lay)

      integer  mz_hor
      integer  iz_hor(nz_lay+1),nz_hor(nz_lay+1)
      real     z_hor(nx_grd,ny_grd,mz_hor)
      real     v_hor(nx_grd,ny_grd,mz_hor)

      integer  nz_grd
      real     z0_grd,dz_grd

      real     v_sum(nz_grd)

      integer  m_work
      real     work(1)

      integer  i_err,j_err

      integer  iz_lay,nx,i
      real     z1,z2,w1,w2

      integer  ix_grd_1,ix_grd_2,ix_sum
      real     x_grd,x_grd_1,fx_grd_1,fx_grd_2

      integer  iy_grd_1,iy_grd_2,iy_sum
      real     y_grd,y_grd_1,fy_grd_1,fy_grd_2

      integer  iz_grd,jz_hor
      integer  iz_grd_1,iz_grd_2
      integer  jz_grd_1,jz_grd_2
      real     z_lay_0,z_hor_0
      real     z_hor_1,z_hor_2
      real     v_hor_1,v_hor_2
      real     v_hor_0
      real     dv_dz
      real     v_scale

              if (
     1(     ix_grd .eq. 1      .and. iy_grd .eq. 1     )
     1.or.
     1(     ix_grd .eq. nx_grd .and. iy_grd .eq. ny_grd)
     1)
     1print'(
     1 /,'' gtol_3dl_to_3dg_a_new''
     1,/,'' nx_sum='',i8,'' dx_sum='',f10.4
     1,/,'' ny_sum='',i8,'' dy_sum='',f10.4
     1,/,'' nz_lay='',i8
     1,/,'' nx_grd='',i8,'' x0_grd='',f10.4,'' dx_grd='',f10.4
     1,/,'' ny_grd='',i8,'' y0_grd='',f10.4,'' dy_grd='',f10.4
     1,/,'' nz_grd='',i8,'' z0_grd='',f10.4,'' dz_grd='',f10.4
     1)'
     1,nx_sum,dx_sum
     1,ny_sum,dy_sum
     1,nz_lay
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd

c  initialize the velocity sum to zero
      call util_setr(nz_grd,v_sum,0.)

      do iy_sum = -ny_sum/2 , ny_sum/2

        y_grd = (iy_grd - 1) * dy_grd + y0_grd + iy_sum * dy_sum

        iy_grd_1 = max(1,min(ny_grd,int((y_grd-y0_grd)/dy_grd)+1))
        iy_grd_2 = max(1,min(ny_grd,iy_grd_1+1))

        y_grd_1 = (iy_grd_1 - 1) * dy_grd + y0_grd

        fy_grd_2 = max(0.,min(1.,(y_grd-y_grd_1)/dy_grd))
        fy_grd_1 = 1. - fy_grd_2

        do ix_sum = -nx_sum/2 , nx_sum/2

          x_grd = (ix_grd - 1) * dx_grd + x0_grd + ix_sum * dx_sum

          ix_grd_1 = max(1,min(nx_grd,int((x_grd-x0_grd)/dx_grd)+1))
          ix_grd_2 = max(1,min(nx_grd,ix_grd_1+1))

          x_grd_1 = (ix_grd_1 - 1) * dx_grd + x0_grd

          fx_grd_2 = max(0.,min(1.,(x_grd-x_grd_1)/dx_grd))
          fx_grd_1 = 1. - fx_grd_2

c  for each layer
          iz_grd_1 = 1

          do iz_lay = 1 , nz_lay+1

c      print*,' ix_grd=',ix_grd,' iy_grd=',iy_grd
c     1,' iz_lay=',iz_lay
c      print*,' iz_hor=',iz_hor(iz_lay),' nz_hor=',nz_hor(iz_lay)

            if (iz_lay .eq. nz_lay+1) then

              iz_grd_2 = nz_grd

            else    ! if (iz_lay .eq. nz_lay+1) then

              z_lay_0 =
     1   fx_grd_1 * fy_grd_1 * z_lay(ix_grd_1,iy_grd_1,iz_lay)
     1 + fx_grd_2 * fy_grd_1 * z_lay(ix_grd_2,iy_grd_1,iz_lay)
     1 + fx_grd_1 * fy_grd_2 * z_lay(ix_grd_1,iy_grd_2,iz_lay)
     1 + fx_grd_2 * fy_grd_2 * z_lay(ix_grd_2,iy_grd_2,iz_lay)

              iz_grd_2 = min(nz_grd,int((z_lay_0-z0_grd)/dz_grd)+1)

            endif    ! if (iz_lay .eq. nz_lay+1) then

            jz_grd_1 = iz_grd_1

            do jz_hor = iz_hor(iz_lay) , iz_hor(iz_lay)+nz_hor(iz_lay)-1

              z_hor_2 =
     1   fx_grd_1 * fy_grd_1 * v_hor(ix_grd_1,iy_grd_1,jz_hor)
     1 + fx_grd_2 * fy_grd_1 * v_hor(ix_grd_2,iy_grd_1,jz_hor)
     1 + fx_grd_1 * fy_grd_2 * v_hor(ix_grd_1,iy_grd_2,jz_hor)
     1 + fx_grd_2 * fy_grd_2 * v_hor(ix_grd_2,iy_grd_2,jz_hor)

              v_hor_2 =
     1   fx_grd_1 * fy_grd_1 * v_hor(ix_grd_1,iy_grd_1,jz_hor)
     1 + fx_grd_2 * fy_grd_1 * v_hor(ix_grd_2,iy_grd_1,jz_hor)
     1 + fx_grd_1 * fy_grd_2 * v_hor(ix_grd_1,iy_grd_2,jz_hor)
     1 + fx_grd_2 * fy_grd_2 * v_hor(ix_grd_2,iy_grd_2,jz_hor)

              if (jz_hor .eq. iz_hor(iz_lay)+nz_hor(iz_lay)-1) then

                jz_grd_2 = iz_grd_2
                v_hor_1  = v_hor_2
                z_hor_1  = z_hor_2

              elseif (jz_hor .eq. iz_hor(iz_lay)) then

                jz_grd_2 = min(nz_grd,int((z_hor_2-z0_grd)/dz_grd)+1)
                v_hor_1  = v_hor_2
                z_hor_1  = z_hor_2

              else    ! if (jz_hor .eq. iz_hor(iz_lay)) then

                jz_grd_2 = min(nz_grd,int((z_hor_2-z0_grd)/dz_grd)+1)

              endif    ! if (jz_hor .eq. iz_hor(iz_lay)+nz_hor(iz_lay)-1) then

              if (z_hor_1 .ne. z_hor_2) then

                dv_dz = (v_hor_2 - v_hor_1) / (z_hor_2 - z_hor_1)

              else    ! if (z_hor_1 .ne. z_hor_2) then

                dv_dz = 0.

              endif    ! ! if (z_hor_1 .ne. z_hor_2) then

              z_hor_0 = (jz_grd_1 - 1) * dz_grd + z0_grd
              v_hor_0 = v_hor_1 + (z_hor_0 - z_hor_1) * dv_dz
              dv_dz = dv_dz / dz_grd

c      if  (ix_grd .eq. 1 .and. iy_grd .eq. 1)
c     1print*,' iz_lay=',iz_lay
c     1,' iz=',iz_grd_1,iz_grd_1
c     1,' jz=',jz_grd_1,jz_grd_1
c      if  (ix_grd .eq. 1 .and. iy_grd .eq. 1)
c     1print*,' z=',z_hor_1,z_hor_2,z_hor_0
c      if  (ix_grd .eq. 1 .and. iy_grd .eq. 1)
c     1print*,' v=',v_hor_1,v_hor_2,v_hor_0

              do iz_grd = jz_grd_1 , jz_grd_2

                v_sum(iz_grd) = v_sum(iz_grd) 
     1+ v_hor_0 + (iz_grd - jz_grd_1) * dv_dz
c      if (util_invert_1(v_sum(iz_grd)) .lt. .001) then
c      print*,' iz_grd=',iz_grd,' v_sum=',v_sum(iz_grd)
c     1,v_hor_0 + (iz_grd - jz_grd_1) * dv_dz
c      print*,' iz_lay=',iz_lay
c     1,' iz=',iz_grd_1,iz_grd_1
c     1,' jz=',jz_grd_1,jz_grd_1
c      print*,' z=',z_hor_1,z_hor_2,z_hor_0
c      print*,' v=',v_hor_1,v_hor_2,v_hor_0
c      stop
c      endif
              enddo    ! do iz_grd = jz_grd_1 , jz_grd_2

              if (
     1(     ix_grd .eq. 1      .and. iy_grd .eq. 1
     1.and. ix_sum .eq. 0      .and. iy_sum .eq. 0      )
     1.or.
     1(     ix_grd .eq. nx_grd .and. iy_grd .eq. ny_grd
     1.and. ix_sum .eq. 0      .and. iy_sum .eq. 0      )
     1)
     1print'('' l='',i5,'' h='',i5
     1,'' z1='',i5,'' z2='',i5
     1,'' z='',f10.4,'' vh='',f7.0
     1,'' v1='',f7.0,'' v2='',f7.0
     1)'
     1,iz_lay,iz_hor(iz_lay),iz_grd_1,iz_grd_2
     1,z_lay_0
     1,util_invert_1(v_hor_0)
     1,util_invert_1(v_sum(iz_grd_1))
     1,util_invert_1(v_sum(iz_grd_2))

              z_hor_1  = z_hor_2
              v_hor_1  = v_hor_2
              jz_grd_1 = jz_grd_2 + 1

            enddo    ! do jz_hor = iz_hor(iz_lay) , 

            iz_grd_1 = iz_grd_2 + 1

          enddo    ! do iz_lay = 1 , nz_lay+1

        enddo    ! do ix_sum = -nx_sum/2 , nx_sum/2

      enddo    ! do iy_sum = -ny_sum/2 , ny_sum/2

c  scale by the number of point sin the sum
      v_scale = 1. / (nx_sum*ny_sum)
      call util_scale(nz_grd,v_sum,v_scale)

c  check for unset velocities
      j_err = 0
      do iz_grd = 1 , nz_grd

        if (util_invert_1(v_sum(iz_grd)) .lt. .001 
     1.and. j_err .eq. 0) 
     1print*,' error - found a zero velocity'
     1,' ix_grd=',ix_grd,' iy_grd=',iy_grd,' iz_grd=',iz_grd
     1,' v_sum=',v_sum(iz_grd),util_invert_1(v_sum(iz_grd))

        if (util_invert_1(v_sum(iz_grd)) .lt. .001) 
     1j_err = j_err -1

      enddo    ! do iz_grd = 1 , nz_grd
      if (j_err .ne. 0) print*,' nz_grd=',nz_grd,' j_err=',j_err
      i_err = j_err

      return

  999 continue
      print'('' error in gtol_3dl_to_3dg_a_new i_err='',i8)',i_err
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dl_to_3dg_b_new(
     1 nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_lay,z_lay
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,z_old
     1,mz_hor
     1,iz_hor,nz_hor
     1,z_hor,v_hor
     1,m_work,work
     1,i_err)
c  compute the gridded horizon velocity and depths 
c  and the layer to grid pointers

      implicit none

      real     util_invert_1

      integer  ncv
      integer  icv(ncv)
      real     xcv(ncv),ycv(ncv),zcv(ncv)

      integer  nxv
      integer  iv1(nxv),iv2(nxv),iv3(nxv)
      real     xv(nxv),yv(nxv),zv(nxv),vel(nxv)

      integer  nx_grd
      real     x0_grd,dx_grd

      integer  ny_grd
      real     y0_grd,dy_grd

      integer  nz_lay
      real     z_lay(nx_grd,ny_grd,nz_lay)

      integer  nx_lay
      real     x0_lay,dx_lay

      integer  ny_lay
      real     y0_lay,dy_lay

      real     z_old(nx_lay,ny_lay,nz_lay)

      integer  mz_hor
      integer  iz_hor(nz_lay+1),nz_hor(nz_lay+1)
      real     z_hor(nx_grd,ny_grd,mz_hor)
      real     v_hor(nx_grd,ny_grd,mz_hor)

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  i_work_i,i_work_n
      integer  i_iyi,n_iyi
      integer  i_nyi,n_nyi
      integer  i_ixi,n_ixi
      integer  i_nxi,n_nxi
      integer  i_work,n_work

      integer  jcv
      integer  iz_lay,ivp,nvp
      integer  iz_top,iz_bot
      real     z_top,z_bot
      real     w_top,w_bot

      real     xh_min,xh_max
      real     yh_min,yh_max
      real     zh_min,zh_max
      real     vh_min,vh_max

c      print'(/,'' gtol_3dl_to_3dg_b_new mz_hor='',i8)',mz_hor

      i_err = 0

c  for each cell pointer
c  1 identify the cell this pointer is in
c  2 identify which velocity horizons are used for this layer
c  3 compute z_grd and v_grd for the velocity horizons
      do jcv = 1 , ncv

c      print'(/,'' cell pointer jcv='',i10,'' icv='',i8)'
c     1,jcv,icv(jcv)

c  get the layer containing this cell pointer
        call gtol_interpolate(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,0.,1.
     1,z_old
     1,1,xcv(jcv),1.
     1,1,ycv(jcv),1.
     1,nz_lay,0.,1.
     1,work)

        call fitcell1(zcv(jcv),0.,nz_lay,work
     1,iz_top,iz_bot,w_top,w_bot)

c      print*,' zcv=',zcv(jcv),' iz_top=',iz_top,' iz_bot=',iz_bot
c      do iz_lay = 1 , nz_lay
c      print*,' iz_lay=',iz_lay,' z=',work(iz_lay)
c      enddo

        z_top = work(iz_top)
        z_bot = work(iz_bot)

        iz_lay = iz_top + 1

        if (zcv(jcv) .lt. z_top) iz_lay = iz_lay - 1

c  identify the first and last velocity control points which are in this layer
c  and which velocity horizons they belong to
c      print'('' bef gtol_pnt1 jcv='',i5
c     1,'' icv='',i8,'' iz_lay='',i8
c     1,/,'' iz_top='',i8,'' iz_bot='',i8
c     1,/,'' xcv='',f10.2,'' ycv='',f10.2,'' zcv='',f10.2
c     1,/,'' z_top='',f10.2,'' z_bot='',f10.2)'
c     1,jcv,icv(jcv),iz_lay,iz_top,iz_bot
c     1,xcv(jcv),ycv(jcv),zcv(jcv),z_top,z_bot

        call gtol_pnt1(icv(jcv),nxv,iv1,iv2
     1,ivp,nvp,iz_hor(iz_lay),nz_hor(iz_lay))

c      print'('' aft gtol_pnt1 iz_lay='',i8
c     1,'' ivp='',i8,'' nvp='',i8
c     1,'' iz_hor='',i8,'' nz_hor='',i8)'
c     1,iz_lay,ivp,nvp,iz_hor(iz_lay),nz_hor(iz_lay)
      call util_min_max(xh_min,xh_max,nvp,xv(ivp))
      call util_min_max(yh_min,yh_max,nvp,yv(ivp))
      call util_min_max(zh_min,zh_max,nvp,zv(ivp))
      call util_min_max(vh_min,vh_max,nvp,vel(ivp))

      print'(/,'' assigning velocity horizons to layers''
     1,/,'' jcv='',i5,'' icv='',i8,'' iz_lay='',i8
     1,/,'' iz_top='',i8,'' iz_bot='',i8
     1,/,'' ivp='',i8,'' nvp='',i8
     1,'' iz_hor='',i8,'' nz_hor='',i8
     1,/,'' xcv='',f10.2,'' ycv='',f10.2,'' zcv='',f10.2
     1,/,'' z_top='',f10.2,'' z_bot='',f10.2)'
     1,jcv,icv(jcv),iz_lay,iz_top,iz_bot
     1,ivp,nvp,iz_hor(iz_lay),nz_hor(iz_lay)
     1,xcv(jcv),ycv(jcv),zcv(jcv),z_top,z_bot
      print'(
     1   '' xh_min='',f10.2,'' xh_max='',f10.2
     1,/,'' yh_min='',f10.2,'' yh_max='',f10.2
     1,/,'' zh_min='',f10.2,'' zh_max='',f10.2
     1,/,'' vh_min='',f10.2,'' vh_max='',f10.2
     1)'
     1,xh_min,xh_max
     1,yh_min,yh_max
     1,zh_min,zh_max
     1,util_invert_1(vh_max),util_invert_1(vh_min)

        if (iz_hor(iz_lay) .gt. mz_hor
     1 .or. iz_hor(iz_lay)+nz_hor(iz_lay)-1 .gt. mz_hor) then

          print'(/,'' error in gtol_3dl_to_3dg_b_new''
     1,/,'' nz_lay='',i8,'' mz_hor='',i8
     1,'' iz_hor='',i8,'' nz_hor='',i8)'
     1,iz_lay,mz_hor,iz_hor(iz_lay),nz_hor(iz_lay)
          stop

        endif    ! if (iz_hor(iz_lay) .gt. mz_hor

c  get space for pointers
        n_iyi = nvp
        n_nyi = nvp
        n_ixi = nvp
        n_nxi = nvp

        call util_wors(i_work_i,i_work_n,m_work)
        call util_work(i_work_i,i_work_n,i_iyi,n_iyi) ! 1st point in each line
        call util_work(i_work_i,i_work_n,i_nyi,n_nyi) ! # of lines in each layer
        call util_work(i_work_i,i_work_n,i_ixi,n_ixi) ! 1st point in each layer
        call util_work(i_work_i,i_work_n,i_nxi,n_nxi) ! # of points in each layer
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 999
        call util_worl(i_work_i,i_work_n,n_work)
        call util_work(i_work_i,i_work_n,i_work,n_work) ! work

c  compute the gridded velocity horizon depth and velocities
c      print'(/,'' before gtol_3dl_to_3dg_c_new''
c     1,'' iz_lay='',i8
c     1,'' ivp='',i8,'' nvp='',i8
c     1,'' iz_hor='',i8,'' nz_hor='',i8)'
c     1,iz_lay,ivp,nvp,iz_hor(iz_lay),nz_hor(iz_lay)

        call gtol_3dl_to_3dg_c_new(
     1 nvp,iv2(ivp),iv3(ivp)
     1,xv(ivp),yv(ivp),zv(ivp),vel(ivp)
     1,nz_hor(iz_lay),work(i_iyi),work(i_nyi),work(i_ixi),work(i_nxi)
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,z_hor(1,1,iz_hor(iz_lay))
     1,v_hor(1,1,iz_hor(iz_lay))
     1,n_work,work(i_work)
     1,i_err)
        if (i_err .ne. 0) goto 999

      enddo    ! do jcv = 1 , ncv

      return

  999 continue
      print'(/,'' error in gtol_3dl_to_3dg_b_new'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dl_to_3dg_c_new(
     1 nvp,iv2,iv3
     1,xi,yi,zi,vi
     1,mz_hor,iyi,nyi,ixi,nxi
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,z_hor,v_hor
     1,m_work,work
     1,i_err)
      implicit none

      real     util_invert_1

      integer  nvp
      integer  iv2(nvp),iv3(nvp)

      integer  mz_hor
      integer  iyi(mz_hor),nyi(mz_hor),ixi(mz_hor),nxi(mz_hor)
      real     xi(nvp),yi(nvp),zi(nvp),vi(nvp)

      integer  nx_grd
      real     x0_grd,dx_grd

      integer  ny_grd
      real     y0_grd,dy_grd

      real     z_hor(nx_grd,ny_grd,mz_hor)
      real     v_hor(nx_grd,ny_grd,mz_hor)

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  nz_hor,iz_hor,ix_1,ix_2,iy_1,iy_2,i

      integer  i_work_i,i_work_n,n_work
      integer  i_yofy,n_yofy
      integer  i_zofx,n_zofx
      integer  i_zofy,n_zofy
      integer  i_z_xy,n_z_xy

c      print'(/,'' gtol_3dl_to_3dg_c_new''
c     1,/,'' nvp='',i8,'' mz_hor='',i8
c     1,/,'' iv2(1)='',i8,'' iv2(nvp)='',i8
c     1,/,'' iv3(1)='',i8,'' iv3(nvp)='',i8
c     1)'
c     1,nvp,mz_hor
c     1,iv2(1),iv2(nvp)
c     1,iv3(1),iv3(nvp)

c      write(88,'('' nvp='',i8)')nvp
c      write(88,'(1x,i8,1x,i8,1x,i8
c     1,1x,f8.0,1x,f8.0,1x,f8.4,1x,f8.0)')
c     1(i,iv2(i),iv3(i),xi(i),yi(i),zi(i),1./vi(i),i=1,nvp)

c  compute the velocity horizon pointers for these velocity horizons
      call fitcpnt2(nvp,iv2,iv3,mz_hor,nz_hor,iyi,nyi,ixi,nxi)

      if (nz_hor .ne. mz_hor) goto 997

      do iz_hor = 1 , nz_hor

c  there are nyi y values at this z horizon
        iy_1  = iyi(iz_hor) + 1                ! # of model y values
        iy_2  = iyi(iz_hor) + nyi(iz_hor)      ! # of model y values
        ix_1  = ixi(iy_1) + 1                  ! # of model values - ki
        ix_2  = ixi(iy_2) + nxi(iy_2)          ! # of model values - ki

c  assign memory
        n_yofy = nyi(iz_hor)
        n_zofx = nyi(iz_hor)
        n_zofy = nx_grd
        n_z_xy = nx_grd * nyi(iz_hor)

        call util_wors(i_work_i,i_work_n,m_work)
        call util_work(i_work_i,i_work_n,i_yofy,n_yofy)
        call util_work(i_work_i,i_work_n,i_zofx,n_zofx)
        call util_work(i_work_i,i_work_n,i_zofy,n_zofy)
        call util_work(i_work_i,i_work_n,i_z_xy,n_z_xy)
        call util_woru(i_work_i,i_work_n,n_work)
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 998

c      print*,' '
c      print*,' nz_hor=',nz_hor,' iz_hor=',iz_hor
c      print*,' iy_1=',iy_1,' iy_2=',iy_2
c      print*,' ix_1=',ix_1,' ix_2=',ix_2
c      print*,' iyi=',iyi(iz_hor),' nyi=',nyi(iz_hor)
c      print*,' ixi1=',ixi(iy_1),' nxi=',nxi(iy_1)
c      print*,' ixi2=',ixi(iy_2),' nxi=',nxi(iy_2)
c      print*,' mxi=',ixi(iy_2)+nxi(iy_2)

c  interpolate depths
        call util_interpolate_i_to_r_2d(
     1 nyi(iz_hor),ixi(iy_1),nxi(iy_1),nvp
     1,xi,yi,zi
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,z_hor(1,1,iz_hor)
     1,work(i_yofy)
     1,work(i_zofx)
     1,work(i_zofy)
     1,work(i_z_xy)
     1)

c  interpolate velocity
        call util_interpolate_i_to_r_2d(
     1 nyi(iz_hor),ixi(iy_1),nxi(iy_1),nvp
     1,xi,yi,vi
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,v_hor(1,1,iz_hor)
     1,work(i_yofy)
     1,work(i_zofx)
     1,work(i_zofy)
     1,work(i_z_xy)
     1)

c        call util_min_max(v0_inp,v1_inp
c     1,ix_2-ix_1+1,vi(ix_1))

c        call util_min_max(v0_out,v1_out
c     1,nx_grd*ny_grd,v_hor(1,1,iz_hor))

c      print'(/,'' iz_hor='',i8,'' iy_1='',i8
c     1,'' ix_1='',i8,'' ix_2='',i8
c     1,/,''  input v min='',f10.2,'' v_max='',f10.2
c     1,/,'' output v min='',f10.2,'' v_max='',f10.2
c     1)'
c     1,iz_hor,iy_1,ix_1,ix_2
c     1,util_invert_1(v1_inp),util_invert_1(v0_inp)
c     1,util_invert_1(v1_out),util_invert_1(v0_out)

        ix_1 = ix_2 + 1

      enddo    ! do iz_hor = 1 , nz_hor

      return

  997 continue
      print'(/,'' error in gtol_3dl_to_3dg_c_new''
     1,/,'' mz_hor='',i8,'' nz_hor='',i8)',mz_hor,nz_hor
       goto 999

  998 continue
      print'(/,'' error in gtol_3dl_to_3dg_c_new''
     1,/,'' need more meory m_work='',i8,'' n_work='',i8)'
      goto 999

  999 continue
      print'('' error in gtol_3dl_to_3dg_c_new'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dl_to_3dg_b_old(
     1 nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_lay,z_lay
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,z_old
     1,mz_hor
     1,iz_hor,nz_hor
     1,z_hor,v_hor
     1,m_work,work
     1,i_err)
c  compute the gridded horizon velocity and depths 
c  and the layer to grid pointers

      implicit none

      real     util_invert_1

      integer  ncv
      integer  icv(ncv)
      real     xcv(ncv),ycv(ncv),zcv(ncv)

      integer  nxv
      integer  iv1(nxv),iv2(nxv),iv3(nxv)
      real     xv(nxv),yv(nxv),zv(nxv),vel(nxv)

      integer  nx_grd
      real     x0_grd,dx_grd

      integer  ny_grd
      real     y0_grd,dy_grd

      integer  nz_lay
      real     z_lay(nx_grd,ny_grd,nz_lay)

      integer  nx_lay
      real     x0_lay,dx_lay

      integer  ny_lay
      real     y0_lay,dy_lay

      real     z_old(nx_lay,ny_lay,nz_lay)

      integer  mz_hor
      integer  iz_hor(nz_lay+1),nz_hor(nz_lay+1)
      real     z_hor(nx_grd,ny_grd,mz_hor)
      real     v_hor(nx_grd,ny_grd,mz_hor)

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  i_work_i,i_work_n
      integer  i_iyi,n_iyi
      integer  i_nyi,n_nyi
      integer  i_ixi,n_ixi
      integer  i_nxi,n_nxi
      integer  i_work,n_work

      integer  jcv
      integer  iz_lay,ivp,nvp
      integer  iz_top,iz_bot
      real     z_top,z_bot
      real     w_top,w_bot

      real     xh_min,xh_max
      real     yh_min,yh_max
      real     zh_min,zh_max
      real     vh_min,vh_max

c      print'(/,'' gtol_3dl_to_3dg_b_old mz_hor='',i8)',mz_hor

      i_err = 0

c  for each cell pointer
c  1 identify the cell this pointer is in
c  2 identify which velocity horizons are used for this layer
c  3 compute z_grd and v_grd for the velocity horizons
      do jcv = 1 , ncv

c      print'(/,'' cell pointer jcv='',i10,'' icv='',i8)'
c     1,jcv,icv(jcv)

c  get the layer containing this cell pointer
        call gtol_interpolate(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,0.,1.
     1,z_old
     1,1,xcv(jcv),1.
     1,1,ycv(jcv),1.
     1,nz_lay,0.,1.
     1,work)

        call fitcell1(zcv(jcv),0.,nz_lay,work
     1,iz_top,iz_bot,w_top,w_bot)

        z_top = work(iz_top)
        z_bot = work(iz_bot)

        iz_lay = iz_top + 1

        if (zcv(jcv) .lt. z_top) iz_lay = iz_lay - 1

c  identify the first and last velocity control points which are in this layer
c  and which velocity horizons they belong to
c      print'('' bef gtol_pnt1 jcv='',i5,'' icv='',i8,'' iz_lay='',i8
c     1,/,'' iz_top='',i8,'' iz_bot='',i8
c     1,/,'' xcv='',f10.2,'' ycv='',f10.2,'' zcv='',f10.2
c     1,/,'' z_top='',f10.2,'' z_bot='',f10.2)'
c     1,jcv,icv(jcv),iz_lay,iz_top,iz_bot
c     1,xcv(jcv),ycv(jcv),zcv(jcv),z_top,z_bot

        call gtol_pnt1(icv(jcv),nxv,iv1,iv2
     1,ivp,nvp,iz_hor(iz_lay),nz_hor(iz_lay))

c      print'('' aft gtol_pnt1 iz_lay='',i8
c     1,'' ivp='',i8,'' nvp='',i8
c     1,'' iz_hor='',i8,'' nz_hor='',i8)'
c     1,iz_lay,ivp,nvp,iz_hor(iz_lay),nz_hor(iz_lay)
c      print'('' aft gtol_pnt1 jcv='',i5,'' icv='',i8,'' iz_lay='',i8
      call util_min_max(xh_min,xh_max,nvp,xv(ivp))
      call util_min_max(yh_min,yh_max,nvp,yv(ivp))
      call util_min_max(zh_min,zh_max,nvp,zv(ivp))
      call util_min_max(vh_min,vh_max,nvp,vel(ivp))

      print'(/,'' assigning velocity horizons to layers''
     1,/,'' jcv='',i5,'' icv='',i8,'' iz_lay='',i8
     1,/,'' iz_top='',i8,'' iz_bot='',i8
     1,/,'' ivp='',i8,'' nvp='',i8
     1,'' iz_hor='',i8,'' nz_hor='',i8
     1,/,'' xcv='',f10.2,'' ycv='',f10.2,'' zcv='',f10.2
     1,/,'' z_top='',f10.2,'' z_bot='',f10.2)'
     1,jcv,icv(jcv),iz_lay,iz_top,iz_bot
     1,ivp,nvp,iz_hor(iz_lay),nz_hor(iz_lay)
     1,xcv(jcv),ycv(jcv),zcv(jcv),z_top,z_bot
      print'(
     1   '' xh_min='',f10.2,'' xh_max='',f10.2
     1,/,'' yh_min='',f10.2,'' yh_max='',f10.2
     1,/,'' zh_min='',f10.2,'' zh_max='',f10.2
     1,/,'' vh_min='',f10.2,'' vh_max='',f10.2
     1)'
     1,xh_min,xh_max
     1,yh_min,yh_max
     1,zh_min,zh_max
     1,util_invert_1(vh_max),util_invert_1(vh_min)

        if (iz_hor(iz_lay) .gt. mz_hor
     1 .or. iz_hor(iz_lay)+nz_hor(iz_lay)-1 .gt. mz_hor) then

          print'(/,'' error in gtol_3dl_to_3dg_b_old''
     1,/,'' nz_lay='',i8,'' mz_hor='',i8
     1,'' iz_hor='',i8,'' nz_hor='',i8)'
     1,iz_lay,mz_hor,iz_hor(iz_lay),nz_hor(iz_lay)
          stop

        endif    ! if (iz_hor(iz_lay) .gt. mz_hor

c  get space for pointers
        n_iyi = nz_hor(iz_lay)
        n_nyi = nz_hor(iz_lay)
        n_ixi = nz_hor(iz_lay)
        n_nxi = nz_hor(iz_lay)

        call util_wors(i_work_i,i_work_n,m_work)
        call util_work(i_work_i,i_work_n,i_iyi,n_iyi) ! 1st point in each line
        call util_work(i_work_i,i_work_n,i_nyi,n_nyi) ! # of lines in each layer
        call util_work(i_work_i,i_work_n,i_ixi,n_ixi) ! 1st point in each layer
        call util_work(i_work_i,i_work_n,i_nxi,n_nxi) ! # of points in each layer
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 999
        call util_worl(i_work_i,i_work_n,n_work)
        call util_work(i_work_i,i_work_n,i_work,n_work) ! work

c  compute the gridded velocity horizon depth and velocities
c      print'(/,'' before gtol_3dl_to_3dg_c_old''
c     1,'' iz_lay='',i8
c     1,'' ivp='',i8,'' nvp='',i8
c     1,'' iz_hor='',i8,'' nz_hor='',i8)'
c     1,iz_lay,ivp,nvp,iz_hor(iz_lay),nz_hor(iz_lay)

        call gtol_3dl_to_3dg_c_old(
     1 nvp,iv2(ivp),iv3(ivp)
     1,xv(ivp),yv(ivp),zv(ivp),vel(ivp)
     1,nz_hor(iz_lay),work(i_iyi),work(i_nyi),work(i_ixi),work(i_nxi)
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,z_hor(1,1,iz_hor(iz_lay))
     1,v_hor(1,1,iz_hor(iz_lay))
     1,n_work,work(i_work)
     1,i_err)
        if (i_err .ne. 0) goto 999

      enddo    ! do jcv = 1 , ncv

      return

  999 continue
      print'(/,'' error in gtol_3dl_to_3dg_b_old'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_3dl_to_3dg_c_old(
     1 nvp,iv2,iv3
     1,xi,yi,zi,vi
     1,mz_hor,iyi,nyi,ixi,nxi
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,z_hor,v_hor
     1,m_work,work
     1,i_err)
      implicit none

      real     util_invert_1

      integer  nvp
      integer  iv2(nvp),iv3(nvp)

      integer  mz_hor
      integer  iyi(mz_hor),nyi(mz_hor),ixi(mz_hor),nxi(mz_hor)
      real     xi(nvp),yi(nvp),zi(nvp),vi(nvp)

      integer  nx_grd
      real     x0_grd,dx_grd

      integer  ny_grd
      real     y0_grd,dy_grd

      real     z_hor(nx_grd,ny_grd,mz_hor)
      real     v_hor(nx_grd,ny_grd,mz_hor)

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  nz_hor,iz_hor,nx_hor,ix_hor_1,ix_hor_2,iy_hor

      integer  nx_tmp
      real     x0_tmp,dx_tmp

      integer  ny_tmp
      real     y0_tmp,dy_tmp

      real     v0_inp,v1_inp
      real     v0_out,v1_out

c      print'(/,'' gtol_3dl_to_3dg_c_old''
c     1,/,'' nvp='',i8,'' mz_hor='',i8
c     1,/,'' iv2(1)='',i8,'' iv2(nvp)='',i8
c     1,/,'' iv3(1)='',i8,'' iv3(nvp)='',i8
c     1)'
c     1,nvp,mz_hor
c     1,iv2(1),iv2(nvp)
c     1,iv3(1),iv3(nvp)

c  compute the velocity horizon pointers for these velocity horizons
      call fitcpnt2(nvp,iv2,iv3,mz_hor,nz_hor,iyi,nyi,ixi,nxi)

c      print'(/,'' gtol_3dl_to_3dg_c_old nvp='',i8
c     1,'' mz_hor='',i8,'' nz_hor='',i8)'
c     1,nvp,mz_hor,nz_hor
c      print*,' m_work=',m_work

      if (nz_hor .ne. mz_hor) then

        print'(/,'' error in gtol_3dl_to_3dg_c_old''
     1,/,'' mz_hor='',i8,'' nz_hor='',i8)',mz_hor,nz_hor
       stop

      endif    ! if (nz_hor .ne. mz_hor) then

        ix_hor_1 = 1

        do iz_hor = 1 , nz_hor

          iy_hor = iyi(iz_hor) + nyi(iz_hor)        ! # of model y values
          ix_hor_2 = ixi(iy_hor) + nxi(iy_hor)      ! # of model values - ki
          nx_hor = ix_hor_2 - ix_hor_1 + 1

      if (nx_hor .le. 0) then
      print*,' iz_hor=',iz_hor,' iy_hor=',iy_hor,' nx_hor=',nx_hor
      print*,' iyi=',iyi(iz_hor),' nyi=',nyi(iz_hor)
      print*,' ixi=',ixi(iy_hor),' nxi=',nxi(iy_hor)
      print*,' ix_hor_1=',ix_hor_1,' ix_hor_2=',ix_hor_2
      stop
      endif

c  determine the x,y grid size for points ix_hor_1 to ix_hor_2
      call rmod_array_size(nx_tmp,x0_tmp,dx_tmp
     1,nx_hor,xi(ix_hor_1),i_err)
          if (i_err .ne. 0) goto 999

      call rmod_array_size(ny_tmp,y0_tmp,dy_tmp
     1,nx_hor,yi(ix_hor_1),i_err)
          if (i_err .ne. 0) goto 999

c  interpolate depths to the output horizons grid
          call util_interpolate_1(
     1 nx_tmp,x0_tmp,dx_tmp
     1,ny_tmp,y0_tmp,dy_tmp
     1,1,0.,1.
     1,zi(ix_hor_1)
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,1,0.,1.
     1,z_hor(1,1,iz_hor)
     1,m_work,work
     1,i_err)
          if (i_err .ne. 0) goto 999

c  interpolate velocities to the output horizons grid
          call util_interpolate_1(
     1 nx_tmp,x0_tmp,dx_tmp
     1,ny_tmp,y0_tmp,dy_tmp
     1,1,0.,1.
     1,vi(ix_hor_1)
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,1,0.,1.
     1,v_hor(1,1,iz_hor)
     1,m_work,work
     1,i_err)
          if (i_err .ne. 0) goto 999

c          call util_min_max(v0_inp,v1_inp
c     1,nx_hor,vi(ix_hor_1))

c          call util_min_max(v0_out,v1_out
c     1,nx_grd*ny_grd,v_hor(1,1,iz_hor))

c      print'(/,'' iz_hor='',i8,'' iy_hor='',i8
c     1,'' ix_hor_1='',i8,'' ix_hor_2='',i8
c     1,/,''  input v min='',f10.2,'' v_max='',f10.2
c     1,/,'' output v min='',f10.2,'' v_max='',f10.2
c     1)'
c     1,iz_hor,iy_hor,ix_hor_1,ix_hor_2
c     1,util_invert_1(v1_inp),util_invert_1(v0_inp)
c     1,util_invert_1(v1_out),util_invert_1(v0_out)

          ix_hor_1 = ix_hor_2 + 1

        enddo    ! do iz_hor = 1 , nz_hor

      return

  999 continue
      print'('' error in gtol_3dl_to_3dg_c_old'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_pnt0(nxv,iv1,iv2,nvh)
c  count the number of velocity horizons
      implicit none
      integer nxv,iv1(1),iv2(1),nvh
      integer ixv,jv1,jv2

      nvh = 0

      jv1 = iv1(1) - 1
      jv2 = iv2(1) - 1

      do ixv = 1 , nxv

        if (iv1(ixv) .ne. jv1 .or. iv2(ixv) .ne. jv2) then 

          nvh = nvh + 1

c          print'('' gtol_pnt0 found a new velocity horizon''
c     1,'' ixv='',i8,'' nvh='',i5)',ixv,nvh

        endif    ! if (iv1(ixv) .ne. jv1 .or. iv2(ixv) .ne. jv2) then 

        jv1 = iv1(ixv)
        jv2 = iv2(ixv)

      enddo    ! do ixv = 1 , nxv

c      print'(/,'' gtol_pnt0 has found the number of velocity horizons''
c     1,/,'' total number of points     ='',i8
c     1,/,'' total number of horizons   ='',i8
c     1)'
c     1,nxv,nvh

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine gtol_pnt1(imv,nxv,iv1,iv2,ivp,nvp,ivh,nvh)
c  determine the first and last velocity control points which belong 
c  to velocity id imv
c  count the number of velocity horizons
      implicit none
      integer imv,nxv,iv1(1),iv2(1),ivp,nvp,ivh,nvh
      integer ixv,jv1,jv2,kv2,jvh

      ivp = 0
      nvp = 0

      ivh = 0
      nvh = 0

      jvh = 0

      jv1 = iv1(1) - 1
      jv2 = iv2(1) - 1
      kv2 = iv2(1) - 1

      do ixv = 1 , nxv

        if (iv1(ixv) .eq. imv) then 

          nvp = nvp + 1

          if (ivp .eq. 0) then

            ivp = ixv

          else    ! if (ivp .eq. 0) then

            if (iv1(ixv-1) .ne. imv) then

              print'('' error in gtol_pnt1 ''
     1,/,'' there are separated velocity horizons'')'
              stop

            endif    ! if (iv1(ixv-1) .ne. imv) then

          endif    ! if (ivp .eq. 0) then

        endif    ! if (iv1(ixv) .eq. imv) then 
        
        if (iv1(ixv) .ne. jv1 .or. iv2(ixv) .ne. jv2) then 

          jvh = jvh + 1

c          print'('' gtol_pnt1 found a new velocity horizon''
c     1,'' ixv='',i8,'' jvh='',i5)',ixv,jvh

          if (iv1(ixv) .eq. imv) then 

            if (ivh .eq. 0) ivh = jvh
            if (nvh .eq. 0 .or. iv2(ixv) .ne. kv2) nvh = nvh + 1
            kv2 = iv2(ixv)

c          print'('' new iv2 for this iv1''
c     1,'' ixv='',i5,'' jvh='',i5,'' ivh='',i5,'' nvh='',i5
c     1,'' iv2='',i5)'
c     1,ixv,jvh,ivh,nvh,iv2(ixv)

          endif    ! if (iv1(ixv) .eq. imv) then 

        endif    ! if (iv1(ixv) .ne. jvh) then 

        jv1 = iv1(ixv)
        jv2 = iv2(ixv)

      enddo    ! do ixv = 1 , nxv

c      print'(/,'' gtol_pnt1 has found the number of velocity horizons''
c     1,/,'' velocity id                ='',i8
c     1,/,'' total number of points     ='',i8
c     1,/,'' first point                ='',i8
c     1,/,'' number of points this id   ='',i8
c     1,/,'' total number of horizons   ='',i8
c     1,/,'' first velocity horizon     ='',i8
c     1,/,'' number of horizons this id ='',i8
c     1)'
c     1,imv,nxv,ivp,nvp,jvh,ivh,nvh

      return
      end

