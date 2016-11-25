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
      SUBROUTINE CELL(XMIN,XMAX,ZMIN,ZMAX,NB,IXB,NXB,XB,ZB
     1,MC,MXC,NC,IXC,NXC,XC,ZC,MWORK,WORK,IERR)
C  CELLS GO CLOCKWISE
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C
CPrimitive name: CELL
C        Author: D W Hanson
C       Written: 90/06/08
C  Last revised: 93/05/13 Hanson
C
C  Purpose:  Convert a set of boundaries into cells.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C      CALL CELL(XMIN,XMAX,ZMIN,ZMAX,NB,IXB,NXB,XB,ZB
C     1,MC,MXC,NC,IXC,NXC,XC,ZC,MWORK,WORK,IERR)
C
C ARGUMENTS
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C XMIN     I     REAL    MINIMUM X COORDINATE IN MODEL.
C XMAX     I     REAL    MAXIMUM X COORDINATE IN MODEL.
C ZMIN     I     REAL    MINIMUM Z COORDINATE IN MODEL.
C ZMAX     I     REAL    MAXIMUM Z COORDINATE IN MODEL.
C NB       B     INT>0   # OF BOUNDARIES IN MODEL
C  ARRAYS IXB,NXB NEED NB ELEMENTS
C  SEE NOTE 1. BELOW
C IXB      B  INT ARRAY  POINTERS TO ARRAYS XB, ZB
C                        BOUNDARY IB BEGINS AT THE IXB(IB)+1
C                        ELEMENT OF XB, ZB
C NXB      B  INT ARRAY  NUMBER OF POINTS IN EACH BOUNDARY
C                        BOUNDARY IB HAS NXB(IB) ELEMENTS
C  ARRAYS XB,ZB NEED MB = IXB(NB)+NXB(NB) ELEMENTS
C  SEE NOTES 1. AND 2. BELOW
C XB       O REAL ARRAY  X COORDINATES OF BOUNDARIES.
C                        NEEDS NXSUM ELEMENTS
C                        NXSUM = SUM NXB(I) I=1,NB
C ZB       O REAL ARRAY  Z COORDINATES OF BOUNDARIES
C                        NEEDS NXSUM ELEMENTS
C MC       I     INT>0   # OF MEMORY LOCATIONS AVAILABLE IN ARRAYS IXC,...
C MXC      I     INT>0   # OF MEMORY LOCATIONS AVAILABLE IN ARRAYS XC,...
C NC       O     INT     # OF CELLS FOUND IN MODEL (SHOULD BE <=NB)
C  ARRAYS IXC,NXC NEED NC ELEMENTS
C IXC      O  INT ARRAY  POINTERS TO ARRAYS XC,ZC ETC
C                        CELL ICELL BEGINS AT THE IXC(ICELL)+1
C                        ELEMENT OF XC,...
C                        NEEDS NC ELEMENTS
C NXC      O  INT ARRAY  NUMBER OF ELEMENTS IN EACH CELL
C                        CELL ICELL HAS NXC(ICELL) POINTS IN IT
C                        NEEDS NC ELEMENTS
C  ARRAYS XC,ZC          NEED MC = IXC(NC)+NXC(NC) ELEMENTS
C XC       O REAL ARRAY  CELL WALL X COORDINATES
C ZC       O REAL ARRAY  CELL WALL Z COORDINATES
C                        A LENS TO A SURROUNDING CELL
C MWORK    I INTEGER     NUMBER OF WORDS IN WORK ARRAY
C WORK     O REAL ARRAY  WORK ARRAY NEEDS 4*MC + 15*NS
C                        NS = # OF SEGMENTS FOUND
C                        SAFE VALUES SHOULD BE MC=3*MB, NS=MB
C IERR     O    INT      ERROR FLAG NUMBER
C-----------------------------------------------------------------------
C                                 NOTES
C
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author      Description
C     ----     ------      -----------
C 20. 93/06/08 Hanson      ENHANCE ERROR PRINTING
C 19. 93/05/13 Hanson      ENHANCE ERROR PRINTING
C 18. 93/05/10 Hanson      CHANGE IMC FLAG=0
C 17. 93/04/06 Hanson      SIMPLIFY ZTOT AND GTOL CALLS
C 16. 93/02/09 Hanson      MODIFY SCALE CALC
C 15. 93/01/08 Hanson      MODIFY CELL ARGUMENT LIST
C 14. 92/12/11 Hanson      ADOPT FITCELL VELOCITY CONVENTION
C 13. 92/11/17 Hanson      CHANGE LENS CONDITION TO INCLUDE BOUNDARY
C 12. 92/10/28 Hanson      REMOVE UNEEDED SUBROUTINES
C 11. 92/10/26 Hanson      MODIFY CELLIVRT CALL
C 10. 92/10/23 Hanson      INITIALIZE IERR IN CHP CALLS
C 9.  92/10/22 Hanson      CHECK VELOCITES IN CHKP
C 8.  92/10/16 Hanson      CHANGE ERROR CONDITION IN CELLFNCN
C 7.  92/10/15 Hanson      CHANGE SACLE CALC IN CELL
C 6.  92/10/06 Hanson      ADD CELLCLVP
C 5.  92/10/03 Hanson      CHECK CELL LIMITS IN CELLBLD
C 4.  92/10/01 Hanson      FIX ADD0 BUX - ADD CELLTOL, CELLTOLN
C 3.  92/09/23 Hanson      ADD DOCUMENTATION
C 2.  92/09/17 Hanson      Add NCRS, FNCM, FNCV, FNMV
C 1.  90/05/03 Hanson      Original version
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C
C   SUBROUTINE NAMES IN THIS MODULE
C CELL     CELLCLVP CELLSEG  CELLMAKE CELLONEC CELLFCON CELLDXR 
C CELLNL1  CELLCLSG CELLBLD  CELLENDS CELLADDN CELLADD1 CELLADD0 CELLADXZ
C CELLCBND CELLADD2 CELLIXNX CELLBTOC CELLSEPS CELLTRUN CELLCBCK CELLEXTD
C CELLBNDI CELLFNDL CELLCROS CELLACON CELLDIR  CELLANG  CELLREV2 CELLREV 
C CELLPACK CELLPAC1 CELLPAC0 CELLPAD  CELLINS  CELLSHF2 CELLSHIF CELLSHFP
C CELLWHER CELLWSEG CELLIPLY CELLPOLY CELLNCRS CELLCOP  CELLSOP  CELLPNT 
C CELLPNT0 CELLPNTL CELLPNTR CELLSPLN CELLSPL0 CELLSPLD CELLSETV CELLLINE
C CELLCOPY CELLCOP2 CELLWORK CELLSUMN CELLSCAL CELLSCAC CELLSUM  CELLAVE 
C CELLMNMX CELLMNM0 CELLIMAX CELLMAX2 CELLMAX1 CELLNMAX CELLTOL  CELLDELI
C CELLDECI CELLEDIT CELLVSRT CELLORD  CELLSORT CELLVMIX CELLCHKU CELLCHKE
C CELLCHKC CELLCHKP CELLCHP1 CELLCHP2 CELLCHP3 CELLCHP4 CELLCHP5 CELLCHP6
C CELLFNCN CELLFNCM CELLFNCV CELLFNMV CELLPSEG CELLPPLT CELLPBVC CELLPRNV
C CELLPRNX CELLPRNB CELLPRNC CELLPCON CELLPRNS CELLPRNL
C 
C   ENTRY NAMES IN THIS MODULE
C CELLSPL0 CELLSPLV CELLWORS CELLWORL
C 
C   FUNCTION NAMES IN THIS MODULE
C
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C
C
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - NONE
C  HEAP(dynamic) - NONE
C-----------------------------------------------------------------------
C\END DOC
C  DETERMINE THE X,Z POINTS THAT MAKE UP CELLS
      dimension ixb(1),nxb(1),xb(1),zb(1)
     1,ixc(1),nxc(1),xc(1),zc(1),work(1)
c  convert from time to depth note scale may be 1. and this does nothing
      ierr = 0
      if (xmin .eq. xmax) xmax = xmin + 1.
      if (zmin .eq. zmax) zmax = zmin + 1.
      xscale = 1. / (xmax - xmin)
      zscale = 1. / (zmax - zmin)
c      call cellscac(scale,xmin,xmax,zmin,zmax)
c      call cellscal(1,zmin,scale)
c      call cellscal(1,zmax,scale)
c      call cellscal(ixb(nb)+nxb(nb),zb,scale)
      call cellscal(1,xmin,xscale)
      call cellscal(1,xmax,xscale)
      call cellscal(ixb(nb)+nxb(nb),xb,xscale)
      call cellscal(1,zmin,zscale)
      call cellscal(1,zmax,zscale)
      call cellscal(ixb(nb)+nxb(nb),zb,zscale)
      call celltol(tol,xmin,xmax)

c  make a copy of the boundary data
c  keep points that fall within the boundary
      mb = nb + 1000
      mxb = 2 * (ixb(nb) + nxb(nb)) + 1000
      call cellwors(1,mwork)
      call cellwork(ixb1,mb,*999)
      call cellwork(nxb1,mb,*999)
      call cellwork(jxb1,mxb,*999)
      call cellwork(jzb1,mxb,*999)
      call celladdn(tol,xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb
     1,mb,nb1,work(ixb1),work(nxb1),work(jxb1),work(jzb1),*999)

c  determine segments
      ms = ixb(nb) + nxb(nb) + 1000
      call cellwork(iln,ms,*999)
      call cellwork(ibs,ms,*999)
      call cellwork(ixs,ms,*999)
      call cellwork(nxs,ms,*999)
      call cellwork(iw1,ms,*999)
      call cellworl(mw1)
      call cellseg(tol,xmin,xmax,zmin,zmax,mxb,nb1
     1,work(ixb1),work(nxb1),work(jxb1),work(jzb1)
     1,nln,work(iln),ns,work(ibs),work(ixs),work(nxs),work(iw1)
     1,ierr,*999)

c  form the cells out of the segments
      m0=10
      call cellwork(icon,4*ns+m0,*999)
      call cellwork(iacon,4*ns+m0,*999)
      call cellwork(isc,ns+m0,*999)
      call cellwork(nsc,ns+m0,*999)
      call cellwork(ics,2*ns+m0,*999)
      call cellwork(ionb,ns+m0,*999)
      call cellwork(iwork,2*ns+m0,*999)
      call cellmake(tol,xmin,xmax,zmin,zmax
     1,nb1,work(ixb1),work(nxb1),work(jxb1),work(jzb1)
     1,mc,mxc,nc,ixc,nxc,xc,zc
     1,nln,work(iln),ns,work(ibs),work(ixs),work(nxs)
     1,work(icon),work(iacon),work(isc),work(nsc),work(ics)
     1,work(ionb),work(iwork),ierr,*999)

c  convert back to time note scale may be 1. and this does nothing
      call cellscal(1,xmin,1./xscale)
      call cellscal(1,xmax,1./xscale)
      call cellscal(ixb(nb)+nxb(nb),xb,1./xscale)
      call cellscal(ixc(nc)+nxc(nc),xc,1./xscale)
      call cellscal(1,zmin,1./zscale)
      call cellscal(1,zmax,1./zscale)
      call cellscal(ixb(nb)+nxb(nb),zb,1./zscale)
      call cellscal(ixc(nc)+nxc(nc),zc,1./zscale)

      if (ierr .ne. 0) goto 999
c  all done
      return
  999 continue
      print*,' error in cell xscale=',xscale,' zscale=',zscale
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellclvp(xmin,xmax,zmin,zmax
     1,nb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc,mwork,work,ierr)
c  construct cell model includeing velocities from data
      dimension icv(1),imc(1),ixc(1),nxc(1),xc(1),zc(1)
      ierr = 0

c  determine the boundaries of the original cells
      call cell(xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb
     1,mc,mxc,nc,ixc,nxc,xc,zc,mwork,work,ierr)
      if (ierr .ne. 0) 
     1print'('' error in cellclvp after cell ierr='',i5)',ierr
      if (ierr .ne. 0) goto 999

c  determine which velocity type is used in which cell
      call cellfncn(0,ncv,icv,xcv,zcv,nv,imv
     1,nc,imc,ixc,nxc,xc,zc,ierr)
      if (ierr .ne. 0) 
     1print'('' error in cellclvp after cellfncn  ierr='',i5)',ierr
      if (ierr .ne. 0) goto 999
      return
  999 continue
      print'('' error in cellclvp ierr='',i5)',ierr
      call cellpbvc(' error in cellclvp'
     1,xmin,xmax,zmin,zmax,nb,ixb,jtb,ixb,nxb,xb,zb
     1,ncv,icv,xcv,zcv,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,0,nc,vc,gc,imc,ixc,nxc,xc,zc)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellseg(tol,xmin,xmax,zmin,zmax,mxb,nb,ixb,nxb
     1,xb,zb,nln,iln,ns,ibs,ixs,nxs,work,ierr,*)
c  determine the segments contained in each boudnary
      dimension ixb(1),nxb(1),xb(1),zb(1)
     1,iln(1),ibs(1),ixs(1),nxs(1),work(1)
      ierr = 0

      call cellprnb(' top of cellseg',nb,ixb,nxb,xb,zb)

c  pad the boundary arrays
      call cellpad(50,1,mxb,nb,ixb,nxb,xb,zb,*999)

c  determine which cells are lenses
      call cellfndl(nln,iln,mxb,nb,ixb,nxb,xb,zb,*999)

c  for each segment of each boundary see if it intersects with another boundary
c  if it does insert into both boundaries a point at the intersection location
c  note using ibs,ixs,nxs as work arrays
      call cellprnb(' aft cellfndl',nb,ixb,nxb,xb,zb)

      call cellbndi(tol,mxb,nb,ixb,nxb,xb,zb,ibs,ixs,nxs,work,ierr,*999)

      call cellprnb(' aft bndi',nb,ixb,nxb,xb,zb)

      call cellpac1(tol,nb,ixb,nxb,xb,zb)

      call cellprnb(' aft ins pac1',nb,ixb,nxb,xb,zb)

c  truncate the boundaries
      call celltrun(tol,mxb,nb,ixb,jxb,ixb,nxb,xb,zb
     1,ibs,ixs,nxs,work,ierr,*999)
      call cellprnb(' aft celltrun',nb,ixb,nxb,xb,zb)

c  separate the boundaries into segments
      call cellseps(tol,ns,ibs,ixs,nxs,nb,ixb,nxb,xb,zb
     1,ierr,*999)

      call cellprnl(' aft segments'
     1,ns,ibs,ixs,nxs,nln,iln,nb,ixb,nxb,xb,zb)
      if (ierr .ne. 0) goto 999
      return
  999 continue
      call cell_get_lpr(lpr)
      write(lpr,'('' error in cellseg ierr='',i3)')ierr
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellmake(tol,xmin,xmax,zmin,zmax
     1,nb,ixb,nxb,xb,zb,mc,mxc,nc,ixc,nxc,xc,zc,nln,iln
     1,ns,ibs,ixs,nxs,icon,acon,isc,nsc,ics,ips1,ips2,ierr,*)

      dimension ixb(1),nxb(1),xb(1),zb(1),ixc(1),nxc(1),xc(1),zc(1)
     1,iln(1),ibs(1),ixs(1),nxs(1),ips1(1),ips2(1)
     1,icon(2,2,1),acon(2,2,1),isc(1),nsc(1),ics(1)
      ierr = 0
c  this creates cells from segments
c
c  the is th segment comes from boundary ibs(is)
c  there are nxs(is) pts and starts at the ixs(is) + 1 point
c
c  determine the segments which connect to the ends of each segment
      call cellprnl(' beg of cellmake'
     1,ns,ibs,ixs,nxs,nln,iln,nb,ixb,nxb,xb,zb)

c  if there is a single cell
      if (nln .eq. 1 .and. ns .eq. 0) then
        call cellonec(tol,nxb,xb,zb,mc,nc,ixc,nxc,xc,zc,*999)
      else    ! if (nln .eq. 1 .and. ns .eq. 0) then

c  find the segment connections
        call cellfcon(tol,xmin,xmax,zmin,zmax,nl1,acon,icon,ips1,ips2
     1,ns,ibs,ixs,nxs,nb,ixb,nxb,xb,zb,ierr,*998)

c  determine which segments make up each cell
        call cellclsg(tol,xmin,xmax,zmin,zmax,nl1,acon,icon,ips1,ips2
     1,ns,ibs,ixs,nxs,mc,nc,isc,nsc,ics,ierr,*998)

        call cellprns(' cell segments',nc,isc,nsc,ics)

c  build cell x,z coordinates form segments
        call cellbld(tol,nb,ixb,nxb,xb,zb,mc,mxc,nc,ixc,nxc,xc,zc
     1,nln,iln,ns,ibs,ixs,nxs,isc,nsc,ics,ips1,ierr,*998)
      endif    ! if (nln .eq. 1 .and. ns .eq. 0) then

  998 continue

      call cellprnc(' aft cellbld',0,nc,vc,gc,imc,ixc,nxc,xc,zc)
      if (ierr .ne. 0) goto 999

      return
  999 continue
      call cell_get_lpr(lpr)
      write(lpr,*)' error in cellmake ierr=',ierr,' tol=',tol
      call cellprnc(' aft cellbld',0,nc,vc,gc,imc,ixc,nxc,xc,zc)
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellonec(tol,nxb,xb,zb,mc,nc,ixc,nxc,xc,zc,*)
c  create model out of a single cell
      dimension nxb(1),xb(1),zb(1),ixc(1),nxc(1),xc(1),zc(1)
      logical cellnots
      call cell_get_lpr(lpr)
      nc = 1
      if (nc .gt. mc) then
        ierr = 41
        write(lpr,'('' error in cellmake mc='',i5
     1,'' nc='',i5)')mc,nc
        goto 999
      endif
      ixc(1) = 0
      nxc(1) = nxb(1)
      do 1 lxc = 1 , nxc(1)
        xc(lxc) = xb(lxc)
        zc(lxc) = zb(lxc)
    1 continue    ! do 1 lxc = 1 , nxc(1)
      if (cellnots(tol,xc(1),zc(1),xc(nxc(1)),zc(nxc(1)))) then
        nxc(1) = nxc(1) + 1
        xc(nxc(1)) = xc(1)
        zc(nxc(1)) = zc(1)
      endif        ! if (cellnots(tol,xc(1),zc(1),xc(nxc(1)),zc(nxc(1)))) then
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellfcon(tol,xmin,xmax,zmin,zmax,nl1,acon,icon
     1,ips1,ips2,ns,ibs,ixs,nxs,n,ix,nx,x,z,ierr,*)
c  determine which segments connect to which
      dimension icon(2,2,1),acon(2,2,1),ips1(1),ips2(1)
     1,ibs(1),ixs(1),nxs(1),ix(1),nx(1),x(1),z(1)

c  look through segments and determine connections
      nl1 = 0
      call cellsetv(ns,ips1,0)
      call cellsetv(2*ns,ips2,0)
      do 1 is = 1 , ns
        icon1 = 0
        icon2 = 0
        call celldxr(ix1,ix2,ri1,ri2,dxi1,dzi1,dxi2,dzi2
     1,is,ibs,ixs,nxs,n,ix,nx,x,z,ierr,*998)
        call cellnl1(tol,xmin,xmax,zmin,zmax
     1,is,nl1,ips1,ips2,nxs(is),x(ix1),z(ix1))
        do 2 js = 1 , ns
          if (js .eq. is) goto 2
          call celldxr(jx1,jx2,rj1,rj2,dxj1,dzj1,dxj2,dzj2
     1,js,ibs,ixs,nxs,n,ix,nx,x,z,ierr,*998)
          r11 = celldist(x(ix1),z(ix1),x(jx1),z(jx1))
          r21 = celldist(x(ix1),z(ix1),x(jx2),z(jx2))
          r12 = celldist(x(ix2),z(ix2),x(jx1),z(jx1))
          r22 = celldist(x(ix2),z(ix2),x(jx2),z(jx2))
          call cellacon(tol,r11,dxi1,dxj1,dzi1,dzj1
     1,js,icon1,acon(1,1,is),icon(1,1,is))
          call cellacon(tol,r21,dxi1,dxj2,dzi1,dzj2
     1,-js,icon1,acon(1,1,is),icon(1,1,is))
          call cellacon(tol,r12,dxi2,dxj1,dzi2,dzj1
     1,js,icon2,acon(1,2,is),icon(1,2,is))
          call cellacon(tol,r22,dxi2,dxj2,dzi2,dzj2
     1,-js,icon2,acon(1,2,is),icon(1,2,is))
    2   continue    ! do 2 js = 1 , ns
    1 continue    ! do 1 is = 1 , ns

c  the model boundary should be a clockwise direction
c  therefore the smaller angle from end 2 should connect to end 1
c  of the next model boundary segment
      call cellchkc(nl1,ips1,ns,icon,acon,ierr)

  998 call cellpcon(ns,icon,acon,nl1,ips1,ips2,ibs,ixs,nxs
     1,n,ix,ns,x,z,ierr)
      if (ierr .eq. 0) return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celldxr(ix1,ix2,ri1,ri2,dxi1,dzi1,dxi2,dzi2
     1,is,ibs,ixs,nxs,n,ix,nx,x,z,ierr,*)
c  compute some distances at endpoints of segment
      dimension ibs(1),ixs(1),nxs(1),ix(1),nx(1),x(1),z(1)
      call cell_get_lpr(lpr)

      ix1 = ix(ibs(is)) + ixs(is) + 1   ! first point
      ix2 = ix1 + nxs(is) - 1           ! last point
      ri1 = celldist(x(ix1),z(ix1),x(ix1+1),z(ix1+1))
      ri2 = celldist(x(ix2),z(ix2),x(ix2-1),z(ix2-1))
      if (ri1 .eq. 0. or. ri2 .eq. 0.) then
        write(lpr,*)' dxr is=',is,' ri1=',ri1,' ri2=',ri2
        write(lpr,*)' dxr is=',is,' ri1=',ri1,' ri2=',ri2
        write(lpr,*)' x1=',x(ix1),x(ix1+1),' x2=',x(ix2),x(ix2-1)
        write(lpr,*)' z1=',z(ix1),z(ix1+1),' z2=',z(ix2),z(ix2-1)
        write(lpr,*)'czzz boundaries'
        call cellpplt(n,ix,nx,x,z,lpr,'error in dxr xx',0)
        write(lpr,*)'czzz segments'
        do ks=1,is
          call cellpplt(1,ix(ibs(ks))+ixs(ks),nxs(ks),x,z,lpr,' ',0)
        enddo
        write(lpr,*)'czzz end segments'
        ierr = 10
        return 1
      endif
      dxi1 = (x(ix1+1) - x(ix1)) / ri1    ! unit vectors pointing along segment
      dzi1 = (z(ix1+1) - z(ix1)) / ri1
      dxi2 = (x(ix2-1) - x(ix2)) / ri2
      dzi2 = (z(ix2-1) - z(ix2)) / ri2
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellnl1(tol,xmin,xmax,zmin,zmax,is,nl1,ips1,ips2,n,x,z)
c  add to the list of segments on boundary
      dimension ips1(1),ips2(1),x(1),z(1)
      do 1 i = 1 , n-1
        if (abs((x(i)+x(i+1))/2.-xmin) .gt. tol
     1.and. abs((x(i)+x(i+1))/2.-xmax) .gt. tol
     1.and. abs((z(i)+z(i+1))/2.-zmin) .gt. tol
     1.and. abs((z(i)+z(i+1))/2.-zmax) .gt. tol) goto 2
    1 continue    ! do 1 i = 1 , n
      nl1 = nl1 + 1
      ips1(nl1) = is
      ips2(is) = 2
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellclsg(tol,xmin,xmax,zmin,zmax,nl1,acon,icon
     1,ips1,ips2,ns,ibs,ixs,nxs,mc,nc,isc,nsc,ics,ierr,*)
c  determine the segments that make up each cell
      dimension ibs(1),ixs(1),nxs(1),ips1(1),ips2(1)
     1,icon(2,2,1),acon(2,2,1),isc(1),nsc(1),ics(1)

      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
c  l1 is a list of segments which have been used once in a cell
c  l2 is a list of segments which have been used twice in a cell
c  create a cell useing the il1 th segment from l1
      nl2 = nl1
      il1 = 0
      nc = 0
      isc0 = 0
      if (ipr .eq. 4)write(lpr,*)' czzz start of cell construction ns='
     1,ns,' nl1=',nl1
    1 continue   ! come to here to check the next element in l1
        il1 = il1 + 1
        if (il1 .gt. min(nl1,ns)) goto  2 ! done if all segs in l1 used
        if (ibs(ips1(il1)) .lt. 0) goto 1 ! if this seg has been used 2 sk
        ibs(ips1(il1)) = -ibs(ips1(il1))
        nc = nc + 1
        if (nc .gt. mc) then
          ierr = 42
          write(lpr,'('' error in cellmake mc='',i5
     1,'' nc='',i5)')mc,nc
          goto 999
        endif
        isc(nc) = isc0
        isc0 = isc0 + 1
        iend = ips2(ips1(il1))
        ics(isc0) = (1 + 2 * (iend - 2)) * ips1(il1)! ie=1 ic<0 ie=2 ic>0
        if (abs(ics(isc0)) .lt. 0 .or. abs(ics(isc0))
     1.gt. ns) goto 999
    3   continue  ! come to here to add another segment to the cell
        isc0 = isc0 + 1
c  ics(isc0) = segment number of cell
c  the last segment of the cell wall was segment ics(isc0-1)
c  the open end of that segment is iend
c  the two segments that connect to end iend
c  at that point are icon(1,iend,abs(ics(isc0)))
c  and icon(2,iend,abs(ics(isc0)))
c  if icon < 0 we connect to end 1 of segment abs(icon)
c  if icon > 0 we connect to end 2 of segment abs(icon)
c  the segments on the other end are icon(1,iend,is0) and icon(2,iend,is0)
        ics(isc0) = icon(2,iend,abs(ics(isc0-1)))
        iend = (3 + sign(1,ics(isc0))) / 2
        if (abs(ics(isc0)) .eq. ips1(il1)) then
          isc0 = isc0 - 1
          goto 4
        endif
        if (abs(ics(isc0)) .gt. ns .or. isc0-isc(nc) .gt. ns) goto 999
c  if this segment is not in l1 add it to l1 otherwise add it to l2
        do 5 jl1 = 1 , nl1
          if (ips1(jl1) .eq. abs(ics(isc0))) then
            ibs(ips1(jl1)) = -ibs(ips1(jl1))
            goto 3
          endif
    5   continue   ! do 5 jl1 = 1 , nl1
        nl1 = nl1 + 1
        ips1(nl1) = abs(ics(isc0))
        ips2(ips1(nl1)) = mod(iend,2) + 1
        goto 3
    4   continue
        nsc(nc) = isc0 - isc(nc)
        goto 1
    2 continue   ! come to here if all cells have been created

c  check to make sure each segment has been used twice
      call cellchku(nl2,ips1,ns,ibs,nc,isc,nsc,ics,ierr,*999)

      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellbld(tol,nb,ixb,nxb,xb,zb,mc,mxc,nc,ixc,nxc,xc,zc
     1,nln,iln,ns,ibs,ixs,nxs,isc,nsc,ics,ips1,ierr,*)
c  construct actual cell boundaries from original boundary values
      dimension ixb(1),nxb(1),xb(1),zb(1),ixc(1),nxc(1),xc(1),zc(1)
     1,iln(1),ibs(1),ixs(1),nxs(1),ips1(1),isc(1),nsc(1),ics(1)
      logical cellnots

      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      nc = nc + nln    ! add the lenses
      if (nc .gt. mc) then
        ierr = 43
        write(lpr,'('' error in cellmake mc='',i5,'' nc='',i5)')mc,nc
        goto 999
      endif
c  first include lenses
c      call cellpplt(nb,ixb,nxb,xb,zb,40,' top of cellbld xx',0)
      do 1 ic = 1 , nln
        ib = iln(ic)
        lxb = ixb(ib) + 1
        call cellixnx(ic,ixc,nxc)
        lxc = ixc(ic) + 1
        if (lxc+nxc(ic)+nxb(ib) .gt. mxc) then
          write(lpr,'('' error in cellbld need more space for cells''
     1,'' mxc='',i8,'' need at least '',i8)')mxc,lxc+nxc(ic)+nxb(ib)
            ierr = 203
            goto 999
          endif
        call cellbtoc(tol,1,nxb(ib),xb(lxb),zb(lxb)
     1,nxc(ic),xc(lxc),zc(lxc))
        call celldir(nxc(ic),xc(lxc),zc(lxc),*999)
        call cellprnc(' creating lens',0,1,vc,gc
     1,imc,0,nxc(ic),xc(lxc),zc(lxc))
    1 continue    ! do 1 ic = 1 , nln
c  now add ordinary cells
      kln = 0
      do 3 jc = 1 , nc - nln
        ic = jc + nln
        call cellixnx(ic,ixc,nxc)
        lxc = ixc(ic) + 1
        if(ipr.eq.4)write(lpr,'('' ic='',i5,'' jc='',i5,'' nc='',i5
     1,'' nln='',i5)')ic,jc,nc,nln
        do 4 jsc = 1 , nsc(jc)
          is = abs(ics(isc(jc)+jsc))
          lxb = ixb(ibs(is)) + ixs(is) + 1   ! first point in segment
          if (lxc+nxc(ic)+nxs(is) .gt. mxc) then
          write(lpr,'('' error in cellbld need more space for cells''
     1,'' mxc='',i8,'' need at least '',i8)')mxc,lxc+nxc(ic)+nxs(is)
            ierr = 204
            goto 999
          endif
          call cellbtoc(tol,sign(1,ics(isc(jc)+jsc))
     1,nxs(is),xb(lxb),zb(lxb),nxc(ic),xc(lxc),zc(lxc))
    4   continue     ! do 4 jsc = 1 , nsc(jc)
        if (cellnots(tol,xc(ixc(ic)+1),zc(ixc(ic)+1)
     1,xc(ixc(ic)+nxc(ic)),zc(ixc(ic)+nxc(ic)))) then
          write(lpr,'('' error cell'',i5,'' does not close'')')ic
          ierr = 14
          goto 999
        endif
        call cellrev2(nxc(ic),xc(ixc(ic)+1),zc(ixc(ic)+1))
        call cellprnc(' creating cell',0,1,vc,gc
     1,imc,0,nxc(ic),xc(lxc),zc(lxc))
c  determine if any of the lenses are inside this cell
c  if they are add the lens values to the cell values
        if (kln .ge. nln) goto 3
        do 6 jln = 1 , nln
          if (iln(jln) .le. 0) goto 6
          call cellpoly(xc(ixc(jln)+1),zc(ixc(jln)+1)
     1 ,nxc(ic),xc(ixc(ic)+1),zc(ixc(ic)+1),inside,*999)
          if (inside .ne. 1) goto 6
          kln = kln + 1
      if(ipr.eq.4)write(lpr,'('' including lens'',i5
     1,'' in cell'',i5,'' kln='',i5,'' lxc='',i5,'' nxc='',i5
     1,/,'' ix  id  xc  zc'',200(/,1x,i5,2(1x,f10.2)))')jln,ic,kln
     1,ixc(jln),nxc(jln),(i,xc(i),zc(i)
     1,i=ixc(jln)+1,ixc(jln)+min(200,nxc(jln)))
          if (iln(jln) .lt. 0) then
      write(lpr,'('' error trying to use same lens twice jln='',i5
     1,'' kln='',i5,'' iln='',i5,'' ic='',i5)')jln,kln,iln(jln),ic
            ierr = 15
            goto 999
          endif    ! if (iln(jln) .lt. 0) then
c  determine what points in cell ic and cell jln are closest together
          il0 = 0
          ic0 = 0
          do 7 ixc1 = ixc(jln)+2  , ixc(jln)+nxc(jln)
            call cellpoly(xc(ixc1),zc(ixc1)
     1,nxc(ic),xc(ixc(ic)+1),zc(ixc(ic)+1),inside,*999)
            if (inside .ne. 1) then
      call cell_get_lpr(lpr)
              write(lpr,'('' error in lens bnd all pnts not in cell''
     1,'' jln='',i5,'' lxc='',i5)')jln,ixc1-ixc(jln)
              ierr = 16
              goto 999
            endif    ! if (inside .ne. 1) then
            do 8 ixc0 = ixc(ic)+2 , ixc(ic)+nxc(ic)
              r1 = celldist(xc(ixc0),zc(ixc0),xc(ixc1),zc(ixc1))
              if (il0 .eq. 0. .or. r1 .lt. r0) then
                r0 = r1
                il0 = ixc1
                ic0 = ixc0
              endif
    8       continue
    7     continue    ! do 7 ixc1 = ixc(jln)+2  , ixc(jln)+nxc(jln)
c  insert pnts from lens into surrounding cell
          ixc0a = ixc(ic) + nxc(ic) + nxc(jln) + 2
          ixc0 = ixc0a - 1
          ixc1 = il0 + 1
          do 9 ixc2 = 1 , nxc(jln)
            ixc1 = ixc1 - 1
            if (ixc1 .le. ixc(jln)+1) ixc1 = ixc(jln) + nxc(jln)
            call celladxz(1,xc(ixc1),zc(ixc1),ixc0,xc,zc)
    9     continue    ! do 9 ixc1 = ixc(jln)+1 , ixc(jln)+nxc
          if (ixc1 .ne. il0) then
            ierr = 17
            goto 999
          endif
          call celladxz(1,xc(ic0),zc(ic0),ixc0,xc,zc)
          nins = ixc0 - ixc0a + 1
          call cellins(ic0-ixc(ic),ic,nins
     1,xc(ixc0a),zc(ixc0a),mxc,ic,ixc,nxc,xc,zc,*999)
          iln(jln) = - abs(iln(jln))
    6   continue    ! do 6 jln = 1 , nln
    3 continue    ! do 3 jc = 1 , nc - nln
      if (kln .ne. nln) then
      call cell_get_lpr(lpr)
        write(lpr,'('' error did not use all lenses kln='',i5
     1,'' nln='',i5,200(/,'' jln='',i5,'' iln='',i5))')
     1kln,nln,(i,iln(i),i=1,min(200,nln))
        ierr = 18
        goto 999
      endif
      call cellends(nc,ixc,nxc,xc,zc)
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellends(n,ix,nx,x,z)
c  make sure the first and last points of each cell are the same
c  if they are not change the last point and search for that point 
c  in all other cells
      dimension ix(1),nx(1),x(1),z(1)
c      data icall/0/
c      icall = icall + 1
c      call cellpplt(n,ix,nx,x,z,61,' top of cellends xx',0)
    1 continue
      ipass = 0
      do 2 i = 1 , n
        i1 = ix(i) + 1
        i2 = ix(i) + nx(i)
        if (x(i1) .ne. x(i2) .or. z(i1) .ne. z(i2)) then
c      print'('' ends icall='',i3,'' i='',i5
c     1,'' x='',2(1x,f10.2),'' z='',2(1x,f10.2))'
c     1,icall,i,x(i1),x(i2),z(i1),z(i2)
          ipass = 1
          do 3 j = 1 , n
            do 4 j1 = ix(j)+1 , ix(j)+nx(j)
              if (x(j1) .eq. x(i2) .and. z(j1) .eq. z(i2) 
     1.and. j1 .ne. i2) then
c      print'('' j='',i5,'' j1='',i5,'' x='',f10.2,'' z='',f10.2)'
c     1,j,j1-ix(j),x(j1),z(j1)
                x(j1) = x(i1)
                z(j1) = z(i1)
              endif
    4       continue
    3     continue
          x(i2) = x(i1)
          z(i2) = z(i1)
        endif
    2 continue
      if (ipass .ne. 0) goto 1
c      call cellpplt(n,ix,nx,x,z,62,' end of cellends xx',0)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celladdn(tol,xmin,xmax,zmin,zmax
     1,n1,ix1,nx1,x1,z1,m2,n2,ix2,nx2,x2,z2,*)
      dimension ix1(1),nx1(1),x1(1),z1(1),ix2(1),nx2(1),x2(1),z2(1)
      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      if(ipr.eq.4)write(lpr,'('' celladdn xmin='',f10.2
     1,'' xmax='',f10.2,'' zmin='',f10.2,'' zmax='',f10.2)')
     1xmin,xmax,zmin,zmax
c  add model edges at min max points
c  delete points outside this area
      call cellprnb(' top celladdn',n1,ix1,nx1,x1,z1)
c      call cellpplt(n1,ix1,nx1,x1,z1,91,'top of addn xx',0)
      n2 = 0
      do 1 i1 = 1 , n1
c copy points from x1 to x2 add points at boundaries
        jx1 = ix1(i1) + 1
        call celladd1(xmin,xmax,zmin,zmax,nx1(i1)
     1,x1(jx1),z1(jx1),m2,n2,ix2,nx2,x2,z2,ierr)
        if (ierr .ne. 0) goto 999
    1 continue

c      call cellpplt(n2,ix2,nx2,x2,z2,93,'mid of addn xx',0)
c  add the boundary to x2,z2
      if (n2+4 .gt. m2) then
        print*,' need more memory in celladdn n2=',n2,' m2=',m2
        goto 999
      endif
      call celladd2(xmax,xmax,zmin,zmax,n2,ix2,nx2,x2,z2)
      call celladd2(xmax,xmin,zmax,zmax,n2,ix2,nx2,x2,z2)
      call celladd2(xmin,xmin,zmax,zmin,n2,ix2,nx2,x2,z2)
      call celladd2(xmin,xmax,zmin,zmin,n2,ix2,nx2,x2,z2)
c      call cellpplt(n2,ix2,nx2,x2,z2,92,'end of addn xx',0)

      call cellprnb(' end of celladdn',n2,ix2,nx2,x2,z2)
      return
  999 continue
      print'('' error in celladdn'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celladd1(xmin,xmax,zmin,zmax
     1,nx1,x1,z1,m2,n2,ix2,nx2,x2,z2,ierr)
      dimension x1(1),z1(1),ix2(1),nx2(1),x2(1),z2(1)
c  add the points from 1 - nx1 to x2,z2
      ierr = 0
      if (n2 .eq. 0) then
        jx2 = 0
      else
        jx2 = ix2(n2) + nx2(n2)
      endif
      nx = 0
      do 1 jx = 1 , nx1-1
        call celladd0(xmin,xmax,zmin,zmax
     1,x1(jx),z1(jx),x1(jx+1),z1(jx+1),nx,x2(jx2+1),z2(jx2+1),ierr)
        if (ierr .ne. 0) goto 999
    1 continue
      
c search through points for those within window
      k2 = n2 + 1
      jx0 = -1
      do 2 jx = jx2+1 , jx2+nx
        if (x2(jx) .lt. xmin .or. x2(jx) .gt. xmax
     1 .or. z2(jx) .lt. zmin .or. z2(jx) .gt. zmax) goto 2
        if (jx-1 .ne. jx0) then
          n2 = n2 + 1
          if (n2 .gt. m2) then
            print*,' need more memory in celladd1 n2=',n2,' m2=',m2
            ierr = 1
            goto 999
          endif
          ix2(n2) = jx - 1
          nx2(n2) = 0
        endif
        nx2(n2) = nx2(n2) + 1
        jx0 = jx
    2 continue

      return
  999 continue
      print'('' error in celladd1'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celladd0(xmin,xmax,zmin,zmax,x1,z1,x2,z2,nx,x,z,ierr)
c  determine which segments cross boundaries of xmin,xmax,zmin,zmax
      dimension x(1),z(1),xc(4),zc(4)
      data xc,zc/0,0,0,0,0,0,0,0/

c  determine if this segment crosses boundaries
      nc = 0
      call cellcbnd(zmin,z1,z2,x1,x2,nc,zc,xc,ierr)    ! top
      if (ierr .ne. 0) goto 999
      call cellcbnd(zmax,z1,z2,x1,x2,nc,zc,xc,ierr)    ! bottom
      if (ierr .ne. 0) goto 999
      call cellcbnd(xmin,x1,x2,z1,z2,nc,xc,zc,ierr)    ! left
      if (ierr .ne. 0) goto 999
      call cellcbnd(xmax,x1,x2,z1,z2,nc,xc,zc,ierr)    ! right
      if (ierr .ne. 0) goto 999

      r1 = celldist(x1,z1,xc(1),zc(1))
      r2 = celldist(x1,z1,xc(2),zc(2))

c  add the first point
      if (nx .eq. 0) call celladxz(1,x1,z1,nx,x,z)

c  add each of the crossing points
      do 2 ic = 1 , nc
        jc = 1
        if (nc .eq. 2
     1.and. ((ic .eq. 1 .and. r2 .lt. r1)
     1  .or. (ic .eq. 2 .and. r1 .lt. r2))) jc = 2
        call celladxz(1,xc(jc),zc(jc),nx,x,z)
    2 continue

c  add the last point
      call celladxz(1,x2,z2,nx,x,z)

      return
  999 continue
      print'('' error in celladd0'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celladxz(nx1,x1,z1,nx2,x2,z2)
      dimension x1(1),z1(1),x2(1),z2(1)
      do 1 i = 1 , nx1
        nx2 = nx2 + 1
        x2(nx2) = x1(i)
        z2(nx2) = z1(i)
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellcbnd(a0,a1,a2,b1,b2,nc,ac,bc,ierr)
      dimension ac(1),bc(1)
      ierr = 0
      if (min(a1,a2) .lt. a0 .and. max(a1,a2) .gt. a0) then
        nc = nc + 1
        ac(nc) = a0
        bc(nc) = b1 + (a0 - a1) * (b2 - b1) / (a2 - a1)
      endif
      if (nc .gt. 2) then
        print*,' nc=',nc
        ierr = 1 
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celladd2(x1,x2,z1,z2,n,ix,nx,x,z)
      dimension ix(1),nx(1),x(1),z(1)
      n = n + 1
      if (n .le. 1) then
        ix(n) = 0
      else
        ix(n) = ix(n-1) + nx(n-1)
      endif
      nx(n) = 0
      call celladxz(1,x1,z1,nx(n),x(ix(n)+1),z(ix(n)+1))
      call celladxz(1,x2,z2,nx(n),x(ix(n)+1),z(ix(n)+1))
      return
      end  

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellixnx(i,ix,nx)
      dimension ix(1),nx(1)
      if (i .eq. 1) then
        ix(i) = 0
      else
        ix(i) = ix(i-1) + nx(i-1)
      endif
      nx(i) = 0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellbtoc(tol,idir,nxb,xb,zb,nxc,xc,zc)
c  copy boundary values into cell values
      dimension xb(1),zb(1),xc(1),zc(1)
      logical cellsame
      if (idir .gt. 0) then
        ixb1 = 1
        ixb2 = nxb
      else
        ixb1 = nxb
        ixb2 = 1
      endif
      do 1 ixb = ixb1 , ixb2 , idir
        nxc = nxc + 1
        call cellcop2(1,xb(ixb),zb(ixb),xc(nxc),zc(nxc))
        if (cellsame(tol,xc(nxc-1),zc(nxc-1),xc(nxc),zc(nxc)) 
     1.and. nxc .ne. 1) nxc = nxc - 1
    1 continue    ! do 1 ixb = ixb1 , ixb2 , idir
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellseps(tol,ns,ibs,ixs,nxs,n,ix,nx,x,z,ierr,*)
c  separate boundaries into constituent segments
      dimension ibs(1),ixs(1),nxs(1),ix(1),nx(1),x(1),z(1)
      logical celllens,cellsame

      ns = 0
      do 1 i = 1 , n
        if (celllens(tol,i,ix,nx,x,z)) goto 1
        ns0 = ns
        ixs0 = 0
        nxs0 = 1
        do 2 ix0 = ix(i)+2 , ix(i)+nx(i)
          nxs0 = nxs0 + 1
          do 3 j = 1 , n
            if (i .eq. j) goto 3
            do 4 jx0 = ix(j)+1 , ix(j)+nx(j)
              if (cellsame(tol,x(ix0),z(ix0),x(jx0),z(jx0))) then
                ns = ns + 1
                ibs(ns) = i
                ixs(ns) = ixs0
                nxs(ns) = nxs0
                ixs0 = ix0 - ix(i) - 1
                nxs0 = 1
                ixlast = ix0
                goto 2
              endif    ! if (cellsame(tol,x(ix0),z(ix0),x(jx0),z(jx0))) then
    4       continue    ! do 4 jx0 = ix(j)+1 , ix(j)+nx(j)
    3     continue    ! do 3 j = 1 , n
    2   continue    ! do 2 ix0 = ix(i)+2 , ix(i)+nx(i)
        if (ns0 .eq. ns) then    ! if (ns0 .eq. ns) then
          call cell_get_lpr(lpr)
          write(lpr,'('' error neither seg or lens i='',i3
     1,'' nseg='',i3,'' ixlast='',i3)')i,ns,ixlast
          ierr = 5
          goto 999
        endif    ! if (ns0 .eq. ns) then
    1 continue    ! do 1 i = 1 , n
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celltrun(tol,mx,n,it,jt,ix,nx,x,z
     1,xmin,xmax,zmin,zmax,ierr,*)
      dimension it(1),ix(1),nx(1),x(1),z(1)
     1,xmin(1),xmax(1),zmin(1),zmax(1)
      logical celllens
      call cellnmax(tol,n,ix,nx,x,z,xmin,xmax,zmin,zmax)
c  search through each end point and see if it should be truncated or extended
      do 1 iloop = 1 , 2
        do 2 i = 1 , n
c  skip this section if this is a lens
          if ((iloop .eq. 1 .and. it(i) .eq. 0)
     1   .or. (iloop .ne. 1 .and. it(i) .ne. 0)
     1   .or. celllens(tol,i,ix,nx,x,z)) goto 2
          do 3 iend = 1 , 2
            if (iend .eq. 1) then
              ix1 = ix(i) + 1
              ix2 = ix(i) + nx(i)
              ins = 0
            else
              ix1 = ix(i) + nx(i)
              ix2 = ix(i) + 1
              ins = nx(i)
            endif

c  determine what segment this segment would end on if extended
            call cellextd(tol,k,kx,r0,x0,z0,i,ix1,ix2,n,ix,nx,x,z
     1,ierr,*999,*3)

c  this segment end should connect with segment kx of boundary k at x0,z0
c  search along boundary i to see if there is a connection with another
c  segment that is closer
            call cellcbck(tol,r0,r2,i,ix1,ix2,ix3,n,ix,nx,x,z
     1,xmin,xmax,zmin,zmax)

c  if the distance to truncate the boundary is shorter than the
c  distance to extend the boundary  (if r2 < r0)
c  we truncate boundary i at lx(ix1)
c  other wise we extend boundary i to the point x0,z0
c  and insert a point x0,z0 into boundary k

c  truncate boudnary i
          if (r2 .lt. r0) then
            if (iend .eq. 1) ix(i) = ix3 - 1
            nx(i) = abs(ix2 - ix3) + 1
            if (nx(i) .le. 1) then
              ierr= 52
              goto 999
            endif
          else    ! if (r2 .lt. r0) then
c  if this point is very close to the intersection replace it
            if (r0 .lt. 1e-3) then
              call cellcop2(1,x0,z0,x(ix1),z(ix1))
            else    ! if (r0 .lt. 1e-3) then
c  add a point to the end of segment i
              call cellins(ins,i,1,x0,z0,mx,n,ix,nx,x,z,*999)
              call cellimax(tol,x0,z0,xmin(i),xmax(i),zmin(i),zmax(i))
            endif    ! if (r0 .lt. 1e-3) then
c  insert a point into boundary k
            call cellins(kx,k,1,x0,z0,mx,n,ix,nx,x,z,*999)
          endif    ! if (r2 .lt. r0) then
    3   continue    ! do 3 iend = 1 , 2
    2 continue    ! do 2 i = 1 , n
    1 continue    ! do 1 iloop = 1 , 2

c  pack the info in these boundarys
      call cellprnb(' bef pack celltrun',n,ix,nx,x,z)
      call cellpack(tol,n,ix,nx,x,z)
      call cellprnb(' aft pack celltrun',n,ix,nx,x,z)

c  check the endpoints to be sure they fall on another boundary
      call cellchke(tol,n,ix,nx,x,z,xmin,xmax,zmin,zmax,ierr,*999)

      return
  999 continue
      call cellprnb('czzz error in celltrun',n,ix,nx,x,z)
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellcbck(tol,r0,r2,i,ix1,ix2,ix3,n,ix,nx,x,z
     1,xmin,xmax,zmin,zmax)
c  check back along an interface for the closest intersection
      dimension ix(1),nx(1),x(1),z(1),xmin(1),xmax(1),zmin(1),zmax(1)
      logical celllens,cellsame
      lxdir = sign(1,ix2-ix1)
      ix3 = ix1
      r2 = 0.
      do 1 ix0 = ix1 , ix2-lxdir , lxdir
        r2 = r2 + celldist(x(ix0),z(ix0),x(ix3),z(ix3))
        ix3 = ix0
        xmin0 = min(x(ix0),x(ix3))
        xmax0 = max(x(ix0),x(ix3))
        zmin0 = min(z(ix0),z(ix3))
        zmax0 = max(z(ix0),z(ix3))
        do 2 j = 1 , n
c  skip this boundary if is the same or a lens
          if (i .eq. j .or. celllens(tol,j,ix,nx,x,z)
     1 .or. xmin0 .gt. xmax(j) .or. xmax0 .lt. xmin(j)
     1 .or. zmin0 .gt. zmax(j) .or. zmax0 .lt. zmin(j)) goto 2
          do 3 jx1 = ix(j)+1 , ix(j)+nx(j)
            if (cellsame(tol,x(ix0),z(ix0),x(jx1),z(jx1))) goto 4
    3     continue    ! do 3 jx1 = ix(j)+1 , ix(j)+nx(j)
    2   continue    ! do 2 j = 1 , n
    1 continue    ! do 1 ix0 = ix1 , ix2 , lxdir
      r2 = r0 + 100
    4 continue    ! come to here when we find the nearest intersection
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellextd(tol,k,kx,r0,x0,z0,i,ix1,ix2,n,ix,nx,x,z
     1,ierr,*,*)
c  search through all segments and find the segment that lies nearest
c  in the direction away from this endpoint
      dimension ix(1),nx(1),x(1),z(1)
      logical celllens,cellsame,cellnots
      tol0 = tol / 1e3
      lxdir = sign(1,ix2-ix1)
      ix3 = ix1 + lxdir
      k = 0
      r0 = 1e8
      do 1 j = 1 , n
        if (i .eq. j .or. celllens(tol,j,ix,nx,x,z)) goto 1
        do 2 jx1 = ix(j)+1 , ix(j)+nx(j)
          if (ix1 .eq. jx1) goto 2
          jx2 = jx1 + 1
          if (cellsame(tol,x(jx1),z(jx1),x(ix1),z(ix1))) then
            call cellcop2(1,x(jx1),z(jx1),x(ix1),z(ix1))
            return 2
          endif    ! if (cellsame(tol,x(ix1),z(ix1),x(jx1),z(jx1))) then
          if (jx1 .eq. ix(j)+nx(j)) goto 2
          call cellcros(icros,x1,z1
     1,x(ix1),x(ix3),z(ix1),z(ix3),x(jx1),x(jx2),z(jx1),z(jx2))

c  if this segment is in the wrong direction from this endpoint of i
c  or if the intersection does not fall between the segment endpoints of jx1
c  skip to next segment
          if ((cellnots(tol,x1,z1,x(ix1),z(ix1)) .and. 
     1celldot(x1,x(ix1),z1,z(ix1),x(ix3),x(ix1),z(ix3),z(ix1)) .gt. 0.)
     1.or.celldot(x1,x(jx1),z1,z(jx1),x1,x(jx2),z1,z(jx2)).gt.0.)goto 2
          r1 = celldist(x1,z1,x(ix1),z(ix1))
          if (r1 .lt. r0 .or. k .eq. 0) then
            k = j
            kx = jx1 - ix(j)
            r0 = r1
            x0 = x1
            z0 = z1
          endif    ! if (r1 .lt. r0 .or. k .eq. 0) then
    2   continue    ! do 2 jx1 = ix(j)+1 , ix(j)+nx(j)
    1 continue    ! do 1 j = 1 , n

c  if there is no connection
      if (k .eq. 0) then
        call cell_get_lpr(lpr)
        write(lpr,'('' cellext i='',i5,'' ix1='',i5
     1,'' xz='',2(1x,f10.2))')i,ix1,x(ix1),z(ix1)
        ierr = 2
        return 1
      endif    ! if(k.eq.0)then
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellbndi(tol,mx,n,ix,nx,x,z,xmin,xmax,zmin,zmax,ierr,*)
c  determine where boundaryes intersect insert the shared locations
      dimension ix(1),nx(1),x(1),z(1),xmin(1),xmax(1),zmin(1),zmax(1)
      logical celllens,cellsame

      call cellnmax(tol,n,ix,nx,x,z,xmin,xmax,zmin,zmax)
      do 1 i = 1 , n
c  skip this boundary if it is a lens
        if (celllens(tol,i,ix,nx,x,z)) goto 1
        ix1 = ix(i)
    2   continue
          ix1 = ix1 + 1
          ix2 = ix1 + 1
          if (ix1 .ge. ix(i) + nx(i)) goto 1
          do 3 j = 1 , n
            if (i .eq. j .or. celllens(tol,j,ix,nx,x,z)
     1 .or. xmin(i) .gt. xmax(j) .or. xmax(i) .lt. xmin(j)
     1 .or. zmin(i) .gt. zmax(j) .or. zmax(i) .lt. zmin(j)) goto 3
            jx1 = ix(j)
    4       continue
            jx1 = jx1 + 1
            jx2 = jx1 + 1
            if (jx1 .ge. ix(j) + nx(j)) goto 3
            call cellcros(icros,x0,z0
     1,x(ix1),x(ix2),z(ix1),z(ix2),x(jx1),x(jx2),z(jx1),z(jx2))
            if (icros .ne. 1) goto 4
c  insert the point into boundaries i and j
c  insert a point into boundary i
            if (cellsame(tol,x0,z0,x(ix1),z(ix1))) then
              call cellcop2(1,x0,z0,x(ix1),z(ix1))
            elseif (cellsame(tol,x0,z0,x(ix2),z(ix2))) then
              call cellcop2(1,x0,z0,x(ix2),z(ix2))
            else
              jx3 = jx1 - ix(j)
              call cellins(ix1-ix(i),i,1,x0,z0,mx,n,ix,nx,x,z,*999)
              jx1 = jx3 + ix(j)    ! if j was shifted
            endif    ! if (cellsame(tol,x0,z0,x(ix1),z(ix1))) then
c  insert a point into boundary j
            if (cellsame(tol,x0,z0,x(jx1),z(jx1))) then
              call cellcop2(1,x0,z0,x(jx1),z(jx1))
            elseif (cellsame(tol,x0,z0,x(jx2),z(jx2))) then
              call cellcop2(1,x0,z0,x(jx2),z(jx2))
            else
              ix3 = ix1 - ix(i)
              call cellins(jx1-ix(j),j,1,x0,z0,mx,n,ix,nx,x,z,*999)
              ix1 = ix3 + ix(i)    ! if i was shifted
              jx1 = jx1 + 1
            endif    ! if (cellsame(tol,x0,z0,x(jx1),z(jx1))) then
            goto 4
    3     continue    ! do 3 j = 1 , n
          goto 2
    1 continue    ! do 1 i = 1 , n
      return
  999 continue
      ierr = 110
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      function celllens(tol,i,ix,nx,x,z)
c  decide if cell i is a lens (if the first and last point are the same)
      logical celllens,cellsame
      dimension ix(1),nx(1),x(1),z(1)
      celllens = cellsame(tol
     1,x(ix(i)+1),z(ix(i)+1),x(ix(i)+nx(i)),z(ix(i)+nx(i)))
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellfndl(nln,iln,mx,n,ix,nx,x,z,*)
c  determine which boundaries are lenses
      dimension iln(1),ix(1),nx(1),x(1),z(1)
      call cellmnm0(xmin,xmax,n,ix,nx,x)
      call cellmnm0(zmin,zmax,n,ix,nx,z)
      nln = 0
      do 1 i = 1 , n
        if (nx(i) .le. 2) goto 1
        ix1 = ix(i) + 1
        ix2 = ix(i) + nx(i)
        rmin = celldist(x(ix1),z(ix1),x(ix2),z(ix2))
c23456789012345678901234567890123456789012345678901234567890123456789012
        if (rmin .ge. abs(x(ix1)-xmin) .or. rmin .ge. abs(x(ix1)-xmax)
     1 .or. rmin .ge. abs(x(ix2)-xmin) .or. rmin .ge. abs(x(ix2)-xmax)
     1 .or. rmin .ge. abs(z(ix1)-zmin) .or. rmin .ge. abs(z(ix1)-zmax)
     1 .or. rmin .ge. abs(z(ix2)-zmin) .or. rmin .ge. abs(z(ix2)-zmax))
     1 goto 1
        do 2 j = 1 , n
          if (j .eq. i) goto 2
          do 3 jx = ix(j)+1 , ix(j)+nx(j)
            if (rmin .ge. min(celldist(x(jx),z(jx),x(ix1),z(ix1))
     1,celldist(x(jx),z(jx),x(ix2),z(ix2)))) goto 1
    3     continue
    2   continue
        nln = nln + 1
        iln(nln) = i
        if (rmin .ne. 0) 
     1call cellins(nx(i),i,1,x(ix1),z(ix1),mx,n,ix,nx,x,z,*999)
        ix2 = ix(i) + nx(i)

c  check to see if there are any segments that cross
        do 4 ix0 = ix1 , ix2-2
          do 5 jx0 = ix2-2 , ix0+2 , -1
            call cellcros(icros,x0,z0
     1,x(ix0),x(ix0+1),z(ix0),z(ix0+1),x(jx0),x(jx0+1),z(jx0),z(jx0+1))
c if the dot product of the vectors from the two segments
c to x0, z0 is positive the vectors are in the same direction and
c  the segments do not cross
            if (icros .eq. 1) then
              ix(i) = ix0 - 1
              nx(i) = jx0 - ix0 + 2
              call cellcop2(1,x0,z0,x(ix0),z(ix0))
              call cellcop2(1,x0,z0,x(jx0+1),z(jx0+1))
              goto 1
            endif    ! if (icros .eq. 1) then
    5     continue    ! do 5 jx0 = ix2-2 , ix0+2
    4   continue    ! do 4 ix0 = ix1 , ix2+2

    1 continue    ! do 1 i = 1 , n
      return
  999 continue
      print*,' error in cellfndl'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellcros(icros,x0,z0,x1a,x1b,z1a,z1b,x2a,x2b,z2a,z2b)
c  determine where the line segements defined by x1a,z1a to x1b,z1b
c  and x2a,z2a to x2b,z2b  cross and if it is between the endpoints
c  if it is icros = 1 if not icros = 0
      dx1 = x1b - x1a
      dz1 = z1b - z1a
      dx2 = x2b - x2a
      dz2 = z2b - z2a
      denom = dz1 * dx2 - dx1 * dz2
      if (denom .eq. 0.) then
        x0 = 0
        z0 = 0
        icros = 0
      else
        x0 = (x1a*dz1*dx2 - x2a*dx1*dz2 + (z2a-z1a) * dx1*dx2) / denom
        z0 = (z2a*dz1*dx2 - z1a*dx1*dz2 + (x1a-x2a) * dz1*dz2) / denom
c if the dot product of the vectors from the two segments
c to x0, z0 is positive the vectors are in the same direction and
c  the segments do not cross
        if (celldot(x0,x1a,z0,z1a,x0,x1b,z0,z1b) .gt. 0.
     1 .or. celldot(x0,x2a,z0,z2a,x0,x2b,z0,z2b) .gt. 0.) then
          icros = 0
        else
          icros = 1
        endif
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellacon(tol,r,dxi,dxj,dzi,dzj,js,ncon,acon,icon)
c  determine the angles between connecting segments
      dimension acon(2),icon(2)
      if (r .gt. tol) return
      pi = 2. * asin(1.)
      ncon = ncon + 1
      sn = (dxi * dzj - dzi * dxj)
      cs = (dxi * dxj + dzi * dzj)
      cs = sign(1.,cs) * min(1.,abs(cs))
      acon0 = pi + sign(1.,sn) * (acos(cs) - pi)
c  acon0 is the angle betwen 0 and 2 pi
c  if this is the first segment connecting to this segment end
c  the connections in both directions are the same
      if (ncon .eq. 1) then
        acon(1) = acon0
        icon(1) = js
        acon(2) = acon0
        icon(2) = js
      else
c  otherwise acon(1) is the smallest connecting angle
c  and acon(2) is the largest connecting angle
        if (acon0 .lt. acon(1)) then
          acon(1) = acon0
          icon(1) = js
        elseif (acon0 .ge. acon(2)) then
          acon(2) = acon0
          icon(2) = js
        endif    ! if (acon0 .lt. acon(1)) then
      endif    ! if (ncon .eq. 1) then
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celldir(n,x,z,*)
c  determine the direction of a cell boundary
c  if the angle swept if one goes from the first point in the cell
c  to the last point is counter clockwise reverse the direction of the cell
      call cellang(ang,n,x,z,ierr)
      if (ierr .ne. 0) goto 999
c  if ang > 0 this cell goes clockwise
c  if ang < 0 this cell goes counter clockwise and we reverse it
      if (ang .ge. 0) call cellrev2(n,x,z)
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellang(ang,n,x,z,ierr)
c  determine the angle swept if one goes from the first point in the cell
c  to the last point
      dimension x(1),z(1)
      data tol/1e-5/
      ierr = 0
      call cellpnt(x0,z0,n,x,z,ins,*999)
      if (ins .eq. 1) then
        i1 = 2
        i2 = n
        goto 1
      endif
  999 continue
      x0 = x(1)
      z0 = z(1)
      i1 = 3
      i2 = n - 1
      ierr = 53
      goto 998
    1 continue
      ang = 0
      dx1 = x(i1-1) - x0
      dz1 = z(i1-1) - z0
      dr1 = max(tol,sqrt(dx1**2 + dz1**2))
      dx1 = dx1 / dr1
      dz1 = dz1 / dr1
      sn = 0
      cs = 0
      dx2 = 0
      dz2 = 0
      do 2 i = i1 , i2
        dx2 = x(i) - x0
        dz2 = z(i) - z0
        dr2 = max(tol,sqrt(dx2**2 + dz2**2))
        dx2 = dx2 / dr2
        dz2 = dz2 / dr2
        sn = dx1 * dz2 - dz1 * dx2
        cs = dx1 * dx2 + dz1 * dz2
        cs = sign(1.,cs) * min(1.,abs(cs))
        ang = ang + sign(1.,sn) * acos(cs)
        dx1 = dx2
        dz1 = dz2
    2 continue    ! do 2 i = i1 , i2
      return
  998 continue
      call cellprnc(' error in cellang',0,1,1.,0.,1,0,n,x,z)
      ierr = 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellrev2(n,x,z)
c  reverse arrays x,z
      call cellrev(n,x)
      call cellrev(n,z)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellrev(n,x)
c  reverse array x
      dimension x(1)
      do 1 i1 = 1 , n / 2
        i2 = n - i1 + 1
        xtemp = x(i1)
        x(i1) = x(i2)
        x(i2) = xtemp
    1 continue    ! do 1 i1 = 1 , n / 2
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpack(tol,n,ix,nx,x,z)
c  pack a set of pointed arrays while shifting the start of each array
      dimension ix(1),nx(1),x(1),z(1)
      logical cellnots
      i2 = 0
      do 1 i = 1 , n
        i1 = ix(i) + 1
        i2 = i2 + 1
        call cellpac0(tol,nx(i),x(i1),z(i1),n2,x(i2),z(i2))
        ix(i) = i2 - 1
        nx(i) = n2
        i2 = ix(i) + nx(i)
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpac1(tol,n,ix,nx,x,z)
c  pack a set of pointed arrays without shifting the start of each array
      dimension ix(1),nx(1),x(1),z(1)
      logical cellnots
      do 1 i = 1 , n
        i1 = ix(i) + 1
        call cellpac0(tol,nx(i),x(i1),z(i1),n2,x(i1),z(i1))
        nx(i) = n2
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpac0(tol,n1,x1,z1,n2,x2,z2)
c  pack a set of points removeing the second of any pair of points
c  closer than tol to one another
      dimension x1(1),z1(1),x2(1),z2(1)
      logical cellnots
      n2 = 1
      call cellcop2(1,x1,z1,x2,z2)
      do 1 i1 = 2 , n1
        if (cellnots(tol,x1(i1),z1(i1),x2(n2),z2(n2))) then
          n2 = n2 + 1
          call cellcop2(1,x1(i1),z1(i1),x2(n2),z2(n2))
        elseif (n2 .ne. 1 .and. (i1 .eq. n1 .or. x1(i1) .lt. x2(n2)
     1 .or. (x2(i1) .eq. x2(n2) .and. z1(i1) .lt. z2(n2)))) then
          call cellcop2(1,x1(i1),z1(i1),x2(n2),z2(n2))
        endif
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpad(npad0,npad1,mx,n,ix,nx,x,z,*)
c  pad a pointed set of arrays by a maximum of npad0
c  however if there is at least npad1 points already between arrays
c  it doesn't do anything
      dimension ix(1),nx(1),x(1),z(1)

      if (n .le. 0) return
c  determine if each ix already has enough space
      iflag = 1
      nxsum = nx(n)
      do 1 i = 1 , n - 1
        if (ix(i+1)-ix(i)-nx(i) .lt. npad1) iflag = 0
        nxsum = nxsum + nx(i)
    1 continue
c  if there is return ... other wise shift the data
      npad = min(npad0,(mx-nxsum)/(n+1))
      if (iflag .eq. 1 .or. npad .le. 0) return
      do 2 i = n , 2 , -1
c x currently starts at ix and has nx pts
c there are nxsum pts from 1 to n - 1
c we want the new ix to be at ix0 = nxsum + (i - 1) * npad
c so we have to shift by ix0 - ix(i)
        nxsum = nxsum - nx(i)
        ix0 = nxsum + (i - 1) * npad
        nxshift = ix0 - ix(i)
        if (nxshift .lt. 0) then
          print*,' nxshift=',nxshift,' npad=',npad
          goto 999
        elseif (nxshift .eq. 0) then
          goto 2
        endif
        call cellshf2(nx(i),nxshift,x(ix(i)+1),z(ix(i)+1))
        ix(i) = ix0
    2 continue    ! do 2 i = n , 2 , -1
      return
  999 continue
      print*,' error in cellpad'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellins(ixi,ii,ni,xi,zi,mx,n,ix,nx,x,z,*)
c  insert ni points into x,z between points ixi and ixi+1 of boundary ii
      dimension xi(1),zi(1),ix(1),nx(1),x(1),z(1)
      data icall/0/
      icall = icall + 1
      jx1 = ix(ii) + ixi + 1
      if (ii .lt. n .and. ni .lt. ix(ii+1) - ix(ii) - nx(ii)) then
        jx2 = ix(ii) + nx(ii)
        nlast = ii
      else
        jx2 = ix(n) + nx(n)
        nlast = n
      endif
      if (jx2 + ni .gt. mx) goto 999

c  shift points beyond the insert location
      call cellshf2(jx2-jx1+1,ni,x(jx1),z(jx1))
c  insert points
      call cellcop2(ni,xi,zi,x(jx1),z(jx1))
c  shift pointers
      do 1 j = ii + 1 , nlast
        ix(j) = ix(j) + ni
    1 continue
      nx(ii) = nx(ii) + ni
      return

  999 continue
      write(6,*)' error in cellins need ',jx2+ni,' and have ',mx
     1,' icall=',icall,' n=',n,' ixi=',ixi,' ii=',ii
     1,' jx1=',jx1,' jx2=',jx2

      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellshf2(n,ns,x,z)
c  shift x,z by ns points
      dimension x(1),z(1)
      call cellshif(n,ns,x)
      call cellshif(n,ns,z)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellshif(n,ns,x)
c  shift x by ns points
      dimension x(1)
      do 1 i = n , 1 , -1
        x(i+ns) = x(i)
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellshfp(ns,i,n,it,v,g,ix,nx)
c  shift cell pointers it,v,g,ix,nx by ns points , n = n + ns
      dimension it(1),v(1),g(1),ix(1),nx(1)
      call cellshif(n-i,ns,it(i+1))
      call cellshif(n-i,ns,ix(i+1))
      call cellshif(n-i,ns,nx(i+1))
      call cellshif(n-i,ns,v(i+1))
      call cellshif(n-i,ns,g(i+1))
      n = n + ns
      return
      end

c&&&
c  the following deal with where points are
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellwher(x0,z0,is0,ic0,xs0,zs0,rs0,nc,ixc,nxc,xc,zc,*)
c  determine what cell x0,z0 is inside
c ic0 = cell #, is0 = segment #, xs0,zs0 = closest location on segment
c rs0 = distance from x0,z0 to xs0,zc0
c nc  = # of cells
c ixc = cell start pointer array
c nxc = # points in cell array
c xc  = array of cell x coordinates
c zc  = array of cell z coordinates
c *   = error return if cannot find cell
c  cell i occupies locations ixc(i)+1 - ixc(i)+nxc(i) in xc,zc
      implicit none
      integer  is0,ic0,nc,ixc(1),nxc(1)
      real     x0,z0,xs0,zs0,rs0,xc(1),zc(1)
      integer  ix0,ixc0,ic,ins
      ix0 = 0
      do ic = 1 , nc
        ixc0 = ixc(ic) + 1
        call cellpoly(x0,z0,nxc(ic),xc(ixc0),zc(ixc0),ins,*998)
        if (ins .eq. 1) then
          ic0 = ic
          call cellwseg(x0,z0,is0,xs0,zs0,rs0
     1,nxc(ic),xc(ixc0),zc(ixc0),*999)
          return
        endif
      enddo    ! do ic = 1 , nc

      return 1
  998 continue
      return 1
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellwseg(x0,z0,is,xs,zs,rs,nx,x,z,*)
      implicit none
      integer  is,nx
      real     x0,z0,xs,zs,rs,x(1),z(1)
      integer  nj,i,j
      real     xl,xr,xc,zl,zr,zc,rl,rr,rc
      nj = 11
      is = 1
      rs = sqrt((x0-x(1))**2+(z0-z(1))**2)
      do i = 1 , nx - 1
        xl = x(i)
        xr = x(i+1)
        zl = z(i)
        zr = z(i+1)
        rl = sqrt((x0-xl)**2+(z0-zl)**2)
        rr = sqrt((x0-xr)**2+(z0-zr)**2)
        if (rr .lt. rl) call cell_switch_3(xl,zl,rl,xr,zr,rr)

        do j = 1 , nj
          xc = (xl + xr) / 2.
          zc = (zl + zr) / 2.
          rc = sqrt((x0-xc)**2+(z0-zc)**2)
          if (rc .lt. rl) then
            call cell_replace_3(xl,zl,rl,xr,zr,rr)
            call cell_replace_3(xc,zc,rc,xl,zl,rl)
          elseif (rc .lt. rr) then
            call cell_replace_3(xc,zc,rc,xr,zr,rr)
          else
            goto 1
          endif

        enddo    ! do j = 1 , nj
    1   continue
        if (rc .lt. rs) then
          is = i
          call cell_replace_3(xc,zc,rc,xs,zs,rs)
        endif
      enddo    ! do i = 1 , nx - 1

      return
  999 continue
      print'('' error in cellwseg'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celliply(x0,z0,rs0,is0,nx,x,z,ins,*)
c  determine if a point x0,z0 is inside a closed polygon x,z
c  inside ins = 1  outside ins = 0
c  if ins=1 determine the distance r0 to the nearest segment ir0
      implicit none

      integer  is0,nx,ins
      real     x0,z0,rs0,x(1),z(1)
      integer  nj,i,j
      real     xs0,zs0

      is0 = 0
      rs0 = 0
      call cellpoly(x0,z0,nx,x,z,ins,*999)
      if (ins .eq. 1) call cellwseg(x0,z0,is0,xs0,zs0,rs0,nx,x,z,*999)

      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpoly(x0,z0,nx,x,z,ins,*)
c  determine if a point x0,z0 is inside a closed polygon x,z
c  inside ins = 1  outside ins = 0
      dimension x(*),z(*)
      data icall/0/

      icall = icall + 1
      iloop = 0
    1 continue
      ins = 0
      nabove = 0
      nbelow = 0
      do 2 ix = 1 , nx-1
        if (x0 .eq. x(ix) .and. z0 .eq. z(ix)) then
          ins = 1
          return
        endif
        if (x0 .ge. min(x(ix),x(ix+1))
     1.and. x0 .le. max(x(ix),x(ix+1))) then
          if (x(ix) .eq. x(ix+1)) then
            if (z0 .lt. min(z(ix),z(ix+1))) then
              nabove = nabove + 2
            elseif (z0 .gt. max(z(ix),z(ix+1))) then
              nbelow = nbelow + 2
            elseif (z0 .ge. min(z(ix),z(ix+1))
     1        .and. z0 .le. max(z(ix),z(ix+1))) then
              ins = 1
              return
            endif
          else    ! if (x(ix) .eq. x(ix+1)) then
            z1 = z(ix)+(x0-x(ix))*(z(ix+1)-z(ix))/(x(ix+1)-x(ix))
            ip = mod(ix+1,nx-1) + 1
            iab = 1
            if (x0 .eq. x(ix+1) .and. x(ix+1) .gt. min(x(ix),x(ip)) 
     1.and. x(ix+1) .lt. max(x(ix),x(ip))) iab = 0
            if (z1 .eq. z0) then
              ins = 1
              return
            elseif (z1 .lt. z0) then
              nabove = nabove + iab
            else
              nbelow = nbelow + iab
            endif    ! if (z1 .eq. z0) then
          endif    ! if (x(ix) .eq. x(ix+1)) then
        endif    ! if (x0 .ge. min(x(ix),x(ix+1))
      if (iloop .ne. 0) write(6,'(4(1x,i3),7(1x,f8.2))')
     1ix,iab,nabove,nbelow,x0,z0,z1,x(ix),x(ix+1),z(ix),z(ix+1)
      if (iloop .ne. 0) write(88,'(4(1x,i3),7(1x,f8.2))')
     1ix,iab,nabove,nbelow,x0,z0,z1,x(ix),x(ix+1),z(ix),z(ix+1)
    2 continue    ! do 2 ix = 1 , nx-1

      if (mod(nabove+nbelow,2) .ne. 0) then
        if (iloop .eq. 0) then
      print'('' error in cellpoly ''
     1,''writing info to logical unit 88'')'
          write(88,*)nx,x0,z0
          do 88 ix = 1 , nx
            write(88,*)x(ix),z(ix)
   88     continue
          print'('' error in cellpoly icall='',i5
     1,'' na='',i5,'' nb='',i5)',icall,nabove,nbelow
          write(88,'('' error in cellpoly icall='',i5
     1,'' na='',i5,'' nb='',i5)')icall,nabove,nbelow
          write(88,*)' x0=',x0,' z0=',z0
          write(88,'('' ix na nb x0 x1 x2 z0 z1'')')
        endif
        iloop = iloop + 1
        if (iloop .eq. 1) goto 1
        goto 999
      endif

      if (mod(nabove,2) .eq. 1) ins = 1
c      print*,' ins=',ins,' na=',nabove,' nb=',nbelow,' il=',iloop
      return

  999 continue
      print'('' error in cellpoly'')'
      return 1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellncrs(x0,n,x,z,nc,zc,work)
c  find the crossing points in cell at x0
      dimension x(1),z(1),zc(1),work(1)
      nc = 0
      do 1 i = 2 , n
        im = i - 1
        ip = mod(i-1,n-1) + 2
        dx1 = x(i) - x(im)
        dx2 = x(ip) - x(i)
c  if x0 falls between 2 x points
c  or it falls on this point and the next point count this as a crossing
        if ((x0 .eq. x(i) .and. dx1*dx2 .ge. 0. 
     1.and. abs(dx1)+abs(dx2) .ne. 0.)
     1.or. (x0.gt.min(x(i),x(ip)) .and. x0.lt.max(x(i),x(ip)))) then
          nc = nc + 1
          if (x(i)  .eq. x(ip)) then
            zc(nc) = z(i)
          else
            zc(nc) = z(i) + (x0 - x(i)) 
     1* (z(ip) - z(i)) / (x(ip) - x(i))
          endif
        endif
    1 continue
      call cellsort(nc,zc,work)
      call cellord(nc,zc,work(1),work(nc+1))
c  check to see the number of times we hit the cell is even
      if (mod(nc,2) .ne. 0) then
        print*,' error in cellncrs nc=',nc,' writing info to 88'
        print*,' x0=',x0,' zc=',(zc(ic),ic=1,nc)
        write(88,*)' n=',n,' x0=',x0
        write(88,'(2(1x,f10.2),1x,i5)')(x(i),z(i),0,i=1,n)
        write(88,'(2(1x,f10.2),1x,i5)')
     1(x0-2.,zc(ic),ic,x0+2.,zc(ic),ic,ic=1,nc)
        write(88,'(2(1x,f10.2),1x,i5)')
     1(x0,zc(ic)-2.,ic,x0,zc(ic)+2.,ic,ic=1,nc)
        nc = nc - 1
        zc(nc) = zc(nc+1)
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellcop(tol,xmin,xmax,zmin,zmax,n,ix
     1,nx,x,z,isop,icop,ierr,*)
c  detemrine the cell and segment opposite each segment
      dimension ix(1),nx(1),x(1),z(1),icop(1),isop(1)

      ierr = 0
      call celltol(tol,xmin,xmax)
      tol0 = tol / 20.
      call cellsetv(ix(n)+nx(n),icop,0)
      do 1 i = 1 , n
        do 2 jx = ix(i)+1 , ix(i)+nx(i)-1
          if (icop(jx) .eq. 0) then
            if (abs(z(jx)-zmin) .le. tol0
     1  .and. abs(z(jx+1)-zmin) .le. tol0) icop(jx) = -1
            if (abs(z(jx)-zmax) .le. tol0
     1  .and. abs(z(jx+1)-zmax) .le. tol0) icop(jx) = -3
            if (abs(x(jx)-xmin) .le. tol0
     1  .and. abs(x(jx+1)-xmin) .le. tol0) icop(jx) = -2
            if (abs(x(jx)-xmax) .le. tol0
     1  .and. abs(x(jx+1)-xmax) .le. tol0) icop(jx) = -4
            isop(jx) = icop(jx)
            if (icop(jx) .eq. 0) 
     1call cellsop(i,jx,n,ix,nx,x,z,isop,icop,ierr)
          endif    ! if (icop(jx) .eq. 0) then
    2   continue    ! do 2 jx = ix(i)+1 , ix(i)+nx(i)
        jx = ix(i) + nx(i)
        icop(jx) = icop(jx-1)
        isop(jx) = isop(jx-1)
    1 continue    ! do 1 i = 1 , n
      if (ierr .ne. 0) goto 999
      return
  999 continue
      call cell_get_lpr(lpr)
      write(lpr,'('' error in cellcop calculating opposite cells'')')
      call cellpplt(n,ix,nx,x,z,lpr,'end of addn xx',0)
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellsop(i,ix0,n,ix,nx,x,z,isop,icop,ierr)
c  find the opposite segment to the points x1,x2,z1,z2
      dimension icop(1),isop(1),ix(1),nx(1),x(1),z(1)
      x1 = x(ix0)
      x2 = x(ix0+1)
      z1 = z(ix0)
      z2 = z(ix0+1)
      ropmin = 1e10
      do 1 j = 1 , n
        do 2 jx = ix(j)+1,ix(j) + nx(j)-1
          if (j .eq. i .and. jx .ge. ix0-1 .and. jx .le. ix0+1) goto 2
          rop = celldist(x1,z1,x(jx+1),z(jx+1))
     1        + celldist(x2,z2,x(jx),z(jx))
          if (rop .lt. ropmin .or. icop(ix0) .eq. 0) then
            icop(ix0) = j
            isop(ix0) = jx - ix(j)
            ropmin = rop
          endif    ! if (rop .lt. ropmin .or. icop(ix0) .eq. 0) then
    2   continue    ! do 2 jx = ix(j)+1,
    1 continue    ! do 1 j = 1 , n
c  set the opposite segments parameters
      jx = ix(icop(ix0)) + isop(ix0)
      icop(jx) = i
      isop(jx) = ix0 - ix(i)
      if (ropmin .gt. 1.) then
c      print*,' ropmin=',ropmin,' ix0=',ix0,' i=',i,' ix=',ix0-ix(i)
c     1,' j=',icop(ix0),' jx=',jx
c      print'('' 1x='',2(1x,f10.2),'' z='',2(1x,f10.2)
c     1,/,'' 2x='',2(1x,f10.2),'' z='',2(1x,f10.2))'
c     1,x1,x2,z1,z2,x(jx+1),x(jx),z(jx+1),z(jx)
        ierr = 19
      endif
      return
      end

c&&&
c the following finds points inside a cell
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpnt(x0,z0,nx,x,z,ins,*)
c  find a point inside a cell round it off
      dimension x(1),z(1)
      call cellpnt0(x0,z0,nx,x,z,ins,*999)
      if (ins .eq. 1) then
        x2 = x0
        z2 = z0
        i0 = 1
        do i = 1 , 5
          x1 = float(i0*nint(x0/i0))
          z1 = float(i0*nint(z0/i0))
          call celliply(x1,z1,r0,ir0,nx,x,z,ins0,*999)
          if (ins0 .eq. 1) then
            i0 = i0 * 10
            x2 = x1
            z2 = z1
          else
            goto 1
          endif
        enddo    ! do i = 1 , 5
    1   continue
        x0 = x2
        z0 = z2
      endif    ! if (ins .eq. 1) then

      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpnt0(x0,z0,nx,x,z,ins,*)
c  find a point inside a cell
      implicit none
      integer  nx,ins
      real     x0,z0,x(1),z(1)
      integer  ir0,irp,nx0,nz0,ix,iz,jx,ny,iy,jy
      real     dx0,dz0,xp,zp,rp,r0,x1,x2,z1,z2,work(50),y(50)
      ins = 0

      rp = 0
      irp = 0

c  try points in middle
      call cellave(nx-1,x,x0)
      call cellave(nx-1,z,z0)
      call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
      call cell_better_point(ins,irp,x0,z0,r0,xp,zp,rp)

c  try the x center line
      call cellave(nx-1,x,x0)
      call cellncrs(x0,nx,x,z,ny,y,work)
      if (ny .gt. 0) then
        jy = 1
        do iy = 2 , ny-1
          if (abs(y(iy+1)-y(iy)) .gt. abs(y(jy+1)-y(jy))) jy = iy
        enddo
        z0 = (y(jy+1) + y(jy)) / 2.
        call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
        call cell_better_point(ins,irp,x0,z0,r0,xp,zp,rp)
      endif

c  try the z center line
      call cellave(nx-1,z,z0)
      call cellncrs(z0,nx,z,x,ny,y,work)
      if (ny .gt. 0) then
        jy = 1
        do iy = 2 , ny-1
          if (abs(y(iy+1)-y(iy)) .gt. abs(y(jy+1)-y(jy))) jy = iy
        enddo
        x0 = (y(jy+1) + y(jy)) / 2.
        call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
        call cell_better_point(ins,irp,x0,z0,r0,xp,zp,rp)
      endif

c  if any of these are acceptable end
      if (irp .ne. 0) then
        call cell_replace_3(xp,zp,rp,x0,z0,r0)
        ins = 1
        goto 1
      endif

c  try a pattern through the cell
      call cellmnmx(x1,x2,nx,x)
      call cellmnmx(z1,z2,nx,z)
      nx0 = 11
      nz0 = 11
      dx0 = (x2 - x1) / (nx0 - 1)
      dz0 = (z2 - z1) / (nz0 - 1)
      do ix = 2 , nx0 - 1
        x0 = x1 + (ix - 1) * dx0
        do iz = 2 , nz0 - 1
          z0 = z1 + (iz - 1) * dz0
          call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
          call cell_better_point(ins,irp,x0,z0,r0,xp,zp,rp)
        enddo    ! do iz = 2 , nz0 - 1
      enddo    ! do ix = 2 , nx0 - 1

c  if any of these are acceptable end
      if (irp .ne. 0) then
        call cell_replace_3(xp,zp,rp,x0,z0,r0)
        ins = 1
        goto 1
      endif

      do jx = 2 , nx - 1
        x0 = (x(jx-1) + x(jx) + x(jx+1)) / 3.
        z0 = (z(jx-1) + z(jx) + z(jx+1)) / 3.
        call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
        if (ins .eq. 1) goto 1
      enddo    ! do jx = 2 , nx - 1

c  could not find point
      x0 = 0
      z0 = 0
    1 continue

      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpntl(x0,z0,nx,x,z,ins,*)
c  find a point inside a cell near the left side
      dimension x(1),z(1)
      call cellmnmx(x1,x2,nx,x)
      call cellmnmx(z1,z2,nx,z)

      nx0 = 11
      nz0 = 5
      dx0 = (x2 - x1) / (nx0 - 1)
      dz0 = (z2 - z1) / (nz0 - 1)
      do 2 ix = 2 , nx0 - 1
      x0 = x1 + (ix - 1) * dx0
        iz = nz0/2+1
        do 3 jz = 2 , nz0-1
          iz = iz + 1
          if (iz .gt. nz0-1) iz = 2
          z0 = z1 + (iz - 1) * dz0
          call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
          if (ins .eq. 1) return
    3   continue
    2 continue
      call cellave(nx-1,x,x0)
      call cellave(nx-1,z,z0)
      call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
      if (ins .eq. 1) return
      do 4 lx = 2 , nx - 1
        x0 = (x(lx-1) + x(lx) + x(lx+1)) / 3.
        z0 = (z(lx-1) + z(lx) + z(lx+1)) / 3.
        call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
        if (ins .eq. 1) return
    4 continue
      x0 = 0
      z0 = 0
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpntr(x0,z0,nx,x,z,ins,*)
c  find a point inside a cell near the right side
      dimension x(1),z(1)
      call cellmnmx(x1,x2,nx,x)
      call cellmnmx(z1,z2,nx,z)

      nx0 = 11
      nz0 = 5
      dx0 = (x2 - x1) / (nx0 - 1)
      dz0 = (z2 - z1) / (nz0 - 1)
      do 2 ix = 2 , nx0 - 1
        x0 = x2 - (ix - 1) * dx0
        iz = nz0/2+1
        do 3 jz = 2 , nz0-1
          iz = iz + 1
          if (iz .gt. nz0-1) iz = 2
          z0 = z1 + (iz - 1) * dz0
          call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
          if (ins .eq. 1) return
    3   continue
    2 continue

      call cellave(nx-1,x,x0)
      call cellave(nx-1,z,z0)
      call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
      if (ins .eq. 1) return
      do 4 lx = 2 , nx - 1
        x0 = (x(lx-1) + x(lx) + x(lx+1)) / 3.
        z0 = (z(lx-1) + z(lx) + z(lx+1)) / 3.
        call celliply(x0,z0,r0,ir0,nx,x,z,ins,*999)
        if (ins .eq. 1) return
    4 continue
      x0 = 0
      z0 = 0
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cell_better_point(ins,irp,x0,z0,r0,xp,zp,rp)
      if (ins .eq. 1 .and. r0 .gt. rp) then
        call cell_replace_3(x0,z0,r0,xp,zp,rp)
        irp = 1
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cell_replace_3(a1,a2,a3,b1,b2,b3)
      implicit none
      real a1,a2,a3,b1,b2,b3
      b1 = a1
      b2 = a2
      b3 = a3
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cell_switch_3(a1,a2,a3,b1,b2,b3)
      implicit none
      real a1,a2,a3,b1,b2,b3
      call cell_switch(a1,b1)
      call cell_switch(a2,b2)
      call cell_switch(a3,b3)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cell_switch(a,b)
      implicit none
      real     a,b,c
      c = a
      a = b
      b = c
      return
      end

c&&&
c  the following are use to compute spline coeeficients

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellspln(n,ix,nx,x,z,id,b,c,d)
c  determine the spline fit coefficients
      dimension ix(1),nx(1),x(1),z(1),id(1),b(1),c(1),d(1)
      do 1 i = 1 , n
        j = ix(i) + 1
        call cellspld(nx(i),x(j),z(j),id(j),b(j),c(j),d(j))
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellspl0 (n,x,y,b,c,d)
c  adapted from spline
c    call cellspl0  (n,x,y,b,c,d)                    cOMPUTE COEFFICIENTS.
c    call cellsplv (n,x,y,b,c,d,xval,ideriv,yval)    eVALUATE FUNCTION.
c    call cellspld (n,x,y,ids,b,c,d)                cOMPUTE COEF WHEN Dx/Dy
c                                                  HAS disconTINUITIES
c
c  entry cellspl0 PARAMETERS:
c     iNPUT:
c        n      nUMBER OF DATA POINTS.
c        x,y    aRRAYS OF LENGTH n CONTAINING ABSCISSAS AND ORDINATES
c               OF THE DATA POINTS.   aRRAY x MUST BE STRICTLY MONOTONE
c               INCREASING OR DECREASING.
c     oUTPUT:
c        b,c,d  aRRAYS OF LENGTH n CONTAINING COMPUTED SPLINE COEFFI-
c               CIENTS.  iF xval LIES BETWEEN x(I) AND x(I+1), THEN
c                   f(xval) = y(I) + b(I)*h + c(I)*h**2 + d(I)*h**3
c               WHERE    h = xval - x(I).
c              nOTE:  c(1) = c(n) = 0  ;  b(n) AND d(n) ARE NOT SET.
c
c  eNTRY cellsplv PARAMETERS:
c     iNPUT:
c        n       nUMBER OF DATA POINTS.
c        x,y     aRRAYS OF LENGTH n CONTAINING DATA POINTS AS IN spline.
c        b,c,d   aRRAYS OF LENGTH n CONTAINING SPLINE COEFFICIENTS
c                PREVIOUSLY COMPUTED BY entry cellspl0.
c        xval    vALUE OF x FOR WHICH INTERPOLATION IS DESIRED.
c        ideriv  0 TO COMPUTE FUNCTION, OR 1 TO COMPUTE DERIVATIVE.
c     oUTPUT:
c        yval    cOMPUTED VALUE OF FUNCTION OR DERIVATIVE.
C
c  eNTRY cellspld PARAMETERS:
c     iNPUT:
c        n      nUMBER OF DATA POINTS.
c        x,y    aRRAYS OF LENGTH n CONTAINING ABSCISSAS AND ORDINATES
c               OF THE DATA POINTS.   aRRAY x NEED NOT BE STRICTLY MONOTONE
c               INCREASING OR DECREASING BUT CAN REVERSE OR HAVE 
c               DISCONTINUITIES IN ITS GRADIENT.
c        ids    iNTEGER ARRAY OF LENGTH n CONTAINING A FLAG TO INDICATE 
c               WHETHER THE DERIVITIVE IS CONTINUOUS (ids >=0) OR
c               DISCONTINUOUS (ids<0).
c     oUTPUT:
c        b,c,d  aRRAYS OF LENGTH n CONTAINING COMPUTED SPLINE COEFFI-
c               CIENTS.  iF xval LIES BETWEEN x(I) AND x(I+1), THEN
c                   f(xval) = y(I) + b(I)*h + c(I)*h**2 + d(I)*h**3
c               WHERE    h = xval - x(I).
c              nOTE:  c(1) = c(n) = 0  ;  b(n) AND d(n) ARE NOT SET.
c              nOTE: IF x(I) = x(I+1), b(I) = c(I) = d(I) = 0.
      dimension x(*),y(*),b(*),c(*),d(*)
      n1 = 1
      n2 = n
      m1 = n1 + 1
      m2 = n2 - 1
      s = 0.
      do 1000 k=n1,m2
         d(k) = x(k+1) - x(k)
         r = (y(k+1)-y(k))/d(k)
         c(k) = r - s
         s = r
 1000 continue
      c(n2) = 0.
      c(n1) = 0.
      s = 0.
      r = 0.
      do 1010 k=m1,m2
         c(k) = c(k) + r*c(k-1)
         b(k) = 2.*(x(k-1)-x(k+1)) - r*s
         s = d(k)
         r = s/b(k)
 1010 continue
      do 1020 l=m2,m1,-1
 1020 c(l) = (d(l)*c(l+1)-c(l)) / b(l)
      do 1030 k=n1,m2
         if (d(k) .eq. 0) then
           b(k) = 0
           d(k) = 0
         else
           b(k) = (y(k+1)-y(k))/d(k) - (c(k)+c(k)+c(k+1))*d(k)
           d(k) = (c(k+1)-c(k))/d(k)
         endif
         c(k) = 3.*c(k)
 1030 continue
      return
c*****************************************************
      entry cellsplv (n,x,y,b,c,d,xval,ideriv,yval)
      if (ideriv.lt.0 .or. ideriv.gt.1)  then
         yval = 0.
         return
      end if
      if (x(n) .ge. x(1))  then
         do 2000 i=n-1,2,-1
            if (xval .ge. x(i))  go to 2020
 2000    continue
         i = 1
      else
         do 2010 i=n-1,2,-1
            if (xval .le. x(i))  go to 2020
 2010    continue
         i = 1
      end if
 2020 h = xval - x(i)
      if (ideriv .eq. 0)  then
         yval = ((d(i)*h + c(i)) * h  +  b(i))  *  h   +   y(i)
      else
         yval = (3.*d(i)*h + 2.*c(i)) * h  +  b(i)
      end if
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellspld(nx,x,y,ids,b,c,d)
      dimension x(*),y(*),ids(*),b(*),c(*),d(*)
c  initialize the pointer for the previous discontinuity to the first point
c  initialize the counter for number of spline fit points
      ix1 = 1
c  cycle over points in a boundary
    1 continue
      xsign = sign(1.,x(ix1+1) - x(ix1))
      do 2 ix = ix1 + 1 , nx
        ix2 = ix
        if (ix .lt. nx) then
          xnext = x(ix+1)
        else
          xnext = x(ix) + xsign
        endif
        if (ids(ix) .lt. 0 .or. (xnext - x(ix)) * xsign .le. 0
     1 .or. (x(ix) - x(ix-1)) * xsign .le. 0) goto 3
    2 continue
c  if this is a discontinuity or the last point
c  compute the spline coefficients between
c  the previous discontinuity or the first point
    3 continue
      nx1 = ix2 - ix1 + 1
c for a vertical segment set all coefficients to 0
      if (x(ix1) .eq. x(ix1+1)) then
        do 4 ix = ix1 , ix2
          b(ix) = 0
          c(ix) = 0
          d(ix) = 0
    4   continue
      else
c  compute spline coefficients
        call cellspl0(nx1,x(ix1),y(ix1),b(ix1),c(ix1),d(ix1))
      endif
      ix1 = ix2
      if (ix1 .lt. nx) goto 1
      return
      end

c&&&
c  the following are useful primitives

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellsetv(n,x,x0)
c  set an array to a constant
      implicit none
      integer i,n
      real x0,x(1)
      do 1 i = 1 , n
        x(i) = x0
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellline(n,x,x0,dx)
c  set an array to a constant plus a linear gradient
      implicit none
      integer i,n
      real x0,dx,x(1)
      do 1 i = 1 , n
        x(i) = x0 + (i - 1) * dx
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellcopy(n,x1,x2)
c  copy n points from arrays x1 into x2
      implicit none
      integer i,n
      real x1(1),x2(1)
      do 1 i = 1 , n
        x2(i) = x1(i)
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellcop2(n,x1,z1,x2,z2)
c  copy n points from arrays x1,z1 into x2,z2
      dimension x1(1),z1(1),x2(1),z2(1)
      do 1 i = 1 , n
        x2(i) = x1(i)
        z2(i) = z1(i)
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      function cellsame(tol,x1,z1,x2,z2)
c  cellsame = .true. if 2 points are less than tol apart
      logical cellsame
      if (celldist(x1,z1,x2,z2) .le. tol) then
        cellsame = .true.
      else
        cellsame = .false.
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      function cellnots(tol,x1,z1,x2,z2)
c  cellnots = .true. if 2 points are greater than tol apart
      logical cellnots
      if (celldist(x1,z1,x2,z2) .le. tol) then
        cellnots  = .false.
      else
        cellnots = .true.
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      function celldot(x1a,x1b,z1a,z1b,x2a,x2b,z2a,z2b)
      celldot = (x1b - x1a) * (x2b - x2a) + (z1b - z1a) * (z2b - z2a)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellwork(i,n,*)
c  assign work space within an array
      implicit none
      integer i,n,i0,m,icall
      data icall/0/,i0/1/,m/0/
      i = i0
      i0 = i0 + n
      icall = icall + 1
c      print'('' memory icall='',i5,'' i='',i8,'' n='',i8,'' l='',i8
c     1,'' m='',i8)',icall,i,n,m-i+1,m
      if (i0-1 .gt. m) then
        print'('' error in work icall='',i8,'' asking for'',i8
     1,/,''need more work space have'',i8,'' and need'',i8)'
     1,icall,n,m,i0
        return 1
      endif
      return
      entry cellwors(i,n)
      i0 = i
      m = n
c      print'('' memory setup i='',i8,'' m='',i8)',i0,m
      return
      entry cellworl(n)
      n = m - i0 + 1
c      print'('' memory left i='',i8,'' m='',i8,'' n='',i8)',i0,m,n
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellsumn(n1,n2,x1,x2)
c  sum x2(i1) = sum i2=1,n2 x1(i1,i2) for i1 = 1,n1  dim x1(n1,1)
      dimension x1(n1,1),x2(1)
      call cellsetv(n1,x2,0.)
      do 1 i2 = 1 , n2
        do 2 i1 = 1 , n1
          x2(i1) = x2(i1) + x1(i1,i2)
    2   continue
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellscal(n,x,scale)
c  x(i) = x(i) * scal i=1,n
      dimension x(1)
      do 1 i = 1 , n
        x(i) = x(i) * scale
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellscac(scale,xmin,xmax,zmin,zmax)
c  determine a scale factor from xmin,xmax,zmin,zmax
      if (xmin .ne. xmax .and. zmin .ne. zmax) then
        scale = abs((xmax-xmin)/(zmax-zmin))
      else
        scale = 1.
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      function celldist(x1,z1,x2,z2)
c  distance between 2 points
      celldist = sqrt((x1-x2)**2+(z1-z2)**2)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellsum(n,x,xsum)
c  sum n points
      dimension x(1)
      xsum = 0.
      do 1 i = 1 , n
        xsum = xsum + x(i)
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellave(n,x,xave)
c  average n points
      call cellsum(n,x,xsum)
      xave = xsum / max(1,n)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellmnmx(xmin,xmax,nx,x)
      dimension x(1)
c  find min and max values within an array
      if (nx .ge. 1) then
        xmin = x(1)
        xmax = x(1)
        do 1 ix = 2 , nx
          xmin = min(xmin,x(ix))
          xmax = max(xmax,x(ix))
    1   continue
      else
        xmin = 0
        xmax = 0
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellmnm0(xmin,xmax,n,ix,nx,x)
      dimension ix(1),nx(1),x(1)
c  find min and max values within a pointed array
      if (n .ge. 1 .and. nx(1) .ge. 1) then
        xmin = x(ix(1)+1)
        xmax = x(ix(1)+1)
        do 1 jx = 1 , n
          do 2 kx = ix(jx)+1 , ix(jx)+nx(jx)
            xmin = min(xmin,x(kx))
            xmax = max(xmax,x(kx))
    2     continue
    1   continue
      else
        xmin = 0
        xmax = 0
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellimax(tol,x0,z0,xmin,xmax,zmin,zmax)
      xmin = min(xmin,x0-tol)
      xmax = max(xmax,x0+tol)
      zmin = min(zmin,z0-tol)
      zmax = max(zmax,z0+tol)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellmax2(n,ix,nx,x,z,xmin,xmax,zmin,zmax)
c  determine min,max of 2 pointed arrays
      call cellmax1(n,ix,nx,x,xmin,xmax)
      call cellmax1(n,ix,nx,z,zmin,zmax)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellmax1(n,ix,nx,x,xmin,xmax)
c  determine min,max of 1 pointed array
      dimension ix(1),nx(1),x(1)
      xmin = 0
      xmax = 0
      do 1 i = 1 , n
        call cellmnmx(xmin0,xmax0,nx(i),x(ix(i)+1))
        if (i .eq. 1) then
          xmin = xmin0
          xmax = xmax0
        else
          xmin = min(xmin,xmin0)
          xmax = max(xmax,xmax0)
        endif
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellnmax(tol,n,ix,nx,x,z,xmin,xmax,zmin,zmax)
      dimension ix(1),nx(1),x(1),z(1),xmin(1),xmax(1),zmin(1),zmax(1)
      do 1 i = 1 , n
        call cellmnmx(xmin(i),xmax(i),nx(i),x(ix(i)+1))
        call cellmnmx(zmin(i),zmax(i),nx(i),z(ix(i)+1))
        xmin(i) = xmin(i) - tol
        xmax(i) = xmax(i) + tol
        zmin(i) = zmin(i) - tol
        zmax(i) = zmax(i) + tol
    1 continue
      return
      end

c&&&
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celltol(tol,xmin,xmax)
c  compute tolerance from xmin,xmax
      tol = max(abs(xmax-xmin)/1e5,min(abs(xmax-xmin)/1e3,1.))
      if (tol .le.0.) print'('' error in celltol tol='',f14.6
     1,'' tol='',f14.6,/,'' xmin='',f10.2,'' xmax='',f10.2)'
     1,tol,xmin,xmax
      if (tol .le. 0.) tol = 1.
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celldeli(ndec,n,ix,nx,x,z,*)
c  decimate array - keep every ndec point
      dimension ix(1),nx(1),x(1),z(1)
      if (ndec .le. 0) return
      jx = 0
      do 1000 i = 1 , n
        jx0 = jx
        jdec = ndec + 1
        if (nx(i) .lt. 5 * jdec) jdec = 1
        do 1001 kx = ix(i)+1 , ix(i)+nx(i) , jdec
          jx = jx + 1
          x(jx) = x(kx)
          z(jx) = z(kx)
 1001   continue
        if (mod(nx(i),jdec) .ne. 1 .and. jdec .ne. 1) then
          kx = ix(i) + nx(i)
          jx = jx + 1
          x(jx) = x(kx)
          z(jx) = z(kx)
        endif
        ix(i) = jx0
        nx(i) = jx - jx0
 1000 continue
      return
  999 continue
      print*,' errror in celldeli'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celldeci(ang,n,ix,nx,x,z)
c  decimate n vectors
c  ang = angle threshold for deleting points (radians)
c  n = # of vectors to decimate
c  ix = poniter to start of each vector
c  nx = # of points in each vector
c  x = x coordinates
c  z = z coordinates
      dimension ix(1),nx(1),x(1),z(1)
      do 1 i = 1 , n
        call celledit(ang,nx(i),x(ix(i)+1),z(ix(i)+1))
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine celledit(ang,n,x,z)
c  decimate a set of x,z points
c  ang = angle threshold for deleting points (radians)
c  n = # of points in x,z
c  x = x coordinates
c  z = z coordinates
      dimension x(1),z(1)
c  edit x,z pairs to a limit of ang degrees
      j = 1
      do 1 i = 2 , n - 1
c  take dot product of segmenti with segment i + 1
        x1 = x(i) - x(j)
        x2 = x(i+1) - x(i)
        z1 = z(i) - z(j)
        z2 = z(i+1) - z(i)
        if (acos(
     1max(-1.,min(1.,(x1*x2+z1*z2)/sqrt((x1**2+z1**2)*(x2**2+z2**2)))))
     1.ge. ang) then
          j = j + 1
          x(j) = x(i)
          z(j) = z(i)
        endif    ! if (ang0 .ge. ang) then
    1 continue    ! do 1 i = 2 , n - 1
      j = j + 1
      x(j) = x(n)
      z(j) = z(n)
      n =  j
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellvsrt(n,it,ix,nx,x,z,v,iord,work)
c  sort the xv, zv, vel
      dimension it(1),ix(1),nx(1),x(1),z(1),v(1)
      do 1 i = 1 , n
        j1 = ix(i) + 1
        j2 = ix(i) + nx(i)
        do 2 j = ix(i)+1 , ix(i)+nx(i)
          if(it(min(j+1,j2)) .ne. it(j1) .or. j .eq. j2) then
            nj = j - j1 + 1
            call cellsort(nj,x(j1),iord)
            call cellord(nj,it(j1),iord,work)
            call cellord(nj,x(j1),iord,work)
            call cellord(nj,z(j1),iord,work)
            call cellord(nj,v(j1),iord,work)
            j1 = j
          endif
    2   continue
    1 continue

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellord(n,x,i,y)
c  order a set of points according to an index i
      dimension x(1),i(1),y(1)
      do 1 j = 1 , n
        y(j) = x(j)
    1 continue
      do 2 j = 1 , n
        x(j) = y(i(j))
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellsort(n,x,i)
c  sort x in icreasing order
      dimension x(1),i(1)
      do 1 j  =  1 , n
        i(j) = j
    1 continue
      if (n .le. 1) return
      l = n / 2 + 1
      i1 = n
    2 continue
        if (l .gt. 1) then
          l = l - 1
          i2 = i(l)
          x0 = x(i2)
        else
          i2 = i(i1)
          x0 = x(i2)
          i(i1) = i(1)
          i1 = i1 - 1
          if (i1 .eq. 1) then
            i(1) = i2
            return
          endif
        endif
        i0 = l
        j = l + l
    3   continue
        if (j .le. i1) then
          if (j .lt. i1) then
            if (x(i(j)) .lt. x(i(j+1))) j = j + 1
          endif
          if (x0 .lt. x(i(j))) then
            i(i0) = i(j)
            i0 = j
            j = j + j
          else
            j = i1 + 1
          endif
        goto 3
        endif
        i(i0) = i2
      goto 2
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellvmix(nx,nz,nxmix,nzmix,vg,vmix,*)
c  mix velocity model over nxmix,nzmix points
      dimension vg(nz,nx),vmix(1)
      if (nxmix .lt. 1 .or. nzmix .lt. 1) return
      nxmix2 = nxmix / 2
      nzmix2 = nzmix / 2
      xscl = 1. / max(1,nxmix)
      zscl = 1. / max(1,nzmix)
c  mix over z
      do 1 ix = 1 , nx
        do 2 iz = 1 , nz
          vmix(iz) = vg(iz,ix) * zscl
    2   continue
        do 3 iz = 1+nzmix2 , nz-nzmix2
          vg(iz,ix) = 0.
    3   continue
        do 4 izmix = -nzmix2 , nzmix2
          do 5 iz = 1+nzmix2 , nz-nzmix2
            vg(iz,ix) = vg(iz,ix) + vmix(iz+izmix)
    5     continue
    4   continue
    1 continue
c  mix over x
      do 6 iz = 1 , nz
        do 7 ix = 1 , nx
          vmix(ix) = vg(iz,ix) * xscl
    7   continue
        do 8 ix = 1+nxmix2 , nx-nxmix2
          vg(iz,ix) = 0.
    8   continue
        do 9 ixmix = -nxmix2 , nxmix2
          do 10 ix = 1+nxmix2 , nx-nxmix2
            vg(iz,ix) = vg(iz,ix) + vmix(ix+ixmix)
   10     continue
    9   continue
    6 continue
      return
  999 continue
      return 1
      end

c&&&
c  the following routines check various parts of the model 
c  to make sure they are resonable

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchku(nl1,ips1,ns,ibs,nc,isc,nsc,ics,ierr,*)
c  check to make sure each segment has been used twice
      dimension ips1(1),ibs(1),isc(1),nsc(1),ics(1)
      call cell_get_lpr(lpr)
      do 1 is = 1 , ns
        iused = 0
        ibs(is) = abs(ibs(is))
        do 2 ic = 1 , nc
          do 3 isc0 = isc(ic)+1 , isc(ic)+nsc(ic)
            if (abs(ics(isc0)).eq.is) iused = iused + 1
    3     continue
    2   continue
        do 4 il1 = 1 , nl1
          if (is .eq. ips1(il1)) iused = iused + 1
    4   continue
        if (iused .eq. 2) goto 1
        write(lpr,'(/,'' is='',i3,'' ibs='',i3,'' iused='',i3)')
     1is,ibs(is),iused
        do 5 ic = 1 , nc
          do 6 isc0 = isc(ic)+1 , isc(ic)+nsc(ic)
            if (abs(ics(isc0)).eq.is) write(lpr,'('' ic='',i3
     1,'' isc='',i3,'' ics='',i3)')ic,isc0-isc(ic),ics(ic)
    6     continue
    5   continue
        ierr = 13
    1 continue    ! do 1 is = 1 , ns
      if (ierr .ne. 0) goto 999
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchke(tol,n,ix,nx,x,z,xmin,xmax,zmin,zmax,ierr,*)
      dimension ix(1),nx(1),x(1),z(1),xmin(1),xmax(1),zmin(1),zmax(1)
      logical celllens,cellsame
      do 1 i = 1 , n
        if (celllens(tol,i,ix,nx,x,z)) goto 1
        do 2 iend = 1 , 2
          if (iend .eq. 1) then
            ix1 = ix(i) + 1
          else    ! if (iend .eq. 1) then
            ix1 = ix(i) + nx(i)
          endif    ! if (iend .eq. 1) then
          do 3 j = 1 , n
            if (i .eq. j .or. celllens(tol,j,ix,nx,x,z)
     1 .or. xmin(i) .gt. xmax(j) .or. xmax(i) .lt. xmin(j)
     1 .or. zmin(i) .gt. zmax(j) .or. zmax(i) .lt. zmin(j)) goto 3
            do 4  jx1 = ix(j)+1 , ix(j)+nx(j)
              if (cellsame(tol,x(ix1),z(ix1),x(jx1),z(jx1))) goto 2
    4       continue    ! do 4 jx1 = ix(j)+1 , ix(j)+nx(j)
    3     continue    ! do 3  j = 1 , n
      call cell_get_lpr(lpr)
      write(lpr,'('' could not find con for end '',i5,'' bnd'',i5
     1,'' ix='',i3,'' x='',f8.2,'' z='',f8.2)')
     1iend,i,ix1,x(ix1),z(ix1)
        ierr = 4
    2   continue    ! do 2 iend = 1 , 2
    1 continue    ! do 1 i = 1 , n
      if (ierr .ne. 0) goto 999
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchkc(nl1,ips1,ns,icon,acon,ierr)
c  check connections
      dimension ips1(1),icon(2,2,1),acon(2,2,1)
      call cell_get_lpr(lpr)

c  make sure all connections are there
      do 1 is = 1 , ns
        do 2 i1 = 1 , 2
          do 3 i2 = 1 , 2
            js = abs(icon(i1,i2,is))
            jend = 1 + (1 - sign(1,icon(i1,i2,is))) / 2
            jdir = mod(i1,2) + 1
            if (is .ne. abs(icon(jdir,jend,js))) then
              write(lpr,'('' could not find con for seg '',i5
     1,'' js='',i5,'' ic='',4(1x,i5),'' ac='',4(1x,f7.1))')is,js
     1,icon(1,1,js),icon(2,1,js),icon(1,2,js),icon(2,2,js)
     1,acon(1,1,js),acon(2,1,js),acon(1,2,js),acon(2,2,js)
              ierr = 11
              return
            endif    ! if (is .ne. abs(icon(jdir,jend,js))) then
    3     continue    ! do 3 i2 = 1 , 2
    2   continue    ! do 2 i1 = 1 , 2
    1 continue    ! do 1 is = 1 , ns

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchkp(nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,nc,imc,ixc,nxc,xc,zc,work,ierr1,*)
c  check each cell and determine if it has a cell pointer
c  check each cell pointer and make sure it has a velocity
      ierr1 = 0

c  check each cell and determine if it has a cell pointer
      call cellchp1(ncv,icv,xcv,zcv,nc,ixc,nxc,xc,zc,ierr,*999)
      ierr1 = max(ierr1,ierr)

c  check each cell and determine if it has more than one cell pointer
      call cellchp2(ncv,icv,xcv,zcv,nc,ixc,nxc,xc,zc,work
     1,ierr,*999)
      ierr1 = max(ierr1,ierr)

c  check each cell pointer and determine if it is in a cell
      call cellchp3(ncv,icv,xcv,zcv,nc,ixc,nxc,xc,zc,ierr,*999)
      ierr1 = max(ierr1,ierr)

c  check each cell pointer and determine if it has a velocity
      call cellchp4(ncv,icv,xcv,zcv,nv,imv,ierr,*999)
      ierr1 = max(ierr1,ierr)

c  check each velocity and determine if it has a cell pointer
      call cellchp5(ncv,icv,nv,imv,itv,ierr,*999)
      ierr1 = max(ierr1,ierr)

      if (ierr1 .ne. 0) return 1

      return
  999 continue
      call cell_get_lpr(lpr)
      if (lpr .ge. 0) write(lpr,'('' error in cellchkp'')')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchp1(ncv,icv,xcv,zcv,nc,ixc,nxc,xc,zc,ierr,*)
c  check each cell and determine if it has a cell pointer
      dimension icv(1),xcv(1),zcv(1),ixc(1),nxc(1),xc(1),zc(1)
      call cell_get_lpr(lpr)
      ierr = 0
      do 1 ic = 1 , nc
        i1 = ixc(ic) + 1
        call cellfncv(jcv,ncv,icv,xcv,zcv,nxc(ic),xc(i1),zc(i1),*999)
        if (jcv .eq. 0) then
          call cellpnt0(x0,z0,nxc(ic),xc(i1),zc(i1),ins,*999)
          if (lpr .ge. 0) write(lpr,'('' cell #'',i5
     1,'' does not have a cell pointer'','' use x='',f10.2
     1,'' z='',f10.2)')ic,x0,z0
          ierr = 1
        endif
    1 continue
      return
  999 continue
      print'('' error in cellchp1 could not find a cell pointer''
     1,'' for every cell lpr='',i5,'' ierr='',i5)',lpr,ierr
      if (lpr .ge. 0) write(lpr,'('' error in cellchp1'')')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchp2(ncv,icv,xcv,zcv,nc,ixc,nxc,xc,zc,ins,ierr,*)
c  check each cell and determine if it has a cell pointer
      dimension icv(1),xcv(1),zcv(1),ixc(1),nxc(1),xc(1),zc(1),ins(1)
      call cell_get_lpr(lpr)
      ierr = 0
      do 1 ic = 1 , nc
        i1 = ixc(ic) + 1
        nins = 0
        do 2 jcv = 1 , ncv
          call cellfncv(kcv,1,icv(jcv),xcv(jcv),zcv(jcv)
     1,nxc(ic),xc(i1),zc(i1),*999)
          if (kcv .ne. 0) then
            nins = nins + 1
            ins(nins) = jcv
          endif
    2   continue
        if (nins .gt. 1) then
          if (lpr .ge. 0) write(lpr,'('' cell #'',i5
     1,'' has too many pointers nins='',i5)')ic,nins
          if (lpr .ge. 0) write(lpr,'('' pointer # '',i5
     1,'' icv='',i5,'' x='',f10.2,'' z='',f10.2)')
     1(ins(j),icv(ins(j)),xcv(ins(j)),zcv(ins(j)),j=1,nins)
          ierr = 2
        endif
    1 continue
      return
  999 continue
      if (lpr .ge. 0) write(lpr,'('' error in cellchp2'')')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchp3(ncv,icv,xcv,zcv,nc,ixc,nxc,xc,zc,ierr,*)
c  check each cell pointer and make sure it is inside a cell
      dimension icv(1),xcv(1),zcv(1),ixc(1),nxc(1),xc(1),zc(1)
      call cell_get_lpr(lpr)
      ierr = 0
      do 1 jcv = 1 , ncv
        do 2 ic = 1 , nc
        i1 = ixc(ic) + 1
        call cellfncv(kcv,1,icv(jcv),xcv(jcv),zcv(jcv)
     1,nxc(ic),xc(i1),zc(i1),*999)
        if (kcv .ne. 0) goto 1
    2   continue
        if (lpr .ge. 0) write(lpr,'('' pointer # '',i5
     1,'' is not in any cell icv='',i5,'' x='',f10.2,'' z='',f10.2)')
     1jcv,icv(jcv),xcv(jcv),zcv(jcv)
        ierr = 3
    1 continue
      return
  999 continue
      if (lpr .ge. 0) write(lpr,'('' error in cellchp3'')')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchp4(ncv,icv,xcv,zcv,nv,imv,ierr,*)
c  check each cell pointer and make sure it has a velocity
      dimension icv(1),xcv(1),zcv(1),imv(1)
      call cell_get_lpr(lpr)
      ierr = 0
      do 1 jcv = 1 , ncv
        call cellfnmv(jmv,icv(jcv),nv,imv)
        if (jmv .eq. 0) then
        if (lpr .ge. 0) write(lpr,'('' pointer # '',i5
     1,'' does not have a velocity icv='',i5,'' x='',f10.2
     1,'' z='',f10.2)')jcv,icv(jcv),xcv(jcv),zcv(jcv)
          ierr = 4
        endif
    1 continue
      return
  999 continue
      if (lpr .ge. 0) write(lpr,'('' error in cellchp4'')')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchp5(ncv,icv,nv,imv,itv,ierr,*)
c  check each velocity and make sure it has a cell pointer
      dimension icv(1),xcv(1),zcv(1),imv(1),itv(1)
      call cell_get_lpr(lpr)
      ierr = 0
      do 1 iv = 1 , nv
        do 2 jcv = 1 , ncv
          call cellfnmv(jmv,icv(jcv),1,imv(iv))
    3     continue
          if (jmv .ne. 0) goto 1
    2   continue
        if (lpr .ge. 0) write(lpr,'('' velocity # '',i5
     1,'' does not have a pointer imv='',i5,'' itv='',i5)')
     1iv,imv(iv),itv(iv)
        ierr = 5
    1 continue
      return
  999 continue
      if (lpr .ge. 0) write(lpr,'('' error in cellchp5'')')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellchp6(nv,imv,nc,imc,ierr,*)
c  check that each cell has a velocity assigned
      dimension imv(1),imc(1)
      ierr = 0
      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      do 1 ic = 1 , nc
        do 2 iv = 1 , nv
          if (imv(iv) .eq. imc(ic)) goto 1
    2   continue
        if (lpr .ge. 0) write(lpr,'('' cell # '',i5
     1,'' does not have a velocity imc='',i5)')ic,imc(ic)
        ierr = 6
    1 continue
      if (ierr .ne. 0) goto 999
      return
  999 continue
      if (lpr .ge. 0) then
      write(lpr,'('' error in cellchp6'')')
      write(lpr,'('' nc='',i8,'' nv='',i8)')nc,nv
      write(lpr,'('' ic='',i8,'' imc='',i8)')(ic,imc(ic),ic=1,nc)
      write(lpr,'('' iv='',i8,'' imv='',i8)')(iv,imv(iv),iv=1,nv)
      endif
      return 1
      end

c  the following routines find properties from pointers

      subroutine cellfncn(itype,ncv,icv,xcv,zcv,nv,imv
     1,nc,imc,ixc,nxc,xc,zc,ierr)
c  determine which velocity type is used in which cell
c  itype=0 find icv value itype=1 find velocity value
      dimension icv(1),imc(1),ixc(1),nxc(1),xc(1),zc(1)
      ierr = 0
      do 1 ic =  1 , nc
        imc(ic) = -9999
        call cellfncm(jcv,jv,ncv,icv,xcv,zcv,nv,imv
     1,nxc(ic),xc(ixc(ic)+1),zc(ixc(ic)+1),*2)
        if (itype .eq. 0) then
          imc(ic) = icv(jcv)
        else
          imc(ic) = jv
        endif
    2   continue
        if (imc(ic) .eq. -9999) ierr = ierr + 1
    1 continue
      if (ierr .ne. 0) goto 999
      return
  999 continue
      print'('' error in cellfncn could not find a velocity type''
     1,'' for every cell pointer ierr='',i5,'' type='',i5)',ierr,itype
      ierr = 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellfncm(jcv,jmv,ncv,icv,xcv,zcv,nv,imv,nxc,xc,zc,*)
c  find the pointer and velocity number within this cell
c icv(jcv) = imv(jmv)
      dimension icv(1),imv(1)
      jmv = 0
      jcv = 0
      call cellfncv(jcv,ncv,icv,xcv,zcv,nxc,xc,zc,*999)
      if (jcv .eq. 0) goto 999
      call cellfnmv(jmv,icv(jcv),nv,imv)
      if (jmv .eq. 0) goto 999
c      print*,' jmv=',jmv,' jcv=',jcv
      return
  999 continue
      print'('' error in cellfncm jcv='',i5,'' jmv='',i5)',jcv,jmv
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellfncv(jcv,ncv,icv,xcv,zcv,nxc,xc,zc,*)
c  find the velocity pointer # jcv within this cell
      dimension icv(1),xcv(1),zcv(1),xc(1),zc(1)
      jcv = 0
      do 1 i = 1 , ncv
        call cellpoly(xcv(i),zcv(i),nxc,xc,zc,inside,*999)
        jcv = i
        if (inside .eq. 1) return
    1 continue
      jcv = 0
      return
  999 continue
      print'('' error in cellfncv'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellfnmv(jmv,icv,nv,imv)
c  determine what material type this is
      dimension imv(1)
      do 1 i = 1 , nv
        jmv = i
        if (icv .eq. imv(i)) return
    1 continue
      jmv = 0
      return
      end

c&&&
c  the following prints various info

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpseg(ns,ibs,ixs,nxs,ix,x,z,lu,title,inc)
c print segments
      dimension ibs(1),ixs(1),nxs(1),ix(1),x(1),z(1)
      data icall/0/
      character title*(*)
      icall = icall + 1
      if (title(1:1) .ne. ' ')print'(1x,a20,'' icall='',i5,'' lu='',i2)'
     1,title(1:20),icall,lu
      do 1 i = 1 , ns
        write(lu,'(1x,f10.2,1x,f10.2,1x,i5)')(x(j),z(j),i
     1,j=ix(ibs(i))+ixs(i)+1,ix(ibs(i))+ixs(i)+nxs(i))
    1 continue
      if (inc .ne. 0) then
        close(lu)
        lu = lu + inc
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpplt(n,ix,nx,x,z,lu,title,inc)
      dimension ix(1),nx(1),x(1),z(1)
c  write a doubly pointed array to a plot file
      data icall/0/
      character title*(*)
      icall = icall + 1
      if (title(1:1) .ne. ' ') then
        nl = 20
        do 1 il = 1 , 19
          if (title(il:il+1) .eq. 'xx') then
            nl = il - 1
            goto 2
          endif
    1   continue
    2   continue
        print'(1x,a20,'' lu='',i2,'' icall='',i5,'' n='',i5)'
     1,title(1:nl),lu,icall,n
      endif
      do 3 i = 1 , n
        icall = icall + 1
        jj=i
        if(n.eq.1)jj=icall
        write(lu,'(1x,f10.2,1x,f10.2,1x,i5)')
     1(x(j),z(j),jj,j=ix(i)+1,ix(i)+nx(i))
    3 continue
      if (inc .ne. 0) then
        close(lu)
        lu = lu + inc
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpbvc(title
     1,xmin,xmax,zmin,zmax,nb,itb,jtb,ixb,nxb,xb,zb
     1,ncv,icv,xcv,zcv,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,mc,nc,vc,gc,imc,ixc,nxc,xc,zc)
c  print boundary velocity and cell info
      character *(*) title
      ierr = 0
      call celltol(tol,xmin,xmax)
    1 continue
      call cellprnx(title,ierr,tol,xmin,xmax,zmin,zmax)
      call cellprnb(title,nb,ixb,nxb,xb,zb)
      call cellprnv(title
     1,ncv,icv,xcv,zcv,nv,imv,itv,ixv,nxv,xv,zv,vel)
      call cellprnc(title,mc,nc,vc,gc,imc,ixc,nxc,xc,zc)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellprnv(title
     1,ncv,icv,xcv,zcv,nv,imv,itv,ixv,nxv,xv,zv,vel)
c  print velocity info
      character title*(*)
      dimension icv(1),xcv(1),zcv(1)
     1,imv(1),itv(1),ixv(1),nxv(1),xv(1),zv(1),vel(1)
      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      if (ipr .ne. 4) return
      write(lpr,'(/,a,/,''czzz velocity ncv='',i5
     1,/,'' xcv  zcv icv ix'',200(/,2(1x,f12.4),2(1x,i5)))')
     1title,ncv,(xcv(i),zcv(i),icv(i),i,i=1,min(200,ncv))
      write(lpr,'(/,'' nv='',i5)')nv
      do 1 i = 1 , nv
        write(lpr,'(/,'' iv='',i3,'' ixv='',i5,'' nxv='',i5
     1,/,'' x  z  imv itv v i''
     1,200(/,2(1x,f12.4),2(1x,i5),1x,f12.4,1x,i5))')
     1i,ixv(i),nxv(i),(xv(j),zv(j),imv(i),itv(j),vel(j),j
     1,j=ixv(i)+1,ixv(i)+min(200,nxv(i)))
    1 continue
      write(lpr,'(''czzz end velocity'')')
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellprnx(title,ierr,tol,xmin,xmax,zmin,zmax)
c  print velocity info
      character title*(*)
      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      if (ipr .ne. 4) return
      write(lpr,'(/,a,/,''czzz ierr='',i5,'' tol='',f8.4
     1,/,'' xmin='',f10.2,'' xmax='',f10.2
     1,/,'' zmin='',f10.2,'' zmax='',f10.2,/,''czzz end xmin'')')
     1title,ierr,tol,xmin,xmax,zmin,zmax
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellprnb(title,n,ix,nx,x,z)
      implicit none
      character title*(*)
      integer n,ix(1),nx(1)
      real x(1),z(1)
      integer ipr,lpr,i,j
      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      if (ipr .ne. 4) return
      write(lpr,'(a,/,''czzz # of pts='',i8,''# of boundarys='',i5)')
     1title,ix(n)+nx(n),n
      do 1 i = 1 , n
        write(lpr,'(/,'' boundary '',i8,'' ix='',i8,'' nx='',i8
     1,/,'' ix  x  z'',200(/,2(1x,f12.4),1x,i8))')i,ix(i),nx(i)
     1,(x(j),z(j),j,j=ix(i)+1,ix(i)+min(200,nx(i)))
    1 continue
      write(lpr,'(''czzz end points'')')
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellprnc(title,ivpr,nc,vc,gc,imc,ixc,nxc,xc,zc)
      implicit none
      integer ivpr,nc,imc(1),ixc(1),nxc(1)
      real vc(1),gc(1),xc(1),zc(1)
      character title*(*)
      integer ipr,lpr,ic,i,imc0
      real vc0,gc0

      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      if (ipr .ne. 4) return
      write(lpr,'(a,/,''czzz # of pts='',i8,'' # of cells='',i5)')
     1title,ixc(nc)+nxc(nc),nc
      do 1 ic = 1 , nc
        if (ivpr .eq. 1) then
          imc0 = imc(ic)
          vc0 = vc(ic)
          gc0 = gc(ic)
        else
          imc0 = 0
          vc0 = 0
          gc0 = 0
        endif
        write(lpr,'('' cell='',i5,'' imc=''i5,'' vc='',f12.4
     1,'' gc='',f10.6,'' ixc='',i5,'' nxc='',i10
     1,/,''  lxc   xc   zc '',200(/,2(1x,f12.4),2(1x,i8)))')
     1ic,imc0,vc0,gc0,ixc(ic),nxc(ic),(xc(i),zc(i),ic,i-ixc(ic)
     1,i=ixc(ic)+1,ixc(ic)+min(200,nxc(ic)))
    1 continue
      write(lpr,'(''czzz end cell'',a)')title
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellpcon(ns,icon,acon,nl1,ips1,ips2,ibs,ixs,nxs
     1,n,ix,nx,x,z,ierr)
c  print segment connections
      dimension icon(2,2,1),acon(2,2,1),ips1(1),ips2(1)
     1,ibs(1),ixs(1),nxs(1),ix(1),nx(1),x(1),z(1)
      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      if (ipr .ne. 4 .and. ierr .eq. 0) return
      rd = 90. / asin(1.)
      write(lpr,'(/,'' czzz list of seg connections ierr='',i5
     1,'' ns='',i5,/
     1,'' seg con11 con12 con21 con22   ac11    ac12    ac21    ac22''
     1,200(/,5(1x,i5),4(1x,f7.1)))')
     1ierr,ns,(i,icon(1,1,i),icon(2,1,i),icon(1,2,i),icon(2,2,i)
     1,acon(1,1,i)*rd,acon(2,1,i)*rd,acon(1,2,i)*rd,acon(2,2,i)*rd
     1,i=1,min(200,ns))
c create a list of segments whose end points are both on the model boundary
      write(lpr,'(/,'' # of seg on model rim='',i5
     1,/,''    il    is    ib   xseg1      xseg2      zseg1      zseg2''
     1200(/,3(1x,i5),4(1x,f10.2)))')
     1nl1,(i,ips1(i),ibs(ips1(i))
     1,x(ix(ibs(ips1(i)))+ixs(ips1(i))+1)
     1,x(ix(ibs(ips1(i)))+ixs(ips1(i))+nxs(ips1(i)))
     1,z(ix(ibs(ips1(i)))+ixs(ips1(i))+1)
     1,z(ix(ibs(ips1(i)))+ixs(ips1(i))+nxs(ips1(i)))
     1,i=1,min(200,nl1))
      write(lpr,*)'czzz con segments'
      call cellpseg(ns,ibs,ixs,nxs,ix,x,z,lpr,' ',0)
      write(lpr,*)'czzz end con segments'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellprns(title,nc,isc,nsc,ics)
      character title*(*)
      dimension isc(1),nsc(1),ics(1)
      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      if (ipr .ne. 4) return
      write(lpr,'(/,a,/,''czzz nc='',i5)')title,nc
      do 1 ic = 1 , nc
        write(lpr,'('' cell='',i5,'' isc='',i5,'' nsc='',i5
     1,/,'' lsc  segment'',200(/,2(1x,i5)))')
     1ic,isc(ic),nsc(ic),(i-isc(ic),ics(i)
     1,i=isc(ic)+1,isc(ic)+min(200,nsc(ic)))
    1 continue
      write(lpr,'(''czzz end prns'')')
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cellprnl(title
     1,ns,ibs,ixs,nxs,nln,iln,nb,ixb,nxb,xb,zb)
      character title*(*)
      dimension ibs(1),ixs(1),nxs(1),iln(1),ixb(1),nxb(1),xb(1),zb(1)
      call cell_get_ipr(ipr)
      call cell_get_lpr(lpr)
      if (ipr .ne. 4) return
      write(lpr,'(/,a,/,''czzz # of pts=''
     1,i5,'' # of boundarys='',i5,'' ns='',i5,'' nln='',i5)')
     1title,ixb(nb)+nxb(nb),nb,ns,nln
      do 1 iis = 1 , ns
        ii = ibs(iis)
        write(lpr,'('' segment='',i3,'' ib='',i5,'' ixb='',i5
     1,'' nxb='',i5,/,'' ibs='',i5,'' ixs='',i5,'' nxs='',i5
     1,/,'' ix  x  z'',200(/,1x,i5,2(1x,f10.2)))')
     1iis,ii,ixb(ii),nxb(ii),ibs(iis)
     1,ixs(iis),nxs(iis),(ix,xb(ix)
     1,zb(ix),ix=ixb(ii)+ixs(iis)+1
     1,ixb(ii)+ixs(iis)+min(200,nxs(iis)))
    1 continue
      do 2 ijln = 1 , nln
        ii = iln(ijln)
        write(lpr,'('' lens='',i3,'' ib='',i5,'' ixb='',i5
     1,'' nxb='',i5,/,'' iln='',i5
     1,/,'' ix  x  z'',200(/,1x,i5,2(1x,f10.2)))')
     1ijln,ii,ixb(ii),nxb(ii),iln(ijln)
     1,(ix,xb(ixb(ii)+ix)
     1,zb(ixb(ii)+ix),ix=1,min(200,nxb(ii)))
    2 continue
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cell_get_lpr(lpr0)
c  put and get logical unit number for error infor
      implicit none
      integer lpr,lpr0
      save lpr
      data lpr/6/
      lpr0 = lpr
      return
      entry cell_put_lpr(lpr0)
      lpr = lpr0
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cell_get_ipr(ipr0)
c  put and get print flag
      implicit none
      integer ipr,ipr0
      save ipr
      data ipr/-1/
      ipr0 = ipr
      return
      entry cell_put_ipr(ipr0)
      ipr = ipr0
      return
      end


