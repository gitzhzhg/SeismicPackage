!<CPS_v1 type="PROGRAM"/>
!!------------------------------ color2cgm.f90 -------------------------------!!
!!------------------------------ color2cgm.f90 -------------------------------!!
!!------------------------------ color2cgm.f90 -------------------------------!!


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
!                       c p s   p r o g r a m 
!
! name       : color2cgm
! category   : stand-alone
! written    : 2002-03-14   by: Karen Goodger
! revised    : 2002-04-15   by: Karen Goodger
! maturity   : production
! purpose    : Read output from color program and make cgm calls.
! portability: no known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          general description
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     date        author       description
!     ----        ------       -----------
!  2. 2002-04-15  Goodger      Fix circle routine to use radians rather than
!                              degrees.
!  1. 2002-03-14  Goodger      Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         portability limitations
!
! No known limitations
!
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     special compiling requirements
!
! Must link with the library libcgm.a
!
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  algorithm description for developers
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           programming notes
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!--------------------------- private module -------------------------------!!
!!--------------------------- private module -------------------------------!!
!!--------------------------- private module -------------------------------!!



!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!


      module color2cgm_module
      use ameq_module
      use cgm_module
      use colrplot_module
      use getlun_module
      use getsys_module
      use named_constants_module
      use putsys_module
      use string_module
      implicit none

      contains

      subroutine color2cgm_main

      integer itemp(6)
      real flib(10000)
      character*1 clib(40000)
!
      character(len=160) :: ctext,ianode,ibnode,isnode,itxt
      character(len=8)   :: jn,lrrl
      character cuser*10,cdevnode*11,cmd*160,card*80,ctmp*8
      character ctmp2*8,queue*10,host*8,tmpcrd*80,command*8,pnode*8
      character flag*4, ipltr*4
      logical there
      logical around
      integer in, cc, us, au, cp, jm, ji, vf, nf, ro, ff
      integer :: bgcols(1),copies,iang,k,lun_ccname,vacols(1)
      character(len=1)  :: letter
      character(len=4)  :: trap
      character(len=8)  :: device,ccname='%CCGMCA',ccopies,medium
      character(len=8)  :: qopt,quality,tmpfil,tmpfil2,type
      character(len=16) :: jobname,popt
      character(len=32) :: cgmfile
      character(len=80) :: cdum1,cdum2
      character(len=160):: workdir
      character(len=500):: cdir
      character iuser*8, node*8, ivecf*8
      integer itrap, itmpfil, itmpfil2, i, j, mostat
      integer mrstat, ixmax, ires, nc, nxys, num, ib, ic
      integer :: color2cgm_tone_count,modes(2),nsamp
      integer :: pen
      real :: ampscale,angle,basescale,ct,datlen,glav,numdots
      real :: rad,radius,size,tinch
      real :: x,x1,x2,xmax,xmaxadd,xmmax,xmmin,xpmax,xpmin,y,y1,y2,yend,yo
      real :: xpts(722),ypts(722)
      logical almost_equal,first_time
!
      integer, pointer, dimension(:) :: id
      integer, pointer, dimension(:) :: lib
      integer ilflmx, ilfl, wbyts, libsz,istat,iunit
      character ilf*8
!
      data ianode/'/NODE=POESP2 /CHARGE=            /PLOTTER=1     '/
      data trap/'TRAP'/, tmpfil/'TMPFIL'/, tmpfil2/'TMPFIL2'/
      data first_time/.true./
      data ilflmx/8/
!
      call getlun(lun_ccname,istat)
      if(istat.ne.0)then
        print*,'COLOR2CGM-->Unable to get unit number for control card file'
        stop
      endif
      call getlun(iunit,istat)
      if(istat.ne.0)then
         print*,'COLOR2CGM-->Unable to get unit number for colrplot calls'
         stop
      endif
      print*,'1    ******************************** COLOR2CGM **************'
  50  call colrplot_init ('OLD', mrstat,iunit)
!
      if (first_time) then
        first_time = .false.
!
        lib => colrplot_libptr ()
        id  => colrplot_idptr ()
        wbyts = bit_size (lib(1)) / 8
        libsz = colrplot_libsz ()
!
        in = id(2) ! plot node: req'S 8 CHARACTERS
        cc = in + ( 8 + wbyts - 1) / wbyts ! charge code: req'S 12 CHARS
        us = cc + (12 + wbyts - 1) / wbyts ! user: req'S 8 CHARS
        au = us + ( 8 + wbyts - 1) / wbyts ! author: req'S 8 CHARS
        cp = au + ( 8 + wbyts - 1) / wbyts ! # of copies: req'S 1 WORD
        jm = cp + 1 ! jobname: req'S 8 CHARS
        ji = jm + ( 8 + wbyts - 1) / wbyts ! jobid: req'S 8 CHARS
        vf = ji + ( 8 + wbyts - 1) / wbyts ! vect file: req'S 8 CHARS
        nf = vf + ( 8 + wbyts - 1) / wbyts ! node from: req'S 8 CHARS
        ro = nf + ( 8 + wbyts - 1) / wbyts ! rotate: req'S 1 WORD
        ff = ro + 1 ! fold flag: req'S 3 CHARS
!
!       fi = (16 + wbyts - 1) / wbyts ! index into temp
!
      end if
!
      call colrplot_lfl (iunit,ilf, ilflmx, ilfl)
      print*,' LOOKING FOR FILE ',ilf(1:ilfl)
!
      there = mrstat .eq. 0
!
      if (.not.there) go to 8000
      print*,' PLOTTING FILE ',ilf
      ibnode=ianode
      do i=1,libsz
        lib(i)=0
      end do

      open(lun_ccname,file=ccname,status='old',iostat=istat,form='unformatted')
      if(istat.ne.0)then
        print*,' COLOR2CGM-->Unable to open file ',trim(ccname)
        stop
      endif
      read(lun_ccname)jobname,xmax,lrrl,device,quality,copies,medium,yo,yend,&
                      nsamp,ct,tinch,glav
      letter=ccname(7:7)
      jobname= trim(jobname) // '_' // letter
      k=ichar(letter)
      k=k+1
      letter=char(k)
      ccname(7:7)=letter
      close(lun_ccname)

      xmax=xmax+3.0
      call cgm_gopks(11)         !Open GKS with logical unit 11 as error file
      cgmfile = trim(jobname) // '.cgm'
      type    = 'CGMPIP'
      call cgm_gopwk(1, cgmfile, type)  !Open workstation 1 
      call cgm_gacwk(1)                        !Activate workstation 1

      xmaxadd=0.0
      if(lrrl.eq.'RL')xmaxadd=12.0
        call cgm_gswkvp(1, 0.0, xmax+xmaxadd, 0.0, 42.0)
        call cgm_gswn(  1, 0.0, xmax+xmaxadd, 0.0, 42.0)
        call cgm_gsvp(  1, 0.0,  1.0, 0.0,  1.0)
        call cgm_gselnt(1)
!               using either 0,0,0 or 1,1,1  comes out black
        call cgm_gscr(1,9,1.0,1.0,1.0)  !Set color 9 to white
        call cgm_gscr(1,1,1.0,1.0,1.0)  !Set color 1 to black
        call cgm_gscr(1,6,1.0,0.0,0.0)  !Set color 6 to red
        call cgm_gsfais(1)              !Set fill type to solid
        call cgm_gsfaci(1)              !Set fill color to black
        call cgm_gstxfp(26,2)           !Try font 26, Times Roman
!
 100  call colrplot_read (iunit,mrstat)
      if(mrstat.ne.0)then
        call colrplot_fini (iunit,'DELETE')
        call cgm_gdawk(1)                        !Deactivate workstation 1
        call cgm_gclwk(1)                        !Close workstation 1
        call cgm_gclks                           !Close GKS 
!              Send cgm file to plotter
        select case(device)
          case('HP5000A')
            popt='5000A'
          case('HP')
            popt='650A'
        end select
        if(medium.eq.'FILM')then
          popt=trim(popt) // '_film'
        else if(medium.eq.'GLOSS')then
          popt=trim(popt) // '_gloss'
        endif
        select case(quality)
          case('PROD')
            qopt='normal'
          case('MAX')
            qopt='best'
          case('FAST')
            qopt='fast'
        end select
        call getsys_current_dir(workdir)
        call string_ii2cc(copies,ccopies)
        if(device.eq.'SAVECGM')then
          call getsys_netinfo(cdum1,cdum2,cdir)
          cmd='mv ' // trim(cgmfile) // ' ' // trim(cdir) // trim(cgmfile)
          print*,trim(cmd)
          call putsys_cmd(cmd,istat)
        else if (device(1:2).eq.'HP')then
          cmd='rsh poeppl02 "/u/poeppl02/zeh/conoco/zeh_submit -f ' // &
               trim(workdir) //  &
               trim(cgmfile) // ' -q ' // trim(qopt) // ' -p ' // &
               trim(popt) // ' -c ' // trim(ccopies) // '"'
          print*,trim(cmd)
          call putsys_cmd(cmd,istat)
        endif
        go to 50 
      endif
!
      call color2cgm_zro2blk (lib(1), 8)
      call colrplot_mvc (lib(1), 1, command(1:1), 1, 8)
      if (command(1:4) .eq. 'PLOT') then
        if (command(1:5) .ne. 'PLOTS') then
          command(5:5) = ' '
        endif
      endif
!
      if(command(1:7).eq.'CONPLOT')then
        call colrplot_mvc (lib(9),1,itxt(1:1),1,160)
!!        call conplot(itxt)
      else if(command(1:6).eq.'DEFRGB')then
        call colrplot_mvc (lib(id(3)),1,flib(id(3)),1,3*wbyts)
        call cgm_gscr(1,lib(id(2)) ,flib(id(3)) ,flib(id(4)) ,flib(id(5)) )
!                     color       red        green       blue
!!        call defrgb(lib(id(2)),flib(id(3)),flib(id(4)),flib(id(5)))
      else if(command(1:6).eq.'SYMBOL')then
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,3*wbyts)
        call colrplot_mvc (lib(id(5)),1,flib(id(5)),1,wbyts)
        call colrplot_mvc (lib(id(7)),1,ctext,1,lib(id(6)))
        x=flib(id(2))
        y=flib(id(3))
        size=flib(id(4))
        iang=nint(flib(id(5)))
        nc=lib(id(6))
        call cgm_gschh(size)
!           To compute x - take tangent of angle
!            Make negative depending on quadrant

        if(iang.eq.0)then
          call cgm_gschup(0.,1.)
        else if(iang.eq.20)then
          call cgm_gschup(-0.364,1.0)
        else if(iang.eq.60)then
          call cgm_gschup(-1.732,1.0)
        else if(iang.eq.90)then
          call cgm_gschup(-1.,0.)
        else if(iang.eq.200)then
          call cgm_gschup(0.364,-1.0)
        else if(iang.eq.270)then
          call cgm_gschup(1.0,0.0)
        else  ! assume 180
          call cgm_gschup(0.,-1.)
        endif
        call string_strip_unprintable(ctext)
       call string_replace_zeroes(ctext) 
        call cgm_gtx(x,y,ctext(1:nc))
        call cgm_gstxp(0)


      else if(command(1:6).eq.'SETTXT')then
        call colrplot_mvc (lib(id(6)),1,flib(id(6)),1,2*wbyts)
!                       hor align  vert align
        call cgm_gstxal(lib(id(4)),lib(id(5)))
!                      font      text path  hor align  ver align  xp
!!        call settxt(lib(id(2)),lib(id(3)),lib(id(4)),lib(id(5)),flib(id(6)),&
!           sp          mode
!!          flib(id(7)),lib(id(8)))
      else if(command(1:6).eq.'NUMBER')then  !not used
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,5*wbyts)
!!        call number(flib(id(2)),flib(id(3)),flib(id(4)),flib(id(5)),&
!!          flib(id(6)),lib(id(7)))
      else if(command(1:6).eq.'PENCLR')then  !not used
!!        call penclr(lib(id(2)),lib(id(3)))
      else if(command(1:6).eq.'NEWPEN')then  !not used
!!        call newpen(lib(id(2)))
      else if(command(1:6).eq.'COPIES')then  !not used
!!        call copies(lib(id(2)))
      else if(command(1:6).eq.'PIXPLT')then
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,4*wbyts)
!!        call colrplot_mvc (lib(id(9)),1,clib,1,lib(id(6))*lib(id(7)))
!                       x1          y1          x2           y2 
        call cgm_gscell(flib(id(2)),flib(id(3)),flib(id(4)),flib(id(5)),&
!                       nx         ny         array
                        lib(id(6)),lib(id(7)),lib(id(9):),1)
!                     x1          y1          x2          y2
!!        call pixplt(flib(id(2)),flib(id(3)),flib(id(4)),flib(id(5)),&
!           nx         ny         array
!!          lib(id(6)),lib(id(7)),clib,lib(id(8)))
      else if(command(1:5).eq.'PLOT ')then
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,2*wbyts)
        pen=lib(id(4))
        if(pen.eq.3)then
          x1=flib(id(2))
          y1=flib(id(3))
        else if(pen.eq.2)then
          xpts(1)=x1
          ypts(1)=y1
          xpts(2)=flib(id(2))
          ypts(2)=flib(id(3))
          call cgm_gpl( 2, xpts, ypts)
        endif
!                   x           y           pen 3=up, 2=down
!!        call plot(flib(id(2)),flib(id(3)),lib(id(4)))
      else if(command(1:5).eq.'PLOTS')then
        call colrplot_mvc (lib(id(2)),1,clib,1,8)
!!        call setup_jobname (clib)
!!        call plots
      else if(command(1:4).eq.'RECT')then 
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,4*wbyts)
        xpts(1)=flib(id(2))
        ypts(1)=flib(id(4))
        xpts(2)=flib(id(3))
        ypts(2)=flib(id(4))
        xpts(3)=flib(id(3))
        ypts(3)=flib(id(5))
        xpts(4)=flib(id(2))
        ypts(4)=flib(id(5))
        call cgm_gfa( 4, xpts, ypts) 
!                         x1     x2         y1          y2
!!        call rect(flib(id(2)),flib(id(3)),flib(id(4)),flib(id(5)),lib(id(6)))
      else if(command(1:6).eq.'SETFNT')then
        if(lib(id(2)).eq.0)then
          call cgm_gstxfp(1,2)
!!          pcnt=1.0
        else
          call cgm_gstxfp(26,2)
!!          pcnt=0.85
        endif
!!        call setfnt(lib(id(2)))
      else if(command(1:6).eq.'TONCLR')then
          call cgm_gsfaci(lib(id(2)))
      else if(command(1:6).eq.'TONFLG')then
!!        call tonflg(lib(id(2)))
      else if(command(1:5).eq.'DASHS')then
        call colrplot_mvc (lib(id(3)),1,flib(id(3)),1,abs(lib(id(2)))*wbyts)
!!        call dashs(flib(id(3)),lib(id(2)))
      else if(command(1:4).eq.'TONE')then
        if (lib(id(2)) .gt. 1) then
          print*,' Broke assumption on TONE patterns <= 1'
          stop ! broke assumption on tone patterns <= 1
        end if
        nxys = color2cgm_tone_count (lib(id(3):), lib(id(2)))
        if (2*nxys+4 .gt. libsz) then
          print*,' ID array too small'
          stop ! id array too small
        end if
        call colrplot_mvc (lib(id(4)),1,flib(id(4)),1,2*nxys*wbyts)
!                    numpoints xarray       yarray
        call cgm_gfa(lib(id(3)),flib(id(4):),flib(id(4)+nxys:))
!                    xarray      yarray            numpoints   num polygons
!!        call tone(flib(id(4):),flib(id(4)+nxys:),lib(id(3):),lib(id(2)))
      else if(command(1:6).eq.'LINEWT')then
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,wbyts)
          numdots=flib(id(2))/0.005
          call cgm_gslwsc(numdots)
!!        call linewt(flib(id(2)))
      else if(command(1:6).eq.'CIRCLE')then
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,4*wbyts)
        x=flib(id(2))
        y=abs(flib(id(3)))
        radius=flib(id(4))
        angle=0.0
        j=0
        do i=1,360
          angle=real(i)
          rad=angle*RADIANS_PER_DEGREE
          xpts(i)=sin(rad)*radius + x
          ypts(i)=cos(rad)*radius + y
        enddo
        call cgm_gfa(360,xpts,ypts)
!                     x           y           radius      width of outline
!!        call circle(flib(id(2)),flib(id(3)),flib(id(4)),lib(id(5)))
        if(flib(id(2)).lt.0.0.or.flib(id(3)).gt.39.5)then
          print*,' BAD CIRCLE CALL - X = ',flib(id(2)),' Y = ',flib(id(3))
          go to 5003
        endif
      else if(command(1:7).eq.'CCIRCLE')then  !not used
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,5*wbyts)
!!        call ccircle(flib(id(2)),flib(id(3)),flib(id(4)),flib(id(5)),&
!!          flib(id(6)),lib(id(7)))
      else if(command(1:7).eq.'DEFPENW')then  !not used
        call colrplot_mvc (lib(id(3)),1,flib(id(3)),1,wbyts)
!!        call defpenw(lib(id(2)),flib(id(3)),lib(id(4)))
      else if(command(1:6).eq.'OVRLAY')then
!!        call ovrlay(lib(id(2)))
      else if(command(1:6).eq.'PLSEIS')then
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,(5+lib(id(4)))*wbyts)
!    
        if(flib(id(3)).le.41.0.and.flib(id(3)).ge.0.0)then
!                             x           y         grp fac trace
           call cgm_pip_trace(flib(id(2)),yo,1,1.0,flib(id(7):),&
!                             nsamp
                              lib(id(4)),vacols,0,bgcols,0)
!                       x           y           tr          nsamp
!!          call plseis(flib(id(2)),flib(id(3)),flib(id(7)),lib(id(4)),&
!             inches      lav
!!            flib(id(5)),flib(id(6)))
        else
          print*,' BAD TRACE CALL X = ',flib(id(2)),' Y = ',flib(id(3))
          go to 5003
        endif
      else if(command(1:7).eq.'SETSEIS')then
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,5*wbyts)
        call cgm_gsln(1)                !set line type to solid
        call cgm_gslwsc(real(lib(id(9)))) !set the line width
        if(lrrl.eq.'LR')then
          call cgm_pip_trace_orientation(0.0,-1.0,1.0,0.0)
        else
          call cgm_pip_trace_orientation(0.0,1.0,-1.0,0.0)
        endif
        xpmin=flib(id(5))
        xpmax=flib(id(6))
        xmmin=flib(id(2))
        xmmax=flib(id(3))
        modes(1)=2
        modes(2)=1
        if(lrrl.eq.'LR')then
          almost_equal=ameq(xpmax,0.0,0.00001) 
        else
          almost_equal=ameq(xmmax,0.0,0.00001)
        endif         
        if(almost_equal)modes(1)=1
        if(lib(id(9)).eq.0)modes(2)=0
        call cgm_pip_trace_display_modes(modes,2) 
        call cgm_pip_trace_va_fill(xpmin,xpmax,xmmin,xmmax,0,1,9,0,0,0)
        ampscale=(ct*0.5)/(tinch*glav)
        datlen=abs(yend-yo)
        basescale=datlen/nsamp
        call cgm_pip_trace_scale_factors(basescale,ampscale)

!                      xmmin       xmmax       imfill     xpmin
!!        call setseis(flib(id(2)),flib(id(3)),lib(id(4)),flib(id(5)),&
!           xpmax       ipfill     1=lr,2=rl  ndots
!!          flib(id(6)),lib(id(7)),lib(id(8)),lib(id(9)))
      else if(command(1:6).eq.'CLRPLT')then !not used
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,2*wbyts)
        call colrplot_mvc (lib(id(5)),1,clib,1,lib(id(4)))
!!        call clrplt(flib(id(2)),flib(id(3)),clib,lib(id(4)))
      else if(command(1:4).eq.'AXIS')then
        call colrplot_mvc (lib(id(2)),1,flib(id(2)),1,7*wbyts)
        call colrplot_mvc (lib(id(9)),1,clib,1,lib(id(4)))
!!        call axis(flib(id(2)),flib(id(3)),clib,lib(id(4)),flib(id(5)),&
!!          flib(id(6)),flib(id(7)),flib(id(8)))
      else if(command(1:5).eq.'SETUP')then
      else
        if(command .ne. ' ') print 1111,command
 1111 format(' *** ERROR *** CALLING FOR ROUTINE ',a8)
      endif
!
      go to 100
!      
!
!
 5000 continue
      print 5001,ilf,mostat          
 5001 format(' ERROR OPENING FILE ',a8,' = ',i3)
 5003 continue
      print*,'COLOR2CGM-->ABORTED'
      stop
 8000 continue
      print*,' ',ilf(1:ilfl),' NOT FOUND'
 9003 format(a80)
10034 format(a10,1x,a11,2x,f4.1,2x,2(i4,1x),a4,1x,a4,1x,a7)

      end subroutine color2cgm_main


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!
      subroutine color2cgm_find_file (path, fn, fnwp, nfnc)
      use putsys_module
      use getlun_module
      implicit none
!
! inquire about a path and filename and return its full path specification
!
      character path*(*), fn*(*), fnwp*(*)
      integer nfnc
!
      integer jstat, mxc, ifn
      character tmp*160
!
      data mxc/160/
!
      mxc = min (mxc, nfnc)
      nfnc = 0
      tmp = ' '
!
! attempt to find the given file
      call putsys_cmd ('find '//path//' -name '//fn//' > COLOR2CGMFF', jstat)
      if (jstat .eq. 0) then
        call getlun (ifn, jstat)
        if (jstat .eq. 0) then
          open (unit=ifn, file='COLOR2CGMFF', status='OLD', iostat=jstat, &
            action='READ')
          if (jstat .eq. 0) then
! read the file name from the first record 
            read (unit=ifn, fmt='(A160)',end=10) tmp
 10         close (ifn)
            call color2cgm_count (tmp(1:1), mxc, nfnc)
            if (nfnc .gt. 0) then
              fnwp(1:nfnc) = tmp(1:nfnc)
            end if
          end if
        end if
      end if
      call putsys_cmd ('rm COLOR2CGMFF', jstat)
      return
      end subroutine color2cgm_find_file
      end module color2cgm_module
      program color2cgm
      use color2cgm_module
      character(len=100),save :: XXXX_IDENT = &
'$Id: color2cgm.f90,v 1.2 2002/04/15 17:49:26 Goodger prod sps $'
      call color2cgm_main
      end program color2cgm
      SUBROUTINE COLOR2CGM_COUNT (A, MAX, NCHAR)
!======================================================================
! --- FIND LAST NON-BLANK CHARACTER IN ARRAY
!
! --- A     = INPUT CHARACTER*1 ARRAY
! --- MAX   = MAXIMUM NON-BLANK CHARACTER POSSIBLE
! --- NCHAR = RETURNED LAST NON-BLANK CHARACTER IN ARRAY
!
! --- MODIFIED FOR NEW CPS:K C CORN                      DATE:AUG 2000
!=====================================================================
      IMPLICIT NONE

      CHARACTER*1 A(*)
      INTEGER MAX, NCHAR
      CHARACTER BLNK*1
      INTEGER I
!
      DATA BLNK/" "/
!
      NCHAR = 0
      DO I = 1, MAX
        IF (A(I) .NE. BLNK) NCHAR = I
      END DO
!
      RETURN
      END SUBROUTINE COLOR2CGM_COUNT
      SUBROUTINE COLOR2CGM_FILLB (A, B, N)
      IMPLICIT NONE
!
!     FILL N BYTES OF ARRAY A WITH B
!
      CHARACTER*1 A(*), B
      INTEGER N
!
      INTEGER I
!
      IF (N .GT. 0) THEN
        DO I = 1, N
          A(I) = B
        END DO
      ENDIF
      RETURN
      END SUBROUTINE COLOR2CGM_FILLB
      SUBROUTINE COLOR2CGM_LJUS (A, NC)
      IMPLICIT NONE
!
!          LEFT JUSTIFY CHARACTERS WITHIN A CHARACTER*1 ARRAY
!
      CHARACTER*1 A(*)
      INTEGER NC
!
      CHARACTER*1 BLNK
      INTEGER I, K, FNBC, IBLK
!
      DATA BLNK/" "/
!
      IBLK = IACHAR (BLNK)
      FNBC = 0
      I    = 1
! FIND THE FIRST NON-BLANK CHARACTER IN THE GIVEN ARRAY
      DO WHILE (FNBC .EQ. 0 .AND. I .LE. NC)
        K = IACHAR (A(I))
        IF (K .NE. 0 .AND. K .NE. IBLK) FNBC = I
        I = I + 1
      END DO
! 
      IF (FNBC .GT. 1) THEN
! DO A LEFT-SHIFT IN PLACE
        DO I = 1, NC-FNBC+1
          A(I) = A(I+FNBC-1)
        END DO
! PUT THE BLANKS ON THE RIGHT
        CALL COLOR2CGM_FILLB (A(NC-FNBC+2), BLNK, FNBC-1)
      END IF
      RETURN
      END SUBROUTINE COLOR2CGM_LJUS
      SUBROUTINE color2cgm_ZRO2BLK (A, N)
!
!          REPLACE HOLLERITH ZERO FILL CHARACTER WITH BLANKS
!
      IMPLICIT NONE
      CHARACTER*1 A(*)
      INTEGER N
!
      CHARACTER BLNK*1
      INTEGER I, K

      DATA BLNK/" "/
!
      DO I = 1, N
        K = IACHAR (A(I))
       IF (K .EQ. 0) A(I) = BLNK
      END DO
!
      RETURN
      END SUBROUTINE color2cgm_ZRO2BLK
      INTEGER FUNCTION color2cgm_TONE_COUNT (NE, NA)
      IMPLICIT NONE
!
      INTEGER NE(*), NA
      INTEGER I
!
      color2cgm_TONE_COUNT = 0
      IF (NA .GT. 0) THEN
        DO I=1,NA
          color2cgm_TONE_COUNT = color2cgm_TONE_COUNT + NE(I)
        END DO
      ENDIF
      RETURN
      END FUNCTION color2cgm_TONE_COUNT

!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

