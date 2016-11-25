!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- geomio1.f90 --------------------------------!!
!!------------------------------- geomio1.f90 --------------------------------!!
!!------------------------------- geomio1.f90 --------------------------------!!


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
!                        C P S   P R I M I T I V E           
!
! Name       : GEOMIO1
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : To read and write old-style CPS field geometry files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading and writing old-style CPS field geometry
! files.
!
! This primitive does not open or close the file.  The calling program must
! use the DIO primitive to open and close the file.
!
!-------------------------------------------------------------------------------
!</descript_doc>

 
!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS           
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                    READ OR WRITE THE HEADER RECORD         
!
!                               b   b      i     o   o
!   call geomio1_read_header  (dio,pjar,secname,err,msg)
!   call geomio1_write_header (dio,pjar,secname,err,msg)
!                               b   b      i     o   o
!
!                        ++++++++++++++++++++++++++
!
! type(dio_struct)  dio     = reference to the DIO I/O structure.
! type(pjar_struct) pjar    = reference to the PJAR data structure.
! char(*)           secname = PJAR section containing the parameters.
! integer           err     = error flag (returned).
! char(*)           msg     = message for possible printing (returned).
!
! real              ve       = reference velocity.
! real              datum    = datum elevation.
! real              fixdist  = uniform inline distance parameter.
! type(grid_struct) grid     = grid transformation structure.
! char(*)           chaining = chaining flag.
! integer           nld      = number of LD cards.
! integer           nrp      = number of RP cards.
! integer           npp      = number of PP cards.
! integer           nzt1     = number of ZT1 cards.
! integer           nzt2     = number of ZT2 cards.
! integer           nzt3     = number of ZT3 cards.
! integer           nzt4     = number of ZT4 cards.
!
!-------------------------------------------------------------------------------
!                      READ AND WRITE INDIVIDUAL CARDS
!
!      (must call for each LD  card with ild  = 1,nld  consecutively)
!      (must call for each RP  card with irp  = 1,nrp  consecutively)
!      (must call for each PP  card with ipp  = 1,npp  consecutively)
!      (must call for each ZT1 card with izt1 = 1,nzt1 consecutively)
!      (must call for each ZT2 card with izt2 = 1,nzt2 consecutively)
!      (must call for each ZT3 card with izt3 = 1,nzt3 consecutively)
!      (must call for each ZT4 card with izt4 = 1,nzt4 consecutively)
!       (the card types must be read or written in the above order)
!
! Read or write one LD card:
!
!                                     b   i    o   o
!        call geomio1_read_ld_card  (dio,ild,err,msg,
!                sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
!                o   o    o    o    o     o    o  o  o   o   o   o    o
!
!                                     b   i   o   o
!        call geomio1_write_ld_card (dio,ild,err,msg,
!                sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
!                i   i    i    i    i     i    i  i  i   i   i   i    i
!
!
! Read or write one RP card:
!
!                                     b   i   o   o
!        call geomio1_read_rp_card  (dio,irp,err,msg,
!                ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
!                  o    o    o    o   o    o   o    o    o    o     o
!
!                                     b   i   o   o
!        call geomio1_write_rp_card (dio,irp,err,msg,
!                ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
!                  i    i    i    i   i    i   i    i    i    i     i
!
!
! Read or write one PP card:
!
!                                     b   i   o   o
!        call geomio1_read_pp_card  (dio,ipp,err,msg,
!          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
!           o    o    o    o     o    o    o    o     o     o     o    o  o  o
!
!                                     b   i   o   o
!        call geomio1_write_pp_card (dio,ipp,err,msg,
!          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
!           i    i    i    i     i    i    i    i     i     i     i    i  i  i
!
!
! Read or write one ZT1 card:
!
!                                      b   i    o   o
!        call geomio1_read_zt1_card  (dio,izt1,err,msg,
!                                     ccc1,sss1,sss1a,lll1)
!                                      o    o     o    o
!
!                                      b   i    o   o
!        call geomio1_write_zt1_card (dio,izt1,err,msg,
!                                     ccc1,sss1,sss1a,lll1)
!                                      i    i     i    i
!
!
! Read or write one ZT2 card:
!
!                                      b   i    o   o
!        call geomio1_read_zt2_card  (dio,izt2,err,msg,
!                                     ccc2,rrr2,rrr2a,lll2)
!                                      o    o     o    o
!
!                                      b   i    o   o
!        call geomio1_write_zt2_card (dio,izt2,err,msg,
!                                     ccc2,rrr2,rrr2a,lll2)
!                                      i    i     i    i
!
!
! Read or write one ZT3 card:
!
!                                      b   i    o   o
!        call geomio1_read_zt3_card  (dio,izt3,err,msg,
!                                     ccc3,iggg3,iggg3a,ittt3,ittt3a)
!                                      o     o     o      o     o
!
!                                      b   i    o   o
!        call geomio1_write_zt3_card (dio,izt3,err,msg,
!                                     ccc3,iggg3,iggg3a,ittt3,ittt3a)
!                                      i     i     i      i     i
!
!
! Read or write one ZT4 card:
!
!                                      b   i    o   o
!        call geomio1_read_zt4_card  (dio,izt4,err,msg,
!                                     ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
!                                      o    o     o    o    o     o     o
!
!                                      b   i    o   o
!        call geomio1_write_zt4_card (dio,izt4,err,msg,
!                                     ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
!                                      i    i     i    i    i     i     i
!
!                        ++++++++++++++++++++++++++
!
! type(dio_struct)  dio  = reference to the DIO data structure.
! integer           err  = error flag (returned).
! char(*)           msg  = message for possible printing (returned).
!
! integer ild      = LD card number.
! integer irp      = RP card number.
! integer ipp      = PP card number.
! integer izt1     = ZT1 card number.
! integer izt2     = ZT2 card number.
! integer izt3     = ZT3 card number.
! integer izt4     = ZT4 card number.
!
! ERR will be set to DIO_OK or DIO_ERROR or DIO_EOF.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  6. 2006-06-20  B. Menger  Removed Unused Variables.
!  5. 2003-12-09  Stoeckley  Fix output format for TUH2.
!  4. 2003-06-17  Stoeckley  Increase the size of BUFFER in geomio1_read_header
!                             to keep from truncating grid transform data.
!  3. 2002-06-24  Stoeckley  Write optional cards to file only when the
!                             parameters are in the pickle jar.
!  2. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR primitive.
!  1. 2000-11-27  Stoeckley  Initial version moved from code in the GEOMIO
!                             primitive.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
 
 
      module geomio1_module
      use dio_module
      use pjar_module
      use string_module
      use named_constants_module
      use grid_module
      implicit none
      public

      private :: geomio1_ff2cc
      private :: geomio1_ii2cc
      private :: geomio1_cc2ff
      private :: geomio1_cc2ii
 
      character(len=100),public,save :: GEOMIO1_IDENT = &
       '$Id: geomio1.f90,v 1.6 2006/06/20 13:11:53 Menger prod sps $'

      contains


!!---------------------------- geomio1 read header --------------------------!!
!!---------------------------- geomio1 read header --------------------------!!
!!---------------------------- geomio1 read header --------------------------!!


      subroutine geomio1_read_header (dio,pjar,secname,err,msg)

      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      type(pjar_struct)  ,intent(inout) :: pjar                  ! arguments
      character(len=*)   ,intent(in)    :: secname               ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real                              :: ve,datum,fixdist      ! local
      type(grid_struct)                 :: grid                  ! local    
      character(len=8)                  :: chaining              ! local    
      integer                           :: nld,nrp,npp           ! local    
      integer                           :: nzt1,nzt2,nzt3,nzt4   ! local    
      integer                           :: status                ! local
      character(len=200)                :: card,buffer           ! local
      character(len=40)                 :: noun,dddd,tttt        ! local
      character(len=80)                 :: keyword               ! local
      character(len=40)                 :: key1,key2,key3        ! local
      double precision                  :: xorigin,yorigin,angle ! local
      double precision                  :: xwidth,ywidth         ! local
      character(len=20)                 :: handed                ! local

!!!!!!!!!! get started:

      call grid_initialize (grid)
      xorigin  = DNIL
      yorigin  = DNIL
      angle    = DNIL
      xwidth   = DNIL
      ywidth   = DNIL
      handed   = ' '

!!!!!!!!!! read header record:

      noun = 'field geometry header'
      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      if (card(1:8) .ne. '  DATE  ') then
           err = DIO_ERROR
           msg = 'this file does not appear to have the right format'
           return
      end if

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      read (card,1000,iostat=status) dddd,tttt,chaining,  &
                                     nld,nrp,npp,nzt1,nzt2,nzt3,nzt4
1000  format (1x,a10,1x,a10,1x,a4,2x,7i7)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error decoding field geometry header'
           return
      end if

      call pjar_choose_section (pjar,secname)
      call pjar_put            (pjar, 'chaining' , chaining )
      call pjar_put            (pjar, 'nld'      , nld      )
      call pjar_put            (pjar, 'nrp'      , nrp      )
      call pjar_put            (pjar, 'npp'      , npp      )
      call pjar_put            (pjar, 'nzt1'     , nzt1     )
      call pjar_put            (pjar, 'nzt2'     , nzt2     )
      call pjar_put            (pjar, 'nzt3'     , nzt3     )
      call pjar_put            (pjar, 'nzt4'     , nzt4     )

!!!!!!!!!! read optional cards:

      do
        noun = 'field geometry optional card'
        call dio_read_card (dio,card)
        call dio_status    (dio,err,msg,noun)
        if (err /= DIO_OK) return

        read (card,2000,iostat=status) keyword,buffer
2000    format (a6,a)
        if (status /= 0) then
             err = DIO_ERROR
             msg = 'error decoding field geometry optional card'
             return
        end if
        select case (keyword)
          case ('OPT1:')
             read (buffer,*,iostat=status) key1,ve,key2,datum,key3,fixdist
             if (status == 0) then
                  call pjar_put (pjar, 've'      , ve     )
                  call pjar_put (pjar, 'datum'   , datum  )
                  call pjar_put (pjar, 'fixdist' , fixdist)
             end if
          case ('OPT2:')
             read (buffer,*,iostat=status) key1,xorigin,key2,yorigin,key3,angle
          case ('OPT3:')
             read (buffer,*,iostat=status) key1,xwidth,key2,ywidth,handed
          case default
             exit
        end select
        if (status /= 0) then
             err = DIO_ERROR
             msg = 'error decoding field geometry '//keyword(1:4)//' card'
             return
        end if
      end do

!!!!!!!!!! reset grid transform:

      if (xorigin /= DNIL .and. yorigin /= DNIL .and.  &
          xwidth  /= DNIL .and. ywidth  /= DNIL .and.  &
          angle   /= DNIL .and. handed  /= ' ') then
           if (handed == 'right-handed') then
                call grid_set_right_handed_transform  &
                               (grid,xorigin,yorigin,angle,xwidth,ywidth)
           else
                call grid_set_left_handed_transform  &
                               (grid,xorigin,yorigin,angle,xwidth,ywidth)
           end if
           call pjar_put (pjar, 'grid', grid)
      end if

!!!!!!!!!! backspace over unused card image and return:

      call dio_backspace (dio)
      err = DIO_OK
      msg = 'field geometry header record successfully read'
      return
      end subroutine geomio1_read_header


!!!---------------------------- geomio1 read header --------------------------!!
!!!---------------------------- geomio1 read header --------------------------!!
!!!---------------------------- geomio1 read header --------------------------!!
!
!
!      subroutine geomio1_read_header (dio,err,msg,             &
!                                ve,datum,fixdist,grid,         &
!                                chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4)
!
!      implicit none
!      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
!      integer            ,intent(out)   :: err                   ! arguments
!      character(len=*)   ,intent(out)   :: msg                   ! arguments
!      real               ,intent(out)   :: ve,datum,fixdist      ! arguments
!      type(grid_struct)  ,intent(out)   :: grid                  ! arguments
!      character(len=*)   ,intent(out)   :: chaining              ! arguments
!      integer            ,intent(out)   :: nld,nrp,npp           ! arguments
!      integer            ,intent(out)   :: nzt1,nzt2,nzt3,nzt4   ! arguments
!      integer                           :: status                ! local
!      character(len=200)                :: card                  ! local
!      character(len=40)                 :: noun,dddd,tttt        ! local
!      character(len=80)                 :: keyword,buffer        ! local
!      character(len=40)                 :: key1,key2,key3        ! local
!      double precision                  :: xorigin,yorigin,angle ! local
!      double precision                  :: xwidth,ywidth         ! local
!      character(len=20)                 :: handed                ! local
!
!!!!!!!!!!! get started:
!
!      chaining = ' '
!      nld      = 0
!      nrp      = 0
!      npp      = 0
!      nzt1     = 0
!      nzt2     = 0
!      nzt3     = 0
!      nzt4     = 0
!      ve       = FNIL
!      datum    = FNIL
!      fixdist  = FNIL
!      call grid_initialize (grid)
!      xorigin  = DNIL
!      yorigin  = DNIL
!      angle    = DNIL
!      xwidth   = DNIL
!      ywidth   = DNIL
!      handed   = ' '
!
!!!!!!!!!!! read header record:
!
!      noun = 'field geometry header'
!      call dio_read_card (dio,card)
!      call dio_status    (dio,err,msg,noun)
!      if (err /= DIO_OK) return
!
!      if (card(1:8) .ne. '  DATE  ') then
!           err = DIO_ERROR
!           msg = 'this file does not appear to have the right format'
!           return
!      end if
!
!      call dio_read_card (dio,card)
!      call dio_status    (dio,err,msg,noun)
!      if (err /= DIO_OK) return
!
!      read (card,1000,iostat=status) dddd,tttt,chaining,  &
!                                     nld,nrp,npp,nzt1,nzt2,nzt3,nzt4
!1000  format (1x,a10,1x,a10,1x,a4,2x,7i7)
!      if (status /= 0) then
!           err = DIO_ERROR
!           msg = 'error decoding field geometry header'
!           return
!      end if
!
!!!!!!!!!!! read optional cards:
!
!      do
!        noun = 'field geometry optional card'
!        call dio_read_card (dio,card)
!        call dio_status    (dio,err,msg,noun)
!        if (err /= DIO_OK) return
!
!        read (card,2000,iostat=status) keyword,buffer
!2000    format (a6,a74)
!        if (status /= 0) then
!             err = DIO_ERROR
!             msg = 'error decoding field geometry optional card'
!             return
!        end if
!        if (keyword == 'OPT1:') then
!             read (buffer,*,iostat=status) key1,ve,key2,datum,key3,fixdist
!        else if (keyword == 'OPT2:') then
!             read (buffer,*,iostat=status) key1,xorigin,key2,yorigin,key3,angle
!        else if (keyword == 'OPT3:') then
!             read (buffer,*,iostat=status) key1,xwidth,key2,ywidth,handed
!        else
!             exit
!        end if
!      end do
!
!!!!!!!!!!! reset grid transform:
!
!      if (xorigin /= DNIL .and. yorigin /= DNIL .and.  &
!          xwidth  /= DNIL .and. ywidth  /= DNIL .and.  &
!          angle   /= DNIL .and. handed  /= ' ') then
!           if (handed == 'right-handed') then
!                call grid_set_right_handed_transform  &
!                               (grid,xorigin,yorigin,angle,xwidth,ywidth)
!           else
!                call grid_set_left_handed_transform  &
!                               (grid,xorigin,yorigin,angle,xwidth,ywidth)
!           end if
!      end if
!
!!!!!!!!!!! backspace over unused card image and return:
!
!      call dio_backspace (dio)
!      err = DIO_OK
!      msg = 'field geometry header record successfully read'
!      return
!      end subroutine geomio1_read_header


!!------------------------- geomio1 write header --------------------------!!
!!------------------------- geomio1 write header --------------------------!!
!!------------------------- geomio1 write header --------------------------!!


      subroutine geomio1_write_header (dio,pjar,secname,err,msg)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                    ! arguments
      type(pjar_struct)  ,intent(inout) :: pjar                   ! arguments
      character(len=*)   ,intent(in)    :: secname                ! arguments
      integer            ,intent(out)   :: err                    ! arguments
      character(len=*)   ,intent(out)   :: msg                    ! arguments
      real                              :: ve,datum,fixdist       ! local
      type(grid_struct)                 :: grid                   ! local
      character(len=8)                  :: chaining               ! local
      integer                           :: nld,nrp,npp            ! local
      integer                           :: nzt1,nzt2,nzt3,nzt4    ! local
      integer                           :: status                 ! local
      character(len=200)                :: card                   ! local
      character(len=40)                 :: noun,dddd,tttt         ! local


      double precision                  :: xorigin,yorigin,angle  ! local
      double precision                  :: xwidth,ywidth          ! local
      character(len=20)                 :: handed                 ! local
      integer                           :: hand                   ! local

!!!!!!!!!! get parameters from pickle jar:

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 've'       , ve       )
      call pjar_get            (pjar, 'datum'    , datum    )
      call pjar_get            (pjar, 'fixdist'  , fixdist  )
      call pjar_get            (pjar, 'grid'     , grid     )
      call pjar_get            (pjar, 'chaining' , chaining )
      call pjar_get            (pjar, 'nld'      , nld      )
      call pjar_get            (pjar, 'nrp'      , nrp      )
      call pjar_get            (pjar, 'npp'      , npp      )
      call pjar_get            (pjar, 'nzt1'     , nzt1     )
      call pjar_get            (pjar, 'nzt2'     , nzt2     )
      call pjar_get            (pjar, 'nzt3'     , nzt3     )
      call pjar_get            (pjar, 'nzt4'     , nzt4     )

!!!!!!!!!! write header record:

      noun = 'field geometry header'

      card = '  DATE       TIME      CHAIN     #LD    #RP    #PP'//  &
                 '    #ZT1   #ZT2   #ZT3   #ZT4'
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      if (err /= DIO_OK) return

      call string_date (dddd)
      call string_time (tttt)
      write (card,1000,iostat=status) dddd,tttt,chaining,  &
                                      nld,nrp,npp,nzt1,nzt2,nzt3,nzt4
1000  format (1x,a10,1x,a10,1x,a4,2x,7i7)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding field geometry header'
           return
      end if
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      if (err /= DIO_OK) return

      noun = 'field geometry optional card'

!!!!!!!!!! write first optional card:

      if (pjar_keyword_present(pjar, 've')) then
           write (card,*,iostat=status) &
                   'OPT1: ve= ',ve,' ref= ',datum,' fixdist= ',fixdist
           if (status /= 0) then
                err = DIO_ERROR
                msg = 'error decoding field geometry optional card'
                return
           end if
           call dio_write_card (dio,card(2:))
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if

!!!!!!!!!! write second optional card:

      if (pjar_keyword_present(pjar, 'grid')) then
           xorigin = grid_get_xorigin        (grid)
           yorigin = grid_get_yorigin        (grid)
           angle   = grid_get_rotation_angle (grid)
           xwidth  = grid_get_xgrid_width    (grid)
           ywidth  = grid_get_ygrid_width    (grid)
           hand    = grid_get_handedness     (grid)
           if (hand > 0) then
                handed = 'right-handed'
           else
                handed = 'left-handed'
           end if

           write (card,*,iostat=status) &
              'OPT2: xorigin= ',xorigin,' yorigin= ',yorigin,' angle= ',angle
           if (status /= 0) then
                err = DIO_ERROR
                msg = 'error decoding field geometry optional card'
                return
           end if
           call dio_write_card (dio,card(2:))
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return

!!!!!!!!!! write third optional card:

           write (card,*,iostat=status) &
              'OPT3: xwidth= ',xwidth,' ywidth= ',ywidth,' ',handed
           if (status /= 0) then
                err = DIO_ERROR
                msg = 'error decoding field geometry optional card'
                return
           end if
           call dio_write_card (dio,card(2:))
           call dio_status     (dio,err,msg,noun)
      end if
      return
      end subroutine geomio1_write_header


!!!------------------------- geomio1 write header --------------------------!!
!!!------------------------- geomio1 write header --------------------------!!
!!!------------------------- geomio1 write header --------------------------!!
!
!
!      subroutine geomio1_write_header (dio,err,msg,ve,datum,fixdist,grid,  &
!                                 chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4)
!      implicit none
!      type(dio_struct)   ,intent(inout) :: dio                    ! arguments
!      integer            ,intent(out)   :: err                    ! arguments
!      character(len=*)   ,intent(out)   :: msg                    ! arguments
!      real               ,intent(in)    :: ve,datum,fixdist       ! arguments
!      type(grid_struct)  ,intent(in)    :: grid                   ! arguments
!      character(len=*)   ,intent(in)    :: chaining               ! arguments
!      integer            ,intent(in)    :: nld,nrp,npp            ! arguments
!      integer            ,intent(in)    :: nzt1,nzt2,nzt3,nzt4    ! arguments
!      integer                           :: status                 ! local
!      character(len=200)                :: card                   ! local
!      character(len=40)                 :: noun,dddd,tttt         ! local
!      character(len=80)                 :: keyword,buffer         ! local
!      character(len=40)                 :: key1,key2,key3         ! local
!      double precision                  :: xorigin,yorigin,angle  ! local
!      double precision                  :: xwidth,ywidth          ! local
!      character(len=20)                 :: handed                 ! local
!      integer                           :: hand                   ! local
!
!!!!!!!!!!! write header record:
!
!      noun = 'field geometry header'
!
!      card = '  DATE       TIME      CHAIN     #LD    #RP    #PP'//  &
!                 '    #ZT1   #ZT2   #ZT3   #ZT4'
!      call dio_write_card (dio,card)
!      call dio_status     (dio,err,msg,noun)
!      if (err /= DIO_OK) return
!
!      call string_date (dddd)
!      call string_time (tttt)
!      write (card,1000,iostat=status) dddd,tttt,chaining,  &
!                                      nld,nrp,npp,nzt1,nzt2,nzt3,nzt4
!1000  format (1x,a10,1x,a10,1x,a4,2x,7i7)
!      if (status /= 0) then
!           err = DIO_ERROR
!           msg = 'error encoding field geometry header'
!           return
!      end if
!      call dio_write_card (dio,card)
!      call dio_status     (dio,err,msg,noun)
!      if (err /= DIO_OK) return
!
!!!!!!!!!!! write optional cards:
!
!      noun = 'field geometry optional card'
!
!      write (card,*,iostat=status) &
!              'OPT1: ve= ',ve,' ref= ',datum,' fixdist= ',fixdist
!      if (status /= 0) then
!           err = DIO_ERROR
!           msg = 'error decoding field geometry optional card'
!           return
!      end if
!      call dio_write_card (dio,card(2:))
!      call dio_status     (dio,err,msg,noun)
!      if (err /= DIO_OK) return
!
!      xorigin = grid_get_xorigin        (grid)
!      yorigin = grid_get_yorigin        (grid)
!      angle   = grid_get_rotation_angle (grid)
!      xwidth  = grid_get_xgrid_width    (grid)
!      ywidth  = grid_get_ygrid_width    (grid)
!      hand    = grid_get_handedness     (grid)
!      if (hand > 0) then
!           handed = 'right-handed'
!      else
!           handed = 'left-handed'
!      end if
!
!      write (card,*,iostat=status) &
!              'OPT2: xorigin= ',xorigin,' yorigin= ',yorigin,' angle= ',angle
!      if (status /= 0) then
!           err = DIO_ERROR
!           msg = 'error decoding field geometry optional card'
!           return
!      end if
!      call dio_write_card (dio,card(2:))
!      call dio_status     (dio,err,msg,noun)
!!      if (err /= DIO_OK) return
!
!      write (card,*,iostat=status) &
!              'OPT3: xwidth= ',xwidth,' ywidth= ',ywidth,' ',handed
!      if (status /= 0) then
!           err = DIO_ERROR
!           msg = 'error decoding field geometry optional card'
!           return
!      end if
!      call dio_write_card (dio,card(2:))
!      call dio_status     (dio,err,msg,noun)
!      return
!      end subroutine geomio1_write_header


!!--------------------- geomio1 read ld card -------------------------------!!
!!--------------------- geomio1 read ld card -------------------------------!!
!!--------------------- geomio1 read ld card -------------------------------!!


      subroutine geomio1_read_ld_card  (dio,ild,err,msg,   &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: ild                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real   ,intent(out)  :: sp,dist,xloc,yloc,elev             ! arguments
      real   ,intent(out)  :: depth,tuh,tr,ts,xsd,ysd,elsd       ! arguments
      integer,intent(out)  :: line                               ! arguments
      character(len=20)    :: noun                               ! local
      character(len=16)    :: sp_cc,dist_cc,xloc_cc,yloc_cc      ! local
      character(len=16)    :: elev_cc                            ! local
      character(len=16)    :: depth_cc,tuh_cc,tr_cc,ts_cc        ! local
      character(len=16)    :: xsd_cc,ysd_cc,elsd_cc              ! local
      integer              :: status                             ! local
      character(len=200)   :: card                               ! local

      if (ild == 1) then
           noun = 'LD card header'
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'LD card '//string_ii2ss(ild)

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      read (card,1000,iostat=status) sp_cc,dist_cc,xloc_cc,yloc_cc, &
          elev_cc,depth_cc,tuh_cc,tr_cc,ts_cc,xsd_cc,ysd_cc,elsd_cc,line
1000  format (a7,3a9,a6,a5,a6,2a5,3a6,i7)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error decoding '//noun
           return
      end if

      err = DIO_OK
      msg = trim(noun)//' successfully read and decoded'
      call geomio1_cc2ff (sp_cc   ,sp   ,noun,err,msg)
      call geomio1_cc2ff (dist_cc ,dist ,noun,err,msg)
      call geomio1_cc2ff (xloc_cc ,xloc ,noun,err,msg)
      call geomio1_cc2ff (yloc_cc ,yloc ,noun,err,msg)
      call geomio1_cc2ff (elev_cc ,elev ,noun,err,msg)
      call geomio1_cc2ff (depth_cc,depth,noun,err,msg)
      call geomio1_cc2ff (tuh_cc  ,tuh  ,noun,err,msg)
      call geomio1_cc2ff (tr_cc   ,tr   ,noun,err,msg)
      call geomio1_cc2ff (ts_cc   ,ts   ,noun,err,msg)
      call geomio1_cc2ff (xsd_cc  ,xsd  ,noun,err,msg)
      call geomio1_cc2ff (ysd_cc  ,ysd  ,noun,err,msg)
      call geomio1_cc2ff (elsd_cc ,elsd ,noun,err,msg)
      return
      end subroutine geomio1_read_ld_card


!!--------------------- geomio1 write ld card -------------------------------!!
!!--------------------- geomio1 write ld card -------------------------------!!
!!--------------------- geomio1 write ld card -------------------------------!!


      subroutine geomio1_write_ld_card  (dio,ild,err,msg,   &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: ild                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real   ,intent(in)   :: sp,dist,xloc,yloc,elev             ! arguments
      real   ,intent(in)   :: depth,tuh,tr,ts,xsd,ysd,elsd       ! arguments
      integer,intent(in)   :: line                               ! arguments
      character(len=20)    :: noun                               ! local
      character(len=16)    :: sp_cc,dist_cc,xloc_cc,yloc_cc      ! local
      character(len=16)    :: elev_cc                            ! local
      character(len=16)    :: depth_cc,tuh_cc,tr_cc,ts_cc        ! local
      character(len=16)    :: xsd_cc,ysd_cc,elsd_cc              ! local
      integer              :: status                             ! local
      character(len=200)   :: card                               ! local

      if (ild == 1) then
           noun = 'LD card header'
           card = ' SP#     DIST     XLOC    YLOC     ELEV '//  &
                   '  HD  TUH    TR   TS  XSD   YSD    ELSD   LINE'
           call dio_write_card (dio,card)
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'LD card '//string_ii2ss(ild)

      err = DIO_OK
      msg = trim(noun)//' successfully encoded and written'
      call geomio1_ff2cc (sp   , sp_cc   , '(f7.1)',noun,err,msg)
      call geomio1_ff2cc (dist , dist_cc , '(f9.1)',noun,err,msg)
      call geomio1_ff2cc (xloc , xloc_cc , '(i9  )',noun,err,msg)
      call geomio1_ff2cc (yloc , yloc_cc , '(i9  )',noun,err,msg)
      call geomio1_ff2cc (elev , elev_cc , '(i6  )',noun,err,msg)
      call geomio1_ff2cc (depth, depth_cc, '(i5  )',noun,err,msg)
      call geomio1_ff2cc (tuh  , tuh_cc  , '(f6.3)',noun,err,msg)
      call geomio1_ff2cc (tr   , tr_cc   , '(f5.1)',noun,err,msg)
      call geomio1_ff2cc (ts   , ts_cc   , '(f5.1)',noun,err,msg)
      call geomio1_ff2cc (xsd  , xsd_cc  , '(i6  )',noun,err,msg)
      call geomio1_ff2cc (ysd  , ysd_cc  , '(i6  )',noun,err,msg)
      call geomio1_ff2cc (elsd , elsd_cc , '(i6  )',noun,err,msg)
      if (err /= DIO_OK) return

      write (card,1000,iostat=status) sp_cc,dist_cc,xloc_cc,yloc_cc, &
             elev_cc,depth_cc,tuh_cc,tr_cc,ts_cc,xsd_cc,ysd_cc,elsd_cc,line
1000  format (a7,3a9,a6,a5,a6,2a5,3a6,i7)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//noun
           return
      end if
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      return
      end subroutine geomio1_write_ld_card


!!--------------------- geomio1 read rp card -------------------------------!!
!!--------------------- geomio1 read rp card -------------------------------!!
!!--------------------- geomio1 read rp card -------------------------------!!


      subroutine geomio1_read_rp_card  (dio,irp,err,msg,   &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: irp                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      integer            ,intent(out)   :: ipat1,line1           ! arguments
      integer            ,intent(out)   :: nx,ixinc,ny,iyinc     ! arguments
      real               ,intent(out)   :: sp1,xsd1,ysd1,elsd1   ! arguments
      character(len=*)   ,intent(out)   :: flag                  ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (irp == 1) then
           noun = 'RP card header'
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'RP card '//string_ii2ss(irp)

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      read (card,1000,iostat=status) ipat1,flag,sp1,line1,  &
                                     nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1
1000  format (1x,i6,1x,a4,f9.0,i6,i7,i6,i8,i7,f9.0,f8.0,f8.0)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error decoding '//noun
           return
      end if
      err = DIO_OK
      msg = trim(noun)//' successfully read and decoded'
      return
      end subroutine geomio1_read_rp_card


!!--------------------- geomio1 write rp card -------------------------------!!
!!--------------------- geomio1 write rp card -------------------------------!!
!!--------------------- geomio1 write rp card -------------------------------!!


      subroutine geomio1_write_rp_card  (dio,irp,err,msg,   &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: irp                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      integer            ,intent(in)    :: ipat1,line1           ! arguments
      integer            ,intent(in)    :: nx,ixinc,ny,iyinc     ! arguments
      real               ,intent(in)    :: sp1,xsd1,ysd1,elsd1   ! arguments
      character(len=*)   ,intent(in)    :: flag                  ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (irp == 1) then
           noun = 'RP card header'
           card = ' PAT#   FLAG     sp#   LINE#    #X  XINC  '//  &
                    '    #Y   YINC     XsD     YsD    ELsD'
           call dio_write_card (dio,card)
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'RP card '//string_ii2ss(irp)

      write (card,1000,iostat=status) ipat1,flag,sp1,line1,  &
                                      nx,ixinc,ny,iyinc,     &
                                      nint(xsd1),nint(ysd1),nint(elsd1)
1000  format (1x,i6,1x,a4,f9.1,i6,i7,i6,i8,i7,i9,i8,i8)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//noun
           return
      end if
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      return
      end subroutine geomio1_write_rp_card


!!--------------------- geomio1 read pp card -------------------------------!!
!!--------------------- geomio1 read pp card -------------------------------!!
!!--------------------- geomio1 read pp card -------------------------------!!


      subroutine geomio1_read_pp_card  (dio,ipp,err,msg,   &
           sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: ipp                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real               ,intent(out)   :: sp2,sp3,xsd2,ysd2     ! arguments
      integer            ,intent(out)   :: line2,line3,ipat2     ! arguments
      real               ,intent(out)   :: elev2,depth2,tuh2     ! arguments
      integer            ,intent(out)   :: hold,is,ir,ig         ! arguments
      character(len=20)                 :: noun                  ! local
      character(len=12)    :: sp2_cc,line2_cc,sp3_cc,line3_cc    ! local
      character(len=12)    :: ipat2_cc,xsd2_cc,ysd2_cc           ! local
      character(len=12)    :: hold_cc,elev2_cc,depth2_cc,tuh2_cc ! local
      character(len=12)    :: is_cc,ir_cc,ig_cc                  ! local
      integer              :: status                             ! local
      character(len=200)   :: card                               ! local

      if (ipp == 1) then
           noun = 'PP card header'
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'PP card '//string_ii2ss(ipp)

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      read (card,1000,iostat=status) sp2_cc,line2_cc,sp3_cc,line3_cc,  &
           ipat2_cc,xsd2_cc,ysd2_cc,hold_cc,elev2_cc,                  &
           depth2_cc,tuh2_cc,is_cc,ir_cc,ig_cc
1000  format (a7,a5,a9,a5,a6,a6,a5,a5,a7,a4,a5,a4,a5,a7)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error decoding '//noun
           return
      end if

      err = DIO_OK
      msg = trim(noun)//' successfully read and decoded'
      call geomio1_cc2ff (sp2_cc   ,sp2   ,noun,err,msg);
      call geomio1_cc2ii (line2_cc ,line2 ,noun,err,msg);
      call geomio1_cc2ff (sp3_cc   ,sp3   ,noun,err,msg);
      call geomio1_cc2ii (line3_cc ,line3 ,noun,err,msg);
      call geomio1_cc2ii (ipat2_cc ,ipat2 ,noun,err,msg);
      call geomio1_cc2ff (xsd2_cc  ,xsd2  ,noun,err,msg);
      call geomio1_cc2ff (ysd2_cc  ,ysd2  ,noun,err,msg);
      call geomio1_cc2ii (hold_cc  ,hold  ,noun,err,msg);
      call geomio1_cc2ff (elev2_cc ,elev2 ,noun,err,msg);
      call geomio1_cc2ff (depth2_cc,depth2,noun,err,msg);
      call geomio1_cc2ff (tuh2_cc  ,tuh2  ,noun,err,msg);
      call geomio1_cc2ii (is_cc    ,is    ,noun,err,msg);
      call geomio1_cc2ii (ir_cc    ,ir    ,noun,err,msg);
      call geomio1_cc2ii (ig_cc    ,ig    ,noun,err,msg);
      return
      end subroutine geomio1_read_pp_card


!!--------------------- geomio1 write pp card -------------------------------!!
!!--------------------- geomio1 write pp card -------------------------------!!
!!--------------------- geomio1 write pp card -------------------------------!!


      subroutine geomio1_write_pp_card  (dio,ipp,err,msg,   &
           sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: ipp                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real               ,intent(in)    :: sp2,sp3,xsd2,ysd2     ! arguments
      integer            ,intent(in)    :: line2,line3,ipat2     ! arguments
      real               ,intent(in)    :: elev2,depth2,tuh2     ! arguments
      integer            ,intent(in)    :: hold,is,ir,ig         ! arguments
      character(len=20)                 :: noun                  ! local
      character(len=12)    :: sp2_cc,line2_cc,sp3_cc,line3_cc    ! local
      character(len=12)    :: ipat2_cc,xsd2_cc,ysd2_cc           ! local
      character(len=12)    :: hold_cc,elev2_cc,depth2_cc,tuh2_cc ! local
      character(len=12)    :: is_cc,ir_cc,ig_cc                  ! local
      integer              :: status                             ! local
      character(len=200)   :: card                               ! local

      if (ipp == 1) then
           noun = 'PP card header'
           card = ' Sp#    Line#  sP#    line#  PAT# Xsd  Ysd '//  &
                     ' HOLD  Elev  Hd  Tuh  SRC  REC  GRP#'
           call dio_write_card (dio,card)
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'PP card '//string_ii2ss(ipp)

      err = DIO_OK
      msg = trim(noun)//' successfully encoded and written'
      call geomio1_ff2cc (sp2   ,sp2_cc   ,'(f7.1)',noun,err,msg)
      call geomio1_ii2cc (line2 ,line2_cc ,'(i5  )',noun,err,msg)
      call geomio1_ff2cc (sp3   ,sp3_cc   ,'(f9.1)',noun,err,msg)
      call geomio1_ii2cc (line3 ,line3_cc ,'(i5  )',noun,err,msg)
      call geomio1_ii2cc (ipat2 ,ipat2_cc ,'(i6  )',noun,err,msg)
      call geomio1_ff2cc (xsd2  ,xsd2_cc  ,'(i6  )',noun,err,msg)
      call geomio1_ff2cc (ysd2  ,ysd2_cc  ,'(i5  )',noun,err,msg)
      call geomio1_ii2cc (hold  ,hold_cc  ,'(i5  )',noun,err,msg)
      call geomio1_ff2cc (elev2 ,elev2_cc ,'(i7  )',noun,err,msg)
      call geomio1_ff2cc (depth2,depth2_cc,'(i4  )',noun,err,msg)
      call geomio1_ff2cc (tuh2  ,tuh2_cc  ,'(f5.0)',noun,err,msg)
      call geomio1_ii2cc (is    ,is_cc    ,'(i4  )',noun,err,msg)
      call geomio1_ii2cc (ir    ,ir_cc    ,'(i5  )',noun,err,msg)
      call geomio1_ii2cc (ig    ,ig_cc    ,'(i7  )',noun,err,msg)
      if (err /= DIO_OK) return

      write (card,1000,iostat=status) sp2_cc,line2_cc,sp3_cc,line3_cc,   &
             ipat2_cc,xsd2_cc,ysd2_cc,hold_cc,elev2_cc,                  &
             depth2_cc,tuh2_cc,is_cc,ir_cc,ig_cc
1000  format (a7,a5,a9,a5,a6,a6,a5,a5,a7,a4,a5,a4,a5,a7)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//noun
           return
      end if
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      return
      end subroutine geomio1_write_pp_card


!!--------------------- geomio1 read zt1 card -------------------------------!!
!!--------------------- geomio1 read zt1 card -------------------------------!!
!!--------------------- geomio1 read zt1 card -------------------------------!!


      subroutine geomio1_read_zt1_card  (dio,izt1,err,msg,   &
                                         ccc1,sss1,sss1a,lll1)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: izt1                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(out)   :: ccc1                  ! arguments
      real               ,intent(out)   :: sss1,sss1a            ! arguments
      integer            ,intent(out)   :: lll1                  ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (izt1 == 1) then
           noun = 'ZT1 card header'
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'ZT1 card '//string_ii2ss(izt1)

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      read (card,1000,iostat=status) ccc1,sss1,sss1a,lll1
1000  format (25x,a4,f11.0,f9.0,i9,22x,i10)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error decoding '//noun
           return
      end if
      err = DIO_OK
      msg = trim(noun)//' successfully read and decoded'
      return
      end subroutine geomio1_read_zt1_card


!!--------------------- geomio1 write zt1 card -------------------------------!!
!!--------------------- geomio1 write zt1 card -------------------------------!!
!!--------------------- geomio1 write zt1 card -------------------------------!!


      subroutine geomio1_write_zt1_card  (dio,izt1,err,msg,   &
                                          ccc1,sss1,sss1a,lll1)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: izt1                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(in)    :: ccc1                  ! arguments
      real               ,intent(in)    :: sss1,sss1a            ! arguments
      integer            ,intent(in)    :: lll1                  ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (izt1 == 1) then
           noun = 'ZT1 card header'
           card = ' ZERO SOURCES       '// &
                       '     code   FROM SP#   TO SP#     LINE#'
           call dio_write_card (dio,card)
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'ZT1 card '//string_ii2ss(izt1)

      write (card,1000,iostat=status) ccc1,sss1,sss1a,lll1
1000  format (25x,a4,f11.1,f9.1,i9,22x,i10)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//noun
           return
      end if
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      return
      end subroutine geomio1_write_zt1_card


!!--------------------- geomio1 read zt2 card -------------------------------!!
!!--------------------- geomio1 read zt2 card -------------------------------!!
!!--------------------- geomio1 read zt2 card -------------------------------!!


      subroutine geomio1_read_zt2_card  (dio,izt2,err,msg,   &
                                         ccc2,rrr2,rrr2a,lll2)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: izt2                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(out)   :: ccc2                  ! arguments
      real               ,intent(out)   :: rrr2,rrr2a            ! arguments
      integer            ,intent(out)   :: lll2                  ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (izt2 == 1) then
           noun = 'ZT2 card header'
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'ZT2 card '//string_ii2ss(izt2)

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      read (card,1000,iostat=status) ccc2,rrr2,rrr2a,lll2
1000  format (25x,a4,f11.0,f9.0,i9,22x,i10)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error decoding '//noun
           return
      end if
      err = DIO_OK
      msg = trim(noun)//' successfully read and decoded'
      return
      end subroutine geomio1_read_zt2_card


!!--------------------- geomio1 write zt2 card -------------------------------!!
!!--------------------- geomio1 write zt2 card -------------------------------!!
!!--------------------- geomio1 write zt2 card -------------------------------!!


      subroutine geomio1_write_zt2_card  (dio,izt2,err,msg,   &
                                          ccc2,rrr2,rrr2a,lll2)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: izt2                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(in)    :: ccc2                  ! arguments
      real               ,intent(in)    :: rrr2,rrr2a            ! arguments
      integer            ,intent(in)    :: lll2                  ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (izt2 == 1) then
           noun = 'ZT2 card header'
           card = ' ZERO RECEIVERS    '// &
                          '      code   FROM SP#   TO SP#     LINE#'
           call dio_write_card (dio,card)
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'ZT2 card '//string_ii2ss(izt2)

      write (card,1000,iostat=status) ccc2,rrr2,rrr2a,lll2
1000  format (25x,a4,f11.1,f9.1,i9,22x,i10)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//noun
           return
      end if
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      return
      end subroutine geomio1_write_zt2_card


!!--------------------- geomio1 read zt3 card -------------------------------!!
!!--------------------- geomio1 read zt3 card -------------------------------!!
!!--------------------- geomio1 read zt3 card -------------------------------!!


      subroutine geomio1_read_zt3_card  (dio,izt3,err,msg,   &
                                         ccc3,iggg3,iggg3a,ittt3,ittt3a)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: izt3                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(out)   :: ccc3                  ! arguments
      integer            ,intent(out)   :: iggg3,iggg3a          ! arguments
      integer            ,intent(out)   :: ittt3,ittt3a          ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (izt3 == 1) then
           noun = 'ZT3 card header'
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'ZT3 card '//string_ii2ss(izt3)

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      read (card,1000,iostat=status) ccc3,iggg3,iggg3a,ittt3,ittt3a
1000  format (25x,a4,i12,i11,i12,i10,6x,i10)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error decoding '//noun
           return
      end if
      err = DIO_OK
      msg = trim(noun)//' successfully read and decoded'
      return
      end subroutine geomio1_read_zt3_card


!!--------------------- geomio1 write zt3 card -------------------------------!!
!!--------------------- geomio1 write zt3 card -------------------------------!!
!!--------------------- geomio1 write zt3 card -------------------------------!!


      subroutine geomio1_write_zt3_card (dio,izt3,err,msg,   &
                                         ccc3,iggg3,iggg3a,ittt3,ittt3a)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: izt3                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(in)    :: ccc3                  ! arguments
      integer            ,intent(in)    :: iggg3,iggg3a          ! arguments
      integer            ,intent(in)    :: ittt3,ittt3a          ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (izt3 == 1) then
           noun = 'ZT3 card header'
           card = ' ZERO TRACES IN GROUPS   code   FROM GRP#  '//  &
                     '  TO GRP#        TRC#   TO TRC#'
           call dio_write_card (dio,card)
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'ZT3 card '//string_ii2ss(izt3)

      write (card,1000,iostat=status) ccc3,iggg3,iggg3a,ittt3,ittt3a
1000  format (25x,a4,i12,i11,i12,i10,6x,i10)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//noun
           return
      end if
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      return
      end subroutine geomio1_write_zt3_card


!!--------------------- geomio1 read zt4 card -------------------------------!!
!!--------------------- geomio1 read zt4 card -------------------------------!!
!!--------------------- geomio1 read zt4 card -------------------------------!!


      subroutine geomio1_read_zt4_card  (dio,izt4,err,msg,   &
                                         ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: izt4                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(out)   :: ccc4                  ! arguments
      real               ,intent(out)   :: sss4,sss4a            ! arguments
      real               ,intent(out)   :: rrr4,rrr4a            ! arguments
      integer            ,intent(out)   :: lll4,lll4a            ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (izt4 == 1) then
           noun = 'ZT4 card header'
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'ZT4 card '//string_ii2ss(izt4)

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg,noun)
      if (err /= DIO_OK) return

      read (card,1000,iostat=status) ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a
1000  format (2x,a4,f22.0,f9.0,i6,f12.0,f9.0,i6,i20)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error decoding '//noun
           return
      end if
      err = DIO_OK
      msg = trim(noun)//' successfully read and decoded'
      return
      end subroutine geomio1_read_zt4_card


!!--------------------- geomio1 write zt4 card -------------------------------!!
!!--------------------- geomio1 write zt4 card -------------------------------!!
!!--------------------- geomio1 write zt4 card -------------------------------!!


      subroutine geomio1_write_zt4_card (dio,izt4,err,msg,   &
                                         ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      implicit none
      type(dio_struct)   ,intent(inout) :: dio                   ! arguments
      integer            ,intent(in)    :: izt4                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(in)    :: ccc4                  ! arguments
      real               ,intent(in)    :: sss4,sss4a            ! arguments
      real               ,intent(in)    :: rrr4,rrr4a            ! arguments
      integer            ,intent(in)    :: lll4,lll4a            ! arguments
      character(len=20)                 :: noun                  ! local
      integer                           :: status                ! local
      character(len=200)                :: card                  ! local

      if (izt4 == 1) then
           noun = 'ZT4 card header'
           card = '  cODe       from SOURCE sp#   to sp#  line# '//  &
                      '   REC sp#   to sp#  line#'
           call dio_write_card (dio,card)
           call dio_status     (dio,err,msg,noun)
           if (err /= DIO_OK) return
      end if
      noun = 'ZT4 card '//string_ii2ss(izt4)

      write (card,1000,iostat=status) ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a
1000  format (2x,a4,f22.1,f9.1,i6,f12.1,f9.1,i6,i20)
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//noun
           return
      end if
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg,noun)
      return
      end subroutine geomio1_write_zt4_card


            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!
            !!--------- encoding and decoding for oldcps ---------!!


!!-------------------------- geomio1 cc2ii ---------------------------------!!
!!-------------------------- geomio1 cc2ii ---------------------------------!!
!!-------------------------- geomio1 cc2ii ---------------------------------!!

!!!!! this routine sets err and msg only if an error occurs.


      subroutine geomio1_cc2ii (cvar,ivar,noun,err,msg)
      implicit none
      character(len=*),intent(in)  :: cvar                ! arguments
      integer         ,intent(out) :: ivar                ! arguments
      character(len=*),intent(in)  :: noun                ! arguments
      integer         ,intent(out) :: err                 ! arguments
      character(len=*),intent(out) :: msg                 ! arguments
      integer                      :: status              ! local

      ivar = string_ss2ii (cvar,status) 
      if (status < 0) then
           err = DIO_ERROR
           msg = 'error decoding '//trim(noun)//' value '//  &
                                                    trim(cvar)//' to integer'
      end if
      return
      end subroutine geomio1_cc2ii


!!-------------------------- geomio1 ii2cc ---------------------------------!!
!!-------------------------- geomio1 ii2cc ---------------------------------!!
!!-------------------------- geomio1 ii2cc ---------------------------------!!

!!!!! this routine sets err and msg only if an error occurs.


      subroutine geomio1_ii2cc (ivar,cvar,fmt,noun,err,msg)
      implicit none
      integer         ,intent(in)  :: ivar                ! arguments
      character(len=*),intent(out) :: cvar                ! arguments
      character(len=*),intent(in)  :: fmt                 ! arguments
      character(len=*),intent(in)  :: noun                ! arguments
      integer         ,intent(out) :: err                 ! arguments
      character(len=*),intent(out) :: msg                 ! arguments
      integer                      :: status              ! local

      if (ivar == INIL) then
           cvar = ' '
           return
      end if
      write (cvar,fmt,iostat = status) ivar
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//trim(noun)//' value from integer'
      end if
      return
      end subroutine geomio1_ii2cc


!!-------------------------- geomio1 cc2ff ---------------------------------!!
!!-------------------------- geomio1 cc2ff ---------------------------------!!
!!-------------------------- geomio1 cc2ff ---------------------------------!!

!!!!! this routine sets err and msg only if an error occurs.


      subroutine geomio1_cc2ff (cvar,fvar,noun,err,msg)
      implicit none
      character(len=*),intent(in)  :: cvar                ! arguments
      real            ,intent(out) :: fvar                ! arguments
      character(len=*),intent(in)  :: noun                ! arguments
      integer         ,intent(out) :: err                 ! arguments
      character(len=*),intent(out) :: msg                 ! arguments
      integer                      :: status              ! local

      fvar = string_ss2ff (cvar,status) 
      if (status < 0) then
           err = DIO_ERROR
           msg = 'error decoding '//trim(noun)//' value '//  &
                                                    trim(cvar)//' to real'
      end if
      return
      end subroutine geomio1_cc2ff


!!-------------------------- geomio1 ff2cc ---------------------------------!!
!!-------------------------- geomio1 ff2cc ---------------------------------!!
!!-------------------------- geomio1 ff2cc ---------------------------------!!

!!!!! this routine sets err and msg only if an error occurs.


      subroutine geomio1_ff2cc (fvar,cvar,fmt,noun,err,msg)
      implicit none
      real            ,intent(in)  :: fvar                  ! arguments
      character(len=*),intent(out) :: cvar                  ! arguments
      character(len=*),intent(in)  :: fmt                   ! arguments
      character(len=*),intent(in)  :: noun                  ! arguments
      integer         ,intent(out) :: err                   ! arguments
      character(len=*),intent(out) :: msg                   ! arguments
      integer                      :: status                ! local

      if (fvar == FNIL) then
           cvar = ' '
           return
      end if
      if (index(fmt,'i') > 0) then
           write (cvar,fmt,iostat = status) nint(fvar)
      else
           write (cvar,fmt,iostat = status) fvar
      end if
      if (status /= 0) then
           err = DIO_ERROR
           msg = 'error encoding '//trim(noun)//' value from real'
      end if
      return
      end subroutine geomio1_ff2cc


!!------------------------- end of module ---------------------------------!!
!!------------------------- end of module ---------------------------------!!
!!------------------------- end of module ---------------------------------!!
 

      end module geomio1_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
