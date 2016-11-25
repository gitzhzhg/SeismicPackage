!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- geomio2.f90 --------------------------------!!
!!------------------------------- geomio2.f90 --------------------------------!!
!!------------------------------- geomio2.f90 --------------------------------!!


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
! Name       : GEOMIO2
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2002-05-20   by: Tom Stoeckley
! Maturity   : production   2002-06-24
! Purpose    : To read and write a geometry section in a self-defining file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading and writing a single field geometry header
! section and data section in a self-defining file which might contain various
! sections with the same or different file types.
!
! This primitive does not open or close the file.
! This primitive only reads and writes self defining ascii field geometry files.
! This primitive cannot read or write self defining binary field geometry files.
! This primitive cannot read or write old-style CPS field geometry files.
! The calling program must use the PJAR and FIO primitives with this primitive.
! See documentation in PJAR and FIO for details.
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
!                       CREATE AND DELETE THE OBJECT               
!
!                              i   b   b      i
!    call geomio2_create     (obj,fio,pjar,secname)
!    call geomio2_delete     (obj)
!                              b
!
!                        ++++++++++++++++++++++++++
!
! type(geomio2_struct) obj      = the GEOMIO2 data structure.
! type(fio_struct)     fio      = reference to the FIO data structure to use.
! type(pjar_struct)    pjar     = reference to the PJAR data structure to use.
! char(*)              secname  = name of main header section.
! real                 ve       = reference velocity.
! real                 datum    = datum elevation.
! real                 fixdist  = uniform inline distance parameter.
! type(grid_struct)    grid     = grid transformation structure.
! char(*)              chaining = chaining flag.
! integer              nld      = number of LD cards.
! integer              nrp      = number of RP cards.
! integer              npp      = number of PP cards.
! integer              nzt1     = number of ZT1 cards.
! integer              nzt2     = number of ZT2 cards.
! integer              nzt3     = number of ZT3 cards.
! integer              nzt4     = number of ZT4 cards.
!
!!CARDTYPE should be set to GEOMIO2_LD, GEOMIO2_RP, GEOMIO2_PP, GEOMIO2_ZT1,
!!GEOMIO2_ZT2, GEOMIO2_ZT3, or GEOMIO2_ZT4.
!
!-------------------------------------------------------------------------------
!                      READ AND WRITE INDIVIDUAL CARDS
!
!      (must call for each LD  card with card number = 1,nld  consecutively)
!      (must call for each RP  card with card number = 1,nrp  consecutively)
!      (must call for each PP  card with card number = 1,npp  consecutively)
!      (must call for each ZT1 card with card number = 1,nzt1 consecutively)
!      (must call for each ZT2 card with card number = 1,nzt2 consecutively)
!      (must call for each ZT3 card with card number = 1,nzt3 consecutively)
!      (must call for each ZT4 card with card number = 1,nzt4 consecutively)
!          (the data sections must be read or written in the above order)
!
! Read or write one LD card:
!
!                                    b   o   o
!       call geomio2_read_ld_card  (obj,err,msg,
!                sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
!                o   o    o    o    o     o    o  o  o   o   o   o    o
!
!                                    b   o   o
!       call geomio2_write_ld_card (obj,err,msg,
!                sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
!                i   i    i    i    i     i    i  i  i   i   i   i    i
!
!
! Read or write one RP card:
!
!                                    b   o   o
!       call geomio2_read_rp_card  (obj,err,msg,
!                ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
!                  o    o    o    o   o    o   o    o    o    o     o
!
!                                    b   o   o
!       call geomio2_write_rp_card (obj,err,msg,
!                ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
!                  i    i    i    i   i    i   i    i    i    i     i
!
!
! Read or write one PP card:
!
!                                    b   o   o
!       call geomio2_read_pp_card  (obj,err,msg,
!          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
!           o    o    o    o     o    o    o    o     o     o     o    o  o  o
!
!                                    b   o   o
!       call geomio2_write_pp_card (obj,err,msg,
!          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
!           i    i    i    i     i    i    i    i     i     i     i    i  i  i
!
!
! Read or write one ZT1 card:
!
!                                     b   o   o
!       call geomio2_read_zt1_card  (obj,err,msg,
!                                    ccc1,sss1,sss1a,lll1)
!                                     o    o     o    o
!
!                                     b   o   o
!       call geomio2_write_zt1_card (obj,err,msg,
!                                    ccc1,sss1,sss1a,lll1)
!                                     i    i     i    i
!
!
! Read or write one ZT2 card:
!
!                                     b   o   o
!       call geomio2_read_zt2_card  (obj,err,msg,
!                                    ccc2,rrr2,rrr2a,lll2)
!                                     o    o     o    o
!
!                                     b   o   o
!       call geomio2_write_zt2_card (obj,err,msg,
!                                    ccc2,rrr2,rrr2a,lll2)
!                                     i    i     i    i
!
!
! Read or write one ZT3 card:
!
!                                     b   o   o
!       call geomio2_read_zt3_card  (obj,err,msg,
!                                    ccc3,iggg3,iggg3a,ittt3,ittt3a)
!                                     o     o     o      o     o
!
!                                     b   o   o
!       call geomio2_write_zt3_card (obj,err,msg,
!                                    ccc3,iggg3,iggg3a,ittt3,ittt3a)
!                                     i     i     i      i     i
!
!
! Read or write one ZT4 card:
!
!                                     b   o   o
!       call geomio2_read_zt4_card  (obj,err,msg,
!                                    ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
!                                     o    o     o    o    o     o     o
!
!                                     b   o   o
!       call geomio2_write_zt4_card (obj,err,msg,
!                                    ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
!                                     i    i     i    i    i     i     i
!
!                        ++++++++++++++++++++++++++
!
! type(geomio2_struct)    obj  = the GEOMIO2 data structure.
! integer                 err  = error flag (returned).
! char(*)                 msg  = message for possible printing (returned).
!
! See documentation in the GEOMIO primitive for the definitions of the
! rest of the arguments to the routines for reading and writing individual
! field geometry cards.
!
! ERR will be set to FIO_OK or FIO_ERROR or FIO_EOF.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2002-06-24  Stoeckley  Add check for blank section names to support CFG.
!  2. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR and FIO primitives.
!  1. 2000-11-27  Stoeckley  Initial version moved from portions of code in
!                             the GEOMIO primitive.
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
 
 
      module geomio2_module
      use fio_module
      use pjar_module
      use named_constants_module
      use grid_module
      implicit none
      public

      character(len=100),public,save :: GEOMIO2_IDENT = &
       '$Id: geomio2.f90,v 1.3 2002/06/20 13:04:16 Stoeckley prod sps $'

      type,public :: geomio2_struct

         private
         type(fio_struct) ,pointer :: fio

         integer :: LD_sp        ! shotpoint.
         integer :: LD_dist      ! distance to next flag.
         integer :: LD_xloc      ! surveyed X location.
         integer :: LD_yloc      ! surveyed Y location.
         integer :: LD_elev      ! default elevation.
         integer :: LD_depth     ! default hole depth.
         integer :: LD_tuh       ! default uphole time.
         integer :: LD_tr        ! receiver static.
         integer :: LD_ts        ! source static.
         integer :: LD_xsd       ! inline skid.
         integer :: LD_ysd       ! crossline skid.
         integer :: LD_elsd      ! elevation skid.
         integer :: LD_line      ! line number.

         integer :: RP_ipat1     ! pattern number.
         integer :: RP_flag      ! flag.
         integer :: RP_sp1       ! first receiver shotpoint.
         integer :: RP_line1     ! first receiver line number.
         integer :: RP_nx        ! number of recvr flags along line.
         integer :: RP_ixinc     ! receiver flag increment along line.
         integer :: RP_ny        ! number of recvr flags across lines.
         integer :: RP_iyinc     ! recvr flag increment across lines.
         integer :: RP_xsd1      ! receiver pattern inline skid.
         integer :: RP_ysd1      ! receiver pattern crossline skid.
         integer :: RP_elsd1     ! receiver pattern elevation skid.

         integer :: PP_sp2       ! source shotpoint.
         integer :: PP_line2     ! source line number.
         integer :: PP_sp3       ! first receiver shotpoint.
         integer :: PP_line3     ! first receiver line number.
         integer :: PP_ipat2     ! pattern number.
         integer :: PP_xsd2      ! source inline skid.
         integer :: PP_ysd2      ! source crossline skid.
         integer :: PP_hold      ! how many groups to hold skid.
         integer :: PP_elev2     ! new source elevation.
         integer :: PP_depth2    ! new hole depth.
         integer :: PP_tuh2      ! new uphole time.
         integer :: PP_is        ! flag moveup to next sources.
         integer :: PP_ir        ! flag moveup to next first recs.
         integer :: PP_ig        ! group number (shot profile number).

         integer :: ZT1_ccc1     ! code.
         integer :: ZT1_sss1     ! first source shotpoint affected.
         integer :: ZT1_sss1a    ! last source shotpoint affected.
         integer :: ZT1_lll1     ! source line number.

         integer :: ZT2_ccc2     ! code.
         integer :: ZT2_rrr2     ! first receiver shotpoint affected.
         integer :: ZT2_rrr2a    ! last receiver shotpoint affected.
         integer :: ZT2_lll2     ! receiver line number.

         integer :: ZT3_ccc3     ! code.
         integer :: ZT3_iggg3    ! first group number affected.
         integer :: ZT3_iggg3a   ! last group number affected.
         integer :: ZT3_ittt3    ! first trace number affected.
         integer :: ZT3_ittt3a   ! last trace number affected.

         integer :: ZT4_ccc4     ! code.
         integer :: ZT4_sss4     ! first source shotpoint affected.
         integer :: ZT4_sss4a    ! last source shotpoint affected.
         integer :: ZT4_lll4     ! source line number.
         integer :: ZT4_rrr4     ! first receiver shotpoint affected.
         integer :: ZT4_rrr4a    ! last receiver shotpoint affected.
         integer :: ZT4_lll4a    ! receiver line number.

      end type geomio2_struct


      contains


!!--------------------------- geomio2 delete -----------------------------!!
!!--------------------------- geomio2 delete -----------------------------!!
!!--------------------------- geomio2 delete -----------------------------!!


      subroutine geomio2_delete (obj)
      implicit none
      type(geomio2_struct),pointer :: obj             ! arguments

      if (associated(obj)) deallocate(obj)
      return
      end subroutine geomio2_delete


!!-------------------------- geomio2 create -----------------------------!!
!!-------------------------- geomio2 create -----------------------------!!
!!-------------------------- geomio2 create -----------------------------!!


      subroutine geomio2_create (obj,fio,pjar,secname)
      implicit none
      type(geomio2_struct),pointer              :: obj           ! arguments
      type(fio_struct)    ,intent(inout),target :: fio           ! arguments
      type(pjar_struct)   ,intent(inout),target :: pjar          ! arguments
      character(len=*)    ,intent(in)           :: secname       ! arguments
      character(len=12)                         :: ld_secname    ! local
      character(len=12)                         :: rp_secname    ! local
      character(len=12)                         :: pp_secname    ! local
      character(len=12)                         :: zt1_secname   ! local
      character(len=12)                         :: zt2_secname   ! local
      character(len=12)                         :: zt3_secname   ! local
      character(len=12)                         :: zt4_secname   ! local

      allocate (obj)

      obj%fio  => fio

      call pjar_choose_section (pjar,secname)

      call pjar_get (pjar, 'ld_secname' , ld_secname)
      call pjar_get (pjar, 'rp_secname' , rp_secname)
      call pjar_get (pjar, 'pp_secname' , pp_secname)
      call pjar_get (pjar, 'zt1_secname', zt1_secname)
      call pjar_get (pjar, 'zt2_secname', zt2_secname)
      call pjar_get (pjar, 'zt3_secname', zt3_secname)
      call pjar_get (pjar, 'zt4_secname', zt4_secname)

      if (ld_secname /= ' ') then
           call pjar_choose_section (pjar,ld_secname)

           obj%ld_sp      = pjar_find (pjar, 'fields', 'sp     ')
           obj%ld_dist    = pjar_find (pjar, 'fields', 'dist   ')
           obj%ld_xloc    = pjar_find (pjar, 'fields', 'xloc   ')
           obj%ld_yloc    = pjar_find (pjar, 'fields', 'yloc   ')
           obj%ld_elev    = pjar_find (pjar, 'fields', 'elev   ')
           obj%ld_depth   = pjar_find (pjar, 'fields', 'depth  ')
           obj%ld_tuh     = pjar_find (pjar, 'fields', 'tuh    ')
           obj%ld_tr      = pjar_find (pjar, 'fields', 'tr     ')
           obj%ld_ts      = pjar_find (pjar, 'fields', 'ts     ')
           obj%ld_xsd     = pjar_find (pjar, 'fields', 'xsd    ')
           obj%ld_ysd     = pjar_find (pjar, 'fields', 'ysd    ')
           obj%ld_elsd    = pjar_find (pjar, 'fields', 'elsd   ')
           obj%ld_line    = pjar_find (pjar, 'fields', 'line   ')
      else
           obj%ld_sp      = 0
           obj%ld_dist    = 0
           obj%ld_xloc    = 0
           obj%ld_yloc    = 0
           obj%ld_elev    = 0
           obj%ld_depth   = 0
           obj%ld_tuh     = 0
           obj%ld_tr      = 0
           obj%ld_ts      = 0
           obj%ld_xsd     = 0
           obj%ld_ysd     = 0
           obj%ld_elsd    = 0
           obj%ld_line    = 0
      end if

      if (rp_secname /= ' ') then
           call pjar_choose_section (pjar,rp_secname)

           obj%rp_ipat1   = pjar_find (pjar, 'fields', 'ipat1  ')
           obj%rp_flag    = pjar_find (pjar, 'fields', 'flag   ')
           obj%rp_sp1     = pjar_find (pjar, 'fields', 'sp1    ')
           obj%rp_line1   = pjar_find (pjar, 'fields', 'line1  ')
           obj%rp_nx      = pjar_find (pjar, 'fields', 'nx     ')
           obj%rp_ixinc   = pjar_find (pjar, 'fields', 'ixinc  ')
           obj%rp_ny      = pjar_find (pjar, 'fields', 'ny     ')
           obj%rp_iyinc   = pjar_find (pjar, 'fields', 'iyinc  ')
           obj%rp_xsd1    = pjar_find (pjar, 'fields', 'xsd1   ')
           obj%rp_ysd1    = pjar_find (pjar, 'fields', 'ysd1   ')
           obj%rp_elsd1   = pjar_find (pjar, 'fields', 'elsd1  ')
      else
           obj%rp_ipat1   = 0
           obj%rp_flag    = 0
           obj%rp_sp1     = 0
           obj%rp_line1   = 0
           obj%rp_nx      = 0
           obj%rp_ixinc   = 0
           obj%rp_ny      = 0
           obj%rp_iyinc   = 0
           obj%rp_xsd1    = 0
           obj%rp_ysd1    = 0
           obj%rp_elsd1   = 0
      end if

      if (pp_secname /= ' ') then
           call pjar_choose_section (pjar,pp_secname)

           obj%pp_sp2     = pjar_find (pjar, 'fields', 'sp2    ')
           obj%pp_line2   = pjar_find (pjar, 'fields', 'line2  ')
           obj%pp_sp3     = pjar_find (pjar, 'fields', 'sp3    ')
           obj%pp_line3   = pjar_find (pjar, 'fields', 'line3  ')
           obj%pp_ipat2   = pjar_find (pjar, 'fields', 'ipat2  ')
           obj%pp_xsd2    = pjar_find (pjar, 'fields', 'xsd2   ')
           obj%pp_ysd2    = pjar_find (pjar, 'fields', 'ysd2   ')
           obj%pp_hold    = pjar_find (pjar, 'fields', 'hold   ')
           obj%pp_elev2   = pjar_find (pjar, 'fields', 'elev2  ')
           obj%pp_depth2  = pjar_find (pjar, 'fields', 'depth2 ')
           obj%pp_tuh2    = pjar_find (pjar, 'fields', 'tuh2   ')
           obj%pp_is      = pjar_find (pjar, 'fields', 'is     ')
           obj%pp_ir      = pjar_find (pjar, 'fields', 'ir     ')
           obj%pp_ig      = pjar_find (pjar, 'fields', 'ig     ')
      else
           obj%pp_sp2     = 0
           obj%pp_line2   = 0
           obj%pp_sp3     = 0
           obj%pp_line3   = 0
           obj%pp_ipat2   = 0
           obj%pp_xsd2    = 0
           obj%pp_ysd2    = 0
           obj%pp_hold    = 0
           obj%pp_elev2   = 0
           obj%pp_depth2  = 0
           obj%pp_tuh2    = 0
           obj%pp_is      = 0
           obj%pp_ir      = 0
           obj%pp_ig      = 0
      end if

      if (zt1_secname /= ' ') then
           call pjar_choose_section (pjar,zt1_secname)

           obj%zt1_ccc1   = pjar_find (pjar, 'fields', 'ccc1   ')
           obj%zt1_sss1   = pjar_find (pjar, 'fields', 'sss1   ')
           obj%zt1_sss1a  = pjar_find (pjar, 'fields', 'sss1a  ')
           obj%zt1_lll1   = pjar_find (pjar, 'fields', 'lll1   ')
      else
           obj%zt1_ccc1   = 0
           obj%zt1_sss1   = 0
           obj%zt1_sss1a  = 0
           obj%zt1_lll1   = 0
      end if

      if (zt2_secname /= ' ') then
           call pjar_choose_section (pjar,zt2_secname)

           obj%zt2_ccc2   = pjar_find (pjar, 'fields', 'ccc2   ')
           obj%zt2_rrr2   = pjar_find (pjar, 'fields', 'rrr2   ')
           obj%zt2_rrr2a  = pjar_find (pjar, 'fields', 'rrr2a  ')
           obj%zt2_lll2   = pjar_find (pjar, 'fields', 'lll2   ')
      else
           obj%zt2_ccc2   = 0
           obj%zt2_rrr2   = 0
           obj%zt2_rrr2a  = 0
           obj%zt2_lll2   = 0
      end if

      if (zt3_secname /= ' ') then
           call pjar_choose_section (pjar,zt3_secname)

           obj%zt3_ccc3   = pjar_find (pjar, 'fields', 'ccc3   ')
           obj%zt3_iggg3  = pjar_find (pjar, 'fields', 'iggg3  ')
           obj%zt3_iggg3a = pjar_find (pjar, 'fields', 'iggg3a ')
           obj%zt3_ittt3  = pjar_find (pjar, 'fields', 'ittt3  ')
           obj%zt3_ittt3a = pjar_find (pjar, 'fields', 'ittt3a ')
      else
           obj%zt3_ccc3   = 0
           obj%zt3_iggg3  = 0
           obj%zt3_iggg3a = 0
           obj%zt3_ittt3  = 0
           obj%zt3_ittt3a = 0
      end if

      if (zt4_secname /= ' ') then
           call pjar_choose_section (pjar,zt4_secname)

           obj%zt4_ccc4   = pjar_find (pjar, 'fields', 'ccc4   ')
           obj%zt4_sss4   = pjar_find (pjar, 'fields', 'sss4   ')
           obj%zt4_sss4a  = pjar_find (pjar, 'fields', 'sss4a  ')
           obj%zt4_lll4   = pjar_find (pjar, 'fields', 'lll4   ')
           obj%zt4_rrr4   = pjar_find (pjar, 'fields', 'rrr4   ')
           obj%zt4_rrr4a  = pjar_find (pjar, 'fields', 'rrr4a  ')
           obj%zt4_lll4a  = pjar_find (pjar, 'fields', 'lll4a  ')
      else
           obj%zt4_ccc4   = 0
           obj%zt4_sss4   = 0
           obj%zt4_sss4a  = 0
           obj%zt4_lll4   = 0
           obj%zt4_rrr4   = 0
           obj%zt4_rrr4a  = 0
           obj%zt4_lll4a  = 0
      end if
      return
      end subroutine geomio2_create


!!--------------------- geomio2 read ld card -------------------------------!!
!!--------------------- geomio2 read ld card -------------------------------!!
!!--------------------- geomio2 read ld card -------------------------------!!


      subroutine geomio2_read_ld_card  (obj,err,msg,   &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      real   ,intent(out)   :: sp,dist,xloc,yloc,elev             ! arguments
      real   ,intent(out)   :: depth,tuh,tr,ts,xsd,ysd,elsd       ! arguments
      integer,intent(out)   :: line                               ! arguments

      call fio_before_read_line (obj%fio)

      call fio_read_line (obj%fio, obj%LD_sp      ,sp   )
      call fio_read_line (obj%fio, obj%LD_dist    ,dist )
      call fio_read_line (obj%fio, obj%LD_xloc    ,xloc )
      call fio_read_line (obj%fio, obj%LD_yloc    ,yloc )
      call fio_read_line (obj%fio, obj%LD_elev    ,elev )
      call fio_read_line (obj%fio, obj%LD_depth   ,depth)
      call fio_read_line (obj%fio, obj%LD_tuh     ,tuh  )
      call fio_read_line (obj%fio, obj%LD_tr      ,tr   )
      call fio_read_line (obj%fio, obj%LD_ts      ,ts   )
      call fio_read_line (obj%fio, obj%LD_xsd     ,xsd  )
      call fio_read_line (obj%fio, obj%LD_ysd     ,ysd  )
      call fio_read_line (obj%fio, obj%LD_elsd    ,elsd )
      call fio_read_line (obj%fio, obj%LD_line    ,line )

      call fio_after_read_line (obj%fio,err,msg)
      return
      end subroutine geomio2_read_ld_card


!!--------------------- geomio2 write ld card -------------------------------!!
!!--------------------- geomio2 write ld card -------------------------------!!
!!--------------------- geomio2 write ld card -------------------------------!!


      subroutine geomio2_write_ld_card  (obj,err,msg,   &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      real   ,intent(in)    :: sp,dist,xloc,yloc,elev             ! arguments
      real   ,intent(in)    :: depth,tuh,tr,ts,xsd,ysd,elsd       ! arguments
      integer,intent(in)    :: line                               ! arguments

      call fio_before_write_line (obj%fio)

      call fio_write_line (obj%fio, obj%LD_sp      ,sp   )
      call fio_write_line (obj%fio, obj%LD_dist    ,dist )
      call fio_write_line (obj%fio, obj%LD_xloc    ,xloc,ndec=0)
      call fio_write_line (obj%fio, obj%LD_yloc    ,yloc,ndec=0)
      call fio_write_line (obj%fio, obj%LD_elev    ,elev )
      call fio_write_line (obj%fio, obj%LD_depth   ,depth)
      call fio_write_line (obj%fio, obj%LD_tuh     ,tuh  )
      call fio_write_line (obj%fio, obj%LD_tr      ,tr   )
      call fio_write_line (obj%fio, obj%LD_ts      ,ts   )
      call fio_write_line (obj%fio, obj%LD_xsd     ,xsd  )
      call fio_write_line (obj%fio, obj%LD_ysd     ,ysd  )
      call fio_write_line (obj%fio, obj%LD_elsd    ,elsd )
      call fio_write_line (obj%fio, obj%LD_line    ,line )

      call fio_after_write_line (obj%fio,err,msg)
      return
      end subroutine geomio2_write_ld_card


!!--------------------- geomio2 read rp card -------------------------------!!
!!--------------------- geomio2 read rp card -------------------------------!!
!!--------------------- geomio2 read rp card -------------------------------!!


      subroutine geomio2_read_rp_card  (obj,err,msg,   &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      integer             ,intent(out)   :: ipat1,line1           ! arguments
      integer             ,intent(out)   :: nx,ixinc,ny,iyinc     ! arguments
      real                ,intent(out)   :: sp1,xsd1,ysd1,elsd1   ! arguments
      character(len=*)    ,intent(out)   :: flag                  ! arguments

      call fio_before_read_line (obj%fio)

      call fio_read_line (obj%fio, obj%RP_ipat1  , ipat1  )
      call fio_read_line (obj%fio, obj%RP_flag   , flag   )
      call fio_read_line (obj%fio, obj%RP_sp1    , sp1    )
      call fio_read_line (obj%fio, obj%RP_line1  , line1  )
      call fio_read_line (obj%fio, obj%RP_nx     , nx     )
      call fio_read_line (obj%fio, obj%RP_ixinc  , ixinc  )
      call fio_read_line (obj%fio, obj%RP_ny     , ny     )
      call fio_read_line (obj%fio, obj%RP_iyinc  , iyinc  )
      call fio_read_line (obj%fio, obj%RP_xsd1   , xsd1   )
      call fio_read_line (obj%fio, obj%RP_ysd1   , ysd1   )
      call fio_read_line (obj%fio, obj%RP_elsd1  , elsd1  )

      call fio_after_read_line (obj%fio,err,msg)
      return
      end subroutine geomio2_read_rp_card


!!--------------------- geomio2 write rp card -------------------------------!!
!!--------------------- geomio2 write rp card -------------------------------!!
!!--------------------- geomio2 write rp card -------------------------------!!


      subroutine geomio2_write_rp_card  (obj,err,msg,   &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      integer             ,intent(in)    :: ipat1,line1           ! arguments
      integer             ,intent(in)    :: nx,ixinc,ny,iyinc     ! arguments
      real                ,intent(in)    :: sp1,xsd1,ysd1,elsd1   ! arguments
      character(len=*)    ,intent(in)    :: flag                  ! arguments

      call fio_before_write_line (obj%fio)

      call fio_write_line (obj%fio, obj%RP_ipat1  , ipat1  )
      call fio_write_line (obj%fio, obj%RP_flag   , flag   )
      call fio_write_line (obj%fio, obj%RP_sp1    , sp1    )
      call fio_write_line (obj%fio, obj%RP_line1  , line1  )
      call fio_write_line (obj%fio, obj%RP_nx     , nx     )
      call fio_write_line (obj%fio, obj%RP_ixinc  , ixinc  )
      call fio_write_line (obj%fio, obj%RP_ny     , ny     )
      call fio_write_line (obj%fio, obj%RP_iyinc  , iyinc  )
      call fio_write_line (obj%fio, obj%RP_xsd1   , xsd1   )
      call fio_write_line (obj%fio, obj%RP_ysd1   , ysd1   )
      call fio_write_line (obj%fio, obj%RP_elsd1  , elsd1  )

      call fio_after_write_line (obj%fio,err,msg)
      return
      end subroutine geomio2_write_rp_card


!!--------------------- geomio2 read pp card -------------------------------!!
!!--------------------- geomio2 read pp card -------------------------------!!
!!--------------------- geomio2 read pp card -------------------------------!!


      subroutine geomio2_read_pp_card  (obj,err,msg,   &
           sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      real                ,intent(out)   :: sp2,sp3,xsd2,ysd2     ! arguments
      integer             ,intent(out)   :: line2,line3,ipat2     ! arguments
      real                ,intent(out)   :: elev2,depth2,tuh2     ! arguments
      integer             ,intent(out)   :: hold,is,ir,ig         ! arguments

      call fio_before_read_line (obj%fio)

      call fio_read_line (obj%fio, obj%PP_sp2    , sp2    )
      call fio_read_line (obj%fio, obj%PP_line2  , line2  )
      call fio_read_line (obj%fio, obj%PP_sp3    , sp3    )
      call fio_read_line (obj%fio, obj%PP_line3  , line3  )
      call fio_read_line (obj%fio, obj%PP_ipat2  , ipat2  )
      call fio_read_line (obj%fio, obj%PP_xsd2   , xsd2   )
      call fio_read_line (obj%fio, obj%PP_ysd2   , ysd2   )
      call fio_read_line (obj%fio, obj%PP_hold   , hold   )
      call fio_read_line (obj%fio, obj%PP_elev2  , elev2  )
      call fio_read_line (obj%fio, obj%PP_depth2 , depth2 )
      call fio_read_line (obj%fio, obj%PP_tuh2   , tuh2   )
      call fio_read_line (obj%fio, obj%PP_is     , is     )
      call fio_read_line (obj%fio, obj%PP_ir     , ir     )
      call fio_read_line (obj%fio, obj%PP_ig     , ig     )

      call fio_after_read_line (obj%fio,err,msg)
      return
      end subroutine geomio2_read_pp_card


!!--------------------- geomio2 write pp card -------------------------------!!
!!--------------------- geomio2 write pp card -------------------------------!!
!!--------------------- geomio2 write pp card -------------------------------!!


      subroutine geomio2_write_pp_card  (obj,err,msg,   &
           sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      real                ,intent(in)    :: sp2,sp3,xsd2,ysd2     ! arguments
      integer             ,intent(in)    :: line2,line3,ipat2     ! arguments
      real                ,intent(in)    :: elev2,depth2,tuh2     ! arguments
      integer             ,intent(in)    :: hold,is,ir,ig         ! arguments

      call fio_before_write_line (obj%fio)

      call fio_write_line (obj%fio, obj%PP_sp2    , sp2    )
      call fio_write_line (obj%fio, obj%PP_line2  , line2  )
      call fio_write_line (obj%fio, obj%PP_sp3    , sp3    )
      call fio_write_line (obj%fio, obj%PP_line3  , line3  )
      call fio_write_line (obj%fio, obj%PP_ipat2  , ipat2  )
      call fio_write_line (obj%fio, obj%PP_xsd2   , xsd2   )
      call fio_write_line (obj%fio, obj%PP_ysd2   , ysd2   )
      call fio_write_line (obj%fio, obj%PP_hold   , hold   )
      call fio_write_line (obj%fio, obj%PP_elev2  , elev2  )
      call fio_write_line (obj%fio, obj%PP_depth2 , depth2 )
      call fio_write_line (obj%fio, obj%PP_tuh2   , tuh2   )
      call fio_write_line (obj%fio, obj%PP_is     , is     )
      call fio_write_line (obj%fio, obj%PP_ir     , ir     )
      call fio_write_line (obj%fio, obj%PP_ig     , ig     )

      call fio_after_write_line (obj%fio,err,msg)
      return
      end subroutine geomio2_write_pp_card


!!--------------------- geomio2 read zt1 card -------------------------------!!
!!--------------------- geomio2 read zt1 card -------------------------------!!
!!--------------------- geomio2 read zt1 card -------------------------------!!


      subroutine geomio2_read_zt1_card  (obj,err,msg,   &
                                         ccc1,sss1,sss1a,lll1)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=*)    ,intent(out)   :: ccc1                  ! arguments
      real                ,intent(out)   :: sss1,sss1a            ! arguments
      integer             ,intent(out)   :: lll1                  ! arguments

      call fio_before_read_line (obj%fio)

      call fio_read_line (obj%fio, obj%ZT1_ccc1   , ccc1   )
      call fio_read_line (obj%fio, obj%ZT1_sss1   , sss1   )
      call fio_read_line (obj%fio, obj%ZT1_sss1a  , sss1a  )
      call fio_read_line (obj%fio, obj%ZT1_lll1   , lll1   )

      call fio_after_read_line (obj%fio,err,msg)
      return
      end subroutine geomio2_read_zt1_card


!!--------------------- geomio2 write zt1 card -------------------------------!!
!!--------------------- geomio2 write zt1 card -------------------------------!!
!!--------------------- geomio2 write zt1 card -------------------------------!!


      subroutine geomio2_write_zt1_card  (obj,err,msg,   &
                                          ccc1,sss1,sss1a,lll1)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=*)    ,intent(in)    :: ccc1                  ! arguments
      real                ,intent(in)    :: sss1,sss1a            ! arguments
      integer             ,intent(in)    :: lll1                  ! arguments

      call fio_before_write_line (obj%fio)

      call fio_write_line (obj%fio, obj%ZT1_ccc1   , ccc1   )
      call fio_write_line (obj%fio, obj%ZT1_sss1   , sss1   )
      call fio_write_line (obj%fio, obj%ZT1_sss1a  , sss1a  )
      call fio_write_line (obj%fio, obj%ZT1_lll1   , lll1   )

      call fio_after_write_line (obj%fio,err,msg)
      return
      end subroutine geomio2_write_zt1_card


!!--------------------- geomio2 read zt2 card -------------------------------!!
!!--------------------- geomio2 read zt2 card -------------------------------!!
!!--------------------- geomio2 read zt2 card -------------------------------!!


      subroutine geomio2_read_zt2_card  (obj,err,msg,   &
                                         ccc2,rrr2,rrr2a,lll2)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=*)    ,intent(out)   :: ccc2                  ! arguments
      real                ,intent(out)   :: rrr2,rrr2a            ! arguments
      integer             ,intent(out)   :: lll2                  ! arguments

      call fio_before_read_line (obj%fio)

      call fio_read_line (obj%fio, obj%ZT2_ccc2   , ccc2   )
      call fio_read_line (obj%fio, obj%ZT2_rrr2   , rrr2   )
      call fio_read_line (obj%fio, obj%ZT2_rrr2a  , rrr2a  )
      call fio_read_line (obj%fio, obj%ZT2_lll2   , lll2   )
 
      call fio_after_read_line (obj%fio,err,msg)
      return
      end subroutine geomio2_read_zt2_card


!!--------------------- geomio2 write zt2 card -------------------------------!!
!!--------------------- geomio2 write zt2 card -------------------------------!!
!!--------------------- geomio2 write zt2 card -------------------------------!!


      subroutine geomio2_write_zt2_card  (obj,err,msg,   &
                                          ccc2,rrr2,rrr2a,lll2)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=*)    ,intent(in)    :: ccc2                  ! arguments
      real                ,intent(in)    :: rrr2,rrr2a            ! arguments
      integer             ,intent(in)    :: lll2                  ! arguments

      call fio_before_write_line (obj%fio)

      call fio_write_line (obj%fio, obj%ZT2_ccc2   , ccc2   )
      call fio_write_line (obj%fio, obj%ZT2_rrr2   , rrr2   )
      call fio_write_line (obj%fio, obj%ZT2_rrr2a  , rrr2a  )
      call fio_write_line (obj%fio, obj%ZT2_lll2   , lll2   )
 
      call fio_after_write_line (obj%fio,err,msg)
      return
      end subroutine geomio2_write_zt2_card


!!--------------------- geomio2 read zt3 card -------------------------------!!
!!--------------------- geomio2 read zt3 card -------------------------------!!
!!--------------------- geomio2 read zt3 card -------------------------------!!


      subroutine geomio2_read_zt3_card  (obj,err,msg,   &
                                         ccc3,iggg3,iggg3a,ittt3,ittt3a)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=*)    ,intent(out)   :: ccc3                  ! arguments
      integer             ,intent(out)   :: iggg3,iggg3a          ! arguments
      integer             ,intent(out)   :: ittt3,ittt3a          ! arguments

      call fio_before_read_line (obj%fio)

      call fio_read_line (obj%fio, obj%ZT3_ccc3   , ccc3   )
      call fio_read_line (obj%fio, obj%ZT3_iggg3  , iggg3  )
      call fio_read_line (obj%fio, obj%ZT3_iggg3a , iggg3a )
      call fio_read_line (obj%fio, obj%ZT3_ittt3  , ittt3  )
      call fio_read_line (obj%fio, obj%ZT3_ittt3a , ittt3a )

      call fio_after_read_line (obj%fio,err,msg)
      return
      end subroutine geomio2_read_zt3_card


!!--------------------- geomio2 write zt3 card -------------------------------!!
!!--------------------- geomio2 write zt3 card -------------------------------!!
!!--------------------- geomio2 write zt3 card -------------------------------!!


      subroutine geomio2_write_zt3_card (obj,err,msg,   &
                                         ccc3,iggg3,iggg3a,ittt3,ittt3a)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=*)    ,intent(in)    :: ccc3                  ! arguments
      integer             ,intent(in)    :: iggg3,iggg3a          ! arguments
      integer             ,intent(in)    :: ittt3,ittt3a          ! arguments

      call fio_before_write_line (obj%fio)

      call fio_write_line (obj%fio, obj%ZT3_ccc3   , ccc3   )
      call fio_write_line (obj%fio, obj%ZT3_iggg3  , iggg3  )
      call fio_write_line (obj%fio, obj%ZT3_iggg3a , iggg3a )
      call fio_write_line (obj%fio, obj%ZT3_ittt3  , ittt3  )
      call fio_write_line (obj%fio, obj%ZT3_ittt3a , ittt3a )

      call fio_after_write_line (obj%fio,err,msg)
      return
      end subroutine geomio2_write_zt3_card


!!--------------------- geomio2 read zt4 card -------------------------------!!
!!--------------------- geomio2 read zt4 card -------------------------------!!
!!--------------------- geomio2 read zt4 card -------------------------------!!


      subroutine geomio2_read_zt4_card  (obj,err,msg,   &
                                         ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=*)    ,intent(out)   :: ccc4                  ! arguments
      real                ,intent(out)   :: sss4,sss4a            ! arguments
      real                ,intent(out)   :: rrr4,rrr4a            ! arguments
      integer             ,intent(out)   :: lll4,lll4a            ! arguments

      call fio_before_read_line (obj%fio)

      call fio_read_line (obj%fio, obj%ZT4_ccc4   , ccc4   )
      call fio_read_line (obj%fio, obj%ZT4_sss4   , sss4   )
      call fio_read_line (obj%fio, obj%ZT4_sss4a  , sss4a  )
      call fio_read_line (obj%fio, obj%ZT4_lll4   , lll4   )
      call fio_read_line (obj%fio, obj%ZT4_rrr4   , rrr4   )
      call fio_read_line (obj%fio, obj%ZT4_rrr4a  , rrr4a  )
      call fio_read_line (obj%fio, obj%ZT4_lll4a  , lll4a  )
 
      call fio_after_read_line (obj%fio,err,msg)
      return
      end subroutine geomio2_read_zt4_card


!!--------------------- geomio2 write zt4 card -------------------------------!!
!!--------------------- geomio2 write zt4 card -------------------------------!!
!!--------------------- geomio2 write zt4 card -------------------------------!!


      subroutine geomio2_write_zt4_card (obj,err,msg,   &
                                         ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      implicit none
      type(geomio2_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=*)    ,intent(in)    :: ccc4                  ! arguments
      real                ,intent(in)    :: sss4,sss4a            ! arguments
      real                ,intent(in)    :: rrr4,rrr4a            ! arguments
      integer             ,intent(in)    :: lll4,lll4a            ! arguments

      call fio_before_write_line (obj%fio)

      call fio_write_line (obj%fio, obj%ZT4_ccc4   , ccc4   )
      call fio_write_line (obj%fio, obj%ZT4_sss4   , sss4   )
      call fio_write_line (obj%fio, obj%ZT4_sss4a  , sss4a  )
      call fio_write_line (obj%fio, obj%ZT4_lll4   , lll4   )
      call fio_write_line (obj%fio, obj%ZT4_rrr4   , rrr4   )
      call fio_write_line (obj%fio, obj%ZT4_rrr4a  , rrr4a  )
      call fio_write_line (obj%fio, obj%ZT4_lll4a  , lll4a  )
 
      call fio_after_write_line (obj%fio,err,msg)
      return
      end subroutine geomio2_write_zt4_card


!!------------------------- end of module ---------------------------------!!
!!------------------------- end of module ---------------------------------!!
!!------------------------- end of module ---------------------------------!!
 

      end module geomio2_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
