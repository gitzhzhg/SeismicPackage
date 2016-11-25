
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ velio2.f90 -------------------------------!!
!!------------------------------ velio2.f90 -------------------------------!!
!!------------------------------ velio2.f90 -------------------------------!!


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
! Name       : VELIO2
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2004-03-15   by: Tom Stoeckley
! Maturity   : production
! Purpose    : To read and write a velocity section in a self-defining file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading and writing a single velocity data
! section in a self-defining non-trace file which might contain various
! sections of different types.
!
! This primitive does not open or close the file.
! This primitive cannot read or write old-style CPS velocity files.
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
!                          CALLING SEQUENCE                  
!
!                               i   b   b      i
!    call velio2_create       (obj,fio,pjar,secname)
!    call velio2_delete       (obj)
!                               b
!                                                        opt    opt
!                               b    o      o      o      o      o   
!    call velio2_read_velfun  (obj,xcoord,ycoord,npicks,tpicks,vpicks,
!             err,msg,velname,veltype,project,line,rdate,pdate,userid,comment)
!              o   o     o       o       o     o     o     o     o       o   
!                       opt     opt     opt   opt   opt   opt   opt     opt  
!
!                               b    i      i      i      i      i  
!    call velio2_write_velfun (obj,xcoord,ycoord,npicks,tpicks,vpicks,
!             err,msg,velname,veltype,project,line,rdate,pdate,userid,comment)
!              o   o     i       i       i     i     i     i     i       i   
!                       opt     opt     opt   opt   opt   opt   opt     opt  
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(velio2_struct)  obj = pointer the to VELIO2 data structure.
! type(fio_struct)     fio = reference to the FIO data structure to use.
! type(pjar_struct)   pjar = reference to the PJAR data structure to use.
! char(*)          secname = PJAR section containing the parameters.
! integer              err = error flag (returned).
! char(*)              msg = message for possible printing (returned).
!
! real    xcoord    = X coordinate of the velocity function.
! real    ycoord    = Y coordinate of the velocity function.
! integer npicks    = number of time/vel picks in the velocity function.
! real    tpicks(:) = array that holds the abscissae (time/depth picks).
! real    vpicks(:) = array that holds the ordinates (velocity picks).
! char(*) velname   = name of velocity function  (8 characters).
! char(*) veltype   = type of velocity function  (4 characters).
! char(*) project   = project name              (10 characters).
! char(*) line      = line name                 (10 characters).
! char(*) rdate     = recording date             (5 characters).
! char(*) pdate     = processing date            (5 characters).
! char(*) userid    = user ID                    (3 characters).
! char(*) comment   = comment                   (15 characters).
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
!  5. 2004-03-15  Stoeckley  Make VPICKS and TPICKS optional when reading
!                             a velocity function.
!  4. 2003-10-20  Stoeckley  Change logic slightly to eliminate need to
!                             allocate VPICKS and TPICKS to one more than
!                             NPICKS when reading a velocity function.
!  3. 2002-04-11  Stoeckley  Add error check on size of TPICKS and VPICKS
!                             array arguments; remove decimal restriction in
!                             call to fio_write_line to allow saving bogus
!                             velocities with any value.
!  2. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR and FIO primitives.
!  1. 2000-11-27  Stoeckley  Initial version moved from portions of code in
!                             the VELIO primitive.
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
 
 
      module velio2_module
      use pjar_module
      use fio_module
      use string_module
      use named_constants_module
      implicit none
      public

      character(len=100),public,save :: VELIO2_IDENT = &
'$Id: velio2.f90,v 1.5 2004/03/15 12:36:26 Stoeckley prod sps $'

      type,public :: velio2_struct
           private
           type(fio_struct) ,pointer :: fio
           character(len=8)          :: encoding
           integer                   :: col_xcoord   
           integer                   :: col_ycoord   
           integer                   :: col_abscissa 
           integer                   :: col_ordinate 
           integer                   :: col_velname  
           integer                   :: col_veltype  
           integer                   :: col_project  
           integer                   :: col_line     
           integer                   :: col_rdate    
           integer                   :: col_pdate    
           integer                   :: col_userid   
           integer                   :: col_comment  
      end type velio2_struct

      contains


!!-------------------------- velio2 create -----------------------------!!
!!-------------------------- velio2 create -----------------------------!!
!!-------------------------- velio2 create -----------------------------!!
 
 
      subroutine velio2_create (obj,fio,pjar,secname)
      implicit none
      type(velio2_struct),pointer              :: obj           ! arguments
      type(fio_struct)   ,intent(inout),target :: fio           ! arguments
      type(pjar_struct)  ,intent(inout)        :: pjar          ! arguments
      character(len=*)   ,intent(in)           :: secname       ! arguments
 
      allocate (obj)

      obj%fio => fio

      call pjar_choose_section (pjar, secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)

      obj%col_xcoord   = pjar_find (pjar, 'fields', 'xcoord'  )
      obj%col_ycoord   = pjar_find (pjar, 'fields', 'ycoord'  )
      obj%col_abscissa = pjar_find (pjar, 'fields', 'abscissa')
      obj%col_ordinate = pjar_find (pjar, 'fields', 'ordinate')
      obj%col_velname  = pjar_find (pjar, 'fields', 'velname' )
      obj%col_veltype  = pjar_find (pjar, 'fields', 'veltype' )
      obj%col_project  = pjar_find (pjar, 'fields', 'project' )
      obj%col_line     = pjar_find (pjar, 'fields', 'line'    )
      obj%col_rdate    = pjar_find (pjar, 'fields', 'rdate'   )
      obj%col_pdate    = pjar_find (pjar, 'fields', 'pdate'   )
      obj%col_userid   = pjar_find (pjar, 'fields', 'userid'  )
      obj%col_comment  = pjar_find (pjar, 'fields', 'comment' )
      return
      end subroutine velio2_create
 
 
!!--------------------------- velio2 delete -----------------------------!!
!!--------------------------- velio2 delete -----------------------------!!
!!--------------------------- velio2 delete -----------------------------!!


      subroutine velio2_delete (obj)
      implicit none
      type(velio2_struct),pointer :: obj             ! arguments

      if (associated(obj)) deallocate(obj)
      return
      end subroutine velio2_delete


!!------------------------ velio2 read velfun -------------------------!!
!!------------------------ velio2 read velfun -------------------------!!
!!------------------------ velio2 read velfun -------------------------!!


      subroutine velio2_read_velfun                                   &
                    (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                     velname,veltype,                                 &
                     project,line,rdate,pdate,userid,comment)
      implicit none
      type(velio2_struct),intent(inout)     :: obj                  ! arguments
      real            ,intent(out)          :: xcoord,ycoord        ! arguments
      integer         ,intent(out)          :: npicks               ! arguments
      real            ,intent(out),optional :: tpicks(:),vpicks(:)  ! arguments
      integer         ,intent(out)          :: err                  ! arguments
      character(len=*),intent(out)          :: msg                  ! arguments
      character(len=*),intent(out),optional :: velname              ! arguments
      character(len=*),intent(out),optional :: veltype              ! arguments
      character(len=*),intent(out),optional :: project,line         ! arguments
      character(len=*),intent(out),optional :: rdate,pdate          ! arguments
      character(len=*),intent(out),optional :: userid               ! arguments
      character(len=*),intent(out),optional :: comment              ! arguments
      logical                               :: end_packet           ! local
      integer                               :: indx                 ! local
      real                                  :: tpicks2,vpicks2      ! local
      logical                               :: maybe_no_picks       ! local

!!!!!!!!!! read binary velfun:

      if (obj%encoding == FIO_BINARY) then

           call fio_before_read_binary (obj%fio,npicks)

           call fio_read_binary (obj%fio, obj%col_xcoord  , xcoord)
           call fio_read_binary (obj%fio, obj%col_ycoord  , ycoord)
          if (present(tpicks)) &
           call fio_read_binary (obj%fio, obj%col_abscissa, tpicks)  ! array
          if (present(vpicks)) &
           call fio_read_binary (obj%fio, obj%col_ordinate, vpicks)  ! array

if (present(velname)) call fio_read_binary (obj%fio, obj%col_velname , velname)
if (present(veltype)) call fio_read_binary (obj%fio, obj%col_veltype , veltype)
if (present(project)) call fio_read_binary (obj%fio, obj%col_project , project)
if (present(line   )) call fio_read_binary (obj%fio, obj%col_line    , line   )
if (present(rdate  )) call fio_read_binary (obj%fio, obj%col_rdate   , rdate  )
if (present(pdate  )) call fio_read_binary (obj%fio, obj%col_pdate   , pdate  )
if (present(userid )) call fio_read_binary (obj%fio, obj%col_userid  , userid )
if (present(comment)) call fio_read_binary (obj%fio, obj%col_comment , comment)

           call fio_after_read_binary (obj%fio,err,msg)

!!!!!!!!!! read ascii or hybrid velfun:

      else

           indx = 0
           maybe_no_picks = .false.
           do
                call fio_before_read_line (obj%fio)

                call fio_read_line (obj%fio, obj%col_xcoord  , xcoord)
                call fio_read_line (obj%fio, obj%col_ycoord  , ycoord)
                call fio_read_line (obj%fio, obj%col_abscissa, tpicks2)
                call fio_read_line (obj%fio, obj%col_ordinate, vpicks2)

if (present(velname)) call fio_read_line (obj%fio, obj%col_velname , velname)
if (present(veltype)) call fio_read_line (obj%fio, obj%col_veltype , veltype)
if (present(project)) call fio_read_line (obj%fio, obj%col_project , project)
if (present(line   )) call fio_read_line (obj%fio, obj%col_line    , line   )
if (present(rdate  )) call fio_read_line (obj%fio, obj%col_rdate   , rdate  )
if (present(pdate  )) call fio_read_line (obj%fio, obj%col_pdate   , pdate  )
if (present(userid )) call fio_read_line (obj%fio, obj%col_userid  , userid )
if (present(comment)) call fio_read_line (obj%fio, obj%col_comment , comment)
 
                call fio_after_read_line (obj%fio,err,msg,end_packet)
                if (end_packet) exit

                indx = indx + 1

                if (present(tpicks)) then
                     if (size(tpicks) < indx) then
                          err = FIO_ERROR
                          msg = 'size of time and velocity arrays too small'
                          exit
                     end if
                     tpicks(indx) = tpicks2
                end if

                if (present(vpicks)) then
                     if (size(vpicks) < indx) then
                          err = FIO_ERROR
                          msg = 'size of time and velocity arrays too small'
                          exit
                     end if
                     vpicks(indx) = vpicks2
                end if

                if (indx == 0) then
                     maybe_no_picks = (tpicks2 == FNIL .and. vpicks2 == FNIL)
                end if

           end do
           npicks = indx

           if (npicks == 1 .and. err == FIO_OK) then
                if (maybe_no_picks) npicks = 0
           end if

      end if
      return
      end subroutine velio2_read_velfun


!!------------------------ velio2 write velfun -------------------------!!
!!------------------------ velio2 write velfun -------------------------!!
!!------------------------ velio2 write velfun -------------------------!!


      subroutine velio2_write_velfun                                  &
                    (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                     velname,veltype,                                 &
                     project,line,rdate,pdate,userid,comment)
      implicit none
      type(velio2_struct),intent(inout)     :: obj                 ! arguments
      real            ,intent(in)           :: xcoord,ycoord       ! arguments
      integer         ,intent(in)           :: npicks              ! arguments
      real            ,intent(in)           :: tpicks(:),vpicks(:) ! arguments
      integer         ,intent(out)          :: err                 ! arguments
      character(len=*),intent(out)          :: msg                 ! arguments
      character(len=*),intent(in) ,optional :: velname             ! arguments
      character(len=*),intent(in) ,optional :: veltype             ! arguments
      character(len=*),intent(in) ,optional :: project,line        ! arguments
      character(len=*),intent(in) ,optional :: rdate,pdate         ! arguments
      character(len=*),intent(in) ,optional :: userid              ! arguments
      character(len=*),intent(in) ,optional :: comment             ! arguments
      integer                               :: ipick               ! local

!!!!!!!!!! write binary velfun:

      if (obj%encoding == FIO_BINARY) then

         call fio_before_write_binary (obj%fio,npicks)

         call fio_write_binary (obj%fio, obj%col_xcoord  , xcoord)
         call fio_write_binary (obj%fio, obj%col_ycoord  , ycoord)
         call fio_write_binary (obj%fio, obj%col_abscissa, tpicks)  ! array
         call fio_write_binary (obj%fio, obj%col_ordinate, vpicks)  ! array

if (present(velname)) call fio_write_binary (obj%fio, obj%col_velname , velname)
if (present(veltype)) call fio_write_binary (obj%fio, obj%col_veltype , veltype)
if (present(project)) call fio_write_binary (obj%fio, obj%col_project , project)
if (present(line   )) call fio_write_binary (obj%fio, obj%col_line    , line   )
if (present(rdate  )) call fio_write_binary (obj%fio, obj%col_rdate   , rdate  )
if (present(pdate  )) call fio_write_binary (obj%fio, obj%col_pdate   , pdate  )
if (present(userid )) call fio_write_binary (obj%fio, obj%col_userid  , userid )
if (present(comment)) call fio_write_binary (obj%fio, obj%col_comment , comment)
 
         call fio_after_write_binary (obj%fio,err,msg)

!!!!!!!!!! write ascii or hybrid velfun with no picks:

      else if (npicks == 0) then

         call fio_before_write_line (obj%fio)

         call fio_write_line (obj%fio, obj%col_xcoord  , xcoord  , ndec=3)
         call fio_write_line (obj%fio, obj%col_ycoord  , ycoord  , ndec=3)
         call fio_write_line (obj%fio, obj%col_abscissa, FNIL    , ndec=3)
         call fio_write_line (obj%fio, obj%col_ordinate, FNIL    , ndec=3)

if (present(velname)) call fio_write_line (obj%fio, obj%col_velname , velname)
if (present(veltype)) call fio_write_line (obj%fio, obj%col_veltype , veltype)
if (present(project)) call fio_write_line (obj%fio, obj%col_project , project)
if (present(line   )) call fio_write_line (obj%fio, obj%col_line    , line   )
if (present(rdate  )) call fio_write_line (obj%fio, obj%col_rdate   , rdate  )
if (present(pdate  )) call fio_write_line (obj%fio, obj%col_pdate   , pdate  )
if (present(userid )) call fio_write_line (obj%fio, obj%col_userid  , userid )
if (present(comment)) call fio_write_line (obj%fio, obj%col_comment , comment)

         call fio_after_write_line (obj%fio,err,msg)

!!!!!!!!!! write ascii or hybrid velfun:

      else

         do ipick = 1,npicks

           call fio_before_write_line (obj%fio)

           call fio_write_line (obj%fio, obj%col_xcoord  , xcoord  , ndec=3)
           call fio_write_line (obj%fio, obj%col_ycoord  , ycoord  , ndec=3)
           call fio_write_line (obj%fio, obj%col_abscissa,tpicks(ipick),ndec=3)
    !!     call fio_write_line (obj%fio, obj%col_ordinate,vpicks(ipick),ndec=3)
           call fio_write_line (obj%fio, obj%col_ordinate,vpicks(ipick))
    !! decimal restriction for ordinates removed above 4/05/02.

if (present(velname)) call fio_write_line (obj%fio, obj%col_velname , velname)
if (present(veltype)) call fio_write_line (obj%fio, obj%col_veltype , veltype)
if (present(project)) call fio_write_line (obj%fio, obj%col_project , project)
if (present(line   )) call fio_write_line (obj%fio, obj%col_line    , line   )
if (present(rdate  )) call fio_write_line (obj%fio, obj%col_rdate   , rdate  )
if (present(pdate  )) call fio_write_line (obj%fio, obj%col_pdate   , pdate  )
if (present(userid )) call fio_write_line (obj%fio, obj%col_userid  , userid )
if (present(comment)) call fio_write_line (obj%fio, obj%col_comment , comment)

           call fio_after_write_line (obj%fio,err,msg)
           if (err == FIO_ERROR) exit
         end do

      end if
      return
      end subroutine velio2_write_velfun


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module velio2_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
