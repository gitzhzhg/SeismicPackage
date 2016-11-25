
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- velio1.f90 --------------------------------!!
!!------------------------------- velio1.f90 --------------------------------!!
!!------------------------------- velio1.f90 --------------------------------!!


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
! Name       : VELIO1
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : To read and write old-style CPS velocity function files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading and writing old-style CPS velocity files.
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
!                          CALLING SEQUENCE                  
!
! Read or write the header record:
!
!                               b   o      i     o   o  
!    call velio1_read_header  (dio,pjar,secname,err,msg)
!    call velio1_write_header (dio,pjar,secname,err,msg)
!                               b   i      i     o   o  
!
! Read or write a single velocity function:
!                                                        opt    opt
!                               b    o      o      o      o      o     o   o 
!    call velio1_read_velfun  (dio,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,
!           velname,veltype,project,line,rdate,pdate,userid,comment)
!              o       o       o     o     o     o     o       o   
!             opt     opt     opt   opt   opt   opt   opt     opt  
!
!                               b    i      i      i      i      i     o   o 
!    call velio1_write_velfun (dio,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,
!           velname,veltype,project,line,rdate,pdate,userid,comment)
!              i       i       i     i     i     i     i       i   
!             opt     opt     opt   opt   opt   opt   opt     opt  
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(dio_struct)  dio     = reference to the DIO data structure.
! type(pjar_struct) pjar    = reference to the PJAR data structure.
! char(*)           secname = PJAR section containing the parameters.
! integer           err     = error flag (returned).
! char(*)           msg     = message for possible printing (returned).
!
! integer nfun     = number of velocity functions in the file.
! integer nhx      = CPS trace header word containing X coordinate.
! integer nhy      = CPS trace header word containing Y coordinate.
! real    nmosign  = the   sign   used for the moveout (normally 1.0).
! real    nmoexp   = the exponent used for the moveout (normally 2.0).
!
! real    xcoord    = X coordinate of the velocity function.
! real    ycoord    = Y coordinate of the velocity function.
! integer npicks    = number of time/vel picks in the velocity function.
! real    tpicks(:) = array that holds the abscissae (  TIME   picks).
! real    vpicks(:) = array that holds the ordinates (VELOCITY picks).
! char(*) velname   = name of velocity function  (8 characters).
! char(*) veltype   = type of velocity function  (4 characters).
! char(*) project   = project name              (10 characters).
! char(*) line      = line name                 (10 characters).
! char(*) rdate     = recording date             (5 characters).
! char(*) pdate     = processing date            (5 characters).
! char(*) userid    = user ID                    (3 characters).
! char(*) comment   = comment                   (15 characters).
!
! ERR will be set to DIO_OK or DIO_ERROR.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!005. 2006-04-25  B. Menger   Removed Unused Variables.
!  4. 2004-03-15  Stoeckley  Make VPICKS and TPICKS optional when reading
!                             a velocity function.
!  3. 2002-04-11  Stoeckley  Remove decimal restriction in call to string_ff2ss
!                             to allow saving bogus velocities with any value.
!  2. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR primitive.
!  1. 2000-11-27  Stoeckley  Initial version moved from private routines in
!                             the VELIO primitive which came originally from
!                             the old CPS primitive IO_CPSVF.
!
! Revision history of the old IO_CPSVF primitive:
!
!     Date        Author     Description
!     ----        ------     -----------
! 17. 1999-11-08  Stoeckley  Add ability to overwrite existing files,
!                             and change default NMOSIGN and NMOEXP upon
!                             read to 1.0 and 2.0.
! 16. 1999-09-21  Stoeckley  Converted from IO_CPSVF in the old system.
! 15. 1999-09-21  Stoeckley  Fixed bug by increasing two local arrays
!                             from 100 to 200 words.
! 14. 1999-05-24  Day        Checks for existence of input file name
!                             before adding the extension VEL to the name.
! 13. 1998-05-07  Stoeckley  Preset X and Y to zero before reading them.
! 12. 1997-05-15  Vunderink  Put new consolidated code on VMS
! 11. 1997-05-07  Stoeckley  Consolidate the VMS and UNIX versions
!                             of this code, with instructions on how
!                             to keep both versions consistent and
!                             up-to-date.  Also add more explicit
!                             error messages (from UNIX version), and
!                             added error returns to INQUIRE statements
!                             and an OPEN statement.
! 10. 1997-04-23  Vunderink  Add entry OPEN_CPS_VELFILE3, and add NMC
!                             SIGN and POWER to *GLOBALS card.
!  9. 1994-01-19  K Goodger  Add common block SPLTVELX to communicate
!                             with SPLT.
!  8. 1992-01-22  Stoeckley  Change dimensioning of SV since Unix
!                             compiler doesn't like it.
!  7. 1991-02-15  Stoeckley  Add entry OPEN_CPS_VELFILE2, and add NHX
!                             and NHY to *GLOBALS card.
!  6. 1990-07-19  WR Troutt  Add error logic for OPEN when IOSTAT=WRITE.
!  5. 1990-03-07  WR Troutt  Add CARRIAGECONTROL='LIST' to OPEN statements
!                             to allow use of TYPE and PRINT commands.
!  4. 1989-05-15  JB Sinton  Changed WRITE_CPS_VELFILE to be more robust.
!  3. 1989-04-10  JB Sinton  Argument FILENAME returns with the full
!                             file name.
!  2. 1989-03-02  JB Sinton  Corrections to OPEN_CPS_VELFILE when reopen-
!                             ing an already opened file.
!  1. 1988-10-19  JB Sinton  Added LFN and NVPP to argument lists.
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
 
 
      module velio1_module
      use string_module
      use dio_module
      use pjar_module
      implicit none
      public
      private :: velio1_oldcps_read_header
      private :: velio1_oldcps_write_header
      private :: velio1_oldcps_read_velfun
      private :: velio1_oldcps_write_velfun

      character(len=100),public,save :: VELIO1_IDENT = &
'$Id: velio1.f90,v 1.5 2006/04/25 13:24:27 Menger prod sps $'

      contains


!!------------------------- velio1 read header ----------------------------!!
!!------------------------- velio1 read header ----------------------------!!
!!------------------------- velio1 read header ----------------------------!!
 
 
      subroutine velio1_read_header (dio,pjar,secname,err,msg)
      implicit none
      type(dio_struct) ,intent(inout)        :: dio               ! arguments
      type(pjar_struct),intent(inout)        :: pjar              ! arguments
      character(len=*) ,intent(in)           :: secname           ! arguments
      integer          ,intent(out)          :: err               ! arguments
      character(len=*) ,intent(out)          :: msg               ! arguments
      integer                                :: nfun,nhx,nhy      ! local
      real                                   :: nmosign,nmoexp    ! local
 
      call velio1_oldcps_read_header (dio,nfun,nhx,nhy,nmosign,nmoexp,err,msg)

      if (err /= DIO_OK) return

      call pjar_choose_section (pjar,secname)
      call pjar_put            (pjar,'nfun'     ,nfun)
      call pjar_put            (pjar,'nhx'      ,nhx)
      call pjar_put            (pjar,'nhy'      ,nhy)
      call pjar_put            (pjar,'nmosign'  ,nmosign)
      call pjar_put            (pjar,'nmoexp'   ,nmoexp)
      return
      end subroutine velio1_read_header
 

!!------------------------- velio1 write header ----------------------------!!
!!------------------------- velio1 write header ----------------------------!!
!!------------------------- velio1 write header ----------------------------!!


      subroutine velio1_write_header (dio,pjar,secname,err,msg)
      implicit none
      type(dio_struct) ,intent(inout)        :: dio               ! arguments
      type(pjar_struct),intent(inout)        :: pjar              ! arguments
      character(len=*) ,intent(in)           :: secname           ! arguments
      integer          ,intent(out)          :: err               ! arguments
      character(len=*) ,intent(out)          :: msg               ! arguments
      integer                                :: nfun,nhx,nhy      ! local
      real                                   :: nmosign,nmoexp    ! local

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar,'nfun'     ,nfun)
      call pjar_get            (pjar,'nhx'      ,nhx)
      call pjar_get            (pjar,'nhy'      ,nhy)
      call pjar_get            (pjar,'nmosign'  ,nmosign)
      call pjar_get            (pjar,'nmoexp'   ,nmoexp)

      call velio1_oldcps_write_header (dio,nfun,nhx,nhy,nmosign,nmoexp,err,msg)
      return
      end subroutine velio1_write_header


!!--------------------------- velio1 read velfun -------------------------!!
!!--------------------------- velio1 read velfun -------------------------!!
!!--------------------------- velio1 read velfun -------------------------!!


      subroutine velio1_read_velfun                                   &
                    (dio,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                     velname,veltype,                                 &
                     project,line,rdate,pdate,userid,comment)
      implicit none
      type(dio_struct),intent(inout)        :: dio                 ! arguments
      real            ,intent(out)          :: xcoord,ycoord       ! arguments
      integer         ,intent(out)          :: npicks              ! arguments
      real            ,intent(out),optional :: tpicks(:),vpicks(:) ! arguments
      integer         ,intent(out)          :: err                 ! arguments
      character(len=*),intent(out)          :: msg                 ! arguments
      character(len=*),intent(out),optional :: velname,veltype     ! arguments
      character(len=*),intent(out),optional :: project,line        ! arguments
      character(len=*),intent(out),optional :: rdate,pdate         ! arguments
      character(len=*),intent(out),optional :: userid              ! arguments
      character(len=*),intent(out),optional :: comment             ! arguments
      character(len=8)                      :: velname2            ! local
      character(len=4)                      :: veltype2            ! local
      character(len=10)                     :: project2,line2      ! local
      character(len=5)                      :: rdate2,pdate2       ! local
      character(len=3)                      :: userid2             ! local
      character(len=15)                     :: comment2            ! local

      xcoord    = 0.0
      ycoord    = 0.0
      npicks    = 0
      velname2  = 'none'
      veltype2  = 'VTNM'
      project2  = 'none' 
      line2     = 'none' 
      rdate2    = 'none' 
      pdate2    = 'none' 
      userid2   = 'none' 
      comment2  = 'none' 

      call velio1_oldcps_read_velfun                         &
            (dio,velname2,npicks,xcoord,ycoord,              &
             veltype2,tpicks,vpicks,                         &
             project2,line2,rdate2,pdate2,userid2,comment2,  &
             err,msg)

      if (present(velname)) velname = velname2
      if (present(veltype)) veltype = veltype2
      if (present(project)) project = project2
      if (present(line   )) line    = line2
      if (present(rdate  )) rdate   = rdate2
      if (present(pdate  )) pdate   = pdate2
      if (present(userid )) userid  = userid2
      if (present(comment)) comment = comment2
      return
      end subroutine velio1_read_velfun


!!--------------------------- velio1 write velfun -------------------------!!
!!--------------------------- velio1 write velfun -------------------------!!
!!--------------------------- velio1 write velfun -------------------------!!


      subroutine velio1_write_velfun                                  &
                    (dio,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                     velname,veltype,                                 &
                     project,line,rdate,pdate,userid,comment)
      implicit none
      type(dio_struct),intent(inout)        :: dio                 ! arguments
      real            ,intent(in)           :: xcoord,ycoord       ! arguments
      integer         ,intent(in)           :: npicks              ! arguments
      real            ,intent(in)           :: tpicks(:),vpicks(:) ! arguments
      integer         ,intent(out)          :: err                 ! arguments
      character(len=*),intent(out)          :: msg                 ! arguments
      character(len=*),intent(in) ,optional :: velname,veltype     ! arguments
      character(len=*),intent(in) ,optional :: project,line        ! arguments
      character(len=*),intent(in) ,optional :: rdate,pdate         ! arguments
      character(len=*),intent(in) ,optional :: userid              ! arguments
      character(len=*),intent(in) ,optional :: comment             ! arguments
      character(len=8)                      :: velname2            ! local
      character(len=4)                      :: veltype2            ! local
      character(len=10)                     :: project2,line2      ! local
      character(len=5)                      :: rdate2,pdate2       ! local
      character(len=3)                      :: userid2             ! local
      character(len=15)                     :: comment2            ! local

      velname2 = 'none';   if (present(velname)) velname2 = velname
      veltype2 = 'VTNM';   if (present(veltype)) veltype2 = veltype
      project2 = 'none';   if (present(project)) project2 = project
      line2    = 'none';   if (present(line   )) line2    = line
      rdate2   = 'none';   if (present(rdate  )) rdate2   = rdate
      pdate2   = 'none';   if (present(pdate  )) pdate2   = pdate
      userid2  = 'none';   if (present(userid )) userid2  = userid
      comment2 = 'none';   if (present(comment)) comment2 = comment

      call velio1_oldcps_write_velfun                        &
            (dio,velname2,npicks,xcoord,ycoord,              &
             veltype2,tpicks,vpicks,                         &
             project2,line2,rdate2,pdate2,userid2,comment2,  &
             err,msg)
      return
      end subroutine velio1_write_velfun


!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!
!!------------ private routines from the old io_cpsvf primitive ------------!!


!!----------------------- velio1 oldcps read header ------------------------!!
!!----------------------- velio1 oldcps read header ------------------------!!
!!----------------------- velio1 oldcps read header ------------------------!!


        subroutine velio1_oldcps_read_header (dio,NFUN,    &
                                             NHX,NHY,NMOSIGN,NMOEXP,err,msg)
        implicit none
        type(dio_struct),intent(inout) :: dio                     ! arguments
        integer         ,intent(out)   :: nfun,nhx,nhy            ! arguments
        REAL            ,intent(out)   :: NMOSIGN,NMOEXP          ! arguments
        integer         ,intent(out)   :: err                     ! arguments
        CHARACTER(len=*),intent(out)   :: msg                     ! arguments
        integer                        :: IVERSION,nvpp           ! local
        CHARACTER(len=200)             :: card                    ! local

        call dio_read_card (dio,card)
        call dio_status    (dio,err,msg)
        if (err /= DIO_OK) return

        IF (CARD(:9).EQ.'*GLOBALS:') THEN
           NHX=0
           NHY=0
           NMOSIGN=1.0
           NMOEXP=2.0
           READ (CARD(10:),*,iostat=err)  &
                               NVPP,NFUN,IVERSION,NHX,NHY,NMOSIGN,NMOEXP
           if (err /= 0) then
                NMOSIGN=1.0
                NMOEXP=2.0
                READ (CARD(10:),*,iostat=err)  &
                               NVPP,NFUN,IVERSION,NHX,NHY
                if (err /= 0) then
                     ERR = DIO_ERROR
                     msg = 'error decoding velocity file header'
                     return
                end if
           end if
           ERR = DIO_OK
        ELSE
           ERR = DIO_ERROR
           msg = 'first card on velocity file does not contain *GLOBALS:'
           return
        ENDIF
        msg = 'input old-style CPS velocity file successfully opened'
        return
        end subroutine velio1_oldcps_read_header


!!------------------------ velio1 oldcps write header ----------------------!!
!!------------------------ velio1 oldcps write header ----------------------!!
!!------------------------ velio1 oldcps write header ----------------------!!


        subroutine velio1_oldcps_write_header (dio,NFUN,    &
                                              NHX,NHY,NMOSIGN,NMOEXP,err,msg)
        implicit none
        type(dio_struct),intent(inout) :: dio                     ! arguments
        integer         ,intent(in)    :: nfun,nhx,nhy            ! arguments
        REAL            ,intent(in)    :: NMOSIGN,NMOEXP          ! arguments
        integer         ,intent(out)   :: err                     ! arguments
        CHARACTER(len=*),intent(out)   :: msg                     ! arguments
        integer,parameter              :: IVERSION = 1            ! local
        CHARACTER(len=200)             :: card                    ! local
        integer,parameter              :: nvpp = 2                ! local

        WRITE (card,'(''*GLOBALS: '',5I10,2(1X,G9.3))') NVPP,NFUN,     &
                             IVERSION,NHX,NHY,NMOSIGN,NMOEXP
        call dio_write_card (dio,card)
        call dio_status     (dio,err,msg)
        msg = 'output old-style CPS velocity file successfully opened'
        return
        end subroutine velio1_oldcps_write_header


!!------------------------ velio1 oldcps read velfun ----------------------!!
!!------------------------ velio1 oldcps read velfun ----------------------!!
!!------------------------ velio1 oldcps read velfun ----------------------!!


        subroutine velio1_oldcps_read_velfun                            &
                         (dio,VFID,NPICKS,X,Y,CTVF,T,V,                 &
                          PROJECT,LINE,RDATE,PDATE,USERID,COMMENT,      &
                          err,msg)
        implicit none
        type(dio_struct),intent(inout) :: dio                  ! arguments
        integer         ,intent(out)   :: npicks               ! arguments
        real            ,intent(out)   :: x,y                  ! arguments
        real ,optional  ,intent(out)   :: T(:),V(:)            ! arguments
        CHARACTER(len=*),intent(out)   :: VFID,CTVF            ! arguments
        CHARACTER(len=*),intent(out)   :: PROJECT,LINE         ! arguments
        CHARACTER(len=*),intent(out)   :: RDATE,PDATE          ! arguments
        character(len=*),intent(out)   :: USERID               ! arguments
        character(len=*),intent(out)   :: COMMENT              ! arguments
        integer         ,intent(out)   :: err                  ! arguments
        CHARACTER(len=*),intent(out)   :: msg                  ! arguments
        CHARACTER(len=120)             :: CARD                 ! local
        CHARACTER(len=40)              :: tokens(20)           ! local
        integer                        :: ntokens              ! local
        integer                        :: ncpv,i,j ! local

        call dio_read_card (dio,card)
        call dio_status    (dio,err,msg)
        if (err /= DIO_OK) return

        ncpv = scan(card(9:),'V')
        IF (NCPV.NE.0) THEN
             CTVF = CARD(9+NCPV-1:9+NCPV+2)
        ELSE
             CTVF = 'VTRM'
        ENDIF
        VFID      = CARD(:8)
        PROJECT   = CARD(40:49)
        LINE      = CARD(51:60)
        RDATE     = CARD(62:66)
        PDATE     = CARD(68:72)
        USERID    = CARD(74:76)
        COMMENT   = CARD(78:92)
        x = 0.0   ! added 5/7/98
        y = 0.0   ! added 5/7/98 since no error occurs if Y is missing!
        READ (CARD(9:),*,END=110) NPICKS,X,Y
 110    CONTINUE
        if (present(t)) then
             if (size(t) < npicks) then
                  err = DIO_ERROR
                  msg = 'size of time and velocity arrays too small'
                  return
             end if
        end if
        if (present(v)) then
             if (size(v) < npicks) then
                  err = DIO_ERROR
                  msg = 'size of time and velocity arrays too small'
                  return
             end if
        end if

!!!!    READ (lfn,*,ERR=704,END=500) (T(I),V(I),I=1,NPICKS)
!!!!    The above reads multiple lines automatically.
!!!!    It is replaced by the following do loop.

        i = 0
        do
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg)
           if (err /= DIO_OK) return

           call string_replace_character (card, ',', ' ')
           call string_get_tokens        (card, tokens, ntokens, '-1.e-30')

           do j = 2,ntokens,2
                i = i + 1
                if (i > npicks) exit
                if (present(t)) t(i) = string_ss2ff (tokens(j-1))
                if (present(v)) v(i) = string_ss2ff (tokens(j  ))
           end do
           if (i == npicks) exit
        end do
        return
        end subroutine velio1_oldcps_read_velfun


!!------------------------ velio1 oldcps write velfun ----------------------!!
!!------------------------ velio1 oldcps write velfun ----------------------!!
!!------------------------ velio1 oldcps write velfun ----------------------!!


        subroutine velio1_oldcps_write_velfun                          &
                         (dio,VFID,NPICKS,X,Y,CTVF,T,V,                &
                          PROJECT,LINE,RDATE,PDATE,USERID,COMMENT,     &
                          err,msg)
        implicit none
        type(dio_struct),intent(inout) :: dio                  ! arguments
        integer         ,intent(in)    :: npicks               ! arguments
        real            ,intent(in)    :: x,y                  ! arguments
        real            ,intent(in)    :: T(*),V(*)            ! arguments
        CHARACTER(len=*),intent(in)    :: VFID,CTVF            ! arguments
        CHARACTER(len=*),intent(in)    :: PROJECT,LINE         ! arguments
        CHARACTER(len=*),intent(in)    :: RDATE,PDATE          ! arguments
        character(len=*),intent(in)    :: USERID               ! arguments
        character(len=*),intent(in)    :: COMMENT              ! arguments
        integer         ,intent(out)   :: err                  ! arguments
        CHARACTER(len=*),intent(out)   :: msg                  ! arguments
        CHARACTER(len=120):: CARD                                  ! local
!       integer           :: ncpv,i,j,k,maxct,maxcv,nvpc,nct,ncv   ! local
!       CHARACTER(len=13) :: CT(200),CV(200)                       ! local
!       character(len=16) :: fmt ! Added to remove non-ansi parts from formats.
        CHARACTER(len=40)            :: tokens(20)           ! local
        integer                      :: ntokens,ndec,i,j     ! local

        if (ctvf /= 'VTNM' .and. ctvf /= 'VTDP' .and.                       &
            ctvf /= 'VTRM' .and. ctvf /= 'VTAV' .and. ctvf /= 'VTIN' .and.  &
            ctvf /= 'VZRM' .and. ctvf /= 'VZAV' .and. ctvf /= 'VZIN' .and.  &
            ctvf /= 'VLRM' .and. ctvf /= 'VLAV' .and. ctvf /= 'VLIN') then
          ERR = DIO_ERROR
          msg = 'illegal velocity function type - function not written'
          return
        end if
        WRITE (CARD(9:),'(1X,I4,1X,F9.1,1X,F9.1,1X,A4)') NPICKS,X,Y,CTVF
        CARD(:8)    = VFID 
        CARD(39:39) = ' '
        CARD(40:)   = PROJECT
        CARD(51:)   = LINE
        CARD(62:)   = RDATE
        CARD(68:)   = PDATE
        CARD(74:)   = USERID
        CARD(78:)   = COMMENT

        call dio_write_card (dio,card)
        call dio_status     (dio,err,msg)
        if (err /= DIO_OK) return
        
        if (ctvf(1:2) == 'VT') then ; ndec = 3
        else                        ; ndec = 0
        end if

        i = 0
        do
           ntokens = min(8, 2*(npicks-i))
           do j = 2,ntokens,2
                i = i + 1
                tokens(j-1) = string_ff2ss (t(i),ndec=ndec)
  !!            tokens(j  ) = string_ff2ss (v(i),ndec=0)
                tokens(j  ) = string_ff2ss (v(i))
  !! ndec=0 removed above 4/05/02 to allow bogus velocities with any value.
           end do
           call string_put_tokens (card, tokens, ntokens, '-1.e-30')
           call dio_write_card    (dio,card)
           call dio_status        (dio,err,msg)
           if (err /= DIO_OK) return
           if (i == npicks) exit
        end do

! The following code writes all picks automatically on as many card images
! as needed.  It is replaced by the above do loop.
!
!       write (fmt,'(A1,I3,A4)') '(',nvpc,'(A))'
!       WRITE (lfn,fmt,ERR=902)                                            &
!   (' '//CT(I     )(:MAXCT)//' '//CV(I     )(:MAXCV)//',',I=1,NPICKS-1),  &
!    ' '//CT(NPICKS)(:MAXCT)//' '//CV(NPICKS)(:MAXCV)

        return
        end subroutine velio1_oldcps_write_velfun


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module velio1_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
