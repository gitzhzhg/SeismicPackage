
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- fioutil.f90 -------------------------------!!
!!------------------------------- fioutil.f90 -------------------------------!!
!!------------------------------- fioutil.f90 -------------------------------!!


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
! Name       : FIOUTIL    (utility which supports the FIO primitive)
! Category   : io
! Written    : 2001-12-28   by: Tom Stoeckley
! Revised    : 2002-07-19   by: Tom Stoeckley
! Maturity   : production   2002-08-19
! Purpose    : To help read and write self-defining non-trace files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION               
!
! This primitive is designed to be used in conjunction with the FIO primitive,
! which is used for reading and writing self-defining sequential non-trace
! files.
!
! This primitive is not an object and does not have its own data structure.
!
! This primitive is used by FIO and by higher-level I/O routines to augment
! and verify the contents of a pickle jar.  The pickle jar is a PJAR data
! structure which contains parameters for reading and writing files.
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
!               SUBROUTINES CALLED FROM THE FIO PRIMITIVE
!
! Augment the FIO parameters in all of the sections in PJAR:
!
!                   call fioutil_augment_all (pjar)
!                                              b
!
! Verify the accuracy of the FIO parameters in a single section in PJAR:
!
!                call fioutil_verify (pjar,secname,msg)
!                                      b      i     o
!
!                              +++++++++++++
!
! type(pjar_struct)  pjar = reference to the PJAR data structure.
! char(*)         secname = name of the PJAR section to access.
! char(*)             msg = message for possible printing (blank if no error).
!
!                              +++++++++++++
!
! FIOUTIL_AUGMENT_ALL:
!  (1) augments FIO parameters for all PJAR sections.
!  (2) augments all header sections with encoding ascii or binary or hybrid.
!  (3) called from FIO just before writing header sections to disk.
!  (4) calls FIOUTIL_AUGMENT3 for each section with one of the above encodings.
!
! FIOUTIL_VERIFY:
!  (1) verifies FIO parameters for a single PJAR section.
!  (2) chooses the PJAR section name.
!  (3) called from FIO while preparing to write a data section  to  disk.
!  (4) called from FIO while preparing to read  a data section from disk.
!  (5) encoding must be ascii or binary or hybrid.
!  (6) returns an appropriate error message (or blank if no error).
!
!-------------------------------------------------------------------------------
!           SUBROUTINES CALLED FROM A HIGH-LEVEL I/O PRIMITIVE
!
! Augment the FIO and HIGH-LEVEL parameters in a single section in PJAR:
!
!                              b      i       i
!      call fioutil_augment1 (pjar,secname,encoding)
!
!      call fioutil_augment2 (pjar,field,defval,hdr,fieldtype,unit,delimiter)
!                              b     i     i     i      i      i       i
!                                               opt    opt    opt     opt
!
!      call fioutil_augment3 (pjar)
!                              b  
!
!
! Verify the accuracy of the HIGH-LEVEL parameters in a single section:
!
!                             b      i     o
!      call fioutil_verify1 (pjar,secname,msg)
!
!      call fioutil_verify2 (keyword,missing,invalid)
!                               i       i       i
!
!      call fioutil_verify3 (pjar,msg)
!                             b    o
!
!
! Verify the accuracy of the FIO parameters in a single section in PJAR:
!
!                call fioutil_verify (pjar,secname,msg)
!                                      b      i     o
!
!                              +++++++++++++
!
! type(pjar_struct)  pjar = reference to the PJAR data structure.
! char(*)         secname = name of the PJAR section to access.
! char(*)             msg = message for possible printing (blank if no error).
! char(*)        encoding = default encoding format for this section.
! char(*)           field = field name (name of column) to read or write.
! (any type)       defval = default value for this field.
! integer             hdr = header word number corresponding to this field.
! char(*)       fieldtype = type of this field.
! char(*)            unit = unit corresponding to this field.
! logical       delimiter = whether this field is a data packet delimiter.
! logical         missing = whether this parameter is missing (or nil).
! logical         invalid = whether this parameter is invalid.
!
!                              +++++++++++++
!
! FIOUTIL_AUGMENT1:
! FIOUTIL_AUGMENT2:
! FIOUTIL_AUGMENT3:
!  (1) augments FIO and HIGH-LEVEL parameters for a single PJAR section.
!  (2) called from a HIGH-LEVEL class before writing a header section to disk.
!  (3) called from a GUI while preparing a header section for writing to disk.
!  (4) encoding must be ascii or binary or hybrid or some value associated
!       with file formats not known by FIO (such as oldcps).
!
! FIOUTIL_AUGMENT1:
!  (1) chooses the PJAR section name.
!  (2) sets ENCODING to the default value if not previously specified.
!  (3) sets NCOLUMNS to NFIELDS if NCOLUMNS has not yet been specified but
!       some fields have already been specified.
!
! FIOUTIL_AUGMENT2:
!  (1) resets certain array elements associated with certain fields.
!  (2) should be called for each possible field name.
!  (3) must be called between FIOUTIL_AUGMENT1 and FIOUTIL_AUGMENT3.
!
! FIOUTIL_AUGMENT3:
!  (1) rearranges the order of the FIO parameters.
!  (2) fills out all FIO field arrays to the final number of fields.
!  (3) sets NCOLUMNS to NFIELDS if it has not previously been set > zero.
!  (4) also called by FIO (indirectly through FIOUTIL_AUGMENT_ALL).
!
!
! FIOUTIL_VERIFY1:
! FIOUTIL_VERIFY2:
! FIOUTIL_VERIFY3:
! FIOUTIL_VERIFY:
!  (1) assists in verifying HIGH-LEVEL parameters for a single PJAR section.
!  (2) called from HIGH-LEVEL class after  reading a header section from disk.
!  (3) called from HIGH-LEVEL class before reading a  data  section from disk.
!  (4) called from HIGH-LEVEL class before writing a header section  to  disk.
!
! FIOUTIL_VERIFY1:
!  (1) chooses the PJAR section name.
!  (2) initializes some static variables for collecting error messages.
!
! FIOUTIL_VERIFY2:
!  (1) collects a possible error message associated with a single parameter.
!  (2) should be called for each HIGH-LEVEL parameter which could have an error.
!  (3) must be called between FIOUTIL_VERIFY1 and FIOUTIL_VERIFY3.
!
! FIOUTIL_VERIFY3:
!  (1) returns an appropriate error message (or blank if no error).
!
!
! FIOUTIL_VERIFY:
!  (1) verifies FIO parameters for a single PJAR section.
!  (2) chooses the PJAR section name.
!  (3) called from FIO while preparing to write a data section  to  disk.
!  (4) called from FIO while preparing to read  a data section from disk.
!  (5) encoding must be ascii or binary or hybrid.
!  (6) returns an appropriate error message (or blank if no error).
!
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! A high-level I/O primitive should contain two subroutines for augmenting
! and verifying parameters.  Here are skeleton examples:
!
!
!     subroutine statio_augment (pjar,secname)
!
!        call fioutil_augment1 (pjar, secname, encoding)
!
!        call fioutil_augment2 (pjar,field,defval,hdr,fieldtype,unit,delimiter)
!        call fioutil_augment2 (pjar,field,defval,hdr,fieldtype,unit,delimiter)
!        call fioutil_augment2 (pjar,field,defval,hdr,fieldtype,unit,delimiter)
!        call fioutil_augment2 (pjar,field,defval,hdr,fieldtype,unit,delimiter)
!
!        call fioutil_augment3 (pjar)
!
!     end subroutine statio_augment
!
!
!
!     subroutine statio_verify (pjar,secname,err,msg)
!
!        call fioutil_verify1 (pjar,secname,msg)
!
!        call pjar_augment    (pjar,keyword1,defval)
!        call pjar_augment    (pjar,keyword2,defval)
!        call pjar_augment    (pjar,keyword3,defval)
!        call pjar_augment    (pjar,keyword4,defval)
!        call pjar_augment    (pjar,keyword5,defval)
!
!        call pjar_get        (pjar,keyword4,value)
!        call pjar_get        (pjar,keyword5,value)
!        call pjar_get        (pjar,keyword6,value)
!        call pjar_get        (pjar,keyword7,value)
!
!        call fioutil_verify2 (keyword4,missing,invalid)
!        call fioutil_verify2 (keyword5,missing,invalid)
!        call fioutil_verify2 (keyword6,missing,invalid)
!        call fioutil_verify2 (keyword7,missing,invalid)
!
!        call fioutil_verify3 (pjar,msg)
!
!        if (encoding /= oldcps) call fioutil_verify (pjar,secname,msg)
!
!     end subroutine statio_verify
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
!  3. 2002-08-19  Stoeckley  Change fioutil_augment3 not to reset address to 0.
!  2. 2002-04-11  Stoeckley  Add MSG argument to FIOUTIL_VERIFY1 and improve
!                             informational/error messages; these are needed
!                             to support workstation applications.
!  1. 2002-02-04  Stoeckley  Initial version, taken from some code in FIO and
!                             some code in STATIO.
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
 
 
      module fioutil_module
      use string_module
      use pjar_module
      implicit none
      public
      private :: fioutil_private_augment2

      character(len=100),public,save :: FIOUTIL_IDENT = &
'$Id: fioutil.f90,v 1.3 2002/08/14 16:05:08 Stoeckley prod sps $'


      interface fioutil_augment2
           module procedure fioutil_augment2_ivar
           module procedure fioutil_augment2_fvar
           module procedure fioutil_augment2_dvar
           module procedure fioutil_augment2_cvar
           module procedure fioutil_augment2_lvar
      end interface

      character(len=*),public,parameter :: FIOUTIL_ASCII      = 'ascii'
      character(len=*),public,parameter :: FIOUTIL_BINARY     = 'binary'
      character(len=*),public,parameter :: FIOUTIL_HYBRID     = 'hybrid'

      integer         ,public,parameter :: FIOUTIL_MAXCOLUMNS = 50

      integer          ,private,save :: bad1,bad2  
      character(len=80),private,save :: list1,list2


      contains

 
!!------------------------------- augment all ------------------------------!!
!!------------------------------- augment all ------------------------------!!
!!------------------------------- augment all ------------------------------!!


      subroutine fioutil_augment_all (pjar)
      implicit none
      type(pjar_struct),intent(inout) :: pjar                      ! argument
      integer                         :: isection,nsections        ! local

      nsections = pjar_num_sections (pjar)
      do isection = 1,nsections
           call pjar_choose_section (pjar,isection)
           call fioutil_augment3    (pjar)
      end do
      call pjar_choose_no_section (pjar)
      return
      end subroutine fioutil_augment_all
 

!!-------------------------------- augment1 ---------------------------------!!
!!-------------------------------- augment1 ---------------------------------!!
!!-------------------------------- augment1 ---------------------------------!!


      subroutine fioutil_augment1 (pjar,secname,encoding)
      implicit none
      type(pjar_struct),intent(inout) :: pjar                      ! argument
      character(len=*) ,intent(in)    :: secname                   ! argument
      character(len=*) ,intent(in)    :: encoding                  ! argument
      integer                         :: ncolumns,nfields          ! local

      call pjar_choose_section (pjar, secname)
      call pjar_augment        (pjar, 'encoding', encoding)
      call pjar_get            (pjar, 'ncolumns', ncolumns, 0)

      nfields = pjar_num_elements (pjar, 'fields')

      if (ncolumns <= 0) ncolumns = nfields

      call pjar_put (pjar, 'ncolumns', ncolumns)
      return
      end subroutine fioutil_augment1
 
 
!!-------------------------------- augment3 ---------------------------------!!
!!-------------------------------- augment3 ---------------------------------!!
!!-------------------------------- augment3 ---------------------------------!!

 
      subroutine fioutil_augment3 (pjar)
      implicit none
      type(pjar_struct),intent(inout) :: pjar                        ! argument
      character(len=8)                :: encoding                       ! local
      character(len=30)               :: nilstring                      ! local
      integer                         :: wrap                           ! local
      integer                         :: ncolumns                       ! local
      logical                         :: fillout                        ! local
      character(len=30)               :: fields    (FIOUTIL_MAXCOLUMNS) ! local
      character(len=30)               :: defaults  (FIOUTIL_MAXCOLUMNS) ! local
      integer                         :: hdrs      (FIOUTIL_MAXCOLUMNS) ! local
      character(len=30)               :: fieldtypes(FIOUTIL_MAXCOLUMNS) ! local
      character(len=30)               :: units     (FIOUTIL_MAXCOLUMNS) ! local
      character(len=1)                :: vartypes  (FIOUTIL_MAXCOLUMNS) ! local
      logical                         :: delimiters(FIOUTIL_MAXCOLUMNS) ! local
      integer                         :: widths    (FIOUTIL_MAXCOLUMNS) ! local
      integer                         :: nfields,ndummy,i               ! local
      integer                         :: nlines,npackets,maxpicks       ! local
      integer                         :: address(2)                     ! local

      call pjar_get (pjar,'encoding',encoding)
      nfields = pjar_num_elements (pjar,'fields')

      if (encoding /= FIOUTIL_ASCII  .and. &
          encoding /= FIOUTIL_BINARY .and. &
          encoding /= FIOUTIL_HYBRID .and. &
          nfields == 0) return

      call pjar_get (pjar, 'nilstring'  , nilstring          , 'nil'  )
      call pjar_get (pjar, 'wrap'       , wrap               , 1      )
      call pjar_get (pjar, 'ncolumns'   , ncolumns           , 0      )
      call pjar_get (pjar, 'fillout'    , fillout            , .false.)
      call pjar_get (pjar, 'fields'     , fields    , nfields, ' '    )
      call pjar_get (pjar, 'defaults'   , defaults  , ndummy , ' '    )
      call pjar_get (pjar, 'hdrs'       , hdrs      , ndummy , 0      )
      call pjar_get (pjar, 'fieldtypes' , fieldtypes, ndummy , ' '    )
      call pjar_get (pjar, 'units'      , units     , ndummy , ' '    )
      call pjar_get (pjar, 'vartypes'   , vartypes  , ndummy , ' '    )
      call pjar_get (pjar, 'delimiters' , delimiters, ndummy , .false.)
      call pjar_get (pjar, 'widths'     , widths    , ndummy , 0      )
      call pjar_get (pjar, 'nlines'     , nlines             , 0      )
      call pjar_get (pjar, 'npackets'   , npackets           , 0      )
      call pjar_get (pjar, 'maxpicks'   , maxpicks           , 0      )
      call pjar_get (pjar, 'address'    , address   , ndummy , 0      )

      call pjar_remove_keyword (pjar, 'encoding'   )
      call pjar_remove_keyword (pjar, 'nilstring'  )
      call pjar_remove_keyword (pjar, 'wrap'       )
      call pjar_remove_keyword (pjar, 'ncolumns'   )
      call pjar_remove_keyword (pjar, 'fillout'    )
      call pjar_remove_keyword (pjar, 'fields'     )
      call pjar_remove_keyword (pjar, 'defaults'   )
      call pjar_remove_keyword (pjar, 'hdrs'       )
      call pjar_remove_keyword (pjar, 'fieldtypes' )
      call pjar_remove_keyword (pjar, 'units'      )
      call pjar_remove_keyword (pjar, 'vartypes'   )
      call pjar_remove_keyword (pjar, 'delimiters' )
      call pjar_remove_keyword (pjar, 'widths'     )
      call pjar_remove_keyword (pjar, 'nlines'     )
      call pjar_remove_keyword (pjar, 'npackets'   )
      call pjar_remove_keyword (pjar, 'maxpicks'   )
      call pjar_remove_keyword (pjar, 'address'    )

      if (ncolumns > 0 .and. nfields == 0) then
           nfields = ncolumns
           do i = 1,nfields
                fields(i) = 'field'//string_ii2ss(i)
           end do
      end if

      if (ncolumns <= 0 .or. ncolumns > nfields) ncolumns = nfields
      if (wrap <        1) wrap = 1
      if (wrap > ncolumns) wrap = ncolumns

      call pjar_put (pjar, 'encoding'   , encoding           )
      call pjar_put (pjar, 'nilstring'  , nilstring          )
      call pjar_put (pjar, 'wrap'       , wrap               )
      call pjar_put (pjar, 'ncolumns'   , ncolumns           )
      call pjar_put (pjar, 'fillout'    , fillout            )
      call pjar_put (pjar, 'fields'     , fields     ,nfields)
      call pjar_put (pjar, 'defaults'   , defaults   ,nfields)
      call pjar_put (pjar, 'hdrs'       , hdrs       ,nfields)
      call pjar_put (pjar, 'fieldtypes' , fieldtypes ,nfields)
      call pjar_put (pjar, 'units'      , units      ,nfields)
      call pjar_put (pjar, 'vartypes'   , vartypes   ,nfields)
      call pjar_put (pjar, 'delimiters' , delimiters ,nfields)
      call pjar_put (pjar, 'widths'     , widths     ,nfields)
      call pjar_put (pjar, 'nlines'     , nlines             )
      call pjar_put (pjar, 'npackets'   , npackets           )
      call pjar_put (pjar, 'maxpicks'   , maxpicks           )
  !   call pjar_put (pjar, 'address'    , (/0,0/)    ,      2)
      call pjar_put (pjar, 'address'    , address    ,      2)
      return
      end subroutine fioutil_augment3
 
 
!!------------------------ private augment2 ---------------------------------!!
!!------------------------ private augment2 ---------------------------------!!
!!------------------------ private augment2 ---------------------------------!!

 
      subroutine fioutil_private_augment2 &
                     (pjar,field,defval,hdr,fieldtype,unit,delimiter,vartype)
      implicit none
      type(pjar_struct),intent(inout)       :: pjar               ! argument
      character(len=*) ,intent(in)          :: field              ! argument
      character(len=*) ,intent(in)          :: defval             ! argument
      integer          ,intent(in),optional :: hdr                ! argument
      character(len=*) ,intent(in),optional :: fieldtype          ! argument
      character(len=*) ,intent(in),optional :: unit               ! argument
      logical          ,intent(in),optional :: delimiter          ! argument
      character(len=*) ,intent(in)          :: vartype            ! argument
      integer                               :: indx               ! local

      indx = pjar_find_add (pjar, 'fields', field)

                        call pjar_put_element (pjar,'defaults'  ,indx, defval )
                        call pjar_put_element (pjar,'hdrs'      ,indx, 0      )
                        call pjar_put_element (pjar,'fieldtypes',indx, ' '    )
                        call pjar_put_element (pjar,'units'     ,indx, ' '    )
                        call pjar_put_element (pjar,'delimiters',indx, .false.)
                        call pjar_put_element (pjar,'vartypes'  ,indx, vartype)

 if(present(hdr      )) call pjar_put_element (pjar,'hdrs'      ,indx,hdr      )
 if(present(fieldtype)) call pjar_put_element (pjar,'fieldtypes',indx,fieldtype)
 if(present(unit     )) call pjar_put_element (pjar,'units'     ,indx,unit     )
 if(present(delimiter)) call pjar_put_element (pjar,'delimiters',indx,delimiter)

      return
      end subroutine fioutil_private_augment2
 
 
!!-------------------------------- augment2 ---------------------------------!!
!!-------------------------------- augment2 ---------------------------------!!
!!-------------------------------- augment2 ---------------------------------!!


      subroutine fioutil_augment2_ivar &
                            (pjar,field,defval,hdr,fieldtype,unit,delimiter)
      implicit none
      type(pjar_struct),intent(inout)       :: pjar               ! argument
      character(len=*) ,intent(in)          :: field              ! argument
      integer          ,intent(in)          :: defval             ! argument
      integer          ,intent(in),optional :: hdr                ! argument
      character(len=*) ,intent(in),optional :: fieldtype          ! argument
      character(len=*) ,intent(in),optional :: unit               ! argument
      logical          ,intent(in),optional :: delimiter          ! argument

      call fioutil_private_augment2 &
             (pjar,field,string_ii2ss(defval),hdr,fieldtype,unit,delimiter,'I')
      return
      end subroutine fioutil_augment2_ivar



      subroutine fioutil_augment2_fvar &
                            (pjar,field,defval,hdr,fieldtype,unit,delimiter)
      implicit none
      type(pjar_struct),intent(inout)       :: pjar               ! argument
      character(len=*) ,intent(in)          :: field              ! argument
      real             ,intent(in)          :: defval             ! argument
      integer          ,intent(in),optional :: hdr                ! argument
      character(len=*) ,intent(in),optional :: fieldtype          ! argument
      character(len=*) ,intent(in),optional :: unit               ! argument
      logical          ,intent(in),optional :: delimiter          ! argument

      call fioutil_private_augment2 &
             (pjar,field,string_ff2ss(defval),hdr,fieldtype,unit,delimiter,'F')
      return
      end subroutine fioutil_augment2_fvar



      subroutine fioutil_augment2_dvar &
                            (pjar,field,defval,hdr,fieldtype,unit,delimiter)
      implicit none
      type(pjar_struct),intent(inout)       :: pjar               ! argument
      character(len=*) ,intent(in)          :: field              ! argument
      double precision ,intent(in)          :: defval             ! argument
      integer          ,intent(in),optional :: hdr                ! argument
      character(len=*) ,intent(in),optional :: fieldtype          ! argument
      character(len=*) ,intent(in),optional :: unit               ! argument
      logical          ,intent(in),optional :: delimiter          ! argument

      call fioutil_private_augment2 &
             (pjar,field,string_dd2ss(defval),hdr,fieldtype,unit,delimiter,'D')
      return
      end subroutine fioutil_augment2_dvar



      subroutine fioutil_augment2_cvar &
                            (pjar,field,defval,hdr,fieldtype,unit,delimiter)
      implicit none
      type(pjar_struct),intent(inout)       :: pjar               ! argument
      character(len=*) ,intent(in)          :: field              ! argument
      character(len=*) ,intent(in)          :: defval             ! argument
      integer          ,intent(in),optional :: hdr                ! argument
      character(len=*) ,intent(in),optional :: fieldtype          ! argument
      character(len=*) ,intent(in),optional :: unit               ! argument
      logical          ,intent(in),optional :: delimiter          ! argument

      call fioutil_private_augment2 &
                     (pjar,field,defval,hdr,fieldtype,unit,delimiter,'C')
      return
      end subroutine fioutil_augment2_cvar



      subroutine fioutil_augment2_lvar &
                            (pjar,field,defval,hdr,fieldtype,unit,delimiter)
      implicit none
      type(pjar_struct),intent(inout)       :: pjar               ! argument
      character(len=*) ,intent(in)          :: field              ! argument
      logical          ,intent(in)          :: defval             ! argument
      integer          ,intent(in),optional :: hdr                ! argument
      character(len=*) ,intent(in),optional :: fieldtype          ! argument
      character(len=*) ,intent(in),optional :: unit               ! argument
      logical          ,intent(in),optional :: delimiter          ! argument

      call fioutil_private_augment2 &
             (pjar,field,string_ll2ss(defval),hdr,fieldtype,unit,delimiter,'L')
      return
      end subroutine fioutil_augment2_lvar


!!---------------------------- verify -------------------------------------!!
!!---------------------------- verify -------------------------------------!!
!!---------------------------- verify -------------------------------------!!


      subroutine fioutil_verify (pjar,secname,msg)
      implicit none
      type(pjar_struct),intent(inout) :: pjar                        ! argument
      character(len=*) ,intent(in)    :: secname                     ! argument
      character(len=*) ,intent(out)   :: msg                         ! argument
      character(len=8)                :: encoding                    ! local
      character(len=30)               :: fields(FIOUTIL_MAXCOLUMNS)  ! local
      logical                         :: skip  (FIOUTIL_MAXCOLUMNS)  ! local
      integer                         :: nfields,nskip,indx,jndx     ! local
      integer                         :: ncolumns                    ! local

      indx = pjar_find_section (pjar, secname)
      if (indx == 0) then
           msg = 'missing header section '//string_2_upper(secname)
           return
      end if

      if (pjar_num_keywords(pjar) == 0) then
           msg = 'empty header section '//string_2_upper(secname)
           return
      end if

      skip(:) = .false.

      call pjar_get    (pjar, 'ncolumns', ncolumns, 0)
      call pjar_get    (pjar, 'encoding', encoding)
      call pjar_get    (pjar, 'fields', fields, nfields)
      call pjar_get    (pjar, 'skip'  , skip  , nskip, .false.)
      call pjar_status (pjar, msg)

      if (msg /= ' ') return

      if (encoding /= FIOUTIL_ASCII  .and. &
          encoding /= FIOUTIL_BINARY .and. &
          encoding /= FIOUTIL_HYBRID) then
           msg = 'illegal or missing encoding'
           return
      end if

      if (nfields == 0) then
           msg = 'no field names specified'
           return
      end if

! The following code has been changed on 3/21/2002 to test for blank
! and duplicate fields only among fields that are not skipped, and
! fields <= ncolumns:

      if (ncolumns > 0 .and. ncolumns < nfields) nfields = ncolumns

      do indx = 1,nfields
           if (skip(indx)) cycle;
           if (fields(indx) == ' ') then
                msg = 'field names cannot be blank'
                return
           end if
      end do

      do indx = 1,nfields
      do jndx = indx+1,nfields
           if (skip(indx)) cycle;
           if (skip(jndx)) cycle;
           if (fields(indx)(1:1) == '-') cycle
           if (fields(indx) == fields(jndx)) then
                msg = 'two field names cannot be the same'
                return
           end if
      end do
      end do

      msg = ' '
      return
      end subroutine fioutil_verify
 

!!------------------------------- verify1 ----------------------------------!!
!!------------------------------- verify1 ----------------------------------!!
!!------------------------------- verify1 ----------------------------------!!


      subroutine fioutil_verify1 (pjar,secname,msg)
      implicit none
      type(pjar_struct),intent(inout) :: pjar               ! arguments
      character(len=*) ,intent(in)    :: secname            ! arguments
      character(len=*) ,intent(out)   :: msg                ! arguments
      integer                         :: indx               ! local

      indx = pjar_find_section (pjar, secname)
      if (indx == 0) then
           msg = 'section '//trim(string_2_upper(secname))//' not found'
           return
      end if
      msg   = ' '

      bad1  = 0            ! initialize static variable.
      bad2  = 0            ! initialize static variable.
      list1 = ' '          ! initialize static variable.
      list2 = ' '          ! initialize static variable.
      return
      end subroutine fioutil_verify1


!!------------------------------- verify2 ----------------------------------!!
!!------------------------------- verify2 ----------------------------------!!
!!------------------------------- verify2 ----------------------------------!!


      subroutine fioutil_verify2 (keyword, missing, invalid)
      implicit none
      character(len=*) ,intent(in)          :: keyword         ! argument
      logical          ,intent(in)          :: missing         ! argument
      logical          ,intent(in),optional :: invalid         ! argument

      if (missing) then
           bad1  = bad1 + 1
           list1 = trim(list1)//' '//string_2_upper(keyword)
      else if (.not.present(invalid)) then
           continue
      else if (invalid) then
           bad2  = bad2 + 1
           list2 = trim(list2)//' '//string_2_upper(keyword)
      end if
      return
      end subroutine fioutil_verify2


!!------------------------------- verify3 ----------------------------------!!
!!------------------------------- verify3 ----------------------------------!!
!!------------------------------- verify3 ----------------------------------!!


      subroutine fioutil_verify3 (pjar,msg)
      implicit none
      type(pjar_struct),intent(inout) :: pjar               ! argument
      character(len=*) ,intent(out)   :: msg                ! argument

      if (bad1 == 1) then
           msg = '(file has the following missing item:'//trim(list1)//')'
      else if (bad1 > 5) then
           msg = '(file has '//trim(string_ii2ss(bad1))//' missing items)'
      else if (bad1 > 0) then
           msg = '(file has '//trim(string_ii2ss(bad1))//' missing items:' &
                             //trim(list1)//')'
      else if (bad2 == 1) then
           msg = '(file has the following invalid item:'//trim(list2)//')'
      else if (bad2 > 5) then
           msg = '(file has '//trim(string_ii2ss(bad2))//' invalid items)'
      else if (bad2 > 0) then
           msg = '(file has '//trim(string_ii2ss(bad2))//' invalid items:' &
                             //trim(list2)//')'
      else
           msg = ' '
      end if
      return
      end subroutine fioutil_verify3


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module fioutil_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

