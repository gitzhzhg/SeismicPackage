!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ mapsegy.f90 --------------------------------!!
!!------------------------------ mapsegy.f90 --------------------------------!!
!!------------------------------ mapsegy.f90 --------------------------------!!


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
! Name       : MAPSEGY 
! Category   : io
! Written    : 2001-03-19   by: Karen Goodger
! Revised    : 2010-04-01   by: Bill Menger
! Maturity   : production
! Purpose    : Map segy header words to cps header words.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive is to be used by process modules which need to may segy
! header words to cps header words, such as trin and ttrin.  A default 
! header mapping is provided for Landmark, Geodepth, Jason, and Hampson-
! Russell formats.  The user can alter this mapping if necessary as well
! as provide the entire mapping structure.
!
! This primitive uses the parameter cache to read and write the parameters
! it needs, to report error messages, etc.
!
! To use this primitive from a process module named xxxx:
!
!    (1) mapsegy_create     should be called from xxxx_create.
!    (2) mapsegy_initialize should be called from xxxx_initialize.
!    (3) mapsegy_update     should be called from xxxx_update.
!    (4) mapsegy            should be called from xxxx.
!    (5) mapsegy_delete     should be called from xxxx_delete.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name       Description                             Action taken
! ----       -----------                             ------------
!
! NWIH       Number of words in trace header         used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#       Description                            Action taken
! ----       -----------                            ------------
! CPS_HDR    Number specified by user               changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>


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
!                                         
!                                    o     
!           call mapsegy_create     (obj)
!
!                                    b
!           call mapsegy_initialize (obj)
!
!                                        
!                                    b    
!           call mapsegy_update     (obj)
!
!                                             opt opt   opt   opt    opt
!                                    i  i  i   o   o     o     o      o
!           call mapsegy            (obj,hd,tr,win,nwin,nlive,index1,index2)
!
!                                    b
!           call mapsegy_delete     (obj)
!
! type(mapsegy_struct) obj    = pointer to the segymap structure.
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! The GUI_DEF section for a process which uses this primitive should contain
! an INCLUDE line which says to include the GUI_DEF section of this primitive.
! The HELPSECTION for the parameters in this primitive will also be made
! available to the process when this is done.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  8. 2010-04-01  Bill Menger  Changed default back to STAN (basic SU-like mapping)
!  7. 2006-08-24  Stoeckley    Replace pc_register_tab_group w HelpSection line.
!  6. 2006-06-20  Stoeckley    Add pc_register_tab_group for SeisSpace.
!  5. 2006-06-01  Stoeckley    Add pc_register_array_names for SeisSpace.
!  4. 2006-05-25  Bill Menger  Modify default to LANDMARK instead of STAN.
!  3. 2004-01-21  K. Goodger   Make variable object private to avoid naming
!                              conflicts.
!  2. 2001-06-18  K. Goodger   Split hampson-russell segy mapping into 2d and
!                              3d.  PRODUCTION.
!  1. 2001-04-30  K. Goodger   Initial version.
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


!-------------------------------------------------------------------------------
!<gui_def>
! -------------------------------------------------------------------
!                     MOD_SEGY = `CCCCCCCCCCCCCCC
!
!                      SBYTE BYTES CPS_HDR MTYPE
!                      `IIIII`IIIII`IIIIIII`SSSS
!                      `IIIII`IIIII`IIIIIII`SSSS
!                      `IIIII`IIIII`IIIIIII`SSSS
!                      `IIIII`IIIII`IIIIIII`SSSS
! -------------------------------------------------------------------
!<PARMS SBYTE_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------
!<HelpSection>
!
!     tabgroup = SEGY Mapping
!
!<Help KEYWORD="MOD_SEGY">
!<Tip> Type of segy mapping. </Tip>
! Default = STAN     
! Allowed = STAN      (Standard SEGY to CPS mapping.
! Allowed = LANDMARK 
! Allowed = GEODEPTH 
! Allowed = JASON 
! Allowed = HAMPSON-RUSSELL-2D
! Allowed = HAMPSON-RUSSELL-3D
! Allowed = USER      (User specified mapping) 
!</Help>
!
!<Help KEYWORD="SBYTE">
!<Tip> Starting byte of the SEGY header to be moved. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
! Active if MOD_SEGY=USER.
!</Help>
!
!<Help KEYWORD="BYTES">
!<Tip> Number of bytes to be moved to a CPS header. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
! Active if MOD_SEGY=USER.
!</Help>
!
!<Help KEYWORD="CPS_HDR">
!<Tip> CPS header word to receive the SEGY header. </Tip>
! Default = -
! Allowed = 1 to NWIH (linked array)
! Active if MOD_SEGY=USER.
!</Help>
!
!<Help KEYWORD="MTYPE">
!<Tip> Treat Segy header word as I(integer), or F(floating point). </Tip>
! Default = -
! Allowed = char(8) (linked array)
! Active if MOD_SEGY=USER.
!</Help>
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module mapsegy_module
      use named_constants_module
      use pc_module
      use trcio_module
      implicit none
      public

      character(len=100),public,save :: MAPSEGY_IDENT = &
       '$Id: mapsegy.f90,v 1.7 2006/08/25 13:03:05 Stoeckley beta sps $'


      type,public :: mapsegy_struct              

       private
       character(len=20)          :: mod_segy  
       integer                    :: nummap    !number of segy headers to map
       integer,pointer            :: sbyte(:)  !byte loc. of segy header,from 1
       integer,pointer            :: bytes(:)   !number of bytes in segy header
       integer,pointer            :: cps_hdr(:) !map to this cps header word
       character(len=8),pointer   :: mtype(:)   !word type of the segy header

      end type mapsegy_struct

      type(mapsegy_struct),pointer,save,private :: object   !needed for traps
      integer,parameter,private      :: noptions=7
      character(len=20),private,save :: mod_segy_options(noptions) &
                                         = (/'STAN                ',&
                                             'LANDMARK            ',&
                                             'GEODEPTH            ',&
                                             'JASON               ' ,&
                                             'HAMPSON-RUSSELL-2D  ',&
                                             'HAMPSON-RUSSELL-3D  ',&
                                             'USER                '/)

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine mapsegy_create (obj,no_mute)
      implicit none
      type(mapsegy_struct),pointer :: obj       ! arguments
      logical,optional,intent(in) :: no_mute   ! arguments

      allocate (obj)


      nullify (obj%sbyte)        ! process parameter
      nullify (obj%bytes)        ! process parameter
      nullify (obj%cps_hdr)      ! process parameter
      nullify (obj%mtype)        ! process parameter

      return
      end subroutine mapsegy_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine mapsegy_delete (obj)
      implicit none
      type(mapsegy_struct),pointer :: obj       ! arguments

      if (associated(obj%sbyte ))  deallocate (obj%sbyte )
      if (associated(obj%bytes ))  deallocate (obj%bytes )
      if (associated(obj%cps_hdr)) deallocate (obj%cps_hdr)
      if (associated(obj%mtype))   deallocate (obj%mtype)

      deallocate(obj)
      return
      end subroutine mapsegy_delete


!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!


      subroutine mapsegy_initialize (obj)
      implicit none
      type(mapsegy_struct)  :: obj                 ! arguments

      integer :: nwih

      call pc_get_global('nwih',nwih)



      if (associated(obj%sbyte      )) deallocate (obj%sbyte )
      if (associated(obj%bytes ))      deallocate (obj%bytes )
      if (associated(obj%cps_hdr))     deallocate (obj%cps_hdr)
      if (associated(obj%mtype))       deallocate (obj%mtype)

      allocate (obj%sbyte (nwih))
      allocate (obj%bytes (nwih))
      allocate (obj%cps_hdr(nwih))
      allocate (obj%mtype(nwih))

      obj%mod_segy  = 'STAN'
      obj%nummap       = 0 !number of segy headers to special map
      obj%sbyte        = 0 !starting byte location of segy header
      obj%bytes        = 0 !number of bytes in segy header
      obj%cps_hdr      = 0 !map to this cps header word
      obj%mtype        = 'I'  
      return
      !----- DEFAULT MODIFIED TO "STAN" by returning here.

      !----- LANDMARK MAPPING SHOWN BELOW --- wmm 
      obj%mod_segy  = 'LANDMARK'
      obj%sbyte(1)=21
      obj%bytes(1)=4
      obj%cps_hdr(1)=7

      obj%sbyte(2)=9
      obj%bytes(2)=4
      obj%cps_hdr(2)=8

      obj%sbyte(3)=73
      obj%bytes(3)=4
      obj%cps_hdr(3)=17

      obj%sbyte(4)=77
      obj%bytes(4)=4
      obj%cps_hdr(4)=18

      obj%sbyte(5)=17
      obj%bytes(5)=4
      obj%cps_hdr(5)=37

      obj%nummap=5
      !-------- END OF LANDMARK MAPPING
      return
      end subroutine mapsegy_initialize


!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!


      subroutine mapsegy_update (obj)
      implicit none
      type(mapsegy_struct) ,intent(inout),target :: obj    
      integer :: i,nummap1,nummap2,nummap3,nummap4,nwih

!----------read parameters:

      nummap1 = obj%nummap
      nummap2 = obj%nummap
      nummap3 = obj%nummap
      nummap4 = obj%nummap

      object => obj                 ! needed for traps

      call pc_register_array_names ('sbyte_arrayset', &
                        (/'SBYTE  ', 'BYTES  ', 'CPS_HDR', 'MTYPE  '/))

      call pc_get_global ('nwih'        , nwih                   ) 

      call pc_get      ('sbyte'  , obj%sbyte  ,nummap1)
      call pc_get      ('bytes'  , obj%bytes  ,nummap2)
      call pc_get      ('cps_hdr' , obj%cps_hdr ,nummap3)
      call pc_get      ('mtype' , obj%mtype ,nummap4)

      if (nummap2/=nummap1 .or. nummap3/=nummap1 .or. nummap4/=nummap1)then
           call pc_error ('SBYTE, BYTES, CPS_HDR, MTYPE'// &
                           ' arrays have different lengths')
           obj%nummap = min(nummap1,nummap2,nummap3,nummap4)
      else
           obj%nummap = nummap1
      end if

      call pc_get      ('mod_segy'    , obj%mod_segy,mapsegy_mod_segy_trap)

!----------verify parameters:

      do i = 1,obj%nummap
        call string_to_upper(obj%mtype(i))
        if(obj%mtype(i) /= 'F') obj%mtype(i)='I'
        if(obj%bytes(i)   < 2) obj%bytes(i)  =2
        if(obj%sbyte(i)   < 1) obj%sbyte(i)  =1
        if(obj%cps_hdr(i) < 0 .or. obj%cps_hdr(i) > nwih) then
         obj%cps_hdr(i)=0         !no mapping
        endif
      enddo

!            Yes and NO no longer valid answers
!      if(obj%mod_segy.eq.'NO')obj%mod_segy='STAN'
       if(obj%mod_segy.eq.'YES')obj%mod_segy='USER'


!----------write parameters:

      call pc_put_options_field ('mod_segy', &
                                  mod_segy_options, noptions)

      call pc_put('mod_segy' ,obj%mod_segy)
      call pc_put('sbyte'    ,obj%sbyte   ,obj%nummap)
      call pc_put('bytes'    ,obj%bytes   ,obj%nummap)
      call pc_put('cps_hdr'  ,obj%cps_hdr ,obj%nummap)
      call pc_put('mtype'    ,obj%mtype   ,obj%nummap)

      if(obj%mod_segy(1:4) .ne. 'USER') then
        call pc_put_sensitive_array_flag('sbyte_arrayset', .false.)
      else
        call pc_put_sensitive_array_flag('sbyte_arrayset', .true.)
      endif
      return
      end subroutine mapsegy_update

      subroutine mapsegy_mod_segy_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword

      select case (object%mod_segy)

        case('STAN')

          object%sbyte=0
          object%bytes=0
          object%cps_hdr=0
          object%mtype='I'
          object%nummap=0


        case ('LANDMARK') 

          object%sbyte(1)=21
          object%bytes(1)=4
          object%cps_hdr(1)=7

          object%sbyte(2)=9
          object%bytes(2)=4
          object%cps_hdr(2)=8

          object%sbyte(3)=73
          object%bytes(3)=4
          object%cps_hdr(3)=17

          object%sbyte(4)=77
          object%bytes(4)=4
          object%cps_hdr(4)=18

          object%sbyte(5)=17
          object%bytes(5)=4
          object%cps_hdr(5)=37

          object%nummap=5

        case ('GEODEPTH')

          object%sbyte(1)=21
          object%bytes(1)=4
          object%cps_hdr(1)=7

          object%sbyte(2)=25
          object%bytes(2)=4
          object%cps_hdr(2)=4

          object%sbyte(3)=37
          object%bytes(3)=4
          object%cps_hdr(3)=6

          object%sbyte(4)=73
          object%bytes(4)=4
          object%cps_hdr(4)=11

          object%sbyte(5)=77
          object%bytes(5)=4
          object%cps_hdr(5)=12

          object%sbyte(6)=81
          object%bytes(6)=4
          object%cps_hdr(6)=14

          object%sbyte(7)=85
          object%bytes(7)=4
          object%cps_hdr(7)=15

          object%sbyte(8)=185
          object%bytes(8)=4
          object%cps_hdr(8)=8

          object%nummap=8

        case ('JASON')

          object%sbyte(1)=21
          object%bytes(1)=4
          object%cps_hdr(1)=7

          object%sbyte(2)=17
          object%bytes(2)=4
          object%cps_hdr(2)=8

          object%sbyte(3)=73
          object%bytes(3)=4
          object%cps_hdr(3)=17

          object%sbyte(4)=77
          object%bytes(4)=4
          object%cps_hdr(4)=18

          object%nummap=4

        case ('HAMPSON-RUSSELL-2D')

          object%sbyte(1)=1
          object%bytes(1)=4
          object%cps_hdr(1)=1

          object%sbyte(2)=21
          object%bytes(2)=4
          object%cps_hdr(2)=7

          object%sbyte(3)=17
          object%bytes(3)=4
          object%cps_hdr(3)=37

          object%sbyte(4)=37
          object%bytes(4)=4
          object%cps_hdr(4)=6

          object%nummap=4
          
        case ('HAMPSON-RUSSELL-3D')

          object%sbyte(1)=1
          object%bytes(1)=4
          object%cps_hdr(1)=1

          object%sbyte(2)=21
          object%bytes(2)=4
          object%cps_hdr(2)=7

          object%sbyte(3)=17
          object%bytes(3)=4
          object%cps_hdr(3)=8

          object%sbyte(4)=37
          object%bytes(4)=4
          object%cps_hdr(4)=6

          object%nummap=4
          
 
      end select
      return
      end subroutine mapsegy_mod_segy_trap





!!------------------------ mapsegy get window -----------------------------!!
!!------------------------ mapsegy get window -----------------------------!!
!!------------------------ mapsegy get window -----------------------------!!


      subroutine mapsegy (obj,trciofile)
      implicit none
      type(mapsegy_struct) ,intent(in)      :: obj  
      type(trcio_struct),pointer            :: trciofile
      character(len=4) :: yesorno

      yesorno='YES'
      if(obj%mod_segy.eq.'STAN')yesorno='NO'


      call  trcio_set_map(trciofile,yesorno,obj%nummap,obj%sbyte,&
      obj%bytes,obj%mtype,obj%cps_hdr)

      return
      end subroutine mapsegy


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module mapsegy_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

