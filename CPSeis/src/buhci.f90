!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- buhci.f90 --------------------------------!!
!!------------------------------- buhci.f90 --------------------------------!!
!!------------------------------- buhci.f90 --------------------------------!!

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
! Name       : B_UHCI
! Category   : velocity
! Written    : 2003-08-26   by: Bill Lucas
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Universal Hydrocarbon Indicator (BUHCI) primitive.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive serves as the framework for the AVO Standard Suite
! module and the 2nd half of the AVO Velocity Iteration module.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
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
!!!  --> This is a REQUIRED section which CANNOT be omitted.
!!!
!!!  --> This section should list all subroutines and functions in this
!!!  --> module, and information about their arguments, in a format
!!!  --> similar to that illustrated here, customized as appropriate.
!!!
!!!  --> If this module contains several groups of related routines which
!!!  --> would be more clearly documented separately, or if this module
!!!  --> contains routines which require sufficient documentation to warrant
!!!  --> a separate section, there can be several sections such as this one,
!!!  --> appropriately individually titled.
!!!
!!!                o                i     b      o
!!!              hello = XXXX     (aaaa, cvar , msg)
!!!
!!!                                 i     b      o
!!!              call    XXXX_YYY (bbbb, cvar , msg)
!!!
!!!                                      opt    opt
!!!                                 i     i      o
!!!              call    XXXX_ZZZ (bbbb, indx, value)
!!!
!!!  --> The type and description of each argument should be shown in a table
!!!  --> such as that shown below.  If an argument is optional, it should be
!!!  --> so identified below as well as above, with an explanation of the
!!!  --> action taken when the argument is omitted.  If an argument is passed
!!!  --> as a pointer, it should be so identified below.  If an array is passed
!!!  --> as an assumed shape array, its indices should be indicated with
!!!  --> colons (:) below.
!!!
!!! character(len=*)           aaaa(*) =    --> description 
!!! character(len=8),pointer   bbbb(:) =    --> description 
!!! double precision           cvar    =    --> description
!!! character(len=*)           msg     =    --> description 
!!! integer                    hello   =    --> description
!!! integer         ,optional  indx    =    --> description
!!! double precision,optional  value   =    --> description
!!!
!!!  --> Add any additional notes about the use of the routines, or about
!!!  --> their arguments, here.
!!!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!!!  --> This is an OPTIONAL section which can be omitted if not needed.
!!!
!!!  --> Enter here more lengthy or more detailed advice than is appropriate
!!!  --> for the above documentation.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  6. 2007-11-29  Stoeckley  Remove unused reference to memman.
!  5. 2007-02-05  D. Glover  Added NULLIFY statements for Intel compiler
!004. 2006-06-20  B. Menger  Removed Unused Variables.
!  3. 2005-01-03  B. Lucas   Added check so that velocity warnings will not
!                            br printed in SILENT mode.
!  2. 2004-02-26  B. Lucas   Fixed ASUM line change re-initialization bug.
!                            Added error codes to UHCI_READ/WRITE calls.
!  1. 2003-08-26  B. Lucas   Initial version.
!
!!!  --> This is a REQUIRED section which CANNOT be omitted.
!!!
!!!  --> Copy the old revision history to this location if it exists,
!!!  --> and renumber as necessary.
!!!
!!!  --> The revision history should be listed in descending order, with
!!!  --> the most recent revision at the top.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
! This process requires 8-byte (64-bit) word sizes.
! This process packs and unpacks values in 8-byte words.
! This process contains the following platform-dependent code...
!
!!!  --> This is a REQUIRED section which CANNOT be omitted, even if empty.
!!!
!!!  --> Omit or modify the inappropriate lines above.
!!!  --> Also you can add additional portability details here.
!!!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
!!!  --> This is an OPTIONAL section which can be omitted if not needed.
!!!
!!!  --> Description of any special compiler or linking requirements
!!!  --> or restrictions.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!!!  --> This is an OPTIONAL section which can be omitted if not needed.
!!!
!!!  --> Description of this particular algorithm, related theory and
!!!  --> relevant references.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
!!!  --> This is an OPTIONAL section which can be omitted if not needed.
!!!
!!!  --> Enter here more lengthy or more detailed information which may be
!!!  --> required.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<HelpSection>
!!!
!!!  --> This is an OPTIONAL section which can be omitted if not needed.
!!!
!!!  --> Context-sensitive help for the GUI and for the DOC program
!!!  --> goes in this section.  Two different formats are supported,
!!!  --> either of which can be used.  Both formats are shown for
!!!  --> keywords KEYWORD1 and KEYWORD2.  Every parameter in the gui_def
!!!  --> section must have an entry in this section.  Each parameter must
!!!  --> have a <tip>, a Default line, and one or more Allowed lines,
!!!  --> followed optionally (but usually) by additional information.
!!!
!!!  --> NOTE: For display-only parameters (i.e. informational fields
!!!  --> designated with XXXXXXX in the gui_def section), the keyword line
!!!  --> below should look like this:
!!!  -->    <Help KEYWORD="keyword1" TYPE="DISPLAY_ONLY">
!!!  --> and the Default and Allowed lines are optional.
!!!
!!!  --> NOTE:
!!!  --> This section is needed only for a primitive which encapsulates
!!!  --> parameters used by several processes and provides a SCREEN LAYOUT
!!!  --> and CONTEXT SENSITIVE HELP to those processes.  A process which
!!!  --> uses this primitive can then include a line like this at the
!!!  --> appropriate location within its own gui_def section where you
!!!  --> want the screen layout from this primitive to reside:
!!!  -->                    <include buhci.f90>
!!!  --> This context sensitive help will then also become available to
!!!  --> that process.
!!!
!!! --> Here are some examples:
!!! --> (Only one exclamation point should start each line)
!!!
!!!<Help KEYWORD="keyword1">
!!!<Tip> Here is the one-line context-sensitive help for this parameter. </Tip>
!!! Default = dddddddd   ! this can be a default value or a short phrase.
!!! Allowed = pppppppp   ! this can be a range of values or a short phrase.
!!!
!!! Continuing here is more multi-line context-sensitive help, which can take
!!! as many lines as necessary, with possible blank lines, and will continue
!!! until the end of this section is reached.
!!!</Help>
!!!
!!!
!!!<Help KEYWORD="keyword2">
!!!<Tip> Here is the one-line context-sensitive help for this parameter. </Tip>
!!! Default = dddddddd   ! this is the default value from a list of options.
!!! Allowed = ppppppp1   Short description for option 1.
!!! Allowed = ppppppp2   Short description for option 2.
!!! Allowed = ppppppp3   Short description for option 3.
!!!
!!! Continuing here is more multi-line context-sensitive help, which can take
!!! as many lines as necessary, with possible blank lines, and will continue
!!! until the end of this section is reached.
!!! is reached.
!!!</Help>
!!!
!!!
!!!<help keyword1> Here is the one-line context-sensitive help for this param.
!!! Default = dddddddd   ! this can be a default value or a short phrase.
!!! Allowed = pppppppp   ! this can be a range of values or a short phrase.
!!!
!!! Continuing here is more multi-line context-sensitive help, which can take
!!! as many lines as necessary, with possible blank lines, and will continue
!!! until the end of this section is reached.
!!!
!!!
!!!<help keyword2> Here is the one-line context-sensitive help for this param.
!!! Default = dddddddd   ! this is the default value from a list of options.
!!! Allowed = ppppppp1   Short description for option 1.
!!! Allowed = ppppppp2   Short description for option 2.
!!! Allowed = ppppppp3   Short description for option 3.
!!!
!!! Continuing here is more multi-line context-sensitive help, which can take
!!! as many lines as necessary, with possible blank lines, and will continue
!!! until the end of this section is reached.
!!! is reached.
!!!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module buhci_module
      use ppavo_module
      use pc_module
      use named_constants_module
      use grid_module
      use pathchoose_module
      use pathcheck_module
      implicit none

      private
      public :: buhci_create
      public :: buhci_delete
      public :: buhci_clear
      public :: buhci_init
      public :: buhci_work
      public :: buhci_init_parms
      public :: buhci_get_parm
      public :: buhci_set_parm
      public :: buhci_get_cparm
      public :: buhci_get_iparm
      public :: buhci_get_rparm
      public :: buhci_set_cparm
      public :: buhci_set_iparm
      public :: buhci_set_rparm

      character(len=100),public,save :: BUHCI_IDENT = &
'$Id: buhci.f90,v 1.6 2007/11/30 13:55:16 Stoeckley beta sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

      type,public :: buhci_dptr
         double precision, pointer, dimension(:) :: ptr
      end type buhci_dptr
      type,public :: buhci_rptr
         real, pointer, dimension(:) :: ptr
      end type buhci_rptr

      type,public :: buhci_struct              
!      private

        integer            :: ipn       ! process number.
        integer            :: numtr     ! max number of input traces.
        logical            :: gathered  ! whether properly gathered.
        integer            :: nwih      ! number of header words.
        integer            :: ndpt      ! number of trace samples.
        real               :: tstrt     ! time of 1st trace sample (sec).
        real               :: dt        ! trace sample interval (sec).
        type(grid_struct)  :: grid      ! grid transform.

        integer            :: print_lun

        type(ppavo_struct), pointer :: ppavo_obj

        integer            :: maxcode
        integer            :: mxocode
        integer            :: lcmxfd
        integer            :: oticd(ppavo_b_mxocode)
        integer            :: ntout
        logical            :: avg_flag
        integer            :: total_saved
        integer            :: len_sav
        integer            :: itooltype

        real               :: radian
        real               :: ln2
        integer            :: hsize
        integer            :: mintid
        integer            :: maxtid
        integer            :: stawdt

        integer            :: actcdp
        integer            :: asums
        integer            :: absumrs
        integer            :: absumis
        integer            :: bsums
        integer            :: bulk1
        integer            :: ddp2
        integer            :: fs1
        integer            :: fsa
        integer            :: fss
        integer            :: isi
        integer            :: jcdpa
        integer            :: jcdpb
        integer            :: kpmitf
        integer            :: kpmotf
        integer            :: kpwarn
        integer            :: kpwrks
        integer            :: kpwrkd(2)
        integer            :: kpwks2
        integer            :: kpwkd2(2)
        integer            :: lasticd
        integer            :: ldd1
        integer            :: ldd2
        integer            :: lineno
        integer            :: lensb
        integer            :: lsa
        integer            :: lss
        integer            :: mxfld
        integer            :: nc1
        integer            :: nc2
        integer            :: nccor
        integer            :: nowds
        integer            :: nproc
        integer            :: ns
        integer            :: nsa
        integer            :: nss
        integer            :: nsmth
        integer            :: nta
        integer            :: outtr
        integer            :: pas2
        integer            :: passed
        integer            :: pcdp
        integer            :: sizeu
        integer            :: statss
        integer            :: thl
        integer            :: wfloc
        integer            :: wkfln1
        integer            :: wkfln2
        integer            :: wkrecl
        integer            :: wtss

        logical            :: ovfflg
        logical            :: hflag
        logical            :: calia
        logical            :: calib
        logical            :: cala
        logical            :: calb
        logical            :: linend

        real               :: fsuhci
        real               :: reft
        real               :: wfreq  
        real               :: limits(ppavo_b_mxocode)
        real               :: scalef(ppavo_b_mxocode)

        character(len=27)  :: when

        integer            :: i
        integer            :: ipr
        integer            :: ticd
        integer            :: icdpn
        integer            :: nst_dump
        integer            :: err
        integer            :: acdpn
        integer            :: itrn
        integer            :: kprtf
        integer            :: filep1
        integer            :: trailtr
        integer            :: lstu
        integer            :: cline
        integer            :: intcde
        integer            :: maxbin
        integer            :: icode
        integer            :: dhd
        integer            :: dtr
        integer            :: fstu
        integer            :: nstu
        integer            :: ntwin
        integer            :: lwhen
        integer            :: flv
        integer            :: llv
        integer            :: l2
        integer            :: ndx1
        integer            :: ndx2
        integer            :: fndx
        integer            :: dunit
        integer            :: fst_dump
        integer            :: lst_dump
        integer            :: filep2
        integer            :: next
        real, pointer, dimension(:) :: sptr
        integer            :: maxvel
        integer            :: numvel
        integer            :: fold

        type(buhci_dptr)  :: h(ppavo_b_mxocode)
        type(buhci_rptr)  :: t(ppavo_b_mxocode)
        integer            :: mem_alloc
        logical            :: live
        logical            :: memt
        logical            :: slope
        logical            :: zoff
        logical            :: cpyflg
        real               :: amxtt
        real               :: factor


        integer            :: kpdrtf
        integer            :: kpfcf
        integer            :: kprno
        integer            :: bulk


        integer            :: iter_cnt
        integer            :: iter_num
        integer            :: any_more
        integer            :: dbug_opt
        integer            :: scdp_num
        integer            :: ecdp_num
        integer            :: ncdp_max
        integer            :: stim_win
        integer            :: etim_win
        integer            :: cwin_tim
        integer            :: cwin_cdp
        integer            :: skip_opt
        integer            :: mplx_opt
        integer            :: corr_opt
        integer            :: cwin_vsh
        integer            :: scal_opt
        integer            :: obdn_opt
        integer            :: smth_vel
        logical            :: swin_opt
        integer            :: scdp_scl
        integer            :: ecdp_scl
        integer            :: stim_scl
        integer            :: etim_scl
        logical            :: con_velp
        integer            :: ivel_min
        integer            :: ivel_max
        integer            :: svel_min
        integer            :: svel_max
        integer            :: vcut_tim
        integer            :: vcut_pow
        logical            :: dump_opt
        integer            :: dtim_dmp
        integer            :: stim_dmp
        integer            :: etim_dmp
        integer            :: scdp_dmp
        integer            :: ecdp_dmp
        integer            :: dcdp_dmp
        logical            :: same_dbg
        integer            :: hflt_len

!       B_UHCI real parameters.
        real               :: scal_fct
        real               :: scal_rms
        real               :: angl_rot
        real               :: ccoef_re
        real               :: ccoef_im
        real               :: atrm_pow
        real               :: btrm_pow
        real               :: q_factor
        real               :: ctr_freq
        real               :: amix_fct
        real               :: bmix_fct
        real               :: alph_fct
        real               :: ffth_dmn
        real               :: ivel_dmn
        real               :: ivel_dmx
        real               :: svel_dmn
        real               :: svel_dmx
        real               :: real_dbg
        real               :: imag_dbg

!       B_UHCI character parameters.
        integer :: mode_opt
        character(len=256) :: otrc_opt
        character(len=256) :: veldname

!       B_UHCI parameter flags.
        logical            :: f_iter_cnt
        logical            :: f_iter_num
        logical            :: f_any_more
        logical            :: f_dbug_opt
        logical            :: f_scdp_num
        logical            :: f_ecdp_num
        logical            :: f_ncdp_max
        logical            :: f_stim_win
        logical            :: f_etim_win
        logical            :: f_cwin_tim
        logical            :: f_cwin_cdp
        logical            :: f_skip_opt
        logical            :: f_mplx_opt
        logical            :: f_corr_opt
        logical            :: f_cwin_vsh
        logical            :: f_scal_opt
        logical            :: f_obdn_opt
        logical            :: f_smth_vel
        logical            :: f_swin_opt
        logical            :: f_scdp_scl
        logical            :: f_ecdp_scl
        logical            :: f_stim_scl
        logical            :: f_etim_scl
        logical            :: f_con_velp
        logical            :: f_ivel_min
        logical            :: f_ivel_max
        logical            :: f_svel_min
        logical            :: f_svel_max
        logical            :: f_vcut_tim
        logical            :: f_vcut_pow
        logical            :: f_dump_opt
        logical            :: f_dtim_dmp
        logical            :: f_stim_dmp
        logical            :: f_etim_dmp
        logical            :: f_scdp_dmp
        logical            :: f_ecdp_dmp
        logical            :: f_dcdp_dmp
        logical            :: f_same_dbg
        logical            :: f_hflt_len
        logical            :: f_scal_fct
        logical            :: f_scal_rms
        logical            :: f_angl_rot
        logical            :: f_ccoef_re
        logical            :: f_ccoef_im
        logical            :: f_atrm_pow
        logical            :: f_btrm_pow
        logical            :: f_q_factor
        logical            :: f_ctr_freq
        logical            :: f_amix_fct
        logical            :: f_bmix_fct
        logical            :: f_alph_fct
        logical            :: f_ffth_dmn
        logical            :: f_ivel_dmn
        logical            :: f_ivel_dmx
        logical            :: f_svel_dmn
        logical            :: f_svel_dmx
        logical            :: f_real_dbg
        logical            :: f_imag_dbg
        logical            :: f_mode_opt
        logical            :: f_otrc_opt
        logical            :: f_veldname

!       B_UHCI integer parameters.
        integer            :: p_iter_cnt
        integer            :: p_iter_num
        integer            :: p_any_more
        integer            :: p_dbug_opt
        integer            :: p_scdp_num
        integer            :: p_ecdp_num
        integer            :: p_ncdp_max
        integer            :: p_stim_win
        integer            :: p_etim_win
        integer            :: p_cwin_tim
        integer            :: p_cwin_cdp
        integer            :: p_skip_opt
        integer            :: p_mplx_opt
        integer            :: p_corr_opt
        integer            :: p_cwin_vsh
        integer            :: p_scal_opt
        integer            :: p_obdn_opt
        integer            :: p_smth_vel
        integer            :: p_swin_opt
        integer            :: p_scdp_scl
        integer            :: p_ecdp_scl
        integer            :: p_stim_scl
        integer            :: p_etim_scl
        integer            :: p_con_velp
        integer            :: p_ivel_min
        integer            :: p_ivel_max
        integer            :: p_svel_min
        integer            :: p_svel_max
        integer            :: p_vcut_tim
        integer            :: p_vcut_pow
        integer            :: p_dump_opt
        integer            :: p_dtim_dmp
        integer            :: p_stim_dmp
        integer            :: p_etim_dmp
        integer            :: p_scdp_dmp
        integer            :: p_ecdp_dmp
        integer            :: p_dcdp_dmp
        integer            :: p_same_dbg
        integer            :: p_hflt_len

!       B_UHCI real parameters.
        real               :: p_scal_fct
        real               :: p_scal_rms
        real               :: p_angl_rot
        real               :: p_ccoef_re
        real               :: p_ccoef_im
        real               :: p_atrm_pow
        real               :: p_btrm_pow
        real               :: p_q_factor
        real               :: p_ctr_freq
        real               :: p_amix_fct
        real               :: p_bmix_fct
        real               :: p_alph_fct
        real               :: p_ffth_dmn
        real               :: p_ivel_dmn
        real               :: p_ivel_dmx
        real               :: p_svel_dmn
        real               :: p_svel_dmx
        real               :: p_real_dbg
        real               :: p_imag_dbg

!       B_UHCI character parameters.
        character(len=256) :: p_mode_opt
        character(len=256) :: p_otrc_opt
        character(len=256) :: p_veldname

!       ProMAX temporary storage arrays.
        integer            :: itempz(50000) ! temporary buffer - integer
        real               :: rtempz(50000) ! temporary buffer - real
        character          :: ctempz(50000) ! temporary buffer - character
        integer            :: ispacez(2) ! temporary space - integer
        real               :: rspacez(2) ! temporary space - real
        double precision   :: dspacez(2) ! temporary space - double precision

!       ProMAX run-time parameters
        real               :: sampratz  ! sample rate (msec)
        integer            :: numsmpz   ! number of trace samples
        integer            :: ipsortz   ! physical primary sort flag
        integer            :: maxdtrz   ! maximum number of traces per ensemble
        integer            :: idtypez   ! primary data type
        integer            :: nthz      ! number of trace headers (=64)
        integer            :: iounitz   ! i/o unit for output diagnostics
        integer            :: ipkeyz    ! trace header index of primary sort key
        integer            :: iskeyz    ! trace header index of secndry sort key
        integer            :: idomainz  ! the domain
        logical            :: cleanupz  ! system clean-up flag
        logical            :: ierrorz   ! system error flag.

!       ProMAX miscellaneous parameters.
        integer            :: iunitsz   ! type of units (ienglishpz, imetricpz)
        integer            :: i3dz      ! 3d flag (1=3d, 0=other)
        integer            :: imultcz   ! multi-component flag (1=multi-comp.)
        integer            :: ntracez   ! maximum sequential trace number
        integer            :: icdpasnz  ! CDP bin assigned flag  (1=assigned)
        integer            :: igeoasnz  ! geometry assigned flag (1=assigned)
        integer            :: numcdpz   ! number of CDPs
        integer            :: idatez    ! integer date

        integer            :: jtkill
        integer            :: jtlive
        integer            :: jtvel
        integer            :: jta
        integer            :: jtb
        integer            :: jtaq
        integer            :: jtbq
        integer            :: jtsa
        integer            :: jtsb
        integer            :: jtrc
        integer            :: jtic
        integer            :: jthci
        integer            :: jtrvi
        integer            :: jtano
        integer            :: jtbno
        integer            :: jtdv
        integer            :: jtitt
        integer            :: ktkill
        integer            :: ktvel
        integer            :: kta
        integer            :: ktb
        integer            :: ktaq
        integer            :: ktbq
        integer            :: ktsa
        integer            :: ktsb
        integer            :: ktrc
        integer            :: ktic
        integer            :: kthci
        integer            :: ktrvi
        integer            :: ktano
        integer            :: ktbno
        integer            :: ktdv
        integer            :: ktitt

        logical :: initialized

        real, pointer, dimension(:) :: ddp1tr
        double precision, pointer, dimension(:) :: ddp1hd

        integer, pointer, dimension(:) :: icdps
        real, pointer, dimension(:) :: fcdps
        real, pointer, dimension(:) :: folds
        double precision, pointer, dimension(:,:) :: stats

        double precision, pointer, dimension(:) :: asum
        double precision, pointer, dimension(:) :: bsum
        double precision, pointer, dimension(:) :: absumr
        double precision, pointer, dimension(:) :: absumi
        double precision, pointer, dimension(:) :: wts
        real, pointer, dimension(:) :: calibr
        real, pointer, dimension(:) :: calibi
        real, pointer, dimension(:) :: caliba
        real, pointer, dimension(:) :: calibb

        double precision, pointer, dimension(:) :: newhd
        real, pointer, dimension(:) :: newtr

        real, pointer, dimension(:) :: rcax
        real, pointer, dimension(:) :: rcbx
        real, pointer, dimension(:) :: wfun
        real, pointer, dimension(:) :: dvv
        real, pointer, dimension(:) :: kpbegtr
        double precision, pointer, dimension(:) :: kpbeghd

      end type buhci_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      interface buhci_get_parm
        module procedure buhci_get_iparm
        module procedure buhci_get_rparm
        module procedure buhci_get_cparm
      end interface

      interface buhci_set_parm
        module procedure buhci_set_iparm
        module procedure buhci_set_rparm
        module procedure buhci_set_cparm
      end interface


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!

      subroutine buhci_create (obj)
      type(buhci_struct),pointer :: obj       ! arguments

      allocate (obj)    ! omit if OBJ is not to be passed as a pointer.

      allocate (obj%ppavo_obj)
      obj%ppavo_obj%init = 0

      nullify (obj%sptr) ! jpa
      nullify (obj%rcax) ! jpa
      nullify (obj%rcbx) ! jpa
      nullify (obj%wfun) ! jpa
      nullify (obj%dvv) ! jpa
      nullify (obj%kpbegtr) ! jpa
      nullify (obj%kpbeghd) ! jpa
      nullify(obj%icdps)
      nullify(obj%fcdps)
      nullify(obj%folds)
      nullify(obj%stats)
      nullify(obj%asum)
      nullify(obj%bsum)
      nullify(obj%absumr)
      nullify(obj%absumi)
      nullify(obj%wts)
      nullify(obj%calibr)
      nullify(obj%calibi)
      nullify(obj%caliba)
      nullify(obj%calibb)
      nullify(obj%newhd)
      nullify(obj%newtr)
      nullify(obj%ddp1tr)
      nullify(obj%ddp1hd)

      obj%initialized = .false.
      call buhci_clear (obj)   ! initialize the data structure variables.
      end subroutine buhci_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!

      subroutine buhci_delete (obj)
      type(buhci_struct),pointer :: obj       ! arguments

      call buhci_clear (obj)

      if (associated(obj%ppavo_obj)) deallocate (obj%ppavo_obj)

      deallocate(obj)    ! omit if OBJ is not to be passed as a pointer.
      end subroutine buhci_delete


!!----------------------------- clear -------------------------------------!!
!!----------------------------- clear -------------------------------------!!
!!----------------------------- clear -------------------------------------!!

      subroutine buhci_clear (obj)
      type(buhci_struct) :: obj       ! arguments

      end subroutine buhci_clear


!!---------------------------- buhci_init -----------------------------------!!
!!---------------------------- buhci_init -----------------------------------!!
!!---------------------------- buhci_init -----------------------------------!!

      subroutine buhci_init (obj, ntr)
      type(buhci_struct),intent(inout) :: obj       ! arguments
      integer,            intent(inout) :: ntr

      integer            :: nnames
      parameter (nnames = 2)

      integer            :: ipr
      integer            :: ipos
      integer            :: idlist(ppavo_b_maxcode)

      integer            :: lcrl
      integer            :: thisno
      integer            :: length



      integer            :: ierr
      integer            :: i   
      integer            :: erin
      integer            :: seqda
      integer            :: idummy
      integer            :: ioerr

      save thisno

      real               :: fs
      real               :: tsamp




      character(len=1)   :: code
      character(len=4)   :: extension

      character(len=100) :: mesg

      character(len=14)  :: codelist
      character(len=14)  :: field
      character(len=14)  :: cumode

      character(len=41)  :: mesgw(ppavo_b_maxcode)
      character(len=40)  :: mpxtxt(0:2)
      character(len=40)  :: cortxt(0:2)
      character(len=40)  :: mpxfld
      character(len=40)  :: corfld
      character(len=40)  :: sclfld
      character(len=40)  :: skpfld
      character(len=40)  :: scltxt(0:2)
      character(len=40)  :: skptxt(0:1)
      character(len=9)   :: ddnam1
      character(len=9)   :: ddnam2
      integer            :: temp

      data thisno    / 0 /
      data codelist  / 'HVABCDSTRIENOX'/
      data idlist    / 42, 46, 43, 44, 17, 18, 45, 47, 48, 41,&
     &                 51, 49, 50, 13 /
      data mpxtxt     / 'MULTIPLEXED', 'DE-MULTIPLEXED',&
     &                  'PASS-THRU FOR AVEL VELOCITY PICKING' /
      data cortxt    / 'NO-WEIGHTING', 'INVERSE WEIGHTING',&
     &                 'INVERSE SQUARE WEIGHT' /
      data scltxt    / 'MAGNITUDE SQUARED OF ''A''',&
     &                 'MAGNITUDE OF ''A''',&
     &                 'NO SCALING' /
      data skptxt    / 'NORMAL OPERATION',&
     &                 'ONLY SELECT TRACES OF THE GIVEN TYPE' /
      data mesgw     / 'OPTIMAL HYDROCARBON INDICATOR',&
     &                 'OPTIMAL RESIDUAL VELOCITY INDICATOR',&
     &                 'IN-PHASE ZERO-OFFSET STACK',&
     &                 'IN-PHASE AVO SLOPE',&
     &                 'QUADRATURE ZERO-OFFSET STACK',&
     &                 'QUADRATURE AVO SLOPE',&
     &                 'AVERAGED RMS ZERO-OFFSET ENVELOPE',&
     &                 'AVERAGED RMS AVO SLOPE ENVELOPE',&
     &                 'CORRELATION COEFFICIENT, REAL PART',&
     &                 'CORRELATION COEFFICIENT, IMAGINARY PART',&
     &                 'REVISED STACKING VELOCITY',&
     &                 'OVERBURDEN-CORRECTED ZERO-OFFSET RESPONSE',&
     &                 'OVERBURDEN-CORRECTED AVO SLOPE',&
     &                 'INTERVAL VELOCITIES' /

      obj%maxcode = ppavo_b_maxcode
      obj%mxocode = ppavo_b_mxocode        
      obj%jtkill  = ppavo_b_jtkill
      obj%jtlive  = ppavo_b_jtlive
      obj%jtvel   = ppavo_b_jtvel
      obj%jta     = ppavo_b_jta
      obj%jtb     = ppavo_b_jtb
      obj%jtaq    = ppavo_b_jtaq
      obj%jtbq    = ppavo_b_jtbq
      obj%jtsa    = ppavo_b_jtsa
      obj%jtsb    = ppavo_b_jtsb
      obj%jtrc    = ppavo_b_jtrc
      obj%jtic    = ppavo_b_jtic
      obj%jthci   = ppavo_b_jthci
      obj%jtrvi   = ppavo_b_jtrvi
      obj%jtano   = ppavo_b_jtano
      obj%jtbno   = ppavo_b_jtbno
      obj%jtdv    = ppavo_b_jtdv
      obj%jtitt   = ppavo_b_jtitt
      obj%ktkill  = ppavo_b_ktkill
      obj%ktvel   = ppavo_b_ktvel
      obj%kta     = ppavo_b_kta
      obj%ktb     = ppavo_b_ktb
      obj%ktaq    = ppavo_b_ktaq
      obj%ktbq    = ppavo_b_ktbq
      obj%ktsa    = ppavo_b_ktsa
      obj%ktsb    = ppavo_b_ktsb
      obj%ktrc    = ppavo_b_ktrc
      obj%ktic    = ppavo_b_ktic
      obj%kthci   = ppavo_b_kthci
      obj%ktrvi   = ppavo_b_ktrvi
      obj%ktano   = ppavo_b_ktano
      obj%ktbno   = ppavo_b_ktbno
      obj%ktdv    = ppavo_b_ktdv
      obj%ktitt   = ppavo_b_ktitt
      obj%radian  = 57.2958
      obj%ln2     = 0.693147
      obj%hsize   = 1000
      obj%mintid  = 11
      obj%maxtid  = 51
      obj%stawdt  = 6

      obj%print_lun = pc_get_lun()

      obj%cleanupz = .false.

      ntr = 0  

!     Initialization and card 1.
      ipr    = obj%iounitz
      fs     = 1e3 / obj%sampratz
      tsamp  = 1.0 / fs
      thisno = thisno + 1
      obj%kprno  = thisno
      obj%kpwarn = 0
      obj%lcmxfd = obj%maxdtrz

!     Set special frequency value fo UHCI only.
      obj%fsuhci = fs

!     Verify the sort option, stacking mode.      
      if (obj%igeoasnz .eq. 1) then
            if (obj%ipsortz .ne. ppavo_cdppz) then
            write(obj%print_lun, *) 'B_UHCI: Sorting &
     &option is not cdp.'
         end if
         if (obj%idtypez .ne. ppavo_stackedpz) then
            write(obj%print_lun, *) 'B_UHCI: Data &
     &is not moved out.'
         end if
      end if

!     Get the debugging level.
      obj%dbug_opt = 0
      call buhci_get_iparm(obj, 'dbug_opt', obj%dbug_opt)

!     Get the iteration number.
      obj%iter_num = 1
      obj%iter_cnt = 1
      call buhci_get_iparm(obj, 'iter_num', obj%iter_num)
      call buhci_get_iparm(obj, 'iter_cnt', obj%iter_cnt)

!     Get starting and ending CDPs.
      obj%scdp_num = 0
      call buhci_get_iparm(obj, 'scdp_num', obj%scdp_num)
      obj%ecdp_num = 0
      call buhci_get_iparm(obj, 'ecdp_num', obj%ecdp_num)
      if (obj%ecdp_num .le. 0) obj%ecdp_num = 99999
      if (obj%ecdp_num .le. obj%scdp_num) obj%ecdp_num = obj%scdp_num

!     Get maximum number of CDPs on the line.
      obj%ncdp_max = 0
      call buhci_get_iparm(obj, 'ncdp_max', obj%ncdp_max)
      obj%numcdpz = obj%ncdp_max
      obj%ncdp_max = max0(obj%ncdp_max, 0)
      if (obj%ncdp_max .le. 0) then
         if (obj%igeoasnz .eq. 1) obj%ncdp_max = obj%numcdpz
         if (obj%ncdp_max .le. 0) then
            write(obj%print_lun, *) 'B_UHCI: Max number of CDPs &
     &is not positive.'
            ntr = FATAL_ERROR
            return
         end if
      end if

!     Get mode of operation.
      obj%mode_opt = 1
      cumode = 'AVO'
      call buhci_get_cparm(obj, 'mode_opt', cumode, length)
      code = cumode(1:1)
      if(code .eq. 'V' .or. code .eq. 'v') obj%mode_opt = 2
      if(code .eq. 'S' .or. code .eq. 's') obj%mode_opt = 3
      if(code .eq. 'G' .or. code .eq. 'g') obj%mode_opt = 4

!     Get start and end time for analysis.
      obj%stim_win = 0
      lcrl = (obj%numsmpz-1) * obj%sampratz
      obj%etim_win = lcrl
      call buhci_get_iparm(obj, 'stim_win', obj%stim_win)
      call buhci_get_iparm(obj, 'etim_win', obj%etim_win)
      if(obj%etim_win .eq. 0) obj%etim_win = lcrl
      if(obj%etim_win .le. obj%stim_win) obj%etim_win = lcrl
      obj%etim_win = min0(obj%etim_win, lcrl)

      obj%fsa = obj%stim_win / obj%sampratz + 1
      obj%lsa = obj%etim_win / obj%sampratz + 1
      obj%lsa = min0(obj%lsa, obj%numsmpz)
      obj%nsa = obj%lsa - obj%fsa + 1

!     Get Hilbert transform filter length.
      obj%hflt_len = 43
      call buhci_get_iparm(obj, 'hflt_len', obj%hflt_len) 

!     Get time and spatial dimensions for averaging.
      obj%cwin_tim = 300
      call buhci_get_iparm(obj, 'cwin_tim', obj%cwin_tim)
      if (obj%cwin_tim .le. 0) then
         write(obj%print_lun, *) 'B_UHCI: Time correlation window &
     &is invalid.'
         ntr = FATAL_ERROR
         return
      end if
      obj%cwin_tim = min0(obj%cwin_tim, lcrl)

      obj%cwin_cdp = 30
      call buhci_get_iparm(obj, 'cwin_cdp', obj%cwin_cdp)
      if (obj%cwin_cdp .le. 0) then
         write (obj%print_lun, *)'B_UHCI: CDP correlation window &
     &is invalid.'
         ntr = FATAL_ERROR
         return
      end if

      if(obj%cwin_cdp .gt. obj%ncdp_max) then
        write (mesg, 2434) obj%cwin_cdp, obj%ncdp_max
2434    format('B_UHCI: CDP window size (',i4,') greater than &
     &maximum CDPS in line (',i4,')')
        write(obj%print_lun, *) mesg
        obj%cwin_cdp = obj%ncdp_max
      end if

!     Get user-defined scale factor for histograms.
      obj%scal_fct = -1.0
      call buhci_get_rparm(obj, 'scal_fct', obj%scal_fct)
      if (obj%scal_fct .eq. 0.0) obj%scal_fct = -1.0

!     Get user-defined scale factor for RMS scaling.
      obj%scal_rms = obj%scal_fct
      call buhci_get_rparm(obj, 'scal_rms', obj%scal_rms)
      if (obj%scal_rms .eq. 0.0) obj%scal_rms = -1.0

!     Get the skip option.
      obj%skip_opt = 0
      call buhci_get_iparm(obj, 'skip_opt', obj%skip_opt)
      skpfld = skptxt(obj%skip_opt)

!     Get list of attributes to output.      
      field = ' '
      call buhci_get_cparm(obj, 'otrc_opt', field, length)
      call ppavo_uppercase(field(1:), length)
      if (field .eq. ' ') field = 'AH'
      do 155 i = 1, obj%mxocode
         code = field(i:i)
         if (code .ne. ' ') then
            if (code .eq. char(0)) goto 175
            ipos = index(codelist, code)
            if (ipos .eq. 0) then
               write (mesg, 9115) code, codelist
9115           format ('B_UHCI: Unknown output code: ', a1,&
     &                 '; must be one of: (', a,')@')
               write(obj%print_lun, *) mesg
               ntr = FATAL_ERROR
               return
            else
               obj%ntout = obj%ntout + 1
               obj%oticd(obj%ntout) = idlist(ipos)
            end if
         end if
155   continue

!     Assign defaults, in case 'opt' card is missing.
175   obj%mplx_opt = 1
      obj%corr_opt = 0
      obj%scal_opt = 1
      obj%alph_fct = 0.0
      obj%ffth_dmn = 10.0
      obj%atrm_pow = 0.0
      obj%btrm_pow = 0.0
      obj%avg_flag = .true.

      obj%ccoef_re = 10.0 
      obj%ccoef_im = 0.0
      obj%amix_fct = 0.0
      obj%bmix_fct = 1.0
      obj%q_factor = 50.0
      obj%angl_rot = 0.0
      obj%obdn_opt = 0
      obj%ctr_freq = 0.0
      obj%smth_vel = 0
      
!     Get trace output order.
      call buhci_get_iparm(obj, 'mplx_opt', obj%mplx_opt)
         mpxfld = mpxtxt(obj%mplx_opt)
         if (obj%mplx_opt .eq. 2 .and. field .eq. ' ') then
         obj%ntout = 1
         obj%oticd(obj%ntout) = 51
         field = 'E'
      end if
      if (obj%mplx_opt.eq. 0 .or. obj%mplx_opt .ge. 2) then
         obj%ipsortz = ppavo_cdppz
      else
         obj%ipsortz = ppavo_unknownpz
      end if

!     Get correlation weighting option.
      call buhci_get_iparm(obj, 'corr_opt', obj%corr_opt)
      corfld = cortxt(obj%corr_opt)

!     Get vertical shift of correlation window.
      obj%cwin_vsh = 0
      call buhci_get_iparm(obj, 'cwin_vsh', obj%cwin_vsh)
      obj%cwin_vsh = obj%cwin_vsh / obj%sampratz

!     Get scaling option.
      call buhci_get_iparm(obj, 'scal_opt', obj%scal_opt)
      sclfld = scltxt(obj%scal_opt)

!     Get correlation from well control.
      call buhci_get_iparm(obj, 'obdn_opt', obj%obdn_opt)
      call buhci_get_rparm(obj, 'angl_rot', obj%angl_rot)
      call buhci_get_rparm(obj, 'ccoef_re', obj%ccoef_re)
      call buhci_get_rparm(obj, 'ccoef_im', obj%ccoef_im)
      if (obj%obdn_opt .eq. 2 .or. obj%obdn_opt .eq. 3) obj%ccoef_re = 0.0
      if (obj%obdn_opt .ne. 0 .and. obj%obdn_opt .ne. 4) then
         if ((obj%ccoef_re**2 + obj%ccoef_im**2) .ge. 1.0) then
            write (mesg, 156)
156         format ('B_UHCI: Magnitude of desired correlation &
     &greater than unity.')
            write(obj%print_lun, *) mesg
            ntr = FATAL_ERROR
            return
         end if
      else
         obj%ccoef_re = 10.0
         obj%ccoef_im = 0.0
      end if

!     Get power overrides on A and B.      
      call buhci_get_rparm(obj, 'atrm_pow', obj%atrm_pow)
      call buhci_get_rparm(obj, 'btrm_pow', obj%btrm_pow)
      if (obj%atrm_pow .lt. 0.0) obj%atrm_pow = 0.0
      if (obj%btrm_pow .lt. 0.0) obj%btrm_pow = 0.0

!     Get Q quality factor (used for automatic velocity correction).
      call buhci_get_rparm(obj, 'q_factor', obj%q_factor)
      if(obj%q_factor .le. 0.0) obj%q_factor = 1000.0

!     Get wavelet center frequency override.
      call buhci_get_rparm(obj, 'ctr_freq', obj%ctr_freq)
      if (obj%ctr_freq .le. 0.0) obj%ctr_freq = 0.0

!     Get smooth velocity function.
      obj%any_more = 0
      call buhci_get_iparm(obj, 'any_more', obj%any_more)
      call buhci_get_iparm(obj, 'smth_vel', obj%smth_vel)
      if(obj%smth_vel .lt. 0) obj%smth_vel = 0
      if(obj%iter_num .eq. obj%iter_cnt) then
         if(obj%any_more .ne. 0) then
            obj%smth_vel = 0
         end if
      else
         obj%smth_vel = 0
      end if

!     Get mixing factors for A and B.
      call buhci_get_rparm(obj, 'amix_fct', obj%amix_fct)
      call buhci_get_rparm(obj, 'bmix_fct', obj%bmix_fct)

!     Get aggressiveness factor.
      call buhci_get_rparm(obj, 'alph_fct', obj%alph_fct)

!     Get minimum delta fluid angle.
      call buhci_get_rparm(obj, 'ffth_dmn', obj%ffth_dmn)

370   continue
      
      if (obj%dbug_opt .ge. 0) then

!        Print seisparm parameters from card 1.
         write (ipr, 8100) obj%scdp_num, obj%ecdp_num, obj%ncdp_max,&
     &                     obj%stim_win, obj%etim_win, obj%hflt_len,&
     &                     obj%cwin_tim, obj%cwin_cdp, obj%ntout,&
     &                     obj%scal_fct, obj%scal_rms*1e3,&
     &                     skpfld, cumode
         write (ipr, 8150)
         if (field .eq. ' ') write (ipr, 8155)
         do 1673 i = 1, obj%mxocode
            code = field(i:i)
            if (code .ne. ' ') then
               ipos = index(codelist, code)
               if (ipos .ne. 0) then
                  write (ipr, 8160) mesgw(ipos), idlist(ipos)
               end if
            end if
1673     continue

!        Print seisparm parameters from card 2.
         write (ipr, 8200) mpxfld, corfld, sclfld, obj%q_factor
         if (obj%ccoef_re .lt. 1 .and. obj%ccoef_im .lt. 1) then
            write (ipr, 8210) obj%ccoef_re, obj%ccoef_im
         end if
         if (obj%atrm_pow .gt. 0.0) write (ipr, 8260) obj%atrm_pow
         if (obj%btrm_pow .gt. 0.0) write (ipr, 8270) obj%btrm_pow
         if (obj%amix_fct .ne. 0.0) write (ipr, 8220) obj%amix_fct, obj%bmix_fct
         if (obj%alph_fct .ne. 0.0) write (ipr, 8230) obj%alph_fct, obj%ffth_dmn

      end if

!     Get scaling parameters.
      obj%scdp_scl = obj%scdp_num
      obj%ecdp_scl = obj%ecdp_num
      obj%stim_scl = obj%stim_win
      obj%etim_scl = obj%etim_win
      obj%swin_opt = .false.

      idummy = 0
      call buhci_get_iparm(obj, 'swin_opt', idummy)
      obj%swin_opt = (idummy .ne. 0)
      if (obj%swin_opt) then
         call buhci_get_iparm(obj, 'scdp_scl', obj%scdp_scl)
         if (obj%scdp_scl .lt. obj%scdp_num) then
            write (obj%print_lun, *) 'B_UHCI: Start scaling CDP &
     &must be >= start CDP.'
            ntr = FATAL_ERROR
            return
         end if
         call buhci_get_iparm(obj, 'ecdp_scl', obj%ecdp_scl)
         if (obj%ecdp_scl .gt. obj%ecdp_num) then
            write (obj%print_lun, *) 'B_UHCI: End scaling CDP &
     &must be <= end CDP.'
            ntr = FATAL_ERROR
            return
         end if
         call buhci_get_iparm(obj, 'stim_scl', obj%stim_scl)
         if (obj%stim_scl .lt. obj%stim_win) then
            write (obj%print_lun, *) 'B_UHCI: Start scaling time &
     &must be >= start time.'
            ntr = FATAL_ERROR
            return
         end if
         call buhci_get_iparm(obj, 'etim_scl', obj%etim_scl)
         if (obj%etim_scl .gt. obj%etim_win) then
            write (obj%print_lun, *) 'B_UHCI: End scaling time &
     &must be <= end time.'
            ntr = FATAL_ERROR
            return
         end if     

!        Print seisparm parameters from card 3.
         if (obj%dbug_opt .ge. 0) then
            write (ipr, 8610) obj%scdp_scl, obj%ecdp_scl, &
     &                        obj%stim_scl, obj%etim_scl
         end if
      end if

      obj%fss = obj%stim_scl / obj%sampratz + 1
      obj%nss = min0(int(obj%etim_scl/obj%sampratz+1),obj%numsmpz)-obj%fss+1
      obj%lss = obj%fss + obj%nss - 1

!     Get constraints for velocity picking.
      obj%ivel_min = 0
      obj%ivel_dmn = 25.0 
      obj%ivel_dmx = 25.0
      obj%ivel_dmn = 10.0
      obj%svel_dmx = 20.0
      obj%svel_min = 0
      if (obj%iunitsz .eq. ppavo_englishpz) then
         obj%ivel_max = 50000
         obj%svel_max = 50000
      else
         obj%ivel_max = 16700
         obj%svel_max = 16700
      endif
      obj%vcut_tim = 6000
      obj%vcut_pow  = 20
      obj%con_velp = .false.

      idummy = 0
      call buhci_get_iparm(obj, 'con_velp', idummy)
      obj%con_velp = (idummy .ne. 0)

      if (obj%con_velp) then
         call buhci_get_iparm(obj, 'ivel_min', obj%ivel_min)
         call buhci_get_iparm(obj, 'ivel_max', obj%ivel_max)
         if (obj%ivel_min .gt. obj%ivel_max) then
            write (obj%print_lun, *) 'B_UHCI: Min. interval &
     &velocity must be <= max. interval velocity.'
            ntr = FATAL_ERROR
            return
         end if
         call buhci_get_rparm(obj, 'ivel_dmn', obj%ivel_dmn)
         call buhci_get_rparm(obj, 'ivel_dmx', obj%ivel_dmx)
         call buhci_get_rparm(obj, 'svel_dmn', obj%svel_dmn)
         call buhci_get_rparm(obj, 'svel_dmx', obj%svel_dmx)
         call buhci_get_iparm(obj, 'svel_min', obj%svel_min)
         call buhci_get_iparm(obj, 'svel_max', obj%svel_max)
         if (obj%svel_min .gt. obj%svel_max) then
            write (obj%print_lun, *) 'B_UHCI: Min. stacking &
     &velocity must be <= max. stacking velocity.'
            ntr = FATAL_ERROR
            return
         end if
         call buhci_get_iparm(obj, 'vcut_tim', obj%vcut_tim)
         call buhci_get_iparm(obj, 'vcut_pow', obj%vcut_pow)
      
!        Print seisparm parameters from card 4. 
         if (obj%dbug_opt .ge. 0) then
            write (ipr,8600) obj%ivel_min, obj%ivel_max,&
     &                       obj%ivel_dmn, obj%ivel_dmx,&
     &                       obj%svel_dmn, obj%svel_dmx,&
     &                       obj%svel_min, obj%svel_max,&
     &                       obj%vcut_tim, obj%vcut_pow
         end if
      end if

!     Get dump velocity card defaults.
      obj%dump_opt = .false.
      obj%stim_dmp = obj%stim_win
      obj%etim_dmp = obj%etim_win
      obj%dtim_dmp = 100
      obj%scdp_dmp = obj%scdp_num
      obj%ecdp_dmp = obj%ecdp_num
      obj%dcdp_dmp = 1
      obj%veldname = ' '

!     Get time increment between velocity picks (set 0 for no dump).
      idummy = 0
      call buhci_get_iparm(obj, 'dump_opt', idummy)
      obj%dump_opt = (idummy .ne. 0)
      if (obj%dump_opt) then
         call buhci_get_iparm(obj, 'dtim_dmp', obj%dtim_dmp)
         if (obj%dtim_dmp .gt. lcrl) then
            write (obj%print_lun, *) 'B_UHCI: Time increment between &
     &dumped velocity picks > seismic record.'
         endif

!        Get start and end time to dump.
         call buhci_get_iparm(obj, 'stim_dmp', obj%stim_dmp)
         call buhci_get_iparm(obj, 'etim_dmp', obj%etim_dmp)
         if (obj%stim_dmp .le. 0) obj%stim_dmp = 0
         if (obj%etim_dmp .le. obj%stim_dmp) obj%etim_dmp = lcrl

!        Get start and end CDPs to dump.
         call buhci_get_iparm(obj, 'scdp_dmp', obj%scdp_dmp)
         call buhci_get_iparm(obj, 'ecdp_dmp', obj%ecdp_dmp)
         if (obj%scdp_dmp .lt. obj%scdp_num) obj%scdp_dmp = obj%scdp_num
         if (obj%scdp_dmp .eq. 0) obj%scdp_dmp = obj%scdp_num
         if (obj%ecdp_dmp .gt. obj%ecdp_num) obj%ecdp_dmp = obj%ecdp_num
         if (obj%ecdp_dmp .eq. 0) obj%ecdp_dmp = obj%ecdp_num

!        Get CDP increment for dumping.
         call buhci_get_iparm(obj, 'dcdp_dmp', obj%dcdp_dmp)
         if (obj%dcdp_dmp .le. 0) obj%dcdp_dmp = 1

!        Get filename in which to dump (append iteration number).
         call buhci_get_cparm(obj, 'veldname', obj%veldname, length)
         temp = len(obj%veldname)
         call ppavo_strlen(obj%veldname, temp, length)
         length = length + 1
         write (extension, 8700) obj%iter_num
         if (length .lt. len(obj%veldname) - 3) then
            obj%veldname(length:) = extension
         end if

!        Print seisparm parameters from card 4.
         if (obj%dbug_opt .ge. 0) then
            write (ipr, 8300) obj%dtim_dmp, obj%stim_dmp, obj%etim_dmp,&
     &                        obj%scdp_dmp, obj%ecdp_dmp, obj%dcdp_dmp,&
     &                        obj%veldname
         end if
      endif
      
!     Assign forced conversion defaults.
5000  obj%same_dbg = .false.
      obj%real_dbg = 0.0
      obj%imag_dbg = 0.0

      idummy = 0
      call buhci_get_iparm(obj, 'same_dbg', idummy)
      obj%same_dbg = (idummy .ne. 0)
      if (obj%same_dbg) then

!        Get the real and imaginary parts of forced conversion factor.
         call buhci_get_rparm(obj, 'real_dbg', obj%real_dbg)
         call buhci_get_rparm(obj, 'imag_dbg', obj%imag_dbg)
      endif

!     Memory allocation.
      allocate (obj%icdps(obj%ncdp_max+1))
      allocate (obj%fcdps(obj%ncdp_max+1))
      allocate (obj%folds(obj%cwin_cdp/2+1))
      allocate (obj%stats(obj%stawdt,obj%mxocode))

      if (obj%skip_opt .eq. 0) then
         allocate (obj%asum(obj%numsmpz))
         allocate (obj%bsum(obj%numsmpz))
         allocate (obj%absumr(obj%numsmpz))
         allocate (obj%absumi(obj%numsmpz))
         allocate (obj%wts(obj%numsmpz))
         allocate (obj%calibr(obj%numsmpz))
         allocate (obj%calibi(obj%numsmpz))
         allocate (obj%caliba(obj%numsmpz))
         allocate (obj%calibb(obj%numsmpz))
      endif

!     Reserve memory for saved trace.
      allocate (obj%newhd(obj%nthz))
      allocate (obj%newtr(obj%numsmpz))

!     Initialize the stats array.
      call ppavo_sauclr('B', obj%stats, obj%stawdt, obj%mxocode,&
     &                 obj%scal_fct, obj%scal_rms)

!     Further pre-processing.

!     In the initial phase of 'buhci', no traces will be passed.
!     Set the index of the next location into the 'fcdps' array.
!     When we start passing, we will do the 1st trace of the 1st CDP.
!     If the line starts with a killed trace, assume is from A.
!        mxfld  = 0        -> maximum CDP fold received.
!        pas2   = 0        -> CDP number to pass in 'pass' mode.
!        calia  = .false.
!        calib  = .false.  -> no correlation trace received.
!        cala   = .false.  -> no A power trace received.
!        calb   = .false.  -> no B power trace received.
!        lensb  = 0        -> last sequential B trace to output.
      obj%passed = 1
      obj%pcdp   = 1
      obj%nproc  = 0
      obj%nccor  = 0
      obj%jcdpa  = 0
      obj%jcdpb  = 0
      obj%nsmth  = obj%cwin_tim / obj%sampratz / 2 + 1
      obj%nta    = 0
      obj%hflag  = .false.
      obj%mxfld  = 0
      obj%pas2   = 0
      obj%calia  = .false.
      obj%calib  = .false.
      obj%cala   = .false.
      obj%calb   = .false.
      obj%lensb  = 0

!     Declare sparc common /p/ variables.
!       kpfcf  = 1  -> first call flag.
!       kpdrtf = 0  -> default return flag.
!       kpmotf = 0  -> more output flag.
!       kpmitf = 1  -> more input flag.
!       kpwarn = 0  -> warning flag.
      obj%kpfcf  = 1
      obj%kpdrtf = 0
      obj%kpmotf = 0
      obj%kpmitf = 1
      obj%kpwarn = 0

!     Force 'cwin_cdp' to be odd, even if specified even. Having done
!     this, let 'nc1' and 'nc2' denote the first and last CPDs which
!     will be passed.   
      if ((obj%cwin_cdp/2)*2 .eq. obj%cwin_cdp) obj%cwin_cdp = obj%cwin_cdp + 1
      obj%nc1 = max0(obj%scdp_num, 1)
      obj%nc2 = obj%ecdp_num

!     Zero out the maximum absolute values of the attributes.
!     Initialize the scale factors to unity.
      call ppavo_vzero(obj%limits, 1, obj%mxocode)
      call ppavo_vfill(1.0, obj%scalef, 1, obj%mxocode)

!     Allocate work file #1 to hold the intermediate results.
      obj%mem_alloc = 0

!WARNING
      obj%wkrecl = (4*obj%numsmpz) + (8*obj%nthz)
      if (obj%skip_opt .eq. 0) then
         obj%wkfln1 = obj%ncdp_max * obj%mxocode

         allocate (obj%ddp1tr(obj%numsmpz))
         allocate (obj%ddp1hd(obj%nthz))
         obj%kpwrkd(1) = 8 * obj%nthz
         obj%kpwrkd(2) = 4 * obj%numsmpz
         call ppavo_upawrk (obj%ppavo_obj, obj%wkfln1, obj%wkrecl, &
     &      'A', obj%kpwrks, obj%kpwrkd, ddnam1, ierr, erin)
         if (ierr .ne. 1) then
            write (obj%print_lun, *) 'B_UHCI: Unable to create &
     &scratch file #1.'
            ntr = FATAL_ERROR
            return
         endif

!        Zero out the disk.
         seqda = 1
         do i = 1, obj%wkfln1
            call ppavo_fowssd(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &          seqda, obj%ddp1hd, obj%ddp1tr, ioerr)
            if(ioerr .ne. 0) then
               ntr = FATAL_ERROR
               return
            end if
         enddo

!        Allocate work file #2 for held pre-stack
!        seismic traces and extra velocity traces.
         if (obj%mplx_opt .eq. 2) then
            obj%wkfln2 = (obj%lcmxfd + 1) * (obj%cwin_cdp/2 + 1)
            obj%kpwkd2(1) = 8 * obj%nthz
            obj%kpwkd2(2) = 4 * obj%numsmpz
            call ppavo_upawrk (obj%ppavo_obj, obj%wkfln2, obj%wkrecl, &
     &         'A', obj%kpwks2, obj%kpwkd2, ddnam2, ierr, erin)
            if (ierr .ne. 1) then
               write (obj%print_lun, *) 'B_UHCI: Unable to create &
     &scratch file #2.',ierr
               ntr = FATAL_ERROR
               return
            endif

!           Zero out the disk.
            seqda = 1
            do i = 1, obj%wkfln2
               call ppavo_fowssd(obj%ppavo_obj, obj%kpwks2, obj%kpwkd2, &
     &            seqda, obj%ddp1hd, obj%ddp1tr, ioerr)
               if(ioerr .ne. 0) then
                  ntr = FATAL_ERROR
                  return
               end if
            enddo
         end if
      endif

!     Get the header location of the wavelet center frequency.
!     (Note: 'wfloc' is not set in the proprietary module.
      obj%wfloc = 0

!     Set the length of the save area and the tool type.
      obj%len_sav   = obj%total_saved
      obj%itooltype = ppavo_complexpz
      obj%initialized = .true.

!     Figure out maximum ensemble folds based upon the multiplex mode.

      if (obj%mplx_opt-1 < 0 ) then
!       The 'mpx' option.
        obj%maxdtrz = obj%ntout
      elseif (obj%mplx_opt-1 == 0 ) then
!       The 'dmx' option.
        obj%maxdtrz = obj%lcmxfd
      else
!       The 'pas' option.
        obj%maxdtrz = obj%lcmxfd + obj%ntout
!       The 'skp' option.
        if (obj%skip_opt .ne. 0) obj%maxdtrz = 20
      endif
      return

!     Format statements.
8100  format(' SEISPARM PARAMETERS FROM DATA CARD(1):'/&
     &       ' --------------------------------------'/&
     &       '      STARTING CDP FOR ANALYSIS:         (SCDP) = ',I10/&
     &       '      ENDING CDP FOR ANALYSIS:           (ECDP) = ',I10/&
     &       '      MAXIMUM NO. OF CDPS TO PROCESS:  (NCDPMX) = ',I5/&
     &       '      STARTING TIME FOR ANALYSIS:         (STA) = ',I5,&
     &       ' (MS)'/&
     &       '      ENDING TIME FOR ANALYSIS:           (ETA) = ',I5,&
     &       ' (MS)'/&
     &       '      HILBERT TRANSFORM FILTER LENGTH: (HFLT_LEN) = ',I5,&
     &       ' (SAMPLES)'/&
     &       '      TIME WINDOW FOR AVERAGING:         (TWIN) = ',I5,&
     &       ' (MS)'/&
     &       '      CDP WINDOW FOR AVERAGING:          (CWIN) = ',I5,&
     &       ' (CDPS)'/&
     &       '      NUMBER OF AUX TRACES TO OUTPUT:   (NTOUT) = ',I5/&
     &       '      % CLIPPING FOR HISTOGRAM SCALING:(SCLFCT) = ',G12.5/&
     &       '      LINEAR SECTION RMS LEVEL:        (RMSSCL) = ',&
     &       G12.5/&
     &       '      SKIPPING OPTION:                 (SKPFLD) = ',A/&
     &       '      USER MODE:                       (CUCODE) = ',A)

8150  format(/' AUXILIARY TRACES TO BE OUTPUT:'/&
     &        ' ------------------------------')
8155  format(/'    ** NONE ** '/)
8160  format(5X, A40, 6X, '(TICD = ', I2, ')' )
8200  format(/' SEISPARM PARAMETERS FROM DATA CARD(2):'/&
     &        ' --------------------------------------'/&
     &        '      TRACE OUTPUT ORDER:              (MPXFLD) = ',A/&
     &        '      CORRELATION WEIGHTING OPTION:    (CORFLD) = ',A/&
     &        '      SCALING OPTION FOR INDICATORS:   (SCLFLD) = ',A/&
     &        '      QUALITY FACTOR:                       (Q) = ',F5.0)
8210  format('      REAL CORRELATION COEF FROM WELL:    (WRR) = ',F8.6/&
     &       '      IMAG CORRELATION COEF FROM WELL:    (WRI) = ',F8.6)
8220  format('      MIXING FACTOR FOR A-TERM:          (AMIX) = ',F5.2/&
     &       '      MIXING FACTOR FOR B-TERM:          (BMIX) = ',F5.2)
8230  format('      AGGRESSIVENESS FACTOR FOR HCI:    (ALPHA) = ',F5.3,&
     &       '      MINIMUM RANGE OF FLUID ANGLES:   (DTHMIN) = ',F5.2,&
     &       ' (DEGREES)')
8260  format('      POWER IN ZERO-OFFSET:              (POWA) = ',G12.5)
8270  format('      POWER IN SLOPE:                    (POWB) = ',G12.5)

8300  format(/' SEISPARM PARAMETERS FROM DATA CARD(5):'/&
     &       ' --------------------------------------'/&
     &       '      TIME INCREMENT BETWEEN PICKS    (DT_DUMP) = ', I5,&
     &       ' (MS)'/&
     &       '      STARTING TIME TO DUMP:          (ST_DUMP) = ', I5,&
     &       ' (MS)'/&
     &       '      ENDING TIME TO DUMP:            (ET_DUMP) = ', I5,&
     &       ' (MS)'/&
     &       '      STARTING CDP TO DUMP:              (FCTD) = ', I5/&
     &       '      ENDING CDP TO DUMP:                (LCTD) = ', I5/&
     &       '      CDP INCREMENT TO DUMP:             (DINC) = ', I5/&
     &       '      DUMP DATASET NAME:                (DNAME) = ', A)

8600  format(/' SEISPARM PARAMETERS FROM CARD(4):'/&
     &       ' --------------------------------'/&
     &       '      MINIMUM INTERVAL VELOCITY:        (MINIV) = ',I5,&
     &       ' (FT/S OR M/S)'/&
     &       '      MAXIMUM INTERVAL VELOCITY:        (MAXIV) = ',I5,&
     &       ' (FT/S OR M/S)'/&
     &       '      MAXIMUM NEGATIVE CORRECTION:     (MINPCT) = ',F7.1,&
     &       ' % OF INTERVAL VELOCITY'/&
     &       '      MAXIMUM POSITIVE CORRECTION:     (MAXPCT) = ',F7.1,&
     &       ' % OF INTERVAL VELOCITY'/&
     &       '      MAXIMUM NEGATIVE CORRECTION:      (MINDV) = ',F7.1,&
     &       ' % OF STACKING VELOCITY'/&
     &       '      MAXIMUM POSITIVE CORRECTION:      (MAXDV) = ',F7.1,&
     &       ' % OF STACKING VELOCITY'/&
     &       '      MINIMUM STACKING VELOCITY:        (MINSV) = ',I5,&
     &       ' (FT/S OR M/S)'/&
     &       '      MAXIMUM STACKING VELOCITY:        (MAXSV) = ',I5,&
     &       ' (FT/S OR M/S)'/&
     &       '      CUTOFF TIME FOR VELOCITY ANALYSIS:(VTIME) = ',I5,&
     &       ' (MS)'/&
     &       '      CUTOFF SHARPNESS:                  (VPOW) = ',I5/)

8610  format(/' SEISPARM PARAMETERS FROM CARD(3):'/&
     &        ' --------------------------------'/&
     &        '      STARTING CDP OF SCALING WINDOW:   (SSCDP) = ',I5/&
     &        '      ENDING CDP OF SCALING WINDOW:     (ESCDP) = ',I5/&
     &        '      START TIME OF SCALING WINDOW:     (SSTIM) = ',I5,&
     &       ' (MS)',&
     &        '      END TIME OF SCALING WINDOW:       (ESTIM) = ',I5,&
     &       ' (MS)'/)

8700  format('.', I2.2)

      end subroutine buhci_init


!!---------------------------- buhci_work -----------------------------------!!
!!---------------------------- buhci_work -----------------------------------!!
!!---------------------------- buhci_work -----------------------------------!!

      subroutine buhci_work (obj,ntr,hd,tr,promode)
      type(buhci_struct),intent(inout) :: obj              ! arguments
      integer            ,intent(inout) :: ntr              ! arguments
      double precision   ,intent(inout) :: hd(:)          ! arguments
      real               ,intent(inout) :: tr(:)          ! arguments
      integer            ,intent(inout) :: promode         ! arguments

!     Local parameters.
      integer :: ddt       ! distance for delta-t computations.
      integer :: tinc      ! time increment for velocity display.
      integer :: sdunit    ! starting fortain i/o unit for velf card dumps.
      integer :: datum     ! datum elevation.
      integer :: bulk1     ! time of first sample (in promax, always 0!)
      integer :: maxvmsg   ! max. number of velocity constrain violations.
      real    :: sclprm    ! global scale factor multiplier.
      real    :: scalar    ! scalar

      parameter ( ddt = 1000 )
      parameter ( tinc = 0 )
      parameter ( sdunit = 70 )
      parameter ( datum = 0 )
      parameter ( bulk1 = 0 )
      parameter ( maxvmsg = 4 )
      parameter ( sclprm = 1000.0 )

!     Local variables.
      character(len=3)  :: vfu, cvfu ! holds the string of 'vfu' for dump file.
      character(len=10) :: ctext(ppavo_b_mxocode) ! description of each type of
                                              ! aux trace generated.
      character(len=15) :: cmpx(0:2) ! either 'multiplexed', 'du-multiplexed',
                                     ! or 'special mpx'.
      character(len=29) :: velmsg(maxvmsg) ! array of velocity constraint
                                               ! errors.

      integer           :: dttab(0:2)
      integer           :: jtab(0:ppavo_b_mxocode)
      integer           :: ctab(11:51)

      integer           :: iticd


      integer           :: ipos
      integer           :: iloc
      integer           :: erin
      integer           :: ii   
      integer           :: gflag
      integer           :: ioerr
      integer           :: temp

      double precision  :: dx(1)


!     Data initialization.
      data dttab    / 0, 2, 3 /
      data cmpx     / 'MULTIPLEXED', 'DEMULTIPLEXED ', 'SPECIAL MPX' /
      data velmsg   / 'STACKING VELOCITY IS TOO LOW ',&
     &                'STACKING VELOCITY IS TOO HIGH',&
     &                'INTERVAL VELOCITY IS TOO LOW ',&
     &                'INTERVAL VELOCITY IS TOO HIGH' /  

!     Correspondence between external codes and internal codes.
!     external codes:   11 12  13 14-16 17  18  19-40 41  42  43  44
      data ctab       / 13, 0, 14, 3*0, 03, 04, 22*0, 08, 09, 01, 02,&
     &                  05, 10, 06, 07, 11, 12, 13 /
!     external codes:   45  46  47  48  49  50  51

!     internal codes:    0   1   2   3   4   5   6   7   8   9  10  11
      data jtab       / 02, 43, 44, 17, 18, 45, 47, 48, 41, 42, 46, 49,&
     &                  50, 51, 13 /
!     internal codes:   12  13  14

      data ctext / 'RE {A}', 'RE {B}', 'IM {A}',   'IM {B}',&
     &             '<|A|>' , '<|B|>' , 'RE {R}',   'IM {R}',&
     &             'B_UHCI'  , 'RVI'   , 'A - OVB',  'B - OVB',&
     &             'STK VEL', 'INT VEL'  /
      data cvfu       / 'VFU' /  

      obj%ipr    = obj%iounitz         ! printer unit.
      obj%kprtf  = obj%kpdrtf          ! return flag.
      obj%memt   = .false.             ! no temp memory reserved.
      obj%intcde = 0                   ! internal code of passed trace.
      obj%isi    = obj%sampratz * 1e3  ! sampling interval (usecs).
      obj%cpyflg = .true.

      gflag = 0
      
!     Final exit.
      if (obj%cleanupz) then

!        Set the kpmitf flag to zero (may not be necessary).
         obj%kpmitf = 0
!        Free any permanently reserved memory.
         if (obj%kpmitf .eq. 0) then
            obj%nss = (obj%ns+1) * 2
            if (obj%skip_opt .eq. 0) then
               if (associated(obj%asum))   deallocate(obj%asum)
               if (associated(obj%bsum))   deallocate(obj%bsum)
               if (associated(obj%absumr)) deallocate(obj%absumr)
               if (associated(obj%absumi)) deallocate(obj%absumi)
               if (associated(obj%wts))    deallocate(obj%wts)
               if (associated(obj%calibr)) deallocate(obj%calibr)
               if (associated(obj%calibi)) deallocate(obj%calibi)
               if (associated(obj%caliba)) deallocate(obj%caliba)
               if (associated(obj%calibb)) deallocate(obj%calibb)
!              Close and delete work files.
               call ppavo_focdd(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd)
               call ppavo_uguwrk(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%err, erin)
               if(obj%mplx_opt .eq. 2) then
                  call ppavo_focdd(obj%ppavo_obj, obj%kpwks2, obj%kpwkd2)
                  call ppavo_uguwrk(obj%ppavo_obj, obj%kpwks2, obj%kpwkd2, &
     &               obj%err, erin)
               end if

               if (obj%dump_opt) close (obj%dunit)
            endif
         endif 

!        Print a list of CDPs processed.
         if (obj%dbug_opt .ge. 0) then
            write (obj%ipr, 9075) obj%lineno,(abs(obj%icdps(ii)),&
     &         ppavo_sauskp(obj%icdps(ii), obj%nc1, obj%nc2),&
     &         ii=1, obj%jcdpb)
         end if

!        Compute and print the global statistics.
         if (obj%dbug_opt .ge. 0) then 
            call ppavo_saustp(obj%stats, obj%stawdt, obj%ntout,&
     &         obj%oticd, ctab, ctext,&
     &         obj%mintid, obj%maxtid, obj%mxocode, obj%ipr)
         end if 

!        Free the remaining memory.
         if (associated(obj%icdps)) deallocate(obj%icdps)
         if (associated(obj%fcdps)) deallocate(obj%fcdps)
         if (associated(obj%folds)) deallocate(obj%folds)
         if (associated(obj%stats)) deallocate(obj%stats)

!        Print some final farewell messages.
         if (obj%same_dbg) write (obj%ipr, 9110)
         if (obj%calib) write (obj%ipr, 9130)
         if (obj%mplx_opt .eq. 2) then
            if (obj%mxfld .gt. obj%lcmxfd) then
               obj%kpwarn = -1
               if (obj%dbug_opt .ge. 0) then
                  write (obj%ipr, 9120) obj%mxfld, obj%lcmxfd
               end if
            endif
            if (obj%dbug_opt .ge. 0) then
               write (obj%ipr, 8852) cmpx(obj%mplx_opt), obj%mxfld
            end if
         else
            if (obj%dbug_opt .ge. 0) then
               write (obj%ipr, 8850) cmpx(obj%mplx_opt)
            end if
         endif
         if (obj%ntout .gt. 0) then
            if (obj%dbug_opt .ge. 0) then
               write (obj%ipr, 8900) obj%nccor, obj%nproc/obj%ntout
            end if
         else
            if (obj%dbug_opt .ge. 0) then
               write (obj%ipr, 8910) obj%nccor
            end if
         endif
         goto 1000
      endif

!     Are we at the end of the job?
!     If so, signify no more traces.
      if (hd(ppavo_end_jobz) .ne. 0) then
         obj%kpmitf = 0
         hd(ppavo_end_jobz) = 0
      endif

99    continue

!     First pass initialization.
      
!     Test the sparc 'first call flag'.
!     Be sure to reset it for next time.
      if (obj%kpfcf .ne. 0 .or. gflag .eq. 100) then
!!!!      if (obj%kpfcf .ne. 0) then
         if(gflag .eq. 100) then
            gflag = 0
            goto 100
         end if

         if (obj%kpmitf .eq. 0) then
            write (obj%ipr, 9050) obj%kpfcf, obj%kpmitf
            obj%linend = .true.
            obj%kprtf = -1
            return
         endif

!        Remember the trace and header lengths.
         obj%ns     = obj%numsmpz
         obj%thl    = obj%nthz
         obj%numvel = 0
         obj%maxvel = 0

!        Open the dump file for writing the velf cards.
         obj%icdpn = hd(ppavo_cdpz)
         if (ppavo_iline_noz .eq. ppavo_nullpz) then
            obj%lineno = 0
         else
            obj%lineno = hd(ppavo_iline_noz)
         endif 

         obj%kpfcf = 0
         obj%linend = .false.

!        Re-initialize for a new 3d line.
100      continue
         if (obj%dump_opt) then
            obj%dunit = obj%kprno + sdunit
            open (obj%dunit, file=obj%veldname)
            call ppavo_time_carr(obj%idatez, obj%when)
            temp = len(obj%when)
            call ppavo_strndx(obj%when, temp, obj%lwhen)
            write (obj%dunit, 8010) obj%icdpn, obj%lineno, obj%kprno,&
     &                              obj%when(:obj%lwhen-1)
            write (obj%dunit, 8020) obj%icdpn, obj%lineno, obj%cwin_tim,&
     &                              obj%cwin_cdp, obj%wfreq, obj%q_factor
            write (obj%dunit, 8030) obj%icdpn, ddt, datum,&
     &                              tinc, obj%lineno
8010        format('VELF', t10, 'C', 2i9, t21, 'FROM B_UHCI-',&
     &             i1,' AT ', a)
8020        format('VELF', t10, 'C', 2i9, t21, '(',i9,' MS X',&
     &             i9,' CDPS); FREQ = ', f6.1, ' HZ; Q = ', f6.1)
8030        format('VELF', t11, 2i9, t41, i9, t51, i9, t76, i9)
         endif
         obj%passed  = 1
         obj%pcdp    = 1
         obj%kpdrtf  = 0
         obj%kprtf   = 0
         obj%jcdpa   = 0
         obj%jcdpb   = 0
         obj%lasticd = obj%jta
         obj%nsmth   = obj%cwin_tim / obj%sampratz / 2 + 1
         obj%nta     = 0
         obj%hflag   = .false.
         obj%mxfld   = 0
         obj%pas2    = 0
         obj%calib   = .false.
         obj%cala    = .false.
         obj%calb    = .false.

!        Zero out the maximum absolute values of the attributes.
!        Initialize the scale factors to unity.     
         call ppavo_vzero(obj%limits, 1, obj%mxocode)
         call ppavo_vfill(1.0, obj%scalef, 1, obj%mxocode)
         if (obj%hflag) then
            write (obj%ipr,9310) obj%mxocode
            obj%kprtf = -1
            return
         endif

!        Zero reserved common. Initialize global statistics.
         if (obj%skip_opt .eq. 0) then
            call ppavo_vzero(obj%asum,   1, obj%numsmpz)
            call ppavo_vzero(obj%bsum,   1, obj%numsmpz)
            call ppavo_vzero(obj%absumr, 1, obj%numsmpz)
            call ppavo_vzero(obj%absumi, 1, obj%numsmpz)
            call ppavo_vzero(obj%wts,    1, obj%numsmpz)
            call ppavo_vzero(obj%calibr, 1, obj%numsmpz)
            call ppavo_vzero(obj%calibi, 1, obj%numsmpz)
            call ppavo_vzero(obj%caliba, 1, obj%numsmpz)
            call ppavo_vzero(obj%calibb, 1, obj%numsmpz) 
         endif

!        Zero the stawdt, and CDP and fold arrays.
         call ppavo_vzero(obj%icdps, 1, obj%ncdp_max+1)
         call ppavo_vzero(obj%fcdps, 1, obj%ncdp_max+1)
         call ppavo_vzero(obj%folds, 1, obj%cwin_cdp/2+1)
         call ppavo_sauclr('B', obj%stats, obj%stawdt, obj%mxocode,&
     &                    obj%scal_fct, obj%scal_rms)

      endif

!     All traces come here. Handle various special circumstances.

!     Under the 'pas' option, play back the stored pre-stack gathers.

120   continue

!     Verify that trace and header lengths have not changed.
      if (obj%ns .ne. obj%numsmpz) then
         write (obj%print_lun, *) 'B_UHCI: Trace length has changed.'
         ntr = FATAL_ERROR
         return
      end if
      if (obj%thl .ne. obj%nthz) then
         write (obj%print_lun, *) 'B_UHCI: Header length has changed.'
         ntr = FATAL_ERROR
         return
      end if

      if (obj%pas2 .gt. 0) then
         obj%fndx = mod(obj%pcdp-1, obj%cwin_cdp/2+1)
         obj%fold = obj%folds(obj%fndx+1) + 1
         obj%kpmotf = 1
         obj%kprtf  = 1
         obj%intcde = 0

         if (obj%pas2 .lt. obj%fold .and. obj%fold .gt. 1) then
            obj%filep2 = obj%fndx*(obj%lcmxfd+1) + obj%pas2
            iloc = 1
            if (obj%dbug_opt .ge. 2) then
               write (obj%ipr, 8810) obj%filep2, iloc
            end if
            call ppavo_uhci_read(obj%ppavo_obj, obj%kpwks2, obj%kpwkd2, &
     &         obj%filep2, tr, hd, obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr,&
     &         ioerr)

            hd(ppavo_seq_noz) = obj%pas2 + obj%ntout
            hd(ppavo_repeatz) = 0
            if (obj%pas2 .eq. obj%fold-1) hd(ppavo_end_ensz) = ppavo_lasttrpz
            obj%ticd  = hd(ppavo_trc_typez)
            obj%icdpn = hd(ppavo_cdpz)

            if (ppavo_iline_noz .eq. ppavo_nullpz) then
               obj%lineno = 0
            else
               obj%lineno = hd(ppavo_iline_noz)
            endif
            obj%pas2  = obj%pas2 + 1

!           We've just finished regurgitating this CDP.
         else
            obj%folds(obj%fndx+1) = 0
            obj%kpmotf = 0
            obj%kprtf  = 0
            obj%pas2   = 0
            obj%pcdp   = obj%pcdp + 1
            if (obj%linend) then
               gflag = 600
!!!!              goto 600
            end if
         end if

!!!!         goto 1000
         if(gflag .eq. 0) goto 1000

!        Under the 'pas' option, play back remaining HCIs.
      else if (obj%passed .gt. 1 .and. obj%mplx_opt .eq. 2) then
!!!!         goto 820
         gflag = 820
      end if

      if(gflag .ne. 0) goto 199

!     Are we at the end of a 3-d line? If not, copy the input
!     trace (unless it's the first trace of a new line).
      if (.not. obj%linend) then
         if (obj%cpyflg) then 
            call ppavo_vmove(hd, 1, obj%newhd, 1, obj%thl)
            call ppavo_vmove(tr, 1, obj%newtr, 1, obj%ns)
         endif

         if (ppavo_iline_noz .eq. ppavo_nullpz) then
            obj%cline = 0
         else
            obj%cline = obj%newhd(ppavo_iline_noz)
         endif
         obj%linend = obj%lineno .ne. obj%cline .or. obj%kpmitf .ne. 1
      endif

!     If 'skp' is in effect, simply pass the trace if selected.
      if (obj%skip_opt .ne. 0) then
         obj%intcde = 0
!!!         if (obj%linend) goto 900
         if (obj%linend) then
            gflag = 900
!!!!            goto 900
         end if
         if(gflag .ne. 0) goto 151

         obj%ticd  = obj%newhd(ppavo_trc_typez)
         obj%icdpn = obj%newhd(ppavo_cdpz)
         do 150 ii = 1, obj%ntout
            if (obj%ticd .eq. obj%oticd(ii)) then
               obj%intcde = ii
            end if
150      continue

         obj%fndx = ppavo_saufdp(obj%icdps, obj%jcdpb, obj%icdpn)
         if (obj%icdps(obj%fndx+1) .eq. 0) then
            if (obj%jcdpb .lt. obj%ncdp_max) then
               obj%icdps(obj%jcdpb+1) = obj%icdpn
               obj%fcdps(obj%jcdpb+1) = obj%icdpn
               obj%jcdpb = obj%jcdpb + 1
            else
               if (obj%kpwarn .eq. 0) write (obj%ipr, 9200) obj%ncdp_max
               obj%kpwarn = -1
            endif
         endif 

!        'skp' also compute the statistics of all traces passed.
         if (obj%icdpn .ge. obj%nc1 .and. obj%icdpn .le. obj%nc2&
     &       .and. obj%intcde .ne. 0) then
            obj%live = .true.
            if (obj%icdpn .ge. obj%scdp_scl .and. &
     &          obj%icdpn .le. obj%ecdp_scl) then
               call ppavo_saustc(obj%newtr, obj%fss, obj%lss,&
     &            obj%stats, obj%stawdt, ctab(obj%ticd), obj%live)
            end if
            if (.not. obj%live) then
               obj%icdps(obj%fndx+1) = -obj%icdpn
               obj%fcdps(obj%fndx+1) = -obj%icdpn
            end if
            call ppavo_vmove(obj%newhd, 1, hd, 1, obj%thl)
            call ppavo_vmove(obj%newtr, 1, tr, 1, obj%ns)

            obj%nproc = obj%nproc + 1
            obj%kprtf = 1
         endif

         goto 1000

151      continue

      endif

199   continue

!     Copy the 'A' trace to the scratch file.
!!!!      if (.not. obj%linend .and. (gflag .eq. 0 .or. gflag .eq. 300)) then
      if ((.not. obj%linend .and. gflag .ne. 820 .and. gflag .ne. 900&
     &.and. gflag .ne. 600) .or. gflag .eq. 300) then
         if(gflag .eq. 300) then
            goto 201
         end if

!        Check the trace header of the current trace.
!        A killed trace belonging to a CDP that we've
!        seen before is treated like a dead 'B' trace.
         obj%ticd  = obj%newhd(ppavo_trc_typez)
         obj%icdpn = obj%newhd(ppavo_cdpz)
         obj%flv   = obj%newhd(ppavo_live_sz) / obj%sampratz + 1
         obj%llv   = obj%newhd(ppavo_live_ez) / obj%sampratz + 1
         obj%fndx  = ppavo_saufdp(obj%icdps, obj%jcdpa, obj%icdpn)
         obj%slope = obj%ticd .eq. obj%jtb .or. &
     &               obj%ticd .eq. obj%jtbno .or. &
     &      (obj%ticd .eq. obj%jtkill .and. obj%fndx .lt. obj%jcdpa)
         obj%zoff  = obj%ticd .eq. obj%jta .or. &
                     obj%ticd .eq. obj%jtano

!        Hilbert transform zero-offset and slope traces.
         call ppavo_vmove(obj%newhd, 1, hd, 1, obj%thl)

         obj%ntwin = obj%cwin_tim / obj%sampratz + 1
         obj%l2    = (obj%cwin_tim - 1)/2 + 1
         obj%fstu  = max0(obj%flv, obj%fsa, 1)
         obj%lstu  = min0(obj%llv, obj%lsa, obj%ns)
         obj%nstu  = obj%lstu - obj%fstu + 1
         obj%ndx1  = max0(obj%fstu - obj%l2 + 1, 1)
         obj%ndx2  = min0(obj%lstu + obj%l2 - 1, obj%ns)
         call ppavo_vzero(obj%rtempz, 1, obj%ns)

         if (obj%zoff .or. obj%slope) then
            if (obj%ticd .ne. obj%jtkill) then
               call ppavo_mahlbt(obj%newtr, obj%rtempz, obj%ns,&
     &            obj%ndx1, obj%ndx2, obj%hflt_len)
            else
               call ppavo_vzero(tr,  1, obj%ns)
               call ppavo_vzero(obj%rtempz, 1, obj%ns)
            endif
         endif
!DBG         if (obj%dbug_opt .gt. 0) err = mmchecka()

201      continue

!        Handle the zero-offset 'A' traces.
         if (obj%zoff .and. gflag .ne. 300) then
            if (obj%fndx .lt. obj%jcdpa) goto 1000
            if (obj%jcdpa .ge. obj%ncdp_max) then
               if (obj%kpwarn .eq. 0) then
                  write (obj%ipr, 9200) obj%ncdp_max
               end if
               obj%kpwarn = -1
               obj%intcde = 0
               goto 1000
            endif

!           Write this trace to scratch file #1.
            obj%filep1 = obj%mxocode*obj%jcdpa + obj%kta
            iloc = 2
            if (obj%dbug_opt .ge. 2) then
               write (obj%ipr, 8820) obj%filep1,iloc
            end if
            call ppavo_uhci_write(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &         obj%filep1, obj%newtr, obj%newhd, obj%thl, obj%ns, &
     &         obj%ddp1hd, obj%ddp1tr, ioerr)
            obj%filep1 = obj%mxocode*obj%jcdpa + obj%ktaq
            hd(ppavo_trc_typez) =  obj%jtaq

            iloc = 3

            if (obj%dbug_opt .ge. 2) then
               write (obj%ipr, 8820) obj%filep1,iloc
            end if
            call ppavo_uhci_write(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &         obj%filep1, obj%rtempz, hd, obj%thl, obj%ns, &
     &         obj%ddp1hd, obj%ddp1tr, ioerr)
            if (obj%icdpn .ge. obj%scdp_scl .and. &
     &          obj%icdpn .le. obj%ecdp_scl) then
               call ppavo_saustc(obj%newtr, obj%fss, obj%lss,&
     &            obj%stats, obj%stawdt, obj%kta, obj%live)
               call ppavo_saustc(obj%rtempz, obj%fss, obj%lss, &
     &            obj%stats, obj%stawdt, obj%ktaq, obj%live)
               call ppavo_maxmgv(obj%newtr(obj%fss:), 1, &
     &            obj%amxtt, ipos, obj%nss)
               if (obj%amxtt .gt. obj%limits(obj%kta)) then
                  obj%limits(obj%kta) = obj%amxtt
               end if
               call ppavo_maxmgv(obj%rtempz(obj%fss:), 1, &
     &            obj%amxtt, ipos, obj%nss)
               if (obj%amxtt .gt. obj%limits(obj%ktaq)) then
                  obj%limits(obj%ktaq) = obj%amxtt
               end if
            endif

!           Bump the 'A' trace pointer. Update the CDP list.
            obj%icdps(obj%jcdpa+1) = obj%icdpn
            obj%fcdps(obj%jcdpa+1) = obj%icdpn
            obj%jcdpa = obj%jcdpa + 1
!DBG            if (obj%dbug_opt .gt. 0) err = mmchecka()

!        It's a 'B' trace: prepare to spit out some results.
         else if (obj%slope .or. gflag .eq. 300) then

            if(gflag .eq. 300) then
               gflag = 0
               goto 300
            end if

            obj%jcdpb = obj%fndx
            if (obj%jcdpb .ge. obj%jcdpa) then
               if (obj%kpwarn .eq. 0) then
                  write (obj%ipr, 9300) obj%jcdpa
               end if
               obj%kpwarn = -1
               obj%intcde = 0
               obj%kpmitf = 0
               goto 1000
            endif
            obj%acdpn = abs(obj%fcdps(obj%jcdpb+1))

!           Check to make sure the CDP numbers match!
            if (obj%acdpn .ne. obj%icdpn) then
               write (obj%ipr, 9400) obj%jcdpb+1, obj%acdpn, obj%icdpn,&
     &            obj%zoff, obj%slope, obj%ticd
               obj%kprtf = -1
            endif

!           Remember the number of the last 'B' trace within the output
!           range (this is needed to set the end-of-ensemble flag).
            if (obj%icdpn .ge. obj%nc1 .and. obj%icdpn .le. obj%nc2) then       
               obj%lensb = obj%jcdpb
            end if

!           Allocate temporary storage for averaging and HCI comp.

!           Note:  we come here:
!           1) by dropping down after receiving a slope trace, or       
!           2) at the end of the dataset after having passed a prestack 
!              gather under 'pas' mode. in this situation, we now have 
!              to compute the lagging output data                       

300         continue

            if (obj%mem_alloc .eq. 0) then
               obj%mem_alloc = 1
               allocate (obj%rcax(obj%ns))
               allocate (obj%rcbx(obj%ns))
               allocate (obj%wfun(obj%ns))
               allocate (obj%dvv(obj%ns))
               allocate (obj%kpbegtr(obj%ns*obj%mxocode))
               allocate (obj%kpbeghd(obj%thl*obj%mxocode))
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()
            endif

            do 380 ii=1, obj%mxocode
               obj%t(ii)%ptr => obj%kpbegtr(((ii-1)*obj%ns):)
               obj%h(ii)%ptr => obj%kpbeghd(((ii-1)*obj%thl):)
380         continue
            obj%memt = .true.

            if (obj%linend) goto 500

!           Recover the corresponding complex 'A' trace.
            call ppavo_vmove(obj%newhd, 1, obj%h(obj%ktb)%ptr, 1, obj%thl)
            call ppavo_vmove(obj%newtr, 1, obj%t(obj%ktb)%ptr, 1, obj%ns)
            obj%filep1 = obj%mxocode*obj%jcdpb + obj%kta
            iloc = 4
            if (obj%dbug_opt .ge. 2) write (obj%ipr, 8800) obj%filep1,iloc
            call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &         obj%filep1, obj%t(obj%kta)%ptr, obj%h(obj%kta)%ptr, &
     &         obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
            obj%filep1 = obj%mxocode*obj%jcdpb + obj%ktaq
            iloc = 5
            if (obj%dbug_opt .ge. 2) write (obj%ipr, 8800) obj%filep1,iloc
            call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &         obj%filep1, obj%t(obj%ktaq)%ptr, obj%h(obj%ktaq)%ptr, &
     &         obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
!DBG            if (obj%dbug_opt .gt. 0) err = mmchecka()

!           If 'same_dbg' is specified, 'B' becomes a fixed
!           multiple of 'A' (for debugging purposes only).
            if (obj%same_dbg) then
               call ppavo_vmove(obj%t(obj%kta)%ptr,  1, &
     &            obj%t(obj%ktb)%ptr,  1, obj%ns)
               call ppavo_vmove(obj%t(obj%ktaq)%ptr, 1, &
     &            obj%t(obj%ktbq)%ptr, 1, obj%ns)
               call ppavo_vsmul(obj%t(obj%ktb)%ptr, 1, obj%real_dbg,&
     &            obj%t(obj%ktb)%ptr,  1, obj%ns)
               call ppavo_vsmul(obj%t(obj%ktbq)%ptr, 1, obj%real_dbg,&
     &            obj%t(obj%ktbq)%ptr, 1, obj%ns)
               call ppavo_vsma(obj%t(obj%ktaq)%ptr, 1, -obj%imag_dbg,&
     &            obj%t(obj%ktb)%ptr, 1, obj%t(obj%ktb)%ptr,  1, obj%ns)
               call ppavo_vsma(obj%t(obj%kta)%ptr,  1, obj%imag_dbg,&
     &            obj%t(obj%ktbq)%ptr, 1, obj%t(obj%ktbq)%ptr, 1, obj%ns)

!           Copy and set trace & headers for complex 'B' trace.
            else
               call ppavo_vmove(tr,  1, obj%t(obj%ktb)%ptr,  1, obj%ns)
               call ppavo_vmove(obj%rtempz, 1, obj%t(obj%ktbq)%ptr, 1, obj%ns)
            endif

            call ppavo_vmove(hd,  1, obj%h(obj%ktb)%ptr,  1, obj%thl)

            if (obj%ticd .ne. obj%jtkill) then
               hd(ppavo_trc_typez) = obj%jtbq
            end if

            call ppavo_vmove(hd, 1, obj%h(obj%ktbq)%ptr, 1, obj%thl)
!DBG            if (obj%dbug_opt .gt. 0) err = mmchecka()

!           Write the complex 'B' trace to the file.
            obj%filep1 = obj%mxocode*obj%jcdpb + obj%ktb
            iloc = 6
            if (obj%dbug_opt .ge. 2) write (obj%ipr, 8820) obj%filep1,iloc
            call ppavo_uhci_write(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &         obj%filep1, obj%t(obj%ktb)%ptr, obj%h(obj%ktb)%ptr, &
     &         obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
            obj%filep1 = obj%mxocode*obj%jcdpb + obj%ktbq
            iloc = 7
            if (obj%dbug_opt .ge. 2) write (obj%ipr, 8820) obj%filep1,iloc
            call ppavo_uhci_write(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &         obj%filep1, obj%t(obj%ktbq)%ptr, obj%h(obj%ktbq)%ptr, &
     &         obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
!DBG            if (obj%dbug_opt .gt. 0) err = mmchecka()

!           Compute the statistics of the complex 'B' trace.
            if (obj%icdpn .ge. obj%scdp_scl .and. &
     &          obj%icdpn .le. obj%ecdp_scl) then
               call ppavo_saustc(obj%t(obj%ktb)%ptr, obj%fss, obj%lss,&
     &            obj%stats, obj%stawdt, obj%ktb, obj%live)
               call ppavo_saustc(obj%t(obj%ktbq)%ptr, obj%fss, obj%lss,&
     &            obj%stats, obj%stawdt, obj%ktbq, obj%live)
               call ppavo_maxmgv(obj%t(obj%ktb)%ptr(obj%fss:), 1,&
     &            obj%amxtt, ipos, obj%nss)
               if (obj%amxtt .gt. obj%limits(obj%ktb)) then
                  obj%limits(obj%ktb) = obj%amxtt
               end if
               call ppavo_maxmgv(obj%t(obj%ktbq)%ptr(obj%fss:), 1,&
     &            obj%amxtt, ipos, obj%nss)
               if (obj%amxtt .gt. obj%limits(obj%ktbq)) then
                  obj%limits(obj%ktbq) = obj%amxtt
               end if
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()
            endif

!           Construct the weighting function for the current trace.
            if (obj%avg_flag .and. obj%ticd .ne. obj%jtkill) then
               if (obj%nstu .gt. 0) then
!                 call ppavo_vfill(0.0, obj%wfun(0:), 1, obj%ns)
                  call ppavo_vfill(1.0, obj%wfun(obj%fstu:), 1, obj%nstu)

!                 Add the current trace to the running sums.
                  obj%factor = 1.0
                  call ppavo_sauavga(obj%asum, obj%bsum,&
     &               obj%absumr,  obj%absumi, obj%wts,&
     &               obj%t(obj%kta)%ptr, obj%t(obj%ktaq)%ptr, &
     &               obj%t(obj%ktb)%ptr, obj%t(obj%ktbq)%ptr, &
     &               obj%wfun, obj%rcax, obj%rcbx, obj%ns, obj%fstu, obj%lstu, &
     &               obj%ntwin, obj%factor, obj%corr_opt, obj%icdpn, &
     &               obj%dbug_opt, obj%ipr)
               endif
            endif
!DBG            if (obj%dbug_opt .gt. 0) err = mmchecka()

!           Determine which trace (if any) to remove from the running sums. 
!           Determine which trace (if any) for which to compute the HCI's.
            obj%trailtr = obj%jcdpb - obj%cwin_cdp
            obj%outtr   = obj%jcdpb - obj%cwin_cdp/2

!           Remove trace 'trailtr' from the running sums.

500         continue
            if (obj%trailtr .ge. 0 .and. obj%avg_flag) then

!              Recover the trailing 'A' & 'B' traces
!              and their weighting functions.
               obj%filep1 = obj%mxocode*obj%trailtr + obj%kta
               iloc = 8
               if (obj%dbug_opt .ge. 2) write (obj%ipr, 8800) obj%filep1,iloc
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%kta)%ptr, obj%h(obj%kta)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
               obj%filep1 = obj%mxocode*obj%trailtr + obj%ktaq
               iloc = 9
               if (obj%dbug_opt .ge. 2) write (obj%ipr, 8800) obj%filep1,iloc
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%ktaq)%ptr, obj%h(obj%ktaq)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
               obj%filep1 = obj%mxocode*obj%trailtr + obj%ktb
               iloc = 10
               if (obj%dbug_opt .ge. 2) write (obj%ipr, 8800) obj%filep1,iloc
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%ktb)%ptr, obj%h(obj%ktb)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
               obj%filep1 = obj%mxocode*obj%trailtr + obj%ktbq
               iloc = 11
               if (obj%dbug_opt .ge. 2) write (obj%ipr, 8800) obj%filep1,iloc
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%ktbq)%ptr, obj%h(obj%ktbq)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Retrieve some trace header elements. Calculate the
!              first & last samples to use in the correlations.
!              Set up the data mask in 'wfun'.
               obj%ticd  = obj%h(obj%ktb)%ptr(ppavo_trc_typez)
               obj%icdpn = obj%h(obj%ktb)%ptr(ppavo_cdpz)
               obj%flv   = obj%h(obj%ktb)%ptr(ppavo_live_sz) / obj%sampratz + 1
               obj%llv   = obj%h(obj%ktb)%ptr(ppavo_live_ez) / obj%sampratz + 1
               obj%fstu  = max0(obj%flv, obj%fsa, 1)
               obj%lstu  = min0(obj%llv, obj%lsa, obj%ns)
               obj%nstu  = obj%lstu - obj%fstu + 1
               call ppavo_vzero(obj%wfun, 1, obj%ns)
               if (obj%ticd .ne. obj%jtkill) then
                  if (obj%nstu .gt. 0) then
                     call ppavo_vfill(1.0, obj%wfun(obj%fstu:), 1, obj%nstu)

!                    Subtract the trailing trace from the
!                    running sums (if it's live).
                     obj%factor = -1.0
                     call ppavo_sauavga(obj%asum, obj%bsum, &
     &                  obj%absumr, obj%absumi,  obj%wts, &
     &                  obj%t(obj%kta)%ptr, obj%t(obj%ktaq)%ptr, &
     &                  obj%t(obj%ktb)%ptr, obj%t(obj%ktbq)%ptr, obj%wfun, &
     &                  obj%rcax, obj%rcbx, obj%ns, obj%fstu, obj%lstu, &
     &                  obj%ntwin, obj%factor, obj%corr_opt, obj%icdpn, &
     &                  obj%dbug_opt, obj%ipr)
                  endif
               endif
            endif

!DBG            if (obj%dbug_opt .gt. 0) err = mmchecka()

!           All the statistics are now available for CDP 'outtr'.
!           Make the following conversions:                     
!              t(ktsa) <-- standard deviation (A).
!              t(ktsb) <-- standard deviation (B).
!              t(ktrc) <-- real part of correlation coeficient. 
!              t(ktic) <-- imag part of correlation coefficient.
            if (obj%outtr .ge. 0 .and. obj%avg_flag) then
               call ppavo_saucora(obj%asum, obj%bsum, &
     &                 obj%absumr, obj%absumi,  obj%wts, &
     &                 obj%t(obj%ktsa)%ptr, obj%t(obj%ktsb)%ptr, &
     &                 obj%t(obj%ktrc)%ptr, obj%t(obj%ktic)%ptr, &
     &                 obj%rtempz, obj%ns, obj%fsa, obj%lsa, &
     &                 obj%nsmth, obj%cwin_vsh, obj%live)

!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Read the 'A' and 'B' traces, and their hilbert transforms:
!                 t(kta)   <-- real part of `A'.
!                 t(ktaq)  <-- imag part of `A'.
!                 t(ktb)   <-- real part of `B'.
!                 t(ktbq)  <-- imag part of `B'.
               obj%filep1 = obj%mxocode*obj%outtr + obj%kta
               iloc = 12
               if (obj%dbug_opt .ge. 2) then
                  write (obj%ipr, 8800) obj%filep1,iloc
               end if
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%kta)%ptr, obj%h(obj%kta)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
               obj%filep1 = obj%mxocode*obj%outtr + obj%ktaq
               iloc = 13
               if (obj%dbug_opt .ge. 2) then
                  write (obj%ipr, 8800) obj%filep1,iloc
               end if
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%ktaq)%ptr, obj%h(obj%ktaq)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
               obj%filep1 = obj%mxocode*obj%outtr + obj%ktb
               iloc = 14
               if (obj%dbug_opt .ge. 2) then
                  write (obj%ipr, 8800) obj%filep1,iloc
               end if
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%ktb)%ptr, obj%h(obj%ktb)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
               obj%filep1 = obj%mxocode*obj%outtr + obj%ktbq
               iloc = 15
               if (obj%dbug_opt .ge. 2) then
                  write (obj%ipr, 8800) obj%filep1,iloc
               end if
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%ktbq)%ptr, obj%h(obj%ktbq)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)

!              t(ktdv) <-- rms velocity trace (or 1's if none).
               obj%filep1 = obj%mxocode*obj%outtr + obj%ktdv
               iloc = 16
               if (obj%dbug_opt .ge. 2) write (obj%ipr, 8800) obj%filep1,iloc
               call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%t(obj%ktdv)%ptr, obj%h(obj%ktdv)%ptr, &
     &            obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)
               obj%ticd = obj%h(obj%ktdv)%ptr(ppavo_trc_typez)

               if (obj%ticd .ne. obj%jtvel) then
                  call ppavo_vfill(1.0, obj%t(obj%ktdv)%ptr, 1, obj%ns)
                  call ppavo_vmove(obj%h(obj%kta)%ptr, 1, &
     &               obj%h(obj%ktdv)%ptr, 1, obj%thl)
               endif

!              hd <-- a temporary header (copied from kta).
!              Retrieve the current trace's CDP number and killed status.
               call ppavo_vmove(obj%h(obj%kta)%ptr, 1, hd, 1, obj%thl)

               hd(ppavo_trc_foldz) = obj%maxvel
               obj%ticd  = hd(ppavo_trc_typez)
               obj%icdpn = hd(ppavo_cdpz)

               if (ppavo_iline_noz .eq. ppavo_nullpz) then
                  obj%lineno = 0
               else
                  obj%lineno = hd(ppavo_iline_noz)
               endif
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Update statistics headers to reflect new type of data present.
               do ii = obj%ktsa, obj%ktic
                  hd(ppavo_trc_typez) = jtab(ii)

                  if (.not. obj%live) then
                     hd(ppavo_trc_typez) = obj%jtkill
                  end if

                  call ppavo_vmove(hd, 1, obj%h(ii)%ptr, 1, obj%thl)

!                 Write these statistics to the work file,
!                 before we butcher them up.
                  obj%sptr => obj%t(ii)%ptr(obj%fss:)
                  call ppavo_maxmgv(obj%sptr, 1, obj%amxtt, ipos, obj%nss)
                  if (obj%amxtt .gt. obj%limits(ii)) then
                     obj%limits(ii) = obj%amxtt
                  end if
                  obj%filep1 = obj%mxocode*obj%outtr + ii
                  iloc = 17
                  if (obj%dbug_opt .ge. 2) write (obj%ipr, 8820) obj%filep1,iloc
                  call ppavo_uhci_write(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd,&
     &               obj%filep1, obj%t(ii)%ptr, obj%h(ii)%ptr, obj%thl, obj%ns,&
     &               obj%ddp1hd, obj%ddp1tr, ioerr)
520            end do

!              Set CDP for which to retrieve lamda parameter.
               dx(1) = hd(ppavo_cdpz)

!              Do the automatic overburden correction

!              Get the wavelet center frequency.
!              If not present, use 30 hz
               obj%wfreq = 30.0
               if (obj%wfloc .ne. 0) obj%wfreq = obj%h(obj%ktb)%ptr(obj%wfloc)
               if (obj%ctr_freq  .ne. 0) obj%wfreq = obj%ctr_freq

!              If a constant overburden override is specified, replicate
!              this override into the calibration arrays.
               if (obj%ccoef_re .lt. 1.0) then
                  call ppavo_vfill(obj%ccoef_re, obj%calibr, 1, obj%ns)
                  call ppavo_vfill(obj%ccoef_im, obj%calibi, 1, obj%ns)
                  if (obj%obdn_opt .eq. 3 .or. obj%obdn_opt .eq. 1) then
                     obj%calib = .true.
                  else if (obj%obdn_opt .eq. 2) then
                     obj%calia = .true.
                  endif
               endif

               if (obj%atrm_pow .gt. 0.0) then
                  call ppavo_vfill(obj%atrm_pow, obj%caliba, 1, obj%ns)
                  obj%cala = .true.
               endif

               if (obj%btrm_pow .gt. 0.0) then
                  call ppavo_vfill(obj%btrm_pow, obj%calibb, 1, obj%ns)
                  obj%calb = .true.
               endif
!DBG              if (obj%dbug_opt .gt. 0) err = mmchecka()

!              If we are to do automatic overburden correction,
!              then do the following:   
!                (rcax, rcbx) <-- k* = cross-term leakage factor from a-->b; 
!                dvv          <-- (-ktic) * ktsb / ktsa                      
!                ktsb         <-- sqrt{<|b + (k * a)|**2> }
               if (obj%calib) then
                  call ppavo_sauovb(obj%t(obj%ktsa)%ptr, obj%t(obj%ktsb)%ptr,&
     &                 obj%t(obj%ktrc)%ptr, obj%t(obj%ktic)%ptr, obj%dvv,&
     &                 obj%rcax, obj%rcbx, obj%rtempz,&
     &                 obj%calibr, obj%calibi, obj%ns)
                  call ppavo_vmove(obj%rtempz, 1, &
     &                             obj%t(obj%ktsb)%ptr, 1, obj%ns)
!DBG                  if (obj%dbug_opt .gt. 0) err = mmchecka()

!                 Compute new velocity functions, unless in 'AVO' mode.
                  if (obj%mode_opt .ne. 1) then

                     call ppavo_sanewv(obj%dvv, obj%t(obj%ktdv)%ptr, obj%ns,&
     &                   obj%bulk1, obj%fsuhci, obj%wfreq, obj%q_factor,&
     &                   obj%fsa, obj%lsa, obj%ivel_min, obj%ivel_max, &
     &                   obj%ivel_dmn, obj%ivel_dmx, &
     &                   obj%svel_dmn, obj%svel_dmx,&
     &                   obj%svel_min, obj%svel_max, &
     &                   obj%vcut_tim, obj%vcut_pow,&
     &                   obj%err, obj%reft)
                     if (obj%err .gt. 0 .and. obj%err .le. maxvmsg) then
                        if(obj%dbug_opt .ge. 0) then
                           write (obj%ipr, 9150) obj%icdpn, velmsg(obj%err), &
     &                        obj%reft
                        endif
                        obj%kpwarn = 1
                     endif
                     if (obj%smth_vel .gt. 0) then
                        ii = obj%smth_vel / obj%sampratz
                        call ppavo_samodv(obj%t(obj%ktdv)%ptr,&
     &                     obj%t(obj%ktitt)%ptr, obj%ns, ii)
                     endif

                  endif

!                 Make the corrections to the AVO slope.
!                    temp <--  re{(kta,ktaq) * (rcax,rcbx)*} 
!                    ktb  <--  ktb  + temp                   
!                    temp <--  im{(kta,ktaq) * (rcax,rcbx)*} 
!                    ktbq <--  ktbq + temp                  
                  call ppavo_nacma(obj%t(obj%ktb)%ptr, obj%t(obj%ktbq)%ptr,&
     &                 obj%t(obj%kta)%ptr, obj%t(obj%ktaq)%ptr,&
     &                 obj%rcax, obj%rcbx, obj%ns)

!DBG                  if (obj%dbug_opt .gt. 0) err = mmchecka()

!                 Copy revised correlation coefficients.
                  call ppavo_vmove(obj%calibr, 1, &
     &                             obj%t(obj%ktrc)%ptr, 1, obj%ns)
                  call ppavo_vmove(obj%calibi, 1, &
     &                             obj%t(obj%ktic)%ptr, 1, obj%ns)

               endif

!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Correlation matching zero-offset response (A).

!              If we are to do automatic correlation matching of 'A', then:
!                (rcax, rcbx) <-- k* = cross-term leakage factor from b-->a; 
!                ktsa         <-- sqrt{<|a + (k * b)|**2>}
               if (obj%calia) then
                  call ppavo_sauova(obj%t(obj%ktsa)%ptr, obj%t(obj%ktsb)%ptr,&
     &                  obj%t(obj%ktrc)%ptr, obj%t(obj%ktic)%ptr,&
     &                  obj%rcax, obj%rcbx, obj%rtempz,&
     &                  obj%calibr, obj%dvv, obj%ns)
                  call ppavo_vmove(obj%rtempz, 1, &
     &                             obj%t(obj%ktsa)%ptr, 1, obj%ns)

!                 Make the corrections to the zero-offset response:      
!                 (kta, ktaq) <-- (kta, ktaq) + (ktb, ktbq)*(rcax, rcbx)*
                  call ppavo_nacma(obj%t(obj%kta)%ptr, obj%t(obj%ktaq)%ptr, &
     &                   obj%t(obj%ktb)%ptr, obj%t(obj%ktbq)%ptr, &
     &                   obj%rcax, obj%rcbx, obj%ns)

!                 Copy the revised correlation coefficients
                  call ppavo_vmove(obj%calibr, 1, &
     &                             obj%t(obj%ktrc)%ptr, 1, obj%ns)
                  call ppavo_vmove(obj%calibi, 1, &
     &                             obj%t(obj%ktic)%ptr, 1, obj%ns)
               endif

!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Scale the zero-offset traces.
               if (obj%cala) then
                  call ppavo_vdivz(obj%caliba, 1, obj%t(obj%ktsa)%ptr, 1,&
     &               0.0, obj%t(obj%ktsa)%ptr, 1, obj%ns)
                  call ppavo_vmul(obj%t(obj%kta)%ptr, 1, &
     &                            obj%t(obj%ktsa)%ptr, 1,&
     &               obj%t(obj%kta)%ptr, 1, obj%ns)
                  call ppavo_vmul(obj%t(obj%ktaq)%ptr, 1, &
     &                            obj%t(obj%ktsa)%ptr, 1,&
     &               obj%t(obj%ktaq)%ptr, 1, obj%ns)
                  call ppavo_vmove(obj%caliba, 1, obj%t(obj%ktsa)%ptr,&
     &               1, obj%ns)
               endif
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Additional corrections to the AVO slope trace.

!              Scale the slope traces.
               if (obj%calb) then
                  call ppavo_vdivz(obj%calibb, 1, obj%t(obj%ktsb)%ptr, 1,&
     &               0.0, obj%t(obj%ktsb)%ptr,  1, obj%ns)
                  call ppavo_vmul(obj%t(obj%ktb)%ptr,  1, &
     &               obj%t(obj%ktsb)%ptr, 1,&
     &               obj%t(obj%ktb)%ptr, 1, obj%ns)
                  call ppavo_vmul(obj%t(obj%ktbq)%ptr, 1, &
     &                            obj%t(obj%ktsb)%ptr, 1,&
     &               obj%t(obj%ktbq)%ptr, 1, obj%ns)
                  call ppavo_vmove(obj%calibb,  1, obj%t(obj%ktsb)%ptr,&
     &               1, obj%ns)
               endif
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Perform additional mixing between 'A' and 'B', if desired.
               if (obj%amix_fct .ne. 0.0) then
                  call ppavo_vsmul(obj%t(obj%ktb)%ptr, 1, obj%bmix_fct,&
     &               obj%t(obj%ktb)%ptr, 1, obj%ns)
                  call ppavo_vsmul(obj%t(obj%ktbq)%ptr, 1, obj%bmix_fct,&
     &               obj%t(obj%ktbq)%ptr, 1, obj%ns)
                  call ppavo_vsma(obj%t(obj%kta)%ptr, 1, obj%amix_fct,&
     &               obj%t(obj%ktb)%ptr, 1, obj%t(obj%ktb)%ptr, 1, obj%ns)
                  call ppavo_vsma(obj%t(obj%ktaq)%ptr, 1, obj%amix_fct,&
     &               obj%t(obj%ktbq)%ptr, 1, obj%t(obj%ktbq)%ptr, 1, obj%ns)
               endif
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Convert stacking velocities to interval velocities.
               call ppavo_cnvitt(obj%t(obj%ktdv)%ptr, obj%t(obj%ktitt)%ptr,&
     &            obj%ns, obj%isi, 0.0)
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Dump the velocity cards, if requested.
               if (obj%dump_opt .and. obj%icdpn .le. obj%ecdp_dmp .and. &
     &            obj%icdpn .ge. obj%scdp_dmp .and. &
     &            mod(obj%icdpn-obj%scdp_dmp, obj%dcdp_dmp) .eq. 0) then
                  obj%dunit = obj%kprno + sdunit
                  obj%fst_dump = min0(1000*obj%stim_dmp/obj%isi + 1, obj%ns)
                  obj%lst_dump = min0(1000*obj%etim_dmp/obj%isi + 1, obj%ns)
                  obj%nst_dump = 1000 * (obj%dtim_dmp - 1) / obj%isi + 1
                  call ppavo_dumpvl(obj%dunit, obj%t(obj%ktdv)%ptr,&
     &               obj%ns, obj%fst_dump, obj%lst_dump, obj%nst_dump,&
     &               obj%icdpn, obj%isi, obj%bulk1)
               endif

!              Copy Smith-Gidlow ('ktb') to 'ktbno', and 'kta' to 'ktano'.
               call ppavo_vmove(obj%h(obj%ktb)%ptr, 1, &
                  obj%h(obj%ktbno)%ptr, 1, obj%thl)
               call ppavo_vmove(obj%t(obj%ktb)%ptr, 1, &
                  obj%t(obj%ktbno)%ptr, 1, obj%ns)
               call ppavo_vmove(obj%h(obj%kta)%ptr, 1, &
                  obj%h(obj%ktano)%ptr, 1, obj%thl)
               call ppavo_vmove(obj%t(obj%kta)%ptr, 1, &
                  obj%t(obj%ktano)%ptr, 1, obj%ns)

!              Compute all the requested HCI's 

!              Compute the exponentiation factor, `p', into the `temp' array.
               obj%fs1 = obj%fsa
               call ppavo_vzero(obj%rtempz, 1, obj%ns)
               if (obj%alph_fct .ne. 0.0) then
               call ppavo_sauhce(obj%t(obj%ktrc)%ptr(obj%fs1:), &
     &            obj%rtempz(obj%fs1:), obj%nsa, obj%alph_fct, obj%ffth_dmn)
               end if

!              Place t(ktrvi)  <-- im{ab*}.
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()
               call ppavo_vzero(obj%t(obj%ktrvi)%ptr, 1, obj%ns)

               call ppavo_vzero(obj%t(obj%kthci)%ptr, 1, obj%ns)

               if (obj%ticd .ne. obj%jtkill) then

               call ppavo_vmove(obj%t(obj%ktb)%ptr, 1, tr, 1, obj%ns)

               call ppavo_saarvi(obj%t(obj%kta)%ptr(obj%fs1:), &
     &            obj%t(obj%ktaq)%ptr(obj%fs1:), &
     &            obj%t(obj%ktb)%ptr(obj%fs1:), &
     &            obj%t(obj%ktbq)%ptr(obj%fs1:), &
     &            obj%t(obj%ktsa)%ptr(obj%fs1:), &
     &            obj%t(obj%ktsb)%ptr(obj%fs1:), &
     &            obj%t(obj%kthci)%ptr(obj%fs1:), &
     &            obj%t(obj%ktrvi)%ptr(obj%fs1:), &
     &            obj%rcax(obj%fs1:), obj%nsa, obj%scal_opt)
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              Compute the optimal hydrocarbon indicator.
               call ppavo_vmove(obj%h(obj%kta)%ptr, 1, &
     &            obj%h(obj%kthci)%ptr, 1, obj%thl)
               call ppavo_saahci(obj%t(obj%kta)%ptr(obj%fs1:), &
     &            obj%t(obj%ktaq)%ptr(obj%fs1:), &
     &            obj%t(obj%ktb)%ptr(obj%fs1:), &
     &            obj%t(obj%ktbq)%ptr(obj%fs1:), &
     &            obj%rtempz(obj%fs1:), &
     &            obj%t(obj%kthci)%ptr(obj%fs1:), &
     &            obj%rcax(obj%fs1:), &
     &            obj%rcbx(obj%fs1:), &
     &            obj%t(obj%ktsa)%ptr(obj%fs1:), &
     &            obj%t(obj%ktsb)%ptr(obj%fs1:), &
     &            obj%alph_fct, obj%nsa, obj%scal_opt)
               endif
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka() 

!              Dispose of the HCIs in the proper way.

!              Update trace headers to reflect the new type of data present.
               do 550 ii = obj%kthci, obj%mxocode
                  hd(ppavo_trc_typez) = jtab(ii)

                  if (obj%ticd .eq. obj%jtkill) then
                     hd(ppavo_trc_typez) = obj%ticd
                  end if
                  call ppavo_vmove(hd, 1, obj%h(ii)%ptr, 1, obj%thl)

!                 Write these traces to the work file
                  obj%filep1 = obj%mxocode*obj%outtr + ii
                  iloc = 18
                  if (obj%dbug_opt .ge. 2) then
                     write (obj%ipr, 8820) obj%filep1,iloc
                  end if
                  call ppavo_uhci_write(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd,&
     &               obj%filep1, obj%t(ii)%ptr, obj%h(ii)%ptr, obj%thl, obj%ns,&
     &               obj%ddp1hd, obj%ddp1tr, ioerr)

!                 Update their statistics. Kill seemingly dead traces.
                  obj%sptr => obj%t(ii)%ptr
                  if (obj%icdpn .ge. obj%scdp_scl .and. &
     &                obj%icdpn .le. obj%ecdp_scl) then
                     obj%icode = ii
                     call ppavo_saustc(obj%sptr, obj%fss, obj%lss,&
     &                  obj%stats, obj%stawdt, obj%icode, obj%live)
                     call ppavo_maxmgv(obj%sptr(obj%fss:), 1,&
     &                  obj%amxtt, ipos, obj%nss)
                     if (obj%amxtt .gt. obj%limits(ii)) then
                        obj%limits(ii) = obj%amxtt
                     end if
                  endif

550            continue
!DBG               if (obj%dbug_opt .gt. 0) err = mmchecka()

!              For the 'pas' option, HCIs within the output range get
!              passed now. if no HCIs are specified, simply pass the
!              prestack gather.
               if (obj%mplx_opt .eq. 2 .and. .not. obj%linend) then
                  obj%jcdpb    = obj%jcdpb  + 1
                  obj%outtr    = obj%outtr  + 1
                  obj%nccor    = obj%nccor  + 1
                  if (obj%icdpn .ge. obj%nc1 .and. obj%icdpn .le. obj%nc2) then
                     if (obj%ntout .gt. 0) then
                        gflag = 820
!!!!                        goto 820
                     end if
                     if(gflag .ne. 0) goto 551
                     obj%pas2 = 1
551                  continue
                  else
                     obj%pas2 = obj%lcmxfd + 1
                     goto 120
                  endif
               endif

            endif
            if(gflag .ne. 0) goto 552

!           Bump the output pointer. Branch ahead if we're finishing up.
            obj%outtr = obj%outtr + 1

!!!            if (obj%linend) goto 600
            if (obj%linend) then
               gflag = 600
!!!!               goto 600
            end if
            if(gflag .ne. 0) goto 552

!           we're now done with the current 'B' trace
!           it's ok to bump the 'B' trace count:     
            obj%jcdpb = obj%jcdpb + 1
            obj%nccor = obj%nccor + 1
            obj%lasticd = obj%jtb

552         continue

!        Now handle all miscellaneous traces that come in.

!        Reserve a place for killed CDPs in the scratch file.
         else if (obj%ticd .eq. obj%jtkill) then
            obj%numvel = 0
            call ppavo_vzero(obj%newtr, 1, obj%ns)
            if (obj%fndx .ge. obj%ncdp_max) then
               if (obj%kpwarn .eq. 0) write (obj%ipr, 9200) obj%ncdp_max
               obj%kpwarn = -1
               obj%kpmitf = 0
               obj%intcde = 0
               goto 1000
            endif
            obj%icdps(obj%fndx+1) = -obj%icdpn
            obj%fcdps(obj%fndx+1) = -obj%icdpn
            if (obj%fndx .eq. obj%jcdpa) then
               do  560 ii = obj%kta, obj%ktbq
                  obj%filep1 = obj%mxocode*obj%jcdpa + ii
                  iloc = 19
                  if (obj%dbug_opt .ge. 2) then
                     write (obj%ipr, 8820) obj%filep1,iloc
                  end if
                  call ppavo_uhci_write(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &               obj%filep1, obj%newtr, obj%newhd, obj%thl, obj%ns, &
     &               obj%ddp1hd, obj%ddp1tr, ioerr)
560            continue
            obj%jcdpa = obj%jcdpa + 1
            endif

!        Calibration traces get saved into reserved storage
         else if (obj%ticd .eq. obj%jtrc .and. obj%ccoef_re .ge. 1.0) then
            call ppavo_vmove(obj%newtr, 1, obj%calibr, 1, obj%ns)
            obj%calib = .true.

         else if (obj%ticd .eq. obj%jtic .and. obj%ccoef_re .ge. 1.0) then
            call ppavo_vmove(obj%newtr, 1, obj%calibi, 1, obj%ns)
            obj%calib = .true.

         else if (obj%ticd .eq. obj%jtsa) then
            call ppavo_vmove(obj%newtr, 1, obj%caliba, 1, obj%ns)
            obj%cala = .true.

         else if (obj%ticd .eq. obj%jtsb) then
            call ppavo_vmove(obj%newtr, 1, obj%calibb, 1, obj%ns)
            obj%calb = .true.

!        Velocity traces get copied to scratch file #1. Let earlier
!        velocities be overwritten by latter ones. Since velocity traces
!        can only come from avel option 'p', interpret the next killed
!        trace to be an 'A' trace substitute.
         else if (obj%ticd .eq. obj%jtvel) then
            obj%numvel = obj%numvel + 1
            if (obj%numvel .gt. obj%maxvel) obj%maxvel = obj%numvel
            if (obj%numvel .eq. 1) then
               obj%filep1 = obj%mxocode*obj%jcdpa + obj%ktdv  
               if (obj%jcdpa .ge. obj%ncdp_max) then
                  if (obj%kpwarn .eq. 0) write (obj%ipr, 9200) obj%ncdp_max
                  obj%kpwarn = -1
                  obj%kpmitf = 0
                  obj%intcde = 0
                  goto 1000
               endif
               obj%icdps(obj%jcdpa+1) = -obj%icdpn
               obj%fcdps(obj%jcdpa+1) = -obj%icdpn
               iloc = 20
               if (obj%dbug_opt .ge. 2) then
                  write (obj%ipr, 8820) obj%filep1, iloc
               end if
               call ppavo_uhci_write(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &            obj%filep1, obj%newtr, obj%newhd, obj%thl, obj%ns, &
     &            obj%ddp1hd, obj%ddp1tr, ioerr)
            endif

!           Save all velocity traces with data.
            obj%fndx  = mod(obj%fndx, obj%cwin_cdp/2+1)
            obj%fold  = obj%folds(obj%fndx+1)
            obj%fold  = obj%fold + 1
            obj%mxfld = max0(obj%mxfld, obj%fold)
            obj%folds(obj%fndx+1) = obj%fold
            if (obj%fold .le. obj%lcmxfd .and. obj%mplx_opt .eq. 2) then
            obj%filep2 = obj%fndx*(obj%lcmxfd+1) + obj%fold
            iloc = 21
            if (obj%dbug_opt .ge. 2) then
               write (obj%ipr, 8830) obj%filep2,iloc
            end if
            call ppavo_uhci_write(obj%ppavo_obj, obj%kpwks2, obj%kpwkd2, &
     &         obj%filep2, obj%newtr, obj%newhd, obj%thl, obj%ns, &
     &         obj%ddp1hd, obj%ddp1tr, ioerr)
            endif

!        Live traces goto scratch file #2 for obj%mplx_opt = 2.
         else if (obj%ticd .eq. obj%jtlive) then

            obj%numvel = 0
            obj%fndx   = mod(obj%fndx, obj%cwin_cdp/2+1)
            obj%fold   = obj%folds(obj%fndx+1)
            obj%fold   = obj%fold + 1
            obj%mxfld  = max0(obj%mxfld, obj%fold)
            obj%folds(obj%fndx+1) = obj%fold

            if (obj%fold .le. obj%lcmxfd .and. obj%mplx_opt .eq. 2) then
               obj%filep2 = obj%fndx*(obj%lcmxfd+1) + obj%fold
               iloc = 22
               if (obj%dbug_opt .ge. 2) then
                  write (obj%ipr, 8830) obj%filep2,iloc
               end if
               call ppavo_uhci_write(obj%ppavo_obj, obj%kpwks2, obj%kpwkd2, &
     &            obj%filep2, obj%newtr, obj%newhd, obj%thl, obj%ns, &
     &            obj%ddp1hd, obj%ddp1tr, ioerr)
            endif

!        Anything other than a zero-offset, slope, velocity, live,
!        killed, or calibration trace gets ignored. 
         endif

!     No more input traces; finish trailing edge of the statistics.
!!!!      else if (((obj%pcdp .le. obj%jcdpb) .and. (obj%mplx_opt .ne. 1)) .or.&
!!!!     &   ((obj%passed .le. obj%ntout) .and. (obj%mplx_opt .eq. 1))) then
      else if ( ((((obj%pcdp .le. obj%jcdpb) .and. (obj%mplx_opt .ne. 1)) .or.&
     &   ((obj%passed .le. obj%ntout) .and. (obj%mplx_opt .eq. 1))) .and.&
     &   gflag .ne. 900) .or. gflag .eq. 820 .or. gflag .eq. 600 )then

         if(gflag .eq. 600) then
            gflag = 0
            goto 600
         end if
         if(gflag .eq. 820) then
            gflag = 0
            goto 820
         end if

!        Recover the trailing 'A' & 'B' traces and their weighting
!        functions. output the remaining statistics and indicators.
600      continue

         if (obj%outtr .lt. obj%jcdpb) then
            obj%trailtr = obj%outtr - obj%cwin_cdp/2 - 1
            gflag = 300
!!!!            goto 300
         endif
         if(gflag .ne. 0) goto 821

!        Everything has been computed. Begin final output scaling
!        the correlation coefficients and velocities are exempt.
         if (.not. obj%hflag .and. obj%mplx_opt.ne.2) then

            do 700 ii = 1, obj%ntout
               obj%itrn  = obj%oticd(ii)
               obj%icode = ctab(obj%itrn)
               if (obj%itrn .eq. obj%jtrc .or. obj%itrn .eq. obj%jtic .or.&
     &             obj%itrn .eq. obj%jtdv .or. obj%itrn .eq. obj%jtitt) goto 700

!              HCI and RVI traces are subject to histogram
!              scaling, unless they are already normalized.
               if (obj%itrn .eq. obj%jthci .or. obj%itrn .eq. obj%jtrvi) then
                  if (obj%scal_fct .gt. 0.0) then
                     if (obj%scalef(obj%icode) .eq. 1.0) then
                        obj%scalef(obj%icode) = obj%limits(obj%icode)
                        call ppavo_sahgram(obj%ppavo_obj, &
     &                     obj%itempz, obj%rtempz, &
     &                     obj%hsize, obj%kpwrks, obj%kpwrkd, obj%jcdpb, &
     &                     obj%mxocode,  obj%icode, hd, tr, obj%thl, obj%ns, &
     &                     obj%scal_fct,  obj%scalef(obj%icode), obj%maxbin, &
     &                     obj%ddp1hd, obj%ddp1tr, ioerr)
                        if (obj%dbug_opt .ge. 0) then
                           write (obj%ipr, 9270) ctext(obj%icode), obj%maxbin, &
     &                        obj%scalef(obj%icode)
                        end if
                        if (obj%maxbin .lt. 0 .or. &
     &                      obj%scalef(obj%icode).le.0) then
                           obj%scalef(obj%icode) = 1.0
                        end if
                     endif
                     obj%stats(obj%stawdt, obj%icode-1) = &
     &                  abs(1d0*sclprm/obj%scalef(obj%icode))
!DBG                     if (obj%dbug_opt .gt. 0) err = mmchecka()
                  endif

!              All others get rms scaling.
               else
                  call ppavo_rmsscl(obj%stats(1:,(obj%icode-1)), &
     &               obj%stawdt, obj%scal_rms)
               endif
700         continue

!           Prepare to recalculate output statistics one more time
            call ppavo_sauclr('B', obj%stats, obj%stawdt, obj%mxocode, 0.0, 0.0)
         endif
         obj%hflag = .true.

!        Begin passing the first of the selected output traces.
!WARNING
!        Check the next trace, to see if it's one to process.
800      obj%icdpn = abs(obj%fcdps(obj%pcdp))

!        If not between 'nc1' and 'nc2', skip to one that is!        
!        obj%mplx_opt = 0 for multiplexed (default); = 1 for demultiplexed.
!        obj%mplx_opt = 2 for pass.
         if (obj%icdpn .lt. obj%nc1 .or. obj%icdpn .gt. obj%nc2) then
            obj%pcdp = obj%pcdp + 1
            if (obj%pcdp .le. obj%jcdpb) goto 800
            if (obj%mplx_opt .ne. 1) goto 1000
            obj%pcdp = 1
            obj%passed = obj%passed + 1
            if (obj%passed .gt. obj%ntout) goto 1000
            goto 800
         endif

!        Read the appropriate trace from the scratch file
!           intcde = passed                      ! index of ext code
!           itrn   = oticd(intcde)               ! external code
!           icode  = ctab(itrn)                  ! internal code
!           filep1 = mxocode*(pcdp-1) + icode    ! file pointer
820      obj%intcde = obj%passed                         
         obj%itrn   = obj%oticd(obj%intcde)           
         obj%icode  = ctab(obj%itrn)              
         obj%filep1 = obj%mxocode*(obj%pcdp-1) + obj%icode
         iloc = 23
         if (obj%dbug_opt .ge. 2) then
            write (obj%ipr, 8800) obj%filep1,iloc
         end if

         call ppavo_uhci_read(obj%ppavo_obj, obj%kpwrks, obj%kpwrkd, &
     &      obj%filep1, tr, hd, obj%thl, obj%ns, obj%ddp1hd, obj%ddp1tr, ioerr)

!        Scale the trace and set some headers.
         scalar = obj%stats(obj%stawdt,obj%icode)
         call ppavo_vsmul(tr, 1, scalar, tr, 1, obj%ns)

!WARNING         hd(ppavo_iffidz) = obj%passed
!DBG         if (obj%dbug_opt .gt. 0) err = mmchecka()

!        Compute the output statistics of this trace.
         if (obj%icdpn .ge. obj%scdp_scl .and. obj%icdpn .le. obj%ecdp_scl) then
            call ppavo_saustc(tr, obj%fss, obj%lss, obj%stats,&
     &         obj%stawdt, obj%icode, obj%live)
         end if

!        Bump 'pcdp' and 'passed' when necessary.
!        Indicate that we wish to pass a trace and
!        that we have another one to follow.    
         obj%kpmotf = 1
         obj%kprtf  = 1
         obj%nproc  = obj%nproc + 1
         hd(ppavo_end_ensz)   = ppavo_nlastpz
         hd(ppavo_end_jobz)   = ppavo_nlastpz
         hd(ppavo_lseg_endz)  = ppavo_nlastpz
         hd(ppavo_repeatz)    = obj%passed
         hd(ppavo_sort_runz)  = 0
         if (obj%mplx_opt .ne. 1) then
            hd(ppavo_seq_noz) = obj%passed
            obj%passed = obj%passed + 1
            if (obj%passed .gt. obj%ntout) then
               if (obj%mplx_opt .eq. 2) then
                  obj%fndx = mod(obj%pcdp-1, obj%cwin_cdp/2+1)
                  obj%fold = obj%folds(obj%fndx+1) + 1
                  if (obj%fold .le. 1) hd(ppavo_end_ensz) = ppavo_lasttrpz
                  obj%pas2 = 1
               else
                  hd(ppavo_end_ensz) = ppavo_lasttrpz
                  obj%pcdp = obj%pcdp + 1
               endif
            obj%passed = 1
            endif
         else
            hd(ppavo_seq_noz) = obj%pcdp
            hd(ppavo_end_ensz) = ppavo_lasttrpz
            if (obj%pcdp .eq. obj%lensb+1) then
               hd(ppavo_sort_runz) = ppavo_lasttrpz
               hd(ppavo_lseg_endz) = ppavo_lasttrpz
            endif
            obj%pcdp = obj%pcdp + 1
            if (obj%pcdp .gt. obj%jcdpb) then
               obj%pcdp = 1
               obj%passed = obj%passed + 1
            endif
         endif

821      continue

!     Complete the processing of this 3d line.

!     We're all finished passing the traces. Ready for clean up.
      else

         if(gflag .eq. 900) then
            gflag = 0
            goto 900
         end if

!        Output the nmoc cards to the dump file, if active.
         vfu   = cvfu
         obj%dunit = obj%kprno + sdunit
         if (obj%dump_opt) then
            do 880 ii=1, obj%jcdpb
               obj%icdpn = abs(obj%fcdps(ii))
               if (obj%icdpn .ge. obj%scdp_dmp .and. &
     &            obj%icdpn .le. obj%ecdp_dmp .and. &
     &            mod(obj%icdpn-obj%scdp_dmp, obj%dcdp_dmp) .eq. 0) then
                  write (obj%dunit, 8040) obj%icdpn, obj%icdpn,&
     &               obj%icdpn, vfu
                  vfu = ' '
               endif
880         continue
         endif

!        Print a list of CDPs processed.
900      continue
         if (obj%dbug_opt .ge. 0) then
            write (obj%ipr, 9075) obj%lineno,(abs(obj%icdps(ii)),&
     &         ppavo_sauskp(obj%icdps(ii), obj%nc1, obj%nc2), &
     &         ii = 1, obj%jcdpb)
         end if

!        Compute and print the global statistics.
         if (obj%dbug_opt .ge. 0) then 
         call ppavo_saustp(obj%stats, obj%stawdt, obj%ntout, obj%oticd,&
     &      ctab, ctext, obj%mintid, obj%maxtid, obj%mxocode, obj%ipr)
         end if

!        Now ready for more input.
!        Go on to the next line, if more data exits.     
         obj%kpmotf = 0
         if (obj%kpmitf .eq. 1) then
            if (ppavo_iline_noz .eq. ppavo_nullpz) then
               obj%lineno = 0
            else
               obj%lineno = obj%newhd(ppavo_iline_noz)
            endif
            obj%icdpn = hd(ppavo_cdpz)
            obj%cpyflg = .false.
            obj%linend = .false.
            gflag = 100
!!!!            goto 100
         endif
         if(gflag .ne. 0) goto 901
         obj%kpmotf = 0
901      continue
      endif

      if(gflag .eq. 100) goto 99
      if(gflag .ne. 0) goto 199

!     The usual escape hatch.

1000  continue

!     General return epilog.
      if (obj%dbug_opt .ge. 3) then
         write (obj%ipr,9800) obj%kprtf, obj%kpdrtf, obj%kpmotf,&
     &                        obj%kpmitf, obj%linend
      end if

!     Set the appropriate exit mode.
      if (obj%kprtf .lt. 0 .or. obj%kprtf .ge. 2) then
         write (obj%print_lun, *) 'B_UHCI: Exit code muckup.'
         ntr = FATAL_ERROR
         return
      end if

      if (obj%kprtf .eq. 0) then
!        No trace is returned.
         if (obj%kpmitf .eq. 0) then
!           No more input traces, either.
            promode = 0
         else
!           Accept another input trace.
            promode = 1
         endif
      else
!        A trace is returned.
         if (obj%kpmotf .eq. 0) then
!           Accept a trace after this.
            promode = 2
         else
!           Don't accept a trace after this. 
            promode = 3
         endif
      iticd = hd(ppavo_trc_typez)

      endif

      return

!     Format statements.
8040  format('NMOC  D', T11, 3I5, T53, A3)
8800  format(' READING TRACE ', I5,' FROM SCRATCH FILE 1 AT ',I5,'.')
8810  format(' READING TRACE ', I5,' FROM SCRATCH FILE 2.AT ',I5,'.')
8820  format(' WRITING TRACE ', I5,'  TO  SCRATCH FILE 1.AT ',I5,'.')
8830  format(' WRITING TRACE ', I5,'  TO  SCRATCH FILE 2.AT ',I5,'.')
8850  format('    OUTPUT DATA ORDER IS:  ', A/)
8852  format('    OUTPUT DATA ORDER IS:  ', A, ';  MAXIMUM CDP FOLD = ',&
     &       I5, '.')
8900  format(' SUCCESSFULLY COMPLETED',&
     &       ', ', I4, ' CDPS CORRELATED; ', I4, ' CDPS PASSED.')
8910  format(' SUCCESSFULLY COMPLETED',&
     &       ', ', I4, ' CDPS CORRELATED.')
9050  format('0',54('*')/&
     &       ' ***         INPUT SEISMIC DATA MISSING             ***'/&
     &       ' ***       KPFCF = ',I6,', KPMITF = ',I6,'          ***'/&
     &       1X,54('*'))
9075  format(/' CDPS PROCESSED FOR LINE', I9, ':  (''K''=KILLED; ''S''',&
     &      '=SKIPPED (CORRELATED BUT NOT PASSED.)'/&
     &      10(I9,A1))
9110  format(/'*****************************************************'/&
     &       ' ***     WARNING:  ''B'' TRACES ARE IGNORED!        ***'/&
     &       ' ***          (DUE TO ''DBG'' CARD.)                ***'/&
     &       ' *****************************************************')
9120  format(/'*******************************************************'/&
     &       ' ***     WARNING:  MAXIMUM FOLD ENCOUNTERED:',I5,'   ***'/&
     &       ' ***       IS GREATER THAN LINE CARD VALUE:',I5,'    ***'/&
     &       ' *******************************************************'/&
     &       )
9130  format(5X, 'OVERBURDEN CORRECTION HAS BEEN APPLIED.')
9140  format(/'*******************************************************'/&
     &       ' ***  ERROR:  TRACE HEADER ''THNFF'' = ',I7,'        ***'/&
     &       ' *** USE THE ',A,' VERSION OF AVEL, OR LATER. ***'/&
     &       ' ******************************************************'/)
9150  format(/'  WARNING:  CDP ',I8,'''S INPUT ', A, ' AT ',F5.3,&
     &       ' SECONDS.')
9200  format(' *** WARNING:  NUMBER OF CDPS FOUND ',&
     &       'EXCEEDS THE MAXIMUM ALLOWED:  ',&
     &       I8,'.'/'    CHECK DATA CARD 1, DF8.')
9270  format(5X,'HISTOGRAM SCALING FOR ', A,' IS BASED UPON BIN #',&
     &       I4,':',1PG13.5)
9300  format(' *** WARNING:  MORE ''B'' TRACES THAN ',&
     &       '''A'' TRACES WERE FOUND:  ', I5)
9310  format(' *** PROGRAMMING ERROR:  YOU FORGOT TO INCREASE THE ',&
     &       'SIZES OF THE ''SCALEF'' ARRAYS TO ', I2,'!')
9400  format(/1X,55('*')/' *** '/ ' *** ERROR:  ',&
     &       'THE ',I5,'TH ''A'' TRACE FOUND ',&
     &       '(NUMBER ',I5,') DOES NOT MATCH ITS ',&
     &       'CORESPONDING ''B'' TRACE (NUMBER ',&
     &       I5,').'/' ***', T20, 'ZOFF=',L1,', SLOPE=',L1,&
     &       ', TICD=',I2 / ' *** '/1X, 55('*'))
9800  format(' EXIT CODES:  KPRTF = ', I5,', KPDRTF = ', I5,&
     &       ' KPMOTF = ', I5, ', KPMITF = ',&
     &       I5, ' LINEND=',L1)
9900  format('0',55('*')/&
     &       ' ***   MEMORY ERROR:  (#9900)' /&
     &       ' ***   NUMBER OF RESERVED WORDS (NOWDS):',&
     &        T50, I10/&
     &       ' ***   UNRESERVED WORDS AVAILABLE (KPNUSM):', T50, I10/&
     &       ' ***   UNRESERVED WORDS NEEDED (SIZEU):',  T50, I10/&
     &       ' ',55('*'))

      end subroutine buhci_work


!!------------------------- buhci_init_parms --------------------------------!!
!!------------------------- buhci_init_parms --------------------------------!!
!!------------------------- buhci_init_parms --------------------------------!!

      subroutine buhci_init_parms (obj)
      type(buhci_struct),intent(inout) :: obj              ! arguments

      obj%f_iter_cnt = .false.
      obj%f_iter_num = .false.
      obj%f_any_more = .false.
      obj%f_dbug_opt = .false.
      obj%f_scdp_num = .false.
      obj%f_ecdp_num = .false.
      obj%f_ncdp_max = .false.
      obj%f_stim_win = .false.
      obj%f_etim_win = .false.
      obj%f_cwin_tim = .false.
      obj%f_cwin_cdp = .false.
      obj%f_skip_opt = .false.
      obj%f_mplx_opt = .false.
      obj%f_corr_opt = .false.
      obj%f_cwin_vsh = .false.
      obj%f_scal_opt = .false.
      obj%f_obdn_opt = .false.
      obj%f_smth_vel = .false.
      obj%f_swin_opt = .false.
      obj%f_scdp_scl = .false.
      obj%f_ecdp_scl = .false.
      obj%f_stim_scl = .false.
      obj%f_etim_scl = .false.
      obj%f_con_velp = .false.
      obj%f_ivel_min = .false.
      obj%f_ivel_max = .false.
      obj%f_svel_min = .false.
      obj%f_svel_max = .false.
      obj%f_vcut_tim = .false.
      obj%f_vcut_pow = .false.
      obj%f_dump_opt = .false.
      obj%f_dtim_dmp = .false.
      obj%f_stim_dmp = .false.
      obj%f_etim_dmp = .false.
      obj%f_scdp_dmp = .false.
      obj%f_ecdp_dmp = .false.
      obj%f_same_dbg = .false.
      obj%f_scal_fct = .false.
      obj%f_scal_rms = .false.
      obj%f_angl_rot = .false.
      obj%f_ccoef_re = .false.
      obj%f_ccoef_im = .false.
      obj%f_atrm_pow = .false.
      obj%f_btrm_pow = .false.
      obj%f_q_factor = .false.
      obj%f_ctr_freq = .false.
      obj%f_amix_fct = .false.
      obj%f_bmix_fct = .false.
      obj%f_alph_fct = .false.
      obj%f_ffth_dmn = .false.
      obj%f_ivel_dmn = .false.
      obj%f_ivel_dmx = .false.
      obj%f_svel_dmn = .false.
      obj%f_svel_dmx = .false.
      obj%f_real_dbg = .false.
      obj%f_imag_dbg = .false.
      obj%f_mode_opt = .false.
      obj%f_otrc_opt = .false.
      obj%f_veldname = .false.

      end subroutine buhci_init_parms


!!-------------------------- buhci_get_iparm --------------------------------!!
!!-------------------------- buhci_get_iparm --------------------------------!!
!!-------------------------- buhci_get_iparm --------------------------------!!

      subroutine buhci_get_iparm (obj, name, parm)
      type(buhci_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      integer            ,intent(out)   :: parm

      select case (name)
      case ('iter_cnt')
         if(obj%f_iter_cnt) parm = obj%p_iter_cnt
      case ('iter_num')
         if(obj%f_iter_num) parm = obj%p_iter_num
      case ('any_more')
         if(obj%f_any_more) parm = obj%p_any_more
      case ('dbug_opt')
         if(obj%f_dbug_opt) parm = obj%p_dbug_opt
      case ('scdp_num')
         if(obj%f_scdp_num) parm = obj%p_scdp_num
      case ('ecdp_num')
         if(obj%f_ecdp_num) parm = obj%p_ecdp_num
      case ('ncdp_max')
         if(obj%f_ncdp_max) parm = obj%p_ncdp_max
      case ('stim_win')
         if(obj%f_stim_win) parm = obj%p_stim_win
      case ('etim_win')
         if(obj%f_etim_win) parm = obj%p_etim_win
      case ('cwin_tim')
         if(obj%f_cwin_tim) parm = obj%p_cwin_tim
      case ('cwin_cdp')
         if(obj%f_cwin_cdp) parm = obj%p_cwin_cdp
      case ('skip_opt')
         if(obj%f_skip_opt) parm = obj%p_skip_opt
      case ('mplx_opt')
         if(obj%f_mplx_opt) parm = obj%p_mplx_opt
      case ('corr_opt')
         if(obj%f_corr_opt) parm = obj%p_corr_opt
      case ('cwin_vsh')
         if(obj%f_cwin_vsh) parm = obj%p_cwin_vsh
      case ('scal_opt')
         if(obj%f_scal_opt) parm = obj%p_scal_opt
      case ('obdn_opt')
         if(obj%f_obdn_opt) parm = obj%p_obdn_opt
      case ('smth_vel')
         if(obj%f_smth_vel) parm = obj%p_smth_vel
      case ('swin_opt')
         if(obj%f_swin_opt) parm = obj%p_swin_opt
      case ('scdp_scl')
         if(obj%f_scdp_scl) parm = obj%p_scdp_scl
      case ('ecdp_scl')
         if(obj%f_ecdp_scl) parm = obj%p_ecdp_scl
      case ('stim_scl')
         if(obj%f_stim_scl) parm = obj%p_stim_scl
      case ('etim_scl')
         if(obj%f_etim_scl) parm = obj%p_etim_scl
      case ('con_velp')
         if(obj%f_con_velp) parm = obj%p_con_velp
      case ('ivel_min')
         if(obj%f_ivel_min) parm = obj%p_ivel_min
      case ('ivel_max')
         if(obj%f_ivel_max) parm = obj%p_ivel_max
      case ('svel_min')
         if(obj%f_svel_min) parm = obj%p_svel_min
      case ('svel_max')
         if(obj%f_svel_max) parm = obj%p_svel_max
      case ('vcut_tim')
         if(obj%f_vcut_tim) parm = obj%p_vcut_tim
      case ('vcut_pow')
         if(obj%f_vcut_pow) parm = obj%p_vcut_pow
      case ('dump_opt')
         if(obj%f_dump_opt) parm = obj%p_dump_opt
      case ('stim_dmp')
         if(obj%f_stim_dmp) parm = obj%p_stim_dmp
      case ('etim_dmp')
         if(obj%f_etim_dmp) parm = obj%p_etim_dmp
      case ('dtim_dmp')
         if(obj%f_dtim_dmp) parm = obj%p_dtim_dmp
      case ('scdp_dmp')
         if(obj%f_scdp_dmp) parm = obj%p_scdp_dmp
      case ('ecdp_dmp')
         if(obj%f_ecdp_dmp) parm = obj%p_ecdp_dmp
      case ('dcdp_dmp')
         if(obj%f_dcdp_dmp) parm = obj%p_dcdp_dmp
      case ('same_dbg')
         if(obj%f_same_dbg) parm = obj%p_same_dbg
      case ('hflt_len')
         if(obj%f_hflt_len) parm = obj%p_hflt_len
      end select
      
      end subroutine buhci_get_iparm


!!-------------------------- buhci_set_iparm --------------------------------!!
!!-------------------------- buhci_set_iparm --------------------------------!!
!!-------------------------- buhci_set_iparm --------------------------------!!

      subroutine buhci_set_iparm (obj, name, parm)
      type(buhci_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      integer            ,intent(in)    :: parm

      select case (name)
      case ('iter_cnt')
         obj%p_iter_cnt = parm
         obj%f_iter_cnt = .true.
      case ('iter_num')
         obj%p_iter_num = parm
         obj%f_iter_num = .true.
      case ('any_more')
         obj%p_any_more = parm
         obj%f_any_more = .true.
      case ('dbug_opt')
         obj%p_dbug_opt = parm
         obj%f_dbug_opt = .true.
      case ('scdp_num')
         obj%p_scdp_num = parm
         obj%f_scdp_num = .true.
      case ('ecdp_num')
         obj%p_ecdp_num = parm
         obj%f_ecdp_num = .true.
      case ('ncdp_max')
         obj%p_ncdp_max = parm
         obj%f_ncdp_max = .true.
      case ('stim_win')
         obj%p_stim_win = parm
         obj%f_stim_win = .true.
      case ('etim_win')
         obj%p_etim_win = parm
         obj%f_etim_win = .true.
      case ('cwin_tim')
         obj%p_cwin_tim = parm
         obj%f_cwin_tim = .true.
      case ('cwin_cdp')
         obj%p_cwin_cdp = parm
         obj%f_cwin_cdp = .true.
      case ('skip_opt')
         obj%p_skip_opt = parm
         obj%f_skip_opt = .true.
      case ('mplx_opt')
         obj%p_mplx_opt = parm
         obj%f_mplx_opt = .true.
      case ('corr_opt')
         obj%p_corr_opt = parm
         obj%f_corr_opt = .true.
      case ('cwin_vsh')
         obj%p_cwin_vsh = parm
         obj%f_cwin_vsh = .true.
      case ('scal_opt')
         obj%p_scal_opt = parm
         obj%f_scal_opt = .true.
      case ('obdn_opt')
         obj%p_obdn_opt = parm
         obj%f_obdn_opt = .true.
      case ('smth_vel')
         obj%p_smth_vel = parm
         obj%f_smth_vel = .true.
      case ('swin_opt')
         obj%p_swin_opt = parm
         obj%f_swin_opt = .true.
      case ('scdp_scl')
         obj%p_scdp_scl = parm
         obj%f_scdp_scl = .true.
      case ('ecdp_scl')
         obj%p_ecdp_scl = parm
         obj%f_ecdp_scl = .true.
      case ('stim_scl')
         obj%p_stim_scl = parm
         obj%f_stim_scl = .true.
      case ('etim_scl')
         obj%p_etim_scl = parm
         obj%f_etim_scl = .true.
      case ('con_velp')
         obj%p_con_velp = parm
         obj%f_con_velp = .true.
      case ('ivel_min')
         obj%p_ivel_min = parm
         obj%f_ivel_min = .true.
      case ('ivel_max')
         obj%p_ivel_max = parm
         obj%f_ivel_max = .true.
      case ('svel_min')
         obj%p_svel_min = parm
         obj%f_svel_min = .true.
      case ('svel_max')
         obj%p_svel_max = parm
         obj%f_svel_max = .true.
      case ('vcut_tim')
         obj%p_vcut_tim = parm
         obj%f_vcut_tim = .true.
      case ('vcut_pow')
         obj%p_vcut_pow = parm
         obj%f_vcut_pow = .true.
      case ('dump_opt')
         obj%p_dump_opt = parm
         obj%f_dump_opt = .true.
      case ('stim_dmp')
         obj%p_stim_dmp = parm
         obj%f_stim_dmp = .true.
      case ('etim_dmp')
         obj%p_etim_dmp = parm
         obj%f_etim_dmp = .true.
      case ('dtim_dmp')
         obj%p_dtim_dmp = parm
         obj%f_dtim_dmp = .true.
      case ('scdp_dmp')
         obj%p_scdp_dmp = parm
         obj%f_scdp_dmp = .true.
      case ('ecdp_dmp')
         obj%p_ecdp_dmp = parm
         obj%f_ecdp_dmp = .true.
      case ('dcdp_dmp')
         obj%p_dcdp_dmp = parm
         obj%f_dcdp_dmp = .true.
      case ('same_dbg')
         obj%p_same_dbg = parm
         obj%f_same_dbg = .true.
      case ('hflt_len')
         obj%p_hflt_len = parm
         obj%f_hflt_len = .true.
      end select

      end subroutine buhci_set_iparm


!!-------------------------- buhci_get_rparm --------------------------------!!
!!-------------------------- buhci_get_rparm --------------------------------!!
!!-------------------------- buhci_get_rparm --------------------------------!!

      subroutine buhci_get_rparm (obj, name, parm)
      type(buhci_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      real               ,intent(out)   :: parm

      select case (name)
      case ('scal_fct')
         if(obj%f_scal_fct) parm = obj%p_scal_fct
      case ('scal_rms')
         if(obj%f_scal_rms) parm = obj%p_scal_rms
      case ('angl_rot')
         if(obj%f_angl_rot) parm = obj%p_angl_rot
      case ('ccoef_re')
         if(obj%f_ccoef_re) parm = obj%p_ccoef_re
      case ('ccoef_im')
         if(obj%f_ccoef_im) parm = obj%p_ccoef_im
      case ('atrm_pow')
         if(obj%f_atrm_pow) parm = obj%p_atrm_pow
      case ('btrm_pow')
         if(obj%f_btrm_pow) parm = obj%p_btrm_pow
      case ('q_factor')
         if(obj%f_q_factor) parm = obj%p_q_factor
      case ('ctr_freq')
         if(obj%f_ctr_freq) parm = obj%p_ctr_freq
      case ('amix_fct')
         if(obj%f_amix_fct) parm = obj%p_amix_fct
      case ('bmix_fct')
         if(obj%f_bmix_fct) parm = obj%p_bmix_fct
      case ('alph_fct')
         if(obj%f_alph_fct) parm = obj%p_alph_fct
      case ('ffth_dmn')
         if(obj%f_ffth_dmn) parm = obj%p_ffth_dmn
      case ('ivel_dmn')
         if(obj%f_ivel_dmn) parm = obj%p_ivel_dmn
      case ('ivel_dmx')
         if(obj%f_ivel_dmx) parm = obj%p_ivel_dmx
      case ('svel_dmn')
         if(obj%f_svel_dmn) parm = obj%p_svel_dmn
      case ('svel_dmx')
         if(obj%f_svel_dmx) parm = obj%p_svel_dmx
      case ('real_dbg')
         if(obj%f_real_dbg) parm = obj%p_real_dbg
      case ('imag_dbg')
         if(obj%f_imag_dbg) parm = obj%p_imag_dbg
      end select

      end subroutine buhci_get_rparm


!!-------------------------- buhci_set_rparm --------------------------------!!
!!-------------------------- buhci_set_rparm --------------------------------!!
!!-------------------------- buhci_set_rparm --------------------------------!!

      subroutine buhci_set_rparm (obj, name, parm)
      type(buhci_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      real               ,intent(in)    :: parm
      
      select case (name)
      case ('scal_fct')
         obj%f_scal_fct = .true. 
         obj%p_scal_fct = parm
      case ('scal_rms')
         obj%f_scal_rms = .true. 
         obj%p_scal_rms = parm
      case ('angl_rot')
         obj%f_angl_rot = .true. 
         obj%p_angl_rot = parm
      case ('ccoef_re')
         obj%f_ccoef_re = .true. 
         obj%p_ccoef_re = parm
      case ('ccoef_im')
         obj%f_ccoef_im = .true. 
         obj%p_ccoef_im = parm
      case ('atrm_pow')
         obj%f_atrm_pow = .true. 
         obj%p_atrm_pow = parm
      case ('btrm_pow')
         obj%f_btrm_pow = .true. 
         obj%p_btrm_pow = parm
      case ('q_factor')
         obj%f_q_factor = .true. 
         obj%p_q_factor = parm
      case ('ctr_freq')
         obj%f_ctr_freq = .true. 
         obj%p_ctr_freq = parm
      case ('amix_fct')
         obj%f_amix_fct = .true. 
         obj%p_amix_fct = parm
      case ('bmix_fct')
         obj%f_bmix_fct = .true. 
         obj%p_bmix_fct = parm
      case ('alph_fct')
         obj%f_alph_fct = .true. 
         obj%p_alph_fct = parm
      case ('ffth_dmn')
         obj%f_ffth_dmn = .true. 
         obj%p_ffth_dmn = parm
      case ('ivel_dmn')
         obj%f_ivel_dmn = .true. 
         obj%p_ivel_dmn = parm
      case ('ivel_dmx')
         obj%f_ivel_dmx = .true. 
         obj%p_ivel_dmx = parm
      case ('svel_dmn')
         obj%f_svel_dmn = .true. 
         obj%p_svel_dmn = parm
      case ('svel_dmx')
         obj%f_svel_dmx = .true. 
         obj%p_svel_dmx = parm
      case ('real_dbg')
         obj%f_real_dbg = .true. 
         obj%p_real_dbg = parm
      case ('imag_dbg')
         obj%f_imag_dbg = .true. 
         obj%p_imag_dbg = parm
      end select

      end subroutine buhci_set_rparm


!!-------------------------- buhci_get_cparm --------------------------------!!
!!-------------------------- buhci_get_cparm --------------------------------!!
!!-------------------------- buhci_get_cparm --------------------------------!!

      subroutine buhci_get_cparm (obj, name, parm, lparm)
      type(buhci_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      character(len=*)   ,intent(out)   :: parm

      integer :: lparm
      integer :: i

      lparm = 0
      select case (name)
      case ('mode_opt')
         if(obj%f_mode_opt) parm = obj%p_mode_opt
      case ('otrc_opt')
         if(obj%f_otrc_opt) parm = obj%p_otrc_opt
      case ('veldname')
         if(obj%f_veldname) parm = obj%p_veldname
      end select

      i = 1
100   if (parm(i:i) .ne. ' ' .and. i .lt. 256) then
         i = i + 1
         goto 100
      end if
      lparm = i - 1

      end subroutine buhci_get_cparm


!!-------------------------- buhci_set_cparm --------------------------------!!
!!-------------------------- buhci_set_cparm --------------------------------!!
!!-------------------------- buhci_set_cparm --------------------------------!!

      subroutine buhci_set_cparm (obj, name, parm)
      type(buhci_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      character(len=*)   ,intent(in)    :: parm

      select case (name)
      case ('mode_opt')
         obj%f_mode_opt = .true.
         obj%p_mode_opt = parm
      case ('otrc_opt')
         obj%f_otrc_opt = .true.
         obj%p_otrc_opt = parm
      case ('veldname')
         obj%f_veldname = .true.
         obj%p_veldname = parm
      end select

      end subroutine buhci_set_cparm


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module buhci_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

