!<CPS_v1 type="PRIMITIVE"/>

!!----------------------------- super.f90 ---------------------------------!!
!!----------------------------- super.f90 ---------------------------------!!
!!----------------------------- super.f90 ---------------------------------!!

!    other files are:  super_wrapper.f90  super_frou.f90  super_crou.c


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
!                        C P S  P R I M I T I V E
!
! Name       : super
! Category   : cfe
! Written    : 1999-09-09   by: Donna K. Vunderink
! Revised    : 2003-12-22   by: SPS_BUILD_SUPER
! Maturity   : beta
! Purpose    : CFE Super Process Object Module.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This module provides generic access to all CPS processes.  It is
! somewhat analogous to a base class in object-oriented languages.
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
!                                   o       i
!        call super_create        (obj, process_name)
!
!                                   b
!        call super_initialize    (obj)
!        call super_update        (obj)
!        call super_wrapup        (obj)
!        call super_delete        (obj)
!
!                                   i        o
!        call super_get_name      (obj, process_name)
!
!                                   b    b    b    b
!        call super_oneset        (obj, ntr, hd,  tr)
!        call super_twosets       (obj, ntr, hd1, tr1, hd2, tr2)
!                                   b    b    b    b    o    o
!
!                                                                   opt
!                                       o             o              i
!        call super_list          (process_list, nprocess_list, category_name)
!
!       o                               i
!     valid = super_validate      (process_name)
!
!                                       i
!        call super_set_view      (desired_view)
!
!                                       i          o
!        call super_get_rcs_ident (process_name, ident)
!
!
! type(super_struct),pointer          obj = pointer to the SUPER structure.
! character(*)               process_name = name of CPS process.
! integer                             ntr = number of traces in and out.
! double precision                hd(:,:) = input and output trace headers.
! real                            tr(:,:) = input and output traces.
! double precision               hd1(:,:) = input trace headers.
! real                           tr1(:,:) = input traces.
! double precision               hd2(:,:) = output trace headers.
! real                           tr2(:,:) = output traces.
! character(*)              category_name = name of CPS process category.
! character(*),pointer    process_list(:) = pointer to list of CPS processes.
! integer                   nprocess_list = number of CPS processes.
! logical                           valid = true if process name is valid.
! integer                    desired_view = index of desired view.
! character(*)                      ident = RCS ident of specified CPS process.
!
! PROCESS_LIST must be nullified before first use.  It will be deallocated
! (if associated) and then reallocated to contain a list of NPROCESS_LIST
! processes.
!
! DESIRED_VIEW must be set to one of the following named constants:
!                         SUPER_PRODUCTION
!                         SUPER_BETA
!                         SUPER_ALPHA
!                         SUPER_RAW
!
! Note: HD1 and TR1 are intent(inout) rather than intent(in) because some
! processes might alter the input traces while working with them.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author           Description
!     ----        ------           -----------
!  2. 2003-12-22  SPS_BUILD_SUPER  Created by the SPS_BUILD_SUPER program.
!  1. 1999-09-09  Vunderink        Initial version.
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


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!<process_list>
!PROCESS_COUNT=171
!ABAL                      AMPLITUDE_MOD             PRODUCTION               
!ABRA                      VELOCITY_ANALYSIS         BETA                     
!ACORR                     DIAGNOSTICS               PRODUCTION               
!ADNS                      SYNTHETICS                PRODUCTION               
!ADPSUB                    MISCELLANEOUS             BETA                     
!ALAMO                     VELOCITY_ANALYSIS         BETA                     
!AMPDG                     DIAGNOSTICS               PRODUCTION               
!AVAGRAD                   DIAGNOSTICS               PRODUCTION               
!AVAST                     STACKS                    PRODUCTION               
!AVOANS                    VELOCITY_ANALYSIS         BETA                     
!AVOSTS                    VELOCITY_ANALYSIS         BETA                     
!AVOVAN                    VELOCITY_ANALYSIS         BETA                     
!AVOVIT                    VELOCITY_ANALYSIS         BETA                     
!BUNCH                     SORTS                     PRODUCTION               
!C4WE                      FILTERS                   BETA                     
!CC3D                      STATICS                   PRODUCTION               
!CFDS                      STATICS                   PRODUCTION               
!CHART                     HEADERS                   PRODUCTION               
!CLEANUP                   MISCELLANEOUS             PRODUCTION               
!CLIP                      AMPLITUDE_MOD             PRODUCTION               
!CNEARTS                   FILTERS                   BETA                     
!CODMO                     MIGRATIONS                PRODUCTION               
!COLOR                     PLOT                      PRODUCTION               
!COMBINE                   MISCELLANEOUS             BETA                     
!COMP                      STACKS                    PRODUCTION               
!CTAN                      TRANSFORMS                PRODUCTION               
!DABRA                     MIGRATIONS                BETA                     
!DBGAIN                    AMPLITUDE_MOD             PRODUCTION               
!DECON                     FILTERS                   PRODUCTION               
!DIST                      DIAGNOSTICS               PRODUCTION               
!DMAP3D                    MIGRATIONS                BETA                     
!DMO3D                     MIGRATIONS                PRODUCTION               
!DMOPREP                   SORTS                     PRODUCTION               
!DRC                       MIGRATIONS                PRODUCTION               
!DSIG                      FILTERS                   PRODUCTION               
!DWTF                      FILTERS                   PRODUCTION               
!DWTP                      FILTERS                   PRODUCTION               
!EAGC                      AMPLITUDE_MOD             BETA                     
!EDA                       FILTERS                   PRODUCTION               
!EDA3D                     FILTERS                   PRODUCTION               
!TRACETERP3D               MISCELLANEOUS             BETA               
!ELEV                      HEADERS                   PRODUCTION               
!EXO                       AMPLITUDE_MOD             BETA                     
!EZCHECK                   MISCELLANEOUS             PRODUCTION               
!FBAL                      FILTERS                   PRODUCTION               
!FFAVA                     STACKS                    PRODUCTION               
!FGD                       HEADERS                   PRODUCTION               
!FGDREV                    HEADERS                   PRODUCTION               
!FILL                      MISCELLANEOUS             PRODUCTION               
!FILTP                     FILTERS                   PRODUCTION               
!FISH                      STATICS                   PRODUCTION               
!FKAP                      FILTERS                   PRODUCTION               
!FKFILT                    FILTERS                   PRODUCTION               
!FKTMIG                    MIGRATIONS                BETA                     
!FKTR                      TRANSFORMS                PRODUCTION               
!FLEXBIN                   MISCELLANEOUS             PRODUCTION               
!FXDECON                   FILTERS                   BETA                     
!FXMIG                     MIGRATIONS                BETA                     
!FXRYT                     MIGRATIONS                BETA                     
!FXTI                      MISCELLANEOUS             BETA                     
!FXYDECON                  FILTERS                   BETA                     
!GATHER                    SORTS                     PRODUCTION               
!GDIV                      AMPLITUDE_MOD             PRODUCTION               
!GENFILT                   FILTERS                   PRODUCTION               
!GRAB                      STATICS                   PRODUCTION               
!GVS                       STACKS                    BETA                     
!HEADCHECK                 HEADERS                   PRODUCTION               
!HEADMAP                   HEADERS                   PRODUCTION               
!HEADSUM                   DIAGNOSTICS               BETA                     
!HRZSTK                    STACKS                    BETA                     
!HSYN                      SYNTHETICS                PRODUCTION               
!HVEL                      VELOCITY_ANALYSIS         PRODUCTION               
!IBSMA                     MISCELLANEOUS             BETA                     
!IMS                       STATICS                   PRODUCTION               
!IQ                        FILTERS                   PRODUCTION               
!JOB_DATA                  MISCELLANEOUS             BETA                     
!KA                        MIGRATIONS                BETA                     
!KASTATS                   MISCELLANEOUS             BETA                     
!KDMIG                     MIGRATIONS                BETA                     
!KDMO                      MIGRATIONS                PRODUCTION               
!KMIG                      MIGRATIONS                BETA                     
!KTMIG                     MIGRATIONS                BETA                     
!LMRKHRZ                   IO                        PRODUCTION               
!LMRKIN                    IO                        PRODUCTION               
!LMRKOUT                   IO                        PRODUCTION               
!MADC                      FILTERS                   PRODUCTION               
!MASKER                    MISCELLANEOUS             BETA                     
!MCLINV                    INVERSION                 PRODUCTION               
!MDIP                      FILTERS                   PRODUCTION               
!MDS                       AMPLITUDE_MOD             PRODUCTION               
!MFRS                      STATICS                   PRODUCTION               
!MGD                       HEADERS                   PRODUCTION               
!MODMO                     TRANSFORMS                BETA                     
!MTFUN                     FILTERS                   PRODUCTION               
!MUTE                      AMPLITUDE_MOD             PRODUCTION               
!MVXP                      AMPLITUDE_MOD             PRODUCTION               
!MZPC                      FILTERS                   PRODUCTION               
!NMO                       TRANSFORMS                BETA                     
!NORM                      AMPLITUDE_MOD             PRODUCTION               
!PAIRMERGE                 MISCELLANEOUS             PRODUCTION               
!PGPS                      IO                        PRODUCTION               
!PH2OFF                    TRANSFORMS                BETA                     
!PROJECT_DATA              MISCELLANEOUS             BETA                     
!PSLINV                    INVERSION                 PRODUCTION               
!PSTMIG                    MIGRATIONS                BETA                     
!QEST                      FILTERS                   PRODUCTION               
!RANLINE                   MISCELLANEOUS             PRODUCTION               
!RCPOUT                    MISCELLANEOUS             PRODUCTION               
!REG                       MISCELLANEOUS             PRODUCTION               
!REGBIN                    FILTERS                   PRODUCTION               
!RES                       TRANSFORMS                PRODUCTION               
!RESTH                     HEADERS                   PRODUCTION               
!RFAB                      FILTERS                   PRODUCTION               
!RMUL                      FILTERS                   BETA                     
!RNSYN                     SYNTHETICS                PRODUCTION               
!RTC                       STATICS                   BETA                     
!RYTOV                     MIGRATIONS                PRODUCTION               
!SCAB                      AMPLITUDE_MOD             PRODUCTION               
!SCALE                     AMPLITUDE_MOD             PRODUCTION               
!SCDECON                   FILTERS                   PRODUCTION               
!SDIP                      FILTERS                   PRODUCTION               
!SDIP3D                    FILTERS                   PRODUCTION               
!SELDMO                    SORTS                     PRODUCTION               
!SELECT                    HEADERS                   PRODUCTION               
!SETMUTE                   AMPLITUDE_MOD             PRODUCTION               
!SETPOLY                   HEADERS                   PRODUCTION               
!SETWORD                   HEADERS                   PRODUCTION               
!SHFT                      STATICS                   PRODUCTION               
!SISC                      STATICS                   PRODUCTION               
!SLAB                      MISCELLANEOUS             PRODUCTION               
!SLICE                     MISCELLANEOUS             BETA                     
!SLICER                    MISCELLANEOUS             BETA                     
!SLST                      TRANSFORMS                PRODUCTION               
!SPCT                      TRANSFORMS                PRODUCTION               
!SPIKE                     SYNTHETICS                BETA                     
!SPLT                      PLOT                      PRODUCTION               
!SPTI                      TRANSFORMS                PRODUCTION               
!STK                       STACKS                    BETA                     
!STRETCH                   TRANSFORMS                BETA                     
!SVA                       VELOCITY_ANALYSIS         PRODUCTION               
!SYNBP                     SYNTHETICS                PRODUCTION               
!TABLESAVE                 SORTS                     PRODUCTION               
!TABLESORT                 SORTS                     PRODUCTION               
!TDC                       TRANSFORMS                PRODUCTION               
!TDMP                      DIAGNOSTICS               PRODUCTION               
!TELAV                     AMPLITUDE_MOD             PRODUCTION               
!TFATT                     TRANSFORMS                PRODUCTION               
!TPOW                      AMPLITUDE_MOD             PRODUCTION               
!TREDIT                    HEADERS                   BETA                     
!TRIN                      IO                        BETA                     
!TRINSORT                  IO                        PRODUCTION               
!TRMO                      VELOCITY_ANALYSIS         BETA                     
!TROT                      IO                        BETA                     
!TRSTATS                   MISCELLANEOUS             BETA                     
!TSEL                      MISCELLANEOUS             PRODUCTION               
!TSLC                      SORTS                     PRODUCTION               
!TSMUTE                    VELOCITY_ANALYSIS         BETA                     
!TSORT                     SORTS                     PRODUCTION               
!PARALLELSORT              SORTS                     PRODUCTION               
!TSVF                      FILTERS                   PRODUCTION               
!TTMO                      VELOCITY_ANALYSIS         BETA                     
!TTRIN                     IO                        BETA                     
!TTROT                     IO                        BETA                     
!TVF                       FILTERS                   PRODUCTION               
!UNGATHER                  SORTS                     PRODUCTION               
!UTEL                      MISCELLANEOUS             BETA                     
!VC                        FILTERS                   PRODUCTION               
!VELEDIT                   VELOCITY_ANALYSIS         BETA                     
!VPICK                     VELOCITY_ANALYSIS         PRODUCTION               
!VTRIM                     VELOCITY_ANALYSIS         PRODUCTION               
!WSF                       FILTERS                   PRODUCTION               
!XP                        AMPLITUDE_MOD             PRODUCTION               
!</process_list>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module super_module
      use pc_module
      use named_constants_module
      use string_module
      use cio_module

      use ABAL_module
      use ABRA_module
      use ACORR_module
      use ADNS_module
      use ADPSUB_module
      use ALAMO_module
      use AMPDG_module
      use AVAGRAD_module
      use AVAST_module
      use AVOANS_module
      use AVOSTS_module
      use AVOVAN_module
      use AVOVIT_module
      use BUNCH_module
      use C4WE_module
      use CC3D_module
      use CFDS_module
      use CHART_module
      use CLEANUP_module
      use CLIP_module
      use CNEARTS_module
      use CODMO_module
      use COLOR_module
      use COMBINE_module
      use COMP_module
      use CTAN_module
      use DABRA_module
      use DBGAIN_module
      use DECON_module
      use DIST_module
      use DMAP3D_module
      use DMO3D_module
      use DMOPREP_module
      use DRC_module
      use DSIG_module
      use DWTF_module
      use DWTP_module
      use EAGC_module
      use EDA_module
      use EDA3D_module
      use TRACETERP3D_module
      use ELEV_module
      use EXO_module
      use EZCHECK_module
      use FBAL_module
      use FFAVA_module
      use FGD_module
      use FGDREV_module
      use FILL_module
      use FILTP_module
      use FISH_module
      use FKAP_module
      use FKFILT_module
      use FKTMIG_module
      use FKTR_module
      use FLEXBIN_module
      use FXDECON_module
      use FXMIG_module
      use FXRYT_module
      use FXTI_module
      use FXYDECON_module
      use GATHER_module
      use GDIV_module
      use GENFILT_module
      use GRAB_module
      use GVS_module
      use HEADCHECK_module
      use HEADMAP_module
      use HEADSUM_module
      use HRZSTK_module
      use HSYN_module
      use HVEL_module
      use IBSMA_module
      use IMS_module
      use IQ_module
      use JOB_DATA_module
      use KA_module
      use KASTATS_module
      use KDMIG_module
      use KDMO_module
      use KMIG_module
      use KTMIG_module
      use LMRKHRZ_module
      use LMRKIN_module
      use LMRKOUT_module
      use MADC_module
      use MASKER_module
      use MCLINV_module
      use MDIP_module
      use MDS_module
      use MFRS_module
      use MGD_module
      use MODMO_module
      use MTFUN_module
      use MUTE_module
      use MVXP_module
      use MZPC_module
      use NMO_module
      use NORM_module
      use PAIRMERGE_module
      use PGPS_module
      use PH2OFF_module
      use PROJECT_DATA_module
      use PSLINV_module
      use PSTMIG_module
      use QEST_module
      use RANLINE_module
      use RCPOUT_module
      use REG_module
      use REGBIN_module
      use RES_module
      use RESTH_module
      use RFAB_module
      use RMUL_module
      use RNSYN_module
      use RTC_module
      use RYTOV_module
      use SCAB_module
      use SCALE_module
      use SCDECON_module
      use SDIP_module
      use SDIP3D_module
      use SELDMO_module
      use SELECT_module
      use SETMUTE_module
      use SETPOLY_module
      use SETWORD_module
      use SHFT_module
      use SISC_module
      use SLAB_module
      use SLICE_module
      use SLICER_module
      use SLST_module
      use SPCT_module
      use SPIKE_module
      use SPLT_module
      use SPTI_module
      use STK_module
      use STRETCH_module
      use SVA_module
      use SYNBP_module
      use TABLESAVE_module
      use TABLESORT_module
      use TDC_module
      use TDMP_module
      use TELAV_module
      use TFATT_module
      use TPOW_module
      use TREDIT_module
      use TRIN_module
      use TRINSORT_module
      use TRMO_module
      use TROT_module
      use TRSTATS_module
      use TSEL_module
      use TSLC_module
      use TSMUTE_module
      use TSORT_module
      use PARALLELSORT_module
      use TSVF_module
      use TTMO_module
      use TTRIN_module
      use TTROT_module
      use TVF_module
      use UNGATHER_module
      use UTEL_module
      use VC_module
      use VELEDIT_module
      use VPICK_module
      use VTRIM_module
      use WSF_module
      use XP_module

      implicit none
      private
      public :: super_create
      public :: super_initialize
      public :: super_update
      public :: super_wrapup
      public :: super_oneset
      public :: super_twosets
      public :: super_delete
      public :: super_list
      public :: super_validate
      public :: super_set_view
      public :: super_get_name
      public :: super_get_rcs_ident

      character(len=100),public,save :: super_ident = &
       '$Id: super.f90,v 1.126 2003/07/17 21:54:16 Goodger beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: super_struct

        private
        character(len=PC_LENGTH) :: name

        type(ABAL_struct         ),pointer :: ABAL
        type(ABRA_struct         ),pointer :: ABRA
        type(ACORR_struct        ),pointer :: ACORR
        type(ADNS_struct         ),pointer :: ADNS
        type(ADPSUB_struct       ),pointer :: ADPSUB
        type(ALAMO_struct        ),pointer :: ALAMO
        type(AMPDG_struct        ),pointer :: AMPDG
        type(AVAGRAD_struct      ),pointer :: AVAGRAD
        type(AVAST_struct        ),pointer :: AVAST
        type(AVOANS_struct       ),pointer :: AVOANS
        type(AVOSTS_struct       ),pointer :: AVOSTS
        type(AVOVAN_struct       ),pointer :: AVOVAN
        type(AVOVIT_struct       ),pointer :: AVOVIT
        type(BUNCH_struct        ),pointer :: BUNCH
        type(C4WE_struct         ),pointer :: C4WE
        type(CC3D_struct         ),pointer :: CC3D
        type(CFDS_struct         ),pointer :: CFDS
        type(CHART_struct        ),pointer :: CHART
        type(CLEANUP_struct      ),pointer :: CLEANUP
        type(CLIP_struct         ),pointer :: CLIP
        type(CNEARTS_struct      ),pointer :: CNEARTS
        type(CODMO_struct        ),pointer :: CODMO
        type(COLOR_struct        ),pointer :: COLOR
        type(COMBINE_struct      ),pointer :: COMBINE
        type(COMP_struct         ),pointer :: COMP
        type(CTAN_struct         ),pointer :: CTAN
        type(DABRA_struct        ),pointer :: DABRA
        type(DBGAIN_struct       ),pointer :: DBGAIN
        type(DECON_struct        ),pointer :: DECON
        type(DIST_struct         ),pointer :: DIST
        type(DMAP3D_struct       ),pointer :: DMAP3D
        type(DMO3D_struct        ),pointer :: DMO3D
        type(DMOPREP_struct      ),pointer :: DMOPREP
        type(DRC_struct          ),pointer :: DRC
        type(DSIG_struct         ),pointer :: DSIG
        type(DWTF_struct         ),pointer :: DWTF
        type(DWTP_struct         ),pointer :: DWTP
        type(EAGC_struct         ),pointer :: EAGC
        type(EDA_struct          ),pointer :: EDA
        type(EDA3D_struct        ),pointer :: EDA3D
        type(TRACETERP3D_struct  ),pointer :: TRACETERP3D
        type(ELEV_struct         ),pointer :: ELEV
        type(EXO_struct          ),pointer :: EXO
        type(EZCHECK_struct      ),pointer :: EZCHECK
        type(FBAL_struct         ),pointer :: FBAL
        type(FFAVA_struct        ),pointer :: FFAVA
        type(FGD_struct          ),pointer :: FGD
        type(FGDREV_struct       ),pointer :: FGDREV
        type(FILL_struct         ),pointer :: FILL
        type(FILTP_struct        ),pointer :: FILTP
        type(FISH_struct         ),pointer :: FISH
        type(FKAP_struct         ),pointer :: FKAP
        type(FKFILT_struct       ),pointer :: FKFILT
        type(FKTMIG_struct       ),pointer :: FKTMIG
        type(FKTR_struct         ),pointer :: FKTR
        type(FLEXBIN_struct      ),pointer :: FLEXBIN
        type(FXDECON_struct      ),pointer :: FXDECON
        type(FXMIG_struct        ),pointer :: FXMIG
        type(FXRYT_struct        ),pointer :: FXRYT
        type(FXTI_struct         ),pointer :: FXTI
        type(FXYDECON_struct     ),pointer :: FXYDECON
        type(GATHER_struct       ),pointer :: GATHER
        type(GDIV_struct         ),pointer :: GDIV
        type(GENFILT_struct      ),pointer :: GENFILT
        type(GRAB_struct         ),pointer :: GRAB
        type(GVS_struct          ),pointer :: GVS
        type(HEADCHECK_struct    ),pointer :: HEADCHECK
        type(HEADMAP_struct      ),pointer :: HEADMAP
        type(HEADSUM_struct      ),pointer :: HEADSUM
        type(HRZSTK_struct       ),pointer :: HRZSTK
        type(HSYN_struct         ),pointer :: HSYN
        type(HVEL_struct         ),pointer :: HVEL
        type(IBSMA_struct        ),pointer :: IBSMA
        type(IMS_struct          ),pointer :: IMS
        type(IQ_struct           ),pointer :: IQ
        type(JOB_DATA_struct     ),pointer :: JOB_DATA
        type(KA_struct           ),pointer :: KA
        type(KASTATS_struct      ),pointer :: KASTATS
        type(KDMIG_struct        ),pointer :: KDMIG
        type(KDMO_struct         ),pointer :: KDMO
        type(KMIG_struct         ),pointer :: KMIG
        type(KTMIG_struct        ),pointer :: KTMIG
        type(LMRKHRZ_struct      ),pointer :: LMRKHRZ
        type(LMRKIN_struct       ),pointer :: LMRKIN
        type(LMRKOUT_struct      ),pointer :: LMRKOUT
        type(MADC_struct         ),pointer :: MADC
        type(MASKER_struct       ),pointer :: MASKER
        type(MCLINV_struct       ),pointer :: MCLINV
        type(MDIP_struct         ),pointer :: MDIP
        type(MDS_struct          ),pointer :: MDS
        type(MFRS_struct         ),pointer :: MFRS
        type(MGD_struct          ),pointer :: MGD
        type(MODMO_struct        ),pointer :: MODMO
        type(MTFUN_struct        ),pointer :: MTFUN
        type(MUTE_struct         ),pointer :: MUTE
        type(MVXP_struct         ),pointer :: MVXP
        type(MZPC_struct         ),pointer :: MZPC
        type(NMO_struct          ),pointer :: NMO
        type(NORM_struct         ),pointer :: NORM
        type(PAIRMERGE_struct    ),pointer :: PAIRMERGE
        type(PGPS_struct         ),pointer :: PGPS
        type(PH2OFF_struct       ),pointer :: PH2OFF
        type(PROJECT_DATA_struct ),pointer :: PROJECT_DATA
        type(PSLINV_struct       ),pointer :: PSLINV
        type(PSTMIG_struct       ),pointer :: PSTMIG
        type(QEST_struct         ),pointer :: QEST
        type(RANLINE_struct      ),pointer :: RANLINE
        type(RCPOUT_struct       ),pointer :: RCPOUT
        type(REG_struct          ),pointer :: REG
        type(REGBIN_struct       ),pointer :: REGBIN
        type(RES_struct          ),pointer :: RES
        type(RESTH_struct        ),pointer :: RESTH
        type(RFAB_struct         ),pointer :: RFAB
        type(RMUL_struct         ),pointer :: RMUL
        type(RNSYN_struct        ),pointer :: RNSYN
        type(RTC_struct          ),pointer :: RTC
        type(RYTOV_struct        ),pointer :: RYTOV
        type(SCAB_struct         ),pointer :: SCAB
        type(SCALE_struct        ),pointer :: SCALE
        type(SCDECON_struct      ),pointer :: SCDECON
        type(SDIP_struct         ),pointer :: SDIP
        type(SDIP3D_struct       ),pointer :: SDIP3D
        type(SELDMO_struct       ),pointer :: SELDMO
        type(SELECT_struct       ),pointer :: SELECT
        type(SETMUTE_struct      ),pointer :: SETMUTE
        type(SETPOLY_struct      ),pointer :: SETPOLY
        type(SETWORD_struct      ),pointer :: SETWORD
        type(SHFT_struct         ),pointer :: SHFT
        type(SISC_struct         ),pointer :: SISC
        type(SLAB_struct         ),pointer :: SLAB
        type(SLICE_struct        ),pointer :: SLICE
        type(SLICER_struct       ),pointer :: SLICER
        type(SLST_struct         ),pointer :: SLST
        type(SPCT_struct         ),pointer :: SPCT
        type(SPIKE_struct        ),pointer :: SPIKE
        type(SPLT_struct         ),pointer :: SPLT
        type(SPTI_struct         ),pointer :: SPTI
        type(STK_struct          ),pointer :: STK
        type(STRETCH_struct      ),pointer :: STRETCH
        type(SVA_struct          ),pointer :: SVA
        type(SYNBP_struct        ),pointer :: SYNBP
        type(TABLESAVE_struct    ),pointer :: TABLESAVE
        type(TABLESORT_struct    ),pointer :: TABLESORT
        type(TDC_struct          ),pointer :: TDC
        type(TDMP_struct         ),pointer :: TDMP
        type(TELAV_struct        ),pointer :: TELAV
        type(TFATT_struct        ),pointer :: TFATT
        type(TPOW_struct         ),pointer :: TPOW
        type(TREDIT_struct       ),pointer :: TREDIT
        type(TRIN_struct         ),pointer :: TRIN
        type(TRINSORT_struct     ),pointer :: TRINSORT
        type(TRMO_struct         ),pointer :: TRMO
        type(TROT_struct         ),pointer :: TROT
        type(TRSTATS_struct      ),pointer :: TRSTATS
        type(TSEL_struct         ),pointer :: TSEL
        type(TSLC_struct         ),pointer :: TSLC
        type(TSMUTE_struct       ),pointer :: TSMUTE
        type(TSORT_struct        ),pointer :: TSORT
        type(PARALLELSORT_struct ),pointer :: PARALLELSORT
        type(TSVF_struct         ),pointer :: TSVF
        type(TTMO_struct         ),pointer :: TTMO
        type(TTRIN_struct        ),pointer :: TTRIN
        type(TTROT_struct        ),pointer :: TTROT
        type(TVF_struct          ),pointer :: TVF
        type(UNGATHER_struct     ),pointer :: UNGATHER
        type(UTEL_struct         ),pointer :: UTEL
        type(VC_struct           ),pointer :: VC
        type(VELEDIT_struct      ),pointer :: VELEDIT
        type(VPICK_struct        ),pointer :: VPICK
        type(VTRIM_struct        ),pointer :: VTRIM
        type(WSF_struct          ),pointer :: WSF
        type(XP_struct           ),pointer :: XP

      end type super_struct

      type :: plist_struct
        character(len=PC_LENGTH) :: process
        character(len=20)        :: category
        integer                  :: maturity
      end type plist_struct

      integer,parameter,public   :: SUPER_PRODUCTION = 1
      integer,parameter,public   :: SUPER_BETA       = 2
      integer,parameter,public   :: SUPER_ALPHA      = 3
      integer,parameter,public   :: SUPER_RAW        = 4
      integer,parameter,private  :: LEN_ALL          = 172

      type(plist_struct)         :: all(LEN_ALL)
      integer,save               :: view = SUPER_BETA
      logical,save               :: first_time = .TRUE.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine super_create (obj,process_name)
      implicit none
      type(super_struct),pointer       :: obj                !argument
      character(len=*),intent(in)      :: process_name       !argument

      allocate (obj)

      nullify (obj%ABAL         )
      nullify (obj%ABRA         )
      nullify (obj%ACORR        )
      nullify (obj%ADNS         )
      nullify (obj%ADPSUB       )
      nullify (obj%ALAMO        )
      nullify (obj%AMPDG        )
      nullify (obj%AVAGRAD      )
      nullify (obj%AVAST        )
      nullify (obj%AVOANS       )
      nullify (obj%AVOSTS       )
      nullify (obj%AVOVAN       )
      nullify (obj%AVOVIT       )
      nullify (obj%BUNCH        )
      nullify (obj%C4WE         )
      nullify (obj%CC3D         )
      nullify (obj%CFDS         )
      nullify (obj%CHART        )
      nullify (obj%CLEANUP      )
      nullify (obj%CLIP         )
      nullify (obj%CNEARTS      )
      nullify (obj%CODMO        )
      nullify (obj%COLOR        )
      nullify (obj%COMBINE      )
      nullify (obj%COMP         )
      nullify (obj%CTAN         )
      nullify (obj%DABRA        )
      nullify (obj%DBGAIN       )
      nullify (obj%DECON        )
      nullify (obj%DIST         )
      nullify (obj%DMAP3D       )
      nullify (obj%DMO3D        )
      nullify (obj%DMOPREP      )
      nullify (obj%DRC          )
      nullify (obj%DSIG         )
      nullify (obj%DWTF         )
      nullify (obj%DWTP         )
      nullify (obj%EAGC         )
      nullify (obj%EDA          )
      nullify (obj%EDA3D        )
      nullify (obj%TRACETERP3D  )
      nullify (obj%ELEV         )
      nullify (obj%EXO          )
      nullify (obj%EZCHECK      )
      nullify (obj%FBAL         )
      nullify (obj%FFAVA        )
      nullify (obj%FGD          )
      nullify (obj%FGDREV       )
      nullify (obj%FILL         )
      nullify (obj%FILTP        )
      nullify (obj%FISH         )
      nullify (obj%FKAP         )
      nullify (obj%FKFILT       )
      nullify (obj%FKTMIG       )
      nullify (obj%FKTR         )
      nullify (obj%FLEXBIN      )
      nullify (obj%FXDECON      )
      nullify (obj%FXMIG        )
      nullify (obj%FXRYT        )
      nullify (obj%FXTI         )
      nullify (obj%FXYDECON     )
      nullify (obj%GATHER       )
      nullify (obj%GDIV         )
      nullify (obj%GENFILT      )
      nullify (obj%GRAB         )
      nullify (obj%GVS          )
      nullify (obj%HEADCHECK    )
      nullify (obj%HEADMAP      )
      nullify (obj%HEADSUM      )
      nullify (obj%HRZSTK       )
      nullify (obj%HSYN         )
      nullify (obj%HVEL         )
      nullify (obj%IBSMA        )
      nullify (obj%IMS          )
      nullify (obj%IQ           )
      nullify (obj%JOB_DATA     )
      nullify (obj%KA           )
      nullify (obj%KASTATS      )
      nullify (obj%KDMIG        )
      nullify (obj%KDMO         )
      nullify (obj%KMIG         )
      nullify (obj%KTMIG        )
      nullify (obj%LMRKHRZ      )
      nullify (obj%LMRKIN       )
      nullify (obj%LMRKOUT      )
      nullify (obj%MADC         )
      nullify (obj%MASKER       )
      nullify (obj%MCLINV       )
      nullify (obj%MDIP         )
      nullify (obj%MDS          )
      nullify (obj%MFRS         )
      nullify (obj%MGD          )
      nullify (obj%MODMO        )
      nullify (obj%MTFUN        )
      nullify (obj%MUTE         )
      nullify (obj%MVXP         )
      nullify (obj%MZPC         )
      nullify (obj%NMO          )
      nullify (obj%NORM         )
      nullify (obj%PAIRMERGE    )
      nullify (obj%PGPS         )
      nullify (obj%PH2OFF       )
      nullify (obj%PROJECT_DATA )
      nullify (obj%PSLINV       )
      nullify (obj%PSTMIG       )
      nullify (obj%QEST         )
      nullify (obj%RANLINE      )
      nullify (obj%RCPOUT       )
      nullify (obj%REG          )
      nullify (obj%REGBIN       )
      nullify (obj%RES          )
      nullify (obj%RESTH        )
      nullify (obj%RFAB         )
      nullify (obj%RMUL         )
      nullify (obj%RNSYN        )
      nullify (obj%RTC          )
      nullify (obj%RYTOV        )
      nullify (obj%SCAB         )
      nullify (obj%SCALE        )
      nullify (obj%SCDECON      )
      nullify (obj%SDIP         )
      nullify (obj%SDIP3D       )
      nullify (obj%SELDMO       )
      nullify (obj%SELECT       )
      nullify (obj%SETMUTE      )
      nullify (obj%SETPOLY      )
      nullify (obj%SETWORD      )
      nullify (obj%SHFT         )
      nullify (obj%SISC         )
      nullify (obj%SLAB         )
      nullify (obj%SLICE        )
      nullify (obj%SLICER       )
      nullify (obj%SLST         )
      nullify (obj%SPCT         )
      nullify (obj%SPIKE        )
      nullify (obj%SPLT         )
      nullify (obj%SPTI         )
      nullify (obj%STK          )
      nullify (obj%STRETCH      )
      nullify (obj%SVA          )
      nullify (obj%SYNBP        )
      nullify (obj%TABLESAVE    )
      nullify (obj%TABLESORT    )
      nullify (obj%TDC          )
      nullify (obj%TDMP         )
      nullify (obj%TELAV        )
      nullify (obj%TFATT        )
      nullify (obj%TPOW         )
      nullify (obj%TREDIT       )
      nullify (obj%TRIN         )
      nullify (obj%TRINSORT     )
      nullify (obj%TRMO         )
      nullify (obj%TROT         )
      nullify (obj%TRSTATS      )
      nullify (obj%TSEL         )
      nullify (obj%TSLC         )
      nullify (obj%TSMUTE       )
      nullify (obj%TSORT        )
      nullify (obj%PARALLELSORT )
      nullify (obj%TSVF         )
      nullify (obj%TTMO         )
      nullify (obj%TTRIN        )
      nullify (obj%TTROT        )
      nullify (obj%TVF          )
      nullify (obj%UNGATHER     )
      nullify (obj%UTEL         )
      nullify (obj%VC           )
      nullify (obj%VELEDIT      )
      nullify (obj%VPICK        )
      nullify (obj%VTRIM        )
      nullify (obj%WSF          )
      nullify (obj%XP           )

      obj%name = process_name
      call string_to_upper (obj%name)

      select case (obj%name)

   case ("ABAL"         ) ; call ABAL_create          (obj%ABAL         )
   case ("ABRA"         ) ; call ABRA_create          (obj%ABRA         )
   case ("ACORR"        ) ; call ACORR_create         (obj%ACORR        )
   case ("ADNS"         ) ; call ADNS_create          (obj%ADNS         )
   case ("ADPSUB"       ) ; call ADPSUB_create        (obj%ADPSUB       )
   case ("ALAMO"        ) ; call ALAMO_create         (obj%ALAMO        )
   case ("AMPDG"        ) ; call AMPDG_create         (obj%AMPDG        )
   case ("AVAGRAD"      ) ; call AVAGRAD_create       (obj%AVAGRAD      )
   case ("AVAST"        ) ; call AVAST_create         (obj%AVAST        )
   case ("AVOANS"       ) ; call AVOANS_create        (obj%AVOANS       )
   case ("AVOSTS"       ) ; call AVOSTS_create        (obj%AVOSTS       )
   case ("AVOVAN"       ) ; call AVOVAN_create        (obj%AVOVAN       )
   case ("AVOVIT"       ) ; call AVOVIT_create        (obj%AVOVIT       )
   case ("BUNCH"        ) ; call BUNCH_create         (obj%BUNCH        )
   case ("C4WE"         ) ; call C4WE_create          (obj%C4WE         )
   case ("CC3D"         ) ; call CC3D_create          (obj%CC3D         )
   case ("CFDS"         ) ; call CFDS_create          (obj%CFDS         )
   case ("CHART"        ) ; call CHART_create         (obj%CHART        )
   case ("CLEANUP"      ) ; call CLEANUP_create       (obj%CLEANUP      )
   case ("CLIP"         ) ; call CLIP_create          (obj%CLIP         )
   case ("CNEARTS"      ) ; call CNEARTS_create       (obj%CNEARTS      )
   case ("CODMO"        ) ; call CODMO_create         (obj%CODMO        )
   case ("COLOR"        ) ; call COLOR_create         (obj%COLOR        )
   case ("COMBINE"      ) ; call COMBINE_create       (obj%COMBINE      )
   case ("COMP"         ) ; call COMP_create          (obj%COMP         )
   case ("CTAN"         ) ; call CTAN_create          (obj%CTAN         )
   case ("DABRA"        ) ; call DABRA_create         (obj%DABRA        )
   case ("DBGAIN"       ) ; call DBGAIN_create        (obj%DBGAIN       )
   case ("DECON"        ) ; call DECON_create         (obj%DECON        )
   case ("DIST"         ) ; call DIST_create          (obj%DIST         )
   case ("DMAP3D"       ) ; call DMAP3D_create        (obj%DMAP3D       )
   case ("DMO3D"        ) ; call DMO3D_create         (obj%DMO3D        )
   case ("DMOPREP"      ) ; call DMOPREP_create       (obj%DMOPREP      )
   case ("DRC"          ) ; call DRC_create           (obj%DRC          )
   case ("DSIG"         ) ; call DSIG_create          (obj%DSIG         )
   case ("DWTF"         ) ; call DWTF_create          (obj%DWTF         )
   case ("DWTP"         ) ; call DWTP_create          (obj%DWTP         )
   case ("EAGC"         ) ; call EAGC_create          (obj%EAGC         )
   case ("EDA"          ) ; call EDA_create           (obj%EDA          )
   case ("EDA3D"        ) ; call EDA3D_create         (obj%EDA3D        )
   case ("TRACETERP3D"  ) ; call TRACETERP3D_create   (obj%TRACETERP3D  )
   case ("ELEV"         ) ; call ELEV_create          (obj%ELEV         )
   case ("EXO"          ) ; call EXO_create           (obj%EXO          )
   case ("EZCHECK"      ) ; call EZCHECK_create       (obj%EZCHECK      )
   case ("FBAL"         ) ; call FBAL_create          (obj%FBAL         )
   case ("FFAVA"        ) ; call FFAVA_create         (obj%FFAVA        )
   case ("FGD"          ) ; call FGD_create           (obj%FGD          )
   case ("FGDREV"       ) ; call FGDREV_create        (obj%FGDREV       )
   case ("FILL"         ) ; call FILL_create          (obj%FILL         )
   case ("FILTP"        ) ; call FILTP_create         (obj%FILTP        )
   case ("FISH"         ) ; call FISH_create          (obj%FISH         )
   case ("FKAP"         ) ; call FKAP_create          (obj%FKAP         )
   case ("FKFILT"       ) ; call FKFILT_create        (obj%FKFILT       )
   case ("FKTMIG"       ) ; call FKTMIG_create        (obj%FKTMIG       )
   case ("FKTR"         ) ; call FKTR_create          (obj%FKTR         )
   case ("FLEXBIN"      ) ; call FLEXBIN_create       (obj%FLEXBIN      )
   case ("FXDECON"      ) ; call FXDECON_create       (obj%FXDECON      )
   case ("FXMIG"        ) ; call FXMIG_create         (obj%FXMIG        )
   case ("FXRYT"        ) ; call FXRYT_create         (obj%FXRYT        )
   case ("FXTI"         ) ; call FXTI_create          (obj%FXTI         )
   case ("FXYDECON"     ) ; call FXYDECON_create      (obj%FXYDECON     )
   case ("GATHER"       ) ; call GATHER_create        (obj%GATHER       )
   case ("GDIV"         ) ; call GDIV_create          (obj%GDIV         )
   case ("GENFILT"      ) ; call GENFILT_create       (obj%GENFILT      )
   case ("GRAB"         ) ; call GRAB_create          (obj%GRAB         )
   case ("GVS"          ) ; call GVS_create           (obj%GVS          )
   case ("HEADCHECK"    ) ; call HEADCHECK_create     (obj%HEADCHECK    )
   case ("HEADMAP"      ) ; call HEADMAP_create       (obj%HEADMAP      )
   case ("HEADSUM"      ) ; call HEADSUM_create       (obj%HEADSUM      )
   case ("HRZSTK"       ) ; call HRZSTK_create        (obj%HRZSTK       )
   case ("HSYN"         ) ; call HSYN_create          (obj%HSYN         )
   case ("HVEL"         ) ; call HVEL_create          (obj%HVEL         )
   case ("IBSMA"        ) ; call IBSMA_create         (obj%IBSMA        )
   case ("IMS"          ) ; call IMS_create           (obj%IMS          )
   case ("IQ"           ) ; call IQ_create            (obj%IQ           )
   case ("JOB_DATA"     ) ; call JOB_DATA_create      (obj%JOB_DATA     )
   case ("KA"           ) ; call KA_create            (obj%KA           )
   case ("KASTATS"      ) ; call KASTATS_create       (obj%KASTATS      )
   case ("KDMIG"        ) ; call KDMIG_create         (obj%KDMIG        )
   case ("KDMO"         ) ; call KDMO_create          (obj%KDMO         )
   case ("KMIG"         ) ; call KMIG_create          (obj%KMIG         )
   case ("KTMIG"        ) ; call KTMIG_create         (obj%KTMIG        )
   case ("LMRKHRZ"      ) ; call LMRKHRZ_create       (obj%LMRKHRZ      )
   case ("LMRKIN"       ) ; call LMRKIN_create        (obj%LMRKIN       )
   case ("LMRKOUT"      ) ; call LMRKOUT_create       (obj%LMRKOUT      )
   case ("MADC"         ) ; call MADC_create          (obj%MADC         )
   case ("MASKER"       ) ; call MASKER_create        (obj%MASKER       )
   case ("MCLINV"       ) ; call MCLINV_create        (obj%MCLINV       )
   case ("MDIP"         ) ; call MDIP_create          (obj%MDIP         )
   case ("MDS"          ) ; call MDS_create           (obj%MDS          )
   case ("MFRS"         ) ; call MFRS_create          (obj%MFRS         )
   case ("MGD"          ) ; call MGD_create           (obj%MGD          )
   case ("MODMO"        ) ; call MODMO_create         (obj%MODMO        )
   case ("MTFUN"        ) ; call MTFUN_create         (obj%MTFUN        )
   case ("MUTE"         ) ; call MUTE_create          (obj%MUTE         )
   case ("MVXP"         ) ; call MVXP_create          (obj%MVXP         )
   case ("MZPC"         ) ; call MZPC_create          (obj%MZPC         )
   case ("NMO"          ) ; call NMO_create           (obj%NMO          )
   case ("NORM"         ) ; call NORM_create          (obj%NORM         )
   case ("PAIRMERGE"    ) ; call PAIRMERGE_create     (obj%PAIRMERGE    )
   case ("PGPS"         ) ; call PGPS_create          (obj%PGPS         )
   case ("PH2OFF"       ) ; call PH2OFF_create        (obj%PH2OFF       )
   case ("PROJECT_DATA" ) ; call PROJECT_DATA_create  (obj%PROJECT_DATA )
   case ("PSLINV"       ) ; call PSLINV_create        (obj%PSLINV       )
   case ("PSTMIG"       ) ; call PSTMIG_create        (obj%PSTMIG       )
   case ("QEST"         ) ; call QEST_create          (obj%QEST         )
   case ("RANLINE"      ) ; call RANLINE_create       (obj%RANLINE      )
   case ("RCPOUT"       ) ; call RCPOUT_create        (obj%RCPOUT       )
   case ("REG"          ) ; call REG_create           (obj%REG          )
   case ("REGBIN"       ) ; call REGBIN_create        (obj%REGBIN       )
   case ("RES"          ) ; call RES_create           (obj%RES          )
   case ("RESTH"        ) ; call RESTH_create         (obj%RESTH        )
   case ("RFAB"         ) ; call RFAB_create          (obj%RFAB         )
   case ("RMUL"         ) ; call RMUL_create          (obj%RMUL         )
   case ("RNSYN"        ) ; call RNSYN_create         (obj%RNSYN        )
   case ("RTC"          ) ; call RTC_create           (obj%RTC          )
   case ("RYTOV"        ) ; call RYTOV_create         (obj%RYTOV        )
   case ("SCAB"         ) ; call SCAB_create          (obj%SCAB         )
   case ("SCALE"        ) ; call SCALE_create         (obj%SCALE        )
   case ("SCDECON"      ) ; call SCDECON_create       (obj%SCDECON      )
   case ("SDIP"         ) ; call SDIP_create          (obj%SDIP         )
   case ("SDIP3D"       ) ; call SDIP3D_create        (obj%SDIP3D       )
   case ("SELDMO"       ) ; call SELDMO_create        (obj%SELDMO       )
   case ("SELECT"       ) ; call SELECT_create        (obj%SELECT       )
   case ("SETMUTE"      ) ; call SETMUTE_create       (obj%SETMUTE      )
   case ("SETPOLY"      ) ; call SETPOLY_create       (obj%SETPOLY      )
   case ("SETWORD"      ) ; call SETWORD_create       (obj%SETWORD      )
   case ("SHFT"         ) ; call SHFT_create          (obj%SHFT         )
   case ("SISC"         ) ; call SISC_create          (obj%SISC         )
   case ("SLAB"         ) ; call SLAB_create          (obj%SLAB         )
   case ("SLICE"        ) ; call SLICE_create         (obj%SLICE        )
   case ("SLICER"       ) ; call SLICER_create        (obj%SLICER       )
   case ("SLST"         ) ; call SLST_create          (obj%SLST         )
   case ("SPCT"         ) ; call SPCT_create          (obj%SPCT         )
   case ("SPIKE"        ) ; call SPIKE_create         (obj%SPIKE        )
   case ("SPLT"         ) ; call SPLT_create          (obj%SPLT         )
   case ("SPTI"         ) ; call SPTI_create          (obj%SPTI         )
   case ("STK"          ) ; call STK_create           (obj%STK          )
   case ("STRETCH"      ) ; call STRETCH_create       (obj%STRETCH      )
   case ("SVA"          ) ; call SVA_create           (obj%SVA          )
   case ("SYNBP"        ) ; call SYNBP_create         (obj%SYNBP        )
   case ("TABLESAVE"    ) ; call TABLESAVE_create     (obj%TABLESAVE    )
   case ("TABLESORT"    ) ; call TABLESORT_create     (obj%TABLESORT    )
   case ("TDC"          ) ; call TDC_create           (obj%TDC          )
   case ("TDMP"         ) ; call TDMP_create          (obj%TDMP         )
   case ("TELAV"        ) ; call TELAV_create         (obj%TELAV        )
   case ("TFATT"        ) ; call TFATT_create         (obj%TFATT        )
   case ("TPOW"         ) ; call TPOW_create          (obj%TPOW         )
   case ("TREDIT"       ) ; call TREDIT_create        (obj%TREDIT       )
   case ("TRIN"         ) ; call TRIN_create          (obj%TRIN         )
   case ("TRINSORT"     ) ; call TRINSORT_create      (obj%TRINSORT     )
   case ("TRMO"         ) ; call TRMO_create          (obj%TRMO         )
   case ("TROT"         ) ; call TROT_create          (obj%TROT         )
   case ("TRSTATS"      ) ; call TRSTATS_create       (obj%TRSTATS      )
   case ("TSEL"         ) ; call TSEL_create          (obj%TSEL         )
   case ("TSLC"         ) ; call TSLC_create          (obj%TSLC         )
   case ("TSMUTE"       ) ; call TSMUTE_create        (obj%TSMUTE       )
   case ("TSORT"        ) ; call TSORT_create         (obj%TSORT        )
   case ("PARALLELSORT" ) ; call PARALLELSORT_create  (obj%PARALLELSORT )
   case ("TSVF"         ) ; call TSVF_create          (obj%TSVF         )
   case ("TTMO"         ) ; call TTMO_create          (obj%TTMO         )
   case ("TTRIN"        ) ; call TTRIN_create         (obj%TTRIN        )
   case ("TTROT"        ) ; call TTROT_create         (obj%TTROT        )
   case ("TVF"          ) ; call TVF_create           (obj%TVF          )
   case ("UNGATHER"     ) ; call UNGATHER_create      (obj%UNGATHER     )
   case ("UTEL"         ) ; call UTEL_create          (obj%UTEL         )
   case ("VC"           ) ; call VC_create            (obj%VC           )
   case ("VELEDIT"      ) ; call VELEDIT_create       (obj%VELEDIT      )
   case ("VPICK"        ) ; call VPICK_create         (obj%VPICK        )
   case ("VTRIM"        ) ; call VTRIM_create         (obj%VTRIM        )
   case ("WSF"          ) ; call WSF_create           (obj%WSF          )
   case ("XP"           ) ; call XP_create            (obj%XP           )

      case default ; call pc_error ("Invalid process",obj%name)
      end select

      end subroutine super_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine super_delete (obj)
      implicit none
      type(super_struct),pointer       :: obj                !argument

      if (.not. associated(obj)) return

      print *,'deleting process '//trim(obj%name)

      select case (obj%name)

   case ("ABAL"         ) ; call ABAL_delete          (obj%ABAL         )
   case ("ABRA"         ) ; call ABRA_delete          (obj%ABRA         )
   case ("ACORR"        ) ; call ACORR_delete         (obj%ACORR        )
   case ("ADNS"         ) ; call ADNS_delete          (obj%ADNS         )
   case ("ADPSUB"       ) ; call ADPSUB_delete        (obj%ADPSUB       )
   case ("ALAMO"        ) ; call ALAMO_delete         (obj%ALAMO        )
   case ("AMPDG"        ) ; call AMPDG_delete         (obj%AMPDG        )
   case ("AVAGRAD"      ) ; call AVAGRAD_delete       (obj%AVAGRAD      )
   case ("AVAST"        ) ; call AVAST_delete         (obj%AVAST        )
   case ("AVOANS"       ) ; call AVOANS_delete        (obj%AVOANS       )
   case ("AVOSTS"       ) ; call AVOSTS_delete        (obj%AVOSTS       )
   case ("AVOVAN"       ) ; call AVOVAN_delete        (obj%AVOVAN       )
   case ("AVOVIT"       ) ; call AVOVIT_delete        (obj%AVOVIT       )
   case ("BUNCH"        ) ; call BUNCH_delete         (obj%BUNCH        )
   case ("C4WE"         ) ; call C4WE_delete          (obj%C4WE         )
   case ("CC3D"         ) ; call CC3D_delete          (obj%CC3D         )
   case ("CFDS"         ) ; call CFDS_delete          (obj%CFDS         )
   case ("CHART"        ) ; call CHART_delete         (obj%CHART        )
   case ("CLEANUP"      ) ; call CLEANUP_delete       (obj%CLEANUP      )
   case ("CLIP"         ) ; call CLIP_delete          (obj%CLIP         )
   case ("CNEARTS"      ) ; call CNEARTS_delete       (obj%CNEARTS      )
   case ("CODMO"        ) ; call CODMO_delete         (obj%CODMO        )
   case ("COLOR"        ) ; call COLOR_delete         (obj%COLOR        )
   case ("COMBINE"      ) ; call COMBINE_delete       (obj%COMBINE      )
   case ("COMP"         ) ; call COMP_delete          (obj%COMP         )
   case ("CTAN"         ) ; call CTAN_delete          (obj%CTAN         )
   case ("DABRA"        ) ; call DABRA_delete         (obj%DABRA        )
   case ("DBGAIN"       ) ; call DBGAIN_delete        (obj%DBGAIN       )
   case ("DECON"        ) ; call DECON_delete         (obj%DECON        )
   case ("DIST"         ) ; call DIST_delete          (obj%DIST         )
   case ("DMAP3D"       ) ; call DMAP3D_delete        (obj%DMAP3D       )
   case ("DMO3D"        ) ; call DMO3D_delete         (obj%DMO3D        )
   case ("DMOPREP"      ) ; call DMOPREP_delete       (obj%DMOPREP      )
   case ("DRC"          ) ; call DRC_delete           (obj%DRC          )
   case ("DSIG"         ) ; call DSIG_delete          (obj%DSIG         )
   case ("DWTF"         ) ; call DWTF_delete          (obj%DWTF         )
   case ("DWTP"         ) ; call DWTP_delete          (obj%DWTP         )
   case ("EAGC"         ) ; call EAGC_delete          (obj%EAGC         )
   case ("EDA"          ) ; call EDA_delete           (obj%EDA          )
   case ("EDA3D"        ) ; call EDA3D_delete         (obj%EDA3D        )
   case ("TRACETERP3D"  ) ; call TRACETERP3D_delete   (obj%TRACETERP3D  )
   case ("ELEV"         ) ; call ELEV_delete          (obj%ELEV         )
   case ("EXO"          ) ; call EXO_delete           (obj%EXO          )
   case ("EZCHECK"      ) ; call EZCHECK_delete       (obj%EZCHECK      )
   case ("FBAL"         ) ; call FBAL_delete          (obj%FBAL         )
   case ("FFAVA"        ) ; call FFAVA_delete         (obj%FFAVA        )
   case ("FGD"          ) ; call FGD_delete           (obj%FGD          )
   case ("FGDREV"       ) ; call FGDREV_delete        (obj%FGDREV       )
   case ("FILL"         ) ; call FILL_delete          (obj%FILL         )
   case ("FILTP"        ) ; call FILTP_delete         (obj%FILTP        )
   case ("FISH"         ) ; call FISH_delete          (obj%FISH         )
   case ("FKAP"         ) ; call FKAP_delete          (obj%FKAP         )
   case ("FKFILT"       ) ; call FKFILT_delete        (obj%FKFILT       )
   case ("FKTMIG"       ) ; call FKTMIG_delete        (obj%FKTMIG       )
   case ("FKTR"         ) ; call FKTR_delete          (obj%FKTR         )
   case ("FLEXBIN"      ) ; call FLEXBIN_delete       (obj%FLEXBIN      )
   case ("FXDECON"      ) ; call FXDECON_delete       (obj%FXDECON      )
   case ("FXMIG"        ) ; call FXMIG_delete         (obj%FXMIG        )
   case ("FXRYT"        ) ; call FXRYT_delete         (obj%FXRYT        )
   case ("FXTI"         ) ; call FXTI_delete          (obj%FXTI         )
   case ("FXYDECON"     ) ; call FXYDECON_delete      (obj%FXYDECON     )
   case ("GATHER"       ) ; call GATHER_delete        (obj%GATHER       )
   case ("GDIV"         ) ; call GDIV_delete          (obj%GDIV         )
   case ("GENFILT"      ) ; call GENFILT_delete       (obj%GENFILT      )
   case ("GRAB"         ) ; call GRAB_delete          (obj%GRAB         )
   case ("GVS"          ) ; call GVS_delete           (obj%GVS          )
   case ("HEADCHECK"    ) ; call HEADCHECK_delete     (obj%HEADCHECK    )
   case ("HEADMAP"      ) ; call HEADMAP_delete       (obj%HEADMAP      )
   case ("HEADSUM"      ) ; call HEADSUM_delete       (obj%HEADSUM      )
   case ("HRZSTK"       ) ; call HRZSTK_delete        (obj%HRZSTK       )
   case ("HSYN"         ) ; call HSYN_delete          (obj%HSYN         )
   case ("HVEL"         ) ; call HVEL_delete          (obj%HVEL         )
   case ("IBSMA"        ) ; call IBSMA_delete         (obj%IBSMA        )
   case ("IMS"          ) ; call IMS_delete           (obj%IMS          )
   case ("IQ"           ) ; call IQ_delete            (obj%IQ           )
   case ("JOB_DATA"     ) ; call JOB_DATA_delete      (obj%JOB_DATA     )
   case ("KA"           ) ; call KA_delete            (obj%KA           )
   case ("KASTATS"      ) ; call KASTATS_delete       (obj%KASTATS      )
   case ("KDMIG"        ) ; call KDMIG_delete         (obj%KDMIG        )
   case ("KDMO"         ) ; call KDMO_delete          (obj%KDMO         )
   case ("KMIG"         ) ; call KMIG_delete          (obj%KMIG         )
   case ("KTMIG"        ) ; call KTMIG_delete         (obj%KTMIG        )
   case ("LMRKHRZ"      ) ; call LMRKHRZ_delete       (obj%LMRKHRZ      )
   case ("LMRKIN"       ) ; call LMRKIN_delete        (obj%LMRKIN       )
   case ("LMRKOUT"      ) ; call LMRKOUT_delete       (obj%LMRKOUT      )
   case ("MADC"         ) ; call MADC_delete          (obj%MADC         )
   case ("MASKER"       ) ; call MASKER_delete        (obj%MASKER       )
   case ("MCLINV"       ) ; call MCLINV_delete        (obj%MCLINV       )
   case ("MDIP"         ) ; call MDIP_delete          (obj%MDIP         )
   case ("MDS"          ) ; call MDS_delete           (obj%MDS          )
   case ("MFRS"         ) ; call MFRS_delete          (obj%MFRS         )
   case ("MGD"          ) ; call MGD_delete           (obj%MGD          )
   case ("MODMO"        ) ; call MODMO_delete         (obj%MODMO        )
   case ("MTFUN"        ) ; call MTFUN_delete         (obj%MTFUN        )
   case ("MUTE"         ) ; call MUTE_delete          (obj%MUTE         )
   case ("MVXP"         ) ; call MVXP_delete          (obj%MVXP         )
   case ("MZPC"         ) ; call MZPC_delete          (obj%MZPC         )
   case ("NMO"          ) ; call NMO_delete           (obj%NMO          )
   case ("NORM"         ) ; call NORM_delete          (obj%NORM         )
   case ("PAIRMERGE"    ) ; call PAIRMERGE_delete     (obj%PAIRMERGE    )
   case ("PGPS"         ) ; call PGPS_delete          (obj%PGPS         )
   case ("PH2OFF"       ) ; call PH2OFF_delete        (obj%PH2OFF       )
   case ("PROJECT_DATA" ) ; call PROJECT_DATA_delete  (obj%PROJECT_DATA )
   case ("PSLINV"       ) ; call PSLINV_delete        (obj%PSLINV       )
   case ("PSTMIG"       ) ; call PSTMIG_delete        (obj%PSTMIG       )
   case ("QEST"         ) ; call QEST_delete          (obj%QEST         )
   case ("RANLINE"      ) ; call RANLINE_delete       (obj%RANLINE      )
   case ("RCPOUT"       ) ; call RCPOUT_delete        (obj%RCPOUT       )
   case ("REG"          ) ; call REG_delete           (obj%REG          )
   case ("REGBIN"       ) ; call REGBIN_delete        (obj%REGBIN       )
   case ("RES"          ) ; call RES_delete           (obj%RES          )
   case ("RESTH"        ) ; call RESTH_delete         (obj%RESTH        )
   case ("RFAB"         ) ; call RFAB_delete          (obj%RFAB         )
   case ("RMUL"         ) ; call RMUL_delete          (obj%RMUL         )
   case ("RNSYN"        ) ; call RNSYN_delete         (obj%RNSYN        )
   case ("RTC"          ) ; call RTC_delete           (obj%RTC          )
   case ("RYTOV"        ) ; call RYTOV_delete         (obj%RYTOV        )
   case ("SCAB"         ) ; call SCAB_delete          (obj%SCAB         )
   case ("SCALE"        ) ; call SCALE_delete         (obj%SCALE        )
   case ("SCDECON"      ) ; call SCDECON_delete       (obj%SCDECON      )
   case ("SDIP"         ) ; call SDIP_delete          (obj%SDIP         )
   case ("SDIP3D"       ) ; call SDIP3D_delete        (obj%SDIP3D       )
   case ("SELDMO"       ) ; call SELDMO_delete        (obj%SELDMO       )
   case ("SELECT"       ) ; call SELECT_delete        (obj%SELECT       )
   case ("SETMUTE"      ) ; call SETMUTE_delete       (obj%SETMUTE      )
   case ("SETPOLY"      ) ; call SETPOLY_delete       (obj%SETPOLY      )
   case ("SETWORD"      ) ; call SETWORD_delete       (obj%SETWORD      )
   case ("SHFT"         ) ; call SHFT_delete          (obj%SHFT         )
   case ("SISC"         ) ; call SISC_delete          (obj%SISC         )
   case ("SLAB"         ) ; call SLAB_delete          (obj%SLAB         )
   case ("SLICE"        ) ; call SLICE_delete         (obj%SLICE        )
   case ("SLICER"       ) ; call SLICER_delete        (obj%SLICER       )
   case ("SLST"         ) ; call SLST_delete          (obj%SLST         )
   case ("SPCT"         ) ; call SPCT_delete          (obj%SPCT         )
   case ("SPIKE"        ) ; call SPIKE_delete         (obj%SPIKE        )
   case ("SPLT"         ) ; call SPLT_delete          (obj%SPLT         )
   case ("SPTI"         ) ; call SPTI_delete          (obj%SPTI         )
   case ("STK"          ) ; call STK_delete           (obj%STK          )
   case ("STRETCH"      ) ; call STRETCH_delete       (obj%STRETCH      )
   case ("SVA"          ) ; call SVA_delete           (obj%SVA          )
   case ("SYNBP"        ) ; call SYNBP_delete         (obj%SYNBP        )
   case ("TABLESAVE"    ) ; call TABLESAVE_delete     (obj%TABLESAVE    )
   case ("TABLESORT"    ) ; call TABLESORT_delete     (obj%TABLESORT    )
   case ("TDC"          ) ; call TDC_delete           (obj%TDC          )
   case ("TDMP"         ) ; call TDMP_delete          (obj%TDMP         )
   case ("TELAV"        ) ; call TELAV_delete         (obj%TELAV        )
   case ("TFATT"        ) ; call TFATT_delete         (obj%TFATT        )
   case ("TPOW"         ) ; call TPOW_delete          (obj%TPOW         )
   case ("TREDIT"       ) ; call TREDIT_delete        (obj%TREDIT       )
   case ("TRIN"         ) ; call TRIN_delete          (obj%TRIN         )
   case ("TRINSORT"     ) ; call TRINSORT_delete      (obj%TRINSORT     )
   case ("TRMO"         ) ; call TRMO_delete          (obj%TRMO         )
   case ("TROT"         ) ; call TROT_delete          (obj%TROT         )
   case ("TRSTATS"      ) ; call TRSTATS_delete       (obj%TRSTATS      )
   case ("TSEL"         ) ; call TSEL_delete          (obj%TSEL         )
   case ("TSLC"         ) ; call TSLC_delete          (obj%TSLC         )
   case ("TSMUTE"       ) ; call TSMUTE_delete        (obj%TSMUTE       )
   case ("TSORT"        ) ; call TSORT_delete         (obj%TSORT        )
   case ("PARALLELSORT" ) ; call PARALLELSORT_delete  (obj%PARALLELSORT )
   case ("TSVF"         ) ; call TSVF_delete          (obj%TSVF         )
   case ("TTMO"         ) ; call TTMO_delete          (obj%TTMO         )
   case ("TTRIN"        ) ; call TTRIN_delete         (obj%TTRIN        )
   case ("TTROT"        ) ; call TTROT_delete         (obj%TTROT        )
   case ("TVF"          ) ; call TVF_delete           (obj%TVF          )
   case ("UNGATHER"     ) ; call UNGATHER_delete      (obj%UNGATHER     )
   case ("UTEL"         ) ; call UTEL_delete          (obj%UTEL         )
   case ("VC"           ) ; call VC_delete            (obj%VC           )
   case ("VELEDIT"      ) ; call VELEDIT_delete       (obj%VELEDIT      )
   case ("VPICK"        ) ; call VPICK_delete         (obj%VPICK        )
   case ("VTRIM"        ) ; call VTRIM_delete         (obj%VTRIM        )
   case ("WSF"          ) ; call WSF_delete           (obj%WSF          )
   case ("XP"           ) ; call XP_delete            (obj%XP           )

      case default ; call pc_error ("Invalid process",obj%name)
      end select

      deallocate(obj)

      end subroutine super_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine super_initialize (obj)
      implicit none
      type(super_struct),pointer       :: obj                !argument

      select case (obj%name)

   case ("ABAL"         ) ; call ABAL_initialize          (obj%ABAL         )
   case ("ABRA"         ) ; call ABRA_initialize          (obj%ABRA         )
   case ("ACORR"        ) ; call ACORR_initialize         (obj%ACORR        )
   case ("ADNS"         ) ; call ADNS_initialize          (obj%ADNS         )
   case ("ADPSUB"       ) ; call ADPSUB_initialize        (obj%ADPSUB       )
   case ("ALAMO"        ) ; call ALAMO_initialize         (obj%ALAMO        )
   case ("AMPDG"        ) ; call AMPDG_initialize         (obj%AMPDG        )
   case ("AVAGRAD"      ) ; call AVAGRAD_initialize       (obj%AVAGRAD      )
   case ("AVAST"        ) ; call AVAST_initialize         (obj%AVAST        )
   case ("AVOANS"       ) ; call AVOANS_initialize        (obj%AVOANS       )
   case ("AVOSTS"       ) ; call AVOSTS_initialize        (obj%AVOSTS       )
   case ("AVOVAN"       ) ; call AVOVAN_initialize        (obj%AVOVAN       )
   case ("AVOVIT"       ) ; call AVOVIT_initialize        (obj%AVOVIT       )
   case ("BUNCH"        ) ; call BUNCH_initialize         (obj%BUNCH        )
   case ("C4WE"         ) ; call C4WE_initialize          (obj%C4WE         )
   case ("CC3D"         ) ; call CC3D_initialize          (obj%CC3D         )
   case ("CFDS"         ) ; call CFDS_initialize          (obj%CFDS         )
   case ("CHART"        ) ; call CHART_initialize         (obj%CHART        )
   case ("CLEANUP"      ) ; call CLEANUP_initialize       (obj%CLEANUP      )
   case ("CLIP"         ) ; call CLIP_initialize          (obj%CLIP         )
   case ("CNEARTS"      ) ; call CNEARTS_initialize       (obj%CNEARTS      )
   case ("CODMO"        ) ; call CODMO_initialize         (obj%CODMO        )
   case ("COLOR"        ) ; call COLOR_initialize         (obj%COLOR        )
   case ("COMBINE"      ) ; call COMBINE_initialize       (obj%COMBINE      )
   case ("COMP"         ) ; call COMP_initialize          (obj%COMP         )
   case ("CTAN"         ) ; call CTAN_initialize          (obj%CTAN         )
   case ("DABRA"        ) ; call DABRA_initialize         (obj%DABRA        )
   case ("DBGAIN"       ) ; call DBGAIN_initialize        (obj%DBGAIN       )
   case ("DECON"        ) ; call DECON_initialize         (obj%DECON        )
   case ("DIST"         ) ; call DIST_initialize          (obj%DIST         )
   case ("DMAP3D"       ) ; call DMAP3D_initialize        (obj%DMAP3D       )
   case ("DMO3D"        ) ; call DMO3D_initialize         (obj%DMO3D        )
   case ("DMOPREP"      ) ; call DMOPREP_initialize       (obj%DMOPREP      )
   case ("DRC"          ) ; call DRC_initialize           (obj%DRC          )
   case ("DSIG"         ) ; call DSIG_initialize          (obj%DSIG         )
   case ("DWTF"         ) ; call DWTF_initialize          (obj%DWTF         )
   case ("DWTP"         ) ; call DWTP_initialize          (obj%DWTP         )
   case ("EAGC"         ) ; call EAGC_initialize          (obj%EAGC         )
   case ("EDA"          ) ; call EDA_initialize           (obj%EDA          )
   case ("EDA3D"        ) ; call EDA3D_initialize         (obj%EDA3D        )
   case ("TRACETERP3D"  ) ; call TRACETERP3D_initialize   (obj%TRACETERP3D  )
   case ("ELEV"         ) ; call ELEV_initialize          (obj%ELEV         )
   case ("EXO"          ) ; call EXO_initialize           (obj%EXO          )
   case ("EZCHECK"      ) ; call EZCHECK_initialize       (obj%EZCHECK      )
   case ("FBAL"         ) ; call FBAL_initialize          (obj%FBAL         )
   case ("FFAVA"        ) ; call FFAVA_initialize         (obj%FFAVA        )
   case ("FGD"          ) ; call FGD_initialize           (obj%FGD          )
   case ("FGDREV"       ) ; call FGDREV_initialize        (obj%FGDREV       )
   case ("FILL"         ) ; call FILL_initialize          (obj%FILL         )
   case ("FILTP"        ) ; call FILTP_initialize         (obj%FILTP        )
   case ("FISH"         ) ; call FISH_initialize          (obj%FISH         )
   case ("FKAP"         ) ; call FKAP_initialize          (obj%FKAP         )
   case ("FKFILT"       ) ; call FKFILT_initialize        (obj%FKFILT       )
   case ("FKTMIG"       ) ; call FKTMIG_initialize        (obj%FKTMIG       )
   case ("FKTR"         ) ; call FKTR_initialize          (obj%FKTR         )
   case ("FLEXBIN"      ) ; call FLEXBIN_initialize       (obj%FLEXBIN      )
   case ("FXDECON"      ) ; call FXDECON_initialize       (obj%FXDECON      )
   case ("FXMIG"        ) ; call FXMIG_initialize         (obj%FXMIG        )
   case ("FXRYT"        ) ; call FXRYT_initialize         (obj%FXRYT        )
   case ("FXTI"         ) ; call FXTI_initialize          (obj%FXTI         )
   case ("FXYDECON"     ) ; call FXYDECON_initialize      (obj%FXYDECON     )
   case ("GATHER"       ) ; call GATHER_initialize        (obj%GATHER       )
   case ("GDIV"         ) ; call GDIV_initialize          (obj%GDIV         )
   case ("GENFILT"      ) ; call GENFILT_initialize       (obj%GENFILT      )
   case ("GRAB"         ) ; call GRAB_initialize          (obj%GRAB         )
   case ("GVS"          ) ; call GVS_initialize           (obj%GVS          )
   case ("HEADCHECK"    ) ; call HEADCHECK_initialize     (obj%HEADCHECK    )
   case ("HEADMAP"      ) ; call HEADMAP_initialize       (obj%HEADMAP      )
   case ("HEADSUM"      ) ; call HEADSUM_initialize       (obj%HEADSUM      )
   case ("HRZSTK"       ) ; call HRZSTK_initialize        (obj%HRZSTK       )
   case ("HSYN"         ) ; call HSYN_initialize          (obj%HSYN         )
   case ("HVEL"         ) ; call HVEL_initialize          (obj%HVEL         )
   case ("IBSMA"        ) ; call IBSMA_initialize         (obj%IBSMA        )
   case ("IMS"          ) ; call IMS_initialize           (obj%IMS          )
   case ("IQ"           ) ; call IQ_initialize            (obj%IQ           )
   case ("JOB_DATA"     ) ; call JOB_DATA_initialize      (obj%JOB_DATA     )
   case ("KA"           ) ; call KA_initialize            (obj%KA           )
   case ("KASTATS"      ) ; call KASTATS_initialize       (obj%KASTATS      )
   case ("KDMIG"        ) ; call KDMIG_initialize         (obj%KDMIG        )
   case ("KDMO"         ) ; call KDMO_initialize          (obj%KDMO         )
   case ("KMIG"         ) ; call KMIG_initialize          (obj%KMIG         )
   case ("KTMIG"        ) ; call KTMIG_initialize         (obj%KTMIG        )
   case ("LMRKHRZ"      ) ; call LMRKHRZ_initialize       (obj%LMRKHRZ      )
   case ("LMRKIN"       ) ; call LMRKIN_initialize        (obj%LMRKIN       )
   case ("LMRKOUT"      ) ; call LMRKOUT_initialize       (obj%LMRKOUT      )
   case ("MADC"         ) ; call MADC_initialize          (obj%MADC         )
   case ("MASKER"       ) ; call MASKER_initialize        (obj%MASKER       )
   case ("MCLINV"       ) ; call MCLINV_initialize        (obj%MCLINV       )
   case ("MDIP"         ) ; call MDIP_initialize          (obj%MDIP         )
   case ("MDS"          ) ; call MDS_initialize           (obj%MDS          )
   case ("MFRS"         ) ; call MFRS_initialize          (obj%MFRS         )
   case ("MGD"          ) ; call MGD_initialize           (obj%MGD          )
   case ("MODMO"        ) ; call MODMO_initialize         (obj%MODMO        )
   case ("MTFUN"        ) ; call MTFUN_initialize         (obj%MTFUN        )
   case ("MUTE"         ) ; call MUTE_initialize          (obj%MUTE         )
   case ("MVXP"         ) ; call MVXP_initialize          (obj%MVXP         )
   case ("MZPC"         ) ; call MZPC_initialize          (obj%MZPC         )
   case ("NMO"          ) ; call NMO_initialize           (obj%NMO          )
   case ("NORM"         ) ; call NORM_initialize          (obj%NORM         )
   case ("PAIRMERGE"    ) ; call PAIRMERGE_initialize     (obj%PAIRMERGE    )
   case ("PGPS"         ) ; call PGPS_initialize          (obj%PGPS         )
   case ("PH2OFF"       ) ; call PH2OFF_initialize        (obj%PH2OFF       )
   case ("PROJECT_DATA" ) ; call PROJECT_DATA_initialize  (obj%PROJECT_DATA )
   case ("PSLINV"       ) ; call PSLINV_initialize        (obj%PSLINV       )
   case ("PSTMIG"       ) ; call PSTMIG_initialize        (obj%PSTMIG       )
   case ("QEST"         ) ; call QEST_initialize          (obj%QEST         )
   case ("RANLINE"      ) ; call RANLINE_initialize       (obj%RANLINE      )
   case ("RCPOUT"       ) ; call RCPOUT_initialize        (obj%RCPOUT       )
   case ("REG"          ) ; call REG_initialize           (obj%REG          )
   case ("REGBIN"       ) ; call REGBIN_initialize        (obj%REGBIN       )
   case ("RES"          ) ; call RES_initialize           (obj%RES          )
   case ("RESTH"        ) ; call RESTH_initialize         (obj%RESTH        )
   case ("RFAB"         ) ; call RFAB_initialize          (obj%RFAB         )
   case ("RMUL"         ) ; call RMUL_initialize          (obj%RMUL         )
   case ("RNSYN"        ) ; call RNSYN_initialize         (obj%RNSYN        )
   case ("RTC"          ) ; call RTC_initialize           (obj%RTC          )
   case ("RYTOV"        ) ; call RYTOV_initialize         (obj%RYTOV        )
   case ("SCAB"         ) ; call SCAB_initialize          (obj%SCAB         )
   case ("SCALE"        ) ; call SCALE_initialize         (obj%SCALE        )
   case ("SCDECON"      ) ; call SCDECON_initialize       (obj%SCDECON      )
   case ("SDIP"         ) ; call SDIP_initialize          (obj%SDIP         )
   case ("SDIP3D"       ) ; call SDIP3D_initialize        (obj%SDIP3D       )
   case ("SELDMO"       ) ; call SELDMO_initialize        (obj%SELDMO       )
   case ("SELECT"       ) ; call SELECT_initialize        (obj%SELECT       )
   case ("SETMUTE"      ) ; call SETMUTE_initialize       (obj%SETMUTE      )
   case ("SETPOLY"      ) ; call SETPOLY_initialize       (obj%SETPOLY      )
   case ("SETWORD"      ) ; call SETWORD_initialize       (obj%SETWORD      )
   case ("SHFT"         ) ; call SHFT_initialize          (obj%SHFT         )
   case ("SISC"         ) ; call SISC_initialize          (obj%SISC         )
   case ("SLAB"         ) ; call SLAB_initialize          (obj%SLAB         )
   case ("SLICE"        ) ; call SLICE_initialize         (obj%SLICE        )
   case ("SLICER"       ) ; call SLICER_initialize        (obj%SLICER       )
   case ("SLST"         ) ; call SLST_initialize          (obj%SLST         )
   case ("SPCT"         ) ; call SPCT_initialize          (obj%SPCT         )
   case ("SPIKE"        ) ; call SPIKE_initialize         (obj%SPIKE        )
   case ("SPLT"         ) ; call SPLT_initialize          (obj%SPLT         )
   case ("SPTI"         ) ; call SPTI_initialize          (obj%SPTI         )
   case ("STK"          ) ; call STK_initialize           (obj%STK          )
   case ("STRETCH"      ) ; call STRETCH_initialize       (obj%STRETCH      )
   case ("SVA"          ) ; call SVA_initialize           (obj%SVA          )
   case ("SYNBP"        ) ; call SYNBP_initialize         (obj%SYNBP        )
   case ("TABLESAVE"    ) ; call TABLESAVE_initialize     (obj%TABLESAVE    )
   case ("TABLESORT"    ) ; call TABLESORT_initialize     (obj%TABLESORT    )
   case ("TDC"          ) ; call TDC_initialize           (obj%TDC          )
   case ("TDMP"         ) ; call TDMP_initialize          (obj%TDMP         )
   case ("TELAV"        ) ; call TELAV_initialize         (obj%TELAV        )
   case ("TFATT"        ) ; call TFATT_initialize         (obj%TFATT        )
   case ("TPOW"         ) ; call TPOW_initialize          (obj%TPOW         )
   case ("TREDIT"       ) ; call TREDIT_initialize        (obj%TREDIT       )
   case ("TRIN"         ) ; call TRIN_initialize          (obj%TRIN         )
   case ("TRINSORT"     ) ; call TRINSORT_initialize      (obj%TRINSORT     )
   case ("TRMO"         ) ; call TRMO_initialize          (obj%TRMO         )
   case ("TROT"         ) ; call TROT_initialize          (obj%TROT         )
   case ("TRSTATS"      ) ; call TRSTATS_initialize       (obj%TRSTATS      )
   case ("TSEL"         ) ; call TSEL_initialize          (obj%TSEL         )
   case ("TSLC"         ) ; call TSLC_initialize          (obj%TSLC         )
   case ("TSMUTE"       ) ; call TSMUTE_initialize        (obj%TSMUTE       )
   case ("TSORT"        ) ; call TSORT_initialize         (obj%TSORT        )
   case ("PARALLELSORT"        ) ; call PARALLELSORT_initialize         (obj%PARALLELSORT        )
   case ("TSVF"         ) ; call TSVF_initialize          (obj%TSVF         )
   case ("TTMO"         ) ; call TTMO_initialize          (obj%TTMO         )
   case ("TTRIN"        ) ; call TTRIN_initialize         (obj%TTRIN        )
   case ("TTROT"        ) ; call TTROT_initialize         (obj%TTROT        )
   case ("TVF"          ) ; call TVF_initialize           (obj%TVF          )
   case ("UNGATHER"     ) ; call UNGATHER_initialize      (obj%UNGATHER     )
   case ("UTEL"         ) ; call UTEL_initialize          (obj%UTEL         )
   case ("VC"           ) ; call VC_initialize            (obj%VC           )
   case ("VELEDIT"      ) ; call VELEDIT_initialize       (obj%VELEDIT      )
   case ("VPICK"        ) ; call VPICK_initialize         (obj%VPICK        )
   case ("VTRIM"        ) ; call VTRIM_initialize         (obj%VTRIM        )
   case ("WSF"          ) ; call WSF_initialize           (obj%WSF          )
   case ("XP"           ) ; call XP_initialize            (obj%XP           )

      case default ; call pc_error ("Invalid process",obj%name)
      end select

      end subroutine super_initialize


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine super_update (obj)
      implicit none
      type(super_struct),pointer       :: obj                !argument


      if (.not. associated(obj)) return

      select case (obj%name)

   case ("ABAL"         ) ; call ABAL_update          (obj%ABAL         )
   case ("ABRA"         ) ; call ABRA_update          (obj%ABRA         )
   case ("ACORR"        ) ; call ACORR_update         (obj%ACORR        )
   case ("ADNS"         ) ; call ADNS_update          (obj%ADNS         )
   case ("ADPSUB"       ) ; call ADPSUB_update        (obj%ADPSUB       )
   case ("ALAMO"        ) ; call ALAMO_update         (obj%ALAMO        )
   case ("AMPDG"        ) ; call AMPDG_update         (obj%AMPDG        )
   case ("AVAGRAD"      ) ; call AVAGRAD_update       (obj%AVAGRAD      )
   case ("AVAST"        ) ; call AVAST_update         (obj%AVAST        )
   case ("AVOANS"       ) ; call AVOANS_update        (obj%AVOANS       )
   case ("AVOSTS"       ) ; call AVOSTS_update        (obj%AVOSTS       )
   case ("AVOVAN"       ) ; call AVOVAN_update        (obj%AVOVAN       )
   case ("AVOVIT"       ) ; call AVOVIT_update        (obj%AVOVIT       )
   case ("BUNCH"        ) ; call BUNCH_update         (obj%BUNCH        )
   case ("C4WE"         ) ; call C4WE_update          (obj%C4WE         )
   case ("CC3D"         ) ; call CC3D_update          (obj%CC3D         )
   case ("CFDS"         ) ; call CFDS_update          (obj%CFDS         )
   case ("CHART"        ) ; call CHART_update         (obj%CHART        )
   case ("CLEANUP"      ) ; call CLEANUP_update       (obj%CLEANUP      )
   case ("CLIP"         ) ; call CLIP_update          (obj%CLIP         )
   case ("CNEARTS"      ) ; call CNEARTS_update       (obj%CNEARTS      )
   case ("CODMO"        ) ; call CODMO_update         (obj%CODMO        )
   case ("COLOR"        ) ; call COLOR_update         (obj%COLOR        )
   case ("COMBINE"      ) ; call COMBINE_update       (obj%COMBINE      )
   case ("COMP"         ) ; call COMP_update          (obj%COMP         )
   case ("CTAN"         ) ; call CTAN_update          (obj%CTAN         )
   case ("DABRA"        ) ; call DABRA_update         (obj%DABRA        )
   case ("DBGAIN"       ) ; call DBGAIN_update        (obj%DBGAIN       )
   case ("DECON"        ) ; call DECON_update         (obj%DECON        )
   case ("DIST"         ) ; call DIST_update          (obj%DIST         )
   case ("DMAP3D"       ) ; call DMAP3D_update        (obj%DMAP3D       )
   case ("DMO3D"        ) ; call DMO3D_update         (obj%DMO3D        )
   case ("DMOPREP"      ) ; call DMOPREP_update       (obj%DMOPREP      )
   case ("DRC"          ) ; call DRC_update           (obj%DRC          )
   case ("DSIG"         ) ; call DSIG_update          (obj%DSIG         )
   case ("DWTF"         ) ; call DWTF_update          (obj%DWTF         )
   case ("DWTP"         ) ; call DWTP_update          (obj%DWTP         )
   case ("EAGC"         ) ; call EAGC_update          (obj%EAGC         )
   case ("EDA"          ) ; call EDA_update           (obj%EDA          )
   case ("EDA3D"        ) ; call EDA3D_update         (obj%EDA3D        )
   case ("TRACETERP3D"  ) ; call TRACETERP3D_update   (obj%TRACETERP3D  )
   case ("ELEV"         ) ; call ELEV_update          (obj%ELEV         )
   case ("EXO"          ) ; call EXO_update           (obj%EXO          )
   case ("EZCHECK"      ) ; call EZCHECK_update       (obj%EZCHECK      )
   case ("FBAL"         ) ; call FBAL_update          (obj%FBAL         )
   case ("FFAVA"        ) ; call FFAVA_update         (obj%FFAVA        )
   case ("FGD"          ) ; call FGD_update           (obj%FGD          )
   case ("FGDREV"       ) ; call FGDREV_update        (obj%FGDREV       )
   case ("FILL"         ) ; call FILL_update          (obj%FILL         )
   case ("FILTP"        ) ; call FILTP_update         (obj%FILTP        )
   case ("FISH"         ) ; call FISH_update          (obj%FISH         )
   case ("FKAP"         ) ; call FKAP_update          (obj%FKAP         )
   case ("FKFILT"       ) ; call FKFILT_update        (obj%FKFILT       )
   case ("FKTMIG"       ) ; call FKTMIG_update        (obj%FKTMIG       )
   case ("FKTR"         ) ; call FKTR_update          (obj%FKTR         )
   case ("FLEXBIN"      ) ; call FLEXBIN_update       (obj%FLEXBIN      )
   case ("FXDECON"      ) ; call FXDECON_update       (obj%FXDECON      )
   case ("FXMIG"        ) ; call FXMIG_update         (obj%FXMIG        )
   case ("FXRYT"        ) ; call FXRYT_update         (obj%FXRYT        )
   case ("FXTI"         ) ; call FXTI_update          (obj%FXTI         )
   case ("FXYDECON"     ) ; call FXYDECON_update      (obj%FXYDECON     )
   case ("GATHER"       ) ; call GATHER_update        (obj%GATHER       )
   case ("GDIV"         ) ; call GDIV_update          (obj%GDIV         )
   case ("GENFILT"      ) ; call GENFILT_update       (obj%GENFILT      )
   case ("GRAB"         ) ; call GRAB_update          (obj%GRAB         )
   case ("GVS"          ) ; call GVS_update           (obj%GVS          )
   case ("HEADCHECK"    ) ; call HEADCHECK_update     (obj%HEADCHECK    )
   case ("HEADMAP"      ) ; call HEADMAP_update       (obj%HEADMAP      )
   case ("HEADSUM"      ) ; call HEADSUM_update       (obj%HEADSUM      )
   case ("HRZSTK"       ) ; call HRZSTK_update        (obj%HRZSTK       )
   case ("HSYN"         ) ; call HSYN_update          (obj%HSYN         )
   case ("HVEL"         ) ; call HVEL_update          (obj%HVEL         )
   case ("IBSMA"        ) ; call IBSMA_update         (obj%IBSMA        )
   case ("IMS"          ) ; call IMS_update           (obj%IMS          )
   case ("IQ"           ) ; call IQ_update            (obj%IQ           )
   case ("JOB_DATA"     ) ; call JOB_DATA_update      (obj%JOB_DATA     )
   case ("KA"           ) ; call KA_update            (obj%KA           )
   case ("KASTATS"      ) ; call KASTATS_update       (obj%KASTATS      )
   case ("KDMIG"        ) ; call KDMIG_update         (obj%KDMIG        )
   case ("KDMO"         ) ; call KDMO_update          (obj%KDMO         )
   case ("KMIG"         ) ; call KMIG_update          (obj%KMIG         )
   case ("KTMIG"        ) ; call KTMIG_update         (obj%KTMIG        )
   case ("LMRKHRZ"      ) ; call LMRKHRZ_update       (obj%LMRKHRZ      )
   case ("LMRKIN"       ) ; call LMRKIN_update        (obj%LMRKIN       )
   case ("LMRKOUT"      ) ; call LMRKOUT_update       (obj%LMRKOUT      )
   case ("MADC"         ) ; call MADC_update          (obj%MADC         )
   case ("MASKER"       ) ; call MASKER_update        (obj%MASKER       )
   case ("MCLINV"       ) ; call MCLINV_update        (obj%MCLINV       )
   case ("MDIP"         ) ; call MDIP_update          (obj%MDIP         )
   case ("MDS"          ) ; call MDS_update           (obj%MDS          )
   case ("MFRS"         ) ; call MFRS_update          (obj%MFRS         )
   case ("MGD"          ) ; call MGD_update           (obj%MGD          )
   case ("MODMO"        ) ; call MODMO_update         (obj%MODMO        )
   case ("MTFUN"        ) ; call MTFUN_update         (obj%MTFUN        )
   case ("MUTE"         ) ; call MUTE_update          (obj%MUTE         )
   case ("MVXP"         ) ; call MVXP_update          (obj%MVXP         )
   case ("MZPC"         ) ; call MZPC_update          (obj%MZPC         )
   case ("NMO"          ) ; call NMO_update           (obj%NMO          )
   case ("NORM"         ) ; call NORM_update          (obj%NORM         )
   case ("PAIRMERGE"    ) ; call PAIRMERGE_update     (obj%PAIRMERGE    )
   case ("PGPS"         ) ; call PGPS_update          (obj%PGPS         )
   case ("PH2OFF"       ) ; call PH2OFF_update        (obj%PH2OFF       )
   case ("PROJECT_DATA" ) ; call PROJECT_DATA_update  (obj%PROJECT_DATA )
   case ("PSLINV"       ) ; call PSLINV_update        (obj%PSLINV       )
   case ("PSTMIG"       ) ; call PSTMIG_update        (obj%PSTMIG       )
   case ("QEST"         ) ; call QEST_update          (obj%QEST         )
   case ("RANLINE"      ) ; call RANLINE_update       (obj%RANLINE      )
   case ("RCPOUT"       ) ; call RCPOUT_update        (obj%RCPOUT       )
   case ("REG"          ) ; call REG_update           (obj%REG          )
   case ("REGBIN"       ) ; call REGBIN_update        (obj%REGBIN       )
   case ("RES"          ) ; call RES_update           (obj%RES          )
   case ("RESTH"        ) ; call RESTH_update         (obj%RESTH        )
   case ("RFAB"         ) ; call RFAB_update          (obj%RFAB         )
   case ("RMUL"         ) ; call RMUL_update          (obj%RMUL         )
   case ("RNSYN"        ) ; call RNSYN_update         (obj%RNSYN        )
   case ("RTC"          ) ; call RTC_update           (obj%RTC          )
   case ("RYTOV"        ) ; call RYTOV_update         (obj%RYTOV        )
   case ("SCAB"         ) ; call SCAB_update          (obj%SCAB         )
   case ("SCALE"        ) ; call SCALE_update         (obj%SCALE        )
   case ("SCDECON"      ) ; call SCDECON_update       (obj%SCDECON      )
   case ("SDIP"         ) ; call SDIP_update          (obj%SDIP         )
   case ("SDIP3D"       ) ; call SDIP3D_update        (obj%SDIP3D       )
   case ("SELDMO"       ) ; call SELDMO_update        (obj%SELDMO       )
   case ("SELECT"       ) ; call SELECT_update        (obj%SELECT       )
   case ("SETMUTE"      ) ; call SETMUTE_update       (obj%SETMUTE      )
   case ("SETPOLY"      ) ; call SETPOLY_update       (obj%SETPOLY      )
   case ("SETWORD"      ) ; call SETWORD_update       (obj%SETWORD      )
   case ("SHFT"         ) ; call SHFT_update          (obj%SHFT         )
   case ("SISC"         ) ; call SISC_update          (obj%SISC         )
   case ("SLAB"         ) ; call SLAB_update          (obj%SLAB         )
   case ("SLICE"        ) ; call SLICE_update         (obj%SLICE        )
   case ("SLICER"       ) ; call SLICER_update        (obj%SLICER       )
   case ("SLST"         ) ; call SLST_update          (obj%SLST         )
   case ("SPCT"         ) ; call SPCT_update          (obj%SPCT         )
   case ("SPIKE"        ) ; call SPIKE_update         (obj%SPIKE        )
   case ("SPLT"         ) ; call SPLT_update          (obj%SPLT         )
   case ("SPTI"         ) ; call SPTI_update          (obj%SPTI         )
   case ("STK"          ) ; call STK_update           (obj%STK          )
   case ("STRETCH"      ) ; call STRETCH_update       (obj%STRETCH      )
   case ("SVA"          ) ; call SVA_update           (obj%SVA          )
   case ("SYNBP"        ) ; call SYNBP_update         (obj%SYNBP        )
   case ("TABLESAVE"    ) ; call TABLESAVE_update     (obj%TABLESAVE    )
   case ("TABLESORT"    ) ; call TABLESORT_update     (obj%TABLESORT    )
   case ("TDC"          ) ; call TDC_update           (obj%TDC          )
   case ("TDMP"         ) ; call TDMP_update          (obj%TDMP         )
   case ("TELAV"        ) ; call TELAV_update         (obj%TELAV        )
   case ("TFATT"        ) ; call TFATT_update         (obj%TFATT        )
   case ("TPOW"         ) ; call TPOW_update          (obj%TPOW         )
   case ("TREDIT"       ) ; call TREDIT_update        (obj%TREDIT       )
   case ("TRIN"         ) ; call TRIN_update          (obj%TRIN         )
   case ("TRINSORT"     ) ; call TRINSORT_update      (obj%TRINSORT     )
   case ("TRMO"         ) ; call TRMO_update          (obj%TRMO         )
   case ("TROT"         ) ; call TROT_update          (obj%TROT         )
   case ("TRSTATS"      ) ; call TRSTATS_update       (obj%TRSTATS      )
   case ("TSEL"         ) ; call TSEL_update          (obj%TSEL         )
   case ("TSLC"         ) ; call TSLC_update          (obj%TSLC         )
   case ("TSMUTE"       ) ; call TSMUTE_update        (obj%TSMUTE       )
   case ("TSORT"        ) ; call TSORT_update         (obj%TSORT        )
   case ("PARALLELSORT"        ) ; call PARALLELSORT_update         (obj%PARALLELSORT        )
   case ("TSVF"         ) ; call TSVF_update          (obj%TSVF         )
   case ("TTMO"         ) ; call TTMO_update          (obj%TTMO         )
   case ("TTRIN"        ) ; call TTRIN_update         (obj%TTRIN        )
   case ("TTROT"        ) ; call TTROT_update         (obj%TTROT        )
   case ("TVF"          ) ; call TVF_update           (obj%TVF          )
   case ("UNGATHER"     ) ; call UNGATHER_update      (obj%UNGATHER     )
   case ("UTEL"         ) ; call UTEL_update          (obj%UTEL         )
   case ("VC"           ) ; call VC_update            (obj%VC           )
   case ("VELEDIT"      ) ; call VELEDIT_update       (obj%VELEDIT      )
   case ("VPICK"        ) ; call VPICK_update         (obj%VPICK        )
   case ("VTRIM"        ) ; call VTRIM_update         (obj%VTRIM        )
   case ("WSF"          ) ; call WSF_update           (obj%WSF          )
   case ("XP"           ) ; call XP_update            (obj%XP           )

      case default ; call pc_error ("Invalid process",obj%name)
      end select

      end subroutine super_update


!!------------------------ get rcs ident -------------------------------!!
!!------------------------ get rcs ident -------------------------------!!
!!------------------------ get rcs ident -------------------------------!!


      subroutine super_get_rcs_ident (name,ident)
      implicit none
      character(len=*)  ,intent(in)    :: name               !argument
      character(len=*)  ,intent(out)   :: ident              !argument

      select case (name)

   case ("ABAL"         ) ; ident = ABAL_ident
   case ("ABRA"         ) ; ident = ABRA_ident
   case ("ACORR"        ) ; ident = ACORR_ident
   case ("ADNS"         ) ; ident = ADNS_ident
   case ("ADPSUB"       ) ; ident = ADPSUB_ident
   case ("ALAMO"        ) ; ident = ALAMO_ident
   case ("AMPDG"        ) ; ident = AMPDG_ident
   case ("AVAGRAD"      ) ; ident = AVAGRAD_ident
   case ("AVAST"        ) ; ident = AVAST_ident
   case ("AVOANS"       ) ; ident = AVOANS_ident
   case ("AVOSTS"       ) ; ident = AVOSTS_ident
   case ("AVOVAN"       ) ; ident = AVOVAN_ident
   case ("AVOVIT"       ) ; ident = AVOVIT_ident
   case ("BUNCH"        ) ; ident = BUNCH_ident
   case ("C4WE"         ) ; ident = C4WE_ident
   case ("CC3D"         ) ; ident = CC3D_ident
   case ("CFDS"         ) ; ident = CFDS_ident
   case ("CHART"        ) ; ident = CHART_ident
   case ("CLEANUP"      ) ; ident = CLEANUP_ident
   case ("CLIP"         ) ; ident = CLIP_ident
   case ("CNEARTS"      ) ; ident = CNEARTS_ident
   case ("CODMO"        ) ; ident = CODMO_ident
   case ("COLOR"        ) ; ident = COLOR_ident
   case ("COMBINE"      ) ; ident = COMBINE_ident
   case ("COMP"         ) ; ident = COMP_ident
   case ("CTAN"         ) ; ident = CTAN_ident
   case ("DABRA"        ) ; ident = DABRA_ident
   case ("DBGAIN"       ) ; ident = DBGAIN_ident
   case ("DECON"        ) ; ident = DECON_ident
   case ("DIST"         ) ; ident = DIST_ident
   case ("DMAP3D"       ) ; ident = DMAP3D_ident
   case ("DMO3D"        ) ; ident = DMO3D_ident
   case ("DMOPREP"      ) ; ident = DMOPREP_ident
   case ("DRC"          ) ; ident = DRC_ident
   case ("DSIG"         ) ; ident = DSIG_ident
   case ("DWTF"         ) ; ident = DWTF_ident
   case ("DWTP"         ) ; ident = DWTP_ident
   case ("EAGC"         ) ; ident = EAGC_ident
   case ("EDA"          ) ; ident = EDA_ident
   case ("EDA3D"        ) ; ident = EDA3D_ident
   case ("TRACETERP3D"  ) ; ident = TRACETERP3D_ident
   case ("ELEV"         ) ; ident = ELEV_ident
   case ("EXO"          ) ; ident = EXO_ident
   case ("EZCHECK"      ) ; ident = EZCHECK_ident
   case ("FBAL"         ) ; ident = FBAL_ident
   case ("FFAVA"        ) ; ident = FFAVA_ident
   case ("FGD"          ) ; ident = FGD_ident
   case ("FGDREV"       ) ; ident = FGDREV_ident
   case ("FILL"         ) ; ident = FILL_ident
   case ("FILTP"        ) ; ident = FILTP_ident
   case ("FISH"         ) ; ident = FISH_ident
   case ("FKAP"         ) ; ident = FKAP_ident
   case ("FKFILT"       ) ; ident = FKFILT_ident
   case ("FKTMIG"       ) ; ident = FKTMIG_ident
   case ("FKTR"         ) ; ident = FKTR_ident
   case ("FLEXBIN"      ) ; ident = FLEXBIN_ident
   case ("FXDECON"      ) ; ident = FXDECON_ident
   case ("FXMIG"        ) ; ident = FXMIG_ident
   case ("FXRYT"        ) ; ident = FXRYT_ident
   case ("FXTI"         ) ; ident = FXTI_ident
   case ("FXYDECON"     ) ; ident = FXYDECON_ident
   case ("GATHER"       ) ; ident = GATHER_ident
   case ("GDIV"         ) ; ident = GDIV_ident
   case ("GENFILT"      ) ; ident = GENFILT_ident
   case ("GRAB"         ) ; ident = GRAB_ident
   case ("GVS"          ) ; ident = GVS_ident
   case ("HEADCHECK"    ) ; ident = HEADCHECK_ident
   case ("HEADMAP"      ) ; ident = HEADMAP_ident
   case ("HEADSUM"      ) ; ident = HEADSUM_ident
   case ("HRZSTK"       ) ; ident = HRZSTK_ident
   case ("HSYN"         ) ; ident = HSYN_ident
   case ("HVEL"         ) ; ident = HVEL_ident
   case ("IBSMA"        ) ; ident = IBSMA_ident
   case ("IMS"          ) ; ident = IMS_ident
   case ("IQ"           ) ; ident = IQ_ident
   case ("JOB_DATA"     ) ; ident = JOB_DATA_ident
   case ("KA"           ) ; ident = KA_ident
   case ("KASTATS"      ) ; ident = KASTATS_ident
   case ("KDMIG"        ) ; ident = KDMIG_ident
   case ("KDMO"         ) ; ident = KDMO_ident
   case ("KMIG"         ) ; ident = KMIG_ident
   case ("KTMIG"        ) ; ident = KTMIG_ident
   case ("LMRKHRZ"      ) ; ident = LMRKHRZ_ident
   case ("LMRKIN"       ) ; ident = LMRKIN_ident
   case ("LMRKOUT"      ) ; ident = LMRKOUT_ident
   case ("MADC"         ) ; ident = MADC_ident
   case ("MASKER"       ) ; ident = MASKER_ident
   case ("MCLINV"       ) ; ident = MCLINV_ident
   case ("MDIP"         ) ; ident = MDIP_ident
   case ("MDS"          ) ; ident = MDS_ident
   case ("MFRS"         ) ; ident = MFRS_ident
   case ("MGD"          ) ; ident = MGD_ident
   case ("MODMO"        ) ; ident = MODMO_ident
   case ("MTFUN"        ) ; ident = MTFUN_ident
   case ("MUTE"         ) ; ident = MUTE_ident
   case ("MVXP"         ) ; ident = MVXP_ident
   case ("MZPC"         ) ; ident = MZPC_ident
   case ("NMO"          ) ; ident = NMO_ident
   case ("NORM"         ) ; ident = NORM_ident
   case ("PAIRMERGE"    ) ; ident = PAIRMERGE_ident
   case ("PGPS"         ) ; ident = PGPS_ident
   case ("PH2OFF"       ) ; ident = PH2OFF_ident
   case ("PROJECT_DATA" ) ; ident = PROJECT_DATA_ident
   case ("PSLINV"       ) ; ident = PSLINV_ident
   case ("PSTMIG"       ) ; ident = PSTMIG_ident
   case ("QEST"         ) ; ident = QEST_ident
   case ("RANLINE"      ) ; ident = RANLINE_ident
   case ("RCPOUT"       ) ; ident = RCPOUT_ident
   case ("REG"          ) ; ident = REG_ident
   case ("REGBIN"       ) ; ident = REGBIN_ident
   case ("RES"          ) ; ident = RES_ident
   case ("RESTH"        ) ; ident = RESTH_ident
   case ("RFAB"         ) ; ident = RFAB_ident
   case ("RMUL"         ) ; ident = RMUL_ident
   case ("RNSYN"        ) ; ident = RNSYN_ident
   case ("RTC"          ) ; ident = RTC_ident
   case ("RYTOV"        ) ; ident = RYTOV_ident
   case ("SCAB"         ) ; ident = SCAB_ident
   case ("SCALE"        ) ; ident = SCALE_ident
   case ("SCDECON"      ) ; ident = SCDECON_ident
   case ("SDIP"         ) ; ident = SDIP_ident
   case ("SDIP3D"       ) ; ident = SDIP3D_ident
   case ("SELDMO"       ) ; ident = SELDMO_ident
   case ("SELECT"       ) ; ident = SELECT_ident
   case ("SETMUTE"      ) ; ident = SETMUTE_ident
   case ("SETPOLY"      ) ; ident = SETPOLY_ident
   case ("SETWORD"      ) ; ident = SETWORD_ident
   case ("SHFT"         ) ; ident = SHFT_ident
   case ("SISC"         ) ; ident = SISC_ident
   case ("SLAB"         ) ; ident = SLAB_ident
   case ("SLICE"        ) ; ident = SLICE_ident
   case ("SLICER"       ) ; ident = SLICER_ident
   case ("SLST"         ) ; ident = SLST_ident
   case ("SPCT"         ) ; ident = SPCT_ident
   case ("SPIKE"        ) ; ident = SPIKE_ident
   case ("SPLT"         ) ; ident = SPLT_ident
   case ("SPTI"         ) ; ident = SPTI_ident
   case ("STK"          ) ; ident = STK_ident
   case ("STRETCH"      ) ; ident = STRETCH_ident
   case ("SVA"          ) ; ident = SVA_ident
   case ("SYNBP"        ) ; ident = SYNBP_ident
   case ("TABLESAVE"    ) ; ident = TABLESAVE_ident
   case ("TABLESORT"    ) ; ident = TABLESORT_ident
   case ("TDC"          ) ; ident = TDC_ident
   case ("TDMP"         ) ; ident = TDMP_ident
   case ("TELAV"        ) ; ident = TELAV_ident
   case ("TFATT"        ) ; ident = TFATT_ident
   case ("TPOW"         ) ; ident = TPOW_ident
   case ("TREDIT"       ) ; ident = TREDIT_ident
   case ("TRIN"         ) ; ident = TRIN_ident
   case ("TRINSORT"     ) ; ident = TRINSORT_ident
   case ("TRMO"         ) ; ident = TRMO_ident
   case ("TROT"         ) ; ident = TROT_ident
   case ("TRSTATS"      ) ; ident = TRSTATS_ident
   case ("TSEL"         ) ; ident = TSEL_ident
   case ("TSLC"         ) ; ident = TSLC_ident
   case ("TSMUTE"       ) ; ident = TSMUTE_ident
   case ("TSORT"        ) ; ident = TSORT_ident
   case ("PARALLELSORT"        ) ; ident = PARALLELSORT_ident
   case ("TSVF"         ) ; ident = TSVF_ident
   case ("TTMO"         ) ; ident = TTMO_ident
   case ("TTRIN"        ) ; ident = TTRIN_ident
   case ("TTROT"        ) ; ident = TTROT_ident
   case ("TVF"          ) ; ident = TVF_ident
   case ("UNGATHER"     ) ; ident = UNGATHER_ident
   case ("UTEL"         ) ; ident = UTEL_ident
   case ("VC"           ) ; ident = VC_ident
   case ("VELEDIT"      ) ; ident = VELEDIT_ident
   case ("VPICK"        ) ; ident = VPICK_ident
   case ("VTRIM"        ) ; ident = VTRIM_ident
   case ("WSF"          ) ; ident = WSF_ident
   case ("XP"           ) ; ident = XP_ident

      case default ; print *, "Invalid process "//trim(name)
      end select

      end subroutine super_get_rcs_ident


!!----------------------------- wrapup ---------------------------------!!
!!----------------------------- wrapup ---------------------------------!!
!!----------------------------- wrapup ---------------------------------!!


      subroutine super_wrapup (obj)
      implicit none
      type(super_struct),pointer       :: obj                !argument


      select case (obj%name)

   case ("ABAL"         ) ; call ABAL_wrapup          (obj%ABAL         )
   case ("ABRA"         ) ; call ABRA_wrapup          (obj%ABRA         )
   case ("ACORR"        ) ; call ACORR_wrapup         (obj%ACORR        )
   case ("ADNS"         ) ; call ADNS_wrapup          (obj%ADNS         )
   case ("ADPSUB"       ) ; call ADPSUB_wrapup        (obj%ADPSUB       )
   case ("ALAMO"        ) ; call ALAMO_wrapup         (obj%ALAMO        )
   case ("AMPDG"        ) ; call AMPDG_wrapup         (obj%AMPDG        )
   case ("AVAGRAD"      ) ; call AVAGRAD_wrapup       (obj%AVAGRAD      )
   case ("AVAST"        ) ; call AVAST_wrapup         (obj%AVAST        )
   case ("AVOANS"       ) ; call AVOANS_wrapup        (obj%AVOANS       )
   case ("AVOSTS"       ) ; call AVOSTS_wrapup        (obj%AVOSTS       )
   case ("AVOVAN"       ) ; call AVOVAN_wrapup        (obj%AVOVAN       )
   case ("AVOVIT"       ) ; call AVOVIT_wrapup        (obj%AVOVIT       )
   case ("BUNCH"        ) ; call BUNCH_wrapup         (obj%BUNCH        )
   case ("C4WE"         ) ; call C4WE_wrapup          (obj%C4WE         )
   case ("CC3D"         ) ; call CC3D_wrapup          (obj%CC3D         )
   case ("CFDS"         ) ; call CFDS_wrapup          (obj%CFDS         )
   case ("CHART"        ) ; call CHART_wrapup         (obj%CHART        )
   case ("CLEANUP"      ) ; call CLEANUP_wrapup       (obj%CLEANUP      )
   case ("CLIP"         ) ; call CLIP_wrapup          (obj%CLIP         )
   case ("CNEARTS"      ) ; call CNEARTS_wrapup       (obj%CNEARTS      )
   case ("CODMO"        ) ; call CODMO_wrapup         (obj%CODMO        )
   case ("COLOR"        ) ; call COLOR_wrapup         (obj%COLOR        )
   case ("COMBINE"      ) ; call COMBINE_wrapup       (obj%COMBINE      )
   case ("COMP"         ) ; call COMP_wrapup          (obj%COMP         )
   case ("CTAN"         ) ; call CTAN_wrapup          (obj%CTAN         )
   case ("DABRA"        ) ; call DABRA_wrapup         (obj%DABRA        )
   case ("DBGAIN"       ) ; call DBGAIN_wrapup        (obj%DBGAIN       )
   case ("DECON"        ) ; call DECON_wrapup         (obj%DECON        )
   case ("DIST"         ) ; call DIST_wrapup          (obj%DIST         )
   case ("DMAP3D"       ) ; call DMAP3D_wrapup        (obj%DMAP3D       )
   case ("DMO3D"        ) ; call DMO3D_wrapup         (obj%DMO3D        )
   case ("DMOPREP"      ) ; call DMOPREP_wrapup       (obj%DMOPREP      )
   case ("DRC"          ) ; call DRC_wrapup           (obj%DRC          )
   case ("DSIG"         ) ; call DSIG_wrapup          (obj%DSIG         )
   case ("DWTF"         ) ; call DWTF_wrapup          (obj%DWTF         )
   case ("DWTP"         ) ; call DWTP_wrapup          (obj%DWTP         )
   case ("EAGC"         ) ; call EAGC_wrapup          (obj%EAGC         )
   case ("EDA"          ) ; call EDA_wrapup           (obj%EDA          )
   case ("EDA3D"        ) ; call EDA3D_wrapup         (obj%EDA3D        )
   case ("TRACETERP3D"  ) ; call TRACETERP3D_wrapup   (obj%TRACETERP3D  )
   case ("ELEV"         ) ; call ELEV_wrapup          (obj%ELEV         )
   case ("EXO"          ) ; call EXO_wrapup           (obj%EXO          )
   case ("EZCHECK"      ) ; call EZCHECK_wrapup       (obj%EZCHECK      )
   case ("FBAL"         ) ; call FBAL_wrapup          (obj%FBAL         )
   case ("FFAVA"        ) ; call FFAVA_wrapup         (obj%FFAVA        )
   case ("FGD"          ) ; call FGD_wrapup           (obj%FGD          )
   case ("FGDREV"       ) ; call FGDREV_wrapup        (obj%FGDREV       )
   case ("FILL"         ) ; call FILL_wrapup          (obj%FILL         )
   case ("FILTP"        ) ; call FILTP_wrapup         (obj%FILTP        )
   case ("FISH"         ) ; call FISH_wrapup          (obj%FISH         )
   case ("FKAP"         ) ; call FKAP_wrapup          (obj%FKAP         )
   case ("FKFILT"       ) ; call FKFILT_wrapup        (obj%FKFILT       )
   case ("FKTMIG"       ) ; call FKTMIG_wrapup        (obj%FKTMIG       )
   case ("FKTR"         ) ; call FKTR_wrapup          (obj%FKTR         )
   case ("FLEXBIN"      ) ; call FLEXBIN_wrapup       (obj%FLEXBIN      )
   case ("FXDECON"      ) ; call FXDECON_wrapup       (obj%FXDECON      )
   case ("FXMIG"        ) ; call FXMIG_wrapup         (obj%FXMIG        )
   case ("FXRYT"        ) ; call FXRYT_wrapup         (obj%FXRYT        )
   case ("FXTI"         ) ; call FXTI_wrapup          (obj%FXTI         )
   case ("FXYDECON"     ) ; call FXYDECON_wrapup      (obj%FXYDECON     )
   case ("GATHER"       ) ; call GATHER_wrapup        (obj%GATHER       )
   case ("GDIV"         ) ; call GDIV_wrapup          (obj%GDIV         )
   case ("GENFILT"      ) ; call GENFILT_wrapup       (obj%GENFILT      )
   case ("GRAB"         ) ; call GRAB_wrapup          (obj%GRAB         )
   case ("GVS"          ) ; call GVS_wrapup           (obj%GVS          )
   case ("HEADCHECK"    ) ; call HEADCHECK_wrapup     (obj%HEADCHECK    )
   case ("HEADMAP"      ) ; call HEADMAP_wrapup       (obj%HEADMAP      )
   case ("HEADSUM"      ) ; call HEADSUM_wrapup       (obj%HEADSUM      )
   case ("HRZSTK"       ) ; call HRZSTK_wrapup        (obj%HRZSTK       )
   case ("HSYN"         ) ; call HSYN_wrapup          (obj%HSYN         )
   case ("HVEL"         ) ; call HVEL_wrapup          (obj%HVEL         )
   case ("IBSMA"        ) ; call IBSMA_wrapup         (obj%IBSMA        )
   case ("IMS"          ) ; call IMS_wrapup           (obj%IMS          )
   case ("IQ"           ) ; call IQ_wrapup            (obj%IQ           )
   case ("JOB_DATA"     ) ; call JOB_DATA_wrapup      (obj%JOB_DATA     )
   case ("KA"           ) ; call KA_wrapup            (obj%KA           )
   case ("KASTATS"      ) ; call KASTATS_wrapup       (obj%KASTATS      )
   case ("KDMIG"        ) ; call KDMIG_wrapup         (obj%KDMIG        )
   case ("KDMO"         ) ; call KDMO_wrapup          (obj%KDMO         )
   case ("KMIG"         ) ; call KMIG_wrapup          (obj%KMIG         )
   case ("KTMIG"        ) ; call KTMIG_wrapup         (obj%KTMIG        )
   case ("LMRKHRZ"      ) ; call LMRKHRZ_wrapup       (obj%LMRKHRZ      )
   case ("LMRKIN"       ) ; call LMRKIN_wrapup        (obj%LMRKIN       )
   case ("LMRKOUT"      ) ; call LMRKOUT_wrapup       (obj%LMRKOUT      )
   case ("MADC"         ) ; call MADC_wrapup          (obj%MADC         )
   case ("MASKER"       ) ; call MASKER_wrapup        (obj%MASKER       )
   case ("MCLINV"       ) ; call MCLINV_wrapup        (obj%MCLINV       )
   case ("MDIP"         ) ; call MDIP_wrapup          (obj%MDIP         )
   case ("MDS"          ) ; call MDS_wrapup           (obj%MDS          )
   case ("MFRS"         ) ; call MFRS_wrapup          (obj%MFRS         )
   case ("MGD"          ) ; call MGD_wrapup           (obj%MGD          )
   case ("MODMO"        ) ; call MODMO_wrapup         (obj%MODMO        )
   case ("MTFUN"        ) ; call MTFUN_wrapup         (obj%MTFUN        )
   case ("MUTE"         ) ; call MUTE_wrapup          (obj%MUTE         )
   case ("MVXP"         ) ; call MVXP_wrapup          (obj%MVXP         )
   case ("MZPC"         ) ; call MZPC_wrapup          (obj%MZPC         )
   case ("NMO"          ) ; call NMO_wrapup           (obj%NMO          )
   case ("NORM"         ) ; call NORM_wrapup          (obj%NORM         )
   case ("PAIRMERGE"    ) ; call PAIRMERGE_wrapup     (obj%PAIRMERGE    )
   case ("PGPS"         ) ; call PGPS_wrapup          (obj%PGPS         )
   case ("PH2OFF"       ) ; call PH2OFF_wrapup        (obj%PH2OFF       )
   case ("PROJECT_DATA" ) ; call PROJECT_DATA_wrapup  (obj%PROJECT_DATA )
   case ("PSLINV"       ) ; call PSLINV_wrapup        (obj%PSLINV       )
   case ("PSTMIG"       ) ; call PSTMIG_wrapup        (obj%PSTMIG       )
   case ("QEST"         ) ; call QEST_wrapup          (obj%QEST         )
   case ("RANLINE"      ) ; call RANLINE_wrapup       (obj%RANLINE      )
   case ("RCPOUT"       ) ; call RCPOUT_wrapup        (obj%RCPOUT       )
   case ("REG"          ) ; call REG_wrapup           (obj%REG          )
   case ("REGBIN"       ) ; call REGBIN_wrapup        (obj%REGBIN       )
   case ("RES"          ) ; call RES_wrapup           (obj%RES          )
   case ("RESTH"        ) ; call RESTH_wrapup         (obj%RESTH        )
   case ("RFAB"         ) ; call RFAB_wrapup          (obj%RFAB         )
   case ("RMUL"         ) ; call RMUL_wrapup          (obj%RMUL         )
   case ("RNSYN"        ) ; call RNSYN_wrapup         (obj%RNSYN        )
   case ("RTC"          ) ; call RTC_wrapup           (obj%RTC          )
   case ("RYTOV"        ) ; call RYTOV_wrapup         (obj%RYTOV        )
   case ("SCAB"         ) ; call SCAB_wrapup          (obj%SCAB         )
   case ("SCALE"        ) ; call SCALE_wrapup         (obj%SCALE        )
   case ("SCDECON"      ) ; call SCDECON_wrapup       (obj%SCDECON      )
   case ("SDIP"         ) ; call SDIP_wrapup          (obj%SDIP         )
   case ("SDIP3D"       ) ; call SDIP3D_wrapup        (obj%SDIP3D       )
   case ("SELDMO"       ) ; call SELDMO_wrapup        (obj%SELDMO       )
   case ("SELECT"       ) ; call SELECT_wrapup        (obj%SELECT       )
   case ("SETMUTE"      ) ; call SETMUTE_wrapup       (obj%SETMUTE      )
   case ("SETPOLY"      ) ; call SETPOLY_wrapup       (obj%SETPOLY      )
   case ("SETWORD"      ) ; call SETWORD_wrapup       (obj%SETWORD      )
   case ("SHFT"         ) ; call SHFT_wrapup          (obj%SHFT         )
   case ("SISC"         ) ; call SISC_wrapup          (obj%SISC         )
   case ("SLAB"         ) ; call SLAB_wrapup          (obj%SLAB         )
   case ("SLICE"        ) ; call SLICE_wrapup         (obj%SLICE        )
   case ("SLICER"       ) ; call SLICER_wrapup        (obj%SLICER       )
   case ("SLST"         ) ; call SLST_wrapup          (obj%SLST         )
   case ("SPCT"         ) ; call SPCT_wrapup          (obj%SPCT         )
   case ("SPIKE"        ) ; call SPIKE_wrapup         (obj%SPIKE        )
   case ("SPLT"         ) ; call SPLT_wrapup          (obj%SPLT         )
   case ("SPTI"         ) ; call SPTI_wrapup          (obj%SPTI         )
   case ("STK"          ) ; call STK_wrapup           (obj%STK          )
   case ("STRETCH"      ) ; call STRETCH_wrapup       (obj%STRETCH      )
   case ("SVA"          ) ; call SVA_wrapup           (obj%SVA          )
   case ("SYNBP"        ) ; call SYNBP_wrapup         (obj%SYNBP        )
   case ("TABLESAVE"    ) ; call TABLESAVE_wrapup     (obj%TABLESAVE    )
   case ("TABLESORT"    ) ; call TABLESORT_wrapup     (obj%TABLESORT    )
   case ("TDC"          ) ; call TDC_wrapup           (obj%TDC          )
   case ("TDMP"         ) ; call TDMP_wrapup          (obj%TDMP         )
   case ("TELAV"        ) ; call TELAV_wrapup         (obj%TELAV        )
   case ("TFATT"        ) ; call TFATT_wrapup         (obj%TFATT        )
   case ("TPOW"         ) ; call TPOW_wrapup          (obj%TPOW         )
   case ("TREDIT"       ) ; call TREDIT_wrapup        (obj%TREDIT       )
   case ("TRIN"         ) ; call TRIN_wrapup          (obj%TRIN         )
   case ("TRINSORT"     ) ; call TRINSORT_wrapup      (obj%TRINSORT     )
   case ("TRMO"         ) ; call TRMO_wrapup          (obj%TRMO         )
   case ("TROT"         ) ; call TROT_wrapup          (obj%TROT         )
   case ("TRSTATS"      ) ; call TRSTATS_wrapup       (obj%TRSTATS      )
   case ("TSEL"         ) ; call TSEL_wrapup          (obj%TSEL         )
   case ("TSLC"         ) ; call TSLC_wrapup          (obj%TSLC         )
   case ("TSMUTE"       ) ; call TSMUTE_wrapup        (obj%TSMUTE       )
   case ("TSORT"        ) ; call TSORT_wrapup         (obj%TSORT        )
   case ("PARALLELSORT"        ) ; call PARALLELSORT_wrapup         (obj%PARALLELSORT        )
   case ("TSVF"         ) ; call TSVF_wrapup          (obj%TSVF         )
   case ("TTMO"         ) ; call TTMO_wrapup          (obj%TTMO         )
   case ("TTRIN"        ) ; call TTRIN_wrapup         (obj%TTRIN        )
   case ("TTROT"        ) ; call TTROT_wrapup         (obj%TTROT        )
   case ("TVF"          ) ; call TVF_wrapup           (obj%TVF          )
   case ("UNGATHER"     ) ; call UNGATHER_wrapup      (obj%UNGATHER     )
   case ("UTEL"         ) ; call UTEL_wrapup          (obj%UTEL         )
   case ("VC"           ) ; call VC_wrapup            (obj%VC           )
   case ("VELEDIT"      ) ; call VELEDIT_wrapup       (obj%VELEDIT      )
   case ("VPICK"        ) ; call VPICK_wrapup         (obj%VPICK        )
   case ("VTRIM"        ) ; call VTRIM_wrapup         (obj%VTRIM        )
   case ("WSF"          ) ; call WSF_wrapup           (obj%WSF          )
   case ("XP"           ) ; call XP_wrapup            (obj%XP           )

      case default ; call pc_error ("Invalid process",obj%name)
      end select

      end subroutine super_wrapup


!!----------------------------- oneset ---------------------------------!!
!!----------------------------- oneset ---------------------------------!!
!!----------------------------- oneset ---------------------------------!!


      subroutine super_oneset (obj,ntr,hd,tr)
      implicit none
      type(super_struct),pointer       :: obj               ! arguments
      integer           ,intent(inout) :: ntr               ! arguments
      double precision  ,intent(inout) :: hd(:,:)           ! arguments
      real              ,intent(inout) :: tr(:,:)           ! arguments

      select case (obj%name)

  case ("ABAL"         ) ; call ABAL          (obj%ABAL         ,ntr,hd,tr)
  case ("ABRA"         ) ; call ABRA          (obj%ABRA         ,ntr,hd,tr)
  case ("ACORR"        ) ; call ACORR         (obj%ACORR        ,ntr,hd,tr)
  case ("ADNS"         ) ; call ADNS          (obj%ADNS         ,ntr,hd,tr)
  case ("ADPSUB"       ) ; call ADPSUB        (obj%ADPSUB       ,ntr,hd,tr)
  case ("ALAMO"        ) ; call ALAMO         (obj%ALAMO        ,ntr,hd,tr)
  case ("AMPDG"        ) ; call AMPDG         (obj%AMPDG        ,ntr,hd,tr)
  case ("AVAGRAD"      ) ; call AVAGRAD       (obj%AVAGRAD      ,ntr,hd,tr)
  case ("AVAST"        ) ; call AVAST         (obj%AVAST        ,ntr,hd,tr)
  case ("AVOANS"       ) ; call AVOANS        (obj%AVOANS       ,ntr,hd,tr)
  case ("AVOSTS"       ) ; call AVOSTS        (obj%AVOSTS       ,ntr,hd,tr)
  case ("AVOVAN"       ) ; call AVOVAN        (obj%AVOVAN       ,ntr,hd,tr)
  case ("AVOVIT"       ) ; call AVOVIT        (obj%AVOVIT       ,ntr,hd,tr)
  case ("BUNCH"        ) ; call BUNCH         (obj%BUNCH        ,ntr,hd,tr)
  case ("C4WE"         ) ; call C4WE          (obj%C4WE         ,ntr,hd,tr)
  case ("CC3D"         ) ; call CC3D          (obj%CC3D         ,ntr,hd,tr)
  case ("CFDS"         ) ; call CFDS          (obj%CFDS         ,ntr,hd,tr)
  case ("CHART"        ) ; call CHART         (obj%CHART        ,ntr,hd,tr)
  case ("CLEANUP"      ) ;                  continue
  case ("CLIP"         ) ; call CLIP          (obj%CLIP         ,ntr,hd,tr)
  case ("CNEARTS"      ) ;                  continue
  case ("CODMO"        ) ;                  continue
  case ("COLOR"        ) ; call COLOR         (obj%COLOR        ,ntr,hd,tr)
  case ("COMBINE"      ) ; call COMBINE       (obj%COMBINE      ,ntr,hd,tr)
  case ("COMP"         ) ;                  continue
  case ("CTAN"         ) ; call CTAN          (obj%CTAN         ,ntr,hd,tr)
  case ("DABRA"        ) ; call DABRA         (obj%DABRA        ,ntr,hd,tr)
  case ("DBGAIN"       ) ; call DBGAIN        (obj%DBGAIN       ,ntr,hd,tr)
  case ("DECON"        ) ; call DECON         (obj%DECON        ,ntr,hd,tr)
  case ("DIST"         ) ; call DIST          (obj%DIST         ,ntr,hd,tr)
  case ("DMAP3D"       ) ; call DMAP3D        (obj%DMAP3D       ,ntr,hd,tr)
  case ("DMO3D"        ) ; call DMO3D         (obj%DMO3D        ,ntr,hd,tr)
  case ("DMOPREP"      ) ;                  continue
  case ("DRC"          ) ; call DRC           (obj%DRC          ,ntr,hd,tr)
  case ("DSIG"         ) ; call DSIG          (obj%DSIG         ,ntr,hd,tr)
  case ("DWTF"         ) ; call DWTF          (obj%DWTF         ,ntr,hd,tr)
  case ("DWTP"         ) ;                  continue
  case ("EAGC"         ) ; call EAGC          (obj%EAGC         ,ntr,hd,tr)
  case ("EDA"          ) ;                  continue
  case ("EDA3D"        ) ;                  continue
  case ("TRACETERP3D"  ) ;                  continue
  case ("ELEV"         ) ; call ELEV          (obj%ELEV         ,ntr,hd,tr)
  case ("EXO"          ) ; call EXO           (obj%EXO          ,ntr,hd,tr)
  case ("EZCHECK"      ) ; call EZCHECK       (obj%EZCHECK      ,ntr,hd,tr)
  case ("FBAL"         ) ; call FBAL          (obj%FBAL         ,ntr,hd,tr)
  case ("FFAVA"        ) ; call FFAVA         (obj%FFAVA        ,ntr,hd,tr)
  case ("FGD"          ) ; call FGD           (obj%FGD          ,ntr,hd,tr)
  case ("FGDREV"       ) ; call FGDREV        (obj%FGDREV       ,ntr,hd,tr)
  case ("FILL"         ) ;                  continue
  case ("FILTP"        ) ; call FILTP         (obj%FILTP        ,ntr,hd,tr)
  case ("FISH"         ) ; call FISH          (obj%FISH         ,ntr,hd,tr)
  case ("FKAP"         ) ; call FKAP          (obj%FKAP         ,ntr,hd,tr)
  case ("FKFILT"       ) ;                  continue
  case ("FKTMIG"       ) ; call FKTMIG        (obj%FKTMIG       ,ntr,hd,tr)
  case ("FKTR"         ) ;                  continue
  case ("FLEXBIN"      ) ; call FLEXBIN       (obj%FLEXBIN      ,ntr,hd,tr)
  case ("FXDECON"      ) ; call FXDECON       (obj%FXDECON      ,ntr,hd,tr)
  case ("FXMIG"        ) ; call FXMIG         (obj%FXMIG        ,ntr,hd,tr)
  case ("FXRYT"        ) ;                  continue
  case ("FXTI"         ) ;                  continue
  case ("FXYDECON"     ) ; call FXYDECON      (obj%FXYDECON     ,ntr,hd,tr)
  case ("GATHER"       ) ;                  continue
  case ("GDIV"         ) ; call GDIV          (obj%GDIV         ,ntr,hd,tr)
  case ("GENFILT"      ) ; call GENFILT       (obj%GENFILT      ,ntr,hd,tr)
  case ("GRAB"         ) ; call GRAB          (obj%GRAB         ,ntr,hd,tr)
  case ("GVS"          ) ; call GVS           (obj%GVS          ,ntr,hd,tr)
  case ("HEADCHECK"    ) ; call HEADCHECK     (obj%HEADCHECK    ,ntr,hd,tr)
  case ("HEADMAP"      ) ; call HEADMAP       (obj%HEADMAP      ,ntr,hd,tr)
  case ("HEADSUM"      ) ; call HEADSUM       (obj%HEADSUM      ,ntr,hd,tr)
  case ("HRZSTK"       ) ; call HRZSTK        (obj%HRZSTK       ,ntr,hd,tr)
  case ("HSYN"         ) ; call HSYN          (obj%HSYN         ,ntr,hd,tr)
  case ("HVEL"         ) ; call HVEL          (obj%HVEL         ,ntr,hd,tr)
  case ("IBSMA"        ) ; call IBSMA         (obj%IBSMA        ,ntr,hd,tr)
  case ("IMS"          ) ; call IMS           (obj%IMS          ,ntr,hd,tr)
  case ("IQ"           ) ; call IQ            (obj%IQ           ,ntr,hd,tr)
  case ("JOB_DATA"     ) ;                  continue
  case ("KA"           ) ; call KA            (obj%KA           ,ntr,hd,tr)
  case ("KASTATS"      ) ; call KASTATS       (obj%KASTATS      ,ntr,hd,tr)
  case ("KDMIG"        ) ; call KDMIG         (obj%KDMIG        ,ntr,hd,tr)
  case ("KDMO"         ) ; call KDMO          (obj%KDMO         ,ntr,hd,tr)
  case ("KMIG"         ) ; call KMIG          (obj%KMIG         ,ntr,hd,tr)
  case ("KTMIG"        ) ; call KTMIG         (obj%KTMIG        ,ntr,hd,tr)
  case ("LMRKHRZ"      ) ; call LMRKHRZ       (obj%LMRKHRZ      ,ntr,hd,tr)
  case ("LMRKIN"       ) ; call LMRKIN        (obj%LMRKIN       ,ntr,hd,tr)
  case ("LMRKOUT"      ) ; call LMRKOUT       (obj%LMRKOUT      ,ntr,hd,tr)
  case ("MADC"         ) ; call MADC          (obj%MADC         ,ntr,hd,tr)
  case ("MASKER"       ) ; call MASKER        (obj%MASKER       ,ntr,hd,tr)
  case ("MCLINV"       ) ; call MCLINV        (obj%MCLINV       ,ntr,hd,tr)
  case ("MDIP"         ) ; call MDIP          (obj%MDIP         ,ntr,hd,tr)
  case ("MDS"          ) ; call MDS           (obj%MDS          ,ntr,hd,tr)
  case ("MFRS"         ) ;                  continue
  case ("MGD"          ) ;                  continue
  case ("MODMO"        ) ; call MODMO         (obj%MODMO        ,ntr,hd,tr)
  case ("MTFUN"        ) ;                  continue
  case ("MUTE"         ) ; call MUTE          (obj%MUTE         ,ntr,hd,tr)
  case ("MVXP"         ) ; call MVXP          (obj%MVXP         ,ntr,hd,tr)
  case ("MZPC"         ) ; call MZPC          (obj%MZPC         ,ntr,hd,tr)
  case ("NMO"          ) ; call NMO           (obj%NMO          ,ntr,hd,tr)
  case ("NORM"         ) ; call NORM          (obj%NORM         ,ntr,hd,tr)
  case ("PAIRMERGE"    ) ; call PAIRMERGE     (obj%PAIRMERGE    ,ntr,hd,tr)
  case ("PGPS"         ) ;                  continue
  case ("PH2OFF"       ) ; call PH2OFF        (obj%PH2OFF       ,ntr,hd,tr)
  case ("PROJECT_DATA" ) ;                  continue
  case ("PSLINV"       ) ; call PSLINV        (obj%PSLINV       ,ntr,hd,tr)
  case ("PSTMIG"       ) ; call PSTMIG        (obj%PSTMIG       ,ntr,hd,tr)
  case ("QEST"         ) ; call QEST          (obj%QEST         ,ntr,hd,tr)
  case ("RANLINE"      ) ; call RANLINE       (obj%RANLINE      ,ntr,hd,tr)
  case ("RCPOUT"       ) ;                  continue
  case ("REG"          ) ;                  continue
  case ("REGBIN"       ) ;                  continue
  case ("RES"          ) ; call RES           (obj%RES          ,ntr,hd,tr)
  case ("RESTH"        ) ; call RESTH         (obj%RESTH        ,ntr,hd,tr)
  case ("RFAB"         ) ; call RFAB          (obj%RFAB         ,ntr,hd,tr)
  case ("RMUL"         ) ; call RMUL          (obj%RMUL         ,ntr,hd,tr)
  case ("RNSYN"        ) ; call RNSYN         (obj%RNSYN        ,ntr,hd,tr)
  case ("RTC"          ) ;                  continue
  case ("RYTOV"        ) ;                  continue
  case ("SCAB"         ) ; call SCAB          (obj%SCAB         ,ntr,hd,tr)
  case ("SCALE"        ) ; call SCALE         (obj%SCALE        ,ntr,hd,tr)
  case ("SCDECON"      ) ; call SCDECON       (obj%SCDECON      ,ntr,hd,tr)
  case ("SDIP"         ) ;                  continue
  case ("SDIP3D"       ) ;                  continue
  case ("SELDMO"       ) ; call SELDMO        (obj%SELDMO       ,ntr,hd,tr)
  case ("SELECT"       ) ; call SELECT        (obj%SELECT       ,ntr,hd,tr)
  case ("SETMUTE"      ) ; call SETMUTE       (obj%SETMUTE      ,ntr,hd,tr)
  case ("SETPOLY"      ) ; call SETPOLY       (obj%SETPOLY      ,ntr,hd,tr)
  case ("SETWORD"      ) ; call SETWORD       (obj%SETWORD      ,ntr,hd,tr)
  case ("SHFT"         ) ; call SHFT          (obj%SHFT         ,ntr,hd,tr)
  case ("SISC"         ) ; call SISC          (obj%SISC         ,ntr,hd,tr)
  case ("SLAB"         ) ; call SLAB          (obj%SLAB         ,ntr,hd,tr)
  case ("SLICE"        ) ; call SLICE         (obj%SLICE        ,ntr,hd,tr)
  case ("SLICER"       ) ; call SLICER        (obj%SLICER       ,ntr,hd,tr)
  case ("SLST"         ) ; call SLST          (obj%SLST         ,ntr,hd,tr)
  case ("SPCT"         ) ;                  continue
  case ("SPIKE"        ) ; call SPIKE         (obj%SPIKE        ,ntr,hd,tr)
  case ("SPLT"         ) ; call SPLT          (obj%SPLT         ,ntr,hd,tr)
  case ("SPTI"         ) ;                  continue
  case ("STK"          ) ;                  continue
  case ("STRETCH"      ) ; call STRETCH       (obj%STRETCH      ,ntr,hd,tr)
  case ("SVA"          ) ; call SVA           (obj%SVA          ,ntr,hd,tr)
  case ("SYNBP"        ) ; call SYNBP         (obj%SYNBP        ,ntr,hd,tr)
  case ("TABLESAVE"    ) ; call TABLESAVE     (obj%TABLESAVE    ,ntr,hd,tr)
  case ("TABLESORT"    ) ;                  continue
  case ("TDC"          ) ; call TDC           (obj%TDC          ,ntr,hd,tr)
  case ("TDMP"         ) ; call TDMP          (obj%TDMP         ,ntr,hd,tr)
  case ("TELAV"        ) ; call TELAV         (obj%TELAV        ,ntr,hd,tr)
  case ("TFATT"        ) ;                  continue
  case ("TPOW"         ) ; call TPOW          (obj%TPOW         ,ntr,hd,tr)
  case ("TREDIT"       ) ; call TREDIT        (obj%TREDIT       ,ntr,hd,tr)
  case ("TRIN"         ) ; call TRIN          (obj%TRIN         ,ntr,hd,tr)
  case ("TRINSORT"     ) ; call TRINSORT      (obj%TRINSORT     ,ntr,hd,tr)
  case ("TRMO"         ) ; call TRMO          (obj%TRMO         ,ntr,hd,tr)
  case ("TROT"         ) ; call TROT          (obj%TROT         ,ntr,hd,tr)
  case ("TRSTATS"      ) ; call TRSTATS       (obj%TRSTATS      ,ntr,hd,tr)
  case ("TSEL"         ) ; call TSEL          (obj%TSEL         ,ntr,hd,tr)
  case ("TSLC"         ) ; call TSLC          (obj%TSLC         ,ntr,hd,tr)
  case ("TSMUTE"       ) ; call TSMUTE        (obj%TSMUTE       ,ntr,hd,tr)
  case ("TSORT"        ) ;                  continue
  case ("PARALLELSORT"        ) ;                  continue
  case ("TSVF"         ) ; call TSVF          (obj%TSVF         ,ntr,hd,tr)
  case ("TTMO"         ) ; call TTMO          (obj%TTMO         ,ntr,hd,tr)
  case ("TTRIN"        ) ; call TTRIN         (obj%TTRIN        ,ntr,hd,tr)
  case ("TTROT"        ) ; call TTROT         (obj%TTROT        ,ntr,hd,tr)
  case ("TVF"          ) ; call TVF           (obj%TVF          ,ntr,hd,tr)
  case ("UNGATHER"     ) ;                  continue
  case ("UTEL"         ) ; call UTEL          (obj%UTEL         ,ntr,hd,tr)
  case ("VC"           ) ; call VC            (obj%VC           ,ntr,hd,tr)
  case ("VELEDIT"      ) ; call VELEDIT       (obj%VELEDIT      ,ntr,hd,tr)
  case ("VPICK"        ) ; call VPICK         (obj%VPICK        ,ntr,hd,tr)
  case ("VTRIM"        ) ; call VTRIM         (obj%VTRIM        ,ntr,hd,tr)
  case ("WSF"          ) ; call WSF           (obj%WSF          ,ntr,hd,tr)
  case ("XP"           ) ; call XP            (obj%XP           ,ntr,hd,tr)

      case default ; call pc_error ("Invalid process",obj%name)
      end select

      end subroutine super_oneset


!!----------------------------- twosets ---------------------------------!!
!!----------------------------- twosets ---------------------------------!!
!!----------------------------- twosets ---------------------------------!!


      subroutine super_twosets (obj,ntr,hd1,tr1,hd2,tr2)
      implicit none
      type(super_struct),pointer       :: obj               ! arguments
      integer           ,intent(inout) :: ntr               ! arguments
      double precision  ,intent(inout) :: hd1(:,:)          ! arguments
      real              ,intent(inout) :: tr1(:,:)          ! arguments
      double precision  ,intent(inout) :: hd2(:,:)          ! arguments
      real              ,intent(inout) :: tr2(:,:)          ! arguments

      select case (obj%name)

 case("ABAL"         );                continue
 case("ABRA"         );                continue
 case("ACORR"        );                continue
 case("ADNS"         );                continue
 case("ADPSUB"       );                continue
 case("ALAMO"        );                continue
 case("AMPDG"        );                continue
 case("AVAGRAD"      );                continue
 case("AVAST"        );                continue
 case("AVOANS"       );                continue
 case("AVOSTS"       );                continue
 case("AVOVAN"       );                continue
 case("AVOVIT"       );                continue
 case("BUNCH"        );                continue
 case("C4WE"         );                continue
 case("CC3D"         );                continue
 case("CFDS"         );                continue
 case("CHART"        );                continue
 case("CLEANUP"      );                continue
 case("CLIP"         );                continue
 case("CNEARTS"      );call CNEARTS      (obj%CNEARTS      ,ntr,hd1,tr1,hd2,tr2)
 case("CODMO"        );call CODMO        (obj%CODMO        ,ntr,hd1,tr1,hd2,tr2)
 case("COLOR"        );                continue
 case("COMBINE"      );                continue
 case("COMP"         );call COMP         (obj%COMP         ,ntr,hd1,tr1,hd2,tr2)
 case("CTAN"         );                continue
 case("DABRA"        );                continue
 case("DBGAIN"       );                continue
 case("DECON"        );                continue
 case("DIST"         );                continue
 case("DMAP3D"       );                continue
 case("DMO3D"        );                continue
 case("DMOPREP"      );call DMOPREP      (obj%DMOPREP      ,ntr,hd1,tr1,hd2,tr2)
 case("DRC"          );                continue
 case("DSIG"         );                continue
 case("DWTF"         );                continue
 case("DWTP"         );call DWTP         (obj%DWTP         ,ntr,hd1,tr1,hd2,tr2)
 case("EAGC"         );                continue
 case("EDA"          );call EDA          (obj%EDA          ,ntr,hd1,tr1,hd2,tr2)
 case("EDA3D"        );call EDA3D        (obj%EDA3D        ,ntr,hd1,tr1,hd2,tr2)
 case("TRACETERP3D"  );call TRACETERP3D  (obj%TRACETERP3D  ,ntr,hd1,tr1,hd2,tr2)
 case("ELEV"         );                continue
 case("EXO"          );                continue
 case("EZCHECK"      );                continue
 case("FBAL"         );                continue
 case("FFAVA"        );                continue
 case("FGD"          );                continue
 case("FGDREV"       );                continue
 case("FILL"         );call FILL         (obj%FILL         ,ntr,hd1,tr1,hd2,tr2)
 case("FILTP"        );                continue
 case("FISH"         );                continue
 case("FKAP"         );                continue
 case("FKFILT"       );call FKFILT       (obj%FKFILT       ,ntr,hd1,tr1,hd2,tr2)
 case("FKTMIG"       );                continue
 case("FKTR"         );call FKTR         (obj%FKTR         ,ntr,hd1,tr1,hd2,tr2)
 case("FLEXBIN"      );                continue
 case("FXDECON"      );                continue
 case("FXMIG"        );                continue
 case("FXRYT"        );call FXRYT        (obj%FXRYT        ,ntr,hd1,tr1,hd2,tr2)
 case("FXTI"         );call FXTI         (obj%FXTI         ,ntr,hd1,tr1,hd2,tr2)
 case("FXYDECON"     );                continue
 case("GATHER"       );call GATHER       (obj%GATHER       ,ntr,hd1,tr1,hd2,tr2)
 case("GDIV"         );                continue
 case("GENFILT"      );                continue
 case("GRAB"         );                continue
 case("GVS"          );                continue
 case("HEADCHECK"    );                continue
 case("HEADMAP"      );                continue
 case("HEADSUM"      );                continue
 case("HRZSTK"       );                continue
 case("HSYN"         );                continue
 case("HVEL"         );                continue
 case("IBSMA"        );                continue
 case("IMS"          );                continue
 case("IQ"           );                continue
 case("JOB_DATA"     );                continue
 case("KA"           );                continue
 case("KASTATS"      );                continue
 case("KDMIG"        );                continue
 case("KDMO"         );                continue
 case("KMIG"         );                continue
 case("KTMIG"        );                continue
 case("LMRKHRZ"      );                continue
 case("LMRKIN"       );                continue
 case("LMRKOUT"      );                continue
 case("MADC"         );                continue
 case("MASKER"       );                continue
 case("MCLINV"       );                continue
 case("MDIP"         );                continue
 case("MDS"          );                continue
 case("MFRS"         );                continue
 case("MGD"          );call MGD          (obj%MGD          ,ntr,hd1,tr1,hd2,tr2)
 case("MODMO"        );                continue
 case("MTFUN"        );call MTFUN        (obj%MTFUN        ,ntr,hd1,tr1,hd2,tr2)
 case("MUTE"         );                continue
 case("MVXP"         );                continue
 case("MZPC"         );                continue
 case("NMO"          );                continue
 case("NORM"         );                continue
 case("PAIRMERGE"    );                continue
 case("PGPS"         );                continue
 case("PH2OFF"       );                continue
 case("PROJECT_DATA" );                continue
 case("PSLINV"       );                continue
 case("PSTMIG"       );                continue
 case("QEST"         );                continue
 case("RANLINE"      );                continue
 case("RCPOUT"       );                continue
 case("REG"          );call REG          (obj%REG          ,ntr,hd1,tr1,hd2,tr2)
 case("REGBIN"       );call REGBIN       (obj%REGBIN       ,ntr,hd1,tr1,hd2,tr2)
 case("RES"          );                continue
 case("RESTH"        );                continue
 case("RFAB"         );                continue
 case("RMUL"         );                continue
 case("RNSYN"        );                continue
 case("RTC"          );call RTC          (obj%RTC          ,ntr,hd1,tr1,hd2,tr2)
 case("RYTOV"        );call RYTOV        (obj%RYTOV        ,ntr,hd1,tr1,hd2,tr2)
 case("SCAB"         );                continue
 case("SCALE"        );                continue
 case("SCDECON"      );                continue
 case("SDIP"         );call SDIP         (obj%SDIP         ,ntr,hd1,tr1,hd2,tr2)
 case("SDIP3D"       );call SDIP3D       (obj%SDIP3D       ,ntr,hd1,tr1,hd2,tr2)
 case("SELDMO"       );                continue
 case("SELECT"       );                continue
 case("SETMUTE"      );                continue
 case("SETPOLY"      );                continue
 case("SETWORD"      );                continue
 case("SHFT"         );                continue
 case("SISC"         );                continue
 case("SLAB"         );                continue
 case("SLICE"        );                continue
 case("SLICER"       );                continue
 case("SLST"         );                continue
 case("SPCT"         );call SPCT         (obj%SPCT         ,ntr,hd1,tr1,hd2,tr2)
 case("SPIKE"        );                continue
 case("SPLT"         );                continue
 case("SPTI"         );call SPTI         (obj%SPTI         ,ntr,hd1,tr1,hd2,tr2)
 case("STK"          );call STK          (obj%STK          ,ntr,hd1,tr1,hd2,tr2)
 case("STRETCH"      );                continue
 case("SVA"          );                continue
 case("SYNBP"        );                continue
 case("TABLESAVE"    );                continue
 case("TABLESORT"    );                continue
 case("TDC"          );                continue
 case("TDMP"         );                continue
 case("TELAV"        );                continue
 case("TFATT"        );call TFATT        (obj%TFATT        ,ntr,hd1,tr1,hd2,tr2)
 case("TPOW"         );                continue
 case("TREDIT"       );                continue
 case("TRIN"         );                continue
 case("TRINSORT"     );                continue
 case("TRMO"         );                continue
 case("TROT"         );                continue
 case("TRSTATS"      );                continue
 case("TSEL"         );                continue
 case("TSLC"         );                continue
 case("TSMUTE"       );                continue
 case("TSORT"        );call TSORT        (obj%TSORT        ,ntr,hd1,tr1,hd2,tr2)
 case("PARALLELSORT"        );call PARALLELSORT        (obj%PARALLELSORT        ,ntr,hd1,tr1,hd2,tr2)
 case("TSVF"         );                continue
 case("TTMO"         );                continue
 case("TTRIN"        );                continue
 case("TTROT"        );                continue
 case("TVF"          );                continue
 case("UNGATHER"     );call UNGATHER     (obj%UNGATHER     ,ntr,hd1,tr1,hd2,tr2)
 case("UTEL"         );                continue
 case("VC"           );                continue
 case("VELEDIT"      );                continue
 case("VPICK"        );                continue
 case("VTRIM"        );                continue
 case("WSF"          );                continue
 case("XP"           );                continue

      case default ; call pc_error ("Invalid process",obj%name)
      end select

      end subroutine super_twosets


!!------------------------------ load_list ---------------------------------!!
!!------------------------------ load_list ---------------------------------!!
!!------------------------------ load_list ---------------------------------!!


      subroutine super_load_list

      all(  1)%process  = "ABAL"
      all(  1)%category = "AMPLITUDE_MOD"
      all(  1)%maturity = SUPER_PRODUCTION
 
      all(  2)%process  = "ABRA"
      all(  2)%category = "VELOCITY_ANALYSIS"
      all(  2)%maturity = SUPER_BETA
 
      all(  3)%process  = "ACORR"
      all(  3)%category = "DIAGNOSTICS"
      all(  3)%maturity = SUPER_PRODUCTION
 
      all(  4)%process  = "ADNS"
      all(  4)%category = "SYNTHETICS"
      all(  4)%maturity = SUPER_PRODUCTION
 
      all(  5)%process  = "ADPSUB"
      all(  5)%category = "MISCELLANEOUS"
      all(  5)%maturity = SUPER_BETA
 
      all(  6)%process  = "ALAMO"
      all(  6)%category = "VELOCITY_ANALYSIS"
      all(  6)%maturity = SUPER_BETA
 
      all(  7)%process  = "AMPDG"
      all(  7)%category = "DIAGNOSTICS"
      all(  7)%maturity = SUPER_PRODUCTION
 
      all(  8)%process  = "AVAGRAD"
      all(  8)%category = "DIAGNOSTICS"
      all(  8)%maturity = SUPER_PRODUCTION
 
      all(  9)%process  = "AVAST"
      all(  9)%category = "STACKS"
      all(  9)%maturity = SUPER_PRODUCTION
 
      all( 10)%process  = "AVOANS"
      all( 10)%category = "VELOCITY_ANALYSIS"
      all( 10)%maturity = SUPER_BETA
 
      all( 11)%process  = "AVOSTS"
      all( 11)%category = "VELOCITY_ANALYSIS"
      all( 11)%maturity = SUPER_BETA
 
      all( 12)%process  = "AVOVAN"
      all( 12)%category = "VELOCITY_ANALYSIS"
      all( 12)%maturity = SUPER_BETA
 
      all( 13)%process  = "AVOVIT"
      all( 13)%category = "VELOCITY_ANALYSIS"
      all( 13)%maturity = SUPER_BETA
 
      all( 14)%process  = "BUNCH"
      all( 14)%category = "SORTS"
      all( 14)%maturity = SUPER_PRODUCTION
 
      all( 15)%process  = "C4WE"
      all( 15)%category = "FILTERS"
      all( 15)%maturity = SUPER_BETA
 
      all( 16)%process  = "CC3D"
      all( 16)%category = "STATICS"
      all( 16)%maturity = SUPER_PRODUCTION
 
      all( 17)%process  = "CFDS"
      all( 17)%category = "STATICS"
      all( 17)%maturity = SUPER_PRODUCTION
 
      all( 18)%process  = "CHART"
      all( 18)%category = "HEADERS"
      all( 18)%maturity = SUPER_PRODUCTION
 
      all( 19)%process  = "CLEANUP"
      all( 19)%category = "MISCELLANEOUS"
      all( 19)%maturity = SUPER_PRODUCTION
 
      all( 20)%process  = "CLIP"
      all( 20)%category = "AMPLITUDE_MOD"
      all( 20)%maturity = SUPER_PRODUCTION
 
      all( 21)%process  = "CNEARTS"
      all( 21)%category = "FILTERS"
      all( 21)%maturity = SUPER_BETA
 
      all( 22)%process  = "CODMO"
      all( 22)%category = "MIGRATIONS"
      all( 22)%maturity = SUPER_PRODUCTION
 
      all( 23)%process  = "COLOR"
      all( 23)%category = "PLOT"
      all( 23)%maturity = SUPER_PRODUCTION
 
      all( 24)%process  = "COMBINE"
      all( 24)%category = "MISCELLANEOUS"
      all( 24)%maturity = SUPER_BETA
 
      all( 25)%process  = "COMP"
      all( 25)%category = "STACKS"
      all( 25)%maturity = SUPER_PRODUCTION
 
      all( 26)%process  = "CTAN"
      all( 26)%category = "TRANSFORMS"
      all( 26)%maturity = SUPER_PRODUCTION
 
      all( 27)%process  = "DABRA"
      all( 27)%category = "MIGRATIONS"
      all( 27)%maturity = SUPER_BETA
 
      all( 28)%process  = "DBGAIN"
      all( 28)%category = "AMPLITUDE_MOD"
      all( 28)%maturity = SUPER_PRODUCTION
 
      all( 29)%process  = "DECON"
      all( 29)%category = "FILTERS"
      all( 29)%maturity = SUPER_PRODUCTION
 
      all( 30)%process  = "DIST"
      all( 30)%category = "DIAGNOSTICS"
      all( 30)%maturity = SUPER_PRODUCTION
 
      all( 31)%process  = "DMAP3D"
      all( 31)%category = "MIGRATIONS"
      all( 31)%maturity = SUPER_BETA
 
      all( 32)%process  = "DMO3D"
      all( 32)%category = "MIGRATIONS"
      all( 32)%maturity = SUPER_PRODUCTION
 
      all( 33)%process  = "DMOPREP"
      all( 33)%category = "SORTS"
      all( 33)%maturity = SUPER_PRODUCTION
 
      all( 34)%process  = "DRC"
      all( 34)%category = "MIGRATIONS"
      all( 34)%maturity = SUPER_PRODUCTION
 
      all( 35)%process  = "DSIG"
      all( 35)%category = "FILTERS"
      all( 35)%maturity = SUPER_PRODUCTION
 
      all( 36)%process  = "DWTF"
      all( 36)%category = "FILTERS"
      all( 36)%maturity = SUPER_PRODUCTION
 
      all( 37)%process  = "DWTP"
      all( 37)%category = "FILTERS"
      all( 37)%maturity = SUPER_PRODUCTION
 
      all( 38)%process  = "EAGC"
      all( 38)%category = "AMPLITUDE_MOD"
      all( 38)%maturity = SUPER_BETA
 
      all( 39)%process  = "EDA"
      all( 39)%category = "FILTERS"
      all( 39)%maturity = SUPER_PRODUCTION
 
      all( 40)%process  = "EDA3D"
      all( 40)%category = "FILTERS"
      all( 40)%maturity = SUPER_PRODUCTION
 
      all( 41)%process  = "ELEV"
      all( 41)%category = "HEADERS"
      all( 41)%maturity = SUPER_PRODUCTION
 
      all( 42)%process  = "EXO"
      all( 42)%category = "AMPLITUDE_MOD"
      all( 42)%maturity = SUPER_BETA
 
      all( 43)%process  = "EZCHECK"
      all( 43)%category = "MISCELLANEOUS"
      all( 43)%maturity = SUPER_PRODUCTION
 
      all( 44)%process  = "FBAL"
      all( 44)%category = "FILTERS"
      all( 44)%maturity = SUPER_PRODUCTION
 
      all( 45)%process  = "FFAVA"
      all( 45)%category = "STACKS"
      all( 45)%maturity = SUPER_PRODUCTION
 
      all( 46)%process  = "FGD"
      all( 46)%category = "HEADERS"
      all( 46)%maturity = SUPER_PRODUCTION
 
      all( 47)%process  = "FGDREV"
      all( 47)%category = "HEADERS"
      all( 47)%maturity = SUPER_PRODUCTION
 
      all( 48)%process  = "FILL"
      all( 48)%category = "MISCELLANEOUS"
      all( 48)%maturity = SUPER_PRODUCTION
 
      all( 49)%process  = "FILTP"
      all( 49)%category = "FILTERS"
      all( 49)%maturity = SUPER_PRODUCTION
 
      all( 50)%process  = "FISH"
      all( 50)%category = "STATICS"
      all( 50)%maturity = SUPER_PRODUCTION
 
      all( 51)%process  = "FKAP"
      all( 51)%category = "FILTERS"
      all( 51)%maturity = SUPER_PRODUCTION
 
      all( 52)%process  = "FKFILT"
      all( 52)%category = "FILTERS"
      all( 52)%maturity = SUPER_PRODUCTION
 
      all( 53)%process  = "FKTMIG"
      all( 53)%category = "MIGRATIONS"
      all( 53)%maturity = SUPER_BETA
 
      all( 54)%process  = "FKTR"
      all( 54)%category = "TRANSFORMS"
      all( 54)%maturity = SUPER_PRODUCTION
 
      all( 55)%process  = "FLEXBIN"
      all( 55)%category = "MISCELLANEOUS"
      all( 55)%maturity = SUPER_PRODUCTION
 
      all( 56)%process  = "FXDECON"
      all( 56)%category = "FILTERS"
      all( 56)%maturity = SUPER_BETA
 
      all( 57)%process  = "FXMIG"
      all( 57)%category = "MIGRATIONS"
      all( 57)%maturity = SUPER_BETA
 
      all( 58)%process  = "FXRYT"
      all( 58)%category = "MIGRATIONS"
      all( 58)%maturity = SUPER_BETA
 
      all( 59)%process  = "FXTI"
      all( 59)%category = "MISCELLANEOUS"
      all( 59)%maturity = SUPER_BETA
 
      all( 60)%process  = "FXYDECON"
      all( 60)%category = "FILTERS"
      all( 60)%maturity = SUPER_BETA
 
      all( 61)%process  = "GATHER"
      all( 61)%category = "SORTS"
      all( 61)%maturity = SUPER_PRODUCTION
 
      all( 62)%process  = "GDIV"
      all( 62)%category = "AMPLITUDE_MOD"
      all( 62)%maturity = SUPER_PRODUCTION
 
      all( 63)%process  = "GENFILT"
      all( 63)%category = "FILTERS"
      all( 63)%maturity = SUPER_PRODUCTION
 
      all( 64)%process  = "GRAB"
      all( 64)%category = "STATICS"
      all( 64)%maturity = SUPER_PRODUCTION
 
      all( 65)%process  = "GVS"
      all( 65)%category = "STACKS"
      all( 65)%maturity = SUPER_BETA
 
      all( 66)%process  = "HEADCHECK"
      all( 66)%category = "HEADERS"
      all( 66)%maturity = SUPER_PRODUCTION
 
      all( 67)%process  = "HEADMAP"
      all( 67)%category = "HEADERS"
      all( 67)%maturity = SUPER_PRODUCTION
 
      all( 68)%process  = "HEADSUM"
      all( 68)%category = "DIAGNOSTICS"
      all( 68)%maturity = SUPER_BETA
 
      all( 69)%process  = "HRZSTK"
      all( 69)%category = "STACKS"
      all( 69)%maturity = SUPER_BETA
 
      all( 70)%process  = "HSYN"
      all( 70)%category = "SYNTHETICS"
      all( 70)%maturity = SUPER_PRODUCTION
 
      all( 71)%process  = "HVEL"
      all( 71)%category = "VELOCITY_ANALYSIS"
      all( 71)%maturity = SUPER_PRODUCTION
 
      all( 72)%process  = "IBSMA"
      all( 72)%category = "MISCELLANEOUS"
      all( 72)%maturity = SUPER_BETA
 
      all( 73)%process  = "IMS"
      all( 73)%category = "STATICS"
      all( 73)%maturity = SUPER_PRODUCTION
 
      all( 74)%process  = "IQ"
      all( 74)%category = "FILTERS"
      all( 74)%maturity = SUPER_PRODUCTION
 
      all( 75)%process  = "JOB_DATA"
      all( 75)%category = "MISCELLANEOUS"
      all( 75)%maturity = SUPER_BETA
 
      all( 76)%process  = "KA"
      all( 76)%category = "MIGRATIONS"
      all( 76)%maturity = SUPER_BETA
 
      all( 77)%process  = "KASTATS"
      all( 77)%category = "MISCELLANEOUS"
      all( 77)%maturity = SUPER_BETA
 
      all( 78)%process  = "KDMIG"
      all( 78)%category = "MIGRATIONS"
      all( 78)%maturity = SUPER_BETA
 
      all( 79)%process  = "KDMO"
      all( 79)%category = "MIGRATIONS"
      all( 79)%maturity = SUPER_PRODUCTION
 
      all( 80)%process  = "KMIG"
      all( 80)%category = "MIGRATIONS"
      all( 80)%maturity = SUPER_BETA
 
      all( 81)%process  = "KTMIG"
      all( 81)%category = "MIGRATIONS"
      all( 81)%maturity = SUPER_BETA
 
      all( 82)%process  = "LMRKHRZ"
      all( 82)%category = "IO"
      all( 82)%maturity = SUPER_PRODUCTION
 
      all( 83)%process  = "LMRKIN"
      all( 83)%category = "IO"
      all( 83)%maturity = SUPER_PRODUCTION
 
      all( 84)%process  = "LMRKOUT"
      all( 84)%category = "IO"
      all( 84)%maturity = SUPER_PRODUCTION
 
      all( 85)%process  = "MADC"
      all( 85)%category = "FILTERS"
      all( 85)%maturity = SUPER_PRODUCTION
 
      all( 86)%process  = "MASKER"
      all( 86)%category = "MISCELLANEOUS"
      all( 86)%maturity = SUPER_BETA
 
      all( 87)%process  = "MCLINV"
      all( 87)%category = "INVERSION"
      all( 87)%maturity = SUPER_PRODUCTION
 
      all( 88)%process  = "MDIP"
      all( 88)%category = "FILTERS"
      all( 88)%maturity = SUPER_PRODUCTION
 
      all( 89)%process  = "MDS"
      all( 89)%category = "AMPLITUDE_MOD"
      all( 89)%maturity = SUPER_PRODUCTION
 
      all( 90)%process  = "MFRS"
      all( 90)%category = "STATICS"
      all( 90)%maturity = SUPER_PRODUCTION
 
      all( 91)%process  = "MGD"
      all( 91)%category = "HEADERS"
      all( 91)%maturity = SUPER_PRODUCTION
 
      all( 92)%process  = "MODMO"
      all( 92)%category = "TRANSFORMS"
      all( 92)%maturity = SUPER_BETA
 
      all( 93)%process  = "MTFUN"
      all( 93)%category = "FILTERS"
      all( 93)%maturity = SUPER_PRODUCTION
 
      all( 94)%process  = "MUTE"
      all( 94)%category = "AMPLITUDE_MOD"
      all( 94)%maturity = SUPER_PRODUCTION
 
      all( 95)%process  = "MVXP"
      all( 95)%category = "AMPLITUDE_MOD"
      all( 95)%maturity = SUPER_PRODUCTION
 
      all( 96)%process  = "MZPC"
      all( 96)%category = "FILTERS"
      all( 96)%maturity = SUPER_PRODUCTION
 
      all( 97)%process  = "NMO"
      all( 97)%category = "TRANSFORMS"
      all( 97)%maturity = SUPER_BETA
 
      all( 98)%process  = "NORM"
      all( 98)%category = "AMPLITUDE_MOD"
      all( 98)%maturity = SUPER_PRODUCTION
 
      all( 99)%process  = "PAIRMERGE"
      all( 99)%category = "MISCELLANEOUS"
      all( 99)%maturity = SUPER_PRODUCTION
 
      all(100)%process  = "PGPS"
      all(100)%category = "IO"
      all(100)%maturity = SUPER_PRODUCTION
 
      all(101)%process  = "PH2OFF"
      all(101)%category = "TRANSFORMS"
      all(101)%maturity = SUPER_BETA
 
      all(102)%process  = "PROJECT_DATA"
      all(102)%category = "MISCELLANEOUS"
      all(102)%maturity = SUPER_BETA
 
      all(103)%process  = "PSLINV"
      all(103)%category = "INVERSION"
      all(103)%maturity = SUPER_PRODUCTION
 
      all(104)%process  = "PSTMIG"
      all(104)%category = "MIGRATIONS"
      all(104)%maturity = SUPER_BETA
 
      all(105)%process  = "QEST"
      all(105)%category = "FILTERS"
      all(105)%maturity = SUPER_PRODUCTION
 
      all(106)%process  = "RANLINE"
      all(106)%category = "MISCELLANEOUS"
      all(106)%maturity = SUPER_PRODUCTION
 
      all(107)%process  = "RCPOUT"
      all(107)%category = "MISCELLANEOUS"
      all(107)%maturity = SUPER_PRODUCTION
 
      all(108)%process  = "REG"
      all(108)%category = "MISCELLANEOUS"
      all(108)%maturity = SUPER_PRODUCTION
 
      all(109)%process  = "REGBIN"
      all(109)%category = "FILTERS"
      all(109)%maturity = SUPER_PRODUCTION
 
      all(110)%process  = "RES"
      all(110)%category = "TRANSFORMS"
      all(110)%maturity = SUPER_PRODUCTION
 
      all(111)%process  = "RESTH"
      all(111)%category = "HEADERS"
      all(111)%maturity = SUPER_PRODUCTION
 
      all(112)%process  = "RFAB"
      all(112)%category = "FILTERS"
      all(112)%maturity = SUPER_PRODUCTION
 
      all(113)%process  = "RMUL"
      all(113)%category = "FILTERS"
      all(113)%maturity = SUPER_BETA
 
      all(114)%process  = "RNSYN"
      all(114)%category = "SYNTHETICS"
      all(114)%maturity = SUPER_PRODUCTION
 
      all(115)%process  = "RTC"
      all(115)%category = "STATICS"
      all(115)%maturity = SUPER_BETA
 
      all(116)%process  = "RYTOV"
      all(116)%category = "MIGRATIONS"
      all(116)%maturity = SUPER_PRODUCTION
 
      all(117)%process  = "SCAB"
      all(117)%category = "AMPLITUDE_MOD"
      all(117)%maturity = SUPER_PRODUCTION
 
      all(118)%process  = "SCALE"
      all(118)%category = "AMPLITUDE_MOD"
      all(118)%maturity = SUPER_PRODUCTION
 
      all(119)%process  = "SCDECON"
      all(119)%category = "FILTERS"
      all(119)%maturity = SUPER_PRODUCTION
 
      all(120)%process  = "SDIP"
      all(120)%category = "FILTERS"
      all(120)%maturity = SUPER_PRODUCTION
 
      all(121)%process  = "SDIP3D"
      all(121)%category = "FILTERS"
      all(121)%maturity = SUPER_PRODUCTION
 
      all(122)%process  = "SELDMO"
      all(122)%category = "SORTS"
      all(122)%maturity = SUPER_PRODUCTION
 
      all(123)%process  = "SELECT"
      all(123)%category = "HEADERS"
      all(123)%maturity = SUPER_PRODUCTION
 
      all(124)%process  = "SETMUTE"
      all(124)%category = "AMPLITUDE_MOD"
      all(124)%maturity = SUPER_PRODUCTION
 
      all(125)%process  = "SETPOLY"
      all(125)%category = "HEADERS"
      all(125)%maturity = SUPER_PRODUCTION
 
      all(126)%process  = "SETWORD"
      all(126)%category = "HEADERS"
      all(126)%maturity = SUPER_PRODUCTION
 
      all(127)%process  = "SHFT"
      all(127)%category = "STATICS"
      all(127)%maturity = SUPER_PRODUCTION
 
      all(128)%process  = "SISC"
      all(128)%category = "STATICS"
      all(128)%maturity = SUPER_PRODUCTION
 
      all(129)%process  = "SLAB"
      all(129)%category = "MISCELLANEOUS"
      all(129)%maturity = SUPER_PRODUCTION
 
      all(130)%process  = "SLICE"
      all(130)%category = "MISCELLANEOUS"
      all(130)%maturity = SUPER_BETA
 
      all(131)%process  = "SLICER"
      all(131)%category = "MISCELLANEOUS"
      all(131)%maturity = SUPER_BETA
 
      all(132)%process  = "SLST"
      all(132)%category = "TRANSFORMS"
      all(132)%maturity = SUPER_PRODUCTION
 
      all(133)%process  = "SPCT"
      all(133)%category = "TRANSFORMS"
      all(133)%maturity = SUPER_PRODUCTION
 
      all(134)%process  = "SPIKE"
      all(134)%category = "SYNTHETICS"
      all(134)%maturity = SUPER_BETA
 
      all(135)%process  = "SPLT"
      all(135)%category = "PLOT"
      all(135)%maturity = SUPER_PRODUCTION
 
      all(136)%process  = "SPTI"
      all(136)%category = "TRANSFORMS"
      all(136)%maturity = SUPER_PRODUCTION
 
      all(137)%process  = "STK"
      all(137)%category = "STACKS"
      all(137)%maturity = SUPER_BETA
 
      all(138)%process  = "STRETCH"
      all(138)%category = "TRANSFORMS"
      all(138)%maturity = SUPER_BETA
 
      all(139)%process  = "SVA"
      all(139)%category = "VELOCITY_ANALYSIS"
      all(139)%maturity = SUPER_PRODUCTION
 
      all(140)%process  = "SYNBP"
      all(140)%category = "SYNTHETICS"
      all(140)%maturity = SUPER_PRODUCTION
 
      all(141)%process  = "TABLESAVE"
      all(141)%category = "SORTS"
      all(141)%maturity = SUPER_PRODUCTION
 
      all(142)%process  = "TABLESORT"
      all(142)%category = "SORTS"
      all(142)%maturity = SUPER_PRODUCTION
 
      all(143)%process  = "TDC"
      all(143)%category = "TRANSFORMS"
      all(143)%maturity = SUPER_PRODUCTION
 
      all(144)%process  = "TDMP"
      all(144)%category = "DIAGNOSTICS"
      all(144)%maturity = SUPER_PRODUCTION
 
      all(145)%process  = "TELAV"
      all(145)%category = "AMPLITUDE_MOD"
      all(145)%maturity = SUPER_PRODUCTION
 
      all(146)%process  = "TFATT"
      all(146)%category = "TRANSFORMS"
      all(146)%maturity = SUPER_PRODUCTION
 
      all(147)%process  = "TPOW"
      all(147)%category = "AMPLITUDE_MOD"
      all(147)%maturity = SUPER_PRODUCTION
 
      all(148)%process  = "TREDIT"
      all(148)%category = "HEADERS"
      all(148)%maturity = SUPER_BETA
 
      all(149)%process  = "TRIN"
      all(149)%category = "IO"
      all(149)%maturity = SUPER_BETA
 
      all(150)%process  = "TRINSORT"
      all(150)%category = "IO"
      all(150)%maturity = SUPER_PRODUCTION
 
      all(151)%process  = "TRMO"
      all(151)%category = "VELOCITY_ANALYSIS"
      all(151)%maturity = SUPER_BETA
 
      all(152)%process  = "TROT"
      all(152)%category = "IO"
      all(152)%maturity = SUPER_BETA
 
      all(153)%process  = "TRSTATS"
      all(153)%category = "MISCELLANEOUS"
      all(153)%maturity = SUPER_BETA
 
      all(154)%process  = "TSEL"
      all(154)%category = "MISCELLANEOUS"
      all(154)%maturity = SUPER_PRODUCTION
 
      all(155)%process  = "TSLC"
      all(155)%category = "SORTS"
      all(155)%maturity = SUPER_PRODUCTION
 
      all(156)%process  = "TSMUTE"
      all(156)%category = "VELOCITY_ANALYSIS"
      all(156)%maturity = SUPER_BETA
 
      all(157)%process  = "TSORT"
      all(157)%category = "SORTS"
      all(157)%maturity = SUPER_PRODUCTION
 
      all(158)%process  = "TSVF"
      all(158)%category = "FILTERS"
      all(158)%maturity = SUPER_PRODUCTION
 
      all(159)%process  = "TTMO"
      all(159)%category = "VELOCITY_ANALYSIS"
      all(159)%maturity = SUPER_BETA
 
      all(160)%process  = "TTRIN"
      all(160)%category = "IO"
      all(160)%maturity = SUPER_BETA
 
      all(161)%process  = "TTROT"
      all(161)%category = "IO"
      all(161)%maturity = SUPER_BETA
 
      all(162)%process  = "TVF"
      all(162)%category = "FILTERS"
      all(162)%maturity = SUPER_PRODUCTION
 
      all(163)%process  = "UNGATHER"
      all(163)%category = "SORTS"
      all(163)%maturity = SUPER_PRODUCTION
 
      all(164)%process  = "UTEL"
      all(164)%category = "MISCELLANEOUS"
      all(164)%maturity = SUPER_BETA
 
      all(165)%process  = "VC"
      all(165)%category = "FILTERS"
      all(165)%maturity = SUPER_PRODUCTION
 
      all(166)%process  = "VELEDIT"
      all(166)%category = "VELOCITY_ANALYSIS"
      all(166)%maturity = SUPER_BETA
 
      all(167)%process  = "VPICK"
      all(167)%category = "VELOCITY_ANALYSIS"
      all(167)%maturity = SUPER_PRODUCTION
 
      all(168)%process  = "VTRIM"
      all(168)%category = "VELOCITY_ANALYSIS"
      all(168)%maturity = SUPER_PRODUCTION
 
      all(169)%process  = "WSF"
      all(169)%category = "FILTERS"
      all(169)%maturity = SUPER_PRODUCTION
 
      all(170)%process  = "XP"
      all(170)%category = "AMPLITUDE_MOD"
      all(170)%maturity = SUPER_PRODUCTION
 
      all(171)%process  = "PARALLELSORT"
      all(171)%category = "SORTS"
      all(171)%maturity = SUPER_PRODUCTION
 
      all(172)%process  = "TRACETERP3D"
      all(172)%category = "MISCELLANEOUS"
      all(172)%maturity = SUPER_BETA
 
      first_time = .FALSE.

      end subroutine super_load_list


!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!


      subroutine super_list (process_list,nprocess_list,category_name)
      implicit none
      character(len=*),pointer             :: process_list(:)    ! argument
      integer         ,intent(out)         :: nprocess_list      ! argument
      character(len=*),optional,intent(in) :: category_name      ! argument
      character(len=PC_LENGTH)             :: category           ! local
      integer                              :: i                  ! local
      integer                              :: istat              ! local

      if (first_time) call super_load_list

      if (present(category_name)) then
        category = category_name
        call string_to_upper(category)
      else
        category = 'ALL_PROCESSES'
      endif

      nprocess_list = 0
      if (associated(process_list)) then
        deallocate(process_list,stat=istat)
        if (istat /= 0) then
          print *,'super_list: deallocate error istat=',istat
        endif
      endif

      select case (category)
        case ('ALL_PROCESSES')
          nprocess_list = 0
          do i=1,LEN_ALL
            if (view >= all(i)%maturity) nprocess_list = nprocess_list + 1
          enddo
          if (nprocess_list > 0) then
            allocate(process_list(nprocess_list))
            nprocess_list = 0
            do i=1,LEN_ALL
              if (view >= all(i)%maturity) then
                nprocess_list = nprocess_list + 1
                process_list(nprocess_list) = all(i)%process
              endif
            enddo
          endif

        case default
          nprocess_list = 0
          do i=1,LEN_ALL
            if (all(i)%category == category .and.    &
                view >= all(i)%maturity) nprocess_list = nprocess_list + 1
          enddo
          if (nprocess_list > 0) then
            allocate(process_list(nprocess_list))
            nprocess_list = 0
            do i=1,LEN_ALL
              if (all(i)%category == category .and.  &
                  view >= all(i)%maturity) then
                nprocess_list = nprocess_list + 1
                process_list(nprocess_list) = all(i)%process
              endif
            enddo
          endif
      end select

      if (.not. associated(process_list)) then
        allocate(process_list(1))
        process_list(1) = ' '
      endif

      end subroutine super_list


!!------------------------------ validate ----------------------------------!!
!!------------------------------ validate ----------------------------------!!
!!------------------------------ validate ----------------------------------!!


      function super_validate (process_name) result (valid)
      implicit none
      character(len=*),intent(in)      :: process_name       !argument
      logical                          :: valid              !result

      character(len=PC_LENGTH)         :: process            !local
      integer                          :: i                  !local

      if (first_time) call super_load_list

      process = process_name
      call string_to_upper (process)

      valid = .FALSE.
      do i=1,LEN_ALL
        if (all(i)%process == process .and.  &
            view >= all(i)%maturity) then
          valid = .TRUE.
          exit
        endif
      enddo

      end function super_validate


!!------------------------------- set_view ---------------------------------!!
!!------------------------------- set_view ---------------------------------!!
!!------------------------------- set_view ---------------------------------!!


      subroutine super_set_view (desired_view)
      implicit none
      integer,intent(in)       :: desired_view                  !argument

      if (desired_view >= SUPER_PRODUCTION .and. desired_view <= SUPER_RAW) then
        view = desired_view
      endif

      end subroutine super_set_view


!!------------------------------- get_name ---------------------------------!!
!!------------------------------- get_name ---------------------------------!!
!!------------------------------- get_name ---------------------------------!!


      subroutine super_get_name (obj, process_name)
      implicit none
      type(super_struct),intent(in)    :: obj                !argument
      character(len=*)  ,intent(out)   :: process_name       !argument

      process_name = obj%name

      end subroutine super_get_name


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module super_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

