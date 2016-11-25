!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- sdiputil.f90 --------------------------------!!
!!---------------------------- sdiputil.f90 --------------------------------!!
!!---------------------------- sdiputil.f90 --------------------------------!!


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
! Name       : SDIPUTIL 
! Category   : math
! Written    : 2001-08-17   by: Tom Stoeckley
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Utility for SDIP and SDIP3D and EDA3D.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive contains code which is common to the SDIP, SDIP3D, and EDA3D
! processes.  It owns some of its own parameters, and therefore supports its
! own gui_def and HelpSection sections.  It also validates parameters owned
! by the calling process and needed by this primitive to do its work.
!
! Since this primitive is used only in conjunction with process modules, and
! owns its own process parameters, it uses the parameter cache.
!
! This primitive puts the correct values into the parameter cache for the
! control parameters NUMTR and GATHERED.
!
! This primitive provides algorithms for generating a mixed trace, or a trace
! containing an edge detection attribute, from a set of input traces which
! are centered symmetrically around the location of the output trace.  The
! mix or edge detection can optionally be performed along dips found using
! a semblance search for the dominant dip.
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
!                           CALLING SEQUENCE
!
! In the process module gui_def section:
!
!     <include sdiputil.f90>
!
!     (The above line should be put at the bottom of the gui_def section,
!     because it will provide a message line followed by a new screen.)
!
!
! In the process module create, initialize, wrapup, and delete routines:
!
!                                o
!     call sdiputil_create     (obj)
!
!     call sdiputil_initialize (obj)
!     call sdiputil_wrapup     (obj)
!     call sdiputil_delete     (obj)
!                                b
!
!
! In the process module update routine (between the PC_GETs and PC_PUTs):
!
!                            b       b          b         b
!     call sdiputil_update (obj, opt_output, pwr_edge, pwr_semb,
!                           opt_semb, fctr_mix, win_len, win_nsamp,   
!                              i         b         b         b
!
!             i                  i              b          b
!      quick_dip_weights, quick_dip_search, dip_max_x, dip_max_y, 
!      num_tr_dip_x, num_tr_dip_y, num_tr_mix_x, num_tr_mix_y,
!           b             b             b             b
!
!        b      b      b      b        b           b
!      hdr_x, hdr_y, x_inc, y_inc, max_x_bins, max_y_bins,
!      nxgather, nygather, nxmix, nymix, nxdips, nydips, dip_inc_x, dip_inc_y)
!         o         o        o      o      o       o         o          o
!
!
! In the process module update routine (after PC_DO_NOT_PROCESS_TRACES):
!
!     call sdiputil_prepare (obj,nhx,nhy,nhw)
!                             b   i   i   i
!
!
! In the process module main trace processing routine:
!
!     call sdiputil_solve (obj,ntr,hdgather,trgather,xmid,ymid,hdo,tro)
!                           b   b     b        b      i    i    o   o
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(sdiputil_struct)  obj = pointer to the SDIPUTIL data structure.
!
! char(len=*)  opt_output = type of traces to output to next process.
! real         pwr_edge   = power of coherency for edge detection.
! real         pwr_semb   = power of semblance for dip mix.
! logical      opt_semb   = whether to weight input trace by local semblance.
! real         fctr_mix   = fraction of orig trace to combine with mixed trace.
! real         win_len    = semblance window length in seconds for dip search.
! integer      win_nsamp  = number of trace samples in edge detection window.
!
! logical   quick_dip_weights = whether to decimate traces for dip search.
! logical   quick_dip_search  = whether to decimate dip searches.
!
! real      dip_max_x   ,dip_max_y    = maximum dip (ms/trace).
! real      dip_inc_x   ,dip_inc_y    = dip increment (ms/trace).
! integer   num_tr_dip_x,num_tr_dip_y = #traces each side for dip search.
! integer   num_tr_mix_x,num_tr_mix_y = #traces each side for mixing. 
! integer   hdr_x       ,hdr_y        = header word.
! real      x_inc       ,y_inc        = bin increment.
! integer   max_x_bins  ,max_y_bins   = max approx number of bins (traces).
! integer   nxgather    ,nygather     = total #traces for dip search.
! integer   nxmix       ,nymix        = total #traces for mixing.
! integer   nxdips      ,nydips       = total number of dips.
!
! integer           nhx,nhy,nhw  = header words containing X, Y, and weight.
!
! integer           ntr                = number of traces in and out.
! double precision  hdgather(nwih,ntr) = gather of traces to search and mix.
! real              trgather(ndpt,ntr) = gather of traces to search and mix.
! real              xmid,ymid          = middle X and Y coordinates.
! double precision  hdo(nwih,ntr)      = output trace.
! real              tro(ndpt,ntr)      = output trace.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! The OPT_OUTPUT parameter must have one of the following values:
!
!                     'MIXED TRACES'
!                     'MEEK EDGE DETECTION'
!                     'SEMBLANCE EDGE DETECTION'
!                     'semblance values'
!                     'dip_x values'
!                     'dip_y values'
!                     'original gathers'
!                     'flattened gathers'
!                     'middle trace in gather'
!
! The OPT_TAPER parameter must have one of the following values:
!
!                     'LINEAR'
!                     'COSINE'
!                     'NONE'
!
! See SDIP or SDIP3D or EDA3D for additional parameter details.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
! 11. 2006-09-11  Stoeckley  Replace pc_register_tab_group w HelpSection line.
! 10. 2006-06-20  Stoeckley  Add pc_register_tab_group for SeisSpace.
!  9. 2006-01-10  B. Menger  Removed Unused Variables.
!  8. 2004-08-11  Lucas      Added subroutine to get filepath names; for use
!                            by SDIP3D in parallel jobs.
!  7. 2004-06-08  Lucas      Added optional worker argument so that parallel
!                            jobs can create and write to different files.
!  6. 2002-08-26  Stoeckley  Put exact coordinates into output trace header.
!  5. 2001-12-11  Stoeckley  Honor mute time for speedup.
!  4. 2001-11-14  Stoeckley  Convert to a creatable/deletable class; add
!                             parameters for diagnostic output files; move
!                             some dip search code to lower level primitive
!                             SEMDIP; add gui_def and HelpSection sections
!                             for new owned parameters.
!  3. 2001-10-11  Stoeckley  Fix problem of getting zero dip_x diagnostic
!                             values when doing quick orthogonal dip searches.
!  2. 2001-08-24  Stoeckley  Add options for speeding up dip searches.
!  1. 2001-08-17  Stoeckley  Initial version, created from code removed
!                             from SDIP and SDIP3D, and copied from EDA.
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


!-------------------------------------------------------------------------------
!<gui_def>
! [message]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!<NS Diagnostic Output Files/NC=80>
!       BASENAME~~~=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! [SAVE_MEDGE]`KK   PATH_MEDGE =`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_MEDGE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_SEDGE]`KK   PATH_SEDGE =`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_SEDGE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_MIXED]`KK   PATH_MIXED =`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_MIXED_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_SEMB]`KK   path_semb~~=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_SEMB_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_XDIP]`KK   path_xdip~~=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_XDIP_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_YDIP]`KK   path_ydip~~=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_YDIP_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_MAXDIP]`KK   path_maxdip~~=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_MAXDIP_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_AZIM]`KK   path_azim~~=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_AZIM_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_ORIG]`KK   path_orig~~=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!    (large file)   [PATH_ORIG_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_FLAT]`KK   path_flat~~=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!    (large file)   [PATH_FLAT_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! [SAVE_MIDDLE]`KK   path_middle=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                   [PATH_MIDDLE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS MESSAGE    [/ML=128/XST]>
!<PARMS BASENAME   [/ML=128/XST]>
!<PARMS PATH_MEDGE [/ML=128/XST]>
!<PARMS PATH_SEDGE [/ML=128/XST]>
!<PARMS PATH_MIXED [/ML=128/XST]>
!<PARMS PATH_SEMB  [/ML=128/XST]>
!<PARMS PATH_XDIP  [/ML=128/XST]>
!<PARMS PATH_YDIP  [/ML=128/XST]>
!<PARMS PATH_MAXDIP[/ML=128/XST]>
!<PARMS PATH_AZIM  [/ML=128/XST]>
!<PARMS PATH_ORIG  [/ML=128/XST]>
!<PARMS PATH_FLAT  [/ML=128/XST]>
!<PARMS PATH_MIDDLE[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!      tabgroup = Diagnostic Output Files
!
!<Help KEYWORD="MESSAGE" TYPE="DISPLAY_ONLY">
!<Tip> Message regarding number of requested diagnostic files. </Tip>
!
! This information refers to the diagnostic files requested on the
! "Diagnostic Output Files" screen.  That screen should be visited
! to insure that the requests are correct and the status of each
! requested file is acceptable.
!</Help>
!
!
!<Help KEYWORD="PATH_ORIG_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_ORIG. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_FLAT_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_FLAT. </Tip>
!</Help>
!
!
!
!<Help KEYWORD="PATH_SEMB_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_SEMB. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_XDIP_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_XDIP. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_YDIP_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_YDIP. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_MAXDIP_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_MAXDIP. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_AZIM_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_AZIM. </Tip>
!</Help>
!
!
!
!<Help KEYWORD="PATH_MIXED_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_MIXED. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_MEDGE_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_MEDGE. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_SEDGE_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_SEDGE. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_MIDDLE_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATH_MIDDLE. </Tip>
!</Help>
!
!
!-------------------------------------------------------------------------------
!
!
!<Help KEYWORD="SAVE_ORIG">
!<Tip> Whether to save original moving gathers to PATH_ORIG. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!<Help KEYWORD="SAVE_FLAT">
!<Tip> Whether to save flattened moving gathers to PATH_FLAT. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!
!
!<Help KEYWORD="SAVE_SEMB">
!<Tip> Whether to save semblance traces to PATH_SEMB. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!<Help KEYWORD="SAVE_XDIP">
!<Tip> Whether to save dip traces (X direction) to PATH_XDIP. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!<Help KEYWORD="SAVE_YDIP">
!<Tip> Whether to save dip traces (Y direction) to PATH_YDIP. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!<Help KEYWORD="SAVE_MAXDIP">
!<Tip> Whether to save maximum dip traces to PATH_MAXDIP. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!<Help KEYWORD="SAVE_AZIM">
!<Tip> Whether to save azimuth traces to PATH_AZIM. </Tip>
! Default = NO
! Allowed = YES or NO
! The azimuth is the direction of the steepest dip in degrees from the
! positive Y axis toward the positive X axis.
!</Help>
!
!
!
!<Help KEYWORD="SAVE_MIXED">
!<Tip> Whether to save mixed traces to PATH_MIXED. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!<Help KEYWORD="SAVE_MEDGE">
!<Tip> Whether to save edge detection traces to PATH_MEDGE. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!<Help KEYWORD="SAVE_SEDGE">
!<Tip> Whether to save edge detection traces to PATH_SEDGE. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!<Help KEYWORD="SAVE_MIDDLE">
!<Tip> Whether to save middle traces to PATH_MIDDLE. </Tip>
! Default = NO
! Allowed = YES or NO
!</Help>
!
!
!-------------------------------------------------------------------------------
!
!
!<Help KEYWORD="BASENAME">
!<Tip> Base name for pathnames for diagnostic trace output files. </Tip>
! Default = NONE
! Allowed = char
!
! This basename is used to create path names for files which will contain
! diagnostic traces.  This basename must not include a filename extension.
!</Help>
!
!
!-------------------------------------------------------------------------------
!
!
!<Help KEYWORD="PATH_ORIG" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for diagnostic traces of original gathers. </Tip>
! Default = NONE
! Allowed = char
!
! These are the original moving gathers used for doing the dip search.
! One moving gather will be output to the file for each trace processed.
! Each moving gather contains all of the traces used to do a dip search
! for each trace processed.  Each moving gather will contain
! (2*NUM_TR_DIP_X + 1) times (2*NUM_TR_DIP_Y + 1) traces.
!
! This file can be very large.
! The file will be an 8-bit file.
!</Help>
!
!
!<Help KEYWORD="PATH_FLAT" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for diagnostic traces of flattened gathers. </Tip>
! Default = NONE
! Allowed = char
!
! These are the moving gathers as they have been flattened using the results
! of the dip search.  One moving gather will be output to the file for each
! trace processed.  Each moving gather contains all of the traces used to do
! a dip search for each trace processed.  Each moving gather will contain
! (2*NUM_TR_DIP_X + 1) times (2*NUM_TR_DIP_Y + 1) traces.
!
! This file can be very large.
! The file will be an 8-bit file.
!
! The traces will be same as original gathers if dip searches are not done.
!</Help>
!
!
!
!
!
!<Help KEYWORD="PATH_SEMB" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for diagnostic traces of semblance values found. </Tip>
! Default = NONE
! Allowed = char
!
! One semblance trace will be output to the file for each trace processed.
! Semblances are between zero and one.
! The file will be an 8-bit file.
!</Help>
!
!
!<Help KEYWORD="PATH_XDIP" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for diagnostic traces of dips found in the X direction. </Tip>
! Default = NONE
! Allowed = char
!
! One dip trace will be output to the file for each trace processed.
! Dips are in milliseconds per trace.
! The file will be an 8-bit file.
!
! The traces will be dead if dip searches are not done in the X direction.
!</Help>
!
!
!<Help KEYWORD="PATH_YDIP" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for diagnostic traces of dips found in the Y direction. </Tip>
! Default = NONE
! Allowed = char
!
! One dip trace will be output to the file for each trace processed.
! Dips are in milliseconds per trace.
! The file will be an 8-bit file.
!
! The traces will be dead if dip searches are not done in the Y direction.
! (This is the case for the 2D SDIP process.)
!</Help>
!
!
!<Help KEYWORD="PATH_MAXDIP" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for diagnostic traces of dips found in the steepest direction. </Tip>
! Default = NONE
! Allowed = char
!
! One dip trace will be output to the file for each trace processed.
! Dips are in milliseconds per trace.
! The file will be an 8-bit file.
!</Help>
!
!
!<Help KEYWORD="PATH_AZIM" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for diagnostic traces of the azimuth of maximum dips. </Tip>
! Default = NONE
! Allowed = char
!
! One azimuth trace will be output to the file for each trace processed.
! Azimuths are in degrees.
! The file will be an 8-bit file.
!</Help>
!
!
!
!<Help KEYWORD="PATH_MIXED" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for mixed traces. </Tip>
! Default = NONE
! Allowed = char
!
! One mixed trace will be output to the file for each trace processed.
! The file will be an 8-bit file.
!
! These traces are calculated from moving gathers centered on each input trace.
!</Help>
!
!
!<Help KEYWORD="PATH_MEDGE" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for Meek edge detection traces. </Tip>
! Default = NONE
! Allowed = char
!
! One edge detection trace will be output to the file for each trace processed.
! The file will be an 8-bit file.
!
! These traces are calculated from moving gathers centered on each input trace.
!</Help>
!
!
!<Help KEYWORD="PATH_SEDGE" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for semblance edge detection traces. </Tip>
! Default = NONE
! Allowed = char
!
! One edge detection trace will be output to the file for each trace processed.
! The file will be an 8-bit file.
!
! These traces are calculated from moving gathers centered on each input trace.
!</Help>
!
!
!<Help KEYWORD="PATH_MIDDLE" TYPE="DISPLAY_ONLY">
!<Tip> Pathname for middle trace of each moving gather. </Tip>
! Default = NONE
! Allowed = char
!
! One trace will be output to the file for each trace processed.
! The file will be an 8-bit file.
!
! Each trace output will be the trace from the moving gather which is the
! closest to the output trace location.  Therefore each trace should be the
! same as an original input trace.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module sdiputil_module
      use mth_module
      use semdip_module
      use named_constants_module
      use rsvd_module
      use pc_module
      use lav_module
      use permtfile_module
      use pathcheck_module
      use string_module
      implicit none
      public
      private :: sdiputil_private_getput
      private :: sdiputil_private_open
      private :: sdiputil_private_close
      private :: sdiputil_private_write
      private :: sdiputil_scale
      private :: sdiputil_semblance_edge
      private :: sdiputil_meek_edge

      character(len=100),public,save :: SDIPUTIL_IDENT = &
'$Id: sdiputil.f90,v 1.11 2006/09/11 13:15:50 Stoeckley prod sps $'

      interface sdiputil_private_write
           module procedure sdiputil_private_write_many
           module procedure sdiputil_private_write_one
      end interface

      integer,parameter,private :: WHICH_ORIG    = 1
      integer,parameter,private :: WHICH_FLAT    = 2
      integer,parameter,private :: WHICH_SEMB    = 3
      integer,parameter,private :: WHICH_XDIP    = 4
      integer,parameter,private :: WHICH_YDIP    = 5
      integer,parameter,private :: WHICH_MAXDIP  = 10
      integer,parameter,private :: WHICH_AZIM    = 11
      integer,parameter,private :: WHICH_MIXED   = 6
      integer,parameter,private :: WHICH_MEDGE   = 7
      integer,parameter,private :: WHICH_SEDGE   = 8
      integer,parameter,private :: WHICH_MIDDLE  = 9


!!--------------------------- data structure -------------------------------!!
!!--------------------------- data structure -------------------------------!!
!!--------------------------- data structure -------------------------------!!


      type,public :: sdiputil_struct

        private
        integer             :: nwih,ndpt             ! globals
        real                :: tstrt,dt              ! globals

        real                :: dip_max_x             ! from process
        real                :: dip_max_y             ! from process
        real                :: pwr_edge              ! from process
        real                :: pwr_semb              ! from process
        logical             :: opt_semb              ! from process
        real                :: fctr_mix              ! from process
        real                :: win_len               ! from process
        integer             :: win_nsamp             ! from process
        logical             :: quick_dip_weights     ! from process
        logical             :: quick_dip_search      ! from process
        integer             :: nhx,nhy,nhw           ! from process

        logical             :: save_orig             ! process parameters
        logical             :: save_flat             ! process parameters
        logical             :: save_semb             ! process parameters
        logical             :: save_xdip             ! process parameters
        logical             :: save_ydip             ! process parameters
        logical             :: save_maxdip           ! process parameters
        logical             :: save_azim             ! process parameters
        logical             :: save_mixed            ! process parameters
        logical             :: save_medge            ! process parameters
        logical             :: save_sedge            ! process parameters
        logical             :: save_middle           ! process parameters

        character(len=FILENAME_LENGTH) :: basename       ! process parameters
        character(len=FILENAME_LENGTH) :: path_orig      ! process parameters
        character(len=FILENAME_LENGTH) :: path_flat      ! process parameters
        character(len=FILENAME_LENGTH) :: path_semb      ! process parameters
        character(len=FILENAME_LENGTH) :: path_xdip      ! process parameters
        character(len=FILENAME_LENGTH) :: path_ydip      ! process parameters
        character(len=FILENAME_LENGTH) :: path_maxdip    ! process parameters
        character(len=FILENAME_LENGTH) :: path_azim      ! process parameters
        character(len=FILENAME_LENGTH) :: path_mixed     ! process parameters
        character(len=FILENAME_LENGTH) :: path_medge     ! process parameters
        character(len=FILENAME_LENGTH) :: path_sedge     ! process parameters
        character(len=FILENAME_LENGTH) :: path_middle    ! process parameters

        integer             :: which                     ! dependent
        real                :: dip_inc_x                 ! dependent
        real                :: dip_inc_y                 ! dependent
        integer             :: maxrec1                   ! dependent
        integer             :: maxrec2                   ! dependent
        integer             :: adjust                    ! dependent

        type(permtfile_struct),pointer :: file_orig      ! dependent
        type(permtfile_struct),pointer :: file_flat      ! dependent
        type(permtfile_struct),pointer :: file_semb      ! dependent
        type(permtfile_struct),pointer :: file_xdip      ! dependent
        type(permtfile_struct),pointer :: file_ydip      ! dependent
        type(permtfile_struct),pointer :: file_maxdip    ! dependent
        type(permtfile_struct),pointer :: file_azim      ! dependent
        type(permtfile_struct),pointer :: file_mixed     ! dependent
        type(permtfile_struct),pointer :: file_medge     ! dependent
        type(permtfile_struct),pointer :: file_sedge     ! dependent
        type(permtfile_struct),pointer :: file_middle    ! dependent

      end type sdiputil_struct


      contains


!!------------------------------ create -----------------------------------!!
!!------------------------------ create -----------------------------------!!
!!------------------------------ create -----------------------------------!!


      subroutine sdiputil_create (obj)
      implicit none
      type(sdiputil_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%file_orig)
      nullify (obj%file_flat)
      nullify (obj%file_semb)
      nullify (obj%file_xdip)
      nullify (obj%file_ydip)
      nullify (obj%file_maxdip)
      nullify (obj%file_azim)
      nullify (obj%file_mixed)
      nullify (obj%file_medge)
      nullify (obj%file_sedge)
      nullify (obj%file_middle)
      return
      end subroutine sdiputil_create


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine sdiputil_wrapup (obj)
      implicit none
      type(sdiputil_struct),intent(inout) :: obj       ! arguments

      call sdiputil_private_close (obj)
      return
      end subroutine sdiputil_wrapup


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine sdiputil_delete (obj)
      implicit none
      type(sdiputil_struct),pointer :: obj       ! arguments

      call sdiputil_private_close (obj)

      deallocate(obj)
      return
      end subroutine sdiputil_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine sdiputil_initialize (obj)
      implicit none
      type(sdiputil_struct),intent(inout) :: obj       ! arguments

      obj%save_orig     = .false.
      obj%save_flat     = .false.
      obj%save_semb     = .false.
      obj%save_xdip     = .false.
      obj%save_ydip     = .false.
      obj%save_maxdip   = .false.
      obj%save_azim     = .false.
      obj%save_mixed    = .false.
      obj%save_medge    = .false.
      obj%save_sedge    = .false.
      obj%save_middle   = .false.

      obj%basename      = PATHCHECK_EMPTY
      obj%path_orig     = PATHCHECK_EMPTY
      obj%path_flat     = PATHCHECK_EMPTY
      obj%path_semb     = PATHCHECK_EMPTY
      obj%path_xdip     = PATHCHECK_EMPTY
      obj%path_ydip     = PATHCHECK_EMPTY
      obj%path_maxdip   = PATHCHECK_EMPTY
      obj%path_azim     = PATHCHECK_EMPTY
      obj%path_mixed    = PATHCHECK_EMPTY
      obj%path_medge    = PATHCHECK_EMPTY
      obj%path_sedge    = PATHCHECK_EMPTY
      obj%path_middle   = PATHCHECK_EMPTY

      call sdiputil_private_close (obj)
      return
      end subroutine sdiputil_initialize


!!---------------------------- update --------------------------------------!!
!!---------------------------- update --------------------------------------!!
!!---------------------------- update --------------------------------------!!


      subroutine sdiputil_update (obj                 , opt_output      ,  &
                                  pwr_edge,                                &
                                  pwr_semb, opt_semb  , fctr_mix        ,  &
                                  win_len             , win_nsamp       ,  &
                                  quick_dip_weights   , quick_dip_search,  &
                                  dip_max_x           , dip_max_y       ,  &
                                  num_tr_dip_x        , num_tr_dip_y    ,  &
                                  num_tr_mix_x        , num_tr_mix_y    ,  &
                                  hdr_x               , hdr_y           ,  &
                                  x_inc               , y_inc           ,  &
                                  max_x_bins          , max_y_bins      ,  &
                                  nxgather            , nygather        ,  &
                                  nxmix               , nymix           ,  &
                                  nxdips              , nydips          ,  &
                                  dip_inc_x           , dip_inc_y       ,  &
                                  worker)
      implicit none
      type(sdiputil_struct),intent(inout) :: obj                    ! argument
      character(len=*)     ,intent(inout) :: opt_output             ! argument
      real                 ,intent(inout) :: pwr_edge               ! argument
      real                 ,intent(inout) :: pwr_semb               ! argument
      logical              ,intent(in)    :: opt_semb               ! argument
      real                 ,intent(inout) :: fctr_mix               ! argument
      real                 ,intent(inout) :: win_len                ! argument
      integer              ,intent(inout) :: win_nsamp              ! argument
      logical              ,intent(in)    :: quick_dip_weights      ! argument
      logical              ,intent(in)    :: quick_dip_search       ! argument

      real            ,intent(inout) :: dip_max_x   ,dip_max_y      ! argument
      integer         ,intent(inout) :: num_tr_dip_x,num_tr_dip_y   ! argument
      integer         ,intent(inout) :: num_tr_mix_x,num_tr_mix_y   ! argument
      integer         ,intent(inout) :: hdr_x       ,hdr_y          ! argument
      real            ,intent(inout) :: x_inc       ,y_inc          ! argument
      integer         ,intent(inout) :: max_x_bins  ,max_y_bins     ! argument
      integer         ,intent(out)   :: nxgather    ,nygather       ! argument
      integer         ,intent(out)   :: nxmix       ,nymix          ! argument
      integer         ,intent(out)   :: nxdips      ,nydips         ! argument
      real            ,intent(out)   :: dip_inc_x   ,dip_inc_y      ! argument
      integer,optional,intent(inout) :: worker                      ! argument
      integer                        :: numsteps,i1,i2,kount        ! local
      real                           :: tlength                     ! local

!----------get and verify and put owned parameters:

      call pc_get_global ('nwih'       , obj%nwih  )
      call pc_get_global ('ndpt'       , obj%ndpt  )
      call pc_get_global ('tstrt'      , obj%tstrt )
      call pc_get_global ('dt'         , obj%dt    )

      call pc_get          ('basename' , obj%basename)
      call pathcheck       ('basename' , obj%basename)

      i1 = index(obj%basename, '.', .true.)
      i2 = index(obj%basename, '/', .true.)

      if (i1 > i2) obj%basename(i1:) = ' '

      call pc_put          ('basename' , obj%basename)

  call sdiputil_private_getput (obj,'orig'  ,obj%save_orig  ,obj%path_orig, &
     &    worker)
  call sdiputil_private_getput (obj,'flat'  ,obj%save_flat  ,obj%path_flat, &
     &    worker)
  call sdiputil_private_getput (obj,'semb'  ,obj%save_semb  ,obj%path_semb, &
     &    worker)
  call sdiputil_private_getput (obj,'xdip'  ,obj%save_xdip  ,obj%path_xdip, &
     &    worker)
  call sdiputil_private_getput (obj,'ydip'  ,obj%save_ydip  ,obj%path_ydip, &
     &    worker)
  call sdiputil_private_getput (obj,'maxdip',obj%save_maxdip,obj%path_maxdip, &
     &    worker)
  call sdiputil_private_getput (obj,'azim'  ,obj%save_azim  ,obj%path_azim, &
     &    worker)
  call sdiputil_private_getput (obj,'mixed' ,obj%save_mixed ,obj%path_mixed, &
     &    worker)
  call sdiputil_private_getput (obj,'medge' ,obj%save_medge ,obj%path_medge, &
     &    worker)
  call sdiputil_private_getput (obj,'sedge' ,obj%save_sedge ,obj%path_sedge, &
     &    worker)
  call sdiputil_private_getput (obj,'middle',obj%save_middle,obj%path_middle, &
     &    worker)

!----------count and report the number of requested diagnostic files:

      kount = 0
      if (obj%path_orig   /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_flat   /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_semb   /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_xdip   /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_ydip   /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_maxdip /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_azim   /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_mixed  /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_medge  /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_sedge  /= PATHCHECK_EMPTY) kount = kount + 1
      if (obj%path_middle /= PATHCHECK_EMPTY) kount = kount + 1

      if (kount > 0) then
           call pc_put ('message', trim(string_ii2ss(kount))//  &
                                   ' diagnostic output files requested')
      else
           call pc_put ('message', 'no diagnostic output files requested')
      end if

!----------verify and adjust general input arguments:

      select case (opt_output(1:5))
  case('MIXED'); opt_output='MIXED TRACES'            ; obj%which=WHICH_MIXED
  case('MEEK '); opt_output='MEEK EDGE DETECTION'     ; obj%which=WHICH_MEDGE
  case('SEMBL'); opt_output='SEMBLANCE EDGE DETECTION'; obj%which=WHICH_SEDGE
  case('sembl'); opt_output='semblance values'        ; obj%which=WHICH_SEMB
  case('dip_x'); opt_output='dip_x values'            ; obj%which=WHICH_XDIP
  case('dip_y'); opt_output='dip_y values'            ; obj%which=WHICH_YDIP
  case('max_d'); opt_output='max_dip values'          ; obj%which=WHICH_MAXDIP
  case('azimu'); opt_output='azimuth values'          ; obj%which=WHICH_AZIM
  case('origi'); opt_output='original gathers'        ; obj%which=WHICH_ORIG
  case('flatt'); opt_output='flattened gathers'       ; obj%which=WHICH_FLAT
  case('middl'); opt_output='middle trace in gather'  ; obj%which=WHICH_MIDDLE
  case default ; opt_output='middle trace in gather'  ; obj%which=WHICH_MIDDLE
      end select

      if (win_len <= 0.0) then
           call pc_warning ('WIN_LEN MUST BE > ZERO - reset to 0.1')
           win_len = 0.1
      end if

      tlength = (obj%ndpt - 1) * obj%dt

      call mth_constrain (pwr_edge  , 1.0   ,   9.0)
      call mth_constrain (pwr_semb  , 0.0   ,   9.0)
      call mth_constrain (fctr_mix  , 0.0   ,   1.0)
      call mth_constrain (win_len   , obj%dt,        tlength, obj%dt)
      call mth_constrain (win_nsamp , 1     , 99)

!----------verify and adjust X and Y input arguments:

      if (num_tr_mix_x == INIL) num_tr_mix_x = num_tr_dip_x
      if (num_tr_mix_y == INIL) num_tr_mix_y = num_tr_dip_y

      call mth_constrain (dip_max_x   , 0.0,        999.9)
      call mth_constrain (dip_max_y   , 0.0,        999.9)
      call mth_constrain (num_tr_dip_x,   0,          999)
      call mth_constrain (num_tr_dip_y,   0,          999)
      call mth_constrain (num_tr_mix_x,   0, num_tr_dip_x)
      call mth_constrain (num_tr_mix_y,   0, num_tr_dip_y)

      if (hdr_x <= 0 .or. hdr_x > obj%nwih) then
           call pc_warning ('BAD X HEADER WORD NUMBER - reset to 7')
           hdr_x = 7
      end if

      if (hdr_y <= 0 .or. hdr_y > obj%nwih) then
           call pc_warning ('BAD Y HEADER WORD NUMBER - reset to 8')
           hdr_x = 8
      end if

      if (x_inc <= 0.0) then
           call pc_warning ('X BIN INCREMENT MUST BE > ZERO - reset to 1.0')
           x_inc = 1.0
      end if

      if (y_inc <= 0.0) then
           call pc_warning ('Y BIN INCREMENT MUST BE > ZERO - reset to 1.0')
           y_inc = 1.0
      end if

      if (max_x_bins == INIL .or. max_x_bins <= 0) then
           call pc_error ('MAX_X_BINS MUST BE SPECIFIED')
           max_x_bins = INIL
      end if

      if (max_y_bins == INIL .or. max_y_bins <= 0) then
      if (obj%path_orig   /= PATHCHECK_EMPTY .or.  &
          obj%path_flat   /= PATHCHECK_EMPTY .or.  &
          obj%path_semb   /= PATHCHECK_EMPTY .or.  &
          obj%path_xdip   /= PATHCHECK_EMPTY .or.  &
          obj%path_ydip   /= PATHCHECK_EMPTY .or.  &
          obj%path_maxdip /= PATHCHECK_EMPTY .or.  &
          obj%path_azim   /= PATHCHECK_EMPTY .or.  &
          obj%path_mixed  /= PATHCHECK_EMPTY .or.  &
          obj%path_medge  /= PATHCHECK_EMPTY .or.  &
          obj%path_sedge  /= PATHCHECK_EMPTY .or.  &
          obj%path_middle /= PATHCHECK_EMPTY) then
           call pc_error ('MAX_Y_BINS MUST BE SPECIFIED &
                           &WHEN REQUESTING DIAGNOSTIC OUTPUT FILES')
           max_y_bins = INIL
      end if
      end if

!----------calculate X and Y output arguments:

      nxgather  = 2 * num_tr_dip_x  + 1
      nygather  = 2 * num_tr_dip_y  + 1
      nxmix     = 2 * num_tr_mix_x  + 1
      nymix     = 2 * num_tr_mix_y  + 1
      nxdips    = 1
      nydips    = 1
      dip_inc_x = 1.0
      dip_inc_y = 1.0

      if (dip_max_x > 0.0 .and. num_tr_dip_x > 0) then
           dip_inc_x = 1000.0 * obj%dt / num_tr_dip_x
           dip_inc_x = min(dip_inc_x, dip_max_x)
           numsteps  = nint(0.4 + dip_max_x / dip_inc_x)
           nxdips    = 2 * numsteps + 1
           dip_inc_x = dip_max_x / numsteps
      end if

      if (dip_max_y > 0.0 .and. num_tr_dip_y > 0) then
           dip_inc_y = 1000.0 * obj%dt / num_tr_dip_y
           dip_inc_y = min(dip_inc_y, dip_max_y)
           numsteps  = nint(0.4 + dip_max_y / dip_inc_y)
           nydips    = 2 * numsteps + 1
           dip_inc_y = dip_max_y / numsteps
      end if

      call pc_print ('SDIPUTIL: dip_max_x =',dip_max_x,  &
                             '  dip_inc_x =',dip_inc_x,  &
                             '  nxdips = '//string_ii2ss(nxdips))
      call pc_print ('SDIPUTIL: dip_max_x =',dip_max_x,  &
                             '  dip_inc_y =',dip_inc_y,  &
                             '  nydips = '//string_ii2ss(nydips))

      if (obj%path_medge /= PATHCHECK_EMPTY .or. &
          obj%which      == WHICH_MEDGE) then
           if (nxmix * nymix <= win_nsamp) then
             call pc_error ('NXMIX * NYMIX MUST &
                           &BE LARGER THAN WIN_NSAMP FOR MEEK EDGE DETECTION')
           end if
      end if

!----------put control parameters:

      if (obj%which == WHICH_ORIG .or. obj%which == WHICH_FLAT) then
           call pc_put_global  ('gathered'   , .true.)
           call pc_put_global  ('numtr'      , nxgather * nygather)
      else
           call pc_put_global  ('gathered'   , .false.)
           call pc_put_global  ('numtr'      , 1)
      end if

!----------save parameters needed by sdiputil:

      obj%pwr_edge          = pwr_edge
      obj%pwr_semb          = pwr_semb
      obj%opt_semb          = opt_semb            
      obj%fctr_mix          = fctr_mix  
      obj%win_len           = win_len   
      obj%win_nsamp         = win_nsamp 
      obj%quick_dip_weights = quick_dip_weights   
      obj%quick_dip_search  = quick_dip_search    
      obj%dip_max_x         = dip_max_x
      obj%dip_max_y         = dip_max_y
      obj%dip_inc_x         = dip_inc_x
      obj%dip_inc_y         = dip_inc_y
      obj%maxrec1           = max_x_bins * max_y_bins
      obj%maxrec2           = max_x_bins * max_y_bins * nxgather * nygather
      return
      end subroutine sdiputil_update


!!---------------------------- prepare ------------------------------------!!
!!---------------------------- prepare ------------------------------------!!
!!---------------------------- prepare ------------------------------------!!


      subroutine sdiputil_prepare (obj, nhx,nhy,nhw)
      implicit none
      type(sdiputil_struct),intent(inout) :: obj             ! arguments
      integer              ,intent(in)    :: nhx,nhy,nhw     ! arguments

      obj%nhx = nhx
      obj%nhy = nhy
      obj%nhw = nhw

      obj%adjust = SEMDIP_ADJUST_NONE

      if (obj%which      == WHICH_MEDGE     .or. &
          obj%which      == WHICH_SEDGE     .or. &
          obj%path_medge /= PATHCHECK_EMPTY .or. &
          obj%path_sedge /= PATHCHECK_EMPTY) then
           obj%adjust = SEMDIP_ADJUST_POSITIVE
      end if

      if (obj%which      == WHICH_FLAT     .or. &
          obj%path_flat  /= PATHCHECK_EMPTY) then
           obj%adjust = SEMDIP_ADJUST_ALL
      end if

      call sdiputil_private_close (obj)

  call sdiputil_private_open (obj,obj%path_orig  ,obj%file_orig  ,obj%maxrec2)
  call sdiputil_private_open (obj,obj%path_flat  ,obj%file_flat  ,obj%maxrec2)
  call sdiputil_private_open (obj,obj%path_semb  ,obj%file_semb  ,obj%maxrec1)
  call sdiputil_private_open (obj,obj%path_xdip  ,obj%file_xdip  ,obj%maxrec1)
  call sdiputil_private_open (obj,obj%path_ydip  ,obj%file_ydip  ,obj%maxrec1)
  call sdiputil_private_open (obj,obj%path_maxdip,obj%file_maxdip,obj%maxrec1)
  call sdiputil_private_open (obj,obj%path_azim  ,obj%file_azim  ,obj%maxrec1)
  call sdiputil_private_open (obj,obj%path_mixed ,obj%file_mixed ,obj%maxrec1)
  call sdiputil_private_open (obj,obj%path_medge ,obj%file_medge ,obj%maxrec1)
  call sdiputil_private_open (obj,obj%path_sedge ,obj%file_sedge ,obj%maxrec1)
  call sdiputil_private_open (obj,obj%path_middle,obj%file_middle,obj%maxrec1)
      return
      end subroutine sdiputil_prepare


!!--------------------------- private getput --------------------------------!!
!!--------------------------- private getput --------------------------------!!
!!--------------------------- private getput --------------------------------!!


      subroutine sdiputil_private_getput (obj, name, saveit, path, worker)
      implicit none
      type(sdiputil_struct) ,intent(inout)       :: obj           ! argument
      character(len=*)      ,intent(in)          :: name          ! argument
      logical               ,intent(inout)       :: saveit        ! argument
      character(len=*)      ,intent(out)         :: path          ! argument
      integer,optional      ,intent(inout)       :: worker        ! argument
      character(len=24)                          :: keypath       ! local
      character(len=24)                          :: keysave       ! local
      character(len=20)                          :: worker_str    ! local

      keypath = 'path_'//name
      keysave = 'save_'//name

      call pc_get          (keysave, saveit)

      if (.not.saveit) then
           path = PATHCHECK_EMPTY
      else if (obj%basename == PATHCHECK_EMPTY) then
           path = PATHCHECK_EMPTY
      else if(.not.present(worker)) then
           path = trim(obj%basename)//'_'//name
      else  if(worker .gt. 0) then
           write(worker_str, *) '.',worker,'.'
           path = trim(obj%basename)//'_'//name//worker_str
      else
           path = trim(obj%basename)//'_'//name
      end if

      call pathcheck       (keypath, path, '.trc8', show=PATHCHECK_INFO_OUTPUT)
      call pc_put_gui_only (keypath, path)
      call pc_put          (keysave, saveit)

      call pc_put_sensitive_field_flag &
                           (keysave, obj%basename /= PATHCHECK_EMPTY)
      return
      end subroutine sdiputil_private_getput


!!--------------------------- private open ---------------------------------!!
!!--------------------------- private open ---------------------------------!!
!!--------------------------- private open ---------------------------------!!


      subroutine sdiputil_private_open (obj, path, file, maxrecords)
      implicit none
      type(sdiputil_struct) ,intent(inout)       :: obj           ! argument
      character(len=*)      ,intent(in)          :: path          ! argument
      type(permtfile_struct),pointer             :: file          ! argument
      integer               ,intent(in)          :: maxrecords    ! argument
      integer                                    :: err,lun,ipn   ! local
      integer               ,parameter           :: NBITS = 8     ! local

      if (path == PATHCHECK_EMPTY) return

      lun = pc_get_lun()
      ipn = pc_get_ipn()

      call permtfile_open_write (file, path, obj%nwih, obj%ndpt,   &
                                 obj%tstrt, obj%dt, lun, err,  &
                                 ipn, NBITS, maxrecords)

      if (err /= PERMTFILE_OK) call pc_error ('SDIPUTIL: error opening',path)
      return
      end subroutine sdiputil_private_open


!!---------------------------- private close -------------------------------!!
!!---------------------------- private close -------------------------------!!
!!---------------------------- private close -------------------------------!!


      subroutine sdiputil_private_close (obj)
      implicit none
      type(sdiputil_struct),intent(inout) :: obj       ! arguments

      call permtfile_close (obj%file_orig  )
      call permtfile_close (obj%file_flat  )
      call permtfile_close (obj%file_semb  )
      call permtfile_close (obj%file_xdip  )
      call permtfile_close (obj%file_ydip  )
      call permtfile_close (obj%file_maxdip)
      call permtfile_close (obj%file_azim  )
      call permtfile_close (obj%file_middle)
      call permtfile_close (obj%file_mixed )
      call permtfile_close (obj%file_medge )
      call permtfile_close (obj%file_sedge )
      return
      end subroutine sdiputil_private_close


!!--------------------------- private write many ---------------------------!!
!!--------------------------- private write many ---------------------------!!
!!--------------------------- private write many ---------------------------!!

! uses ntr if saving the gather to a file.
! uses ntr if outputting the gather to the next process.
! sets ntr to FATAL_ERROR if an error occurs.
! does nothing if not saving or outputting the gather.


      subroutine sdiputil_private_write_many (obj,which,ntr,hdo,tro, &
                                              file,hdgather,trgather)
      implicit none
      type(sdiputil_struct) ,intent(inout) :: obj               ! argument
      integer               ,intent(in)    :: which             ! argument
      integer               ,intent(inout) :: ntr               ! argument
      double precision      ,intent(out)   :: hdo(:,:)          ! argument
      real                  ,intent(out)   :: tro(:,:)          ! argument
      type(permtfile_struct),pointer       :: file              ! argument
      double precision      ,intent(inout) :: hdgather(:,:)     ! argument
      real                  ,intent(in)    :: trgather(:,:)     ! argument
      integer                              :: err,i,ntr2        ! local

      if (associated(file)) then
           ntr2 = ntr
           do i = 1,ntr2
                call permtfile_write (file, hdgather(:,i), trgather(:,i), err)
                if (err /= PERMTFILE_OK) then
                     call pc_error ('SDIPUTIL: error writing to output file')
                     ntr = FATAL_ERROR
                     return
                end if
           end do
      end if

      if (obj%which == which) then
           hdo(1:obj%nwih,1:ntr) = hdgather(1:obj%nwih,1:ntr)
           tro(1:obj%ndpt,1:ntr) = trgather(1:obj%ndpt,1:ntr)
      end if
      return
      end subroutine sdiputil_private_write_many


!!--------------------------- private write one ----------------------------!!
!!--------------------------- private write one ----------------------------!!
!!--------------------------- private write one ----------------------------!!

! sets ntr to 1 if outputting the trace to the next process.
! sets ntr to FATAL_ERROR if an error occurs.
! does nothing if not saving or outputting the trace.


      subroutine sdiputil_private_write_one (obj,which,ntr,hdo,tro, &
                                             file,header,trace)
      implicit none
      type(sdiputil_struct) ,intent(inout) :: obj               ! argument
      integer               ,intent(in)    :: which             ! argument
      integer               ,intent(out)   :: ntr               ! argument
      double precision      ,intent(out)   :: hdo(:,:)          ! argument
      real                  ,intent(out)   :: tro(:,:)          ! argument
      type(permtfile_struct),pointer       :: file              ! argument
      double precision      ,intent(inout) :: header(:)         ! argument
      real                  ,intent(in)    :: trace (:)         ! argument
      integer                              :: err               ! local

      if (associated(file)) then
           call permtfile_write (file, header, trace, err)
           if (err /= PERMTFILE_OK) then
                call pc_error ('SDIPUTIL: error writing to output file')
                ntr = FATAL_ERROR
                return
           end if
      end if

      if (obj%which == which) then
           ntr = 1
           hdo(1:obj%nwih,1) = header(1:obj%nwih)
           tro(1:obj%ndpt,1) = trace (1:obj%ndpt)
      end if
      return
      end subroutine sdiputil_private_write_one


!!----------------------------- solve ---------------------------------------!!
!!----------------------------- solve ---------------------------------------!!
!!----------------------------- solve ---------------------------------------!!


      subroutine sdiputil_solve (obj,ntr,hdgather,trgather,xmid,ymid,hdo,tro)
      implicit none
      type(sdiputil_struct),intent(inout) :: obj                  ! argument
      integer              ,intent(inout) :: ntr                  ! argument
      double precision     ,intent(inout) :: hdgather(:,:)        ! argument
      real                 ,intent(inout) :: trgather(:,:)        ! argument
      real                 ,intent(in)    :: xmid,ymid            ! argument
      double precision     ,intent(out)   :: hdo(:,:)             ! argument
      real                 ,intent(out)   :: tro(:,:)             ! argument
      integer                        :: mid,i ! local
      real                           :: dist,xdist,ydist,xydist   ! local
      real                           :: semb_trace   (obj%ndpt)   ! local
      real                           :: xdip_trace   (obj%ndpt)   ! local
      real                           :: ydip_trace   (obj%ndpt)   ! local
      real                           :: maxdip_trace (obj%ndpt)   ! local
      real                           :: azim_trace   (obj%ndpt)   ! local
      real                           :: mixed_trace  (obj%ndpt)   ! local
      real                           :: medge_trace  (obj%ndpt)   ! local
      real                           :: sedge_trace  (obj%ndpt)   ! local
      double precision               :: middle_header(obj%nwih)   ! local
      real                           :: middle_trace (obj%ndpt)   ! local

!----------get started:

      mid  = 0
      dist = FNIL

      do i = 1,ntr
           xdist  = hdgather(obj%nhx,i) - xmid
           ydist  = hdgather(obj%nhy,i) - ymid
           xydist = xdist**2 + ydist**2
           if (dist == FNIL .or. xydist < dist) then
                dist = xydist
                mid  = i
           end if
      end do

      middle_header(1:obj%nwih) = hdgather(1:obj%nwih,mid)
      middle_trace (1:obj%ndpt) = trgather(1:obj%ndpt,mid)

      middle_header(obj%nhx) = xmid             ! added 2002-07-30
      middle_header(obj%nhy) = ymid             ! added 2002-07-30

      do i = 1,ntr
           hdgather(3,i) = middle_header(1)
           hdgather(4,i) = i
      end do

!----------save and/or return original gather:

      call sdiputil_private_write (obj, WHICH_ORIG, ntr, hdo, tro, &
                                   obj%file_orig, hdgather, trgather)
      if (ntr == FATAL_ERROR) return

!----------do the dip search and mix:

      call semdip (ntr,hdgather,trgather,obj%ndpt,obj%dt,obj%win_len,  &
                   obj%nhx,obj%nhy,obj%nhw,                            &
                   obj%dip_max_x,obj%dip_max_y,                        &
                   obj%dip_inc_x,obj%dip_inc_y,                        &
                   middle_header,mixed_trace,obj%adjust,               &
                   obj%quick_dip_weights,obj%quick_dip_search,         &
                   semb_trace,xdip_trace,ydip_trace,maxdip_trace,azim_trace)

!----------save and/or return flattened gather:

      call sdiputil_private_write (obj, WHICH_FLAT, ntr, hdo, tro, &
                                   obj%file_flat, hdgather, trgather)
      if (ntr == FATAL_ERROR) return

!----------calculate the required output traces:

      if (associated(obj%file_mixed) .or. obj%which == WHICH_MIXED) then
        call sdiputil_scale (obj,ntr,middle_trace,semb_trace,mixed_trace)
      end if

      if (associated(obj%file_medge) .or. obj%which == WHICH_MEDGE) then
        call sdiputil_meek_edge (obj,ntr,hdgather,trgather,medge_trace)
      end if

      if (associated(obj%file_sedge) .or. obj%which == WHICH_SEDGE) then
        call sdiputil_semblance_edge &
                       (obj,ntr,hdgather,trgather,middle_header,sedge_trace)
      end if

!----------save and/or return trace:

      call sdiputil_private_write (obj, WHICH_SEMB, ntr, hdo, tro, &
                                   obj%file_semb, middle_header, semb_trace)
      if (ntr == FATAL_ERROR) return

      call sdiputil_private_write (obj, WHICH_XDIP, ntr, hdo, tro, &
                                   obj%file_xdip, middle_header, xdip_trace)
      if (ntr == FATAL_ERROR) return

      call sdiputil_private_write (obj, WHICH_YDIP, ntr, hdo, tro, &
                                   obj%file_ydip, middle_header, ydip_trace)
      if (ntr == FATAL_ERROR) return

      call sdiputil_private_write (obj, WHICH_MAXDIP, ntr, hdo, tro, &
                                   obj%file_maxdip, middle_header, maxdip_trace)
      if (ntr == FATAL_ERROR) return

      call sdiputil_private_write (obj, WHICH_AZIM, ntr, hdo, tro, &
                                   obj%file_azim, middle_header, azim_trace)
      if (ntr == FATAL_ERROR) return

      call sdiputil_private_write (obj, WHICH_MIXED, ntr, hdo, tro, &
                                   obj%file_mixed, middle_header, mixed_trace)
      if (ntr == FATAL_ERROR) return

      call sdiputil_private_write (obj, WHICH_MEDGE, ntr, hdo, tro, &
                                   obj%file_medge, middle_header, medge_trace)
      if (ntr == FATAL_ERROR) return

      call sdiputil_private_write (obj, WHICH_SEDGE, ntr, hdo, tro, &
                                   obj%file_sedge, middle_header, sedge_trace)
      if (ntr == FATAL_ERROR) return

      call sdiputil_private_write (obj, WHICH_MIDDLE, ntr, hdo, tro, &
                                   obj%file_middle, middle_header, middle_trace)
      if (ntr == FATAL_ERROR) return

!----------finish up and return:

      call lav_set_hdr (hdo,tro,obj%ndpt,ntr)
      return
      end subroutine sdiputil_solve


!!---------------------------- sdiputil scale -------------------------------!!
!!---------------------------- sdiputil scale -------------------------------!!
!!---------------------------- sdiputil scale -------------------------------!!

! subroutine to scale the mixed trace.
! MIDDLE_TRACE(ndpt) = original middle trace from gather which was mixed.
! MIXED_TRACE (ndpt) = mixed trace (input and output).
! MIXED_TRACE is changed by adding part of the original middle trace back in.
! optional semblance scaling and weighting is also done.

      subroutine sdiputil_scale (obj,ntr,middle_trace,semb_trace,mixed_trace)
      implicit none
      type(sdiputil_struct),intent(in)    :: obj                  ! argument
      integer              ,intent(in)    :: ntr                  ! argument
      real                 ,intent(in)    :: middle_trace(:)      ! argument
      real                 ,intent(in)    :: semb_trace  (:)      ! argument
      real                 ,intent(inout) :: mixed_trace (:)      ! argument
      integer                             :: j                    ! local
      real                                :: f1,f2,ww             ! local

!----------get started.

      if (ntr == 1) return

      f1  = obj%fctr_mix
      f2  = 1.0 - f1

!----------semblance is not to be used.

      if (obj%pwr_semb == 0.0) then
           do j = 1,obj%ndpt
                mixed_trace(j) = middle_trace(j) * f1 + mixed_trace(j) * f2
           end do
           return
      end if

!----------do the weighting by semblance.

      do j = 1,obj%ndpt

        ww = semb_trace(j)
        if (ww > 0.0) ww = ww**obj%pwr_semb

        if (obj%opt_semb) then
          mixed_trace(j) = (middle_trace(j) * f1 + mixed_trace(j) * f2) * ww
        else
          mixed_trace(j) = (middle_trace(j) * f1) + (mixed_trace(j) * f2 * ww)
        end if

      end do
      return
      end subroutine sdiputil_scale


!!------------------------ sdiputil semblance edge --------------------------!!
!!------------------------ sdiputil semblance edge --------------------------!!
!!------------------------ sdiputil semblance edge --------------------------!!

! subroutine to calculate an edge detection attribute trace.
! MIDDLE_HEADER(nwih) = header for edge detection trace.
! SEDGE_TRACE  (ndpt) = edge detection attribute trace upon output.


      subroutine sdiputil_semblance_edge &
                       (obj,ntr,hdgather,trgather,middle_header,sedge_trace)
      implicit none
      type(sdiputil_struct),intent(in)    :: obj                  ! argument
      integer              ,intent(in)    :: ntr                  ! argument
      double precision     ,intent(inout) :: hdgather(:,:)        ! argument
      real                 ,intent(inout) :: trgather(:,:)        ! argument
      double precision     ,intent(in)    :: middle_header(:)     ! argument
      real                 ,intent(out)   :: sedge_trace  (:)     ! argument
      real                                :: win_len              ! local
      integer                             :: i,j                  ! local

!----------get semblances in subset of flattened trace gather.

      do i=1,ntr
         if (hdgather(obj%nhw,i) == 0.0) hdgather(obj%nhw,i) = -1.0
      end do

      win_len = obj%win_nsamp * obj%dt

      call semdip (ntr,hdgather,trgather,obj%ndpt,obj%dt,win_len,    &
                   obj%nhx,obj%nhy,obj%nhw,                          &
                   0.0, 0.0, 1.0, 1.0,                               &
                   middle_header,                                    &
                   adjust=SEMDIP_ADJUST_NONE,                        &
                   semb_trace=sedge_trace)

!----------adjust semblance values.

      do j = 1,obj%ndpt
           if (sedge_trace(j) == 1.0) then
                sedge_trace(j) = 0.0
           else
                sedge_trace(j) = (1.0 - sedge_trace(j)) **obj%pwr_edge
  !!!           sedge_trace(j) =  1.0 - sedge_trace(j)  **obj%pwr_edge
           end if
      end do
      return
      end subroutine sdiputil_semblance_edge


!!------------------------- sdiputil meek edge ------------------------------!!
!!------------------------- sdiputil meek edge ------------------------------!!
!!------------------------- sdiputil meek edge ------------------------------!!

! subroutine to calculate an edge detection attribute trace.
! MEDGE_TRACE(ndpt) = edge detection attribute trace upon output.


      subroutine sdiputil_meek_edge (obj,ntr,hdgather,trgather,medge_trace)
      implicit none
      type(sdiputil_struct),intent(in)  :: obj                      ! argument
      integer              ,intent(in)  :: ntr                      ! argument
      double precision     ,intent(in)  :: hdgather(:,:)            ! argument
      real                 ,intent(in)  :: trgather(:,:)            ! argument
      real                 ,intent(out) :: medge_trace(:)           ! argument
      integer                           :: nhalf,imute              ! local
      real                              :: aaa(ntr,obj%win_nsamp)   ! local
      real                              :: sss(    obj%win_nsamp)   ! local
      integer                           :: itime,igather,isamp      ! local
      integer                           :: itime2,kount ! local
      real                              :: sum                      ! local

!----------get mute time.

      imute = obj%ndpt
      DO igather = 1,NTR
           if (hdgather(obj%nhw,igather) <= 0.0) cycle
           imute = min(imute,nint(hdgather(2,igather)))
      end do
      imute = max(imute-3,1)      ! keep a little bit of filter tail.

      medge_trace(1:imute) = 0.0

!----------get started.

      nhalf = obj%win_nsamp / 2

      do itime = imute,obj%ndpt

!----------create data matrix (ntr traces total in x & y direction).

         aaa(:,:) = 0.0
         kount = 0
         do igather = 1, ntr
             if (hdgather(obj%nhw,igather) <= 0.0) cycle
             kount = kount + 1
             do isamp = 1, obj%win_nsamp
               itime2 = itime - nhalf - 1 + isamp
               itime2 = max(1       ,itime2)
               itime2 = min(obj%ndpt,itime2)
               aaa(kount,isamp) = trgather(itime2,igather)
             end do
         end do

!----------do singular value decomposition.
!----------rsvd will set sss(:)=0 and sss(1)=1 if it tries to divide by zero.

         if (kount <= obj%win_nsamp) then              ! restriction in RSVD.
           sss(:) = 0.0
         else
                 !!    a  mmax      nmax      m          n      ip nu nv   s
                 !!    |   |         |        |          |       |  |  |   |
           call rsvd (aaa,ntr,obj%win_nsamp,kount,obj%win_nsamp, 0, 0, 0, sss)
         end if

!----------put edge detection attribute into output trace.

         sum = 0.0
         do isamp = 1, obj%win_nsamp
              sss(isamp) = max(sss(isamp),0.0)
              if (isamp == 1 .or. sss(isamp) > 0.01 * sss(1)) &
                                         sum = sum + sss(isamp)**obj%pwr_edge
         end do

         if (sum == 0.0) then
            medge_trace(itime) = 0.0
         else if (sss(1) == 0.0) then
            medge_trace(itime) = 1.0
         else
            medge_trace(itime) = 1.0 - sss(1)**obj%pwr_edge / sum
         endif

!----------finish up and return.

      end do
      return
      end subroutine sdiputil_meek_edge


!!------------------------- sdiputil get_filepath ---------------------------!!
!!------------------------- sdiputil get_filepath ---------------------------!!
!!------------------------- sdiputil get_filepath ---------------------------!!

! subroutine to get the filepath names, based on an index (id).
      subroutine sdiputil_get_filepath(obj, id, path)
      implicit none
      type(sdiputil_struct),          intent(in)    :: obj         ! argument
      integer,                        intent(inout) :: id          ! argument
      character(len=FILENAME_LENGTH), intent(inout) :: path        ! argument

      select case(id)
         case (1)
            path = obj%path_orig
         case (2)
            path = obj%path_flat
         case (3)
            path = obj%path_semb
         case (4)
            path = obj%path_xdip
         case (5)
            path = obj%path_ydip
         case (10)
            path = obj%path_maxdip
         case (11)
            path = obj%path_azim
         case (6)
            path = obj%path_mixed
         case (7)
            path = obj%path_medge
         case (8)
            path = obj%path_sedge
         case (9)
            path = obj%path_middle
         case default
            path = PATHCHECK_EMPTY
      end select

      end subroutine sdiputil_get_filepath

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module sdiputil_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

