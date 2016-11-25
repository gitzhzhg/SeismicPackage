!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- geomdata.f90 -------------------------------!!
!!----------------------------- geomdata.f90 -------------------------------!!
!!----------------------------- geomdata.f90 -------------------------------!!


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
! Name       : GEOMDATA
! Category   : math
! Written    : 2000-04-04   by: Tom Stoeckley
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Maintain field geometry data.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This module maintains field geometry data.
!
! This module also contains the definition of a data structure which maintains
! information needed to assist in the creation of trace headers.  This data
! structure, called GEOMDATA_ITERATOR, contains counters, card numbers, and
! other information which gets updated as trace headers are created and
! returned by GEOMDATA.  An instance of the GEOMDATA_ITERATOR must be owned
! by the calling routine and passed to GEOMDATA to maintain each time the
! calling routine asks GEOMDATA for the next trace header.  The calling
! routine has no access to the contents of this structure.  This arrangement
! allows a single GEOMDATA object to be used by more than one calling routine
! to create trace headers simultaneously.  The GEOMDATA object itself does
! not change while it is being used to create trace headers.
!
! The reason that the GEOMDATA_ITERATOR is not in a separate module is to
! allow it full access to all of the GEOMDATA data without making all that
! data public.
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
!                        CREATE AND DELETE THE DATA
!
!                               o
!        call geomdata_create (obj)   ! to create and clear the data structure.
!        call geomdata_delete (obj)   ! to delete the data structure.
!                               b
!
! type(geomdata_struct),pointer OBJ = pointer to the GEOMDATA data structure.
!
! GEOMDATA_CREATE allocates and clears the data structure.
! GEOMDATA_DELETE deallocates the data structure.
!
!-------------------------------------------------------------------------------
!                       INITIALIZE OR CLEAR THE DATA
!
!                                   b  i    i     i      i
!        call geomdata_initialize (obj,ve,datum,fixdist,grid,
!                        chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4,scan)
!                           i      i   i   i   i    i    i    i    i
!                                                                 opt
!        call geomdata_clear      (obj)
!                                   b
!
! type(geomdata_struct)  obj = the GEOMDATA data structure.
! real                    ve = reference velocity.
! real                 datum = datum elevation.
! real               fixdist = uniform inline distance parameter.
! type(grid_struct)     grid = grid transformation structure.
! char(*)           chaining = chaining flag.
! integer                nld = number of LD cards.
! integer                nrp = number of RP cards.
! integer                npp = number of PP cards.
! integer               nzt1 = number of ZT1 cards.
! integer               nzt2 = number of ZT2 cards.
! integer               nzt3 = number of ZT3 cards.
! integer               nzt4 = number of ZT4 cards.
! logical               scan = whether to just scan the data (default false).
!
! If SCAN is present and true, no data arrays will be stored in this object,
! and the calls to add individual cards to the data will do validation checks
! only.
!
!-------------------------------------------------------------------------------
!                    ADD INDIVIDUAL CARDS TO THE DATA
!                   (after calling geomdata_initialize)
!
!      (must call for each LD  card with ild  = 1,nld  consecutively)
!      (must call for each RP  card with irp  = 1,nrp  consecutively)
!      (must call for each PP  card with ipp  = 1,npp  consecutively)
!      (must call for each ZT1 card with izt1 = 1,nzt1 consecutively)
!      (must call for each ZT2 card with izt2 = 1,nzt2 consecutively)
!      (must call for each ZT3 card with izt3 = 1,nzt3 consecutively)
!      (must call for each ZT4 card with izt4 = 1,nzt4 consecutively)
!
!                                 b   i
!     call geomdata_set_ld_card (obj,ild,
!             sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
!             i   i    i    i    i     i    i  i  i   i   i   i    i
!
!                                 b   i
!     call geomdata_set_rp_card (obj,irp,
!             ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
!               i    i    i    i   i    i   i    i    i    i     i
!
!                                 b   i
!     call geomdata_set_pp_card (obj,ipp,
!        sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
!         i    i    i    i     i    i    i    i     i     i     i   i  i  i
!
!                                  b   i
!     call geomdata_set_zt1_card (obj,izt1,
!                                 ccc1,sss1,sss1a,lll1)
!                                  i    i     i    i
!
!                                  b   i
!     call geomdata_set_zt2_card (obj,izt2,
!                                 ccc2,rrr2,rrr2a,lll2)
!                                  i    i     i    i
!
!                                  b   i
!     call geomdata_set_zt3_card (obj,izt3,
!                                 ccc3,iggg3,iggg3a,ittt3,ittt3a)
!                                  i     i     i      i     i
!
!                                  b   i
!     call geomdata_set_zt4_card (obj,izt4,
!                                 ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
!                                  i    i     i    i    i     i     i
!
! type(geomdata_struct)  obj = the GEOMDATA data structure.
!
! integer ild    = LD card number.
! integer irp    = RP card number.
! integer ipp    = PP card number.
! integer izt1   = ZT1 card number.
! integer izt2   = ZT2 card number.
! integer izt3   = ZT3 card number.
! integer izt4   = ZT4 card number.
!
! real    sp     = LD card: (nil OK) shotpoint.
! real    dist   = LD card: (nil OK) distance to next flag.
! real    xloc   = LD card: (nil OK) surveyed X location.
! real    yloc   = LD card: (nil OK) surveyed Y location.
! real    elev   = LD card: (nil OK) elevation.
! real    depth  = LD card: (nil OK) hole depth.
! real    tuh    = LD card: (nil OK) uphole time.
! real    tr     = LD card: (nil OK) receiver static.
! real    ts     = LD card: (nil OK) source static.
! real    xsd    = LD card: (nil OK) inline skid.
! real    ysd    = LD card: (nil OK) crossline skid.
! real    elsd   = LD card: (nil OK) elevation skid.
! integer line   = LD card:          line number.
!
! integer ipat1  = RP card: pattern number.
! char(*) flag   = RP card: flag.
! real    sp1    = RP card: first receiver shotpoint.
! integer line1  = RP card: first receiver line number.
! integer nx     = RP card: number of receiver flags along line.
! integer ixinc  = RP card: receiver flag increment along line.
! integer ny     = RP card: number of receiver flags across lines.
! integer iyinc  = RP card: receiver flag increment across lines.
! real    xsd1   = RP card: receiver pattern inline skid.
! real    ysd1   = RP card: receiver pattern crossline skid.
! real    elsd1  = RP card: receiver pattern elevation skid.
!
! real    sp2    = PP card: (nil OK) source shotpoint.
! integer line2  = PP card: (nil OK) source line number.
! real    sp3    = PP card: (nil OK) first receiver shotpoint.
! integer line3  = PP card: (nil OK) first receiver line number.
! integer ipat2  = PP card: (nil OK) pattern number.
! real    xsd2   = PP card: (nil OK) source inline skid.
! real    ysd2   = PP card: (nil OK) source crossline skid.
! integer hold   = PP card: (nil OK) how many groups to hold skid.
! real    elev2  = PP card: (nil OK) new source elevation.
! real    depth2 = PP card: (nil OK) new hole depth.
! real    tuh2   = PP card: (nil OK) new uphole time.
! integer is     = PP card: (nil OK) flag moveup to subsequent sources.
! integer ir     = PP card: (nil OK) flag moveup to subsequent first receivers.
! integer ig     = PP card: (nil OK) group number (shot profile number).
!
! char(*) ccc1   = ZT1 card: code.
! real    sss1   = ZT1 card: first source shotpoint affected.
! real    sss1a  = ZT1 card: last source shotpoint affected.
! integer lll1   = ZT1 card: source line number.
!
! char(*) ccc2   = ZT2 card: code.
! real    rrr2   = ZT2 card: first receiver shotpoint affected.
! real    rrr2a  = ZT2 card: last receiver shotpoint affected.
! integer lll2   = ZT2 card: receiver line number.
!
! char(*) ccc3   = ZT3 card: code.
! integer iggg3  = ZT3 card: first group number affected.
! integer iggg3a = ZT3 card: last group number affected.
! integer ittt3  = ZT3 card: first trace number affected.
! integer ittt3a = ZT3 card: last trace number affected.
!
! char(*) ccc4   = ZT4 card: code.
! real    sss4   = ZT4 card: first source shotpoint affected.
! real    sss4a  = ZT4 card: last source shotpoint affected.
! integer lll4   = ZT4 card: source line number.
! real    rrr4   = ZT4 card: first receiver shotpoint affected.
! real    rrr4a  = ZT4 card: last receiver shotpoint affected.
! integer lll4a  = ZT4 card: receiver line number.
!
!-------------------------------------------------------------------------------
!                           VALIDATE THE DATA
!              (after all cards have been added to the data)
!
!                                    b    i       i        o    o 
!           call geomdata_validate (obj, lun, opt_tables, err, msg)
!
!
! type(geomdata_struct)   obj = the GEOMDATA data structure.
! integer                 lun = logical unit number for printing (or 0).
! character(len=*) opt_tables = printing option.
! integer                 err = success or error flag.
! character(len=*)        msg = message describing success or error.
!
! OPT_TABLES = LD            means to print LD             table.
! OPT_TABLES = RPZ           means to print     RP, PP, ZT tables.
! OPT_TABLES = LRPZ          means to print LD, RP, PP, ZT tables.
! OPT_TABLES = anything else means to print only a few lines from each table.
!
! ERR = GEOMDATA_OK    if all of the data appears to be valid.
! ERR = GEOMDATA_ERROR if there is a problem with the data.
!
!-------------------------------------------------------------------------------
!                        GET MISCELLANEOUS VALUES
!                    (after calling geomdata_validate)
!            (none of these routines change the data structure)
!
!                  o                             i
!               nld     = geomdata_get_nld     (obj)
!               nlines  = geomdata_get_nlines  (obj)
!               ngroups = geomdata_get_ngroups (obj)
!               nstore  = geomdata_get_nstore  (obj)
!
!
! type(geomdata_struct)  obj = the GEOMDATA data structure.
! integer                nld = number of LD cards.
! integer             nlines = number of seismic lines on LD cards.
! integer            ngroups = number of shot profiles on PP cards.
! integer             nstore = number of words of storage required.
!
! If you were just scanning the data (without storing the arrays), NSTORE will
! still indicate the number of words of storage which would be required for
! the arrays if they had been allocated.
!
!-------------------------------------------------------------------------------
!                      GET TRACE HEADER INFORMATION
!                    (after calling geomdata_validate)
!           (none of these routines change the data structure)
!         (must not be called if you were just scanning the data)
!
! First the calling program must have access to the GEOMDATA object which
! it wishes to use to create trace headers.  Then it must declare and
! initialize a GEOMDATA_ITERATOR which is defined in the GEOMDATA_MODULE
! (note that this is a structure, not a pointer to a structure):
!
!       type(geomdata_iterator) :: iterator
!                                          i      o        i
!       call geomdata_initialize_headers (obj, iterator, trskip)
!
!
! Then the calling program must call the following subroutine for each
! trace header:
!                                   i      b      o    o    o
!       call geomdata_next_header (obj, iterator, hd, err, msg)
!
!
! type(geomdata_struct)         obj = the GEOMDATA data structure.
! type(geomdata_iterator)  iterator = the GEOMDATA iterator.
! integer                    trskip = initial number of trace headers to skip.
! double       hd(HDR_NOMINAL_SIZE) = trace header array.
! integer                       err = success or error flag.
! character(len=*)              msg = message describing success or error.
!
! ERR = GEOMDATA_OK       if the next trace header array is returned.
! ERR = GEOMDATA_FINISHED if there are no more trace headers.
! ERR = GEOMDATA_ERROR    if an error occurs.
!
! The contents of the GEOMDATA object are not modified while trace headers
! are being built and returned.  Therefore, there is no restriction on when
! or how many iterators can be operating with data in a single instance of
! GEOMDATA at the same time.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!          LIST OF ALL HEADER WORDS AND HOW THEY ARE SET BY GEOMDATA
!          [items in square brackets are the values when FIXDIST > 0]
!
! header word and type  description and value set
! --------------------  -------------------------
!    1      normal      trace sequence number.
!    2      normal      head mute index = 1.
!    3      normal      current gather number = header 9.
!    4      normal      trace number within current gather = header 10.
!    5      normal      fold of stack = 1.
!    6      normal      offset (in surveyed coordinates).
!   7,8     normal      CMP grid coordinates (from 17,18 using grid transform).
!    9      normal      original shot profile number.
!   10      normal      trace number within original shot profile.
!  11,12    normal      source surveyed easting and northing coordinates.
!   13      normal      source elevation.
!  14,15    normal      receiver surveyed easting and northing coordinates.
!   16      normal      receiver elevation.
!   17      normal      CMP surveyed easting [or center CMP inline distance].
!   18      normal      CMP surveyed northing [or 0].
!   19      normal      nearest [or center] CMP elevation.
!   20      normal      source hole depth.
!   21      normal      receiver hole depth = 0.
!  22,23    normal      source and receiver component numbers = 0.
!   24      normal      panel number = 0.
!   25      normal      LAV = 1 (live) or 0 (dead) or -1 (reverse polarity).
!   26      normal      source line number.
!   27      normal      receiver line number.
!   28      normal      receiver shotpoint.
!   29      normal      source shotpoint.
!   30      scratch     nearest CMP inline distance.
!   31      scratch     center  CMP inline distance.
!   32      scratch     1 (last trace in group) or 2 (last trace)
!                            or -999 (next trace will have errors) or 0.
!  33,34    normal      source grid coords (from 11,12 using grid transform).
!  35,36    normal      receiver grid coords (from 14,15 using grid transform).
!   37      normal      nearest [or center] CMP shotpoint.
!   38      normal      CMP line number (average of source and receiver).
!   39      normal      pre-NMO datum shift = 0.
!   40      normal      post-NMO datum shift = total datum shift.
! 41,42,43  normal      cumulative datum, refraction, residual static = 0.
!   44      normal      source uphole time.
!   45      normal      receiver uphole time = 0.
!  46,47    normal      source and receiver sequential ground positions.
!  48-55  user defined  value = 0.
!  56,57    normal      pre-NMO and post-NMO refraction shift = 0.
!  58,59    scratch     CMP surveyed easting and northing coordinates.
!  60,61    scratch     source and receiver inline distance.
!   62      scratch     trace count including skipped and missing traces.
!   63      normal      GVS modifier = 0.
!   64      normal      tail mute index = 1.
!
! Note: The following header words may have to be reset if applied to
! seismic traces:
!      mute indices 2 and 64.
!      largest absolute value 25.
!      user defined values 48-55 and header words beyond 64.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
! 12. 2007-11-29  Stoeckley  Eliminate the use of the memman primitive.
!011. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!010. 2006-01-10  B. Menger  Removed Unused Variables.
!  9. 2003-06-17  Stoeckley  Replace the ARRAY module with the MEMMAN module.
!  8. 2001-12-10  Stoeckley  Add additional printouts and error announcement
!                             capabilities; fix longstanding (but apparently
!                             rarely encountered) receiver pattern card bug
!                             associated with the YINC parameter (was being
!                             used as a step in line INDEX instead of line
!                             NUMBER; results now match CFG; the bug apparently
!                             existed in the old CPS FGD as well).
!                            Change to call the new ARRAY primitive for
!                             allocation and deallocation, and the TERPUTIL
!                             primitive for interpolation, in order to
!                             eliminate duplicate code.
!  7. 2001-03-22  Stoeckley  Remove requirement that there be at least two 
!                             LD cards for each line.
!  6. 2000-10-19  Stoeckley  Fix another bug regarding more than one line.
!  5. 2000-10-11  Stoeckley  Fix bug when LD cards contain more than one line.
!  4. 2000-10-09  Stoeckley  Fix inaccurate error message when shotpoint
!                             or line number is not found on LD cards; add
!                             required missing documentation section.
!  3. 2000-05-03  Stoeckley  Add printout options.
!  2. 2000-04-28  Stoeckley  Fix bug while replacing integer nil values.
!  1. 2000-04-04  Stoeckley  Initial version.
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


!<programming_doc>
!-------------------------------------------------------------------------------
!            DETAILS OF WHAT ROUTINES SET WHICH HEADER WORDS
!
!              i = input to the routine.
!              o = temporary or preliminary value output from the routine.
!              F = final value output from the routine.
!
!                                private                               public
!  header                         next   use   use   use   use          next
!   word                          hdr     pp    rp    ld    zt   grid   hdr
! --------                        ----   ----  ----  ----  ----  ----   ----
!    1     sequence number          F      -     -     -     -     -      -
!    2     head mute          = 1   F      -     -     -     -     -      -
!    3     group                    -      F     -     -     -     -      -
!    4     channel                  o     i-    iF     -     -     -      -
!    5     fold               = 1   F      -     -     -     -     -      -
!    6     offset                   -      -     -     F     -     -      -
!   7,8    CMP XY grid              -      -     -     -     -     F      -
!    9     group                    -      F     -     -    i-     -      -
!   10     channel                  -      -     F     -    i-     -      -
!  11,12   source XY survey         -      -     -     F     -     -      -
!   13     source elev              -      o     -    iF     -     -      -
!  14,15   rec XY survey            -      -     -     F     -     -      -
!   16     rec elev                 -      -     o    iF     -     -      -
!  17-18   CMP XY (or 2D) survey    -      -     -     F     -     -      -
!   19     CMP elev                 -      -     -     F     -     -      -
!   20     source hole depth        -      o     -    iF     -     -      -
!   21     rec hole depth     = 0   F      -     -     F     -     -      -
!  22,23   component numbers  = 0   F      -     -     -     -     -      -
!   24     panel number       = 0   F      -     -     -     -     -      -
!   25     live/dead/rp flag        -      -     -     -     F     -      -
!   26     source line              -      F     -    i     i-     -      -
!   27     rec line                 -      o    iF    i     i-     -      -
!   28     rec shotpoint            -      o    io    iF    i-     -      -
!   29     source shotpoint         -      o     -    iF    i-     -      -
!   30     nearest CMP inline       -      o     -    iF     -     -      -
!   31     center CMP inline        -      o     -    iF     -     -      -
!   32     last trace flag          -      -     o    i-     -     -      F
!   33     source X grid            -      -     o    i-     -     F      -
!   34     source Y grid            -      -     -     -     -     F      -
!  35,36   rec XY grid              -      -     -     -     -     F      -
!   37     CMP shotpoint            -      -     -     F     -     -      -
!   38     CMP line number          -      -     -     F     -     -      -
!   39     pre shift          = 0   F      -     -     F     -     -      -
!   40     post shift               -      -     -     F     -     -      -
! 41,42,43 cumulative static  = 0   F      -     -     -     -     -      -
!   44     source uphole time       -      o     -    iF     -     -      -
!   45     rec uphole time    = 0   F      -     -     F     -     -      -
!  46,47   source/rec ground pos    -      -     -     F     -     -      -
!  48-55   user defined       = 0   F      -     -     -     -     -      -
!  56,57   rpre/rpost shift   = 0   F      -     -     -     -     -      -
!  58,59   CMP XY survey            -      -     -     F     -     -      -
!  60,61   source/rec inline        -      -     -     F     -     -      -
!   62     trace count              F     i-     -     -     -     -      -
!   63     GVS modifier       = 0   F      -     -     -     -     -      -
!   64     tail mute          = 1   F      -     -     -     -     -      -
!
! Note: Reading from left to right: first notation must be o or F.
! Note: Reading from left to right: o must be followed by i.
! Note: Reading from left to right: last notation must be F.
! Note: Private_next_hdr initially sets all other headers to zero.
! Note: A few zero values are set redundantly in two places (two F flags).
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module geomdata_module
      use named_constants_module
      use grid_module
      use string_module
      use mth_module
      use terputil_module
      implicit none
      private
      public :: geomdata_create
      public :: geomdata_delete
      public :: geomdata_clear
      public :: geomdata_initialize
      public :: geomdata_set_ld_card
      public :: geomdata_set_rp_card
      public :: geomdata_set_pp_card
      public :: geomdata_set_zt1_card
      public :: geomdata_set_zt2_card
      public :: geomdata_set_zt3_card
      public :: geomdata_set_zt4_card
      public :: geomdata_validate
      public :: geomdata_get_nstore
      public :: geomdata_get_nld
      public :: geomdata_get_nlines
      public :: geomdata_get_ngroups
      public :: geomdata_initialize_headers
      public :: geomdata_next_header

      character(len=100),public,save :: GEOMDATA_IDENT = &
       '$Id: geomdata.f90,v 1.12 2007/11/30 13:55:18 Stoeckley beta sps $'

      integer,public,parameter :: GEOMDATA_OK       = 0
      integer,public,parameter :: GEOMDATA_ERROR    = 1
      integer,public,parameter :: GEOMDATA_FINISHED = 2


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: geomdata_struct              
 
        private
        logical                        :: scan
        integer                        :: err
        character(len=80)              :: msg

        real                           :: ve
        real                           :: datum
        real                           :: fixdist
        type(grid_struct)              :: grid
        character(len=4)               :: chaining

        integer :: nld,nrp,npp,nzt1,nzt2,nzt3,nzt4,nlines,ngroups
        integer :: ild,irp,ipp,izt1,izt2,izt3,izt4
        integer :: minspi
        real    :: cmpfirst,cmpinc

        integer,pointer :: linelist(:)   ! seismic line number.
        integer,pointer :: ifirst  (:)   ! first LD card number for this line.
        integer,pointer :: ilast   (:)   ! last LD card number for this line.
        integer,pointer :: iconst  (:)   ! constant to add to shotpoint index
                                         !  to get LD card number.

        real   ,pointer :: sp   (:)      ! LD card array.
        real   ,pointer :: dist (:)      ! LD card array.
        real   ,pointer :: xloc (:)      ! LD card array.
        real   ,pointer :: yloc (:)      ! LD card array.
        real   ,pointer :: elev (:)      ! LD card array.
        real   ,pointer :: depth(:)      ! LD card array.
        real   ,pointer :: tuh  (:)      ! LD card array.
        real   ,pointer :: tr   (:)      ! LD card array.
        real   ,pointer :: ts   (:)      ! LD card array.
        real   ,pointer :: xsd  (:)      ! LD card array.
        real   ,pointer :: ysd  (:)      ! LD card array.
        real   ,pointer :: elsd (:)      ! LD card array.
        integer,pointer :: line (:)      ! LD card array.
        real   ,pointer :: cum  (:)      ! LD card array.
        real   ,pointer :: sina (:)      ! LD card array.
        real   ,pointer :: cosa (:)      ! LD card array.

        integer          ,pointer :: ipat1 (:)    ! RP card array.
        character(len=16),pointer :: flag  (:)    ! RP card array.
        real             ,pointer :: sp1   (:)    ! RP card array.
        integer          ,pointer :: line1 (:)    ! RP card array.
        integer          ,pointer :: nx    (:)    ! RP card array.
        integer          ,pointer :: ixinc (:)    ! RP card array.
        integer          ,pointer :: ny    (:)    ! RP card array.
        integer          ,pointer :: iyinc (:)    ! RP card array.
        real             ,pointer :: xsd1  (:)    ! RP card array.
        real             ,pointer :: ysd1  (:)    ! RP card array.
        real             ,pointer :: elsd1 (:)    ! RP card array.

        real   ,pointer :: sp2   (:)    ! PP card array.
        integer,pointer :: line2 (:)    ! PP card array.
        real   ,pointer :: sp3   (:)    ! PP card array.
        integer,pointer :: line3 (:)    ! PP card array.
        integer,pointer :: ipat2 (:)    ! PP card array.
        real   ,pointer :: xsd2  (:)    ! PP card array.
        real   ,pointer :: ysd2  (:)    ! PP card array.
        integer,pointer :: hold  (:)    ! PP card array.
        real   ,pointer :: elev2 (:)    ! PP card array.
        real   ,pointer :: depth2(:)    ! PP card array.
        real   ,pointer :: tuh2  (:)    ! PP card array.
        integer,pointer :: is    (:)    ! PP card array.
        integer,pointer :: ir    (:)    ! PP card array.
        integer,pointer :: ig    (:)    ! PP card array.
 
        character(len=12),pointer :: ccc1   (:)    ! ZT1 card array.
        real             ,pointer :: sss1   (:)    ! ZT1 card array.
        real             ,pointer :: sss1a  (:)    ! ZT1 card array.
        integer          ,pointer :: lll1   (:)    ! ZT1 card array.
        
        character(len=12),pointer :: ccc2   (:)    ! ZT2 card array.
        real             ,pointer :: rrr2   (:)    ! ZT2 card array.
        real             ,pointer :: rrr2a  (:)    ! ZT2 card array.
        integer          ,pointer :: lll2   (:)    ! ZT2 card array.
        
        character(len=12),pointer :: ccc3   (:)    ! ZT3 card array.
        integer          ,pointer :: iggg3  (:)    ! ZT3 card array.
        integer          ,pointer :: iggg3a (:)    ! ZT3 card array.
        integer          ,pointer :: ittt3  (:)    ! ZT3 card array.
        integer          ,pointer :: ittt3a (:)    ! ZT3 card array.
        
        character(len=12),pointer :: ccc4   (:)    ! ZT4 card array.
        real             ,pointer :: sss4   (:)    ! ZT4 card array.
        real             ,pointer :: sss4a  (:)    ! ZT4 card array.
        integer          ,pointer :: lll4   (:)    ! ZT4 card array.
        real             ,pointer :: rrr4   (:)    ! ZT4 card array.
        real             ,pointer :: rrr4a  (:)    ! ZT4 card array.
        integer          ,pointer :: lll4a  (:)    ! ZT4 card array.

      end type geomdata_struct


!!------------------------------ iterator ----------------------------------!!
!!------------------------------ iterator ----------------------------------!!
!!------------------------------ iterator ----------------------------------!!


      type,public :: geomdata_iterator

        private
        integer           :: trskip,kount,sequence,err
        character(len=80) :: msg
        double precision  :: hhh4keep
        double precision  :: hhh(HDR_NOMINAL_SIZE)
        integer           :: igrp,ipp,nprev            ! needed by use_pp.
        integer           :: ipatkeep,irp,irpfirst     ! needed by use_rp.
        integer           :: irpstart,irreg,irplast    ! needed by use_rp.
        integer           :: ix,iy                     ! needed by use_rp.
        real              :: xstart,ystart,x,y         ! needed by use_rp.

      end type geomdata_iterator


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


   character(len=70),public,save :: hdr_description(HDR_NOMINAL_SIZE)

   data hdr_description(1:32) /                                     &
      '  1            trace sequence number                      ', &
      '  2            head mute index                            ', &
      '  3            current gather number                      ', &
      '  4            trace number within current gather         ', &
      '  5            fold of stack                              ', &
      '  6            offset                                     ', &
      '  7    CMP     X grid coordinate (from 17 and 18)         ', &
      '  8    CMP     Y grid coordinate (from 17 and 18)         ', &
      '  9            original shot profile number               ', &
      ' 10            trace number within original shot profile  ', &
      ' 11   source    easting survey coordinate                 ', &
      ' 12   source   northing survey coordinate                 ', &
      ' 13   source   elevation                                  ', &
      ' 14  receiver   easting survey coordinate                 ', &
      ' 15  receiver  northing survey coordinate                 ', &
      ' 16  receiver  elevation                                  ', &
      ' 17    CMP      easting survey coordinate                 ', &
      ' 18    CMP     northing survey coordinate                 ', &
      ' 19    CMP     elevation                                  ', &
      ' 20   source   hole depth                                 ', &
      ' 21  receiver  hole depth                                 ', &
      ' 22   source   component number                           ', &
      ' 23  receiver  component number                           ', &
      ' 24            panel number                               ', &
      ' 25            largest absolute value                     ', &
      ' 26   source   line number                                ', &
      ' 27  receiver  line number                                ', &
      ' 28  receiver  shotpoint                                  ', &
      ' 29   source   shotpoint                                  ', &
      ' 30            scratch                                    ', &
      ' 31            scratch                                    ', &
      ' 32            scratch                                    '/  

   data hdr_description(33:64) /                                    &
      ' 33   source   X grid coordinate (from 11 and 12)         ', &
      ' 34   source   Y grid coordinate (from 11 and 12)         ', &
      ' 35  receiver  X grid coordinate (from 14 and 15)         ', &
      ' 36  receiver  Y grid coordinate (from 14 and 15)         ', &
      ' 37    CMP     shotpoint                                  ', &
      ' 38    CMP     line number                                ', &
      ' 39   pre-NMO  datum shift        (millisecs)             ', &
      ' 40  post-NMO  datum shift        (millisecs)             ', &
      ' 41 cumulative      datum static  (millisecs)             ', &
      ' 42 cumulative refraction static  (millisecs)             ', &
      ' 43 cumulative   residual static  (millisecs)             ', &
      ' 44   source   uphole time        (millisecs)             ', &
      ' 45  receiver  uphole time        (millisecs)             ', &
      ' 46   source   sequential ground position                 ', &
      ' 47  receiver  sequential ground position                 ', &
      ' 48            user defined                               ', &
      ' 49            user defined                               ', &
      ' 50            user defined                               ', &
      ' 51            user defined                               ', &
      ' 52            user defined                               ', &
      ' 53            user defined                               ', &
      ' 54            user defined                               ', &
      ' 55            user defined                               ', &
      ' 56   pre-NMO  refraction shift   (millisecs)             ', &
      ' 57  post-NMO  refraction shift   (millisecs)             ', &
      ' 58            scratch                                    ', &
      ' 59            scratch                                    ', &
      ' 60            scratch                                    ', &
      ' 61            scratch                                    ', &
      ' 62            scratch                                    ', &
      ' 63            GVS modifier                               ', &
      ' 64            tail mute index                            '/


      contains


!!-------------------- geomdata private deallocate ---------------------------!!
!!-------------------- geomdata private deallocate ---------------------------!!
!!-------------------- geomdata private deallocate ---------------------------!!


      subroutine geomdata_private_deallocate (obj)
      implicit none
      type(geomdata_struct),intent(inout) :: obj       ! arguments

      if (associated(obj%linelist)) deallocate (obj%linelist)
      if (associated(obj%ifirst  )) deallocate (obj%ifirst)
      if (associated(obj%ilast   )) deallocate (obj%ilast)
      if (associated(obj%iconst  )) deallocate (obj%iconst)

      if (associated(obj%sp    )) deallocate (obj%sp    )    ! LD card array.
      if (associated(obj%dist  )) deallocate (obj%dist  )    ! LD card array.
      if (associated(obj%xloc  )) deallocate (obj%xloc  )    ! LD card array.
      if (associated(obj%yloc  )) deallocate (obj%yloc  )    ! LD card array.
      if (associated(obj%elev  )) deallocate (obj%elev  )    ! LD card array.
      if (associated(obj%depth )) deallocate (obj%depth )    ! LD card array.
      if (associated(obj%tuh   )) deallocate (obj%tuh   )    ! LD card array.
      if (associated(obj%tr    )) deallocate (obj%tr    )    ! LD card array.
      if (associated(obj%ts    )) deallocate (obj%ts    )    ! LD card array.
      if (associated(obj%xsd   )) deallocate (obj%xsd   )    ! LD card array.
      if (associated(obj%ysd   )) deallocate (obj%ysd   )    ! LD card array.
      if (associated(obj%elsd  )) deallocate (obj%elsd  )    ! LD card array.
      if (associated(obj%line  )) deallocate (obj%line  )    ! LD card array.
      if (associated(obj%cum   )) deallocate (obj%cum   )    ! LD card array.
      if (associated(obj%sina  )) deallocate (obj%sina  )    ! LD card array.
      if (associated(obj%cosa  )) deallocate (obj%cosa  )    ! LD card array.

      if (associated(obj%ipat1 )) deallocate (obj%ipat1 )    ! RP card array.
      if (associated(obj%flag  )) deallocate (obj%flag  )    ! RP card array.
      if (associated(obj%sp1   )) deallocate (obj%sp1   )    ! RP card array.
      if (associated(obj%line1 )) deallocate (obj%line1 )    ! RP card array.
      if (associated(obj%nx    )) deallocate (obj%nx    )    ! RP card array.
      if (associated(obj%ixinc )) deallocate (obj%ixinc )    ! RP card array.
      if (associated(obj%ny    )) deallocate (obj%ny    )    ! RP card array.
      if (associated(obj%iyinc )) deallocate (obj%iyinc )    ! RP card array.
      if (associated(obj%xsd1  )) deallocate (obj%xsd1  )    ! RP card array.
      if (associated(obj%ysd1  )) deallocate (obj%ysd1  )    ! RP card array.
      if (associated(obj%elsd1 )) deallocate (obj%elsd1 )    ! RP card array.

      if (associated(obj%sp2   )) deallocate (obj%sp2   )    ! PP card array.
      if (associated(obj%line2 )) deallocate (obj%line2 )    ! PP card array.
      if (associated(obj%sp3   )) deallocate (obj%sp3   )    ! PP card array.
      if (associated(obj%line3 )) deallocate (obj%line3 )    ! PP card array.
      if (associated(obj%ipat2 )) deallocate (obj%ipat2 )    ! PP card array.
      if (associated(obj%xsd2  )) deallocate (obj%xsd2  )    ! PP card array.
      if (associated(obj%ysd2  )) deallocate (obj%ysd2  )    ! PP card array.
      if (associated(obj%hold  )) deallocate (obj%hold  )    ! PP card array.
      if (associated(obj%elev2 )) deallocate (obj%elev2 )    ! PP card array.
      if (associated(obj%depth2)) deallocate (obj%depth2)    ! PP card array.
      if (associated(obj%tuh2  )) deallocate (obj%tuh2  )    ! PP card array.
      if (associated(obj%is    )) deallocate (obj%is    )    ! PP card array.
      if (associated(obj%ir    )) deallocate (obj%ir    )    ! PP card array.
      if (associated(obj%ig    )) deallocate (obj%ig    )    ! PP card array.

      if (associated(obj%ccc1  )) deallocate (obj%ccc1  )    ! ZT1 card array.
      if (associated(obj%sss1  )) deallocate (obj%sss1  )    ! ZT1 card array.
      if (associated(obj%sss1a )) deallocate (obj%sss1a )    ! ZT1 card array.
      if (associated(obj%lll1  )) deallocate (obj%lll1  )    ! ZT1 card array.

      if (associated(obj%ccc2  )) deallocate (obj%ccc2  )    ! ZT2 card array.
      if (associated(obj%rrr2  )) deallocate (obj%rrr2  )    ! ZT2 card array.
      if (associated(obj%rrr2a )) deallocate (obj%rrr2a )    ! ZT2 card array.
      if (associated(obj%lll2  )) deallocate (obj%lll2  )    ! ZT2 card array.

      if (associated(obj%ccc3  )) deallocate (obj%ccc3  )    ! ZT3 card array.
      if (associated(obj%iggg3 )) deallocate (obj%iggg3 )    ! ZT3 card array.
      if (associated(obj%iggg3a)) deallocate (obj%iggg3a)    ! ZT3 card array.
      if (associated(obj%ittt3 )) deallocate (obj%ittt3 )    ! ZT3 card array.
      if (associated(obj%ittt3a)) deallocate (obj%ittt3a)    ! ZT3 card array.

      if (associated(obj%ccc4  )) deallocate (obj%ccc4  )    ! ZT4 card array.
      if (associated(obj%sss4  )) deallocate (obj%sss4  )    ! ZT4 card array.
      if (associated(obj%sss4a )) deallocate (obj%sss4a )    ! ZT4 card array.
      if (associated(obj%lll4  )) deallocate (obj%lll4  )    ! ZT4 card array.
      if (associated(obj%rrr4  )) deallocate (obj%rrr4  )    ! ZT4 card array.
      if (associated(obj%rrr4a )) deallocate (obj%rrr4a )    ! ZT4 card array.
      if (associated(obj%lll4a )) deallocate (obj%lll4a )    ! ZT4 card array.

      return
      end subroutine geomdata_private_deallocate


!!----------------------- geomdata private allocate -------------------------!!
!!----------------------- geomdata private allocate -------------------------!!
!!----------------------- geomdata private allocate -------------------------!!

! this routine does not include allocation for these arrays:
!                obj%linelist(obj%nlines)
!                obj%ifirst  (obj%nlines)
!                obj%ilast   (obj%nlines)
!                obj%iconst  (obj%nlines)


      subroutine geomdata_private_allocate (obj)
      implicit none
      type(geomdata_struct),intent(inout) :: obj                ! arguments
      integer                             :: status,ierr        ! local

      if (obj%scan) return

      status = 0

      allocate (obj%sp    (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%dist  (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%xloc  (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%yloc  (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%elev  (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%depth (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%tuh   (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%tr    (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ts    (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%xsd   (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ysd   (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%elsd  (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%line  (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%cum   (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%sina  (obj%nld), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%cosa  (obj%nld), stat=ierr); if (ierr/=0) status=ierr

      allocate (obj%ipat1 (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%flag  (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%sp1   (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%line1 (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%nx    (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ixinc (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ny    (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%iyinc (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%xsd1  (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ysd1  (obj%nrp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%elsd1 (obj%nrp), stat=ierr); if (ierr/=0) status=ierr

      allocate (obj%sp2   (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%line2 (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%sp3   (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%line3 (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ipat2 (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%xsd2  (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ysd2  (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%hold  (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%elev2 (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%depth2(obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%tuh2  (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%is    (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ir    (obj%npp), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ig    (obj%npp), stat=ierr); if (ierr/=0) status=ierr
 
      allocate (obj%ccc1  (obj%nzt1), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%sss1  (obj%nzt1), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%sss1a (obj%nzt1), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%lll1  (obj%nzt1), stat=ierr); if (ierr/=0) status=ierr
        
      allocate (obj%ccc2  (obj%nzt2), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%rrr2  (obj%nzt2), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%rrr2a (obj%nzt2), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%lll2  (obj%nzt2), stat=ierr); if (ierr/=0) status=ierr
       
      allocate (obj%ccc3  (obj%nzt3), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%iggg3 (obj%nzt3), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%iggg3a(obj%nzt3), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ittt3 (obj%nzt3), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%ittt3a(obj%nzt3), stat=ierr); if (ierr/=0) status=ierr
        
      allocate (obj%ccc4  (obj%nzt4), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%sss4  (obj%nzt4), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%sss4a (obj%nzt4), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%lll4  (obj%nzt4), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%rrr4  (obj%nzt4), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%rrr4a (obj%nzt4), stat=ierr); if (ierr/=0) status=ierr
      allocate (obj%lll4a (obj%nzt4), stat=ierr); if (ierr/=0) status=ierr

      if (status /= 0) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'memory allocation error'
      end if
      return
      end subroutine geomdata_private_allocate


!!--------------------------- private clear -------------------------------!!
!!--------------------------- private clear -------------------------------!!
!!--------------------------- private clear -------------------------------!!


      subroutine geomdata_private_clear (obj)
      implicit none
      type(geomdata_struct),intent(inout) :: obj       ! arguments

      obj%scan     = .false.
      obj%err      = GEOMDATA_OK
      obj%msg      = 'field geometry information appears to be valid'

      call grid_initialize (obj%grid)

      obj%ve           = FNIL
      obj%datum        = FNIL 
      obj%fixdist      = FNIL 
      obj%chaining     = CNIL

      obj%nld          = 0
      obj%nrp          = 0
      obj%npp          = 0
      obj%nzt1         = 0
      obj%nzt2         = 0
      obj%nzt3         = 0
      obj%nzt4         = 0

      obj%nlines       = 0
      obj%ngroups      = 0

      obj%ild          = 0
      obj%irp          = 0
      obj%ipp          = 0
      obj%izt1         = 0
      obj%izt2         = 0
      obj%izt3         = 0
      obj%izt4         = 0

      obj%minspi   = INIL
      obj%cmpfirst = FNIL
      obj%cmpinc   = FNIL
      return
      end subroutine geomdata_private_clear


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine geomdata_create (obj)
      implicit none
      type(geomdata_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%linelist) ! jpa
      nullify (obj%ifirst) ! jpa
      nullify (obj%ilast) ! jpa
      nullify (obj%iconst) ! jpa
      nullify (obj%sp) ! jpa
      nullify (obj%dist) ! jpa
      nullify (obj%xloc) ! jpa
      nullify (obj%yloc) ! jpa
      nullify (obj%elev) ! jpa
      nullify (obj%depth) ! jpa
      nullify (obj%tuh) ! jpa
      nullify (obj%tr) ! jpa
      nullify (obj%ts) ! jpa
      nullify (obj%xsd) ! jpa
      nullify (obj%ysd) ! jpa
      nullify (obj%elsd) ! jpa
      nullify (obj%line) ! jpa
      nullify (obj%cum) ! jpa
      nullify (obj%sina) ! jpa
      nullify (obj%cosa) ! jpa
      nullify (obj%ipat1) ! jpa
      nullify (obj%flag) ! jpa
      nullify (obj%sp1) ! jpa
      nullify (obj%line1) ! jpa
      nullify (obj%nx) ! jpa
      nullify (obj%ixinc) ! jpa
      nullify (obj%ny) ! jpa
      nullify (obj%iyinc) ! jpa
      nullify (obj%xsd1) ! jpa
      nullify (obj%ysd1) ! jpa
      nullify (obj%elsd1) ! jpa
      nullify (obj%sp2) ! jpa
      nullify (obj%line2) ! jpa
      nullify (obj%sp3) ! jpa
      nullify (obj%line3) ! jpa
      nullify (obj%ipat2) ! jpa
      nullify (obj%xsd2) ! jpa
      nullify (obj%ysd2) ! jpa
      nullify (obj%hold) ! jpa
      nullify (obj%elev2) ! jpa
      nullify (obj%depth2) ! jpa
      nullify (obj%tuh2) ! jpa
      nullify (obj%is) ! jpa
      nullify (obj%ir) ! jpa
      nullify (obj%ig) ! jpa
      nullify (obj%ccc1) ! jpa
      nullify (obj%sss1) ! jpa
      nullify (obj%sss1a) ! jpa
      nullify (obj%lll1) ! jpa
      nullify (obj%ccc2) ! jpa
      nullify (obj%rrr2) ! jpa
      nullify (obj%rrr2a) ! jpa
      nullify (obj%lll2) ! jpa
      nullify (obj%ccc3) ! jpa
      nullify (obj%iggg3) ! jpa
      nullify (obj%iggg3a) ! jpa
      nullify (obj%ittt3) ! jpa
      nullify (obj%ittt3a) ! jpa
      nullify (obj%ccc4) ! jpa
      nullify (obj%sss4) ! jpa
      nullify (obj%sss4a) ! jpa
      nullify (obj%lll4) ! jpa
      nullify (obj%rrr4) ! jpa
      nullify (obj%rrr4a) ! jpa
      nullify (obj%lll4a) ! jpa

      call geomdata_private_clear   (obj)
      return
      end subroutine geomdata_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine geomdata_delete (obj)
      implicit none
      type(geomdata_struct),pointer :: obj       ! arguments

      call geomdata_private_deallocate (obj)

      deallocate(obj)
      return
      end subroutine geomdata_delete


!!------------------------------- clear -----------------------------------!!
!!------------------------------- clear -----------------------------------!!
!!------------------------------- clear -----------------------------------!!


      subroutine geomdata_clear (obj)
      implicit none
      type(geomdata_struct),intent(inout) :: obj       ! arguments

      call geomdata_private_deallocate (obj)
      call geomdata_private_clear      (obj)
      return
      end subroutine geomdata_clear


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine geomdata_initialize                       &
                        (obj,ve,datum,fixdist,grid,        &
                         chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4,scan)
      implicit none
      type(geomdata_struct),intent(inout) :: obj                  ! arguments
      real                 ,intent(in)    :: ve,datum,fixdist     ! arguments
      type(grid_struct)    ,intent(in)    :: grid                 ! arguments
      character(len=*)     ,intent(in)    :: chaining             ! arguments
      integer              ,intent(in)    :: nld,nrp,npp          ! arguments
      integer              ,intent(in)    :: nzt1,nzt2,nzt3,nzt4  ! arguments
      logical,optional     ,intent(in)    :: scan                 ! arguments
      type(grid_struct)                   :: nilgrid              ! local

      call geomdata_private_deallocate (obj)
      call geomdata_private_clear      (obj)

      obj%ve       = ve  
      obj%datum    = datum
      obj%fixdist  = fixdist
      obj%grid     = grid
      obj%chaining = ' '
      obj%nld      = nld
      obj%nrp      = nrp
      obj%npp      = npp
      obj%nzt1     = nzt1
      obj%nzt2     = nzt2
      obj%nzt3     = nzt3
      obj%nzt4     = nzt4

      if (chaining(1:1) == 'H' .or. chaining(1:1) == 'h') obj%chaining = 'HORI'
      if (chaining(1:1) == 'S' .or. chaining(1:1) == 's') obj%chaining = 'SLOP'
      if (chaining(1:1) == 'N' .or. chaining(1:1) == 'n') obj%chaining = 'NONE'

      call grid_initialize (nilgrid)

      if (obj%ve <= 0.0) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'reference velocity must be > 0'
      else if (obj%chaining == ' ') then
           obj%err = GEOMDATA_ERROR
           obj%msg =  &
               'illegal chaining parameter - must be HORI or SLOP or NONE'
      else if (obj%grid == nilgrid) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'grid transform apparently not set'
      else if (obj%nld == 0) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'no LD cards'
      else if (obj%nrp == 0) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'no RP cards'
      else if (obj%npp == 0) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'no PP cards'
      end if

      if (present(scan)) obj%scan = scan

      if (obj%err == GEOMDATA_ERROR) return
      call geomdata_private_allocate (obj)
      return
      end subroutine geomdata_initialize


!!----------------------------- private step -------------------------------!!
!!----------------------------- private step -------------------------------!!
!!----------------------------- private step -------------------------------!!


      subroutine geomdata_private_step (obj,ild,which,objild)
      implicit none
      type(geomdata_struct),intent(inout) :: obj             ! arguments
      integer              ,intent(in)    :: ild             ! arguments
      character(len=*)     ,intent(in)    :: which           ! arguments
      integer              ,intent(inout) :: objild          ! arguments

      if (obj%err == GEOMDATA_ERROR) return
      objild = objild + 1
      if (ild /= objild) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'calling geomdata_set_'//trim(which)// &
                                        '_card out of sequence'
      end if
      return
      end subroutine geomdata_private_step


!!------------------------------ set ld card -------------------------------!!
!!------------------------------ set ld card -------------------------------!!
!!------------------------------ set ld card -------------------------------!!


      subroutine geomdata_set_ld_card  (obj,ild,           &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      implicit none
      type(geomdata_struct),intent(inout) :: obj                    ! arguments
      integer              ,intent(in)    :: ild                    ! arguments
      real                 ,intent(in)    :: sp,dist,xloc,yloc,elev ! arguments
      real                 ,intent(in)    :: depth,tuh,tr,ts        ! arguments
      real                 ,intent(in)    :: xsd,ysd,elsd           ! arguments
      integer              ,intent(in)    :: line                   ! arguments
      integer,save                        :: linekeep               ! local

      call geomdata_private_step (obj,ild,'LD',obj%ild)
      if (obj%err == GEOMDATA_ERROR) return

      if (ild == 1) then
           obj%nlines = 1
           linekeep   = line
      else if (line /= linekeep) then
           obj%nlines = obj%nlines + 1
           linekeep   = line
      end if

      if (obj%scan) return

      obj%sp   (ild) = sp      ! can be nil.
      obj%dist (ild) = dist    ! can be nil.
      obj%xloc (ild) = xloc    ! can be nil.
      obj%yloc (ild) = yloc    ! can be nil.
      obj%elev (ild) = elev    ! can be nil.
      obj%depth(ild) = depth   ! can be nil.
      obj%tuh  (ild) = tuh     ! can be nil.
      obj%tr   (ild) = tr      ! can be nil.
      obj%ts   (ild) = ts      ! can be nil.
      obj%xsd  (ild) = xsd     ! can be nil.
      obj%ysd  (ild) = ysd     ! can be nil.
      obj%elsd (ild) = elsd    ! can be nil.
      obj%line (ild) = line
      obj%cum  (ild) = 0.0     ! to change later.
      obj%sina (ild) = 0.0     ! to change later.
      obj%cosa (ild) = 0.0     ! to change later.
      return
      end subroutine geomdata_set_ld_card


!!------------------------------ set rp card -------------------------------!!
!!------------------------------ set rp card -------------------------------!!
!!------------------------------ set rp card -------------------------------!!


      subroutine geomdata_set_rp_card  (obj,irp,           &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      implicit none
      type(geomdata_struct),intent(inout) :: obj                   ! arguments
      integer              ,intent(in)    :: irp                   ! arguments
      integer              ,intent(in)    :: ipat1,line1           ! arguments
      integer              ,intent(in)    :: nx,ixinc,ny,iyinc     ! arguments
      real                 ,intent(in)    :: sp1,xsd1,ysd1,elsd1   ! arguments
      character(len=*)     ,intent(in)    :: flag                  ! arguments
      character(len=4)                    :: local_flag            ! local

      call geomdata_private_step (obj,irp,'RP',obj%irp)
      if (obj%err == GEOMDATA_ERROR) return

      local_flag = ' '
      if (flag(1:1) == 'X' .or. flag(1:1) == 'x') local_flag = 'X'
      if (flag(1:1) == 'Y' .or. flag(1:1) == 'y') local_flag = 'Y'
      if (flag(1:1) == 'S' .or. flag(1:1) == 's') local_flag = 'SKIP'
      if (flag(1:1) == 'D' .or. flag(1:1) == 'd') local_flag = 'DUP'

      if (local_flag == ' ') then
           obj%err = GEOMDATA_ERROR
           call string_encode (obj%msg,  &
               'illegal flag '//trim(flag)//' for RP card',irp,  &
               '- should be X or Y or SKIP or DUP')
      end if

      if (obj%scan) return

      obj%ipat1(irp) = ipat1
      obj%flag (irp) = local_flag
      obj%sp1  (irp) = sp1  
      obj%line1(irp) = line1
      obj%nx   (irp) = max(nx,1)
      obj%ixinc(irp) = ixinc
      obj%ny   (irp) = max(ny,1)
      obj%iyinc(irp) = iyinc
      obj%xsd1 (irp) = xsd1 
      obj%ysd1 (irp) = ysd1 
      obj%elsd1(irp) = elsd1
      return
      end subroutine geomdata_set_rp_card


!!------------------------------ set pp card -------------------------------!!
!!------------------------------ set pp card -------------------------------!!
!!------------------------------ set pp card -------------------------------!!


      subroutine geomdata_set_pp_card  (obj,ipp,           &
           sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      implicit none
      type(geomdata_struct),intent(inout) :: obj                   ! arguments
      integer              ,intent(in)    :: ipp                   ! arguments
      real                 ,intent(in)    :: sp2,sp3,xsd2,ysd2     ! arguments
      integer              ,intent(in)    :: line2,line3,ipat2     ! arguments
      real                 ,intent(in)    :: elev2,depth2,tuh2     ! arguments
      integer              ,intent(in)    :: hold,is,ir,ig         ! arguments

      call geomdata_private_step (obj,ipp,'PP',obj%ipp)
      if (obj%err == GEOMDATA_ERROR) return

      if (ipp == obj%npp) obj%ngroups = ig

      if (obj%scan) return

      obj%sp2   (ipp) = sp2     ! can be nil.
      obj%line2 (ipp) = line2   ! can be nil.
      obj%sp3   (ipp) = sp3     ! can be nil.
      obj%line3 (ipp) = line3   ! can be nil.
      obj%ipat2 (ipp) = ipat2   ! can be nil.
      obj%xsd2  (ipp) = xsd2    ! can be nil.
      obj%ysd2  (ipp) = ysd2    ! can be nil.
      obj%hold  (ipp) = hold    ! can be nil.
      obj%elev2 (ipp) = elev2   ! can be nil.
      obj%depth2(ipp) = depth2  ! can be nil.
      obj%tuh2  (ipp) = tuh2    ! can be nil.
      obj%is    (ipp) = is      ! can be nil.
      obj%ir    (ipp) = ir      ! can be nil.
      obj%ig    (ipp) = ig

      if (hold /= INIL) obj%hold(ipp) = max( min(hold,99999),1 )
      return
      end subroutine geomdata_set_pp_card


!!-------------------------- private set code ------------------------------!!
!!-------------------------- private set code ------------------------------!!
!!-------------------------- private set code ------------------------------!!


      subroutine geomdata_private_set_code (obj,ccc,izt,which,local_ccc)
      implicit none
      type(geomdata_struct),intent(inout) :: obj             ! arguments
      character(len=*)     ,intent(in)    :: ccc             ! arguments
      integer              ,intent(in)    :: izt             ! arguments
      character(len=*)     ,intent(in)    :: which           ! arguments
      character(len=*)     ,intent(out)   :: local_ccc       ! arguments

      local_ccc = ' '

      if (ccc(1:1) == 'Z' .or. ccc(1:1) == 'z') local_ccc = 'ZERO'
      if (ccc(1:1) == 'R' .or. ccc(1:1) == 'r') local_ccc = 'REV'
      if (ccc(1:1) == 'M' .or. ccc(1:1) == 'm') local_ccc = 'MISS'

      if (local_ccc == ' ') then
           obj%err = GEOMDATA_ERROR
           call string_encode (obj%msg,  &
               'illegal code '//trim(ccc)//' for '//trim(which)//' card', &
               izt,'- should be ZERO or REV or MISS')
      end if
      return
      end subroutine geomdata_private_set_code


!!------------------------------ set zt1 card -------------------------------!!
!!------------------------------ set zt1 card -------------------------------!!
!!------------------------------ set zt1 card -------------------------------!!


      subroutine geomdata_set_zt1_card (obj,izt1,           &
                                        ccc1,sss1,sss1a,lll1)
      implicit none
      type(geomdata_struct),intent(inout) :: obj               ! arguments
      integer              ,intent(in)    :: izt1              ! arguments
      character(len=*)     ,intent(in)    :: ccc1              ! arguments
      real                 ,intent(in)    :: sss1,sss1a        ! arguments
      integer              ,intent(in)    :: lll1              ! arguments
      character(len=4)                    :: local_ccc1        ! local

      call geomdata_private_step (obj,izt1,'ZT1',obj%izt1)
      if (obj%err == GEOMDATA_ERROR) return

      call geomdata_private_set_code (obj,ccc1,izt1,'ZT1',local_ccc1)

      if (obj%scan) return

      obj%ccc1 (izt1) = local_ccc1
      obj%sss1 (izt1) = min(sss1,sss1a)
      obj%sss1a(izt1) = max(sss1,sss1a)
      obj%lll1 (izt1) = lll1
      return
      end subroutine geomdata_set_zt1_card


!!------------------------------ set zt2 card -------------------------------!!
!!------------------------------ set zt2 card -------------------------------!!
!!------------------------------ set zt2 card -------------------------------!!


      subroutine geomdata_set_zt2_card (obj,izt2,           &
                                        ccc2,rrr2,rrr2a,lll2)
      implicit none
      type(geomdata_struct),intent(inout) :: obj               ! arguments
      integer              ,intent(in)    :: izt2              ! arguments
      character(len=*)     ,intent(in)    :: ccc2              ! arguments
      real                 ,intent(in)    :: rrr2,rrr2a        ! arguments
      integer              ,intent(in)    :: lll2              ! arguments
      character(len=4)                    :: local_ccc2        ! local

      call geomdata_private_step (obj,izt2,'ZT2',obj%izt2)
      if (obj%err == GEOMDATA_ERROR) return

      call geomdata_private_set_code (obj,ccc2,izt2,'ZT2',local_ccc2)

      if (obj%scan) return

      obj%ccc2 (izt2) = local_ccc2
      obj%rrr2 (izt2) = min(rrr2,rrr2a)
      obj%rrr2a(izt2) = max(rrr2,rrr2a)
      obj%lll2 (izt2) = lll2
      return
      end subroutine geomdata_set_zt2_card


!!------------------------------ set zt3 card -------------------------------!!
!!------------------------------ set zt3 card -------------------------------!!
!!------------------------------ set zt3 card -------------------------------!!


      subroutine geomdata_set_zt3_card (obj,izt3,           &
                                        ccc3,iggg3,iggg3a,ittt3,ittt3a)
      implicit none
      type(geomdata_struct),intent(inout) :: obj               ! arguments
      integer              ,intent(in)    :: izt3              ! arguments
      character(len=*)     ,intent(in)    :: ccc3              ! arguments
      integer              ,intent(in)    :: iggg3,iggg3a      ! arguments
      integer              ,intent(in)    :: ittt3,ittt3a      ! arguments
      character(len=4)                    :: local_ccc3        ! local

      call geomdata_private_step (obj,izt3,'ZT3',obj%izt3)
      if (obj%err == GEOMDATA_ERROR) return

      call geomdata_private_set_code (obj,ccc3,izt3,'ZT3',local_ccc3)

      if (obj%scan) return

      obj%ccc3  (izt3) = local_ccc3
      obj%iggg3 (izt3) = max( min(iggg3,iggg3a),1 )
      obj%iggg3a(izt3) = max( max(iggg3,iggg3a),1 )
      obj%ittt3 (izt3) = max( min(ittt3,ittt3a),1 )
      obj%ittt3a(izt3) = max( max(ittt3,ittt3a),1 )
      return
      end subroutine geomdata_set_zt3_card


!!------------------------------ set zt4 card -------------------------------!!
!!------------------------------ set zt4 card -------------------------------!!
!!------------------------------ set zt4 card -------------------------------!!


      subroutine geomdata_set_zt4_card (obj,izt4,           &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      implicit none
      type(geomdata_struct),intent(inout) :: obj               ! arguments
      integer              ,intent(in)    :: izt4              ! arguments
      character(len=*)     ,intent(in)    :: ccc4              ! arguments
      real                 ,intent(in)    :: sss4,sss4a        ! arguments
      real                 ,intent(in)    :: rrr4,rrr4a        ! arguments
      integer              ,intent(in)    :: lll4,lll4a        ! arguments
      character(len=4)                    :: local_ccc4        ! local

      call geomdata_private_step (obj,izt4,'ZT4',obj%izt4)
      if (obj%err == GEOMDATA_ERROR) return

      call geomdata_private_set_code (obj,ccc4,izt4,'ZT4',local_ccc4)

      if (obj%scan) return

      obj%ccc4 (izt4) = local_ccc4
      obj%sss4 (izt4) = min(sss4,sss4a)
      obj%sss4a(izt4) = max(sss4,sss4a)
      obj%rrr4 (izt4) = min(rrr4,rrr4a)
      obj%rrr4a(izt4) = max(rrr4,rrr4a)
      obj%lll4 (izt4) = lll4
      obj%lll4a(izt4) = lll4a
      return
      end subroutine geomdata_set_zt4_card


!!---------------------------- validate ----------------------------------!!
!!---------------------------- validate ----------------------------------!!
!!---------------------------- validate ----------------------------------!!


      subroutine geomdata_validate  (obj,lun,opt_tables,err,msg)
      implicit none
      type(geomdata_struct),intent(inout) :: obj                ! arguments
      integer              ,intent(in)    :: lun                ! arguments
      character(len=*)     ,intent(in)    :: opt_tables         ! arguments
      integer              ,intent(out)   :: err                ! arguments
      character(len=*)     ,intent(out)   :: msg                ! arguments
      integer                             :: incr2              ! local
      integer                             :: lds,ldr,incs,incr  ! local

      if (obj%err == GEOMDATA_ERROR) then
           continue
      else if (obj%ild /= obj%nld) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'LD cards not all set'
      else if (obj%irp /= obj%nrp) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'RP cards not all set'
      else if (obj%ipp /= obj%npp) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'PP cards not all set'
      else if (obj%izt1 /= obj%nzt1) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'ZT1 cards not all set'
      else if (obj%izt2 /= obj%nzt2) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'ZT2 cards not all set'
      else if (obj%izt3 /= obj%nzt3) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'ZT3 cards not all set'
      else if (obj%izt4 /= obj%nzt4) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'ZT4 cards not all set'
      end if

      if (obj%scan) then
           err = obj%err
           msg = obj%msg
           return
      end if

      call geomdata_fixup_ld_cards (obj,lun,opt_tables)
      call geomdata_fixup_rp_cards (obj,lun,opt_tables,  incr2)
      call geomdata_fixup_pp_cards (obj,lun,opt_tables,  lds,ldr,incs,incr)
      call geomdata_fixup_zt_cards (obj,lun,opt_tables)

      if (obj%err == GEOMDATA_OK) then
           obj%cmpfirst = 0.5*(lds+ldr)
           obj%cmpinc = min(real(incs),0.5*incr2)
           if (obj%cmpfirst-obj%cmpinc >= 0.999)  &
                                obj%cmpfirst = obj%cmpfirst-obj%cmpinc
           if (lun > 0) then
               write (lun,4500) ' source ',real(lds)   ,' source ',real(incs)
               write (lun,4500) 'receiver',real(ldr)   ,'receiver',real(incr2)
               write (lun,4500) '  CMP   ',obj%cmpfirst,'  CMP   ',obj%cmpinc
4500           format ('first possible ',a8,' location',f8.2,  &
                       ' (LD card#)',6x,                       &
                       'smallest ',a8,' increment',f8.2,' (LD card increment)')
           end if
      end if

      err = obj%err
      msg = obj%msg
      return
      end subroutine geomdata_validate


!-------------------------- get miscellaneous values -----------------------!!
!-------------------------- get miscellaneous values -----------------------!!
!-------------------------- get miscellaneous values -----------------------!!


      function geomdata_get_nld (obj) result (nld)
      implicit none
      type(geomdata_struct),intent(in)    :: obj      ! arguments
      integer                             :: nld      ! result

      nld = obj%nld
      return
      end function geomdata_get_nld



      function geomdata_get_nlines (obj) result (nlines)
      implicit none
      type(geomdata_struct),intent(in)    :: obj      ! arguments
      integer                             :: nlines   ! result

      nlines = obj%nlines
      return
      end function geomdata_get_nlines



      function geomdata_get_ngroups (obj) result (ngroups)
      implicit none
      type(geomdata_struct),intent(in)    :: obj      ! arguments
      integer                             :: ngroups  ! result

      ngroups = obj%ngroups
      return
      end function geomdata_get_ngroups



      function geomdata_get_nstore (obj) result (nstore)
      implicit none
      type(geomdata_struct),intent(in)    :: obj      ! arguments
      integer                             :: nstore   ! result

      nstore  = 16*obj%nld  + 11*obj%nrp  + 14*obj%npp  +               &
                 4*obj%nzt1 +  4*obj%nzt2 +  5*obj%nzt3 + 7*obj%nzt4 +  &
                 4*obj%nlines
      return
      end function geomdata_get_nstore


             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!
             !!-------- private routines to fixup cards -------!!


!!------------------------ geomdata fixup ld cards -------------------------!!
!!------------------------ geomdata fixup ld cards -------------------------!!
!!------------------------ geomdata fixup ld cards -------------------------!!


      subroutine geomdata_fixup_ld_cards (obj,lun,opt_tables)
      implicit none
      type(geomdata_struct),intent(inout) :: obj                ! arguments.
      integer              ,intent(in)    :: lun                ! arguments.
      character(len=*)     ,intent(in)    :: opt_tables         ! arguments.
      integer       :: ierr1,ierr2,ierr3,ierr4                  ! local
      integer       :: i,j,n,isw,ka,kb,kc,la                    ! local
      real          :: cumfirst,xdif,ydif,dif,hyp,denom,temp    ! local

!----------get started.

      if (obj%err == GEOMDATA_ERROR) return

!----------allocate memory.

      if (associated (obj%linelist)) deallocate (obj%linelist)
      if (associated (obj%ifirst  )) deallocate (obj%ifirst  )
      if (associated (obj%ilast   )) deallocate (obj%ilast   )
      if (associated (obj%iconst  )) deallocate (obj%iconst  )

      allocate (obj%linelist (obj%nlines), stat=ierr1)
      allocate (obj%ifirst   (obj%nlines), stat=ierr2)
      allocate (obj%ilast    (obj%nlines), stat=ierr3)
      allocate (obj%iconst   (obj%nlines), stat=ierr4)

      if (ierr1 /= 0 .or. ierr2 /= 0 .or. ierr3 /= 0 .or. ierr4 /= 0) then
           obj%err = GEOMDATA_ERROR
           obj%msg = 'memory allocation error'
           return
      end if

!----------get the list of lines.

      obj%nlines      = 1
      obj%linelist(1) = obj%line(1)
      obj%ifirst  (1) = 1
      do i = 2,obj%nld
           if (obj%line(i) == obj%line(i-1)) cycle
           obj%ilast (obj%nlines) = i-1
           obj%nlines = obj%nlines + 1
           obj%linelist(obj%nlines) = obj%line(i)
           obj%ifirst  (obj%nlines) = i
      end do
      obj%ilast(obj%nlines) = obj%nld

!----------sort the list of lines (bubble sort).

      if (obj%nlines > 1) then
           do j = 1,obj%nlines
                isw = 0
                do i = 2,obj%nlines
                     if (obj%linelist(i) >  obj%linelist(i-1)) cycle
                     ka                = obj%linelist(i)
                     kb                = obj%ifirst  (i)
                     kc                = obj%ilast   (i)
                     obj%linelist(i)   = obj%linelist(i-1)
                     obj%ifirst  (i)   = obj%ifirst  (i-1)
                     obj%ilast   (i)   = obj%ilast   (i-1)
                     obj%linelist(i-1) = ka
                     obj%ifirst  (i-1) = kb
                     obj%ilast   (i-1) = kc
                     isw = 1
                end do
                if (isw == 0) exit
           end do
      end if

!----------cycle through all the lines.

      cumfirst = 0.0
      do j = 1,obj%nlines
        ka = obj%ifirst(j)
        kb = obj%ilast (j)
        n  = kb - ka + 1

!----------replace nils by interpolated values.

        call terputil_replace_nils (obj%sp   (ka:kb), n, TERPUTIL_FLAT)
        call terputil_replace_nils (obj%yloc (ka:kb), n, TERPUTIL_SLOPING)
        call terputil_replace_nils (obj%elev (ka:kb), n, TERPUTIL_FLAT)
        call terputil_replace_nils (obj%depth(ka:kb), n, TERPUTIL_FLAT)
        call terputil_replace_nils (obj%tuh  (ka:kb), n, TERPUTIL_FLAT)
        call terputil_replace_nils (obj%tr   (ka:kb), n, TERPUTIL_FLAT)
        call terputil_replace_nils (obj%ts   (ka:kb), n, TERPUTIL_FLAT)
        call terputil_replace_nils (obj%xsd  (ka:kb), n, TERPUTIL_ZERO)
        call terputil_replace_nils (obj%ysd  (ka:kb), n, TERPUTIL_ZERO)
        call terputil_replace_nils (obj%elsd (ka:kb), n, TERPUTIL_ZERO)

!----------get horizontal dist values from xloc,yloc.

        if (obj%chaining == "NONE") then
           call terputil_replace_nils (obj%xloc(ka:kb), n, TERPUTIL_SLOPING)
           obj%dist(ka) = obj%xloc(ka)
           do i = ka+1,kb
                obj%dist(i) = sqrt ((obj%xloc(i) - obj%xloc(i-1))**2 +  &
                                    (obj%yloc(i) - obj%yloc(i-1))**2)
           end do

!----------get xloc values from dist,yloc,elev.

        else
           call terputil_replace_nils (obj%dist(ka:kb), n, TERPUTIL_DOWN)
           obj%xloc(ka) = obj%dist(ka)
           do i = ka+1,kb
                xdif = obj%dist(i)**2 - (obj%yloc(i) - obj%yloc(i-1))**2
                if (obj%chaining == "SLOP")  &
                            xdif = xdif - (obj%elev(i) - obj%elev(i-1))**2
                if (xdif < 0.0) then
                     obj%err = GEOMDATA_ERROR
                     obj%msg = 'imaginary X-distance between flags'
                     return
                end if
                obj%xloc(i) = obj%xloc(i-1) + sqrt(xdif)
           end do
        end if

!----------get inline distance (cumulative horiz distance from start of line).
!----------also get local azimuth of line.

        if (obj%fixdist == 0.0) then
             obj%cum(ka) = 0.0
        else
             if (obj%sp(1) == obj%sp(2)) then
                  obj%err = GEOMDATA_ERROR
                  call string_encode (obj%msg,  &
                            'two shotpoints',obj%sp(1),'are the same')
                  return
             end if
             obj%cum(ka) = (obj%sp(ka)-obj%sp(1))/(obj%sp(2)-obj%sp(1))  &
                                * abs(obj%fixdist)
             cumfirst = min(obj%cum(ka),cumfirst)
        end if
        do i = ka+1,kb
          xdif = obj%xloc(i)-obj%xloc(i-1)
          ydif = obj%yloc(i)-obj%yloc(i-1)
          hyp = sqrt(xdif**2+ydif**2)
          if (hyp <= 0.0) then
               obj%err = GEOMDATA_ERROR
               obj%msg = trim(string_ff2ss(obj%xloc(i),ndec=0))//','// &
                         trim(string_ff2ss(obj%yloc(i),ndec=0))
               call string_encode (obj%msg,  &
                      'two flags (card',i,'coords '//trim(obj%msg)// &
                      ') are coincident on line',obj%linelist(j))
               return
          end if
          obj%cum(i) = obj%cum(i-1)+hyp
          obj%sina(i) = ydif/hyp
          obj%cosa(i) = xdif/hyp
        end do
        obj%sina(ka) = obj%sina(ka+1)
        obj%cosa(ka) = obj%cosa(ka+1)
      end do

!----------reset the inline distances if necessary.

      if (cumfirst /= 0.0) then
           do j = 1,obj%nlines
                ka = obj%ifirst(j)
                kb = obj%ilast(j)
                do i = ka,kb
                     obj%cum(i) = obj%cum(i)-cumfirst
                end do
           end do
      end if

!----------get minimum shotpoint index.
!----------get the constant to add to the shotpoint index.
!             (the same shotpoint on two lines will have the same sp index.)

      obj%iconst(1) = 0        ! constant to add to sp index to get ld card#.
      obj%minspi = 1                        ! minimum shotpoint index.
      if (obj%nlines >= 2) then
           do j = 2,obj%nlines
                ka = obj%ifirst(j)
                kb = obj%ilast (j)
                la = obj%ifirst(j-1)
                if (ka < kb) then
                     denom = obj%sp(ka+1)-obj%sp(ka)   ! shotpoint increment.
                else
                     denom = 1.0                       ! shotpoint increment.
                end if
                if (denom == 0.0) then
                     obj%err = GEOMDATA_ERROR
                     call string_encode (obj%msg,  &
                        'two identical shotpoints on line',obj%linelist(j))
                     return
                end if
                dif = nint((obj%sp(ka)-obj%sp(la))/denom)
                obj%iconst(j) = obj%iconst(j-1)+ka-la-dif
                obj%minspi = min(ka-obj%iconst(j),obj%minspi)
           end do
      end if

!----------print ld cards.

      call geomdata_print_line_summary (obj,lun)
      call geomdata_print_ld_cards     (obj,lun,opt_tables)

!----------reset inline distance to fixed increments if required.

      if (obj%fixdist /= 0.0) then
           do j = 1,obj%nlines
             ka = obj%ifirst(j)
             kb = obj%ilast(j)
             temp = obj%cum(ka)
             do i = ka,kb
               obj%cum(i) = temp+(i-ka)*abs(obj%fixdist)
             end do
           end do
      end if
      return
      end subroutine geomdata_fixup_ld_cards


!!-------------------------- geomdata index ------------------------------!!
!!-------------------------- geomdata index ------------------------------!!
!!-------------------------- geomdata index ------------------------------!!

! replace shotpoint by shotpoint index.


      subroutine geomdata_index (obj,spwant,lwant)
      implicit none
      type(geomdata_struct),intent(inout) :: obj            ! arguments.
      real                 ,intent(inout) :: spwant         ! arguments.
      integer              ,intent(in)    :: lwant          ! arguments.
      integer                             :: j,i            ! local

      if (obj%err == GEOMDATA_ERROR) return
      do j = 1,obj%nlines
           if (lwant == obj%linelist(j)) then
                do i = obj%ifirst(j),obj%ilast(j)
                     if (abs(spwant-obj%sp(i)) < 0.0001) then
                          spwant = i - obj%iconst(j)
                          return
                     end if
                end do
                obj%err = GEOMDATA_ERROR
                call string_encode (obj%msg,  &
                  'shotpoint',spwant,'on line',real(lwant),  &
                  'not found on LD cards')
                return
           end if
      end do
      obj%err = GEOMDATA_ERROR
      call string_encode (obj%msg,'line number',lwant,'not found on LD cards')
      return
      end subroutine geomdata_index


!!------------------------ geomdata fixup rp cards -------------------------!!
!!------------------------ geomdata fixup rp cards -------------------------!!
!!------------------------ geomdata fixup rp cards -------------------------!!


      subroutine geomdata_fixup_rp_cards (obj,lun,opt_tables,incr2)
      implicit none
      type(geomdata_struct),intent(inout) :: obj            ! arguments.
      integer              ,intent(in)    :: lun            ! arguments.
      character(len=*)     ,intent(in)    :: opt_tables     ! arguments.
      integer              ,intent(out)   :: incr2          ! arguments.
      integer                             :: i              ! local

!----------get started.

      if (obj%err == GEOMDATA_ERROR) return

!----------print rp cards.

      call geomdata_print_rp_cards (obj,lun,opt_tables)

!----------replace shotpoints with indices.

      do i = 1,obj%nrp
           call geomdata_index (obj,obj%sp1(i),obj%line1(i))
      end do

!----------get minimum receiver ld increment.

      incr2 = 99999
      do i = 1,obj%nrp
           if (obj%flag(i) /= 'X'.and.obj%flag(i) /= 'Y') cycle
           if (obj%ixinc(i) /= 0) incr2 = min(incr2,abs(obj%ixinc(i)))
      end do
      return
      end subroutine geomdata_fixup_rp_cards


!!------------------------ geomdata fixup pp cards -------------------------!!
!!------------------------ geomdata fixup pp cards -------------------------!!
!!------------------------ geomdata fixup pp cards -------------------------!!


      subroutine geomdata_fixup_pp_cards (obj,lun,opt_tables,lds,ldr,incs,incr)
      implicit none
      type(geomdata_struct),intent(inout) :: obj                ! arguments.
      integer              ,intent(in)    :: lun                ! arguments.
      character(len=*)     ,intent(in)    :: opt_tables         ! arguments.
      integer              ,intent(out)   :: lds,ldr,incs,incr  ! arguments.
      integer                             :: i,lds2,ldr2        ! local
      integer                             :: istemp,irtemp      ! local

!----------get started.

      if (obj%err == GEOMDATA_ERROR) return

!----------replace nils by interpolated values.

      call terputil_replace_nils (obj%sp2  , obj%npp, TERPUTIL_DOWN)
      call terputil_replace_nils (obj%line2, obj%npp, TERPUTIL_DOWN)
      call terputil_replace_nils (obj%sp3  , obj%npp, TERPUTIL_DOWN)
      call terputil_replace_nils (obj%line3, obj%npp, TERPUTIL_DOWN)
      call terputil_replace_nils (obj%ipat2, obj%npp, TERPUTIL_DOWN)
      call terputil_replace_nils (obj%xsd2 , obj%npp, TERPUTIL_ZERO)
      call terputil_replace_nils (obj%ysd2 , obj%npp, TERPUTIL_ZERO)
      call terputil_replace_nils (obj%hold , obj%npp, TERPUTIL_ONE )
      call terputil_replace_nils (obj%is   , obj%npp, TERPUTIL_DOWN)
      call terputil_replace_nils (obj%ir   , obj%npp, TERPUTIL_DOWN)
      call terputil_replace_nils (obj%ig   , obj%npp, TERPUTIL_DOWN)

!----------check for invalid values.

      do i = 1,obj%npp
           if (i == 1) then
                if (obj%ig(i) <= 0) then
                     obj%err = GEOMDATA_ERROR
                     obj%msg = 'PP card error in GRP# field'
                     return
                end if
           else
                if (obj%ig(i) <= obj%ig(i-1)) then
                     obj%err = GEOMDATA_ERROR
                     obj%msg = 'PP card error in GRP# field'
                     return
                end if
           end if
      end do

!----------print pp cards.

      call geomdata_print_pp_cards (obj,lun,opt_tables)

!----------replace shotpoints with indices.

      do i = 1,obj%npp
           call geomdata_index (obj,obj%sp2(i),obj%line2(i))
           call geomdata_index (obj,obj%sp3(i),obj%line3(i))
      end do

!----------get minimum source and receiver ld increments.

      incs = 99999
      incr = 99999
      do i = 1,obj%npp
           if (obj%is(i) /= 0) incs = min(incs,abs(obj%is(i)))
           if (obj%ir(i) /= 0) incr = min(incr,abs(obj%ir(i)))
      end do
      do i = 2,obj%npp
           istemp = nint(abs(obj%sp2(i)-obj%sp2(i-1)))
           irtemp = nint(abs(obj%sp3(i)-obj%sp3(i-1)))
           if (istemp /= 0) incs = min(incs,istemp)
           if (irtemp /= 0) incr = min(incr,irtemp)
      end do

!----------get minimum possible source and receiver ld card numbers.

      lds = nint(obj%sp2(1))
      ldr = nint(obj%sp3(1))
      lds2 = (lds-1)/incs
      ldr2 = (ldr-1)/incr
      lds = lds - lds2 * incs
      ldr = ldr - ldr2 * incr
      return
      end subroutine geomdata_fixup_pp_cards


!!------------------------ geomdata fixup zt cards -------------------------!!
!!------------------------ geomdata fixup zt cards -------------------------!!
!!------------------------ geomdata fixup zt cards -------------------------!!


      subroutine geomdata_fixup_zt_cards (obj,lun,opt_tables)
      implicit none
      type(geomdata_struct),intent(inout) :: obj            ! arguments.
      integer              ,intent(in)    :: lun            ! arguments.
      character(len=*)     ,intent(in)    :: opt_tables     ! arguments.


!----------get started.

      if (obj%err == GEOMDATA_ERROR) return

!----------print zt cards.

      call geomdata_print_zt_cards (obj,lun,opt_tables)

      return
      end subroutine geomdata_fixup_zt_cards


           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!
           !!-------- public routines to get trace headers -------!!


!!-------------------------- initialize headrs ---------------------------!!
!!-------------------------- initialize headrs ---------------------------!!
!!-------------------------- initialize headrs ---------------------------!!


      subroutine geomdata_initialize_headers (obj, iterator, trskip)
      implicit none
      type(geomdata_struct)  ,intent(in)  :: obj            ! arguments
      type(geomdata_iterator),intent(out) :: iterator       ! arguments
      integer                ,intent(in)  :: trskip         ! arguments

      iterator%trskip   = trskip
      iterator%kount    = 0
      iterator%sequence = 0
      iterator%hhh4keep = 0.0
      iterator%hhh(:)   = 0.0      ! output by geomdata_private_next_header.
      iterator%err      = 0        ! output by geomdata_private_next_header.
      iterator%msg      = ' '      ! output by geomdata_private_next_header.

      iterator%igrp     = 0        ! needed by geomdata_use_pp_cards.
      iterator%ipp      = 1        ! needed by geomdata_use_pp_cards.
      iterator%nprev    = -1       ! needed by geomdata_use_pp_cards.

      iterator%ipatkeep = -99999   ! needed by geomdata_use_rp_cards.
      iterator%irp      = 0        ! needed by geomdata_use_rp_cards.
      iterator%irpfirst = 0        ! needed by geomdata_use_rp_cards.
      iterator%irpstart = 0        ! needed by geomdata_use_rp_cards.
      iterator%irreg    = 0        ! needed by geomdata_use_rp_cards.
      iterator%irplast  = 0        ! needed by geomdata_use_rp_cards.
      iterator%ix       = 0        ! needed by geomdata_use_rp_cards.
      iterator%iy       = 0        ! needed by geomdata_use_rp_cards.
      iterator%xstart   = 0        ! needed by geomdata_use_rp_cards.
      iterator%ystart   = 0        ! needed by geomdata_use_rp_cards.
      iterator%x        = 0        ! needed by geomdata_use_rp_cards.
      iterator%y        = 0        ! needed by geomdata_use_rp_cards.

      call geomdata_private_next_header (obj,iterator%trskip,        & ! in
          iterator%kount,iterator%sequence,iterator%hhh4keep,        & ! inout
          iterator%igrp,iterator%ipp,iterator%nprev,                 & ! inout
          iterator%ipatkeep,iterator%irp,                            & ! inout
          iterator%irpfirst,iterator%irpstart,iterator%irreg,        & ! inout
          iterator%irplast,iterator%ix,iterator%iy,iterator%xstart,  & ! inout
          iterator%ystart,iterator%x,iterator%y,                     & ! inout
          iterator%hhh,iterator%err,iterator%msg)                      ! out
      return
      end subroutine geomdata_initialize_headers


!!------------------------------ next header ----------------------------!!
!!------------------------------ next header ----------------------------!!
!!------------------------------ next header ----------------------------!!


      subroutine geomdata_next_header (obj, iterator, hd, err, msg)
      implicit none
      type(geomdata_struct)  ,intent(in)    :: obj                ! arguments
      type(geomdata_iterator),intent(inout) :: iterator           ! arguments
      double precision       ,intent(out)   :: hd(:)              ! arguments
      integer                ,intent(out)   :: err                ! arguments
      character(len=*)       ,intent(out)   :: msg                ! arguments

!----------prepare last header to return.

      hd(1:HDR_NOMINAL_SIZE) = iterator%hhh(1:HDR_NOMINAL_SIZE)
      err                    = iterator%err
      msg                    = iterator%msg

      if (err /= GEOMDATA_OK) return

!----------calculate next header.

      call geomdata_private_next_header (obj,iterator%trskip,        & ! in
          iterator%kount,iterator%sequence,iterator%hhh4keep,        & ! inout
          iterator%igrp,iterator%ipp,iterator%nprev,                 & ! inout
          iterator%ipatkeep,iterator%irp,                            & ! inout
          iterator%irpfirst,iterator%irpstart,iterator%irreg,        & ! inout
          iterator%irplast,iterator%ix,iterator%iy,iterator%xstart,  & ! inout
          iterator%ystart,iterator%x,iterator%y,                     & ! inout
          iterator%hhh,iterator%err,iterator%msg)                      ! out

!----------maybe reset last trace flag of previous header before returning it.

      if (iterator%err == GEOMDATA_ERROR) then
           hd(32) = -999.0       ! flag indicating next trace will have error.
      else if (iterator%err == GEOMDATA_FINISHED) then
           hd(32) = 2.0                       ! flag for last trace.
      else if (iterator%hhh(4) == 1.0) then
           hd(32) = 1.0                       ! flag for last trace in gather.
      end if
      return
      end subroutine geomdata_next_header


           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!
           !!-------- private routines to get trace headers -------!!


!!--------------------- private next header -----------------------------!!
!!--------------------- private next header -----------------------------!!
!!--------------------- private next header -----------------------------!!

! this routine is called to get the next header.
! the next header is iterator%hhh.
! this routine sets the last trace flag (header 32) to zero.
! this routine is never called after it returns err = GEOMDATA_ERROR.
! this routine is never called after it returns err = GEOMDATA_FINISHED.
! this routine receives all iterator arguments individually for readability.


      subroutine geomdata_private_next_header (obj,trskip,     &  ! in
                                  kount,sequence,hhh4keep,     &  ! inout
                                  igrp,ipp,nprev,              &  ! inout
                                  ipatkeep,irp,                &  ! inout
                                  irpfirst,irpstart,irreg,     &  ! inout
                                  irplast,ix,iy,xstart,        &  ! inout
                                  ystart,x,y,                  &  ! inout
                                  hhh,err,msg)                    ! out
      implicit none
      type(geomdata_struct),intent(in)    :: obj                    ! arguments
      integer              ,intent(in)    :: trskip                 ! arguments
      double precision     ,intent(inout) :: hhh4keep               ! arguments
      integer              ,intent(inout) :: kount,sequence         ! arguments
      integer              ,intent(inout) :: igrp,ipp,nprev         ! arguments
      integer              ,intent(inout) :: ipatkeep,irp,irpfirst  ! arguments
      integer              ,intent(inout) :: irpstart,irreg,irplast ! arguments
      integer              ,intent(inout) :: ix,iy                  ! arguments
      real                 ,intent(inout) :: xstart,ystart,x,y      ! arguments
      double precision     ,intent(out)   :: hhh(:)                 ! arguments
      integer              ,intent(out)   :: err                    ! arguments
      character(len=*)     ,intent(out)   :: msg                    ! arguments
      integer                             :: ipat                   ! local

!----------initialize next header.

111   hhh(:)  = 0.0
      kount   = kount+1
      hhh(62) = kount     ! trace count including skipped and missing traces.
      hhh(4)  = hhh4keep
      hhh(2)  = 1.0
      hhh(5)  = 1.0
      hhh(64) = 1.0

!----------get header information for this shot profile.

444   call geomdata_use_pp_cards (obj,hhh,igrp,ipp,nprev,ipat,err,msg)
      if (err == GEOMDATA_ERROR)    return
      if (err == GEOMDATA_FINISHED) return

!----------get header information for next trace in this shot profile.

      call geomdata_use_rp_cards (obj,hhh,ipatkeep,irp,     &
                                  irpfirst,irpstart,irreg,  &
                                  irplast,ix,iy,xstart,     &
                                  ystart,x,y,ipat,err,msg)
      if (err == GEOMDATA_ERROR) return

      if (hhh(4) == 0.0) go to 444    ! no more channels in this shot profile.
      hhh4keep = hhh(4)
      if (kount <= trskip) go to 111  ! we are skipping this trace.

!----------get header information from ld and zt cards.

      call geomdata_use_ld_cards (obj,hhh,err,msg)
      if (err == GEOMDATA_ERROR) return

      call geomdata_use_zt_cards (obj,hhh)
      if (hhh(25) == -999.0) go to 111    ! the trace does not actually exist.

!----------calculate grid coordinates.

      call grid_get_grid_coords (obj%grid, hhh(17),hhh(18), hhh( 7),hhh( 8))
      call grid_get_grid_coords (obj%grid, hhh(11),hhh(12), hhh(33),hhh(34))
      call grid_get_grid_coords (obj%grid, hhh(14),hhh(15), hhh(35),hhh(36))

!----------set header word 1 based on actual output sequence.
!----------(excluding missing traces and traces skipped by TRSKIP)

      sequence = sequence + 1
      hhh(1) = sequence

!----------finish up and return.

      err = GEOMDATA_OK
      msg = 'next header returned'
      return
      end subroutine geomdata_private_next_header


!!-------------------------- geomdata use pp cards ------------------------!!
!!-------------------------- geomdata use pp cards ------------------------!!
!!-------------------------- geomdata use pp cards ------------------------!!

!----------headers we have to use.
!             header 62  = trace count including skipped and missing traces.
!             header 4   = channel number.

!----------headers we calculate.
!     final   header 3   = group number.
!     final   header 9   = group number.
!             header 13  = new source elevation (or nil).
!             header 20  = new source depth (or nil).
!     final   header 26  = source line number.
!   temporary header 27  = receiver line number.
!   temporary header 28  = receiver shotpoint INDEX.
!   temporary header 29  = source shotpoint INDEX.
!     scratch header 30  = source inline skid.
!     scratch header 31  = source crossline skid.
!             header 44  = new source uphole time (or nil).

!     if header 4 = 0, we start a new shot profile.
!     if header 4 > 0, we stay on the same shot profile.
!     sets err to GEOMDATA_OK if the header array is updated.
!     sets err to GEOMDATA_ERROR if an error is encountered.
!     sets err to GEOMDATA_FINISHED if there are no more shot profiles.


      subroutine geomdata_use_pp_cards (obj,hhh,igrp,ipp,nprev,ipat,err,msg)
      implicit none
      type(geomdata_struct)  ,intent(in)    :: obj                ! arguments
      double precision       ,intent(inout) :: hhh(:)             ! arguments
      integer                ,intent(inout) :: igrp,ipp,nprev     ! arguments
      integer                ,intent(out)   :: ipat               ! arguments
      integer                ,intent(out)   :: err                ! arguments
      character(len=*)       ,intent(out)   :: msg                ! arguments

!----------get started.

      if (hhh( 4) >  0.0) go to 33
      if (hhh(62) == 1.0) then
           igrp  = 0
           ipp   = 1
           nprev = -1
      end if

!----------increment the shot profile number.

      igrp = igrp + 1
      if (igrp > obj%ig(ipp)) then
           ipp = ipp + 1
           if (ipp > obj%npp) then
                 err = GEOMDATA_FINISHED
                 msg = 'finished generating trace headers'
                 return
           end if
           nprev = 0
      else
           nprev = nprev + 1    ! number of previous groups on this PP card
      end if

!----------set headers for this shot profile.

33    ipat = obj%ipat2(ipp)
      hhh( 3) = igrp
      hhh( 9) = igrp
      hhh(26) = obj%line2(ipp)
      hhh(27) = obj%line3(ipp)
      hhh(28) = obj%sp3(ipp)+nprev*obj%ir(ipp)
      hhh(29) = obj%sp2(ipp)+nprev*obj%is(ipp)
      if (obj%hold(ipp) > nprev .or. obj%hold(ipp) >= 999) then
           hhh(30) = obj%xsd2(ipp)
           hhh(31) = obj%ysd2(ipp)
      else
           hhh(30) = 0.0
           hhh(31) = 0.0
      end if
      if (nprev == 0) then
           hhh(13) = obj%elev2 (ipp)       !  might be nil (which is OK)
           hhh(20) = obj%depth2(ipp)       !  might be nil (which is OK)
           hhh(44) = obj%tuh2  (ipp)       !  might be nil (which is OK)
      else
           hhh(13) = FNIL
           hhh(20) = FNIL
           hhh(44) = FNIL
      end if
      err = GEOMDATA_OK
      msg = 'OK'
      return
      end subroutine geomdata_use_pp_cards


!!---------------------- geomdata use rp cards ----------------------------!!
!!---------------------- geomdata use rp cards ----------------------------!!
!!---------------------- geomdata use rp cards ----------------------------!!

!     sets hhh(4)=0 if there are no more traces in this shot profile.
!     sets err to GEOMDATA_OK if the header array is updated.
!     sets err to GEOMDATA_ERROR if an error is encountered.

!----------headers we have to use.
!             header 4   = channel number.                      (changed)
!   temporary header 27  = receiver line number.                (changed)
!     scratch header 28  = receiver shotpoint INDEX.            (changed)

!----------headers we calculate.
!     final   header 4   = channel number (incremented).        (changed)
!     final   header 10  = channel number.
!             header 16  = receiver elevation skid.
!     final   header 27  = receiver line number.                (changed)
!     scratch header 28  = receiver shotpoint INDEX.            (changed)
!     scratch header 32  = receiver inline skid.
!   temporary header 33  = receiver crossline skid.


      subroutine geomdata_use_rp_cards &
                     (obj,hhh,ipatkeep,irp,irpfirst,irpstart,irreg, &
                      irplast,ix,iy,xstart,ystart,x,y,ipat,err,msg)
      implicit none
      type(geomdata_struct),intent(in)    :: obj                    ! arguments
      double precision     ,intent(inout) :: hhh(:)                 ! arguments
      integer              ,intent(inout) :: ipatkeep,irp,irpfirst  ! arguments
      integer              ,intent(inout) :: irpstart,irreg,irplast ! arguments
      integer              ,intent(inout) :: ix,iy                  ! arguments
      real                 ,intent(inout) :: xstart,ystart,x,y      ! arguments
      integer              ,intent(in)    :: ipat                   ! arguments
      integer              ,intent(out)   :: err                    ! arguments
      character(len=*)     ,intent(out)   :: msg                    ! arguments
      integer                             :: i                      ! local

!----------get started.

!----------find first and last pattern card for new pattern.
!----------also set irreg=1 if flag=skip or flag=dup found.
!----------also get first card that is not a flag=skip or flag=dup card.

      if (ipat /= ipatkeep) then
           do i = 1,obj%nrp
                if (obj%ipat1(i) == ipat) go to 444
           end do
           err = GEOMDATA_ERROR
           call string_encode(msg, &
                   'receiver pattern number',ipat,'not found on RP cards')
           return
444        irpfirst = i
           irpstart = 0
           irreg = 0
           do i = irpfirst,obj%nrp
             if (obj%ipat1(i) /= ipat) go to 555
             if (obj%flag(i) == "SKIP" .or. obj%flag(i) == "DUP") then
                  irreg = 1
             else if (irpstart == 0) then
                  irpstart = i
             end if
           end do
           i = obj%nrp + 1
555        irplast = i-1
           if (irpstart == 0) then
                err = GEOMDATA_ERROR
                call string_encode(msg, &
                  'receiver pattern number',ipat,'has only SKIP and DUP cards')
                return
           end if
           ipatkeep = ipat
      end if

!----------start new shot profile.

      if (hhh(4) == 0.0) then
           irp = irpstart-1
           go to 777
      end if

!----------go to next channel when x changes fastest.

      if (obj%flag(irp) == "X") then
           ix = ix+1
           if (ix > obj%nx(irp)) then
                iy = iy+1
                if (iy > obj%ny(irp)) go to 777
                ix = 1
           end if

!----------go to next channel when y changes fastest.

      else  !  if (obj%flag(irp) == "Y") then
           iy = iy+1
           if (iy > obj%ny(irp)) then
                ix = ix+1
                if (ix > obj%nx(irp)) go to 777
                iy = 1
           end if
      end if
      go to 666

!----------go to next rp card.

777   irp = irp+1
      if (irp > irplast) then
           hhh(4) = 0.0           ! no more traces for this shot profile.
           err = GEOMDATA_OK
           msg = 'OK'
           return
      end if
      if (obj%flag(irp) == "SKIP" .or. obj%flag(irp) == "DUP") go to 777
      xstart = hhh(28) + obj%sp1  (irp) - obj%sp1  (irpstart)   ! sp INDEX.
      ystart = hhh(27) + obj%line1(irp) - obj%line1(irpstart)   ! line number.
      ix = 1
      iy = 1

!----------set the shotpoint indices and line numbers.

666   x = xstart + (ix-1)*obj%ixinc(irp)     ! shotpoint INDEX.
      y = ystart + (iy-1)*obj%iyinc(irp)     ! line number.

!----------adjust for irregularities.

!!!   skips and dups are always in the direction of changing shotpoints, and
!!!     occur only if there is a non-zero x increment on this pattern card.

      if (irreg == 1 .and. obj%ixinc(irp) /= 0) then
           do i = irpfirst,irplast
                if (obj%flag(i) /= "SKIP" .and. obj%flag(i) /= "DUP") cycle
                if (obj%line1(i) /= nint(y))                          cycle
                if (nint(obj%sp1(i)) < nint(min(real(hhh(28)),x)))    cycle
                if (nint(obj%sp1(i)) > nint(max(real(hhh(28)),x)))    cycle
                if (obj%flag(i) == "SKIP") then
                     if (nint(x) > nint(obj%sp1(i))) then
                          x = x + abs(obj%ixinc(irp))
                     else if (nint(x) < nint(obj%sp1(i))) then
                          x = x - abs(obj%ixinc(irp))
                     else
                          x = x + obj%ixinc(irp)
                     end if
                else  !  if (obj%flag(I) == "DUP") then
                     if (nint(x) > nint(obj%sp1(i))) then
                          x = x - abs(obj%ixinc(irp))
                     else if (nint(x) < nint(obj%sp1(i))) then
                          x = x + abs(obj%ixinc(irp))
                     end if
                end if
           end do
      end if

!----------get headers for this trace.

      hhh( 4) = hhh(4) + 1.0    !  receiver channel number
      hhh(10) = hhh(4)          !  receiver channel number
      hhh(28) = x               !  receiver shotpoint INDEX
      hhh(27) = y               !  receiver line number
      hhh(16) = obj%elsd1(irp)  !  receiver elevation skid
      hhh(32) = obj%xsd1 (irp)  !  receiver inline skid
      hhh(33) = obj%ysd1 (irp)  !  receiver crossline skid
      err = GEOMDATA_OK
      msg = 'OK'
      return
      end subroutine geomdata_use_rp_cards


!!----------------------- geomdata use ld cards -----------------------------!!
!!----------------------- geomdata use ld cards -----------------------------!!
!!----------------------- geomdata use ld cards -----------------------------!!


!----------headers we have to use.
!   preliminary header 13  = new source elevation (or nil).      (changed)
!   temporary   header 16  = receiver elevation skid.            (changed)
!   preliminary header 20  = new source depth (or nil).          (changed)
!   final       header 26  = source line number.
!   final       header 27  = receiver line number.
!   preliminary header 28  = receiver shotpoint INDEX.           (changed)
!   preliminary header 29  = source shotpoint INDEX.             (changed)
!   temporary   header 30  = source inline skid.                 (changed)
!   temporary   header 31  = source crossline skid.              (changed)
!   temporary   header 32  = receiver inline skid.
!   temporary   header 33  = receiver crossline skid.
!   preliminary header 44  = new source uphole time (or nil).    (changed)

!----------headers we calculate.
!     final header  6  = offset.
!     final header 11  = source x location.
!     final header 12  = source y location.
!     final header 13  = source elevation (if nil).              (changed)
!     final header 14  = receiver x location.
!     final header 15  = receiver y location.
!     final header 16  = receiver elevation.                     (changed)
!     final header 17  = cmp x location [or center cmp inline distance].
!     final header 18  = cmp y location [or center cmp line# or zero].
!     final header 19  = nearest [or center] cmp elevation.
!     final header 20  = source hole depth (if nil).             (changed)
!     final header 21  = receiver hole depth = 0.
!     final header 28  = receiver shotpoint.                     (changed)
!     final header 29  = source shotpoint.                       (changed)
!     final header 30  = nearest cmp inline distance.            (changed)
!     final header 31  = center cmp inline distance.             (changed)
!     final header 37  = nearest [or center] cmp shotpoint.
!     final header 38  = cmp line number (average of source and receiver).
!     final header 39  = pre-nmo datum shift = 0.
!     final header 40  = post-nmo datum shift.
!     final header 44  = source uphole time (if nil).            (changed)
!     final header 45  = receiver uphole time = 0.
!     final header 46  = source sequential ground pos.  (possibly adjusted for
!     final header 47  = receiver sequential ground pos. excessive inline skid)
!     final header 58 = cmp x location.
!     final header 59 = cmp y location.
!     final header 60 = source inline distance.
!     final header 61 = receiver inline distance.


      subroutine geomdata_use_ld_cards (obj,hhh,err,msg)
      implicit none
      type(geomdata_struct),intent(in)    :: obj                  ! arguments
      double precision     ,intent(inout) :: hhh(:)               ! arguments
      integer              ,intent(out)   :: err                  ! arguments
      character(len=*)     ,intent(out)   :: msg                  ! arguments
      integer                             :: kas,kbs,ls           ! local
      integer                             :: kar,kbr,lr           ! local
      integer                             :: la,lb                ! local
      real                                :: wa,wb,source,receiv  ! local
      real                                :: past1,past2,past3    ! local

!----------get source headers.

      call geomdata_get_sr_headers (obj,'source',                    &  ! in
                                    hhh(29),hhh(26),hhh(30),hhh(31), &  ! inout
                                    hhh(11),hhh(12),hhh(46),hhh(60), &  ! out
                                    kas,kbs,ls,err,msg)                 ! out
      if (err == GEOMDATA_ERROR) return
      if (hhh(13) == FNIL) hhh(13) = obj%elev (ls)  ! source surface elevation.
      if (hhh(20) == FNIL) hhh(20) = obj%depth(ls)  ! source hole depth.
      if (hhh(44) == FNIL) hhh(44) = obj%tuh  (ls)  ! source uphole time.

!----------get receiver headers.

      call geomdata_get_sr_headers (obj,'receiver',                  &  ! in
                                    hhh(28),hhh(27),hhh(32),hhh(33), &  ! inout
                                    hhh(14),hhh(15),hhh(47),hhh(61), &  ! out
                                    kar,kbr,lr,err,msg)                 ! out
      if (err == GEOMDATA_ERROR) return
      hhh(16) = obj%elev(lr) + obj%elsd(lr) + hhh(16)  ! receiver surface elev.
      hhh(21) = 0.0                                    ! receiver hole depth.
      hhh(45) = 0.0                                    ! receiver uphole time.

!----------calculate pre and post static.

      source = -1000.0*(hhh(13)-hhh(20)      -obj%datum)/obj%ve + obj%ts(ls)
      receiv = -obj%tuh(lr)  &
               -1000.0*(hhh(16)-obj%depth(lr)-obj%datum)/obj%ve + obj%tr(lr)
      hhh(39) = 0.0                     ! pre static.
      hhh(40) = source + receiv         ! post static.

!----------set several coordinate headers.

      hhh(6) = sqrt((hhh(14)-hhh(11))**2+(hhh(15)-hhh(12))**2)    !  offset.
      hhh(17) = 0.5 * (hhh(11) + hhh(14))     !  CMP X distance coordinate.
      hhh(18) = 0.5 * (hhh(12) + hhh(15))     !  CMP Y distance coordinate.
      hhh(38) = 0.5 * (hhh(26) + hhh(27))     !  CMP line number.
      hhh(58) = hhh(17)                       !  CMP X distance coordinate.
      hhh(59) = hhh(18)                       !  CMP Y distance coordinate.
      hhh(31) = 0.5 * (hhh(60) + hhh(61))     !  center CMP inline distance.

!----------find nearest ld cards to this cmp (on source line).

      call geomdata_find (hhh(17),hhh(18),obj%xloc,obj%yloc,kas,kbs, &
                          la,lb,wa,wb)
      hhh(19) = wa*obj%elev(la) + wb*obj%elev(lb)   ! nearest CMP surface elev.
      hhh(37) = wa*obj%sp  (la) + wb*obj%sp  (lb)   ! nearest CMP shotpoint.
      hhh(30) = wa*obj%cum (la) + wb*obj%cum (lb)   ! nearest CMP inline dist.

!----------find nearest ld cards to this cmp (on receiver line).

      if (kar /= kas) then
        call geomdata_find (hhh(17),hhh(18),obj%xloc,obj%yloc,kar,kbr, &
                            la,lb,wa,wb)
        hhh(19) =   0.5 * (hhh(19)  +  wa*obj%elev(la)  +  wb*obj%elev(lb))
        hhh(37) =   0.5 * (hhh(37)  +  wa*obj%sp  (la)  +  wb*obj%sp  (lb))
        hhh(30) =   0.5 * (hhh(30)  +  wa*obj%cum (la)  +  wb*obj%cum (lb))
      end if

!----------adjust cmp coordinates if fixdist is positive.

      if (obj%fixdist <= 0.0) then
           err = GEOMDATA_OK
           msg = 'OK'
           return
      end if
      hhh(17) = hhh(31)                !  CMP X distance coordinate.
      hhh(18) = 0.0                    !  CMP Y distance coordinate.

!----------find center ld cards for this cmp (on source line).

      call geomdata_find2 (hhh(31),obj%cum,kas,kbs,   la,lb,wa,wb,past1)
      hhh(19) = wa*obj%elev(la) + wb*obj%elev(lb)   ! center CMP surface elev.
      hhh(37) = wa*obj%sp  (la) + wb*obj%sp  (lb)   ! center CMP shotpoint.

!----------find center ld cards for this cmp (on receiver line).

      if (kar /= kas) then
        call geomdata_find2 (hhh(31),obj%cum,kar,kbr,   la,lb,wa,wb,past2)
        if (past1 == 0.0 .and. past2 == 0.0) then
          hhh(19) = 0.5*(  hhh(19) + wa*obj%elev(la) + wb*obj%elev(lb)  )
          hhh(37) = 0.5*(  hhh(37) + wa*obj%sp  (la) + wb*obj%sp  (lb)  )
        else
          past3 = past1+past2
          hhh(19) = ( past2*hhh(19) +  &
                      past1*(wa*obj%elev(la) + wb*obj%elev(lb)) ) /past3
          hhh(37) = ( past2*hhh(37) +  &
                      past1*(wa*obj%sp  (la) + wb*obj%sp  (lb)) ) /past3
        end if
      end if
      err = GEOMDATA_OK
      msg = 'OK'
      return
      end subroutine geomdata_use_ld_cards


!!----------------------- geomdata use zt cards ---------------------------!!
!!----------------------- geomdata use zt cards ---------------------------!!
!!----------------------- geomdata use zt cards ---------------------------!!

!----------headers we have to use.
!     final header 9   = group number.
!     final header 10  = channel number.
!     final header 26  = source line number.
!     final header 27  = receiver line number.
!     final header 28  = receiver shotpoint.
!     final header 29  = source shotpoint.

!----------headers we calculate.
!     final header 25  = 1 (live) or 0 (dead) or -1 (reverse polarity).


      subroutine geomdata_use_zt_cards (obj,hhh)
      implicit none
      type(geomdata_struct),intent(in)    :: obj      ! arguments
      double precision     ,intent(inout) :: hhh(:)   ! arguments
      integer                             :: i        ! local

!----------get started.

      hhh(25) = 1.0

!----------test zt1 cards.

      do i = 1,obj%nzt1
           if (obj%lll1(i) /= hhh(26)) cycle
           if (hhh(29) < obj%sss1(i) .or. hhh(29) > obj%sss1a(i)) cycle
           call geomdata_use_zt_cards_helper (obj%ccc1(i),hhh)
      end do

!----------test zt2 cards.

      do i = 1,obj%nzt2
           if (obj%lll2(i) /= hhh(27)) cycle
           if (hhh(28) < obj%rrr2(i) .or. hhh(28) > obj%rrr2a(i)) cycle
           call geomdata_use_zt_cards_helper (obj%ccc2(i),hhh)
      end do

!----------test zt3 cards.

      do i = 1,obj%nzt3
           if (hhh( 9) < obj%iggg3(i) .or. hhh( 9) > obj%iggg3a(i)) cycle
           if (hhh(10) < obj%ittt3(i) .or. hhh(10) > obj%ittt3a(i)) cycle
           call geomdata_use_zt_cards_helper (obj%ccc3(i),hhh)
      end do

!----------test zt4 cards.

      do i = 1,obj%nzt4
           if (obj%lll4 (i) /= hhh(26)) cycle
           if (obj%lll4a(i) /= hhh(27)) cycle
           if (hhh(29) < obj%sss4(i) .or. hhh(29) > obj%sss4a(i)) cycle
           if (hhh(28) < obj%rrr4(i) .or. hhh(28) > obj%rrr4a(i)) cycle
           call geomdata_use_zt_cards_helper (obj%ccc4(i),hhh)
      end do
      return
      end subroutine geomdata_use_zt_cards




                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!
                    !!------ private routines -------!!



!!-------------------- geomdata use zt cards helper -------------------------!!
!!-------------------- geomdata use zt cards helper -------------------------!!
!!-------------------- geomdata use zt cards helper -------------------------!!

! set header word 25 based upon code on zt card.
! codes are "MISS", "ZERO", "REV" in order of priority.
! headers are originally set as follows: hhh(25) = 1.
! headers might have subsequently changed in previous calls to this routine.


      subroutine geomdata_use_zt_cards_helper (code,hhh)
      implicit none
      character(len=*),intent(in)    :: code      ! arguments
      double precision,intent(inout) :: hhh(:)    ! arguments

      if (code == "MISS") hhh(25) = -999.0
      if (hhh(25) == -999) return
      if (code == "ZERO") then
           hhh(25) = 0.0
      else if (hhh(25) /= 0.0) then
           hhh(25) = -1.0
      end if
      return
      end subroutine geomdata_use_zt_cards_helper


!!--------------------------- geomdata find -------------------------------!!
!!--------------------------- geomdata find -------------------------------!!
!!--------------------------- geomdata find -------------------------------!!

! find nearest ld cards to this cmp location.
! restricts search between indices ka and kb.
! uses both x and y coordinates.
! returns la and lb such that xloc(la),yloc(la) and xloc(lb),yloc(lb)
!  bracket the cmp.
! returns wa and wb which are the appropriate corresponding weights.


      subroutine geomdata_find (hdx,hdy,   xloc,yloc,ka,kb,   la,lb,wa,wb)
      implicit none
      double precision,intent(in)  :: hdx,hdy                ! arguments
      integer         ,intent(in)  :: ka,kb                  ! arguments
      real            ,intent(in)  :: xloc(:),yloc(:)        ! arguments
      integer         ,intent(out) :: la,lb                  ! arguments
      real            ,intent(out) :: wa,wb                  ! arguments
      real                         :: scr(kb)                ! local
      real                         :: dab2,dab,dac,sum       ! local
      integer                      :: i                      ! local

!----------get distance of cmp from each flag.

      do i = ka,kb
           scr(i) = (xloc(i)-hdx)**2+(yloc(i)-hdy)**2
      end do

!----------get index of nearest flag.

      la = mth_ismin(kb-ka+1,scr(ka),1)+ka-1
      if (scr(la) < 0.01) go to 20

!----------find second nearest location.

      if (la == ka) then
           lb = ka+1
      else if (la == kb) then
           lb = kb-1
      else if (scr(la-1) <= scr(la+1)) then
           lb = la-1
      else
           lb = la+1
      end if

!----------get the corresponding weights.

      dab2 = (xloc(lb)-xloc(la))**2+(yloc(lb)-yloc(la))**2
      dab = sqrt(dab2)
      if (dab == 0.0) go to 20
      dac = (scr(la)-scr(lb)+dab2)/(2.0*dab)
      if (dac <= 0.0 .or. dac >= dab) go to 20
      wa  = 1.0/dac
      wb  = 1.0/(dab-dac)
      sum = wa+wb
      wa  = wa/sum
      wb  = wb/sum
      return

!----------exact location found.

20    lb = la
      wa = 1.0
      wb = 0.0
      return
      end subroutine geomdata_find


!!-------------------------- geomdata find2 -------------------------------!!
!!-------------------------- geomdata find2 -------------------------------!!
!!-------------------------- geomdata find2 -------------------------------!!

! find nearest ld cards to this cmp location.
! restricts search between indices ka and kb.
! uses only x coordinates.
! returns la and lb such that xloc(la) and xloc(lb) bracket the cmp.
! returns wa and wb which are the appropriate corresponding weights.
! returns past = distance past end of line; otherwise past=0.


      subroutine geomdata_find2 (hdx,   xloc,ka,kb,   la,lb,wa,wb,past)
      implicit none
      double precision,intent(in)  :: hdx                    ! arguments
      integer         ,intent(in)  :: ka,kb                  ! arguments
      real            ,intent(in)  :: xloc(:)                ! arguments
      integer         ,intent(out) :: la,lb                  ! arguments
      real            ,intent(out) :: wa,wb,past             ! arguments
      real                         :: scr(kb)                ! local
      real                         :: sum                    ! local
      integer                      :: i                      ! local

!----------test for being at (or past) beginning or end.

      if ( (hdx < xloc(ka).and.xloc(ka) < xloc(ka+1)) .or.  &
           (hdx > xloc(ka).and.xloc(ka) > xloc(ka+1))     ) then
           la = ka
           past = abs(xloc(ka)-hdx)
           go to 20
      else if ( (hdx < xloc(kb).and.xloc(kb) < xloc(kb-1)) .or.  &
                (hdx > xloc(kb).and.xloc(kb) > xloc(kb-1))     ) then
           la = kb
           past = abs(xloc(kb)-hdx)
           go to 20
      end if

!----------get distance of cmp from each flag.

      past = 0.0
      do i = ka,kb
           scr(i) = abs(xloc(i)-hdx)
      end do

!----------get index of nearest flag.

      la = mth_ismin(kb-ka+1,scr(ka),1)+ka-1
      if (scr(la) < 0.01) go to 20

!----------find second nearest location.

      if (la == ka) then
           lb = ka+1
      else if (la == kb) then
           lb = kb-1
      else if (scr(la-1) <= scr(la+1)) then
           lb = la-1
      else
           lb = la+1
      end if

!----------get the corresponding weights.

      wa  = 1.0/scr(la)
      wb  = 1.0/scr(lb)
      sum = wa+wb
      wa  = wa/sum
      wb  = wb/sum
      return

!----------exact location found.

20    lb = la
      wa = 1.0
      wb = 0.0
      return
      end subroutine geomdata_find2


!!------------------------ geomdata get sr headers ------------------------!!
!!------------------------ geomdata get sr headers ------------------------!!
!!------------------------ geomdata get sr headers ------------------------!!

! calculate some source or receiver header words from ld cards.

!----------input parameters.
! which              = 'source' for source and 'receiver' for receiver.
! header hdsp        = shotpoint INDEX of source or receiver.
! header hdline      = source or receiver line (not INDEX).
! header hdiskid     = inline    skid of source (from pp) or receiver (from rp).
! header hdcskid     = crossline skid of source (from pp) or receiver (from rp).
! obj%linelist(nlines) = line numbers.
! obj%ifirst  (nlines) = first ld card index for each line.
! obj%ilast   (nlines) = last ld card index for each line.
! obj%iconst  (nlines) = special constant for each line.
! obj%sp    (:)      = shotpoint coordinates of all flags.
! obj%xloc  (:)      = x coordinates of all flags.
! obj%yloc  (:)      = y coordinates of all flags.
! obj%cum   (:)      = inline distance.
! obj%xsd   (:)      = inline    skids of receiver at all flags.
! obj%ysd   (:)      = crossline skids of receiver at all flags.
! obj%sina  (:)      = sines   of local azimuths of line at all flags.
! obj%cosa  (:)      = cosines of local azimuths of line at all flags.
! obj%cmpinc        = ld card increment for adjacent cmp locs (multiple of 0.5).
! obj%fixdist        = fixed distance.

!----------output parameters.
! header hdsp     = shotpoint of source or receiver.
! header hdline   = line number of source or receiver.
! header hdiskid  = inline    skid (updated if receiver, or excessive skid).
! header hdcskid  = crossline skid (updated if receiver).
! header hdx      = true skidded x coordinate.
! header hdy      = true skidded y coordinate.
! header hdgp     = sequential ground position.
! header hdinline = inline distance.
! ka              = index of first ld card for this line.
! kb              = index of last ld card for this line.
! indx            = index of ld card for this line and shotpoint.
! err             = error flag.
! msg             = error message.


      subroutine geomdata_get_sr_headers                       &
                      (obj,which,hdsp,hdline,hdiskid,hdcskid,  &
                       hdx,hdy,hdgp,hdinline,ka,kb,indx,err,msg)
      implicit none
      type(geomdata_struct),intent(in) :: obj                       ! arguments
      character(len=*),intent(in)    :: which                       ! arguments
      double precision,intent(in)    :: hdline                      ! arguments
      double precision,intent(inout) :: hdsp,hdiskid,hdcskid        ! arguments
      double precision,intent(out)   :: hdx,hdy,hdgp,hdinline       ! arguments
      integer         ,intent(out)   :: ka,kb,indx                  ! arguments
      integer         ,intent(out)   :: err                         ! arguments
      character(len=*),intent(out)   :: msg                         ! arguments
      integer                        :: inc,j,indx2,i,line          ! local

!----------get started.

      inc = nint(2.0*obj%cmpinc)
      err = GEOMDATA_OK
      msg = 'OK'

!----------get and verify the line index.

      line = nint(hdline)
      j = 0
      do i = 1,obj%nlines
           if (line == obj%linelist(i)) then
                j = i
                exit
           end if
      end do
      if (j == 0) then
           call string_encode (msg, trim(which)//' line ', &
                               line,'not found on LD cards')
           err = GEOMDATA_ERROR
           return
      end if

!----------get and verify the shotpoint index.

      indx = nint(hdsp)+obj%iconst(j)
      if (indx < obj%ifirst(j) .or. indx > obj%ilast(j)) then
           call string_encode (msg, trim(which)//' shotpoint with index', &
                                    indx,'not found on LD cards')
           err = GEOMDATA_ERROR
           return
      end if

!----------update the skids if this is a receiver.

      if (which(1:1) == 'r') then
           hdiskid = hdiskid+obj%xsd(indx)    !  inline skid
           hdcskid = hdcskid+obj%ysd(indx)    !  crossline skid
      end if

!----------return first and last ld card for this line.

      ka = obj%ifirst(j)
      kb = obj%ilast(j)

!----------adjust the ld card number for excessive positive inline skid.

      if (hdiskid > 1.0) then
6542       if (indx+inc <= obj%ilast(j)) then
                if (hdiskid > 0.51*(obj%cum(indx+inc)-obj%cum(indx))) then
                     hdiskid = hdiskid-(obj%cum(indx+inc)-obj%cum(indx))
                     indx = indx+inc
                     if (hdiskid > 1) go to 6542
                end if
           end if

!----------adjust the ld card number for excessive negative inline skid.

      else if (hdiskid < -1.0) then
6543       if (indx-inc >= obj%ifirst(j)) then
                if (hdiskid < 0.51*(obj%cum(indx-inc)-obj%cum(indx))) then
                     hdiskid = hdiskid-(obj%cum(indx-inc)-obj%cum(indx))
                     indx = indx-inc
                     if (hdiskid < -1.0) go to 6543
                end if
           end if
      end if

!----------return various header words.

      if (obj%fixdist > 0.0) then
           hdgp = nint(hdsp) - obj%minspi + 1
      else
           hdgp = indx                   ! hdgp = nint(hdsp) + obj%iconst(j)
      end if

      if (hdiskid > 0.0 .and. indx < obj%ilast(j)) then
           indx2 = indx + 1
      else
           indx2 = indx
      end if

      hdsp     = obj%sp(indx)
      hdx      = obj%xloc(indx)+hdiskid*obj%cosa(indx2)-hdcskid*obj%sina(indx2)
      hdy      = obj%yloc(indx)+hdiskid*obj%sina(indx2)+hdcskid*obj%cosa(indx2)
      hdinline = obj%cum(indx)
      return
      end subroutine geomdata_get_sr_headers


!!------------------ geomdata print line summary ---------------------------!!
!!------------------ geomdata print line summary ---------------------------!!
!!------------------ geomdata print line summary ---------------------------!!


      subroutine geomdata_print_line_summary (obj,lun)
      implicit none
      type(geomdata_struct),intent(in) :: obj                  ! arguments
      integer              ,intent(in) :: lun                  ! arguments
      integer                          :: j,ka,kb,ia,ib        ! local

      if (lun <= 0) return

      write(lun,*) ' '
      write(lun,*) 'NUMBER OF LINES ON LD CARDS = ',obj%nlines, &
                               '      minspi (for debug) = ',obj%minspi
      write(lun,*) ' '

      write(lun,1001)
1001  format (7x,' LINE    FIRST  LAST    FIRST     LAST     NUMBER  ', &
                                      '   first     last    iconst   '/ &
              7x,'NUMBER    LD     LD   SHOTPOINT SHOTPOINT    OF    ', &
                                      ' shotpoint shotpoint          '/ &
              7x,'         card#  card#                    SHOTPOINTS', &
                                      '   index     index            ')
      do j=1,obj%nlines
           ka = obj%ifirst(j)          ! first LD card number.
           kb = obj%ilast(j)           ! last LD card number.
           ia = ka - obj%iconst(j)     ! first shotpoint index.
           ib = kb - obj%iconst(j)     ! last shotpoint index.
           write(lun,1000) j,obj%linelist(j),ka,kb,obj%sp(ka),obj%sp(kb), &
                           kb-ka+1,ia,ib,obj%iconst(j)
1000        format (1x,i4,i7,1x,2i7,2f10.2,i8,2i10,i9)
      end do
      write(lun,*) ' '
      return
      end subroutine geomdata_print_line_summary


!!------------------ geomdata print ld cards -------------------------------!!
!!------------------ geomdata print ld cards -------------------------------!!
!!------------------ geomdata print ld cards -------------------------------!!


      subroutine geomdata_print_ld_cards (obj,lun,opt_tables)
      implicit none
      type(geomdata_struct),intent(in) :: obj                  ! arguments
      integer              ,intent(in) :: lun                  ! arguments
      character(len=*)     ,intent(in) :: opt_tables           ! arguments.
      integer                          :: i,j,ka,kb,igp        ! local
      real                             :: temp,azimuth         ! local
      logical                          :: full                 ! local

      if (lun <= 0) return

      full = (opt_tables == 'LD' .or. opt_tables == 'LRPZ')
      do j=1,obj%nlines
        if (.not.full .and. j /= 1 .and. j /= obj%nlines) then
           if (j == 2) then
               write(lun,*) ' '
               write(lun,*) 'LD CARDS FOR JUST THE FIRST AND',   &
                            ' LAST LINES ARE LISTED ABOVE AND BELOW'
           end if
           cycle
        end if
        write(lun,2001) obj%linelist(j),' '
2001    format (2x/55x,'LINE',i5,2x,a9/                               &
              '     LD                 INCR     --COORDINATES-- ',    &
         '                               --REC SK',                   &
         'ID---  GROUND  -inline distance-         '/                 &
              '   card#       SP#      DIST     XLOC       YLOC ',    &
         '   ELEV    HD  TUH   TR   TS   XSD  YSD',                   &
         ' ELSD    POS   actual      fixed  azimuth')
        ka   = obj%ifirst(j)
        kb   = obj%ilast(j)
        temp = 0.0
        do i = ka,kb
            if (.not.full .and. i > ka+5 .and. i < kb-5) then
               if (i == ka+6) &
                     write(lun,*) '      --- cards skipped here ---'
               cycle
            end if
            if (full .and. i > ka .and. mod(i-ka+1,50) == 1) then
               write(lun,2001) obj%linelist(j),'CONTINUED'
            end if
            if (obj%fixdist /= 0.0) &
                          temp = obj%cum(ka) + (i-ka)*abs(obj%fixdist)
            if (obj%fixdist >  0.0) then
               igp = i - obj%iconst(j) - obj%minspi + 1
            else
               igp = i
            end if
            azimuth = DEGREES_PER_RADIAN * asin(obj%sina(i))
            write(lun,2000) i,obj%sp(i),obj%dist(i),                &
                   obj%xloc(i),obj%yloc(i),obj%elev(i),                  &
                   nint(obj%depth(i)),nint(obj%tuh(i)),                  &
                   nint(obj%tr(i)),nint(obj%ts(i)),                      &
                   nint(obj%xsd(i)),nint(obj%ysd(i)),nint(obj%elsd(i)),  &
                   igp,obj%cum(i),temp,azimuth
2000        format (1x,i7,f10.2,3f10.1,f8.0,i6,3i5,i6,2i5,i7,2f10.1,f9.0)
        end do
      end do
      return
      end subroutine geomdata_print_ld_cards


!!------------------ geomdata print rp cards -------------------------------!!
!!------------------ geomdata print rp cards -------------------------------!!
!!------------------ geomdata print rp cards -------------------------------!!


      subroutine geomdata_print_rp_cards (obj,lun,opt_tables)
      implicit none
      type(geomdata_struct),intent(in) :: obj                  ! arguments
      integer              ,intent(in) :: lun                  ! arguments
      character(len=*)     ,intent(in) :: opt_tables           ! arguments.
      integer                          :: i,nchan              ! local
      character(len=6)                 :: msg                  ! local
      logical                          :: full                 ! local

      if (lun <= 0) return
      full = (opt_tables == 'RPZ' .or. opt_tables == 'LRPZ')

      write(lun,3000)
3000  format (2x/'     RP                       --RECEIVER--   ',  &
                '--INLINE--   --CROSSLINE--   ---RECEIVER SKID---')
      write(lun,2001)
2001  format ('   card#      ',                                 &
                ' PAT#   FLAG     sp#   LINE#    #X  XINC  ',   &
                '    #Y   YINC     XsD     YsD    ELsD ',       &
                '     #channels for this PAT#')
      nchan=0
      do i = 1,obj%nrp
          if (obj%flag(i) == "X" .or. obj%flag(i) == "Y") &
                                        nchan = nchan + obj%nx(i)*obj%ny(i)
          msg = ' '
          if (i == obj%nrp) then
               write (msg,'(I6)') nchan
               nchan=0
          else if (obj%ipat1(i+1) /= obj%ipat1(i)) then
               write (msg,'(I6)') nchan
               nchan=0
          end if
          if (.not.full .and. i > 5 .and. i < obj%nrp-5) then
               if (i == 6) write(lun,*) '      --- cards skipped here ---'
               cycle
          end if
          write(lun,1001) i,obj%ipat1(i),obj%flag(i),        &
                         obj%sp1(i),obj%line1(i),obj%nx(i),       &
                         obj%ixinc(i),obj%ny(i),obj%iyinc(i),     &
                         obj%xsd1(i),obj%ysd1(i),obj%elsd1(i),msg
1001      format (1x,i7,i11,3x,a4,f9.2,i6,i7,i6,i8,i7,f9.0,f8.0,f8.0,12x,a6)
      end do
      return
      end subroutine geomdata_print_rp_cards


!!------------------ geomdata print pp cards -------------------------------!!
!!------------------ geomdata print pp cards -------------------------------!!
!!------------------ geomdata print pp cards -------------------------------!!


      subroutine geomdata_print_pp_cards (obj,lun,opt_tables)
      implicit none
      type(geomdata_struct),intent(in) :: obj                  ! arguments
      integer              ,intent(in) :: lun                  ! arguments
      character(len=*)     ,intent(in) :: opt_tables           ! arguments.
      integer                          :: i,nthis              ! local
      character(len=8)                 :: eee,hhh,ttt          ! local
      logical                          :: full                 ! local

      if (lun <= 0) return
      full = (opt_tables == 'RPZ' .or. opt_tables == 'LRPZ')

      write(lun,2001)
2001  format (2x/'    PP      ---SOURCE----    ---RECEIVER---    ',   &
            '        --SOURCE SKID---    --------NEW---------   ',    &
            '  ----MOVE---    THRU   #groups on'/                     &
                 '  card#     SP#     LINE#    SP#      LINE#    ',   &
            'PAT#    XSD   YSD   HOLD    ELEV    HD       TUH   ',    &
            '  SOURCE  REC    GRP#    this card')
      nthis = obj%ig(1)
      do i = 1,obj%npp
          if (.not.full .and. i > 5 .and. i < obj%npp-5) then
               if (i == 6) write(lun,*) '      --- cards skipped here ---'
               cycle
          end if
          if (i > 1) nthis = obj%ig(i) - obj%ig(i-1)
          eee = 'none'
          hhh = 'none'
          ttt = 'none'
          if (obj%elev2 (i) /= FNIL) eee = string_ff2ss(obj%elev2 (i), 8)
          if (obj%depth2(i) /= FNIL) hhh = string_ff2ss(obj%depth2(i), 8)
          if (obj%tuh2  (i) /= FNIL) ttt = string_ff2ss(obj%tuh2  (i), 8)
          write(lun,2000) i,obj%sp2(i),obj%line2(i),obj%sp3(i),obj%line3(i), &
             obj%ipat2(i),obj%xsd2(i),obj%ysd2(i),obj%hold(i),eee,hhh,ttt,   &
             obj%is(i),obj%ir(i),obj%ig(i),nthis
2000      format (i7,f10.2,i7,f10.2,i8,i9,2x,2f6.0,i6,4x,3a8,2i6,2i8)
      end do
      return
      end subroutine geomdata_print_pp_cards


!!------------------ geomdata print zt cards -------------------------------!!
!!------------------ geomdata print zt cards -------------------------------!!
!!------------------ geomdata print zt cards -------------------------------!!


      subroutine geomdata_print_zt_cards (obj,lun,opt_tables)
      implicit none
      type(geomdata_struct),intent(in) :: obj                  ! arguments
      integer              ,intent(in) :: lun                  ! arguments
      character(len=*)     ,intent(in) :: opt_tables           ! arguments.
      integer                          :: i                    ! local
      logical                          :: full                 ! local

      if (lun <= 0) return
      full = (opt_tables == 'RPZ' .or. opt_tables == 'LRPZ')

      if (obj%nzt1 > 0) then
           write(lun,*) ' '
           write(lun,4001)
4001       format (' ZERO SOURCES            code   FROM SP#   TO SP# ', &
                   '    LINE#',23x,'  ZT1 card#')
           do i = 1,obj%nzt1
                if (.not.full .and. i > 5 .and. i < obj%nzt1-5) then
                     if (i == 6) &
                       write(lun,*) '      --- cards skipped here ---'
                else
                     write(lun,4000) obj%ccc1(i),obj%sss1(i),  &
                                          obj%sss1a(i),obj%lll1(i),i
                end if
           end do
4000       format (25x,a4,f11.0,f9.0,i9,22x,i10)
      end if

      if (obj%nzt2 > 0) then
           write(lun,*) ' '
           write(lun,5001)
5001       format (' ZERO RECEIVERS          code   FROM SP#   TO SP# ', &
                   '    LINE#',23x,'  ZT2 card#')
           do i = 1,obj%nzt2
                if (.not.full .and. i > 5 .and. i < obj%nzt2-5) then
                     if (i == 6) &
                       write(lun,*) '      --- cards skipped here ---'
                else
                     write(lun,4000) obj%ccc2(i),obj%rrr2(i),  &
                                          obj%rrr2a(i),obj%lll2(i),i
                end if
           end do
      end if

      if (obj%nzt3 > 0) then
           write(lun,*) ' '
           write(lun,6001)
6001       format (' ZERO TRACES IN GROUPS   code   FROM GRP#  ', &
                   '  TO GRP#        TRC#   TO TRC#',8x,'  ZT3 card#')
           do i = 1,obj%nzt3
                if (.not.full .and. i > 5 .and. i < obj%nzt3-5) then
                     if (i == 6) &
                       write(lun,*) '      --- cards skipped here ---'
                else
                     write(lun,6000) obj%ccc3(i),  &
                                          obj%iggg3(i),obj%iggg3a(i),  &
                                          obj%ittt3(i),obj%ittt3a(i),i
                end if
           end do
6000       format (25x,a4,i12,i11,i12,i10,6x,i10)
      end if

      if (obj%nzt4 > 0) then
           write(lun,*) ' '
           write(lun,7001)
7001       format (' cODe       from SOURCE sp#   to sp#  line#  ',  &
                   '  REC sp#   to sp#  line#',12x,'  ZT4 card#')
           do i = 1,obj%nzt4
                if (.not.full .and. i > 5 .and. i < obj%nzt4-5) then
                     if (i == 6) &
                       write(lun,*) '      --- cards skipped here ---'
                else
                     write(lun,7000) obj%ccc4(i),  &
                             obj%sss4(i),obj%sss4a(i),obj%lll4(i),  &
                             obj%rrr4(i),obj%rrr4a(i),obj%lll4a(i),i
                end if
           end do
7000       format (2x,a4,f22.0,f9.0,i6,f12.0,f9.0,i6,i20)
      end if
      write(lun,*) ' '
      return
      end subroutine geomdata_print_zt_cards


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module geomdata_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

