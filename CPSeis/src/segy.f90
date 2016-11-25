!<CPS_v1 type="PRIMITIVE"/>
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
! Name       : SEGY
! Category   : io
! Written    : 1999-12-09   by: Bill Menger
! Revised    : 2007-01-25   by: Bill Menger
! Maturity   : beta
! Purpose    : primitive to read/write segy file headers and to convert segy
!              trace headers to/from cps header format.
! Portability: Solaris has trouble with fortran i/o that uses "advance=no",
!              especially when mixed with "c" i/o from fprint.  Since the
!              segy_tabplot routine uses fprints, sometimes Solaris dies on
!              a fortran i/o error in an unrelated write statement if your
!              calling routine uses "advance=no" on fortran writes.
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!  These routines read/write a segy ebcdic header,
!                 read/write a segy binary header, and
!                 convert segy/cps and back for trace headers.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
! ----------- MAPPING OF CPS to SEGY -----------------------------------
!    fescale = 1.0
!    fxscale = 1.0
!    segyhd%tracl               = cpshd(HDR_SEQUENCE)
!    segyhd%tracr               = cpshd(HDR_SEQUENCE)
!    segyhd%fldr                = cpshd(HDR_CURRENT_GROUP)
!    segyhd%tracf               = cpshd(HDR_CURRENT_CHANNEL)
!    segyhd%ep                  = cpshd(HDR_SOURCE_SHOTPOINT)
!    segyhd%cdp                 = cpshd(HDR_MIDPOINT_SHOTPOINT)
!    segyhd%cdpt                = cpshd(HDR_MIDPOINT_LINE)
!    segyhd%trid                = 1
!    segyhd%nvs                 = 0
!    segyhd%nhs                 = cpshd(HDR_FOLD)
!    segyhd%duse                = 0
!    segyhd%offset              = cpshd(HDR_OFFSET)
!    segyhd%gelev               = cpshd(HDR_RECEIVER_ELEV)/fescale
!    segyhd%selev               = cpshd(HDR_SOURCE_ELEV)/fescale
!    segyhd%sdepth              = cpshd(HDR_SOURCE_DEPTH)/fescale
!    segyhd%gdel                = 0
!    segyhd%sdel                = 0
!    segyhd%swdep               = 0
!    segyhd%gwdep               = 0
!    if(fescale >= 1.0 ) then
!      segyhd%scalel            = nint(fescale)
!    else
!      segyhd%scalel            = -nint(1./fescale)
!    endif
!    if(fxscale >= 1.0 ) then
!      segyhd%scalco            = nint(fxscale)
!    else
!      segyhd%scalco            = -nint(1./fxscale)
!    endif
!    segyhd%sx                  = cpshd(HDR_SOURCE_XLOC)/fxscale
!    segyhd%sy                  = cpshd(HDR_SOURCE_YLOC)/fxscale
!    segyhd%gx                  = cpshd(HDR_RECEIVER_XLOC)/fxscale
!    segyhd%gy                  = cpshd(HDR_RECEIVER_YLOC)/fxscale
!    segyhd%counit              = 0
!    segyhd%wevel               = 0
!    segyhd%swevel              = 0
!    segyhd%sut                 = cpshd(HDR_SOURCE_UPTIME)
!    segyhd%gut                 = cpshd(HDR_RECEIVER_UPTIME)
!    segyhd%sstat               = cpshd(HDR_PRE)
!    segyhd%gstat               = cpshd(HDR_POST)
!    segyhd%tstat               = cpshd(HDR_CUM_RESID_STATIC)
!    segyhd%laga                = 0
!    segyhd%lagb                = 0
!    segyhd%delrt               = 0
!    segyhd%muts                = 1e3*(tstart + (cpshd(HDR_TOP_MUTE)-1)*dt)
!    segyhd%mute                = 1e3*(tstart + (cpshd(HDR_BOTTOM_MUTE)-1)*dt)
!    segyhd%ns                  = ns
!    segyhd%dt                  = dt*1.e6
!    segyhd%gain                = 0
!    segyhd%igc                 = 0
!    segyhd%igi                 = 0
!    segyhd%corr                = 0
!    segyhd%sfs                 = 0
!    segyhd%sfe                 = 0
!    segyhd%slen                = 0
!    segyhd%styp                = 0
!    segyhd%stas                = 0
!    segyhd%stae                = 0
!    segyhd%tatyp               = 0
!    segyhd%afilf               = 0
!    segyhd%afils               = 0
!    segyhd%nofilf              = 0
!    segyhd%nofils              = 0
!    segyhd%lcf                 = 0
!    segyhd%hcf                 = 0
!    segyhd%lcs                 = 0
!    segyhd%hcs                 = 0
!    segyhd%year                = 0
!    segyhd%day                 = 0
!    segyhd%hour                = 0
!    segyhd%minute              = 0
!    segyhd%sec                 = 0
!    segyhd%timbas              = 0
!    segyhd%trwf                = 0
!    segyhd%grnors              = 0
!    segyhd%grnofr              = 0
!    segyhd%grnlof              = 0
!    segyhd%gaps                = 0
!    segyhd%otrav               = 0
!    segyhd%unused1             = 0                   !    -> 237:240
!
!
! ----------- MAPPING OF SEGY to CPS -----------------------------------
!
!    top_mute    = 1 + nint((segyhd%muts*1d-3 - tstart)/(segyhd%dt*1D-6))
!    bottom_mute = 1 + nint((segyhd%mute*1d-3 - tstart)/(segyhd%dt*1D-6))
!    ! calculate the scalers
!    fescale = 1.0
!    if (segyhd%scalel /= 0 .and. &
!        segyhd%scalel > -10001 .and. &
!        segyhd%scalel <  10001) then
!      if(segyhd%scalel > 0 ) then
!        fescale = segyhd%scalel*fescale
!      else
!        fescale = -fescale/segyhd%scalel
!      endif
!    endif
!    fxscale = 1.0
!    if (segyhd%scalco /= 0 .and. &
!        segyhd%scalco > -10001 .and. &
!        segyhd%scalco <  10001) then
!      if(segyhd%scalco > 0 ) then
!        fxscale = segyhd%scalco*fxscale
!      else
!        fxscale = -fxscale/segyhd%scalco
!      endif
!    endif
!    !                                                cps <- SEGY bytes
!    !---                                             head#  stb:enb
!    cpshd(HDR_SEQUENCE) = segyhd%tracl               ! 1 <- 001:004
!    cpshd(HDR_TOP_MUTE) = top_mute                   ! 2 <- 111:112 derived
!    cpshd(HDR_CURRENT_GROUP) = segyhd%fldr           ! 3 <- 009:012
!    cpshd(HDR_CURRENT_CHANNEL) = segyhd%tracf        ! 4 <- 013:016
!    cpshd(HDR_FOLD) = segyhd%nhs                     ! 5 <- 033:034
!    cpshd(HDR_OFFSET) = segyhd%offset                ! 6 <- 037:040
!    cpshd(HDR_MIDPOINT_XGRID) = segyhd%cdp           ! 7 <- 021:024
!    cpshd(HDR_MIDPOINT_YGRID) = segyhd%unused1       ! 8 <- 237:240
!    cpshd(HDR_ORIGINAL_GROUP) = segyhd%fldr          ! 9 <- 009:012
!    cpshd(HDR_ORIGINAL_CHANNEL) = segyhd%tracf       !10 <- 013:016
!    cpshd(HDR_SOURCE_XLOC) = fxscale*segyhd%sx       !11 <- 073:076
!    cpshd(HDR_SOURCE_YLOC) = fxscale*segyhd%sy       !12 <- 077:080
!    cpshd(HDR_SOURCE_ELEV) = fescale*segyhd%selev    !13 <- 045:048
!    cpshd(HDR_RECEIVER_XLOC) = fxscale*segyhd%gx     !14 <- 081:084
!    cpshd(HDR_RECEIVER_YLOC) = fxscale*segyhd%gy     !15 <- 085:088
!    cpshd(HDR_RECEIVER_ELEV) = fescale*segyhd%gelev  !16 <- 041:044
!    cpshd(HDR_MIDPOINT_XLOC) = .5*  &
!                     ( cpshd(HDR_SOURCE_XLOC) + &
!                   cpshd(HDR_RECEIVER_XLOC) )        !17 <-         derived
!    cpshd(HDR_MIDPOINT_YLOC) = .5*  &
!                     ( cpshd(HDR_SOURCE_YLOC) + &
!                   cpshd(HDR_RECEIVER_YLOC) )        !18 <-         derived
!    cpshd(HDR_MIDPOINT_ELEV) = .5*  &
!                     ( cpshd(HDR_SOURCE_ELEV) + &
!                   cpshd(HDR_RECEIVER_ELEV) )        !19 <-         derived
!    cpshd(HDR_SOURCE_DEPTH) = fescale*segyhd%sdepth  !20 <- 049:052
!    cpshd(HDR_SOURCE_SHOTPOINT) = segyhd%ep          !29 <- 017:020
!    if ((segyhd%trwf > 0) .and. (segyhd%trwf < 32)) then
!      cpshd(HDR_SCRATCH_30) = 2.0**(-segyhd%trwf)    !30 <- 169:170 derived
!    else
!      cpshd(HDR_SCRATCH_30) = 1.0
!    endif
!    cpshd(HDR_MIDPOINT_SHOTPOINT) =  segyhd%cdp      !37 <- 021:024
!    cpshd(HDR_MIDPOINT_LINE     ) =  segyhd%cdpt     !38 <- 025:028
!    cpshd(HDR_PRE) =  segyhd%sstat                   !39 <- 099:100
!    cpshd(HDR_POST) = segyhd%gstat                   !40 <- 101:102
!    cpshd(HDR_CUM_RESID_STATIC) = segyhd%tstat       !43 <- 103:104
!    cpshd(HDR_SOURCE_UPTIME) = segyhd%sut            !44 <- 095:096
!    cpshd(HDR_RECEIVER_UPTIME) = segyhd%gut          !45 <- 097:098
!!   calculate trace scaling.
!    cpshd(HDR_BOTTOM_MUTE) = bottom_mute             !64 <- 113:114 derived
!    ! For downward compatibility with spws applications.  ehs -- 16may01
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
!--------------------Data Structures---------------------
!      type(segy_ebcdic_hdr)
!        character(len=80) :: h(40)
!      end type segy_ebcdic_hdr
!
!      type :: segy_bin_hdr
!      sequence
!        integer                      :: jobid
!        integer                      :: lino
!        integer                      :: reno
!        integer(kind=2)              :: ntrpr
!        integer(kind=2)              :: nart
!        integer(kind=2)              :: hdt ! usec
!        integer(kind=2)              :: dto ! usec
!        integer(kind=2)              :: hns
!        integer(kind=2)              :: nso
!        integer(kind=2)              :: format  !     1 = float 4byte
!!                                                     2 = int   4byte
!!                                                     3 = int   2byte
!!                                                     4 = int w/gain code 4byte
!!                                                     5 = float 4byte IEEE Landmark "special"
!!                                                     6 = int   1byte Landmark
!        integer(kind=2)              :: fold     ! expected # trc/CDP
!        integer(kind=2)              :: tsort
!!                                       1 = none (as recorded)
!!                                       2 = CDP ensemble
!!                                       3 = single fold continuous profile
!!                                       4 = horizontally stacked
!        integer(kind=2)              :: vscode
!!                                       1 = no sum
!!                                       2 = two sum
!!                                       ...
!!                                       N = N-sum  (up to 32,767)
!        integer(kind=2)              :: hsfs
!        integer(kind=2)              :: hsfe
!        integer(kind=2)              :: hslen
!        integer(kind=2)              :: hstyp
!!                                       1 = linear
!!                                       2 = parabolic
!!                                       3 = exponential
!!                                       4 = other
!        integer(kind=2)              :: schn
!        integer(kind=2)              :: hstas
!        integer(kind=2)              :: hstae
!        integer(kind=2)              :: htatyp
!!                                       1 = linear, 2 = cos**2, 3=other
!        integer(kind=2)              :: hcorr ! 1=no 2=yes
!        integer(kind=2)              :: bgrcv ! 1=yes, 2=no
!        integer(kind=2)              :: rcvm
!!                                       1 = none
!!                                       2 = spherical divergence
!!                                       3 = AGC
!!                                       4 = other
!        integer(kind=2)              :: mfeet ! 1=meters 2=feet
!        integer(kind=2)              :: polyt
!!                                      1 = increase pressure or upward mov=neg.
!!                                      2 = increase pressure or upward mov=pos.
!        integer(kind=2)              :: vpol
!!                                       1 = 337.5 to  22.5 degrees
!!                                       2 =  22.5 to  67.5
!!                                       3 =  67.5 to 112.5
!!                                       4 = 112.5 to 157.5
!!                                       5 = 157.5 to 202.5
!!                                       6 = 202.5 to 247.5
!!                                       7 = 247.5 to 292.5
!!                                       8 = 292.5 to 337.5
!!       First 60 bytes defined by seg-y standard above          !   1- 60
!        character(len=340)           :: hunass                  !
!      end type segy_bin_hdr
!
!      type :: segy_trc_hdr
!      sequence               !12345678901234567890  bytepos wrd#
!        integer(kind=4)    :: tracl               ! 001-004  1
!        integer(kind=4)    :: tracr               ! 005-008  2
!        integer(kind=4)    :: fldr                ! 009-012  3
!        integer(kind=4)    :: tracf               ! 013-016  4
!        integer(kind=4)    :: ep                  ! 017-020  5
!        integer(kind=4)    :: cdp                 ! 021-024  6
!        integer(kind=4)    :: cdpt                ! 025-028  7
!        integer(kind=2)    :: trid                ! 029-030  8.1
!!                             1 = seismic data
!!                             2 = dead
!!                             3 = dummy
!!                             4 = time break
!!                             5 = uphole
!!                             6 = sweep
!!                             7 = timing
!!                             8 = water break
!!                             9+= Optional use (max = 32767)
!        integer(kind=2)    :: nvs                 ! 031-032 8.2
!        integer(kind=2)    :: nhs                 ! 033-034 9.1
!        integer(kind=2)    :: duse                ! 035-036 9.2
!!                             1 = production
!!                             2 = test
!        integer(kind=4)    :: offset              ! 037-040 10
!        integer(kind=4)    :: gelev               ! 041-044 11
!        integer(kind=4)    :: selev               ! 045-048 12 (pos up)
!        integer(kind=4)    :: sdepth              ! 049-052 13 (pos down)
!        integer(kind=4)    :: gdel                ! 053-056 14 (pos up)
!        integer(kind=4)    :: sdel                ! 057-060 15 (pos up)
!        integer(kind=4)    :: swdep               ! 061-064 16 (pos down)
!        integer(kind=4)    :: gwdep               ! 065-068 17 (pos down)
!        integer(kind=2)    :: scalel              ! 069-070 18.1
!!                             1,+/-10,+/-100,+/-1000, +/-10000. If pos,
!!                             use as multiplier, if neg, as divisor.
!!                             Apply to bytes 41-68 (words 11-17).
!        integer(kind=2)    :: scalco              ! 071-072 18.2
!!                             1,+/-10,+/-100,+/-1000, +/-10000. If pos,
!!                             use as multiplier, if neg, as divisor.
!!                             Apply to bytes 73-88 (words 19-22).
!        integer(kind=4)    :: sx                  ! 073-076 19
!        integer(kind=4)    :: sy                  ! 077-080 20
!        integer(kind=4)    :: gx                  ! 081-084 21
!        integer(kind=4)    :: gy                  ! 085-088 22
!        integer(kind=2)    :: counit              ! 089-090 23.1
!!                             1=length(meters or feet) 2=seconds of arc.
!        integer(kind=2)    :: wevel               ! 091-092 23.2
!        integer(kind=2)    :: swevel              ! 093-094 24.1
!        integer(kind=2)    :: sut                 ! 095-096 24.2
!        integer(kind=2)    :: gut                 ! 097-098 25.1
!        integer(kind=2)    :: sstat               ! 099-100 25.2
!        integer(kind=2)    :: gstat               ! 101-102 26.1
!        integer(kind=2)    :: tstat               ! 103-104 26.2
!        integer(kind=2)    :: laga                ! 105-106 27.1
!        integer(kind=2)    :: lagb                ! 107-108 27.2
!        integer(kind=2)    :: delrt               ! 109-110 28.1
!        integer(kind=2)    :: muts                ! 111-112 28.2
!        integer(kind=2)    :: mute                ! 113-114 29.1
!        integer(kind=2)    :: ns                  ! 115-116 29.2
!        integer(kind=2)    :: dt                  ! 117-118 30.1
!        integer(kind=2)    :: gain                ! 119-120 30.2
!!                            1 = fixed 2 = binary 3 = floating pt 4+ = optional
!        integer(kind=2)    :: igc                 ! 121-122 31.1
!        integer(kind=2)    :: igi                 ! 123-124 31.2
!        integer(kind=2)    :: corr                ! 125-126 32.1
!!                             1 = no 2 = yes
!        integer(kind=2)    :: sfs                 ! 127-128 32.2
!        integer(kind=2)    :: sfe                 ! 129-130 33.1
!        integer(kind=2)    :: slen                ! 131-132 33.2
!        integer(kind=2)    :: styp                ! 133-134 34.1
!!                             1 = linear
!!                             2 = parabolic
!!                             3 = exponential
!!                             4 = other
!        integer(kind=2)    :: stas                ! 135-136 34.2 (ms)
!        integer(kind=2)    :: stae                ! 137-138 35.1 (ms)
!        integer(kind=2)    :: tatyp               ! 139-140 35.2
!!                             1 = linear
!!                             2 = cos**2
!!                             3 = other
!        integer(kind=2)    :: afilf               ! 141-142 36.1
!        integer(kind=2)    :: afils               ! 143-144 36.2
!        integer(kind=2)    :: nofilf              ! 145-146 37.1
!        integer(kind=2)    :: nofils              ! 147-148 37.2
!        integer(kind=2)    :: lcf                 ! 149-150 38.1
!        integer(kind=2)    :: hcf                 ! 151-152 38.2
!        integer(kind=2)    :: lcs                 ! 153-154 39.1
!        integer(kind=2)    :: hcs                 ! 155-156 39.2
!        integer(kind=2)    :: year                ! 157-158 40.1
!        integer(kind=2)    :: day                 ! 159-160 40.2
!        integer(kind=2)    :: hour                ! 161-162 41.1
!        integer(kind=2)    :: minute              ! 163-164 41.2
!        integer(kind=2)    :: sec                 ! 165-166 42.1
!        integer(kind=2)    :: timbas              ! 167-168 42.2
!!                             1 = local 2 = GMT 3 = other
!        integer(kind=2)    :: trwf                ! 169-170 43.1
!!                             n**(-N) volts for least sig. bit (N=0,1,..32767)
!        integer(kind=2)    :: grnors              ! 171-172 43.2
!        integer(kind=2)    :: grnofr              ! 173-174 44.1
!        integer(kind=2)    :: grnlof              ! 175-176 44.2
!        integer(kind=2)    :: gaps                ! 177-178 45.1
!        integer(kind=2)    :: otrav               ! 179-180 45.2
!        integer(kind=4)    :: unused1             ! 237-240 60
!      end type segy_trc_hdr
!
!      -------------------CALLING SEQUENCE-------------------------
!
!        o                      i    o
!      status = segy_binh2buf(binhdr,h)
!        Purpose: Convert segy_binary_header structure to integer buffer
!        type(segy_bin_hdr)       :: binhdr
!        integer, dimension(100)  :: h
!
!        o                    i    o    i(opt)        o(opt)
!      status = segy_buf2binh(h,binhdr,optional_swap,endian)
!        Purpose: Convert integer buffer to segy_binary_header structure
!        integer, dimension(100)  :: h
!        type(segy_bin_hdr)       :: binhdr
!        logical                  :: optional_swap
!        integer,optional         :: endian
!
!                                 i    o   i(opt) o(opt)
!      status = segy_read_binhdr(unit,bhed,swap,endian)
!        Purpose: Read the binary header (400 bytes)
!        integer :: status 0=ok
!        type(segy_bin_hdr) :: bhed
!        integer :: unit number (from trcio or cio or cpsio)
!        logical :: swap(set to true if endian of file is not native to machine
!        integer,optional         :: endian
!
!        o                         i    i
!      status = segy_write_binhdr(unit,bhed)
!        Purpose:  Write the binary header.
!        integer :: status 0=ok
!        type(segy_bin_hdr) :: bhed
!        integer :: unit (from cio,cpsio,trcio)
!
!        o                         i    o
!      status = segy_read_ebchdr(unit,ascii)
!        Purpose: Read the file's ebcdic header and load it into the ascii
!                 string array in ASCII format (so you can read it!)
!        integer :: unit (from cio,cpsio,trcio)
!        type(segy_ebcdic_hdr) :: ascii
!        integer :: status (0 = ok)
!
!        o                          i    i
!      status = segy_write_ebchdr(unit,ascii)
!        Purpose: Take your ascii formatted string array and convert to EBCDIC
!                 then write it to the file.
!        type(segy_ebcdic_hdr) :: ascii
!        integer :: status (0 = ok)
!        integer :: unit (from cio, cpsio, trcio)
!
!                              b
!      call segy_init_ebchdr(sehdr)
!        Purpose: Initialize contents of ebcdic head to standard with blank
!                 entries.
!        type(segy_ebcdic_hdr) :: ascii
!
!                                   b      i       i
!      call segy_set_field_ebchdr(sehdr, field, contents)
!        Purpose: Set contents of ebcdic field identified by string
!                 "field" to "contents".
!        type(segy_ebcdic_hdr) :: ascii
!        character(len=*)      :: field
!        character(len=*)      :: contents
!
!                             i  o  i(opt)
!      call segy_pack_segyhd(syh,h,swap)
!        Purpose: Pack the segy trace header into a 60 word array "h".
!        integer :: h(60)
!        logical :: swap
!        type(segy_trc_hdr) :: syh
!
!                               o  i  i(opt)
!      call segy_unpack_segyhd(syh,h,swap)
!        Purpose: Unpack the segy trace header from a 60 word array "h".
!        integer :: h(60)
!        logical :: swap
!        type(segy_trc_hdr) :: syh
!
!                                 i      o     i      i i
!      call segy_cpshd_to_segyhd(cpshd,segyhd,tstart,dt,ns)
!        Purpose: Convert cps header into segy header.
!        double precision :: cpshd(64)
!        type(segy_trc_hdr):: segyhd (segy header structure)
!        double precision  :: tstart ,dt (start time for trace, samp rate (sec))
!        integer           :: ns (number of samples per trace)
!
!                                 o      i      i
!      call segy_segyhd_to_cpshd(cpshd,segyhd,tstart)
!        Purpose: Convert segy header into cps header.
!        double precision :: cpshd(64)
!        type(segy_trc_hdr) :: segyhd (segy header structure)
!        double precision   :: tstart (start time for trace)
!
!                                 i       i
!      call segy_print_trace_hd(segyhd, lun_in)
!         type(segy_trc_hdr),intent(in) :: segyhd
!         integer,intent(in), optional  :: lun_in
!         Purpose: print non-zero values from segy trace header (240 byte).
!
!                               i       i
!      call segy_print_binhdr(binhdr, lun_in)
!         type(segy_bin_hdr)           :: binhdr
!         integer,intent(in), optional :: lun_in
!         Purpose: print non-zero values from binary (400 byte) header.
!
!                         i                     i     i
!      call segy_tabplot(trace_start_address,istart,iend)
!         Purpose:  To plot values graphically to terminal
!         real :: trace_start_address !! like trace(1) or array(1,1)
!         integer :: istart,iend (istart = 1 for first element)
!         OUTPUT: Ascii text <= 80 characters wide.  One line per sample.
!
!
!  - custom map from segy to cps headers or vice versa
!                            i    i      i      i      i       i       o
! call segy_map_segy_to_cps(segy,nummap,nbytes, sbyte, cpsmap, wtype, cpshd)
!
!                           o    i      i      i     i      i     i
! call segy_map_cps_to_segy(segy,nummap,nbytes,sbyte,cpsmap,wtype,cpshd)
!
!  integer*4(60) segy  .. the 240 byte segy header (60 word integer*4 array)
!  integer*4     nummap.. the number of words to custom map to/from cps headers
!  integer*4     nbytes.. array with number of bytes for ith segy word
!  integer*4     sbyte... byte location of the ith segy word(from 1)
!  integer*4     wtype... word type of the ith segy word(0=integer, 1=float)
!  integer*4     cpsmap.. map ith custom word to cpsmap[i] in cpshd(from 1)
!  double prec   cpshd... the double precision cps header array
!
!
!                      i  i(opt)
! call segy_hdrdump(flush,syh)
!
! segy_trc_header  syh .. segy header array created in segy_unpack_segyhd
! logical          flush  Set to true after all traces read to flush last buf
!

!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             _
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 21. 2007-01-25  Bill Menger  cleaned up endian swap if block.
! 20. 2006-04-25  B. Menger    Removed Unused Variables.
! 19. 2004-08-23  R Selzler    Improved segy recognition of format variations.
! 18. 2004-01-21  Goodger      Add routine segy_hdrdump.
! 17. 2003-08-18  Bill Done    Modify struct segy_trc_hdr to account for the
!                              unused portion of the segy trace header from
!                              bytes 181-236. Modify the pack and unpack
!                              routines segy_pack_segyhd and segy_unpack_segyhd
!                              to handle these items. Modify subroutine
!                              segy_cpshd_to_segyhd to initialize these items
!                              to zero and segy_print_trace_hd to print them.
!                              Add new subroutine segy_init_ebchdr to initialize
!                              the ebcdic header to standard content with no
!                              values set and subroutine segy_set_field_ebchdr
!                              to set the value of a specified ebcdic header
!                              field.
! 16. 2002-08-26  K. Goodger   Always set the bottom mute to the number of
!                              samples in segy_segyhd_to_cpshd.  There is no
!                              equivalent to cps header word 64 in the segy
!                              header.
! 15. 2001-12-10  Ed Schmauch  Removed all Conoco extensions to segy format.
!                              Modified segy_segyhd_to_cpshd so that if
!                              bottom mute is equal to top mute, the bottom mute
!                              is reset to the number of samples.
!                              Removed illegal use of scratch and user headers.
!                              Replaced illegal use of HDR_USER_54 with legal
!                              use of HDR_SCRATCH_30.
!                              Used nint to round all segy headers.
! 14. 2001-06-18  Ed Schmauch  Non-Conoco segys will get y grid from segy
!                              header 237-240.  This is for downward
!                              compatibility of spws applications.
!                              PRODUCTION.
! 13. 2001-05-14  Ed Schmauch  Doc change to add Landmark segy to
!                              segy_bin_hd%format.  Fixed bug in calculation of
!                              cpshd(HDR_USER_54) from segyhd%trwf in
!                              segy_segyhd_to_cpshd.  Eliminated an illegal
!                              statement that I can't list here or checkc
!                              will jump on it.
! 12. 2001-03-21  K. Goodger   Modified "is_file_segy" by counting the number
!                              of c's in the header.  If there are at least 5,
!                              assume segy.
!                              Fixed ending header_word_doc tag.
! 11. 2001-02-13  Bill Menger  Modified "is_file_segy" by allowing lowercase
!                              "c" on ebcdic header first character. (non-std.)
! 10. 2000-11-13  Bill Menger  Added interface to segy_map_functions.
!  9. 2000-07-18  Bill Menger  Removed gain function from hdr word 54.
!  8. 2000-07-11  Bill Menger  Modified so that we always write segy with
!                              "big-endian"
!  7. 2000-05-30  Bill Menger  Added binh2buf, buf2binh, new 'is it segy' test.
!  6. 2000-03-08  Bill Menger  Added tabplot routine,print_binhdr routine, and
!                              print_trace_hd routine.
!  5. 2000-01-21  Bill Menger  Modified ebcdic read/write to handle chr(10).
!                              Fixed some automatic swapping parameters to not
!                              automatically swap just because format > 4.
!  4. 2000-01-18  Bill Menger  Added nbits to binhead, gain_for_recovery to
!                              segyhead.  Still Need cps header word for gain.
!  3. 2000-01-11  Bill Menger  Fixed it to do IBM-FLT on Linux
!  2. 2000-01-07  Bill Menger  Took out the trwf/user_54 conversion code
!  1. 1999-12-09  Bill Menger  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
! This was not written with 8-byte machines in mind.
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!-------------------------------------------------------------------------------
!</programming_doc>

module segy_module
use sizeof_module
use wrdc_module
use named_constants_module
use swap_module
use cio_module
implicit none
private
!---   Data Structures

public :: segy_ebcdic_hdr     , segy_bin_hdr, segy_trc_hdr

!---   Routines

private :: segy_swaptapehval  , segy_swaptapebhval
public :: segy_read_binhdr    , segy_write_binhdr
public :: segy_read_ebchdr    , segy_write_ebchdr
public :: segy_init_ebchdr    , segy_set_field_ebchdr
public :: segy_pack_segyhd    , segy_unpack_segyhd
public :: segy_cpshd_to_segyhd, segy_segyhd_to_cpshd
public :: segy_print_trace_hd , segy_print_binhdr, segy_tabplot
public :: segy_binh2buf,        segy_buf2binh
public :: segy_map_segy_to_cps, segy_map_cps_to_segy
public :: segy_hdrdump
public :: segy_is_file_segy


character(len=100),save,public :: segy_ident = &
'$Id: segy.f90,v 1.139 2008/02/15 20:25:53 mengewm Exp $'
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


type :: segy_ebcdic_hdr
sequence
character(len=80)            :: h(40)
end type segy_ebcdic_hdr


type :: segy_bin_hdr
sequence
integer                      :: jobid
integer                      :: lino
integer                      :: reno
integer(kind=2)              :: ntrpr
integer(kind=2)              :: nart
integer(kind=2)              :: hdt ! usec
integer(kind=2)              :: dto ! usec
integer(kind=2)              :: hns
integer(kind=2)              :: nso
integer(kind=2)              :: format  !      1 = float 4byte
!                                                      2 = int   4byte
!                                                      3 = int   2byte
!                                                      4 = int w/gain code 4byte
!                                                      5 = float 4byte Landmark IEEE "special"
!                                                      6 = int   1byte Landmark
integer(kind=2)              :: fold     ! expected # trc/CDP
integer(kind=2)              :: tsort
!                                       1 = none (as recorded)
!                                       2 = CDP ensemble
!                                       3 = single fold continuous profile
!                                       4 = horizontally stacked
integer(kind=2)              :: vscode
!                                       1 = no sum
!                                       2 = two sum
!                                       ...
!                                       N = N-sum  (up to 32,767)
integer(kind=2)              :: hsfs
integer(kind=2)              :: hsfe
integer(kind=2)              :: hslen
integer(kind=2)              :: hstyp
!                                       1 = linear
!                                       2 = parabolic
!                                       3 = exponential
!                                       4 = other
integer(kind=2)              :: schn
integer(kind=2)              :: hstas
integer(kind=2)              :: hstae
integer(kind=2)              :: htatyp
!                                       1 = linear, 2 = cos**2, 3=other
integer(kind=2)              :: hcorr ! 1=no 2=yes
integer(kind=2)              :: bgrcv ! 1=yes, 2=no
integer(kind=2)              :: rcvm
!                                       1 = none
!                                       2 = spherical divergence
!                                       3 = AGC
!                                       4 = other
integer(kind=2)              :: mfeet ! 1=meters 2=feet
integer(kind=2)              :: polyt
!                                       1 = increase pressure or upward mov=neg.
!                                       2 = increase pressure or upward mov=pos.
integer(kind=2)              :: vpol
!                                       1 = 337.5 to  22.5 degrees
!                                       2 =  22.5 to  67.5
!                                       3 =  67.5 to 112.5
!                                       4 = 112.5 to 157.5
!                                       5 = 157.5 to 202.5
!                                       6 = 202.5 to 247.5
!                                       7 = 247.5 to 292.5
!                                       8 = 292.5 to 337.5
!       First 60 bytes defined by seg-y standard above          !   1- 60
!
! No more conoco segy format.
! ehs 02nov01
!
!       character(len= 8)            :: conid                   !  61- 68
!       real                         :: tstart                  !
!       double precision             :: xorigin                 !
!       double precision             :: yorigin                 !
!       double precision             :: dx11                    !
!       double precision             :: dx12                    !
!       double precision             :: dx21                    !
!       double precision             :: dx22                    !
!       integer(kind=4)              :: endian                  !
!!                                      1 = big, 0 = little
!       integer(kind=4)              :: nbits
character(len=340)           :: hunass                  !
end type segy_bin_hdr

type :: segy_trc_hdr
sequence               !12345678901234567890  bytepos wrd#
integer(kind=4)    :: tracl               ! 001-004  1
integer(kind=4)    :: tracr               ! 005-008  2
integer(kind=4)    :: fldr                ! 009-012  3
integer(kind=4)    :: tracf               ! 013-016  4
integer(kind=4)    :: ep                  ! 017-020  5
integer(kind=4)    :: cdp                 ! 021-024  6
integer(kind=4)    :: cdpt                ! 025-028  7
integer(kind=2)    :: trid                ! 029-030  8.1
!                             1 = seismic data
!                             2 = dead
!                             3 = dummy
!                             4 = time break
!                             5 = uphole
!                             6 = sweep
!                             7 = timing
!                             8 = water break
!                             9+= Optional use (max = 32767)
integer(kind=2)    :: nvs                 ! 031-032 8.2
integer(kind=2)    :: nhs                 ! 033-034 9.1
integer(kind=2)    :: duse                ! 035-036 9.2
!                             1 = production
!                             2 = test
integer(kind=4)    :: offset              ! 037-040 10
integer(kind=4)    :: gelev               ! 041-044 11
integer(kind=4)    :: selev               ! 045-048 12 (pos up)
integer(kind=4)    :: sdepth              ! 049-052 13 (pos down)
integer(kind=4)    :: gdel                ! 053-056 14 (pos up)
integer(kind=4)    :: sdel                ! 057-060 15 (pos up)
integer(kind=4)    :: swdep               ! 061-064 16 (pos down)
integer(kind=4)    :: gwdep               ! 065-068 17 (pos down)
integer(kind=2)    :: scalel              ! 069-070 18.1
!                             1,+/-10,+/-100,+/-1000, +/-10000. If pos,
!                             use as multiplier, if neg, as divisor.
!                             Apply to bytes 41-68 (words 11-17).
integer(kind=2)    :: scalco              ! 071-072 18.2
!                             1,+/-10,+/-100,+/-1000, +/-10000. If pos,
!                             use as multiplier, if neg, as divisor.
!                             Apply to bytes 73-88 (words 19-22).
integer(kind=4)    :: sx                  ! 073-076 19
integer(kind=4)    :: sy                  ! 077-080 20
integer(kind=4)    :: gx                  ! 081-084 21
integer(kind=4)    :: gy                  ! 085-088 22
integer(kind=2)    :: counit              ! 089-090 23.1
!                             1=length(meters or feet) 2=seconds of arc.
integer(kind=2)    :: wevel               ! 091-092 23.2
integer(kind=2)    :: swevel              ! 093-094 24.1
integer(kind=2)    :: sut                 ! 095-096 24.2
integer(kind=2)    :: gut                 ! 097-098 25.1
integer(kind=2)    :: sstat               ! 099-100 25.2
integer(kind=2)    :: gstat               ! 101-102 26.1
integer(kind=2)    :: tstat               ! 103-104 26.2
integer(kind=2)    :: laga                ! 105-106 27.1
integer(kind=2)    :: lagb                ! 107-108 27.2
integer(kind=2)    :: delrt               ! 109-110 28.1
integer(kind=2)    :: muts                ! 111-112 28.2
integer(kind=2)    :: mute                ! 113-114 29.1
integer(kind=2)    :: ns                  ! 115-116 29.2
integer(kind=2)    :: dt                  ! 117-118 30.1
integer(kind=2)    :: gain                ! 119-120 30.2
!                             1 = fixed 2 = binary 3 = floating pt 4+ = optional
integer(kind=2)    :: igc                 ! 121-122 31.1
integer(kind=2)    :: igi                 ! 123-124 31.2
integer(kind=2)    :: corr                ! 125-126 32.1
!                             1 = no 2 = yes
integer(kind=2)    :: sfs                 ! 127-128 32.2
integer(kind=2)    :: sfe                 ! 129-130 33.1
integer(kind=2)    :: slen                ! 131-132 33.2
integer(kind=2)    :: styp                ! 133-134 34.1
!                             1 = linear
!                             2 = parabolic
!                             3 = exponential
!                             4 = other
integer(kind=2)    :: stas                ! 135-136 34.2 (ms)
integer(kind=2)    :: stae                ! 137-138 35.1 (ms)
integer(kind=2)    :: tatyp               ! 139-140 35.2
!                             1 = linear
!                             2 = cos**2
!                             3 = other
integer(kind=2)    :: afilf               ! 141-142 36.1
integer(kind=2)    :: afils               ! 143-144 36.2
integer(kind=2)    :: nofilf              ! 145-146 37.1
integer(kind=2)    :: nofils              ! 147-148 37.2
integer(kind=2)    :: lcf                 ! 149-150 38.1
integer(kind=2)    :: hcf                 ! 151-152 38.2
integer(kind=2)    :: lcs                 ! 153-154 39.1
integer(kind=2)    :: hcs                 ! 155-156 39.2
integer(kind=2)    :: year                ! 157-158 40.1
integer(kind=2)    :: day                 ! 159-160 40.2
integer(kind=2)    :: hour                ! 161-162 41.1
integer(kind=2)    :: minute              ! 163-164 41.2
integer(kind=2)    :: sec                 ! 165-166 42.1
integer(kind=2)    :: timbas              ! 167-168 42.2
!                             1 = local 2 = GMT 3 = other
integer(kind=2)    :: trwf                ! 169-170 43.1
!                             n**(-N) volts for least sig. bit (N=0,1,..32767)
integer(kind=2)    :: grnors              ! 171-172 43.2
integer(kind=2)    :: grnofr              ! 173-174 44.1
integer(kind=2)    :: grnlof              ! 175-176 44.2
integer(kind=2)    :: gaps                ! 177-178 45.1
integer(kind=2)    :: otrav               ! 179-180 45.2
!
! No more conoco segy format.
! ehs 02nov01
!
!       character(len=8)   :: conid               ! 181-188 46-47
!       real               :: lav                 ! 189-192 48
!       double precision   :: mp_xloc             ! 193-200 49-50
!       double precision   :: mp_yloc             ! 201-208 51-52
!       double precision   :: mp_elev             ! 209-216 53-54
!       double precision   :: mp_xgrid            ! 217-224 55-56
!       double precision   :: mp_ygrid            ! 225-232 57-58
!       real               :: gain_for_recovery   ! 233-236 59
!
!
! Introduce place holders for segy trace header elements 46-59
! wjd 2003/8/11
integer(kind=4)    :: unused(14)          ! 181-236 46-59
!
integer(kind=4)    :: unused1             ! 237-240 60
end type segy_trc_hdr


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

interface
subroutine segy_map_segy_to_cps_c &
  (segyhd,nummap,nbytes,sbyte,cpsmap,wtype,cpshd)
  integer,intent(in )                       :: segyhd
  integer,intent(in )                       :: nummap
  integer,intent(in )                       :: nbytes
  integer,intent(in )                       :: sbyte
  integer,intent(in )                       :: cpsmap
  integer,intent(in )                       :: wtype
  double precision,intent(out)              :: cpshd
end subroutine segy_map_segy_to_cps_c
end interface

interface
subroutine segy_map_cps_to_segy_c &
  (segyhd,nummap,nbytes,sbyte,cpsmap,wtype,cpshd)
  integer,intent(out)                       :: segyhd
  integer,intent(in )                       :: nummap
  integer,intent(in )                       :: nbytes
  integer,intent(in )                       :: sbyte
  integer,intent(in )                       :: cpsmap
  integer,intent(in )                       :: wtype
  double precision,intent(in )              :: cpshd
end subroutine segy_map_cps_to_segy_c
end interface

interface
subroutine segy_tabplot(trace,istart,iend)
  real, intent(in)                           :: trace
  integer,intent(in)                         :: istart,iend
end subroutine segy_tabplot
end interface

interface
subroutine segy_swaptapebhval(bhead,idx)
  integer,intent(inout)                      :: bhead
  integer,intent(in)                         :: idx
end subroutine segy_swaptapebhval
end interface

interface
subroutine segy_swaptapehval(head,idx)
  integer,intent(inout)                      :: head
  integer,intent(in)                         :: idx
end subroutine segy_swaptapehval
end interface

interface segy_gettapebhval

subroutine segy_gettapebhval_b(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  integer(kind=1),intent(out)             :: valp
end subroutine segy_gettapebhval_b

subroutine segy_gettapebhval_u(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  integer(kind=2),intent(out)             :: valp
end subroutine segy_gettapebhval_u

subroutine segy_gettapebhval_p(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  integer(kind=4),intent(out)             :: valp
end subroutine segy_gettapebhval_p

subroutine segy_gettapebhval_f(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  real           ,intent(out)             :: valp
end subroutine segy_gettapebhval_f

subroutine segy_gettapebhval_d(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  double precision, intent(out)           :: valp
end subroutine segy_gettapebhval_d

subroutine segy_gettapebhval_c(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  character(len=8),intent(out)            :: valp
end subroutine segy_gettapebhval_c

end interface

interface segy_puttapebhval

subroutine segy_puttapebhval_b(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  integer(kind=1),intent(in )             :: valp
end subroutine segy_puttapebhval_b

subroutine segy_puttapebhval_u(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  integer(kind=2),intent(in )             :: valp
end subroutine segy_puttapebhval_u

subroutine segy_puttapebhval_p(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  integer(kind=4),intent(in )             :: valp
end subroutine segy_puttapebhval_p

subroutine segy_puttapebhval_f(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  real           ,intent(in )             :: valp
end subroutine segy_puttapebhval_f

subroutine segy_puttapebhval_d(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  double precision, intent(in )           :: valp
end subroutine segy_puttapebhval_d

subroutine segy_puttapebhval_c(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  character(len=8),intent(in )            :: valp
end subroutine segy_puttapebhval_c

end interface


interface segy_gettapehval

subroutine segy_gettapehval_u(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  integer(kind=2),intent(out)             :: valp
end subroutine segy_gettapehval_u

subroutine segy_gettapehval_p(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  integer(kind=4),intent(out)             :: valp
end subroutine segy_gettapehval_p

subroutine segy_gettapehval_f(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  real           ,intent(out)             :: valp
end subroutine segy_gettapehval_f

subroutine segy_gettapehval_d(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  double precision, intent(out)           :: valp
end subroutine segy_gettapehval_d

subroutine segy_gettapehval_c(bhd, index,valp)
  integer , intent(in)                    :: bhd
  integer , intent(in)                    :: index
  character(len=8),intent(out)            :: valp
end subroutine segy_gettapehval_c

end interface

interface segy_puttapehval

subroutine segy_puttapehval_u(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  integer(kind=2),intent(in )             :: valp
end subroutine segy_puttapehval_u

subroutine segy_puttapehval_p(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  integer(kind=4),intent(in )             :: valp
end subroutine segy_puttapehval_p

subroutine segy_puttapehval_f(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  real           ,intent(in )             :: valp
end subroutine segy_puttapehval_f

subroutine segy_puttapehval_d(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  double precision, intent(in )           :: valp
end subroutine segy_puttapehval_d

subroutine segy_puttapehval_c(bhd, index,valp )
  integer , intent(inout)                 :: bhd
  integer , intent(in)                    :: index
  character(len=8),intent(in )            :: valp
end subroutine segy_puttapehval_c

end interface

interface segy_is_file_segy
  integer function segy_is_file_segy_c(unit) result(status)
  integer, intent(in)                     :: unit
  end function segy_is_file_segy_c
end interface

contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!


subroutine segy_buf2asc_hdr(buffer,ascii)
integer,intent(in),dimension(:)    :: buffer(800)
type(segy_ebcdic_hdr),intent(out)  :: ascii
!--- local ------------------------------------------------------
integer                            :: i
!-----------------------------------------------------------------
do i = 1, 40
ascii%h(i)(1:80) = transfer(buffer(1+20*(i-1):20*i),ascii%h(1))
call wrdc_ebc_asc(ascii%h(i))
end do
end subroutine segy_buf2asc_hdr

function segy_read_binhdr(unit,binhdr,optional_swap,endian) result(status)
integer,intent(in)               :: unit
type(segy_bin_hdr),intent(out)   :: binhdr
integer                          :: status
logical,optional                 :: optional_swap
integer,optional                 :: endian
!--- LOCAL VARIABLES ---
integer                          :: h(0:99)    


!--- read the entire binary header in one swell foop.
!--- The bin header will be in the "h" array, and using "C" code
!--- we extract each word.

status = cio_fread(h,400,1,unit) -1
if(status /= 0 ) return
if(present(optional_swap)) then
status = segy_buf2binh(h,binhdr,optional_swap,endian)
else
status = segy_buf2binh(h,binhdr,endian=endian)
endif
end function segy_read_binhdr

function segy_buf2binh(buffer,binhdr,optional_swap,endian) result (status)
integer,intent(in)               :: buffer(0:99)
type(segy_bin_hdr),intent(out)   :: binhdr
integer                          :: status
logical,optional                 :: optional_swap
integer,optional                 :: endian
!--- LOCAL VARIABLES ---
integer                          :: i,hshorts(3:26),h(0:99)
logical                          :: swap,swapset,triedswap,putswapback

if(present(optional_swap) ) then
  swap = optional_swap
  swapset = .true.
else
  swap = .false.
  swapset = .false.
endif
triedswap = .false.
putswapback = .false.
h = buffer

100 continue

!--- "h(0)" is now the address of the binary header.
!--- The second argument is the byte offset of the corresponding word.
!--- Check to see if the "conoco id" is in the binary header.
!
! No more conoco segy format.
! ehs 01nov01
!
!   call segy_gettapebhval(h(0), 27,binhdr%conid   )
!   if(binhdr%conid == '*CONOCO*') then
!     !--- if we are lucky, this file has a conoco id which means the
!     !--- "endian" of the writing machine was recorded here and is 1.
!     if(swap_endian() /= 1) swap = .true.
!     !--- we need to swap bytes on the integers/floats that were read.
!   else
!--- figure out the swapping logical some other way.
!--- Try reading num traces per group.  If it falls outside of
!--- a reasonable range, swap bytes and try again.  If it then looks
!--- reasonable, set swap to yes.  Otherwise, wing it with no swapping.
hshorts(3) = h(3)
status = wrdc_unpack(hshorts,1,2,4)
binhdr%ntrpr=hshorts(3)
if(binhdr%ntrpr < 0 .or. binhdr%ntrpr >= 8193 ) then
  hshorts(3) = h(3)
  status = wrdc_unpack(hshorts,1,2,4,.true.)
  binhdr%ntrpr=hshorts(3)
  if(binhdr%ntrpr > 0 .and. binhdr%ntrpr < 8193 ) then
    if(.not. swapset) swap = .true.
  endif
endif
!   endif
hshorts(3:26) = h(3:26)
if (swap) then
  do i = 0, 2
    call segy_swaptapebhval(h(0),i)
  end do
  !
  ! No more conoco segy format.
  ! ehs 06nov01
  !
  !     do i = 28, 36
  !       call segy_swaptapebhval(h(0),i)
  !     end do
endif
call segy_gettapebhval(h(0), 0,binhdr%jobid )
call segy_gettapebhval(h(0), 1,binhdr%lino )
call segy_gettapebhval(h(0), 2,binhdr%reno)
status = wrdc_unpack(hshorts,24,2,4,swap)
binhdr%ntrpr        =   hshorts( 3)
binhdr%nart         =   hshorts( 4)
binhdr%hdt          =   hshorts( 5)
binhdr%dto          =   hshorts( 6)
binhdr%hns          =   hshorts( 7)
binhdr%nso          =   hshorts( 8)
binhdr%format       =   hshorts( 9)
!
! No more conoco segy format.
! ehs 01nov01
!
!   if(binhdr%conid /= '*CONOCO*') then
if(.not.triedswap) then
  !if(binhdr%hdt < 0 .or. binhdr%hns < 0 .or. binhdr%format < 0 .or. binhdr%format > 212 ) then
  if(binhdr%hns < 0 .or. binhdr%format < 0 .or. binhdr%format > 212 ) then
    ! probably have a swap problem
    triedswap = .true.
    swapset   = .true.
    swap = .not. swap
    goto 100
  endif
else
  !if(binhdr%hdt < 0 .or. binhdr%hns < 0  .or. binhdr%format < 0 .or. binhdr%format > 212 ) then
  if(binhdr%hns < 0  .or. binhdr%format < 0 .or. binhdr%format > 212 ) then
    swap = .not. swap ! put it back, who knows what is wrong?
    if(.not. putswapback ) then
      putswapback = .true.
      goto 100
    endif
  endif
endif
!   endif
binhdr%fold         =   hshorts(10)
binhdr%tsort        =   hshorts(11)
binhdr%vscode       =   hshorts(12)
binhdr%hsfs         =   hshorts(13)
binhdr%hsfe         =   hshorts(14)
binhdr%hslen        =   hshorts(15)
binhdr%hstyp        =   hshorts(16)
binhdr%schn         =   hshorts(17)
binhdr%hstas        =   hshorts(18)
binhdr%hstae        =   hshorts(19)
binhdr%htatyp       =   hshorts(20)
binhdr%hcorr        =   hshorts(21)
binhdr%bgrcv        =   hshorts(22)
binhdr%rcvm         =   hshorts(23)
binhdr%mfeet        =   hshorts(24)
binhdr%polyt        =   hshorts(25)
binhdr%vpol         =   hshorts(26)
!
! No more conoco segy format.
! ehs 01nov01
!
!   binhdr%tstart = 0.0
!   binhdr%xorigin= 0.0
!   binhdr%yorigin= 0.0
!   binhdr%dx11   = 1.0
!   binhdr%dx12   = 0.0
!   binhdr%dx21   = 0.0
!   binhdr%dx22   = 1.0
!   binhdr%nbits  = 32
!   if(binhdr%conid == '*CONOCO*') then
!     call segy_gettapebhval(h(0), 28,binhdr%tstart  )
!     call segy_gettapebhval(h(0), 29,binhdr%xorigin )
!     call segy_gettapebhval(h(0), 30,binhdr%yorigin )
!     call segy_gettapebhval(h(0), 31,binhdr%dx11    )
!     call segy_gettapebhval(h(0), 32,binhdr%dx12    )
!     call segy_gettapebhval(h(0), 33,binhdr%dx21    )
!     call segy_gettapebhval(h(0), 34,binhdr%dx22    )
!     call segy_gettapebhval(h(0), 35,binhdr%endian  )
!     call segy_gettapebhval(h(0), 36,binhdr%nbits   )
!   endif
!
if (present(endian)) then
  if(swap) then
    endian = 1 - swap_endian()
  else
    endian = swap_endian()
  endif
endif

end function segy_buf2binh

function segy_write_binhdr(unit,binhdr) result(status)
integer,intent(in)               :: unit
type(segy_bin_hdr),intent(in   ) :: binhdr
integer                          :: status
!--- local ---
integer                          :: h(100)

!--- "h" is the address of the binary header that will be written.
!--- The second argument to segy_puttapebhval is the byte offset of the
!--- corresponding word.
status = segy_binh2buf(binhdr,h)
if( status /= 0 ) return
status = cio_fwrite(h,400,1,unit) -1

end function segy_write_binhdr

function segy_binh2buf(binhdr,h) result(status)
integer,intent(out)              :: h(100)
type(segy_bin_hdr),intent(in   ) :: binhdr
integer                          :: status
!--- local ---
integer                          :: i
logical                          :: swap


if(swap_endian() == 0 ) then
swap = .true.
else
swap = .false.
endif

!--- "h" is the address of the binary header that will be written.
!--- The second argument to segy_puttapebhval is the byte offset of the
!--- corresponding word.
h = 0

call segy_puttapebhval(h(1), 0,binhdr%jobid   ) ! h1
call segy_puttapebhval(h(1), 1,binhdr%lino    ) ! h2
call segy_puttapebhval(h(1), 2,binhdr%reno    ) ! h3

if (swap) then
do i = 0, 2
call segy_swaptapebhval(h(1),i)
end do
endif

h( 4) = binhdr%ntrpr ! h4
h( 5) = binhdr%nart
h( 6) = binhdr%hdt   ! h5
h( 7) = binhdr%dto
h( 8) = binhdr%hns   ! h6
h( 9) = binhdr%nso
h(10) = binhdr%format! h7
h(11) = binhdr%fold
h(12) = binhdr%tsort ! h8
h(13) = binhdr%vscode
h(14) = binhdr%hsfs  ! h9
h(15) = binhdr%hsfe

h(16) = binhdr%hslen ! h10
h(17) = binhdr%hstyp
h(18) = binhdr%schn  ! h11
h(19) = binhdr%hstas
h(20) = binhdr%hstae ! h12
h(21) = binhdr%htatyp
h(22) = binhdr%hcorr ! h13
h(23) = binhdr%bgrcv
h(24) = binhdr%rcvm  ! h14
h(25) = binhdr%mfeet
h(26) = binhdr%polyt ! h15
h(27) = binhdr%vpol
status = wrdc_pack(h(4),24,4,2,swap)

!
! No more conoco segy format.
! ehs 02nov01
!
!   call segy_puttapebhval(h(1),27,binhdr%conid   ) ! h16,17
!   call segy_puttapebhval(h(1),28,binhdr%tstart  ) ! h18
!   call segy_puttapebhval(h(1),29,binhdr%xorigin ) ! h19,20
!   call segy_puttapebhval(h(1),30,binhdr%yorigin ) ! h21,22
!   call segy_puttapebhval(h(1),31,binhdr%dx11    ) ! h23,24
!   call segy_puttapebhval(h(1),32,binhdr%dx12    ) ! h25,26
!   call segy_puttapebhval(h(1),33,binhdr%dx21    ) ! h27,28
!   call segy_puttapebhval(h(1),34,binhdr%dx22    ) ! h29,30
!   call segy_puttapebhval(h(1),35,binhdr%endian  ) ! h31
!   call segy_puttapebhval(h(1),36,binhdr%nbits   ) ! h32
!   if (swap) then
!     do i = 28, 36
!       call segy_swaptapebhval(h(1),i)
!     end do
!   endif

end function segy_binh2buf

function segy_read_ebchdr(unit,ascii) result(status)
integer,intent(in)               :: unit
type(segy_ebcdic_hdr),intent(out):: ascii
integer                          :: status
! --- local
integer                          :: i,j,buffer(800)
!--------------------------------------------------------------
status = cio_fread(buffer,3200,1,unit) - 1
if(status /= 0 ) return
call segy_buf2asc_hdr(buffer,ascii)
do i = 1, 40
do j = 1, 80
if(ascii%h(i)(j:j) == char(10) ) then
  ascii%h(i)(j:j)  = ' '
endif
end do
end do
end function segy_read_ebchdr

function segy_write_ebchdr(unit,ascii) result(status)
integer,intent(in)               :: unit
type(segy_ebcdic_hdr),intent(in) :: ascii
character(len=80),dimension(40)  :: ebcdic
integer                          :: status
! local vars
integer                          :: i,j
do i = 1, 40
ebcdic(i) = ascii%h(i)
do j = 1, 80
if(ebcdic(i)(j:j) == char(10) ) then
   ebcdic(i)(j:j)  = ' '
endif
end do
do j = len_trim(ebcdic(i))+1,80
ebcdic(i)(j:j) = ' '
end do
end do
call wrdc_asc_ebc(ebcdic)
status = cio_fwrite(ebcdic,3200,1,unit) - 1
end function segy_write_ebchdr

subroutine segy_init_ebchdr(sehdr)
!     initialize contents of ebcdic header to standard set of items,
!     initialized to null
type(segy_ebcdic_hdr), intent(inout) :: sehdr

character(len=80) :: ctmp

ctmp = ' '

sehdr%h( 1)( 1:40)='C 1 CLIENT                        COMPAN'
sehdr%h( 1)(41:80)='Y                       CREW NO         '

sehdr%h( 2)( 1:40)='C 2 LINE            AREA                '
sehdr%h( 2)(41:80)='        MAP ID                          '

sehdr%h( 3)( 1:40)='C 3 REEL              DAY-START OF REEL '
sehdr%h( 3)(41:80)='    YEAR      OBSERVER                  '

sehdr%h( 4)( 1:40)='C 4 INSTRUMENT: MFG            MODEL    '
sehdr%h( 4)(41:80)='        SERIAL NO                       '

sehdr%h( 5)( 1:40)='C 5 DATA TRACES/RECORD        AUXILIARY '
sehdr%h( 5)(41:80)='TRACES/RECORD         CDP FOLD          '

sehdr%h( 6)( 1:40)='C 6 SAMPLE INTERVAL         SAMPLES/TRAC'
sehdr%h( 6)(41:80)='E       BITS/IN      BYTES/SAMPLE       '

sehdr%h( 7)( 1:40)='C 7 RECORDING FORMAT        FORMAT THIS '
sehdr%h( 7)(41:80)='REEL        MEASUREMENT SYSTEM          '

sehdr%h( 8)( 1:40)='C 8 SAMPLE CODE:                        '
sehdr%h( 8)(41:80)='                                        '

sehdr%h( 9)( 1:40)='C 9 GAIN  TYPE:                         '
sehdr%h( 9)(41:80)='                                        '

sehdr%h(10)( 1:40)='C10 FILTERS: ALIAS     HZ  NOTCH     HZ '
sehdr%h(10)(41:80)=' BAND     -     HZ  SLOPE         DB/OCT'

sehdr%h(11)( 1:40)='C11 SOURCE: TYPE            NUMBER/POINT'
sehdr%h(11)(41:80)='        POINT INTERVAL                  '

sehdr%h(12)( 1:40)='C12     PATTERN:                        '
sehdr%h(12)(41:80)='   LENGTH        WIDTH                  '

sehdr%h(13)( 1:40)='C13 SWEEP: START     HZ  END     HZ  LEN'
sehdr%h(13)(41:80)='GTH      MS  CHANNEL NO     TYPE        '

sehdr%h(14)( 1:40)='C14 TAPER: START LENGTH       MS  END LE'
sehdr%h(14)(41:80)='NGTH       MS  TYPE                     '

sehdr%h(15)( 1:40)='C15 SPREAD: OFFSET        MAX DISTANCE  '
sehdr%h(15)(41:80)='      GROUP INTERVAL                    '

sehdr%h(16)( 1:40)='C16 GEOPHONES: PER GROUP     SPACING    '
sehdr%h(16)(41:80)=' FREQUENCY     MFG          MODEL       '

sehdr%h(17)( 1:40)='C17     PATTERN:                        '
sehdr%h(17)(41:80)='   LENGTH        WIDTH                  '

sehdr%h(18)( 1:40)='C18 TRACES SORTED BY:                   '
sehdr%h(18)(41:80)='                                        '

sehdr%h(19)( 1:40)='C19 AMPLITUDE RECOVERY:                 '
sehdr%h(19)(41:80)='                                        '

sehdr%h(20)( 1:40)='C20 MAP PROJECTION                      '
sehdr%h(20)(41:80)='ZONE ID       COORDINATE UNITS          '

sehdr%h(21)( 1:40)='C21 PROCESSING:                         '
sehdr%h(21)(41:80)='                                        '

sehdr%h(22)( 1:40)='C22                                     '
sehdr%h(22)(41:80)='                                        '

sehdr%h(23)( 1:40)='C23                                     '
sehdr%h(23)(41:80)='                                        '

sehdr%h(24)( 1:40)='C24                                     '
sehdr%h(24)(41:80)='                                        '

sehdr%h(25)( 1:40)='C25                                     '
sehdr%h(25)(41:80)='                                        '

sehdr%h(26)( 1:40)='C26                                     '
sehdr%h(26)(41:80)='                                        '

sehdr%h(27)( 1:40)='C27                                     '
sehdr%h(27)(41:80)='                                        '

sehdr%h(28)( 1:40)='C28                                     '
sehdr%h(28)(41:80)='                                        '

sehdr%h(29)( 1:40)='C29                                     '
sehdr%h(29)(41:80)='                                        '

sehdr%h(30)( 1:40)='C30                                     '
sehdr%h(30)(41:80)='                                        '

sehdr%h(31)( 1:40)='C31                                     '
sehdr%h(31)(41:80)='                                        '

sehdr%h(32)( 1:40)='C32                                     '
sehdr%h(32)(41:80)='                                        '

sehdr%h(33)( 1:40)='C33                                     '
sehdr%h(33)(41:80)='                                        '

sehdr%h(34)( 1:40)='C34                                     '
sehdr%h(34)(41:80)='                                        '

sehdr%h(35)( 1:40)='C35                                     '
sehdr%h(35)(41:80)='                                        '

sehdr%h(36)( 1:40)='C36                                     '
sehdr%h(36)(41:80)='                                        '

sehdr%h(37)( 1:40)='C37                                     '
sehdr%h(37)(41:80)='                                        '

sehdr%h(38)( 1:40)='C38                                     '
sehdr%h(38)(41:80)='                                        '

sehdr%h(39)( 1:40)='C39                                     '
sehdr%h(39)(41:80)='                                        '

sehdr%h(40)( 1:40)='C40 END EBCDIC                          '
sehdr%h(40)(41:80)='                                        '

sehdr%h( 1)(12:33) = ctmp(1:22)              ! H1_CLIENT
sehdr%h( 1)(43:63) = ctmp(1:21)              ! H1_COMPANY
sehdr%h( 1)(73:79) = ctmp(1:7)               ! H1_CREW_NO
sehdr%h( 2)(10:19) = ctmp(1:10)              ! H1_LINE
sehdr%h( 2)(26:47) = ctmp(1:22)              ! H1_AREA
sehdr%h( 2)(56:79) = ctmp(1:24)              ! H1_MAP_ID
sehdr%h( 3)(10:21) = ctmp(1:12)              ! H1_REEL_NO
sehdr%h( 3)(41:43) = ctmp(1:3)               ! H1_DAY
sehdr%h( 3)(50:53) = ctmp(1:4)               ! H1_YEAR
sehdr%h( 3)(64:79) = ctmp(1:16)              ! H1_OBSERVER
sehdr%h( 4)(21:30) = ctmp(1:10)              ! H1_INSTRUMENT_MFG
sehdr%h( 4)(38:47) = ctmp(1:10)              ! H1_INSTRUMENT_MODEL
sehdr%h( 4)(59:79) = ctmp(1:21)              ! H1_INSTRUMENT_SERIAL_NO
sehdr%h( 5)(24:29) = ctmp(1:6)               ! H1_DATA_TRACES_PER_RECORD
sehdr%h( 5)(55:61) = ctmp(1:7)               ! H1_AUX_TRACES_PER_RECORD
sehdr%h( 5)(72:79) = ctmp(1:8)               ! H1_CDP_FOLD
sehdr%h( 6)(21:27) = ctmp(1:7)               ! H1_SAMPLE_INTERVAL
sehdr%h( 6)(43:47) = ctmp(1:5)               ! H1_SAMPLES_PER_TRACE
sehdr%h( 6)(57:60) = ctmp(1:4)               ! H1_BITS_PER_IN
sehdr%h( 6)(75:79) = ctmp(1:5)               ! H1_BYTES_PER_SAMPLE
sehdr%h( 7)(22:27) = ctmp(1:6)               ! H1_RECORDING_FORMAT
sehdr%h( 7)(46:51) = ctmp(1:6)               ! H1_FORMAT_THIS_REEL
sehdr%h( 7)(72:79) = ctmp(1:8)               ! H1_MEASUREMENT_SYSTEM
sehdr%h( 8)(18:80) = ctmp(1:63)              ! H1_SAMPLE_CODE
sehdr%h( 9)(17:80) = ctmp(1:64)              ! H1_GAIN_TYPE
sehdr%h(10)(20:22) = ctmp(1:3)               ! H1_FILTER_ALIAS
sehdr%h(10)(34:36) = ctmp(1:3)               ! H1_FILTER_NOTCH
sehdr%h(10)(47:49) = ctmp(1:3)               ! H1_FILTER_LOW_CUT
sehdr%h(10)(53:55) = ctmp(1:3)               ! H1_FILTER_HIGH_CUT
sehdr%h(10)(67:73) = ctmp(1:7)               ! H1_FILTER_SLOPE
sehdr%h(11)(18:27) = ctmp(1:10)              ! H1_SOURCE_TYPE
sehdr%h(11)(42:47) = ctmp(1:6)               ! H1_SOURCE_NUMBER_PER_POINT
sehdr%h(11)(64:79) = ctmp(1:16)              ! H1_SOURCE_POINT_INTERVAL
sehdr%h(12)(18:42) = ctmp(1:25)              ! H1_SOURCE_PATTERN
sehdr%h(12)(51:56) = ctmp(1:6)               ! H1_SOURCE_LENGTH
sehdr%h(12)(64:79) = ctmp(1:16)              ! H1_SOURCE_WIDTH
sehdr%h(13)(18:20) = ctmp(1:3)               ! H1_SWEEP_START
sehdr%h(13)(30:32) = ctmp(1:3)               ! H1_SWEEP_END
sehdr%h(13)(45:48) = ctmp(1:4)               ! H1_SWEEP_LENGTH
sehdr%h(13)(65:67) = ctmp(1:3)               ! H1_SWEEP_CHANNEL_NO
sehdr%h(13)(74:79) = ctmp(1:6)               ! H1_SWEEP_TYPE
sehdr%h(14)(25:29) = ctmp(1:5)               ! H1_TAPER_START_LENGTH
sehdr%h(14)(46:50) = ctmp(1:5)               ! H1_TAPER_END_LENGTH
sehdr%h(14)(61:79) = ctmp(1:19)              ! H1_TAPER_TYPE
sehdr%h(15)(20:25) = ctmp(1:6)               ! H1_SPREAD_OFFSET
sehdr%h(15)(40:45) = ctmp(1:6)               ! H1_SPREAD_MAX_DISTANCE
sehdr%h(15)(62:79) = ctmp(1:18)              ! H1_SPREAD_GROUP_INTERVAL
sehdr%h(16)(26:28) = ctmp(1:3)               ! H1_GEOPHONES_PER_GROUP
sehdr%h(16)(38:40) = ctmp(1:3)               ! H1_GEOPHONE_SPACING
sehdr%h(16)(52:54) = ctmp(1:3)               ! H1_GEOPHONE_FREQUENCY
sehdr%h(16)(60:67) = ctmp(1:8)               ! H1_GEOPHONE_MFG
sehdr%h(16)(75:79) = ctmp(1:5)               ! H1_GEOPHONE_MODEL
sehdr%h(17)(18:42) = ctmp(1:25)              ! H1_GEOPHONE_PATTERN
sehdr%h(17)(51:56) = ctmp(1:6)               ! H1_GEOPHONE_LENGTH
sehdr%h(17)(64:79) = ctmp(1:16)              ! H1_GEOPHONE_WIDTH
sehdr%h(18)(23:80) = ctmp(1:58)              ! H1_TRACES_SORTED_BY
sehdr%h(19)(25:80) = ctmp(1:56)              ! H1_AMPLITUDE_RECOVERY
sehdr%h(20)(20:39) = ctmp(1:20)              ! H1_MAP_PROJECTION
sehdr%h(20)(49:53) = ctmp(1:5)               ! H1_ZONE_ID
sehdr%h(20)(72:79) = ctmp(1:8)               ! H1_COORDINATE_UNITS
sehdr%h(21)(17:79) = ctmp(1:72)              ! H1_PROCESSING
sehdr%h(22)(5:80) = ctmp(1:76)               ! H1_COMMENT01
sehdr%h(23)(5:80) = ctmp(1:76)               ! H1_COMMENT02
sehdr%h(24)(5:80) = ctmp(1:76)               ! H1_COMMENT03
sehdr%h(25)(5:80) = ctmp(1:76)               ! H1_COMMENT04
sehdr%h(26)(5:80) = ctmp(1:76)               ! H1_COMMENT05
sehdr%h(27)(5:80) = ctmp(1:76)               ! H1_COMMENT06
sehdr%h(28)(5:80) = ctmp(1:76)               ! H1_COMMENT07
sehdr%h(29)(5:80) = ctmp(1:76)               ! H1_COMMENT08
sehdr%h(30)(5:80) = ctmp(1:76)               ! H1_COMMENT09
sehdr%h(31)(5:80) = ctmp(1:76)               ! H1_COMMENT10
sehdr%h(32)(5:80) = ctmp(1:76)               ! H1_COMMENT11
sehdr%h(33)(5:80) = ctmp(1:76)               ! H1_COMMENT12
sehdr%h(34)(5:80) = ctmp(1:76)               ! H1_COMMENT13
sehdr%h(35)(5:80) = ctmp(1:76)               ! H1_COMMENT14
sehdr%h(36)(5:80) = ctmp(1:76)               ! H1_COMMENT15
sehdr%h(37)(5:80) = ctmp(1:76)               ! H1_COMMENT16
sehdr%h(38)(5:80) = ctmp(1:76)               ! H1_COMMENT17
sehdr%h(39)(5:80) = ctmp(1:76)               ! H1_COMMENT18

return

end subroutine segy_init_ebchdr

subroutine segy_set_field_ebchdr(sehdr, field, contents)
type(segy_ebcdic_hdr), intent(inout) :: sehdr
character(len=*),      intent(in)    :: field, contents

if (field == 'H1_CLIENT') then
sehdr%h( 1)(12:33) = contents
else if (field == 'H1_COMPANY') then
sehdr%h( 1)(43:63) = contents
else if (field == 'H1_CREW_NO') then
sehdr%h( 1)(73:79) = contents
else if (field == 'H1_LINE') then
sehdr%h( 2)(10:19) = contents
else if (field == 'H1_AREA') then
sehdr%h( 2)(26:47) = contents
else if (field == 'H1_MAP_ID') then
sehdr%h( 2)(56:79) = contents
else if (field == 'H1_REEL_NO') then
sehdr%h( 3)(10:21) = contents
else if (field == 'H1_DAY') then
sehdr%h( 3)(41:43) = contents
else if (field == 'H1_YEAR') then
sehdr%h( 3)(50:53) = contents
else if (field == 'H1_OBSERVER') then
sehdr%h( 3)(64:79) = contents
else if (field == 'H1_INSTRUMENT_MFG') then
sehdr%h( 4)(21:30) = contents
else if (field == 'H1_INSTRUMENT_MODEL') then
sehdr%h( 4)(38:47) = contents
else if (field == 'H1_INSTRUMENT_SERIAL_NO') then
sehdr%h( 4)(59:79) = contents
else if (field == 'H1_DATA_TRACES_PER_RECORD') then
sehdr%h( 5)(24:29) = contents
else if (field == 'H1_AUX_TRACES_PER_RECORD') then
sehdr%h( 5)(55:61) = contents
else if (field == 'H1_CDP_FOLD') then
sehdr%h( 5)(72:79) = contents
else if (field == 'H1_SAMPLE_INTERVAL') then
sehdr%h( 6)(21:27) = contents
else if (field == 'H1_SAMPLES_PER_TRACE') then
sehdr%h( 6)(43:47) = contents
else if (field == 'H1_BITS_PER_IN') then
sehdr%h( 6)(57:60) = contents
else if (field == 'H1_BYTES_PER_SAMPLE') then
sehdr%h( 6)(75:79) = contents
else if (field == 'H1_RECORDING_FORMAT') then
sehdr%h( 7)(22:27) = contents
else if (field == 'H1_FORMAT_THIS_REEL') then
sehdr%h( 7)(46:51) = contents
else if (field == 'H1_MEASUREMENT_SYSTEM') then
sehdr%h( 7)(72:79) = contents
else if (field == 'H1_SAMPLE_CODE') then
sehdr%h( 8)(18:80) = contents
else if (field == 'H1_GAIN_TYPE') then
sehdr%h( 9)(17:80) = contents
else if (field == 'H1_FILTER_ALIAS') then
sehdr%h(10)(20:22) = contents
else if (field == 'H1_FILTER_NOTCH') then
sehdr%h(10)(34:36) = contents
else if (field == 'H1_FILTER_LOW_CUT') then
sehdr%h(10)(47:49) = contents
else if (field == 'H1_FILTER_HIGH_CUT') then
sehdr%h(10)(53:55) = contents
else if (field == 'H1_FILTER_SLOPE') then
sehdr%h(10)(67:73) = contents
else if (field == 'H1_SOURCE_TYPE') then
sehdr%h(11)(18:27) = contents
else if (field == 'H1_SOURCE_NUMBER_PER_POINT') then
sehdr%h(11)(42:47) = contents
else if (field == 'H1_SOURCE_POINT_INTERVAL') then
sehdr%h(11)(64:79) = contents
else if (field == 'H1_SOURCE_PATTERN') then
sehdr%h(12)(18:42) = contents
else if (field == 'H1_SOURCE_LENGTH') then
sehdr%h(12)(51:56) = contents
else if (field == 'H1_SOURCE_WIDTH') then
sehdr%h(12)(64:79) = contents
else if (field == 'H1_SWEEP_START') then
sehdr%h(13)(18:20) = contents
else if (field == 'H1_SWEEP_END') then
sehdr%h(13)(30:32) = contents
else if (field == 'H1_SWEEP_LENGTH') then
sehdr%h(13)(45:48) = contents
else if (field == 'H1_SWEEP_CHANNEL_NO') then
sehdr%h(13)(65:67) = contents
else if (field == 'H1_SWEEP_TYPE') then
sehdr%h(13)(74:79) = contents
else if (field == 'H1_TAPER_START_LENGTH') then
sehdr%h(14)(25:29) = contents
else if (field == 'H1_TAPER_END_LENGTH') then
sehdr%h(14)(46:50) = contents
else if (field == 'H1_TAPER_TYPE') then
sehdr%h(14)(61:79) = contents
else if (field == 'H1_SPREAD_OFFSET') then
sehdr%h(15)(20:25) = contents
else if (field == 'H1_SPREAD_MAX_DISTANCE') then
sehdr%h(15)(40:45) = contents
else if (field == 'H1_SPREAD_GROUP_INTERVAL') then
sehdr%h(15)(62:79) = contents
else if (field == 'H1_GEOPHONES_PER_GROUP') then
sehdr%h(16)(26:28) = contents
else if (field == 'H1_GEOPHONE_SPACING') then
sehdr%h(16)(38:40) = contents
else if (field == 'H1_GEOPHONE_FREQUENCY') then
sehdr%h(16)(52:54) = contents
else if (field == 'H1_GEOPHONE_MFG') then
sehdr%h(16)(60:67) = contents
else if (field == 'H1_GEOPHONE_MODEL') then
sehdr%h(16)(75:79) = contents
else if (field == 'H1_GEOPHONE_PATTERN') then
sehdr%h(17)(18:42) = contents
else if (field == 'H1_GEOPHONE_LENGTH') then
sehdr%h(17)(51:56) = contents
else if (field == 'H1_GEOPHONE_WIDTH') then
sehdr%h(17)(64:79) = contents
else if (field == 'H1_TRACES_SORTED_BY') then
sehdr%h(18)(23:80) = contents
else if (field == 'H1_AMPLITUDE_RECOVERY') then
sehdr%h(19)(25:80) = contents
else if (field == 'H1_MAP_PROJECTION') then
sehdr%h(20)(20:39) = contents
else if (field == 'H1_ZONE_ID') then
sehdr%h(20)(49:53) = contents
else if (field == 'H1_COORDINATE_UNITS') then
sehdr%h(20)(72:79) = contents
else if (field == 'H1_PROCESSING') then
sehdr%h(21)(17:79) = contents
else if (field == 'H1_COMMENT01') then
sehdr%h(22)(5:80)  = contents
else if (field == 'H1_COMMENT02') then
sehdr%h(23)(5:80)  = contents
else if (field == 'H1_COMMENT03') then
sehdr%h(24)(5:80)  = contents
else if (field == 'H1_COMMENT04') then
sehdr%h(25)(5:80)  = contents
else if (field == 'H1_COMMENT05') then
sehdr%h(26)(5:80)  = contents
else if (field == 'H1_COMMENT06') then
sehdr%h(27)(5:80)  = contents
else if (field == 'H1_COMMENT07') then
sehdr%h(28)(5:80)  = contents
else if (field == 'H1_COMMENT08') then
sehdr%h(29)(5:80)  = contents
else if (field == 'H1_COMMENT09') then
sehdr%h(30)(5:80)  = contents
else if (field == 'H1_COMMENT10') then
sehdr%h(31)(5:80)  = contents
else if (field == 'H1_COMMENT11') then
sehdr%h(32)(5:80)  = contents
else if (field == 'H1_COMMENT12') then
sehdr%h(33)(5:80)  = contents
else if (field == 'H1_COMMENT13') then
sehdr%h(34)(5:80)  = contents
else if (field == 'H1_COMMENT14') then
sehdr%h(35)(5:80)  = contents
else if (field == 'H1_COMMENT15') then
sehdr%h(36)(5:80)  = contents
else if (field == 'H1_COMMENT16') then
sehdr%h(37)(5:80)  = contents
else if (field == 'H1_COMMENT17') then
sehdr%h(38)(5:80)  = contents
else if (field == 'H1_COMMENT18') then
sehdr%h(39)(5:80)  = contents
end if

return

end subroutine segy_set_field_ebchdr


subroutine segy_pack_segyhd(syh,hdum,optional_swap)
type(segy_trc_hdr),intent(in)              :: syh
integer, intent(out),dimension(60)         :: hdum
logical , intent(in),optional              :: optional_swap
!--- local variables
integer                                    :: i,status
logical                                    :: swap
integer                                    :: h(68)
if(present(optional_swap)) then
swap = optional_swap
else
swap = .false.
endif

call segy_puttapehval(h(1), 0,syh%tracl      ) !h01
call segy_puttapehval(h(1), 1,syh%tracr      ) !h02
call segy_puttapehval(h(1), 2,syh%fldr       ) !h03
call segy_puttapehval(h(1), 3,syh%tracf      ) !h04
call segy_puttapehval(h(1), 4,syh%ep         ) !h05
call segy_puttapehval(h(1), 5,syh%cdp        ) !h06
call segy_puttapehval(h(1), 6,syh%cdpt       ) !h07
if(swap) then
do i = 0, 6
call segy_swaptapehval(h(1),i)
end do
endif

h( 8) = syh%trid      ! h 08
h( 9) = syh%nvs
h(10) = syh%nhs       ! h 09
h(11) = syh%duse
status = wrdc_pack(h(8),4,4,2,swap)

call segy_puttapehval(h(1),11,syh%offset     ) ! h10
call segy_puttapehval(h(1),12,syh%gelev      ) ! h11
call segy_puttapehval(h(1),13,syh%selev      ) ! h12
call segy_puttapehval(h(1),14,syh%sdepth     ) ! h13
call segy_puttapehval(h(1),15,syh%gdel       ) ! h14
call segy_puttapehval(h(1),16,syh%sdel       ) ! h15
call segy_puttapehval(h(1),17,syh%swdep      ) ! h16
call segy_puttapehval(h(1),18,syh%gwdep      ) ! h17
if(swap) then
do i = 11,18
call segy_swaptapehval(h(1),i)
end do
endif

h(18) = syh%scalel    ! h18
h(19) = syh%scalco
status = wrdc_pack(h(18),2,4,2,swap)

call segy_puttapehval(h(1),21,syh%sx         ) ! h19
call segy_puttapehval(h(1),22,syh%sy         ) ! h20
call segy_puttapehval(h(1),23,syh%gx         ) ! h21
call segy_puttapehval(h(1),24,syh%gy         ) ! h22
if(swap) then
do i = 21,24
call segy_swaptapehval(h(1),i)
end do
endif

h(23) = syh%counit    ! h23
h(24) = syh%wevel     !
h(25) = syh%swevel    ! h24
h(26) = syh%sut       !
h(27) = syh%gut       ! h25
h(28) = syh%sstat     !
h(29) = syh%gstat     ! h26
h(30) = syh%tstat     !
h(31) = syh%laga      ! h27
h(32) = syh%lagb      !
h(33) = syh%delrt     ! h28
h(34) = syh%muts      !
h(35) = syh%mute      ! h29
h(36) = syh%ns        !
h(37) = syh%dt        ! h30
h(38) = syh%gain      !
h(39) = syh%igc       ! h31
h(40) = syh%igi       !
h(41) = syh%corr      ! h32
h(42) = syh%sfs       !
h(43) = syh%sfe       ! h33
h(44) = syh%slen      !
h(45) = syh%styp      ! h34
h(46) = syh%stas      !
h(47) = syh%stae      ! h35
h(48) = syh%tatyp     !
h(49) = syh%afilf     ! h36
h(50) = syh%afils     !
h(51) = syh%nofilf    ! h37
h(52) = syh%nofils    !
h(53) = syh%lcf       ! h38
h(54) = syh%hcf       !
h(55) = syh%lcs       ! h39
h(56) = syh%hcs       !
h(57) = syh%year      ! h40
h(58) = syh%day       !
h(59) = syh%hour      ! h41
h(60) = syh%minute    !
h(61) = syh%sec       ! h42
h(62) = syh%timbas    !
h(63) = syh%trwf      ! h43
h(64) = syh%grnors    !
h(65) = syh%grnofr    ! h44
h(66) = syh%grnlof    !
h(67) = syh%gaps      ! h45
h(68) = syh%otrav     !
status = wrdc_pack(h(23),46,4,2,swap)

!
! No more conoco segy format.
! ehs 02nov01
!
!   call segy_puttapehval(h(1),71,syh%conid      )
!   call segy_puttapehval(h(1),72,syh%lav        )
!   call segy_puttapehval(h(1),73,syh%mp_xloc    )
!   call segy_puttapehval(h(1),74,syh%mp_yloc    )
!   call segy_puttapehval(h(1),75,syh%mp_elev    )
!   call segy_puttapehval(h(1),76,syh%mp_xgrid   )
!   call segy_puttapehval(h(1),77,syh%mp_ygrid   )
!   call segy_puttapehval(h(1),78,syh%gain_for_recovery)

! Pack the first fourteen 4 byte entities from unused portion
! of segy trace header (bytes 181 through 236).
!   wjd: 2003/8/11
h(46:59) = syh%unused(:)
if (swap) then
call swap_bytes(h(46), 14)
end if

! Change index of 71 to 85 in following segy_swaptapehval() and
! segy_gettapehval() calls because of change in tapehdr[]
! in segy_crou.h.
!   wjd: 2003/8/11
call segy_puttapehval(h(1),85,syh%unused1    )
if(swap) then
call segy_swaptapehval(h(1),85)
endif

!   now assign the sixty 4 byte words of packed h array to output array hdum
hdum = h(:60)
end subroutine segy_pack_segyhd

subroutine segy_unpack_segyhd(syh,hdum,optional_swap)
type(segy_trc_hdr),intent(out)             :: syh
integer, intent(in ),dimension(60)         :: hdum
logical , intent(in),optional              :: optional_swap
!--- local variables
integer                                    ::   status 
integer                                    :: h(68)
logical                                    :: swap

h(1:60)       = hdum
h(61:)        = 0
if(present(optional_swap)) then
swap    = optional_swap
else
swap    = .false.
endif
if (swap ) call swap_bytes(h(1),7)
syh%tracl           = h( 1)
syh%tracr           = h( 2)
syh%fldr            = h( 3)
syh%tracf           = h( 4)
syh%ep              = h( 5)
syh%cdp             = h( 6)
syh%cdpt            = h( 7)

status = wrdc_unpack(h(8),4,2,4,swap)
syh%trid            = h( 8)
syh%nvs             = h( 9)
syh%nhs             = h(10)
syh%duse            = h(11)

h(10:11) = hdum(10:11)
if (swap ) call swap_bytes(h(10),8)
syh%offset          = h(10)
syh%gelev           = h(11)
syh%selev           = h(12)
syh%sdepth          = h(13)
syh%gdel            = h(14)
syh%sdel            = h(15)
syh%swdep           = h(16)
syh%gwdep           = h(17)

status = wrdc_unpack(h(18),2,2,4,swap)
syh%scalel          = h(18)
syh%scalco          = h(19)

h(19) = hdum(19)
if (swap ) call swap_bytes(h(19),4)

syh%sx              = h(19)
syh%sy              = h(20)
syh%gx              = h(21)
syh%gy              = h(22)

status = wrdc_unpack(h(23),46,2,4,swap)
syh%counit          = h(23)
syh%wevel           = h(24)
syh%swevel          = h(25)
syh%sut             = h(26)
syh%gut             = h(27)
syh%sstat           = h(28)
syh%gstat           = h(29)
syh%tstat           = h(30)
syh%laga            = h(31)
syh%lagb            = h(32)
syh%delrt           = h(33)
syh%muts            = h(34)
syh%mute            = h(35)
syh%ns              = h(36)
syh%dt              = h(37)
syh%gain            = h(38)
syh%igc             = h(39)
syh%igi             = h(40)
syh%corr            = h(41)
syh%sfs             = h(42)
syh%sfe             = h(43)
syh%slen            = h(44)
syh%styp            = h(45)
syh%stas            = h(46)
syh%stae            = h(47)
syh%tatyp           = h(48)
syh%afilf           = h(49)
syh%afils           = h(50)
syh%nofilf          = h(51)
syh%nofils          = h(52)
syh%lcf             = h(53)
syh%hcf             = h(54)
syh%lcs             = h(55)
syh%hcs             = h(56)
syh%year            = h(57)
syh%day             = h(58)
syh%hour            = h(59)
syh%minute          = h(60)
syh%sec             = h(61)
syh%timbas          = h(62)
syh%trwf            = h(63)
syh%grnors          = h(64)
syh%grnofr          = h(65)
syh%grnlof          = h(66)
syh%gaps            = h(67)
syh%otrav           = h(68)

h(46:60) = hdum(46:60)

!
! No more conoco segy format.
! ehs 01nov01
!
!   call segy_gettapehval(h(1),71,syh%conid      )
!   if(swap) then
!     do i = 72,79
!       call segy_swaptapehval(h(1),i)
!     end do
!   endif
!   call segy_gettapehval(h(1),72,syh%lav        )
!   call segy_gettapehval(h(1),73,syh%mp_xloc    )
!   call segy_gettapehval(h(1),74,syh%mp_yloc    )
!   call segy_gettapehval(h(1),75,syh%mp_elev    )
!   call segy_gettapehval(h(1),76,syh%mp_xgrid   )
!   call segy_gettapehval(h(1),77,syh%mp_ygrid   )
!   call segy_gettapehval(h(1),78,syh%gain_for_recovery)

! Unpack the unused portion, segy trace header bytes 181-236
!   wjd: 2003/8/11
if (swap ) call swap_bytes(h(46),14)
syh%unused          = h(46:59)
!
! Change index of 71 to 85 in following segy_swaptapehval() and
! segy_gettapehval() calls because of change in tapehdr[]
! in segy_crou.h
!   wjd: 2003/8/11
if (swap) then
call segy_swaptapehval(h(1),85)
endif
call segy_gettapehval (h(1),85,syh%unused1)

end subroutine segy_unpack_segyhd

subroutine segy_cpshd_to_segyhd(cpshd,segyhd,tstart,dt,ns)
double precision,intent(in),dimension(:)    :: cpshd
type(segy_trc_hdr),intent(out)              :: segyhd
double precision,intent(in)                 :: tstart,dt
integer,intent(in)                          :: ns
double precision                            :: fescale,fxscale

fescale = 1.0
fxscale = 1.0

segyhd%tracl               = nint(cpshd(HDR_SEQUENCE))
segyhd%tracr               = nint(cpshd(HDR_SEQUENCE))
segyhd%fldr                = nint(cpshd(HDR_CURRENT_GROUP))
segyhd%tracf               = nint(cpshd(HDR_CURRENT_CHANNEL))
segyhd%ep                  = nint(cpshd(HDR_SOURCE_SHOTPOINT))
segyhd%cdp                 = nint(cpshd(HDR_MIDPOINT_SHOTPOINT))
segyhd%cdpt                = nint(cpshd(HDR_MIDPOINT_LINE))
segyhd%trid                = 1
segyhd%nvs                 = 0
segyhd%nhs                 = nint(cpshd(HDR_FOLD))
segyhd%duse                = 0
segyhd%offset              = nint(cpshd(HDR_OFFSET))
segyhd%gelev               = nint(cpshd(HDR_RECEIVER_ELEV)/fescale)
segyhd%selev               = nint(cpshd(HDR_SOURCE_ELEV)/fescale)
segyhd%sdepth              = nint(cpshd(HDR_SOURCE_DEPTH)/fescale)
segyhd%gdel                = 0
segyhd%sdel                = 0
segyhd%swdep               = 0
segyhd%gwdep               = 0
segyhd%scalel              = 1
if(fescale >= 1.0 ) then
segyhd%scalel            = nint(fescale)
elseif(fescale /= 0.0 ) then
segyhd%scalel            = -nint(1./fescale)
endif
segyhd%scalco              = 1
if(fxscale >= 1.0 ) then
segyhd%scalco            = nint(fxscale)
elseif(fxscale /= 0.0 ) then
segyhd%scalco            = -nint(1./fxscale)
endif
segyhd%sx                  = nint(cpshd(HDR_SOURCE_XLOC)/fxscale)
segyhd%sy                  = nint(cpshd(HDR_SOURCE_YLOC)/fxscale)
segyhd%gx                  = nint(cpshd(HDR_RECEIVER_XLOC)/fxscale)
segyhd%gy                  = nint(cpshd(HDR_RECEIVER_YLOC)/fxscale)
segyhd%counit              = 1
segyhd%wevel               = 0
segyhd%swevel              = 0
segyhd%sut                 = nint(cpshd(HDR_SOURCE_UPTIME))
segyhd%gut                 = nint(cpshd(HDR_RECEIVER_UPTIME))
segyhd%sstat               = nint(cpshd(HDR_PRE))
segyhd%gstat               = nint(cpshd(HDR_POST))
segyhd%tstat               = nint(cpshd(HDR_CUM_RESID_STATIC))
segyhd%laga                = 0
segyhd%lagb                = 0
segyhd%delrt               = 0
segyhd%muts                = nint(1e3*(tstart + (cpshd(HDR_TOP_MUTE)-1)*dt))
segyhd%mute                = nint(1e3*(tstart + &
                              (cpshd(HDR_BOTTOM_MUTE)-1)*dt))
segyhd%ns                  = ns
segyhd%dt                  = nint(dt*1.e6)
segyhd%gain                = 0
segyhd%igc                 = 0
segyhd%igi                 = 0
segyhd%corr                = 0
segyhd%sfs                 = 0
segyhd%sfe                 = 0
segyhd%slen                = 0
segyhd%styp                = 0
segyhd%stas                = 0
segyhd%stae                = 0
segyhd%tatyp               = 0
segyhd%afilf               = 0
segyhd%afils               = 0
segyhd%nofilf              = 0
segyhd%nofils              = 0
segyhd%lcf                 = 0
segyhd%hcf                 = 0
segyhd%lcs                 = 0
segyhd%hcs                 = 0
segyhd%year                = 0
segyhd%day                 = 0
segyhd%hour                = 0
segyhd%minute              = 0
segyhd%sec                 = 0
segyhd%timbas              = 0
segyhd%trwf                = 0
segyhd%grnors              = 0
segyhd%grnofr              = 0
segyhd%grnlof              = 0
segyhd%gaps                = 0
segyhd%otrav               = 0
!
! No more conoco segy format.
! ehs 02nov01
!
!   segyhd%conid               = '*CONOCO*'               !    -> 181:188
!   segyhd%lav                 = 1.0*cpshd(HDR_LAV)       ! 25 -> 189:192
!   segyhd%mp_xloc             = cpshd(HDR_MIDPOINT_XLOC) ! 17 -> 193:200
!   segyhd%mp_yloc             = cpshd(HDR_MIDPOINT_YLOC) ! 18 -> 201:208
!   segyhd%mp_elev             = cpshd(HDR_MIDPOINT_ELEV) ! 19 -> 209:216
!   segyhd%mp_xgrid            = cpshd(HDR_MIDPOINT_XGRID)!  7 -> 217:224
!   segyhd%mp_ygrid            = cpshd(HDR_MIDPOINT_YGRID)!  8 -> 225:232
!   !segyhd%gain_for_recovery  = cpshd(HDR_LAV    )       !    -> 233:236
!
! Initialize the unused portion of segy header prior to last 4 byte word
!   wjd: 2003/8/11
segyhd%unused              = 0                        !    -> 181:236
segyhd%unused1             = 0                        !    -> 237:240
end subroutine segy_cpshd_to_segyhd

subroutine segy_segyhd_to_cpshd(cpshd,segyhd,tstart)
double precision,intent(out),dimension(:)    :: cpshd
type(segy_trc_hdr),intent(in)                :: segyhd
double precision                             :: tstart
!--- tstart = time of first sample
!   --- local variables ---
double precision                             :: fescale,fxscale
integer                                      :: top_mute,bottom_mute

cpshd(:) = 0
top_mute    = 1
bottom_mute = 1
!--- Calculate mute indices from dt,tstart, and segy header.
if(segyhd%dt /= 0 ) then
top_mute    = 1 + nint((segyhd%muts*1d-3 - tstart)/(segyhd%dt*1D-6))
!          There is no segy equivalent to cps header 64.  The segy start
!        and end mutes refer to a range of samples to mute.  Always set
!        word 64 to the number of samples.
!!!      bottom_mute = 1 + nint((segyhd%mute*1d-3 - tstart)/(segyhd%dt*1D-6))

bottom_mute = int(segyhd%ns)

endif
! calculate the scalers
fescale = 1.0
if (segyhd%scalel /= 0 .and. &
segyhd%scalel > -10001 .and. &
segyhd%scalel <  10001) then
if(segyhd%scalel > 0 ) then
fescale = segyhd%scalel*fescale
elseif(segyhd%scalel < 0 ) then
fescale = -fescale/segyhd%scalel
endif
endif
fxscale = 1.0
if (segyhd%scalco /= 0 .and. &
segyhd%scalco > -10001 .and. &
segyhd%scalco <  10001) then
if(segyhd%scalco > 0 ) then
fxscale = segyhd%scalco*fxscale
elseif (segyhd%scalco < 0 ) then
fxscale = -fxscale/segyhd%scalco
endif
endif
!                                                cps <- SEGY bytes
!---                                             head#  stb:enb
cpshd(HDR_SEQUENCE) = segyhd%tracl               ! 1 <- 001:004
cpshd(HDR_TOP_MUTE) = top_mute                   ! 2 <- 111:112 derived
cpshd(HDR_CURRENT_GROUP) = segyhd%fldr           ! 3 <- 009:012
cpshd(HDR_CURRENT_CHANNEL) = segyhd%tracf        ! 4 <- 013:016
cpshd(HDR_FOLD) = segyhd%nhs                     ! 5 <- 033:034
cpshd(HDR_OFFSET) = segyhd%offset                ! 6 <- 037:040
cpshd(HDR_MIDPOINT_XGRID) = segyhd%cdp           ! 7 <- 021:024
cpshd(HDR_ORIGINAL_GROUP) = segyhd%fldr          ! 9 <- 009:012
cpshd(HDR_ORIGINAL_CHANNEL) = segyhd%tracf       !10 <- 013:016
cpshd(HDR_SOURCE_XLOC) = fxscale*segyhd%sx       !11 <- 073:076
cpshd(HDR_SOURCE_YLOC) = fxscale*segyhd%sy       !12 <- 077:080
cpshd(HDR_SOURCE_ELEV) = fescale*segyhd%selev    !13 <- 045:048
cpshd(HDR_RECEIVER_XLOC) = fxscale*segyhd%gx     !14 <- 081:084
cpshd(HDR_RECEIVER_YLOC) = fxscale*segyhd%gy     !15 <- 085:088
cpshd(HDR_RECEIVER_ELEV) = fescale*segyhd%gelev  !16 <- 041:044
cpshd(HDR_MIDPOINT_XLOC) = .5*  &
             ( cpshd(HDR_SOURCE_XLOC) + &
           cpshd(HDR_RECEIVER_XLOC) )        !17 <-         derived
cpshd(HDR_MIDPOINT_YLOC) = .5*  &
             ( cpshd(HDR_SOURCE_YLOC) + &
           cpshd(HDR_RECEIVER_YLOC) )        !18 <-         derived
cpshd(HDR_MIDPOINT_ELEV) = .5*  &
             ( cpshd(HDR_SOURCE_ELEV) + &
           cpshd(HDR_RECEIVER_ELEV) )        !19 <-         derived
cpshd(HDR_SOURCE_DEPTH) = fescale*segyhd%sdepth  !20 <- 049:052
cpshd(HDR_SOURCE_SHOTPOINT) = segyhd%ep          !29 <- 017:020
cpshd(HDR_MIDPOINT_SHOTPOINT) =  segyhd%cdp      !37 <- 021:024
cpshd(HDR_MIDPOINT_LINE     ) =  segyhd%cdpt     !38 <- 025:028
cpshd(HDR_PRE) =  segyhd%sstat                   !39 <- 099:100
cpshd(HDR_POST) = segyhd%gstat                   !40 <- 101:102
cpshd(HDR_CUM_RESID_STATIC) = segyhd%tstat       !43 <- 103:104
cpshd(HDR_SOURCE_UPTIME) = segyhd%sut            !44 <- 095:096
cpshd(HDR_RECEIVER_UPTIME) = segyhd%gut          !45 <- 097:098

!
! No more using user or scratch headers.
! ehs 05nov01
!
!   cpshd(HDR_USER_53) = segyhd%dt*1D-6              !53 <- 117:118 modif.

! Calculate trace scaling.
! HDR_SCRATCH_30 will be used in trcio_read_trace to scale.
!
if ((segyhd%trwf > 0) .and. (segyhd%trwf < 32)) then
cpshd(HDR_SCRATCH_30) = 2.0**(-segyhd%trwf)    !30 <- 169:170 derived
else
cpshd(HDR_SCRATCH_30) = 1.0
endif

!
! No more using user or scratch headers.
! ehs 05nov01
!
!   cpshd(HDR_USER_55) = segyhd%delrt                !55 <- 109:110
!   cpshd(HDR_SCRATCH_58) = segyhd%trid              !58 <- 029:030
!   cpshd(HDR_SCRATCH_59) = segyhd%duse              !59 <- 035:036
!   cpshd(HDR_SCRATCH_60) = segyhd%ns                !60 <- 115:116
!   cpshd(HDR_SCRATCH_61) = fescale*segyhd%swdep     !61 <- 061:064
!   cpshd(HDR_SCRATCH_62) = fescale*segyhd%gwdep     !62 <- 065:068
cpshd(HDR_BOTTOM_MUTE) = bottom_mute             !64 <- 113:114 derived
!
! No more conoco segy format.
! ehs 01nov01
!
!   if(segyhd%conid == '*CONOCO*' ) then
!     cpshd(HDR_LAV) = 1D0*segyhd%lav                !25 <- 189:192
!     cpshd(HDR_MIDPOINT_XLOC) = segyhd%mp_xloc      !17 <- 193:200
!     cpshd(HDR_MIDPOINT_YLOC) = segyhd%mp_yloc      !18 <- 201:208
!     cpshd(HDR_MIDPOINT_ELEV) = segyhd%mp_elev      !19 <- 209:216
!     cpshd(HDR_MIDPOINT_XGRID) = segyhd%mp_xgrid    ! 7 <- 217:224
!     cpshd(HDR_MIDPOINT_YGRID) = segyhd%mp_ygrid    ! 8 <- 225:232
!     !cpshd(HDR_LAV)    = segyhd%gain_for_recovery  !52 <- 233:236
!   else
!     ! For downward compatibility with spws applications.  ehs -- 16may01
cpshd(HDR_MIDPOINT_YGRID) = segyhd%unused1     ! 8 <- 237:240
!   endif

end subroutine segy_segyhd_to_cpshd

subroutine segy_print_trace_hd(segyhd, lun_in)

type(segy_trc_hdr),intent(in) :: segyhd
integer, intent(in), optional :: lun_in

integer                       :: lun

if (present(lun_in)) then
lun = lun_in
else
lun = 6
endif

if(segyhd%tracl /= 0)write(lun, *)' segyhd%tracl     ',segyhd%tracl
if(segyhd%tracr /= 0)write(lun, *)' segyhd%tracr     ',segyhd%tracr
if(segyhd%fldr /= 0)write(lun, *)' segyhd%fldr      ',segyhd%fldr
if(segyhd%tracf /= 0)write(lun, *)' segyhd%tracf     ',segyhd%tracf
if(segyhd%ep /= 0)write(lun, *)' segyhd%ep        ',segyhd%ep
if(segyhd%cdp /= 0)write(lun, *)' segyhd%cdp       ',segyhd%cdp
if(segyhd%cdpt /= 0)write(lun, *)' segyhd%cdpt      ',segyhd%cdpt
if(segyhd%trid /= 0)write(lun, *)' segyhd%trid      ',segyhd%trid
if(segyhd%nvs /= 0)write(lun, *)' segyhd%nvs       ',segyhd%nvs
if(segyhd%nhs /= 0)write(lun, *)' segyhd%nhs       ',segyhd%nhs
if(segyhd%duse /= 0)write(lun, *)' segyhd%duse      ',segyhd%duse
if(segyhd%offset /= 0)write(lun, *)' segyhd%offset    ',segyhd%offset
if(segyhd%gelev /= 0)write(lun, *)' segyhd%gelev     ',segyhd%gelev
if(segyhd%selev /= 0)write(lun, *)' segyhd%selev     ',segyhd%selev
if(segyhd%sdepth /= 0)write(lun, *)' segyhd%sdepth    ',segyhd%sdepth
if(segyhd%gdel /= 0)write(lun, *)' segyhd%gdel      ',segyhd%gdel
if(segyhd%sdel /= 0)write(lun, *)' segyhd%sdel      ',segyhd%sdel
if(segyhd%swdep /= 0)write(lun, *)' segyhd%swdep     ',segyhd%swdep
if(segyhd%gwdep /= 0)write(lun, *)' segyhd%gwdep     ',segyhd%gwdep
if(segyhd%scalel /= 0)write(lun, *)' segyhd%scalel    ',segyhd%scalel
if(segyhd%scalco /= 0)write(lun, *)' segyhd%scalco    ',segyhd%scalco
if(segyhd%sx /= 0)write(lun, *)' segyhd%sx        ',segyhd%sx
if(segyhd%sy /= 0)write(lun, *)' segyhd%sy        ',segyhd%sy
if(segyhd%gx /= 0)write(lun, *)' segyhd%gx        ',segyhd%gx
if(segyhd%gy /= 0)write(lun, *)' segyhd%gy        ',segyhd%gy
if(segyhd%counit /= 0)write(lun, *)' segyhd%counit    ',segyhd%counit
if(segyhd%wevel /= 0)write(lun, *)' segyhd%wevel     ',segyhd%wevel
if(segyhd%swevel /= 0)write(lun, *)' segyhd%swevel    ',segyhd%swevel
if(segyhd%sut /= 0)write(lun, *)' segyhd%sut       ',segyhd%sut
if(segyhd%gut /= 0)write(lun, *)' segyhd%gut       ',segyhd%gut
if(segyhd%sstat /= 0)write(lun, *)' segyhd%sstat     ',segyhd%sstat
if(segyhd%gstat /= 0)write(lun, *)' segyhd%gstat     ',segyhd%gstat
if(segyhd%tstat /= 0)write(lun, *)' segyhd%tstat     ',segyhd%tstat
if(segyhd%laga /= 0)write(lun, *)' segyhd%laga      ',segyhd%laga
if(segyhd%lagb /= 0)write(lun, *)' segyhd%lagb      ',segyhd%lagb
if(segyhd%delrt /= 0)write(lun, *)' segyhd%delrt     ',segyhd%delrt
if(segyhd%muts /= 0)write(lun, *)' segyhd%muts      ',segyhd%muts
if(segyhd%mute /= 0)write(lun, *)' segyhd%mute      ',segyhd%mute
if(segyhd%ns /= 0)write(lun, *)' segyhd%ns        ',segyhd%ns
if(segyhd%dt /= 0)write(lun, *)' segyhd%dt        ',segyhd%dt
if(segyhd%gain /= 0)write(lun, *)' segyhd%gain      ',segyhd%gain
if(segyhd%igc /= 0)write(lun, *)' segyhd%igc       ',segyhd%igc
if(segyhd%igi /= 0)write(lun, *)' segyhd%igi       ',segyhd%igi
if(segyhd%corr /= 0)write(lun, *)' segyhd%corr      ',segyhd%corr
if(segyhd%sfs /= 0)write(lun, *)' segyhd%sfs       ',segyhd%sfs
if(segyhd%sfe /= 0)write(lun, *)' segyhd%sfe       ',segyhd%sfe
if(segyhd%slen /= 0)write(lun, *)' segyhd%slen      ',segyhd%slen
if(segyhd%styp /= 0)write(lun, *)' segyhd%styp      ',segyhd%styp
if(segyhd%stas /= 0)write(lun, *)' segyhd%stas      ',segyhd%stas
if(segyhd%stae /= 0)write(lun, *)' segyhd%stae      ',segyhd%stae
if(segyhd%tatyp /= 0)write(lun, *)' segyhd%tatyp     ',segyhd%tatyp
if(segyhd%afilf /= 0)write(lun, *)' segyhd%afilf     ',segyhd%afilf
if(segyhd%afils /= 0)write(lun, *)' segyhd%afils     ',segyhd%afils
if(segyhd%nofilf /= 0)write(lun, *)' segyhd%nofilf    ',segyhd%nofilf
if(segyhd%nofils /= 0)write(lun, *)' segyhd%nofils    ',segyhd%nofils
if(segyhd%lcf /= 0)write(lun, *)' segyhd%lcf       ',segyhd%lcf
if(segyhd%hcf /= 0)write(lun, *)' segyhd%hcf       ',segyhd%hcf
if(segyhd%lcs /= 0)write(lun, *)' segyhd%lcs       ',segyhd%lcs
if(segyhd%hcs /= 0)write(lun, *)' segyhd%hcs       ',segyhd%hcs
if(segyhd%year /= 0)write(lun, *)' segyhd%year      ',segyhd%year
if(segyhd%day /= 0)write(lun, *)' segyhd%day       ',segyhd%day
if(segyhd%hour /= 0)write(lun, *)' segyhd%hour      ',segyhd%hour
if(segyhd%minute /= 0)write(lun, *)' segyhd%minute    ',segyhd%minute
if(segyhd%sec /= 0)write(lun, *)' segyhd%sec       ',segyhd%sec
if(segyhd%timbas /= 0)write(lun, *)' segyhd%timbas    ',segyhd%timbas
if(segyhd%trwf /= 0)write(lun, *)' segyhd%trwf      ',segyhd%trwf
if(segyhd%grnors /= 0)write(lun, *)' segyhd%grnors    ',segyhd%grnors
if(segyhd%grnofr /= 0)write(lun, *)' segyhd%grnofr    ',segyhd%grnofr
if(segyhd%grnlof /= 0)write(lun, *)' segyhd%grnlof    ',segyhd%grnlof
if(segyhd%gaps /= 0)write(lun, *)' segyhd%gaps      ',segyhd%gaps
if(segyhd%otrav /= 0)write(lun, *)' segyhd%otrav     ',segyhd%otrav
!
! No more conoco segy format.
! ehs 01nov01
!
!     if(segyhd%conid == '*CONOCO*') then
!       write(lun, *)' segyhd%conid     ',segyhd%conid
!       if(segyhd%lav /= 0)write(lun, *)' segyhd%lav       ',segyhd%lav
!       if(segyhd%mp_xloc /= 0)write(lun, *)' segyhd%mp_xloc   ', &
!          segyhd%mp_xloc
!       if(segyhd%mp_yloc /= 0)write(lun, *)' segyhd%mp_yloc   ', &
!          segyhd%mp_yloc
!       if(segyhd%mp_elev /= 0)write(lun, *)' segyhd%mp_elev   ', &
!          segyhd%mp_elev
!       if(segyhd%mp_xgrid /= 0)write(lun, *)' segyhd%mp_xgrid  ', &
!          segyhd%mp_xgrid
!       if(segyhd%mp_ygrid /= 0)write(lun, *)' segyhd%mp_ygrid  ', &
!          segyhd%mp_ygrid
!     endif
!
! Print the unused portion of segy header prior to last word
!   wjd: 2003/8/11
if(segyhd%unused(1)/= 0)write(lun, *)' segyhd%mp_unused(1) ', &
 segyhd%unused(1)
if(segyhd%unused(2)/= 0)write(lun, *)' segyhd%mp_unused(2) ', &
 segyhd%unused(2)
if(segyhd%unused(3)/= 0)write(lun, *)' segyhd%mp_unused(3) ', &
 segyhd%unused(3)
if(segyhd%unused(4)/= 0)write(lun, *)' segyhd%mp_unused(4) ', &
 segyhd%unused(4)
if(segyhd%unused(5)/= 0)write(lun, *)' segyhd%mp_unused(5) ', &
 segyhd%unused(5)
if(segyhd%unused(6)/= 0)write(lun, *)' segyhd%mp_unused(6) ', &
 segyhd%unused(6)
if(segyhd%unused(7)/= 0)write(lun, *)' segyhd%mp_unused(7) ', &
 segyhd%unused(7)
if(segyhd%unused(8)/= 0)write(lun, *)' segyhd%mp_unused(8) ', &
 segyhd%unused(8)
if(segyhd%unused(9)/= 0)write(lun, *)' segyhd%mp_unused(9) ', &
 segyhd%unused(9)
if(segyhd%unused(10)/= 0)write(lun, *)' segyhd%mp_unused(10) ', &
 segyhd%unused(10)
if(segyhd%unused(11)/= 0)write(lun, *)' segyhd%mp_unused(11) ', &
 segyhd%unused(11)
if(segyhd%unused(12)/= 0)write(lun, *)' segyhd%mp_unused(12) ', &
 segyhd%unused(12)
if(segyhd%unused(13)/= 0)write(lun, *)' segyhd%mp_unused(13) ', &
 segyhd%unused(13)
if(segyhd%unused(14)/= 0)write(lun, *)' segyhd%mp_unused(14) ', &
 segyhd%unused(14)
if(segyhd%unused1/= 0)write(lun, *)' segyhd%mp_unused1 ',segyhd%unused1

end subroutine segy_print_trace_hd

subroutine segy_print_binhdr(binhdr, lun_in)

type(segy_bin_hdr), intent(in) :: binhdr
integer, intent(in), optional  :: lun_in

integer                        :: lun

if (present(lun_in)) then
lun = lun_in
else
lun = 6
endif

if(binhdr%jobid /= 0 ) write(lun, *)' binhdr%jobid      = ', &
binhdr%jobid
if(binhdr%lino /= 0 ) write(lun, *)' binhdr%lino       = ', &
binhdr%lino
if(binhdr%reno /= 0 ) write(lun, *)' binhdr%reno       = ', &
binhdr%reno
if(binhdr%ntrpr /= 0 ) write(lun, *)' binhdr%ntrpr      = ', &
binhdr%ntrpr
if(binhdr%nart /= 0 ) write(lun, *)' binhdr%nart       = ', &
binhdr%nart
if(binhdr%hdt /= 0 ) write(lun, *)' binhdr%hdt        = ', &
binhdr%hdt
if(binhdr%dto /= 0 ) write(lun, *)' binhdr%dto        = ', &
binhdr%dto
if(binhdr%hns /= 0 ) write(lun, *)' binhdr%hns        = ', &
binhdr%hns
if(binhdr%nso /= 0 ) write(lun, *)' binhdr%nso        = ', &
binhdr%nso
if(binhdr%format /= 0 ) write(lun, *)' binhdr%format     = ', &
binhdr%format
if(binhdr%fold /= 0 ) write(lun, *)' binhdr%fold       = ', &
binhdr%fold
if(binhdr%tsort /= 0 ) write(lun, *)' binhdr%tsort      = ', &
binhdr%tsort
if(binhdr%vscode /= 0 ) write(lun, *)' binhdr%vscode     = ', &
binhdr%vscode
if(binhdr%hsfs /= 0 ) write(lun, *)' binhdr%hsfs       = ', &
binhdr%hsfs
if(binhdr%hsfe /= 0 ) write(lun, *)' binhdr%hsfe       = ', &
binhdr%hsfe
if(binhdr%hslen /= 0 ) write(lun, *)' binhdr%hslen      = ', &
binhdr%hslen
if(binhdr%hstyp /= 0 ) write(lun, *)' binhdr%hstyp      = ', &
binhdr%hstyp
if(binhdr%schn /= 0 ) write(lun, *)' binhdr%schn       = ', &
binhdr%schn
if(binhdr%hstas /= 0 ) write(lun, *)' binhdr%hstas      = ', &
binhdr%hstas
if(binhdr%hstae /= 0 ) write(lun, *)' binhdr%hstae      = ', &
binhdr%hstae
if(binhdr%htatyp /= 0 ) write(lun, *)' binhdr%htatyp     = ', &
binhdr%htatyp
if(binhdr%hcorr /= 0 ) write(lun, *)' binhdr%hcorr      = ', &
binhdr%hcorr
if(binhdr%bgrcv /= 0 ) write(lun, *)' binhdr%bgrcv      = ', &
binhdr%bgrcv
if(binhdr%rcvm /= 0 ) write(lun, *)' binhdr%rcvm       = ', &
binhdr%rcvm
if(binhdr%mfeet /= 0 ) write(lun, *)' binhdr%mfeet      = ', &
binhdr%mfeet
if(binhdr%polyt /= 0 ) write(lun, *)' binhdr%polyt      = ', &
binhdr%polyt
if(binhdr%vpol /= 0 ) write(lun, *)' binhdr%vpol       = ', &
binhdr%vpol
!
! No more conoco segy format.
! ehs 01nov01
!
!   if(binhdr%conid == '*CONOCO*' )then
!     write(lun, *)' binhdr%conid      = ',binhdr%conid
!     if(binhdr%tstart /= 0 ) write(lun, *)' binhdr%tstart     = ', &
!      binhdr%tstart
!     if(binhdr%xorigin /= 0 ) write(lun, *)' binhdr%xorigin    = ', &
!        binhdr%xorigin
!     if(binhdr%yorigin /= 0 ) write(lun, *)' binhdr%yorigin    = ', &
!        binhdr%yorigin
!     if(binhdr%dx11 /= 0 ) write(lun, *)' binhdr%dx11       = ', &
!      binhdr%dx11
!     if(binhdr%dx12 /= 0 ) write(lun, *)' binhdr%dx12       = ', &
!      binhdr%dx12
!     if(binhdr%dx21 /= 0 ) write(lun, *)' binhdr%dx21       = ', &
!      binhdr%dx21
!     if(binhdr%dx22 /= 0 ) write(lun, *)' binhdr%dx22       = ', &
!      binhdr%dx22
!     if(binhdr%nbits /= 0 ) write(lun, *)' binhdr%nbits     = ', &
!      binhdr%nbits
!     if(binhdr%endian /= 0 ) write(lun, *)' binhdr%endian     = ', &
!      binhdr%endian
!   endif
end subroutine segy_print_binhdr

subroutine &
segy_map_segy_to_cps (segyhd,nummap,nbytes,sbyte,cpsmap,wtype,cpshd)
integer,intent(in ),dimension(:)          :: segyhd
integer,intent(in )                       :: nummap
integer,intent(in ),dimension(:)          :: nbytes
integer,intent(in ),dimension(:)          :: sbyte
integer,intent(in ),dimension(:)          :: cpsmap
integer,intent(in ),dimension(:)          :: wtype
double precision,intent(out),dimension(:) :: cpshd

call segy_map_segy_to_cps_c(segyhd(1),nummap,nbytes(1),sbyte(1),&
 cpsmap(1),wtype(1),cpshd(1))

end subroutine segy_map_segy_to_cps

subroutine &
segy_map_cps_to_segy (segyhd,nummap,nbytes,sbyte,cpsmap,wtype,cpshd)
integer,intent(out),dimension(:)          :: segyhd
integer,intent(in )                       :: nummap
integer,intent(in ),dimension(:)          :: nbytes
integer,intent(in ),dimension(:)          :: sbyte
integer,intent(in ),dimension(:)          :: cpsmap
    integer,intent(in ),dimension(:)          :: wtype
    double precision,intent(in ),dimension(:) :: cpshd

    call segy_map_cps_to_segy_c(segyhd(1),nummap,nbytes(1),sbyte(1),&
         cpsmap(1),wtype(1),cpshd(1))

  end subroutine segy_map_cps_to_segy

      subroutine segy_hdrdump(flush,syh)



      type(segy_trc_hdr),optional, intent(in) :: syh
      logical, intent(inout) :: flush

      integer,parameter :: ncol=10

      integer,save :: knt=0,kntall=1
      integer      :: i,j,k,endbyt(86),startbyt(86)

      character(len=24),save :: legend(86)
      character(len=80),save :: fmt1

      integer,save :: ibuf(ncol,86)
      logical,save :: first=.true.

      data fmt1/'(1x,i3,"-",i3,10i10,1x,T111,A)'/
      data startbyt /1,5,9,13,17,21,25,29,31,33,35,37,41,45,49,53,57,61,65,&
                     69,71,73,77,81,85,89,91,93,95,97,99,101,103,105,107,109,&
                     111,113,115,117,119,121,123,125,127,129,131,133,135,137,&
                     139,141,143,145,147,149,151,153,155,157,159,161,163,165,&
                     167,169,171,173,175,177,179,181,185,189,193,197,201,&
                     205,209,213,217,221,225,229,233,237/

      data endbyt /4,8,12,16,20,24,28,30,32,34,36,40,44,48,52,56,60,64,68,70,&
                72,76,80,84,88,90,92,94,96,98,100,102,104,106,108,110,112,114,&
                   116,118,120,122,124,126,128,130,132,134,136,138,140,142,&
                   144,146,148,150,152,154,156,158,160,162,164,166,168,170,&
                   172,174,176,178,180,184,188,192,196,200,204,208,212,216,&
                   220,224,228,232,236,240/

      data legend/'trc# line','trc# reel','record#','trc# in rec',&
                  &'source','cdp','trc# in cpd','trc id','nvs','nhs',&
                  &'duse','offset','rec elev','source elev','source depth',&
                  &'datum elev rec','datum elev source','water depth sor',&
                  &'water depth grp','scaler elev','scaler coord',&
                  &'source x','source y','rec x','rec y','coord units',&
                  &'vel wea','vel subwea','uhtime source','uhtime grp',&
                  &'static source','static grp','static total','lag time A',&
                  &'lag time B','delay','mute start','mute end','num samples',&
                  &'dt','gain','igc','igi','corr','sfs','sfe','slen',&
                  &'stype','stas','stae','afilf','afils','nofilf','nofils',&
                  &'lcf','hcf','lcs','hcs','year','day','hour','minute',&
                  &'second','timebas','trwf','grnors','grnlof','grnofr',&
                  &'grnlof','gaps','otrav',15*'unused'/

      if(present(syh))flush=.false.
      if(flush)then
        k=mod(knt,10)
        if(k.eq.0)return
        write(fmt1(15:16),'(i2)')knt
        go to 100
      endif

      if(first)then
         knt=0
         ibuf=0
      endif
      first=.false.
      knt=knt+1

    if(knt.le.ncol)then
      ibuf(knt,1 )=syh%tracl
      ibuf(knt,2 )=syh%tracr
      ibuf(knt,3 )=syh%fldr
      ibuf(knt,4 )=syh%tracf
      ibuf(knt,5  )=syh%ep
      ibuf(knt,6  )=syh%cdp
      ibuf(knt,7  )=syh%cdpt
      ibuf(knt,8  )=syh%trid
      ibuf(knt,9  )=syh%nvs
      ibuf(knt,10 )=syh%nhs
      ibuf(knt,11 )=syh%duse
      ibuf(knt,12  )=syh%offset
      ibuf(knt,13  )=syh%gelev
      ibuf(knt,14  )=syh%selev
      ibuf(knt,15  )=syh%sdepth
      ibuf(knt,16  )=syh%gdel
      ibuf(knt,17  )=syh%sdel
      ibuf(knt,18  )=syh%swdep
      ibuf(knt,19  )=syh%gwdep
      ibuf(knt,20  )=syh%scalel
      ibuf(knt,21  )=syh%scalco
      ibuf(knt,22  )=syh%sx
      ibuf(knt,23  )=syh%sy
      ibuf(knt,24  )=syh%gx
      ibuf(knt,25  )=syh%gy
      ibuf(knt,26  )=syh%counit
      ibuf(knt,27  )=syh%wevel
      ibuf(knt,28  )=syh%swevel
      ibuf(knt,29  )=syh%sut
      ibuf(knt,30  )=syh%gut
      ibuf(knt,31  )=syh%sstat
      ibuf(knt,32  )=syh%gstat
      ibuf(knt,33  )=syh%tstat
      ibuf(knt,34  )=syh%laga
      ibuf(knt,35  )=syh%lagb
      ibuf(knt,36  )=syh%delrt
      ibuf(knt,37  )=syh%muts
      ibuf(knt,38  )=syh%mute
      ibuf(knt,39  )=syh%ns
      ibuf(knt,40  )=syh%dt
      ibuf(knt,41  )=syh%gain
      ibuf(knt,42  )=syh%igc
      ibuf(knt,43  )=syh%igi
      ibuf(knt,44  )=syh%corr
      ibuf(knt,45  )=syh%sfs
      ibuf(knt,46  )=syh%sfe
      ibuf(knt,47  )=syh%slen
      ibuf(knt,48  )=syh%styp
      ibuf(knt,49  )=syh%stas
      ibuf(knt,50  )=syh%stae
      ibuf(knt,51  )=syh%tatyp
      ibuf(knt,52  )=syh%afilf
      ibuf(knt,53  )=syh%afils
      ibuf(knt,54  )=syh%nofilf
      ibuf(knt,55  )=syh%nofils
      ibuf(knt,56  )=syh%lcf
      ibuf(knt,57  )=syh%hcf
      ibuf(knt,58  )=syh%lcs
      ibuf(knt,59 )=syh%hcs
      ibuf(knt,60 )=syh%year
      ibuf(knt,61 )=syh%day
      ibuf(knt,62 )=syh%hour
      ibuf(knt,63 )=syh%minute
      ibuf(knt,64 )=syh%sec
      ibuf(knt,65 )=syh%timbas
      ibuf(knt,66 )=syh%trwf
      ibuf(knt,67 )=syh%grnors
      ibuf(knt,68 )=syh%grnofr
      ibuf(knt,69 )=syh%grnlof
      ibuf(knt,70 )=syh%gaps
      ibuf(knt,71 )=syh%otrav
      ibuf(knt,72 )=syh%unused(1)
      ibuf(knt,73 )=syh%unused(2)
      ibuf(knt,74 )=syh%unused(3)
      ibuf(knt,75 )=syh%unused(4)
      ibuf(knt,76 )=syh%unused(5)
      ibuf(knt,77 )=syh%unused(6)
      ibuf(knt,78 )=syh%unused(7)
      ibuf(knt,79 )=syh%unused(8)
      ibuf(knt,80 )=syh%unused(9)
      ibuf(knt,81 )=syh%unused(10)
      ibuf(knt,82 )=syh%unused(11)
      ibuf(knt,83 )=syh%unused(12)
      ibuf(knt,84 )=syh%unused(13)
      ibuf(knt,85 )=syh%unused(14)
      ibuf(knt,86 )=syh%unused1

    endif

 100  continue
      j=mod(knt,ncol)
      if(j.eq.0.or.flush)then
        print 9002,(j,j=kntall,kntall+ncol-1)
        print*,' '
        kntall=kntall+ncol
        do i=1,86
          print fmt1,startbyt(i),endbyt(i),(ibuf(j,i),j=1,knt),legend(i)
        enddo
        first=.true.
        print*,' '
        print*,' '
      endif

 9002 format(8x,10i10)
    end subroutine segy_hdrdump
end module segy_module
