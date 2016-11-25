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
!!---------------------------- jsf_wrapper.f90 ----------------------------!!
!!---------------------------- jsf_wrapper.f90 ----------------------------!!
!!---------------------------- jsf_wrapper.f90 ----------------------------!!

           ! other files are:  jsf_crou.c  jswrapper.c jswrapper.h



!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : JSF_WRAPPER
! Category   : io
! Written    : 2006-08-22   by: Corn
! Revised    : 2008-11-06   by: Corn
! Maturity   : beta
! Purpose    : Interface between jsf_wrapper.f90 and jswrapper.c
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!          o                            i
!         num   = jsf_wrapper_create (count)
!
!          o                               i      i
!         lun   = jsf_wrapper_getlun (filename, rw)
!
!          o                         i
!          ok   = jsf_wrapper_open (lun)
!
!          o                          i
!         stat  = jsf_wrapper_close (lun)
!
!          o                        i
!         isa   = jsf_wrapper_isa (lun)
!
!          o                           i
!         stat  = jsf_wrapper_status (lun)
!
!          o                            i    b       i
!         len   = jsf_wrapper_message (lun, mess, max_size)
!
!          o                          i     b       i
!         len   = jsf_wrapper_wtype (lun, wtype, max_size)
!
!          o                                  i
!         tcnt  = jsf_wrapper_gettracecount (lun)
!
!          o                                     i
!         ndim  = jsf_wrapper_getnumdimensions (lun)
!
!          o                                   i
!         scnt  = jsf_wrapper_getsamplecount (lun)
!
!           o                                 i
!         srate = jsf_wrapper_getsamplerate (lun)
!
!          o                           i
!         lav   = jsf_wrapper_getlav (lun)
!
!           o                                      i
!         tstrt = jsf_wrapper_getstarttimeinsecs (lun)
!
!          o                                   i
!         hcnt  = jsf_wrapper_getheadercount (lun)
!
!          o                             i     b       i
!         len   = jsf_wrapper_gettrace (lun, trace, max_size)
!
!          o                               i      b        i
!         len   = jsf_wrapper_getheaders (lun, headers, max_size)
!
!          o                             i     i     i
!         scnt  = jsf_wrapper_puttrace (lun, trace, size)
!
!          o                               i      i      i
!         hcnt  = jsf_wrapper_putheaders (lun, headers, size)
!
!         o                                    i       i
!         ok    = jsf_wrapper_settracenumber (lun, trace_num)
!
!          o                                   i
!         tnum  = jsf_wrapper_gettracenumber (lun)
!
!          o                            i    i      i       i       i
!          ok   = jsf_wrapper_setaxis (lun, idim, length, domain, units,
!                                           i            i            i
!                                      logical_org, logical_del, physical_org,
!                                           i
!                                      physical_del)
!
!          o                                i    i
!          ok   = jsf_wrapper_setdatatype (lun, type)
!
!          o                                   i     i
!          ok   = jsf_wrapper_settraceformat (lun, format)
!
!          o                           i
!          ok   = jsf_wrapper_remove (lun)
!
!                                      i
!         call    jsf_wrapper_delete (lun)
!
! integer                 num       = returned number of JavaSeisFiles
!                                     possible
! integer                 count     = given number of JavaSeisFiles desired
! integer                 lun       = logical unit of JavaSeisFile opened
! character(len=*)        filename  = name of JavaSeisFile to initialize
! character(len=*)        rw        = read: "r" or read/write: "rw"
! integer                 stat      = returned status of JavaSeisFile (0 = OK)
! integer                 isa       = returned 0 if file is invalid
!                                     JavaSeisFile
! integer                 len       = returned length of array
! character(len=*)        mess      = populated error message
! integer                 max_size  = given size for which array is allocated
! character(len=*)        wtype     = populated wtype code (e.g. "JSEIS")
! integer(kind=8)         tcnt      = returned number of traces in
!                                     JavaSeisFile
! integer                 ndim      = returned number of dimension in
!                                     JavaSeisFile
! integer                 scnt      = returned number of samples in
!                                     JavaSeisFile
! real                    srate     = returned sample rate for JavaSeisFile
! double precision        lav       = returned largest amp value (est) in
!                                     JavaSeisFile
! double precision        tstrt     = returned start of trace in seconds
! integer                 hcnt      = returned number of headers for each
!                                     trace
! integer(kind=8)         trace_num = given trace number (1-relative)
! integer(kind=8)         tnum      = returned trace number (1-relative)
! real(len=*)             trace     = populated/given trace array
! double precision(len=*) headers   = populated/given header array
! integer                 size      = given size of input array
! integer                 ok        = 1 for normal return, 0 otherwise
! integer                 idim      = given axis dimension (1-relative)
! character(len=*)        domain    = given axis domain
! character(len=*)        units     = given axis units
! integer(kind=8)       logical_org = given axis logical origin
! integer(kind=8)       logical_del = given axis logical delta
! double precision      physical_org = given axis physical origin
! double precision      physical_del = given axis physical delta
! integer                 length    = given number of coordinates in axis
! character(len=*)        type      = given data type
! character(len=*)        format    = given trace format
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  4. 2008-11-06  Corn       Added gettracenumber
!  3. 2008-09-02  Corn       Added settracenumber
!  2. 2008-08-21  Corn       Added writing capability.
!  1. 2006-08-22  Corn       Initial version.
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


      module jsf_wrapper_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: XXXX_IDENT = &
'$Id: jsf_wrapper.f90,v 1.2 2008/11/19 19:18:25 mengewm Exp $'

!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!



!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      function jsf_wrapper_create (count) result (num)
      integer                   ,intent(in)    :: count          ! arguments
      integer                                  :: num            ! result
      integer                   :: jsf_crou_create               ! local

      num = jsf_crou_create (count)
      end function jsf_wrapper_create


!!------------------------------ getlun -----------------------------------!!
!!------------------------------ getlun -----------------------------------!!
!!------------------------------ getlun -----------------------------------!!


      function jsf_wrapper_getlun (filename, rw) result (lun)
      character(len=*)          ,intent(in)    :: filename       ! arguments
      character(len=*)          ,intent(in)    :: rw             ! arguments
      integer                                  :: lun            ! result
      integer                   :: jsf_crou_getlun               ! local
      integer                   :: fnh(100)                      ! local
      integer                   :: rwh(4)                        ! local

      call string_cc2hh (filename, fnh)     ! because filename intent has in
      call string_cc2hh (rw, rwh)           ! because rw intent has in
      lun = jsf_crou_getlun (fnh, rwh)
      end function jsf_wrapper_getlun


!!------------------------------- open ------------------------------------!!
!!------------------------------- open ------------------------------------!!
!!------------------------------- open ------------------------------------!!


      function jsf_wrapper_open (lun) result (ok)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                                  :: ok             ! result
      integer                   :: jsf_crou_open                 ! local

      ok = jsf_crou_open (lun)
      end function jsf_wrapper_open


!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!


      function jsf_wrapper_close (lun) result (stat)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                                  :: stat           ! result
      integer                   :: jsf_crou_close                ! local

      stat = jsf_crou_close (lun)
      end function jsf_wrapper_close


!!------------------------- is a JavaSeisFile ------------------------------!!
!!------------------------- is a JavaSeisFile ------------------------------!!
!!------------------------- is a JavaSeisFile ------------------------------!!


      function jsf_wrapper_isa (lun) result (isa)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                                  :: isa            ! result
      integer                   :: jsf_crou_isa                  ! local

      isa = jsf_crou_isa (lun)
      end function jsf_wrapper_isa


!!---------------------------- get status ----------------------------------!!
!!---------------------------- get status ----------------------------------!!
!!---------------------------- get status ----------------------------------!!


      function jsf_wrapper_status (lun) result (stat)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                                  :: stat           ! result
      integer                   :: jsf_crou_status               ! local

      stat = jsf_crou_status (lun)
      end function jsf_wrapper_status


!!--------------------------- get message ----------------------------------!!
!!--------------------------- get message ----------------------------------!!
!!--------------------------- get message ----------------------------------!!


      function jsf_wrapper_message (lun, mess, max_size) result (len)
      integer                   ,intent(in)    :: lun            ! arguments
      character(len=*)          ,intent(inout) :: mess           ! arguments
      integer                   ,intent(in)    :: max_size       ! arguments
      integer                                  :: len            ! result
      integer                   :: messh(max_size/4 + 1)         ! local
      integer                   :: jsf_crou_message              ! local

      call string_cc2hh (mess, messh)     ! because mess intent has in
      len = jsf_crou_message (lun, messh, max_size)
      call string_hh2cc (messh, mess)     ! because mess intent has out
      end function jsf_wrapper_message


!!---------------------------- get wtype ----------------------------------!!
!!---------------------------- get wtype ----------------------------------!!
!!---------------------------- get wtype ----------------------------------!!


      function jsf_wrapper_wtype (lun, wtype, max_size) result (len)
      integer                   ,intent(in)    :: lun            ! arguments
      character(len=*)          ,intent(inout) :: wtype          ! arguments
      integer                   ,intent(in)    :: max_size       ! arguments
      integer                                  :: len            ! result
      integer                   :: wtypeh(max_size/4 + 1)        ! local
      integer                   :: jsf_crou_wtype                ! local

      call string_cc2hh (wtype, wtypeh)     ! because wtype intent has in
      len = jsf_crou_wtype (lun, wtypeh, max_size)
      call string_hh2cc (wtypeh, wtype)     ! because wtype intent has out
      end function jsf_wrapper_wtype


!!------------------------- get trace count --------------------------------!!
!!------------------------- get trace count --------------------------------!!
!!------------------------- get trace count --------------------------------!!


      function jsf_wrapper_gettracecount (lun) result (tcnt)
      integer                   ,intent(in)    :: lun            ! arguments
      integer(kind=8)                          :: tcnt           ! result
      integer(kind=8)           :: jsf_crou_gettracecount        ! local

      tcnt = jsf_crou_gettracecount (lun)
      end function jsf_wrapper_gettracecount


!!---------------------- get number of dimensions --------------------------!!
!!---------------------- get number of dimensions --------------------------!!
!!---------------------- get number of dimensions --------------------------!!


      function jsf_wrapper_getnumdimensions (lun) result (ndim)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                                  :: ndim           ! result
      integer                   :: jsf_crou_getnumdimensions     ! local

      ndim = jsf_crou_getnumdimensions (lun)
      end function jsf_wrapper_getnumdimensions


!!------------------------- get sample count --------------------------------!!
!!------------------------- get sample count --------------------------------!!
!!------------------------- get sample count --------------------------------!!


      function jsf_wrapper_getsamplecount (lun) result (scnt)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                                  :: scnt           ! result
      integer                   :: jsf_crou_getsamplecount       ! local

      scnt = jsf_crou_getsamplecount (lun)
      end function jsf_wrapper_getsamplecount


!!------------------------- get sample rate --------------------------------!!
!!------------------------- get sample rate --------------------------------!!
!!------------------------- get sample rate --------------------------------!!


      function jsf_wrapper_getsamplerate (lun) result (srate)
      integer                   ,intent(in)    :: lun            ! arguments
      real                                     :: srate          ! result
      real                      :: jsf_crou_getsamplerate        ! local

      srate = jsf_crou_getsamplerate (lun)
      end function jsf_wrapper_getsamplerate


!!------------------- get largest amplitude value --------------------------!!
!!------------------- get largest amplitude value --------------------------!!
!!------------------- get largest amplitude value --------------------------!!


      function jsf_wrapper_getlav (lun) result (lav)
      integer                   ,intent(in)    :: lun            ! arguments
      double precision                         :: lav            ! result
      double precision          :: jsf_crou_getlav               ! local

      lav = jsf_crou_getlav (lun)
      end function jsf_wrapper_getlav


!!-------------------- get start time in seconds ---------------------------!!
!!-------------------- get start time in seconds ---------------------------!!
!!-------------------- get start time in seconds ---------------------------!!


      function jsf_wrapper_getstarttimeinsecs (lun) result (tstrt)
      integer                   ,intent(in)    :: lun            ! arguments
      double precision                         :: tstrt          ! result
      double precision          :: jsf_crou_getstarttimeinsecs   ! local

      tstrt = jsf_crou_getstarttimeinsecs (lun)
      end function jsf_wrapper_getstarttimeinsecs


!!------------------------- get header count --------------------------------!!
!!------------------------- get header count --------------------------------!!
!!------------------------- get header count --------------------------------!!


      function jsf_wrapper_getheadercount (lun) result (hcnt)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                                  :: hcnt           ! result
      integer                   :: jsf_crou_getheadercount       ! local

      hcnt = jsf_crou_getheadercount (lun)
      end function jsf_wrapper_getheadercount



!!--------------------------- get a trace ----------------------------------!!
!!--------------------------- get a trace ----------------------------------!!
!!--------------------------- get a trace ----------------------------------!!


      function jsf_wrapper_gettrace (lun, trace, max_size) &
        result (len)
      integer                   ,intent(in)    :: lun            ! arguments
      real, dimension(max_size) ,intent(inout) :: trace          ! arguments
      integer                   ,intent(in)    :: max_size       ! arguments
      integer                                  :: len            ! result
      integer                   :: jsf_crou_gettrace             ! local

      len = jsf_crou_gettrace (lun, trace, max_size)
      end function jsf_wrapper_gettrace


!!--------------------- get headers of a trace -----------------------------!!
!!--------------------- get headers of a trace -----------------------------!!
!!--------------------- get headers of a trace -----------------------------!!


      function jsf_wrapper_getheaders (lun, headers, max_size) &
        result (len)
      integer                   ,intent(in)    :: lun            ! arguments
      double precision, dimension(max_size),intent(inout) :: headers ! args
      integer                   ,intent(in)    :: max_size       ! arguments
      integer                                  :: len            ! result
      integer                   :: jsf_crou_getheaders           ! local

      len = jsf_crou_getheaders (lun, headers, max_size)
      end function jsf_wrapper_getheaders


!!-------------------------- write a trace ---------------------------------!!
!!-------------------------- write a trace ---------------------------------!!
!!-------------------------- write a trace ---------------------------------!!


      function jsf_wrapper_puttrace (lun, trace, isize) &
        result (len)
      integer                   ,intent(in)    :: lun            ! arguments
      real, dimension(isize)    ,intent(in)    :: trace          ! arguments
      integer                   ,intent(in)    :: isize          ! arguments
      integer                                  :: len            ! result
      integer                   :: jsf_crou_puttrace             ! local

      len = jsf_crou_puttrace (lun, trace, isize)
      end function jsf_wrapper_puttrace


!!--------------------- write headers of a trace ---------------------------!!
!!--------------------- write headers of a trace ---------------------------!!
!!--------------------- write headers of a trace ---------------------------!!


      function jsf_wrapper_putheaders (lun, headers, isize) &
        result (len)
      integer                   ,intent(in)    :: lun            ! arguments
      double precision, dimension(isize),intent(in) :: headers   ! arguments
      integer                   ,intent(in)    :: isize          ! arguments
      integer                                  :: len            ! result
      integer                   :: jsf_crou_putheaders           ! local

      len = jsf_crou_putheaders (lun, headers, isize)
      end function jsf_wrapper_putheaders


!!------------------------ set a trace number ------------------------------!!
!!------------------------ set a trace number ------------------------------!!
!!------------------------ set a trace number ------------------------------!!


      function jsf_wrapper_settracenumber (lun, trace_num) result (ok)
      integer                   ,intent(in)    :: lun            ! arguments
      integer(kind=8)           ,intent(in)    :: trace_num      ! arguments
      integer                                  :: ok             ! result
      integer                   :: jsf_crou_settracenumber       ! local

      ok = jsf_crou_settracenumber (lun, trace_num)
      end function jsf_wrapper_settracenumber


!!----------------------- get the trace number -----------------------------!!
!!----------------------- get the trace number -----------------------------!!
!!----------------------- get the trace number -----------------------------!!


      function jsf_wrapper_gettracenumber (lun) result (tnum)
      integer                   ,intent(in)    :: lun            ! arguments
      integer(kind=8)                          :: tnum           ! result
      integer(kind=8)           :: jsf_crou_gettracenumber       ! local

      tnum = jsf_crou_gettracenumber (lun)
      end function jsf_wrapper_gettracenumber


!!--------------------- set an axis description ----------------------------!!
!!--------------------- set an axis description ----------------------------!!
!!--------------------- set an axis description ----------------------------!!


      function jsf_wrapper_setaxis (lun, idim, length, domain, units, &
        logical_org, logical_del, physical_org, physical_del) result (ok)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                   ,intent(in)    :: idim           ! arguments
      integer                   ,intent(in)    :: length         ! arguments
      character(len=*)          ,intent(in)    :: domain         ! arguments
      character(len=*)          ,intent(in)    :: units          ! arguments
      integer(kind=8)           ,intent(in)    :: logical_org    ! arguments
      integer(kind=8)           ,intent(in)    :: logical_del    ! arguments
      double precision          ,intent(in)    :: physical_org   ! arguments
      double precision          ,intent(in)    :: physical_del   ! arguments
      integer                                  :: ok             ! result
      integer                   :: dh(100)                       ! local
      integer                   :: uh(100)                       ! local
      integer                   :: jsf_crou_setaxis              ! local

      call string_cc2hh (domain, dh)          ! because domain intent has in
      call string_cc2hh (units, uh)           ! because units intent has in
      ok = jsf_crou_setaxis (lun, idim, length, dh, uh, &
        logical_org, logical_del, physical_org, physical_del)
      end function jsf_wrapper_setaxis


!!--------------------------- set data type -------------------------------!!
!!--------------------------- set data type -------------------------------!!
!!--------------------------- set data type -------------------------------!!


      function jsf_wrapper_setdatatype (lun, type) result (ok)
      integer                   ,intent(in)    :: lun            ! arguments
      character(len=*)          ,intent(in)    :: type           ! arguments
      integer                                  :: ok             ! result
      integer                   :: th(100)                       ! local
      integer                   :: jsf_crou_setdatatype          ! local

      call string_cc2hh (type, th)          ! because type intent has in
      ok = jsf_crou_setdatatype (th)
      end function jsf_wrapper_setdatatype


!!-------------------------- set trace format -----------------------------!!
!!-------------------------- set trace format -----------------------------!!
!!-------------------------- set trace format -----------------------------!!


      function jsf_wrapper_settraceformat (lun, format) result (ok)
      integer                   ,intent(in)    :: lun            ! arguments
      character(len=*)          ,intent(in)    :: format         ! arguments
      integer                                  :: ok             ! result
      integer                   :: fh(100)                       ! local
      integer                   :: jsf_crou_settraceformat       ! local

      call string_cc2hh (format, fh)           ! because format intent has in
      ok = jsf_crou_settraceformat (fh)
      end function jsf_wrapper_settraceformat


!!------------------------------- remove ----------------------------------!!
!!------------------------------- remove ----------------------------------!!
!!------------------------------- remove ----------------------------------!!


      function jsf_wrapper_remove (lun) result (ok)
      integer                   ,intent(in)    :: lun            ! arguments
      integer                                  :: ok             ! result
      integer                   :: jsf_crou_remove               ! local

      ok = jsf_crou_remove (lun)
      end function jsf_wrapper_remove


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine jsf_wrapper_delete ()
      call jsf_crou_delete ()
      end subroutine jsf_wrapper_delete


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module jsf_wrapper_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
