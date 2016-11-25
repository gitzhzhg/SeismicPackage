!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- tftable.f90 ----------------------------------!!
!!---------------------------- tftable.f90 ----------------------------------!!
!!---------------------------- tftable.f90 ----------------------------------!!


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
! Name       : TFTABLE              (trace file table)
! Category   : sorts
! Written    : 2001-12-28   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Maintain and sort a table of contents for traces on a trace file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive maintains and sorts a table of contents for traces on a trace
! file.  It performs the following operations:
!
!  (1) It collects several header words for each trace written to a file.
!  (2) It can be sorted as requested.
!  (3) It provides the order in which traces should be read randomly
!       from the trace file.
!  (4) It can save itself to a file or read itself from a file.
!
! This primitive does not write traces to disk or read traces from disk.
! It only collects and provides information to allow the calling program
! to read or write traces.
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
! To open a new empty trace file table and fill it with information:
! (the sorted order will be the initial trace order)
!
!                                 o     i      o     i    
!       call tftable_open_write (obj,filename,msg,progname,
!                                headers,nheaders,hist,nhist)
!                                   i       i      i     i
!                                                 opt   opt
!
!                                 b  i   o
!       call tftable_add_trace  (obj,hd,msg)
!
!                                 b 
!       call tftable_close      (obj)
!
!-------------------------------------------------------------------------------
! To copy an existing trace file table to a new one, sorting the new one
! to the specified order:
!
!                                  i          i      o     i    
!       call tftable_copy_sort (filename1,filename2,msg,progname,
!                               hdr,init,inc,hist,nhist)
!                                i   i    i   i     i
!                                            opt   opt
!
!-------------------------------------------------------------------------------
! To read trace header word values from a trace file table:
!
!                                    i      o   i    o       o
!       call tftable_read_values (filename,msg,hdr,values,ntraces)
!
!-------------------------------------------------------------------------------
! To open an existing trace file table, sort it to the specified order, and
! use it for reading traces:
!
!                                  o     i      o     o   
!       call tftable_open_sort   (obj,filename,msg,ntraces,
!                                 hdr,init,inc)
!                                  i   i    i
!
!      o                           b   o 
!     irec = tftable_next_record (obj,msg)
!
!                                  b 
!       call tftable_close       (obj)
!
!-------------------------------------------------------------------------------
! To open an existing trace file table which is already sorted to the
! desired order, and use it for reading traces:
!
!                                  o     i      o     o   
!       call tftable_open_read   (obj,filename,msg,ntraces,
!                                 hdr,init,inc)
!                                  o   o    o
!
!      o                           b   o 
!     irec = tftable_next_record (obj,msg)
!
!                                  b 
!       call tftable_close       (obj)
!
!-------------------------------------------------------------------------------
! To verify that an existing trace file table can be read and the header
! and increment arguments are valid:
!
!                                     i      o   o    i
!           call tftable_checkout (filename,msg,info,hdr)
!
!-------------------------------------------------------------------------------
!                          SUBROUTINE ARGUMENTS
!
! type(tftable_struct)           obj = pointer to the data structure.
! character(len=*)          filename = name of file to read or save.
! character(len=*)          progname = name of program saving the file.
! integer          headers(nheaders) = list of header words to save.
! integer                   nheaders = number of header words to save.
! character(len=*)               msg = error message (blank if no error).
! character(len=*)              info = information about file contants.
! double precision             hd(:) = the trace header to add to the table.
! character(len=*)         filename1 = name of file to read.
! character(len=*)         filename2 = name of file to save.
! type(triplesort_ints)          hdr = header word numbers.
! type(triplesort_doubles)      init = centers of first (or any) bin.
! type(triplesort_doubles)       inc = bin increments (non-zero).
! type(triplesort_doubles) values(:) = pointer to trace values for all traces
!                                       on the file.
! integer                    ntraces = number of traces in the table.
! integer                       irec = record number of next trace to read
!                                      (-1 if no more traces or zero if error).
!
! FILENAME1 and FILENAME2 must be different.
!
! The table is sorted using HDR as the primary, secondary, and tertiary sort
! keys.  These three header words must be chosen from HEADERS(NHEADERS).  If
! fewer than three are needed, some of them can be duplicated or can be <= 1.
! It is recommended that HEADERS(NHEADERS) include at least header words
! 6 7 8 9 10.
!
! The table file will contain trace record numbers in the first column
! and HEADERS(1:NHEADERS) in the remaining columns.
!
! VALUES is a pointer which is allocated (or reallocated) to store trace
! header words values for NTRACES traces.  It must be nullified before first
! use and deallocated when no longer needed.  If an HDR header word does not
! match any header words on the file, the corresponding element in VALUES
! will be nil.
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
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!005. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  4. 2003-12-09  Stoeckley  Use double precision triplesort instead of real.
!  3. 2002-07-11  Stoeckley  Change routine tftable_read_values to return
!                             trace header values instead of bin numbers.
!  2. 2002-04-08  Stoeckley  Add routine tftable_read_values.
!  1. 2002-02-04  Stoeckley  Initial version.
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


      module tftable_module
      use triplesort_module
      use pjar_module
      use floatio_module
      use fioutil_module
      use named_constants_module
      implicit none
      public
      private :: tftable_private_create
      private :: tftable_private_open
      private :: tftable_private_sort

      character(len=100),public,save :: TFTABLE_IDENT = &
'$Id: tftable.f90,v 1.5 2006/10/17 13:45:48 Glover prod sps $'

      integer          ,parameter,private :: MAXCOLUMNS      = 30
      character(len=20),parameter,private :: DEFAULT_SECNAME = 'tftable'

      type,public :: tftable_struct              
      private

           integer              ,pointer  :: indices(:)
           type(floatio_struct) ,pointer  :: floatio
           integer                        :: ncolumns,ntraces,itrace
           integer                        :: hdrs(MAXCOLUMNS)

      end type tftable_struct

      contains


!!---------------------------- private create -----------------------------!!
!!---------------------------- private create -----------------------------!!
!!---------------------------- private create -----------------------------!!


      subroutine tftable_private_create (obj)
      implicit none
      type(tftable_struct),pointer :: obj                ! arguments

      allocate (obj)
      nullify  (obj%indices)
      nullify  (obj%floatio)

      obj%ncolumns  = 0
      obj%ntraces   = 0
      obj%itrace    = 0
      obj%hdrs(:)   = 0
      return
      end subroutine tftable_private_create


!!------------------------------ close -----------------------------------!!
!!------------------------------ close -----------------------------------!!
!!------------------------------ close -----------------------------------!!


      subroutine tftable_close (obj)
      implicit none
      type(tftable_struct),pointer :: obj       ! arguments

      if (.not.associated(obj)) return

      if (associated(obj%indices)) deallocate         (obj%indices)
      if (associated(obj%floatio)) call floatio_close (obj%floatio)

      deallocate(obj)
      return
      end subroutine tftable_close


!!----------------------------- open write --------------------------------!!
!!----------------------------- open write --------------------------------!!
!!----------------------------- open write --------------------------------!!

      subroutine tftable_open_write (obj,filename,msg,progname,  &
                                     headers,nheaders,hist,nhist)

      implicit none
      type(tftable_struct)     ,pointer     :: obj                ! arguments
      character(len=*)         ,intent(in)  :: filename           ! arguments
      character(len=*)         ,intent(out) :: msg                ! arguments
      character(len=*)         ,intent(in)  :: progname           ! arguments
      integer                  ,intent(in)  :: headers(:)         ! arguments
      integer                  ,intent(in)  :: nheaders           ! arguments
      character(len=*),optional,intent(in)  :: hist(:)            ! arguments
      integer         ,optional,intent(in)  :: nhist              ! arguments
      integer                               :: err                ! local
      character(len=80)                     :: hist2(1)           ! local
      type(pjar_struct)        ,pointer     :: pjar               ! local

      nullify (pjar) ! jpa
      call tftable_private_create (obj)

      obj%ncolumns             = nheaders + 1
      obj%hdrs(1)              = 0
      obj%hdrs(2:obj%ncolumns) = headers(1:nheaders)

      hist2(1) = 'sort order is the same as the order of the received traces'

      call pjar_create         (pjar)
      call pjar_choose_section (pjar,DEFAULT_SECNAME)
      call pjar_put            (pjar,'hdr_pri'   ,0     )
      call pjar_put            (pjar,'hdr_sec'   ,0     )
      call pjar_put            (pjar,'hdr_tert'  ,0     )
      call pjar_put            (pjar,'pri_init'  ,0.0   )
      call pjar_put            (pjar,'sec_init'  ,0.0   )
      call pjar_put            (pjar,'tert_init' ,0.0   )
      call pjar_put            (pjar,'pri_inc'   ,1.0   )
      call pjar_put            (pjar,'sec_inc'   ,1.0   )
      call pjar_put            (pjar,'tert_inc'  ,1.0   )
      call pjar_put            (pjar,'ncolumns'  ,obj%ncolumns   )
      call pjar_put            (pjar,'encoding'  ,FLOATIO_HYBRID )
      call pjar_put            (pjar,'hdrs'      ,obj%hdrs  ,obj%ncolumns)
      call pjar_choose_section (pjar,'history')
      call pjar_put_cards      (pjar,hist,nhist)
      call pjar_add_cards      (pjar,hist2,1,progname)

      call floatio_open_write &
                   (obj%floatio,filename,pjar,DEFAULT_SECNAME,err,msg)

      call pjar_delete (pjar)

      if (err == FLOATIO_OK) msg = ' '
      return
      end subroutine tftable_open_write


!!--------------------------- add trace -----------------------------------!!
!!--------------------------- add trace -----------------------------------!!
!!--------------------------- add trace -----------------------------------!!


      subroutine tftable_add_trace (obj,hd,msg)
      implicit none
      type(tftable_struct)  ,intent(inout) :: obj                 ! arguments
      double precision      ,intent(in)    :: hd(:)               ! arguments
      character(len=*)      ,intent(out)   :: msg                 ! arguments
      integer                              :: err                 ! local
      real                                 :: vline(MAXCOLUMNS)   ! local

      obj%itrace            = obj%itrace + 1
      vline(1)              = obj%itrace
      vline(2:obj%ncolumns) = hd(obj%hdrs(2:obj%ncolumns))

      call floatio_write_line (obj%floatio,err,msg,vline)
      if (err == FLOATIO_OK) msg = ' '
      return
      end subroutine tftable_add_trace


!!----------------------------- copy sort ----------------------------------!!
!!----------------------------- copy sort ----------------------------------!!
!!----------------------------- copy sort ----------------------------------!!

!!!! this method reads thru input file twice in order to avoid
!!!! storing columns 2++ in memory, since only column 1 is modified.


      subroutine tftable_copy_sort                      &
                             (filename1,filename2,msg,  &
                              progname,                 &
                              hdr,init,inc,             &
                              hist,nhist)
      implicit none
      character(len=*)         ,intent(in)  :: filename1            ! arguments
      character(len=*)         ,intent(in)  :: filename2            ! arguments
      character(len=*)         ,intent(out) :: msg                  ! arguments
      character(len=*)         ,intent(in)  :: progname             ! arguments
      type(triplesort_ints)    ,intent(in)  :: hdr                  ! arguments
      type(triplesort_doubles) ,intent(in)  :: init                 ! arguments
      type(triplesort_doubles) ,intent(in)  :: inc                  ! arguments
      character(len=*),optional,intent(in)  :: hist(:)              ! arguments
      integer         ,optional,intent(in)  :: nhist                ! arguments
      type(floatio_struct)     ,pointer     :: floatio1,floatio2    ! local
      integer                  ,pointer     :: indices(:)           ! local
      integer                               :: err,ntraces,ncolumns ! local
      integer                               :: indx                 ! local
      character(len=80)        ,pointer     :: phist2(:)            ! local
      integer                               :: nhist2               ! local
      integer                               :: hdrs (MAXCOLUMNS)    ! local
      real                                  :: vline(MAXCOLUMNS)    ! local
      type(pjar_struct)        ,pointer     :: pjar                 ! local
      integer                               :: ndummy               ! local

      nullify (floatio1) ! jpa
      nullify (floatio2) ! jpa
      nullify (indices) ! jpa
      nullify (pjar) ! jpa

!----------read input file and sort the table:

      if (filename1 == filename2) then
           msg = 'input and output filenames must be different'
           return
      end if

      call tftable_private_sort (filename1,msg,       &
                                 ntraces,indices,     &
                                 hdr,init,inc)
      if (msg /= ' ') return

!----------open input file:

      nullify (phist2)

      call pjar_create (pjar)

      call floatio_open_read &
                    (floatio1,filename1,pjar,DEFAULT_SECNAME,err,msg)

      call pjar_choose_section (pjar,DEFAULT_SECNAME)
      call pjar_get            (pjar,'ncolumns'  ,ncolumns  ,0 )
      call pjar_get            (pjar,'nlines'    ,ntraces   ,0 )
      call pjar_get            (pjar,'hdrs'      ,hdrs      ,ndummy)
      call pjar_choose_section (pjar,'history')
      call pjar_alloc_cards    (pjar,phist2,nhist2)
      call pjar_delete         (pjar)

      if (err /= FLOATIO_OK) then
           deallocate (indices)
           deallocate (phist2)
           return
      end if

!----------open output file:

      call pjar_create         (pjar)
      call pjar_choose_section (pjar,DEFAULT_SECNAME)
      call pjar_put            (pjar,'hdr_pri'   ,hdr%primary    )
      call pjar_put            (pjar,'hdr_sec'   ,hdr%secondary  )
      call pjar_put            (pjar,'hdr_tert'  ,hdr%tertiary   )
      call pjar_put            (pjar,'pri_init'  ,init%primary   )
      call pjar_put            (pjar,'sec_init'  ,init%secondary )
      call pjar_put            (pjar,'tert_init' ,init%tertiary  )
      call pjar_put            (pjar,'pri_inc'   ,inc%primary    )
      call pjar_put            (pjar,'sec_inc'   ,inc%secondary  )
      call pjar_put            (pjar,'tert_inc'  ,inc%tertiary   )
      call pjar_put            (pjar,'ncolumns'  ,ncolumns       )
      call pjar_put            (pjar,'encoding'  ,FLOATIO_HYBRID )
      call pjar_put            (pjar,'hdrs'      ,hdrs,  ncolumns)
      call pjar_choose_section (pjar,'history')
      call pjar_put_cards      (pjar,phist2,nhist2)
      call pjar_add_cards      (pjar,hist,nhist,progname)

      call floatio_open_write &
                    (floatio2,filename2,pjar,DEFAULT_SECNAME,err,msg)

      call pjar_delete (pjar)

      deallocate (phist2)

      if (err /= FLOATIO_OK) then
           call floatio_close (floatio1)
           deallocate         (indices)
           return
      end if

!----------copy input file to output file:

      msg = ' '
      do indx = 1,ntraces

           call floatio_read_line (floatio1,err,msg,vline)
           if (err /= FLOATIO_OK) exit

           vline(1) = indices(indx)

           call floatio_write_line (floatio2,err,msg,vline)
           if (err /= FLOATIO_OK) exit
           msg = ' '

      end do

!----------close files:

      call floatio_close (floatio1)
      call floatio_close (floatio2)
      deallocate         (indices)
      return
      end subroutine tftable_copy_sort


!!-------------------------- read values ---------------------------------!!
!!-------------------------- read values ---------------------------------!!
!!-------------------------- read values ---------------------------------!!


      subroutine tftable_read_values (filename,msg,hdr,values,ntraces)
      implicit none
      character(len=*)        ,intent(in)  :: filename            ! arguments
      character(len=*)        ,intent(out) :: msg                 ! arguments
      type(triplesort_ints)   ,intent(in)  :: hdr                 ! arguments
      type(triplesort_doubles),pointer     :: values(:)           ! arguments
      integer                 ,intent(out) :: ntraces             ! arguments
      type(floatio_struct)    ,pointer     :: floatio             ! local
      character(len=80)                    :: info                ! local
      type(triplesort_ints)                :: col                 ! local
      real                                 :: vline(MAXCOLUMNS)   ! local
      integer                              :: indx,err            ! local

      nullify (floatio) ! jpa
!----------get started:

      if (associated(values)) deallocate (values)

      call tftable_private_open (floatio,filename,msg,info,ntraces,hdr,col, &
                                                                 .false.)
      if (msg /= ' ') then
           call floatio_close (floatio)
           return
      end if

      allocate (values(ntraces))

!----------read the desired columns:

      do indx = 1,ntraces
           call floatio_read_line (floatio,err,msg,vline)
           if (err /= FLOATIO_OK) then
                call floatio_close (floatio)
                deallocate (values)
                return
           end if

           values(indx) = DNIL
           if (col%primary   > 0) values(indx)%primary   = vline(col%primary  )
           if (col%secondary > 0) values(indx)%secondary = vline(col%secondary)
           if (col%tertiary  > 0) values(indx)%tertiary  = vline(col%tertiary )
      end do

!----------close the trace file table:

      call floatio_close (floatio)
      msg  = ' '
      return
      end subroutine tftable_read_values


!!----------------------------- open sort --------------------------------!!
!!----------------------------- open sort --------------------------------!!
!!----------------------------- open sort --------------------------------!!


      subroutine tftable_open_sort (obj,filename,msg,           &
                                    ntraces,                    &
                                    hdr,init,inc)
      implicit none
      type(tftable_struct)    ,pointer     :: obj                 ! arguments
      character(len=*)        ,intent(in)  :: filename            ! arguments
      character(len=*)        ,intent(out) :: msg                 ! arguments
      integer                 ,intent(out) :: ntraces             ! arguments
      type(triplesort_ints)   ,intent(in)  :: hdr                 ! arguments
      type(triplesort_doubles),intent(in)  :: init                ! arguments
      type(triplesort_doubles),intent(in)  :: inc                 ! arguments

      call tftable_private_create (obj)
      call tftable_private_sort   (filename,msg,         &
                                   ntraces,obj%indices,  &
                                   hdr,init,inc)
      obj%ntraces = ntraces
      return
      end subroutine tftable_open_sort


!!----------------------------- checkout ------------------------------------!!
!!----------------------------- checkout ------------------------------------!!
!!----------------------------- checkout ------------------------------------!!

! opens and closes specified file.
! verifies the file and the subroutine arguments.


      subroutine tftable_checkout (filename,msg,info,hdr)
      implicit none
      character(len=*)     ,intent(in)  :: filename                ! arguments
      character(len=*)     ,intent(out) :: msg                     ! arguments
      character(len=*)     ,intent(out) :: info                    ! arguments
      type(triplesort_ints),intent(in)  :: hdr                     ! arguments
      type(floatio_struct) ,pointer     :: floatio                 ! local
      integer                           :: ntraces                 ! local
      type(triplesort_ints)             :: col                     ! local

      nullify (floatio) ! jpa
      call tftable_private_open (floatio,filename,msg,info,ntraces,hdr,col, &
                                                                 .true.)
      call floatio_close        (floatio)
      return
      end subroutine tftable_checkout


!!------------------------- private open ------------------------------------!!
!!------------------------- private open ------------------------------------!!
!!------------------------- private open ------------------------------------!!

! opens the specified existing input file.
! verifies the file.
! makes sure the file contains columns for the requested header words.
! returns column numbers for the requested header words.
! returns number of traces represented on the file.
! returns blank message if no error occurred.
! if force_match is false, the requested header words do not have to match.


      subroutine tftable_private_open (floatio,filename,msg,info,   &
                                       ntraces,hdr,col,force_match)
      implicit none
      type(floatio_struct) ,pointer     :: floatio                 ! arguments
      character(len=*)     ,intent(in)  :: filename                ! arguments
      character(len=*)     ,intent(out) :: msg                     ! arguments
      character(len=*)     ,intent(out) :: info                    ! arguments
      integer              ,intent(out) :: ntraces                 ! arguments
      type(triplesort_ints),intent(in)  :: hdr                     ! arguments
      type(triplesort_ints),intent(out) :: col                     ! arguments
      logical              ,intent(in)  :: force_match             ! arguments
      integer                           :: column,ncolumns,err     ! local
      integer                           :: hdrs (MAXCOLUMNS)       ! local
      type(triplesort_ints)             :: tmp                     ! local
      type(pjar_struct)    ,pointer     :: pjar                    ! local
      integer                           :: ndummy                  ! local

!----------open the trace file table:

      nullify (pjar) ! jpa
      call pjar_create (pjar)

      call floatio_open_read &
                  (floatio,filename,pjar,DEFAULT_SECNAME,err,msg)

      call pjar_choose_section (pjar,DEFAULT_SECNAME)
      call pjar_get            (pjar,'ncolumns'  ,ncolumns  ,0 )
      call pjar_get            (pjar,'nlines'    ,ntraces   ,0 )
      call pjar_get            (pjar,'hdrs'      ,hdrs      ,ndummy)
      call pjar_delete         (pjar)

      if (err /= FLOATIO_OK) then
           info = ' '
           return
      end if

!----------find the desired columns to use for sorting:

      info = trim(string_ii2ss(ntraces))//' traces    '// &
             trim(string_ii2ss(ncolumns-1))//' headers:'
      col  = 0
      tmp  = hdr

      if (force_match) then
           if (tmp%secondary <= 1) tmp%secondary = tmp%primary
           if (tmp%tertiary  <= 1 .or. &
               tmp%secondary == 1) tmp%tertiary  = tmp%secondary
      end if

      do column = 2,ncolumns
           if (tmp%primary   == hdrs(column)) col%primary   = column
           if (tmp%secondary == hdrs(column)) col%secondary = column
           if (tmp%tertiary  == hdrs(column)) col%tertiary  = column
           info = trim(info)//' '//string_ii2ss(hdrs(column))
      end do

!----------finish up and return:

      if (force_match) then
      if (col%primary == 0 .or. col%secondary == 0 .or. col%tertiary == 0) then
           msg = 'specified header words not available in trace file table'
           return
      end if
      end if

      if (ntraces == 0) then
           msg = 'no traces on trace file table'
           return
      end if

      msg  = ' '
      return
      end subroutine tftable_private_open


!!----------------------------- private sort --------------------------------!!
!!----------------------------- private sort --------------------------------!!
!!----------------------------- private sort --------------------------------!!

! opens, reads, and closes specified input file.
! allocates and fills indices(ntraces) with sorted indices.
! returns blank message if no error occurred.


      subroutine tftable_private_sort (filename,msg,       &
                                       ntraces,indices,    &
                                       hdr,init,inc)
      implicit none
      character(len=*)        ,intent(in)  :: filename             ! arguments
      character(len=*)        ,intent(out) :: msg                  ! arguments
      integer                 ,intent(out) :: ntraces              ! arguments
      integer                 ,pointer     :: indices(:)           ! arguments
      type(triplesort_ints)   ,intent(in)  :: hdr                  ! arguments
      type(triplesort_doubles),intent(in)  :: init                 ! arguments
      type(triplesort_doubles),intent(in)  :: inc                  ! arguments
      type(floatio_struct)    ,pointer     :: floatio              ! local
      integer                              :: err,indx             ! local
      type(triplesort_ints)   ,allocatable :: bins(:)              ! local
      type(triplesort_ints)                :: col                  ! local
      real                                 :: vline(MAXCOLUMNS)    ! local
      character(len=80)                    :: info                 ! local
      type(triplesort_doubles)             :: val                  ! local

!----------get started:

      nullify (indices)
      nullify (floatio) ! jpa

      call tftable_private_open (floatio,filename,msg,info,  &
                                 ntraces,hdr,col,.true.)

      if (msg /= ' ') then
           call floatio_close (floatio)
           return
      end if

!----------read the desired columns and preset the indices:

      allocate (bins   (ntraces))
      allocate (indices(ntraces))

      do indx = 1,ntraces
           call floatio_read_line (floatio,err,msg,vline)
           if (err /= FLOATIO_OK) then
                call floatio_close (floatio)
                deallocate (bins)
                deallocate (indices)
                return
           end if

           val%primary   = vline(col%primary  )
           val%secondary = vline(col%secondary)
           val%tertiary  = vline(col%tertiary )
           bins(indx)    = triplesort_binning(val,init,inc)
           indices(indx) = indx
      end do

!----------close the trace file table and sort the data:

      call floatio_close   (floatio)
      call triplesort_sort (indices,bins,ntraces)

      deallocate (bins)
      msg  = ' '
      return
      end subroutine tftable_private_sort


!!----------------------------- open read --------------------------------!!
!!----------------------------- open read --------------------------------!!
!!----------------------------- open read --------------------------------!!


      subroutine tftable_open_read (obj,filename,msg,ntraces,  &
                                    hdr,init,inc)
      implicit none
      type(tftable_struct)    ,pointer     :: obj                 ! arguments
      character(len=*)        ,intent(in)  :: filename            ! arguments
      character(len=*)        ,intent(out) :: msg                 ! arguments
      integer                 ,intent(out) :: ntraces             ! arguments
      type(triplesort_ints)   ,intent(out) :: hdr                 ! arguments
      type(triplesort_doubles),intent(out) :: init                ! arguments
      type(triplesort_doubles),intent(out) :: inc                 ! arguments
      integer                              :: err                 ! local
      type(pjar_struct)       ,pointer     :: pjar                ! local

      nullify (pjar) ! jpa
      call tftable_private_create (obj)

      call pjar_create (pjar)

      call floatio_open_read &
                   (obj%floatio,filename,pjar,DEFAULT_SECNAME,err,msg)

      call pjar_choose_section (pjar,DEFAULT_SECNAME)
      call pjar_get            (pjar,'hdr_pri'   ,hdr%primary    )
      call pjar_get            (pjar,'hdr_sec'   ,hdr%secondary  )
      call pjar_get            (pjar,'hdr_tert'  ,hdr%tertiary   )
      call pjar_get            (pjar,'pri_init'  ,init%primary   )
      call pjar_get            (pjar,'sec_init'  ,init%secondary )
      call pjar_get            (pjar,'tert_init' ,init%tertiary  )
      call pjar_get            (pjar,'pri_inc'   ,inc%primary    )
      call pjar_get            (pjar,'sec_inc'   ,inc%secondary  )
      call pjar_get            (pjar,'tert_inc'  ,inc%tertiary   )
      call pjar_get            (pjar,'nlines'    ,ntraces,0)
      call pjar_delete         (pjar)

      if (err /= FLOATIO_OK) return

      obj%ntraces = ntraces
      msg = ' '
      return
      end subroutine tftable_open_read


!!-------------------------- next record -----------------------------------!!
!!-------------------------- next record -----------------------------------!!
!!-------------------------- next record -----------------------------------!!


      function tftable_next_record (obj,msg) result (irec)
      implicit none
      type(tftable_struct)  ,intent(inout) :: obj               ! arguments
      character(len=*)      ,intent(out)   :: msg               ! arguments
      integer                              :: irec              ! result
      integer                              :: err               ! local
      real                                 :: vline(1)          ! local

      obj%itrace = obj%itrace + 1

      if (obj%itrace > obj%ntraces) then
           msg  = 'no more traces in trace file table'
           irec = -1
      else if (associated(obj%floatio)) then                    ! open_read.
           call floatio_read_line (obj%floatio,err,msg,vline)
           if (err == FLOATIO_OK) then
                msg  = ' '
                irec = nint(vline(1))
           else
                irec = 0
           end if
      else if (associated(obj%indices)) then                    ! open_sort.
           msg  = ' '
           irec = obj%indices(obj%itrace)
      else
           msg  = 'not reading file and indices not allocated'
           irec = 0
      end if
      return
      end function tftable_next_record


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module tftable_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

