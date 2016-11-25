!<CPS_v1 type="PROGRAM"/>


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
!                       C P S   P R O G R A M
!
! Name       : rptstats 
! Category   : miscellaneous
! Written    : 1998-09-02   by: Donna K. Vunderink
! Revised    : 2002-10-09   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : Program to extract specific words from CPS report files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! A program to read CPS report files and extract specific words into a <TAB>
! delimited ASCII file which can be read into a spreadsheet application such as
! Microsoft's EXCEL.
!
! This program is part of ciu (CPS Interactive Utilities).
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! 1.  You may read multiple files in at once using an astrisk (*) for
!     a wild card, e.g. proj*.rpt.* or you can also have a file containing
!     a list of the report files in the order to be read.
!
! 2.  Defaults for the parameters STRING, OCCURRENCE, LINES, WORD, and
!     TITLE are taken from an initialization file in the user's current
!     directory.  If the file does not exist, it is created the first
!     time the program is executed.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2002-10-09  Vunderink  Converted from VAX program cpr_stats.
!  1. 1998-09-02  Vunderink  Original version.
!
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


!-------------------------------------------------------------------------------
!
!
!<gui_def>
!<include ciu_top.lay>
!<NS rptstats Program/NC=80>
!
! `- Input file(s) -----------------------------------------------------------+
!  Select REPORT[REPORTFILE]~~`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [REPORTFILE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!  Select LISTFILE[LISTFILE]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [LISTFILE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! `---------------------------------------------------------------------------+
!
! `- Output file -------------------------------------------------------------+
!  Select OUTPUT[OUTPUT]~~`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [OUTPUT_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! `---------------------------------------------------------------------------+
!
! `- Array defaults file -----------------------------------------------------+
!  Select INITFILE[INITFILE]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [INITFILE_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! `---------------------------------------------------------------------------+
!
! STRING                    OCCURRENCE LINES WORDS TITLE
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSS `IIIIIIIII `IIII `IIII `SSSSSSSSSSSSSSSSSSSSSSSS
!
!<PARMS REPORTFILE[/ML=140/XST]>
!<PARMS REPORTFILE_INFO[/ML=140/XST]>
!<PARMS LISTFILE[/ML=140/XST]>
!<PARMS LISTFILE_INFO[/ML=140/XST]>
!<PARMS OUTPUT[/ML=140/XST]>
!<PARMS OUTPUT_INFO[/ML=140/XST]>
!<PARMS INITFILE[/ML=140/XST]>
!<PARMS INITFILE_INFO[/ML=140/XST]>
!<PARMS STRING_ARRAYSET[/XST/YST]>
!<PARMS STRING[/CS=25/ML=40]>
!<PARMS OCCURRENCE[/CS=10/ML=2]>
!<PARMS LINES[/CS=5/ML=2]>
!<PARMS WORDS[/CS=6/ML=2]>
!<PARMS TITLE[/CS=25/ML=40]>
!
!<include ciu_bottom.lay>
!</gui_def>
!
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
!
!<HelpSection>
!
!<Help KEYWORD="INITFILE">
!<Tip> The file containing defaults. </Tip>
! Default =
! Allowed =
! The file containing defaults for the parameters STRING, OCCURRENCE, LINES,
! WORD and TITLE.
!</Help>
!
!<Help KEYWORD="SELECT_INITFILE">
!<Tip> Choose INITFILE using a file selection dialog box. </Tip>
! Choose INITFILE using a file selection dialog box.
!</Help>
!
!<Help KEYWORD="INITFILE_INFO">
!<Tip> File status for INITFILE. </Tip>
! File status for INITFILE.
!</Help>
!
!<Help KEYWORD="REPORTFILE">
!<Tip> File name to search. </Tip>
! Default =
! Allowed =
! File name to search.
! You may use * as wildcard for multiple file searches.
!</Help>
!
!<Help KEYWORD="SELECT_REPORTFILE">
!<Tip> Choose REPORTFILE using a file selection dialog box. </Tip>
! Choose REPORTFILE using a file selection dialog box.
!</Help>
!
!<Help KEYWORD="REPORTFILE_INFO">
!<Tip> File status for REPORTFILE. </Tip>
! File status for REPORTFILE.
!</Help>
!
!<Help KEYWORD="LISTFILE">
!<Tip>List file containing files to search. </Tip>
! Default =
! Allowed =
! List file containing files to search..
!</Help>
!
!<Help KEYWORD="SELECT_LISTFILE">
!<Tip> Choose LISTFILE using a file selection dialog box. </Tip>
! Choose LISTFILE using a file selection dialog box.
!</Help>
!
!<Help KEYWORD="LISTFILE_INFO">
!<Tip> File status for LISTFILE. </Tip>
! File status for LISTFILE.
!</Help>
!
!<Help KEYWORD="OUTPUT">
!<Tip> File name for output search information. </Tip>
! Default =
! Allowed =
! File name for output search information.
!</Help>
!
!<Help KEYWORD="SELECT_OUTPUT">
!<Tip> Choose OUTPUT using a file selection dialog box. </Tip>
! Choose OUTPUT using a file selection dialog box.
!</Help>
!
!<Help KEYWORD="OUTPUT_INFO">
!<Tip> File status for OUTPUT. </Tip>
! File status for OUTPUT.
!</Help>
!
!<Help KEYWORD="STRING">
!<Tip> String to search for. </Tip>
! Default =
! Allowed =
! String to search for.  Is case sensitive.
!</Help>
!
!<Help KEYWORD="OCCURRENCE">
!<Tip> Which occurrence of the string do you want to search for ? </Tip>
! Default =
! Allowed =
! Which occurrence of the string do you want to search for ?
!</Help>
!
!<Help KEYWORD="LINES">
!<Tip> The number of lines to move once the string is found. </Tip>
! Default =
! Allowed =
! The number of lines above (negative number) or below (positive number) to move
! once the string is found.
!</Help>
!
!<Help KEYWORD="WORDS">
!<Tip> Which word on the line do you want ? </Tip>
! Default =
! Allowed =
! Which word on the line do you want ?
!</Help>
!
!<Help KEYWORD="TITLE">
!<Tip> The title to output for this word. </Tip>
! Default =
! Allowed =
! The title to output for this word.
!</Help>
!
!<Help KEYWORD="OK">
!<Tip> Button to accept parameter values, run traps and remove screen. </Tip>
! Default =
! Allowed = 
! Button that accepts parameter values, runs the parameter traps and removes
! the parameter screen. 
!</Help>
!
!<Help KEYWORD="APPLY">
!<Tip> Button that accepts parameter values and runs the parameter traps. </Tip>
! Default = 
! Allowed = 
! Button that accepts parameter values and runs the parameter traps but does
! not remove the parameter screen.
!</Help>
!
!<Help KEYWORD="CANCEL">
!<Tip> Button to remove the parameter screen without changing parameters. </Tip>
! Default = 
! Allowed = 
!</Help>
!
!<Help KEYWORD="HELP">
!<Tip> Allows the user to access the pop-up Help Window. </Tip>
! Default = 
! Allowed = 
! The Help Window allows access to three kinds of help:  Parameter Help, CPS 
! Process Help and Application (CFE) Help.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module rptstats_module

      use cio_module
      use pathcheck_module
      use pathchoose_module
      use pc_module
      use putsys_module
      use finquire_module

      implicit none

      private
      public :: rptstats_create     ! uses the parameter cache.
      public :: rptstats_initialize ! uses the parameter cache.
      public :: rptstats_update     ! uses the parameter cache.
      public :: rptstats_delete
      public :: rptstats            ! main execution

      character(len=100),public,save :: rptstats_IDENT = &
'$Id: rptstats.f90,v 1.2 2002/10/09 19:10:29 Vunderink prod sps $'

      integer,parameter              :: LINE_LENGTH      =  133
      integer,parameter              :: LINE_OUT_LENGTH  = 1000
      integer,parameter              :: NSTRINGS         =  100
      integer,parameter              :: MAX_SAVE         =   99

      interface rptstats_append_array_element
        module procedure rptstats_append_array_element_c
        module procedure rptstats_append_array_element_i
      end interface


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: rptstats_struct              
      private
        integer                           :: stdout
        type(pathchoose_struct),pointer   :: initfile_dialog
        type(pathchoose_struct),pointer   :: listfile_dialog
        type(pathchoose_struct),pointer   :: reportfile_dialog
        type(pathchoose_struct),pointer   :: output_dialog
        character(len=FILENAME_LENGTH)    :: initfile
        character(len=FILENAME_LENGTH)    :: listfile
        character(len=FILENAME_LENGTH)    :: reportfile
        character(len=FILENAME_LENGTH)    :: output
        integer                           :: num_items
        character(len=PC_LENGTH),pointer  :: string(:)
        integer                 ,pointer  :: occurrence(:)
        integer                 ,pointer  :: lines(:)
        integer                 ,pointer  :: words(:)
        character(len=PC_LENGTH),pointer  :: title(:)
      end type rptstats_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      type(rptstats_struct),pointer,save :: object      ! needed for traps.


      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine rptstats_create (obj)
      implicit none
      type(rptstats_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%string)
      nullify(obj%occurrence)
      nullify(obj%lines)
      nullify(obj%words)
      nullify(obj%title)
      call pathchoose_create (obj%initfile_dialog  ,'INITFILE'  ,'init' )
      call pathchoose_create (obj%listfile_dialog  ,'LISTFILE'  ,'lst'  )
      call pathchoose_create (obj%reportfile_dialog,'REPORTFILE','rpt.*')
      call pathchoose_create (obj%output_dialog    ,'OUTPUT'    ,'sst'  )

      call rptstats_initialize (obj)

      return
      end subroutine rptstats_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine rptstats_delete (obj)
      implicit none
      type(rptstats_struct),pointer :: obj       ! arguments

      if (associated(obj%string))     deallocate(obj%string)
      if (associated(obj%occurrence)) deallocate(obj%occurrence)
      if (associated(obj%lines))      deallocate(obj%lines)
      if (associated(obj%words))      deallocate(obj%words)
      if (associated(obj%title))      deallocate(obj%title)

      call pathchoose_delete (obj%initfile_dialog)
      call pathchoose_delete (obj%listfile_dialog)
      call pathchoose_delete (obj%reportfile_dialog)
      call pathchoose_delete (obj%output_dialog)

      deallocate(obj)

      return
      end subroutine rptstats_delete


!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!


      subroutine rptstats_initialize (obj)
      implicit none
      type(rptstats_struct) :: obj       ! arguments

      obj%stdout     = 6
      obj%initfile   = 'rptstats.init'
      obj%listfile   = ''
      obj%reportfile = '*.rpt.*'
      obj%output     = 'rptstats.sst'
      obj%num_items  = 0

      allocate(obj%string(1))
      allocate(obj%occurrence(1))
      allocate(obj%lines(1))
      allocate(obj%words(1))
      allocate(obj%title(1))

      call rptstats_initfile_read (obj)

      call rptstats_update (obj)

      return
      end subroutine rptstats_initialize


!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!


      subroutine rptstats_update (obj)
      implicit none
      type(rptstats_struct),target         :: obj                      !argument

      integer                              :: nstring                  !local
      integer                              :: noccurrence              !local
      integer                              :: nlines                   !local
      integer                              :: nwords                   !local
      integer                              :: ntitle                   !local

      object => obj               ! needed for traps.

      if (pathchoose_update(obj%initfile_dialog  ,obj%initfile  ,  &
                                               rptstats_initfile_trap  )) return
      if (pathchoose_update(obj%listfile_dialog  ,obj%listfile  ,  &
                                               rptstats_listfile_trap  )) return
      if (pathchoose_update(obj%reportfile_dialog,obj%reportfile,  &
                                               rptstats_reportfile_trap)) return
      if (pathchoose_update(obj%output_dialog    ,obj%output    ,  &
                                               rptstats_output_trap    )) return

      call pc_get ('INITFILE'  , obj%initfile  , rptstats_initfile_trap  )
      call pc_get ('LISTFILE'  , obj%listfile  , rptstats_listfile_trap  )
      call pc_get ('REPORTFILE', obj%reportfile, rptstats_reportfile_trap)
      call pc_get ('OUTPUT'    , obj%output    , rptstats_output_trap    )

      nstring     = obj%num_items
      noccurrence = obj%num_items
      nlines      = obj%num_items
      nwords      = obj%num_items
      ntitle      = obj%num_items

      call pc_alloc ('STRING'    , obj%string    , nstring    )
      call pc_alloc ('OCCURRENCE', obj%occurrence, noccurrence)
      call pc_alloc ('LINES'     , obj%lines     , nlines     )
      call pc_alloc ('WORDS'     , obj%words     , nwords     )
      call pc_alloc ('TITLE'     , obj%title     , ntitle     )

      obj%num_items = min(nstring,noccurrence,nlines,nwords,ntitle)

      call pc_put ('INITFILE'  , obj%initfile  )
      call pc_put ('LISTFILE'  , obj%listfile  )
      call pc_put ('REPORTFILE', obj%reportfile)
      call pc_put ('OUTPUT'    , obj%output    )

      call pc_put ('STRING'    , obj%string    , obj%num_items)
      call pc_put ('OCCURRENCE', obj%occurrence, obj%num_items)
      call pc_put ('LINES'     , obj%lines     , obj%num_items)
      call pc_put ('WORDS'     , obj%words     , obj%num_items)
      call pc_put ('TITLE'     , obj%title     , obj%num_items)

      return
      end subroutine rptstats_update


!!--------------------------- main execution ------------------------------!!
!!--------------------------- main execution ------------------------------!!
!!--------------------------- main execution ------------------------------!!


      subroutine rptstats (obj)
      implicit none
      type(rptstats_struct)                :: obj                      !argument

      character(len=FILENAME_LENGTH)       :: filename                 !local
      character(len=2)                     :: mode                     !local
      character(len=PC_LENGTH)             :: string                   !local
      character(len=LINE_OUT_LENGTH)       :: line_out                 !local
      character(len=80)                    :: token(NSTRINGS)          !local
      character(len=LINE_LENGTH)           :: save_lines(MAX_SAVE)     !local
      character(len=LINE_LENGTH)           :: line                     !local
      character(len=132)                   :: mess                     !local

      integer                              :: cpi                      !local
      integer,allocatable                  :: hfilename(:)             !local
      integer                              :: lun_init                 !local
      integer                              :: lun_lst                  !local
      integer                              :: lun_rpt                  !local
      integer                              :: lun_out                  !local
      integer                              :: istat                    !local
      integer                              :: i                        !local
      integer                              :: i1                       !local
      integer                              :: i2                       !local
      integer                              :: indx                     !local
      integer                              :: j                        !local
      integer                              :: line_count               !local
      integer                              :: indx_save_lines          !local
      integer                              :: nfilename                !local
      integer                              :: ntitle                   !local
      integer                              :: nline                    !local
      integer                              :: nline_out                !local
      integer                              :: multi_file               !local
      integer                              :: list_file                !local
      integer                              :: nstring                  !local
      integer                              :: ocount(NSTRINGS)         !local
      integer                              :: next(NSTRINGS)           !local
      integer                              :: lenstr(NSTRINGS)         !local
      integer                              :: ntoken                   !local

      integer,external                     :: rptstats_opendir
      integer,external                     :: rptstats_readdir
      integer,external                     :: rptstats_closedir
      integer                              :: temp

      cpi = string_chars_per_integer()
      i   = (FILENAME_LENGTH+1)/cpi + 1
      allocate(hfilename(i),stat=istat)
      hfilename  = 0

!---- Write init file
      mode = "w"
      lun_init = cio_fopen(trim(obj%initfile),mode,  &
                      file_space_commit=PREALLOCATE_FILE_SPACE_DISABLED,  &
                      file_lock=FILE_LOCK_DISABLED)
      if (lun_init .lt. 100) then
        write(mess,*)  &
                  'Error opening file '//trim(obj%initfile)//'  lun=',lun_init
        call pc_error (mess)
        return
      endif

      do i=1,obj%num_items
        write(string,'(A20,3I6,1X,A15)') obj%string(i),obj%occurrence(i),  &
                                         obj%lines(i),obj%words(i),obj%title(i)
        temp    = len_trim(string)
        nstring = cio_fputline (string,temp,lun_init)
        if (nstring .lt. len_trim(string)) then
          write(mess,*) 'Error writing init file ',nstring,len_trim(string)
          call pc_error (mess)
        endif
      enddo

      istat = cio_fclose (lun_init)

!---- Open output
      mode = "w"
      lun_out = cio_fopen(trim(obj%output),mode,  &
                      file_space_commit=PREALLOCATE_FILE_SPACE_DISABLED,  &
                      file_lock=FILE_LOCK_DISABLED)
      if (lun_out .lt. 100) then
        write(mess,*) 'Error opening file '//trim(obj%output)//'  lun=',lun_out
        call pc_error (mess)
        return
      endif

!---- Write titles
      i1 = 1
      do i=1,obj%num_items
         ntitle = len_trim(obj%title(i))
         if (ntitle .eq. 0) ntitle = 1
         i2 = i1+ntitle
         if (i2 .gt. LINE_OUT_LENGTH) exit
         write(line_out(i1:i2),'(A,A)') obj%title(i)(1:ntitle),char(9)
         i1 = i2+1
      enddo

      nline_out = cio_fputline (line_out(1:i1-2),i1-2,lun_out)
      if (nline_out .lt. i1-2) then
        write(mess,*) 'Error writing titles ',nline_out,i1-2
        call pc_error (mess)
      endif

      multi_file = index(obj%reportfile,'*')
      if (multi_file .gt. 0) then
        call string_cc2hh(obj%reportfile,hfilename)
        istat = rptstats_opendir(hfilename)
      endif

      list_file = len_trim(obj%listfile)
      if (list_file .gt. 0) then
        mode = "r"
        lun_lst = cio_fopen(trim(obj%listfile),mode,  &
                            file_lock=FILE_LOCK_DISABLED)
        if (lun_lst .lt. 100) then
          write(mess,*)  &
                  'Error opening file '//trim(obj%listfile)//'  lun=',lun_lst
          call pc_error (mess)
          list_file = 0
        endif
      endif

      do
!------ Open file to search
        if (list_file .gt. 0) then
          nfilename = cio_fgetline (filename,FILENAME_LENGTH,lun_lst)
          if (nfilename .lt. 0) then
            istat = cio_fclose(lun_lst)
            list_file = 0
            cycle
          else
            mode = "r"
            lun_rpt = cio_fopen(trim(filename),mode,  &
                                file_lock=FILE_LOCK_DISABLED)
            if (lun_rpt .lt. 100) then
              write(mess,*)   &
                       'Error opening file '//trim(filename)//'  lun=',lun_rpt
              call pc_error (mess)
              cycle
            endif
          endif
        else
          if (multi_file .gt. 0) then
            istat = rptstats_readdir(hfilename)
            if (istat .ne. 0) exit
            call string_hh2cc(hfilename,filename)
          else
            filename = obj%reportfile
          endif
          mode = "r"
          lun_rpt = cio_fopen(trim(filename),mode,file_lock=FILE_LOCK_DISABLED)
          if (lun_rpt .lt. 100) then
            write(mess,*)  &
                         'Error opening file '//trim(filename)//'  lun=',lun_rpt
            call pc_error (mess)
            if (multi_file .eq. 0) then
              exit
            else
              cycle
            endif
          endif
        endif

!----- Initialize variables
        line_count      = 0
        indx_save_lines = 0
        do i=1,NSTRINGS
           ocount(i) = 0
           next(i)   = -1
           do j=1,80
              token(i)(j:j) = ' '
           enddo
        enddo
        do i=1,obj%num_items
           lenstr(i) = len_trim(obj%string(i))
        enddo

!------ Read and search file
        do
          nline = cio_fgetline (line,LINE_LENGTH,lun_rpt)
          if (nline .lt. 0) exit
          line_count = line_count + 1
          indx_save_lines = indx_save_lines + 1
          if (indx_save_lines .gt. max_save) indx_save_lines = 1
          save_lines(indx_save_lines) = line(1:nline)

          do i=1,obj%num_items
           if (next(i) .gt. -1) then
             next(i) = next(i) - 1
             if (next(i) .eq. 0) then
               call rptstats_token(line(1:nline),token(i),obj%words(i))
               next(i) = -1
             endif
           endif
          enddo

          do i=1,obj%num_items
            indx = index(line(1:nline),obj%string(i)(1:lenstr(i)))
            if (indx .gt. 0) then
              ocount(i) = ocount(i) + 1
              if (ocount(i) .eq. obj%occurrence(i)) then
                if (obj%lines(i) .le. 0) then
                  j = indx_save_lines + obj%lines(i)
                  if (j .gt. 0) then
                    call rptstats_token(save_lines(j),token(i),obj%words(i))
                  else if (line_count .gt. max_save) then
                    j = max_save + j
                    call rptstats_token(save_lines(j),token(i),obj%words(i))
                  endif
                else
                  next(i) = obj%lines(i)
                endif
              endif
            endif
          enddo
        enddo

!------ Write output line for file
        i1 = 1
        do i=1,obj%num_items
          call string_strip_blanks(token(i),token(i),ntoken)
          if (ntoken .eq. 0) ntoken = 1
          i2 = i1+ntoken
          if (i2 .gt. LINE_OUT_LENGTH) exit
          write(line_out(i1:i2),'(A,A)') token(i)(1:ntoken),char(9)
          i1 = i2+1
        enddo

        nline_out = cio_fputline (line_out(1:i1-2),i1-2,lun_out)
        if (nline_out .lt. i1-2) then
          write(mess,*) 'Error writing line ',nline_out,i1-2
          call pc_error (mess)
        endif

        istat = cio_fclose(lun_rpt)
        if (list_file .eq. 0 .and. multi_file .eq. 0) exit
      enddo

      if (multi_file .gt. 0) istat = rptstats_closedir()
      istat = cio_fclose(lun_out)

      deallocate(hfilename,stat=istat)

      return
      end subroutine rptstats


      subroutine rptstats_token(line,token,iwords)
!***********************************************************************
!     Get token/word from line
!***********************************************************************

      character(len=*)                     :: line                     !argument
      character(len=*)                     :: token                    !argument
      integer                              :: iwords                   !argument

      integer                              :: i                        !local
      integer                              :: i1                       !local
      integer                              :: i2                       !local
      integer                              :: icount                   !local
      integer                              :: n                        !local
      integer                              :: ntoken                   !local

      icount = 0
      ntoken = len(token)
      do i=1,ntoken
         token(i:i) = ' '
      enddo
      n = len(line)

      do i1=1,n
         if (line(i1:i1) .ne. ' ') goto 100
      enddo
      return

  100 continue
      i2 = index(line(i1:n),' ')
      if (i2 .eq. 0) then
         i2 = n+1
      else
         i2 = i1+i2-1
      endif
      icount = icount + 1
      if (icount .eq. iwords) then
         token(1:i2-i1) = line(i1:i2-1)
         do i=1,i2-i1
           if (token(i:i).lt.' '.or.token(i:i).gt.'}') token(i:i) = ' '
         enddo
         return
      endif
      i2 = i2 + 1
      if (i2 .le. n) then
         do i1=i2,n
           if (line(i1:i1) .ne. ' ') goto 100
        enddo
      endif

      return
      end subroutine rptstats_token


!!--------------------------- initfile_trap --------------------------------!!
!!--------------------------- initfile_trap --------------------------------!!
!!--------------------------- initfile_trap --------------------------------!!


      subroutine rptstats_initfile_trap (keyword)
      implicit none
      character(len=*),intent(in)          ::  keyword                 !argument

      character(len=2)                     :: mode                     !local
      integer                              :: lun                      !local
      integer                              :: istat                    !local
      integer                              :: ntemp                    !local
      integer                              :: occurrence               !local
      integer                              :: lines                    !local
      integer                              :: words                    !local
      character(len=PC_LENGTH)             :: str                      !local
      integer                              :: nstr                     !local
      character(len=PC_LENGTH)             :: string                   !local
      character(len=PC_LENGTH)             :: title                    !local

      call pathcheck ('INITFILE', object%initfile, 'init',  &
                      required=.true., show=PATHCHECK_INFO_GENERAL)

      istat = finquire_input (trim(object%initfile))
      if (istat .ne. FINQUIRE_FOUND) return

      mode = "r"
      lun = cio_fopen(trim(object%initfile),mode,file_lock=FILE_LOCK_DISABLED)
      if (lun .lt. 100) then
        write(object%stdout,*) 'Error opening file '//trim(object%initfile)// &
                               '  lun=',lun
        return
      endif

      object%num_items = 0
      do
        nstr = cio_fgetline (str,PC_LENGTH,lun)
        if (nstr .lt. 0) exit
        read(str,'(A20,3I6,1X,A15)') string,occurrence,lines,words,title
        ntemp = object%num_items
        call rptstats_append_array_element(object%string    ,ntemp,trim(string))
        ntemp = object%num_items
        call rptstats_append_array_element(object%occurrence,ntemp,occurrence)
        ntemp = object%num_items
        call rptstats_append_array_element(object%lines     ,ntemp,lines)
        ntemp = object%num_items
        call rptstats_append_array_element(object%words     ,ntemp,words)
        ntemp = object%num_items
        call rptstats_append_array_element(object%title     ,ntemp,trim(title))
        object%num_items = object%num_items + 1
      enddo

      istat = cio_fclose (lun)

      return
      end subroutine rptstats_initfile_trap


!!--------------------------- listfile_trap --------------------------------!!
!!--------------------------- listfile_trap --------------------------------!!
!!--------------------------- listfile_trap --------------------------------!!


      subroutine rptstats_listfile_trap (keyword)
      implicit none
      character(len=*),intent(in)          ::  keyword                 !argument

      call pathcheck ('LISTFILE', object%listfile,'lst',  &
                      required=.false., show=PATHCHECK_INFO_INPUT)
      if (trim(object%listfile) .eq. PATHCHECK_EMPTY) object%listfile = ''

      return
      end subroutine rptstats_listfile_trap


!!-------------------------- reportfile_trap -------------------------------!!
!!-------------------------- reportfile_trap -------------------------------!!
!!-------------------------- reportfile_trap -------------------------------!!


      subroutine rptstats_reportfile_trap (keyword)
      implicit none
      character(len=*),intent(in)          ::  keyword                 !argument

      if (index(object%reportfile,'*') .gt. 0) then
        call pc_put ('REPORTFILE_INFO',' ')
      else
        call pathcheck ('REPORTFILE', object%reportfile,'rpt',  &
                        required=.false., show=PATHCHECK_INFO_INPUT)
      endif

      return
      end subroutine rptstats_reportfile_trap


!!---------------------------- output_trap ---------------------------------!!
!!---------------------------- output_trap ---------------------------------!!
!!---------------------------- output_trap ---------------------------------!!


      subroutine rptstats_output_trap (keyword)
      implicit none
      character(len=*),intent(in)          ::  keyword                 !argument

      call pathcheck ('OUTPUT', object%output, 'sst',  &
                      required=.true., show=PATHCHECK_INFO_OUTPUT)

      return
      end subroutine rptstats_output_trap



!!--------------------------- initfile_read --------------------------------!!
!!--------------------------- initfile_read --------------------------------!!
!!--------------------------- initfile_read --------------------------------!!


      subroutine rptstats_initfile_read (object)
      implicit none
      type(rptstats_struct)                :: object                   !argument

      character(len=2)                     :: mode                     !local
      integer                              :: lun                      !local
      integer                              :: istat                    !local
      integer                              :: ntemp                    !local
      integer                              :: occurrence               !local
      integer                              :: lines                    !local
      integer                              :: words                    !local
      character(len=PC_LENGTH)             :: str                      !local
      integer                              :: nstr                     !local
      character(len=PC_LENGTH)             :: string                   !local
      character(len=PC_LENGTH)             :: title                    !local

      mode = "r"
      lun = cio_fopen(trim(object%initfile),mode,file_lock=FILE_LOCK_DISABLED)
      if (lun .lt. 100) return

      object%num_items = 0
      do
        nstr = cio_fgetline (str,PC_LENGTH,lun)
        if (nstr .lt. 0) exit
        read(str,'(A20,3I6,1X,A15)') string,occurrence,lines,words,title
        ntemp = object%num_items
        call rptstats_append_array_element(object%string    ,ntemp,trim(string))
        ntemp = object%num_items
        call rptstats_append_array_element(object%occurrence,ntemp,occurrence)
        ntemp = object%num_items
        call rptstats_append_array_element(object%lines     ,ntemp,lines)
        ntemp = object%num_items
        call rptstats_append_array_element(object%words     ,ntemp,words)
        ntemp = object%num_items
        call rptstats_append_array_element(object%title     ,ntemp,trim(title))
        object%num_items = object%num_items + 1
      enddo

      istat = cio_fclose (lun)

      return
      end subroutine rptstats_initfile_read


!!----------------------- append_array_element_c ---------------------------!!
!!----------------------- append_array_element_c ---------------------------!!
!!----------------------- append_array_element_c ---------------------------!!


      subroutine rptstats_append_array_element_c (array,narray,value)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (narray .le. 0) then
        if (associated(array)) deallocate(array)
        narray = 1
        allocate(array(narray))
        array(narray) = value
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray+1))
        array(1:narray) = temp_array(1:narray)
        array(narray+1:narray+1) = value
        narray = narray + 1
        deallocate(temp_array)
      endif

      return
      end subroutine rptstats_append_array_element_c


!!----------------------- append_array_element_i ---------------------------!!
!!----------------------- append_array_element_i ---------------------------!!
!!----------------------- append_array_element_i ---------------------------!!


      subroutine rptstats_append_array_element_i (array,narray,value)
      implicit none
      integer,pointer                          :: array(:)             !argument
      integer,intent(inout)                    :: narray               !argument
      integer,intent(in)                       :: value                !argument

      integer,allocatable                      :: temp_array(:)        !local

      if (narray .le. 0) then
        if (associated(array)) deallocate(array)
        narray = 1
        allocate(array(narray))
        array(narray) = value
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray+1))
        array(1:narray) = temp_array(1:narray)
        array(narray+1:narray+1) = value
        narray = narray + 1
        deallocate(temp_array)
      endif

      return
      end subroutine rptstats_append_array_element_i


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module rptstats_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

