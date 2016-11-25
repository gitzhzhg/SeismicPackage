!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ tempname.f90 ------------------------------!!
!!------------------------------ tempname.f90 ------------------------------!!
!!------------------------------ tempname.f90 ------------------------------!!


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
! Name       : tempname 
! Category   : io
! Written    : 2000-01-03   by: Tom Stoeckley
! Revised    : 2001-11-12   by: Ed Schmauch
! Maturity   : production   2001-12-10
! Purpose    : Make up local non-existent filename for temporary use.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                 
!
! Use this primitive to make up a local non-existent filename for temporary
! use.  The filename will point to the local directory or to any specified
! directory.
!
! This primitive does not create any files or directories.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS             
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE                    
!
!                                                opt
!                      o                  i       i
!                   filename = tempname (seed,directory)
!
! character(len=200)  filename = name of temporary non-existent file to use.
! character(len=*)        seed = name to use as starting point.
! character(len=*)   directory = directory in which to put the file.
!
! FILENAME will be the name of a non-existent file in the specified directory.
! SEED can be any word.  If FILENAME is to be a local copy of some other
! file residing in any directory on any machine, SEED can simply be
! the name of that other file, optionally including the user ID, node,
! and/or directory.
!
! FILENAME is made using the SEED, with some characters replaced by other
! characters.  If the resulting FILENAME is the name of a file which already
! exists in the specified directory, it is modified by tacking on a suffix to
! make the name unique.
!
! FILENAME will be returned as a blank if a temporary name cannot be found.
! This would normally occur if there is no permission to write a file in the
! specified directory.
!
! If DIRECTORY is not specified or blank, the cpstemp directory will be used.
! This is a special directory below the user's home directory.  The temporary
! file will reside on the local disk, with a link from the cpstemp directory.
! If the file is too large to fit on the local disk, parts of the file are
! distributed to other disks.  Because of NFS mounting, the local disk should
! be much faster.  The cpstemp directory does not have to pre-exist.
!
! If DIRECTORY is specified and not blank, the filename will be prepended
! by the specified directory, which can be an absolute or relative path.
! For example:
! Setting DIRECTORY to '/tmp' will place the file into the /tmp directory.
! Setting DIRECTORY to '.' will place the file into the local directory.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                   
!
!     Date       Author     Description
!     ----       ------     -----------
!  8. 2001-12-10 Schmauch   Corrected doc for calling sequence.
!                            Made ifc compilable.
!  7. 2001-03-15 Stoeckley  Also add an additional counter to deal with
!                            NFS delay problems when two requests are made
!                            nearly simultaneously by the same job.
!  6. 2001-02-23 Stoeckley  Add random numbers (based on clock time and pid)
!                            to filename to get around NFS delays when checking
!                            for file existence.
!  5. 2001-02-13 Stoeckley  Put CPSTEMP directory under user's home directory,
!                            and add call to EXPTILDE.
!  4. 2001-01-23 Stoeckley  Implement use of the CPSTEMP directory.
!  3. 2000-10-19 Stoeckley  Add missing required documentation section.
!  2. 2000-08-22 Stoeckley  Add optional subroutine argument DIRECTORY.
!  1. 2000-01-07 Stoeckley  Initial version.
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


      module tempname_module

      use string_module
      use finquire_module
      use exptilde_module
      use getsys_module
      use string_module

      implicit none

     character(len=100),save,public :: tempname_ident = &
     "$Id: tempname.f90,v 1.8 2001/12/05 21:53:18 Schmauch prod sps $"

      public

      integer,private,save :: kounter = 0

      contains


!!-------------------------------- tempname -------------------------------!!
!!-------------------------------- tempname -------------------------------!!
!!-------------------------------- tempname -------------------------------!!


      function tempname (seed,directory) result (filename)
      implicit none
      character(len=*),         intent(in) :: seed                  ! argument
      character(len=*),optional,intent(in) :: directory             ! argument
      character(len=200)                   :: filename              ! result
      character(len=200)                   :: directory2,seed2      ! local
      integer                              :: istat,length,kount    ! local
      character(len=24)                    :: suffix,time,pid,kid   ! local
      character(len=20),parameter          :: DEFDIR = '~/cpstemp'  ! local
      character(len=20),parameter          :: DEFSEED = 'temptfile' ! local
      integer          ,parameter          :: NUMTRIES = 50         ! local

      ! Temporary character string used to eliminate ifc character string
      ! truncated compiler warning.  Summing the lengths of all strings
      ! that will be concatenated into tmp_string at different places is
      ! overkill, but it should make the length of tmp_string robust with
      ! respect to changes in other string lengths.
      ! ehs 08nov01
      !
      character(len=len(seed2       ) &
                   +len(time        ) &
                   +len(pid         ) &
                   +len(kid         ) &
                 +3*len('+'         ) &
                   +len(directory2  ) &
                   +len(filename    ) &
                   +len(suffix      ) ) :: tmp_string

      if (.not.present(directory)) then
           directory2 = DEFDIR
      else if (directory == ' ') then
           directory2 = DEFDIR
      else
           directory2 = directory
      end if

      if (seed == ' ') then
           seed2 = DEFSEED
      else
           seed2 = seed
      end if

      kounter = kounter + 1
      call string_time  (time)
      call string_ii2cc (getsys_pid(),pid)
      call string_ii2cc (kounter,kid)

      tmp_string = trim(seed2) // '+' // trim(time) // '+' // trim(pid)  &
                             // '+' // trim(kid)
      filename = tmp_string(1:len(filename))

      call string_replace_character (filename, '@', '%')
      call string_replace_character (filename, ':', '%')
      call string_replace_character (filename, '/', '%')
      call string_replace_character (filename, ' ', '%')
      call string_replace_character (filename, "'", '%')
      call string_replace_character (filename, '"', '%')
      call string_replace_character (filename, '~', '%')

      tmp_string = trim(directory2) // '/' // filename
      filename = tmp_string(1:len(filename))

      if (filename(1:1) == '~') call exptilde (filename)

      length = len_trim(filename)
      kount  = 0
      do
        istat = finquire_file(filename)
        select case (istat)
          case (FINQUIRE_BLANK         ) ; return    ! should never happen.
          case (FINQUIRE_NOT_FOUND     ) ; return    ! we have a unique name.
          case (FINQUIRE_FOUND         ) ; continue  ! the name already exists.
          case (FINQUIRE_NOT_READABLE  ) ; continue
          case (FINQUIRE_NOT_WRITEABLE ) ; continue
          case (FINQUIRE_NOT_READ_WRITE) ; continue
          case (FINQUIRE_NOT_CREATEABLE) 
                                 if (directory2 /= DEFDIR) filename = '' 
                                 return
        end select
        if (kount > NUMTRIES) then
             filename = ''
             return
        end if
        kount = kount + 1
        tmp_string = string_ii2ss(kount)
        suffix = tmp_string(1:len(suffix))
        tmp_string = filename(1:length) // '+' // suffix
        filename = tmp_string(1:len(filename))
      end do
      return
      end function tempname


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module tempname_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

