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
! Name       : cnfg
! Category   : main_prog
! Written    : 2002-06-20   by: Charles C. Burch
! Revised    : 2003-01-20   by: Charles C. Burch
! Maturity   : production   2003-02-27
! Purpose    : Provide CPS system configuration information
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This provides CPS configuration information. It should be called to obtain
! any configuration information instead of embedding such information in code
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS
!
!  Not Applicable
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS
!
! Not Applicable
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
! Not Applicable
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
! subroutine cnfg_get_value(code_value, value)
!   character(len=*), intent(in)  :: code_value
!   character(len=*), intent(out) :: value
!
! Purpose: Get value of various parameters in the configuration file
! Valid values for code_value (as of 6/19/02) are:
!     "cps_config_file_code"
!     "cps_main_host_code"
!     "cps_lock_file_code"
!     "cps_mess_lock_file_code"
!     "pfio_port_file_code"
!     "cpsdata_nodes_file_code"
!     "cpswork_nodes_file_code"
!     "cpstemp_nodes_file_code"
!     "migration_tt_config_file_code"
!
!-------------------------------------------------------------------------------
!  subroutine cnfg_get_tt_names(n_names, names)
!    integer, intent(out)                       :: n_names
!    character(len=*),pointer                   :: names(:)
!
!  Purpose Get number of tt path names, allocate names array to proper size
!  and fill it with the path names
!
!-------------------------------------------------------------------------------
!  subroutine cnfg_get_tt_set_info(name, n_disks, disks)
!    character(len=*), intent(in)                :: name
!    integer, intent(inout)                      :: n_disks
!    character(len=*), pointer                   :: disks(:)
!
!  Purpose For a given path name, get the number of disks for the path,
!    allocate disks to be the proper size and place the disk names
!    in it
!-------------------------------------------------------------------------------
!</calling_doc>
!
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author    Description
!     ----        ------    -----------
! 4.  2003-02-27  C C Burch Removed fixed-length character parms in tt routines
! 3.  2002-08-12  C C Burch Changed all internal disk write with do loop moves
! 2.  2002-07-26  C C Burch Changed name1 size and removed internal write to
!                             work with Intel compiler
! 1.  2002-06-20  Burch CC  Initial version
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
! None 
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
! Note: calling programs to cnfg_get_tt_names and cnfg_get_tt_set_info
! should have character lenths for name >=cnfg_tt_name_length and
!                              for disk >= cnfg_tt_disk_length
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!


module cnfg_module
  implicit none

  private

  public :: cnfg_get_tt_names
  public :: cnfg_get_tt_set_info
  public :: cnfg_get_value

!  Most of the work is done with c routines in cnfg_crou.c
!  Below are the interfaces for these routines

  interface
    subroutine cnfg_get_tt_info_c(n_names,n_disks)
      integer, intent(out)   :: n_names, n_disks
    end subroutine cnfg_get_tt_info_c
  end interface

  interface
    subroutine cnfg_get_tt_names_c(n,names,len_names)
      integer, intent(out)   :: n
      character              :: names
      integer, intent(in)    :: len_names
    end subroutine cnfg_get_tt_names_c
  end interface

  interface
    subroutine cnfg_get_tt_set_c(name, n_name, disks, n_disks, len_disks)
      character               :: name
      integer, intent(in)     :: n_name
      character               :: disks
      integer, intent(inout)  :: n_disks
      integer, intent(in)     :: len_disks
    end subroutine cnfg_get_tt_set_c
  end interface

  interface
    subroutine cnfg_get_value_f(code_value, value, n_value)
      character, intent(in)  :: code_value
      character, intent(out) :: value 
      integer, intent(in)    :: n_value
    end subroutine cnfg_get_value_f
  end interface


  integer, parameter, public :: cnfg_tt_name_length          = 8
  integer, parameter, public :: cnfg_tt_disk_length          = 40

  character(len=100),save,public :: CNFG_ident =                               &
   "$Id: cnfg.f90,v 1.4 2003/02/26 22:15:34 Burch prod sps $"

contains

!-----------------------------------------------------------------
! Get value of various parameters in the configuration file
!
! Written June 2002 by Charles C Burch
!-----------------------------------------------------------------
  subroutine cnfg_get_value(code_in, value)
    character(len=*), intent(in)  :: code_in
    character(len=*), intent(out) :: value

    character                     :: c_value(len(value))
    character                     :: c_code_in(len(code_in)+1)
    integer                       :: i,n

    n=len_trim(code_in)
    do i=1,n
      c_code_in(i)=code_in(i:i)
    enddo
    c_code_in(n+1)=char(0)

    n=len(value)
    call cnfg_get_value_f(c_code_in(1), c_value(1), n)

    do i=1,n
      value(i:i)=c_value(i)
    enddo

    return
  end subroutine cnfg_get_value

!-----------------------------------------------------------------
! Get number of tt path names, allocate names array to proper size
! and fill it with the path names
!
! Written June 2002 by Charles C Burch
!-----------------------------------------------------------------
  subroutine cnfg_get_tt_names(n_names, names)
    integer, intent(out)     :: n_names
    character(len=*),pointer :: names(:)

    character, allocatable   :: names1(:) !work space for c code
    integer                  :: i, j, istat, n_disks, len_name

    call cnfg_get_tt_info_c(n_names, n_disks)
    if(n_names.le.0) return
    
! - allocate names to proper size
    
    if(associated(names)) then
      if(size(names).ne.n_names) deallocate(names,stat=istat)
    endif

    if(.not.associated(names)) then
      allocate(names(n_names), stat=istat)
    endif
    len_name=len(names(1))

    allocate(names1(1:n_names*len_name),stat=istat)
    call cnfg_get_tt_names_c(n_names,names1(1),len_name)

    do i=1,n_names         !put names in output varaiables
      do j=1,len_name
        names(i)(j:j)=names1((i-1)*len_name+j)
      enddo
    enddo

    deallocate(names1,stat=istat)
    return
  end subroutine cnfg_get_tt_names

!-----------------------------------------------------------------
! For a given path name, get the number of disks for the path,
! allocate disks to be the proper size and place the disk names
! in it
!
! Written June 2002 by Charles C Burch
!-----------------------------------------------------------------
  subroutine cnfg_get_tt_set_info(name, n_disks, disks)
    character(len=*), intent(in)                :: name
    integer, intent(inout)                      :: n_disks
    character(len=*), pointer                   :: disks(:)

    character,allocatable         :: disks1(:)  !for c code
    character                     :: name1(len(name))
    integer                       :: n_names, i, j, istat, len_disk
    integer                       :: temp

    call cnfg_get_tt_info_c(n_names, n_disks)
    if(n_disks.le.0) return

! - allocate disks to proper size
    if(associated(disks)) then
      if(size(disks).ne.n_disks) deallocate(disks,stat=istat)
    endif

    if(.not.associated(disks)) then
      allocate(disks(n_disks), stat=istat)
    endif
    len_disk=len(disks(1))

    allocate(disks1(1:len_disk*n_disks), stat=istat)
    
    do i=1,len(name)
      name1(i) = name(i:i)
    enddo
    temp = size(name1)
    call cnfg_get_tt_set_c(name1(1), temp, disks1(1), n_disks, &
     len_disk)
    
! - place disk names into output array
    do i=1,n_disks
      do j=1,len_disk
        disks(i)(j:j)=disks1((i-1)*len_disk+j)
       enddo
    enddo

    deallocate(disks1,stat=istat)
    return
  end subroutine cnfg_get_tt_set_info
  

end module cnfg_module


!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!
