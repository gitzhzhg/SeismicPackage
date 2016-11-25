!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-06-29. />

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
!                         C P S   P R O C E S S                  
!
! Name       : SELDMO
! Category   : sorts
! Written    : 1990-12-19   by: Mike Howard
! Revised    : 2000-12-07   by: Chiu
! Maturity   : production   2001-06-04   
! Purpose    : Select traces for DMO or stack from an input swath of lines.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
!
! For MODE = DMO, SELDMO will select traces from an input dataset that are 
! appropriate for a DMO process by deleting traces from the job stream if a line
! joining source and the receiver lies entirely outside the range CRL_BEG -0.5 
! to CRL_END +0.5.
!
! For MODE = STK, traces are deleted if their midpoint lies outside the range 
! CRL_BEG -0.5 to CRL_END +0.5.
! 
! In either mode, SELDMO prints in the .RPT file the number of traces selected 
! that belong to each inline.  (Within this process, an "inline" is defined as 
! a set of traces whose value of HDR_CRL rounds to a specific whole number.)
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Report-Only Operation
!
! If CRL_BEG is given a very small value and CRL_END a very large value, so 
! that no traces are deleted from the input dataset, then SELDMO prints the
! number of traces belonging to each inline, for the entire input dataset.  
! This is a useful diagnostic tool in 3D processing.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
! This process does not alter input traces.
! This process may delete some input traces.
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED          
!
! 
! Name     Description                           Action taken
! ----     -----------                           ------------
! IPN      process number                        used (must not be changed)
! GRID     GRID primitive                        used (values not changed)
! 
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! 4       Trace number within gather Renumbered.
!         HDR_CRL                    Occupation values printed.
! 
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author    Description
!     ----        ------    -----------
! 12  2001-06-04  S Chiu    Change wrapped_up to skip_wrapup.
! 11  2000-08-07  S Chiu    Use new coordinate for header 7 and 8.
! 10  2000-06-14  S Chiu    Added Gui parameters.
! 9   1999-12-15  S Chiu    Update documentation to the standard.
! 8   1999-12-13  S Chiu    Change grid parameters to double precision.
! 7   1999-11-04  S Chiu    Convert to new CPS. 
! 6.  1998-11-20  Vunderink Begin using the f90 compiler.
! 5.  1998-04-15  Vunderink Allow HDLINE to be 37 or 38 when MODE=STK
! 4.  1997-12-29  Vunderink Added HDLINE parameter.
! 3.  1992-07-02  M Howard  Fix bug in rejection of traces at bin edges.
! 2.  1991-01-15  M Howard  Add MODE=STK.
! 1.  1990-12-19  M Howard  Original version. 
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
!
! No known limitations.
! 
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
!
! 
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS            
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS          
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS         
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!<gui_def>
!<NS SELDMO Process/NC=80>
!
! Select traces for DMO or stack from an input swath of lines.
!
!   MODE=~~~`CCC            HDR_CRL=`IIIIII    
!
!   CRL_BEG=`FFFFFFFFFFF    CRL_END=`FFFFFFFFFFF
!</gui_def>                                  
!
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Select traces for a DMO or a stack process. </Tip>
! Default = DMO
! Allowed = DMO
! Allowed = STK
! In MODE = DMO, SELDMO selects traces for a DMO process for the swath defined 
! by the range LINE_BEG -.5 to LINE_END +.5.
!
! In MODE = STK, SELDMO selects traces for a stack process for the swath 
! defined by the range LINE_BEG -.5 to LINE_END +.5.
!</Help>
!
!<Help KEYWORD="HDR_CRL">
!<Tip> Header word for crossline coordinate. </Tip>
! Default = 8
! Allowed = 7 or 8 (for DMO)
! Allowed = 7 or 8 or 37 or 38 (for STK)
! HDR_CRL is used to define the desired crossline coordinate range.  SELDMO 
! also prints in the .RPT file the number of selected traces in each inline. 
! (Within this process, an "inline" is defined as a set of traces whose value 
! of HDR_CRL rounds to a specific whole number.)
!</Help>
!
!<Help KEYWORD="CRL_BEG">
!<Tip> Minimum value in desired crossline coordinate range. </Tip>
! Default = 0.0
! Allowed = real
! Actual minimum value used is CRL_BEG - 0.5.
!</Help>
!
!<Help KEYWORD="CRL_END">
!<Tip> Maximum value in desired crossline coordinate range. </Tip>
! Default = 1.0
! Allowed = real > CRL_BEG
! Actual maximum value used is CRL_END + 0.5.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
! NOTES FOR CONVERSION PROGRAMMER
!

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module seldmo_module
      use pc_module
      use named_constants_module
      use grid_module             ! need the grid transformation.
      implicit none
      private
      public :: seldmo_create     ! uses the parameter cache.
      public :: seldmo_initialize
      public :: seldmo_update     ! uses the parameter cache.
      public :: seldmo_delete

!<execute_only>

      public :: seldmo            ! main execution (trace processing) routine.
      public :: seldmo_wrapup

!</execute_only>

      character(len=100),public,save :: seldmo_IDENT = &
       '$Id: seldmo.f90,v 1.12 2001/05/31 13:48:31 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: seldmo_struct              
 
        private
        logical                    :: skip_wrapup       ! wrapup flag.

        character(len=8)           :: mode       ! process parameters.
        integer                    :: hdr_crl   ! process parameters.
        integer                    :: crl_beg   ! process parameters.
        integer                    :: crl_end   ! process parameters.
        integer                    :: nstor      ! dependent variables.
        integer                    :: ncount     ! dependent variables.
        integer                    :: nreject    ! dependent variables. 

        integer,           pointer :: lcount(:)  ! dependent variables. 
        type(grid_struct)          :: grid       ! dependent variables.
        integer                    :: print_lun  ! dependent variables. 

      end type seldmo_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(seldmo_struct),pointer,save :: object      ! needed for traps. 

      integer,parameter     :: mode_noptions = 2
      character(len=3),save :: mode_options(mode_noptions)
      data mode_options/'DMO','STK'/

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine seldmo_create (obj)
      implicit none
      type(seldmo_struct),pointer :: obj       ! arguments

      allocate (obj)

!!  Nullify ALL POINTERS in your parameter structure :

      nullify  (obj%lcount)              ! must be done for all pointers.

      call seldmo_initialize (obj)

      return
      end subroutine seldmo_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine seldmo_delete (obj)
      implicit none
      type(seldmo_struct),pointer :: obj       ! arguments

!<execute_only>
      call seldmo_wrapup (obj)
!</execute_only>

!! Make sure ALL POINTERS in your parameter structure are deallocated

      if (associated(obj%lcount )) deallocate(obj%lcount)
      deallocate(obj)

      return
      end subroutine seldmo_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine seldmo_initialize (obj)
      implicit none
      type(seldmo_struct),pointer :: obj       ! arguments

      obj%mode ='DMO'      
      obj%hdr_crl = 8  
      obj%crl_beg = 0   
      obj%crl_end = 0  
      obj%ncount   = 0 
      obj%nreject  = 0      
      obj%print_lun = pc_get_lun()

      call grid_initialize(obj%grid)
      
      call seldmo_update (obj)

      return
      end subroutine seldmo_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine seldmo_update (obj)
      implicit none
      type(seldmo_struct),target :: obj                           ! arguments

      integer                    :: ier1                          ! local
      integer                    :: nstore, nscratch              ! local
      character(len=3)           :: need_label,need_request       ! local
      character(len=3)           :: twosets                       ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('grid', obj%grid)  ! grid transform data structure.

      call pc_get ('mode'    , obj%mode,     seldmo_mode_trap)
      call pc_get ('hdr_crl', obj%hdr_crl, seldmo_hdr_crl_trap)            
      call pc_get ('crl_beg', obj%crl_beg)
      call pc_get ('crl_end', obj%crl_end)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if (obj%crl_beg < 0 ) then
        call pc_error ('Invalid: CRL_BEG  must >= 0')
      end if          
      if (obj%crl_end < obj%crl_beg) then
        call pc_error ('Invalid: CRL_END  must >= CRL_BEG')
      end if 

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('mode', mode_options,   &
                                   mode_noptions)

      call pc_put ('mode'    , obj%mode)
      call pc_put ('hdr_crl', obj%hdr_crl)            
      call pc_put ('crl_beg', obj%crl_beg)
      call pc_put ('crl_end', obj%crl_end)

 
        need_label   = 'NO'
        need_request = 'YES'
        twosets      = 'NO'
 
        nstore   = obj%crl_end - obj%crl_beg + 1
        nscratch = 0
 
        call pc_put_control ('nstore',             nstore)
        call pc_put_control ('nscratch',         nscratch)
        call pc_put_control ('need_label',     need_label)
        call pc_put_control ('need_request', need_request)
        call pc_put_control ('twosets',           twosets)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!!  Conditionally deallocate all arrays like this:
!!
    if (associated(obj%lcount)) deallocate (obj%lcount)   

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      obj%nstor = obj%crl_end - obj%crl_beg + 1 
      allocate(obj%lcount(obj%nstor),stat=ier1)
      if (ier1 /= 0) call pc_error ('Seldmo: allocate error of lcount')
      obj%lcount = 0

      if (pc_do_not_process_traces()) return

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine seldmo_update

!!------------------------- traps ------------------------------------------!!
!!------------------------- traps ------------------------------------------!!
!!------------------------- traps ------------------------------------------!!


      subroutine seldmo_mode_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments

!    Convert mode to upper case

      if(object%mode(1:1)=='S' .or. object%mode(1:1)=='s') then
        object%mode='STK'
      else
        object%mode='DMO'
      endif
      return
      end subroutine seldmo_mode_trap

      subroutine seldmo_hdr_crl_trap(keyword)

      implicit none
      character(len=*),intent(in) :: keyword           ! arguments

      if(object%mode(1:1)=='S' .or. object%mode(1:1)=='s' ) then
        if (object%hdr_crl /= 7  .and. object%hdr_crl /= 8 .and.   &
            object%hdr_crl /= 37 .and. object%hdr_crl /= 38  ) then
           call pc_error ('Invalid HDR_CRL value.  must be 7,8,37, or 38.')
         endif
      else
         if (object%hdr_crl /= 7  .and. object%hdr_crl /= 8) then
            call pc_error ('Invalid HDR_CRL value.  must be 7 or 8')
         endif
      endif
      return
      end subroutine seldmo_hdr_crl_trap

!!------------------------- end session traps ------------------------------!!
!!------------------------- end session traps ------------------------------!!
!!------------------------- end session traps ------------------------------!!

!<execute_only>

      subroutine seldmo (obj,ntr,hd,tr)
      implicit none
      type(seldmo_struct)             :: obj                    ! arguments
      integer         ,intent(inout)  :: ntr                    ! arguments
      double precision,intent(inout)  :: hd(:,:)                ! arguments
      real            ,intent(inout)  :: tr(:,:)                ! arguments
     
      real             :: sg, rg                                ! local
      integer          :: imin, imax, i                         ! local

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR .or. ntr > 1) then
           call seldmo_wrapup (obj)
      end if 
       
      if (ntr > 0) then 
        obj%ncount = obj%ncount + 1 
        if (obj%mode /= 'STK') then 
          if (obj%hdr_crl == 7) then 
            sg = grid_get_xgrid_coord(obj%grid, hd(11,1), hd(12,1) )
            rg = grid_get_xgrid_coord(obj%grid, hd(14,1), hd(15,1) )   
          else 
            sg = grid_get_ygrid_coord(obj%grid, hd(11,1), hd(12,1) )
            rg = grid_get_ygrid_coord(obj%grid, hd(14,1), hd(15,1) ) 
          endif 

          if ( (sg<obj%crl_beg-0.5 .and. rg<obj%crl_beg-0.5)          & 
           .or.(sg>obj%crl_end+0.5 .and. rg>obj%crl_end+0.5) ) then 
              obj%nreject = obj%nreject + 1 
              ntr = NEED_TRACES 
              return  
          endif
 
          imin = nint(min(sg,rg)) 
          imin = max(imin,obj%crl_beg) 
          imax = nint(max(sg,rg)) 
          imax = min(imax,obj%crl_end) 
          obj%lcount(imin-obj%crl_beg+1:imax+1-obj%crl_beg) =         &
          obj%lcount(imin-obj%crl_beg+1:imax+1-obj%crl_beg) + 1 
          return  
        else 
          if (nint(hd(obj%hdr_crl,1))<obj%crl_beg .or.  &
              nint(hd(obj%hdr_crl,1))>obj%crl_end ) then 
            obj%nreject = obj%nreject + 1 
            ntr = NEED_TRACES  
            return  
          endif 
          obj%lcount(nint(hd(obj%hdr_crl,1))-obj%crl_beg+1) =          &
          obj%lcount(nint(hd(obj%hdr_crl,1))-obj%crl_beg+1) + 1 
          return  
        endif 
      else                                       ! ntr=0 
        write(obj%print_lun, *) 'seldmo: traces scanned =', obj%ncount,  & 
                            ' traces rejected=', obj%nreject 
        do i = obj%crl_beg, obj%crl_end 
          if (obj%lcount(i-obj%crl_beg+1) == 0) cycle  
          write(obj%print_lun, *) 'traces contributing to line #', i,    & 
            ' = ', obj%lcount(i-obj%crl_beg+1) 
        end do 
      endif 
      return
      end subroutine seldmo

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine seldmo_wrapup (obj)
      implicit none
      type(seldmo_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine seldmo_wrapup

!</execute_only>

!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module seldmo_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

