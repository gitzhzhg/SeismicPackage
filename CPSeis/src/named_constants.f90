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
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------- named_constants.f90 ---------------------------!!
!!------------------------- named_constants.f90 ---------------------------!!
!!------------------------- named_constants.f90 ---------------------------!!

                ! other files are:  named_constants.h




      module named_constants_module
      implicit none

      character(len=100),public,save :: NAMED_CONSTANTS_IDENT = &
       '$Id: named_constants.f90,v 1.7 2000/10/19 19:13:53 sps prod sps $'


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : NAMED_CONSTANTS 
! Category   : miscellaneous
! Written    : 1999-06-07   by: Tom Stoeckley
! Revised    : 2000-10-09   by: Tom Stoeckley
! Maturity   : production   2000-10-19
! Purpose    : Defines various named constants used in seismic processing.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  There are no subroutines in this module (although a few could be added
!  if appropriate).
!
!  This module contains various named constants of general use in the
!  various components of the seismic processing system.  These constants
!  are not directly related to any single process or primitive and have
!  been collected here for convenience.  These constants are listed in
!  the following separate sections.
!
!  When additional useful constants are recognized, they should be added
!  to this module.
!
!  There is a C header file called named_constants.h which is part of this
!  primitive and should be kept up-to-date and consistent with this module.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                             NIL VALUES
!
       integer         ,parameter :: INIL = -888728
       real            ,parameter :: FNIL = -1.0e-30
       double precision,parameter :: DNIL = -1.0d-30
       character(len=1),parameter :: CNIL = ' '
       logical         ,parameter :: LNIL = .false.
!
!-------------------------------------------------------------------------------
!                          FILE NAME LENGTH    
!
       integer,parameter :: FILENAME_LENGTH = 140
!
! The above named constant should be used to specify the length of character
! variables which contain names of disk files.  The intention is that this
! length should be large enough for any likely path (including remote user
! ID, remote node, and absolute directory path if necessary).
!
!-------------------------------------------------------------------------------
!                          TRACE FLOW CONSTANTS
!
! The following named trace-flow constants are useful for processing modules
! and the programs which call them (which might be processing systems or other
! processing modules or any program which uses a processing module).  These
! constants are provided here for convenient access by processing modules
! and their calling programs.
!
       integer,parameter :: NO_MORE_TRACES   =  0
       integer,parameter :: FATAL_ERROR      = -1
       integer,parameter :: NEED_TRACES      = -2
!
! One of the arguments to the processing module "execution" subroutine
! (which processes traces) is NTR (number of traces input and output).  The
! zero and negative values of NTR have the special meanings indicated by the
! above trace-flow constants.
!
! Upon input to a process, NTR must have one of these values:
!
!     NTR >= 1              means to process the input traces.
!     NTR == NO_MORE_TRACES means there are no more imput traces.
!     NTR == NEED_TRACES    means someone from below needs more traces
!                            (must not occur unless this process has a label).
!
! Upon output from a process, NTR must have one of these values:
!
!     NTR >= 1              if the process is outputting traces.
!     NTR == NO_MORE_TRACES if there are no more traces to output.
!     NTR == FATAL_ERROR    if the process has a fatal error.
!     NTR == NEED_TRACES    if the process needs another trace before
!                             passing any out (must not occur unless the
!                             process specified that it might need to
!                             request more traces).
!
!-------------------------------------------------------------------------------
!                         TRACE HEADER WORD INDICES
!
     integer,parameter :: HDR_NOMINAL_SIZE              = 64

     integer,parameter :: HDR_SEQUENCE                  =  1
     integer,parameter :: HDR_TOP_MUTE                  =  2
     integer,parameter :: HDR_CURRENT_GROUP             =  3
     integer,parameter :: HDR_CURRENT_CHANNEL           =  4
     integer,parameter :: HDR_FOLD                      =  5
     integer,parameter :: HDR_OFFSET                    =  6
     integer,parameter :: HDR_MIDPOINT_XGRID            =  7
     integer,parameter :: HDR_MIDPOINT_YGRID            =  8
     integer,parameter :: HDR_ORIGINAL_GROUP            =  9
     integer,parameter :: HDR_ORIGINAL_CHANNEL          = 10
     integer,parameter :: HDR_SOURCE_XLOC               = 11
     integer,parameter :: HDR_SOURCE_YLOC               = 12
     integer,parameter :: HDR_SOURCE_ELEV               = 13
     integer,parameter :: HDR_RECEIVER_XLOC             = 14
     integer,parameter :: HDR_RECEIVER_YLOC             = 15
     integer,parameter :: HDR_RECEIVER_ELEV             = 16
     integer,parameter :: HDR_MIDPOINT_XLOC             = 17
     integer,parameter :: HDR_MIDPOINT_YLOC             = 18
     integer,parameter :: HDR_MIDPOINT_ELEV             = 19
     integer,parameter :: HDR_SOURCE_DEPTH              = 20
     integer,parameter :: HDR_RECEIVER_DEPTH            = 21
     integer,parameter :: HDR_SOURCE_COMPONENT          = 22
     integer,parameter :: HDR_RECEIVER_COMPONENT        = 23
     integer,parameter :: HDR_PANEL                     = 24
     integer,parameter :: HDR_LAV                       = 25
     integer,parameter :: HDR_SOURCE_LINE               = 26
     integer,parameter :: HDR_RECEIVER_LINE             = 27
     integer,parameter :: HDR_RECEIVER_SHOTPOINT        = 28
     integer,parameter :: HDR_SOURCE_SHOTPOINT          = 29
     integer,parameter :: HDR_SCRATCH_30                = 30
     integer,parameter :: HDR_SCRATCH_31                = 31
     integer,parameter :: HDR_SCRATCH_32                = 32
     integer,parameter :: HDR_SOURCE_XGRID              = 33
     integer,parameter :: HDR_SOURCE_YGRID              = 34
     integer,parameter :: HDR_RECEIVER_XGRID            = 35
     integer,parameter :: HDR_RECEIVER_YGRID            = 36
     integer,parameter :: HDR_MIDPOINT_SHOTPOINT        = 37
     integer,parameter :: HDR_MIDPOINT_LINE             = 38
     integer,parameter :: HDR_PRE                       = 39
     integer,parameter :: HDR_POST                      = 40
     integer,parameter :: HDR_CUM_DATUM_STATIC          = 41
     integer,parameter :: HDR_CUM_REFR_STATIC           = 42
     integer,parameter :: HDR_CUM_RESID_STATIC          = 43
     integer,parameter :: HDR_SOURCE_UPTIME             = 44
     integer,parameter :: HDR_RECEIVER_UPTIME           = 45
     integer,parameter :: HDR_SOURCE_GP                 = 46
     integer,parameter :: HDR_RECEIVER_GP               = 47
     integer,parameter :: HDR_USER_48                   = 48
     integer,parameter :: HDR_USER_49                   = 49
     integer,parameter :: HDR_USER_50                   = 50
     integer,parameter :: HDR_USER_51                   = 51
     integer,parameter :: HDR_USER_52                   = 52
     integer,parameter :: HDR_USER_53                   = 53
     integer,parameter :: HDR_USER_54                   = 54
     integer,parameter :: HDR_USER_55                   = 55
     integer,parameter :: HDR_RPRE                      = 56
     integer,parameter :: HDR_RPOST                     = 57
     integer,parameter :: HDR_SCRATCH_58                = 58
     integer,parameter :: HDR_SCRATCH_59                = 59
     integer,parameter :: HDR_SCRATCH_60                = 60
     integer,parameter :: HDR_SCRATCH_61                = 61
     integer,parameter :: HDR_SCRATCH_62                = 62
     integer,parameter :: HDR_GVS_MODIFIER              = 63
     integer,parameter :: HDR_BOTTOM_MUTE               = 64
!
!-------------------------------------------------------------------------------
!                         MATHEMATICAL CONSTANTS
!
     double precision,parameter :: PI                 =  3.1415926535898d0
     double precision,parameter :: RADIANS_PER_DEGREE =  PI / 180.0d0
     double precision,parameter :: DEGREES_PER_RADIAN =  180.0d0 / PI
!
!-------------------------------------------------------------------------------
!                           PHYSICAL CONSTANTS
!
!   --> Constants will be placed here.
!
!-------------------------------------------------------------------------------
!                         OPERATING SYSTEM FLAGS
!
!   --> Constants will be placed here if this is the logical place for them.
!
!-------------------------------------------------------------------------------
!                              DEBUG FLAGS
!
!   --> Constants will be placed here unless they are placed into the
!   --> parameter cache, which might be the logical place for them.
!
!-------------------------------------------------------------------------------
!                         C-LANGUAGE POINTERS
!
! C-language pointers passed to and from Fortran should have the following
! Fortran derived type.  Usually, such pointers can be stored in integer
! variables, but that does not provide enough room on some platforms.
!
     type CPOINTER
        private
        integer a(2)
     end type CPOINTER
!
!-------------------------------------------------------------------------------
!</calling_doc>



!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  7. 2000-10-19  Stoeckley  Add missing required documentation section.
!  6. 2000-01-28  Stoeckley  Add FILENAME_LENGTH constant.
!  5. 1999-12-29  Stoeckley  Change constants involving PI to consistent
!                             accuracies.
!  4. 1999-11-17  Stoeckley  Add ident string for RCS.
!  3. 1999-09-23  Stoeckley  Moved to the miscellaneous subdirectory.
!  2. 1999-09-10  Stoeckley  Add reference to other files.
!  1. 1999-06-07  Stoeckley  Initial version.
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
!                            PROGRAMMING NOTES
!
!  1. In order to eliminate the need to list each constant twice (once in the
!     documentation and once in the code), the comment character in the first
!     column of the documentation for each constant is removed so the line
!     can serve as both the documentation and the source code for the compiler.
!
!-------------------------------------------------------------------------------
!</programming_doc>


      end module named_constants_module


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

