!<CPS_v1 type="PROCESS"/>
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
! Name       : PROJECT_DATA
! Category   : miscellaneous
! Written    : 1999-06-14   by: Donna K. Vunderink
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Project-oriented parameter entry for project-specific parameters
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! 
! The project-oriented parameter entry for all parameters specific to the
! particular project.  These parameters are unlikely to change for the
! duration of the project.
!
! The USER_LEVEL parameter was added to allow module programmers the ability
! to control whether or not users can change the value of certain parameters
! by using subroutines from the conditional_pc module to get and put
! parameters. The three possible settings for USER_LEVEL affect how the
! conditional_pc routines behave:
!
!     REGULAR - parameters protected by the use of the conditional get and
!               put routines defined in the conditional_pc module will be
!               desensitized, that is, the user will not be able to change
!               them. Protected parameters can have nondefault values, but
!               the user is warned of these values when the work file is
!               loaded. This is the default value for user_level.
!
!      EXPERT - protected parameters' sensitivity is controlled by module
!               requirements and is not automatically desensitized. The
!               user can set protected parameters to any value the module 
!               allows.
!
!  RESTRICTED - protected parameters will be desensitized. Protected
!               parameters will be forced to have default values when the
!               work file is loaded or when the USER_LEVEL on the project_data
!               screen is changed to RESTRICTED. The user is warned of these
!               parameter value changes when they occur.
! 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! 
! 
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This process is a setup only process.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process is a setup only process.
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
! GRID     grid transformation structure         changed
!
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! This process is a setup only process.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!022. 2006-10-16  D. Glover    Added NULLIFY statements for Intel compiler.
! 21. 2006-01-10  B. Menger    Removed Unused Variables.
! 20. 2005-07-11  Goodger      Make the Project Name field a combo box.  Get
!                              valid project names from the cpsdata_nodes.dat
!                              file.
! 19. 2004-01-29  Goodger      Do not allow blanks in PROJECT or ACCOUNT.
! 18. 2003-07-30  Done         Add USER_LEVEL parameter.
! 17. 2002-03-14  Vunderink    Set grid transform parameters on second screen
!                                instead of separate window, add file select to
!                                jd file and activate control point arrays.
! 16. 2001-08-10  Vunderink    Added project_data_wrapup subroutine.
! 15. 2001-03-20  Vunderink    Increased PROJECT to 15 characters.
! 14. 2001-03-13  Vunderink    Increased field width for grid transform
!                                parameters
! 13. 2000-10-12  Vunderink    Added PATHNAME to gridtrans parameters.
! 12. 2000-08-31  Vunderink    Fixed bug of grid transform parameters not
!                                displaying on project_data screen.
! 11. 2000-08-23  Vunderink    Made parameter name changes required by
!                                coordinate naming standards, moved rec_keeping
!                                to JOB_DATA process
! 10. 2000-05-12  Vunderink    Added SETUP_ONLY to control parameters
!  9. 2000-03-24  Vunderink    Removed TOP.LAY and BOT.LAY includes from gui_def
!                                layout
!  8. 2000-03-07  Vunderink    Set default user_name
!  7. 2000-02-23  Vunderink    Changed project_name and user_name length to 10,
!                                added arrayset to gridtrans keywords, and
!                                added gui definition to documentation
!  6. 2000-02-10  Vunderink    Previous fix still left xwidth and ywidth not
!                                saved into workfile.  This one should save all
!                                gridtrans parameters to workfile.
!  5. 2000-02-07  Vunderink    Fixed bug of gridtrans changes not getting saved
!                                into workfile
!  4. 2000-02-04  Vunderink    Made parameter change required by Chuck I. Burch
!                                documentation changes, and made REC_KEEPING1
!                                insensitive until it is supported.
!  3. 2000-02-01  Vunderink    Added RECALCULATE to list of gridtrans keywords
!  2. 2000-01-19  Vunderink    Added calls to gridtrans primitive to support
!                               GRID_TRANSFORM_PARAMETERS button, added RCS "Id"
!                               string, merged trace units type and trace units
!                               into single option field, and commented out
!                               unused REC_KEEPING2 parameter
!  1. 1999-06-15  Vunderink    Initial version.
!
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
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process is a setup only process.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! 
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>

!<gui_def>
!
!<NS PROJECT_DATA Process>
!
!     [/L]Project Name~~=~~~[PROJECT]`CCCCCCCCCCCCCCCCCCCCCCC             [/L]Trace Sample Unit~~= [TR_UNITS_COMBO]`CCCCCCCCCCCCCCCCCCC
!     [/L]Account Number~~= [ACCOUNT]`SSSSSSSSSSSSSSSSSSSSSSS
!     [/L]User Name~~=~~~~~~[USER_NAME]`SSSSSSSSSSSSSSSSSSSSSSS             [/L]Survey Units~~=~~~~~~[SURVEY_UNITS]`CCCCCCC
!     [/L]Address~~=~~~~~~~~[ADDRESS]`SSSSSSSSSSSSSSSSSSSSSSS
!     [/L]Routing Name~~=~~~[ROUTING_NAME]`SSSSSSSSSSSSSSSSSSSSSSS
!     [/L]Phone~~=~~~~~~~~~~[PHONE]`SSSSSSSSSSSSSSSSSSSSSSS             [/L]User Level~~=~~~~~~~~[USER_LEVEL]`CCCCCCCCC
!
!
!
!
!
!
!
!
!
!
!     `-------------------------------------------------------------------------------------------------+
!     | [/L]Easting Origin~~=~~[GUI_ONLY_ORIGIN_EAST]`XXXXXXXXXXXXXXXXXX              [/L]Northing Origin~~= [GUI_ONLY_ORIGIN_NORTH]`XXXXXXXXXXXXXXXXX
!     | [/L]X Grid Distance~~= [GUI_ONLY_X_GRID_DIST]`XXXXXXXXXXXXXXXXXX              [/L]Y Grid Distance~~= [GUI_ONLY_Y_GRID_DIST]`XXXXXXXXXXXXXXXXX
!     | [/L]Angle (degrees)~~= [GUI_ONLY_ANGLE]`XXXXXXXXXXXXXXXXXX              [/L]Handedness~~=~~~~~~[GUI_ONLY_HANDEDNESS]`XXXXXXXXXXXXXXXXX
!     `-------------------------------------------------------------------------------------------------+
!
!
!<NS Set Grid Transform Parameters>
!
! [MODE/XST]`CCCCCCCCCCCC    [/XST]Easting Origin~~=~~[ORIGIN_EAST/XST]`FFFFFFFFFFFFFFFFFFFFF    [/XST]Northing Origin~~=[ORIGIN_NORTH/XST]`FFFFFFFFFFFFFFFFFFFFF
!                  [/XST]X Grid Distance~~= [X_GRID_DIST/XST]`FFFFFFFFFFFFFFFFFFFFF    [/XST]Y Grid Distance~~=[Y_GRID_DIST/XST]`FFFFFFFFFFFFFFFFFFFFF
!                  [/XST]Angle (degrees)~~= [ANGLE/XST]`FFFFFFFFFFFFFFFFFFFFF    [/XST]Handedness~~=~~~~~[HANDEDNESS/XST]`CCCCCCCCCCCCCCCCCCCCC
!
!
!                  [/C/XST]Forward Rotation Matrix~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                  [/XST]DX(1,1)~~=~~~~~~~~~[DX11/XST]`FFFFFFFFFFFFFFFFFFFFF    [/XST]DX(1,2)~~=~~~~~~~~[DX12/XST]`FFFFFFFFFFFFFFFFFFFFF
!                  [/XST]DX(2,1)~~=~~~~~~~~~[DX21/XST]`FFFFFFFFFFFFFFFFFFFFF    [/XST]DX(2,2)~~=~~~~~~~~[DX22/XST]`FFFFFFFFFFFFFFFFFFFFF
!
!
!                  [/C/XST]Inverse Rotation Matrix~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                  [/XST]DN(1,1)~~=~~~~~~~~~[DN11/XST]`FFFFFFFFFFFFFFFFFFFFF    [/XST]DN(1,2)~~=~~~~~~~~[DN12/XST]`FFFFFFFFFFFFFFFFFFFFF
!                  [/XST]DN(2,1)~~=~~~~~~~~~[DN21/XST]`FFFFFFFFFFFFFFFFFFFFF    [/XST]DN(2,2)~~=~~~~~~~~[DN22/XST]`FFFFFFFFFFFFFFFFFFFFF
!
!
!                  Select JD File [PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!
!               [/C/XST]Control Points to Define the Transform~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Survey_East     Survey_North    Grid_X          Grid_Y          X_Residual      Y_Residual          RECALCULATE`P
! `FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
! `FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
! `FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
! `FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
! `FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
! `FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
! `FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF`FFFFFFFFFFFFFFF
!<PARMS SURVEY_EAST_ARRAYSET[/XST/YST]>
!<PARMS PATHNAME[/ML=140/XST]>
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="PROJECT">
!<Tip> Name given to the project. </Tip>
! Default =
! Allowed = char*15
! 
! The name given to the project.  If the user elects to SAVE PROJECT DEFAULTS,
! the defaults are saved and recalled by the project name.
!</Help>
!
!<Help KEYWORD="ACCOUNT">
!<Tip> The accounting code assigned to the project. </Tip>
! Default =
! Allowed = char*24
! 
! The project accounting code.  If an invalid code is entered, the user will be
! warned.
!</Help>
!
!<Help KEYWORD="USER_NAME">
!<Tip> The logon id that the batch job will run under. </Tip>
! Default =
! Allowed = char*10
! 
! The logon id that the batch job will run under.
!</Help>
!
!<Help KEYWORD="ROUTING_NAME">
!<Tip> The name to print on plot output. </Tip>
! Default =
! Allowed = char*16
! 
! The name to print on plot output.
!</Help>
!
!<Help KEYWORD="ADDRESS">
!<Tip> The address to print on plot output. </Tip>
! Default =
! Allowed = char*16
! 
! The address to print on plot output.
!</Help>
!
!<Help KEYWORD="PHONE">
!<Tip> A phone number where computer operations can contact you. </Tip>
! Default =
! Allowed = char*16
! 
! A phone number where computer operations can contact you.
!</Help>
!
!<Help KEYWORD="TR_UNITS_COMBO">
!<Tip>The vertical (trace sample) units for the input seismic data. </Tip>
! Default = TIME - SECONDS
! Allowed = TIME  - SECONDS
!           DEPTH - FEET
!           DEPTH - METERS
!           DEPTH - KILOMETERS
!           DEPTH - KILOFEET
!           FREQ  - HZ
! The vertical (trace sample) units for the input data.
!</Help>
!
!<Help KEYWORD="SURVEY_UNITS">
!<Tip> The horizonal units for the input seismic data. </Tip>
! Default = METERS
! Allowed = METERS
!           FEET
! The horizonal units for the input seismic data.
!</Help>
!
!<Help KEYWORD="USER_LEVEL">
!<Tip> Status assigned to user. </Tip>
! Default = REGULAR
! Allowed = REGULAR    - User warned if nondefault value is read from work
!                        file. Desensitized field for that parameter.
!           EXPERT     - No restrictions on parameter values and no warnings
!                        if nondefault values are used. Field is sensitized
!                        for that parameter.
!           RESTRICTED - force parameters to default values when read from
!                        a work file. Desensitized field for that parameter.
! Various modules in a flow may have some parameters in which default values
! are handled specially. Expert level allows user to load that parameter from
! a work file or change the parameter on the gui with no warnings. Regular
! user level allows protected parameters to be loaded from a work file. If
! value is not the default, a warning is issued, but nondefault value allowed.
! Field is not sensitized, disabling further changes. A restricted user level
! forces protected parameters to have the default value when read from a 
! work file. Field is not sensitized, disabling further changes.
!</Help>
!
!<Help KEYWORD="GUI_ONLY_ORIGIN_EAST">
!<Tip>Easting surveyed coordinate value of origin of the 3D-grid system.</Tip>
! Default = 0.0
! Allowed = real
!
! Easting surveyed coordinate value of the origin of the 3D-grid system.
!</Help>
!
!<Help KEYWORD="GUI_ONLY_ORIGIN_NORTH">
!<Tip>Northing surveyed coordinate value of origin of the 3D-grid system.</Tip>
! Default = 0.0
! Allowed = real
!
! Northing surveyed coordinate value of the origin of the 3D-grid system.
!</Help>
!
!<Help KEYWORD="GUI_ONLY_X_GRID_DIST">
!<Tip>Physical distance associated with unit change in x-grid coordinate.</Tip>
! Default = 1.0
! Allowed = real>0
!
! Physical distance in surveyed coordinate units, feet or meters, associated
! with a unit increment in the x-grid coordinate.  (Usually this will be the
! width of the stack bins in the x-grid direction.)
!</Help>
!
!<Help KEYWORD="GUI_ONLY_Y_GRID_DIST">
!<Tip>Physical distance associated with unit change in y-grid coordinate.</Tip>
! Default = 1.0
! Allowed = real>0
!
! Physical distance in surveyed coordinate units, feet or meters, associated
! with a unit increment in the y-grid coordinate.  (Usually this will be the
! width of the stack bins in the y-grid direction.)
!</Help>
!
!<Help KEYWORD="GUI_ONLY_ANGLE">
!<Tip>Rotation Angle, in degrees, measured counter-clockwise</Tip>
! Default = 0.0
! Allowed = real
!
! The rotation angle, in degrees, measured counter-clockwise from the east
! direction to the positive x-grid coordinate direction.
!</Help>
!
!<Help KEYWORD="GUI_ONLY_HANDEDNESS">
!<Tip>Whether the 3D-grid coordinate system is right- or left-handed.</Tip>
! Default = RIGHT
! Allowed = RIGHT
!           LEFT
!
! Whether the 3D-grid coordinate system is right-handed or left-handed.
!</Help>
!
!<Help KEYWORD="MODE">
!<Tip>Choose between 3D mode and 2D mode for setting the grid parameters</Tip>
! Default = 3D mode
! Allowed = 3D Mode
!           2D Mode
!
! Allows the user to choose 3D mode or 2D mode for setting the grid transform
! parameters.  If 3D mode is selected, all screen elements are sensitive.  If
! 2D mode, only the CMP INTERVAL and the X COORDINATE of one control point will
! be sensitive.
!</Help>
!
!<Help KEYWORD="ORIGIN_EAST">
!<Tip>Easting surveyed coordinate value of origin of the 3D-grid system.</Tip>
! Default = 0.0
! Allowed = real
!
! Easting surveyed coordinate value of the origin of the 3D-grid system.
!</Help>
!
!<Help KEYWORD="ORIGIN_NORTH">
!<Tip>Northing surveyed coordinate value of origin of the 3D-grid system.</Tip>
! Default = 0.0
! Allowed = real
!
! Northing surveyed coordinate value of the origin of the 3D-grid system.
!</Help>
!
!<Help KEYWORD="X_GRID_DIST">
!<Tip>Physical distance associated with unit change in x-grid coordinate.</Tip>
! Default = 1.0
! Allowed = real>0
!
! Physical distance in surveyed coordinate units, feet or meters, associated
! with a unit increment in the x-grid coordinate.  (Usually this will be the
! width of the stack bins in the x-grid direction.)
!</Help>
!
!<Help KEYWORD="Y_GRID_DIST">
!<Tip>Physical distance associated with unit change in y-grid coordinate.</Tip>
! Default = 1.0
! Allowed = real>0
!
! Physical distance in surveyed coordinate units, feet or meters, associated
! with a unit increment in the y-grid coordinate.  (Usually this will be the
! width of the stack bins in the y-grid direction.)
!</Help>
!
!<Help KEYWORD="ANGLE">
!<Tip>Rotation Angle, in degrees, measured counter-clockwise</Tip>
! Default = 0.0
! Allowed = real
!
! The rotation angle, in degrees, measured counter-clockwise from the east
! direction to the positive x-grid coordinate direction.
!</Help>
!
!<Help KEYWORD="HANDEDNESS">
!<Tip>Whether the 3D-grid coordinate system is right- or left-handed.</Tip>
! Default = RIGHT
! Allowed = RIGHT
!           LEFT
!
! Whether the 3D-grid coordinate system is right-handed or left-handed.
!</Help>
!
!<Help KEYWORD="DX11">
!<Tip>Forward rotation matrix</Tip>
! Default = 1.0
! Allowed = real
!
! The rotation matrix for transforming from the Grid Coordinate System to the
! Surveyed Coordinate System.
!</Help>
!
!<Help KEYWORD="DX12">
!<Tip>Forward rotation matrix</Tip>
! Default = 0.0
! Allowed = real
!
! The rotation matrix for transforming from the Grid Coordinate System to the
! Surveyed Coordinate System.
!</Help>
!
!<Help KEYWORD="DX21">
!<Tip>Forward rotation matrix</Tip>
! Default = 0.0
! Allowed = real
!
! The rotation matrix for transforming from the Grid Coordinate System to the
! Surveyed Coordinate System.
!</Help>
!
!<Help KEYWORD="DX22">
!<Tip>Forward rotation matrix</Tip>
! Default = 1.0
! Allowed = real
!
! The rotation matrix for transforming from the Grid Coordinate System to the
! Surveyed Coordinate System.
!</Help>
!
!<Help KEYWORD="DN11">
!<Tip>Inverse rotation matrix</Tip>
! Default = 1.0
! Allowed = real
!
! The rotation matrix for transforming from the Surveyed Coordinate System to
! the Grid Coordinate System.
!</Help>
!
!<Help KEYWORD="DN12">
!<Tip>Inverse rotation matrix</Tip>
! Default = 0.0
! Allowed = real
!
! The rotation matrix for transforming from the Surveyed Coordinate System to
! the Grid Coordinate System.
!</Help>
!
!<Help KEYWORD="DN21">
!<Tip>Inverse rotation matrix</Tip>
! Default = 0.0
! Allowed = real
!
! The rotation matrix for transforming from the Surveyed Coordinate System to
! the Grid Coordinate System.
!</Help>
!
!<Help KEYWORD="DN22">
!<Tip>Inverse rotation matrix</Tip>
! Default = 1.0
! Allowed = real
!
! The rotation matrix for transforming from the Surveyed Coordinate System to
! the Grid Coordinate System.
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for the .jd file written by CFG. </Tip>
! Default = 
! Allowed = char
!
! The pathname for a JD file to open and load the rotation matrix with the
! values found in the file header.
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> This button accesses the Select JD file dialog box. </Tip>
! Choose a JD file via a file selection dialog.
!</Help>
!
!<Help KEYWORD="SURVEY_EAST">
!<Tip>Control point pairs used to calculate the rotation maxtrix</Tip>
! Default =
! Allowed = real
!
! The user may enter a list of control point pairs and push the RECALCULATE
! button at which time the front-end will calculate a grid transform for the
! user.  If four or more control point pairs are entered, then self-consistence
! is calculated and the residuals are displayed in the X_RESID and Y_RESID
! arrays.
!</Help>
!
!<Help KEYWORD="SURVEY_NORTH">
!<Tip>Control point pairs used to calculate the rotation maxtrix</Tip>
! Default = 
! Allowed = real
!
! The user may enter a list of control point pairs and push the RECALCULATE
! button at which time the front-end will calculate a grid transform for the
! user.  If four or more control point pairs are entered, then self-consistence
! is calculated and the residuals are displayed in the X_RESIDUAL and Y_RESIDUAL
! arrays.
!</Help>
!
!<Help KEYWORD="GRID_X">
!<Tip>Control point pairs used to calculate the rotation maxtrix</Tip>
! Default =
! Allowed = real
!
! The user may enter a list of control point pairs and push the RECALCULATE
! button at which time the front-end will calculate a grid transform for the
! user.  If four or more control point pairs are entered, then self-consistence
! is calculated and the residuals are displayed in the X_RESIDUAL and Y_RESIDUAL
! arrays.
!</Help>
!
!<Help KEYWORD="GRID_Y">
!<Tip>Control point pairs used to calculate the rotation maxtrix</Tip>
! Default =
! Allowed = real
!
! The user may enter a list of control point pairs and push the RECALCULATE
! button at which time the front-end will calculate a grid transform for the
! user.  If four or more control point pairs are entered, then self-consistence
! is calculated and the residuals are displayed in the X_RESIDUAL and Y_RESIDUAL
! arrays.
!</Help>
!
!<Help KEYWORD="X_RESIDUAL">
!<Tip>Difference between input and calculated grid coordinates</Tip>
! Default =
! Allowed = real
!
! The user may enter a list of control point pairs and push the RECALCULATE
! button at which time the front-end will calculate a grid transform for the
! user.  If four or more control point pairs are entered, then self-consistence
! is calculated and the residuals are displayed in the X_RESIDUAL and Y_RESIDUAL
! arrays.
!</Help>
!
!<Help KEYWORD="Y_RESIDUAL">
!<Tip>Difference between input and calculated grid coordinates</Tip>
! Default =
! Allowed = real
!
! The user may enter a list of control point pairs and push the RECALCULATE
! button at which time the front-end will calculate a grid transform for the
! user.  If four or more control point pairs are entered, then self-consistence
! is calculated and the residuals are displayed in the X_RESIDUAL and Y_RESIDUAL
! arrays.
!</Help>
!
!<Help KEYWORD="RECALCULATE">
!<Tip>Recalculate grid transform</Tip>
!
! The user may enter a list of control point pairs and push the RECALCULATE
! button at which time the front-end will calculate a grid transform for the
! user.  If four or more control point pairs are entered, then self-consistence
! is calculated and the residuals are displayed in the X_RESIDUAL and Y_RESIDUAL
! arrays.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module project_data_module
      use alphasort_module
      use cps_module
      use cnfg_module
      use getlun_module
      use geomio_module
      use getsys_module
      use grid_module
      use named_constants_module
      use nthcol_module
      use pathcheck_module
      use pathchoose_module
      use pc_module

      implicit none
      private
      public :: project_data_create     ! uses the parameter cache.
      public :: project_data_initialize ! uses the parameter cache.
      public :: project_data_update     ! uses the parameter cache.
      public :: project_data_delete
      public :: project_data_wrapup

      character(len=100),public,save :: project_data_ident = &
       '$Id: project_data.f90,v 1.22 2006/10/17 13:45:46 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: project_data_struct              
 
        private
        logical                   :: skip_wrapup            ! wrapup flag
        character(len=15)         :: project                ! process parameter
        character(len=24)         :: account                ! process parameter
        character(len=10)         :: user_name              ! process parameter
        character(len=16)         :: routing_name           ! process parameter
        character(len=16)         :: address                ! process parameter
        character(len=16)         :: phone                  ! process parameter
        character(len=10)         :: user_level             ! process parameter
        character(len=8)          :: tr_units_typ           ! process parameter
        character(len=8)          :: tr_units               ! process parameter
        character(len=8)          :: survey_units           ! process parameter
 
        character(len=20)         :: tr_units_combo         ! gui-only parameter

        integer                   :: ntr_units_combo_menu   ! option menu
        character(len=20)         :: tr_units_combo_menu(6) ! option menu
        integer                   :: nsurvey_units_menu     ! option menu
        character(len=8)          :: survey_units_menu(2)   ! option menu
        integer                   :: nproject
        character(len=15),pointer :: project_menu(:)
        integer                   :: nuser_level_menu       ! option menu
        character(len=10)         :: user_level_menu(3)     ! option menu

        double precision          :: gui_only_origin_east   ! dependent variable
        double precision          :: gui_only_origin_north  ! dependent variable
        double precision          :: gui_only_x_grid_dist   ! dependent variable
        double precision          :: gui_only_y_grid_dist   ! dependent variable
        double precision          :: gui_only_angle         ! dependent variable
        character(len=8)          :: gui_only_handedness    ! dependent variable

        character(len=8)          :: mode                   ! process parameter
        double precision          :: origin_east            ! process parameter
        double precision          :: origin_north           ! process parameter
        double precision          :: x_grid_dist            ! process parameter
        double precision          :: y_grid_dist            ! process parameter
        double precision          :: angle                  ! process parameter
        character(len=8)          :: handedness             ! process parameter
        double precision          :: dx11                   ! process parameter
        double precision          :: dx12                   ! process parameter
        double precision          :: dx21                   ! process parameter
        double precision          :: dx22                   ! process parameter
        double precision          :: dn11                   ! process parameter
        double precision          :: dn12                   ! process parameter
        double precision          :: dn21                   ! process parameter
        double precision          :: dn22                   ! process parameter
        type(pathchoose_struct),pointer :: pathname_dialog  ! process parameter
        character(len=FILENAME_LENGTH)  :: pathname         ! process parameter
        integer                   :: npoints                ! process parameter
        double precision,pointer  :: survey_east(:)         ! process parameter
        double precision,pointer  :: survey_north(:)        ! process parameter
        double precision,pointer  :: grid_x(:)              ! process parameter
        double precision,pointer  :: grid_y(:)              ! process parameter
        double precision,pointer  :: x_residual(:)          ! process parameter
        double precision,pointer  :: y_residual(:)          ! process parameter

        integer                     :: nmode_menu           ! option menu
        character(len=8)            :: mode_menu(2)         ! option menu
        integer                     :: nhandedness_menu     ! option menu
        character(len=8)            :: handedness_menu(2)   ! option menu

        type(grid_struct)           :: grid                 ! dependent variable

      end type project_data_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(project_data_struct),pointer,save :: object      ! needed for traps.


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine project_data_create (obj)
      implicit none
      type(project_data_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%survey_east)
      nullify(obj%survey_north)
      nullify(obj%grid_x)
      nullify(obj%grid_y)
      nullify(obj%x_residual)
      nullify(obj%y_residual)
      nullify(obj%project_menu)
      nullify (obj%pathname_dialog) ! jpa

      call pathchoose_create (obj%pathname_dialog,'PATHNAME','jd')

      call project_data_initialize (obj)

      return
      end subroutine project_data_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine project_data_delete (obj)
      implicit none
      type(project_data_struct),pointer :: obj       ! arguments

      call project_data_wrapup (obj)

      if (associated(obj%survey_east) ) deallocate(obj%survey_east)
      if (associated(obj%survey_north)) deallocate(obj%survey_north)
      if (associated(obj%grid_x)      ) deallocate(obj%grid_x)
      if (associated(obj%grid_y)      ) deallocate(obj%grid_y)
      if (associated(obj%x_residual)  ) deallocate(obj%x_residual)
      if (associated(obj%y_residual)  ) deallocate(obj%y_residual)
      if (associated(obj%project_menu)  ) deallocate(obj%project_menu)

      call pathchoose_delete (obj%pathname_dialog)

      deallocate(obj)

      return
      end subroutine project_data_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine project_data_initialize (obj)
      implicit none
      type(project_data_struct),target :: obj       ! arguments



      obj%project                = 'NONE'
      obj%account                = ' '
      call getsys_username (obj%user_name)
      obj%routing_name           = ' '
      obj%address                = ' '
      obj%phone                  = ' '
      obj%user_level             = 'REGULAR'
      obj%tr_units_typ           = 'TIME'
      obj%tr_units               = 'SECONDS'
      obj%survey_units           = 'FEET'

      obj%tr_units_combo         = 'TIME  - SECONDS'

      obj%ntr_units_combo_menu   = 6
      obj%tr_units_combo_menu(1) = 'TIME  - SECONDS'
      obj%tr_units_combo_menu(2) = 'DEPTH - FEET'
      obj%tr_units_combo_menu(3) = 'DEPTH - METERS'
      obj%tr_units_combo_menu(4) = 'DEPTH - KILOMETERS'
      obj%tr_units_combo_menu(5) = 'DEPTH - KILOFEET'
      obj%tr_units_combo_menu(6) = 'FREQ  - HZ'

      obj%nsurvey_units_menu     = 2
      obj%survey_units_menu(1)   = 'FEET'
      obj%survey_units_menu(2)   = 'METERS'

      call project_data_get_valid_projects(obj)

      obj%nuser_level_menu     = 3
      obj%user_level_menu(1)   = 'REGULAR'
      obj%user_level_menu(2)   = 'EXPERT'
      obj%user_level_menu(3)   = 'RESTRICTED'

      obj%mode                   = '3D Mode'
      call grid_initialize (obj%grid)
      obj%origin_east            = grid_get_xorigin        (obj%grid)
      obj%origin_north           = grid_get_yorigin        (obj%grid)
      obj%x_grid_dist            = grid_get_xgrid_width    (obj%grid)
      obj%y_grid_dist            = grid_get_ygrid_width    (obj%grid)
      obj%angle                  = grid_get_rotation_angle (obj%grid)
      if (grid_is_right_handed(obj%grid)) then
        obj%handedness           = 'RIGHT'
      else
        obj%handedness           = 'LEFT'
      endif
      obj%dx11                   = grid_get_dx11           (obj%grid)
      obj%dx12                   = grid_get_dx12           (obj%grid)
      obj%dx21                   = grid_get_dx21           (obj%grid)
      obj%dx22                   = grid_get_dx22           (obj%grid)
      obj%dn11                   = grid_get_dn11           (obj%grid)
      obj%dn12                   = grid_get_dn12           (obj%grid)
      obj%dn21                   = grid_get_dn21           (obj%grid)
      obj%dn22                   = grid_get_dn22           (obj%grid)
      obj%pathname               = ' '
      obj%npoints                = 0

      obj%nmode_menu             = 2
      obj%mode_menu(1)           = '3D Mode'
      obj%mode_menu(2)           = '2D Mode'

      obj%nhandedness_menu       = 2
      obj%handedness_menu(1)     = 'RIGHT'
      obj%handedness_menu(2)     = 'LEFT'

      allocate(obj%survey_east(1))
      allocate(obj%survey_north(1))
      allocate(obj%grid_x(1))
      allocate(obj%grid_y(1))
      allocate(obj%x_residual(1))
      allocate(obj%y_residual(1))

      obj%survey_east(1)         = DNIL
      obj%survey_north(1)        = DNIL
      obj%grid_x(1)              = DNIL
      obj%grid_y(1)              = DNIL
      obj%x_residual(1)          = DNIL
      obj%y_residual(1)          = DNIL

      call project_data_update (obj)

      return
      end subroutine project_data_initialize


!!--------------------- project_data_pathname_trap -------------------------!!
!!--------------------- project_data_pathname_trap -------------------------!!
!!--------------------- project_data_pathname_trap -------------------------!!


      subroutine project_data_pathname_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword                        ! arugment

      type(geomio_struct),pointer  ::  geomio_obj                     ! local
      integer                      ::  err                            ! local
      character(len=PC_LENGTH)     ::  msg                            ! local
      real                         ::  ve                             ! local
      real                         ::  datum                          ! local
      real                         ::  fixdist                        ! local
      character(len=PC_LENGTH)     ::  chaining                       ! local
      integer                      ::  nld                            ! local
      integer                      ::  nrp                            ! local
      integer                      ::  npp                            ! local
      integer                      ::  nzt1                           ! local
      integer                      ::  nzt2                           ! local
      integer                      ::  nzt3                           ! local
      integer                      ::  nzt4                           ! local

      nullify (geomio_obj) ! jpa
      if (len_trim(object%pathname) .gt. 0) then
        call geomio_open_read (geomio_obj,object%pathname,err,msg,ve,datum,    &
                               fixdist,object%grid,chaining,nld,nrp,npp,nzt1,  &
                               nzt2,nzt3,nzt4)

        if (err .ne. GEOMIO_OK) then
          call pc_error (trim(msg)//' - '//trim(object%pathname))
        endif

        call geomio_close (geomio_obj)

        object%pathname = ' '
      endif

      return
      end subroutine project_data_pathname_trap


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine project_data_update (obj)
      implicit none
      type(project_data_struct),target :: obj                        ! arguments

      integer                                  :: nsurvey_east       ! local
      integer                                  :: nsurvey_north      ! local
      integer                                  :: ngrid_x            ! local
      integer                                  :: ngrid_y            ! local
      integer                                  :: nx_residual        ! local
      integer                                  :: ny_residual        ! local
      integer                                  :: grid_handedness    ! local
      integer                                  :: nscratch           ! local
      integer                                  :: nstore             ! local
      integer                                  :: i                  ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!

      if (pathchoose_update(obj%pathname_dialog,obj%pathname)) return

      if (pc_pressed('OK')) call project_data_pathname_trap('OK')

      if (pc_pressed('Recalculate')) then
        if (obj%npoints .gt. 2) then
          if (associated(obj%x_residual)) deallocate(obj%x_residual)
          if (associated(obj%y_residual)) deallocate(obj%y_residual)
          allocate(obj%x_residual(obj%npoints))
          allocate(obj%y_residual(obj%npoints))
          call project_data_recalculate_trap ('Recalculate')
        else
          call pc_error ('Must have at least 3 points')
          return
        endif
      endif

  
      call pc_get ('PROJECT'       ,obj%project      , project_data_trap)
      call pc_get ('ACCOUNT'       ,obj%account      , project_data_trap)
      call pc_get ('USER_NAME'     ,obj%user_name    , project_data_trap)
      call pc_get ('ROUTING_NAME'  ,obj%routing_name , project_data_trap)
      call pc_get ('ADDRESS'       ,obj%address      , project_data_trap)
      call pc_get ('PHONE'         ,obj%phone        , project_data_trap)
      call pc_get ('USER_LEVEL'    ,obj%user_level   , project_data_trap)
      call pc_get ('TR_UNITS_TYP'  ,obj%tr_units_typ , project_data_trap)
      call pc_get ('TR_UNITS'      ,obj%tr_units     , project_data_trap)
      call pc_get ('SURVEY_UNITS'  ,obj%survey_units , project_data_trap)
      call pc_get ('TR_UNITS_COMBO',obj%tr_units_combo)


      if (pc_get_update_state() .ne. PC_GUI) then
        call pc_get ('ORIGIN_EAST' ,obj%origin_east )
        call pc_get ('ORIGIN_NORTH',obj%origin_north)
        call pc_get ('ANGLE'       ,obj%angle       )
        call pc_get ('X_GRID_DIST' ,obj%x_grid_dist )
        call pc_get ('Y_GRID_DIST' ,obj%y_grid_dist )
        call pc_get ('HANDEDNESS'  ,obj%handedness  )

        if (trim(obj%handedness) .eq. 'RIGHT') grid_handedness = 1
        if (trim(obj%handedness) .eq. 'LEFT' ) grid_handedness = -1
        call grid_set_transform (obj%grid,obj%origin_east,obj%origin_north,  &
                                 obj%angle,obj%x_grid_dist,obj%y_grid_dist,  &
                                 grid_handedness)
      else
!       call pc_get ('MODE'        ,obj%mode        ,project_data_mode_trap    )
        call pc_get ('ORIGIN_EAST' ,obj%origin_east ,project_data_xorigin_trap )
        call pc_get ('ORIGIN_NORTH',obj%origin_north,project_data_yorigin_trap )
        call pc_get ('X_GRID_DIST' ,obj%x_grid_dist ,project_data_xwidth_trap  )
        call pc_get ('Y_GRID_DIST' ,obj%y_grid_dist ,project_data_ywidth_trap  )
        call pc_get ('ANGLE'       ,obj%angle       ,project_data_angle_trap   )
        call pc_get ('HANDEDNESS'  ,obj%handedness,project_data_handedness_trap)
        call pc_get ('DX11'        ,obj%dx11        ,project_data_dx11_trap    )
        call pc_get ('DX12'        ,obj%dx12        ,project_data_dx12_trap    )
        call pc_get ('DX21'        ,obj%dx21        ,project_data_dx21_trap    )
        call pc_get ('DX22'        ,obj%dx22        ,project_data_dx22_trap    )
        call pc_get ('DN11'        ,obj%dn11        ,project_data_dn11_trap    )
        call pc_get ('DN12'        ,obj%dn12        ,project_data_dn12_trap    )
        call pc_get ('DN21'        ,obj%dn21        ,project_data_dn21_trap    )
        call pc_get ('DN22'        ,obj%dn22        ,project_data_dn22_trap    )
        call pc_get ('PATHNAME'    ,obj%pathname    ,project_data_pathname_trap)
      endif

      nsurvey_east  = obj%npoints
      nsurvey_north = obj%npoints
      ngrid_x       = obj%npoints
      ngrid_y       = obj%npoints
      nx_residual   = obj%npoints
      ny_residual   = obj%npoints

      call pc_alloc ('SURVEY_EAST' ,obj%survey_east ,nsurvey_east,   &
                                                      project_data_points_trap)
      call pc_alloc ('SURVEY_NORTH',obj%survey_north,nsurvey_north,  &
                                                      project_data_points_trap)
      call pc_alloc ('GRID_X'      ,obj%grid_x      ,ngrid_x,        &
                                                      project_data_points_trap)
      call pc_alloc ('GRID_Y'      ,obj%grid_y      ,ngrid_y,        &
                                                      project_data_points_trap)
      call pc_alloc ('X_RESIDUAL'  ,obj%x_residual  ,nx_residual              )
      call pc_alloc ('Y_RESIDUAL'  ,obj%y_residual  ,ny_residual              )

      if (nsurvey_east.ne.nsurvey_north .or. nsurvey_east.ne.ngrid_x .or.  &
          nsurvey_east.ne.ngrid_y) then
        call pc_error ('SURVEY_EAST, SURVEY_NORTH, ' //  &
                       'GRID_X, GRID_Y array have different lengths')
        obj%npoints = min(nsurvey_east,nsurvey_north,ngrid_x,ngrid_y)
      else
        obj%npoints = nsurvey_east
      endif


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      nscratch = 0
      nstore   = 0
 
      if (pc_gui_action_present('TR_UNITS_COMBO','ModifyField')) then
        i = index(obj%tr_units_combo,' ')-1
        obj%tr_units_typ = obj%tr_units_combo(1:i)
        i = index(trim(obj%tr_units_combo),' ',.true.)
        obj%tr_units     = obj%tr_units_combo(i:)
      endif

      obj%origin_east  = grid_get_xorigin        (obj%grid)
      obj%origin_north = grid_get_yorigin        (obj%grid)
      obj%x_grid_dist  = grid_get_xgrid_width    (obj%grid)
      obj%y_grid_dist  = grid_get_ygrid_width    (obj%grid)
      obj%angle        = grid_get_rotation_angle (obj%grid)
      if (grid_is_right_handed(obj%grid)) then
        obj%handedness = 'RIGHT'
      else
        obj%handedness = 'LEFT'
      endif
      obj%dx11         = grid_get_dx11           (obj%grid)
      obj%dx12         = grid_get_dx12           (obj%grid)
      obj%dx21         = grid_get_dx21           (obj%grid)
      obj%dx22         = grid_get_dx22           (obj%grid)
      obj%dn11         = grid_get_dn11           (obj%grid)
      obj%dn12         = grid_get_dn12           (obj%grid)
      obj%dn21         = grid_get_dn21           (obj%grid)
      obj%dn22         = grid_get_dn22           (obj%grid)

      obj%gui_only_origin_east  = obj%origin_east
      obj%gui_only_origin_north = obj%origin_north
      obj%gui_only_x_grid_dist  = obj%x_grid_dist
      obj%gui_only_y_grid_dist  = obj%y_grid_dist
      obj%gui_only_angle        = obj%angle
      obj%gui_only_handedness   = obj%handedness

      if (obj%npoints .gt. 0) then
        if (.not.associated(obj%x_residual)) then
          allocate(obj%x_residual(obj%npoints))
          obj%x_residual = DNIL
        else
          if (size(obj%x_residual) .ne. obj%npoints) then
            deallocate(obj%x_residual)
            allocate(obj%x_residual(obj%npoints))
            obj%x_residual = DNIL
          endif
        endif

        if (.not.associated(obj%y_residual)) then
          allocate(obj%y_residual(obj%npoints))
          obj%y_residual = DNIL
        else
          if (size(obj%y_residual) .ne. obj%npoints) then
            deallocate(obj%y_residual)
            allocate(obj%y_residual(obj%npoints))
            obj%y_residual = DNIL
          endif
        endif
      else
        if (associated(obj%x_residual)) deallocate(obj%x_residual)
        if (associated(obj%y_residual)) deallocate(obj%y_residual)
        allocate(obj%x_residual(1))
        allocate(obj%y_residual(1))
        obj%x_residual = DNIL
        obj%y_residual = DNIL
      endif


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      call pc_put_options_field ('TR_UNITS_COMBO' ,obj%tr_units_combo_menu,  &
                                                   obj%ntr_units_combo_menu)
      call pc_put_options_field ('SURVEY_UNITS'   ,obj%survey_units_menu,  &
                                                   obj%nsurvey_units_menu)
      call pc_put_options_field ('USER_LEVEL'     ,obj%user_level_menu,  &
                                                   obj%nuser_level_menu)
      call pc_put_options_field ('PROJECT'        ,obj%project_menu,  &
                                                   obj%nproject)

      call pc_put ('PROJECT'      ,obj%project          )
      call pc_put ('ACCOUNT'      ,obj%account          )
      call pc_put ('USER_NAME'    ,obj%user_name        )
      call pc_put ('ROUTING_NAME' ,obj%routing_name     )
      call pc_put ('ADDRESS'      ,obj%address          )
      call pc_put ('PHONE'        ,obj%phone            )
      call pc_put ('USER_LEVEL'   ,obj%user_level       )
      call pc_put ('SURVEY_UNITS' ,obj%survey_units     )

      call pc_put_process ('TR_UNITS_TYP' ,obj%tr_units_typ     )
      call pc_put_process ('TR_UNITS'     ,obj%tr_units         )

      call pc_put_gui_only ('TR_UNITS_COMBO'        ,obj%tr_units_combo      )
      call pc_put_gui_only ('GUI_ONLY_ORIGIN_EAST' ,obj%gui_only_origin_east )
      call pc_put_gui_only ('GUI_ONLY_X_GRID_DIST' ,obj%gui_only_x_grid_dist )
      call pc_put_gui_only ('GUI_ONLY_ANGLE'       ,obj%gui_only_angle       )
      call pc_put_gui_only ('GUI_ONLY_ORIGIN_NORTH',obj%gui_only_origin_north)
      call pc_put_gui_only ('GUI_ONLY_Y_GRID_DIST' ,obj%gui_only_y_grid_dist )
      call pc_put_gui_only ('GUI_ONLY_HANDEDNESS'  ,obj%gui_only_handedness  )
 
      call pc_put_pdata ('PROJECT'      ,obj%project      )
      if(obj%project.eq.'NONE')then
        call cps_set_project_name("")
      else
        call cps_set_project_name(obj%project)
      endif
      call pc_put_pdata ('ACCOUNT'      ,obj%account      )
      call pc_put_pdata ('USER_NAME'    ,obj%user_name    )
      call pc_put_pdata ('ROUTING_NAME' ,obj%routing_name )
      call pc_put_pdata ('ADDRESS'      ,obj%address      )
      call pc_put_pdata ('PHONE'        ,obj%phone        )
      call pc_put_pdata ('USER_LEVEL'   ,obj%user_level   )
      call pc_put_pdata ('TR_UNITS_TYP' ,obj%tr_units_typ )
      call pc_put_pdata ('TR_UNITS'     ,obj%tr_units     )
      call pc_put_pdata ('SURVEY_UNITS' ,obj%survey_units )
      call pc_put_pdata ('GRID'         ,obj%grid         )
 
      call pc_put_options_field ('MODE'       ,obj%mode_menu,        &
                                               obj%nmode_menu)
      call pc_put_options_field ('HANDEDNESS' ,obj%handedness_menu,  &
                                               obj%nhandedness_menu)

      call pc_put ('MODE'        ,obj%mode        )
      call pc_put ('ORIGIN_EAST' ,obj%origin_east )
      call pc_put ('ORIGIN_NORTH',obj%origin_north)
      call pc_put ('X_GRID_DIST' ,obj%x_grid_dist )
      call pc_put ('Y_GRID_DIST' ,obj%y_grid_dist )
      call pc_put ('ANGLE'       ,obj%angle       )
      call pc_put ('HANDEDNESS'  ,obj%handedness  )
      call pc_put ('DX11'        ,obj%dx11        )
      call pc_put ('DX12'        ,obj%dx12        )
      call pc_put ('DX21'        ,obj%dx21        )
      call pc_put ('DX22'        ,obj%dx22        )
      call pc_put ('DN11'        ,obj%dn11        )
      call pc_put ('DN12'        ,obj%dn12        )
      call pc_put ('DN21'        ,obj%dn21        )
      call pc_put ('DN22'        ,obj%dn22        )

      call pc_put_gui_only ('PATHNAME',obj%pathname)

      call pc_put ('SURVEY_EAST' ,obj%survey_east ,obj%npoints)
      call pc_put ('SURVEY_NORTH',obj%survey_north,obj%npoints)
      call pc_put ('GRID_X'      ,obj%grid_x      ,obj%npoints)
      call pc_put ('GRID_Y'      ,obj%grid_y      ,obj%npoints)
      call pc_put ('X_RESIDUAL'  ,obj%x_residual  ,obj%npoints)
      call pc_put ('Y_RESIDUAL'  ,obj%y_residual  ,obj%npoints)

      call pc_put_sensitive_field_flag    ('MODE'      ,.false.)
      call pc_put_sensitive_array_flag    ('X_RESIDUAL',.false.)
      call pc_put_sensitive_array_flag    ('Y_RESIDUAL',.false.)

      call pc_put_global   ('GRID'      ,obj%grid)
 
      call pc_put_control  ('NSCRATCH'  ,nscratch)
      call pc_put_control  ('NSTORE'    ,nstore  )
      call pc_put_control  ('SETUP_ONLY',.true.  )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces())then
        return
      endif

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine project_data_update


!!------------------------- project_data_trap ------------------------------!!
!!------------------------- project_data_trap ------------------------------!!
!!------------------------- project_data_trap ------------------------------!!


      subroutine project_data_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword
      integer :: i
 
      select case (keyword)
 
        case ('PROJECT'     )

!              Check for valid project
        do i=1,object%nproject
          if(object%project.ne.object%project_menu(i))cycle
          return
        enddo  
        object%project='NONE' 
        case ('SUB_PROJECT' )
        case ('ACCOUNT'     )
           call string_squeeze_blanks(object%account)
        case ('USER_NAME'   )
        case ('ROUTING_NAME')
        case ('ADDRESS'     )
        case ('PHONE'       )
        case ('USER_LEVEL'  )
        case ('TR_UNITS_TYP')
        case ('TR_UNITS'    )
        case ('SURVEY_UNITS')
        case default
 
      end select
 
      return
      end subroutine project_data_trap


!!-------------------- project_data_recalculate_trap -----------------------!!
!!-------------------- project_data_recalculate_trap -----------------------!!
!!-------------------- project_data_recalculate_trap -----------------------!!


      subroutine project_data_recalculate_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_define_transform (object%grid       , object%npoints     ,  &
                                  object%survey_east, object%survey_north,  &
                                  object%grid_x     , object%grid_y      ,  &
                                  object%x_residual , object%y_residual )

      return
      end subroutine project_data_recalculate_trap


!!----------------------- project_data_mode_trap ---------------------------!!
!!----------------------- project_data_mode_trap ---------------------------!!
!!----------------------- project_data_mode_trap ---------------------------!!


      subroutine project_data_mode_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      if (trim(object%mode) .eq. '3D Mode') then
        call pc_put_sensitive_field_flag ('ORIGIN_EAST'      ,.true.)
        call pc_put_sensitive_field_flag ('ORIGIN_NORTH'     ,.true.)
        call pc_put_sensitive_field_flag ('X_GRID_DIST'      ,.true.)
        call pc_put_sensitive_field_flag ('Y_GRID_DIST'      ,.true.)
        call pc_put_sensitive_field_flag ('ANGLE'            ,.true.)
        call pc_put_sensitive_field_flag ('HANDEDNESS'       ,.true.)
        call pc_put_sensitive_field_flag ('DX11'             ,.true.)
        call pc_put_sensitive_field_flag ('DX12'             ,.true.)
        call pc_put_sensitive_field_flag ('DX21'             ,.true.)
        call pc_put_sensitive_field_flag ('DX22'             ,.true.)
        call pc_put_sensitive_field_flag ('DN11'             ,.true.)
        call pc_put_sensitive_field_flag ('DN12'             ,.true.)
        call pc_put_sensitive_field_flag ('DN21'             ,.true.)
        call pc_put_sensitive_field_flag ('DN22'             ,.true.)
        call pc_put_sensitive_field_flag ('RECALCULATEBUTON' ,.true.)
        call pc_put_sensitive_array_flag ('SURVEY_EAST'      ,.true.)
        call pc_put_sensitive_array_flag ('SURVEY_NORTH'     ,.true.)
        call pc_put_sensitive_array_flag ('GRID_X'           ,.true.)
        call pc_put_sensitive_array_flag ('GRID_Y'           ,.true.)
        call pc_put_sensitive_array_flag ('X_RESIDUAL'       ,.true.)
        call pc_put_sensitive_array_flag ('Y_RESIDUAL'       ,.true.)
      else
        call pc_put_sensitive_field_flag ('ORIGIN_EAST'      ,.false.)
        call pc_put_sensitive_field_flag ('ORIGIN_NORTH'     ,.false.)
        call pc_put_sensitive_field_flag ('X_GRID_DIST'      ,.true. )
        call pc_put_sensitive_field_flag ('Y_GRID_DIST'      ,.false.)
        call pc_put_sensitive_field_flag ('ANGLE'            ,.false.)
        call pc_put_sensitive_field_flag ('HANDEDNESS'       ,.false.)
        call pc_put_sensitive_field_flag ('DX11'             ,.false.)
        call pc_put_sensitive_field_flag ('DX12'             ,.false.)
        call pc_put_sensitive_field_flag ('DX21'             ,.false.)
        call pc_put_sensitive_field_flag ('DX22'             ,.false.)
        call pc_put_sensitive_field_flag ('DN11'             ,.false.)
        call pc_put_sensitive_field_flag ('DN12'             ,.false.)
        call pc_put_sensitive_field_flag ('DN21'             ,.false.)
        call pc_put_sensitive_field_flag ('DN22'             ,.false.)
        call pc_put_sensitive_field_flag ('RECALCULATEBUTON' ,.false.)
        call pc_put_sensitive_array_flag ('SURVEY_EAST'      ,.true. )
        call pc_put_sensitive_array_flag ('SURVEY_NORTH'     ,.false.)
        call pc_put_sensitive_array_flag ('GRID_X'           ,.false.)
        call pc_put_sensitive_array_flag ('GRID_Y'           ,.false.)
        call pc_put_sensitive_array_flag ('X_RESIDUAL'       ,.false.)
        call pc_put_sensitive_array_flag ('Y_RESIDUAL'       ,.false.)
      endif

      return
      end subroutine project_data_mode_trap


!!---------------------- project_data_xorigin_trap -------------------------!!
!!---------------------- project_data_xorigin_trap -------------------------!!
!!---------------------- project_data_xorigin_trap -------------------------!!


      subroutine project_data_xorigin_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_xorigin (object%grid, object%origin_east)

      return
      end subroutine project_data_xorigin_trap


!!--------------------- project_data_yorigin_trap --------------------------!!
!!--------------------- project_data_yorigin_trap --------------------------!!
!!--------------------- project_data_yorigin_trap --------------------------!!


      subroutine project_data_yorigin_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_yorigin (object%grid, object%origin_north)

      return
      end subroutine project_data_yorigin_trap


!!---------------------- project_data_xwidth_trap --------------------------!!
!!---------------------- project_data_xwidth_trap --------------------------!!
!!---------------------- project_data_xwidth_trap --------------------------!!


      subroutine project_data_xwidth_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_xgrid_width (object%grid, object%x_grid_dist)

      return
      end subroutine project_data_xwidth_trap


!!---------------------- project_data_ywidth_trap --------------------------!!
!!---------------------- project_data_ywidth_trap --------------------------!!
!!---------------------- project_data_ywidth_trap --------------------------!!


      subroutine project_data_ywidth_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_ygrid_width (object%grid, object%y_grid_dist)

      return
      end subroutine project_data_ywidth_trap


!!----------------------- project_data_angle_trap --------------------------!!
!!----------------------- project_data_angle_trap --------------------------!!
!!----------------------- project_data_angle_trap --------------------------!!


      subroutine project_data_angle_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_rotation_angle (object%grid, object%angle)

      return
      end subroutine project_data_angle_trap


!!-------------------- project_data_handedness_trap ------------------------!!
!!-------------------- project_data_handedness_trap ------------------------!!
!!-------------------- project_data_handedness_trap ------------------------!!


      subroutine project_data_handedness_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      if (trim(object%handedness) .eq. 'RIGHT') then
        call grid_set_right_handed (object%grid)
      else
        call grid_set_left_handed  (object%grid)
      endif

      return
      end subroutine project_data_handedness_trap




!!----------------------- project_data_dx11_trap ---------------------------!!
!!----------------------- project_data_dx11_trap ---------------------------!!
!!----------------------- project_data_dx11_trap ---------------------------!!


      subroutine project_data_dx11_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_dx11 (object%grid, object%dx11)

      return
      end subroutine project_data_dx11_trap


!!----------------------- project_data_dx12_trap ---------------------------!!
!!----------------------- project_data_dx12_trap ---------------------------!!
!!----------------------- project_data_dx12_trap ---------------------------!!


      subroutine project_data_dx12_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_dx12 (object%grid, object%dx12)

      return
      end subroutine project_data_dx12_trap


!!----------------------- project_data_dx21_trap ---------------------------!!
!!----------------------- project_data_dx21_trap ---------------------------!!
!!----------------------- project_data_dx21_trap ---------------------------!!


      subroutine project_data_dx21_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_dx21 (object%grid, object%dx21)

      return
      end subroutine project_data_dx21_trap


!!----------------------- project_data_dx22_trap ---------------------------!!
!!----------------------- project_data_dx22_trap ---------------------------!!
!!----------------------- project_data_dx22_trap ---------------------------!!


      subroutine project_data_dx22_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_dx22 (object%grid, object%dx22)

      return
      end subroutine project_data_dx22_trap


!!----------------------- project_data_dn11_trap ---------------------------!!
!!----------------------- project_data_dn11_trap ---------------------------!!
!!----------------------- project_data_dn11_trap ---------------------------!!


      subroutine project_data_dn11_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_dn11 (object%grid, object%dn11)

      return
      end subroutine project_data_dn11_trap


!!----------------------- project_data_dn12_trap ---------------------------!!
!!----------------------- project_data_dn12_trap ---------------------------!!
!!----------------------- project_data_dn12_trap ---------------------------!!


      subroutine project_data_dn12_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_dn12 (object%grid, object%dn12)

      return
      end subroutine project_data_dn12_trap


!!----------------------- project_data_dn21_trap ---------------------------!!
!!----------------------- project_data_dn21_trap ---------------------------!!
!!----------------------- project_data_dn21_trap ---------------------------!!


      subroutine project_data_dn21_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_dn21 (object%grid, object%dn21)

      return
      end subroutine project_data_dn21_trap


!!----------------------- project_data_dn22_trap ---------------------------!!
!!----------------------- project_data_dn22_trap ---------------------------!!
!!----------------------- project_data_dn22_trap ---------------------------!!


      subroutine project_data_dn22_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword

      call grid_set_dn22 (object%grid, object%dn22)

      return
      end subroutine project_data_dn22_trap


!!---------------------- project_data_points_trap --------------------------!!
!!---------------------- project_data_points_trap --------------------------!!
!!---------------------- project_data_points_trap --------------------------!!


      subroutine project_data_points_trap (keyword,indx,action)
      implicit none
      character(len=*),intent(in)  ::  keyword
      integer         ,intent(in)  ::  indx
      integer         ,intent(in)  ::  action

      if (associated(object%x_residual)) deallocate(object%x_residual)
      if (associated(object%y_residual)) deallocate(object%y_residual)

      return
      end subroutine project_data_points_trap


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine project_data_wrapup (obj)
      implicit none
      type(project_data_struct),pointer    :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine project_data_wrapup





      subroutine project_data_get_valid_projects(obj)

      type(project_data_struct),target :: obj            
!!!      type(project_data_struct) :: obj     

      integer :: i,istat,j,k
      integer,save :: lun=0
      character(len=160) :: card,filename
      character(len=15) :: tempproj(500)


      
      if(lun.ne.0)then
        call getlun(lun,istat)
        if(istat.ne.0)&
          call pc_error('Unable to get unit number for project names')
        endif
      call cnfg_get_value('cpsdata_nodes_file',filename)
      open(lun,file=filename,status='old',iostat=istat)
!
!!!      open(lun,file='/home/goodgkp/src/project_data/cpsdata_nodes.dat',&
!!!               status='old',iostat=istat)
      if(istat.ne.0)call pc_error('Unable to open cpsdata_nodes.dat file')



      obj%nproject=0
      i=0
      DO
        read(lun,'(A)',iostat=istat)card
        if(istat.lt.0)exit
        if(card(1:1).eq.'#')cycle
        i=i+1
        if(i.gt.499)call pc_error('More than 500 cpsdata cards')
        call nthcol(card,5,tempproj(i),istat)
        if(istat.ne.0)call pc_error('unable to get project name from file')
      ENDDO
      close(lun)

      call alphasort_sort(tempproj,i)
      tempproj(i+1)='END'

!                 Now set up the combo box
      if(.not.associated(obj%project_menu))allocate(obj%project_menu(i))

      k=0
      do j=1,i
        if(tempproj(j).eq.' ')cycle
        if(tempproj(j).eq.tempproj(j+1))cycle
        k=k+1
        obj%project_menu(k)=tempproj(j)
      enddo
      obj%nproject=k+1   
      obj%project_menu(k+1)='NONE'


      end subroutine project_data_get_valid_projects


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module project_data_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

